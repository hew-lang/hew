import type { TraceBuilder } from "../interpreter/trace.js";
import type { JsonValue, RuntimeFailure, TraceSpan, TrapKind } from "../interpreter/types.js";
import { UNIT, cloneValue, toJsonValue, type VmValue } from "../interpreter/values.js";
import { DeterministicIds } from "./ids.js";
import { SeededPrng } from "./prng.js";

export type SchedulerPolicy = "round_robin" | "chaos";

export interface ActorFieldLayout {
  name: string;
  type: string;
  index: number;
}

export interface ActorHandlerLayout {
  name: string;
  function: string;
}

export interface ActorLayout {
  id: string;
  name: string;
  state_fields: ActorFieldLayout[];
  handlers: ActorHandlerLayout[];
  max_mailbox?: number;
  max_heap?: number;
}

export type SupervisorStrategy = "one_for_one" | "one_for_all" | "rest_for_one";
export type ChildRestartClass = "permanent" | "transient" | "temporary";

export interface SupervisorChildSpec {
  id: string;
  restart: ChildRestartClass;
  actorLayoutId: string;
  initialState: VmValue[];
}

export interface SupervisorLayout {
  id: string;
  name: string;
  strategy: SupervisorStrategy;
  restartIntensity: number;
  restartWindowMs: number;
  children: SupervisorChildSpec[];
}

export class SchedulerRuntimeError extends Error {
  constructor(
    readonly kind: "trap" | "unsupported",
    readonly message: string,
    readonly trapKind: TrapKind = "unsupported_instruction"
  ) {
    super(message);
    this.name = "SchedulerRuntimeError";
  }
}

export class ActorTurnCrash extends Error {
  constructor(readonly failure: RuntimeFailure) {
    super(failure.message);
    this.name = "ActorTurnCrash";
  }
}

interface ActorMessage {
  id: string;
  from: string;
  to: string;
  name: string;
  payload: VmValue[];
  replyId: string | null;
}

interface ReplySlot {
  id: string;
  resolved: boolean;
  value: VmValue;
}

interface ActorCell {
  id: string;
  layout: ActorLayout;
  state: VmValue[];
  initialState: VmValue[];
  mailbox: ActorMessage[];
  ready: boolean;
  alive: boolean;
  startPending: boolean;
  watermark: number;
  supervisorId: string | null;
  childId: string | null;
  generation: number;
}

interface SchedulerCallbacks {
  invoke(functionId: string, args: VmValue[]): VmValue;
  consumeStep(): void;
}

interface ChildSlot {
  spec: SupervisorChildSpec;
  actorId: string | null;
  generation: number;
  status: "starting" | "running" | "restarting" | "stopped" | "failed";
  lastFailure: RuntimeFailure | null;
}

interface SupervisorCell {
  id: string;
  layout: SupervisorLayout;
  parentSupervisorId: string | null;
  slots: ChildSlot[];
  restartHistory: number[];
  alive: boolean;
}

interface MonitorCell {
  id: string;
  ownerId: string;
  targetId: string;
  active: boolean;
}

const REPLAY_FAMILY = "actor.scheduler-step";
const WATERMARK_THRESHOLDS = [25, 50, 75, 100] as const;
const DEFAULT_MAX_MAILBOX = 64;

export class ActorScheduler {
  readonly ids: DeterministicIds;
  readonly prng: SeededPrng;

  private readonly actors = new Map<string, ActorCell>();
  private readonly supervisors = new Map<string, SupervisorCell>();
  private readonly links = new Map<string, Set<string>>();
  private readonly monitors = new Map<string, MonitorCell>();
  private readonly ready: string[] = [];
  private readonly replies = new Map<string, ReplySlot>();
  private readonly replaySteps: string[];
  private replayStepIndex = 0;
  private activeActorId: string | null = null;
  private activeMessage: ActorMessage | null = null;
  private activeLifecycleHookName: string | null = null;
  private replaying: boolean;

  constructor(
    seed: number,
    private readonly policy: SchedulerPolicy,
    private readonly trace: TraceBuilder,
    private readonly callbacks: SchedulerCallbacks
  ) {
    this.ids = new DeterministicIds(seed);
    this.prng = new SeededPrng(seed);
    this.replaySteps = trace.replay.inputs
      .filter((input) => input.kind === "user_event" && isSchedulerStepInput(input.data))
      .map((input) => String((input.data as { actor_id: JsonValue }).actor_id));
    this.replaying = this.replaySteps.length > 0;
  }

  get activeActor(): string | null {
    return this.activeActorId;
  }

  get activeLifecycleHook(): string | null {
    return this.activeLifecycleHookName;
  }

  spawn(layout: ActorLayout, initialState: VmValue[], parentId: string, span: TraceSpan | null): VmValue {
    return { kind: "actor", id: this.spawnActor(layout, initialState, parentId, null, null, 0, span).id };
  }

  spawnSupervisor(layout: SupervisorLayout, actorLayouts: Map<string, ActorLayout>, parentId: string, span: TraceSpan | null): VmValue {
    const id = this.ids.supervisor();
    const supervisor: SupervisorCell = {
      id,
      layout,
      parentSupervisorId: parentId.startsWith("supervisor:") ? parentId : null,
      slots: layout.children.map((spec) => ({
        spec,
        actorId: null,
        generation: 0,
        status: "starting",
        lastFailure: null
      })),
      restartHistory: [],
      alive: true
    };
    this.supervisors.set(id, supervisor);
    this.trace.allocateId("supervisor", id, supervisor.parentSupervisorId);
    this.trace.snapshot("supervisor-spec-registered", this.supervisorSnapshot(supervisor), span);
    for (const slot of supervisor.slots) {
      this.startChild(supervisor, slot, actorLayouts, "initial", span);
    }
    this.trace.snapshot("supervisor-tree-state", this.supervisorSnapshot(supervisor), span);
    return { kind: "supervisor", id };
  }

  child(supervisorValue: VmValue, childId: string): VmValue {
    if (supervisorValue.kind !== "supervisor") {
      throw new SchedulerRuntimeError("trap", "supervisor.child requires a supervisor handle", "invalid_local");
    }
    const supervisor = this.supervisors.get(supervisorValue.id);
    if (!supervisor?.alive) {
      throw new SchedulerRuntimeError("trap", `supervisor ${supervisorValue.id} is stopped`, "invalid_call");
    }
    const slot = supervisor.slots.find((candidate) => candidate.spec.id === childId);
    if (!slot?.actorId) {
      throw new SchedulerRuntimeError("trap", `supervisor child ${childId} is unavailable`, "invalid_call");
    }
    return { kind: "actor", id: slot.actorId };
  }

  link(leftValue: VmValue, rightValue: VmValue, span: TraceSpan | null): void {
    const left = this.actorForValue(leftValue);
    const right = this.actorForValue(rightValue);
    if (!left.alive || !right.alive) {
      throw new SchedulerRuntimeError("trap", "M6_LINK_DEAD_ACTOR: actor.link requires both actors to be alive", "invalid_call");
    }
    this.linkSet(left.id).add(right.id);
    this.linkSet(right.id).add(left.id);
    this.trace.snapshot("link-formed", { left_actor_id: left.id, right_actor_id: right.id }, span);
  }

  unlink(leftValue: VmValue, rightValue: VmValue, span: TraceSpan | null): void {
    const left = this.actorForValue(leftValue);
    const right = this.actorForValue(rightValue);
    this.links.get(left.id)?.delete(right.id);
    this.links.get(right.id)?.delete(left.id);
    this.trace.snapshot("link-broken", { left_actor_id: left.id, right_actor_id: right.id }, span);
  }

  monitor(ownerValue: VmValue, targetValue: VmValue, span: TraceSpan | null): VmValue {
    const owner = this.actorForValue(ownerValue);
    const target = this.actorForValue(targetValue);
    const id = this.ids.monitor();
    const monitor: MonitorCell = { id, ownerId: owner.id, targetId: target.id, active: true };
    this.monitors.set(id, monitor);
    this.trace.snapshot("monitor-formed", { monitor_id: id, owner_actor_id: owner.id, target_actor_id: target.id }, span);
    if (!target.alive) {
      this.fireMonitor(monitor, stoppedFailure(target.id), span);
    }
    return { kind: "monitor", id };
  }

  demonitor(monitorValue: VmValue, span: TraceSpan | null): void {
    if (monitorValue.kind !== "monitor") {
      throw new SchedulerRuntimeError("trap", "actor.demonitor requires a monitor reference", "invalid_local");
    }
    const monitor = this.monitors.get(monitorValue.id);
    if (!monitor) {
      throw new SchedulerRuntimeError("trap", `monitor ${monitorValue.id} not found`, "invalid_call");
    }
    monitor.active = false;
    this.trace.snapshot("monitor-cancelled", { monitor_id: monitor.id, owner_actor_id: monitor.ownerId, target_actor_id: monitor.targetId }, span);
  }

  private spawnActor(
    layout: ActorLayout,
    initialState: VmValue[],
    parentId: string,
    supervisorId: string | null,
    childId: string | null,
    generation: number,
    span: TraceSpan | null
  ): ActorCell {
    const id = this.ids.actor();
    const state = layout.state_fields.map((_, index) => cloneValue(initialState[index] ?? UNIT));
    const actor: ActorCell = {
      id,
      layout,
      state,
      initialState: state.map(cloneValue),
      mailbox: [],
      ready: false,
      alive: true,
      startPending: true,
      watermark: 0,
      supervisorId,
      childId,
      generation
    };
    this.actors.set(id, actor);
    this.trace.allocateId("actor", id, parentId);
    this.trace.snapshot("actor.spawn", this.actorSnapshot(actor, { parent_id: parentId }), span);
    this.markReady(actor);
    return actor;
  }

  send(target: VmValue, name: string, payload: VmValue[], span: TraceSpan | null): void {
    const actor = this.actorForValue(target);
    this.enqueue(actor, name, payload, null, span);
  }

  ask(target: VmValue, name: string, payload: VmValue[], span: TraceSpan | null): VmValue {
    if (this.activeActorId !== null) {
      throw new SchedulerRuntimeError("unsupported", "Unsupported::M5_DEFERRED: actor.ask from actor turn", "unsupported_instruction");
    }
    const actor = this.actorForValue(target);
    const replyId = this.ids.reply();
    this.replies.set(replyId, { id: replyId, resolved: false, value: UNIT });
    this.trace.snapshot("actor.ask", {
      actor_id: actor.id,
      handler: name,
      reply_id: replyId,
      payload: payload.map(toJsonValue)
    });
    this.enqueue(actor, name, payload, replyId, span);
    this.runUntil(() => this.replies.get(replyId)?.resolved === true);
    const reply = this.replies.get(replyId);
    if (!reply?.resolved) {
      throw new SchedulerRuntimeError("trap", "actor ask did not receive a reply", "invalid_call");
    }
    return cloneValue(reply.value);
  }

  reply(token: VmValue, value: VmValue): void {
    // A unit token means no reply was requested (tell / actor.send path).
    // Drop the value silently rather than trapping — the handler trailing
    // expression is a reply value only when a reply token is present.
    if (token.kind === "unit") {
      return;
    }
    if (token.kind !== "reply") {
      throw new SchedulerRuntimeError("trap", "actor.reply requires a reply token", "invalid_local");
    }
    const slot = this.replies.get(token.id);
    if (!slot) {
      throw new SchedulerRuntimeError("trap", "reply token not found", "invalid_local");
    }
    slot.resolved = true;
    slot.value = cloneValue(value);
    this.trace.snapshot("actor.reply", {
      actor_id: this.activeActorId ?? "actor:root",
      reply_id: token.id,
      value: toJsonValue(value)
    });
  }

  dequeueCurrentPayload(): VmValue {
    if (!this.activeMessage) {
      throw new SchedulerRuntimeError("trap", "mailbox.dequeue requires an active actor message", "invalid_call");
    }
    return this.activeMessage.payload.length === 1
      ? cloneValue(this.activeMessage.payload[0]!)
      : { kind: "vector", elementType: "type:any", items: this.activeMessage.payload.map(cloneValue) };
  }

  drain(): void {
    this.runUntil(() => false);
  }

  private enqueue(actor: ActorCell, name: string, payload: VmValue[], replyId: string | null, span: TraceSpan | null): void {
    if (!actor.alive) {
      throw new SchedulerRuntimeError("trap", `actor ${actor.id} is stopped`, "invalid_call");
    }
    const maxMailbox = actor.layout.max_mailbox ?? DEFAULT_MAX_MAILBOX;
    if (actor.mailbox.length >= maxMailbox) {
      this.trace.snapshot("actor.send-backpressure", {
        actor_id: actor.id,
        mailbox_depth: actor.mailbox.length,
        max_mailbox: maxMailbox,
        sender: this.activeActorId ?? "actor:root"
      });
      if (this.activeActorId !== null) {
        throw new SchedulerRuntimeError("trap", "actor send backpressure cannot park an active actor turn in the educational VM", "budget_exhausted");
      }
      this.runUntil(() => !actor.alive || actor.mailbox.length < maxMailbox);
      if (!actor.alive) {
        throw new SchedulerRuntimeError("trap", `actor ${actor.id} is stopped`, "invalid_call");
      }
      if (actor.mailbox.length >= maxMailbox) {
        throw new SchedulerRuntimeError("trap", "actor mailbox backpressure could not make progress", "budget_exhausted");
      }
    }
    const message: ActorMessage = {
      id: this.ids.message(),
      from: this.activeActorId ?? "actor:root",
      to: actor.id,
      name,
      payload: payload.map(cloneValue),
      replyId
    };
    actor.mailbox.push(message);
    this.trace.snapshot("actor.send", this.messageSnapshot(message, actor));
    this.emitWatermark(actor);
    this.markReady(actor);
  }

  private runUntil(done: () => boolean): void {
    while (!done() && this.ready.length > 0) {
      const actor = this.nextReadyActor();
      if (!actor) {
        return;
      }
      this.callbacks.consumeStep();
      this.trace.stepCommitted(REPLAY_FAMILY, actor.id);
      this.trace.recordReplayInput(
        {
          kind: "user_event",
          data: { family: REPLAY_FAMILY, actor_id: actor.id }
        },
        !this.replaying
      );
      this.step(actor);
    }
  }

  private nextReadyActor(): ActorCell | null {
    if (this.ready.length === 0) {
      return null;
    }
    if (this.replaying) {
      const replayActorId = this.replaySteps[this.replayStepIndex++];
      const index = this.ready.indexOf(replayActorId);
      if (index >= 0) {
        return this.actors.get(this.ready.splice(index, 1)[0]!) ?? null;
      }
      throw new SchedulerRuntimeError("trap", `replay scheduler step ${replayActorId} is not ready`, "invalid_call");
    }
    const index = this.policy === "chaos" ? this.prng.nextIndex(this.ready.length) : 0;
    return this.actors.get(this.ready.splice(index, 1)[0]!) ?? null;
  }

  private step(actor: ActorCell): void {
    actor.ready = false;
    if (!actor.alive) {
      return;
    }
    if (actor.startPending) {
      actor.startPending = false;
      this.invokeLifecycle(actor, "on(start)", UNIT);
      this.requeueIfNeeded(actor);
      return;
    }
    const message = actor.mailbox.shift();
    if (!message) {
      return;
    }
    this.activeActorId = actor.id;
    this.activeMessage = message;
    this.trace.snapshot("actor.receive", this.messageSnapshot(message, actor));
    try {
      const handler = this.handler(actor, message.name);
      const args = [
        message.replyId ? { kind: "reply" as const, id: message.replyId } : UNIT,
        ...actor.state.map(cloneValue),
        ...message.payload.map(cloneValue)
      ];
      const returned = this.callbacks.invoke(handler.function, args);
      // Trampoline-fallback reply: if this was an ask and the handler returned
      // without executing actor.reply (e.g. via an early `return`), resolve the
      // reply slot now with the handler's return value. Mirrors native behaviour:
      // the native dispatch trampoline calls hew_reply() after the handler
      // function returns, unconditionally, regardless of where the return landed.
      // The resolved guard is REQUIRED — when the handler called actor.reply
      // normally the slot is already resolved, and we must NOT double-reply.
      if (message.replyId !== null) {
        const slot = this.replies.get(message.replyId);
        if (slot !== undefined && !slot.resolved) {
          slot.resolved = true;
          slot.value = cloneValue(returned);
          this.trace.snapshot("actor.reply", {
            actor_id: actor.id,
            reply_id: message.replyId,
            value: toJsonValue(returned),
            via: "trampoline_fallback"
          });
        }
      }
      this.updateStateFromReturn(actor, returned);
    } catch (error) {
      if (error instanceof ActorTurnCrash) {
        this.crashActor(actor, error.failure);
      } else if (error instanceof SchedulerRuntimeError && error.kind === "trap") {
        this.crashActor(actor, runtimeFailureFromSchedulerError(error));
      } else {
        throw error;
      }
    } finally {
      this.activeActorId = null;
      this.activeMessage = null;
    }
    this.requeueIfNeeded(actor);
  }

  private invokeLifecycle(actor: ActorCell, hookName: string, arg: VmValue): void {
    const hook = actor.layout.handlers.find((handler) => handler.name === hookName);
    if (!hook) {
      return;
    }
    this.activeActorId = actor.id;
    this.activeLifecycleHookName = hookName;
    this.trace.snapshot("actor.lifecycle", { actor_id: actor.id, hook: hookName });
    try {
      const returned = this.callbacks.invoke(hook.function, [arg, ...actor.state.map(cloneValue)]);
      if (hookName !== "on(crash)") {
        this.updateStateFromReturn(actor, returned);
      }
    } catch (error) {
      if (error instanceof ActorTurnCrash) {
        actor.alive = false;
        this.trace.snapshot("actor.hook-crash", {
          actor_id: actor.id,
          hook: hookName,
          failure: error.failure.message,
          trap_kind: error.failure.trap_kind
        });
      } else {
        throw error;
      }
    } finally {
      this.activeActorId = null;
      this.activeLifecycleHookName = null;
    }
  }

  private crashActor(actor: ActorCell, failure: RuntimeFailure): void {
    this.trace.snapshot("actor.crash", {
      actor_id: actor.id,
      failure: failure.message,
      trap_kind: failure.trap_kind
    });
    actor.alive = false;
    actor.ready = false;
    this.removeReady(actor.id);
    this.deliverLinkedExits(actor, failure);
    this.fireMonitors(actor, failure);
    const crashHook = actor.layout.handlers.find((handler) => handler.name === "on(crash)");
    if (crashHook) {
      this.activeActorId = actor.id;
      this.activeLifecycleHookName = "on(crash)";
      this.trace.snapshot("actor.lifecycle", { actor_id: actor.id, hook: "on(crash)" });
      try {
        this.callbacks.invoke(crashHook.function, [{ kind: "string", value: failure.message }, ...actor.state.map(cloneValue)]);
      } catch (error) {
        if (error instanceof ActorTurnCrash) {
          this.trace.snapshot("actor.hook-crash", {
            actor_id: actor.id,
            hook: "on(crash)",
            failure: error.failure.message,
            trap_kind: error.failure.trap_kind
          });
        } else {
          throw error;
        }
      } finally {
        this.activeActorId = null;
        this.activeLifecycleHookName = null;
      }
    }
    this.invokeLifecycle(actor, "on(stop)", UNIT);
    this.handleSupervisorChildCrash(actor, failure);
  }

  private handler(actor: ActorCell, name: string): ActorHandlerLayout {
    const handler = actor.layout.handlers.find((candidate) => candidate.name === name);
    if (!handler) {
      throw new SchedulerRuntimeError("trap", `actor handler ${name} not found`, "invalid_call");
    }
    return handler;
  }

  private updateStateFromReturn(actor: ActorCell, returned: VmValue): void {
    if (actor.layout.state_fields.length === 0 || returned.kind === "unit") {
      return;
    }
    if (actor.layout.state_fields.length === 1) {
      actor.state[0] = cloneValue(returned);
      this.trace.snapshot("actor.state", this.actorSnapshot(actor));
      this.enforceHeap(actor);
      return;
    }
    if (returned.kind === "record" && returned.fields.length === actor.layout.state_fields.length) {
      actor.state = returned.fields.map(cloneValue);
      this.trace.snapshot("actor.state", this.actorSnapshot(actor));
      this.enforceHeap(actor);
      return;
    }
    if (returned.kind === "record") {
      throw new ActorTurnCrash({
        kind: "trap",
        message: `actor state arity mismatch: expected ${actor.layout.state_fields.length}, got ${returned.fields.length}`,
        span: null,
        trap_kind: "invalid_record_field"
      });
    }
  }

  private markReady(actor: ActorCell): void {
    if (!actor.ready && actor.alive && (actor.startPending || actor.mailbox.length > 0)) {
      actor.ready = true;
      this.ready.push(actor.id);
    }
  }

  private requeueIfNeeded(actor: ActorCell): void {
    if (actor.alive && (actor.startPending || actor.mailbox.length > 0)) {
      this.markReady(actor);
    }
  }

  private startChild(
    supervisor: SupervisorCell,
    slot: ChildSlot,
    actorLayouts: Map<string, ActorLayout>,
    reason: "initial" | "restart",
    span: TraceSpan | null
  ): void {
    const layout = actorLayouts.get(slot.spec.actorLayoutId);
    slot.generation += 1;
    if (!layout) {
      slot.actorId = null;
      slot.status = "failed";
      slot.lastFailure = {
        kind: "trap",
        message: `M6_START_SPEC_FAILED: actor layout ${slot.spec.actorLayoutId} not found`,
        span,
        trap_kind: "invalid_call"
      };
      this.trace.snapshot("child-crashed", this.childSnapshot(supervisor, slot, { reason: "start_spec_failed" }), span);
      this.handleChildStartFailure(supervisor, slot, actorLayouts, span);
      return;
    }
    const actor = this.spawnActor(layout, slot.spec.initialState, supervisor.id, supervisor.id, slot.spec.id, slot.generation, span);
    slot.actorId = actor.id;
    slot.status = "running";
    slot.lastFailure = null;
    this.trace.snapshot("child-spawned", this.childSnapshot(supervisor, slot, {
      actor_id: actor.id,
      reason,
      max_heap: layout.max_heap ?? null
    }), span);
  }

  private handleChildStartFailure(
    supervisor: SupervisorCell,
    slot: ChildSlot,
    actorLayouts: Map<string, ActorLayout>,
    span: TraceSpan | null
  ): void {
    while (true) {
      const decision = this.reserveRestart(supervisor);
      if (!decision.allowed) {
        this.exhaustSupervisor(supervisor, slot, slot.lastFailure ?? stoppedFailure(slot.spec.id), span);
        return;
      }
      this.trace.snapshot("restart-decision", {
        supervisor_id: supervisor.id,
        child_id: slot.spec.id,
        actor_id: slot.actorId,
        policy: supervisor.layout.strategy,
        restart_class: slot.spec.restart,
        decision: "restart",
        reason: "start_spec_failed",
        budget_remaining: decision.remaining,
        window_ms: supervisor.layout.restartWindowMs,
        virtual_time_ms: this.trace.virtualTimeMs
      }, span);
      slot.generation += 1;
      this.trace.snapshot("child-crashed", this.childSnapshot(supervisor, slot, { reason: "start_spec_failed" }), span);
    }
  }

  private handleSupervisorChildCrash(actor: ActorCell, failure: RuntimeFailure): void {
    if (!actor.supervisorId || !actor.childId) {
      return;
    }
    const supervisor = this.supervisors.get(actor.supervisorId);
    if (!supervisor?.alive) {
      return;
    }
    const slotIndex = supervisor.slots.findIndex((slot) => slot.actorId === actor.id);
    if (slotIndex < 0) {
      return;
    }
    const slot = supervisor.slots[slotIndex]!;
    slot.status = "failed";
    slot.lastFailure = failure;
    this.trace.snapshot("child-crashed", this.childSnapshot(supervisor, slot, {
      actor_id: actor.id,
      failure: failure.message,
      trap_kind: failure.trap_kind
    }));
    if (!this.shouldRestart(slot, failure)) {
      this.trace.snapshot("restart-decision", {
        supervisor_id: supervisor.id,
        child_id: slot.spec.id,
        actor_id: actor.id,
        policy: supervisor.layout.strategy,
        restart_class: slot.spec.restart,
        decision: "stop",
        budget_remaining: this.restartBudgetRemaining(supervisor),
        virtual_time_ms: this.trace.virtualTimeMs
      });
      this.trace.snapshot("supervisor-tree-state", this.supervisorSnapshot(supervisor));
      return;
    }
    const decision = this.reserveRestart(supervisor);
    if (!decision.allowed) {
      this.trace.snapshot("restart-decision", {
        supervisor_id: supervisor.id,
        child_id: slot.spec.id,
        actor_id: actor.id,
        policy: supervisor.layout.strategy,
        restart_class: slot.spec.restart,
        decision: "escalate",
        budget_remaining: 0,
        virtual_time_ms: this.trace.virtualTimeMs
      });
      this.exhaustSupervisor(supervisor, slot, failure, null);
      return;
    }
    this.trace.snapshot("restart-decision", {
      supervisor_id: supervisor.id,
      child_id: slot.spec.id,
      actor_id: actor.id,
      policy: supervisor.layout.strategy,
      restart_class: slot.spec.restart,
      decision: "restart",
      budget_remaining: decision.remaining,
      virtual_time_ms: this.trace.virtualTimeMs
    });
    const indexes = this.restartIndexes(supervisor, slotIndex);
    for (const index of indexes) {
      const restartSlot = supervisor.slots[index]!;
      if (restartSlot.actorId && restartSlot.actorId !== actor.id) {
        const sibling = this.actors.get(restartSlot.actorId);
        if (sibling?.alive) {
          sibling.alive = false;
          sibling.ready = false;
          this.removeReady(sibling.id);
          this.deliverLinkedExits(sibling, stoppedFailure(sibling.id));
          this.fireMonitors(sibling, stoppedFailure(sibling.id));
        }
      }
      restartSlot.status = "restarting";
      this.startChild(supervisor, restartSlot, new Map([...this.actors.values()].map((cell) => [cell.layout.id, cell.layout])), "restart", null);
    }
    this.trace.snapshot("supervisor-tree-state", this.supervisorSnapshot(supervisor));
  }

  private shouldRestart(slot: ChildSlot, failure: RuntimeFailure): boolean {
    if (slot.spec.restart === "temporary") {
      return false;
    }
    if (slot.spec.restart === "transient") {
      return failure.kind !== "internal_error";
    }
    return true;
  }

  private restartIndexes(supervisor: SupervisorCell, crashedIndex: number): number[] {
    switch (supervisor.layout.strategy) {
      case "one_for_all":
        return supervisor.slots.map((_, index) => index);
      case "rest_for_one":
        return supervisor.slots.map((_, index) => index).filter((index) => index >= crashedIndex);
      case "one_for_one":
        return [crashedIndex];
    }
  }

  private reserveRestart(supervisor: SupervisorCell): { allowed: true; remaining: number } | { allowed: false; remaining: 0 } {
    const now = this.trace.virtualTimeMs;
    const window = supervisor.layout.restartWindowMs;
    supervisor.restartHistory = supervisor.restartHistory.filter((startedAt) => window <= 0 || now - startedAt < window);
    if (supervisor.restartHistory.length >= supervisor.layout.restartIntensity) {
      return { allowed: false, remaining: 0 };
    }
    supervisor.restartHistory.push(now);
    return { allowed: true, remaining: Math.max(0, supervisor.layout.restartIntensity - supervisor.restartHistory.length) };
  }

  private restartBudgetRemaining(supervisor: SupervisorCell): number {
    const now = this.trace.virtualTimeMs;
    const window = supervisor.layout.restartWindowMs;
    const used = supervisor.restartHistory.filter((startedAt) => window <= 0 || now - startedAt < window).length;
    return Math.max(0, supervisor.layout.restartIntensity - used);
  }

  private exhaustSupervisor(supervisor: SupervisorCell, slot: ChildSlot, failure: RuntimeFailure, span: TraceSpan | null): void {
    supervisor.alive = false;
    this.trace.snapshot("budget-exhausted", {
      supervisor_id: supervisor.id,
      child_id: slot.spec.id,
      policy: supervisor.layout.strategy,
      restart_intensity: supervisor.layout.restartIntensity,
      window_ms: supervisor.layout.restartWindowMs,
      virtual_time_ms: this.trace.virtualTimeMs,
      failure: failure.message,
      trap_kind: failure.trap_kind
    }, span);
    this.trace.snapshot("supervisor-escalated", {
      supervisor_id: supervisor.id,
      parent_supervisor_id: supervisor.parentSupervisorId,
      reason: "restart_budget_exhausted"
    }, span);
    if (!supervisor.parentSupervisorId) {
      throw new SchedulerRuntimeError("trap", `M6_SUPERVISOR_BUDGET_EXHAUSTED: ${supervisor.id}`, "budget_exhausted");
    }
  }

  private deliverLinkedExits(actor: ActorCell, failure: RuntimeFailure): void {
    const linked = [...(this.links.get(actor.id) ?? [])].sort();
    this.links.delete(actor.id);
    for (const otherId of linked) {
      this.links.get(otherId)?.delete(actor.id);
      this.trace.snapshot("link-broken-by-death", { actor_id: actor.id, linked_actor_id: otherId });
      const other = this.actors.get(otherId);
      if (other?.alive) {
        this.enqueueSystem(other, "link.exit", [exitSignal(actor.id, failure)], null);
      }
    }
  }

  private fireMonitors(actor: ActorCell, failure: RuntimeFailure): void {
    const monitors = [...this.monitors.values()]
      .filter((monitor) => monitor.active && monitor.targetId === actor.id)
      .sort((left, right) => left.id.localeCompare(right.id));
    for (const monitor of monitors) {
      this.fireMonitor(monitor, failure, null);
    }
  }

  private fireMonitor(monitor: MonitorCell, failure: RuntimeFailure, span: TraceSpan | null): void {
    if (!monitor.active) {
      return;
    }
    monitor.active = false;
    this.trace.snapshot("monitor-fired", {
      monitor_id: monitor.id,
      owner_actor_id: monitor.ownerId,
      target_actor_id: monitor.targetId,
      trap_kind: failure.trap_kind,
      failure: failure.message
    }, span);
    const owner = this.actors.get(monitor.ownerId);
    if (owner?.alive) {
      this.enqueueSystem(owner, "monitor.down", [monitorSignal(monitor.id, monitor.targetId, failure)], null);
    }
  }

  private enqueueSystem(actor: ActorCell, name: string, payload: VmValue[], replyId: string | null): void {
    const message: ActorMessage = {
      id: this.ids.message(),
      from: "actor:system",
      to: actor.id,
      name,
      payload: payload.map(cloneValue),
      replyId
    };
    actor.mailbox.push(message);
    this.trace.snapshot("actor.send", this.messageSnapshot(message, actor));
    this.emitWatermark(actor);
    this.markReady(actor);
  }

  private linkSet(actorId: string): Set<string> {
    const existing = this.links.get(actorId);
    if (existing) {
      return existing;
    }
    const created = new Set<string>();
    this.links.set(actorId, created);
    return created;
  }

  private removeReady(actorId: string): void {
    for (let index = this.ready.length - 1; index >= 0; index -= 1) {
      if (this.ready[index] === actorId) {
        this.ready.splice(index, 1);
      }
    }
  }

  private enforceHeap(actor: ActorCell): void {
    if (actor.layout.max_heap === undefined) {
      return;
    }
    const used = actor.state.reduce((total, value) => total + heapUnits(value), 0);
    this.trace.snapshot("actor.heap", { actor_id: actor.id, used, max_heap: actor.layout.max_heap });
    if (used > actor.layout.max_heap) {
      throw new ActorTurnCrash({
        kind: "budget_exhausted",
        message: `actor ${actor.id} max_heap exceeded`,
        span: null,
        trap_kind: "budget_exhausted"
      });
    }
  }

  private emitWatermark(actor: ActorCell): void {
    const maxMailbox = actor.layout.max_mailbox ?? DEFAULT_MAX_MAILBOX;
    const percent = Math.ceil((actor.mailbox.length / maxMailbox) * 100);
    const threshold = WATERMARK_THRESHOLDS.find((candidate) => percent <= candidate) ?? 100;
    if (threshold > actor.watermark) {
      actor.watermark = threshold;
      this.trace.snapshot("actor.mailbox-watermark", {
        actor_id: actor.id,
        mailbox_depth: actor.mailbox.length,
        max_mailbox: maxMailbox,
        threshold
      });
    }
  }

  private actorForValue(value: VmValue): ActorCell {
    if (value.kind !== "actor") {
      throw new SchedulerRuntimeError("trap", "expected actor operand", "invalid_local");
    }
    const actor = this.actors.get(value.id);
    if (!actor) {
      throw new SchedulerRuntimeError("trap", `actor ${value.id} not found`, "invalid_call");
    }
    return actor;
  }

  private actorSnapshot(actor: ActorCell, extra: Record<string, JsonValue> = {}): JsonValue {
    const snapshot: Record<string, JsonValue> = {
      actor_id: actor.id,
      layout: actor.layout.id,
      alive: actor.alive,
      mailbox_depth: actor.mailbox.length,
      state: actor.state.map(toJsonValue),
      ...extra
    };
    if (actor.supervisorId !== null) {
      snapshot.supervisor_id = actor.supervisorId;
      snapshot.child_id = actor.childId;
      snapshot.generation = actor.generation;
    }
    return snapshot;
  }

  private messageSnapshot(message: ActorMessage, actor: ActorCell): JsonValue {
    return {
      actor_id: actor.id,
      from: message.from,
      handler: message.name,
      mailbox_depth: actor.mailbox.length,
      message_id: message.id,
      payload: message.payload.map(toJsonValue),
      reply_id: message.replyId
    };
  }

  private supervisorSnapshot(supervisor: SupervisorCell): JsonValue {
    return {
      supervisor_id: supervisor.id,
      layout: supervisor.layout.id,
      strategy: supervisor.layout.strategy,
      restart_intensity: supervisor.layout.restartIntensity,
      restart_window_ms: supervisor.layout.restartWindowMs,
      alive: supervisor.alive,
      restart_budget_remaining: this.restartBudgetRemaining(supervisor),
      children: supervisor.slots.map((slot) => this.childSnapshot(supervisor, slot))
    };
  }

  private childSnapshot(supervisor: SupervisorCell, slot: ChildSlot, extra: Record<string, JsonValue> = {}): JsonValue {
    return {
      supervisor_id: supervisor.id,
      child_id: slot.spec.id,
      actor_id: slot.actorId,
      actor_layout: slot.spec.actorLayoutId,
      restart_class: slot.spec.restart,
      generation: slot.generation,
      status: slot.status,
      ...extra
    };
  }
}

function isSchedulerStepInput(value: JsonValue): boolean {
  return (
    typeof value === "object" &&
    value !== null &&
    !Array.isArray(value) &&
    value.family === REPLAY_FAMILY &&
    typeof value.actor_id === "string"
  );
}

function runtimeFailureFromSchedulerError(error: SchedulerRuntimeError): RuntimeFailure {
  return {
    kind: error.trapKind === "budget_exhausted" ? "budget_exhausted" : "trap",
    message: error.message,
    span: null,
    trap_kind: error.trapKind
  };
}

function stoppedFailure(actorId: string): RuntimeFailure {
  return {
    kind: "trap",
    message: `actor ${actorId} stopped`,
    span: null,
    trap_kind: "invalid_call"
  };
}

function exitSignal(actorId: string, failure: RuntimeFailure): VmValue {
  return {
    kind: "record",
    typeId: "type:sandbox.ExitSignal",
    fields: [
      { kind: "string", value: actorId },
      { kind: "string", value: failure.trap_kind ?? "unknown" },
      { kind: "string", value: failure.message }
    ]
  };
}

function monitorSignal(monitorId: string, actorId: string, failure: RuntimeFailure): VmValue {
  return {
    kind: "record",
    typeId: "type:sandbox.MonitorDown",
    fields: [
      { kind: "string", value: monitorId },
      { kind: "string", value: actorId },
      { kind: "string", value: failure.trap_kind ?? "unknown" },
      { kind: "string", value: failure.message }
    ]
  };
}

function heapUnits(value: VmValue): number {
  switch (value.kind) {
    case "unit":
    case "bool":
    case "i64":
    case "f64":
    case "actor":
    case "supervisor":
    case "monitor":
    case "reply":
    case "channel":
    case "task":
    case "stream":
    case "sink":
    case "duplex":
    case "regex":
      return 1;
    case "string":
      return Math.max(1, value.value.length);
    case "record":
      return 1 + value.fields.reduce((total, field) => total + heapUnits(field), 0);
    case "enum":
      return 1 + value.payload.reduce((total, field) => total + heapUnits(field), 0);
    case "vector":
      return 1 + value.items.reduce((total, item) => total + heapUnits(item), 0);
    case "function":
      return 1;
  }
}
