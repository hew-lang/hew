/// The `hew.sandbox.bytecode.v1` executor.
///
/// The package is a projection of verified ownership SIR, so this executor
/// carries out the decisions it records and makes none of its own: it copies
/// where the package says `copy_value`, releases where it says `destroy_value`,
/// follows the `unwind` edge the package names, and takes the failure edge a
/// `checked.binary` names rather than trapping on its own.

import { TraceBuilder, runtimeFailure } from "../trace.js";
import type {
  JsonValue,
  ReplayConfig,
  RunOptions,
  RuntimeStatus,
  SandboxTrace,
  TrapKind,
} from "../types.js";
import { UNIT, cloneValue, toJsonValue, type VmValue } from "../values.js";
import type {
  ActorShape,
  SupervisorShape,
  ActorProtocol,
  RequestShapes,
  BlockV1,
  BoundaryDecision,
  BoundaryOperand,
  CallResult,
  ClosureShape,
  Edge,
  FunctionV1,
  OpV1,
  PackageV1,
  TermV1,
  TrapName,
  ValueDef,
  VariantShape,
} from "./package.js";
import { StructuralRenderer } from "./structural.js";
import {
  selectedValue,
  collectionOperation,
  sharedOperation,
  type ValueProgram,
} from "./value-operations.js";
import { Pipes } from "./pipes.js";
import { FrameScheduler } from "./scheduler.js";
import { DeterministicIds } from "../../scheduler/ids.js";
import { Mt19937 } from "./mt19937.js";
import {
  ShimFault,
  resolveExternShim,
  resolveRuntimeShim,
  type ShimHost,
} from "./shims.js";
import { admitPackage } from "./validate.js";

const DEFAULT_STEP_BUDGET = 1_000_000;

/// A storage cell. Every SSA value and every place owns one; a loan refers to
/// one rather than holding a snapshot of its contents.
interface Cell {
  value: VmValue;
  valid: boolean;
}

/// Where a value id or a place currently lives. A projected loan is a
/// `(container, key)` reference, so writing through it is visible in the
/// container and a later `store.assign` is visible through the loan.
type Ref =
  | { kind: "cell"; cell: Cell }
  | { kind: "field"; parent: Ref; index: number }
  | { kind: "payload"; parent: Ref; index: number };

type Fault =
  | { kind: "panic"; message: string; cancelled?: boolean; deadline?: boolean }
  | { kind: "trap"; trap: TrapName; message?: string };

interface FrameContext {
  id: string;
  actor: ActorInstance | null;
  cancel?: (fault?: Fault) => void;
  generator?: GeneratorEntry;
  returned(value: VmValue): void;
  failed(fault: Fault): void;
}

interface GeneratorEntry {
  id: string;
  frame: Activation;
  callable: VmValue;
  started: boolean;
  done: boolean;
  fault: Fault | null;
  resume?: () => void;
  receive?: (value: VmValue | null, fault: Fault | null) => void;
  closed: Array<(fault: Fault | null) => void>;
}

interface ActorMessage {
  handler: ActorShape["handlers"][number];
  payload: VmValue[];
  reply: boolean;
  complete(value: VmValue | null, error: string | null): void;
}

interface ActorInstance {
  id: string;
  layout: ActorShape;
  state: VmValue;
  mailbox: ActorMessage[];
  admission: Array<() => void>;
  busy: boolean;
  alive: boolean;
  completed: boolean;
  closing: boolean;
  drainingMailbox: boolean;
  mailboxDrained: Array<() => void>;
  closingFault: Fault | null;
  crashing: Fault | null;
  active: FrameContext | null;
  closed: Array<() => void>;
  supervisor?: { owner: SupervisorInstance; child: number };
}

interface SupervisorInstance {
  id: string;
  layout: SupervisorShape;
  config: VmValue[];
  children: Array<VmValue | null>;
  alive: boolean;
  restartTimes: number[];
  specs: SupervisorShape["children"];
  offsets: number[];
  recovering: Set<number>;
  restartBusy: boolean;
  pendingRestarts: Array<{ child: number; debts: number[] }>;
  spent: Set<number>;
  completed: boolean;
  supervisor?: { owner: SupervisorInstance; child: number };
  closed: Array<() => void>;
}

interface RoleSlot {
  ownerId: string;
  child: number;
  latest: VmValue | null;
  waiting: Array<() => void>;
}

interface TaskEntry {
  id: string;
  done: boolean;
  finishing: boolean;
  value: VmValue;
  fault: Fault | null;
  actorError: string | null;
  cancel(): void;
  completedAt: number;
  changed: Array<() => void>;
}

interface TaskGroup {
  tasks: TaskEntry[];
  changed: Array<() => void>;
  deadline: Fault | null;
  joining: boolean;
  cancelTimer?: () => void;
}

interface Activation {
  context: FrameContext;
  fn: FunctionV1;
  block: BlockV1;
  opIndex: number;
  env: Map<number, Ref>;
  trivial: Set<number>;
  scopes: Map<number, TaskGroup>;
  places: Map<number, Cell>;
  fault: Fault | null;
  /// Faults parked by the `enter_defer` boundaries this activation is inside,
  /// innermost last. A deferred body runs with its own fault state; its
  /// `finish_defer` restores the fault the boundary was entered with.
  parked: Array<Fault | null>;
  caller: Activation | null;
  /// Where the caller binds this activation's return value.
  result: CallResult;
  normal: Edge | undefined;
  unwind: Edge | null;
}

/// Ends the run. The status is already recorded on the trace.
class Halt extends Error {
  constructor(readonly status: RuntimeStatus) {
    super(status);
  }
}

/// Page stdin travels as a replay input and is handed out one line per read at
/// run time, so a loop sees successive lines.
class StdinReader {
  private readonly bytes: Uint8Array;
  private offset = 0;

  constructor(stdin: string) {
    this.bytes = new TextEncoder().encode(stdin);
  }

  static fromReplay(
    inputs: ReadonlyArray<{ kind: string; data: JsonValue }>,
  ): StdinReader {
    return new StdinReader(
      inputs
        .filter((input) => input.kind === "stdin")
        .map((input) => (typeof input.data === "string" ? input.data : ""))
        .join(""),
    );
  }

  readLine(): string {
    if (this.offset >= this.bytes.length) {
      return "";
    }
    const start = this.offset;
    while (
      this.offset < this.bytes.length &&
      this.bytes[this.offset] !== 0x0a
    ) {
      this.offset += 1;
    }
    let end = this.offset;
    if (this.offset < this.bytes.length) {
      this.offset += 1;
    }
    if (end > start && this.bytes[end - 1] === 0x0d) {
      end -= 1;
    }
    return new TextDecoder().decode(this.bytes.slice(start, end));
  }
}

export function runPackageV1(
  pkg: PackageV1,
  options: RunOptions = {},
): SandboxTrace {
  const fixtureId = options.fixtureId ?? "package";
  const replay: ReplayConfig = {
    seed: options.replay?.seed ?? 0,
    step_budget:
      options.stepBudget ?? options.replay?.step_budget ?? DEFAULT_STEP_BUDGET,
    virtual_clock: options.replay?.virtual_clock
      ? { ...options.replay.virtual_clock }
      : { epoch_ms: 0, tick_ms: 1, current_ms: 0 },
    inputs: [...(options.replay?.inputs ?? [])],
  };
  const trace = new TraceBuilder(
    pkg,
    fixtureId,
    options.traceId ?? `trace:${fixtureId}`,
    replay,
    options.sandboxVersion ?? "0.0.0-spec",
  );

  // Admission is a load-time decision: nothing runs when the package names a
  // capability the VM has no shim for.
  const rejection = admitPackage(pkg);
  if (rejection) {
    trace.reject(rejection);
    return trace.finish();
  }

  const vm = new ExecutorV1(
    pkg,
    trace,
    StdinReader.fromReplay(replay.inputs),
    options.schedulerPolicy ?? "round_robin",
  );
  try {
    vm.run();
  } catch (error) {
    if (!(error instanceof Halt)) {
      const message = error instanceof Error ? error.message : String(error);
      trace.fail(
        "runtime_failure",
        "runtime.failure",
        runtimeFailure(
          "internal_error",
          `sandbox internal error: ${message}`,
          "internal_error",
          null,
        ),
      );
    }
  }
  return trace.finish();
}

class ExecutorV1 {
  private current: Activation;
  private readonly host: ShimHost;
  private readonly scheduler: FrameScheduler;
  private readonly ids: DeterministicIds;
  private readonly actors = new Map<string, ActorInstance>();
  private readonly supervisors = new Map<string, SupervisorInstance>();
  private readonly roles = new Map<string, RoleSlot>();
  private readonly pipes = new Pipes();
  private readonly generators = new Map<string, GeneratorEntry>();
  private readonly tasks = new Map<string, TaskEntry>();
  private completedTasks = 0;
  private readonly faultDebts = new Set<number>();
  private nextFaultDebt = 0;
  private readonly closingResources = new WeakSet<object>();
  private readonly consumedResources = new WeakSet<object>();
  private running = false;
  private rootComplete = false;
  private rootFault: Fault | null = null;

  constructor(
    private readonly pkg: PackageV1,
    private readonly trace: TraceBuilder,
    stdin: StdinReader,
    policy: "round_robin" | "chaos",
  ) {
    this.scheduler = new FrameScheduler(trace, policy);
    this.ids = new DeterministicIds(trace.replay.seed);
    this.host = {
      pipes: this.pipes,
      newPipe: (capacity) => this.newPipe(capacity),
      closePipe: (value) =>
        this.pipes.close(value, this.pipeFault(this.current)),
      releaseValue: (value) =>
        this.closeValue(value, this.pipeFault(this.current)),
      writeStdout: (text) => this.trace.writeStdout(text, null),
      readLine: () => {
        const line = stdin.readLine();
        this.trace.recordReplayInput({ kind: "stdin", data: line }, false);
        return line;
      },
      prng: new Mt19937(),
      regexPatterns: pkg.regex_patterns,
      enumValue: (shape, caseName, payload) => ({
        kind: "enum",
        typeId: shape.name,
        // The tag is the case's position in the descriptor's declaration order.
        tag: shape.cases.findIndex((entry) => entry.name === caseName),
        payload,
      }),
    };
    // `entry` is present: admission refused the package otherwise.
    this.current = this.activate(
      this.functionAt(pkg.entry!.function),
      [],
      null,
      null,
      undefined,
      null,
      {
        id: "actor:root",
        actor: null,
        returned: (value) => {
          this.rootComplete = true;
          this.trace.exitCode =
            pkg.entry?.exit === "status" && value.kind === "i64"
              ? Number(value.value)
              : 0;
        },
        failed: (fault) => {
          this.rootFault = fault;
          this.rootComplete = true;
        },
      },
    );
  }

  /// One instruction or terminator executed is one step, ownership ops
  /// included: the package's ops are what the program does.
  run(): void {
    this.runFrame(this.current);
    this.scheduler.run();
    if (!this.rootComplete) {
      throw new Error("the root frame is waiting with no runnable work");
    }
    // Native main return drains accepted work before closing idle actors.
    for (const owner of this.supervisors.values())
      if (!owner.supervisor) this.stopSupervisor(owner);
    for (const actor of this.actors.values())
      if (!actor.supervisor && actor.alive) this.requestActorClose(actor);
    this.scheduler.run();
    if (this.rootFault) this.haltWithFault(this.rootFault);
    if (this.trace.exitCode === 0 && this.faultDebts.size)
      this.trace.exitCode = 1;
  }

  private runFrame(frame: Activation): void {
    this.current = frame;
    this.running = true;
    while (this.running) {
      const act = this.current;
      while (act.opIndex < act.block.ops.length) {
        const op = act.block.ops[act.opIndex++]!;
        this.commitStep();
        try {
          this.execute(act, op);
        } catch (error) {
          if (error instanceof Halt) throw error;
          throw new Error(
            `${act.fn.name} block ${act.block.id} ${op.op}: ${String(error)}`,
          );
        }
        if (!this.running) return;
      }
      if (!this.running) return;
      this.commitStep();
      try {
        this.terminate(act, act.block.term);
      } catch (error) {
        if (error instanceof Halt) throw error;
        throw new Error(
          `${act.fn.name} block ${act.block.id} ${act.block.term.op}: ${String(error)}`,
        );
      }
    }
  }

  // ── frames ───────────────────────────────────────────────────────────────

  private activate(
    fn: FunctionV1,
    args: VmValue[],
    caller: Activation | null,
    result: CallResult,
    normal: Edge | undefined,
    unwind: Edge | null,
    context?: FrameContext,
  ): Activation {
    if (
      (this.pkg.resources ?? []).some((resource) => resource.close === fn.id) &&
      args[0]
    )
      this.closingResources.add(args[0]);
    const env = new Map<number, Ref>();
    for (const [at, param] of fn.params.entries()) {
      env.set(param.value, ownedRef(args[at] ?? UNIT));
    }
    const places = new Map<number, Cell>();
    const trivial = new Set<number>();
    const record = (value: ValueDef) => {
      if (value.own === "none") trivial.add(value.value);
    };
    fn.params.forEach(record);
    for (const block of fn.blocks) {
      block.params.forEach(record);
      for (const op of block.ops) {
        if (op.dst != null && op.own === "none") trivial.add(op.dst);
      }
      const term = block.term;
      if ("result" in term && term.result && term.result !== "never")
        record(term.result);
      if (term.op === "switch.variant")
        for (const arm of term.arms) arm.fields.forEach(record);
    }
    return {
      context: caller?.context ?? context!,
      fn,
      block: this.blockAt(fn, fn.entry),
      opIndex: 0,
      env,
      trivial,
      scopes: new Map(),
      places,
      fault: null,
      parked: [],
      caller,
      result,
      normal,
      unwind,
    };
  }

  private functionAt(id: number): FunctionV1 {
    const fn =
      this.pkg.functions[id] ??
      this.pkg.functions.find((candidate) => candidate.id === id);
    if (!fn) {
      throw new Error(`function ${id} is not in the package`);
    }
    return fn;
  }

  private blockAt(fn: FunctionV1, id: number): BlockV1 {
    const block =
      fn.blocks[id] ?? fn.blocks.find((candidate) => candidate.id === id);
    if (!block) {
      throw new Error(`${fn.name}: block ${id} is not in the function`);
    }
    return block;
  }

  private commitStep(): void {
    if (this.trace.budgetRemaining === 0) {
      this.trace.fail(
        "budget_exhausted",
        "budget.exhausted",
        runtimeFailure(
          "budget_exhausted",
          "step budget exhausted",
          "budget_exhausted",
          null,
        ),
      );
      throw new Halt("budget_exhausted");
    }
    this.trace.stepCount += 1;
  }

  // ── values, refs and places ──────────────────────────────────────────────

  private refOf(act: Activation, id: number): Ref {
    const ref = act.env.get(id);
    if (!ref) {
      throw new Error(
        `${act.fn.name}: value ${id} is not defined on this path`,
      );
    }
    return ref;
  }

  private read(act: Activation, id: number): VmValue {
    try {
      return readRef(this.refOf(act, id));
    } catch (error) {
      throw new Error(
        `${act.fn.name} block ${act.block.id} value ${id}: ${(error as Error).message}`,
      );
    }
  }

  private define(
    act: Activation,
    id: number | null | undefined,
    value: VmValue,
  ): void {
    if (id === null || id === undefined) {
      return;
    }
    act.env.set(id, ownedRef(value));
  }

  private invalidate(act: Activation, id: number): void {
    // A boundary's transfer of a trivial value has no ownership to end. SIR
    // can use that SSA value again, including a supervisor or child role.
    if (!act.trivial.has(id)) invalidateRef(this.refOf(act, id));
  }

  /// Where a place currently lives. Only a `local` is a cell of its own; a
  /// projected place is a `(container, field)` reference through the storage
  /// its base already holds, so a store through it is visible in the container
  /// and a loan of it sees a later `store.assign`.
  private placeRef(act: Activation, id: number): Ref {
    const decl =
      act.fn.places[id] ??
      act.fn.places.find((candidate) => candidate.id === id);
    if (!decl) {
      throw new Error(`${act.fn.name}: place ${id} is not declared`);
    }
    switch (decl.origin) {
      case "local": {
        const cell = act.places.get(id);
        if (!cell) {
          throw new Error(
            `${act.fn.name}: place ${id} was never created by alloc_place`,
          );
        }
        return { kind: "cell", cell };
      }
      case "aggregate":
        return {
          kind: "field",
          parent:
            "place" in decl.base
              ? this.placeRef(act, decl.base.place)
              : this.refOf(act, decl.base.value),
          index: decl.field,
        };
      case "actor_state":
      case "capture":
        // `environment` is the value id the body holds its environment under,
        // and the place is one field of it.
        return {
          kind: "field",
          parent: this.refOf(act, decl.environment),
          index: decl.field,
        };
      default:
        throw new Error(
          `${act.fn.name}: place origin ${decl.origin} has no sequential executor`,
        );
    }
  }

  /// Bind a call or suspension operand according to the decision the boundary
  /// owns: a copy or snapshot deep-clones, a move transfers and invalidates,
  /// and a borrow passes the live value.
  private boundary(act: Activation, operand: BoundaryOperand): VmValue {
    const value = this.read(act, operand.value);
    switch (operand.decision) {
      case "copy":
      case "snapshot":
        return cloneValue(value);
      case "move":
        this.invalidate(act, operand.value);
        return value;
      default:
        return value;
    }
  }

  // ── instructions ─────────────────────────────────────────────────────────

  private execute(act: Activation, op: OpV1): void {
    switch (op.op) {
      case "const.int":
        this.define(act, op.dst, {
          kind: "i64",
          value: narrowInt(BigInt(op.value), op.ty),
        });
        return;
      case "const.bool":
        this.define(act, op.dst, { kind: "bool", value: op.value });
        return;
      case "const.float":
        this.define(act, op.dst, { kind: "f64", value: floatConst(op) });
        return;
      case "const.char":
        this.define(act, op.dst, { kind: "string", value: op.value });
        return;
      case "const.unit":
        this.define(act, op.dst, UNIT);
        return;
      case "const.duration":
        this.define(act, op.dst, { kind: "i64", value: BigInt(op.nanos) });
        return;
      case "const.str":
        this.define(act, op.dst, {
          kind: "string",
          value: this.pkg.strings[op.str] ?? "",
        });
        return;
      case "const.bytes":
        this.define(act, op.dst, bytesValue(this.pkg.bytes[op.bytes] ?? []));
        return;

      case "copy_value":
      case "fork":
        this.define(act, op.dst, cloneValue(this.read(act, op.source)));
        return;
      case "move":
        this.define(act, op.dst, this.read(act, op.source));
        this.invalidate(act, op.source);
        return;
      case "destroy_value": {
        const value = this.read(act, op.value);
        this.invalidate(act, op.value);
        this.releaseOperation(act, value);
        return;
      }
      case "begin_borrow":
        act.env.set(op.dst, this.refOf(act, op.owner));
        return;
      case "end_borrow":
      case "finish_linear_receiver":
        return;

      case "alloc_place":
        act.places.set(op.place, { value: UNIT, valid: false });
        return;
      case "store.init":
      case "store.assign":
        writeRef(this.placeRef(act, op.place), this.read(act, op.value));
        return;
      case "load.copy":
        this.define(
          act,
          op.dst,
          cloneValue(readRef(this.placeRef(act, op.place))),
        );
        return;
      case "load.take": {
        const ref = this.placeRef(act, op.place);
        this.define(act, op.dst, readRef(ref));
        invalidateRef(ref);
        return;
      }
      case "load.borrow":
        act.env.set(op.dst, this.placeRef(act, op.place));
        return;
      case "end_lifetime": {
        const ref = this.placeRef(act, op.place);
        const value = liveRef(ref) ? readRef(ref) : UNIT;
        invalidateRef(ref);
        this.releaseOperation(act, value);
        return;
      }

      case "tuple.make":
        this.define(act, op.dst, {
          kind: "record",
          typeId: "",
          fields: op.elements.map((id) => this.read(act, id)),
        });
        return;
      case "tuple.get":
        this.define(
          act,
          op.dst,
          cloneValue(fieldsOf(this.read(act, op.tuple))[op.index] ?? UNIT),
        );
        return;
      case "aggregate.make":
        this.define(act, op.dst, {
          kind: "record",
          typeId: this.aggregateName(op.shape),
          fields: op.fields.map((id) => this.read(act, id)),
        });
        return;
      case "aggregate.project_copy":
        this.define(
          act,
          op.dst,
          cloneValue(fieldsOf(this.read(act, op.aggregate))[op.field] ?? UNIT),
        );
        return;
      case "aggregate.project_borrow":
        act.env.set(op.dst, {
          kind: "field",
          parent: this.refOf(act, op.aggregate),
          index: op.field,
        });
        return;
      case "destructure":
        this.spread(act, fieldsOf(this.read(act, op.aggregate)), op.results);
        this.invalidate(act, op.aggregate);
        return;
      case "array.make":
        this.define(act, op.dst, {
          kind: "vector",
          elementType: "",
          items: op.elements.map((id) => this.read(act, id)),
        });
        return;
      case "array.repeat":
        this.define(act, op.dst, {
          kind: "vector",
          elementType: "",
          items: Array.from({ length: op.count ?? 0 }, () =>
            cloneValue(this.read(act, op.value)),
          ),
        });
        return;

      case "variant.make":
        this.define(act, op.dst, {
          kind: "enum",
          typeId: this.variantName(op.shape),
          tag: op.variant,
          payload: op.fields.map((id) => this.read(act, id)),
        });
        return;
      case "variant.is":
        this.define(act, op.dst, {
          kind: "bool",
          value: tagOf(this.read(act, op.source)) === op.variant,
        });
        return;
      case "variant.project_copy":
        this.define(
          act,
          op.dst,
          cloneValue(payloadOf(this.read(act, op.source))[op.field] ?? UNIT),
        );
        return;
      case "variant.project_borrow":
        act.env.set(op.dst, {
          kind: "payload",
          parent: this.refOf(act, op.source),
          index: op.field,
        });
        return;
      case "variant.destructure":
        this.spread(act, payloadOf(this.read(act, op.source)), op.results);
        this.invalidate(act, op.source);
        return;

      case "unary":
        this.define(
          act,
          op.dst,
          applyUnary(op.unary_op, this.read(act, op.value), op.ty),
        );
        return;
      case "binary":
        this.define(
          act,
          op.dst,
          applyBinary(
            op.binary_op,
            this.read(act, op.lhs),
            this.read(act, op.rhs),
            op.ty,
          ),
        );
        return;
      case "cast":
        this.define(act, op.dst, applyCast(this.read(act, op.value), op.to));
        return;
      case "str.eq":
      case "bytes.eq":
        this.define(act, op.dst, {
          kind: "bool",
          value:
            canonical(this.read(act, op.lhs)) ===
            canonical(this.read(act, op.rhs)),
        });
        return;

      case "function.make":
        this.define(act, op.dst, { kind: "function", id: String(op.callable) });
        return;
      case "closure.make": {
        // The captures become one environment, in the order the closure shape
        // declares; a `capture` place in the body reaches its field.
        const shape = this.closureAt(op.closure);
        this.define(act, op.dst, {
          kind: "closure",
          body: shape.body,
          environment: {
            kind: "record",
            typeId: "",
            fields: op.fields.map((id) => this.read(act, id)),
          },
        });
        return;
      }
      case "callable.coerce":
        // A coercion only weakens the callable's proved capabilities; SIR
        // refuses one that changes an argument, a result or a capture shape,
        // so the value itself is unchanged.
        this.define(act, op.dst, this.read(act, op.source));
        return;
      case "dyn.make":
        this.define(act, op.dst, {
          kind: "dyn",
          vtable: op.vtable,
          value: this.read(act, op.value),
        });
        return;

      // The registration reserves the free places the body reaches; the drain
      // order is already the CFG's, so there is nothing to record here.
      case "register_defer":
        return;

      case "stream.pipe": {
        const pair = this.newPipe(op.capacity);
        this.define(
          act,
          op.results[0]!.value,
          this.pipes.extract(pair, "stream"),
        );
        this.define(
          act,
          op.results[1]!.value,
          this.pipes.extract(pair, "sink"),
        );
        this.pipes.freePair(pair);
        return;
      }
      case "task_scope.enter": {
        const group: TaskGroup = {
          tasks: [],
          changed: [],
          deadline: null,
          joining: false,
        };
        act.scopes.set(op.scope, group);
        if (op.duration !== null) {
          const duration = this.read(act, op.duration);
          if (duration.kind !== "i64")
            throw new Error("scope duration is not a duration");
          group.cancelTimer = this.scheduler.after(duration.value, () => {
            group.deadline = {
              kind: "panic",
              message: "scope deadline elapsed",
              deadline: true,
            };
            for (const task of group.tasks) if (!task.done) task.cancel();
            if (!group.joining) act.context.cancel?.(group.deadline);
            for (const changed of [...group.changed]) changed();
          });
        }
        return;
      }
      case "task_scope.close": {
        const group = act.scopes.get(op.scope);
        group?.cancelTimer?.();
        this.releaseOperation(
          act,
          {
            kind: "vector",
            elementType: "task",
            items: (group?.tasks ?? []).map((task) => ({
              kind: "task",
              id: task.id,
            })),
          },
          () => {
            for (const task of group?.tasks ?? []) this.tasks.delete(task.id);
            act.scopes.delete(op.scope);
          },
        );
        return;
      }
      case "generator.make": {
        const callable = this.read(act, op.callable);
        this.invalidate(act, op.callable);
        const id = `generator:${this.generators.size}`;
        const context: FrameContext = {
          id,
          actor: act.context.actor,
          returned: (value) =>
            this.closeValueAsync(value, null, (fault) =>
              this.finishGenerator(generator, fault),
            ),
          failed: (fault) => this.finishGenerator(generator, fault),
        };
        const generator: GeneratorEntry = {
          id,
          callable,
          started: false,
          done: false,
          fault: null,
          closed: [],
          frame: this.activate(
            this.functionAt(calleeFunction(callable)),
            callable.kind === "closure" ? [callable.environment] : [],
            null,
            null,
            undefined,
            null,
            context,
          ),
        };
        context.generator = generator;
        this.generators.set(id, generator);
        this.define(act, op.dst, { kind: "generator", id });
        return;
      }
      case "task.spawn": {
        const group = act.scopes.get(op.scope);
        if (!group) throw new Error(`task scope ${op.scope} is missing`);
        const callable = this.read(act, op.callable);
        this.invalidate(act, op.callable);
        const task = this.newTask();
        group.tasks.push(task);
        const settle = (value: VmValue, fault: Fault | null) => {
          if (task.done || task.finishing) return;
          task.finishing = true;
          if (
            fault &&
            !(fault.kind === "panic" && (fault.cancelled || fault.deadline)) &&
            act.context.actor
          )
            act.context.actor.crashing ??= fault;
          this.closeValueAsync(
            callable,
            act.context.actor
              ? this.faultText(act.context.actor.crashing)
              : null,
            (closeFault) => {
              if (!fault || (fault.kind === "panic" && fault.cancelled))
                fault = closeFault ?? fault;
              this.settleTask(task, value, fault);
              if (fault && !(fault.kind === "panic" && fault.cancelled)) {
                for (const sibling of group.tasks)
                  if (!sibling.done) sibling.cancel();
                if (!group.joining) act.context.cancel?.(fault);
              }
              for (const changed of [...group.changed]) changed();
            },
          );
        };
        const context: FrameContext = {
          id: task.id,
          actor: act.context.actor,
          returned: (value) => settle(value, null),
          failed: (fault) => settle(UNIT, fault),
        };
        const frame = this.activate(
          this.functionAt(calleeFunction(callable)),
          callable.kind === "closure" ? [callable.environment] : [],
          null,
          null,
          undefined,
          null,
          context,
        );
        let started = false;
        task.cancel = () => {
          if (task.done || task.finishing) return;
          if (started) context.cancel?.();
          else
            settle(UNIT, {
              kind: "panic",
              message: "task cancelled",
              cancelled: true,
            });
        };
        this.scheduler.enqueue(task.id, () => {
          if (task.done || task.finishing) return;
          started = true;
          this.runFrame(frame);
        });
        this.trace.allocateId("task", task.id, act.context.id);
        this.define(act, op.dst, { kind: "task", id: task.id });
        return;
      }
      default:
        throw new Error(`opcode ${op.op} has no sequential executor`);
    }
  }

  /// Hand each field of a consumed container, or each payload of a matched
  /// case, to the values the package names.
  private spread(
    act: Activation,
    fields: VmValue[],
    defs: Array<ValueDef | number>,
  ): void {
    for (const [at, def] of defs.entries()) {
      if (typeof def !== "number" && def.own === "none")
        act.trivial.add(def.value);
      this.define(
        act,
        typeof def === "number" ? def : def.value,
        fields[at] ?? UNIT,
      );
    }
  }

  private aggregateName(shape: number): string {
    return this.pkg.aggregates[shape]?.name ?? "";
  }

  private variantName(shape: number): string {
    return this.pkg.variants[shape]?.name ?? "";
  }

  private closureAt(id: number): ClosureShape {
    const shape =
      this.pkg.closures[id] ??
      this.pkg.closures.find((candidate) => candidate.id === id);
    if (!shape) {
      throw new Error(`closure ${id} is not in the package`);
    }
    return shape;
  }

  // ── terminators ──────────────────────────────────────────────────────────

  private terminate(act: Activation, term: TermV1): void {
    switch (term.op) {
      case "goto":
        this.takeEdge(act, term.edge);
        return;
      case "branch":
        this.takeEdge(
          act,
          truth(this.read(act, term.condition)) ? term.then : term.else,
        );
        return;
      case "switch.variant": {
        const scrutinee = this.read(act, term.scrutinee);
        const tag = tagOf(scrutinee);
        // The arms cover every tag; there is no default.
        const arm = term.arms.find((candidate) => candidate.variant === tag);
        if (!arm) {
          throw new Error(`switch.variant has no arm for tag ${tag}`);
        }
        this.spread(act, payloadOf(scrutinee), arm.fields);
        this.takeEdge(act, arm.edge);
        return;
      }
      case "return":
        this.returnFrom(
          act,
          term.value ? this.boundary(act, term.value) : UNIT,
        );
        return;
      case "checked.binary": {
        const failure = checkedFailure(
          term.binary_op,
          this.read(act, term.lhs),
          this.read(act, term.rhs),
          term.ty,
        );
        if (failure) {
          const edge = term.failures.find(
            (candidate) => candidate.trap === failure,
          );
          if (!edge) {
            throw new Error(`checked.binary names no ${failure} edge`);
          }
          this.takeEdge(act, edge.edge);
          return;
        }
        this.define(
          act,
          term.result.value,
          applyBinary(
            term.binary_op,
            this.read(act, term.lhs),
            this.read(act, term.rhs),
            term.ty,
          ),
        );
        this.takeEdge(act, term.normal);
        return;
      }

      case "dyn.call": {
        const receiver = this.boundary(act, term.receiver);
        if (receiver.kind !== "dyn") {
          throw new TypeError(
            `dyn.call expected a trait object, got ${receiver.kind}`,
          );
        }
        const vtable =
          this.pkg.vtables[receiver.vtable] ??
          this.pkg.vtables.find(
            (candidate) => candidate.id === receiver.vtable,
          );
        // The slot number is the checker's own index, so it is matched rather
        // than used as a position in the slot array.
        const entry = vtable?.slots.find(
          (candidate) => candidate.slot === term.slot,
        );
        if (!entry) {
          throw new Error(`vtable ${receiver.vtable} has no slot ${term.slot}`);
        }
        // The wrapped value is the method's receiver, ahead of the call's own
        // arguments, passed under the decision the slot records: the erasure
        // the concrete type was made under, not something read off the method.
        const args = [
          this.erasedReceiver(
            act,
            term.receiver,
            receiver.value,
            entry.receiver,
          ),
          ...term.args.map((operand) => this.boundary(act, operand)),
        ];
        this.current = this.activate(
          this.functionAt(entry.callee),
          args,
          act,
          term.result,
          term.normal,
          term.unwind,
        );
        return;
      }
      case "call": {
        const args = term.args.map((operand) => this.boundary(act, operand));
        this.current = this.activate(
          this.functionAt(term.callee),
          args,
          act,
          term.result,
          term.normal,
          term.unwind,
        );
        return;
      }
      case "indirect.call": {
        // A closure's environment reaches its body's first parameter, ahead of
        // the call's own arguments, the way a trait object's value reaches
        // `self`. A plain function value carries none.
        const callee = this.boundary(act, term.callee);
        const args = term.args.map((operand) => this.boundary(act, operand));
        this.current = this.activate(
          this.functionAt(calleeFunction(callee)),
          callee.kind === "closure" ? [callee.environment, ...args] : args,
          act,
          term.result,
          term.normal,
          term.unwind,
        );
        return;
      }
      case "actor.call":
        this.actorCall(act, term);
        return;
      case "runtime.call": {
        const entry = this.pkg.runtime_families[term.family];
        if (
          entry &&
          (entry.family === "Map" ||
            entry.family === "Set" ||
            (entry.family === "Array" && entry.detail === "Set") ||
            (entry.family === "Vector" &&
              ["Contains", "Clear", "Set"].includes(String(entry.detail))))
        ) {
          const args = term.args.map((operand) => this.boundary(act, operand));
          const iterator = collectionOperation(
            this.pkg,
            entry.family === "Array" ? "Vector" : entry.family,
            String(entry.detail),
            args,
            term.callbacks ?? [],
            term.result_member_shapes?.[1] ?? term.result_shape,
          );
          this.runValueProgram(act, term, args, iterator);
          return;
        }
        if (
          entry &&
          [
            "RcNew",
            "RcClone",
            "RcGet",
            "RcIsUnique",
            "RcSet",
            "RcStrongCount",
            "RcWeakCount",
          ].includes(entry.family)
        ) {
          const args = term.args.map((operand) => this.boundary(act, operand));
          this.runValueProgram(
            act,
            term,
            args,
            sharedOperation(entry.family, args),
          );
          return;
        }
        if (entry?.family === "SupervisorPool") {
          this.supervisorPool(act, term, String(entry.detail));
          return;
        }
        if (entry && ["SinkClose", "StreamClose"].includes(entry.family)) {
          const value = this.boundary(act, term.args[0]!);
          this.running = false;
          this.closeValueAsync(value, this.pipeFault(act), (fault) => {
            if (fault) this.raiseFault(act, fault, term.unwind);
            else this.completeShim(act, term, UNIT);
            this.scheduler.enqueue(act.context.id, () => this.runFrame(act));
          });
          return;
        }
        if (entry?.family === "StructuralFormat") {
          this.structuralFormat(act, term);
          return;
        }
        const shim = entry ? resolveRuntimeShim(entry) : undefined;
        if (!entry || !shim) {
          throw new Error(
            `runtime family ${term.family} resolved no shim after admission`,
          );
        }
        this.callShim(act, term, shim);
        return;
      }
      case "extern.call": {
        const entry = this.pkg.externs[term.extern];
        const shim = entry ? resolveExternShim(entry.symbol) : undefined;
        if (!entry || !shim) {
          throw new Error(
            `extern ${term.extern} resolved no shim after admission`,
          );
        }
        this.callShim(act, term, shim);
        return;
      }
      case "value.call": {
        const args = term.args.map((operand) => this.boundary(act, operand));
        this.runValueProgram(
          act,
          term,
          args,
          selectedValue(this.pkg, term.plan, args),
        );
        return;
      }

      case "panic": {
        const message = this.boundary(act, term.message);
        act.fault = { kind: "panic", message: renderMessage(message) };
        if (act.context.actor) act.context.actor.crashing ??= act.fault;
        this.takeEdge(act, term.cleanup);
        return;
      }
      case "checked_raise":
        act.fault = { kind: "trap", trap: term.trap };
        if (act.context.actor) act.context.actor.crashing ??= act.fault;
        this.takeEdge(act, term.cleanup);
        return;
      case "trap":
        this.haltWithFault({ kind: "trap", trap: term.trap });
        return;
      case "cleanup.dispatch":
        this.takeEdge(act, act.fault ? term.fault : term.normal);
        return;
      case "enter_defer":
        // The deferred body runs on its own fault state: a body reached while
        // unwinding must not read the fault it is cleaning up after.
        act.parked.push(act.fault);
        act.fault = null;
        this.takeEdge(act, term.body);
        return;
      case "finish_defer": {
        // Combine the parked fault with whatever the body left, keeping the
        // first: a join must never forget the fault the boundary carried in.
        const parked = act.parked.pop();
        if (parked === undefined) {
          throw new Error(`${act.fn.name}: finish_defer has no active body`);
        }
        act.fault =
          parked?.kind === "panic" && parked.cancelled
            ? (act.fault ?? parked)
            : (parked ?? act.fault);
        this.takeEdge(act, term.next);
        return;
      }
      case "recover_fault": {
        const fault = act.fault;
        if (!fault) throw new Error("recover_fault has no pending fault");
        if (fault.kind === "panic" && fault.cancelled) {
          this.raiseFault(act, fault, term.unwind);
          return;
        }
        const shape =
          term.result_shape == null
            ? undefined
            : this.pkg.variants[term.result_shape];
        const deadline = fault.kind === "panic" && fault.deadline;
        const value: VmValue = {
          kind: "enum",
          typeId: shape?.name ?? "",
          tag: deadline ? term.deadline_variant : term.fault_variant,
          payload: [
            {
              kind: "string",
              value:
                fault.kind === "panic"
                  ? fault.message
                  : (fault.message ?? trapMessage(fault.trap)),
            },
          ],
        };
        act.fault = null;
        if (act.context.actor) act.context.actor.crashing = null;
        this.completeShim(act, term, value);
        return;
      }
      case "resume_unwind":
        this.resumeUnwind(act);
        return;

      case "suspend":
        this.suspend(act, term);
        return;

      case "unreachable":
        throw new Error(`${act.fn.name}: reached an unreachable endpoint`);
      default:
        throw new Error(`terminator ${term.op} has no sequential executor`);
    }
  }

  private takeEdge(act: Activation, edge: Edge): void {
    // Read every argument before rebinding, so a block that branches to itself
    // with permuted parameters binds the incoming values, not the rebound ones.
    const args = edge.args.map((id) => this.refOf(act, id));
    const target = this.blockAt(act.fn, edge.to);
    for (const [at, param] of target.params.entries()) {
      act.env.set(param.value, args[at] ?? ownedRef(UNIT));
    }
    act.block = target;
    act.opIndex = 0;
  }

  /// Run a runtime or extern shim. A fault it raises takes the call's `unwind`
  /// edge so the cleanup the package names runs before the program ends.
  private callShim(
    act: Activation,
    term: Extract<TermV1, { op: "runtime.call" | "extern.call" }>,
    shim: (
      host: ShimHost,
      args: VmValue[],
      shape: VariantShape | null,
    ) => VmValue,
  ): void {
    const args = term.args.map((operand) => this.boundary(act, operand));
    const shape =
      term.result_shape === null
        ? null
        : (this.pkg.variants[term.result_shape] ?? null);
    let value: VmValue;
    try {
      value = shim(this.host, args, shape);
    } catch (error) {
      if (error instanceof ShimFault) {
        this.raiseFault(
          act,
          { kind: "trap", trap: error.trap, message: error.message },
          term.unwind,
        );
        return;
      }
      throw error;
    }
    this.completeShim(act, term, value);
  }

  private runValueProgram(
    act: Activation,
    term: Extract<TermV1, { op: "runtime.call" | "value.call" }>,
    args: VmValue[],
    iterator: ValueProgram,
  ): void {
    this.running = false;
    let initial = true;
    const resume = () => {
      act.context.cancel = undefined;
      if (initial) this.running = true;
      else this.scheduler.enqueue(act.context.id, () => this.runFrame(act));
    };
    const fail = (fault: Fault, extra: VmValue = UNIT) => {
      const owned = args.filter((_arg, index) =>
        ["move", "copy", "snapshot"].includes(term.args[index]!.decision),
      );
      this.closeValueAsync(
        { kind: "vector", elementType: "", items: [...owned, extra] },
        this.faultText(fault),
        (closeFault) => {
          if (fault.kind === "panic" && fault.cancelled && closeFault)
            fault = closeFault;
          this.raiseFault(act, fault, term.unwind);
          resume();
        },
      );
    };
    const next = (value: VmValue = UNIT) => {
      let step: IteratorResult<
        import("./value-operations.js").ValueRequest,
        VmValue
      >;
      try {
        step = iterator.next(value);
      } catch (error) {
        if (error instanceof ShimFault) {
          fail({ kind: "trap", trap: error.trap, message: error.message });
          return;
        }
        throw error;
      }
      if (step.done) {
        this.completeShim(act, term, step.value);
        resume();
      } else if (step.value.kind === "release") {
        const request = step.value;
        this.closeValueAsync(request.value, this.pipeFault(act), (fault) => {
          if (fault) {
            if (term.op !== "runtime.call" || !term.releases_contents) {
              fail(fault, request.onFault);
              return;
            }
            // The shared contract republishes the receiver and result owners
            // before SIR dispatches a failure from releasing their contents.
            act.fault ??= fault;
          }
          next();
        });
      } else {
        const request = step.value;
        const child = this.invokeFrame(
          act.context.actor,
          request.callee,
          request.args,
          next,
          fail,
        );
        act.context.cancel = (fault) => child.cancel?.(fault);
      }
    };
    next();
    initial = false;
  }

  private supervisorPool(
    act: Activation,
    term: Extract<TermV1, { op: "runtime.call" }>,
    operation: string,
  ): void {
    const [view, index] = term.args.map((arg) => this.boundary(act, arg));
    if (view?.kind !== "vector" || index?.kind !== "i64")
      throw new Error("pool operation has no view and index");
    const value = view.items[Number(index.value)];
    if (operation === "Get") {
      this.completeShim(
        act,
        term,
        this.variant(
          term.result_shape,
          value ? "Some" : "None",
          value ? [value] : [],
        ),
      );
      return;
    }
    if (!value) {
      this.raiseFault(
        act,
        { kind: "trap", trap: "vector_bounds" },
        term.unwind,
      );
      return;
    }
    const role = this.roles.get((value as { id: string }).id)!;
    if (operation === "AwaitRestartMember" && this.rolePending(role)) {
      this.running = false;
      const settled = () => {
        if (this.rolePending(role)) {
          role.waiting.push(settled);
          return;
        }
        this.completeShim(act, term, value);
        this.scheduler.enqueue(act.context.id, () => this.runFrame(act));
      };
      role.waiting.push(settled);
    } else this.completeShim(act, term, value);
  }

  private structuralFormat(
    act: Activation,
    term: Extract<TermV1, { op: "runtime.call" }>,
  ): void {
    if (term.structural == null)
      throw new Error("StructuralFormat has no checked recipe");
    const renderer = new StructuralRenderer(
      this.pkg.structural_render ?? [],
      term.structural,
      this.boundary(act, term.args[0]!),
    );
    this.running = false;
    const continueRendering = () => {
      const next = renderer.next();
      if (typeof next === "string") {
        this.completeShim(act, term, { kind: "string", value: next });
        this.scheduler.enqueue(act.context.id, () => this.runFrame(act));
        return;
      }
      const callback = this.invokeFrame(
        act.context.actor,
        next.callee,
        [next.value],
        (value) => {
          renderer.append(value);
          continueRendering();
        },
        (fault) => {
          this.raiseFault(act, fault, term.unwind);
          this.scheduler.enqueue(act.context.id, () => this.runFrame(act));
        },
      );
      act.context.cancel = (fault) => callback.cancel?.(fault);
    };
    continueRendering();
  }

  /// A shim returned: bind the call's result on its normal edge and continue.
  private completeShim(
    act: Activation,
    term: TermV1 & { result: CallResult; normal?: Edge },
    value: VmValue,
  ): void {
    if (!term.normal) {
      throw new Error(
        `${term.op} with a "never" result cannot return in the sandbox`,
      );
    }
    if (term.result && term.result !== "never") {
      this.define(act, term.result.value, value);
    }
    this.takeEdge(act, term.normal);
  }

  private returnFrom(act: Activation, value: VmValue): void {
    const caller = act.caller;
    if (!caller) {
      this.running = false;
      act.context.returned(value);
      return;
    }
    if (!act.normal) {
      throw new Error(
        `${act.fn.name}: returned from a call with a "never" result`,
      );
    }
    // The result is defined on the normal edge, like a block parameter, so it
    // is in scope for that edge's arguments.
    if (act.result && act.result !== "never") {
      this.define(caller, act.result.value, value);
    }
    this.current = caller;
    this.takeEdge(caller, act.normal);
  }

  /// The wrapped value on its way to a trait method's `self`. A `copy` receiver
  /// deep-clones, a `move` invalidates the trait object it came out of, and a
  /// borrow passes the live value.
  private erasedReceiver(
    act: Activation,
    operand: BoundaryOperand,
    wrapped: VmValue,
    decision: BoundaryDecision,
  ): VmValue {
    switch (decision) {
      case "copy":
      case "snapshot":
        return cloneValue(wrapped);
      case "move":
        this.invalidate(act, operand.value);
        return wrapped;
      default:
        return wrapped;
    }
  }

  /// Raise a fault at a call site: enter the named unwind edge, or end the
  /// program when the call names none.
  private raiseFault(act: Activation, fault: Fault, unwind: Edge | null): void {
    if (!unwind) {
      this.running = false;
      act.context.failed(fault);
      return;
    }
    act.fault = fault;
    this.takeEdge(act, unwind);
  }

  private resumeUnwind(act: Activation): void {
    const fault = act.fault;
    if (!fault) {
      throw new Error(`${act.fn.name}: resume_unwind with no pending fault`);
    }
    const caller = act.caller;
    if (!caller) {
      this.running = false;
      act.context.failed(fault);
      return;
    }
    if (!act.unwind) {
      throw new Error(
        `${act.fn.name}: unwound into a call that names no unwind edge`,
      );
    }
    caller.fault = fault;
    this.current = caller;
    this.takeEdge(caller, act.unwind);
  }

  private suspend(
    act: Activation,
    term: Extract<TermV1, { op: "suspend" }>,
  ): void {
    switch (term.kind) {
      case "Yield": {
        const generator = act.context.generator;
        if (!generator || !generator.receive)
          throw new Error("yield has no generator consumer");
        const value = this.boundary(act, term.inputs[0]!);
        const wake = this.park(act, term);
        generator.resume = () => wake(UNIT);
        const receive = generator.receive;
        generator.receive = undefined;
        receive(value, null);
        return;
      }
      case "GeneratorNext": {
        const generator = this.generatorFor(
          this.boundary(act, term.inputs[0]!),
        );
        const wake = this.park(act, term);
        const receive = (value: VmValue | null, fault: Fault | null) => {
          if (fault) wake(UNIT, fault);
          else
            wake(
              this.variant(
                term.result_shape,
                value === null ? "None" : "Some",
                value === null ? [] : [value],
              ),
            );
        };
        if (generator.done) {
          receive(null, generator.fault);
          return;
        }
        if (generator.receive)
          throw new Error("generator already has a pending next");
        generator.receive = receive;
        const cancel = act.context.cancel;
        act.context.cancel = (fault) => {
          generator.receive = undefined;
          this.closeGenerator(generator, (closeFault) =>
            cancel?.(closeFault ?? fault),
          );
        };
        if (generator.started) {
          const resume = generator.resume;
          generator.resume = undefined;
          if (!resume) throw new Error("generator has no suspended yield");
          resume();
        } else {
          generator.started = true;
          this.scheduler.enqueue(generator.id, () =>
            this.runFrame(generator.frame),
          );
        }
        return;
      }
      case "ValueClose": {
        const place = valueCloseePlace(term.detail);
        let value: VmValue = UNIT;
        if (place !== null) {
          const ref = this.placeRef(act, place);
          value = liveRef(ref) ? readRef(ref) : UNIT;
          if (liveRef(ref)) invalidateRef(ref);
        } else if (term.inputs[0]) {
          value = this.boundary(act, term.inputs[0]);
        }
        if (term.detail.selection === "vector_element") {
          const index = this.boundary(act, term.inputs[1]!);
          if (value.kind !== "vector" || index.kind !== "i64")
            throw new Error("element close has no vector index");
          value = value.items[Number(index.value)]!;
        }
        const wake = this.park(act, term);
        this.closeValueAsync(value, this.pipeFault(act), (fault) =>
          wake(UNIT, fault),
        );
        return;
      }
      case "StreamSend": {
        const [sink, value] = term.inputs.map((input) =>
          this.boundary(act, input),
        );
        let cancelWait = () => {};
        let owned = true;
        let closing = false;
        let cancelled: Fault | null = null;
        const wake = this.park(act, term, () => cancelWait());
        const cancel = act.context.cancel!;
        act.context.cancel = (
          fault = { kind: "panic", message: "task cancelled", cancelled: true },
        ) => {
          cancelled ??= fault;
          cancelWait();
          if (owned) {
            owned = false;
            closing = true;
            this.closeValueAsync(value!, this.faultText(fault), (closeFault) =>
              cancel(closeFault ?? fault),
            );
          } else if (!closing) cancel(fault);
        };
        cancelWait = this.pipes.send(
          sink!,
          value!,
          term.detail.park,
          (status) => {
            owned = false;
            if (status === 0) wake(UNIT);
            else {
              closing = true;
              this.closeValueAsync(value!, this.pipeFault(act), (fault) =>
                wake(UNIT, fault ?? cancelled, status),
              );
            }
          },
        );
        return;
      }
      case "StreamNext": {
        const stream = this.boundary(act, term.inputs[0]!);
        let cancelWait = () => {};
        const wake = this.park(act, term, () => cancelWait());
        cancelWait = this.pipes.receive(
          stream,
          term.detail.park,
          (value, fault) => {
            if (fault !== null) wake(UNIT, { kind: "panic", message: fault });
            else
              wake(
                this.variant(
                  term.result_shape,
                  value === null ? "None" : "Some",
                  value === null ? [] : [value],
                ),
              );
          },
        );
        return;
      }
      case "Await": {
        const task = this.taskFor(this.boundary(act, term.inputs[0]!));
        const wake = this.park(act, term);
        const ready = () => {
          if (!task.done) return;
          if (task.fault) wake(UNIT, task.fault);
          else {
            const value = task.value;
            task.value = UNIT;
            wake(value);
          }
        };
        task.changed.push(ready);
        ready();
        return;
      }
      case "Join": {
        const group = act.scopes.get(term.detail.scope);
        if (!group) throw new Error("join has no task scope");
        group.joining = true;
        const wake = this.park(act, term);
        const mode = term.detail.mode;
        if (mode !== "wait")
          for (const task of group.tasks) if (!task.done) task.cancel();
        const ready = () => {
          if (group.tasks.some((task) => !task.done)) return;
          const failure =
            group.deadline ??
            group.tasks.find(
              (task) =>
                task.fault &&
                !(task.fault.kind === "panic" && task.fault.cancelled),
            )?.fault;
          wake(
            UNIT,
            mode === "propagate_fault" || mode === "cancel_losers_after_fault"
              ? null
              : (failure ?? null),
          );
        };
        group.changed.push(ready);
        ready();
        return;
      }
      case "Select": {
        const values = term.inputs.map((input) => this.boundary(act, input));
        const hasTimeout = term.detail.has_timeout;
        const sources = hasTimeout ? values.slice(0, -1) : values;
        const unwatch: Array<() => void> = [];
        const wake = this.park(act, term, () =>
          unwatch.forEach((remove) => remove()),
        );
        let cancelTimer: (() => void) | undefined;
        const choose = () => {
          const ready = sources.flatMap((value, index) => {
            if (value.kind === "stream")
              return this.pipes.readable(value) ? [{ index, order: 0 }] : [];
            const task = this.taskFor(value);
            return task.done ? [{ index, order: task.completedAt }] : [];
          });
          if (term.detail.order === "completion")
            ready.sort((a, b) => a.order - b.order);
          if (ready[0]) {
            cancelTimer?.();
            wake({ kind: "i64", value: BigInt(ready[0].index) });
          }
        };
        if (hasTimeout) {
          const duration = values[values.length - 1];
          if (duration?.kind !== "i64")
            throw new Error("selection timeout is not a duration");
          cancelTimer = this.scheduler.after(duration.value, () =>
            wake({ kind: "i64", value: BigInt(sources.length) }),
          );
        }
        sources.forEach((value) => {
          if (value.kind === "stream")
            unwatch.push(this.pipes.observe(value, choose));
          else {
            const task = this.taskFor(value);
            task.changed.push(choose);
            unwatch.push(() => {
              const index = task.changed.indexOf(choose);
              if (index >= 0) task.changed.splice(index, 1);
            });
          }
        });
        choose();
        return;
      }
      case "Ask": {
        const protocol = term.detail as ActorProtocol;
        const inputs = term.inputs.map((input) => this.boundary(act, input));
        const wake = this.park(act, term);
        this.ask(
          protocol,
          inputs,
          (value, error) => {
            wake(
              this.completion(
                term.result_shape,
                term.error_shape,
                value,
                error,
              ),
            );
          },
          term.request_shapes,
        );
        return;
      }
      case "Sleep": {
        const duration = term.inputs[0]
          ? this.boundary(act, term.inputs[0])
          : UNIT;
        const nanos = duration.kind === "i64" ? duration.value : 0n;
        let cancelTimer = () => {};
        const wake = this.park(act, term, () => cancelTimer());
        cancelTimer = this.scheduler.after(nanos, () => wake(UNIT));
        return;
      }
      default:
        throw new Error(
          `suspension kind ${term.kind} resolved no handler after admission`,
        );
    }
  }

  private park(
    act: Activation,
    term: Extract<TermV1, { op: "suspend" }>,
    cleanup: () => void = () => {},
  ): (value: VmValue, fault?: Fault | null, edgeIndex?: number) => void {
    this.running = false;
    let pending: { value: VmValue; fault: Fault | null; edge: number } | null =
      null;
    const resume = (value: VmValue, fault: Fault | null = null, edge = 0) => {
      if (pending) return;
      pending = { value, fault, edge };
      cleanup();
      this.scheduler.enqueue(act.context.id, () => {
        const outcome = pending!;
        act.context.cancel = undefined;
        const finish = (closeFault: Fault | null = null) => {
          const fault = closeFault ?? outcome.fault;
          if (fault) {
            act.fault = fault;
            this.takeEdge(
              act,
              fault.kind === "panic" && fault.cancelled
                ? term.cancel
                : term.unwind!,
            );
          } else {
            if (term.result && term.result !== "never")
              this.define(act, term.result.value, outcome.value);
            const edge = term.resumes[outcome.edge];
            if (!edge) throw new Error(`${term.kind} has no resume edge`);
            this.takeEdge(act, edge);
          }
          this.runFrame(act);
        };
        // A close can arrive after readiness but before this frame runs. Keep
        // the cancellation latch live through that window and retire a result
        // whose normal edge the consumer never entered.
        if (outcome.fault)
          this.closeValueAsync(
            outcome.value,
            this.faultText(outcome.fault),
            finish,
          );
        else finish();
      });
    };
    act.context.cancel = (
      fault = { kind: "panic", message: "task cancelled", cancelled: true },
    ) => {
      if (pending) pending.fault ??= fault;
      else resume(UNIT, fault);
    };
    return resume;
  }

  private newPipe(capacity: number): VmValue {
    const id = this.ids.channel();
    this.trace.allocateId(
      "channel",
      id,
      this.current?.context.id ?? "actor:root",
    );
    return this.pipes.create(id, capacity);
  }

  private faultText(fault: Fault | null): string | null {
    return !fault ||
      (fault.kind === "panic" && (fault.cancelled || fault.deadline))
      ? null
      : fault.kind === "panic"
        ? fault.message
        : (fault.message ?? trapMessage(fault.trap));
  }

  private pipeFault(act: Activation): string | null {
    return this.faultText(act.context.actor?.crashing ?? act.fault);
  }

  private releaseOperation(
    act: Activation,
    value: VmValue,
    finish?: () => void,
  ): void {
    let synchronous = true;
    let completed = false;
    this.closeValueAsync(value, this.pipeFault(act), (fault) => {
      completed = true;
      finish?.();
      if (
        fault &&
        (!act.fault || (act.fault.kind === "panic" && act.fault.cancelled))
      )
        act.fault = fault;
      if (!synchronous)
        this.scheduler.enqueue(act.context.id, () => this.runFrame(act));
    });
    synchronous = false;
    if (!completed) this.running = false;
  }

  private generatorFor(value: VmValue): GeneratorEntry {
    if (value.kind !== "generator")
      throw new Error("generator operand has no generator identity");
    const generator = this.generators.get(value.id);
    if (!generator) throw new Error("generator does not exist");
    return generator;
  }

  private finishGenerator(
    generator: GeneratorEntry,
    fault: Fault | null,
  ): void {
    if (generator.done) return;
    generator.done = true;
    generator.fault = fault?.kind === "panic" && fault.cancelled ? null : fault;
    this.closeValueAsync(
      generator.callable,
      this.faultText(fault),
      (closeFault) => {
        generator.fault ??= closeFault;
        const receive = generator.receive;
        generator.receive = undefined;
        receive?.(null, generator.fault);
        for (const closed of generator.closed.splice(0))
          closed(generator.fault);
      },
    );
  }

  private closeGenerator(
    generator: GeneratorEntry,
    done: (fault: Fault | null) => void,
  ): void {
    if (generator.done) {
      done(generator.fault);
      return;
    }
    generator.closed.push(done);
    if (!generator.started) this.finishGenerator(generator, null);
    else generator.frame.context.cancel?.();
  }

  private closeValueAsync(
    value: VmValue,
    fault: string | null,
    done: (fault: Fault | null) => void,
    actor: ActorInstance | null = this.current.context.actor,
  ): void {
    const pending = [value];
    let firstFault: Fault | null = null;
    const next = (failure: Fault | null = null) => {
      firstFault ??= failure;
      while (pending.length) {
        const value = pending.pop()!;
        switch (value.kind) {
          case "rc":
            if (!value.closed) {
              value.closed = true;
              if (--value.cell.refs === 0) {
                pending.push(value.cell.value);
                value.cell.value = UNIT;
              }
            }
            break;
          case "task": {
            // Dropping a handle does not cancel scoped work. Join drains the
            // task, and scope close releases any result that was never taken.
            const task = this.tasks.get(value.id);
            if (task?.done) {
              const result = task.value;
              task.value = UNIT;
              this.closeValueAsync(result, fault, next);
              return;
            }
            break;
          }
          case "map":
            for (const entry of [...value.entries.values()].reverse())
              pending.push(entry.value, entry.key);
            break;
          case "generator":
            this.closeGenerator(this.generatorFor(value), next);
            return;
          case "closure":
            pending.push(value.environment);
            break;
          case "record": {
            const resource = (this.pkg.resources ?? []).find(
              (resource) =>
                resource.kind === "record" && resource.ty === value.typeId,
            );
            if (resource?.close !== undefined) {
              if (this.consumedResources.has(value)) break;
              if (!this.closingResources.has(value)) {
                this.invokeFrame(
                  actor,
                  resource.close,
                  [value],
                  () => next(),
                  next,
                );
                return;
              }
              this.consumedResources.add(value);
            }
            pending.push(...value.fields);
            break;
          }
          case "enum":
            pending.push(...[...value.payload].reverse());
            break;
          case "vector":
            pending.push(...[...value.items].reverse());
            break;
          case "sink":
          case "stream":
            pending.push(...this.pipes.close(value, fault).reverse());
            break;
        }
      }
      done(firstFault);
    };
    next();
  }

  private closeValue(value: VmValue, fault: string | null): void {
    if (value.kind === "closure") this.closeValue(value.environment, fault);
    else if (value.kind === "sink" || value.kind === "stream")
      this.pipes.close(value, fault);
    else if (value.kind === "record")
      for (const field of value.fields) this.closeValue(field, fault);
    else if (value.kind === "enum")
      for (const field of value.payload) this.closeValue(field, fault);
    else if (value.kind === "vector")
      for (const item of value.items) this.closeValue(item, fault);
  }

  private newTask(): TaskEntry {
    const task: TaskEntry = {
      id: this.ids.task(),
      done: false,
      finishing: false,
      value: UNIT,
      fault: null,
      actorError: null,
      cancel: () => {},
      completedAt: 0,
      changed: [],
    };
    this.tasks.set(task.id, task);
    return task;
  }

  private taskFor(value: VmValue): TaskEntry {
    if (value.kind !== "task")
      throw new Error(`expected a task, got ${value.kind}`);
    const task = this.tasks.get(value.id);
    if (!task) throw new Error("task does not exist");
    return task;
  }

  private settleTask(
    task: TaskEntry,
    value: VmValue,
    fault: Fault | null,
  ): void {
    if (task.done) return;
    task.done = true;
    task.value = value;
    task.fault = fault;
    task.completedAt = ++this.completedTasks;
    for (const changed of [...task.changed]) changed();
  }

  private invokeFrame(
    actor: ActorInstance | null,
    callable: number,
    args: VmValue[],
    returned: (value: VmValue) => void,
    failed: (fault: Fault) => void,
  ): FrameContext {
    const frame = this.activate(
      this.functionAt(callable),
      args,
      null,
      null,
      undefined,
      null,
      {
        id: actor?.id ?? "actor:root",
        actor,
        returned,
        failed,
      },
    );
    let cancelled: Fault | null = null;
    frame.context.cancel = (
      fault = { kind: "panic", message: "task cancelled", cancelled: true },
    ) => {
      cancelled = fault;
    };
    this.scheduler.enqueue(frame.context.id, () => {
      frame.context.cancel = undefined;
      if (cancelled) frame.context.failed(cancelled);
      else this.runFrame(frame);
    });
    return frame.context;
  }

  private variant(
    shapeId: number | null | undefined,
    name: string,
    payload: VmValue[] = [],
  ): VmValue {
    const shape = shapeId == null ? undefined : this.pkg.variants[shapeId];
    if (!shape)
      throw new Error(`runtime result has no variant shape ${shapeId}`);
    const tag = shape.cases.findIndex((entry) => entry.name === name);
    if (tag < 0) throw new Error(`${shape.name} has no ${name} case`);
    return { kind: "enum", typeId: shape.name, tag, payload };
  }

  private completion(
    result: number | null | undefined,
    errorShape: number | null | undefined,
    value: VmValue | null,
    error: string | null,
  ): VmValue {
    return error
      ? this.variant(result, "Err", [
          this.variant(
            errorShape,
            error,
            (error === "Failed" || error === "Rejected") && value
              ? [value]
              : [],
          ),
        ])
      : this.variant(result, "Ok", [value ?? UNIT]);
  }

  private actorFor(value: VmValue): ActorInstance {
    if (value.kind !== "actor")
      throw new Error(`expected an actor handle, got ${value.kind}`);
    const role = this.roles.get(value.id);
    if (role) {
      const child = this.roleValue(role) ?? role.latest;
      if (!child) throw new Error("supervised role is between incarnations");
      return this.actorFor(child);
    }
    const actor = this.actors.get(value.id);
    if (!actor) throw new Error(`actor ${value.id} does not exist`);
    return actor;
  }

  private actorCall(
    act: Activation,
    term: Extract<TermV1, { op: "actor.call" }>,
  ): void {
    const args = term.args.map((arg) => this.boundary(act, arg));
    const operation = term.operation;
    switch (operation.op) {
      case "spawn": {
        const layout = this.pkg.actors?.find(
          (actor) => actor.id === operation.actor,
        );
        if (!layout)
          throw new Error(`actor layout ${operation.actor} is missing`);
        let supplied = 0;
        const actor: ActorInstance = {
          id: this.ids.actor(),
          layout,
          state: {
            kind: "record",
            typeId: "",
            fields: layout.state_fields.map((field) =>
              field.deferred ? UNIT : (args[supplied++] ?? UNIT),
            ),
          },
          mailbox: [],
          admission: [],
          busy: true,
          alive: true,
          completed: false,
          closing: false,
          drainingMailbox: false,
          mailboxDrained: [],
          closingFault: null,
          crashing: null,
          active: null,
          closed: [],
        };
        this.actors.set(actor.id, actor);
        this.trace.allocateId("actor", actor.id, act.context.id);
        this.trace.snapshot("actor.spawn", { actor_id: actor.id });
        const handle: VmValue = { kind: "actor", id: actor.id };
        this.running = false;
        const failed = (fault: Fault) => {
          actor.alive = false;
          this.closeValueAsync(actor.state, this.faultText(fault), () => {
            actor.completed = true;
            for (const message of actor.mailbox.splice(0))
              message.complete(null, "Dead");
            for (const closed of actor.closed.splice(0)) closed();
            this.raiseFault(act, fault, term.unwind);
            this.scheduler.enqueue(act.context.id, () => this.runFrame(act));
          });
        };
        const started = () => {
          actor.busy = false;
          this.dispatchActor(actor);
          this.completeShim(act, term, handle);
          this.scheduler.enqueue(act.context.id, () => this.runFrame(act));
        };
        const start = () => {
          if (layout.start !== undefined)
            this.invokeFrame(
              actor,
              layout.start,
              [actor.state],
              started,
              failed,
            );
          else started();
        };
        if (layout.init !== undefined)
          this.invokeFrame(
            actor,
            layout.init,
            [actor.state, ...args.slice(supplied)],
            start,
            failed,
          );
        else start();
        return;
      }
      case "stream_start": {
        const [target, payload] = args;
        const fields = fieldsOf(payload!);
        this.running = false;
        const admit = () => {
          const role = "id" in target! ? this.roles.get(target.id) : undefined;
          if (role && this.rolePending(role)) {
            role.waiting.push(admit);
            return;
          }
          const actor = this.actorFor(target!);
          if (!actor.alive || actor.closing) {
            this.closeValue(payload!, "stream producer is closed");
            this.raiseFault(
              act,
              { kind: "panic", message: "stream producer is closed" },
              term.unwind,
            );
          } else {
            if (
              actor.layout.mailbox_capacity !== undefined &&
              actor.mailbox.length >= actor.layout.mailbox_capacity
            ) {
              actor.admission.push(admit);
              return;
            }
            const handler = actor.layout.handlers.find(
              (handler) =>
                handler.message_id === operation.message && handler.streams,
            );
            if (!handler)
              throw new Error("stream request has no producer handler");
            actor.mailbox.push({
              handler,
              payload: fields,
              reply: false,
              complete: (_value, error) => {
                if (error) this.closeValue(payload!, error);
              },
            });
            this.dispatchActor(actor);
            this.completeShim(act, term, UNIT);
          }
          this.scheduler.enqueue(act.context.id, () => this.runFrame(act));
        };
        admit();
        return;
      }
      case "call_start": {
        const task = this.newTask();
        this.ask(operation.protocol, args, (value, error) => {
          task.actorError = error;
          this.settleTask(task, value ?? UNIT, null);
        });
        this.completeShim(act, term, { kind: "task", id: task.id });
        return;
      }
      case "call_take": {
        const task = this.taskFor(args[0]!);
        if (!task.done)
          throw new Error("actor completion taken before readiness");
        const value = task.value;
        task.value = UNIT;
        this.completeShim(
          act,
          term,
          this.completion(
            term.result_shape,
            term.error_shape,
            value,
            task.actorError,
          ),
        );
        return;
      }
      case "submit": {
        const request = args[0];
        if (request?.kind !== "record")
          throw new Error("submission has no addressed request");
        const [target, member, payload] = request.fields;
        if (member?.kind !== "i64" || payload?.kind !== "record")
          throw new Error("invalid message description");
        const actor = this.actorFor(target!);
        const handler = actor.layout.handlers.find(
          (handler) => handler.message_id === Number(member.value),
        );
        if (!handler) throw new Error("submission names no actor handler");
        const shapes = term.submission_shapes;
        if (!shapes) throw new Error("submission lacks its result shapes");
        const accepted = (discarded: boolean) =>
          this.variant(term.result_shape, "Ok", [
            this.variant(shapes.success, discarded ? "Discarded" : "Accepted"),
          ]);
        const rejected = (reason: string) =>
          this.variant(term.result_shape, "Err", [
            {
              kind: "record",
              typeId: this.aggregateName(shapes.failure!),
              fields: [this.variant(shapes.reason, reason), request],
            },
          ]);
        let parked = false;
        const finish = (value: VmValue) => {
          this.completeShim(act, term, value);
          if (parked)
            this.scheduler.enqueue(act.context.id, () => this.runFrame(act));
        };
        const submit = () => {
          if (!actor.alive || actor.closing) {
            finish(rejected("Closed"));
            return;
          }
          const full =
            actor.layout.mailbox_capacity !== undefined &&
            actor.mailbox.length >= actor.layout.mailbox_capacity;
          if (full) {
            if (
              actor.layout.overflow === "drop_new" ||
              operation.policy === "drop_newest"
            ) {
              finish(accepted(true));
              return;
            }
            if (
              actor.layout.overflow === "drop_old" ||
              operation.policy === "replace_latest"
            ) {
              actor.mailbox.shift()?.complete(null, "Dead");
            } else if (
              actor.layout.overflow === "fail" ||
              operation.policy === "reject"
            ) {
              finish(rejected("Full"));
              return;
            } else {
              parked = true;
              this.running = false;
              actor.admission.push(submit);
              return;
            }
          }
          actor.mailbox.push({
            handler,
            payload: payload.fields,
            reply: false,
            complete: () => {},
          });
          this.trace.snapshot("actor.send", {
            actor_id: actor.id,
            handler: handler.name,
          });
          this.dispatchActor(actor);
          finish(accepted(false));
        };
        submit();
        return;
      }
      case "supervisor_spawn": {
        const layout = this.pkg.supervisors?.find(
          (layout) => layout.id === operation.supervisor,
        );
        if (!layout) throw new Error("supervisor descriptor is missing");
        const owner: SupervisorInstance = {
          id: this.ids.supervisor(),
          layout,
          config: args,
          children: layout.children.flatMap((child) =>
            Array.from({ length: child.pool_count ?? 1 }, () => null),
          ),
          specs: layout.children.flatMap((child) =>
            Array.from({ length: child.pool_count ?? 1 }, () => child),
          ),
          offsets: layout.children.map((_child, index) =>
            layout.children
              .slice(0, index)
              .reduce((sum, child) => sum + (child.pool_count ?? 1), 0),
          ),
          recovering: new Set(),
          restartBusy: false,
          pendingRestarts: [],
          spent: new Set(),
          completed: false,
          alive: true,
          restartTimes: [],
          closed: [],
        };
        this.supervisors.set(owner.id, owner);
        this.trace.allocateId("supervisor", owner.id, act.context.id);
        this.trace.snapshot("supervisor.spawn", { supervisor_id: owner.id });
        this.running = false;
        let child = 0;
        const next = () => {
          if (child < owner.children.length) {
            this.startChild(owner, child++, next, (fault) => {
              owner.closed.push(() => {
                this.raiseFault(act, fault, term.unwind);
                this.scheduler.enqueue(act.context.id, () =>
                  this.runFrame(act),
                );
              });
              this.stopSupervisor(owner);
            });
          } else {
            this.completeShim(act, term, { kind: "supervisor", id: owner.id });
            this.scheduler.enqueue(act.context.id, () => this.runFrame(act));
          }
        };
        next();
        return;
      }
      case "supervisor_pool_view":
      case "supervisor_await_restart":
      case "supervisor_child": {
        const owner = this.supervisorFor(args[0]!);
        const spec = owner.layout.children[operation.child];
        if (!spec) throw new Error("supervisor child is missing");
        const ownerId = (args[0] as { id: string }).id;
        const makeRole = (child: number): VmValue => {
          const id = `${ownerId}:role:${child}`;
          if (!this.roles.has(id))
            this.roles.set(id, {
              ownerId,
              child,
              latest: owner.children[child] ?? null,
              waiting: [],
            });
          return { kind: "actor" in spec.role ? "actor" : "supervisor", id };
        };
        const offset = owner.offsets[operation.child]!;
        if (operation.op === "supervisor_pool_view") {
          this.completeShim(act, term, {
            kind: "vector",
            elementType: "role",
            items: Array.from({ length: spec.pool_count ?? 1 }, (_, index) =>
              makeRole(offset + index),
            ),
          });
          return;
        }
        const value = makeRole(offset);
        const role = this.roles.get((value as { id: string }).id)!;
        if (
          operation.op === "supervisor_await_restart" &&
          this.rolePending(role)
        ) {
          this.running = false;
          const settled = () => {
            if (this.rolePending(role)) {
              role.waiting.push(settled);
              return;
            }
            this.completeShim(act, term, value);
            this.scheduler.enqueue(act.context.id, () => this.runFrame(act));
          };
          role.waiting.push(settled);
        } else this.completeShim(act, term, value);
        return;
      }
      case "supervisor_stop": {
        const owner = this.supervisorFor(args[0]!);
        this.stopSupervisor(owner);
        this.completeShim(act, term, args[0]!);
        return;
      }
      case "supervisor_role_await_closed":
      case "supervisor_await_closed": {
        const owner = this.supervisorFor(args[0]!);
        if (
          operation.op === "supervisor_role_await_closed" &&
          operation.closing
        )
          this.stopSupervisor(owner);
        if (owner.completed) this.completeShim(act, term, UNIT);
        else {
          this.running = false;
          owner.closed.push(() => {
            this.completeShim(act, term, UNIT);
            this.scheduler.enqueue(act.context.id, () => this.runFrame(act));
          });
        }
        return;
      }
      case "self_handle":
        if (!act.context.actor)
          throw new Error("self handle outside an actor turn");
        this.completeShim(act, term, {
          kind: "actor",
          id: act.context.actor.id,
        });
        return;
      case "close": {
        const actor = this.actorFor(args[0]!);
        this.requestActorClose(actor);
        this.completeShim(act, term, args[0]!);
        return;
      }
      case "await_closed": {
        const actor = this.actorFor(args[0]!);
        if (actor.completed) {
          this.completeShim(act, term, UNIT);
          return;
        }
        this.running = false;
        actor.closed.push(() => {
          this.completeShim(act, term, UNIT);
          this.scheduler.enqueue(act.context.id, () => this.runFrame(act));
        });
        return;
      }
      default:
        throw new Error(`actor operation ${operation.op} has no executor`);
    }
  }

  private ask(
    protocol: ActorProtocol,
    inputs: VmValue[],
    complete: ActorMessage["complete"],
    shapes?: RequestShapes,
  ): void {
    let settled = false;
    let cancelTimer = () => {};
    const finish: ActorMessage["complete"] = (value, error) => {
      if (settled) return;
      settled = true;
      cancelTimer();
      complete(value, error);
    };
    if (protocol.deadline_ns != null)
      cancelTimer = this.scheduler.after(BigInt(protocol.deadline_ns), () =>
        finish(null, "Timeout"),
      );
    const target = inputs[0]!;
    const payload = protocol.sealed ? fieldsOf(inputs[1]!) : inputs.slice(1);
    const attempt = () => {
      if (settled) return;
      const role = "id" in target ? this.roles.get(target.id) : undefined;
      if (role && this.rolePending(role)) {
        role.waiting.push(attempt);
        return;
      }
      const actor = this.actorFor(target);
      if (!actor.alive || actor.closing) {
        finish(null, "Dead");
        return;
      }
      const handler = actor.layout.handlers.find(
        (handler) => handler.message_id === protocol.message,
      );
      if (!handler)
        throw new Error(`actor ${actor.id} has no message ${protocol.message}`);
      const full =
        actor.layout.mailbox_capacity !== undefined &&
        actor.mailbox.length >= actor.layout.mailbox_capacity;
      if (full) {
        if (protocol.policy === "reject") {
          if (
            !shapes ||
            shapes.failure === null ||
            shapes.reason === null ||
            shapes.message === null ||
            shapes.request === null
          )
            throw new Error("rejected call lacks its checked request shapes");
          const record = (shape: number, fields: VmValue[]): VmValue => ({
            kind: "record",
            typeId: this.aggregateName(shape),
            fields,
          });
          const owner: VmValue = {
            kind: "record",
            typeId: "",
            fields: payload,
          };
          const request = record(shapes.request, [owner]);
          const message = record(shapes.message, [
            target,
            { kind: "i64", value: BigInt(protocol.message) },
            request,
          ]);
          finish(
            record(shapes.failure, [
              this.variant(shapes.reason, "Full"),
              message,
            ]),
            "Rejected",
          );
        } else {
          actor.admission.push(attempt);
        }
        return;
      }
      actor.mailbox.push({ handler, payload, complete: finish, reply: true });
      this.trace.snapshot("actor.ask", {
        actor_id: actor.id,
        handler: handler.name,
      });
      this.dispatchActor(actor);
    };
    attempt();
  }

  private dispatchActor(actor: ActorInstance): void {
    if (actor.busy || !actor.alive || actor.drainingMailbox) return;
    if (actor.mailbox.length === 0) {
      if (actor.closing) {
        if (actor.closingFault) this.crashActor(actor, actor.closingFault);
        else this.stopActor(actor);
      }
      return;
    }
    actor.busy = true;
    this.scheduler.enqueue(actor.id, () => {
      const message = actor.mailbox.shift();
      if (!message || !actor.alive) {
        actor.busy = false;
        if (actor.closing && actor.alive) this.dispatchActor(actor);
        return;
      }
      for (const admit of actor.admission.splice(0)) admit();
      this.trace.snapshot("actor.receive", {
        actor_id: actor.id,
        handler: message.handler.name,
      });
      const frame = this.activate(
        this.functionAt(message.handler.callable),
        [actor.state, ...message.payload],
        null,
        null,
        undefined,
        null,
        {
          id: actor.id,
          actor,
          returned: (value) => {
            actor.active = null;
            actor.busy = false;
            this.trace.snapshot("actor.reply", {
              actor_id: actor.id,
              value: toJsonValue(value),
            });
            if (message.handler.fallible && value.kind === "enum") {
              const shape = this.pkg.variants[message.handler.result_shape!];
              if (shape?.cases[value.tag]?.name === "Err") {
                if (message.reply)
                  message.complete(value.payload[0] ?? UNIT, "Failed");
                else {
                  this.crashActor(actor, {
                    kind: "panic",
                    message: "unhandled declared handler failure",
                  });
                  return;
                }
              } else {
                message.complete(value.payload[0] ?? UNIT, null);
              }
            } else {
              message.complete(value, null);
            }
            this.dispatchActor(actor);
          },
          failed: (fault) => {
            actor.active = null;
            actor.busy = false;
            if (fault.kind === "panic" && fault.cancelled) {
              message.complete(null, "Dead");
              this.dispatchActor(actor);
            } else {
              this.crashActor(actor, fault);
              message.complete(null, "Trapped");
            }
          },
        },
      );
      actor.active = frame.context;
      this.runFrame(frame);
    });
  }

  private crashActor(actor: ActorInstance, fault: Fault): void {
    const debt = ++this.nextFaultDebt;
    this.faultDebts.add(debt);
    if (actor.supervisor)
      actor.supervisor.owner.recovering.add(actor.supervisor.child);
    actor.crashing ??= fault;
    actor.alive = false;
    actor.busy = false;
    this.trace.snapshot("actor.crash", {
      actor_id: actor.id,
      message: this.faultText(fault),
    });
    const finished = (action: string) => {
      if (actor.drainingMailbox) {
        actor.mailboxDrained.push(() => finished(action));
        return;
      }
      this.closeValueAsync(
        actor.state,
        this.faultText(fault),
        () =>
          this.drainMailbox(actor, this.faultText(fault), () => {
            actor.completed = true;
            for (const wake of actor.closed.splice(0)) wake();
            for (const admit of actor.admission.splice(0)) admit();
            if (actor.supervisor) {
              const { owner, child } = actor.supervisor;
              if (action === "Restart") this.restartChild(owner, child, [debt]);
              else if (action === "Escalate")
                this.escalateSupervisor(owner, [debt]);
              else {
                owner.spent.add(child);
                owner.recovering.delete(child);
                this.wakeRoles();
              }
            }
          }),
        actor,
      );
    };
    if (actor.layout.crash !== undefined) {
      const [code, message] =
        fault.kind === "panic"
          ? [212, "UserPanic"]
          : {
              integer_overflow: [201, "IntegerOverflow"],
              divide_by_zero: [202, "DivideByZero"],
              shift_out_of_range: [204, "ShiftOutOfRange"],
              vector_bounds: [205, "IndexOutOfBounds"],
            }[fault.trap];
      const info: VmValue = {
        kind: "record",
        typeId: this.aggregateName(actor.layout.crash_info!),
        fields: [
          { kind: "i64", value: BigInt(code!) },
          { kind: "string", value: String(message) },
        ],
      };
      this.invokeFrame(
        actor,
        actor.layout.crash,
        [actor.state, info],
        (value) => {
          const shape = this.pkg.variants[actor.layout.crash_action!];
          const action =
            value.kind === "enum" ? shape?.cases[value.tag]?.name : undefined;
          if (!action)
            throw new Error("crash hook returned no declared action");
          finished(action);
        },
        () => finished("Kill"),
      );
    } else finished("Restart");
  }

  private supervisorFor(value: VmValue): SupervisorInstance {
    if (value.kind !== "supervisor")
      throw new Error("expected a supervisor handle");
    const role = this.roles.get(value.id);
    if (role) return this.supervisorFor((this.roleValue(role) ?? role.latest)!);
    const owner = this.supervisors.get(value.id);
    if (!owner) throw new Error("supervisor does not exist");
    return owner;
  }

  private startChild(
    owner: SupervisorInstance,
    child: number,
    done: () => void,
    failed: (fault: Fault) => void,
  ): void {
    const spec = owner.specs[child]!;
    this.invokeFrame(
      null,
      spec.spawn,
      owner.config,
      (value) => {
        owner.children[child] = value;
        if (value.kind === "actor")
          this.actorFor(value).supervisor = { owner, child };
        else if (value.kind === "supervisor")
          this.supervisorFor(value).supervisor = { owner, child };
        owner.recovering.delete(child);
        this.wakeRoles();
        this.trace.snapshot("supervisor.child", {
          supervisor_id: owner.id,
          child,
        });
        done();
      },
      failed,
    );
  }

  private roleOwner(role: RoleSlot): SupervisorInstance | null {
    const parentRole = this.roles.get(role.ownerId);
    const value = parentRole
      ? this.roleValue(parentRole)
      : ({ kind: "supervisor", id: role.ownerId } as const);
    return value?.kind === "supervisor"
      ? (this.supervisors.get(value.id) ?? null)
      : null;
  }

  private roleValue(role: RoleSlot): VmValue | null {
    const value = this.roleOwner(role)?.children[role.child] ?? null;
    if (value) role.latest = value;
    return value;
  }

  private rolePending(role: RoleSlot): boolean {
    const parentRole = this.roles.get(role.ownerId);
    if (parentRole && this.rolePending(parentRole)) return true;
    const owner = this.roleOwner(role);
    return (
      !!owner?.alive &&
      (owner.recovering.has(role.child) || owner.children[role.child] === null)
    );
  }

  private wakeRoles(): void {
    for (const role of this.roles.values()) {
      this.roleValue(role);
      if (!this.rolePending(role))
        for (const wake of role.waiting.splice(0)) wake();
    }
  }

  private restartChild(
    owner: SupervisorInstance,
    child: number,
    debts: number[],
  ): void {
    if (
      !owner.alive ||
      owner.specs[child]!.restart === "temporary" ||
      owner.spent.has(child)
    ) {
      owner.spent.add(child);
      owner.recovering.delete(child);
      this.wakeRoles();
      return;
    }
    if (owner.restartBusy) {
      owner.pendingRestarts.push({ child, debts });
      return;
    }
    const now = this.trace.virtualTimeMs;
    owner.restartTimes = owner.restartTimes.filter(
      (at) => now - at < owner.layout.window_secs * 1000,
    );
    if (owner.restartTimes.length >= owner.layout.max_restarts) {
      this.escalateSupervisor(owner, debts);
      return;
    }
    owner.restartTimes.push(now);
    owner.restartBusy = true;
    const affected = owner.children
      .map((_, index) => index)
      .filter(
        (index) =>
          !owner.spent.has(index) &&
          (owner.layout.strategy === "one_for_all" ||
            (owner.layout.strategy === "rest_for_one" && index >= child) ||
            index === child),
      );
    for (const index of affected) owner.recovering.add(index);
    this.trace.snapshot("supervisor.restart", {
      supervisor_id: owner.id,
      child,
    });
    const stopping = [...affected].reverse();
    const stop = () => {
      const index = stopping.shift();
      if (index === undefined) {
        start();
        return;
      }
      this.stopChild(owner.children[index]!, () => {
        owner.children[index] = null;
        stop();
      });
    };
    const starting = [...affected];
    const start = () => {
      if (!owner.alive) {
        owner.restartBusy = false;
        this.wakeRoles();
        return;
      }
      const index = starting.shift();
      if (index === undefined) {
        for (const debt of debts) this.faultDebts.delete(debt);
        for (const pending of owner.pendingRestarts.filter((pending) =>
          affected.includes(pending.child),
        )) {
          for (const debt of pending.debts) this.faultDebts.delete(debt);
        }
        owner.pendingRestarts = owner.pendingRestarts.filter(
          (pending) => !affected.includes(pending.child),
        );
        owner.restartBusy = false;
        this.wakeRoles();
        const pending = owner.pendingRestarts.shift();
        if (pending) this.restartChild(owner, pending.child, pending.debts);
        return;
      }
      this.startChild(owner, index, start, () => {
        owner.restartBusy = false;
        this.restartChild(owner, index, debts);
      });
    };
    stop();
  }

  private escalateSupervisor(owner: SupervisorInstance, debts: number[]): void {
    const parent = owner.supervisor;
    if (parent) parent.owner.recovering.add(parent.child);
    owner.closed.push(() => {
      if (parent) this.restartChild(parent.owner, parent.child, debts);
      else this.wakeRoles();
    });
    this.stopSupervisor(owner);
  }

  private stopChild(value: VmValue | null, done: () => void): void {
    if (!value) {
      done();
      return;
    }
    if (value.kind === "actor") {
      const actor = this.actorFor(value);
      if (actor.completed) {
        done();
        return;
      }
      actor.closed.push(done);
      this.requestActorClose(actor);
    } else if (value.kind === "supervisor") {
      const nested = this.supervisorFor(value);
      if (nested.completed) {
        done();
        return;
      }
      nested.closed.push(done);
      this.stopSupervisor(nested);
    } else throw new Error("supervisor child is neither actor nor supervisor");
  }

  private stopSupervisor(owner: SupervisorInstance): void {
    if (!owner.alive) return;
    owner.alive = false;
    const children = [...owner.children].reverse();
    const next = () => {
      if (children.length) {
        this.stopChild(children.shift()!, next);
        return;
      }
      this.closeValueAsync(
        { kind: "record", typeId: "", fields: owner.config },
        null,
        (fault) => {
          if (fault) this.faultDebts.add(++this.nextFaultDebt);
          owner.completed = true;
          for (const wake of owner.closed.splice(0)) wake();
          this.wakeRoles();
        },
      );
    };
    next();
  }

  private drainMailbox(
    actor: ActorInstance,
    fault: string | null,
    done: (fault: Fault | null) => void,
  ): void {
    const messages = actor.mailbox.splice(0);
    actor.drainingMailbox = true;
    this.closeValueAsync(
      {
        kind: "vector",
        elementType: "message",
        items: messages.map((message) => ({
          kind: "record",
          typeId: "",
          fields: message.payload,
        })),
      },
      fault,
      (closeFault) => {
        actor.drainingMailbox = false;
        for (const message of messages) message.complete(null, "Dead");
        done(closeFault);
        for (const wake of actor.mailboxDrained.splice(0)) wake();
      },
      actor,
    );
  }

  private requestActorClose(actor: ActorInstance): void {
    if (actor.closing || actor.completed) return;
    actor.closing = true;
    // Hold terminal state release until both the active turn and queued owners
    // have drained; authored message cleanup can itself call or suspend.
    actor.drainingMailbox = true;
    actor.active?.cancel?.();
    this.drainMailbox(actor, null, (fault) => {
      actor.closingFault ??= fault;
      this.dispatchActor(actor);
    });
    for (const admit of actor.admission.splice(0)) admit();
  }

  private stopActor(actor: ActorInstance): void {
    actor.busy = true;
    const hooks = [...actor.layout.stop];
    const next = () => {
      const hook = hooks.shift();
      if (hook !== undefined) {
        this.invokeFrame(actor, hook, [actor.state], next, (fault) =>
          this.crashActor(actor, fault),
        );
      } else {
        this.closeValueAsync(actor.state, null, (fault) => {
          if (fault) {
            this.crashActor(actor, fault);
            return;
          }
          actor.completed = true;
          actor.alive = false;
          actor.busy = false;
          this.trace.snapshot("actor.stop", { actor_id: actor.id });
          for (const wake of actor.closed.splice(0)) wake();
        });
      }
    };
    next();
  }

  // ── exit and faults ──────────────────────────────────────────────────────

  private haltWithFault(fault: Fault): never {
    const isPanic = fault.kind === "panic";
    const status: RuntimeStatus = isPanic ? "panic" : "trap";
    const trapKind: TrapKind = isPanic ? "panic" : fault.trap;
    const message = isPanic
      ? fault.message
      : (fault.message ?? trapMessage(fault.trap));
    // A failing run reports no exit code: native exits 1 for every fault and
    // names the kind in its message, so the kind is what travels, in
    // `runtime_failures`. The page turns it into an exit code of its own.
    this.trace.fail(
      status,
      "runtime.failure",
      runtimeFailure(isPanic ? "panic" : "trap", message, trapKind, null),
    );
    throw new Halt(status);
  }
}

// ── refs ────────────────────────────────────────────────────────────────────

function ownedRef(value: VmValue): Ref {
  return { kind: "cell", cell: { value, valid: true } };
}

function writeRef(ref: Ref, value: VmValue): void {
  switch (ref.kind) {
    case "cell":
      ref.cell.value = value;
      ref.cell.valid = true;
      return;
    case "field":
      fieldsOf(readRef(ref.parent))[ref.index] = value;
      return;
    case "payload":
      payloadOf(readRef(ref.parent))[ref.index] = value;
      return;
  }
}

/// Only a cell of its own can be ended; a projection lives as long as the
/// storage it reaches through.
function invalidateRef(ref: Ref): void {
  if (ref.kind === "cell") ref.cell.valid = false;
  else writeRef(ref, UNIT);
}

function readRef(ref: Ref): VmValue {
  switch (ref.kind) {
    case "cell":
      if (!ref.cell.valid) {
        throw new Error(
          "read of a value whose ownership was already transferred or released",
        );
      }
      return ref.cell.value;
    case "field":
      return fieldsOf(readRef(ref.parent))[ref.index] ?? UNIT;
    case "payload":
      return payloadOf(readRef(ref.parent))[ref.index] ?? UNIT;
  }
}

// ── value helpers ───────────────────────────────────────────────────────────

function fieldsOf(value: VmValue): VmValue[] {
  if (value.kind !== "record") {
    throw new TypeError(`expected a record or tuple, got ${value.kind}`);
  }
  return value.fields;
}

function payloadOf(value: VmValue): VmValue[] {
  if (value.kind !== "enum") {
    throw new TypeError(`expected an enum value, got ${value.kind}`);
  }
  return value.payload;
}

function tagOf(value: VmValue): number {
  if (value.kind !== "enum") {
    throw new TypeError(`expected an enum value, got ${value.kind}`);
  }
  return value.tag;
}

function calleeFunction(value: VmValue): number {
  switch (value.kind) {
    case "function":
      return Number(value.id);
    case "closure":
      return value.body;
    default:
      throw new TypeError(
        `indirect.call expected a callable value, got ${value.kind}`,
      );
  }
}

function truth(value: VmValue): boolean {
  return value.kind === "bool"
    ? value.value
    : value.kind === "i64" && value.value !== 0n;
}

function bytesValue(bytes: readonly number[]): VmValue {
  return {
    kind: "vector",
    elementType: "u8",
    items: bytes.map((byte) => ({ kind: "i64", value: BigInt(byte) })),
  };
}

function renderMessage(value: VmValue): string {
  return value.kind === "string" ? value.value : canonical(value);
}

function canonical(value: VmValue): string {
  return JSON.stringify(toComparable(value));
}

function toComparable(value: VmValue): unknown {
  switch (value.kind) {
    case "i64":
      return value.value.toString();
    case "record":
      return value.fields.map(toComparable);
    case "enum":
      return [value.tag, value.payload.map(toComparable)];
    case "vector":
      return value.items.map(toComparable);
    case "rc":
      return toComparable(value.cell.value);
    case "map":
      return [...value.entries.values()].map((entry) => [
        toComparable(entry.key),
        toComparable(entry.value),
      ]);
    case "unit":
      return null;
    case "bool":
    case "f64":
    case "string":
      return value.value;
    default:
      return value.kind;
  }
}

/// A literal is held at its declared precision from the moment it is loaded,
/// so an `f32` never carries digits a single cannot represent.
function floatConst(op: Extract<OpV1, { op: "const.float" }>): number {
  switch (op.nonfinite) {
    case "nan":
      return Number.NaN;
    case "inf":
      return Number.POSITIVE_INFINITY;
    case "-inf":
      return Number.NEGATIVE_INFINITY;
    default:
      return narrowFloat(op.value ?? 0, op.ty);
  }
}

function valueCloseePlace(detail: unknown): number | null {
  const place = (detail as { place?: unknown } | null | undefined)?.place;
  return typeof place === "number" ? place : null;
}

function trapMessage(trap: TrapName): string {
  switch (trap) {
    case "integer_overflow":
      return "integer overflow";
    case "divide_by_zero":
      return "divide by zero";
    case "shift_out_of_range":
      return "shift amount out of range";
    case "vector_bounds":
      return "index out of bounds";
  }
}

// ── scalar arithmetic ───────────────────────────────────────────────────────

interface IntTy {
  bits: bigint;
  signed: boolean;
}

/// The operand type the package records. `isize` and `usize` are 64-bit in the
/// VM, matching native execution.
function intTy(name: string | undefined): IntTy {
  switch (name) {
    case "i8":
      return { bits: 8n, signed: true };
    case "i16":
      return { bits: 16n, signed: true };
    case "i32":
      return { bits: 32n, signed: true };
    case "u8":
      return { bits: 8n, signed: false };
    case "u16":
      return { bits: 16n, signed: false };
    case "u32":
      return { bits: 32n, signed: false };
    case "u64":
    case "usize":
      return { bits: 64n, signed: false };
    default:
      return { bits: 64n, signed: true };
  }
}

/// The VM holds every float as a double, so an `f32` operand rounds to single
/// precision after each step. `16777216f32 + 1.0 + 1.0` is `16777216` natively
/// because neither addition is representable, and rounding here reproduces it.
function narrowFloat(value: number, name: string | undefined): number {
  return name === "f32" ? Math.fround(value) : value;
}

function narrowInt(value: bigint, name: string | undefined): bigint {
  const ty = intTy(name);
  return ty.signed
    ? BigInt.asIntN(Number(ty.bits), value)
    : BigInt.asUintN(Number(ty.bits), value);
}

function inRange(value: bigint, ty: IntTy): boolean {
  return ty.signed
    ? value >= -(1n << (ty.bits - 1n)) && value < 1n << (ty.bits - 1n)
    : value >= 0n && value < 1n << ty.bits;
}

/// Which failure edge a `checked.binary` must take, or `null` for the normal
/// edge. The VM never traps here: the package names every outcome.
function checkedFailure(
  op: string,
  lhs: VmValue,
  rhs: VmValue,
  ty: string | undefined,
): TrapName | null {
  if (lhs.kind !== "i64" || rhs.kind !== "i64") {
    return null;
  }
  const scalar = intTy(ty);
  if ((op === "Divide" || op === "Modulo") && rhs.value === 0n) {
    return "divide_by_zero";
  }
  if (
    (op === "Shl" || op === "Shr") &&
    (rhs.value < 0n || rhs.value >= scalar.bits)
  ) {
    return "shift_out_of_range";
  }
  const exact = exactInt(op, lhs.value, rhs.value);
  return exact !== null && !inRange(exact, scalar) ? "integer_overflow" : null;
}

function exactInt(op: string, lhs: bigint, rhs: bigint): bigint | null {
  switch (op) {
    case "Add":
      return lhs + rhs;
    case "Subtract":
      return lhs - rhs;
    case "Multiply":
      return lhs * rhs;
    case "Divide":
      return rhs === 0n ? null : lhs / rhs;
    case "Modulo":
      return rhs === 0n ? null : lhs % rhs;
    default:
      // A shift's only checked outcome is its count; the shifted result wraps.
      return null;
  }
}

function applyUnary(op: string, value: VmValue, ty: string): VmValue {
  switch (op) {
    case "Not":
      return { kind: "bool", value: !truth(value) };
    case "Negate":
      return value.kind === "f64"
        ? { kind: "f64", value: narrowFloat(-value.value, ty) }
        : { kind: "i64", value: narrowInt(-asInt(value), ty) };
    case "BitNot":
      return { kind: "i64", value: narrowInt(~asInt(value), ty) };
    default:
      throw new Error(`unary operator ${op} has no sequential executor`);
  }
}

function applyBinary(
  op: string,
  lhs: VmValue,
  rhs: VmValue,
  ty: string,
): VmValue {
  switch (op) {
    case "Equal":
      return { kind: "bool", value: equals(lhs, rhs) };
    case "NotEqual":
      // `fcmp UNE`: unequal, or either operand is NaN.
      return { kind: "bool", value: !equals(lhs, rhs) };
    case "Less":
    case "LessEqual":
    case "Greater":
    case "GreaterEqual":
      return { kind: "bool", value: ordered(op, lhs, rhs) };
    case "And":
      return { kind: "bool", value: truth(lhs) && truth(rhs) };
    case "Or":
      return { kind: "bool", value: truth(lhs) || truth(rhs) };
    default:
      break;
  }
  if (lhs.kind === "f64" || rhs.kind === "f64") {
    return {
      kind: "f64",
      value: narrowFloat(floatBinary(op, asFloat(lhs), asFloat(rhs)), ty),
    };
  }
  if (lhs.kind === "string") {
    return { kind: "string", value: lhs.value + asString(rhs) };
  }
  return {
    kind: "i64",
    value: narrowInt(intBinary(op, asInt(lhs), asInt(rhs), intTy(ty)), ty),
  };
}

/// The non-trapping family: wrapping arithmetic and bitwise operations. The
/// caller narrows the result to the operand type, which is the wrap.
function intBinary(op: string, lhs: bigint, rhs: bigint, ty: IntTy): bigint {
  switch (op) {
    case "Add":
    case "WrappingAdd":
      return lhs + rhs;
    case "Subtract":
    case "WrappingSub":
      return lhs - rhs;
    case "Multiply":
    case "WrappingMul":
      return lhs * rhs;
    case "Divide":
      return rhs === 0n ? 0n : lhs / rhs;
    case "Modulo":
      return rhs === 0n ? 0n : lhs % rhs;
    case "BitAnd":
      return lhs & rhs;
    case "BitOr":
      return lhs | rhs;
    case "BitXor":
      return lhs ^ rhs;
    case "Shl":
      return lhs << (rhs % ty.bits);
    case "Shr":
      return lhs >> (rhs % ty.bits);
    default:
      throw new Error(`binary operator ${op} has no sequential executor`);
  }
}

function floatBinary(op: string, lhs: number, rhs: number): number {
  switch (op) {
    case "Add":
    case "WrappingAdd":
      return lhs + rhs;
    case "Subtract":
    case "WrappingSub":
      return lhs - rhs;
    case "Multiply":
    case "WrappingMul":
      return lhs * rhs;
    case "Divide":
      return lhs / rhs;
    case "Modulo":
      return lhs % rhs;
    default:
      throw new Error(`binary operator ${op} has no float executor`);
  }
}

function applyCast(value: VmValue, to: string): VmValue {
  switch (to) {
    case "f32":
    case "f64":
      return { kind: "f64", value: narrowFloat(asFloat(value), to) };
    case "bool":
      return { kind: "bool", value: truth(value) };
    case "char":
    case "string":
      return { kind: "string", value: asString(value) };
    default:
      break;
  }
  if (value.kind === "f64") {
    // Native float-to-integer conversion saturates at the target's bounds.
    return { kind: "i64", value: saturate(value.value, intTy(to)) };
  }
  return { kind: "i64", value: narrowInt(asInt(value), to) };
}

function saturate(value: number, ty: IntTy): bigint {
  if (Number.isNaN(value)) {
    return 0n;
  }
  const low = ty.signed ? -(1n << (ty.bits - 1n)) : 0n;
  const high = (ty.signed ? 1n << (ty.bits - 1n) : 1n << ty.bits) - 1n;
  const truncated = BigInt(
    Math.trunc(Math.max(Math.min(value, Number(high)), Number(low))),
  );
  return truncated < low ? low : truncated > high ? high : truncated;
}

/// Structural equality, matching native codegen. An f64 leaf compares with
/// `fcmp OEQ` (NaN equals nothing, the two zeros are equal), so a record or
/// enum containing one compares that field the same way rather than through a
/// canonical rendering that would collapse NaN.
function equals(lhs: VmValue, rhs: VmValue): boolean {
  if (lhs.kind === "f64" || rhs.kind === "f64") {
    return lhs.kind === "f64" && rhs.kind === "f64" && lhs.value === rhs.value;
  }
  if (lhs.kind === "record" && rhs.kind === "record") {
    return (
      lhs.fields.length === rhs.fields.length &&
      lhs.fields.every((field, at) => equals(field, rhs.fields[at]!))
    );
  }
  if (lhs.kind === "enum" && rhs.kind === "enum") {
    return (
      lhs.tag === rhs.tag &&
      lhs.payload.length === rhs.payload.length &&
      lhs.payload.every((field, at) => equals(field, rhs.payload[at]!))
    );
  }
  if (lhs.kind === "vector" && rhs.kind === "vector") {
    return (
      lhs.items.length === rhs.items.length &&
      lhs.items.every((item, at) => equals(item, rhs.items[at]!))
    );
  }
  return canonical(lhs) === canonical(rhs);
}

function ordered(op: string, lhs: VmValue, rhs: VmValue): boolean {
  const left =
    lhs.kind === "string"
      ? lhs.value
      : lhs.kind === "f64"
        ? lhs.value
        : asInt(lhs);
  const right =
    rhs.kind === "string"
      ? rhs.value
      : rhs.kind === "f64"
        ? rhs.value
        : asInt(rhs);
  switch (op) {
    case "Less":
      return left < right;
    case "LessEqual":
      return left <= right;
    case "Greater":
      return left > right;
    default:
      return left >= right;
  }
}

function asInt(value: VmValue): bigint {
  switch (value.kind) {
    case "i64":
      return value.value;
    case "bool":
      return value.value ? 1n : 0n;
    case "f64":
      return BigInt(Math.trunc(value.value));
    case "string":
      return BigInt(value.value.codePointAt(0) ?? 0);
    default:
      return 0n;
  }
}

function asFloat(value: VmValue): number {
  switch (value.kind) {
    case "f64":
      return value.value;
    case "i64":
      return Number(value.value);
    case "bool":
      return value.value ? 1 : 0;
    default:
      return 0;
  }
}

function asString(value: VmValue): string {
  return value.kind === "string" ? value.value : canonical(value);
}

function liveRef(ref: Ref): boolean {
  return ref.kind === "cell" ? ref.cell.valid : liveRef(ref.parent);
}
