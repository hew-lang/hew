import {
  BrowserDirectories,
  type BrowserIoError,
  type DirectoryOperation,
} from "./browser-directory.js";
import type { EncodingFormat } from "./encoding-values.js";
/// The `hew.sandbox.bytecode.v1` executor.
///
/// The package is a projection of verified ownership SIR, so this executor
/// carries out the decisions it records and makes none of its own: it copies
/// where the package says `copy_value`, releases where it says `destroy_value`,
/// follows the `unwind` edge the package names, and takes the failure edge a
/// `checked.binary` names rather than trapping on its own.

import { executeCodec } from "./codecs.js";
import { actorIdentical } from "./actor-identity.js";
import { ActorWaits } from "./actor-waits.js";
import { TraceBuilder, runtimeFailure } from "../trace.js";
import type {
  JsonValue,
  ReplayConfig,
  RunOptions,
  RuntimeStatus,
  SandboxTrace,
  TrapKind,
} from "../types.js";
import {
  UNIT,
  cloneValue,
  stringValue,
  toJsonValue,
  type VmValue,
  type RcOwner,
} from "../values.js";
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
  RuntimeVariantRole,
} from "./package.js";
import { runtimeTag } from "./package.js";
import { StructuralRenderer } from "./structural.js";
import { timestampBinary } from "./timestamp.js";
import {
  selectedValue,
  collectionOperation,
  sharedOperation,
  type ValueProgram,
} from "./value-operations.js";
import { Pipes } from "./pipes.js";
import { Semaphores, type Acquisition } from "./semaphores.js";
import { FrameScheduler } from "./scheduler.js";
import { DeterministicIds } from "../../scheduler/ids.js";
import { Mt19937 } from "./mt19937.js";
import {
  ShimFault,
  CODEC_REGISTRATION,
  decodeUtf8Checked,
  resolveExtern,
  resolveRuntime,
  resolveSuspension,
  type ShimHost,
  type RuntimeShim,
  type RuntimeContinuation,
} from "./shims.js";
import { AdmittedPackage } from "./validate.js";
import type { CheckedPackage } from "../schema-validator.js";

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

type Fault = (
  | {
      kind: "panic";
      message: string;
      assertion?: { operator: string; left: string; right: string };
      cancelled?: boolean;
      deadline?: boolean;
      handlerFailed?: boolean;
      timerOverdue?: boolean;
      actorSendFailed?: boolean;
    }
  | { kind: "trap"; trap: TrapName; message?: string }
) & { primary?: Fault; secondary?: Fault[]; reported?: boolean };

type CallerStamp =
  | { kind: "Task" | "Timer" }
  | { kind: "Actor"; identity: bigint };

const DEFAULT_ACTOR_STOP_NS = 5_000_000_000;

interface FrameContext {
  fileError?: BrowserIoError | null;
  parked?: boolean;
  rcOwner?: RcOwner | null;
  pollBudget?: { remaining: number };
  pollSuppression?: number;
  pendingCancellation?: Fault;
  requestDeadline?: number;
  caller?: CallerStamp;
  reply?: boolean;
  id: string;
  actor: ActorInstance | null;
  cancel?: (fault?: Fault) => void;
  generator?: GeneratorEntry;
  returned(value: VmValue): void;
  failed(fault: Fault, handback?: VmValue | null): void;
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

interface GeneratorStream {
  generator: GeneratorEntry;
  closed: boolean;
  pending: VmValue | null;
  fault: Fault | null;
  receiving: boolean;
  waiting?: (value: VmValue | null, fault: Fault | null) => void;
}

interface SenderOwner {
  sink: VmValue;
  encoder: VmValue;
  references: number;
  endpoints: number;
  busy: boolean;
  waiting: Set<() => void>;
}

interface SenderEndpoint {
  owner: SenderOwner;
  open: boolean;
}

interface ActorTimer {
  cancel(): void;
}

interface OneShotTimer extends ActorTimer {
  started(): boolean;
  discarded(): void;
}

interface ActorMessage {
  timer?: OneShotTimer;
  requestDeadline?: number;
  caller: CallerStamp;
  handler: ActorShape["handlers"][number];
  payload: VmValue[];
  reply: boolean;
  obligated: boolean;
  complete(
    value: VmValue | null,
    error: string | null,
    drained?: (fault: Fault | null) => void,
  ): void;
}

interface ActorInstance {
  /** Native encoding error slots follow actor identity across handler turns. */
  encodingErrors?: Partial<Record<EncodingFormat, string>>;
  rcOwner: RcOwner;
  identity: bigint;
  id: string;
  layout: ActorShape;
  state: VmValue;
  mailbox: ActorMessage[];
  timers: Set<ActorTimer>;
  disposals: VmValue[];
  admission: Array<() => void>;
  busy: boolean;
  constructing: boolean;
  alive: boolean;
  completed: boolean;
  closing: boolean;
  terminating: boolean;
  drainingMailbox: boolean;
  mailboxDrained: Array<() => void>;
  closingFault: Fault | null;
  closeFaultDebt?: number;
  crashing: Fault | null;
  active: FrameContext | null;
  terminalHook: FrameContext | null;
  closed: Array<() => void>;
  supervisor?: { owner: SupervisorInstance; child: number };
}

interface SupervisorInstance {
  id: string;
  layout: SupervisorShape;
  config: VmValue[];
  children: Array<VmValue | null>;
  alive: boolean;
  terminating: boolean;
  teardownChild: VmValue | null;
  teardownCancelTimer: (() => void) | null;
  restartTimes: number[];
  backoff: { initial: bigint; maximum: bigint } | null;
  restartDelays: Map<number, { delay: bigint; publishedAt: number | null }>;
  restartTimers: Map<number, () => void>;
  specs: SupervisorShape["children"];
  offsets: number[];
  recovering: Set<number>;
  restartBusy: boolean;
  constructor: FrameContext | null;
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
  grouped: boolean;
  id: string;
  done: boolean;
  finishing: boolean;
  value: VmValue;
  fault: Fault | null;
  actorError: string | null;
  actorTarget?: ActorInstance;
  cancel(): void;
  // Actor-call handles own admission and reply storage. Scoped task handles
  // leave their execution and any unclaimed result with the enclosing scope.
  close?: (done: (fault: Fault | null) => void) => void;
  completedAt: number;
  changed: Array<() => void>;
}

interface TaskGroup {
  expiresAt?: number;
  tasks: TaskEntry[];
  changed: Array<() => void>;
  deadline: Fault | null;
  joining: boolean;
  cancelTimer?: () => void;
}

interface Activation {
  rcOwner: RcOwner | null;
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
  /// Where the caller binds the receiver a failing `var self` method hands
  /// back on the unwind edge.
  handback?: ValueDef | null;
  erasedHandback?: Extract<VmValue, { kind: "dyn" }>;
  /// A consuming invocation of a borrowed closure body owns this receiver
  /// until both the body and its capture cleanup have completed.
  consumedCallable?: Extract<VmValue, { kind: "closure" }>;
}

/// Ends the run. The status is already recorded on the trace.
class Halt extends Error {
  constructor(readonly status: RuntimeStatus) {
    super(status);
  }
}

/// Page stdin travels as a replay input and is handed out one line per read at
/// run time, so a loop sees successive lines. A line keeps its terminator and
/// the unterminated tail is returned as it is, matching native
/// `hew_stdin_read_line`; `null` is end of input.
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

  readLine(): Uint8Array | null {
    if (this.offset >= this.bytes.length) {
      return null;
    }
    const start = this.offset;
    const newline = this.bytes.indexOf(0x0a, start);
    this.offset = newline < 0 ? this.bytes.length : newline + 1;
    return this.bytes.slice(start, this.offset);
  }
}

export async function runPackageV1(
  pkg: CheckedPackage,
  options: RunOptions = {},
): Promise<SandboxTrace> {
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
  const admission = AdmittedPackage.load(pkg, options);
  if (!(admission instanceof AdmittedPackage)) {
    trace.reject(admission);
    return trace.finish();
  }

  const directoryRefusal = await BrowserDirectories.admission(
    options.directoryRoots,
  );
  if (directoryRefusal) {
    trace.reject({
      category: "not_implemented",
      code: "sandbox.host.directory",
      capability: "directoryRoots",
      message: directoryRefusal,
      span: null,
    });
    return trace.finish();
  }

  const vm = new ExecutorV1(
    admission,
    trace,
    StdinReader.fromReplay(replay.inputs),
    options.schedulerPolicy ?? "round_robin",
    options,
  );
  try {
    await vm.run();
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
  private encodingErrors: Partial<Record<EncodingFormat, string>> = {};
  private current: Activation;
  private readonly host: ShimHost;
  private readonly scheduler: FrameScheduler;
  private readonly ids: DeterministicIds;
  private readonly actors = new Map<string, ActorInstance>();
  private readonly actorWaits = new ActorWaits<ActorInstance>();
  private readonly supervisors = new Map<string, SupervisorInstance>();
  private readonly roles = new Map<string, RoleSlot>();
  private readonly pipes = new Pipes();
  private readonly generators = new Map<string, GeneratorEntry>();
  private readonly generatorStreams = new Map<string, GeneratorStream>();
  private readonly senderEndpoints = new Map<string, SenderEndpoint>();
  private readonly tasks = new Map<string, TaskEntry>();
  private completedTasks = 0;
  private actorIdentity = 0n;
  private readonly faultDebts = new Set<number>();
  private nextFaultDebt = 0;
  private readonly closingResources = new WeakSet<object>();
  private readonly consumedResources = new WeakSet<object>();
  private running = false;
  private rootComplete = false;
  private terminating = false;
  private rootFault: Fault | null = null;

  private readonly pkg: PackageV1;
  private readonly directories: BrowserDirectories;
  private readonly semaphores: Semaphores;

  constructor(
    admission: AdmittedPackage,
    private readonly trace: TraceBuilder,
    private readonly stdin: StdinReader,
    policy: "round_robin" | "chaos",
    private readonly options: RunOptions,
  ) {
    const pkg = admission.bytecode;
    this.pkg = pkg;
    this.directories = new BrowserDirectories(options.directoryRoots);
    this.scheduler = new FrameScheduler(trace, policy);
    this.semaphores = new Semaphores((nanos, ready) =>
      this.scheduler.after(nanos, ready),
    );
    this.ids = new DeterministicIds(trace.replay.seed);
    this.host = {
      argv: [...(options.argv ?? ["main.hew"])],
      unixTimeMillis: () => this.trace.virtualTimeMs,
      monotonicTimeNanos: () => {
        const millis = this.trace.virtualTimeMs;
        return (
          BigInt(Math.trunc(millis)) * 1_000_000n +
          BigInt(Math.round((millis % 1) * 1_000_000))
        );
      },
      calendarBridge: options.calendarBridge,
      codecBridge: options.codecBridge,
      encodingError: (format, message) => {
        const actor = this.current.context.actor;
        const errors = actor
          ? (actor.encodingErrors ??= {})
          : this.encodingErrors;
        if (message !== undefined) errors[format] = message;
        return errors[format] ?? "";
      },
      setFileError: (message) => {
        this.current.context.fileError =
          message === null ? null : { kind: 0, message };
      },
      fileError: (operation) => {
        const error = this.current.context.fileError;
        switch (operation) {
          case "HasError":
            return { kind: "bool", value: error != null };
          case "LastErrorKind":
            return { kind: "i64", value: BigInt(error?.kind ?? 0) };
          case "LastErrno":
            if (error) error.kind = 0;
            return { kind: "i64", value: 0n };
          case "LastError":
            this.current.context.fileError = null;
            return stringValue(error?.message ?? "");
        }
      },
      pipes: this.pipes,
      semaphores: this.semaphores,
      generatorStream: (value) => {
        const id = this.ids.channel();
        this.generatorStreams.set(id, {
          generator: this.generatorFor(value),
          closed: false,
          pending: null,
          fault: null,
          receiving: false,
        });
        return { kind: "stream", channelId: id };
      },
      encoderSink: (sink, encoder) =>
        this.senderEndpoint(
          {
            sink,
            encoder,
            references: 0,
            endpoints: 0,
            busy: false,
            waiting: new Set(),
          },
          !this.sinkClosed(sink),
        ),
      cloneSink: (value) => {
        const endpoint =
          value.kind === "sink"
            ? this.senderEndpoints.get(value.channelId)
            : undefined;
        return endpoint
          ? this.senderEndpoint(endpoint.owner, endpoint.open)
          : this.pipes.cloneSink(value);
      },
      newPipe: (capacity) => this.newPipe(capacity),
      releaseValue: (value) =>
        this.closeValue(value, this.pipeFault(this.current)),
      writeStdout: (text) => this.trace.writeStdout(text, null),
      writeStderr: (text) => this.trace.writeStderr(text, null),
      prng: new Mt19937(),
      regexPatterns: pkg.regex_patterns,
      enumValue: (shape, role, payload) => ({
        kind: "enum",
        typeId: shape.name,
        shape: shape.id,
        tag: runtimeTag(shape, role),
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
          this.completeRoot();
          this.trace.exitCode =
            pkg.entry?.exit === "status" && value.kind === "i64"
              ? Number(value.value)
              : 0;
        },
        failed: (fault) => {
          this.rootFault = fault;
          this.completeRoot();
        },
      },
    );
  }

  /// One instruction or terminator executed is one step, ownership ops
  /// included: the package's ops are what the program does.
  async run(): Promise<void> {
    const root = this.current.context;
    const cancel = () => {
      const fault: Fault = {
        kind: "panic",
        message: "execution cancelled",
        cancelled: true,
      };
      if (root.cancel) root.cancel(fault);
      else root.pendingCancellation ??= fault;
    };
    this.options.signal?.addEventListener("abort", cancel, { once: true });
    if (this.options.signal?.aborted) cancel();
    try {
      this.runFrame(this.current);
      await this.scheduler.run();
      if (!this.rootComplete) {
        throw new Error("the root frame is waiting with no runnable work");
      }
      // Native main return drains accepted work before closing idle actors.
      for (const owner of this.supervisors.values())
        if (!owner.supervisor) this.stopSupervisor(owner);
      for (const actor of this.actors.values())
        if (!actor.supervisor && actor.alive) this.requestActorStop(actor);
      await this.scheduler.run();
      if (this.rootFault) this.haltWithFault(this.rootFault);
      if (this.trace.exitCode === 0 && this.faultDebts.size)
        this.trace.exitCode = 1;
    } finally {
      this.terminating = true;
      await this.scheduler.retireHost();
      this.directories.retire((value) => this.closeHostResource(value));
      this.options.signal?.removeEventListener("abort", cancel);
    }
  }

  private runFrame(frame: Activation): void {
    this.current = frame;
    this.running = true;
    while (this.running) {
      const act = this.current;
      act.context.cancel = (
        fault = { kind: "panic", message: "task cancelled", cancelled: true },
      ) => {
        if (this.scopeDeadline(act, fault) !== fault)
          act.context.pendingCancellation ??= fault;
      };
      while (act.opIndex < act.block.ops.length) {
        const op = act.block.ops[act.opIndex++]!;
        if (op.op !== "coverage.hit") this.commitStep();
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
    normal: Edge | null | undefined,
    unwind: Edge | null,
    context?: FrameContext,
  ): Activation {
    const mandatoryClose = (this.pkg.resources ?? []).some(
      (resource) => "close" in resource && resource.close === fn.id,
    );
    const parentContext = caller?.context ?? context!;
    parentContext.pollBudget ??= { remaining: 0 };
    if (mandatoryClose && args[0]) this.closingResources.add(args[0]);
    const executionContext = mandatoryClose
      ? {
          ...parentContext,
          cancel: undefined,
          pendingCancellation: undefined,
          requestDeadline: undefined,
        }
      : parentContext;
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
        if ("dst" in op && op.dst != null && op.own === "none")
          trivial.add(op.dst);
      }
      const term = block.term;
      if ("result" in term && term.result && term.result !== "never")
        record(term.result);
      if (term.op === "switch.variant")
        for (const arm of term.arms) arm.fields.forEach(record);
    }
    return {
      rcOwner: fn.actor_construction
        ? { blocks: new Set(), closed: false }
        : (caller?.rcOwner ??
          executionContext.rcOwner ??
          executionContext.actor?.rcOwner ??
          null),
      context: executionContext,
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
      normal: normal ?? undefined,
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
      case "coverage.hit": {
        const key = String(op.counter);
        this.trace.coverage[key] = (this.trace.coverage[key] ?? 0) + 1;
        return;
      }
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
        this.define(act, op.dst, stringValue(op.value));
        return;
      case "const.unit":
        this.define(act, op.dst, UNIT);
        return;
      case "const.duration":
        this.define(act, op.dst, { kind: "i64", value: BigInt(op.nanos) });
        return;
      case "const.str":
        this.define(act, op.dst, stringValue(this.pkg.strings[op.str] ?? ""));
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
        writeRef(this.placeRef(act, op.place), this.read(act, op.value));
        return;
      case "store.assign": {
        const ref = this.placeRef(act, op.place);
        const replacement = this.read(act, op.value);
        const previous = liveRef(ref) ? readRef(ref) : UNIT;
        // Retire the old owner before invoking user close code. Publish the
        // replacement after cleanup, including when close records a fault;
        // the following cleanup.dispatch then drains the new owner once.
        invalidateRef(ref);
        this.releaseOperation(act, previous, () => writeRef(ref, replacement));
        return;
      }
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
          typeId: op.shape === null ? "" : this.aggregateName(op.shape),
          shape: op.shape ?? undefined,
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
          shape: op.shape,
          tag: op.variant,
          payload: op.fields.map((id) => this.read(act, id)),
        });
        return;
      case "variant.is":
        this.define(act, op.dst, {
          kind: "bool",
          value:
            this.variantAt(this.read(act, op.source), op.shape).tag ===
            op.variant,
        });
        return;
      case "variant.project_copy":
        this.define(
          act,
          op.dst,
          cloneValue(
            this.variantAt(this.read(act, op.source), op.shape, op.variant)
              .payload[op.field] ?? UNIT,
          ),
        );
        return;
      case "variant.project_borrow":
        this.variantAt(this.read(act, op.source), op.shape, op.variant);
        act.env.set(op.dst, {
          kind: "payload",
          parent: this.refOf(act, op.source),
          index: op.field,
        });
        return;
      case "variant.destructure":
        this.spread(
          act,
          this.variantAt(this.read(act, op.source), op.shape, op.variant)
            .payload,
          op.results,
        );
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
          entries: shape.entries,
          specialization: null,
          environment: {
            kind: "record",
            typeId: "",
            fields: op.fields.map((id) => this.read(act, id)),
          },
        });
        return;
      }
      case "callable.coerce": {
        const callable = this.read(act, op.source);
        if (op.specialization === null) {
          this.define(act, op.dst, callable);
        } else {
          if (
            callable.kind !== "closure" ||
            !callable.entries.some(
              (entry) => entry.specialization === op.specialization,
            )
          ) {
            throw new Error(
              "callable specialization has no checked invocation entry",
            );
          }
          this.define(act, op.dst, {
            ...callable,
            specialization: op.specialization,
          });
        }
        return;
      }
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
          group.expiresAt =
            this.trace.virtualTimeMs +
            Math.max(0, Number(duration.value) / 1_000_000);
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
          rcOwner: act.rcOwner,
          requestDeadline: this.requestDeadline(act),
          returned: (value) =>
            this.closeValueAsync(
              value,
              null,
              (fault) => this.finishGenerator(generator, fault),
              act.context.actor,
              act.rcOwner,
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
      case "task.race": {
        const scope = act.scopes.get(op.scope);
        if (!scope) throw new Error("race has no task scope");
        const members = op.members.map((operand) => {
          const member = this.taskFor(this.read(act, operand));
          this.invalidate(act, operand);
          if (member.grouped) throw new Error("race reuses a task handle");
          member.grouped = true;
          return member;
        });
        if (!members.length) throw new Error("race requires members");
        const task = this.newTask();
        scope.tasks.push(task);
        let selected = false;
        let winner: TaskEntry | null = null;
        let value: VmValue = UNIT;
        let failure: Fault | null = null;
        let cancelled = false;
        const losersCancelled = new Set<TaskEntry>();
        const ready = () => {
          if (task.done || task.finishing) return;
          if (!selected) {
            const first = members
              .filter((member) => member.done)
              .sort((a, b) => a.completedAt - b.completedAt)[0];
            if (!first) return;
            selected = true;
            if (!first.fault) {
              winner = first;
              value = first.value;
              first.value = UNIT;
            }
          }
          if (!cancelled) {
            cancelled = true;
            for (const member of members)
              if (!member.done) {
                losersCancelled.add(member);
                member.cancel();
              }
          }
          if (members.some((member) => !member.done)) return;
          task.finishing = true;
          for (const member of members) {
            const index = member.changed.indexOf(ready);
            if (index >= 0) member.changed.splice(index, 1);
          }
          for (const member of [...members].sort(
            (a, b) => a.completedAt - b.completedAt,
          )) {
            if (
              member.fault &&
              !(
                member.fault.kind === "panic" &&
                member.fault.cancelled &&
                losersCancelled.has(member)
              )
            )
              failure = combineFaults(failure, member.fault);
            member.fault = null;
          }
          const losers = members
            .filter((member) => member !== winner)
            .map((member) => {
              const result = member.value;
              member.value = UNIT;
              return result;
            });
          this.closeValueAsync(
            { kind: "vector", elementType: "race result", items: losers },
            this.faultText(failure),
            (closeFault) => {
              failure = combineFaults(failure, closeFault);
              const finish = (releaseFault: Fault | null = null) => {
                failure = combineFaults(failure, releaseFault);
                this.settleTask(task, failure ? UNIT : value, failure);
                for (const changed of [...scope.changed]) changed();
              };
              if (failure && winner)
                this.closeValueAsync(
                  value,
                  this.faultText(failure),
                  finish,
                  act.context.actor,
                  act.rcOwner,
                );
              else finish();
            },
            act.context.actor,
            act.rcOwner,
          );
        };
        task.cancel = () => {
          if (task.done || task.finishing) return;
          failure ??= {
            kind: "panic",
            message: "task cancelled",
            cancelled: true,
          };
          selected = true;
          ready();
        };
        for (const member of members) member.changed.push(ready);
        this.define(act, op.dst, { kind: "task", id: task.id });
        ready();
        return;
      }
      case "task.spawn":
      case "task.invoke": {
        const group = act.scopes.get(op.scope);
        if (!group) throw new Error(`task scope ${op.scope} is missing`);
        const consuming =
          op.op === "task.spawn" ||
          ("receiver" in op.callee && op.callee.receiver.decision === "move");
        const dynamic =
          op.op === "task.invoke" && "dyn" in op.callee ? op.callee : null;
        const dynamicReceiver = dynamic
          ? this.boundary(act, dynamic.receiver)
          : null;
        if (dynamicReceiver && dynamicReceiver.kind !== "dyn")
          throw new TypeError(
            "prepared dynamic receiver is not a trait object",
          );
        const dynamicEntry =
          dynamicReceiver?.kind === "dyn" && dynamic
            ? this.dynamicEntry(dynamicReceiver, dynamic.dyn)
            : null;
        if (dynamic && dynamicEntry?.method !== dynamic.method)
          throw new Error(
            "prepared dynamic slot differs from its checked method",
          );
        const callable =
          op.op === "task.spawn"
            ? this.read(act, op.callable)
            : "direct" in op.callee
              ? { kind: "function" as const, id: String(op.callee.direct) }
              : "mutable" in op.callee
                ? { kind: "function" as const, id: String(op.callee.mutable) }
                : "dyn" in op.callee
                  ? {
                      kind: "function" as const,
                      id: String(dynamicEntry!.callee),
                    }
                  : this.boundary(act, op.callee.receiver);
        if (op.op === "task.spawn") this.invalidate(act, op.callable);
        const args =
          op.op === "task.invoke"
            ? op.args.map((arg) => this.boundary(act, arg))
            : [];
        const specialization =
          op.op === "task.invoke" && "specialization" in op.callee
            ? op.callee.specialization
            : null;
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
          const complete = (closeFault: Fault | null) => {
            if (!fault || (fault.kind === "panic" && fault.cancelled))
              fault = closeFault ?? fault;
            this.settleTask(task, value, fault);
            if (
              !task.grouped &&
              fault &&
              !(fault.kind === "panic" && fault.cancelled)
            ) {
              for (const sibling of group.tasks)
                if (!sibling.done) sibling.cancel();
              if (!group.joining) act.context.cancel?.(fault);
            }
            for (const changed of [...group.changed]) changed();
          };
          if (!consuming) {
            complete(null);
            return;
          }
          this.closeValueAsync(
            callable,
            act.context.actor
              ? this.faultText(act.context.actor.crashing)
              : null,
            complete,
            act.context.actor,
            act.rcOwner,
          );
        };
        const mutable =
          op.op === "task.invoke" && "mutable" in op.callee ? op.callee : null;
        const receiverRef = mutable
          ? this.refOf(act, mutable.receiver.value)
          : null;
        if (dynamicReceiver?.kind === "dyn" && dynamicEntry && dynamic)
          args.unshift(
            this.erasedReceiver(
              act,
              dynamic.receiver,
              dynamicReceiver.value,
              dynamicEntry.receiver,
            ),
          );
        if (receiverRef)
          args.unshift(
            mutable!.staged
              ? cloneValue(readRef(receiverRef))
              : readRef(receiverRef),
          );
        const handback = (
          value: VmValue | null,
          fault: Fault | null,
          result: VmValue,
        ) => {
          if (!receiverRef || !mutable || value === null)
            throw new Error(
              "prepared mutable method omitted its receiver handback",
            );
          if (mutable.staged && fault) {
            this.closeValueAsync(
              value,
              this.faultText(fault),
              (closeFault) => settle(UNIT, combineFaults(fault, closeFault)),
              act.context.actor,
              act.rcOwner,
            );
          } else {
            const previous = readRef(receiverRef);
            writeRef(receiverRef, value);
            if (mutable.staged)
              this.closeValueAsync(
                previous,
                null,
                (closeFault) => {
                  if (closeFault)
                    this.closeValueAsync(
                      result,
                      this.faultText(closeFault),
                      (resultFault) =>
                        settle(UNIT, combineFaults(closeFault, resultFault)),
                      act.context.actor,
                      act.rcOwner,
                    );
                  else settle(result, null);
                },
                act.context.actor,
                act.rcOwner,
              );
            else settle(result, fault);
          }
        };
        const context: FrameContext = {
          id: task.id,
          actor: act.context.actor,
          rcOwner: act.rcOwner,
          requestDeadline: this.requestDeadline(act),
          returned: (value) => {
            if (
              dynamicEntry?.receiver_handback &&
              dynamicReceiver?.kind === "dyn"
            ) {
              if (value.kind !== "record" || value.fields.length !== 2)
                throw new Error(
                  "prepared dynamic method omitted its dual result",
                );
              dynamicReceiver.value = value.fields[1]!;
              settle(value.fields[0]!, null);
            } else if (!mutable) settle(value, null);
            else if (value.kind === "record" && value.fields.length === 2)
              handback(value.fields[1]!, null, value.fields[0]!);
            else
              throw new Error(
                "prepared mutable method returned no dual result",
              );
          },
          failed: (fault, value) => {
            if (
              dynamicEntry?.receiver_handback &&
              dynamicReceiver?.kind === "dyn"
            ) {
              if (!value)
                throw new Error(
                  "prepared dynamic method omitted its fault handback",
                );
              dynamicReceiver.value = value;
            }
            if (mutable) handback(value ?? null, fault, UNIT);
            else settle(UNIT, fault);
          },
        };
        const frame = this.activate(
          this.functionAt(calleeFunction(callable, specialization)),
          callable.kind === "closure" ? [callable.environment, ...args] : args,
          null,
          null,
          undefined,
          null,
          context,
        );
        task.cancel = () => {
          if (task.done || task.finishing) return;
          context.pendingCancellation ??= {
            kind: "panic",
            message: "task cancelled",
            cancelled: true,
          };
          context.cancel?.(context.pendingCancellation);
        };
        this.scheduler.enqueue(task.id, () => {
          if (task.done || task.finishing) return;
          this.runFrame(frame);
        });
        this.trace.allocateId("task", task.id, act.context.id);
        this.define(act, op.dst, { kind: "task", id: task.id });
        return;
      }
      case "actor.ingress_adapter":
        throw new Error("actor ingress adapters are not admitted in the VM");
      default: {
        const exhaustive: never = op;
        throw new Error("unhandled instruction: " + String(exhaustive));
      }
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
        const scrutinee = this.variantAt(
          this.read(act, term.scrutinee),
          term.shape,
        );
        const tag = scrutinee.tag;
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
      case "timestamp.binary": {
        const outcome = timestampBinary(
          this.pkg,
          term,
          this.read(act, term.lhs),
          this.read(act, term.rhs),
        );
        if (outcome === null) {
          const failure = term.failures.find(
            (edge) => edge.trap === "integer_overflow",
          );
          if (!failure)
            throw new Error("timestamp arithmetic lacks its overflow edge");
          this.takeEdge(act, failure.edge);
        } else {
          this.define(act, term.result.value, outcome);
          this.takeEdge(act, term.normal);
        }
        return;
      }
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
        const entry = this.dynamicEntry(receiver, term.slot);
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
        if (entry.receiver_handback) this.current.erasedHandback = receiver;
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
        this.current.handback = term.handback ?? null;
        return;
      }
      case "indirect.call": {
        // A closure's environment reaches its body's first parameter, ahead of
        // the call's own arguments, the way a trait object's value reaches
        // `self`. A plain function value carries none.
        const callee = this.boundary(act, term.callee);
        const args = term.args.map((operand) => this.boundary(act, operand));
        const body = this.functionAt(
          calleeFunction(callee, term.specialization),
        );
        this.current = this.activate(
          body,
          callee.kind === "closure" ? [callee.environment, ...args] : args,
          act,
          term.result,
          term.normal,
          term.unwind,
        );
        // SIR already distinguishes a consuming body from a borrowed body
        // exposed through a once-callable view. Only the latter leaves its
        // receiver cleanup to this invocation boundary.
        if (
          callee.kind === "closure" &&
          term.callee.decision === "move" &&
          body.params[0]?.own !== "owned"
        )
          this.current.consumedCallable = callee;
        return;
      }
      case "actor.call":
        this.actorCall(act, term);
        return;
      case "runtime.call": {
        const entry = this.pkg.runtime_families[term.family];
        if (!entry) throw new Error("runtime family is absent after admission");
        const registration = resolveRuntime(entry);
        if (registration.kind === "ordinary") {
          this.callShim(act, term, registration.shim);
          return;
        }
        if (registration.kind === "refused")
          throw new Error(
            `runtime family ${registration.capability} was refused after admission`,
          );
        switch (registration.handler) {
          case "actor_caller": {
            const caller = act.context.caller ?? { kind: "Task" };
            const payload: VmValue[] = [];
            if (caller.kind === "Actor") {
              const shape = this.pkg.variants[term.result_shape!];
              const tag = runtimeTag(shape!, "CallerActor");
              payload.push({
                kind: "record",
                typeId: shape!.cases[tag]!.fields[0]!,
                fields: [{ kind: "i64", value: caller.identity }],
              });
            }
            this.completeShim(
              act,
              term,
              this.variant(
                term.result_shape,
                caller.kind === "Actor"
                  ? "CallerActor"
                  : caller.kind === "Timer"
                    ? "CallerTimer"
                    : "CallerTask",
                payload,
              ),
            );
            return;
          }
          case "actor_refusal_log": {
            const [message, handler, reason] = term.args.map((argument) =>
              this.boundary(act, argument),
            );
            this.trace.snapshot("actor.delivery_refused", {
              actor_id: act.context.actor?.id ?? null,
              handler: toJsonValue(handler!),
              message: toJsonValue(message!),
              reason: toJsonValue(reason!),
            });
            if (!act.context.reply)
              this.trace.writeStderr(
                `WARN actor.delivery_refused actor=${act.context.actor?.identity ?? 0n} handler=${asString(handler!)} reason=${JSON.stringify(asString(reason!))}\n`,
                null,
              );
            this.completeShim(act, term, UNIT);
            return;
          }
          case "collection": {
            const args = term.args.map((operand) =>
              this.boundary(act, operand),
            );
            const iterator = collectionOperation(
              this.pkg,
              registration,
              args,
              term.callbacks ?? [],
              term.result_member_shapes?.[1] ?? term.result_shape,
            );
            this.runValueProgram(act, term, args, iterator);
            return;
          }
          case "shared": {
            const args = term.args.map((operand) =>
              this.boundary(act, operand),
            );
            this.runValueProgram(
              act,
              term,
              args,
              sharedOperation(
                registration.operation,
                args,
                act.rcOwner,
                term.shared_payload_type ?? "",
                (value) =>
                  this.variant(
                    term.result_shape,
                    value ? "OptionSome" : "OptionNone",
                    value ? [value] : [],
                  ),
              ),
            );
            return;
          }
          case "supervisor_pool": {
            this.supervisorPool(act, term, registration.operation);
            return;
          }
          case "close": {
            const value = this.boundary(act, term.args[0]!);
            this.running = false;
            this.closeValueAsync(
              value,
              this.pipeFault(act),
              (fault) => {
                if (fault) this.raiseFault(act, fault, term.unwind);
                else this.completeShim(act, term, UNIT);
                this.scheduler.enqueue(act.context.id, () =>
                  this.runFrame(act),
                );
              },
              act.context.actor,
              act.rcOwner,
            );
            return;
          }
          case "structural_format": {
            this.structuralFormat(act, term);
            return;
          }
          case "utf8_decode": {
            const value = decodeUtf8Checked(this.boundary(act, term.args[0]!));
            let result: VmValue;
            if ("value" in value)
              result = this.variant(term.result_shape, "ResultOk", [
                stringValue(value.value),
              ]);
            else {
              const shapes = term.variant_result_shapes!;
              const error = this.pkg.aggregates[shapes.error]!;
              result = this.variant(term.result_shape, "ResultErr", [
                {
                  kind: "record",
                  typeId: error.name,
                  shape: error.id,
                  fields: [
                    { kind: "i64", value: BigInt(value.validUpTo) },
                    this.variant(
                      shapes.error_len,
                      value.errorLen === null ? "OptionNone" : "OptionSome",
                      value.errorLen === null
                        ? []
                        : [{ kind: "i64", value: BigInt(value.errorLen) }],
                    ),
                  ],
                },
              ]);
            }
            this.completeShim(act, term, result);
            return;
          }
          default: {
            const exhaustive: never = registration;
            throw new Error(
              `unknown runtime continuation ${String(exhaustive)}`,
            );
          }
        }
      }
      case "extern.call": {
        const entry = this.pkg.externs[term.extern];
        if (!entry)
          throw new Error("external operation is absent after admission");
        const registration = resolveExtern(entry);
        if (registration.kind === "refused")
          throw new Error(
            `external operation ${registration.capability} was refused after admission`,
          );
        if (registration.kind === "directory")
          this.callDirectory(act, term, registration.operation);
        else this.callShim(act, term, registration.shim);
        return;
      }
      case "wire.codec": {
        const args = term.args.map((operand) => this.boundary(act, operand));
        this.runValueProgram(
          act,
          term,
          args,
          executeCodec(
            this.pkg,
            term,
            args[0]!,
            this.options[CODEC_REGISTRATION.bridge],
            this.options.propertyInput ?? [],
          ),
        );
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
        const assertion = term.assertion
          ? {
              operator: renderMessage(this.boundary(act, term.assertion[0])),
              left: renderMessage(this.boundary(act, term.assertion[1])),
              right: renderMessage(this.boundary(act, term.assertion[2])),
            }
          : undefined;
        act.fault = {
          kind: "panic",
          message: renderMessage(message),
          ...(assertion ? { assertion } : {}),
        };
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
        act.context.pollSuppression = (act.context.pollSuppression ?? 0) + 1;
        // The deferred body runs on its own fault state: a body reached while
        // unwinding must not read the fault it is cleaning up after.
        act.parked.push(act.fault);
        act.fault = null;
        this.takeEdge(act, term.body);
        return;
      case "finish_defer": {
        act.context.pollSuppression = (act.context.pollSuppression ?? 0) - 1;
        // Combine the parked fault with whatever the body left, keeping the
        // first: a join must never forget the fault the boundary carried in.
        const parked = act.parked.pop();
        if (parked === undefined) {
          throw new Error(`${act.fn.name}: finish_defer has no active body`);
        }
        act.fault = combineFaults(parked, act.fault);
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
        if (!shape) throw new Error("recover_fault has no exact variant shape");
        const deadline = fault.kind === "panic" && fault.deadline;
        const value: VmValue = {
          kind: "enum",
          typeId: shape.name,
          shape: shape.id,
          tag: deadline ? term.deadline_variant : term.fault_variant,
          payload: [stringValue(faultRecoveryMessage(fault))],
        };
        act.fault = null;
        if (act.context.actor) act.context.actor.crashing = null;
        this.completeShim(act, term, value);
        return;
      }
      case "resume_unwind":
        this.resumeUnwind(
          act,
          term.handback ? this.boundary(act, term.handback) : null,
        );
        return;

      case "suspend":
        this.suspend(act, term);
        return;

      case "unreachable":
        throw new Error(`${act.fn.name}: reached an unreachable endpoint`);
      default: {
        const exhaustive: never = term;
        throw new Error(`unhandled terminator: ${String(exhaustive)}`);
      }
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

  private callDirectory(
    act: Activation,
    term: Extract<TermV1, { op: "extern.call" }>,
    operation: DirectoryOperation,
  ): void {
    const args = term.args.map((operand) => this.boundary(act, operand));
    const result = this.directories.call(operation, args, term.result_resource);
    if (!(result instanceof Promise)) {
      this.completeShim(act, term, result);
      return;
    }
    // Acquisitions are synchronous in the language. Retain borrowed inputs and
    // publish the new owner before the next authored cancellation safepoint.
    this.running = false;
    act.context.parked = true;
    act.context.cancel = (
      fault = { kind: "panic", message: "task cancelled", cancelled: true },
    ) => {
      act.context.pendingCancellation ??= fault;
    };
    this.scheduler.submitHost(
      act.context.id,
      result,
      (outcome) => {
        act.context.parked = false;
        act.context.fileError = outcome.error;
        this.completeShim(act, term, outcome.value);
        this.runFrame(act);
      },
      (error) => {
        throw error;
      },
    );
  }

  /// Run a runtime or extern shim. A fault it raises takes the call's `unwind`
  /// edge so the cleanup the package names runs before the program ends.
  private callShim(
    act: Activation,
    term: Extract<TermV1, { op: "runtime.call" | "extern.call" }>,
    shim: RuntimeShim,
  ): void {
    const args = term.args.map((operand) => this.boundary(act, operand));
    // A call that hands back its updated receiver names its value's shape as
    // the pair's second member.
    const shapeId =
      (term.op === "runtime.call" ? term.result_member_shapes?.[1] : null) ??
      term.result_shape;
    const shape =
      shapeId === null || shapeId === undefined
        ? null
        : (this.pkg.variants[shapeId] ?? null);
    let value: VmValue;
    try {
      const aggregateId = term.aggregate_result_shape;
      const aggregate =
        aggregateId == null ? null : (this.pkg.aggregates[aggregateId] ?? null);
      value = shim(this.host, args, shape, aggregate);
    } catch (error) {
      if (error instanceof ShimFault) {
        this.keepInputs(act, term, args);
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

  /// A runtime call whose contract keeps its inputs on failure fails before
  /// taking them, so each moved argument stays with the caller, whose unwind
  /// edge stores it back.
  private keepsInputs(term: TermV1): boolean {
    return term.op === "runtime.call" && term.keeps_inputs_on_failure === true;
  }

  private keepInputs(
    act: Activation,
    term: Extract<
      TermV1,
      { op: "runtime.call" | "extern.call" | "value.call" | "wire.codec" }
    >,
    args: VmValue[],
  ): void {
    if (!this.keepsInputs(term)) return;
    term.args.forEach((operand, index) => {
      if (operand.decision === "move")
        this.define(act, operand.value, args[index]!);
    });
  }

  private runValueProgram(
    act: Activation,
    term: Extract<TermV1, { op: "runtime.call" | "value.call" | "wire.codec" }>,
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
      const kept = this.keepsInputs(term) ? ["move"] : [];
      const owned = args.filter((_arg, index) => {
        const decision = term.args[index]!.decision;
        return (
          ["move", "copy", "snapshot"].includes(decision) &&
          !kept.includes(decision)
        );
      });
      this.closeValueAsync(
        { kind: "vector", elementType: "", items: [...owned, extra] },
        this.faultText(fault),
        (closeFault) => {
          if (fault.kind === "panic" && fault.cancelled && closeFault)
            fault = closeFault;
          this.keepInputs(act, term, args);
          this.raiseFault(act, fault, term.unwind);
          resume();
        },
        act.context.actor,
        act.rcOwner,
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
      } else if (step.value.kind === "fault") {
        fail({ kind: "panic", message: step.value.message });
      } else if (step.value.kind === "release") {
        const request = step.value;
        this.closeValueAsync(
          request.value,
          this.pipeFault(act),
          (fault) => {
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
          },
          act.context.actor,
          act.rcOwner,
        );
      } else {
        const request = step.value;
        const child = this.invokeFrame(
          act.context.actor,
          request.callee,
          request.args,
          next,
          fail,
          act.rcOwner,
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
    operation: Extract<
      RuntimeContinuation,
      { handler: "supervisor_pool" }
    >["operation"],
  ): void {
    const [view, index] = term.args.map((arg) => this.boundary(act, arg));
    if (view?.kind !== "vector") throw new Error("pool operation has no view");
    if (operation === "Len") {
      this.completeShim(act, term, {
        kind: "i64",
        value: BigInt(view.items.length),
      });
      return;
    }
    if (index?.kind !== "i64") throw new Error("pool operation has no index");
    const value = view.items[Number(index.value)];
    if (operation === "Get") {
      this.completeShim(
        act,
        term,
        this.variant(
          term.result_shape,
          value ? "OptionSome" : "OptionNone",
          value ? [value] : [],
        ),
      );
      return;
    }
    switch (operation) {
      case "Member":
        if (!value) {
          this.raiseFault(
            act,
            { kind: "trap", trap: "vector_bounds" },
            term.unwind,
          );
          return;
        }
        this.completeShim(act, term, value);
        return;
      default: {
        const exhaustive: never = operation;
        throw new Error(
          `unknown supervisor pool operation ${String(exhaustive)}`,
        );
      }
    }
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
        this.completeShim(act, term, stringValue(next));
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
    term: TermV1 & Pick<Extract<TermV1, { op: "call" }>, "result" | "normal">,
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

  private sweepRcOwner(
    owner: RcOwner,
    actor: ActorInstance | null,
    done: (fault: Fault | null) => void,
  ): void {
    let firstFault: Fault | null = null;
    let count = 0;
    const byType = new Map<string, number>();
    const schedule = (work: () => void) =>
      this.scheduler.enqueue(actor?.id ?? "actor:root", work);
    const next = () => {
      const blocks = owner.blocks;
      owner.blocks = new Set();
      if (!blocks.size) {
        owner.closed = true;
        if (actor && count)
          this.trace.snapshot("actor.rc_sweep", {
            actor_id: actor.id,
            rc_sweep: {
              count: String(count),
              by_type: [...byType]
                .sort(([a], [b]) => a.localeCompare(b))
                .map(([type_name, amount]) => ({
                  type_name,
                  count: String(amount),
                })),
            },
          });
        done(firstFault);
        return;
      }
      const marking = blocks.values();
      const payloads = blocks.values();
      let remaining = 64;
      const release = () => {
        if (!remaining--) {
          remaining = 64;
          schedule(release);
          return;
        }
        const item = payloads.next();
        if (item.done) {
          schedule(next);
          return;
        }
        const cell = item.value;
        const payload = cell.value;
        cell.value = UNIT;
        blocks.delete(cell);
        this.closeValueAsync(
          payload,
          this.faultText(firstFault),
          (fault) => {
            firstFault = combineFaults(firstFault, fault);
            release();
          },
          actor,
          owner,
        );
      };
      const mark = () => {
        for (let i = 0; i < 64; i++) {
          const item = marking.next();
          if (item.done) {
            release();
            return;
          }
          const cell = item.value;
          cell.dying = true;
          cell.refs = 0;
          cell.owner = null;
          count += 1;
          byType.set(cell.typeName, (byType.get(cell.typeName) ?? 0) + 1);
        }
        schedule(mark);
      };
      mark();
    };
    next();
  }

  private finishConstruction(act: Activation, done: () => void): boolean {
    if (!act.fn.actor_construction || !act.rcOwner) return false;
    const owner = act.rcOwner;
    act.rcOwner = null;
    this.running = false;
    this.sweepRcOwner(owner, act.context.actor, (fault) => {
      act.fault = combineFaults(act.fault, fault);
      done();
      const current = this.current;
      this.scheduler.enqueue(current.context.id, () => this.runFrame(current));
    });
    return true;
  }

  private returnFrom(act: Activation, value: VmValue): void {
    if (
      this.finishConsumedCallable(act, value, () => this.returnFrom(act, value))
    )
      return;
    if (this.finishConstruction(act, () => this.returnFrom(act, value))) return;
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
    if (act.erasedHandback) {
      if (value.kind !== "record" || value.fields.length !== 2) {
        throw new Error(`${act.fn.name}: invalid erased receiver dual return`);
      }
      act.erasedHandback.value = value.fields[1]!;
      value = value.fields[0]!;
    }
    // The result is defined on the normal edge, like a block parameter, so it
    // is in scope for that edge's arguments.
    if (act.result && act.result !== "never") {
      this.define(caller, act.result.value, value);
    }
    this.current = caller;
    this.takeEdge(caller, act.normal);
  }

  private dynamicEntry(
    receiver: Extract<VmValue, { kind: "dyn" }>,
    slot: number,
  ) {
    const table =
      this.pkg.vtables[receiver.vtable] ??
      this.pkg.vtables.find((table) => table.id === receiver.vtable);
    const entry = table?.slots.find((entry) => entry.slot === slot);
    if (!entry)
      throw new Error(`vtable ${receiver.vtable} has no slot ${slot}`);
    return entry;
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

  private resumeUnwind(act: Activation, handback: VmValue | null): void {
    if (
      this.finishConsumedCallable(act, null, () =>
        this.resumeUnwind(act, handback),
      )
    )
      return;
    if (this.finishConstruction(act, () => this.resumeUnwind(act, handback)))
      return;
    const fault = act.fault;
    if (!fault) {
      throw new Error(`${act.fn.name}: resume_unwind with no pending fault`);
    }
    const caller = act.caller;
    if (!caller) {
      this.running = false;
      act.context.failed(fault, handback);
      return;
    }
    if (!act.unwind) {
      throw new Error(
        `${act.fn.name}: unwound into a call that names no unwind edge`,
      );
    }
    // A failing `var self` method hands its receiver back, defined on the
    // unwind edge like a result on the normal edge.
    if ((handback === null) !== !(act.handback || act.erasedHandback)) {
      throw new Error(
        `${act.fn.name}: receiver handback disagrees with its call`,
      );
    }
    if (handback !== null && act.handback) {
      this.define(caller, act.handback.value, handback);
    }
    if (handback !== null && act.erasedHandback) {
      act.erasedHandback.value = handback;
    }
    caller.fault = fault;
    this.current = caller;
    this.takeEdge(caller, act.unwind);
  }

  private finishConsumedCallable(
    act: Activation,
    value: VmValue | null,
    done: () => void,
  ): boolean {
    const callable = act.consumedCallable;
    if (!callable) return false;
    act.consumedCallable = undefined;
    this.running = false;
    this.closeValueAsync(
      callable,
      this.faultText(act.fault),
      (fault) => {
        act.fault = combineFaults(act.fault, fault);
        const resume = () => {
          if (value !== null && act.fault) this.resumeUnwind(act, null);
          else done();
          const current = this.current;
          this.scheduler.enqueue(current.context.id, () =>
            this.runFrame(current),
          );
        };
        // A successful body result is not observable until receiver cleanup
        // succeeds. A cleanup fault must release that unpublished result.
        if (value !== null && act.fault)
          this.closeValueAsync(
            value,
            this.faultText(act.fault),
            (resultFault) => {
              act.fault = combineFaults(act.fault, resultFault);
              resume();
            },
            act.context.actor,
            act.rcOwner,
          );
        else resume();
      },
      act.context.actor,
      act.rcOwner,
    );
    return true;
  }

  private suspend(
    act: Activation,
    input: Extract<TermV1, { op: "suspend" }>,
  ): void {
    const registration = resolveSuspension(input);
    if (registration.kind === "refused")
      throw new Error(
        `suspension ${registration.capability} was refused after admission`,
      );
    const term = registration.term;
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
                value === null ? "OptionNone" : "OptionSome",
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
      case "StreamSend": {
        const sink = this.boundary(act, term.inputs[0]!);
        const value = this.boundary(act, term.inputs[1]!);
        const wake = this.park(act, term);
        const cancel = act.context.cancel;
        const stop = this.sendStream(
          sink,
          value,
          term.detail.park,
          act.context.actor,
          (status, fault) => {
            wake(UNIT, fault, status);
          },
          act.rcOwner,
        );
        act.context.cancel = (
          fault = { kind: "panic", message: "task cancelled", cancelled: true },
        ) => {
          stop(fault, (closeFault) => cancel?.(closeFault ?? fault));
        };
        return;
      }
      case "StreamNext": {
        const stream = this.boundary(act, term.inputs[0]!);
        const adapter =
          stream.kind === "stream"
            ? this.generatorStreams.get(stream.channelId)
            : undefined;
        if (adapter) {
          this.receiveGeneratorStream(act, term, adapter);
          return;
        }
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
                  value === null ? "OptionNone" : "OptionSome",
                  value === null ? [] : [value],
                ),
              );
          },
        );
        return;
      }
      case "Await": {
        const task = this.taskFor(this.boundary(act, term.inputs[0]!));
        let unwatch = () => {};
        const wake = this.park(act, term, () => unwatch());
        const ready = () => {
          if (!task.done) {
            this.pendingActorWait(
              act,
              [task.actorTarget],
              false,
              "ask",
              (fault) => wake(UNIT, fault),
            );
            return;
          }
          if (task.fault) wake(UNIT, task.fault);
          else {
            const value = task.value;
            task.value = UNIT;
            wake(value);
          }
        };
        task.changed.push(ready);
        unwatch = () => {
          const index = task.changed.indexOf(ready);
          if (index >= 0) task.changed.splice(index, 1);
        };
        ready();
        return;
      }
      case "Join": {
        const group = act.scopes.get(term.detail.scope);
        if (!group) throw new Error("join has no task scope");
        group.joining = true;
        const wake = this.park(act, term);
        const cancel = act.context.cancel!;
        let cancellation: Fault | null = null;
        const mode = term.detail.mode;
        if (mode !== "wait")
          for (const task of group.tasks) if (!task.done) task.cancel();
        const ready = () => {
          if (group.tasks.some((task) => !task.done)) return;
          if (cancellation) {
            cancel(cancellation);
            return;
          }
          const failure =
            group.deadline ??
            group.tasks.find(
              (task) =>
                !task.grouped &&
                task.fault &&
                !(task.fault.kind === "panic" && task.fault.cancelled),
            )?.fault;
          wake(UNIT, mode === "propagate_fault" ? null : (failure ?? null));
        };
        // Cancellation requests descendant cleanup; it cannot bypass the
        // join barrier and let the parent retire their captured resources.
        act.context.cancel = (
          fault = { kind: "panic", message: "task cancelled", cancelled: true },
        ) => {
          cancellation ??= fault;
          for (const task of group.tasks) if (!task.done) task.cancel();
          ready();
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
          } else {
            this.pendingActorWait(
              act,
              sources.map((value) =>
                value.kind === "stream"
                  ? undefined
                  : this.taskFor(value).actorTarget,
              ),
              hasTimeout,
              "select",
              (fault) => wake(UNIT, fault),
            );
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
        let target: ActorInstance | undefined;
        let checkWait: (() => void) | undefined;
        const cancelRequest = this.ask(
          protocol,
          inputs,
          (value, error, closeFault) => {
            act.fault ??= closeFault ?? null;
            return wake(
              this.completion(
                term.result_shape,
                term.error_shape,
                value,
                error,
              ),
            );
          },
          term.request_shapes,
          (actor) => {
            target = actor;
            checkWait?.();
          },
        );
        const cancel = act.context.cancel!;
        act.context.cancel = (fault) =>
          cancelRequest((closeFault) => cancel(closeFault ?? fault));
        checkWait = () =>
          this.pendingActorWait(act, [target], false, "ask", (fault) =>
            cancelRequest((closeFault) => wake(UNIT, closeFault ?? fault)),
          );
        checkWait();
        return;
      }
      case "NativeIo": {
        if (
          term.detail.operation === "FileRead" ||
          term.detail.operation === "FileWrite"
        ) {
          const args = term.inputs.map((operand) =>
            this.boundary(act, operand),
          );
          const wake = this.park(act, term);
          const cancel = act.context.cancel!;
          let stopped: Fault | null =
            act.context.pendingCancellation ?? this.scopeDeadline(act) ?? null;
          act.context.cancel = (
            fault = {
              kind: "panic",
              message: "task cancelled",
              cancelled: true,
            },
          ) => {
            stopped ??= fault;
          };
          this.scheduler.submitHost(
            act.context.id,
            this.directories.io(
              term.detail.operation,
              args,
              () => stopped !== null || this.terminating,
            ),
            (outcome) => {
              // The producer is quiescent. Restore park's latch so cancellation
              // between readiness and frame resumption still retires the result.
              act.context.cancel = cancel;
              act.context.fileError = outcome.error;
              if (stopped) {
                this.closeValue(outcome.value, null);
                cancel(stopped);
              } else wake(outcome.value);
            },
            (error) => {
              throw error;
            },
          );
          return;
        }
        if (term.detail?.operation === "SinkFinish") {
          const wake = this.park(act, term);
          const cancel = act.context.cancel!;
          let stopped: Fault | null = null;
          act.context.cancel = (
            fault = {
              kind: "panic",
              message: "task cancelled",
              cancelled: true,
            },
          ) => {
            stopped ??= fault;
          };
          this.finishSink(this.boundary(act, term.inputs[0]!), null, () => {
            if (stopped) cancel(stopped);
            else wake(UNIT);
          });
          return;
        }
        if (term.detail.operation !== "StdinReadLine") {
          const exhaustive: never = term.detail.operation;
          throw new Error(`unknown native I/O handler ${String(exhaustive)}`);
        }
        // The record is the raw line,
        // so the records concatenate to the stdin the program consumed.
        const line = this.stdin.readLine() ?? new Uint8Array();
        act.context.fileError = null;
        this.trace.recordReplayInput(
          { kind: "stdin", data: new TextDecoder().decode(line) },
          false,
        );
        this.park(act, term)(bytesValue([...line]));
        return;
      }
      case "Safepoint": {
        if (act.context.pollSuppression) {
          this.takeEdge(act, term.resumes[0]!);
          return;
        }
        if (act.context.pendingCancellation && term.cancel) {
          act.fault = act.context.pendingCancellation;
          this.takeEdge(act, term.cancel);
          return;
        }
        const budget = term.detail?.budget;
        if (!Number.isInteger(budget) || budget <= 0)
          throw new Error(
            "safepoint requires a positive compiler-authored budget",
          );
        const countdown = act.context.pollBudget!;
        countdown.remaining = (countdown.remaining || budget) - 1;
        if (countdown.remaining > 0) {
          this.takeEdge(act, term.resumes[0]!);
          return;
        }
        countdown.remaining = budget;
        this.scheduler.advanceSlice();
        const fault =
          act.context.pendingCancellation ?? this.scopeDeadline(act);
        if (fault && term.cancel) {
          act.fault = fault;
          this.takeEdge(act, term.cancel);
        } else if (this.scheduler.hasRunnableWork()) {
          this.park(act, term)(UNIT);
        } else {
          this.takeEdge(act, term.resumes[0]!);
        }
        return;
      }
      case "SemaphoreAcquire": {
        const handle = this.boundary(act, term.inputs[0]!);
        const timeout =
          term.detail.operation === "AcquireTimeout"
            ? this.boundary(act, term.inputs[1]!)
            : null;
        if (timeout !== null && timeout.kind !== "i64")
          throw new Error("semaphore timeout must be an integer");
        let operation: Acquisition | undefined;
        const wake = this.park(
          act,
          term,
          () => {},
          () => operation?.take(),
        );
        operation = this.semaphores.acquire(
          handle,
          timeout?.value ?? null,
          (acquired) => {
            wake(timeout === null ? UNIT : { kind: "bool", value: acquired });
          },
        );
        const cancel = act.context.cancel;
        act.context.cancel = (fault) => {
          operation?.cancel();
          cancel?.(fault);
        };
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
      default: {
        const exhaustive: never = term;
        throw new Error(`unknown suspension handler ${String(exhaustive)}`);
      }
    }
  }

  private pendingActorWait(
    act: Activation,
    targets: Array<ActorInstance | undefined>,
    externalProgress: boolean,
    operation: string,
    fail: (fault: Fault) => void,
  ): void {
    const owner = act.context.actor;
    // Independent tasks carry actor ownership for cleanup, but do not hold its turn.
    if (!owner || owner.active !== act.context || !act.context.parked) return;
    this.actorWaits.ready(owner);
    if (
      externalProgress ||
      this.requestDeadline(act) !== undefined ||
      targets.some((target) => !target)
    )
      return;
    const path = this.actorWaits.pending(owner, targets as ActorInstance[]);
    if (path)
      fail({
        kind: "panic",
        message: `local actor wait cycle at ${operation}: ${path.map((actor) => actor.identity).join(" -> ")}`,
      });
  }

  private park(
    act: Activation,
    term: Extract<TermV1, { op: "suspend" }>,
    cleanup: () => void = () => {},
    accepted: () => void = () => {},
  ): (value: VmValue, fault?: Fault | null, edgeIndex?: number) => boolean {
    this.running = false;
    act.context.parked = true;
    let pending: { value: VmValue; fault: Fault | null; edge: number } | null =
      null;
    const resume = (value: VmValue, fault: Fault | null = null, edge = 0) => {
      if (pending) return false;
      pending = { value, fault, edge };
      act.context.parked = false;
      if (act.context.actor?.active === act.context)
        this.actorWaits.ready(act.context.actor);
      cleanup();
      this.scheduler.enqueue(act.context.id, () => {
        const outcome = pending!;
        act.context.cancel = undefined;
        const finish = (closeFault: Fault | null = null) => {
          const fault =
            closeFault ?? (outcome.fault ? (act.fault ?? outcome.fault) : null);
          if (fault) {
            act.fault = fault;
            this.takeEdge(
              act,
              term.kind === "Safepoint" ||
                (fault.kind === "panic" && fault.cancelled)
                ? term.cancel!
                : term.unwind!,
            );
          } else {
            accepted();
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
            this.faultText(act.fault ?? outcome.fault),
            finish,
            act.context.actor,
            act.rcOwner,
          );
        else finish();
      });
      return true;
    };
    act.context.cancel = (
      fault = { kind: "panic", message: "task cancelled", cancelled: true },
    ) => {
      if (term.cancel == null) return;
      if (pending) pending.fault ??= fault;
      else resume(UNIT, fault);
    };
    return resume;
  }

  private sinkClosed(value: VmValue): boolean {
    const endpoint =
      value.kind === "sink"
        ? this.senderEndpoints.get(value.channelId)
        : undefined;
    return endpoint ? !endpoint.open : this.pipes.isClosed(value);
  }

  private senderEndpoint(owner: SenderOwner, open = true): VmValue {
    const id = this.ids.channel();
    owner.references++;
    if (open) owner.endpoints++;
    this.senderEndpoints.set(id, { owner, open });
    return { kind: "sink", channelId: id };
  }

  private finishSink(
    value: VmValue,
    fault: string | null,
    done: () => void,
  ): void {
    const endpoint =
      value.kind === "sink"
        ? this.senderEndpoints.get(value.channelId)
        : undefined;
    if (!endpoint) {
      this.pipes.close(value, fault);
      done();
      return;
    }
    if (!endpoint.open) {
      done();
      return;
    }
    endpoint.open = false;
    endpoint.owner.endpoints--;
    if (fault || endpoint.owner.endpoints === 0) {
      const finish = () => {
        if (endpoint.owner.busy) {
          endpoint.owner.waiting.add(finish);
          return;
        }
        this.finishSink(endpoint.owner.sink, fault, done);
      };
      finish();
    } else done();
  }

  private sendStream(
    sink: VmValue,
    value: VmValue,
    park: boolean,
    actor: ActorInstance | null,
    complete: (status: number, fault: Fault | null) => void,
    rcOwner: RcOwner | null,
  ): (fault: Fault, drained: (fault: Fault | null) => void) => void {
    const endpoint =
      sink.kind === "sink"
        ? this.senderEndpoints.get(sink.channelId)
        : undefined;
    let owned = true;
    let finished = false;
    let closing = false;
    let claimed = false;
    let stopped: Fault | null = null;
    let encoder: FrameContext | null = null;
    let child: ReturnType<ExecutorV1["sendStream"]> | null = null;
    let withdraw = () => {};
    const drained: Array<(fault: Fault | null) => void> = [];
    const release = () => {
      if (!claimed || !endpoint) return;
      claimed = false;
      endpoint.owner.busy = false;
      for (const wake of endpoint.owner.waiting)
        this.scheduler.enqueue(actor?.id ?? "actor:root", wake);
      endpoint.owner.waiting.clear();
    };
    const finish = (status: number, fault: Fault | null = null) => {
      if (finished || closing) return;
      closing = true;
      release();
      const done = (closeFault: Fault | null = null) => {
        finished = true;
        const failure = combineFaults(fault, closeFault);
        if (stopped) for (const wake of drained.splice(0)) wake(failure);
        else complete(status, failure);
      };
      if (owned) {
        owned = false;
        this.closeValueAsync(
          value,
          this.faultText(fault ?? stopped),
          done,
          actor,
          rcOwner,
        );
      } else done();
    };
    const start = () => {
      if (stopped || finished || closing) return;
      if (!endpoint) {
        withdraw = this.pipes.send(sink, value, park, (status) => {
          if (status === 0) owned = false;
          finish(status);
        });
        return;
      }
      if (!endpoint.open) {
        finish(1);
        return;
      }
      const owner = endpoint.owner;
      if (owner.busy) {
        if (!park) {
          finish(2);
          return;
        }
        owner.waiting.add(start);
        withdraw = () => {
          owner.waiting.delete(start);
        };
        return;
      }
      owner.busy = true;
      claimed = true;
      const callable = owner.encoder;
      encoder = this.invokeFrame(
        actor,
        calleeFunction(callable, null),
        callable.kind === "closure" ? [callable.environment, value] : [value],
        (encoded) => {
          encoder = null;
          if (stopped) {
            this.closeValueAsync(
              encoded,
              this.faultText(stopped),
              (fault) => finish(0, fault),
              actor,
              rcOwner,
            );
          } else {
            child = this.sendStream(
              owner.sink,
              encoded,
              park,
              actor,
              (status, fault) => {
                child = null;
                finish(status, fault);
              },
              rcOwner,
            );
          }
        },
        (fault) => {
          encoder = null;
          finish(3, fault);
        },
        rcOwner,
      );
    };
    start();
    return (fault, done) => {
      if (finished) {
        done(null);
        return;
      }
      stopped ??= fault;
      drained.push(done);
      withdraw();
      if (closing) return;
      if (encoder) encoder.cancel?.(fault);
      else if (child)
        child(fault, (failure) => {
          child = null;
          finish(0, failure);
        });
      else finish(0);
    };
  }

  private receiveGeneratorStream(
    act: Activation,
    term: Extract<TermV1, { op: "suspend"; kind: "StreamNext" }>,
    adapter: GeneratorStream,
  ): void {
    const wake = this.park(act, term);
    const receive = (value: VmValue | null, fault: Fault | null) => {
      if (fault) wake(UNIT, fault);
      else
        wake(
          this.variant(
            term.result_shape,
            value === null ? "OptionNone" : "OptionSome",
            value === null ? [] : [value],
          ),
        );
    };
    if (adapter.pending !== null) {
      const value = adapter.pending;
      adapter.pending = null;
      receive(value, null);
      return;
    }
    const generator = adapter.generator;
    if (generator.done) {
      receive(null, adapter.fault ?? generator.fault);
      return;
    }
    if (term.detail.park) adapter.waiting = receive;
    else receive(null, null);
    const cancel = act.context.cancel;
    if (term.detail.park)
      act.context.cancel = (fault) => {
        adapter.waiting = undefined;
        this.closeGenerator(generator, (closeFault) =>
          cancel?.(closeFault ?? fault),
        );
      };
    if (adapter.receiving) return;
    adapter.receiving = true;
    generator.receive = (value, fault) => {
      adapter.receiving = false;
      if (adapter.closed) {
        if (value !== null) this.closeValueAsync(value, null, () => {});
        return;
      }
      adapter.fault = fault;
      const waiting = adapter.waiting;
      adapter.waiting = undefined;
      if (waiting) waiting(value, fault);
      else adapter.pending = value;
    };
    if (generator.started) {
      const resume = generator.resume;
      generator.resume = undefined;
      if (!resume) throw new Error("generator stream lost its suspended yield");
      resume();
    } else {
      this.scheduler.enqueue(generator.id, () => {
        if (generator.done) return;
        generator.started = true;
        this.runFrame(generator.frame);
      });
    }
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
    this.closeValueAsync(
      value,
      this.pipeFault(act),
      (fault) => {
        completed = true;
        finish?.();
        act.fault = combineFaults(act.fault, fault);
        if (!synchronous)
          this.scheduler.enqueue(act.context.id, () => this.runFrame(act));
      },
      act.context.actor,
      act.rcOwner,
    );
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
      generator.frame.context.actor,
      generator.frame.rcOwner,
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
    rcOwner: RcOwner | null = actor?.rcOwner ?? this.current.rcOwner,
  ): void {
    const pending = [value];
    let firstFault: Fault | null = null;
    const next = (failure: Fault | null = null) => {
      firstFault = combineFaults(firstFault, failure);
      // Authored cleanup can fail after this drain began. Later owned sinks
      // must disclose that failure instead of publishing a clean end.
      const currentFault =
        fault ?? this.faultText(actor?.crashing ?? firstFault);
      while (pending.length) {
        const value = pending.pop()!;
        switch (value.kind) {
          case "encoding":
            value.value = null;
            break;
          case "string":
            value.release();
            break;
          case "rc":
            if (!value.closed) {
              value.closed = true;
              if (value.cell && !value.cell.dying && --value.cell.refs === 0) {
                value.cell.owner?.blocks.delete(value.cell);
                value.cell.owner = null;
                pending.push(value.cell.value);
                value.cell.value = UNIT;
              }
            }
            break;
          case "weak":
            if (!value.closed) {
              value.closed = true;
              value.cell.weak -= 1;
            }
            break;
          case "semaphore":
            this.semaphores.close(value);
            break;
          case "host_resource": {
            if (value.owner.closed) break;
            this.closeHostResource(value);
            break;
          }
          case "timer": {
            if (value.closed) break;
            const resource = this.pkg.resources?.[value.resource];
            if (resource?.kind !== "opaque")
              throw new Error("opaque owner lost its selected close");
            this.invokeFrame(
              actor,
              resource.close,
              [value],
              () => next(),
              next,
              rcOwner,
            );
            return;
          }
          case "task": {
            // Dropping a handle does not cancel scoped work. Join drains the
            // task, and scope close releases any result that was never taken.
            const task = this.tasks.get(value.id);
            if (task?.done) {
              const result = task.value;
              task.value = UNIT;
              this.closeValueAsync(
                result,
                currentFault,
                (fault) => next(fault ?? (task.close ? task.fault : null)),
                actor,
                rcOwner,
              );
              return;
            }
            if (task?.close) {
              task.close(next);
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
          case "dyn":
            pending.push(value.value);
            break;
          case "record": {
            const resource = (this.pkg.resources ?? []).find(
              (resource) =>
                resource.kind === "record" && resource.shape === value.shape,
            );
            if (resource?.kind === "record") {
              if (this.consumedResources.has(value)) break;
              if (!this.closingResources.has(value)) {
                this.invokeFrame(
                  actor,
                  resource.close,
                  [value],
                  () => next(),
                  next,
                  rcOwner,
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
          case "stream": {
            const adapter = this.generatorStreams.get(value.channelId);
            if (adapter) {
              this.generatorStreams.delete(value.channelId);
              adapter.closed = true;
              if (adapter.pending !== null) pending.push(adapter.pending);
              adapter.pending = null;
              adapter.waiting = undefined;
              this.closeGenerator(adapter.generator, next);
              return;
            }
            pending.push(...this.pipes.close(value, currentFault).reverse());
            break;
          }
          case "sink": {
            const endpoint = this.senderEndpoints.get(value.channelId);
            if (endpoint) {
              this.finishSink(value, currentFault, () => {
                this.senderEndpoints.delete(value.channelId);
                if (--endpoint.owner.references === 0)
                  pending.push(endpoint.owner.sink, endpoint.owner.encoder);
                next();
              });
              return;
            } else
              pending.push(...this.pipes.close(value, currentFault).reverse());
            break;
          }
        }
      }
      done(firstFault);
    };
    next();
  }

  private closeHostResource(
    value: Extract<VmValue, { kind: "host_resource" }>,
  ): void {
    if (value.owner.closed) return;
    const resource = this.pkg.resources?.[value.resource];
    const release =
      resource?.kind === "nominal"
        ? resolveExtern({ symbol: resource.release })
        : null;
    if (release?.kind !== "directory" || release.operation.action !== "close")
      throw new Error("filesystem owner lost its selected release");
    this.directories.call(release.operation, [value]);
  }

  private closeValue(value: VmValue, fault: string | null): void {
    if (value.kind === "encoding") value.value = null;
    if (value.kind === "host_resource") this.closeHostResource(value);
    if (value.kind === "string") value.release();
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
      grouped: false,
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
    rcOwner: RcOwner | null = actor?.rcOwner ?? null,
    inline = false,
  ): FrameContext {
    const context: FrameContext = {
      id: actor?.id ?? "actor:root",
      actor,
      rcOwner,
      returned,
      failed: (fault) => failed(fault),
    };
    const frame = this.activate(
      this.functionAt(callable),
      args,
      null,
      null,
      undefined,
      null,
      context,
    );
    let cancelled: Fault | null = null;
    context.cancel = (
      fault = { kind: "panic", message: "task cancelled", cancelled: true },
    ) => {
      cancelled = fault;
    };
    if (inline) {
      this.current = frame;
      this.running = true;
    } else {
      this.scheduler.enqueue(frame.context.id, () => {
        context.cancel = undefined;
        if (cancelled && frame.context === context) context.failed(cancelled);
        else this.runFrame(frame);
      });
    }
    return context;
  }

  private variant(
    shapeId: number | null | undefined,
    role: RuntimeVariantRole,
    payload: VmValue[] = [],
  ): VmValue {
    const shape = shapeId == null ? undefined : this.pkg.variants[shapeId];
    if (!shape)
      throw new Error(`runtime result has no variant shape ${shapeId}`);
    const tag = runtimeTag(shape, role);
    return { kind: "enum", typeId: shape.name, shape: shape.id, tag, payload };
  }

  private variantAt(
    value: VmValue,
    shape: number,
    tag?: number,
  ): Extract<VmValue, { kind: "enum" }> {
    if (
      value.kind !== "enum" ||
      value.shape !== shape ||
      (tag !== undefined && value.tag !== tag)
    ) {
      throw new TypeError(
        `expected variant ${shape}${tag === undefined ? "" : `:${tag}`}`,
      );
    }
    return value;
  }

  private completion(
    result: number | null | undefined,
    errorShape: number | null | undefined,
    value: VmValue | null,
    error: string | null,
  ): VmValue {
    return error
      ? this.variant(result, "ResultErr", [
          this.variant(
            errorShape,
            actorErrorRole(error),
            (error === "Failed" || error === "Rejected") && value
              ? [value]
              : [],
          ),
        ])
      : this.variant(result, "ResultOk", [value ?? UNIT]);
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
    const caller: CallerStamp = act.context.actor
      ? { kind: "Actor", identity: act.context.actor.identity }
      : { kind: "Task" };
    switch (operation.op) {
      case "identity": {
        const [left, right] = args;
        this.completeShim(act, term, {
          kind: "bool",
          value: actorIdentical(left, right),
        });
        return;
      }
      case "spawn": {
        const layout = this.pkg.actors?.find(
          (actor) => actor.id === operation.actor,
        );
        if (!layout)
          throw new Error(`actor layout ${operation.actor} is missing`);
        let supplied = 0;
        const actor: ActorInstance = {
          rcOwner: act.rcOwner ?? { blocks: new Set(), closed: false },
          identity: ++this.actorIdentity,
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
          timers: new Set(),
          disposals: [],
          admission: [],
          busy: true,
          constructing: true,
          alive: true,
          completed: false,
          closing: false,
          terminating: false,
          drainingMailbox: false,
          mailboxDrained: [],
          closingFault: null,
          crashing: null,
          active: null,
          terminalHook: null,
          closed: [],
        };
        this.actors.set(actor.id, actor);
        this.trace.allocateId("actor", actor.id, act.context.id);
        this.trace.snapshot("actor.spawn", { actor_id: actor.id });
        const handle: VmValue = { kind: "actor", id: actor.id };
        act.context.cancel = (
          fault = {
            kind: "panic",
            message: "actor construction cancelled",
            cancelled: true,
          },
        ) => {
          actor.closing = true;
          if (actor.active) {
            actor.active.pendingCancellation ??= fault;
            actor.active.cancel?.(actor.active.pendingCancellation);
          }
        };
        const failed = (fault: Fault) => {
          act.context.cancel = undefined;
          actor.active = null;
          actor.alive = false;
          actor.constructing = false;
          act.rcOwner = null;
          this.cancelActorTimers(actor);
          this.drainMailbox(actor, this.faultText(fault), (payloadFault) => {
            const primary = combineFaults(fault, payloadFault)!;
            this.closeValueAsync(
              actor.state,
              this.faultText(primary),
              (stateFault) => {
                const retained = combineFaults(primary, stateFault)!;
                this.sweepRcOwner(actor.rcOwner, actor, (sweepFault) => {
                  actor.completed = true;
                  for (const closed of actor.closed.splice(0)) closed();
                  this.raiseFault(
                    act,
                    combineFaults(retained, sweepFault)!,
                    term.unwind,
                  );
                  this.scheduler.enqueue(act.context.id, () =>
                    this.runFrame(act),
                  );
                });
              },
              actor,
              actor.rcOwner,
            );
          });
        };
        const started = () => {
          if (actor.closing) {
            failed(
              actor.active?.pendingCancellation ?? {
                kind: "panic",
                message: "actor construction cancelled",
                cancelled: true,
              },
            );
            return;
          }
          actor.constructing = false;
          actor.active = null;
          act.context.cancel = undefined;
          act.rcOwner = null;
          actor.busy = false;
          this.startPeriodicTimers(actor);
          this.dispatchActor(actor);
          this.completeShim(act, term, handle);
          this.current = act;
          this.running = true;
        };
        // Construction enters the same interpreter loop as its caller. Only
        // a real suspension queues work and advances deterministic scheduling.
        const start = () => {
          if (layout.start != null)
            actor.active = this.invokeFrame(
              actor,
              layout.start,
              [actor.state],
              started,
              failed,
              actor.rcOwner,
              true,
            );
          else started();
        };
        if (layout.init != null)
          actor.active = this.invokeFrame(
            actor,
            layout.init,
            [actor.state, ...args.slice(supplied)],
            start,
            failed,
            actor.rcOwner,
            true,
          );
        else start();
        return;
      }
      case "timer_schedule": {
        const [target, after, overdue, hasOverdue, payload] = args;
        const actor = this.actorFor(target!);
        if (
          actor !== act.context.actor ||
          after?.kind !== "i64" ||
          overdue?.kind !== "i64" ||
          hasOverdue?.kind !== "bool" ||
          payload?.kind !== "record"
        )
          throw new Error("timer schedule lost its checked owner or timing");
        const handler = actor.layout.handlers.find(
          (handler) => handler.message_id === operation.message,
        );
        if (!handler) throw new Error("timer schedule lost its handler");
        if (this.rootComplete || actor.closing) {
          this.running = false;
          const refused: Fault = {
            kind: "panic",
            actorSendFailed: true,
            message: "actor timer admission is closed",
          };
          this.closeValueAsync(
            payload,
            faultReport(refused),
            (fault) => {
              this.raiseFault(act, combineFaults(refused, fault)!, term.unwind);
              this.scheduler.enqueue(act.context.id, () => this.runFrame(act));
            },
            actor,
            act.rcOwner,
          );
          return;
        }
        let phase: "scheduled" | "fired" | "started" | "cancelled" | "overdue" =
          "scheduled";
        let pending: VmValue | null = payload;
        let overdueAt: number | null = null;
        let disarm: (() => void) | null = null;
        const forget = () => {
          disarm?.();
          disarm = null;
          actor.timers.delete(timer);
        };
        const miss = (discarded: boolean) => {
          if (phase !== "fired") return;
          phase = hasOverdue.value ? "overdue" : "cancelled";
          forget();
          if (discarded && !hasOverdue.value)
            this.trace.writeStderr(
              "actor.timer_discarded: the one-shot handler did not start\n",
              null,
            );
          if (!hasOverdue.value || !actor.alive) return;
          const fault: Fault = {
            kind: "panic",
            timerOverdue: true,
            message: "actor timer did not start before its overdue deadline",
          };
          if (actor.active) {
            actor.active.pendingCancellation ??= fault;
            actor.active.cancel?.(actor.active.pendingCancellation);
          } else this.crashActor(actor, fault, handler);
        };
        const timer: OneShotTimer = {
          cancel: () => {
            if (phase !== "overdue") phase = "cancelled";
            forget();
            if (pending) {
              actor.disposals.push(pending);
              pending = null;
              this.dispatchActor(actor);
            }
          },
          started: () => {
            if (
              phase === "fired" &&
              overdueAt !== null &&
              this.trace.virtualTimeMs >= overdueAt
            )
              miss(false);
            const allowed = phase !== "overdue";
            if (phase === "fired") phase = "started";
            forget();
            return allowed;
          },
          discarded: () => miss(true),
        };
        actor.timers.add(timer);
        disarm = this.scheduler.after(after.value, () => {
          disarm = null;
          if (phase !== "scheduled") return;
          phase = "fired";
          pending = null;
          if (hasOverdue.value) {
            overdueAt =
              this.trace.virtualTimeMs + Number(overdue.value) / 1_000_000;
            disarm = this.scheduler.after(overdue.value, () => miss(false));
          }
          const message: ActorMessage = {
            timer,
            caller: { kind: "Timer" },
            handler,
            payload: payload.fields,
            reply: false,
            obligated: false,
            complete: (_value, _error, drained) => drained?.(null),
          };
          const status = this.admitActorMessage(actor, message);
          if (status === "full" || status === "closed") {
            actor.disposals.push(payload);
            timer.discarded();
            this.dispatchActor(actor);
          }
        });
        this.completeShim(act, term, {
          kind: "timer",
          resource: operation.resource,
          cancel: () => timer.cancel(),
          closed: false,
        });
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
              actor.layout.mailbox_capacity != null &&
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
              caller,
              handler,
              payload: fields,
              reply: false,
              obligated: true,
              complete: (_value, error, drained) => {
                if (error) this.closeValue(payload!, error);
                drained?.(null);
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
        const cancelRequest = this.ask(
          operation.protocol,
          args,
          (value, error, closeFault) => {
            task.actorError = error;
            this.settleTask(task, value ?? UNIT, closeFault ?? null);
            return true;
          },
          undefined,
          (actor) => {
            task.actorTarget = actor;
            for (const changed of [...task.changed]) changed();
          },
        );
        task.close = (done) =>
          cancelRequest((fault) => {
            this.settleTask(task, UNIT, fault);
            done(fault);
          });
        this.completeShim(act, term, { kind: "task", id: task.id });
        return;
      }
      case "call_take": {
        const task = this.taskFor(args[0]!);
        if (!task.done)
          throw new Error("actor completion taken before readiness");
        act.fault ??= task.fault;
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
          this.variant(term.result_shape, "ResultOk", [
            this.variant(
              shapes.success,
              discarded ? "DeliveryDiscarded" : "DeliveryAccepted",
            ),
          ]);
        const rejected = (reason: string) =>
          this.variant(term.result_shape, "ResultErr", [
            this.variant(term.error_shape, "ActorErrorRejected", [
              {
                kind: "record",
                typeId: this.aggregateName(shapes.failure!),
                shape: shapes.failure!,
                fields: [
                  this.variant(shapes.reason, sendErrorRole(reason)),
                  request,
                ],
              },
            ]),
          ]);
        let parked = false;
        const finish = (value: VmValue) => {
          this.completeShim(act, term, value);
          if (parked)
            this.scheduler.enqueue(act.context.id, () => this.runFrame(act));
        };
        const submit = () => {
          if (!actor.alive || actor.closing) {
            finish(rejected("Dead"));
            return;
          }
          const message: ActorMessage = {
            caller,
            handler,
            payload: payload.fields,
            reply: false,
            obligated: false,
            complete: (_value, _error, drained) => drained?.(null),
          };
          const status = this.admitActorMessage(actor, message);
          if (status === "full") {
            if (operation.policy === "drop_newest") {
              let settled = false;
              let waiting = false;
              this.closeValueAsync(
                payload,
                this.pipeFault(act),
                (fault) => {
                  act.fault ??= fault;
                  settled = true;
                  if (waiting) finish(accepted(true));
                  else this.completeShim(act, term, accepted(true));
                },
                act.context.actor,
                act.rcOwner,
              );
              if (!settled) {
                waiting = true;
                parked = true;
                this.running = false;
              }
              return;
            }
            if (operation.policy === "reject") {
              finish(rejected("Full"));
              return;
            }
            if (operation.policy !== "wait")
              throw new Error("submission has no admitted policy");
            parked = true;
            this.running = false;
            actor.admission.push(submit);
            return;
          }
          if (status === "closed") finish(rejected("Dead"));
          else finish(accepted(status === "discarded"));
        };
        submit();
        return;
      }
      case "supervisor_spawn": {
        const layout = this.pkg.supervisors?.find(
          (layout) => layout.id === operation.supervisor,
        );
        if (!layout) throw new Error("supervisor descriptor is missing");
        const isPool = layout.children.some((child) => child.is_pool);
        const count = isPool ? args[args.length - 1] : undefined;
        if (isPool && count?.kind !== "i64")
          throw new Error("pool construction has no count");
        const configured = isPool ? args.slice(0, -1) : args;
        const initial = layout.backoff
          ? configured[configured.length - 2]
          : undefined;
        const maximum = layout.backoff
          ? configured[configured.length - 1]
          : undefined;
        const backoff =
          initial?.kind === "i64" && maximum?.kind === "i64"
            ? { initial: initial.value, maximum: maximum.value }
            : null;
        const validBackoff =
          !layout.backoff ||
          (backoff !== null &&
            backoff.initial > 0n &&
            backoff.maximum >= backoff.initial);
        const config = layout.backoff ? configured.slice(0, -2) : configured;
        if (
          !validBackoff ||
          (count?.kind === "i64" &&
            (count.value <= 0n || count.value > 2147483647n))
        ) {
          const fault: Fault = {
            kind: "panic",
            message: !validBackoff
              ? "supervisor backoff requires positive initial and maximum >= initial"
              : "supervisor pool count must be between 1 and 2147483647",
          };
          this.running = false;
          this.closeValueAsync(
            { kind: "record", typeId: "", fields: config },
            fault.message,
            (closeFault) => {
              this.raiseFault(
                act,
                combineFaults(fault, closeFault)!,
                term.unwind,
              );
              this.scheduler.enqueue(act.context.id, () => this.runFrame(act));
            },
            act.context.actor,
            act.rcOwner,
          );
          return;
        }
        const members = count?.kind === "i64" ? Number(count.value) : 1;
        const owner: SupervisorInstance = {
          id: this.ids.supervisor(),
          layout,
          config,
          children: layout.children.flatMap(() =>
            Array.from({ length: members }, () => null),
          ),
          specs: layout.children.flatMap((child) =>
            Array.from({ length: members }, () => child),
          ),
          offsets: layout.children.map((_child, index) => index),
          recovering: new Set(),
          restartBusy: true,
          constructor: null,
          pendingRestarts: [],
          spent: new Set(),
          completed: false,
          alive: true,
          terminating: false,
          teardownChild: null,
          teardownCancelTimer: null,
          restartTimes: [],
          backoff,
          restartDelays: new Map(),
          restartTimers: new Map(),
          closed: [],
        };
        this.supervisors.set(owner.id, owner);
        this.trace.allocateId("supervisor", owner.id, act.context.id);
        this.trace.snapshot("supervisor.spawn", { supervisor_id: owner.id });
        this.running = false;
        const order = this.supervisorStartupOrder(owner);
        let fault: Fault | null = null;
        const fail = (cause: Fault) => {
          if (fault) return;
          fault = cause;
          owner.closed.push(() => {
            act.context.cancel = undefined;
            this.raiseFault(act, fault!, term.unwind);
            this.scheduler.enqueue(act.context.id, () => this.runFrame(act));
          });
          if (owner.constructor)
            owner.constructor.pendingCancellation ??= cause;
          this.stopSupervisor(owner);
        };
        act.context.cancel = (
          cause = {
            kind: "panic",
            message: "construction cancelled",
            cancelled: true,
          },
        ) => fail(cause);
        const next = () => {
          const child = order.shift();
          if (child !== undefined) {
            this.startChild(owner, child, next, fail);
          } else {
            act.context.cancel = undefined;
            owner.restartBusy = false;
            this.completeShim(act, term, { kind: "supervisor", id: owner.id });
            this.scheduler.enqueue(act.context.id, () => this.runFrame(act));
            const pending = owner.pendingRestarts.shift();
            if (pending) this.restartChild(owner, pending.child, pending.debts);
          }
        };
        next();
        return;
      }
      case "supervisor_pool_view":
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
            items: Array.from({ length: owner.children.length }, (_, index) =>
              makeRole(offset + index),
            ),
          });
          return;
        }
        this.completeShim(act, term, makeRole(offset));
        return;
      }
      case "await_restarted": {
        const target = args[0]!;
        const role = "id" in target ? this.roles.get(target.id) : undefined;
        if (!role) {
          if (target.kind !== "actor" && target.kind !== "supervisor")
            throw new Error("restarted requires an actor or supervisor handle");
          this.completeShim(act, term, UNIT);
          return;
        }
        if (this.roleRestartPending(role)) {
          this.running = false;
          const settled = () => {
            if (this.roleRestartPending(role)) {
              role.waiting.push(settled);
              return;
            }
            this.completeShim(act, term, UNIT);
            this.scheduler.enqueue(act.context.id, () => this.runFrame(act));
          };
          role.waiting.push(settled);
        } else this.completeShim(act, term, UNIT);
        return;
      }
      case "stop": {
        const target = args[0]!;
        if (target.kind === "actor")
          this.requestActorStop(this.actorFor(target));
        else if (target.kind === "supervisor")
          this.stopSupervisor(this.supervisorFor(target));
        else throw new Error("stop requires an actor or supervisor handle");
        this.completeShim(act, term, UNIT);
        return;
      }
      case "terminate": {
        const target = args[0]!;
        if (target.kind === "actor")
          this.requestActorTerminate(this.actorFor(target));
        else if (target.kind === "supervisor")
          this.stopSupervisor(this.supervisorFor(target), true);
        else
          throw new Error("terminate requires an actor or supervisor handle");
        this.completeShim(act, term, UNIT);
        return;
      }
      case "await_stopped": {
        const target = args[0]!;
        if (target.kind === "actor") {
          const actor = this.actorFor(target);
          const complete = () => {
            // A crash was reported when it happened and already counts toward
            // the exit status; only a fault raised while the actor was being
            // stopped passes to the caller that waits for it.
            const fault = actor.closingFault;
            if (actor.closeFaultDebt !== undefined)
              this.faultDebts.delete(actor.closeFaultDebt);
            if (fault) this.raiseFault(act, fault, term.unwind);
            else this.completeShim(act, term, UNIT);
          };
          if (actor.completed) complete();
          else {
            this.running = false;
            actor.closed.push(() => {
              complete();
              this.scheduler.enqueue(act.context.id, () => this.runFrame(act));
            });
          }
        } else if (target.kind === "supervisor") {
          const owner = this.supervisorFor(target);
          if (owner.completed) this.completeShim(act, term, UNIT);
          else {
            this.running = false;
            owner.closed.push(() => {
              this.completeShim(act, term, UNIT);
              this.scheduler.enqueue(act.context.id, () => this.runFrame(act));
            });
          }
        } else
          throw new Error("stopped requires an actor or supervisor handle");
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
      case "local_observation":
        throw new Error("actor observations are not admitted in the VM");
      default: {
        const exhaustive: never = operation;
        throw new Error("unhandled actor operation: " + String(exhaustive));
      }
    }
  }

  private scopeDeadline(act: Activation, expected?: Fault): Fault | undefined {
    for (
      let frame: Activation | null = act;
      frame?.context === act.context;
      frame = frame.caller
    ) {
      for (const group of frame.scopes.values())
        if (group.deadline && (!expected || group.deadline === expected))
          return group.deadline;
    }
    return undefined;
  }

  private requestDeadline(act: Activation): number | undefined {
    let deadline = act.context.requestDeadline;
    for (let frame: Activation | null = act; frame; frame = frame.caller) {
      for (const group of frame.scopes.values()) {
        if (group.expiresAt !== undefined)
          deadline = Math.min(deadline ?? Infinity, group.expiresAt);
      }
    }
    return deadline;
  }

  private ask(
    protocol: ActorProtocol,
    inputs: VmValue[],
    complete: (
      value: VmValue | null,
      error: string | null,
      closeFault?: Fault | null,
    ) => boolean,
    shapes?: RequestShapes | null,
    selectedTarget?: (actor: ActorInstance) => void,
  ): (done: (fault: Fault | null) => void) => void {
    let settled = false;
    let admitted = false;
    let closing = false;
    const owner = this.current.context.actor;
    const rcOwner = this.current.rcOwner;
    const deadline = this.requestDeadline(this.current);
    let cancelTimer: (() => void) | undefined;
    let message: ActorMessage | undefined;
    const caller: CallerStamp = owner
      ? { kind: "Actor", identity: owner.identity }
      : { kind: "Task" };
    let receiver: ActorInstance | null = null;
    const finish: ActorMessage["complete"] = (value, error, drained) => {
      const alreadySettled = settled;
      settled = true;
      cancelTimer?.();
      if (alreadySettled || complete(value, error) === false) {
        // The finishing receiver still owns a reply that its caller no
        // longer accepts. Its next turn must wait for this release.
        this.closeValueAsync(
          value ?? UNIT,
          null,
          (fault) => {
            if (drained) drained(fault);
            else if (fault && owner) this.crashActor(owner, fault);
            else if (fault) this.rootFault ??= fault;
          },
          admitted ? receiver : owner,
          admitted ? (receiver?.rcOwner ?? null) : rcOwner,
        );
      } else drained?.(null);
    };
    const target = inputs[0]!;
    const payload = protocol.sealed ? fieldsOf(inputs[1]!) : inputs.slice(1);
    let closed = false;
    let closeFault: Fault | null = null;
    const closeWaiters: Array<(fault: Fault | null) => void> = [];
    const closeRequest = (done: (fault: Fault | null) => void) => {
      if (closed) {
        done(closeFault);
        return;
      }
      closeWaiters.push(done);
      if (closing) return;
      closing = true;
      this.closeValueAsync(
        { kind: "record", typeId: "", fields: payload },
        null,
        (fault) => {
          closed = true;
          closeFault = fault;
          for (const waiter of closeWaiters.splice(0)) waiter(fault);
        },
        owner,
        rcOwner,
      );
    };
    const reject = (reason: string) => {
      if (settled || closing) return;
      closeRequest((fault) => {
        if (settled) return;
        settled = true;
        cancelTimer?.();
        complete(null, reason, fault);
      });
    };
    const attempt = () => {
      if (settled || closing) return;
      if (deadline !== undefined && deadline <= this.trace.virtualTimeMs) {
        reject("TimedOut");
        return;
      }
      const role = "id" in target ? this.roles.get(target.id) : undefined;
      if (role && this.rolePending(role)) {
        role.waiting.push(attempt);
        return;
      }
      const actor = this.actorFor(target);
      if (!actor.alive || actor.closing) {
        reject("Dead");
        return;
      }
      const handler = actor.layout.handlers.find(
        (handler) => handler.message_id === protocol.message,
      );
      if (!handler)
        throw new Error(`actor ${actor.id} has no message ${protocol.message}`);
      selectedTarget?.(actor);
      if (settled || closing) return;
      const full =
        actor.layout.mailbox_capacity != null &&
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
            shape,
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
              this.variant(shapes.reason, "SendErrorFull"),
              message,
            ]),
            "Rejected",
          );
        } else {
          actor.admission.push(attempt);
        }
        return;
      }
      admitted = true;
      receiver = actor;
      message = {
        caller,
        handler,
        payload,
        complete: finish,
        reply: true,
        obligated: true,
        requestDeadline: deadline,
      };
      actor.mailbox.push(message);
      this.trace.snapshot("actor.ask", {
        actor_id: actor.id,
        handler: handler.name,
      });
      this.dispatchActor(actor);
    };
    const withdrawQueued = () => {
      if (!message || !receiver) return;
      const index = receiver.mailbox.indexOf(message);
      if (index < 0) return;
      receiver.mailbox.splice(index, 1);
      admitted = false;
      for (const admit of receiver.admission.splice(0)) admit();
    };
    if (deadline !== undefined) {
      cancelTimer = this.scheduler.after(
        BigInt(
          Math.max(
            0,
            Math.round((deadline - this.trace.virtualTimeMs) * 1_000_000),
          ),
        ),
        () => {
          if (settled || closing) return;
          withdrawQueued();
          if (admitted) finish(null, "TimedOut");
          else reject("TimedOut");
        },
      );
    }
    attempt();
    return (done) => {
      const wasSettled = settled;
      settled = true;
      cancelTimer?.();
      withdrawQueued();
      if (admitted || (wasSettled && !closing)) done(null);
      else closeRequest(done);
    };
  }

  private admitActorMessage(
    actor: ActorInstance,
    message: ActorMessage,
  ): "accepted" | "discarded" | "full" | "closed" {
    if (!actor.alive || actor.closing) return "closed";
    const full =
      actor.layout.mailbox_capacity != null &&
      actor.mailbox.length >= actor.layout.mailbox_capacity;
    let declared = full ? actor.layout.overflow : null;
    const discard = (lost: ActorMessage) => {
      actor.disposals.push({
        kind: "record",
        typeId: "",
        fields: lost.payload,
      });
      lost.timer?.discarded();
      this.dispatchActor(actor);
    };
    const coalesce = actor.layout.coalesce;
    if (declared === "coalesce" && coalesce) {
      const key = coalesce.keys.find(
        (key) => key.message === message.handler.message_id,
      );
      const queued = key
        ? actor.mailbox.findIndex(
            (queued) =>
              !queued.obligated &&
              queued.handler.message_id === message.handler.message_id &&
              equals(queued.payload[key.param]!, message.payload[key.param]!),
          )
        : -1;
      if (queued >= 0) {
        const superseded = actor.mailbox[queued]!;
        actor.mailbox[queued] = message;
        discard(superseded);
        return "accepted";
      }
      declared = coalesce.fallback;
    }
    if (declared === "drop_new") {
      discard(message);
      return "discarded";
    }
    const oldest =
      declared === "drop_old"
        ? actor.mailbox.findIndex((queued) => !queued.obligated)
        : -1;
    if (oldest >= 0) {
      const [evicted] = actor.mailbox.splice(oldest, 1);
      actor.mailbox.push(message);
      discard(evicted!);
      return "accepted";
    }
    if (full) return "full";
    actor.mailbox.push(message);
    this.trace.snapshot("actor.send", {
      actor_id: actor.id,
      handler: message.handler.name,
    });
    this.dispatchActor(actor);
    return "accepted";
  }

  private completeRoot(): void {
    this.rootComplete = true;
    // Quiesce actor timers before draining accepted work; sleeps still wake.
    for (const actor of this.actors.values()) this.cancelActorTimers(actor);
  }

  private startPeriodicTimers(actor: ActorInstance): void {
    if (this.rootComplete || actor.closing) return;
    for (const handler of actor.layout.handlers) {
      if (handler.every_ns == null) continue;
      const interval = Math.floor(handler.every_ns / 1_000_000);
      let next = this.trace.virtualTimeMs + interval;
      let disarm: (() => void) | null = null;
      const timer: ActorTimer = {
        cancel: () => {
          disarm?.();
          disarm = null;
          actor.timers.delete(timer);
        },
      };
      const fire = () => {
        disarm = null;
        if (!actor.timers.has(timer)) return;
        this.admitActorMessage(actor, {
          caller: { kind: "Timer" },
          handler,
          payload: [],
          reply: false,
          obligated: false,
          complete: (_value, _error, drained) => drained?.(null),
        });
        if (!actor.timers.has(timer)) return;
        next += interval;
        if (next <= this.trace.virtualTimeMs)
          next +=
            (Math.floor((this.trace.virtualTimeMs - next) / interval) + 1) *
            interval;
        arm();
      };
      const arm = () => {
        disarm = this.scheduler.after(
          BigInt(Math.round((next - this.trace.virtualTimeMs) * 1_000_000)),
          fire,
        );
      };
      actor.timers.add(timer);
      arm();
    }
  }

  private cancelActorTimers(actor: ActorInstance): void {
    for (const timer of [...actor.timers]) timer.cancel();
  }

  private bindActorStream(context: FrameContext, sink: VmValue): () => void {
    let unwatch = () => {};
    let active = true;
    const detach = () => {
      active = false;
      unwatch();
    };
    const closed = () => {
      if (!active || !this.pipes.peerClosed(sink)) return;
      // Bind the reader to this exact producer turn, including while it waits
      // on another operation. Later reader closes must not cancel a new turn.
      detach();
      context.pendingCancellation ??= {
        kind: "panic",
        message: "actor stream reader closed",
        cancelled: true,
      };
      context.cancel?.(context.pendingCancellation);
    };
    unwatch = this.pipes.observe(sink, closed);
    closed();
    return detach;
  }

  private dispatchActor(actor: ActorInstance): void {
    if (actor.busy || !actor.alive || actor.drainingMailbox) return;
    if (actor.disposals.length > 0) {
      actor.busy = true;
      const values = actor.disposals.splice(0);
      this.closeValueAsync(
        { kind: "vector", elementType: "message", items: values },
        this.faultText(actor.crashing),
        (fault) => {
          actor.busy = false;
          if (fault) this.crashActor(actor, fault);
          else this.dispatchActor(actor);
        },
        actor,
      );
      return;
    }
    if (actor.terminating && actor.mailbox.length > 0) {
      this.drainMailbox(actor, this.faultText(actor.closingFault), (fault) => {
        actor.closingFault = combineFaults(actor.closingFault, fault);
        this.dispatchActor(actor);
      });
      return;
    }
    if (actor.mailbox.length === 0) {
      if (actor.closing) {
        this.stopActor(actor);
      }
      return;
    }
    actor.busy = true;
    this.scheduler.enqueue(actor.id, () => {
      if (actor.terminating) {
        actor.busy = false;
        this.dispatchActor(actor);
        return;
      }
      const message = actor.mailbox[0];
      if (!message || !actor.alive) {
        actor.busy = false;
        if (actor.closing && actor.alive) this.dispatchActor(actor);
        return;
      }
      // An overdue claim can synchronously begin terminal cleanup. Keep the
      // payload in its mailbox until that claim succeeds.
      if (message.timer && !message.timer.started()) {
        actor.busy = false;
        this.dispatchActor(actor);
        return;
      }
      actor.mailbox.shift();
      for (const admit of actor.admission.splice(0)) admit();
      if (
        message.requestDeadline !== undefined &&
        message.requestDeadline <= this.trace.virtualTimeMs
      ) {
        this.closeValueAsync(
          { kind: "record", typeId: "", fields: message.payload },
          null,
          (fault) => {
            message.complete(null, "TimedOut");
            actor.busy = false;
            if (fault) this.crashActor(actor, fault);
            else this.dispatchActor(actor);
          },
          actor,
        );
        return;
      }
      this.trace.snapshot("actor.receive", {
        actor_id: actor.id,
        handler: message.handler.name,
      });
      let unwatchStream = () => {};
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
          caller: message.caller,
          requestDeadline: message.requestDeadline,
          reply: message.reply,
          returned: (value) => {
            unwatchStream();
            actor.active = null;
            const drained = (fault: Fault | null) => {
              actor.busy = false;
              if (fault) this.crashActor(actor, fault, message.handler);
              else this.dispatchActor(actor);
            };
            this.trace.snapshot("actor.reply", {
              actor_id: actor.id,
              value: toJsonValue(value),
            });
            if (message.handler.fallible && value.kind === "enum") {
              const shape = this.pkg.variants[message.handler.result_shape!];
              if (shape?.cases[value.tag]?.name === "Err") {
                if (message.reply)
                  message.complete(value.payload[0] ?? UNIT, "Failed", drained);
                else {
                  this.crashActor(
                    actor,
                    {
                      kind: "panic",
                      handlerFailed: true,
                      message: "unhandled declared handler failure",
                    },
                    message.handler,
                  );
                  return;
                }
              } else {
                message.complete(value.payload[0] ?? UNIT, null, drained);
              }
            } else {
              message.complete(value, null, drained);
            }
          },
          failed: (fault) => {
            unwatchStream();
            actor.active = null;
            actor.busy = false;
            if (
              fault.kind === "trap" &&
              fault.trap === "actor_refused" &&
              !fault.secondary?.length
            ) {
              actor.crashing = null;
              message.complete(null, "Refused");
              this.dispatchActor(actor);
            } else if (fault.kind === "panic" && fault.cancelled) {
              message.complete(null, "Dead");
              this.dispatchActor(actor);
            } else {
              this.crashActor(actor, fault, message.handler);
              message.complete(null, "Trapped");
            }
          },
        },
      );
      actor.active = frame.context;
      if (message.handler.streams) {
        // StreamStart's checked producer protocol appends its output sink.
        const sink = message.payload.at(-1);
        if (sink?.kind !== "sink")
          throw new Error("actor stream producer has no final sink");
        unwatchStream = this.bindActorStream(frame.context, sink);
      }
      this.runFrame(frame);
    });
  }

  private crashActor(
    actor: ActorInstance,
    fault: Fault,
    handler?: ActorShape["handlers"][number],
  ): void {
    this.reportFault(fault);
    if (handler) {
      const context = `${actor.layout.name}.${handler.name}`;
      this.trace.writeStderr(
        `hew: actor crash in ${context} (actor ${actor.identity}): ${faultInfo(fault)[1]}\n`,
        null,
      );
    }
    const debt = ++this.nextFaultDebt;
    this.faultDebts.add(debt);
    if (actor.supervisor)
      actor.supervisor.owner.recovering.add(actor.supervisor.child);
    actor.crashing ??= fault;
    actor.alive = false;
    actor.busy = false;
    this.cancelActorTimers(actor);
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
        (stateFault) => {
          actor.closingFault = combineFaults(actor.closingFault, stateFault);
          fault = combineFaults(fault, stateFault)!;
          actor.crashing = fault;
          this.reportFault(fault);
          this.drainMailbox(actor, this.faultText(fault), (payloadFault) => {
            actor.closingFault = combineFaults(
              actor.closingFault,
              payloadFault,
            );
            fault = combineFaults(fault, payloadFault)!;
            actor.crashing = fault;
            this.reportFault(fault);
            this.sweepRcOwner(actor.rcOwner, actor, (sweepFault) => {
              actor.closingFault = combineFaults(
                actor.closingFault,
                sweepFault,
              );
              actor.crashing = combineFaults(actor.crashing, sweepFault);
              this.reportFault(actor.crashing!);
              if (actor.closingFault) {
                this.reportFault(actor.closingFault);
              }
              actor.completed = true;
              for (const wake of actor.closed.splice(0)) wake();
              for (const admit of actor.admission.splice(0)) admit();
              if (actor.supervisor) {
                const { owner, child } = actor.supervisor;
                if (action === "Restart")
                  this.restartChild(owner, child, [debt]);
                else if (action === "Escalate")
                  this.escalateSupervisor(owner, [debt]);
                else {
                  owner.spent.add(child);
                  owner.recovering.delete(child);
                  this.wakeRoles();
                }
              }
            });
          });
        },
        actor,
      );
    };
    const runHook = () => {
      if (actor.layout.crash != null) {
        const [code, reason] = faultInfo(fault);
        const message = faultApplicationMessage(fault) ?? reason;
        const info: VmValue = {
          kind: "record",
          typeId: this.aggregateName(actor.layout.crash_info!),
          shape: actor.layout.crash_info!,
          fields: [
            {
              kind: "enum",
              typeId: this.variantName(actor.layout.crash_kind!),
              shape: actor.layout.crash_kind!,
              tag: runtimeTag(
                this.pkg.variants[actor.layout.crash_kind!]!,
                code === 214
                  ? "CrashKindTimerOverdue"
                  : code === 213
                    ? "CrashKindHandlerFailed"
                    : "CrashKindFault",
              ),
              payload: [],
            },
            stringValue(String(message)),
          ],
        };
        let cancelTimer: (() => void) | undefined;
        const complete = (action: string) => {
          cancelTimer?.();
          actor.terminalHook = null;
          finished(action);
        };
        const hook = this.invokeFrame(
          actor,
          actor.layout.crash,
          [actor.state, info],
          (value) => {
            const shape = this.pkg.variants[actor.layout.crash_action!];
            const action =
              value.kind === "enum" ? shape?.cases[value.tag]?.name : undefined;
            if (!action)
              throw new Error("crash hook returned no declared action");
            complete(action);
          },
          (hookFault) => {
            actor.closingFault = combineFaults(actor.closingFault, hookFault);
            fault = combineFaults(fault, hookFault)!;
            actor.crashing = fault;
            this.reportFault(fault);
            complete("Restart");
          },
        );
        actor.terminalHook = hook;
        const parent = actor.supervisor;
        const stopNs = parent
          ? this.childStopBudget(parent.owner, parent.child)!
          : DEFAULT_ACTOR_STOP_NS;
        hook.requestDeadline = this.trace.virtualTimeMs + stopNs / 1_000_000;
        const expire = () => {
          hook.pendingCancellation ??= {
            kind: "panic",
            message: "crash hook deadline elapsed",
            deadline: true,
          };
          hook.cancel?.(hook.pendingCancellation);
        };
        if (stopNs <= 0) expire();
        else cancelTimer = this.scheduler.after(BigInt(stopNs), expire);
      } else finished("Restart");
    };
    this.drainMailbox(actor, this.faultText(fault), (cleanupFault) => {
      actor.closingFault = combineFaults(actor.closingFault, cleanupFault);
      fault = combineFaults(fault, cleanupFault)!;
      actor.crashing = fault;
      this.reportFault(fault);
      runHook();
    });
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

  private supervisorStartupOrder(owner: SupervisorInstance): number[] {
    const declarations =
      owner.layout.startup_order ??
      owner.layout.children.map((_child, index) => index);
    return declarations.flatMap((declaration) => {
      const offset = owner.offsets[declaration]!;
      const end = owner.offsets[declaration + 1] ?? owner.children.length;
      return Array.from(
        { length: end - offset },
        (_value, index) => offset + index,
      );
    });
  }

  private startChild(
    owner: SupervisorInstance,
    child: number,
    done: () => void,
    failed: (fault: Fault) => void,
  ): void {
    const spec = owner.specs[child]!;
    const arity = this.functionAt(spec.spawn).params.length;
    // Packages predating sibling wiring have config-only constructors. Their
    // actual callable signature, rather than a source name, selects that ABI.
    const args =
      arity === owner.config.length + 1
        ? [{ kind: "supervisor" as const, id: owner.id }, ...owner.config]
        : arity === owner.config.length
          ? owner.config
          : null;
    if (!args)
      throw new Error(
        "supervisor constructor signature does not match its config",
      );
    owner.constructor = this.invokeFrame(
      null,
      spec.spawn,
      args,
      (value) => {
        const cancellation = owner.constructor?.pendingCancellation;
        owner.constructor = null;
        if (!owner.alive) {
          // The result is still owned here. Its declaration slot hands it to
          // canonical reverse-order teardown, including the configured budget.
          owner.children[child] = value;
          failed(
            cancellation ?? {
              kind: "panic",
              message: "supervisor construction cancelled",
              cancelled: true,
            },
          );
          this.finishSupervisorStop(owner);
          return;
        }
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
      (fault) => {
        const stopped = !owner.alive;
        owner.constructor = null;
        // The initial caller still needs one terminal notification if the
        // supervisor was stopped directly rather than through caller cancel.
        failed(fault);
        if (stopped) this.finishSupervisorStop(owner);
      },
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

  private roleRestartPending(role: RoleSlot): boolean {
    const parent = this.roles.get(role.ownerId);
    if (parent && this.roleRestartPending(parent)) return true;
    if (this.rolePending(role)) return true;
    const value = this.roleValue(role) ?? role.latest;
    if (value?.kind === "actor") {
      const actor = this.actorFor(value);
      return !actor.completed && (actor.closing || !actor.alive);
    }
    if (value?.kind === "supervisor") {
      const owner = this.supervisorFor(value);
      return !owner.completed && !owner.alive;
    }
    return false;
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
    delayElapsed = false,
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
    if (owner.backoff && debts.length > 0 && !delayElapsed) {
      const previous = owner.restartDelays.get(child);
      const healthy =
        previous?.publishedAt != null &&
        now - previous.publishedAt >= owner.layout.window_secs * 1000;
      const doubled = previous ? previous.delay * 2n : owner.backoff.initial;
      const delay =
        !previous || healthy
          ? owner.backoff.initial
          : doubled < owner.backoff.maximum
            ? doubled
            : owner.backoff.maximum;
      owner.restartDelays.set(child, { delay, publishedAt: null });
      owner.recovering.add(child);
      owner.restartTimers.set(
        child,
        this.scheduler.after(delay, () => {
          owner.restartTimers.delete(child);
          this.restartChild(owner, child, debts, true);
        }),
      );
      return;
    }
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
    const starting = this.supervisorStartupOrder(owner).filter((index) =>
      affected.includes(index),
    );
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
      this.startChild(
        owner,
        index,
        () => {
          const delay = owner.restartDelays.get(index);
          if (delay) delay.publishedAt = this.trace.virtualTimeMs;
          start();
        },
        () => {
          owner.restartBusy = false;
          this.restartChild(owner, index, debts);
        },
      );
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

  private stopChild(
    value: VmValue | null,
    done: () => void,
    force = false,
  ): void {
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
      if (force) this.requestActorTerminate(actor);
      else this.requestActorStop(actor);
    } else if (value.kind === "supervisor") {
      const nested = this.supervisorFor(value);
      if (nested.completed) {
        done();
        return;
      }
      nested.closed.push(done);
      this.stopSupervisor(nested, force);
    } else throw new Error("supervisor child is neither actor nor supervisor");
  }

  private terminateChild(value: VmValue | null): void {
    if (!value) return;
    if (value.kind === "actor")
      this.requestActorTerminate(this.actorFor(value));
    else if (value.kind === "supervisor")
      this.stopSupervisor(this.supervisorFor(value), true);
  }

  private childStopBudget(
    owner: SupervisorInstance,
    child: number,
  ): number | null {
    const spec = owner.specs[child]!;
    if (spec.stop_config != null) {
      const duration = owner.config[spec.stop_config];
      if (duration?.kind !== "i64" || duration.value < 0n)
        throw new Error("child stop budget is not a non-negative duration");
      return Number(duration.value);
    }
    return (
      spec.stop_ns ?? ("actor" in spec.role ? DEFAULT_ACTOR_STOP_NS : null)
    );
  }

  private stopSupervisor(owner: SupervisorInstance, force = false): void {
    if (force) {
      owner.terminating = true;
      owner.teardownCancelTimer?.();
      owner.teardownCancelTimer = null;
      this.terminateChild(owner.teardownChild);
    }
    if (!owner.alive) return;
    owner.alive = false;
    for (const cancel of owner.restartTimers.values()) cancel();
    owner.restartTimers.clear();
    if (owner.constructor) {
      owner.constructor.pendingCancellation ??= {
        kind: "panic",
        message: "supervisor construction cancelled",
        cancelled: true,
      };
      owner.constructor.cancel?.(owner.constructor.pendingCancellation);
      return;
    }
    this.finishSupervisorStop(owner);
  }

  private finishSupervisorStop(owner: SupervisorInstance): void {
    let childIndex = owner.children.length - 1;
    const next = () => {
      owner.teardownCancelTimer?.();
      owner.teardownCancelTimer = null;
      owner.teardownChild = null;
      if (childIndex >= 0) {
        const index = childIndex--;
        const child = owner.children[index]!;
        owner.teardownChild = child;
        const stopNs = this.childStopBudget(owner, index);
        if (!owner.terminating && stopNs !== null && stopNs > 0) {
          owner.teardownCancelTimer = this.scheduler.after(BigInt(stopNs), () =>
            this.terminateChild(child),
          );
        }
        this.stopChild(child, next, owner.terminating || stopNs === 0);
        return;
      }
      this.closeValueAsync(
        { kind: "record", typeId: "", fields: owner.config },
        null,
        (fault) => {
          const debt = fault ? ++this.nextFaultDebt : undefined;
          if (debt !== undefined) this.faultDebts.add(debt);
          owner.completed = true;
          this.settleSupervisedEnd(
            owner,
            fault ? "Crashed" : owner.terminating ? "Terminated" : "Stopped",
            debt === undefined ? [] : [debt],
          );
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
        items: [
          ...actor.disposals.splice(0),
          ...messages.map<VmValue>((message) => ({
            kind: "record",
            typeId: "",
            fields: message.payload,
          })),
        ],
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

  private requestActorStop(actor: ActorInstance): void {
    if (actor.closing || actor.completed) return;
    actor.closing = true;
    this.cancelActorTimers(actor);
    if (actor.constructing && actor.active) {
      actor.active.pendingCancellation ??= {
        kind: "panic",
        message: "actor construction stopped",
        cancelled: true,
      };
      actor.active.cancel?.(actor.active.pendingCancellation);
    }
    // Admission closes immediately. The active turn and every accepted queued
    // turn finish before stop hooks and state release.
    this.dispatchActor(actor);
    for (const admit of actor.admission.splice(0)) admit();
  }

  private requestActorTerminate(actor: ActorInstance): void {
    if (actor.completed || actor.terminating) return;
    actor.closing = true;
    actor.terminating = true;
    this.cancelActorTimers(actor);
    for (const context of [actor.active, actor.terminalHook]) {
      if (!context) continue;
      context.pendingCancellation ??= {
        kind: "panic",
        message: "actor terminated",
        cancelled: true,
      };
      context.cancel?.(context.pendingCancellation);
    }
    this.dispatchActor(actor);
    for (const admit of actor.admission.splice(0)) admit();
  }

  private stopActor(actor: ActorInstance): void {
    actor.busy = true;
    const hooks = actor.terminating ? [] : [...actor.layout.stop];
    const next = () => {
      const hook = actor.terminating ? undefined : hooks.shift();
      if (hook !== undefined) {
        actor.terminalHook = this.invokeFrame(
          actor,
          hook,
          [actor.state],
          () => {
            actor.terminalHook = null;
            next();
          },
          (fault) => {
            actor.terminalHook = null;
            if (
              !(actor.terminating && fault.kind === "panic" && fault.cancelled)
            )
              actor.closingFault = combineFaults(actor.closingFault, fault);
            hooks.length = 0;
            next();
          },
        );
      } else {
        this.closeValueAsync(
          actor.state,
          this.faultText(actor.closingFault),
          (fault) => {
            actor.closingFault = combineFaults(actor.closingFault, fault);
            this.sweepRcOwner(actor.rcOwner, actor, (sweepFault) => {
              actor.closingFault = combineFaults(
                actor.closingFault,
                sweepFault,
              );
              // Stopped cleanup belongs to the completion barrier, independently
              // of handler-crash debt and supervisor restart decisions.
              actor.crashing = null;
              if (actor.closingFault) {
                this.reportFault(actor.closingFault);
                actor.closeFaultDebt = ++this.nextFaultDebt;
                this.faultDebts.add(actor.closeFaultDebt);
              }
              actor.completed = true;
              actor.alive = false;
              actor.busy = false;
              this.trace.snapshot(
                actor.terminating ? "actor.terminate" : "actor.stop",
                { actor_id: actor.id },
              );
              this.settleSupervisedEnd(
                actor,
                actor.closingFault
                  ? "Crashed"
                  : actor.terminating
                    ? "Terminated"
                    : "Stopped",
                actor.closeFaultDebt === undefined
                  ? []
                  : [actor.closeFaultDebt],
              );
              for (const wake of actor.closed.splice(0)) wake();
            });
          },
          actor,
        );
      }
    };
    next();
  }

  private settleSupervisedEnd(
    subject: ActorInstance | SupervisorInstance,
    reason: "Stopped" | "Terminated" | "Crashed",
    debts: number[],
  ): void {
    const parent = subject.supervisor;
    if (
      !parent ||
      !parent.owner.alive ||
      parent.owner.recovering.has(parent.child)
    )
      return;
    const { owner, child } = parent;
    const policy = owner.specs[child]!.restart;
    if (
      policy === "permanent" ||
      (policy === "transient" && reason !== "Stopped")
    ) {
      this.restartChild(owner, child, debts);
    } else {
      owner.spent.add(child);
      this.wakeRoles();
    }
  }

  // ── exit and faults ──────────────────────────────────────────────────────

  private reportFault(fault: Fault): void {
    const diagnostics = [fault.primary ?? fault, ...(fault.secondary ?? [])];
    diagnostics.forEach((diagnostic, index) => {
      const origin = diagnostic.primary ?? diagnostic;
      if (origin.reported) return;
      this.trace.writeStderr(faultDiagnostic(origin, index !== 0), null);
      origin.reported = true;
    });
  }

  private haltWithFault(fault: Fault): never {
    const isPanic = fault.kind === "panic";
    const status: RuntimeStatus = isPanic ? "panic" : "trap";
    const trapKind: TrapKind = isPanic ? "panic" : fault.trap;
    const message = isPanic
      ? fault.message
      : (fault.message ?? trapMessage(fault.trap));
    const [code, kind] = faultInfo(fault);
    // Native writes the failure line to stderr before it exits; so does this.
    this.reportFault(fault);
    // A failing run reports no exit code: native exits 1 for every fault and
    // names the kind in its message, so the kind is what travels, in
    // `runtime_failures`. The page turns it into an exit code of its own.
    this.trace.fail(
      status,
      "runtime.failure",
      runtimeFailure(
        isPanic ? "panic" : "trap",
        message,
        trapKind,
        null,
        undefined,
        fault.kind === "panic" ? fault.assertion : undefined,
        { code, kind },
      ),
    );
    throw new Halt(status);
  }
}

function combineFaults(
  primary: Fault | null,
  secondary: Fault | null,
): Fault | null {
  if (!primary) return secondary;
  if (!secondary || primary === secondary) return primary;
  const diagnostics: Fault[] = [];
  const visited = new Set<Fault>();
  const append = (fault: Fault) => {
    if (visited.has(fault)) return;
    visited.add(fault);
    const origin = fault.primary ?? fault;
    if (!diagnostics.includes(origin)) diagnostics.push(origin);
    for (const next of fault.secondary ?? []) append(next);
  };
  append(primary);
  append(secondary);
  // Reports own an immutable ordered view of original diagnostics. Faults can
  // be shared by a parked unwind, actor completion and multiple observers;
  // combining those views must neither duplicate nor mutate their graph.
  const control = (fault: Fault) =>
    (fault.kind === "panic" && fault.cancelled) ||
    (fault.kind === "trap" && fault.trap === "actor_refused");
  const retained = control(primary)
    ? diagnostics.filter((fault) => !control(fault))
    : diagnostics;
  const [first, ...rest] = retained.length ? retained : diagnostics;
  return { ...first!, primary: first!, secondary: rest };
}

function faultInfo(fault: Fault): [number, string] {
  if (fault.kind === "panic") {
    if (fault.handlerFailed) return [213, "ActorUnhandledFailure"];
    if (fault.timerOverdue) return [214, "ActorTimerOverdue"];
    if (fault.actorSendFailed) return [206, "ActorSendFailed"];
    if (fault.cancelled) return [-1, "Cancelled"];
    if (fault.deadline) return [-2, "Deadline"];
    return [212, "UserPanic"];
  }
  return {
    integer_overflow: [201, "IntegerOverflow"],
    divide_by_zero: [202, "DivideByZero"],
    shift_out_of_range: [204, "ShiftOutOfRange"],
    vector_bounds: [205, "IndexOutOfBounds"],
    actor_refused: [-4, "Refused"],
  }[fault.trap] as [number, string];
}

function faultApplicationMessage(fault: Fault): string | undefined {
  return fault.kind === "panic" &&
    (fault.cancelled || fault.deadline || fault.timerOverdue)
    ? undefined
    : fault.message;
}

function faultDiagnostic(fault: Fault, secondary = false): string {
  const [code, reason] = faultInfo(fault);
  const label = secondary ? "secondary failure" : "failure";
  const message = faultApplicationMessage(fault);
  const detail = message === undefined ? "" : `: ${message}`;
  return `hew: ${label}: ${reason} (${code})${detail}\n`;
}

function faultRecoveryMessage(fault: Fault): string {
  let text = faultApplicationMessage(fault) ?? faultInfo(fault)[1];
  for (const diagnostic of fault.secondary ?? []) {
    text += `; secondary failure: ${faultInfo(diagnostic)[1]}`;
    const detail = faultApplicationMessage(diagnostic);
    if (detail !== undefined) text += `: ${detail}`;
  }
  return text;
}

function faultReport(fault: Fault, secondary = false): string {
  return (
    faultDiagnostic(fault, secondary) +
    (fault.secondary ?? []).map((next) => faultReport(next, true)).join("")
  );
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

function actorErrorRole(reason: string): RuntimeVariantRole {
  switch (reason) {
    case "Rejected":
      return "ActorErrorRejected";
    case "Failed":
      return "ActorErrorFailed";
    case "Trapped":
      return "ActorErrorTrapped";
    case "Refused":
      return "ActorErrorRefused";
    case "Dead":
      return "ActorErrorDead";
    case "TimedOut":
      return "ActorErrorTimedOut";
    case "NodeNotRunning":
      return "ActorErrorNodeNotRunning";
    case "RoutingFailed":
      return "ActorErrorRoutingFailed";
    case "EncodeFailed":
      return "ActorErrorEncodeFailed";
    case "MessageTooLarge":
      return "ActorErrorMessageTooLarge";
    case "ConnectionDropped":
      return "ActorErrorConnectionDropped";
    case "Partition":
      return "ActorErrorPartition";
    default:
      throw new Error(`unknown actor error reason ${reason}`);
  }
}

function sendErrorRole(reason: string): RuntimeVariantRole {
  switch (reason) {
    case "MessageTooLarge":
      return "SendErrorMessageTooLarge";
    case "Full":
      return "SendErrorFull";
    case "Closed":
      return "SendErrorClosed";
    case "Partition":
      return "SendErrorPartition";
    case "StaleRef":
      return "SendErrorStaleRef";
    case "LocalShutdown":
      return "SendErrorLocalShutdown";
    case "Cancelled":
      return "SendErrorCancelled";
    case "VersionMismatch":
      return "SendErrorVersionMismatch";
    case "Backpressure":
      return "SendErrorBackpressure";
    case "Dead":
      return "SendErrorDead";
    default:
      throw new Error(`unknown send error reason ${reason}`);
  }
}

function calleeFunction(
  value: VmValue,
  specialization: number | null = null,
): number {
  switch (value.kind) {
    case "function":
      return Number(value.id);
    case "closure": {
      const selected = specialization ?? value.specialization;
      const entry = value.entries.find(
        (candidate) => candidate.specialization === selected,
      );
      if (!entry)
        throw new Error("callable has no selected concrete invocation entry");
      return entry.body;
    }
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
    case "actor":
    case "supervisor":
      return [value.kind, value.id];
    case "i64":
      return value.value.toString();
    case "record":
      return value.fields.map(toComparable);
    case "enum":
      return [value.tag, value.payload.map(toComparable)];
    case "vector":
      return value.items.map(toComparable);
    case "rc":
      return "Rc";
    case "weak":
      return "Weak";
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
    case "actor_refused":
      return "delivery refused by interceptor";
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
    case "BitAnd":
    case "BitOr":
    case "BitXor":
      // Both operands were already evaluated; bool operands stay bool.
      if (lhs.kind === "bool" && rhs.kind === "bool") {
        return {
          kind: "bool",
          value:
            op === "BitAnd"
              ? lhs.value && rhs.value
              : op === "BitOr"
                ? lhs.value || rhs.value
                : lhs.value !== rhs.value,
        };
      }
      break;
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
    return stringValue(lhs.value + asString(rhs));
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
      return stringValue(asString(value));
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
