import { validateBytecodePackage } from "./schema-validator.js";
import { TraceBuilder, runtimeFailure } from "./trace.js";
import {
  ActorScheduler,
  ActorTurnCrash,
  SchedulerRuntimeError,
  type ActorLayout,
  type SchedulerPolicy,
  type SupervisorLayout,
  type SupervisorStrategy,
  type ChildRestartClass
} from "../scheduler/scheduler.js";
import type {
  BytecodeFunction,
  Instruction,
  Operand,
  ReplayConfig,
  RunOptions,
  RuntimeStatus,
  SandboxBytecodePackage,
  SandboxTrace,
  Terminator,
  JsonValue,
  TrapKind
} from "./types.js";
import { UNIT, canonicalJson, cloneValue, i64ComparableJson, renderStdout, toJsonValue, valueFromLiteral, type VmValue } from "./values.js";
import {
  SANDBOX_STDLIB_PROFILE,
  lookupStdlibProfile,
  unsupportedDiagnosticFor,
  validateStdlibProfile,
  type StdlibProfileEntry,
  type StdlibShimHandler,
  type UnsupportedStdlibDiagnostic
} from "./stdlib-profile.js";

const DEFAULT_STEP_BUDGET = 1_000_000;
const I64_MIN = -(1n << 63n);
const I64_MAX = (1n << 63n) - 1n;
const MAX_LAYOUT_INT32 = 0x7fffffff;

const NATIVE_ONLY_OP_PREFIXES = ["stream.file.", "stream.net."];

// Machine ops (machine.new/step/state) are implemented; no machine op is deferred.
const M6_DEFERRED_OP_PREFIXES: readonly string[] = [];

const IMPLEMENTED_STDLIB_HANDLERS: ReadonlySet<StdlibShimHandler> = new Set([
  "io.stdout.print",
  "io.stdout.println",
  "io.stderr.print",
  "io.stderr.println",
  "io.log",
  "process.exit",
  "time.now",
  "time.sleep",
  "time.deadline",
  "fmt.to_string",
  "string.len",
  "string.concat",
  "string.slice",
  "math.abs",
  "math.min",
  "math.max",
  "math.clamp",
  "vec.len",
  "vec.get",
  "vec.push",
  "regex.compile",
  "regex.is_match",
  "regex.find",
  "regex.replace"
]);

validateStdlibProfile(SANDBOX_STDLIB_PROFILE, IMPLEMENTED_STDLIB_HANDLERS);

interface Frame {
  fn: BytecodeFunction;
  locals: Map<string, VmValue>;
}

interface ChannelCell {
  id: string;
  capacity: number;
  queue: VmValue[];
  closed: boolean;
}

interface MachineTransitionSpec {
  event: string;
  from: string;
  to: string;
}

interface MachineLayout {
  id: string;
  name: string;
  states: string[];
  events: string[];
  transitions: MachineTransitionSpec[];
}

interface TaskCell {
  id: string;
  functionId: string;
  args: VmValue[];
  status: "pending" | "running" | "completed" | "cancelled";
  value: VmValue;
  scopeId: string | null;
}

interface ScopeCell {
  id: string;
  children: Set<string>;
  cancelled: boolean;
}

interface SelectArm {
  kind: "recv" | "timer";
  label: string;
  index: number;
  channel?: ChannelCell;
  afterMs?: number;
}

type ScalarComparable =
  | { kind: "i64"; value: bigint }
  | { kind: "f64"; value: number }
  | { kind: "string"; value: string };

type IntegerCastType = {
  kind: "integer";
  bits: 8 | 16 | 32 | 64;
  signed: boolean;
};

type NumericCastType =
  | IntegerCastType
  | { kind: "float"; bits: 32 | 64 }
  | { kind: "bool" }
  | { kind: "char" };

class VmHalt extends Error {
  constructor(readonly status: RuntimeStatus) {
    super(status);
  }
}

export function runBytecode(input: unknown, options: RunOptions = {}): SandboxTrace {
  const bytecode = validateBytecodePackage(input);
  const fixtureId = options.fixtureId ?? fixtureIdFromPackage(bytecode);
  const replay: ReplayConfig = {
    seed: options.replay?.seed ?? 0,
    step_budget: options.stepBudget ?? options.replay?.step_budget ?? DEFAULT_STEP_BUDGET,
    virtual_clock: options.replay?.virtual_clock ? { ...options.replay.virtual_clock } : { epoch_ms: 0, tick_ms: 1, current_ms: 0 },
    inputs: [...(options.replay?.inputs ?? [])]
  };
  const trace = new TraceBuilder(
    bytecode,
    fixtureId,
    options.traceId ?? `trace:${fixtureId}`,
    replay,
    options.sandboxVersion ?? "0.0.0-spec"
  );
  const vm = new Interpreter(bytecode, trace, options.schedulerPolicy ?? "round_robin");
  try {
    vm.run();
  } catch (error) {
    if (!(error instanceof VmHalt)) {
      const failure = runtimeFailure("internal_error", "sandbox internal error", "internal_error", null);
      trace.fail("runtime_failure", "runtime.failure", failure);
    }
  }
  return trace.finish();
}

class Interpreter {
  private readonly functions = new Map<string, BytecodeFunction>();
  private readonly stdlibSymbols: Map<string, SandboxBytecodePackage["stdlib_symbols"][number]>;
  private readonly capabilities: Map<string, SandboxBytecodePackage["capabilities"][number]>;
  private readonly records: Map<string, SandboxBytecodePackage["layouts"]["records"][number]>;
  private readonly enums: Map<string, SandboxBytecodePackage["layouts"]["enums"][number]>;
  private readonly actors: Map<string, ActorLayout>;
  private readonly supervisors: Map<string, SupervisorLayout>;
  private readonly machines: Map<string, MachineLayout>;
  private readonly scheduler: ActorScheduler;
  private readonly channels = new Map<string, ChannelCell>();
  private readonly tasks = new Map<string, TaskCell>();
  private readonly readyTasks: string[] = [];
  private readonly scopes: ScopeCell[] = [];
  private selectCounter = 0;
  private scopeCounter = 0;
  private taskDriverDepth = 0;

  constructor(
    private readonly bytecode: SandboxBytecodePackage,
    private readonly trace: TraceBuilder,
    schedulerPolicy: SchedulerPolicy
  ) {
    for (const fn of bytecode.functions) {
      this.functions.set(fn.id, fn);
    }
    this.stdlibSymbols = new Map(bytecode.stdlib_symbols.map((symbol) => [symbol.id, symbol]));
    this.capabilities = new Map(bytecode.capabilities.map((capability) => [capability.id, capability]));
    this.records = new Map(bytecode.layouts.records.map((record) => [record.id, record]));
    this.enums = new Map(bytecode.layouts.enums.map((enumLayout) => [enumLayout.id, enumLayout]));
    this.actors = new Map(bytecode.layouts.actors.map((layout) => {
      const actor = actorLayout(layout);
      return [actor.id, actor];
    }));
    this.supervisors = new Map(bytecode.layouts.supervisors.map((layout) => {
      const supervisor = supervisorLayout(layout);
      return [supervisor.id, supervisor];
    }));
    this.machines = new Map(bytecode.layouts.machines.map((layout) => {
      const machine = machineLayout(layout);
      return [machine.id, machine];
    }));
    this.scheduler = new ActorScheduler(trace.replay.seed, schedulerPolicy, trace, {
      invoke: (functionId, args) => this.invoke(this.functionFor(functionId, null), args),
      consumeStep: () => this.commitOrExhaust()
    });
  }

  run(): void {
    try {
      const entry = this.entryFunction();
      this.invoke(entry, []);
      this.drainTasks();
      this.scheduler.drain();
    } catch (error) {
      if (error instanceof SchedulerRuntimeError) {
        this.raiseSchedulerError(error, null);
      }
      throw error;
    }
  }

  private entryFunction(): BytecodeFunction {
    const entryModule = this.bytecode.module_graph.modules.find((module) => module.id === this.bytecode.module_graph.entry);
    const entryFunctionId =
      entryModule?.functions.find((id) => this.functions.get(id)?.name === "main") ??
      entryModule?.functions[0] ??
      this.bytecode.functions[0]?.id;
    const entry = entryFunctionId ? this.functions.get(entryFunctionId) : undefined;
    if (!entry) {
      this.trap("invalid_call", "entry function not found", null);
    }
    return entry;
  }

  private invoke(fn: BytecodeFunction, args: VmValue[]): VmValue {
    const frame: Frame = { fn, locals: new Map() };
    for (const [index, param] of fn.params.entries()) {
      frame.locals.set(param, cloneValue(args[index] ?? UNIT));
    }
    for (const local of fn.locals) {
      if (!frame.locals.has(local.id)) {
        frame.locals.set(local.id, UNIT);
      }
    }

    const blocks = new Map(fn.blocks.map((block) => [block.id, block]));
    let block = fn.blocks[0];
    let instructionIndex = 0;

    while (true) {
      if (instructionIndex < block.instructions.length) {
        const instruction = block.instructions[instructionIndex]!;
        this.commitOrExhaust();
        this.executeInstruction(frame, instruction);
        if (hasMetadataFlag(instruction, "trace_step")) {
          this.trace.stepCommitted();
        }
        instructionIndex += 1;
        continue;
      }

      this.commitOrExhaust();
      const next = this.executeTerminator(frame, block.terminator, blocks);
      if (next.returned) {
        return next.value;
      }
      block = next.block;
      instructionIndex = 0;
    }
  }

  private executeInstruction(frame: Frame, instruction: Instruction): void {
    if (M6_DEFERRED_OP_PREFIXES.some((prefix) => instruction.op.startsWith(prefix))) {
      this.unsupported(`Unsupported::M6_DEFERRED: ${instruction.op}`, instruction.span);
    }
    if (NATIVE_ONLY_OP_PREFIXES.some((prefix) => instruction.op.startsWith(prefix))) {
      this.unsupported(`Unsupported::NATIVE_ONLY: ${instruction.op}`, instruction.span, {
        kind: "Unsupported::NATIVE_ONLY",
        symbol: instruction.op,
        status: "unsupported_native_only",
        reason: "File/network-backed streams are unavailable in the deterministic browser sandbox"
      });
    }

    switch (instruction.op) {
      case "const.unit":
        this.writeDst(frame, instruction, UNIT);
        return;
      case "const.bool":
        this.writeDst(frame, instruction, { kind: "bool", value: Boolean(instruction.args[0]?.value) });
        return;
      case "const.i64":
      case "const.u64":
        this.writeDst(frame, instruction, this.i64(this.bigintArg(instruction.args[0], instruction.span)));
        return;
      case "const.f32":
        this.writeDst(frame, instruction, this.f32(this.numberArg(instruction.args[0], instruction.span)));
        return;
      case "const.f64":
        this.writeDst(frame, instruction, { kind: "f64", value: this.numberArg(instruction.args[0], instruction.span) });
        return;
      case "const.string":
        this.writeDst(frame, instruction, { kind: "string", value: this.stringArg(instruction.args[0], instruction.span) });
        return;
      case "const.regex":
        this.writeDst(frame, instruction, this.compileRegex(this.stringArg(instruction.args[0], instruction.span), instruction.span));
        return;
      case "bool.not": {
        const value = this.resolve(frame, instruction.args[0], instruction.span);
        if (value.kind !== "bool") {
          this.trap("invalid_local", "bool.not requires a bool operand", instruction.span);
        }
        this.writeDst(frame, instruction, { kind: "bool", value: !value.value });
        return;
      }
      case "numeric.cast":
        this.writeDst(frame, instruction, this.numericCast(frame, instruction));
        return;
      case "local.get":
      case "local.move":
      case "local.borrow":
        // Return the stored value directly (no deep-clone) so in-place mutation ops
        // (vector.push, vector.set) that receive this value as their first operand
        // observe the mutation on the same object that `local.set` stored.
        // Aliasing safety: `local.set` clones on write, so an independent assignment
        // `local:b = local:a` produces a distinct copy — the clone happens at the
        // write site, not the read site.
        this.writeDst(frame, instruction, this.readLocal(frame, this.localId(instruction.args[0], instruction.span), instruction.span));
        return;
      case "local.set": {
        const target = this.localId(instruction.args[0], instruction.span);
        frame.locals.set(target, cloneValue(this.resolve(frame, instruction.args[1], instruction.span)));
        return;
      }
      // Wrapping arithmetic (`&+`/`&-`/`&*`): truncate to two's-complement 64 bits
      // to match native LLVM `i64.add/sub/mul`, which wrap silently on overflow.
      // Without `asIntN(64)` an overflowing operation yields a mathematically
      // unbounded BigInt (e.g. `i64::MAX &+ 1` -> 2^63 instead of `i64::MIN`).
      // The shift opcode already applies the same truncation; see #2341.
      case "i64.add":
        this.writeDst(frame, instruction, this.i64(BigInt.asIntN(64, this.i64Arg(frame, instruction.args[0], instruction.span) + this.i64Arg(frame, instruction.args[1], instruction.span))));
        return;
      case "i64.sub":
        this.writeDst(frame, instruction, this.i64(BigInt.asIntN(64, this.i64Arg(frame, instruction.args[0], instruction.span) - this.i64Arg(frame, instruction.args[1], instruction.span))));
        return;
      case "i64.mul":
        this.writeDst(frame, instruction, this.i64(BigInt.asIntN(64, this.i64Arg(frame, instruction.args[0], instruction.span) * this.i64Arg(frame, instruction.args[1], instruction.span))));
        return;
      case "i64.and":
        this.writeDst(frame, instruction, this.i64(this.i64Arg(frame, instruction.args[0], instruction.span) & this.i64Arg(frame, instruction.args[1], instruction.span)));
        return;
      case "i64.or":
        this.writeDst(frame, instruction, this.i64(this.i64Arg(frame, instruction.args[0], instruction.span) | this.i64Arg(frame, instruction.args[1], instruction.span)));
        return;
      case "i64.xor":
        this.writeDst(frame, instruction, this.i64(this.i64Arg(frame, instruction.args[0], instruction.span) ^ this.i64Arg(frame, instruction.args[1], instruction.span)));
        return;
      case "i64.shl": {
        const count = this.shiftCount(frame, instruction.args[1], instruction.span);
        this.writeDst(frame, instruction, this.i64(BigInt.asIntN(64, this.i64Arg(frame, instruction.args[0], instruction.span) << count)));
        return;
      }
      case "i64.shr": {
        const count = this.shiftCount(frame, instruction.args[1], instruction.span);
        this.writeDst(frame, instruction, this.i64(this.i64Arg(frame, instruction.args[0], instruction.span) >> count));
        return;
      }
      case "i64.div":
      case "i64.checked_div":
        this.writeDst(frame, instruction, this.i64(this.checkedDiv(frame, instruction.args[0], instruction.args[1], instruction.span)));
        return;
      case "i64.rem":
      case "i64.checked_rem":
        this.writeDst(frame, instruction, this.i64(this.checkedRem(frame, instruction.args[0], instruction.args[1], instruction.span)));
        return;
      case "i64.neg":
        this.writeDst(frame, instruction, this.checkedI64(-this.i64Arg(frame, instruction.args[0], instruction.span), instruction.span));
        return;
      case "i64.checked_add":
        this.writeDst(frame, instruction, this.checkedI64(this.i64Arg(frame, instruction.args[0], instruction.span) + this.i64Arg(frame, instruction.args[1], instruction.span), instruction.span));
        return;
      case "i64.checked_sub":
        this.writeDst(frame, instruction, this.checkedI64(this.i64Arg(frame, instruction.args[0], instruction.span) - this.i64Arg(frame, instruction.args[1], instruction.span), instruction.span));
        return;
      case "i64.checked_mul":
        this.writeDst(frame, instruction, this.checkedI64(this.i64Arg(frame, instruction.args[0], instruction.span) * this.i64Arg(frame, instruction.args[1], instruction.span), instruction.span));
        return;
      // f32 values use the VM's numeric value representation, but every literal
      // and arithmetic result is rounded to IEEE-754 single precision.
      case "f32.add":
        this.writeDst(frame, instruction, this.f32(this.f32Arg(frame, instruction.args[0], instruction.span) + this.f32Arg(frame, instruction.args[1], instruction.span)));
        return;
      case "f32.sub":
        this.writeDst(frame, instruction, this.f32(this.f32Arg(frame, instruction.args[0], instruction.span) - this.f32Arg(frame, instruction.args[1], instruction.span)));
        return;
      case "f32.mul":
        this.writeDst(frame, instruction, this.f32(this.f32Arg(frame, instruction.args[0], instruction.span) * this.f32Arg(frame, instruction.args[1], instruction.span)));
        return;
      case "f32.div":
        this.writeDst(frame, instruction, this.f32(this.f32Arg(frame, instruction.args[0], instruction.span) / this.f32Arg(frame, instruction.args[1], instruction.span)));
        return;
      case "f32.rem":
        this.writeDst(frame, instruction, this.f32(this.f32Arg(frame, instruction.args[0], instruction.span) % this.f32Arg(frame, instruction.args[1], instruction.span)));
        return;
      case "f32.neg":
        this.writeDst(frame, instruction, this.f32(-this.f32Arg(frame, instruction.args[0], instruction.span)));
        return;
      // f64 arithmetic follows IEEE-754 exactly (matching native LLVM
      // fadd/fsub/fmul/fdiv/frem/fneg). JS `number` is an IEEE-754 double, so
      // the host operators reproduce native results bit-for-bit. Unlike the
      // checked i64 ops these never trap: float divide-by-zero yields
      // ±Infinity/NaN, never `divide_by_zero`.
      case "f64.add":
        this.writeDst(frame, instruction, this.f64(this.f64Arg(frame, instruction.args[0], instruction.span) + this.f64Arg(frame, instruction.args[1], instruction.span)));
        return;
      case "f64.sub":
        this.writeDst(frame, instruction, this.f64(this.f64Arg(frame, instruction.args[0], instruction.span) - this.f64Arg(frame, instruction.args[1], instruction.span)));
        return;
      case "f64.mul":
        this.writeDst(frame, instruction, this.f64(this.f64Arg(frame, instruction.args[0], instruction.span) * this.f64Arg(frame, instruction.args[1], instruction.span)));
        return;
      case "f64.div":
        this.writeDst(frame, instruction, this.f64(this.f64Arg(frame, instruction.args[0], instruction.span) / this.f64Arg(frame, instruction.args[1], instruction.span)));
        return;
      case "f64.rem":
        // JS `%` on doubles is C `fmod` / LLVM `frem`: truncated remainder
        // carrying the sign of the dividend — identical to native.
        this.writeDst(frame, instruction, this.f64(this.f64Arg(frame, instruction.args[0], instruction.span) % this.f64Arg(frame, instruction.args[1], instruction.span)));
        return;
      case "f64.neg":
        this.writeDst(frame, instruction, this.f64(-this.f64Arg(frame, instruction.args[0], instruction.span)));
        return;
      case "cmp.eq":
      case "cmp.ne":
      case "cmp.lt":
      case "cmp.le":
      case "cmp.gt":
      case "cmp.ge":
        this.writeDst(frame, instruction, { kind: "bool", value: this.compare(frame, instruction) });
        return;
      case "call.direct": {
        const fn = this.functionFor(this.stringOperand(instruction.args[0], instruction.span), instruction.span);
        const args = instruction.args.slice(1).map((arg) => this.resolve(frame, arg, instruction.span));
        this.writeDst(frame, instruction, this.invoke(fn, args));
        return;
      }
      case "call.indirect": {
        // Resolve the callee local, which must hold a `function`-kind value
        // materialised by `const.function`.  Look up the function by its id and
        // invoke it with the remaining operands as arguments.
        const calleeValue = this.resolve(frame, instruction.args[0], instruction.span);
        if (calleeValue.kind !== "function") {
          this.trap("invalid_call", "call.indirect: callee is not a function value", instruction.span);
        }
        const fn = this.functionFor(calleeValue.id, instruction.span);
        const args = instruction.args.slice(1).map((arg) => this.resolve(frame, arg, instruction.span));
        this.writeDst(frame, instruction, this.invoke(fn, args));
        return;
      }
      case "const.function": {
        // Materialise a first-class function reference.  The single operand is a
        // `function`-kind static operand carrying the bytecode function id.
        const id = this.functionOperand(instruction.args[0], instruction.span);
        this.writeDst(frame, instruction, { kind: "function", id });
        return;
      }
      case "call.stdlib":
        this.callStdlib(frame, instruction);
        return;
      case "actor.spawn": {
        const layoutId = this.stringOperand(instruction.args[0], instruction.span);
        const layout = this.actors.get(layoutId);
        if (!layout) {
          this.trap("invalid_call", "actor layout not found", instruction.span);
        }
        this.writeDst(
          frame,
          instruction,
          this.scheduler.spawn(layout, instruction.args.slice(1).map((arg) => this.resolve(frame, arg, instruction.span)), this.scheduler.activeActor ?? "actor:root", this.trace.span(instruction.span))
        );
        return;
      }
      case "supervisor.spawn": {
        this.assertSupervisorOpAllowed(instruction);
        const layoutId = this.stringOperand(instruction.args[0], instruction.span);
        const layout = this.supervisors.get(layoutId);
        if (!layout) {
          this.trap("invalid_call", "supervisor layout not found", instruction.span);
        }
        this.writeDst(
          frame,
          instruction,
          this.withSchedulerError(instruction, () =>
            this.scheduler.spawnSupervisor(layout, this.actors, this.scheduler.activeActor ?? "actor:root", this.trace.span(instruction.span))
          )
        );
        return;
      }
      case "supervisor.child":
        this.assertSupervisorOpAllowed(instruction);
        this.writeDst(
          frame,
          instruction,
          this.withSchedulerError(instruction, () =>
            this.scheduler.child(
              this.resolve(frame, instruction.args[0], instruction.span),
              this.stringOperand(instruction.args[1], instruction.span)
            )
          )
        );
        return;
      case "supervisor.restart":
      case "supervisor.stop":
        this.assertSupervisorOpAllowed(instruction);
        this.unsupported(`Unsupported::M6_DEFERRED: ${instruction.op} direct control`, instruction.span);
        return;
      case "machine.new":
        this.writeDst(frame, instruction, this.machineNew(instruction));
        return;
      case "machine.state":
        this.writeDst(frame, instruction, this.machineState(frame, instruction));
        return;
      case "machine.step":
        this.writeDst(frame, instruction, this.machineStep(frame, instruction));
        return;
      case "actor.send":
        this.withSchedulerError(instruction, () => {
          this.scheduler.send(
            this.resolve(frame, instruction.args[0], instruction.span),
            this.messageName(instruction.args[1], instruction.span),
            instruction.args.slice(2).map((arg) => this.resolve(frame, arg, instruction.span)),
            this.trace.span(instruction.span)
          );
        });
        return;
      case "actor.ask":
        this.writeDst(
          frame,
          instruction,
          this.withSchedulerError(instruction, () =>
            this.scheduler.ask(
              this.resolve(frame, instruction.args[0], instruction.span),
              this.messageName(instruction.args[1], instruction.span),
              instruction.args.slice(2).map((arg) => this.resolve(frame, arg, instruction.span)),
              this.trace.span(instruction.span)
            )
          )
        );
        return;
      case "actor.reply":
        this.withSchedulerError(instruction, () => {
          this.scheduler.reply(this.resolve(frame, instruction.args[0], instruction.span), this.resolve(frame, instruction.args[1], instruction.span));
        });
        return;
      case "actor.link":
        this.withSchedulerError(instruction, () => {
          const [left, right] = this.actorPair(frame, instruction);
          this.scheduler.link(left, right, this.trace.span(instruction.span));
        });
        return;
      case "actor.unlink":
        this.withSchedulerError(instruction, () => {
          const [left, right] = this.actorPair(frame, instruction);
          this.scheduler.unlink(left, right, this.trace.span(instruction.span));
        });
        return;
      case "actor.monitor":
        this.writeDst(
          frame,
          instruction,
          this.withSchedulerError(instruction, () => {
            const [owner, target] = this.actorPair(frame, instruction);
            return this.scheduler.monitor(owner, target, this.trace.span(instruction.span));
          })
        );
        return;
      case "actor.demonitor":
        this.withSchedulerError(instruction, () => {
          this.scheduler.demonitor(this.resolve(frame, instruction.args[0], instruction.span), this.trace.span(instruction.span));
        });
        return;
      case "mailbox.dequeue":
        this.writeDst(frame, instruction, this.withSchedulerError(instruction, () => this.scheduler.dequeueCurrentPayload()));
        return;
      case "channel.new":
        this.writeDst(frame, instruction, { kind: "channel", id: this.createChannel(this.capacityArg(frame, instruction.args[1], instruction.span), null, instruction.span).id });
        return;
      case "channel.send":
        this.channelSend(
          this.channelArg(frame, instruction.args[0], instruction.span),
          this.resolve(frame, instruction.args[1], instruction.span),
          instruction.span
        );
        return;
      case "channel.recv":
        this.writeDst(frame, instruction, this.channelRecv(this.channelArg(frame, instruction.args[0], instruction.span), instruction.span));
        return;
      case "channel.close":
        this.channelClose(this.channelArg(frame, instruction.args[0], instruction.span), instruction.span);
        return;
      case "stream.memory": {
        const channel = this.createChannel(this.capacityArg(frame, instruction.args[0], instruction.span), "stream.memory", instruction.span);
        this.writeDst(frame, instruction, { kind: "duplex", channelId: channel.id });
        return;
      }
      case "stream.write":
        this.channelSend(this.streamWritableArg(frame, instruction.args[0], instruction.span), this.resolve(frame, instruction.args[1], instruction.span), instruction.span);
        return;
      case "stream.read":
        this.writeDst(frame, instruction, this.channelRecv(this.streamReadableArg(frame, instruction.args[0], instruction.span), instruction.span));
        return;
      case "stream.close":
        this.channelClose(this.streamChannelArg(frame, instruction.args[0], instruction.span), instruction.span);
        return;
      case "task.spawn":
        this.writeDst(frame, instruction, this.spawnTask(this.functionOperand(instruction.args[0], instruction.span), instruction.args.slice(1).map((arg) => this.resolve(frame, arg, instruction.span)), null));
        return;
      case "task.await":
        this.writeDst(frame, instruction, this.awaitTask(this.taskArg(frame, instruction.args[0], instruction.span), instruction.span));
        return;
      case "task.cancel":
        this.cancelTask(this.taskArg(frame, instruction.args[0], instruction.span), "task.cancel");
        return;
      case "scope.enter":
        this.enterScope();
        return;
      case "scope.launch":
        this.writeDst(frame, instruction, this.spawnTask(this.functionOperand(instruction.args[0], instruction.span), instruction.args.slice(1).map((arg) => this.resolve(frame, arg, instruction.span)), this.currentScope().id));
        return;
      case "scope.await":
        this.writeDst(frame, instruction, this.awaitTask(this.taskArg(frame, instruction.args[0], instruction.span), instruction.span));
        return;
      case "scope.cancel":
        this.cancelScope("scope.cancel");
        return;
      case "scope.exit":
        this.exitScope();
        return;
      case "select.poll":
        this.writeDst(frame, instruction, this.selectPoll(frame, instruction));
        return;
      case "clock.sleep":
        this.sleepVirtual(this.durationArg(frame, instruction.args[0], instruction.span), instruction.span);
        return;
      case "record.new":
        this.writeDst(frame, instruction, {
          kind: "record",
          typeId: this.stringOperand(instruction.args[0], instruction.span),
          fields: instruction.args.slice(1).map((arg) => this.resolve(frame, arg, instruction.span))
        });
        return;
      case "record.get":
        this.writeDst(frame, instruction, this.recordGet(frame, instruction));
        return;
      case "record.set":
        this.unsupported("Unsupported::M4_DEFERRED: record.set mutable projection", instruction.span);
        return;
      case "enum.new":
        this.writeDst(frame, instruction, {
          kind: "enum",
          typeId: this.stringOperand(instruction.args[0], instruction.span),
          tag: this.numberArg(instruction.args[1], instruction.span),
          payload: instruction.args.slice(2).map((arg) => this.resolve(frame, arg, instruction.span))
        });
        return;
      case "enum.tag": {
        const value = this.resolve(frame, instruction.args[0], instruction.span);
        if (value.kind !== "enum") {
          this.trap("invalid_enum_tag", "invalid enum tag", instruction.span);
        }
        this.writeDst(frame, instruction, this.i64(BigInt(value.tag)));
        return;
      }
      case "enum.payload":
        this.writeDst(frame, instruction, this.enumPayload(frame, instruction));
        return;
      case "vector.new":
        this.writeDst(frame, instruction, { kind: "vector", elementType: this.stringOperand(instruction.args[0], instruction.span), items: [] });
        return;
      case "vector.push": {
        const vector = this.vectorArg(frame, instruction.args[0], instruction.span);
        vector.items.push(cloneValue(this.resolve(frame, instruction.args[1], instruction.span)));
        return;
      }
      case "vector.get": {
        const vector = this.vectorArg(frame, instruction.args[0], instruction.span);
        const index = this.indexArg(frame, instruction.args[1], instruction.span);
        if (index < 0 || index >= vector.items.length) {
          this.trap("vector_bounds", "vector index out of bounds", instruction.span);
        }
        this.writeDst(frame, instruction, cloneValue(vector.items[index]!));
        return;
      }
      case "vector.set": {
        const vector = this.vectorArg(frame, instruction.args[0], instruction.span);
        const index = this.indexArg(frame, instruction.args[1], instruction.span);
        if (index < 0 || index >= vector.items.length) {
          this.trap("vector_bounds", "vector index out of bounds", instruction.span);
        }
        vector.items[index] = cloneValue(this.resolve(frame, instruction.args[2], instruction.span));
        return;
      }
      case "vector.len":
        this.writeDst(frame, instruction, this.i64(BigInt(this.vectorArg(frame, instruction.args[0], instruction.span).items.length)));
        return;
      case "vector.contains": {
        const vec = this.vectorArg(frame, instruction.args[0], instruction.span);
        const needle = this.resolve(frame, instruction.args[1], instruction.span);
        // Use valuesEqual so f64 non-finite values (NaN, +/-Infinity) follow
        // native fcmp-OEQ semantics: [NaN].contains(NaN) == false.
        const found = vec.items.some((item) => valuesEqual(item, needle));
        this.writeDst(frame, instruction, { kind: "bool", value: found });
        return;
      }
      case "vector.range_slice": {
        const vec = this.vectorArg(frame, instruction.args[0], instruction.span);
        const start = this.indexArg(frame, instruction.args[1], instruction.span);
        const end = this.indexArg(frame, instruction.args[2], instruction.span);
        if (start < 0 || end < start || end > vec.items.length) {
          this.trap("vector_bounds", "vector slice out of bounds", instruction.span);
        }
        this.writeDst(frame, instruction, {
          kind: "vector",
          elementType: vec.elementType,
          items: vec.items.slice(start, end).map(cloneValue),
        });
        return;
      }
      case "string.concat":
        this.writeDst(frame, instruction, {
          kind: "string",
          value: this.stringValue(frame, instruction.args[0], instruction.span) + this.stringValue(frame, instruction.args[1], instruction.span)
        });
        return;
      case "string.len":
        this.writeDst(frame, instruction, this.i64(BigInt([...this.stringValue(frame, instruction.args[0], instruction.span)].length)));
        return;
      case "string.slice": {
        const value = this.stringValue(frame, instruction.args[0], instruction.span);
        const chars = [...value];
        const start = this.indexArg(frame, instruction.args[1], instruction.span);
        const end = this.indexArg(frame, instruction.args[2], instruction.span);
        if (start < 0 || end < start || end > chars.length) {
          this.trap("string_bounds", "string slice out of bounds", instruction.span);
        }
        this.writeDst(frame, instruction, { kind: "string", value: chars.slice(start, end).join("") });
        return;
      }
      case "regex.compile":
        this.writeDst(frame, instruction, this.compileRegex(this.stringValue(frame, instruction.args[0], instruction.span), instruction.span));
        return;
      case "regex.is_match":
        this.writeDst(frame, instruction, { kind: "bool", value: this.regexArg(frame, instruction.args[0], instruction.span).regex.test(this.stringValue(frame, instruction.args[1], instruction.span)) });
        return;
      case "regex.find": {
        const regex = this.regexArg(frame, instruction.args[0], instruction.span).regex;
        const found = regex.exec(this.stringValue(frame, instruction.args[1], instruction.span))?.[0] ?? "";
        this.writeDst(frame, instruction, { kind: "string", value: found });
        return;
      }
      case "regex.replace": {
        const regex = this.regexArg(frame, instruction.args[0], instruction.span).regex;
        const input = this.stringValue(frame, instruction.args[1], instruction.span);
        const replacement = this.stringValue(frame, instruction.args[2], instruction.span);
        this.writeDst(frame, instruction, { kind: "string", value: input.replace(regex, replacement) });
        return;
      }
      case "regex.free":
        return;
      case "panic":
        this.fail("panic", "panic", this.stringValue(frame, instruction.args[0], instruction.span), instruction.span);
        return;
      case "trap":
        this.trap(this.trapKind(instruction.args[0], instruction.span), trapMessage(this.trapKind(instruction.args[0], instruction.span)), instruction.span);
        return;
      default:
        this.unsupported(`unsupported instruction ${instruction.op}`, instruction.span);
    }
  }

  private executeTerminator(
    frame: Frame,
    terminator: Terminator,
    blocks: Map<string, Frame["fn"]["blocks"][number]>
  ): { returned: true; value: VmValue } | { returned: false; block: Frame["fn"]["blocks"][number] } {
    switch (terminator.op) {
      case "br":
        return { returned: false, block: this.branchTarget(frame, terminator.target, terminator.args, blocks, terminator.span) };
      case "br_if": {
        const condition = this.resolve(frame, terminator.condition, terminator.span);
        if (condition.kind !== "bool") {
          this.trap("invalid_block", "branch condition is not bool", terminator.span);
        }
        return {
          returned: false,
          block: this.branchTarget(
            frame,
            condition.value ? terminator.target : terminator.else_target,
            terminator.args,
            blocks,
            terminator.span
          )
        };
      }
      case "return":
        return { returned: true, value: terminator.args.length > 0 ? cloneValue(this.resolve(frame, terminator.args[0], terminator.span)) : UNIT };
      case "tail_call":
        this.unsupported("Unsupported::M4_DEFERRED: tail_call", terminator.span);
        break;
      case "trap":
        this.trap(terminator.trap_kind ?? "internal_error", trapMessage(terminator.trap_kind ?? "internal_error"), terminator.span);
        break;
      case "unreachable":
        this.trap("internal_error", "unreachable bytecode executed", terminator.span);
        break;
    }
    throw new VmHalt(this.trace.status);
  }

  private createChannel(capacity: number, origin: string | null, spanRef: string | null): ChannelCell {
    if (capacity <= 0) {
      this.trap("invalid_call", "channel capacity must be positive", spanRef);
    }
    const id = this.scheduler.ids.channel();
    const channel: ChannelCell = { id, capacity, queue: [], closed: false };
    this.channels.set(id, channel);
    this.trace.allocateId("channel", id, null);
    this.trace.snapshot("channel-new", { channel_id: id, capacity, origin }, this.trace.span(spanRef));
    return channel;
  }

  private channelSend(channel: ChannelCell, value: VmValue, spanRef: string | null): void {
    if (channel.closed) {
      this.trap("invalid_call", `channel ${channel.id} is closed`, spanRef);
    }
    if (channel.queue.length >= channel.capacity) {
      this.drainTasks();
      this.scheduler.drain();
    }
    if (channel.queue.length >= channel.capacity) {
      this.trap("invalid_call", `M6_CHANNEL_SEND_WOULD_BLOCK: channel ${channel.id} send would block`, spanRef);
    }
    channel.queue.push(cloneValue(value));
    this.trace.snapshot("channel-send", {
      channel_id: channel.id,
      depth: channel.queue.length,
      capacity: channel.capacity,
      value: toTraceValue(value)
    }, this.trace.span(spanRef));
  }

  private channelRecv(channel: ChannelCell, spanRef: string | null): VmValue {
    if (channel.queue.length === 0 && !channel.closed) {
      this.drainTasks();
      this.scheduler.drain();
    }
    if (channel.queue.length === 0) {
      this.trap("invalid_call", channel.closed ? `channel ${channel.id} is closed and drained` : `channel ${channel.id} recv would block`, spanRef);
    }
    const value = channel.queue.shift()!;
    this.trace.snapshot("channel-recv", {
      channel_id: channel.id,
      depth: channel.queue.length,
      closed: channel.closed,
      value: toTraceValue(value)
    }, this.trace.span(spanRef));
    return cloneValue(value);
  }

  private channelClose(channel: ChannelCell, spanRef: string | null): void {
    if (channel.closed) {
      this.trap("invalid_call", `channel ${channel.id} is already closed`, spanRef);
    }
    channel.closed = true;
    this.trace.snapshot("channel-close", {
      channel_id: channel.id,
      drained_after_close: channel.queue.length
    }, this.trace.span(spanRef));
  }

  private spawnTask(functionId: string, args: VmValue[], scopeId: string | null): VmValue {
    const id = this.scheduler.ids.task();
    const task: TaskCell = {
      id,
      functionId,
      args: args.map(cloneValue),
      status: "pending",
      value: UNIT,
      scopeId
    };
    this.tasks.set(id, task);
    this.readyTasks.push(id);
    if (scopeId) {
      this.scopes.find((scope) => scope.id === scopeId)?.children.add(id);
    }
    this.trace.allocateId("task", id, this.scheduler.activeActor ?? "actor:root");
    this.trace.snapshot("async-spawn", { task_id: id, function_id: functionId, scope_id: scopeId });
    return { kind: "task", id };
  }

  private awaitTask(task: TaskCell, spanRef: string | null): VmValue {
    if (task.status === "cancelled") {
      this.trap("invalid_call", `task ${task.id} was cancelled`, spanRef);
    }
    while (task.status !== "completed") {
      if (!this.runNextTask()) {
        this.trap("invalid_call", `task ${task.id} cannot make progress`, spanRef);
      }
    }
    return cloneValue(task.value);
  }

  private drainTasks(): void {
    while (this.runNextTask()) {
      continue;
    }
  }

  private runNextTask(): boolean {
    if (this.taskDriverDepth > 0) {
      this.trace.snapshot("async-trampoline", {
        depth: this.taskDriverDepth,
        ready_tasks: this.readyTasks.length
      });
    }
    if (this.taskDriverDepth >= 256) {
      this.trap("budget_exhausted", "M6_TASK_DRIVER_TRAMPOLINE_DEPTH_EXCEEDED", null);
    }
    const taskId = this.readyTasks.shift();
    if (!taskId) {
      return false;
    }
    const task = this.tasks.get(taskId);
    if (!task || task.status !== "pending") {
      return true;
    }
    this.taskDriverDepth += 1;
    try {
      this.commitOrExhaust();
      task.status = "running";
      this.trace.stepCommitted("task.scheduler-step");
      this.trace.snapshot("async-resume", { task_id: task.id, function_id: task.functionId });
      const returned = this.invoke(this.functionFor(task.functionId, null), task.args.map(cloneValue));
      task.status = "completed";
      task.value = cloneValue(returned);
      this.trace.snapshot("async-complete", { task_id: task.id, status: "completed", value: toTraceValue(returned) });
      return true;
    } finally {
      this.taskDriverDepth -= 1;
    }
  }

  private cancelTask(task: TaskCell, reason: string): void {
    if (task.status === "completed" || task.status === "cancelled") {
      return;
    }
    task.status = "cancelled";
    const readyIndex = this.readyTasks.indexOf(task.id);
    if (readyIndex >= 0) {
      this.readyTasks.splice(readyIndex, 1);
    }
    this.trace.snapshot("async-complete", { task_id: task.id, status: "cancelled", reason });
  }

  private enterScope(): void {
    this.scopeCounter += 1;
    const scope: ScopeCell = { id: `scope:s${this.scopeCounter}`, children: new Set(), cancelled: false };
    this.scopes.push(scope);
    this.trace.snapshot("scope-enter", { scope_id: scope.id });
  }

  private currentScope(): ScopeCell {
    const scope = this.scopes[this.scopes.length - 1];
    if (!scope) {
      this.trap("invalid_call", "scope operation requires an active scope", null);
    }
    return scope;
  }

  private cancelScope(reason: string): void {
    const scope = this.currentScope();
    scope.cancelled = true;
    for (const taskId of scope.children) {
      const task = this.tasks.get(taskId);
      if (task) {
        this.cancelTask(task, reason);
      }
    }
    this.trace.snapshot("scope-cancel", { scope_id: scope.id, reason, children: [...scope.children] });
  }

  private exitScope(): void {
    const scope = this.currentScope();
    for (const taskId of scope.children) {
      const task = this.tasks.get(taskId);
      if (task && task.status !== "completed") {
        this.cancelTask(task, "scope.exit");
        scope.cancelled = true;
      }
    }
    this.trace.snapshot("scope-exit", {
      scope_id: scope.id,
      cancelled: scope.cancelled,
      children: [...scope.children].map((taskId) => ({ task_id: taskId, status: this.tasks.get(taskId)?.status ?? "unknown" }))
    });
    this.scopes.pop();
  }

  private selectPoll(frame: Frame, instruction: Instruction): VmValue {
    const arms = this.selectArms(frame, instruction);
    const recvReady = arms.filter((arm) => arm.kind === "recv" && arm.channel && arm.channel.queue.length > 0);
    const timerArms = arms.filter((arm) => arm.kind === "timer");
    const ready = recvReady.length > 0 ? recvReady : timerArms;
    if (ready.length === 0) {
      this.trap("invalid_call", "select has no ready arm and no timer", instruction.span);
    }
    this.selectCounter += 1;
    const selected = ready[selectTieIndex(this.trace.replay.seed, this.selectCounter, ready.length)]!;
    this.trace.snapshot("select-resolve", {
      select_id: `select:${this.selectCounter}`,
      selected: selected.label,
      ready: ready.map((arm) => arm.label)
    }, this.trace.span(instruction.span));
    for (const arm of arms) {
      if (arm !== selected) {
        this.trace.snapshot("select-loser-cleanup", {
          select_id: `select:${this.selectCounter}`,
          arm: arm.label,
          kind: arm.kind
        }, this.trace.span(instruction.span));
      }
    }
    if (selected.kind === "timer") {
      this.sleepVirtual(selected.afterMs ?? 0, instruction.span);
      return { kind: "string", value: selected.label };
    }
    return this.channelRecv(selected.channel!, instruction.span);
  }

  private selectArms(frame: Frame, instruction: Instruction): SelectArm[] {
    const metadata = metadataRecord(instruction.metadata);
    const rawArms = Array.isArray(metadata.arms) ? metadata.arms : [];
    if (rawArms.length === 0) {
      this.trap("invalid_call", "select requires at least one arm", instruction.span);
    }
    return rawArms.map((raw, index) => {
      const arm = metadataRecord(raw);
      const label = typeof arm.label === "string" ? arm.label : `arm:${index}`;
      if (arm.kind === "recv") {
        const argIndex = typeof arm.arg === "number" ? arm.arg : index;
        return { kind: "recv", label, index, channel: this.channelArg(frame, instruction.args[argIndex], instruction.span) };
      }
      if (arm.kind === "timer") {
        const afterMs = typeof arm.after_ms === "number" ? arm.after_ms : 0;
        return { kind: "timer", label, index, afterMs };
      }
      this.trap("invalid_call", "unsupported select arm", instruction.span);
    });
  }

  private sleepVirtual(amountMs: number, spanRef: string | null): void {
    if (!Number.isInteger(amountMs) || amountMs < 0) {
      this.trap("invalid_call", "virtual sleep duration must be a non-negative integer", spanRef);
    }
    this.trace.advanceVirtualClock(amountMs, this.trace.span(spanRef));
  }

  private branchTarget(
    frame: Frame,
    target: string | undefined,
    args: Operand[],
    blocks: Map<string, Frame["fn"]["blocks"][number]>,
    span: string | null
  ): Frame["fn"]["blocks"][number] {
    const block = target ? blocks.get(target) : undefined;
    if (!block) {
      this.trap("invalid_block", "invalid branch target", span);
    }
    for (const [index, param] of block.params.entries()) {
      frame.locals.set(param, cloneValue(this.resolve(frame, args[index], span)));
    }
    return block;
  }

  private commitOrExhaust(): void {
    if (this.trace.budgetRemaining === 0) {
      const failure = runtimeFailure("budget_exhausted", "step budget exhausted", "budget_exhausted", null);
      this.trace.fail("budget_exhausted", "budget.exhausted", failure);
      throw new VmHalt("budget_exhausted");
    }
    this.trace.stepCount += 1;
  }

  private callStdlib(frame: Frame, instruction: Instruction): void {
    const symbolId = this.stringOperand(instruction.args[0], instruction.span);
    const symbol = this.stdlibSymbols.get(symbolId);
    if (!symbol) {
      this.trap("invalid_call", "stdlib symbol not found", instruction.span);
    }
    const profile = lookupStdlibProfile(symbol);
    const capability = symbol.capability ? this.capabilities.get(symbol.capability) : undefined;
    if (symbol.admission !== "allowed" || capability?.disposition === "rejected" || capability?.disposition === "reserved") {
      this.unsupportedStdlib(symbol, profile, instruction.span);
    }
    if (!profile || profile.status !== "shim" || !profile.handler) {
      this.unsupportedStdlib(symbol, profile, instruction.span);
    }
    this.applyStdlibShim(profile.handler, frame, instruction, profile);
  }

  private applyStdlibShim(handler: StdlibShimHandler, frame: Frame, instruction: Instruction, profile: StdlibProfileEntry): void {
    switch (handler) {
      case "io.stdout.print":
        this.trace.writeStdout(this.renderStdlibArgs(frame, instruction), this.trace.span(instruction.span));
        return;
      case "io.stdout.println":
        this.trace.writeStdout(`${this.renderStdlibArgs(frame, instruction)}\n`, this.trace.span(instruction.span));
        return;
      case "io.stderr.print":
        this.trace.writeStderr(this.renderStdlibArgs(frame, instruction), this.trace.span(instruction.span));
        return;
      case "io.stderr.println":
        this.trace.writeStderr(`${this.renderStdlibArgs(frame, instruction)}\n`, this.trace.span(instruction.span));
        return;
      case "io.log":
        this.trace.snapshot("page.log", { text: this.renderStdlibArgs(frame, instruction) }, this.trace.span(instruction.span));
        return;
      case "process.exit": {
        const code = instruction.args.length > 1 ? this.indexArg(frame, instruction.args[1], instruction.span) : 0;
        this.trace.exit(code, this.trace.span(instruction.span));
        throw new VmHalt("ok");
      }
      case "time.now":
        this.writeDst(frame, instruction, this.i64(BigInt(this.trace.clockSnapshot().current_ms)));
        return;
      case "time.sleep":
        this.sleepVirtual(this.durationArg(frame, instruction.args[1], instruction.span), instruction.span);
        return;
      case "time.deadline": {
        const now = BigInt(this.trace.clockSnapshot().current_ms);
        const duration = BigInt(this.durationArg(frame, instruction.args[1], instruction.span));
        this.writeDst(frame, instruction, this.checkedI64(now + duration, instruction.span));
        return;
      }
      case "fmt.to_string":
        this.writeDst(frame, instruction, { kind: "string", value: renderStdout(this.resolve(frame, instruction.args[1], instruction.span)) });
        return;
      case "string.len":
        this.writeDst(frame, instruction, this.i64(BigInt([...this.stringValue(frame, instruction.args[1], instruction.span)].length)));
        return;
      case "string.concat":
        this.writeDst(frame, instruction, {
          kind: "string",
          value: this.stringValue(frame, instruction.args[1], instruction.span) + this.stringValue(frame, instruction.args[2], instruction.span)
        });
        return;
      case "string.slice": {
        const value = this.stringValue(frame, instruction.args[1], instruction.span);
        const chars = [...value];
        const start = this.indexArg(frame, instruction.args[2], instruction.span);
        const end = this.indexArg(frame, instruction.args[3], instruction.span);
        if (start < 0 || end < start || end > chars.length) {
          this.trap("string_bounds", "string slice out of bounds", instruction.span);
        }
        this.writeDst(frame, instruction, { kind: "string", value: chars.slice(start, end).join("") });
        return;
      }
      case "math.abs":
        this.writeDst(frame, instruction, this.scalarAbs(this.resolve(frame, instruction.args[1], instruction.span), instruction.span));
        return;
      case "math.min":
        this.writeDst(frame, instruction, this.scalarMinMax(frame, instruction, "min"));
        return;
      case "math.max":
        this.writeDst(frame, instruction, this.scalarMinMax(frame, instruction, "max"));
        return;
      case "math.clamp":
        this.writeDst(frame, instruction, this.scalarClamp(frame, instruction));
        return;
      case "vec.len":
        this.writeDst(frame, instruction, this.i64(BigInt(this.vectorArg(frame, instruction.args[1], instruction.span).items.length)));
        return;
      case "vec.get": {
        const vector = this.vectorArg(frame, instruction.args[1], instruction.span);
        const index = this.indexArg(frame, instruction.args[2], instruction.span);
        if (index < 0 || index >= vector.items.length) {
          this.trap("vector_bounds", "vector index out of bounds", instruction.span);
        }
        this.writeDst(frame, instruction, cloneValue(vector.items[index]!));
        return;
      }
      case "vec.push":
        this.vectorArg(frame, instruction.args[1], instruction.span).items.push(cloneValue(this.resolve(frame, instruction.args[2], instruction.span)));
        return;
      case "regex.compile":
        this.writeDst(frame, instruction, this.compileRegex(this.stringValue(frame, instruction.args[1], instruction.span), instruction.span));
        return;
      case "regex.is_match":
        this.writeDst(frame, instruction, { kind: "bool", value: this.regexArg(frame, instruction.args[1], instruction.span).regex.test(this.stringValue(frame, instruction.args[2], instruction.span)) });
        return;
      case "regex.find": {
        const regex = this.regexArg(frame, instruction.args[1], instruction.span).regex;
        this.writeDst(frame, instruction, { kind: "string", value: regex.exec(this.stringValue(frame, instruction.args[2], instruction.span))?.[0] ?? "" });
        return;
      }
      case "regex.replace": {
        const regex = this.regexArg(frame, instruction.args[1], instruction.span).regex;
        const input = this.stringValue(frame, instruction.args[2], instruction.span);
        const replacement = this.stringValue(frame, instruction.args[3], instruction.span);
        this.writeDst(frame, instruction, { kind: "string", value: input.replace(regex, replacement) });
        return;
      }
      default:
        this.unsupportedStdlib({ id: profile.id, module: profile.module, name: profile.name }, profile, instruction.span);
    }
  }

  private renderStdlibArgs(frame: Frame, instruction: Instruction): string {
    return instruction.args.slice(1).map((arg) => renderStdout(this.resolve(frame, arg, instruction.span))).join(" ");
  }

  private scalarAbs(value: VmValue, span: string | null): VmValue {
    if (value.kind === "i64") {
      if (value.value === I64_MIN) {
        this.trap("integer_overflow", "integer overflow", span);
      }
      return this.i64(value.value < 0n ? -value.value : value.value);
    }
    if (value.kind === "f64") {
      return { kind: "f64", value: Math.abs(value.value) };
    }
    this.trap("invalid_local", "math.abs requires numeric operand", span);
  }

  private scalarMinMax(frame: Frame, instruction: Instruction, mode: "min" | "max"): VmValue {
    const left = this.resolve(frame, instruction.args[1], instruction.span);
    const right = this.resolve(frame, instruction.args[2], instruction.span);
    if (left.kind === "i64" && right.kind === "i64") {
      return this.i64(mode === "min" ? (left.value <= right.value ? left.value : right.value) : (left.value >= right.value ? left.value : right.value));
    }
    if (left.kind === "f64" && right.kind === "f64") {
      return { kind: "f64", value: mode === "min" ? Math.min(left.value, right.value) : Math.max(left.value, right.value) };
    }
    this.trap("invalid_local", `math.${mode} requires matching numeric operands`, instruction.span);
  }

  private scalarClamp(frame: Frame, instruction: Instruction): VmValue {
    const value = this.resolve(frame, instruction.args[1], instruction.span);
    const low = this.resolve(frame, instruction.args[2], instruction.span);
    const high = this.resolve(frame, instruction.args[3], instruction.span);
    if (value.kind === "i64" && low.kind === "i64" && high.kind === "i64") {
      if (low.value > high.value) {
        this.trap("invalid_local", "math.clamp lower bound exceeds upper bound", instruction.span);
      }
      return this.i64(value.value < low.value ? low.value : value.value > high.value ? high.value : value.value);
    }
    if (value.kind === "f64" && low.kind === "f64" && high.kind === "f64") {
      if (low.value > high.value) {
        this.trap("invalid_local", "math.clamp lower bound exceeds upper bound", instruction.span);
      }
      return { kind: "f64", value: Math.min(Math.max(value.value, low.value), high.value) };
    }
    this.trap("invalid_local", "math.clamp requires matching numeric operands", instruction.span);
  }

  private unsupportedStdlib(
    symbol: { id: string; module: string; name: string },
    profile: StdlibProfileEntry | undefined,
    spanRef: string | null
  ): never {
    const diagnostic = unsupportedDiagnosticFor(symbol, profile);
    this.unsupported(`${diagnostic.kind}: ${diagnostic.symbol}`, spanRef, diagnostic);
  }

  private compare(frame: Frame, instruction: Instruction): boolean {
    const left = this.resolve(frame, instruction.args[0], instruction.span);
    const right = this.resolve(frame, instruction.args[1], instruction.span);
    switch (instruction.op) {
      case "cmp.eq":
        // f64 equality mirrors native `fcmp OEQ`; see valuesEqual.
        // All other types use canonical-JSON equality.
        return valuesEqual(left, right);
      case "cmp.ne":
        // f64 inequality mirrors native `fcmp UNE` (unordered not-equal): true
        // when the operands differ or either operand is NaN. JS `!==` matches
        // UNE exactly (`NaN!==NaN` and `NaN!==Infinity` are true).
        if (left.kind === "f64" && right.kind === "f64") {
          return left.value !== right.value;
        }
        return canonicalComparable(left) !== canonicalComparable(right);
      case "cmp.lt":
        return compareScalar(this.scalarComparable(left, instruction.span), this.scalarComparable(right, instruction.span), "<", (message) =>
          this.trap("invalid_local", message, instruction.span)
        );
      case "cmp.le":
        return compareScalar(this.scalarComparable(left, instruction.span), this.scalarComparable(right, instruction.span), "<=", (message) =>
          this.trap("invalid_local", message, instruction.span)
        );
      case "cmp.gt":
        return compareScalar(this.scalarComparable(left, instruction.span), this.scalarComparable(right, instruction.span), ">", (message) =>
          this.trap("invalid_local", message, instruction.span)
        );
      case "cmp.ge":
        return compareScalar(this.scalarComparable(left, instruction.span), this.scalarComparable(right, instruction.span), ">=", (message) =>
          this.trap("invalid_local", message, instruction.span)
        );
      default:
        return false;
    }
  }

  private scalarComparable(value: VmValue, span: string | null): ScalarComparable {
    if (value.kind === "i64") {
      return { kind: "i64", value: value.value };
    }
    if (value.kind === "f64") {
      return { kind: "f64", value: value.value };
    }
    if (value.kind === "string") {
      return { kind: "string", value: value.value };
    }
    this.trap("invalid_local", "comparison requires scalar operands", span);
  }

  private recordGet(frame: Frame, instruction: Instruction): VmValue {
    const record = this.resolve(frame, instruction.args[0], instruction.span);
    if (record.kind !== "record") {
      this.trap("invalid_record_field", "invalid record value", instruction.span);
    }
    const field = instruction.args[1]?.value;
    const index =
      typeof field === "number"
        ? field
        : this.records.get(record.typeId)?.fields.find((layout) => layout.name === field)?.index ?? -1;
    if (index < 0 || index >= record.fields.length) {
      this.trap("invalid_record_field", "invalid record field", instruction.span);
    }
    return cloneValue(record.fields[index]!);
  }

  private enumPayload(frame: Frame, instruction: Instruction): VmValue {
    const value = this.resolve(frame, instruction.args[0], instruction.span);
    if (value.kind !== "enum") {
      this.trap("invalid_enum_tag", "invalid enum payload", instruction.span);
    }
    if (!this.enums.has(value.typeId)) {
      this.trap("invalid_enum_tag", "invalid enum tag", instruction.span);
    }
    const index = this.numberArg(instruction.args[1], instruction.span);
    if (index < 0 || index >= value.payload.length) {
      this.trap("invalid_enum_tag", "invalid enum payload", instruction.span);
    }
    return cloneValue(value.payload[index]!);
  }

  private machineNew(instruction: Instruction): VmValue {
    const layoutId = this.stringOperand(instruction.args[0], instruction.span);
    const layout = this.machines.get(layoutId);
    if (!layout) {
      this.trap("invalid_call", "machine layout not found", instruction.span);
    }
    const state = this.messageName(instruction.args[1], instruction.span);
    if (!layout.states.includes(state)) {
      this.trap("invalid_call", `machine ${layout.name} has no state ${state}`, instruction.span);
    }
    return { kind: "record", typeId: layoutId, fields: [{ kind: "string", value: state }] };
  }

  private machineState(frame: Frame, instruction: Instruction): VmValue {
    const machine = this.resolve(frame, instruction.args[0], instruction.span);
    return { kind: "string", value: this.machineCurrentState(machine, instruction.span) };
  }

  private machineStep(frame: Frame, instruction: Instruction): VmValue {
    const machine = this.resolve(frame, instruction.args[0], instruction.span);
    if (machine.kind !== "record") {
      this.trap("invalid_local", "machine.step requires a machine value", instruction.span);
    }
    const current = this.machineCurrentState(machine, instruction.span);
    const event = this.messageName(instruction.args[1], instruction.span);
    const layout = this.machines.get(machine.typeId);
    if (!layout) {
      this.trap("invalid_call", "machine layout not found", instruction.span);
    }
    const transition = layout.transitions.find((candidate) => candidate.from === current && candidate.event === event);
    if (!transition) {
      this.trap("invalid_call", `machine ${layout.name} has no transition for ${event} in state ${current}`, instruction.span);
    }
    this.trace.snapshot("machine.step", {
      machine: layout.id,
      event,
      from: current,
      to: transition.to
    }, this.trace.span(instruction.span));
    return { kind: "record", typeId: machine.typeId, fields: [{ kind: "string", value: transition.to }] };
  }

  private machineCurrentState(machine: VmValue, span: string | null): string {
    if (machine.kind !== "record" || machine.fields[0]?.kind !== "string") {
      this.trap("invalid_local", "expected a machine value", span);
    }
    return machine.fields[0].value;
  }

  private checkedDiv(frame: Frame, left: Operand | undefined, right: Operand | undefined, span: string | null): bigint {
    const lhs = this.i64Arg(frame, left, span);
    const rhs = this.i64Arg(frame, right, span);
    if (rhs === 0n) {
      this.trap("divide_by_zero", "divide by zero", span);
    }
    if (lhs === I64_MIN && rhs === -1n) {
      this.trap("integer_overflow", "integer overflow", span);
    }
    return lhs / rhs;
  }

  private checkedRem(frame: Frame, left: Operand | undefined, right: Operand | undefined, span: string | null): bigint {
    const lhs = this.i64Arg(frame, left, span);
    const rhs = this.i64Arg(frame, right, span);
    if (rhs === 0n) {
      this.trap("divide_by_zero", "divide by zero", span);
    }
    return lhs % rhs;
  }

  private compileRegex(source: string, span: string | null): VmValue {
    try {
      return { kind: "regex", source, regex: new RegExp(source) };
    } catch {
      this.trap("regex_compile", "regex compile failed", span);
    }
  }

  private resolve(frame: Frame, operand: Operand | undefined, span: string | null): VmValue {
    if (!operand) {
      this.trap("invalid_local", "missing operand", span);
    }
    switch (operand.kind) {
      case "local":
        return this.readLocal(frame, this.localId(operand, span), span);
      case "literal":
      case "constant":
        return valueFromLiteral(operand.value);
      default:
        return valueFromLiteral(operand.value);
    }
  }

  private readLocal(frame: Frame, localId: string, span: string | null): VmValue {
    const value = frame.locals.get(localId);
    if (!value) {
      this.trap("invalid_local", `invalid local ${localId}`, span);
    }
    return value;
  }

  private writeDst(frame: Frame, instruction: Instruction, value: VmValue): void {
    if (instruction.dst !== null) {
      frame.locals.set(instruction.dst, value);
    }
  }

  private localId(operand: Operand | undefined, span: string | null): string {
    const value = this.stringOperand(operand, span);
    if (!value.startsWith("local:")) {
      this.trap("invalid_local", "operand is not a local id", span);
    }
    return value;
  }

  private functionFor(id: string, span: string | null): BytecodeFunction {
    const fn = this.functions.get(id);
    if (!fn) {
      this.trap("invalid_call", "function not found", span);
    }
    return fn;
  }

  private numberArg(operand: Operand | undefined, span: string | null): number {
    if (typeof operand?.value !== "number") {
      this.trap("invalid_local", "expected numeric operand", span);
    }
    return operand.value;
  }

  private numericCast(frame: Frame, instruction: Instruction): VmValue {
    const value = this.resolve(frame, instruction.args[0], instruction.span);
    const from = this.numericCastType(this.stringOperand(instruction.args[1], instruction.span), instruction.span);
    const to = this.numericCastType(this.stringOperand(instruction.args[2], instruction.span), instruction.span);

    let integerValue: bigint | null = null;
    let floatValue: number | null = null;
    if (from.kind === "integer") {
      if (value.kind !== "i64") {
        this.trap("invalid_local", "numeric.cast integer source is not an integer", instruction.span);
      }
      integerValue = this.normalizeInteger(value.value, from);
    } else if (from.kind === "float") {
      if (value.kind !== "f64") {
        this.trap("invalid_local", "numeric.cast float source is not a float", instruction.span);
      }
      floatValue = from.bits === 32 ? Math.fround(value.value) : value.value;
    } else if (from.kind === "bool") {
      if (value.kind !== "bool") {
        this.trap("invalid_local", "numeric.cast bool source is not a bool", instruction.span);
      }
      integerValue = value.value ? 1n : 0n;
    } else {
      if (value.kind !== "string" || [...value.value].length !== 1) {
        this.trap("invalid_local", "numeric.cast char source is not one Unicode scalar", instruction.span);
      }
      integerValue = BigInt(value.value.codePointAt(0)!);
    }

    if (to.kind === "bool") {
      if (integerValue === null || from.kind !== "integer") {
        this.trap("invalid_local", "numeric.cast to bool requires an integer source", instruction.span);
      }
      return { kind: "bool", value: integerValue !== 0n };
    }
    if (to.kind === "integer") {
      if (integerValue !== null) {
        return this.i64(this.normalizeInteger(integerValue, to));
      }
      if (floatValue !== null) {
        return this.i64(this.saturatingFloatToInteger(floatValue, to));
      }
      this.trap("invalid_local", "numeric.cast has no numeric source value", instruction.span);
    }
    if (to.kind === "float") {
      const number = integerValue !== null ? Number(integerValue) : floatValue;
      if (number === null) {
        this.trap("invalid_local", "numeric.cast has no numeric source value", instruction.span);
      }
      return this.f64(to.bits === 32 ? Math.fround(number) : number);
    }
    this.trap("invalid_local", "numeric.cast cannot target char", instruction.span);
  }

  private numericCastType(name: string, span: string | null): NumericCastType {
    switch (name) {
      case "i8":
        return { kind: "integer", bits: 8, signed: true };
      case "i16":
        return { kind: "integer", bits: 16, signed: true };
      case "i32":
        return { kind: "integer", bits: 32, signed: true };
      case "i64":
        return { kind: "integer", bits: 64, signed: true };
      // The sandbox parity oracle is 64-bit host-native `hew run`, not wasm32.
      case "isize":
        return { kind: "integer", bits: 64, signed: true };
      case "u8":
        return { kind: "integer", bits: 8, signed: false };
      case "u16":
        return { kind: "integer", bits: 16, signed: false };
      case "u32":
        return { kind: "integer", bits: 32, signed: false };
      case "u64":
        return { kind: "integer", bits: 64, signed: false };
      case "usize":
        return { kind: "integer", bits: 64, signed: false };
      case "f32":
        return { kind: "float", bits: 32 };
      case "f64":
        return { kind: "float", bits: 64 };
      case "bool":
        return { kind: "bool" };
      case "char":
        return { kind: "char" };
      default:
        this.trap("invalid_local", `numeric.cast has unknown type ${name}`, span);
    }
  }

  private normalizeInteger(value: bigint, ty: IntegerCastType): bigint {
    return ty.signed ? BigInt.asIntN(ty.bits, value) : BigInt.asUintN(ty.bits, value);
  }

  private saturatingFloatToInteger(value: number, ty: IntegerCastType): bigint {
    if (Number.isNaN(value)) {
      return 0n;
    }
    const bits = BigInt(ty.bits);
    const min = ty.signed ? -(1n << (bits - 1n)) : 0n;
    const max = ty.signed ? (1n << (bits - 1n)) - 1n : (1n << bits) - 1n;
    const upperExclusive = ty.signed ? 2 ** (ty.bits - 1) : 2 ** ty.bits;
    if (value <= Number(min)) {
      return min;
    }
    if (value >= upperExclusive) {
      return max;
    }
    return BigInt(Math.trunc(value));
  }

  private capacityArg(frame: Frame, operand: Operand | undefined, span: string | null): number {
    if (!operand) {
      return 1;
    }
    if (typeof operand.value === "number" && Number.isInteger(operand.value)) {
      return operand.value;
    }
    return this.indexArg(frame, operand, span);
  }

  private durationArg(frame: Frame, operand: Operand | undefined, span: string | null): number {
    if (typeof operand?.value === "number" && Number.isInteger(operand.value)) {
      return operand.value;
    }
    return this.indexArg(frame, operand, span);
  }

  private functionOperand(operand: Operand | undefined, span: string | null): string {
    const value = this.stringOperand(operand, span);
    if (!value.startsWith("fn:")) {
      this.trap("invalid_call", "expected function operand", span);
    }
    return value;
  }

  private stringArg(operand: Operand | undefined, span: string | null): string {
    if (typeof operand?.value !== "string") {
      this.trap("invalid_local", "expected string operand", span);
    }
    return operand.value;
  }

  private stringOperand(operand: Operand | undefined, span: string | null): string {
    if (typeof operand?.value !== "string") {
      this.trap("invalid_local", "expected string operand", span);
    }
    return operand.value;
  }

  private stringValue(frame: Frame, operand: Operand | undefined, span: string | null): string {
    const value = this.resolve(frame, operand, span);
    if (value.kind === "string") {
      return value.value;
    }
    return renderStdout(value);
  }

  private i64Arg(frame: Frame, operand: Operand | undefined, span: string | null): bigint {
    const value = this.resolve(frame, operand, span);
    if (value.kind !== "i64") {
      this.trap("invalid_local", "expected i64 operand", span);
    }
    return value.value;
  }

  private f64Arg(frame: Frame, operand: Operand | undefined, span: string | null): number {
    const value = this.resolve(frame, operand, span);
    if (value.kind !== "f64") {
      this.trap("invalid_local", "expected f64 operand", span);
    }
    return value.value;
  }

  private f32Arg(frame: Frame, operand: Operand | undefined, span: string | null): number {
    return Math.fround(this.f64Arg(frame, operand, span));
  }

  private indexArg(frame: Frame, operand: Operand | undefined, span: string | null): number {
    const value = this.i64Arg(frame, operand, span);
    if (value < BigInt(Number.MIN_SAFE_INTEGER) || value > BigInt(Number.MAX_SAFE_INTEGER)) {
      this.trap("invalid_local", "index is outside sandbox safe range", span);
    }
    return Number(value);
  }

  private vectorArg(frame: Frame, operand: Operand | undefined, span: string | null): Extract<VmValue, { kind: "vector" }> {
    const value = this.resolve(frame, operand, span);
    if (value.kind !== "vector") {
      this.trap("invalid_local", "expected vector operand", span);
    }
    return value;
  }

  private channelArg(frame: Frame, operand: Operand | undefined, span: string | null): ChannelCell {
    const value = this.resolve(frame, operand, span);
    if (value.kind !== "channel") {
      this.trap("invalid_local", "expected channel operand", span);
    }
    const channel = this.channels.get(value.id);
    if (!channel) {
      this.trap("invalid_call", `channel ${value.id} not found`, span);
    }
    return channel;
  }

  private taskArg(frame: Frame, operand: Operand | undefined, span: string | null): TaskCell {
    const value = this.resolve(frame, operand, span);
    if (value.kind !== "task") {
      this.trap("invalid_local", "expected task operand", span);
    }
    const task = this.tasks.get(value.id);
    if (!task) {
      this.trap("invalid_call", `task ${value.id} not found`, span);
    }
    return task;
  }

  private streamChannelArg(frame: Frame, operand: Operand | undefined, span: string | null): ChannelCell {
    const value = this.resolve(frame, operand, span);
    if (value.kind !== "stream" && value.kind !== "sink" && value.kind !== "duplex") {
      this.trap("invalid_local", "expected stream operand", span);
    }
    const channel = this.channels.get(value.channelId);
    if (!channel) {
      this.trap("invalid_call", `stream channel ${value.channelId} not found`, span);
    }
    return channel;
  }

  private streamReadableArg(frame: Frame, operand: Operand | undefined, span: string | null): ChannelCell {
    const value = this.resolve(frame, operand, span);
    if (value.kind !== "stream" && value.kind !== "duplex") {
      this.trap("invalid_local", "expected readable stream operand", span);
    }
    const channel = this.channels.get(value.channelId);
    if (!channel) {
      this.trap("invalid_call", `stream channel ${value.channelId} not found`, span);
    }
    return channel;
  }

  private streamWritableArg(frame: Frame, operand: Operand | undefined, span: string | null): ChannelCell {
    const value = this.resolve(frame, operand, span);
    if (value.kind !== "sink" && value.kind !== "duplex") {
      this.trap("invalid_local", "expected writable stream operand", span);
    }
    const channel = this.channels.get(value.channelId);
    if (!channel) {
      this.trap("invalid_call", `stream channel ${value.channelId} not found`, span);
    }
    return channel;
  }

  private regexArg(frame: Frame, operand: Operand | undefined, span: string | null): Extract<VmValue, { kind: "regex" }> {
    const value = this.resolve(frame, operand, span);
    if (value.kind !== "regex") {
      this.trap("invalid_local", "expected regex operand", span);
    }
    return value;
  }

  private bigintArg(operand: Operand | undefined, span: string | null): bigint {
    if (typeof operand?.value === "number" && Number.isInteger(operand.value)) {
      return BigInt(operand.value);
    }
    if (typeof operand?.value === "string" && /^-?[0-9]+$/.test(operand.value)) {
      return BigInt(operand.value);
    }
    this.trap("invalid_local", "expected i64 literal operand", span);
  }

  private i64(value: bigint): VmValue {
    return { kind: "i64", value };
  }

  private f64(value: number): VmValue {
    return { kind: "f64", value };
  }

  private f32(value: number): VmValue {
    return this.f64(Math.fround(value));
  }

  private checkedI64(value: bigint, span: string | null): VmValue {
    if (value < I64_MIN || value > I64_MAX) {
      this.trap("integer_overflow", "integer overflow", span);
    }
    return this.i64(value);
  }

  private shiftCount(frame: Frame, operand: Operand | undefined, span: string | null): bigint {
    const value = this.i64Arg(frame, operand, span);
    if (value < 0n || value >= 64n) {
      this.trap("shift_out_of_range", "shift out of range", span);
    }
    return value;
  }

  private trapKind(operand: Operand | undefined, span: string | null): TrapKind {
    const value = this.stringOperand(operand, span);
    return value as TrapKind;
  }

  private messageName(operand: Operand | undefined, span: string | null): string {
    const value = this.stringOperand(operand, span);
    return value.startsWith("sym:") ? value.slice("sym:".length) : value;
  }

  private actorPair(frame: Frame, instruction: Instruction): [VmValue, VmValue] {
    if (instruction.args.length === 1) {
      const active = this.scheduler.activeActor;
      if (!active) {
        this.trap("invalid_call", `${instruction.op} with one actor operand requires an active actor`, instruction.span);
      }
      return [{ kind: "actor", id: active }, this.resolve(frame, instruction.args[0], instruction.span)];
    }
    return [
      this.resolve(frame, instruction.args[0], instruction.span),
      this.resolve(frame, instruction.args[1], instruction.span)
    ];
  }

  private assertSupervisorOpAllowed(instruction: Instruction): void {
    if (this.scheduler.activeLifecycleHook !== null) {
      this.trap("invalid_call", `M6_SUPERVISOR_HOOK_DENIED: ${instruction.op} cannot run from ${this.scheduler.activeLifecycleHook}`, instruction.span);
    }
  }

  private withSchedulerError<T>(instruction: Instruction, callback: () => T): T {
    try {
      return callback();
    } catch (error) {
      if (error instanceof SchedulerRuntimeError) {
        this.raiseSchedulerError(error, instruction.span);
      }
      throw error;
    }
  }

  private raiseSchedulerError(error: SchedulerRuntimeError, spanRef: string | null): never {
    if (error.kind === "unsupported") {
      this.unsupported(error.message, spanRef);
    }
    this.trap(error.trapKind, error.message, spanRef);
  }

  private trap(trapKind: TrapKind, message: string, spanRef: string | null): never {
    const status: RuntimeStatus = trapKind === "panic" ? "panic" : "trap";
    this.fail(status, trapKind, message, spanRef);
  }

  private unsupported(message: string, spanRef: string | null, unsupported?: UnsupportedStdlibDiagnostic): never {
    const failure = runtimeFailure("unsupported", message, "unsupported_instruction", this.trace.span(spanRef), unsupported);
    // When executing inside an actor turn, route through ActorTurnCrash so that
    // the supervisor can catch and restart the actor, rather than halting the whole VM.
    if (this.scheduler.activeActor !== null) {
      throw new ActorTurnCrash(failure);
    }
    this.trace.fail("runtime_failure", "runtime.failure", failure);
    throw new VmHalt("runtime_failure");
  }

  private fail(status: RuntimeStatus, trapKind: TrapKind, message: string, spanRef: string | null): never {
    const kind = status === "panic" ? "panic" : "trap";
    const failure = runtimeFailure(kind, message, trapKind, this.trace.span(spanRef));
    if (this.scheduler.activeActor !== null) {
      throw new ActorTurnCrash(failure);
    }
    this.trace.fail(status, "runtime.failure", failure);
    throw new VmHalt(status);
  }
}

function fixtureIdFromPackage(bytecode: SandboxBytecodePackage): string {
  return bytecode.package_id.replace(/^pkg:/, "");
}

function hasMetadataFlag(instruction: Instruction, key: string): boolean {
  return typeof instruction.metadata === "object" && instruction.metadata !== null && !Array.isArray(instruction.metadata) && instruction.metadata[key] === true;
}

function metadataRecord(value: JsonValue | undefined): Record<string, JsonValue> {
  return typeof value === "object" && value !== null && !Array.isArray(value) ? value : {};
}

function toTraceValue(value: VmValue): JsonValue {
  return toJsonValue(value);
}

function selectTieIndex(seed: number, counter: number, length: number): number {
  if (length <= 0) {
    throw new RangeError("length must be positive");
  }
  let state = BigInt.asUintN(64, BigInt(seed) ^ (BigInt(counter) * 0x9e3779b97f4a7c15n));
  state = BigInt.asUintN(64, (state ^ (state >> 30n)) * 0xbf58476d1ce4e5b9n);
  state = BigInt.asUintN(64, (state ^ (state >> 27n)) * 0x94d049bb133111ebn);
  state = state ^ (state >> 31n);
  return Number(state & 0xffffffffn) % length;
}

function canonicalComparable(value: VmValue): string {
  return canonicalJson(renderComparable(value));
}

/**
 * Structural equality matching native code-gen semantics for f64 operands.
 *
 * f64 pairs use JS `===`, which mirrors LLVM `fcmp OEQ`:
 *   - NaN !== NaN  (OEQ is false for any NaN operand)
 *   - +Infinity !== -Infinity  (sign must match)
 *   - -0.0 === 0.0  (IEEE-754 OEQ)
 *
 * The canonical-JSON path collapses NaN and +/-Infinity to `null` and cannot
 * model these distinctions, so it must not be used for f64 equality.  All
 * other types (i64, string, bool, structural records, vectors) use the
 * canonical-JSON path as before.
 *
 * Used by both `cmp.eq` and `vector.contains` so that the two sites can never
 * drift apart again.
 */
function valuesEqual(a: VmValue, b: VmValue): boolean {
  if (a.kind === "f64" && b.kind === "f64") {
    return a.value === b.value;
  }
  return canonicalComparable(a) === canonicalComparable(b);
}

function renderComparable(value: VmValue): JsonValue {
  switch (value.kind) {
    case "unit":
      return null;
    case "bool":
    case "f64":
    case "string":
      return value.value;
    case "i64":
      return i64ComparableJson(value.value);
    case "actor":
      return { kind: "actor", id: value.id };
    case "supervisor":
      return { kind: "supervisor", id: value.id };
    case "monitor":
      return { kind: "monitor", id: value.id };
    case "reply":
      return { kind: "reply", id: value.id };
    case "channel":
      return { kind: "channel", id: value.id };
    case "task":
      return { kind: "task", id: value.id };
    case "stream":
    case "sink":
    case "duplex":
      return { kind: value.kind, channel_id: value.channelId };
    case "regex":
      return value.source;
    case "record":
      return { type: value.typeId, fields: value.fields.map(renderComparable) };
    case "enum":
      return { type: value.typeId, tag: value.tag, payload: value.payload.map(renderComparable) };
    case "vector":
      return value.items.map(renderComparable);
    case "function":
      return value.id;
  }
}

function compareScalar(left: ScalarComparable, right: ScalarComparable, op: "<" | "<=" | ">" | ">=", trapMixed: (message: string) => never): boolean {
  if (left.kind !== right.kind) {
    trapMixed(`mixed scalar comparison requires matching operand types (${left.kind} vs ${right.kind})`);
  }
  if (left.kind === "string" && right.kind === "string") {
    switch (op) {
      case "<":
        return left.value < right.value;
      case "<=":
        return left.value <= right.value;
      case ">":
        return left.value > right.value;
      case ">=":
        return left.value >= right.value;
    }
  }
  if (left.kind === "i64" && right.kind === "i64") {
    switch (op) {
      case "<":
        return left.value < right.value;
      case "<=":
        return left.value <= right.value;
      case ">":
        return left.value > right.value;
      case ">=":
        return left.value >= right.value;
    }
  }
  const lhs = left.value;
  const rhs = right.value;
  switch (op) {
    case "<":
      return lhs < rhs;
    case "<=":
      return lhs <= rhs;
    case ">":
      return lhs > rhs;
    case ">=":
      return lhs >= rhs;
  }
}

function actorLayout(value: unknown): ActorLayout {
  if (!isRecord(value) || typeof value.id !== "string" || typeof value.name !== "string") {
    throw new Error("invalid actor layout");
  }
  const stateFields = Array.isArray(value.state_fields)
    ? value.state_fields.map((field, index) => {
        if (!isRecord(field) || typeof field.name !== "string" || typeof field.type !== "string") {
          throw new Error("invalid actor state field layout");
        }
        return { name: field.name, type: field.type, index: typeof field.index === "number" ? field.index : index };
      })
    : [];
  const handlers = Array.isArray(value.handlers)
    ? value.handlers.map((handler) => {
        if (!isRecord(handler) || typeof handler.name !== "string" || typeof handler.function !== "string") {
          throw new Error("invalid actor handler layout");
        }
        return { name: handler.name, function: handler.function };
      })
    : [];
  const maxMailbox = typeof value.max_mailbox === "number" ? value.max_mailbox : undefined;
  const maxHeap = typeof value.max_heap === "number" ? value.max_heap : undefined;
  return { id: value.id, name: value.name, state_fields: stateFields, handlers, max_mailbox: maxMailbox, max_heap: maxHeap };
}

function machineLayout(value: unknown): MachineLayout {
  if (!isRecord(value) || typeof value.id !== "string" || typeof value.name !== "string") {
    throw new Error("invalid machine layout");
  }
  const states = Array.isArray(value.states) ? value.states.filter((state): state is string => typeof state === "string") : [];
  const events = Array.isArray(value.events) ? value.events.filter((event): event is string => typeof event === "string") : [];
  const transitions = Array.isArray(value.transitions)
    ? value.transitions.map((raw) => {
        if (!isRecord(raw) || typeof raw.event !== "string" || typeof raw.from !== "string" || typeof raw.to !== "string") {
          throw new Error("invalid machine transition");
        }
        return { event: raw.event, from: raw.from, to: raw.to };
      })
    : [];
  return { id: value.id, name: value.name, states, events, transitions };
}

function supervisorLayout(value: unknown): SupervisorLayout {
  if (!isRecord(value) || typeof value.id !== "string" || typeof value.name !== "string") {
    throw new Error("invalid supervisor layout");
  }
  const strategy = supervisorStrategy(typeof value.strategy === "string" ? value.strategy : "one_for_one");
  const restartIntensity =
    typeof value.restart_intensity === "number"
      ? value.restart_intensity
      : typeof value.max_restarts === "number"
        ? value.max_restarts
        : 1;
  const restartWindowMs =
    typeof value.restart_window_ms === "number"
      ? value.restart_window_ms
      : typeof value.window_ms === "number"
        ? value.window_ms
        : typeof value.window === "number"
          ? value.window * 1000
          : 60_000;
  const rawChildren = Array.isArray(value.child_specs)
    ? value.child_specs
    : Array.isArray(value.children)
      ? value.children
      : [];
  const children = rawChildren.map((raw, index) => {
    if (typeof raw === "string") {
      return { id: `child:${index}`, restart: "permanent" as const, actorLayoutId: raw, initialState: [] };
    }
    if (!isRecord(raw)) {
      throw new Error("invalid supervisor child spec");
    }
    const startSpec = isRecord(raw.start_spec) ? raw.start_spec : raw;
    const actorLayoutId =
      typeof startSpec.actor === "string"
        ? startSpec.actor
        : typeof startSpec.actor_layout === "string"
          ? startSpec.actor_layout
          : typeof startSpec.type_id === "string"
            ? startSpec.type_id
            : "";
    if (!actorLayoutId) {
      throw new Error("invalid supervisor child actor layout");
    }
    const args = Array.isArray(startSpec.args)
      ? startSpec.args
      : Array.isArray(startSpec.initial_state)
        ? startSpec.initial_state
        : [];
    return {
      id: typeof raw.id === "string" ? raw.id : `child:${index}`,
      restart: childRestartClass(typeof raw.restart === "string" ? raw.restart : "permanent"),
      actorLayoutId,
      initialState: args.map(valueFromLiteral)
    };
  });
  return {
    id: value.id,
    name: value.name,
    strategy,
    restartIntensity: layoutNonNegativeInt(restartIntensity),
    restartWindowMs: layoutNonNegativeInt(restartWindowMs),
    children
  };
}

function layoutNonNegativeInt(value: number): number {
  if (!Number.isFinite(value)) {
    return 0;
  }
  if (value >= MAX_LAYOUT_INT32) {
    return MAX_LAYOUT_INT32;
  }
  return Math.max(0, value | 0);
}

function supervisorStrategy(value: string): SupervisorStrategy {
  if (value === "one_for_all" || value === "rest_for_one") {
    return value;
  }
  return "one_for_one";
}

function childRestartClass(value: string): ChildRestartClass {
  if (value === "transient" || value === "temporary") {
    return value;
  }
  return "permanent";
}

function isRecord(value: unknown): value is Record<string, unknown> {
  return typeof value === "object" && value !== null && !Array.isArray(value);
}

function trapMessage(trapKind: TrapKind): string {
  switch (trapKind) {
    case "divide_by_zero":
      return "divide by zero";
    case "integer_overflow":
      return "integer overflow";
    case "shift_out_of_range":
      return "shift out of range";
    case "budget_exhausted":
      return "step budget exhausted";
    case "panic":
      return "panic";
    case "unsupported_instruction":
      return "unsupported instruction";
    default:
      return trapKind.replaceAll("_", " ");
  }
}
