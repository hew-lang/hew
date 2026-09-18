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
import { UNIT, cloneValue, type VmValue } from "../values.js";
import type {
  BlockV1,
  BoundaryOperand,
  CallResult,
  Edge,
  FunctionV1,
  OpV1,
  PackageV1,
  TermV1,
  TrapName,
  ValueDef,
  VariantShape,
} from "./package.js";
import { Mt19937 } from "./mt19937.js";
import {
  ShimFault,
  resolveExternShim,
  resolveRuntimeShim,
  type ShimHost,
} from "./shims.js";
import { admitPackage } from "./validate.js";

const DEFAULT_STEP_BUDGET = 1_000_000;
const PANIC_EXIT = 101;
const NANOS_PER_MS = 1_000_000n;

/// SIR's trap kinds and the process statuses native execution uses
/// (opcodes-v1.md, "Trap kinds"). A panic is its own kind at 101.
const TRAP_EXIT_CODES: Readonly<Record<TrapName, number>> = {
  integer_overflow: 201,
  divide_by_zero: 202,
  shift_out_of_range: 204,
  vector_bounds: 205,
};

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
  | { kind: "panic"; message: string }
  | { kind: "trap"; trap: TrapName; message?: string };

interface Activation {
  fn: FunctionV1;
  block: BlockV1;
  env: Map<number, Ref>;
  places: Map<number, Cell>;
  fault: Fault | null;
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

  const vm = new ExecutorV1(pkg, trace, StdinReader.fromReplay(replay.inputs));
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

  constructor(
    private readonly pkg: PackageV1,
    private readonly trace: TraceBuilder,
    stdin: StdinReader,
  ) {
    this.host = {
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
    );
  }

  /// One instruction or terminator executed is one step, ownership ops
  /// included: the package's ops are what the program does.
  run(): void {
    while (true) {
      const act = this.current;
      for (const op of act.block.ops) {
        this.commitStep();
        this.execute(act, op);
      }
      this.commitStep();
      this.terminate(act, act.block.term);
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
  ): Activation {
    const env = new Map<number, Ref>();
    for (const [at, param] of fn.params.entries()) {
      env.set(param.value, ownedRef(args[at] ?? UNIT));
    }
    const places = new Map<number, Cell>();
    return {
      fn,
      block: this.blockAt(fn, fn.entry),
      env,
      places,
      fault: null,
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
    invalidateRef(this.refOf(act, id));
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
      case "destroy_value":
        this.invalidate(act, op.value);
        return;
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
      case "end_lifetime":
        invalidateRef(this.placeRef(act, op.place));
        return;

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
      case "callable.coerce":
        this.define(act, op.dst, this.read(act, op.source));
        return;

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

      case "call":
      case "indirect.call": {
        const callee =
          term.op === "call"
            ? term.callee
            : functionId(this.boundary(act, term.callee));
        const args = term.args.map((operand) => this.boundary(act, operand));
        this.current = this.activate(
          this.functionAt(callee),
          args,
          act,
          term.result,
          term.normal,
          term.unwind,
        );
        return;
      }
      case "runtime.call": {
        const entry = this.pkg.runtime_families[term.family];
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
        const plan = this.pkg.value_capabilities[term.plan];
        if (!plan) {
          throw new Error(`value.call names no capability plan ${term.plan}`);
        }
        const args = term.args.map((operand) => this.boundary(act, operand));
        if (plan.callable !== undefined) {
          // A user implementation the checker selected: call it.
          this.current = this.activate(
            this.functionAt(plan.callable),
            args,
            act,
            term.result,
            term.normal,
            term.unwind,
          );
          return;
        }
        const derived: VmValue =
          plan.capability === "Eq"
            ? { kind: "bool", value: equals(args[0] ?? UNIT, args[1] ?? UNIT) }
            : { kind: "i64", value: structuralHash(args[0] ?? UNIT) };
        this.completeShim(act, term, derived);
        return;
      }

      case "panic": {
        const message = this.boundary(act, term.message);
        act.fault = { kind: "panic", message: renderMessage(message) };
        this.takeEdge(act, term.cleanup);
        return;
      }
      case "checked_raise":
        act.fault = { kind: "trap", trap: term.trap };
        this.takeEdge(act, term.cleanup);
        return;
      case "trap":
        this.haltWithFault({ kind: "trap", trap: term.trap });
        return;
      case "cleanup.dispatch":
        this.takeEdge(act, act.fault ? term.fault : term.normal);
        return;
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
      this.exitProgram(value);
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

  /// Raise a fault at a call site: enter the named unwind edge, or end the
  /// program when the call names none.
  private raiseFault(act: Activation, fault: Fault, unwind: Edge | null): void {
    if (!unwind) {
      this.haltWithFault(fault);
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
      this.haltWithFault(fault);
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
      case "ValueClose": {
        // Release the named place; a value holding no resource releases to
        // nothing and the activation resumes immediately.
        const place = valueCloseePlace(term.detail);
        if (place !== null) {
          invalidateRef(this.placeRef(act, place));
        }
        break;
      }
      case "Sleep": {
        const duration = term.inputs[0]
          ? this.boundary(act, term.inputs[0])
          : UNIT;
        const nanos = duration.kind === "i64" ? duration.value : 0n;
        this.trace.advanceVirtualClock(Number(nanos / NANOS_PER_MS), null);
        break;
      }
      default:
        throw new Error(
          `suspension kind ${term.kind} resolved no handler after admission`,
        );
    }
    const resume = term.resumes[0];
    if (!resume) {
      throw new Error(`suspension ${term.kind} names no resume edge`);
    }
    if (term.result && term.result !== "never") {
      this.define(act, term.result.value, UNIT);
    }
    this.takeEdge(act, resume);
  }

  // ── exit and faults ──────────────────────────────────────────────────────

  private exitProgram(value: VmValue): never {
    // `entry.exit` names what the entry publishes: nothing, or the integer
    // process status the body returned.
    const exit =
      this.pkg.entry?.exit === "status" && value.kind === "i64"
        ? Number(value.value)
        : 0;
    this.trace.exitCode = exit;
    throw new Halt("ok");
  }

  private haltWithFault(fault: Fault): never {
    const isPanic = fault.kind === "panic";
    const status: RuntimeStatus = isPanic ? "panic" : "trap";
    const trapKind: TrapKind = isPanic ? "panic" : fault.trap;
    const message = isPanic
      ? fault.message
      : (fault.message ?? trapMessage(fault.trap));
    // The parity runner reads `final_state.exit_code` directly, so a failing
    // run publishes the process status native execution would use.
    this.trace.exitCode = isPanic ? PANIC_EXIT : TRAP_EXIT_CODES[fault.trap];
    this.trace.publishExitCode = true;
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
  if (ref.kind === "cell") {
    ref.cell.valid = false;
  }
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

function functionId(value: VmValue): number {
  if (value.kind !== "function") {
    throw new TypeError(
      `indirect.call expected a function value, got ${value.kind}`,
    );
  }
  return Number(value.id);
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

function floatConst(op: Extract<OpV1, { op: "const.float" }>): number {
  switch (op.nonfinite) {
    case "nan":
      return Number.NaN;
    case "inf":
      return Number.POSITIVE_INFINITY;
    case "-inf":
      return Number.NEGATIVE_INFINITY;
    default:
      return op.value ?? 0;
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

/// The derived structural hash. FNV-1a over the canonical rendering keeps equal
/// values hashing equally and the result stable across runs.
function structuralHash(value: VmValue): bigint {
  let hash = 0xcbf29ce484222325n;
  for (const unit of canonical(value)) {
    hash = BigInt.asUintN(
      64,
      (hash ^ BigInt(unit.codePointAt(0) ?? 0)) * 0x100000001b3n,
    );
  }
  return BigInt.asIntN(64, hash);
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
