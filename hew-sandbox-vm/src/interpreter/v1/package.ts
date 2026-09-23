/// TypeScript shape of `hew.sandbox.bytecode.v1`, mirroring
/// `bytecode/sandbox-bytecode-v1.schema.json` and `bytecode/opcodes-v1.md`.
///
/// The package is a projection of verified ownership SIR. Every fact here was
/// proved by SIR verification, so the executor reads these fields; it does not
/// re-derive them from a type, a name or a symbol spelling.

export type Own = "none" | "owned" | "guaranteed";

export type BoundaryDecision =
  | "borrow"
  | "borrow_mut"
  | "copy"
  | "move"
  | "snapshot";

export type TrapName =
  | "integer_overflow"
  | "divide_by_zero"
  | "shift_out_of_range"
  | "vector_bounds";

/// A caret into the compiled source: SIR's debug facts carry a per-site byte
/// offset, so `start` equals `end`. v1 carries no source table, so the trace
/// reports `span: null` and this exists for diagnostics only.
export interface SpanV1 {
  start: number;
  end: number;
}

/// An ordinary operand is a value id and carries no mode: what a use does to
/// its value is the op it feeds (SIR §1.3).
export type Operand = number;

export interface BoundaryOperand {
  value: number;
  decision: BoundaryDecision;
}

export interface Edge {
  to: number;
  args: Operand[];
}

export interface ValueDef {
  value: number;
  own: Own;
}

/// `null` for a unit result, `"never"` for a call that does not return, and a
/// value definition otherwise. The value is defined on the normal edge.
export type CallResult = null | "never" | ValueDef;

export interface AggregateShape {
  id: number;
  name: string;
  fields: string[];
}

export interface VariantCase {
  name: string;
  fields: string[];
}

export interface VariantShape {
  id: number;
  name: string;
  cases: VariantCase[];
}

/// One entry per distinct runtime-call family the instruction stream reaches.
/// `detail` is the serde payload of the `RuntimeCallFamily` variant: an object
/// for a struct variant (`Print`), the bare operation name for a newtype
/// variant (`Vector`, `Map`), and absent for a unit variant.
export interface RuntimeFamilyEntry {
  id: number;
  family: string;
  detail?: unknown;
}

export interface ExternEntry {
  id: number;
  symbol: string;
}

/// A semantic storage location. Only a `local` is a cell of its own, created by
/// `alloc_place` and ended by `end_lifetime`; every other origin resolves
/// through storage the body already holds and is never allocated.
export type PlaceDecl =
  | { id: number; origin: "local" }
  | {
      id: number;
      origin: "aggregate";
      base: { place: number } | { value: number };
      shape?: number;
      field: number;
    }
  | { id: number; origin: "capture"; environment: number; field: number }
  | {
      id: number;
      origin: "actor_state";
      environment: number;
      field: number;
      initialized: boolean;
    }
  | { id: number; origin: "runtime" };

/// A value capability the checker selected for a concrete type. `callable` is a
/// user implementation to call; without it the operation is the derived,
/// structural one over the VM's own value representation.
export interface ValueCapability {
  id: number;
  capability: "Eq" | "Hash";
  ty: string;
  callable?: number;
  components?: number[];
  variants?: number[][];
}

export interface ClosureShape {
  id: number;
  body: number;
  fields: number;
}

export interface VtableShape {
  id: number;
  /// `receiver` is how the wrapped value reaches the callee's `self`, spelled
  /// as a boundary operand's decision. It is the erasure the concrete type was
  /// made under, so it is read rather than inferred from the method.
  slots: Array<{
    slot: number;
    method: string;
    callee: number;
    receiver: BoundaryDecision;
  }>;
}

export interface FunctionV1 {
  id: number;
  name: string;
  params: ValueDef[];
  entry: number;
  places: PlaceDecl[];
  blocks: BlockV1[];
}

export interface BlockV1 {
  id: number;
  params: ValueDef[];
  ops: OpV1[];
  term: TermV1;
}

interface OpBase {
  own?: Own;
  dst?: number | null;
  span?: SpanV1 | null;
}

export type OpV1 =
  // Constants
  | (OpBase & { op: "const.int"; dst: number; value: string; ty: string })
  | (OpBase & { op: "const.bool"; dst: number; value: boolean })
  | (OpBase & {
      op: "const.float";
      dst: number;
      value?: number;
      ty: string;
      nonfinite?: "nan" | "inf" | "-inf";
    })
  | (OpBase & { op: "const.char"; dst: number; value: string })
  | (OpBase & { op: "const.unit"; dst: number })
  | (OpBase & { op: "const.duration"; dst: number; nanos: string })
  | (OpBase & { op: "const.str"; dst: number; str: number })
  | (OpBase & { op: "const.bytes"; dst: number; bytes: number })
  // Ownership
  | (OpBase & { op: "copy_value"; dst: number; source: Operand })
  | (OpBase & { op: "move"; dst: number; source: Operand })
  | (OpBase & { op: "fork"; dst: number; source: Operand })
  | (OpBase & { op: "destroy_value"; value: Operand })
  | (OpBase & { op: "begin_borrow"; dst: number; owner: Operand })
  | (OpBase & { op: "end_borrow"; borrow: Operand })
  | (OpBase & { op: "finish_linear_receiver" })
  // Places
  | (OpBase & { op: "alloc_place"; place: number })
  | (OpBase & { op: "store.init"; place: number; value: Operand })
  | (OpBase & { op: "store.assign"; place: number; value: Operand })
  | (OpBase & { op: "load.copy"; dst: number; place: number })
  | (OpBase & { op: "load.take"; dst: number; place: number })
  | (OpBase & { op: "load.borrow"; dst: number; place: number })
  | (OpBase & { op: "end_lifetime"; place: number })
  // Aggregates, tuples, arrays and variants
  | (OpBase & { op: "tuple.make"; dst: number; elements: Operand[] })
  | (OpBase & { op: "tuple.get"; dst: number; tuple: Operand; index: number })
  | (OpBase & {
      op: "aggregate.make";
      dst: number;
      shape: number;
      fields: Operand[];
    })
  | (OpBase & {
      op: "aggregate.project_copy";
      dst: number;
      shape: number;
      aggregate: Operand;
      field: number;
    })
  | (OpBase & {
      op: "aggregate.project_borrow";
      dst: number;
      shape: number;
      aggregate: Operand;
      field: number;
    })
  | (OpBase & {
      op: "destructure";
      shape: number | null;
      aggregate: Operand;
      results: number[];
    })
  | (OpBase & { op: "array.make"; dst: number; elements: Operand[] })
  | (OpBase & {
      op: "array.repeat";
      dst: number;
      value: Operand;
      count?: number;
    })
  | (OpBase & {
      op: "variant.make";
      dst: number;
      shape: number;
      variant: number;
      fields: Operand[];
    })
  | (OpBase & {
      op: "variant.is";
      dst: number;
      shape: number;
      variant: number;
      source: Operand;
    })
  | (OpBase & {
      op: "variant.project_copy";
      dst: number;
      shape: number;
      variant: number;
      source: Operand;
      field: number;
    })
  | (OpBase & {
      op: "variant.project_borrow";
      dst: number;
      shape: number;
      variant: number;
      source: Operand;
      field: number;
    })
  | (OpBase & {
      op: "variant.destructure";
      shape: number;
      variant: number;
      source: Operand;
      results: number[];
    })
  // Operators
  | (OpBase & {
      op: "unary";
      dst: number;
      unary_op: string;
      value: Operand;
      ty: string;
    })
  | (OpBase & {
      op: "binary";
      dst: number;
      binary_op: string;
      lhs: Operand;
      rhs: Operand;
      ty: string;
    })
  | (OpBase & {
      op: "cast";
      dst: number;
      value: Operand;
      from: string;
      to: string;
    })
  | (OpBase & { op: "str.eq"; dst: number; lhs: Operand; rhs: Operand })
  | (OpBase & { op: "bytes.eq"; dst: number; lhs: Operand; rhs: Operand })
  // Callables and defer; concurrency has no producer in a sequential package
  | (OpBase & { op: "function.make"; dst: number; callable: number })
  | (OpBase & {
      op: "closure.make";
      dst: number;
      closure: number;
      fields: Operand[];
    })
  | (OpBase & { op: "callable.coerce"; dst: number; source: Operand })
  | (OpBase & { op: "dyn.make"; dst: number; vtable: number; value: Operand })
  | (OpBase & {
      op: "register_defer";
      defer: number;
      scope: number;
      dependencies: Operand[];
    })
  | (OpBase & {
      op: "generator.make";
      dst: number;
      closure: number;
      callable: Operand;
    })
  | (OpBase & { op: "actor.ingress_adapter" })
  | (OpBase & { op: "stream.pipe"; capacity: number; results: ValueDef[] })
  | (OpBase & {
      op: "task_scope.enter";
      scope: number;
      parent: number | null;
      duration: Operand | null;
    })
  | (OpBase & { op: "task_scope.close"; scope: number })
  | (OpBase & {
      op: "task.spawn";
      dst: number;
      scope: number;
      callable: Operand;
    });

export type OpName = OpV1["op"];

interface TermBase {
  span?: SpanV1 | null;
}

export interface SwitchArm {
  variant: number;
  fields: ValueDef[];
  edge: Edge;
}

export interface CheckedFailure {
  trap: TrapName;
  edge: Edge;
}

/// The fields every call terminator shares. `normal` is absent exactly when
/// `result` is `"never"`; `unwind` is `null` when the call cannot raise.
export interface RequestShapes {
  failure: number | null;
  reason: number | null;
  message: number | null;
  request: number | null;
}

export interface CallShape {
  args: BoundaryOperand[];
  result: CallResult;
  normal?: Edge;
  unwind: Edge | null;
  request_shapes?: RequestShapes;
}

export type TermV1 =
  | (TermBase & { op: "return"; value: BoundaryOperand | null })
  | (TermBase & { op: "goto"; edge: Edge })
  | (TermBase & { op: "branch"; condition: Operand; then: Edge; else: Edge })
  | (TermBase & {
      op: "switch.variant";
      shape: number;
      scrutinee: Operand;
      arms: SwitchArm[];
    })
  | (TermBase & {
      op: "checked.binary";
      binary_op: string;
      lhs: Operand;
      rhs: Operand;
      result: ValueDef;
      normal: Edge;
      failures: CheckedFailure[];
      ty: string;
    })
  | (TermBase & { op: "unreachable" })
  | (TermBase &
      CallShape & {
        op: "call";
        callee: number;
        /// The receiver a failing `var self` callee hands back, defined on the
        /// unwind edge.
        handback?: ValueDef | null;
      })
  | (TermBase & CallShape & { op: "indirect.call"; callee: BoundaryOperand })
  | (TermBase &
      CallShape & { op: "dyn.call"; receiver: BoundaryOperand; slot: number })
  | (TermBase & CallShape & { op: "value.call"; plan: number })
  | (TermBase &
      CallShape & {
        op: "runtime.call";
        family: number;
        structural?: number | null;
        callbacks?: number[];
        releases_contents?: boolean;
        result_member_shapes?: Array<number | null>;
        result_shape: number | null;
      })
  | (TermBase &
      CallShape & {
        op: "extern.call";
        extern: number;
        result_shape: number | null;
      })
  | (TermBase &
      CallShape & {
        op: "actor.call";
        operation: ActorOperation;
        submission_shapes?: {
          success: number | null;
          failure: number | null;
          reason: number | null;
        };
        result_shape: number | null;
        error_shape: number | null;
      })
  | (TermBase &
      CallShape & { op: "wire.codec"; direction: string; plan: number })
  | (TermBase & { op: "panic"; message: BoundaryOperand; cleanup: Edge })
  | (TermBase & { op: "trap"; trap: TrapName })
  | (TermBase & { op: "checked_raise"; trap: TrapName; cleanup: Edge })
  | (TermBase & { op: "cleanup.dispatch"; normal: Edge; fault: Edge })
  | (TermBase & { op: "resume_unwind"; handback?: BoundaryOperand | null })
  | (TermBase & { op: "enter_defer"; defer: number; park: number; body: Edge })
  | (TermBase & { op: "finish_defer"; defer: number; park: number; next: Edge })
  | (TermBase & {
      op: "recover_fault";
      result_shape?: number | null;
      result: ValueDef;
      deadline_variant: number;
      fault_variant: number;
      normal: Edge;
      unwind: Edge | null;
    })
  | (TermBase & {
      op: "suspend";
      kind: string;
      detail?: any;
      request_shapes?: RequestShapes;
      result_shape?: number | null;
      error_shape?: number | null;
      inputs: BoundaryOperand[];
      result: CallResult;
      resumes: Edge[];
      cancel: Edge;
      unwind: Edge | null;
    });

export type TermName = TermV1["op"];

export interface EntryPlan {
  function: number;
  exit: "unit" | "status";
}

export interface ActorProtocol {
  actor: number;
  message: number;
  policy: "reject" | "wait" | "drop_newest" | "replace_latest";
  deadline_ns: number | null;
  sealed: boolean;
}

export type ActorOperation =
  | { op: "spawn" | "self_handle" | "close" | "await_closed"; actor: number }
  | { op: "call_start" | "call_take"; protocol: ActorProtocol }
  | { op: "submit"; actor: number; policy: ActorProtocol["policy"] }
  | { op: "stream_start"; actor: number; message: number }
  | { op: "local_observation"; kind: string }
  | {
      op: "supervisor_spawn" | "supervisor_stop" | "supervisor_await_closed";
      supervisor: number;
    }
  | {
      op:
        | "supervisor_child"
        | "supervisor_await_restart"
        | "supervisor_pool_view";
      supervisor: number;
      child: number;
      owner_is_role: boolean;
    }
  | {
      op: "supervisor_role_await_closed";
      supervisor: number;
      closing: boolean;
    };

export interface ActorShape {
  id: number;
  state_fields: Array<{ mutable: boolean; deferred: boolean }>;
  init?: number;
  start?: number;
  stop: number[];
  crash?: number;
  crash_info?: number;
  crash_action?: number;
  exit?: number;
  down?: number;
  handlers: Array<{
    name: string;
    message_id: number;
    callable: number;
    params: number;
    streams: boolean;
    fallible: boolean;
    result_shape: number | null;
    every_ns?: number;
  }>;
  mailbox_capacity?: number;
  overflow: string;
  coalesce?: {
    fallback: string;
    keys: Array<{ message: number; param: number; kind: string }>;
  };
  max_heap_bytes?: number;
}

export interface SupervisorShape {
  id: number;
  strategy: string;
  max_restarts: number;
  window_secs: number;
  children: Array<{
    name: string;
    role: { actor: number } | { supervisor: number };
    restart: string;
    pool_count?: number;
    spawn: number;
  }>;
}

export interface PackageV1 {
  resources?: Array<{
    kind: string;
    ty?: string;
    close?: number;
    release?: string;
  }>;
  structural_render?: import("./structural.js").StructuralRecipe[];
  schema_version: "hew.sandbox.bytecode.v1";
  hew_version: string;
  compiler_version: string;
  profile: string;
  entry?: EntryPlan;
  strings: string[];
  bytes: number[][];
  regex_patterns: string[];
  aggregates: AggregateShape[];
  variants: VariantShape[];
  runtime_families: RuntimeFamilyEntry[];
  externs: ExternEntry[];
  /// Every `SuspendKind` variant name the instruction stream reaches.
  suspend_kinds: string[];
  value_capabilities: ValueCapability[];
  closures: ClosureShape[];
  vtables: VtableShape[];
  actors?: ActorShape[];
  supervisors?: SupervisorShape[];
  functions: FunctionV1[];
}

export function isPackageV1(input: unknown): input is PackageV1 {
  return (
    typeof input === "object" &&
    input !== null &&
    (input as { schema_version?: unknown }).schema_version ===
      "hew.sandbox.bytecode.v1"
  );
}
