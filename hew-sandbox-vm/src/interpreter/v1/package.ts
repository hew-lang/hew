/// Package envelope for v1 sandbox bytecode.
/// Instruction types are generated from the Rust wire contract.
///
/// The package is a projection of verified ownership SIR. Every fact here was
/// proved by SIR verification, so the executor reads these fields; it does not
/// re-derive them from a type, a name or a symbol spelling.

import type {
  BoundaryDecision,
  BoundaryOperand,
  Edge,
  OpV1,
  TermV1,
  ValueDef,
  RuntimeFamilyEntry,
} from "./instructions.generated.js";
export type {
  Own,
  BoundaryDecision,
  TrapName,
  SpanV1,
  BoundaryOperand,
  Edge,
  ValueDef,
  CallResult,
  OpV1,
  OpName,
  TermV1,
  TermName,
  SwitchArm,
  CheckedFailure,
  RequestShapes,
  ActorProtocol,
  ActorOperation,
  RuntimeFamilyEntry,
} from "./instructions.generated.js";
export type Operand = number;

export interface AggregateShape {
  id: number;
  name: string;
  fields: string[];
  builtin?: "Timestamp";
}

export interface VariantCase {
  name: string;
  fields: string[];
}

export interface VariantShape {
  id: number;
  name: string;
  cases: VariantCase[];
  runtime_tags?: Partial<Record<RuntimeVariantRole, number>>;
}

export type RuntimeVariantRole =
  | "CrashKindFault"
  | "CrashKindHeapExceeded"
  | "CrashKindPartitionDetected"
  | "CrashKindHandlerFailed"
  | "CrashKindTimerOverdue"
  | "OptionSome"
  | "OptionNone"
  | "ResultOk"
  | "ResultErr"
  | "ActorErrorRejected"
  | "ActorErrorFailed"
  | "ActorErrorTrapped"
  | "ActorErrorRefused"
  | "CallerActor"
  | "CallerTask"
  | "CallerTimer"
  | "CallerNode"
  | "ActorErrorDead"
  | "ActorErrorTimedOut"
  | "ActorErrorNodeNotRunning"
  | "ActorErrorRoutingFailed"
  | "ActorErrorEncodeFailed"
  | "ActorErrorConnectionDropped"
  | "ActorErrorPartition"
  | "SendErrorFull"
  | "SendErrorClosed"
  | "SendErrorPartition"
  | "SendErrorStaleRef"
  | "SendErrorLocalShutdown"
  | "SendErrorCancelled"
  | "SendErrorVersionMismatch"
  | "SendErrorBackpressure"
  | "SendErrorDead"
  | "SendErrorUnauthorized"
  | "SendErrorInvalidMessage"
  | "SendErrorTimedOut"
  | "SendErrorWriteTimedOut"
  | "DeliveryAccepted"
  | "DeliveryDiscarded"
  | "LinkErrorNoContext";

export function runtimeTag(
  shape: VariantShape,
  role: RuntimeVariantRole,
): number {
  const tag = shape.runtime_tags?.[role];
  if (
    tag === undefined ||
    !Number.isInteger(tag) ||
    tag < 0 ||
    tag >= shape.cases.length
  )
    throw new Error(`variant shape ${shape.id} has no checked ${role} tag`);
  return tag;
}

/// One entry per distinct runtime-call family the instruction stream reaches.
/// `detail` is the serde payload of the `RuntimeCallFamily` variant: an object
/// for a struct variant (`Print`), the bare operation name for a newtype
/// variant (`Vector`, `Map`), and absent for a unit variant.

export interface ExternEntry {
  id: number;
  symbol: string;
  /// The manifest label of a native-only runtime endpoint.
  native_capability?: string;
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
  capability: "Eq" | "Hash" | "Less" | "LessEqual" | "Greater" | "GreaterEqual";
  ty: string;
  callable?: number;
  components?: ValueComponentCapabilities[];
  variants?: ValueComponentCapabilities[][];
}

export interface ValueComponentCapabilities {
  selected: number;
  ordering?: {
    less: number;
    less_equal: number;
    greater: number;
    greater_equal: number;
  };
}

export interface ClosureShape {
  id: number;
  entries: Array<{ body: number; specialization: number | null }>;
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
    receiver_handback?: boolean;
  }>;
}

export interface FunctionV1 {
  actor_construction?: boolean;
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

/// Call boundaries consume the generated instruction contract.
export type CallShape = Pick<
  Extract<TermV1, { op: "call" }>,
  "args" | "result" | "normal" | "unwind" | "request_shapes"
>;

export interface EntryPlan {
  function: number;
  exit: "unit" | "status";
}

export interface ActorShape {
  id: number;
  name: string;
  state_fields: Array<{ mutable: boolean; deferred: boolean }>;
  init?: number;
  start?: number;
  stop: number[];
  crash?: number;
  crash_info?: number;
  crash_kind?: number;
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
    keys: Array<{ message: number; param: number }>;
  };
  max_heap_bytes?: number;
}

export interface SupervisorShape {
  id: number;
  startup_order?: number[];
  strategy: string;
  max_restarts: number;
  window_secs: number;
  children: Array<{
    name: string;
    dependencies?: number[];
    role: { actor: number } | { supervisor: number };
    restart: string;
    stop_ns?: number | null;
    stop_config?: number;
    is_pool?: boolean;
    spawn: number;
  }>;
}

export interface PackageV1 {
  codecs?: string[];
  resources?: Array<{
    kind: string;
    ty?: string;
    shape?: number;
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
  suspend_kinds: Extract<TermV1, { op: "suspend" }>["kind"][];
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
