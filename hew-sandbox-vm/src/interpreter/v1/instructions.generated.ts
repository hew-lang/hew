// Generated from hew-wasm/src/sandbox/bytecode.rs. Run make sandbox-bytecode.
export type ActorOperation =
  | { op: "identity" }
  | { op: "spawn"; actor: number }
  | { op: "self_handle"; actor: number }
  | ({ op: "stop" } & ({ actor: number } | { supervisor: number }))
  | ({ op: "terminate" } & ({ actor: number } | { supervisor: number }))
  | ({ op: "await_stopped" } & ({ actor: number } | { supervisor: number }))
  | ({ op: "await_restarted" } & ({ actor: number } | { supervisor: number }))
  | { op: "call_start"; protocol: ActorProtocol }
  | { op: "call_take"; protocol: ActorProtocol }
  | { op: "submit"; actor: number; policy: SendPolicy }
  | { op: "stream_start"; actor: number; message: number }
  | {
      op: "timer_schedule";
      actor: number;
      message: number;
      resource: number;
      policy: TimerSendPolicy;
    }
  | { op: "local_observation"; kind: string }
  | { op: "supervisor_spawn"; supervisor: number }
  | { op: "supervisor_child"; supervisor: number; child: number }
  | { op: "supervisor_pool_view"; supervisor: number; child: number };
export type ActorProtocol = {
  actor: number;
  message: number;
  policy: SendPolicy;
  sealed: boolean;
};
export type BoundaryDecision =
  | "borrow"
  | "borrow_mut"
  | "copy"
  | "move"
  | "snapshot";
export type BoundaryOperand = { value: number; decision: BoundaryDecision };
export type CallResult = null | CallResultNever | ValueDef;
export type CallResultNever = "never";
export type CheckedFailure = { trap: TrapName; edge: Edge };
export type CodecDirection = "encode" | "decode" | "test_input";
export type DecodedCodec = {
  shape: number;
  ok: number;
  error: number;
  error_root: number;
};
export type DirectInvocation = { direct: number };
export type DynInvocation = {
  dyn: number;
  method: string;
  receiver: BoundaryOperand;
};
export type Edge = { to: number; args: Array<number> };
export type IndirectInvocation = {
  receiver: BoundaryOperand;
  specialization: number | null;
};
export type InvocationCallee =
  | DirectInvocation
  | MutableInvocation
  | DynInvocation
  | IndirectInvocation;
export type MutableInvocation = {
  mutable: number;
  receiver: BoundaryOperand;
  staged: boolean;
};
export type Nonfinite = "nan" | "inf" | "-inf";
export type OpV1 = { own?: Own | null; span?: SpanV1 | null | null } & (
  | { op: "coverage.hit"; counter: number }
  | { op: "const.int"; dst: number; value: string; ty: string }
  | { op: "const.bool"; dst: number; value: boolean }
  | {
      op: "const.float";
      dst: number;
      value?: number | null;
      ty: string;
      nonfinite?: Nonfinite | null;
    }
  | { op: "const.char"; dst: number; value: string }
  | { op: "const.unit"; dst: number }
  | { op: "const.duration"; dst: number; nanos: string }
  | { op: "const.str"; dst: number; str: number }
  | { op: "const.bytes"; dst: number; bytes: number }
  | { op: "copy_value"; dst: number; source: number }
  | { op: "move"; dst: number; source: number }
  | { op: "fork"; dst: number; source: number }
  | { op: "destroy_value"; dst?: number | null | null; value: number }
  | { op: "begin_borrow"; dst: number; owner: number }
  | { op: "end_borrow"; dst?: number | null | null; borrow: number }
  | { op: "finish_linear_receiver"; dst?: number | null | null }
  | { op: "alloc_place"; dst?: number | null | null; place: number }
  | {
      op: "store.init";
      dst?: number | null | null;
      place: number;
      value: number;
    }
  | {
      op: "store.assign";
      dst?: number | null | null;
      place: number;
      value: number;
    }
  | { op: "load.copy"; dst: number; place: number }
  | { op: "load.take"; dst: number; place: number }
  | { op: "load.borrow"; dst: number; place: number }
  | { op: "end_lifetime"; dst?: number | null | null; place: number }
  | { op: "tuple.make"; dst: number; elements: Array<number> }
  | { op: "tuple.get"; dst: number; tuple: number; index: number }
  | {
      op: "aggregate.make";
      dst: number;
      shape: number | null;
      fields: Array<number>;
    }
  | {
      op: "aggregate.project_copy";
      dst: number;
      shape: number | null;
      aggregate: number;
      field: number;
    }
  | {
      op: "aggregate.project_borrow";
      dst: number;
      shape: number | null;
      aggregate: number;
      field: number;
    }
  | {
      op: "destructure";
      dst?: number | null | null;
      shape: number | null;
      aggregate: number;
      results: Array<number>;
    }
  | { op: "array.make"; dst: number; elements: Array<number> }
  | { op: "array.repeat"; dst: number; value: number; count?: number | null }
  | {
      op: "variant.make";
      dst: number;
      shape: number;
      variant: number;
      fields: Array<number>;
    }
  | {
      op: "variant.is";
      dst: number;
      shape: number;
      variant: number;
      source: number;
    }
  | {
      op: "variant.project_copy";
      dst: number;
      shape: number;
      variant: number;
      source: number;
      field: number;
    }
  | {
      op: "variant.project_borrow";
      dst: number;
      shape: number;
      variant: number;
      source: number;
      field: number;
    }
  | {
      op: "variant.destructure";
      dst?: number | null | null;
      shape: number;
      variant: number;
      source: number;
      results: Array<number>;
    }
  | { op: "unary"; dst: number; unary_op: string; value: number; ty: string }
  | {
      op: "binary";
      dst: number;
      binary_op: string;
      lhs: number;
      rhs: number;
      ty: string;
    }
  | { op: "cast"; dst: number; value: number; from: string; to: string }
  | { op: "str.eq"; dst: number; lhs: number; rhs: number }
  | { op: "bytes.eq"; dst: number; lhs: number; rhs: number }
  | { op: "function.make"; dst: number; callable: number }
  | { op: "closure.make"; dst: number; closure: number; fields: Array<number> }
  | {
      op: "callable.coerce";
      dst: number;
      source: number;
      specialization: number | null;
    }
  | { op: "dyn.make"; dst: number; vtable: number; value: number }
  | {
      op: "register_defer";
      dst?: number | null | null;
      defer: number;
      scope: number;
      dependencies: Array<number>;
    }
  | { op: "generator.make"; dst: number; closure: number; callable: number }
  | {
      op: "actor.ingress_adapter";
      dst?: number | null | null;
      actor: number;
      message: number;
    }
  | {
      op: "stream.pipe";
      dst?: number | null | null;
      capacity: number;
      results: Array<ValueDef>;
    }
  | {
      op: "task_scope.enter";
      dst?: number | null | null;
      scope: number;
      parent: number | null;
      duration: number | null;
    }
  | { op: "task_scope.close"; dst?: number | null | null; scope: number }
  | { op: "task.race"; dst: number; scope: number; members: Array<number> }
  | { op: "task.spawn"; dst: number; scope: number; callable: number }
  | {
      op: "task.invoke";
      dst: number;
      scope: number;
      callee: InvocationCallee;
      args: Array<BoundaryOperand>;
    }
);
export type Own = "none" | "owned" | "guaranteed";
export type RequestShapes = {
  failure: number | null;
  reason: number | null;
  message: number | null;
  request: number | null;
};
export type SendPolicy = "reject" | "wait" | "drop_newest";
export type SpanV1 = { start: number; end: number };
export type SubmissionShapes = {
  success: number | null;
  failure: number | null;
  reason: number | null;
};
export type SwitchArm = {
  variant: number;
  fields: Array<ValueDef>;
  edge: Edge;
};
export type TermV1 = { span?: SpanV1 | null | null } & (
  | { op: "return"; value: BoundaryOperand | null }
  | { op: "goto"; edge: Edge }
  | { op: "branch"; condition: number; then: Edge; else: Edge }
  | {
      op: "switch.variant";
      shape: number;
      scrutinee: number;
      arms: Array<SwitchArm>;
    }
  | {
      op: "checked.binary";
      binary_op: string;
      lhs: number;
      rhs: number;
      result: ValueDef;
      normal: Edge;
      failures: Array<CheckedFailure>;
      ty: string;
    }
  | {
      op: "timestamp.binary";
      binary_op: TimestampBinaryOp;
      lhs: number;
      rhs: number;
      lhs_timestamp: boolean;
      rhs_timestamp: boolean;
      shape: number | null;
      result: ValueDef;
      normal: Edge;
      failures: Array<CheckedFailure>;
    }
  | { op: "unreachable" }
  | {
      op: "call";
      args: Array<BoundaryOperand>;
      result: CallResult;
      normal?: Edge | null | null;
      unwind: Edge | null;
      request_shapes?: RequestShapes | null | null;
      callee: number;
      handback?: ValueDef | null | null;
    }
  | {
      op: "indirect.call";
      args: Array<BoundaryOperand>;
      result: CallResult;
      normal?: Edge | null | null;
      unwind: Edge | null;
      request_shapes?: RequestShapes | null | null;
      callee: BoundaryOperand;
      specialization: number | null;
    }
  | {
      op: "dyn.call";
      args: Array<BoundaryOperand>;
      result: CallResult;
      normal?: Edge | null | null;
      unwind: Edge | null;
      request_shapes?: RequestShapes | null | null;
      receiver: BoundaryOperand;
      slot: number;
    }
  | {
      op: "value.call";
      args: Array<BoundaryOperand>;
      result: CallResult;
      normal?: Edge | null | null;
      unwind: Edge | null;
      request_shapes?: RequestShapes | null | null;
      plan: number;
    }
  | {
      op: "wire.codec";
      args: Array<BoundaryOperand>;
      result: CallResult;
      normal?: Edge | null | null;
      unwind: Edge | null;
      request_shapes?: RequestShapes | null | null;
      codec: number;
      format: number;
      direction: CodecDirection;
      decoded: DecodedCodec | null;
      callbacks: { [key in string]: Array<number> };
    }
  | {
      op: "runtime.call";
      args: Array<BoundaryOperand>;
      result: CallResult;
      normal?: Edge | null | null;
      unwind: Edge | null;
      request_shapes?: RequestShapes | null | null;
      shared_payload_type?: string | null;
      family: number;
      structural?: number | null | null;
      callbacks?: Array<number> | null;
      releases_contents?: boolean | null;
      keeps_inputs_on_failure?: boolean | null;
      result_member_shapes?: Array<number | null> | null;
      variant_result_shapes?: VariantResultShapes | null;
      result_shape: number | null;
      /**
       * Checked aggregate descriptor supplied by the SIR result type.
       */
      aggregate_result_shape?: number | null | null;
    }
  | {
      op: "extern.call";
      args: Array<BoundaryOperand>;
      result: CallResult;
      normal?: Edge | null | null;
      unwind: Edge | null;
      request_shapes?: RequestShapes | null | null;
      extern: number;
      result_shape: number | null;
      /**
       * Checked aggregate descriptor supplied by the SIR result type.
       */
      aggregate_result_shape?: number | null | null;
    }
  | {
      op: "actor.call";
      args: Array<BoundaryOperand>;
      result: CallResult;
      normal?: Edge | null | null;
      unwind: Edge | null;
      request_shapes?: RequestShapes | null | null;
      operation: ActorOperation;
      submission_shapes?: SubmissionShapes | null | null;
      result_shape: number | null;
      error_shape: number | null;
    }
  | {
      op: "panic";
      message: BoundaryOperand;
      assertion?:
        | [BoundaryOperand, BoundaryOperand, BoundaryOperand]
        | null
        | null;
      cleanup: Edge;
    }
  | { op: "trap"; trap: TrapName }
  | { op: "checked_raise"; trap: TrapName; cleanup: Edge }
  | { op: "cleanup.dispatch"; normal: Edge; fault: Edge }
  | { op: "resume_unwind"; handback?: BoundaryOperand | null | null }
  | { op: "enter_defer"; defer: number; park: number; body: Edge }
  | { op: "finish_defer"; defer: number; park: number; next: Edge }
  | {
      op: "recover_fault";
      result_shape?: number | null | null;
      result: ValueDef;
      deadline_variant: number;
      fault_variant: number;
      normal: Edge;
      unwind: Edge | null;
    }
  | {
      op: "suspend";
      kind: string;
      detail?: any;
      request_shapes?: RequestShapes | null | null;
      result_shape?: number | null | null;
      error_shape?: number | null | null;
      inputs: Array<BoundaryOperand>;
      result: CallResult;
      resumes: Array<Edge>;
      cancel: Edge | null;
      unwind: Edge | null;
    }
);
export type TimerSendPolicy = "reject" | "drop_newest";
export type TimestampBinaryOp = "Add" | "Subtract";
export type TrapName =
  | "integer_overflow"
  | "divide_by_zero"
  | "shift_out_of_range"
  | "vector_bounds"
  | "actor_refused";
export type ValueDef = { value: number; own: Own };
export type VariantResultShapes = { error: number; error_len: number };
export const OP_NAMES = [
  "coverage.hit",
  "const.int",
  "const.bool",
  "const.float",
  "const.char",
  "const.unit",
  "const.duration",
  "const.str",
  "const.bytes",
  "copy_value",
  "move",
  "fork",
  "destroy_value",
  "begin_borrow",
  "end_borrow",
  "finish_linear_receiver",
  "alloc_place",
  "store.init",
  "store.assign",
  "load.copy",
  "load.take",
  "load.borrow",
  "end_lifetime",
  "tuple.make",
  "tuple.get",
  "aggregate.make",
  "aggregate.project_copy",
  "aggregate.project_borrow",
  "destructure",
  "array.make",
  "array.repeat",
  "variant.make",
  "variant.is",
  "variant.project_copy",
  "variant.project_borrow",
  "variant.destructure",
  "unary",
  "binary",
  "cast",
  "str.eq",
  "bytes.eq",
  "function.make",
  "closure.make",
  "callable.coerce",
  "dyn.make",
  "register_defer",
  "generator.make",
  "actor.ingress_adapter",
  "stream.pipe",
  "task_scope.enter",
  "task_scope.close",
  "task.race",
  "task.spawn",
  "task.invoke",
] as const;
export const TERM_NAMES = [
  "return",
  "goto",
  "branch",
  "switch.variant",
  "checked.binary",
  "timestamp.binary",
  "unreachable",
  "call",
  "indirect.call",
  "dyn.call",
  "value.call",
  "wire.codec",
  "runtime.call",
  "extern.call",
  "actor.call",
  "panic",
  "trap",
  "checked_raise",
  "cleanup.dispatch",
  "resume_unwind",
  "enter_defer",
  "finish_defer",
  "recover_fault",
  "suspend",
] as const;
export type OpName = OpV1["op"];
export type TermName = TermV1["op"];
