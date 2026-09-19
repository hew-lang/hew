export type JsonValue =
  | null
  | boolean
  | number
  | string
  | JsonValue[]
  | { [key: string]: JsonValue };

export type SandboxBytecodePackage = import("./v1/package.js").PackageV1;

export type TrapKind =
  | "integer_overflow"
  | "shift_out_of_range"
  | "divide_by_zero"
  | "invalid_local"
  | "invalid_block"
  | "invalid_call"
  | "invalid_enum_tag"
  | "invalid_record_field"
  | "vector_bounds"
  | "string_bounds"
  | "regex_compile"
  | "capability_missing"
  | "budget_exhausted"
  | "panic"
  | "unsupported_instruction"
  | "internal_error";

export type RuntimeStatus =
  | "ok"
  | "compile_error"
  | "sandbox_rejected"
  | "runtime_failure"
  | "budget_exhausted"
  | "panic"
  | "trap";

export interface RuntimeFailure {
  kind:
    | "panic"
    | "trap"
    | "budget_exhausted"
    | "unsupported"
    | "internal_error";
  message: string;
  span: TraceSpan | null;
  trap_kind: TrapKind | null;
  unsupported?: UnsupportedDiagnostic;
}

/// A load-time admission refusal. `capability` names the runtime family or
/// extern symbol the VM has no shim for.
export interface SandboxRejection {
  category: "native_only" | "not_implemented" | "invalid_package";
  code: string;
  capability: string | null;
  message: string;
  span: TraceSpan | null;
}

export interface UnsupportedDiagnostic {
  kind:
    | "Unsupported::SANDBOX_OUT_OF_SCOPE"
    | "Unsupported::M7_DEFERRED"
    | "Unsupported::NATIVE_ONLY";
  symbol: string;
  status:
    | "unsupported_out_of_scope"
    | "unsupported_m7_deferred_to_post_v05"
    | "unsupported_native_only";
  reason: string;
}

export interface TraceEvent {
  seq: number;
  type:
    | "trace.started"
    | "trace.ended"
    | "compile.diagnostic"
    | "sandbox.rejected"
    | "runtime.failure"
    | "budget.exhausted"
    | "io.stdout"
    | "io.stderr"
    | "replay.input"
    | "clock.virtual_advance"
    | "runtime.id_allocated"
    | "state.snapshot"
    | "step.committed";
  phase: "compile" | "profile" | "run" | "replay";
  span: TraceSpan | null;
  message?: string;
  text?: string;
  failure?: RuntimeFailure;
  rejection?: SandboxRejection;
  replay_input?: { kind: string; data: JsonValue };
  id_kind?: "actor" | "channel" | "task" | "supervisor" | "machine";
  id?: string;
  parent_id?: string | null;
  step_count?: number;
  budget_remaining?: number;
  trap_kind?: TrapKind;
  clock?: VirtualClock;
  amount_ms?: number;
}

export interface ReplayConfig {
  seed: number;
  step_budget: number;
  virtual_clock: VirtualClock;
  inputs: Array<{ kind: string; data: JsonValue }>;
}

export interface VirtualClock {
  epoch_ms: number;
  tick_ms: number;
  current_ms: number;
}

export interface SourcePosition {
  line: number;
  column: number;
  byte_offset: number;
}

export interface TraceSpan {
  source_id: string;
  path: string;
  start: SourcePosition;
  end: SourcePosition;
}

export interface SandboxTrace {
  schema_version: "hew.sandbox.trace.v0";
  trace_id: string;
  fixture_id: string;
  profile: string;
  hew_version: string;
  sandbox_version: string;
  result: RuntimeStatus;
  replay: ReplayConfig;
  events: TraceEvent[];
  final_state: {
    status: RuntimeStatus;
    exit_code: number | null;
    step_count: number;
    budget_remaining: number;
    virtual_clock: VirtualClock;
    stdout: string[];
    stderr: string[];
    ids: {
      actors: string[];
      channels: string[];
      tasks: string[];
      supervisors: string[];
      machines: string[];
    };
    diagnostics: unknown[];
    sandbox_rejections: SandboxRejection[];
    runtime_failures: RuntimeFailure[];
    globals: Array<{ name: string; type: string; value: JsonValue }>;
  };
}

export interface RunOptions {
  fixtureId?: string;
  traceId?: string;
  replay?: Partial<ReplayConfig>;
  stepBudget?: number;
  sandboxVersion?: string;
  schedulerPolicy?: "round_robin" | "chaos";
}
