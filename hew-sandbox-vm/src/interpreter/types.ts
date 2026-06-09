export type JsonValue =
  | null
  | boolean
  | number
  | string
  | JsonValue[]
  | { [key: string]: JsonValue };

export interface Operand {
  kind:
    | "local"
    | "constant"
    | "function"
    | "block"
    | "type"
    | "symbol"
    | "capability"
    | "trap"
    | "literal";
  value: JsonValue;
}

export interface Instruction {
  op: string;
  dst: string | null;
  args: Operand[];
  span: string | null;
  metadata?: JsonValue;
}

export interface Terminator {
  op: "br" | "br_if" | "return" | "tail_call" | "trap" | "unreachable";
  target?: string;
  else_target?: string;
  condition?: Operand;
  function?: string;
  args: Operand[];
  trap_kind?: TrapKind;
  span: string | null;
}

export interface Block {
  id: string;
  params: string[];
  instructions: Instruction[];
  terminator: Terminator;
  span: string | null;
}

export interface Local {
  id: string;
  name: string | null;
  type: string;
  mutable: boolean;
  span: string | null;
}

export interface BytecodeFunction {
  id: string;
  module: string;
  name: string;
  params: string[];
  result: string;
  locals: Local[];
  blocks: Block[];
  span: string | null;
}

export interface StdlibSymbol {
  id: string;
  module: string;
  name: string;
  params: string[];
  result: string;
  capability: string | null;
  admission: "allowed" | "rejected" | "reserved";
}

export interface Capability {
  id: string;
  disposition: "allowed" | "rejected" | "reserved";
  reason: string;
  required_by: string[];
}

export interface FieldLayout {
  name: string;
  type: string;
  index: number;
}

export interface RecordLayout {
  id: string;
  name: string;
  fields: FieldLayout[];
}

export interface VariantLayout {
  name: string;
  tag: number;
  payload: string[];
}

export interface EnumLayout {
  id: string;
  name: string;
  variants: VariantLayout[];
}

export interface TypeLayout {
  id: string;
  kind: string;
  name: string;
  parameters?: string[];
}

export interface SandboxBytecodePackage {
  schema_version: "hew.sandbox.bytecode.v0";
  package_id: string;
  hew_version: string;
  compiler_version: string;
  profile: string;
  source_map: {
    sources: Array<{ id: string; path: string; content_sha256: string }>;
    spans: Array<{
      id: string;
      source_id: string;
      start: SourcePosition;
      end: SourcePosition;
    }>;
  };
  module_graph: {
    entry: string;
    modules: Array<{
      id: string;
      path: string;
      source_id: string;
      imports: Array<{ path: string; resolved_module: string }>;
      functions: string[];
    }>;
  };
  layouts: {
    types: TypeLayout[];
    records: RecordLayout[];
    enums: EnumLayout[];
    actors: unknown[];
    supervisors: unknown[];
    machines: unknown[];
  };
  stdlib_symbols: StdlibSymbol[];
  capabilities: Capability[];
  functions: BytecodeFunction[];
}

export type TrapKind =
  | "integer_overflow"
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
  kind: "panic" | "trap" | "budget_exhausted" | "unsupported" | "internal_error";
  message: string;
  span: TraceSpan | null;
  trap_kind: TrapKind | null;
  unsupported?: UnsupportedDiagnostic;
}

export interface UnsupportedDiagnostic {
  kind: "Unsupported::SANDBOX_OUT_OF_SCOPE" | "Unsupported::M7_DEFERRED" | "Unsupported::NATIVE_ONLY";
  symbol: string;
  status: "unsupported_out_of_scope" | "unsupported_m7_deferred_to_post_v05" | "unsupported_native_only";
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
    sandbox_rejections: unknown[];
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
