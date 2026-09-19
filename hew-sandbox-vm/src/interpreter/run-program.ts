import { runBytecode } from "./interpreter.js";
import type { PackageV1 } from "./v1/package.js";
import type {
  JsonValue,
  RuntimeStatus,
  SandboxRejection,
  SandboxTrace,
  TrapKind,
} from "./types.js";

const SANDBOX_PROFILE = "sandbox-vm-export";

export interface Diagnostic {
  severity: string;
  phase: string;
  message: string;
  kind: string;
  [key: string]: JsonValue;
}

export interface RunProgramResult {
  status: RuntimeStatus;
  compiler_version: string | null;
  hew_version: string | null;
  sandbox_rejections: SandboxRejection[];
  stdout: string;
  exit_code: number;
  diagnostics: Diagnostic[];
}

interface CompileOutput {
  diagnostics: Diagnostic[];
  bytecode: PackageV1 | null;
}

type SandboxCompiler = (
  source: string,
  profile?: string,
) => string | CompileOutput;

declare global {
  var __hewSandboxCompileToSandboxBytecode: SandboxCompiler | undefined;
  var compileToSandboxBytecode: SandboxCompiler | undefined;
}

export function runProgram(source: string, stdin: string): RunProgramResult {
  const compileOutput = compileSource(source);
  if (
    hasErrorDiagnostics(compileOutput.diagnostics) ||
    compileOutput.bytecode === null
  ) {
    return {
      status: "compile_error",
      compiler_version: compileOutput.bytecode?.compiler_version ?? null,
      hew_version: compileOutput.bytecode?.hew_version ?? null,
      sandbox_rejections: [],
      stdout: "",
      exit_code: SandboxExitCode.CompileError,
      diagnostics: compileOutput.diagnostics,
    };
  }

  const bytecode = compileOutput.bytecode;
  const trace = runBytecode(bytecode, {
    fixtureId: "page-run",
    traceId: "trace:page-run",
    replay: { inputs: [{ kind: "stdin", data: stdin }] },
  });

  return {
    status: trace.result,
    compiler_version: bytecode.compiler_version,
    hew_version: bytecode.hew_version,
    sandbox_rejections: trace.final_state.sandbox_rejections,
    stdout: trace.final_state.stdout.join(""),
    exit_code: exitCodeForTrace(trace),
    diagnostics: [...compileOutput.diagnostics, ...runtimeDiagnostics(trace)],
  };
}

function compileSource(source: string): CompileOutput {
  const compiler =
    globalThis.__hewSandboxCompileToSandboxBytecode ??
    globalThis.compileToSandboxBytecode;
  if (!compiler) {
    throw new Error(
      "compile_to_sandbox_bytecode WASM bridge is not initialized",
    );
  }
  const output = compiler(source, SANDBOX_PROFILE);
  const parsed =
    typeof output === "string" ? (JSON.parse(output) as unknown) : output;
  if (!isCompileOutput(parsed)) {
    throw new Error("compile_to_sandbox_bytecode returned an invalid payload");
  }
  return parsed;
}

enum SandboxExitCode {
  CompileError = 1,
  Panic = 101,
  InternalError = 199,
  HeapExceeded = 200,
  IntegerOverflow = 201,
  DivideByZero = 202,
  ShiftOutOfRange = 204,
  IndexOutOfBounds = 205,
  ActorSendFailed = 206,
  MachineDispatchUnreachable = 207,
  ExhaustivenessFallthrough = 208,
}

const TRAP_EXIT_CODES: Readonly<Record<TrapKind, SandboxExitCode>> = {
  integer_overflow: SandboxExitCode.IntegerOverflow,
  shift_out_of_range: SandboxExitCode.ShiftOutOfRange,
  divide_by_zero: SandboxExitCode.DivideByZero,
  invalid_local: SandboxExitCode.InternalError,
  invalid_block: SandboxExitCode.InternalError,
  invalid_call: SandboxExitCode.InternalError,
  invalid_enum_tag: SandboxExitCode.ExhaustivenessFallthrough,
  invalid_record_field: SandboxExitCode.InternalError,
  vector_bounds: SandboxExitCode.IndexOutOfBounds,
  string_bounds: SandboxExitCode.IndexOutOfBounds,
  regex_compile: SandboxExitCode.InternalError,
  capability_missing: SandboxExitCode.InternalError,
  budget_exhausted: SandboxExitCode.HeapExceeded,
  panic: SandboxExitCode.Panic,
  unsupported_instruction: SandboxExitCode.InternalError,
  internal_error: SandboxExitCode.InternalError,
};

function exitCodeForTrace(trace: SandboxTrace): number {
  if (trace.final_state.exit_code !== null) {
    return trace.final_state.exit_code;
  }
  const trapKind = trace.final_state.runtime_failures[0]?.trap_kind;
  return trapKind ? TRAP_EXIT_CODES[trapKind] : SandboxExitCode.InternalError;
}

function runtimeDiagnostics(trace: SandboxTrace): Diagnostic[] {
  return [
    // A package refused at load has no runtime failure to report, so the
    // refusal is what the page shows instead of an empty result.
    ...trace.final_state.sandbox_rejections.map((rejection) => ({
      severity: "error",
      phase: "profile",
      message: rejection.message,
      kind: rejection.code,
      category: rejection.category,
      ...(rejection.capability ? { capability: rejection.capability } : {}),
    })),
    ...trace.final_state.runtime_failures.map((failure) => ({
      severity: "error",
      phase: "run",
      message: failure.message,
      kind: failure.trap_kind ?? failure.kind,
      ...(failure.span ? { span: failure.span as unknown as JsonValue } : {}),
      ...(failure.trap_kind ? { trap_kind: failure.trap_kind } : {}),
    })),
  ];
}

function hasErrorDiagnostics(diagnostics: readonly Diagnostic[]): boolean {
  return diagnostics.some((diagnostic) => diagnostic.severity === "error");
}

function isCompileOutput(value: unknown): value is CompileOutput {
  if (
    typeof value !== "object" ||
    value === null ||
    !("diagnostics" in value) ||
    !("bytecode" in value)
  ) {
    return false;
  }
  const output = value as { diagnostics: unknown; bytecode: unknown };
  return (
    Array.isArray(output.diagnostics) &&
    (output.bytecode === null || typeof output.bytecode === "object")
  );
}
