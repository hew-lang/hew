import { runBytecode } from "./interpreter.js";
import type { Instruction, JsonValue, Operand, SandboxBytecodePackage, SandboxTrace, TrapKind } from "./types.js";

const SANDBOX_PROFILE = "sandbox-vm-export";
const STDIN_READ_LINE_HELPER = "fn:__hew_sandbox_stdin_read_line";
const STDIN_READ_LINE_SYMBOL = "sym:core.stdin.read_line";
const PROCESS_EXIT_SYMBOL = "sym:core.exit";
const PROCESS_EXIT_CAPABILITY = "core.exit";
const LINE_FEED = 0x0a;
const CARRIAGE_RETURN = 0x0d;

export interface Diagnostic {
  severity: string;
  phase: string;
  message: string;
  kind: string;
  [key: string]: JsonValue;
}

export interface RunProgramResult {
  stdout: string;
  exit_code: number;
  diagnostics: Diagnostic[];
}

interface CompileOutput {
  diagnostics: Diagnostic[];
  bytecode: SandboxBytecodePackage | null;
}

type SandboxCompiler = (source: string, profile?: string) => string | CompileOutput;

declare global {
  var __hewSandboxCompileToSandboxBytecode: SandboxCompiler | undefined;
  var compileToSandboxBytecode: SandboxCompiler | undefined;
}

export function runProgram(source: string, stdin: string): RunProgramResult {
  const compileOutput = compileSource(source);
  if (hasErrorDiagnostics(compileOutput.diagnostics) || compileOutput.bytecode === null) {
    return {
      stdout: "",
      exit_code: SandboxExitCode.CompileError,
      diagnostics: compileOutput.diagnostics
    };
  }

  const bytecode = withPageStdin(compileOutput.bytecode, stdin);
  const trace = runBytecode(bytecode, {
    fixtureId: "page-run",
    traceId: "trace:page-run"
  });

  return {
    stdout: trace.final_state.stdout.join(""),
    exit_code: exitCodeForTrace(trace),
    diagnostics: [...compileOutput.diagnostics, ...runtimeDiagnostics(trace)]
  };
}

function compileSource(source: string): CompileOutput {
  const compiler = globalThis.__hewSandboxCompileToSandboxBytecode ?? globalThis.compileToSandboxBytecode;
  if (!compiler) {
    throw new Error("compile_to_sandbox_bytecode WASM bridge is not initialized");
  }
  const output = compiler(source, SANDBOX_PROFILE);
  const parsed = typeof output === "string" ? (JSON.parse(output) as unknown) : output;
  if (!isCompileOutput(parsed)) {
    throw new Error("compile_to_sandbox_bytecode returned an invalid payload");
  }
  return parsed;
}

function withPageStdin(bytecode: SandboxBytecodePackage, stdin: string): SandboxBytecodePackage {
  const input = new PageStdinBuffer(stdin);
  const patched = {
    ...bytecode,
    capabilities: ensureCapability(bytecode.capabilities, {
      id: PROCESS_EXIT_CAPABILITY,
      disposition: "allowed",
      reason: "main return value maps to the sandbox process exit code",
      required_by: [PROCESS_EXIT_SYMBOL]
    }),
    stdlib_symbols: ensureStdlibSymbol(bytecode.stdlib_symbols, {
      id: PROCESS_EXIT_SYMBOL,
      module: "core",
      name: "exit",
      params: ["type:i64"],
      result: "type:never",
      capability: PROCESS_EXIT_CAPABILITY,
      admission: "allowed"
    }),
    functions: bytecode.functions.map((fn) => ({
      ...fn,
      locals: fn.locals.map((local) => ({ ...local })),
      blocks: fn.blocks.map((block) => ({
        ...block,
        params: [...block.params],
        instructions: block.instructions.map((instruction) => {
          if (fn.id !== STDIN_READ_LINE_HELPER && isStdinReadInstruction(instruction)) {
            return {
              ...instruction,
              op: "const.string",
              args: [{ kind: "literal", value: input.readLine() } satisfies Operand]
            };
          }
          return {
            ...instruction,
            args: instruction.args.map((arg) => ({ ...arg }))
          };
        }),
        terminator: {
          ...block.terminator,
          args: block.terminator.args.map((arg) => ({ ...arg })),
          ...(block.terminator.condition ? { condition: { ...block.terminator.condition } } : {})
        }
      }))
    }))
  };
  return withMainReturnExit(patched);
}

class PageStdinBuffer {
  private readonly bytes: Uint8Array;
  private offset = 0;

  constructor(stdin: string) {
    this.bytes = new TextEncoder().encode(stdin);
  }

  readLine(): string {
    if (this.offset >= this.bytes.length) {
      return "";
    }
    const start = this.offset;
    while (this.offset < this.bytes.length && this.bytes[this.offset] !== LINE_FEED) {
      this.offset += 1;
    }
    let end = this.offset;
    if (this.offset < this.bytes.length && this.bytes[this.offset] === LINE_FEED) {
      this.offset += 1;
    }
    if (end > start && this.bytes[end - 1] === CARRIAGE_RETURN) {
      end -= 1;
    }
    return new TextDecoder().decode(this.bytes.slice(start, end));
  }
}

function withMainReturnExit(bytecode: SandboxBytecodePackage): SandboxBytecodePackage {
  const entryModule = bytecode.module_graph.modules.find((module) => module.id === bytecode.module_graph.entry);
  const mainId = entryModule?.functions.find((id) => bytecode.functions.find((fn) => fn.id === id)?.name === "main");
  if (!mainId) {
    return bytecode;
  }
  return {
    ...bytecode,
    functions: bytecode.functions.map((fn) => {
      if (fn.id !== mainId || fn.result !== "type:i64") {
        return fn;
      }
      return {
        ...fn,
        blocks: fn.blocks.map((block, index) => {
          const lastInstruction = block.instructions.at(-1);
          if (index !== fn.blocks.length - 1 || block.terminator.op !== "trap" || block.terminator.trap_kind !== "internal_error" || !lastInstruction?.dst) {
            return block;
          }
          return {
            ...block,
            instructions: [
              ...block.instructions,
              {
                op: "call.stdlib",
                dst: null,
                args: [
                  { kind: "symbol", value: PROCESS_EXIT_SYMBOL },
                  { kind: "local", value: lastInstruction.dst }
                ],
                span: block.terminator.span
              } satisfies Instruction
            ]
          };
        })
      };
    })
  };
}

function ensureCapability(
  capabilities: SandboxBytecodePackage["capabilities"],
  capability: SandboxBytecodePackage["capabilities"][number]
): SandboxBytecodePackage["capabilities"] {
  return capabilities.some((item) => item.id === capability.id)
    ? capabilities.map((item) => ({ ...item, required_by: [...item.required_by] }))
    : [...capabilities.map((item) => ({ ...item, required_by: [...item.required_by] })), capability];
}

function ensureStdlibSymbol(
  symbols: SandboxBytecodePackage["stdlib_symbols"],
  symbol: SandboxBytecodePackage["stdlib_symbols"][number]
): SandboxBytecodePackage["stdlib_symbols"] {
  return symbols.some((item) => item.id === symbol.id) ? symbols.map((item) => ({ ...item, params: [...item.params] })) : [...symbols.map((item) => ({ ...item, params: [...item.params] })), symbol];
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
  ExhaustivenessFallthrough = 208
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
  internal_error: SandboxExitCode.InternalError
};

function exitCodeForTrace(trace: SandboxTrace): number {
  if (trace.final_state.exit_code !== null) {
    return trace.final_state.exit_code;
  }
  const trapKind = trace.final_state.runtime_failures[0]?.trap_kind;
  return trapKind ? TRAP_EXIT_CODES[trapKind] : SandboxExitCode.InternalError;
}

function runtimeDiagnostics(trace: SandboxTrace): Diagnostic[] {
  return trace.final_state.runtime_failures.map((failure) => ({
    severity: "error",
    phase: "run",
    message: failure.message,
    kind: failure.trap_kind ?? failure.kind,
    ...(failure.span ? { span: failure.span as unknown as JsonValue } : {}),
    ...(failure.trap_kind ? { trap_kind: failure.trap_kind } : {})
  }));
}

function hasErrorDiagnostics(diagnostics: readonly Diagnostic[]): boolean {
  return diagnostics.some((diagnostic) => diagnostic.severity === "error");
}

function symbolOperandValue(operand: { kind: string; value: JsonValue } | undefined): string | null {
  return operand?.kind === "symbol" && typeof operand.value === "string" ? operand.value : null;
}

function functionOperandValue(operand: { kind: string; value: JsonValue } | undefined): string | null {
  return operand?.kind === "function" && typeof operand.value === "string" ? operand.value : null;
}

function isStdinReadInstruction(instruction: Instruction): boolean {
  return (
    (instruction.op === "call.stdlib" && symbolOperandValue(instruction.args[0]) === STDIN_READ_LINE_SYMBOL) ||
    (instruction.op === "call.direct" && functionOperandValue(instruction.args[0]) === STDIN_READ_LINE_HELPER)
  );
}

function isCompileOutput(value: unknown): value is CompileOutput {
  if (typeof value !== "object" || value === null || !("diagnostics" in value) || !("bytecode" in value)) {
    return false;
  }
  const output = value as { diagnostics: unknown; bytecode: unknown };
  return Array.isArray(output.diagnostics) && (output.bytecode === null || typeof output.bytecode === "object");
}
