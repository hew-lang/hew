import { runBytecode } from "./interpreter.js";
import { buildPlaygroundState } from "./playground.js";
import { runProgram } from "./run-program.js";
import { BytecodeValidationError } from "./schema-validator.js";
import type { RunOptions } from "./types.js";

type WorkerRunOptions = Omit<
  RunOptions,
  "signal" | "codecBridge" | "calendarBridge"
>;

interface RunBytecodeRequest {
  kind?: "runBytecode";
  id?: string;
  bytecode: unknown;
  options?: WorkerRunOptions;
}

interface RunSourceRequest {
  kind: "run";
  id?: string;
  source: string;
  options?: WorkerRunOptions;
  stdin: string;
}

type WorkerRequest = RunBytecodeRequest | RunSourceRequest;

const worker = globalThis as unknown as DedicatedWorkerGlobalScope;

worker.addEventListener(
  "message",
  async (event: MessageEvent<WorkerRequest>) => {
    const { id } = event.data;
    try {
      if (event.data.kind === "run") {
        const result = await runProgram(
          event.data.source,
          event.data.stdin,
          undefined,
          event.data.options,
        );
        worker.postMessage({ id, ok: true, result });
        return;
      }
      const { bytecode, options } = event.data;
      const trace = await runBytecode(bytecode, {
        ...options,
        codecBridge: globalThis.__hewSandboxExecuteCodec,
        calendarBridge: globalThis.__hewSandboxExecuteCalendar,
      });
      worker.postMessage({
        id,
        ok: true,
        trace,
        playground: buildPlaygroundState(trace),
      });
    } catch (error) {
      if (error instanceof BytecodeValidationError) {
        worker.postMessage({
          id,
          ok: false,
          error: "bytecode_validation",
          diagnostics: error.errors,
        });
        return;
      }
      worker.postMessage({
        id,
        ok: false,
        error: "interpreter_error",
        message: String(error),
      });
    }
  },
);
