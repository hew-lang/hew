import { runBytecode } from "./interpreter.js";
import { buildPlaygroundState } from "./playground.js";
import { runProgram } from "./run-program.js";
import { BytecodeValidationError } from "./schema-validator.js";
import type { RunOptions } from "./types.js";

interface RunBytecodeRequest {
  kind?: "runBytecode";
  id?: string;
  bytecode: unknown;
  options?: RunOptions;
}

interface RunSourceRequest {
  kind: "run";
  id?: string;
  source: string;
  stdin: string;
}

type WorkerRequest = RunBytecodeRequest | RunSourceRequest;

const worker = globalThis as unknown as DedicatedWorkerGlobalScope;

worker.addEventListener("message", (event: MessageEvent<WorkerRequest>) => {
  const { id } = event.data;
  try {
    if (event.data.kind === "run") {
      const result = runProgram(event.data.source, event.data.stdin);
      worker.postMessage({ id, ok: true, result });
      return;
    }
    const { bytecode, options } = event.data;
    const trace = runBytecode(bytecode, options);
    worker.postMessage({ id, ok: true, trace, playground: buildPlaygroundState(trace) });
  } catch (error) {
    if (error instanceof BytecodeValidationError) {
      worker.postMessage({ id, ok: false, error: "bytecode_validation", diagnostics: error.errors });
      return;
    }
    worker.postMessage({ id, ok: false, error: "interpreter_error", message: String(error) });
  }
});
