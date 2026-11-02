// Sandbox VM host for the differential parity oracle.
//
// Reads one JSON request per line on stdin ({"source": "..."}), compiles it
// through the browser compiler package and runs the package on the sandbox
// VM with the parity runner's replay settings. Writes one JSON reply per line.
// The oracle bounds each request with a deadline and replaces this process
// when one is exceeded, so a request never needs its own timer.
import fs from "node:fs";
import path from "node:path";
import readline from "node:readline";
import { fileURLToPath, pathToFileURL } from "node:url";

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "../..");
const vm = await import(
  pathToFileURL(path.join(root, "hew-sandbox-vm/dist/interpreter/index.js")).href
);
const location =
  process.env.HEW_WASM_MODULE ??
  pathToFileURL(path.join(root, "hew-wasm/pkg/hew_wasm.js")).href;
const wasm = await import(location);
if (typeof wasm.default === "function") {
  const resolved = location.startsWith("file:")
    ? location
    : import.meta.resolve(location);
  await wasm.default({
    module_or_path: fs.readFileSync(new URL(resolved.replace(/\.js$/, "_bg.wasm"))),
  });
}
const compile = wasm.compileToSandboxBytecode ?? wasm.default?.compileToSandboxBytecode;
const codecBridge = wasm.executeCodec ?? wasm.default?.executeCodec;
const seed = Number.parseInt(process.env.HEW_SEED ?? "42", 10);
const stepBudget = Number.parseInt(process.env.HEW_PARITY_STEP_BUDGET ?? "5000000", 10);

async function handle(request) {
  const compiled = JSON.parse(compile(request.source, "sandbox-vm-export", undefined));
  if (compiled.error !== undefined) {
    return { phase: "compile", error: compiled.error };
  }
  const errors = (compiled.diagnostics ?? []).filter(
    (diagnostic) => diagnostic.severity === "error",
  );
  if (!compiled.bytecode || errors.length) {
    return { phase: "compile", diagnostics: errors.length ? errors : compiled.diagnostics };
  }
  const trace = await vm.runBytecode(compiled.bytecode, {
    codecBridge,
    replay: {
      seed,
      step_budget: stepBudget,
      virtual_clock: { epoch_ms: 0, tick_ms: 1, current_ms: 0 },
      inputs: [],
    },
  });
  const state = trace.final_state;
  return {
    phase: "run",
    status: trace.result,
    stdout: state.stdout.join(""),
    stderr: state.stderr.join(""),
    exit_code: state.exit_code,
    rejections: state.sandbox_rejections,
    failures: state.runtime_failures,
  };
}

process.stdout.write(JSON.stringify({ ready: true }) + "\n");
const lines = readline.createInterface({ input: process.stdin, crlfDelay: Infinity });
for await (const line of lines) {
  if (!line.trim()) continue;
  let reply;
  try {
    reply = await handle(JSON.parse(line));
  } catch (error) {
    reply = { phase: "host", error: error instanceof Error ? error.stack : String(error) };
  }
  process.stdout.write(JSON.stringify(reply) + "\n");
}
