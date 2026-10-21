import fs from "node:fs";
import process from "node:process";
import path from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import { runBytecode } from "./index.js";

interface CliArgs {
  bytecodePath: string;
  seed: number;
  /// A file whose contents are the program's standard input.
  stdinPath: string | undefined;
}

const DEFAULT_SEED = 42;
const DEFAULT_STEP_BUDGET = 1_000_000;

function parseArgs(argv: string[]): CliArgs {
  let bytecodePath: string | undefined;
  let seed = DEFAULT_SEED;
  let stdinPath: string | undefined;

  for (let index = 0; index < argv.length; index += 1) {
    const arg = argv[index];
    if (arg === "--seed") {
      const value = argv[index + 1];
      if (!value) {
        throw new Error("--seed requires an integer value");
      }
      seed = Number.parseInt(value, 10);
      if (!Number.isSafeInteger(seed)) {
        throw new Error(`invalid --seed value: ${value}`);
      }
      index += 1;
      continue;
    }
    if (arg === "--stdin") {
      stdinPath = argv[index + 1];
      if (!stdinPath) {
        throw new Error("--stdin requires a file path");
      }
      index += 1;
      continue;
    }
    if (arg.startsWith("-")) {
      throw new Error(`unknown argument: ${arg}`);
    }
    if (bytecodePath) {
      throw new Error(`unexpected extra argument: ${arg}`);
    }
    bytecodePath = arg;
  }

  if (!bytecodePath) {
    throw new Error(
      "usage: parity-runner <bytecode.json> [--seed <integer>] [--stdin <file>]",
    );
  }

  return { bytecodePath, seed, stdinPath };
}

try {
  const args = parseArgs(process.argv.slice(2));
  const bytecode = JSON.parse(
    fs.readFileSync(args.bytecodePath, "utf8"),
  ) as unknown;
  let codecBridge: ((request: string) => string) | undefined;
  if (
    typeof bytecode === "object" &&
    bytecode !== null &&
    "codecs" in bytecode &&
    Array.isArray(bytecode.codecs) &&
    bytecode.codecs.length
  ) {
    const local = path.resolve(
      path.dirname(fileURLToPath(import.meta.url)),
      "../../../hew-wasm/pkg/hew_wasm.js",
    );
    const location =
      process.env.HEW_WASM_MODULE ??
      (fs.existsSync(local) ? pathToFileURL(local).href : "@hew-lang/wasm");
    const wasm = await import(location);
    if (typeof wasm.default === "function") {
      const resolved = location.startsWith("file:")
        ? location
        : import.meta.resolve(location);
      await wasm.default({
        module_or_path: fs.readFileSync(
          new URL(resolved.replace(/\.js$/, "_bg.wasm")),
        ),
      });
    }
    codecBridge = wasm.executeCodec ?? wasm.default?.executeCodec;
    if (typeof codecBridge !== "function") {
      throw new Error(
        "hew-wasm executeCodec export is missing; supply a compatible HEW_WASM_MODULE",
      );
    }
  }
  const trace = await runBytecode(bytecode, {
    codecBridge,
    replay: {
      seed: args.seed,
      step_budget: DEFAULT_STEP_BUDGET,
      virtual_clock: { epoch_ms: 0, tick_ms: 1, current_ms: 0 },
      inputs:
        args.stdinPath === undefined
          ? []
          : [{ kind: "stdin", data: fs.readFileSync(args.stdinPath, "utf8") }],
    },
  });

  process.stdout.write(trace.final_state.stdout.join(""));
  process.stderr.write(trace.final_state.stderr.join(""));
  if (
    trace.final_state.exit_code === null &&
    trace.final_state.stderr.length === 0
  ) {
    process.stderr.write(
      `parity-runner: ${JSON.stringify({
        status: trace.result,
        diagnostics: trace.final_state.diagnostics,
        rejections: trace.final_state.sandbox_rejections,
        failures: trace.final_state.runtime_failures,
      })}\n`,
    );
  }
  process.exitCode = trace.final_state.exit_code ?? 1;
} catch (error) {
  const message = error instanceof Error ? error.message : String(error);
  process.stderr.write(`parity-runner: ${message}\n`);
  process.exitCode = 1;
}
