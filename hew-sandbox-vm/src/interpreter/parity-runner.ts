import fs from "node:fs";
import process from "node:process";
import { runBytecode } from "./index.js";

interface CliArgs {
  bytecodePath: string;
  seed: number;
}

const DEFAULT_SEED = 42;
const DEFAULT_STEP_BUDGET = 1_000_000;

function parseArgs(argv: string[]): CliArgs {
  let bytecodePath: string | undefined;
  let seed = DEFAULT_SEED;

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
    if (arg.startsWith("-")) {
      throw new Error(`unknown argument: ${arg}`);
    }
    if (bytecodePath) {
      throw new Error(`unexpected extra argument: ${arg}`);
    }
    bytecodePath = arg;
  }

  if (!bytecodePath) {
    throw new Error("usage: parity-runner <bytecode.json> [--seed <integer>]");
  }

  return { bytecodePath, seed };
}

try {
  const args = parseArgs(process.argv.slice(2));
  const bytecode = JSON.parse(fs.readFileSync(args.bytecodePath, "utf8")) as unknown;
  const trace = runBytecode(bytecode, {
    replay: {
      seed: args.seed,
      step_budget: DEFAULT_STEP_BUDGET,
      virtual_clock: { epoch_ms: 0, tick_ms: 1, current_ms: 0 },
      inputs: []
    }
  });

  process.stdout.write(trace.final_state.stdout.join(""));
  process.exitCode = trace.final_state.exit_code ?? 1;
} catch (error) {
  const message = error instanceof Error ? error.message : String(error);
  process.stderr.write(`parity-runner: ${message}\n`);
  process.exitCode = 1;
}
