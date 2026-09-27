import fs from "node:fs";
import process from "node:process";
import { runBytecode } from "../dist/interpreter/index.js";

function parseArgs(args) {
  if (args.length !== 7 || args[1] !== "--schedule" || args[3] !== "--seed" || args[5] !== "--step-budget") {
    throw new Error("usage: test-runner <bytecode.json> --schedule fifo|random --seed N --step-budget N");
  }
  const schedule = args[2];
  if (schedule !== "fifo" && schedule !== "random") throw new Error(`unknown schedule: ${schedule}`);
  const seed = BigInt(args[4]);
  const budget = Number(args[6]);
  if (!Number.isSafeInteger(budget) || budget <= 0) throw new Error("step budget must be a positive safe integer");
  return { path: args[0], schedule, seed, budget };
}

const reportPath = process.env.HEW_TEST_REPORT;
try {
  const args = parseArgs(process.argv.slice(2));
  const packageValue = JSON.parse(fs.readFileSync(args.path, "utf8"));
  const trace = runBytecode(packageValue, {
    fixtureId: "hew-test",
    traceId: "trace:hew-test",
    schedulerPolicy: args.schedule === "random" ? "chaos" : "round_robin",
    replay: {
      seed: args.seed.toString(),
      step_budget: args.budget,
      virtual_clock: { epoch_ms: 0, tick_ms: 1, current_ms: 0 },
      inputs: [],
    },
  });
  const final = trace.final_state;
  const rejection = final.sandbox_rejections[0];
  const fault = final.runtime_failures[0];
  const passed = trace.result === "ok" && final.exit_code === 0;
  const outcome = passed ? "passed" : rejection ? "refused" : fault ? "fault" : "exit";
  const message = rejection?.message ?? fault?.message ?? (passed ? null : `VM result: ${trace.result}`);
  const faultKind = fault?.kind === "panic" ? "UserPanic" : fault?.kind ?? null;
  const report = {
    version: 1,
    outcome,
    status: passed ? 0 : 1,
    fault_kind: faultKind,
    fault_code: null,
    message,
    site_offset: null,
    assertion: fault?.assertion ?? null,
    schedule: args.schedule,
    seed: args.seed.toString(),
    steps: final.step_count,
    virtual_time_ms: final.virtual_clock.current_ms,
  };
  if (reportPath) fs.writeFileSync(reportPath, JSON.stringify(report));
  process.stdout.write(final.stdout.join(""));
  process.stderr.write(final.stderr.join(""));
  if (message) process.stderr.write(`${message}\n`);
  process.exitCode = report.status;
} catch (error) {
  const message = error instanceof Error ? error.message : String(error);
  if (reportPath) fs.writeFileSync(reportPath, JSON.stringify({ version: 1, outcome: "launch", status: 1, message }));
  process.stderr.write(`hew test VM: ${message}\n`);
  process.exitCode = 1;
}
