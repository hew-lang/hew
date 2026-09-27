import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import test from "node:test";
import { fileURLToPath } from "node:url";

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");
const runner = path.join(root, "cli/test-runner.mjs");

for (const [fixture, result, status] of [
  ["01-hello-world", "ok", 0],
  ["11-runtime-panic", "panic", 1],
]) {
  test(`test runner exports the complete ${result} trace`, () => {
    const directory = fs.mkdtempSync(path.join(os.tmpdir(), "hew-test-trace-"));
    try {
      const tracePath = path.join(directory, "trace.json");
      const reportPath = path.join(directory, "report.json");
      const bytecode = path.join(root, "fixtures", fixture, "bytecode.json");
      const run = spawnSync(process.execPath, [
        runner, bytecode, "--schedule", "fifo", "--seed", "11", "--step-budget", "1000",
      ], {
        encoding: "utf8",
        env: { ...process.env, HEW_TEST_TRACE_PATH: tracePath, HEW_TEST_REPORT: reportPath },
      });

      assert.equal(run.status, status, run.stderr);
      const traceText = fs.readFileSync(tracePath, "utf8");
      const trace = JSON.parse(traceText);
      const report = JSON.parse(fs.readFileSync(reportPath, "utf8"));
      assert.equal(trace.schema_version, "hew.sandbox.trace.v0");
      assert.equal(trace.result, result);
      assert.ok(Array.isArray(trace.events) && trace.events.length > 0);
      assert.ok(trace.final_state && trace.replay);
      assert.equal(report.status, status);
      assert.equal(traceText.endsWith("\n"), true);
      if (status !== 0) {
        assert.equal(report.outcome, "fault");
        assert.equal(trace.final_state.runtime_failures[0].kind, "panic");
      }
    } finally {
      fs.rmSync(directory, { recursive: true, force: true });
    }
  });
}
