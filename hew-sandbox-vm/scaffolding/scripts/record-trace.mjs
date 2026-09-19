// Record a fixture's golden trace by running its bytecode on the VM.
//
// The replay configuration is a property of the fixture, not of the run, so it
// is read from the existing golden and carried through unchanged: the same
// seed, step budget, virtual clock and inputs. Only `events` and `final_state`
// are regenerated. This is what `cargo run -p xtask -- sandbox-fixtures
// --record` invokes; a golden is never hand-edited.
import fs from "node:fs";
import path from "node:path";
import process from "node:process";
import { fileURLToPath } from "node:url";

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "../..");
const { runBytecode } = await import(path.join(root, "dist/interpreter/index.js"));

const [bytecodePath, tracePath] = process.argv.slice(2);
if (!bytecodePath || !tracePath) {
  process.stderr.write("usage: record-trace.mjs <bytecode.json> <expected.trace.json>\n");
  process.exit(2);
}

const bytecode = JSON.parse(fs.readFileSync(bytecodePath, "utf8"));
const previous = JSON.parse(fs.readFileSync(tracePath, "utf8"));

const trace = runBytecode(bytecode, {
  fixtureId: previous.fixture_id,
  traceId: previous.trace_id,
  replay: { ...previous.replay, inputs: previous.replay.inputs.filter((input) => input.kind !== "user_event" || input.data?.family !== "actor.scheduler-step") },
  sandboxVersion: previous.sandbox_version
});

process.stdout.write(`${JSON.stringify(trace, null, 2)}\n`);
