import Ajv2020 from "ajv/dist/2020.js";
import assert from "node:assert/strict";
import fs from "node:fs";
import path from "node:path";
import test from "node:test";
import { fileURLToPath } from "node:url";
import { runBytecode } from "../dist/interpreter/index.js";

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");
const m4FixtureDirs = [
  "15-actor-counter",
  "16-actor-ask-reply",
  "24-actor-pipeline",
  "25-actor-crash",
  "26-i64-bigint",
  "27-actor-crash-restart-ignored",
  "28-actor-state-arity-trap",
  "29-mixed-scalar-compare-trap"
];

const ajv = new Ajv2020({ allErrors: true, strict: true, validateFormats: false });
const traceSchema = readJson("specs/trace-schema-v0.schema.json");
const validateTrace = ajv.compile(traceSchema);

for (const dir of m4FixtureDirs) {
  test(`M4 conformance: ${dir}`, () => {
    const bytecode = readJson(`fixtures/${dir}/bytecode.json`);
    const expected = readJson(`fixtures/${dir}/expected.trace.json`);

    const actual = runBytecode(bytecode, {
      fixtureId: expected.fixture_id,
      traceId: expected.trace_id,
      replay: expected.replay,
      sandboxVersion: expected.sandbox_version
    });

    assert.ok(validateTrace(actual), ajv.errorsText(validateTrace.errors));
    assert.deepEqual(actual.final_state, expected.final_state, `${dir} final state changed`);
    assert.deepEqual(traceFamilies(actual), traceFamilies(expected), `${dir} trace family sequence changed`);
    assert.deepEqual(actual, expected, `${dir} golden trace changed`);

    const rerun = runBytecode(bytecode, {
      fixtureId: expected.fixture_id,
      traceId: expected.trace_id,
      replay: actual.replay,
      sandboxVersion: expected.sandbox_version
    });
    assert.equal(JSON.stringify(actual), JSON.stringify(rerun), `${dir} replay rerun is not byte-stable`);
  });
}

test("M4 actor fixture coverage is explicit", () => {
  const counter = readJson("fixtures/15-actor-counter/expected.trace.json");
  assert.deepEqual(counter.final_state.ids.actors, ["actor:root", "actor:a1"]);
  assert.ok(traceFamilies(counter).includes("actor.ask"));
  assert.ok(traceFamilies(counter).includes("actor.reply"));

  const pipeline = readJson("fixtures/24-actor-pipeline/expected.trace.json");
  assert.deepEqual(pipeline.final_state.stdout, ["42\n"]);
  assert.deepEqual(pipeline.final_state.ids.actors, ["actor:root", "actor:a1", "actor:a2"]);
  assert.ok(pipeline.events.some((event) => event.text?.includes('"message_id":"message:m2"')));
  assert.ok(traceFamilies(pipeline).includes("actor.send"));
  assert.ok(traceFamilies(pipeline).includes("actor.receive"));

  const crash = readJson("fixtures/25-actor-crash/expected.trace.json");
  assert.equal(crash.final_state.status, "ok");
  assert.deepEqual(crash.final_state.stdout, ["crashed\n"]);
  assert.ok(traceFamilies(crash).includes("actor.crash"));
  assert.ok(traceFamilies(crash).includes("actor.lifecycle"));

  const bigint = readJson("fixtures/26-i64-bigint/expected.trace.json");
  assert.deepEqual(bigint.final_state.stdout, ["9007199254740994\n", "matched\n"]);

  const ignoredRestart = readJson("fixtures/27-actor-crash-restart-ignored/expected.trace.json");
  assert.equal(ignoredRestart.final_state.status, "ok");
  assert.deepEqual(ignoredRestart.final_state.stdout, ["crash hook\n", "stopped\n"]);
  assert.ok(!traceFamilies(ignoredRestart).includes("actor.restart"));

  const arityTrap = readJson("fixtures/28-actor-state-arity-trap/expected.trace.json");
  assert.equal(arityTrap.final_state.status, "ok");
  assert.ok(arityTrap.events.some((event) => event.message === "actor.crash" && event.text?.includes('"trap_kind":"invalid_record_field"')));

  const mixedScalar = readJson("fixtures/29-mixed-scalar-compare-trap/expected.trace.json");
  assert.equal(mixedScalar.final_state.status, "trap");
  assert.equal(mixedScalar.final_state.runtime_failures[0].trap_kind, "invalid_local");
});

test("M4 chaos scheduling is seeded and replay byte-stable", () => {
  const bytecode = readJson("fixtures/24-actor-pipeline/bytecode.json");
  const replay = { seed: 2401, step_budget: 1000, virtual_clock: { epoch_ms: 0, tick_ms: 1, current_ms: 0 }, inputs: [] };
  const chaos = runBytecode(bytecode, {
    fixtureId: "actor-pipeline",
    traceId: "trace:actor-pipeline-chaos",
    replay,
    sandboxVersion: "0.0.0-spec",
    schedulerPolicy: "chaos"
  });
  const chaosRerun = runBytecode(bytecode, {
    fixtureId: "actor-pipeline",
    traceId: "trace:actor-pipeline-chaos",
    replay: chaos.replay,
    sandboxVersion: "0.0.0-spec",
    schedulerPolicy: "chaos"
  });

  assert.equal(JSON.stringify(chaos), JSON.stringify(chaosRerun));
  assert.deepEqual(chaos.final_state.stdout, ["42\n"]);
  assert.ok(chaos.replay.inputs.every((input) => input.kind === "user_event"));
});

test("M5 actor.send mailbox back-pressure drains before enqueueing", () => {
  const trace = runBytecode(mailboxOverflowPackage(), {
    fixtureId: "actor-mailbox-overflow",
    traceId: "trace:actor-mailbox-overflow",
    replay: { seed: 44, step_budget: 1000, virtual_clock: { epoch_ms: 0, tick_ms: 1, current_ms: 0 }, inputs: [] },
    sandboxVersion: "0.0.0-spec"
  });

  assert.equal(trace.result, "ok");
  assert.deepEqual(trace.final_state.runtime_failures, []);
  assert.ok(traceFamilies(trace).includes("actor.send-backpressure"));
  assert.ok(traceFamilies(trace).includes("actor.mailbox-watermark"));
});

function traceFamilies(trace) {
  return trace.events.map((event) => event.message ?? event.type);
}

function readJson(relativePath) {
  return JSON.parse(fs.readFileSync(path.join(root, relativePath), "utf8"));
}

function mailboxOverflowPackage() {
  return {
    schema_version: "hew.sandbox.bytecode.v0",
    package_id: "pkg:actor-mailbox-overflow",
    hew_version: "0.5.0-pre",
    compiler_version: "test",
    profile: "sandbox.educational.v0",
    source_map: { sources: [], spans: [] },
    module_graph: {
      entry: "mod:main",
      modules: [{ id: "mod:main", path: "main.hew", source_id: "src:main", imports: [], functions: ["fn:noop", "fn:main"] }]
    },
    layouts: {
      types: [
        { id: "type:unit", kind: "unit", name: "()" },
        { id: "type:reply", kind: "opaque", name: "ReplyToken" },
        { id: "type:Small", kind: "actor", name: "Small" }
      ],
      records: [],
      enums: [],
      actors: [
        {
          id: "type:Small",
          name: "Small",
          state_fields: [],
          handlers: [{ name: "noop", function: "fn:noop" }],
          max_mailbox: 1
        }
      ],
      supervisors: [],
      machines: []
    },
    stdlib_symbols: [],
    capabilities: [],
    functions: [
      {
        id: "fn:noop",
        module: "mod:main",
        name: "Small.noop",
        params: ["local:noop.reply"],
        result: "type:unit",
        locals: [{ id: "local:noop.reply", name: "reply", type: "type:reply", mutable: false, span: null }],
        blocks: [{ id: "block:noop.entry", params: [], instructions: [], terminator: { op: "return", args: [], span: null }, span: null }],
        span: null
      },
      {
        id: "fn:main",
        module: "mod:main",
        name: "main",
        params: [],
        result: "type:unit",
        locals: [{ id: "local:main.actor", name: "actor", type: "type:Small", mutable: false, span: null }],
        blocks: [
          {
            id: "block:main.entry",
            params: [],
            instructions: [
              { op: "actor.spawn", dst: "local:main.actor", args: [{ kind: "type", value: "type:Small" }], span: null },
              { op: "actor.send", dst: null, args: [{ kind: "local", value: "local:main.actor" }, { kind: "symbol", value: "noop" }], span: null },
              { op: "actor.send", dst: null, args: [{ kind: "local", value: "local:main.actor" }, { kind: "symbol", value: "noop" }], span: null }
            ],
            terminator: { op: "return", args: [], span: null },
            span: null
          }
        ],
        span: null
      }
    ]
  };
}
