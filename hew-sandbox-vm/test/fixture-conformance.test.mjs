import Ajv2020 from "ajv/dist/2020.js";
import assert from "node:assert/strict";
import fs from "node:fs";
import path from "node:path";
import test from "node:test";
import { fileURLToPath } from "node:url";
import { runBytecode } from "../dist/interpreter/index.js";

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");
const m3FixtureDirs = fs.readdirSync(path.join(root, "fixtures")).filter((name) => fs.existsSync(path.join(root, "fixtures", name, "bytecode.json")));

const ajv = new Ajv2020({ allErrors: true, strict: true, validateFormats: false });
const traceSchema = readJson("specs/trace-schema-v0.schema.json");
const validateTrace = ajv.compile(traceSchema);
// A package declares which schema describes it, so the suite validates each
// fixture against its own rather than against one the emitter may have left.
const validateBytecodeBySchema = {
  "hew.sandbox.bytecode.v1": ajv.compile(readJson("bytecode/sandbox-bytecode-v1.schema.json"))
};

function validateBytecodePackage(bytecode, label) {
  const validate = validateBytecodeBySchema[bytecode.schema_version];
  assert.ok(validate, `${label} declares an unknown schema ${bytecode.schema_version}`);
  assert.ok(validate(bytecode), ajv.errorsText(validate.errors));
}

for (const dir of m3FixtureDirs) {
  test(`compiled source replay: ${dir}`, () => {
    const bytecode = readJson(`fixtures/${dir}/bytecode.json`);
    const expected = readJson(`fixtures/${dir}/expected.trace.json`);

    assert.ok(bytecode.functions.length > 0, `${dir} bytecode fixture must not be empty`);
    validateBytecodePackage(bytecode, dir);

    const actual = runBytecode(bytecode, {
      fixtureId: expected.fixture_id,
      traceId: expected.trace_id,
      replay: expected.replay,
      sandboxVersion: expected.sandbox_version
    });

    assert.ok(validateTrace(actual), ajv.errorsText(validateTrace.errors));
    assert.deepEqual(
      actual.events.map((event) => event.type),
      expected.events.map((event) => event.type),
      `${dir} trace event sequence changed`
    );
    assert.deepEqual(actual.final_state, expected.final_state, `${dir} final state changed`);
    assert.deepEqual(actual, expected, `${dir} golden trace changed`);

    const rerun = runBytecode(bytecode, {
      fixtureId: expected.fixture_id,
      traceId: expected.trace_id,
      replay: expected.replay,
      sandboxVersion: expected.sandbox_version
    });
    assert.equal(JSON.stringify(actual), JSON.stringify(rerun), `${dir} trace is not byte-stable across reruns`);
  });
}

test("Vec::get returns None rather than trapping when the index is past the end", () => {
  const bytecode = readJson("fixtures/06-vector-basics/bytecode.json");
  // The fixture reads index 1, which is in range. Move the read past the end
  // through the constant that feeds it: `get` answers None, where an indexing
  // read of the same element would take its out-of-bounds unwind edge.
  const index = bytecode.functions
    .flatMap((fn) => fn.blocks)
    .flatMap((block) => block.ops)
    .find((op) => op.op === "const.int" && op.value === "1");
  assert.ok(index, "vector fixture must read a constant index");
  index.value = "99";

  const trace = runBytecode(bytecode, {
    fixtureId: "vector-get-none",
    traceId: "trace:vector-get-none",
    replay: {
      seed: 6,
      step_budget: 1000,
      virtual_clock: { epoch_ms: 0, tick_ms: 1, current_ms: 0 },
      inputs: []
    }
  });

  assert.equal(trace.result, "ok");
  assert.deepEqual(trace.final_state.stdout, ["2\n", "-1\n"]);
  assert.deepEqual(trace.final_state.runtime_failures, []);
});

test("the published schema admits the emitted package and rejects an unknown opcode", () => {
  const emitted = readJson("fixtures/06-vector-basics/bytecode.json");
  validateBytecodePackage(emitted, "06-vector-basics");

  const unknownOp = structuredClone(emitted);
  const op = unknownOp.functions
    .flatMap((fn) => fn.blocks)
    .flatMap((block) => block.ops)
    .find((entry) => entry.op === "const.int");
  assert.ok(op, "vector fixture must carry an instruction to corrupt");
  op.op = "const.not_real";
  const validate = validateBytecodeBySchema[unknownOp.schema_version];
  assert.equal(validate(unknownOp), false, "an opcode outside the registry must not validate");

  const unknownTerminator = structuredClone(emitted);
  unknownTerminator.functions[0].blocks[0].term.op = "goto.not_real";
  assert.equal(
    validate(unknownTerminator),
    false,
    "a terminator outside the registry must not validate"
  );
});

function readJson(relativePath) {
  return JSON.parse(fs.readFileSync(path.join(root, relativePath), "utf8"));
}
