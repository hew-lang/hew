import Ajv2020 from "ajv/dist/2020.js";
import assert from "node:assert/strict";
import fs from "node:fs";
import path from "node:path";
import test from "node:test";
import { fileURLToPath } from "node:url";
import { runBytecode } from "../dist/interpreter/index.js";

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");
const m3FixtureDirs = [
  "01-hello-world",
  "02-arithmetic-checked",
  "03-branch-loop",
  "04-record-fields",
  "05-enum-match",
  "06-vector-basics",
  "07-string-interpolation",
  "08-regex-match",
  "11-runtime-panic",
  "12-divide-by-zero-trap",
  "13-step-budget-exhausted",
  "23-direct-call"
];

const ajv = new Ajv2020({ allErrors: true, strict: true, validateFormats: false });
const traceSchema = readJson("specs/trace-schema-v0.schema.json");
const bytecodeSchema = readJson("bytecode/sandbox-bytecode-v0.schema.json");
const validateTrace = ajv.compile(traceSchema);
const validateBytecode = ajv.compile(bytecodeSchema);

for (const dir of m3FixtureDirs) {
  test(`M3 conformance: ${dir}`, () => {
    const bytecode = readJson(`fixtures/${dir}/bytecode.json`);
    const expected = readJson(`fixtures/${dir}/expected.trace.json`);

    assert.ok(bytecode.functions.length > 0, `${dir} bytecode fixture must not be empty`);
    assert.ok(validateBytecode(bytecode), ajv.errorsText(validateBytecode.errors));

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

test("M3 fixture subset is explicit and complete", () => {
  for (const dir of m3FixtureDirs) {
    assert.ok(fs.existsSync(path.join(root, "fixtures", dir, "bytecode.json")), `${dir} is missing bytecode.json`);
  }
});

test("Vec::get returns Option::None instead of trapping out of bounds", () => {
  const bytecode = readJson("fixtures/06-vector-basics/bytecode.json");
  const get = bytecode.functions
    .flatMap((fn) => fn.blocks)
    .flatMap((block) => block.instructions)
    .find((instruction) => instruction.op === "vector.get");
  assert.ok(get, "vector fixture must contain vector.get");
  get.args[1] = { kind: "literal", value: 99 };

  const trace = runBytecode(bytecode, {
    fixtureId: "vector-get-none",
    traceId: "trace:vector-get-none",
    replay: { seed: 6, step_budget: 1000, virtual_clock: { epoch_ms: 0, tick_ms: 1, current_ms: 0 }, inputs: [] }
  });

  assert.equal(trace.result, "ok");
  assert.deepEqual(trace.final_state.stdout, ["2\n", "-1\n"]);
  assert.deepEqual(trace.final_state.runtime_failures, []);
});

test("published bytecode schema admits vector.index and vector.get but rejects unknown opcodes", () => {
  const emittedIndexing = structuredClone(readJson("fixtures/06-vector-basics/bytecode.json"));
  const indexedInstruction = emittedIndexing.functions
    .flatMap((fn) => fn.blocks)
    .flatMap((block) => block.instructions)
    .find((instruction) => instruction.op === "vector.get");
  assert.ok(indexedInstruction, "vector fixture must contain compiler-shaped vector access");

  // The compiler's direct indexing lowering uses the same vector/index operand
  // shape as this emitted Vec::get fixture, but changes the opcode to the raw,
  // bounds-trapping `vector.index` contract.
  indexedInstruction.op = "vector.index";
  assert.ok(validateBytecode(emittedIndexing), ajv.errorsText(validateBytecode.errors));

  const optionGet = structuredClone(emittedIndexing);
  optionGet.functions[0].blocks[0].instructions.find(
    (instruction) => instruction.op === "vector.index"
  ).op = "vector.get";
  assert.ok(validateBytecode(optionGet), ajv.errorsText(validateBytecode.errors));

  const unknownOpcode = structuredClone(emittedIndexing);
  unknownOpcode.functions[0].blocks[0].instructions.find(
    (instruction) => instruction.op === "vector.index"
  ).op = "vector.not_real";
  assert.equal(validateBytecode(unknownOpcode), false);
});

function readJson(relativePath) {
  return JSON.parse(fs.readFileSync(path.join(root, relativePath), "utf8"));
}
