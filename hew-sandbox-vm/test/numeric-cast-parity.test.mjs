import assert from "node:assert/strict";
import fs from "node:fs";
import test from "node:test";
import { runBytecode } from "../dist/interpreter/index.js";

const basePackage = JSON.parse(
  fs.readFileSync(new URL("../fixtures/01-hello-world/bytecode.json", import.meta.url), "utf8")
);

function runCast(sourceOp, sourceValue, from, to) {
  const bytecode = structuredClone(basePackage);
  const fn = bytecode.functions[0];
  fn.locals = [
    { id: "local:main.source", name: null, type: "type:i64", mutable: false, span: null },
    { id: "local:main.result", name: null, type: "type:i64", mutable: false, span: null },
    { id: "local:main.unit", name: null, type: "type:unit", mutable: false, span: null }
  ];
  fn.blocks = [
    {
      id: "block:entry",
      params: [],
      span: null,
      instructions: [
        {
          op: sourceOp,
          dst: "local:main.source",
          args: [{ kind: "literal", value: sourceValue }],
          span: null
        },
        {
          op: "numeric.cast",
          dst: "local:main.result",
          args: [
            { kind: "local", value: "local:main.source" },
            { kind: "symbol", value: from },
            { kind: "symbol", value: to }
          ],
          span: null
        },
        {
          op: "call.stdlib",
          dst: null,
          args: [
            { kind: "symbol", value: "sym:core.stdout.println" },
            { kind: "local", value: "local:main.result" }
          ],
          span: null
        },
        { op: "const.unit", dst: "local:main.unit", args: [], span: null }
      ],
      terminator: {
        op: "return",
        args: [{ kind: "local", value: "local:main.unit" }],
        span: null
      }
    }
  ];

  const trace = runBytecode(bytecode, {
    fixtureId: `numeric-cast-${from}-${to}`,
    traceId: `trace:numeric-cast-${from}-${to}`,
    replay: {
      seed: 42,
      step_budget: 1000,
      virtual_clock: { epoch_ms: 0, tick_ms: 1, current_ms: 0 },
      inputs: []
    }
  });
  assert.equal(trace.result, "ok");
  assert.deepEqual(trace.final_state.runtime_failures, []);
  return trace.final_state.stdout.join("").trim();
}

test("numeric.cast preserves native integer width and signedness", () => {
  assert.equal(runCast("const.i64", "-1", "i8", "u16"), "65535");
  assert.equal(runCast("const.u64", "18446744073709551615", "u64", "i64"), "-1");
  assert.equal(runCast("const.i64", "4294967296", "i64", "isize"), "4294967296");
  assert.equal(runCast("const.u64", "4294967296", "u64", "usize"), "4294967296");
});

test("numeric.cast saturates float to integer like native", () => {
  assert.equal(runCast("const.f64", 300.9, "f64", "u8"), "255");
  assert.equal(runCast("const.f64", -200.5, "f64", "i8"), "-128");
});

test("numeric.cast handles bool and char conversions exactly", () => {
  assert.equal(runCast("const.bool", true, "bool", "i32"), "1");
  assert.equal(runCast("const.i64", 2, "i64", "bool"), "true");
  assert.equal(runCast("const.string", "A", "char", "u32"), "65");
});

test("numeric.cast rounds f32 through IEEE single precision", () => {
  assert.equal(runCast("const.i64", "16777217", "i64", "f32"), "1.67772e+07");
  assert.equal(runCast("const.f64", 16777217, "f32", "f64"), "1.67772e+07");
});
