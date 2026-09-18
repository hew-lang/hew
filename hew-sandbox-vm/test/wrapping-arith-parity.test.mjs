// Wrapping arithmetic (`&+`, `&-`, `&*`) must truncate to two's complement at
// the operand's width, matching native LLVM, which wraps silently on overflow.
// Regression coverage for #2341, where an overflowing wrap produced a
// mathematically unbounded value instead of the wrapped one.
//
// Every expected value here was measured by running the equivalent Hew source
// through `hew run`. The package is assembled here so the cases run without the
// wasm bridge; the skeleton is the one the emitter produces for
// `println(a &+ b)`: two constants, the `binary` op carrying its operand type,
// then a `Print` of that type's kind.

import assert from "node:assert/strict";
import test from "node:test";
import { runBytecode } from "../dist/interpreter/index.js";

const I64_MIN = "-9223372036854775808";
const I64_MAX = "9223372036854775807";

function wrappingPackage(binaryOp, lhs, rhs, ty, printKind) {
  return {
    schema_version: "hew.sandbox.bytecode.v1",
    hew_version: "0.6.0-rc4",
    compiler_version: "wrapping-arith-parity-test",
    profile: "sandbox-vm-export",
    entry: { function: 0, exit: "unit" },
    strings: [],
    bytes: [],
    regex_patterns: [],
    aggregates: [],
    variants: [],
    runtime_families: [{ id: 0, family: "Print", detail: { kind: printKind, newline: true } }],
    externs: [],
    suspend_kinds: [],
    value_capabilities: [],
    closures: [],
    vtables: [],
    functions: [
      {
        id: 0,
        name: "main",
        params: [],
        entry: 0,
        places: [],
        blocks: [
          {
            id: 0,
            params: [],
            ops: [
              { op: "const.int", dst: 0, value: lhs, ty, span: null },
              { op: "const.int", dst: 1, value: rhs, ty, span: null },
              { op: "binary", dst: 2, binary_op: binaryOp, lhs: 0, rhs: 1, ty, span: null }
            ],
            term: {
              op: "runtime.call",
              family: 0,
              args: [{ value: 2, decision: "borrow" }],
              result: null,
              result_shape: null,
              normal: { to: 1, args: [] },
              unwind: null,
              span: null
            }
          },
          { id: 1, params: [], ops: [], term: { op: "return", value: null, span: null } }
        ]
      }
    ]
  };
}

function runWrapping(binaryOp, lhs, rhs, ty = "i64", printKind = "I64") {
  const trace = runBytecode(wrappingPackage(binaryOp, lhs, rhs, ty, printKind), {
    fixtureId: "wrapping-arith",
    traceId: "trace:wrapping-arith",
    replay: { seed: 42, step_budget: 1000, virtual_clock: { epoch_ms: 0, tick_ms: 1, current_ms: 0 }, inputs: [] }
  });
  assert.equal(trace.result, "ok", JSON.stringify(trace.final_state.runtime_failures));
  assert.deepEqual(trace.final_state.runtime_failures, []);
  return trace.final_state.stdout.join("").trim();
}

test("WrappingAdd wraps i64::MAX &+ 1 to i64::MIN", () => {
  assert.equal(runWrapping("WrappingAdd", I64_MAX, "1"), I64_MIN);
});

test("WrappingSub wraps i64::MIN &- 1 to i64::MAX", () => {
  assert.equal(runWrapping("WrappingSub", I64_MIN, "1"), I64_MAX);
});

test("WrappingMul wraps i64::MAX &* 2 to -2", () => {
  // i64::MAX * 2 is 2^64 - 2, which truncates to -2.
  assert.equal(runWrapping("WrappingMul", I64_MAX, "2"), "-2");
});

test("WrappingMul wraps i64::MIN &* -1 back to i64::MIN", () => {
  // -(i64::MIN) is 2^63, unrepresentable, so two's complement leaves i64::MIN.
  assert.equal(runWrapping("WrappingMul", I64_MIN, "-1"), I64_MIN);
});

test("wrapping arithmetic truncates at the operand's own width", () => {
  // The operand type the package records is what wraps, so a narrower type
  // wraps sooner: 127 &+ 1 leaves i8 at its minimum rather than reaching 128.
  assert.equal(runWrapping("WrappingAdd", "127", "1", "i8", "I32"), "-128");
  assert.equal(runWrapping("WrappingAdd", "255", "1", "u8", "U8"), "0");
  assert.equal(runWrapping("WrappingMul", "2147483647", "2", "i32", "I32"), "-2");
});

test("non-overflow wrapping arithmetic still matches native", () => {
  // Negative controls: in-range wrapping ops are unchanged by the truncation.
  assert.equal(runWrapping("WrappingAdd", "2", "3"), "5");
  assert.equal(runWrapping("WrappingSub", "10", "4"), "6");
  assert.equal(runWrapping("WrappingMul", "-6", "7"), "-42");
});
