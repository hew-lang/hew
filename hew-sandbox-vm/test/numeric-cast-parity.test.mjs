// A `cast` must match native width, signedness, truncation and saturation, and
// an f32 must round at single precision.
//
// Every expected value here was measured by running the equivalent Hew source
// through `hew run`, not carried over from an earlier expectation. The packages
// are assembled here rather than mutated from a fixture, so the cases do not
// depend on an unrelated fixture's shape; the skeleton is the one the emitter
// produces for `println(x as T)`: a constant, the casts, then a `Print` of the
// target's own kind.

import assert from "node:assert/strict";
import test from "node:test";
import { runBytecode } from "../dist/interpreter/index.js";

/// `println(<constant> as <to> ...)`: one function, one cast chain, one print.
function castPackage(constant, casts, printKind) {
  const ops = [{ ...constant, dst: 0, span: null }];
  for (const [step, cast] of casts.entries()) {
    ops.push({
      op: "cast",
      dst: step + 1,
      value: step,
      from: cast.from,
      to: cast.to,
      span: null
    });
  }

  return {
    schema_version: "hew.sandbox.bytecode.v1",
    hew_version: "0.6.0-rc4",
    compiler_version: "numeric-cast-parity-test",
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
            ops,
            term: {
              op: "runtime.call",
              family: 0,
              args: [{ value: casts.length, decision: "borrow" }],
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

function runCast(constant, casts, printKind) {
  const trace = runBytecode(castPackage(constant, casts, printKind), {
    fixtureId: "numeric-cast",
    traceId: "trace:numeric-cast",
    replay: {
      seed: 42,
      step_budget: 1000,
      virtual_clock: { epoch_ms: 0, tick_ms: 1, current_ms: 0 },
      inputs: []
    }
  });
  assert.equal(trace.result, "ok", JSON.stringify(trace.final_state.runtime_failures));
  assert.deepEqual(trace.final_state.runtime_failures, []);
  return trace.final_state.stdout.join("").trim();
}

const int = (value, ty) => ({ op: "const.int", value, ty });

test("cast preserves native integer width and signedness", () => {
  // `let a: i8 = -1; println(a as u16)` — sign extension then truncation.
  assert.equal(runCast(int("-1", "i8"), [{ from: "i8", to: "u16" }, { from: "u16", to: "u32" }], "U32"), "65535");
  // u64::MAX reinterpreted as i64. The sandbox profile refuses this literal in
  // source, so the package is the only way to reach the case.
  assert.equal(runCast(int("18446744073709551615", "u64"), [{ from: "u64", to: "i64" }], "I64"), "-1");
  // isize and usize are 64-bit in the VM, matching native.
  assert.equal(
    runCast(int("4294967296", "i64"), [{ from: "i64", to: "isize" }, { from: "isize", to: "i64" }], "I64"),
    "4294967296"
  );
  assert.equal(
    runCast(int("4294967296", "u64"), [{ from: "u64", to: "usize" }, { from: "usize", to: "u64" }], "U64"),
    "4294967296"
  );
});

test("cast saturates float to integer like native", () => {
  assert.equal(runCast({ op: "const.float", value: 300.9, ty: "f64" }, [{ from: "f64", to: "u8" }], "U8"), "255");
  assert.equal(
    runCast(
      { op: "const.float", value: -200.5, ty: "f64" },
      [{ from: "f64", to: "i8" }, { from: "i8", to: "i32" }],
      "I32"
    ),
    "-128"
  );
});

test("cast handles bool and char conversions exactly", () => {
  assert.equal(runCast({ op: "const.bool", value: true }, [{ from: "bool", to: "i32" }], "I32"), "1");
  assert.equal(runCast(int("2", "i64"), [{ from: "i64", to: "bool" }], "Bool"), "true");
  assert.equal(runCast({ op: "const.char", value: "A" }, [{ from: "char", to: "u32" }], "U32"), "65");
});

test("cast rounds through IEEE single precision", () => {
  // The round trip back to i64 is what makes the rounding observable: printing
  // the f64 renders 6 significant digits and hides which value it holds.
  const roundTrip = (value) =>
    runCast(int(value, "i64"), [{ from: "i64", to: "f32" }, { from: "f32", to: "i64" }], "I64");

  // 16777217 is the first integer f32 cannot hold, so it rounds down.
  assert.equal(roundTrip("16777217"), "16777216");
  // Negative controls: both neighbours are representable and survive intact,
  // so the case above is single-precision rounding and not a blanket clamp.
  assert.equal(roundTrip("16777216"), "16777216");
  assert.equal(roundTrip("16777218"), "16777218");
});

test("an f32 literal is held at single precision from the start", () => {
  // `let b: f32 = 16777217.0; println(b as i64)` reads 16777216 natively. The
  // constant carries its own type, so the value never holds a digit a single
  // cannot represent rather than being corrected at the first cast.
  const literal = (value, ty) =>
    runCast({ op: "const.float", value, ty }, [{ from: ty, to: "i64" }], "I64");

  assert.equal(literal(16777217, "f32"), "16777216");
  // Negative controls: representable neighbours are untouched.
  assert.equal(literal(16777216, "f32"), "16777216");
  assert.equal(literal(16777218, "f32"), "16777218");
  // An f64 literal keeps every digit, so the rounding follows the declared
  // type rather than being applied to floats in general.
  assert.equal(literal(16777217, "f64"), "16777217");
});

test("a float prints with native's 6 significant digits", () => {
  assert.equal(
    runCast(int("16777217", "i64"), [{ from: "i64", to: "f32" }, { from: "f32", to: "f64" }], "F64"),
    "1.67772e+07"
  );
});
