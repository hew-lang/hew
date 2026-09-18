// `import std.math` lowers every function to a `runtime.call` of the
// `MathIntrinsic` family, keyed by its intrinsic name. Native emits the LLVM
// intrinsic, so these cases pin the IEEE behaviour rather than whatever JS
// spells similarly.
//
// Every expected value was measured by running the equivalent Hew source
// through `hew run`. Values print through `Print{kind: F64}`, which is `%g` at
// six significant digits, the same as native.

import assert from "node:assert/strict";
import test from "node:test";
import { runBytecode } from "../dist/interpreter/index.js";

/// `println(math.<key>(args...))` for one call.
function mathPackage(key, args, printKind) {
  const ops = args.map((arg, at) =>
    arg.kind === "int"
      ? { op: "const.int", dst: at, value: arg.value, ty: arg.ty ?? "i64", span: null }
      : { op: "const.float", dst: at, value: arg.value, ty: "f64", span: null }
  );

  return {
    schema_version: "hew.sandbox.bytecode.v1",
    hew_version: "0.6.0-rc4",
    compiler_version: "math-intrinsic-test",
    profile: "sandbox-vm-export",
    entry: { function: 0, exit: "unit" },
    strings: [],
    bytes: [],
    regex_patterns: [],
    aggregates: [],
    variants: [],
    runtime_families: [
      { id: 0, family: "MathIntrinsic", detail: key },
      { id: 1, family: "Print", detail: { kind: printKind, newline: true } }
    ],
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
              args: args.map((_, at) => ({ value: at, decision: "copy" })),
              result: { value: args.length, own: "owned" },
              result_shape: null,
              normal: { to: 1, args: [] },
              unwind: null,
              span: null
            }
          },
          {
            id: 1,
            params: [],
            ops: [],
            term: {
              op: "runtime.call",
              family: 1,
              args: [{ value: args.length, decision: "borrow" }],
              result: null,
              result_shape: null,
              normal: { to: 2, args: [] },
              unwind: null,
              span: null
            }
          },
          { id: 2, params: [], ops: [], term: { op: "return", value: null, span: null } }
        ]
      }
    ]
  };
}

function math(key, args, printKind = "F64") {
  const trace = runBytecode(mathPackage(key, args, printKind), {
    fixtureId: "math-intrinsic",
    traceId: "trace:math-intrinsic",
    replay: { seed: 1, step_budget: 1000, virtual_clock: { epoch_ms: 0, tick_ms: 1, current_ms: 0 }, inputs: [] }
  });
  assert.equal(trace.result, "ok", JSON.stringify(trace.final_state.runtime_failures));
  return trace.final_state.stdout.join("").trim();
}

const f = (value) => ({ kind: "float", value });
const i = (value) => ({ kind: "int", value: String(value) });
const int64 = (key, args) => math(key, args, "I64");

test("the elementary f64 intrinsics match native", () => {
  assert.equal(math("Sqrt", [f(16)]), "4");
  assert.equal(math("Sqrt", [f(2)]), "1.41421");
  assert.equal(math("Exp", [f(1)]), "2.71828");
  assert.equal(math("Log", [f(2.718281828459045)]), "1");
  assert.equal(math("Sin", [f(0.5)]), "0.479426");
  assert.equal(math("Cos", [f(0.5)]), "0.877583");
  assert.equal(math("Tan", [f(0.5)]), "0.546302");
  assert.equal(math("Asin", [f(0.5)]), "0.523599");
  assert.equal(math("Acos", [f(0.5)]), "1.0472");
  assert.equal(math("Atan", [f(0.5)]), "0.463648");
  assert.equal(math("Sinh", [f(0.5)]), "0.521095");
  assert.equal(math("Cosh", [f(0.5)]), "1.12763");
  assert.equal(math("Tanh", [f(0.5)]), "0.462117");
  assert.equal(math("Exp2", [f(10)]), "1024");
  assert.equal(math("Log2", [f(1024)]), "10");
  assert.equal(math("Log10", [f(1000)]), "3");
  assert.equal(math("Log1p", [f(1e-7)]), "1e-07");
  assert.equal(math("Expm1", [f(1e-7)]), "1e-07");
  assert.equal(math("Cbrt", [f(27)]), "3");
  assert.equal(math("AbsF64", [f(-3.5)]), "3.5");
});

test("the two- and three-argument f64 intrinsics match native", () => {
  assert.equal(math("Pow", [f(2), f(10)]), "1024");
  assert.equal(math("Atan2", [f(1), f(1)]), "0.785398");
  assert.equal(math("Hypot", [f(3), f(4)]), "5");
  assert.equal(math("Powi", [f(2), i(10)]), "1024");
  assert.equal(math("Powi", [f(1.5), i(-2)]), "0.444444");
  assert.equal(math("Fma", [f(2), f(3), f(4)]), "10");
  assert.equal(math("FromBits", [i("4636737291354636288")]), "100");
});

test("rounding follows the IEEE operation, not the JS one of the same name", () => {
  assert.equal(math("Floor", [f(2.7)]), "2");
  assert.equal(math("Floor", [f(-2.7)]), "-3");
  assert.equal(math("Ceil", [f(2.1)]), "3");
  assert.equal(math("Ceil", [f(-2.1)]), "-2");
  assert.equal(math("Trunc", [f(2.9)]), "2");
  assert.equal(math("Trunc", [f(-2.9)]), "-2");

  // `Round` is half away from zero. `Math.round` ties toward positive
  // infinity, so it would read -2 for -2.5 and 1 for 0.49999999999999994.
  assert.equal(math("Round", [f(2.5)]), "3");
  assert.equal(math("Round", [f(-2.5)]), "-3");
  assert.equal(math("Round", [f(3.5)]), "4");
  assert.equal(math("Round", [f(1.5)]), "2");
  assert.equal(math("Round", [f(-1.5)]), "-2");
  assert.equal(math("Round", [f(-0.5)]), "-1");
  assert.equal(math("Round", [f(0.49999999999999994)]), "0");
});

test("min and max let a NaN operand lose, as minnum and maxnum do", () => {
  // `Math.min`/`Math.max` propagate NaN; the LLVM intrinsics return the other
  // operand, which is what native prints.
  assert.equal(math("MinF64", [f(NaN), f(3)]), "3");
  assert.equal(math("MaxF64", [f(NaN), f(3)]), "3");
  assert.equal(math("MinF64", [f(3), f(NaN)]), "3");
  assert.equal(math("MaxF64", [f(3), f(NaN)]), "3");
  assert.equal(math("MinF64", [f(NaN), f(NaN)]), "nan");
  // Negative control: ordinary operands still order normally.
  assert.equal(math("MinF64", [f(3.5), f(7.5)]), "3.5");
  assert.equal(math("MaxF64", [f(3.5), f(7.5)]), "7.5");
});

test("copysign and the signed zeros match native", () => {
  assert.equal(math("Copysign", [f(3), f(-1)]), "-3");
  assert.equal(math("Copysign", [f(-3), f(1)]), "3");
  assert.equal(math("Copysign", [f(0), f(-1)]), "-0");
  assert.equal(math("Round", [f(-0)]), "-0");
  assert.equal(math("Trunc", [f(-0.5)]), "-0");
  // Operands that compare equal resolve to the first, so the sign of a zero
  // travels through min and max the way native's does.
  assert.equal(math("MinF64", [f(0), f(-0)]), "0");
  assert.equal(math("MinF64", [f(-0), f(0)]), "-0");
  assert.equal(math("MaxF64", [f(0), f(-0)]), "0");
  assert.equal(math("MaxF64", [f(-0), f(0)]), "-0");
});

test("the integer intrinsics work in the integer domain", () => {
  assert.equal(int64("AbsI64", [i(-5)]), "5");
  assert.equal(int64("AbsI64", [i(5)]), "5");
  assert.equal(int64("AbsI64", [i("-9223372036854775807")]), "9223372036854775807");
  assert.equal(int64("MinI64", [i(3), i(7)]), "3");
  assert.equal(int64("MaxI64", [i(3), i(7)]), "7");
  // Beyond the JS safe range, so a float-domain implementation would lose
  // digits here.
  assert.equal(int64("MaxI64", [i("9007199254740993"), i("9007199254740992")]), "9007199254740993");
});

test("fma is fused: the product is not rounded before the addend arrives", () => {
  // Native reads 4.93038e-32 for the fused form and 0 for the unfused one, so
  // this case fails on any implementation that computes a * b + c in two
  // roundings.
  assert.equal(math("Fma", [f(1.0000000000000002), f(1.0000000000000002), f(-1.0000000000000004)]), "4.93038e-32");
});

test("a math intrinsic the VM has no shim for is refused at load", () => {
  const trace = runBytecode(
    {
      ...mathPackage("Sqrt", [f(16)], "F64"),
      runtime_families: [
        { id: 0, family: "MathIntrinsic", detail: "NotAnIntrinsic" },
        { id: 1, family: "Print", detail: { kind: "F64", newline: true } }
      ]
    },
    {
      fixtureId: "math-reject",
      traceId: "trace:math-reject",
      replay: { seed: 1, step_budget: 1000, virtual_clock: { epoch_ms: 0, tick_ms: 1, current_ms: 0 }, inputs: [] }
    }
  );

  assert.equal(trace.result, "sandbox_rejected");
  assert.deepEqual(trace.final_state.stdout, []);
  assert.equal(trace.final_state.step_count, 0);
  assert.equal(trace.final_state.sandbox_rejections[0].capability, "MathIntrinsic::NotAnIntrinsic");
});
