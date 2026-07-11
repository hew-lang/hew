import assert from "node:assert/strict";
import test from "node:test";
import { runBytecode } from "../dist/interpreter/index.js";

// Regression coverage for #2341: the sandbox VM's wrapping arithmetic opcodes
// (`&+`/`&-`/`&*` -> i64.add/sub/mul) must truncate their result to
// two's-complement 64 bits, matching native LLVM which wraps silently on
// overflow. Before the fix these opcodes fed the raw (unbounded) BigInt to
// `this.i64()`, so an overflowing operation produced a mathematically
// unbounded value (e.g. `i64::MAX &+ 1` yielded 2^63 instead of `i64::MIN`).
//
// These fixtures are handcrafted inline bytecode (same approach as
// unsupported-gates.test.mjs) rather than compiler output, so they run without
// the wasm bridge. Each pins one boundary case where wrapping and native
// arithmetic diverge; ordinary non-overflow wrapping is already covered by the
// arithmetic parity fixtures.

const I64_MIN = "-9223372036854775808";
const I64_MAX = "9223372036854775807";

// Build a minimal single-function package: two i64 constants, one wrapping
// op, then println(result). `lhs`/`rhs` are decimal string literals so the
// full 64-bit range is expressible (const.i64 accepts string literals for
// values outside the JS safe-integer range, as in fixture 26-i64-bigint).
function wrappingOpPackage(op, lhs, rhs) {
  return {
    schema_version: "hew.sandbox.bytecode.v0",
    package_id: "pkg:wrapping-arith",
    hew_version: "0.6.0-pre",
    compiler_version: "test",
    profile: "sandbox.educational.v0",
    source_map: { sources: [], spans: [] },
    module_graph: {
      entry: "mod:main",
      modules: [{ id: "mod:main", path: "main.hew", source_id: "src:main", imports: [], functions: ["fn:main"] }]
    },
    layouts: {
      types: [
        { id: "type:unit", kind: "unit", name: "()" },
        { id: "type:i64", kind: "integer", name: "i64" },
        { id: "type:string", kind: "string", name: "String" }
      ],
      records: [],
      enums: [],
      actors: [],
      supervisors: [],
      machines: []
    },
    stdlib_symbols: [
      {
        id: "sym:core.stdout.println",
        module: "core.stdout",
        name: "println",
        params: ["type:i64"],
        result: "type:unit",
        capability: "core.stdout",
        admission: "allowed"
      }
    ],
    capabilities: [
      {
        id: "core.stdout",
        disposition: "allowed",
        reason: "wrapping-arith parity test stdout",
        required_by: ["sym:core.stdout.println"]
      }
    ],
    functions: [
      {
        id: "fn:main",
        module: "mod:main",
        name: "main",
        params: [],
        result: "type:unit",
        locals: [
          { id: "local:main.lhs", name: null, type: "type:i64", mutable: false, span: null },
          { id: "local:main.rhs", name: null, type: "type:i64", mutable: false, span: null },
          { id: "local:main.res", name: null, type: "type:i64", mutable: false, span: null },
          { id: "local:main.unit", name: null, type: "type:unit", mutable: false, span: null }
        ],
        blocks: [
          {
            id: "block:main.entry",
            params: [],
            instructions: [
              { op: "const.i64", dst: "local:main.lhs", args: [{ kind: "literal", value: lhs }], span: null },
              { op: "const.i64", dst: "local:main.rhs", args: [{ kind: "literal", value: rhs }], span: null },
              {
                op,
                dst: "local:main.res",
                args: [
                  { kind: "local", value: "local:main.lhs" },
                  { kind: "local", value: "local:main.rhs" }
                ],
                span: null
              },
              {
                op: "call.stdlib",
                dst: null,
                args: [
                  { kind: "symbol", value: "sym:core.stdout.println" },
                  { kind: "local", value: "local:main.res" }
                ],
                span: null
              },
              { op: "const.unit", dst: "local:main.unit", args: [], span: null }
            ],
            terminator: { op: "return", args: [{ kind: "local", value: "local:main.unit" }], span: null },
            span: null
          }
        ],
        span: null
      }
    ]
  };
}

function runWrapping(op, lhs, rhs) {
  const trace = runBytecode(wrappingOpPackage(op, lhs, rhs), {
    fixtureId: "wrapping-arith",
    traceId: "trace:wrapping-arith",
    replay: { seed: 42, step_budget: 1000, virtual_clock: { epoch_ms: 0, tick_ms: 1, current_ms: 0 }, inputs: [] }
  });
  assert.equal(
    trace.result,
    "ok",
    `expected ok but got ${trace.result}: ${JSON.stringify(trace.final_state.runtime_failures)}`
  );
  assert.deepEqual(trace.final_state.runtime_failures, []);
  return trace.final_state.stdout.join("").trim();
}

test("i64.add wraps i64::MAX &+ 1 to i64::MIN (two's-complement)", () => {
  assert.equal(runWrapping("i64.add", I64_MAX, "1"), I64_MIN);
});

test("i64.sub wraps i64::MIN &- 1 to i64::MAX (two's-complement)", () => {
  assert.equal(runWrapping("i64.sub", I64_MIN, "1"), I64_MAX);
});

test("i64.mul wraps i64::MAX &* 2 to -2 (two's-complement)", () => {
  // i64::MAX * 2 = 2^64 - 2; asIntN(64) -> -2.
  assert.equal(runWrapping("i64.mul", I64_MAX, "2"), "-2");
});

test("i64.mul wraps i64::MIN &* -1 back to i64::MIN (magnitude unrepresentable)", () => {
  // -(i64::MIN) = 2^63 is unrepresentable; two's-complement leaves i64::MIN.
  assert.equal(runWrapping("i64.mul", I64_MIN, "-1"), I64_MIN);
});

test("non-overflow wrapping arithmetic still matches native", () => {
  // Negative control: ordinary in-range wrapping ops are unchanged by the
  // truncation (asIntN(64) is the identity on values already in range).
  assert.equal(runWrapping("i64.add", "2", "3"), "5");
  assert.equal(runWrapping("i64.sub", "10", "4"), "6");
  assert.equal(runWrapping("i64.mul", "-6", "7"), "-42");
});
