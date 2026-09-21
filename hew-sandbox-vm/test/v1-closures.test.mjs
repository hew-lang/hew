// Closures: `closure.make` gathers the captures into an environment, and
// calling the closure through `indirect.call` hands that environment to the
// body's first parameter, where a `capture` place reaches its field.
//
// The package under test is the shape of this program, which prints `9` then
// `7` natively:
//
//     fn read(f: fn() -> i64) -> i64 { f() }
//     fn main() {
//         var n = 7;
//         let get = || n;
//         n = 9;
//         println(n);
//         println(read(get));
//     }
//
// The `9` is the negative control: the assignment landed, so the `7` is the
// capture holding the value it was made with rather than the store failing.

import assert from "node:assert/strict";
import test from "node:test";
import { runBytecode } from "../dist/interpreter/index.js";

const PRINT_I64 = { id: 0, family: "Print", detail: { kind: "I64", newline: true } };

/// `captureAt` names the value the closure captures, so a test can make the
/// same closure over a different reading of the same place.
function closurePackage(captureAt = 1) {
  return {
    schema_version: "hew.sandbox.bytecode.v1",
    hew_version: "0.6.0-rc4",
    compiler_version: "v1-closure-test",
    profile: "sandbox-vm-export",
    entry: { function: 0, exit: "unit" },
    strings: [],
    bytes: [],
    regex_patterns: [],
    aggregates: [],
    variants: [],
    runtime_families: [PRINT_I64],
    externs: [],
    suspend_kinds: [],
    value_capabilities: [],
    closures: [{ id: 0, body: 1, fields: 1 }],
    vtables: [],
    functions: [
      // main
      {
        id: 0,
        name: "main",
        params: [],
        entry: 0,
        places: [
          { id: 0, origin: "local" },
          { id: 1, origin: "local" }
        ],
        blocks: [
          {
            id: 0,
            params: [],
            ops: [
              { op: "const.int", dst: 0, value: "7", ty: "i64", span: null },
              { op: "alloc_place", place: 0, span: null },
              { op: "store.init", place: 0, value: 0, span: null },
              { op: "load.copy", dst: 1, place: 0, span: null },
              { op: "const.int", dst: 9, value: "9", ty: "i64", span: null },
              { op: "store.assign", place: 0, value: 9, span: null },
              { op: "load.copy", dst: 10, place: 0, span: null },
              { op: "closure.make", dst: 2, closure: 0, fields: [captureAt], span: null },
              { op: "alloc_place", place: 1, span: null },
              { op: "store.init", place: 1, value: 2, span: null }
            ],
            term: printTerm(10, 1)
          },
          {
            id: 1,
            params: [],
            ops: [
              { op: "load.take", dst: 5, place: 1, span: null },
              { op: "callable.coerce", dst: 6, source: 5, span: null }
            ],
            term: {
              op: "call",
              callee: 2,
              args: [{ value: 6, decision: "borrow" }],
              result: { value: 7, own: "none" },
              normal: { to: 2, args: [] },
              unwind: null,
              span: null
            }
          },
          { id: 2, params: [], ops: [], term: printTerm(7, 3) },
          { id: 3, params: [], ops: [], term: { op: "return", value: null, span: null } }
        ]
      },
      // The closure body. Its first parameter is the environment, and place 0
      // is one field of it.
      {
        id: 1,
        name: "main$closure",
        params: [{ value: 0, own: "guaranteed" }],
        entry: 0,
        places: [{ id: 0, origin: "capture", environment: 0, field: 0 }],
        blocks: [
          {
            id: 0,
            params: [],
            ops: [{ op: "load.copy", dst: 1, place: 0, span: null }],
            term: { op: "return", value: { value: 1, decision: "move" }, span: null }
          }
        ]
      },
      // read(f: fn() -> i64) -> i64
      {
        id: 2,
        name: "read",
        params: [{ value: 0, own: "guaranteed" }],
        entry: 0,
        places: [],
        blocks: [
          {
            id: 0,
            params: [],
            ops: [],
            term: {
              op: "indirect.call",
              callee: { value: 0, decision: "borrow" },
              args: [],
              result: { value: 1, own: "none" },
              normal: { to: 1, args: [] },
              unwind: null,
              span: null
            }
          },
          {
            id: 1,
            params: [],
            ops: [],
            term: { op: "return", value: { value: 1, decision: "move" }, span: null }
          }
        ]
      }
    ]
  };
}

function printTerm(value, to) {
  return {
    op: "runtime.call",
    family: 0,
    args: [{ value, decision: "copy" }],
    result: null,
    result_shape: null,
    normal: { to, args: [] },
    unwind: null,
    span: null
  };
}

function run(bytecode) {
  return runBytecode(bytecode, {
    fixtureId: "v1-closure",
    traceId: "trace:v1-closure",
    replay: { seed: 1, step_budget: 10_000, virtual_clock: { epoch_ms: 0, tick_ms: 1, current_ms: 0 }, inputs: [] }
  });
}

function stdout(trace) {
  return trace.final_state.stdout.join("");
}

test("a closure carries the value it captured, not the place it came from", () => {
  const trace = run(closurePackage());

  assert.equal(trace.result, "ok", JSON.stringify(trace.final_state.runtime_failures));
  // 9 is the reassigned place; 7 is what the closure captured before it.
  assert.equal(stdout(trace), "9\n7\n");
  assert.equal(trace.final_state.exit_code, 0);
});

test("a closure made from a later reading captures that later value", () => {
  // Negative control for the case above: capturing value 10, the reading taken
  // after the store, yields 9. So the 7 there is the captured value travelling
  // with the closure and not a stale read the VM cannot tell apart.
  const trace = run(closurePackage(10));

  assert.equal(trace.result, "ok", JSON.stringify(trace.final_state.runtime_failures));
  assert.equal(stdout(trace), "9\n9\n");
});

test("two closures over one body keep their own environments", () => {
  // Both closures name the same body; only the environment differs, so calling
  // each in turn must print what each captured.
  const pkg = closurePackage();
  const main = pkg.functions[0];
  main.places.push({ id: 2, origin: "local" });
  main.blocks[0].ops.push(
    { op: "closure.make", dst: 11, closure: 0, fields: [10], span: null },
    { op: "alloc_place", place: 2, span: null },
    { op: "store.init", place: 2, value: 11, span: null }
  );
  main.blocks[2].term = {
    op: "runtime.call",
    family: 0,
    args: [{ value: 7, decision: "copy" }],
    result: null,
    result_shape: null,
    normal: { to: 4, args: [] },
    unwind: null,
    span: null
  };
  main.blocks.push(
    {
      id: 4,
      params: [],
      ops: [{ op: "load.take", dst: 12, place: 2, span: null }],
      term: {
        op: "call",
        callee: 2,
        args: [{ value: 12, decision: "borrow" }],
        result: { value: 13, own: "none" },
        normal: { to: 5, args: [] },
        unwind: null,
        span: null
      }
    },
    { id: 5, params: [], ops: [], term: printTerm(13, 3) }
  );

  const trace = run(pkg);
  assert.equal(trace.result, "ok", JSON.stringify(trace.final_state.runtime_failures));
  assert.equal(stdout(trace), "9\n7\n9\n");
});

test("an indirect call on a value that is not callable names what it got", () => {
  const pkg = closurePackage();
  // Hand `read` an integer where its callable belongs.
  pkg.functions[0].blocks[1].ops = [{ op: "const.int", dst: 6, value: "1", ty: "i64", span: null }];

  const trace = run(pkg);
  assert.equal(trace.result, "runtime_failure");
  assert.match(trace.final_state.runtime_failures[0].message, /expected a callable value, got i64/);
});
