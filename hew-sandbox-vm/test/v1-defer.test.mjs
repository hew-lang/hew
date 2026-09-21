// Defer: `register_defer` reserves the places a deferred body reaches, and
// `enter_defer` / `finish_defer` bound that body while the scope drains. SIR
// has already elaborated the drain into the CFG in reverse registration order,
// so the VM follows the edges; what it owes is the fault park, which keeps a
// body reached while unwinding from reading the fault it is cleaning up after.

import assert from "node:assert/strict";
import test from "node:test";
import { runBytecode } from "../dist/interpreter/index.js";

const PRINT_LN = { id: 0, family: "Print", detail: { kind: "Str", newline: true } };

function pkg(strings, blocks) {
  return {
    schema_version: "hew.sandbox.bytecode.v1",
    hew_version: "0.6.0-rc4",
    compiler_version: "v1-defer-test",
    profile: "sandbox-vm-export",
    entry: { function: 0, exit: "unit" },
    strings,
    bytes: [],
    regex_patterns: [],
    aggregates: [],
    variants: [],
    runtime_families: [PRINT_LN],
    externs: [],
    suspend_kinds: [],
    value_capabilities: [],
    closures: [],
    vtables: [],
    functions: [{ id: 0, name: "main", params: [], entry: 0, places: [], blocks }]
  };
}

function printTerm(value, to) {
  return {
    op: "runtime.call",
    family: 0,
    args: [{ value, decision: "borrow" }],
    result: null,
    result_shape: null,
    normal: { to, args: [] },
    unwind: null,
    span: null
  };
}

function run(bytecode) {
  return runBytecode(bytecode, {
    fixtureId: "v1-defer",
    traceId: "trace:v1-defer",
    replay: { seed: 1, step_budget: 10_000, virtual_clock: { epoch_ms: 0, tick_ms: 1, current_ms: 0 }, inputs: [] }
  });
}

function stdout(trace) {
  return trace.final_state.stdout.join("");
}

test("deferred bodies run in reverse registration order as the scope ends", () => {
  // The shape of:
  //
  //     fn main() {
  //         defer println("third: cleanup done");
  //         defer println("second: closing resources");
  //         println("first: doing work");
  //     }
  const trace = run(
    pkg(
      ["first: doing work", "second: closing resources", "third: cleanup done"],
      [
        {
          id: 0,
          params: [],
          ops: [
            { op: "register_defer", defer: 1, scope: 0, dependencies: [], span: null },
            { op: "register_defer", defer: 2, scope: 0, dependencies: [], span: null },
            { op: "const.str", dst: 0, str: 0, span: null }
          ],
          term: printTerm(0, 1)
        },
        {
          id: 1,
          params: [],
          ops: [{ op: "destroy_value", value: 0, span: null }],
          term: { op: "enter_defer", defer: 2, park: 0, body: { to: 2, args: [] }, span: null }
        },
        {
          id: 2,
          params: [],
          ops: [{ op: "const.str", dst: 1, str: 1, span: null }],
          term: printTerm(1, 3)
        },
        {
          id: 3,
          params: [],
          ops: [{ op: "destroy_value", value: 1, span: null }],
          term: { op: "finish_defer", defer: 2, park: 0, next: { to: 4, args: [] }, span: null }
        },
        {
          id: 4,
          params: [],
          ops: [],
          term: { op: "enter_defer", defer: 1, park: 0, body: { to: 5, args: [] }, span: null }
        },
        {
          id: 5,
          params: [],
          ops: [{ op: "const.str", dst: 2, str: 2, span: null }],
          term: printTerm(2, 6)
        },
        {
          id: 6,
          params: [],
          ops: [{ op: "destroy_value", value: 2, span: null }],
          term: { op: "finish_defer", defer: 1, park: 0, next: { to: 7, args: [] }, span: null }
        },
        {
          id: 7,
          params: [],
          ops: [],
          term: { op: "cleanup.dispatch", normal: { to: 8, args: [] }, fault: { to: 9, args: [] }, span: null }
        },
        { id: 8, params: [], ops: [], term: { op: "return", value: null, span: null } },
        { id: 9, params: [], ops: [], term: { op: "resume_unwind", span: null } }
      ]
    )
  );

  assert.equal(trace.result, "ok", JSON.stringify(trace.final_state.runtime_failures));
  assert.equal(
    stdout(trace),
    "first: doing work\nsecond: closing resources\nthird: cleanup done\n"
  );
  assert.equal(trace.final_state.exit_code, 0);
});

/// A deferred body whose own `cleanup.dispatch` reports which edge it took, so
/// a test can see whether the enclosing fault was parked. `faulting` chooses
/// whether the body is reached by a panic or on the ordinary path.
function parkPackage(faulting) {
  const entry = faulting
    ? { op: "panic", message: { value: 0, decision: "copy" }, cleanup: { to: 1, args: [] }, span: null }
    : { op: "goto", edge: { to: 1, args: [] }, span: null };

  return pkg(
    ["boom", "body saw no fault", "body saw a fault"],
    [
      {
        id: 0,
        params: [],
        ops: [
          { op: "register_defer", defer: 1, scope: 0, dependencies: [], span: null },
          { op: "const.str", dst: 0, str: 0, span: null }
        ],
        term: entry
      },
      {
        id: 1,
        params: [],
        ops: [],
        term: { op: "enter_defer", defer: 1, park: 0, body: { to: 2, args: [] }, span: null }
      },
      {
        id: 2,
        params: [],
        ops: [],
        term: { op: "cleanup.dispatch", normal: { to: 3, args: [] }, fault: { to: 4, args: [] }, span: null }
      },
      {
        id: 3,
        params: [],
        ops: [{ op: "const.str", dst: 1, str: 1, span: null }],
        term: printTerm(1, 5)
      },
      {
        id: 4,
        params: [],
        ops: [{ op: "const.str", dst: 2, str: 2, span: null }],
        term: printTerm(2, 5)
      },
      {
        id: 5,
        params: [],
        ops: [],
        term: { op: "finish_defer", defer: 1, park: 0, next: { to: 6, args: [] }, span: null }
      },
      {
        id: 6,
        params: [],
        ops: [],
        term: { op: "cleanup.dispatch", normal: { to: 7, args: [] }, fault: { to: 8, args: [] }, span: null }
      },
      { id: 7, params: [], ops: [], term: { op: "return", value: null, span: null } },
      { id: 8, params: [], ops: [], term: { op: "resume_unwind", span: null } }
    ]
  );
}

test("a deferred body reached while unwinding runs on its own fault state", () => {
  const trace = run(parkPackage(true));

  // The body's own dispatch took its normal edge, so the panic was parked at
  // the boundary rather than leaking into the cleanup being run.
  assert.equal(stdout(trace), "body saw no fault\n");
  // `finish_defer` gave the panic back, so the enclosing dispatch unwound.
  assert.equal(trace.result, "panic");
  assert.equal(trace.final_state.exit_code, null);
  assert.equal(trace.final_state.runtime_failures[0].trap_kind, "panic");
  assert.equal(trace.final_state.runtime_failures[0].message, "boom");
});

test("the same deferred body on the ordinary path leaves the program succeeding", () => {
  // Negative control: the body prints the same line either way, so the panic
  // above is the parked fault coming back at `finish_defer` and not something
  // the body itself produced.
  const trace = run(parkPackage(false));

  assert.equal(trace.result, "ok", JSON.stringify(trace.final_state.runtime_failures));
  assert.equal(stdout(trace), "body saw no fault\n");
  assert.equal(trace.final_state.exit_code, 0);
});
