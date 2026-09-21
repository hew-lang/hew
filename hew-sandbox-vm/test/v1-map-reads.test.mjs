// Map reads: `Map(ContainsKey)` answers with a bool, and `Map(Get)` builds its
// `Option` from the descriptor the call's `result_shape` names, so the tag is
// the case's position in that descriptor and never an order the shim assumes.
//
// The package under test is the shape of:
//
//     var scores: HashMap<string, i64> = HashMap.new();
//     scores.insert("alice", 95);
//     println(scores.contains_key("alice"));
//     println(scores.contains_key("dave"));
//     match scores.get("alice") { .Some(n) => println(n), .None => println("missing") }
//     match scores.get("dave")  { .Some(n) => println(n), .None => println("missing") }

import assert from "node:assert/strict";
import test from "node:test";
import { runBytecode } from "../dist/interpreter/index.js";

const FAMILIES = [
  { id: 0, family: "Map", detail: "New" },
  { id: 1, family: "Map", detail: "Insert" },
  { id: 2, family: "Map", detail: "ContainsKey" },
  { id: 3, family: "Map", detail: "Get" },
  { id: 4, family: "Print", detail: { kind: "Bool", newline: true } },
  { id: 5, family: "Print", detail: { kind: "I64", newline: true } },
  { id: 6, family: "Print", detail: { kind: "Str", newline: true } }
];

/// `cases` is the demanded `Option` descriptor. The compiler emits it in the
/// declaration order of the instantiation, which is why a test can reorder it.
function mapPackage(cases = [{ name: "Some", fields: ["0"] }, { name: "None", fields: [] }]) {
  const someTag = cases.findIndex((entry) => entry.name === "Some");
  const noneTag = cases.findIndex((entry) => entry.name === "None");

  return {
    schema_version: "hew.sandbox.bytecode.v1",
    hew_version: "0.6.0-rc4",
    compiler_version: "v1-map-test",
    profile: "sandbox-vm-export",
    entry: { function: 0, exit: "unit" },
    strings: ["alice", "dave", "missing"],
    bytes: [],
    regex_patterns: [],
    aggregates: [],
    variants: [{ id: 0, name: "Option<i64>", cases }],
    runtime_families: FAMILIES,
    externs: [],
    suspend_kinds: [],
    value_capabilities: [{ id: 0, capability: "Hash", ty: "string", components: [] }, { id: 1, capability: "Eq", ty: "string", components: [] }],
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
              { op: "const.str", dst: 0, str: 0, span: null },
              { op: "const.str", dst: 1, str: 1, span: null },
              { op: "const.int", dst: 2, value: "95", ty: "i64", span: null }
            ],
            term: call(0, [], { value: 3, own: "owned" }, 1)
          },
          // The receiver arrives by move and leaves as the result, so the
          // insert's map is the one every later read sees.
          {
            id: 1,
            params: [],
            ops: [],
            term: call(
              1,
              [
                { value: 3, decision: "move" },
                { value: 0, decision: "borrow" },
                { value: 2, decision: "borrow" }
              ],
              { value: 4, own: "owned" },
              2
            )
          },
          {
            id: 2,
            params: [],
            ops: [],
            term: call(
              2,
              [
                { value: 4, decision: "borrow" },
                { value: 0, decision: "borrow" }
              ],
              { value: 5, own: "none" },
              3
            )
          },
          { id: 3, params: [], ops: [], term: print(4, 5, 4) },
          {
            id: 4,
            params: [],
            ops: [],
            term: call(
              2,
              [
                { value: 4, decision: "borrow" },
                { value: 1, decision: "borrow" }
              ],
              { value: 6, own: "none" },
              5
            )
          },
          { id: 5, params: [], ops: [], term: print(4, 6, 6) },
          {
            id: 6,
            params: [],
            ops: [],
            term: call(
              3,
              [
                { value: 4, decision: "borrow" },
                { value: 0, decision: "borrow" }
              ],
              { value: 7, own: "none" },
              7,
              0
            )
          },
          { id: 7, params: [], ops: [], term: match(7, someTag, noneTag, 8, 10, 9) },
          { id: 8, params: [], ops: [], term: print(5, 9, 11) },
          // Unreached: the arms above cover both tags.
          { id: 9, params: [], ops: [], term: { op: "return", value: null, span: null } },
          { id: 10, params: [], ops: [{ op: "const.str", dst: 12, str: 2, span: null }], term: print(6, 12, 11) },
          {
            id: 11,
            params: [],
            ops: [],
            term: call(
              3,
              [
                { value: 4, decision: "borrow" },
                { value: 1, decision: "borrow" }
              ],
              { value: 13, own: "none" },
              12,
              0
            )
          },
          { id: 12, params: [], ops: [], term: match(13, someTag, noneTag, 13, 14, 15) },
          { id: 13, params: [], ops: [], term: print(5, 15, 16) },
          { id: 14, params: [], ops: [{ op: "const.str", dst: 17, str: 2, span: null }], term: print(6, 17, 16) },
          { id: 15, params: [], ops: [], term: { op: "return", value: null, span: null } },
          { id: 16, params: [], ops: [], term: { op: "return", value: null, span: null } }
        ]
      }
    ]
  };
}

function call(family, args, result, to, resultShape = null) {
  return {
    op: "runtime.call",
    family,
    callbacks: [1, 2, 3].includes(family) ? [0, 1] : [],
    args,
    result,
    result_shape: resultShape,
    normal: { to, args: [] },
    unwind: null,
    span: null
  };
}

function print(family, value, to) {
  return call(family, [{ value, decision: "borrow" }], null, to);
}

/// A `switch.variant` over the demanded descriptor. The `Some` arm binds the
/// payload as `payload`; the `None` arm binds nothing.
function match(scrutinee, someTag, noneTag, someTo, noneTo, payload) {
  return {
    op: "switch.variant",
    shape: 0,
    scrutinee,
    arms: [
      { variant: someTag, fields: [{ value: payload, own: "none" }], edge: { to: someTo, args: [] } },
      { variant: noneTag, fields: [], edge: { to: noneTo, args: [] } }
    ],
    span: null
  };
}

function run(bytecode) {
  return runBytecode(bytecode, {
    fixtureId: "v1-map",
    traceId: "trace:v1-map",
    replay: { seed: 1, step_budget: 10_000, virtual_clock: { epoch_ms: 0, tick_ms: 1, current_ms: 0 }, inputs: [] }
  });
}

function stdout(trace) {
  return trace.final_state.stdout.join("");
}

test("a map answers contains_key and get for a present key and a missing one", () => {
  const trace = run(mapPackage());

  assert.equal(trace.result, "ok", JSON.stringify(trace.final_state.runtime_failures));
  assert.equal(stdout(trace), "true\nfalse\n95\nmissing\n");
  assert.equal(trace.final_state.exit_code, 0);
});

test("Map(Get) takes its tag from the descriptor the call names", () => {
  // The same program with `None` declared first. A shim assuming a tag order of
  // its own would send the hit down the miss arm; reading the descriptor keeps
  // both answers where they belong.
  const trace = run(
    mapPackage([{ name: "None", fields: [] }, { name: "Some", fields: ["0"] }])
  );

  assert.equal(trace.result, "ok", JSON.stringify(trace.final_state.runtime_failures));
  assert.equal(stdout(trace), "true\nfalse\n95\nmissing\n");
});
