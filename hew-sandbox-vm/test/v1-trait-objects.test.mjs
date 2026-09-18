// Trait objects: `dyn.make` wraps a value with the vtable the checker
// selected, and `dyn.call` resolves the method through that vtable's slot.
//
// The package under test is the shape of this program, which prints
// `hello Ada` natively:
//
//     trait Greeter { fn greet(self) -> string; }
//     type English { name: string }
//     impl Greeter for English { fn greet(self) -> string { f"hello {self.name}" } }
//     fn speak(g: dyn Greeter) -> string { g.greet() }
//     fn main() { let e = English { name: "Ada" }; println(speak(e)); }

import assert from "node:assert/strict";
import test from "node:test";
import { runBytecode } from "../dist/interpreter/index.js";

const PRINT_LN = { id: 0, family: "Print", detail: { kind: "Str", newline: true } };
const CONCAT = { id: 1, family: "StringConcat" };

/// `vtables` carries one entry per trait implementation. `slot` is the
/// checker's own index for the method, and `callee` the function that
/// implements it.
function traitPackage(options = {}) {
  const vtables = options.vtables ?? [
    { id: 0, slots: [{ slot: 3, method: "greet", callee: 1 }] }
  ];
  const callSlot = options.callSlot ?? 3;

  return {
    schema_version: "hew.sandbox.bytecode.v1",
    hew_version: "0.6.0-rc4",
    compiler_version: "trait-object-test",
    profile: "sandbox-vm-export",
    entry: { function: 0, exit: "unit" },
    strings: ["Ada", "hello "],
    bytes: [],
    regex_patterns: [],
    aggregates: [{ id: 0, name: "English", fields: ["name"] }],
    variants: [],
    runtime_families: [PRINT_LN, CONCAT],
    externs: [],
    suspend_kinds: [],
    value_capabilities: [],
    closures: [],
    vtables,
    functions: [
      // main: build the record, wrap it as a trait object, hand it to speak.
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
              { op: "aggregate.make", dst: 1, shape: 0, fields: [0], span: null },
              { op: "dyn.make", dst: 2, vtable: 0, value: 1, span: null }
            ],
            term: {
              op: "call",
              callee: 2,
              args: [{ value: 2, decision: "move" }],
              result: { value: 3, own: "owned" },
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
              family: 0,
              args: [{ value: 3, decision: "borrow" }],
              result: null,
              result_shape: null,
              normal: { to: 2, args: [] },
              unwind: null,
              span: null
            }
          },
          { id: 2, params: [], ops: [], term: { op: "return", value: null, span: null } }
        ]
      },
      // English::greet(self) -> string
      {
        id: 1,
        name: "English::greet",
        params: [{ value: 0, own: "owned" }],
        entry: 0,
        places: [],
        blocks: [
          {
            id: 0,
            params: [],
            ops: [
              { op: "const.str", dst: 1, str: 1, span: null },
              { op: "aggregate.project_copy", dst: 2, shape: 0, aggregate: 0, field: 0, span: null }
            ],
            term: {
              op: "runtime.call",
              family: 1,
              args: [
                { value: 1, decision: "borrow" },
                { value: 2, decision: "borrow" }
              ],
              result: { value: 3, own: "owned" },
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
            term: { op: "return", value: { value: 3, decision: "move" }, span: null }
          }
        ]
      },
      // speak(g: dyn Greeter) -> string
      {
        id: 2,
        name: "speak",
        params: [{ value: 0, own: "owned" }],
        entry: 0,
        places: [],
        blocks: [
          {
            id: 0,
            params: [],
            ops: [],
            term: {
              op: "dyn.call",
              receiver: { value: 0, decision: "borrow" },
              slot: callSlot,
              args: [],
              result: { value: 1, own: "owned" },
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

function run(bytecode) {
  return runBytecode(bytecode, {
    fixtureId: "trait-object",
    traceId: "trace:trait-object",
    replay: { seed: 1, step_budget: 1000, virtual_clock: { epoch_ms: 0, tick_ms: 1, current_ms: 0 }, inputs: [] }
  });
}

test("a dyn call reaches the implementation through its vtable slot", () => {
  const trace = run(traitPackage());

  assert.equal(trace.result, "ok", JSON.stringify(trace.final_state.runtime_failures));
  assert.equal(trace.final_state.stdout.join(""), "hello Ada\n");
  assert.equal(trace.final_state.exit_code, 0);
});

test("the slot number is matched, not used as a position in the slot array", () => {
  // Two slots in an order that does not match their numbers: resolving by
  // position would call the wrong method.
  const trace = run(
    traitPackage({
      vtables: [
        {
          id: 0,
          slots: [
            { slot: 7, method: "other", callee: 0 },
            { slot: 3, method: "greet", callee: 1 }
          ]
        }
      ]
    })
  );

  assert.equal(trace.result, "ok", JSON.stringify(trace.final_state.runtime_failures));
  assert.equal(trace.final_state.stdout.join(""), "hello Ada\n");
});

test("a dyn call names a slot the vtable does not carry", () => {
  const trace = run(traitPackage({ callSlot: 9 }));

  assert.equal(trace.result, "runtime_failure");
  assert.match(trace.final_state.runtime_failures[0].message, /no slot 9/);
  assert.deepEqual(trace.final_state.stdout, []);
});

test("a trait object copies with the value it wraps", () => {
  // Take a copy of the trait object out of a place, then overwrite the place
  // with a different one. The copy must still carry the record it was made
  // from: if `load.copy` aliased the cell, `speak` would read the new name.
  const pkg = traitPackage();
  pkg.strings = ["Ada", "hello ", "Grace"];
  const main = pkg.functions[0];
  main.places = [{ id: 0, origin: "local" }];
  main.blocks[0].ops = [
    { op: "const.str", dst: 0, str: 0, span: null },
    { op: "aggregate.make", dst: 1, shape: 0, fields: [0], span: null },
    { op: "dyn.make", dst: 2, vtable: 0, value: 1, span: null },
    { op: "alloc_place", place: 0, span: null },
    { op: "store.init", place: 0, value: 2, span: null },
    { op: "load.copy", dst: 4, place: 0, span: null },
    { op: "const.str", dst: 5, str: 2, span: null },
    { op: "aggregate.make", dst: 6, shape: 0, fields: [5], span: null },
    { op: "dyn.make", dst: 7, vtable: 0, value: 6, span: null },
    { op: "store.assign", place: 0, value: 7, span: null }
  ];
  main.blocks[0].term = {
    op: "call",
    callee: 2,
    args: [{ value: 4, decision: "move" }],
    result: { value: 3, own: "owned" },
    normal: { to: 1, args: [] },
    unwind: null,
    span: null
  };

  const trace = run(pkg);
  assert.equal(trace.result, "ok", JSON.stringify(trace.final_state.runtime_failures));
  assert.equal(trace.final_state.stdout.join(""), "hello Ada\n");

  // Negative control: reading the place back after the assignment does see the
  // new value, so the case above is the copy being independent and not the
  // store failing to land.
  const aliased = structuredClone(pkg);
  aliased.functions[0].blocks[0].term.args = [{ value: 8, decision: "move" }];
  aliased.functions[0].blocks[0].ops.push({
    op: "load.take",
    dst: 8,
    place: 0,
    span: null
  });
  const after = run(aliased);
  assert.equal(after.result, "ok", JSON.stringify(after.final_state.runtime_failures));
  assert.equal(after.final_state.stdout.join(""), "hello Grace\n");
});
