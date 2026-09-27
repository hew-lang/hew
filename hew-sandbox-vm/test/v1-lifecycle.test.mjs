import assert from "node:assert/strict";
import test from "node:test";
import { runBytecode } from "../dist/interpreter/index.js";
import { admitPackage } from "../dist/interpreter/v1/validate.js";

function call(operation, args, result, to) {
  return {
    op: "actor.call", operation, args, result,
    result_shape: null, error_shape: null,
    normal: { to, args: [] }, unwind: null, span: null,
  };
}

function print(value, to) {
  return {
    op: "runtime.call", family: 0,
    args: [{ value, decision: "borrow" }], result: null,
    result_shape: null, normal: { to, args: [] }, unwind: null, span: null,
  };
}

function block(id, ops, term) {
  return { id, params: [], ops, term };
}

function outputBody(id, name, stringIndex) {
  return {
    id, name, params: [{ value: 0, own: "guaranteed" }], entry: 0, places: [],
    blocks: [
      block(0, [{ op: "const.str", dst: 1, str: stringIndex, span: null }], print(1, 1)),
      block(1, [], { op: "return", value: null, span: null }),
    ],
  };
}

function lifecyclePackage(request) {
  return {
    schema_version: "hew.sandbox.bytecode.v1",
    hew_version: "0.6.0-rc4",
    compiler_version: "lifecycle-test",
    profile: "sandbox-vm-export",
    entry: { function: 0, exit: "unit" },
    strings: ["after", "hook", "release"], bytes: [], regex_patterns: [],
    aggregates: [{ id: 0, name: "Resource", fields: [] }], variants: [],
    resources: [{ kind: "record", ty: "Resource", shape: 0, close: 2 }],
    runtime_families: [{ id: 0, family: "Print", detail: { kind: "Str", newline: true } }],
    externs: [], suspend_kinds: [], value_capabilities: [], closures: [], vtables: [],
    actors: [{ id: 0, state_fields: [{ mutable: false, deferred: false }],
      stop: [1], handlers: [], overflow: "block" }], supervisors: [],
    functions: [
      { id: 0, name: "main", params: [], entry: 0, places: [], blocks: [
        block(0, [{ op: "aggregate.make", dst: 0, shape: 0, fields: [], span: null }],
          call({ op: "spawn", actor: 0 }, [{ value: 0, decision: "move" }],
            { value: 1, own: "none" }, 1)),
        block(1, [], call({ op: request, actor: 0 }, [{ value: 1, decision: "borrow" }], null, 2)),
        block(2, [{ op: "const.str", dst: 2, str: 0, span: null }], print(2, 3)),
        block(3, [], call({ op: "await_stopped", actor: 0 },
          [{ value: 1, decision: "borrow" }], null, 4)),
        block(4, [], { op: "return", value: null, span: null }),
      ] },
      outputBody(1, "stop hook", 1),
      outputBody(2, "resource close", 2),
    ],
  };
}

function run(bytecode) {
  return runBytecode(bytecode, {
    fixtureId: "lifecycle", traceId: "trace:lifecycle",
    replay: { seed: 0, step_budget: 1000,
      virtual_clock: { epoch_ms: 0, tick_ms: 1, current_ms: 0 }, inputs: [] },
  });
}

test("stop returns before the hook and releases state after it", () => {
  const bytecode = lifecyclePackage("stop");
  assert.equal(admitPackage(bytecode), null);
  const trace = run(bytecode);
  assert.equal(trace.result, "ok", JSON.stringify(trace.final_state.runtime_failures));
  assert.equal(trace.final_state.stdout.join(""), "after\nhook\nrelease\n");
});

test("terminate skips the stop hook and still releases owned state", () => {
  const bytecode = lifecyclePackage("terminate");
  assert.equal(admitPackage(bytecode), null);
  const trace = run(bytecode);
  assert.equal(trace.result, "ok", JSON.stringify(trace.final_state.runtime_failures));
  assert.equal(trace.final_state.stdout.join(""), "after\nrelease\n");
});
