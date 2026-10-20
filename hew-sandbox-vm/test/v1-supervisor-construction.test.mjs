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

function block(id, ops, term) {
  return { id, params: [], ops, term };
}

function constructorPackage(withOwner) {
  return {
    schema_version: "hew.sandbox.bytecode.v1",
    hew_version: "0.6.2", compiler_version: "constructor-test",
    profile: "sandbox-vm-export", entry: { function: 0, exit: "unit" },
    strings: ["config"], bytes: [], regex_patterns: [], aggregates: [], variants: [],
    resources: [], runtime_families: [], externs: [], suspend_kinds: [],
    value_capabilities: [], closures: [], vtables: [],
    actors: [{ id: 0, name: "Worker", state_fields: [], stop: [], handlers: [], overflow: "block" }],
    supervisors: [{
      id: 0, strategy: "one_for_one", max_restarts: 3, window_secs: 5,
      ...(withOwner ? { startup_order: [0] } : {}),
      children: [{
        name: "worker", role: { actor: 0 }, restart: "permanent", spawn: 1,
        ...(withOwner ? { dependencies: [] } : {}),
      }],
    }],
    functions: [{
      id: 0, name: "main", params: [], entry: 0, places: [],
      blocks: [
        block(0, [{ op: "const.str", dst: 0, str: 0, span: null }],
          call({ op: "supervisor_spawn", supervisor: 0 },
            [{ value: 0, decision: "move" }], { value: 1, own: "none" }, 1)),
        block(1, [], { op: "return", value: null, span: null }),
      ],
    }, {
      id: 1, name: "child constructor",
      params: [
        ...(withOwner ? [{ value: 9, own: "none" }] : []),
        { value: 10, own: "guaranteed" },
      ],
      entry: 0, places: [],
      blocks: [
        block(0, [], call({ op: "spawn", actor: 0 }, [],
          { value: 20, own: "none" }, 1)),
        block(1, [], { op: "return", value: { value: 20, decision: "move" }, span: null }),
      ],
    }],
  };
}

for (const withOwner of [false, true]) {
  test(withOwner ? "owner and config constructor ABI executes" : "legacy config-only constructor ABI executes", () => {
    const bytecode = constructorPackage(withOwner);
    assert.equal(admitPackage(bytecode), null);
    const trace = runBytecode(bytecode);
    assert.equal(trace.result, "ok", JSON.stringify(trace.final_state.runtime_failures));
    assert.equal(trace.final_state.exit_code, 0);
    assert.ok(trace.events.some((event) => event.message === "supervisor.child"));
  });
}

test("constructor callable arity mismatch is rejected before execution", () => {
  const bytecode = constructorPackage(true);
  bytecode.functions[1].params.push({ value: 11, own: "guaranteed" });
  assert.equal(admitPackage(bytecode)?.code, "sandbox.package.supervisor_constructor");
});

for (const order of [[0, 0], [1], [], "invalid", null]) {
  test("malformed supervisor startup order is rejected: " + JSON.stringify(order), () => {
    const bytecode = constructorPackage(true);
    bytecode.supervisors[0].startup_order = order;
    assert.equal(admitPackage(bytecode)?.code, "sandbox.package.supervisor_startup_order");
  });
}

test("startup order cannot follow a required sibling or contain a cycle", () => {
  const bytecode = constructorPackage(true);
  const supervisor = bytecode.supervisors[0];
  supervisor.children.push({ ...supervisor.children[0], dependencies: [0] });
  supervisor.children[0].dependencies = [1];
  for (const order of [[0, 1], [1, 0]]) {
    supervisor.startup_order = order;
    assert.equal(admitPackage(bytecode)?.code, "sandbox.package.supervisor_startup_order");
  }
});


test("malformed and duplicate prerequisite lists are rejected", () => {
  for (const dependencies of [null, "invalid", [0, 0]]) {
    const bytecode = constructorPackage(true);
    bytecode.supervisors[0].children[0].dependencies = dependencies;
    assert.equal(admitPackage(bytecode)?.code, "sandbox.package.supervisor_startup_order");
  }
});
