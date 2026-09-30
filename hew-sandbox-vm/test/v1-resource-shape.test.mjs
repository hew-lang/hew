import assert from "node:assert/strict";
import test from "node:test";
import { runBytecode } from "../dist/interpreter/index.js";
import { admitPackage } from "../dist/interpreter/v1/validate.js";

function packageWithSameDisplayName() {
  return {
    schema_version: "hew.sandbox.bytecode.v1",
    hew_version: "0.6.0-rc5",
    compiler_version: "resource-shape-test",
    profile: "sandbox-vm-export",
    entry: { function: 0, exit: "unit" },
    strings: ["closed"],
    bytes: [],
    regex_patterns: [],
    aggregates: [
      { id: 0, name: "Shape", fields: [] },
      { id: 1, name: "Shape", fields: [] },
    ],
    variants: [],
    resources: [{ kind: "record", ty: "Shape", shape: 1, close: 1 }],
    runtime_families: [
      { id: 0, family: "Print", detail: { kind: "Str", newline: true } },
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
            ops: [
              { op: "aggregate.make", dst: 0, shape: 0, fields: [], span: null },
              { op: "aggregate.make", dst: 1, shape: 1, fields: [], span: null },
              { op: "destroy_value", value: 0, span: null },
              { op: "destroy_value", value: 1, span: null },
            ],
            term: { op: "return", value: null, span: null },
          },
        ],
      },
      {
        id: 1,
        name: "close",
        params: [{ value: 0, own: "owned" }],
        entry: 0,
        places: [],
        blocks: [
          {
            id: 0,
            params: [],
            ops: [{ op: "const.str", dst: 1, str: 0, span: null }],
            term: {
              op: "runtime.call",
              family: 0,
              args: [{ value: 1, decision: "borrow" }],
              result: null,
              result_shape: null,
              normal: { to: 1, args: [] },
              unwind: null,
              span: null,
            },
          },
          {
            id: 1,
            params: [],
            ops: [],
            term: { op: "return", value: null, span: null },
          },
        ],
      },
    ],
  };
}

test("record close follows exact shape when display names collide", () => {
  const bytecode = packageWithSameDisplayName();
  assert.equal(admitPackage(bytecode), null);
  const trace = runBytecode(bytecode, {
    fixtureId: "resource-shape",
    traceId: "trace:resource-shape",
    replay: {
      seed: 0,
      step_budget: 1000,
      virtual_clock: { epoch_ms: 0, tick_ms: 1, current_ms: 0 },
      inputs: [],
    },
  });
  assert.equal(trace.result, "ok");
  assert.equal(trace.final_state.stdout.join(""), "closed\n");
});

test("record close without an exact shape is refused at load", () => {
  const bytecode = packageWithSameDisplayName();
  delete bytecode.resources[0].shape;
  assert.equal(
    admitPackage(bytecode)?.code,
    "sandbox.package.resource_shape_missing",
  );
});
