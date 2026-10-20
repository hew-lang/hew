import assert from "node:assert/strict";
import fs from "node:fs";
import test from "node:test";
import { runBytecode } from "../dist/interpreter/interpreter.js";

const external = JSON.parse(
  fs.readFileSync(
    new URL("../fixtures/01-hello-world/bytecode.json", import.meta.url),
    "utf8",
  ),
);
test("malformed instruction operands are rejected before any output", () => {
  for (const value of [-1, 4294967296, "0", null]) {
    const package_ = structuredClone(external);
    const operation = package_.functions
      .flatMap((fn) => fn.blocks)
      .flatMap((block) => block.ops)
      .find((op) => op.op === "const.str");
    assert.ok(operation, "external package contains a string instruction");
    operation.str = value;
    const trace = runBytecode(package_);
    assert.equal(trace.result, "sandbox_rejected", JSON.stringify(value));
    assert.equal(
      trace.final_state.sandbox_rejections[0].category,
      "invalid_package",
    );
    assert.deepEqual(trace.final_state.stdout, []);
  }
});
test("malformed call boundaries are rejected before execution", () => {
  const package_ = structuredClone(external);
  const term = package_.functions
    .flatMap((fn) => fn.blocks)
    .map((block) => block.term)
    .find((term) => term.op === "runtime.call");
  assert.ok(term, "external package contains a runtime call");
  term.args[0].decision = "invent_ownership";
  const trace = runBytecode(package_);
  assert.equal(trace.result, "sandbox_rejected");
  assert.equal(
    trace.final_state.sandbox_rejections[0].category,
    "invalid_package",
  );
  assert.deepEqual(trace.final_state.stdout, []);
});

function assertBeforeOutput(
  bytecode,
  category = "invalid_package",
  options = {},
) {
  const trace = runBytecode(bytecode, options);
  assert.equal(trace.result, "sandbox_rejected");
  assert.equal(trace.final_state.sandbox_rejections[0].category, category);
  assert.deepEqual(trace.final_state.stdout, []);
  assert.equal(trace.final_state.step_count, 0);
  return trace;
}

test("nested runtime identities reject unknown operations and malformed payloads before output", () => {
  assert.equal(
    runBytecode(external).result,
    "ok",
    "the independent package executes before mutation",
  );
  for (const identity of [
    { family: "Vector", detail: "InventOperation" },
    { family: "Array", detail: "Contains" },
    { family: "IntMethod", detail: ["RotateRight", "U128"] },
    { family: "IntArith", detail: ["WrappingDivide", "I64"] },
    { family: "MathIntrinsic", detail: null },
    { family: "Print", detail: { kind: "Str", newline: "true" } },
    { family: "TimestampNow", detail: "unexpected" },
  ]) {
    const bytecode = structuredClone(external);
    bytecode.runtime_families.push({
      id: bytecode.runtime_families.length,
      ...identity,
    });
    assertBeforeOutput(bytecode);
  }
});

test("known unavailable runtime operations retain their declared capability identities", () => {
  for (const [identity, capability] of [
    [{ family: "Vector", detail: "TakeAll" }, "Vector::TakeAll"],
    [{ family: "Semaphore", detail: "Acquire" }, "Semaphore::Acquire"],
    [{ family: "VecGet", detail: "I32" }, "VecGet::I32"],
  ]) {
    const bytecode = structuredClone(external);
    bytecode.runtime_families.push({
      id: bytecode.runtime_families.length,
      ...identity,
    });
    const trace = assertBeforeOutput(bytecode, "not_implemented");
    assert.equal(
      trace.final_state.sandbox_rejections[0].capability,
      capability,
    );
  }
});

test("nested suspension details and manifest identities are checked before output", () => {
  for (const replacement of [
    { kind: "Safepoint", detail: { budget: "4000" } },
    { kind: "NativeIo", detail: { operation: "InventOperation" } },
    { kind: "Join", detail: { scope: -1, mode: "wait" } },
    { kind: "StreamNext", detail: { park: null } },
  ]) {
    const bytecode = structuredClone(external);
    const term = bytecode.functions[0].blocks[0].term;
    assert.equal(term.op, "suspend");
    Object.assign(term, replacement);
    bytecode.suspend_kinds = [replacement.kind];
    assertBeforeOutput(bytecode);
  }
  const bytecode = structuredClone(external);
  bytecode.suspend_kinds.push("InventSuspension");
  assertBeforeOutput(bytecode);
});

function bridgePackage(term, ops = [], extra = {}) {
  const bytecode = structuredClone(external);
  Object.assign(bytecode, extra);
  bytecode.suspend_kinds = [];
  bytecode.strings = ["effect before bridge"];
  bytecode.runtime_families = [
    { id: 0, family: "Print", detail: { kind: "Str", newline: true } },
  ];
  bytecode.functions = [
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
          ops: [{ op: "const.str", dst: 0, str: 0 }],
          term: {
            op: "runtime.call",
            family: 0,
            args: [{ value: 0, decision: "borrow" }],
            result: null,
            result_shape: null,
            normal: { to: 1, args: [] },
            unwind: null,
          },
        },
        {
          id: 1,
          params: [],
          ops,
          term: {
            result: { value: 2, own: "owned" },
            normal: { to: 2, args: [] },
            unwind: null,
            ...term,
          },
        },
        { id: 2, params: [], ops: [], term: { op: "return", value: null } },
      ],
    },
  ];
  return bytecode;
}

test("missing codec bridge rejects before output; a supplied bridge executes the registered route", () => {
  const bytecode = bridgePackage(
    {
      op: "wire.codec",
      codec: 0,
      format: 1,
      direction: "encode",
      decoded: null,
      callbacks: {},
      args: [{ value: 1, decision: "borrow" }],
    },
    [{ op: "const.unit", dst: 1 }],
    {
      codecs: [
        JSON.stringify({
          root: 0,
          nodes: ["Unit"],
          names: [""],
          aggregates: [null],
          variants: [null],
          grouped_payloads: [false],
        }),
      ],
    },
  );
  assert.equal(
    assertBeforeOutput(bytecode, "not_implemented").final_state
      .sandbox_rejections[0].capability,
    "codecBridge",
  );
  let calls = 0;
  const trace = runBytecode(bytecode, {
    codecBridge(request) {
      calls += 1;
      assert.equal(JSON.parse(request).value, null);
      return JSON.stringify({ ok: true, value: [110, 117, 108, 108] });
    },
  });
  assert.equal(trace.result, "ok");
  assert.equal(calls, 1);
  assert.deepEqual(trace.final_state.stdout, ["effect before bridge\n"]);

  // This valid control really invokes the bridge. A malformed package must
  // refuse before either the earlier print or that same callback can run.
  const malformed = structuredClone(bytecode);
  malformed.actors = [null];
  calls = 0;
  assertBeforeOutput(malformed, "invalid_package", {
    codecBridge() {
      calls += 1;
      throw new Error("bridge must not run for an invalid package");
    },
  });
  assert.equal(calls, 0);
});

test("missing calendar bridge rejects before output and the supplied bridge receives the call", () => {
  const bytecode = bridgePackage(
    {
      op: "extern.call",
      extern: 0,
      args: [],
      result_shape: null,
      aggregate_result_shape: 0,
    },
    [],
    {
      externs: [{ id: 0, symbol: "hew_calendar_format" }],
      aggregates: [
        {
          id: 0,
          name: "CalendarFormat",
          fields: ["status", "text", "message"],
        },
      ],
    },
  );
  assert.equal(
    assertBeforeOutput(bytecode, "not_implemented").final_state
      .sandbox_rejections[0].capability,
    "calendarBridge",
  );
  let calls = 0;
  const trace = runBytecode(bytecode, {
    calendarBridge(request) {
      calls += 1;
      assert.equal(JSON.parse(request).operation, "format");
      return JSON.stringify({ status: 0, text: "calendar", message: "" });
    },
  });
  assert.equal(trace.result, "ok");
  assert.equal(calls, 1);
  assert.deepEqual(trace.final_state.stdout, ["effect before bridge\n"]);
});
