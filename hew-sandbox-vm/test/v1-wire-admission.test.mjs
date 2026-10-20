import assert from "node:assert/strict";
import fs from "node:fs";
import test from "node:test";
import { runBytecode } from "../dist/interpreter/interpreter.js";

const external = JSON.parse(fs.readFileSync(new URL("../fixtures/01-hello-world/bytecode.json", import.meta.url), "utf8"));
test("malformed instruction operands are rejected before any output", () => {
  for (const value of [-1, 4294967296, "0", null]) {
    const package_ = structuredClone(external);
    const operation = package_.functions.flatMap((fn) => fn.blocks).flatMap((block) => block.ops).find((op) => op.op === "const.str");
    assert.ok(operation, "external package contains a string instruction");
    operation.str = value;
    const trace = runBytecode(package_);
    assert.equal(trace.result, "sandbox_rejected", JSON.stringify(value));
    assert.equal(trace.final_state.sandbox_rejections[0].category, "invalid_package");
    assert.deepEqual(trace.final_state.stdout, []);
  }
});
test("malformed call boundaries are rejected before execution", () => {
  const package_ = structuredClone(external);
  const term = package_.functions.flatMap((fn) => fn.blocks).map((block) => block.term).find((term) => term.op === "runtime.call");
  assert.ok(term, "external package contains a runtime call");
  term.args[0].decision = "invent_ownership";
  const trace = runBytecode(package_);
  assert.equal(trace.result, "sandbox_rejected");
  assert.equal(trace.final_state.sandbox_rejections[0].category, "invalid_package");
  assert.deepEqual(trace.final_state.stdout, []);
});
