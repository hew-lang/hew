import assert from "node:assert/strict";
import test from "node:test";
import { runBytecode } from "../dist/interpreter/index.js";

function literalPackage(literal, length) {
  return {
    schema_version: "hew.sandbox.bytecode.v1",
    hew_version: "0.11.0-rc2",
    compiler_version: "literal-append-test",
    profile: "sandbox-vm-export",
    entry: { function: 0, exit: "unit" },
    strings: ["left:", literal],
    bytes: [],
    regex_patterns: [],
    aggregates: [],
    variants: [],
    externs: [],
    suspend_kinds: [],
    value_capabilities: [],
    closures: [],
    vtables: [],
    runtime_families: [
      { id: 0, family: "StringAppendLiteral" },
      { id: 1, family: "Print", detail: { kind: "Str", newline: true } },
    ],
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
              {
                op: "const.int",
                dst: 2,
                value: String(length),
                ty: "u32",
                span: null,
              },
            ],
            term: {
              op: "runtime.call",
              family: 0,
              args: [
                { value: 0, decision: "move" },
                { value: 1, decision: "copy" },
                { value: 2, decision: "copy" },
              ],
              result: { value: 3, own: "owned" },
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
            term: {
              op: "runtime.call",
              family: 1,
              args: [{ value: 3, decision: "borrow" }],
              result: null,
              result_shape: null,
              normal: { to: 2, args: [] },
              unwind: null,
              span: null,
            },
          },
          {
            id: 2,
            params: [],
            ops: [],
            term: { op: "return", value: null, span: null },
          },
        ],
      },
    ],
  };
}

test("literal append honours the ABI byte length, including zero, UTF-8, BOM and NUL", async () => {
  for (const [literal, length, expected] of [
    ["é✓tail", 0, "left:"],
    ["é✓tail", 2, "left:é"],
    ["é✓tail", 5, "left:é✓"],
    ["é✓tail", 9, "left:é✓tail"],
    ["\ufeffé", 3, "left:\ufeff"],
    ["é\0tail", 3, "left:é\0"],
  ]) {
    const trace = await runBytecode(literalPackage(literal, length));
    assert.equal(trace.result, "ok", JSON.stringify(trace));
    assert.deepEqual(trace.final_state.stdout, [expected + "\n"]);
  }
});

test("literal append rejects an out-of-storage or split UTF-8 prefix before output", async () => {
  for (const length of [1, 20]) {
    const trace = await runBytecode(literalPackage("é✓tail", length));
    assert.equal(trace.result, "runtime_failure", JSON.stringify(trace));
    assert.deepEqual(trace.final_state.stdout, []);
  }
});
