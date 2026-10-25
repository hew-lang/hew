// `runProgram` is the page's entry point. These cases stub the compiler bridge
// with a v1 package so the page path is exercised without a WASM build, and
// assert what a person running code on the page sees.

import assert from "node:assert/strict";
import test from "node:test";
import { runProgram } from "../dist/interpreter/run-program.js";

function v1Package(overrides = {}) {
  return {
    schema_version: "hew.sandbox.bytecode.v1",
    hew_version: "0.9.4",
    compiler_version: "v1-run-program-test",
    profile: "sandbox-vm-export",
    entry: { function: 0, exit: "unit" },
    strings: ["hello from the page"],
    bytes: [],
    regex_patterns: [],
    aggregates: [],
    variants: [],
    runtime_families: [{ id: 0, family: "Print", detail: { kind: "Str", newline: true } }],
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
            ops: [{ op: "const.str", dst: 0, str: 0, span: null }],
            term: {
              op: "runtime.call",
              family: 0,
              args: [{ value: 0, decision: "borrow" }],
              result: null,
              result_shape: null,
              normal: { to: 1, args: [] },
              unwind: null,
              span: null
            }
          },
          { id: 1, params: [], ops: [], term: { op: "return", value: null, span: null } }
        ]
      }
    ],
    ...overrides
  };
}

async function withCompiler(bytecode, body) {
  const previous = globalThis.__hewSandboxCompileToSandboxBytecode;
  globalThis.__hewSandboxCompileToSandboxBytecode = () => ({ diagnostics: [], bytecode });
  try {
    return await body();
  } finally {
    globalThis.__hewSandboxCompileToSandboxBytecode = previous;
  }
}

test("runProgram runs a v1 package without the v0 exit patching", async () => {
  const result = await withCompiler(v1Package(), async () => await runProgram("fn main() {}", ""));

  assert.equal(result.stdout, "hello from the page\n");
  assert.equal(result.exit_code, 0);
  assert.equal(result.status, "ok");
  assert.equal(result.compiler_version, "v1-run-program-test");
  assert.equal(result.hew_version, "0.9.4");
  assert.deepEqual(result.diagnostics, []);
});

test("runProgram publishes the entry's status as the page exit code", async () => {
  const statusPackage = v1Package({
    entry: { function: 0, exit: "status" },
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
            ops: [{ op: "const.int", dst: 0, value: "2", ty: "i64", span: null }],
            term: { op: "return", value: { value: 0, decision: "move" }, span: null }
          }
        ]
      }
    ]
  });

  const result = await withCompiler(statusPackage, async () => await runProgram("fn main() -> i64 { 2 }", ""));
  assert.equal(result.exit_code, 2);
});

test("runProgram reports a load-time refusal rather than an empty run", async () => {
  const rejected = v1Package({
    runtime_families: [
      { id: 0, family: "Print", detail: { kind: "Str", newline: true } },
      { id: 1, family: "FileRead", detail: "Open" }
    ]
  });

  const result = await withCompiler(rejected, async () => await runProgram("fn main() {}", ""));

  assert.equal(result.stdout, "");
  assert.equal(result.status, "sandbox_rejected");
  assert.equal(result.sandbox_rejections[0].category, "native_only");
  assert.equal(result.sandbox_rejections[0].capability, "FileRead::Open");
  assert.ok(
    result.diagnostics.some(
      (diagnostic) => diagnostic.severity === "error" && diagnostic.capability === "FileRead::Open" && diagnostic.message.includes("requires native execution")
    ),
    `a refused capability must reach the page: ${JSON.stringify(result.diagnostics)}`
  );
});

test("unknown instructions and terminators reject the package before output", async () => {
  for (const corrupt of [
    (bytecode) => bytecode.functions[0].blocks[1].ops.push({ op: "unrecognized.opcode" }),
    (bytecode) => { bytecode.functions[0].blocks[1].term.op = "unrecognized.terminator"; },
  ]) {
    const bytecode = v1Package();
    corrupt(bytecode);
    const result = await withCompiler(bytecode, async () => await runProgram("", ""));
    assert.equal(result.status, "sandbox_rejected");
    assert.equal(result.stdout, "");
    assert.equal(result.sandbox_rejections[0].category, "invalid_package");
  }
});
