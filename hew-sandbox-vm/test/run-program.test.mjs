import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import test from "node:test";
import { pathToFileURL, fileURLToPath } from "node:url";
import { runBytecode } from "../dist/interpreter/index.js";
import { runProgram } from "../dist/interpreter/run-program.js";

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");
const repoRoot = path.resolve(root, "..");
const wasmDir = fs.mkdtempSync(path.join(os.tmpdir(), "hew-sandbox-wasm-"));

process.env.HEWPATH = repoRoot;
buildSandboxWasmBridge();
const wasmModule = await import(pathToFileURL(path.join(wasmDir, "hew_sandbox_wasm.js")).href);
globalThis.__hewSandboxCompileToSandboxBytecode =
  wasmModule.compileToSandboxBytecode ?? wasmModule.default?.compileToSandboxBytecode;
assert.equal(typeof globalThis.__hewSandboxCompileToSandboxBytecode, "function");

test.after(() => {
  fs.rmSync(wasmDir, { recursive: true, force: true });
  delete globalThis.__hewSandboxCompileToSandboxBytecode;
});

test("runProgram hello_world returns stdout and zero exit code", () => {
  const source = fs.readFileSync(path.join(root, "fixtures/01-hello-world/main.hew"), "utf8");
  const result = runProgram(source, "");

  assert.equal(result.stdout, "Hello, sandbox!\n");
  assert.equal(result.exit_code, 0);
  assert.deepEqual(result.diagnostics, []);
});

test("serialized bytecode preserves supervisor child i64 bounds", () => {
  const compileOutput = globalThis.__hewSandboxCompileToSandboxBytecode(
    `
actor Bounds {
    let max: i64;
    let min: i64;
}

supervisor BoundsTree {
    strategy: one_for_one;
    intensity: 1 within 60s;
    child bounds: Bounds(max: 9223372036854775807, min: -9223372036854775808);
}

fn main() {
    let tree = spawn BoundsTree;
}
`,
    "sandbox-vm-export"
  );
  const compiled = typeof compileOutput === "string" ? JSON.parse(compileOutput) : compileOutput;

  assert.ok(compiled.diagnostics.every((diagnostic) => diagnostic.severity !== "error"), JSON.stringify(compiled.diagnostics));
  assert.ok(compiled.bytecode, "compiler should emit bytecode");
  const bytecode = JSON.parse(JSON.stringify(compiled.bytecode));
  assert.deepEqual(bytecode.layouts.supervisors[0].children[0].start_spec.args, [
    { kind: "i64", value: "9223372036854775807" },
    { kind: "i64", value: "-9223372036854775808" }
  ]);

  const trace = runBytecode(bytecode);
  const childSpawn = trace.events.find((event) => event.message === "actor.spawn");
  assert.ok(childSpawn?.text, "supervisor child should spawn");
  assert.deepEqual(JSON.parse(childSpawn.text).state, ["9223372036854775807", "-9223372036854775808"]);
});

test("runProgram reads two stdin lines from the page input buffer byte-cleanly", () => {
  const result = runProgram(
    `
import std::io;

fn main() {
    let first = io.read_line();
    let second = io.read_line();
    println(f"{first}|{second}");
}
`,
    "héw\nbytes\n"
  );

  assert.equal(result.stdout, "héw|bytes\n");
  assert.equal(result.exit_code, 0);
  assert.deepEqual(result.diagnostics, []);
});

test("runProgram parse errors return diagnostics and do not execute cached bytecode", () => {
  const result = runProgram("fn main( {\n    println(\"nope\");\n}\n", "");

  assert.notEqual(result.exit_code, 0);
  assert.equal(result.stdout, "");
  assert.ok(result.diagnostics.length > 0);
  assert.ok(result.diagnostics.some((diagnostic) => diagnostic.phase === "parse"));
});

test("runProgram type errors return diagnostics and do not execute", () => {
  const result = runProgram("fn main() {\n    let x: i64 = \"oops\";\n    println(x);\n}\n", "");

  assert.notEqual(result.exit_code, 0);
  assert.equal(result.stdout, "");
  assert.ok(result.diagnostics.length > 0);
  assert.ok(result.diagnostics.some((diagnostic) => diagnostic.phase === "typecheck"));
});

test("runProgram panics map to non-zero exit code and a trap diagnostic", () => {
  const result = runProgram("fn main() {\n    panic(\"sandbox panic\");\n}\n", "");

  assert.notEqual(result.exit_code, 0);
  assert.ok(result.diagnostics.some((diagnostic) => diagnostic.phase === "run" && diagnostic.trap_kind === "panic"));
});

function buildSandboxWasmBridge() {
  const result = spawnSync(
    "wasm-pack",
    ["build", path.join(repoRoot, "hew-sandbox-wasm"), "--target", "nodejs", "--dev", "--out-dir", wasmDir],
    {
      cwd: repoRoot,
      encoding: "utf8"
    }
  );
  assert.equal(result.status, 0, result.stderr || result.stdout);
}
