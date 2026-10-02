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
const wasmDir = fs.mkdtempSync(path.join(os.tmpdir(), "hew-wasm-"));

process.env.HEWPATH = repoRoot;
buildSandboxWasmBridge();
const wasmModule = await import(pathToFileURL(path.join(wasmDir, "hew_wasm.js")).href);
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
    receive fn bounds() -> string { f"{max}|{min}" }
}

supervisor BoundsTree {
    strategy: one_for_one;
    intensity: 1 within 60s;
    child bounds: Bounds(max: 9223372036854775807, min: -9223372036854775808);
}

fn main() {
    let tree = spawn BoundsTree;
    println(match tree.bounds.bounds() { .Ok(value) => value, .Err(_) => "error" });
}
`,
    "sandbox-vm-export"
  );
  const compiled = typeof compileOutput === "string" ? JSON.parse(compileOutput) : compileOutput;

  assert.ok(compiled.diagnostics.every((diagnostic) => diagnostic.severity !== "error"), JSON.stringify(compiled.diagnostics));
  assert.ok(compiled.bytecode, "compiler should emit bytecode");
  const bytecode = JSON.parse(JSON.stringify(compiled.bytecode));
  assert.equal(bytecode.schema_version, "hew.sandbox.bytecode.v1");
  const trace = runBytecode(bytecode);
  assert.equal(trace.result, "ok");
  assert.equal(trace.final_state.stdout.join(""), "9223372036854775807|-9223372036854775808\n");
});

test("runProgram reads two stdin lines from the page input buffer byte-cleanly", () => {
  const result = runProgram(
    `
import std.io;

fn main() {
    let first = io.read_line().unwrap_or("<none>");
    let second = io.read_line().unwrap_or("<none>");
    println(f"{first}|{second}");
}
`,
    "héw\r\nbytes\n"
  );

  assert.equal(result.stdout, "héw|bytes\n");
  assert.equal(result.exit_code, 0);
  assert.deepEqual(result.diagnostics, []);
});

test("runProgram hands a read_line loop successive lines, an empty line, then None at end of input", () => {
  const result = runProgram(
    `
import std.io;

fn main() {
    for i in 0..6 {
        match io.read_line() {
            .Some(line) => println(f"[{line}]"),
            .None => {
                println("end");
                return;
            }
        }
    }
}
`,
    "one\n\ntwo\r\nthree"
  );

  assert.equal(result.stdout, "[one]\n[]\n[two]\n[three]\nend\n");
  assert.equal(result.exit_code, 0);
  assert.deepEqual(result.diagnostics, []);
});

test("runProgram read_all keeps line endings and the unterminated tail after a read_line", () => {
  const result = runProgram(
    `
import std.io;

fn main() {
    let first = io.read_line().unwrap_or("<none>");
    let rest = io.read_all();
    println(f"{first}|[{rest}]");
    let after = io.read_line().unwrap_or("<none>");
    println(after);
}
`,
    "a\r\n\nb\r\nc"
  );

  assert.equal(result.stdout, "a|[\nb\r\nc]\n<none>\n");
  assert.equal(result.exit_code, 0);
  assert.deepEqual(result.diagnostics, []);
});

test("runProgram records each consumed stdin line as a replay.input event without duplicating the record", () => {
  const compiled = globalThis.__hewSandboxCompileToSandboxBytecode(
    `
import std.io;

fn main() {
    let a = io.read_line().unwrap_or("<none>");
    let b = io.read_line().unwrap_or("<none>");
    println(a);
    println(b);
}
`,
    "sandbox-vm-export"
  );
  const bytecode = typeof compiled === "string" ? JSON.parse(compiled).bytecode : compiled.bytecode;
  const trace = runBytecode(bytecode, { replay: { inputs: [{ kind: "stdin", data: "x\ny\n" }] } });
  // A read suspends, so the scheduler records its steps alongside; stdin is
  // persisted once, as given, and each read records the line it consumed.
  const stdin = (input) => input.kind === "stdin";
  assert.deepEqual(trace.replay.inputs.filter(stdin), [{ kind: "stdin", data: "x\ny\n" }]);
  assert.deepEqual(
    trace.events
      .filter((event) => event.type === "replay.input")
      .map((event) => event.replay_input)
      .filter(stdin),
    [{ kind: "stdin", data: "x\n" }, { kind: "stdin", data: "y\n" }]
  );
  assert.deepEqual(trace.final_state.stdout, ["x\n", "y\n"]);
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
  const mode = process.env.HEW_WASM_PACK_MODE;
  const args = ["build", path.join(repoRoot, "hew-wasm"), "--target", "nodejs", "--dev", "--out-dir", wasmDir];
  if (mode) args.push("--mode", mode);
  const result = spawnSync(
    "wasm-pack",
    args,
    {
      cwd: repoRoot,
      encoding: "utf8"
    }
  );
  assert.equal(result.status, 0, result.stderr || result.stdout);
}
