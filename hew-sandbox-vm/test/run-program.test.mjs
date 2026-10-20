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
globalThis.__hewSandboxExecuteCodec = wasmModule.executeCodec ?? wasmModule.default?.executeCodec;
globalThis.__hewSandboxExecuteCalendar = wasmModule.executeCalendar ?? wasmModule.default?.executeCalendar;

test.after(() => {
  fs.rmSync(wasmDir, { recursive: true, force: true });
  delete globalThis.__hewSandboxCompileToSandboxBytecode;
  delete globalThis.__hewSandboxExecuteCodec;
  delete globalThis.__hewSandboxExecuteCalendar;
});

test("runProgram hello_world returns stdout and zero exit code", () => {
  const source = fs.readFileSync(path.join(root, "fixtures/01-hello-world/main.hew"), "utf8");
  const result = runProgram(source, "");

  assert.equal(result.stdout, "Hello, sandbox!\n");
  assert.equal(result.exit_code, 0);
  assert.deepEqual(result.diagnostics, []);
});

test("runProgram preserves timestamp codec values and located errors", () => {
  const source = fs.readFileSync(
    path.join(repoRoot, "tests/core-acceptance/cases/timestamp-codecs.hew"),
    "utf8"
  );
  const result = runProgram(source, "");
  assert.equal(result.exit_code, 0, JSON.stringify(result.diagnostics));
  assert.deepEqual(result.diagnostics, []);
  assert.equal(result.stdout, "timestamp codecs ok\n");
});

test("timestamp.now uses the replay clock's Unix coordinate exactly once", () => {
  const output = globalThis.__hewSandboxCompileToSandboxBytecode(`
fn main() {
    let (seconds, nanos) = timestamp.now().unix_parts();
    println(f"{seconds}|{nanos}");
}
`, "sandbox-vm-export");
  const compiled = typeof output === "string" ? JSON.parse(output) : output;
  assert.ok(compiled.bytecode, JSON.stringify(compiled.diagnostics));
  for (const [epoch_ms, current_ms, expected] of [
    [0, 0, "0|0\n"],
    [1700000000000, 1700000000123, "1700000000|123000000\n"],
    [-1000, -1, "-1|999000000\n"],
    [-1000, -1001, "-2|999000000\n"],
  ]) {
    const trace = runBytecode(compiled.bytecode, {
      replay: { virtual_clock: { epoch_ms, current_ms, tick_ms: 0 } },
    });
    assert.equal(trace.result, "ok", JSON.stringify(trace.final_state));
    assert.equal(trace.final_state.stdout.join(""), expected);
  }
  for (const current_ms of [NaN, Infinity, -Infinity]) {
    const trace = runBytecode(compiled.bytecode, {
      replay: { virtual_clock: { epoch_ms: 0, current_ms, tick_ms: 0 } },
    });
    assert.notEqual(trace.result, "ok");
  }
  const malformed = structuredClone(compiled.bytecode);
  const clockCall = malformed.functions.flatMap(fn => fn.blocks).map(block => block.term)
    .find(term => term.op === "runtime.call" && malformed.runtime_families[term.family].family === "TimestampNow");
  delete clockCall.aggregate_result_shape;
  assert.equal(runBytecode(malformed).result, "sandbox_rejected");
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
    child bounds: Bounds { max: 9223372036854775807, min: -9223372036854775808 };
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
    let first = io.read_line() ?? "<none>";
    let second = io.read_line() ?? "<none>";
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
    let first = io.read_line() ?? "<none>";
    let rest = io.read_all();
    println(f"{first}|[{rest}]");
    let after = io.read_line() ?? "<none>";
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
    let a = io.read_line() ?? "<none>";
    let b = io.read_line() ?? "<none>";
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

test("compute-only loops and recursion yield, cancel and close their owners", () => {
  const source = fs.readFileSync(path.join(repoRoot, "tests/core-acceptance/cases/compute-safepoints.hew"), "utf8");
  const result = runProgram(source, "");
  assert.equal(result.exit_code, 0, JSON.stringify(result.diagnostics));
  assert.equal(result.stdout, "peer progressed\ndeferred loop 10000\nclosed loop\n7\npeer progressed\ndeferred recursion 10000\nclosed recursion\n7\ndeferred actor 10000\nclosed actor\nactor stopped\nclosed deadline\ndeadline recovered\nrollback:ORIGINAL\nmachine deadline recovered\n");
});

test("authored close helpers yield without abandoning explicit or implicit release", () => {
  const source = fs.readFileSync(path.join(repoRoot, "tests/core-acceptance/cases/compute-close-safepoints.hew"), "utf8");
  const result = runProgram(source, "");
  assert.equal(result.exit_code, 0, JSON.stringify(result.diagnostics));
  assert.equal(result.stdout, "peer progressed\nfinished 40000\nclosed explicit\n7\npeer progressed\nfinished 40000\nclosed implicit\n7\n");
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

for (const [name, body, expected] of [
  [
    "repeated assignment",
    "fresh = Ticket { id: 2 }; fresh = Ticket { id: 3 };",
    "closed 1\nclosed 2\nclosed 3\n",
  ],
  [
    "explicit close",
    "fresh = Ticket { id: 2 }; fresh.close();",
    "closed 1\nclosed 2\n",
  ],
  [
    "reinitialization",
    "fresh.close(); fresh = Ticket { id: 2 };",
    "closed 1\nclosed 2\n",
  ],
]) {
  test(`compiled resource lifecycle: ${name}`, () => {
    const result = runProgram(
      `
#[resource]
type Ticket { id: i64; }
impl Ticket { fn close(consume self) { println(f"closed {self.id}"); } }
fn main() { var fresh = Ticket { id: 1 }; ${body} }
`,
      "",
    );
    assert.equal(result.exit_code, 0, JSON.stringify(result.diagnostics));
    assert.deepEqual(result.diagnostics, []);
    assert.equal(result.stdout, expected);
  });
}


test("runProgram shares checked UTC calendar semantics with native", () => {
  const source = fs.readFileSync(path.join(repoRoot, "tests/core-acceptance/cases/timestamp-calendar.hew"), "utf8");
  const result = runProgram(source, "");
  assert.equal(result.exit_code, 0, JSON.stringify(result.diagnostics));
  assert.deepEqual(result.diagnostics, []);
  assert.equal(result.stdout, "timestamp calendar ok\n");
});
