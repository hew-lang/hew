import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import test from "node:test";
import { pathToFileURL, fileURLToPath } from "node:url";
import { runBytecode } from "../dist/interpreter/index.js";
import { runProgram } from "../dist/interpreter/run-program.js";
import { assertFixtureTrace } from "./support/fixture-trace.mjs";

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");
const repoRoot = path.resolve(root, "..");
// A supplied build is an immutable test input. This lets the VM qualify
// compatibility with an independently produced compiler without rebuilding it.
const suppliedModule = process.env.HEW_WASM_MODULE;
const reusedBindings = process.env.HEW_WASM_TEST_BINDINGS_DIR;
const ownsBindings = !suppliedModule && !reusedBindings;
const wasmDir = suppliedModule
  ? undefined
  : reusedBindings
    ? path.resolve(reusedBindings)
    : fs.mkdtempSync(path.join(os.tmpdir(), "hew-wasm-"));

process.env.HEWPATH = repoRoot;
buildSandboxWasmBridge();
const location =
  suppliedModule ?? pathToFileURL(path.join(wasmDir, "hew_wasm.js")).href;
const wasmModule = await import(location);
if (typeof wasmModule.default === "function") {
  const resolved = location.startsWith("file:")
    ? location
    : import.meta.resolve(location);
  await wasmModule.default({
    module_or_path: fs.readFileSync(
      new URL(resolved.replace(/\.js$/, "_bg.wasm")),
    ),
  });
}
globalThis.__hewSandboxCompileToSandboxBytecode =
  wasmModule.compileToSandboxBytecode ??
  wasmModule.default?.compileToSandboxBytecode;
assert.equal(
  typeof globalThis.__hewSandboxCompileToSandboxBytecode,
  "function",
);
globalThis.__hewSandboxExecuteCodec =
  wasmModule.executeCodec ?? wasmModule.default?.executeCodec;
globalThis.__hewSandboxExecuteCalendar =
  wasmModule.executeCalendar ?? wasmModule.default?.executeCalendar;

test.after(() => {
  try {
    const artifactDirectory = process.env.HEW_WASM_TEST_ARTIFACT_DIR;
    if (artifactDirectory && ownsBindings) {
      // Retain the tested bindings for downstream provider checks. Copy only
      // runtime artefacts; caller-owned destinations are never cleaned up.
      fs.mkdirSync(artifactDirectory, { recursive: true });
      for (const filename of [
        "hew_wasm.js",
        "hew_wasm_bg.wasm",
        "package.json",
      ]) {
        fs.copyFileSync(
          path.join(wasmDir, filename),
          path.join(artifactDirectory, filename),
        );
      }
      console.log(`Retained VM bridge artefacts in ${artifactDirectory}`);
    }
  } finally {
    if (ownsBindings) fs.rmSync(wasmDir, { recursive: true, force: true });
    delete globalThis.__hewSandboxCompileToSandboxBytecode;
    delete globalThis.__hewSandboxExecuteCodec;
    delete globalThis.__hewSandboxExecuteCalendar;
  }
});

// Keep current emitter behaviour checked independently of the recorded v1
// packages, which deliberately retain older valid lowering and omitted fields.
for (const dir of fs.readdirSync(path.join(root, "fixtures"))) {
  const fixture = path.join(root, "fixtures", dir);
  if (!fs.existsSync(path.join(fixture, "bytecode.json"))) continue;
  test(`freshly compiled fixture replay: ${dir}`, async () => {
    const source = fs.readFileSync(path.join(fixture, "main.hew"), "utf8");
    const output = globalThis.__hewSandboxCompileToSandboxBytecode(
      source,
      "sandbox-vm-export",
    );
    const compiled = typeof output === "string" ? JSON.parse(output) : output;
    assert.ok(compiled.bytecode, JSON.stringify(compiled.diagnostics));
    const expected = JSON.parse(
      fs.readFileSync(path.join(fixture, "expected.trace.json"), "utf8"),
    );
    const actual = await runBytecode(compiled.bytecode, {
      fixtureId: expected.fixture_id,
      traceId: expected.trace_id,
      replay: expected.replay,
      sandboxVersion: expected.sandbox_version,
    });
    assertFixtureTrace(actual, expected, dir);
  });
}

test("runProgram hello_world returns stdout and zero exit code", async () => {
  const source = fs.readFileSync(
    path.join(root, "fixtures/01-hello-world/main.hew"),
    "utf8",
  );
  const result = await runProgram(source, "");

  assert.equal(result.stdout, "Hello, sandbox!\n");
  assert.equal(result.exit_code, 0);
  assert.deepEqual(result.diagnostics, []);
});

for (const [name, expected] of [
  ["capacity", "4\n4\nlimit\n"],
  ["alias", "8\nretained backing\n"],
  ["bytes-slice", "1\nslice capacity\n"],
  ["remote-cow", "5\ndetach transaction\n4\n"],
]) {
  test(`actor-owned budget preserves ${name} accounting through cleanup`, async () => {
    const source = fs.readFileSync(
      path.join(
        repoRoot,
        `tests/core-acceptance/cases/actor-owned-budget-${name}.hew`,
      ),
      "utf8",
    );
    const result = await runProgram(source, "");
    // Handling Trapped preserves the caller. An unsupervised actor crash
    // still leaves the existing unrecovered process-exit debt.
    assert.equal(result.exit_code, 1, JSON.stringify(result));
    assert.equal(result.stdout, expected);
  });
}

test("actor-owned task publication reserves result and pending callable storage", async () => {
  const result = await runProgram(
    `
fn ready() -> i64 { 7 }
#[max_heap(8)]
actor Small {
    receive fn run() -> i64 {
        scope { let task = fork ready(); await task }
    }
}
#[max_heap(16)]
actor Enough {
    receive fn run() -> i64 {
        scope { let task = fork ready(); await task }
    }
}
fn main() -> i64 {
    let enough = spawn Enough;
    match enough.run() { .Ok(n) => println(n), _ => return 2, }
    stop(enough); stopped(enough);
    let small = spawn Small;
    match small.run() { .Err(.Trapped) => println("task limit"), _ => return 3, }
    stopped(small);
    0
}`,
    "",
  );
  assert.equal(result.stdout, "7\ntask limit\n", JSON.stringify(result));
  assert.equal(result.exit_code, 1, JSON.stringify(result));
});

test("actor-owned zero permits no-allocation turns and rejects oversized array backing", async () => {
  for (const count of ["4294967296", "9223372036854775807"]) {
    const result = await runProgram(
      `
#[max_heap(0)]
actor Empty {
    receive fn ready() {}
    receive fn allocate() {
        let values = [0; ${count}];
        println(values.len());
    }
}
fn main() -> i64 {
    let empty = spawn Empty;
    empty.ready().expect("zero-byte turn");
    match empty.allocate() { .Err(.Trapped) => println("array limit"), _ => return 2, }
    stopped(empty);
    0
}`,
      "",
    );
    assert.equal(result.stdout, "array limit\n", JSON.stringify(result));
    assert.equal(result.exit_code, 1, JSON.stringify(result));
  }
});

test("actor-owned mailbox refusal rolls back the recipient and preserves caller storage", async () => {
  const source = fs.readFileSync(
    path.join(
      repoRoot,
      "tests/core-acceptance/cases/actor-owned-budget-mailbox.hew",
    ),
    "utf8",
  );
  const result = await runProgram(source, "");
  assert.equal(result.stdout, "admission limit\n5\n", JSON.stringify(result));
  assert.equal(result.exit_code, 1, JSON.stringify(result));
});

for (const [name, file, stdout] of [
  ["String", "actor-owned-budget-pipe.hew", "6\n6\npipe limit\nsender retained\n"],
  ["Bytes slice", "actor-owned-budget-pipe-bytes.hew", "1\nretained slice capacity\nsender retained\n"],
]) {
  test(`actor-owned Pipe ${name} retains capacity and preserves a refused sender`, async () => {
    const source = fs.readFileSync(
      path.join(repoRoot, "tests/core-acceptance/cases", file), "utf8",
    );
    const result = await runProgram(source, "");
    assert.equal(result.stdout, stdout, JSON.stringify(result));
    assert.equal(result.exit_code, 1, JSON.stringify(result));
  });
}

test("actor-owned map and set snapshots retain native Vec capacity", async () => {
  const source = fs.readFileSync(
    path.join(
      repoRoot,
      "tests/core-acceptance/cases/actor-owned-budget-map.hew",
    ),
    "utf8",
  );
  const result = await runProgram(source, "");
  assert.equal(result.stdout, "2\n2\nsnapshot limit\n", JSON.stringify(result));
  assert.equal(result.exit_code, 1, JSON.stringify(result));
});

test("actor-owned select charges the actual reply envelope and preserves the sender", async () => {
  const source = fs.readFileSync(
    path.join(
      repoRoot,
      "tests/core-acceptance/cases/actor-owned-budget-reply.hew",
    ),
    "utf8",
  );
  const result = await runProgram(source, "");
  assert.equal(result.stdout, "7\nreply limit\n7\n", JSON.stringify(result));
  assert.equal(result.exit_code, 1, JSON.stringify(result));
});

test("actor-owned select retains the complete declared failure envelope", async () => {
  const source = fs.readFileSync(
    path.join(
      repoRoot,
      "tests/core-acceptance/cases/actor-owned-budget-fallible-reply.hew",
    ),
    "utf8",
  );
  const result = await runProgram(source, "");
  assert.equal(
    result.stdout,
    "9\ndeclared reply limit\n9\n",
    JSON.stringify(result),
  );
  assert.equal(result.exit_code, 1, JSON.stringify(result));
});

test("actor-owned collection iterators and arena retain structural typed descendants", async () => {
  const source = fs.readFileSync(
    path.join(
      repoRoot,
      "tests/core-acceptance/cases/actor-owned-budget-collections.hew",
    ),
    "utf8",
  );
  const result = await runProgram(source, "");
  assert.equal(result.stdout, "13\n", JSON.stringify(result));
  assert.equal(result.exit_code, 0, JSON.stringify(result));
});

test("actor-owned race reserves its owning member backing before publication", async () => {
  const source = fs.readFileSync(
    path.join(
      repoRoot,
      "tests/core-acceptance/cases/actor-owned-budget-race.hew",
    ),
    "utf8",
  );
  const result = await runProgram(source, "");
  assert.equal(result.stdout, "7\nrace limit\n", JSON.stringify(result));
  assert.equal(result.exit_code, 1, JSON.stringify(result));
});

test("actor-owned YAML copies retain one native tree until checked detach", async () => {
  const result = await runProgram(
    `
import std.encoding.yaml;
#[max_heap(80)]
actor Document {
    var values: yaml.Value;
    receive fn copied_len() -> i64 {
        let copy = values;
        copy.array_len().expect("array")
    }
    receive fn detach() {
        var copy = values;
        copy.push(yaml.from_int(5)).expect("array");
    }
}
fn main() -> i64 {
    let document = spawn Document { values: yaml.parse("[1,2,3,4]").expect("parse") };
    match document.copied_len() { .Ok(n) => println(n), _ => return 2, }
    match document.detach() { .Err(.Trapped) => println("tree limit"), _ => return 3, }
    stopped(document);
    0
}`,
    "",
  );
  assert.equal(result.stdout, "4\ntree limit\n", JSON.stringify(result));
  assert.equal(result.exit_code, 1, JSON.stringify(result));
});

test("actor-owned JSON mutation follows native retained capacity", async () => {
  const source = fs.readFileSync(
    path.join(
      repoRoot,
      "tests/core-acceptance/cases/actor-owned-budget-encoding.hew",
    ),
    "utf8",
  );
  const result = await runProgram(source, "");
  assert.equal(result.stdout, "32\nencoding limit\n", JSON.stringify(result));
  assert.equal(result.exit_code, 1, JSON.stringify(result));
});

test("actor-owned generator adoption transfers a live gated task scope", async () => {
  const source = fs.readFileSync(
    path.join(
      repoRoot,
      "tests/core-acceptance/cases/actor-owned-budget-generator-transfer.hew",
    ),
    "utf8",
  );
  const result = await runProgram(source, "");
  assert.equal(result.stdout, "7\n", JSON.stringify(result));
  assert.equal(result.exit_code, 0, JSON.stringify(result));
});

test("actor-owned generator adoption rebinds suspended authored cleanup", async () => {
  const source = fs.readFileSync(
    path.join(
      repoRoot,
      "tests/core-acceptance/cases/actor-owned-budget-cleanup-transfer.hew",
    ),
    "utf8",
  );
  const result = await runProgram(source, "");
  assert.equal(
    result.stdout,
    "cleanup transfer limit\n",
    JSON.stringify(result),
  );
  assert.equal(result.exit_code, 1, JSON.stringify(result));
});

test("actor-owned supervised restart releases the old incarnation before fresh capacity", async () => {
  const source = fs.readFileSync(
    path.join(
      repoRoot,
      "tests/core-acceptance/cases/actor-owned-budget-restart.hew",
    ),
    "utf8",
  );
  const result = await runProgram(source, "");
  assert.equal(
    result.stdout,
    "fresh account after cleanup\n",
    JSON.stringify(result),
  );
  assert.equal(result.exit_code, 0, JSON.stringify(result));
});

test("actor-owned final payload remains charged across suspended authored cleanup", async () => {
  const source = fs.readFileSync(
    path.join(
      repoRoot,
      "tests/core-acceptance/cases/actor-owned-budget-cleanup.hew",
    ),
    "utf8",
  );
  const result = await runProgram(source, "");
  assert.equal(result.stdout, "cleanup limit\n", JSON.stringify(result));
  assert.equal(result.exit_code, 1, JSON.stringify(result));
});

test("source-defined collection updates preserve ownership and callback recovery", async () => {
  const source = fs.readFileSync(
    path.join(repoRoot, "tests/core-acceptance/cases/collection-update.hew"),
    "utf8",
  );
  const result = await runProgram(source, "");
  assert.equal(result.exit_code, 0, JSON.stringify(result.diagnostics));
  assert.deepEqual(result.diagnostics, []);
  assert.equal(
    result.stdout,
    "factory coffee\nupdate coffee\nclosed update\nupdate recovered\nclosed factory\nfactory recovered\nclosed record\nrecord recovered\ncollection update ok\n",
  );
});

test("runProgram preserves timestamp codec values and located errors", async () => {
  const source = fs.readFileSync(
    path.join(repoRoot, "tests/core-acceptance/cases/timestamp-codecs.hew"),
    "utf8",
  );
  const result = await runProgram(source, "");
  assert.equal(result.exit_code, 0, JSON.stringify(result.diagnostics));
  assert.deepEqual(result.diagnostics, []);
  assert.equal(result.stdout, "timestamp codecs ok\n");
});

test("timestamp.now uses the replay clock's Unix coordinate exactly once", async () => {
  const output = globalThis.__hewSandboxCompileToSandboxBytecode(
    `
fn main() {
    let (seconds, nanos) = timestamp.now().unix_parts();
    println(f"{seconds}|{nanos}");
}
`,
    "sandbox-vm-export",
  );
  const compiled = typeof output === "string" ? JSON.parse(output) : output;
  assert.ok(compiled.bytecode, JSON.stringify(compiled.diagnostics));
  for (const [epoch_ms, current_ms, expected] of [
    [0, 0, "0|0\n"],
    [1700000000000, 1700000000123, "1700000000|123000000\n"],
    [-1000, -1, "-1|999000000\n"],
    [-1000, -1001, "-2|999000000\n"],
  ]) {
    const trace = await runBytecode(compiled.bytecode, {
      replay: { virtual_clock: { epoch_ms, current_ms, tick_ms: 0 } },
    });
    assert.equal(trace.result, "ok", JSON.stringify(trace.final_state));
    assert.equal(trace.final_state.stdout.join(""), expected);
  }
  for (const current_ms of [NaN, Infinity, -Infinity]) {
    const trace = await runBytecode(compiled.bytecode, {
      replay: { virtual_clock: { epoch_ms: 0, current_ms, tick_ms: 0 } },
    });
    assert.notEqual(trace.result, "ok");
  }
  const malformed = structuredClone(compiled.bytecode);
  const clockCall = malformed.functions
    .flatMap((fn) => fn.blocks)
    .map((block) => block.term)
    .find(
      (term) =>
        term.op === "runtime.call" &&
        malformed.runtime_families[term.family].family === "TimestampNow",
    );
  delete clockCall.aggregate_result_shape;
  assert.equal((await runBytecode(malformed)).result, "sandbox_rejected");
});

test("serialized bytecode preserves supervisor child i64 bounds", async () => {
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
    "sandbox-vm-export",
  );
  const compiled =
    typeof compileOutput === "string"
      ? JSON.parse(compileOutput)
      : compileOutput;

  assert.ok(
    compiled.diagnostics.every((diagnostic) => diagnostic.severity !== "error"),
    JSON.stringify(compiled.diagnostics),
  );
  assert.ok(compiled.bytecode, "compiler should emit bytecode");
  const bytecode = JSON.parse(JSON.stringify(compiled.bytecode));
  assert.equal(bytecode.schema_version, "hew.sandbox.bytecode.v1");
  const trace = await runBytecode(bytecode);
  assert.equal(trace.result, "ok");
  assert.equal(
    trace.final_state.stdout.join(""),
    "9223372036854775807|-9223372036854775808\n",
  );
});

test("runProgram reads two stdin lines from the page input buffer byte-cleanly", async () => {
  const result = await runProgram(
    `
import std.io;

fn main() {
    let first = io.read_line().expect("read stdin") ?? "<none>";
    let second = io.read_line().expect("read stdin") ?? "<none>";
    println(f"{first}|{second}");
}
`,
    "héw\r\nbytes\n",
  );

  assert.equal(result.stdout, "héw|bytes\n");
  assert.equal(result.exit_code, 0);
  assert.deepEqual(result.diagnostics, []);
});

test("runProgram hands a read_line loop successive lines, an empty line, then None at end of input", async () => {
  const result = await runProgram(
    `
import std.io;

fn main() {
    for i in 0..6 {
        match io.read_line().expect("read stdin") {
            .Some(line) => println(f"[{line}]"),
            .None => {
                println("end");
                return;
            }
        }
    }
}
`,
    "one\n\ntwo\r\nthree",
  );

  assert.equal(result.stdout, "[one]\n[]\n[two]\n[three]\nend\n");
  assert.equal(result.exit_code, 0);
  assert.deepEqual(result.diagnostics, []);
});

test("runProgram read_all keeps line endings and the unterminated tail after a read_line", async () => {
  const result = await runProgram(
    `
import std.io;

fn main() {
    let first = io.read_line().expect("read stdin") ?? "<none>";
    let rest = io.read_all().expect("read stdin");
    println(f"{first}|[{rest}]");
    let after = io.read_line().expect("read stdin") ?? "<none>";
    println(after);
}
`,
    "a\r\n\nb\r\nc",
  );

  assert.equal(result.stdout, "a|[\nb\r\nc]\n<none>\n");
  assert.equal(result.exit_code, 0);
  assert.deepEqual(result.diagnostics, []);
});

test("runProgram records each consumed stdin line as a replay.input event without duplicating the record", async () => {
  const compiled = globalThis.__hewSandboxCompileToSandboxBytecode(
    `
import std.io;

fn main() {
    let a = io.read_line().expect("read stdin") ?? "<none>";
    let b = io.read_line().expect("read stdin") ?? "<none>";
    println(a);
    println(b);
}
`,
    "sandbox-vm-export",
  );
  const bytecode =
    typeof compiled === "string"
      ? JSON.parse(compiled).bytecode
      : compiled.bytecode;
  const trace = await runBytecode(bytecode, {
    replay: { inputs: [{ kind: "stdin", data: "x\ny\n" }] },
  });
  // A read suspends, so the scheduler records its steps alongside; stdin is
  // persisted once, as given, and each read records the line it consumed.
  const stdin = (input) => input.kind === "stdin";
  assert.deepEqual(trace.replay.inputs.filter(stdin), [
    { kind: "stdin", data: "x\ny\n" },
  ]);
  assert.deepEqual(
    trace.events
      .filter((event) => event.type === "replay.input")
      .map((event) => event.replay_input)
      .filter(stdin),
    [
      { kind: "stdin", data: "x\n" },
      { kind: "stdin", data: "y\n" },
    ],
  );
  assert.deepEqual(trace.final_state.stdout, ["x\n", "y\n"]);
});

test("compute-only loops and recursion yield, cancel and close their owners", async () => {
  const source = fs.readFileSync(
    path.join(repoRoot, "tests/core-acceptance/cases/compute-safepoints.hew"),
    "utf8",
  );
  const result = await runProgram(source, "");
  assert.equal(result.exit_code, 0, JSON.stringify(result.diagnostics));
  assert.equal(
    result.stdout,
    "peer progressed\ndeferred loop 10000\nclosed loop\n7\npeer progressed\ndeferred recursion 10000\nclosed recursion\n7\ndeferred actor 10000\nclosed actor\nactor stopped\nclosed deadline\ndeadline recovered\nrollback:ORIGINAL\nmachine deadline recovered\n",
  );
});

test("authored close helpers yield without abandoning explicit or implicit release", async () => {
  const source = fs.readFileSync(
    path.join(
      repoRoot,
      "tests/core-acceptance/cases/compute-close-safepoints.hew",
    ),
    "utf8",
  );
  const result = await runProgram(source, "", undefined, {
    stepBudget: 2000000,
  });
  assert.equal(result.exit_code, 0, JSON.stringify(result.diagnostics));
  assert.equal(
    result.stdout,
    "peer progressed\nfinished 40000\nclosed explicit\n7\npeer progressed\nfinished 40000\nclosed implicit\n7\n",
  );
});

test("runProgram parse errors return diagnostics and do not execute cached bytecode", async () => {
  const result = await runProgram('fn main( {\n    println("nope");\n}\n', "");

  assert.notEqual(result.exit_code, 0);
  assert.equal(result.stdout, "");
  assert.ok(result.diagnostics.length > 0);
  assert.ok(
    result.diagnostics.some((diagnostic) => diagnostic.phase === "parse"),
  );
});

test("runProgram type errors return diagnostics and do not execute", async () => {
  const result = await runProgram(
    'fn main() {\n    let x: i64 = "oops";\n    println(x);\n}\n',
    "",
  );

  assert.notEqual(result.exit_code, 0);
  assert.equal(result.stdout, "");
  assert.ok(result.diagnostics.length > 0);
  assert.ok(
    result.diagnostics.some((diagnostic) => diagnostic.phase === "typecheck"),
  );
});

test("runProgram panics map to non-zero exit code and a trap diagnostic", async () => {
  const result = await runProgram(
    'fn main() {\n    panic("sandbox panic");\n}\n',
    "",
  );

  assert.notEqual(result.exit_code, 0);
  assert.ok(
    result.diagnostics.some(
      (diagnostic) =>
        diagnostic.phase === "run" && diagnostic.trap_kind === "panic",
    ),
  );
});

function buildSandboxWasmBridge() {
  if (!ownsBindings) {
    console.log(
      `Using existing VM bridge bindings from ${suppliedModule ?? wasmDir}`,
    );
    return;
  }
  const mode = process.env.HEW_WASM_PACK_MODE;
  const args = [
    "build",
    path.join(repoRoot, "hew-wasm"),
    "--target",
    "nodejs",
    "--dev",
    "--out-dir",
    wasmDir,
  ];
  if (mode) args.push("--mode", mode);
  const result = spawnSync("wasm-pack", args, {
    cwd: repoRoot,
    encoding: "utf8",
  });
  if (result.error) throw result.error;
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
  test(`compiled resource lifecycle: ${name}`, async () => {
    const result = await runProgram(
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

test("runProgram shares checked UTC calendar semantics with native", async () => {
  const source = fs.readFileSync(
    path.join(repoRoot, "tests/core-acceptance/cases/timestamp-calendar.hew"),
    "utf8",
  );
  const result = await runProgram(source, "");
  assert.equal(result.exit_code, 0, JSON.stringify(result.diagnostics));
  assert.deepEqual(result.diagnostics, []);
  assert.equal(result.stdout, "timestamp calendar ok\n");
});

test("runProgram executes the shared source cron parser and search", async () => {
  const source = fs.readFileSync(
    path.join(repoRoot, "tests/core-acceptance/cases/timestamp-cron.hew"),
    "utf8",
  );
  const result = await runProgram(source, "");
  assert.equal(result.exit_code, 0, JSON.stringify(result.diagnostics));
  assert.deepEqual(result.diagnostics, []);
  assert.equal(result.stdout, "timestamp cron ok\n");
});

test("calendar extern results require their checked record descriptor", async () => {
  const output = globalThis.__hewSandboxCompileToSandboxBytecode(
    'import std.time.datetime; fn main() { println(datetime.to_iso8601(timestamp.from_unix_secs(0)).expect("calendar")); }',
    "sandbox-vm-export",
  );
  const compiled = typeof output === "string" ? JSON.parse(output) : output;
  assert.ok(compiled.bytecode, JSON.stringify(compiled.diagnostics));
  for (const mismatch of ["missing", "reordered"]) {
    const malformed = structuredClone(compiled.bytecode);
    const call = malformed.functions
      .flatMap((fn) => fn.blocks)
      .map((block) => block.term)
      .find(
        (term) =>
          term.op === "extern.call" &&
          malformed.externs[term.extern].symbol === "hew_calendar_format",
      );
    assert.ok(call);
    if (mismatch === "missing") delete call.aggregate_result_shape;
    else malformed.aggregates[call.aggregate_result_shape].fields.reverse();
    const trace = await runBytecode(malformed, {
      calendarBridge: globalThis.__hewSandboxExecuteCalendar,
    });
    assert.equal(trace.result, "sandbox_rejected");
    assert.deepEqual(trace.final_state.stdout, []);
  }
});
