import assert from "node:assert/strict";
import test from "node:test";
import {
  buildPlaygroundState,
  decodePlaygroundShare,
  encodePlaygroundShare,
  runBytecode,
  stdlibProfileEntries,
  traceToDownloadJson,
  validateLessonManifest,
  validateStdlibProfile,
  StdlibProfileValidationError,
  PlaygroundIntegrationError
} from "../dist/interpreter/index.js";

test("M7 stdlib unsupported statuses emit typed fail-closed diagnostics", () => {
  for (const scenario of [
    {
      id: "out-of-scope",
      symbol: stdlibSymbol("sym:std.misc.toast", "std.misc", "toast"),
      expectedKind: "Unsupported::SANDBOX_OUT_OF_SCOPE",
      expectedStatus: "unsupported_out_of_scope"
    },
    {
      id: "deferred",
      symbol: stdlibSymbol("sym:std.encoding.csv.parse", "std.encoding.csv", "parse"),
      expectedKind: "Unsupported::M7_DEFERRED",
      expectedStatus: "unsupported_m7_deferred_to_post_v05"
    },
    {
      id: "native-only",
      symbol: stdlibSymbol("sym:std.fs.read", "std.fs", "read"),
      expectedKind: "Unsupported::NATIVE_ONLY",
      expectedStatus: "unsupported_native_only"
    }
  ]) {
    const trace = runBytecode(packageWithInstructions(`stdlib-${scenario.id}`, [
      instr("call.stdlib", null, [sym(scenario.symbol.id)])
    ], [scenario.symbol]), runOptions(`stdlib-${scenario.id}`));
    const failure = trace.final_state.runtime_failures[0];

    assert.equal(trace.result, "runtime_failure");
    assert.equal(failure.kind, "unsupported");
    assert.equal(failure.unsupported.kind, scenario.expectedKind);
    assert.equal(failure.unsupported.status, scenario.expectedStatus);
    assert.match(failure.message, new RegExp(`^${scenario.expectedKind.replaceAll(":", "\\:")}:`));
  }
});

test("M7 stdlib profile validates shim claims before runtime use", () => {
  assert.throws(
    () =>
      validateStdlibProfile(
        [{ id: "sym:bad.claim", module: "std.bad", name: "claim", status: "shim", reason: "test", handler: "missing.handler" }],
        new Set()
      ),
    (error) => error instanceof StdlibProfileValidationError && /not implemented/.test(error.message)
  );

  const statuses = new Set(stdlibProfileEntries().map((entry) => entry.status));
  assert.deepEqual(statuses, new Set(["shim", "unsupported_out_of_scope", "unsupported_m7_deferred_to_post_v05", "unsupported_native_only"]));
});

test("M7 page I/O, virtual clock, and conservative pure shims are deterministic", () => {
  const symbols = [
    stdlibSymbol("sym:std.time.now", "std.time", "now"),
    stdlibSymbol("sym:std.time.sleep", "std.time", "sleep"),
    stdlibSymbol("sym:std.time.deadline", "std.time", "deadline"),
    stdlibSymbol("sym:std.string.len", "std.string", "len"),
    stdlibSymbol("sym:std.string.slice", "std.string", "slice"),
    stdlibSymbol("sym:std.math.abs", "std.math", "abs"),
    stdlibSymbol("sym:std.math.max", "std.math", "max"),
    stdlibSymbol("sym:std.vec.push", "std.vec", "push"),
    stdlibSymbol("sym:std.vec.get", "std.vec", "get"),
    stdlibSymbol("sym:core.stdout.print", "core.stdout", "print"),
    stdlibSymbol("sym:core.stdout.println", "core.stdout", "println"),
    stdlibSymbol("sym:std.misc.log", "std.misc", "log")
  ];
  const bytecode = packageWithInstructions("stdlib-shims", [
    instr("call.stdlib", "local:now1", [sym("sym:std.time.now")]),
    instr("call.stdlib", null, [sym("sym:std.time.sleep"), lit(25)]),
    instr("call.stdlib", "local:now2", [sym("sym:std.time.now")]),
    instr("call.stdlib", "local:deadline", [sym("sym:std.time.deadline"), lit(10)]),
    instr("const.string", "local:text", [lit("héw")]),
    instr("call.stdlib", "local:len", [sym("sym:std.string.len"), loc("text")]),
    instr("call.stdlib", "local:slice", [sym("sym:std.string.slice"), loc("text"), lit(1), lit(2)]),
    instr("const.i64", "local:neg", [lit(-7)]),
    instr("call.stdlib", "local:abs", [sym("sym:std.math.abs"), loc("neg")]),
    instr("const.i64", "local:two", [lit(2)]),
    instr("call.stdlib", "local:max", [sym("sym:std.math.max"), loc("abs"), loc("two")]),
    instr("vector.new", "local:vec", [type("type:i64")]),
    instr("call.stdlib", null, [sym("sym:std.vec.push"), loc("vec"), loc("max")]),
    instr("call.stdlib", "local:item", [sym("sym:std.vec.get"), loc("vec"), lit(0)]),
    instr("call.stdlib", null, [sym("sym:core.stdout.print"), loc("now1"), loc("now2"), loc("deadline")]),
    instr("call.stdlib", null, [sym("sym:core.stdout.println"), loc("len"), loc("slice"), loc("item")]),
    instr("call.stdlib", null, [sym("sym:std.misc.log"), lit("log line")])
  ], symbols, [
    local("now1", "type:i64"),
    local("now2", "type:i64"),
    local("deadline", "type:i64"),
    local("text", "type:string"),
    local("len", "type:i64"),
    local("slice", "type:string"),
    local("neg", "type:i64"),
    local("abs", "type:i64"),
    local("two", "type:i64"),
    local("max", "type:i64"),
    local("vec", "type:VecI64"),
    local("item", "type:i64")
  ]);
  const first = runBytecode(bytecode, runOptions("stdlib-shims", replay(707, 5)));
  const replayed = runBytecode(bytecode, { ...runOptions("stdlib-shims", replay(707, 5)), replay: first.replay });

  assert.equal(first.result, "ok");
  assert.deepEqual(first.final_state.stdout, ["5 30 40", "3 é 7\n"]);
  assert.equal(first.final_state.virtual_clock.current_ms, 30);
  assert.equal(JSON.stringify(first), JSON.stringify(replayed));
  assert.ok(buildPlaygroundState(first).output.page_log.includes("log line"));
});

test("M7 playground JSON contract projects deterministic trace views and controls", () => {
  const state = buildPlaygroundState(fakeTrace());

  assert.equal(state.schema_version, "hew.sandbox.playground.v0");
  assert.equal(state.source_diagnostics.length, 2);
  assert.equal(state.runtime_trace.length, 7);
  assert.equal(state.views.actor_mailboxes.length, 1);
  assert.equal(state.views.supervisor_tree.length, 1);
  assert.equal(state.views.channel_queues.length, 1);
  assert.equal(state.views.machine_transitions.length, 1);
  assert.equal(state.controls.seed, 42);
  assert.equal(state.controls.virtual_clock.current_ms, 12);
  assert.equal(state.controls.stop_reset.reset_replay.inputs.length, 0);
  assert.match(state.trace_download.json, /"schema_version":"hew\.sandbox\.trace\.v0"/);
});

test("M7 share links are inert data and lesson virtual files validate fail-closed", () => {
  const source = "<script>globalThis.__hew_xss = true</script>\nprintln(\"safe data\")";
  const link = encodePlaygroundShare({ source, seed: 31337, fixture_id: "shared", profile: "sandbox.educational.v0" });
  const decoded = decodePlaygroundShare(link);

  assert.equal(decoded.source, source);
  assert.equal(decoded.seed, 31337);
  assert.equal(globalThis.__hew_xss, undefined);
  assert.throws(() => decodePlaygroundShare("hew=not-json"), PlaygroundIntegrationError);

  const lessons = validateLessonManifest([{ id: "hello", name: "Hello", source_path: "lessons/hello.hew", seed: 9 }], {
    "lessons/hello.hew": "println(\"hello\")"
  });
  assert.deepEqual(lessons, [{ id: "hello", name: "Hello", path: "lessons/hello.hew", source: "println(\"hello\")", seed: 9 }]);
  assert.throws(
    () => validateLessonManifest([{ id: "bad", name: "Bad", source_path: "../bad.hew", source: "println(1)" }]),
    PlaygroundIntegrationError
  );
});

test("M7 trace download JSON is byte-stable", () => {
  const trace = fakeTrace();
  assert.equal(traceToDownloadJson(trace), traceToDownloadJson(trace));
});

function runOptions(fixtureId, customReplay = replay(700, 0)) {
  return { fixtureId, traceId: `trace:${fixtureId}`, replay: customReplay, sandboxVersion: "0.0.0-spec" };
}

function replay(seed, currentMs) {
  return { seed, step_budget: 1000, virtual_clock: { epoch_ms: 0, tick_ms: 1, current_ms: currentMs }, inputs: [] };
}

function packageWithInstructions(id, instructions, stdlibSymbols, locals = [local("main.tmp", "type:unit")]) {
  return {
    schema_version: "hew.sandbox.bytecode.v0",
    package_id: `pkg:${id}`,
    hew_version: "0.5.0-pre",
    compiler_version: "test",
    profile: "sandbox.educational.v0",
    source_map: { sources: [], spans: [] },
    module_graph: {
      entry: "mod:main",
      modules: [{ id: "mod:main", path: "main.hew", source_id: "src:main", imports: [], functions: ["fn:main"] }]
    },
    layouts: {
      types: [
        { id: "type:unit", kind: "unit", name: "()" },
        { id: "type:i64", kind: "integer", name: "i64" },
        { id: "type:string", kind: "string", name: "String" },
        { id: "type:VecI64", kind: "vector", name: "Vec<i64>" }
      ],
      records: [],
      enums: [],
      actors: [],
      supervisors: [],
      machines: []
    },
    stdlib_symbols: stdlibSymbols,
    capabilities: [],
    functions: [
      {
        id: "fn:main",
        module: "mod:main",
        name: "main",
        params: [],
        result: "type:unit",
        locals,
        blocks: [{ id: "block:main.entry", params: [], instructions, terminator: { op: "return", args: [], span: null }, span: null }],
        span: null
      }
    ]
  };
}

function stdlibSymbol(id, module, name) {
  return { id, module, name, params: [], result: "type:unit", capability: null, admission: "allowed" };
}

function instr(op, dst = null, args = [], metadata = undefined) {
  return metadata === undefined ? { op, dst, args, span: null } : { op, dst, args, span: null, metadata };
}

function local(name, typeName) {
  return { id: `local:${name}`, name: null, type: typeName, mutable: false, span: null };
}

function loc(name) {
  return { kind: "local", value: `local:${name}` };
}

function lit(value) {
  return { kind: "literal", value };
}

function sym(value) {
  return { kind: "symbol", value };
}

function type(value) {
  return { kind: "type", value };
}

function fakeTrace() {
  const diagnostic = { phase: "type", severity: "error", code: "E_TEST", message: "test diagnostic", span: null, notes: [], suggestions: [] };
  return {
    schema_version: "hew.sandbox.trace.v0",
    trace_id: "trace:playground-contract",
    fixture_id: "playground-contract",
    profile: "sandbox.educational.v0",
    hew_version: "0.5.0-pre",
    sandbox_version: "0.0.0-spec",
    result: "ok",
    replay: replay(42, 12),
    events: [
      { seq: 0, type: "trace.started", phase: "run", span: null, message: "fixture start" },
      { seq: 1, type: "compile.diagnostic", phase: "compile", span: null, diagnostic },
      { seq: 2, type: "state.snapshot", phase: "run", span: null, message: "actor.send", text: "{\"actor_id\":\"actor:a1\",\"mailbox_depth\":1}" },
      { seq: 3, type: "state.snapshot", phase: "run", span: null, message: "supervisor-tree-state", text: "{\"supervisor_id\":\"supervisor:s1\",\"slots\":[]}" },
      { seq: 4, type: "state.snapshot", phase: "run", span: null, message: "channel-send", text: "{\"channel_id\":\"channel:c1\",\"depth\":1}" },
      { seq: 5, type: "state.snapshot", phase: "run", span: null, message: "machine.transition", text: "{\"machine_id\":\"machine:m1\",\"from\":\"a\",\"to\":\"b\"}" },
      { seq: 6, type: "trace.ended", phase: "run", span: null, message: "ok" }
    ],
    final_state: {
      status: "ok",
      exit_code: 0,
      step_count: 1,
      budget_remaining: 999,
      virtual_clock: { epoch_ms: 0, tick_ms: 1, current_ms: 12 },
      stdout: ["hello\n"],
      stderr: [],
      ids: { actors: ["actor:root"], channels: [], tasks: [], supervisors: [], machines: [] },
      diagnostics: [diagnostic],
      sandbox_rejections: [],
      runtime_failures: [],
      globals: []
    }
  };
}
