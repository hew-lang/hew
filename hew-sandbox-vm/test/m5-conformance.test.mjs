import Ajv2020 from "ajv/dist/2020.js";
import assert from "node:assert/strict";
import fs from "node:fs";
import path from "node:path";
import test from "node:test";
import { fileURLToPath } from "node:url";
import { runBytecode } from "../dist/interpreter/index.js";

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");
const ajv = new Ajv2020({ allErrors: true, strict: true, validateFormats: false });
const validateTrace = ajv.compile(readJson("specs/trace-schema-v0.schema.json"));

test("M5 bounded channel send/recv fixture", () => {
  const trace = assertFixture("bounded-channel", boundedChannelPackage(), 501);

  assert.equal(trace.final_state.status, "ok");
  assert.deepEqual(trace.final_state.stdout, ["1 2\n"]);
  assert.deepEqual(trace.final_state.ids.channels, ["channel:c1"]);
  assert.deepEqual(
    traceFamilies(trace).filter((family) => family.startsWith("channel-")),
    ["channel-new", "channel-send", "channel-send", "channel-recv", "channel-recv"]
  );
});

test("M5 channel close drains buffered values and traps on double-close", () => {
  const trace = assertFixture("channel-close", channelClosePackage(), 502);

  assert.equal(trace.final_state.status, "ok");
  assert.deepEqual(trace.final_state.stdout, ["9\n"]);
  assert.deepEqual(
    traceFamilies(trace).filter((family) => family.startsWith("channel-")),
    ["channel-new", "channel-send", "channel-close", "channel-recv"]
  );

  const doubleClose = runBytecode(channelDoubleClosePackage(), { fixtureId: "channel-double-close", traceId: "trace:channel-double-close", replay: replay(503) });
  assert.equal(doubleClose.result, "trap");
  assert.match(doubleClose.final_state.runtime_failures[0].message, /already closed/);
});

test("M5 async/await fixture", () => {
  const trace = assertFixture("async-await", asyncAwaitPackage(), 504);

  assert.equal(trace.final_state.status, "ok");
  assert.deepEqual(trace.final_state.stdout, ["42\n"]);
  assert.deepEqual(trace.final_state.ids.tasks, ["task:t1"]);
  assert.deepEqual(
    traceFamilies(trace).filter((family) => family.startsWith("async-") || family === "task.scheduler-step"),
    ["async-spawn", "task.scheduler-step", "async-resume", "async-complete"]
  );
});

test("M5 structured scope cancellation fixture", () => {
  const trace = assertFixture("scope-cancel", scopeCancelPackage(), 505);

  assert.equal(trace.final_state.status, "ok");
  assert.deepEqual(trace.final_state.stdout, ["scope done\n"]);
  assert.deepEqual(trace.final_state.ids.tasks, ["task:t1"]);
  assert.deepEqual(
    traceFamilies(trace).filter((family) => family.startsWith("scope-") || family.startsWith("async-")),
    ["scope-enter", "async-spawn", "async-complete", "scope-exit"]
  );
  assert.ok(!traceFamilies(trace).includes("async-resume"));
});

test("M5 select tie-break fixture is seed-deterministic and cleans losers", () => {
  const trace = assertFixture("select-tie", selectTiePackage(), 506);
  const selected = trace.final_state.stdout[0];

  assert.equal(trace.final_state.status, "ok");
  assert.ok(selected === "10\n" || selected === "20\n");
  assert.deepEqual(trace.final_state.ids.channels, ["channel:c1", "channel:c2"]);
  assert.equal(traceFamilies(trace).filter((family) => family === "select-resolve").length, 1);
  assert.equal(traceFamilies(trace).filter((family) => family === "select-loser-cleanup").length, 1);
  assert.equal(traceFamilies(trace).filter((family) => family === "channel-recv").length, 1);
});

test("M5 timer select fixture advances virtual time", () => {
  const trace = assertFixture("select-timer", selectTimerPackage(), 507);

  assert.equal(trace.final_state.status, "ok");
  assert.deepEqual(trace.final_state.stdout, ["timeout\n"]);
  assert.equal(trace.final_state.virtual_clock.current_ms, 25);
  assert.deepEqual(
    traceFamilies(trace).filter((family) => family === "select-resolve" || family === "clock.virtual_advance"),
    ["select-resolve", "clock.virtual_advance"]
  );
});

test("M5 in-memory duplex stream fixture uses bounded channel semantics", () => {
  const trace = assertFixture("memory-duplex", memoryDuplexPackage(), 508);

  assert.equal(trace.final_state.status, "ok");
  assert.deepEqual(trace.final_state.stdout, ["77\n"]);
  assert.deepEqual(trace.final_state.ids.channels, ["channel:c1"]);
  assert.deepEqual(
    traceFamilies(trace).filter((family) => family.startsWith("channel-")),
    ["channel-new", "channel-send", "channel-recv", "channel-close"]
  );
});

function assertFixture(fixtureId, bytecode, seed) {
  const actual = runBytecode(bytecode, {
    fixtureId,
    traceId: `trace:${fixtureId}`,
    replay: replay(seed),
    sandboxVersion: "0.0.0-spec"
  });
  assert.ok(validateTrace(actual), ajv.errorsText(validateTrace.errors));

  const rerun = runBytecode(bytecode, {
    fixtureId,
    traceId: `trace:${fixtureId}`,
    replay: actual.replay,
    sandboxVersion: "0.0.0-spec"
  });
  assert.equal(JSON.stringify(actual), JSON.stringify(rerun), `${fixtureId} replay rerun is not byte-stable`);
  assert.deepEqual(traceFamilies(actual), traceFamilies(rerun), `${fixtureId} trace family sequence changed`);
  return actual;
}

function boundedChannelPackage() {
  return mainPackage("bounded-channel", [
    local("ch", "type:ChannelI64"),
    local("one", "type:i64"),
    local("two", "type:i64"),
    local("out1", "type:i64"),
    local("out2", "type:i64")
  ], [
    instr("channel.new", "local:ch", [type("type:ChannelI64"), lit(2)]),
    instr("const.i64", "local:one", [lit(1)]),
    instr("const.i64", "local:two", [lit(2)]),
    instr("channel.send", null, [loc("ch"), loc("one")]),
    instr("channel.send", null, [loc("ch"), loc("two")]),
    instr("channel.recv", "local:out1", [loc("ch")]),
    instr("channel.recv", "local:out2", [loc("ch")]),
    println(loc("out1"), loc("out2"))
  ]);
}

function channelClosePackage() {
  return mainPackage("channel-close", [
    local("ch", "type:ChannelI64"),
    local("value", "type:i64"),
    local("out", "type:i64")
  ], [
    instr("channel.new", "local:ch", [type("type:ChannelI64"), lit(1)]),
    instr("const.i64", "local:value", [lit(9)]),
    instr("channel.send", null, [loc("ch"), loc("value")]),
    instr("channel.close", null, [loc("ch")]),
    instr("channel.recv", "local:out", [loc("ch")]),
    println(loc("out"))
  ]);
}

function channelDoubleClosePackage() {
  return mainPackage("channel-double-close", [local("ch", "type:ChannelI64")], [
    instr("channel.new", "local:ch", [type("type:ChannelI64"), lit(1)]),
    instr("channel.close", null, [loc("ch")]),
    instr("channel.close", null, [loc("ch")])
  ]);
}

function asyncAwaitPackage() {
  const pkg = mainPackage("async-await", [local("task", "type:TaskI64"), local("out", "type:i64")], [
    instr("task.spawn", "local:task", [fnop("fn:compute")]),
    instr("task.await", "local:out", [loc("task")]),
    println(loc("out"))
  ]);
  pkg.module_graph.modules[0].functions.unshift("fn:compute");
  pkg.functions.unshift(computeFunction());
  return pkg;
}

function scopeCancelPackage() {
  const pkg = mainPackage("scope-cancel", [local("task", "type:TaskI64"), local("msg", "type:string")], [
    instr("scope.enter"),
    instr("scope.launch", "local:task", [fnop("fn:compute")]),
    instr("scope.exit"),
    instr("const.string", "local:msg", [lit("scope done")]),
    println(loc("msg"))
  ]);
  pkg.module_graph.modules[0].functions.unshift("fn:compute");
  pkg.functions.unshift(computeFunction());
  return pkg;
}

function selectTiePackage() {
  return mainPackage("select-tie", [
    local("leftCh", "type:ChannelI64"),
    local("rightCh", "type:ChannelI64"),
    local("left", "type:i64"),
    local("right", "type:i64"),
    local("selected", "type:i64")
  ], [
    instr("channel.new", "local:leftCh", [type("type:ChannelI64"), lit(1)]),
    instr("channel.new", "local:rightCh", [type("type:ChannelI64"), lit(1)]),
    instr("const.i64", "local:left", [lit(10)]),
    instr("const.i64", "local:right", [lit(20)]),
    instr("channel.send", null, [loc("leftCh"), loc("left")]),
    instr("channel.send", null, [loc("rightCh"), loc("right")]),
    instr("select.poll", "local:selected", [loc("leftCh"), loc("rightCh")], {
      arms: [
        { kind: "recv", label: "left", arg: 0 },
        { kind: "recv", label: "right", arg: 1 }
      ]
    }),
    println(loc("selected"))
  ]);
}

function selectTimerPackage() {
  return mainPackage("select-timer", [local("selected", "type:string")], [
    instr("select.poll", "local:selected", [], { arms: [{ kind: "timer", label: "timeout", after_ms: 25 }] }),
    println(loc("selected"))
  ]);
}

function memoryDuplexPackage() {
  return mainPackage("memory-duplex", [
    local("duplex", "type:DuplexI64"),
    local("value", "type:i64"),
    local("out", "type:i64")
  ], [
    instr("stream.memory", "local:duplex", [lit(1)]),
    instr("const.i64", "local:value", [lit(77)]),
    instr("stream.write", null, [loc("duplex"), loc("value")]),
    instr("stream.read", "local:out", [loc("duplex")]),
    instr("stream.close", null, [loc("duplex")]),
    println(loc("out"))
  ]);
}

function computeFunction() {
  return {
    id: "fn:compute",
    module: "mod:main",
    name: "compute",
    params: [],
    result: "type:i64",
    locals: [local("compute.result", "type:i64")],
    blocks: [
      {
        id: "block:compute.entry",
        params: [],
        instructions: [instr("const.i64", "local:compute.result", [lit(42)])],
        terminator: { op: "return", args: [loc("compute.result")], span: null },
        span: null
      }
    ],
    span: null
  };
}

function mainPackage(id, locals, instructions) {
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
        { id: "type:ChannelI64", kind: "opaque", name: "Channel<i64>" },
        { id: "type:TaskI64", kind: "opaque", name: "Task<i64>" },
        { id: "type:DuplexI64", kind: "opaque", name: "Duplex<i64>" }
      ],
      records: [],
      enums: [],
      actors: [],
      supervisors: [],
      machines: []
    },
    stdlib_symbols: [
      {
        id: "sym:core.stdout.println",
        module: "core.stdout",
        name: "println",
        params: ["type:any"],
        result: "type:unit",
        capability: "core.stdout",
        admission: "allowed"
      }
    ],
    capabilities: [{ id: "core.stdout", disposition: "allowed", reason: "page stdout shim", required_by: ["sym:core.stdout.println"] }],
    functions: [
      {
        id: "fn:main",
        module: "mod:main",
        name: "main",
        params: [],
        result: "type:unit",
        locals,
        blocks: [
          {
            id: "block:main.entry",
            params: [],
            instructions,
            terminator: { op: "return", args: [], span: null },
            span: null
          }
        ],
        span: null
      }
    ]
  };
}

function instr(op, dst = null, args = [], metadata = undefined) {
  return metadata === undefined ? { op, dst, args, span: null } : { op, dst, args, span: null, metadata };
}

function println(...args) {
  return instr("call.stdlib", null, [sym("sym:core.stdout.println"), ...args]);
}

function local(name, type) {
  return { id: `local:${name}`, name: null, type, mutable: false, span: null };
}

function loc(name) {
  return { kind: "local", value: `local:${name}` };
}

function lit(value) {
  return { kind: "literal", value };
}

function type(value) {
  return { kind: "type", value };
}

function fnop(value) {
  return { kind: "function", value };
}

function sym(value) {
  return { kind: "symbol", value };
}

function replay(seed) {
  return { seed, step_budget: 1000, virtual_clock: { epoch_ms: 0, tick_ms: 1, current_ms: 0 }, inputs: [] };
}

function traceFamilies(trace) {
  return trace.events.map((event) => event.message ?? event.type);
}

function readJson(relativePath) {
  return JSON.parse(fs.readFileSync(path.join(root, relativePath), "utf8"));
}
