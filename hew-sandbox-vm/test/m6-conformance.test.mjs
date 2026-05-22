import assert from "node:assert/strict";
import fs from "node:fs";
import path from "node:path";
import test from "node:test";
import { fileURLToPath } from "node:url";
import { runBytecode } from "../dist/interpreter/index.js";
import { TraceBuilder, runtimeFailure } from "../dist/interpreter/trace.js";
import { ActorScheduler, ActorTurnCrash, SchedulerRuntimeError } from "../dist/scheduler/scheduler.js";
import { UNIT } from "../dist/interpreter/values.js";

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");

test("M6 child crash fixture records hook observation before supervisor restart", () => {
  const trace = runFixture("30-child-crash-supervisor-restart-decision");
  const families = traceFamilies(trace);

  assert.equal(trace.result, "ok");
  assert.equal(count(families, "supervisor-spec-registered"), 1);
  assert.equal(count(families, "child-spawned"), 2);
  assert.equal(count(families, "child-crashed"), 1);
  assert.equal(count(families, "restart-decision"), 1);
  assert.ok(indexOfMessage(trace, "actor.lifecycle", '"hook":"on(crash)"') < indexOfMessage(trace, "restart-decision"));
  assert.ok(eventText(trace, "restart-decision").includes('"decision":"restart"'));
  assert.ok(eventText(trace, "restart-decision").includes('"budget_remaining":0'));
});

test("M6 restart budget exhaustion is distinct from channel would-block", () => {
  const exhausted = runBytecode(supervisorCrashPackage("budget-exhausted", { restartIntensity: 0 }), runOptions("budget-exhausted", 601));

  assert.equal(exhausted.result, "trap");
  assert.equal(exhausted.final_state.runtime_failures[0].trap_kind, "budget_exhausted");
  assert.match(exhausted.final_state.runtime_failures[0].message, /M6_SUPERVISOR_BUDGET_EXHAUSTED/);
  assert.ok(traceFamilies(exhausted).includes("budget-exhausted"));
  assert.ok(traceFamilies(exhausted).includes("supervisor-escalated"));

  const wouldBlock = runBytecode(channelWouldBlockPackage(), runOptions("channel-would-block", 602));
  assert.equal(wouldBlock.result, "trap");
  assert.equal(wouldBlock.final_state.runtime_failures[0].trap_kind, "invalid_call");
  assert.match(wouldBlock.final_state.runtime_failures[0].message, /M6_CHANNEL_SEND_WOULD_BLOCK/);
});

test("M6 link exit is enqueued during crash before the crash hook returns", () => {
  const trace = runBytecode(linkPackage(), runOptions("link-exit", 603));

  assert.equal(trace.result, "ok");
  assert.ok(traceFamilies(trace).includes("link-formed"));
  assert.ok(traceFamilies(trace).includes("link-broken-by-death"));
  assert.ok(indexOfMessage(trace, "link-broken-by-death") < indexOfMessage(trace, "actor.lifecycle", '"hook":"on(crash)"'));
  assert.ok(indexOfMessage(trace, "actor.send", '"handler":"link.exit"') < indexOfMessage(trace, "actor.lifecycle", '"hook":"on(crash)"'));
  assert.deepEqual(trace.final_state.stdout, ["crash hook\n", "actor:a1\n"]);
});

test("M6 monitors fire on crash and immediately for already-dead actors", () => {
  const trace = runBytecode(monitorPackage(), runOptions("monitor-fired", 604));

  assert.equal(trace.result, "ok");
  assert.ok(traceFamilies(trace).includes("monitor-formed"));
  assert.ok(traceFamilies(trace).includes("monitor-fired"));
  assert.deepEqual(trace.final_state.stdout, ["crash hook\n", "actor:a1\n"]);

  const { scheduler, trace: directTrace, boom, observer } = directScheduler();
  scheduler.send(boom, "explode", [], null);
  scheduler.drain();
  const ref = scheduler.monitor(observer, boom, null);
  assert.equal(ref.kind, "monitor");
  assert.ok(traceFamilies(directTrace.finish()).includes("monitor-fired"));
});

test("M6 link to a dead actor traps fail-closed", () => {
  const { scheduler, boom, observer } = directScheduler();
  scheduler.send(boom, "explode", [], null);
  scheduler.drain();

  assert.throws(
    () => scheduler.link(observer, boom, null),
    (error) =>
      error instanceof SchedulerRuntimeError &&
      error.trapKind === "invalid_call" &&
      /M6_LINK_DEAD_ACTOR/.test(error.message)
  );
});

test("M6 start_spec failures consume restart budget and escalate root supervisors", () => {
  const trace = runBytecode(startSpecFailurePackage(), runOptions("start-spec-failure", 605));

  assert.equal(trace.result, "trap");
  assert.equal(trace.final_state.runtime_failures[0].trap_kind, "budget_exhausted");
  assert.ok(eventText(trace, "child-crashed").includes("start_spec_failed"));
  assert.ok(traceFamilies(trace).includes("budget-exhausted"));
});

test("M6 restart windows use virtual time and replay deterministically", () => {
  const pkg = virtualWindowPackage();
  const first = runBytecode(pkg, runOptions("virtual-window", 606));
  const replayed = runBytecode(pkg, { ...runOptions("virtual-window", 606), replay: first.replay });

  assert.equal(first.result, "ok");
  assert.equal(count(traceFamilies(first), "restart-decision"), 2);
  assert.equal(first.final_state.virtual_clock.current_ms, 20);
  assert.equal(JSON.stringify(first), JSON.stringify(replayed));
});

test("M6 on(crash) hook return is ignored by temporary restart policy", () => {
  const trace = runBytecode(supervisorCrashPackage("hook-return-ignored", { restart: "temporary", crashHookReturn: "restart" }), runOptions("hook-return-ignored", 607));

  assert.equal(trace.result, "ok");
  assert.equal(count(traceFamilies(trace), "child-spawned"), 1);
  assert.ok(eventText(trace, "restart-decision").includes('"decision":"stop"'));
});

test("M6 supervisor calls from crash hooks fail closed and do not drive restart decisions", () => {
  const trace = runBytecode(hookSupervisorDeniedPackage(), runOptions("hook-supervisor-denied", 608));

  assert.equal(trace.result, "ok");
  assert.ok(eventText(trace, "actor.hook-crash").includes("M6_SUPERVISOR_HOOK_DENIED"));
  assert.ok(traceFamilies(trace).includes("restart-decision"));
  assert.equal(count(traceFamilies(trace), "child-spawned"), 2);
});

test("M6 hook panic is traced and supervisor proceeds from the original crash", () => {
  const trace = runBytecode(hookPanicPackage(), runOptions("hook-panic", 609));

  assert.equal(trace.result, "ok");
  assert.ok(eventText(trace, "actor.hook-crash").includes("hook panic"));
  assert.ok(traceFamilies(trace).includes("restart-decision"));
  assert.equal(count(traceFamilies(trace), "child-spawned"), 2);
});

test("M6 max_heap exceeded restarts with the same max_heap limit", () => {
  const trace = runBytecode(maxHeapPackage(), runOptions("max-heap-restart", 610));

  assert.equal(trace.result, "ok");
  assert.ok(eventText(trace, "actor.heap").includes('"max_heap":1'));
  assert.ok(eventText(trace, "actor.heap").includes('"used":3'));
  assert.equal(count(traceFamilies(trace), "child-spawned"), 2);
  assert.ok(trace.events.filter((event) => event.message === "child-spawned").every((event) => event.text.includes('"max_heap":1')));
});

test("M6 task driver trampolines nested awaits with a deterministic trace", () => {
  const trace = runBytecode(taskTrampolinePackage(), runOptions("task-trampoline", 612));

  assert.equal(trace.result, "ok");
  assert.ok(traceFamilies(trace).includes("async-trampoline"));
  assert.equal(JSON.stringify(trace), JSON.stringify(runBytecode(taskTrampolinePackage(), { ...runOptions("task-trampoline", 612), replay: trace.replay })));
});

function runFixture(name) {
  return runBytecode(readJson(`fixtures/${name}/bytecode.json`), runOptions(name.replace(/^\d+-/, ""), 600));
}

function supervisorCrashPackage(id, options = {}) {
  return supervisorPackage(id, {
    restart: options.restart ?? "permanent",
    strategy: options.strategy ?? "one_for_one",
    restartIntensity: options.restartIntensity ?? 1,
    restartWindowMs: options.restartWindowMs ?? 60_000,
    workerHandlers: [
      { name: "explode", function: "fn:worker.explode" },
      { name: "on(crash)", function: "fn:worker.crash" }
    ],
    extraFunctions: [explodeFunction(), crashHookFunction(options.crashHookReturn ?? "observed")],
    mainInstructions: [
      instr("supervisor.spawn", "local:sup", [type("type:Sup")]),
      instr("supervisor.child", "local:child", [loc("sup"), lit("worker")]),
      instr("actor.send", null, [loc("child"), sym("explode")])
    ],
    mainLocals: [local("sup", "type:Sup"), local("child", "type:Worker")]
  });
}

function linkPackage() {
  const pkg = supervisorCrashPackage("link-exit");
  pkg.layouts.types.push({ id: "type:Observer", kind: "actor", name: "Observer" });
  pkg.layouts.actors.push({ id: "type:Observer", name: "Observer", state_fields: [], handlers: [{ name: "link.exit", function: "fn:observer.link_exit" }] });
  pkg.functions.unshift(linkExitFunction());
  pkg.module_graph.modules[0].functions.unshift("fn:observer.link_exit");
  pkg.functions.find((fn) => fn.id === "fn:main").locals.push(local("observer", "type:Observer"));
  pkg.functions.find((fn) => fn.id === "fn:main").blocks[0].instructions = [
    instr("supervisor.spawn", "local:sup", [type("type:Sup")]),
    instr("supervisor.child", "local:child", [loc("sup"), lit("worker")]),
    instr("actor.spawn", "local:observer", [type("type:Observer")]),
    instr("actor.link", null, [loc("child"), loc("observer")]),
    instr("actor.send", null, [loc("child"), sym("explode")])
  ];
  return pkg;
}

function monitorPackage() {
  const pkg = linkPackage();
  pkg.package_id = "pkg:monitor-fired";
  pkg.layouts.actors.find((actor) => actor.id === "type:Observer").handlers = [{ name: "monitor.down", function: "fn:observer.monitor_down" }];
  pkg.functions = pkg.functions.filter((fn) => fn.id !== "fn:observer.link_exit");
  pkg.functions.unshift(monitorDownFunction());
  pkg.module_graph.modules[0].functions = pkg.module_graph.modules[0].functions.filter((id) => id !== "fn:observer.link_exit");
  pkg.module_graph.modules[0].functions.unshift("fn:observer.monitor_down");
  pkg.functions.find((fn) => fn.id === "fn:main").locals.push(local("mon", "type:Monitor"));
  pkg.layouts.types.push({ id: "type:Monitor", kind: "opaque", name: "Monitor" });
  pkg.functions.find((fn) => fn.id === "fn:main").blocks[0].instructions = [
    instr("supervisor.spawn", "local:sup", [type("type:Sup")]),
    instr("supervisor.child", "local:child", [loc("sup"), lit("worker")]),
    instr("actor.spawn", "local:observer", [type("type:Observer")]),
    instr("actor.monitor", "local:mon", [loc("observer"), loc("child")]),
    instr("actor.send", null, [loc("child"), sym("explode")])
  ];
  return pkg;
}

function startSpecFailurePackage() {
  return supervisorPackage("start-spec-failure", {
    restartIntensity: 4,
    childActor: "type:MissingWorker",
    workerHandlers: [],
    extraFunctions: [],
    mainLocals: [local("sup", "type:Sup")],
    mainInstructions: [instr("supervisor.spawn", "local:sup", [type("type:Sup")])]
  });
}

function virtualWindowPackage() {
  const pkg = supervisorPackage("virtual-window", {
    restartIntensity: 1,
    restartWindowMs: 10,
    workerHandlers: [{ name: "explode", function: "fn:worker.reply_then_explode" }],
    extraFunctions: [replyThenExplodeFunction()],
    mainLocals: [local("sup", "type:Sup"), local("child1", "type:Worker"), local("child2", "type:Worker"), local("reply1", "type:string"), local("reply2", "type:string")],
    mainInstructions: [
      instr("supervisor.spawn", "local:sup", [type("type:Sup")]),
      instr("supervisor.child", "local:child1", [loc("sup"), lit("worker")]),
      instr("actor.ask", "local:reply1", [loc("child1"), sym("explode")]),
      instr("clock.sleep", null, [lit(20)]),
      instr("supervisor.child", "local:child2", [loc("sup"), lit("worker")]),
      instr("actor.ask", "local:reply2", [loc("child2"), sym("explode")])
    ]
  });
  return pkg;
}

function hookSupervisorDeniedPackage() {
  return supervisorPackage("hook-supervisor-denied", {
    workerHandlers: [
      { name: "explode", function: "fn:worker.explode" },
      { name: "on(crash)", function: "fn:worker.bad_hook" }
    ],
    extraFunctions: [explodeFunction(), supervisorDeniedHookFunction()],
    mainLocals: [local("sup", "type:Sup"), local("child", "type:Worker")],
    mainInstructions: [
      instr("supervisor.spawn", "local:sup", [type("type:Sup")]),
      instr("supervisor.child", "local:child", [loc("sup"), lit("worker")]),
      instr("actor.send", null, [loc("child"), sym("explode")])
    ]
  });
}

function hookPanicPackage() {
  return supervisorPackage("hook-panic", {
    workerHandlers: [
      { name: "explode", function: "fn:worker.explode" },
      { name: "on(crash)", function: "fn:worker.hook_panic" }
    ],
    extraFunctions: [explodeFunction(), hookPanicFunction()],
    mainLocals: [local("sup", "type:Sup"), local("child", "type:Worker")],
    mainInstructions: [
      instr("supervisor.spawn", "local:sup", [type("type:Sup")]),
      instr("supervisor.child", "local:child", [loc("sup"), lit("worker")]),
      instr("actor.send", null, [loc("child"), sym("explode")])
    ]
  });
}

function maxHeapPackage() {
  return supervisorPackage("max-heap-restart", {
    workerLayout: {
      id: "type:Worker",
      name: "Worker",
      state_fields: [{ name: "items", type: "type:Vec", index: 0 }],
      handlers: [{ name: "grow", function: "fn:worker.grow" }],
      max_heap: 1
    },
    workerHandlers: [],
    extraFunctions: [growFunction()],
    mainLocals: [local("sup", "type:Sup"), local("child", "type:Worker")],
    mainInstructions: [
      instr("supervisor.spawn", "local:sup", [type("type:Sup")]),
      instr("supervisor.child", "local:child", [loc("sup"), lit("worker")]),
      instr("actor.send", null, [loc("child"), sym("grow")])
    ]
  });
}

function channelWouldBlockPackage() {
  return basePackage("channel-would-block", [local("ch", "type:ChannelI64"), local("one", "type:i64"), local("two", "type:i64")], [
    instr("channel.new", "local:ch", [type("type:ChannelI64"), lit(1)]),
    instr("const.i64", "local:one", [lit(1)]),
    instr("const.i64", "local:two", [lit(2)]),
    instr("channel.send", null, [loc("ch"), loc("one")]),
    instr("channel.send", null, [loc("ch"), loc("two")])
  ], [], [], []);
}

function taskTrampolinePackage() {
  return basePackage("task-trampoline", [local("task", "type:TaskI64"), local("out", "type:i64")], [
    instr("task.spawn", "local:task", [fnop("fn:parent_task")]),
    instr("task.await", "local:out", [loc("task")])
  ], [], [], [childTaskFunction(), parentTaskFunction()]);
}

function supervisorPackage(id, options) {
  const workerLayout = options.workerLayout ?? {
    id: "type:Worker",
    name: "Worker",
    state_fields: [],
    handlers: options.workerHandlers,
    max_mailbox: 4
  };
  return basePackage(id, options.mainLocals, options.mainInstructions, [workerLayout], [
    {
      id: "type:Sup",
      name: "Sup",
      strategy: options.strategy ?? "one_for_one",
      restart_intensity: options.restartIntensity ?? 1,
      restart_window_ms: options.restartWindowMs ?? 60_000,
      child_specs: [
        {
          id: "worker",
          restart: options.restart ?? "permanent",
          start_spec: { actor: options.childActor ?? "type:Worker", args: [] }
        }
      ]
    }
  ], options.extraFunctions);
}

function basePackage(id, mainLocals, mainInstructions, actors, supervisors, extraFunctions) {
  return {
    schema_version: "hew.sandbox.bytecode.v0",
    package_id: `pkg:${id}`,
    hew_version: "0.5.0-pre",
    compiler_version: "test",
    profile: "sandbox.educational.v0",
    source_map: { sources: [], spans: [] },
    module_graph: {
      entry: "mod:main",
      modules: [{ id: "mod:main", path: "main.hew", source_id: "src:main", imports: [], functions: [...extraFunctions.map((fn) => fn.id), "fn:main"] }]
    },
    layouts: {
      types: [
        { id: "type:unit", kind: "unit", name: "()" },
        { id: "type:i64", kind: "integer", name: "i64" },
        { id: "type:string", kind: "string", name: "String" },
        { id: "type:reply", kind: "opaque", name: "ReplyToken" },
        { id: "type:ChannelI64", kind: "opaque", name: "Channel<i64>" },
        { id: "type:TaskI64", kind: "opaque", name: "Task<i64>" },
        { id: "type:Vec", kind: "vector", name: "Vec<i64>" },
        { id: "type:Worker", kind: "actor", name: "Worker" },
        { id: "type:Sup", kind: "supervisor", name: "Sup" }
      ],
      records: [],
      enums: [],
      actors,
      supervisors,
      machines: []
    },
    stdlib_symbols: [{ id: "sym:core.stdout.println", module: "core.stdout", name: "println", params: ["type:any"], result: "type:unit", capability: "core.stdout", admission: "allowed" }],
    capabilities: [{ id: "core.stdout", disposition: "allowed", reason: "page stdout shim", required_by: ["sym:core.stdout.println"] }],
    functions: [
      ...extraFunctions,
      {
        id: "fn:main",
        module: "mod:main",
        name: "main",
        params: [],
        result: "type:unit",
        locals: mainLocals,
        blocks: [{ id: "block:main.entry", params: [], instructions: mainInstructions, terminator: { op: "return", args: [], span: null }, span: null }],
        span: null
      }
    ]
  };
}

function explodeFunction() {
  return fn("fn:worker.explode", "Worker.explode", [local("explode.reply", "type:reply"), local("explode.msg", "type:string")], [
    instr("const.string", "local:explode.msg", [lit("boom")]),
    instr("panic", null, [loc("explode.msg")])
  ], [], "type:unit", ["local:explode.reply"]);
}

function replyThenExplodeFunction() {
  return fn("fn:worker.reply_then_explode", "Worker.reply_then_explode", [local("explode.reply", "type:reply"), local("explode.ok", "type:string"), local("explode.msg", "type:string")], [
    instr("const.string", "local:explode.ok", [lit("ok")]),
    instr("actor.reply", null, [loc("explode.reply"), loc("explode.ok")]),
    instr("const.string", "local:explode.msg", [lit("boom")]),
    instr("panic", null, [loc("explode.msg")])
  ], [], "type:unit", ["local:explode.reply"]);
}

function crashHookFunction(decision) {
  return fn("fn:worker.crash", "Worker.on_crash", [local("crash.failure", "type:string"), local("crash.text", "type:string"), local("crash.decision", "type:string")], [
    instr("const.string", "local:crash.text", [lit("crash hook")]),
    instr("call.stdlib", null, [sym("sym:core.stdout.println"), loc("crash.text")]),
    instr("const.string", "local:crash.decision", [lit(decision)])
  ], [loc("crash.decision")], "type:string", ["local:crash.failure"]);
}

function supervisorDeniedHookFunction() {
  return fn("fn:worker.bad_hook", "Worker.bad_hook", [local("crash.failure", "type:string"), local("crash.sup", "type:Sup")], [
    instr("supervisor.spawn", "local:crash.sup", [type("type:Sup")])
  ], [], "type:unit", ["local:crash.failure"]);
}

function hookPanicFunction() {
  return fn("fn:worker.hook_panic", "Worker.hook_panic", [local("crash.failure", "type:string"), local("crash.msg", "type:string")], [
    instr("const.string", "local:crash.msg", [lit("hook panic")]),
    instr("panic", null, [loc("crash.msg")])
  ], [], "type:unit", ["local:crash.failure"]);
}

function linkExitFunction() {
  return signalPrinter("fn:observer.link_exit", "Observer.link_exit", "local:signal", "type:sandbox.ExitSignal");
}

function monitorDownFunction() {
  return signalPrinter("fn:observer.monitor_down", "Observer.monitor_down", "local:signal", "type:sandbox.MonitorDown", 1);
}

function signalPrinter(id, name, signalLocal, signalType, actorField = 0) {
  return fn(id, name, [local("observer.reply", "type:reply"), { id: signalLocal, name: null, type: signalType, mutable: false, span: null }, local("observer.actor", "type:string")], [
    instr("record.get", "local:observer.actor", [{ kind: "local", value: signalLocal }, lit(actorField)]),
    instr("call.stdlib", null, [sym("sym:core.stdout.println"), loc("observer.actor")])
  ], [], "type:unit", ["local:observer.reply", signalLocal]);
}

function growFunction() {
  return fn("fn:worker.grow", "Worker.grow", [local("grow.reply", "type:reply"), local("grow.state", "type:Vec"), local("grow.items", "type:Vec"), local("grow.one", "type:i64"), local("grow.two", "type:i64")], [
    instr("vector.new", "local:grow.items", [type("type:i64")]),
    instr("const.i64", "local:grow.one", [lit(1)]),
    instr("const.i64", "local:grow.two", [lit(2)]),
    instr("vector.push", null, [loc("grow.items"), loc("grow.one")]),
    instr("vector.push", null, [loc("grow.items"), loc("grow.two")])
  ], [loc("grow.items")], "type:Vec", ["local:grow.reply", "local:grow.state"]);
}

function childTaskFunction() {
  return fn("fn:child_task", "child_task", [local("child.result", "type:i64")], [
    instr("const.i64", "local:child.result", [lit(42)])
  ], [loc("child.result")], "type:i64", []);
}

function parentTaskFunction() {
  return fn("fn:parent_task", "parent_task", [local("parent.task", "type:TaskI64"), local("parent.out", "type:i64")], [
    instr("task.spawn", "local:parent.task", [fnop("fn:child_task")]),
    instr("task.await", "local:parent.out", [loc("parent.task")])
  ], [loc("parent.out")], "type:i64", []);
}

function fn(id, name, locals, instructions, returnArgs, result = "type:unit", params = []) {
  return {
    id,
    module: "mod:main",
    name,
    params,
    result,
    locals,
    blocks: [{ id: `block:${id.replace("fn:", "")}.entry`, params: [], instructions, terminator: { op: "return", args: returnArgs, span: null }, span: null }],
    span: null
  };
}

function directScheduler() {
  const bytecode = basePackage("direct-scheduler", [], [], [], [], []);
  const trace = new TraceBuilder(bytecode, "direct-scheduler", "trace:direct-scheduler", replay(611), "0.0.0-spec");
  const scheduler = new ActorScheduler(611, "round_robin", trace, {
    invoke(functionId) {
      if (functionId === "fn:explode") {
        throw new ActorTurnCrash(runtimeFailure("panic", "boom", "panic", null));
      }
      return UNIT;
    },
    consumeStep() {
      trace.stepCount += 1;
    }
  });
  const boom = scheduler.spawn({ id: "type:Boom", name: "Boom", state_fields: [], handlers: [{ name: "explode", function: "fn:explode" }] }, [], "actor:root", null);
  const observer = scheduler.spawn({ id: "type:Observer", name: "Observer", state_fields: [], handlers: [] }, [], "actor:root", null);
  return { scheduler, trace, boom, observer };
}

function instr(op, dst = null, args = [], metadata = undefined) {
  return metadata === undefined ? { op, dst, args, span: null } : { op, dst, args, span: null, metadata };
}

function local(name, type) {
  const id = name.startsWith("local:") ? name : `local:${name}`;
  return { id, name: null, type, mutable: false, span: null };
}

function loc(name) {
  return { kind: "local", value: name.startsWith("local:") ? name : `local:${name}` };
}

function lit(value) {
  return { kind: "literal", value };
}

function sym(value) {
  return { kind: "symbol", value };
}

function fnop(value) {
  return { kind: "function", value };
}

function type(value) {
  return { kind: "type", value };
}

function replay(seed) {
  return { seed, step_budget: 1000, virtual_clock: { epoch_ms: 0, tick_ms: 1, current_ms: 0 }, inputs: [] };
}

function runOptions(fixtureId, seed) {
  return { fixtureId, traceId: `trace:${fixtureId}`, replay: replay(seed), sandboxVersion: "0.0.0-spec" };
}

function readJson(relativePath) {
  return JSON.parse(fs.readFileSync(path.join(root, relativePath), "utf8"));
}

function traceFamilies(trace) {
  return trace.events.map((event) => event.message ?? event.type);
}

function count(values, value) {
  return values.filter((candidate) => candidate === value).length;
}

function indexOfMessage(trace, message, textIncludes = "") {
  const index = trace.events.findIndex((event) => event.message === message && (!textIncludes || event.text?.includes(textIncludes)));
  assert.notEqual(index, -1, `${message} not found`);
  return index;
}

function eventText(trace, message) {
  const event = trace.events.find((candidate) => candidate.message === message);
  assert.ok(event, `${message} not found`);
  return event.text ?? "";
}
