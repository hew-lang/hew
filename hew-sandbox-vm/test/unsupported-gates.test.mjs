import assert from "node:assert/strict";
import fs from "node:fs";
import path from "node:path";
import test from "node:test";
import { fileURLToPath } from "node:url";
import { runBytecode } from "../dist/interpreter/index.js";

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");

// ── Actor/supervisor end-to-end gate ─────────────────────────────────────────

test("actor spawn → ask → reply → state update end-to-end (15-actor-counter fixture)", () => {
  const bytecode = readJson("fixtures/15-actor-counter/bytecode.json");
  const expected = readJson("fixtures/15-actor-counter/expected.trace.json");

  const trace = runBytecode(bytecode, {
    fixtureId: expected.fixture_id,
    traceId: expected.trace_id,
    replay: expected.replay,
    sandboxVersion: expected.sandbox_version
  });

  // The counter actor increments state on each ask.
  assert.equal(trace.result, "ok");
  assert.deepEqual(trace.final_state.runtime_failures, []);
  // actor.ask and actor.reply must both appear in the trace.
  const families = trace.events.map((e) => e.message ?? e.type);
  assert.ok(families.includes("actor.ask"), "actor.ask event expected");
  assert.ok(families.includes("actor.reply"), "actor.reply event expected");
  // A spawned actor is allocated beyond actor:root.
  assert.ok(trace.final_state.ids.actors.length >= 2, "at least one spawned actor expected");
  // stdout matches the known golden.
  assert.deepEqual(trace.final_state.stdout, expected.final_state.stdout);
});

test("actor.send (tell) does not trap when no reply token is present", () => {
  // Construct a minimal actor that is sent a message without a reply token.
  // The handler has a trailing expression; actor.reply with a unit token must
  // silently drop the value rather than trapping (reply semantics conditional).
  const bytecode = {
    schema_version: "hew.sandbox.bytecode.v0",
    package_id: "pkg:tell-no-trap",
    hew_version: "0.5.0-pre",
    compiler_version: "test",
    profile: "sandbox.educational.v0",
    source_map: { sources: [], spans: [] },
    module_graph: {
      entry: "mod:main",
      modules: [{ id: "mod:main", path: "main.hew", source_id: "src:main", imports: [], functions: ["fn:actor.Greeter.greet", "fn:main"] }]
    },
    layouts: {
      types: [
        { id: "type:unit", kind: "unit", name: "()" },
        { id: "type:i64", kind: "integer", name: "i64" },
        { id: "type:string", kind: "string", name: "String" },
        { id: "type:reply", kind: "opaque", name: "ReplyToken" },
        { id: "type:Greeter", kind: "actor", name: "Greeter" }
      ],
      records: [],
      enums: [],
      actors: [
        {
          id: "type:Greeter",
          name: "Greeter",
          state_fields: [],
          handlers: [{ name: "greet", function: "fn:actor.Greeter.greet" }]
        }
      ],
      supervisors: [],
      machines: []
    },
    stdlib_symbols: [],
    capabilities: [],
    functions: [
      {
        // Handler: receives reply token (unit for tell) + no state; trailing expr = const.i64 42.
        // The emitter inserts actor.reply(reply_token, 42); for tell, token is unit → no-op.
        id: "fn:actor.Greeter.greet",
        module: "mod:main",
        name: "Greeter.greet",
        params: ["local:greet.reply"],
        result: "type:i64",
        locals: [
          { id: "local:greet.reply", name: "reply", type: "type:reply", mutable: false, span: null },
          { id: "local:greet.val", name: "val", type: "type:i64", mutable: false, span: null }
        ],
        blocks: [
          {
            id: "block:greet.entry",
            params: [],
            instructions: [
              { op: "const.i64", dst: "local:greet.val", args: [{ kind: "literal", value: 42 }], span: null },
              { op: "actor.reply", dst: null, args: [{ kind: "local", value: "local:greet.reply" }, { kind: "local", value: "local:greet.val" }], span: null }
            ],
            terminator: { op: "return", args: [{ kind: "local", value: "local:greet.val" }], span: null },
            span: null
          }
        ],
        span: null
      },
      {
        id: "fn:main",
        module: "mod:main",
        name: "main",
        params: [],
        result: "type:unit",
        locals: [
          { id: "local:main.actor", name: "actor", type: "type:Greeter", mutable: false, span: null },
          { id: "local:main.unit", name: null, type: "type:unit", mutable: false, span: null }
        ],
        blocks: [
          {
            id: "block:main.entry",
            params: [],
            instructions: [
              { op: "actor.spawn", dst: "local:main.actor", args: [{ kind: "type", value: "type:Greeter" }], span: null },
              // actor.send = tell; no reply token → handler receives unit as reply arg.
              { op: "actor.send", dst: null, args: [{ kind: "local", value: "local:main.actor" }, { kind: "symbol", value: "greet" }], span: null },
              { op: "const.unit", dst: "local:main.unit", args: [], span: null }
            ],
            terminator: { op: "return", args: [{ kind: "local", value: "local:main.unit" }], span: null },
            span: null
          }
        ],
        span: null
      }
    ]
  };

  const trace = runBytecode(bytecode, {
    fixtureId: "tell-no-trap",
    traceId: "trace:tell-no-trap",
    replay: replay()
  });

  // The tell should complete without trapping.
  assert.equal(trace.result, "ok", `expected ok but got ${trace.result}: ${JSON.stringify(trace.final_state.runtime_failures)}`);
  assert.deepEqual(trace.final_state.runtime_failures, []);
});

test("supervisor spawn → child resolution → crash/restart (30-child-crash fixture)", () => {
  const bytecode = readJson("fixtures/30-child-crash-supervisor-restart-decision/bytecode.json");

  const trace = runBytecode(bytecode, {
    fixtureId: "child-crash-supervisor-restart-decision",
    traceId: "trace:child-crash-supervisor-restart-decision",
    replay: { seed: 42, step_budget: 5000, virtual_clock: { epoch_ms: 0, tick_ms: 1, current_ms: 0 }, inputs: [] }
  });

  // The fixture crashes a child actor and the supervisor decides to restart it.
  assert.equal(trace.result, "ok");
  assert.deepEqual(trace.final_state.runtime_failures, []);
  const families = trace.events.map((e) => e.message ?? e.type);
  assert.ok(families.some((f) => f === "child-crashed" || f === "actor.crash"), "child crash event expected");
  assert.ok(families.some((f) => f === "restart-decision" || f === "child-spawned"), "supervisor restart/spawn event expected");
  // supervisor handle must be in the trace ids.
  assert.ok(trace.final_state.ids.supervisors.length >= 1, "at least one supervisor expected");
});

// ── Actor reply semantics (Q401) ─────────────────────────────────────────────

test("early-return ask handler delivers the early value to the caller (Q401 trampoline-fallback)", () => {
  // Mirrors the native probe: receive fn compute(n: i64) -> i64 { if n < 0 { return 0 } return n*2 }
  // The handler is encoded as two blocks:
  //   entry: if n < 0 goto early-return else goto normal-return
  //   early-return: return 0 (WITHOUT calling actor.reply — tests the trampoline fallback)
  //   normal-return: actor.reply(token, n*2); return n*2
  // Before the fix, ask(-5) hangs (unresolved slot); after the fix it delivers 0.
  const bytecode = earlyReturnActorPackage();

  const trace = runBytecode(bytecode, {
    fixtureId: "early-return-ask",
    traceId: "trace:early-return-ask",
    replay: replay(801)
  });

  assert.equal(trace.result, "ok", `expected ok; failures: ${JSON.stringify(trace.final_state.runtime_failures)}`);
  assert.deepEqual(trace.final_state.runtime_failures, []);
  // ask(-5) → early return path → 0; ask(10) → normal path → 20
  assert.deepEqual(trace.final_state.stdout, ["0\n", "20\n"]);
});

test("normal-exit ask handler delivers exactly one reply (no double-reply regression)", () => {
  // A handler that calls actor.reply explicitly then returns — the fallback must NOT
  // double-reply because slot.resolved is already true when invoke() returns.
  const trace = runBytecode(normalExitActorPackage(), {
    fixtureId: "normal-exit-ask",
    traceId: "trace:normal-exit-ask",
    replay: replay(802)
  });

  assert.equal(trace.result, "ok", `expected ok; failures: ${JSON.stringify(trace.final_state.runtime_failures)}`);
  assert.deepEqual(trace.final_state.runtime_failures, []);
  // actor.reply delivered 7; no double-reply trap
  assert.deepEqual(trace.final_state.stdout, ["7\n"]);
  // Exactly one actor.reply event in the trace — no duplicate
  const replyEvents = trace.events.filter((e) => e.message === "actor.reply");
  assert.equal(replyEvents.length, 1, `expected exactly 1 actor.reply event, got ${replyEvents.length}`);
});

test("stateful early-return: reply value is correct and state is preserved (Q401 emitter fix)", () => {
  // Encodes the reviewer's reproduction case:
  //   actor Counter { let count: i64;
  //     receive fn bump(n: i64) -> i64 { return n; }   // early return, no field write
  //     receive fn get()        -> i64 { return count; }
  //   }
  //   fn main() { let c = spawn Counter(count: 100); println(bump(5)); println(get()); }
  //
  // Expected (native-correct): ["5\n", "100\n"]
  // Before the emitter fix: ["5\n", "5\n"] — count silently corrupted to 5
  //
  // The fixed `bump` handler bytecode:
  //   actor.reply(token, n)   — reply with n
  //   ret([count])            — return state local (not n)
  // This matches the normal-exit emitter path (emit_handler:671-698).
  const trace = runBytecode(statefulEarlyReturnCounterPackage(), {
    fixtureId: "stateful-early-return-counter",
    traceId:   "trace:stateful-early-return-counter",
    replay:    replay(803)
  });

  assert.equal(trace.result, "ok", `expected ok; failures: ${JSON.stringify(trace.final_state.runtime_failures)}`);
  assert.deepEqual(trace.final_state.runtime_failures, []);
  // bump(5) replies 5; count stays 100; get() returns 100.
  assert.deepEqual(
    trace.final_state.stdout, ["5\n", "100\n"],
    "bump must reply 5 AND leave count=100 (state independent of reply value)"
  );
});

test("stateful early-return: pre-return field mutation survives the early return (Q401 multi-field)", () => {
  // Encodes:
  //   actor Pair { let a: i64; let b: i64;
  //     receive fn set_a_return_b(x: i64) -> i64 { a = x; return b; }
  //     receive fn get_a()                -> i64 { return a; }
  //   }
  //   fn main() { let p = spawn Pair(a: 1, b: 99);
  //     println(set_a_return_b(42));  // replies 99; sets a=42
  //     println(get_a());             // must return 42
  //   }
  //
  // Before the fix, `updateStateFromReturn` receives a scalar (b=99) for a
  // two-field actor — neither branch in updateStateFromReturn fires — and
  // silently leaves state stale (a stays 1, mutation lost).
  //
  // The fixed `set_a_return_b` handler:
  //   local.set a ← x           (mutation)
  //   actor.reply(token, b)      (reply = b)
  //   record.new [a, b] → state  (state record with mutated a)
  //   ret([state])
  const trace = runBytecode(statefulEarlyReturnPairPackage(), {
    fixtureId: "stateful-early-return-pair",
    traceId:   "trace:stateful-early-return-pair",
    replay:    replay(804)
  });

  assert.equal(trace.result, "ok", `expected ok; failures: ${JSON.stringify(trace.final_state.runtime_failures)}`);
  assert.deepEqual(trace.final_state.runtime_failures, []);
  // set_a_return_b(42) → replies 99 (b); get_a() → must return 42 (mutated a).
  assert.deepEqual(
    trace.final_state.stdout, ["99\n", "42\n"],
    "set_a_return_b must reply b=99 AND persist a=42 (pre-return mutation survives)"
  );
});

// ── Machine operations ────────────────────────────────────────────────────────

test("machine operations execute the declared transition table", () => {
  const bytecode = packageWithInstruction("machine.new", []);
  bytecode.layouts.machines = [
    {
      id: "type:Light",
      name: "Light",
      states: ["Off", "On"],
      events: ["Toggle"],
      transitions: [
        { event: "Toggle", from: "Off", to: "On" },
        { event: "Toggle", from: "On", to: "Off" }
      ]
    }
  ];
  bytecode.functions[0].locals = [
    { id: "local:main.0", name: "light", type: "type:Light", mutable: true, span: null },
    { id: "local:main.1", name: null, type: "type:string", mutable: false, span: null }
  ];
  bytecode.functions[0].blocks[0].instructions = [
    { op: "machine.new", dst: "local:main.0", args: [{ kind: "type", value: "type:Light" }, { kind: "symbol", value: "Off" }], span: null },
    { op: "machine.step", dst: "local:main.0", args: [{ kind: "local", value: "local:main.0" }, { kind: "symbol", value: "Toggle" }], span: null },
    { op: "machine.state", dst: "local:main.1", args: [{ kind: "local", value: "local:main.0" }], span: null }
  ];

  const trace = runBytecode(bytecode, {
    fixtureId: "machine-step",
    traceId: "trace:machine-step",
    replay: replay()
  });

  assert.equal(trace.final_state.status, "ok");
  assert.deepEqual(trace.final_state.runtime_failures, []);
});

test("M7 file and network stream operations fail closed", () => {
  for (const op of ["stream.file.open", "stream.net.connect"]) {
    const trace = runBytecode(packageWithInstruction(op, []), {
      fixtureId: `unsupported-${op.replaceAll(".", "-")}`,
      traceId: `trace:unsupported-${op.replaceAll(".", "-")}`,
      replay: replay()
    });

    assert.equal(trace.result, "runtime_failure");
    assert.equal(trace.final_state.runtime_failures[0].kind, "unsupported");
    assert.equal(trace.final_state.runtime_failures[0].message, `Unsupported::NATIVE_ONLY: ${op}`);
    assert.equal(trace.final_state.runtime_failures[0].unsupported.kind, "Unsupported::NATIVE_ONLY");
  }
});

test("M7 host I/O stdlib symbols fail closed", () => {
  const bytecode = packageWithInstruction("call.stdlib", [{ kind: "symbol", value: "sym:std.fs.read" }]);
  bytecode.stdlib_symbols = [
    {
      id: "sym:std.fs.read",
      module: "std.fs",
      name: "read",
      params: [],
      result: "type:string",
      capability: "std.fs.read",
      admission: "rejected"
    }
  ];
  bytecode.capabilities = [
    { id: "std.fs.read", disposition: "rejected", reason: "host filesystem unavailable", required_by: ["sym:std.fs.read"] }
  ];

  const trace = runBytecode(bytecode, {
    fixtureId: "unsupported-host-io",
    traceId: "trace:unsupported-host-io",
    replay: replay()
  });

  assert.equal(trace.result, "runtime_failure");
  assert.equal(trace.events[1].type, "runtime.failure");
  assert.equal(trace.final_state.runtime_failures[0].kind, "unsupported");
  assert.match(trace.final_state.runtime_failures[0].message, /Unsupported::NATIVE_ONLY: std\.fs\.read/);
  assert.equal(trace.final_state.runtime_failures[0].unsupported.status, "unsupported_native_only");
});

function replay(seed = 999) {
  return { seed, step_budget: 1000, virtual_clock: { epoch_ms: 0, tick_ms: 1, current_ms: 0 }, inputs: [] };
}

// ── Q401: early-return actor package ─────────────────────────────────────────
//
// Encodes:
//   actor Calc { receive fn compute(n: i64) -> i64 { if n < 0 { return 0 } return n * 2 } }
//   fn main() { let c = spawn Calc; println(await c.compute(-5)); println(await c.compute(10)); }
//
// The compute handler is split into three blocks:
//   block:compute.entry  — compare n < 0, br_if to early / normal
//   block:compute.early  — return 0 WITHOUT calling actor.reply (tests trampoline fallback)
//   block:compute.normal — actor.reply(token, n*2); return n*2
//
// main sends both asks sequentially via actor.ask instructions.
function earlyReturnActorPackage() {
  return {
    schema_version: "hew.sandbox.bytecode.v0",
    package_id: "pkg:early-return-ask",
    hew_version: "0.5.0-pre",
    compiler_version: "test",
    profile: "sandbox.educational.v0",
    source_map: { sources: [], spans: [] },
    module_graph: {
      entry: "mod:main",
      modules: [{ id: "mod:main", path: "main.hew", source_id: "src:main", imports: [], functions: ["fn:calc.compute", "fn:main"] }]
    },
    layouts: {
      types: [
        { id: "type:unit", kind: "unit", name: "()" },
        { id: "type:i64", kind: "integer", name: "i64" },
        { id: "type:string", kind: "string", name: "String" },
        { id: "type:bool", kind: "boolean", name: "bool" },
        { id: "type:reply", kind: "opaque", name: "ReplyToken" },
        { id: "type:Calc", kind: "actor", name: "Calc" }
      ],
      records: [],
      enums: [],
      actors: [{ id: "type:Calc", name: "Calc", state_fields: [], handlers: [{ name: "compute", function: "fn:calc.compute" }] }],
      supervisors: [],
      machines: []
    },
    stdlib_symbols: [{ id: "sym:core.stdout.println", module: "core.stdout", name: "println", params: ["type:any"], result: "type:unit", capability: "core.stdout", admission: "allowed" }],
    capabilities: [{ id: "core.stdout", disposition: "allowed", reason: "page stdout shim", required_by: ["sym:core.stdout.println"] }],
    functions: [
      {
        // receive fn compute(reply: ReplyToken, n: i64) -> i64
        // Block entry: compare n < 0; br_if to early-return, else to normal-return
        // Block early-return: return 0 (no actor.reply — fallback must fire)
        // Block normal-return: compute n*2; actor.reply(token, n*2); return n*2
        id: "fn:calc.compute",
        module: "mod:main",
        name: "Calc.compute",
        params: ["local:compute.token", "local:compute.n"],
        result: "type:i64",
        locals: [
          { id: "local:compute.token", name: "token", type: "type:reply", mutable: false, span: null },
          { id: "local:compute.n", name: "n", type: "type:i64", mutable: false, span: null },
          { id: "local:compute.zero", name: "zero", type: "type:i64", mutable: false, span: null },
          { id: "local:compute.cond", name: "cond", type: "type:bool", mutable: false, span: null },
          { id: "local:compute.two", name: "two", type: "type:i64", mutable: false, span: null },
          { id: "local:compute.result", name: "result", type: "type:i64", mutable: false, span: null }
        ],
        blocks: [
          {
            id: "block:compute.entry",
            params: [],
            instructions: [
              { op: "const.i64", dst: "local:compute.zero", args: [{ kind: "literal", value: 0 }], span: null },
              // cond = n < 0
              { op: "cmp.lt", dst: "local:compute.cond", args: [{ kind: "local", value: "local:compute.n" }, { kind: "local", value: "local:compute.zero" }], span: null }
            ],
            terminator: {
              op: "br_if",
              condition: { kind: "local", value: "local:compute.cond" },
              target: "block:compute.early",
              else_target: "block:compute.normal",
              span: null
            },
            span: null
          },
          {
            // Early return path: return 0 without calling actor.reply.
            // The trampoline fallback in stepActor() must deliver 0 to the waiting ask.
            id: "block:compute.early",
            params: [],
            instructions: [
              { op: "const.i64", dst: "local:compute.zero", args: [{ kind: "literal", value: 0 }], span: null }
            ],
            terminator: { op: "return", args: [{ kind: "local", value: "local:compute.zero" }], span: null },
            span: null
          },
          {
            // Normal return path: compute n*2; actor.reply; return.
            id: "block:compute.normal",
            params: [],
            instructions: [
              { op: "const.i64", dst: "local:compute.two", args: [{ kind: "literal", value: 2 }], span: null },
              { op: "i64.checked_mul", dst: "local:compute.result", args: [{ kind: "local", value: "local:compute.n" }, { kind: "local", value: "local:compute.two" }], span: null },
              { op: "actor.reply", dst: null, args: [{ kind: "local", value: "local:compute.token" }, { kind: "local", value: "local:compute.result" }], span: null }
            ],
            terminator: { op: "return", args: [{ kind: "local", value: "local:compute.result" }], span: null },
            span: null
          }
        ],
        span: null
      },
      {
        // fn main: spawn Calc; ask(-5) → print; ask(10) → print
        id: "fn:main",
        module: "mod:main",
        name: "main",
        params: [],
        result: "type:unit",
        locals: [
          { id: "local:main.calc", name: "calc", type: "type:Calc", mutable: false, span: null },
          { id: "local:main.neg5", name: "neg5", type: "type:i64", mutable: false, span: null },
          { id: "local:main.ten", name: "ten", type: "type:i64", mutable: false, span: null },
          { id: "local:main.r1", name: "r1", type: "type:i64", mutable: false, span: null },
          { id: "local:main.r2", name: "r2", type: "type:i64", mutable: false, span: null }
        ],
        blocks: [
          {
            id: "block:main.entry",
            params: [],
            instructions: [
              { op: "actor.spawn", dst: "local:main.calc", args: [{ kind: "type", value: "type:Calc" }], span: null },
              { op: "const.i64", dst: "local:main.neg5", args: [{ kind: "literal", value: -5 }], span: null },
              { op: "const.i64", dst: "local:main.ten", args: [{ kind: "literal", value: 10 }], span: null },
              // ask(-5) → early return path → 0
              { op: "actor.ask", dst: "local:main.r1", args: [{ kind: "local", value: "local:main.calc" }, { kind: "symbol", value: "compute" }, { kind: "local", value: "local:main.neg5" }], span: null },
              { op: "call.stdlib", dst: null, args: [{ kind: "symbol", value: "sym:core.stdout.println" }, { kind: "local", value: "local:main.r1" }], span: null },
              // ask(10) → normal path → 20
              { op: "actor.ask", dst: "local:main.r2", args: [{ kind: "local", value: "local:main.calc" }, { kind: "symbol", value: "compute" }, { kind: "local", value: "local:main.ten" }], span: null },
              { op: "call.stdlib", dst: null, args: [{ kind: "symbol", value: "sym:core.stdout.println" }, { kind: "local", value: "local:main.r2" }], span: null }
            ],
            terminator: { op: "return", args: [], span: null },
            span: null
          }
        ],
        span: null
      }
    ]
  };
}

// A simple actor handler that explicitly calls actor.reply then returns.
// Used to verify no double-reply occurs when the trampoline fallback is present.
// Handler: actor.reply(token, 7); return 7
function normalExitActorPackage() {
  return {
    schema_version: "hew.sandbox.bytecode.v0",
    package_id: "pkg:normal-exit-ask",
    hew_version: "0.5.0-pre",
    compiler_version: "test",
    profile: "sandbox.educational.v0",
    source_map: { sources: [], spans: [] },
    module_graph: {
      entry: "mod:main",
      modules: [{ id: "mod:main", path: "main.hew", source_id: "src:main", imports: [], functions: ["fn:counter.get", "fn:main"] }]
    },
    layouts: {
      types: [
        { id: "type:unit", kind: "unit", name: "()" },
        { id: "type:i64", kind: "integer", name: "i64" },
        { id: "type:string", kind: "string", name: "String" },
        { id: "type:reply", kind: "opaque", name: "ReplyToken" },
        { id: "type:Counter", kind: "actor", name: "Counter" }
      ],
      records: [],
      enums: [],
      actors: [{ id: "type:Counter", name: "Counter", state_fields: [], handlers: [{ name: "get", function: "fn:counter.get" }] }],
      supervisors: [],
      machines: []
    },
    stdlib_symbols: [{ id: "sym:core.stdout.println", module: "core.stdout", name: "println", params: ["type:any"], result: "type:unit", capability: "core.stdout", admission: "allowed" }],
    capabilities: [{ id: "core.stdout", disposition: "allowed", reason: "page stdout shim", required_by: ["sym:core.stdout.println"] }],
    functions: [
      {
        // Handler: actor.reply(token, 7); return 7  — reply resolved BEFORE invoke() returns
        id: "fn:counter.get",
        module: "mod:main",
        name: "Counter.get",
        params: ["local:get.token"],
        result: "type:i64",
        locals: [
          { id: "local:get.token", name: "token", type: "type:reply", mutable: false, span: null },
          { id: "local:get.val", name: "val", type: "type:i64", mutable: false, span: null }
        ],
        blocks: [{
          id: "block:get.entry",
          params: [],
          instructions: [
            { op: "const.i64", dst: "local:get.val", args: [{ kind: "literal", value: 7 }], span: null },
            { op: "actor.reply", dst: null, args: [{ kind: "local", value: "local:get.token" }, { kind: "local", value: "local:get.val" }], span: null }
          ],
          terminator: { op: "return", args: [{ kind: "local", value: "local:get.val" }], span: null },
          span: null
        }],
        span: null
      },
      {
        id: "fn:main",
        module: "mod:main",
        name: "main",
        params: [],
        result: "type:unit",
        locals: [
          { id: "local:main.c", name: "c", type: "type:Counter", mutable: false, span: null },
          { id: "local:main.r", name: "r", type: "type:i64", mutable: false, span: null }
        ],
        blocks: [{
          id: "block:main.entry",
          params: [],
          instructions: [
            { op: "actor.spawn", dst: "local:main.c", args: [{ kind: "type", value: "type:Counter" }], span: null },
            { op: "actor.ask", dst: "local:main.r", args: [{ kind: "local", value: "local:main.c" }, { kind: "symbol", value: "get" }], span: null },
            { op: "call.stdlib", dst: null, args: [{ kind: "symbol", value: "sym:core.stdout.println" }, { kind: "local", value: "local:main.r" }], span: null }
          ],
          terminator: { op: "return", args: [], span: null },
          span: null
        }],
        span: null
      }
    ]
  };
}

// ── Q401: stateful early-return packages ─────────────────────────────────────
//
// These packages encode the corrected emitter output for stateful actors.
// On the early-return path the fixed emitter emits:
//   1. actor.reply(token, expr)   — reply with the return expression
//   2. ret([state local])         — return state so updateStateFromReturn
//                                   preserves the actor's field(s)
//
// This mirrors the normal-exit path (emit_handler:671-698) and ensures
// reply value and next-actor-state are independent, matching native.

// Encodes:
//   actor Counter { let count: i64;
//     receive fn bump(n: i64) -> i64 { return n; }
//     receive fn get()        -> i64 { return count; }
//   }
//   fn main() { let c = spawn Counter(count: 100); println(bump(5)); println(get()); }
//
// Fixed bump handler (params: [token, count, n]):
//   actor.reply(token, n)
//   ret([count])   ← state, NOT n
function statefulEarlyReturnCounterPackage() {
  return {
    schema_version: "hew.sandbox.bytecode.v0",
    package_id: "pkg:stateful-early-return-counter",
    hew_version: "0.5.0-pre",
    compiler_version: "test",
    profile: "sandbox.educational.v0",
    source_map: { sources: [], spans: [] },
    module_graph: {
      entry: "mod:main",
      modules: [{
        id: "mod:main", path: "main.hew", source_id: "src:main", imports: [],
        functions: ["fn:counter.bump", "fn:counter.get", "fn:main"]
      }]
    },
    layouts: {
      types: [
        { id: "type:unit",    kind: "unit",    name: "()" },
        { id: "type:i64",     kind: "integer", name: "i64" },
        { id: "type:string",  kind: "string",  name: "String" },
        { id: "type:reply",   kind: "opaque",  name: "ReplyToken" },
        { id: "type:Counter", kind: "actor",   name: "Counter" }
      ],
      records: [],
      enums: [],
      actors: [{
        id: "type:Counter",
        name: "Counter",
        state_fields: [{ name: "count", type: "type:i64" }],
        handlers: [
          { name: "bump", function: "fn:counter.bump" },
          { name: "get",  function: "fn:counter.get"  }
        ]
      }],
      supervisors: [],
      machines: []
    },
    stdlib_symbols: [{
      id: "sym:core.stdout.println",
      module: "core.stdout",
      name: "println",
      params: ["type:any"],
      result: "type:unit",
      capability: "core.stdout",
      admission: "allowed"
    }],
    capabilities: [{
      id: "core.stdout",
      disposition: "allowed",
      reason: "page stdout shim",
      required_by: ["sym:core.stdout.println"]
    }],
    functions: [
      {
        // receive fn bump(token: Reply, count: i64, n: i64) -> i64
        // Fixed emitter output:
        //   actor.reply(token, n)   — deliver reply
        //   ret([count])            — return state (count unchanged)
        id: "fn:counter.bump",
        module: "mod:main",
        name: "Counter.bump",
        params: ["local:bump.token", "local:bump.count", "local:bump.n"],
        result: "type:i64",
        locals: [
          { id: "local:bump.token", name: "token", type: "type:reply", mutable: false, span: null },
          { id: "local:bump.count", name: "count", type: "type:i64",   mutable: true,  span: null },
          { id: "local:bump.n",     name: "n",     type: "type:i64",   mutable: false, span: null }
        ],
        blocks: [{
          id: "block:bump.entry",
          params: [],
          instructions: [
            // actor.reply(token, n) — reply value = n
            { op: "actor.reply", dst: null,
              args: [{ kind: "local", value: "local:bump.token" }, { kind: "local", value: "local:bump.n" }],
              span: null }
          ],
          // ret([count]) — next state = count (unchanged; NOT n)
          terminator: { op: "return", args: [{ kind: "local", value: "local:bump.count" }], span: null },
          span: null
        }],
        span: null
      },
      {
        // receive fn get(token: Reply, count: i64) -> i64
        // Normal-exit path: actor.reply(token, count); ret([count])
        id: "fn:counter.get",
        module: "mod:main",
        name: "Counter.get",
        params: ["local:get.token", "local:get.count"],
        result: "type:i64",
        locals: [
          { id: "local:get.token", name: "token", type: "type:reply", mutable: false, span: null },
          { id: "local:get.count", name: "count", type: "type:i64",   mutable: true,  span: null }
        ],
        blocks: [{
          id: "block:get.entry",
          params: [],
          instructions: [
            { op: "actor.reply", dst: null,
              args: [{ kind: "local", value: "local:get.token" }, { kind: "local", value: "local:get.count" }],
              span: null }
          ],
          terminator: { op: "return", args: [{ kind: "local", value: "local:get.count" }], span: null },
          span: null
        }],
        span: null
      },
      {
        // fn main: spawn Counter(count: 100); bump(5) → print; get() → print
        id: "fn:main",
        module: "mod:main",
        name: "main",
        params: [],
        result: "type:unit",
        locals: [
          { id: "local:main.c",   name: "c",   type: "type:Counter", mutable: false, span: null },
          { id: "local:main.100", name: null,   type: "type:i64",     mutable: false, span: null },
          { id: "local:main.5",   name: null,   type: "type:i64",     mutable: false, span: null },
          { id: "local:main.r1",  name: "r1",  type: "type:i64",     mutable: false, span: null },
          { id: "local:main.r2",  name: "r2",  type: "type:i64",     mutable: false, span: null }
        ],
        blocks: [{
          id: "block:main.entry",
          params: [],
          instructions: [
            // spawn Counter(count: 100)
            { op: "const.i64",   dst: "local:main.100", args: [{ kind: "literal", value: 100 }], span: null },
            { op: "actor.spawn", dst: "local:main.c",
              args: [{ kind: "type", value: "type:Counter" }, { kind: "local", value: "local:main.100" }],
              span: null },
            // bump(5) → r1
            { op: "const.i64",  dst: "local:main.5",  args: [{ kind: "literal", value: 5 }], span: null },
            { op: "actor.ask",  dst: "local:main.r1",
              args: [{ kind: "local", value: "local:main.c" }, { kind: "symbol", value: "bump" }, { kind: "local", value: "local:main.5" }],
              span: null },
            { op: "call.stdlib", dst: null,
              args: [{ kind: "symbol", value: "sym:core.stdout.println" }, { kind: "local", value: "local:main.r1" }],
              span: null },
            // get() → r2
            { op: "actor.ask",  dst: "local:main.r2",
              args: [{ kind: "local", value: "local:main.c" }, { kind: "symbol", value: "get" }],
              span: null },
            { op: "call.stdlib", dst: null,
              args: [{ kind: "symbol", value: "sym:core.stdout.println" }, { kind: "local", value: "local:main.r2" }],
              span: null }
          ],
          terminator: { op: "return", args: [], span: null },
          span: null
        }],
        span: null
      }
    ]
  };
}

// Encodes:
//   actor Pair { let a: i64; let b: i64;
//     receive fn set_a_return_b(x: i64) -> i64 { a = x; return b; }
//     receive fn get_a()                -> i64 { return a; }
//   }
//   fn main() { let p = spawn Pair(a: 1, b: 99);
//     println(set_a_return_b(42));  // replies 99; sets a=42
//     println(get_a());             // must return 42
//   }
//
// Fixed set_a_return_b handler (params: [token, a, b, x]):
//   local.set a ← x                — mutate a
//   actor.reply(token, b)          — reply with b
//   record.new [a, b] → state_rec  — pack state record
//   ret([state_rec])               — return updated state
function statefulEarlyReturnPairPackage() {
  return {
    schema_version: "hew.sandbox.bytecode.v0",
    package_id: "pkg:stateful-early-return-pair",
    hew_version: "0.5.0-pre",
    compiler_version: "test",
    profile: "sandbox.educational.v0",
    source_map: { sources: [], spans: [] },
    module_graph: {
      entry: "mod:main",
      modules: [{
        id: "mod:main", path: "main.hew", source_id: "src:main", imports: [],
        functions: ["fn:pair.set_a_return_b", "fn:pair.get_a", "fn:main"]
      }]
    },
    layouts: {
      types: [
        { id: "type:unit",     kind: "unit",    name: "()" },
        { id: "type:i64",      kind: "integer", name: "i64" },
        { id: "type:string",   kind: "string",  name: "String" },
        { id: "type:reply",    kind: "opaque",  name: "ReplyToken" },
        { id: "type:Pair",     kind: "actor",   name: "Pair" },
        { id: "type:Pair.state", kind: "record", name: "Pair.State", parameters: [] }
      ],
      records: [{ id: "type:Pair.state", name: "Pair.State",
        fields: [{ name: "a", type: "type:i64" }, { name: "b", type: "type:i64" }] }],
      enums: [],
      actors: [{
        id: "type:Pair",
        name: "Pair",
        state_fields: [{ name: "a", type: "type:i64" }, { name: "b", type: "type:i64" }],
        handlers: [
          { name: "set_a_return_b", function: "fn:pair.set_a_return_b" },
          { name: "get_a",          function: "fn:pair.get_a"           }
        ]
      }],
      supervisors: [],
      machines: []
    },
    stdlib_symbols: [{
      id: "sym:core.stdout.println",
      module: "core.stdout",
      name: "println",
      params: ["type:any"],
      result: "type:unit",
      capability: "core.stdout",
      admission: "allowed"
    }],
    capabilities: [{
      id: "core.stdout",
      disposition: "allowed",
      reason: "page stdout shim",
      required_by: ["sym:core.stdout.println"]
    }],
    functions: [
      {
        // receive fn set_a_return_b(token, a, b, x) -> i64
        //   local.set a ← x
        //   actor.reply(token, b)
        //   record.new [a, b] → state_rec
        //   ret([state_rec])
        id: "fn:pair.set_a_return_b",
        module: "mod:main",
        name: "Pair.set_a_return_b",
        params: ["local:sab.token", "local:sab.a", "local:sab.b", "local:sab.x"],
        result: "type:i64",
        locals: [
          { id: "local:sab.token",     name: "token",     type: "type:reply",      mutable: false, span: null },
          { id: "local:sab.a",         name: "a",         type: "type:i64",        mutable: true,  span: null },
          { id: "local:sab.b",         name: "b",         type: "type:i64",        mutable: true,  span: null },
          { id: "local:sab.x",         name: "x",         type: "type:i64",        mutable: false, span: null },
          { id: "local:sab.state_rec", name: "state_rec", type: "type:Pair.state", mutable: false, span: null }
        ],
        blocks: [{
          id: "block:sab.entry",
          params: [],
          instructions: [
            // a = x
            { op: "local.set", dst: null,
              args: [{ kind: "local", value: "local:sab.a" }, { kind: "local", value: "local:sab.x" }],
              span: null },
            // actor.reply(token, b)
            { op: "actor.reply", dst: null,
              args: [{ kind: "local", value: "local:sab.token" }, { kind: "local", value: "local:sab.b" }],
              span: null },
            // state_rec = record.new Pair.State { a, b }  (with mutated a)
            { op: "record.new", dst: "local:sab.state_rec",
              args: [
                { kind: "type",  value: "type:Pair.state" },
                { kind: "local", value: "local:sab.a" },
                { kind: "local", value: "local:sab.b" }
              ],
              span: null }
          ],
          terminator: { op: "return", args: [{ kind: "local", value: "local:sab.state_rec" }], span: null },
          span: null
        }],
        span: null
      },
      {
        // receive fn get_a(token, a, b) -> i64
        //   actor.reply(token, a); ret([record.new [a, b]])
        id: "fn:pair.get_a",
        module: "mod:main",
        name: "Pair.get_a",
        params: ["local:ga.token", "local:ga.a", "local:ga.b"],
        result: "type:i64",
        locals: [
          { id: "local:ga.token",     name: "token",     type: "type:reply",      mutable: false, span: null },
          { id: "local:ga.a",         name: "a",         type: "type:i64",        mutable: true,  span: null },
          { id: "local:ga.b",         name: "b",         type: "type:i64",        mutable: true,  span: null },
          { id: "local:ga.state_rec", name: "state_rec", type: "type:Pair.state", mutable: false, span: null }
        ],
        blocks: [{
          id: "block:ga.entry",
          params: [],
          instructions: [
            { op: "actor.reply", dst: null,
              args: [{ kind: "local", value: "local:ga.token" }, { kind: "local", value: "local:ga.a" }],
              span: null },
            { op: "record.new", dst: "local:ga.state_rec",
              args: [
                { kind: "type",  value: "type:Pair.state" },
                { kind: "local", value: "local:ga.a" },
                { kind: "local", value: "local:ga.b" }
              ],
              span: null }
          ],
          terminator: { op: "return", args: [{ kind: "local", value: "local:ga.state_rec" }], span: null },
          span: null
        }],
        span: null
      },
      {
        // fn main: spawn Pair(a: 1, b: 99); set_a_return_b(42) → print; get_a() → print
        id: "fn:main",
        module: "mod:main",
        name: "main",
        params: [],
        result: "type:unit",
        locals: [
          { id: "local:main.p",  name: "p",  type: "type:Pair", mutable: false, span: null },
          { id: "local:main.1",  name: null,  type: "type:i64",  mutable: false, span: null },
          { id: "local:main.99", name: null,  type: "type:i64",  mutable: false, span: null },
          { id: "local:main.42", name: null,  type: "type:i64",  mutable: false, span: null },
          { id: "local:main.r1", name: "r1", type: "type:i64",  mutable: false, span: null },
          { id: "local:main.r2", name: "r2", type: "type:i64",  mutable: false, span: null }
        ],
        blocks: [{
          id: "block:main.entry",
          params: [],
          instructions: [
            { op: "const.i64",   dst: "local:main.1",  args: [{ kind: "literal", value: 1  }], span: null },
            { op: "const.i64",   dst: "local:main.99", args: [{ kind: "literal", value: 99 }], span: null },
            // spawn Pair(a: 1, b: 99)
            { op: "actor.spawn", dst: "local:main.p",
              args: [{ kind: "type", value: "type:Pair" },
                     { kind: "local", value: "local:main.1"  },
                     { kind: "local", value: "local:main.99" }],
              span: null },
            // set_a_return_b(42) → r1
            { op: "const.i64",  dst: "local:main.42", args: [{ kind: "literal", value: 42 }], span: null },
            { op: "actor.ask",  dst: "local:main.r1",
              args: [{ kind: "local", value: "local:main.p" },
                     { kind: "symbol", value: "set_a_return_b" },
                     { kind: "local", value: "local:main.42" }],
              span: null },
            { op: "call.stdlib", dst: null,
              args: [{ kind: "symbol", value: "sym:core.stdout.println" }, { kind: "local", value: "local:main.r1" }],
              span: null },
            // get_a() → r2
            { op: "actor.ask",  dst: "local:main.r2",
              args: [{ kind: "local", value: "local:main.p" }, { kind: "symbol", value: "get_a" }],
              span: null },
            { op: "call.stdlib", dst: null,
              args: [{ kind: "symbol", value: "sym:core.stdout.println" }, { kind: "local", value: "local:main.r2" }],
              span: null }
          ],
          terminator: { op: "return", args: [], span: null },
          span: null
        }],
        span: null
      }
    ]
  };
}

function readJson(relativePath) {
  return JSON.parse(fs.readFileSync(path.join(root, relativePath), "utf8"));
}

function packageWithInstruction(op, args) {
  return {
    schema_version: "hew.sandbox.bytecode.v0",
    package_id: `pkg:${op.replaceAll(".", "-")}`,
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
        { id: "type:actor", kind: "actor", name: "Actor" }
      ],
      records: [],
      enums: [],
      actors: [],
      supervisors: [],
      machines: []
    },
    stdlib_symbols: [],
    capabilities: [],
    functions: [
      {
        id: "fn:main",
        module: "mod:main",
        name: "main",
        params: [],
        result: "type:unit",
        locals: [{ id: "local:main.0", name: null, type: "type:unit", mutable: false, span: null }],
        blocks: [
          {
            id: "block:main.entry",
            params: [],
            instructions: [{ op, dst: "local:main.0", args, span: null }],
            terminator: { op: "return", args: [], span: null },
            span: null
          }
        ],
        span: null
      }
    ]
  };
}
