import assert from "node:assert/strict";
import test from "node:test";
import { runBytecode } from "../dist/interpreter/index.js";
import { admitPackage } from "../dist/interpreter/v1/validate.js";

function call(operation, args, result, to) {
  return {
    op: "actor.call",
    operation,
    args,
    result,
    result_shape: null,
    error_shape: null,
    normal: { to, args: [] },
    unwind: null,
    span: null,
  };
}

function print(value, to) {
  return {
    op: "runtime.call",
    family: 0,
    args: [{ value, decision: "borrow" }],
    result: null,
    result_shape: null,
    normal: { to, args: [] },
    unwind: null,
    span: null,
  };
}

function block(id, ops, term) {
  return { id, params: [], ops, term };
}

function outputBody(id, name, stringIndex) {
  return {
    id,
    name,
    params: [{ value: 0, own: "guaranteed" }],
    entry: 0,
    places: [],
    blocks: [
      block(
        0,
        [{ op: "const.str", dst: 1, str: stringIndex, span: null }],
        print(1, 1),
      ),
      block(1, [], { op: "return", value: null, span: null }),
    ],
  };
}

function lifecyclePackage(request) {
  return {
    schema_version: "hew.sandbox.bytecode.v1",
    hew_version: "0.7.0",
    compiler_version: "lifecycle-test",
    profile: "sandbox-vm-export",
    entry: { function: 0, exit: "unit" },
    strings: ["after", "hook", "release"],
    bytes: [],
    regex_patterns: [],
    aggregates: [{ id: 0, name: "Resource", fields: [] }],
    variants: [],
    resources: [{ kind: "record", ty: "Resource", shape: 0, close: 2 }],
    runtime_families: [
      { id: 0, family: "Print", detail: { kind: "Str", newline: true } },
    ],
    externs: [],
    suspend_kinds: [],
    value_capabilities: [],
    closures: [],
    vtables: [],
    actors: [
      {
        id: 0,
        name: "Worker",
        state_fields: [{ mutable: false, deferred: false }],
        stop: [1],
        handlers: [],
        overflow: "block",
      },
    ],
    supervisors: [],
    functions: [
      {
        id: 0,
        name: "main",
        params: [],
        entry: 0,
        places: [],
        blocks: [
          block(
            0,
            [
              {
                op: "aggregate.make",
                dst: 0,
                shape: 0,
                fields: [],
                span: null,
              },
            ],
            call(
              { op: "spawn", actor: 0 },
              [{ value: 0, decision: "move" }],
              { value: 1, own: "none" },
              1,
            ),
          ),
          block(
            1,
            [],
            call(
              { op: request, actor: 0 },
              [{ value: 1, decision: "borrow" }],
              null,
              2,
            ),
          ),
          block(
            2,
            [{ op: "const.str", dst: 2, str: 0, span: null }],
            print(2, 3),
          ),
          block(
            3,
            [],
            call(
              { op: "await_stopped", actor: 0 },
              [{ value: 1, decision: "borrow" }],
              null,
              4,
            ),
          ),
          block(4, [], { op: "return", value: null, span: null }),
        ],
      },
      outputBody(1, "stop hook", 1),
      outputBody(2, "resource close", 2),
    ],
  };
}

async function run(bytecode) {
  return await runBytecode(bytecode, {
    fixtureId: "lifecycle",
    traceId: "trace:lifecycle",
    replay: {
      seed: 0,
      step_budget: 1000,
      virtual_clock: { epoch_ms: 0, tick_ms: 1, current_ms: 0 },
      inputs: [],
    },
  });
}

test("stop returns before the hook and releases state after it", async () => {
  const bytecode = lifecyclePackage("stop");
  assert.equal(admitPackage(bytecode), null);
  const trace = await run(bytecode);
  assert.equal(
    trace.result,
    "ok",
    JSON.stringify(trace.final_state.runtime_failures),
  );
  assert.equal(trace.final_state.stdout.join(""), "after\nhook\nrelease\n");
});

test("terminate skips the stop hook and still releases owned state", async () => {
  const bytecode = lifecyclePackage("terminate");
  assert.equal(admitPackage(bytecode), null);
  const trace = await run(bytecode);
  assert.equal(
    trace.result,
    "ok",
    JSON.stringify(trace.final_state.runtime_failures),
  );
  assert.equal(trace.final_state.stdout.join(""), "after\nrelease\n");
});

test("a crash hook requires its exact crash kind shape", () => {
  const bytecode = lifecyclePackage("stop");
  Object.assign(bytecode.actors[0], {
    crash: 1,
    crash_info: 0,
    crash_action: 0,
  });
  bytecode.variants = [
    { id: 0, name: "CrashAction", cases: [], runtime_tags: {} },
  ];
  assert.equal(
    admitPackage(bytecode)?.code,
    "sandbox.package.crash_hook_shapes",
  );
});

test("only an authored close may omit a safepoint cancellation edge", () => {
  const bytecode = lifecyclePackage("stop");
  bytecode.suspend_kinds = ["Safepoint"];
  const poll = {
    op: "suspend",
    kind: "Safepoint",
    detail: { budget: 4000 },
    inputs: [],
    result: null,
    resumes: [{ to: 1, args: [] }],
    cancel: null,
    unwind: null,
    span: null,
  };
  bytecode.functions[2].blocks[0].term = poll;
  assert.equal(admitPackage(bytecode), null);
  bytecode.functions[0].blocks[0].term = poll;
  assert.equal(
    admitPackage(bytecode)?.code,
    "sandbox.package.cancellation_ancestry",
  );
});

test("a timer package cannot request waiting admission", () => {
  const bytecode = lifecyclePackage("stop");
  bytecode.actors[0].handlers = [
    {
      name: "tick",
      message_id: 1,
      callable: 1,
      params: 0,
      streams: false,
      fallible: false,
      result_shape: null,
    },
  ];
  bytecode.resources.push({ kind: "opaque", ty: "Timer", close: 2 });
  const operation = {
    op: "timer_schedule",
    actor: 0,
    message: 1,
    resource: 1,
    policy: "wait",
  };
  bytecode.functions[0].blocks[1].term = call(
    operation,
    Array.from({ length: 5 }, () => ({ value: 1, decision: "move" })),
    { value: 2, own: "owned" },
    2,
  );
  assert.equal(admitPackage(bytecode)?.code, "sandbox.package.invalid_timer");
  operation.policy = "drop_newest";
  assert.equal(admitPackage(bytecode), null);
});


test("periodic descriptors require a positive interval and zero-argument body", () => {
  const bytecode = lifecyclePackage("stop");
  const handler = {
    name: "tick", message_id: 1, callable: 1, params: 0,
    streams: false, fallible: false, result_shape: null, every_ns: 1_000_000,
  };
  bytecode.actors[0].handlers.push(handler);
  assert.equal(admitPackage(bytecode), null);
  for (const invalid of [0, -1, 999_999, 1_000_000.5]) {
    handler.every_ns = invalid;
    assert.equal(admitPackage(bytecode)?.code, "sandbox.package.invalid_periodic");
  }
  handler.every_ns = 1_000_000;
  handler.params = 1;
  assert.equal(admitPackage(bytecode)?.code, "sandbox.package.invalid_periodic");
});
