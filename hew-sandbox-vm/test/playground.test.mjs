import assert from "node:assert/strict";
import test from "node:test";
import { buildPlaygroundState, decodePlaygroundShare, encodePlaygroundShare, validateLessonManifest, PlaygroundIntegrationError } from "../dist/interpreter/index.js";

test("playground JSON contract projects deterministic trace views and controls", () => {
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

test("share links are inert data and lesson virtual files validate fail-closed", () => {
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


function replay(seed, currentMs) { return { seed, step_budget: 1000, virtual_clock: { epoch_ms: 0, tick_ms: 1, current_ms: currentMs }, inputs: [] }; }
function fakeTrace() {
  const diagnostic = { phase: "type", severity: "error", code: "E_TEST", message: "test diagnostic", span: null, notes: [], suggestions: [] };
  return {
    schema_version: "hew.sandbox.trace.v0",
    trace_id: "trace:playground-contract",
    fixture_id: "playground-contract",
    profile: "sandbox.educational.v0",
    hew_version: "0.6.0-rc1",
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
