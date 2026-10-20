import assert from "node:assert/strict";

export function assertFixtureTrace(actual, expected, label) {
  // Compare observable behaviour only: event sequence, stdout, exit and
  // diagnostics. Internal VM bookkeeping (budget_remaining, step_count,
  // virtual_clock, ids, globals) is not part of the sandbox's observable
  // contract, so it is deliberately excluded here rather than pinned via
  // a whole-trace deepEqual.
  assert.deepEqual(
    actual.events.map((event) => event.type),
    expected.events.map((event) => event.type),
    `${label} trace event sequence changed`
  );
  assert.deepEqual(actual.final_state.stdout, expected.final_state.stdout, `${label} stdout changed`);
  assert.deepEqual(actual.final_state.exit_code, expected.final_state.exit_code, `${label} exit code changed`);
  assert.deepEqual(actual.final_state.diagnostics, expected.final_state.diagnostics, `${label} diagnostics changed`);
  assert.deepEqual(
    actual.final_state.runtime_failures,
    expected.final_state.runtime_failures,
    `${label} runtime failures changed`
  );
  assert.deepEqual(
    actual.final_state.sandbox_rejections,
    expected.final_state.sandbox_rejections,
    `${label} sandbox rejections changed`
  );
}
