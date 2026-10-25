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
    `${label} trace event sequence changed`,
  );
  assert.deepEqual(
    actual.final_state.stdout,
    expected.final_state.stdout,
    `${label} stdout changed`,
  );
  assert.deepEqual(
    actual.final_state.exit_code,
    expected.final_state.exit_code,
    `${label} exit code changed`,
  );
  const diagnostics = (trace) =>
    trace.final_state.diagnostics.map(({ phase, severity, code, span }) => ({
      phase,
      severity,
      code,
      span,
    }));
  assert.deepEqual(
    diagnostics(actual),
    diagnostics(expected),
    `${label} diagnostics changed`,
  );
  const failures = (trace) =>
    trace.final_state.runtime_failures.map(
      ({ hew_fault, kind, message, span, trap_kind }) => ({
        hew_fault,
        kind,
        span,
        trap_kind,
        ...(hew_fault?.kind === "UserPanic" ? { message } : {}),
      }),
    );
  assert.deepEqual(
    failures(actual),
    failures(expected),
    `${label} runtime failures changed`,
  );
  const rejections = (trace) =>
    trace.final_state.sandbox_rejections.map(
      ({ capability, category, code, span }) => ({
        capability,
        category,
        code,
        span,
      }),
    );
  assert.deepEqual(
    rejections(actual),
    rejections(expected),
    `${label} sandbox rejections changed`,
  );
}
