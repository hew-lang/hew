# Sandbox execution and trace contract

The sandbox executes ordinary Hew through verified ownership SIR. Source
behaviour follows native Hew: actor calls, task scopes, ownership transfer,
cleanup and fault propagation are language semantics, including when execution
suspends. A capability refusal is not execution parity.

The supported simulation differences are deterministic scheduling, virtual
time, a page-provided stdin buffer, and explicit refusal of native host I/O.
Missing operations must fail admission before executing any program instruction.
They must not silently succeed or be replaced with AST interpretation.

## Replay and traces

The [trace schema](trace-schema-v0.schema.json) records output, faults,
admission refusals, scheduling decisions, virtual-clock progress and final
status. Identical bytecode and replay inputs produce identical traces.
Scheduler decisions recorded in a trace can replay a seeded chaos run using
the ordinary scheduling policy. A new recording preserves host inputs and
regenerates scheduler decisions for the new instruction stream.

Each SIR operation and terminator consumes a step. A finite step budget stops
unbounded execution and reports `budget_exhausted`. Program panics, checked
traps, compiler diagnostics, package refusals and VM defects remain distinct.

`runProgram` exposes `status`, `stdout`, `exit_code`, `diagnostics`,
`compiler_version`, `hew_version` and `sandbox_rejections`. Refusal entries
have categories `native_only`, `not_implemented` or `invalid_package`.

## Evidence

`make sandbox-parity` executes source through native Hew and the public browser
compiler/VM, and runs VM contract tests. `make sandbox-fixtures-record`
regenerates source-compiled packages and traces; `make sandbox-fixtures-check`
checks package provenance. Goldens must be reviewed against the source and its
expected outcome before accepting changes.
