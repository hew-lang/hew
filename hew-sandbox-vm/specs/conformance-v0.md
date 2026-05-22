# Sandbox VM conformance contract v0

**Status:** M0 substrate contract. This document defines the educational
sandbox VM's observable behavior before any interpreter exists.

The sandbox VM is "correct enough for education" when a learner can paste the
same source into the sandbox repeatedly and get the same diagnostics, output,
trace records, and final state snapshot for the same replay seed and inputs.
It is not correct if unsupported runtime behavior silently succeeds, if host
nondeterminism leaks into the trace, or if diagnostics disagree with the
canonical Hew frontend.

## Scope

This contract covers:

- trace schema v0 and final state snapshots
- stdout and stderr records
- compile diagnostic records
- sandbox-profile rejection records
- runtime failure records
- deterministic replay inputs
- step budget semantics
- seed semantics
- virtual clock records
- actor, channel, task, supervisor, and machine ID spaces
- source span mapping
- accepted simulation divergences
- non-negotiable parity expectations

This contract does not define the bytecode interpreter, stdlib shims, actor
scheduler, mailbox semantics, channel semantics, task runtime, supervisor
strategy execution, machine execution, or frontend reuse implementation.

## Trace Schema v0

The normative machine-readable shape is
[`trace-schema-v0.schema.json`](trace-schema-v0.schema.json).

Every trace records:

1. the trace schema version, fixture or run identifier, profile name, Hew
   version, and sandbox scaffold version
2. deterministic replay configuration: seed, step budget, virtual clock, and
   host inputs admitted into the sandbox
3. an ordered event stream
4. a final state snapshot

Event sequence numbers are zero-based and strictly increasing. Consumers may
ignore event types they do not understand, but producers must not emit fields
outside the schema. Schema versioning is for forward evolution only; v0 does
not promise compatibility with any pre-v0 sandbox shape.

## Correctness for education

A sandbox run is conforming when all of the following hold:

- **Deterministic replay:** same Hew source, same profile, same bytecode package,
  same seed, same step budget, same virtual clock, and same replay inputs produce
  byte-for-byte identical normalized traces.
- **Frontend parity:** parse, resolve, type, and capability diagnostics match the
  canonical frontend for severity, primary source span, diagnostic code, and
  message intent.
- **Observable value parity:** pure sequential language behavior, stdout text,
  stderr text, panic messages, trap kinds, record fields, enum tags, vector
  contents, string values, and regex match results match canonical Hew behavior
  after the relevant frontend and stdlib substrate lands.
- **Fail closed:** any capability, stdlib call, profile feature, or bytecode
  instruction that lacks a sandbox-safe implementation emits a structured
  rejection or runtime failure record. It must never no-op or fabricate success.
- **Trace completeness:** learners can explain how the final state follows from
  the event stream without relying on host-specific state.

## Stdout and stderr records

`io.stdout` and `io.stderr` events contain normalized UTF-8 text. Newline bytes
are preserved. The sandbox may coalesce adjacent writes from the same logical
operation, but it must not reorder writes across source order or actor/task
delivery order as defined by the deterministic scheduler once that scheduler
exists.

The final state also stores stdout and stderr as ordered arrays of text chunks.
The concatenation of those arrays is the learner-visible output.

## Compile diagnostics

`compile.diagnostic` events use canonical frontend diagnostics. Diagnostic
records include:

- `phase`: `lex`, `parse`, `resolve`, `type`, `lower`, `profile`, or `bytecode`
- `severity`: `error`, `warning`, `info`, or `help`
- stable diagnostic `code`
- user-facing `message`
- primary source span or `null` when no source location exists
- notes and suggestions

A compile-error trace has result `compile_error`, step count `0`, no sandbox
runtime side effects, and a final state containing the emitted diagnostics.

## Sandbox-profile rejections

`sandbox.rejected` events describe code that is valid Hew but not admitted by
the selected educational sandbox profile. Capability identifiers are open-ended
strings in v0 because the sandbox profile has not been implemented yet.

Examples include filesystem I/O, network I/O, wall-clock access, native process
execution, unbounded host memory allocation, and stdlib modules whose sandbox
shim has not been ratified. Rejection is a success of the sandbox safety model:
the trace result is `sandbox_rejected`, not an internal failure.

## Runtime failures

`runtime.failure` events cover behavior that passes compilation and profile
admission but cannot complete normally. The v0 failure classes are:

| Kind | Meaning |
| --- | --- |
| `panic` | Hew `panic` or assertion-style failure with a source span. |
| `trap` | Defined VM trap such as divide by zero, integer overflow, invalid enum tag, invalid local, or missing capability. |
| `budget_exhausted` | The step budget reached zero before normal completion. |
| `unsupported` | A reserved bytecode or runtime surface was encountered before implementation. |
| `internal_error` | Sandbox bug; must include enough detail to reproduce but not host secrets. |

Runtime failures have deterministic final snapshots. They must include stdout
and stderr already emitted before the failure.

## Replay inputs

The replay record is the only path for external input. The allowed input kinds
are `stdin`, `argv`, `env`, `random_bytes`, `clock_advance`,
`capability_response`, and `user_event`.

Host APIs must not read ambient state directly. A future browser UI may collect
input interactively, but it must persist that input into replay records before
the sandbox run consumes it.

## Step budget semantics

The step budget is a deterministic fuel counter. A conforming interpreter must
decrement it for every committed bytecode instruction and terminator. Later
scheduler work may define additional budget charges for actor mailbox delivery,
task wakeups, channel selection, supervisor decisions, and machine transitions,
but those charges must be deterministic and documented before execution lands.

When the budget reaches zero before normal completion:

1. execution stops at the next budget check
2. the trace emits a `budget.exhausted` event
3. the result is `budget_exhausted`
4. the final state reports the exact committed step count and remaining budget

Compilation and profile rejection do not consume runtime steps.

## Seed semantics

The seed is a 64-bit unsigned integer represented as a JSON integer inside the
safe integer range for v0 fixtures. It initializes all sandbox pseudo-random
choices, including future scheduler tie-breakers. The seed does not affect
frontend diagnostics, profile admission, or pure deterministic code.

Random bytes requested by a future sandbox-safe API must be emitted as replay
inputs so reruns do not depend on a host PRNG.

## Virtual clock semantics

The sandbox has no wall clock. The replay record defines:

- `epoch_ms`: logical start time in milliseconds
- `tick_ms`: minimum logical clock quantum
- `current_ms`: the current logical time for the trace snapshot

`clock.virtual_advance` events are the only way logical time moves. Future
sleep and timer operations may advance the virtual clock or suspend work until a
recorded logical time, but they must not observe browser or operating-system
time directly.

## ID spaces

IDs are deterministic strings. The reserved prefixes are:

| Prefix | Space |
| --- | --- |
| `actor:` | actor instances, including `actor:root` for the entry task's actor context |
| `channel:` | channel handles |
| `task:` | structured tasks and futures |
| `supervisor:` | supervisor instances |
| `machine:` | state machine instances |

IDs are allocated in deterministic creation order within each prefix. Reuse is
forbidden within a trace, even after an actor, task, channel, supervisor, or
machine terminates. The final state lists every allocated ID, not only live IDs,
so learners can correlate events with the complete run history.

## Source span mapping

Source spans use UTF-8 byte offsets plus one-based line and column numbers.
Offsets are measured against the exact source text admitted to the sandbox. The
end position is exclusive. Generated bytecode without a specific source origin
uses `null` spans and must include a message that explains why no source span is
available.

Bytecode source maps must preserve the path that the learner sees in the
browser UI. Host-local absolute paths must not appear in committed fixtures or
browser-produced traces.

## Accepted simulation divergences

All divergences are rejected unless listed here:

| Divergence | Accepted behavior |
| --- | --- |
| Host wall clock | Replaced by the virtual clock. |
| Host filesystem, network, process, and native environment | Rejected by the sandbox profile unless a future safe capability shim records deterministic replay input. |
| OS threads and true parallelism | Replaced by a deterministic educational scheduler once actor execution exists. |
| Resource limits | The sandbox may use smaller memory, recursion, and step limits than native execution when the limit is explicit in the trace. |
| Educational trace detail | The sandbox may emit additional explanatory events that native execution does not emit. |
| Bytecode package shape | Sandbox bytecode is an educational IR and does not need to match native backend internals. |

No divergence is accepted for stdout/stderr text, compile diagnostic severity
and primary span, pure arithmetic results, checked arithmetic failures, record
field values, enum tags, vector element order, string contents, regex answers,
or panic/trap kind once those surfaces are admitted by the sandbox profile.

## Golden fixture obligations

The M0 fixture catalog must contain at least 20 fixtures before interpreter work
starts. Each fixture has:

- a Hew source file
- an expected trace JSON file that validates against Trace Schema v0
- a manifest entry naming its feature coverage and status

Fixtures may be marked `planned` while no interpreter exists, but their traces
remain contract examples for future implementation. See
[`golden-fixtures-v0.md`](golden-fixtures-v0.md) and
[`../fixtures/manifest.json`](../fixtures/manifest.json).
