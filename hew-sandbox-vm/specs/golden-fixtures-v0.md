# Golden fixture catalog v0

**Status:** M0 contract catalog. These fixtures are specification fixtures, not
interpreter tests yet. They pin the source programs and expected trace shapes
that M2+ and M4+ must eventually satisfy.

Every fixture listed here appears in [`../fixtures/manifest.json`](../fixtures/manifest.json)
and has:

- `main.hew`: learner-facing Hew source
- `expected.trace.json`: Trace Schema v0 output contract
- `status: "planned"` until the sandbox interpreter exists

## Coverage requirements

The initial catalog intentionally covers more than the minimum 20 fixtures so
later milestones can wire execution incrementally without redefining M0:

| Fixture | Category | Required coverage |
| --- | --- | --- |
| `hello-world` | sequential | stdout record, final state snapshot |
| `arithmetic-checked` | sequential | integer arithmetic, checked arithmetic expectation |
| `branch-loop` | sequential | branch and loop step accounting |
| `record-fields` | values | record layout and field projection |
| `enum-match` | values | enum tag and match result |
| `vector-basics` | values | vector create, push, get, length |
| `string-interpolation` | values | string formatting and stdout |
| `regex-match` | stdlib | regex symbol reference and deterministic match result |
| `compile-type-error` | diagnostics | compile diagnostic record and source span |
| `sandbox-reject-filesystem` | sandbox profile | fail-closed capability rejection |
| `runtime-panic` | runtime failure | panic record |
| `divide-by-zero-trap` | runtime failure | trap kind record |
| `step-budget-exhausted` | runtime failure | budget exhaustion record |
| `virtual-sleep-clock` | replay | virtual clock advance record |
| `actor-counter` | concurrency | actor ID allocation and message result |
| `actor-ask-reply` | concurrency | ask/reply shape without scheduler semantics |
| `channel-select` | concurrency | channel ID and select shape |
| `task-await` | concurrency | task ID and await result |
| `supervisor-restart` | concurrency | supervisor ID and restart trace shape |
| `machine-traffic-light` | state machine | machine ID and state transition output |
| `replay-stdin` | replay | deterministic stdin input |
| `deterministic-random` | replay | seed and random byte replay input |

## Fixture status rules

`planned` means the fixture is part of the contract but is not executable by a
sandbox VM yet. Future implementation may add statuses such as `implemented`
or `blocked`, but it must not remove a v0 fixture without replacing its coverage
with an equivalent fixture and updating this catalog.

When a future interpreter lands, the expected traces may gain more educational
detail, but they must preserve the externally meaningful result, output,
diagnostics, rejection/failure kind, replay inputs, and final snapshot fields
defined here.
