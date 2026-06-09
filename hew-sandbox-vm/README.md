# Hew Sandbox VM

`hew-sandbox-vm` is the specification and scaffolding home for Hew's future
educational browser sandbox bytecode VM. The sandbox name is intentional: this
tree defines a deterministic teaching runtime contract, not a production browser
runtime and not a replacement for `hew-runtime`.

The first layer is substrate-only. It defines the trace contract, fixture
catalog, bytecode package shape, opcode registry, and validation scaffolding
that later milestones can consume when frontend reuse, sandbox-safe stdlib
shims, and interpreter work are ready.

## Status

| Area | Status | Notes |
| --- | --- | --- |
| M0 conformance contract | Defined | [`specs/conformance-v0.md`](specs/conformance-v0.md) defines "correct enough for education" and fail-closed trace records. |
| M0 trace schema v0 | Defined | [`specs/trace-schema-v0.schema.json`](specs/trace-schema-v0.schema.json) covers deterministic traces, diagnostics, replay inputs, IDs, and final snapshots. |
| M0 golden fixtures | Seeded | [`fixtures/manifest.json`](fixtures/manifest.json) lists 22 planned contract fixtures with expected traces. |
| M1 bytecode package | Defined | [`bytecode/package-v0.md`](bytecode/package-v0.md) and [`bytecode/sandbox-bytecode-v0.schema.json`](bytecode/sandbox-bytecode-v0.schema.json) define encoding and metadata only. |
| M1 opcode families | Defined | [`bytecode/opcodes-v0.md`](bytecode/opcodes-v0.md) registers opcode names and capability metadata; concurrent semantics are explicitly deferred. |
| M2 frontend reuse | Started | `hew-sandbox-wasm` owns the browser `sandbox-vm-export` tier and emits bytecode packages after parse, type-check, and profile admission. |
| M3+ stdlib shims | Deferred | Unsupported capabilities must reject with structured diagnostics. |
| M4+ interpreter/scheduler | Deferred | No bytecode execution logic belongs in this dispatch. |

## Tree

```text
hew-sandbox-vm/
  bytecode/      Sandbox Bytecode IR package and opcode specs.
  fixtures/      Golden Hew source programs and expected trace contracts.
  scaffolding/   Validation scripts and package metadata.
  specs/         Conformance, trace schema, and fixture catalog docs.
```

## Validation

The scaffold uses a private Node package with `ajv` to validate JSON Schemas and
fixture traces. From this directory:

```sh
npm install
npm run build
npm test
npm run validate
```

The validation harness checks:

- JSON Schema compilation for the trace and bytecode package schemas
- every golden fixture listed in `fixtures/manifest.json`
- fixture count lower bound for the M0 contract
- Markdown links inside this tree

It does not execute bytecode; Rust tests in `hew-sandbox-wasm` cover the
compile/profile/emit path for the current sandbox VM export subset.

## Non-goals for this phase

- No stdlib shim implementation is added.
- No interpreter, scheduler, mailbox, channel, task, supervisor, or machine
  execution semantics are implemented.
- No compatibility layer is promised for pre-v0 sandbox formats.
