# Hew Observe protocol

`v0.5/openapi.json` is the producer-owned, authoritative wire contract for the
Hew profiler/Observe HTTP surface. It describes the existing HTTP/1.1 JSON,
text, and binary endpoints rather than inventing a second transport.

## Why OpenAPI 3.1

The deployed protocol is HTTP with JSON envelopes. OpenAPI 3.1 embeds JSON
Schema, documents every route/header/media type, and preserves that transport.
Protobuf would require a new binary endpoint and serializer before it could be
authoritative. CBOR plus CDDL would have the same migration cost and materially
weaker Swift and TypeScript generation. Neither binary choice fixes drift in
the current JSON clients.

The contract marks all full-width integer fields and the generator maps them to
Rust `u64`/`i64`, Swift `UInt64`/`Int64`, and TypeScript `bigint`. TypeScript's
generated decoder tokenizes integer literals before `JSON.parse`, so values are
never rounded through IEEE-754 `number` first. Small bounded integers remain
TypeScript `number` after a safe-integer check.

The TypeScript decoders also accept decimal strings for full-width integer
fields. That is the required projection when a Rust/Tauri command relays these
models through JSON IPC: serialize each `u64`/`i64` as a decimal string, then
decode it to `bigint` in TypeScript. Do not pass a full-width value through a
JavaScript `number` or ordinary `JSON.parse` first.

## Compatibility policy

- A v0.5 producer emits every property listed in a model's `required` array.
- `actor_type` and `handler_name` keys are required and nullable. `null` means
  attribution was unavailable; an omitted key is malformed v0.5.
- Unknown object properties and unknown `event_type`, `state`, and `trap_kind`
  strings are accepted. Those taxonomies are intentionally not JSON enums.
- Consumers reject malformed envelopes and any `schema_version` other than
  exactly `v0.5`.
- Transport failures, endpoint failures, reconnection, and last-good-view state
  remain client service/view-model responsibilities. They are not wire fields.
- `/api/traces` is a destructive, process-global drain (maximum 256 events per
  request from a 16,384-event queue). v0.5 has no source/destination attribution
  and no per-consumer cursor. Those require a future versioned contract.

## Generated outputs

- Rust crate: `hew-observe-protocol/src/generated.rs`
- Swift: `v0.5/generated/ObserveProtocolV05.swift`
- TypeScript: `v0.5/generated/observe-protocol-v0.5.ts`

Python 3.12 (the repository minimum) is the pinned generator runtime. Run:

```sh
make observe-protocol-generate
make observe-protocol-check
```

CI runs the check target so edits to the OpenAPI document cannot land with
stale generated bindings. Hew owns the schema and Rust producer/reference-client
integration. Native Observe apps should copy or package the generated Swift and
TypeScript output at a pinned Hew commit, and validate their real-runtime golden
fixtures against that same commit.
