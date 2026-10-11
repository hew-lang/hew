# Hew Observe protocol

`v1/openapi.json` is the producer-owned, authoritative wire contract for the
Hew profiler/Observe HTTP surface. It describes the existing HTTP/1.1 JSON,
text, and binary endpoints rather than inventing a second transport.

## Why OpenAPI 3.1

The deployed protocol is HTTP with JSON envelopes. OpenAPI 3.1 embeds JSON
Schema, documents every route/header/media type, and preserves that transport.
Protobuf would require a new binary endpoint and serializer before it could be
authoritative. CBOR plus CDDL would have the same migration cost and materially
weaker Swift and TypeScript generation. Neither binary choice fixes drift in
the current JSON clients.

Every full-width integer is a canonical base-10 JSON string on the wire. The
generator maps those strings to Rust `u64`/`i64`, Swift `UInt64`/`Int64`, and
TypeScript `bigint`; JSON numbers are rejected. Small bounded integers remain
JSON numbers and TypeScript `number` values after integer and range checks.
This representation is safe through ordinary `JSON.parse` and Rust/Tauri JSON
IPC without a custom tokenizer or an IEEE-754 rounding window.

## Compatibility policy

- A v1 producer emits every property listed in a model's `required` array.
- Required-nullable keys are distinct from omitted keys. A `null` actor or
  handler means the producer explicitly could not or should not attribute it;
  omission is malformed v1.
- Unknown object properties and unknown `event_type`, `state`, and `trap_kind`
  strings are accepted. Those taxonomies are intentionally not JSON enums.
- Consumers reject malformed envelopes and any `schema_version` other than
  exactly `v1`.
- Transport failures, reconnection policy, durable recordings, and
  last-good-view state remain client responsibilities. Producer stream identity,
  cursor validity, and retention overruns are wire data because clients cannot
  reconstruct them honestly.

## Trace stream

`GET /api/traces` is a non-destructive, stateless cursor read. Omit both
`stream_id` and `after` to start at the oldest retained event. Thereafter send
both values, with `after` set to the previous response's `next_cursor`.
`after` is exclusive. `limit` is an integer from 1 through 256 and defaults to
256. A response containing 256 events proves only that the response limit was
reached; use `has_more` for retained backlog and `overrun` for proven loss.

Source sequences start at 1 and increase strictly within a `stream_id`.
`next_sequence` is one past the highest assigned sequence, while `next_cursor`
is the last sequence returned or the effective caught-up cursor for an empty
page. `oldest_sequence` equals `next_sequence` when the ring is empty. Two
consumers using the same cursor receive the same retained events and never
drain one another.

`producer_instance_id` is stable for one process lifetime. `stream_id` changes
on trace clear/reset and disable-to-enable transitions. A cursor from another
stream receives HTTP 409 with `trace_stream_mismatch`, the current stream, and
the current producer instance. Malformed, unpaired, or future cursors receive a
versioned HTTP 400 error. A stale cursor within the current stream succeeds but
includes the exact inclusive missing sequence range in `overrun`.

Every trace event has explicit source and destination attribution. The known
attribution values are `local_actor`, `external`, `remote_unknown`, and
`not_applicable`. `local_actor` requires the corresponding actor object; the
other current values require it to be null. This release does not fabricate a
remote identity. Event timestamps remain producer-monotonic; `clock_anchor`
correlates that clock approximately with Unix time and declares its sampling
uncertainty. Consumers must not infer causal links from timestamp proximity.

## Generated outputs

- Rust crate: `hew-observe-protocol/src/generated.rs`
- Swift: `v1/generated/ObserveProtocolV1.swift`
- TypeScript: `v1/generated/observe-protocol-v1.ts`

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
