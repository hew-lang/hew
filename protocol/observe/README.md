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

This compact stale-cursor example fixes the boundary arithmetic for client
fixtures. Given `after=4`, retained sequence 8 proves that 5 through 7 were
lost; sequence 8 is returned, and sequence 9 remains as backlog:

```http
GET /api/traces?stream_id=0123456789abcdef0123456789abcdef&after=4&limit=1
```

```json
{"schema_version":"v1","data":{"producer_instance_id":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","stream_id":"0123456789abcdef0123456789abcdef","oldest_sequence":"8","next_sequence":"10","next_cursor":"8","has_more":true,"overrun":{"first_missing_sequence":"5","last_missing_sequence":"7","count":"3"},"tracing_enabled":true,"sampling_mode":"all","events_recorded_total":"9","events_overwritten_total":"7","events_sampled_out_total":"0","batch_source_monotonic_ns":"1200","clock_anchor":{"monotonic_ns":"1000","unix_epoch_ns":"1791702000000000000","uncertainty_ns":"20"},"events":[{"sequence":"8","trace_id":"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb","span_id":"80","parent_span_id":"0","subject":null,"source_attribution":"local_actor","source_actor":{"actor_id":"2","spawn_serial":"2","actor_type":"Router"},"destination_attribution":"local_actor","destination_actor":{"actor_id":"1","spawn_serial":"1","actor_type":"Ledger"},"event_type":"send","msg_type":7,"timestamp_ns":"1100","handler_name":"Ledger.record"}]}}
```

The next request uses `after=8`. It must not add one client-side: `after` is
already the last consumed sequence, and the producer performs the exclusive
increment. On HTTP 409 the client starts a new segment and reattaches without
either cursor parameter after validating the mismatch envelope.

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

The Linux gate cannot type-check Swift. The smallest targeted macOS/Clunk check
is:

```sh
swiftc -typecheck protocol/observe/v1/generated/ObserveProtocolV1.swift
tmpdir="$(mktemp -d)"
cp protocol/observe/v1/generated/ObserveProtocolV1.swift "$tmpdir/ObserveProtocolV1.swift"
cat >"$tmpdir/main.swift" <<'SWIFT'
import Foundation

let valid = #"{"sequence":"18446744073709551615","trace_id":"0123456789abcdef0123456789abcdef","span_id":"0","parent_span_id":"0","subject":null,"source_attribution":"local_actor","source_actor":{"actor_id":"18446744073709551615","spawn_serial":null,"actor_type":null},"destination_attribution":"remote_unknown","destination_actor":null,"event_type":"send","msg_type":0,"timestamp_ns":"0","handler_name":null}"#
let event = try JSONDecoder().decode(TraceEvent.self, from: Data(valid.utf8))
precondition(event.sequence == UInt64.max)
let invalid = valid.replacingOccurrences(of: #""source_attribution":"local_actor""#, with: #""source_attribution":"remote_unknown""#)
do {
    _ = try JSONDecoder().decode(TraceEvent.self, from: Data(invalid.utf8))
    fatalError("accepted inconsistent actor attribution")
} catch is DecodingError {}
SWIFT
swiftc "$tmpdir/ObserveProtocolV1.swift" "$tmpdir/main.swift" -o "$tmpdir/observe-v1-smoke"
"$tmpdir/observe-v1-smoke"
```
