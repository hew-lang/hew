# hew-runtime/schemas

This directory contains structural CDDL for the Hew CBOR wire protocol.
`protocol/node_controls.hew` owns the service-offer and monitor-DOWN structures,
including their nested location, session, reason and cleanup records. Ordinary
checker data plans generate `src/node_protocol_generated.rs` and
`schemas/node-controls.generated.cddl`; use `make node-protocol` to regenerate
and `make node-protocol-check` to detect drift. Standalone runtime builds consume
the checked-in Rust and do not invoke the compiler or generator.

Read `envelope.cddl` together with `node-controls.generated.cddl`. Runtime
adapters own semantic validation (identity, grants, sorted interfaces, terminal
reason tags and resource bounds); the generated schema describes data structure,
not those admission rules. The remaining frame, membership and handshake shapes
have not yet moved to checked Hew declarations.

## What is CDDL?

[CDDL (Concise Data Definition Language, RFC 8610)](https://www.rfc-editor.org/rfc/rfc8610)
is a schema language for CBOR and JSON data structures. The checked
`protocol/node_controls.hew` declarations generate both
`src/node_protocol_generated.rs` and `schemas/node-controls.generated.cddl`.
The adapters in `src/envelope.rs` retain domain policy. Remaining legacy
protocol structures in that module and the frame reader must stay consistent
with their manually maintained CDDL until they move to checked declarations.

## Files

| File                           | Covers                                                                                                         |
| ------------------------------ | -------------------------------------------------------------------------------------------------------------- |
| `node-controls.generated.cddl` | Checked service-offer and monitor-DOWN data structures                                                         |
| `envelope.cddl`                | Control frames and data envelope frames (CBOR wire layer) — the transport CONTAINER                            |
| `wire-body.cddl`               | The per-`#[wire]`-type CBOR body carried inside an envelope frame's `payload` (`bstr`) slot — the message BODY |

The fixed-width authenticated handshake is specified in HEW-DIST-SPEC §4.

### Frame vs body

`envelope.cddl` describes the frame map and treats `payload` (key 6) as an
opaque `bstr`. `wire-body.cddl` describes the bytes INSIDE that `bstr`: the
tagged-CBOR body the codegen serializer emits for each message type (a record as
an `@N`-keyed map, an enum as a unit tag or a single-entry map-of-one). The
conformance test `hew-runtime/tests/wire_body_cddl_conformance.rs` validates
emitted bytes against `wire-body.cddl` so the schema stays an enforced contract.

## Design decisions

### Payload length is NOT in the envelope

The CBOR payload is a byte string (`bstr`) whose length is carried by the CBOR
encoding itself. The transport layer (QUIC stream or TCP with a length-prefix
framing) owns message delimitation. Encoding the length a second time inside
the CBOR would create a redundant, possibly conflicting source of truth. The
schemas deliberately omit any `payload_len` field.

### Field numbering

Integer keys are used (rather than text keys) to minimise frame size. Field
numbers are stable across protocol versions; adding new fields uses new numbers
and leaves older decoders unaffected (they reject unknown versions via the
`version` field).

### Version gating

`wire-version` is a constant in the schema, matching `WIRE_VERSION` in the
runtime. Any other version MUST be rejected by the decoder with an
`UnknownVersion` error. The authenticated handshake also checks this version
before publishing a route.

### Granted requests

Every actor request carries its handler declaration, recipient-bound export
grant and request kind. Its request id is positive, including for sends whose
completion reports mailbox admission. A text-only handler name cannot authorize
delivery. Replies and withdrawals echo the original request id.

### Request budgets and withdrawal

Envelope key 8 carries a bounded remaining ask budget or `nil`. It never holds
an absolute clock value. Withdrawal uses message tag 2 with an exact target,
positive request id, empty payload and nil source/budget. The receiver binds
withdrawal to the authenticated connection publication that admitted the ask.
