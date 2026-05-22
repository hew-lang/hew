# hew-runtime/schemas

This directory contains the CDDL schema files that are the **doc-of-truth**
for the Hew CBOR wire protocol.

## What is CDDL?

[CDDL (Concise Data Definition Language, RFC 8610)](https://www.rfc-editor.org/rfc/rfc8610)
is a schema language for CBOR and JSON data structures. Each `.cddl` file in
this directory specifies the exact shape of one family of wire frames. The Rust
types in `src/envelope.rs` and the codec in `src/cbor_envelope.rs` (W2) MUST
remain consistent with these definitions.

## Files

| File | Covers |
|------|--------|
| `envelope.cddl` | Control frames and data envelope frames (CBOR wire layer, v1) |

> W2 will add `handshake.cddl` once the CBOR handshake codec lands.

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

### C-FFI vs CBOR shape

The legacy `HewWireEnvelope` C struct (see `src/wire.rs`) carries a
`(payload_size: u32, payload: *mut u8)` pair because C cannot express
owned byte slices. The CBOR shape collapses these into a single `bstr` field.
The two representations are semantically equivalent; the split is
FFI-only noise with no wire significance.

### Version gating

`wire-version = 1` is a constant in the schema. Any frame whose `version`
field differs from 1 MUST be rejected by the decoder with an `UnknownVersion`
error. This allows the schema to evolve without silent misparsing.
