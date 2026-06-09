# HBF to Cap'n Proto Migration — Reach Audit

> **Status: historical — C++/MLIR was retired in v0.5 (commit 842842bd). Current implementation: hew-codegen-rs direct Rust/Inkwell LLVM emission.**
> Codegen reachability findings in this audit describe the retired C++ backend;
> runtime HBF inventory remains useful migration context.

**Date**: 2026-05-17  
**Branch**: `v05-integration` @ `35b60a9d`  
**Scope**: Every file, function, and ABI symbol that touches the Hew Binary Format (HBF), classified by fate during a Cap'n Proto migration.

---

## Background

HBF (Hew Binary Format) is the current wire encoding used for actor-message transport across nodes. It consists of two distinct layers:

1. **Field-level encoding** — a Protocol Buffers-compatible TLV encoding (varint, fixed32, fixed64, length-delimited) used to serialize individual record types into bytes. This is the format `wire type` declarations compile to. The MLIR codegen layer calls the `hew_wire_encode_field_*` / `hew_wire_decode_*` family.

2. **Transport framing** — an HBF envelope with 4-byte magic `"HEW1"`, 1-byte version, 1-byte flags, and a length-prefixed payload. The envelope carries `target_actor_id`, `source_actor_id`, `msg_type`, `request_id`, and `source_node_id`. The runtime uses `hew_wire_encode_envelope` / `hew_wire_decode_envelope` to frame and unframe messages before they cross the network.

These two layers are implemented in a single file (`hew-runtime/src/wire.rs`) but serve different callers. Understanding which callers use which layer is the central finding of this audit.

---

## File-Level Inventory

| File | LoC | HBF role | Fate |
|---|---|---|---|
| `hew-runtime/src/wire.rs` | 2,430 | Defines all HBF types, constants, and 36 `#[no_mangle]` exported functions | **REPLACE** (field-level) + **DELETE** (HBF framing layer) — see per-symbol table |
| `hew-runtime/src/connection.rs` | 3,232 | Calls `hew_wire_encode_envelope`, `hew_wire_decode_envelope`, `hew_wire_buf_*` for actor-message framing; imports `HBF_MAGIC`, `HBF_VERSION`, `HBF_FLAG_COMPRESSED` | **REPLACE** — the 26 HBF-touching lines must be rewritten against Cap'n Proto framing |
| `hew-runtime/src/transport.rs` | 2,283 | Calls `hew_wire_buf_init`, `hew_wire_encode_envelope`, `hew_wire_buf_free` in `wire_send_envelope` | **REPLACE** — the 9 HBF-touching lines are the outbound framing seam |
| `hew-runtime/src/hew_node.rs` | 4,912 | Calls `hew_wire_buf_init`, `hew_wire_encode_envelope`, `hew_wire_buf_free` in three message-dispatch sites (lines 967, 1044, 2133) | **REPLACE** — the 43 HBF-touching lines cover intra-node and cross-node dispatch paths |
| `hew-runtime/src/quic_transport.rs` | 1,488 | Uses length-prefixed framing (`framed_send_quic`, `framed_recv_quic`) layered on top of HBF for QUIC transport compatibility (comment on line 10) | **KEEP** — the framing layer is LE-length-prefix independent of HBF magic; only the outer framing protocol changes |
| `hew-runtime/src/cluster.rs` | 3,191 | No direct HBF imports found | **KEEP** — no change required |
| `hew-runtime/src/remote_sup.rs` | 2,613 | No direct HBF imports found | **KEEP** — no change required |
| `hew-runtime/src/sim_transport.rs` | 1,739 | No direct HBF imports found | **KEEP** — no change required |
| `retired C++ backend: MLIRGenWire.cpp` | 2,846 | Calls 22 distinct `hew_wire_*` symbols by name for MLIR codegen of `wire type` encode/decode lowering | **REPLACE** — all 22 symbols are in the field-encoding half; each must be replaced with Cap'n Proto equivalents |
| `retired C++ backend integration tests` | (large) | References `hew_wire_buf_new`, `hew_wire_buf_init`, `hew_wire_encode_field_bytes`, `hew_wire_encode_field_string`, `hew_wire_decode_bytes`, `hew_wire_decode_string` in wire-encode test assertions | **REPLACE** — test assertions must be rewritten for Cap'n Proto symbol names |
| `std/encoding/wire/wire.hew` | 86 | Hew-language stdlib module that re-implements HBF header encode/decode/validate in pure Hew for user-facing use | **REPLACE** — header layout, magic, and validation logic all change with Cap'n Proto |
| `hew-runtime/src/actor.rs` | — | No HBF imports found | **KEEP** |
| `hew-types/` | — | No HBF imports found | **KEEP** |
| `hew-codegen-rs/` | — | No HBF imports found | **KEEP** |
| `hew-mir/` | — | No HBF imports found | **KEEP** |
| `hew-cabi/` | — | No HBF imports found | **KEEP** |

---

## Per-Symbol Table: `hew_wire_*` Exported Functions

All 36 exported symbols are defined in `hew-runtime/src/wire.rs`. They divide into three functional groups.

### Group A — Buffer management (8 symbols)

These manage the `HewWireBuf` scratch buffer used for both field encoding and envelope framing.

| Symbol | Purpose | Called by | Fate |
|---|---|---|---|
| `hew_wire_buf_init` | Initialise stack-allocated `HewWireBuf` for write | `connection.rs`, `hew_node.rs`, `transport.rs`, `MLIRGenWire.cpp` (indirectly via `hew_wire_buf_new`) | **REPLACE** — buffer abstraction needs a Cap'n Proto equivalent (`capnp::MallocMessageBuilder` or similar) |
| `hew_wire_buf_new` | Heap-allocate a `HewWireBuf` | `MLIRGenWire.cpp` | **REPLACE** |
| `hew_wire_buf_free` | Free the data buffer inside a stack `HewWireBuf` | `connection.rs`, `hew_node.rs`, `transport.rs` | **REPLACE** |
| `hew_wire_buf_destroy` | Free a heap-allocated `HewWireBuf` | `MLIRGenWire.cpp` | **REPLACE** |
| `hew_wire_buf_reset` | Reset write position for reuse | Internal to `wire.rs` | **REPLACE** |
| `hew_wire_buf_init_read` | Set up a `HewWireBuf` for reading a byte slice | `connection.rs`, `MLIRGenWire.cpp` | **REPLACE** |
| `hew_wire_buf_data` | Return pointer to written bytes | `MLIRGenWire.cpp` | **REPLACE** |
| `hew_wire_buf_len` | Return number of written bytes | `MLIRGenWire.cpp` | **REPLACE** |
| `hew_wire_buf_has_remaining` | Check if read cursor has bytes left | `MLIRGenWire.cpp` | **REPLACE** |

### Group B — Field-level encoding (11 symbols, called by codegen only)

These implement the Protocol Buffers-compatible TLV field encoding used by `wire type` declarations. They are the ABI contract between `hew-runtime` and `hew-codegen` for message payload serialization.

| Symbol | Purpose | Called by | Fate |
|---|---|---|---|
| `hew_wire_encode_varint` | Encode a raw LEB128 varint | Internal / `MLIRGenWire.cpp` | **REPLACE** |
| `hew_wire_decode_varint` | Decode a LEB128 varint | Internal / `MLIRGenWire.cpp` | **REPLACE** |
| `hew_wire_zigzag_encode` | ZigZag encode for signed integers | `MLIRGenWire.cpp` | **REPLACE** |
| `hew_wire_zigzag_decode` | ZigZag decode for signed integers | `MLIRGenWire.cpp` | **REPLACE** |
| `hew_wire_encode_field_varint` | Encode a varint TLV field (tag + value) | `MLIRGenWire.cpp` | **REPLACE** |
| `hew_wire_encode_field_fixed32` | Encode a fixed-32 TLV field | `MLIRGenWire.cpp` | **REPLACE** |
| `hew_wire_encode_field_fixed64` | Encode a fixed-64 TLV field | `MLIRGenWire.cpp` | **REPLACE** |
| `hew_wire_encode_field_bytes` | Encode a length-delimited bytes field | `MLIRGenWire.cpp` | **REPLACE** |
| `hew_wire_encode_field_string` | Encode a length-delimited string field | `MLIRGenWire.cpp` | **REPLACE** |
| `hew_wire_decode_fixed32` | Decode a fixed-32 field | `MLIRGenWire.cpp` | **REPLACE** |
| `hew_wire_decode_fixed64` | Decode a fixed-64 field | `MLIRGenWire.cpp` | **REPLACE** |
| `hew_wire_decode_bytes` | Decode a length-delimited bytes field | `MLIRGenWire.cpp` | **REPLACE** |
| `hew_wire_decode_string` | Decode a length-delimited string into a malloc'd C string | `MLIRGenWire.cpp` | **REPLACE** |
| `hew_wire_decode_tag` | Decode a TLV tag (field number + wire type) | `MLIRGenWire.cpp` | **REPLACE** |
| `hew_wire_skip_field` | Skip an unknown TLV field | `MLIRGenWire.cpp` | **REPLACE** |

### Group C — HBF framing (9 symbols, called by runtime only)

These implement the HBF header and actor-message envelope. They are the format boundary that changes to Cap'n Proto.

| Symbol | Purpose | Called by | Fate |
|---|---|---|---|
| `hew_wire_encode_header` | Encode a 10-byte HBF header (`HEW1` + version + flags + payload_len) | `wire.rs` internal, `wire.hew` stdlib | **DELETE** — HBF magic and header layout go away |
| `hew_wire_decode_header` | Decode an HBF header from raw bytes | `wire.rs` internal | **DELETE** |
| `hew_wire_validate_header` | Validate `HEW1` magic + version + flags | `wire.rs` internal | **DELETE** |
| `hew_wire_write_hbf_header` | Write an HBF header into a `HewWireBuf` | `wire.rs` tests | **DELETE** |
| `hew_wire_encode_header_hew` | Hew-ABI wrapper: encode header, return a `HewVec` bytes | Hew codegen (Hew-side `wire.encode_header`) | **DELETE** — the stdlib API changes |
| `hew_wire_decode_header_hew` | Hew-ABI wrapper: decode header from a `HewVec` | Hew codegen (Hew-side `wire.decode_header`) | **DELETE** |
| `hew_wire_validate_header_hew` | Hew-ABI wrapper: validate header from a `HewVec` | Hew codegen (Hew-side `wire.validate_header`) | **DELETE** |
| `hew_wire_encode_envelope` | Encode `HewWireEnvelope` (target, source, msg_type, payload, request_id, source_node_id) into a `HewWireBuf` using TLV fields | `connection.rs`, `transport.rs`, `hew_node.rs` | **DELETE** — the envelope structure is replaced by Cap'n Proto message schema |
| `hew_wire_decode_envelope` | Decode TLV bytes into a `HewWireEnvelope` | `connection.rs` | **DELETE** |

### Group D — HewVec bridge (2 symbols)

These bridge between `HewWireBuf` and Hew's runtime bytes type (`HewVec`). Used by codegen for the `wire type` encode/decode pattern.

| Symbol | Purpose | Called by | Fate |
|---|---|---|---|
| `hew_wire_buf_to_bytes` | Convert a `HewWireBuf` → `HewVec` (bytes) | `MLIRGenWire.cpp` | **REPLACE** — Cap'n Proto builder exposes a flat byte view differently |
| `hew_wire_bytes_to_buf` | Convert a `HewVec` (bytes) → `HewWireBuf` for decoding | `MLIRGenWire.cpp` | **REPLACE** |
| `hew_vec_from_raw_bytes` | Create a `HewVec` from a raw `(data, len)` pair | Codegen / internal | **KEEP** — not HBF-specific; general runtime utility |

---

## Caller Graph: External References to `hew_wire_*`

The brief's expected finding of "zero outside callers" is **incorrect**. There are two major external callers:

### Caller 1: `retired C++ backend: MLIRGenWire.cpp` (22 symbols)

The C++ MLIR codegen layer calls 22 `hew_wire_*` symbols by name to lower `wire type` encode/decode operations to MLIR calls. This is the active ABI contract between the compiler and the runtime for user-defined wire types.

Symbols called from `MLIRGenWire.cpp`:
- `hew_wire_buf_new`, `hew_wire_buf_init_read`, `hew_wire_buf_data`, `hew_wire_buf_len`, `hew_wire_buf_has_remaining`, `hew_wire_buf_destroy`
- `hew_wire_encode_field_varint`, `hew_wire_encode_field_fixed32`, `hew_wire_encode_field_fixed64`, `hew_wire_encode_field_bytes`, `hew_wire_encode_field_string`
- `hew_wire_decode_varint`, `hew_wire_decode_fixed32`, `hew_wire_decode_fixed64`, `hew_wire_decode_bytes`, `hew_wire_decode_string`, `hew_wire_decode_tag`
- `hew_wire_zigzag_encode`, `hew_wire_zigzag_decode`
- `hew_wire_skip_field`
- `hew_wire_buf_to_bytes`, `hew_wire_bytes_to_buf`

All 22 are in the field-encoding group (Group A + B + D). None of the HBF framing symbols (Group C) are called from codegen. The codegen layer does not write HBF headers or envelopes — it only serializes field values.

**Migration implication**: `MLIRGenWire.cpp` must be rewritten to emit Cap'n Proto builder/reader calls instead of `hew_wire_encode_field_*` / `hew_wire_decode_*` calls. This is a complete rewrite of 2,846 LOC of C++ MLIR codegen. This was not flagged in the networking master plan and is the most significant finding of this audit.

### Caller 2: `retired C++ backend integration tests`

Test assertions verify that the MLIR output contains calls to `hew_wire_buf_new`, `hew_wire_buf_init`, `hew_wire_encode_field_bytes`, `hew_wire_encode_field_string`, `hew_wire_decode_bytes`, `hew_wire_decode_string`. These tests must be updated to match the new Cap'n Proto symbol names.

### Within `hew-runtime/` (internal callers)

- `connection.rs`: calls Group C (envelope encode/decode) and Group A (buffer management)
- `transport.rs`: calls Group C (envelope encode) and Group A (buffer management)  
- `hew_node.rs`: calls Group C (envelope encode) and Group A (buffer management)

These are all within `hew-runtime` and are expected callers.

---

## LoC Summary by Fate

| Fate | Files | Approx LoC |
|---|---|---|
| **DELETE** | `wire.rs` (Group C: ~500 LoC of framing functions + HBF constants/structs), `std/encoding/wire/wire.hew` (86 LoC) | ~586 LoC removed |
| **REPLACE** | `wire.rs` (Group A+B+D: ~1,800 LoC), `connection.rs` (~26 call sites), `transport.rs` (~9 call sites), `hew_node.rs` (~43 call sites), `MLIRGenWire.cpp` (2,846 LoC), `retired backend integration tests` (partial) | ~4,800 LoC changed |
| **KEEP** | `quic_transport.rs` (1,488), `cluster.rs` (3,191), `remote_sup.rs` (2,613), `sim_transport.rs` (1,739), `hew-types/`, `hew-mir/`, `hew-codegen-rs/`, `hew-cabi/`, `actor.rs` | ~9,000+ LoC unchanged |

**Total HBF-coupled LoC**: approximately 5,386 LoC across 9 files.

---

## Key Architectural Findings

### Finding 1: Two-layer architecture requires two separate migration tracks

HBF encodes two different things:
- **User-facing message fields** — the `wire type` compile target. Currently protobuf-compatible TLV. The codegen layer (MLIR, C++) emits calls to `hew_wire_encode_field_*`. Migrating this to Cap'n Proto requires rewriting `MLIRGenWire.cpp`.
- **Network transport envelope** — the `HEW1` framing with actor IDs, msg_type, payload. Used only within `hew-runtime` by `connection.rs`, `transport.rs`, `hew_node.rs`. Migrating this to Cap'n Proto requires a new envelope schema and new encode/decode functions.

These two migrations can proceed independently:
- **Track 1 (envelope)**: New Cap'n Proto schema for `HewEnvelope`; replace `hew_wire_encode_envelope` / `hew_wire_decode_envelope` call sites in `connection.rs`, `transport.rs`, `hew_node.rs`.
- **Track 2 (field encoding)**: New MLIR lowering in `MLIRGenWire.cpp` emitting Cap'n Proto builder/reader calls; replace the 22 `hew_wire_*` symbols called from codegen.

Track 1 is a runtime-internal change. Track 2 involves the compiler ABI and is the heavier lift.

### Finding 2: `MLIRGenWire.cpp` is a major external caller

The networking master plan (`m3-networking-master-plan.md`) classified `hew_wire_*` callers as limited to `hew-runtime`. This is inaccurate. `MLIRGenWire.cpp` (2,846 LoC, C++) calls 22 HBF symbols to lower `wire type` encode/decode operations. Any Cap'n Proto migration plan that only addresses `hew-runtime` will leave the compiler emitting dead symbol names.

### Finding 3: `hew_vec_from_raw_bytes` is not HBF-specific

The symbol `hew_vec_from_raw_bytes` (line 1293 of `wire.rs`) creates a `HewVec` from a raw byte slice. It is a general utility, not tied to HBF semantics. It should be moved to a runtime utility module rather than deleted.

### Finding 4: `std/encoding/wire/wire.hew` is the user-facing HBF API

The Hew stdlib exposes `std::encoding::wire` with `encode_header`, `decode_header`, and `validate_header` functions that implement the `HEW1` header format in pure Hew. This is the user-facing surface. If HBF is retired, this stdlib module's API changes or is removed. Any existing Hew programs importing `std::encoding::wire` will break.

### Finding 5: Envelope symbols are Rust-internal; field symbols cross the Rust/C++ boundary

The HBF framing symbols (`hew_wire_encode_envelope`, `hew_wire_decode_envelope`, `hew_wire_encode_header`, etc.) are only called within `hew-runtime`'s Rust code. They are `#[no_mangle]` for C-ABI compatibility but have no C or C++ callers.

The field-encoding symbols (`hew_wire_encode_field_varint`, `hew_wire_decode_bytes`, etc.) are called from `hew-codegen` (C++ MLIR). This is the live cross-language ABI seam.

---

## Migration Risk Assessment

| Risk | Severity | Notes |
|---|---|---|
| `MLIRGenWire.cpp` must be fully rewritten | High | 2,846 LoC of C++ MLIR codegen; every `wire type` encode/decode pattern changes |
| `wire type` language feature is coupled to HBF field encoding | High | If Cap'n Proto uses a fundamentally different builder pattern (non-TLV), the codegen IR changes, not just the runtime calls |
| `std::encoding::wire` stdlib API is a breaking change | Medium | Any user code calling `wire.encode_header` / `wire.validate_header` breaks |
| `HewWireEnvelope` layout is `repr(C)` | Medium | Three call sites in `hew_node.rs` directly construct the struct; Cap'n Proto envelope is a different shape |
| Compression flag (`HBF_FLAG_COMPRESSED`) | Low | No evidence compression is actively used in `connection.rs`; the flag exists but payload inspection shows it's not set in normal paths |
| Test coverage for wire encoding is extensive | Positive | `wire.rs` has ~900 LoC of inline tests; `retired backend integration tests` has wire-specific test cases; coverage exists for the behaviours that must be preserved |

---

## Out of Scope (This Audit)

- Cap'n Proto schema design for the replacement envelope or field encoding
- Migration sequencing and slice ordering (that is the implementation plan)
- `hew-codegen-rs/` (the Rust new-backend codegen) — no HBF references found; out of scope
- Any changes to `hew-runtime/src/wire.rs`, `connection.rs`, or any other file — this is a read-only audit

---

## Recommended Next Steps

1. **Confirm Cap'n Proto is the right choice for both layers.** The field-encoding layer (`wire type`) was already protobuf-compatible TLV; Cap'n Proto's flat builder model is structurally different. A proto3 migration (keeping TLV semantics, replacing HBF envelope only) would have a narrower blast radius.

2. **Treat `MLIRGenWire.cpp` as a first-class migration target.** The plan that gates on this rewrite is not the runtime-only envelope change — it is a compiler rewrite. Scope accordingly.

3. **Sequence envelope migration (Track 1) before field-encoding migration (Track 2).** Track 1 changes only `hew-runtime` internals and has no compiler impact. Track 2 requires co-ordinated changes to `hew-codegen`, `hew-runtime`, and tests.

4. **Preserve `hew_vec_from_raw_bytes` as a non-HBF utility.** Move it to `hew-runtime/src/vec.rs` or a dedicated bridge module rather than deleting it with the wire module.
