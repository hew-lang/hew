# HEW-WIRE-FORMAT-DOCTRINE

> **Status:** Canonical wire-format choices across the project.
> **Audience:** language designers, runtime implementers, stdlib maintainers,
> codegen/ABI reviewers, docs reviewers, integration authors.
> **Stance:** different layers of the system have different wire-format
> needs. This document fixes which format is canonical at which layer, what
> is implemented today, and what is honestly still a stub.

This doctrine governs five orthogonal questions:

1. What format does the Hew runtime use for **inter-process** (actor-to-actor,
   cross-node) messaging?
2. What formats are available to **user code** that needs to read or write
   wire bytes at the language layer?
3. What is Hew's posture toward **consumer interop** tooling (OpenAPI,
   `proto-gen`, schema-first generators)?
4. What patterns are explicitly **anti-doctrine** — known footguns to avoid?
5. What **stubs and migration commitments** exist today, and when do they
   become obsolete?

Each section names what is real, cites the source-of-truth files, and marks
unimplemented surfaces with `WHEN-obsolete` + `WHAT-real-solution-looks-like`
markers per the project's `Shims, Stubs, and Seams` rule.

---

## 1. Inter-process messaging — CDDL + CBOR

### Decision

The Hew runtime's actor-to-actor wire format is **CBOR** (RFC 8949), with
the schema defined in **CDDL** (RFC 8610). The doc-of-truth schema is
[`hew-runtime/schemas/envelope.cddl`][cddl]. The Rust-native frame types
that conform to that schema live in
[`hew-runtime/src/envelope.rs`][envelope-rs].

[cddl]: ../../hew-runtime/schemas/envelope.cddl
[envelope-rs]: ../../hew-runtime/src/envelope.rs

### What lives in the substrate

- A `wire-frame` is either a `control-frame` or an `envelope-frame`. Both
  are CBOR maps keyed by small integers (1..=8). `frame_type` (key `2`)
  selects the branch.
- An `envelope-frame` carries one actor-to-actor message and includes
  `version`, `frame_type`, `target_actor_id`, `source_actor_id`,
  `msg_type`, `payload` (the user-level serialised body, as `bstr`),
  `request_id`, and `source_node_id`.
- `version` is currently fixed at `1`. Decoders **must** reject unknown
  versions with `UnknownVersion`; they must not best-effort parse a frame
  whose version they do not recognise.

The Rust mirror is `EnvelopeFrame` (and `ControlFrame`) in
[`hew-runtime/src/envelope.rs`][envelope-rs] starting near line 30. The
CDDL integer keys are mirrored as field-doc comments on each struct field.

### Why CBOR, not JSON / msgpack / Protobuf / HBF

- **JSON.** Text-based, ambiguous numeric typing (no native `i64` vs
  `f64`), no native byte-string type. Unsuitable for a tight
  actor-messaging hot path that carries arbitrary user payloads as
  opaque bytes.
- **MessagePack.** Closely related to CBOR but has no equivalent of
  CDDL: no widely adopted schema language with formal validation
  tooling. Schema-first decoders are the fail-closed posture we want for
  cross-node traffic from untrusted peers.
- **Protocol Buffers (and the legacy HBF TLV substrate).** Schema-first,
  but the v0.5 substrate retired the C++/MLIR codegen path that produced
  HBF descriptors (see `CHANGELOG.md [Unreleased]`). Doubling down on a
  Protobuf variant would require re-introducing a schema compiler we do
  not own.
  CBOR + CDDL gets us schema-first validation without owning a code
  generator.
- **Cap'n Proto.** Considered during the HBF retirement. The zero-copy
  win is real, but the toolchain commitment and code-generation surface
  exceeded what v0.5 could absorb. CBOR is the deliberate, smaller-scope
  alternative.

### Substrate provenance

The CBOR envelope substrate is built from three layers:

- **Envelope types + CDDL schema** — `EnvelopeFrame` / `ControlFrame` Rust
  types in `hew-runtime/src/envelope.rs` and the canonical schema in
  `hew-runtime/schemas/envelope.cddl`.
- **Fail-closed decoder** — unknown versions and truncated input are rejected
  (`UnknownVersion`); this is what makes CBOR usable as a trust boundary,
  not just a serialisation format.
- **Runtime cutover** — the runtime's actor-messaging call sites use the CBOR
  envelope; HBF is not on the inter-process hot path.

The round-trip and version-rejection tests live in
[`hew-runtime/tests/envelope_round_trip.rs`][envelope-test].

[envelope-test]: ../../hew-runtime/tests/envelope_round_trip.rs

### Required posture for runtime code touching the wire

- Add new envelope fields **first to the CDDL**, then to `EnvelopeFrame`.
  The CDDL is the doc-of-truth; the Rust struct conforms to it, not the
  other way around.
- New frame kinds extend the `wire-frame` rule alternatives. Do not
  invent a parallel framing layer.
- Decoders must reject unknown `version` values rather than degrading.
  Best-effort parsing of an unrecognised version is a silent
  compatibility hazard.
- The transport container (QUIC stream or TCP length-prefix) is
  responsible for framing on the wire. Do **not** re-encode payload
  length inside the CBOR envelope — the schema deliberately omits a
  redundant length field for this reason.

---

## 2. User-facing wire formats — stdlib encoding modules

### Decision

User code that needs to read or write wire bytes at the **language
layer** (HTTP bodies, file formats, third-party APIs) reaches for a
module under `std::encoding::*`. Hew does **not** expose the runtime
CBOR envelope to user code as a general-purpose serialisation surface.
That envelope is an internal trust boundary, not a user API.

The opaque `Value` contract shared by the structural encoders is
described in [`std/encoding/wire/README.md`](../../std/encoding/wire/README.md)
and was settled by issue #1247.

### Module inventory and honest status

| Module | Status | What's there today |
| --- | --- | --- |
| `std::encoding::json` | **Real.** Production-shape encoder/decoder. | `std/encoding/json/` (~2k LOC). Backing parser + the opaque `Value` surface. |
| `std::encoding::yaml` | **Real.** Full parser/serialiser. | `std/encoding/yaml/` (~2.4k LOC). |
| `std::encoding::toml` | **Real.** Parser/generator + datetime variant. | `std/encoding/toml/` (~1.2k LOC). |
| `std::encoding::msgpack` | **Real, JSON-bridged.** Encode/decode against the canonical `Value`; per the `wire` README it bridges through JSON's value model when crossing the opaque surface. | `std/encoding/msgpack/` (~1k LOC). |
| `std::encoding::protobuf` | **Real, scoped.** Wire-format encode/decode helpers; not a schema compiler. | `std/encoding/protobuf/` (~1.5k LOC). |
| `std::encoding::xml` | **Real, scoped.** Parse/serialise. | `std/encoding/xml/` (~850 LOC). |
| `std::encoding::csv` | **Real, scoped.** | `std/encoding/csv/`. |
| `std::encoding::markdown` | **Real, scoped.** Markdown → HTML rendering only. | `std/encoding/markdown/`. |
| `std::encoding::base64` | **Real.** | `std/encoding/base64/`. |
| `std::encoding::hex` | **Real.** | `std/encoding/hex/`. |
| `std::encoding::compress` | **Real.** gzip/deflate/zlib. | `std/encoding/compress/`. |
| `std::encoding::wire` | **Substrate.** Holds the opaque `Value` contract and the legacy HBF byte-layout helpers (`encode_header` / framing). | `std/encoding/wire/` — see §5 for the migration story. |

Each module's README states its own scope. The doctrine here is about
**which one to reach for**, not how each one is implemented.

### Recommended path for user code that needs wire serialisation

- **Talking to an HTTP / file ecosystem:** pick the module that matches
  the wire format you owe the other side. JSON for most HTTP, MessagePack
  for compact RPC, Protobuf when interoperating with a Protobuf-defined
  service, YAML/TOML for config files, XML when you must.
- **Talking to another Hew node (same cluster):** you do not pick. The
  runtime owns this. Your message types compile through the runtime's
  CBOR envelope automatically. User code does not import `envelope.rs`.
- **Talking to a non-Hew consumer that wants to read your message
  types:** see §3 — this is consumer-interop, deferred.

### Anti-pattern: do not use `std::encoding::wire` directly

`std::encoding::wire` exposes low-level helpers (`encode_header` and
friends) that reflect the **legacy HBF** byte layout, not the current
CBOR envelope. These helpers are a migration stub pending deletion (see §5
S1 for the obsolescence trigger). New code must not reach for them.

---

## 3. Consumer interop tooling — deferred to v0.5.1+

### Decision

OpenAPI generators, `proto-gen`-style toolchains, and any other
"schema-first generator that produces Hew types from a non-Hew schema"
surface are **out of scope** for v0.5.

> **WHEN-obsolete:** when a v0.5.1+ consumer-interop effort lands. That
> effort's plan must reference this section by name and state which of
> {OpenAPI, gRPC/Protobuf service stubs, AsyncAPI, JSON Schema} it
> commits to.
>
> **WHAT-real-solution-looks-like:** a `hew gen` (or equivalent)
> command that consumes a non-Hew schema file and emits Hew type
> declarations under a stable module layout, with tests covering at
> least one round-trip from external schema → generated Hew types →
> emitted wire bytes that the external ecosystem accepts.

### Why the scope cut

- v0.5 is the substrate cutover. Re-introducing a schema-driven code
  generator (the path HBF + the retired C++/MLIR codegen represented)
  is the surface we deliberately collapsed. Adding a separate
  consumer-interop generator in the same release would re-open that
  surface under a different name.
- The existing stdlib `protobuf` module is **wire-level** (encode and
  decode the bytes), not **schema-level** (consume `.proto` files and
  emit types). The first is small and contained. The second is a
  toolchain commitment that wants its own dedicated effort.
- Until that effort exists, users with consumer-interop needs can:
  hand-author the Hew types that match the external schema, use the
  stdlib wire-level module to encode/decode them, and document the
  schema-to-Hew mapping in their own project.

### What this doctrine forbids in the interim

- Do not land partial OpenAPI / proto-gen tooling under
  `std::encoding::*` or anywhere else in the workspace as a "preview"
  surface. If it is not the effort's deliverable, it does not ship.
- Do not advertise OpenAPI / proto-gen support in user-facing docs,
  examples, or release notes for v0.5.x.

---

## 4. Anti-doctrine — do not do these things

The following patterns are known footguns. They are listed here so that
reviewers can point at this section by number rather than re-deriving
the argument each time.

### A1. Do not roll your own framing on top of CBOR

The CBOR envelope deliberately omits a redundant payload-length field;
the transport container (QUIC stream / TCP length-prefix) owns framing.
Adding a parallel length or magic-number prefix inside or around the
envelope creates two sources of truth that will drift.

> **Where this would tempt you:** writing a new transport adapter and
> deciding to "be safe" by re-prefixing. Don't. Use the existing
> framing the transport already provides.

### A2. Do not use CBOR (or `EnvelopeFrame`) in user-facing API contracts

The runtime envelope is an internal trust boundary between Hew nodes
talking to other Hew nodes. It is not a public RPC format. Exposing it
as one couples external consumers to schema decisions that the runtime
team reserves the right to evolve.

> **What to do instead:** if your service has external consumers, pick
> the stdlib encoding module that matches what those consumers speak
> (typically JSON or Protobuf), and translate at the edge.

### A3. Do not mix the two substrates in one module

A single module either talks the runtime CBOR envelope (it's runtime
code, lives under `hew-runtime/`) or it talks a user-facing wire format
(it's stdlib code, lives under `std/encoding/<format>/`). A module that
does both is a sign that the layer boundary has been crossed and should
be split.

### A4. Do not reach for `std::encoding::wire`'s legacy helpers in new code

The legacy HBF helpers in `std/encoding/wire/wire.hew` (e.g.
`encode_header`) describe the pre-v0.5 substrate that the runtime no
longer uses on the hot path. New stdlib or user code that needs wire
bytes must use the format-specific module (`json`, `msgpack`, etc.),
not these helpers. See §5 for the migration commitment.

### A5. Do not best-effort parse an unknown envelope `version`

Decoders must reject unknown `version` values. Best-effort parsing of
an unrecognised version silently couples the receiver to whatever
the next sender happens to send and defeats the schema-validation posture
the CBOR substrate buys. The version-rejection enforcement landed in
`bb8826e7` precisely to close this hole.

### A6. Do not introduce new `unwrap_or_default` / silent-`None` paths in encoders

Per the project's fail-closed codegen rule, wire encoders must return
explicit errors (e.g. `EnvelopeEncodeError::Cbor`), not paper over the
failure. The existing `EnvelopeFrame::encode` surface is the template.

---

## 5. Stubs and migration commitments

### S1. `std::encoding::wire` — legacy HBF helpers

**State today.** `std/encoding/wire/wire.hew` exposes `encode_header`
and related helpers that describe the pre-v0.5 HBF header (`"HEW1"`
magic + version + flags + length). The runtime no longer uses these
on the inter-process hot path (the CBOR envelope substrate replaced
them in `04bfb422`).

> **WHEN-obsolete:** when the user-facing surface either (a) is
> migrated to format-specific stdlib modules and these helpers can be
> deleted, or (b) is explicitly retargeted at the CBOR envelope and
> renamed accordingly. Either path closes this stub.
>
> **WHAT-real-solution-looks-like:** the `std::encoding::wire` module
> either disappears (consumers migrated, helpers deleted) or it
> documents itself as "low-level CBOR envelope access for users who
> deliberately want to talk the runtime substrate at the language
> layer" — with the same fail-closed posture the runtime decoder has
> (version rejection, truncation rejection, no silent defaults).
> Until then, the `Value` contract under `std::encoding::wire` stays;
> the legacy HBF byte helpers do not get new callers.

### S2. Cap'n Proto stub crate

**State today.** Not yet present in the workspace. The decision to defer
Cap'n Proto in favour of CBOR + CDDL is recorded in §1.

> **WHEN-obsolete:** when a future effort lands a placeholder
> `hew-codegen-capnproto` (or equivalently named) stub crate that
> compiles to a build-time error stating "Cap'n Proto codegen is not
> implemented; use the CBOR envelope substrate for inter-process
> messaging." The stub's purpose is to make the deliberate non-choice
> legible in the workspace, not to deliver Cap'n Proto support.
>
> **WHAT-real-solution-looks-like:** the stub crate exists with a
> `// JUSTIFIED: documented non-choice per HEW-WIRE-FORMAT-DOCTRINE §5`
> marker, a one-line `unimplemented!()` in its public surface, and a
> CI gate that fails if anything in the workspace builds against it.
> If a future effort decides to actually implement Cap'n Proto codegen,
> deleting that gate is the explicit signal that the doctrine is
> being revised.

### S3. Consumer-interop tooling

Already covered in §3 with `WHEN-obsolete` / `WHAT-real-solution-looks-like`
markers. Restated here so the migration-and-stubs section is a single
checklist.

### S4. Module renames / deprecations

No stdlib `std::encoding::*` modules are scheduled for rename or
deprecation in v0.5. The list in §2 is the v0.5 surface. The next
candidate for revisiting is `std::encoding::wire` per S1; that is the
only one with an active obsolescence trigger.

---

## Cross-references

- Runtime substrate provenance: commits `63a486d6`, `bb8826e7`, `04bfb422`
  on `v05-integration`.
- CDDL schema: [`hew-runtime/schemas/envelope.cddl`](../../hew-runtime/schemas/envelope.cddl).
- Runtime envelope types: [`hew-runtime/src/envelope.rs`](../../hew-runtime/src/envelope.rs).
- Round-trip tests: [`hew-runtime/tests/envelope_round_trip.rs`](../../hew-runtime/tests/envelope_round_trip.rs).
- Stdlib `Value` contract: [`std/encoding/wire/README.md`](../../std/encoding/wire/README.md), issue #1247.
- Distributed actor companion doc: [`HEW-DIST-SPEC.md`](./HEW-DIST-SPEC.md).
