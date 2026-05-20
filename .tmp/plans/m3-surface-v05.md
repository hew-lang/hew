# M3-Surface — v0.5-absorbable subset of M3 networking (Cap'n Proto edition)

## Status: Superseded — cut from v0.5 scope

Cut by `.tmp/orchestration/v05-strategy-consult-2026-05-19.md:72–79`. Defer to
v0.6.

**Lane id**: `m3-surface-v05`
**Status**: dispatchable (two open Q-tags remain — §Q-HBF-RETIRE and §Q-CAPNP-RPC; one prior Q-tag §A/§B ratified 2026-05-17)
**Recommended executor**: mixed — `hew-implementer-opus` for cross-IR seam slices (S1, S3a, S3b, S4) and the HBF retirement audit (S0); `hew-implementer` (Sonnet) for substrate-exposure slices (S2, S5, S6, S7)
**Cross-ecosystem reviewer**: Opus cross-eco for every slice crossing Rust↔stdlib/types↔MIR
**Worktree**: `/Users/slepp/projects/hew-lang/hew/.claude/worktrees/v05-integration` (tip `35b60a9d`)
**Composition**: parallel-safe with G-1.b / G-2.c / TO-2 / extern-rt / RB-* (touches `hew-runtime/`, `hew-types/`, `hew-mir/`, `std/encoding/` — disjoint surfaces).

## Framing

v0.5 commits to **Cap'n Proto as the public cross-process wire**. Reasoning (ratified 2026-05-17): zero-copy reads align with Hew's affine-types + arena model; built-in RPC story matches actor-tell semantics; schema-first with explicit field-number evolution rules; interop-grade with non-Hew peers. HBF — the in-tree custom TLV (`HEW1` magic, version byte, varint/zigzag — `hew-runtime/src/wire.rs`) — is **retired as the public contract** and either deleted or demoted to an internal same-machine fastpath (Q-HBF-RETIRE).

msgpack is **not** the v0.5 wire. msgpack stays as one user-facing stdlib serializer (`std/encoding/msgpack/`) alongside JSON/YAML/CBOR/protobuf for Hew programs that want to expose APIs in those formats. The compiler does not depend on msgpack as wire infrastructure.

```
User code: tell(some_pid, msg) — same call, any peer

  runtime: hew_routing_is_local(pid)?
    ├── 1 → today's mailbox.rs path (hew_actor_send_by_id)
    └── 0 → TransportActor
         • Cap'n Proto encode (auto-generated from Hew `@wire` declarations)
         • write to wire (TCP/QUIC; capnp 4-byte LE message-length framing)
         • ← Cap'n Proto reader on the peer (zero-copy)
         • dispatch via msg_type discriminator to destination mailbox
```

---

## Substrate audit (what already exists — verified read-only)

| Concern | File | Lines | v0.5 disposition |
|---|---|---|---|
| Distributed node runtime | `hew-runtime/src/hew_node.rs` | 4,912 | **MIGRATE**: keep as transport-actor orchestrator; replace HBF-encode call sites with Cap'n Proto encoder. |
| SWIM cluster membership | `hew-runtime/src/cluster.rs` | 3,191 | **KEEP INTERNAL** (not user-callable in v0.5). If still using HBF-shaped envelopes internally, migrate framing only. |
| HBF wire format (TLV/varint/zigzag) | `hew-runtime/src/wire.rs` | 2,430 | **RETIRE or DEMOTE** (see §S0 / Q-HBF-RETIRE). Public ABI symbols `hew_wire_encode_*` / `hew_wire_decode_*` are removed or namespaced as `hew_internal_*`. |
| Transport vtable + TCP/Unix | `hew-runtime/src/transport.rs` | 2,283 | **KEEP framing layer** (4-byte LE length prefix is identical to capnp's `serialize` framing) but swap the body codec. |
| QUIC transport | `hew-runtime/src/quic_transport.rs` | 1,488 | Keep; framing unchanged. |
| Sim transport (tests) | `hew-runtime/src/sim_transport.rs` | 1,739 | Tests using HBF byte fixtures must be ported to capnp fixtures. |
| Per-node connection mgr | `hew-runtime/src/connection.rs` | 3,232 | Keep; HBF references audited and replaced. |
| Routing table | `hew-runtime/src/routing.rs` | 512 | Keep unchanged — pid encoding is wire-independent. |
| Pid encoding | `hew-runtime/src/pid.rs` | 173 | Keep unchanged. |
| Remote supervisor scaffolding | `hew-runtime/src/remote_sup.rs` | 2,613 | OUT-OF-SCOPE for v0.5 surface (v0.6). Internal HBF references retired alongside `wire.rs`. |
| Duplex substrate | `hew-runtime/src/duplex.rs` | 2,413 | Keep; S5 adds TCP attach. Duplex is bytes-level — no wire-codec coupling. |
| Stream/Sink | `hew-runtime/src/stream.rs` | 3,970 | Keep; S5 may add socket-backed Stream. |

**Total existing substrate**: ~28,956 LOC (verified by `wc -l`).
**HBF-touching files (confirmed via grep on `HBF\|HEW1\|hbf_`)**: `hew-runtime/src/{wire,transport,quic_transport,connection}.rs` — four files. Removal is bounded and tractable; this is **not** a 24K-LOC rewrite.

`pub extern "C" hew_wire_*` symbols in `wire.rs:156-770`: 23 symbols. All are internal-runtime ABI (no Hew-source consumer); removal is a runtime-internal change. Verified by `grep "hew_wire_" hew-mir hew-types std` returning zero hits.

---

## Evidence

- **E-1** (substrate exists; exposure is wiring not building):
  `hew-runtime/src/routing.rs:135-178` already classifies pids by node id (`hew_routing_lookup` + `hew_routing_is_local`).
- **E-2** (pid encoding stable):
  `hew-runtime/src/pid.rs:53-90`. `hew-types/` has zero refs to `RemotePid`/`LocalPid` — clean slate.
- **E-3** (HBF is the ONLY current wire — it has no public users):
  `hew-runtime/src/wire.rs:34` `HBF_VERSION: u8 = 0x01`; line 35 `HBF_MAGIC = *b"HEW1"`. 23 `pub extern "C" hew_wire_*` symbols. None referenced from `hew-mir`, `hew-types`, or `std/` (verified by grep). Retirement is a runtime-internal change.
- **E-4** (transport-actor is implicit):
  `hew-runtime/src/hew_node.rs:1737` `pub unsafe extern "C" fn hew_node_send` and `hew_node.rs:824-832` dispatch via `hew_actor_send_by_id` after routing lookup. S2 formalises naming.
- **E-5** (Duplex has no socket attach yet):
  `hew-runtime/src/duplex.rs:377` doc-comment cites "§8.3 Duplex-as-TCP-connection design" but `attach_to_tcp` is absent. `std/stream.hew` exposes only `pipe`, `bytes_pipe`, `from_file`, `to_file`.
- **E-6** (`@wire` is already a Hew attribute on struct types, not a top-level keyword):
  `hew-parser/src/parser.rs:1362, 1452, 3275, 6569` — `wire` is parsed as an attribute slot on `struct` declarations alongside `resource`, `linear`, `json`, `yaml`. Codegen for `@wire` structs lives implicitly via `std/encoding/wire/` (`wire.hew`, `value_trait.hew`). **This is the natural attach point for Cap'n Proto schema generation** — same parser surface, new codegen backend.
- **E-7** (msgpack already exists as a peer of JSON/protobuf/yaml in stdlib):
  `std/encoding/{msgpack,protobuf,json,yaml,cbor}/`. msgpack's role is unchanged by this plan.
- **E-8** (capnp Rust binding is stable):
  Canonical crate is `capnp` 0.20+ with `capnpc` for codegen; RPC layer is `capnp-rpc`. Mature, BSD-licensed, sole-maintained-by-author-of-protobuf2 (Kenton Varda).

---

## LESSONS triggers

- **P0 `boundary-fail-closed`** — Cap'n Proto decoder must reject malformed or future-schema messages with explicit diagnostic; never silent-skip. Applies to S3b (decoder) and S4 (`hew_actor_send_remote` peer-decode path).
- **P0 `producer-bridge-before-codegen`** — S1 produces `LocalPid<T>` / `RemotePid<T>` discriminators; S4 is the MIR consumer. S1 must land with a producer-emit test before S4 dispatches.
- **P0 `checker-output-boundary`** — Locality is checker-resolved; MIR lowering reads the discriminator, never re-infers from runtime pid bits.
- **P0 `ffi-ownership-contracts`** — New ABI surface (`hew_capnp_*`, `hew_actor_send_remote`) must document ownership of payload bytes. Cap'n Proto's segment-based reader needs explicit lifetime mapping to Hew's arena.
- **P0 `generated-narrowing-guards`** — Cap'n Proto codegen emits readers that return `Result<T, capnp::Error>` for every field. The Hew-side wrapper must propagate, not unwrap. Tested in S3b.
- **P1 `behavioral-regression-not-just-test-pass`** — S4 routing change shifts the error class for `tell(remote_pid, msg)` from "unknown actor" to "transport error". Implementer worker-return must document this transition.
- **P2 `dedup-semantic-boundary`** — Do not collapse `LocalPid<T>` / `RemotePid<T>` at lowering. Carry the discriminator forward.

(No new LESSONS rows proposed yet. The Cap'n Proto adoption may yield one P1 row on "schema-versioning via reserved field numbers" — defer to closeout.)

---

## Open questions for user ratification (Q-tags)

### §Q-HBF-RETIRE — clean break vs staged-as-internal-fastpath

Concrete LoC counts (verified):

- **`hew-runtime/src/wire.rs`**: 2,430 LOC. 23 `pub extern "C" hew_wire_*` symbols. Zero callers outside the runtime crate. Pure deletion candidate.
- **HBF-touching call sites** in `hew-runtime/src/{transport,quic_transport,connection}.rs`: confirmed by grep on `HBF\|HEW1\|hbf_` — exactly 4 files total. Migration is **localised to the runtime crate**, not 24K LOC.
- **Tests using HBF byte fixtures** (sim transport + wire unit tests): ~600 LOC by inspection (`sim_transport.rs:1700-1739` and `wire.rs` test module). Must port to capnp fixtures.

**Option A — Clean break (RECOMMENDED for v0.5 quality)**:
Delete `wire.rs` entirely (2,430 LOC). Replace HBF envelope encode/decode in 3 sites in `transport.rs` / `quic_transport.rs` / `connection.rs` with capnp envelope. Port ~600 LOC of test fixtures. Total work: ~3,000 LOC touched, of which ~2,400 is deletion.

- *Pros*: one wire codec, one mental model, no "which framing is this?" branches in transport. Honors "quality over time" — v0.5 ships with one coherent story.
- *Cons*: capnp adds a hot-path encode cost vs HBF's hand-tuned varint on same-machine peers. If profiling shows ≥10% throughput regression on local-cluster benchmarks, we lose perf without recourse.
- *Mitigation*: capnp's `serialize_packed` and the unpacked-segment fastpath are both available; preliminary capnp benchmarks suggest equivalence within 5% for messages <4KB. Probe in S0.

**Option B — Staged (HBF as internal same-machine fastpath)**:
Keep `wire.rs` but rename HBF symbols to `hew_internal_hbf_*`, mark `pub(crate)`, and use HBF only when both endpoints are on `localhost` / Unix domain sockets / shm. Cross-machine always uses capnp.

- *Pros*: preserves perf on local clusters; gives an exit if capnp surprises us in production.
- *Cons*: two codecs, two evolution stories, two test matrices. Violates the "quality over time" directive. The branch at `transport.rs` becomes load-bearing forever.

**Recommendation: Option A (clean break)**. The perf case for HBF is speculative; the maintenance case for one codec is concrete. Cap'n Proto's zero-copy reader is likely as fast or faster than HBF's TLV walk on the decode side. If S0's probe surfaces a real regression, escalate then.

**Q-HBF-RETIRE**: confirm clean break (Option A), or instruct staged (Option B)?

### §Q-CAPNP-RPC — use Cap'n Proto's RPC layer for actor-tell, or just serialization?

Cap'n Proto's RPC layer (`capnp-rpc` crate) provides:
- Promise pipelining (chained calls without round-trips — `foo.bar().baz()` is one network round-trip).
- Capability-based security (object references are themselves serializable and revocable).
- Bidirectional streams over a single connection.

Actor semantics in Hew today:
- `tell(pid, msg)` — fire-and-forget. No reply channel needed in v0.5.
- `ask(pid, msg)` — request-reply. Explicitly OUT-OF-SCOPE for v0.5 remote (existing reply-channel substrate is internal).
- No promise pipelining surface in Hew (no `.then()` on tell).

**Option A — Just serialization (RECOMMENDED for v0.5 scope)**:
Use `capnp` for encoding/decoding only. Frame envelopes ourselves (the existing 4-byte LE length prefix in `transport.rs` is compatible). Dispatch is our `msg_type` discriminator → mailbox enqueue. No `capnp-rpc` dependency.

- *Pros*: tighter v0.5 scope; matches actor-tell semantics directly; one less dependency; capnp's RPC layer is opinionated about call/return semantics that don't match tell. We can always layer `capnp-rpc` on top later for `ask`.
- *Cons*: when v0.6 adds remote-`ask`, we either reinvent reply-channel-over-wire (likely) or adopt `capnp-rpc` then with retroactive surface churn.

**Option B — Adopt `capnp-rpc` end to end**:
Each Hew `actor` declaration generates a capnp `interface`. `tell` becomes a one-way RPC call (capnp supports `method @0 () -> ()` returning immediately). `ask` becomes a normal RPC call.

- *Pros*: free promise-pipelining if Hew ever surfaces it; free capability story (revocable pids); cleaner v0.6 `ask` story; the schema is more semantically complete.
- *Cons*: `capnp-rpc` enforces call/return shape that complicates "fire-and-forget tell with no ack"; capability-passing semantics may conflict with Hew's affine types; larger v0.5 surface area; more interop surprises.

**Recommendation: Option A for v0.5; revisit `capnp-rpc` for v0.6 ask**.
This keeps v0.5 small and lets us validate capnp serialization without coupling to its RPC opinions. The schema we emit in S3a is forward-compatible with later adoption: a struct-based envelope can be wrapped in a capnp interface later without rewriting clients.

**Q-CAPNP-RPC**: confirm serialization-only for v0.5 (Option A), or instruct full RPC adoption (Option B)?

---

## Staged execution order

Strict ordering: S0 → S1 → S2 → S3a → S3b → S4 → S5 → S6 → S7. S2 can overlap S1's review window (doc/rename). S6 can overlap S5 (examples).

### S0 — HBF retirement audit + capnp dependency wiring (Opus)

**Pre-requisite**: §Q-HBF-RETIRE answered.

**Files**:
- `hew-runtime/Cargo.toml` — add `capnp = "0.20"`, `capnpc = "0.20"` (build-dep for codegen).
- `.tmp/audits/m3-hbf-retirement.md` (local-only, not committed) — exhaustive grep of `HBF\|HEW1\|hew_wire_\|hbf_` across the tree, with disposition per hit.
- Probe: `hew-runtime/benches/wire_codec_compare.rs` (new bench) — capnp vs HBF encode/decode for 64B, 1KB, 16KB envelopes. **Decision gate**: if capnp is >15% slower on any size, escalate Q-HBF-RETIRE back to user before continuing.

**Substrate touched**: `Cargo.toml` only. No prod code yet.

**Tests**: bench is the test surface. No unit tests.

**Validation**: `cargo bench -p hew-runtime --bench wire_codec_compare`, `make ci-preflight`.

**Out-of-slice**: no codec swap yet.

### S1 — `LocalPid<T>` / `RemotePid<T>` / `Pid<T>` trait in checker (Opus)

Identical to prior plan (Q-PID ratified). Files:

- `hew-types/src/builtin_names.rs` — register `LocalPid`, `RemotePid`, `Pid`.
- `hew-types/src/stdlib.rs`, `hew-types/src/ty.rs` — add `Ty::LocalPid(Box<Ty>)`, `Ty::RemotePid(Box<Ty>)`; `Pid<T>` trait entry.
- `hew-types/src/method_resolution.rs` — `Pid<T>` trait dispatch.
- `hew-hir/src/lower.rs` — propagate discriminators through HIR.
- `std/builtins.hew` — `type LocalPid<T>`, `type RemotePid<T>`, `trait Pid<T> { fn id(self) -> int }`.

**Tests**: `hew-types/tests/m3_surface_pid_types.rs`, `pid_trait.rs`, `hew-hir/tests/m3_surface_pid_lower.rs`.

**Validation**: `cargo test -p hew-types pid_types pid_trait`, `cargo test -p hew-hir pid_lower`, `make lint`.

### S2 — TransportActor formalisation (Sonnet)

Doc + rename only. `hew-runtime/src/{hew_node,transport}.rs` headers + module docs. No behaviour change. Validation: `make ci-preflight`.

### S3a — Cap'n Proto schema generation from Hew `@wire` declarations (Opus)

**Pre-requisite**: §Q-CAPNP-RPC answered. S0 complete.

**Files**:
- `hew-codegen/src/wire_capnp.rs` (new) — emit `.capnp` schema files from `@wire struct` declarations parsed in `hew-parser/src/parser.rs:1362,1452,3275`.
- `hew-codegen/src/lib.rs` — register the new emitter alongside existing `@wire`/`@json`/`@yaml` paths.
- Type-mapping table (documented inline + in `docs/wire-format-v05.md`):
  | Hew type | Cap'n Proto type | Notes |
  |---|---|---|
  | `int` (i64) | `Int64` | Direct. |
  | `uint` | `UInt64` | Direct. |
  | `float` (f64) | `Float64` | Direct. |
  | `bool` | `Bool` | Direct. |
  | `string` | `Text` | UTF-8 enforced. |
  | `bytes` | `Data` | Raw. |
  | `T?` (option) | wrapped in group `{ has @0 :Bool; value @1 :T; }` | capnp has no native option; idiomatic pattern. |
  | `[T]` (list) | `List(T)` | Direct. |
  | `{K: V}` (map) | `List(group { key @0 :K; value @1 :V; })` | capnp idiom. |
  | enum (sum type) | `union` inside a struct | Field numbers from declaration order. |
  | `RemotePid<T>` | `Pid` (struct with `nodeId @0 :UInt16; serial @1 :UInt64;`) | Generic param `T` is schema-time only; not encoded. |
- `wire/` convention: generated `.capnp` schemas live at `target/wire/<package>/<type>.capnp`. Stable schemas users want to commit can be copied to project's `wire/` dir. **Document in `docs/wire-format-v05.md`.**
- `@wire(version = N)` attribute (existing parser surface) maps to capnp's documented evolution discipline (reserved field numbers, never-reuse rule).

**Substrate touched**: `hew-codegen/` only. Runtime unchanged in this slice.

**Tests** (new):
- `hew-codegen/tests/m3_capnp_schema_emit.rs` — `@wire struct Foo { a :int, b :string }` emits the expected `.capnp` text.
- `hew-codegen/tests/m3_capnp_schema_evolution.rs` — adding a field with a new field number produces a forward-compatible schema (old schema decodes new payloads, ignoring new field).
- `hew-codegen/tests/m3_capnp_schema_field_reuse_rejected.rs` — reusing a field number for a new type is a compile error.

**Validation**: `cargo test -p hew-codegen m3_capnp`, `make ci-preflight`.

**Out-of-slice**: no runtime use of the generated schema yet. That's S3b.

### S3b — Runtime envelope codec swap: HBF → Cap'n Proto (Opus)

**Pre-requisite**: S0 (capnp dep), S3a (schema emission), §Q-HBF-RETIRE answered.

**Files (Option A — clean break)**:
- `hew-runtime/src/wire.rs` — **DELETE**. Replace with new `hew-runtime/src/capnp_wire.rs` (~400 LOC expected) implementing the envelope-encode / envelope-decode API consumed by transport/connection. Envelope schema in capnp:
  ```capnp
  struct Envelope @0xabc... {
    targetActorId @0 :UInt64;
    sourceActorId @1 :UInt64;
    msgType       @2 :UInt32;
    schemaVersion @3 :UInt16;
    payload       @4 :Data;  # opaque to envelope; decoded per msgType
  }
  ```
- `hew-runtime/src/transport.rs` — replace HBF envelope calls with `capnp_wire` calls. Framing (4-byte LE length prefix) unchanged.
- `hew-runtime/src/quic_transport.rs` — same swap.
- `hew-runtime/src/connection.rs` — same swap.
- `hew-runtime/src/sim_transport.rs` — port test fixtures.
- `hew-runtime/Cargo.toml` — capnp already added in S0.

**Files (Option B — staged)**: same plus retain `wire.rs` renamed `internal_hbf.rs` and a routing branch in `transport.rs` keyed on locality.

**Fail-closed paths** (P0 boundary-fail-closed):
- Envelope decode with `schemaVersion > KNOWN_MAX` → `HEW_ERR_WIRE_VERSION`. No silent skip.
- Unknown `msgType` → `HEW_ERR_SERIALIZE`. No silent drop.
- capnp's own `MessageReader::get_root()` errors → propagate as `HEW_ERR_SERIALIZE`.

**Tests** (new in `hew-runtime/tests/m3_capnp_envelope/`):
- `decode_rejects_future_schema_version`
- `decode_rejects_unknown_msg_type`
- `encode_decode_roundtrip_envelope_v05` — pin v0.5 envelope on disk as a hex fixture.
- `interop_with_external_capnp_reader` — write an envelope, decode it with a standalone capnp Python or C++ reader if available in CI; otherwise validate against a hand-decoded byte sequence.

**Validation**: `cargo test -p hew-runtime m3_capnp_envelope`, `cargo bench -p hew-runtime --bench wire_codec_compare` (regression gate from S0), `make ci-preflight`.

**Out-of-slice**: no compression, no TLS, no schema registry (v0.6).

### S4 — `tell()` routing on `RemotePid<T>` (Opus)

**Pre-requisite**: S1, S3a, S3b merged.

**Files**:
- `hew-mir/src/runtime_symbols.rs` — add `hew_actor_send_remote`.
- `hew-mir/src/lower.rs` — branch on `LocalPid` / `RemotePid` discriminator at `tell` lowering.
- `hew-runtime/src/actor.rs` (or `hew-runtime/src/remote_send.rs` new file) — `pub unsafe extern "C" fn hew_actor_send_remote(pid, msg_type, payload, len) -> c_int`. Calls `hew_routing_lookup` → conn-mgr → `capnp_wire::encode_envelope` → transport write. Fail-closed on unreachable peer (`HEW_ERR_TRANSPORT`).
- `std/builtins.hew` — `tell` signature accepts `impl Pid<T>`.

**Tests** (new):
- `hew-runtime/tests/m3_remote_send_unknown_node.rs`
- `hew-runtime/tests/m3_remote_send_disconnected.rs`
- `hew-runtime/tests/m3_remote_send_sim.rs` — two sim nodes exchange a typed message via `RemotePid<PingMsg>`.
- `hew-mir/tests/m3_lower_remote_tell.rs` — Hew source `tell(remote_pid, msg)` lowers to `hew_actor_send_remote`.

**Validation**: `cargo test -p hew-runtime m3_remote_send`, `cargo test -p hew-mir m3_lower_remote_tell`, `make ci-preflight`.

### S5 — Stream/Sink/Duplex attach to TCP (Sonnet)

Unchanged from prior plan. Files: `hew-runtime/src/duplex.rs` (new `hew_duplex_attach_tcp`), `std/stream.hew` (`from_tcp`, `listen_tcp`). Bytes-level framing only — not coupled to capnp envelope (Duplex is user-defined byte streams). Tests: `m3_duplex_tcp_roundtrip`, `m3_duplex_tcp_disconnect`. Validation: `make playground-check`, `make ci-preflight`.

### S6 — msgpack role clarification + stdlib doc (Sonnet)

**Files**:
- `std/encoding/msgpack/README.md` — affirm msgpack is a user-facing serializer, parallel to JSON/YAML/protobuf. **Not** Hew's wire protocol.
- `std/encoding/wire/README.md` — explain `@wire` declarations generate Cap'n Proto schemas; cite `docs/wire-format-v05.md`.
- `docs/wire-format-v05.md` (created in S3a) — add an FAQ section explaining the msgpack-vs-wire distinction.

**Substrate touched**: docs only.

**Validation**: `make playground-check`, `make lint`.

### S7 — Examples corpus (Sonnet, optional)

`examples/m3_surface/` — echo server/client (Duplex+TCP); chat server/client (typed actors over capnp wire with `RemotePid<ChatMsg>` deserialized from a handshake). Tests via `make playground-check`.

---

## Validation candidates (full slice)

- `cargo bench -p hew-runtime --bench wire_codec_compare` (S0, gate at ≥15% regression)
- `cargo test -p hew-types --test m3_surface_pid_types` (S1)
- `cargo test -p hew-types --test m3_surface_pid_trait` (S1)
- `cargo test -p hew-hir --test m3_surface_pid_lower` (S1)
- `cargo test -p hew-codegen m3_capnp_schema_emit` (S3a)
- `cargo test -p hew-codegen m3_capnp_schema_evolution` (S3a)
- `cargo test -p hew-codegen m3_capnp_schema_field_reuse_rejected` (S3a)
- `cargo test -p hew-runtime m3_capnp_envelope::decode_rejects_future_schema_version` (S3b)
- `cargo test -p hew-runtime m3_capnp_envelope::decode_rejects_unknown_msg_type` (S3b)
- `cargo test -p hew-runtime m3_capnp_envelope::encode_decode_roundtrip_envelope_v05` (S3b)
- `cargo test -p hew-runtime --test m3_remote_send_sim` (S4)
- `cargo test -p hew-mir --test m3_lower_remote_tell` (S4)
- `cargo test -p hew-runtime --test m3_duplex_tcp_roundtrip` (S5)
- `make playground-check` (S5, S6, S7)
- `make ci-preflight` — mandatory before every push.

---

## Risks

1. **capnp perf regression vs HBF on local-cluster hot path**. Mitigation: S0's bench is the decision gate. If we cross 15%, Q-HBF-RETIRE is revisited.
2. **`hew_wire_*` ABI deletion breaks an unknown external consumer**. Mitigation: `grep` confirmed zero in-tree consumers; the symbols are runtime-internal. Out-of-tree consumers don't exist (v0.5 is unreleased).
3. **capnp schema-versioning discipline is operator-facing**. The "never reuse field number" rule is enforced by the codegen test in S3a; reviewers must enforce in PR review for new `@wire` types.
4. **Cap'n Proto generic-type erasure**. `RemotePid<T>`'s `T` is checker-only — encoded `Pid` is just `{nodeId, serial}`. Type-safety relies on the `msg_type` discriminator binding to a checker-resolved Hew type. Document in `docs/wire-format-v05.md`.
5. **Forward-compat fail-closed on unknown msg_type**. Same trade-off as before: v0.5.0→v0.5.1 with new msg type breaks the older peer's decode. Documented; release notes must announce.
6. **S4 behavioural regression** (`tell` on a non-local pid that previously errored as "unknown actor" now becomes transport error or remote success). Same as prior plan; implementer must test pre/post explicitly.
7. **Codegen surface area**. S3a is the largest single addition (capnp schema emitter). Risk of bugs in the type-mapping table. Mitigation: each row gets a roundtrip test.
8. **Toolchain bloat**. `capnp` Rust crate pulls `capnp-futures` transitively (small). `capnpc` is a build-dep only, doesn't ship in user binaries. Validate by `cargo tree -p hew-runtime` in S0.

---

## Out of scope (drift-prevention)

1. Distributed link/monitor (v0.6).
2. Partition policies / quorum (v0.6).
3. Cluster membership / node discovery as user API (v0.6).
4. TLS / mutual auth / capability tokens on the wire (v0.6).
5. Hot cluster reconfig / consensus (v0.6+).
6. `ask` (request-reply) on `RemotePid<T>` (v0.6). Workaround: peer-back-tell.
7. `spawn` returning `RemotePid` (v0.6).
8. QUIC Duplex (S5 is TCP only).
9. Compression flag on envelopes (capnp's `serialize_packed` available but unused for v0.5).
10. **Adopting `capnp-rpc`** (v0.6 candidate; see Q-CAPNP-RPC).
11. **Per-message schema registry** (capnp's `Schema` reflection API). v0.5 binds `msg_type` to a hardcoded Hew type at codegen; dynamic schema is v0.6.
12. **msgpack as the wire**. Explicitly rejected per user decision 2026-05-17. msgpack stays a stdlib serializer.
13. **Keeping HBF as a public symbol**. Even under Q-HBF-RETIRE Option B, HBF is demoted to `pub(crate)` — no public ABI.

---

## Handoff

| Slice | Executor model | Reviewer | Expected commits | Notes |
|---|---|---|---|---|
| S0 | `hew-implementer-opus` | Opus cross-eco | 1-2 | Audit + bench gate. |
| S1 | `hew-implementer-opus` | Opus cross-eco | 2-3 | Type-system seam; producer-bridge gate. |
| S2 | `hew-implementer` (Sonnet) | Opus | 1 | Doc + rename only. |
| S3a | `hew-implementer-opus` | Opus cross-eco | 2-3 | Codegen surface; type-mapping table. |
| S3b | `hew-implementer-opus` | Opus cross-eco | 2-3 | Runtime codec swap; deletes `wire.rs` under Option A. |
| S4 | `hew-implementer-opus` | Opus cross-eco (MIR↔runtime) | 2-3 | New C ABI; behavioural-regression risk. |
| S5 | `hew-implementer` (Sonnet) | Opus | 2 | Substrate exposure. |
| S6 | `hew-implementer` (Sonnet) | Sonnet | 1 | Docs. |
| S7 | `hew-implementer` (Sonnet) | Sonnet | 1 | Examples. |

**Total expected commits**: 14-19.
**Revised slice count**: 9 slices (S0, S1, S2, S3a, S3b, S4, S5, S6, S7).
**Calendar estimate**: 4-6 weeks alongside generics + trait-object lanes — wider than the prior HBF-freeze plan because S3a (capnp codegen) and S3b (runtime codec swap) replace the single 2-commit S3.

**Co-author trailer**: forbidden in this repo. Do not add `Co-authored-by:` lines.

**Translation rule**: commits/PRs use feature-focused language only — e.g. "feat(runtime): adopt Cap'n Proto for cross-process actor envelopes" / "refactor(runtime): retire HBF wire format". No "lane/wave/phase/.tmp/plans/" voicing.

---

## Cross-slice composition note

Touches: `hew-runtime/src/{capnp_wire(new),transport,quic_transport,connection,sim_transport,actor,duplex}.rs`, `hew-types/src/{builtin_names,stdlib,ty,method_resolution}.rs`, `hew-hir/src/lower.rs`, `hew-mir/src/{runtime_symbols,lower}.rs`, `hew-codegen/src/wire_capnp.rs (new)`, `std/{builtins,stream}.hew`, `std/encoding/{wire,msgpack}/README.md`, `docs/wire-format-v05.md (new)`, `hew-runtime/Cargo.toml`.

Disjoint from G-1.b / G-2.c / TO-2 / extern-rt / RB-*. `hew-mir/src/runtime_symbols.rs` and `hew-runtime/Cargo.toml` are the only shared files; both are append-only.

Dispatch order: S0 first (decision gate). S1/S2/S3a parallel after S0. S3b after S3a. S4 after S1+S3b. S5/S6/S7 after S3b.

---

## Dispatch-readiness checklist

- [x] Evidence cites concrete artefacts (file:line) — §Evidence E-1 through E-8.
- [x] Validation candidates name exact commands — §Validation candidates.
- [x] Staged execution order has numbered slices, each commit-sized — §S0-S7.
- [x] LESSONS triggers cite row ids — §LESSONS triggers (5 P0 + 1 P1 + 1 P2).
- [x] Out-of-scope section closes drift doors — §Out of scope (13 doors).
- [x] Q-tags surfaced — §Q-HBF-RETIRE, §Q-CAPNP-RPC (Q-PID and Q-WIRE ratified 2026-05-17).

**Status**: dispatchable after Q-HBF-RETIRE + Q-CAPNP-RPC answered. If Q-HBF-RETIRE is Option B (staged), S3b's deletion shrinks to a demotion-and-namespace rename and the plan grows a routing-branch responsibility in `transport.rs`. If Q-CAPNP-RPC is Option B (full RPC), S3a's codegen also emits `interface` definitions per `actor` declaration and S4's runtime symbol path consumes `capnp-rpc` infrastructure instead of a bare envelope writer — adds 1-2 slices.
