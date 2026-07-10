# Hew Future Surface

This document collects language surfaces that have been **designed and
described in prose** but are **not part of the current normative
specification** (`HEW-SPEC-2026.md`). Each surface carries an explicit
version target.

Status legend:

- **[Target: v0.6]** — planned for the next minor compiler release within
  edition 2026; additive to existing programs.
- **[Target: v0.6 / gated on Cluster N]** — planned for v0.6, but depends
  on a specific compiler-internal milestone landing first.
- **[Target: v0.7+]** — further out, subject to revision.
- **[Target: v1.0+]** — long-horizon, expected to coincide with the
  stability event.
- **[Target: tracks separately]** — owned by an out-of-band roadmap
  (tooling, release-engineering, ecosystem).
- **[REJECTED]** — formally considered and not adopted.

Editions reference: a future edition (e.g. `2028`) may *promote* one or
more sections in this document into normative status. Within edition
2026, every section below is informative only.

---

## 1. Concurrency surfaces

### 1.1 Channels (`std::channel::channel`)

**[Landed in v0.5; not deferred]**

The v0.5 stdlib ships `std::channel::channel.new(capacity)` returning a
typed `(Sender<T>, Receiver<T>)` pair. `await rx.recv()` in an execution
context suspends worker-free and returns `Option<T>`; `rx.try_recv()` is
non-suspending. A channel receive can also participate in `select` as
`pat from rx.recv()`. Future channel work may still add new topology
forms, but the bounded MPSC surface itself is no longer a future item.

### 1.2 Cancellation tokens

**[Target: v0.6]**

Edition 2026 cancellation is **scope-structural only**: a `scope {}`
block cancels its children when any child fails or when the scope exits. No
user-visible `CancellationToken` type, no `Token.cancel()`, no
`#[noncancellable]` attribute as a stabilised surface (it parses today
but is gated to future-edition status).

A first-class token vocabulary lets a parent abort *one* child without
unwinding the whole scope. Useful for long-running computations the
caller can preempt mid-flight. Defer until a real need arises.

### 1.3 Actor await and read-after-send barrier (former §4.10)

**[Target: v0.6 / needs more design]**

`await actor`, `await close(actor)`, and the "awaited read acts as a
barrier" rule parse today but lack end-to-end implementation and have
not been audited against the actor mailbox protocol's failure modes.
Track under #1236.

### 1.4 Deferred `select{}` arms: stream-next and task-await

**[Target: returns with its substrate]**

Edition 2026's `select{}` is a **three-form** sealed construct: actor
ask (`<id> from <actor>.<method>(...)`), channel receive
(`<id> from <rx>.recv()`), and timer (`after <duration>`) — see
HEW-SPEC-2026 §4.11.1. Two arm forms from earlier drafts are deferred,
each blocked on a missing first-class substrate, not on the `select`
machinery (the select winner/loser-cleanup codegen seam is already live
for the shipped arms):

- **Stream-next arm** (`<id> from <stream>.recv()` over `Stream<T>`,
  binding `Option<T>`). Deferred because no usable `Stream<T>` handle can
  be obtained today: every acquisition path (`stream.pipe()` tuple
  extraction, `stream.from_file(...)?` Result extraction) trips the
  owned-handle aggregate-extraction fail-closed (`OwnedHandleAggregate*`),
  and a bare `Stream<T>.recv()` is not yet ABI-wired in codegen. Returns
  once stream-handle binding lands.
- **Task-await arm** (`<id> from await <task>`, binding `T` for
  `Task<T>`). Deferred because `Task<T>` is not nameable (§4.3) and
  `fork name = expr;` is parser-only in this build, so there is no
  bindable first-class task handle to select on. Returns with the
  `fork`/`Task` substrate.

Both forms are rejected at **check** time today (the type checker
restricts the arm set; codegen is not involved), so re-introducing them
is purely additive.

### 1.5 Supervision extras beyond ask / restart / escalation

**[Target: v0.6]**

Edition 2026 ships supervisor strategies (`one_for_one`, `one_for_all`,
`rest_for_one`), restart classifications (`permanent`, `transient`,
`temporary`), and restart-budget escalation. Surfaces beyond this —
hot-swap upgrades, dynamic strategy changes, supervisor introspection
APIs — are deferred.

### 1.6 Generators (`gen fn`, `async gen fn`, `receive gen fn`, `Lazy<T>`, `#[prefetch(N)]`)

**[Scalar-parameter + fn-typed-parameter forms live in v0.5 / remaining forms deferred]**

Zero-parameter `gen fn` functions compile and run. The LLVM coroutine machinery,
`yield`, `.next()`, and `for x in generator()` are all live. Parameterized `gen fn`
with scalar parameters (e.g. `n: i64`) and fn-typed parameters are also live — the
Cluster 1 parameter/capture lowering that unblocked these shipped and the
`gen_fn_param_capture` and `gen_fn_fn_typed_param` vertical-slice fixtures pass.

Remaining deferred:

- `async gen fn` returning `AsyncGenerator<Y>` with `for await`.
- `receive gen fn` on actors returning `Stream<Y>` backed by mailbox
  protocol (cross-actor streaming with natural backpressure).
- `Lazy<T>` for memoised one-shot computations.
- `#[prefetch(N)]` attribute as an optimisation hint on cross-actor
  generators.

### 1.7 Auto-injected `RwLock`/`Mutex`/`Atomic` wrappers

**[REJECTED]**

A "magic v0.5" sketch proposed auto-wrapping concurrently-mutated fields
with the appropriate synchronisation primitive. Reversed 2026-05-09.
Concurrent unsynchronised mutable bindings are an error in edition 2026;
the diagnostic includes a fix-it pointing at the right wrapper.

### 1.8 On-crash consuming-handler attribute for `@linear` actor fields

**[Target: v0.7+]**

Edition 2026 admits a `@linear` field on an actor only when the field
type also satisfies `@resource` semantics, so heap teardown drops it on
the supervised-crash path (HEW-SPEC-2026 §3.7.8.4, Path 3). A bare
`@linear` field whose consume can be bypassed by a crash is a compile
error today.

A future edition may relax this by introducing an opt-in attribute —
sketched as `#[on_crash = method]` on the field — that declares a
runtime-invoked consuming handler the supervisor runs *before* heap
teardown on the crash path. The handler's signature, the supervisor's
delivery guarantee, and the diagnostic that fires when the actor's
declared restart classification cannot honour the attribute are
unsettled and need design work alongside the broader supervision-extras
surface (§1.5). Defer until a real workload demands a `@linear` actor
field that is neither escalation-only nor `@resource`-backed.

---

## 2. Type-system surfaces deferred from edition 2026

### 2.1 Closures — transitive (skip-level) capture

**[Target: v0.6]**

Closure literals parse and lower for the general case: captured-state
closures, `move` captures, closure-typed record/struct fields, and
`Option<T>`-returning closures are all implemented with drop-safe
handling of the captured environment. The remaining gap is narrower —
transitive capture through two or more nested scope levels (a closure
capturing a binding from an enclosing closure that itself captured it
from a further-enclosing scope) fails with `E_HIR
CheckerBoundaryViolation`. Direct one-level closure-captures-closure
(`let wrap = |y| base(y);` where `base` is itself a closure) already
works.

### 2.2 Advanced trait surface (dyn, object safety, associated-type bounds, heavy where-clauses)

**[Target: v0.6 / gated on coherence rules]**

Edition 2026 ships a deliberately small trait surface:

- User-defined traits with associated types and named receivers.
- Trait `impl` blocks for concrete and generic types.
- Bounds on type parameters (`T: Display`).

Out of scope for edition 2026:

- Single-trait `dyn Trait` dispatch works (vtable registry is live; see
  `examples/types_and_traits.hew`). Remaining partial: object-safety
  enforcement, associated-type bounds in `dyn` position, and higher-ranked
  trait bounds.
- Associated-type bounds in where-clauses (`where T::Item: Display`).
- Trait coherence (orphan rule) across crates — the current single-crate
  coherence rule stays; multi-crate coherence depends on the package
  system maturing.
- Higher-ranked trait bounds (`for<'a> Fn(&'a T)`).

### 2.3 User-facing `Arc<T>`

**[Target: v0.7]**

The runtime contains ABI-level atomic refcount machinery. The
**user-visible** `Arc<T>` surface — including `Arc::new`, `Arc::clone`,
and the cross-actor sharing rule — is not stabilised in edition 2026.
The intended invariant is that only deeply-immutable (`Frozen`) data is
shareable across actors; until that surface lands, cross-actor sharing
is via owned messages and actor state.

### 2.4 `DoubleEndedIterator`

**[Target: v0.6 / Cluster 6]**

Edition 2026 ships the lazy `Iterator`/`IntoIterator` trait hierarchy
(`std/builtins.hew`) with chainable adapter types — `Map`, `Filter`,
`Take`, `Skip` (`std/iter.hew`) — and terminals (`fold`, `count`,
`collect`, `any`, `all`, `sum`, `product`) over `Vec`, `HashMap`,
`Generator`, and `AsyncGenerator`. The earlier eager per-type helper
table (`map_int`, `filter_int`, `fold_int`, ...) has been retired. The
only member of the hierarchy still absent is `DoubleEndedIterator`
(back-to-front adapters like `.rev()`), which is deferred to Cluster 6's
stdlib port-forward.

### 2.5 Generic `HashMap<K, V>` over arbitrary `K`

**[Target: v0.6 / Cluster 6]**

Edition 2026 admits `HashMap<K, V>`/`HashSet<K>` keys for any
structural-hash-eligible `K` — scalars, strings, and records/enums whose
leaves are hash-eligible (structural hashing descends string fields via
FNV-1a, the hash twin of structural equality). The remaining gap is
narrower than "generic keys": keys containing an owned `Vec`/`bytes`
field, and float-typed keys, are rejected with a diagnostic (not a
fixed-size-`Copy` shape). Lifting that gap lands with the stdlib port in
Cluster 6.

---

## 3. Standard-library surfaces deferred from edition 2026

The edition 2026 stdlib is deliberately narrow — see HEW-SPEC-2026
§3.10. The following modules exist in `std/` today but are not part of
the edition 2026 normative surface; their inclusion in the next edition
is tracked per module.

**[Target: v0.6+ / per module]**

- `std::net::dns` — DNS resolution.
- `std::net::tls` — TLS streams.
- `std::net::quic` — QUIC transport.
- `std::net::websocket` — WebSocket framing.
- `std::encoding::xml`, `::yaml`, `::toml`, `::csv` — encoding modules
  beyond JSON and MessagePack.
- `std::text::regex` — regular expressions as first-class values
  (`regex.Pattern` is already a `#[resource]`-annotated RAII handle —
  the calibration pilot landed; promoting `std::text::regex` itself to
  the edition 2026 normative surface is part of the next edition's
  stdlib port).
- `std::process` — child-process spawning.
- `std::encoding::compress` — gzip/deflate/zlib decompressors.

Each module above has working in-tree code; the deferral is **status**,
not implementation. Programs that import them compile today; the spec
does not yet make normative guarantees about their stability.

---

## 4. Tooling, runtime, and distribution

### 4.1 Tooling specification (LSP, debugger, profiler)

**[Target: tracks separately]**

`hew debug`, `HEW_PPROF`, `hew-observe`, and the LSP surface all have
working implementations and changelog histories, but their behavioural
guarantees do not belong in the language specification. They are tracked
in `docs/observe.md`, `docs/troubleshooting.md`, `docs/dev/lsp-editor-setup.md`,
and the per-component documentation; future specification work will
reference them rather than duplicate them here.

### 4.2 Distributed computing and the Node API

**[Target: v0.7+ / depends on wire, actors, auth all settled]**

Earlier drafts described `node.spawn`, cross-node registry gossip, QUIC
transport, and remote-message dispatch as a stable surface. Distribution
sits at the intersection of three subsystems that need to be stable
first — wire types (settling), actors (stable), and an authentication
story (not yet started). The Node API as a normative surface waits for
all three.

The runtime contains a working `sim-transport` for property testing
distributed scenarios; the simulator does not commit to the source-level
surface.

---

## 5. Long-horizon

### 5.1 Self-hosting roadmap

**[Target: v1.0+]**

Bootstrap chain, minimum viable subset for self-hosting, kernel-language
concept, WASM as portable bootstrap format. The self-hosting work is a
post-stability project; the specification commitment to a kernel
language constrains v1.0+ designs in ways that need v1.0 in hand first.

---

## Notes on promotion

A surface moves out of this document when:

1. It has a working implementation in the compiler and runtime.
2. It has been audited against the rest of the language for coherence.
3. The maintainers agree it is part of the normative language, not a
   library or tooling addition.

Promotion lands in the next edition (`HEW-SPEC-2028.md`), not as an
edition-2026 amendment. Edition stability means edition 2026's prose
does not change once stabilised; new surfaces wait for the next edition.

Within a single edition, **additive** stdlib surfaces and **additive**
compiler features (new optimisations, new diagnostics, soundness fixes)
land without an edition bump. Anything that would reject previously-
accepted code on an old edition is an edition-breaking change and waits.
