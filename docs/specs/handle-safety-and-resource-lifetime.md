# Handle safety and resource lifetime

**Status:** Accepted design spec. Records the durable language invariant for
owned, FFI-backed resource handles and the staged work that earns it. The
stop-the-bleeding, calibration, and runtime-scope-cleanup tracking issues
(#1228, #1251, #1252, #1295, #1399) are closed; `#[resource]` and
`#[linear]` (§3.7.8 of HEW-SPEC-2026.md) ship unconditionally as the
mechanism this spec's staged work converges on. Per-handle migration
continues incrementally — see §11 for the remaining handle types.
**Audience:** Hew language designers, stdlib maintainers, codegen and runtime
implementers, and reviewers of the migration PRs that follow.
**Scope:** the language-surface contract for owned, FFI-backed resource handles
(file descriptors, network connections, parsed values, compiled patterns,
process/actor scopes). Not a re-litigation of the v0.4.0 manual-`free()`
migration, which shipped for a documented reason; this spec builds forward from
that state.

---

## 1. Prime invariant

> **Hew programs are written as if there is no manual `free` in the language
> and no garbage collector.**

Every owned handle, every heap allocation, every channel, every connection,
every JIT session, and every actor mailbox is released *exactly once* on every
exit path — sync return, async cancel / future drop, actor shutdown / mailbox
drain, runtime cleanup / session reset, and panic — without the user typing
`close()` / `free()` and without a tracing or reference-counting collector.

This invariant is the binding test for every subsequent decision in this
document. Wherever a candidate mechanism makes user code carry an explicit
release call as the *normal* shape, the mechanism fails the test.

GC is **explicitly excluded**, restating the user directive. A finalising GC
would re-introduce the same failure class that drove the v0.4.0 manual-`free()`
migration (LESSONS row `ffi-ownership-contracts`): nondeterministic finalisation
ordering against an FFI free function whose idempotence was never proved. We
will not pay the user-facing cost of GC pauses *and* the soundness cost of
finalisers to land in the same place we are leaving.

`defer` is **not** the default answer. A `defer h.close();` line is still
manual `close()`, just relocated. It may exist as a narrow opt-in escape hatch
(§3, mechanism G), but the user-facing default must be the typed model where
no cleanup line is written at all.

---

## 2. The manual-`free()` model is transitional

The migration guide (`docs/migrations/v0.4.0.md` §§ #1314, #1500) records that
`impl Drop` for `http.Server`, `http.Request`, `regex.Pattern`, and `json.Value`
was removed because the existing drop paths called the same C free function as
the explicit `close()`/`free()` methods, producing a double-free whenever a
caller invoked the explicit method and then exited scope. Removing `impl Drop`
made `close()` / `free()` the single, unambiguous release path. That decision
was correct under the constraints at the time: Hew had no move checker, no
null-after-move infrastructure for arbitrary handle types, no field-alias
analysis, and no receiver mutability primitive (issue #1295).

This spec does **not** rewind that decision. The manual-`free()` model is
the current supported user contract. The destination — automatic single-release
at scope exit — is reached only after the substrate work in §§ 7–8 lands and
is calibrated on a single pilot handle (the calibration milestone — see §7).

The migration guide marks the manual-`free()` state as transitional and
forward-links to this spec.

---

## 3. Mechanism comparison and verdict

The candidate mechanisms below were evaluated against the prime invariant and
the operational constraints from `LESSONS.md` rows `ffi-ownership-contracts`,
`raii-null-after-move`, `field-alias-fail-closed`, `cleanup-all-exits`,
`checker-output-boundary`, `checker-codegen-pattern-contract`, and
`native-wasm-parity`.

| ID | Mechanism | User-facing shape | Verdict |
| --- | --- | --- | --- |
| A | RAII-like scope drop, handle types non-`Copy`, single-use, dropped at scope end. | `let p = regex.compile(...); ... // no free` | **Adopted as the soundness frame.** Provides the language proof that drop emission cannot fire twice. |
| B | Linear (must-use, must-consume) types — compile error on implicit drop. | `let _ = h;` required to discard. | **Stdlib-internal opt-in only.** Too verbose as the user default; reserved for resources where forgetting is a correctness bug, not a hygiene bug (e.g. transactional commit/abort tokens). |
| C | Affine types — implicit drop allowed, double-use forbidden. | Same as A from the user's seat. | **Adopted as the user-facing default.** Affine + RAII = "no manual free" in practice; matches the prime invariant directly. |
| D | Ownership tokens / capability handles. | `Token<H>` value passed to consumer. | **Adopted, but only for shutdown / cancellation / session lifetime** — not per-call resources. |
| E | Stdlib-level `OwnedHandle<T>` wrappers; user never sees the raw FFI handle. | Existing tier-2 owned-handle struct discipline. | **Required as transition substrate.** Pairs with C during the per-handle migration milestone. |
| F | Idempotent FFI close + `closed: bool` flag in `impl Drop`. | The canonical Rust pattern. | **Rejected as primary.** Rev1/rev2 of PR #1292 demonstrated the address-reuse failure mode this pattern admits without language support; permitted only as last-resort escape hatch behind explicit annotation. |
| G | `defer` / scope-guard syntax. | `defer h.close();` | **Reserved as narrow opt-in.** Useful for conditional release in state machines; never the default and never the recommended teaching shape. |
| H | Codegen drop tables (per-type drop-fn registry). | Invisible. | **Required emission substrate** under A/C/E. Already partly built (`registerDroppable` / `emitDropForVariable`). |
| I | Runtime / actor / session-scope cleanup hooks. | Invisible. | **Required complement** — process-scoped resources whose lifetime is not a lexical scope must hook into a session-reset registry (LESSONS `cleanup-all-exits`). |
| J | Garbage collection. | Invisible. | **Excluded by user directive.** |

### 3.1 Verdict (binding)

- **Primary user-facing model:** **affine handle types (C) with codegen-emitted
  scope-exit drops (H), proved sound by the move/borrow analysis published by
  the move-checker substrate (issue #1399) and the tier-1/tier-2 handle
  classification published by the owned-handle validator (issues #1252, #1251).**
  The user
  writes no `close()` or `free()` line in normal code.
- **Stricter mode (B):** opt-in `#[must_consume]` (or equivalent annotation, to
  be named at calibration time) for stdlib-internal resources where dropping
  on the floor is a *correctness* bug, not a hygiene bug. Not exposed to
  general user code as the default.
- **Tokens (D):** for actor stop, async cancel, and JIT/session lifetime.
  Per-call resources (`Pattern`, `Value`, `Request`, `Response`, `Url`) do not
  use tokens.
- **Escape hatch (G):** `defer` for the rare conditional-release case;
  user-visible but explicit; never the default.
- **Excluded:** F as primary (rev1/rev2 evidence); J GC entirely.
- **Required substrate:** E (wrappers as transition state), H (drop emission),
  I (session/actor hooks).

This verdict gates the calibration milestone (a single pilot handle migrated
under the new model). If calibration shows the affine model produces
unacceptable false positives, the verdict is reopened; per-handle migration
may not begin until the verdict survives calibration.

---

## 4. Resolving issue #1295 — receiver mutability and consume-on-call

Issue #1295 records that struct methods receive `self` by value, with no
`&mut self` and no `consume`/`move` modifier. This is the language gap that
made the canonical `closed: bool` Drop pattern unexpressible and forced the
v0.4.0 removal of `impl Drop`. Any spec that promises automatic release must
pick a resolution direction.

### 4.1 Decision

Hew adopts **consume-on-call as the default for methods on affine handle
types**, and introduces a `consume` modifier to make consumption explicit at
the declaration site for any other type that wants the same semantics.
Specifically:

- A method declared on a tier-1 affine handle type takes its receiver
  **by-move**. After the call, the receiver binding is **null-after-move**
  (LESSONS `raii-null-after-move`); the move-checker (issue #1399) rejects any
  subsequent use, including any subsequent automatic drop.
- A method declared `consume fn name(self, ...)` on any other type takes its
  receiver by-move regardless of the type's affine status.
- Non-consuming methods continue to take the receiver by-borrow. This is the
  semantic correction for #1295: today's "by-value" semantics are reframed as
  "by-borrow" for non-consuming methods on owned types, which matches what user
  code already assumes for `srv.accept()`, `pat.find_all(...)`, etc.

This direction is preferred over `&mut self` for two reasons. First, it does
not require introducing reference types into the surface language as a
prerequisite — Hew today has no `&mut` syntax in user code, and adding one is a
larger effort than the handle-safety work. Second, it makes the linearity proof
local: the move-checker can reason about each consuming call as a discrete
ownership transfer without tracking borrow lifetimes across complex control
flow.

### 4.2 Stdlib impact estimate

Counting `fn name(rcvr: T, ...)` signatures in `std/**/*.hew` for the inventory
handle types only (Server, Request, Pattern, Value, Url, Conn, Message,
TlsStream, Response) returned **387 methods**. Roughly an order of magnitude
more methods exist on non-handle stdlib types and are not affected by the
consume-on-call default but may opt into the `consume` modifier on a per-method
basis.

Of the 387 handle-receiver methods, the migration breakdown is:

- **Release methods (`close()`, `free()`):** ~9–11 (one per handle type listed
  in §7, with `websocket` accounting for three). These are the only methods
  that are *consuming* in v0.4.0 today, and they become redundant under the
  new model — the affine drop replaces them. They remain callable as a no-op
  or annotated as `#[deprecated]` through the per-handle migration milestone,
  then are removed in v0.6.x.
- **Read-only methods** (`get_field`, `get_string`, `find_all`, `len`, etc.):
  the large majority. Re-classified as borrow-receiver; no user-visible change.
- **Mutating-but-not-consuming methods** (`req.respond_text`, `srv.accept`):
  borrow-receiver; no user-visible change in call shape.
- **Methods that produce a new owned handle from an owning one**
  (`json.Value::get_field`, `http.Server::accept`): the returned handle becomes
  affine on its own; the receiver remains borrowed.

The estimate that no more than **~9–11 stdlib methods** require a *signature*
change (the release methods) and that the remaining receiver-bearing methods
only require re-classification of an already-borrow-shaped call as
borrow-shaped is the signal that this resolution direction is migratable in a
one-PR-per-handle-module cadence.

> **Counts derivation.** The receiver-method counts above were produced by
> grepping `std/**/*.hew` for signatures of the form
> `fn <name>(<rcvr>: <Handle>, ...)` for each handle type from §7. Reproduce
> with, for example,
> `rg -nE '^\s*(pub )?fn \w+\(\s*\w+:\s*Pattern\b' std/`. Numbers are
> snapshot-as-of-spec and will drift; treat them as order-of-magnitude
> evidence that the migration is bounded, not as a regression target.

A separate language change tracked under issue #1295 owns the parser, checker,
and codegen work to introduce the `consume` modifier and the borrow-default
reclassification. This spec records the *direction*; that work records the
implementation.

---

## 5. Where ownership classification lives

The checker is the authority. Codegen consumes pre-validated facts.

- `feat-owned-handle-validator` Phase 1 (#1252) publishes `handle_types` as
  checker-side metadata: which type definitions are tier-1 affine handles.
- Phase 2 (#1251) publishes `handle_bearing_structs` and rejects accessor
  methods that leak handle fields (matching LESSONS `field-alias-fail-closed`).
- The #1399 move-checker publishes the per-call/per-binding ownership
  transfer facts (Owned / Borrowed / FieldAlias / ClosureCapture /
  YieldedAcrossSuspend / Temporary). These are the categories LESSONS row
  `exhaustive-traversal-and-lowering` requires; the spec mandates their
  exhaustive presence with no wildcard fallthrough.
- The HIR/MIR pipeline carries these facts into `hew-codegen-rs` as
  validated MIR metadata. No `TypedProgram`/MessagePack/C++ backend seam
  exists; the HIR/MIR pipeline is the sole path.
- Codegen's role is fail-closed consumption only: missing classification at
  lowering is an invariant diagnostic (LESSONS `assignment-target-authority`,
  `checker-codegen-pattern-contract`), never a fall-back inference, and never
  a silent drop emission.
- Serialization is fail-closed (LESSONS `serializer-fail-closed`): every
  ownership classification serialises an explicit form or hard-fails with span
  and context.

This gives a single ownership oracle (the checker) feeding two consumers
(move-checker substrate per issue #1399, codegen drop emission). The checker
is the single ownership oracle.

---

## 6. Interaction with the move-checker substrate (single ownership model)

This spec and the move-checker substrate (issue #1399) publish **one**
ownership model, not two.

- **The move-checker substrate (issue #1399) owns:** the per-binding
  move/borrow analysis pass, its diagnostic vocabulary, its prototype
  implementation, and its acceptance / reject test fixtures. It is the
  *analysis substrate*.
- **This spec (handle safety) owns:** the user-facing language invariant
  (§1), the mechanism choice (§3), the per-handle migration tier assignment
  (§7), and the failure-mode taxonomy (§8). It is the *language surface*.
- **Shared:** the classification vocabulary (Owned / Borrowed / FieldAlias /
  ClosureCapture / YieldedAcrossSuspend / Temporary) and the `TypedProgram`
  serialised representation. Neither side redefines these without cross-review
  by the other.
- **Disagreement protocol:** if a per-handle migration PR cannot be expressed
  in the move-checker's vocabulary, the PR is held; the substrate amends its
  vocabulary *or* this spec amends its mechanism choice — but only after both
  sides' reviewers concur. This spec does not silently introduce a parallel
  oracle.
- **Analysis contract:** the full spec for what the move-checker substrate
  must implement — DROP-TODO inventory, failure-mode mapping, diagnostic
  vocabulary, and rejection fixtures — was tracked under issue #1399.

---

## 7. Per-handle tier assignment (the v0.4.0 inventory)

Every owned handle that exists in v0.4.0 is assigned a tier. Tiers determine
which mechanism applies and the order of the per-handle migration PRs.

| Module | Type | Current release | Tier | Mechanism | Migration order | Notes |
| --- | --- | --- | --- | --- | --- | --- |
| `std/text/regex` | `Pattern` | `close()` | 1 — per-call resource | C (affine) + H (drop table) | **1 (calibration pilot) — landed** | Smallest blast radius; no streaming; no state machine. `Pattern` is `#[resource]` with an opaque `handle` field; scope-exit drop calls `close()`, which frees the handle. Value-position `re"..."` literals clone the module-init-compiled handle on load (`hew-codegen-rs/src/runtime_abi.rs` `F::RegexHandle`), so each binding owns an independent handle and closes exactly once. |
| `std/net/url` | `Url` | `free()` | 1 — per-call resource | C + H | 2 | Read-mostly; no streaming. |
| `std/encoding/json` | `Value` | `free()` | 1 — per-call resource (with field-alias hazard) | C + H, requires `field-alias-fail-closed` enforcement to be live | 3 | Recursive ownership: every `get_field`/`array_get` returns a new owned `Value`; affine drop of the parent must not fire while a child is live. The move-checker substrate (issue #1399) must publish FieldAlias before this PR ships. |
| `std/net/http_client` | `Response` | `free()` | 1 — per-call resource | C + H | 4 | One-shot; not streaming. |
| `std/net/http` | `Request` | `free()` | 2 — per-recv allocation inside an actor scope | C + H + I (actor cleanup hook) | 5 | Lives inside a `receive fn` body; actor shutdown hook must release any in-flight `Request` whose handler did not return. |
| `std/net/http` | `Server` | `close()` | 3 — long-lived server scope | C + H + I (session/actor cleanup hook) | 6 | Process-or-actor scoped; `hew_runtime_cleanup` must release any live `Server` (LESSONS `cleanup-all-exits`). |
| `std/net/http` | response sink (`body.close()`) | `close()` | 2 — streaming sink | C + H, with explicit-finish protocol kept as opt-in (G `defer`) for conditional flush | 6 (with `Server`) | The sink represents an in-progress response; finishing the response is a *protocol* event, not just a release. Affine drop produces a default flush; `defer body.close();` permits explicit early finish. |
| `std/net/tls` | `TlsStream` | `close()` | 2 — streaming connection | C + H + I (actor cleanup) | 7 | Same shape as `Server`. |
| `std/net/websocket` | `Conn` | `close()` | 2 — streaming connection | C + H + I | 8 | Holds the read loop; per-recv `Message` allocation lives inside it. |
| `std/net/websocket` | `Server` | `close()` | 3 — long-lived | C + H + I | 8 | Same shape as `http.Server`. |
| `std/net/websocket` | `Message` | `free()` | 1 — per-recv allocation | C + H, scoped to the read-loop iteration | 8 | Stresses the model the hardest; affine drop fires per loop iteration, not per `Conn` lifetime. |
| Runtime actor / scheduler | implicit | `hew_runtime_cleanup` (`hew-runtime/src/scheduler.rs:459-490`) | 4 — runtime scope | I (session-reset registry) only; not affine | runtime-scope cleanup milestone | Process-scoped; the session-reset hook registry is the canonical teardown (LESSONS `cleanup-all-exits`). No user-facing release call; no per-binding affinity. Tracked under issue #1228. |
| WASM mirror | parity | `hew-runtime/src/scheduler_wasm.rs` | 4 — runtime scope | I, parity-required | runtime-scope cleanup milestone | LESSONS `native-wasm-parity` mandates symmetric session_reset from `hew_sched_shutdown` paths in both targets. See §10. |

Tiers explained:

- **Tier 1 — per-call resource:** lifetime is a lexical scope; affine drop at
  scope exit is the whole story. No actor / async / session interaction.
- **Tier 2 — streaming or actor-scoped:** lifetime is a `receive fn` body, an
  async task, or an iterator loop. Affine drop is the default; cleanup hooks
  in the actor / async-cancel paths catch the cases where the lexical scope
  did not get to run to completion (cancel, panic, mailbox drain).
- **Tier 3 — long-lived server scope:** lifetime spans many tier-2
  allocations. Same mechanism as tier 2 plus a session-reset entry so
  `hew_runtime_cleanup` releases any still-live instance.
- **Tier 4 — runtime / process / WASM scope:** not a per-binding handle in
  user code; lives in the runtime crate; released through the session-reset
  registry only.

Migration order is "smallest blast radius first": tier 1 before tier 2;
non-streaming before streaming; single-handle modules before multi-handle
modules. `regex.Pattern` ships first as the calibration pilot;
`websocket.{Conn, Server, Message}` ships last because the per-recv `Message`
allocation lives inside `Conn`'s read loop and exercises every part of the
model.

---

## 8. Failure-mode taxonomy

Every cleanup context has a named failure mode and a named guard. The guard is
*where* the failure is caught (checker diagnostic, codegen invariant, runtime
hook, sanitiser test), not just *what* the failure is.

| ID | Failure | Guard location | Guard mechanism |
| --- | --- | --- | --- |
| FM-1 | Double-free (explicit release + automatic drop both fire) | Checker (move-checker, issue #1399) | Reject at compile time; LESSONS `field-alias-fail-closed`, `raii-null-after-move`. |
| FM-2 | Use-after-close (binding referenced after consuming call) | Checker (move-checker) | Reject at compile time; null-after-move enforces. |
| FM-3 | Leak — sync return path (silent forget; binding goes out of scope and no drop fires) | Codegen drop emission (H) + sanitiser test in CI | Affine drop is unconditional at scope exit; LSAN run on every per-handle migration fixture. |
| FM-4 | Leak — async cancel / future drop | Codegen + async cancel path | Cancellation runs the same scope-exit drop sequence as a normal return; LESSONS `cleanup-all-exits` "cleanup must mirror setup on every exit path". An `async_cancel_cleanup` fixture is mandatory at the calibration milestone. |
| FM-5 | Leak — actor shutdown / mailbox drain | Runtime session-reset hook (I) + per-actor fixture | `hew_sched_shutdown` calls the session-reset registry; tier-2/3 handles register a release entry. An `actor_shutdown_cleanup` fixture is mandatory at the calibration milestone. |
| FM-6 | Leak — runtime cleanup / session reset | Session-reset hook registry (`hew-runtime/src/session.rs`) | Every subsystem registers unconditionally at init; `hew_runtime_cleanup` invokes the registry from every shutdown path. LESSONS `cleanup-all-exits` PR #1271 evidence. |
| FM-7 | Leak — panic unwind | Codegen drop emission on the unwind path | Same drop table as normal return; verified by a panic-path fixture at the calibration milestone. |
| FM-8 | FFI-side last-error race (a release function clobbers thread-local last-error after its caller observed an error) | Runtime `hew_*_free` wrapper convention | Release functions never read or write last-error; LESSONS `ffi-ownership-contracts`. |
| FM-9 | WASM parity gap (native target gets the cleanup, WASM target does not) | `make test-wasm` per per-handle PR | LESSONS `native-wasm-parity`; either parity green or named `WASM-TODO` (§10). |
| FM-10 | Field-alias double-free (struct field passed as call arg whose callee drops it) | Checker pre-scan (LESSONS `field-alias-fail-closed`) | Reject at compile time before call IR is emitted; `errorCount_` incremented alongside `emitError`. |
| FM-11 | Closure-capture leak (handle captured by long-lived closure that is never invoked) | Checker move-classification (`ClosureCapture`) | The move-checker substrate (issue #1399) publishes the classification; codegen emits a drop on closure deallocation. Reject if the captured handle's affine semantics cannot be satisfied. |
| FM-12 | Generator suspend/resume use-after-free (retired-backend DROP-TODO D6) | Checker move-classification (`YieldedAcrossSuspend`) | The move-checker substrate (issue #1399) must publish this category before any per-handle migration that touches a generator. Until then, generators that hold handle bindings are checker-rejected. |
| FM-13 | C ABI consumer double-free with **address reuse** — a C caller invokes `hew_*_free` after Hew already released, the underlying allocator hands the same address back to a *different* live allocation, and the second free corrupts the new allocation (the failure mode that drove rejection of mechanism F in §3) | C ABI test harness in `hew-cabi/tests/` | Calibration-milestone deliverable; release functions guarded by an FFI-side `closed` flag *only when proven idempotent and regression-tested* (LESSONS `ffi-ownership-contracts`). |

---

## 9. Cleanup contexts (every exit path covered)

The prime invariant requires *every* exit path to release exactly once. The
table below maps the five contexts the user directive named to their
mechanism:

| Context | Tier-1 (per-call) | Tier-2 (streaming/actor) | Tier-3 (server) | Tier-4 (runtime/WASM) |
| --- | --- | --- | --- | --- |
| Sync return | Affine scope-exit drop (H) | Affine scope-exit drop (H) | Affine scope-exit drop (H), session entry cleared | n/a |
| Panic unwind | Same drop table on unwind path | Same drop table on unwind path | Same drop table on unwind path; session-reset still fires | Session-reset on panic-during-shutdown (already covered by `hew_runtime_cleanup`) |
| Async cancel / future drop | Cancel runs the scope-exit drop sequence | Cancel runs the scope-exit drop sequence | Cancel-during-handler releases the in-flight handle; the long-lived server is unaffected | n/a |
| Actor shutdown / mailbox drain | n/a (per-call resources do not outlive a single `receive fn` body) | Per-handler scope drops fire as each in-flight message is finished or aborted; session-reset hook releases anything left | Session-reset hook releases the server | Session-reset hook releases all subsystems |
| Runtime cleanup / session reset | n/a | Session-reset hook (I) catches anything actor shutdown missed | Session-reset hook (I) | Session-reset hook (I) — the canonical teardown |
| WASM parity | Same drop emission in the WASM codegen path | Same actor cleanup in `scheduler_wasm.rs` | Same session-reset entries from `hew_sched_shutdown` in WASM | LESSONS `native-wasm-parity` — symmetric session_reset from both `hew_sched_shutdown` paths, or named `WASM-TODO` (§10) |

No row in the table is "the user types `close()` here." That is the prime
invariant restated as a property of the table.

---

## 10. WASM parity

Per LESSONS `native-wasm-parity`: any handle-lifecycle behaviour that lands in
`hew-runtime/src/scheduler.rs` must land symmetrically in
`hew-runtime/src/scheduler_wasm.rs` or carry a named `WASM-TODO`.

For this work specifically:

- The **codegen drop emission table** (H) is target-independent — it is derived
  before native/WASM target emission in the Rust HIR/MIR/codegen-rs path. WASM
  parity is automatic for tier 1 once the shared MIR facts are accepted.
- The **actor cleanup hook** (I) is duplicated across the two scheduler files.
  The runtime-scope cleanup milestone explicitly touches both. A per-handle
  migration PR that introduces a new hook entry must update both schedulers
  in the same diff.
- The **session-reset registry** must be invoked from `hew_sched_shutdown` in
  both `scheduler.rs` and `scheduler_wasm.rs`. PR #1271 fixed the analogous
  gap for `hew_trace_reset` and `clear_dispatch_registry`; this spec inherits
  that discipline.
- **Explicit WASM-TODO carve-outs allowed in this work:** none at spec time.
  If the calibration milestone discovers a WASM-only gap (e.g. async cancel
  paths not yet implemented under `wasm32-wasip1`), the `WASM-TODO` is named
  in the calibration notes and tracked in a follow-on issue, *not* hidden by
  silent divergence.

---

## 11. Per-handle migration table (work breakdown)

For every entry in §7, the per-handle migration PR records what changes for
the user, the stdlib, and codegen-rs. The full content of each row is finalised
by the PR itself; this spec fixes the *shape*.

| Handle | User-facing change | Stdlib-internal change | Codegen change |
| --- | --- | --- | --- |
| `regex.Pattern` (calibration pilot — landed) | `pat.free()` removed; `pat.close()` releases early, scope-exit drop covers the rest | `Pattern` marked `#[resource]` with an opaque `handle` field; `close` is the method the compiler calls implicitly at scope exit | Drop emission for `Pattern` registered unconditionally (the `--experimental-handle-safety` flag this row described was never built — `#[resource]`/`#[linear]` ship unconditionally); null-after-move enforced; value-position `re"..."` literals clone the shared module-init handle on load so each binding owns an independent handle |
| `url.Url` | `url.free()` becomes a no-op | tier-1 affine | drop registered |
| `json.Value` | `val.free()` becomes a no-op; nested `get_field` results no longer require manual frees | tier-1 affine; FieldAlias must be live in the move-checker substrate (issue #1399) | drop registered; field-alias pre-scan enforced |
| `http_client.Response` | `resp.free()` becomes a no-op | tier-1 affine | drop registered |
| `http.Request` | `req.free()` becomes a no-op; `receive fn` handler never types release | tier-2; actor cleanup hook entry added | drop registered + actor hook |
| `http.Server` | `srv.close()` becomes a no-op; long-lived server cleaned at session reset | tier-3; session-reset registry entry added | drop registered + session entry |
| `http` response sink | `body.close()` becomes a no-op; affine drop produces a default flush; `defer body.close();` available for explicit early finish | tier-2; flush-on-drop semantics specified | drop registered with flush hook |
| `tls.TlsStream` | `stream.close()` becomes a no-op | tier-2; actor hook | drop registered + actor hook |
| `websocket.Conn` | `conn.close()` becomes a no-op | tier-2; actor hook | drop registered + actor hook |
| `websocket.Server` | `srv.close()` becomes a no-op | tier-3; session entry | drop registered + session entry |
| `websocket.Message` | `msg.free()` becomes a no-op; per-recv allocation released per loop iteration | tier-1 affine inside `Conn`'s read loop | drop registered |

Each PR includes six fixtures (`forget_free`, `double_free`,
`use_after_close`, `accept_no_manual_free`, `actor_shutdown_cleanup`,
`async_cancel_cleanup`) instantiated for that handle.

---

## 12. C ABI ownership tests (required validation surface)

C consumers cannot benefit from compile-time linearity proofs. **C ABI
release functions are wrapped in an idempotent FFI-side guard** *only after*
the lower layer's idempotence is proved and regression-tested (the constraint
LESSONS `ffi-ownership-contracts` records). Until that proof exists for a
given handle, the C ABI release stays manual-close-only and the Hew side does
not auto-release for C-owned instances.

The test harness lives at **`hew-cabi/tests/`**. Each per-handle migration PR
adds:

- A C consumer test that calls `hew_<module>_<release>` exactly once and
  verifies LSAN-clean exit.
- A C consumer test that calls `hew_<module>_<release>` twice and verifies
  the second call is a no-op (idempotence) — *not* a crash, *not* a
  double-free.
- A mixed test where the Hew side auto-releases (unconditionally, via
  `#[resource]`) and a C consumer also calls the release function; the
  result must be exactly-one-release.

Without these tests passing for a given handle, that handle's per-handle
migration PR is held.

---

## 13. Stop-the-bleeding rules (historical — superseded)

The gating conditions below (successful calibration, the owned-handle
validator #1252/#1251, and the move-checker substrate #1399) are all
closed, and `#[resource]`/`#[linear]` ship unconditionally as the
converged mechanism. These interim rules are no longer binding; retained
as a record of the discipline the v0.4.x → v0.5 migration followed:

1. **No new stdlib handle types** that require user-visible manual `free()` /
   `close()`. A new module that needs a handle type must either (a) wait for
   tier-1 discipline, or (b) ship the handle type behind an explicit
   `// TEMPORARY: manual-free; remove when automatic single-release at scope exit ships (see docs/specs/handle-safety-and-resource-lifetime.md and tracking issue #...)`
   comment **and** file a tracking GitHub issue that links this spec.
   Reviewers gate PR approval on (b) being explicit.
2. **No reintroduction of `impl Drop`** for any v0.4.0 handle type until the
   substrate work in §§ 5–7 lands. The v0.4.0 manual-free state is the floor;
   reverting it informally re-opens the double-free class that drove #1314 /
   #1500.
3. **Examples teach the v0.4.0 contract until each per-handle migration
   ships.** `examples/http_json_demo.hew` and any other example with manual
   `free()` lines is updated *in the same diff* as the per-handle migration
   PR that makes those lines no-ops, not before and not later.
4. **Audit pass on v0.4.x examples for missing frees.** Each leak found
   becomes either an explicit `.free()` or an example rewrite that avoids the
   owned clone. This is in scope as a stop-the-bleeding task on v0.4.x.
5. **Negative tests for forget-to-free** on every existing stdlib handle type:
   each handle type must have at least one LSAN-driven CI test that fails if
   the handle is dropped on the floor without a `free()`. Without this,
   "manual free" silently regresses into "forgotten free" between v0.4.x
   releases.

---

## 14. References

- Migration record: `docs/migrations/v0.4.0.md` §§ #1314, #1500.
- Tracking issues: #1228 (runtime handle API / runtime-scope cleanup), #1251
  (owned-handle validator phase 2), #1252 (owned-handle validator phase 1),
  #1281 (FFI ownership remediation predecessor), #1295 (receiver mutability
  and consume-on-call), #1314 (`http.Server` / `regex.Pattern` manual-release
  migration), #1399 (move-checker substrate), #1500 (`http.Request` /
  `json.Value` manual-release migration).
- Substrate paths cited in this spec: `hew-mir/`, `hew-codegen-rs/`,
  `hew-runtime/src/scheduler.rs`, `hew-runtime/src/scheduler_wasm.rs`,
  `hew-runtime/src/session.rs`, `hew-cabi/tests/`.
- LESSONS rows: `ffi-ownership-contracts`, `raii-null-after-move`,
  `field-alias-fail-closed`, `cleanup-all-exits`, `checker-output-boundary`,
  `checker-codegen-pattern-contract`, `serializer-fail-closed`,
  `assignment-target-authority`, `exhaustive-traversal-and-lowering`,
  `native-wasm-parity`, historical retired-backend local type hints,
  `error-count-exit-code`,
  `preflight-perf-discipline`, `check-pass-does-not-imply-run-pass`,
  `network-smoke-readiness`.
- Issues: #1228, #1251, #1252, #1281, #1295, #1314, #1399, #1500.
