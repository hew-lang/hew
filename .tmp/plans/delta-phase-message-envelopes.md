# δ-phase — Arena-Backed Message Envelopes (v0.5)

## Status: Superseded — cut from v0.5 scope

δ-phase arena-backed message envelopes were cut from v0.5 by
`.tmp/orchestration/v05-strategy-consult-2026-05-19.md:54–58`. The
"Status: dispatchable" line in this document is stale. Defer to v0.6.

**Lane id**: `delta-phase-message-envelopes`
**Status**: dispatchable
**Composed with**: D24-1 auto-locks (`6740fd61`), D24-2 cancellation (`2fafcd8d`, `e5e4c3c6`), D24-3 binding-accurate captures (`5dc2a49b`, `04ea44bb`), Phase-α envelope FFI (PR #1721, `73c9c243`)
**Mission tenets**: Tenet 1 (reliability / fail-closed first), Tenet 2 (actor model first-class)
**Ratifies**: Q28.4 = b (pursue δ-phase in v0.5), Q28.6 = b (COW fork on activation end)
**Recommended executor**: Mixed — see Handoff
**Cross-ecosystem reviewer**: Copilot (GPT-5.5) for every slice touching the C ABI surface

---

## 1. Goals

1. Eliminate the `libc::malloc` + `libc::memcpy` per-message round-trip on the actor send path for the common case — a sender constructs a message in its per-dispatch arena (Tier B) and the receiver reads it through a refcounted envelope without any copy.
2. Establish the move-checker invariant (`MirCheck::ActorSendEscape` + send-as-consume on the four-state lattice in `hew-mir/src/dataflow.rs:81-118`) that makes envelope aliasing safe by construction. Fork-on-write becomes the cold safety net, not the hot path.
3. Delete the libc Tier-C path. After δ-phase lands, every actor message in v0.5 flows through the envelope. No fallback shim. No `HewMsgNode.data` / `data_size` fields.
4. Maintain WASM parity (`hew-runtime/src/mailbox_wasm.rs:218`, `wasm_parity_tests.rs:286-569`).
5. Maintain the existing scheduler reset cadence: `hew_arena_reset` fires once per activation (`scheduler.rs:893-896`). Receiver pace must never pin sender arena lifetime.
6. Maintain fail-closed semantics on every error path: arena exhaustion mid-send, send-after-arena-reset, receiver consuming after arena reclaim — all emit a typed diagnostic or longjmp through the supervisor seam.

## 2. Non-goals

- No user-visible memory primitives, lifetime annotations, or alloc APIs. δ-phase is pure substrate; permanent non-goal Q66.c stands.
- No change to D24-1 lock semantics or D24-2 cancellation token contract.
- No change to `#[max_heap(N)]` semantics: it remains a Tier-B per-activation cap. **Forks on activation end allocate from libc, not from the sender's arena.** Cap accounting is unchanged.
- No introduction of `Arc<T>`, shared-mutable Tier, or cross-actor references. Q28.1 (Tier A cap) stays a future-design item.
- No remote / cross-node envelope. Distributed messaging is a v0.6 substrate.
- No removal of `hew_msg_envelope_fork_for_write` (`mailbox.rs:396-460`). It remains as the cold safety net for the static-analysis-cannot-prove case and for the same-arena defragmentation path.
- No γ-phase (Frozen shared) or §12 capability-transfer envelope work. Only the δ-phase bit (`HEW_MSG_ENVELOPE_ARENA_BACKED`, `mailbox.rs:158`) is wired.

## 3. Surface inventory

Files touched, with file:line citations of the surface being changed. No legacy paths preserved.

### 3.1 Runtime

| File | Lines | Role |
|---|---|---|
| `hew-runtime/src/mailbox.rs` | `113-135` | Drop `data` / `data_size` from `HewMsgNode`; envelope becomes the only payload carrier. |
| `hew-runtime/src/mailbox.rs` | `189-202` | Add `CONSUMED` bit semantics on `HewMsgEnvelope::header_bits`. |
| `hew-runtime/src/mailbox.rs` | `154-172` | Add `HEW_MSG_ENVELOPE_CONSUMED = 1 << 9`; widen `MUST_BE_ZERO_MASK` to `!((1 << 10) - 1)`. WASM mirror must be updated atomically. |
| `hew-runtime/src/mailbox.rs` | `263-285` | `hew_msg_envelope_new_arena` constructor — payload pointer is the sender's arena cursor, `ARENA_BACKED` bit set. |
| `hew-runtime/src/mailbox.rs` | `329-352` | `hew_msg_envelope_release` skips `drop_glue` when `CONSUMED` is set; runs it otherwise (discard path). |
| `hew-runtime/src/mailbox.rs` | `462-506` | Delete `msg_node_alloc` (legacy deep-copy). |
| `hew-runtime/src/mailbox.rs` | `508-556` | Promote `msg_node_alloc_aliased` from `#[allow(dead_code)]` to the canonical send-allocator. |
| `hew-runtime/src/mailbox.rs` | `596-615` | `hew_msg_node_free` sets `CONSUMED` on the envelope before `release`. |
| `hew-runtime/src/mailbox.rs` | `1431-1495` | Remove the fail-closed panic in `hew_mailbox_send_aliased`; wire to the actual MPSC enqueue. |
| `hew-runtime/src/mailbox.rs` | `1526-1543` | Delete `hew_mailbox_send` (deep-copy path) entirely. Same for `hew_mailbox_send_with_reply`, `hew_mailbox_try_send`. **Or** rename to wrap aliased send for back-compat-during-cutover; pick on a per-symbol basis (see Slice 5). |
| `hew-runtime/src/mailbox_wasm.rs` | `218`, full file | Mirror every native change. WASM `hew_mailbox_send_aliased` was already fail-closed. |
| `hew-runtime/src/actor.rs` | `1739-1805` | Remove fail-closed panic in `hew_actor_send_aliased`; route to mailbox enqueue. |
| `hew-runtime/src/actor.rs` | `1729-1738`, `1919-1955`, `2886-2890` | Delete deep-copy `hew_actor_send`, `hew_actor_send_by_id`. |
| `hew-runtime/src/scheduler.rs` | `653-665`, `893-896` | Insert a **fork-pending-envelopes pass** before `hew_arena_reset`. Walks the per-actor pending-envelope side table, calls `hew_msg_envelope_fork_for_write` on every envelope whose `ARENA_BACKED` bit is set and whose backing arena matches `actor_arena`. **Uses the cached `actor_arena` pointer** (already cached at `:657`) per `cleanup-all-exits`. |
| `hew-runtime/src/scheduler.rs` | `818-835` (crash path) | Same fork pass runs on crash path before crash-driven reset. |
| `hew-runtime/src/scheduler.rs` | `749-758` | Delete the envelope/legacy branch — `dispatch_data` always reads from envelope. |
| `hew-runtime/src/actor.rs` | new field on `HewActor` | `pending_envelopes: AtomicPtr<...>` — intrusive list of `HewMsgEnvelope*` that this actor has sent with `ARENA_BACKED` and that have not yet been consumed by their receiver. See Slice 4. |
| `hew-runtime/src/arena.rs` | `113-131` | Add `arena_id: u64` field on `ActorArena` (monotonic). Envelope records this on construction; fork-on-activation-end matches on `arena_id == sender.arena.arena_id`. Cheaper than pointer comparison if arenas are reused. |
| `hew-runtime/src/wasm_parity_tests.rs` | `286-569` | Extend parity tests to cover `CONSUMED` bit and `hew_msg_envelope_new_arena`. |

### 3.2 Types / move-checker

| File | Lines | Role |
|---|---|---|
| `hew-mir/src/model.rs` | `181-185` | `Terminator::Send` becomes constructed (was declared-only). |
| `hew-mir/src/model.rs` | `1108-1112` | `MirCheck::ActorSendEscape` becomes constructed; emitted when `value`'s type contains a non-`Send` transitively reachable type. |
| `hew-mir/src/dataflow.rs` | `81-220` | Extend the four-state lattice consumer to recognise `Terminator::Send { value: Place::Local(b), .. }` as a consume site for `b`. `Live → Consumed(send_site)`. A subsequent `Use(b)` reaching this point fires `MirCheck::UseAfterConsume { consumed_at: send_site, used_at }`. |
| `hew-mir/src/dataflow.rs` | `282`, `301`, `579` | Extend traversal cases for `Terminator::Send` (already declared in match arms). |
| `hew-mir/src/lower.rs` | new producer | Lower HIR `actor.send(value)` (and the lambda-actor analog) to `Terminator::Send { actor, value, next }`. This is the producer side the consumer slices need (per `producer-bridge-before-codegen`). |
| `hew-mir/tests/producer_method_send.rs` | extend | Fixture: `actor.send(local_binding)` lowers; `local_binding` used after send produces `UseAfterConsume` with site equal to the lowered `Terminator::Send`. |
| `hew-types/src/check/` | actor-method-call site | Tag every `actor.send(...)` call expression with metadata that the codegen layer reads to choose envelope-vs-fallback. In v0.5 with no fallback, the metadata is just "this is a send"; it exists so the codegen lowering can be authoritative without re-deriving from HIR. (`type-info-survival`, `assignment-target-authority`.) |

### 3.3 Codegen

| File | Lines | Role |
|---|---|---|
| `hew-codegen-rs/src/llvm.rs` | `actor.send` lowering site | Replace the `hew_mailbox_send` call sequence with: (1) `hew_msg_envelope_new_arena(payload_ptr, payload_size, drop_glue)` reading payload from the current Tier B cursor; (2) `hew_mailbox_send_aliased(mb, msg_type, envelope)`. On `Oom` path the longjmp seam fires (no silent fallback). |
| `hew-codegen-rs/src/llvm.rs` | drop-elaboration / `nullOutDropSlot` | After the send-site lowering, null the source slot — `Terminator::Send` is a consume; the value MUST NOT be visible to user code afterwards (`raii-null-after-move`). |
| `hew-mir/src/runtime_symbols.rs` | new entries | Add `hew_msg_envelope_new_arena`, `hew_msg_envelope_clone_alias`, `hew_msg_envelope_release` to the allowlist so MIR lowering can `Instr::CallRuntimeAbi` them. |
| `hew-mir/src/runtime_symbols.rs` | delete entries | Remove `hew_mailbox_send` if it's deleted in Slice 5 (or keep as a δ-aware shim). |

### 3.4 Tests / benches

| File | Role |
|---|---|
| `hew-runtime/tests/envelope_consumed_bit.rs` (new) | Pin `CONSUMED` semantics: post-dispatch release does not run drop glue; discard-path release does. |
| `hew-runtime/tests/envelope_arena_backed.rs` (new) | End-to-end: arena-backed send, receiver reads, sender activation ends, no `memcpy` observed (instrumented `mailbox_malloc` counter). |
| `hew-runtime/tests/envelope_fork_on_activation_end.rs` (new) | Cross-actor send where receiver does not consume before sender activation ends — verifies fork pass fires, envelope payload pointer rewritten, sender arena reset cleanly. |
| `hew-mir/tests/actor_send_consume.rs` (new) | `actor.send(x); use(x)` → `UseAfterConsume` diagnostic with the send site as the consume anchor. |
| `hew-runtime/benches/pipeline_throughput.rs` (new) | Microbenchmark: producer → consumer pipeline, 1M messages, payload sizes 16B, 256B, 4KB. Reports memcpy count and total time. Separately reports same-actor self-send (where arena has not reset) and cross-actor (fork-on-end). |

---

## 4. Evidence (current substrate state, with file:line citations)

### E1 — Three-tier model is unchanged by this plan

The named-tier model from `per-actor-memory-model-design.md` §E1 is the foundation. Tier A (libc, long-lived `state`, `actor.rs:656` `*mut c_void`), Tier B (per-actor `ActorArena`, `arena.rs:113-131`), Tier C (mailbox payload). δ-phase folds Tier C *into* Tier B for the sender's hot path: payload is allocated on the sender's `ActorArena` cursor; the envelope structure itself remains `libc::malloc`'d (`mailbox.rs:269`) so envelope lifetime is decoupled from the arena's reset cycle.

### E2 — Envelope FFI surface is already planted (PR #1721, commit `73c9c243`)

- `HewMsgEnvelope` struct: `mailbox.rs:188-202`. Atomic `refcount`, atomic `header_bits`, `payload: *mut c_void`, `payload_size`, optional `drop_glue`.
- Constructor: `hew_msg_envelope_new(payload, size, drop_glue)` at `mailbox.rs:263-285`. Initial refcount 1, header bits 0.
- Clone-alias (refcount++, sets `ALIAS_ACTIVE`): `mailbox.rs:303-317`.
- Release (refcount--, runs drop glue + frees payload + frees envelope on final): `mailbox.rs:329-352`.
- Fork-for-write: `mailbox.rs:396-460`. Allocates a fresh libc buffer, memcpys payload, decrements original refcount, sets `FORKED` on the new envelope.
- Header bits: `mailbox.rs:154-172`. `ARENA_BACKED = 1 << 2` is reserved for δ. `MUST_BE_ZERO_MASK = !((1<<9) - 1)` — bits 9+ must read zero.
- `header_validate` at `mailbox.rs:233-242` enforces the must-be-zero invariant on every release.
- WASM mirror: `mailbox_wasm.rs:218` (`HEW_MSG_ENVELOPE_ARENA_BACKED`).
- Parity tests: `wasm_parity_tests.rs:286-287, 533, 552, 569` already assert both sides agree on the bit value.

### E3 — `HewMsgNode` discriminator branch is already wired

- `HewMsgNode.envelope: *mut HewMsgEnvelope` field at `mailbox.rs:132`. Null = legacy copy; non-null = envelope path.
- Scheduler dispatch branches on the discriminator at `scheduler.rs:749-758`:
  ```
  let (dispatch_data, dispatch_size) = if msg_ref.envelope.is_null() {
      (msg_ref.data, msg_ref.data_size)
  } else {
      let env = msg_ref.envelope;
      unsafe { ((*env).payload, (*env).payload_size) }
  };
  ```
- `hew_msg_node_free` branches at `mailbox.rs:607-612`: null envelope → `libc::free(node.data)`; non-null → `hew_msg_envelope_release(node.envelope)`.
- `msg_node_alloc_aliased` already exists at `mailbox.rs:531-556`, gated behind `#[allow(dead_code, reason = "Phase α: alias send is fail-closed; preserved for Phase β re-enable")]`.

### E4 — The send-aliased entry points are fail-closed (the prerequisite to unwind)

- `hew_mailbox_send_aliased` (`mailbox.rs:1431-1495`) **panics via `hew_panic`** on every call. The doc comment at `mailbox.rs:1452-1462` names the root cause and the fix:

  > `hew_msg_envelope_release` calls `drop_glue` on every final release with no "consumed" flag, so post-dispatch release would double-free fields the receiver moved out of the payload, while a discard-path release with `drop_glue=null` leaks them. … A correct Phase β fix introduces a CONSUMED bit on `HewMsgEnvelope::header_bits` set by `hew_msg_node_free` after dispatch, with release skipping `drop_glue` when CONSUMED is set.

- `hew_actor_send_aliased` (`actor.rs:1786-1805`): same fail-closed panic.
- WASM stub `hew_actor_send_aliased` (`actor.rs:1808-1825`): same.
- The codegen `ActorSendOpLowering` (referenced by doc comment at `mailbox.rs:1439-1441`) is gated off; today every `actor.send` lowers to `hew_mailbox_send` (deep-copy path). This is the surface flip slice 3 owns.

### E5 — The arena lifecycle and reset cadence is single-worker per activation

- Arena install: `scheduler.rs:665` `let prev_arena = crate::arena::set_current_arena(a.arena);`
- Cached for crash-recovery dangling-read safety: `scheduler.rs:657` `let actor_arena = a.arena;`
- Reset on normal exit: `scheduler.rs:893-896` once per activation (NOT once per message).
- Reset on crash: `scheduler.rs:828-834`, uses cached `actor_arena`.
- Restore on both paths: `scheduler.rs:891` (normal) and `:827` (crash).

The single-worker invariant during an activation is what makes the move-checker's `Live → Consumed(send_site)` transition sufficient. There is no concurrent reader on the sender's arena during its activation.

### E6 — D24-3 move-checker substrate is in flight and lattice-correct

- Four-state lattice on `BindingId`: `hew-mir/src/dataflow.rs:81-118` (`Uninit | Live | Consumed(s) | MaybeConsumed(s)`). Meet rules at `:114-118`.
- Per-block worklist + per-`Terminator::Return` exit enumeration: `cleanup-all-exits` LESSONS row cites `dataflow.rs` + `lower::enumerate_exits` — already in place.
- `Terminator::Send` enters traversal arms at `dataflow.rs:282`, `:301`, `:579` (declared, traversed, but no consume effect yet — that's Slice 2).
- Binding-accurate captures landed in `04ea44bb` (D24-3 slice 2): captures are recorded by `BindingId`, not by string match. This means the move-checker now has the precise binding identity needed to attribute a send-as-consume to the right source.
- Producer side: HIR-to-MIR lowering does not currently emit `Terminator::Send` for `actor.send(...)` (verified: `grep -rn "Terminator::Send" hew-mir/src/lower.rs` returns no construction sites). All current actor sends route through `hew_duplex_send` or directly to `hew_mailbox_send` via `Instr::CallRuntimeAbi`. This is the producer bridge Slice 2 owns (per `producer-bridge-before-codegen`).

### E7 — `hew_arena_malloc` cap-trap is fail-closed via longjmp

- `arena.rs:339-349`: when `arena.cap > 0` and an allocation would exceed it, the arena returns null then calls `crate::signal::try_direct_longjmp_with_code(HEW_TRAP_HEAP_EXCEEDED)`. This crashes the actor cleanly, supervisor sees `ExitReason::HeapExceeded`.
- Implication for δ-phase: a sender that tries to allocate an arena-backed envelope past its cap traps the same way as any other Tier B allocation. No new fail-closed boundary is introduced; the existing one is reused.
- WASM gap: `arena.rs:338` `#[cfg(not(target_arch = "wasm32"))]` — on WASM the null is returned to the caller and a subsequent trap may fire. Envelope path must propagate the null upward; `hew_msg_envelope_new_arena` returning null routes through the same `SendOutcome::Oom` path as today's `hew_mailbox_send` OOM.

### E8 — The legacy Tier-C blast radius is bounded

Files holding logic that disappears with `data` / `data_size`:

- `mailbox.rs:121-126` (struct fields), `:462-506` (`msg_node_alloc`), `:1526-1543` (`hew_mailbox_send`), `:1558-1576` (`hew_mailbox_send_with_reply`), `:1595-…` (`hew_mailbox_try_send`), `:1620-…` (`hew_mailbox_send_sys`), test calls at `:1956`, `:1980`, `:1984`, `:2136`, `:2146`, `:2259-2293`, `:2349`, `:2373`, `:2406`, `:2428`, `:2431`, `:2435`, `:2518`, `:2557`, `:2727`.
- `actor.rs:1729-1738` (`hew_actor_send`), `:1837-1916` (`hew_actor_send_wire` native + WASM), `:1919-1955` (`hew_actor_send_by_id`), `:2886-2890` (the duplex wrapper that selects `hew_mailbox_send_with_reply`), `:3555` (WASM extern declaration), `:3692-3736` (WASM `hew_actor_send` / `hew_actor_try_send`), `:3774` (WASM with-reply), and ~10 test call sites in the `#[cfg(test)]` block.
- `scheduler.rs:749-758` (the discriminator branch becomes unconditional envelope read).
- `mailbox_wasm.rs` mirror.

A single slice that removes the legacy path is in the 800–1200 line range. Sonnet-sized if the surface is purely mechanical (no semantic decision in the cutover) — which it is, because all the semantic decisions land in slices 1–4 first.

### E9 — No arena-backed envelope path is wired yet

Verified by grep:
- `grep -rn "HEW_MSG_ENVELOPE_ARENA_BACKED" hew-runtime/src/ hew-codegen-rs/src/` returns only the bit definition (`mailbox.rs:158`, `mailbox_wasm.rs:218`), parity-test sites (`wasm_parity_tests.rs:286, 287, 533, 539, 552, 557, 569`), and the test assertion at `mailbox.rs:2758` that the bit value is `1 << 2`.
- No constructor `hew_msg_envelope_new_arena` exists. No code path sets the `ARENA_BACKED` bit on a real envelope.
- No `pending_envelopes` field exists on `HewActor` (verified: `grep "pending_envelopes" hew-runtime/src/actor.rs` returns nothing).

The δ-phase is a green-field implementation against a reserved-but-unused FFI hook.

---

## 5. D24-3 move-checker dependency

### 5.1 The exact invariant the type-checker must enforce

For every MIR function `f` and every `Terminator::Send { actor, value: Place::Local(b), next }` in `f`'s CFG:

**(I-Send-Consume)** At the program point preceding the terminator, the dataflow state of binding `b` must be `Live`. The terminator's effect transitions `b` to `Consumed(send_site)` where `send_site` is the `SiteId` of this `Terminator::Send`.

**(I-Send-Use-After)** For every subsequent program point `p` reachable from this terminator's `next` successor where `b` is used (any `Use(b)` operand in a statement, terminator, or `Place` projection), the dataflow state of `b` at `p` must NOT be `Consumed(_)` or `MaybeConsumed(_)`. A violation emits the diagnostic kind `MirCheck::UseAfterConsume { binding: b, name: <symbol>, consumed_at: send_site, used_at: p }` (`hew-mir/src/model.rs:UseAfterConsume`).

**(I-Send-Escape)** If `value`'s `ResolvedTy` contains a transitively non-`Send` type (or is, today, any type since `Send` auto-trait inference is not yet shipped in v0.5 — see Slice 2 note), the diagnostic kind `MirCheck::ActorSendEscape { place: Place::Local(b), send_site }` (`model.rs:1112`) is emitted *instead of* lowering `Terminator::Send`. v0.5 reality: until auto-`Send` inference lands, every `actor.send(x)` is treated as `Send`-correct by definition (the type system has no surface to express a non-`Send` type). The diagnostic kind exists for the v0.6 surface and the lowering must construct it where the future inference would fire — placeholder for now, scaffolding only.

### 5.2 Why this is load-bearing for the entire path

If (I-Send-Consume) is not enforced, an arena-backed send is unsound:

- Sender bumps Tier B for a payload `p`.
- Sender calls `actor.send(receiver, p)`. Envelope refcount = 2 (one for sender's transient hold, one for the mailbox node).
- Symmetric-affine semantics: sender drops its refcount immediately, leaving refcount = 1 owned by the mailbox.
- **If (I-Send-Consume) is absent, the sender's binding `p` is still in scope and observably readable in the rest of the activation.** A subsequent `p.field = ...` write would race the receiver's read of the same memory (in the same-worker case, a re-entrant handler in the budget loop; in the cross-worker case, the receiver thread).

(I-Send-Consume) eliminates this hazard by construction: codegen sees the binding is consumed and emits `nullOutDropSlot` (`raii-null-after-move`), the optimiser drops the dead store, and any user code that tries to read `p` post-send is rejected at the move-checker.

### 5.3 The diagnostic kind that must fire on violation

`MirCheck::UseAfterConsume { binding: BindingId, name: String, consumed_at: SiteId, used_at: SiteId }` — already defined (`hew-mir/src/model.rs` near line 86, search for `UseAfterConsume`). Slice 2 wires `Terminator::Send` into the producer side and the existing `dataflow.rs` lattice traversal automatically picks up the consume effect (per the meet-rules at `:114-118` and the consume-recording loop at `:193-220`). Verification: extend `dataflow.rs` traversal to mark `value: Place::Local(b)` as consumed when crossing `Terminator::Send`, just as `Terminator::Yield`'s value-place is treated.

### 5.4 D24-3 status check (must be re-verified before Slice 2 dispatch)

- Slice 1 (parser surface, `5dc2a49b`): merged.
- Slice 2 (binding-accurate captures + MIR fail-closed gate, `04ea44bb`): merged.
- Slice 3 (ACP `bp8sylp9g`, env lowering + spawn-D24-2 inheritance): per the brief, just completed. **Implementer must verify on disk before assuming surface.** Concrete check: `git log --oneline -5 -- hew-mir/src/lower.rs hew-types/src/check/captures.rs` and grep for `D24-3 slice 3` in commit subjects.

If D24-3 slice 3 has not landed on `v05-integration` tip, **Slice 2 of this plan blocks**. Return `blocked` and re-dispatch this plan only after slice 3 merges.

---

## 6. Q28.6 resolution: COW fork on activation end

### 6.1 The pick

**Option (b): COW fork on activation end** — before the scheduler runs `hew_arena_reset` at end of activation, it walks the actor's pending-envelope list and, for every envelope whose `ARENA_BACKED` bit is set and whose backing `arena_id` matches the activation's arena, calls `hew_msg_envelope_fork_for_write(env)`. The fork allocates a fresh libc buffer (NOT a new arena bump — see §6.5), memcpys the payload, swings the envelope's `payload` pointer to the new buffer, clears `ARENA_BACKED`, sets `FORKED`. The original sender's arena pages are then reset cleanly.

### 6.2 Why this pick, in scheduler-semantics terms

Stated as a sequence-derived deduction, not a tradeoff vote:

1. **The arena reset cadence is per-activation, not per-message** (`scheduler.rs:893-896`). This is fixed. Changing it would require rebuilding D24-1's lock composition.
2. **Cross-actor messages cross a worker boundary.** Receiver runs in a separate activation, scheduled independently. At the moment sender's activation ends, receiver may not have run yet (could be hours later under backpressure).
3. **Option (a) "defer reset" entangles sender's `#[max_heap]` cap with receiver progress.** A slow or stopped receiver pins sender's arena. Sender's next activation hits its cap — but the cap accounting is reset on arena reset (`arena.rs:130, 247-251`), so deferral means the cap math drifts. Worst case: a paused receiver causes `HeapExceeded` on a sender that allocated nothing on its current cycle. That is a P0 fail-closed-in-the-wrong-direction hazard. Cited LESSON: `boundary-fail-closed`.
4. **Option (c) "forbid arena-backed send mid-batch" is leaky at the call site.** The compiler cannot tell which `actor.send(...)` call inside a handler body is the last one in the activation (control flow / loops). Forbidding all but the last requires whole-activation analysis with surface coupling. Higher engineering cost, lower benefit ceiling.
5. **Option (b) keeps sender's reset cadence intact, decouples receiver pace from sender cap, and pays the memcpy cost only when receiver has not consumed by sender's activation end.** For same-actor self-send within a single budget loop, the receiver activation IS the sender activation — receiver consumes (and `hew_msg_node_free` releases the envelope) before sender's reset fires. No fork. Win is largest exactly where the typical pipeline shape concentrates — small same-tier hops.
6. **The honest framing the plan must own:** for cross-actor sends, the memcpy is *moved*, not eliminated. The big win is (a) same-actor self-send and (b) the scheduler can now compose with future Frozen-shared (γ-phase) and capability-transfer (§12) without rethinking the reset cadence.

### 6.3 Implementation cost (lines / seams)

- New field on `HewActor`: `pending_envelopes: Mutex<Vec<*mut HewMsgEnvelope>>` (or lock-free MPSC-like structure if Mutex contention is measurable). Realistic size: 1 word atomic pointer head, `Mutex<Vec>` is enough for v0.5. ~10 lines.
- New field on `HewMsgEnvelope`: `arena_id: u64` (or none, if we recover sender-arena identity from a thread-local at envelope-new time). Pick: record on the envelope. ~3 lines added to struct + constructor.
- New field on `ActorArena`: `arena_id: u64`. Initialised from a global monotonic counter at construction. ~5 lines in `arena.rs`.
- Fork-pass entry: insert before `hew_arena_reset` calls at `scheduler.rs:828, 895`. ~40 lines (lock the pending list, iterate, call `fork_for_write`, drop the references the actor held). Total scheduler delta: ~50 lines.
- Send-site registration: when `hew_msg_envelope_new_arena` is called from inside an actor activation, the new envelope is pushed onto `pending_envelopes` of the current actor (via thread-local set at `scheduler.rs:662` `set_current_actor`). ~15 lines in `mailbox.rs`.
- Consumed-side removal: when `hew_msg_node_free` releases an envelope with `ARENA_BACKED`, it must also remove the envelope from the sender actor's `pending_envelopes`. Since the consumer is a different actor (typically a different worker thread), the removal is a remote-pop. Use a hazard pointer or, simpler, mark the envelope's `arena_id = 0` on consume so the sender's fork pass skips it. ~10 lines.

Total: ~120 lines of new substrate across `arena.rs`, `mailbox.rs`, `actor.rs`, `scheduler.rs`. The fork-pass is the only non-trivial piece.

### 6.4 Why not just reuse `arena_backed → arena_id` and skip the pending list

A pending list is needed because the fork-pass must enumerate envelopes deterministically before reset. Walking every live envelope in the runtime to find ones matching `arena_id` is O(global) — not acceptable. Per-actor list is O(in-flight envelopes from this actor). Single-producer (the sender during its activation) — no contention on push. Single-consumer on the fork pass (the sender's worker thread). Cross-actor remote-mark on consume — `arena_id` swap to zero suffices.

### 6.5 The fork allocates from libc, NOT from `hew_arena_malloc`

If the fork called `hew_arena_malloc`, it would route through `arena.rs:339-349`'s cap check. If the actor is at-cap (which is precisely when the activation is about to reset and the fork pass is firing), the fork allocation traps `HEW_TRAP_HEAP_EXCEEDED` — fail-closed in the wrong direction (sender crashes because the receiver was slow). The fork uses `mailbox_malloc` → `libc::malloc`, which is uncapped (`mailbox.rs:82-92`). On libc OOM, the fork returns null, which surfaces as a release-without-fork (the envelope still points into the about-to-be-reset arena) → memory unsafety.

**Fail-closed boundary for fork OOM**: if `hew_msg_envelope_fork_for_write` returns null during the activation-end fork pass, the scheduler MUST trap. Suggested mechanism: increment a runtime counter and longjmp via `try_direct_longjmp_with_code(HEW_TRAP_FORK_OOM)`. A new trap code (e.g., 201) is added to `supervisor.rs`. The actor exits with `ExitReason::ForkOom`. This is the new fail-closed boundary the plan introduces; it must be explicitly documented and tested.

---

## 7. Staged execution order

Six slices. Each slice is one commit-sized for an implementer dispatch (Sonnet unless noted). Slices are sequential — each gates the next on a green preflight + the named test. Where two slices could be stacked (per `feedback_stack_work_while_ci_runs`), they are noted.

### Slice 1 — CONSUMED bit + ordered envelope release (Sonnet)

**Substrate ownership**: `hew-runtime/src/mailbox.rs`, `mailbox_wasm.rs`, `wasm_parity_tests.rs`.

**Files changed**:
- `mailbox.rs:154-172` — add `HEW_MSG_ENVELOPE_CONSUMED = 1 << 9`; update `MUST_BE_ZERO_MASK = !((1 << 10) - 1)`.
- `mailbox.rs:329-352` — `hew_msg_envelope_release` reads `CONSUMED` bit; if set, skip `drop_glue` invocation but still `libc::free(payload)` and `libc::free(env)`.
- `mailbox.rs:596-615` — `hew_msg_node_free` sets `CONSUMED` via `header_bits.fetch_or(HEW_MSG_ENVELOPE_CONSUMED, Ordering::AcqRel)` immediately before calling `hew_msg_envelope_release(env)`.
- `mailbox_wasm.rs` — mirror.
- `wasm_parity_tests.rs:286-569` — parity assertions for the new bit value.

**Test**: `hew-runtime/tests/envelope_consumed_bit.rs` (new). Two scenarios:
- Dispatch-completed release: envelope with refcount=1, CONSUMED set by `hew_msg_node_free`, drop_glue is `Some(fn)`. After release, fn was NOT called. Payload + envelope freed.
- Discard-path release: envelope with refcount=1, CONSUMED NOT set. Release runs drop_glue then frees.

**Fail-closed boundary**: A re-entrant release on a CONSUMED envelope still hits the `header_validate` mask check at `mailbox.rs:233-242`. CONSUMED is within the recognised bits (post-widening), so validation passes; the drop_glue skip is correct. **Do not move CONSUMED into the MUST-BE-ZERO band**.

**LESSONS triggers**: `boundary-fail-closed`, `serializer-fail-closed` (MUST_BE_ZERO mask is exactly the contract), `native-wasm-parity`.

**Preflight gate**: `cargo test -p hew-runtime envelope_consumed_bit` + `cargo test -p hew-runtime wasm_parity_tests` + `make ci-preflight`.

**Reviewer**: Copilot (`feedback_cross_model_implementer_default`).

---

### Slice 2 — MIR move-checker: `Terminator::Send` producer + consume effect (Opus)

**Substrate ownership**: `hew-mir/src/lower.rs`, `hew-mir/src/dataflow.rs`, `hew-mir/src/model.rs`, `hew-types/src/check/`.

**Files changed**:
- `hew-mir/src/lower.rs` — new producer: HIR `actor.send(value)` lowers to `Terminator::Send { actor, value: <Place>, next }`. Replaces the current `Instr::CallRuntimeAbi { symbol: "hew_mailbox_send", ... }` emission for the actor-send method-call lowering site. (For the WASM duplex-send path which is structurally distinct, leave alone — Slice 5 reconciles.)
- `hew-mir/src/dataflow.rs:282, 301, 579` — extend traversal to record `value: Place::Local(b)` as consumed when crossing `Terminator::Send` (mirror the `Terminator::Yield` handling).
- `hew-mir/src/model.rs:1112` — promote `MirCheck::ActorSendEscape` to construction. Emit when `value`'s `ResolvedTy` is known non-`Send` (today: never fires — auto-`Send` inference is post-v0.5; this is scaffolding per Slice 2 note in §5.1).
- `hew-types/src/check/` — tag actor-method-call sites; thread through to MIR lowering (`type-info-survival`).

**Test**: `hew-mir/tests/actor_send_consume.rs` (new):
- `let p = make_payload(); actor.send(p); use(p);` → `UseAfterConsume { consumed_at: <send-site>, used_at: <use-site> }` diagnostic at the second use.
- `let p = make_payload(); if cond { actor.send(p); } use(p);` → `UseAfterConsume` because dataflow reports `MaybeConsumed` at the use site (per the lattice meet rule at `dataflow.rs:114`).
- `let p = make_payload(); actor.send(p);` (no subsequent use) → no diagnostic.

**Fail-closed boundary**: any MIR function reaching the elaboration pass with a `Terminator::Send` whose `value` is in state other than `Live` at the send site is a `MirCheck` violation. The diagnostic must include the binding name and the send-site `SiteId`.

**LESSONS triggers**: `cleanup-all-exits` (the per-block dataflow rule directly applies — this is the same lattice the row pinned), `producer-bridge-before-codegen` (producer + consumer in the same slice), `checker-codegen-pattern-contract` (checker rejects shapes codegen can't lower).

**Preflight gate**: `cargo test -p hew-mir actor_send_consume` + `make ci-preflight`.

**Reviewer**: Copilot for the MIR ABI / move-checker semantics; Opus implementer cross-attentions LESSONS rows.

**Stacking note**: Slice 2 does NOT depend on Slice 1 landing — it can be dispatched in parallel on a sibling worktree, then rebased per `feedback_rebase_then_preflight_before_squash`.

---

### Slice 3 — Codegen flip: emit envelope-backed sends (Opus)

**Substrate ownership**: `hew-codegen-rs/src/llvm.rs`, `hew-mir/src/runtime_symbols.rs`, `hew-runtime/src/mailbox.rs` (de-fail-close), `hew-runtime/src/actor.rs` (de-fail-close).

**Files changed**:
- `hew-runtime/src/mailbox.rs` — add `hew_msg_envelope_new_arena(payload_size, drop_glue) -> *mut HewMsgEnvelope`. Allocates the envelope structure via `mailbox_malloc` (libc), allocates the payload via `hew_arena_malloc` (current actor's arena, per the thread-local at `arena.rs:289-291`), sets `ARENA_BACKED` bit, sets `arena_id` from the current arena (new accessor on `ActorArena`). Returns null on OOM. Registers the new envelope into the current actor's `pending_envelopes` list (new field — see Slice 4 for the field definition; for Slice 3, define the field as a no-op `Mutex<Vec<*mut HewMsgEnvelope>>` and push on construction).
- `hew-runtime/src/mailbox.rs:1431-1495` — remove fail-closed panic in `hew_mailbox_send_aliased`; wire through to MPSC enqueue with `msg_node_alloc_aliased`.
- `hew-runtime/src/actor.rs:1786-1805` — remove fail-closed panic in `hew_actor_send_aliased`; forward to `hew_mailbox_send_aliased`.
- `hew-codegen-rs/src/llvm.rs` — `actor.send` lowering emits:
  1. `payload_ptr = hew_msg_envelope_new_arena(size_of_payload, drop_glue_ptr)`.
  2. `serialize_value_into(payload_ptr, value)` — codegen writes the moved value's bytes into the arena slot. (Mechanically: existing struct/scalar layout code, redirected destination.)
  3. `hew_actor_send_aliased(target_actor, msg_type, envelope)`.
  4. `nullOutDropSlot(source_binding)` — `raii-null-after-move`.
- `hew-mir/src/runtime_symbols.rs` — allowlist `hew_msg_envelope_new_arena`, `hew_msg_envelope_clone_alias`, `hew_msg_envelope_release`, `hew_actor_send_aliased`, `hew_mailbox_send_aliased`.
- `hew-runtime/src/mailbox.rs:531-556` — remove the `#[allow(dead_code)]` on `msg_node_alloc_aliased`.
- WASM mirror: `mailbox_wasm.rs`, `actor.rs:1808-1825`.

**Test**: `hew-runtime/tests/envelope_arena_backed.rs` (new):
- Spawn two actors A and B. A's handler calls `B.send(payload)` with a 256-byte payload.
- Instrument `mailbox_malloc` calls (test-only thread-local counter).
- Assert: total `mailbox_malloc` calls = 2 (one for the `HewMsgEnvelope`, one for the `HewMsgNode`). No call for a payload buffer — the payload came from A's arena.
- Receiver reads the payload correctly.
- After B's handler returns and `hew_msg_node_free` runs, envelope refcount goes to zero, `mailbox_free` count matches.

**Fail-closed boundary**: `hew_msg_envelope_new_arena` returns null if either `mailbox_malloc` returns null (libc OOM) or `hew_arena_malloc` returns null (arena cap exceeded → already longjmps via `arena.rs:339-349`). The codegen-emitted sequence checks for null and routes the error through the existing `SendOutcome::Oom` → `HewError::ErrOom` (-5) path. No silent fallback to libc copy. Cite `behavioral-regression-not-just-test-pass`: the user-observable error semantics are unchanged (still `ErrOom`).

**LESSONS triggers**: `producer-bridge-before-codegen` (the MIR Slice 2 producer must be live first — verify before dispatch), `raii-null-after-move`, `ffi-ownership-contracts` (the envelope refcount transfer contract is a new boundary), `behavioral-regression-not-just-test-pass`, `native-wasm-parity`.

**Preflight gate**: `cargo test -p hew-runtime envelope_arena_backed` + `cargo test -p hew-codegen-rs` + `make ci-preflight`.

**Reviewer**: Copilot.

**Stacking note**: Slice 3 depends on Slice 1 (CONSUMED bit) AND Slice 2 (producer). Dispatch only after both are merged. Do not stack on the merge queue.

---

### Slice 4 — Fork-on-activation-end pass (Opus)

**Substrate ownership**: `hew-runtime/src/scheduler.rs`, `hew-runtime/src/actor.rs`, `hew-runtime/src/arena.rs`, `hew-runtime/src/mailbox.rs`, `hew-runtime/src/supervisor.rs`.

**Files changed**:
- `hew-runtime/src/arena.rs:113-131` — add `pub arena_id: u64` field; initialise from a global monotonic `AtomicU64::fetch_add(1, Relaxed)`. ~5 lines.
- `hew-runtime/src/mailbox.rs:189-202` — add `pub arena_id: AtomicU64` field on `HewMsgEnvelope` (atomic so consumers can mark `0` for "already consumed"). ~3 lines + constructor wiring.
- `hew-runtime/src/actor.rs` — add `pending_envelopes: Mutex<Vec<*mut HewMsgEnvelope>>` field on `HewActor`. Initialise in spawn. Pop on actor free. ~20 lines.
- `hew-runtime/src/mailbox.rs` — `hew_msg_envelope_new_arena` pushes the new envelope onto the current actor's `pending_envelopes` (lookup via `actor::current_actor()` thread-local at `actor.rs:set_current_actor` site). ~10 lines.
- `hew-runtime/src/mailbox.rs:329-352` — `hew_msg_envelope_release` on a CONSUMED envelope with `ARENA_BACKED` swaps `arena_id` to 0 (atomic) so the sender's fork pass skips it. ~5 lines.
- `hew-runtime/src/scheduler.rs:893-896` (normal path) and `:828-834` (crash path) — insert `fork_pending_envelopes_for_arena(a, actor_arena)` BEFORE `hew_arena_reset`. New function defined in `scheduler.rs`: locks the actor's `pending_envelopes` Vec, drains it, for each envelope checks `arena_id.load(Acquire)` against the activation's `arena_id`. If match AND `ARENA_BACKED` bit set, calls `hew_msg_envelope_fork_for_write(env)` (which rewrites the payload to libc storage and clears `ARENA_BACKED`, sets `FORKED`). If `fork_for_write` returns null, longjmps with the new trap code `HEW_TRAP_FORK_OOM = 201`. ~50 lines.
- `hew-runtime/src/supervisor.rs:406` (near `HEW_TRAP_HEAP_EXCEEDED`) — add `HEW_TRAP_FORK_OOM = 201`; add `ExitReason::ForkOom` variant. ~10 lines.

**Test**: `hew-runtime/tests/envelope_fork_on_activation_end.rs` (new):
- Spawn A and B. A sends 5 messages to B in a single handler invocation. B is throttled (`#[mailbox(capacity = 100, overflow = Block)]` is not what we want — instead, hold B by not letting it dispatch within A's activation, e.g., explicit single-threaded scheduler in the test).
- A's activation ends. Assert: each envelope's `ARENA_BACKED` bit cleared, `FORKED` set, `payload` pointer is not within A's arena chunk range, A's arena reset OK.
- B activates, reads each message correctly (payload is the libc-forked buffer, not the arena one).
- Memcpy count = 5 (one per fork, expected).

Second scenario: A sends to A (self-send), processes in the same activation budget loop. Assert: no fork fires (receiver consumed before reset). Memcpy count = 0.

Third scenario: forced fork OOM (test hook to make `mailbox_malloc` return null). Assert: A exits with `ExitReason::ForkOom`, supervisor sees `HEW_TRAP_FORK_OOM`.

**Fail-closed boundary**: fork OOM is a hard actor exit. No silent payload-pointer-into-reset-arena hazard. Crash path runs through the existing longjmp seam, lock released non-poisoned (per Q28.2 ratification, `actor.rs:315-323`).

**Cached `actor_arena` pointer use**: the fork pass uses the cached `actor_arena` pointer at `scheduler.rs:657` for the crash-path branch. The normal-path branch can read `a.arena` directly (actor is not yet freed). Cite `cleanup-all-exits` (the dangling-read invariant the row pinned).

**LESSONS triggers**: `cleanup-all-exits` (cached arena pointer, longjmp routing), `boundary-fail-closed` (fork OOM = hard trap), `serializer-fail-closed` (envelope header consistency after fork), `native-wasm-parity` (fork pass needs WASM mirror; WASM cooperative scheduler has the same activation boundary).

**Preflight gate**: full `make ci-preflight` (touches scheduler + supervisor, fallback lane runs the full test suite).

**Reviewer**: Copilot. Cross-ecosystem mandatory because the new trap code is C-ABI surface.

**Stacking note**: Slice 4 depends on Slice 3 (the arena-backed envelope is the thing the fork pass operates on). Do not dispatch until Slice 3 is merged.

---

### Slice 5 — Delete legacy Tier-C path (Sonnet)

**Substrate ownership**: `hew-runtime/src/mailbox.rs`, `mailbox_wasm.rs`, `hew-runtime/src/actor.rs`, `hew-runtime/src/scheduler.rs`, `hew-mir/src/runtime_symbols.rs`, every test in `mailbox.rs`/`actor.rs` that uses `hew_mailbox_send` / `hew_actor_send`.

**Files changed**:
- `mailbox.rs:113-135` — drop `data` and `data_size` fields from `HewMsgNode`. Update `#[repr(C)]` layout. Update all node constructors.
- `mailbox.rs:462-506` — delete `msg_node_alloc`.
- `mailbox.rs:1526-1543, 1558-1576, 1595-…, 1620-…` — delete `hew_mailbox_send`, `hew_mailbox_send_with_reply`, `hew_mailbox_try_send`, `hew_mailbox_send_sys` (or rewrite each as an envelope-aliased shim).
- `actor.rs:1729-1738, 1837-1955, 2886-2890, 3692-3736, 3774` — delete the equivalent in actor.rs (native + WASM).
- `scheduler.rs:749-758` — collapse the envelope/legacy branch into unconditional envelope read.
- `mailbox_wasm.rs` — mirror.
- All test call sites (~30 of them, enumerated in §E8) updated to use envelope-aliased entry points OR a test-only helper `test_send_legacy_emulation(mb, msg_type, data, size)` that wraps the deep-copy path *inside the test crate* for tests that genuinely need to test the mailbox without involving codegen. Pick: prefer the helper to avoid rewriting 30 tests.
- `hew-mir/src/runtime_symbols.rs` — remove `hew_mailbox_send` etc from allowlist; rely on `hew_actor_send_aliased`.

**Test**: existing `cargo test -p hew-runtime mailbox` and `cargo test -p hew-runtime actor` must pass. Add `mailbox_legacy_symbols_removed.rs` (new): static check via `extern "C"` declaration that `hew_mailbox_send` no longer resolves (compile-fail test).

**Fail-closed boundary**: any external (FFI / dlopen) caller that linked `hew_mailbox_send` now gets a link error. This is the desired behaviour — no fallback shim — per the no-legacy rule. JIT symbol classification (`scripts/jit-symbol-classification.toml` per `mailbox.rs:1447-1450`) must be updated to remove the deleted symbols.

**LESSONS triggers**: `dead-surface-sweep`, `audit-completeness-via-multiple-greps` (search every crate for `hew_mailbox_send` before deleting), `native-wasm-parity`.

**Preflight gate**: `make ci-preflight` (fallback lane — large diff in the runtime).

**Reviewer**: Copilot for the C-ABI surface deletion (confirms no external consumer assumption was missed).

**Stacking note**: Slice 5 depends on Slice 4 (the envelope path must be the sole supported path). It can be developed against Slice 4's branch tip.

---

### Slice 6 — Pipeline benchmark + perf table (Sonnet)

**Substrate ownership**: `hew-runtime/benches/`, `.tmp/plans/delta-phase-message-envelopes.md` (this file, update with measured numbers).

**Files changed**:
- `hew-runtime/benches/pipeline_throughput.rs` (new). Two scenarios:
  - **Same-actor self-send**: an actor sends to itself in a tight loop, 1M messages, payload sizes 16B / 256B / 4KB.
  - **Cross-actor pipeline**: producer → consumer (1:1 → 1:N → N:N fanout), 1M messages, same payload sizes.
- Instrument `mailbox_malloc` counter, `libc::memcpy` byte total (via `arena_id` mismatch counter on fork), total wall-clock time.

**Test / artifact**: bench output committed to `.tmp/perf/delta-phase-pipeline-{date}.md` (gitignored — for the lane only). The plan doc § Risks gets a one-line "measured: X% memcpy reduction same-actor, Y% cross-actor" footnote.

**Fail-closed boundary**: none — this is a perf measurement, not a correctness boundary.

**LESSONS triggers**: `preflight-perf-discipline` (benchmarks must be reproducible and committed as repo targets, not ad-hoc scripts).

**Preflight gate**: `cargo bench -p hew-runtime --bench pipeline_throughput` + `make ci-preflight`.

**Reviewer**: Sonnet self-review acceptable since this is non-substrate measurement. Optional Copilot cross-check on the methodology.

---

## 8. Composition with D24-1, D24-2, D24-3

### 8.1 With D24-1 (auto-injected locks, `6740fd61`)

The state-lock wraps the entire handler body (`actor.rs:222-251`, contract from D24-1). Tier B is exclusively owned by the lock-holder during dispatch. δ-phase adds:

- Envelope construction (`hew_msg_envelope_new_arena`) runs **inside** the lock-held window — single-worker access to Tier B is preserved.
- `pending_envelopes` push runs inside the lock-held window — but the `pending_envelopes` `Mutex` itself is independent of the state-lock. The Mutex is acquired only during push (sender's activation) and during the fork pass (sender's worker thread, post-lock-release but pre-arena-reset). No nested lock-ordering hazard because the state-lock is released before the fork pass fires.
- The fork pass runs AFTER `hew_actor_state_lock_release` (D24-1 release-on-normal-exit, `actor.rs:`) and BEFORE `hew_arena_reset`. Sequence preserved:
  ```
  state-lock held → handler runs → state-lock released → fork pass → arena reset
  ```
- On crash: state-lock release-after-panic (no poison, `actor.rs:315-323` rationale) runs first, then fork pass, then arena reset. Same sequence. Cited LESSON: `cleanup-all-exits`.

### 8.2 With D24-2 (cancellation tokens, `e5e4c3c6`)

Cancellation tokens live in `task_scope.rs`, on the Rust heap. They are not in any Tier. Arena reset and envelope forks do not touch tokens. The interaction is one-way: if a handler is cancelled mid-send, the partial envelope's refcount is in the `pending_envelopes` list and the receiver mailbox node. Cancellation does not abort the in-flight envelope (the message is already on the mailbox queue once `hew_actor_send_aliased` returns); it just stops the handler from doing more work. The fork pass at activation-end still fires correctly.

Edge case: if a handler is cancelled BEFORE `hew_actor_send_aliased` returns but AFTER `hew_msg_envelope_new_arena` returned, the envelope exists in `pending_envelopes` but is not on any mailbox. The fork pass forks it (over-fork, harmless) and the envelope's refcount-to-zero release runs the drop_glue (since CONSUMED is not set — discard path). This is correct: the payload's owned fields are dropped via `drop_glue` exactly once. Cite `cleanup-all-exits` (the cancel-vs-Drop gap is the documented hazard the row addresses).

### 8.3 With D24-3 (binding-accurate captures, `04ea44bb`)

D24-3 gives the move-checker precise binding identity. δ-phase Slice 2 builds on this: the `Terminator::Send { value: Place::Local(b), .. }` consume effect is attributed to the exact `BindingId` D24-3 records. Without binding-accurate captures, a send-as-consume would have ambiguous source attribution (which `let p = ...` is being consumed?). With D24-3, it is exact. This is the foundation that lets fork-on-write be the cold path: the static analysis is tight enough that runtime forks fire only when the analysis cannot reason (e.g., conditional sends inside loops where path-sensitivity is exhausted).

---

## 9. Validation strategy

### 9.1 Per-slice

| Slice | Test | Gate |
|---|---|---|
| 1 | `cargo test -p hew-runtime envelope_consumed_bit` | CONSUMED bit semantics |
| 1 | `cargo test -p hew-runtime wasm_parity_tests` | WASM mirror |
| 2 | `cargo test -p hew-mir actor_send_consume` | Move-checker consume effect |
| 2 | `cargo test -p hew-mir producer_method_send` | Producer side wired |
| 3 | `cargo test -p hew-runtime envelope_arena_backed` | Memcpy elimination |
| 3 | `cargo test -p hew-codegen-rs` | Lowering emits envelope |
| 4 | `cargo test -p hew-runtime envelope_fork_on_activation_end` | Fork pass correctness |
| 4 | `cargo test -p hew-runtime envelope_fork_oom_traps` | Fork OOM trap |
| 5 | `cargo test -p hew-runtime` | Full mailbox suite passes with legacy deleted |
| 5 | `cargo test -p hew-runtime mailbox_legacy_symbols_removed` | Compile-fail on deleted symbols |
| 6 | `cargo bench -p hew-runtime --bench pipeline_throughput` | Perf numbers reproducible |

### 9.2 End-to-end

After Slice 4 merges (Slice 5 / 6 follow):
- `make ci-preflight` fallback-lane (full `make test`) — required signal.
- WASM parity tests pass (`cargo test -p hew-runtime --target wasm32-unknown-unknown wasm_parity_tests`).
- Manual smoke: spawn a 3-actor pipeline (`hew run examples/pipeline.hew`), verify deterministic output + zero memcpy on same-actor hops via the instrumented build.

### 9.3 The perf claim (Slice 6) is the user-facing payoff

The plan must NOT oversell. The honest framing:
- Same-actor self-send (within budget loop): memcpy eliminated, expect significant throughput win.
- Cross-actor (typical pipeline): memcpy moved from send-time to activation-end fork. Total memcpy bytes is approximately the same; CPU spike pattern changes (batched at activation end, instead of per-send).
- The structural win: the runtime now composes cleanly with γ-phase (Frozen-shared) and §12 (capability-transfer). δ-phase is the substrate, not the destination.

The bench output is committed to `.tmp/perf/` and referenced in the closing PR body; do NOT inline `lane`/`δ-phase` framing into the commit messages or PR body per `feedback_no_orchestration_framing_in_commits`.

---

## 10. Risks

Every risk from `per-actor-memory-model-design.md` §354+ is addressed:

| Risk | Addressed in |
|---|---|
| `#[max_heap]` per-batch semantics gap | Slice 0 documentation invariant (carried from Q28); unchanged by δ |
| **δ-phase arena-reset entanglement (Q28.6)** | **§6 — resolved as COW fork on activation end; fail-closed via `HEW_TRAP_FORK_OOM`** |
| Tier A unbounded growth | Out of scope (Q28.1); door closed |
| HeapExceeded on WASM longjmp gap | §6.5 + Slice 4 — fork OOM uses the same WASM gap pre-existing; envelope-new on WASM returns null, surfaces as `ErrOom`, no longjmp |
| Crash-path lock-arena ordering | §3.1 + §8.1 — fork pass uses cached `actor_arena` for crash branch; preserves `cleanup-all-exits` invariant |
| Fork OOM during activation end | §6.5 — new fail-closed trap `HEW_TRAP_FORK_OOM = 201`; documented + tested in Slice 4 |
| Stale receiver pinning sender arena (the option (a) hazard) | §6.2 — eliminated by picking (b): fork at sender boundary, receiver pace decoupled |
| Symbol-classification drift | Slice 5 — `scripts/jit-symbol-classification.toml` updated; `audit-completeness-via-multiple-greps` |
| Over-fork on cancel | §8.2 — over-fork is correctness-preserving; drop_glue runs once via CONSUMED-bit discriminator |

---

## 11. LESSONS triggers (cited)

| Row id | Why it applies here | Where in the plan it's applied |
|---|---|---|
| `boundary-fail-closed` | δ-phase introduces a new cross-boundary handoff (sender Tier B → receiver via envelope) and a new failure mode (fork OOM at activation end) | §6.5; Slice 4 (`HEW_TRAP_FORK_OOM`); Slice 1 (CONSUMED bit fail-closed semantics) |
| `serializer-fail-closed` | Envelope header bits cross the runtime's stability boundary; new CONSUMED bit must respect `MUST_BE_ZERO_MASK` | Slice 1 (mask widening); §E2 |
| `cleanup-all-exits` | Fork pass must fire on both normal and crash exits; cached `actor_arena` use; per-block dataflow on Slice 2 | §8.1, Slice 2 (lattice), Slice 4 (both exit paths) |
| `raii-null-after-move` | Codegen must null the source slot after `actor.send(...)` because `Terminator::Send` is a consume | Slice 3 (`nullOutDropSlot` after send) |
| `ffi-ownership-contracts` | New envelope refcount-transfer contract across `hew_msg_envelope_new_arena` / `hew_actor_send_aliased` / `hew_msg_node_free` | Slice 1, Slice 3 |
| `native-wasm-parity` | Every change to envelope FFI must mirror to `mailbox_wasm.rs`; parity tests must extend | Slices 1, 3, 4, 5 |
| `producer-bridge-before-codegen` | Slice 3 (codegen flip) cannot dispatch until Slice 2 (MIR producer) is live | Slice ordering: 2 → 3 |
| `behavioral-regression-not-just-test-pass` | Removing the fail-closed panic in `hew_*_send_aliased` is an observable-error-semantics change | Slice 3 (call out unchanged ErrOom semantics) |
| `checker-codegen-pattern-contract` | Move-checker rejects `actor.send(x); use(x)` before codegen sees it | Slice 2 |
| `dead-surface-sweep` | Legacy Tier-C deletion in Slice 5 must enumerate every call site | Slice 5 (§E8 lists them) |
| `audit-completeness-via-multiple-greps` | Slice 5's delete pass requires multiple grep passes across crates | Slice 5 |
| `preflight-perf-discipline` | Slice 6 bench must be a repo target, not ad-hoc bash | Slice 6 |
| `type-info-survival` | Send-site metadata threaded from checker to MIR, not re-derived | Slice 2 (`hew-types/src/check/`) |
| `plan-prescribes-helper-implementer-honors` | Slice 3 codegen MUST call `hew_msg_envelope_new_arena` and `nullOutDropSlot`; bespoke inline is a revision-required defect | Handoff |

---

## 12. Composition with γ-phase, §12, and the v0.6 boundary

Not part of v0.5, but the design must compose cleanly with what comes next. The δ-phase substrate naturally extends:

- **γ-phase (Frozen-shared)**: `HEW_MSG_ENVELOPE_SHARED_FROZEN = 1 << 1` (`mailbox.rs:156`). An envelope marked Frozen never forks (the type is read-only — `Arc`-like in v0.6). The fork pass at activation end skips Frozen envelopes. Trivial extension.
- **§12 capability transfer**: `HEW_MSG_ENVELOPE_CAPABILITY_TRANSFER = 1 << 4` (`mailbox.rs:162`). Same fork-pass exclusion rule; capability transfer has its own runtime tracking.
- **v0.6 `Arc<T>`**: not part of any envelope path. Lives in Tier A. Out of scope.

The plan does NOT pre-build hooks for these; the existing reserved bits are sufficient.

---

## 13. Out of scope

Explicit list — closing drift doors:

- Hibernation arena policy (Q28.5). Independent design pass.
- Tier A cap (`#[max_state(N)]` or any analog). Q28.1; permanent non-goal as currently phrased.
- Auto-`Send` trait inference. `MirCheck::ActorSendEscape` construction is scaffolded (Slice 2) but never fires in v0.5. Real `Send` enforcement is a v0.6 surface.
- Remote / distributed actor messaging. v0.6 substrate.
- `Arc<T>` or shared-mutable cross-actor types.
- γ-phase (Frozen-shared) implementation.
- §12 capability transfer implementation.
- Per-message arena reset (Q28.3). Per-activation reset is preserved as-is.
- `hew_duplex_send` migration to envelope path. Duplex is a different boundary; it stays libc-copy in v0.5.
- Changes to D24-1, D24-2, or D24-3 contracts.
- Performance work on `mailbox_malloc` (libc envelope-struct allocation). The envelope is small and well-cacheable; no slab allocator.
- LSP / diagnostic surface for the new traps. `HEW_TRAP_FORK_OOM` surfaces via `ExitReason::ForkOom` — visible in supervisor logs but no user-facing diagnostic kind. v0.6 may add one.

---

## 14. Handoff

### 14.1 Per-slice executor tier

| Slice | Executor | Rationale |
|---|---|---|
| 1 — CONSUMED bit | `hew-implementer` (Sonnet) | Mechanical: add bit, widen mask, gate drop_glue, mirror WASM. ~60 lines. |
| 2 — MIR producer + consume effect | `hew-implementer-opus` | Cross-layer: HIR → MIR producer, dataflow consumer, checker metadata. Cross-layer ownership reasoning. |
| 3 — Codegen flip | `hew-implementer-opus` | Cross-layer: codegen + runtime + symbol allowlist. Concurrent-ownership reasoning. |
| 4 — Fork-on-activation-end | `hew-implementer-opus` | Cross-thread Mutex protocol + new C-ABI trap + scheduler hot path. Maximum risk slice. |
| 5 — Delete legacy | `hew-implementer` (Sonnet) | Mechanical sweep across ~40 call sites + WASM mirror + symbol classification update. |
| 6 — Benchmark | `hew-implementer` (Sonnet) | Standard `criterion`/`cargo bench` harness. |

### 14.2 Per-slice cross-ecosystem reviewer

All slices: **Copilot (GPT-5.5)** per `feedback_cross_model_implementer_default`. Mandatory for slices 1, 3, 4, 5 (C-ABI surface). Optional for slices 2 and 6.

### 14.3 PR sequencing

Each slice is one PR per `feedback_orchestration_tooling_location` and the v0.5 local-orchestration pattern. Slices 1–4 squash-merge into `v05-integration` locally (no public PR until v0.5 publishes per `project_v05_local_orchestration_pattern`). Slices 5 and 6 same.

### 14.4 Slice-ordering gate

```
Slice 1 ─┐
         ├─→ Slice 3 ─→ Slice 4 ─→ Slice 5 ─→ Slice 6
Slice 2 ─┘
```

Slices 1 and 2 can run in parallel on sibling worktrees (no shared files). All others are sequential.

### 14.5 Closeout questions (applies after Slice 4 + 5 + 6)

Per CLAUDE.md and `feedback_review_before_pushing`:

1. Did this work reveal a new durable lesson? Candidate: "Refcounted-envelope refcount-discriminator pattern: when a runtime release fn cannot tell consumed-vs-discarded paths apart, add a header-bit set by the consume site before release, and gate drop_glue on it." If LESSONS does not already cover this pattern (`ffi-ownership-contracts` is close but does not name the header-bit discriminator technique), add a new row in the closeout. P0 band.
2. Did this work reveal a new arena/scheduler invariant? Candidate: "When envelope lifetime crosses an arena reset boundary, fork at the reset boundary (sender-owned) — never defer the reset to the envelope's release (receiver-owned)." Close call vs `boundary-fail-closed`; strengthen `apply` field instead of new row.
3. Verify `behavioral-regression-not-just-test-pass` was applied for Slice 3 (removing the fail-closed panic is an observable-semantics change — the new behaviour must be correct, not just non-panicking).

---
