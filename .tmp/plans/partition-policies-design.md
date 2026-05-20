# Partition Policies — Design Pass (Q29)

## Status: Superseded — cut from v0.5 scope

Cut by `.tmp/orchestration/v05-strategy-consult-2026-05-19.md:72–79`. Defer to
v0.6.

**Status:** design memo — not yet dispatchable as an implementation lane.
**Worktree:** `/Users/slepp/projects/hew-lang/hew/.claude/worktrees/v05-integration`
**Milestone scope:** Pre-M3. Surface is pre-distributed; single-node runtime only.
**Authored:** 2026-05-18
**Tenets served:** Reliability first (1), Actor model + supervision first-class (2), Substrate over surface (3).

---

## 1. Goals

1. Define the complete taxonomy of partition events a Hew program can observe — today (single-node, v0.5) and at M3 (multi-node).
2. Specify how an actor or supervisor expresses a partition-response policy in Hew source code — the syntax/attribute shape, not just the runtime mechanism.
3. Answer the fail-closed question: what does the compiler or runtime do when an actor handles a `RecvError::PartitionDetected` arm and no policy is declared?
4. Show how partition policy composes with the three v0.5 substrate pieces already planned: D24-1 auto-injected locks, D24-2 cancellation tokens, and the supervisor restart cap.
5. Establish which work belongs in the IR ladder (compiler-owned guarantee) vs runtime-only convention.

---

## 2. Non-goals

- Distributed consensus, quorum, or placement durability. These are far beyond v0.5 and explicitly out of scope for the HEW-DIST-SPEC v0 (§1 "Out of scope for v0").
- Peer-id payloads on `PartitionDetected`. The duplex substrate already marks this as future work (v0.5 unit-variant justified at `hew-runtime/src/duplex.rs:133-143`; peer-id expansion named for M3).
- `MonitorLost` handling as a policy attachment point. `MonitorLost` is a separate distributed-monitoring error (HEW-DIST-SPEC §6); it is named as an open question below (Q29.d) but is not part of this design.
- `Suspect`/`Dead` failure-detector observation hooks. The detector model (φ-accrual/SWIM-Lifeguard family, HEW-DIST-SPEC §9) is native-only and multi-node; in-band observation hooks for these transitions belong in a separate distributed-surface lane, not here.
- Cross-node link. HEW-DIST-SPEC §8 says cross-node link is opt-in and not designed yet; partition policy for cross-node link depends on decisions made there.
- WASM runtime. HEW-DIST-SPEC §15 gates `Node::*` as native-only. Partition attributes must parse on WASM targets (cross-platform parity) but the underlying semantics are rejected at compile time for `Node::*`-bound operations per §15. This memo does not design WASM stubs for partition behaviour.
- User-facing `Arc<T>`, hierarchical machine states, channels as a separate source surface — all v0.6 boundary per the mission file.

---

## 3. Current State Inventory

### 3.1 Runtime substrate: `RecvError::PartitionDetected`

`hew-runtime/src/duplex.rs:112-144` defines `RecvError` as a stable `#[repr(i32)]` C-ABI type with four discriminants:

```
RecvError::Ok             = 0
RecvError::Closed         = 1
RecvError::Empty          = 2
RecvError::PartitionDetected = 3
```

`PartitionDetected` (duplex.rs:124-143) documents:
- The peer node is unreachable (heartbeat timeout or explicit injection).
- Unlike `Closed`, the partition may heal. Application layer decides retry or escalation.
- In v0.5 (single-node), reachable only via `Queue::force_partition_for_test` (`cfg(test)`) or future multi-node peer machinery.
- The discriminant is a unit variant by design: `#[repr(i32)]` + stable ABI; peer-id payload deferred to M3.
- The shim annotation explicitly names when it becomes obsolete and what the real solution looks like (duplex.rs:138-143).

Test injection: `Queue::force_partition_for_test` (duplex.rs:217-219) stores true into the `AtomicBool partitioned` flag (only compiled in `cfg(test)`). The `HewDuplex::force_partition_for_test` wrapper (duplex.rs:496-502) delegates to this.

### 3.2 SimTransport: network-level partition injection

`hew-runtime/src/sim_transport.rs:907-934` — `sim_transport_set_partition(transport, partitioned: bool)` toggles the `AtomicBool partition` on a `SimTransportState`. While set, every `send` and `recv` returns `Partitioned` (sim_transport.rs:67, 473-474, 608-609). This is the mechanism `HEW-DIST-SPEC §14` requires for deterministic distributed testing.

### 3.3 HEW-DIST-SPEC §6 error taxonomy

The distributed spec (docs/specs/HEW-DIST-SPEC.md:137-151) defines `Partition` as the typed failure for lookup, send, ask, and monitor when "the node cannot currently establish or maintain the route required for the operation." This is the multi-node counterpart to `RecvError::PartitionDetected` on the duplex channel.

Anti-pattern §17.5 (dist-spec line 387): "making a partition look like ordinary local absence" is forbidden.

### 3.4 Supervisor restart substrate

`hew-runtime/src/supervisor.rs` — relevant constants and shapes:

- `RESTART_PERMANENT = 0`, `RESTART_TRANSIENT = 1`, `RESTART_TEMPORARY = 2` (lines 318-320)
- `restart_times: [u64; MAX_RESTARTS_TRACK]`, `restart_count: usize` (lines 539-540) — a rolling ring-buffer of restart timestamps
- `restart_within_window` (lines 728-745) — counts restarts within the supervisor's window; used by `restart_with_budget_and_strategy` to gate escalation
- `record_restart` (lines 748-755) — stamps a new entry into the ring buffer on every restart

Currently, every actor restart (whether caused by a code panic, a trap, or — in the future — a partition-induced crash) increments `restart_count` the same way. There is no per-cause restart budget.

### 3.5 Auto-injected locks substrate (D24-1)

`hew-runtime/src/actor.rs:222` — `hew_actor_state_lock_acquire`. The auto-locks plan (`auto-injected-locks.md`) establishes that the state lock is held for the duration of a handler body. On a handler crash (trap), the supervisor takes over after the guard releases — this is the existing crash-isolation path. A partition-triggered policy must define what happens to the in-flight lock.

### 3.6 Cancellation tokens substrate (D24-2)

`hew-runtime/src/task_scope.rs:82-106` — `HewCancellationToken`, a ref-counted, tree-structured cooperative-cancellation primitive. The cancellation plan (`cancellation-tokens.md`) establishes that cancel is scope-attached: the parent calls `hew_cancel_token_request` and children observe at cooperate-sites. A "surrender" partition policy = cancel the actor's scope token and let the supervisor handle the restart.

### 3.7 Parser/AST: what exists

`hew-parser/src/ast.rs:1158-1281` — existing actor and supervisor shapes:

- `ActorDecl` has `mailbox_capacity`, `overflow_policy` (OverflowPolicy enum: DropNew, DropOld, Block, Fail, Coalesce), `is_isolated`, `max_heap_bytes`. No partition-related field.
- `SupervisorDecl` has `strategy: Option<SupervisorStrategy>` (OneForOne, OneForAll, RestForOne, SimpleOneForOne), `max_restarts`, `window`. No partition-related field.
- `ChildSpec` has `restart: Option<RestartPolicy>` (Permanent, Transient, Temporary). No partition-related field.
- Attribute syntax (`#[name]`, `#[name(args)]`) is parsed generically — no special attribute for partition policy exists.

---

## 4. Design Alternatives

### Alternative A: Exhaustive-match enforcement — no new surface

**Premise:** The null hypothesis. `RecvError::PartitionDetected` is already in the exhaustive recv-error discriminant set. The checker enforces exhaustive match on `RecvError`. An actor that receives on a duplex must pattern-match all variants, including `PartitionDetected`. The response logic — retry, escalate, trap — is plain code inside the match arm. Policy is expressed as program logic, not as a language-level attribute.

**Critical finding from inventory:** As of the current main/v05-integration tip, `Duplex::recv` is **not yet wired in Hew codegen**. `hew-codegen-rs/src/llvm.rs:850-858` contains an explicit TODO: "Wiring lands in follow-on slices (Duplex::recv vertebra for follow-on Duplex::recv + lambda-actor lanes)." `RecvError` is a pure Rust runtime ABI type (`hew-runtime/src/duplex.rs:112-144`) with no Hew-language binding in the stdlib (verified: `std/` contains no `.hew` file mentioning `RecvError`, `PartitionDetected`, or `Duplex` recv). There is no `hew_duplex_recv` in the codegen ABI surface table (`llvm.rs:458-480`).

This means Alternative A's "exhaustive-match enforcement" mechanism does not exist in v0.5. The Hew checker cannot enforce `RecvError` pattern exhaustiveness because Hew source code cannot currently call `duplex.recv()` and observe its typed error result. The recv path is an ABI stub ("follow-on Duplex::recv vertebra").

**Revised premise (truthful):** Alternative A requires, as a precondition, that `Duplex::recv` be surfaced as a Hew-callable method returning a typed `RecvError`-equivalent enum. Until that vertebra lands, Alternative A is not exercisable. It is the correct long-run design but cannot be enforced today. The wildcard-arm problem is also real: if Hew allows `_ => ()` arms and there is no checker rule specifically blocking wildcard coverage of `PartitionDetected`, exhaustive match alone does not guarantee fail-closed behaviour.

**Fail-closed guarantee:** Only reachable once `Duplex::recv` is wired. At that point, the checker must: (1) enforce exhaustive match on the returned error type, and (2) either warn or hard-error on a wildcard arm that covers `PartitionDetected` without explicit handling. If neither is added at wire time, the fail-closed guarantee is programmer discipline, not a compiler invariant.

**Composition with D24-1 (auto-locks):** The lock is held during the handler body. If the `PartitionDetected` arm traps, the crash-isolation path releases the lock (existing path). If the arm returns normally (e.g., retry), the lock stays held through the retry loop — the actor-state guard is not released mid-handler. Backpressure from a stalled retry would manifest as a hung mailbox drainer, which the reductions budget (RB-3) bounds.

**Composition with D24-2 (cancellation):** The actor explicitly calls `cancel(self.scope_token)` in the `PartitionDetected` arm if it wants to surrender and let the supervisor restart it. No new mechanism needed.

**Composition with supervisor restart cap:** A `PartitionDetected`-triggered trap counts toward the supervisor's `restart_count` ring buffer identically to a code crash. This is transparent but may be surprising: a flapping network partition burns the actor's restart budget the same as a bug.

**Tradeoffs:**
- Pros: Zero additional surface beyond `Duplex::recv` wiring. Substrate-over-surface bias satisfied. No new parser, HIR, or codegen changes beyond the recv vertebra itself. `force_partition_for_test` already provides test injection. Exhaustive-match enforcement is the right long-run policy guarantee.
- Cons: Not enforceable until `Duplex::recv` is wired in codegen (currently absent). Wildcard arm problem requires an additional checker rule. Policy is scattered across every actor that touches a duplex. No compiler-level propagation to the supervisor. Partition budget conflated with restart budget (Q29.a).

### Alternative B: Per-actor attribute on the declaration

**Premise:** Add a `#[partition(policy)]` attribute to `ActorDecl`. The checker reads it, verifies it is declared wherever the actor uses a `Duplex` (or, conservatively, on every actor declaration). Codegen emits the appropriate handler frame depending on the chosen policy.

Example syntax:
```
#[partition(fail_closed)]     // trap on PartitionDetected (default / implicit)
#[partition(surrender)]       // cancel scope, let supervisor restart
#[partition(retry(max = 3))]  // retry recv up to N times, then surrender
#[partition(degrade)]         // continue with degraded state, caller-visible
actor OrderProcessor { ... }
```

**Fail-closed guarantee:** The checker rejects any `actor` declaration that touches a `Duplex` and lacks a `#[partition(...)]` attribute. The implicit default is `fail_closed` — the checker emits a lint (or hard error) so that the actor author makes a deliberate choice. This is the "every actor declares a policy or inherits one — fail-closed if neither" guarantee.

**Composition with D24-1 (auto-locks):** Each policy variant has a defined lock-release semantics:
- `fail_closed`, `surrender`: lock released via existing crash-isolation path (trap path).
- `retry`: lock held across retry iterations. Reductions budget bounds stall.
- `degrade`: lock released normally; handler returns a `PartitionedResult` type to the caller.

**Composition with D24-2 (cancellation):** `surrender` and `retry`-exhausted call `hew_cancel_token_request` on the actor's scope token. `degrade` leaves the token live. The cancellation substrate owns propagation.

**Composition with supervisor restart cap:** Partition-triggered restarts still count toward `restart_count` unless a new `partition_restart_budget` field is added to `HewSupervisor` (open question Q29.a). The attribute could carry `#[partition(fail_closed, budget = 5)]` to configure a separate counter, but this adds runtime state.

**Tradeoffs:**
- Pros: Every actor's partition stance is visible at the declaration site. Checker can emit "no partition policy declared" as a warning or error. Auditable.
- Cons: Parser changes (`ActorDecl` gains a new optional field or the attribute is generic). HIR must understand and propagate the attribute. Codegen must branch on policy. Complex interaction with inheritance (what if the actor is supervised by a supervisor that also declares a partition policy?). `retry` and `degrade` semantics need runtime machinery (retry counter, degraded state propagation path). Risk of over-surface before the multi-node substrate exists to exercise these paths.

### Alternative C: Per-supervisor child-spec attribute

**Premise:** Partition policy belongs at the supervision boundary, not on the actor declaration. The supervisor child spec — already the carrier of `RestartPolicy` — gains a `partition_policy` field. The supervisor owns partition-response authority the same way it owns restart authority.

Example syntax (in supervisor body):
```
supervisor OrderSupervisor {
  child processor: OrderProcessor(db)
    restart: permanent
    partition: surrender   // supervisor cancels and restarts on PartitionDetected escalation
}
```

The actor does not declare a policy. It either matches `PartitionDetected` and traps (which the supervisor observes as a crash), or calls an intrinsic `escalate_partition()` which sends a structured message to the supervisor. The supervisor then applies its declared policy.

**Fail-closed guarantee:** If `partition` is omitted from a child spec, the implicit default is `surrender` (actor traps, supervisor restarts). This is fail-closed: partition is never silently ignored. The checker can emit a note when partition policy is implicitly defaulted.

**Composition with D24-1 (auto-locks):** The actor always traps or surrenders on `PartitionDetected`. The crash-isolation path handles lock release. The supervisor policy governs what comes next.

**Composition with D24-2 (cancellation):** The `escalate_partition()` intrinsic is sugar for: post a system message to the supervisor mailbox; the supervisor reads it and calls `hew_cancel_token_request` on the actor's scope token. Follows existing supervisor system-message infrastructure (`SYS_MSG_DELAYED_RESTART` pattern at supervisor.rs:454-468).

**Composition with supervisor restart cap:** The supervisor can distinguish partition-triggered restarts from code-crash restarts via the message type, enabling a separate `partition_restart_budget` (Q29.a) to be defined without changing actor semantics.

**Tradeoffs:**
- Pros: Policy authority is co-located with restart authority. Supervisor can distinguish partition from code crash. Enables separate partition restart budget. No per-actor attribute needed. Consistent with Erlang/OTP heritage (supervisors own policy, not actors).
- Cons: Actors cannot independently decide to degrade (partition-tolerant actors that want to serve stale data need a different path). Requires new child-spec field in parser/AST and new system message type in supervisor. Not exercisable until multi-node peer machinery exists.

---

## 5. Recommended Design

**Recommendation: Phased — A preconditions at Duplex::recv wiring, then C at M3.**

This is not a hybrid of two simultaneous approaches. Phase 0 is infrastructure; Phase 1 is enforcement; Phase 2 is the supervisor attribute surface.

### Phase 0 (within v0.5, at Duplex::recv wiring): Establish the typed-error surface

The mandatory precondition for any partition policy is that `Duplex::recv` is surfaced as a Hew-callable method returning a typed error enum. This vertebra is explicitly future-work-annotated at `hew-codegen-rs/src/llvm.rs:850-858`. Until it lands, partition policy is not expressible in Hew source code at all.

When the `Duplex::recv` vertebra lands, it must:
1. Return a Hew-visible typed error (e.g. `RecvResult<T>` with variants `Ok(T)`, `Closed`, `Empty`, `PartitionDetected`), not a raw `i32` ABI discriminant that the Hew program cannot name.
2. The checker must enforce exhaustive match on this error type at every recv call site.
3. The checker must warn or hard-error when a wildcard arm (`_ =>`) covers `PartitionDetected` without a specific arm, so "fail-closed by default" is not defeatable by a careless wildcard.

**This is the policy substrate. Without it, all subsequent design is aspirational.**

### Phase 1 (v0.5, post Duplex::recv wiring): Exhaustive-match enforcement as the policy

Do not add any new attribute, trait, or supervisor field in v0.5 beyond Phase 0. The exhaustive-match requirement on the recv error type is the entire policy surface.

The fail-closed guarantee: the checker rejects non-exhaustive matches on the recv error enum, including wildcard arms that cover `PartitionDetected`. An actor that calls `recv()` and does not explicitly handle `PartitionDetected` is a compile-time error. The action inside the `PartitionDetected` arm is code — there is no implicit ignore path.

For v0.5, all partition-handling code in actors should trap in the `PartitionDetected` arm. This is the right default because:
1. There is no healed-partition machinery to retry against.
2. Letting the supervisor restart is the correct fail-closed posture.
3. It composes cleanly with D24-1 (lock released on crash-isolation path) and D24-2 (supervisor cancel token fires).

Example actor code (Hew source, not implementation code — shows the composition story):
```
receive fn handle_response(self, response: Response) {
    match self.backend.recv() {
        Ok(data) => { /* process data */ }
        Err(PartitionDetected) => {
            // fail_closed: trap and let supervisor restart
            trap("backend partition detected")
        }
        Err(Closed) => stop()
        Err(Empty) => { /* try again next tick */ }
    }
}
```

The checker must enforce that the `Err(PartitionDetected)` arm is present. A wildcard `Err(_) => trap(...)` is permissible only if the checker can statically verify the wildcard cannot silently succeed — meaning any wildcard covering `PartitionDetected` emits at minimum a lint at warning level.

### Phase 2 (M3, multi-node): Supervisor child-spec `partition` attribute (Alternative C)

When the multi-node peer layer lands and `PartitionDetected` can be raised by real heartbeat timeout or connection loss, the supervisor child spec gains:

```
partition: fail_closed    // trap, supervisor restarts (default — same as Phase 1)
partition: surrender      // escalate structured message, supervisor restarts
partition: budget(N)      // separate partition restart budget, escalates after N
```

The `degrade` / `retry` use-cases belong in the actor's own code in the `PartitionDetected` arm, not in a policy attribute. Codifying them as attributes would require runtime machinery (retry counter, degraded-mode flag) that duplicates what the actor can do in plain Hew code. Substrate-over-surface: policy authority belongs at the supervision boundary; actor-side degrade logic is application code.

### Fail-closed guarantee (explicit answer)

**Before `Duplex::recv` is wired (today):** The fail-closed guarantee does not exist in Hew source code. Partition events are only observable in Rust test code via `force_partition_for_test`. This is not a regression — the recv surface is stub-annotated as future work.

**After `Duplex::recv` is wired (Phase 0 + Phase 1):** The checker enforces exhaustive match on the recv error enum. There is no runtime default that silently handles `PartitionDetected`. An actor that does not have a `PartitionDetected` arm is a compile-time error. A wildcard arm covering `PartitionDetected` without an explicit arm is a lint-level warning that must be acknowledged. This is the compiler-owned guarantee.

**In Phase 2 (M3, supervisor attribute):** The supervisor's implicit default for unspecified `partition:` in a child spec is `fail_closed` (actor traps, supervisor restarts), with a checker note that the policy is implicit. An explicit declaration is encouraged; the checker emits a note when `partition_policy` is absent from a child spec whose actor type uses a Duplex.

### Is policy per-actor, per-supervisor, or both?

Policy is per-supervisor-child-spec (at M3). In Phase 0 and Phase 1, it is implicitly per-actor (the match arm is actor code). The recommended design does not layer per-actor attributes and supervisor attributes — that dual-attribution leads to conflicting sources of authority and makes auditing impossible.

### Stdlib trait or codegen + runtime?

No stdlib trait is needed. The policy is either:
1. (Phase 1) A pattern-match arm — pure code, no infrastructure.
2. (Phase 2) A child-spec field in the supervisor — a `c_int` discriminant stored in `HewActorSpec` alongside `restart_policy`, consumed by `restart_with_budget_and_strategy` when a `PartitionDetected`-triggered crash notification arrives.

Introducing a `PartitionPolicy` trait (Alternative B's trait-impl variant) would require trait dispatch at the supervision boundary, which is an HKT/dispatch problem the compiler is not yet required to solve for internal runtime contracts. The discriminant-in-child-spec approach is consistent with how `restart_policy` is already handled.

---

## 6. Composition with D24-1 Auto-Locks + D24-2 Cancellation + Supervisor Restart Cap

### 6.1 Auto-injected locks (D24-1)

**Key seam:** `hew_actor_state_lock_acquire` wraps the entire handler body. The lock is released when the handler returns or when the crash-isolation path fires.

- **fail_closed (Phase 1 default):** The `PartitionDetected` arm traps. The trap fires the crash-isolation path. The actor state lock is released via the existing `hew_actor_state_lock_poison_after_panic` mechanism. No new path required.
- **degrade (actor-code pattern, not a declared policy):** The handler returns normally after setting a degraded-mode flag in actor state. The lock is released at normal handler return. The auto-lock mechanism does not care about the partition event.
- **retry (actor-code pattern):** The lock is held across the retry loop because the actor is still inside the handler body. The reductions budget cooperate-site emission (RB-3, already landed) bounds how long a retry loop can spin before yielding. This is the critical interaction: an infinite retry in the `PartitionDetected` arm while holding the state lock is a liveness hazard. The reductions budget is the mitigation; the design memo should flag this and the implementer should add cooperate sites in the recv-retry pattern.

### 6.2 Cancellation tokens (D24-2)

**Key seam:** `hew_cancel_token_request` on the actor's scope token signals child tasks to stop.

- **fail_closed / surrender:** The actor traps or explicitly calls the surrender intrinsic. The supervisor (on restart) creates a new scope token for the replacement actor. The old token tree is invalidated when the crashed actor's scope is torn down.
- **degrade:** The actor continues normally. Its scope token stays live. Child tasks spawned inside the actor (if any) remain cancellable by the supervisor independently.
- **Partition-triggered supervisor cancel:** In Phase 2, when the supervisor receives a partition escalation message, it can call `hew_cancel_token_request` on the actor's scope token directly (same mechanism as a graceful shutdown) before restarting. This is consistent with how supervisor-initiated stop works today.

### 6.3 Supervisor restart cap

**Key seam:** `record_restart` (supervisor.rs:748-755) stamps every restart into the ring buffer. `restart_within_window` (lines 728-745) gates escalation.

**Open issue (Q29.a):** Partition-triggered restarts currently burn the same restart budget as code crashes. A network partition that flaps 10 times exhausts the restart cap and escalates to the parent supervisor, which may kill an entire supervision subtree for a network condition that will heal. This is undesirable.

**Proposed resolution:** In Phase 2, the supervisor child spec gains `partition_restart_budget: Option<u32>` alongside `partition: PartitionPolicy`. When a crash arrives via a partition escalation message (distinguishable from a code crash by message type), the supervisor checks the partition budget rather than the main restart budget. The main `restart_count` ring buffer is not incremented for partition-originated restarts until the partition budget is also exhausted. This requires:

1. A new `partition_restart_count` and `partition_restart_times` ring in `HewSupervisor` (or a separate `partition_restart_budget` counter).
2. A new system message discriminant alongside `SYS_MSG_DELAYED_RESTART` for partition-originated restarts.
3. The `restart_with_budget_and_strategy` function to branch on message type.

This is Phase 2 work. In Phase 1, the current behavior (partition burns the code-crash budget) is acceptable because `PartitionDetected` is only reachable via test injection.

---

## 7. Open Questions (Q-tag Candidates)

**Q29.0 — Duplex::recv vertebra: where is it tracked?**

`Duplex::recv` wiring in codegen is explicitly future-work (hew-codegen-rs/src/llvm.rs:850-858: "Wiring lands in follow-on slices (Duplex::recv vertebra)"). The partition-policy design depends on this as its Phase 0 precondition. What GitHub issue or lane is this tracked under? If none exists, a new issue should be filed before this design is considered actionable. All partition-policy enforcement is blocked on this vertebra landing.

**Q29.a — Partition restart budget vs code crash budget**

Do partition-triggered restarts count toward the supervisor's main `restart_count` ring buffer, or should there be a separate `partition_restart_budget` counter? If shared, a flapping network partition exhausts the crash budget and kills the supervision subtree.

Proposed answer (Phase 2): separate budget, separate message type, separate ring. The `HewSupervisor` grows a second counter pair. Implementation tracked when Phase 2 lands.

**Q29.b — Policy attachment site: child-spec override of actor default**

If both the actor declaration (`#[partition(P)]` attribute, Alternative B) and the supervisor child spec (`partition: Q`) declare a policy, which wins? The recommended design avoids this conflict entirely by making Phase 1 policy-free (no attribute) and Phase 2 supervisor-only. However, if Alternative B is revisited for per-actor degradation logic, the resolution rule must be: supervisor child spec wins over actor attribute, because supervisors own restart authority.

**Q29.c — Heal notification**

When a partition heals (the peer becomes reachable again), should the actor receive a `PartitionHealed` notification, or only discover this by attempting the next recv and succeeding? The current `RecvError` discriminant set has no `PartitionHealed` variant. HEW-DIST-SPEC §9 says the peer re-enters via a fresh negotiated session. A heal notification would require a new recv-success variant or an out-of-band system message. This belongs in the M3 distributed surface design, not here.

**Q29.d — `MonitorLost` as a partition event for policy purposes**

HEW-DIST-SPEC §6 defines `MonitorLost` as what observers receive when a monitor relationship cannot be maintained across restart, partition, or teardown. Should the supervisor child-spec `partition` attribute apply when the supervisor receives `MonitorLost` for a remote child, or only when the actor's own recv channel raises `PartitionDetected`? These are distinct events: one is a monitoring-relationship event, the other is a channel-receive error. Recommend treating them as separate policy hooks in Phase 2 design, not conflating them here.

**Q29.e — WASM compile-time gate scope**

HEW-DIST-SPEC §15 requires a compile-time rejection when WASM programs use `Node::*`. If a `partition:` child spec attribute is added to supervisors in Phase 2, should WASM programs that declare `partition: surrender` be rejected (because surrender implies a restart policy that may not be well-defined for WASM actors with no distributed peer), or accepted (because local supervisors work on WASM and the attribute is inert for non-distributed actors)? This is a surface boundary question for the M3 design phase.

---

## 8. Implementation Outline (IR Ladder Placement)

### Phase 0 (v0.5, within Duplex::recv wiring lane) — establish the typed error surface

This is the mandatory precondition. It belongs in the `Duplex::recv` vertebra lane, not as a separate partition-policies lane:

- **Stdlib / builtin registration:** Define a Hew-visible enum type for recv errors (e.g. `RecvResult<T>` or `DuplexError`). This lives either in `std/` as a `.hew` file or in the builtin type registry (`hew-hir/src/builtin_type_classes.rs`). The type must expose `PartitionDetected` as a named variant, not as a raw integer the user cannot match on.
- **Codegen-rs:** Wire `hew_duplex_recv` into the ABI surface table (`llvm.rs`, alongside `hew_duplex_send` at line 469). The recv intrinsic maps the C-ABI `i32` discriminant to the Hew-visible enum type.
- **HIR / checker:** When the resolved HIR sees a recv call on a `Duplex`, it must: (a) resolve the return type to `RecvResult<T>`, (b) enforce exhaustive match at every use site, (c) detect and warn/error when a wildcard arm covers `PartitionDetected`.
- **No AST changes.** No new attribute field on `ActorDecl` or `SupervisorDecl`.
- **Runtime:** `force_partition_for_test` and `sim_transport_set_partition` are already present. No runtime changes needed for Phase 0.

### Phase 1 (v0.5, post Duplex::recv wiring) — checker enforcement

The only additional work beyond Phase 0 is verifying the checker diagnostic fires correctly:

- **THIR:** When a `recv()` result is matched, THIR carries the `RecvResult<T>` type. The match elaboration in THIR or Raw MIR verifies all arms.
- **Checker diagnostic:** "recv result missing `PartitionDetected` arm" diagnostic with a clear suggestion to add the arm and trap or surrender.
- **Wildcard diagnostic:** "wildcard arm covers `PartitionDetected` — use an explicit arm to ensure fail-closed handling" diagnostic at lint-warning level.

### Phase 2 (M3+) — supervisor child-spec partition attribute (AST → checker → supervisor runtime)

If the supervisor child-spec attribute (Alternative C / recommended Phase 2 design) is implemented:

1. **AST (hew-parser/src/ast.rs):** `ChildSpec` gains `partition_policy: Option<PartitionPolicy>`. `PartitionPolicy` is a new enum: `FailClosed`, `Surrender`, `Budget(u32)`.
2. **Parser:** `child name: Type restart: ... partition: ...` syntax lowered into `ChildSpec`.
3. **Resolved HIR:** Checker validates `partition_policy` is a known variant. Emits a note when `partition_policy` is None and the actor type's receive functions handle a `Duplex` (the checker would need to track which actors are duplex-connected, which requires a dataflow analysis or an explicit annotation).
4. **THIR/MIR:** No structural changes needed. The policy is supervisor metadata, not an actor value.
5. **Elaborated MIR:** If a `Budget(N)` policy is declared, the MIR may need to propagate a partition-restart-count operand alongside the standard restart-count; or this can remain purely runtime state.
6. **Codegen-rs:** The child-spec C struct gains a `partition_policy: c_int` field. Codegen writes this field during supervisor tree initialisation. New discriminant constant: `PARTITION_FAIL_CLOSED = 0`, `PARTITION_SURRENDER = 1`, `PARTITION_BUDGET = 2`.
7. **Runtime (hew-runtime/src/supervisor.rs):** `HewActorSpec` gains `partition_policy: c_int` and optional `partition_restart_budget: u32`. `restart_with_budget_and_strategy` branches on whether the inbound crash message is a partition-originated or code-crash notification. `record_restart` gains a parallel `record_partition_restart` path.
8. **New system message:** A discriminant alongside `SYS_MSG_DELAYED_RESTART` for partition-originated crashes; e.g. `SYS_MSG_PARTITION_TRIGGERED = 106`.

---

## 9. Validation Strategy

### Phase 0 validation (at Duplex::recv wiring)

**Duplex-level partition injection (already present, cfg(test) only):**

- `hew-runtime/src/duplex.rs` — `Queue::force_partition_for_test` + `HewDuplex::force_partition_for_test` inject `PartitionDetected` on the next recv. These exist but are only reachable from Rust test code; no Hew source can call `recv()` yet.
- When `hew_duplex_recv` is wired in codegen, the runtime path is already exercisable at the Rust level. The test infrastructure (`hew-runtime/tests/sim_transport_property.rs`) exercises partition at the transport level.
- The duplex-level partition injection landed in commit `4dce8d24`.

**Checker validation (part of the Duplex::recv vertebra lane):**

- `hew-analysis` / `tests/v05-vertical-slice/reject/`: add a test that attempts to match a `recv()` result with a missing `PartitionDetected` arm and asserts the expected diagnostic.
- Add a matching accept test: exhaustive match with explicit `PartitionDetected => trap(...)` arm compiles without warning.
- Add a reject test: wildcard arm `Err(_) => ()` covering `PartitionDetected` emits the wildcard diagnostic.

**Composition tests (post-Phase 0):**

- Actor calls `recv()`, gets `PartitionDetected`, traps → supervisor observes `HewActorState::Crashed` → supervisor restarts → `restart_count` incremented. Assert the full sequence via Rust-level test that calls `force_partition_for_test`.
- Actor in `PartitionDetected` arm with a retry loop → assert the reductions-budget cooperate site fires (does not spin forever).

**Note:** The partition-policy validation strategy cannot be completed until `Duplex::recv` is wired. The runtime side is ready; the language surface is the blocker.

**SimTransport invariant (HEW-DIST-SPEC §14.1):**

The existing property: `sim_transport_set_partition(transport, true)` → every subsequent send/recv returns `Partitioned`, not a sentinel success. Validated in `sim_transport_property.rs`.

### Phase 2 validation (M3)

When multi-node peer machinery and the supervisor attribute land:

- Property test: a partition that flaps N times does not exhaust the code-crash restart budget (only the partition restart budget).
- Property test: `partition: fail_closed` (default) behaves identically to Phase 1 behaviour.
- Property test: `partition: surrender` + heal → actor is restarted and resumes normal recv.
- `SimTransport` partition injection (already present) provides the test harness without real network dependencies.

---

## 10. LESSONS Triggers

**`transparent-but-typed-failure` (LESSONS.md line 58):** This row is directly triggered. Anti-pattern HEW-DIST-SPEC §17.5 ("making a partition look like ordinary local absence") is the failure mode this design prevents. The apply checklist: reject value-shaped partition results; require typed failure variants; grep examples for sentinel checks; keep distributed examples handling the error branch explicitly. This row's `apply` becomes part of the execution and review checklist for any Phase 2 implementation lane.

No `P0` LESSONS rows with `trigger` matching "supervisor restart policy change" were found. The closest match is the `transparent-but-typed-failure` row above.

---

## Out of Scope

- **`Duplex::recv` wiring itself.** This design depends on it as a precondition (Q29.0), but the vertebra is a separate codegen lane. This document does not design that wiring; it specifies what the wiring must expose to support partition policy.
- Distributed consensus, quorum, placement durability (v0.6+, explicitly out of HEW-DIST-SPEC v0).
- `Suspect`/`Dead` failure-detector observation hooks for user code (M3 distributed surface design).
- Peer-id payloads on `PartitionDetected` (duplex.rs:138-143 marks this for M3 expansion).
- `MonitorLost` as a partition-policy hook (separate event, separate design — Q29.d).
- Cross-node link partition semantics (HEW-DIST-SPEC §8 says cross-node link is not yet designed).
- WASM partition stubs. WASM compile-time gate per HEW-DIST-SPEC §15 is sufficient; no stubs that pretend to handle partitions on WASM.
- Per-actor `#[partition(...)]` attribute (Alternative B). Not recommended for v0.5; revisit only if actor-side degrade patterns prove necessary after M3 observability.
- Replacing the existing `OverflowPolicy` or `RestartPolicy` enums — this design adds alongside, does not replace.
