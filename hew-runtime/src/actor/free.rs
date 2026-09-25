//! Actor drain, finalize, cleanup and free machinery.
#[allow(
    clippy::wildcard_imports,
    reason = "submodule split from a single large file; re-exports its parent scope"
)]
use super::*;

/// Typed outcome for draining a set of actors to quiescence.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DrainOutcome {
    /// Every requested actor was already gone or drained successfully.
    Drained,
    /// At least one requested actor was still live or crashed at the deadline.
    Incomplete {
        still_live: Vec<ActorId>,
        crashed: Vec<ActorId>,
    },
}

/// C ABI representation of [`DrainOutcome`].
#[repr(C)]
#[derive(Debug, Default)]
pub struct DrainOutcomeRepr {
    pub still_live_ptr: *mut ActorId,
    pub still_live_len: usize,
    pub crashed_ptr: *mut ActorId,
    pub crashed_len: usize,
}

#[inline]
pub(crate) fn actor_free_state_is_quiescent(state: i32) -> bool {
    state == HewActorState::Stopped as i32
        || state == HewActorState::Crashed as i32
        || state == HewActorState::Idle as i32
}

#[cfg(not(target_arch = "wasm32"))]
#[derive(Clone, Copy)]
struct DeferredActorFree(*mut HewActor);

#[cfg(not(target_arch = "wasm32"))]
// SAFETY: the deferred free thread only observes the raw pointer value after
// the owning dispatch thread has requested teardown.
unsafe impl Send for DeferredActorFree {}

#[cfg(not(target_arch = "wasm32"))]
fn free_deferred_actor(deferred: DeferredActorFree) {
    // SAFETY: the runtime still owns `actor`; the background thread simply
    // retries the same free once the current dispatch unwinds.
    let rc = unsafe { hew_actor_free(deferred.0) };
    if rc != 0 {
        eprintln!("hew: warning: deferred actor free failed with rc={rc}");
    }
}

#[cfg(not(target_arch = "wasm32"))]
fn defer_actor_free_on_background_thread(actor: *mut HewActor) -> c_int {
    let deferred = DeferredActorFree(actor);
    let Ok(handle) = std::thread::Builder::new()
        .name("deferred-actor-free".into())
        .spawn(move || free_deferred_actor(deferred))
    else {
        crate::set_last_error("hew_actor_free: failed to spawn deferred free thread");
        return -1;
    };
    live_actors::push_deferred_teardown_thread(handle);
    0
}

/// Quiesce actor-owned wake producers before retiring a quiescent actor.
///
/// Cancels periodic timers and (on native targets) detaches reactor and
/// named-node bindings. Used by all four teardown paths
/// (`hew_actor_free`, `drain_actors`, `cleanup_all_actors`, and the WASM
/// `actor_free_wasm_impl`) so that the ordering invariant is identical
/// regardless of how an actor is being torn down.
///
/// On `wasm32` the link/monitor/named-node modules are not compiled in,
/// so this collapses to a timer cancellation. The native and WASM call
/// sites share the same surface, which keeps callers honest about
/// ordering when the WASM build eventually grows the missing primitives.
///
/// # Safety
///
/// `actor` must be valid and quiescent. Callers that run while the scheduler
/// is still live must invoke this *before* untracking the actor from
/// `LIVE_ACTORS` so an in-flight reactor delivery can be drained safely.
/// Callers that run after the runtime has been shut down (such as
/// `cleanup_all_actors`) may call this whether or not the actor is still
/// tracked, because no concurrent dispatch is possible.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) unsafe fn prepare_quiescent_actor_for_cleanup(actor: *mut HewActor) {
    #[cfg(not(target_arch = "wasm32"))]
    {
        // SAFETY: caller guarantees `actor` is valid and quiescent.
        let actor_id = unsafe { (*actor).id };
        crate::timer_periodic::cancel_all_timers_for_actor(actor);
        // Unregister any connection fds owned by this actor BEFORE it is
        // untracked/freed, so a readiness event arriving after the actor stops
        // is dropped (the dead-actor-while-registered race) rather than
        // delivered to a freed actor. Keyed by the INCARNATION the reactor
        // recorded at registration, so the scrub and the Dekker handshake both
        // name this actor and not whatever later spawn inherits its address.
        // SAFETY: the caller guarantees `actor` is valid here.
        crate::reactor::reactor_detach_actor(unsafe {
            crate::lifetime::live_actors::ActorIncarnation::of(actor)
        });
        // SAFETY: caller guarantees `actor` is valid; `unregister_actor_names`
        // does not require LIVE_ACTORS membership, only the actor id.
        unsafe { crate::hew_node::unregister_actor_names(actor_id) };
        // Remove all parse-error slots for this actor across every parser kind.
        // Prevents unbounded growth of the global map on long-running nodes that
        // spawn and reap many actors.
        crate::parse_error_slot::clear_all_for_actor(actor_id);
    }
}

/// Remove semantic relationships after actor retirement and pin drain.
///
/// Stable-handle link/monitor operations pin every participating actor before
/// mutating these registries. Removing the actor from `LIVE_ACTORS` prevents
/// new operations from taking a pin; waiting for the existing pins to drain
/// establishes the single retirement linearization. This final scrub must run
/// only after that wait, otherwise an already-pinned operation can resume after
/// the scrub and reinsert a retired `ActorId`.
///
/// # Safety
///
/// `actor` must remain allocated, must no longer be tracked in `LIVE_ACTORS`,
/// and its `send_pin_count` must be zero.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) unsafe fn scrub_actor_relationships_after_pin_drain(actor: *mut HewActor) {
    #[cfg(not(target_arch = "wasm32"))]
    {
        // SAFETY: the caller guarantees the allocation remains valid.
        let actor_id = unsafe { (*actor).id };
        crate::link::remove_all_links_for_actor(actor_id, actor);
        crate::monitor::remove_all_monitors_for_actor(actor_id, actor);
    }
}

/// Release the continuation frame of an actor that is being abandoned mid-suspend,
/// discharge the reply that activation still owed, and latch the actor out of the
/// non-quiescent `Suspended` window.
///
/// C1 abandonment teardown (D-C1, R326/R327). A `Suspended` actor (`cont_tag`
/// `Parked`) holds a live continuation frame in `suspended_cont`. That frame is
/// a reference to the actor that outlives every ordinary teardown decision:
/// `Suspended` is deliberately NOT quiescent
/// (`actor_free_state_is_quiescent` excludes it), so a teardown path that only
/// knows how to finalize quiescent actors cannot touch it and must leak both
/// the frame and the actor box fail-closed.
///
/// Destroying the frame is what makes the actor reachable by that decision
/// again: `destroy_parked` wins the single `… → Destroyed` CAS (FG1), runs the
/// `coro.destroy` cleanup outline, and nulls the slot (FG4); the CAS serialises
/// against any concurrent resume waking the actor at the same instant (FG2).
/// Only the winner of that CAS reaches the `Suspended → Stopped` latch, so the
/// state transition cannot race a resume — a resume would have refused the
/// destroy.
///
/// That same CAS decides the OTHER debt a parked activation carries. If the
/// handler was serving an `ask`, its suspend edge moved the caller's
/// reply-sender reference into `suspended_reply_channel`; destroying the frame
/// means no resume will ever deposit a reply through it, and an asking thread is
/// blocked in `hew_reply_wait` on a reply that can no longer arrive. Winning the
/// CAS is what makes this teardown the owner of the abandoned activation, so it
/// is also what makes that unanswered reply this teardown's to retire — which is
/// why the retire sits in the same won-the-CAS branch as the destroy and not at
/// the call sites. A resume that won the CAS instead still owns, and answers,
/// its own reply.
///
/// Every teardown route that abandons an actor must call this, and for the same
/// reason. `hew_actor_free_inner` calls it before its bounded quiescence wait,
/// which would otherwise spin to the 2 s deadline and return `-2`.
/// [`retire_parked_activations`] calls it for every still-parked actor at the
/// head of runtime cleanup, so the shutdown sweep meets those actors quiescent
/// instead of leaking them fail-closed.
///
/// ORDERING, and it is load-bearing: the `coro.destroy` cleanup outline this
/// runs re-enters the runtime. A frame parked on `sleep` cancels its await
/// registration, which cancels through the global periodic timer wheel. So this
/// may only run while that machinery is still alive — before
/// `hew_periodic_shutdown` frees the wheel and before `reactor_shutdown` joins
/// the reactor. Running it later dereferences a freed wheel and crashes. It
/// must equally run after the worker threads are joined, so no resume can be
/// attempted concurrently. That leaves exactly one window during shutdown, and
/// [`retire_parked_activations`] is called in it. The teardown routes that run
/// OUTSIDE that window — `cleanup_all_actors` and
/// `free_actor_resources`, both of which run after
/// `hew_periodic_shutdown` — must therefore never call this. They sweep the
/// reply slot directly instead, which is a bare atomic swap and re-enters
/// nothing.
///
/// A no-op for the overwhelmingly common actor that never suspended.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn abandon_parked_activation(a: &HewActor) {
    if !a.checked_invocation.load(Ordering::Acquire).is_null() {
        // A checked turn owns scoped work that may still be cleaning up. Its
        // scheduler activation must cancel and drain before reclaiming it.
        // SAFETY: the caller keeps this actor live throughout teardown.
        unsafe { hew_actor_stop(std::ptr::from_ref(a).cast_mut()) };
        return;
    }
    if !crate::coro_exec::has_live_parked_cont(a) {
        return;
    }
    // SAFETY: `a` is the actor being torn down; `destroy_parked`'s CAS guards
    // serialise against any concurrent resume (FG1/FG2).
    let destroyed = unsafe { crate::coro_exec::destroy_parked(a) };
    if !destroyed.is_ok() {
        // A concurrent resume holds the handle, or the frame was already
        // reclaimed. Leave the state alone: latching a resuming actor to
        // `Stopped` would strand its activation.
        return;
    }
    clear_suspended_cancel_token(a);
    // The parked handler may have been serving an `ask`. Its suspend edge MOVED
    // the caller's reply-sender reference into `suspended_reply_channel`, and
    // destroying the frame above means nothing will ever resume to answer it, so
    // the asking thread is blocked in `hew_reply_wait` on a reply that can no
    // longer arrive. Retiring it belongs HERE, in the won-the-CAS branch,
    // because winning the `… → Destroyed` CAS is exactly what makes this
    // teardown the owner of the abandoned activation — and therefore the owner
    // of the reply it still owes. A concurrent resume that won the CAS instead
    // took the early return above and answers its own reply; this branch cannot
    // be reached in that case, so the obligation is never discharged twice.
    // Placing it inside the function rather than at each caller also releases
    // the asker at the first instant abandonment commits — not after
    // `hew_actor_free_inner`'s two-second quiescence wait — and makes it a
    // property of abandonment rather than something every route has to
    // remember. The swap inside `retire_suspended_reply_channel` keeps it
    // exactly once even though `free_actor_resources` sweeps the
    // same slot on the way out.
    crate::activation::retire_suspended_reply_channel(a);
    // `destroy_parked` above just ran the pump frame's `coro.destroy` cleanup
    // outline, which releases the generator companion (heap env + coro handle)
    // living as a local INSIDE that frame via its normal scope-exit drop
    // (`hew_gen_coro_destroy`) — exactly once, and NOT touched here. This call's
    // sole job is the separate SINK: if this parked activation was a
    // `receive gen fn` pump, fault-close its still-registered sink so the
    // consumer awaiting the stream observes the fault instead of hanging on a
    // stream whose producer will never resume. A no-op if nothing is registered
    // (this was not a gen-stream pump).
    //
    // This one IS destroy-gated, and is only a backstop: the fault is owed
    // whether or not this teardown won the destroy, so the routes that abandon a
    // producer publish it unconditionally themselves —
    // `hew_actor_free_inner` before this call, and
    // `free_actor_resources` on the way out of every free,
    // including the one `retire_parked_activations` hands to
    // `cleanup_all_actors`. The publish is a single atomic swap, so the
    // overlapping calls settle to exactly one release.
    fault_close_registered_gen_sink(a);
    let _ = a.actor_state.compare_exchange(
        HewActorState::Suspended as i32,
        HewActorState::Stopped as i32,
        Ordering::AcqRel,
        Ordering::Acquire,
    );
}

/// Abandon every activation still parked at a suspend point, before the rest of
/// runtime cleanup runs.
///
/// Called once from `hew_runtime_cleanup`, at the head, and only from there.
/// Shutdown IS abandonment: no worker survives to resume a parked activation,
/// so every live continuation frame at this point is a reference that would
/// otherwise outlive teardown and pin its actor in the non-quiescent
/// `Suspended` state — where `cleanup_all_actors` can only leak both, fail
/// closed. Releasing the frames here hands that sweep ordinary quiescent actors
/// it can reclaim.
///
/// This is deliberately NOT folded into `cleanup_all_actors`, and the position
/// is the whole point: see the ORDERING note on [`abandon_parked_activation`].
/// By the time the sweep runs, `hew_periodic_shutdown` has freed the global
/// timer wheel, and a frame parked on `sleep` cancels through that wheel as it
/// unwinds. Iterating without draining also matters — the actors stay tracked so
/// `cleanup_all_actors` still owns reclamation, and this pass owns only the
/// frames.
///
/// # Safety
///
/// All worker threads must be joined (the documented precondition of
/// `hew_runtime_cleanup`), so no activation can be resumed concurrently.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) unsafe fn retire_parked_activations() {
    for actor in crate::lifetime::live_actors::snapshot_live_actor_ptrs() {
        if actor.is_null() {
            continue;
        }
        // SAFETY: the pointer came from the live-actor registry and workers are
        // joined, so nothing can free it underneath this call.
        abandon_parked_activation(unsafe { &*actor });
    }
}

/// Outcome of [`decide_finalize_by_latch`] — the canonical "is it safe to
/// finalize this quiescent-but-possibly-re-enqueued actor?" decision.
#[cfg_attr(target_arch = "wasm32", allow(dead_code))]
pub(crate) enum FinalizeDecision {
    /// Safe to finalize. Carries the state to hand to
    /// [`finalize_quiescent_actor_cleanup`]; that value drives only the
    /// terminate-vs-skip choice (`Crashed` ⇒ skip terminate, every other value
    /// ⇒ run terminate). It is NOT a stale liveness gate.
    Finalize(i32),
    /// The actor is neither `Idle` nor a quiescent terminal state. It is
    /// `Runnable`/`Running` (re-enqueued or actively dispatching), `Suspended`
    /// (a live continuation frame is parked against it), or another non-quiescent
    /// state. A scheduler queue may hold its raw pointer, or a parked frame may
    /// still own it, so freeing it would be a use-after-free or a frame leak. The
    /// caller MUST skip/leak fail-closed.
    Skip,
}

/// Decide whether a quiescent-but-possibly-re-enqueued actor is safe to
/// finalize, using the CAS RESULT (the actual state at decision time) rather
/// than any pre-loaded snapshot.
///
/// This is the single robust primitive shared by every bulk/terminal free path
/// (`cleanup_all_actors`, `drain_quiesced_actor`, `actor_free_wasm_impl`).
/// `hew_actor_free_inner` implements the same CAS-result discipline inline, but
/// with retry-instead-of-skip semantics (a single explicit free can afford to
/// wait the queued activation out and try again, whereas a bulk sweep leaks the
/// straggler fail-closed).
///
/// Why a snapshot is unsafe: between loading `actor_state` and acting on it, a
/// pinned by-ID sender can win `CAS Idle→Runnable` (+ `sched_enqueue`) and
/// re-enqueue the actor. A decision that branches on the stale snapshot can then
/// (a) believe the actor is still `Idle` and free it after a lost latch, or
/// (b) observe the snapshot already `Runnable`,
/// short-circuit the latch entirely, and finalize the queued actor. Both are
/// use-after-frees. Latching and branching on the CAS result closes both:
///
/// - `Ok(_)`                       ⇒ we latched it out of `Idle` into the
///   terminal `Stopped` state; every waker's `CAS Idle→Runnable` now fails, so
///   nothing can enqueue it. Finalize and run terminate (`Finalize(Idle)` — the
///   pre-latch identity, so finalize treats it as a clean stop, not a crash).
/// - `Err(s)` with `actor_free_state_is_quiescent(s)` ⇒ already terminal and
///   wake-proof (necessarily `Stopped`/`Crashed`, since the CAS proved `s` was
///   not `Idle`). Finalize under the observed terminal state (preserves the
///   `Crashed ⇒ skip-terminate` path). No CAS needed.
/// - `Err(s)` otherwise (`Runnable`/`Running`/`Suspended`/…) ⇒ re-enqueued,
///   actively dispatching, or holding a parked continuation frame; a scheduler
///   queue may hold its raw pointer, or a parked frame still owns it. `Skip` —
///   leak fail-closed; never free a queued/active/parked actor. Gating on the
///   shared `actor_free_state_is_quiescent` predicate (rather than an ad-hoc
///   `Stopped || Crashed` test) keeps this decision consistent with the sibling
///   free paths and routes `Suspended` to the same fail-closed leak — closing a
///   latent finalize-over-a-parked-frame on the cleanup path.
pub(crate) fn decide_finalize_by_latch(a: &HewActor) -> FinalizeDecision {
    if !a.checked_invocation.load(Ordering::Acquire).is_null()
        || a.native_completion
            .as_ref()
            .is_some_and(|c| c.cleanup_in_progress())
    {
        return FinalizeDecision::Skip;
    }
    match a.actor_state.compare_exchange(
        HewActorState::Idle as i32,
        HewActorState::Stopped as i32,
        Ordering::AcqRel,
        Ordering::Acquire,
    ) {
        Ok(_) => FinalizeDecision::Finalize(HewActorState::Idle as i32),
        // CAS failed: the actor was not `Idle`. Decide from the ACTUAL observed
        // state `s` using the SAME `actor_free_state_is_quiescent` predicate the
        // sibling free paths gate on (`hew_actor_free_inner`'s quiescence wait,
        // `drain_actors`). A quiescent `s` (here necessarily `Stopped`/`Crashed`,
        // since the CAS proved it was not `Idle`) is already terminal and
        // wake-proof ⇒ finalize under it. Any non-quiescent `s` — `Runnable`/
        // `Running` (re-enqueued or actively dispatching), or `Suspended` (a live
        // continuation frame is parked) — must NOT be finalized: a scheduler
        // queue may hold its raw pointer, or a parked frame still owns it. Leak
        // fail-closed.
        Err(s) if actor_free_state_is_quiescent(s) => FinalizeDecision::Finalize(s),
        Err(_) => FinalizeDecision::Skip,
    }
}

/// Finish the `hew_actor_free` cleanup path after the actor has been untracked.
///
/// # Safety
///
/// `actor` must be valid, quiescent, and no longer tracked in `LIVE_ACTORS`.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) unsafe fn finalize_quiescent_actor_cleanup(actor: *mut HewActor, state: i32) {
    if state != HewActorState::Crashed as i32 {
        // SAFETY: caller guarantees the actor is quiescent and not dispatching.
        unsafe { call_terminate_fn(actor) };
    }

    // SAFETY: caller guarantees the actor remains valid and is no longer dispatching.
    unsafe { free_actor_resources(actor) };
}

/// Free all remaining tracked actors. Called during scheduler shutdown
/// after all worker threads have been joined.
///
/// # Safety
///
/// Must only be called after all worker threads have stopped (native)
/// or when no dispatch is in progress (WASM).
#[cfg(not(target_arch = "wasm32"))]
pub(crate) unsafe fn cleanup_all_actors() {
    // Join every in-flight background teardown (deferred actor frees AND
    // deferred supervisor stops) before sweeping the registry. A deferred
    // supervisor-stop thread dereferences its supervisor's self actor and
    // child actors while it waits for quiescence; freeing those allocations
    // out from under it would be a use-after-free followed by a double-free.
    #[cfg(not(target_arch = "wasm32"))]
    live_actors::drain_deferred_teardown_threads();

    // Close the publication gate and wait for every spawn that reserved a
    // route to either publish a fully initialised actor or roll back. No actor
    // in LIVE_ACTORS can therefore carry an invalid or uncommitted token.
    crate::lifetime::local_handles::begin_current_shutdown();
    let actors = live_actors::drain_all_for_cleanup();
    // After drain_all_for_cleanup LIVE_ACTORS is empty: any subsequent
    // `with_actor_send_by_id` for these actors returns None (map lookup
    // fails), so no new send pins can be taken.  Drain any in-flight pins
    // before finalizing each actor.  LIVE_ACTORS is not held here, so
    // pinned senders can re-acquire it freely (e.g. enqueue_resume).

    let mut skipped_free_for_selftest = false;

    for live_actors::ActorPtr(actor) in actors.into_values() {
        if actor.is_null() {
            continue;
        }
        // Counterfactual for the actor-box balance oracle: with
        // `HEW_ACTOR_LEAK_SELFTEST=skip-free` armed, omit the free of exactly
        // one actor this sweep would otherwise reclaim. The same program must
        // then exit `HEW_EXIT_ACTOR_LEAK`; if it still exits cleanly, the
        // accounting in `actor_balance` has stopped proving anything and the
        // corpus gate that relies on it fails. Inert unless
        // `HEW_ACTOR_LEAK_CHECK=1` is also set — see `actor_balance`.
        if !skipped_free_for_selftest && crate::actor_balance::leak_selftest_skips_free() {
            skipped_free_for_selftest = true;
            continue;
        }
        // SAFETY: actor is valid (from LIVE_ACTORS); scheduler is shut down.
        let a = unsafe { &*actor };

        // Quiesce timers and other actor-owned wake producers before the
        // wake-proof decision. Relationship registries are scrubbed only after
        // the already-completed global retirement and pin drain below.
        // SAFETY: actor is quiescent (scheduler is shut down) and the helper
        // tolerates already-untracked actors when no concurrent dispatch is
        // possible.
        unsafe { prepare_quiescent_actor_for_cleanup(actor) };

        // Remove any pending WASM sleep timer entry for this actor before
        // freeing it. This prevents a use-after-free if a timer tick
        // is called after cleanup but before the timer fires naturally.
        // SAFETY: scheduler is shut down; no concurrent timer-wheel access.

        // Test-only rendezvous: fires after prepare, before the finalize
        // decision. A test uses this to simulate a concurrent by-ID send
        // CAS-ing Idle→Runnable in the wake-proofing window, verifying the
        // skip fires.
        #[cfg(all(test, not(target_arch = "wasm32")))]
        run_cleanup_post_prepare_hook(actor);

        // Wake-proof + finalize decision, by the CAS RESULT — never a stale
        // snapshot (see `decide_finalize_by_latch`).
        //
        // A pinned by-ID sender (that incremented `send_pin_count` before
        // `drain_all_for_cleanup` removed the map entry) can still be running
        // its send closure, which may CAS `Idle→Runnable` (+ `sched_enqueue`)
        // to re-enqueue the actor. The latch `Idle→Stopped` wake-proofs a
        // still-Idle actor (every waker's CAS then fails). If a waker already
        // won — whether the wake landed BEFORE this sweep reached the actor
        // (the snapshot-already-Runnable window) or AFTER, in the latch
        // window — the CAS returns
        // `Err(Runnable/Running/…)` and we leak fail-closed rather than
        // finalize a queued actor (a dangling scheduler pointer → UAF). The
        // decision uses the actual state at CAS time, so neither window can
        // finalize a re-enqueued actor.
        //
        // A still-`Suspended` actor normally does NOT reach this decision on a
        // canonical shutdown path. Native `retire_parked_activations` runs at
        // the head of `hew_runtime_cleanup`; WASM
        // `retire_parked_activations_wasm` runs after the cooperative run queue
        // is empty and before timer teardown in `hew_sched_shutdown`. Both have
        // already destroyed the parked frame and latched the actor to `Stopped`,
        // so it arrives here quiescent and is reclaimed like any other. What
        // still reaches the fail-closed branch is an actor whose frame could
        // not be released, or a sweep reached by some other route than the
        // target's canonical shutdown chain. Leaking those remains correct:
        // the frame that survived still owns the actor.
        let finalize_state = match decide_finalize_by_latch(a) {
            FinalizeDecision::Finalize(state) => Some(state),
            FinalizeDecision::Skip => {
                eprintln!(
                    "hew: runtime error: actor {:#x} was non-quiescent at \
                     shutdown cleanup (re-enqueued/active after a concurrent \
                     send beat the wake-proof latch, or parked at a suspend \
                     point); actor leaked to avoid UAF",
                    a.id
                );
                // Leaking the box and the parked frame is the right fail-closed
                // answer to a possible UAF -- but it is an answer about MEMORY,
                // and a parked `ask` handler owes something else: a reply. Its
                // suspend edge moved the asking thread's reply-sender reference
                // into `suspended_reply_channel`, and this branch is the
                // runtime deciding never to resume that activation. Leaking the
                // reference too would leave a foreign thread blocked in
                // `hew_reply_wait` for the rest of the process's life, which is
                // strictly worse than the leak we accepted. Retiring is safe
                // here in a way finalizing is not: it only swaps the slot and
                // publishes the orphan failure, touching neither the frame nor
                // the box the UAF concern is about.
                retire_parked_activation_reply(a);
                None
            }
        };

        // Drain send pins.  After drain_all_for_cleanup no new pins can be
        // taken; after the Idle→Stopped latch (a `Finalize` decision means the
        // actor is now terminal), any in-flight pin holder whose CAS
        // Idle→Runnable is rejected cannot re-enqueue.  We still wait for
        // existing pin holders to finish their send closures and drop the pin
        // before finalizing.
        // SAFETY: LIVE_ACTORS is not held here; no deadlock risk.
        {
            let pin_deadline = std::time::Instant::now() + std::time::Duration::from_secs(10);
            let mut pinned = false;
            loop {
                if a.send_pin_count.load(Ordering::Acquire) == 0
                    && !a.dispatch_active.load(Ordering::Acquire)
                {
                    break;
                }
                if std::time::Instant::now() >= pin_deadline {
                    eprintln!(
                        "hew: runtime error: actor {:#x} lifetime pins or dispatch \
                         ownership did not drain during shutdown cleanup; actor \
                         leaked to avoid UAF",
                        a.id
                    );
                    pinned = true;
                    break;
                }
                #[cfg(not(target_arch = "wasm32"))]
                std::thread::yield_now();
            }
            if pinned {
                // Do not scrub while a pin is outstanding: it may be a
                // relationship operation that has not inserted yet. The
                // allocation and any semantic entries remain leaked together
                // rather than claiming a false post-retirement cleanup.
                continue;
            }
        }

        // ActorId retirement is now visible and every operation that pinned
        // before it has completed. This is the final semantic-registry scrub:
        // no stable-handle link/monitor operation can reinsert this identity.
        // SAFETY: drain_all_for_cleanup retired the actor and the loop above
        // proved that every ActorPin has dropped.
        unsafe { scrub_actor_relationships_after_pin_drain(actor) };

        let Some(finalize_state) = finalize_state else {
            continue;
        };

        // Run terminate for actors that never reached a terminal state (still
        // IDLE at process exit; `Finalize(Idle)`). Skip crashed actors — their
        // state may be corrupted. `finalize_quiescent_actor_cleanup` performs
        // the terminate-or-skip dance plus the resource free.
        // SAFETY: actor is quiescent, no longer tracked, wake-proofed (latched
        // out of Idle, or already terminal), and all send pins have drained.
        unsafe { finalize_quiescent_actor_cleanup(actor, finalize_state) };
    }
    crate::lifetime::local_handles::assert_current_actor_routes_empty();
}

/// Free an actor's resources without untracking.
///
/// Typed state teardown is decided only by the incarnation's explicit
/// provenance and one-shot `state_drop_consumed` authority. Supervisor restart
/// context is not evidence that state was consumed: init thunks and clone
/// callbacks produce fresh independently-owned state, ordinary stops never open
/// crash escrow, and a crash can occur before escrow takes ownership. Every
/// free route therefore converges here and atomically consumes whichever owned
/// authority remains; shallow-template borrowers never acquire one.
///
/// # Safety
///
/// `actor` must be a valid pointer to a live `HewActor` that is not
/// currently being dispatched.
pub(crate) unsafe fn free_actor_resources(actor: *mut HewActor) {
    #[cfg(feature = "profiler")]
    // SAFETY: `actor` is valid.
    unsafe {
        crate::profiler::actor_registry::unregister(actor);
    };

    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };

    if !a.checked_invocation.load(Ordering::Acquire).is_null() {
        // Terminal state cannot revoke a live checked frame's ownership of
        // state and child work. Retain it if terminal teardown raced its drain.
        crate::set_last_error("checked actor cleanup has not completed");
        return;
    }

    // Every route into this function is a route that abandons the actor: the
    // box is about to go away. If it was parked mid-`ask`, its suspend edge
    // moved the caller's reply-sender reference into `suspended_reply_channel`
    // and no resume will ever consume it, so the asking thread is parked in
    // `hew_reply_wait` on a reply that cannot arrive. Retire it FIRST -- ahead
    // of the five-second terminate wait and its quarantine early-return below,
    // both of which would otherwise hold the asker for the duration or forever.
    // This is the sweep that covers the free routes which do not come through
    // `hew_actor_free_inner` (`cleanup_all_actors`, `drain_quiesced_actor`,
    // supervisor child teardown); the swap makes it exactly once when they
    // overlap.
    crate::activation::retire_suspended_reply_channel(a);

    // Same argument, other debt: if this actor was running a `receive gen fn`
    // pump, its registered sink is the consumer's only source of values and no
    // activation will ever run again to produce one. A consumer parked in
    // `ChannelCore::blocking_recv` is woken only by a send, a close or a fault,
    // so freeing the box without publishing the fault parks it forever. Publish
    // it here, at the same choke point and for the same reason, so "the box is
    // never freed with a live registered gen sink" is a property of this
    // function rather than something each free route has to remember. The swap
    // inside `fault_close_registered_gen_sink` makes it exactly once when it
    // overlaps the stop, crash or `hew_actor_free_inner` publish.
    fault_close_registered_gen_sink(a);

    // Wait for any in-progress terminate callback to complete. This
    // prevents freeing state while another thread is running terminate.
    // Bounded to 5 seconds to avoid hanging forever if terminate blocks.
    let terminate_deadline = std::time::Instant::now() + TERMINATE_WAIT_TIMEOUT;
    let mut terminate_timed_out = false;
    while a.terminate_called.load(Ordering::Acquire)
        && !a.terminate_finished.load(Ordering::Acquire)
    {
        let now = std::time::Instant::now();
        if now >= terminate_deadline {
            eprintln!(
                "hew: warning: actor {} terminate callback did not finish within 5s, quarantining actor",
                a.id
            );
            terminate_timed_out = true;
            break;
        }
        record_terminate_wait_poll_tick();
        std::thread::sleep(TERMINATE_WAIT_POLL_INTERVAL.min(terminate_deadline - now));
    }

    // If the terminate callback is still running, the state pointer is in
    // use on another thread. Quarantine the actor (intentional leak) to
    // avoid use-after-free. The memory cost is bounded because this only
    // happens for actors whose terminate hangs.
    if terminate_timed_out {
        return;
    }

    // Run codegen-generated state-drop on the live state so types
    // implementing `impl Drop` (Vec, String, HashMap, IO handles) release
    // their resources before the underlying allocation goes away.
    //
    // Lifecycle state is deliberately not used as drop authority. An actor can
    // be marked Crashed before dispatch begins (or externally while idle), in
    // which case no escrow ever touched its initialized state. Conversely, an
    // in-dispatch recovery sets `state_drop_consumed` only after taking the
    // typed escrow. The atomic bit is the exact once-only authority.
    //
    // SAFETY rationale for NOT calling `state_drop_fn` on `a.init_state`:
    //
    // `deep_copy_state` (see line 967) is `ptr::copy_nonoverlapping` — a
    // byte memcpy, not a semantic clone. At spawn time the runtime takes
    // one wrapper buffer (already containing field-level deep copies made
    // by codegen) and byte-copies it into two slots:
    // `a.state` and `a.init_state`. Both wrappers therefore contain the
    // same field pointers (Vec.ptr, String.ptr, IO handle ptrs) for every
    // owned field of the actor's state struct.
    //
    // Consequences:
    // 1. `state_drop_fn(a.state)` already releases each owned field via
    //    its `impl Drop`. Calling `state_drop_fn(a.init_state)` afterward
    //    would walk the same field pointers a second time and double-free.
    //    The trailing `crate::mem::buf_free(a.init_state)` releases only the wrapper
    //    bytes; it does not dereference the embedded pointers.
    // 2. User code that overwrites a state field (`self.x = newHeap`) goes
    //    through drop-on-assign on `a.state`, which frees the original
    //    heap. The corresponding pointer inside `a.init_state` becomes
    //    dangling, but is never dereferenced — only `buf_free` runs over
    //    the wrapper bytes.
    // 3. Supervisor restart never reads `a.init_state`. Each restart
    //    allocates a fresh state buffer from `InternalChildSpec.init_state`,
    //    which `hew_supervisor_add_child_spec` (supervisor.rs:1379) created
    //    by an independent sized-block allocation + `ptr::copy_nonoverlapping`
    //    from the caller's spec bytes at registration time.
    // A pipe sink among the state fields learns here whether its owner
    // crashed, so the consumer observes a fault rather than a clean EOF.
    let crashed = a.actor_state.load(Ordering::Acquire) == HewActorState::Crashed as i32;
    crate::fault::release_actor_state(a.id, crashed, || {
        // C1 abandonment teardown (D-C1, R326/R327): a never-woken `Suspended`
        // actor freed at shutdown still owns a live coroutine frame in
        // `suspended_cont` (a `scope` whose child awaits, or an actor awaiting
        // a reply that never arrives before shutdown). Destroy it exactly once
        // BEFORE reclaiming the box, or the frame + any frame-owned heap values
        // leak. `destroy_parked` wins the single `… → Destroyed` CAS (FG1), runs
        // the `cleanup` outline (coro.free → hew_cont_frame_free), and nulls the
        // slot in the same critical section (FG4); it refuses if a resume is in
        // flight or it was already destroyed, so this is the safe
        // single-teardown owner on the free path. It runs INSIDE the disclosure
        // window because the frame's owners are the crashed actor's too: a sink
        // in an abandoned frame released outside it would publish a clean EOF
        // and the consumer would read `None` where the crash belongs. NOTE: the
        // single-task cancellation FLOW (unregister-readiness +
        // resume-with-cancel) is a separate concern; this is only the
        // single-destroy plumbing.
        if crate::coro_exec::has_live_parked_cont(a) {
            // SAFETY: `a` is the actor being freed; the caller guarantees
            // exclusive access (no concurrent dispatch), so no resume can
            // race this teardown.
            let _ = unsafe { crate::coro_exec::destroy_parked(a) };
        }
        // SAFETY: terminal teardown owns every remaining state field.
        unsafe { drop_initialized_actor_state(a) };
    });
    // SAFETY: all native state fields are released before publishing completion.
    unsafe { crate::actor_native::finish_native_terminal(a) };

    // SAFETY: state came from deep_copy_state's sized-block allocation.
    unsafe {
        crate::mem::buf_free(a.state);
        crate::mem::buf_free(a.init_state);
    }

    if !a.arena.is_null() {
        let arena_ptr = a.arena;
        // Null the slot BEFORE freeing — defense-in-depth per LESSONS row
        // `raii-null-after-move`.  Any straggler reader that holds only
        // `actor` (not a cached copy) now fails closed at the C-ABI
        // entry-guard null check (`hew_arena_reset` / `hew_arena_free_all`
        // are both null-tolerant) instead of dereferencing freed memory.
        // The cached-`actor_arena` reader in `scheduler.rs::activate_actor`
        // is protected by the `Crashing → Crashed` two-step instead; this
        // null-out covers other helpers that re-read `a.arena`.
        // SAFETY: caller guarantees exclusive access to `actor` during free.
        unsafe { (*actor).arena = std::ptr::null_mut() };
        // SAFETY: Arena was created by hew_arena_new during spawn.
        unsafe { crate::arena::hew_arena_free_all(arena_ptr) };
    }

    unregister_actor_state_lock(actor);

    let mb = a.mailbox.cast::<HewMailbox>();
    if !mb.is_null() {
        // Observation point for the teardown-ordering proof. This is the last
        // instruction before the actor's system queue is destroyed, so a test
        // reading actor_state and live-actor tracking here reads exactly the
        // state that holds AT destruction rather than around it.
        #[cfg(all(test, not(target_arch = "wasm32")))]
        run_pre_queue_destroy_hook(actor);
        // Null the mailbox slot before freeing — same defense-in-depth
        // discipline as the arena slot above (`raii-null-after-move`).
        // SAFETY: caller guarantees exclusive access to `actor` during free.
        unsafe { (*actor).mailbox = std::ptr::null_mut() };
        // SAFETY: Mailbox was allocated by hew_mailbox_new.
        unsafe { mailbox::hew_mailbox_free(mb) };
    }

    // The single site that reclaims an actor box; the balancing half of the
    // `record_actor_box_alloc` in `spawn_actor_internal`.
    crate::actor_balance::record_actor_box_free();
    // SAFETY: Actor was allocated with Box::new / Box::into_raw.
    drop(unsafe { Box::from_raw(actor) });
}

// ── Terminate callback invocation ───────────────────────────────────────

/// Release initialized state once, shared by native terminal completion and free.
///
/// # Safety
/// No active handler or lifecycle callback may borrow the actor's state.
pub(crate) unsafe fn drop_initialized_actor_state(a: &HewActor) {
    let state_drop_consumed = a.state_drop_consumed.swap(true, Ordering::AcqRel);
    if !a.state_drop_borrowed.load(Ordering::Acquire) && !state_drop_consumed {
        if let Some(state_drop_fn) = a.state_drop_fn {
            if !a.state.is_null() {
                // SAFETY: `a.state` is the live state allocation;
                // `state_drop_fn` is a codegen-emitted function that walks
                // owned fields after the completed activation releases its borrows.
                unsafe { state_drop_fn(a.state) };
            }
        }
    }
}

/// Run the actor's terminate callback exactly once, with crash recovery.
///
/// Sets up the actor lane and catches Hew language panics unwinding from the
/// terminate block. Hardware synchronous faults remain process-fatal.
///
/// Called at terminal state transitions (→ Stopped), **not** at free time.
///
/// # Safety
///
/// `actor` must be a valid pointer to a live [`HewActor`] in a terminal
/// state (`Stopped`) that is not currently being dispatched.
pub(crate) unsafe fn call_terminate_fn(actor: *mut HewActor) {
    // SAFETY: caller guarantees `actor` is valid.
    let a = unsafe { &*actor };

    // Guard: only run once across all terminal-transition paths.
    if a.terminate_called.swap(true, Ordering::AcqRel) {
        return;
    }

    let Some(terminate_fn) = a.terminate_fn else {
        a.terminate_finished.store(true, Ordering::Release);
        // SAFETY: no lifecycle callback or handler borrows this terminal state.
        unsafe { crate::actor_native::finish_native_terminal(a) };
        return;
    };

    if a.state.is_null() {
        a.terminate_finished.store(true, Ordering::Release);
        // SAFETY: no lifecycle callback or handler borrows this terminal state.
        unsafe { crate::actor_native::finish_native_terminal(a) };
        return;
    }

    let state = a.state;
    let mut execution_context = crate::execution_context::HewExecutionContext {
        actor,
        actor_id: a.id,
        flags: crate::execution_context::HEW_CTX_FLAG_UNWIND_BOUNDARY_INSTALLED,
        arena: a.arena,
        prev_context: crate::execution_context::current_context(),
        ..crate::execution_context::HewExecutionContext::default()
    };
    let prev_context = execution_context.prev_context;
    let installed_prev = crate::execution_context::set_current_context(&raw mut execution_context);
    debug_assert_eq!(installed_prev, prev_context);

    // Bind this thread to the owning runtime for the terminate body, beside the
    // execution-context install and torn down on the same exit edge below. The
    // terminate callback runs user `on(stop)` code that can touch runtime
    // authorities (spawn/send), so it must resolve the right `RuntimeInner`
    // through TLS rather than relying on the caller's binding. Worker-thread
    // callers are already `enter()`-ed (this re-enters the same default); the
    // install matters for any terminate path reached without a worker `enter()`.
    // Single-runtime: the entered runtime equals the default, so behaviour is
    // preserved. `None` when no runtime is installed (e.g. a bare terminate unit
    // test) — the existing fallback covers that, and the guard drops as a no-op.
    // The guard is held until the matching `set_current_context(prev_context)`
    // restore at the end of this function, so it covers every normal/panic/trap
    // exit edge (lifecycle-symmetry).
    //
    // SAFETY: `rt_default()` borrows the installed default runtime, which is
    // process-lifetime once `install_default` has run (it is detached only by
    // `take_default` at cleanup, after all workers join). It therefore outlives
    // this guard and every `rt_current()` deref taken through it during the
    // terminate body, satisfying `enter`'s lifetime obligation.
    #[cfg(not(target_arch = "wasm32"))]
    let _rt_guard = crate::runtime::rt_default().map(|rt| unsafe { crate::runtime::enter(rt) });

    // The emitted terminate callback acquires the actor-state lock. Structured
    // unwinding runs generated lexical cleanups; this boundary releases the
    // state lock and contains the lifecycle failure.
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        // SAFETY: terminate_fn and state are valid; actor is not dispatched.
        unsafe { terminate_fn(state) };
    }));
    if let Err(panic_payload) = result {
        // SAFETY: actor is valid; the registry tolerates an unheld lock.
        unsafe {
            let _ = hew_actor_state_lock_release_after_panic(actor);
        }
        crate::util::quarantine_panic_payload(panic_payload);
    }

    a.terminate_finished.store(true, Ordering::Release);
    // SAFETY: the lifecycle callback has returned and the native turn is finished.
    unsafe { crate::actor_native::finish_native_terminal(a) };
    let restored_context = crate::execution_context::set_current_context(prev_context);
    debug_assert_eq!(restored_context, &raw mut execution_context);
}

/// Free an actor and all associated resources.
///
/// Spin-waits until the actor reaches a terminal state, then frees state,
/// mailbox, and the actor itself.
///
/// # Safety
///
/// - `actor` must have been returned by a spawn function.
/// - The actor must not be used after this call.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_free(actor: *mut HewActor) -> c_int {
    // SAFETY: caller forwards the same invariants the inner requires.
    unsafe { hew_actor_free_inner(actor) }
}

#[cfg(not(target_arch = "wasm32"))]
unsafe fn hew_actor_free_inner(actor: *mut HewActor) -> c_int {
    if actor.is_null() {
        crate::set_last_error("hew_actor_free: null actor pointer");
        return -1;
    }

    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };

    if hew_actor_self() == actor {
        let state = a.actor_state.load(Ordering::Acquire);
        if state == HewActorState::Stopping as i32 || actor_free_state_is_quiescent(state) {
            return defer_actor_free_on_background_thread(actor);
        }
        crate::set_last_error("hew_actor_free: current actor is still dispatching");
        return -2;
    }

    // Drive the actor to a *wake-proof* terminal state, then free. There are two
    // independent wake sources that can re-enqueue a freshly-Idle actor while we
    // tear it down, and both gate on the wake CAS `Idle->Runnable`:
    //
    //   - The reactor: a delivery that published `DELIVERING_ACTOR` before its
    //     registry scrub can run `hew_actor_try_send` (CAS Idle->Runnable +
    //     sched_enqueue) *during* `reactor_detach_actor`.
    //   - Non-reactor wakers: in-flight link/monitor exit/down propagation
    //     (`send_exit_signal` / `send_down_notification`) and direct
    //     actor-to-actor sends. Each snapshots the target under its own shard
    //     lock, then later reaches `with_live_actor_by_id` and, if the target is
    //     still tracked and `Idle`, does CAS Idle->Runnable + sched_enqueue
    //     *inside* the `LIVE_ACTORS` lock. `remove_all_links_for_actor` /
    //     `remove_all_monitors_for_actor` only scrub the tables; they do not
    //     drain an already-snapshotted propagation, so scrubbing alone cannot
    //     close this window.
    //
    // Detaching/scrubbing closes the reactor's ability to *start* a new wake, but
    // an actor merely observed `Idle` after detach can still be enqueued by a
    // non-reactor waker in the window before `untrack_actor`. Freeing under that
    // leaves a dangling pointer in a worker/stealer queue → use-after-free in
    // `activate_actor`.
    //
    // The fix mirrors why `hew_actor_stop` / `drain_actors` are immune: before
    // untracking, latch the actor OUT of `Idle` into the `Stopped` terminal
    // state. After that CAS succeeds, *every* waker's `CAS Idle->Runnable` fails
    // and `activate_actor` early-returns on `Stopped`, so no wake — reactor or
    // not — can enqueue the actor. Only then is untrack+free safe.
    //
    //   1. Wait (bounded) until `actor_state` is quiescent (Idle/Stopped/Crashed).
    //   2. Quiesce pre-retirement producers. `reactor_detach_actor` waits out an
    //      in-flight reactor delivery; relationship cleanup is deferred until
    //      retirement has prevented new pins and all admitted pins have drained.
    //   3. Re-load `actor_state` after cleanup, then *latch*:
    //        - `Idle`    → `CAS Idle->Stopped`. Success ⇒ wake-proof; free under
    //                      `Stopped`. Loss ⇒ a waker won the race (now
    //                      Runnable/Running and queued); do NOT free, loop back so
    //                      the queued activation drains it to `Idle`.
    //        - `Stopped`/`Crashed` → already wake-proof (no CAS needed; preserves
    //                      the `Crashed` => skip-terminate path in finalize).
    //      A continuously-woken actor returns `-2` at the shared deadline
    //      (fail-closed; the caller leaks rather than frees a queued actor).
    //
    // `state` carried out of the loop is the post-latch terminal state passed to
    // `finalize_quiescent_actor_cleanup`, which runs the terminate
    // callback exactly once for `Stopped` (== the old `Idle` behaviour) and skips
    // it for `Crashed`.
    //
    // C1 abandonment teardown (D-C1, R326/R327): a `Suspended` actor (cont_tag
    // `Parked`) holds a live continuation frame in `suspended_cont`. `Suspended`
    // is non-quiescent (`actor_free_state_is_quiescent` excludes it), so without
    // this the quiescence wait below would spin to the 2 s deadline, return `-2`,
    // and LEAK the frame + the actor box for any actor abandoned mid-suspend
    // (a `scope` whose child awaits, an actor awaiting a never-arriving reply at
    // shutdown). Destroy the parked frame exactly once HERE, before the
    // quiescence wait: `destroy_parked` wins the single `… → Destroyed` CAS
    // (FG1), runs the `cleanup` outline, and nulls the slot (FG4); the CAS
    // serialises against any concurrent resume waking the actor at the same
    // instant (FG2). After the destroy the slot is `Empty`, so the actor can
    // reach a quiescent terminal state through the normal path below.
    //
    // This is the single-DESTROY plumbing only. The single-task cancellation
    // FLOW (unregister-readiness + resume-with-cancellation + the two-phase
    // park lost-wake-vs-cancel race) is NEW-6; this teardown is the minimum that
    // makes the live suspend edge non-leaking.
    //
    // Reaching this function is already terminal for the actor: the box is about
    // to be reclaimed, so a `receive gen fn` pump registered here will never
    // produce another value. Fault-close its sink NOW — before the bounded
    // quiescence wait below and unconditionally, i.e. NOT gated on this path
    // winning the parked-frame destroy. The destroy is a race with every other
    // abandonment path (the out-of-band stop cancel, a concurrent resume settle),
    // and whoever loses it still owes the consumer the fault. Gating the publish
    // on the destroy is what let a stopped producer leave a consumer parked in
    // `ChannelCore::blocking_recv` forever. Idempotent (a single atomic swap), so
    // the destroy-gated publish inside `abandon_parked_activation` below, and an
    // earlier publish on the stop or crash path, both become no-ops.
    fault_close_registered_gen_sink(a);
    abandon_parked_activation(a);

    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(2);
    let state = loop {
        // Step 1: wait until the actor first looks quiescent (bounded).
        loop {
            let state = a.actor_state.load(Ordering::Acquire);
            // A terminal state alone is NOT enough: an external `hew_actor_trap`
            // CAS-es a still-dispatching actor straight to `Crashed`/`Stopped`
            // out from under its owning worker, which is still reading the actor
            // box, arena, and mailbox in its post-dispatch settle. Freeing then
            // is a use-after-free. `dispatch_active` (cleared by the scheduler's
            // `ActivationOwnership` guard when the activation leaves) gates the
            // quiescence decision so we wait the worker out. The `Acquire` load
            // pairs with the guard's `Release` clear so we also observe every
            // write the activation made before we proceed to free.
            //
            // `send_pin_count` is NOT checked here.  Instead we untrack the actor
            // first (so no new pins can be taken) and then drain any in-flight
            // pins after the untrack — see below.  The untrack-first ordering
            // makes the two operations mutually exclusive: either the sender pins
            // before the freer's map removal (freer waits in the drain loop), or
            // the freer removes the map entry before the sender's lookup (sender
            // gets `None`, no pin, no UAF).
            if actor_free_state_is_quiescent(state)
                && !a.dispatch_active.load(Ordering::Acquire)
                && a.checked_invocation.load(Ordering::Acquire).is_null()
            {
                break;
            }
            if std::time::Instant::now() >= deadline {
                crate::set_last_error("actor still running after timeout");
                return -2;
            }
            std::thread::yield_now();
        }

        // Test-only rendezvous: the actor just looked quiescent, but detach has
        // not run yet. A test uses this point to force a reactor delivery to wake
        // + enqueue the actor during the detach window below.
        #[cfg(all(test, not(target_arch = "wasm32")))]
        run_free_pre_detach_hook(actor);

        // Step 2: cancel periodic timers and detach reactor registrations before
        // untracking. `reactor_detach_actor` may wait out an in-flight delivery
        // that re-wakes the actor (see above); this call is idempotent across
        // retries. Link/monitor cleanup must wait until after retirement and pin
        // drain so an admitted stable-handle registration cannot reinsert state.
        // SAFETY: the wait loop proved the actor was quiescent and still tracked.
        unsafe { prepare_quiescent_actor_for_cleanup(actor) };

        // Deterministic proof hook: a stable-handle registration has acquired
        // all pins but has not mutated its relationship table. The test lets it
        // register while this actor is still live and Idle, then teardown must
        // retire, drain, and remove that late entry in the final scrub.
        #[cfg(all(test, not(target_arch = "wasm32")))]
        run_registration_retirement_hook(&FREE_PRE_LATCH_REGISTRATION_HOOK, a.id);

        // Step 3: re-load after cleanup, then latch the actor out of `Idle`.
        //
        // This is the inline, *retry*-on-loss variant of the shared
        // `decide_finalize_by_latch` primitive (used by `cleanup_all_actors`,
        // `drain_quiesced_actor`, and `actor_free_wasm_impl`): all four branch on
        // the CAS RESULT, never on a pre-loaded snapshot. The bulk/terminal paths
        // *skip* (leak fail-closed) when the latch loses to a waker; this explicit
        // single-actor free instead *loops back* to wait the queued activation out
        // and free cleanly, returning `-2` only at the deadline. Same decision
        // table (`Ok⇒Stopped`, `Err(Stopped|Crashed)⇒that state`, else not-safe);
        // different not-safe handling.
        let state = a.actor_state.load(Ordering::Acquire);
        if state == HewActorState::Idle as i32 {
            // Latch Idle->Stopped. This is the wake-proofing step: after it
            // succeeds, every waker (reactor `hew_actor_try_send`, link/monitor
            // `send_exit_signal`/`send_down_notification`, direct send) finds the
            // actor non-Idle and its `CAS Idle->Runnable` fails, so nothing can
            // enqueue the actor between here and `untrack_actor`. We do NOT emit a
            // SPAN_STOP lifecycle event (unlike `hew_actor_stop`): this is a free,
            // not a user-visible stop, and finalize already runs terminate.
            if a.actor_state
                .compare_exchange(
                    HewActorState::Idle as i32,
                    HewActorState::Stopped as i32,
                    Ordering::AcqRel,
                    Ordering::Acquire,
                )
                .is_ok()
            {
                break HewActorState::Stopped as i32;
            }
            // Lost the latch to a concurrent wake: the actor is now
            // Runnable/Running and queued in the scheduler. Do NOT free under it.
            // Loop back; the queued activation drains it to Idle and the next pass
            // latches+frees cleanly.
        } else if state == HewActorState::Stopped as i32 || state == HewActorState::Crashed as i32 {
            // Already wake-proof: a prior stop/close (`Stopped`) or trap
            // (`Crashed`) drove the actor out of `Idle`, so no waker's
            // `CAS Idle->Runnable` can succeed. Free under the observed state
            // (preserves the `Crashed` => skip-terminate path in finalize). No CAS
            // needed.
            break state;
        }

        // Either the post-detach reload was non-quiescent (a wake landed during
        // cleanup) or the Idle->Stopped latch lost to a wake. Do NOT free; loop
        // back so the queued activation drains the actor to Idle, then retry.
        if std::time::Instant::now() >= deadline {
            crate::set_last_error("actor still running after timeout");
            return -2;
        }
        std::thread::yield_now();
    };

    // Test-only rendezvous: the actor is latched out of `Idle` (Stopped/Crashed)
    // but not yet untracked. A test fires a non-reactor wake here to prove the
    // producer-side `CAS Idle->Runnable` now fails (no enqueue) — the window the
    // verdict reproduced as a UAF is closed.
    #[cfg(all(test, not(target_arch = "wasm32")))]
    run_free_post_latch_hook(actor);

    clear_suspended_cancel_token(a);

    // Remove from live tracking. If the actor was already consumed by
    // cleanup_all_actors (returns false), skip freeing to avoid
    // double-free.
    if !live_actors::untrack_actor(actor) {
        crate::set_last_error("hew_actor_free: actor already freed or not tracked");
        return -1;
    }

    // Deterministic watcher-retirement proof hook: the actor is retired, but a
    // previously admitted monitor operation still holds its pin and may insert
    // watcher-owned state before the pin drain and final scrub below.
    #[cfg(all(test, not(target_arch = "wasm32")))]
    run_registration_retirement_hook(&FREE_POST_RETIRE_REGISTRATION_HOOK, a.id);

    // After untrack_actor the map entry is removed: any subsequent
    // `with_actor_send_by_id` for this actor gets `None` from the map
    // lookup, so no new send pins can be taken.  Drain any in-flight pins
    // that were incremented before the untrack (e.g. a concurrent by-ID
    // send that pinned the actor just before the map entry was removed).
    // LIVE_ACTORS is NOT held here, so pinned senders can freely re-acquire
    // it (e.g. via `enqueue_resume` → `with_live_actor`) without deadlock.
    // The `Release` in `SendPinGuard::drop` pairs with this `Acquire` load.
    //
    // Fresh deadline: the quiescence wait above may have consumed most of
    // `deadline`; give the pin drain its own full budget.
    let drain_deadline = std::time::Instant::now() + std::time::Duration::from_secs(2);
    loop {
        if a.send_pin_count.load(Ordering::Acquire) == 0
            && !a.dispatch_active.load(Ordering::Acquire)
        {
            break;
        }
        if std::time::Instant::now() >= drain_deadline {
            // An outstanding pin may still insert a relationship, so no final
            // scrub is safe on this fail-closed allocation leak.
            crate::set_last_error(
                "hew_actor_free: lifetime pins or dispatch ownership did not drain after timeout",
            );
            return -2;
        }
        std::thread::yield_now();
    }

    // Actor retirement prevents new pins; the drain above waited out every
    // stable-handle operation that began before retirement. Scrub only now so
    // no paused registration can reinsert this ActorId afterward.
    // SAFETY: actor is untracked, allocated, and has no remaining pins.
    unsafe { scrub_actor_relationships_after_pin_drain(actor) };

    // SAFETY: actor is quiescent (re-verified after detach), no longer tracked,
    // all send pins drained, and not being dispatched.
    unsafe { finalize_quiescent_actor_cleanup(actor, state) };
    0
}

#[cfg(not(target_arch = "wasm32"))]
fn drain_outcome_from_lists(
    mut still_live: Vec<ActorId>,
    mut crashed: Vec<ActorId>,
) -> DrainOutcome {
    still_live.sort_unstable();
    crashed.sort_unstable();
    if still_live.is_empty() && crashed.is_empty() {
        DrainOutcome::Drained
    } else {
        DrainOutcome::Incomplete {
            still_live,
            crashed,
        }
    }
}

#[cfg(not(target_arch = "wasm32"))]
fn collect_pending_actor(id: ActorId) -> Option<(ActorId, live_actors::ActorPin)> {
    live_actors::pin_actor_by_id(id).map(|pin| (id, pin))
}

#[cfg(not(target_arch = "wasm32"))]
fn pin_pending_actor(actor_id: ActorId, expected: *mut HewActor) -> Option<live_actors::ActorPin> {
    let pin = live_actors::pin_actor_by_id(actor_id)?;
    (pin.as_ptr() == expected).then_some(pin)
}

#[cfg(not(target_arch = "wasm32"))]
fn drain_backoff_duration(delay: std::time::Duration) -> std::time::Duration {
    (delay.saturating_mul(2)).min(std::time::Duration::from_millis(50))
}

/// Quiesce and free an actor that has already reached a terminal state inside
/// `drain_actors`.
///
/// This consolidates the sequence — cancel timers/links/monitors, wake-proof,
/// take ownership from `LIVE_ACTORS`, drain pins, and run
/// `finalize_quiescent_actor_cleanup` — into one call site so both the inner
/// loop and the post-deadline pass use the same ordering.
///
/// ## Finalize decision (CAS-result, never a snapshot)
///
/// The finalize decision comes from [`decide_finalize_by_latch`] — the same
/// CAS-result primitive `cleanup_all_actors` uses — applied BEFORE untracking,
/// not from a state value the caller snapshotted earlier. `drain_actors` calls
/// `hew_actor_stop` on every actor BEFORE waiting for quiescence, so by the time
/// this runs the actor is already terminal (`Stopped`): the latch CAS returns
/// `Err(Stopped)` ⇒ `Finalize(Stopped)`, the same finalize the previous
/// snapshot-based code performed. The latch additionally HARDENS the path
/// against stop-first contract drift: were a future caller to skip the stop, a
/// re-enqueued (`Runnable`) actor now fails closed (`Skip` ⇒ leak) instead of
/// being finalized while a scheduler queue still holds its pointer.
///
/// **Callers should still uphold the stop-first contract** so the common path
/// finalizes cleanly rather than relying on the defensive `Skip` leak.
///
/// # Safety
///
/// `pin` must name the exact actor tracked under `actor_id`, and the caller must
/// have observed it in a quiescent state while holding this pin.
#[cfg(not(target_arch = "wasm32"))]
unsafe fn drain_quiesced_actor(
    actor_id: ActorId,
    pin: live_actors::ActorPin,
    deadline: std::time::Instant,
) {
    let expected = pin.as_ptr();
    // The caller pin bridges the state-observation -> first-dereference gap.
    // A concurrent free may retire the actor now, but its post-untrack pin
    // drain cannot reclaim the allocation until this preparation and the
    // retirement claim below finish.
    //
    // SAFETY: caller guarantees `pin` owns this valid quiescent allocation.
    unsafe { prepare_quiescent_actor_for_cleanup(expected) };

    // Wake-proof + finalize decision by the CAS RESULT, BEFORE untracking
    // (mirrors `hew_actor_free_inner` / `cleanup_all_actors`). Under the
    // stop-first contract this is `Err(Stopped) ⇒ Finalize(Stopped)`; a
    // re-enqueued actor (contract drift) takes the fail-closed `Skip` leak.
    let a = pin.actor();
    let finalize_state = match decide_finalize_by_latch(a) {
        FinalizeDecision::Finalize(state) => state,
        FinalizeDecision::Skip => {
            // Re-enqueued/active despite stop-first: leave it tracked so the
            // shutdown sweep (`cleanup_all_actors`) reclaims it once it drains,
            // and leak fail-closed here rather than free a queued actor.
            crate::set_last_error(
                "drain_quiesced_actor: actor re-enqueued during drain; leaked fail-closed",
            );
            return;
        }
    };

    if let Some(actor) = live_actors::take_actor_by_id(actor_id, expected) {
        // This function now owns the retired allocation. Release its caller pin
        // before waiting for all remaining pins, otherwise it would wait on
        // itself until the deadline. The allocation stays live by cleanup
        // ownership after take_actor_by_id.
        drop(pin);

        // After take_actor_by_id the map entry is removed: no new send pins
        // can be taken.  Drain any in-flight pins before finalizing.
        // LIVE_ACTORS is not held here; pinned senders can re-acquire it.
        // SAFETY: actor is a live pointer returned by take_actor_by_id.
        let a = unsafe { &*actor };
        loop {
            if a.send_pin_count.load(Ordering::Acquire) == 0
                && !a.dispatch_active.load(Ordering::Acquire)
            {
                break;
            }
            if std::time::Instant::now() >= deadline {
                // An outstanding pin may still insert a relationship, so no
                // final scrub is safe on this fail-closed allocation leak.
                crate::set_last_error(
                    "drain_quiesced_actor: lifetime pins or dispatch ownership did not drain \
                     after timeout",
                );
                // Fail-closed: actor is untracked but not freed (leak).
                return;
            }
            std::thread::yield_now();
        }
        // SAFETY: take_actor_by_id retired the actor and the loop above proved
        // every operation pinned before retirement has completed.
        unsafe { scrub_actor_relationships_after_pin_drain(actor) };
        // SAFETY: the actor is quiescent, prepared for cleanup, wake-proofed,
        // no longer tracked, and all send pins have drained.
        unsafe { finalize_quiescent_actor_cleanup(actor, finalize_state) };
    }
    // If another freer won retirement, `pin` drops here and releases that
    // winner's final reclamation wait.
}

/// Cooperatively stop a set of native actors and wait for quiescence with a shared deadline.
#[cfg(not(target_arch = "wasm32"))]
#[must_use]
pub fn drain_actors(ids: &[ActorId], deadline: std::time::Instant) -> DrainOutcome {
    if ids.is_empty() {
        return DrainOutcome::Drained;
    }

    let mut seen = HashSet::with_capacity(ids.len());
    let mut pending = Vec::with_capacity(ids.len());
    for actor_id in ids.iter().copied().filter(|id| seen.insert(*id)) {
        let Some((actor_id, pin)) = collect_pending_actor(actor_id) else {
            continue;
        };
        let actor = pin.as_ptr();
        // Deterministic proof hook: a concurrent free may retire this actor
        // now, but cannot reclaim it while `pin` is held across stop.
        #[cfg(all(test, not(target_arch = "wasm32")))]
        run_registration_retirement_hook(&DRAIN_POST_PIN_PRE_STOP_HOOK, actor_id);

        // SAFETY: `pin` was acquired while actor_id was tracked and keeps the
        // allocation live across this raw-pointer stop operation.
        unsafe { hew_actor_stop(actor) };
        pending.push((actor_id, actor));
        // Release exactly after the last unvalidated raw-pointer dereference.
        // Later state/cleanup work acquires a fresh exact-pointer pin.
        drop(pin);
    }

    let mut crashed = Vec::new();
    let mut backoff = std::time::Duration::from_millis(1);

    loop {
        let mut index = 0;
        while index < pending.len() {
            let (actor_id, expected) = pending[index];
            let Some(pin) = pin_pending_actor(actor_id, expected) else {
                pending.swap_remove(index);
                continue;
            };
            let state = pin.actor().actor_state.load(Ordering::Acquire);
            match state {
                state if state == HewActorState::Crashed as i32 => {
                    drop(pin);
                    crashed.push(actor_id);
                    pending.swap_remove(index);
                }
                state if actor_free_state_is_quiescent(state) => {
                    // Deterministic proof hook: the exact allocation remains
                    // pinned from this state observation into cleanup.
                    #[cfg(all(test, not(target_arch = "wasm32")))]
                    run_registration_retirement_hook(&DRAIN_POST_STATE_PRE_CLEANUP_HOOK, actor_id);

                    // SAFETY: `pin` names expected under actor_id and held the
                    // allocation across the quiescent state observation.
                    unsafe { drain_quiesced_actor(actor_id, pin, deadline) };
                    pending.swap_remove(index);
                }
                _ => {
                    drop(pin);
                    index += 1;
                }
            }
        }

        if pending.is_empty() {
            return drain_outcome_from_lists(Vec::new(), crashed);
        }

        let now = std::time::Instant::now();
        if now >= deadline {
            break;
        }

        let sleep_for = backoff.min(deadline.saturating_duration_since(now));
        if !sleep_for.is_zero() {
            std::thread::sleep(sleep_for);
        }
        backoff = drain_backoff_duration(backoff);
    }

    let mut still_live = Vec::with_capacity(pending.len());
    for (actor_id, expected) in pending {
        let Some(pin) = pin_pending_actor(actor_id, expected) else {
            continue;
        };
        let state = pin.actor().actor_state.load(Ordering::Acquire);
        match state {
            state if state == HewActorState::Crashed as i32 => {
                drop(pin);
                crashed.push(actor_id);
            }
            state if actor_free_state_is_quiescent(state) => {
                // SAFETY: `pin` names expected under actor_id and held the
                // allocation across the quiescent state observation.
                unsafe { drain_quiesced_actor(actor_id, pin, deadline) };
            }
            _ => {
                drop(pin);
                still_live.push(actor_id);
            }
        }
    }

    drain_outcome_from_lists(still_live, crashed)
}

fn actor_ids_to_malloc(ids: &[ActorId]) -> Result<*mut ActorId, &'static str> {
    if ids.is_empty() {
        return Ok(ptr::null_mut());
    }

    let Some(bytes) = ids.len().checked_mul(std::mem::size_of::<ActorId>()) else {
        return Err("hew_actor_drain_set: actor id list size overflow");
    };
    // SAFETY: malloc returns an allocation large enough for `ids.len()` ActorIds or null on failure.
    let out = crate::mem::buf_try_alloc(bytes).cast::<ActorId>();
    if out.is_null() {
        return Err("hew_actor_drain_set: failed to allocate outcome buffer");
    }

    // SAFETY: `out` points to `ids.len()` initialized ActorId slots allocated above.
    unsafe { ptr::copy_nonoverlapping(ids.as_ptr(), out, ids.len()) };
    Ok(out)
}

fn write_drain_outcome_repr(
    out: &mut DrainOutcomeRepr,
    outcome: DrainOutcome,
) -> Result<(), &'static str> {
    *out = DrainOutcomeRepr::default();
    let (still_live, crashed) = match outcome {
        DrainOutcome::Drained => (Vec::new(), Vec::new()),
        DrainOutcome::Incomplete {
            still_live,
            crashed,
        } => (still_live, crashed),
    };

    let still_live_ptr = actor_ids_to_malloc(&still_live)?;
    let crashed_ptr = match actor_ids_to_malloc(&crashed) {
        Ok(ptr) => ptr,
        Err(err) => {
            // SAFETY: `still_live_ptr` came from `actor_ids_to_malloc` in this function.
            unsafe { crate::mem::buf_free(still_live_ptr.cast()) };
            return Err(err);
        }
    };

    out.still_live_ptr = still_live_ptr;
    out.still_live_len = still_live.len();
    out.crashed_ptr = crashed_ptr;
    out.crashed_len = crashed.len();
    Ok(())
}

/// Free buffers allocated by [`hew_actor_drain_set`].
///
/// # Safety
///
/// `out` must point to an initialized [`DrainOutcomeRepr`] from this runtime.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_drain_outcome_free(out: *mut DrainOutcomeRepr) {
    if out.is_null() {
        return;
    }

    // SAFETY: caller guarantees `out` points to a valid DrainOutcomeRepr.
    let out = unsafe { &mut *out };
    // SAFETY: the buffers were allocated by `actor_ids_to_malloc`; null is allowed.
    unsafe {
        crate::mem::buf_free(out.still_live_ptr.cast());
        crate::mem::buf_free(out.crashed_ptr.cast());
    }
    *out = DrainOutcomeRepr::default();
}

/// Drain a set of actors using a caller-supplied timeout in nanoseconds.
///
/// The timeout is measured relative to `Instant::now()` on entry.
///
/// # Safety
///
/// - `ids_ptr` must point to `ids_len` actor IDs when `ids_len > 0`.
/// - `out` must be a valid mutable pointer to writable [`DrainOutcomeRepr`] storage.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_drain_set(
    ids_ptr: *const ActorId,
    ids_len: usize,
    timeout_ns: u64,
    out: *mut DrainOutcomeRepr,
) -> i32 {
    if out.is_null() {
        crate::set_last_error("hew_actor_drain_set: null outcome pointer");
        return -1;
    }

    let ids = if ids_len == 0 {
        &[]
    } else if ids_ptr.is_null() {
        crate::set_last_error("hew_actor_drain_set: null ids pointer");
        return -1;
    } else {
        // SAFETY: caller guarantees `ids_ptr` points to `ids_len` readable ActorIds.
        unsafe { std::slice::from_raw_parts(ids_ptr, ids_len) }
    };

    let deadline = std::time::Instant::now() + std::time::Duration::from_nanos(timeout_ns);
    let outcome = drain_actors(ids, deadline);
    // SAFETY: caller guarantees `out` points to writable storage.
    let out = unsafe { &mut *out };
    if let Err(err) = write_drain_outcome_repr(out, outcome) {
        crate::set_last_error(err);
        // SAFETY: `out` points to initialized repr storage owned by the caller.
        unsafe { hew_actor_drain_outcome_free(out) };
        return -1;
    }

    0
}

// ── Budget API ──────────────────────────────────────────────────────────

/// Register a Hew actor type name for a dispatch function.
///
/// Generated code calls this once per actor type (before spawning any
/// instance) so the profiler can display the Hew type name instead of the
/// generic `"Actor"` label.
///
/// `name` must be a NUL-terminated string with static lifetime (i.e. a
/// string literal baked into the binary).  The function is idempotent:
/// subsequent calls for the same `dispatch` pointer are silently ignored.
///
/// # Safety
///
/// - `dispatch` must be a valid dispatch function for the actor type.
/// - `name` must point to a valid NUL-terminated UTF-8 string with `'static`
///   lifetime.
#[cfg(feature = "profiler")]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_register_type(
    dispatch: *const c_void,
    name: *const std::ffi::c_char,
) {
    if name.is_null() || dispatch.is_null() {
        return;
    }
    // SAFETY: Caller guarantees `name` is a NUL-terminated static string.
    let cstr = unsafe { std::ffi::CStr::from_ptr(name) };
    // Pass the owned name to the registry, which leaks it to `&'static str`
    // exactly once — only when it actually inserts a new type. Type names must
    // outlive all profiler snapshots, but this call fires once per *spawn*, not
    // once per *type*: leaking here would orphan one string per spawn of an
    // already-registered type. Deferring the leak into the table's locked
    // insert keeps it one-per-type.
    // SHIM: WHY: the registry stores `&'static str`; the leak-on-insert keeps
    //       it bounded to one string per actor type.
    //       WHEN: Remove the leak if we switch to an owned/`Arc<str>` map.
    //       REAL: Store an `Arc<str>` or intern into a static arena.
    // JIT LEAK RISK: Under ORCv2 JIT reloads each *new* unique dispatch fn
    //       leaks one String per reload cycle (the table inserts a fresh key).
    //       `clear_dispatch_registry()` (called at session reset) clears the
    //       pointer-to-name map entries but cannot reclaim the leaked strings.
    //       Acceptable for Milestone 2; tracked in #1226 M3 (ORCv2
    //       ResourceTracker choreography).
    let Ok(s) = cstr.to_str() else { return };
    let name = s.to_owned();
    // Convert the void pointer to the dispatch function type for registration.
    // SAFETY: The caller has cast the dispatch function pointer to void*; we cast it
    // back to the correct function pointer type. This is safe as long as the caller
    // passed a valid dispatch function pointer.
    let dispatch_fn: Option<HewDispatchFn> = unsafe { std::mem::transmute(dispatch) };
    crate::profiler::actor_registry::register_dispatch_type(dispatch_fn, name);
}

/// No-op stub for non-profiler native builds.
///
/// The symbol must exist so that codegen can emit unconditional calls to
/// `hew_actor_register_type` without needing to know whether the profiler
/// feature is enabled.  In non-profiler builds this is a near-zero-cost no-op.
///
/// SHIM: WHY: Codegen cannot conditionally emit calls based on Rust feature flags.
///       WHEN: Remove if we add a build-system mechanism to communicate the profiler
///       feature flag to the codegen.
///       REAL: Pass a feature flag to the codegen so it can omit the call entirely.
///
/// # Safety
///
/// This stub never dereferences its arguments, so any pointer values are
/// accepted. The signature stays `unsafe extern "C"` to match the
/// profiler-enabled variant that codegen links against.
#[cfg(not(feature = "profiler"))]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_register_type(
    _dispatch: *const c_void,
    _name: *const std::ffi::c_char,
) {
}

/// Register a fully-qualified handler name for a native profiler build.
///
/// Generated code calls this once per `(actor_type, handler)` pair at program
/// startup (alongside `hew_actor_register_type`) so the profiler can resolve
/// `msg_type` integers to human-readable names in trace events.
///
/// The key is `(dispatch_fn_ptr, msg_type)` — this is unambiguous even when
/// multiple actor types use overlapping `msg_type` integers (unlike the WASM
/// bridge's flat `msg_type → name` map).
///
/// `name` must be a NUL-terminated `"ActorName.handler_name"` string with
/// static lifetime (a string literal baked into the binary).
///
/// # Safety
///
/// - `dispatch` must be a valid dispatch function for the actor type.
/// - `name` must point to a valid NUL-terminated UTF-8 string with `'static`
///   lifetime.
#[cfg(feature = "profiler")]
#[no_mangle]
pub unsafe extern "C" fn hew_register_handler_name(
    dispatch: *const c_void,
    msg_type: i32,
    name: *const std::ffi::c_char,
) {
    if name.is_null() || dispatch.is_null() {
        return;
    }
    // SAFETY: Caller guarantees `name` is a NUL-terminated static string.
    let cstr = unsafe { std::ffi::CStr::from_ptr(name) };
    let Ok(s) = cstr.to_str() else { return };
    // Convert the void pointer to the dispatch function type for registration.
    // SAFETY: The caller has cast the dispatch function pointer to void*; we cast it
    // back to the correct function pointer type. This is safe as long as the caller
    // passed a valid dispatch function pointer.
    let dispatch_fn: Option<HewDispatchFn> = unsafe { std::mem::transmute(dispatch) };
    crate::profiler::actor_registry::register_handler_name(dispatch_fn, msg_type, s.to_owned());
}

/// No-op stub for non-profiler native builds.
///
/// SHIM: WHY: Codegen emits unconditional calls; profiler feature determines
///       whether the body does anything.
///       WHEN: Remove if a build-system mechanism can communicate feature flags to codegen.
///       REAL: Pass a feature flag to the codegen so it can omit the call entirely.
///
/// # Safety
///
/// This stub never dereferences its arguments, so any pointer values are
/// accepted. The signature stays `unsafe extern "C"` to match the
/// profiler-enabled variant that codegen links against.
#[cfg(not(feature = "profiler"))]
#[no_mangle]
pub unsafe extern "C" fn hew_register_handler_name(
    _dispatch: *const c_void,
    _msg_type: i32,
    _name: *const std::ffi::c_char,
) {
}

/// Set the per-actor message processing budget.
///
/// A budget of `0` resets to the default ([`HEW_MSG_BUDGET`]).
///
/// # Safety
///
/// `actor` must be a valid pointer returned by a spawn function.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_set_budget(actor: *mut HewActor, budget: u32) {
    cabi_guard!(actor.is_null());
    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };
    #[expect(
        clippy::cast_possible_wrap,
        reason = "budget values are small positive integers, well within i32 range"
    )]
    if budget == 0 {
        a.budget.store(HEW_MSG_BUDGET, Ordering::Relaxed);
    } else {
        a.budget.store(budget as i32, Ordering::Relaxed);
    }
}

/// Query the current per-actor message processing budget.
///
/// # Safety
///
/// `actor` must be a valid pointer returned by a spawn function.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_get_budget(actor: *const HewActor) -> u32 {
    cabi_guard!(actor.is_null(), 0);
    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };
    #[expect(
        clippy::cast_sign_loss,
        reason = "budget is always set to a positive value"
    )]
    let result = a.budget.load(Ordering::Relaxed) as u32;
    result
}

/// Register the actor's SYSTEM dispatch entry point.
///
/// The second dispatch channel: nodes dequeued with
/// [`crate::mailbox_header::Origin::Sys`] are routed here, and nodes dequeued
/// with `Origin::User` are routed to `dispatch`. Neither can reach the other,
/// so the application `msg_type` namespace and the lifecycle-signal namespace
/// are disjoint by construction rather than by a reserved-value convention.
///
/// Generated code emits this immediately after `hew_actor_spawn` /
/// `hew_actor_spawn_opts` for every actor type, alongside the state
/// clone/drop registration. Passing `None` leaves the actor with no system
/// entry point, in which case an arriving lifecycle signal is dropped with a
/// diagnostic (fail-closed) rather than routed to the user trampoline.
///
/// # Safety
///
/// - `actor` may be null (no-op); if non-null it must be a valid pointer
///   returned by a spawn function.
/// - `sys_dispatch` must match [`HewSysDispatchFn`] exactly.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_set_sys_dispatch(
    actor: *mut HewActor,
    sys_dispatch: Option<HewSysDispatchFn>,
) {
    cabi_guard!(actor.is_null());
    // SAFETY: caller guarantees `actor` is valid and exclusively owned during
    // post-spawn registration.
    unsafe {
        (*actor).sys_dispatch = sys_dispatch;
    }
}

/// Register a terminate callback on an actor.
///
/// The terminate function is called with the actor's state pointer when
/// the actor transitions to the Stopped state (or at process exit for
/// actors still idle). Panics inside the callback are caught and do not
/// prevent cleanup.
///
/// # Safety
///
/// - `actor` must be a valid pointer returned by a spawn function.
/// - `terminate_fn` must point to a function with C ABI that accepts
///   a single `*mut c_void` (the actor state).
#[no_mangle]
pub unsafe extern "C" fn hew_actor_set_terminate(
    actor: *mut HewActor,
    terminate_fn: unsafe extern "C-unwind" fn(*mut c_void),
) {
    cabi_guard!(actor.is_null());
    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &mut *actor };
    a.terminate_fn = Some(terminate_fn);
}

/// Register a state-drop callback on an actor.
///
/// The state-drop function is called with the actor's live state pointer
/// (`a.state`) immediately before `crate::mem::buf_free(a.state)` in
/// `free_actor_resources`. Codegen emits one such function per actor that
/// walks every owned field and invokes its `impl Drop`. Types that do not
/// participate in RAII generate an empty body — calling state-drop is a
/// no-op for actors with only value-type fields.
///
/// State-drop runs after every `#[on(stop)]` hook has finished and before
/// the state allocation is freed, so the field-level `Drop` callbacks
/// see the same state pointer the runtime is about to release. State-drop
/// is invoked on `a.state` only; the companion `a.init_state` is a byte
/// memcpy of the same wrapper buffer (its embedded field pointers alias
/// `a.state`'s) and is released with `buf_free` of just the wrapper
/// bytes. Walking it through state-drop would double-free every owned field.
/// The supervisor child spec holds its own independent deep copy used for
/// restarts and never reads `a.init_state`.
///
/// **Calling window**: safe to call any time between a successful spawn and the
/// first message dispatch. Codegen emits the call immediately after spawn in
/// the same basic block, satisfying this constraint. Calling after the actor
/// has started processing messages is a data race on `state_drop_fn`.
///
/// **Supervisor back-fill**: [`hew_supervisor_set_child_state_drop`] calls this
/// function on the already-spawned actor so that both the in-flight actor and
/// every future restart see the same drop callback. The supervisor stores the
/// pointer in its child spec and re-applies it to each newly spawned actor in
/// `restart_child_from_spec`.
///
/// # Safety
///
/// - `actor` must be a valid pointer returned by a spawn function.
/// - `state_drop_fn` must point to a function with C ABI that accepts a
///   single `*mut c_void` (the actor state), and must be safe to call once
///   on that allocation immediately before it is freed. The function pointer
///   must remain valid for the entire lifetime of the actor — it is stored
///   in the actor struct and invoked during teardown without further
///   lifetime checks.
/// - This setter has a null guard (unlike [`hew_actor_set_terminate`]).
///   Codegen wraps the `hew_actor_set_state_drop` call in an explicit null
///   check so that an OOM spawn (which returns null) skips the FFI call
///   entirely. This runtime guard is a second layer of defence-in-depth for
///   the same OOM path.
///   `hew_actor_set_terminate` has no equivalent at either layer — its codegen
///   emit site is unconditional and this function has no runtime null check.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_set_state_drop(
    actor: *mut HewActor,
    state_drop_fn: unsafe extern "C" fn(*mut c_void),
) {
    // Spawn paths return null on allocation failure (see hew_actor_spawn /
    // hew_actor_spawn_opts). The codegen null-guard (an explicit null check
    // before the hew_actor_set_state_drop call) already skips this function on
    // OOM. This cabi_guard is defence-in-depth. The terminate path has neither
    // guard: its codegen call is unconditional and hew_actor_set_terminate has
    // no runtime null check.
    cabi_guard!(actor.is_null());
    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &mut *actor };
    a.state_drop_fn = Some(state_drop_fn);
}

/// Register the typed queued-message destructor on a spawned actor.
///
/// # Safety
///
/// `actor` must be a live actor pointer or null. `message_drop_fn` must match
/// every handler payload layout for the actor and remain valid for its lifetime.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_set_message_drop(
    actor: *mut HewActor,
    message_drop_fn: unsafe extern "C" fn(i32, *mut c_void, usize),
) {
    cabi_guard!(actor.is_null());
    // SAFETY: caller guarantees `actor` is valid.
    let mailbox_ptr = unsafe { (*actor).mailbox };
    #[cfg(not(target_arch = "wasm32"))]
    // SAFETY: the actor owns a live native mailbox and the callback lifetime is
    // guaranteed by the caller.
    unsafe {
        mailbox::hew_mailbox_set_message_drop_fn(mailbox_ptr.cast(), Some(message_drop_fn));
    }
}

/// Register the codegen-emitted deep-clone callback on a spawned actor.
///
/// Symmetric to [`hew_actor_set_state_drop`]. Stored on the actor struct
/// so future direct-spawn restart consumers can deep-clone the initial-state
/// template without going through a supervisor; today the supervisor
/// back-fills this slot from its child spec after every restart (mirror of
/// `state_drop_fn` back-fill).
///
/// # Safety
///
/// - `actor` must be a valid pointer returned by a spawn function, or null
///   (null is a no-op for OOM-spawn parity with [`hew_actor_set_state_drop`]).
/// - `state_clone_fn` must point to a function matching the [`HewStateCloneFn`]
///   contract: reads `init_state_size` bytes from `src`, returns a freshly
///   `malloc`-compatible heap-allocated wrapper with independent owned-field
///   clones, or null on allocation failure. The function pointer must remain
///   valid for the entire lifetime of the actor.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_set_state_clone(
    actor: *mut HewActor,
    state_clone_fn: HewStateCloneFn,
) {
    // Matches the cabi_guard / null-tolerance shape of hew_actor_set_state_drop;
    // codegen (Lane A2) will null-guard the call site analogously for OOM spawn.
    cabi_guard!(actor.is_null());
    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &mut *actor };
    a.state_clone_fn = Some(state_clone_fn);
}

/// Set the per-actor reduction budget (operations per dispatch).
///
/// A value of `0` resets to the default ([`HEW_DEFAULT_REDUCTIONS`]).
/// Higher values allow an actor to run longer before yielding.
///
/// # Safety
///
/// `actor` must be a valid pointer returned by a spawn function.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_set_reductions(actor: *mut HewActor, reductions: u32) {
    cabi_guard!(actor.is_null());
    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };
    #[expect(
        clippy::cast_possible_wrap,
        reason = "reduction values are small positive integers, well within i32 range"
    )]
    if reductions == 0 {
        a.reductions
            .store(HEW_DEFAULT_REDUCTIONS, Ordering::Relaxed);
    } else {
        a.reductions.store(reductions as i32, Ordering::Relaxed);
    }
}

/// Query the current per-actor reduction budget.
///
/// # Safety
///
/// `actor` must be a valid pointer returned by a spawn function.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_get_reductions(actor: *const HewActor) -> u32 {
    cabi_guard!(actor.is_null(), 0);
    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };
    #[expect(
        clippy::cast_sign_loss,
        reason = "reductions is always set to a positive value"
    )]
    {
        a.reductions.load(Ordering::Relaxed) as u32
    }
}

/// Enable hibernation for an actor.
///
/// When an actor goes through `threshold` consecutive activations with
/// zero messages, it is marked as hibernating. A hibernating actor is
/// skipped by the scheduler until a new message arrives.
///
/// Pass 0 to disable hibernation (default).
///
/// # Safety
///
/// `actor` must be a valid pointer returned by a spawn function.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_set_hibernation(actor: *mut HewActor, threshold: c_int) {
    cabi_guard!(actor.is_null());
    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };
    a.hibernation_threshold
        .store(threshold.max(0), Ordering::Relaxed);
    // Reset hibernation state when threshold changes.
    a.idle_count.store(0, Ordering::Relaxed);
    a.hibernating.store(0, Ordering::Relaxed);
}

/// Return 1 if the actor is currently hibernating, 0 otherwise.
///
/// # Safety
///
/// `actor` must be a valid pointer returned by a spawn function.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_is_hibernating(actor: *const HewActor) -> c_int {
    cabi_guard!(actor.is_null(), 0);
    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };
    a.hibernating.load(Ordering::Relaxed)
}

/// Wake an actor from hibernation.
///
/// This is automatically called when a message is sent to a hibernating
/// actor, but can also be called explicitly.
///
/// # Safety
///
/// `actor` must be a valid pointer returned by a spawn function.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_wake(actor: *mut HewActor) {
    cabi_guard!(actor.is_null());
    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };
    a.idle_count.store(0, Ordering::Relaxed);
    a.hibernating.store(0, Ordering::Relaxed);
}

/// Update hibernation tracking after an activation cycle.
///
/// - If no messages were processed and the threshold is set, increments the
///   idle counter and sets the hibernating flag once the threshold is reached.
/// - If messages were processed, resets both the idle counter and the flag.
/// - If neither condition applies (threshold == 0 and msgs == 0), does nothing.
#[inline]
pub(crate) fn update_hibernation_state(a: &HewActor, msgs_processed: u32) {
    let hib_threshold = a.hibernation_threshold.load(Ordering::Relaxed);
    if msgs_processed == 0 && hib_threshold > 0 {
        let prev_idle = a.idle_count.fetch_add(1, Ordering::Relaxed);
        if prev_idle + 1 >= hib_threshold {
            a.hibernating.store(1, Ordering::Relaxed);
        }
    } else if msgs_processed > 0 {
        a.idle_count.store(0, Ordering::Relaxed);
        a.hibernating.store(0, Ordering::Relaxed);
    }
}
///
/// - 0 = high priority (gets 2× message budget)
/// - 1 = normal priority (default)
/// - 2 = low priority (gets ½ message budget)
///
/// Values outside 0-2 are clamped.
///
/// # Safety
///
/// `actor` must be a valid pointer returned by a spawn function.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_set_priority(actor: *mut HewActor, priority: c_int) {
    cabi_guard!(actor.is_null());
    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };
    let clamped = priority.clamp(HEW_PRIORITY_HIGH, HEW_PRIORITY_LOW);
    a.priority.store(clamped, Ordering::Relaxed);
}

/// Query the current scheduling priority.
///
/// Returns 0 (high), 1 (normal), or 2 (low).
///
/// # Safety
///
/// `actor` must be a valid pointer returned by a spawn function.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_get_priority(actor: *const HewActor) -> c_int {
    cabi_guard!(actor.is_null(), HEW_PRIORITY_NORMAL);
    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };
    a.priority.load(Ordering::Relaxed)
}

// ── Internal send helper ────────────────────────────────────────────────

/// Send a message, returning a runtime error code.
///
/// # Safety
///
/// Same requirements as [`hew_actor_send`].
/// Fail-closed cross-runtime boundary check for the held-actor-pointer send /
/// ask / by-id paths.
///
/// Returns `true` when `a` is owned by the runtime currently bound on this
/// thread, so the send may proceed. Returns `false` when the calling runtime
/// and the target actor's stamped `runtime_id` disagree — a foreign pointer
/// reached a send path. The caller refuses the operation (`ErrForeignRuntime`)
/// rather than routing it: the runtime ids are compared as plain discriminants,
/// so nothing in the foreign runtime is dereferenced to make the decision
/// (`boundary-fail-closed`).
///
/// In a single-runtime program every actor carries `RuntimeId::DEFAULT` and the
/// thread resolves the same default runtime, so this always returns `true` and
/// the check is invisible. It becomes load-bearing once more than one runtime
/// can exist in one process (the multi-runtime / `:reset` milestone), where it
/// is the wall that keeps two runtimes' actors from accepting each other's
/// pointers. A mismatch is a logic error, not a normal outcome, so it is logged
/// once at the boundary before the refusal.
#[inline]
pub(crate) fn actor_runtime_matches(a: &HewActor) -> bool {
    // wasm32 has one runtime by construction, so no actor can be foreign to the
    // caller and the boundary is always in-runtime.
    #[cfg(target_arch = "wasm32")]
    {
        let _ = a;
        true
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        // Resolve the calling runtime's id without trapping when none is installed.
        // With no runtime bound there is no second runtime the actor could be
        // foreign to, so the boundary treats the send as in-runtime — it must not
        // introduce a "runtime must be installed" precondition the pre-check never
        // had (e.g. an alias send before init, or a unit test driving a send path
        // without a runtime guard).
        let Some(current) = crate::runtime::rt_current_id() else {
            return true;
        };
        if current == a.runtime_id {
            return true;
        }
        eprintln!(
            "hew-runtime: refused a send to actor {:#x} owned by runtime {} from runtime {} \
             (cross-runtime boundary; pointer not routed)",
            a.id,
            a.runtime_id.as_u64(),
            current.as_u64(),
        );
        false
    }
}

/// Terminal-state send gate: `true` once the actor has been published into a
/// terminal state (`Crashed`/`Stopped`) by [`hew_actor_trap`]'s authoritative
/// CAS, so the send path must reject before touching the mailbox.
///
/// `hew_actor_trap` takes the terminal CAS BEFORE closing the mailbox (the
/// lost-crash-notify fix: making the trap the authoritative terminator so the
/// worker's settle CAS cannot self-stop the actor out from under it). That
/// reorder leaves a window — terminal CAS done, mailbox not yet closed — in
/// which the mailbox is still open. Without this gate a send racing that window
/// observes the open mailbox and enqueues an undeliverable node (or, for the
/// alias path, consumes the caller's refcount into one), reporting false
/// success into an actor that will never dispatch it. The mailbox-closed check
/// alone does not cover this window because the close has not happened yet.
///
/// The gate closes it: the trap publishes terminal with a release CAS, so a
/// sender's acquire-load that observes the terminal state rejects exactly as a
/// send to a closed mailbox would — same outcome, releasing the alias envelope
/// and returning `ErrActorStopped`, so no send succeeds after terminal
/// publication. A sender that loads non-terminal proceeds to the mailbox, where
/// the existing closed check rejects it once the trap's close lands; the only
/// node that can still land is one whose enqueue linearizes before BOTH the
/// gate observes terminal and the close runs — the inherent "sent the instant
/// before the crash" case, drained by `hew_mailbox_free`, identical to every
/// prior ordering.
#[inline]
pub(crate) fn actor_send_is_terminal(a: &HewActor) -> bool {
    let state = a.actor_state.load(Ordering::Acquire);
    state == HewActorState::Crashed as i32 || state == HewActorState::Stopped as i32
}

pub(crate) unsafe fn actor_send_result_internal(
    actor: *mut HewActor,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
) -> i32 {
    // SAFETY: Caller guarantees `actor` is valid.
    unsafe { actor_send_result_internal_reply(actor, msg_type, data, size, ptr::null_mut()) }
}

/// Submit a tell while the caller holds an identity-verified actor send pin.
///
/// # Safety
///
/// `data` must point to `size` readable bytes.
pub(crate) unsafe fn actor_send_pinned(
    pin: &crate::lifetime::live_actors::ActorPin,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
) -> i32 {
    // SAFETY: the typed pin proves the actor allocation remains live; the
    // caller supplies the readable payload range.
    unsafe { actor_send_result_internal(pin.as_ptr(), msg_type, data, size) }
}

/// Like [`actor_send_result_internal`] but with an explicit reply channel
/// that is set on the message node (for the ask pattern).
pub(crate) unsafe fn actor_send_result_internal_reply(
    actor: *mut HewActor,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
    reply_channel: *mut c_void,
) -> i32 {
    cabi_guard!(actor.is_null(), HewError::ErrActorStopped as i32);
    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };

    // Fail closed if this actor belongs to a different runtime than the caller
    // (the single routing authority for held-pointer send/ask/by-id; by-id and
    // wire delivery both funnel through here). Never fires single-runtime.
    if !actor_runtime_matches(a) {
        return HewError::ErrForeignRuntime as i32;
    }

    #[cfg(not(target_arch = "wasm32"))]
    let Ok(_ingress) = crate::shutdown::admit_external_work() else {
        return HewError::ErrActorStopped as i32;
    };

    // Terminal-state send gate (see `actor_send_is_terminal`): reject once the
    // actor is terminal, even if its mailbox is not yet closed — closes the
    // trap's terminal-CAS-before-mailbox-close window so no copy-mode send
    // enqueues into, or reports false success against, a terminal actor.
    if actor_send_is_terminal(a) {
        return HewError::ErrActorStopped as i32;
    }

    // Check for injected drop fault (testing only). The message is discarded
    // without reaching the queue, which is the same observable outcome as a
    // declared-policy loss: report it as loss, never as delivery.
    if crate::deterministic::check_drop_fault(a.id) {
        return HEW_ACTOR_SEND_MESSAGE_LOST;
    }

    let mb = a.mailbox.cast::<HewMailbox>();

    if reply_channel.is_null() {
        // A no-reply send still preserves the raw policy outcome so checked
        // language sends can distinguish delivery from declared loss. A
        // DropNew outcome queued nothing and therefore must not wake the
        // actor; DropOld and Coalesce did leave runnable work behind.
        // SAFETY: Mailbox is valid for the actor's lifetime; data/size from caller.
        return match unsafe { mailbox::hew_mailbox_send_fire_and_forget(mb, msg_type, data, size) }
        {
            mailbox::SendOutcome::Enqueued => {
                // SAFETY: `actor`/`a` valid; a node actually reached the
                // queue, so the actor may be scheduled to run.
                unsafe { schedule_actor_after_enqueue(actor, a, msg_type) };
                HewError::Ok as i32
            }
            mailbox::SendOutcome::Coalesced | mailbox::SendOutcome::DroppedOld => {
                // A node reached the queue (or was updated in place), so the
                // actor must run even though this send reports visible loss.
                // SAFETY: `actor` and `a` remain pinned and valid for this send.
                unsafe { schedule_actor_after_enqueue(actor, a, msg_type) };
                HEW_ACTOR_SEND_MESSAGE_LOST
            }
            // DropNew consumed the incoming payload but queued nothing.
            mailbox::SendOutcome::Dropped => HEW_ACTOR_SEND_MESSAGE_LOST,
            mailbox::SendOutcome::Closed => HewError::ErrActorStopped as i32,
            // `Fail`-policy overflow is a genuine, caller-visible failure —
            // never silently dropped.
            mailbox::SendOutcome::Failed => HewError::ErrMailboxFull as i32,
            mailbox::SendOutcome::Oom => HewError::ErrOom as i32,
        };
    }

    // Ask (reply channel attached): every overflow outcome, including a
    // policy-drop, must stay caller-visible — a silently dropped ask would
    // leave the caller waiting forever for a reply that will never arrive.
    // SAFETY: Mailbox is valid for the actor's lifetime; reply_channel is non-null and valid.
    // Suspending asks must not park a scheduler worker while waiting for
    // bounded `Block` capacity. A pending ask stays suspended on its reply
    // channel; mailbox admission transfers its queued node to the actor FIFO.
    let result = unsafe {
        mailbox::mailbox_send_with_reply_cooperative(mb, msg_type, data, size, reply_channel)
    };
    if result != 0 {
        return result;
    }

    // SAFETY: `actor`/`a` valid; the message is enqueued so the actor
    // may be scheduled to run.
    unsafe { schedule_actor_after_enqueue(actor, a, msg_type) };

    HewError::Ok as i32
}

/// Retire an enqueue that completed after a terminal drain but before the
/// producer attempted its wake CAS.
///
/// # Safety
///
/// `a` must remain live through the terminal-state and dispatch-owner probes.
pub(crate) unsafe fn reclaim_terminal_enqueue_if_unowned(a: &HewActor) {
    // SAFETY: production always closes the dispatch-owner handoff. The
    // test-only false branch below is the exact pre-fix omission oracle.
    unsafe { reclaim_terminal_enqueue_if_unowned_inner(a, true) };
}

/// Implementation seam for [`reclaim_terminal_enqueue_if_unowned`].
///
/// `close_dispatch_handoff = false` exists only to execute the precise
/// pre-fix counterfactual in the delayed-link regression.
///
/// # Safety
///
/// Same contract as [`reclaim_terminal_enqueue_if_unowned`].
unsafe fn reclaim_terminal_enqueue_if_unowned_inner(a: &HewActor, close_dispatch_handoff: bool) {
    // A terminal publisher may have drained before this producer, which had
    // already passed the mailbox-open check, completed its enqueue. Once the
    // wake CAS loses to terminal and no activation owns the consumer, this
    // producer is the only remaining site guaranteed to run. Help with a
    // serialised terminal drain so the late node cannot remain stranded.
    if !actor_send_is_terminal(a) {
        return;
    }

    if a.dispatch_active.load(Ordering::Acquire) && !close_dispatch_handoff {
        return;
    }

    // Test dispatch ownership while holding the same terminal-reclaim lock as
    // the activation's final drain and Release-clear. If this producer gets the
    // lock first and sees an owner, that owner must drain after the fully-linked
    // enqueue. If it gets the lock after the owner, the cleared flag authorizes
    // this producer to drain. Self-sends remain non-deadlocking: they take the
    // lock, observe their own active frame, and defer to its eventual final
    // drain.
    //
    // SAFETY: terminal state prevents a new activation from winning. A false
    // dispatch-active predicate proves no existing activation consumes the
    // mailbox. The lock serialises other terminal helpers. By-ID callers hold a
    // send pin through this point; held-pointer callers' public contract
    // requires the actor allocation to remain live for the call.
    unsafe {
        mailbox::mailbox_reclaim_queued_terminal_if(a.mailbox.cast::<HewMailbox>(), || {
            !a.dispatch_active.load(Ordering::Acquire)
        });
    }
}

/// Complete the one post-link handoff shared by every native mailbox producer.
///
/// A successful `Idle -> Runnable` transition publishes one scheduler entry.
/// Every other outcome still passes through terminal handoff so a producer
/// that completed a delayed MPSC predecessor link cannot strand its node after
/// the last activation drain.
///
/// # Safety
///
/// `actor` must be live for the call and `a` must borrow the same allocation.
pub(crate) unsafe fn finish_mailbox_enqueue(actor: *mut HewActor, a: &HewActor) {
    // SAFETY: production always includes terminal handoff.
    unsafe { finish_mailbox_enqueue_inner(actor, a, true) };
}

/// Test seam for the canonical post-link handoff.
///
/// `close_terminal_handoff = false` executes the exact omission: the producer
/// still performs its wake CAS but does not help reclaim after terminal wins.
///
/// # Safety
///
/// Same contract as [`finish_mailbox_enqueue`].
pub(crate) unsafe fn finish_mailbox_enqueue_inner(
    actor: *mut HewActor,
    a: &HewActor,
    close_terminal_handoff: bool,
) {
    let observed = a.actor_state.load(Ordering::Acquire);
    if observed != HewActorState::Idle as i32 {
        if close_terminal_handoff {
            // SAFETY: this producer just completed an enqueue against `a`.
            unsafe { reclaim_terminal_enqueue_if_unowned(a) };
        }
        return;
    }

    // Own the prospective queue entry before publishing Runnable. If the CAS
    // loses, dropping the unused entry releases the reference; if it wins,
    // ownership moves through the queue into `dispatch_active`.
    // SAFETY: the function contract guarantees the actor is live.
    let queue_entry = unsafe { crate::activation::SchedulerQueueEntry::retain(actor) };
    if a.actor_state
        .compare_exchange(
            HewActorState::Idle as i32,
            HewActorState::Runnable as i32,
            Ordering::AcqRel,
            Ordering::Acquire,
        )
        .is_ok()
    {
        a.idle_count.store(0, Ordering::Relaxed);
        a.hibernating.store(0, Ordering::Relaxed);
        crate::resume::sched_enqueue_owned(queue_entry);
    } else {
        drop(queue_entry);
        if close_terminal_handoff {
            // SAFETY: this producer just completed an enqueue against `a`.
            unsafe { reclaim_terminal_enqueue_if_unowned(a) };
        }
    }
}

/// Enqueue one typed runtime system signal and perform the canonical post-link
/// wake/terminal handoff.
///
/// # Safety
///
/// `actor` must be a non-null live actor pointer for the call. `data` must
/// point to `size` readable bytes (or be null when `size == 0`).
#[cfg(not(target_arch = "wasm32"))]
pub(crate) unsafe fn send_system_message(
    actor: *mut HewActor,
    kind: crate::mailbox_header::HewSysMsg,
    data: *mut c_void,
    size: usize,
) -> bool {
    if actor.is_null() {
        return false;
    }
    // SAFETY: caller guarantees a live actor.
    let a = unsafe { &*actor };
    let mailbox = a.mailbox.cast::<HewMailbox>();
    if mailbox.is_null() {
        return false;
    }
    // SAFETY: actor ownership keeps the mailbox live and caller supplies data.
    if !unsafe { mailbox::mailbox_send_sys_checked(mailbox, kind, data, size) } {
        return false;
    }
    // SAFETY: the system node is fully linked and actor remains live.
    unsafe { finish_mailbox_enqueue(actor, a) };
    true
}

/// Record the send and wake the actor, or retire a late enqueue if a terminal
/// transition already won.
///
/// # Safety
///
/// `actor` must be a valid pointer and `a` must borrow the same actor.
pub(crate) unsafe fn schedule_actor_after_enqueue(
    actor: *mut HewActor,
    a: &HewActor,
    msg_type: i32,
) {
    let sender = hew_actor_self();
    let trace_actor_id = if sender.is_null() {
        a.id
    } else {
        // SAFETY: the scheduler installs a live actor during dispatch.
        unsafe { (*sender).id }
    };
    crate::tracing::record_send(trace_actor_id, msg_type);

    // Deterministic ownership seam: the message (and an ask's retained sender
    // reference) is already owned by the mailbox, but this sender has not yet
    // attempted its wake CAS.
    #[cfg(test)]
    run_send_post_enqueue_pre_wake_hook(a);

    // SAFETY: this producer fully linked a node and still owns actor lifetime.
    unsafe { finish_mailbox_enqueue(actor, a) };
}

/// Send a message, returning `true` on success.
///
/// # Safety
///
/// Same requirements as [`hew_actor_send`].
pub(crate) unsafe fn actor_send_internal(
    actor: *mut HewActor,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
) -> bool {
    // SAFETY: same preconditions as actor_send_result_internal; we only
    // translate its error code into a boolean success/failure result.
    unsafe { actor_send_result_internal(actor, msg_type, data, size) == HewError::Ok as i32 }
}

#[cfg(not(target_arch = "wasm32"))]
#[derive(Clone, Copy)]
enum AskReplyChannelFailureCleanup {
    FreeCreatorRef,
    KeepCreatorRef,
}

#[cfg(not(target_arch = "wasm32"))]
unsafe fn submit_ask_with_reply_channel<F>(
    ch: *mut HewReplyChannel,
    failure_cleanup: AskReplyChannelFailureCleanup,
    send: F,
) -> i32
where
    F: FnOnce(*mut HewReplyChannel) -> i32,
{
    if ch.is_null() {
        // Classify the refusal before returning the raw code: with-channel
        // callers read the failure kind from `hew_actor_ask_take_last_error`.
        record_ask_error(send_err_to_ask_err(HewError::ErrOom as i32));
        return HewError::ErrOom as i32;
    }

    // Retain a sender-side reference before enqueueing so mailbox teardown and
    // successful replies consume the queued ref while the caller keeps its own.
    // DROP-SAFETY: send failure must release both references for owned ask
    // channels and only the queued retain for caller-provided channels.
    unsafe { reply_channel::hew_reply_channel_retain(ch) };

    let send_result = send(ch);
    if send_result != HewError::Ok as i32 {
        // Classify the failure in the TLS ask-error slot BEFORE returning the
        // raw code: the suspending (with-channel) callers surface their Err
        // through `hew_actor_ask_take_last_error`, and an unwritten slot
        // misreports the failure as `AskError::None`. The blocking twins
        // overwrite this with the same mapped value via `actor_ask_null`.
        record_ask_error(send_err_to_ask_err(send_result));
        if send_result == HewError::ErrOom as i32 {
            // Mirror `alloc_reply_buffer`: record allocation failure before the
            // error cleanup path releases the channel.
            // SAFETY: `ch` is still live until the cleanup frees below.
            unsafe { reply_channel::hew_reply_channel_mark_allocation_failed(ch) };
        }
        // SAFETY: release the queued sender-side reference retained above.
        unsafe { reply_channel::hew_reply_channel_free(ch) };
        if matches!(
            failure_cleanup,
            AskReplyChannelFailureCleanup::FreeCreatorRef
        ) {
            // SAFETY: owned ask paths must also release the creator reference.
            unsafe { reply_channel::hew_reply_channel_free(ch) };
        }
    }

    send_result
}

/// Submit an ask with a caller-owned reply channel against an actor
/// allocation whose liveness the CALLER pins (the owner-scoped stable-role
/// path: `hew_supervisor_role_ask_with_channel` resolves the child slot and
/// submits while holding the supervisor's `children_lock`, so the incarnation
/// cannot be replaced or reclaimed across the submission).
///
/// Channel-reference discipline is identical to
/// [`hew_actor_ask_with_channel`]: the queued sender-side ref is retained here
/// and released on a failed submission; the caller-provided creator ref
/// survives failure so the caller can still free the channel.
///
/// # Safety
///
/// - `actor` must be a live `HewActor` the caller keeps live for the call.
/// - `data` and `ch` must satisfy [`hew_actor_ask_with_channel`]'s contract.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) unsafe fn ask_with_channel_pinned(
    actor: *mut HewActor,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
    ch: *mut c_void,
) -> i32 {
    // SAFETY: the caller pins `actor`; channel/data follow this fn's contract.
    unsafe {
        submit_ask_with_reply_channel(
            ch.cast(),
            AskReplyChannelFailureCleanup::KeepCreatorRef,
            |ch| actor_send_result_internal_reply(actor, msg_type, data, size, ch.cast()),
        )
    }
}

// ── Ask (request-response) ──────────────────────────────────────────────
// Native asks block on threaded reply channels; WASM asks cooperate by
// driving the single-threaded scheduler in bounded ticks.

/// Send a synchronous request and block until a reply arrives.
///
/// The reply channel pointer is **packed at the end** of the message
/// data, matching the C runtime convention:
/// `[original_data | reply_channel_ptr]`
///
/// Returns the reply value (caller must free with `buf_free`), or
/// null if no reply was produced.
///
/// # Safety
///
/// - `actor` must be a valid actor pointer.
/// - `data` must point to at least `size` readable bytes, or be null.
///
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_ask(
    actor: *mut HewActor,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
) -> *mut c_void {
    let ch = reply_channel::hew_reply_channel_new();

    // SAFETY: `ch` is a live reply channel owned by this ask call and the
    // closure uses the same actor/data preconditions as this function.
    let send_result = unsafe {
        submit_ask_with_reply_channel(ch, AskReplyChannelFailureCleanup::FreeCreatorRef, |ch| {
            // Send the message with the reply channel in the HewMsgNode
            // field (not packed in the data buffer).
            actor_send_result_internal_reply(actor, msg_type, data, size, ch.cast())
        })
    };

    if send_result != HewError::Ok as i32 {
        return actor_ask_null(send_err_to_ask_err(send_result));
    }

    // SAFETY: ch is valid, single-reader.
    let result = unsafe { reply_channel::hew_reply_wait(ch) };

    if result.is_null() {
        // Distinguish an orphaned ask (mailbox teardown before reply) from a
        // legitimate null reply deposited by the handler.
        // SAFETY: ch is still live — we hold the caller-side reference.
        let is_orphaned = unsafe { (*ch).orphaned.load(Ordering::Acquire) };
        // SAFETY: ch was created by hew_reply_channel_new.
        unsafe { reply_channel::hew_reply_channel_free(ch) };
        if is_orphaned {
            return actor_ask_null(AskError::OrphanedAsk);
        }
        // Legitimate null reply — clear any stale error.
        actor_ask_clear();
    } else {
        // SAFETY: ch was created by hew_reply_channel_new.
        unsafe { reply_channel::hew_reply_channel_free(ch) };
        actor_ask_clear();
    }

    result
}

/// Send a message and block until the actor replies or the timeout
/// expires.
///
/// Returns the reply value, or null on timeout.
///
/// # Safety
///
/// Same requirements as [`hew_actor_ask`].
///
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_ask_timeout(
    actor: *mut HewActor,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
    timeout_ms: i32,
) -> *mut c_void {
    let ch = reply_channel::hew_reply_channel_new();

    // SAFETY: `ch` is a live reply channel owned by this ask call and the
    // closure uses the same actor/data preconditions as this function.
    let send_result = unsafe {
        submit_ask_with_reply_channel(ch, AskReplyChannelFailureCleanup::FreeCreatorRef, |ch| {
            actor_send_result_internal_reply(actor, msg_type, data, size, ch.cast())
        })
    };

    if send_result != HewError::Ok as i32 {
        return actor_ask_null(send_err_to_ask_err(send_result));
    }

    // SAFETY: ch is valid, single-reader.
    let result = unsafe { reply_channel::hew_reply_wait_timeout(ch, timeout_ms) };

    if result.is_null() {
        // Distinguish timeout (channel not ready) from legitimate null reply or orphan.
        // SAFETY: ch is still live — we hold the caller-side reference.
        let is_ready = unsafe { reply_channel::hew_reply_channel_is_ready(ch) };
        if !is_ready {
            // Deadline elapsed before any reply arrived.
            // Mark the channel as cancelled so the late replier handles cleanup.
            // SAFETY: ch is still live while the caller-side reference is released.
            unsafe { reply_channel::hew_reply_channel_cancel(ch) };
            // SAFETY: release the caller-side reference after recording cancellation.
            unsafe { reply_channel::hew_reply_channel_free(ch) };
            return actor_ask_null(AskError::Timeout);
        }
        // Channel is ready but value is null — could be orphaned or legitimate.
        // SAFETY: ch is still live — we hold the caller-side reference.
        let is_orphaned = unsafe { (*ch).orphaned.load(Ordering::Acquire) };
        // SAFETY: ch was created by hew_reply_channel_new.
        unsafe { reply_channel::hew_reply_channel_free(ch) };
        if is_orphaned {
            return actor_ask_null(AskError::OrphanedAsk);
        }
        actor_ask_clear();
    } else {
        // Got a non-null reply — release the caller-side reference.
        // SAFETY: ch was created by hew_reply_channel_new.
        unsafe { reply_channel::hew_reply_channel_free(ch) };
        actor_ask_clear();
    }

    result
}

/// Send a message with a caller-provided reply channel.
///
/// The reply channel is packed into the message data.
/// The caller is responsible for waiting on and freeing `ch`.
///
/// # Safety
///
/// - `actor` must be a valid actor pointer.
/// - `data` must point to at least `size` readable bytes, or be null.
/// - `ch` must be a valid reply channel pointer.
///
/// Returns `0` ([`HewError::Ok`]) on success, or a negative [`HewError`] code
/// if the ask could not be submitted. Callers must handle failures explicitly
/// instead of waiting on `ch`, because no reply will ever arrive in that case.
///
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_ask_with_channel(
    actor: *mut HewActor,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
    ch: *mut HewReplyChannel,
) -> i32 {
    // SAFETY: `ch` is caller-provided and valid per this function's contract;
    // the closure forwards the same actor/data preconditions.
    unsafe {
        submit_ask_with_reply_channel(ch, AskReplyChannelFailureCleanup::KeepCreatorRef, |ch| {
            actor_send_result_internal_reply(actor, msg_type, data, size, ch.cast())
        })
    }
}

/// Perform a blocking ask against an actor identified by PID.
///
/// Looks up the actor in `LIVE_ACTORS`, packs a reply channel into the
/// message, and waits for the reply. Returns the reply pointer and writes
/// the reply size to `*out_size`.
///
/// Returns null if the actor is not found locally or the send fails.
///
/// # Safety
///
/// - `data` must point to at least `size` readable bytes, or be null when
///   `size` is 0.
/// - `out_size` must be a valid, non-null writable pointer.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) unsafe fn hew_actor_ask_by_id(
    actor_id: u64,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
) -> *mut c_void {
    // SAFETY: same contract as this fn; `expected_serial: None` selects the
    // plain by-ID pin.
    unsafe { actor_ask_by_id_inner(actor_id, None, msg_type, data, size) }
}

/// Identity-verified variant of [`hew_actor_ask_by_id`]: pins the resolved
/// incarnation by `actor_id` AND requires its full [`HewActor::spawn_serial`]
/// to equal `expected_serial` before enqueuing, so a masked-`id` alias (a fresh
/// actor reusing a retired incarnation's low-48-bit `id` after 2^48
/// allocations) fails closed to `AskError::ActorStopped` instead of delivering
/// to the wrong actor. Used by the blocking owner-scoped role ask, whose phase
/// one resolves the serial under `children_lock`.
///
/// # Safety
///
/// Same as [`hew_actor_ask_by_id`].
#[cfg(not(target_arch = "wasm32"))]
pub(crate) unsafe fn hew_actor_ask_by_identity(
    actor_id: u64,
    expected_serial: u64,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
) -> *mut c_void {
    // SAFETY: same contract as this fn; the serial gate runs under the pin.
    unsafe { actor_ask_by_id_inner(actor_id, Some(expected_serial), msg_type, data, size) }
}

/// Shared body for the by-ID blocking ask. When `expected_serial` is `Some`,
/// the send phase pins through [`live_actors::with_actor_send_by_identity`] so
/// an aliased `id` (serial mismatch) refuses closed without enqueuing;
/// otherwise it uses the plain by-ID pin.
///
/// # Safety
///
/// `data` must point to at least `size` readable bytes, or be null when `size`
/// is 0.
#[cfg(not(target_arch = "wasm32"))]
unsafe fn actor_ask_by_id_inner(
    actor_id: u64,
    expected_serial: Option<u64>,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
) -> *mut c_void {
    let ch = reply_channel::hew_reply_channel_new();

    // SAFETY: `ch` is a live reply channel owned by this ask call and the
    // closure preserves the same actor-ID/data preconditions.
    let send_result_code = unsafe {
        submit_ask_with_reply_channel(ch, AskReplyChannelFailureCleanup::FreeCreatorRef, |ch| {
            // Use the liveness-pin protocol (same as hew_actor_send_by_id):
            // under LIVE_ACTORS, validate + pin; release lock; run the send;
            // SendPinGuard decrements on return.  The untrack-first free path
            // cannot finalize while this pin is held.  With `expected_serial`
            // set, the pin additionally verifies the incarnation's full serial
            // so an aliased `id` fails closed here instead of at a later deref.
            let dispatch = |actor: *mut HewActor| {
                // SAFETY: `actor` is pinned live by the by-ID pin; allocation
                // valid for the closure.  `ch` is a live reply channel retained
                // above.  Same data/size preconditions as hew_actor_ask.
                actor_send_result_internal_reply(actor, msg_type, data, size, ch.cast())
            };
            match expected_serial {
                Some(serial) => live_actors::with_actor_send_by_identity(actor_id, serial, |pin| {
                    dispatch(pin.as_ptr())
                }),
                None => live_actors::with_actor_send_by_id(actor_id, dispatch),
            }
            .unwrap_or(HewError::ErrActorStopped as i32)
        })
    };

    if send_result_code != HewError::Ok as i32 {
        return actor_ask_null(send_err_to_ask_err(send_result_code));
    }

    let mut reply_size: usize = 0;
    // SAFETY: ch is valid and single-reader; reply_size is a valid stack pointer.
    let result = unsafe { reply_channel::hew_reply_wait_with_size(ch, &raw mut reply_size) };

    if result.is_null() {
        // SAFETY: ch is still live — we hold the caller-side reference.
        let is_orphaned = unsafe { (*ch).orphaned.load(Ordering::Acquire) };
        // SAFETY: ch was created by hew_reply_channel_new.
        unsafe { reply_channel::hew_reply_channel_free(ch) };
        if is_orphaned {
            return actor_ask_null(AskError::OrphanedAsk);
        }
        actor_ask_clear();
    } else {
        // SAFETY: ch was created by hew_reply_channel_new.
        unsafe { reply_channel::hew_reply_channel_free(ch) };
        actor_ask_clear();
    }

    result
}

/// Send a synchronous request through a stable local actor identity.
///
/// # Safety
/// `data` must be readable for `size` bytes, or null when `size` is zero.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_local_pid_ask(
    token: crate::lifetime::local_handles::HewLocalPidId,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
) -> *mut c_void {
    let Some(actor_id) = crate::lifetime::local_handles::resolve_current_actor(token) else {
        return actor_ask_null(AskError::OrphanedAsk);
    };
    #[cfg(not(target_arch = "wasm32"))]
    // SAFETY: the resolved ActorId is pinned by the by-ID ask send phase.
    return unsafe { hew_actor_ask_by_id(actor_id, msg_type, data, size) };
}

/// Submit an ask with a caller-owned reply channel through a stable identity.
///
/// # Safety
/// `data` and `ch` must satisfy [`hew_actor_ask_with_channel`]'s contract.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_local_pid_ask_with_channel(
    token: crate::lifetime::local_handles::HewLocalPidId,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
    ch: *mut c_void,
) -> i32 {
    let Some(actor_id) = crate::lifetime::local_handles::resolve_current_actor(token) else {
        record_ask_error(AskError::ActorStopped);
        return HewError::ErrActorStopped as i32;
    };
    live_actors::with_actor_send_by_id(actor_id, |actor| {
        #[cfg(not(target_arch = "wasm32"))]
        // SAFETY: actor is pinned; channel and data follow this function's contract.
        return unsafe {
            submit_ask_with_reply_channel(
                ch.cast(),
                AskReplyChannelFailureCleanup::KeepCreatorRef,
                |ch| actor_send_result_internal_reply(actor, msg_type, data, size, ch.cast()),
            )
        };
    })
    .unwrap_or_else(|| {
        record_ask_error(AskError::ActorStopped);
        HewError::ErrActorStopped as i32
    })
}

// ── Receive-gen stream-producer sink registry ─────────────────────────────

/// Register the `receive gen fn` pump's own producer sink with its actor
/// (decision 7). Called once in the pump's PROLOGUE, before its first
/// `GeneratorNext`, so a terminal teardown reaching this actor while the
/// pump is still live (crashed, or parked on backpressure) can find and
/// fault-close the sink instead of leaving the consumer to hang.
///
/// # Safety
///
/// `actor` must be null or a live `HewActor` pointer (the pump's own
/// dispatching actor, `hew_actor_self()`). `sink` must be a live
/// channel-backed `HewSink` pointer (the producer's `Sink<T>` half); it is
/// NOT consumed here — the pump's own scope-exit path still owns freeing it
/// (via [`hew_actor_gen_sink_complete`] on a clean exit, or the fault-close
/// teardown walk on an abandoned one).
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_gen_sink_register(
    actor: *mut HewActor,
    sink: *mut crate::stream::HewSink,
) {
    if actor.is_null() {
        return;
    }
    // SAFETY: caller guarantees actor is valid.
    unsafe { (*actor).gen_sink.store(sink.cast(), Ordering::Release) };
}

/// Clean close + deregister: the pump's `None` (generator-exhausted) exit
/// (decision 7). Replaces the bare `hew_sink_close` call the pump used
/// earlier — deregisters first via a CAS on the shared slot, so a terminal
/// teardown racing this exit cannot ALSO fault-close the sink this call is
/// about to free. This call frees `sink` itself ONLY if its own CAS won
/// that race; if a concurrent [`fault_close_registered_gen_sink`] already
/// swapped the slot to null first, this call has lost ownership and
/// returns without touching `sink` again (the fault path already closed
/// and freed it) — mirroring that function's own idempotent
/// swap-to-null-then-release pattern from the other side of the race.
///
/// # Safety
///
/// `actor` must be null or a live `HewActor` pointer (the pump's own
/// actor). `sink` must be the same live pointer
/// [`hew_actor_gen_sink_register`] recorded; ownership of `sink` transfers
/// to this call exactly like `hew_sink_close` UNLESS a concurrent
/// fault-close won the race first, in which case `sink` is already freed
/// and must not be touched by the caller either way — do not use `sink`
/// after calling this function regardless of which side won.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_gen_sink_complete(
    actor: *mut HewActor,
    sink: *mut crate::stream::HewSink,
) {
    if !actor.is_null() {
        // Deregister only if the slot still holds THIS sink. This is NOT
        // defensive: a concurrent terminal teardown
        // (`fault_close_registered_gen_sink`, called from `hew_actor_trap`
        // or the parked-activation reclaim path) can race this call on the
        // exact same `AtomicPtr` slot. Both sides perform a single atomic
        // RMW, so exactly one of them observes the slot non-null and "wins"
        // the release; the loser must treat `sink` as already freed by the
        // winner and must NOT touch it again. Only close/free `sink` here
        // when this call's own CAS won the race — i.e. this call still
        // owned the registered pointer at the moment it ran.
        // SAFETY: caller guarantees actor is valid.
        let won = unsafe {
            (*actor)
                .gen_sink
                .compare_exchange(
                    sink.cast(),
                    ptr::null_mut(),
                    Ordering::AcqRel,
                    Ordering::Acquire,
                )
                .is_ok()
        };
        if !won {
            // The fault-close teardown path already won this race and has
            // already closed/freed `sink` via `fault_close_registered_sink`.
            // Closing it again here would be a double-free of the same
            // `Box<HewSink>` allocation. Mirror
            // `fault_close_registered_gen_sink`'s own idempotent
            // swap-to-null pattern: the loser is a no-op.
            return;
        }
    }
    // SAFETY: sink is the live, not-yet-freed pointer per the fn contract;
    // this call either isn't actor-registered (actor is null, so no
    // teardown race is possible) or just won the CAS above, so this is
    // still the sink's single release on the clean-exit path.
    unsafe { crate::stream::hew_sink_close(sink) };
}

/// Fault-close this actor's still-registered gen-sink, if any (decision
/// 7), so a consumer awaiting the stream observes the fault on every terminal
/// cause (`death-signal-fires-on-every-terminal-cause`), never a silent hang.
///
/// A parked consumer in `ChannelCore::blocking_recv` (or a suspended `recv`
/// bind edge) is woken by exactly three things: a send, a clean close, or this
/// fault. A producer actor that will never run again publishes none of the
/// first two, so this call is the ONLY thing standing between a dead producer
/// and a permanently parked consumer. It is therefore called from every route
/// that makes the producer unable to produce, not just the ones that happen to
/// reclaim a parked frame:
///
/// - [`hew_actor_trap`] — the crash / explicit-terminal path.
/// - `scheduler::settle_after_activation`'s two `→ Stopped` transitions — the
///   graceful-stop terminals, including the one the out-of-band stop cancel of
///   a parked activation funnels into. This publishes at the instant the
///   producer stops, ahead of any free.
/// - `hew_actor_free_inner` and `free_actor_resources` — the
///   backstop for the abandonment routes that never settle an activation at all
///   (shutdown sweep, quiesced drain, supervisor child teardown, leak).
///
/// Idempotent: swaps the slot to null before touching the sink, so a second
/// call (or a race between callers) sees an already-null slot and is a no-op —
/// the sink is fault-closed exactly once.
pub(crate) fn fault_close_registered_gen_sink(a: &HewActor) {
    let raw = a.gen_sink.swap(ptr::null_mut(), Ordering::AcqRel);
    if raw.is_null() {
        return;
    }
    // SAFETY: `raw` was registered by `hew_actor_gen_sink_register` as a live
    // `HewSink` pointer and has not been consumed — the swap-to-null above is
    // the single point that can observe it non-null, so no other caller can
    // race this release.
    #[cfg(not(target_arch = "wasm32"))]
    // SAFETY: see above.
    unsafe {
        crate::stream::fault_close_registered_sink(raw.cast(), a.id);
    };
    #[cfg(target_arch = "wasm32")]
    {
        let _ = raw;
    }
}

// ── Trap / Error ────────────────────────────────────────────────────────

/// Publish a DOWN notification for a dead actor.
///
/// `link` and `monitor` are a manifest reject on wasm32 (`link-monitor`), so no
/// program this target admits can hold either: there is no watcher to notify
/// and no graph entry to reclaim.
pub(crate) fn notify_monitors_on_death(actor_id: u64, state: i32, reason: u32) {
    #[cfg(not(target_arch = "wasm32"))]
    crate::monitor::notify_monitors_on_death(actor_id, state, reason);
    #[cfg(target_arch = "wasm32")]
    {
        let _ = (actor_id, state, reason);
    }
}

/// Trap (panic) an actor: store an error code, close the mailbox, and
/// transition to a terminal state. If the actor has a supervisor, notify it.
///
/// # Safety
///
/// `actor` must be a valid pointer returned by a spawn function.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_trap(actor: *mut HewActor, error_code: i32) {
    // A parked checked turn still owns its coroutine frame and state borrow.
    // Drain it through the scheduler before publishing a terminal crash, so a
    // supervisor cannot reclaim an incarnation with a live invocation.
    if error_code != 0 {
        // SAFETY: the public trap contract keeps `actor` valid throughout this
        // terminal request.
        if unsafe { defer_external_trap_until_checked_drain(actor, error_code) } {
            return;
        }
    }
    // SAFETY: forwarded public contract. An external trap may race a live
    // activation, so it drains only when no scheduler frame owns the mailbox
    // consumer. Otherwise that frame observes the terminal state and performs
    // the deferred drain before releasing `dispatch_active`.
    unsafe { hew_actor_trap_inner(actor, error_code, TrapMailboxReclaim::IfQuiescent) };
}

/// Request a terminal crash after a parked checked turn has cancelled itself.
///
/// Returns true only after recording the first external crash code and asking
/// the existing cooperative-stop path to wake the parked continuation.
///
/// # Safety
///
/// `actor` must be valid for the duration of this call.
#[cfg(not(target_arch = "wasm32"))]
unsafe fn defer_external_trap_until_checked_drain(actor: *mut HewActor, error_code: i32) -> bool {
    if actor.is_null() {
        return false;
    }
    // SAFETY: caller guarantees a live actor allocation.
    let a = unsafe { &*actor };
    if a.checked_invocation.load(Ordering::Acquire).is_null()
        || a.actor_state.load(Ordering::Acquire) != HewActorState::Suspended as i32
    {
        return false;
    }
    // The first terminal request owns the diagnostic. A second caller must not
    // overwrite the cause whose cancellation it is joining.
    if a.pending_external_trap_code
        .compare_exchange(0, error_code, Ordering::AcqRel, Ordering::Acquire)
        .is_err()
    {
        return true;
    }
    // SAFETY: the pending code is published before this call can wake the
    // parked continuation. `hew_actor_stop` latches cancellation and performs
    // the Suspended -> Runnable hand-off when it still owns that transition.
    unsafe { hew_actor_stop(actor) };
    true
}

/// Consume a deferred external terminal request after its checked turn drains.
pub(crate) fn take_deferred_external_trap(a: &HewActor) -> Option<i32> {
    let code = a.pending_external_trap_code.swap(0, Ordering::AcqRel);
    (code != 0).then_some(code)
}

#[derive(Clone, Copy)]
pub(crate) enum TrapMailboxReclaim {
    /// The caller is the scheduler frame that owns the mailbox consumer.
    OwnedActivation,
    /// Drain only if no scheduler frame owns the mailbox consumer.
    IfQuiescent,
    /// Exact pre-fix counterfactual used by the ownership witness.
    #[cfg(test)]
    OmitForTest,
}

/// Trap publication from the scheduler frame that owns this actor's active
/// mailbox consumer.
///
/// # Safety
///
/// `actor` must be the live actor whose activation the calling scheduler frame
/// owns.
pub(crate) unsafe fn hew_actor_trap_from_activation(actor: *mut HewActor, error_code: i32) {
    // SAFETY: forwarded contract; the caller supplies the sole-consumer proof.
    unsafe { hew_actor_trap_inner(actor, error_code, TrapMailboxReclaim::OwnedActivation) };
}

/// Put a crash on the exit-status authority BEFORE anything can observe it.
///
/// Called from `hew_actor_trap_inner` the moment that thread wins the terminal
/// CAS, and before any step that can release another thread — the mailbox close
/// wakes blocked senders, and the queued-terminal reclaim retires pending asks,
/// which is what completes a waiter's `await`. A thread woken there runs on
/// immediately, and if the next thing it does is `exit(0)` it reads the exit
/// status.
///
/// Recording after those wake-ups made a program's exit status a RACE between
/// the crashing thread and the thread it had just woken. Linux and macOS
/// happened to win it; Windows lost it, and `exit(0)` reported success over the
/// very crash that had already woken `main`.
///
/// An UNSUPERVISED crash is unrecovered by construction — no authority exists to
/// rule on it. A SUPERVISED one gets its record opened here, and the caller
/// carries the id to the supervisor notification. Both are therefore accounted
/// for before the crash is observable at all.
fn publish_crash_fault_record(
    terminal: i32,
    error_code: i32,
    supervisor: *mut c_void,
    supervisor_child_index: i32,
) -> crate::exit_status::FaultRecord {
    if terminal != HewActorState::Crashed as i32 {
        return crate::exit_status::FaultRecord::NONE;
    }
    if supervisor.is_null() {
        // A cancellation reaching this unsupervised actor while a requested
        // shutdown (SIGTERM/SIGINT or a program-exit drain) is in flight is
        // expected termination, not a fault: nothing supervises it to rule
        // on the crash, but shutdown itself is the authority that asked for
        // it, so it must not fail the process the way a genuine unrecovered
        // crash does. Any other error_code, including a cancellation outside
        // shutdown, is still unrecovered.
        let cancelled_by_shutdown = error_code == crate::fault::HEW_FAULT_CANCELLED
            && crate::shutdown::hew_is_shutting_down() != 0;
        if !cancelled_by_shutdown {
            crate::exit_status::record_unrecovered_actor_fault();
        }
        return crate::exit_status::FaultRecord::NONE;
    }
    let record = crate::exit_status::open_supervised_fault();
    // Attribute the record to the declared role this actor occupies and to
    // every ancestor role above it, so an `await_restart` on any of them treats
    // the crash as pending until a ruling settles it. Attribution happens here,
    // with the record, and therefore also before the first wake.
    #[cfg(not(target_arch = "wasm32"))]
    if let Ok(child_index) = u32::try_from(supervisor_child_index) {
        // SAFETY: the back-pointer was set by `hew_supervisor_add_child` and
        // the supervisor outlives the child that names it.
        let roles = unsafe { crate::supervisor::child_role_chain(supervisor.cast(), child_index) };
        crate::exit_status::attribute_supervised_fault(record, roles);
    }
    record
}

/// Retained terminal observation, published only after native state cleanup.
#[derive(Debug)]
pub(crate) struct TerminalNotification {
    actor_id: u64,
    terminal: i32,
    error_code: i32,
    supervisor: *mut c_void,
    supervisor_child_index: i32,
    fault_record: crate::exit_status::FaultRecord,
}

// SAFETY: the terminal actor retains its supervisor until cleanup completes;
// only the unique terminal owner transfers this notice to its scheduler turn.
unsafe impl Send for TerminalNotification {}

impl TerminalNotification {
    /// Publish after cleanup while the actor still retains its supervisor.
    pub(crate) unsafe fn publish_terminal_notification(self) {
        let Self {
            actor_id,
            terminal,
            error_code,
            supervisor,
            supervisor_child_index,
            fault_record,
        } = self;
        #[cfg(target_arch = "wasm32")]
        let _ = (supervisor, supervisor_child_index, fault_record);
        let lifecycle_event = if terminal == HewActorState::Crashed as i32 {
            crate::tracing::SPAN_CRASH
        } else {
            crate::tracing::SPAN_STOP
        };
        crate::tracing::hew_trace_lifecycle(actor_id, lifecycle_event);

        // Test-only crash ledger (cross-node link probe): record the TERMINAL STATE so a
        // two-process link fixture can confirm a LOCAL linked actor actually crashed
        // (terminal Crashed == 5) after a cross-node link-down, surviving the actor's
        // free. Gated by HEW_LINK_PROBE so production pays nothing.
        #[cfg(not(target_arch = "wasm32"))]
        crate::link::record_link_probe_terminal(actor_id, terminal);

        // Propagate exit to linked actors and notify monitors.
        // Do this BEFORE notifying supervisor to ensure proper ordering.
        run_crash_teardown_order_hook(HEW_ACTOR_CRASH_TEARDOWN_BEFORE_EXIT_PROPAGATION);
        #[cfg(not(target_arch = "wasm32"))]
        crate::link::propagate_exit_to_links(actor_id, error_code);
        run_crash_teardown_order_hook(HEW_ACTOR_CRASH_TEARDOWN_AFTER_EXIT_PROPAGATION);
        let crash_kind = if terminal == HewActorState::Crashed as i32 {
            crate::internal::types::CrashKind::tag_from_error_code(error_code).cast_unsigned()
        } else {
            0
        };
        notify_monitors_on_death(actor_id, terminal, crash_kind);

        // Wake any actor group condvars waiting on this actor.
        crate::actor_group::notify_actor_death(actor_id);

        // Notify supervisor if one exists. An actor whose supervisor index was
        // never assigned (the `-1` initial value) is not a supervised child, so
        // there is nothing to notify — the `u32` parameter makes that case a
        // conversion failure here rather than a negative index the supervisor has
        // to reinterpret.
        #[cfg(not(target_arch = "wasm32"))]
        if !supervisor.is_null() {
            if let Ok(child_index) = u32::try_from(supervisor_child_index) {
                // Hand the record opened at the terminal CAS to the supervisor on
                // the event. It was opened before the first wake rather than here
                // so no thread can observe this crash while it is unaccounted for;
                // by the time the notification is queued the fault already counts
                // as failing. Until the ruling arrives it stays that way: a
                // supervisor that is already stopping, a closed mailbox, or an
                // immediate `hew_sched_shutdown` joining the workers before the
                // queued decision runs all leave it open rather than silently
                // successful.
                let record = fault_record;
                // SAFETY: supervisor back-pointer was set by hew_supervisor_add_child.
                let notified = unsafe {
                    crate::supervisor::hew_supervisor_notify_child_actor_event(
                        supervisor.cast(),
                        child_index,
                        actor_id,
                        terminal,
                        error_code,
                        record.as_raw(),
                    )
                };
                if !notified {
                    // The supervisor never received the event — a null supervisor
                    // actor, or a mailbox that refused it. The record reached no
                    // authority, so it is settled here rather than left to time out
                    // in the shutdown quiesce.
                    crate::exit_status::settle_supervised_fault(
                        record,
                        crate::exit_status::FaultRuling::Unrecovered,
                    );
                }
            } else if terminal == HewActorState::Crashed as i32 {
                // A supervisor back-pointer with no usable child index names no
                // roster entry, so no supervisor can ever rule on this crash. That
                // is the same "no recovery authority" case as an unsupervised
                // crash: settle the record opened above rather than leave it open
                // forever for a supervisor that can never be reached.
                crate::exit_status::settle_supervised_fault(
                    fault_record,
                    crate::exit_status::FaultRuling::Unrecovered,
                );
            }
        }
    }
}

/// Implementation seam for [`hew_actor_trap`].
///
/// Tests use `OmitForTest` to execute the precise pre-fix counterfactual: all
/// crash publication remains intact, but the one queued-mailbox reclaim edge
/// is omitted.
///
/// # Safety
///
/// Same contract as [`hew_actor_trap`].
pub(crate) unsafe fn hew_actor_trap_inner(
    actor: *mut HewActor,
    error_code: i32,
    mailbox_reclaim: TrapMailboxReclaim,
) {
    cabi_guard!(actor.is_null());
    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };

    // A parked checked turn may already have accepted an external trap and be
    // racing a separate terminal edge while it drains. Preserve that first
    // cause if this caller wins publication before the scheduler consumes it.
    let error_code = match a.pending_external_trap_code.load(Ordering::Acquire) {
        0 => error_code,
        deferred => deferred,
    };

    // Choose terminal state: Crashed if error_code != 0, Stopped otherwise.
    //
    // `Stopped` here means "reached its own clean termination sequence"
    // (`finish_native_terminal` additionally requires `terminate_finished`
    // for it), which an abandoned/unwound frame never runs — so a
    // shutdown-cancelled `accept()` still takes the mechanical `Crashed`
    // path below. What changes is only whether that terminal counts as an
    // UNRECOVERED FAULT for the exit-status authority: `publish_crash_fault_record`
    // exempts a cancellation reaching here while a requested shutdown
    // (SIGTERM/SIGINT or a program-exit drain) is in flight, since that is
    // expected termination, not a fault the caller failed to handle.
    let terminal = if error_code != 0 {
        HewActorState::Crashed as i32
    } else {
        HewActorState::Stopped as i32
    };

    // Read supervisor fields before setting terminal state to avoid a race
    // where the supervisor on another thread frees the actor between the
    // state transition and the supervisor field reads.
    let supervisor = a.supervisor;
    let supervisor_child_index = a.supervisor_child_index;
    let actor_id = a.id;

    // Claim the terminal transition BEFORE closing the mailbox.
    //
    // Ordering matters. Closing the mailbox first opens a lost-crash-notify
    // race: a worker concurrently dispatching this actor reaches its
    // post-dispatch settle, observes the now-closed mailbox, and drives the
    // actor IDLE -> STOPPED via the "mailbox closed while draining" self-stop
    // (scheduler `activate_actor`). That self-stop path is for graceful stop
    // and does NOT notify the supervisor. If it wins the terminal CAS, this
    // trap then reads STOPPED, treats the actor as already-terminal, bails out,
    // and the child-crashed notification is never delivered — any observer
    // blocked on the supervisor's restart counter blocks to its full timeout
    // ceiling.
    //
    // Taking the terminal CAS first makes the trap authoritative: once the
    // actor is CRASHED/STOPPED, the worker's `Running -> Idle` / `Idle ->
    // Stopped` settle CAS fails, so it cannot self-stop the actor out from
    // under us, and the notify below always runs. The mailbox is closed
    // immediately after to reject new sends and wake blocked senders — by then
    // the actor is already terminal, so every sender's `Idle -> Runnable` CAS
    // fails regardless.
    //
    // Publish the crash code BEFORE claiming the terminal state. Every reader
    // of an exit reason loads `actor_state` first and only then `error_code`:
    // `link::terminal_exit_reason` for a link registered after the crash,
    // `monitor`'s terminal-reason lookup for a late monitor, and
    // `hew_actor_await` for a blocked caller. Those are two independent
    // atomics, so the only thing that makes that pair sound is this store
    // preceding the CAS below — the CAS's release then publishes the code to
    // every acquire-observer of the terminal state. Storing it after the CAS
    // left a window in which the actor was already CRASHED while `error_code`
    // still held its `0` default, so a reader that won the race reported "no
    // error" for an actor whose EXIT propagation carried the real code.
    //
    // A dispatch that trapped through `hew_panic` has already stamped its code
    // here (`trap_code::stamp_current_actor_error_code`), so a non-zero
    // `error_code` on a not-yet-terminal actor is the ordinary state of a
    // crashing dispatch, not something this store introduces.
    //
    // The store sits INSIDE the loop, after the already-terminal check and
    // before the exchange, rather than once ahead of the whole loop. An actor
    // that is already Stopped or Crashed by the time this trap runs (a
    // send-failure trap on a monitor whose target's mailbox is already
    // closed, for example) takes the early `return` above the store: the
    // recorded reason from whichever trap actually won the terminal race is
    // left untouched. Storing ahead of that check would overwrite a settled
    // actor's reason with this trap's code on every call, turning a clean
    // Stopped exit into a spurious crash reason or zeroing a real crash code.
    //
    // WHY this is a store and not a claim: publishing ahead of the CAS means a
    // trap that goes on to LOSE the CAS has also stored its code, so two traps
    // racing the same actor with different codes leave whichever store landed
    // last rather than the winner's. Only one trap per activation is
    // reachable from dispatch; the external callers (`monitor`'s failed-DOWN
    // trap) all pass `HEW_TRAP_ACTOR_SEND_FAILED`, so the codes agree in
    // practice for the racing-writers case. The already-terminal case above
    // is common (the monitor site hits it whenever the target already
    // stopped or crashed) and is handled by the early return, not by this
    // store's ordering.
    // WHEN this stops being good enough: as soon as two distinct non-zero
    // codes can race the CAS itself — a second external trap site with its
    // own code. WHAT the real fix is: publish state and code in one atomic
    // (pack the terminal state and the code into a single `AtomicI64` claimed
    // by one compare_exchange), which makes the winner of the claim the only
    // writer.
    loop {
        let current = a.actor_state.load(Ordering::Acquire);
        if current == HewActorState::Stopped as i32 || current == HewActorState::Crashed as i32 {
            return;
        }
        a.error_code.store(error_code, Ordering::Release);
        if a.actor_state
            .compare_exchange(current, terminal, Ordering::AcqRel, Ordering::Acquire)
            .is_ok()
        {
            // This terminal edge has consumed any deferred request. The
            // scheduler may now only observe its already-published terminal.
            a.pending_external_trap_code.store(0, Ordering::Release);
            break;
        }
    }

    if terminal == HewActorState::Crashed as i32 {
        crate::fault::note_actor_crash(actor_id);
    }

    let fault_record =
        publish_crash_fault_record(terminal, error_code, supervisor, supervisor_child_index);
    let notice = TerminalNotification {
        actor_id,
        terminal,
        error_code,
        supervisor,
        supervisor_child_index,
        fault_record,
    };
    let notice = if let Some(completion) = &a.native_completion {
        completion.defer_terminal_notification(notice);
        None
    } else {
        Some(notice)
    };
    run_crash_teardown_order_hook(HEW_ACTOR_CRASH_TEARDOWN_BEFORE_FIRST_WAKE);

    // This actor just became terminal — the crash/trap path. Any
    // `receive gen fn` pump this actor was running (or had parked) will
    // never produce another value; fault-close its still-registered sink so
    // a consumer awaiting the stream observes the fault rather than hanging.
    // A no-op if nothing is registered (no pump ever ran, or it already
    // deregistered via a clean exit before this trap).
    fault_close_registered_gen_sink(a);

    // Close mailbox to reject new messages and wake any blocked senders. Safe
    // after the terminal CAS: sends are already rejected by the terminal state.
    let mb = a.mailbox.cast::<HewMailbox>();
    if !mb.is_null() {
        // SAFETY: mailbox is valid for actor's lifetime.
        unsafe { mailbox::mailbox_close(mb) };
    }

    // This is the last crash site that still owns a live actor and mailbox.
    // Drain BEFORE exit propagation or supervisor notification: either can
    // hand the terminal incarnation to another thread for replacement/free.
    // Returning to `scheduler::activate_actor` to reclaim would therefore read
    // through ownership that this function has already transferred.
    match mailbox_reclaim {
        TrapMailboxReclaim::OwnedActivation => {
            // SAFETY: the calling activation owns the mailbox consumer.
            unsafe { mailbox::mailbox_reclaim_queued_terminal(mb) };
        }
        TrapMailboxReclaim::IfQuiescent => {
            // Test activation ownership under the same terminal-reclaim lock as
            // ActivationOwnership's terminal-state test, final drain, and
            // Release-clear. Therefore either this path sees quiescence and
            // drains, or that activation must subsequently observe the terminal
            // publication and drain before it clears ownership.
            //
            // SAFETY: a true predicate proves there is no active scheduler
            // consumer. The actor remains live through the notification tail.
            unsafe {
                mailbox::mailbox_reclaim_queued_terminal_if(mb, || {
                    !a.dispatch_active.load(Ordering::Acquire)
                });
            }
        }
        #[cfg(test)]
        TrapMailboxReclaim::OmitForTest => {}
    }

    #[cfg(not(target_arch = "wasm32"))]
    if terminal == HewActorState::Crashed as i32 {
        let scope = crate::task_scope::current_task_scope();
        if !scope.is_null() {
            // SAFETY: the task-scope lane is installed only while the scope is live.
            unsafe { crate::task_scope::checked::hew_checked_scope_cancel(scope) };
        }
    }
    if matches!(mailbox_reclaim, TrapMailboxReclaim::OwnedActivation)
        || !a.dispatch_active.load(Ordering::Acquire)
    {
        // SAFETY: the trap owns completed dispatch cleanup or observes a
        // quiescent terminal actor; the checked-frame guard retains live turns.
        unsafe { crate::actor_native::finish_native_terminal(a) };
    }

    if let Some(notice) = notice {
        // SAFETY: legacy terminal publication retains the same supervisor
        // lifetime through this tail as the original trap activation.
        unsafe { notice.publish_terminal_notification() };
    }
}

/// Return the error code stored on an actor (0 = no error).
///
/// # Safety
///
/// `actor` must be a valid pointer to a [`HewActor`].
#[no_mangle]
pub unsafe extern "C" fn hew_actor_get_error(actor: *const HewActor) -> i32 {
    cabi_guard!(actor.is_null(), 0);
    // SAFETY: Caller guarantees `actor` is valid.
    unsafe { &*actor }.error_code.load(Ordering::Acquire)
}

// ── Self (canonical context) ────────────────────────────────────────────

/// Return the actor currently installed in the canonical execution context.
///
/// Returns null if called outside of a dispatch context.
#[no_mangle]
pub extern "C" fn hew_actor_self() -> *mut HewActor {
    let ctx = crate::execution_context::require_current_context();
    if ctx.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: a non-null canonical context points to a live context slot owned
    // by the current dispatch/scope boundary.
    unsafe { (*ctx).actor }
}

/// Crash the current actor in response to an unhandled link EXIT.
///
/// A linked actor that does NOT trap exits (`#[on(exit)]`) must CRASH when its
/// linked peer dies — the OTP fail-together semantic. The dispatch trampoline
/// routes a `HewSysMsg::Exit` with no `#[on(exit)]` hook here instead of the
/// exhaustiveness `llvm.trap` default (which is UB — it SIGILLs on Linux and
/// only accidentally produced a terminal state on macOS). This drives the SAME
/// controlled crash path a handler panic uses, with the carried reason stamped
/// on the actor, and is target-symmetric: both backends export a
/// `hew_trap_with_code(i32)` symbol with identical semantics but from different
/// modules, because the crash seam itself differs per target.
///
/// * Native (`crate::supervisor::hew_trap_with_code`): unwinds through generated
///   cleanup pads to the scheduler's recovery boundary — terminal `Crashed`, the carried reason
///   stamped, link / monitor / supervisor fan-out.
/// * wasm32 (`crate::trap_code::hew_trap_with_code`): stamps the carried reason
///   on the actor and panics; under `panic = "abort"` (the wasm32-wasip1 runtime
///   profile) the panic aborts the module — the fail-closed crash. On a host
///   build with unwinding the cooperative scheduler's `catch_unwind` activation
///   boundary observes the stamped code and transitions the actor to `Crashed`,
///   the WASM counterpart of the native caught-unwind seam. wasm32 has no `supervisor`
///   module (it is `#[cfg(not(target_arch = "wasm32"))]`), so the call must
///   target `trap_code` there or the wasm runtime archive does not compile.
///
/// `reason` is the EXIT's carried terminal reason; a zero (clean) reason is
/// coerced to the non-zero `Crashed` sentinel so an unhandled EXIT ALWAYS
/// crashes the non-trapping linked actor (a cleanly-exited linked peer still
/// takes it down, OTP-style). `hew_trap_with_code` does not return when called
/// inside dispatch; outside an actor context there is no recovery seam, where
/// the trampoline's `llvm.trap` is unreachable because a `HewSysMsg::Exit` only
/// arrives at a scheduler-driven dispatch.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub extern "C-unwind" fn hew_actor_exit_unhandled(reason: i32) {
    // Coerce a zero (clean) reason to a non-zero crash sentinel: an unhandled
    // EXIT always crashes the non-trapping linked actor (Crashed, not Stopped).
    let crash_code = if reason == 0 {
        HewActorState::Crashed as i32
    } else {
        reason
    };
    // SAFETY: routes through the per-target language-trap seam. Native stamps
    // the actor error and unwinds through the C-unwind ABI; wasm32 stamps the
    // error and panics. Both are safe to call from generated dispatch code.
    #[cfg(not(target_arch = "wasm32"))]
    unsafe {
        crate::supervisor::hew_trap_with_code(crash_code);
    }
}

/// Return the current actor's id, or -1 outside a dispatch context.
///
/// Test-introspection probe: a linker actor reports its own id so a
/// two-process link fixture can poll its terminal state after a cross-node
/// link-down. Not part of the user surface; the compiler emits no calls to this
/// symbol. Callable from `.hew` via
/// `extern "C" { fn hew_actor_self_id() -> i64; }`.
#[no_mangle]
pub extern "C" fn hew_actor_self_id() -> i64 {
    let actor = hew_actor_self();
    if actor.is_null() {
        return -1;
    }
    #[expect(
        clippy::cast_possible_wrap,
        reason = "actor ids are a monotonic counter far below i64::MAX; the Hew side reads i64"
    )]
    // SAFETY: hew_actor_self returned a non-null live actor pointer.
    unsafe {
        (*actor).id as i64
    }
}

/// Typed payload propagated through generated LLVM cleanup landing pads to the
/// scheduler's actor-dispatch `catch_unwind` boundary.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct HewPanic {
    pub(crate) code: i32,
}

/// Whether a panic-hook invocation is the typed language unwind that the
/// current runtime boundary is about to catch.
///
/// Rust invokes the process-global panic hook before `catch_unwind`. Printing
/// the default `panicked at ...` diagnostic for this internal control-flow
/// payload makes a successfully isolated actor crash look like a process abort
/// to external runners. Ordinary Rust panics and Hew panics without a proven
/// catch boundary must retain the host's prior hook unchanged.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn is_caught_hew_panic(payload: &(dyn std::any::Any + Send)) -> bool {
    payload.is::<HewPanic>() && crate::execution_context::current_context_can_unwind()
}

/// Install the process-global filter for typed Hew language unwinds.
///
/// The filter is installed once before scheduler workers start. Its decision
/// is thread-local through [`crate::execution_context::current_context_can_unwind`],
/// so unrelated threads and lifecycle/main contexts continue through the hook
/// that was installed before the runtime initialized.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn install_hew_panic_hook() {
    static INSTALL: std::sync::Once = std::sync::Once::new();
    INSTALL.call_once(|| {
        let previous = std::panic::take_hook();
        std::panic::set_hook(Box::new(move |info| {
            if is_caught_hew_panic(info.payload()) {
                return;
            }
            previous(info);
        }));
    });
}

/// Trigger a panic in the current execution context.
///
/// On a native target this Rust-unwinds through the MIR-authored LLVM cleanup
/// edges whenever a scheduler dispatch catch boundary encloses the stack, so
/// drop obligations discharge and `#[resource]` closes run. Hardware signals
/// never use this path. On wasm32 it stamps the panic sentinel and terminates
/// the module because portable WASM EH is not enabled by the shipped target.
///
/// With no catch boundary at all - a synchronous lifecycle hook running on the
/// spawning stack - process termination is the ownership boundary and the OS
/// reclaims what is left.
///
/// This function never returns.
#[no_mangle]
pub extern "C-unwind" fn hew_panic() {
    crate::cont::abort_if_crash_cleanup_finalizer_trap("Hew panic");

    {
        // The stamp publishes the crash code to the actor the scheduler is about
        // to transition to Crashed. Outside an actor it is a no-op read of a
        // null actor lane, so it never gates the unwind decision.
        let _ = crate::trap_code::stamp_current_actor_error_code(101);
        if crate::execution_context::current_context_can_unwind() {
            std::panic::panic_any(HewPanic { code: 101 });
        }
        // No catch boundary encloses this stack - a synchronous lifecycle hook
        // runs on the spawning thread with no scheduler recovery frame beneath
        // it. Starting a foreign exception would ask the platform unwinder to
        // cross a frame that cannot catch it and can terminate with an unwinder
        // initialization failure instead of Hew's documented panic status, so
        // process termination is the ownership boundary here and the OS reclaims
        // all remaining process resources. The status is `1`: an unrecovered
        // panic is a fault under the one exit rule (HEW-SPEC-2026 5.8), and the
        // panic's own text has already reached stderr.
        let _ = std::io::Write::flush(&mut std::io::stdout());
        let _ = std::io::Write::flush(&mut std::io::stderr());
        std::process::exit(1);
    }
}

/// Crash the current actor after printing a message.
///
/// # Safety
///
/// `msg` must be a valid null-terminated C string.
#[no_mangle]
pub unsafe extern "C-unwind" fn hew_panic_msg(msg: *const std::ffi::c_char) {
    if !msg.is_null() {
        // SAFETY: msg is non-null (checked above) and caller guarantees valid C string.
        let s = unsafe { std::ffi::CStr::from_ptr(msg) };
        if let Ok(text) = s.to_str() {
            if !text.is_empty() {
                eprintln!("{text}");
            }
        }
    }
    hew_panic();
}

/// Return the PID of the given actor.
///
/// # Safety
///
/// `actor` must be a valid pointer to a [`HewActor`].
#[no_mangle]
pub unsafe extern "C" fn hew_actor_pid(actor: *mut HewActor) -> u64 {
    cabi_guard!(actor.is_null(), 0);
    // SAFETY: Caller guarantees `actor` is valid.
    unsafe { &*actor }.id
}

/// Return the PID of the actor currently installed in the canonical execution
/// context.
///
/// Returns `0` if called outside of a dispatch context.
#[no_mangle]
pub extern "C" fn hew_actor_self_pid() -> u64 {
    let actor = hew_actor_self();
    if actor.is_null() {
        return 0;
    }
    // SAFETY: The canonical context only installs valid actor pointers during dispatch.
    unsafe { &*actor }.id
}

/// The running actor's own `LocalPid` token, or `0` outside an actor.
///
/// `self` in an actor body is that actor's handle, and a handle is the same
/// token `hew_actor_spawn_native` hands back — not the `HewActor*` that
/// [`hew_actor_self`] returns. Reading it from the registry keeps one authority
/// for local handle identity, so a self-handle routes exactly as a spawned one
/// does.
#[no_mangle]
pub extern "C" fn hew_actor_self_token() -> usize {
    let actor_id = hew_actor_self_pid();
    if actor_id == 0 {
        return 0;
    }
    crate::lifetime::local_handles::current_actor_token(actor_id).map_or(0, usize::from)
}

/// Self-stop: the currently running actor requests its own shutdown.
///
/// Closes the mailbox and CAS transitions from `Running` to `Stopping`.
/// The scheduler will handle the final transition to `Stopped` after
/// dispatch returns.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub extern "C" fn hew_actor_self_stop() {
    let actor = hew_actor_self();
    if actor.is_null() {
        return;
    }
    // SAFETY: The canonical context only installs valid actor pointers during dispatch.
    let a = unsafe { &*actor };

    if !a.checked_invocation.load(Ordering::Acquire).is_null() {
        // Keep the checked turn runnable until its cancellation cleanup ends.
        // SAFETY: this is the current, exclusively owned actor activation.
        unsafe {
            hew_actor_stop(actor);
            crate::actor_native::cancel_checked_turn(a);
        }
        return;
    }

    // Close the mailbox to reject new messages.
    let mb = a.mailbox.cast::<HewMailbox>();
    if !mb.is_null() {
        // SAFETY: mailbox is valid for actor's lifetime.
        unsafe { mailbox::mailbox_close(mb) };
    }

    // CAS Running → Stopping. Only the dispatching worker can be in Running
    // for this actor, so this CAS should succeed.
    let _ = a.actor_state.compare_exchange(
        HewActorState::Running as i32,
        HewActorState::Stopping as i32,
        Ordering::AcqRel,
        Ordering::Acquire,
    );
}

// ── WASM actor API ──────────────────────────────────────────────────────
// On WASM, spawn/send/ask/stop/close use the WASM mailbox and cooperative
// scheduler. These provide the same C ABI surface as native so that
// codegen-emitted calls resolve transparently.
