//! One actor's dispatch turn, shared by every target.
//!
//! Activation is actor state, not scheduler state: it takes a queue entry,
//! runs the actor's handler or resumes its parked continuation, and settles
//! the actor's lifecycle. Nothing here reads a worker, a parker or a steal
//! deque, so the native work-stealing scheduler and the single-threaded
//! wasm32 driver run the same code and differ only in where a queue entry is
//! published and who pops it ([`crate::resume`]).

use std::ffi::c_void;
use std::panic::{catch_unwind, AssertUnwindSafe};
#[cfg(any(test, debug_assertions))]
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;
use std::time::Duration;

#[cfg(test)]
use crate::lifetime::poison_safe::PoisonSafe;
#[cfg(test)]
use crate::scheduler::{ActivationPreTerminalLockHook, SchedulerQueueHandoffHook};

use crate::set_last_error;

use crate::actor::{self, HewActor, HEW_DEFAULT_REDUCTIONS, HEW_MSG_BUDGET};
use crate::execution_context::HewExecutionContext;
use crate::internal::types::{HewActorState, HewDispatchFn, HewSysDispatchFn};
use crate::lifetime::live_actors::ActorIncarnation;
use crate::mailbox::{self, hew_mailbox_has_messages, hew_msg_node_free, HewMailbox};
use crate::mailbox_header::{HewSysMsg, Origin};
use crate::resume::{sched_enqueue, sched_enqueue_owned};

/// Activations running right now, and activations finished. The native
/// scheduler reports these as its worker and task counters; the wasm32 driver
/// has one activation at a time by construction.
pub(crate) static ACTIVE_ACTIVATIONS: std::sync::atomic::AtomicU64 =
    std::sync::atomic::AtomicU64::new(0);
pub(crate) static ACTIVATIONS_COMPLETED: std::sync::atomic::AtomicU64 =
    std::sync::atomic::AtomicU64::new(0);

#[cfg(any(test, debug_assertions))]
fn dispatch_lock_seat_for_actor(
    actor: *mut HewActor,
) -> *mut crate::execution_context::HewActorStateLockState {
    if INJECT_NULL_LOCK_SEAT_ONCE.swap(false, Ordering::AcqRel) {
        std::ptr::null_mut()
    } else {
        crate::actor::actor_state_lock_seat(actor)
    }
}
#[cfg(not(any(test, debug_assertions)))]
fn dispatch_lock_seat_for_actor(
    actor: *mut HewActor,
) -> *mut crate::execution_context::HewActorStateLockState {
    crate::actor::actor_state_lock_seat(actor)
}
pub(crate) fn current_reply_channel_consumed_on(
    ctx: *mut crate::execution_context::HewExecutionContext,
) -> bool {
    if ctx.is_null() {
        return false;
    }
    // SAFETY: caller passes a context still installed (or recently installed
    // and not yet unmapped) for this worker's dispatch frame.
    unsafe { ((*ctx).flags & crate::execution_context::HEW_CTX_FLAG_REPLY_CHANNEL_CONSUMED) != 0 }
}
pub(crate) fn clear_reply_channel_on(
    ctx: *mut crate::execution_context::HewExecutionContext,
) -> *mut c_void {
    if ctx.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: caller passes a context still backed by live storage for this
    // worker's dispatch frame.
    unsafe {
        let ch = (*ctx).reply_channel;
        (*ctx).reply_channel = std::ptr::null_mut();
        (*ctx).flags &= !crate::execution_context::HEW_CTX_FLAG_REPLY_CHANNEL_CONSUMED;
        ch
    }
}
fn stash_suspended_cancel_token(
    a: &HewActor,
    token: *mut crate::cancel_token::HewCancellationToken,
) {
    if token.is_null() {
        return;
    }
    // SAFETY: the dispatch execution context holds a live token; retain it for
    // the parked continuation's resume context.
    unsafe { crate::cancel_token::hew_cancel_token_retain(token) };
    let old = a
        .suspended_cancel_token
        .swap(token.cast(), Ordering::AcqRel);
    if !old.is_null() {
        // SAFETY: the actor slot owned the old retained token.
        unsafe { crate::cancel_token::hew_cancel_token_release(old.cast()) };
    }
}
pub(crate) fn clear_suspended_cancel_token(a: &HewActor) {
    let token = a
        .suspended_cancel_token
        .swap(std::ptr::null_mut(), Ordering::AcqRel);
    if !token.is_null() {
        // SAFETY: the actor slot owned this retained token.
        unsafe { crate::cancel_token::hew_cancel_token_release(token.cast()) };
    }
}
/// Resolve the reply channel a suspending handler still owed its `ask` caller,
/// on a path that abandons the parked activation without ever resuming it.
///
/// The suspend edge MOVES the mailbox node's sender-side reference into
/// `suspended_reply_channel` (nulling the node's copy) precisely so the
/// continuation can deposit the reply on its resume edge. When there is no
/// resume — the actor was stopped, trapped, or refused its park — that
/// reference is the caller's ONLY link to a reply, and simply nulling the slot
/// strands a blocking `hew_actor_ask` at `hew_reply_wait` forever and leaks the
/// channel. Mirror the crash path (which publishes a crash fallback before
/// going terminal) by publishing the orphaned/actor-stopped failure instead, so
/// the waiter resolves to a status-bearing `Err`.
///
/// EXACTLY ONCE: the slot is taken with a `swap`, so only the caller that
/// observes the non-null pointer publishes and releases; a second visit to the
/// same actor (or a racing one) sees null and does nothing.
/// `hew_reply_channel_retire_orphaned_ask_sender_ref` consumes the sender ref
/// it publishes through — the same single release the mailbox teardown path
/// performs for an ask node that is dropped before dispatch.
pub(crate) fn retire_suspended_reply_channel(a: &HewActor) {
    // Every route that abandons the parked activation also ends the ask the
    // shutdown drain gate was tracking; drop the gate's independent reference
    // in the same motion so the pair cannot drift apart.
    release_parked_ask_channel(a);
    let ch = a
        .suspended_reply_channel
        .swap(std::ptr::null_mut(), Ordering::AcqRel);
    if !ch.is_null() {
        // SAFETY: the actor slot owned this sender-side reference, transferred
        // from the mailbox node on the suspend edge and not yet consumed.
        unsafe {
            crate::reply_channel::hew_reply_channel_retire_orphaned_ask_sender_ref(ch.cast());
        }
    }
}
/// One allocation-lifetime reference destined for a scheduler queue entry.
///
/// Queue ownership shares the actor's general lifetime-pin counter with by-ID
/// operations. This is intentional: after untracking, free waits for both
/// admitted senders and every queued/dequeued-before-claim scheduler pointer.
pub(crate) struct SchedulerQueueEntry {
    pub(crate) actor: *mut HewActor,
}
impl SchedulerQueueEntry {
    /// Retain the actor allocation before making it scheduler-reachable.
    ///
    /// # Safety
    ///
    /// `actor` must be non-null and live while this reference is acquired.
    pub(crate) unsafe fn retain(actor: *mut HewActor) -> Self {
        debug_assert!(!actor.is_null());
        // SAFETY: caller guarantees a live actor allocation.
        let pins = unsafe { &(*actor).send_pin_count };
        pins.fetch_update(Ordering::AcqRel, Ordering::Acquire, |current| {
            current.checked_add(1)
        })
        .expect("scheduler queue reference count overflow");
        Self { actor }
    }

    pub(crate) fn disarm(&mut self) {
        self.actor = std::ptr::null_mut();
    }
}
impl Drop for SchedulerQueueEntry {
    fn drop(&mut self) {
        if !self.actor.is_null() {
            // SAFETY: an armed entry owns exactly one lifetime reference.
            unsafe { release_scheduler_queue_ref(self.actor) };
        }
    }
}
/// Release one scheduler queue entry's allocation-lifetime reference.
///
/// # Safety
///
/// `actor` must still be protected by this queue reference (or by dispatch
/// ownership published before this release), and exactly one matching retain
/// must exist.
pub(crate) unsafe fn release_scheduler_queue_ref(actor: *mut HewActor) {
    // SAFETY: caller guarantees this queue entry keeps the allocation live.
    let previous = unsafe { (*actor).send_pin_count.fetch_sub(1, Ordering::Release) };
    assert!(previous > 0, "scheduler queue reference count underflow");
}
/// The SUSPEND edge: park the actor's current continuation against a readiness
/// source and return to the worker WITHOUT re-enqueuing, freeing the worker to
/// run someone else. This is the executor's third dispatch outcome.
///
/// Ordering is load-bearing (the four footguns):
/// 1. Release the per-actor state lock (FG2 / R2 P0) so senders do not
///    deadlock while the actor is suspended. The lock is held across each
///    message dispatch; a suspend that returns between acquire and release MUST
///    release it here, exactly as the panic path releases on its crash edge.
/// 2. `begin_park` publishes the `Parked` tag (FG3 phase 1), BEFORE the handle
///    is stored. It does NOT clear `pending_wake`: a reply armed inside the
///    coroutine body can fire before this edge, so the wake flag is kept
///    monotonic within a cycle and consumed once by `take_pending_wake` at each
///    drain (the lost-wake fix; see `coro_exec::begin_park`).
/// 3. `finish_park` stores the handle (FG3 phase 2).
/// 4. CAS `Running → Suspended` so wakes can find the parked actor.
/// 5. Drain the FG3 lost-wake flag: a wake that fired in the park window
///    (between phase 1 and the published `Suspended`) set `pending_wake`; if so
///    we re-enqueue ourselves (the wake's own `Suspended → Runnable` CAS lost
///    the race), so the wake is observed exactly once and never lost.
///
/// # Safety
///
/// `actor` is owned by the calling activation frame (the Running CAS is held),
/// `cont` is the live, suspended continuation handle the dispatch produced. The
/// caller MUST have released the per-actor state lock before this call (the
/// dispatch loop's `hew_actor_state_lock_release_for_context` on the dispatch
/// return edge) so a suspended actor does not hold its lock against senders
/// (FG2 / R2 P0).
pub(crate) unsafe fn park_suspended_activation(actor: *mut HewActor, cont: *mut c_void) -> bool {
    // SAFETY: caller owns `actor` via the Running CAS.
    let a = unsafe { &*actor };

    // (1) Lock release is the caller's responsibility on the real dispatch
    // path (it holds the execution context); the seed/test path holds no lock.

    // (2) FG3 phase 1: publish the park intent before storing the handle.
    if !crate::coro_exec::begin_park(a).is_ok() {
        // P1-B: begin_park refused (tag not Empty/Resuming — e.g. a stale
        // Destroyed that the quiescent re-arm has not reached, or a corrupt
        // tag). We still OWN `cont` (the dispatch produced it and it was never
        // stored), so destroy it here rather than dropping it silently — a
        // dropped handle leaks the coro frame + any frame-owned heap values.
        // `hew_cont_destroy` is null-safe and runs the single cleanup outline.
        // SAFETY: `cont` is the live, not-yet-parked, not-yet-destroyed frame
        // this activation produced; no other owner exists (park never stored
        // it, so no resume/destroy edge can race it).
        unsafe { crate::cont::hew_cont_destroy(cont) };
        return false;
    }
    // (3) FG3 phase 2: store the handle.
    // SAFETY: `cont` is a live suspended continuation per the fn contract.
    unsafe { crate::coro_exec::finish_park(a, cont) };

    // (4) Publish `Suspended` so wakes can find us. CAS from Running; if it
    // fails the actor was concurrently stopped/crashed — undo the park.
    if a.actor_state
        .compare_exchange(
            HewActorState::Running as i32,
            HewActorState::Suspended as i32,
            Ordering::AcqRel,
            Ordering::Acquire,
        )
        .is_err()
    {
        // The actor left Running underneath us (stop/crash). Destroy the parked
        // continuation exactly once rather than leaking the frame.
        // SAFETY: we still own the parked handle (no concurrent resume could
        // have started — the actor never reached Suspended).
        let _ = unsafe { crate::coro_exec::destroy_parked(a) };
        return false;
    }

    // (5) FG3: drain a wake that fired in the park window. If present, the
    // wake's own `Suspended → Runnable` CAS lost the race (we had not yet
    // published Suspended), so we re-enqueue ourselves to deliver it once.
    if crate::coro_exec::take_pending_wake(a)
        && a.actor_state
            .compare_exchange(
                HewActorState::Suspended as i32,
                HewActorState::Runnable as i32,
                Ordering::AcqRel,
                Ordering::Acquire,
            )
            .is_ok()
    {
        sched_enqueue(actor);
    }
    crate::observe::record_coroutine_suspend();
    crate::observe::hew_observe_probe_suspend(
        a.dispatch
            .map_or(std::ptr::null(), |f| f as *const std::ffi::c_void),
    );
    true
}
/// Cancel a parked continuation because an out-of-band stop was latched, and
/// finalize the activation through the ordinary `Stopping → Stopped` settle.
///
/// The caller must already have published `Stopping` (from `Running` or
/// `Suspended`) so no other worker can drive this actor, and must own the
/// activation. Destroys the parked frame exactly once (FG1 — the `… →
/// Destroyed` CAS also serialises against a concurrent resume, FG2), re-arms
/// the tag so a later park is still possible, RESOLVES the `ask` reply channel
/// the cancelled handler still owed its caller (publishing the orphaned failure
/// so the asking thread is not left blocked in `hew_reply_wait`), drops the
/// suspend-edge cancel token, resets the per-activation arena, and hands
/// off to [`settle_after_activation`], which runs the monitors + terminate
/// callback for `Stopping` — and, on that same `Stopping → Stopped` edge,
/// fault-closes any `receive gen fn` sink this cancelled pump still had
/// registered. That last part matters here: this path destroys the parked frame
/// itself, so the free path's own reclaim finds nothing left to destroy and
/// cannot be relied on to publish the fault. Cancelling a parked producer
/// without publishing it leaves its consumer parked in the channel's recv with
/// nothing left alive to wake it.
///
/// # Safety
///
/// `actor` is owned by the calling activation frame and is in `Stopping`.
pub(crate) unsafe fn cancel_parked_activation_for_stop(actor: *mut HewActor) {
    // SAFETY: caller owns `actor`.
    let a = unsafe { &*actor };
    // SAFETY: the caller owns the activation and has published `Stopping`, so
    // no concurrent resume can be driving this frame.
    let _ = unsafe { crate::coro_exec::destroy_parked(a) };
    let _ = crate::coro_exec::re_arm(a);
    // The continuation is gone: nothing will ever read the stashed reply
    // channel or the suspend-edge cancel token again. A suspending handler that
    // was serving an `ask` still OWES its caller a reply, and this slot holds
    // the only reference to that caller's channel — clearing it without
    // resolving it hangs the asking thread in `hew_reply_wait` forever. Publish
    // the orphaned failure and release the reference exactly once (see
    // [`retire_suspended_reply_channel`]), mirroring the crash path's late
    // crash-reply, before dropping the cancel token.
    retire_suspended_reply_channel(a);
    clear_suspended_cancel_token(a);
    if !a.arena.is_null() {
        // SAFETY: arena was created at spawn; the cancelled activation is over.
        unsafe { crate::arena::hew_arena_reset(a.arena) };
    }
    settle_after_activation(actor, 0);
}
/// The RESUME re-entry: drive the actor's parked continuation to its next
/// suspend (or completion) and settle the activation.
///
/// - `ResumePoll::Pending` → the continuation suspended again. Re-park: CAS
///   `Running → Suspended` (the handle stays parked, tag already back to
///   `Parked`), and if a wake fired in the meantime drain it and re-enqueue.
///   The actor is left `Suspended`, awaiting the next wake. If an out-of-band
///   stop was latched while the continuation was running, cancel the park
///   instead of re-parking — see [`cancel_parked_activation_for_stop`].
/// - `ResumePoll::Ready` → the continuation completed. Destroy it exactly once
///   (FG1) — which nulls the slot in the same critical section (FG4) — then
///   fall through to the standard idle/requeue settle so queued messages are
///   still served.
/// - refused (`None`) → the slot was null or the tag was not resumable
///   (FG2/FG4). Treat as completed (nothing live to drive) and settle to idle.
///
/// # Safety
///
/// `actor` is owned by the calling activation frame (the Running CAS is held).
#[expect(
    clippy::too_many_lines,
    reason = "keep the activation resume and cleanup sequence together"
)]
unsafe fn resume_suspended_activation(actor: *mut HewActor) {
    // SAFETY: caller owns `actor` via the Running CAS.
    let a = unsafe { &*actor };

    // W6.010 value routing: re-establish an execution context carrying the
    // handler's stashed reply channel (saved at park) BEFORE driving the resume,
    // so the resumed coroutine body's final-return `hew_reply` (via
    // `hew_get_reply_channel`) deposits the reply to the handler's caller. The
    // suspend tore down the original dispatch context; without this the body
    // would see no reply channel and the caller would hang (R1). The context is
    // a scheduler-owned stack carrier for the duration of the resume, restored
    // after (mirroring the fresh-dispatch carrier install/restore).
    let invocation = a.checked_invocation.load(Ordering::Acquire);
    // Snapshot before driving the frame: completion clears and frees invocation
    // state before this activation reports its fault.
    let message_type = if invocation.is_null() {
        0
    } else {
        // SAFETY: activation ownership keeps the parked invocation live here.
        unsafe { (*invocation.cast::<crate::coro_state::HewCoroState>()).actor_message_type }
    };
    let stashed_reply = a.suspended_reply_channel.load(Ordering::Acquire);
    let stashed_cancel_token = a.suspended_cancel_token.load(Ordering::Acquire);
    let mut resume_context = HewExecutionContext {
        actor,
        actor_id: a.id,
        parent_supervisor: a.supervisor,
        supervisor_child_index: a.supervisor_child_index,
        flags: crate::execution_context::HEW_CTX_FLAG_UNWIND_BOUNDARY_INSTALLED,
        cancel_token: stashed_cancel_token.cast(),
        task_scope: std::ptr::null_mut(),
        arena: a.arena,
        trace: crate::tracing::HewTraceContext::default(),
        partition_policy: std::ptr::null_mut(),
        prev_context: crate::execution_context::current_context(),
        lock_seat: dispatch_lock_seat_for_actor(actor),
        reply_channel: stashed_reply,
        checked_fault: crate::actor_native::CheckedActorFault::default(),
    };
    let prev_context = resume_context.prev_context;
    let installed_prev = crate::execution_context::set_current_context(&raw mut resume_context);
    debug_assert_eq!(installed_prev, prev_context);

    // Establish the actor-state escrow before resumed user code can mutate it.
    // Lexical and coroutine-frame owners are handled by LLVM cleanup edges;
    // this compatibility scope is now state-only.
    // SAFETY: this activation exclusively owns the live actor state through
    // the matching finish/recovery call.
    let crash_state_drop = if a.state_drop_borrowed.load(Ordering::Acquire) {
        None
    } else {
        match (a.state_clone_fn, a.state_drop_fn) {
            (Some(_), Some(drop)) => Some(drop),
            (None, None) => None,
            _ => {
                eprintln!("fatal: actor state has half-registered clone/drop classifier proof");
                std::process::abort();
            }
        }
    };
    // SAFETY: this activation exclusively owns the live actor state through
    // the matching finish/recovery call.
    if !unsafe {
        crate::cont::begin_dispatch_crash_cleanup(a.state, a.state_size, crash_state_drop)
    } {
        eprintln!("fatal: could not establish resumed dispatch crash cleanup");
        std::process::abort();
    }

    crate::observe::record_coroutine_resume();
    // SAFETY: the parked handle is the executor-owned frame; `resume_park`
    // enforces FG2/FG4 internally (refuses a null slot or non-Parked tag).
    let poll = catch_unwind(AssertUnwindSafe(|| {
        // SAFETY: the parked handle is owned by this activation.
        unsafe { crate::coro_exec::resume_park(a) }
    }));
    let poll = match poll {
        Ok(poll) => {
            let fault = resume_context.checked_fault.take().and_then(|fault| {
                // SAFETY: this scheduler activation owns the actor and completed fault.
                unsafe { crate::actor_native::normalize_stopped_turn(actor, fault) }
            });
            if let Some(fault) = fault {
                // SAFETY: the checked body returned through its cleanup graph;
                // this activation owns the completed frame and actor state.
                unsafe {
                    finish_failed_resume(
                        actor,
                        &raw mut resume_context,
                        message_type,
                        crate::actor_native::DispatchFailure::Checked(fault),
                    );
                }
                return;
            }
            poll
        }
        Err(payload) => {
            // SAFETY: catch_unwind proves the resumed stack is dead; this
            // activation still exclusively owns actor and resume_context.
            unsafe {
                finish_failed_resume(
                    actor,
                    &raw mut resume_context,
                    message_type,
                    crate::actor_native::DispatchFailure::Unwind(payload),
                );
            }
            return;
        }
    };

    // A normal resume must have retired every lexical dispatch token. The
    // state escrow is raw-discarded here; the actor's live state remains the
    // sole normal-path owner.
    // SAFETY: this is the matching close for the scope opened above.
    if !unsafe { crate::cont::finish_dispatch_crash_cleanup() } {
        eprintln!("fatal: resumed dispatch returned with live crash-cleanup owners");
        std::process::abort();
    }

    // Restore the prior context now that the resume step (resume + poll, and any
    // body-side reply deposit it performed) has run. On a Ready completion the
    // body normally deposited its reply, which released the transferred sender
    // reference — read the consumed flag off the resume context BEFORE the
    // restore to find out, because a completion that did NOT deposit (or a
    // refused resume, which ran nothing at all) leaves the slot holding the only
    // reference to a caller still parked in `hew_reply_wait`. Clear the stash
    // either way so a re-armed multi-await actor does not reuse a freed channel.
    // A reply may also be consumed before a Pending result: disposing of an
    // undelivered reply can suspend. That continuation no longer owns a sender
    // reference, so never reinstall the channel on its next resume.
    let resume_reply_consumed = current_reply_channel_consumed_on(&raw mut resume_context);
    let restored = crate::execution_context::set_current_context(prev_context);
    debug_assert_eq!(restored, &raw mut resume_context);
    if resume_reply_consumed {
        a.suspended_reply_channel
            .store(std::ptr::null_mut(), Ordering::Release);
        release_parked_ask_channel(a);
    }
    if matches!(poll, Some(crate::cont::ResumePoll::Ready) | None) {
        // The parked ask is over either way; drop the drain gate's reference
        // (a no-op inside `retire_suspended_reply_channel` on the else arm —
        // the swap keeps it exactly once).
        release_parked_ask_channel(a);
        if !resume_reply_consumed {
            retire_suspended_reply_channel(a);
        }
        clear_suspended_cancel_token(a);
    }

    match poll {
        Some(crate::cont::ResumePoll::Pending) => {
            // SAFETY: this frame still owns the activation.
            unsafe { settle_pending_resume(actor) };
        }
        Some(crate::cont::ResumePoll::Ready) | None => {
            // Completed (or refused: nothing live). Destroy exactly once (FG1),
            // which nulls the slot in the same critical section (FG4).
            // SAFETY: the tag is `Done` (Ready) or already terminal (None);
            // destroy_parked refuses a second teardown.
            let _ = unsafe { crate::coro_exec::destroy_parked(a) };
            // P1-B: the continuation is fully reclaimed and the slot is null
            // (FG4). Re-arm the tag `Destroyed → Empty` on this quiescent edge
            // so the SAME actor can park a NEW continuation on its next
            // `await` (multi-await). Fail-closed: `re_arm` only transitions a
            // Destroyed tag with a null slot, so a refusal here (the None
            // branch where nothing was ever parked) is harmless.
            let _ = crate::coro_exec::re_arm(a);
            // Settle: the resumed dispatch is finished. Mirror the post-loop
            // idle/requeue CAS so queued messages are served. Reset the arena
            // for the completed activation.
            if !a.arena.is_null() {
                // SAFETY: arena was created at spawn; no references survive.
                unsafe { crate::arena::hew_arena_reset(a.arena) };
            }
            settle_after_activation(actor, 0);
        }
    }
}
/// Activate an actor: CAS state to `Running`, drain messages up to budget,
/// then transition back to `Idle` or re-enqueue as `Runnable`.
#[expect(
    clippy::too_many_lines,
    reason = "actor activation state machine with multiple CAS transitions"
)]
pub(crate) fn activate_queued_actor(actor: *mut HewActor) {
    if actor.is_null() {
        return;
    }
    // SAFETY: the dequeued pointer references a live actor box. `hew_actor_free`
    // latches the actor out of `Idle` into the `Stopped` terminal state *before*
    // `untrack_actor` (after detaching the reactor and scrubbing links/monitors).
    // Once `Stopped`, every waker's `CAS Idle->Runnable` fails, so no reactor,
    // link/monitor, or direct-send wake can enqueue the actor in the window
    // between that latch and untrack+free — closing the use-after-free class. (A
    // consumer-side LIVE_ACTORS membership check was considered but rejected: it
    // cannot defeat the untrack-before-free ABA window — a reused address can
    // pass a bare membership check — and it would add a registry-lock acquisition
    // to every activation. The producer-side latch is the cheaper, complete fix.)
    let a = unsafe { &*actor };

    #[cfg(test)]
    run_scheduler_queue_handoff_hook(&ACTIVATE_PRE_CLAIM_HOOK, actor);

    // Transfer the popped queue entry's lifetime reference to activation
    // ownership before releasing it. A terminal trap/free can run at either
    // side of this handoff: before the claim the queue reference keeps the box
    // live; afterward `dispatch_active` does. A self-reenqueue can be consumed
    // before its prior owner drops, so the claim waits while state remains
    // Runnable; duplicates that have already been claimed or terminalized fail
    // closed without clearing another worker's ownership.
    let Some(activation_ownership) = ActivationOwnership::claim_dequeued(a, actor) else {
        // SAFETY: this popped entry still owns exactly one queue reference.
        unsafe { release_scheduler_queue_ref(actor) };
        return;
    };
    // SAFETY: dispatch_active is now published and keeps free from finalizing.
    unsafe { release_scheduler_queue_ref(actor) };

    // Mark the activation owned BEFORE the state load/CAS so `dispatch_active`
    // is already
    // published the instant this actor can become `Running` and thus
    // trap-stealable. A trap can only flip a `Running` actor terminal; if the
    // flag were claimed *after* a winning CAS, the actor would be `Running`
    // with the flag still `false` for the instructions in between, and a trap
    // landing there (`Running -> Crashed`) plus an async free observing
    // `Crashed && !dispatch_active` would reclaim the box out from under this
    // still-running worker — the CAS->marker-gap UAF. Held until every exit
    // below (settle / suspend-park / crash-break / fall-through) so the async
    // free path cannot reclaim the actor box while this worker is still reading
    // it. See `ActivationOwnership`.
    let state = a.actor_state.load(Ordering::Acquire);
    if state == HewActorState::Stopped as i32 || state == HewActorState::Crashed as i32 {
        drop(activation_ownership);
        return;
    }

    // CAS: RUNNABLE → RUNNING.
    if a.actor_state
        .compare_exchange(
            HewActorState::Runnable as i32,
            HewActorState::Running as i32,
            Ordering::AcqRel,
            Ordering::Acquire,
        )
        .is_err()
    {
        // Lost the CAS: this worker never owned the activation (the actor was
        // already terminal / claimed / no longer `Runnable`). Clear the flag we
        // optimistically set — no concurrent winner exists to clobber (single
        // dispatch per actor; see `ActivationOwnership`), so this restores the
        // flag to the `false` a non-owning worker must leave behind.
        drop(activation_ownership);
        return;
    }

    // The CAS won: keep ownership for the rest of this frame.
    let _activation_ownership = activation_ownership;

    if a.checked_invocation.load(Ordering::Acquire).is_null()
        || a.native_completion
            .as_ref()
            .is_some_and(|completion| completion.cleanup.is_driving())
    {
        // SAFETY: this worker owns the Running activation. Cleanup resumes
        // through the same incarnation wake protocol before another handler.
        if unsafe { crate::actor_native::cleanup::drive_actor_cleanup(a) } {
            return;
        }
    }

    // Test-only rendezvous at the exact CAS->marker-gap location: the actor is
    // now `Running` and trap-stealable. A regression test fires an external trap
    // HERE and asserts `dispatch_active` is already set (claimed before the CAS),
    // so a concurrent free refuses to reclaim the box under this worker.
    #[cfg(test)]
    run_activate_post_cas_hook(actor);

    // Resume re-entry (slice-4 executor). This activation may be a resumed
    // continuation rather than a fresh message dispatch. The discriminator —
    // carried by the suspend edge (commit 2/3) and verified before any branch
    // keys off it (R2) — is a live parked continuation: `cont_tag == Parked`
    // AND a non-null `suspended_cont` slot. `enqueue_resume` woke us
    // (Suspended → Runnable) and we just CAS'd Runnable → Running, so if the
    // discriminator is present this is a resume. Drive the continuation to its
    // next suspend (or completion) instead of draining the mailbox from
    // scratch; on completion destroy it exactly once and fall through to the
    // normal requeue/idle CAS so any queued messages are still served.
    if crate::coro_exec::has_live_parked_cont(a) {
        // OUT-OF-BAND STOP, checked BEFORE the resume so a stopping actor never
        // runs another slice of user code. The loop-top check below is on the
        // fresh-dispatch path only — this activation returns before reaching it.
        // SAFETY: mailbox pointer is valid for the lifetime of the actor.
        if unsafe { mailbox::mailbox_stop_requested(a.mailbox.cast::<HewMailbox>()) }
            // SAFETY: this worker owns the activation and invocation borrow.
            && !unsafe { crate::actor_native::cancel_checked_turn(a) }
            && a.actor_state
                .compare_exchange(
                    HewActorState::Running as i32,
                    HewActorState::Stopping as i32,
                    Ordering::AcqRel,
                    Ordering::Acquire,
                )
                .is_ok()
        {
            // SAFETY: this frame owns the activation (Running CAS held) and
            // just published `Stopping`.
            unsafe { cancel_parked_activation_for_stop(actor) };
            return;
        }
        // SAFETY: `actor` is owned by this frame (we hold the Running CAS); the
        // parked handle is the executor-owned frame the suspend edge stored.
        unsafe { resume_suspended_activation(actor) };
        // After a resume we fall through to the standard post-loop requeue/idle
        // logic below (which reads `cur_state` etc.). A still-suspended
        // continuation re-parked itself inside `resume_suspended_activation`
        // and already returned the actor to `Suspended` + broke out — handled
        // by the early return there.
        return;
    }

    let base_budget = {
        let b = a.budget.load(Ordering::Relaxed);
        if b > 0 {
            b
        } else {
            HEW_MSG_BUDGET
        }
    };
    // Scale budget by priority: high (0) = 2×, normal (1) = 1×, low (2) = ½×.
    let budget = match a.priority.load(Ordering::Relaxed) {
        actor::HEW_PRIORITY_HIGH => base_budget.saturating_mul(2),
        actor::HEW_PRIORITY_LOW => (base_budget / 2).max(1),
        _ => base_budget,
    };
    let mailbox = a.mailbox.cast::<HewMailbox>();
    // Cache arena pointer before dispatch — the actor may be freed by a
    // supervisor on another worker during crash recovery, making `a.arena`
    // a dangling read.
    let actor_arena = a.arena;

    let _activation_metrics = ActivationMetricsGuard::new();

    let mut msgs_processed: u32 = 0;
    let mut crashed = false;

    if !mailbox.is_null() {
        // Process up to `budget` messages.
        for _ in 0..budget {
            // OUT-OF-BAND STOP, checked BEFORE any receive.
            //
            // `hew_actor_stop` on a Running actor latches this flag with an
            // atomic store. It is not a message and never occupies a queue
            // slot, so — unlike the sentinel node this replaces — the request
            // cannot be lost when a `HewMsgNode` allocation fails. The former
            // producer allocated the node before latching the flag and both
            // callers discarded its `bool`, so under memory pressure a Running
            // actor silently never observed its own stop.
            //
            // Checking here also means the stop pre-empts any still-queued
            // system signal: an actor that has been told to stop is not going
            // to service another lifecycle notification first.
            // SAFETY: mailbox pointer is valid for the lifetime of the actor.
            if unsafe { mailbox::mailbox_stop_requested(mailbox) } {
                // Drive Running -> Stopping so the post-loop settle finalizes
                // the Stopping -> Stopped terminal transition
                // (monitors/terminate).
                let _ = a.actor_state.compare_exchange(
                    HewActorState::Running as i32,
                    HewActorState::Stopping as i32,
                    Ordering::AcqRel,
                    Ordering::Acquire,
                );
                break;
            }

            // SAFETY: mailbox pointer is valid for the lifetime of the actor.
            // Receive WITH provenance so a SYSTEM-queue lifecycle signal is not
            // confused with an application message that shares its value.
            let mailbox::RecvNode { node: msg, origin } =
                unsafe { mailbox::mailbox_try_recv_with_origin(mailbox) };
            if msg.is_null() {
                break;
            }

            // Route by the node's TYPED provenance. An exhaustive `match`: the
            // two channels are different types, not different values of one
            // type, so the routing cannot be silently weakened into a value
            // test the way the previous `from_sys && msg_type == -1` conjunct
            // could be by dropping one term.
            //
            // A USER-queue node is never intercepted here whatever its
            // `msg_type`: application tags are unrestricted `i32` in the public
            // C ABI (`hew_actor_send`) and are `SipHash` values in generated
            // code, so a user message carrying a `HewSysMsg` discriminant is a
            // real message that MUST reach the handler.
            let dispatch_target = match origin {
                Origin::Sys(kind) => {
                    let Some(sys_dispatch) = a.sys_dispatch else {
                        // Fail-closed: this actor registered no system entry
                        // point, so the signal has nowhere legitimate to go. It
                        // is NOT downgraded onto the user trampoline.
                        eprintln!(
                            "[scheduler] actor {} received system signal {kind:?} but \
                             registered no system dispatch; dropping",
                            a.id
                        );
                        // SAFETY: `msg` is exclusively owned by this worker.
                        unsafe { hew_msg_node_free(msg) };
                        continue;
                    };
                    DispatchTarget::Sys(sys_dispatch, kind)
                }
                Origin::User => {
                    let Some(dispatch) = a.dispatch else {
                        // SAFETY: `msg` is exclusively owned by this worker.
                        unsafe { hew_msg_node_free(msg) };
                        msgs_processed += 1;
                        continue;
                    };
                    DispatchTarget::User(dispatch)
                }
            };

            // Dispatch the message (with profiling and crash recovery).
            {
                let dispatch = dispatch_target;
                let t0 = std::time::Instant::now();
                // SAFETY: `msg` is exclusively owned by this worker.
                let msg_ref = unsafe { &*msg };
                // The call owner may withdraw until this exact dispatch claim.
                // Once claimed, losing its select only tombstones the reply;
                // it does not cancel a handler that has already started.
                // SAFETY: this node retains its sender-side reply reference.
                if !unsafe {
                    crate::reply_channel::claim_native_request_dispatch(
                        msg_ref.reply_channel.cast(),
                    )
                } {
                    // SAFETY: this worker owns the unclaimed node and its typed
                    // request; normal node retirement settles the sender debt.
                    unsafe { hew_msg_node_free(msg) };
                    msgs_processed += 1;
                    continue;
                }
                let observe_dispatch_ticket = crate::observe::observe_dispatch_begin();
                // Check for injected crash fault (testing only).
                if crate::deterministic::check_crash_fault(a.id) {
                    // Simulate a crash: use hew_actor_trap to trigger
                    // the full crash path (link propagation, monitor
                    // notification, supervisor restart).
                    crate::observe::observe_dispatch_abandon(observe_dispatch_ticket);
                    // SAFETY: `actor` is valid — we hold it via CAS.
                    // SAFETY: `msg` is exclusively owned by this worker.
                    unsafe { hew_msg_node_free(msg) };
                    let actor_id = a.id;
                    // SAFETY: this frame owns the actor activation and has
                    // already retired its in-flight message.
                    unsafe { crate::actor::hew_actor_trap_from_activation(actor, -1) };
                    // Do not read through `a` after trap notification can
                    // transfer the crashed incarnation to a supervisor.
                    crate::crash::record_injected_crash(actor_id);
                    crashed = true;
                    break;
                }

                // Check for injected delay fault (testing only).
                let delay_ms = crate::deterministic::check_delay_fault(a.id);

                // Reset reduction counter for this dispatch.
                a.reductions
                    .store(HEW_DEFAULT_REDUCTIONS, Ordering::Relaxed);

                // The reply channel travels with the dispatch carrier
                // (`execution_context.reply_channel` below) so that
                // `hew_get_reply_channel` reads sole-authoritatively from
                // the currently-installed context. Nested dispatch is
                // restored via `prev_context`.

                // Open the cooperative cleanup domain before any
                // dispatch-adjacent fail-closed guard can call Hew panic.
                // The handler's state is fully initialized at this point.
                // SAFETY: this activation exclusively owns the live actor
                // state until the matching finish/recovery call.
                let crash_state_drop = if a.state_drop_borrowed.load(Ordering::Acquire) {
                    None
                } else {
                    match (a.state_clone_fn, a.state_drop_fn) {
                        (Some(_), Some(drop)) => Some(drop),
                        (None, None) => None,
                        _ => {
                            eprintln!(
                                "fatal: actor state has half-registered clone/drop classifier proof"
                            );
                            std::process::abort();
                        }
                    }
                };
                // SAFETY: this activation exclusively owns the live actor
                // state until the matching finish/recovery call.
                if !unsafe {
                    crate::cont::begin_dispatch_crash_cleanup(
                        a.state,
                        a.state_size,
                        // The paired clone/drop classifier is also the
                        // relocation proof for byte-escrowing this state.
                        // Unsupported/interior-pointer layouts fall back
                        // to lexical cleanup only.
                        crash_state_drop,
                    )
                } {
                    eprintln!("fatal: could not establish actor dispatch crash cleanup");
                    std::process::abort();
                }

                let (dispatch_data, dispatch_size) = if msg_ref.envelope.is_null() {
                    (msg_ref.data, msg_ref.data_size)
                } else {
                    // The generated adapter takes each payload field and clears
                    // its owner bit before entering checked handler cleanup.
                    // An aliased envelope cannot satisfy that exclusive contract.
                    // SAFETY: this dequeued node owns one live envelope reference.
                    let envelope = unsafe { &*msg_ref.envelope };
                    if a.dispatch_ownership != crate::actor::HewDispatchOwnership::UniqueEnvelope
                        || envelope.refcount.load(Ordering::Acquire) != 1
                    {
                        eprintln!("fatal: actor dispatch lacks unique envelope ownership");
                        std::process::abort();
                    }
                    (envelope.payload, envelope.payload_size)
                };

                let mut execution_context = HewExecutionContext {
                    actor,
                    actor_id: a.id,
                    parent_supervisor: a.supervisor,
                    supervisor_child_index: a.supervisor_child_index,
                    flags: crate::execution_context::HEW_CTX_FLAG_UNWIND_BOUNDARY_INSTALLED,
                    cancel_token: std::ptr::null_mut(),
                    task_scope: std::ptr::null_mut(),
                    arena: a.arena,
                    trace: msg_ref.trace_context,
                    partition_policy: std::ptr::null_mut(),
                    prev_context: crate::execution_context::current_context(),
                    lock_seat: dispatch_lock_seat_for_actor(actor),
                    reply_channel: msg_ref.reply_channel,
                    checked_fault: crate::actor_native::CheckedActorFault::default(),
                };
                let prev_context = execution_context.prev_context;
                // Publish a single raw pointer to the dispatch-local context
                // and thread it through every subsequent use. Re-borrowing the
                // local with `&raw mut` is fine (SharedReadWrite tags coexist),
                // but capturing it by `&mut` — as
                // `catch_unwind(AssertUnwindSafe(|| dispatch(&raw mut
                // execution_context, …)))` previously did — issues a Unique
                // retag that invalidates the pointer already stored in the
                // thread-local context slot. The post-dispatch `hew_trace_end`
                // read of that slot was then Stacked-Borrows UB (Miri:
                // tracing.rs `(*ctx).trace`). Threading one raw pointer avoids
                // any further borrow of the local, so the published pointer
                // stays valid for the whole dispatch.
                let ec_ptr: *mut HewExecutionContext = &raw mut execution_context;
                let installed_prev = crate::execution_context::set_current_context(ec_ptr);
                debug_assert_eq!(installed_prev, prev_context);
                crate::tracing::hew_trace_begin(a.id, msg_ref.msg_type);

                // SAFETY: `execution_context` is the scheduler-owned stack
                // context for this dispatch and its lock seat came from the
                // actor's registered sidecar. The helper fails closed when
                // the seat is absent or poisoned.
                let lock_acquired =
                    unsafe { crate::actor::hew_actor_state_lock_acquire_for_context(ec_ptr) }
                        == crate::actor::HEW_ACTOR_STATE_LOCK_OK;
                if !lock_acquired {
                    // A refused state lock traps the actor: the same crash
                    // teardown, counted in flight from its first cleanup
                    // step for the same reason.
                    let _crash_publication = crate::exit_status::CrashPublication::begin();
                    // SAFETY: the handler was not entered; this activation
                    // exclusively owns the open dispatch cleanup scope.
                    let outcome =
                        unsafe { crate::cont::recover_dispatch_crash_cleanup_with_outcome(true) };
                    if outcome.state_authority_consumed {
                        // SAFETY: this activation exclusively owns actor.
                        unsafe { crate::actor::record_dispatch_state_drop_consumed(actor) };
                    }
                    // Refuse to enter the handler without the per-actor lock.
                    // SAFETY: `actor` is the actor currently owned by this
                    // scheduler frame.
                    unsafe {
                        crate::actor::hew_actor_trap_from_activation(
                            actor,
                            crate::actor::HEW_ACTOR_STATE_LOCK_ERR,
                        );
                    }
                    crate::tracing::hew_trace_end(a.id, msg_ref.msg_type);
                    // Read reply-channel state from the dispatch ctx
                    // BEFORE restoring `prev_context`, so the values come
                    // from this dispatch's frame.
                    let reply_consumed = current_reply_channel_consumed_on(ec_ptr);
                    let crash_reply = clear_reply_channel_on(ec_ptr);
                    let restored_context =
                        crate::execution_context::set_current_context(prev_context);
                    debug_assert_eq!(restored_context, ec_ptr);

                    if !reply_consumed && !crash_reply.is_null() {
                        // SAFETY: crash_reply is a valid HewReplyChannel pointer.
                        // Classified as a handler trap (the lock-refused
                        // dispatch trapped the actor) so the waiter's null
                        // reply is status-bearing.
                        unsafe {
                            crate::reply_channel::hew_reply_channel_publish_crash_fallback(
                                crash_reply.cast(),
                            );
                        }
                    }
                    // SAFETY: msg is exclusively owned by this worker.
                    unsafe {
                        (*msg).reply_channel = std::ptr::null_mut();
                        hew_msg_node_free(msg);
                    }
                    crate::observe::observe_dispatch_abandon(observe_dispatch_ticket);
                    crashed = true;
                    break;
                }

                // SAFETY: `dispatch`, `ctx`, and `a.state` are valid;
                // message fields come from a well-formed `HewMsgNode`.
                //
                // D-A.2 (R326/R327): the trampoline returns the dispatch
                // suspend outcome as a nullable continuation handle — `null`
                // for a run-to-completion handler, or the `coro.begin`
                // handle produced by live suspending-terminator codegen when
                // a handler suspended. The handle is captured here; the
                // production park edge consumes a non-null handle to park the
                // activation.
                // The ONE call site, with the entry point already chosen by
                // the node's typed origin. A system signal cannot reach the
                // user trampoline and an application message cannot reach
                // the system entry point — the discriminator is which arm
                // of `DispatchTarget` was built, not a value either callee
                // inspects.
                let dispatch_result = catch_unwind(AssertUnwindSafe(|| match dispatch {
                    DispatchTarget::User(user_dispatch) =>
                    // SAFETY: `user_dispatch` is the actor's registered
                    // application trampoline; the arguments come from a
                    // well-formed copy-mode `HewMsgNode`.
                    unsafe {
                        user_dispatch(
                            ec_ptr,
                            a.state,
                            msg_ref.msg_type,
                            dispatch_data,
                            dispatch_size,
                            // Both admitted adapters own their argument fields.
                            0,
                        )
                    },
                    DispatchTarget::Sys(sys_dispatch, kind) => {
                        // SAFETY: `sys_dispatch` is the actor's registered
                        // system entry point and `kind` decoded from the
                        // system queue. System handlers run to completion,
                        // so there is no continuation handle to park.
                        unsafe {
                            sys_dispatch(
                                ec_ptr,
                                a.state,
                                kind.as_i32(),
                                dispatch_data,
                                dispatch_size,
                            );
                        }
                        std::ptr::null_mut()
                    }
                }));

                // SAFETY: `execution_context.lock_seat` was initialized from the
                // live actor immediately before the matching acquire.
                let release_result =
                    unsafe { crate::actor::hew_actor_state_lock_release_for_context(ec_ptr) };
                if release_result != crate::actor::HEW_ACTOR_STATE_LOCK_OK {
                    // A refused lock release traps the actor: the same
                    // crash teardown, counted in flight from its first
                    // cleanup step for the same reason.
                    let _crash_publication = crate::exit_status::CrashPublication::begin();
                    // SAFETY: dispatch returned and this activation owns
                    // the still-open cleanup scope.
                    let outcome =
                        unsafe { crate::cont::recover_dispatch_crash_cleanup_with_outcome(true) };
                    if outcome.state_authority_consumed {
                        // SAFETY: this activation exclusively owns actor.
                        unsafe { crate::actor::record_dispatch_state_drop_consumed(actor) };
                    }
                    // SAFETY: `actor` is the actor currently owned by this
                    // scheduler frame.
                    unsafe {
                        crate::actor::hew_actor_trap_from_activation(
                            actor,
                            crate::actor::HEW_ACTOR_STATE_LOCK_ERR,
                        );
                    }
                    crate::tracing::hew_trace_end(a.id, msg_ref.msg_type);
                    let _ = clear_reply_channel_on(ec_ptr);
                    let restored_context =
                        crate::execution_context::set_current_context(prev_context);
                    debug_assert_eq!(restored_context, ec_ptr);
                    // SAFETY: msg is exclusively owned by this worker.
                    unsafe {
                        (*msg).reply_channel = std::ptr::null_mut();
                        hew_msg_node_free(msg);
                    }
                    crate::observe::observe_dispatch_abandon(observe_dispatch_ticket);
                    crashed = true;
                    break;
                }

                // D-A.2: the suspend handle the trampoline returned. `null`
                // on the run-to-completion path; live suspending-terminator
                // codegen returns a non-null handle for in-handler
                // await/ask/recv suspends, and the suspend edge parks it
                // below.
                // SAFETY: the returned callback relinquished this context's
                // checked completion slot to its sole scheduler activation.
                let dispatch_result =
                    unsafe { crate::actor_native::dispatch_result(ec_ptr, dispatch_result) };
                let suspend_handle: *mut c_void = match dispatch_result {
                    Ok(handle) => {
                        // SAFETY: normal dispatch return matches the cleanup
                        // scope opened immediately before handler entry.
                        if !unsafe { crate::cont::finish_dispatch_crash_cleanup() } {
                            eprintln!(
                                "fatal: actor dispatch returned with live crash-cleanup owners"
                            );
                            std::process::abort();
                        }
                        handle
                    }
                    Err(failure) => {
                        let _crash_publication = crate::exit_status::CrashPublication::begin();
                        // SAFETY: the activation owns the returned native
                        // completion or caught legacy unwind and its state seat.
                        let code =
                            unsafe { crate::actor_native::finish_dispatch_failure(actor, failure) };
                        set_last_error("actor dispatch failed");
                        crate::crash::record_logical_crash(
                            a.id,
                            code,
                            msg_ref.msg_type,
                            a.dispatch.map_or(0, |f| f as usize),
                        );
                        // SAFETY: this scheduler frame exclusively owns the
                        // active actor and has completed stack cleanup.
                        unsafe {
                            crate::actor::hew_actor_trap_from_activation(actor, code);
                        }
                        crate::tracing::hew_trace_end(a.id, msg_ref.msg_type);
                        let reply_consumed = current_reply_channel_consumed_on(ec_ptr);
                        let crash_reply = clear_reply_channel_on(ec_ptr);
                        let restored_context =
                            crate::execution_context::set_current_context(prev_context);
                        debug_assert_eq!(restored_context, ec_ptr);
                        if !reply_consumed && !crash_reply.is_null() {
                            // SAFETY: the message owns this live reply ref.
                            unsafe {
                                crate::reply_channel::hew_reply_channel_publish_crash_fallback(
                                    crash_reply.cast(),
                                );
                            }
                        }
                        // SAFETY: msg is exclusively owned by this worker.
                        unsafe {
                            (*msg).reply_channel = std::ptr::null_mut();
                            hew_msg_node_free(msg);
                        }
                        crate::observe::observe_dispatch_abandon(observe_dispatch_ticket);
                        crashed = true;
                        break;
                    }
                };

                // W6.010 value routing: a suspending handler still owes a
                // reply to ITS caller. Stash this dispatch's reply channel on
                // the actor BEFORE the context/msg reply-channel teardown
                // below clears it, so the resume edge can re-establish a
                // context carrying it and the resumed coroutine body deposits
                // the reply (the body, not the unwound trampoline frame, owns
                // the deposit — the trampoline's out-slot is dead by resume).
                //
                // This is a MOVE, not a copy: the node's sender-side
                // reference becomes the actor slot's, and the node's pointer
                // is nulled here so `hew_msg_node_free` below cannot also
                // retire it. Owning the reference in exactly one place is
                // what lets every path that abandons the park resolve it
                // exactly once. A handler that already deposited its reply
                // before suspending owes nothing, and its reference is
                // already released, so that channel is NOT stashed.
                if !suspend_handle.is_null()
                    && !current_reply_channel_consumed_on(ec_ptr)
                    && !msg_ref.reply_channel.is_null()
                {
                    // Shutdown-drain ask gate: retain an INDEPENDENT
                    // channel reference for `parked_ask_channel` before the
                    // move below. The drain scan dereferences the channel's
                    // `cancelled` flag through this slot to tell an
                    // abandoned ask (caller resolved by `| after d` /
                    // cancel — not in-flight work) from a live one; the
                    // moved W6.010 reference below cannot serve that read
                    // because the resumed body may consume-and-free it
                    // while the slot still holds the stale pointer. The
                    // store precedes the `Running → Suspended` park CAS, so
                    // any scan that observes `Suspended` observes the gate.
                    // SAFETY: the mailbox node still owns a live reference;
                    // retain adds the gate's own.
                    unsafe {
                        crate::reply_channel::hew_reply_channel_retain(
                            msg_ref.reply_channel.cast(),
                        );
                    }
                    a.parked_ask_channel
                        .store(msg_ref.reply_channel, Ordering::Release);
                    a.suspended_reply_channel
                        .store(msg_ref.reply_channel, Ordering::Release);
                    // SAFETY: msg is exclusively owned by this worker; the
                    // reference now belongs to the actor slot.
                    unsafe { (*msg).reply_channel = std::ptr::null_mut() };
                }
                if !suspend_handle.is_null() {
                    let invocation = a.checked_invocation.load(Ordering::Acquire);
                    if !invocation.is_null() {
                        // SAFETY: this activation owns the invocation until its
                        // park is published; resume consumes the message identity.
                        unsafe {
                            (*invocation.cast::<crate::coro_state::HewCoroState>())
                                .actor_message_type = msg_ref.msg_type;
                        }
                    }
                    // SAFETY: `ec_ptr` points at the live dispatch-local
                    // context; reading `cancel_token` through it avoids
                    // re-borrowing the local (which would Unique-retag and
                    // invalidate the published thread-local pointer).
                    let cancel_token = unsafe { (*ec_ptr).cancel_token };
                    stash_suspended_cancel_token(a, cancel_token.cast());
                }

                let reply_consumed = current_reply_channel_consumed_on(ec_ptr);
                let _ = clear_reply_channel_on(ec_ptr);

                // Preserve the mailbox-owned reply sender only for teardown
                // states so hew_msg_node_free can publish the self-stop
                // fallback reply. Ordinary no-reply returns must keep the
                // prior pending-ask behavior instead of resolving early.
                let actor_state = a.actor_state.load(Ordering::Acquire);
                if reply_consumed
                    || (actor_state != HewActorState::Stopping as i32
                        && actor_state != HewActorState::Stopped as i32)
                {
                    // SAFETY: msg is exclusively owned by this worker.
                    unsafe { (*msg).reply_channel = std::ptr::null_mut() };
                }

                // Dispatch completed successfully — clear recovery point.
                crate::tracing::hew_trace_end(a.id, msg_ref.msg_type);
                let restored_context = crate::execution_context::set_current_context(prev_context);
                debug_assert_eq!(restored_context, ec_ptr);

                #[expect(
                    clippy::cast_possible_truncation,
                    reason = "single message dispatch will never exceed u64::MAX nanoseconds"
                )]
                let elapsed_ns = t0.elapsed().as_nanos() as u64;
                msgs_processed += 1;
                a.prof_messages_processed.fetch_add(1, Ordering::Relaxed);
                a.prof_processing_time_ns
                    .fetch_add(elapsed_ns, Ordering::Relaxed);
                crate::observe::record_actor_turn(elapsed_ns);
                crate::observe::hew_observe_probe_turn(
                    a.dispatch
                        .map_or(std::ptr::null(), |f| f as *const std::ffi::c_void),
                    msg_ref.msg_type,
                    elapsed_ns,
                );
                crate::observe::observe_dispatch_attributed(observe_dispatch_ticket);

                // SAFETY: `msg` was returned by `hew_mailbox_try_recv` and is
                // now exclusively owned by this worker.
                unsafe { hew_msg_node_free(msg) };

                // SUSPEND EDGE (D-A.2 / R326/R327): the handler suspended at a
                // non-final `coro.suspend` and handed back its `coro.begin`
                // frame handle. Park it against the executor and break out of
                // the message loop WITHOUT re-enqueuing — the worker is freed
                // to run other actors; a wake (`enqueue_resume`) later puts
                // this actor back on the scheduler run queue and the resume
                // re-entry drives the parked continuation. The per-actor lock
                // was already released on the dispatch-return edge above (FG2).
                // `park_suspended_activation` publishes `Parked` + stores the
                // handle + CAS `Running → Suspended` and drains a lost wake
                // (FG3).
                if !suspend_handle.is_null() {
                    // SAFETY: `actor` is owned by this frame (Running CAS held);
                    // `suspend_handle` is the live, suspended continuation the
                    // dispatch produced; the lock is released.
                    let parked = unsafe { park_suspended_activation(actor, suspend_handle) };
                    if parked {
                        // Parked: the actor is now `Suspended` (or was
                        // re-enqueued by a lost-wake drain). Do not requeue
                        // or settle here — the resume re-entry owns the rest
                        // of this activation's lifecycle.
                        return;
                    }
                    clear_suspended_cancel_token(a);
                    // Park refused (actor concurrently stopped/crashed): the
                    // handle was destroyed once inside the park guard. The
                    // suspend edge already moved the caller's reply reference
                    // into the actor slot, and no resume will ever consume it,
                    // so resolve it here. Fall through to the standard settle
                    // so the terminal state is honoured.
                    retire_suspended_reply_channel(a);
                }

                // Apply injected delay after dispatch (testing only).
                if delay_ms > 0 {
                    std::thread::sleep(Duration::from_millis(u64::from(delay_ms)));
                }
            }

            // If actor self-stopped during dispatch, stop processing.
            let mid_state = a.actor_state.load(Ordering::Acquire);
            if mid_state == HewActorState::Stopping as i32
                || mid_state == HewActorState::Stopped as i32
                || mid_state == HewActorState::Crashed as i32
            {
                break;
            }
        }
    }

    if !crashed && !actor_arena.is_null() {
        // SAFETY: Arena was created during spawn; no references survive past activation.
        unsafe { crate::arena::hew_arena_reset(actor_arena) };
    }

    // An external trap defers its mailbox drain while this frame owns the
    // consumer. `dispatch_active` prevents a supervisor from freeing the actor
    // until the activation-ownership guard drops, so retire any queued nodes
    // before releasing that ownership. Owned crash publication already drained
    // the queue; this second pass is harmless and keeps one exit invariant.
    let terminal_state = a.actor_state.load(Ordering::Acquire);
    if terminal_state == HewActorState::Stopped as i32
        || terminal_state == HewActorState::Crashed as i32
    {
        // SAFETY: this frame still owns the activation/mailbox consumer.
        unsafe { mailbox::mailbox_reclaim_queued_terminal(mailbox) };
    }

    // After a crash, do not enter the normal Running-state settle.
    if crashed {
        return;
    }

    // Check if actor transitioned to Stopping during dispatch (self-stop).
    let cur_state = a.actor_state.load(Ordering::Acquire);
    if cur_state == HewActorState::Stopping as i32 {
        // Reclaim anything still queued BEFORE the terminal state is published.
        // `hew_actor_stop` on a Running actor enqueues its shutdown sentinel on
        // the SYSTEM queue, which `mailbox_try_recv_with_origin` dequeues ahead
        // of user messages that were enqueued first — so the loop above breaks
        // on the sentinel with those messages still queued. A queued ask holds
        // the sender-side reply reference its caller is blocked on, and freeing
        // the node is what retires it. See `mailbox_reclaim_queued_terminal`.
        //
        // SAFETY: this worker owns the activation and is the mailbox's sole
        // consumer; `Stopping` is not quiescent, so no concurrent
        // `hew_actor_free` can be freeing the mailbox underneath the drain.
        unsafe { mailbox::mailbox_reclaim_queued_terminal(mailbox) };
        // Finalize: Stopping → Stopped.
        if a.actor_state
            .compare_exchange(
                HewActorState::Stopping as i32,
                HewActorState::Stopped as i32,
                Ordering::AcqRel,
                Ordering::Acquire,
            )
            .is_ok()
        {
            crate::tracing::hew_trace_lifecycle(a.id, crate::tracing::SPAN_STOP);
            // Terminal: publish any registered `receive gen fn` stream fault
            // before the monitors, mirroring `settle_after_activation`'s
            // `Stopping -> Stopped` settle. A stopped pump owes its consumer the
            // fault at the instant it stops, not whenever the box is freed.
            crate::actor::fault_close_registered_gen_sink(a);
            // A clean self-stop is a terminal transition, so monitors must be
            // notified with the Stopped reason — exactly as a crash trap notifies
            // them with Crashed. Without this, a monitor of a cleanly-stopped
            // actor (local OR cross-node) never observes the DOWN. The crash
            // path runs this from `hew_actor_trap`; the self-stop finalize is the
            // only place the Stopping → Stopped transition completes, so it must
            // run it too.
            if a.native_completion.is_none() {
                // Native completion publishes DOWN after typed state cleanup.
                crate::actor::notify_monitors_on_death(a.id, HewActorState::Stopped as i32, 0);
            }
            crate::actor_group::notify_actor_death(a.id);
            // SAFETY: actor just transitioned to Stopped; dispatch is finished.
            unsafe { crate::actor::call_terminate_fn(actor) };
        }
        return;
    }

    // Check if actor was stopped or crashed during dispatch.
    if cur_state == HewActorState::Stopped as i32 || cur_state == HewActorState::Crashed as i32 {
        return;
    }

    // Hibernation: track idle activations.
    actor::update_hibernation_state(a, msgs_processed);

    #[cfg(test)]
    run_activate_pre_reenqueue_hook(actor);

    // After processing: check for remaining messages.
    let cleanup_ready = a
        .native_completion
        .as_ref()
        .is_some_and(|completion| completion.cleanup.has_work());
    let has_more = cleanup_ready
        || if mailbox.is_null() {
            false
        } else {
            // SAFETY: mailbox pointer is valid.
            unsafe { hew_mailbox_has_messages(mailbox) != 0 }
        };

    if has_more {
        // Budget exhausted, more work pending → RUNNING → RUNNABLE, re-enqueue.
        // Only re-enqueue if CAS succeeds; if it fails the actor was
        // stopped/freed concurrently and touching it would be use-after-free.
        if a.actor_state
            .compare_exchange(
                HewActorState::Running as i32,
                HewActorState::Runnable as i32,
                Ordering::AcqRel,
                Ordering::Acquire,
            )
            .is_ok()
        {
            sched_enqueue(actor);
        }
    } else {
        // No more messages → RUNNING → IDLE.
        if a.actor_state
            .compare_exchange(
                HewActorState::Running as i32,
                HewActorState::Idle as i32,
                Ordering::AcqRel,
                Ordering::Acquire,
            )
            .is_ok()
        {
            // Recheck: a sender may have pushed a message while we were
            // RUNNING but before we transitioned to IDLE.  The sender's
            // CAS IDLE→RUNNABLE would have failed, so we must re-check.
            if !mailbox.is_null()
                // SAFETY: mailbox pointer is valid for the actor's lifetime.
                && (unsafe { hew_mailbox_has_messages(mailbox) != 0 }
                || a.native_completion.as_ref().is_some_and(|completion| completion.cleanup.has_work()))
            {
                // Messages appeared → IDLE → RUNNABLE, re-enqueue.
                if a.actor_state
                    .compare_exchange(
                        HewActorState::Idle as i32,
                        HewActorState::Runnable as i32,
                        Ordering::AcqRel,
                        Ordering::Acquire,
                    )
                    .is_ok()
                {
                    sched_enqueue(actor);
                }
            } else if !mailbox.is_null()
                // SAFETY: mailbox pointer is valid for the actor's lifetime.
                && unsafe { mailbox::mailbox_is_closed(mailbox) }
            {
                // Mailbox closed while draining → IDLE → STOPPED.
                if a.actor_state
                    .compare_exchange(
                        HewActorState::Idle as i32,
                        HewActorState::Stopped as i32,
                        Ordering::AcqRel,
                        Ordering::Acquire,
                    )
                    .is_ok()
                {
                    // Close can race a producer that already passed its open
                    // check. If its enqueue lands after the empty recheck and
                    // before its Idle -> Runnable CAS, this terminal CAS makes
                    // the wake fail; retire that late node while this worker
                    // still owns the live mailbox.
                    // SAFETY: this activation is the mailbox's sole consumer.
                    unsafe { mailbox::mailbox_reclaim_queued_terminal(mailbox) };
                    crate::tracing::hew_trace_lifecycle(a.id, crate::tracing::SPAN_STOP);
                    // Terminal, same reasoning as the `Stopping -> Stopped`
                    // finalize above.
                    crate::actor::fault_close_registered_gen_sink(a);
                    crate::actor_group::notify_actor_death(a.id);
                    // SAFETY: actor just transitioned to Stopped; dispatch is finished.
                    unsafe { crate::actor::call_terminate_fn(actor) };
                }
            }
        }
    }
}
#[cfg(test)]
pub(crate) fn activate_actor_for_test(actor: *mut HewActor) {
    if actor.is_null() {
        return;
    }
    // Direct unit tests do not arrive through a deque, so mint the same single
    // queue reference the production entry consumes.
    // SAFETY: test caller guarantees a live actor for the call.
    let mut entry = unsafe { SchedulerQueueEntry::retain(actor) };
    entry.disarm();
    activate_queued_actor(actor);
}
#[cfg(test)]
pub(crate) unsafe fn release_scheduler_queue_ref_for_test(actor: *mut HewActor) {
    // SAFETY: caller guarantees one scheduler queue reference is owned.
    unsafe { release_scheduler_queue_ref(actor) };
}
#[cfg(test)]
pub(crate) fn activate_actor(actor: *mut HewActor) {
    activate_actor_for_test(actor);
}
struct ActivationMetricsGuard;
impl ActivationMetricsGuard {
    fn new() -> Self {
        ACTIVE_ACTIVATIONS.fetch_add(1, Ordering::Relaxed);
        Self
    }
}
impl Drop for ActivationMetricsGuard {
    fn drop(&mut self) {
        ACTIVE_ACTIVATIONS.fetch_sub(1, Ordering::Relaxed);
        ACTIVATIONS_COMPLETED.fetch_add(1, Ordering::Relaxed);
    }
}
#[cfg(any(test, debug_assertions))]
pub(crate) static INJECT_NULL_LOCK_SEAT_ONCE: AtomicBool = AtomicBool::new(false);
/// Drop the shutdown-drain gate's independently retained reply-channel
/// reference ([`HewActor::parked_ask_channel`]).
///
/// The swap runs under the live-actors registry lock
/// ([`crate::lifetime::live_actors::swap_slot_under_registry_lock`]) because
/// the drain scan dereferences the channel through this slot under that lock;
/// swapping first means a scan that observed a non-null pointer cannot have
/// its channel freed beneath it. The reference release itself happens outside
/// the lock — the final release may run the reply payload's registered
/// destructor, which must not execute under a global registry lock.
///
/// Idempotent (the swap yields null on a second visit), so overlapping
/// abandon routes settle to exactly one release.
pub(crate) fn release_parked_ask_channel(a: &HewActor) {
    let ch = crate::lifetime::live_actors::swap_slot_under_registry_lock(&a.parked_ask_channel);
    if !ch.is_null() {
        // SAFETY: the gate slot owned this retained reference, taken on the
        // suspend edge; the swap above transferred it to this call.
        unsafe { crate::reply_channel::hew_reply_channel_free(ch.cast()) };
    }
}
/// Settle a resumed activation whose continuation suspended AGAIN
/// (`ResumePoll::Pending`).
///
/// Normally this re-parks: CAS `Running -> Suspended` (the handle stays parked,
/// the tag is already back to `Parked`), draining a wake that fired during the
/// resume window so it is not lost (FG3).
///
/// It also consults the out-of-band stop latch, which nothing else on this path
/// does. `activate_actor` returns immediately after `resume_suspended_activation`
/// for a live parked continuation, so its loop-top latch check is on the
/// fresh-dispatch path only. Without the two checks below, a continuation that
/// was `Running` when `hew_actor_stop` latched and then hit another await would
/// be re-parked `Suspended` with `stop_requested` still set: if the awaited
/// operation never wakes again the actor never reaches `Stopped` and never runs
/// its terminate callback, and every later wake takes this same
/// resume-before-loop path.
///
/// Cancel rather than re-enqueue: re-enqueuing would drive the actor straight
/// back into a resume that re-parks, a busy loop with no external wake behind it.
///
/// # Safety
///
/// `actor` is owned by the calling activation frame, which holds the `Running`
/// CAS, and its parked continuation just reported `Pending`.
unsafe fn settle_pending_resume(actor: *mut HewActor) {
    // SAFETY: caller owns `actor`.
    let a = unsafe { &*actor };
    let mailbox = a.mailbox.cast::<HewMailbox>();
    // Latch check BEFORE re-parking: the stopper observed `Running` while the
    // continuation was executing.
    // SAFETY: the mailbox pointer is valid for the actor's lifetime
    // (null-tolerant).
    if unsafe { mailbox::mailbox_stop_requested(mailbox) }
        // SAFETY: the caller owns this activation and its parked child state.
        && !unsafe { crate::actor_native::cancel_checked_turn(a) }
        && a.actor_state
            .compare_exchange(
                HewActorState::Running as i32,
                HewActorState::Stopping as i32,
                Ordering::AcqRel,
                Ordering::Acquire,
            )
            .is_ok()
    {
        // SAFETY: this frame owns the activation and just published `Stopping`.
        unsafe { cancel_parked_activation_for_stop(actor) };
        return;
    }

    // Re-park: the continuation suspended again. CAS back to Suspended.
    if a.actor_state
        .compare_exchange(
            HewActorState::Running as i32,
            HewActorState::Suspended as i32,
            Ordering::AcqRel,
            Ordering::Acquire,
        )
        .is_ok()
    {
        crate::observe::record_coroutine_suspend();
        crate::observe::hew_observe_probe_suspend(
            a.dispatch
                .map_or(std::ptr::null(), |f| f as *const std::ffi::c_void),
        );
        // FG3: a wake during the resume window must not be lost.
        if crate::coro_exec::take_pending_wake(a)
            && a.actor_state
                .compare_exchange(
                    HewActorState::Suspended as i32,
                    HewActorState::Runnable as i32,
                    Ordering::AcqRel,
                    Ordering::Acquire,
                )
                .is_ok()
        {
            sched_enqueue(actor);
            return;
        }
        // Latch re-check AFTER publishing `Suspended`, mirroring the FG3 drain
        // above. Closes the window where the stopper observed `Running`, we
        // passed the check at the top, and the latch landed before the CAS. The
        // stopper's own latch-then-recheck covers the remaining direction (it
        // observed `Running`, we published `Suspended`, and it latched after
        // this load) by waking the actor so this path runs again.
        // SAFETY: the mailbox pointer is valid for the actor's lifetime.
        if unsafe { mailbox::mailbox_stop_requested(mailbox) }
        // SAFETY: the caller owns this activation and its parked child state.
        && !unsafe { crate::actor_native::cancel_checked_turn(a) }
            && a.actor_state
                .compare_exchange(
                    HewActorState::Suspended as i32,
                    HewActorState::Stopping as i32,
                    Ordering::AcqRel,
                    Ordering::Acquire,
                )
                .is_ok()
        {
            // SAFETY: winning the `Suspended -> Stopping` CAS makes this frame
            // the sole owner: a resume would have had to win
            // `Suspended -> Runnable` first.
            unsafe { cancel_parked_activation_for_stop(actor) };
        }
    } else {
        // Stopped/crashed under us -- destroy the parked frame once, and resolve
        // the ask the dead continuation will never answer.
        // SAFETY: no concurrent resume; we just observed Pending.
        let _ = unsafe { crate::coro_exec::destroy_parked(a) };
        retire_suspended_reply_channel(a);
        clear_suspended_cancel_token(a);
    }
}
/// Finish a checked failure or native unwind from a resumed continuation.
///
/// Both paths retire the parked reply and publish the actor fault through the
/// same terminal transition. Checked completion has already run source cleanup
/// and destroys its final frame normally. Native unwind instead recovers the
/// abandoned frame's owners before raw reclamation.
///
/// # Safety
///
/// Called after a checked body reaches final suspend or `catch_unwind` returns
/// `Err`. `actor` is owned by this frame (Running CAS held). `resume_context` is the
/// still-installed dispatch context (a live stack local in the caller frame);
/// the prior context is restored via `restore_current_context_after_dispatch`,
/// which walks `resume_context`'s `prev_context`.
unsafe fn finish_failed_resume(
    actor: *mut HewActor,
    resume_context: *mut HewExecutionContext,
    message_type: i32,
    failure: crate::actor_native::DispatchFailure,
) {
    // This frame is a crash teardown, and a crash teardown RELEASES OTHER
    // THREADS long before it reaches the trap that puts the crash on the
    // exit-status authority: the parked-ask gate release below, the crash
    // fallback that resolves the waiter's `await`, the mailbox close, the
    // queued-ask retirement. Count the crash as in flight from the first
    // instruction, so a thread woken by any of them cannot read a clean status
    // over a crash that is already under way.
    let _crash_publication = crate::exit_status::CrashPublication::begin();

    // SAFETY: caller owns `actor` via the Running CAS.
    let a = unsafe { &*actor };
    let actor_arena = a.arena;
    let checked = matches!(&failure, crate::actor_native::DispatchFailure::Checked(_));
    let code = match failure {
        crate::actor_native::DispatchFailure::Checked(fault) => {
            // SAFETY: the source cleanup returned normally; its actor state
            // escrow remains borrowed and the final frame has one destroy owner.
            if !unsafe { crate::cont::finish_dispatch_crash_cleanup() } {
                eprintln!("fatal: checked resumed actor failure retained crash-cleanup owners");
                std::process::abort();
            }
            crate::actor_native::report_checked_failure(&fault)
        }
        crate::actor_native::DispatchFailure::Unwind(payload) => {
            let code = payload
                .downcast_ref::<crate::actor::HewPanic>()
                .map_or(101, |panic| panic.code);
            crate::util::quarantine_panic_payload(payload);
            code
        }
    };
    crate::crash::record_logical_crash(
        a.id,
        code,
        message_type,
        a.dispatch.map_or(0, |f| f as usize),
    );

    // Generated dispatch wrappers acquire the actor-state lock before the
    // handler body; the unwind may bypass their explicit release edge, so release any
    // guard this dispatch held before the crash path notifies supervisors.
    // SAFETY: `actor` is the actor this frame is resuming; the release helper
    // tolerates an unheld/unregistered lock.
    unsafe {
        let _ = crate::actor::hew_actor_state_lock_release_after_panic(actor);
    }

    // The actor slot remains the sole raw-allocation owner of the resumed
    // handler root. Exclude it while raw-reclaiming nested synchronous child
    // ramps; `abandon_resuming_after_crash` below removes/frees that root exactly
    // once. Its typed field obligations are independent and run in swap unwind.
    if !checked {
        let scheduler_root = a.suspended_cont.load(Ordering::Acquire);
        // A child suspending-closure call that unwound inside the resume
        // bypassed the driver's swap-pop and driver-channel teardown. Restore the
        // outer reply routing, tear those channels down, and typed-drop abandoned
        // frame slots before raw reclamation. Root field drops run here exactly
        // once; only its raw frame allocation remains reserved for the actor-slot
        // authority below.
        crate::execution_context::reply_channel_swap_unwind();
        // SAFETY: catch_unwind proves the active resume stack is dead. The drain
        // frees only positively tracked nested frames and preserves
        // `scheduler_root` for the actor-slot authority.
        let _ = unsafe { crate::cont::reclaim_active_coroutine_frames_excluding(scheduler_root) };
        // Frame/nested owners are newer and drain first. The dispatch registry then
        // releases ordinary stack owners and finally the structurally valid actor
        // state escrow, all before arena reset and raw state disposal.
        // SAFETY: catch_unwind proves the dispatch stack is abandoned and this recovery
        // path exclusively owns its cleanup scope.
        let outcome = unsafe { crate::cont::recover_dispatch_crash_cleanup_with_outcome(false) };
        if outcome.state_authority_consumed {
            // SAFETY: this recovery frame exclusively owns the crashed actor.
            unsafe { crate::actor::record_dispatch_state_drop_consumed(actor) };
        }
    }

    // Capture the crashed resume's reply-channel state from the still-installed
    // resume context (carrying the handler's stashed reply channel) before
    // restoring the previous context.
    let reply_consumed = current_reply_channel_consumed_on(resume_context);
    let crash_reply = clear_reply_channel_on(resume_context);
    restore_current_context_after_dispatch();

    // The resume's reply channel was a snapshot of the actor's stashed slot;
    // clear that slot + the drain gate's retained reference + the stashed
    // cancel token so a re-armed actor does not reuse the freed channel/token
    // (the crashed activation is terminal).
    release_parked_ask_channel(a);
    a.suspended_reply_channel
        .store(std::ptr::null_mut(), Ordering::Release);
    clear_suspended_cancel_token(a);

    // Transition `Running → Crashing` (accepting `Stopping` too, as a pending
    // self-stop is dominated by the crash) BEFORE any publication so a waiter in
    // `hew_actor_free` cannot observe the actor terminal and free the arena out
    // from under this worker. Fails only if already terminal / a state this
    // worker did not set.
    let took_crashing = loop {
        let cur = a.actor_state.load(Ordering::Acquire);
        if cur != HewActorState::Running as i32 && cur != HewActorState::Stopping as i32 {
            break false;
        }
        if a.actor_state
            .compare_exchange(
                cur,
                HewActorState::Crashing as i32,
                Ordering::AcqRel,
                Ordering::Acquire,
            )
            .is_ok()
        {
            break true;
        }
    };

    // Retire the frame before publishing a quiescent actor state. The checked
    // path reached Done; the unwind path abandoned a frame still tagged Resuming.
    if checked {
        // SAFETY: a returned checked fault has completed source cleanup and
        // reached final suspend. Reclaim it through its normal destroy outline.
        let _ = unsafe { crate::coro_exec::destroy_parked(a) };
    } else {
        // SAFETY: the native unwind abandoned a running frame; its typed
        // obligations were recovered above, so only raw reclamation remains.
        let _ = unsafe { crate::coro_exec::abandon_resuming_after_crash(a) };
    }

    // Per-activation arena cleanup BEFORE publishing terminal `Crashed`.
    if took_crashing && !actor_arena.is_null() {
        // SAFETY: arena was created at spawn; the crash discards all in-flight
        // data, and `Crashing` prevents `hew_actor_free` from reclaiming it
        // ahead of us.
        unsafe { crate::arena::hew_arena_reset(actor_arena) };
    }

    // If the handler did not already deposit its reply, send the empty crash
    // fallback so the waiting `hew_actor_ask` caller resolves to `Err` rather
    // than deadlocking. Done before publishing `Crashed` so the channel-count
    // invariant is observable as soon as the actor is terminal.
    if !reply_consumed && !crash_reply.is_null() {
        // SAFETY: `crash_reply` is a valid `HewReplyChannel` pointer.
        // Classified as a handler trap so the waiter's null reply is
        // status-bearing, never a bare null.
        unsafe {
            crate::reply_channel::hew_reply_channel_publish_crash_fallback(crash_reply.cast());
        }
    }

    // Publish terminal `Crashed` and run supervisor / link / monitor
    // notifications.
    if took_crashing {
        // SAFETY: per-activation cleanup (frame reclaim, arena reset, late
        // crash-reply) has already run and this frame owns the actor.
        unsafe { crate::actor::hew_actor_trap_from_activation(actor, code) };
    }
}
/// Shared post-activation settle: CAS `Running → Runnable` (re-enqueue) when
/// the mailbox still has work, else `Running → Idle` with the standard
/// idle→runnable / idle→stopped rechecks. Factored out so the resume re-entry
/// and the message loop share one settle path.
/// Settle a completed runtime-owned cleanup turn through ordinary requeue rules.
/// # Safety
/// The scheduler owns the actor's Running activation.
pub(crate) unsafe fn settle_native_cleanup(actor: *mut HewActor) {
    settle_after_activation(actor, 0);
}

#[expect(
    clippy::too_many_lines,
    reason = "mailbox and consuming cleanup share one atomic idle recheck protocol"
)]
fn settle_after_activation(actor: *mut HewActor, msgs_processed: u32) {
    // SAFETY: caller owns `actor` via the Running CAS.
    let a = unsafe { &*actor };
    let mailbox = a.mailbox.cast::<HewMailbox>();

    if let Some(code) = crate::actor::take_deferred_external_trap(a) {
        debug_assert!(
            a.checked_invocation.load(Ordering::Acquire).is_null(),
            "a deferred external trap must wait for the checked turn to drain"
        );
        // SAFETY: this activation has completed its checked cancellation and
        // still owns the mailbox consumer. Publish the originally requested
        // crash through the ordinary activation-owned terminal path.
        unsafe { crate::actor::hew_actor_trap_from_activation(actor, code) };
        return;
    }

    let cur_state = a.actor_state.load(Ordering::Acquire);
    if cur_state == HewActorState::Stopped as i32 || cur_state == HewActorState::Crashed as i32 {
        // An external trap can publish the terminal state while this scheduler
        // frame still owns the mailbox consumer. The trap cannot drain safely
        // in that case; do it here before `dispatch_active` is released.
        // SAFETY: this activation remains the sole mailbox consumer.
        unsafe { mailbox::mailbox_reclaim_queued_terminal(mailbox) };
        return;
    }
    if cur_state == HewActorState::Stopping as i32 {
        // Reclaim anything still queued BEFORE the terminal state is published.
        // The shutdown sentinel outranks the user queue, so this activation can
        // reach `Stopping` with user messages — including asks whose callers are
        // blocked on their reply channels — still in the mailbox. See
        // `mailbox_reclaim_queued_terminal` for why draining here is the wake.
        //
        // SAFETY: this worker owns the activation and is the mailbox's sole
        // consumer; `Stopping` is not quiescent, so no concurrent
        // `hew_actor_free` can be freeing the mailbox underneath the drain.
        unsafe { mailbox::mailbox_reclaim_queued_terminal(mailbox) };
        if a.actor_state
            .compare_exchange(
                HewActorState::Stopping as i32,
                HewActorState::Stopped as i32,
                Ordering::AcqRel,
                Ordering::Acquire,
            )
            .is_ok()
        {
            crate::tracing::hew_trace_lifecycle(a.id, crate::tracing::SPAN_STOP);
            // Terminal: no further activation of this actor will ever run, so a
            // `receive gen fn` pump registered here can never produce another
            // value. Publish the stream fault BEFORE the monitors and the
            // terminate callback, so a consumer parked in the channel's recv is
            // released at the instant the producer stops rather than waiting for
            // the actor to be freed (which a supervisor may defer, or which may
            // race this path's own frame destroy and lose it).
            crate::actor::fault_close_registered_gen_sink(a);
            // Clean self-stop on the resume path: notify monitors with the
            // Stopped reason, mirroring the crash trap and the non-resume
            // finalize. See the companion comment in `activate_actor`.
            if a.native_completion.is_none() {
                // Native completion publishes DOWN after typed state cleanup.
                crate::actor::notify_monitors_on_death(a.id, HewActorState::Stopped as i32, 0);
            }
            crate::actor_group::notify_actor_death(a.id);
            // SAFETY: actor just transitioned to Stopped; dispatch is finished.
            unsafe { crate::actor::call_terminate_fn(actor) };
        }
        return;
    }
    if cur_state == HewActorState::Stopped as i32 || cur_state == HewActorState::Crashed as i32 {
        return;
    }

    actor::update_hibernation_state(a, msgs_processed);

    let cleanup_ready = a
        .native_completion
        .as_ref()
        .is_some_and(|completion| completion.cleanup.has_work());
    let has_more = cleanup_ready
        || if mailbox.is_null() {
            false
        } else {
            // SAFETY: mailbox pointer is valid for the actor's lifetime.
            unsafe { hew_mailbox_has_messages(mailbox) != 0 }
        };

    if has_more {
        if a.actor_state
            .compare_exchange(
                HewActorState::Running as i32,
                HewActorState::Runnable as i32,
                Ordering::AcqRel,
                Ordering::Acquire,
            )
            .is_ok()
        {
            sched_enqueue(actor);
        }
    } else if a
        .actor_state
        .compare_exchange(
            HewActorState::Running as i32,
            HewActorState::Idle as i32,
            Ordering::AcqRel,
            Ordering::Acquire,
        )
        .is_ok()
    {
        if !mailbox.is_null()
            // SAFETY: mailbox pointer is valid for the actor's lifetime.
            && (unsafe { hew_mailbox_has_messages(mailbox) != 0 }
                || a.native_completion.as_ref().is_some_and(|completion| completion.cleanup.has_work()))
        {
            if a.actor_state
                .compare_exchange(
                    HewActorState::Idle as i32,
                    HewActorState::Runnable as i32,
                    Ordering::AcqRel,
                    Ordering::Acquire,
                )
                .is_ok()
            {
                sched_enqueue(actor);
            }
        } else if !mailbox.is_null()
            // SAFETY: mailbox pointer is valid for the actor's lifetime.
            && unsafe { mailbox::mailbox_is_closed(mailbox) }
            && a.actor_state
                .compare_exchange(
                    HewActorState::Idle as i32,
                    HewActorState::Stopped as i32,
                    Ordering::AcqRel,
                    Ordering::Acquire,
                )
                .is_ok()
        {
            // A producer can have passed the mailbox's open check before close,
            // then publish its node after our empty recheck but before its own
            // wake CAS. Winning Idle -> Stopped makes that CAS fail, so this
            // worker is the last consumer that can retire the node.
            // SAFETY: this activation owns the mailbox consumer and the actor
            // remains live until the activation returns.
            unsafe { mailbox::mailbox_reclaim_queued_terminal(mailbox) };
            crate::tracing::hew_trace_lifecycle(a.id, crate::tracing::SPAN_STOP);
            // Terminal, same reasoning as the `Stopping -> Stopped` settle
            // above: a pump that stops here (mailbox closed while it sat idle
            // between yields) owes its consumer the stream fault.
            crate::actor::fault_close_registered_gen_sink(a);
            crate::actor_group::notify_actor_death(a.id);
            // SAFETY: actor just transitioned to Stopped; dispatch is finished.
            unsafe { crate::actor::call_terminate_fn(actor) };
        }
    }
}
/// RAII marker that an `activate_actor` frame owns an actor's in-flight
/// activation. Set `dispatch_active = true` *before* the worker attempts the
/// `Runnable -> Running` CAS and clears it on EVERY exit of the activation
/// (settle, suspend-park, crash-break, normal fall-through, or a lost CAS).
///
/// The flag exists because an external `hew_actor_trap` CAS-es the actor
/// straight to the quiescent `Crashed`/`Stopped` state out from under this
/// worker — erasing the `Running` state while the worker is still reading
/// `a.actor_state`, the arena, and the mailbox in its post-dispatch settle. The
/// async free that the trap's supervisor-notify ultimately drives
/// (`hew_actor_free`) treats `Crashed`/`Stopped` as quiescent and would reclaim
/// the box + mailbox in that window — a use-after-free that surfaces as a
/// corrupted msg-node tripping the envelope-mode dispatch guard. `hew_actor_free`
/// additionally waits on this flag, so it never frees under an in-flight
/// activation regardless of which thread published the terminal state.
///
/// CAS-gap ordering (why claim runs BEFORE the CAS, not after): a trap can only
/// flip a `Running` actor terminal. If the flag were set *after* a successful
/// `Runnable -> Running` CAS, the actor would be `Running` and trap-stealable
/// for the instructions between the CAS and the set, during which the flag
/// still reads `false`. A trap landing there flips `Running -> Crashed`, and a
/// free observing `Crashed && !dispatch_active` reclaims the box before the
/// worker sets the flag and continues reading the (now freed) box — UAF.
/// Claiming the flag *before* the CAS guarantees it is already `true` the
/// instant the actor can become `Running`, so the trap-stealable window is
/// never flag-`false`. Once claimed, a lost state CAS drops this worker's own
/// guard on the early-return path. A dequeuer that finds the flag already owned
/// must not discard a `Runnable` entry: the prior activation publishes its
/// self-reenqueue before its guard drops, so that overlap is the legitimate
/// activation handoff rather than a duplicate.
///
/// Queue-epoch invariant: every transition into `Runnable` is CAS-gated and
/// only its winner publishes a scheduler entry. A busy claim with state still
/// `Runnable` therefore denotes the prior owner's publish-before-release tail
/// (or a duplicate entry whose winner is about to leave `Runnable`), never
/// authority to discard the last runnable entry.
pub(crate) struct ActivationOwnership<'a> {
    pub(crate) actor: &'a HewActor,
}
impl<'a> ActivationOwnership<'a> {
    /// Mark the activation owned. Call BEFORE the `Runnable -> Running` CAS so
    /// the flag is already published when the actor first becomes `Running`.
    pub(crate) fn claim(actor: &'a HewActor) -> Option<Self> {
        actor
            .dispatch_active
            .compare_exchange(false, true, Ordering::AcqRel, Ordering::Acquire)
            .ok()
            .map(|_| Self { actor })
    }

    /// Transfer a popped `Runnable` queue entry to exclusive activation
    /// ownership.
    ///
    /// A budget-exhausted activation performs `Running -> Runnable`, publishes
    /// the replacement queue entry, and only then drops its ownership guard.
    /// Another worker may dequeue in that short interval. It must retain the
    /// queue reference and wait for the old guard to release; dropping the
    /// entry would strand a Runnable actor with pending messages and no
    /// scheduler entry. If state leaves Runnable, another consumer or a
    /// terminal transition won the race and this entry is redundant.
    ///
    /// The retry intentionally has no fail-open timeout: abandoning this sole
    /// queue reference would recreate the strand. Every path that publishes
    /// `Runnable` while holding activation ownership proceeds directly to guard
    /// release; after a short spin, yielding lets that owner finish.
    fn claim_dequeued(
        actor: &'a HewActor,
        #[cfg(test)] actor_ptr: *mut HewActor,
        #[cfg(not(test))] _actor_ptr: *mut HewActor,
    ) -> Option<Self> {
        let mut attempts = 0_u32;
        loop {
            if let Some(ownership) = Self::claim(actor) {
                return Some(ownership);
            }
            if actor.actor_state.load(Ordering::Acquire) != HewActorState::Runnable as i32 {
                return None;
            }

            #[cfg(test)]
            run_scheduler_queue_handoff_hook(&ACTIVATE_CLAIM_BUSY_HOOK, actor_ptr);

            attempts = attempts.saturating_add(1);
            if attempts < 64 {
                std::hint::spin_loop();
            } else {
                std::thread::yield_now();
            }
        }
    }
}
impl Drop for ActivationOwnership<'_> {
    fn drop(&mut self) {
        #[cfg(test)]
        run_activation_pre_terminal_lock_hook(self.actor);

        let native_pin = self.actor.native_completion.as_ref().and_then(|_| {
            crate::lifetime::live_actors::pin_actor_by_id(self.actor.id)
                .filter(|pin| std::ptr::eq(pin.actor(), self.actor))
        });
        // Test terminal state, perform the final drain, and publish ownership
        // release under one terminal-reclaim lock. If an external trap gets the
        // lock first and observes this owner, this check must run afterward and
        // see its terminal publication. If this check runs first and sees
        // non-terminal, the ownership clear must precede the trap's locked
        // quiescence check, authorizing the trap to drain.
        //
        // The same critical section closes the producer link handoff: a node
        // linked after this drain cannot observe ownership release until the
        // lock becomes available for its own conditional drain.
        //
        // SAFETY: this guard still publishes exclusive activation ownership.
        // The predicate authorizes a terminal drain only while that ownership
        // remains held; no actor free can run until the release callback.
        unsafe {
            mailbox::mailbox_reclaim_queued_terminal_if_then(
                self.actor.mailbox.cast::<HewMailbox>(),
                || {
                    let state = self.actor.actor_state.load(Ordering::Acquire);
                    state == HewActorState::Stopped as i32 || state == HewActorState::Crashed as i32
                },
                || {
                    // Release so a free path that subsequently observes the
                    // cleared flag also observes every write and queued-node
                    // retirement this activation made to the actor box.
                    self.actor.dispatch_active.store(false, Ordering::Release);
                },
            );
        }
        if let Some(pin) = native_pin {
            let actor = pin.actor();
            let state = actor.actor_state.load(Ordering::Acquire);
            if (state == HewActorState::Stopped as i32 || state == HewActorState::Crashed as i32)
                && !actor.dispatch_active.load(Ordering::Acquire)
            {
                // SAFETY: the terminal state excludes future handler execution;
                // the pin retains the allocation after activation release. All
                // mailbox locks have been released before invoking destructors.
                unsafe { crate::actor_native::finish_native_terminal(actor) };
            }
        }
    }
}
/// The dispatch entry point selected for one dequeued message.
///
/// Built from the node's [`Origin`] BEFORE any handler runs, so the choice of
/// callee is the provenance decision itself. There is no path that constructs
/// `Sys` from a user-queue node or `User` from a system-queue node.
#[derive(Clone, Copy)]
pub(crate) enum DispatchTarget {
    /// An application message for the actor's user trampoline.
    User(HewDispatchFn),
    /// A runtime lifecycle signal for the actor's system entry point.
    Sys(HewSysDispatchFn, HewSysMsg),
}
fn restore_current_context_after_dispatch() {
    let ctx = crate::execution_context::current_context();
    if !ctx.is_null() {
        // SAFETY: scheduler-installed contexts point to live stack slots until
        // the dispatch recovery path restores the previously active context.
        let prev = unsafe { (*ctx).prev_context };
        let _ = crate::execution_context::set_current_context(prev);
    }
}
/// Wake a `Suspended` actor whose parked continuation has become resumable.
///
/// This is the SINGLE resume edge every readiness source feeds: the seed/test
/// source today, and post-slice-4 the reactor (NEW-1), reply slot (NEW-3),
/// channels (NEW-4), and wire reply (NEW-5). It is the dual of `sched_enqueue`
/// for a parked-then-woken actor and is structurally identical to the
/// `Sleeping → Runnable` re-enqueue the wasm sleeper-drain already proves.
///
/// `cont` is the continuation the source has made resumable. If the actor's
/// resume slot is empty (the park has not finished publishing — the FG3
/// lost-wake window), the wake is RECORDED via `mark_pending_wake` and the
/// suspend edge drains it; the wake is never lost.
///
/// CAS discipline (fail-closed): the wake only enqueues on a successful
/// `Suspended → Runnable` CAS. If the actor is terminal (`Stopped`/`Crashed`)
/// or not yet `Suspended`, the CAS fails and the actor is NOT enqueued —
/// mirroring the `Idle → Runnable` waker discipline in `activate_actor`, which
/// closes the use-after-free window against a freed actor.
///
/// RECORDED-vs-DIRECT delivery is exclusive (stale-wake guard): a call that
/// wins the CAS delivers DIRECTLY and must CONSUME any `pending_wake` marker —
/// including one this same call recorded moments earlier in the mid-park
/// window. A marker left set after a direct delivery is a STALE wake: it
/// survives into the actor's NEXT park cycle, whose FG3 drain re-enqueues a
/// wake with no readiness behind it, and a suspending `select` resumed by that
/// stale wake scans `hew_select_ready_index() == -1` and fabricates a timeout
/// (the `after` arm fires with the deadline unexpired — the event sits queued,
/// unconsumed, until teardown). Consuming under the registry lock BEFORE the
/// enqueue is race-free: the actor is `Runnable` but not yet on the queue, so
/// nothing can run it and re-park between the CAS and the consume.
///
/// # Not a wake entry point
///
/// A pointer names an ADDRESS, and addresses are recycled, so this cannot be
/// the edge a readiness source calls: the registry probe below proves the
/// address is live, never that it still holds the incarnation that parked.
/// Production reaches this only through [`enqueue_resume_by_incarnation`],
/// which resolves an [`ActorIncarnation`] and holds a pin over the call. The
/// scheduler's own unit tests call it directly to exercise the CAS and
/// pending-wake discipline in isolation.
///
/// That is enforced by visibility, not by this comment: the function is private
/// to this module, so the only callers it can ever have are
/// `enqueue_resume_by_incarnation` and the tests below. A readiness source in
/// another module cannot name it, and so cannot reintroduce the address-keyed
/// wake this change removed.
///
/// # Safety
///
/// `actor`, if non-null, must be pinned live by the caller for the duration of
/// this call. `cont`, if non-null, must be the continuation parked on `actor`
/// (a `coro.begin` frame). The caller (a readiness source) owns the wake edge;
/// the executor owns teardown.
pub(crate) unsafe fn enqueue_resume_pinned(actor: *mut HewActor, cont: *mut c_void) {
    if actor.is_null() {
        return;
    }

    // W6.010 caller-actor UAF guard (S1). This entry point is reached not only by
    // a live reply but also by the orphan-retire teardown
    // (`hew_reply_channel_retire_orphaned_ask_sender_ref`), which the CALLEE
    // mailbox runs during its own teardown. `cleanup_all_actors` frees actors in
    // nondeterministic `HashMap` order, so the caller box can already be freed
    // when the callee teardown fires this wake. Dereferencing `actor` directly
    // would be a heap-use-after-free. (Production callers arrive through
    // `enqueue_resume_by_incarnation`, which has already refused a wake whose
    // incarnation is gone; this guard is what makes the raw-pointer entry point
    // safe for the scheduler's own tests and for a pin held across the call.)
    //
    // `with_live_actor` makes the liveness check and the wake one atomic action:
    // it holds the `LIVE_ACTORS` registry lock across the closure, and EVERY free
    // path (`hew_actor_free_inner`, `drain_quiesced_actor`, `cleanup_all_actors`)
    // removes the actor from `LIVE_ACTORS` BEFORE reclaiming the box. So while the
    // closure runs the box cannot be freed, and if the caller was already torn
    // down the closure never runs (the pointer is no longer tracked) — the stale
    // wake is dropped, never dereferenced. The freed caller's continuation is
    // already destroyed by its own C1 teardown, so dropping the wake is correct.
    let enqueued = crate::lifetime::live_actors::with_live_actor(actor, |a| {
        // Capture the actor's owning-runtime id under the registry lock (the
        // actor is guaranteed live here). The R4 wake-routing net is asserted
        // AFTER `with_live_actor` returns and the lock is released (below), so a
        // trap never poisons the registry lock.
        let actor_runtime_id = a.runtime_id;
        // If the park has not yet stored a handle, the suspend edge is mid-park
        // (the FG3 window). Record the wake so the suspend edge re-enqueues; do
        // NOT store the handle ourselves (the suspend edge owns the slot write).
        let parked = a.suspended_cont.load(Ordering::Acquire);
        if parked.is_null() {
            crate::coro_exec::mark_pending_wake(a);
            // The actor is not yet `Suspended`; the CAS below would fail anyway.
            // Re-check after marking: if the park JUST finished publishing
            // `Suspended` between our load and the mark, fall through to the CAS
            // so the wake is delivered now rather than waiting on the suspend
            // edge's pending-wake drain. (Two-phase park, both directions.)
            if a.actor_state.load(Ordering::Acquire) != HewActorState::Suspended as i32 {
                let _ = cont; // handle is owned by the suspend edge; nothing to store.
                return (None, actor_runtime_id);
            }
        }

        // Own the prospective queue entry before publishing Runnable. The
        // registry lock keeps the allocation live while this reference is
        // acquired; afterward the entry itself closes the pre-publish window.
        // SAFETY: `with_live_actor` holds registry authority for this actor.
        let queue_entry = unsafe { SchedulerQueueEntry::retain(actor) };

        // CAS Suspended → Runnable; only enqueue on success (fail-closed against
        // a terminal or not-yet-parked actor). The loop runs AT MOST twice: the
        // second iteration exists only for the mark-after-drain window (see the
        // `Err(_)` arm) and every second-iteration outcome is terminal.
        let mut retried = false;
        loop {
            match a.actor_state.compare_exchange(
                HewActorState::Suspended as i32,
                HewActorState::Runnable as i32,
                Ordering::AcqRel,
                Ordering::Acquire,
            ) {
                Ok(_) => {
                    // Direct delivery won: consume the pending marker so recorded
                    // and direct delivery collapse into ONE wake. Without this,
                    // the mark at the top of this call (mid-park window, park
                    // published `Suspended` between the slot load and the
                    // re-check) leaks past the park's own FG3 drain and fires a
                    // stale wake on the actor's NEXT park — the
                    // fabricated-timeout race (see the fn doc). Consuming a
                    // CONCURRENT racer's marker here is equally correct: its
                    // readiness deposit happens-before its mark (deposit-then-
                    // wake contract), so the single direct wake's resume scan
                    // observes that readiness too.
                    let _ = crate::coro_exec::take_pending_wake(a);
                    break (Some(queue_entry), actor_runtime_id);
                }
                Err(observed) if observed == HewActorState::Runnable as i32 => {
                    // `Runnable`: a delivery is ALREADY enqueued — either the
                    // park's FG3 drain (it consumed a marker and re-enqueued) or
                    // another source's direct CAS win. Marking here would be the
                    // stale-wake duplicate: if this call's own mark above was
                    // already drained, a second mark survives into a later park
                    // and fires with no readiness behind it. No wake is lost by
                    // not marking:
                    // - if the drain consumed OUR mark, its AcqRel swap reading
                    //   our Release mark orders our readiness deposit before the
                    //   drain's enqueue and hence before the resume's scan;
                    // - if another source won directly, every enqueue_resume
                    //   source settles a one-shot arbiter BEFORE waking — our
                    //   source either won it (its readiness is published before
                    //   the completion the resume edge observes, so the resume's
                    //   status-gated re-scan finds it) or lost it (the winner
                    //   carries the resume; our effect is resolved by the
                    //   arbiter).
                    break (None, actor_runtime_id);
                }
                Err(_) => {
                    // `Running` (dispatch park not yet published — the FG3
                    // window) or `Idle` (lifecycle park's Idle → Suspended
                    // window): the wake could genuinely be missed — record it so
                    // the park's drain delivers. Terminal actors
                    // (`Stopped`/`Crashed`) also land here; the mark is inert (a
                    // terminal actor never parks again).
                    #[cfg(test)]
                    run_enqueue_resume_cas_fail_hook(actor);
                    crate::coro_exec::mark_pending_wake(a);
                    // Mark-after-drain guard: the park can publish `Suspended`
                    // AND run its ONE-SHOT drain inside the gap between our
                    // failed CAS and the mark above. The mark then lands after
                    // the only drain that would have consumed it, stranding the
                    // actor `Suspended` with a set marker (the lifecycle park
                    // has no second drain; the dispatch park's is equally
                    // one-shot). Re-check: if the state now reads `Suspended`,
                    // retry the CAS ourselves — the `Ok` arm consumes the
                    // marker, so the retry self-cleans. ONE retry suffices;
                    // every retry outcome is terminal:
                    // - `Ok`: delivered, marker consumed;
                    // - `Err(Runnable)`: another delivery is in flight (the
                    //   no-mark arm's safety argument applies; the residual
                    //   marker is at worst one honest respark);
                    // - `Err(Running|Idle)`: a NEW park cycle began after our
                    //   mark, so its future drain (which runs after it publishes
                    //   `Suspended`) is ordered after our mark and consumes it —
                    //   the strand needs mark-after-drain, and our mark is now
                    //   provably before that park's drain.
                    if !retried
                        && a.actor_state.load(Ordering::Acquire) == HewActorState::Suspended as i32
                    {
                        retried = true;
                        continue;
                    }
                    break (None, actor_runtime_id);
                }
            }
        }
    });

    // The owned entry keeps the allocation live after dropping the registry
    // lock and until the queue consumer claims dispatch ownership.
    //
    // R4 wake-routing net: `sched_enqueue` resolves its scheduler through
    // `get_scheduler()` → `rt_default()`, which `enter()` does not reroute, so an
    // actor owned by a non-default runtime would wake on the WRONG scheduler.
    // Fail closed before the enqueue (single-runtime actors pass straight
    // through). The runtime id was captured under the registry lock above, so
    // this reads no freed memory.
    if let Some((Some(entry), actor_runtime_id)) = enqueued {
        assert_wake_routes_to_owning_runtime(actor_runtime_id);
        sched_enqueue_owned(entry);
    }
}
/// Wake the exact actor incarnation a readiness source parked.
///
/// This is the single wake edge every readiness source calls: the reactor, the
/// reply slot, channels, task scopes, await-cancel, supervisor restart-await,
/// the wire reply, and mailbox block-send admission.
///
/// The source records an [`ActorIncarnation`] at park time, not a pointer.
/// Resolution here refuses two distinct failures and is silent about both,
/// because dropping the wake is the correct answer to each:
///
/// - the id is untracked — the actor died and its own teardown already
///   destroyed the continuation this wake would have resumed;
/// - the id resolves but the `spawn_serial` differs — the address (or the id,
///   after a 2^48 wrap that the spawn allocator refuses anyway) now belongs to
///   a DIFFERENT incarnation. Waking it would CAS a stranger
///   `Suspended -> Runnable` with no readiness behind it, and a suspending
///   `select` resumed that way fabricates a timeout.
///
/// The refusal happens before any mutation of the resolved actor, so a stale
/// wake cannot leave a `pending_wake` marker on the wrong incarnation either.
/// The pin taken during resolution is held across the enqueue, so the target
/// cannot die or have its address reused mid-call.
pub(crate) fn enqueue_resume_by_incarnation(target: ActorIncarnation) {
    let _ = crate::lifetime::live_actors::with_live_incarnation(target, |pin| {
        // SAFETY: the pin keeps THIS incarnation's allocation live across the
        // wake; the free path drains `send_pin_count` before reclaiming it.
        // `cont` is null because the suspend edge owns the parked handle — the
        // resume edge reads it from `suspended_cont`, never from here.
        unsafe { enqueue_resume_pinned(pin.as_ptr(), std::ptr::null_mut()) };
    });
}
/// Fail-closed wake-routing net (R4).
///
/// The scheduler wake path resolves its scheduler through [`get_scheduler`] →
/// [`runtime::rt_default`], **independent** of `rt_current()`, so an `enter()`
/// (or [`runtime::enter_actor_runtime`]) does NOT reroute the wake. Until M4
/// reroutes the wake to the actor's owning runtime, a wake for a non-default
/// actor would silently enqueue on the default runtime's scheduler — the same
/// silent-default hazard `enter_actor_runtime` traps on the send/teardown side.
///
/// This asserts the resolved (default) scheduler's runtime owns the actor about
/// to be enqueued, and traps otherwise (`no-fail-open-fallback-after-authority`:
/// assert-net first, reroute/delete at M4). A single-runtime
/// (`RuntimeId::DEFAULT`) actor never trips it — `rt_default()` IS its runtime —
/// so the production hot path is unaffected. Full rerouting of the wake to the
/// owning runtime's scheduler is M4; this change only installs the trap.
#[inline]
fn assert_wake_routes_to_owning_runtime(actor_runtime_id: crate::runtime_id::RuntimeId) {
    if actor_runtime_id == crate::runtime_id::RuntimeId::DEFAULT {
        return;
    }
    #[cfg(target_arch = "wasm32")]
    panic!(
        "hew-runtime: enqueue_resume: a wake for an actor owned by runtime {} reached the one \
         wasm32 runtime; there is no second scheduler to route it to",
        actor_runtime_id.as_u64(),
    );
    #[cfg(not(target_arch = "wasm32"))]
    let resolved = crate::runtime::rt_default().map(crate::runtime::RuntimeInner::runtime_id);
    #[cfg(not(target_arch = "wasm32"))]
    assert_eq!(
        resolved,
        Some(actor_runtime_id),
        "hew-runtime: enqueue_resume: a wake for an actor owned by runtime {} would route \
         through the default scheduler (runtime {:?}); the scheduler wake path is not yet \
         rerouted to the owning runtime (M4) — failing closed rather than waking on a \
         foreign scheduler",
        actor_runtime_id.as_u64(),
        resolved.map(crate::runtime_id::RuntimeId::as_u64),
    );
}
#[cfg(test)]
pub(crate) fn run_scheduler_queue_handoff_hook(
    hook: &PoisonSafe<Option<SchedulerQueueHandoffHook>>,
    actor: *mut HewActor,
) {
    // SAFETY: both seams still hold the scheduler queue lifetime reference.
    let actor_id = unsafe { (*actor).id };
    let rendezvous = hook.access(|slot| {
        slot.as_ref().and_then(|(target, entered, release)| {
            (*target == actor_id).then(|| (entered.clone(), release.clone()))
        })
    });
    if let Some((entered, release)) = rendezvous {
        entered.wait();
        release.wait();
    }
}
#[cfg(test)]
fn run_activate_pre_reenqueue_hook(actor: *mut HewActor) {
    let hook = ACTIVATE_PRE_REENQUEUE_HOOK.access(|h| *h);
    if let Some(hook) = hook {
        hook(actor);
    }
}
#[cfg(test)]
fn run_activate_post_cas_hook(actor: *mut HewActor) {
    let hook = ACTIVATE_POST_CAS_HOOK.access(|h| *h);
    if let Some(hook) = hook {
        hook(actor);
    }
}
#[cfg(test)]
fn run_enqueue_resume_cas_fail_hook(actor: *mut HewActor) {
    let hook = ENQUEUE_RESUME_CAS_FAIL_HOOK.access(|h| *h);
    if let Some(hook) = hook {
        hook(actor);
    }
}
#[cfg(test)]
pub(crate) fn run_activation_pre_terminal_lock_hook(actor: &HewActor) {
    let rendezvous = ACTIVATION_PRE_TERMINAL_LOCK_HOOK.access(|hook| {
        hook.as_ref().and_then(|(actor_id, entered, release)| {
            (*actor_id == actor.id).then(|| (entered.clone(), release.clone()))
        })
    });
    if let Some((entered, release)) = rendezvous {
        entered.wait();
        release.wait();
    }
}
#[cfg(test)]
pub(crate) static ACTIVATE_PRE_REENQUEUE_HOOK: PoisonSafe<Option<fn(*mut HewActor)>> =
    PoisonSafe::new(None);
/// Fires inside `activate_actor` immediately after the worker WINS the
/// `Runnable -> Running` CAS — the exact CAS->marker-gap location. A regression
/// test installs a hook here to fire an external trap in the precise window the
/// pre-fix code left `dispatch_active == false` while the actor was already
/// `Running`, and asserts the fix (claim before the CAS) keeps the flag set so
/// the free-quiescence predicate refuses.
#[cfg(test)]
pub(crate) static ACTIVATE_POST_CAS_HOOK: PoisonSafe<Option<fn(*mut HewActor)>> =
    PoisonSafe::new(None);
/// Fires inside `enqueue_resume`'s CAS-lose arm, AFTER the failed
/// `Suspended -> Runnable` CAS and BEFORE the pending-wake mark — the exact
/// window the lifecycle-park lost-wake interleaving spans: a park can publish
/// `Suspended` AND run its one-shot drain inside this gap, so the mark lands
/// after the only drain that would have consumed it. A regression test installs
/// a park-completion hook here to force that ordering deterministically and
/// asserts the post-mark re-check + CAS retry delivers the wake instead of
/// stranding the actor `Suspended` with a set marker.
#[cfg(test)]
pub(crate) static ENQUEUE_RESUME_CAS_FAIL_HOOK: PoisonSafe<Option<fn(*mut HewActor)>> =
    PoisonSafe::new(None);
#[cfg(test)]
pub(crate) static ACTIVATE_PRE_CLAIM_HOOK: PoisonSafe<Option<SchedulerQueueHandoffHook>> =
    PoisonSafe::new(None);
/// Rendezvous after a dequeued actor observes a still-active prior activation
/// while the actor is already `Runnable`. This is the self-reenqueue handoff:
/// the prior activation published the queue entry before releasing
/// `dispatch_active`.
#[cfg(test)]
pub(crate) static ACTIVATE_CLAIM_BUSY_HOOK: PoisonSafe<Option<SchedulerQueueHandoffHook>> =
    PoisonSafe::new(None);
#[cfg(test)]
pub(crate) static ACTIVATION_PRE_TERMINAL_LOCK_HOOK: PoisonSafe<
    Option<ActivationPreTerminalLockHook>,
> = PoisonSafe::new(None);
