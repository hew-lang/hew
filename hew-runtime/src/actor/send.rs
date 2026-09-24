//! Actor send/ask/close/stop machinery and the corresponding C ABI functions.
#[allow(
    clippy::wildcard_imports,
    reason = "submodule split from a single large file; re-exports its parent scope"
)]
use super::*;

// ── Send ────────────────────────────────────────────────────────────────
// Standard send functions use the native mailbox/scheduler. WASM standard
// sends go through bridge lowering; wire sends also expose a direct runtime
// entrypoint so encoded actor messages can use the same deterministic path.

/// Send a message to an actor (fire-and-forget).
///
/// Deep-copies `data`. If the actor is idle, transitions it to runnable
/// and enqueues it on the scheduler.
///
/// # Safety
///
/// - `actor` must be a valid pointer returned by a spawn function.
/// - `data` must point to at least `size` readable bytes, or be null
///   when `size` is 0.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_send(
    actor: *mut HewActor,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
) {
    // SAFETY: Caller guarantees `actor` is valid.
    unsafe { actor_send_internal(actor, msg_type, data, size) };
}

/// Send a wire-encoded message to an actor.
///
/// Extracts raw bytes from the `HewVec` (bytes type), deep-copies them
/// into the actor's mailbox, and frees the `HewVec`.
///
/// # Safety
///
/// - `actor` must be a valid pointer returned by a spawn function.
/// - `bytes` must be a valid `HewVec*` (bytes type) or null.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_send_wire(
    actor: *mut HewActor,
    msg_type: i32,
    bytes: *mut crate::vec::HewVec,
) {
    if bytes.is_null() || actor.is_null() {
        return;
    }
    // SAFETY: bytes is a valid HewVec. Extract raw byte data.
    let data = unsafe { crate::vec::hwvec_to_u8(bytes) };
    // SAFETY: actor is valid, data slice is valid.
    unsafe { actor_send_internal(actor, msg_type, data.as_ptr() as *mut c_void, data.len()) };
    // SAFETY: bytes was allocated by hew_vec and is no longer needed.
    unsafe { crate::vec::hew_vec_free(bytes) };
}

/// Send a message to an actor by actor ID.
///
/// Returns `0` ([`HewError::Ok`]) on success — including a declared bounded
/// mailbox's silent policy-drop (`DropNew`/`DropOld`/`Coalesce`; spec §6.2).
/// A genuine, caller-visible failure keeps its own distinct non-zero
/// [`HewError`] code: `-1` (`ErrMailboxFull`) for a `Fail`-policy rejection,
/// `-2` (`ErrActorStopped`) if the actor is gone (not tracked locally,
/// stopped, or crashed), `-5` (`ErrOom`) on allocation failure, `-6`
/// (`ErrForeignRuntime`) for a cross-runtime pointer, or a remote-send error
/// code if the PID belongs to another node. Callers must trap on any
/// non-zero result rather than special-casing `-1` — see
/// `hew_mailbox_send_fire_and_forget` for why the two failure shapes are no
/// longer conflated.
///
/// `dispatch` is the TARGET actor TYPE's dispatch function pointer
/// (`__hew_actor_dispatch_<Actor>`), supplied by codegen at the remote-send
/// site (it knows the target type statically from `RemotePid<T>`). It keys the
/// cross-node serialize codec `(dispatch, msg_type)` so a colliding `msg_type`
/// on another actor type cannot select the wrong serializer for the value being
/// shipped. It is unused on the LOCAL send path (the local mailbox copies the
/// in-memory value directly); local-only callers may pass null.
///
/// # Safety
///
/// `data` must point to at least `size` readable bytes, or be null when
/// `size` is 0. For local actors, callers must only send to actor IDs whose
/// lifetime they still coordinate; once the live lookup succeeds, this path
/// shares the same liveness contract as [`hew_actor_send`]. `dispatch` is an
/// opaque codec key, never dereferenced.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_send_by_id(
    actor_id: u64,
    _dispatch: *const c_void,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
) -> c_int {
    // Use the liveness-pin protocol: under LIVE_ACTORS, validate the actor
    // and increment its `send_pin_count`; release the lock; run the send;
    // the RAII SendPinGuard decrements the pin on return.  The free path
    // calls `untrack_actor` first (so no new pins can be taken after that
    // point), then spins until `send_pin_count == 0` before finalizing.
    // Because pin-increment and map-removal are both under LIVE_ACTORS, the
    // two are mutually exclusive: this send either pins before the freer
    // untracks (freer waits) or the freer untracks before this map lookup
    // (lookup returns None, no pin, no UAF).
    //
    // The closure returns the actual `HewError` code from
    // `actor_send_result_internal` — NOT a collapsed bool. Every genuine
    // failure (actor gone/stopped, OOM, foreign-runtime, a `Fail`-policy
    // overflow) keeps its own distinct negative code; declared loss under
    // `DropNew`/`DropOld`/`Coalesce` uses a dedicated positive code. Keeping
    // those classes distinct lets checked sends report the exact outcome.
    let send_result = live_actors::with_actor_send_by_id(actor_id, |actor| {
        // SAFETY: `actor` is pinned live by `with_actor_send_by_id`;
        // the allocation is guaranteed valid for the duration of this
        // closure.  Same data/size preconditions as hew_actor_send.
        unsafe { actor_send_result_internal(actor, msg_type, data, size) }
    });
    if let Some(code) = send_result {
        return code;
    }

    // Actor not tracked locally. If the PID belongs to a remote node, route
    // through the distributed node infrastructure (which serializes the
    // payload under the `(dispatch, msg_type)` codec key).
    if crate::pid::hew_pid_is_local(actor_id) == 0 {
        crate::set_last_error(
            "hew_actor_send_by_id accepts local actor ids only; remote sends require an exact Location",
        );
        return -1;
    }
    // A local PID with no live actor behind it is gone — already stopped,
    // freed, or never existed — a genuine, caller-visible failure. Report
    // it as `ErrActorStopped`, never the overloaded `ErrMailboxFull`, so it
    // can never be mistaken for a declared bounded-mailbox overflow outcome.
    HewError::ErrActorStopped as i32
}

/// Cooperatively send to a bounded `overflow block` mailbox.
///
/// Returns `0` after registering the copied message and parked producer, `1`
/// when the message was admitted immediately, or a negative [`HewError`] when
/// no ownership transfer occurred. A registered producer is resumed by the
/// target mailbox after dequeue creates capacity.
///
/// `out_id` receives the [`mailbox::BlockedSenderId`] minted for a parked
/// registration (`0` when the send was admitted immediately, since there is
/// then no registration to detach). The caller passes it back to
/// [`hew_actor_detach_await_send_by_id`] to withdraw the registration by
/// identity rather than by the read-slot address, which is recycled (#3147).
///
/// # Safety
///
/// `data` must cover `size` readable bytes; `sender` is the current live actor
/// and `slot` is a live read slot whose creator ref remains with the caller.
/// `out_id` must be a writable `*mut u64`, or null to discard the id.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_await_send_by_id(
    actor_id: u64,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
    sender: *mut HewActor,
    slot: *mut crate::read_slot::HewReadSlot,
    out_id: *mut u64,
) -> c_int {
    let result = live_actors::with_actor_send_by_id(actor_id, |actor| {
        // SAFETY: the live registry pin keeps the actor valid for this closure;
        // the remaining arguments retain the public ABI's contract.
        unsafe { actor_await_send_pinned(actor, msg_type, data, size, sender, slot) }
    });
    let (rc, id) = result.unwrap_or((HewError::ErrActorStopped as i32, 0));
    if !out_id.is_null() {
        // SAFETY: caller-provided non-null output, per this function's contract.
        unsafe { out_id.write(id) };
    }
    rc
}

/// Register a cooperative send while an identity-verified live-actor pin is
/// held. Shared by direct-PID and stable-supervisor-role submission.
///
/// Returns the status code and the [`mailbox::BlockedSenderId`] minted for a
/// parked registration (`0` when admitted immediately or refused).
///
/// # Safety
///
/// `actor` must remain pinned, `data` must cover `size` readable bytes, and
/// `sender`/`slot` must satisfy [`mailbox::mailbox_await_send`].
pub(crate) unsafe fn actor_await_send_pinned(
    actor: *mut HewActor,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
    sender: *mut HewActor,
    slot: *mut crate::read_slot::HewReadSlot,
) -> (c_int, mailbox::BlockedSenderId) {
    // SAFETY: the caller holds an identity-verified live-actor pin.
    let target = unsafe { &*actor };
    if !actor_runtime_matches(target) {
        return (HewError::ErrForeignRuntime as i32, 0);
    }
    #[cfg(not(target_arch = "wasm32"))]
    let Ok(_ingress) = crate::shutdown::admit_external_work() else {
        return (HewError::ErrActorStopped as i32, 0);
    };
    if actor_send_is_terminal(target) {
        return (HewError::ErrActorStopped as i32, 0);
    }
    // SAFETY: the pin keeps the mailbox live and the caller supplies the
    // payload, sender, and read-slot contracts.
    let (rc, id) = unsafe {
        mailbox::mailbox_await_send(target.mailbox.cast(), msg_type, data, size, sender, slot)
    };
    if rc == mailbox::MAILBOX_AWAIT_SEND_READY {
        // SAFETY: immediate readiness means a node reached the target queue.
        unsafe { schedule_actor_after_enqueue(actor, target, msg_type) };
    }
    (rc, id)
}

/// Detach an abandoned cooperative block-send registration, named by the
/// [`mailbox::BlockedSenderId`] [`hew_actor_await_send_by_id`] minted for it.
/// Idempotent when admission or target close already resolved the waiter.
///
/// # Safety
///
/// No pointer safety obligations beyond the ordinary ABI call convention;
/// `registration_id` is an opaque scalar identity, not a pointer.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_detach_await_send_by_id(
    actor_id: u64,
    registration_id: mailbox::BlockedSenderId,
) {
    let _ = live_actors::with_actor_send_by_id(actor_id, |actor| {
        // SAFETY: the registry pin keeps the actor and mailbox live.
        unsafe { mailbox::mailbox_detach_await_send((*actor).mailbox.cast(), registration_id) };
    });
}

/// Resolve the exact actor incarnation behind a stable local handle.
///
/// # Safety
/// `out_actor_id` must be a valid writable pointer when non-null.
#[no_mangle]
pub unsafe extern "C" fn hew_local_pid_actor_id(
    token: crate::lifetime::local_handles::HewLocalPidId,
    out_actor_id: *mut u64,
) -> i32 {
    if out_actor_id.is_null() {
        return HewError::ErrActorStopped as i32;
    }
    let Some(actor_id) = crate::lifetime::local_handles::resolve_current_actor(token) else {
        return HewError::ErrActorStopped as i32;
    };
    let Some(pin) = live_actors::pin_actor_by_id(actor_id) else {
        return HewError::ErrActorStopped as i32;
    };
    // SAFETY: the caller supplied a writable out pointer; write only on success.
    unsafe { *out_actor_id = pin.actor().id };
    HewError::Ok as i32
}

/// Send through a stable local actor identity.
///
/// # Safety
/// `data` must be readable for `size` bytes, or null when `size` is zero.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_local_pid_send(
    token: crate::lifetime::local_handles::HewLocalPidId,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
) -> i32 {
    let Some(actor_id) = crate::lifetime::local_handles::resolve_current_actor(token) else {
        return HewError::ErrActorStopped as i32;
    };
    live_actors::with_actor_send_by_id(actor_id, |actor| {
        #[cfg(not(target_arch = "wasm32"))]
        // SAFETY: the actor is pinned and data follows this function's contract.
        return unsafe { actor_send_result_internal(actor, msg_type, data, size) };
    })
    .unwrap_or(HewError::ErrActorStopped as i32)
}

/// Try to send a message, returning an error code on failure.
///
/// Returns `0` on success, or a negative error code (see [`HewError`]).
///
/// # Safety
///
/// Same requirements as [`hew_actor_send`].
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_try_send(
    actor: *mut HewActor,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
) -> i32 {
    cabi_guard!(actor.is_null(), HewError::ErrActorStopped as i32);
    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };
    // Fail closed on a cross-runtime pointer (never fires single-runtime).
    if !actor_runtime_matches(a) {
        return HewError::ErrForeignRuntime as i32;
    }
    #[cfg(not(target_arch = "wasm32"))]
    let Ok(_ingress) = crate::shutdown::admit_external_work() else {
        return HewError::ErrClosed as i32;
    };
    // Terminal-state send gate (see `actor_send_is_terminal`): reject once the
    // actor is terminal even if its mailbox is not yet closed, closing the
    // trap's terminal-CAS-before-mailbox-close window. The non-blocking caller
    // gets `ErrClosed` here — the same code `hew_mailbox_try_send` returns for a
    // closed mailbox — preserving the try_send-vs-blocking error divergence
    // (`hew_mailbox_send`'s native/WASM note): a terminal actor and a closed
    // mailbox are the same observable condition for a non-blocking sender.
    if actor_send_is_terminal(a) {
        return HewError::ErrClosed as i32;
    }
    let mb = a.mailbox.cast::<HewMailbox>();

    // SAFETY: Mailbox is valid for the actor's lifetime.
    let result = unsafe { mailbox::hew_mailbox_try_send(mb, msg_type, data, size) };
    if result != 0 {
        return result;
    }

    // SAFETY: this producer fully linked a node and still owns actor lifetime.
    unsafe { finish_mailbox_enqueue(actor, a) };

    0
}

// ── Close / Stop / Free ─────────────────────────────────────────────────

/// Try to terminalize an idle actor after its mailbox has been closed.
///
/// The winning `Idle -> Stopped` CAS is also the last point that still owns a
/// live, non-dispatching actor and its mailbox. A sender can already have
/// transferred an ask node into that mailbox while paused before its own
/// `Idle -> Runnable` CAS. Once this CAS wins the sender's wake must fail, so
/// this path must retire queued nodes before returning; no later activation can
/// do it.
///
/// `reclaim_queued` exists solely to make the pre-fix omission executable in a
/// unit counterfactual. Production callers always pass `true`; the false branch
/// differs by exactly the missing reclaim edge.
///
/// # Safety
///
/// `actor`, `a`, and `mb` must name the same live actor/mailbox allocation, and
/// the mailbox must already be closed.
pub(crate) unsafe fn try_terminalize_idle_actor(
    actor: *mut HewActor,
    a: &HewActor,
    mb: *mut HewMailbox,
    reclaim_queued: bool,
) -> bool {
    if a.actor_state
        .compare_exchange(
            HewActorState::Idle as i32,
            HewActorState::Stopped as i32,
            Ordering::AcqRel,
            Ordering::Acquire,
        )
        .is_err()
    {
        return false;
    }

    if reclaim_queued {
        // SAFETY: winning Idle -> Stopped proves no activation owns this
        // mailbox's consumer side. The actor and mailbox remain live until this
        // function returns, and the closed mailbox rejects later sends.
        unsafe { mailbox::mailbox_reclaim_queued_terminal(mb) };
    }

    crate::tracing::hew_trace_lifecycle(a.id, crate::tracing::SPAN_STOP);
    // SAFETY: actor just transitioned to Stopped; it is not being dispatched.
    unsafe { call_terminate_fn(actor) };
    true
}

/// Close an actor, rejecting new messages.
///
/// Transitions the actor state to `Stopping`.
///
/// # Safety
///
/// `actor` must be a valid pointer returned by a spawn function.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_close(actor: *mut HewActor) {
    cabi_guard!(actor.is_null());
    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };

    // Close the mailbox so future sends are rejected.
    let mb = a.mailbox.cast::<HewMailbox>();
    if !mb.is_null() {
        // SAFETY: mailbox is valid for actor's lifetime.
        unsafe { mailbox::mailbox_close(mb) };
    }

    // SAFETY: actor/a/mb are the same live allocation and the mailbox was
    // closed immediately above.
    let _ = unsafe { try_terminalize_idle_actor(actor, a, mb, true) };
}

/// Stop an actor.
///
/// Closes the mailbox, transitions idle actors directly to `Stopped`, and for
/// an actor that is already `Running` latches the mailbox's out-of-band stop
/// flag so its dispatch loop observes the request at the top of its next
/// iteration. A `Suspended` actor — one parked at an `await` with a live
/// continuation — is latched and then WOKEN, so a scheduler activation reaches
/// the resume path's latch check and cancels the park; otherwise a stop of an
/// actor whose awaited operation never completes would never be observed at
/// all. A queued continuation is also latched; a fresh queued dispatch can
/// instead drain the closed mailbox naturally to `Stopped`.
///
/// The stop is a FLAG, not a queued message: latching it allocates nothing and
/// cannot fail, so the request can never be lost under memory pressure.
///
/// # Safety
///
/// `actor` must be a valid pointer returned by a spawn function.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_stop(actor: *mut HewActor) {
    cabi_guard!(actor.is_null());
    // SAFETY: Caller guarantees `actor` is valid and remains valid throughout this function.
    let a = unsafe { &*actor };
    let mb = a.mailbox.cast::<HewMailbox>();
    if !mb.is_null() {
        // SAFETY: Mailbox is valid for the actor's lifetime.
        unsafe { mailbox::mailbox_close(mb) };
    }

    // SAFETY: actor/a/mb are the same live allocation and the mailbox was
    // closed immediately above.
    if unsafe { try_terminalize_idle_actor(actor, a, mb, true) } {
        return;
    }

    let state = a.actor_state.load(Ordering::Acquire);
    // A readiness wake may already have queued a parked continuation. Its
    // activation resumes the existing turn rather than draining the closed
    // mailbox, so it needs the same stop latch as a still-suspended turn.
    let queued_continuation =
        state == HewActorState::Runnable as i32 && crate::coro_exec::has_live_parked_cont(a);
    if state != HewActorState::Running as i32
        && state != HewActorState::Suspended as i32
        && !queued_continuation
    {
        return;
    }

    // Running actors are already inside a dispatch; latch the stop request so
    // the next loop iteration — or, for a resumed continuation, the resume
    // path's own latch check — observes the close request. This is an atomic
    // store — no node allocation, hence no failure mode on which the request is
    // silently dropped.
    // SAFETY: Mailbox is valid for the actor's lifetime (null-tolerant).
    unsafe { mailbox::mailbox_request_stop(mb) };

    // Latch-then-recheck. Between the load above and this store a `Running`
    // continuation can have hit another await and re-parked itself
    // `Suspended`, passing both latch checks on the resume path. Nothing
    // consults the flag again until something wakes the actor, so if the
    // awaited operation never completes the stop is stranded and the terminate
    // callback never runs. Re-read here and, when the actor is now (or already
    // was) parked, wake it: that activation takes the resume path, observes the
    // latch, and cancels the park. Fail-closed — losing the CAS means another
    // delivery is already in flight and will drive the same path.
    if a.actor_state.load(Ordering::Acquire) == HewActorState::Suspended as i32
        && a.actor_state
            .compare_exchange(
                HewActorState::Suspended as i32,
                HewActorState::Runnable as i32,
                Ordering::AcqRel,
                Ordering::Acquire,
            )
            .is_ok()
    {
        crate::resume::sched_enqueue(actor);
    }
}
