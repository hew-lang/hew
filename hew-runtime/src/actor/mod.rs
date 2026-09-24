//! Hew runtime: actor struct definition and state constants.
//!
//! Defines the [`HewActor`] struct layout for C ABI compatibility and the
//! actor state machine constants. The full actor API (spawn, send, activate)
//! will be implemented in a future iteration.
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

use crate::lifetime::live_actors;
use std::cell::Cell;
use std::collections::HashMap;
// live on not(wasm32) — drain_actors; dead here; caller actor.rs:2729
use std::collections::HashSet;
use std::ffi::{c_int, c_void};
use std::ptr;
use std::sync::atomic::{AtomicBool, AtomicI32, AtomicPtr, AtomicU32, AtomicU64, Ordering};
use std::sync::{Condvar, Mutex, OnceLock, PoisonError};
use std::thread::ThreadId;

use crate::execution_context::HewExecutionContext;
use crate::internal::types::{
    AskError, HewActorState, HewDispatchFn, HewError, HewOverflowPolicy, HewSysDispatchFn,
};
use crate::mailbox::{self, HewMailbox};
use crate::reply_channel::{self, HewReplyChannel};
#[cfg(test)]
use crate::scheduler;

/// Policy-sensitive actor-send status: the incoming send completed, but the
/// declared mailbox policy discarded, replaced, or coalesced work.
///
/// This positive code is intentionally outside the negative [`HewError`]
/// space. A delivery view reports the same loss as `Ok(Delivery.Discarded)`
/// through its own status protocol; ordinary lossless sends never produce it.
pub const HEW_ACTOR_SEND_MESSAGE_LOST: i32 = 1;

// ── Crash teardown ordering hook ─────────────────────────────────────────

type CrashTeardownOrderHook = Option<fn(c_int)>;

static CRASH_TEARDOWN_ORDER_HOOK: Mutex<CrashTeardownOrderHook> = Mutex::new(None);

#[doc(hidden)]
pub const HEW_ACTOR_CRASH_TEARDOWN_BEFORE_EXIT_PROPAGATION: c_int = 1;

#[doc(hidden)]
pub const HEW_ACTOR_CRASH_TEARDOWN_AFTER_EXIT_PROPAGATION: c_int = 2;

/// The FIRST point in the crash teardown at which another thread can be
/// released — the mailbox close that wakes blocked senders, immediately ahead
/// of the queued-ask retirement that completes a waiter's `await`.
///
/// A thread woken here runs on to whatever it does next, including `exit(0)`,
/// so the exit-status authority must already carry this crash by the time this
/// event fires. That is what makes the ordering testable rather than a race
/// whose outcome depends on which platform's scheduler is faster.
#[doc(hidden)]
pub const HEW_ACTOR_CRASH_TEARDOWN_BEFORE_FIRST_WAKE: c_int = 3;

#[cfg(not(target_arch = "wasm32"))]
#[doc(hidden)]
pub fn hew_actor_set_crash_teardown_order_hook(hook: Option<fn(c_int)>) {
    let mut guard = CRASH_TEARDOWN_ORDER_HOOK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    *guard = hook;
}

fn run_crash_teardown_order_hook(event: c_int) {
    let hook = {
        let guard = CRASH_TEARDOWN_ORDER_HOOK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        *guard
    };
    if let Some(hook) = hook {
        hook(event);
    }
}

// ── Send post-enqueue / pre-wake rendezvous hook (test-only) ─────────────
//
// An actor send transfers node ownership into the mailbox BEFORE attempting
// `Idle -> Runnable`. This hook pauses at that exact ownership boundary so a
// test can let stop/close win `Idle -> Stopped`, then verify the terminal path
// retires the exact node and its retained reply-sender reference even though
// the sender's wake CAS necessarily loses.
#[cfg(all(test, not(target_arch = "wasm32")))]
type SendPostEnqueueHook = (
    u64,
    std::sync::Arc<std::sync::Barrier>,
    std::sync::Arc<std::sync::Barrier>,
);

#[cfg(all(test, not(target_arch = "wasm32")))]
static SEND_POST_ENQUEUE_PRE_WAKE_HOOK: Mutex<Option<SendPostEnqueueHook>> = Mutex::new(None);

#[cfg(all(test, not(target_arch = "wasm32")))]
fn run_send_post_enqueue_pre_wake_hook(a: &HewActor) {
    let rendezvous = {
        let guard = SEND_POST_ENQUEUE_PRE_WAKE_HOOK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        guard.as_ref().and_then(|(actor_id, entered, release)| {
            (*actor_id == a.id).then(|| (entered.clone(), release.clone()))
        })
    };
    if let Some((entered, release)) = rendezvous {
        entered.wait();
        release.wait();
    }
}

#[cfg(all(test, not(target_arch = "wasm32")))]
pub(crate) struct SendPostEnqueueHookGuard;

#[cfg(all(test, not(target_arch = "wasm32")))]
impl SendPostEnqueueHookGuard {
    pub(crate) fn install(
        actor_id: u64,
    ) -> (
        Self,
        std::sync::Arc<std::sync::Barrier>,
        std::sync::Arc<std::sync::Barrier>,
    ) {
        let entered = std::sync::Arc::new(std::sync::Barrier::new(2));
        let release = std::sync::Arc::new(std::sync::Barrier::new(2));
        let mut hook = SEND_POST_ENQUEUE_PRE_WAKE_HOOK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        assert!(hook.is_none(), "send ownership hook already installed");
        *hook = Some((actor_id, entered.clone(), release.clone()));
        (Self, entered, release)
    }
}

#[cfg(all(test, not(target_arch = "wasm32")))]
impl Drop for SendPostEnqueueHookGuard {
    fn drop(&mut self) {
        *SEND_POST_ENQUEUE_PRE_WAKE_HOOK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner) = None;
    }
}

// ── Free-path pre-detach rendezvous hook (test-only) ─────────────────────
//
// Lets a test deterministically force the reactor-detach UAF window: the hook
// fires inside `hew_actor_free_inner` *after* the actor first looks quiescent
// and *before* `prepare_quiescent_actor_for_cleanup` (which runs
// `reactor_detach_actor`). A test installs a hook that releases a "reactor
// delivery" thread to publish a wake (`CAS Idle->Runnable` + `sched_enqueue`)
// during the detach window, so the producer-side re-check is exercised every
// run rather than by timing luck.
#[cfg(all(test, not(target_arch = "wasm32")))]
static FREE_PRE_DETACH_HOOK: Mutex<Option<fn(*mut HewActor)>> = Mutex::new(None);

#[cfg(all(test, not(target_arch = "wasm32")))]
#[cfg_attr(
    not(unix),
    allow(
        dead_code,
        reason = "only consumed by the unix-gated free-during-reactor-detach race test"
    )
)]
fn set_free_pre_detach_hook_for_test(hook: Option<fn(*mut HewActor)>) {
    let mut guard = FREE_PRE_DETACH_HOOK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    *guard = hook;
}

#[cfg(all(test, not(target_arch = "wasm32")))]
fn run_free_pre_detach_hook(actor: *mut HewActor) {
    let hook = {
        let guard = FREE_PRE_DETACH_HOOK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        *guard
    };
    if let Some(hook) = hook {
        hook(actor);
    }
}

// ── Free-path post-latch rendezvous hook (test-only) ─────────────────────
//
// Lets a test deterministically force the *non-reactor* wake UAF window: the
// hook fires inside `hew_actor_free_inner` *after* free has latched the actor
// out of `Idle` into the `Stopped` terminal state (step 3) and *before*
// `untrack_actor`. A test installs a hook that performs the exact link/monitor
// side effect (`with_live_actor_by_id` → `CAS Idle->Runnable` + `sched_enqueue`)
// that `send_exit_signal` / `send_down_notification` run for a crashing peer.
// Because free has already CAS'd the actor to `Stopped`, that producer-side
// `CAS Idle->Runnable` must fail and no enqueue can happen — proving the
// non-reactor wake is closed. Reverting the latch (breaking with the bare
// post-detach `Idle` observation) lets the hook's CAS succeed and leaves a
// freed actor queued (the UAF the verdict reproduced).
#[cfg(all(test, not(target_arch = "wasm32")))]
static FREE_POST_LATCH_HOOK: Mutex<Option<fn(*mut HewActor)>> = Mutex::new(None);

#[cfg(all(test, not(target_arch = "wasm32")))]
fn set_free_post_latch_hook_for_test(hook: Option<fn(*mut HewActor)>) {
    let mut guard = FREE_POST_LATCH_HOOK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    *guard = hook;
}

#[cfg(all(test, not(target_arch = "wasm32")))]
fn run_free_post_latch_hook(actor: *mut HewActor) {
    let hook = {
        let guard = FREE_POST_LATCH_HOOK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        *guard
    };
    if let Some(hook) = hook {
        hook(actor);
    }
}

// The destruction of an actor's system queue is only defensible if it cannot
// race a producer. That rests on an ORDERING -- the actor is latched into a
// terminal state and removed from live tracking before anything reaches the
// queue -- and an ordering is a checkable fact, not a paragraph. This hook
// fires on the instruction before `hew_mailbox_free`, so a test reads the
// state that holds AT destruction. Move the queue free above the latch or the
// untrack and `teardown_reaches_queue_destruction_only_after_terminal_and_untracked`
// fails.
#[cfg(all(test, not(target_arch = "wasm32")))]
static PRE_QUEUE_DESTROY_HOOK: Mutex<Option<fn(*mut HewActor)>> = Mutex::new(None);

#[cfg(all(test, not(target_arch = "wasm32")))]
fn set_pre_queue_destroy_hook_for_test(hook: Option<fn(*mut HewActor)>) {
    let mut guard = PRE_QUEUE_DESTROY_HOOK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    *guard = hook;
}

#[cfg(all(test, not(target_arch = "wasm32")))]
fn run_pre_queue_destroy_hook(actor: *mut HewActor) {
    let hook = {
        let guard = PRE_QUEUE_DESTROY_HOOK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        *guard
    };
    if let Some(hook) = hook {
        hook(actor);
    }
}

// ── Stable-registration retirement rendezvous hooks (test-only) ──────────
#[cfg(all(test, not(target_arch = "wasm32")))]
#[derive(Clone)]
struct RegistrationRetirementHook {
    actor_id: u64,
    entered: std::sync::Arc<std::sync::Barrier>,
    release: std::sync::Arc<std::sync::Barrier>,
}

#[cfg(all(test, not(target_arch = "wasm32")))]
static FREE_PRE_LATCH_REGISTRATION_HOOK: Mutex<Option<RegistrationRetirementHook>> =
    Mutex::new(None);

#[cfg(all(test, not(target_arch = "wasm32")))]
static FREE_POST_RETIRE_REGISTRATION_HOOK: Mutex<Option<RegistrationRetirementHook>> =
    Mutex::new(None);

#[cfg(all(test, not(target_arch = "wasm32")))]
pub(crate) struct RegistrationRetirementHookGuard {
    slot: &'static Mutex<Option<RegistrationRetirementHook>>,
}

#[cfg(all(test, not(target_arch = "wasm32")))]
impl Drop for RegistrationRetirementHookGuard {
    fn drop(&mut self) {
        *self
            .slot
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner) = None;
    }
}

#[cfg(all(test, not(target_arch = "wasm32")))]
fn install_registration_retirement_hook(
    slot: &'static Mutex<Option<RegistrationRetirementHook>>,
    actor_id: u64,
    entered: std::sync::Arc<std::sync::Barrier>,
    release: std::sync::Arc<std::sync::Barrier>,
) -> RegistrationRetirementHookGuard {
    *slot
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner) = Some(RegistrationRetirementHook {
        actor_id,
        entered,
        release,
    });
    RegistrationRetirementHookGuard { slot }
}

#[cfg(all(test, not(target_arch = "wasm32")))]
pub(crate) fn install_free_pre_latch_registration_hook_for_test(
    actor_id: u64,
    entered: std::sync::Arc<std::sync::Barrier>,
    release: std::sync::Arc<std::sync::Barrier>,
) -> RegistrationRetirementHookGuard {
    install_registration_retirement_hook(
        &FREE_PRE_LATCH_REGISTRATION_HOOK,
        actor_id,
        entered,
        release,
    )
}

#[cfg(all(test, not(target_arch = "wasm32")))]
pub(crate) fn install_free_post_retire_registration_hook_for_test(
    actor_id: u64,
    entered: std::sync::Arc<std::sync::Barrier>,
    release: std::sync::Arc<std::sync::Barrier>,
) -> RegistrationRetirementHookGuard {
    install_registration_retirement_hook(
        &FREE_POST_RETIRE_REGISTRATION_HOOK,
        actor_id,
        entered,
        release,
    )
}

#[cfg(all(test, not(target_arch = "wasm32")))]
fn run_registration_retirement_hook(
    slot: &'static Mutex<Option<RegistrationRetirementHook>>,
    actor_id: u64,
) {
    let hook = {
        let mut guard = slot
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        if guard.as_ref().is_some_and(|hook| hook.actor_id == actor_id) {
            guard.take()
        } else {
            None
        }
    };
    if let Some(hook) = hook {
        hook.entered.wait();
        hook.release.wait();
    }
}

// ── Drain target pin rendezvous hook (test-only) ─────────────────────────
//
// `drain_actors` resolves actor IDs under `LIVE_ACTORS`, then calls the raw
// pointer `hew_actor_stop` entry point after the registry lock is released.
// The allocation must remain pinned across that gap. This hook pauses after
// the pin is acquired and immediately before stop dereferences the pointer so
// a test can drive a concurrent free through untracking and prove that final
// reclamation remains blocked on this exact pin.
#[cfg(all(test, not(target_arch = "wasm32")))]
static DRAIN_POST_PIN_PRE_STOP_HOOK: Mutex<Option<RegistrationRetirementHook>> = Mutex::new(None);

#[cfg(all(test, not(target_arch = "wasm32")))]
fn install_drain_post_pin_pre_stop_hook_for_test(
    actor_id: u64,
    entered: std::sync::Arc<std::sync::Barrier>,
    release: std::sync::Arc<std::sync::Barrier>,
) -> RegistrationRetirementHookGuard {
    install_registration_retirement_hook(&DRAIN_POST_PIN_PRE_STOP_HOOK, actor_id, entered, release)
}

// The second drain lifetime boundary is the handoff from a quiescent state
// observation to cleanup preparation and the LIVE_ACTORS retirement claim.
// This hook pauses after the exact actor has been pinned and its state read,
// but before cleanup first dereferences it.
#[cfg(all(test, not(target_arch = "wasm32")))]
static DRAIN_POST_STATE_PRE_CLEANUP_HOOK: Mutex<Option<RegistrationRetirementHook>> =
    Mutex::new(None);

#[cfg(all(test, not(target_arch = "wasm32")))]
fn install_drain_post_state_pre_cleanup_hook_for_test(
    actor_id: u64,
    entered: std::sync::Arc<std::sync::Barrier>,
    release: std::sync::Arc<std::sync::Barrier>,
) -> RegistrationRetirementHookGuard {
    install_registration_retirement_hook(
        &DRAIN_POST_STATE_PRE_CLEANUP_HOOK,
        actor_id,
        entered,
        release,
    )
}

// ── cleanup_all_actors post-prepare rendezvous hook (test-only) ───────────
//
// Fires inside `cleanup_all_actors` for each actor, AFTER
// `prepare_quiescent_actor_for_cleanup` runs and BEFORE the Idle→Stopped
// wake-proofing latch. A test uses this point to simulate a concurrent
// by-ID send that CAS-es `Idle→Runnable` in the latch window, proving that
// the latch-fail path (actor skipped / leaked) fires instead of a UAF
// finalize-under-queued-actor.
#[cfg(all(test, not(target_arch = "wasm32")))]
static CLEANUP_POST_PREPARE_HOOK: Mutex<Option<fn(*mut HewActor)>> = Mutex::new(None);

#[cfg(all(test, not(target_arch = "wasm32")))]
fn set_cleanup_post_prepare_hook_for_test(hook: Option<fn(*mut HewActor)>) {
    let mut guard = CLEANUP_POST_PREPARE_HOOK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    *guard = hook;
}

#[cfg(all(test, not(target_arch = "wasm32")))]
fn run_cleanup_post_prepare_hook(actor: *mut HewActor) {
    let hook = {
        let guard = CLEANUP_POST_PREPARE_HOOK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        *guard
    };
    if let Some(hook) = hook {
        hook(actor);
    }
}

// ── Thread-local local ask error ────────────────────────────────────────

thread_local! {
    /// Error discriminant for the most recent `hew_actor_ask` /
    /// `hew_actor_ask_timeout` / `hew_actor_ask_by_id` call on this thread.
    ///
    /// Set to an [`AskError`] value on every NULL return; reset to
    /// `AskError::None` on every non-NULL return and after being read via
    /// `hew_actor_ask_take_last_error`.
    static LAST_ACTOR_ASK_ERROR: Cell<i32> = const { Cell::new(AskError::None as i32) };
}

/// Write `err` to the local-ask error slot and return `null`.
#[inline]
fn actor_ask_null(err: AskError) -> *mut c_void {
    record_ask_error(err);
    ptr::null_mut()
}

/// Record `err` in the local-ask error slot without producing a null reply.
///
/// The with-channel ask family's counterpart of [`actor_ask_null`]: those
/// entry points return a `HewError` code rather than a reply pointer, but
/// codegen's Err-binding reads the SAME TLS slot
/// (`hew_actor_ask_take_last_error`) to classify the failure. A synchronous
/// refusal that returns its code without writing this slot surfaces as
/// `Err(AskError::NoError)` — a genuine failure classified as "no error at
/// all" (dogfood F1, mechanism 2). Every refuse/failure path in the ask
/// family must therefore record a real kind before returning its code.
#[inline]
#[cfg_attr(
    target_arch = "wasm32",
    allow(
        dead_code,
        reason = "the ask family reaches wasm32 with the actor core"
    )
)]
pub(crate) fn record_ask_error(err: AskError) {
    LAST_ACTOR_ASK_ERROR.with(|c| c.set(err as i32));
}

/// Refuse an ask closed with `AskError::ActorStopped` and return null —
/// the blocking stable-role ask's classified-refusal surface
/// (`hew_supervisor_role_ask` records the slot-state diagnostic separately
/// via the error slot; this binds the user-visible `Err(AskError::*)`).
#[cfg(not(target_arch = "wasm32"))]
#[inline]
pub(crate) fn actor_ask_null_actor_stopped() -> *mut c_void {
    actor_ask_null(AskError::ActorStopped)
}

/// Clear the local-ask error slot (called on successful ask return).
#[inline]
fn actor_ask_clear() {
    LAST_ACTOR_ASK_ERROR.with(|c| c.set(AskError::None as i32));
}

/// Read and clear the last local ask error for the current thread.
///
/// Intended to be called by `hew_node.rs` when bridging a local delegation
/// error into the node error slot, without exposing the slot directly.
pub(crate) fn actor_ask_take_last_error_raw() -> i32 {
    LAST_ACTOR_ASK_ERROR.with(|c| {
        let v = c.get();
        c.set(AskError::None as i32);
        v
    })
}

/// Map a send-side [`HewError`] code to its [`AskError`] discriminant.
///
/// Only `ErrMailboxFull` has a dedicated ask-error discriminant.  All other
/// failure codes mean the actor is unreachable and map to `ActorStopped`.
/// The WASM ask path normalises `ErrClosed` → `ErrActorStopped` before
/// calling this function, so `ErrClosed` never reaches the `_` arm in
/// practice.
#[inline]
fn send_err_to_ask_err(code: i32) -> AskError {
    const FULL: i32 = HewError::ErrMailboxFull as i32;
    match code {
        FULL => AskError::MailboxFull,
        // JUSTIFIED: `ErrActorStopped` (-2) is the normal "unreachable" code.
        // `ErrOom` (-5) has no dedicated ask-error discriminant — OOM is a
        // fatal system condition and callers cannot usefully retry.  `ErrClosed`
        // (-4) is normalised to `ErrActorStopped` by the WASM ask path before
        // reaching here, but would also be correctly subsumed.  Any future
        // unknown code is similarly "actor unreachable" — the only actionable
        // send-side distinction for callers is `MailboxFull` vs `ActorStopped`.
        _ => AskError::ActorStopped,
    }
}

/// Read and clear the last local ask error discriminant for the current thread.
///
/// Returns one of the [`AskError`] values as an `i32`.  The slot is reset to
/// `AskError::None` (0) after each call, so repeated calls without an
/// intervening failed ask return 0.
///
/// Call this immediately after `hew_actor_ask` or `hew_actor_ask_timeout`
/// returns `NULL` to distinguish the failure reason:
///
/// - `0` (`None`): the ask succeeded (non-null reply) or returned a
///   legitimate null reply; no error.
/// - `5` (`Timeout`): deadline elapsed before the handler replied.
/// - `9` (`ActorStopped`): the target actor was stopped, the mailbox was
///   closed (actor not found), or message-node allocation failed (OOM) —
///   all cases where the send could not be delivered and retry is not useful.
/// - `10` (`MailboxFull`): bounded mailbox was at capacity.
/// - `11` (`OrphanedAsk`): send succeeded but the actor's mailbox was torn
///   down before the handler called `hew_reply`.
/// - `12` (`NoRunnableWork`): WASM cooperative path only — no runnable work
///   remains, so the ask loop cannot make further progress.
#[no_mangle]
pub extern "C" fn hew_actor_ask_take_last_error() -> i32 {
    actor_ask_take_last_error_raw()
}

// ── Compiler-injected actor-state lock substrate ─────────────────────────

/// Runtime ABI return code for successful actor-state lock operations.
pub const HEW_ACTOR_STATE_LOCK_OK: c_int = 0;
/// Runtime ABI return code for failed actor-state lock operations.
pub const HEW_ACTOR_STATE_LOCK_ERR: c_int = -1;

#[derive(Debug, Default)]
struct ActorStateLockState {
    held: bool,
    owner: Option<ThreadId>,
    poisoned: bool,
}

#[derive(Debug, Default)]
struct ActorStateLock {
    state: Mutex<ActorStateLockState>,
    available: Condvar,
}

fn actor_state_locks() -> &'static Mutex<HashMap<usize, std::sync::Arc<ActorStateLock>>> {
    static LOCKS: OnceLock<Mutex<HashMap<usize, std::sync::Arc<ActorStateLock>>>> = OnceLock::new();
    LOCKS.get_or_init(|| Mutex::new(HashMap::new()))
}

fn recover_runtime_mutex<T>(
    err: PoisonError<std::sync::MutexGuard<'_, T>>,
) -> std::sync::MutexGuard<'_, T> {
    err.into_inner()
}

fn lookup_actor_state_lock(actor: *mut HewActor) -> Option<std::sync::Arc<ActorStateLock>> {
    let locks = actor_state_locks()
        .lock()
        .unwrap_or_else(recover_runtime_mutex);
    locks.get(&(actor as usize)).cloned()
}

pub(crate) fn actor_state_lock_seat(
    actor: *mut HewActor,
) -> *mut crate::execution_context::HewActorStateLockState {
    #[cfg(test)]
    {
        let mut locks = actor_state_locks()
            .lock()
            .unwrap_or_else(recover_runtime_mutex);
        locks.entry(actor as usize).or_default();
        locks
            .get(&(actor as usize))
            .map_or(ptr::null_mut(), |lock| {
                std::sync::Arc::as_ptr(lock).cast_mut().cast()
            })
    }
    #[cfg(not(test))]
    lookup_actor_state_lock(actor).map_or(ptr::null_mut(), |lock| {
        std::sync::Arc::as_ptr(&lock).cast_mut().cast()
    })
}

fn register_actor_state_lock(actor: *mut HewActor) {
    let mut locks = actor_state_locks()
        .lock()
        .unwrap_or_else(recover_runtime_mutex);
    locks.insert(actor as usize, std::sync::Arc::default());
}

fn unregister_actor_state_lock(actor: *mut HewActor) {
    let mut locks = actor_state_locks()
        .lock()
        .unwrap_or_else(recover_runtime_mutex);
    locks.remove(&(actor as usize));
}

fn acquire_actor_state_lock_ref(lock: &ActorStateLock) -> c_int {
    let current = std::thread::current().id();
    let mut state = lock.state.lock().unwrap_or_else(recover_runtime_mutex);
    loop {
        if state.poisoned {
            crate::set_last_error("actor-state lock acquire: lock poisoned by prior handler panic");
            return HEW_ACTOR_STATE_LOCK_ERR;
        }
        if !state.held {
            state.held = true;
            state.owner = Some(current);
            return HEW_ACTOR_STATE_LOCK_OK;
        }
        if state.owner == Some(current) {
            crate::set_last_error("actor-state lock acquire: lock already held by this dispatch");
            return HEW_ACTOR_STATE_LOCK_ERR;
        }
        state = lock
            .available
            .wait(state)
            .unwrap_or_else(recover_runtime_mutex);
    }
}

fn release_actor_state_lock_ref(lock: &ActorStateLock) -> c_int {
    let current = std::thread::current().id();
    let mut state = lock.state.lock().unwrap_or_else(recover_runtime_mutex);
    if !state.held {
        crate::set_last_error("actor-state lock release: lock is not held");
        return HEW_ACTOR_STATE_LOCK_ERR;
    }
    if state.owner != Some(current) {
        crate::set_last_error("actor-state lock release: lock held by another dispatch thread");
        return HEW_ACTOR_STATE_LOCK_ERR;
    }

    state.held = false;
    state.owner = None;
    drop(state);
    lock.available.notify_one();
    HEW_ACTOR_STATE_LOCK_OK
}

/// Test-only: observe whether an actor's registered state lock is currently
/// held. Used by the scheduler suspend-edge test to assert the per-actor lock is
/// RELEASED across the suspend edge (a suspended actor must hold no lock against
/// senders — R2 P0). Returns `None` when no lock is registered for `actor`.
#[cfg(all(test, not(target_arch = "wasm32")))]
pub(crate) fn actor_state_lock_is_held_for_test(actor: *mut HewActor) -> Option<bool> {
    let lock = lookup_actor_state_lock(actor)?;
    let state = lock.state.lock().unwrap_or_else(recover_runtime_mutex);
    Some(state.held)
}

/// Acquire the compiler-owned actor-state lock for `actor`.
///
/// Generated dispatch wrappers call this before entering a receive handler
/// body. The lock is actor-lifetime state stored in a runtime sidecar so the
/// `repr(C)` actor layout stays stable.
///
/// # Safety
///
/// `actor` must be a valid pointer returned by a Hew actor spawn function.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_state_lock_acquire(actor: *mut HewActor) -> c_int {
    cabi_guard!(actor.is_null(), HEW_ACTOR_STATE_LOCK_ERR);

    let Some(lock) = lookup_actor_state_lock(actor) else {
        crate::set_last_error("actor-state lock acquire: actor has no registered state lock");
        return HEW_ACTOR_STATE_LOCK_ERR;
    };
    acquire_actor_state_lock_ref(&lock)
}

/// Release the compiler-owned actor-state lock after normal handler return.
///
/// # Safety
///
/// `actor` must be valid and the current thread must hold its actor-state lock.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_state_lock_release(actor: *mut HewActor) -> c_int {
    cabi_guard!(actor.is_null(), HEW_ACTOR_STATE_LOCK_ERR);

    let Some(lock) = lookup_actor_state_lock(actor) else {
        crate::set_last_error("actor-state lock release: actor has no registered state lock");
        return HEW_ACTOR_STATE_LOCK_ERR;
    };
    release_actor_state_lock_ref(&lock)
}

pub(crate) unsafe fn hew_actor_state_lock_acquire_for_context(
    ctx: *mut HewExecutionContext,
) -> c_int {
    if ctx.is_null() {
        crate::set_last_error("actor-state lock acquire: execution context is null");
        return HEW_ACTOR_STATE_LOCK_ERR;
    }
    // SAFETY: `ctx` is non-null and points to the scheduler-owned dispatch context.
    let seat = unsafe { (*ctx).lock_seat };
    if seat.is_null() {
        crate::set_last_error("actor-state lock acquire: ctx lock_seat is null");
        return HEW_ACTOR_STATE_LOCK_ERR;
    }
    // SAFETY: scheduler obtained `lock_seat` from `actor_state_lock_seat`, which
    // casts the live sidecar `ActorStateLock` allocation to the opaque ctx type.
    let lock = unsafe { &*seat.cast::<ActorStateLock>() };
    acquire_actor_state_lock_ref(lock)
}

pub(crate) unsafe fn hew_actor_state_lock_release_for_context(
    ctx: *mut HewExecutionContext,
) -> c_int {
    if ctx.is_null() {
        crate::set_last_error("actor-state lock release: execution context is null");
        return HEW_ACTOR_STATE_LOCK_ERR;
    }
    // SAFETY: `ctx` is non-null and points to the scheduler-owned dispatch context.
    let seat = unsafe { (*ctx).lock_seat };
    if seat.is_null() {
        crate::set_last_error("actor-state lock release: ctx lock_seat is null");
        return HEW_ACTOR_STATE_LOCK_ERR;
    }
    // SAFETY: scheduler obtained `lock_seat` from `actor_state_lock_seat`, which
    // casts the live sidecar `ActorStateLock` allocation to the opaque ctx type.
    let lock = unsafe { &*seat.cast::<ActorStateLock>() };
    release_actor_state_lock_ref(lock)
}

unsafe fn actor_state_lock_release_after_panic_impl(actor: *mut HewActor, poison: bool) -> c_int {
    if actor.is_null() {
        return HEW_ACTOR_STATE_LOCK_OK;
    }
    let Some(lock) = lookup_actor_state_lock(actor) else {
        return HEW_ACTOR_STATE_LOCK_OK;
    };

    let current = std::thread::current().id();
    let mut state = lock.state.lock().unwrap_or_else(recover_runtime_mutex);
    if !state.held {
        return HEW_ACTOR_STATE_LOCK_OK;
    }
    if state.owner != Some(current) {
        crate::set_last_error(
            "actor-state lock release-after-panic: lock held by another dispatch thread",
        );
        return HEW_ACTOR_STATE_LOCK_ERR;
    }

    state.held = false;
    state.owner = None;
    state.poisoned |= poison;
    drop(state);
    lock.available.notify_one();
    HEW_ACTOR_STATE_LOCK_OK
}

/// Release the actor-state lock from a runtime crash-recovery edge.
///
/// This path deliberately does not poison the replacement actor's substrate:
/// signal recovery may bypass generated cleanup frames, and supervisor restart
/// must observe the child as replaceable instead of deadlocking on an orphaned
/// lock.
///
/// # Safety
///
/// `actor` may be null. If non-null, it must be a valid actor pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_state_lock_release_after_panic(actor: *mut HewActor) -> c_int {
    // SAFETY: this extern entry point forwards its documented raw-pointer
    // contract to the shared implementation.
    unsafe { actor_state_lock_release_after_panic_impl(actor, false) }
}

/// Mark the actor-state lock poisoned and release it after a Rust panic in a
/// generated handler wrapper that did not go through supervisor crash recovery.
///
/// # Safety
///
/// `actor` may be null. If non-null, it must be a valid actor pointer.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_state_lock_poison_after_panic(actor: *mut HewActor) -> c_int {
    // SAFETY: this extern entry point forwards its documented raw-pointer
    // contract to the shared implementation.
    unsafe { actor_state_lock_release_after_panic_impl(actor, true) }
}

#[cfg(all(test, not(target_arch = "wasm32")))]
thread_local! {
    static FAIL_ACTOR_STATE_ALLOC_ON_NTH: Cell<usize> = const { Cell::new(usize::MAX) };
}

#[cfg(all(test, not(target_arch = "wasm32")))]
struct ActorStateAllocFailureGuard;

#[cfg(all(test, not(target_arch = "wasm32")))]
impl Drop for ActorStateAllocFailureGuard {
    fn drop(&mut self) {
        FAIL_ACTOR_STATE_ALLOC_ON_NTH.with(|slot| slot.set(usize::MAX));
    }
}

#[cfg(all(test, not(target_arch = "wasm32")))]
fn fail_actor_state_alloc_on_nth(n: usize) -> ActorStateAllocFailureGuard {
    FAIL_ACTOR_STATE_ALLOC_ON_NTH.with(|slot| slot.set(n));
    ActorStateAllocFailureGuard
}

#[cfg(all(test, not(target_arch = "wasm32")))]
fn should_fail_actor_state_alloc() -> bool {
    FAIL_ACTOR_STATE_ALLOC_ON_NTH.with(|slot| {
        let remaining = slot.get();
        if remaining == usize::MAX {
            return false;
        }
        if remaining == 0 {
            slot.set(usize::MAX);
            return true;
        }
        slot.set(remaining - 1);
        false
    })
}

// Thread-local one-shot flag: when set, the next `alloc_actor_arena` call
// returns null to simulate an OOM on the arena allocation step.
// Reset to `false` automatically by `ArenaAllocFailureGuard::drop`.
#[cfg(all(test, not(target_arch = "wasm32")))]
thread_local! {
    static FAIL_ARENA_ALLOC_NEXT: Cell<bool> = const { Cell::new(false) };
}

#[cfg(all(test, not(target_arch = "wasm32")))]
struct ArenaAllocFailureGuard;

#[cfg(all(test, not(target_arch = "wasm32")))]
impl Drop for ArenaAllocFailureGuard {
    fn drop(&mut self) {
        FAIL_ARENA_ALLOC_NEXT.with(|slot| slot.set(false));
    }
}

/// Arm the arena-alloc failure injection.  Returns a guard that disarms it on
/// drop so the hook cannot leak across test boundaries.
#[cfg(all(test, not(target_arch = "wasm32")))]
fn fail_arena_alloc_next() -> ArenaAllocFailureGuard {
    FAIL_ARENA_ALLOC_NEXT.with(|slot| slot.set(true));
    ArenaAllocFailureGuard
}

#[cfg(all(test, not(target_arch = "wasm32")))]
fn should_fail_arena_alloc() -> bool {
    FAIL_ARENA_ALLOC_NEXT.with(|slot| {
        if slot.get() {
            slot.set(false);
            true
        } else {
            false
        }
    })
}

#[cfg(all(test, not(target_arch = "wasm32")))]
thread_local! {
    static NEXT_SPAWN_ACTOR_ID_OVERRIDE: Cell<Option<(u64, u64)>> = const { Cell::new(None) };
}

/// Force the next spawn to adopt `actor_id` as its packed `id` while keeping the
/// full serial equal to it — the common case where aliasing is not under test.
#[cfg(all(test, not(target_arch = "wasm32")))]
fn override_next_spawn_actor_id(actor_id: u64) {
    NEXT_SPAWN_ACTOR_ID_OVERRIDE.with(|slot| slot.set(Some((actor_id, actor_id))));
}

/// Force the next spawn to adopt `actor_id` as its packed `id` but a DISTINCT
/// full `serial`. Fabricates the masked-`id` alias shape: two incarnations that
/// collide on `id` yet differ on the aliasing-proof discriminator. Used by the
/// supervisor role-ask alias tooth (`supervisor.rs`).
#[cfg(all(test, not(target_arch = "wasm32")))]
pub(crate) fn override_next_spawn_actor_identity(actor_id: u64, serial: u64) {
    NEXT_SPAWN_ACTOR_ID_OVERRIDE.with(|slot| slot.set(Some((actor_id, serial))));
}

#[cfg(all(test, not(target_arch = "wasm32")))]
type SpawnPublicationHook = Option<(
    std::sync::Arc<std::sync::Barrier>,
    std::sync::Arc<std::sync::Barrier>,
)>;

#[cfg(all(test, not(target_arch = "wasm32")))]
static SPAWN_PUBLICATION_HOOK: Mutex<SpawnPublicationHook> = Mutex::new(None);

#[cfg(all(test, not(target_arch = "wasm32")))]
fn run_spawn_publication_hook() {
    let hook = SPAWN_PUBLICATION_HOOK
        .lock()
        .unwrap_or_else(PoisonError::into_inner)
        .clone();
    if let Some((entered, release)) = hook {
        entered.wait();
        release.wait();
    }
}

/// Derive the current actor's ID from an execution context pointer.
///
/// Returns -1 when the context is null or carries no actor.
fn actor_id_from_context(ctx: *mut crate::execution_context::HewExecutionContext) -> i64 {
    if ctx.is_null() {
        return -1;
    }
    // SAFETY: a non-null canonical context points to a live context slot owned
    // by the current dispatch/scope boundary.
    let actor = unsafe { (*ctx).actor };
    if actor.is_null() {
        return -1;
    }
    #[expect(clippy::cast_possible_wrap, reason = "actor IDs fit in i64")]
    {
        // SAFETY: actor is non-null and valid when installed by the scheduler.
        unsafe { &*actor }.id as i64
    }
}

/// Get the ID of the actor currently being dispatched on this thread.
///
/// Returns -1 if no actor is active (called from main or non-actor context).
/// When no execution context is installed, records the diagnostic
/// `EXECUTION_CONTEXT_NOT_INSTALLED` in the generic last-error slot — callers
/// treating an absent context as a failure rely on that write.
#[no_mangle]
pub extern "C" fn hew_actor_current_id() -> i64 {
    let ctx = crate::execution_context::require_current_context();
    actor_id_from_context(ctx)
}

/// Silent variant of [`hew_actor_current_id`]: returns the current actor id,
/// or -1 outside any actor context, WITHOUT writing the generic `LAST_ERROR`
/// slot when no execution context is installed. Use this for identity-routing
/// decisions (e.g. the `parse_error_slot` non-actor fallback) where "no actor"
/// is an expected, non-error condition — not for paths where an absent context
/// is itself a diagnosable failure.
pub(crate) fn hew_actor_current_id_silent() -> i64 {
    let ctx = crate::execution_context::current_context();
    actor_id_from_context(ctx)
}

/// Default message processing budget per activation.
pub const HEW_MSG_BUDGET: i32 = 256;

/// Default reduction budget per dispatch call.
///
/// This is the number of "reduction points" (loop iterations, function
/// calls) an actor can execute within a single message dispatch before
/// it yields. 4000 is roughly similar to Erlang's default of 4000
/// reductions.
pub const HEW_DEFAULT_REDUCTIONS: i32 = 4000;

/// Maximum number of workers the scheduler supports.
pub const HEW_MAX_WORKERS: usize = 256;

/// Priority levels for actor scheduling.
pub const HEW_PRIORITY_HIGH: i32 = 0;
/// Normal priority (default).
pub const HEW_PRIORITY_NORMAL: i32 = 1;
/// Low priority.
pub const HEW_PRIORITY_LOW: i32 = 2;

// ── Actor struct ────────────────────────────────────────────────────────

/// Codegen-emitted deep-clone function for actor initial-state templates.
///
/// Called by [`crate::supervisor`] when restarting a child actor to produce a
/// fresh, independently-owned copy of the supervisor's `init_state` template.
/// Mirror of `state_drop_fn` for the restart path.
///
/// **Contract**:
/// - `src` points to a valid wrapper of the actor's state type
///   (`init_state_size` bytes).
/// - Returns a freshly heap-allocated wrapper (a sized-block allocation the
///   runtime releases with `buf_free`) whose owned heap fields
///   (`Vec`, `String`, IO handles…) are independent deep clones — no byte
///   aliasing with `src`.
/// - Returns `NULL` on allocation failure. The supervisor treats null as
///   "restart blocked": the new child slot is left null, the circuit-breaker
///   success counter is NOT advanced, and the failure propagates back to the
///   restart-budget machinery which will escalate / back off as usual.
///
/// **`extern "C-unwind"`** rather than plain `extern "C"` so that a panic
/// from a generated impl-Drop / impl-Clone helper can unwind through the FFI
/// boundary into the actor-level `catch_unwind` guards the runtime already
/// installs. Plain `extern "C"` (used by `state_drop_fn`) cannot legally
/// unwind. A clone function that allocates is more likely to OOM-panic than a
/// drop function that releases.
pub type HewStateCloneFn = unsafe extern "C-unwind" fn(*const c_void) -> *mut c_void;

/// Payload ownership required by the registered generated dispatch adapter.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum HewDispatchOwnership {
    #[default]
    CopiedPayload,
    UniqueEnvelope,
}

/// Actor struct layout. MUST match the C definition exactly.
///
/// The `sched_link_next` field (intrusive MPSC next pointer) MUST be the
/// first field so that `*mut HewActor` can be cast to/from `*mut MpscNode`.
#[repr(C)]
pub struct HewActor {
    /// Intrusive MPSC node for the global scheduler queue.
    pub sched_link_next: AtomicPtr<HewActor>,

    /// Unique, monotonically increasing actor ID.
    ///
    /// This is the location-transparent PID: `(node_id << 48) | serial`.
    /// All runtime APIs that take or return a "pid" operate on this value.
    pub id: u64,

    /// Actor-owned mutable state.
    pub state: *mut c_void,

    /// Size of the state allocation.
    pub state_size: usize,

    /// Dispatch function for APPLICATION messages (context-leading canonical
    /// signature). Reached only by nodes dequeued with
    /// [`crate::mailbox_header::Origin::User`].
    pub dispatch: Option<HewDispatchFn>,

    /// Pointer to the actor's mailbox.
    ///
    /// Typed as `*mut c_void` to avoid circular module dependencies;
    /// the scheduler casts to `*mut HewMailbox` when processing messages.
    pub mailbox: *mut c_void,

    /// Current lifecycle state (CAS transitions).
    pub actor_state: AtomicI32,

    /// Messages to process per activation.
    pub budget: AtomicI32,

    /// Saved initial state for supervisor restart (deep copy).
    pub init_state: *mut c_void,

    /// Size of the initial state.
    pub init_state_size: usize,

    /// Optional coalesce key function for message coalescing.
    pub coalesce_key_fn: Option<unsafe extern "C" fn(i32, *mut c_void, usize) -> u64>,

    /// Optional cleanup function called when the actor is freed.
    /// Generated from the actor's `#[on(stop)]` lifecycle hooks: codegen
    /// concatenates every hook body (in lexical declaration order) into
    /// a single synthetic `_terminate` symbol so the runtime ABI stays
    /// one C function pointer. See HEW-SPEC-2026 §9.1.2.
    pub terminate_fn: Option<unsafe extern "C-unwind" fn(*mut c_void)>,

    /// Optional state-drop function that runs `impl Drop` callbacks on every
    /// owned field of the actor's live state immediately before
    /// `crate::mem::buf_free(a.state)`. Generated unconditionally for every actor by
    /// codegen, even when the body is empty (no owned fields). Wired
    /// at spawn time via [`hew_actor_set_state_drop`]. Distinct from
    /// `terminate_fn`: terminate runs the user's `#[on(stop)]` hooks while
    /// the actor is still RUNNING; state-drop runs unconditionally after
    /// terminate has finished, immediately before the state allocation is
    /// freed, so that types implementing `Drop` (Vec, String, IO handles)
    /// release their resources rather than being raw-freed.
    pub state_drop_fn: Option<unsafe extern "C" fn(*mut c_void)>,

    /// Optional state-clone function that deep-clones the actor's initial-state
    /// template before a supervisor restart spawns a fresh actor. Mirror of
    /// [`state_drop_fn`]. Generated by codegen (Lane A2) when the actor's state
    /// contains any owned heap field (e.g. `Vec`, `String`, `IO handle`).
    /// Wired at spawn time via [`hew_actor_set_state_clone`] and forwarded by
    /// the supervisor's `restart_child_from_spec`.
    ///
    /// **Calling contract**: `clone_fn(src)` reads the wrapper at `src` and
    /// returns a freshly heap-allocated wrapper whose owned fields are
    /// independent deep clones. Returns null on allocation failure; on null
    /// the supervisor blocks the restart attempt (does not record success;
    /// does not spawn a new actor). See [`HewStateCloneFn`].
    ///
    /// **C1 fix**: prior runtimes byte-copied `spec.init_state` into every
    /// spawned actor, causing the spec's wrapper to byte-alias the actor's
    /// owned heap pointers. Once the actor mutated/reallocated those fields,
    /// `spec.init_state` carried dangling pointers; the next restart's
    /// byte-copy propagated the dangle to the new actor. With
    /// `state_clone_fn` registered, `spec.init_state` is converted to an
    /// independently-owned template at registration time (see
    /// `hew_supervisor_set_child_state_clone`) and every restart deep-clones
    /// from that template.
    ///
    /// Stored on `HewActor` for symmetry with `state_drop_fn` and as a hook
    /// for future direct-spawn restart consumers (outside supervisor).
    pub state_clone_fn: Option<HewStateCloneFn>,

    /// Guard flag ensuring the terminate callback runs exactly once.
    pub terminate_called: AtomicBool,

    /// Set to `true` after the terminate callback returns (or was skipped).
    /// Free paths wait on this to avoid freeing state while terminate
    /// is still running on another thread.
    pub terminate_finished: AtomicBool,

    /// `true` while a scheduler worker owns this actor's activation, from the
    /// moment it wins the `Runnable -> Running` CAS in `activate_actor` until it
    /// leaves the activation (settle / suspend / crash-break). Distinct from the
    /// `Running` lifecycle state because an external `hew_actor_trap` CAS-es the
    /// actor straight to the quiescent `Crashed`/`Stopped` state *out from under*
    /// the owning worker, erasing the `Running` marker while the worker is still
    /// in its dispatch/settle critical section (reading `a.actor_state`, the
    /// arena, the mailbox). `hew_actor_free` treats `Crashed`/`Stopped` as
    /// quiescent and would reclaim the box+mailbox in that window — a
    /// use-after-free. The free path waits on this flag (in addition to the
    /// terminal state) so it never frees under an in-flight activation, no matter
    /// which thread published the terminal state. Native-only: the WASM
    /// scheduler is cooperative single-threaded, so no concurrent free can race
    /// an activation.
    pub dispatch_active: AtomicBool,

    /// Error code set by `hew_actor_trap` (0 = no error).
    pub error_code: AtomicI32,

    /// Back-pointer to the supervising [`HewSupervisor`] (null if unsupervised).
    pub supervisor: *mut c_void,

    /// Index of this actor within its supervisor's child array.
    pub supervisor_child_index: i32,

    // ── Priority scheduling ─────────────────────────────────────────────
    /// Scheduling priority: 0 = high, 1 = normal (default), 2 = low.
    ///
    /// Higher-priority actors get their message budget multiplied,
    /// allowing them to process more messages per activation.
    pub priority: AtomicI32,

    // ── Reduction-based preemption ────────────────────────────────────
    /// Remaining reduction budget for the current dispatch. Decremented
    /// at compiler-inserted yield points. When it reaches 0 the actor
    /// yields control back to the scheduler.
    pub reductions: AtomicI32,

    // ── Hibernation ─────────────────────────────────────────────────────
    /// Number of consecutive activations with zero messages.
    /// When this reaches `hibernation_threshold`, the actor is
    /// considered hibernating and its arena may be freed.
    pub idle_count: AtomicI32,

    /// Number of consecutive idle activations before hibernation.
    /// 0 disables hibernation (default).
    pub hibernation_threshold: AtomicI32,

    /// Whether the actor is currently hibernating.
    /// Set to 1 when `idle_count` >= `hibernation_threshold`.
    pub hibernating: AtomicI32,

    // ── Profiler stats (appended at end to preserve C ABI layout) ────
    /// Total messages dispatched to this actor.
    pub prof_messages_processed: AtomicU64,

    /// Cumulative nanoseconds spent in dispatch for this actor.
    pub prof_processing_time_ns: AtomicU64,

    /// Per-actor arena bump allocator. Installed in the dispatch context so
    /// `hew_arena_malloc` routes through it. Reset after each activation.
    pub arena: *mut crate::arena::ActorArena,

    // ── Slice-4 suspend/resume executor (appended to preserve C ABI layout) ──
    //
    // These two fields are NOT codegen-mirrored: only `id` and `state` have
    // offset literals in `hew-codegen-rs/src/llvm.rs`, and both precede this
    // append point, so adding these fields keeps every mirrored offset fixed
    // (verified by `abi_offset_parity`). They are appended at the very end of
    // the struct, mirroring how `prof_*` and `arena` were appended.
    /// The continuation handle parked on this actor while it is `Suspended`,
    /// or null when no dispatch is suspended.
    ///
    /// SINGLE-OWNER CONTRACT (FG1): the executor is the sole owner of this
    /// handle's teardown. It is written exactly once per suspend (the FG3
    /// two-phase park stores it after publishing the `Resuming`→`Parked`
    /// intent), read by the resume re-entry, and nulled in the SAME critical
    /// section as the `ContTag::Destroyed` transition so no later activation
    /// dereferences a destroyed frame (FG4: no use-after-destroy).
    pub suspended_cont: AtomicPtr<c_void>,

    /// The [`crate::internal::types::ContTag`] lifecycle tag (as a raw i32)
    /// serializing resume vs destroy on [`Self::suspended_cont`] (FG1/FG2/FG4).
    ///
    /// Zero-init is `ContTag::Empty`. The executor CAS-transitions it; an
    /// unexpected current tag fails closed (the operation refuses) rather than
    /// double-resuming or double-destroying. Because the per-actor state lock
    /// is released while `Suspended`, THIS tag — not the lock — is the
    /// serialization point for the handle's lifecycle.
    pub cont_tag: AtomicI32,

    /// FG3 two-phase park: a wake (`enqueue_resume`) that fires in the window
    /// between the suspend returning to the executor and the park completing
    /// sets this flag instead of being lost. The executor re-checks it after
    /// publishing the park and, if set, immediately re-enqueues the actor so
    /// the wake is observed exactly once rather than dropped.
    ///
    /// Go's runtime calls this the `pdNil→pdWait→pdReady` race; this flag is
    /// the `pdReady`-arrived-early signal the parker drains.
    pub pending_wake: AtomicBool,

    /// W6.010 value routing: the suspended handler's OWN reply channel (the one
    /// its caller is awaiting), stashed from the dispatch execution context when
    /// the handler parks. A suspendable handler tears down its execution context
    /// on suspend, but its coroutine body still owes a reply to its caller; on
    /// resume the scheduler re-establishes a `HewExecutionContext` carrying this
    /// channel so the body's final-return `hew_reply` (via
    /// `hew_get_reply_channel`) deposits the reply — regardless of whether the
    /// coroutine completes on the trampoline's first poll or on a later
    /// `resume_park`. Null when the handler had no reply channel (a
    /// fire-and-forget handler that suspended) or between dispatches.
    pub suspended_reply_channel: AtomicPtr<c_void>,

    /// Cancel token retained from the execution context that produced the
    /// suspended continuation. The resume edge installs it into the temporary
    /// `HewExecutionContext`, then clears/releases it when the await frame exits.
    pub suspended_cancel_token: AtomicPtr<c_void>,

    // ── Runtime identity (appended to preserve C ABI layout) ────────────────
    //
    // Stamped at spawn with the spawning runtime's id (`build_spawned_actor`
    // reads `rt_current().runtime_id()`). Like `prof_*`, `arena`, and the
    // Slice-4 suspend fields, this is appended at the very end of the struct
    // and is NOT codegen-mirrored: only `id` (offset 8) and `state` (offset
    // 16) have offset literals in `hew-codegen-rs/src/llvm.rs`, and both
    // precede this append point, so adding this field keeps every mirrored
    // offset fixed (verified by `abi_offset_parity`).
    //
    // It is the discriminant the cross-runtime send/ask/by-id check compares
    // against the calling runtime's id, so a held actor pointer from a foreign
    // runtime fails closed without dereferencing a foreign handle. In a
    // single-runtime program every actor carries `RuntimeId::DEFAULT` and the
    // check never fires. A bare `RuntimeId` (a `u64` discriminant) and a
    // non-owning raw pointer — not an `Arc`/handle — are stamped here on purpose:
    // single-runtime actors never outlive their one runtime, and a strong handle
    // field would form a runtime→workers→actors→runtime ownership cycle
    // (`ownership-over-locks`).
    //
    // Typed through `crate::runtime_id` (not `crate::runtime`) so it resolves
    // on wasm too, where the native-only `runtime` module is configured out
    // but this struct is still compiled.
    pub runtime_id: crate::runtime_id::RuntimeId,

    /// Non-owning pointer to the `RuntimeInner` that owns this actor.
    ///
    /// Off-dispatch producers that already hold an actor pointer can enter the
    /// actor's owning runtime through `runtime::enter_actor_runtime` instead of
    /// resolving through the default slot. This does not own or retain the
    /// runtime: `RuntimeInner` owns the live-actor table and cleanup only drops
    /// the runtime after actors/workers are drained, so the runtime outlives every
    /// actor that carries this pointer. Null is reserved for legacy/test actors
    /// and preserves the existing default-runtime fallback.
    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) runtime: *const crate::runtime::RuntimeInner,

    /// Count of in-flight by-ID operations and scheduler queue entries
    /// currently pinning this actor allocation.
    ///
    /// `with_actor_send_by_id` increments this field (atomically, under
    /// `LIVE_ACTORS`) before releasing the registry lock, then decrements it
    /// via a RAII guard when the operation completes. Scheduler producers take
    /// another reference before making an actor Runnable, and the queue
    /// consumer transfers that ownership to `dispatch_active` before releasing
    /// it. The free path in `hew_actor_free_inner` calls `untrack_actor` first
    /// (removing the actor from `LIVE_ACTORS` so no new registry pins can be
    /// taken), then waits for both this count and `dispatch_active` before
    /// finalizing.
    ///
    /// **Why not `dispatch_active`**: `dispatch_active` serialises the
    /// scheduler worker's activation ownership; reusing it for external sends
    /// would conflate two orthogonal notions of "in use".
    ///
    /// **ABI note**: appended at the end of `HewActor` after all previously
    /// appended fields (`prof_*`, `arena`, suspend/resume, `runtime_id`,
    /// `runtime`), so the only codegen-mirrored offsets — `id` at 8 and
    /// `state` at 16 — are unaffected (verified by `abi_offset_parity`).
    pub send_pin_count: AtomicU32,

    /// The `receive gen fn` stream-producer pump's own `Sink<T>*` while its
    /// activation is alive. Registered by
    /// [`hew_actor_gen_sink_register`] in the pump's prologue, cleared by
    /// [`hew_actor_gen_sink_complete`] on a clean (generator-exhausted) exit.
    /// A terminal actor teardown that reaches this actor while the slot is
    /// still non-null — a crash mid-pump (`hew_actor_trap`), or a stop/free
    /// while the pump is parked on backpressure (`hew_actor_free_inner`'s
    /// parked-activation reclaim) — fault-closes the registered sink instead
    /// of leaving the consumer to hang on a silent EOF.
    ///
    /// At most one gen-sink is live per actor at a time: the actor model
    /// runs one pump to completion or park before a second `receive gen fn`
    /// call can start a new activation on the same actor (no concurrent
    /// pump interleaving — out of scope per the receive-gen-fn plan), so a
    /// single slot, not a registry, is the correct shape.
    ///
    /// **ABI note**: appended at the very end of `HewActor`, after
    /// `send_pin_count`, so no previously-mirrored offset moves.
    pub gen_sink: AtomicPtr<c_void>,

    /// Stable process-local identity exposed to Hew values after the atomic
    /// compiler cutover. Appended at the tail so the codegen-mirrored `id` and
    /// `state` offsets and every established prefix offset remain unchanged.
    pub local_pid_id: crate::lifetime::local_handles::HewLocalPidId,

    /// Full, un-masked spawn serial — the aliasing-proof incarnation
    /// discriminator.
    ///
    /// `id` packs only the low 48 bits of the serial (`pid::hew_pid_make`
    /// masks with `SERIAL_MASK`), so two incarnations can in principle collide
    /// on the masked `id`. The two-phase owner-scoped role ask copies this full
    /// serial out under `children_lock`
    /// (`supervisor::role_resolve_current_child_id`) and re-checks it against
    /// the pinned actor before enqueue
    /// (`live_actors::with_actor_send_by_identity`): an aliased `id` pins a
    /// DIFFERENT incarnation whose serial differs, so the submission refuses
    /// closed instead of delivering to the wrong actor.
    ///
    /// [`take_actor_serial`] refuses past `MAX_SPAWN_SERIAL` rather than
    /// wrapping, so the collision is not reachable in production; this field is
    /// what makes wrong-actor delivery unrepresentable at the seam regardless,
    /// and the supervisor alias tooth fabricates the collision to prove it.
    /// Runtime-internal; not mirrored by codegen. Appended at the struct tail so
    /// no codegen-mirrored offset (`id` at 8, `state` at 16) moves.
    pub spawn_serial: u64,

    /// Dispatch entry point for runtime lifecycle signals — the second,
    /// disjoint channel. Reached only by nodes dequeued with
    /// [`crate::mailbox_header::Origin::Sys`], so no application `msg_type`
    /// can express a lifecycle signal and no lifecycle signal can be
    /// mistaken for an application message.
    ///
    /// `None` for actors that declare no `#[on(exit)]` / `#[on(down)]` hook
    /// and are not supervisors: an arriving system signal is then freed with
    /// a diagnostic instead of being routed anywhere (fail-closed).
    ///
    /// **ABI note**: appended at the struct tail, after `spawn_serial`, so no
    /// previously-mirrored offset moves. Registered post-spawn via
    /// [`hew_actor_set_sys_dispatch`], never through the spawn arg list.
    pub sys_dispatch: Option<HewSysDispatchFn>,

    /// Explicit one-shot authority proving the dispatch crash escrow already
    /// invoked (or quarantined after entering) the typed state finalizer.
    /// Lifecycle state alone is insufficient: actors may become `Crashed`
    /// before dispatch opens an escrow or while idle. Only the recovery path
    /// that actually consumes a state snapshot sets this bit.
    pub state_drop_consumed: AtomicBool,

    /// Provenance bit for a shallow state wrapper without typed-field ownership.
    /// The persistent template pins copied wrapper bytes, not external pointees.
    /// Explicitly borrowed fields remain externally owned through reclamation;
    /// fresh init-thunk and state-clone incarnations leave this false. Separate
    /// from `state_drop_consumed` so a successful clone registration can transfer
    /// an implicit initial alias to owned without reviving consumed crash escrow.
    /// Explicit external borrows cannot take that ownership-transfer path.
    pub state_drop_borrowed: AtomicBool,

    /// The reply channel of the `ask` this actor's PARKED activation still
    /// owes, retained INDEPENDENTLY for the shutdown drain gate.
    ///
    /// Distinct from [`Self::suspended_reply_channel`], which holds the MOVED
    /// sender-side reference the resume edge consumes to deposit the reply.
    /// That reference can be released by the resumed body (a deposit into an
    /// already-cancelled channel frees the channel while the slot still holds
    /// the stale pointer), so a foreign thread must never dereference it. This
    /// slot instead owns its own `hew_reply_channel_retain`ed reference, taken
    /// on the suspend edge in the same guarded block as the W6.010 stash, so
    /// the channel allocation is pinned for as long as this slot is non-null.
    ///
    /// The shutdown drain scan (`live_actors::has_drain_blocking_suspended_actor`)
    /// reads it under the live-actors registry lock to decide whether a
    /// `Suspended` actor still represents in-flight work: a suspended handler
    /// whose ask reply channel is `cancelled` was ABANDONED by its caller
    /// (`await … | after d` deadline, task cancel) and must not hold the drain
    /// open; a live ask, or a parked handler with no ask at all, still blocks.
    /// Release sites swap this slot to null UNDER that same registry lock
    /// (`scheduler::release_parked_ask_channel`) before dropping the reference,
    /// so the scan's dereference can never race the free.
    ///
    /// Set on the suspend edge; cleared wherever the parked activation's reply
    /// obligation resolves (resume completion, resume crash, park refusal,
    /// stop-cancel, and every `retire_suspended_reply_channel` route). Null
    /// between dispatches and for actors that never suspend mid-`ask`.
    ///
    /// **ABI note**: appended at the struct tail, after `state_drop_borrowed`,
    /// so no previously-mirrored offset (`id` at 8, `state` at 16) moves.
    pub parked_ask_channel: AtomicPtr<c_void>,
    /// Published with the dispatch callback before the actor becomes visible.
    pub dispatch_ownership: HewDispatchOwnership,

    /// Borrowed invocation state of the active checked handler, protected by
    /// activation ownership. Stop requests cancel and drain this invocation
    /// before its frame can be destroyed. Null between checked turns.
    pub checked_invocation: AtomicPtr<c_void>,
    /// Retained terminal cleanup result for checked native actor observers.
    pub native_completion: Option<std::sync::Arc<crate::actor_native::NativeActorCompletion>>,
    /// An externally requested crash deferred until a parked checked turn has
    /// drained its coroutine-owned state. Zero means no deferred terminal.
    ///
    /// A parked checked turn cannot be made terminal in place: its invocation
    /// owns lexical cleanup and the actor state borrow until cooperative
    /// cancellation completes. The first external trap records its code here,
    /// requests cancellation, and the resumed activation publishes `Crashed`
    /// only after it has cleared [`Self::checked_invocation`].
    ///
    /// Appended at the tail so codegen-mirrored offsets remain stable.
    pub pending_external_trap_code: AtomicI32,
}

// SAFETY: `HewActor` is designed for concurrent access across worker threads.
// All mutable shared fields use atomic types. Raw pointers are managed by the
// scheduler/actor lifecycle, which ensures exclusive activation access (CAS
// `RUNNABLE` → `RUNNING`).
unsafe impl Send for HewActor {}
// SAFETY: Concurrent reads/writes of shared mutable fields use atomics.
// Raw-pointer fields are lifecycle-managed by scheduler CAS transitions.
unsafe impl Sync for HewActor {}

/// Transfer the actor's one-shot typed state-drop authority to a completed
/// dispatch crash escrow.
///
/// The descriptor remains installed for supervisor/restart metadata; this bit
/// belongs to one actor incarnation and is initialized false for every spawn.
/// A second transfer is an invariant violation (it would imply two escrows
/// both believed they owned the same initialized state).
pub(crate) unsafe fn record_dispatch_state_drop_consumed(actor: *mut HewActor) {
    if actor.is_null() {
        eprintln!("fatal: null actor while recording crash-escrow state authority");
        std::process::abort();
    }
    // SAFETY: caller owns the actor activation or terminal recovery edge.
    let already_consumed = unsafe { &*actor }
        .state_drop_consumed
        .swap(true, Ordering::AcqRel);
    if already_consumed {
        eprintln!("fatal: actor state-drop authority consumed by more than one crash escrow");
        std::process::abort();
    }
}

/// Mark a newly-created supervised incarnation as borrowing typed state from
/// its persistent shallow-copy template.
///
/// # Safety
///
/// `actor` must remain live for the write and either be unpublished or already
/// lack typed-drop authority. This must never revoke an owning incarnation's
/// cleanup obligation. A published shallow child is protected by its roster.
// KEEP(wasm32): production caller in supervisor.rs marks a shallow-template
// restart incarnation as borrowing state from the persistent child spec.
// lib.rs gates `pub mod supervisor` behind
// `#[cfg(not(target_arch = "wasm32"))]` while `pub mod actor` is ungated — the
// same asymmetry already annotated elsewhere in this file. The bit it writes,
// `HewActor::state_drop_borrowed`, is READ on both targets.
#[cfg_attr(target_arch = "wasm32", allow(dead_code))]
pub(crate) unsafe fn mark_state_drop_borrowed(actor: *mut HewActor) {
    if actor.is_null() {
        eprintln!("fatal: null actor while recording borrowed state provenance");
        std::process::abort();
    }
    // SAFETY: caller keeps the non-owning actor incarnation live.
    unsafe { &*actor }
        .state_drop_borrowed
        .store(true, Ordering::Release);
}

/// Transfer a shallow-template initial incarnation to independently-owned
/// state after the supervisor successfully replaces its template with a deep
/// clone. This changes provenance only; a concurrently consumed crash-escrow
/// authority remains consumed in the separate atomic bit.
///
/// # Safety
///
/// `actor` must be the live child whose former template alias was just broken.
/// Its fields must be eligible for ownership transfer, never explicitly borrowed
/// from an external owner under the supervisor's borrowed-state contract.
// KEEP(wasm32): production caller is `hew_supervisor_set_child_state_clone` in
// the native-only supervisor module; it flips provenance to owned once the
// template deep-clone breaks the alias. Same cfg asymmetry as
// `mark_state_drop_borrowed`.
#[cfg_attr(target_arch = "wasm32", allow(dead_code))]
pub(crate) unsafe fn mark_state_drop_owned(actor: *mut HewActor) {
    if actor.is_null() {
        eprintln!("fatal: null actor while recording owned state provenance");
        std::process::abort();
    }
    // SAFETY: caller guarantees the template no longer aliases actor state.
    unsafe { &*actor }
        .state_drop_borrowed
        .store(false, Ordering::Release);
}

pub(crate) fn clear_suspended_cancel_token(actor: &HewActor) {
    let token = actor
        .suspended_cancel_token
        .swap(std::ptr::null_mut(), Ordering::AcqRel);
    if !token.is_null() {
        // `task_scope` is native-only, and so is the suspend edge that stashes
        // a token, so nothing on wasm32 writes this slot. The wasm build
        // therefore cannot reach a non-null token, and asserts that rather than
        // silently dropping a retained reference if that ever changes.
        // SAFETY: the actor slot owns a retained task-scope cancellation token.
        #[cfg(not(target_arch = "wasm32"))]
        unsafe {
            crate::cancel_token::hew_cancel_token_release(token.cast());
        }
    }
}

/// Discharge the reply a parked `ask` handler still owes, on whichever target
/// this build is.
///
/// The two schedulers own the slot on their own target and are configured out
/// on the other, but the obligation is one invariant, so target-neutral
/// teardown code -- `cleanup_all_actors`, the free paths -- routes through here
/// rather than repeating the `cfg` split at every call site.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn retire_parked_activation_reply(actor: &HewActor) {
    #[cfg(not(target_arch = "wasm32"))]
    crate::activation::retire_suspended_reply_channel(actor);
}

// ── Codegen-mirrored ABI offsets ────────────────────────────────────────
//
// Codegen (`hew-codegen-rs/src/llvm.rs`) emits raw GEPs into `HewActor` using
// hand-copied byte-offset literals so the compiler backend does not link the
// runtime crate. These `offset_of!`-derived constants are the canonical source
// of truth those literals mirror; the `abi_offset_parity` test in
// `hew-codegen-rs` asserts the codegen literals equal these exports so a field
// reorder (which is how the `state` offset silently drifted 24→16 when
// `HewActor.pid` was removed) fails closed instead of corrupting actor state
// pointers at runtime. Mirror of the `HEW_CTX_OFFSET_*` discipline in
// `execution_context.rs`.

/// Byte offset of [`HewActor::id`].
pub const HEW_ACTOR_OFFSET_ID: usize = std::mem::offset_of!(HewActor, id);
/// Byte offset of [`HewActor::state`].
pub const HEW_ACTOR_OFFSET_STATE: usize = std::mem::offset_of!(HewActor, state);

impl std::fmt::Debug for HewActor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("HewActor")
            .field("id", &self.id)
            .field("actor_state", &self.actor_state)
            .field("budget", &self.budget.load(Ordering::Relaxed))
            .field("arena", &self.arena)
            .finish_non_exhaustive()
    }
}

// ── Spawn options ───────────────────────────────────────────────────────

/// Monotonically increasing actor serial counter.
static NEXT_ACTOR_SERIAL: AtomicU64 = AtomicU64::new(1);

/// The largest serial the spawn allocator will issue.
///
/// Native packs the serial into the low 48 bits of the actor id
/// (`pid::hew_pid_make`), so `pid::MAX_ACTOR_SERIAL` is the last value that
/// survives the pack. WASM stores the raw serial as the id — nothing is packed,
/// so the only unrepresentable value is `0` (the invalid-actor sentinel) and the
/// bound is the last serial short of the `u64` wrap that would reach it.
const MAX_SPAWN_SERIAL: u64 = crate::pid::MAX_ACTOR_SERIAL;

/// Take the next representable actor serial from `counter`, or `None` once the
/// serial space is exhausted.
///
/// The counter STOPS at `MAX_SPAWN_SERIAL + 1` instead of running on: past that
/// point every value it could hand out is unrepresentable, so continuing to
/// increment would only walk toward a `u64` wrap that re-enters the valid range
/// and re-issues ids already live. Refusing is the only outcome that cannot
/// alias.
fn take_actor_serial(counter: &AtomicU64) -> Option<u64> {
    counter
        .fetch_update(Ordering::Relaxed, Ordering::Relaxed, |serial| {
            (serial <= MAX_SPAWN_SERIAL).then_some(serial + 1)
        })
        .ok()
}

// Thread-local one-shot seed for the allocator's counter, mirroring
// `FAIL_ARENA_ALLOC_NEXT`.
//
// WHY: the exhaustion boundary is 2^48 allocations away; a test cannot reach it
// by spawning. Seeding a private counter drives the real `take_actor_serial` at
// the real boundary without mutating the process-global one (which would race
// sibling tests under threaded execution).
// WHEN OBSOLETE: when actor identity stops being a packed 48-bit alias (the
// Stage 3b compiler aggregate migration named in `pid.rs`), the boundary and
// this seam both disappear.
// WHAT THE REAL SOLUTION LOOKS LIKE: a per-runtime serial counter a test can
// construct directly, instead of one process-global static.
#[cfg(test)]
thread_local! {
    static NEXT_ACTOR_SERIAL_SEED: Cell<Option<u64>> = const { Cell::new(None) };
}

/// Seed the next actor-serial allocation on this thread. One-shot.
#[cfg(test)]
fn seed_next_actor_serial(serial: u64) {
    NEXT_ACTOR_SERIAL_SEED.with(|slot| slot.set(Some(serial)));
}

/// The runtime this process spawns into.
///
/// wasm32 has exactly one, so every actor carries the default id; natively the
/// installed runtime names itself.
fn owning_runtime_id() -> crate::runtime_id::RuntimeId {
    #[cfg(not(target_arch = "wasm32"))]
    {
        crate::runtime::rt_current().runtime_id()
    }
    #[cfg(target_arch = "wasm32")]
    {
        crate::runtime_id::RuntimeId::DEFAULT
    }
}

/// Allocate the next actor serial, or `None` when the serial space is exhausted.
fn allocate_actor_serial() -> Option<u64> {
    #[cfg(test)]
    if let Some(seed) = NEXT_ACTOR_SERIAL_SEED.with(Cell::take) {
        return take_actor_serial(&AtomicU64::new(seed));
    }
    take_actor_serial(&NEXT_ACTOR_SERIAL)
}

// PID is now unified with id — actors use location-transparent IDs everywhere.

// ── Live actor tracking (delegated to lifetime::live_actors) ──────────────

const TERMINATE_WAIT_TIMEOUT: std::time::Duration = std::time::Duration::from_secs(5);
const TERMINATE_WAIT_POLL_INTERVAL: std::time::Duration = std::time::Duration::from_millis(1);

#[cfg(test)]
static TERMINATE_WAIT_POLL_TICKS: std::sync::atomic::AtomicUsize =
    std::sync::atomic::AtomicUsize::new(0);

#[cfg(test)]
#[inline]
fn record_terminate_wait_poll_tick() {
    TERMINATE_WAIT_POLL_TICKS.fetch_add(1, Ordering::Relaxed);
}

// live on not(wasm32) — actor_stop/drain wait-loop; dead on wasm32; caller actor.rs:1194
#[cfg_attr(target_arch = "wasm32", allow(dead_code))]
#[cfg(not(test))]
#[inline]
fn record_terminate_wait_poll_tick() {}

/// Check whether an actor ID still maps to the expected live actor pointer.
///
/// Test wrapper around [`live_actors::with_live_actor_by_id`].
#[cfg(test)]
#[cfg_attr(target_arch = "wasm32", allow(dead_code))]
pub(crate) fn with_live_actor_by_id<R>(
    actor_id: u64,
    expected: *mut HewActor,
    f: impl FnOnce(&HewActor) -> R,
) -> Option<R> {
    live_actors::with_live_actor_by_id(actor_id, expected, f)
}

/// Check whether an actor pointer is still live (tracked and not yet freed).
#[cfg_attr(
    not(test),
    allow(
        dead_code,
        reason = "supervisor and actor tests rely on the liveness probe"
    )
)]
pub(crate) fn is_actor_live(actor: *mut HewActor) -> bool {
    live_actors::is_actor_live(actor)
}

/// ABA-proof liveness probe: matches both the actor id and the pointer.
///
/// See [`live_actors::is_actor_live_with_id`]; required for any test that
/// waits for an actor to be released while sibling threads may spawn actors
/// (a recycled allocation address would otherwise probe as live again).
#[cfg_attr(
    not(test),
    allow(
        dead_code,
        reason = "supervisor and actor tests rely on the liveness probe"
    )
)]
pub(crate) fn is_actor_live_with_id(actor_id: u64, expected: *mut HewActor) -> bool {
    live_actors::is_actor_live_with_id(actor_id, expected)
}

/// Stable runtime actor identifier.
pub type ActorId = u64;

mod free;
mod send;
mod spawn;

pub use free::*;
pub use send::*;
pub use spawn::*;

#[cfg(all(feature = "composition-test", not(target_arch = "wasm32")))]
#[allow(
    clippy::wildcard_imports,
    reason = "the feature-gated FFI composition seam mirrors the actor test harness and keeps its lifecycle operations together"
)]
pub mod composition_test_support;

#[cfg(all(test, not(target_arch = "wasm32")))]
mod tests;
#[cfg(all(test, target_arch = "wasm32"))]
mod wasm_tests;
