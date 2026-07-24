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
#[cfg(not(target_arch = "wasm32"))]
use std::collections::HashSet;
use std::ffi::{c_int, c_void};
use std::ptr;
use std::sync::atomic::{AtomicBool, AtomicI32, AtomicPtr, AtomicU32, AtomicU64, Ordering};
#[cfg(not(target_arch = "wasm32"))]
use std::sync::{Condvar, Mutex, OnceLock, PoisonError};
#[cfg(not(target_arch = "wasm32"))]
use std::thread::ThreadId;

use crate::execution_context::HewExecutionContext;
use crate::internal::types::{AskError, HewActorState, HewDispatchFn, HewError, HewOverflowPolicy};
#[cfg(not(target_arch = "wasm32"))]
use crate::mailbox::{self, HewMailbox};
#[cfg(not(target_arch = "wasm32"))]
use crate::reply_channel::{self, HewReplyChannel};
#[cfg(not(target_arch = "wasm32"))]
use crate::scheduler;

// ── Crash teardown ordering hook ─────────────────────────────────────────

#[cfg(not(target_arch = "wasm32"))]
type CrashTeardownOrderHook = Option<fn(c_int)>;

#[cfg(not(target_arch = "wasm32"))]
static CRASH_TEARDOWN_ORDER_HOOK: Mutex<CrashTeardownOrderHook> = Mutex::new(None);

#[cfg(not(target_arch = "wasm32"))]
#[doc(hidden)]
pub const HEW_ACTOR_CRASH_TEARDOWN_BEFORE_EXIT_PROPAGATION: c_int = 1;

#[cfg(not(target_arch = "wasm32"))]
#[doc(hidden)]
pub const HEW_ACTOR_CRASH_TEARDOWN_AFTER_EXIT_PROPAGATION: c_int = 2;

#[cfg(not(target_arch = "wasm32"))]
#[doc(hidden)]
pub fn hew_actor_set_crash_teardown_order_hook(hook: Option<fn(c_int)>) {
    let mut guard = CRASH_TEARDOWN_ORDER_HOOK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    *guard = hook;
}

#[cfg(not(target_arch = "wasm32"))]
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
    LAST_ACTOR_ASK_ERROR.with(|c| c.set(err as i32));
    ptr::null_mut()
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

#[cfg(not(target_arch = "wasm32"))]
#[derive(Debug, Default)]
struct ActorStateLockState {
    held: bool,
    owner: Option<ThreadId>,
    poisoned: bool,
}

#[cfg(not(target_arch = "wasm32"))]
#[derive(Debug, Default)]
struct ActorStateLock {
    state: Mutex<ActorStateLockState>,
    available: Condvar,
}

#[cfg(not(target_arch = "wasm32"))]
fn actor_state_locks() -> &'static Mutex<HashMap<usize, std::sync::Arc<ActorStateLock>>> {
    static LOCKS: OnceLock<Mutex<HashMap<usize, std::sync::Arc<ActorStateLock>>>> = OnceLock::new();
    LOCKS.get_or_init(|| Mutex::new(HashMap::new()))
}

#[cfg(not(target_arch = "wasm32"))]
fn recover_runtime_mutex<T>(
    err: PoisonError<std::sync::MutexGuard<'_, T>>,
) -> std::sync::MutexGuard<'_, T> {
    err.into_inner()
}

#[cfg(not(target_arch = "wasm32"))]
fn lookup_actor_state_lock(actor: *mut HewActor) -> Option<std::sync::Arc<ActorStateLock>> {
    let locks = actor_state_locks()
        .lock()
        .unwrap_or_else(recover_runtime_mutex);
    locks.get(&(actor as usize)).cloned()
}

#[cfg(not(target_arch = "wasm32"))]
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

#[cfg(not(target_arch = "wasm32"))]
fn register_actor_state_lock(actor: *mut HewActor) {
    let mut locks = actor_state_locks()
        .lock()
        .unwrap_or_else(recover_runtime_mutex);
    locks.insert(actor as usize, std::sync::Arc::default());
}

#[cfg(not(target_arch = "wasm32"))]
fn unregister_actor_state_lock(actor: *mut HewActor) {
    let mut locks = actor_state_locks()
        .lock()
        .unwrap_or_else(recover_runtime_mutex);
    locks.remove(&(actor as usize));
}

#[cfg(not(target_arch = "wasm32"))]
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

#[cfg(not(target_arch = "wasm32"))]
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

#[cfg(target_arch = "wasm32")]
#[derive(Debug, Default)]
struct ActorStateLockState {
    held: bool,
    poisoned: bool,
}

#[cfg(target_arch = "wasm32")]
thread_local! {
    static ACTOR_STATE_LOCKS: std::cell::RefCell<HashMap<usize, Box<ActorStateLockState>>> =
        std::cell::RefCell::new(HashMap::new());
}

#[cfg(target_arch = "wasm32")]
fn register_actor_state_lock(actor: *mut HewActor) {
    ACTOR_STATE_LOCKS.with(|locks| {
        locks
            .borrow_mut()
            .insert(actor as usize, Box::new(ActorStateLockState::default()));
    });
}

#[cfg(target_arch = "wasm32")]
pub(crate) fn actor_state_lock_seat(
    actor: *mut HewActor,
) -> *mut crate::execution_context::HewActorStateLockState {
    ACTOR_STATE_LOCKS.with(|locks| {
        let mut locks = locks.borrow_mut();
        #[cfg(test)]
        locks
            .entry(actor as usize)
            .or_insert_with(|| Box::new(ActorStateLockState::default()));
        locks
            .get_mut(&(actor as usize))
            .map_or(ptr::null_mut(), |state| {
                (&raw mut **state).cast::<crate::execution_context::HewActorStateLockState>()
            })
    })
}

#[cfg(target_arch = "wasm32")]
fn unregister_actor_state_lock(actor: *mut HewActor) {
    ACTOR_STATE_LOCKS.with(|locks| {
        locks.borrow_mut().remove(&(actor as usize));
    });
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

#[cfg(not(target_arch = "wasm32"))]
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

#[cfg(not(target_arch = "wasm32"))]
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

#[cfg(not(target_arch = "wasm32"))]
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
#[cfg(not(target_arch = "wasm32"))]
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

#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_state_lock_acquire(actor: *mut HewActor) -> c_int {
    cabi_guard!(actor.is_null(), HEW_ACTOR_STATE_LOCK_ERR);
    ACTOR_STATE_LOCKS.with(|locks| {
        let mut locks = locks.borrow_mut();
        let Some(state) = locks.get_mut(&(actor as usize)) else {
            crate::set_last_error("actor-state lock acquire: actor has no registered state lock");
            return HEW_ACTOR_STATE_LOCK_ERR;
        };
        acquire_actor_state_lock_state(state)
    })
}

#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_state_lock_release(actor: *mut HewActor) -> c_int {
    cabi_guard!(actor.is_null(), HEW_ACTOR_STATE_LOCK_ERR);
    ACTOR_STATE_LOCKS.with(|locks| {
        let mut locks = locks.borrow_mut();
        let Some(state) = locks.get_mut(&(actor as usize)) else {
            crate::set_last_error("actor-state lock release: actor has no registered state lock");
            return HEW_ACTOR_STATE_LOCK_ERR;
        };
        release_actor_state_lock_state(state)
    })
}

#[cfg(target_arch = "wasm32")]
fn acquire_actor_state_lock_state(state: &mut ActorStateLockState) -> c_int {
    if state.poisoned {
        crate::set_last_error("actor-state lock acquire: lock poisoned by prior handler panic");
        return HEW_ACTOR_STATE_LOCK_ERR;
    }
    if state.held {
        crate::set_last_error("actor-state lock acquire: nested WASM actor dispatch");
        return HEW_ACTOR_STATE_LOCK_ERR;
    }
    state.held = true;
    HEW_ACTOR_STATE_LOCK_OK
}

#[cfg(target_arch = "wasm32")]
fn release_actor_state_lock_state(state: &mut ActorStateLockState) -> c_int {
    if !state.held {
        crate::set_last_error("actor-state lock release: lock is not held");
        return HEW_ACTOR_STATE_LOCK_ERR;
    }
    state.held = false;
    HEW_ACTOR_STATE_LOCK_OK
}

#[cfg(target_arch = "wasm32")]
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
    // SAFETY: `lock_seat` is a stable Box allocation in ACTOR_STATE_LOCKS.
    let state = unsafe { &mut *seat.cast::<ActorStateLockState>() };
    acquire_actor_state_lock_state(state)
}

#[cfg(target_arch = "wasm32")]
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
    // SAFETY: `lock_seat` is a stable Box allocation in ACTOR_STATE_LOCKS.
    let state = unsafe { &mut *seat.cast::<ActorStateLockState>() };
    release_actor_state_lock_state(state)
}

#[cfg(target_arch = "wasm32")]
unsafe fn actor_state_lock_release_after_panic_impl(actor: *mut HewActor, poison: bool) -> c_int {
    if actor.is_null() {
        return HEW_ACTOR_STATE_LOCK_OK;
    }
    ACTOR_STATE_LOCKS.with(|locks| {
        let mut locks = locks.borrow_mut();
        let Some(state) = locks.get_mut(&(actor as usize)) else {
            return HEW_ACTOR_STATE_LOCK_OK;
        };
        state.held = false;
        state.poisoned |= poison;
        HEW_ACTOR_STATE_LOCK_OK
    })
}

#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_state_lock_release_after_panic(actor: *mut HewActor) -> c_int {
    unsafe { actor_state_lock_release_after_panic_impl(actor, false) }
}

#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_state_lock_poison_after_panic(actor: *mut HewActor) -> c_int {
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
    static NEXT_SPAWN_ACTOR_ID_OVERRIDE: Cell<Option<u64>> = const { Cell::new(None) };
}

#[cfg(all(test, not(target_arch = "wasm32")))]
fn override_next_spawn_actor_id(actor_id: u64) {
    NEXT_SPAWN_ACTOR_ID_OVERRIDE.with(|slot| slot.set(Some(actor_id)));
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
/// - Returns a freshly heap-allocated wrapper (`malloc`-compatible allocation
///   so the runtime can pair it with `libc::free`) whose owned heap fields
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

    /// Dispatch function (context-leading canonical signature).
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
    pub terminate_fn: Option<unsafe extern "C" fn(*mut c_void)>,

    /// Optional state-drop function that runs `impl Drop` callbacks on every
    /// owned field of the actor's live state immediately before
    /// `libc::free(a.state)`. Generated unconditionally for every actor by
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
    #[cfg(not(target_arch = "wasm32"))]
    pub arena: *mut crate::arena::ActorArena,
    /// Per-actor arena bump allocator on WASM.  Allocated during spawn via
    /// `hew_arena_new()`, installed as the current arena during each activation,
    /// reset after each dispatch cycle, and freed during actor teardown.
    #[cfg(target_arch = "wasm32")]
    pub arena: *mut c_void,

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
    /// WASM has no native `RuntimeInner`; keep the layout mirror opaque.
    #[cfg(target_arch = "wasm32")]
    pub(crate) runtime: *const c_void,

    /// Count of in-flight by-ID send/ask/stop operations currently pinning
    /// this actor allocation.
    ///
    /// `with_actor_send_by_id` increments this field (atomically, under
    /// `LIVE_ACTORS`) before releasing the registry lock, then decrements it
    /// via a RAII guard when the operation completes.  The free path in
    /// `hew_actor_free_inner` calls `untrack_actor` first (removing the actor
    /// from `LIVE_ACTORS` so no new pins can be taken), then spins until
    /// `send_pin_count` reaches 0 (draining any in-flight pins taken before
    /// the untrack) before calling `finalize`.  This untrack-first ordering
    /// makes the pin-drain and the liveness check mutually exclusive: either
    /// the sender pins before the freer untracks (freer waits), or the freer
    /// untracks before the sender's map lookup (sender gets `None`, no pin).
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
}

// SAFETY: `HewActor` is designed for concurrent access across worker threads.
// All mutable shared fields use atomic types. Raw pointers are managed by
// the scheduler/actor lifecycle which ensures exclusive access during
// activation (CAS `RUNNABLE` → `RUNNING`).
unsafe impl Send for HewActor {}
// SAFETY: Concurrent reads/writes of shared mutable fields use atomics.
// Raw-pointer fields are lifecycle-managed by scheduler CAS transitions.
unsafe impl Sync for HewActor {}

#[cfg(not(target_arch = "wasm32"))]
fn clear_suspended_cancel_token(actor: &HewActor) {
    let token = actor
        .suspended_cancel_token
        .swap(std::ptr::null_mut(), Ordering::AcqRel);
    if !token.is_null() {
        // SAFETY: the actor slot owns a retained task-scope cancellation token.
        unsafe { crate::task_scope::hew_cancel_token_release(token.cast()) };
    }
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

// PID is now unified with id — actors use location-transparent IDs everywhere.

// ── Live actor tracking (delegated to lifetime::live_actors) ──────────────

#[cfg(not(target_arch = "wasm32"))]
const TERMINATE_WAIT_TIMEOUT: std::time::Duration = std::time::Duration::from_secs(5);
#[cfg(not(target_arch = "wasm32"))]
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
/// Delegates to [`live_actors::with_live_actor_by_id`].
// live on not(wasm32) — monitor.rs + link.rs; dead on wasm32; callers monitor.rs:98, link.rs:201
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
fn actor_free_state_is_quiescent(state: i32) -> bool {
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
unsafe fn prepare_quiescent_actor_for_cleanup(actor: *mut HewActor) {
    #[cfg(not(target_arch = "wasm32"))]
    {
        // SAFETY: caller guarantees `actor` is valid and quiescent.
        let actor_id = unsafe { (*actor).id };
        crate::timer_periodic::cancel_all_timers_for_actor(actor);
        // Unregister any active-mode connection fds owned by this actor BEFORE
        // it is untracked/freed, so a readiness event arriving after the actor
        // stops is dropped (the dead-actor-while-registered race) rather than
        // delivered to a freed actor. Keyed by the actor's address, matching
        // the snapshot the reactor stored at attach time.
        crate::reactor::reactor_detach_actor(actor as usize);
        // SAFETY: caller guarantees `actor` is valid; `unregister_actor_names`
        // does not require LIVE_ACTORS membership, only the actor id.
        unsafe { crate::hew_node::unregister_actor_names(actor_id) };
        // Remove all parse-error slots for this actor across every parser kind.
        // Prevents unbounded growth of the global map on long-running nodes that
        // spawn and reap many actors.
        crate::parse_error_slot::clear_all_for_actor(actor_id);
    }
    #[cfg(target_arch = "wasm32")]
    {
        // SAFETY: caller guarantees `actor` is valid and not being dispatched.
        let actor_id = unsafe { (*actor).id };
        unsafe { crate::timer_periodic_wasm::cancel_all_timers_for_actor(actor) };
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
unsafe fn scrub_actor_relationships_after_pin_drain(actor: *mut HewActor) {
    #[cfg(not(target_arch = "wasm32"))]
    {
        // SAFETY: the caller guarantees the allocation remains valid.
        let actor_id = unsafe { (*actor).id };
        crate::link::remove_all_links_for_actor(actor_id, actor);
        crate::monitor::remove_all_monitors_for_actor(actor_id, actor);
    }
    #[cfg(target_arch = "wasm32")]
    let _ = actor;
}

/// Outcome of [`decide_finalize_by_latch`] — the canonical "is it safe to
/// finalize this quiescent-but-possibly-re-enqueued actor?" decision.
#[cfg_attr(target_arch = "wasm32", allow(dead_code))]
enum FinalizeDecision {
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
fn decide_finalize_by_latch(a: &HewActor) -> FinalizeDecision {
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
unsafe fn finalize_quiescent_actor_cleanup(actor: *mut HewActor, state: i32) {
    // SAFETY: caller guarantees `actor` is quiescent and not dispatching.
    unsafe { finalize_quiescent_actor_cleanup_with_options(actor, state, false) };
}

/// Same as [`finalize_quiescent_actor_cleanup`], but the caller may suppress
/// the codegen-emitted `state_drop_fn` invocation.
///
/// `suppress_state_drop = true` is used by supervisor restart paths
/// (`ONE_FOR_ALL` / `REST_FOR_ONE`) when freeing a non-crashed sibling whose
/// state is about to be reused by the next spawn. See the rationale in
/// [`free_actor_resources_with_options`].
///
/// # Safety
///
/// `actor` must be valid, quiescent, and no longer tracked in `LIVE_ACTORS`.
unsafe fn finalize_quiescent_actor_cleanup_with_options(
    actor: *mut HewActor,
    state: i32,
    suppress_state_drop: bool,
) {
    if state != HewActorState::Crashed as i32 {
        // SAFETY: caller guarantees the actor is quiescent and not dispatching.
        unsafe { call_terminate_fn(actor) };
    }

    // SAFETY: caller guarantees the actor remains valid and is no longer dispatching.
    unsafe { free_actor_resources_with_options(actor, suppress_state_drop) };
}

/// Free all remaining tracked actors. Called during scheduler shutdown
/// after all worker threads have been joined.
///
/// # Safety
///
/// Must only be called after all worker threads have stopped (native)
/// or when no dispatch is in progress (WASM).
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

    for live_actors::ActorPtr(actor) in actors.into_values() {
        if actor.is_null() {
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
        // freeing it. This prevents a use-after-free if hew_wasm_timer_tick
        // is called after cleanup but before the timer fires naturally.
        // SAFETY: scheduler is shut down; no concurrent timer-wheel access.
        #[cfg(target_arch = "wasm32")]
        unsafe {
            crate::scheduler_wasm::cancel_actor_sleep_queue_entry(actor);
        }

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
        // The same quiescence gate routes a still-`Suspended` actor (one parked
        // at a non-final `coro.suspend` with a live continuation frame) to the
        // fail-closed leak: `actor_free_state_is_quiescent` excludes `Suspended`,
        // so cleanup never finalizes a parked actor (which would free its box
        // and leak the frame). Unlike `hew_actor_free_inner`, the shutdown sweep
        // cannot block to destroy the parked frame (workers are joined), so it
        // leaks-not-frees — the correct fail-closed shutdown behaviour.
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
                if a.send_pin_count.load(Ordering::Acquire) == 0 {
                    break;
                }
                if std::time::Instant::now() >= pin_deadline {
                    eprintln!(
                        "hew: runtime error: actor {:#x} send pins did not drain \
                         during shutdown cleanup; actor leaked to avoid UAF",
                        a.id
                    );
                    pinned = true;
                    break;
                }
                #[cfg(target_arch = "wasm32")]
                std::hint::spin_loop();
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
    #[cfg(target_arch = "wasm32")]
    crate::lifetime::local_handles::finish_current_shutdown();
}

/// Free an actor's resources without untracking, optionally suppressing the
/// codegen-emitted `state_drop_fn` invocation.
///
/// `suppress_state_drop = true` is the supervisor-restart path. The runtime
/// byte-copies `spec.init_state` into every spawned actor's `state` /
/// `init_state` slots, which means owned-field pointers (Vec.ptr, String.ptr,
/// IO handles) are aliased between the supervisor's `spec.init_state` and
/// every actor instance ever spawned from that spec. Running `state_drop_fn`
/// on a non-crashed sibling that is about to be replaced via
/// `restart_child_from_spec` would free those field pointers, leaving the
/// supervisor's `spec.init_state` byte-aliasing dangling pointers. The next
/// restart byte-copies that dangling spec into a fresh actor and dereferences
/// it on the next teardown — a use-after-free.
///
/// State-drop must not regress the existing leak-not-UAF invariant: prior to
/// the state-drop slot landing, non-crashed teardowns leaked the field
/// allocations rather than freeing them, so the supervisor's spec aliases
/// stayed valid (just leaked) until final teardown. The supervisor still
/// frees `spec.init_state` exactly once during `hew_supervisor_stop`, where
/// no further restart can observe the dangling bytes.
///
/// This narrows the regression window to non-crashed restarts. The deeper
/// latent issue — user mutation `self.x = newHeap` already leaves a dangling
/// pointer in `spec.init_state` via drop-on-assign — predates state-drop and
/// requires a semantic clone of `init_state` to fix; that is out of scope
/// here.
///
/// # Safety
///
/// `actor` must be a valid pointer to a live `HewActor` that is not
/// currently being dispatched.
#[cfg(not(target_arch = "wasm32"))]
unsafe fn free_actor_resources_with_options(actor: *mut HewActor, suppress_state_drop: bool) {
    #[cfg(feature = "profiler")]
    // SAFETY: `actor` is valid.
    unsafe {
        crate::profiler::actor_registry::unregister(actor);
    };

    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };

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

    // C1 abandonment teardown (D-C1, R326/R327): a never-woken `Suspended` actor
    // freed at shutdown still owns a live coroutine frame in `suspended_cont`
    // (e.g. a `scope` whose child awaits, or an actor awaiting a reply that
    // never arrives before shutdown). Destroy it exactly once BEFORE reclaiming
    // the box, or the frame + any frame-owned heap values leak. `destroy_parked`
    // wins the single `… → Destroyed` CAS (FG1), runs the `cleanup` outline
    // (coro.free → hew_cont_frame_free), and nulls the slot in the same critical
    // section (FG4); it refuses if a resume is in flight or it was already
    // destroyed, so this is the safe single-teardown owner on the free path.
    // Dormant today (no actor reaches `Suspended` while the source surface stays
    // thread-parked), but it makes the live suspend edge non-leaking. NOTE: the
    // single-task cancellation FLOW (unregister-readiness + resume-with-cancel)
    // is a separate concern; this is only the single-destroy plumbing.
    if crate::coro_exec::has_live_parked_cont(a) {
        // SAFETY: `a` is the actor being freed; the caller guarantees exclusive
        // access (no concurrent dispatch), so no resume can race this teardown.
        let _ = unsafe { crate::coro_exec::destroy_parked(a) };
    }

    // Run codegen-generated state-drop on the live state so types
    // implementing `impl Drop` (Vec, String, HashMap, IO handles) release
    // their resources before the underlying allocation goes away.
    //
    // Crashed actors are skipped: their state may be partially torn down
    // by the panic that crashed them, so re-entering user Drop on it
    // risks a second crash or a double-free of fields the crash already
    // released. This mirrors the terminate-skip in
    // `finalize_quiescent_actor_cleanup` and the comment in
    // `cleanup_all_actors` ("Skip crashed actors — their state may be
    // corrupted"). Structural cleanup (libc::free, arena, mailbox,
    // Box::from_raw) below still runs unconditionally.
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
    //    The trailing `libc::free(a.init_state)` releases only the wrapper
    //    bytes; it does not dereference the embedded pointers.
    // 2. User code that overwrites a state field (`self.x = newHeap`) goes
    //    through drop-on-assign on `a.state`, which frees the original
    //    heap. The corresponding pointer inside `a.init_state` becomes
    //    dangling, but is never dereferenced — only `libc::free` runs over
    //    the wrapper bytes.
    // 3. Supervisor restart never reads `a.init_state`. Each restart
    //    allocates a fresh state buffer from `InternalChildSpec.init_state`,
    //    which `hew_supervisor_add_child_spec` (supervisor.rs:1379) created
    //    by independent `libc::malloc` + `ptr::copy_nonoverlapping` from
    //    the caller's spec bytes at registration time.
    let actor_is_crashed = a.actor_state.load(Ordering::Acquire) == HewActorState::Crashed as i32;
    if !actor_is_crashed && !suppress_state_drop {
        if let Some(state_drop_fn) = a.state_drop_fn {
            if !a.state.is_null() {
                // SAFETY: `a.state` is the live state allocation;
                // `state_drop_fn` is a codegen-emitted function that walks
                // owned fields and tolerates null sub-pointers per LESSONS
                // row `raii-null-after-move`.
                unsafe { state_drop_fn(a.state) };
            }
        }
    }

    // SAFETY: State was malloc'd by deep_copy_state.
    unsafe {
        libc::free(a.state);
        libc::free(a.init_state);
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
        // Null the mailbox slot before freeing — same defense-in-depth
        // discipline as the arena slot above (`raii-null-after-move`).
        // SAFETY: caller guarantees exclusive access to `actor` during free.
        unsafe { (*actor).mailbox = std::ptr::null_mut() };
        // SAFETY: Mailbox was allocated by hew_mailbox_new.
        unsafe { mailbox::hew_mailbox_free(mb) };
    }

    // SAFETY: Actor was allocated with Box::new / Box::into_raw.
    drop(unsafe { Box::from_raw(actor) });
}

/// Free an actor's resources (WASM version — delegates to `free_actor_resources_wasm`).
///
/// # Safety
///
/// `actor` must be a valid pointer to a live `HewActor` that is not
/// currently being dispatched.
#[cfg(target_arch = "wasm32")]
unsafe fn free_actor_resources_with_options(actor: *mut HewActor, suppress_state_drop: bool) {
    // SAFETY: target_arch = wasm32 shares the same invariants as the test helper.
    unsafe { free_actor_resources_wasm_with_options(actor, suppress_state_drop) };
}

/// Free an actor's resources using the WASM cleanup path.  Always runs
/// `state_drop_fn` on non-crashed actors (the standard teardown path).
/// Test-only entry point preserved for unit tests under `cfg(test)`.
///
/// # Safety
///
/// `actor` must be a valid pointer to a live `HewActor` that is not
/// currently being dispatched.
// live on test — scheduler_wasm tests; dead on non-test wasm build; caller scheduler_wasm.rs:4154
#[cfg_attr(not(test), allow(dead_code))]
#[cfg(any(target_arch = "wasm32", test))]
pub(crate) unsafe fn free_actor_resources_wasm(actor: *mut HewActor) {
    // SAFETY: caller forwards the same invariants the inner requires.
    unsafe { free_actor_resources_wasm_with_options(actor, false) };
}

/// Free an actor's resources using the WASM cleanup path, with the option to
/// suppress `state_drop_fn` (supervisor-restart paths).  See
/// [`free_actor_resources_with_options`] (native) for the alias-chain
/// rationale; the WASM and native `HewActor` layouts are byte-identical
/// (offset assertions in `scheduler_wasm.rs`).
///
/// # Safety
///
/// `actor` must be a valid pointer to a live `HewActor` that is not
/// currently being dispatched.
#[cfg(any(target_arch = "wasm32", test))]
pub(crate) unsafe fn free_actor_resources_wasm_with_options(
    actor: *mut HewActor,
    suppress_state_drop: bool,
) {
    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };

    // C1 abandonment teardown (D-C1) — parity with the native
    // `free_actor_resources_with_options`. A never-woken `Suspended` actor freed
    // at shutdown owns a live coroutine frame; destroy it exactly once before
    // reclaiming the box or the frame leaks. WASM is single-threaded so no
    // resume can race, but the source shape mirrors native so both free paths
    // read as one invariant (`raii-native-wasm-parity`).
    if crate::coro_exec::has_live_parked_cont(a) {
        // SAFETY: `a` is the actor being freed; single-threaded, no concurrent
        // dispatch can race this teardown.
        let _ = unsafe { crate::coro_exec::destroy_parked(a) };
    }

    // Run codegen-generated state-drop on the live state so types
    // implementing `impl Drop` release their resources before the
    // allocation goes away. The WASM path has identical layout
    // (compile-time enforced by the offset assertions in
    // `scheduler_wasm.rs`) and the same `a.init_state` aliasing as the
    // native path, so state-drop runs on `a.state` only — running it on
    // `a.init_state` would double-free every owned field. See the SAFETY
    // block in the native `free_actor_resources_with_options` for the full
    // rationale, including why `suppress_state_drop` is required on
    // supervisor-restart paths.
    //
    // Crashed actors are skipped here too: same rationale as the native
    // path. Structural cleanup below still runs unconditionally so the
    // allocation, arena, mailbox, and HewActor box are always released.
    let actor_is_crashed = a.actor_state.load(Ordering::Acquire) == HewActorState::Crashed as i32;
    if !actor_is_crashed && !suppress_state_drop {
        if let Some(state_drop_fn) = a.state_drop_fn {
            if !a.state.is_null() {
                // SAFETY: `a.state` is the live state allocation;
                // `state_drop_fn` is a codegen-emitted function that walks
                // owned fields and tolerates null sub-pointers.
                unsafe { state_drop_fn(a.state) };
            }
        }
    }

    // SAFETY: State was malloc'd by deep_copy_state.
    unsafe {
        libc::free(a.state);
        libc::free(a.init_state);
    }

    if !a.arena.is_null() {
        let arena_ptr = a.arena;
        // Null the slot BEFORE freeing — parity with the native
        // `free_actor_resources_with_options` (`raii-native-wasm-parity`
        // + `raii-null-after-move`).  WASM is single-threaded so the
        // arena UAF cannot fire here, but the source shape must mirror
        // native to keep both paths reviewable as one invariant.
        // SAFETY: caller guarantees exclusive access to `actor` during free.
        unsafe { (*actor).arena = std::ptr::null_mut() };
        // SAFETY: Arena was created by hew_arena_new during spawn.
        unsafe { crate::arena::hew_arena_free_all(arena_ptr.cast::<crate::arena::ActorArena>()) };
    }

    unregister_actor_state_lock(actor);

    if !a.mailbox.is_null() {
        let mailbox_ptr = a.mailbox;
        // Null before free — parity with the native path; covers any
        // straggler reader that re-reads `a.mailbox` during teardown.
        // SAFETY: caller guarantees exclusive access to `actor` during free.
        unsafe { (*actor).mailbox = std::ptr::null_mut() };
        let mb = mailbox_ptr.cast::<crate::mailbox_wasm::HewMailboxWasm>();
        // SAFETY: this helper is only used with WASM mailboxes.
        unsafe { crate::mailbox_wasm::hew_mailbox_free(mb) };
    }

    // SAFETY: Actor was allocated with Box::new / Box::into_raw.
    drop(unsafe { Box::from_raw(actor) });
}

// ── Terminate callback invocation ───────────────────────────────────────

/// Run the actor's terminate callback exactly once, with crash recovery.
///
/// Sets up the actor lane and (on worker threads) a `sigsetjmp` recovery frame
/// so that Hew panics or signals inside the terminate block are
/// caught instead of aborting the process.
///
/// Called at terminal state transitions (→ Stopped), **not** at free time.
///
/// # Safety
///
/// `actor` must be a valid pointer to a live [`HewActor`] in a terminal
/// state (`Stopped`) that is not currently being dispatched.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) unsafe fn call_terminate_fn(actor: *mut HewActor) {
    // SAFETY: caller guarantees `actor` is valid.
    let a = unsafe { &*actor };

    // Guard: only run once across all terminal-transition paths.
    if a.terminate_called.swap(true, Ordering::AcqRel) {
        return;
    }

    let Some(terminate_fn) = a.terminate_fn else {
        a.terminate_finished.store(true, Ordering::Release);
        return;
    };

    if a.state.is_null() {
        a.terminate_finished.store(true, Ordering::Release);
        return;
    }

    let state = a.state;
    let mut execution_context = crate::execution_context::HewExecutionContext {
        actor,
        actor_id: a.id,
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
    let _rt_guard = crate::runtime::rt_default().map(|rt| unsafe { crate::runtime::enter(rt) });

    // Set up crash recovery (returns null on non-worker threads).
    // SAFETY: `actor` is valid and in a terminal state; null msg is fine.
    let jmp_buf_ptr = unsafe { crate::signal::prepare_dispatch_recovery(actor, ptr::null_mut()) };

    let is_normal_path = if jmp_buf_ptr.is_null() {
        // No recovery context (external thread or not initialised).
        // Hew panics (longjmp) from an external thread will still
        // abort the process — that's an acceptable limitation.
        true
    } else {
        // SAFETY: jmp_buf_ptr is valid (from prepare_dispatch_recovery).
        let ret = unsafe { crate::signal::sigsetjmp(jmp_buf_ptr, 1) };
        if ret == 0 {
            crate::signal::mark_recovery_active();
            true
        } else {
            false
        }
    };

    if is_normal_path {
        // catch_unwind guards against Rust panics; the sigsetjmp frame
        // (when present) guards against Hew panics and signals.
        //
        // The terminate callback (emitted by codegen) acquires the actor-state
        // lock before calling the user's on(stop) body, mirroring the
        // dispatch-handler lock protocol (LESSONS: cleanup-all-exits P0).
        // If the user body panics, catch_unwind returns Err and the lock is
        // still held — release it here on the panic path so teardown can
        // proceed (state_drop_fn and arena free both run unconditionally).
        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            // SAFETY: terminate_fn and state are valid; actor is not
            // being dispatched.
            unsafe { terminate_fn(state) };
        }));
        if result.is_err() {
            // Release a lock the trampoline may have acquired before the panic.
            // This is a no-op when no terminate_fn was set or the lock was
            // already released normally.
            // SAFETY: actor is valid; the lock registry tolerates an unheld
            // or unregistered lock (same invariant as the scheduler panic path).
            unsafe {
                let _ = hew_actor_state_lock_release_after_panic(actor);
            }
        }
        if !jmp_buf_ptr.is_null() {
            crate::signal::clear_dispatch_recovery();
        }
    } else {
        // Terminate block crashed via signal/longjmp. The actor is
        // already in a terminal state so hew_actor_trap is a no-op for
        // the state transition, but handle_crash_recovery properly
        // clears in_recovery and logs a crash report.
        // Release any lock the trampoline acquired before the crash (same
        // invariant as the scheduler signal-recovery path at scheduler.rs:991).
        // SAFETY: actor is valid; the release helper tolerates an unheld lock.
        unsafe {
            let _ = hew_actor_state_lock_release_after_panic(actor);
        }
        // SAFETY: called immediately after sigsetjmp returned non-zero.
        unsafe { crate::signal::handle_crash_recovery() };
    }

    a.terminate_finished.store(true, Ordering::Release);
    let restored_context = crate::execution_context::set_current_context(prev_context);
    debug_assert_eq!(restored_context, &raw mut execution_context);
}

/// Run the actor's terminate callback exactly once (WASM version).
///
/// No signal recovery on WASM — `catch_unwind` is the only guard.
///
/// # Safety
///
/// `actor` must be a valid pointer to a live [`HewActor`] in a terminal
/// state (`Stopped`) that is not currently being dispatched.
#[cfg(target_arch = "wasm32")]
pub(crate) unsafe fn call_terminate_fn(actor: *mut HewActor) {
    let a = unsafe { &*actor };

    if a.terminate_called.swap(true, Ordering::AcqRel) {
        return;
    }

    let Some(terminate_fn) = a.terminate_fn else {
        a.terminate_finished.store(true, Ordering::Release);
        return;
    };

    if a.state.is_null() {
        a.terminate_finished.store(true, Ordering::Release);
        return;
    }

    let state = a.state;
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        unsafe { terminate_fn(state) };
    }));
    if result.is_err() {
        // Release any lock the trampoline acquired before the panic.
        // SAFETY: actor is valid; the release helper tolerates an unheld lock.
        unsafe {
            let _ = hew_actor_state_lock_release_after_panic(actor);
        }
    }
    a.terminate_finished.store(true, Ordering::Release);
}

/// Actor spawn options for [`hew_actor_spawn_opts`].
#[repr(C)]
#[derive(Debug)]
pub struct HewActorOpts {
    /// Pointer to initial state (deep-copied).
    pub init_state: *mut c_void,
    /// Size of `init_state` in bytes.
    pub state_size: usize,
    /// Dispatch function.
    pub dispatch: Option<HewDispatchFn>,
    /// Mailbox capacity (`-1` or `0` = unbounded).
    pub mailbox_capacity: i32,
    /// Overflow policy (see [`HewOverflowPolicy`]).
    pub overflow: i32,
    /// Optional coalesce key function.
    pub coalesce_key_fn: Option<unsafe extern "C" fn(i32, *mut c_void, usize) -> u64>,
    /// Fallback policy used when coalescing finds no key match.
    pub coalesce_fallback: i32,
    /// Messages per activation (`0` = default).
    pub budget: i32,
    /// Per-actor arena cap in bytes (`0` = unbounded, same as `hew_arena_new`).
    ///
    /// Non-zero values cause [`hew_actor_spawn_opts`] to call
    /// `hew_arena_new_with_cap(arena_cap_bytes)` instead of `hew_arena_new()`.
    /// Set from the `#[max_heap(N)]` actor attribute; callers that do not use
    /// the attribute must supply `0`.
    pub arena_cap_bytes: usize,
    /// Non-zero when the checker determined this actor participates in an
    /// actor-ref cycle. Future consumer: cycle-detection / Machine Lane B
    /// cycle handling.
    pub cycle_capable: i32,
    /// Typed destructor for queued message payloads evicted before dispatch.
    pub message_drop_fn: Option<unsafe extern "C" fn(i32, *mut c_void, usize)>,
}

fn parse_overflow_policy(policy: i32) -> HewOverflowPolicy {
    match policy {
        x if x == HewOverflowPolicy::Block as i32 => HewOverflowPolicy::Block,
        x if x == HewOverflowPolicy::DropOld as i32 => HewOverflowPolicy::DropOld,
        x if x == HewOverflowPolicy::Fail as i32 => HewOverflowPolicy::Fail,
        x if x == HewOverflowPolicy::Coalesce as i32 => HewOverflowPolicy::Coalesce,
        _ => HewOverflowPolicy::DropNew,
    }
}

// ── Spawn ───────────────────────────────────────────────────────────────
// All spawn functions use native mailbox/scheduler and are not available on WASM.
// WASM actors are created through the bridge module instead.

fn actor_state_malloc(size: usize) -> *mut c_void {
    #[cfg(all(test, not(target_arch = "wasm32")))]
    {
        if should_fail_actor_state_alloc() {
            return ptr::null_mut();
        }
    }

    // SAFETY: `size` is forwarded to libc unchanged.
    unsafe { libc::malloc(size) }
}

/// Deep-copy `src` into a new malloc'd buffer.
///
/// Returns null if `src` is null, `size` is 0, or allocation fails.
/// On allocation failure, sets `hew_last_error` with the details.
///
/// # Safety
///
/// `src` must point to at least `size` readable bytes.
unsafe fn deep_copy_state(src: *mut c_void, size: usize) -> *mut c_void {
    if src.is_null() || size == 0 {
        return ptr::null_mut();
    }
    // SAFETY: Caller guarantees `src` is readable for `size` bytes.
    unsafe {
        let dst = actor_state_malloc(size);
        if dst.is_null() {
            crate::set_last_error(format!(
                "OOM: failed to allocate {size} bytes for actor state copy"
            ));
            return ptr::null_mut();
        }
        ptr::copy_nonoverlapping(src.cast::<u8>(), dst.cast::<u8>(), size);
        dst
    }
}

/// Configuration for the internal actor spawn helper.
///
/// All three public spawn functions build one of these and delegate to
/// [`spawn_actor_internal`].
struct ActorSpawnConfig {
    state: *mut c_void,
    state_size: usize,
    dispatch: Option<HewDispatchFn>,
    mailbox: *mut c_void,
    budget: i32,
    coalesce_key_fn: Option<unsafe extern "C" fn(i32, *mut c_void, usize) -> u64>,
    /// Checker-derived cycle capability for future Machine Lane B handling.
    #[expect(
        dead_code,
        reason = "receiver-side ABI bit is staged for the Machine Lane B cycle-detection consumer"
    )]
    cycle_capable: bool,
    /// Arena cap in bytes. `0` = unbounded (calls `hew_arena_new`).
    /// Non-zero calls `hew_arena_new_with_cap(cap_bytes)`.
    cap_bytes: usize,
    /// When true, `spawn_actor_internal` adopts `state` as a pre-built
    /// `malloc`-compatible clone (set by [`hew_actor_spawn_opts_adopt`]) and
    /// skips the second `deep_copy_state` that would create `init_state`.
    /// The actor's `init_state` slot is left null: the supervisor's spec
    /// holds the source-of-truth clone template, and direct-spawn restart
    /// paths must re-allocate via `state_clone_fn` rather than reading
    /// `actor.init_state`. Required to avoid byte-aliasing the cloned
    /// wrapper's owned fields with a sibling `init_state` byte-copy (C1).
    adopt: bool,
}

#[cfg(not(target_arch = "wasm32"))]
unsafe fn free_spawn_mailbox(mailbox: *mut c_void) {
    let mb = mailbox.cast::<HewMailbox>();
    if !mb.is_null() {
        // SAFETY: `mb` came from the native mailbox constructors used by spawn.
        unsafe { mailbox::hew_mailbox_free(mb) };
    }
}

#[cfg(target_arch = "wasm32")]
unsafe fn free_spawn_mailbox(mailbox: *mut c_void) {
    let mb = mailbox.cast::<crate::mailbox_wasm::HewMailboxWasm>();
    if !mb.is_null() {
        // SAFETY: `mb` came from the WASM mailbox constructors used by spawn.
        unsafe { crate::mailbox_wasm::hew_mailbox_free(mb) };
    }
}

/// Release spawn-owned inputs when actor construction fails before tracking.
///
/// # Safety
///
/// - `config.state` and `init_state` must be allocations owned by the spawn path,
///   or null.
/// - `config.mailbox` must be a mailbox pointer transferred to the spawn path, or null.
unsafe fn cleanup_failed_spawn(config: &ActorSpawnConfig, init_state: *mut c_void) {
    // SAFETY: caller guarantees these pointers are owned by the in-progress spawn.
    unsafe {
        libc::free(config.state);
        if !init_state.is_null() {
            libc::free(init_state);
        }
        free_spawn_mailbox(config.mailbox);
    }
}

fn next_spawn_actor_identity() -> u64 {
    #[cfg(not(target_arch = "wasm32"))]
    {
        #[cfg(test)]
        if let Some(actor_id) = NEXT_SPAWN_ACTOR_ID_OVERRIDE.with(Cell::take) {
            return actor_id;
        }
        crate::pid::next_actor_id(NEXT_ACTOR_SERIAL.fetch_add(1, Ordering::Relaxed))
    }

    #[cfg(target_arch = "wasm32")]
    {
        NEXT_ACTOR_SERIAL.fetch_add(1, Ordering::Relaxed)
    }
}

#[expect(
    clippy::needless_pass_by_value,
    reason = "config is a lightweight aggregate of Copy fields; consuming it reads clearly at call sites"
)]
fn build_spawned_actor(
    config: ActorSpawnConfig,
    actor_id: u64,
    init_state: *mut c_void,
    arena: *mut crate::arena::ActorArena,
) -> Box<HewActor> {
    #[cfg(not(target_arch = "wasm32"))]
    let rt = crate::runtime::rt_current();

    Box::new(HewActor {
        sched_link_next: AtomicPtr::new(ptr::null_mut()),
        id: actor_id,
        state: config.state,
        state_size: config.state_size,
        dispatch: config.dispatch,
        mailbox: config.mailbox,
        actor_state: AtomicI32::new(HewActorState::Idle as i32),
        budget: AtomicI32::new(config.budget),
        init_state,
        init_state_size: config.state_size,
        coalesce_key_fn: config.coalesce_key_fn,
        terminate_fn: None,
        state_drop_fn: None,
        state_clone_fn: None,
        terminate_called: AtomicBool::new(false),
        terminate_finished: AtomicBool::new(false),
        dispatch_active: AtomicBool::new(false),
        error_code: AtomicI32::new(0),
        supervisor: ptr::null_mut(),
        supervisor_child_index: -1,
        priority: AtomicI32::new(HEW_PRIORITY_NORMAL),
        reductions: AtomicI32::new(HEW_DEFAULT_REDUCTIONS),
        idle_count: AtomicI32::new(0),
        hibernation_threshold: AtomicI32::new(0),
        hibernating: AtomicI32::new(0),
        prof_messages_processed: AtomicU64::new(0),
        prof_processing_time_ns: AtomicU64::new(0),
        #[cfg(not(target_arch = "wasm32"))]
        arena,
        #[cfg(target_arch = "wasm32")]
        arena: arena.cast::<c_void>(),
        suspended_cont: AtomicPtr::new(ptr::null_mut()),
        cont_tag: AtomicI32::new(crate::internal::types::ContTag::Empty as i32),
        pending_wake: AtomicBool::new(false),
        suspended_reply_channel: AtomicPtr::new(std::ptr::null_mut()),
        suspended_cancel_token: AtomicPtr::new(std::ptr::null_mut()),
        // Stamp the spawning runtime's id. Native resolves the runtime this
        // spawn runs under (TLS-first via `rt_current`); single-runtime
        // programs always resolve `RuntimeId::DEFAULT`. The wasm cooperative
        // runtime is single-runtime by construction and has no `rt_current`,
        // so it stamps `DEFAULT` directly.
        #[cfg(not(target_arch = "wasm32"))]
        runtime_id: rt.runtime_id(),
        #[cfg(target_arch = "wasm32")]
        runtime_id: crate::runtime_id::RuntimeId::DEFAULT,
        #[cfg(not(target_arch = "wasm32"))]
        runtime: rt as *const crate::runtime::RuntimeInner,
        #[cfg(target_arch = "wasm32")]
        runtime: ptr::null(),
        send_pin_count: AtomicU32::new(0),
        gen_sink: AtomicPtr::new(ptr::null_mut()),
        local_pid_id: crate::lifetime::local_handles::HewLocalPidId::INVALID,
    })
}

#[cfg(not(target_arch = "wasm32"))]
unsafe fn finalize_spawned_actor(raw: *mut HewActor, actor_id: u64) -> bool {
    // SAFETY: same caller guarantee; `runtime_id` is initialized before track.
    let runtime_id = unsafe { (*raw).runtime_id };
    // SAFETY: native spawn stamps this non-null owner from the same runtime
    // read as `runtime_id`; the owner outlives actor construction and routes.
    let owner = unsafe { &*(*raw).runtime };
    let publication =
        match crate::lifetime::local_handles::begin_actor_publication_in(&owner.local_handles) {
            Ok(publication) => publication,
            Err(error) => {
                crate::set_last_error(format!(
                    "hew_actor_spawn: local handle publication failed: {error:?}"
                ));
                return false;
            }
        };
    let token = match publication.register_actor(runtime_id, actor_id) {
        Ok(token) => token,
        Err(error) => {
            crate::set_last_error(format!(
                "hew_actor_spawn: local handle allocation failed: {error:?}"
            ));
            return false;
        }
    };
    // Initialise the complete semantic identity before publishing liveness.
    // SAFETY: raw is owned by this unpublished spawn and fully initialised.
    unsafe { (*raw).local_pid_id = token };
    #[cfg(test)]
    run_spawn_publication_hook();
    // SAFETY: caller guarantees raw is valid and fully initialised.
    if !unsafe { live_actors::track_actor(raw) } {
        let retired = publication.retire_actor(token, actor_id);
        debug_assert_eq!(
            retired,
            crate::lifetime::local_handles::RetireActorResult::Retired
        );
        crate::set_last_error(format!(
            "hew_actor_spawn: actor identity collision: {actor_id}"
        ));
        return false;
    }
    #[cfg(feature = "profiler")]
    // SAFETY: `raw` was just allocated by `Box::into_raw` and is valid.
    unsafe {
        crate::profiler::actor_registry::register(raw);
    };
    crate::tracing::hew_trace_lifecycle(actor_id, crate::tracing::SPAN_SPAWN);
    true
}

#[cfg(target_arch = "wasm32")]
unsafe fn finalize_spawned_actor(raw: *mut HewActor, actor_id: u64) -> bool {
    let publication = match crate::lifetime::local_handles::begin_current_actor_publication() {
        Ok(publication) => publication,
        Err(error) => {
            crate::set_last_error(format!(
                "hew_actor_spawn: local handle publication failed: {error:?}"
            ));
            return false;
        }
    };
    let token = match publication.register_actor(crate::runtime_id::RuntimeId::DEFAULT, actor_id) {
        Ok(token) => token,
        Err(error) => {
            crate::set_last_error(format!(
                "hew_actor_spawn: local handle allocation failed: {error:?}"
            ));
            return false;
        }
    };
    unsafe { (*raw).local_pid_id = token };
    // SAFETY: caller guarantees raw is valid and fully initialised.
    if !unsafe { live_actors::track_actor(raw) } {
        let retired = publication.retire_actor(token, actor_id);
        debug_assert_eq!(
            retired,
            crate::lifetime::local_handles::RetireActorResult::Retired
        );
        crate::set_last_error(format!(
            "hew_actor_spawn: actor identity collision: {actor_id}"
        ));
        return false;
    }
    true
}

/// Allocate the per-actor arena for a native spawn.
///
/// Centralises the `cap_bytes == 0` / `cap_bytes > 0` branch and provides a
/// test-only injection point: when `FAIL_ARENA_ALLOC_NEXT` is set, the first
/// call returns null to simulate an OOM on the arena allocation step.
#[cfg(not(target_arch = "wasm32"))]
fn alloc_actor_arena(cap_bytes: usize) -> *mut crate::arena::ActorArena {
    #[cfg(test)]
    if should_fail_arena_alloc() {
        return ptr::null_mut();
    }
    if cap_bytes > 0 {
        crate::arena::hew_arena_new_with_cap(cap_bytes)
    } else {
        crate::arena::hew_arena_new()
    }
}

/// Shared implementation for all native actor spawn functions.
///
/// # Safety
///
/// - `config.state` must be a deep-copied allocation (or null for zero-sized state).
/// - `config.mailbox` must be a valid mailbox pointer (already configured).
#[cfg(not(target_arch = "wasm32"))]
unsafe fn spawn_actor_internal(config: ActorSpawnConfig) -> *mut HewActor {
    // Adopt-state path (set by hew_actor_spawn_opts_adopt): `config.state` is
    // already an independently-owned deep clone (state_clone_fn output); skip
    // the second `deep_copy_state` and leave `init_state` null so the spec's
    // clone-template is the sole source-of-truth for future restarts. Avoids
    // byte-aliasing the cloned wrapper's owned fields with a sibling
    // `init_state` byte-copy (root cause of C1).
    let init_state = if config.adopt {
        ptr::null_mut()
    } else {
        // SAFETY: Caller already deep-copied state; make a second copy for restart.
        unsafe { deep_copy_state(config.state, config.state_size) }
    };

    // OOM on the restart-state copy: free resources the caller transferred
    // ownership of and propagate the failure as null. Skipped on the adopt
    // path because we never allocated `init_state`.
    if !config.adopt && !config.state.is_null() && config.state_size > 0 && init_state.is_null() {
        // SAFETY: `config` still owns the transferred state/mailbox on this failure path.
        unsafe { cleanup_failed_spawn(&config, ptr::null_mut()) };
        return ptr::null_mut();
    }

    // Allocate the per-actor arena bump allocator.  Mirror the wasm path:
    // if allocation fails, free all resources already owned and return null.
    let arena = alloc_actor_arena(config.cap_bytes);
    if arena.is_null() {
        // SAFETY: `init_state` was created above and ownership has not been
        // transferred.  `cleanup_failed_spawn` also frees `config.state` and
        // the mailbox; on the adopt path `init_state` is null (no extra alloc).
        unsafe { cleanup_failed_spawn(&config, init_state) };
        return ptr::null_mut();
    }
    let actor_id = next_spawn_actor_identity();
    let actor = build_spawned_actor(config, actor_id, init_state, arena);
    let raw = Box::into_raw(actor);
    register_actor_state_lock(raw);
    // SAFETY: `raw` comes from `Box::into_raw` and has not yet been tracked.
    if !unsafe { finalize_spawned_actor(raw, actor_id) } {
        // SAFETY: registration failed after liveness was rolled back; no caller
        // or scheduler can observe `raw`.
        unsafe { free_actor_resources_with_options(raw, false) };
        return ptr::null_mut();
    }
    raw
}

/// Shared implementation for all WASM actor spawn functions.
///
/// # Safety
///
/// Same requirements as [`spawn_actor_internal`] but for WASM targets.
#[cfg(target_arch = "wasm32")]
unsafe fn spawn_actor_internal(config: ActorSpawnConfig) -> *mut HewActor {
    // Adopt-state path: see native fork for rationale.
    let init_state = if config.adopt {
        ptr::null_mut()
    } else {
        // SAFETY: Caller already deep-copied state; make a second copy for restart.
        unsafe { deep_copy_state(config.state, config.state_size) }
    };

    // OOM on the restart-state copy: free resources the caller transferred
    // ownership of and propagate the failure as null. Skipped on the adopt
    // path because we never allocated `init_state`.
    if !config.adopt && !config.state.is_null() && config.state_size > 0 && init_state.is_null() {
        // SAFETY: `config` still owns the transferred state/mailbox on this failure path.
        unsafe { cleanup_failed_spawn(&config, ptr::null_mut()) };
        return ptr::null_mut();
    }

    // Allocate the per-actor arena bump allocator.  Mirror the native path:
    // if allocation fails, free all resources already owned and return null.
    let arena = if config.cap_bytes > 0 {
        crate::arena::hew_arena_new_with_cap(config.cap_bytes)
    } else {
        crate::arena::hew_arena_new()
    };
    if arena.is_null() {
        // SAFETY: `init_state` was created above and ownership has not been transferred.
        // On the adopt path init_state is null (no allocation to release here);
        // `cleanup_failed_spawn` will still libc::free `config.state` (the
        // adopted clone wrapper).
        unsafe { cleanup_failed_spawn(&config, init_state) };
        return ptr::null_mut();
    }

    let actor_id = next_spawn_actor_identity();
    let actor = build_spawned_actor(config, actor_id, init_state, arena);
    let raw = Box::into_raw(actor);
    register_actor_state_lock(raw);
    // SAFETY: `raw` comes from `Box::into_raw` and has not yet been tracked.
    if !unsafe { finalize_spawned_actor(raw, actor_id) } {
        // SAFETY: registration failed after liveness was rolled back; no caller
        // or scheduler can observe `raw`.
        unsafe { free_actor_resources_with_options(raw, false) };
        return ptr::null_mut();
    }
    raw
}

/// Spawn a new actor with an unbounded mailbox.
///
/// The initial state is deep-copied. The returned pointer must be freed
/// with [`hew_actor_free`]. Returns null on allocation failure
/// (details via [`hew_last_error`]).
///
/// # Safety
///
/// - `state` must point to at least `state_size` readable bytes, or be
///   null when `state_size` is 0.
/// - `dispatch` will be called from worker threads with the actor's
///   state pointer.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_spawn(
    state: *mut c_void,
    state_size: usize,
    dispatch: Option<HewDispatchFn>,
) -> *mut HewActor {
    // SAFETY: Caller guarantees `state` validity.
    let actor_state = unsafe { deep_copy_state(state, state_size) };
    if !state.is_null() && state_size > 0 && actor_state.is_null() {
        return ptr::null_mut();
    }

    // SAFETY: hew_mailbox_new returns a valid pointer.
    let mailbox = unsafe { mailbox::hew_mailbox_new() };
    // SAFETY: mailbox pointer is valid.
    unsafe {
        mailbox::hew_mailbox_set_coalesce_config(mailbox, None, HewOverflowPolicy::DropOld);
    }

    // SAFETY: actor_state is a fresh deep-copy; mailbox is valid.
    unsafe {
        spawn_actor_internal(ActorSpawnConfig {
            state: actor_state,
            state_size,
            dispatch,
            mailbox: mailbox.cast(),
            budget: HEW_MSG_BUDGET,
            coalesce_key_fn: None,
            cycle_capable: false,
            cap_bytes: 0,
            adopt: false,
        })
    }
}

/// Spawn a new actor from a [`HewActorOpts`] struct.
///
/// Uses a bounded mailbox if `opts.mailbox_capacity > 0`.
///
/// # Safety
///
/// - `opts` must be a valid pointer to a [`HewActorOpts`].
/// - Same state/dispatch requirements as [`hew_actor_spawn`].
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_spawn_opts(opts: *const HewActorOpts) -> *mut HewActor {
    if opts.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: Caller guarantees `opts` is valid.
    let opts = unsafe { &*opts };

    // SAFETY: Caller guarantees state validity.
    let actor_state = unsafe { deep_copy_state(opts.init_state, opts.state_size) };
    if !opts.init_state.is_null() && opts.state_size > 0 && actor_state.is_null() {
        return ptr::null_mut();
    }

    let mailbox = if opts.mailbox_capacity > 0 {
        let capacity = usize::try_from(opts.mailbox_capacity).unwrap_or(usize::MAX);
        let policy = parse_overflow_policy(opts.overflow);
        // SAFETY: Returns a valid pointer.
        unsafe { mailbox::hew_mailbox_new_with_policy(capacity, policy) }
    } else {
        // SAFETY: Returns a valid pointer.
        unsafe { mailbox::hew_mailbox_new() }
    };
    let coalesce_fallback = parse_overflow_policy(opts.coalesce_fallback);
    // SAFETY: mailbox pointer is valid.
    unsafe {
        mailbox::hew_mailbox_set_coalesce_config(mailbox, opts.coalesce_key_fn, coalesce_fallback);
        mailbox::hew_mailbox_set_message_drop_fn(mailbox, opts.message_drop_fn);
    }

    let budget = if opts.budget > 0 {
        opts.budget
    } else {
        HEW_MSG_BUDGET
    };

    // SAFETY: actor_state is a fresh deep-copy; mailbox is valid.
    unsafe {
        spawn_actor_internal(ActorSpawnConfig {
            state: actor_state,
            state_size: opts.state_size,
            dispatch: opts.dispatch,
            mailbox: mailbox.cast(),
            budget,
            coalesce_key_fn: opts.coalesce_key_fn,
            cycle_capable: opts.cycle_capable != 0,
            cap_bytes: opts.arena_cap_bytes,
            adopt: false,
        })
    }
}

/// Spawn a new actor that adopts a pre-built deep-clone of its initial state.
///
/// Companion to [`hew_actor_spawn_opts`] for supervisor-restart and other
/// clone-aware spawn paths. The caller passes a freshly heap-allocated state
/// wrapper (`cloned_state`) — typically the return value of the actor's
/// codegen-emitted [`HewStateCloneFn`] — and this function consumes ownership
/// of that allocation, wiring it directly into the new actor's `state` slot
/// **without** an additional `deep_copy_state` (which would byte-alias the
/// cloned wrapper's owned fields and re-introduce the C1 UAF) and **without**
/// allocating an `init_state` byte-copy alongside it.
///
/// `opts.init_state` is ignored on this path; the runtime reads only the
/// scalar opts (`state_size`, `dispatch`, mailbox/overflow/coalesce/budget,
/// arena cap, cycle bit) and the adopted `cloned_state`.
///
/// **Ownership / failure**: on success, the returned actor owns
/// `cloned_state` (released via `state_drop_fn` + `libc::free` at teardown).
/// On failure (null return), this function performs a raw `libc::free` of
/// `cloned_state`. The caller's `state_drop_fn` is **not** invoked on the
/// failure path, so any owned heap fields inside the wrapper are leaked.
/// This is a known Lane A1 limitation (proper failure-path drop is Lane A3
/// work). Callers wanting safe failure cleanup should null-check the spawn
/// result and call `state_drop_fn(cloned_state); libc::free(cloned_state);`
/// themselves before returning — but they must NOT then call this function
/// (i.e. they must pre-allocate via a probe). In practice the supervisor
/// restart path tolerates the leak because spawn-failure here implies
/// system-wide OOM and the supervisor will escalate.
///
/// Chosen over an `adopt_state: bool` field on `HewActorOpts` because the
/// LLVM codegen at `hew-codegen-rs/src/llvm.rs` builds the opts struct as a
/// fixed-shape 10-field literal — appending an `adopt_state` bit would read
/// uninitialized stack pad from codegen-emitted callers and spuriously
/// consume their borrowed `init_state` pointer (UAF). A separate ABI entry
/// is forward-compatible without touching codegen.
///
/// # Safety
///
/// - `opts` must be a valid pointer to a [`HewActorOpts`].
/// - `cloned_state` must be either null (when `state_size == 0`) or a
///   `malloc`-compatible heap allocation of exactly `opts.state_size` bytes
///   whose owned fields are independent deep clones (the
///   [`HewStateCloneFn`] contract).
/// - After this call returns, the caller MUST NOT free or otherwise touch
///   `cloned_state`; ownership has transferred.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_spawn_opts_adopt(
    opts: *const HewActorOpts,
    cloned_state: *mut c_void,
) -> *mut HewActor {
    if opts.is_null() {
        // Caller still owns `cloned_state`; we have no way to release it
        // safely (no state_drop_fn in scope), so we must not free it here.
        // Returning null with the pointer retained matches the precondition.
        return ptr::null_mut();
    }
    // SAFETY: Caller guarantees `opts` is valid.
    let opts = unsafe { &*opts };

    let mailbox = if opts.mailbox_capacity > 0 {
        let capacity = usize::try_from(opts.mailbox_capacity).unwrap_or(usize::MAX);
        let policy = parse_overflow_policy(opts.overflow);
        // SAFETY: Returns a valid pointer.
        unsafe { mailbox::hew_mailbox_new_with_policy(capacity, policy) }
    } else {
        // SAFETY: Returns a valid pointer.
        unsafe { mailbox::hew_mailbox_new() }
    };
    let coalesce_fallback = parse_overflow_policy(opts.coalesce_fallback);
    // SAFETY: mailbox pointer is valid.
    unsafe {
        mailbox::hew_mailbox_set_coalesce_config(mailbox, opts.coalesce_key_fn, coalesce_fallback);
        mailbox::hew_mailbox_set_message_drop_fn(mailbox, opts.message_drop_fn);
    }

    let budget = if opts.budget > 0 {
        opts.budget
    } else {
        HEW_MSG_BUDGET
    };

    // SAFETY: cloned_state ownership has been transferred to us; mailbox is valid.
    unsafe {
        spawn_actor_internal(ActorSpawnConfig {
            state: cloned_state,
            state_size: opts.state_size,
            dispatch: opts.dispatch,
            mailbox: mailbox.cast(),
            budget,
            coalesce_key_fn: opts.coalesce_key_fn,
            cycle_capable: opts.cycle_capable != 0,
            cap_bytes: opts.arena_cap_bytes,
            adopt: true,
        })
    }
}

/// WASM fork of [`hew_actor_spawn_opts_adopt`]. Same contract.
///
/// # Safety
///
/// Same requirements as the native [`hew_actor_spawn_opts_adopt`].
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_spawn_opts_adopt(
    opts: *const HewActorOpts,
    cloned_state: *mut c_void,
) -> *mut HewActor {
    if opts.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: Caller guarantees `opts` is valid.
    let opts = unsafe { &*opts };

    let mailbox = if opts.mailbox_capacity > 0 {
        let capacity = usize::try_from(opts.mailbox_capacity).unwrap_or(usize::MAX);
        let policy = parse_overflow_policy(opts.overflow);
        // SAFETY: Trusted FFI constructor.
        unsafe { hew_mailbox_new_with_policy(capacity, policy) }
    } else {
        // SAFETY: Trusted FFI constructor for an unbounded mailbox.
        unsafe { hew_mailbox_new() }
    };
    let coalesce_fallback = parse_overflow_policy(opts.coalesce_fallback);
    // SAFETY: mailbox is a valid WASM mailbox pointer created above.
    unsafe {
        crate::mailbox_wasm::hew_mailbox_set_coalesce_config(
            mailbox.cast::<crate::mailbox_wasm::HewMailboxWasm>(),
            opts.coalesce_key_fn,
            coalesce_fallback,
        );
        crate::mailbox_wasm::hew_mailbox_set_message_drop_fn(
            mailbox.cast::<crate::mailbox_wasm::HewMailboxWasm>(),
            opts.message_drop_fn,
        );
    }

    let budget = if opts.budget > 0 {
        opts.budget
    } else {
        HEW_MSG_BUDGET
    };

    // SAFETY: cloned_state ownership has been transferred to us; mailbox is valid.
    unsafe {
        spawn_actor_internal(ActorSpawnConfig {
            state: cloned_state,
            state_size: opts.state_size,
            dispatch: opts.dispatch,
            mailbox,
            budget,
            coalesce_key_fn: opts.coalesce_key_fn,
            cycle_capable: opts.cycle_capable != 0,
            cap_bytes: opts.arena_cap_bytes,
            adopt: true,
        })
    }
}

/// Spawn a new actor with a bounded mailbox.
///
/// # Safety
///
/// Same requirements as [`hew_actor_spawn`].
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_spawn_bounded(
    state: *mut c_void,
    state_size: usize,
    dispatch: Option<HewDispatchFn>,
    capacity: i32,
) -> *mut HewActor {
    // SAFETY: Caller guarantees `state` validity.
    let actor_state = unsafe { deep_copy_state(state, state_size) };
    if !state.is_null() && state_size > 0 && actor_state.is_null() {
        return ptr::null_mut();
    }

    // SAFETY: Returns a valid pointer.
    let mailbox = unsafe { mailbox::hew_mailbox_new_bounded(capacity) };
    // SAFETY: mailbox pointer is valid.
    unsafe {
        mailbox::hew_mailbox_set_coalesce_config(mailbox, None, HewOverflowPolicy::DropOld);
    }

    // SAFETY: actor_state is a fresh deep-copy; mailbox is valid.
    unsafe {
        spawn_actor_internal(ActorSpawnConfig {
            state: actor_state,
            state_size,
            dispatch,
            mailbox: mailbox.cast(),
            budget: HEW_MSG_BUDGET,
            coalesce_key_fn: None,
            cycle_capable: false,
            cap_bytes: 0,
            adopt: false,
        })
    }
}

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
#[cfg(not(target_arch = "wasm32"))]
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

/// Send an envelope-aliased message to an actor.
///
/// The caller transfers exactly one refcount on `envelope`. This is the
/// runtime entry for the codegen `SendAliasMode::Alias` lowering: the
/// sender's already-owned payload is wrapped in a refcounted
/// [`HewMsgEnvelope`] and delivered by reference instead of being
/// deep-copied, with the move-checker invalidating the sender's binding
/// so no observable alias survives.
///
/// # Single-release contract
///
/// The envelope refcount is consumed **exactly once** on every exit:
///
/// - **Null actor** — release the refcount directly and return (an
///   absent/dead actor is a normal outcome, not a fault, so we do not
///   panic).
/// - **Drop-fault injection** (deterministic test harness) — the message
///   is silently discarded and the receiver never consumes the payload,
///   so we release the refcount directly.
/// - **Otherwise** — delegate to [`crate::mailbox::hew_mailbox_send_aliased`],
///   which consumes the refcount on every outcome (enqueued node → freed
///   on dispatch/drain; rejected → released immediately). After that call
///   the envelope must not be touched again.
///
/// On a successful enqueue the destination actor is woken via
/// [`schedule_actor_after_enqueue`], mirroring the copy-mode path.
///
/// # Safety
///
/// - `actor` may be null; if non-null it must be a valid actor pointer
///   under the same liveness contract as [`hew_actor_send`].
/// - `envelope` must carry exactly one caller-transferred refcount (from
///   [`crate::mailbox::hew_msg_envelope_new`] /
///   [`crate::mailbox::hew_msg_envelope_clone_alias`]), or be null.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_send_aliased(
    actor: *mut HewActor,
    msg_type: i32,
    envelope: *mut crate::mailbox::HewMsgEnvelope,
) {
    if actor.is_null() {
        // EXIT(null-actor): no destination. Release the
        // caller-transferred refcount exactly once so the buffer does
        // not leak, then return cleanly (a dead/absent actor is normal —
        // do not panic).
        if !envelope.is_null() {
            // SAFETY: caller transferred one refcount on `envelope`.
            unsafe { crate::mailbox::hew_msg_envelope_release(envelope) };
        }
        return;
    }

    // SAFETY: caller guarantees `actor` is valid (same liveness contract
    // as `hew_actor_send`).
    let a = unsafe { &*actor };

    // EXIT(cross-runtime): the actor belongs to a different runtime than the
    // caller. Fail closed without routing the foreign pointer, releasing the
    // caller-transferred refcount exactly once so the buffer does not leak.
    // Never fires single-runtime.
    if !actor_runtime_matches(a) {
        if !envelope.is_null() {
            // SAFETY: caller transferred one refcount on `envelope`.
            unsafe { crate::mailbox::hew_msg_envelope_release(envelope) };
        }
        return;
    }

    // EXIT(terminal): the actor is terminal (Crashed/Stopped). Reject before
    // touching the mailbox — see `actor_send_is_terminal`. The trap takes the
    // terminal CAS before closing the mailbox, so without this gate an alias
    // send racing that window would consume the caller's refcount into an
    // undeliverable enqueued node and report success. Release the single
    // caller-transferred refcount exactly once (same outcome as a send to a
    // closed mailbox) and return.
    if actor_send_is_terminal(a) {
        if !envelope.is_null() {
            // SAFETY: caller transferred one refcount on `envelope`.
            unsafe { crate::mailbox::hew_msg_envelope_release(envelope) };
        }
        return;
    }

    // EXIT(drop-fault-injection): the deterministic harness asks us to
    // silently discard this message. The receiver never consumes the
    // payload, so the alias path must release the envelope here — the
    // copy path has no buffer to free, but we own one refcount.
    if crate::deterministic::check_drop_fault(a.id) {
        if !envelope.is_null() {
            // SAFETY: caller transferred one refcount on `envelope`.
            unsafe { crate::mailbox::hew_msg_envelope_release(envelope) };
        }
        return;
    }

    let mb = a.mailbox.cast::<crate::mailbox::HewMailbox>();
    // Delegate to the mailbox alias-enqueue, which consumes the single
    // envelope refcount on every outcome. We must NOT touch `envelope`
    // after this call.
    // SAFETY: `mb` is valid for the actor's lifetime; `envelope` carries
    // the single caller-transferred refcount.
    let result = unsafe { crate::mailbox::hew_mailbox_send_aliased(mb, msg_type, envelope) };
    if result == HewError::Ok as i32 {
        // SAFETY: `actor`/`a` valid; delivery succeeded so the actor may run.
        unsafe { schedule_actor_after_enqueue(actor, a, msg_type) };
    }
}

/// WASM stub for [`hew_actor_send_aliased`] — **fail-closed**.
///
// WASM-TODO(#1451): alias-send WASM routing deferred to the WASM send gate (the gate covers it).
/// The native entry above delivers aliased sends via the envelope-mode
/// enqueue, but the WASM mailbox routing for the alias path is not yet
/// wired. Until then this stub releases the caller-transferred envelope
/// refcount (so the buffer container does not leak) and aborts via
/// [`hew_panic`] rather than silently dropping or mis-delivering.
///
/// # Safety
///
/// - `envelope` may be null; if non-null it carries exactly one
///   caller-transferred refcount that this stub releases before aborting.
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_send_aliased(
    _actor: *mut HewActor,
    _msg_type: i32,
    envelope: *mut crate::mailbox_wasm::HewMsgEnvelope,
) {
    if !envelope.is_null() {
        // SAFETY: caller transferred one refcount on `envelope`;
        // release it so the buffer container does not leak when we
        // abort below.
        unsafe { crate::mailbox_wasm::hew_msg_envelope_release(envelope) };
    }
    hew_panic();
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

/// Send a wire-encoded message to an actor on wasm32.
///
/// Extracts raw bytes from the `HewVec` (bytes type), deep-copies them into the
/// cooperative mailbox, wakes the target actor when delivery succeeds, and
/// frees the temporary `HewVec` in all cases.
///
/// # Safety
///
/// - `actor` must be a valid pointer returned by a spawn function.
/// - `bytes` must be a valid `HewVec*` (bytes type) or null.
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_send_wire(
    actor: *mut HewActor,
    msg_type: i32,
    bytes: *mut crate::vec::HewVec,
) {
    if bytes.is_null() {
        return;
    }

    if actor.is_null() {
        // SAFETY: bytes was allocated by hew_vec and must be released on early return.
        unsafe { crate::vec::hew_vec_free(bytes) };
        return;
    }

    // SAFETY: bytes is a valid HewVec. Extract raw byte data before freeing it.
    let data = unsafe { crate::vec::hwvec_to_u8(bytes) };
    let data_ptr = if data.is_empty() {
        ptr::null_mut()
    } else {
        data.as_ptr().cast_mut().cast()
    };

    // SAFETY: actor is valid and owns a wasm mailbox for its lifetime.
    let result = unsafe {
        crate::mailbox_wasm::hew_mailbox_send(
            (*actor).mailbox.cast(),
            msg_type,
            data_ptr,
            data.len(),
        )
    };
    if result == HewError::Ok as i32 {
        // SAFETY: actor is valid and delivery succeeded, so the scheduler may run it.
        unsafe { wake_wasm_actor(actor) };
    }

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
    // overflow) keeps its own distinct non-zero code; only the
    // deliberately-silent `DropNew`/`DropOld`/`Coalesce` policy outcomes
    // come back as `Ok`. Collapsing these into a single reused code (as a
    // prior version of this function did) is what let a genuine send
    // failure hide behind the same value as a declared-silent overflow drop.
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
        #[cfg(target_arch = "wasm32")]
        // SAFETY: the actor is pinned and data follows this function's contract.
        unsafe {
            hew_actor_try_send(actor, msg_type, data, size)
        }
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

    // CAS IDLE → RUNNABLE; on success, schedule the actor.
    if a.actor_state
        .compare_exchange(
            HewActorState::Idle as i32,
            HewActorState::Runnable as i32,
            Ordering::AcqRel,
            Ordering::Acquire,
        )
        .is_ok()
    {
        scheduler::sched_enqueue(actor);
    }

    0
}

/// Guaranteed (non-blocking, non-dropping) send for a terminal/out-of-band
/// event that must survive a full mailbox under data backpressure.
///
/// Unlike [`hew_actor_try_send`], the enqueue **bypasses the bounded-capacity
/// overflow policy** ([`mailbox::hew_mailbox_send_guaranteed`]): the message is
/// appended to the tail of the user queue even when the mailbox is at capacity,
/// so it is never silently dropped. It is still **non-blocking** — it never
/// waits on the mailbox condvar — so the calling thread (the single active-mode
/// reactor thread) is never stalled and can never deadlock with the synchronous
/// actor-teardown path that spin-waits on the in-flight-delivery guard.
///
/// FIFO is preserved: the event lands behind every already-queued message
/// (the user queue, not the priority system queue), so a terminal `on_close`
/// never overtakes buffered `on_data`.
///
/// Returns `0` on success. A non-zero return means the mailbox is closed or
/// allocation failed — for a terminal event both mean the actor is already
/// gone, so there is nothing left to deliver.
///
/// # Safety
///
/// Same requirements as [`hew_actor_try_send`].
#[cfg(not(target_arch = "wasm32"))]
pub(crate) unsafe fn hew_actor_send_guaranteed(
    actor: *mut HewActor,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
) -> i32 {
    if actor.is_null() {
        return HewError::ErrActorStopped as i32;
    }
    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };
    // Fail closed on a cross-runtime pointer (never fires single-runtime).
    if !actor_runtime_matches(a) {
        return HewError::ErrForeignRuntime as i32;
    }
    // Terminal-state send gate (see `actor_send_is_terminal`): a terminal actor
    // is already gone, so the out-of-band terminal event is moot. Reject before
    // the mailbox even if it is not yet closed, matching the closed-mailbox
    // outcome ("nothing left to deliver") and closing the trap's
    // terminal-CAS-before-mailbox-close window.
    if actor_send_is_terminal(a) {
        return HewError::ErrActorStopped as i32;
    }
    let mb = a.mailbox.cast::<HewMailbox>();

    // SAFETY: Mailbox is valid for the actor's lifetime.
    let result = unsafe { mailbox::hew_mailbox_send_guaranteed(mb, msg_type, data, size) };
    if result != 0 {
        return result;
    }

    // CAS IDLE → RUNNABLE; on success, schedule the actor so it drains the
    // terminal event (and the buffered messages ahead of it).
    if a.actor_state
        .compare_exchange(
            HewActorState::Idle as i32,
            HewActorState::Runnable as i32,
            Ordering::AcqRel,
            Ordering::Acquire,
        )
        .is_ok()
    {
        scheduler::sched_enqueue(actor);
    }

    0
}

// ── Close / Stop / Free ─────────────────────────────────────────────────

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

    // If actor is IDLE, transition directly to STOPPED.
    if a.actor_state
        .compare_exchange(
            HewActorState::Idle as i32,
            HewActorState::Stopped as i32,
            Ordering::AcqRel,
            Ordering::Acquire,
        )
        .is_ok()
    {
        crate::tracing::hew_trace_lifecycle(a.id, crate::tracing::SPAN_STOP);
        // SAFETY: actor just transitioned to Stopped; not being dispatched.
        unsafe { call_terminate_fn(actor) };
    }
}

/// Stop an actor, sending a system shutdown message.
///
/// Closes the mailbox, transitions idle actors directly to `Stopped`, and
/// only enqueues a shutdown system message (`msg_type = -1`) when the actor is
/// already `Running`. Runnable actors already have a queued activation, so
/// closing their mailbox is enough to let that activation drain naturally to
/// `Stopped` without fabricating an extra system-queue entry.
///
/// # Safety
///
/// `actor` must be a valid pointer returned by a spawn function.
#[cfg(not(target_arch = "wasm32"))]
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

    if a.actor_state
        .compare_exchange(
            HewActorState::Idle as i32,
            HewActorState::Stopped as i32,
            Ordering::AcqRel,
            Ordering::Acquire,
        )
        .is_ok()
    {
        crate::tracing::hew_trace_lifecycle(a.id, crate::tracing::SPAN_STOP);
        // SAFETY: actor just transitioned to Stopped; not being dispatched.
        unsafe { call_terminate_fn(actor) };
        return;
    }

    let state = a.actor_state.load(Ordering::Acquire);
    if state != HewActorState::Running as i32 {
        return;
    }

    if !mb.is_null() {
        // Running actors are already inside a dispatch; enqueue one sys
        // message (-1) so the next mailbox poll observes the shutdown
        // request. Runnable actors already have queued work and do not need an
        // extra wake-up signal just to notice that the mailbox closed.
        // SAFETY: Mailbox is valid for the actor's lifetime.
        unsafe {
            let _ = mailbox::mailbox_send_stop_sys_once(mb);
        }
    }
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
    unsafe { hew_actor_free_inner(actor, false) }
}

/// Free an actor and all associated resources, suppressing the codegen-emitted
/// `state_drop_fn` invocation on the live state.
///
/// Used by supervisor restart paths (`ONE_FOR_ALL` / `REST_FOR_ONE`) for
/// non-crashed siblings whose state is about to be reused by the next spawn.
/// See [`free_actor_resources_with_options`] for the alias-chain rationale.
///
/// # Safety
///
/// - `actor` must have been returned by a spawn function.
/// - The actor must not be used after this call.
/// - Caller (the supervisor) guarantees the actor's `state` field allocations
///   are still owned by the supervisor's `spec.init_state` byte-aliasing and
///   will be freed exactly once during `hew_supervisor_stop`.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) unsafe fn hew_actor_free_for_restart(actor: *mut HewActor) -> c_int {
    // SAFETY: caller forwards the same invariants the inner requires.
    unsafe { hew_actor_free_inner(actor, true) }
}

#[cfg(not(target_arch = "wasm32"))]
unsafe fn hew_actor_free_inner(actor: *mut HewActor, suppress_state_drop: bool) -> c_int {
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
    // `finalize_quiescent_actor_cleanup_with_options`, which runs the terminate
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
    // makes the live suspend edge non-leaking. Dormant today (no actor reaches
    // `Suspended` while the source surface stays thread-parked).
    if crate::coro_exec::has_live_parked_cont(a) {
        // SAFETY: `a` is the actor being freed; `destroy_parked`'s CAS guards
        // serialise against any concurrent resume (FG1/FG2).
        let destroyed = unsafe { crate::coro_exec::destroy_parked(a) };
        // The parked frame is gone; latch the abandoned actor out of the
        // non-quiescent `Suspended` window into `Stopped` so the quiescence wait
        // below passes instead of spinning to the deadline. Only this teardown
        // owns the slot (it won the `… → Destroyed` CAS), so the state CAS is
        // race-free against a resume (which would have refused the destroy).
        if destroyed.is_ok() {
            clear_suspended_cancel_token(a);
            // Risk 1: `destroy_parked` above just ran the pump frame's
            // `coro.destroy` cleanup outline, which releases the generator
            // companion (heap env + coro handle) living as a local INSIDE
            // that frame via its normal scope-exit drop
            // (`hew_gen_coro_destroy`) — exactly once, and NOT touched here.
            // This call's sole job is the separate SINK: if this parked
            // activation was a `receive gen fn` pump, fault-close its
            // still-registered sink so the consumer awaiting the stream
            // observes the fault instead of hanging on a stream whose
            // producer will never resume. A no-op if nothing is registered
            // (this was not a gen-stream pump).
            fault_close_registered_gen_sink(a);
            let _ = a.actor_state.compare_exchange(
                HewActorState::Suspended as i32,
                HewActorState::Stopped as i32,
                Ordering::AcqRel,
                Ordering::Acquire,
            );
        }
    }

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
            if actor_free_state_is_quiescent(state) && !a.dispatch_active.load(Ordering::Acquire) {
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
        if a.send_pin_count.load(Ordering::Acquire) == 0 {
            break;
        }
        if std::time::Instant::now() >= drain_deadline {
            // An outstanding pin may still insert a relationship, so no final
            // scrub is safe on this fail-closed allocation leak.
            crate::set_last_error("hew_actor_free: send pins did not drain after timeout");
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
    unsafe { finalize_quiescent_actor_cleanup_with_options(actor, state, suppress_state_drop) };
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
fn collect_pending_actor(id: ActorId) -> Option<(ActorId, *mut HewActor)> {
    live_actors::get_actor_ptr_by_id(id).map(|actor| (id, actor))
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
/// `expected` must be a valid, quiescent actor pointer that is still tracked
/// in `LIVE_ACTORS`.
#[cfg(not(target_arch = "wasm32"))]
unsafe fn drain_quiesced_actor(
    actor_id: ActorId,
    expected: *mut HewActor,
    deadline: std::time::Instant,
) {
    // SAFETY: caller guarantees `expected` is quiescent and still tracked in
    // LIVE_ACTORS. prepare_quiescent_actor_for_cleanup must run before
    // untracking so that any in-flight timer callback or signal propagation
    // still observes the actor as live and bails out cooperatively.
    unsafe { prepare_quiescent_actor_for_cleanup(expected) };

    // Wake-proof + finalize decision by the CAS RESULT, BEFORE untracking
    // (mirrors `hew_actor_free_inner` / `cleanup_all_actors`). Under the
    // stop-first contract this is `Err(Stopped) ⇒ Finalize(Stopped)`; a
    // re-enqueued actor (contract drift) takes the fail-closed `Skip` leak.
    // SAFETY: caller guarantees `expected` is valid.
    let a = unsafe { &*expected };
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
        // After take_actor_by_id the map entry is removed: no new send pins
        // can be taken.  Drain any in-flight pins before finalizing.
        // LIVE_ACTORS is not held here; pinned senders can re-acquire it.
        // SAFETY: actor is a live pointer returned by take_actor_by_id.
        let a = unsafe { &*actor };
        loop {
            if a.send_pin_count.load(Ordering::Acquire) == 0 {
                break;
            }
            if std::time::Instant::now() >= deadline {
                // An outstanding pin may still insert a relationship, so no
                // final scrub is safe on this fail-closed allocation leak.
                crate::set_last_error(
                    "drain_quiesced_actor: send pins did not drain after timeout",
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
}

/// Cooperatively stop a set of native actors and wait for quiescence with a shared deadline.
#[cfg(not(target_arch = "wasm32"))]
pub fn drain_actors(ids: &[ActorId], deadline: std::time::Instant) -> DrainOutcome {
    if ids.is_empty() {
        return DrainOutcome::Drained;
    }

    let mut seen = HashSet::with_capacity(ids.len());
    let mut pending: Vec<(ActorId, *mut HewActor)> = ids
        .iter()
        .copied()
        .filter(|id| seen.insert(*id))
        .filter_map(collect_pending_actor)
        .collect();

    for &(_, actor) in &pending {
        // SAFETY: actor came from LIVE_ACTORS and remains owned by the runtime.
        unsafe { hew_actor_stop(actor) };
    }

    let mut crashed = Vec::new();
    let mut backoff = std::time::Duration::from_millis(1);

    loop {
        let mut index = 0;
        while index < pending.len() {
            let (actor_id, expected) = pending[index];
            let state = with_live_actor_by_id(actor_id, expected, |actor| {
                actor.actor_state.load(Ordering::Acquire)
            });
            match state {
                None => {
                    pending.swap_remove(index);
                }
                Some(state) if state == HewActorState::Crashed as i32 => {
                    crashed.push(actor_id);
                    pending.swap_remove(index);
                }
                Some(state) if actor_free_state_is_quiescent(state) => {
                    // SAFETY: `expected` is quiescent and still tracked in LIVE_ACTORS.
                    unsafe { drain_quiesced_actor(actor_id, expected, deadline) };
                    pending.swap_remove(index);
                }
                Some(_) => {
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
        let state = with_live_actor_by_id(actor_id, expected, |actor| {
            actor.actor_state.load(Ordering::Acquire)
        });
        match state {
            None => {}
            Some(state) if state == HewActorState::Crashed as i32 => crashed.push(actor_id),
            Some(state) if actor_free_state_is_quiescent(state) => {
                // SAFETY: `expected` is quiescent and still tracked in LIVE_ACTORS.
                unsafe { drain_quiesced_actor(actor_id, expected, deadline) };
            }
            Some(_) => still_live.push(actor_id),
        }
    }

    drain_outcome_from_lists(still_live, crashed)
}

/// WASM-TODO(#1451): drain_actors primitive pending WASM scheduler integration.
#[cfg(target_arch = "wasm32")]
pub fn drain_actors(ids: &[ActorId], _deadline: std::time::Instant) -> DrainOutcome {
    let mut still_live = ids.to_vec();
    still_live.sort_unstable();
    still_live.dedup();
    if still_live.is_empty() {
        DrainOutcome::Drained
    } else {
        DrainOutcome::Incomplete {
            still_live,
            crashed: Vec::new(),
        }
    }
}

fn actor_ids_to_malloc(ids: &[ActorId]) -> Result<*mut ActorId, &'static str> {
    if ids.is_empty() {
        return Ok(ptr::null_mut());
    }

    let Some(bytes) = ids.len().checked_mul(std::mem::size_of::<ActorId>()) else {
        return Err("hew_actor_drain_set: actor id list size overflow");
    };
    // SAFETY: malloc returns an allocation large enough for `ids.len()` ActorIds or null on failure.
    let out = unsafe { libc::malloc(bytes) }.cast::<ActorId>();
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
            unsafe { libc::free(still_live_ptr.cast()) };
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
        libc::free(out.still_live_ptr.cast());
        libc::free(out.crashed_ptr.cast());
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
#[cfg(all(not(target_arch = "wasm32"), feature = "profiler"))]
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
#[cfg(all(not(target_arch = "wasm32"), not(feature = "profiler")))]
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
/// `name` must be a NUL-terminated `"ActorName::handler_name"` string with
/// static lifetime (a string literal baked into the binary).
///
/// # Safety
///
/// - `dispatch` must be a valid dispatch function for the actor type.
/// - `name` must point to a valid NUL-terminated UTF-8 string with `'static`
///   lifetime.
#[cfg(all(not(target_arch = "wasm32"), feature = "profiler"))]
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
#[cfg(all(not(target_arch = "wasm32"), not(feature = "profiler")))]
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
    terminate_fn: unsafe extern "C" fn(*mut c_void),
) {
    cabi_guard!(actor.is_null());
    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &mut *actor };
    a.terminate_fn = Some(terminate_fn);
}

/// Register a state-drop callback on an actor.
///
/// The state-drop function is called with the actor's live state pointer
/// (`a.state`) immediately before `libc::free(a.state)` in
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
/// `a.state`'s) and is released with a raw `libc::free` of just the wrapper
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
    #[cfg(target_arch = "wasm32")]
    // SAFETY: the actor owns a live WASM mailbox and the callback lifetime is
    // guaranteed by the caller.
    unsafe {
        crate::mailbox_wasm::hew_mailbox_set_message_drop_fn(
            mailbox_ptr.cast(),
            Some(message_drop_fn),
        );
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
#[cfg(not(target_arch = "wasm32"))]
#[inline]
fn actor_runtime_matches(a: &HewActor) -> bool {
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
#[cfg(not(target_arch = "wasm32"))]
#[inline]
fn actor_send_is_terminal(a: &HewActor) -> bool {
    let state = a.actor_state.load(Ordering::Acquire);
    state == HewActorState::Crashed as i32 || state == HewActorState::Stopped as i32
}

#[cfg(not(target_arch = "wasm32"))]
unsafe fn actor_send_result_internal(
    actor: *mut HewActor,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
) -> i32 {
    // SAFETY: Caller guarantees `actor` is valid.
    unsafe { actor_send_result_internal_reply(actor, msg_type, data, size, ptr::null_mut()) }
}

/// Like [`actor_send_result_internal`] but with an explicit reply channel
/// that is set on the message node (for the ask pattern).
#[cfg(not(target_arch = "wasm32"))]
unsafe fn actor_send_result_internal_reply(
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

    // Terminal-state send gate (see `actor_send_is_terminal`): reject once the
    // actor is terminal, even if its mailbox is not yet closed — closes the
    // trap's terminal-CAS-before-mailbox-close window so no copy-mode send
    // enqueues into, or reports false success against, a terminal actor.
    if actor_send_is_terminal(a) {
        return HewError::ErrActorStopped as i32;
    }

    // Check for injected drop fault (testing only). Silently discard
    // the message without enqueuing it.
    if crate::deterministic::check_drop_fault(a.id) {
        return HewError::Ok as i32; // Pretend success.
    }

    let mb = a.mailbox.cast::<HewMailbox>();

    if reply_channel.is_null() {
        // Fire-and-forget (no reply channel expected): resolve against the
        // raw `SendOutcome` rather than a collapsed status code. A
        // `DropNew` policy-drop is spec-silent per §6.2 — it must report
        // success — but unlike an actual enqueue, it queued nothing, so it
        // must NOT wake/schedule the actor (there is no new message for it
        // to find). See `hew_mailbox_send_fire_and_forget` for the seam
        // rationale.
        // SAFETY: Mailbox is valid for the actor's lifetime; data/size from caller.
        return match unsafe { mailbox::hew_mailbox_send_fire_and_forget(mb, msg_type, data, size) }
        {
            mailbox::SendOutcome::Enqueued
            | mailbox::SendOutcome::Coalesced
            | mailbox::SendOutcome::DroppedOld => {
                // SAFETY: `actor`/`a` valid; a node actually reached the
                // queue, so the actor may be scheduled to run.
                unsafe { schedule_actor_after_enqueue(actor, a, msg_type) };
                HewError::Ok as i32
            }
            // Policy-drop: silent per spec §6.2, nothing was queued, so
            // there is nothing to wake the actor for.
            mailbox::SendOutcome::Dropped => HewError::Ok as i32,
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
    let result =
        unsafe { mailbox::hew_mailbox_send_with_reply(mb, msg_type, data, size, reply_channel) };
    if result != 0 {
        return result;
    }

    // SAFETY: `actor`/`a` valid; the message is enqueued so the actor
    // may be scheduled to run.
    unsafe { schedule_actor_after_enqueue(actor, a, msg_type) };

    HewError::Ok as i32
}

/// Record the send in the trace log and, if the destination actor is
/// idle, transition it `Idle → Runnable` and enqueue it on the
/// scheduler. Shared by the copy-mode delivery path
/// ([`actor_send_result_internal_reply`]) and the envelope-mode alias
/// path ([`hew_actor_send_aliased`]) so both wake the actor identically.
///
/// # Safety
///
/// `actor` must be a valid pointer and `a` must borrow the same actor.
#[cfg(not(target_arch = "wasm32"))]
unsafe fn schedule_actor_after_enqueue(actor: *mut HewActor, a: &HewActor, msg_type: i32) {
    let sender = hew_actor_self();
    let trace_actor_id = if sender.is_null() {
        a.id
    } else {
        // SAFETY: the scheduler installs a live actor during dispatch.
        unsafe { (*sender).id }
    };
    crate::tracing::record_send(trace_actor_id, msg_type);

    // CAS IDLE → RUNNABLE; on success, schedule the actor.
    if a.actor_state
        .compare_exchange(
            HewActorState::Idle as i32,
            HewActorState::Runnable as i32,
            Ordering::AcqRel,
            Ordering::Acquire,
        )
        .is_ok()
    {
        // Clear hibernation state — the actor has work to do.
        a.idle_count.store(0, Ordering::Relaxed);
        a.hibernating.store(0, Ordering::Relaxed);
        scheduler::sched_enqueue(actor);
    }
}

/// Send a message, returning `true` on success.
///
/// # Safety
///
/// Same requirements as [`hew_actor_send`].
#[cfg(not(target_arch = "wasm32"))]
unsafe fn actor_send_internal(
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
        return HewError::ErrOom as i32;
    }

    // Retain a sender-side reference before enqueueing so mailbox teardown and
    // successful replies consume the queued ref while the caller keeps its own.
    // DROP-SAFETY: send failure must release both references for owned ask
    // channels and only the queued retain for caller-provided channels.
    unsafe { reply_channel::hew_reply_channel_retain(ch) };

    let send_result = send(ch);
    if send_result != HewError::Ok as i32 {
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

// ── Ask (request-response) ──────────────────────────────────────────────
// Native asks block on threaded reply channels; WASM asks cooperate by
// driving the single-threaded scheduler in bounded ticks.

/// Send a synchronous request and block until a reply arrives.
///
/// The reply channel pointer is **packed at the end** of the message
/// data, matching the C runtime convention:
/// `[original_data | reply_channel_ptr]`
///
/// Returns the reply value (caller must free with [`libc::free`]), or
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

#[cfg(any(target_arch = "wasm32", test))]
const HEW_WASM_ASK_TICK_ACTIVATIONS: i32 = 1;

#[cfg(any(target_arch = "wasm32", test))]
#[inline]
fn is_terminal(state: i32) -> bool {
    state == HewActorState::Stopped as i32 || state == HewActorState::Crashed as i32
}

#[cfg(any(target_arch = "wasm32", test))]
pub(crate) unsafe fn wake_wasm_actor(actor: *mut HewActor) {
    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };
    if a.actor_state.load(Ordering::Relaxed) == HewActorState::Idle as i32 {
        a.actor_state
            .store(HewActorState::Runnable as i32, Ordering::Relaxed);
        a.idle_count.store(0, Ordering::Relaxed);
        a.hibernating.store(0, Ordering::Relaxed);
        // SAFETY: actor is valid and the cooperative scheduler is initialized.
        unsafe { crate::scheduler_wasm::hew_wasm_sched_enqueue(actor.cast()) };
    }
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
    let ch = reply_channel::hew_reply_channel_new();

    // SAFETY: `ch` is a live reply channel owned by this ask call and the
    // closure preserves the same actor-ID/data preconditions.
    let send_result_code = unsafe {
        submit_ask_with_reply_channel(ch, AskReplyChannelFailureCleanup::FreeCreatorRef, |ch| {
            // Use the liveness-pin protocol (same as hew_actor_send_by_id):
            // under LIVE_ACTORS, validate + pin; release lock; run the send;
            // SendPinGuard decrements on return.  The untrack-first free path
            // cannot finalize while this pin is held.
            live_actors::with_actor_send_by_id(actor_id, |actor| {
                // SAFETY: `actor` is pinned live by `with_actor_send_by_id`;
                // allocation valid for the closure.  `ch` is a live reply
                // channel retained above.  Same data/size preconditions as
                // hew_actor_ask.  Outer `unsafe` block covers this call.
                actor_send_result_internal_reply(actor, msg_type, data, size, ch.cast())
            })
            .unwrap_or(HewError::ErrActorStopped as i32)
        })
    };

    if send_result_code != HewError::Ok as i32 {
        return actor_ask_null(send_err_to_ask_err(send_result_code));
    }

    let mut reply_size: usize = 0;
    // SAFETY: ch is valid and single-reader; reply_size is a valid stack pointer.
    let result = unsafe { reply_channel::hew_reply_wait_with_size(ch, &raw mut reply_size) };

    // Store the reply size in a thread-local so the caller can retrieve it.
    LAST_REPLY_SIZE.set(reply_size);

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
    #[cfg(target_arch = "wasm32")]
    // SAFETY: the helper pins only while submitting, then retains ActorId only.
    unsafe {
        actor_ask_wasm_by_id_impl(actor_id, msg_type, data, size, None)
    }
}

/// Submit an ask with a caller-owned reply channel through a stable identity.
///
/// # Safety
/// `data` and `ch` must satisfy [`hew_actor_ask_with_channel`]'s contract.
#[no_mangle]
pub unsafe extern "C" fn hew_local_pid_ask_with_channel(
    token: crate::lifetime::local_handles::HewLocalPidId,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
    ch: *mut c_void,
) -> i32 {
    let Some(actor_id) = crate::lifetime::local_handles::resolve_current_actor(token) else {
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
        #[cfg(target_arch = "wasm32")]
        // SAFETY: actor is pinned; channel and data follow this function's contract.
        unsafe {
            ask_with_channel_wasm_internal(actor, msg_type, data, size, ch)
        }
    })
    .unwrap_or(HewError::ErrActorStopped as i32)
}

// Thread-local storage for the reply size from the last `hew_actor_ask_by_id`.
std::thread_local! {
    static LAST_REPLY_SIZE: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

/// Retrieve the size of the reply data from the most recent
/// `hew_actor_ask_by_id` call on the current thread.
// live on not(wasm32) — hew_node.rs ask path; dead on wasm32; caller hew_node.rs:1009
#[cfg_attr(target_arch = "wasm32", allow(dead_code))]
pub(crate) unsafe fn hew_reply_data_size(_ptr: *mut c_void) -> usize {
    LAST_REPLY_SIZE.get()
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
/// 7). Called from BOTH terminal finalize paths — [`hew_actor_trap`] (the
/// crash path) and `hew_actor_free_inner`'s parked-activation reclaim (the
/// stop/teardown-with-a-live-parked-pump path) — so a consumer awaiting the
/// stream observes the fault on every terminal cause
/// (`death-signal-fires-on-every-terminal-cause`), never a silent hang.
///
/// Idempotent: swaps the slot to null before touching the sink, so a second
/// call (or a race between the two callers) sees an already-null slot and is
/// a no-op — the sink is fault-closed exactly once.
#[cfg(not(target_arch = "wasm32"))]
fn fault_close_registered_gen_sink(a: &HewActor) {
    let raw = a.gen_sink.swap(ptr::null_mut(), Ordering::AcqRel);
    if raw.is_null() {
        return;
    }
    // SAFETY: `raw` was registered by `hew_actor_gen_sink_register` as a live
    // `HewSink` pointer and has not been consumed — the swap-to-null above is
    // the single point that can observe it non-null, so no other caller can
    // race this release.
    unsafe { crate::stream::fault_close_registered_sink(raw.cast(), a.id) };
}

// ── Trap / Error ────────────────────────────────────────────────────────

/// Trap (panic) an actor: store an error code, close the mailbox, and
/// transition to a terminal state. If the actor has a supervisor, notify it.
///
/// # Safety
///
/// `actor` must be a valid pointer returned by a spawn function.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_trap(actor: *mut HewActor, error_code: i32) {
    cabi_guard!(actor.is_null());
    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };

    // Choose terminal state: Crashed if error_code != 0, Stopped otherwise.
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
    // and the child-crashed notification is never delivered — the supervisor's
    // `hew_supervisor_wait_restart` blocks to its full timeout ceiling.
    //
    // Taking the terminal CAS first makes the trap authoritative: once the
    // actor is CRASHED/STOPPED, the worker's `Running -> Idle` / `Idle ->
    // Stopped` settle CAS fails, so it cannot self-stop the actor out from
    // under us, and the notify below always runs. The mailbox is closed
    // immediately after to reject new sends and wake blocked senders — by then
    // the actor is already terminal, so every sender's `Idle -> Runnable` CAS
    // fails regardless.
    loop {
        let current = a.actor_state.load(Ordering::Acquire);
        if current == HewActorState::Stopped as i32 || current == HewActorState::Crashed as i32 {
            return;
        }
        if a.actor_state
            .compare_exchange(current, terminal, Ordering::AcqRel, Ordering::Acquire)
            .is_ok()
        {
            break;
        }
    }

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

    // Store error code only after winning the CAS race.
    a.error_code.store(error_code, Ordering::Release);
    if terminal == HewActorState::Crashed as i32 {
        let scope = crate::task_scope::current_task_scope();
        if !scope.is_null() {
            // SAFETY: the task-scope lane is installed only while the scope is live.
            unsafe { crate::task_scope::hew_task_scope_cancel(scope) };
        }
    }
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
    crate::link::record_link_probe_terminal(actor_id, terminal);

    // Propagate exit to linked actors and notify monitors.
    // Do this BEFORE notifying supervisor to ensure proper ordering.
    run_crash_teardown_order_hook(HEW_ACTOR_CRASH_TEARDOWN_BEFORE_EXIT_PROPAGATION);
    crate::link::propagate_exit_to_links(actor_id, error_code);
    run_crash_teardown_order_hook(HEW_ACTOR_CRASH_TEARDOWN_AFTER_EXIT_PROPAGATION);
    let crash_kind = if terminal == HewActorState::Crashed as i32 {
        crate::internal::types::CrashKind::tag_from_error_code(error_code).cast_unsigned()
    } else {
        0
    };
    crate::monitor::notify_monitors_on_death(actor_id, terminal, crash_kind);

    // Wake any actor group condvars waiting on this actor.
    crate::actor_group::notify_actor_death(actor_id);

    // Notify supervisor if one exists.
    if !supervisor.is_null() {
        // SAFETY: supervisor back-pointer was set by hew_supervisor_add_child.
        unsafe {
            crate::supervisor::hew_supervisor_notify_child_event(
                supervisor.cast(),
                supervisor_child_index,
                actor_id,
                terminal,
                error_code,
            );
        }
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
/// routes a `SYS_MSG_EXIT` (103) with no `#[on(exit)]` hook here instead of the
/// exhaustiveness `llvm.trap` default (which is UB — it SIGILLs on Linux and
/// only accidentally produced a terminal state on macOS). This drives the SAME
/// controlled crash path a handler panic uses, with the carried reason stamped
/// on the actor, and is target-symmetric: both backends export a
/// `hew_trap_with_code(i32)` symbol with identical semantics but from different
/// modules, because the crash seam itself differs per target.
///
/// * Native (`crate::supervisor::hew_trap_with_code`): longjmps to the
///   scheduler's recovery frame — terminal `Crashed`, the carried reason
///   stamped, link / monitor / supervisor fan-out.
/// * wasm32 (`crate::trap_code::hew_trap_with_code`): stamps the carried reason
///   on the actor and panics; under `panic = "abort"` (the wasm32-wasip1 runtime
///   profile) the panic aborts the module — the fail-closed crash. On a host
///   build with unwinding the cooperative scheduler's `catch_unwind` activation
///   boundary observes the stamped code and transitions the actor to `Crashed`,
///   the WASM counterpart of the native longjmp seam. wasm32 has no `supervisor`
///   module (it is `#[cfg(not(target_arch = "wasm32"))]`), so the call must
///   target `trap_code` there or the wasm runtime archive does not compile.
///
/// `reason` is the EXIT's carried terminal reason; a zero (clean) reason is
/// coerced to the non-zero `Crashed` sentinel so an unhandled EXIT ALWAYS
/// crashes the non-trapping linked actor (a cleanly-exited linked peer still
/// takes it down, OTP-style). `hew_trap_with_code` does not return when called
/// inside dispatch; outside an actor context there is no recovery seam, where
/// the trampoline's `llvm.trap` is unreachable because a `SYS_MSG_EXIT` only
/// arrives at a scheduler-driven dispatch.
#[no_mangle]
pub extern "C" fn hew_actor_exit_unhandled(reason: i32) {
    // Coerce a zero (clean) reason to a non-zero crash sentinel: an unhandled
    // EXIT always crashes the non-trapping linked actor (Crashed, not Stopped).
    let crash_code = if reason == 0 {
        HewActorState::Crashed as i32
    } else {
        reason
    };
    // SAFETY: routes through the per-target crash seam. Native's
    // `try_direct_longjmp_with_code` checks the per-thread recovery context and
    // is a no-op when none is active; wasm32's stamps the actor error code and
    // panics. Both are safe to call from generated dispatch code.
    #[cfg(not(target_arch = "wasm32"))]
    unsafe {
        crate::supervisor::hew_trap_with_code(crash_code);
    }
    #[cfg(target_arch = "wasm32")]
    unsafe {
        crate::trap_code::hew_trap_with_code(crash_code);
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

/// Stamp the WASM actor panic sentinel on the current actor, when present.
#[cfg(any(target_arch = "wasm32", test))]
pub(crate) fn stamp_wasm_actor_panic() -> bool {
    crate::trap_code::stamp_current_actor_error_code(101)
}

/// Trigger a panic in the current execution context.
///
/// Inside an actor: longjmps back to the scheduler on native, or stamps the
/// panic sentinel and unwinds to the scheduler on WASM. The supervisor (if any)
/// will restart the actor according to its restart strategy.
///
/// Outside an actor (e.g. `main`): exits the process with code 101.
///
/// This function never returns.
#[no_mangle]
pub extern "C" fn hew_panic() {
    #[cfg(target_arch = "wasm32")]
    {
        if stamp_wasm_actor_panic() {
            panic!("hew_panic: actor panic");
        }
        // JUSTIFIED: wasm32 non-actor Hew panic terminates the process
        // immediately with Rust's panic exit convention, so bypassing Rust Drop
        // is deliberate and the WASI host reclaims process resources. Actor
        // paths stamp `actor.error_code` and unwind above; they do not use this
        // process-exit route.
        std::process::exit(101);
    }

    // Try direct longjmp recovery first. This avoids going through the
    // signal/exception path, which is essential on Windows where longjmp
    // from a VEH handler causes STATUS_BAD_STACK.
    //
    // SAFETY: Called from actor dispatch context (stack chain includes the
    // scheduler's sigsetjmp frame). If recovery context exists, longjmps
    // directly — never returns. If no context, returns and we fall through
    // to a clean process exit.
    #[cfg(not(target_arch = "wasm32"))]
    unsafe {
        crate::signal::try_direct_longjmp();
    }

    // No recovery context (e.g. panic called from main) — exit cleanly.
    // Exit code 101 follows Rust's convention for panics.
    #[cfg(not(target_arch = "wasm32"))]
    std::process::exit(101);
}

/// Crash the current actor after printing a message.
///
/// # Safety
///
/// `msg` must be a valid null-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_panic_msg(msg: *const std::ffi::c_char) {
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

/// Self-stop: the currently running actor requests its own shutdown.
///
/// Closes the mailbox and CAS transitions from `Running` to `Stopping`.
/// The WASM scheduler will handle the final transition to `Stopped` after
/// dispatch returns.
#[cfg(any(target_arch = "wasm32", test))]
pub(crate) unsafe fn actor_self_stop_wasm_impl(actor: *mut HewActor) {
    if actor.is_null() {
        return;
    }
    // SAFETY: caller guarantees `actor` is the currently running actor.
    let a = unsafe { &*actor };

    let mailbox = a.mailbox.cast::<crate::mailbox_wasm::HewMailboxWasm>();
    if !mailbox.is_null() {
        // SAFETY: mailbox is valid for the actor's lifetime.
        unsafe { crate::mailbox_wasm::hew_mailbox_close(mailbox) };
    }

    // CAS Running → Stopping.
    let _ = a.actor_state.compare_exchange(
        HewActorState::Running as i32,
        HewActorState::Stopping as i32,
        Ordering::AcqRel,
        Ordering::Acquire,
    );
}

#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub extern "C" fn hew_actor_self_stop() {
    let actor = hew_actor_self();
    // SAFETY: the canonical context actor lane is only set during dispatch.
    unsafe { actor_self_stop_wasm_impl(actor) };
}

// ── WASM actor API ──────────────────────────────────────────────────────
// On WASM, spawn/send/ask/stop/close use the WASM mailbox and cooperative
// scheduler. These provide the same C ABI surface as native so that
// codegen-emitted calls resolve transparently.

#[cfg(target_arch = "wasm32")]
extern "C" {
    fn hew_mailbox_new() -> *mut c_void;
    fn hew_mailbox_new_bounded(capacity: i32) -> *mut c_void;
    fn hew_mailbox_new_with_policy(capacity: usize, policy: HewOverflowPolicy) -> *mut c_void;
    fn hew_mailbox_send(mb: *mut c_void, msg_type: i32, data: *mut c_void, size: usize) -> i32;
    fn hew_mailbox_close(mb: *mut c_void);
    fn hew_wasm_sched_enqueue(actor: *mut c_void);
}

/// Spawn a new actor with an unbounded mailbox (WASM).
///
/// # Safety
///
/// Same requirements as the native [`hew_actor_spawn`].
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_spawn(
    state: *mut c_void,
    state_size: usize,
    dispatch: Option<HewDispatchFn>,
) -> *mut HewActor {
    // SAFETY: Caller guarantees `state` validity.
    let actor_state = unsafe { deep_copy_state(state, state_size) };
    if !state.is_null() && state_size > 0 && actor_state.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: hew_mailbox_new is a trusted FFI constructor returning a valid mailbox pointer.
    let mailbox = unsafe { hew_mailbox_new() };

    // SAFETY: actor_state is a fresh deep-copy; mailbox is valid.
    unsafe {
        spawn_actor_internal(ActorSpawnConfig {
            state: actor_state,
            state_size,
            dispatch,
            mailbox,
            budget: HEW_MSG_BUDGET,
            coalesce_key_fn: None,
            cycle_capable: false,
            cap_bytes: 0,
            adopt: false,
        })
    }
}

/// Spawn a new actor with a bounded mailbox (WASM).
///
/// # Safety
///
/// Same requirements as the native [`hew_actor_spawn_bounded`].
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_spawn_bounded(
    state: *mut c_void,
    state_size: usize,
    dispatch: Option<HewDispatchFn>,
    capacity: i32,
) -> *mut HewActor {
    // SAFETY: Caller guarantees `state` validity.
    let actor_state = unsafe { deep_copy_state(state, state_size) };
    if !state.is_null() && state_size > 0 && actor_state.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: hew_mailbox_new_bounded is a trusted FFI constructor returning a valid mailbox pointer.
    let mailbox = unsafe { hew_mailbox_new_bounded(capacity) };

    // SAFETY: actor_state is a fresh deep-copy; mailbox is valid.
    unsafe {
        spawn_actor_internal(ActorSpawnConfig {
            state: actor_state,
            state_size,
            dispatch,
            mailbox,
            budget: HEW_MSG_BUDGET,
            coalesce_key_fn: None,
            cycle_capable: false,
            cap_bytes: 0,
            adopt: false,
        })
    }
}

/// Spawn a new actor from options (WASM).
///
/// # Safety
///
/// Same requirements as the native [`hew_actor_spawn_opts`].
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_spawn_opts(opts: *const HewActorOpts) -> *mut HewActor {
    if opts.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: Caller guarantees `opts` points to a valid HewActorOpts.
    let opts = unsafe { &*opts };

    // SAFETY: Caller guarantees opts.init_state is readable for opts.state_size bytes.
    let actor_state = unsafe { deep_copy_state(opts.init_state, opts.state_size) };
    if !opts.init_state.is_null() && opts.state_size > 0 && actor_state.is_null() {
        return ptr::null_mut();
    }

    let mailbox = if opts.mailbox_capacity > 0 {
        let capacity = usize::try_from(opts.mailbox_capacity).unwrap_or(usize::MAX);
        let policy = parse_overflow_policy(opts.overflow);
        // SAFETY: Trusted FFI constructor; capacity/policy were derived from opts above.
        unsafe { hew_mailbox_new_with_policy(capacity, policy) }
    } else {
        // SAFETY: Trusted FFI constructor for an unbounded mailbox.
        unsafe { hew_mailbox_new() }
    };
    let coalesce_fallback = parse_overflow_policy(opts.coalesce_fallback);
    // SAFETY: mailbox is a valid WASM mailbox pointer created above.
    unsafe {
        crate::mailbox_wasm::hew_mailbox_set_coalesce_config(
            mailbox.cast::<crate::mailbox_wasm::HewMailboxWasm>(),
            opts.coalesce_key_fn,
            coalesce_fallback,
        );
        crate::mailbox_wasm::hew_mailbox_set_message_drop_fn(
            mailbox.cast::<crate::mailbox_wasm::HewMailboxWasm>(),
            opts.message_drop_fn,
        );
    }

    let budget = if opts.budget > 0 {
        opts.budget
    } else {
        HEW_MSG_BUDGET
    };

    // SAFETY: actor_state is a fresh deep-copy; mailbox is valid.
    unsafe {
        spawn_actor_internal(ActorSpawnConfig {
            state: actor_state,
            state_size: opts.state_size,
            dispatch: opts.dispatch,
            mailbox,
            budget,
            coalesce_key_fn: opts.coalesce_key_fn,
            cycle_capable: opts.cycle_capable != 0,
            cap_bytes: opts.arena_cap_bytes,
            adopt: false,
        })
    }
}

/// Send a message to an actor (WASM, fire-and-forget).
///
/// # Safety
///
/// Same requirements as the native [`hew_actor_send`].
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_send(
    actor: *mut HewActor,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
) {
    cabi_guard!(actor.is_null());
    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };
    // SAFETY: Mailbox is valid for the actor's lifetime.
    unsafe { hew_mailbox_send(a.mailbox, msg_type, data, size) };

    // Transition IDLE → RUNNABLE and enqueue.
    if a.actor_state.load(Ordering::Relaxed) == HewActorState::Idle as i32 {
        a.actor_state
            .store(HewActorState::Runnable as i32, Ordering::Relaxed);
        a.idle_count.store(0, Ordering::Relaxed);
        a.hibernating.store(0, Ordering::Relaxed);
        // SAFETY: actor is valid.
        unsafe { hew_wasm_sched_enqueue(actor.cast()) };
    }
}

/// Try to send a message (WASM). Identical to [`hew_actor_send`] on WASM
/// since there is no blocking distinction.
///
/// # Safety
///
/// Same requirements as [`hew_actor_send`].
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_try_send(
    actor: *mut HewActor,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
) -> i32 {
    cabi_guard!(actor.is_null(), HewError::ErrActorStopped as i32);
    // SAFETY: Caller guarantees `actor` is a valid pointer.
    let a = unsafe { &*actor };
    // SAFETY: a.mailbox is a valid mailbox pointer for the actor's lifetime.
    let result = unsafe { hew_mailbox_send(a.mailbox, msg_type, data, size) };
    if result != 0 {
        return result;
    }

    if a.actor_state.load(Ordering::Relaxed) == HewActorState::Idle as i32 {
        a.actor_state
            .store(HewActorState::Runnable as i32, Ordering::Relaxed);
        a.idle_count.store(0, Ordering::Relaxed);
        a.hibernating.store(0, Ordering::Relaxed);
        // SAFETY: actor is valid.
        unsafe { hew_wasm_sched_enqueue(actor.cast()) };
    }

    0
}

/// Shared WASM send-with-channel primitive for ask/select lowering.
///
/// # Safety
///
/// Same requirements as the native [`hew_actor_ask_with_channel`].
#[cfg(any(target_arch = "wasm32", test))]
pub(crate) unsafe fn ask_with_channel_wasm_internal(
    actor: *mut HewActor,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
    ch: *mut c_void,
) -> i32 {
    cabi_guard!(actor.is_null(), HewError::ErrActorStopped as i32);
    // SAFETY: the actor now holds the sender-side reference until it replies.
    unsafe { crate::reply_channel_wasm::hew_reply_channel_retain(ch.cast()) };

    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };
    // SAFETY: a.mailbox is a valid mailbox pointer; ch is a valid reply channel.
    let mut send_result = unsafe {
        crate::mailbox_wasm::hew_mailbox_send_with_reply(a.mailbox.cast(), msg_type, data, size, ch)
    };
    if send_result == HewError::ErrClosed as i32 {
        send_result = HewError::ErrActorStopped as i32;
    }
    if send_result != HewError::Ok as i32 {
        // SAFETY: release the sender-side reference retained for the failed send.
        unsafe { crate::reply_channel_wasm::hew_reply_channel_free(ch.cast()) };
        return send_result;
    }

    // SAFETY: actor is valid and owned by the runtime.
    unsafe { wake_wasm_actor(actor) };

    HewError::Ok as i32
}

#[cfg(any(target_arch = "wasm32", test))]
#[derive(Clone, Copy)]
enum WasmAskTarget {
    Pointer(*mut HewActor),
    #[cfg_attr(
        not(target_arch = "wasm32"),
        allow(dead_code, reason = "ActorId ask targets are WASM-only")
    )]
    ActorId(u64),
}

#[cfg(any(target_arch = "wasm32", test))]
impl WasmAskTarget {
    unsafe fn send(self, msg_type: i32, data: *mut c_void, size: usize, ch: *mut c_void) -> i32 {
        match self {
            // SAFETY: inherited from the raw-pointer ask caller.
            Self::Pointer(actor) => unsafe {
                ask_with_channel_wasm_internal(actor, msg_type, data, size, ch)
            },
            Self::ActorId(actor_id) => live_actors::with_actor_send_by_id(actor_id, |actor| {
                // SAFETY: liveness lookup pins actor for the complete send.
                unsafe { ask_with_channel_wasm_internal(actor, msg_type, data, size, ch) }
            })
            .unwrap_or(HewError::ErrActorStopped as i32),
        }
    }

    fn terminal_state(self) -> Option<i32> {
        match self {
            Self::Pointer(actor) => {
                if actor.is_null() {
                    return None;
                }
                // SAFETY: inherited from the raw-pointer ask caller.
                Some(unsafe { (*actor).actor_state.load(Ordering::Acquire) })
            }
            Self::ActorId(actor_id) => live_actors::pin_actor_by_id(actor_id)
                .map(|pin| pin.actor().actor_state.load(Ordering::Acquire)),
        }
    }
}

#[cfg(any(target_arch = "wasm32", test))]
unsafe fn actor_ask_wasm_target_impl(
    target: WasmAskTarget,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
    timeout_ms: Option<i32>,
) -> *mut c_void {
    use crate::reply_channel_wasm;

    let ch = reply_channel_wasm::hew_reply_channel_new();

    // SAFETY: ch is live; target and data inherit this helper's contract.
    let send_result = unsafe { target.send(msg_type, data, size, ch.cast()) };
    if send_result != HewError::Ok as i32 {
        // SAFETY: ch was created above; failed send released its sender retain.
        unsafe { reply_channel_wasm::hew_reply_channel_free(ch) };
        return actor_ask_null(send_err_to_ask_err(send_result));
    }

    let deadline = timeout_ms.map(|ms| {
        std::time::Instant::now()
            + std::time::Duration::from_millis(u64::try_from(ms.max(0)).unwrap_or(0))
    });

    loop {
        // SAFETY: ch stays live until the caller-side release below.
        if unsafe { reply_channel_wasm::reply_ready(ch) } {
            break;
        }

        if deadline.is_some_and(|limit| std::time::Instant::now() >= limit) {
            // SAFETY: ch remains live through cancellation and release.
            unsafe {
                reply_channel_wasm::hew_reply_channel_cancel(ch);
                reply_channel_wasm::hew_reply_channel_free(ch);
            }
            return actor_ask_null(AskError::Timeout);
        }

        // SAFETY: scheduler must be initialized by the runtime/host.
        let remaining = unsafe { crate::bridge::hew_wasm_tick(HEW_WASM_ASK_TICK_ACTIVATIONS) };

        if deadline.is_some_and(|limit| std::time::Instant::now() >= limit) {
            // SAFETY: ch remains live through cancellation and release.
            unsafe {
                reply_channel_wasm::hew_reply_channel_cancel(ch);
                reply_channel_wasm::hew_reply_channel_free(ch);
            }
            return actor_ask_null(AskError::Timeout);
        }

        // SAFETY: ch stays live until the caller-side release below.
        if unsafe { reply_channel_wasm::reply_ready(ch) } {
            break;
        }

        if remaining == 0 && crate::scheduler_wasm::hew_wasm_sleeping_count() == 0 {
            // SAFETY: ch remains live through cancellation and release.
            unsafe {
                reply_channel_wasm::hew_reply_channel_cancel(ch);
                reply_channel_wasm::hew_reply_channel_free(ch);
            }
            if target.terminal_state().is_none_or(is_terminal) {
                return actor_ask_null(AskError::OrphanedAsk);
            }
            return actor_ask_null(AskError::NoRunnableWork);
        }
    }

    // SAFETY: ch is a valid reply channel pointer created above.
    let reply = unsafe { reply_channel_wasm::reply_take(ch) };
    if reply.is_null() {
        // SAFETY: ch remains live until the release immediately below.
        let is_orphaned = unsafe { reply_channel_wasm::reply_is_orphaned(ch) };
        // SAFETY: release the caller-side channel reference.
        unsafe { reply_channel_wasm::hew_reply_channel_free(ch) };
        if is_orphaned {
            return actor_ask_null(AskError::OrphanedAsk);
        }
        actor_ask_clear();
    } else {
        // SAFETY: release the caller-side channel reference.
        unsafe { reply_channel_wasm::hew_reply_channel_free(ch) };
        actor_ask_clear();
    }
    reply
}

#[cfg(any(target_arch = "wasm32", test))]
pub(crate) unsafe fn actor_ask_wasm_impl(
    actor: *mut HewActor,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
    timeout_ms: Option<i32>,
) -> *mut c_void {
    // SAFETY: preserves the raw-pointer ask contract.
    unsafe {
        actor_ask_wasm_target_impl(
            WasmAskTarget::Pointer(actor),
            msg_type,
            data,
            size,
            timeout_ms,
        )
    }
}

#[cfg(target_arch = "wasm32")]
unsafe fn actor_ask_wasm_by_id_impl(
    actor_id: u64,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
    timeout_ms: Option<i32>,
) -> *mut c_void {
    // SAFETY: ActorId resolution pins only during send/state probes.
    unsafe {
        actor_ask_wasm_target_impl(
            WasmAskTarget::ActorId(actor_id),
            msg_type,
            data,
            size,
            timeout_ms,
        )
    }
}

/// Send a message with a caller-provided reply channel (WASM).
///
/// Mirrors the native send-with-channel contract for `select.add`: retain
/// the caller-provided reply channel for the queued send, wake an idle actor,
/// and return a status code without waiting for a reply.
///
/// # Safety
///
/// Same requirements as the native [`hew_actor_ask_with_channel`].
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_ask_with_channel(
    actor: *mut HewActor,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
    ch: *mut c_void,
) -> i32 {
    // SAFETY: same preconditions as ask_with_channel_wasm_internal.
    unsafe { ask_with_channel_wasm_internal(actor, msg_type, data, size, ch) }
}

/// Cooperative ask: send a request and run the scheduler until a reply
/// arrives (WASM).
///
/// # Safety
///
/// Same requirements as the native [`hew_actor_ask`].
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_ask(
    actor: *mut HewActor,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
) -> *mut c_void {
    // SAFETY: same preconditions as actor_ask_wasm_impl.
    unsafe { actor_ask_wasm_impl(actor, msg_type, data, size, None) }
}

/// Cooperative ask with timeout: send a request and drive the scheduler in
/// bounded ticks until the reply arrives or the timeout expires (WASM).
///
/// Returns the reply value, or null on timeout / when no runnable work
/// remains that can satisfy the ask before control returns to the host.
///
/// # Safety
///
/// Same requirements as the native [`hew_actor_ask_timeout`].
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_ask_timeout(
    actor: *mut HewActor,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
    timeout_ms: i32,
) -> *mut c_void {
    // SAFETY: same preconditions as actor_ask_wasm_impl.
    unsafe { actor_ask_wasm_impl(actor, msg_type, data, size, Some(timeout_ms)) }
}

/// Cooperative await: pump the scheduler until the actor reaches a terminal
/// state (WASM).
///
/// Returns the actor error code (0 for clean stop, non-zero for crash).
/// Returns `-1` for null actor pointers.
///
/// # Safety
///
/// `actor` must be a valid pointer returned by a spawn function.
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_await(actor: *mut HewActor) -> i32 {
    if actor.is_null() {
        return -1;
    }

    // SAFETY: caller guarantees `actor` is valid.
    let a = unsafe { &*actor };
    if is_terminal(a.actor_state.load(Ordering::Acquire)) {
        return a.error_code.load(Ordering::Acquire);
    }

    loop {
        // SAFETY: scheduler must be initialized by the runtime/host.
        let remaining = unsafe { crate::bridge::hew_wasm_tick(HEW_WASM_ASK_TICK_ACTIVATIONS) };
        if is_terminal(a.actor_state.load(Ordering::Acquire)) {
            return a.error_code.load(Ordering::Acquire);
        }
        if remaining == 0 && crate::scheduler_wasm::hew_wasm_sleeping_count() == 0 {
            return HewError::ErrTimeout as i32;
        }
    }
}

/// Cooperative await-all: wait for all provided actors to reach terminal
/// states by pumping the WASM scheduler.
///
/// Returns `0` if every actor stopped normally, or the first non-zero
/// error code encountered. Returns `-1` on null/invalid arguments.
///
/// # Safety
///
/// - `actors` must point to an array of at least `count` valid
///   `*mut HewActor` pointers (null entries are skipped).
/// - `count` must be non-negative.
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_await_all(actors: *const *mut HewActor, count: i64) -> i32 {
    if actors.is_null() || count < 0 {
        return -1;
    }

    let mut first_error = 0;
    #[expect(
        clippy::cast_sign_loss,
        clippy::cast_possible_truncation,
        reason = "count >= 0 checked above; practical array sizes fit in usize"
    )]
    for i in 0..count as usize {
        // SAFETY: caller guarantees the array is valid for `count` elements.
        let actor = unsafe { *actors.add(i) };
        if actor.is_null() {
            continue;
        }
        // SAFETY: actor pointer validity follows the caller contract.
        let rc = unsafe { hew_actor_await(actor) };
        if first_error == 0 && rc != 0 {
            first_error = rc;
        }
    }
    first_error
}

/// Close an actor, rejecting new messages (WASM).
///
/// # Safety
///
/// `actor` must be a valid pointer returned by a spawn function.
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_close(actor: *mut HewActor) {
    cabi_guard!(actor.is_null());
    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };

    // Close the mailbox.
    if !a.mailbox.is_null() {
        // SAFETY: a.mailbox is a valid mailbox pointer.
        unsafe { hew_mailbox_close(a.mailbox) };
    }

    // If IDLE, transition directly to STOPPED.
    if a.actor_state
        .compare_exchange(
            HewActorState::Idle as i32,
            HewActorState::Stopped as i32,
            Ordering::AcqRel,
            Ordering::Acquire,
        )
        .is_ok()
    {
        // WASM-R37-S2: direct close of an idle actor mirrors native
        // `hew_actor_close` observability before invoking terminate_fn.
        // WASM-TODO(#1451) / WASM-R37-S9: actor_type_id remains zero at trace
        // drain time until WASM codegen emits handler-name/type registration.
        crate::tracing::hew_trace_lifecycle(a.id, crate::tracing::SPAN_STOP);
        // SAFETY: actor just transitioned to Stopped; not being dispatched.
        unsafe { call_terminate_fn(actor) };
        return;
    }

    // If SLEEPING, cancel the sleep-queue entry and transition to STOPPED.
    // Sleeping actors use a distinct state so message sends don't wake them
    // early; closing one must still produce an immediate terminal transition.
    if a.actor_state
        .compare_exchange(
            HewActorState::Sleeping as i32,
            HewActorState::Stopped as i32,
            Ordering::AcqRel,
            Ordering::Acquire,
        )
        .is_ok()
    {
        // SAFETY: actor is valid; cancel is safe from the scheduler thread.
        unsafe { crate::scheduler_wasm::cancel_actor_sleep_queue_entry(actor.cast()) };
        // WASM-R37-S2: mirror native stop lifecycle observability.
        // WASM-TODO(#1451) / WASM-R37-S9: actor_type_id remains zero until
        // WASM handler registration lands.
        crate::tracing::hew_trace_lifecycle(a.id, crate::tracing::SPAN_STOP);
        // SAFETY: actor just transitioned to Stopped.
        unsafe { call_terminate_fn(actor) };
    }
}

/// Stop an actor, sending a system shutdown message (WASM).
///
/// # Safety
///
/// `actor` must be a valid pointer returned by a spawn function.
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_stop(actor: *mut HewActor) {
    cabi_guard!(actor.is_null());
    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };
    if !a.mailbox.is_null() {
        // SAFETY: a.mailbox is a valid mailbox pointer.
        unsafe { hew_mailbox_close(a.mailbox) };
    }

    if a.actor_state
        .compare_exchange(
            HewActorState::Idle as i32,
            HewActorState::Stopped as i32,
            Ordering::AcqRel,
            Ordering::Acquire,
        )
        .is_ok()
    {
        // WASM-R37-S2: direct stop of an idle actor mirrors native
        // `hew_actor_stop` observability before invoking terminate_fn.
        // WASM-TODO(#1451) / WASM-R37-S9: actor_type_id remains zero at trace
        // drain time until WASM codegen emits handler-name/type registration.
        crate::tracing::hew_trace_lifecycle(a.id, crate::tracing::SPAN_STOP);
        // SAFETY: actor just transitioned to Stopped; not being dispatched.
        unsafe { call_terminate_fn(actor) };
        return;
    }

    // If SLEEPING, cancel the sleep-queue entry and stop immediately.
    if a.actor_state
        .compare_exchange(
            HewActorState::Sleeping as i32,
            HewActorState::Stopped as i32,
            Ordering::AcqRel,
            Ordering::Acquire,
        )
        .is_ok()
    {
        // SAFETY: actor is valid; cancel is safe from the scheduler thread.
        unsafe { crate::scheduler_wasm::cancel_actor_sleep_queue_entry(actor.cast()) };
        // WASM-R37-S2: mirror native stop lifecycle observability.
        // WASM-TODO(#1451) / WASM-R37-S9: actor_type_id remains zero until
        // WASM handler registration lands.
        crate::tracing::hew_trace_lifecycle(a.id, crate::tracing::SPAN_STOP);
        // SAFETY: actor just transitioned to Stopped.
        unsafe { call_terminate_fn(actor) };
        return;
    }

    let state = a.actor_state.load(Ordering::Acquire);
    if state != HewActorState::Running as i32 {
        return;
    }

    // Running actors are already inside a dispatch; enqueue one sys message so
    // the next mailbox poll observes the shutdown request. Runnable actors
    // already have queued work and do not need an extra wake-up signal.
    if !a.mailbox.is_null() {
        // SAFETY: a.mailbox is a valid mailbox pointer.
        unsafe {
            let _ = crate::mailbox_wasm::mailbox_send_stop_sys_once(a.mailbox.cast());
        }
    }
}

/// Free an actor and all associated resources (WASM).
///
/// Waits until the actor is quiescent (`Stopped`, `Crashed`, or `Idle`)
/// before untracking and freeing it, mirroring the native free contract.
///
/// # Safety
///
/// - `actor` must have been returned by a spawn function.
/// - The actor must not be used after this call.
#[cfg(any(target_arch = "wasm32", test))]
pub(crate) unsafe fn actor_free_wasm_impl(actor: *mut HewActor) -> c_int {
    if actor.is_null() {
        crate::set_last_error("hew_actor_free: null actor pointer");
        return -1;
    }

    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };

    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(2);
    loop {
        let state = a.actor_state.load(Ordering::Acquire);
        if actor_free_state_is_quiescent(state) {
            break;
        }
        if std::time::Instant::now() >= deadline {
            break;
        }
        #[cfg(target_arch = "wasm32")]
        std::hint::spin_loop();
        #[cfg(not(target_arch = "wasm32"))]
        std::thread::yield_now();
    }

    let state = a.actor_state.load(Ordering::Acquire);
    if !actor_free_state_is_quiescent(state) {
        return -2;
    }

    // Quiesce actor-owned producers before untracking. Relationship cleanup is
    // performed after retirement and pin drain for native/WASM ordering parity.
    // SAFETY: the wait loop above ensures the actor is quiescent and not dispatching.
    unsafe { prepare_quiescent_actor_for_cleanup(actor) };

    // Wake-proof + finalize decision by the CAS RESULT — parity with the native
    // bulk/terminal free paths (`cleanup_all_actors`, `drain_quiesced_actor`)
    // and the same primitive `hew_actor_free_inner` applies inline. WASM is
    // single-threaded, so the latch always observes the quiescent state the
    // gate above checked: `Ok ⇒ Finalize(Idle)` or `Err(Stopped|Crashed) ⇒
    // Finalize(s)`. The `Skip` arm is unreachable here (no concurrent waker) but
    // fails closed — leaving the actor tracked and unfreed — if it ever fires.
    // SAFETY: actor is valid and quiescent.
    let finalize_state = match decide_finalize_by_latch(a) {
        FinalizeDecision::Finalize(state) => state,
        FinalizeDecision::Skip => {
            crate::set_last_error("hew_actor_free: actor re-enqueued; leaked fail-closed");
            return -2;
        }
    };

    if !live_actors::untrack_actor(actor) {
        crate::set_last_error("hew_actor_free: actor already freed or not tracked");
        return -1;
    }

    // WASM is single-threaded: no concurrent by-ID operations can hold a
    // pin after we reach this point.
    debug_assert_eq!(
        a.send_pin_count.load(Ordering::Acquire),
        0,
        "send_pin_count must be 0 before finalize in actor_free_wasm_impl"
    );

    // SAFETY: the actor is retired and WASM has no concurrent pins.
    unsafe { scrub_actor_relationships_after_pin_drain(actor) };

    // SAFETY: actor is quiescent, wake-proofed, no longer tracked, and not
    // being dispatched.
    unsafe { finalize_quiescent_actor_cleanup(actor, finalize_state) };
    0
}

#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_free(actor: *mut HewActor) -> c_int {
    // SAFETY: same preconditions as actor_free_wasm_impl.
    unsafe { actor_free_wasm_impl(actor) }
}

#[cfg(all(test, not(target_arch = "wasm32")))]
mod tests {
    use super::*;
    use crate::execution_context::TestExecutionContext;

    struct SpawnPublicationHookGuard;

    impl SpawnPublicationHookGuard {
        fn install(
            entered: std::sync::Arc<std::sync::Barrier>,
            release: std::sync::Arc<std::sync::Barrier>,
        ) -> Self {
            *SPAWN_PUBLICATION_HOOK
                .lock()
                .unwrap_or_else(PoisonError::into_inner) = Some((entered, release));
            Self
        }
    }

    impl Drop for SpawnPublicationHookGuard {
        fn drop(&mut self) {
            *SPAWN_PUBLICATION_HOOK
                .lock()
                .unwrap_or_else(PoisonError::into_inner) = None;
        }
    }

    static LAST_NATIVE_ASK_REPLY_CHANNEL: AtomicPtr<reply_channel::HewReplyChannel> =
        AtomicPtr::new(ptr::null_mut());
    static SEND_BY_ID_DISPATCH_COUNT: std::sync::atomic::AtomicUsize =
        std::sync::atomic::AtomicUsize::new(0);
    static ASK_SEND_BY_ID_DISPATCH_COUNT: std::sync::atomic::AtomicUsize =
        std::sync::atomic::AtomicUsize::new(0);
    static DRAIN_BUSY_LOOP_STARTED: AtomicBool = AtomicBool::new(false);
    static DRAIN_BUSY_LOOP_RELEASE: AtomicBool = AtomicBool::new(false);
    static DRAIN_TRAP_ON_STOP_STARTED: AtomicBool = AtomicBool::new(false);
    /// Release flag for `drain_trap_on_stop_dispatch`: the dispatch holds
    /// in `Running` state until the test sets this, guaranteeing that
    /// `drain_actors` calls `hew_actor_stop` while the actor is still
    /// `Running` (not yet `Idle`). Without this gate the dispatch could
    /// finish before drain calls stop, causing the actor to transition
    /// `Running → Idle → Stopped` instead of `Running → Crashed`, and drain
    /// returns `Drained` instead of `Incomplete { crashed }`.
    static DRAIN_TRAP_ON_STOP_RELEASE: AtomicBool = AtomicBool::new(false);

    #[test]
    fn spawned_actor_direct_identity_retires_before_reclamation() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: runtime guard installs the owning liveness/handle authority;
        // null state with zero size and the test dispatch satisfy spawn.
        let actor = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
        assert!(!actor.is_null());
        // SAFETY: spawn returned a live actor.
        let (actor_id, token) = unsafe { ((*actor).id, (*actor).local_pid_id) };
        assert_ne!(
            token,
            crate::lifetime::local_handles::HewLocalPidId::INVALID
        );
        assert_eq!(
            crate::lifetime::local_handles::resolve_current_actor(token),
            Some(actor_id)
        );
        assert_eq!(
            crate::lifetime::local_handles::current_counts_for_test(),
            (1, 1)
        );

        // SAFETY: actor is live, idle, and not used after successful free.
        assert_eq!(unsafe { hew_actor_free(actor) }, 0);
        assert_eq!(
            crate::lifetime::local_handles::resolve_current_actor(token),
            None
        );
        assert_eq!(
            crate::lifetime::local_handles::current_counts_for_test(),
            (0, 0)
        );
    }

    #[test]
    fn duplicate_spawn_identity_preserves_original_liveness_and_route() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: runtime guard installs the owning authority; empty state is valid.
        let original = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
        assert!(!original.is_null());
        // SAFETY: original is live until the final free below.
        let (actor_id, token) = unsafe { ((*original).id, (*original).local_pid_id) };

        override_next_spawn_actor_id(actor_id);
        // SAFETY: the injected identity collision is handled before publication.
        let duplicate = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
        assert!(duplicate.is_null());
        assert_eq!(live_actors::get_actor_ptr_by_id(actor_id), Some(original));
        assert_eq!(
            crate::lifetime::local_handles::resolve_current_actor(token),
            Some(actor_id)
        );
        assert_eq!(
            crate::lifetime::local_handles::current_counts_for_test(),
            (1, 1)
        );

        // SAFETY: original remains the tracked, idle allocation.
        assert_eq!(unsafe { hew_actor_free(original) }, 0);
    }

    #[test]
    fn route_exhaustion_rolls_back_spawn_ownership_and_publication() {
        let _guard = crate::runtime_test_guard();
        crate::runtime::rt_current()
            .local_handles
            .fail_next_registration_for_test();
        let mut state = 37_u64;
        // SAFETY: state is readable for its exact size; injected exhaustion is
        // expected to release both copies, the mailbox, arena, and actor box.
        let actor = unsafe {
            hew_actor_spawn(
                (&raw mut state).cast(),
                std::mem::size_of::<u64>(),
                Some(noop_dispatch),
            )
        };
        assert!(actor.is_null());
        assert_eq!(live_actors::actor_count_for_test(), 0);
        assert_eq!(
            crate::lifetime::local_handles::current_counts_for_test(),
            (0, 0)
        );
    }

    #[test]
    fn cleanup_waits_for_atomic_actor_publication() {
        let _guard = crate::runtime_test_guard();
        let entered = std::sync::Arc::new(std::sync::Barrier::new(2));
        let release = std::sync::Arc::new(std::sync::Barrier::new(2));
        let _hook = SpawnPublicationHookGuard::install(
            std::sync::Arc::clone(&entered),
            std::sync::Arc::clone(&release),
        );

        let spawn = std::thread::spawn(|| {
            // SAFETY: the installed runtime is process-visible and empty state is valid.
            unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) as usize }
        });
        entered.wait();
        assert_eq!(live_actors::actor_count_for_test(), 0);
        assert_eq!(
            crate::lifetime::local_handles::current_counts_for_test(),
            (1, 1),
            "route reservation precedes liveness publication"
        );

        let cleanup_done = std::sync::Arc::new(AtomicBool::new(false));
        let cleanup_done_thread = std::sync::Arc::clone(&cleanup_done);
        let cleanup = std::thread::spawn(move || {
            // SAFETY: scheduler workers are absent under runtime_test_guard.
            unsafe { cleanup_all_actors() };
            cleanup_done_thread.store(true, Ordering::Release);
        });
        std::thread::sleep(std::time::Duration::from_millis(25));
        assert!(!cleanup_done.load(Ordering::Acquire));

        release.wait();
        assert_ne!(spawn.join().expect("spawn thread"), 0);
        cleanup.join().expect("cleanup thread");
        assert_eq!(live_actors::actor_count_for_test(), 0);
        assert_eq!(
            crate::lifetime::local_handles::current_counts_for_test(),
            (0, 0)
        );
    }

    #[test]
    fn local_pid_operations_resolve_stable_actor_identity() {
        let _guard = crate::runtime_test_guard();
        let runtime = NativeSchedulerGuard::new();
        // SAFETY: runtime guard installs the owning authority; empty state is valid.
        let actor = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
        assert!(!actor.is_null());
        // SAFETY: spawn returned a live actor.
        let (expected_id, token) = unsafe { ((*actor).id, (*actor).local_pid_id) };

        let mut resolved_id = 0;
        assert_eq!(
            unsafe {
                // SAFETY: resolved_id is writable for the call.
                hew_local_pid_actor_id(token, &raw mut resolved_id)
            },
            0
        );
        assert_eq!(resolved_id, expected_id);
        // SAFETY: a null payload with size zero is readable by contract.
        assert_eq!(
            unsafe {
                // SAFETY: a null payload with size zero is readable by contract.
                hew_local_pid_send(token, 17, ptr::null_mut(), 0)
            },
            HewError::Ok as i32
        );

        assert!(wait_for_condition(
            std::time::Duration::from_secs(1),
            || {
                // SAFETY: actor remains live until the free immediately below.
                (unsafe { (*actor).actor_state.load(Ordering::Acquire) })
                    == HewActorState::Idle as i32
            }
        ));
        // SAFETY: scheduler drained the message and actor is idle.
        assert_eq!(unsafe { hew_actor_free(actor) }, 0);
        assert_eq!(
            // SAFETY: resolved_id is writable for the call.
            unsafe { hew_local_pid_actor_id(token, &raw mut resolved_id) },
            HewError::ErrActorStopped as i32
        );
        // SAFETY: a null payload with size zero is readable by contract.
        assert_eq!(
            unsafe {
                // SAFETY: a null payload with size zero is readable by contract.
                hew_local_pid_send(token, 17, ptr::null_mut(), 0)
            },
            HewError::ErrActorStopped as i32
        );
        drop(runtime);
    }

    #[test]
    fn local_pid_ask_uses_actor_identity_and_clears_error_slot() {
        let _guard = crate::runtime_test_guard();
        let runtime = NativeSchedulerGuard::new();
        // SAFETY: null state and the reply dispatch form a valid actor spawn.
        let actor =
            unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(native_reply_once_dispatch)) };
        assert!(!actor.is_null());
        // SAFETY: actor is live until teardown below.
        let token = unsafe { (*actor).local_pid_id };

        LAST_ACTOR_ASK_ERROR.with(|slot| slot.set(AskError::Timeout as i32));
        // SAFETY: a null payload with size zero is readable by contract.
        let reply = unsafe { hew_local_pid_ask(token, 1, ptr::null_mut(), 0) };
        assert!(!reply.is_null());
        // SAFETY: successful replies are malloc-allocated.
        unsafe { libc::free(reply) };
        assert_eq!(hew_actor_ask_take_last_error(), AskError::None as i32);

        // SAFETY: ask completed and actor is idle.
        assert_eq!(unsafe { hew_actor_free(actor) }, 0);
        drop(runtime);
    }

    #[test]
    fn local_pid_ask_with_channel_failure_preserves_caller_reference() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: null state and valid dispatch form a valid actor spawn.
        let actor = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
        assert!(!actor.is_null());
        // SAFETY: actor is live until teardown below.
        let token = unsafe { (*actor).local_pid_id };
        // SAFETY: actor is live; close makes subsequent asks fail closed.
        unsafe { hew_actor_close(actor) };

        let before = reply_channel::active_channel_count();
        let ch = reply_channel::hew_reply_channel_new();
        assert_eq!(reply_channel::active_channel_count(), before + 1);
        // SAFETY: ch is caller-owned and payload is empty.
        let status =
            unsafe { hew_local_pid_ask_with_channel(token, 1, ptr::null_mut(), 0, ch.cast()) };
        assert_eq!(status, HewError::ErrActorStopped as i32);
        assert_eq!(
            reply_channel::active_channel_count(),
            before + 1,
            "failed token ask must preserve the caller-owned channel reference"
        );

        // SAFETY: release the preserved caller ref, then free the closed actor.
        unsafe {
            reply_channel::hew_reply_channel_free(ch);
            assert_eq!(hew_actor_free(actor), 0);
        }
        assert_eq!(reply_channel::active_channel_count(), before);
    }

    #[test]
    fn actor_identity_pin_blocks_reclamation_until_guard_drop() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: empty state and no-op dispatch form a valid actor spawn.
        let actor = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
        assert!(!actor.is_null());
        // SAFETY: actor remains allocated while the pin is held.
        let actor_id = unsafe { (*actor).id };
        let pin = live_actors::pin_actor_by_id(actor_id).expect("live actor pin");

        let free_started = std::sync::Arc::new(std::sync::Barrier::new(2));
        let free_started_thread = std::sync::Arc::clone(&free_started);
        let free_done = std::sync::Arc::new(AtomicBool::new(false));
        let free_done_thread = std::sync::Arc::clone(&free_done);
        let actor_addr = actor as usize;
        let free = std::thread::spawn(move || {
            free_started_thread.wait();
            // SAFETY: the actor remains pinned until the main thread releases it.
            let status = unsafe { hew_actor_free(actor_addr as *mut HewActor) };
            free_done_thread.store(true, Ordering::Release);
            status
        });

        free_started.wait();
        std::thread::sleep(std::time::Duration::from_millis(25));
        assert!(
            !free_done.load(Ordering::Acquire),
            "actor free must wait for the identity pin"
        );
        drop(pin);
        assert_eq!(free.join().expect("free thread"), 0);
        assert!(free_done.load(Ordering::Acquire));
    }

    #[test]
    fn actor_cleanup_drains_every_direct_identity() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: runtime guard installs the authority and both spawns use
        // empty test state.
        let first = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
        // SAFETY: same runtime and empty-state preconditions as the first spawn.
        let second = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
        assert!(!first.is_null() && !second.is_null());
        assert_eq!(
            crate::lifetime::local_handles::current_counts_for_test(),
            (2, 2)
        );

        // SAFETY: no scheduler workers or dispatches exist under the test guard.
        unsafe { cleanup_all_actors() };
        assert_eq!(
            crate::lifetime::local_handles::current_counts_for_test(),
            (0, 0)
        );
    }

    #[test]
    fn take_by_actor_id_retires_exact_direct_identity() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: runtime guard installs the authority; empty state satisfies spawn.
        let actor = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
        assert!(!actor.is_null());
        // SAFETY: spawn returned a live actor.
        let (actor_id, token) = unsafe { ((*actor).id, (*actor).local_pid_id) };
        // SAFETY: the fresh actor is idle and has no external registrations.
        unsafe { prepare_quiescent_actor_for_cleanup(actor) };
        // SAFETY: actor remains live through the latch decision.
        let finalize_state = match decide_finalize_by_latch(unsafe { &*actor }) {
            FinalizeDecision::Finalize(state) => state,
            FinalizeDecision::Skip => panic!("fresh idle actor must be finalizable"),
        };

        assert_eq!(live_actors::take_actor_by_id(actor_id, actor), Some(actor));
        assert_eq!(
            crate::lifetime::local_handles::resolve_current_actor(token),
            None
        );
        assert_eq!(
            crate::lifetime::local_handles::current_counts_for_test(),
            (0, 0)
        );
        // SAFETY: the actor is retired and has no pins.
        unsafe { scrub_actor_relationships_after_pin_drain(actor) };
        // SAFETY: actor is wake-proof, untracked, unpinned, and test-owned.
        unsafe { finalize_quiescent_actor_cleanup(actor, finalize_state) };
    }

    /// With no execution context installed, the diagnostic accessor
    /// `hew_actor_current_id` writes `EXECUTION_CONTEXT_NOT_INSTALLED` into the
    /// generic last-error slot (callers treating an absent context as a failure
    /// depend on that), while `hew_actor_current_id_silent` returns the same -1
    /// without touching the slot — it is a routing probe, not a diagnostic.
    #[test]
    fn silent_probe_diverges_from_diagnostic_on_missing_context() {
        let prev = crate::execution_context::set_current_context(ptr::null_mut());

        crate::hew_clear_error();
        assert_eq!(hew_actor_current_id_silent(), -1);
        assert!(
            crate::hew_last_error().is_null(),
            "silent probe must not write the generic last-error slot"
        );

        assert_eq!(hew_actor_current_id(), -1);
        let err = crate::hew_last_error();
        assert!(
            !err.is_null(),
            "diagnostic accessor must record the missing-context error"
        );
        // SAFETY: hew_last_error returned a non-null, NUL-terminated C string
        // owned by the thread-local slot; it stays valid until the next write.
        let msg = unsafe { std::ffi::CStr::from_ptr(err) }
            .to_str()
            .expect("last-error message is valid UTF-8");
        assert_eq!(
            msg,
            crate::execution_context::EXECUTION_CONTEXT_NOT_INSTALLED
        );

        crate::hew_clear_error();
        let _ = crate::execution_context::set_current_context(prev);
    }

    /// `Suspended` is non-quiescent: a suspended actor owns a live continuation
    /// frame, so a `hew_actor_free` caller spinning on the state must block
    /// through the `Suspended` window rather than freeing the box out from
    /// under the parked continuation (R7 / `cleanup-all-exits`). The only
    /// quiescent states are the truly idle/terminal ones.
    #[test]
    fn suspended_state_is_not_quiescent() {
        assert!(
            !actor_free_state_is_quiescent(HewActorState::Suspended as i32),
            "Suspended owns a live frame and must block actor_free, like Sleeping/Crashing"
        );
        // The quiescent set is exactly Idle/Stopped/Crashed; no live-frame or
        // in-flight state may leak into it.
        assert!(actor_free_state_is_quiescent(HewActorState::Idle as i32));
        assert!(actor_free_state_is_quiescent(HewActorState::Stopped as i32));
        assert!(actor_free_state_is_quiescent(HewActorState::Crashed as i32));
        assert!(!actor_free_state_is_quiescent(
            HewActorState::Running as i32
        ));
        assert!(!actor_free_state_is_quiescent(
            HewActorState::Runnable as i32
        ));
        assert!(!actor_free_state_is_quiescent(
            HewActorState::Sleeping as i32
        ));
        assert!(!actor_free_state_is_quiescent(
            HewActorState::Crashing as i32
        ));
        assert!(!actor_free_state_is_quiescent(
            HewActorState::Stopping as i32
        ));
    }

    struct NativeSchedulerGuard;

    impl NativeSchedulerGuard {
        fn new() -> Self {
            // Retire any `runtime_test_guard()` worker-less placeholder, then
            // install a real worker-backed scheduler (see
            // `init_real_scheduler_for_test`). This guard's `Drop` tears the
            // real runtime down symmetrically (`hew_sched_shutdown` +
            // `hew_runtime_cleanup`), so its workers are joined before free.
            crate::scheduler::init_real_scheduler_for_test();
            Self
        }
    }

    impl Drop for NativeSchedulerGuard {
        fn drop(&mut self) {
            crate::scheduler::hew_sched_shutdown();
            crate::scheduler::hew_runtime_cleanup();
        }
    }

    unsafe extern "C-unwind" fn noop_dispatch(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        std::ptr::null_mut()
    }

    unsafe extern "C-unwind" fn count_send_by_id_dispatch(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        SEND_BY_ID_DISPATCH_COUNT.fetch_add(1, Ordering::AcqRel);

        std::ptr::null_mut()
    }

    unsafe extern "C-unwind" fn count_ask_send_by_id_dispatch(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        ASK_SEND_BY_ID_DISPATCH_COUNT.fetch_add(1, Ordering::AcqRel);
        let ch = crate::scheduler::hew_get_reply_channel();
        if ch.is_null() {
            return std::ptr::null_mut();
        }
        let mut value: i32 = 7;
        // SAFETY: `ch` is the scheduler-installed reply channel for this dispatch
        // and `value` lives for the duration of the call.
        unsafe {
            let _ = crate::reply_channel::hew_reply(
                ch.cast(),
                (&raw mut value).cast(),
                std::mem::size_of::<i32>(),
            );
        }

        std::ptr::null_mut()
    }

    unsafe extern "C-unwind" fn drain_busy_loop_dispatch(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        DRAIN_BUSY_LOOP_STARTED.store(true, Ordering::Release);
        while !DRAIN_BUSY_LOOP_RELEASE.load(Ordering::Acquire) {
            std::hint::spin_loop();
            std::thread::sleep(std::time::Duration::from_millis(1));
        }

        std::ptr::null_mut()
    }

    unsafe extern "C-unwind" fn drain_trap_on_stop_dispatch(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        DRAIN_TRAP_ON_STOP_STARTED.store(true, Ordering::Release);
        // Hold in Running until the test's release thread observes that
        // drain_actors has called hew_actor_stop (the shutdown system message is
        // queued). This prevents the dispatch from finishing before the stop is
        // requested, which would let the actor transition Running→Idle→Stopped
        // instead of crashing mid-drain and yield Drained rather than
        // Incomplete{crashed}.
        while !DRAIN_TRAP_ON_STOP_RELEASE.load(Ordering::Acquire) {
            std::hint::spin_loop();
            std::thread::sleep(std::time::Duration::from_millis(1));
        }

        // Crash from within this still-Running dispatch, modelling an actor that
        // faults as it is being drained. The crash trigger is a self-trap rather
        // than handling the shutdown sentinel: the scheduler now intercepts the
        // `msg_type == -1` shutdown signal as a clean self-stop and never
        // delivers it to a handler (a real codegen actor has no arm for it and
        // would trap on the dispatch default arm).
        // SAFETY: this runs on the actor's own dispatch thread while its context is installed.
        unsafe { hew_actor_trap(hew_actor_self(), 77) };

        std::ptr::null_mut()
    }

    fn wait_for_condition(
        timeout: std::time::Duration,
        mut condition: impl FnMut() -> bool,
    ) -> bool {
        let deadline = std::time::Instant::now() + timeout;
        while std::time::Instant::now() < deadline {
            if condition() {
                return true;
            }
            std::thread::sleep(std::time::Duration::from_millis(10));
        }
        condition()
    }

    fn defer_state_transition(
        actor: *mut HewActor,
        target_state: HewActorState,
        delay: std::time::Duration,
    ) -> std::thread::JoinHandle<()> {
        let actor_addr = actor as usize;
        std::thread::spawn(move || {
            std::thread::sleep(delay);
            // SAFETY: the test keeps the actor allocation alive until the
            // background transition fires.
            unsafe {
                (*(actor_addr as *mut HewActor))
                    .actor_state
                    .store(target_state as i32, Ordering::Release);
            }
        })
    }

    fn wait_for_actor_quiescent(actor: *mut HewActor, timeout: std::time::Duration) -> bool {
        wait_for_condition(timeout, || {
            // SAFETY: tests only call this while the actor allocation is still live.
            let state = unsafe { (*actor).actor_state.load(Ordering::Acquire) };
            actor_free_state_is_quiescent(state)
        })
    }

    unsafe extern "C-unwind" fn native_self_stop_without_reply_dispatch(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        let ch = crate::scheduler::hew_get_reply_channel().cast::<reply_channel::HewReplyChannel>();
        LAST_NATIVE_ASK_REPLY_CHANNEL.store(ch, Ordering::Release);
        hew_actor_self_stop();

        std::ptr::null_mut()
    }

    unsafe extern "C-unwind" fn native_reply_once_dispatch(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        let ch = crate::scheduler::hew_get_reply_channel();
        if ch.is_null() {
            return std::ptr::null_mut();
        }
        let mut value: i32 = 21;
        // SAFETY: `ch` is the scheduler-installed reply channel for this dispatch
        // and `value` lives for the duration of the call.
        unsafe {
            let _ = crate::reply_channel::hew_reply(
                ch.cast(),
                (&raw mut value).cast(),
                std::mem::size_of::<i32>(),
            );
        }

        std::ptr::null_mut()
    }

    unsafe extern "C-unwind" fn native_late_reply_dispatch(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        std::thread::sleep(std::time::Duration::from_millis(20));
        let ch = crate::scheduler::hew_get_reply_channel();
        if ch.is_null() {
            return std::ptr::null_mut();
        }
        let mut value: i32 = 99;
        // SAFETY: `ch` is the scheduler-installed reply channel for this dispatch
        // and `value` lives for the duration of the call.
        unsafe {
            let _ = crate::reply_channel::hew_reply(
                ch.cast(),
                (&raw mut value).cast(),
                std::mem::size_of::<i32>(),
            );
        }

        std::ptr::null_mut()
    }

    unsafe extern "C-unwind" fn native_reply_then_trap_dispatch(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        let ch = crate::scheduler::hew_get_reply_channel();
        if ch.is_null() {
            return std::ptr::null_mut();
        }
        let mut value: i32 = 123;
        // SAFETY: `ch` is the scheduler-installed reply channel for this dispatch
        // and `value` lives for the duration of the call.
        unsafe {
            let _ = crate::reply_channel::hew_reply(
                ch.cast(),
                (&raw mut value).cast(),
                std::mem::size_of::<i32>(),
            );
        }
        hew_panic();

        std::ptr::null_mut()
    }

    fn make_stop_test_actor(initial_state: HewActorState) -> (*mut HewActor, *mut HewMailbox) {
        make_stop_test_actor_with_id(1, initial_state)
    }

    fn make_stop_test_actor_with_id(
        id: u64,
        initial_state: HewActorState,
    ) -> (*mut HewActor, *mut HewMailbox) {
        // SAFETY: test helper fully owns the returned actor/mailbox and never publishes them.
        unsafe {
            let mailbox = mailbox::hew_mailbox_new();
            assert!(!mailbox.is_null());
            let actor = Box::into_raw(Box::new(HewActor {
                sched_link_next: AtomicPtr::new(ptr::null_mut()),
                id,
                state: ptr::null_mut(),
                state_size: 0,
                dispatch: Some(noop_dispatch),
                mailbox: mailbox.cast(),
                actor_state: AtomicI32::new(initial_state as i32),
                budget: AtomicI32::new(HEW_MSG_BUDGET),
                init_state: ptr::null_mut(),
                init_state_size: 0,
                coalesce_key_fn: None,
                terminate_fn: None,
                state_drop_fn: None,
                state_clone_fn: None,
                terminate_called: AtomicBool::new(false),
                terminate_finished: AtomicBool::new(false),
                dispatch_active: AtomicBool::new(false),
                error_code: AtomicI32::new(0),
                supervisor: ptr::null_mut(),
                supervisor_child_index: -1,
                priority: AtomicI32::new(HEW_PRIORITY_NORMAL),
                reductions: AtomicI32::new(HEW_DEFAULT_REDUCTIONS),
                idle_count: AtomicI32::new(0),
                hibernation_threshold: AtomicI32::new(0),
                hibernating: AtomicI32::new(0),
                prof_messages_processed: AtomicU64::new(0),
                prof_processing_time_ns: AtomicU64::new(0),
                arena: ptr::null_mut(),
                suspended_cont: AtomicPtr::new(std::ptr::null_mut()),
                cont_tag: AtomicI32::new(crate::internal::types::ContTag::Empty as i32),
                pending_wake: AtomicBool::new(false),
                suspended_reply_channel: AtomicPtr::new(std::ptr::null_mut()),
                suspended_cancel_token: AtomicPtr::new(std::ptr::null_mut()),
                runtime_id: crate::runtime_id::RuntimeId::DEFAULT,
                runtime: ptr::null(),
                send_pin_count: AtomicU32::new(0),
                gen_sink: AtomicPtr::new(ptr::null_mut()),
                local_pid_id: crate::lifetime::local_handles::HewLocalPidId::INVALID,
            }));
            (actor, mailbox)
        }
    }

    fn make_tracked_wasm_free_test_actor(initial_state: HewActorState) -> *mut HewActor {
        let actor_id = crate::pid::next_actor_id(NEXT_ACTOR_SERIAL.fetch_add(1, Ordering::Relaxed));
        let actor = Box::into_raw(Box::new(HewActor {
            sched_link_next: AtomicPtr::new(ptr::null_mut()),
            id: actor_id,
            state: ptr::null_mut(),
            state_size: 0,
            dispatch: Some(noop_dispatch),
            mailbox: ptr::null_mut(),
            actor_state: AtomicI32::new(initial_state as i32),
            budget: AtomicI32::new(HEW_MSG_BUDGET),
            init_state: ptr::null_mut(),
            init_state_size: 0,
            coalesce_key_fn: None,
            terminate_fn: None,
            state_drop_fn: None,
            state_clone_fn: None,
            terminate_called: AtomicBool::new(false),
            terminate_finished: AtomicBool::new(false),
            dispatch_active: AtomicBool::new(false),
            error_code: AtomicI32::new(0),
            supervisor: ptr::null_mut(),
            supervisor_child_index: -1,
            priority: AtomicI32::new(HEW_PRIORITY_NORMAL),
            reductions: AtomicI32::new(HEW_DEFAULT_REDUCTIONS),
            idle_count: AtomicI32::new(0),
            hibernation_threshold: AtomicI32::new(0),
            hibernating: AtomicI32::new(0),
            prof_messages_processed: AtomicU64::new(0),
            prof_processing_time_ns: AtomicU64::new(0),
            arena: ptr::null_mut(),
            suspended_cont: AtomicPtr::new(std::ptr::null_mut()),
            cont_tag: AtomicI32::new(crate::internal::types::ContTag::Empty as i32),
            pending_wake: AtomicBool::new(false),
            suspended_reply_channel: AtomicPtr::new(std::ptr::null_mut()),
            suspended_cancel_token: AtomicPtr::new(std::ptr::null_mut()),
            runtime_id: crate::runtime_id::RuntimeId::DEFAULT,
            runtime: ptr::null(),
            send_pin_count: AtomicU32::new(0),
            gen_sink: AtomicPtr::new(ptr::null_mut()),
            local_pid_id: crate::lifetime::local_handles::HewLocalPidId::INVALID,
        }));
        // SAFETY: actor is fully initialised above with a valid id field.
        assert!(unsafe { live_actors::track_actor(actor) });
        actor
    }

    // --- null-guard regression tests ---
    //
    // Each test passes a null pointer to an FFI setter/getter that previously
    // dereferenced unconditionally.  The expected behaviour after this fix is:
    //  - void functions: return without crashing (SIGSEGV before fix)
    //  - value functions: return the documented zero sentinel
    //
    // These tests do NOT need a scheduler or a real actor allocation.

    #[test]
    fn null_actor_close_returns_without_crash() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: null is the input we are testing the guard against.
        unsafe { hew_actor_close(ptr::null_mut()) };
    }

    #[test]
    fn actor_self_without_execution_context_fails_closed() {
        let _guard = crate::runtime_test_guard();
        crate::hew_clear_error();
        assert!(hew_actor_self().is_null());
        let err = crate::hew_last_error();
        assert!(!err.is_null());
        // SAFETY: hew_last_error returned a non-null C string.
        let err = unsafe { std::ffi::CStr::from_ptr(err).to_str().unwrap() };
        assert_eq!(
            err,
            crate::execution_context::EXECUTION_CONTEXT_NOT_INSTALLED
        );
        crate::hew_clear_error();
    }

    #[test]
    fn null_actor_stop_returns_without_crash() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: null is the input we are testing the guard against.
        unsafe { hew_actor_stop(ptr::null_mut()) };
    }

    #[test]
    fn null_actor_set_budget_returns_without_crash() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: null is the input we are testing the guard against.
        unsafe { hew_actor_set_budget(ptr::null_mut(), 10) };
    }

    #[test]
    fn null_actor_get_budget_returns_sentinel() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: null is the input we are testing the guard against.
        let v = unsafe { hew_actor_get_budget(ptr::null()) };
        assert_eq!(v, 0, "expected zero sentinel for null actor");
    }

    unsafe extern "C" fn null_guard_dummy_terminate(_: *mut c_void) {}

    #[test]
    fn null_actor_set_terminate_returns_without_crash() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: null is the input we are testing the guard against.
        unsafe { hew_actor_set_terminate(ptr::null_mut(), null_guard_dummy_terminate) };
    }

    #[test]
    fn null_actor_set_reductions_returns_without_crash() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: null is the input we are testing the guard against.
        unsafe { hew_actor_set_reductions(ptr::null_mut(), 5) };
    }

    #[test]
    fn null_actor_get_reductions_returns_sentinel() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: null is the input we are testing the guard against.
        let v = unsafe { hew_actor_get_reductions(ptr::null()) };
        assert_eq!(v, 0, "expected zero sentinel for null actor");
    }

    #[test]
    fn null_actor_pid_returns_sentinel() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: null is the input we are testing the guard against.
        let v = unsafe { hew_actor_pid(ptr::null_mut()) };
        assert_eq!(v, 0, "expected zero sentinel for null actor");
    }

    // --- null-guard regression tests for the high-frequency send/ask paths ---
    //
    // These cover the paths the prior batch missed: `hew_actor_send`,
    // `hew_actor_try_send`, and the ask-family helper.  Each test passes a
    // null actor pointer and expects the guard to fire without a SIGSEGV and
    // to return `HewError::ErrActorStopped` for i32-returning variants.

    #[test]
    fn null_actor_send_returns_without_crash() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: null is the input we are testing the guard against.
        unsafe { hew_actor_send(ptr::null_mut(), 0, ptr::null_mut(), 0) };
    }

    #[test]
    fn null_actor_try_send_returns_err_actor_stopped() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: null is the input we are testing the guard against.
        let result = unsafe { hew_actor_try_send(ptr::null_mut(), 0, ptr::null_mut(), 0) };
        assert_eq!(
            result,
            HewError::ErrActorStopped as i32,
            "expected ErrActorStopped for null actor"
        );
    }

    #[test]
    fn null_actor_send_result_internal_reply_returns_err_actor_stopped() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: null is the input we are testing the guard against.
        let result = unsafe {
            actor_send_result_internal_reply(
                ptr::null_mut(),
                0,
                ptr::null_mut(),
                0,
                ptr::null_mut(),
            )
        };
        assert_eq!(
            result,
            HewError::ErrActorStopped as i32,
            "expected ErrActorStopped for null actor"
        );
    }

    #[test]
    fn null_actor_ask_with_channel_wasm_internal_returns_err_actor_stopped() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: null actor is the input we are testing the guard against.
        // A null ch is safe here because the guard fires before the retain.
        let result = unsafe {
            ask_with_channel_wasm_internal(ptr::null_mut(), 0, ptr::null_mut(), 0, ptr::null_mut())
        };
        assert_eq!(
            result,
            HewError::ErrActorStopped as i32,
            "expected ErrActorStopped for null actor"
        );
    }

    #[test]
    fn send_by_id_concurrent_no_deadlock() {
        let _guard = crate::runtime_test_guard();
        let _scheduler = NativeSchedulerGuard::new();
        SEND_BY_ID_DISPATCH_COUNT.store(0, Ordering::Release);

        // SAFETY: null state + valid dispatch are valid spawn args.
        let actor =
            unsafe { hew_actor_spawn(std::ptr::null_mut(), 0, Some(count_send_by_id_dispatch)) };
        assert!(!actor.is_null());

        // SAFETY: actor is live for the duration of the test.
        let actor_id = unsafe { (*actor).id };
        let thread_count = 8usize;
        let sends_per_thread = 32usize;
        let start = std::sync::Arc::new(std::sync::Barrier::new(thread_count));
        let mut handles = Vec::with_capacity(thread_count);

        for _ in 0..thread_count {
            let start = start.clone();
            handles.push(std::thread::spawn(move || {
                start.wait();
                for _ in 0..sends_per_thread {
                    // SAFETY: actor remains live until all sender threads join.
                    let rc = unsafe {
                        hew_actor_send_by_id(actor_id, ptr::null(), 1, ptr::null_mut(), 0)
                    };
                    assert_eq!(rc, 0);
                }
            }));
        }

        for handle in handles {
            handle.join().expect("send thread must not panic");
        }

        let expected = thread_count * sends_per_thread;
        assert!(
            wait_for_condition(std::time::Duration::from_secs(2), || {
                SEND_BY_ID_DISPATCH_COUNT.load(Ordering::Acquire) == expected
            }),
            "scheduler should drain all by-id sends without deadlocking"
        );

        // SAFETY: actor remains live until teardown below.
        unsafe {
            hew_actor_close(actor);
            assert_eq!(hew_actor_free(actor), 0);
        }
    }

    /// A fire-and-forget send-by-id to an actor ID that is no longer
    /// tracked locally (freed, stopped, or never existed) is a genuine,
    /// caller-visible failure. It must report a code DISTINCT from
    /// `ErrMailboxFull` — the status that a declared bounded mailbox's
    /// `DropNew`/`DropOld`/`Coalesce` policy-drop resolves to `Ok` and
    /// returns (see `send_by_id_dropnew_policy_drop_is_silent` below).
    ///
    /// Before this fix, `hew_actor_send_by_id` returned the same `-1` for
    /// both cases, preventing codegen's `Terminator::Send` from
    /// distinguishing genuine failure from policy-drop without swallowing
    /// the genuine failure.
    #[test]
    fn send_by_id_after_free_returns_genuine_failure_not_mailbox_full() {
        let _guard = crate::runtime_test_guard();

        // SAFETY: null state + valid dispatch are valid spawn args.
        let actor = unsafe { hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch)) };
        assert!(!actor.is_null());

        // SAFETY: actor is valid until the free below.
        let actor_id = unsafe { (*actor).id };

        // SAFETY: actor is quiescent after close and fully owned by this test.
        unsafe {
            hew_actor_close(actor);
            assert_eq!(hew_actor_free(actor), 0);
        }

        // SAFETY: caller only provides message bytes; the runtime should reject
        // the now-untracked actor ID instead of crashing.
        let rc = unsafe { hew_actor_send_by_id(actor_id, ptr::null(), 1, ptr::null_mut(), 0) };
        assert_eq!(
            rc,
            HewError::ErrActorStopped as i32,
            "send-by-id to a gone actor must report ErrActorStopped, not the \
             ErrMailboxFull code a declared overflow policy-drop also used to \
             report — the two must never collapse onto the same value"
        );
        assert_ne!(
            rc,
            HewError::ErrMailboxFull as i32,
            "a genuine send failure must never be confused with a policy-drop"
        );
    }

    /// A fire-and-forget send-by-id into a `DropNew` bounded mailbox that
    /// is at capacity is a declared-silent policy outcome (spec §6.2) and
    /// must report success (`0`), not `ErrMailboxFull`. Paired with
    /// `send_by_id_after_free_returns_genuine_failure_not_mailbox_full`:
    /// the two scenarios must produce DIFFERENT codes so `Terminator::Send`
    /// can trap on genuine failure while treating policy-drop as success.
    #[test]
    fn send_by_id_dropnew_policy_drop_is_silent() {
        let _guard = crate::runtime_test_guard();

        let opts = HewActorOpts {
            init_state: ptr::null_mut(),
            state_size: 0,
            dispatch: Some(noop_dispatch),
            mailbox_capacity: 1,
            overflow: HewOverflowPolicy::DropNew as i32,
            coalesce_key_fn: None,
            coalesce_fallback: 0,
            message_drop_fn: None,
            budget: 0,
            arena_cap_bytes: 0,
            cycle_capable: 0,
        };
        // SAFETY: opts is valid for the duration of the call.
        let actor = unsafe { hew_actor_spawn_opts(&raw const opts) };
        assert!(!actor.is_null(), "bounded DropNew spawn must succeed");
        // SAFETY: actor is valid; the mailbox pointer is valid for its lifetime.
        let actor_id = unsafe { (*actor).id };
        // SAFETY: actor is valid; the mailbox pointer is valid for its lifetime.
        let mb = unsafe { (*actor).mailbox.cast::<mailbox::HewMailbox>() };

        // Directly fill the one capacity slot, bypassing the scheduler so the
        // actor stays Idle and the slot stays occupied (same technique as
        // `native_ask_bounded_mailbox_full_sets_mailbox_full_error`).
        // SAFETY: mb is a valid, non-null pointer to a HewMailbox owned by this actor.
        let pre_fill = unsafe { mailbox::hew_mailbox_send(mb, 1, ptr::null_mut(), 0) };
        assert_eq!(pre_fill, HewError::Ok as i32, "pre-fill must succeed");

        // The mailbox is now at capacity; this send must overflow into the
        // DropNew policy and be reported as silent success.
        // SAFETY: actor remains live for this call.
        let rc = unsafe { hew_actor_send_by_id(actor_id, ptr::null(), 1, ptr::null_mut(), 0) };
        assert_eq!(
            rc,
            HewError::Ok as i32,
            "a DropNew policy-drop on a fire-and-forget send must be silent (Ok), \
             per spec §6.2"
        );

        // Actor is still Idle (no state transition occurred: the DropNew
        // overflow never enqueues, so nothing wakes the scheduler).
        // hew_actor_stop CAS Idle → Stopped succeeds directly; no scheduler
        // needed — mirrors `native_ask_bounded_mailbox_full_sets_mailbox_full_error`.
        // SAFETY: actor is valid; stopping a live actor is safe.
        unsafe { hew_actor_stop(actor) };
        // SAFETY: actor is Stopped (quiescent); hew_mailbox_free drains the
        // pre-filled message during free_actor_resources.
        assert_eq!(unsafe { hew_actor_free(actor) }, 0);
    }

    /// The `Fail` overflow policy is fail-closed for fire-and-forget sends.
    /// Unlike `DropNew`/`DropOld`/`Coalesce` (which are spec-silent), the
    /// `Fail` policy is an explicit, genuine rejection: it must report a
    /// distinct non-zero code even on the no-reply-channel send path, so
    /// `Terminator::Send` traps rather than silently dropping the message.
    #[test]
    fn send_by_id_fail_policy_overflow_is_genuine_failure() {
        let _guard = crate::runtime_test_guard();

        let opts = HewActorOpts {
            init_state: ptr::null_mut(),
            state_size: 0,
            dispatch: Some(noop_dispatch),
            mailbox_capacity: 1,
            overflow: HewOverflowPolicy::Fail as i32,
            coalesce_key_fn: None,
            coalesce_fallback: 0,
            message_drop_fn: None,
            budget: 0,
            arena_cap_bytes: 0,
            cycle_capable: 0,
        };
        // SAFETY: opts is valid for the duration of the call.
        let actor = unsafe { hew_actor_spawn_opts(&raw const opts) };
        assert!(!actor.is_null(), "bounded Fail spawn must succeed");
        // SAFETY: actor is valid; the mailbox pointer is valid for its lifetime.
        let actor_id = unsafe { (*actor).id };
        // SAFETY: actor is valid; the mailbox pointer is valid for its lifetime.
        let mb = unsafe { (*actor).mailbox.cast::<mailbox::HewMailbox>() };

        // SAFETY: mb is a valid, non-null pointer to a HewMailbox owned by this actor.
        let pre_fill = unsafe { mailbox::hew_mailbox_send(mb, 1, ptr::null_mut(), 0) };
        assert_eq!(pre_fill, HewError::Ok as i32, "pre-fill must succeed");

        // SAFETY: actor remains live for this call.
        let rc = unsafe { hew_actor_send_by_id(actor_id, ptr::null(), 1, ptr::null_mut(), 0) };
        assert_ne!(
            rc,
            HewError::Ok as i32,
            "Fail-policy overflow must not silently succeed on a fire-and-forget send"
        );

        // SAFETY: actor is valid; stopping a live actor is safe.
        unsafe { hew_actor_stop(actor) };
        // SAFETY: actor is Stopped (quiescent); hew_mailbox_free drains the
        // pre-filled message during free_actor_resources.
        assert_eq!(unsafe { hew_actor_free(actor) }, 0);
    }

    /// A held actor pointer stamped with a different `runtime_id` than the
    /// runtime bound on this thread fails closed on every held-pointer send
    /// path (`boundary-fail-closed`): the boundary refuses with
    /// `ErrForeignRuntime` and never routes the foreign pointer. In a
    /// single-runtime program this never fires, so a DEFAULT-stamped actor
    /// (the control) still accepts the same sends.
    ///
    /// This stamps the discriminant directly rather than constructing a second
    /// worker-backed runtime: the check compares two `RuntimeId`s and the
    /// second runtime is never dereferenced, so a foreign id is sufficient to
    /// exercise the wall without standing up a second scheduler.
    #[test]
    fn cross_runtime_send_fails_closed() {
        let _guard = crate::runtime_test_guard();

        // The thread is bound to the default runtime (RuntimeId::DEFAULT) via
        // the test guard. Build a fully-formed test actor and re-stamp it as if
        // it were spawned by a different runtime.
        let (actor, mailbox) = make_stop_test_actor_with_id(0xBEEF, HewActorState::Idle);
        // SAFETY: the test exclusively owns `actor` and never publishes it.
        unsafe {
            (*actor).runtime_id = crate::runtime_id::RuntimeId(1);
        }

        // Every held-pointer send path refuses the foreign actor.
        // SAFETY: `actor` is valid and fully owned by this test; null payload.
        let try_rc = unsafe { hew_actor_try_send(actor, 1, ptr::null_mut(), 0) };
        assert_eq!(
            try_rc,
            HewError::ErrForeignRuntime as i32,
            "try_send to a foreign-runtime actor must fail closed"
        );

        // SAFETY: as above.
        let guaranteed_rc = unsafe { hew_actor_send_guaranteed(actor, 1, ptr::null_mut(), 0) };
        assert_eq!(
            guaranteed_rc,
            HewError::ErrForeignRuntime as i32,
            "send_guaranteed to a foreign-runtime actor must fail closed"
        );

        // The fire-and-forget result path (used by `hew_actor_send`) also
        // refuses; assert on the result-returning internal it delegates to.
        // SAFETY: as above.
        let send_rc = unsafe { actor_send_result_internal(actor, 1, ptr::null_mut(), 0) };
        assert_eq!(
            send_rc,
            HewError::ErrForeignRuntime as i32,
            "send to a foreign-runtime actor must fail closed"
        );

        // The refusal must NOT have enqueued anything: no message reached the
        // mailbox, so nothing was routed to the foreign actor.
        // SAFETY: `mailbox` is valid and owned by this test.
        let has_messages = unsafe { mailbox::hew_mailbox_has_messages(mailbox) };
        assert_eq!(
            has_messages, 0,
            "a refused cross-runtime send must not enqueue a message"
        );

        // Control: re-stamp as the default runtime and the SAME send now
        // succeeds (the check is invisible single-runtime).
        // SAFETY: the test owns `actor`.
        unsafe {
            (*actor).runtime_id = crate::runtime_id::RuntimeId::DEFAULT;
        }
        // SAFETY: as above.
        let ok_rc = unsafe { hew_actor_try_send(actor, 1, ptr::null_mut(), 0) };
        assert_eq!(
            ok_rc,
            HewError::Ok as i32,
            "a same-runtime actor must accept the send"
        );

        // SAFETY: the test fully owns the actor and its mailbox.
        unsafe {
            drop(Box::from_raw(actor));
            mailbox::hew_mailbox_free(mailbox);
        }
    }

    /// V2(a–c): the off-dispatch producer choke point `enter_actor_runtime`
    /// binds the actor's OWNING runtime, not the process default. A second
    /// worker-less runtime is minted carrying `RuntimeId(1)`, and an actor is
    /// stamped to it (both `runtime` pointer and `runtime_id` from that runtime,
    /// the spawn invariant). Then:
    ///   (a) WITHOUT `enter_actor_runtime` the thread is bound to the default
    ///       (`RuntimeId::DEFAULT`) via the test guard, so a held-pointer send
    ///       fails closed `ErrForeignRuntime` (the existing cross-runtime wall);
    ///   (b) WITH `enter_actor_runtime(actor)` the guard binds `RuntimeId(1)` —
    ///       `rt_current_id()` is asserted to OBSERVE it (anti-vacuous: the bind
    ///       is checked, not assumed, per `static-classification-vacuates-…`) —
    ///       and the SAME send now succeeds and enqueues; the guard restores the
    ///       previous (default) binding on drop (`lifecycle-symmetry`);
    ///   (c) the by-construction skew invariant holds: the actor's owning-runtime
    ///       stamp id equals its `runtime_id`.
    #[test]
    fn enter_actor_runtime_binds_owning_runtime_off_dispatch() {
        let _guard = crate::runtime_test_guard();

        // Mint a second, worker-less runtime carrying RuntimeId(1). It is a stack
        // local that outlives every guard/stamp derived from it below: the
        // actor's `runtime` field points at it and `enter_actor_runtime` borrows
        // it, and both are dropped before `rt_b` leaves scope.
        let rt_b = crate::runtime::RuntimeInner::new_with_id_for_test(
            crate::scheduler::worker_less_scheduler(),
            crate::runtime_id::RuntimeId(1),
        );

        // Build a test actor and re-stamp it as if spawned by rt_b: both the
        // `runtime` pointer and `runtime_id` come from the SAME runtime (the
        // spawn invariant). `Runnable` state so a successful send does not also
        // push it onto a scheduler queue — the mailbox enqueue is what (b)
        // asserts.
        let (actor, mailbox) = make_stop_test_actor_with_id(0xB2, HewActorState::Runnable);
        // SAFETY: the test exclusively owns `actor` and never publishes it.
        unsafe {
            (*actor).runtime_id = crate::runtime_id::RuntimeId(1);
            (*actor).runtime = &raw const rt_b;
        }

        // (c) Skew guard: the owning-runtime stamp's id equals the runtime_id.
        // SAFETY: the test owns `actor` and `rt_b`; the stamp is non-null here.
        unsafe {
            assert!(
                (*actor).runtime.is_null()
                    || (*(*actor).runtime).runtime_id() == (*actor).runtime_id,
                "a spawned actor must carry an owning-runtime stamp whose id equals its runtime_id"
            );
        }

        // (a) WITHOUT enter_actor_runtime: the calling thread is bound to the
        // default runtime, so the foreign actor fails closed and enqueues
        // nothing.
        // SAFETY: `actor` is valid and fully owned by this test; null payload.
        let foreign_rc = unsafe { hew_actor_try_send(actor, 1, ptr::null_mut(), 0) };
        assert_eq!(
            foreign_rc,
            HewError::ErrForeignRuntime as i32,
            "without entering the owner, an off-dispatch send is foreign and fails closed"
        );
        // SAFETY: `mailbox` is owned by the test.
        let refused_count = unsafe { mailbox::hew_mailbox_has_messages(mailbox) };
        assert_eq!(
            refused_count, 0,
            "a refused cross-runtime send must not enqueue a message"
        );

        // (b) WITH enter_actor_runtime: the choke point binds rt_b, observed via
        // rt_current_id; the same send is now in-runtime and reaches the mailbox.
        {
            // SAFETY: `actor` is live and owns a non-null `runtime` stamp to
            // `rt_b`, which outlives this guard.
            let _bind = unsafe { crate::runtime::enter_actor_runtime(actor) }
                .expect("entering the owning runtime yields a guard");
            assert_eq!(
                crate::runtime::rt_current_id(),
                Some(crate::runtime_id::RuntimeId(1)),
                "enter_actor_runtime must bind the actor's owning runtime, not the default"
            );

            // SAFETY: as above.
            let bound_rc = unsafe { hew_actor_try_send(actor, 1, ptr::null_mut(), 0) };
            assert_eq!(
                bound_rc,
                HewError::Ok as i32,
                "with the owner bound, the off-dispatch send is in-runtime and succeeds"
            );
            // SAFETY: `mailbox` is owned by the test.
            let accepted_count = unsafe { mailbox::hew_mailbox_has_messages(mailbox) };
            assert_eq!(
                accepted_count, 1,
                "the accepted send must have enqueued exactly the one message"
            );
        }

        // The guard restored the previous (default) binding on drop.
        assert_eq!(
            crate::runtime::rt_current_id(),
            Some(crate::runtime_id::RuntimeId::DEFAULT),
            "dropping the enter_actor_runtime guard restores the previous (default) binding"
        );

        // SAFETY: the test fully owns the actor and its mailbox; drop the actor
        // before `rt_b` leaves scope so its `runtime` stamp is never read after.
        unsafe {
            drop(Box::from_raw(actor));
            mailbox::hew_mailbox_free(mailbox);
        }
    }

    /// V2(d): `enter_actor_runtime` TRAPS — it does not silently default and does
    /// not return `None` — when an actor carries a non-default `runtime_id` but a
    /// NULL owning-runtime stamp. That pairing is a spawn-invariant contradiction
    /// (spawn stamps `runtime` and `runtime_id` from the same runtime), so
    /// silently binding the default would re-open the silent-default hazard the
    /// stamp closes (`no-fail-open-fallback-after-authority`). The DEFAULT path
    /// is unaffected: a default actor with a null stamp resolves the default
    /// fallback exactly as in M2.
    #[test]
    fn enter_actor_runtime_traps_on_multi_runtime_null_stamp() {
        let _guard = crate::runtime_test_guard();

        // Control (the legitimate M2 path): a DEFAULT-stamped actor with a null
        // `runtime` resolves the default fallback — NO trap, returns Some.
        let (default_actor, default_mailbox) =
            make_stop_test_actor_with_id(0xD0, HewActorState::Idle);
        // SAFETY: the test owns `default_actor`; it carries runtime_id DEFAULT and
        // a null runtime stamp (the helper defaults).
        let default_bound = unsafe { crate::runtime::enter_actor_runtime(default_actor) };
        assert!(
            default_bound.is_some(),
            "a DEFAULT actor with a null stamp must resolve the default fallback (no trap)"
        );
        drop(default_bound);

        // The trap: a non-default runtime_id with a null stamp must panic rather
        // than silently default.
        let (foreign_actor, foreign_mailbox) =
            make_stop_test_actor_with_id(0xD1, HewActorState::Idle);
        // SAFETY: the test owns `foreign_actor`; stamp a non-default id but leave
        // `runtime` null — precisely the contradiction the trap exists to catch.
        unsafe {
            (*foreign_actor).runtime_id = crate::runtime_id::RuntimeId(1);
        }
        let trapped = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            // SAFETY: `foreign_actor` is live and owned by the test; the trap
            // fires before any runtime is entered, so no guard leaks.
            let _ = unsafe { crate::runtime::enter_actor_runtime(foreign_actor) };
        }));
        assert!(
            trapped.is_err(),
            "a non-default runtime_id with a null owning-runtime stamp must TRAP, not default"
        );

        // SAFETY: the test fully owns both actors and mailboxes.
        unsafe {
            drop(Box::from_raw(default_actor));
            mailbox::hew_mailbox_free(default_mailbox);
            drop(Box::from_raw(foreign_actor));
            mailbox::hew_mailbox_free(foreign_mailbox);
        }
    }

    #[test]
    fn actor_crash_cancels_current_task_scope() {
        let _guard = crate::runtime_test_guard();

        // SAFETY: null state + valid dispatch are valid spawn args.
        let actor = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
        assert!(!actor.is_null());

        // SAFETY: test owns the scope pointer and restores the context before teardown.
        unsafe {
            let _ctx = TestExecutionContext::install(HewExecutionContext::default());
            let scope = crate::task_scope::hew_task_scope_new();
            let previous = crate::task_scope::hew_task_scope_set_current(scope);

            hew_actor_trap(actor, 99);

            assert_eq!(crate::task_scope::hew_task_scope_is_cancelled(scope), 1);
            let _ = crate::task_scope::hew_task_scope_set_current(previous);
            crate::task_scope::hew_task_scope_destroy(scope);
            assert_eq!(hew_actor_free(actor), 0);
        }
    }

    #[cfg_attr(
        not(unix),
        allow(
            dead_code,
            reason = "only consumed by the unix-gated free-during-reactor-detach race test"
        )
    )]
    static REACTOR_WAKE_HOOK_FIRED: AtomicBool = AtomicBool::new(false);

    /// Pre-detach hook that models a reactor delivery waking the actor during
    /// the `hew_actor_free` detach window. Runs after free observed the actor
    /// quiescent (`Idle`) but before `reactor_detach_actor`, and performs the
    /// exact wake side-effect a real delivery's `hew_actor_try_send` does:
    /// `CAS Idle->Runnable` + `sched_enqueue`. This is the side effect detach
    /// does not undo and that the buggy free path freed under. Self-contained
    /// (uses only the `actor` argument) so it can be a plain `fn` pointer, and
    /// it deliberately does NOT touch the process-global `DELIVERING_ACTOR`
    /// guard, so it needs no cross-test serialization with the reactor tests.
    #[cfg_attr(
        not(unix),
        allow(
            dead_code,
            reason = "only consumed by the unix-gated free-during-reactor-detach race test"
        )
    )]
    fn reactor_wake_during_detach_hook(actor: *mut HewActor) {
        // SAFETY: the free path holds the actor live across the hook; it is the
        // same pointer free is about to detach.
        let a = unsafe { &*actor };
        // The wake side-effect: a reactor `on_data` delivery's
        // `hew_actor_try_send` CASes Idle->Runnable and enqueues the actor.
        if a.actor_state
            .compare_exchange(
                HewActorState::Idle as i32,
                HewActorState::Runnable as i32,
                Ordering::AcqRel,
                Ordering::Acquire,
            )
            .is_ok()
        {
            scheduler::sched_enqueue(actor);
        }
        REACTOR_WAKE_HOOK_FIRED.store(true, Ordering::Release);
    }

    /// A reactor delivery that wakes + enqueues an actor *during* the
    /// `hew_actor_free` reactor-detach window must never let that actor be
    /// freed while a live pointer to it remains in a scheduler queue
    /// (use-after-free in `activate_actor`).
    ///
    /// Forced ordering (deterministic, no timing luck): a worker-less scheduler
    /// guarantees nothing drains the queue, and the pre-detach hook performs the
    /// wake inline in the exact window between free's pre-detach quiescence read
    /// and `reactor_detach_actor`. So every run reproduces the race. The hook
    /// performs only the actor-local wake (no global `DELIVERING_ACTOR` write),
    /// so the test is self-contained and does not race the reactor tests.
    ///
    /// With the producer-side post-detach re-check, free observes the actor is
    /// `Runnable` (woken during detach) and refuses to free it: it returns -2
    /// ("still running") and the actor stays tracked + queued + intact.
    ///
    /// WITHOUT the fix (free using only the pre-detach quiescence read), free
    /// would untrack + free the actor and return 0, leaving a dangling pointer
    /// in the global queue — the bug. The assertions below (`rc == -2`, actor
    /// still live in `LIVE_ACTORS`, still Runnable, identity intact, pointer still
    /// queued) all flip in that case: `rc` would be 0 and the queued pointer
    /// would reference freed memory (a genuine UAF when later activated, caught
    /// under a sanitizer). Verified: reverting the producer-side re-check makes
    /// this test fail at the `rc == -2` assertion with the observed `rc == 0`.
    #[test]
    #[cfg(unix)]
    fn free_refuses_actor_woken_by_reactor_during_detach() {
        let _guard = crate::runtime_test_guard();
        // Worker-less scheduler: sched_enqueue works, nothing drains the queue.
        // The guard holds SCHED_TEST_MUTEX, serializing against scheduler tests.
        let sched = scheduler::NoWorkerSchedulerForTest::install();
        // Also hold the tracing lock (consistent lock order: SCHED then tracing):
        // this test's `hew_actor_close`/free emits SPAN_STOP lifecycle events into
        // the process-global trace ring whenever tracing is enabled, which would
        // otherwise race a concurrent tracing/span test's ring assertions.
        let _tracing = crate::tracing::tracing_test_guard();

        // SAFETY: null state + valid dispatch are valid spawn args.
        let actor = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
        assert!(!actor.is_null());
        // SAFETY: actor is valid and owned by this test.
        let actor_id = unsafe { (*actor).id };

        // Freshly spawned actors are Idle (quiescent) — free's pre-detach check
        // will pass, then the hook wakes the actor during detach.
        // SAFETY: actor is valid (just spawned, owned by this test).
        let spawned_state = unsafe { (*actor).actor_state.load(Ordering::Acquire) };
        assert_eq!(spawned_state, HewActorState::Idle as i32);

        REACTOR_WAKE_HOOK_FIRED.store(false, Ordering::Release);
        set_free_pre_detach_hook_for_test(Some(reactor_wake_during_detach_hook));

        // SAFETY: actor is valid; free is the operation under test.
        let rc = unsafe { hew_actor_free(actor) };

        // Always clear the hook so it cannot affect teardown or other tests.
        set_free_pre_detach_hook_for_test(None);

        assert!(
            REACTOR_WAKE_HOOK_FIRED.load(Ordering::Acquire),
            "pre-detach hook must have fired — the test did not exercise the race"
        );
        assert_eq!(
            rc, -2,
            "hew_actor_free must REFUSE to free an actor woken+enqueued during \
             reactor detach (got {rc}; rc==0 means the queued actor was freed — UAF)"
        );
        // The actor must still be tracked and intact (not freed).
        assert!(
            live_actors::is_actor_live(actor),
            "refused-free actor must remain tracked in LIVE_ACTORS"
        );
        // SAFETY: assertion above proves the actor is still live (not freed).
        let queued_state = unsafe { (*actor).actor_state.load(Ordering::Acquire) };
        assert_eq!(
            queued_state,
            HewActorState::Runnable as i32,
            "refused-free actor must remain Runnable (woken by the delivery)"
        );
        // SAFETY: actor still live; reading its stable id is sound.
        assert_eq!(unsafe { (*actor).id }, actor_id, "actor identity intact");

        // The wake left a live pointer in the global queue. It must be the
        // (still-valid) actor — not a dangling pointer to freed memory.
        let queued = sched.pop_global();
        assert_eq!(
            queued,
            Some(actor),
            "the woken actor's pointer must still be queued and valid"
        );

        // Teardown: return the actor to Idle so the final free succeeds. The
        // mailbox is empty, so a real activation would simply CAS Runnable->Idle;
        // do that directly here to avoid emitting tracing span events into the
        // process-global trace ring (which a concurrent tracing test asserts on).
        // SAFETY: actor is still live; this test exclusively owns it now.
        unsafe {
            (*actor)
                .actor_state
                .store(HewActorState::Idle as i32, Ordering::Release);
        }
        // Drain the stale pointer the wake left in the global queue before the
        // box is freed, so nothing dequeues it after free.
        assert_eq!(
            sched.pop_global(),
            None,
            "the single queued pointer was already consumed above"
        );
        // SAFETY: actor is valid and back to Idle.
        unsafe {
            hew_actor_close(actor);
            assert_eq!(hew_actor_free(actor), 0);
        }
        drop(sched);
    }

    static POST_LATCH_WAKE_HOOK_FIRED: AtomicBool = AtomicBool::new(false);
    static POST_LATCH_WAKE_SUCCEEDED: AtomicBool = AtomicBool::new(false);

    /// Post-latch hook that models a non-reactor wake — the exact link/monitor
    /// exit/down propagation side effect (`send_exit_signal` /
    /// `send_down_notification`) — firing in the window between free latching the
    /// actor out of `Idle` and `untrack_actor`. It routes through the *real*
    /// `with_live_actor_by_id` guard (holding the `LIVE_ACTORS` lock, exactly as
    /// the production link/monitor paths do) and performs the producer-side
    /// `CAS Idle->Runnable` + `sched_enqueue`. Self-contained (uses only the
    /// `actor` argument), so it can be a plain `fn` pointer. Whether that CAS
    /// succeeds is the load-bearing observation:
    ///   - WITH the latch: free has already CAS'd the actor to `Stopped`, so this
    ///     CAS fails — no enqueue, no queued-after-free, no UAF.
    ///   - WITHOUT the latch (free breaking on the bare post-detach `Idle`): the
    ///     actor is still `Idle`, this CAS succeeds and enqueues a pointer that
    ///     free then untracks + frees → dangling queue entry (the verdict's UAF).
    fn nonreactor_wake_post_latch_hook(actor: *mut HewActor) {
        // SAFETY: free holds the actor live across this hook; it is the same
        // pointer free is about to untrack.
        let id = unsafe { (*actor).id };
        let woke = with_live_actor_by_id(id, actor, |a_ref| {
            if a_ref
                .actor_state
                .compare_exchange(
                    HewActorState::Idle as i32,
                    HewActorState::Runnable as i32,
                    Ordering::AcqRel,
                    Ordering::Acquire,
                )
                .is_ok()
            {
                scheduler::sched_enqueue(actor);
                true
            } else {
                false
            }
        });
        if woke == Some(true) {
            POST_LATCH_WAKE_SUCCEEDED.store(true, Ordering::Release);
        }
        POST_LATCH_WAKE_HOOK_FIRED.store(true, Ordering::Release);
    }

    /// A non-reactor wake — in-flight link/monitor exit/down propagation (or a
    /// direct actor-to-actor send) for a crashing peer — must never enqueue an
    /// actor that `hew_actor_free` is about to untrack + free. This is the defect
    /// the independent review reproduced and BLOCKED on: the reactor fix
    /// closed only the reactor wake; a non-reactor waker could still
    /// `CAS Idle->Runnable` + `sched_enqueue` in the window between free's
    /// post-detach `Idle` observation and `untrack_actor`, after which free freed
    /// a still-queued actor → UAF in `activate_actor`.
    ///
    /// Forced ordering (deterministic, no timing luck): a worker-less scheduler
    /// guarantees nothing drains the queue, and the post-latch hook performs the
    /// real link/monitor wake inline in the exact window between free latching the
    /// actor out of `Idle` and `untrack_actor`. The wake routes through the same
    /// `with_live_actor_by_id` guard the production link/monitor paths use, so the
    /// test exercises the production reachability, not a synthetic shortcut.
    ///
    /// WITH the producer-side Idle->Stopped latch: by the time the hook runs the
    /// actor is `Stopped`, so the waker's `CAS Idle->Runnable` FAILS — nothing is
    /// enqueued, free completes cleanly (`rc == 0`), and the global queue is empty
    /// (no queued-after-free). The assertions below encode exactly that.
    ///
    /// WITHOUT the latch (revert step 3 to break on the bare post-detach `Idle`):
    /// the hook's CAS succeeds, `sched_enqueue` leaves a live pointer in the
    /// queue, and free untracks + frees it → `POST_LATCH_WAKE_SUCCEEDED == true`
    /// and a dangling pointer is observable via `sched.pop_global()` after the box
    /// is freed (the UAF; would trip ASAN on a later `activate_actor`). Verified:
    /// reverting the latch flips this test to fail at the
    /// `!POST_LATCH_WAKE_SUCCEEDED` assertion (observed `rc=0 queued_after_free=true`).
    #[test]
    fn free_latches_actor_against_nonreactor_wake_before_untrack() {
        let _guard = crate::runtime_test_guard();
        // Worker-less scheduler: sched_enqueue works, nothing drains the queue, so
        // any wake-enqueued pointer stays observable. The guard holds
        // SCHED_TEST_MUTEX, serializing against scheduler tests.
        let sched = scheduler::NoWorkerSchedulerForTest::install();
        // Hold the tracing lock too (consistent lock order: SCHED then tracing):
        // free's terminate/finalize path emits lifecycle events into the
        // process-global trace ring when tracing is enabled, which would otherwise
        // race a concurrent tracing/span test's ring assertions.
        let _tracing = crate::tracing::tracing_test_guard();

        // SAFETY: null state + valid dispatch are valid spawn args.
        let actor = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
        assert!(!actor.is_null());

        // Freshly spawned actors are Idle (quiescent) — free's wait + post-detach
        // reload both observe Idle, then free latches Idle->Stopped before the
        // post-latch hook fires the non-reactor wake.
        // SAFETY: actor is valid (just spawned, owned by this test).
        let spawned_state = unsafe { (*actor).actor_state.load(Ordering::Acquire) };
        assert_eq!(spawned_state, HewActorState::Idle as i32);

        POST_LATCH_WAKE_HOOK_FIRED.store(false, Ordering::Release);
        POST_LATCH_WAKE_SUCCEEDED.store(false, Ordering::Release);
        set_free_post_latch_hook_for_test(Some(nonreactor_wake_post_latch_hook));

        // SAFETY: actor is valid; free is the operation under test.
        let rc = unsafe { hew_actor_free(actor) };

        // Always clear the hook so it cannot affect teardown or other tests.
        set_free_post_latch_hook_for_test(None);

        assert!(
            POST_LATCH_WAKE_HOOK_FIRED.load(Ordering::Acquire),
            "post-latch hook must have fired — the test did not exercise the window"
        );
        // The load-bearing assertion: the non-reactor waker's CAS Idle->Runnable
        // must FAIL because free latched the actor to Stopped first. If it
        // succeeds, the producer-side latch did not close the window (the UAF).
        assert!(
            !POST_LATCH_WAKE_SUCCEEDED.load(Ordering::Acquire),
            "a non-reactor wake CAS'd Idle->Runnable in the free window — free \
             latched the actor too late (this is the use-after-free the latch must close)"
        );
        // With the wake blocked, free completes cleanly.
        assert_eq!(
            rc, 0,
            "hew_actor_free must succeed once the actor is wake-proof (got {rc})"
        );
        // The actor is freed and no longer tracked.
        assert!(
            !live_actors::is_actor_live(actor),
            "freed actor must no longer be tracked in LIVE_ACTORS"
        );
        // No pointer was left in the global queue — nothing dangles after free.
        let queued_after_free = sched.pop_global();
        assert_eq!(
            queued_after_free, None,
            "no actor pointer may remain queued after free (a queued pointer here \
             would dangle — the use-after-free)"
        );
        drop(sched);
    }

    // ── Forced-ordering test: free must drain send pins before finalizing ──

    /// Set to `true` by the background thread spawned by
    /// `post_latch_inject_send_pin` BEFORE it decrements `send_pin_count`.
    /// On the fixed code the freer spins until the pin drops, so the bg
    /// thread runs while the freer is blocked and `SEND_PIN_DRAIN_WAITED` is
    /// `true` when the freer proceeds to finalize.  Without the post-untrack
    /// pin-drain loop the freer proceeds to finalize immediately (no spin); the
    /// bg thread is still sleeping so `SEND_PIN_DRAIN_WAITED` is `false` when
    /// `hew_actor_free` returns → assertion fails → test fails.
    static SEND_PIN_DRAIN_WAITED: AtomicBool = AtomicBool::new(false);

    /// Set to `true` by the test thread immediately after `hew_actor_free`
    /// returns.  The background thread checks this flag before decrementing
    /// the pin: on the old (unfixed) code, free returned while the bg thread
    /// was still sleeping; setting CANCEL prevents the bg thread from
    /// performing a use-after-free write to the freed actor box.
    static SEND_PIN_TEST_CANCEL: AtomicBool = AtomicBool::new(false);

    /// Post-latch hook for `free_waits_for_send_pin_drain_before_finalize`.
    ///
    /// Simulates a concurrent by-ID sender that managed to pin the actor
    /// after the quiescence check but before `untrack_actor`.  Increments
    /// `send_pin_count` directly (as `with_actor_send_by_id` would) and
    /// spawns a background thread that will release the pin after a delay
    /// long enough to distinguish "free waited" from "free raced ahead".
    fn post_latch_inject_send_pin(actor: *mut HewActor) {
        // Simulate pin-increment (the step with_actor_send_by_id performs
        // under LIVE_ACTORS before releasing the lock).
        // SAFETY: the actor is still live; free holds it across this hook.
        unsafe { (*actor).send_pin_count.fetch_add(1, Ordering::AcqRel) };

        // Cast to usize so the closure captures a Send-safe integer.
        // (RFC 2229 field-level capture would capture `ap.0: *mut HewActor`
        // — not Send — if we used a newtype wrapper with a field access.)
        let actor_addr = actor as usize;
        std::thread::spawn(move || {
            // Sleep long enough that an unblocked freer returns before we run.
            std::thread::sleep(std::time::Duration::from_millis(60));

            // Check the cancel flag set by the test thread after free returns.
            // Without the post-untrack pin-drain spin, free returns immediately
            // and CANCEL is set before we wake — we must NOT do the fetch_sub on
            // the now-freed box.
            if SEND_PIN_TEST_CANCEL.load(Ordering::Acquire) {
                return;
            }

            // Record that we ran before decrementing — the freer must observe
            // this flag as `true` when it proceeds past the pin drain loop.
            SEND_PIN_DRAIN_WAITED.store(true, Ordering::Release);

            // Release the pin.  The freer's Acquire load of send_pin_count
            // pairs with this Release, so it sees all writes we made above.
            let actor_ptr = actor_addr as *mut HewActor;
            // SAFETY: CANCEL was false → free is still spinning (pin drain loop),
            // so the actor box is still live and the atomic write is valid.
            unsafe { (*actor_ptr).send_pin_count.fetch_sub(1, Ordering::Release) };
        });
    }

    /// Verify that `hew_actor_free` drains all send pins **before** finalizing
    /// (calling terminate + freeing the box), not before the quiescence check.
    ///
    /// **Why this test catches the post-untrack pin-drain TOCTOU bug:**
    ///
    /// The pre-drain form checked `send_pin_count == 0` inside the quiescence
    /// *wait loop* (before the latch), not after `untrack_actor`.  The
    /// post-latch hook fires AFTER the latch succeeds but BEFORE `untrack_actor`.
    /// In that window, `with_actor_send_by_id` can still find the actor in the
    /// map and increment the pin — the quiescence check that passed was
    /// already stale.  That form then proceeded to `untrack_actor` +
    /// `finalize` without waiting → use-after-free.
    ///
    /// **Pre-drain form (FAIL):** the hook increments `send_pin_count`; free
    /// has no pin-drain loop after `untrack_actor` → proceeds to finalize
    /// immediately → returns in < 5 ms.  The bg thread wakes at 60 ms, finds
    /// `CANCEL == true` (set just after free returned), skips the `fetch_sub`
    /// (no UAF).  `SEND_PIN_DRAIN_WAITED` is `false` → assertion fails.
    ///
    /// **On fixed code (PASS):** free calls `untrack_actor`, then spins on
    /// `send_pin_count`.  The bg thread wakes at 60 ms, sees `CANCEL ==
    /// false` (free still spinning), stores `SEND_PIN_DRAIN_WAITED = true`,
    /// decrements pin.  Free sees pin == 0, finalize, return.  Assertion
    /// passes.
    #[test]
    fn free_waits_for_send_pin_drain_before_finalize() {
        let _guard = crate::runtime_test_guard();
        // Worker-less scheduler: same setup as the latch / reactor tests.
        let sched = scheduler::NoWorkerSchedulerForTest::install();
        let _tracing = crate::tracing::tracing_test_guard();

        // SAFETY: null state + valid dispatch are valid spawn args.
        let actor = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
        assert!(!actor.is_null());

        // Freshly spawned actors are Idle — the quiescence wait passes
        // immediately so the post-latch hook fires before untrack.
        // SAFETY: actor is valid (just spawned); the state field is always initialized.
        let spawned_state = unsafe { (*actor).actor_state.load(Ordering::Acquire) };
        assert_eq!(spawned_state, HewActorState::Idle as i32);

        SEND_PIN_DRAIN_WAITED.store(false, Ordering::Release);
        SEND_PIN_TEST_CANCEL.store(false, Ordering::Release);
        set_free_post_latch_hook_for_test(Some(post_latch_inject_send_pin));

        // SAFETY: actor is valid; free is the operation under test.
        let rc = unsafe { hew_actor_free(actor) };

        set_free_post_latch_hook_for_test(None);
        // Signal the bg thread: if free returned before the bg thread ran
        // (the pre-drain regression path), the bg thread must not touch the
        // freed box.
        SEND_PIN_TEST_CANCEL.store(true, Ordering::Release);

        // Fixed code: free spun until the bg thread decremented send_pin_count;
        // the bg thread stored WAITED = true before decrementing, so when free
        // proceeded to finalize it observed WAITED = true, meaning all pins
        // were drained before finalize ran.
        //
        // Pre-drain form: free returned before the bg thread ran → WAITED = false.
        assert!(
            SEND_PIN_DRAIN_WAITED.load(Ordering::Acquire),
            "hew_actor_free must drain all send pins before finalizing the actor \
             box; if this assertion fails, finalize ran while a send pin was held \
             (use-after-free window)"
        );
        assert_eq!(rc, 0, "hew_actor_free must succeed (got {rc})");
        assert!(
            !live_actors::is_actor_live(actor),
            "freed actor must no longer be tracked in LIVE_ACTORS"
        );
        drop(sched);
    }

    /// Forced-ordering regression for the `cleanup_all_actors` re-enqueue UAF
    /// in the post-prepare latch window.
    ///
    /// A pinned by-ID sender that incremented `send_pin_count` before
    /// `drain_all_for_cleanup` removed the map entry can still be running its
    /// send closure, which may CAS `Idle→Runnable` to re-enqueue the actor into
    /// the scheduler.  Without the `Idle→Stopped` latch, `cleanup_all_actors`
    /// would call `finalize` on a still-queued actor — a UAF.
    ///
    /// This test simulates that race with `CLEANUP_POST_PREPARE_HOOK`: the hook
    /// fires after `prepare_quiescent_actor_for_cleanup` and before the latch,
    /// performs `CAS Idle→Runnable` (as a concurrent sender would), and then
    /// asserts that the latch-fail path skips finalize (the actor is leaked) and
    /// does NOT free the allocation.
    ///
    /// **Before the fix**: `cleanup_all_actors` proceeded to finalize regardless
    /// of state → the hook would observe state=Runnable post-finalize (freed
    /// memory read → UB), and in practice the scheduler would later dereference
    /// the queued dangling pointer.
    ///
    /// **After the fix**: `CAS Idle→Stopped` fails (state is already Runnable)
    /// → the actor is logged + leaked.  Allocation still valid post-call.
    static CLEANUP_REENQUEUE_CAS_SUCCEEDED: AtomicBool = AtomicBool::new(false);

    fn reenqueue_for_cleanup_hook(actor: *mut HewActor) {
        // SAFETY: actor is valid (from cleanup_all_actors iteration).
        let a = unsafe { &*actor };
        let ok = a
            .actor_state
            .compare_exchange(
                HewActorState::Idle as i32,
                HewActorState::Runnable as i32,
                Ordering::AcqRel,
                Ordering::Acquire,
            )
            .is_ok();
        CLEANUP_REENQUEUE_CAS_SUCCEEDED.store(ok, Ordering::Release);
    }

    #[test]
    fn cleanup_skips_actor_reenqueued_during_latch_window() {
        let _guard = crate::runtime_test_guard();
        let _scheduler = scheduler::NoWorkerSchedulerForTest::install();
        let _tracing = crate::tracing::tracing_test_guard();

        // SAFETY: null state + valid dispatch are valid spawn args.
        let actor = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
        assert!(!actor.is_null());

        // Freshly spawned actors are Idle — the hook will win the
        // CAS Idle→Runnable race before the latch can run.
        // SAFETY: actor is valid (just spawned).
        let spawned_state = unsafe { (*actor).actor_state.load(Ordering::Acquire) };
        assert_eq!(spawned_state, HewActorState::Idle as i32);

        CLEANUP_REENQUEUE_CAS_SUCCEEDED.store(false, Ordering::Release);

        set_cleanup_post_prepare_hook_for_test(Some(reenqueue_for_cleanup_hook));

        // SAFETY: scheduler is stopped (NoWorkerSchedulerForTest installed);
        // no dispatch is in progress.
        unsafe { cleanup_all_actors() };

        set_cleanup_post_prepare_hook_for_test(None);

        // 1. Hook must have fired and the CAS must have succeeded (state was Idle
        //    when the hook ran, before the latch attempt).
        assert!(
            CLEANUP_REENQUEUE_CAS_SUCCEEDED.load(Ordering::Acquire),
            "hook must fire and CAS Idle→Runnable must succeed in the latch window"
        );

        // 2. The latch-fail path must have SKIPPED finalize: the allocation is
        //    still valid and the state is Runnable (not freed/corrupted).
        // SAFETY: actor was not freed (latch-fail → continue; allocation is valid).
        let post_state = unsafe { (*actor).actor_state.load(Ordering::Acquire) };
        assert_eq!(
            post_state,
            HewActorState::Runnable as i32,
            "actor must be Runnable (leaked, not freed) after cleanup latch-fail"
        );

        // 3. The actor must be untracked — drain_all_for_cleanup removed it.
        assert!(
            !live_actors::is_actor_live(actor),
            "actor must be untracked even when cleanup skips finalize"
        );

        // Manual cleanup: cleanup_all_actors deliberately leaked this actor to
        // avoid UAF.  Finalize it here to avoid a test memory leak.  The actor
        // has no terminate_fn (noop_dispatch), so call_terminate_fn is a no-op.
        // SAFETY: actor is valid, untracked, and no concurrent access is possible.
        unsafe {
            (*actor)
                .actor_state
                .store(HewActorState::Stopped as i32, Ordering::Release);
            finalize_quiescent_actor_cleanup(actor, HewActorState::Stopped as i32);
        }
    }

    /// Deterministic free-vs-leak probe for the `cleanup_all_actors`
    /// *snapshot-already-non-Idle* re-enqueue UAF.
    ///
    /// Set by [`cleanup_runnable_leak_state_drop_callback`] when (and only when)
    /// `free_actor_resources` runs the codegen state-drop — i.e. when the actor
    /// was *finalized* (freed).  A leaked (skipped) actor never reaches finalize,
    /// so the counter stays 0.  Reading this global (not the actor box) keeps the
    /// assertion well-defined on BOTH the buggy path (box freed) and the fixed
    /// path (box leaked): no use-after-free read is needed to tell them apart.
    static CLEANUP_RUNNABLE_LEAK_STATE_DROP_COUNT: std::sync::atomic::AtomicUsize =
        std::sync::atomic::AtomicUsize::new(0);

    unsafe extern "C" fn cleanup_runnable_leak_state_drop_callback(_state: *mut c_void) {
        CLEANUP_RUNNABLE_LEAK_STATE_DROP_COUNT.fetch_add(1, Ordering::SeqCst);
    }

    /// Forced-ordering regression for the `cleanup_all_actors`
    /// *snapshot-already-non-Idle* re-enqueue UAF.
    ///
    /// Companion to `cleanup_skips_actor_reenqueued_during_latch_window`, which
    /// covers the window where the wake CAS lands AFTER cleanup loads the state.
    /// This test covers the window a stale-snapshot decision MISSES: a pinned
    /// by-ID sender wins `CAS Idle→Runnable` BEFORE cleanup reaches the actor, so
    /// the actor is already `Runnable` (re-enqueued) when the finalize decision
    /// runs.
    ///
    /// We reproduce that window deterministically and end-to-end: store
    /// `Runnable` AND `sched_enqueue` the actor into the scheduler's global
    /// queue BEFORE `cleanup_all_actors` runs — exactly the end-state of a
    /// pinned by-ID sender that won `CAS Idle→Runnable` + `sched_enqueue` before
    /// the sweep reached the actor. `drain_all_for_cleanup` untracks it, and the
    /// per-actor finalize decision then observes `Runnable`. After cleanup we
    /// DRAIN the queue (`pop_global`) and deref the popped pointer — the
    /// "anything ever drains `global_queue` post-cleanup" scenario, where a
    /// later consumer pops the queue — proving the leaked pointer still resolves
    /// to a live actor (no UAF). On the buggy path that deref would hit freed
    /// memory, but the counter assertion fails first, so the deref only runs once
    /// the fix has proven the box was leaked, not freed.
    ///
    /// **Under a snapshot-gated decision (FAIL):** cleanup loads `state =
    /// Runnable`, the `if state == Idle` short-circuits (no latch CAS, no
    /// fail-closed `continue`), and `finalize_quiescent_actor_cleanup(actor,
    /// Runnable)` frees the re-enqueued actor — state-drop runs → counter == 1 →
    /// assertion fails. (A scheduler queue holding the now-dangling pointer is
    /// the UAF.)
    ///
    /// **Under the CAS-result decision (PASS):** the finalize decision attempts
    /// `CAS Idle→Stopped`, observes `Err(Runnable)`, and SKIPs (leaks
    /// fail-closed) — finalize never runs → counter == 0 → assertion passes.
    /// The leaked actor is reclaimed manually at the end so the test does not
    /// leak.
    #[test]
    fn cleanup_skips_actor_already_runnable_before_finalize_decision() {
        let _guard = crate::runtime_test_guard();
        let scheduler = scheduler::NoWorkerSchedulerForTest::install();
        let _tracing = crate::tracing::tracing_test_guard();

        CLEANUP_RUNNABLE_LEAK_STATE_DROP_COUNT.store(0, Ordering::SeqCst);

        // Spawn with a malloc'd source so `state` is non-null: the state-drop
        // callback only fires when finalize runs over a non-null, non-crashed
        // state — that is the "was freed" signal.
        // SAFETY: malloc returns a valid 8-byte allocation or null.
        let src = unsafe { libc::malloc(8) };
        assert!(!src.is_null());
        // SAFETY: spawn deep-copies the bytes; src is released immediately after.
        let actor = unsafe { hew_actor_spawn(src, 8, Some(noop_dispatch)) };
        assert!(!actor.is_null());
        // SAFETY: spawn copied the bytes; release the source allocation.
        unsafe { libc::free(src) };

        // SAFETY: actor is valid and not being dispatched.
        unsafe {
            hew_actor_set_state_drop(actor, cleanup_runnable_leak_state_drop_callback);
            assert!(
                !(*actor).state.is_null(),
                "spawn must produce a non-null state for the state-drop signal"
            );
            // Simulate the end-state of a pinned by-ID sender that already won
            // `CAS Idle→Runnable` (+ `sched_enqueue`) BEFORE the sweep loop
            // observes this actor — the exact snapshot-already-non-Idle window.
            (*actor)
                .actor_state
                .store(HewActorState::Runnable as i32, Ordering::Release);
        }

        // Genuinely enqueue the actor, so this is "Runnable AND queued" — the
        // real shape of a won wake CAS, not just a state store. `sched_enqueue`
        // pushes the raw pointer into the global queue and notifies a parker
        // (no worker exists to deref it under NoWorkerSchedulerForTest).
        scheduler::sched_enqueue(actor);

        // SAFETY: scheduler is stopped (NoWorkerSchedulerForTest installed); no
        // dispatch is in progress.
        unsafe { cleanup_all_actors() };

        // Load-bearing assertion: a `Runnable` (re-enqueued) actor must be
        // SKIPPED, not finalized.  state-drop running means finalize ran means
        // the queued actor was freed — the use-after-free.  Reads only the
        // global counter, never the (possibly-freed) actor box.
        assert_eq!(
            CLEANUP_RUNNABLE_LEAK_STATE_DROP_COUNT.load(Ordering::SeqCst),
            0,
            "cleanup_all_actors must SKIP (leak) an actor that is Runnable at the \
             finalize decision; a non-zero count means it ran finalize over a \
             re-enqueued actor (the snapshot-already-Runnable use-after-free)"
        );

        // Drain the scheduler queue exactly as a post-cleanup consumer would.
        // The pointer cleanup left enqueued must still be VALID. On the buggy
        // (snapshot-gated) path cleanup freed this box, so this pop would return
        // a dangling pointer and the deref below would read freed memory — but
        // the counter assertion above already failed before we reach here, so
        // the deref only ever runs on the fixed (leaked-not-freed) path. This is
        // the "anything ever drains global_queue post-cleanup" scenario, made
        // observable.
        let popped = scheduler.pop_global();
        assert_eq!(
            popped,
            Some(actor),
            "the enqueued actor must still be in the scheduler queue (cleanup \
             leaked it rather than freeing a queued pointer)"
        );
        // SAFETY: reached only because the counter assertion passed, i.e. cleanup
        // leaked (did not free) the actor — the queued pointer is still valid.
        let queued_state = unsafe { (*actor).actor_state.load(Ordering::Acquire) };
        assert_eq!(
            queued_state,
            HewActorState::Runnable as i32,
            "the queued pointer must still resolve to the live Runnable actor (no UAF)"
        );

        // The actor must be untracked (drain_all_for_cleanup removed it) even
        // though cleanup skipped finalize.  Pointer-identity probe; no deref.
        assert!(
            !live_actors::is_actor_live(actor),
            "actor must be untracked even when cleanup skips finalize"
        );

        // Manual reclaim: cleanup deliberately leaked this actor to avoid the
        // UAF.  Finalize it here so the test itself does not leak.
        // SAFETY: actor is valid, untracked, pin-free, no concurrent access.
        unsafe {
            (*actor)
                .actor_state
                .store(HewActorState::Stopped as i32, Ordering::Release);
            finalize_quiescent_actor_cleanup(actor, HewActorState::Stopped as i32);
        }
        assert_eq!(
            CLEANUP_RUNNABLE_LEAK_STATE_DROP_COUNT.load(Ordering::SeqCst),
            1,
            "manual reclaim must run state-drop exactly once (no leak)"
        );
    }

    /// Deterministic free-vs-leak probe for the bonus `cleanup_all_actors`
    /// *Suspended*-finalize fix.  Set by
    /// [`cleanup_suspended_leak_state_drop_callback`] only when finalize runs
    /// over the actor (i.e. it was freed); a leaked (skipped) actor never reaches
    /// finalize, so the counter stays 0.
    static CLEANUP_SUSPENDED_LEAK_STATE_DROP_COUNT: std::sync::atomic::AtomicUsize =
        std::sync::atomic::AtomicUsize::new(0);

    unsafe extern "C" fn cleanup_suspended_leak_state_drop_callback(_state: *mut c_void) {
        CLEANUP_SUSPENDED_LEAK_STATE_DROP_COUNT.fetch_add(1, Ordering::SeqCst);
    }

    /// Forced-ordering regression for the bonus `cleanup_all_actors`
    /// *Suspended*-finalize leak that the quiescence gate also closes.
    ///
    /// A `Suspended` actor is parked at a non-final `coro.suspend` with a live
    /// continuation frame (`suspended_cont`).  `actor_free_state_is_quiescent`
    /// excludes `Suspended`, so it is never safe to finalize on the shutdown
    /// sweep: `hew_actor_free_inner` destroys the parked frame first, but
    /// `cleanup_all_actors` (workers joined) cannot block to do so — it must
    /// leak-not-free.
    ///
    /// **Under a snapshot-gated decision (FAIL):** cleanup loads `state =
    /// Suspended`, the `if state == Idle` short-circuits (no latch, no
    /// fail-closed `continue`), and `finalize_quiescent_actor_cleanup(actor,
    /// Suspended)` frees the parked actor — running its state-drop and leaking
    /// the continuation frame → counter == 1 → assertion fails.
    ///
    /// **Under the quiescence gate (PASS):** the finalize decision attempts `CAS
    /// Idle→Stopped`, observes `Err(Suspended)`, and — because `Suspended` is
    /// not `actor_free_state_is_quiescent` — SKIPs (leaks fail-closed) → finalize
    /// never runs → counter == 0 → assertion passes.
    ///
    /// The finalize decision reads only `actor_state`, so storing `Suspended` is
    /// the faithful observable; no real parked frame is needed to exercise the
    /// gate.
    #[test]
    fn cleanup_skips_suspended_actor_at_finalize_decision() {
        let _guard = crate::runtime_test_guard();
        let _scheduler = scheduler::NoWorkerSchedulerForTest::install();
        let _tracing = crate::tracing::tracing_test_guard();

        CLEANUP_SUSPENDED_LEAK_STATE_DROP_COUNT.store(0, Ordering::SeqCst);

        // Non-null state so the state-drop callback is the "was finalized" signal.
        // SAFETY: malloc returns a valid 8-byte allocation or null.
        let src = unsafe { libc::malloc(8) };
        assert!(!src.is_null());
        // SAFETY: spawn deep-copies the bytes; src is released immediately after.
        let actor = unsafe { hew_actor_spawn(src, 8, Some(noop_dispatch)) };
        assert!(!actor.is_null());
        // SAFETY: spawn copied the bytes; release the source allocation.
        unsafe { libc::free(src) };

        // SAFETY: actor is valid and not being dispatched.
        unsafe {
            hew_actor_set_state_drop(actor, cleanup_suspended_leak_state_drop_callback);
            assert!(
                !(*actor).state.is_null(),
                "spawn must produce a non-null state for the state-drop signal"
            );
            // Park the actor at a suspend point (non-quiescent). The finalize
            // decision reads only `actor_state`, so this state store is the
            // faithful observable of a parked actor at shutdown.
            (*actor)
                .actor_state
                .store(HewActorState::Suspended as i32, Ordering::Release);
        }

        // SAFETY: scheduler is stopped (NoWorkerSchedulerForTest installed); no
        // dispatch is in progress.
        unsafe { cleanup_all_actors() };

        // A `Suspended` actor is non-quiescent: cleanup must SKIP (leak) it,
        // never finalize a parked actor.  counter != 0 means it freed one
        // (and leaked the continuation frame).  Reads only the global counter.
        assert_eq!(
            CLEANUP_SUSPENDED_LEAK_STATE_DROP_COUNT.load(Ordering::SeqCst),
            0,
            "cleanup_all_actors must SKIP (leak) a Suspended actor; a non-zero \
             count means it finalized a parked actor (freeing its box and leaking \
             the continuation frame)"
        );

        // Untracked even though cleanup skipped finalize. Pointer-identity; no deref.
        assert!(
            !live_actors::is_actor_live(actor),
            "actor must be untracked even when cleanup skips finalize"
        );

        // Manual reclaim so the test itself does not leak.  Drop to a terminal
        // state first (no real frame was parked, so no destroy_parked needed).
        // SAFETY: actor is valid, untracked, pin-free, no concurrent access.
        unsafe {
            (*actor)
                .actor_state
                .store(HewActorState::Stopped as i32, Ordering::Release);
            finalize_quiescent_actor_cleanup(actor, HewActorState::Stopped as i32);
        }
        assert_eq!(
            CLEANUP_SUSPENDED_LEAK_STATE_DROP_COUNT.load(Ordering::SeqCst),
            1,
            "manual reclaim must run state-drop exactly once (no leak)"
        );
    }

    #[test]
    fn ask_by_id_concurrent_with_sends_completes_without_leaking_channels() {
        let _guard = crate::runtime_test_guard();
        let runtime = NativeSchedulerGuard::new();

        assert_eq!(reply_channel::active_channel_count(), 0);
        ASK_SEND_BY_ID_DISPATCH_COUNT.store(0, Ordering::Release);

        // SAFETY: null state + valid dispatch are valid spawn args.
        let actor = unsafe {
            hew_actor_spawn(std::ptr::null_mut(), 0, Some(count_ask_send_by_id_dispatch))
        };
        assert!(!actor.is_null());

        // SAFETY: actor is live for the duration of the test.
        let actor_id = unsafe { (*actor).id };
        let ask_threads = 6usize;
        let send_threads = 6usize;
        let asks_per_thread = 12usize;
        let sends_per_thread = 12usize;
        let start = std::sync::Arc::new(std::sync::Barrier::new(ask_threads + send_threads));
        let mut handles = Vec::with_capacity(ask_threads + send_threads);

        for _ in 0..ask_threads {
            let start = start.clone();
            handles.push(std::thread::spawn(move || {
                start.wait();
                for _ in 0..asks_per_thread {
                    // SAFETY: actor remains live until all worker threads join.
                    let reply = unsafe { hew_actor_ask_by_id(actor_id, 1, ptr::null_mut(), 0) };
                    assert!(!reply.is_null(), "by-id ask should receive a reply");
                    // SAFETY: successful ask replies are malloc-allocated.
                    unsafe {
                        assert_eq!(*reply.cast::<i32>(), 7);
                        libc::free(reply);
                    }
                }
            }));
        }

        for _ in 0..send_threads {
            let start = start.clone();
            handles.push(std::thread::spawn(move || {
                start.wait();
                for _ in 0..sends_per_thread {
                    // SAFETY: actor remains live until all worker threads join.
                    let rc = unsafe {
                        hew_actor_send_by_id(actor_id, ptr::null(), 1, ptr::null_mut(), 0)
                    };
                    assert_eq!(rc, 0);
                }
            }));
        }

        for handle in handles {
            handle.join().expect("mixed ask/send thread must not panic");
        }

        let expected = (ask_threads * asks_per_thread) + (send_threads * sends_per_thread);
        assert!(
            wait_for_condition(std::time::Duration::from_secs(2), || {
                ASK_SEND_BY_ID_DISPATCH_COUNT.load(Ordering::Acquire) == expected
            }),
            "scheduler should drain mixed by-id ask/send traffic without deadlocking"
        );
        assert!(
            wait_for_condition(std::time::Duration::from_secs(1), || {
                reply_channel::active_channel_count() == 0
            }),
            "concurrent by-id asks should release all reply channels"
        );

        // SAFETY: actor remains live until teardown below.
        unsafe {
            hew_actor_close(actor);
            assert_eq!(hew_actor_free(actor), 0);
        }

        drop(runtime);
        assert_eq!(reply_channel::active_channel_count(), 0);
    }

    #[test]
    fn with_live_actor_by_id_requires_matching_id_and_pointer() {
        let _guard = crate::runtime_test_guard();

        // SAFETY: null state + valid dispatch are valid spawn args.
        let actor = unsafe { hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch)) };
        // SAFETY: null state + valid dispatch are valid spawn args.
        let other = unsafe { hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch)) };
        assert!(!actor.is_null());
        assert!(!other.is_null());

        // SAFETY: both actors remain live until teardown below.
        let actor_id = unsafe { (*actor).id };
        // SAFETY: `other` remains live until teardown below.
        let other_id = unsafe { (*other).id };

        assert_eq!(
            with_live_actor_by_id(actor_id, actor, |actor_ref| actor_ref.id),
            Some(actor_id)
        );
        assert_eq!(with_live_actor_by_id(other_id, actor, |_| ()), None);
        assert_eq!(with_live_actor_by_id(actor_id, other, |_| ()), None);

        // SAFETY: both actors are quiescent after close and fully owned by this test.
        unsafe {
            hew_actor_close(actor);
            assert_eq!(hew_actor_free(actor), 0);
            hew_actor_close(other);
            assert_eq!(hew_actor_free(other), 0);
        }
    }

    #[test]
    fn ask_with_channel_send_failure_returns_error() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Spawning with null state and a valid dispatch function.
        let actor = unsafe { hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch)) };
        assert!(!actor.is_null());

        // SAFETY: actor pointer is valid — returned by hew_actor_spawn above.
        unsafe {
            hew_actor_close(actor);
        }

        let ch = reply_channel::hew_reply_channel_new();
        // SAFETY: actor and ch are valid pointers from their respective constructors.
        let rc = unsafe { hew_actor_ask_with_channel(actor, 0, std::ptr::null_mut(), 0, ch) };
        assert_eq!(rc, HewError::ErrActorStopped as i32);

        // SAFETY: ch and actor are valid pointers; freeing resources after test.
        unsafe {
            reply_channel::hew_reply_channel_free(ch);
            assert_eq!(hew_actor_free(actor), 0);
        }
    }

    #[test]
    fn ask_with_channel_send_oom_marks_allocation_failed() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: null state + valid dispatch are valid spawn args.
        let actor = unsafe { hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch)) };
        assert!(!actor.is_null());

        let ch = reply_channel::hew_reply_channel_new();
        let _alloc_guard = crate::mailbox::fail_mailbox_alloc_on_nth(0);

        // SAFETY: actor and ch are valid pointers from their respective constructors.
        let rc = unsafe { hew_actor_ask_with_channel(actor, 0, std::ptr::null_mut(), 0, ch) };
        assert_eq!(rc, HewError::ErrOom as i32);
        // SAFETY: the failed send preserves the caller-owned ref so tests can
        // inspect the channel before releasing it.
        unsafe {
            assert!(reply_channel::hew_reply_channel_allocation_failed_for_test(
                ch
            ));
            reply_channel::hew_reply_channel_free(ch);
            hew_actor_stop(actor);
            assert_eq!(hew_actor_free(actor), 0);
        }
        assert_eq!(reply_channel::active_channel_count(), 0);
    }

    #[test]
    fn native_ask_self_stop_without_reply_returns_null_and_releases_channel() {
        let _guard = crate::runtime_test_guard();
        let runtime = NativeSchedulerGuard::new();

        assert_eq!(reply_channel::active_channel_count(), 0);
        LAST_NATIVE_ASK_REPLY_CHANNEL.store(ptr::null_mut(), Ordering::Release);

        // SAFETY: null state and dispatch function are valid for actor spawn.
        let actor = unsafe {
            hew_actor_spawn(
                std::ptr::null_mut(),
                0,
                Some(native_self_stop_without_reply_dispatch),
            )
        };
        assert!(!actor.is_null());

        let actor_addr = actor as usize;
        let (tx, rx) = std::sync::mpsc::channel();
        let ask_thread = std::thread::spawn(move || {
            let actor = actor_addr as *mut HewActor;
            // SAFETY: actor was spawned by this test and remains live until the thread joins.
            let reply = unsafe { hew_actor_ask(actor, 1, ptr::null_mut(), 0) };
            let reply_is_null = reply.is_null();
            if !reply.is_null() {
                // SAFETY: successful ask replies are malloc-allocated.
                unsafe { libc::free(reply) };
            }
            tx.send(reply_is_null)
                .expect("native ask waiter should report its result");
        });

        let reply_is_null = match rx.recv_timeout(std::time::Duration::from_secs(1)) {
            Ok(reply_is_null) => reply_is_null,
            Err(std::sync::mpsc::RecvTimeoutError::Timeout) => {
                let ch = LAST_NATIVE_ASK_REPLY_CHANNEL.swap(ptr::null_mut(), Ordering::AcqRel);
                if !ch.is_null() {
                    // SAFETY: this is the captured in-flight reply channel from the stalled ask.
                    unsafe {
                        let _ = crate::reply_channel::hew_reply(ch, ptr::null_mut(), 0);
                    }
                }
                let recovered = rx
                    .recv_timeout(std::time::Duration::from_secs(1))
                    .expect("manual fallback reply should unblock a stalled self-stop ask");
                ask_thread
                    .join()
                    .expect("native ask waiter thread should not panic after cleanup");
                assert!(
                    recovered,
                    "manual fallback reply should still resolve self-stop asks as null"
                );
                panic!(
                    "native hew_actor_ask should resolve null after self-stop without manual cleanup"
                );
            }
            Err(err) => panic!("native ask waiter thread disconnected unexpectedly: {err:?}"),
        };

        ask_thread
            .join()
            .expect("native ask waiter thread should not panic");

        assert!(
            reply_is_null,
            "ask should resolve as null when the actor self-stops before replying"
        );
        // SAFETY: `actor` remains allocated and owned by this test while we
        // inspect its atomic state.
        let actor_state = unsafe { (*actor).actor_state.load(Ordering::Acquire) };
        assert!(
            actor_state == HewActorState::Stopping as i32
                || actor_state == HewActorState::Stopped as i32,
            "self-stop ask should leave the actor in teardown, got state {actor_state}"
        );
        assert!(
            wait_for_condition(std::time::Duration::from_secs(1), || {
                // SAFETY: `actor` remains allocated and owned by this test while
                // we poll its atomic state.
                unsafe {
                    (*actor).actor_state.load(Ordering::Acquire) == HewActorState::Stopped as i32
                }
            }),
            "self-stop ask should eventually drive the actor to Stopped"
        );
        assert!(
            wait_for_condition(std::time::Duration::from_secs(1), || {
                reply_channel::active_channel_count() == 0
            }),
            "self-stop ask cleanup should release the native reply channel",
        );

        // SAFETY: actor is stopped and owned by this test.
        assert_eq!(unsafe { hew_actor_free(actor) }, 0);

        drop(runtime);
        assert_eq!(reply_channel::active_channel_count(), 0);
    }

    #[test]
    fn native_ask_successful_reply_returns_value_without_duplicate_cleanup() {
        let _guard = crate::runtime_test_guard();
        let runtime = NativeSchedulerGuard::new();

        assert_eq!(reply_channel::active_channel_count(), 0);

        // SAFETY: null state and dispatch function are valid for actor spawn.
        let actor =
            unsafe { hew_actor_spawn(std::ptr::null_mut(), 0, Some(native_reply_once_dispatch)) };
        assert!(!actor.is_null());

        // SAFETY: actor is valid for the duration of the ask.
        let reply = unsafe { hew_actor_ask(actor, 1, ptr::null_mut(), 0) };
        assert!(!reply.is_null(), "native ask should return the reply value");
        // SAFETY: non-null asks return a malloc-allocated i32 payload here.
        assert_eq!(unsafe { *reply.cast::<i32>() }, 21);
        // SAFETY: successful ask replies are malloc-allocated.
        unsafe { libc::free(reply) };

        assert!(
            wait_for_condition(std::time::Duration::from_secs(1), || {
                reply_channel::active_channel_count() == 0
            }),
            "successful native asks should leave no live reply channels",
        );

        // SAFETY: actor is idle and owned by this test.
        assert_eq!(unsafe { hew_actor_free(actor) }, 0);

        drop(runtime);
        assert_eq!(reply_channel::active_channel_count(), 0);
    }

    #[test]
    fn native_ask_timeout_rejects_late_reply_after_blocking_dispatch() {
        let _guard = crate::runtime_test_guard();
        let runtime = NativeSchedulerGuard::new();

        assert_eq!(reply_channel::active_channel_count(), 0);

        // SAFETY: null state and dispatch function are valid for actor spawn.
        let actor =
            unsafe { hew_actor_spawn(std::ptr::null_mut(), 0, Some(native_late_reply_dispatch)) };
        assert!(!actor.is_null());

        // SAFETY: actor is valid for the duration of the timed ask.
        let reply = unsafe { hew_actor_ask_timeout(actor, 1, ptr::null_mut(), 0, 1) };
        assert!(
            reply.is_null(),
            "timed native asks should reject replies that only arrive after the timeout"
        );
        assert!(
            wait_for_condition(std::time::Duration::from_secs(1), || {
                reply_channel::active_channel_count() == 0
            }),
            "timed-out native asks should release late-reply channels after cancellation",
        );
        assert!(
            wait_for_condition(std::time::Duration::from_secs(1), || {
                // SAFETY: actor remains owned by this test while waiting for dispatch to finish.
                let state = unsafe { (*actor).actor_state.load(Ordering::Acquire) };
                state == HewActorState::Idle as i32 || state == HewActorState::Stopped as i32
            }),
            "late-reply dispatch should finish after the timeout path",
        );

        // SAFETY: actor is quiescent and owned by this test.
        assert_eq!(unsafe { hew_actor_free(actor) }, 0);

        drop(runtime);
        assert_eq!(reply_channel::active_channel_count(), 0);
    }

    #[test]
    fn native_ask_reply_then_trap_returns_value_without_duplicate_crash_reply() {
        let _guard = crate::runtime_test_guard();
        let runtime = NativeSchedulerGuard::new();

        assert_eq!(reply_channel::active_channel_count(), 0);

        // SAFETY: null state and dispatch function are valid for actor spawn.
        let actor = unsafe {
            hew_actor_spawn(
                std::ptr::null_mut(),
                0,
                Some(native_reply_then_trap_dispatch),
            )
        };
        assert!(!actor.is_null());

        // SAFETY: actor is valid for the duration of the ask.
        let reply = unsafe { hew_actor_ask(actor, 1, ptr::null_mut(), 0) };
        assert!(
            !reply.is_null(),
            "asks should preserve the first reply even if dispatch traps afterwards"
        );
        // SAFETY: non-null asks return a malloc-allocated i32 payload here.
        assert_eq!(unsafe { *reply.cast::<i32>() }, 123);
        // SAFETY: successful ask replies are malloc-allocated.
        unsafe { libc::free(reply) };

        assert!(
            wait_for_condition(std::time::Duration::from_secs(1), || {
                // SAFETY: actor remains owned by this test while we poll its state.
                let state = unsafe { (*actor).actor_state.load(Ordering::Acquire) };
                state == HewActorState::Crashed as i32
            }),
            "reply-then-trap dispatch should still transition the actor to Crashed",
        );
        assert!(
            wait_for_condition(std::time::Duration::from_secs(1), || {
                reply_channel::active_channel_count() == 0
            }),
            "trap-after-reply asks should not double-complete or leak reply channels",
        );

        // SAFETY: actor is quiescent and owned by this test.
        assert_eq!(unsafe { hew_actor_free(actor) }, 0);

        drop(runtime);
        assert_eq!(reply_channel::active_channel_count(), 0);
    }

    unsafe extern "C-unwind" fn native_self_stop_then_trap_dispatch(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        // The handler self-stops (transitions Running → Stopping) and then
        // panics.  Crash recovery must dominate the pending self-stop:
        // publish `Crashed` (not `Stopped`) and run the full
        // link/monitor/supervisor notification path.  Before the Stage 1
        // ASan-cleanup fix this path went through `handle_crash_recovery`
        // → `hew_actor_trap`'s CAS loop, which accepts any non-terminal
        // current state and writes `Crashed`.  The Crashing-intermediate
        // ordering must preserve the same dominance semantics: the worker
        // CAS-loops both `Running → Crashing` and `Stopping → Crashing` so
        // that a self-stopped-then-crashed actor still publishes `Crashed`
        // and notifies supervisors/links/monitors rather than stalling
        // permanently in `Stopping`.
        hew_actor_self_stop();
        hew_panic();

        std::ptr::null_mut()
    }

    /// Regression: self-stop followed by a panic in the same dispatch must
    /// still publish `Crashed` (crash dominates the pending `Stopping`),
    /// run the supervisor/link/monitor notification path, and allow
    /// `hew_actor_free` to complete within bounded wait.  Without
    /// `Stopping → Crashing` acceptance in the scheduler's crash branch,
    /// the actor would be stranded in `Stopping` (non-quiescent), no
    /// crash report would publish, and `hew_actor_free` would time out.
    #[test]
    fn native_self_stop_then_crash_publishes_crashed_and_notifies_supervisor() {
        let _guard = crate::runtime_test_guard();
        let runtime = NativeSchedulerGuard::new();

        assert_eq!(reply_channel::active_channel_count(), 0);

        // SAFETY: null state + valid dispatch are valid spawn args.
        let actor = unsafe {
            hew_actor_spawn(
                std::ptr::null_mut(),
                0,
                Some(native_self_stop_then_trap_dispatch),
            )
        };
        assert!(!actor.is_null());

        // Deliver a message to trigger the dispatch (no ask — the handler
        // self-stops then crashes; no reply is expected or possible).
        // SAFETY: actor is valid and tracked.
        unsafe { hew_actor_send(actor, 1, ptr::null_mut(), 0) };

        // (a) State reaches `Crashed` and the crash path has published its
        // error code.  Bounded by 2s — the worker runs arena_reset + msg-node
        // free + handle_crash_recovery synchronously and the test fails fast
        // rather than hanging.
        //
        // The terminal-state CAS in `hew_actor_trap` intentionally happens
        // before mailbox close / teardown / `error_code.store(...)`.  Polling
        // only `actor_state == Crashed` and then immediately reading
        // `error_code` leaves a tiny synchronization gap on cold or contended
        // runners: the state can be visible before the release-store of the
        // error code.  Wait for both observations together so the assertion
        // tests the intended invariant instead of racing the implementation's
        // documented ordering.
        assert!(
            wait_for_condition(std::time::Duration::from_secs(2), || {
                // SAFETY: actor remains owned by this test while we poll its state.
                let state = unsafe { (*actor).actor_state.load(Ordering::Acquire) };
                // SAFETY: actor is owned by this test.
                let err = unsafe { hew_actor_get_error(actor) };
                state == HewActorState::Crashed as i32 && err != 0
            }),
            "self-stop-then-crash must publish Crashed and a non-zero error_code; actor must not be stranded in Stopping/Crashing or race before crash publication completes",
        );

        // (b) `hew_actor_free` completes within its bounded wait
        // (`actor.rs::hew_actor_free_inner` has a 2s timeout on the
        // quiescence spin).  If `Crashing` had stalled the waiter, this
        // would return -2 instead of 0.
        // SAFETY: actor is quiescent and owned by this test.
        let free_rc = unsafe { hew_actor_free(actor) };
        assert_eq!(
            free_rc, 0,
            "hew_actor_free must complete bounded after Crashing → Crashed publication",
        );

        drop(runtime);
        assert_eq!(reply_channel::active_channel_count(), 0);
    }

    // ── ask error discrimination tests ───────────────────────────────────

    /// `hew_actor_ask` on a stopped actor sets `ActorStopped` in the error slot.
    #[test]
    fn native_ask_stopped_actor_sets_actor_stopped_error() {
        let _guard = crate::runtime_test_guard();

        // SAFETY: null state + valid dispatch are valid spawn args.
        let actor = unsafe { hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch)) };
        assert!(!actor.is_null());
        // SAFETY: actor is valid.
        unsafe { hew_actor_stop(actor) };

        // Reset error slot, then attempt ask.
        LAST_ACTOR_ASK_ERROR.with(|c| c.set(AskError::None as i32));
        // SAFETY: actor is stopped but pointer remains valid.
        let reply = unsafe { hew_actor_ask(actor, 1, ptr::null_mut(), 0) };
        assert!(reply.is_null(), "ask on stopped actor must return null");
        assert_eq!(
            hew_actor_ask_take_last_error(),
            AskError::ActorStopped as i32,
            "stopped actor must report ActorStopped error"
        );

        // SAFETY: actor is stopped and owned by this test.
        assert_eq!(unsafe { hew_actor_free(actor) }, 0);
    }

    #[test]
    fn native_ask_send_oom_releases_reply_channel() {
        let _guard = crate::runtime_test_guard();
        let runtime = NativeSchedulerGuard::new();

        assert_eq!(reply_channel::active_channel_count(), 0);
        // SAFETY: null state + valid dispatch are valid spawn args.
        let actor = unsafe { hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch)) };
        assert!(!actor.is_null());

        LAST_ACTOR_ASK_ERROR.with(|c| c.set(AskError::None as i32));
        let _alloc_guard = crate::mailbox::fail_mailbox_alloc_on_nth(0);
        // SAFETY: actor is live and the forced mailbox allocation failure makes
        // the ask fail before any reply can be queued.
        let reply = unsafe { hew_actor_ask(actor, 1, ptr::null_mut(), 0) };
        assert!(reply.is_null(), "OOM ask send must return null");
        assert_eq!(
            hew_actor_ask_take_last_error(),
            AskError::ActorStopped as i32,
            "send-side OOM is reported through the ActorStopped ask bucket"
        );
        assert_eq!(
            reply_channel::active_channel_count(),
            0,
            "failed ask send must release both reply-channel references"
        );

        // SAFETY: the ask never enqueued work, so stopping/freely cleaning the
        // actor is valid once the reply-channel invariant above holds.
        unsafe {
            hew_actor_stop(actor);
            assert_eq!(hew_actor_free(actor), 0);
        }
        drop(runtime);
    }

    /// `hew_actor_ask_timeout` on a stopped actor sets `ActorStopped`.
    #[test]
    fn native_ask_timeout_stopped_actor_sets_actor_stopped_error() {
        let _guard = crate::runtime_test_guard();

        // SAFETY: null state + valid dispatch are valid spawn args.
        let actor = unsafe { hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch)) };
        assert!(!actor.is_null());
        // SAFETY: actor is live and single-owner; stopping it to force send failure.
        unsafe { hew_actor_stop(actor) };

        LAST_ACTOR_ASK_ERROR.with(|c| c.set(AskError::None as i32));
        // SAFETY: actor is stopped.
        let reply = unsafe { hew_actor_ask_timeout(actor, 1, ptr::null_mut(), 0, 50) };
        assert!(reply.is_null());
        assert_eq!(
            hew_actor_ask_take_last_error(),
            AskError::ActorStopped as i32,
            "send failure on stopped actor must report ActorStopped"
        );
        // SAFETY: actor was stopped above; no asks are pending.
        assert_eq!(unsafe { hew_actor_free(actor) }, 0);
    }

    /// `hew_actor_ask_timeout` fires `Timeout` when the handler does not reply in time.
    #[test]
    fn native_ask_timeout_sets_timeout_error() {
        let _guard = crate::runtime_test_guard();
        let runtime = NativeSchedulerGuard::new();

        // SAFETY: null state + valid dispatch.
        let actor =
            unsafe { hew_actor_spawn(std::ptr::null_mut(), 0, Some(native_late_reply_dispatch)) };
        assert!(!actor.is_null());

        LAST_ACTOR_ASK_ERROR.with(|c| c.set(AskError::None as i32));
        // SAFETY: actor is valid; 1 ms deadline is too short for the 20 ms handler.
        let reply = unsafe { hew_actor_ask_timeout(actor, 1, ptr::null_mut(), 0, 1) };
        assert!(reply.is_null(), "ask must time out");
        assert_eq!(
            hew_actor_ask_take_last_error(),
            AskError::Timeout as i32,
            "timed-out ask must report Timeout"
        );

        // Let the late-reply dispatch finish and free the actor cleanly.
        assert!(
            wait_for_condition(std::time::Duration::from_secs(1), || {
                reply_channel::active_channel_count() == 0
            }),
            "late-reply channel must be released after cancellation",
        );
        // SAFETY: actor was spawned above and all channels are drained.
        assert_eq!(unsafe { hew_actor_free(actor) }, 0);
        drop(runtime);
    }

    /// `hew_actor_ask` when the actor self-stops without replying sets `OrphanedAsk`.
    #[test]
    fn native_ask_orphaned_sets_orphaned_ask_error() {
        let _guard = crate::runtime_test_guard();
        let runtime = NativeSchedulerGuard::new();

        LAST_NATIVE_ASK_REPLY_CHANNEL.store(ptr::null_mut(), Ordering::Release);
        // SAFETY: null state + valid dispatch.
        let actor = unsafe {
            hew_actor_spawn(
                std::ptr::null_mut(),
                0,
                Some(native_self_stop_without_reply_dispatch),
            )
        };
        assert!(!actor.is_null());

        LAST_ACTOR_ASK_ERROR.with(|c| c.set(AskError::None as i32));

        let actor_addr = actor as usize;
        let (tx, rx) = std::sync::mpsc::channel();
        let handle = std::thread::spawn(move || {
            let actor = actor_addr as *mut HewActor;
            // SAFETY: actor was spawned above and remains live until the thread joins.
            let reply = unsafe { hew_actor_ask(actor, 1, ptr::null_mut(), 0) };
            let is_null = reply.is_null();
            if !reply.is_null() {
                // SAFETY: reply was allocated by the runtime and ownership transfers to caller.
                unsafe { libc::free(reply) };
            }
            let err = hew_actor_ask_take_last_error();
            tx.send((is_null, err)).expect("sender should be live");
        });

        let (is_null, err) = if let Ok(v) = rx.recv_timeout(std::time::Duration::from_secs(2)) {
            v
        } else {
            // Fallback: manually unblock a stalled ask (test environment artefact).
            let ch = LAST_NATIVE_ASK_REPLY_CHANNEL.swap(ptr::null_mut(), Ordering::AcqRel);
            if !ch.is_null() {
                // SAFETY: ch was retrieved from the atomic; hew_reply takes ownership.
                unsafe {
                    let _ = crate::reply_channel::hew_reply(ch, ptr::null_mut(), 0);
                }
            }
            rx.recv_timeout(std::time::Duration::from_secs(1))
                .expect("fallback reply should unblock ask")
        };
        handle.join().expect("ask thread must not panic");

        assert!(is_null, "orphaned ask must return null");
        assert_eq!(
            err,
            AskError::OrphanedAsk as i32,
            "orphaned ask must report OrphanedAsk"
        );

        assert!(
            wait_for_condition(std::time::Duration::from_secs(1), || {
                reply_channel::active_channel_count() == 0
            }),
            "orphaned ask must release its reply channel"
        );
        // SAFETY: actor has self-stopped; all channels are released.
        assert_eq!(unsafe { hew_actor_free(actor) }, 0);
        drop(runtime);
    }

    /// Successful ask clears the error slot.
    #[test]
    fn native_ask_success_clears_error_slot() {
        let _guard = crate::runtime_test_guard();
        let runtime = NativeSchedulerGuard::new();

        // SAFETY: null state + valid dispatch.
        let actor =
            unsafe { hew_actor_spawn(std::ptr::null_mut(), 0, Some(native_reply_once_dispatch)) };
        assert!(!actor.is_null());

        // Poison slot, then succeed.
        LAST_ACTOR_ASK_ERROR.with(|c| c.set(AskError::Timeout as i32));
        // SAFETY: actor is valid.
        let reply = unsafe { hew_actor_ask(actor, 1, ptr::null_mut(), 0) };
        assert!(!reply.is_null(), "ask must succeed");
        // SAFETY: non-null reply is malloc-allocated.
        unsafe { libc::free(reply) };
        assert_eq!(
            hew_actor_ask_take_last_error(),
            AskError::None as i32,
            "successful ask must clear the error slot"
        );

        // SAFETY: actor is live; ask has returned and no pending channels remain.
        assert_eq!(unsafe { hew_actor_free(actor) }, 0);
        drop(runtime);
    }

    /// `hew_actor_ask_take_last_error` resets the slot to None after reading.
    #[test]
    fn actor_ask_take_last_error_resets_slot() {
        LAST_ACTOR_ASK_ERROR.with(|c| c.set(AskError::Timeout as i32));
        let first = hew_actor_ask_take_last_error();
        let second = hew_actor_ask_take_last_error();
        assert_eq!(
            first,
            AskError::Timeout as i32,
            "first take must return Timeout"
        );
        assert_eq!(
            second,
            AskError::None as i32,
            "second take must return None"
        );
    }

    // ── MailboxFull / NoRunnableWork discrimination (native) ─────────────

    /// `hew_actor_ask` on a bounded mailbox that is at capacity returns `MailboxFull`.
    ///
    /// The send inside the ask sees a full mailbox (capacity = 1, one pre-queued
    /// message) and returns `ErrMailboxFull` before the ask-wait loop is entered.
    ///
    /// The pre-fill is done by calling `hew_mailbox_send` directly on the mailbox
    /// pointer.  This bypasses `actor_send_result_internal_reply` (and therefore
    /// `sched_enqueue`) intentionally: we want the message to sit in the mailbox
    /// without the actor being scheduled, so the slot is still occupied when the
    /// ask executes.  The actor remains in the `Idle` state throughout, which lets
    /// `hew_actor_stop` CAS it directly to `Stopped` for clean teardown — no
    /// scheduler is required.
    #[test]
    fn native_ask_bounded_mailbox_full_sets_mailbox_full_error() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: null state + valid dispatch are valid spawn args.
        let actor = unsafe { hew_actor_spawn_bounded(ptr::null_mut(), 0, Some(noop_dispatch), 1) };
        assert!(!actor.is_null());

        // Directly enqueue one message into the mailbox, bypassing the actor-state
        // transition and scheduler enqueue.  The actor stays Idle; the mailbox now
        // has count=1 == capacity=1.
        // SAFETY: actor is valid; mailbox pointer is valid for the actor's lifetime.
        let mb = unsafe { (*actor).mailbox.cast::<mailbox::HewMailbox>() };
        // SAFETY: mb is a valid, non-null pointer to a HewMailbox owned by this actor.
        // The null data pointer is intentional — the message slot just needs to exist.
        let pre_fill = unsafe { mailbox::hew_mailbox_send(mb, 1, ptr::null_mut(), 0) };
        assert_eq!(
            pre_fill,
            HewError::Ok as i32,
            "pre-fill into empty bounded mailbox must succeed"
        );

        // Reset the error slot, then ask. The send inside the ask hits the full
        // mailbox and returns ErrMailboxFull immediately — the ask-wait loop is
        // never entered.
        LAST_ACTOR_ASK_ERROR.with(|c| c.set(AskError::None as i32));
        // SAFETY: actor is valid; the ask send will fail with MailboxFull.
        let reply = unsafe { hew_actor_ask(actor, 1, ptr::null_mut(), 0) };
        assert!(
            reply.is_null(),
            "ask into full bounded mailbox must return null"
        );
        assert_eq!(
            hew_actor_ask_take_last_error(),
            AskError::MailboxFull as i32,
            "full bounded mailbox must report MailboxFull"
        );

        // Actor is still Idle (no state transition occurred during pre-fill).
        // hew_actor_stop CAS Idle → Stopped succeeds; no scheduler needed.
        // SAFETY: actor is valid; closing a live actor's mailbox is safe.
        unsafe { hew_actor_stop(actor) };
        // SAFETY: actor is Stopped (quiescent); hew_mailbox_free drains the
        // pre-filled message during free_actor_resources.
        assert_eq!(unsafe { hew_actor_free(actor) }, 0);
    }

    /// Bounded-mailbox actor that self-stops without replying sets `OrphanedAsk`,
    /// not `MailboxFull`: the mailbox has room for the ask message, so the
    /// discriminant is the orphaned reply channel, not a send failure.
    #[test]
    fn native_ask_bounded_actor_orphan_sets_orphaned_ask_error() {
        let _guard = crate::runtime_test_guard();
        let runtime = NativeSchedulerGuard::new();

        LAST_NATIVE_ASK_REPLY_CHANNEL.store(ptr::null_mut(), Ordering::Release);
        // capacity=8: plenty of room for the ask message, so the send succeeds
        // and the discriminant is the orphaned reply channel.
        // SAFETY: null state + valid dispatch are valid spawn args.
        let actor = unsafe {
            hew_actor_spawn_bounded(
                ptr::null_mut(),
                0,
                Some(native_self_stop_without_reply_dispatch),
                8,
            )
        };
        assert!(!actor.is_null());

        LAST_ACTOR_ASK_ERROR.with(|c| c.set(AskError::None as i32));

        let actor_addr = actor as usize;
        let (tx, rx) = std::sync::mpsc::channel();
        let handle = std::thread::spawn(move || {
            let actor = actor_addr as *mut HewActor;
            // SAFETY: actor was spawned above and remains live until the thread joins.
            let reply = unsafe { hew_actor_ask(actor, 1, ptr::null_mut(), 0) };
            let is_null = reply.is_null();
            if !reply.is_null() {
                // SAFETY: reply was allocated by the runtime and ownership transfers to caller.
                unsafe { libc::free(reply) };
            }
            let err = hew_actor_ask_take_last_error();
            tx.send((is_null, err)).expect("sender should be live");
        });

        let (is_null, err) = if let Ok(v) = rx.recv_timeout(std::time::Duration::from_secs(2)) {
            v
        } else {
            let ch = LAST_NATIVE_ASK_REPLY_CHANNEL.swap(ptr::null_mut(), Ordering::AcqRel);
            if !ch.is_null() {
                // SAFETY: ch was retrieved from the atomic; hew_reply takes ownership.
                unsafe {
                    let _ = crate::reply_channel::hew_reply(ch, ptr::null_mut(), 0);
                }
            }
            rx.recv_timeout(std::time::Duration::from_secs(1))
                .expect("fallback reply should unblock ask")
        };
        handle.join().expect("ask thread must not panic");

        assert!(is_null, "bounded-actor orphaned ask must return null");
        assert_eq!(
            err,
            AskError::OrphanedAsk as i32,
            "bounded-actor orphaned ask must report OrphanedAsk, not MailboxFull"
        );

        assert!(
            wait_for_condition(std::time::Duration::from_secs(1), || {
                reply_channel::active_channel_count() == 0
            }),
            "orphaned ask on bounded actor must release its reply channel"
        );
        // SAFETY: actor has self-stopped; all channels are released.
        assert_eq!(unsafe { hew_actor_free(actor) }, 0);
        drop(runtime);
    }

    #[test]
    fn stop_idle_actor_is_idempotent_and_queues_no_shutdown_sys_messages() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: Spawning with null state and a valid dispatch function.
        let actor = unsafe { hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch)) };
        assert!(!actor.is_null());

        // SAFETY: actor/mailbox pointers are valid for the duration of the test.
        unsafe {
            let mb = (*actor).mailbox.cast::<HewMailbox>();
            assert_eq!(mailbox::hew_mailbox_sys_len(mb), 0);

            hew_actor_stop(actor);
            assert_eq!(
                (*actor).actor_state.load(Ordering::Acquire),
                HewActorState::Stopped as i32
            );
            assert_eq!(
                mailbox::hew_mailbox_sys_len(mb),
                0,
                "stopping an idle actor should not enqueue an unprocessable shutdown signal"
            );

            hew_actor_stop(actor);
            hew_actor_stop(actor);
            assert_eq!(
                mailbox::hew_mailbox_sys_len(mb),
                0,
                "repeated stop calls on a stopped actor must not accumulate system messages"
            );

            assert_eq!(hew_actor_free(actor), 0);
        }
    }

    #[test]
    fn stop_runnable_actor_does_not_enqueue_shutdown_signal() {
        let (actor, mailbox) = make_stop_test_actor(HewActorState::Runnable);

        // SAFETY: actor/mailbox pointers are valid for the duration of the test.
        unsafe {
            hew_actor_stop(actor);
            hew_actor_stop(actor);
            assert!(
                mailbox::mailbox_is_closed(mailbox),
                "stop must close runnable actors before they drain their queued activation"
            );
            assert_eq!(
                mailbox::hew_mailbox_sys_len(mailbox),
                0,
                "runnable actors already have a queued activation and should not receive an extra shutdown signal"
            );
            mailbox::hew_mailbox_free(mailbox);
            drop(Box::from_raw(actor));
        }
    }

    /// Live actor-level alias delivery: drive an envelope through
    /// `hew_actor_send_aliased` to a real (non-null) actor, drain its
    /// mailbox, and assert the payload is delivered by reference and the
    /// envelope is released **exactly once**. The actor starts `Running`
    /// so the wake CAS (`Idle → Runnable`) is a no-op and no scheduler is
    /// needed. Pins the success exit of the actor-level single-release
    /// contract.
    /// Shared serialisation lock for the actor-level alias-send tests:
    /// the live-delivery test (id 1) and the drop-fault test (its own
    /// dedicated id) both run `hew_actor_send_aliased` with a process-wide
    /// drop counter, so they take this lock to keep the counter readings
    /// unambiguous. The drop-fault test additionally pins a *unique* actor
    /// id so its armed fault can never be consumed by an unrelated id-1
    /// sender elsewhere in the suite.
    static ALIAS_SEND_TEST_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());

    #[test]
    fn actor_send_aliased_delivers_to_live_actor_and_releases_once() {
        static DROP_COUNT: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);

        unsafe extern "C" fn count_drop_glue(_payload: *mut c_void) {
            DROP_COUNT.fetch_add(1, Ordering::SeqCst);
        }

        let _guard = ALIAS_SEND_TEST_LOCK.lock().unwrap();
        DROP_COUNT.store(0, Ordering::SeqCst);

        let (actor, mailbox) = make_stop_test_actor(HewActorState::Running);
        // SAFETY: actor/mailbox are valid for the test; envelope carries
        // one refcount that transfers into the alias send.
        unsafe {
            let size = 5usize;
            let payload = libc::malloc(size);
            assert!(!payload.is_null());
            libc::memcpy(payload, b"alive".as_ptr().cast(), size);
            let env = crate::mailbox::hew_msg_envelope_new(payload, size, Some(count_drop_glue));
            assert_eq!((*env).refcount.load(Ordering::SeqCst), 1);

            hew_actor_send_aliased(actor, 4, env);
            // Enqueued, not yet consumed.
            assert_eq!(DROP_COUNT.load(Ordering::SeqCst), 0);
            assert_eq!(mailbox::hew_mailbox_has_messages(mailbox), 1);

            // Drain (models dispatch); node free releases the envelope once.
            let node = mailbox::hew_mailbox_try_recv(mailbox);
            assert!(!node.is_null());
            assert_eq!((*node).msg_type, 4);
            let borrowed = crate::mailbox::hew_msg_envelope_payload_ptr((*node).envelope);
            assert_eq!(
                borrowed, payload,
                "payload delivered by reference, not copied"
            );
            mailbox::hew_msg_node_free(node);
            assert_eq!(
                DROP_COUNT.load(Ordering::SeqCst),
                1,
                "live-actor alias send must release the envelope exactly once"
            );

            mailbox::hew_mailbox_free(mailbox);
            drop(Box::from_raw(actor));
        }
    }

    /// EXIT(drop-fault-injection): when the deterministic harness asks the
    /// runtime to silently discard a message, `hew_actor_send_aliased`
    /// never enqueues the node — the receiver will never consume the
    /// payload — so it must release the caller-transferred envelope
    /// refcount directly, exactly once. Pins the drop-fault exit of the
    /// actor-level single-release contract.
    #[test]
    fn actor_send_aliased_drop_fault_releases_once() {
        static DROP_COUNT: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);

        unsafe extern "C" fn count_drop_glue(_payload: *mut c_void) {
            DROP_COUNT.fetch_add(1, Ordering::SeqCst);
        }

        let _guard = ALIAS_SEND_TEST_LOCK.lock().unwrap();
        DROP_COUNT.store(0, Ordering::SeqCst);

        // Use a unique, suite-private actor id so the armed drop fault can
        // never be consumed by an unrelated id-1 sender running in
        // parallel (both send paths consult the process-global fault
        // table, keyed by actor id).
        let fault_actor_id: u64 = 0x0A11_A5ED_DEAD_0001;
        let (actor, mailbox) = make_stop_test_actor_with_id(fault_actor_id, HewActorState::Running);
        // SAFETY: actor/mailbox are valid for the test; envelope carries
        // one refcount that transfers into the alias send.
        unsafe {
            // Arm a single-shot drop fault for this actor. Clear
            // first/last so the process-global fault table cannot leak
            // across tests.
            crate::deterministic::hew_fault_clear(fault_actor_id);
            crate::deterministic::hew_fault_inject_drop(fault_actor_id, 1);

            let size = 4usize;
            let payload = libc::malloc(size);
            assert!(!payload.is_null());
            libc::memcpy(payload, b"drop".as_ptr().cast(), size);
            let env = crate::mailbox::hew_msg_envelope_new(payload, size, Some(count_drop_glue));
            assert_eq!((*env).refcount.load(Ordering::SeqCst), 1);

            hew_actor_send_aliased(actor, 9, env);

            // Message discarded: nothing enqueued, envelope released once.
            assert_eq!(
                mailbox::hew_mailbox_has_messages(mailbox),
                0,
                "drop-fault must not enqueue the alias node"
            );
            assert_eq!(
                DROP_COUNT.load(Ordering::SeqCst),
                1,
                "drop-fault exit must release the envelope exactly once"
            );

            crate::deterministic::hew_fault_clear(fault_actor_id);
            mailbox::hew_mailbox_free(mailbox);
            drop(Box::from_raw(actor));
        }
    }

    /// Terminal-state send gate, copy-mode paths. `hew_actor_trap` takes its
    /// terminal CAS BEFORE closing the mailbox, leaving a window in which the
    /// actor is terminal but the mailbox is still open. A send racing that
    /// window must be rejected by the actor-level terminal gate so it never
    /// enqueues into, or reports false success against, a terminal actor —
    /// closing the window the lost-crash-notify reorder opened.
    ///
    /// Drives the test actor directly into each terminal state (the trap's CAS
    /// post-state, mailbox deliberately left OPEN to model the window) and
    /// asserts every copy-mode held-pointer send path rejects with
    /// `ErrActorStopped` and enqueues nothing.
    #[test]
    fn terminal_actor_copy_send_paths_reject_without_enqueue() {
        let _guard = crate::runtime_test_guard();

        for terminal in [HewActorState::Crashed, HewActorState::Stopped] {
            // `Running` initial state so a (hypothetically) accepted send would
            // not also try to push onto a scheduler queue; the mailbox is left
            // OPEN to model the trap's terminal-CAS-before-close window.
            let (actor, mailbox) = make_stop_test_actor(HewActorState::Running);
            // SAFETY: the test exclusively owns `actor`; publish the terminal
            // state with a release store, exactly as the trap's CAS does.
            unsafe {
                (*actor)
                    .actor_state
                    .store(terminal as i32, Ordering::Release);
                assert!(
                    !mailbox::mailbox_is_closed(mailbox),
                    "mailbox must be OPEN to model the terminal-CAS-before-close window"
                );
            }

            // try_send rejects without enqueue. The non-blocking path returns
            // `ErrClosed` (mirroring `hew_mailbox_try_send`'s closed-mailbox
            // code), distinct from the blocking paths' `ErrActorStopped`.
            // SAFETY: `actor` valid and owned; null payload.
            let try_rc = unsafe { hew_actor_try_send(actor, 1, ptr::null_mut(), 0) };
            assert_eq!(
                try_rc,
                HewError::ErrClosed as i32,
                "try_send into a {terminal:?} actor must be rejected by the terminal gate"
            );

            // The fire-and-forget result path (used by `hew_actor_send`).
            // SAFETY: as above.
            let send_rc = unsafe { actor_send_result_internal(actor, 1, ptr::null_mut(), 0) };
            assert_eq!(
                send_rc,
                HewError::ErrActorStopped as i32,
                "send into a {terminal:?} actor must be rejected by the terminal gate"
            );

            // The guaranteed (out-of-band terminal-event) path.
            // SAFETY: as above.
            let guaranteed_rc = unsafe { hew_actor_send_guaranteed(actor, 1, ptr::null_mut(), 0) };
            assert_eq!(
                guaranteed_rc,
                HewError::ErrActorStopped as i32,
                "send_guaranteed into a {terminal:?} actor must be rejected by the terminal gate"
            );

            // No path enqueued: nothing reached the (still-open) mailbox.
            // SAFETY: `mailbox` is valid and owned by this test.
            let has_messages = unsafe { mailbox::hew_mailbox_has_messages(mailbox) };
            assert_eq!(
                has_messages, 0,
                "a send rejected by the terminal gate must not enqueue into a {terminal:?} actor"
            );

            // SAFETY: the test fully owns the actor and its mailbox.
            unsafe {
                mailbox::hew_mailbox_free(mailbox);
                drop(Box::from_raw(actor));
            }
        }
    }

    /// Terminal-state send gate, alias path. An alias send transfers exactly
    /// one envelope refcount; on rejection that refcount must be released
    /// exactly once (no leak, no false success consuming it into an
    /// undeliverable enqueued node) — the same single-release outcome as a send
    /// to a closed mailbox. The drop-glue counter is the deterministic oracle
    /// for exactly-once; the mailbox-empty assertion proves no undeliverable
    /// node was enqueued. Both terminal states, mailbox left OPEN to model the
    /// trap's terminal-CAS-before-close window.
    #[test]
    fn terminal_actor_alias_send_releases_envelope_once_without_enqueue() {
        static DROP_COUNT: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);

        unsafe extern "C" fn count_drop_glue(_payload: *mut c_void) {
            DROP_COUNT.fetch_add(1, Ordering::SeqCst);
        }

        let _guard = ALIAS_SEND_TEST_LOCK.lock().unwrap();

        for terminal in [HewActorState::Crashed, HewActorState::Stopped] {
            DROP_COUNT.store(0, Ordering::SeqCst);

            let (actor, mailbox) = make_stop_test_actor(HewActorState::Running);
            // SAFETY: the test exclusively owns `actor`; publish terminal state
            // with a release store as the trap's CAS does, mailbox left OPEN.
            unsafe {
                (*actor)
                    .actor_state
                    .store(terminal as i32, Ordering::Release);
                assert!(
                    !mailbox::mailbox_is_closed(mailbox),
                    "mailbox must be OPEN to model the terminal-CAS-before-close window"
                );

                let size = 5usize;
                let payload = libc::malloc(size);
                assert!(!payload.is_null());
                libc::memcpy(payload, b"alias".as_ptr().cast(), size);
                let env =
                    crate::mailbox::hew_msg_envelope_new(payload, size, Some(count_drop_glue));
                assert_eq!((*env).refcount.load(Ordering::SeqCst), 1);

                hew_actor_send_aliased(actor, 4, env);

                // Rejected: nothing enqueued, envelope released exactly once.
                assert_eq!(
                    mailbox::hew_mailbox_has_messages(mailbox),
                    0,
                    "terminal-gate alias rejection must not enqueue an undeliverable node \
                     ({terminal:?})"
                );
                assert_eq!(
                    DROP_COUNT.load(Ordering::SeqCst),
                    1,
                    "terminal-gate alias rejection must release the envelope exactly once \
                     ({terminal:?})"
                );

                mailbox::hew_mailbox_free(mailbox);
                drop(Box::from_raw(actor));
            }
        }
    }

    /// Concurrent racing-sender window. A real alias send is issued from a
    /// second thread CONCURRENTLY with `hew_actor_trap`'s terminal transition,
    /// looped so the send lands across the whole pre-CAS / in-window /
    /// post-close spectrum. The invariants the gate must hold on EVERY
    /// interleaving: (1) the envelope refcount is released exactly once per
    /// send — the per-iteration drop counter must equal the send count (no
    /// leak, no double-free); (2) the crash notify is never lost — the actor
    /// reaches the `Crashed` terminal state. Run under ASan/LSan this also
    /// proves the rejected send's alias is freed exactly once.
    #[test]
    fn racing_alias_sender_during_trap_releases_once_and_crash_notifies() {
        const ITERS: usize = 2_000;

        static DROP_COUNT: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);

        unsafe extern "C" fn count_drop_glue(_payload: *mut c_void) {
            DROP_COUNT.fetch_add(1, Ordering::SeqCst);
        }

        // `hew_actor_trap` walks the runtime-owned monitor table. Participate
        // in the shared runtime-test serialization contract so another test
        // cannot tear down the installed `RuntimeInner` during that walk.
        let _runtime_guard = crate::runtime_test_guard();
        assert!(
            crate::scheduler::SchedTestLock::is_held(),
            "trap/monitor tests must hold the shared runtime test lock"
        );
        let _alias_guard = ALIAS_SEND_TEST_LOCK.lock().unwrap();

        for _ in 0..ITERS {
            DROP_COUNT.store(0, Ordering::SeqCst);

            // `Running` so the sender's success path would not also enqueue on a
            // scheduler; the trap drives this actor terminal under the sender.
            let (actor, mailbox) = make_stop_test_actor(HewActorState::Running);
            let actor_addr = actor as usize;

            let start = std::sync::Arc::new(std::sync::Barrier::new(2));
            let sender_start = start.clone();

            let sender = std::thread::spawn(move || {
                let actor = actor_addr as *mut HewActor;
                // SAFETY: the actor outlives both threads (freed after join);
                // the envelope carries one refcount transferred into the send.
                unsafe {
                    let size = 5usize;
                    let payload = libc::malloc(size);
                    assert!(!payload.is_null());
                    libc::memcpy(payload, b"alias".as_ptr().cast(), size);
                    let env =
                        crate::mailbox::hew_msg_envelope_new(payload, size, Some(count_drop_glue));
                    sender_start.wait();
                    hew_actor_send_aliased(actor, 4, env);
                }
            });

            start.wait();
            // SAFETY: `actor` is valid; the trap drives it to the Crashed
            // terminal state concurrently with the racing sender.
            unsafe { hew_actor_trap(actor, 1) };

            sender.join().expect("sender thread must not panic");

            // SAFETY: both threads have joined; the actor and mailbox are now
            // exclusively owned by this thread.
            unsafe {
                // Crash notify never lost: the trap published the terminal state.
                assert_eq!(
                    (*actor).actor_state.load(Ordering::Acquire),
                    HewActorState::Crashed as i32,
                    "trap must win the terminal race and publish Crashed"
                );

                // Drain whatever the send enqueued before the gate/close (the
                // inherent "sent the instant before the crash" node, if any) so
                // its envelope release is accounted before the exactly-once check.
                let mut drained = 0usize;
                loop {
                    let node = mailbox::hew_mailbox_try_recv(mailbox);
                    if node.is_null() {
                        break;
                    }
                    mailbox::hew_msg_node_free(node);
                    drained += 1;
                }
                assert!(
                    drained <= 1,
                    "at most one node can land in the pre-close window, drained {drained}"
                );

                mailbox::hew_mailbox_free(mailbox);

                // Exactly-once release of the single send's envelope across
                // EVERY interleaving: rejected-by-gate, rejected-by-close, or
                // enqueued-then-drained — each releases the refcount once.
                assert_eq!(
                    DROP_COUNT.load(Ordering::SeqCst),
                    1,
                    "the racing send's envelope must be released exactly once"
                );

                drop(Box::from_raw(actor));
            }
        }
    }

    /// PROBE (P5-RX Stage 2a, A625): models the codegen contract for an
    /// escaping borrowed `String` view under both runtime receipt modes, and
    /// asserts exactly-once release in each. This test was first reinstated in
    /// its PRE-FIX shape — a naked handler drop of the borrowed handle followed
    /// by the envelope release — which `ASan` flagged as a heap-use-after-free /
    /// double-free (the borrowed buffer is owned by the envelope, so the
    /// handler must NOT free it). The retain-on-escape mechanism flips it green:
    ///
    ///   - BORROW arm (`borrow_mode != 0`): at the owned sink the handler takes
    ///     its OWN retained owner via `hew_string_clone` (a refcount bump on the
    ///     shared buffer). The handler's owned-drop then releases that clone,
    ///     and `hew_msg_envelope_release` releases the envelope's original — two
    ///     decrements against a refcount that the clone raised to two, so the
    ///     backing buffer is freed exactly once.
    ///   - COPY arm (`borrow_mode == 0`): ownership of the payload transferred
    ///     to the handler outright; codegen emits a plain move (no clone), the
    ///     handler frees its private owner once, and nothing else aliases it.
    ///
    /// Wrapped in a 20× loop so a residual double-free or leak is overwhelmingly
    /// likely to trip `ASan` / the per-iteration single-release assertion.
    #[test]
    fn live_borrow_receive_retains_escaping_payload_releases_once() {
        static DROP_COUNT: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);

        unsafe extern "C" fn drop_string_payload(payload: *mut c_void) {
            // SAFETY: the envelope stores a `*mut c_char` string handle in the
            // first pointer-sized slot of `payload` (set by the test below);
            // load it and release one owner.
            let handle = unsafe { *payload.cast::<*mut std::ffi::c_char>() };
            // SAFETY: `handle` is a live header-aware String produced by
            // `hew_string_from_char` (or a clone of it), released exactly once.
            unsafe { crate::string::hew_string_drop(handle) };
            DROP_COUNT.fetch_add(1, Ordering::SeqCst);
        }

        let _guard = ALIAS_SEND_TEST_LOCK.lock().unwrap();

        for _ in 0..20 {
            // ---- BORROW arm: borrow_mode != 0, retain-on-escape ----
            DROP_COUNT.store(0, Ordering::SeqCst);
            // SAFETY: a self-contained envelope lifecycle — allocate a one-slot
            // payload holding a fresh String handle, wrap it, model the handler
            // retain/drop, then release the envelope. Every pointer is live for
            // the block and freed exactly once.
            unsafe {
                let s = crate::string::hew_string_from_char(i32::from(b'x'));
                let slot = std::mem::size_of::<*mut std::ffi::c_char>();
                let buf = libc::malloc(slot).cast::<*mut std::ffi::c_char>();
                assert!(!buf.is_null());
                *buf = s;
                let env = crate::mailbox::hew_msg_envelope_new(
                    buf.cast(),
                    slot,
                    Some(drop_string_payload),
                );

                // Handler escapes the borrowed view into an owned sink. The
                // gated retain hands it a private owner (refcount bump).
                let borrowed = crate::mailbox::hew_msg_envelope_payload_ptr(env);
                let received_handle = *borrowed.cast::<*mut std::ffi::c_char>();
                let retained = crate::string::hew_string_clone(received_handle);

                // Sink's owned-drop releases the handler's clone (1st decrement).
                crate::string::hew_string_drop(retained);
                // Envelope releases its original (2nd decrement -> frees once).
                crate::mailbox::hew_msg_envelope_release(env);

                assert_eq!(
                    DROP_COUNT.load(Ordering::SeqCst),
                    1,
                    "borrow-mode escape must release the shared buffer exactly once"
                );
            }

            // ---- COPY arm: borrow_mode == 0, plain move, sole owner ----
            DROP_COUNT.store(0, Ordering::SeqCst);
            // SAFETY: same self-contained envelope lifecycle as the borrow arm;
            // copy mode emits no clone, so the envelope release is the sole free.
            unsafe {
                let s = crate::string::hew_string_from_char(i32::from(b'y'));
                let slot = std::mem::size_of::<*mut std::ffi::c_char>();
                let buf = libc::malloc(slot).cast::<*mut std::ffi::c_char>();
                assert!(!buf.is_null());
                *buf = s;
                let env = crate::mailbox::hew_msg_envelope_new(
                    buf.cast(),
                    slot,
                    Some(drop_string_payload),
                );

                // No clone in copy mode: the handler owns the payload outright;
                // the envelope release is its sole, single free.
                crate::mailbox::hew_msg_envelope_release(env);

                assert_eq!(
                    DROP_COUNT.load(Ordering::SeqCst),
                    1,
                    "copy-mode receipt must free its owner exactly once"
                );
            }
        }
    }

    #[test]
    fn close_then_stop_runnable_actor_keeps_shutdown_queue_empty() {
        let (actor, mailbox) = make_stop_test_actor(HewActorState::Runnable);

        // SAFETY: actor/mailbox pointers are valid for the duration of the test.
        unsafe {
            hew_actor_close(actor);
            assert_eq!(
                (*actor).actor_state.load(Ordering::Acquire),
                HewActorState::Runnable as i32,
                "close should leave runnable actors runnable while only closing the mailbox"
            );
            assert!(
                mailbox::mailbox_is_closed(mailbox),
                "close must mark the mailbox closed before stop is requested"
            );

            hew_actor_stop(actor);
            assert_eq!(
                mailbox::hew_mailbox_sys_len(mailbox),
                0,
                "stop after close should not enqueue a redundant shutdown signal for runnable actors"
            );

            hew_actor_stop(actor);
            assert_eq!(
                mailbox::hew_mailbox_sys_len(mailbox),
                0,
                "repeated stop after close must keep runnable actors' shutdown queue empty"
            );

            mailbox::hew_mailbox_free(mailbox);
            drop(Box::from_raw(actor));
        }
    }

    #[test]
    fn stop_running_actor_enqueues_at_most_one_shutdown_signal() {
        let (actor, mailbox) = make_stop_test_actor(HewActorState::Running);

        // SAFETY: actor/mailbox pointers are valid for the duration of the test.
        unsafe {
            hew_actor_stop(actor);
            hew_actor_stop(actor);
            assert_eq!(
                mailbox::hew_mailbox_sys_len(mailbox),
                1,
                "only the first stop call should enqueue a shutdown system message for a running actor"
            );
            mailbox::hew_mailbox_free(mailbox);
            drop(Box::from_raw(actor));
        }
    }

    #[test]
    fn close_then_stop_running_actor_enqueues_shutdown_signal_once() {
        let (actor, mailbox) = make_stop_test_actor(HewActorState::Running);

        // SAFETY: actor/mailbox pointers are valid for the duration of the test.
        unsafe {
            hew_actor_close(actor);
            assert_eq!(
                (*actor).actor_state.load(Ordering::Acquire),
                HewActorState::Running as i32,
                "close should leave running actors running while only closing the mailbox"
            );
            assert!(
                mailbox::mailbox_is_closed(mailbox),
                "close must mark the mailbox closed before stop is requested"
            );

            hew_actor_stop(actor);
            assert_eq!(
                mailbox::hew_mailbox_sys_len(mailbox),
                1,
                "stop after close must still enqueue one shutdown system message for a running actor"
            );

            hew_actor_stop(actor);
            assert_eq!(
                mailbox::hew_mailbox_sys_len(mailbox),
                1,
                "repeated stop after close must not accumulate shutdown system messages for a running actor"
            );

            mailbox::hew_mailbox_free(mailbox);
            drop(Box::from_raw(actor));
        }
    }

    #[test]
    fn free_actor_resources_completes_when_terminate_finishes_quickly() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: null state, valid dispatch.
        let actor = unsafe { hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch)) };
        assert!(!actor.is_null());

        // SAFETY: actor pointer is valid — returned by hew_actor_spawn.
        unsafe {
            hew_actor_close(actor);
        }

        let start = std::time::Instant::now();
        // SAFETY: actor is valid, closed, and in a terminal-safe state.
        let rc = unsafe { hew_actor_free(actor) };
        let elapsed = start.elapsed();

        assert_eq!(rc, 0);
        assert!(
            elapsed < std::time::Duration::from_secs(1),
            "free should complete quickly for a cooperating actor, took {elapsed:?}"
        );
    }

    #[test]
    fn terminate_long_does_not_spin() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: null state, valid dispatch.
        let actor = unsafe { hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch)) };
        assert!(!actor.is_null());

        // SAFETY: actor is valid for the duration of the wait below.
        let a = unsafe { &*actor };
        a.terminate_called.store(true, Ordering::Release);
        a.terminate_finished.store(false, Ordering::Release);
        a.actor_state
            .store(HewActorState::Stopped as i32, Ordering::Release);

        TERMINATE_WAIT_POLL_TICKS.store(0, Ordering::Release);
        let actor_addr = actor as usize;
        let finisher = std::thread::spawn(move || {
            std::thread::sleep(std::time::Duration::from_millis(200));
            // SAFETY: free waits for this store before reclaiming the actor.
            unsafe {
                (*(actor_addr as *mut HewActor))
                    .terminate_finished
                    .store(true, Ordering::Release);
            }
        });

        let start = std::time::Instant::now();
        // SAFETY: actor is valid and waits for terminate_finished before free.
        let rc = unsafe { hew_actor_free(actor) };
        let elapsed = start.elapsed();
        finisher.join().unwrap();

        assert_eq!(rc, 0);
        assert!(
            elapsed >= std::time::Duration::from_millis(150),
            "free should wait for the long terminate path, took {elapsed:?}"
        );
        assert!(
            elapsed < std::time::Duration::from_secs(1),
            "sleep-based polling should still finish promptly once terminate completes, took {elapsed:?}"
        );
        assert!(
            TERMINATE_WAIT_POLL_TICKS.load(Ordering::Acquire) < 400,
            "terminate wait should sleep between polls instead of busy-spinning"
        );
    }

    #[test]
    fn free_current_actor_from_dispatch_is_deferred() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: this test fully owns the spawned actor and only mutates its
        // fields while no other runtime threads can access it.
        unsafe {
            let actor = hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch));
            assert!(!actor.is_null());
            (*actor)
                .actor_state
                .store(HewActorState::Stopping as i32, Ordering::Release);

            let _ctx = TestExecutionContext::install(HewExecutionContext {
                actor,
                actor_id: (*actor).id,
                ..HewExecutionContext::default()
            });
            let unblock = defer_state_transition(
                actor,
                HewActorState::Stopped,
                std::time::Duration::from_millis(200),
            );

            let rc = hew_actor_free(actor);
            // Logical proof that the current-thread free DEFERRED instead of
            // tearing the actor down synchronously: the actor must still be live
            // the instant free returns. The real teardown runs on a background
            // thread that waits for the actor to reach a terminal state (driven
            // by `unblock` ~200 ms from now), so a deferred free leaves the actor
            // live here while a synchronous free would already have freed it.
            // This replaces a wall-clock `elapsed < 100ms` bound that coverage
            // instrumentation and load could inflate past the threshold.
            let live_immediately_after = is_actor_live(actor);

            unblock.join().unwrap();

            let freed =
                wait_for_condition(std::time::Duration::from_secs(2), || !is_actor_live(actor));
            if !freed && is_actor_live(actor) {
                (*actor)
                    .actor_state
                    .store(HewActorState::Stopped as i32, Ordering::Release);
                assert_eq!(hew_actor_free(actor), 0);
            }

            assert_eq!(
                rc, 0,
                "current-thread frees should defer instead of timing out"
            );
            assert!(
                live_immediately_after,
                "current-thread free should defer: the actor must still be live the instant free returns, with teardown deferred to a background thread"
            );
            assert!(
                freed,
                "actor should be freed asynchronously after dispatch unwinds"
            );
        }
    }

    #[test]
    fn cleanup_all_actors_waits_for_deferred_free_threads() {
        let _guard = crate::runtime_test_guard();
        let _scheduler = NativeSchedulerGuard::new();

        // SAFETY: this test owns the actor and coordinates all concurrent access.
        unsafe {
            let actor = hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch));
            assert!(!actor.is_null());

            (*actor)
                .actor_state
                .store(HewActorState::Stopped as i32, Ordering::Release);
            (*actor).terminate_called.store(true, Ordering::Release);
            (*actor).terminate_finished.store(false, Ordering::Release);

            let _ctx = TestExecutionContext::install(HewExecutionContext {
                actor,
                actor_id: (*actor).id,
                ..HewExecutionContext::default()
            });
            assert_eq!(hew_actor_free(actor), 0, "self-free should defer");

            let cleanup_started = std::sync::Arc::new(std::sync::Barrier::new(2));
            let cleanup_done = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false));
            let started = cleanup_started.clone();
            let done = cleanup_done.clone();

            let cleanup = std::thread::spawn(move || {
                started.wait();
                // SAFETY: the test synchronizes access and no scheduler work is active.
                cleanup_all_actors();
                done.store(true, Ordering::Release);
            });

            cleanup_started.wait();
            std::thread::sleep(std::time::Duration::from_millis(50));
            assert!(
                !cleanup_done.load(Ordering::Acquire),
                "cleanup_all_actors must wait for deferred self-free threads"
            );

            (*actor).terminate_finished.store(true, Ordering::Release);
            cleanup.join().unwrap();
            assert!(
                !is_actor_live(actor),
                "deferred free should finish before cleanup returns"
            );
        }
    }

    #[test]
    fn drain_actors_all_drain_cleans_registries() {
        let _guard = crate::runtime_test_guard();
        let _scheduler = NativeSchedulerGuard::new();
        let _ticker_guard = crate::timer_periodic::TICKER_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);

        // SAFETY: null state + valid dispatch are valid spawn args.
        let actor_one = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
        // SAFETY: null state + valid dispatch are valid spawn args.
        let actor_two = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
        // SAFETY: null state + valid dispatch are valid spawn args.
        let actor_three = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
        assert!(!actor_one.is_null());
        assert!(!actor_two.is_null());
        assert!(!actor_three.is_null());

        // SAFETY: the spawned actors remain live until the assertions below finish.
        let actor_one_id = unsafe { (*actor_one).id };
        // SAFETY: the spawned actors remain live until the assertions below finish.
        let actor_two_id = unsafe { (*actor_two).id };
        // SAFETY: the spawned actors remain live until the assertions below finish.
        let actor_three_id = unsafe { (*actor_three).id };

        // SAFETY: actor_one is a valid live actor pointer returned by spawn.
        let timer =
            unsafe { crate::timer_periodic::hew_actor_schedule_periodic(actor_one, 7, 100) };
        assert!(
            !timer.is_null(),
            "periodic timer should register successfully"
        );
        // SAFETY: both actor pointers were returned by spawn and are still live.
        unsafe {
            crate::link::hew_actor_link(actor_one, actor_two);
        }
        // SAFETY: both actor pointers were returned by spawn and are still live.
        let monitor_ref = unsafe {
            crate::monitor::register_actor_monitor(actor_three, actor_one)
                .expect("monitor registration")
        };
        assert_ne!(monitor_ref, 0, "monitor registration should succeed");

        assert_eq!(crate::timer_periodic::timer_count_for_actor(actor_one), 1);
        assert!(crate::link::has_links_for_actor(actor_one_id, actor_one));
        assert!(crate::link::has_links_for_actor(actor_two_id, actor_two));
        assert!(crate::monitor::has_monitors_for_actor(
            actor_one_id,
            actor_one
        ));
        assert!(crate::monitor::has_monitors_for_actor(
            actor_three_id,
            actor_three
        ));

        let outcome = drain_actors(
            &[actor_one_id, actor_two_id, actor_three_id],
            std::time::Instant::now() + std::time::Duration::from_secs(1),
        );
        assert_eq!(outcome, DrainOutcome::Drained);
        assert!(!is_actor_live(actor_one));
        assert!(!is_actor_live(actor_two));
        assert!(!is_actor_live(actor_three));
        assert_eq!(crate::timer_periodic::timer_count_for_actor(actor_one), 0);
        assert!(!crate::link::has_links_for_actor(actor_one_id, actor_one));
        assert!(!crate::link::has_links_for_actor(actor_two_id, actor_two));
        assert!(!crate::monitor::has_monitors_for_actor(
            actor_one_id,
            actor_one
        ));
        assert!(!crate::monitor::has_monitors_for_actor(
            actor_three_id,
            actor_three
        ));
    }

    #[test]
    fn drain_actors_partial_drain_with_timeout() {
        let _guard = crate::runtime_test_guard();
        let _scheduler = NativeSchedulerGuard::new();

        DRAIN_BUSY_LOOP_STARTED.store(false, Ordering::Release);
        DRAIN_BUSY_LOOP_RELEASE.store(false, Ordering::Release);

        // SAFETY: null state + valid dispatch are valid spawn args.
        let stubborn_actor =
            unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(drain_busy_loop_dispatch)) };
        // SAFETY: null state + valid dispatch are valid spawn args.
        let helper_actor = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
        // SAFETY: null state + valid dispatch are valid spawn args.
        let spare_actor = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
        assert!(!stubborn_actor.is_null());
        assert!(!helper_actor.is_null());
        assert!(!spare_actor.is_null());

        // SAFETY: the spawned actors remain live until the assertions below finish.
        let stubborn_actor_id = unsafe { (*stubborn_actor).id };
        // SAFETY: the spawned actors remain live until the assertions below finish.
        let helper_actor_id = unsafe { (*helper_actor).id };
        // SAFETY: the spawned actors remain live until the assertions below finish.
        let spare_actor_id = unsafe { (*spare_actor).id };

        // SAFETY: stubborn_actor is a valid live actor pointer returned by spawn.
        unsafe { hew_actor_send(stubborn_actor, 1, ptr::null_mut(), 0) };
        assert!(
            wait_for_condition(std::time::Duration::from_secs(1), || {
                DRAIN_BUSY_LOOP_STARTED.load(Ordering::Acquire)
            }),
            "busy loop actor should begin running before drain starts"
        );

        let outcome = drain_actors(
            &[stubborn_actor_id, helper_actor_id, spare_actor_id],
            std::time::Instant::now() + std::time::Duration::from_millis(100),
        );
        assert_eq!(
            outcome,
            DrainOutcome::Incomplete {
                still_live: vec![stubborn_actor_id],
                crashed: Vec::new(),
            }
        );
        assert!(
            is_actor_live(stubborn_actor),
            "busy actor must remain live at the deadline"
        );
        assert!(
            !is_actor_live(helper_actor),
            "cooperating actor should be drained"
        );
        assert!(
            !is_actor_live(spare_actor),
            "cooperating actor should be drained"
        );

        DRAIN_BUSY_LOOP_RELEASE.store(true, Ordering::Release);
        assert!(
            wait_for_actor_quiescent(stubborn_actor, std::time::Duration::from_secs(5)),
            "busy actor should become quiescent after releasing the loop"
        );
        // SAFETY: stubborn_actor is quiescent after the wait above.
        let free_rc = unsafe { hew_actor_free(stubborn_actor) };
        assert_eq!(free_rc, 0);
    }

    #[test]
    fn drain_actors_crashed_during_drain_reports_crashed() {
        let _guard = crate::runtime_test_guard();
        let _scheduler = NativeSchedulerGuard::new();

        DRAIN_TRAP_ON_STOP_STARTED.store(false, Ordering::Release);
        DRAIN_TRAP_ON_STOP_RELEASE.store(false, Ordering::Release);

        // SAFETY: null state + valid dispatch are valid spawn args.
        let actor =
            unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(drain_trap_on_stop_dispatch)) };
        assert!(!actor.is_null());
        // SAFETY: actor remains live until the assertions below finish.
        let actor_id = unsafe { (*actor).id };

        // SAFETY: actor is a valid live actor pointer returned by spawn.
        unsafe { hew_actor_send(actor, 1, ptr::null_mut(), 0) };
        assert!(
            wait_for_condition(std::time::Duration::from_secs(1), || {
                DRAIN_TRAP_ON_STOP_STARTED.load(Ordering::Acquire)
            }),
            "trap-on-stop actor should begin running before drain starts"
        );

        // Release the dispatch spin only once drain_actors has actually called
        // hew_actor_stop AND the shutdown system message is enqueued, observed
        // via the mailbox system-queue length becoming non-zero. `sys_count` is
        // incremented *after* the stop node is published to the queue (see
        // enqueue_sys_node), so a non-zero length means the next mailbox poll
        // will deliver the stop and the actor takes Running→Crashed (the trap
        // fires on stop) rather than Running→Idle→Stopped. Waiting on this real
        // condition removes the timing bet: under load a fixed sleep could
        // elapse before drain reached stop, releasing the dispatch while the
        // actor was still Idle-bound and yielding Drained.
        //
        // SAFETY: the actor and its mailbox outlive the joined release thread.
        let mailbox_addr = unsafe { (*actor).mailbox } as usize;
        let release_handle = std::thread::spawn(move || {
            let mb = mailbox_addr as *const HewMailbox;
            let deadline = std::time::Instant::now() + std::time::Duration::from_secs(5);
            loop {
                // SAFETY: `mb` is the live actor's mailbox; it stays valid until
                // the test joins this thread and frees the actor below.
                let stop_queued = !mb.is_null() && unsafe { mailbox::hew_mailbox_sys_len(mb) > 0 };
                if stop_queued || std::time::Instant::now() >= deadline {
                    break;
                }
                std::hint::spin_loop();
                std::thread::sleep(std::time::Duration::from_millis(1));
            }
            DRAIN_TRAP_ON_STOP_RELEASE.store(true, Ordering::Release);
        });

        let outcome = drain_actors(
            &[actor_id],
            std::time::Instant::now() + std::time::Duration::from_secs(2),
        );

        release_handle
            .join()
            .expect("release thread should not panic");

        assert_eq!(
            outcome,
            DrainOutcome::Incomplete {
                still_live: Vec::new(),
                crashed: vec![actor_id],
            }
        );
        assert!(
            is_actor_live(actor),
            "crashed actors should remain tracked for caller-directed cleanup"
        );
        // SAFETY: crashed actors remain tracked until the explicit free below.
        let actor_state = unsafe { (*actor).actor_state.load(Ordering::Acquire) };
        assert_eq!(actor_state, HewActorState::Crashed as i32);
        // SAFETY: crashed actors are quiescent and can be explicitly freed.
        let free_rc = unsafe { hew_actor_free(actor) };
        assert_eq!(free_rc, 0);
    }

    #[test]
    fn drain_actors_with_pending_timer_cancels_timer() {
        // Pin the canonical ordering: when an actor with a registered
        // periodic timer is drained, the timer must be cancelled before
        // the actor is freed.
        let _guard = crate::runtime_test_guard();
        let _scheduler = NativeSchedulerGuard::new();
        let _ticker_guard = crate::timer_periodic::TICKER_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);

        // SAFETY: null state + valid dispatch are valid spawn args.
        let actor = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
        assert!(!actor.is_null());
        // SAFETY: the spawned actor remains live until the assertions below finish.
        let actor_id = unsafe { (*actor).id };

        // SAFETY: actor is a valid live actor pointer returned by spawn.
        let timer = unsafe { crate::timer_periodic::hew_actor_schedule_periodic(actor, 7, 100) };
        assert!(
            !timer.is_null(),
            "periodic timer should register successfully"
        );
        assert_eq!(crate::timer_periodic::timer_count_for_actor(actor), 1);

        let outcome = drain_actors(
            &[actor_id],
            std::time::Instant::now() + std::time::Duration::from_secs(1),
        );
        assert_eq!(outcome, DrainOutcome::Drained);
        assert!(
            !is_actor_live(actor),
            "drained actor should be removed from live tracking"
        );
        assert_eq!(
            crate::timer_periodic::timer_count_for_actor(actor),
            0,
            "drain must cancel pending periodic timers"
        );
    }

    #[test]
    fn drain_actors_with_active_link_removes_link() {
        // Pin the canonical ordering: draining an actor with active link
        // entries must drop both sides of the link before the actor is
        // freed. This guards against teardown paths that skipped link
        // cleanup and left dangling references.
        let _guard = crate::runtime_test_guard();
        let _scheduler = NativeSchedulerGuard::new();

        // SAFETY: null state + valid dispatch are valid spawn args.
        let actor_one = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
        // SAFETY: null state + valid dispatch are valid spawn args.
        let actor_two = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
        assert!(!actor_one.is_null());
        assert!(!actor_two.is_null());

        // SAFETY: spawned actors remain live until the assertions below finish.
        let actor_one_id = unsafe { (*actor_one).id };
        // SAFETY: spawned actors remain live until the assertions below finish.
        let actor_two_id = unsafe { (*actor_two).id };

        // SAFETY: both actor pointers were returned by spawn and are still live.
        unsafe {
            crate::link::hew_actor_link(actor_one, actor_two);
        }
        assert!(crate::link::has_links_for_actor(actor_one_id, actor_one));
        assert!(crate::link::has_links_for_actor(actor_two_id, actor_two));

        // Drain only `actor_one`. The peer side of the link must be cleared
        // even though `actor_two` is being drained in the same batch.
        let outcome = drain_actors(
            &[actor_one_id, actor_two_id],
            std::time::Instant::now() + std::time::Duration::from_secs(1),
        );
        assert_eq!(outcome, DrainOutcome::Drained);
        assert!(!is_actor_live(actor_one));
        assert!(!is_actor_live(actor_two));
        assert!(
            !crate::link::has_links_for_actor(actor_one_id, actor_one),
            "drain must remove links owned by drained actors"
        );
        assert!(
            !crate::link::has_links_for_actor(actor_two_id, actor_two),
            "drain must remove links owned by drained actors"
        );
    }

    #[test]
    fn drain_actors_with_active_monitor_removes_monitor() {
        // Pin the canonical ordering: draining an actor that is being monitored
        // must remove both the monitored and the observer side of the monitor
        // entry before the actors are freed. This guards against teardown paths
        // that skipped monitor cleanup and left dangling references.
        let _guard = crate::runtime_test_guard();
        let _scheduler = NativeSchedulerGuard::new();

        // SAFETY: null state + valid dispatch are valid spawn args.
        let monitored = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
        // SAFETY: null state + valid dispatch are valid spawn args.
        let observer = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
        assert!(!monitored.is_null());
        assert!(!observer.is_null());

        // SAFETY: spawned actors remain live until the assertions below finish.
        let monitored_id = unsafe { (*monitored).id };
        // SAFETY: spawned actors remain live until the assertions below finish.
        let observer_id = unsafe { (*observer).id };

        // Register `observer` as a monitor of `monitored`.
        // SAFETY: both actor pointers were returned by spawn and are still live.
        let monitor_ref = unsafe {
            crate::monitor::register_actor_monitor(observer, monitored)
                .expect("monitor registration")
        };
        assert_ne!(monitor_ref, 0, "monitor registration should succeed");
        assert!(
            crate::monitor::has_monitors_for_actor(monitored_id, monitored),
            "monitored actor should have a monitor entry"
        );
        assert!(
            crate::monitor::has_monitors_for_actor(observer_id, observer),
            "observer actor should have a monitor entry"
        );

        let outcome = drain_actors(
            &[monitored_id, observer_id],
            std::time::Instant::now() + std::time::Duration::from_secs(1),
        );
        assert_eq!(outcome, DrainOutcome::Drained);
        assert!(!is_actor_live(monitored));
        assert!(!is_actor_live(observer));
        assert!(
            !crate::monitor::has_monitors_for_actor(monitored_id, monitored),
            "drain must remove monitor entries owned by the monitored actor"
        );
        assert!(
            !crate::monitor::has_monitors_for_actor(observer_id, observer),
            "drain must remove monitor entries owned by the observer actor"
        );
    }

    #[test]
    fn drain_actors_empty_set_returns_drained() {
        assert_eq!(
            drain_actors(&[], std::time::Instant::now()),
            DrainOutcome::Drained
        );
    }

    #[test]
    fn deep_copy_state_copies_data_correctly() {
        let src: [u8; 4] = [0xDE, 0xAD, 0xBE, 0xEF];
        // SAFETY: src is a valid 4-byte buffer.
        let dst = unsafe { deep_copy_state(src.as_ptr().cast_mut().cast(), 4) };
        assert!(!dst.is_null());
        // SAFETY: dst is a freshly-allocated 4-byte buffer.
        let copied = unsafe { std::slice::from_raw_parts(dst.cast::<u8>(), 4) };
        assert_eq!(copied, &src);
        // SAFETY: dst was allocated with libc::malloc.
        unsafe { libc::free(dst) };
    }

    #[test]
    fn deep_copy_state_null_source_returns_null() {
        // SAFETY: null source is explicitly handled.
        let dst = unsafe { deep_copy_state(ptr::null_mut(), 64) };
        assert!(dst.is_null());
        // No error should be set for a legitimate null/zero call.
        assert!(crate::hew_last_error().is_null());
    }

    #[test]
    fn deep_copy_state_zero_size_returns_null() {
        let src: u8 = 42;
        // SAFETY: src is valid; size=0 triggers the early return.
        let dst = unsafe { deep_copy_state(std::ptr::from_ref(&src).cast_mut().cast(), 0) };
        assert!(dst.is_null());
    }

    #[test]
    fn hew_actor_set_state_drop_records_callback_pointer() {
        // Verify the field roundtrip: setter stores the function pointer and
        // a subsequent read sees the same address. This is the four-touch
        // counterpart of `terminate_fn`'s setter and uses the same shape.
        unsafe extern "C" fn dummy_state_drop(_state: *mut c_void) {}

        let (actor, mailbox) = make_stop_test_actor(HewActorState::Idle);
        // SAFETY: actor is freshly built and not published; setter is the only
        // writer.
        unsafe {
            assert!(
                (*actor).state_drop_fn.is_none(),
                "state_drop_fn must default to None"
            );
            hew_actor_set_state_drop(actor, dummy_state_drop);
            let stored = (*actor).state_drop_fn.expect("setter must populate slot");
            assert_eq!(
                stored as *const () as usize, dummy_state_drop as *const () as usize,
                "stored callback pointer must match the one passed to the setter"
            );
        }
        // SAFETY: actor and mailbox were allocated above and never published.
        unsafe {
            drop(Box::from_raw(actor));
            mailbox::hew_mailbox_free(mailbox);
        }
    }

    static CRASH_SKIP_STATE_DROP_COUNT: std::sync::atomic::AtomicUsize =
        std::sync::atomic::AtomicUsize::new(0);

    unsafe extern "C" fn crash_skip_state_drop_callback(_state: *mut c_void) {
        CRASH_SKIP_STATE_DROP_COUNT.fetch_add(1, Ordering::SeqCst);
    }

    #[test]
    fn free_actor_resources_skips_state_drop_on_crashed_actor() {
        // Crashed actors may have partially torn-down state; running user
        // Drop on it risks a second crash or a double-free of fields the
        // crash already released. The free path must skip the state-drop
        // callback when actor_state is Crashed but still run structural
        // cleanup (libc::free, arena, mailbox, Box::from_raw) so the
        // allocation is not leaked.
        let _guard = crate::runtime_test_guard();
        CRASH_SKIP_STATE_DROP_COUNT.store(0, Ordering::SeqCst);

        // Spawn with a malloc'd source so the resulting actor has a
        // non-null `state` field (deep-copied). This ensures the
        // state-drop call is only suppressed by the actor_state ==
        // Crashed check, not by the inner is_null guard.
        // SAFETY: malloc returns a valid 8-byte allocation or null.
        let src = unsafe { libc::malloc(8) };
        assert!(!src.is_null());
        // SAFETY: spawn deep-copies the bytes; src is freed below.
        let actor = unsafe { hew_actor_spawn(src, 8, Some(noop_dispatch)) };
        assert!(!actor.is_null());
        // SAFETY: spawn copied the bytes; release the source allocation.
        unsafe { libc::free(src) };

        // SAFETY: actor is valid and not being dispatched.
        unsafe {
            hew_actor_set_state_drop(actor, crash_skip_state_drop_callback);
            let a = &*actor;
            assert!(!a.state.is_null(), "spawn must produce a non-null state");
            a.actor_state
                .store(HewActorState::Crashed as i32, Ordering::Release);

            // Go through the public hew_actor_free entry point so the
            // LIVE_ACTORS untracking, timer cancellation, and link/monitor
            // teardown all fire in the order the runtime expects. The
            // crash-skip lives in free_actor_resources, which
            // hew_actor_free calls after the prerequisites above.
            let rc = hew_actor_free(actor);
            assert_eq!(rc, 0);
        }

        assert_eq!(
            CRASH_SKIP_STATE_DROP_COUNT.load(Ordering::SeqCst),
            0,
            "state-drop callback must not run on a Crashed actor"
        );
    }

    #[test]
    fn free_actor_resources_runs_state_drop_on_stopped_actor() {
        // Companion to free_actor_resources_skips_state_drop_on_crashed_actor:
        // a non-Crashed actor MUST still see its state-drop callback fire.
        // Pins the negative case so the crash-skip guard cannot regress to
        // an unconditional skip.
        let _guard = crate::runtime_test_guard();
        CRASH_SKIP_STATE_DROP_COUNT.store(0, Ordering::SeqCst);

        // SAFETY: malloc returns a valid 8-byte allocation or null.
        let src = unsafe { libc::malloc(8) };
        assert!(!src.is_null());
        // SAFETY: spawn deep-copies the bytes; src is freed below.
        let actor = unsafe { hew_actor_spawn(src, 8, Some(noop_dispatch)) };
        assert!(!actor.is_null());
        // SAFETY: spawn copied the bytes; release the source allocation.
        unsafe { libc::free(src) };

        // SAFETY: actor is valid and not being dispatched.
        unsafe {
            hew_actor_set_state_drop(actor, crash_skip_state_drop_callback);
            let a = &*actor;
            assert!(!a.state.is_null(), "spawn must produce a non-null state");
            a.actor_state
                .store(HewActorState::Stopped as i32, Ordering::Release);

            let rc = hew_actor_free(actor);
            assert_eq!(rc, 0);
        }

        assert_eq!(
            CRASH_SKIP_STATE_DROP_COUNT.load(Ordering::SeqCst),
            1,
            "state-drop callback must fire exactly once on a Stopped actor"
        );
    }

    /// C1 leak probe (D-C1): freeing a never-woken `Suspended` actor destroys
    /// its parked continuation exactly once on the free path — the frame-owned
    /// heap value (`heap_guard`) does NOT leak. The scratch frame's destroy
    /// outline frees `heap_guard` and bumps `destroyed`; asserting `destroyed ==
    /// 1` proves the C1 teardown ran, and the freed `heap_guard` is what
    /// `MallocScribble` / `leaks --atExit` accounts for in the exec probe.
    ///
    /// Bite-proof: WITHOUT the free-path destroy the `destroyed` counter would
    /// stay 0 (and `heap_guard` would leak) — so this assertion fails closed if
    /// the C1 wiring regresses. `scratch_destroy` frees only `heap_guard`, not
    /// the frame struct, so the test reclaims the frame box afterward (no test
    /// leak).
    #[test]
    fn free_path_destroys_parked_continuation_c1() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: spawn a real actor (null state / size 0 is documented legal).
        let actor = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
        assert!(!actor.is_null());

        // Park a scratch continuation, as a never-woken suspended dispatch
        // would: publish Parked + store the handle, then mark the actor
        // Suspended. The scratch frame owns a real heap_guard allocation the
        // destroy outline must free.
        let frame = Box::new(crate::coro_exec::test_support::ScratchFrame::new(4));
        let handle = Box::into_raw(frame).cast::<c_void>();
        // SAFETY: actor is live and owned by this test thread.
        unsafe {
            let a = &*actor;
            assert!(crate::coro_exec::begin_park(a).is_ok());
            crate::coro_exec::finish_park(a, handle);
            a.actor_state
                .store(HewActorState::Suspended as i32, Ordering::Release);
            assert!(
                crate::coro_exec::has_live_parked_cont(a),
                "the actor now owns a live parked continuation"
            );
        }

        // Free the actor WITHOUT ever waking the continuation. The C1 free-path
        // teardown must destroy the parked frame exactly once before reclaiming
        // the box (which frees heap_guard via the scratch destroy outline).
        // SAFETY: actor is valid and not being dispatched.
        let rc = unsafe { hew_actor_free(actor) };
        assert_eq!(rc, 0);

        // Reclaim the scratch frame struct (scratch_destroy freed only its
        // heap_guard, not the frame) and assert the destroy outline ran exactly
        // once on the free path.
        // SAFETY: `handle` is the scratch frame `Box::into_raw`'d above; its
        // struct memory is still valid (scratch_destroy frees only heap_guard).
        let frame =
            unsafe { Box::from_raw(handle.cast::<crate::coro_exec::test_support::ScratchFrame>()) };
        assert_eq!(
            frame.destroyed.load(Ordering::Acquire),
            1,
            "C1: the parked continuation is destroyed exactly once on the free path"
        );
        assert!(
            frame.heap_guard.load(Ordering::Acquire).is_null(),
            "the frame-owned heap value was freed by the destroy outline (no leak)"
        );
    }

    #[test]
    fn hew_actor_free_for_restart_skips_state_drop_on_stopped_sibling() {
        // Supervisor restart paths (ONE_FOR_ALL / REST_FOR_ONE) free
        // non-crashed siblings before re-spawning them from
        // `spec.init_state`. The runtime byte-copies `spec.init_state` into
        // every actor's `state` and `init_state` slots, so the field
        // pointers (Vec.ptr, String.ptr, IO handles) inside `spec.init_state`
        // are byte-aliased by every spawned actor. Running `state_drop_fn`
        // on a stopped sibling would free those field pointers, leaving
        // `spec.init_state` byte-aliasing dangling pointers; the next
        // restart byte-copies that dangling spec into the new actor and
        // dereferences it on the next teardown — a use-after-free.
        //
        // This test pins the contract: `hew_actor_free_for_restart` MUST
        // NOT invoke the codegen-emitted `state_drop_fn` on a non-crashed
        // (Stopped/Idle) actor, even when that callback is registered.
        //
        // Companion to `free_actor_resources_runs_state_drop_on_stopped_actor`,
        // which pins the inverse: the regular `hew_actor_free` entry point
        // MUST run the callback on a Stopped actor.
        let _guard = crate::runtime_test_guard();
        CRASH_SKIP_STATE_DROP_COUNT.store(0, Ordering::SeqCst);

        // SAFETY: malloc returns a valid 8-byte allocation or null.
        let src = unsafe { libc::malloc(8) };
        assert!(!src.is_null());
        // SAFETY: spawn deep-copies the bytes; src is freed below.
        let actor = unsafe { hew_actor_spawn(src, 8, Some(noop_dispatch)) };
        assert!(!actor.is_null());
        // SAFETY: spawn copied the bytes; release the source allocation.
        unsafe { libc::free(src) };

        // SAFETY: actor is valid and not being dispatched.
        unsafe {
            hew_actor_set_state_drop(actor, crash_skip_state_drop_callback);
            let a = &*actor;
            assert!(!a.state.is_null(), "spawn must produce a non-null state");
            a.actor_state
                .store(HewActorState::Stopped as i32, Ordering::Release);

            // Drive the supervisor-restart-aware free path. Even though the
            // actor is Stopped (not Crashed), the state-drop callback must
            // be suppressed because the supervisor's `spec.init_state`
            // still byte-aliases this actor's field pointers.
            let rc = hew_actor_free_for_restart(actor);
            assert_eq!(rc, 0);
        }

        assert_eq!(
            CRASH_SKIP_STATE_DROP_COUNT.load(Ordering::SeqCst),
            0,
            "state-drop callback must NOT run on a Stopped sibling freed via the restart-aware path"
        );
    }

    #[test]
    fn hew_actor_set_state_drop_null_actor_is_noop() {
        // Spawn returns null on allocation failure; codegen unconditionally
        // calls this setter, so it must tolerate a null receiver without
        // dereferencing. Verifies the cabi_guard short-circuit.
        unsafe extern "C" fn dummy_state_drop(_state: *mut c_void) {}
        // SAFETY: passing null is exactly what we are guarding against; the
        // function must return without touching the pointer.
        unsafe { hew_actor_set_state_drop(std::ptr::null_mut(), dummy_state_drop) };
    }

    #[test]
    fn deep_copy_state_alloc_failure_returns_null_and_sets_error() {
        let _guard = crate::runtime_test_guard();
        let src: u8 = 1;
        crate::hew_clear_error();
        let _guard = fail_actor_state_alloc_on_nth(0);
        // SAFETY: src is valid; allocation failure is injected by the test.
        let dst = unsafe { deep_copy_state(std::ptr::from_ref(&src).cast_mut().cast(), 1) };
        assert!(dst.is_null(), "should return null on allocation failure");
        let err = crate::hew_last_error();
        assert!(!err.is_null(), "hew_last_error should be set after OOM");
        // SAFETY: hew_last_error returned a non-null C string.
        let msg = unsafe { std::ffi::CStr::from_ptr(err) }.to_string_lossy();
        assert!(
            msg.contains("OOM"),
            "error message should mention OOM, got: {msg}"
        );
    }

    static TERMINATE_CALL_COUNT: std::sync::atomic::AtomicUsize =
        std::sync::atomic::AtomicUsize::new(0);

    unsafe extern "C" fn counting_terminate_callback(_state: *mut c_void) {
        TERMINATE_CALL_COUNT.fetch_add(1, Ordering::SeqCst);
    }

    #[test]
    fn terminate_fires_on_normal_stop_and_not_on_crash() {
        // LESSONS: cleanup-all-exits (P0) — on(stop) must run at normal actor
        // teardown (finalize_quiescent_actor_cleanup_with_options) but must NOT
        // run when the actor is in the Crashed state (same path guards
        // state_drop_fn). Pins the crash-skip invariant and the normal-stop
        // fire invariant with a minimal in-process test.
        //
        // Both actors spawn with a non-null state (8-byte malloc) so
        // call_terminate_fn does not bail out at the null-state early-return.
        let _guard = crate::runtime_test_guard();
        TERMINATE_CALL_COUNT.store(0, Ordering::SeqCst);

        // --- normal-stop path: terminate_fn must fire ---
        // SAFETY: malloc returns a valid 8-byte allocation or null; freed below.
        let src = unsafe { libc::malloc(8) };
        assert!(!src.is_null());
        // SAFETY: spawn deep-copies the 8 bytes.
        let stopped_actor = unsafe { hew_actor_spawn(src, 8, Some(noop_dispatch)) };
        assert!(!stopped_actor.is_null());
        // SAFETY: spawn copied the bytes; release the source.
        unsafe { libc::free(src) };

        // SAFETY: actor is valid; terminate not yet called.
        unsafe {
            hew_actor_set_terminate(stopped_actor, counting_terminate_callback);
            let a = &*stopped_actor;
            a.actor_state
                .store(HewActorState::Stopped as i32, Ordering::Release);
            let rc = hew_actor_free(stopped_actor);
            assert_eq!(rc, 0, "hew_actor_free on stopped actor must succeed");
        }
        assert_eq!(
            TERMINATE_CALL_COUNT.load(Ordering::SeqCst),
            1,
            "terminate callback must fire exactly once for a Stopped actor"
        );

        // --- crash path: terminate_fn must NOT fire ---
        TERMINATE_CALL_COUNT.store(0, Ordering::SeqCst);
        // SAFETY: malloc returns a valid 8-byte allocation or null; freed below.
        let src = unsafe { libc::malloc(8) };
        assert!(!src.is_null());
        // SAFETY: spawn deep-copies the bytes.
        let crashed_actor = unsafe { hew_actor_spawn(src, 8, Some(noop_dispatch)) };
        assert!(!crashed_actor.is_null());
        // SAFETY: spawn copied the bytes; release the source.
        unsafe { libc::free(src) };

        // SAFETY: actor is valid; terminate registered but must not run on crash.
        unsafe {
            hew_actor_set_terminate(crashed_actor, counting_terminate_callback);
            let a = &*crashed_actor;
            a.actor_state
                .store(HewActorState::Crashed as i32, Ordering::Release);
            let rc = hew_actor_free(crashed_actor);
            assert_eq!(rc, 0, "hew_actor_free on crashed actor must succeed");
        }
        assert_eq!(
            TERMINATE_CALL_COUNT.load(Ordering::SeqCst),
            0,
            "terminate callback must NOT fire for a Crashed actor"
        );
    }

    #[test]
    fn free_actor_resources_times_out_on_hanging_terminate() {
        let _guard = crate::runtime_test_guard();
        // Simulate an actor whose terminate_called is true but
        // terminate_finished never becomes true. The bounded wait in
        // free_actor_resources should time out after ~5s and proceed.
        // SAFETY: null state, valid dispatch.
        let actor = unsafe { hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch)) };
        assert!(!actor.is_null());

        // SAFETY: actor is valid.
        let a = unsafe { &*actor };
        // Simulate a hung terminate: called=true, finished=false.
        a.terminate_called.store(true, Ordering::Release);
        a.terminate_finished.store(false, Ordering::Release);
        // Put actor in Stopped state so hew_actor_free doesn't fail the
        // state check.
        a.actor_state
            .store(HewActorState::Stopped as i32, Ordering::Release);

        let start = std::time::Instant::now();
        // SAFETY: actor is valid and in Stopped state.
        let rc = unsafe { hew_actor_free(actor) };
        let elapsed = start.elapsed();

        assert_eq!(rc, 0);
        // Should take roughly 5 seconds (the timeout), not hang forever.
        assert!(
            elapsed >= std::time::Duration::from_secs(4),
            "should wait ~5s before timing out, took {elapsed:?}"
        );
        assert!(
            elapsed < std::time::Duration::from_secs(10),
            "should not hang much longer than the timeout, took {elapsed:?}"
        );
    }

    #[test]
    fn free_current_actor_from_terminate_is_deferred() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: this test fully owns the spawned actor and simulates the
        // terminate callback state on the current thread.
        unsafe {
            let actor = hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch));
            assert!(!actor.is_null());
            let a = &*actor;
            a.actor_state
                .store(HewActorState::Stopped as i32, Ordering::Release);
            a.terminate_called.store(true, Ordering::Release);
            a.terminate_finished.store(false, Ordering::Release);

            let _ctx = TestExecutionContext::install(HewExecutionContext {
                actor,
                actor_id: (*actor).id,
                ..HewExecutionContext::default()
            });

            let start = std::time::Instant::now();
            let rc = hew_actor_free(actor);
            let elapsed = start.elapsed();

            a.terminate_finished.store(true, Ordering::Release);

            let _ = wait_for_condition(std::time::Duration::from_secs(2), || !is_actor_live(actor));

            assert_eq!(rc, 0, "reentrant terminate frees should still succeed");
            assert!(
                elapsed < std::time::Duration::from_secs(1),
                "reentrant free should defer instead of spin-waiting in terminate, took {elapsed:?}"
            );
        }
    }

    #[test]
    fn wasm_free_waits_for_quiescent_actor_state_before_freeing() {
        let _guard = crate::runtime_test_guard();
        let actor = make_tracked_wasm_free_test_actor(HewActorState::Runnable);

        let start = std::time::Instant::now();
        // SAFETY: actor is tracked and owned by this test.
        let rc = unsafe { actor_free_wasm_impl(actor) };
        let elapsed = start.elapsed();

        assert_eq!(rc, -2, "runnable WASM actors must not be freed immediately");
        assert!(
            elapsed >= std::time::Duration::from_secs(1),
            "WASM free should wait for quiescence before timing out, took {elapsed:?}"
        );
        assert!(
            is_actor_live(actor),
            "timed-out WASM free must leave the actor tracked to avoid dangling scheduler pointers"
        );

        // SAFETY: actor remains tracked after the timed-out free attempt.
        unsafe {
            (*actor)
                .actor_state
                .store(HewActorState::Stopped as i32, Ordering::Release);
            assert_eq!(actor_free_wasm_impl(actor), 0);
        }
    }

    #[test]
    fn wasm_free_reports_null_actor_failure_like_native_free() {
        let _guard = crate::runtime_test_guard();
        crate::hew_clear_error();

        // SAFETY: null actor pointer is explicitly rejected by the free path.
        let rc = unsafe { actor_free_wasm_impl(ptr::null_mut()) };

        assert_eq!(
            rc, -1,
            "WASM free should mirror native null-pointer failure"
        );
        let err = crate::hew_last_error();
        assert!(!err.is_null(), "WASM free should populate hew_last_error");
        // SAFETY: hew_last_error returned a non-null C string.
        let msg = unsafe { std::ffi::CStr::from_ptr(err) }.to_string_lossy();
        assert_eq!(msg, "hew_actor_free: null actor pointer");
    }

    #[test]
    fn wasm_free_reports_untracked_actor_failure_like_native_free() {
        let _guard = crate::runtime_test_guard();
        let actor = make_tracked_wasm_free_test_actor(HewActorState::Stopped);
        assert!(
            live_actors::untrack_actor(actor),
            "test precondition: actor should start tracked"
        );
        crate::hew_clear_error();

        // SAFETY: actor remains allocated and owned by this test.
        let rc = unsafe { actor_free_wasm_impl(actor) };

        assert_eq!(rc, -1, "WASM free should mirror native untrack failure");
        let err = crate::hew_last_error();
        assert!(!err.is_null(), "WASM free should populate hew_last_error");
        // SAFETY: hew_last_error returned a non-null C string.
        let msg = unsafe { std::ffi::CStr::from_ptr(err) }.to_string_lossy();
        assert_eq!(msg, "hew_actor_free: actor already freed or not tracked");

        // SAFETY: untrack failure must not free the actor; the test still owns it.
        unsafe { drop(Box::from_raw(actor)) };
    }

    /// `actor_free_wasm_impl` must free the actor's arena when it is non-null.
    ///
    /// The existing WASM free tests use actors with `arena: ptr::null_mut()` and
    /// therefore never enter the `if !a.arena.is_null()` branch in
    /// `free_actor_resources_wasm`.  This test constructs an actor with a live
    /// arena (mirroring what `spawn_actor_internal` on WASM does) and verifies:
    ///
    /// 1. `hew_arena_free_all` was called with **this specific arena's address**
    ///    (via `crate::arena::LAST_FREED_ARENA_ADDR`, a thread-local).  The
    ///    assertion fails if the non-null arena branch is accidentally removed.
    /// 2. `actor_free_wasm_impl` returns 0 (success).
    /// 3. The actor is removed from the live-actor set.
    ///
    /// ## Why this is order-independent under parallel test execution
    ///
    /// `LAST_FREED_ARENA_ADDR` is a **thread-local**, not a global counter.
    /// Tests on other threads update their own copy; only the thread executing
    /// this test touches the local that this test reads.  `actor_free_wasm_impl`
    /// is synchronous, so nothing on this thread can overwrite the value between
    /// the call and the assertion.
    #[test]
    fn wasm_free_with_arena_releases_arena_on_teardown() {
        let _guard = crate::runtime_test_guard();

        // Allocate a real arena exactly as spawn_actor_internal (WASM) does.
        let arena = crate::arena::hew_arena_new();
        assert!(!arena.is_null(), "arena allocation must succeed");
        // Capture the address before transferring ownership to the actor struct.
        let arena_addr = arena as usize;

        let actor_id = crate::pid::next_actor_id(NEXT_ACTOR_SERIAL.fetch_add(1, Ordering::Relaxed));
        let actor = Box::into_raw(Box::new(HewActor {
            sched_link_next: AtomicPtr::new(ptr::null_mut()),
            id: actor_id,
            state: ptr::null_mut(),
            state_size: 0,
            dispatch: Some(noop_dispatch),
            mailbox: ptr::null_mut(),
            actor_state: AtomicI32::new(HewActorState::Stopped as i32),
            budget: AtomicI32::new(HEW_MSG_BUDGET),
            init_state: ptr::null_mut(),
            init_state_size: 0,
            coalesce_key_fn: None,
            terminate_fn: None,
            state_drop_fn: None,
            state_clone_fn: None,
            terminate_called: AtomicBool::new(false),
            terminate_finished: AtomicBool::new(false),
            dispatch_active: AtomicBool::new(false),
            error_code: AtomicI32::new(0),
            supervisor: ptr::null_mut(),
            supervisor_child_index: -1,
            priority: AtomicI32::new(HEW_PRIORITY_NORMAL),
            reductions: AtomicI32::new(HEW_DEFAULT_REDUCTIONS),
            idle_count: AtomicI32::new(0),
            hibernation_threshold: AtomicI32::new(0),
            hibernating: AtomicI32::new(0),
            prof_messages_processed: AtomicU64::new(0),
            prof_processing_time_ns: AtomicU64::new(0),
            // Wire up the real arena — same assignment as spawn_actor_internal (WASM).
            arena,
            suspended_cont: AtomicPtr::new(std::ptr::null_mut()),
            cont_tag: AtomicI32::new(crate::internal::types::ContTag::Empty as i32),
            pending_wake: AtomicBool::new(false),
            suspended_reply_channel: AtomicPtr::new(std::ptr::null_mut()),
            suspended_cancel_token: AtomicPtr::new(std::ptr::null_mut()),
            runtime_id: crate::runtime_id::RuntimeId::DEFAULT,
            runtime: ptr::null(),
            send_pin_count: AtomicU32::new(0),
            gen_sink: AtomicPtr::new(ptr::null_mut()),
            local_pid_id: crate::lifetime::local_handles::HewLocalPidId::INVALID,
        }));
        // SAFETY: actor is fully initialised above with a valid id field.
        assert!(unsafe { live_actors::track_actor(actor) });

        // Zero the thread-local witness immediately before the call under test.
        // Without this, a prior test on the same worker thread that freed an
        // arena at the same address could leave LAST_FREED_ARENA_ADDR == arena_addr
        // before we even reach actor_free_wasm_impl, making the assertion a
        // false-positive if the teardown path is later removed.
        // arena_addr is always non-zero (hew_arena_new is asserted non-null above),
        // so 0 is a safe sentinel: if hew_arena_free_all is never called,
        // the witness stays 0 and the assert_eq below fails.
        crate::arena::LAST_FREED_ARENA_ADDR.with(|c| c.set(0));

        // SAFETY: actor is Box-allocated, tracked, in Stopped state, not dispatching.
        // state / init_state are null (libc::free(null) is a no-op), mailbox is null.
        let rc = unsafe { actor_free_wasm_impl(actor) };

        // Primary assertion: hew_arena_free_all must have been called with exactly
        // this actor's arena address.  LAST_FREED_ARENA_ADDR is thread-local so
        // parallel tests on other threads cannot interfere, and it was zeroed above
        // so stale same-thread state cannot produce a false positive.
        let last_freed = crate::arena::LAST_FREED_ARENA_ADDR.with(std::cell::Cell::get);
        assert_eq!(
            last_freed, arena_addr,
            "free_actor_resources_wasm must call hew_arena_free_all with the actor's own arena"
        );

        assert_eq!(rc, 0, "WASM free with non-null arena must succeed");
        assert!(
            !is_actor_live(actor),
            "freed actor must be removed from the live-actor set"
        );
    }

    #[test]
    fn spawn_with_restart_state_alloc_failure_returns_null_and_sets_error() {
        let _guard = crate::runtime_test_guard();
        let src: u8 = 1;
        crate::hew_clear_error();
        let _guard = fail_actor_state_alloc_on_nth(1);
        // SAFETY: src is valid; allocation failure is injected into the restart-state copy.
        let actor = unsafe {
            hew_actor_spawn(
                std::ptr::from_ref(&src).cast_mut().cast(),
                1,
                Some(noop_dispatch),
            )
        };
        assert!(actor.is_null(), "spawn should return null on OOM");
        let err = crate::hew_last_error();
        assert!(!err.is_null(), "hew_last_error should be set after OOM");
        // SAFETY: hew_last_error returned a non-null C string.
        let msg = unsafe { std::ffi::CStr::from_ptr(err) }.to_string_lossy();
        assert!(
            msg.contains("OOM"),
            "error message should mention OOM, got: {msg}"
        );
    }

    /// Freeing an actor via `hew_actor_free` must remove all parse-error slot
    /// entries for that actor across every parser kind.
    ///
    /// This guards against unbounded growth of the global parse-error map on
    /// long-running nodes that spawn and reap many actors.
    ///
    /// Run 3× to satisfy the flake gate.
    #[test]
    fn hew_actor_free_clears_parse_error_slots() {
        for _run in 0..3 {
            let _guard = crate::runtime_test_guard();

            // SAFETY: null state, valid dispatch.
            let actor = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
            assert!(!actor.is_null());
            // SAFETY: actor is valid — returned by hew_actor_spawn.
            let actor_id = unsafe { (*actor).id };

            // Inject errors for all four error kinds.
            crate::parse_error_slot::__set_error_for_actor(
                actor_id,
                crate::parse_error_slot::ErrorSlotKind::Datetime,
                "datetime error",
            );
            crate::parse_error_slot::__set_error_for_actor(
                actor_id,
                crate::parse_error_slot::ErrorSlotKind::Yaml,
                "yaml error",
            );
            crate::parse_error_slot::__set_error_for_actor(
                actor_id,
                crate::parse_error_slot::ErrorSlotKind::Toml,
                "toml error",
            );
            crate::parse_error_slot::__set_error_for_actor(
                actor_id,
                crate::parse_error_slot::ErrorSlotKind::Json,
                "json error",
            );

            // Verify they are present before free.
            assert!(crate::parse_error_slot::__get_error_for_actor(
                actor_id,
                crate::parse_error_slot::ErrorSlotKind::Datetime
            )
            .is_some());

            // Free the actor — this calls prepare_quiescent_actor_for_cleanup
            // which calls parse_error_slot::clear_all_for_actor.
            // SAFETY: actor is valid and was spawned by hew_actor_spawn above.
            let rc = unsafe { hew_actor_free(actor) };
            assert_eq!(rc, 0, "hew_actor_free must succeed");

            // All four slots must now be empty.
            for kind in [
                crate::parse_error_slot::ErrorSlotKind::Datetime,
                crate::parse_error_slot::ErrorSlotKind::Yaml,
                crate::parse_error_slot::ErrorSlotKind::Toml,
                crate::parse_error_slot::ErrorSlotKind::Json,
            ] {
                assert_eq!(
                    crate::parse_error_slot::__get_error_for_actor(actor_id, kind),
                    None,
                    "error slot for {kind:?} must be cleared after actor free"
                );
            }
        }
    }

    // ── arena_cap_bytes threading via hew_actor_spawn_opts ───────────────

    /// `hew_actor_spawn_opts` with `arena_cap_bytes > 0` spawns an actor whose
    /// arena enforces the cap: the first allocation over the cap returns null.
    #[test]
    fn max_heap_spawn_opts_threads_cap_to_arena() {
        let _guard = crate::runtime_test_guard();

        // Cap: exactly 128 bytes.
        let opts = HewActorOpts {
            init_state: ptr::null_mut(),
            state_size: 0,
            dispatch: Some(noop_dispatch),
            mailbox_capacity: 0,
            overflow: 0,
            coalesce_key_fn: None,
            coalesce_fallback: 0,
            message_drop_fn: None,
            budget: 0,
            arena_cap_bytes: 128,
            cycle_capable: 0,
        };

        // SAFETY: opts is valid for the duration of the call.
        let actor = unsafe { hew_actor_spawn_opts(&raw const opts) };
        assert!(
            !actor.is_null(),
            "spawn with arena_cap_bytes=128 must succeed"
        );

        // Verify the arena cap was set: install the actor's arena, attempt to
        // alloc 129 bytes (one over cap), and assert it returns null.
        // SAFETY: actor is valid; arena pointer comes from the actor struct.
        let arena = unsafe { (*actor).arena };
        assert!(!arena.is_null(), "actor arena must be allocated");
        // SAFETY: actor is live for the duration of this test.
        let actor_id = unsafe { (*actor).id };
        let _ctx = TestExecutionContext::install(HewExecutionContext {
            actor,
            actor_id,
            ..HewExecutionContext::default()
        });

        // Install the arena lane so hew_arena_malloc routes through it.
        // SAFETY: arena is a valid pointer from hew_arena_new_with_cap.
        unsafe { crate::arena::hew_arena_set_current(arena) };

        // Allocate up to the cap: 128 bytes in a single call.
        // SAFETY: arena is installed and valid.
        let p = unsafe { crate::arena::hew_arena_malloc(128) };
        assert!(!p.is_null(), "128-byte alloc at cap must succeed");

        // Now exceed the cap: one more byte should return null.
        // SAFETY: arena is still installed.
        let over = unsafe { crate::arena::hew_arena_malloc(1) };
        assert!(
            over.is_null(),
            "alloc over arena cap must return null (HeapExceeded path)"
        );

        // Restore no-arena state before teardown.
        // SAFETY: null restores no-arena state.
        unsafe { crate::arena::hew_arena_set_current(ptr::null_mut()) };

        // SAFETY: actor is valid and was spawned above.
        let rc = unsafe { hew_actor_free(actor) };
        assert_eq!(rc, 0, "hew_actor_free must succeed");
    }

    /// `hew_actor_spawn_opts` with `arena_cap_bytes = 0` spawns an actor with
    /// an unbounded arena (same as legacy `hew_arena_new`).
    #[test]
    fn max_heap_spawn_opts_zero_cap_is_unbounded() {
        let _guard = crate::runtime_test_guard();

        let opts = HewActorOpts {
            init_state: ptr::null_mut(),
            state_size: 0,
            dispatch: Some(noop_dispatch),
            mailbox_capacity: 0,
            overflow: 0,
            coalesce_key_fn: None,
            coalesce_fallback: 0,
            message_drop_fn: None,
            budget: 0,
            arena_cap_bytes: 0,
            cycle_capable: 0,
        };

        // SAFETY: opts is valid for the duration of the call.
        let actor = unsafe { hew_actor_spawn_opts(&raw const opts) };
        assert!(
            !actor.is_null(),
            "spawn with arena_cap_bytes=0 must succeed"
        );

        // SAFETY: actor is valid; arena pointer comes from the actor struct.
        let arena = unsafe { (*actor).arena };
        assert!(!arena.is_null(), "actor arena must be allocated");
        // SAFETY: actor is live for the duration of this test.
        let actor_id = unsafe { (*actor).id };
        let _ctx = TestExecutionContext::install(HewExecutionContext {
            actor,
            actor_id,
            ..HewExecutionContext::default()
        });

        // Install the arena and alloc a large block — must succeed (unbounded).
        // SAFETY: arena is a valid pointer from hew_arena_new.
        unsafe { crate::arena::hew_arena_set_current(arena) };

        // SAFETY: arena is installed.
        let p = unsafe { crate::arena::hew_arena_malloc(65536) };
        assert!(!p.is_null(), "64 KiB alloc in unbounded arena must succeed");

        // SAFETY: null restores no-arena state.
        unsafe { crate::arena::hew_arena_set_current(ptr::null_mut()) };

        // SAFETY: actor is valid.
        let rc = unsafe { hew_actor_free(actor) };
        assert_eq!(rc, 0, "hew_actor_free must succeed");
    }

    // ── null-arena guard: backport of wasm OOM behaviour to native path ───

    /// `hew_actor_spawn` must return null and release all owned resources
    /// (`state`, `init_state` copy, mailbox) when `hew_arena_new` fails (OOM).
    ///
    /// Covers the native `spawn_actor_internal` null-arena guard introduced
    /// to match the wasm twin's existing OOM handling.  The "no-leak" half is
    /// enforced by ASAN in CI; the "no null-arena actor" half is enforced by
    /// the null return asserted here (a non-null return from the broken pre-fix
    /// code would carry `actor.arena = null` and crash on the first arena alloc).
    #[test]
    fn spawn_arena_alloc_failure_returns_null() {
        let _guard = crate::runtime_test_guard();

        // ── case 1: zero-size state (init_state is null on the spawn path) ──
        // Arena fails → cleanup_failed_spawn frees the (empty) state + mailbox.
        crate::hew_clear_error();
        let _arena_guard = fail_arena_alloc_next();
        // SAFETY: null state with size=0 is valid; dispatch is a valid fn ptr.
        let actor = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
        assert!(
            actor.is_null(),
            "spawn must return null when arena allocation fails (zero-state path)"
        );
        // Guard is consumed; injection is disarmed.

        // ── case 2: non-null state (init_state is allocated then freed) ──
        // `deep_copy_state` succeeds twice (state copy + init_state copy);
        // arena fails → cleanup_failed_spawn frees both copies + mailbox.
        let src: [u8; 8] = [0xA1, 0xB2, 0xC3, 0xD4, 0xE5, 0xF6, 0x07, 0x18];
        crate::hew_clear_error();
        let _arena_guard = fail_arena_alloc_next();
        // SAFETY: src is a valid 8-byte readable buffer; dispatch is valid.
        let actor = unsafe {
            hew_actor_spawn(
                src.as_ptr().cast_mut().cast(),
                src.len(),
                Some(noop_dispatch),
            )
        };
        assert!(
            actor.is_null(),
            "spawn must return null when arena allocation fails (with-state path)"
        );

        // ── case 3: normal spawn succeeds immediately after ──
        // Verifies the injection is fully disarmed and the runtime is intact.
        // SAFETY: null state with size=0 is valid.
        let ok_actor = unsafe { hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
        assert!(
            !ok_actor.is_null(),
            "normal spawn after failed arena alloc must succeed"
        );
        // SAFETY: ok_actor is a valid pointer from hew_actor_spawn.
        let rc = unsafe { hew_actor_free(ok_actor) };
        assert_eq!(rc, 0, "hew_actor_free on the recovery actor must succeed");
    }

    // ── gen_sink CAS-race double-free regression (PR #2401 finding) ──────
    //
    // `hew_actor_gen_sink_complete` (the pump's clean-exit release) and
    // `fault_close_registered_gen_sink` (the crash/teardown release) both
    // race on the same `AtomicPtr` slot when a terminal teardown fires
    // concurrently with the pump's own generator-exhausted exit. Before the
    // fix, `hew_actor_gen_sink_complete` discarded its CAS result and
    // unconditionally called `hew_sink_close(sink)` even when it LOST the
    // race — double-freeing the same `Box<HewSink>` the fault path had
    // already closed. These tests deterministically force each ordering
    // (no real thread race needed) by calling the two release paths
    // back-to-back on a single actor + sink pair.

    #[test]
    fn gen_sink_complete_is_noop_when_fault_close_already_won_the_race() {
        let _guard = crate::runtime_test_guard();
        let actor = make_tracked_wasm_free_test_actor(HewActorState::Runnable);
        // SAFETY: hew_stream_channel returns a valid pair; hew_stream_pair_sink
        // extracts its live sink half.
        let sink = unsafe {
            let pair = crate::stream::hew_stream_channel(1);
            let sink = crate::stream::hew_stream_pair_sink(pair);
            // The pair's own stream (consumer) half is unused by this test;
            // close it so the channel's other end doesn't linger.
            crate::stream::hew_stream_close(crate::stream::hew_stream_pair_stream(pair));
            crate::stream::hew_stream_pair_free(pair);
            sink
        };

        // SAFETY: actor is a live, freshly built test actor.
        unsafe { hew_actor_gen_sink_register(actor, sink) };

        // Simulate the fault-close teardown path winning the race first:
        // it swaps the slot to null and closes/frees `sink` via
        // `fault_close_registered_sink`.
        // SAFETY: actor owns a registered sink; this is the crash/teardown
        // release path exercised directly instead of through a real crash.
        unsafe { fault_close_registered_gen_sink(&*actor) };
        // SAFETY: actor remains valid; only its gen_sink slot was touched
        // above.
        let slot_after_fault_close = unsafe { (*actor).gen_sink.load(Ordering::Acquire) };
        assert!(
            slot_after_fault_close.is_null(),
            "fault-close must leave the slot null after winning"
        );

        // The pump's own clean-exit release now runs on the SAME `sink`
        // pointer, having lost the race (the slot the pump's CAS is looking
        // for is already null, not `sink`). Before the fix this called
        // `hew_sink_close(sink)` unconditionally here, double-freeing the
        // allocation `fault_close_registered_gen_sink` already freed above.
        // A double-free is UB and not guaranteed to crash without ASan, so
        // the meaningful assertion is the ABSENCE of a second free: verified
        // separately below by checking the loser makes no further slot
        // mutation and by running this exact test under AddressSanitizer
        // (see PR description / heartbeat verification notes), where the
        // pre-fix code aborts with a confirmed heap-use-after-free /
        // double-free and the fixed code does not.
        // SAFETY: sink was already freed by the fault-close call above; the
        // fixed implementation must detect the lost CAS and return without
        // dereferencing or freeing `sink` again.
        unsafe { hew_actor_gen_sink_complete(actor, sink) };

        // SAFETY: actor is tracked and owned by this test.
        unsafe {
            live_actors::untrack_actor(actor);
            drop(Box::from_raw(actor));
        }
    }

    #[test]
    fn gen_sink_complete_frees_normally_when_it_wins_the_race() {
        let _guard = crate::runtime_test_guard();
        let actor = make_tracked_wasm_free_test_actor(HewActorState::Runnable);
        // SAFETY: hew_stream_channel returns a valid pair; hew_stream_pair_sink
        // extracts its live sink half.
        let sink = unsafe {
            let pair = crate::stream::hew_stream_channel(1);
            let sink = crate::stream::hew_stream_pair_sink(pair);
            crate::stream::hew_stream_close(crate::stream::hew_stream_pair_stream(pair));
            crate::stream::hew_stream_pair_free(pair);
            sink
        };

        // SAFETY: actor is a live, freshly built test actor.
        unsafe { hew_actor_gen_sink_register(actor, sink) };

        // No concurrent fault-close this time: the pump's own clean-exit
        // release runs uncontested and must win its CAS, then free `sink`
        // exactly once (the pre-existing, still-correct behaviour).
        // SAFETY: actor owns a registered sink that nothing else has touched.
        unsafe { hew_actor_gen_sink_complete(actor, sink) };

        // SAFETY: actor is tracked and owned by this test.
        unsafe {
            assert!(
                (*actor).gen_sink.load(Ordering::Acquire).is_null(),
                "a winning clean-exit release must still clear the slot"
            );
            live_actors::untrack_actor(actor);
            drop(Box::from_raw(actor));
        }
    }
}

#[cfg(all(test, target_arch = "wasm32"))]
mod wasm_tests {
    use super::*;

    unsafe extern "C-unwind" fn self_stop_without_reply_dispatch(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        hew_actor_self_stop();

        std::ptr::null_mut()
    }

    unsafe extern "C-unwind" fn reply_once_dispatch(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        let ch = crate::scheduler_wasm::hew_get_reply_channel();
        let mut value: i32 = 21;
        unsafe {
            let _ = crate::reply_channel_wasm::hew_reply(
                ch.cast(),
                (&raw mut value).cast(),
                size_of::<i32>(),
            );
        }

        std::ptr::null_mut()
    }

    unsafe extern "C-unwind" fn late_reply_dispatch(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        std::thread::sleep(std::time::Duration::from_millis(20));
        let ch = crate::scheduler_wasm::hew_get_reply_channel();
        let mut value: i32 = 99;
        unsafe {
            let _ = crate::reply_channel_wasm::hew_reply(
                ch.cast(),
                (&raw mut value).cast(),
                size_of::<i32>(),
            );
        }

        std::ptr::null_mut()
    }

    /// Dispatch that replies with a null payload and then self-stops in the
    /// same activation.  Used to verify that null-reply + self-stop is NOT
    /// misclassified as an orphaned ask.
    unsafe extern "C-unwind" fn null_reply_then_self_stop_dispatch(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        let ch = crate::scheduler_wasm::hew_get_reply_channel();
        if !ch.is_null() {
            // SAFETY: ch is the scheduler-installed reply channel; depositing
            // a null payload is a legitimate zero-size reply.
            unsafe {
                let _ = crate::reply_channel_wasm::hew_reply(ch.cast(), ptr::null_mut(), 0);
            }
        }
        // Self-stop AFTER the explicit null reply — must NOT set orphaned.
        hew_actor_self_stop();

        std::ptr::null_mut()
    }

    #[test]
    fn ask_self_stop_without_reply_returns_null_and_releases_channel() {
        let _guard = crate::runtime_test_guard();

        unsafe {
            crate::scheduler_wasm::hew_sched_init();
            assert_eq!(crate::reply_channel_wasm::active_channel_count(), 0);

            let actor = hew_actor_spawn(ptr::null_mut(), 0, Some(self_stop_without_reply_dispatch));
            assert!(!actor.is_null());

            let reply = hew_actor_ask(actor, 1, ptr::null_mut(), 0);
            assert!(
                reply.is_null(),
                "ask should resolve as null when the actor stops before replying"
            );
            assert_eq!(
                (&*actor).actor_state.load(Ordering::Relaxed),
                HewActorState::Stopped as i32
            );
            assert_eq!(
                crate::reply_channel_wasm::active_channel_count(),
                0,
                "ask cleanup should release the sender-side WASM reply-channel ref"
            );

            assert_eq!(hew_actor_free(actor), 0);
            crate::scheduler_wasm::hew_sched_shutdown();
            crate::scheduler_wasm::hew_runtime_cleanup();

            assert_eq!(crate::reply_channel_wasm::active_channel_count(), 0);
        }
    }

    #[test]
    fn ask_successful_reply_returns_value_without_duplicate_cleanup() {
        let _guard = crate::runtime_test_guard();

        unsafe {
            crate::scheduler_wasm::hew_sched_init();
            assert_eq!(crate::reply_channel_wasm::active_channel_count(), 0);

            let actor = hew_actor_spawn(ptr::null_mut(), 0, Some(reply_once_dispatch));
            assert!(!actor.is_null());

            let reply = hew_actor_ask(actor, 1, ptr::null_mut(), 0);
            assert!(!reply.is_null(), "happy-path ask should return a reply");
            assert_eq!(*reply.cast::<i32>(), 21);
            libc::free(reply);

            assert_eq!(
                crate::reply_channel_wasm::active_channel_count(),
                0,
                "successful asks should leave no live WASM reply channels"
            );

            assert_eq!(hew_actor_free(actor), 0);
            crate::scheduler_wasm::hew_sched_shutdown();
            crate::scheduler_wasm::hew_runtime_cleanup();

            assert_eq!(crate::reply_channel_wasm::active_channel_count(), 0);
        }
    }

    #[test]
    fn wasm_ask_timeout_rejects_late_reply_after_blocking_tick() {
        let _guard = crate::runtime_test_guard();

        unsafe {
            crate::scheduler_wasm::hew_sched_init();
            assert_eq!(crate::reply_channel_wasm::active_channel_count(), 0);

            let actor = hew_actor_spawn(ptr::null_mut(), 0, Some(late_reply_dispatch));
            assert!(!actor.is_null());

            LAST_ACTOR_ASK_ERROR.with(|c| c.set(AskError::None as i32));
            let reply = actor_ask_wasm_impl(actor, 1, ptr::null_mut(), 0, Some(1));
            assert!(
                reply.is_null(),
                "timed WASM asks should reject replies that only arrive after the timeout"
            );
            assert_eq!(
                hew_actor_ask_take_last_error(),
                AskError::Timeout as i32,
                "timed-out WASM ask must report Timeout"
            );
            assert_eq!(
                crate::reply_channel_wasm::active_channel_count(),
                0,
                "timed-out WASM asks should free buffered late replies and reply channels"
            );

            assert_eq!(hew_actor_free(actor), 0);
            crate::scheduler_wasm::hew_sched_shutdown();
            crate::scheduler_wasm::hew_runtime_cleanup();

            assert_eq!(crate::reply_channel_wasm::active_channel_count(), 0);
        }
    }

    // ── WASM ask error discrimination tests ─────────────────────────────

    /// WASM ask on a stopped actor (send failure) sets `ActorStopped`.
    #[test]
    fn wasm_ask_stopped_actor_sets_actor_stopped_error() {
        let _guard = crate::runtime_test_guard();

        unsafe {
            crate::scheduler_wasm::hew_sched_init();

            // Dispatch function is irrelevant — the actor will be stopped before
            // the ask is submitted, so dispatch is never invoked.
            let actor = hew_actor_spawn(ptr::null_mut(), 0, Some(self_stop_without_reply_dispatch));
            assert!(!actor.is_null());
            hew_actor_stop(actor);

            LAST_ACTOR_ASK_ERROR.with(|c| c.set(AskError::None as i32));
            let reply = actor_ask_wasm_impl(actor, 1, ptr::null_mut(), 0, None);
            assert!(reply.is_null(), "ask on stopped actor must return null");
            assert_eq!(
                hew_actor_ask_take_last_error(),
                AskError::ActorStopped as i32,
                "stopped actor send failure must report ActorStopped"
            );

            assert_eq!(hew_actor_free(actor), 0);
            crate::scheduler_wasm::hew_sched_shutdown();
            crate::scheduler_wasm::hew_runtime_cleanup();
        }
    }

    /// WASM unbounded ask when actor stops without replying sets `OrphanedAsk`.
    #[test]
    fn wasm_ask_self_stop_sets_orphaned_ask_error() {
        let _guard = crate::runtime_test_guard();

        unsafe {
            crate::scheduler_wasm::hew_sched_init();

            let actor = hew_actor_spawn(ptr::null_mut(), 0, Some(self_stop_without_reply_dispatch));
            assert!(!actor.is_null());

            LAST_ACTOR_ASK_ERROR.with(|c| c.set(AskError::None as i32));
            let reply = actor_ask_wasm_impl(actor, 1, ptr::null_mut(), 0, None);
            assert!(reply.is_null(), "orphaned WASM ask must return null");
            assert_eq!(
                hew_actor_ask_take_last_error(),
                AskError::OrphanedAsk as i32,
                "WASM ask orphaned by actor self-stop must report OrphanedAsk"
            );

            assert_eq!(hew_actor_free(actor), 0);
            crate::scheduler_wasm::hew_sched_shutdown();
            crate::scheduler_wasm::hew_runtime_cleanup();
        }
    }

    /// WASM ask success clears the error slot.
    #[test]
    fn wasm_ask_success_clears_error_slot() {
        let _guard = crate::runtime_test_guard();

        unsafe {
            crate::scheduler_wasm::hew_sched_init();

            let actor = hew_actor_spawn(ptr::null_mut(), 0, Some(reply_once_dispatch));
            assert!(!actor.is_null());

            LAST_ACTOR_ASK_ERROR.with(|c| c.set(AskError::Timeout as i32));
            let reply = actor_ask_wasm_impl(actor, 1, ptr::null_mut(), 0, None);
            assert!(!reply.is_null(), "WASM ask must succeed");
            // SAFETY: reply was allocated by the runtime; caller takes ownership.
            unsafe { libc::free(reply) };
            assert_eq!(
                hew_actor_ask_take_last_error(),
                AskError::None as i32,
                "successful WASM ask must clear error slot"
            );

            assert_eq!(hew_actor_free(actor), 0);
            crate::scheduler_wasm::hew_sched_shutdown();
            crate::scheduler_wasm::hew_runtime_cleanup();
        }
    }

    /// Regression: `hew_reply(ch, NULL, 0); hew_actor_self_stop()` in the same
    /// dispatch must be treated as a legitimate null reply, NOT as OrphanedAsk.
    ///
    /// The `orphaned` flag is only set by `retire_reply_channel` (called when
    /// the mailbox is torn down WITHOUT a handler reply).  When the handler
    /// explicitly replies — even with null — `orphaned` stays false.
    #[test]
    fn wasm_ask_null_reply_then_self_stop_is_not_orphaned() {
        let _guard = crate::runtime_test_guard();

        unsafe {
            crate::scheduler_wasm::hew_sched_init();

            // SAFETY: null state + valid dispatch.
            let actor =
                hew_actor_spawn(ptr::null_mut(), 0, Some(null_reply_then_self_stop_dispatch));
            assert!(!actor.is_null());

            LAST_ACTOR_ASK_ERROR.with(|c| c.set(AskError::Timeout as i32));
            let reply = actor_ask_wasm_impl(actor, 1, ptr::null_mut(), 0, None);
            assert!(
                reply.is_null(),
                "explicit null reply must still be returned as null"
            );
            assert_eq!(
                hew_actor_ask_take_last_error(),
                AskError::None as i32,
                "null reply + self-stop must NOT be classified as OrphanedAsk"
            );
            assert_eq!(
                crate::reply_channel_wasm::active_channel_count(),
                0,
                "null reply + self-stop must not leak reply channels"
            );

            // SAFETY: actor stopped itself; pointer is still allocated.
            assert_eq!(hew_actor_free(actor), 0);
            crate::scheduler_wasm::hew_sched_shutdown();
            crate::scheduler_wasm::hew_runtime_cleanup();
        }
    }

    // ── MailboxFull / NoRunnableWork discrimination (WASM) ───────────────

    /// Dispatch that does nothing: receives the message but does not reply and
    /// does not self-stop. Used to drive `MailboxFull` and `NoRunnableWork` tests.
    unsafe extern "C-unwind" fn noop_dispatch(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        std::ptr::null_mut()
    }

    /// `hew_actor_ask` on a bounded WASM mailbox that is at capacity returns
    /// `MailboxFull`.
    ///
    /// WASM is cooperative: the scheduler only runs when ticked, so a pre-queued
    /// message stays in the mailbox until `hew_wasm_tick` is called. The ask send
    /// therefore hits a full mailbox and fails before the scheduler loop is entered.
    #[test]
    fn wasm_ask_bounded_mailbox_full_sets_mailbox_full_error() {
        let _guard = crate::runtime_test_guard();

        unsafe {
            crate::scheduler_wasm::hew_sched_init();
            assert_eq!(crate::reply_channel_wasm::active_channel_count(), 0);

            // Spawn with capacity=1 (default DropNew overflow policy).
            let actor = hew_actor_spawn_bounded(ptr::null_mut(), 0, Some(noop_dispatch), 1);
            assert!(!actor.is_null());

            // Pre-fill the single slot before ticking the scheduler.
            // On WASM the scheduler is cooperative: the actor stays Runnable until
            // we call hew_wasm_tick, so the slot remains occupied.
            hew_actor_send(actor, 1, ptr::null_mut(), 0);

            // The ask send hits the full mailbox and returns ErrMailboxFull before
            // the scheduler loop is entered.
            LAST_ACTOR_ASK_ERROR.with(|c| c.set(AskError::None as i32));
            let reply = actor_ask_wasm_impl(actor, 1, ptr::null_mut(), 0, None);
            assert!(
                reply.is_null(),
                "ask into full bounded WASM mailbox must return null"
            );
            assert_eq!(
                hew_actor_ask_take_last_error(),
                AskError::MailboxFull as i32,
                "full bounded WASM mailbox must report MailboxFull"
            );
            assert_eq!(
                crate::reply_channel_wasm::active_channel_count(),
                0,
                "failed WASM ask must not leak reply channels"
            );

            // Tick to drain the pre-filled message (actor → Idle after noop_dispatch).
            crate::bridge::hew_wasm_tick(HEW_WASM_ASK_TICK_ACTIVATIONS);
            // Actor is Idle — close and free without a separate stop.
            hew_actor_stop(actor);
            assert_eq!(hew_actor_free(actor), 0);

            crate::scheduler_wasm::hew_sched_shutdown();
            crate::scheduler_wasm::hew_runtime_cleanup();

            assert_eq!(crate::reply_channel_wasm::active_channel_count(), 0);
        }
    }

    /// WASM unbounded ask returns `NoRunnableWork` when the scheduler has no more
    /// runnable actors and the handler never replied.
    ///
    /// `noop_dispatch` processes the ask message but does not call `hew_reply` and
    /// does not self-stop. After one tick the run queue is empty (`remaining == 0`)
    /// and the actor is alive (Idle), so the ask path returns `NoRunnableWork`.
    #[test]
    fn wasm_ask_no_runnable_work_sets_no_runnable_work_error() {
        let _guard = crate::runtime_test_guard();

        unsafe {
            crate::scheduler_wasm::hew_sched_init();
            assert_eq!(crate::reply_channel_wasm::active_channel_count(), 0);

            let actor = hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch));
            assert!(!actor.is_null());

            LAST_ACTOR_ASK_ERROR.with(|c| c.set(AskError::None as i32));
            let reply = actor_ask_wasm_impl(actor, 1, ptr::null_mut(), 0, None);
            assert!(
                reply.is_null(),
                "ask when handler does not reply must return null"
            );
            assert_eq!(
                hew_actor_ask_take_last_error(),
                AskError::NoRunnableWork as i32,
                "no-reply handler with drained scheduler must report NoRunnableWork"
            );
            assert_eq!(
                crate::reply_channel_wasm::active_channel_count(),
                0,
                "NoRunnableWork path must not leak reply channels"
            );

            // Actor is Idle after noop_dispatch drained its message.
            // Idle is quiescent — free directly without an explicit stop.
            assert_eq!(hew_actor_free(actor), 0);

            crate::scheduler_wasm::hew_sched_shutdown();
            crate::scheduler_wasm::hew_runtime_cleanup();

            assert_eq!(crate::reply_channel_wasm::active_channel_count(), 0);
        }
    }

    #[test]
    fn wasm_cleanup_reopens_handle_registry_for_next_session_without_reuse() {
        let _guard = crate::runtime_test_guard();

        unsafe {
            crate::scheduler_wasm::hew_sched_init();
            let first = hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch));
            assert!(!first.is_null());
            let (first_id, first_token) = ((*first).id, (*first).local_pid_id);
            assert_eq!(
                crate::lifetime::local_handles::resolve_current_actor(first_token),
                Some(first_id)
            );

            crate::scheduler_wasm::hew_sched_shutdown();
            crate::scheduler_wasm::hew_runtime_cleanup();
            assert_eq!(
                crate::lifetime::local_handles::current_counts_for_test(),
                (0, 0)
            );
            assert_eq!(
                crate::lifetime::local_handles::resolve_current_actor(first_token),
                None
            );

            crate::scheduler_wasm::hew_sched_init();
            let second = hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch));
            assert!(!second.is_null());
            let (second_id, second_token) = ((*second).id, (*second).local_pid_id);
            assert_ne!(second_token, first_token);
            assert_eq!(
                crate::lifetime::local_handles::resolve_current_actor(first_token),
                None
            );
            assert_eq!(
                crate::lifetime::local_handles::resolve_current_actor(second_token),
                Some(second_id)
            );

            crate::scheduler_wasm::hew_sched_shutdown();
            crate::scheduler_wasm::hew_runtime_cleanup();
            assert_eq!(
                crate::lifetime::local_handles::current_counts_for_test(),
                (0, 0)
            );
        }
    }

    #[test]
    fn wasm_local_pid_ask_pins_only_for_send_and_resolves_reply() {
        let _guard = crate::runtime_test_guard();

        unsafe {
            crate::scheduler_wasm::hew_sched_init();
            let actor = hew_actor_spawn(ptr::null_mut(), 0, Some(reply_once_dispatch));
            assert!(!actor.is_null());
            let token = (*actor).local_pid_id;

            let reply = hew_local_pid_ask(token, 1, ptr::null_mut(), 0);
            assert!(!reply.is_null());
            assert_eq!(*reply.cast::<i32>(), 21);
            libc::free(reply);
            assert_eq!(hew_actor_ask_take_last_error(), AskError::None as i32);

            assert_eq!(hew_actor_free(actor), 0);
            let stale_reply = hew_local_pid_ask(token, 1, ptr::null_mut(), 0);
            assert!(stale_reply.is_null());
            assert_eq!(
                hew_actor_ask_take_last_error(),
                AskError::OrphanedAsk as i32
            );

            crate::scheduler_wasm::hew_sched_shutdown();
            crate::scheduler_wasm::hew_runtime_cleanup();
        }
    }
}
