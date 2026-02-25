//! Hew runtime: dynamic actor group with condvar-based waiting.
//!
//! [`HewActorGroup`] is a heap-allocated, dynamically-growing actor
//! container. Unlike the legacy [`HewScope`](super::scope::HewScope), it
//! has no fixed capacity limit and uses a condvar for efficient waiting.

use std::collections::HashMap;
use std::ffi::c_void;
use std::sync::atomic::Ordering;
use std::sync::{Arc, LazyLock};

use crate::actor::{self, HewActor};
use crate::internal::types::{HewActorState, HewError};
use crate::io_time;

/// Initial capacity of the actor array.
const HEW_ACTOR_GROUP_INIT_CAP: usize = 16;

// ── Death notification registry ────────────────────────────────────────

/// Maps actor ID → list of condvars to notify when the actor reaches a
/// terminal state.  Allows `hew_actor_trap` / scheduler stop to wake
/// `wait_all` immediately instead of polling.
static DEATH_NOTIFIERS: LazyLock<std::sync::Mutex<HashMap<u64, Vec<Arc<std::sync::Condvar>>>>> =
    LazyLock::new(|| std::sync::Mutex::new(HashMap::new()));

/// Register a condvar to be notified when `actor_id` dies.
fn register_death_notifier(actor_id: u64, cv: Arc<std::sync::Condvar>) {
    let mut map = DEATH_NOTIFIERS.lock().unwrap_or_else(|e| e.into_inner());
    map.entry(actor_id).or_default().push(cv);
}

/// Unregister all condvar entries for `actor_id` that point to `cv`.
fn unregister_death_notifier(actor_id: u64, cv: &Arc<std::sync::Condvar>) {
    let mut map = DEATH_NOTIFIERS.lock().unwrap_or_else(|e| e.into_inner());
    if let Some(list) = map.get_mut(&actor_id) {
        list.retain(|c| !Arc::ptr_eq(c, cv));
        if list.is_empty() {
            map.remove(&actor_id);
        }
    }
}

/// Notify all registered condvars that `actor_id` has died.
///
/// Called from actor death paths (trap, self-stop finalisation).
pub(crate) fn notify_actor_death(actor_id: u64) {
    let map = DEATH_NOTIFIERS.lock().unwrap_or_else(|e| e.into_inner());
    if let Some(condvars) = map.get(&actor_id) {
        for cv in condvars {
            cv.notify_all();
        }
    }
}

/// Dynamic actor group (opaque, Box-allocated).
#[derive(Debug)]
pub struct HewActorGroup {
    actors: Vec<*mut c_void>,
    lock: std::sync::Mutex<()>,
    done_cond: Arc<std::sync::Condvar>,
}

// SAFETY: The raw pointers in `actors` are only accessed while holding
// the Mutex. Each pointer is exclusively owned by the group.
unsafe impl Send for HewActorGroup {}
// SAFETY: All concurrent access is serialised through the Mutex.
unsafe impl Sync for HewActorGroup {}

// ── Lifecycle ──────────────────────────────────────────────────────────

/// Create a new empty actor group.
///
/// # Safety
///
/// Returned pointer must be freed with [`hew_actor_group_destroy`].
#[no_mangle]
pub unsafe extern "C" fn hew_actor_group_new() -> *mut HewActorGroup {
    let group = Box::new(HewActorGroup {
        actors: Vec::with_capacity(HEW_ACTOR_GROUP_INIT_CAP),
        lock: std::sync::Mutex::new(()),
        done_cond: Arc::new(std::sync::Condvar::new()),
    });
    Box::into_raw(group)
}

/// Destroy an actor group, freeing all internal resources.
///
/// Does **not** free or stop the actors themselves.
///
/// # Safety
///
/// `g` must have been returned by [`hew_actor_group_new`] and must not
/// be used after this call.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_group_destroy(g: *mut HewActorGroup) {
    if g.is_null() {
        return;
    }
    // SAFETY: Caller guarantees `g` was Box-allocated.
    let group = unsafe { Box::from_raw(g) };

    // Unregister death notifiers for all tracked actors.
    for &actor_ptr in &group.actors {
        if !actor_ptr.is_null() {
            let a = actor_ptr.cast::<HewActor>();
            // SAFETY: actor pointers are valid per add contract.
            let actor_id = unsafe { (*a).id };
            unregister_death_notifier(actor_id, &group.done_cond);
        }
    }

    drop(group);
}

// ── Add ────────────────────────────────────────────────────────────────

/// Add an actor to the group.
///
/// Returns `0` on success, `-1` on null arguments.
///
/// # Panics
///
/// Panics if the internal mutex is poisoned.
///
/// # Safety
///
/// - `g` must be a valid pointer returned by [`hew_actor_group_new`].
/// - `actor` must be a valid `*mut HewActor`.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_group_add(g: *mut HewActorGroup, actor: *mut c_void) -> i32 {
    if g.is_null() || actor.is_null() {
        return -1;
    }
    // SAFETY: Caller guarantees `g` is valid.
    let group = unsafe { &mut *g };

    let _guard = group.lock.lock().unwrap_or_else(|e| e.into_inner());

    // Register death notifier so wait_all wakes immediately on actor death.
    let a = actor.cast::<HewActor>();
    // SAFETY: Caller guarantees `actor` is a valid `*mut HewActor`.
    let actor_id = unsafe { (*a).id };
    register_death_notifier(actor_id, Arc::clone(&group.done_cond));

    group.actors.push(actor);
    0
}

// ── Wait ───────────────────────────────────────────────────────────────

/// Returns `true` if the given state value is terminal (Stopped or Crashed).
#[inline]
fn is_terminal(state: i32) -> bool {
    state == HewActorState::Stopped as i32 || state == HewActorState::Crashed as i32
}

/// Check whether all actors in the group are in a terminal state.
///
/// Must be called with the group's lock held.
fn all_stopped(group: &HewActorGroup) -> bool {
    for &actor_ptr in &group.actors {
        if actor_ptr.is_null() {
            continue;
        }
        let a = actor_ptr.cast::<HewActor>();
        // SAFETY: actor pointers are valid per add contract.
        let state = unsafe { (*a).actor_state.load(Ordering::Acquire) };
        if !is_terminal(state) {
            return false;
        }
    }
    true
}

/// Block until all actors in the group have stopped.
///
/// # Panics
///
/// Panics if the internal mutex or condvar is poisoned.
///
/// # Safety
///
/// `g` must be a valid pointer returned by [`hew_actor_group_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_actor_group_wait_all(g: *mut HewActorGroup) {
    if g.is_null() {
        return;
    }
    // SAFETY: Caller guarantees `g` is valid.
    let group = unsafe { &*g };

    loop {
        let guard = group.lock.lock().unwrap_or_else(|e| e.into_inner());
        if all_stopped(group) {
            return;
        }
        // Wait with a 10 ms timeout to re-check actor states.
        let _guard = group
            .done_cond
            .wait_timeout(guard, std::time::Duration::from_millis(10))
            .unwrap_or_else(|e| e.into_inner());
    }
}

/// Block until all actors stop or the timeout expires.
///
/// Returns `0` on success, [`HewError::ErrTimeout`] on timeout.
///
/// # Panics
///
/// Panics if the internal mutex or condvar is poisoned.
///
/// # Safety
///
/// `g` must be a valid pointer returned by [`hew_actor_group_new`].
#[expect(clippy::cast_sign_loss, reason = "C ABI: timeout_ms is non-negative")]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_group_wait_timeout(
    g: *mut HewActorGroup,
    timeout_ms: i32,
) -> i32 {
    if g.is_null() {
        return -1;
    }
    // SAFETY: Caller guarantees `g` is valid.
    let group = unsafe { &*g };

    // SAFETY: hew_now_ms has no preconditions.
    let deadline = unsafe { io_time::hew_now_ms() } + timeout_ms as u64;

    loop {
        // SAFETY: hew_now_ms has no preconditions.
        if unsafe { io_time::hew_now_ms() } >= deadline {
            return HewError::ErrTimeout as i32;
        }

        let guard = group.lock.lock().unwrap_or_else(|e| e.into_inner());
        if all_stopped(group) {
            return 0;
        }
        let _guard = group
            .done_cond
            .wait_timeout(guard, std::time::Duration::from_millis(10))
            .unwrap_or_else(|e| e.into_inner());
    }
}

// ── Stop ───────────────────────────────────────────────────────────────

/// Stop all actors in the group.
///
/// # Panics
///
/// Panics if the internal mutex is poisoned.
///
/// # Safety
///
/// `g` must be a valid pointer returned by [`hew_actor_group_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_actor_group_stop_all(g: *mut HewActorGroup) {
    if g.is_null() {
        return;
    }
    // SAFETY: Caller guarantees `g` is valid.
    let group = unsafe { &*g };

    let _guard = group.lock.lock().unwrap_or_else(|e| e.into_inner());
    for &actor_ptr in &group.actors {
        if !actor_ptr.is_null() {
            // SAFETY: actor pointer is valid per add contract.
            unsafe { actor::hew_actor_stop(actor_ptr.cast()) };
        }
    }
}

// ── Single-actor await ────────────────────────────────────────────────

/// Block until the given actor reaches a terminal state (Stopped or Crashed).
///
/// Returns the actor's error code (0 = normal stop, non-zero = crash).
/// Returns `-1` if `actor` is null.
///
/// # Safety
///
/// `actor` must be a valid pointer to a [`HewActor`].
#[no_mangle]
pub unsafe extern "C" fn hew_actor_await(actor: *mut HewActor) -> i32 {
    if actor.is_null() {
        return -1;
    }
    // SAFETY: Caller guarantees `actor` is valid.
    let a = unsafe { &*actor };

    // Fast path: already terminal.
    if is_terminal(a.actor_state.load(Ordering::Acquire)) {
        return a.error_code.load(Ordering::Acquire);
    }

    // Slow path: poll with exponential backoff up to 10 ms
    // (matches the existing actor_group wait pattern).
    let mut wait = std::time::Duration::from_micros(100);
    let max_wait = std::time::Duration::from_millis(10);
    loop {
        std::thread::sleep(wait);
        if is_terminal(a.actor_state.load(Ordering::Acquire)) {
            return a.error_code.load(Ordering::Acquire);
        }
        wait = std::cmp::min(wait * 2, max_wait);
    }
}

/// Block until all actors in the array reach a terminal state.
///
/// Returns `0` if every actor stopped normally, or the first non-zero
/// error code encountered. Returns `-1` on null/invalid arguments.
///
/// # Safety
///
/// - `actors` must point to an array of at least `count` valid
///   `*mut HewActor` pointers (null entries are skipped).
/// - `count` must be non-negative.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_await_all(actors: *const *mut HewActor, count: i64) -> i32 {
    if actors.is_null() || count < 0 {
        return -1;
    }

    let mut first_error: i32 = 0;
    for i in 0..count as usize {
        // SAFETY: Caller guarantees the array is valid for `count` elements.
        let actor = unsafe { *actors.add(i) };
        if actor.is_null() {
            continue;
        }
        // SAFETY: Each actor pointer is valid per caller contract.
        let rc = unsafe { hew_actor_await(actor) };
        if first_error == 0 && rc != 0 {
            first_error = rc;
        }
    }
    first_error
}
