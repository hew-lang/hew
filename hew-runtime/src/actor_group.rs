//! Hew runtime: dynamic actor group with condvar-based waiting.
//!
//! [`HewActorGroup`] is a heap-allocated, dynamically-growing actor
//! container with no fixed capacity limit; it uses a condvar for
//! efficient waiting. It is the heap-allocated companion to
//! [`HewTaskScope`](super::task_scope::HewTaskScope), the canonical
//! structured-concurrency substrate that `scope { … }` lowers to.
//!
//! **Note:** `HewActorGroup` is currently unintegrated and retained as a
//! reference design for future heap-allocated actor groups. It is not
//! currently emitted by codegen.
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

use std::collections::HashMap;
use std::ffi::c_void;
use std::sync::atomic::Ordering;
use std::sync::{Arc, LazyLock};

use crate::actor::{self, HewActor};
use crate::internal::types::{HewActorState, HewError};
use crate::lifetime::live_actors;
use crate::util::{CondvarExt, MutexExt};

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
    let mut map = DEATH_NOTIFIERS.lock_or_recover();
    map.entry(actor_id).or_default().push(cv);
}

/// Unregister all condvar entries for `actor_id` that point to `cv`.
fn unregister_death_notifier(actor_id: u64, cv: &Arc<std::sync::Condvar>) {
    let mut map = DEATH_NOTIFIERS.lock_or_recover();
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
    let map = DEATH_NOTIFIERS.lock_or_recover();
    if let Some(condvars) = map.get(&actor_id) {
        for cv in condvars {
            cv.notify_all();
        }
    }
}

/// Dynamic actor group (opaque, Box-allocated).
#[derive(Debug)]
pub struct HewActorGroup {
    /// Each entry is `(actor_id, *mut HewActor)`.
    ///
    /// Storing the actor-ID alongside the pointer lets `all_stopped`
    /// validate liveness through `LIVE_ACTORS` (keyed by ID) rather than
    /// dereferencing the raw pointer bare — closing the UAF window that
    /// opens when a supervisor frees a sibling while `wait_all` loops.
    actors: Vec<(u64, *mut c_void)>,
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
    Box::into_raw(group) // ALLOCATOR-PAIRING: GlobalAlloc
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
    let group = unsafe { Box::from_raw(g) }; // ALLOCATOR-PAIRING: GlobalAlloc

    // Unregister death notifiers for all tracked actors.
    for &(actor_id, _actor_ptr) in &group.actors {
        unregister_death_notifier(actor_id, &group.done_cond);
    }

    drop(group);
}

// ── Add ────────────────────────────────────────────────────────────────

/// Add an actor to the group.
///
/// Returns `0` on success, `-1` on null arguments.
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

    let _guard = group.lock.lock_or_recover();

    // Register death notifier so wait_all wakes immediately on actor death.
    let a = actor.cast::<HewActor>();
    // SAFETY: Caller guarantees `actor` is a valid `*mut HewActor`.
    let actor_id = unsafe { (*a).id };
    register_death_notifier(actor_id, Arc::clone(&group.done_cond));

    group.actors.push((actor_id, actor));
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
///
/// Reads each actor's state through `live_actors::with_live_actor_by_id`,
/// which holds the `LIVE_ACTORS` registry lock for the duration of each
/// state read.  This closes the UAF window that opens when a supervisor
/// frees a sibling actor between `wait_all`'s condvar wake and the state
/// read — the actor cannot be reclaimed while the registry lock is held.
///
/// If an actor has already been removed from `LIVE_ACTORS` (freed by a
/// supervisor before we reach it), we treat it as terminal: a freed actor
/// is no longer running.
fn all_stopped(group: &HewActorGroup) -> bool {
    for &(actor_id, actor_ptr) in &group.actors {
        if actor_ptr.is_null() {
            continue;
        }
        let a = actor_ptr.cast::<HewActor>();
        // Hold LIVE_ACTORS across the state read.  A concurrent
        // hew_actor_free_inner cannot free `a` while the closure runs
        // (untrack_actor removes the entry under the same lock, and the
        // free is deferred until after the lock is released).
        let is_term = live_actors::with_live_actor_by_id(actor_id, a, |actor_ref| {
            is_terminal(actor_ref.actor_state.load(Ordering::Acquire))
        })
        // None means the actor is no longer in LIVE_ACTORS (already freed
        // by a supervisor restart); treat that as terminal — a freed actor
        // is not running.
        .unwrap_or(true);
        if !is_term {
            return false;
        }
    }
    true
}

/// Block until all actors in the group have stopped.
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
        let guard = group.lock.lock_or_recover();
        if all_stopped(group) {
            return;
        }
        // Wait with a 10 ms timeout to re-check actor states.
        let (_guard, _timeout) = group
            .done_cond
            .wait_timeout_or_recover(guard, std::time::Duration::from_millis(10));
    }
}

/// Block until all actors stop or the timeout expires.
///
/// Returns `0` on success, [`HewError::ErrTimeout`] on timeout.
///
/// # Safety
///
/// `g` must be a valid pointer returned by [`hew_actor_group_new`].
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

    let deadline = std::time::Instant::now()
        + std::time::Duration::from_millis(u64::try_from(timeout_ms.max(0)).unwrap_or(0));
    let mut guard = group.lock.lock_or_recover();

    loop {
        if all_stopped(group) {
            return 0;
        }

        let remaining = deadline.saturating_duration_since(std::time::Instant::now());
        if remaining.is_zero() {
            return HewError::ErrTimeout as i32;
        }

        let wait_for = remaining.min(std::time::Duration::from_millis(10));
        let (new_guard, wait_result) = group.done_cond.wait_timeout_or_recover(guard, wait_for);
        guard = new_guard;

        if wait_result.timed_out() && all_stopped(group) {
            return 0;
        }
    }
}

// ── Stop ───────────────────────────────────────────────────────────────

/// Stop all actors in the group.
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

    let _guard = group.lock.lock_or_recover();
    for &(_actor_id, actor_ptr) in &group.actors {
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
    #[expect(
        clippy::cast_sign_loss,
        clippy::cast_possible_truncation,
        reason = "count >= 0 checked above; practical array sizes fit in usize"
    )]
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

// ── Tests ──────────────────────────────────────────────────────────────────
//
// These tests cover the lock-discipline fix in `all_stopped`:
//
// - F-1 regression: `all_stopped` now reads actor state through
//   `live_actors::with_live_actor_by_id`, holding `LIVE_ACTORS` for the
//   duration of the read.  Direct pointer dereference (the UAF source) is
//   gone.
// - The untracked-as-terminal path is exercised: if a supervisor removes an
//   actor from `LIVE_ACTORS` while the group holds the raw pointer,
//   `all_stopped` treats the entry as terminal rather than dereferencing freed
//   memory.
#[cfg(all(test, not(target_arch = "wasm32")))]
mod tests {
    use super::*;
    use crate::actor::{HEW_DEFAULT_REDUCTIONS, HEW_MSG_BUDGET, HEW_PRIORITY_NORMAL};
    use std::ptr;
    use std::sync::atomic::{AtomicBool, AtomicI32, AtomicPtr, AtomicU64};

    // IDs in the high range to avoid clashing with concurrent tests.
    static NEXT_TEST_ID: AtomicU64 = AtomicU64::new(0x00ff_0000_0000_0000);
    fn next_id() -> u64 {
        NEXT_TEST_ID.fetch_add(1, Ordering::Relaxed)
    }

    /// Minimal `HewActor` stub sufficient for actor-group liveness tests.
    /// Only `id` and `actor_state` are exercised by `all_stopped`.
    fn stub_actor(id: u64, state: HewActorState) -> HewActor {
        HewActor {
            sched_link_next: AtomicPtr::new(ptr::null_mut()),
            id,
            state: ptr::null_mut(),
            state_size: 0,
            dispatch: None,
            mailbox: ptr::null_mut(),
            actor_state: AtomicI32::new(state as i32),
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
            suspended_cont: AtomicPtr::new(ptr::null_mut()),
            cont_tag: AtomicI32::new(0),
            pending_wake: AtomicBool::new(false),
            suspended_reply_channel: AtomicPtr::new(ptr::null_mut()),
            suspended_cancel_token: AtomicPtr::new(ptr::null_mut()),
            runtime_id: crate::runtime_id::RuntimeId::DEFAULT,
            runtime: ptr::null(),
        }
    }

    /// RAII guard: allocates actor on heap, tracks in `LIVE_ACTORS`, untracks + frees on drop.
    struct TrackedActor {
        ptr: *mut HewActor,
    }

    impl TrackedActor {
        fn install(actor: HewActor) -> Self {
            let ptr = Box::into_raw(Box::new(actor));
            // SAFETY: `ptr` is a freshly-boxed, fully-initialised actor.
            unsafe { crate::lifetime::live_actors::track_actor(ptr) };
            Self { ptr }
        }

        fn ptr(&self) -> *mut HewActor {
            self.ptr
        }

        /// Untrack without freeing: models a supervisor removing the actor from
        /// `LIVE_ACTORS` before (or instead of) freeing the allocation.  The box
        /// stays live so the test can safely proceed; in production the allocation
        /// is freed by `hew_actor_free_box` immediately after `untrack_actor`.
        fn untrack(&self) {
            crate::lifetime::live_actors::untrack_actor(self.ptr);
        }
    }

    impl Drop for TrackedActor {
        fn drop(&mut self) {
            // Idempotent: returns false if already removed by `untrack()`.
            crate::lifetime::live_actors::untrack_actor(self.ptr);
            // SAFETY: `ptr` came from Box::into_raw above; freed exactly once.
            unsafe { drop(Box::from_raw(self.ptr)) };
        }
    }

    /// `wait_all` exits immediately when all actors are already in a terminal
    /// state and tracked in `LIVE_ACTORS`.
    #[test]
    fn wait_all_returns_for_stopped_actors() {
        let _rt = crate::runtime_test_guard();

        let actor_a = TrackedActor::install(stub_actor(next_id(), HewActorState::Stopped));
        let actor_b = TrackedActor::install(stub_actor(next_id(), HewActorState::Crashed));

        // SAFETY: group was just returned by hew_actor_group_new.
        let group = unsafe { hew_actor_group_new() };
        assert!(!group.is_null());
        // SAFETY: group and actor pointers are valid per their respective constructors.
        let _ = unsafe { hew_actor_group_add(group, actor_a.ptr().cast()) };
        // SAFETY: same as above.
        let _ = unsafe { hew_actor_group_add(group, actor_b.ptr().cast()) };

        // All actors already terminal — should complete well within 200 ms.
        // SAFETY: group is valid and not concurrently destroyed.
        let rc = unsafe { hew_actor_group_wait_timeout(group, 200) };
        assert_eq!(rc, 0, "wait_all should succeed for already-stopped actors");

        // SAFETY: group is valid and no longer used after this.
        unsafe { hew_actor_group_destroy(group) };
    }

    /// F-1 regression: when an actor is removed from `LIVE_ACTORS` (simulating a
    /// supervisor free), `all_stopped` must NOT dereference the raw pointer; it
    /// must treat the untracked actor as terminal and return true without a UAF.
    ///
    /// Deterministic concurrent-race reproduction is infeasible in a unit test, so
    /// this test verifies the corrected code path: `all_stopped` sees
    /// `with_live_actor_by_id` return `None` (actor gone from registry) and
    /// `unwrap_or(true)` → terminal, rather than performing a bare dereference.
    #[test]
    fn wait_all_treats_untracked_actor_as_terminal() {
        let _rt = crate::runtime_test_guard();

        // Actor starts Runnable (not terminal).
        let actor_a = TrackedActor::install(stub_actor(next_id(), HewActorState::Runnable));

        // SAFETY: group was just returned by hew_actor_group_new.
        let group = unsafe { hew_actor_group_new() };
        // SAFETY: group and actor_a pointer are valid per their constructors.
        let rc_add = unsafe { hew_actor_group_add(group, actor_a.ptr().cast()) };
        assert_eq!(rc_add, 0);

        // Verify baseline: actor is not stopped → wait_timeout should time out.
        // SAFETY: group is valid and not concurrently destroyed.
        let rc_timeout = unsafe { hew_actor_group_wait_timeout(group, 30) };
        assert_ne!(rc_timeout, 0, "should time out: actor is still running");

        // Simulate supervisor removing the actor from LIVE_ACTORS.
        // In production, hew_actor_free_box would immediately follow; here the
        // box stays live so the test can safely call wait_timeout again.
        actor_a.untrack();

        // all_stopped sees the actor is not in LIVE_ACTORS → treats as terminal.
        // SAFETY: group is valid and not concurrently destroyed.
        let rc_done = unsafe { hew_actor_group_wait_timeout(group, 200) };
        assert_eq!(rc_done, 0, "untracked actor should be treated as terminal");

        // SAFETY: group is valid and no longer used after this.
        unsafe { hew_actor_group_destroy(group) };
        // TrackedActor::drop calls untrack_actor idempotently (no-op) then frees box.
    }
}
