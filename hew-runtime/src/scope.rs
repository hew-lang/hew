//! Hew runtime: structured concurrency scope (legacy, fixed-capacity).
//!
//! [`HewScope`] is a `#[repr(C)]` actor container allocated on the stack in
//! generated code. It holds up to [`HEW_SCOPE_MAX_ACTORS`] actor pointers
//! and a pthread mutex for thread-safe access.
//!
//! New structured-concurrency integrations should target
//! [`super::actor_group::HewActorGroup`]. [`HewScope`] remains in place for
//! existing compiler-generated stack ABI until codegen can migrate without
//! breaking the fixed-size by-value layout.

use std::ffi::c_void;
use std::sync::atomic::{AtomicBool, Ordering};

use crate::actor::{self, HewActor};
use crate::internal::types::HewActorState;
use crate::mailbox;

// ── Cross-platform mutex for #[repr(C)] structs ─────────────────────────

#[cfg(unix)]
pub type PlatformMutex = libc::pthread_mutex_t;

#[cfg(windows)]
#[repr(C)]
pub struct PlatformMutex(*mut std::ffi::c_void);

#[cfg(windows)]
impl std::fmt::Debug for PlatformMutex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("<srwlock>")
    }
}

#[cfg(unix)]
const MUTEX_INIT: PlatformMutex = libc::PTHREAD_MUTEX_INITIALIZER;
#[cfg(windows)]
const MUTEX_INIT: PlatformMutex = PlatformMutex(std::ptr::null_mut());

#[cfg(windows)]
#[link(name = "kernel32")]
unsafe extern "system" {
    fn AcquireSRWLockExclusive(lock: *mut PlatformMutex);
    fn ReleaseSRWLockExclusive(lock: *mut PlatformMutex);
    fn TryAcquireSRWLockExclusive(lock: *mut PlatformMutex) -> i32;
}

unsafe fn mutex_lock(m: *mut PlatformMutex) {
    #[cfg(unix)]
    // SAFETY: caller guarantees `m` points to an initialised mutex.
    unsafe {
        libc::pthread_mutex_lock(m)
    };
    #[cfg(windows)]
    // SAFETY: caller guarantees `m` points to an initialised SRWLOCK.
    unsafe {
        AcquireSRWLockExclusive(m)
    };
}

unsafe fn mutex_unlock(m: *mut PlatformMutex) {
    #[cfg(unix)]
    // SAFETY: caller guarantees `m` is locked by the current thread.
    unsafe {
        libc::pthread_mutex_unlock(m)
    };
    #[cfg(windows)]
    // SAFETY: caller guarantees `m` is locked by the current thread.
    unsafe {
        ReleaseSRWLockExclusive(m)
    };
}

#[cfg(test)]
unsafe fn mutex_try_lock(m: *mut PlatformMutex) -> bool {
    #[cfg(unix)]
    // SAFETY: caller guarantees `m` points to an initialised mutex.
    unsafe {
        libc::pthread_mutex_trylock(m) == 0
    }
    #[cfg(windows)]
    // SAFETY: caller guarantees `m` points to an initialised SRWLOCK.
    unsafe {
        TryAcquireSRWLockExclusive(m) != 0
    }
}

unsafe fn mutex_destroy(m: *mut PlatformMutex) {
    #[cfg(unix)]
    // SAFETY: caller guarantees `m` is an initialised, unlocked mutex.
    unsafe {
        libc::pthread_mutex_destroy(m)
    };
    #[cfg(windows)]
    let _ = m;
}

/// Maximum number of actors a scope can hold.
///
/// This fixed limit is part of the legacy stack ABI. New call sites should
/// migrate to [`super::actor_group::HewActorGroup`] instead of raising it.
pub const HEW_SCOPE_MAX_ACTORS: usize = 64;

/// Structured concurrency scope — fixed-capacity actor container.
///
/// **Must be `#[repr(C)]`** because it is created on the stack in
/// compiler-generated code and returned by value from [`hew_scope_new`].
///
/// DEPRECATED for new runtime integrations: preserve this type for existing
/// compiler output, but prefer [`super::actor_group::HewActorGroup`] for new
/// structured-concurrency entry points and any path that may exceed
/// [`HEW_SCOPE_MAX_ACTORS`].
#[repr(C)]
pub struct HewScope {
    /// Pointers to owned actors (`*mut HewActor`).
    pub actors: [*mut c_void; HEW_SCOPE_MAX_ACTORS],
    /// Number of actors currently tracked.
    pub actor_count: i32,
    /// Mutex protecting the actor list (SRWLOCK on Windows, pthread on Unix).
    pub lock: PlatformMutex,
    /// Cooperative cancellation flag.
    pub cancelled: AtomicBool,
}

// SAFETY: The raw pointers in `actors` are only accessed while holding the
// pthread mutex. Each pointer is exclusively owned by the scope.
unsafe impl Send for HewScope {}
// SAFETY: Concurrent access is serialised through the pthread mutex.
unsafe impl Sync for HewScope {}

impl std::fmt::Debug for HewScope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("HewScope")
            .field("actor_count", &self.actor_count)
            .finish_non_exhaustive()
    }
}

// ── Stack-allocated scope ──────────────────────────────────────────────

/// Create a new scope initialised to zero (returned **by value**).
///
/// # Safety
///
/// No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_scope_new() -> HewScope {
    HewScope {
        actors: [std::ptr::null_mut(); HEW_SCOPE_MAX_ACTORS],
        actor_count: 0,
        lock: MUTEX_INIT,
        cancelled: AtomicBool::new(false),
    }
}

/// Add an actor to the scope.
///
/// Returns `0` on success, `-1` if the scope is full.
///
/// # Safety
///
/// - `scope` must be a valid pointer to an initialised [`HewScope`].
/// - `actor` must be a valid `*mut HewActor`.
#[expect(
    clippy::cast_sign_loss,
    reason = "C ABI: index parameter is non-negative"
)]
#[no_mangle]
pub unsafe extern "C" fn hew_scope_spawn(scope: *mut HewScope, actor: *mut c_void) -> i32 {
    // SAFETY: Caller guarantees `scope` is valid.
    let s = unsafe { &mut *scope };

    // SAFETY: `s.lock` was initialised by `hew_scope_new`.
    unsafe { mutex_lock(&raw mut s.lock) };

    if s.actor_count as usize >= HEW_SCOPE_MAX_ACTORS {
        // SAFETY: Lock is held.
        unsafe { mutex_unlock(&raw mut s.lock) };
        return -1;
    }

    s.actors[s.actor_count as usize] = actor;
    s.actor_count += 1;

    // SAFETY: Lock is held.
    unsafe { mutex_unlock(&raw mut s.lock) };
    0
}

/// Maximum time Phase 3 will spin waiting for an actor to reach a terminal
/// state before giving up.
const SCOPE_WAIT_TIMEOUT: std::time::Duration = std::time::Duration::from_secs(2);

/// Wait for all actors in the scope to finish, then free them.
///
/// Drains all mailboxes, closes actors, spin-waits until each actor
/// reaches a terminal state, and then frees the actor.
///
/// # Safety
///
/// `scope` must be a valid pointer to an initialised [`HewScope`].
#[expect(
    clippy::cast_sign_loss,
    reason = "C ABI: index parameter is non-negative"
)]
#[no_mangle]
pub unsafe extern "C" fn hew_scope_wait_all(scope: *mut HewScope) {
    // SAFETY: Caller guarantees `scope` is valid.
    let s = unsafe { &mut *scope };

    // Phase 1: Wait until all mailboxes are drained.
    // SAFETY: `s.lock` was initialised by `hew_scope_new`.
    unsafe { mutex_lock(&raw mut s.lock) };
    for i in 0..s.actor_count as usize {
        let actor_ptr = s.actors[i].cast::<HewActor>();
        if actor_ptr.is_null() {
            continue;
        }
        // SAFETY: actor_ptr is valid per spawn contract.
        let mb = unsafe { (*actor_ptr).mailbox.cast::<mailbox::HewMailbox>() };
        if mb.is_null() {
            continue;
        }
        // SAFETY: mb is valid for the actor's lifetime.
        while unsafe { mailbox::hew_mailbox_has_messages(mb) } != 0 {
            std::thread::sleep(std::time::Duration::from_millis(1));
        }
    }

    // Phase 2: Close all mailboxes.
    for i in 0..s.actor_count as usize {
        if !s.actors[i].is_null() {
            // SAFETY: actor pointer is valid.
            unsafe { actor::hew_actor_close(s.actors[i].cast()) };
        }
    }

    // Phase 3: Wait for all actors to reach a terminal state (Stopped or
    // Crashed).  `Stopping` is *not* terminal — the scheduler is still
    // unwinding the actor's stack and will CAS it to `Stopped` itself, so
    // we must keep spinning until that completes.
    for i in 0..s.actor_count as usize {
        let actor_ptr = s.actors[i].cast::<HewActor>();
        if actor_ptr.is_null() {
            continue;
        }
        // SAFETY: actor_ptr is valid.
        let a = unsafe { &*actor_ptr };
        let deadline = std::time::Instant::now() + SCOPE_WAIT_TIMEOUT;
        loop {
            let state = a.actor_state.load(Ordering::Acquire);
            if state == HewActorState::Stopped as i32 || state == HewActorState::Crashed as i32 {
                break;
            }
            if std::time::Instant::now() >= deadline {
                break;
            }
            std::thread::sleep(std::time::Duration::from_micros(100));
        }
    }

    // Phase 4: Free actors that reached a terminal state.  Non-terminal
    // actors (still in Stopping after the timeout) remain tracked in
    // LIVE_ACTORS and will be reclaimed at process exit by cleanup_all_actors.
    for i in 0..s.actor_count as usize {
        let actor_ptr = s.actors[i].cast::<HewActor>();
        if actor_ptr.is_null() {
            continue;
        }
        // SAFETY: actor_ptr is valid per spawn contract.
        let state = unsafe { &*actor_ptr }.actor_state.load(Ordering::Acquire);
        if state == HewActorState::Stopped as i32 || state == HewActorState::Crashed as i32 {
            // SAFETY: actor reached a terminal state and is not being dispatched.
            unsafe { actor::hew_actor_free(actor_ptr) };
        }
        s.actors[i] = std::ptr::null_mut();
    }
    s.actor_count = 0;
    // SAFETY: Lock is held.
    unsafe { mutex_unlock(&raw mut s.lock) };
}

/// Destroy a stack-allocated scope (mutex cleanup only).
///
/// Does **not** free actors; call [`hew_scope_wait_all`] first if needed.
///
/// # Safety
///
/// `scope` must be a valid pointer to an initialised [`HewScope`].
#[no_mangle]
pub unsafe extern "C" fn hew_scope_destroy(scope: *mut HewScope) {
    // SAFETY: Caller guarantees `scope` is valid.
    let s = unsafe { &mut *scope };
    // SAFETY: Lock was initialised by hew_scope_new.
    unsafe { mutex_destroy(&raw mut s.lock) };
    s.actors = [std::ptr::null_mut(); HEW_SCOPE_MAX_ACTORS];
    s.actor_count = 0;
}

// ── Cancellation ───────────────────────────────────────────────────────

/// Set the scope's cancellation flag.
///
/// # Safety
///
/// `scope` must be a valid pointer to an initialised [`HewScope`].
#[no_mangle]
pub unsafe extern "C" fn hew_scope_cancel(scope: *mut HewScope) {
    if scope.is_null() {
        return;
    }
    // SAFETY: Caller guarantees `scope` is valid.
    unsafe { (*scope).cancelled.store(true, Ordering::Release) };
}

/// Check whether the scope's cancellation flag is set.
///
/// Returns `1` if cancelled, `0` otherwise.
///
/// # Safety
///
/// `scope` must be a valid pointer to an initialised [`HewScope`].
#[no_mangle]
pub unsafe extern "C" fn hew_scope_is_cancelled(scope: *mut HewScope) -> i32 {
    if scope.is_null() {
        return 0;
    }
    // SAFETY: Caller guarantees `scope` is valid.
    i32::from(unsafe { (*scope).cancelled.load(Ordering::Acquire) })
}

// ── Heap-allocated scope ───────────────────────────────────────────────

/// Heap-allocate a new scope (via `malloc`).
///
/// # Safety
///
/// Returned pointer must be freed with [`hew_scope_free`].
#[no_mangle]
pub unsafe extern "C" fn hew_scope_create() -> *mut HewScope {
    // SAFETY: malloc with correct size.
    let ptr = unsafe { libc::malloc(std::mem::size_of::<HewScope>()) }.cast::<HewScope>();
    if ptr.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: ptr is non-null, properly sized and exclusively owned.
    unsafe {
        std::ptr::write(ptr, hew_scope_new());
    }
    ptr
}

/// Free a heap-allocated scope.
///
/// # Safety
///
/// `scope` must have been returned by [`hew_scope_create`].
#[no_mangle]
pub unsafe extern "C" fn hew_scope_free(scope: *mut HewScope) {
    if scope.is_null() {
        return;
    }
    // SAFETY: Caller guarantees scope was heap-allocated by hew_scope_create.
    unsafe {
        hew_scope_destroy(scope);
        libc::free(scope.cast());
    }
}

// ── Tests ──────────────────────────────────────────────────────────────

#[cfg(all(test, not(target_arch = "wasm32")))]
mod tests {
    use super::*;
    use std::sync::Arc;
    use std::time::{Duration, Instant};

    unsafe extern "C" fn noop_dispatch(
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _size: usize,
    ) {
    }

    /// An actor stuck in `Stopping` must NOT be freed before it reaches a
    /// terminal state. A background thread simulates the scheduler
    /// completing the `Stopping → Stopped` transition after a delay.
    #[test]
    fn stopping_actor_waits_for_terminal_state() {
        // SAFETY: null state + noop dispatch is the minimal valid spawn.
        let actor = unsafe { actor::hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch)) };
        assert!(!actor.is_null());

        // SAFETY: scope is valid; actor was just spawned.
        let mut scope = unsafe { hew_scope_new() };
        // SAFETY: scope and actor are valid.
        let rc = unsafe { hew_scope_spawn(&raw mut scope, actor.cast()) };
        assert_eq!(rc, 0);

        // Simulate: actor called self_stop mid-dispatch → Stopping.
        // SAFETY: actor is valid.
        let a = unsafe { &*actor };
        a.actor_state
            .store(HewActorState::Stopping as i32, Ordering::Release);

        // Background thread simulates the scheduler finishing dispatch.
        let actor_addr = actor as usize;
        let handle = std::thread::spawn(move || {
            std::thread::sleep(Duration::from_millis(300));
            // SAFETY: actor_addr came from a valid actor pointer above.
            let a = unsafe { &*(actor_addr as *const HewActor) };
            a.actor_state
                .store(HewActorState::Stopped as i32, Ordering::Release);
        });

        let start = Instant::now();
        // SAFETY: scope is valid.
        unsafe { hew_scope_wait_all(&raw mut scope) };
        let elapsed = start.elapsed();

        // Scope must have waited for the transition — not exited instantly.
        assert!(
            elapsed >= Duration::from_millis(200),
            "scope exited too early ({elapsed:?}); Stopping was treated as terminal"
        );

        handle.join().unwrap();
        // SAFETY: scope is valid.
        unsafe { hew_scope_destroy(&raw mut scope) };
    }

    /// Actors already in `Stopped` or `Crashed` are freed without delay.
    #[test]
    fn terminal_actors_freed_promptly() {
        // SAFETY: null state + noop dispatch is the minimal valid spawn.
        let a1 = unsafe { actor::hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch)) };
        // SAFETY: null state + noop dispatch is the minimal valid spawn.
        let a2 = unsafe { actor::hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch)) };
        assert!(!a1.is_null());
        assert!(!a2.is_null());

        // Pre-set to terminal states.
        // SAFETY: a1 is valid (non-null checked above).
        unsafe { &*a1 }
            .actor_state
            .store(HewActorState::Stopped as i32, Ordering::Release);
        // SAFETY: a2 is valid (non-null checked above).
        unsafe { &*a2 }
            .actor_state
            .store(HewActorState::Crashed as i32, Ordering::Release);

        // SAFETY: scope and actors are valid.
        let mut scope = unsafe { hew_scope_new() };
        // SAFETY: scope and actors are valid.
        let rc1 = unsafe { hew_scope_spawn(&raw mut scope, a1.cast()) };
        assert_eq!(rc1, 0);
        // SAFETY: scope and actors are valid.
        let rc2 = unsafe { hew_scope_spawn(&raw mut scope, a2.cast()) };
        assert_eq!(rc2, 0);

        let start = Instant::now();
        // SAFETY: scope is valid.
        unsafe { hew_scope_wait_all(&raw mut scope) };
        let elapsed = start.elapsed();

        assert!(
            elapsed < Duration::from_secs(1),
            "scope took too long for terminal actors ({elapsed:?})"
        );

        // SAFETY: scope is valid.
        unsafe { hew_scope_destroy(&raw mut scope) };
    }

    /// If an actor is permanently stuck in `Stopping` (the scheduler never
    /// completes the transition), the scope wait must still return after its
    /// bounded timeout rather than spinning forever.
    #[test]
    fn scope_wait_bounded_on_stuck_stopping() {
        // SAFETY: null state + noop dispatch is the minimal valid spawn.
        let actor = unsafe { actor::hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch)) };
        assert!(!actor.is_null());

        // SAFETY: actor is valid.
        unsafe { &*actor }
            .actor_state
            .store(HewActorState::Stopping as i32, Ordering::Release);

        // SAFETY: scope and actor are valid.
        let mut scope = unsafe { hew_scope_new() };
        // SAFETY: scope and actor are valid.
        let rc = unsafe { hew_scope_spawn(&raw mut scope, actor.cast()) };
        assert_eq!(rc, 0);

        let start = Instant::now();
        // SAFETY: scope is valid.
        unsafe { hew_scope_wait_all(&raw mut scope) };
        let elapsed = start.elapsed();

        // Must have waited at least the scope timeout, not returned instantly.
        assert!(
            elapsed >= Duration::from_secs(1),
            "scope returned too quickly on stuck Stopping actor ({elapsed:?})"
        );

        // SAFETY: scope is valid.
        unsafe { hew_scope_destroy(&raw mut scope) };
    }

    #[test]
    fn wait_all_keeps_scope_locked_until_mailboxes_drain() {
        // SAFETY: null state + noop dispatch is the minimal valid spawn.
        let actor1 =
            unsafe { actor::hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch)) };
        // SAFETY: null state + noop dispatch is the minimal valid spawn.
        let actor2 =
            unsafe { actor::hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch)) };
        assert!(!actor1.is_null());
        assert!(!actor2.is_null());

        // SAFETY: actors are valid.
        unsafe { &*actor1 }
            .actor_state
            .store(HewActorState::Stopped as i32, Ordering::Release);
        // SAFETY: actor2 is valid.
        unsafe { &*actor2 }
            .actor_state
            .store(HewActorState::Stopped as i32, Ordering::Release);

        // SAFETY: hew_scope_new returns a fully initialised scope by value.
        let mut scope = Box::new(unsafe { hew_scope_new() });
        // SAFETY: scope and actor are valid.
        let spawn_actor1 = unsafe { hew_scope_spawn(&raw mut *scope, actor1.cast()) };
        assert_eq!(spawn_actor1, 0);

        // SAFETY: actor1 is valid and owns a mailbox for its lifetime.
        let mailbox = unsafe { (*actor1).mailbox.cast::<mailbox::HewMailbox>() };
        assert!(!mailbox.is_null());
        // SAFETY: mailbox is live and accepts an empty test message.
        let send_rc = unsafe { mailbox::hew_mailbox_send(mailbox, 1, std::ptr::null_mut(), 0) };
        assert_eq!(send_rc, 0);

        let scope_addr = (&raw mut *scope) as usize;
        let actor2_addr = actor2 as usize;
        let lock_addr = (&raw mut scope.lock) as usize;
        let spawn_started = Arc::new(AtomicBool::new(false));
        let spawn_returned = Arc::new(AtomicBool::new(false));
        let spawn_started_in_thread = Arc::clone(&spawn_started);
        let spawn_returned_in_thread = Arc::clone(&spawn_returned);

        let wait_handle = std::thread::spawn(move || {
            // SAFETY: scope_addr points to the boxed scope that outlives this thread.
            unsafe { hew_scope_wait_all(scope_addr as *mut HewScope) };
        });

        loop {
            // SAFETY: lock_addr points at the scope mutex, which remains live for
            // the duration of the test.
            if unsafe { !mutex_try_lock(lock_addr as *mut PlatformMutex) } {
                break;
            }
            // SAFETY: This thread just acquired the scope lock via try_lock.
            unsafe { mutex_unlock(lock_addr as *mut PlatformMutex) };
            std::thread::yield_now();
        }

        let spawn_handle = std::thread::spawn(move || {
            spawn_started_in_thread.store(true, Ordering::Release);
            // SAFETY: scope_addr and actor2_addr remain valid for the duration of the test.
            let rc =
                unsafe { hew_scope_spawn(scope_addr as *mut HewScope, actor2_addr as *mut c_void) };
            assert_eq!(rc, 0);
            spawn_returned_in_thread.store(true, Ordering::Release);
        });

        while !spawn_started.load(Ordering::Acquire) {
            std::thread::yield_now();
        }
        for _ in 0..32 {
            std::thread::yield_now();
        }
        assert!(
            !spawn_returned.load(Ordering::Acquire),
            "spawn returned before wait_all released the phase-1 scope lock"
        );

        // SAFETY: mailbox is live; draining the message lets wait_all finish.
        let node = unsafe { mailbox::hew_mailbox_try_recv(mailbox) };
        assert!(!node.is_null());
        // SAFETY: node was just removed from the mailbox and is exclusively owned.
        unsafe { mailbox::hew_msg_node_free(node) };

        wait_handle.join().unwrap();
        spawn_handle.join().unwrap();

        assert!(spawn_returned.load(Ordering::Acquire));

        // SAFETY: actor2 was inserted after the first wait_all returned.
        unsafe { hew_scope_wait_all(&raw mut *scope) };
        // SAFETY: scope is valid.
        unsafe { hew_scope_destroy(&raw mut *scope) };
    }
}
