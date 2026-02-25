//! Hew runtime: structured concurrency scope (legacy, fixed-capacity).
//!
//! [`HewScope`] is a `#[repr(C)]` actor container allocated on the stack in
//! generated code. It holds up to [`HEW_SCOPE_MAX_ACTORS`] actor pointers
//! and a pthread mutex for thread-safe access.

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
}

unsafe fn mutex_init(m: *mut PlatformMutex) {
    #[cfg(unix)]
    unsafe {
        libc::pthread_mutex_init(m, std::ptr::null())
    };
    #[cfg(windows)]
    let _ = m;
}

unsafe fn mutex_lock(m: *mut PlatformMutex) {
    #[cfg(unix)]
    unsafe {
        libc::pthread_mutex_lock(m)
    };
    #[cfg(windows)]
    unsafe {
        AcquireSRWLockExclusive(m)
    };
}

unsafe fn mutex_unlock(m: *mut PlatformMutex) {
    #[cfg(unix)]
    unsafe {
        libc::pthread_mutex_unlock(m)
    };
    #[cfg(windows)]
    unsafe {
        ReleaseSRWLockExclusive(m)
    };
}

unsafe fn mutex_destroy(m: *mut PlatformMutex) {
    #[cfg(unix)]
    unsafe {
        libc::pthread_mutex_destroy(m)
    };
    #[cfg(windows)]
    let _ = m;
}

/// Maximum number of actors a scope can hold.
pub const HEW_SCOPE_MAX_ACTORS: usize = 64;

/// Structured concurrency scope — fixed-capacity actor container.
///
/// **Must be `#[repr(C)]`** because it is created on the stack in
/// compiler-generated code and returned by value from [`hew_scope_new`].
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
    let mut scope = HewScope {
        actors: [std::ptr::null_mut(); HEW_SCOPE_MAX_ACTORS],
        actor_count: 0,
        lock: MUTEX_INIT,
        cancelled: AtomicBool::new(false),
    };
    // SAFETY: `scope.lock` is a valid mutex ready for initialisation.
    unsafe {
        mutex_init(&raw mut scope.lock);
    }
    scope
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

/// Wait for all actors in the scope to finish, then free them.
///
/// Drains all mailboxes, closes actors, spin-waits until each actor
/// reaches `STOPPED`, and then frees the actor.
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
            // SAFETY: Lock is held.
            unsafe { mutex_unlock(&raw mut s.lock) };
            std::thread::sleep(std::time::Duration::from_micros(1000));
            // SAFETY: Lock was initialised.
            unsafe { mutex_lock(&raw mut s.lock) };
        }
    }

    // Phase 2: Close all mailboxes.
    for i in 0..s.actor_count as usize {
        if !s.actors[i].is_null() {
            // SAFETY: actor pointer is valid.
            unsafe { actor::hew_actor_close(s.actors[i].cast()) };
        }
    }
    // SAFETY: Lock is held.
    unsafe { mutex_unlock(&raw mut s.lock) };

    // Phase 3: Wait for all actors to reach STOPPED.
    for i in 0..s.actor_count as usize {
        let actor_ptr = s.actors[i].cast::<HewActor>();
        if actor_ptr.is_null() {
            continue;
        }
        // SAFETY: actor_ptr is valid.
        let a = unsafe { &*actor_ptr };
        while {
            let state = a.actor_state.load(Ordering::Acquire);
            state == HewActorState::Running as i32 || state == HewActorState::Runnable as i32
        } {
            std::thread::sleep(std::time::Duration::from_micros(100));
        }
        // CAS to STOPPED if not already.
        let state = a.actor_state.load(Ordering::Acquire);
        if state != HewActorState::Stopped as i32 {
            let _ = a.actor_state.compare_exchange(
                state,
                HewActorState::Stopped as i32,
                Ordering::AcqRel,
                Ordering::Acquire,
            );
        }
    }

    // Phase 4: Free all actors.
    for i in 0..s.actor_count as usize {
        let actor_ptr = s.actors[i].cast::<HewActor>();
        if actor_ptr.is_null() {
            continue;
        }
        // SAFETY: actor_ptr is valid.
        unsafe { actor::hew_actor_free(actor_ptr) };
        s.actors[i] = std::ptr::null_mut();
    }
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
