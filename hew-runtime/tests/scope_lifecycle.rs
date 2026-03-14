//! Scope lifecycle integration tests.
//!
//! Tests structured concurrency scopes: creation, actor tracking,
//! cancellation propagation, and cleanup behaviour.  The scope module
//! has only ~40 % line coverage — these tests target the gaps.

// FFI test harness — safety invariants documented per-test.
#![expect(
    clippy::undocumented_unsafe_blocks,
    reason = "FFI test harness — safety invariants are documented per-test"
)]
#![expect(
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap,
    reason = "FFI tests use deliberate casts between pointer and integer types"
)]

use std::ffi::c_void;
use std::ptr;
use std::sync::atomic::{AtomicI32, Ordering};
use std::sync::{Condvar, Mutex};
use std::time::{Duration, Instant};

use hew_runtime::actor::{hew_actor_free, hew_actor_send, hew_actor_spawn};
use hew_runtime::scope::{
    hew_scope_cancel, hew_scope_create, hew_scope_destroy, hew_scope_free, hew_scope_is_cancelled,
    hew_scope_new, hew_scope_spawn, hew_scope_wait_all, HEW_SCOPE_MAX_ACTORS,
};

// ── Global scheduler init ───────────────────────────────────────────────

static SCHED_INIT: std::sync::Once = std::sync::Once::new();

fn ensure_scheduler() {
    SCHED_INIT.call_once(|| {
        hew_runtime::scheduler::hew_sched_init();
    });
}

// ── Condvar helper ──────────────────────────────────────────────────────

struct Signal {
    count: Mutex<i32>,
    cond: Condvar,
}

impl Signal {
    const fn new() -> Self {
        Self {
            count: Mutex::new(0),
            cond: Condvar::new(),
        }
    }

    fn reset(&self) {
        *self.count.lock().unwrap() = 0;
    }

    fn increment(&self) {
        let mut c = self.count.lock().unwrap();
        *c += 1;
        self.cond.notify_all();
    }

    fn wait_for(&self, expected: i32, timeout: Duration) -> bool {
        let mut count = self.count.lock().unwrap();
        let deadline = Instant::now() + timeout;
        while *count < expected {
            let remaining = deadline.saturating_duration_since(Instant::now());
            if remaining.is_zero() {
                return false;
            }
            let (guard, result) = self.cond.wait_timeout(count, remaining).unwrap();
            count = guard;
            if result.timed_out() && *count < expected {
                return false;
            }
        }
        true
    }
}

// ── Shared dispatch functions ───────────────────────────────────────────

unsafe extern "C" fn noop_dispatch(
    _state: *mut c_void,
    _msg_type: i32,
    _data: *mut c_void,
    _size: usize,
) {
}

// ═══════════════════════════════════════════════════════════════════════
// 1. Scope creation and basic properties
// ═══════════════════════════════════════════════════════════════════════

/// A freshly created stack-allocated scope starts with zero actors and
/// is not cancelled.
#[test]
fn scope_new_initial_state() {
    unsafe {
        let mut scope = hew_scope_new();
        assert_eq!(scope.actor_count, 0, "new scope should have no actors");
        assert_eq!(
            hew_scope_is_cancelled(&raw mut scope),
            0,
            "new scope should not be cancelled"
        );
        hew_scope_destroy(&raw mut scope);
    }
}

/// A heap-allocated scope behaves identically to a stack-allocated one.
#[test]
fn scope_create_heap_allocated() {
    unsafe {
        let scope = hew_scope_create();
        assert!(!scope.is_null(), "hew_scope_create must return non-null");
        assert_eq!((*scope).actor_count, 0);
        assert_eq!(hew_scope_is_cancelled(scope), 0);
        hew_scope_free(scope);
    }
}

// ═══════════════════════════════════════════════════════════════════════
// 2. Scope spawn — adding actors
// ═══════════════════════════════════════════════════════════════════════

/// Spawning actors into a scope increments the actor count.
#[test]
fn scope_spawn_tracks_actors() {
    unsafe {
        let mut scope = hew_scope_new();
        let a = hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch));
        let b = hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch));

        assert_eq!(
            hew_scope_spawn(&raw mut scope, a.cast()),
            0,
            "spawn should succeed"
        );
        assert_eq!(scope.actor_count, 1);

        assert_eq!(
            hew_scope_spawn(&raw mut scope, b.cast()),
            0,
            "spawn should succeed"
        );
        assert_eq!(scope.actor_count, 2);

        // Clean up: close actors so wait_all can proceed.
        hew_runtime::actor::hew_actor_close(a);
        hew_runtime::actor::hew_actor_close(b);
        hew_scope_wait_all(&raw mut scope);
        hew_scope_destroy(&raw mut scope);
    }
}

/// Exceeding `HEW_SCOPE_MAX_ACTORS` returns -1.
#[test]
fn scope_spawn_rejects_when_full() {
    unsafe {
        let mut scope = hew_scope_new();

        // Fill the scope to capacity with dummy (null) pointers.
        // We cheat slightly: directly set the count to max.
        scope.actor_count = HEW_SCOPE_MAX_ACTORS as i32;

        let actor = hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch));
        let rc = hew_scope_spawn(&raw mut scope, actor.cast());
        assert_eq!(rc, -1, "scope should reject when full");

        // Reset count and clean up.
        scope.actor_count = 0;
        hew_actor_free(actor);
        hew_scope_destroy(&raw mut scope);
    }
}

// ═══════════════════════════════════════════════════════════════════════
// 3. Scope cancellation
// ═══════════════════════════════════════════════════════════════════════

/// Setting the cancellation flag is visible via `hew_scope_is_cancelled`.
#[test]
fn scope_cancel_sets_flag() {
    unsafe {
        let mut scope = hew_scope_new();
        assert_eq!(hew_scope_is_cancelled(&raw mut scope), 0);

        hew_scope_cancel(&raw mut scope);
        assert_eq!(
            hew_scope_is_cancelled(&raw mut scope),
            1,
            "cancellation flag should be set"
        );

        hew_scope_destroy(&raw mut scope);
    }
}

/// Cancelling a null scope is a safe no-op.
#[test]
fn scope_cancel_null_is_noop() {
    unsafe {
        hew_scope_cancel(ptr::null_mut());
        // Reaching here without a crash is the assertion.
    }
}

/// `hew_scope_is_cancelled` returns 0 for a null pointer.
#[test]
fn scope_is_cancelled_null_returns_zero() {
    unsafe {
        assert_eq!(hew_scope_is_cancelled(ptr::null_mut()), 0);
    }
}

// ═══════════════════════════════════════════════════════════════════════
// 4. Scope wait_all — actor cleanup
// ═══════════════════════════════════════════════════════════════════════

/// `hew_scope_wait_all` drains actor mailboxes, closes actors, waits for
/// them to reach Stopped, and frees them.
#[test]
fn scope_wait_all_cleans_up_actors() {
    static SCOPE_SIGNAL: Signal = Signal::new();
    static SCOPE_LOCK: Mutex<()> = Mutex::new(());

    unsafe extern "C" fn scope_dispatch(
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _size: usize,
    ) {
        SCOPE_SIGNAL.increment();
    }

    let _guard = SCOPE_LOCK.lock().unwrap();
    ensure_scheduler();
    SCOPE_SIGNAL.reset();

    unsafe {
        let mut scope = hew_scope_new();
        let mut state: i32 = 0;
        let actor = hew_actor_spawn(
            (&raw mut state).cast(),
            size_of::<i32>(),
            Some(scope_dispatch),
        );
        assert!(!actor.is_null());

        hew_scope_spawn(&raw mut scope, actor.cast());

        // Send a message so the actor has work to do.
        hew_actor_send(actor, 1, ptr::null_mut(), 0);
        assert!(
            SCOPE_SIGNAL.wait_for(1, Duration::from_secs(10)),
            "dispatch not invoked"
        );

        // wait_all should: drain mailbox → close → wait for Stopped → free.
        hew_scope_wait_all(&raw mut scope);

        // After wait_all, the scope's actor slots should be nulled out.
        assert!(
            scope.actors[0].is_null(),
            "wait_all should null out actor slots after freeing"
        );

        hew_scope_destroy(&raw mut scope);
    }
}

/// `hew_scope_wait_all` on an empty scope is a safe no-op.
#[test]
fn scope_wait_all_empty_is_noop() {
    unsafe {
        let mut scope = hew_scope_new();
        hew_scope_wait_all(&raw mut scope);
        // Reaching here without a crash proves the empty-scope path works.
        hew_scope_destroy(&raw mut scope);
    }
}

// ═══════════════════════════════════════════════════════════════════════
// 5. Scope with multiple actors
// ═══════════════════════════════════════════════════════════════════════

/// Multiple actors in a scope are all cleaned up by `wait_all`.
#[test]
fn scope_wait_all_multiple_actors() {
    static MULTI_SIGNAL: Signal = Signal::new();
    static MULTI_LOCK: Mutex<()> = Mutex::new(());

    unsafe extern "C" fn multi_dispatch(
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _size: usize,
    ) {
        MULTI_SIGNAL.increment();
    }

    let _guard = MULTI_LOCK.lock().unwrap();
    ensure_scheduler();
    MULTI_SIGNAL.reset();

    unsafe {
        let mut scope = hew_scope_new();

        // Spawn 3 actors into the scope.
        let mut actors = Vec::new();
        for _ in 0..3 {
            let actor = hew_actor_spawn(ptr::null_mut(), 0, Some(multi_dispatch));
            assert!(!actor.is_null());
            hew_scope_spawn(&raw mut scope, actor.cast());
            actors.push(actor);
        }
        assert_eq!(scope.actor_count, 3);

        // Send one message to each actor.
        for &actor in &actors {
            hew_actor_send(actor, 1, ptr::null_mut(), 0);
        }
        assert!(
            MULTI_SIGNAL.wait_for(3, Duration::from_secs(10)),
            "all 3 dispatches should complete"
        );

        // Clean up all actors via scope.
        hew_scope_wait_all(&raw mut scope);

        // All slots should be nulled out.
        for i in 0..3 {
            assert!(
                scope.actors[i].is_null(),
                "actor slot {i} should be null after wait_all"
            );
        }

        hew_scope_destroy(&raw mut scope);
    }
}

// ═══════════════════════════════════════════════════════════════════════
// 6. Nested scope cancellation
// ═══════════════════════════════════════════════════════════════════════

/// Record whether the dispatch function observed cancellation.
static CANCEL_OBSERVED: AtomicI32 = AtomicI32::new(0);
static CANCEL_SIGNAL: Signal = Signal::new();
static CANCEL_LOCK: Mutex<()> = Mutex::new(());

/// Dispatch that checks a scope's cancellation flag.  The scope pointer
/// is passed as the actor's state.
unsafe extern "C" fn cancel_checking_dispatch(
    state: *mut c_void,
    _msg_type: i32,
    _data: *mut c_void,
    _size: usize,
) {
    if !state.is_null() {
        // State holds the scope pointer (as usize, then cast).
        let scope_ptr = unsafe { *(state.cast::<usize>()) };
        if scope_ptr != 0 {
            let cancelled = unsafe { hew_scope_is_cancelled(scope_ptr as *mut _) };
            CANCEL_OBSERVED.store(cancelled, Ordering::Release);
        }
    }
    CANCEL_SIGNAL.increment();
}

/// Cancel a parent scope before the actor dispatches and verify the
/// actor can observe the cancellation.
#[test]
fn scope_cancellation_visible_to_actors() {
    let _guard = CANCEL_LOCK.lock().unwrap();
    ensure_scheduler();
    CANCEL_SIGNAL.reset();
    CANCEL_OBSERVED.store(0, Ordering::Release);

    unsafe {
        let scope = hew_scope_create();
        assert!(!scope.is_null());

        // Store the scope pointer as the actor's state so the dispatch
        // function can read the cancellation flag.
        let mut scope_addr: usize = scope as usize;
        let actor = hew_actor_spawn(
            (&raw mut scope_addr).cast(),
            size_of::<usize>(),
            Some(cancel_checking_dispatch),
        );
        assert!(!actor.is_null());
        hew_scope_spawn(scope, actor.cast());

        // Cancel the scope BEFORE sending the message.
        hew_scope_cancel(scope);
        assert_eq!(hew_scope_is_cancelled(scope), 1);

        // Send a message — the dispatch function should see cancellation = 1.
        hew_actor_send(actor, 1, ptr::null_mut(), 0);
        assert!(
            CANCEL_SIGNAL.wait_for(1, Duration::from_secs(10)),
            "dispatch was not invoked"
        );

        let observed = CANCEL_OBSERVED.load(Ordering::Acquire);
        assert_eq!(
            observed, 1,
            "actor should observe the scope's cancellation flag"
        );

        hew_scope_wait_all(scope);
        hew_scope_free(scope);
    }
}

/// Two nested heap-allocated scopes: cancelling the parent also cancels
/// the child (manually propagated in this test, since the runtime uses
/// cooperative cancellation — the parent propagates to child scopes).
#[test]
fn nested_scopes_cancellation_propagation() {
    unsafe {
        let parent = hew_scope_create();
        let child = hew_scope_create();
        assert!(!parent.is_null());
        assert!(!child.is_null());

        assert_eq!(hew_scope_is_cancelled(parent), 0);
        assert_eq!(hew_scope_is_cancelled(child), 0);

        // Cancel parent.
        hew_scope_cancel(parent);
        assert_eq!(hew_scope_is_cancelled(parent), 1);

        // Propagate to child (cooperative — the runtime expects user code
        // or generated code to propagate cancellation down the scope tree).
        if hew_scope_is_cancelled(parent) != 0 {
            hew_scope_cancel(child);
        }
        assert_eq!(
            hew_scope_is_cancelled(child),
            1,
            "child scope should be cancelled after propagation"
        );

        hew_scope_free(child);
        hew_scope_free(parent);
    }
}

// ═══════════════════════════════════════════════════════════════════════
// 7. Scope destroy resets state
// ═══════════════════════════════════════════════════════════════════════

/// After `hew_scope_destroy`, `actor_count` is 0 and actor slots are null.
#[test]
fn scope_destroy_resets_fields() {
    unsafe {
        let mut scope = hew_scope_new();

        // Simulate adding actors by incrementing count manually.
        scope.actor_count = 3;
        scope.actors[0] = 0x1234 as *mut c_void; // dummy non-null pointer
        scope.actors[1] = 0x5678 as *mut c_void;
        scope.actors[2] = 0x9abc as *mut c_void;

        hew_scope_destroy(&raw mut scope);

        assert_eq!(
            scope.actor_count, 0,
            "destroy should reset actor_count to 0"
        );
        for i in 0..3 {
            assert!(
                scope.actors[i].is_null(),
                "destroy should null out actor slot {i}"
            );
        }
    }
}

// ═══════════════════════════════════════════════════════════════════════
// 8. Scope free (heap) null is a safe no-op
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn scope_free_null_is_noop() {
    unsafe {
        hew_scope_free(ptr::null_mut());
        // Reaching here without a crash is the assertion.
    }
}
