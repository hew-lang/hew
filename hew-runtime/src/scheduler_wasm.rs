//! Cooperative actor scheduler for WASM targets (single-threaded).
//!
//! This is the WASM counterpart of [`crate::scheduler`]. Since WASM runs
//! in a single-threaded environment, there is no work stealing, no thread
//! parking, and no concurrent CAS contention. State transitions use plain
//! atomic stores with `Relaxed` ordering, and the run queue is a simple
//! `VecDeque`.
//!
//! # C ABI
//!
//! - [`hew_sched_init`] — create the run queue.
//! - [`hew_sched_shutdown`] — drain the queue, reset state.
//! - [`hew_sched_run`] — run all actors to completion.
//!
//! # Internal API
//!
//! - [`sched_enqueue`] — submit an actor for scheduling.

use std::collections::VecDeque;
use std::ffi::c_void;
use std::sync::atomic::{AtomicI32, AtomicPtr, AtomicU64, Ordering};

use crate::internal::types::HewActorState;

// ── Constants ───────────────────────────────────────────────────────────

/// Default message processing budget per activation.
const HEW_MSG_BUDGET: i32 = 256;

/// Default reduction budget per dispatch call.
const HEW_DEFAULT_REDUCTIONS: i32 = 4000;

/// Priority: high (2x budget).
const HEW_PRIORITY_HIGH: i32 = 0;

/// Priority: normal (1x budget, default).
#[cfg(test)]
const HEW_PRIORITY_NORMAL: i32 = 1;

/// Priority: low (0.5x budget).
const HEW_PRIORITY_LOW: i32 = 2;

// ── HewActor layout (matches native actor.rs exactly) ───────────────────

/// Actor struct layout for WASM. Field order and types MUST match the
/// native [`crate::actor::HewActor`] definition to maintain C ABI
/// compatibility.
#[repr(C)]
#[derive(Debug)]
pub struct HewActor {
    pub sched_link_next: AtomicPtr<HewActor>,
    pub id: u64,
    pub pid: u64,
    pub state: *mut c_void,
    pub state_size: usize,
    pub dispatch: Option<unsafe extern "C" fn(*mut c_void, i32, *mut c_void, usize)>,
    pub mailbox: *mut c_void,
    pub actor_state: AtomicI32,
    pub budget: i32,
    pub init_state: *mut c_void,
    pub init_state_size: usize,
    pub coalesce_key_fn: Option<unsafe extern "C" fn(i32, *mut c_void, usize) -> u64>,
    pub error_code: AtomicI32,
    pub supervisor: *mut c_void,
    pub supervisor_child_index: i32,
    pub priority: AtomicI32,
    pub reductions: AtomicI32,
    pub idle_count: AtomicI32,
    pub hibernation_threshold: i32,
    pub hibernating: AtomicI32,
    pub prof_messages_processed: AtomicU64,
    pub prof_processing_time_ns: AtomicU64,
    pub arena: *mut c_void,
}

// SAFETY: Single-threaded on WASM; on native (tests), the struct is only
// used from one thread at a time.
unsafe impl Send for HewActor {}
unsafe impl Sync for HewActor {}

// ── HewMsgNode layout (matches native mailbox.rs) ───────────────────────

/// Message node layout. MUST match [`crate::mailbox::HewMsgNode`].
#[repr(C)]
#[derive(Debug)]
pub struct HewMsgNode {
    pub next: AtomicPtr<HewMsgNode>,
    pub msg_type: i32,
    pub data: *mut c_void,
    pub data_size: usize,
    pub reply_channel: *mut c_void,
}

// ── External mailbox functions ──────────────────────────────────────────
// Resolved at link time: from mailbox_wasm.rs on WASM, from mailbox.rs
// on native (tests).

extern "C" {
    fn hew_mailbox_try_recv(mb: *mut c_void) -> *mut HewMsgNode;
    fn hew_mailbox_has_messages(mb: *mut c_void) -> i32;
    fn hew_msg_node_free(node: *mut HewMsgNode);
}

// ── Global state (single-threaded, no atomics needed) ───────────────────

static mut RUN_QUEUE: Option<VecDeque<*mut HewActor>> = None;
static mut INITIALIZED: bool = false;

/// Whether an actor is currently being activated (for active_workers metric).
static mut ACTIVATING: bool = false;

/// The actor currently being dispatched (WASM equivalent of the native
/// thread-local `CURRENT_ACTOR`).
static mut CURRENT_ACTOR: *mut HewActor = std::ptr::null_mut();

/// Saved arena pointer during activation.
static mut PREV_ARENA: *mut c_void = std::ptr::null_mut();

// ── Metrics counters (plain u64, no atomics needed) ─────────────────────

static mut TASKS_SPAWNED: u64 = 0;
static mut TASKS_COMPLETED: u64 = 0;
static mut MESSAGES_SENT: u64 = 0;
static mut MESSAGES_RECEIVED: u64 = 0;

// ── C ABI ───────────────────────────────────────────────────────────────

/// Initialize the cooperative scheduler.
///
/// Creates the run queue. Calling more than once is a no-op.
#[cfg_attr(not(test), no_mangle)]
pub extern "C" fn hew_sched_init() {
    // SAFETY: Single-threaded on WASM.
    unsafe {
        if INITIALIZED {
            return;
        }
        RUN_QUEUE = Some(VecDeque::new());
        INITIALIZED = true;
    }
}

/// Shut down the cooperative scheduler.
///
/// Process all pending actors and then reset state. On WASM the
/// scheduler is cooperative, so we must drain the run queue (just like
/// [`hew_sched_run`]) before tearing down. Safe to call if the
/// scheduler was never initialized.
#[cfg_attr(not(test), no_mangle)]
pub extern "C" fn hew_sched_shutdown() {
    // Process all pending messages before shutting down.
    hew_sched_run();

    // SAFETY: Single-threaded on WASM.
    unsafe {
        RUN_QUEUE = None;
        INITIALIZED = false;
    }
}

/// Clean up all remaining runtime resources after shutdown.
///
/// WASM counterpart of the native `hew_runtime_cleanup()`. Frees any
/// actors not explicitly freed by user code and clears the registry.
#[cfg_attr(not(test), no_mangle)]
pub extern "C" fn hew_runtime_cleanup() {
    // Free all tracked actors.
    // SAFETY: Single-threaded on WASM, called after hew_sched_shutdown.
    unsafe { crate::actor::cleanup_all_actors() };
    // Clear the name registry.
    crate::registry::hew_registry_clear();
}

/// Run all enqueued actors to completion.
///
/// Loops until the run queue is empty: pops the front actor, activates
/// it, and re-enqueues it if it still has pending messages.
///
/// This is the main entry point for standalone WASM programs.
#[cfg_attr(not(test), no_mangle)]
pub extern "C" fn hew_sched_run() {
    // SAFETY: Single-threaded on WASM.
    unsafe {
        loop {
            let actor = match RUN_QUEUE {
                Some(ref mut q) => q.pop_front(),
                None => break,
            };
            let Some(actor) = actor else {
                break;
            };
            activate_actor_wasm(actor);

            // Re-enqueue if the actor is still runnable.
            let a = &*actor;
            let state = a.actor_state.load(Ordering::Relaxed);
            if state == HewActorState::Runnable as i32 {
                if let Some(ref mut q) = RUN_QUEUE {
                    q.push_back(actor);
                }
            }
        }
    }
}

// ── Internal API ────────────────────────────────────────────────────────

/// Submit an actor to the run queue.
///
/// # Safety
///
/// `actor` must be a valid pointer to a live `HewActor`.
pub unsafe fn sched_enqueue(actor: *mut HewActor) {
    // SAFETY: Single-threaded on WASM; caller guarantees actor validity.
    unsafe {
        TASKS_SPAWNED += 1;
        if let Some(ref mut q) = RUN_QUEUE {
            q.push_back(actor);
        }
    }
}

/// C ABI wrapper for [`sched_enqueue`], callable from [`crate::bridge`].
///
/// # Safety
///
/// `actor` must be a valid pointer to a live `HewActor`.
#[cfg_attr(not(test), no_mangle)]
pub unsafe extern "C" fn hew_wasm_sched_enqueue(actor: *mut c_void) {
    // SAFETY: Caller guarantees actor is a valid HewActor pointer.
    unsafe { sched_enqueue(actor.cast::<HewActor>()) };
}

/// Tick-based scheduler: run up to `max_activations` actor activations,
/// then return the number of actors still in the run queue.
///
/// This is the primary host-driven scheduling API. Unlike [`hew_sched_run`]
/// which runs to completion, this returns control to the host after a
/// bounded amount of work.
///
/// # Safety
///
/// The scheduler must have been initialized with [`hew_sched_init`].
#[cfg_attr(not(test), no_mangle)]
pub unsafe extern "C" fn hew_wasm_sched_tick(max_activations: i32) -> i32 {
    // SAFETY: Single-threaded on WASM.
    unsafe {
        let mut activations = 0i32;
        loop {
            if activations >= max_activations {
                break;
            }
            let actor = match RUN_QUEUE {
                Some(ref mut q) => q.pop_front(),
                None => break,
            };
            let Some(actor) = actor else {
                break;
            };
            activate_actor_wasm(actor);
            activations += 1;

            // Re-enqueue if the actor is still runnable.
            let a = &*actor;
            let state = a.actor_state.load(Ordering::Relaxed);
            if state == HewActorState::Runnable as i32 {
                if let Some(ref mut q) = RUN_QUEUE {
                    q.push_back(actor);
                }
            }
        }

        // Return remaining queue length.
        #[expect(
            clippy::cast_possible_truncation,
            clippy::cast_possible_wrap,
            reason = "run queue length will not exceed i32::MAX"
        )]
        match RUN_QUEUE {
            Some(ref q) => q.len() as i32,
            None => 0,
        }
    }
}

// ── Actor activation ────────────────────────────────────────────────────

/// Activate an actor: drain messages up to budget, then transition to
/// the appropriate state.
///
/// This is the WASM-simplified version of the native `activate_actor`.
/// Key differences from native:
/// - No signal recovery (`sigsetjmp`/`siglongjmp`) — no signals on WASM.
/// - No `ACTIVE_WORKERS` tracking (always 1 worker).
/// - No crash fault injection or delay faults.
/// - State transitions use plain `.store()` — single thread, no contention.
///   Atomics are still used because [`HewActor`] fields are `AtomicI32`.
///
/// # Safety
///
/// `actor` must be a valid pointer to a live `HewActor`.
#[expect(
    clippy::too_many_lines,
    reason = "actor activation state machine with multiple state transitions"
)]
unsafe fn activate_actor_wasm(actor: *mut HewActor) {
    // SAFETY: Only valid actor pointers are ever enqueued by the runtime.
    let a = unsafe { &*actor };

    // Skip terminal states.
    let state = a.actor_state.load(Ordering::Relaxed);
    if state == HewActorState::Stopped as i32 || state == HewActorState::Crashed as i32 {
        return;
    }

    // Transition: RUNNABLE -> RUNNING (plain store — single thread, no CAS needed).
    if state != HewActorState::Runnable as i32 {
        return;
    }
    a.actor_state
        .store(HewActorState::Running as i32, Ordering::Relaxed);

    // Compute budget with priority scaling.
    let base_budget = if a.budget > 0 {
        a.budget
    } else {
        HEW_MSG_BUDGET
    };
    let budget = match a.priority.load(Ordering::Relaxed) {
        HEW_PRIORITY_HIGH => base_budget.saturating_mul(2),
        HEW_PRIORITY_LOW => (base_budget / 2).max(1),
        _ => base_budget,
    };

    // SAFETY: Single-threaded global state access.
    unsafe {
        ACTIVATING = true;
        CURRENT_ACTOR = actor;
        PREV_ARENA = std::ptr::null_mut();
    }

    let mailbox = a.mailbox;
    let mut msgs_processed: u32 = 0;

    if !mailbox.is_null() {
        // Process up to `budget` messages.
        for _ in 0..budget {
            // SAFETY: mailbox pointer is valid for the lifetime of the actor.
            let msg = unsafe { hew_mailbox_try_recv(mailbox) };
            if msg.is_null() {
                break;
            }

            if let Some(dispatch) = a.dispatch {
                // Reset reduction counter for this dispatch.
                a.reductions
                    .store(HEW_DEFAULT_REDUCTIONS, Ordering::Relaxed);

                // SAFETY: `dispatch` and `a.state` are valid; message fields
                // come from a well-formed `HewMsgNode`.
                unsafe {
                    let msg_ref = &*msg;
                    dispatch(a.state, msg_ref.msg_type, msg_ref.data, msg_ref.data_size);
                }

                msgs_processed += 1;
                a.prof_messages_processed.fetch_add(1, Ordering::Relaxed);
                // Skip timing for now (use 0 for elapsed_ns). Timing can be
                // added later with WASI clock_time_get.
            }

            // SAFETY: `msg` was returned by `hew_mailbox_try_recv` and is
            // now exclusively owned by us.
            unsafe { hew_msg_node_free(msg) };

            // Check for mid-dispatch stop.
            let mid_state = a.actor_state.load(Ordering::Relaxed);
            if mid_state == HewActorState::Stopping as i32
                || mid_state == HewActorState::Stopped as i32
                || mid_state == HewActorState::Crashed as i32
            {
                break;
            }
        }
    }

    // Restore global state.
    // SAFETY: Single-threaded global state access.
    unsafe {
        CURRENT_ACTOR = std::ptr::null_mut();
        PREV_ARENA = std::ptr::null_mut();
        ACTIVATING = false;
        TASKS_COMPLETED += 1;
    }

    // ── Post-activation state transitions ───────────────────────────────

    let cur_state = a.actor_state.load(Ordering::Relaxed);

    // Stopping -> Stopped.
    if cur_state == HewActorState::Stopping as i32 {
        a.actor_state
            .store(HewActorState::Stopped as i32, Ordering::Relaxed);
        return;
    }

    // Already terminal — nothing to do.
    if cur_state == HewActorState::Stopped as i32 || cur_state == HewActorState::Crashed as i32 {
        return;
    }

    // Hibernation tracking.
    if msgs_processed == 0 && a.hibernation_threshold > 0 {
        let prev_idle = a.idle_count.fetch_add(1, Ordering::Relaxed);
        if prev_idle + 1 >= a.hibernation_threshold {
            a.hibernating.store(1, Ordering::Relaxed);
        }
    } else if msgs_processed > 0 {
        a.idle_count.store(0, Ordering::Relaxed);
        a.hibernating.store(0, Ordering::Relaxed);
    }

    // Check for remaining messages.
    let has_more = if mailbox.is_null() {
        false
    } else {
        // SAFETY: mailbox pointer is valid.
        unsafe { hew_mailbox_has_messages(mailbox) != 0 }
    };

    if has_more {
        // More work pending -> RUNNING -> RUNNABLE.
        a.actor_state
            .store(HewActorState::Runnable as i32, Ordering::Relaxed);
        // NOTE: The caller (hew_sched_run) handles re-enqueue by checking
        // the actor state after activation.
    } else {
        // No more messages -> RUNNING -> IDLE.
        a.actor_state
            .store(HewActorState::Idle as i32, Ordering::Relaxed);

        // Recheck: messages may have arrived during activation. On WASM
        // this is less likely (single-threaded), but host callbacks or
        // dispatch-triggered sends can enqueue messages.
        if !mailbox.is_null()
            // SAFETY: mailbox pointer is valid.
            && unsafe { hew_mailbox_has_messages(mailbox) != 0 }
        {
            // Messages appeared -> IDLE -> RUNNABLE.
            a.actor_state
                .store(HewActorState::Runnable as i32, Ordering::Relaxed);
            // SAFETY: actor is valid.
            unsafe { sched_enqueue(actor) };
        }
    }
}

// ── Metrics C ABI ───────────────────────────────────────────────────────

/// Return the total number of tasks spawned (enqueued) since startup or last reset.
#[cfg_attr(not(test), no_mangle)]
pub extern "C" fn hew_sched_metrics_tasks_spawned() -> u64 {
    // SAFETY: Single-threaded on WASM.
    unsafe { TASKS_SPAWNED }
}

/// Return the total number of actor activations completed since startup or last reset.
#[cfg_attr(not(test), no_mangle)]
pub extern "C" fn hew_sched_metrics_tasks_completed() -> u64 {
    // SAFETY: Single-threaded on WASM.
    unsafe { TASKS_COMPLETED }
}

/// Return the total number of work-steals. Always 0 on WASM (no stealing).
#[cfg_attr(not(test), no_mangle)]
pub extern "C" fn hew_sched_metrics_steals() -> u64 {
    0
}

/// Return the total number of messages sent since startup or last reset.
#[cfg_attr(not(test), no_mangle)]
pub extern "C" fn hew_sched_metrics_messages_sent() -> u64 {
    // SAFETY: Single-threaded on WASM.
    unsafe { MESSAGES_SENT }
}

/// Return the total number of messages received since startup or last reset.
#[cfg_attr(not(test), no_mangle)]
pub extern "C" fn hew_sched_metrics_messages_received() -> u64 {
    // SAFETY: Single-threaded on WASM.
    unsafe { MESSAGES_RECEIVED }
}

/// Return the number of workers currently processing actors.
/// On WASM, returns 1 during activation, 0 otherwise.
#[cfg_attr(not(test), no_mangle)]
pub extern "C" fn hew_sched_metrics_active_workers() -> u64 {
    // SAFETY: Single-threaded on WASM.
    unsafe {
        if ACTIVATING {
            1
        } else {
            0
        }
    }
}

/// Reset all scheduler metrics counters to zero.
#[cfg_attr(not(test), no_mangle)]
pub extern "C" fn hew_sched_metrics_reset() {
    // SAFETY: Single-threaded on WASM.
    unsafe {
        TASKS_SPAWNED = 0;
        TASKS_COMPLETED = 0;
        MESSAGES_SENT = 0;
        MESSAGES_RECEIVED = 0;
    }
}

/// Return the total number of worker threads. Always 1 on WASM.
#[cfg_attr(not(test), no_mangle)]
pub extern "C" fn hew_sched_metrics_worker_count() -> u64 {
    1
}

/// Return the approximate length of the global run queue.
#[cfg_attr(not(test), no_mangle)]
pub extern "C" fn hew_sched_metrics_global_queue_len() -> u64 {
    // SAFETY: Single-threaded on WASM.
    unsafe {
        match RUN_QUEUE {
            Some(ref q) => q.len() as u64,
            None => 0,
        }
    }
}

// ── Tests ───────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use std::ptr;
    use std::sync::Mutex;

    /// Serialize all tests in this module since they share `static mut`
    /// global state. Rust's test harness runs tests in parallel by
    /// default; without this lock, concurrent mutation of the globals
    /// causes undefined behavior.
    static TEST_LOCK: Mutex<()> = Mutex::new(());

    /// Build a minimal `HewActor` with sensible defaults.
    fn stub_actor() -> HewActor {
        HewActor {
            sched_link_next: AtomicPtr::new(ptr::null_mut()),
            id: 1,
            pid: 0,
            state: ptr::null_mut(),
            state_size: 0,
            dispatch: None,
            mailbox: ptr::null_mut(),
            actor_state: AtomicI32::new(HewActorState::Runnable as i32),
            budget: HEW_MSG_BUDGET,
            init_state: ptr::null_mut(),
            init_state_size: 0,
            coalesce_key_fn: None,
            error_code: AtomicI32::new(0),
            supervisor: ptr::null_mut(),
            supervisor_child_index: -1,
            priority: AtomicI32::new(HEW_PRIORITY_NORMAL),
            reductions: AtomicI32::new(HEW_DEFAULT_REDUCTIONS),
            idle_count: AtomicI32::new(0),
            hibernation_threshold: 0,
            hibernating: AtomicI32::new(0),
            prof_messages_processed: AtomicU64::new(0),
            prof_processing_time_ns: AtomicU64::new(0),
            arena: ptr::null_mut(),
        }
    }

    /// Reset all global state between tests.
    ///
    /// # Safety
    ///
    /// Must not be called concurrently with other test code (Rust test
    /// harness serialises tests within the same module by default).
    unsafe fn reset_globals() {
        // SAFETY: Single-threaded test environment. Use raw pointer
        // writes to avoid creating references to mutable statics.
        unsafe {
            ptr::addr_of_mut!(RUN_QUEUE).write(None);
            ptr::addr_of_mut!(INITIALIZED).write(false);
            ptr::addr_of_mut!(ACTIVATING).write(false);
            ptr::addr_of_mut!(CURRENT_ACTOR).write(ptr::null_mut());
            ptr::addr_of_mut!(PREV_ARENA).write(ptr::null_mut());
            ptr::addr_of_mut!(TASKS_SPAWNED).write(0);
            ptr::addr_of_mut!(TASKS_COMPLETED).write(0);
            ptr::addr_of_mut!(MESSAGES_SENT).write(0);
            ptr::addr_of_mut!(MESSAGES_RECEIVED).write(0);
        }
    }

    /// Read INITIALIZED without creating a shared reference.
    unsafe fn read_initialized() -> bool {
        unsafe { ptr::addr_of!(INITIALIZED).read() }
    }

    /// Read TASKS_SPAWNED without creating a shared reference.
    unsafe fn read_tasks_spawned() -> u64 {
        unsafe { ptr::addr_of!(TASKS_SPAWNED).read() }
    }

    /// Read TASKS_COMPLETED without creating a shared reference.
    unsafe fn read_tasks_completed() -> u64 {
        unsafe { ptr::addr_of!(TASKS_COMPLETED).read() }
    }

    /// Read the run queue length without creating a shared reference.
    unsafe fn read_queue_len() -> usize {
        // SAFETY: Single-threaded test — no concurrent mutation.
        unsafe {
            let q_ptr = ptr::addr_of!(RUN_QUEUE);
            match &*q_ptr {
                Some(q) => q.len(),
                None => 0,
            }
        }
    }

    /// Check if the run queue exists (Some) without creating a shared ref.
    unsafe fn run_queue_exists() -> bool {
        // SAFETY: Single-threaded test — no concurrent mutation.
        unsafe {
            let q_ptr = ptr::addr_of!(RUN_QUEUE);
            (*q_ptr).is_some()
        }
    }

    #[test]
    fn init_and_shutdown_dont_panic() {
        let _guard = TEST_LOCK.lock().unwrap();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };

        hew_sched_init();
        // SAFETY: Single-threaded test.
        unsafe {
            assert!(read_initialized());
            assert!(run_queue_exists());
        }

        hew_sched_shutdown();
        // SAFETY: Single-threaded test.
        unsafe {
            assert!(!read_initialized());
            assert!(!run_queue_exists());
        }
    }

    #[test]
    fn double_init_is_noop() {
        let _guard = TEST_LOCK.lock().unwrap();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };

        hew_sched_init();
        hew_sched_init(); // Should not panic or create a second queue.

        // SAFETY: Single-threaded test.
        unsafe {
            assert!(read_initialized());
        }

        hew_sched_shutdown();
    }

    #[test]
    fn enqueue_and_run_with_null_mailbox() {
        let _guard = TEST_LOCK.lock().unwrap();
        // An actor with no mailbox (null) should transition from
        // Runnable -> Running -> Idle after activation.
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();

        let actor = stub_actor();
        let actor_ptr: *mut HewActor = &actor as *const HewActor as *mut HewActor;

        // SAFETY: actor is valid, scheduler is initialized.
        unsafe { sched_enqueue(actor_ptr) };

        // SAFETY: Single-threaded test.
        unsafe {
            assert_eq!(read_tasks_spawned(), 1);
            assert_eq!(read_queue_len(), 1);
        }

        hew_sched_run();

        assert_eq!(
            actor.actor_state.load(Ordering::Relaxed),
            HewActorState::Idle as i32,
            "actor with null mailbox should transition to Idle"
        );

        // SAFETY: Single-threaded test.
        unsafe {
            assert_eq!(read_tasks_completed(), 1);
        }

        hew_sched_shutdown();
    }

    #[test]
    fn activate_skips_stopped_actor() {
        let _guard = TEST_LOCK.lock().unwrap();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();

        let actor = stub_actor();
        actor
            .actor_state
            .store(HewActorState::Stopped as i32, Ordering::Relaxed);
        let actor_ptr: *mut HewActor = &actor as *const HewActor as *mut HewActor;

        // SAFETY: actor is valid.
        unsafe { activate_actor_wasm(actor_ptr) };

        assert_eq!(
            actor.actor_state.load(Ordering::Relaxed),
            HewActorState::Stopped as i32,
            "stopped actor should remain stopped"
        );

        hew_sched_shutdown();
    }

    #[test]
    fn activate_skips_crashed_actor() {
        let _guard = TEST_LOCK.lock().unwrap();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();

        let actor = stub_actor();
        actor
            .actor_state
            .store(HewActorState::Crashed as i32, Ordering::Relaxed);
        let actor_ptr: *mut HewActor = &actor as *const HewActor as *mut HewActor;

        // SAFETY: actor is valid.
        unsafe { activate_actor_wasm(actor_ptr) };

        assert_eq!(
            actor.actor_state.load(Ordering::Relaxed),
            HewActorState::Crashed as i32,
            "crashed actor should remain crashed"
        );

        hew_sched_shutdown();
    }

    #[test]
    fn activate_skips_idle_actor() {
        let _guard = TEST_LOCK.lock().unwrap();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();

        let actor = stub_actor();
        actor
            .actor_state
            .store(HewActorState::Idle as i32, Ordering::Relaxed);
        let actor_ptr: *mut HewActor = &actor as *const HewActor as *mut HewActor;

        // SAFETY: actor is valid.
        unsafe { activate_actor_wasm(actor_ptr) };

        // State should remain IDLE (only RUNNABLE actors get activated).
        assert_eq!(
            actor.actor_state.load(Ordering::Relaxed),
            HewActorState::Idle as i32,
            "idle actor should remain idle"
        );

        hew_sched_shutdown();
    }

    #[test]
    fn metrics_counters_increment() {
        let _guard = TEST_LOCK.lock().unwrap();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();

        let actor = stub_actor();
        let actor_ptr: *mut HewActor = &actor as *const HewActor as *mut HewActor;

        // SAFETY: actor is valid.
        unsafe { sched_enqueue(actor_ptr) };
        assert_eq!(hew_sched_metrics_tasks_spawned(), 1);

        hew_sched_run();
        assert_eq!(hew_sched_metrics_tasks_completed(), 1);
        assert_eq!(hew_sched_metrics_steals(), 0);
        assert_eq!(hew_sched_metrics_worker_count(), 1);
        assert_eq!(hew_sched_metrics_active_workers(), 0);

        hew_sched_shutdown();
    }

    #[test]
    fn metrics_reset() {
        let _guard = TEST_LOCK.lock().unwrap();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();

        let actor = stub_actor();
        let actor_ptr: *mut HewActor = &actor as *const HewActor as *mut HewActor;

        // SAFETY: actor is valid.
        unsafe { sched_enqueue(actor_ptr) };
        hew_sched_run();

        assert!(hew_sched_metrics_tasks_spawned() > 0);
        assert!(hew_sched_metrics_tasks_completed() > 0);

        hew_sched_metrics_reset();

        assert_eq!(hew_sched_metrics_tasks_spawned(), 0);
        assert_eq!(hew_sched_metrics_tasks_completed(), 0);
        assert_eq!(hew_sched_metrics_messages_sent(), 0);
        assert_eq!(hew_sched_metrics_messages_received(), 0);

        hew_sched_shutdown();
    }

    #[test]
    fn global_queue_len_reflects_enqueued_actors() {
        let _guard = TEST_LOCK.lock().unwrap();
        // SAFETY: Serialized by TEST_LOCK — no concurrent access.
        unsafe { reset_globals() };
        hew_sched_init();

        assert_eq!(hew_sched_metrics_global_queue_len(), 0);

        let actor1 = stub_actor();
        let actor2 = stub_actor();
        let ptr1: *mut HewActor = &actor1 as *const HewActor as *mut HewActor;
        let ptr2: *mut HewActor = &actor2 as *const HewActor as *mut HewActor;

        // SAFETY: actors are valid.
        unsafe {
            sched_enqueue(ptr1);
            sched_enqueue(ptr2);
        }
        assert_eq!(hew_sched_metrics_global_queue_len(), 2);

        hew_sched_run();
        assert_eq!(hew_sched_metrics_global_queue_len(), 0);

        hew_sched_shutdown();
    }
}
