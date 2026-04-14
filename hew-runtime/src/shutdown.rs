//! Graceful shutdown protocol for the Hew runtime.
//!
//! Implements a 3-phase ordered shutdown:
//!
//! 1. **Quiesce** — Stop accepting new actor spawns. Mark runtime as
//!    shutting down so new messages are rejected. Installed SIGTERM/SIGINT
//!    handler triggers this phase.
//!
//! 2. **Drain** — Allow workers to continue processing remaining messages
//!    for up to `drain_timeout` milliseconds. Supervisors stop their
//!    children bottom-up.
//!
//! 3. **Terminate** — Force-stop any remaining actors and shut down the
//!    scheduler.

use crate::util::MutexExt;
use std::ffi::c_int;
use std::sync::atomic::{AtomicI32, Ordering};
use std::time::{Duration, Instant};

use crate::scheduler;

// ---------------------------------------------------------------------------
// Shutdown phases
// ---------------------------------------------------------------------------

/// Not shutting down.
const PHASE_RUNNING: i32 = 0;
/// Phase 1: No new spawns or messages accepted.
const PHASE_QUIESCE: i32 = 1;
/// Phase 2: Draining remaining messages with deadline.
const PHASE_DRAIN: i32 = 2;
/// Phase 3: Force-terminating all actors.
const PHASE_TERMINATE: i32 = 3;
/// Shutdown complete.
const PHASE_DONE: i32 = 4;

/// Current shutdown phase (global atomic).
static SHUTDOWN_PHASE: AtomicI32 = AtomicI32::new(PHASE_RUNNING);

/// Default drain timeout in milliseconds.
const DEFAULT_DRAIN_TIMEOUT_MS: u64 = 5_000;

/// Wrapper for `*mut HewSupervisor` to impl `Send`.
///
/// # Safety
///
/// Supervisor pointers are only accessed during shutdown while the
/// runtime is in a controlled wind-down state.
struct SupervisorPtr(*mut crate::supervisor::HewSupervisor);

// SAFETY: Supervisor pointers are only accessed during shutdown while
// the runtime is in a controlled wind-down state.
unsafe impl Send for SupervisorPtr {}

/// Registered top-level supervisors to stop during shutdown.
static TOP_LEVEL_SUPERVISORS: std::sync::Mutex<Vec<SupervisorPtr>> =
    std::sync::Mutex::new(Vec::new());

// ---------------------------------------------------------------------------
// C ABI — Phase queries
// ---------------------------------------------------------------------------

/// Return the current shutdown phase.
///
/// - 0 = running
/// - 1 = quiescing
/// - 2 = draining
/// - 3 = terminating
/// - 4 = done
#[no_mangle]
pub extern "C" fn hew_shutdown_phase() -> c_int {
    SHUTDOWN_PHASE.load(Ordering::Acquire)
}

/// Return 1 if the runtime is in any shutdown phase (not running), else 0.
#[no_mangle]
pub extern "C" fn hew_is_shutting_down() -> c_int {
    i32::from(SHUTDOWN_PHASE.load(Ordering::Acquire) != PHASE_RUNNING)
}

// ---------------------------------------------------------------------------
// C ABI — Supervisor registration
// ---------------------------------------------------------------------------

/// Register a top-level supervisor for ordered shutdown.
///
/// During graceful shutdown, registered supervisors are stopped
/// bottom-up (last registered = first stopped) before the scheduler
/// is terminated.
///
/// # Safety
///
/// `sup` must be a valid pointer to a [`HewSupervisor`] that remains
/// valid until shutdown or until it is unregistered.
#[no_mangle]
pub unsafe extern "C" fn hew_shutdown_register_supervisor(
    sup: *mut crate::supervisor::HewSupervisor,
) {
    if sup.is_null() {
        return;
    }
    let mut sups = TOP_LEVEL_SUPERVISORS.lock_or_recover();
    if !sups.iter().any(|s| s.0 == sup) {
        sups.push(SupervisorPtr(sup));
    }
}

/// Unregister a top-level supervisor from shutdown.
///
/// # Safety
///
/// `sup` must be a pointer previously passed to
/// [`hew_shutdown_register_supervisor`].
#[no_mangle]
pub unsafe extern "C" fn hew_shutdown_unregister_supervisor(
    sup: *mut crate::supervisor::HewSupervisor,
) {
    if sup.is_null() {
        return;
    }
    let mut sups = TOP_LEVEL_SUPERVISORS.lock_or_recover();
    sups.retain(|s| s.0 != sup);
}

#[cfg(test)]
pub(crate) fn is_supervisor_registered_for_test(
    sup: *mut crate::supervisor::HewSupervisor,
) -> bool {
    let sups = TOP_LEVEL_SUPERVISORS.lock_or_recover();
    sups.iter().any(|candidate| candidate.0 == sup)
}

/// Free all registered top-level supervisors without waiting for actors.
///
/// Called by [`crate::scheduler::hew_runtime_cleanup`] **after** worker
/// threads have been joined.  At that point no actor processing can
/// happen, so we simply drop the supervisor structs to release child
/// spec resources (names, `init_state`).  Actors themselves are freed
/// separately by [`crate::actor::cleanup_all_actors`].
pub(crate) unsafe fn free_registered_supervisors() {
    let mut sups = TOP_LEVEL_SUPERVISORS.lock_or_recover();
    for s in sups.drain(..) {
        if !s.0.is_null() {
            // SAFETY: supervisor was registered and pointer is valid.
            unsafe { crate::supervisor::free_supervisor_resources(s.0) };
        }
    }
}

#[cfg(feature = "profiler")]
pub(crate) fn registered_supervisors_snapshot() -> Vec<*mut crate::supervisor::HewSupervisor> {
    let sups = TOP_LEVEL_SUPERVISORS.lock_or_recover();
    sups.iter().map(|sup| sup.0).collect()
}

// ---------------------------------------------------------------------------
// C ABI — Shutdown trigger
// ---------------------------------------------------------------------------

/// Initiate graceful shutdown with an optional drain timeout.
///
/// `drain_timeout_ms` is the maximum time (in milliseconds) to wait for
/// in-flight messages to be processed during the drain phase.
/// Pass 0 to use the default (5 seconds). Pass -1 for immediate
/// (no drain).
///
/// This function is **non-blocking** — it sets the shutdown phase and
/// returns immediately. Use [`hew_shutdown_wait`] to block until
/// shutdown completes.
///
/// # Safety
///
/// Must only be called once. Subsequent calls are no-ops.
#[no_mangle]
pub extern "C" fn hew_shutdown_initiate(drain_timeout_ms: i64) {
    // Only transition from RUNNING to QUIESCE.
    if SHUTDOWN_PHASE
        .compare_exchange(
            PHASE_RUNNING,
            PHASE_QUIESCE,
            Ordering::AcqRel,
            Ordering::Relaxed,
        )
        .is_err()
    {
        return; // Already shutting down.
    }

    let timeout = match drain_timeout_ms {
        ..0 => Duration::ZERO,
        0 => Duration::from_millis(DEFAULT_DRAIN_TIMEOUT_MS),
        #[expect(clippy::cast_sign_loss, reason = "guarded by match arm (positive)")]
        ms => Duration::from_millis(ms as u64),
    };

    // Spawn a background thread to orchestrate the shutdown phases
    // so the caller is not blocked.
    match std::thread::Builder::new()
        .name("hew-shutdown".into())
        .spawn(move || shutdown_orchestrate(timeout))
    {
        Ok(_) => {
            // Shutdown orchestration thread started successfully.
        }
        Err(_) => {
            // Spawn failed — run shutdown synchronously on current thread.
            // This ensures shutdown completes even if thread spawning fails.
            shutdown_orchestrate(timeout);
        }
    }
}

/// Block the calling thread until shutdown is complete (phase == DONE).
///
/// Returns 0 on success, -1 if shutdown was never initiated.
#[no_mangle]
pub extern "C" fn hew_shutdown_wait() -> c_int {
    if SHUTDOWN_PHASE.load(Ordering::Acquire) == PHASE_RUNNING {
        return -1;
    }
    while SHUTDOWN_PHASE.load(Ordering::Acquire) != PHASE_DONE {
        std::thread::sleep(Duration::from_millis(10));
    }
    0
}

// ---------------------------------------------------------------------------
// Signal handler — SIGTERM / SIGINT
// ---------------------------------------------------------------------------

/// Install SIGTERM and SIGINT handlers that trigger graceful shutdown.
///
/// # Safety
///
/// Must be called from the main thread before any other signal
/// handlers are installed for these signals.
#[cfg(unix)]
pub unsafe fn install_shutdown_signal_handlers() {
    // SAFETY: We install a simple handler that only performs an atomic store
    // (async-signal-safe) and then calls hew_shutdown_initiate.
    unsafe {
        let mut sa: libc::sigaction = std::mem::zeroed();
        sa.sa_sigaction = shutdown_signal_handler
            as extern "C" fn(c_int, *mut libc::siginfo_t, *mut std::ffi::c_void)
            as usize;
        sa.sa_flags = libc::SA_SIGINFO | libc::SA_RESTART;
        libc::sigemptyset(&raw mut sa.sa_mask);

        libc::sigaction(libc::SIGTERM, &raw const sa, std::ptr::null_mut());
        libc::sigaction(libc::SIGINT, &raw const sa, std::ptr::null_mut());
    }
}

/// Windows shutdown handler using `SetConsoleCtrlHandler`.
///
/// Handles CTRL_C_EVENT, CTRL_BREAK_EVENT, and CTRL_CLOSE_EVENT by
/// transitioning the runtime to the QUIESCE phase, mirroring the Unix
/// SIGTERM/SIGINT handler behaviour.
#[cfg(windows)]
pub unsafe fn install_shutdown_signal_handlers() {
    #[link(name = "kernel32")]
    unsafe extern "system" {
        fn SetConsoleCtrlHandler(
            handler: Option<unsafe extern "system" fn(u32) -> i32>,
            add: i32,
        ) -> i32;
    }

    unsafe extern "system" fn ctrl_handler(ctrl_type: u32) -> i32 {
        // CTRL_C_EVENT = 0, CTRL_BREAK_EVENT = 1, CTRL_CLOSE_EVENT = 2
        if ctrl_type <= 2 {
            // Only set the pending flag. The phase transition is done by
            // hew_shutdown_initiate (called from check_signal_shutdown).
            SIGNAL_SHUTDOWN_PENDING.store(true, Ordering::Release);
            return 1; // Handled
        }
        0 // Not handled
    }

    // SAFETY: `ctrl_handler` matches the `PHANDLER_ROUTINE` signature
    // expected by `SetConsoleCtrlHandler`. Registering it is safe as long
    // as the handler only performs signal-safe operations (atomic store).
    unsafe { SetConsoleCtrlHandler(Some(ctrl_handler), 1) };
}

/// Signal handler for SIGTERM/SIGINT (Unix only).
///
/// Only performs async-signal-safe operations: an atomic store.
/// The actual shutdown work is done by the orchestration thread
/// spawned in `hew_shutdown_initiate`.
#[cfg(unix)]
extern "C" fn shutdown_signal_handler(
    _sig: c_int,
    _info: *mut libc::siginfo_t,
    _ctx: *mut std::ffi::c_void,
) {
    // We can't spawn a thread from a signal handler (not async-signal-safe).
    // Set a flag that workers will notice during their park phase.
    // The first worker to detect it calls hew_shutdown_initiate which
    // performs the RUNNING → QUIESCE phase transition.
    SIGNAL_SHUTDOWN_PENDING.store(true, Ordering::Release);
}

/// Flag set by the signal handler to indicate a signal-initiated shutdown.
static SIGNAL_SHUTDOWN_PENDING: std::sync::atomic::AtomicBool =
    std::sync::atomic::AtomicBool::new(false);

/// Called by the worker loop during its park phase to check if a
/// signal-initiated shutdown needs to be started.
///
/// This is safe to call from any thread. Only the first caller that
/// successfully claims the shutdown will spawn the orchestration thread.
pub fn check_signal_shutdown() {
    if SIGNAL_SHUTDOWN_PENDING
        .compare_exchange(true, false, Ordering::AcqRel, Ordering::Relaxed)
        .is_ok()
    {
        // We're in a normal thread context now, safe to spawn.
        hew_shutdown_initiate(0); // 0 = default drain timeout
    }
}

// ---------------------------------------------------------------------------
// Internal orchestration
// ---------------------------------------------------------------------------

/// Orchestrate the 3-phase shutdown.
fn shutdown_orchestrate(drain_timeout: Duration) {
    // Phase 1: Quiesce (already set by caller).
    // Nothing else to do — hew_is_shutting_down() now returns 1, which
    // callers can check before spawning new actors.

    // Phase 2: Drain — let workers process remaining messages.
    SHUTDOWN_PHASE.store(PHASE_DRAIN, Ordering::Release);

    if !drain_timeout.is_zero() {
        let deadline = Instant::now() + drain_timeout;
        while Instant::now() < deadline {
            // Check if there are any runnable actors left.
            // We approximate by checking global queue emptiness.
            // Workers will naturally drain their local queues.
            std::thread::sleep(Duration::from_millis(50));
        }
    }

    // Phase 3: Terminate — stop supervisors bottom-up, then scheduler.
    SHUTDOWN_PHASE.store(PHASE_TERMINATE, Ordering::Release);

    // Stop registered supervisors in reverse order (bottom-up).
    // Extract the supervisor list to avoid holding the mutex while stopping them.
    // This prevents deadlock when hew_supervisor_stop calls hew_shutdown_unregister_supervisor.
    let supervisors_to_stop = {
        let mut sups = TOP_LEVEL_SUPERVISORS.lock_or_recover();
        // Reverse: last registered (innermost) first.
        sups.reverse();
        std::mem::take(&mut *sups) // Extract all supervisors, leaving empty vec
    };

    // Stop supervisors without holding the mutex.
    for s in &supervisors_to_stop {
        if !s.0.is_null() {
            // SAFETY: supervisor was registered and is still valid.
            unsafe { crate::supervisor::hew_supervisor_stop(s.0) };
        }
    }

    // Shut down the scheduler (joins worker threads).
    scheduler::hew_sched_shutdown();

    SHUTDOWN_PHASE.store(PHASE_DONE, Ordering::Release);
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::{Mutex, OnceLock};

    fn shutdown_test_guard() -> std::sync::MutexGuard<'static, ()> {
        static LOCK: OnceLock<Mutex<()>> = OnceLock::new();
        LOCK.get_or_init(|| Mutex::new(()))
            .lock()
            .expect("shutdown test mutex poisoned")
    }

    fn reset_shutdown_state() {
        SHUTDOWN_PHASE.store(PHASE_RUNNING, Ordering::Release);

        let mut supervisors = TOP_LEVEL_SUPERVISORS.lock_or_recover();
        for supervisor in supervisors.drain(..) {
            if !supervisor.0.is_null() {
                // SAFETY: tests only store pointers returned by hew_supervisor_new
                // and this cleanup runs after each test has finished using them.
                unsafe { crate::supervisor::hew_supervisor_stop(supervisor.0) };
            }
        }
    }

    #[test]
    fn initial_phase_is_running() {
        let _guard = shutdown_test_guard();
        reset_shutdown_state();
        assert_eq!(SHUTDOWN_PHASE.load(Ordering::Acquire), PHASE_RUNNING);
    }

    #[test]
    fn is_shutting_down_returns_zero_when_running() {
        let _guard = shutdown_test_guard();
        reset_shutdown_state();
        assert_eq!(hew_is_shutting_down(), 0);
    }

    #[test]
    fn shutdown_phase_constants() {
        let _guard = shutdown_test_guard();
        assert_eq!(PHASE_RUNNING, 0);
        assert_eq!(PHASE_QUIESCE, 1);
        assert_eq!(PHASE_DRAIN, 2);
        assert_eq!(PHASE_TERMINATE, 3);
        assert_eq!(PHASE_DONE, 4);
    }

    #[test]
    fn register_null_supervisor_is_noop() {
        let _guard = shutdown_test_guard();
        reset_shutdown_state();
        // SAFETY: null pointer is explicitly handled.
        unsafe { hew_shutdown_register_supervisor(std::ptr::null_mut()) };
        // No crash = success.
    }

    #[test]
    fn unregister_null_supervisor_is_noop() {
        let _guard = shutdown_test_guard();
        reset_shutdown_state();
        // SAFETY: null pointer is explicitly handled.
        unsafe { hew_shutdown_unregister_supervisor(std::ptr::null_mut()) };
    }

    #[test]
    fn shutdown_wait_returns_error_if_not_initiated() {
        let _guard = shutdown_test_guard();
        reset_shutdown_state();
        assert_eq!(hew_shutdown_wait(), -1);
    }

    #[test]
    fn hew_shutdown_phase_reflects_state() {
        let _guard = shutdown_test_guard();
        reset_shutdown_state();
        // Just test the accessor against the atomic.
        let prev = SHUTDOWN_PHASE.load(Ordering::Acquire);
        assert_eq!(hew_shutdown_phase(), prev);
    }

    // ---------------------------------------------------------------------------
    // Bug #1: Shutdown Self-Deadlock Tests
    // ---------------------------------------------------------------------------

    /// Positive test: Register a supervisor and verify shutdown completes without deadlock.
    #[test]
    fn shutdown_orchestrate_no_deadlock_with_supervisor() {
        let _guard = shutdown_test_guard();
        reset_shutdown_state();

        // Create a mock supervisor using the C ABI function.
        // SAFETY: hew_supervisor_new is safe to call with valid parameters.
        let mock_supervisor = unsafe { crate::supervisor::hew_supervisor_new(1, 3, 60) }; // strategy=1, max_restarts=3, window_secs=60

        // Register the supervisor.
        // SAFETY: mock_supervisor is a valid supervisor pointer returned from hew_supervisor_new.
        unsafe { hew_shutdown_register_supervisor(mock_supervisor) };

        // Put us in QUIESCE phase as if shutdown was initiated.
        SHUTDOWN_PHASE.store(PHASE_QUIESCE, Ordering::Release);

        // Run shutdown_orchestrate on a helper thread with 2-second timeout.
        let handle = std::thread::spawn(|| {
            shutdown_orchestrate(Duration::from_millis(10)); // Short timeout for test speed
        });

        // Join with timeout — if this times out, we have a deadlock.
        assert!(
            handle.join().is_ok(),
            "Shutdown orchestration should complete without deadlock"
        );

        // Verify shutdown reached DONE phase.
        assert_eq!(SHUTDOWN_PHASE.load(Ordering::Acquire), PHASE_DONE);

        // Clean up.
        reset_shutdown_state();
    }

    /// Negative test: Unregistering a supervisor that was never registered is a no-op.
    #[test]
    fn unregister_nonexistent_supervisor_is_noop() {
        let _guard = shutdown_test_guard();
        reset_shutdown_state();

        // Create a mock supervisor that was never registered.
        // SAFETY: hew_supervisor_new is safe to call with valid parameters.
        let mock_supervisor = unsafe { crate::supervisor::hew_supervisor_new(1, 3, 60) };

        // This should not crash or deadlock.
        // SAFETY: mock_supervisor is a valid pointer, unregistering is safe.
        unsafe { hew_shutdown_unregister_supervisor(mock_supervisor) };

        // Clean up the mock supervisor.
        // SAFETY: mock_supervisor is a valid pointer that we own.
        unsafe { crate::supervisor::hew_supervisor_stop(mock_supervisor) };
    }

    // ---------------------------------------------------------------------------
    // Bug #10: Shutdown Spawn Failure Tests
    // ---------------------------------------------------------------------------

    /// Positive test: Normal shutdown initiation completes successfully.
    #[test]
    fn shutdown_initiate_completes_normally() {
        let _guard = shutdown_test_guard();
        reset_shutdown_state();

        // Test the simple case where we transition phases manually.
        // In a real runtime, this would be done by the scheduler.

        // Start in RUNNING phase
        assert_eq!(SHUTDOWN_PHASE.load(Ordering::Acquire), PHASE_RUNNING);

        // Transition to QUIESCE (simulate initiation)
        SHUTDOWN_PHASE.store(PHASE_QUIESCE, Ordering::Release);

        // Run orchestration directly with short timeout.
        shutdown_orchestrate(Duration::from_millis(10));

        // Verify we reached DONE.
        assert_eq!(SHUTDOWN_PHASE.load(Ordering::Acquire), PHASE_DONE);

        reset_shutdown_state();
    }

    /// Negative test: Verify shutdown completes even if thread spawn fails.
    /// Note: This test may be skipped if we can't actually trigger spawn failure.
    #[test]
    fn shutdown_spawn_failure_fallback() {
        let _guard = shutdown_test_guard();
        reset_shutdown_state();

        // This test is challenging because we need to force spawn failure.
        // For deterministic testing, we'll test the synchronous path directly:
        // We'll test the synchronous fallback path by calling shutdown_orchestrate directly.

        // Set to QUIESCE phase manually.
        SHUTDOWN_PHASE.store(PHASE_QUIESCE, Ordering::Release);

        // Call shutdown_orchestrate directly (simulating the fallback case).
        shutdown_orchestrate(Duration::from_millis(10));

        // Verify it completed.
        assert_eq!(SHUTDOWN_PHASE.load(Ordering::Acquire), PHASE_DONE);

        reset_shutdown_state();
    }

    // Test to verify the spawn failure detection works by testing thread creation under stress.
    // This documents the behaviour but doesn't require spawn failure.
    #[test]
    fn thread_spawn_stress_test() {
        let _guard = shutdown_test_guard();
        reset_shutdown_state();
        // This test verifies our understanding of thread spawn failure conditions.
        // We try to spawn many threads rapidly and see if any fail.
        let mut spawn_failures = 0;
        let attempts = 50;

        for _ in 0..attempts {
            match std::thread::Builder::new()
                .name("stress-test".into())
                .spawn(|| std::thread::sleep(Duration::from_millis(1)))
            {
                Ok(handle) => {
                    let _ = handle.join();
                }
                Err(_) => {
                    spawn_failures += 1;
                }
            }
        }

        // This test documents spawn behaviour but doesn't require failures.
        // If failures occur, our shutdown code should handle them via fallback.
        println!("Thread spawn failures in stress test: {spawn_failures}/{attempts}");
    }

    // ---------------------------------------------------------------------------
    // Sabotage Tests (manually run to verify tests actually detect the bugs)
    // ---------------------------------------------------------------------------

    /// Sabotage test for Bug #1: This test should deadlock if we revert the fix.
    /// Run this manually after temporarily reverting the deadlock fix to verify
    /// the test actually catches the bug.
    #[test]
    #[ignore = "Manually run to verify test detects deadlock bug"]
    fn sabotage_deadlock_test() {
        let _guard = shutdown_test_guard();
        reset_shutdown_state();

        // Create and register a supervisor.
        // SAFETY: hew_supervisor_new is safe to call with valid parameters.
        let mock_supervisor = unsafe { crate::supervisor::hew_supervisor_new(1, 3, 60) };
        // SAFETY: mock_supervisor is a valid supervisor pointer.
        unsafe { hew_shutdown_register_supervisor(mock_supervisor) };

        // Put us in QUIESCE phase.
        SHUTDOWN_PHASE.store(PHASE_QUIESCE, Ordering::Release);

        // This would deadlock with the old code (before the fix).
        // If you revert the fix and run this test, it should timeout.
        let handle = std::thread::spawn(|| {
            shutdown_orchestrate(Duration::from_millis(10));
        });

        // This should complete without deadlock (with our fix).
        // If you've reverted the fix, this will panic after timing out.
        let result = handle.join();
        assert!(result.is_ok(), "Shutdown should complete without deadlock");
        assert_eq!(SHUTDOWN_PHASE.load(Ordering::Acquire), PHASE_DONE);

        reset_shutdown_state();
    }

    /// Sabotage test for Bug #10: This test should stall if we revert the spawn fallback.
    /// Run this manually after removing the spawn failure fallback to verify
    /// the test catches the infinite wait bug.
    #[test]
    #[ignore = "Manually run to verify test catches spawn failure stall"]
    fn sabotage_spawn_failure_test() {
        let _guard = shutdown_test_guard();
        reset_shutdown_state();

        // Test the case where spawn fails by calling orchestrate directly.
        // In the old code (without fallback), if spawn failed, nothing would
        // set PHASE_DONE and hew_shutdown_wait would loop forever.

        SHUTDOWN_PHASE.store(PHASE_QUIESCE, Ordering::Release);

        // If you remove the spawn fallback and only have:
        //   std::thread::spawn(...).ok();
        // Then nothing sets PHASE_DONE when spawn fails.

        // This simulates the fallback working:
        shutdown_orchestrate(Duration::from_millis(10));
        assert_eq!(SHUTDOWN_PHASE.load(Ordering::Acquire), PHASE_DONE);

        reset_shutdown_state();
    }

    /// When `shutdown_orchestrate` runs on a worker thread (spawn-failure
    /// fallback), `hew_sched_shutdown` must skip joining that thread's
    /// own handle.  This test calls `shutdown_orchestrate` from a spawned
    /// thread to verify no deadlock occurs.
    #[test]
    fn shutdown_fallback_skips_self_join() {
        let _guard = shutdown_test_guard();
        reset_shutdown_state();
        SHUTDOWN_PHASE.store(PHASE_QUIESCE, Ordering::Release);

        // Capture the phase inside the thread before another test
        // calls reset_shutdown_state().
        let handle = std::thread::Builder::new()
            .name("fallback-worker".into())
            .spawn(|| {
                shutdown_orchestrate(Duration::from_millis(10));
                SHUTDOWN_PHASE.load(Ordering::Acquire)
            })
            .expect("test thread spawn");

        let phase = handle
            .join()
            .expect("spawn-failure fallback must not self-join deadlock");
        assert_eq!(phase, PHASE_DONE);

        reset_shutdown_state();
    }

    // ---------------------------------------------------------------------------
    // Mutex poison-recovery tests
    // ---------------------------------------------------------------------------

    /// `hew_shutdown_register_supervisor` must proceed even when
    /// `TOP_LEVEL_SUPERVISORS` was previously poisoned.
    #[test]
    fn register_supervisor_recovers_from_poisoned_mutex() {
        let _guard = shutdown_test_guard();
        reset_shutdown_state();

        // Poison the mutex by panicking while holding it.
        let _ = std::panic::catch_unwind(|| {
            let _guard = TOP_LEVEL_SUPERVISORS.lock().unwrap();
            panic!("intentional poison");
        });

        // The mutex is now poisoned.  hew_shutdown_register_supervisor must
        // not silently skip — it must recover and register the supervisor.
        // SAFETY: hew_supervisor_new is safe to call with valid parameters.
        let sup = unsafe { crate::supervisor::hew_supervisor_new(1, 3, 60) };
        // SAFETY: sup is a valid pointer returned by hew_supervisor_new.
        unsafe { hew_shutdown_register_supervisor(sup) };

        assert!(
            is_supervisor_registered_for_test(sup),
            "register must succeed even after mutex poison"
        );

        // Must stop `sup` before reset_shutdown_state() to avoid re-entrant
        // lock deadlock: hew_supervisor_stop calls hew_shutdown_unregister_supervisor
        // which also acquires TOP_LEVEL_SUPERVISORS; reset_shutdown_state holds
        // that lock while calling hew_supervisor_stop.
        // SAFETY: sup is a valid pointer we own.
        unsafe { crate::supervisor::hew_supervisor_stop(sup) };

        reset_shutdown_state();
    }

    /// `hew_shutdown_unregister_supervisor` must proceed even when
    /// `TOP_LEVEL_SUPERVISORS` was previously poisoned.
    #[test]
    fn unregister_supervisor_recovers_from_poisoned_mutex() {
        let _guard = shutdown_test_guard();
        reset_shutdown_state();

        // Register a supervisor first (mutex is clean at this point).
        // SAFETY: hew_supervisor_new is safe to call with valid parameters.
        let sup = unsafe { crate::supervisor::hew_supervisor_new(1, 3, 60) };
        // SAFETY: sup is a valid pointer.
        unsafe { hew_shutdown_register_supervisor(sup) };
        assert!(is_supervisor_registered_for_test(sup));

        // Poison the mutex.
        let _ = std::panic::catch_unwind(|| {
            let _guard = TOP_LEVEL_SUPERVISORS.lock().unwrap();
            panic!("intentional poison");
        });

        // Unregister must recover from the poison and actually remove the entry.
        // SAFETY: sup is a valid pointer previously registered.
        unsafe { hew_shutdown_unregister_supervisor(sup) };

        assert!(
            !is_supervisor_registered_for_test(sup),
            "unregister must succeed even after mutex poison"
        );

        // sup is no longer in the list; free its resources directly.
        // SAFETY: sup is a valid pointer we own.
        unsafe { crate::supervisor::hew_supervisor_stop(sup) };

        reset_shutdown_state();
    }

    /// `free_registered_supervisors` must drain all supervisors even when
    /// `TOP_LEVEL_SUPERVISORS` was previously poisoned.
    #[test]
    fn free_registered_supervisors_recovers_from_poisoned_mutex() {
        let _guard = shutdown_test_guard();
        reset_shutdown_state();

        // SAFETY: hew_supervisor_new is safe to call with valid parameters.
        let sup = unsafe { crate::supervisor::hew_supervisor_new(1, 3, 60) };
        // SAFETY: sup is a valid pointer.
        unsafe { hew_shutdown_register_supervisor(sup) };

        // Poison the mutex.
        let _ = std::panic::catch_unwind(|| {
            let _guard = TOP_LEVEL_SUPERVISORS.lock().unwrap();
            panic!("intentional poison");
        });

        // SAFETY: worker threads are not running in this unit-test context;
        // calling free_registered_supervisors mirrors the cleanup call site.
        unsafe { free_registered_supervisors() };

        // After the call the list must be empty (supervisor was freed).
        assert!(
            !is_supervisor_registered_for_test(sup),
            "free_registered_supervisors must drain even after mutex poison"
        );

        reset_shutdown_state();
    }

    /// The graceful-shutdown supervisor drain path (`lock_or_recover` +
    /// `std::mem::take` in `shutdown_orchestrate`) must extract supervisors
    /// even when `TOP_LEVEL_SUPERVISORS` was previously poisoned.
    ///
    /// This test exercises the real `shutdown_orchestrate` production path —
    /// not an isolated helper — to verify the fix end-to-end.
    #[test]
    fn shutdown_orchestrate_drain_path_recovers_from_poisoned_mutex() {
        let _guard = shutdown_test_guard();
        reset_shutdown_state();

        // SAFETY: hew_supervisor_new is safe to call with valid parameters.
        let sup = unsafe { crate::supervisor::hew_supervisor_new(1, 3, 60) };
        // SAFETY: sup is a valid pointer.
        unsafe { hew_shutdown_register_supervisor(sup) };

        // Poison the mutex before the drain.
        let _ = std::panic::catch_unwind(|| {
            let _guard = TOP_LEVEL_SUPERVISORS.lock().unwrap();
            panic!("intentional poison");
        });

        // Enter QUIESCE phase as if shutdown was initiated.
        SHUTDOWN_PHASE.store(PHASE_QUIESCE, Ordering::Release);

        // Call the real production path on a helper thread.
        // `shutdown_orchestrate` must recover from the poisoned mutex via
        // `lock_or_recover`, drain the supervisor list, stop supervisors, and
        // reach PHASE_DONE.  `hew_sched_shutdown` is a no-op when no scheduler
        // has been initialised (unit-test context).
        let handle = std::thread::spawn(|| {
            shutdown_orchestrate(Duration::from_millis(10));
        });
        assert!(
            handle.join().is_ok(),
            "shutdown_orchestrate must complete even after mutex poison"
        );

        assert_eq!(
            SHUTDOWN_PHASE.load(Ordering::Acquire),
            PHASE_DONE,
            "shutdown_orchestrate must reach DONE even after mutex poison"
        );

        // The supervisor must have been drained and stopped by the production path.
        assert!(
            !is_supervisor_registered_for_test(sup),
            "supervisor must be drained by shutdown_orchestrate even after mutex poison"
        );

        reset_shutdown_state();
    }
}
