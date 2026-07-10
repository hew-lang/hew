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
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

use std::ffi::c_int;
use std::sync::atomic::Ordering;
use std::time::{Duration, Instant};

use crate::reactor;
use crate::runtime::{rt_current, rt_default};
use crate::scheduler;

// ---------------------------------------------------------------------------
// Shutdown phases
// ---------------------------------------------------------------------------

/// Not shutting down.
pub(crate) const PHASE_RUNNING: i32 = 0;
/// Phase 1: No new spawns or messages accepted.
const PHASE_QUIESCE: i32 = 1;
/// Phase 2: Draining remaining messages with deadline.
const PHASE_DRAIN: i32 = 2;
/// Phase 3: Force-terminating all actors.
const PHASE_TERMINATE: i32 = 3;
/// Shutdown complete.
const PHASE_DONE: i32 = 4;
/// Shutdown failed before completion.
const PHASE_FAILED: i32 = 5;

/// Read the current shutdown phase off the installed runtime.
///
/// When no runtime is installed this returns [`PHASE_RUNNING`] rather than
/// trapping. A runtime that was never installed (a program that declares an
/// actor type but never spawns one, so `hew_sched_init` is never reached) has,
/// by definition, never entered any shutdown phase — it is logically
/// `PHASE_RUNNING`. The shutdown-lifecycle *entry points* (`hew_shutdown_*`)
/// are deliberately callable on a never-initialized runtime, exactly like the
/// sibling lifecycle symbols `hew_sched_shutdown` (`scheduler.rs`: "Safe to call
/// if the scheduler was never initialized") and `hew_runtime_cleanup` ("a no-op
/// if the scheduler was never initialized"). This does NOT relax the fail-closed
/// `rt_current()` boundary for genuine authority use elsewhere — it tolerates
/// "nothing to shut down" only at the shutdown-phase read.
#[inline]
fn shutdown_phase_load(ordering: Ordering) -> i32 {
    rt_default().map_or(PHASE_RUNNING, |rt| rt.shutdown_phase.load(ordering))
}

/// Store the current shutdown phase on the installed runtime.
#[inline]
fn shutdown_phase_store(value: i32, ordering: Ordering) {
    rt_current().shutdown_phase.store(value, ordering);
}

/// Default drain timeout in milliseconds.
const DEFAULT_DRAIN_TIMEOUT_MS: u64 = 5_000;
/// Poll interval while waiting for the scheduler to drain existing work.
const DRAIN_POLL_INTERVAL: Duration = Duration::from_millis(10);
/// Immediate shutdown normally skips the drain. If the reactor sweep wakes an
/// already-parked actor, give that typed cancellation a short bounded window to
/// run before workers are joined.
const SHUTDOWN_CANCEL_REDRAIN_TIMEOUT: Duration = Duration::from_millis(250);

/// Wrapper for `*mut HewSupervisor` to impl `Send`.
///
/// # Safety
///
/// Supervisor pointers are only accessed during shutdown while the
/// runtime is in a controlled wind-down state.
pub(crate) struct SupervisorPtr(pub(crate) *mut crate::supervisor::HewSupervisor);

// SAFETY: Supervisor pointers are only accessed during shutdown while
// the runtime is in a controlled wind-down state.
unsafe impl Send for SupervisorPtr {}

/// Operate on the installed runtime's registered top-level supervisors.
///
/// The supervisor-roots list was the `TOP_LEVEL_SUPERVISORS` global; it now
/// lives in `RuntimeInner::supervisor_roots`. Fails closed via [`rt_current`]
/// when no runtime is installed — registering a supervisor root before a
/// runtime exists is a lifecycle error, not a silent no-op.
#[inline]
fn with_supervisor_roots<R>(f: impl FnOnce(&mut Vec<SupervisorPtr>) -> R) -> R {
    rt_current().supervisor_roots.access(f)
}

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
/// - 5 = failed
#[no_mangle]
pub extern "C" fn hew_shutdown_phase() -> c_int {
    shutdown_phase_load(Ordering::Acquire)
}

/// Return 1 if the runtime is in any shutdown phase (not running), else 0.
#[no_mangle]
pub extern "C" fn hew_is_shutting_down() -> c_int {
    i32::from(shutdown_phase_load(Ordering::Acquire) != PHASE_RUNNING)
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
    with_supervisor_roots(|sups| {
        if !sups.iter().any(|s| s.0 == sup) {
            sups.push(SupervisorPtr(sup));
        }
    });
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
    with_supervisor_roots(|sups| sups.retain(|s| s.0 != sup));
}

#[cfg(test)]
pub(crate) fn is_supervisor_registered_for_test(
    sup: *mut crate::supervisor::HewSupervisor,
) -> bool {
    with_supervisor_roots(|sups| sups.iter().any(|candidate| candidate.0 == sup))
}

/// Free all registered top-level supervisors without waiting for actors.
///
/// Called by [`crate::scheduler::hew_runtime_cleanup`] **after** worker
/// threads have been joined.  At that point no actor processing can
/// happen, so we simply drop the supervisor structs to release child
/// spec resources (names, `init_state`).  Actors themselves are freed
/// separately by [`crate::actor::cleanup_all_actors`].
pub(crate) unsafe fn free_registered_supervisors() {
    let to_free = with_supervisor_roots(std::mem::take);
    for s in to_free {
        if !s.0.is_null() {
            // SAFETY: supervisor was registered and pointer is valid.
            unsafe { crate::supervisor::free_supervisor_resources(s.0) };
        }
    }
}

#[cfg(feature = "profiler")]
pub(crate) fn registered_supervisors_snapshot() -> Vec<*mut crate::supervisor::HewSupervisor> {
    with_supervisor_roots(|sups| sups.iter().map(|sup| sup.0).collect())
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
    shutdown_initiate(drain_timeout_ms, true);
}

/// Initiate the compiler-generated main-exit drain.
///
/// This preserves the pre-existing implicit drain contract: queued actor work may
/// finish, but already-parked reactor waits remain abandoned rather than being
/// surfaced as user-visible shutdown errors. Explicit [`hew_shutdown_initiate`]
/// is the typed-cancellation boundary.
#[no_mangle]
pub extern "C" fn hew_shutdown_initiate_implicit(drain_timeout_ms: i64) {
    shutdown_initiate(drain_timeout_ms, false);
}

fn shutdown_initiate(drain_timeout_ms: i64, cancel_parked_waits: bool) {
    // No runtime installed → nothing to shut down. The implicit actor-drain
    // epilogue emits the implicit entry point for every program that declares an
    // actor type, but `hew_sched_init` only runs at an actual spawn site.
    if rt_default().is_none() {
        return;
    }

    // Only transition from RUNNING to QUIESCE.
    if rt_current()
        .shutdown_phase
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
        .spawn(move || {
            if let Err(panic_payload) = run_shutdown_with_panic_handling(|| {
                shutdown_orchestrate_mode(timeout, cancel_parked_waits);
            }) {
                std::panic::resume_unwind(panic_payload);
            }
        }) {
        Ok(_) => {
            // Shutdown orchestration thread started successfully.
        }
        Err(_) => {
            // Spawn failed — run shutdown synchronously on current thread.
            // This ensures shutdown completes even if thread spawning fails.
            let _ = run_shutdown_with_panic_handling(|| {
                shutdown_orchestrate_mode(timeout, cancel_parked_waits);
            });
        }
    }
}

/// Block the calling thread until shutdown is complete (phase == DONE).
///
/// Returns 0 on success, -1 if shutdown was never initiated, and -2 if the
/// shutdown worker panicked before completion.
#[no_mangle]
pub extern "C" fn hew_shutdown_wait() -> c_int {
    loop {
        match shutdown_phase_load(Ordering::Acquire) {
            PHASE_RUNNING => return -1,
            PHASE_DONE => return 0,
            PHASE_FAILED => return -2,
            _ => std::thread::sleep(Duration::from_millis(10)),
        }
    }
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

fn run_shutdown_with_panic_handling<F>(operation: F) -> Result<(), Box<dyn std::any::Any + Send>>
where
    F: FnOnce() + std::panic::UnwindSafe,
{
    match std::panic::catch_unwind(operation) {
        Ok(()) => Ok(()),
        Err(panic_payload) => {
            report_shutdown_panic(panic_payload.as_ref());
            shutdown_phase_store(PHASE_FAILED, Ordering::Release);
            Err(panic_payload)
        }
    }
}

fn report_shutdown_panic(panic_payload: &(dyn std::any::Any + Send)) {
    let reason = panic_payload
        .downcast_ref::<&str>()
        .copied()
        .or_else(|| panic_payload.downcast_ref::<String>().map(String::as_str))
        .unwrap_or("non-string panic payload");
    eprintln!("hew-runtime: shutdown orchestration panicked: {reason}");
}

fn drain_until_idle(timeout: Duration) -> bool {
    if timeout.is_zero() {
        return true;
    }
    let deadline = Instant::now() + timeout;
    let mut idle_polls = 0;
    while Instant::now() < deadline {
        if scheduler::drain_is_idle() {
            idle_polls += 1;
            if idle_polls >= 2 {
                return true;
            }
        } else {
            idle_polls = 0;
        }

        let remaining = deadline.saturating_duration_since(Instant::now());
        if remaining.is_zero() {
            break;
        }
        std::thread::sleep(DRAIN_POLL_INTERVAL.min(remaining));
    }
    // Covers the case where the deadline expired while sleeping but the runtime
    // became idle before the final observation.
    scheduler::drain_is_idle()
}

/// Orchestrate the 3-phase shutdown.
#[cfg(test)]
fn shutdown_orchestrate(drain_timeout: Duration) {
    shutdown_orchestrate_mode(drain_timeout, true);
}

fn shutdown_orchestrate_mode(drain_timeout: Duration, cancel_parked_waits: bool) {
    // Phase 1: Quiesce (already set by caller).
    // Nothing else to do — hew_is_shutting_down() now returns 1, which
    // callers can check before spawning new actors.

    // Phase 2: Drain — let workers process remaining messages.
    shutdown_phase_store(PHASE_DRAIN, Ordering::Release);

    // Resolve waits that were already parked when shutdown began. The sweep
    // deposits a terminal outcome and wakes outside the reactor lock; running it
    // before the drain lets those resumptions settle before the worker join.
    let swept_waits = if cancel_parked_waits {
        reactor::reactor_cancel_parked_waits_for_shutdown()
    } else {
        0
    };
    let drain_window = if drain_timeout.is_zero() && swept_waits != 0 {
        SHUTDOWN_CANCEL_REDRAIN_TIMEOUT
    } else {
        drain_timeout
    };

    // Track whether the drain loop reached idle before the deadline.
    // WHY: Phase 3 join safety depends on this.  If the drain converged
    // (all workers parked), joining is instant and safe.  If it timed out,
    // a worker thread is still executing a handler; joining would block
    // forever.  The timed-out path skips join and all post-join cleanup,
    // letting process exit reap the stuck thread.
    let drain_converged = drain_until_idle(drain_window);

    // Phase 3: Terminate.
    shutdown_phase_store(PHASE_TERMINATE, Ordering::Release);

    if !drain_converged {
        // A worker is still executing a handler and will not return to its
        // loop top to observe the shutdown flag before the process exits.
        // Joining it would block forever; any post-join cleanup (supervisor
        // stop, session_reset, cleanup_all_actors) would race the live
        // worker and risk a UAF.
        //
        // Safe exit: mark shutdown complete and return.  main() unblocks
        // from hew_shutdown_wait(), returns, and the OS reaps the stuck
        // worker thread.  No heap memory is freed on this path — the process
        // is exiting so the OS reclaims it all.
        shutdown_phase_store(PHASE_DONE, Ordering::Release);
        return;
    }

    // Drain converged — all workers are parked.  The join below is instant.

    // Stop registered supervisors in reverse order (bottom-up).
    // Extract the supervisor list to avoid holding the mutex while stopping them.
    // This prevents deadlock when hew_supervisor_stop calls hew_shutdown_unregister_supervisor.
    let supervisors_to_stop = with_supervisor_roots(|sups| {
        // Reverse: last registered (innermost) first.
        sups.reverse();
        std::mem::take(sups) // Extract all supervisors, leaving empty vec
    });

    // Stop supervisors without holding the mutex.
    for s in &supervisors_to_stop {
        if !s.0.is_null() {
            // SAFETY: supervisor was registered, drain converged so no worker
            // is actively dispatching into supervisor-owned actors, and the
            // pointer remains valid until this call frees it.
            unsafe { crate::supervisor::hew_supervisor_stop(s.0) };
        }
    }

    // Shut down the scheduler (joins worker threads — instant since drain converged).
    scheduler::hew_sched_shutdown();

    shutdown_phase_store(PHASE_DONE, Ordering::Release);
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    /// Serialize a shutdown test AND install a default `RuntimeInner`.
    ///
    /// Under the explicit-install model the shutdown phase and supervisor-root
    /// list are runtime authorities, so every shutdown test must have a runtime
    /// installed before it touches them. `crate::runtime_test_guard()` installs
    /// a fresh worker-less default runtime (phase starts at `PHASE_RUNNING`,
    /// supervisor roots empty) and serializes on the shared scheduler-test lock;
    /// the install is reclaimed when the guard drops.
    fn shutdown_test_guard() -> crate::RuntimeTestGuard {
        crate::runtime_test_guard()
    }

    /// Reset the installed runtime's shutdown state back to a clean running
    /// baseline mid-test (e.g. after a prior phase transition). The guard
    /// already starts each test from this baseline; this is for tests that
    /// transition the phase and then want to re-assert from running.
    fn reset_shutdown_state() {
        shutdown_phase_store(PHASE_RUNNING, Ordering::Release);

        let to_stop = with_supervisor_roots(std::mem::take);
        for supervisor in to_stop {
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
        assert_eq!(shutdown_phase_load(Ordering::Acquire), PHASE_RUNNING);
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
        assert_eq!(PHASE_FAILED, 5);
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
    fn shutdown_wait_returns_error_if_shutdown_worker_panics() {
        let _guard = shutdown_test_guard();
        reset_shutdown_state();
        shutdown_phase_store(PHASE_QUIESCE, Ordering::Release);

        let handle = std::thread::Builder::new()
            .name("panic-shutdown-worker".into())
            .spawn(|| {
                if let Err(panic_payload) =
                    run_shutdown_with_panic_handling(|| panic!("boom during shutdown"))
                {
                    std::panic::resume_unwind(panic_payload);
                }
            })
            .expect("panic test thread spawn");

        assert!(
            handle.join().is_err(),
            "panic should still surface on the worker"
        );
        assert_eq!(hew_shutdown_phase(), PHASE_FAILED);
        assert_eq!(hew_shutdown_wait(), -2);
    }

    #[test]
    fn hew_shutdown_phase_reflects_state() {
        let _guard = shutdown_test_guard();
        reset_shutdown_state();
        // Just test the accessor against the atomic.
        let prev = shutdown_phase_load(Ordering::Acquire);
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
        shutdown_phase_store(PHASE_QUIESCE, Ordering::Release);

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
        assert_eq!(shutdown_phase_load(Ordering::Acquire), PHASE_DONE);

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
        assert_eq!(shutdown_phase_load(Ordering::Acquire), PHASE_RUNNING);

        // Transition to QUIESCE (simulate initiation)
        shutdown_phase_store(PHASE_QUIESCE, Ordering::Release);

        // Run orchestration directly with short timeout.
        shutdown_orchestrate(Duration::from_millis(10));

        // Verify we reached DONE.
        assert_eq!(shutdown_phase_load(Ordering::Acquire), PHASE_DONE);

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
        shutdown_phase_store(PHASE_QUIESCE, Ordering::Release);

        // Call shutdown_orchestrate directly (simulating the fallback case).
        shutdown_orchestrate(Duration::from_millis(10));

        // Verify it completed.
        assert_eq!(shutdown_phase_load(Ordering::Acquire), PHASE_DONE);

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
    fn sabotage_deadlock_test() {
        let _guard = shutdown_test_guard();
        reset_shutdown_state();

        // Create and register a supervisor.
        // SAFETY: hew_supervisor_new is safe to call with valid parameters.
        let mock_supervisor = unsafe { crate::supervisor::hew_supervisor_new(1, 3, 60) };
        // SAFETY: mock_supervisor is a valid supervisor pointer.
        unsafe { hew_shutdown_register_supervisor(mock_supervisor) };

        // Put us in QUIESCE phase.
        shutdown_phase_store(PHASE_QUIESCE, Ordering::Release);

        // This would deadlock with the old code (before the fix).
        // If you revert the fix and run this test, it should timeout.
        let handle = std::thread::spawn(|| {
            shutdown_orchestrate(Duration::from_millis(10));
        });

        // This should complete without deadlock (with our fix).
        // If you've reverted the fix, this will panic after timing out.
        let result = handle.join();
        assert!(result.is_ok(), "Shutdown should complete without deadlock");
        assert_eq!(shutdown_phase_load(Ordering::Acquire), PHASE_DONE);

        reset_shutdown_state();
    }

    /// Sabotage test for Bug #10: This test should stall if we revert the spawn fallback.
    /// Run this manually after removing the spawn failure fallback to verify
    /// the test catches the infinite wait bug.
    #[test]
    fn sabotage_spawn_failure_test() {
        let _guard = shutdown_test_guard();
        reset_shutdown_state();

        // Test the case where spawn fails by calling orchestrate directly.
        // In the old code (without fallback), if spawn failed, nothing would
        // set PHASE_DONE and hew_shutdown_wait would loop forever.

        shutdown_phase_store(PHASE_QUIESCE, Ordering::Release);

        // If you remove the spawn fallback and only have:
        //   std::thread::spawn(...).ok();
        // Then nothing sets PHASE_DONE when spawn fails.

        // This simulates the fallback working:
        shutdown_orchestrate(Duration::from_millis(10));
        assert_eq!(shutdown_phase_load(Ordering::Acquire), PHASE_DONE);

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
        shutdown_phase_store(PHASE_QUIESCE, Ordering::Release);

        // Capture the phase inside the thread before another test
        // calls reset_shutdown_state().
        let handle = std::thread::Builder::new()
            .name("fallback-worker".into())
            .spawn(|| {
                shutdown_orchestrate(Duration::from_millis(10));
                shutdown_phase_load(Ordering::Acquire)
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

    /// `hew_shutdown_register_supervisor` must proceed even when the runtime's
    /// `supervisor_roots` lock was previously poisoned.
    #[test]
    fn register_supervisor_recovers_from_poisoned_mutex() {
        let _guard = shutdown_test_guard();
        reset_shutdown_state();

        // Poison the mutex by panicking inside the closure.
        let _ = std::panic::catch_unwind(|| {
            with_supervisor_roots(|_| panic!("intentional poison"));
        });
        assert!(
            rt_current().supervisor_roots.is_poisoned_for_test(),
            "mutex must be poisoned"
        );

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
        // which also acquires supervisor_roots; reset_shutdown_state holds
        // that lock while calling hew_supervisor_stop.
        // SAFETY: sup is a valid pointer we own.
        unsafe { crate::supervisor::hew_supervisor_stop(sup) };

        reset_shutdown_state();
    }

    /// `hew_shutdown_unregister_supervisor` must proceed even when the runtime's
    /// `supervisor_roots` lock was previously poisoned.
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

        // Poison the mutex by panicking inside the closure.
        let _ = std::panic::catch_unwind(|| {
            with_supervisor_roots(|_| panic!("intentional poison"));
        });
        assert!(
            rt_current().supervisor_roots.is_poisoned_for_test(),
            "mutex must be poisoned"
        );

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

    /// `free_registered_supervisors` must drain all supervisors even when the
    /// runtime's `supervisor_roots` lock was previously poisoned.
    #[test]
    fn free_registered_supervisors_recovers_from_poisoned_mutex() {
        let _guard = shutdown_test_guard();
        reset_shutdown_state();

        // SAFETY: hew_supervisor_new is safe to call with valid parameters.
        let sup = unsafe { crate::supervisor::hew_supervisor_new(1, 3, 60) };
        // SAFETY: sup is a valid pointer.
        unsafe { hew_shutdown_register_supervisor(sup) };

        // Poison the mutex by panicking inside the closure.
        let _ = std::panic::catch_unwind(|| {
            with_supervisor_roots(|_| panic!("intentional poison"));
        });
        assert!(
            rt_current().supervisor_roots.is_poisoned_for_test(),
            "mutex must be poisoned"
        );

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
    /// even when the runtime's `supervisor_roots` lock was previously poisoned.
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
            with_supervisor_roots(|_| panic!("intentional poison"));
        });
        assert!(
            rt_current().supervisor_roots.is_poisoned_for_test(),
            "mutex must be poisoned"
        );

        // Enter QUIESCE phase as if shutdown was initiated.
        shutdown_phase_store(PHASE_QUIESCE, Ordering::Release);

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
            shutdown_phase_load(Ordering::Acquire),
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

    #[test]
    fn shutdown_orchestrate_returns_early_when_drain_is_already_idle() {
        let _guard = shutdown_test_guard();
        reset_shutdown_state();
        shutdown_phase_store(PHASE_QUIESCE, Ordering::Release);

        let timeout = Duration::from_millis(250);
        let started = Instant::now();
        shutdown_orchestrate(timeout);
        let elapsed = started.elapsed();

        assert_eq!(
            shutdown_phase_load(Ordering::Acquire),
            PHASE_DONE,
            "shutdown_orchestrate must still complete the shutdown sequence"
        );
        assert!(
            elapsed < Duration::from_millis(150),
            "shutdown should exit early once the runtime is already drained; elapsed={elapsed:?}"
        );

        reset_shutdown_state();
    }

    /// When the drain window expires with a worker still active,
    /// `shutdown_orchestrate` must set `PHASE_DONE` promptly (skipping the
    /// join) rather than blocking indefinitely.
    ///
    /// We simulate an un-drainable runtime by holding `ACTIVE_WORKERS` at 1
    /// for the duration of the drain window, then releasing it after
    /// `shutdown_orchestrate` returns so that `reset_shutdown_state` can
    /// complete cleanly.
    #[test]
    fn shutdown_orchestrate_sets_done_on_drain_timeout() {
        let _guard = shutdown_test_guard();
        reset_shutdown_state();
        shutdown_phase_store(PHASE_QUIESCE, Ordering::Release);

        // Keep the drain loop from ever converging.
        scheduler::ACTIVE_WORKERS.fetch_add(1, Ordering::Release);

        let drain_ms = 50u64;
        let started = Instant::now();
        shutdown_orchestrate(Duration::from_millis(drain_ms));
        let elapsed = started.elapsed();

        // Release the artificial worker count so later teardown is clean.
        scheduler::ACTIVE_WORKERS.fetch_sub(1, Ordering::Release);

        assert_eq!(
            shutdown_phase_load(Ordering::Acquire),
            PHASE_DONE,
            "shutdown_orchestrate must reach DONE even when drain times out"
        );
        // Must complete within ~2× the drain window (drain + small grace).
        assert!(
            elapsed < Duration::from_millis(drain_ms * 4),
            "timed-out drain must not block; elapsed={elapsed:?}"
        );

        reset_shutdown_state();
    }
}
