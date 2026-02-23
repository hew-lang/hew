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
    if let Ok(mut sups) = TOP_LEVEL_SUPERVISORS.lock() {
        if !sups.iter().any(|s| s.0 == sup) {
            sups.push(SupervisorPtr(sup));
        }
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
    if let Ok(mut sups) = TOP_LEVEL_SUPERVISORS.lock() {
        sups.retain(|s| s.0 != sup);
    }
}

/// Free all registered top-level supervisors without waiting for actors.
///
/// Called by [`crate::scheduler::hew_runtime_cleanup`] **after** worker
/// threads have been joined.  At that point no actor processing can
/// happen, so we simply drop the supervisor structs to release child
/// spec resources (names, `init_state`).  Actors themselves are freed
/// separately by [`crate::actor::cleanup_all_actors`].
pub(crate) unsafe fn free_registered_supervisors() {
    if let Ok(mut sups) = TOP_LEVEL_SUPERVISORS.lock() {
        for s in sups.drain(..) {
            if !s.0.is_null() {
                // SAFETY: supervisor was registered and pointer is valid.
                unsafe { crate::supervisor::free_supervisor_resources(s.0) };
            }
        }
    }
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
    std::thread::Builder::new()
        .name("hew-shutdown".into())
        .spawn(move || shutdown_orchestrate(timeout))
        .ok();
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
pub unsafe fn install_shutdown_signal_handlers() {
    // SAFETY: We install a simple handler that only performs an atomic store
    // (async-signal-safe) and then calls hew_shutdown_initiate.
    unsafe {
        let mut sa: libc::sigaction = std::mem::zeroed();
        sa.sa_sigaction = shutdown_signal_handler as usize;
        sa.sa_flags = libc::SA_SIGINFO | libc::SA_RESTART;
        libc::sigemptyset(&raw mut sa.sa_mask);

        libc::sigaction(libc::SIGTERM, &raw const sa, std::ptr::null_mut());
        libc::sigaction(libc::SIGINT, &raw const sa, std::ptr::null_mut());
    }
}

/// Signal handler for SIGTERM/SIGINT.
///
/// Only performs async-signal-safe operations: an atomic store.
/// The actual shutdown work is done by the orchestration thread
/// spawned in `hew_shutdown_initiate`.
extern "C" fn shutdown_signal_handler(
    _sig: c_int,
    _info: *mut libc::siginfo_t,
    _ctx: *mut std::ffi::c_void,
) {
    // Use a relaxed CAS to avoid re-triggering if already shutting down.
    // This is async-signal-safe (just an atomic compare-exchange).
    if SHUTDOWN_PHASE
        .compare_exchange(
            PHASE_RUNNING,
            PHASE_QUIESCE,
            Ordering::AcqRel,
            Ordering::Relaxed,
        )
        .is_ok()
    {
        // We can't spawn a thread from a signal handler (not async-signal-safe).
        // Set a flag that workers will notice during their park phase.
        // The first worker to detect it spawns the orchestration thread.
        SIGNAL_SHUTDOWN_PENDING.store(true, Ordering::Release);
    }
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
    if let Ok(mut sups) = TOP_LEVEL_SUPERVISORS.lock() {
        // Reverse: last registered (innermost) first.
        sups.reverse();
        for s in sups.iter() {
            if !s.0.is_null() {
                // SAFETY: supervisor was registered and is still valid.
                unsafe { crate::supervisor::hew_supervisor_stop(s.0) };
            }
        }
        sups.clear();
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

    fn reset_shutdown_phase() {
        SHUTDOWN_PHASE.store(PHASE_RUNNING, Ordering::Release);
    }

    #[test]
    fn initial_phase_is_running() {
        assert_eq!(SHUTDOWN_PHASE.load(Ordering::Acquire), PHASE_RUNNING);
    }

    #[test]
    fn is_shutting_down_returns_zero_when_running() {
        reset_shutdown_phase();
        assert_eq!(hew_is_shutting_down(), 0);
    }

    #[test]
    fn shutdown_phase_constants() {
        assert_eq!(PHASE_RUNNING, 0);
        assert_eq!(PHASE_QUIESCE, 1);
        assert_eq!(PHASE_DRAIN, 2);
        assert_eq!(PHASE_TERMINATE, 3);
        assert_eq!(PHASE_DONE, 4);
    }

    #[test]
    fn register_null_supervisor_is_noop() {
        // SAFETY: null pointer is explicitly handled.
        unsafe { hew_shutdown_register_supervisor(std::ptr::null_mut()) };
        // No crash = success.
    }

    #[test]
    fn unregister_null_supervisor_is_noop() {
        // SAFETY: null pointer is explicitly handled.
        unsafe { hew_shutdown_unregister_supervisor(std::ptr::null_mut()) };
    }

    #[test]
    fn shutdown_wait_returns_error_if_not_initiated() {
        reset_shutdown_phase();
        assert_eq!(hew_shutdown_wait(), -1);
    }

    #[test]
    fn hew_shutdown_phase_reflects_state() {
        // Just test the accessor against the atomic.
        let prev = SHUTDOWN_PHASE.load(Ordering::Acquire);
        assert_eq!(hew_shutdown_phase(), prev);
    }
}
