//! Deterministic scheduling and testing for the Hew actor runtime.
//!
//! Provides seed-based PRNG control, simulated time, fault injection,
//! and scheduling event recording for reproducible actor system testing.
//!
//! # Scheduling Modes
//!
//! - **Normal** (default): OS-managed threading, non-deterministic.
//! - **Seeded**: Fixed PRNG seed makes work-stealing deterministic.
//!   Workers still run on separate OS threads but random victim
//!   selection is reproducible across runs with the same seed.
//!
//! # Simulated Time
//!
//! When enabled, `hew_now_ms()` returns a monotonically-advancing
//! virtual clock controlled by the test harness. Actors see time
//! that only moves forward when the test explicitly advances it.
//!
//! # Fault Injection
//!
//! The fault injection API lets tests crash, delay, or drop messages
//! for specific actors to exercise failure-handling code paths.
//!
//! # C ABI
//!
//! - [`hew_deterministic_set_seed`] — Set PRNG seed for all workers.
//! - [`hew_deterministic_get_seed`] — Get the current seed.
//! - [`hew_simtime_enable`] — Switch to simulated time.
//! - [`hew_simtime_disable`] — Switch back to real time.
//! - [`hew_simtime_advance_ms`] — Advance simulated clock.
//! - [`hew_simtime_now_ms`] — Read simulated clock.
//! - [`hew_fault_inject_crash`] — Schedule a crash for an actor.
//! - [`hew_fault_inject_delay`] — Add latency to an actor's dispatch.
//! - [`hew_fault_inject_drop`] — Drop next N messages to an actor.
//! - [`hew_fault_clear`] — Remove all faults for an actor.
//! - [`hew_fault_clear_all`] — Remove all faults system-wide.

use std::ffi::c_int;
use std::sync::atomic::{AtomicBool, AtomicI64, AtomicU64, Ordering};
use std::sync::Mutex;

// ── PRNG Seed Control ──────────────────────────────────────────────────

/// Global PRNG seed. When non-zero, workers derive their PRNG state from
/// `seed + worker_id` instead of `worker_id + 1`.
static GLOBAL_SEED: AtomicU64 = AtomicU64::new(0);

/// Set a deterministic PRNG seed for all worker threads.
///
/// Workers derive their per-thread PRNG as `seed + worker_id`. Setting
/// `seed = 0` reverts to the default behavior (`worker_id + 1`).
///
/// **Must be called before `hew_sched_init`** for the seed to take
/// effect on all workers from their first scheduling decision.
#[no_mangle]
pub extern "C" fn hew_deterministic_set_seed(seed: u64) {
    GLOBAL_SEED.store(seed, Ordering::Release);
}

/// Return the current global PRNG seed (0 = default/non-deterministic).
#[no_mangle]
pub extern "C" fn hew_deterministic_get_seed() -> u64 {
    GLOBAL_SEED.load(Ordering::Acquire)
}

/// Compute the effective PRNG seed for a given worker.
///
/// If a global seed is set, returns `global_seed + worker_id`.
/// Otherwise returns `worker_id + 1` (the default).
pub(crate) fn effective_worker_seed(worker_id: u64) -> u64 {
    let seed = GLOBAL_SEED.load(Ordering::Acquire);
    if seed != 0 {
        seed.wrapping_add(worker_id)
    } else {
        worker_id + 1
    }
}

// ── Simulated Time ─────────────────────────────────────────────────────

/// Whether simulated time is active.
static SIMTIME_ENABLED: AtomicBool = AtomicBool::new(false);

/// Simulated clock value in milliseconds.
static SIMTIME_MS: AtomicI64 = AtomicI64::new(0);

/// Enable simulated time. `hew_now_ms()` will return the simulated
/// clock value instead of the real monotonic clock.
///
/// Resets the simulated clock to `start_ms`.
#[no_mangle]
pub extern "C" fn hew_simtime_enable(start_ms: i64) {
    SIMTIME_MS.store(start_ms, Ordering::Release);
    SIMTIME_ENABLED.store(true, Ordering::Release);
}

/// Disable simulated time. `hew_now_ms()` reverts to the real clock.
#[no_mangle]
pub extern "C" fn hew_simtime_disable() {
    SIMTIME_ENABLED.store(false, Ordering::Release);
}

/// Advance the simulated clock by `delta_ms` milliseconds.
///
/// Returns the new clock value.
#[no_mangle]
pub extern "C" fn hew_simtime_advance_ms(delta_ms: i64) -> i64 {
    SIMTIME_MS.fetch_add(delta_ms, Ordering::AcqRel) + delta_ms
}

/// Set the simulated clock to an absolute value.
///
/// Returns the previous clock value.
#[no_mangle]
pub extern "C" fn hew_simtime_set_ms(ms: i64) -> i64 {
    SIMTIME_MS.swap(ms, Ordering::AcqRel)
}

/// Read the current simulated clock value.
#[no_mangle]
pub extern "C" fn hew_simtime_now_ms() -> i64 {
    SIMTIME_MS.load(Ordering::Acquire)
}

/// Check if simulated time is enabled. Returns 1 if enabled, 0 if not.
#[no_mangle]
pub extern "C" fn hew_simtime_is_enabled() -> c_int {
    c_int::from(SIMTIME_ENABLED.load(Ordering::Acquire))
}

/// Called by `hew_now_ms()` to check if simulated time should be used.
///
/// Returns `Some(ms)` if simulated time is active, `None` otherwise.
#[inline]
pub(crate) fn simtime_now() -> Option<u64> {
    if SIMTIME_ENABLED.load(Ordering::Acquire) {
        let ms = SIMTIME_MS.load(Ordering::Acquire);
        // Clamp negative values to 0 for the u64 return type.
        #[expect(clippy::cast_sign_loss, reason = "clamped to non-negative")]
        Some(ms.max(0) as u64)
    } else {
        None
    }
}

// ── Fault Injection ────────────────────────────────────────────────────

/// A scheduled fault for a specific actor.
#[derive(Debug, Clone)]
pub(crate) struct ActorFault {
    /// Actor ID this fault applies to.
    pub actor_id: u64,
    /// Fault kind.
    pub kind: FaultKind,
}

/// Types of injectable faults.
#[derive(Debug, Clone)]
pub(crate) enum FaultKind {
    /// Simulate a crash (SIGSEGV-style) on next dispatch.
    Crash {
        /// Number of dispatches to crash (0 = cleared).
        remaining: u32,
    },
    /// Add artificial latency (milliseconds) to dispatch.
    Delay {
        /// Delay per dispatch in milliseconds.
        ms: u32,
    },
    /// Drop the next N messages to this actor's mailbox.
    Drop {
        /// Number of messages to drop (0 = cleared).
        remaining: u32,
    },
}

/// Global fault injection table. Protected by a mutex because fault
/// injection is a testing-only path and performance is not critical.
static FAULTS: Mutex<Vec<ActorFault>> = Mutex::new(Vec::new());

/// Inject a crash fault: the actor's next `count` dispatches will
/// simulate a crash (supervisor sees it as a SIGABRT-style failure).
///
/// `actor_id` is the ID returned by `hew_actor_get_id()`.
///
/// # Panics
///
/// Panics if the internal fault table mutex is poisoned.
#[no_mangle]
pub extern "C" fn hew_fault_inject_crash(actor_id: u64, count: u32) {
    let mut faults = FAULTS.lock().expect("fault lock poisoned");
    // Remove any existing crash fault for this actor.
    faults.retain(|f| !(f.actor_id == actor_id && matches!(f.kind, FaultKind::Crash { .. })));
    if count > 0 {
        faults.push(ActorFault {
            actor_id,
            kind: FaultKind::Crash { remaining: count },
        });
    }
}

/// Inject a delay fault: add `ms` milliseconds of latency to every
/// dispatch of this actor.
///
/// Set `ms = 0` to clear.
///
/// # Panics
///
/// Panics if the internal fault table mutex is poisoned.
#[no_mangle]
pub extern "C" fn hew_fault_inject_delay(actor_id: u64, ms: u32) {
    let mut faults = FAULTS.lock().expect("fault lock poisoned");
    faults.retain(|f| !(f.actor_id == actor_id && matches!(f.kind, FaultKind::Delay { .. })));
    if ms > 0 {
        faults.push(ActorFault {
            actor_id,
            kind: FaultKind::Delay { ms },
        });
    }
}

/// Inject a drop fault: silently drop the next `count` messages sent
/// to this actor's mailbox.
///
/// Set `count = 0` to clear.
///
/// # Panics
///
/// Panics if the internal fault table mutex is poisoned.
#[no_mangle]
pub extern "C" fn hew_fault_inject_drop(actor_id: u64, count: u32) {
    let mut faults = FAULTS.lock().expect("fault lock poisoned");
    faults.retain(|f| !(f.actor_id == actor_id && matches!(f.kind, FaultKind::Drop { .. })));
    if count > 0 {
        faults.push(ActorFault {
            actor_id,
            kind: FaultKind::Drop { remaining: count },
        });
    }
}

/// Clear all faults for a specific actor.
///
/// # Panics
///
/// Panics if the internal fault table mutex is poisoned.
#[no_mangle]
pub extern "C" fn hew_fault_clear(actor_id: u64) {
    let mut faults = FAULTS.lock().expect("fault lock poisoned");
    faults.retain(|f| f.actor_id != actor_id);
}

/// Clear all faults for all actors.
///
/// # Panics
///
/// Panics if the internal fault table mutex is poisoned.
#[no_mangle]
pub extern "C" fn hew_fault_clear_all() {
    let mut faults = FAULTS.lock().expect("fault lock poisoned");
    faults.clear();
}

/// Return the number of active faults (for testing).
///
/// # Panics
///
/// Panics if the internal fault table mutex is poisoned.
#[no_mangle]
pub extern "C" fn hew_fault_count() -> u32 {
    let faults = FAULTS.lock().expect("fault lock poisoned");
    #[expect(
        clippy::cast_possible_truncation,
        reason = "fault count will never exceed u32::MAX in practice"
    )]
    {
        faults.len() as u32
    }
}

/// Check and consume a crash fault for the given actor.
///
/// Returns `true` if a crash should be simulated (caller should mark
/// the actor as `Crashed` and notify the supervisor).
pub(crate) fn check_crash_fault(actor_id: u64) -> bool {
    let mut faults = FAULTS.lock().expect("fault lock poisoned");
    for fault in faults.iter_mut() {
        if fault.actor_id == actor_id {
            if let FaultKind::Crash { remaining } = &mut fault.kind {
                if *remaining > 0 {
                    *remaining -= 1;
                    return true;
                }
            }
        }
    }
    // Clean up exhausted crash faults.
    faults.retain(|f| {
        !matches!(
            f,
            ActorFault {
                kind: FaultKind::Crash { remaining: 0 },
                ..
            }
        )
    });
    false
}

/// Check and apply a delay fault for the given actor.
///
/// Returns the delay in milliseconds (0 = no delay).
pub(crate) fn check_delay_fault(actor_id: u64) -> u32 {
    let faults = FAULTS.lock().expect("fault lock poisoned");
    for fault in &*faults {
        if fault.actor_id == actor_id {
            if let FaultKind::Delay { ms } = fault.kind {
                return ms;
            }
        }
    }
    0
}

/// Check and consume a drop fault for the given actor.
///
/// Returns `true` if the message should be dropped.
pub(crate) fn check_drop_fault(actor_id: u64) -> bool {
    let mut faults = FAULTS.lock().expect("fault lock poisoned");
    for fault in faults.iter_mut() {
        if fault.actor_id == actor_id {
            if let FaultKind::Drop { remaining } = &mut fault.kind {
                if *remaining > 0 {
                    *remaining -= 1;
                    return true;
                }
            }
        }
    }
    // Clean up exhausted drop faults.
    faults.retain(|f| {
        !matches!(
            f,
            ActorFault {
                kind: FaultKind::Drop { remaining: 0 },
                ..
            }
        )
    });
    false
}

// ── Reset (for test isolation) ─────────────────────────────────────────

/// Reset all deterministic testing state.
///
/// Clears the global seed, disables simulated time, and removes all
/// faults. Intended for use in test teardown.
///
/// # Panics
///
/// Panics if the internal fault table mutex is poisoned.
#[no_mangle]
pub extern "C" fn hew_deterministic_reset() {
    GLOBAL_SEED.store(0, Ordering::Release);
    SIMTIME_ENABLED.store(false, Ordering::Release);
    SIMTIME_MS.store(0, Ordering::Release);
    let mut faults = FAULTS.lock().expect("fault lock poisoned");
    faults.clear();
}

// ── Tests ──────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    /// Serialize deterministic tests since they share global state
    /// (GLOBAL_SEED, SIMTIME_ENABLED, SIMTIME_MS, FAULTS).
    static TEST_LOCK: Mutex<()> = Mutex::new(());

    #[test]
    fn seed_control() {
        let _guard = TEST_LOCK.lock().expect("test lock poisoned");
        hew_deterministic_set_seed(42);
        assert_eq!(hew_deterministic_get_seed(), 42);

        // Worker 0 seed = 42 + 0 = 42
        assert_eq!(effective_worker_seed(0), 42);
        // Worker 3 seed = 42 + 3 = 45
        assert_eq!(effective_worker_seed(3), 45);

        // Default (seed = 0): worker 3 seed = 3 + 1 = 4
        hew_deterministic_set_seed(0);
        assert_eq!(effective_worker_seed(3), 4);
    }

    #[test]
    fn simulated_time() {
        let _guard = TEST_LOCK.lock().expect("test lock poisoned");
        hew_simtime_enable(1000);
        assert_eq!(hew_simtime_is_enabled(), 1);
        assert_eq!(hew_simtime_now_ms(), 1000);

        assert_eq!(simtime_now(), Some(1000));

        // Advance by 500ms
        let new_val = hew_simtime_advance_ms(500);
        assert_eq!(new_val, 1500);
        assert_eq!(hew_simtime_now_ms(), 1500);

        // Set absolute
        hew_simtime_set_ms(5000);
        assert_eq!(hew_simtime_now_ms(), 5000);

        hew_simtime_disable();
        assert_eq!(hew_simtime_is_enabled(), 0);
        assert_eq!(simtime_now(), None);
    }

    #[test]
    fn crash_fault_injection() {
        let _guard = TEST_LOCK.lock().expect("test lock poisoned");
        hew_deterministic_reset();

        // Inject crash for actor 100, 2 times
        hew_fault_inject_crash(100, 2);
        assert_eq!(hew_fault_count(), 1);

        // First dispatch: should crash
        assert!(check_crash_fault(100));
        // Second dispatch: should crash
        assert!(check_crash_fault(100));
        // Third dispatch: no more crashes
        assert!(!check_crash_fault(100));

        // Different actor: no faults
        assert!(!check_crash_fault(200));

        hew_deterministic_reset();
    }

    #[test]
    fn delay_fault_injection() {
        let _guard = TEST_LOCK.lock().expect("test lock poisoned");
        hew_deterministic_reset();

        hew_fault_inject_delay(100, 50);
        assert_eq!(check_delay_fault(100), 50);
        assert_eq!(check_delay_fault(200), 0);

        // Clear
        hew_fault_inject_delay(100, 0);
        assert_eq!(check_delay_fault(100), 0);

        hew_deterministic_reset();
    }

    #[test]
    fn drop_fault_injection() {
        let _guard = TEST_LOCK.lock().expect("test lock poisoned");
        hew_deterministic_reset();

        hew_fault_inject_drop(100, 3);

        // Drop 3 messages
        assert!(check_drop_fault(100));
        assert!(check_drop_fault(100));
        assert!(check_drop_fault(100));
        // 4th: no drop
        assert!(!check_drop_fault(100));

        hew_deterministic_reset();
    }

    #[test]
    fn clear_faults() {
        let _guard = TEST_LOCK.lock().expect("test lock poisoned");
        hew_deterministic_reset();

        hew_fault_inject_crash(100, 5);
        hew_fault_inject_delay(100, 20);
        hew_fault_inject_drop(100, 3);
        hew_fault_inject_crash(200, 1);

        assert_eq!(hew_fault_count(), 4);

        // Clear actor 100 only
        hew_fault_clear(100);
        assert_eq!(hew_fault_count(), 1);

        // Clear all
        hew_fault_clear_all();
        assert_eq!(hew_fault_count(), 0);

        hew_deterministic_reset();
    }

    #[test]
    fn full_reset() {
        let _guard = TEST_LOCK.lock().expect("test lock poisoned");
        hew_deterministic_set_seed(99);
        hew_simtime_enable(5000);
        hew_fault_inject_crash(1, 1);

        hew_deterministic_reset();

        assert_eq!(hew_deterministic_get_seed(), 0);
        assert_eq!(hew_simtime_is_enabled(), 0);
        assert_eq!(hew_fault_count(), 0);
    }
}
