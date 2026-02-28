//! Crash forensics and reporting for the Hew actor runtime.
//!
//! Provides crash reporting, per-actor crash statistics, and a global crash log
//! for debugging actor crashes. This builds on the signal handling infrastructure
//! in [`crate::signal`] to record detailed crash information.
//!
//! # Architecture
//!
//! - [`CrashReport`] — detailed crash information for a single incident
//! - [`CrashStats`] — per-actor crash statistics with sliding window
//! - [`RECENT_CRASHES`] — global ring buffer of recent crashes (64 entries)
//! - C ABI functions for allocating/managing crash statistics

use std::collections::VecDeque;
use std::sync::Mutex;

// ── Crash report struct ────────────────────────────────────────────────

/// Detailed information about a single actor crash.
///
/// This struct is exchanged with C code and must maintain ABI compatibility.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct CrashReport {
    /// Unique ID of the crashed actor.
    pub actor_id: u64,
    /// Process identifier (PID) of the crashed actor.
    pub actor_pid: u64,
    /// Signal number that caused the crash (SIGSEGV=11, SIGBUS=7, etc.).
    pub signal: i32,
    /// Signal sub-code (`SEGV_MAPERR=1`, `SEGV_ACCERR=2`, etc.).
    pub signal_code: i32,
    /// Memory address that caused the fault.
    pub fault_addr: usize,
    /// Message type being processed when the crash occurred.
    pub msg_type: i32,
    /// Monotonic timestamp in nanoseconds when the crash occurred.
    pub timestamp_ns: u64,
    /// Worker thread ID that was executing the actor.
    pub worker_id: u32,
    /// Total crash count for this actor (from `CrashStats`).
    pub total_crashes: u32,
}

impl CrashReport {
    /// Create a zeroed crash report (for when no crashes have occurred).
    const fn zeroed() -> Self {
        Self {
            actor_id: 0,
            actor_pid: 0,
            signal: 0,
            signal_code: 0,
            fault_addr: 0,
            msg_type: 0,
            timestamp_ns: 0,
            worker_id: 0,
            total_crashes: 0,
        }
    }
}

// ── Per-actor crash statistics ─────────────────────────────────────────

/// Per-actor crash statistics with sliding window tracking.
///
/// Maintains a count of total crashes and a circular buffer of recent
/// crash timestamps for rate limiting and supervision decisions.
#[repr(C)]
#[derive(Debug)]
pub struct CrashStats {
    /// Total number of crashes for this actor over its lifetime.
    pub total_crashes: u32,
    /// Circular buffer of recent crash timestamps (monotonic nanoseconds).
    pub recent_timestamps: [u64; 8],
    /// Head index into the circular buffer (next write position).
    pub recent_head: u32,
    /// Signal number from the most recent crash.
    pub last_signal: i32,
}

impl CrashStats {
    /// Create new crash statistics with all fields zeroed.
    const fn new() -> Self {
        Self {
            total_crashes: 0,
            recent_timestamps: [0; 8],
            recent_head: 0,
            last_signal: 0,
        }
    }

    /// Record a new crash, updating statistics.
    fn record_crash(&mut self, signal: i32, timestamp_ns: u64) {
        self.total_crashes = self.total_crashes.saturating_add(1);
        self.last_signal = signal;

        // Add to circular buffer
        let idx = (self.recent_head as usize) % 8;
        self.recent_timestamps[idx] = timestamp_ns;
        self.recent_head = self.recent_head.wrapping_add(1);
    }

    /// Count crashes within a time window (in nanoseconds).
    fn recent_crash_count(&self, window_ns: u64, now_ns: u64) -> u32 {
        let cutoff_time = now_ns.saturating_sub(window_ns);
        let valid_entries = (self.total_crashes as usize).min(self.recent_timestamps.len());
        let mut count = 0;

        // Only examine slots that have been written to. The circular buffer
        // is zero-initialized, and zero timestamps would be spuriously
        // counted when the monotonic epoch is younger than the window.
        for i in 0..valid_entries {
            // Walk backwards from the most recent entry.
            let idx = (self.recent_head as usize + self.recent_timestamps.len() - 1 - i)
                % self.recent_timestamps.len();
            let timestamp = self.recent_timestamps[idx];
            if timestamp >= cutoff_time && timestamp <= now_ns {
                count += 1;
            }
        }

        count
    }
}

// ── Global crash log ────────────────────────────────────────────────────

/// Global ring buffer of recent crashes, bounded to 64 entries.
static RECENT_CRASHES: Mutex<VecDeque<CrashReport>> = Mutex::new(VecDeque::new());

/// Maximum number of crashes to keep in the global log.
const MAX_CRASH_LOG_SIZE: usize = 64;

/// Add a crash report to the global crash log.
///
/// If the log is full, removes the oldest entry to make room.
pub(crate) fn push_crash_report(report: CrashReport) {
    if let Ok(mut crashes) = RECENT_CRASHES.lock() {
        // Make room if needed
        while crashes.len() >= MAX_CRASH_LOG_SIZE {
            crashes.pop_front();
        }
        crashes.push_back(report);
    }
}

/// Record a fault-injected crash in the global crash log.
///
/// Creates a minimal crash report with `signal = -1` (injected) so it
/// can be distinguished from real crashes.
pub(crate) fn record_injected_crash(actor_id: u64) {
    let report = CrashReport {
        actor_id,
        actor_pid: 0,
        signal: -1, // Indicates injected fault, not a real signal.
        signal_code: 0,
        fault_addr: 0,
        msg_type: 0,
        timestamp_ns: monotonic_time_ns(),
        worker_id: 0,
        total_crashes: 0,
    };
    push_crash_report(report);
}

// ── Monotonic timestamp utility ─────────────────────────────────────────

/// Get the current monotonic time in nanoseconds (cross-platform).
fn monotonic_time_ns() -> u64 {
    use std::sync::OnceLock;
    use std::time::Instant;
    static EPOCH: OnceLock<Instant> = OnceLock::new();
    let epoch = EPOCH.get_or_init(Instant::now);
    #[expect(
        clippy::cast_possible_truncation,
        reason = "monotonic ns since process start won't exceed u64"
    )]
    {
        epoch.elapsed().as_nanos() as u64
    }
}

// ── C ABI functions ─────────────────────────────────────────────────────

/// Allocate a new crash statistics structure.
///
/// Returns a pointer to a heap-allocated [`CrashStats`] initialized to zero.
/// The caller must free it with [`hew_crash_stats_free`].
///
/// # Safety
///
/// No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_crash_stats_new() -> *mut CrashStats {
    let stats = Box::new(CrashStats::new());
    Box::into_raw(stats)
}

/// Record a crash in the statistics, updating counters and timestamps.
///
/// # Safety
///
/// `stats` must be a valid pointer returned by [`hew_crash_stats_new`]
/// and must not be used concurrently from multiple threads.
#[no_mangle]
pub unsafe extern "C" fn hew_crash_stats_record(
    stats: *mut CrashStats,
    signal: i32,
    timestamp_ns: u64,
) {
    if stats.is_null() {
        return;
    }

    // SAFETY: Caller guarantees stats is valid and non-null.
    let stats_ref = unsafe { &mut *stats };
    stats_ref.record_crash(signal, timestamp_ns);
}

/// Count crashes within a time window from the current time.
///
/// Returns the number of crashes that occurred within `window_ns` nanoseconds
/// of the current monotonic time.
///
/// # Safety
///
/// `stats` must be a valid pointer returned by [`hew_crash_stats_new`]
/// and must not be used concurrently from multiple threads.
#[no_mangle]
pub unsafe extern "C" fn hew_crash_stats_recent_count(
    stats: *mut CrashStats,
    window_ns: u64,
) -> u32 {
    if stats.is_null() {
        return 0;
    }

    let now = monotonic_time_ns();
    // SAFETY: Caller guarantees stats is valid and non-null.
    let stats_ref = unsafe { &*stats };
    stats_ref.recent_crash_count(window_ns, now)
}

/// Free a crash statistics structure.
///
/// # Safety
///
/// `stats` must be a valid pointer returned by [`hew_crash_stats_new`]
/// and must not be used after this call.
#[no_mangle]
pub unsafe extern "C" fn hew_crash_stats_free(stats: *mut CrashStats) {
    if stats.is_null() {
        return;
    }

    // SAFETY: Caller guarantees stats was created by hew_crash_stats_new.
    drop(unsafe { Box::from_raw(stats) });
}

/// Add a crash report to the global crash log.
///
/// # Safety
///
/// No preconditions — `report` is passed by value.
#[no_mangle]
pub unsafe extern "C" fn hew_crash_log_push(report: CrashReport) {
    push_crash_report(report);
}

/// Get the total number of crashes recorded in the global log.
///
/// # Safety
///
/// No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_crash_log_count() -> i32 {
    RECENT_CRASHES.lock().map_or(0, |crashes| {
        #[expect(
            clippy::cast_possible_wrap,
            clippy::cast_possible_truncation,
            reason = "crash log is bounded to 64 entries, well within i32 positive range"
        )]
        {
            crashes.len() as i32
        }
    })
}

/// Get the most recent crash report from the global log.
///
/// Returns a zeroed [`CrashReport`] if no crashes have been recorded.
///
/// # Safety
///
/// No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_crash_log_last() -> CrashReport {
    RECENT_CRASHES
        .lock()
        .map_or(CrashReport::zeroed(), |crashes| {
            crashes.back().copied().unwrap_or_else(CrashReport::zeroed)
        })
}

// ── Integration with signal recovery ────────────────────────────────────

/// Build a crash report from signal recovery context.
///
/// This function is called from [`crate::signal::handle_crash_recovery`]
/// to create a detailed crash report from the recovery context.
///
/// # Safety
///
/// Must be called from a worker thread immediately after crash recovery.
/// `actor` must be a valid pointer to the crashed actor.
pub(crate) unsafe fn build_crash_report(
    actor: *mut crate::actor::HewActor,
    signal: i32,
    signal_code: i32,
    fault_addr: usize,
    msg_type: i32,
    worker_id: u32,
) -> CrashReport {
    let timestamp_ns = monotonic_time_ns();

    // SAFETY: Caller guarantees actor is valid.
    {
        let (id, pid) = if actor.is_null() {
            (0, 0)
        } else {
            // SAFETY: Caller guarantees actor pointer is valid.
            unsafe { ((*actor).id, (*actor).pid) }
        };

        CrashReport {
            actor_id: id,
            actor_pid: pid,
            signal,
            signal_code,
            fault_addr,
            msg_type,
            timestamp_ns,
            worker_id,
            total_crashes: 0, // Will be updated by caller if they have CrashStats
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn crash_stats_new_initializes_zero() {
        // SAFETY: No preconditions for this test.
        let stats = unsafe { hew_crash_stats_new() };
        assert!(!stats.is_null());

        // SAFETY: stats was just created and is valid.
        unsafe {
            assert_eq!((*stats).total_crashes, 0);
            assert_eq!((*stats).recent_head, 0);
            assert_eq!((*stats).last_signal, 0);
            hew_crash_stats_free(stats);
        }
    }

    #[test]
    fn crash_stats_record_updates_counters() {
        // SAFETY: No preconditions for this test.
        let stats = unsafe { hew_crash_stats_new() };

        // Record a crash
        // SAFETY: stats is valid and we're single-threaded in test.
        unsafe {
            hew_crash_stats_record(stats, 11, 1000);

            assert_eq!((*stats).total_crashes, 1);
            assert_eq!((*stats).last_signal, 11);
            assert_eq!((*stats).recent_timestamps[0], 1000);
            assert_eq!((*stats).recent_head, 1);

            hew_crash_stats_free(stats);
        }
    }

    #[test]
    fn crash_stats_circular_buffer_wraps() {
        // SAFETY: No preconditions for this test.
        let stats = unsafe { hew_crash_stats_new() };

        // Fill the circular buffer and wrap around
        // SAFETY: stats is valid and we're single-threaded in test.
        unsafe {
            for i in 0..10 {
                hew_crash_stats_record(stats, 11, 1000 + i);
            }

            assert_eq!((*stats).total_crashes, 10);
            assert_eq!((*stats).recent_head, 10);
            // Check that old entries were overwritten
            assert_eq!((*stats).recent_timestamps[0], 1008); // 10 % 8 = 2, so slot 0 has timestamp 1008
            assert_eq!((*stats).recent_timestamps[1], 1009); // 11 % 8 = 3, so slot 1 has timestamp 1009

            hew_crash_stats_free(stats);
        }
    }

    #[test]
    fn crash_log_push_and_count() {
        let report = CrashReport {
            actor_id: 42,
            actor_pid: 100,
            signal: 11,
            signal_code: 1,
            fault_addr: 0xdeadbeef,
            msg_type: 5,
            timestamp_ns: 1000,
            worker_id: 3,
            total_crashes: 1,
        };

        // SAFETY: No preconditions for test.
        unsafe {
            let initial_count = hew_crash_log_count();
            hew_crash_log_push(report);
            let new_count = hew_crash_log_count();

            assert_eq!(new_count, initial_count + 1);

            let last = hew_crash_log_last();
            assert_eq!(last.actor_id, 42);
            assert_eq!(last.signal, 11);
        }
    }

    #[test]
    fn crash_report_zeroed() {
        let report = CrashReport::zeroed();
        assert_eq!(report.actor_id, 0);
        assert_eq!(report.signal, 0);
        assert_eq!(report.fault_addr, 0);
    }

    #[test]
    fn monotonic_time_is_reasonable() {
        let t1 = monotonic_time_ns();
        std::thread::sleep(std::time::Duration::from_millis(1));
        let t2 = monotonic_time_ns();

        // Should have advanced by at least 1ms (1_000_000 ns)
        assert!(t2 > t1);
        assert!(t2 - t1 >= 1_000_000);
    }
}
