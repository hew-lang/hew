//! Process-wide monotonic clock epoch.
//!
//! Every subsystem that reports a "duration since process start" — timers,
//! the supervisor restart window, crash timestamps, tracing event ages, the
//! SWIM failure detector — must anchor against the **same** [`Instant`]. A
//! per-module epoch makes timestamps from two subsystems incomparable (e.g. a
//! crash report's `ts_ns` and a trace event's `ts_ns` would count from two
//! different zero points), and means the process exposes several slightly
//! different monotonic clocks instead of one.
//!
//! This module owns the single anchor. The epoch is lazily initialized on the
//! first read via [`OnceLock`], so the zero point is "first time anything in
//! the process asked for the monotonic clock" — stable for the lifetime of the
//! process and shared by every caller.

use std::sync::OnceLock;
use std::time::Instant;

/// The single process-wide monotonic epoch. Initialized on first read.
static EPOCH: OnceLock<Instant> = OnceLock::new();

/// Return the process-wide monotonic epoch [`Instant`].
///
/// All subsystems anchor relative durations against this single instant so
/// that timestamps from different subsystems share one zero point.
#[inline]
#[must_use]
pub fn process_epoch() -> Instant {
    *EPOCH.get_or_init(Instant::now)
}

/// Nanoseconds elapsed since the process-wide monotonic epoch.
#[inline]
#[must_use]
pub fn monotonic_ns() -> u64 {
    #[expect(
        clippy::cast_possible_truncation,
        reason = "monotonic ns since process start won't exceed u64 (~584 years)"
    )]
    {
        process_epoch().elapsed().as_nanos() as u64
    }
}

/// Milliseconds elapsed since the process-wide monotonic epoch.
#[inline]
#[must_use]
pub fn monotonic_ms() -> u64 {
    #[expect(
        clippy::cast_possible_truncation,
        reason = "monotonic ms since process start won't exceed u64"
    )]
    {
        process_epoch().elapsed().as_millis() as u64
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn epoch_is_stable_across_calls() {
        // The same anchor must come back every time; a per-call epoch would
        // make this fail.
        assert_eq!(process_epoch(), process_epoch());
    }

    #[test]
    fn monotonic_ns_is_non_decreasing() {
        let a = monotonic_ns();
        let b = monotonic_ns();
        assert!(b >= a, "monotonic clock went backwards: {a} -> {b}");
    }

    #[test]
    fn ms_and_ns_share_one_epoch() {
        // Both helpers measure from the same anchor, so ms*1e6 must be close to
        // ns (within a generous slack for the time between the two reads).
        let ns = monotonic_ns();
        let ms = monotonic_ms();
        let ns_from_ms = ms.saturating_mul(1_000_000);
        // ns was sampled first; ms slightly later. ns_from_ms truncates to the
        // ms boundary, so it can be up to ~1ms below ns, and at most a few ms
        // above due to elapsed time between the two reads.
        let diff = ns.abs_diff(ns_from_ms);
        assert!(
            diff < 50_000_000,
            "ms and ns epochs diverged: ns={ns} ns_from_ms={ns_from_ms} diff={diff}"
        );
    }
}
