//! Monotonic clock, blocking sleep and duration arithmetic.
//!
//! Compiled on every target: the C ABI here reads a clock and does integer
//! arithmetic, and `wasm32-wasip1` supplies both through wasi-libc. Suspending
//! callers never reach `hew_sleep_ns` — MIR lowering arms the timer wheel
//! through `coro_sleep` instead — so this module holds no scheduler state.
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

pub use crate::duration::{hew_milliseconds, hew_seconds, HewDuration};

// ---------------------------------------------------------------------------
// Sleep / Clock
// ---------------------------------------------------------------------------

/// Sleep for `ns` nanoseconds (the `sleep(duration)` ABI).
///
/// Called by the blocking (free-fn) path. Suspending actor callers are
/// intercepted at the MIR lowering stage and arm the timer wheel directly.
///
/// # Safety
///
/// No preconditions — delegates to the OS.
#[no_mangle]
pub unsafe extern "C" fn hew_sleep_ns(ns: i64) {
    if ns > 0 {
        // SAFETY: ns > 0 checked above, so cast is lossless.
        #[expect(clippy::cast_sign_loss, reason = "guarded by ns > 0")]
        let dur = std::time::Duration::from_nanos(ns as u64);
        std::thread::sleep(dur);
    }
}

/// Sleep until `instant_ns` (nanosecond monotonic timestamp).
///
/// Computes remaining = `instant_ns - now`; if positive, sleeps that duration.
/// Called by the blocking (free-fn) path. Suspending actor callers are
/// intercepted at MIR lowering and arm the timer wheel for the remaining ms.
///
/// # Safety
///
/// No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_sleep_until_ns(instant_ns: i64) {
    // SAFETY: hew_instant_now has no preconditions.
    let now_ns = unsafe { hew_instant_now() };
    let remaining_ns = instant_ns.saturating_sub(now_ns);
    if remaining_ns > 0 {
        // SAFETY: remaining_ns > 0 checked above.
        #[expect(clippy::cast_sign_loss, reason = "guarded by remaining_ns > 0")]
        let dur = std::time::Duration::from_nanos(remaining_ns as u64);
        std::thread::sleep(dur);
    }
}

/// Cross-platform monotonic clock in milliseconds, anchored on the
/// process-wide epoch ([`crate::monotonic`]).
fn monotonic_ms() -> u64 {
    crate::monotonic::monotonic_ms()
}

/// Return the current monotonic clock time in milliseconds.
///
/// When simulated time is enabled (via [`crate::deterministic::hew_simtime_enable`]),
/// returns the simulated clock value instead of the real clock.
///
/// # Safety
///
/// No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_now_ms() -> u64 {
    // Check simulated time first (testing fast-path).
    if let Some(ms) = crate::deterministic::simtime_now() {
        return ms;
    }

    monotonic_ms()
}

/// Return the current monotonic clock time in nanoseconds.
///
/// # Safety
///
/// No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_instant_now() -> i64 {
    let ms = crate::deterministic::simtime_now().unwrap_or_else(monotonic_ms);
    i64::try_from(ms)
        .unwrap_or(i64::MAX)
        .saturating_mul(1_000_000)
}

/// Return elapsed nanoseconds since `instant_ns`.
///
/// # Safety
///
/// No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_instant_elapsed(instant_ns: i64) -> i64 {
    // SAFETY: hew_instant_now has no preconditions.
    let now = unsafe { hew_instant_now() };
    now.saturating_sub(instant_ns)
}

/// Return the saturating duration between two instant nanosecond stamps.
///
/// # Safety
///
/// No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_instant_duration_since(later_ns: i64, earlier_ns: i64) -> i64 {
    later_ns.saturating_sub(earlier_ns)
}

/// Return duration nanoseconds unchanged.
///
/// # Safety
///
/// No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_duration_nanos(duration_ns: i64) -> i64 {
    duration_ns
}

/// Return duration whole microseconds.
///
/// # Safety
///
/// No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_duration_micros(duration_ns: i64) -> i64 {
    duration_ns / 1_000
}

/// Return duration whole milliseconds.
///
/// # Safety
///
/// No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_duration_millis(duration_ns: i64) -> i64 {
    duration_ns / 1_000_000
}

/// Return duration whole seconds.
///
/// # Safety
///
/// No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_duration_secs(duration_ns: i64) -> i64 {
    duration_ns / 1_000_000_000
}

/// Return duration whole minutes.
///
/// # Safety
///
/// No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_duration_mins(duration_ns: i64) -> i64 {
    duration_ns / 60_000_000_000
}

/// Return duration whole hours.
///
/// # Safety
///
/// No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_duration_hours(duration_ns: i64) -> i64 {
    duration_ns / 3_600_000_000_000
}

/// Return the saturating absolute value of a duration.
///
/// # Safety
///
/// No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_duration_abs(duration_ns: i64) -> i64 {
    duration_ns.saturating_abs()
}

/// Return 1 if a duration is zero, otherwise 0.
///
/// # Safety
///
/// No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_duration_is_zero(duration_ns: i64) -> i32 {
    i32::from(duration_ns == 0)
}
