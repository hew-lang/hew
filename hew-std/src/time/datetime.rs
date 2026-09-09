//! Hew runtime: `datetime` module.
//!
//! Provides datetime utilities for compiled Hew programs using Unix epoch
//! milliseconds as the canonical time representation. Returned strings are
//! managed strings: the caller receives one owner and releases it with
//! `hew_string_drop`. Null is the canonical empty string;
//! [`hew_datetime_last_error`] returns null when no error has been recorded.

use hew_cabi::string::{string_as_str, string_from_str, HewString};

use chrono::format::{Item, StrftimeItems};
use chrono::{DateTime, Datelike, NaiveDateTime, Timelike, Utc, Weekday};

/// Convert epoch milliseconds to a `DateTime<Utc>`, returning `None` if out of range.
fn epoch_ms_to_utc(epoch_ms: i64) -> Option<DateTime<Utc>> {
    DateTime::<Utc>::from_timestamp_millis(epoch_ms)
}

fn epoch_ms_component(epoch_ms: i64, component: impl FnOnce(DateTime<Utc>) -> i64) -> i64 {
    if let Some(dt) = epoch_ms_to_utc(epoch_ms) {
        clear_datetime_last_error();
        component(dt)
    } else {
        set_datetime_last_error("datetime epoch is out of range");
        -1
    }
}

fn set_datetime_last_error(msg: impl Into<String>) {
    hew_runtime::parse_error_slot::set_error(
        hew_runtime::parse_error_slot::ErrorSlotKind::Datetime,
        msg,
    );
}

fn clear_datetime_last_error() {
    hew_runtime::parse_error_slot::clear_error(
        hew_runtime::parse_error_slot::ErrorSlotKind::Datetime,
    );
}

fn clone_datetime_last_error() -> Option<String> {
    hew_runtime::parse_error_slot::get_error(hew_runtime::parse_error_slot::ErrorSlotKind::Datetime)
}

// ---------------------------------------------------------------------------
// Current time
// ---------------------------------------------------------------------------

/// Return the current time as Unix epoch milliseconds.
///
/// # Safety
///
/// No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_datetime_now_ms() -> i64 {
    Utc::now().timestamp_millis()
}

// ---------------------------------------------------------------------------
// Formatting / Parsing
// ---------------------------------------------------------------------------

/// Format epoch milliseconds using a `strftime` format string.
///
/// Returns an owned managed string; release it with `hew_string_drop`.
/// Returns null on invalid input.
///
/// # Safety
///
/// `fmt` must be null (canonical empty) or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_datetime_format(
    epoch_ms: i64,
    fmt: *const HewString,
) -> *mut HewString {
    // SAFETY: the caller borrows a live managed string or canonical empty handle.
    let fmt_str = unsafe { string_as_str(fmt) };
    if StrftimeItems::new(fmt_str).any(|item| matches!(item, Item::Error)) {
        set_datetime_last_error("invalid datetime format directive");
        return std::ptr::null_mut();
    }
    let Some(dt) = epoch_ms_to_utc(epoch_ms) else {
        set_datetime_last_error("datetime epoch is out of range");
        return std::ptr::null_mut();
    };
    clear_datetime_last_error();
    let formatted = dt.format(fmt_str).to_string();
    string_from_str(&formatted)
}

/// Parse a datetime string with the given `strftime` format, returning epoch
/// milliseconds. Returns -1 on parse error; call [`hew_datetime_last_error`] to
/// distinguish a parse failure from a valid pre-epoch `-1` timestamp.
///
/// # Safety
///
/// `s` and `fmt` must each be null (canonical empty) or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_datetime_parse(s: *const HewString, fmt: *const HewString) -> i64 {
    // SAFETY: the caller borrows a live managed string or canonical empty handle.
    let s_str = unsafe { string_as_str(s) };
    // SAFETY: the caller borrows a live managed string or canonical empty handle.
    let fmt_str = unsafe { string_as_str(fmt) };
    match NaiveDateTime::parse_from_str(s_str, fmt_str) {
        Ok(naive) => {
            clear_datetime_last_error();
            naive.and_utc().timestamp_millis()
        }
        Err(err) => {
            set_datetime_last_error(format!("parse error: {err}"));
            -1
        }
    }
}

/// Return the most recent parse error for this Hew actor.
///
/// Returns an owned managed string; release it with `hew_string_drop`.
/// Returns canonical empty (null) when no error is set.
///
/// Errors are keyed per (actor, parser-kind), so a different parser's success
/// does not clear this slot.
#[no_mangle]
pub extern "C" fn hew_datetime_last_error() -> *mut HewString {
    match clone_datetime_last_error() {
        Some(message) => string_from_str(&message),
        None => std::ptr::null_mut(),
    }
}

// ---------------------------------------------------------------------------
// Component extraction
// ---------------------------------------------------------------------------

/// Extract the year from epoch milliseconds. Returns -1 if out of range and
/// records an error retrievable through [`hew_datetime_last_error`].
///
/// # Safety
///
/// No preconditions — pure computation.
#[no_mangle]
pub unsafe extern "C" fn hew_datetime_year(epoch_ms: i64) -> i64 {
    epoch_ms_component(epoch_ms, |dt| i64::from(dt.year()))
}

/// Extract the month (1–12) from epoch milliseconds. Returns -1 if out of range.
///
/// # Safety
///
/// No preconditions — pure computation.
#[no_mangle]
pub unsafe extern "C" fn hew_datetime_month(epoch_ms: i64) -> i64 {
    epoch_ms_component(epoch_ms, |dt| i64::from(dt.month()))
}

/// Extract the day (1–31) from epoch milliseconds. Returns -1 if out of range.
///
/// # Safety
///
/// No preconditions — pure computation.
#[no_mangle]
pub unsafe extern "C" fn hew_datetime_day(epoch_ms: i64) -> i64 {
    epoch_ms_component(epoch_ms, |dt| i64::from(dt.day()))
}

/// Extract the hour (0–23) from epoch milliseconds. Returns -1 if out of range.
///
/// # Safety
///
/// No preconditions — pure computation.
#[no_mangle]
pub unsafe extern "C" fn hew_datetime_hour(epoch_ms: i64) -> i64 {
    epoch_ms_component(epoch_ms, |dt| i64::from(dt.hour()))
}

/// Extract the minute (0–59) from epoch milliseconds. Returns -1 if out of range.
///
/// # Safety
///
/// No preconditions — pure computation.
#[no_mangle]
pub unsafe extern "C" fn hew_datetime_minute(epoch_ms: i64) -> i64 {
    epoch_ms_component(epoch_ms, |dt| i64::from(dt.minute()))
}

/// Extract the second (0–59) from epoch milliseconds. Returns -1 if out of range.
///
/// # Safety
///
/// No preconditions — pure computation.
#[no_mangle]
pub unsafe extern "C" fn hew_datetime_second(epoch_ms: i64) -> i64 {
    epoch_ms_component(epoch_ms, |dt| i64::from(dt.second()))
}

/// Return the day of the week (0=Mon, 6=Sun) from epoch milliseconds.
/// Returns -1 if out of range.
///
/// # Safety
///
/// No preconditions — pure computation.
#[no_mangle]
pub unsafe extern "C" fn hew_datetime_weekday(epoch_ms: i64) -> i64 {
    epoch_ms_component(epoch_ms, |dt| {
        i64::from(match dt.weekday() {
            Weekday::Mon => 0,
            Weekday::Tue => 1,
            Weekday::Wed => 2,
            Weekday::Thu => 3,
            Weekday::Fri => 4,
            Weekday::Sat => 5,
            Weekday::Sun => 6,
        })
    })
}

/// Return the current monotonic clock time in nanoseconds.
///
/// High-resolution timing suitable for benchmarking; not affected by
/// wall-clock adjustments. The zero point is the single process-wide
/// monotonic epoch in `hew_runtime::monotonic`, shared with every runtime
/// subsystem (timers, supervisor, crash, tracing, SWIM) so this datetime
/// reading and a runtime timestamp count from the same instant.
///
/// # Safety
///
/// No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_datetime_now_nanos() -> i64 {
    #[expect(
        clippy::cast_possible_wrap,
        reason = "monotonic ns since process start won't exceed i64 (~292 years)"
    )]
    {
        hew_runtime::monotonic::monotonic_ns() as i64
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_string::ManagedString;
    use hew_cabi::string::string_release;

    /// Helper to read a managed string and release it.
    ///
    /// # Safety
    ///
    /// `ptr` must be a non-null, live managed string owner.
    unsafe fn read_and_free(ptr: *mut HewString) -> String {
        assert!(!ptr.is_null());
        // SAFETY: ptr is the live owner returned by the producer per caller.
        let s = unsafe { string_as_str(ptr) }.to_owned();
        // SAFETY: this test holds the only owner of `ptr`.
        unsafe { string_release(ptr) };
        s
    }

    #[test]
    fn test_format_parse_roundtrip() {
        // 2024-01-15 09:30:00 UTC
        let epoch_ms: i64 = 1_705_311_000_000;
        let fmt = ManagedString::new("%Y-%m-%d %H:%M:%S");

        // SAFETY: fmt is a live managed string handle.
        let formatted = unsafe { hew_datetime_format(epoch_ms, fmt.as_ptr()) };
        // SAFETY: formatted was returned by hew_datetime_format.
        let text = unsafe { read_and_free(formatted) };
        assert_eq!(text, "2024-01-15 09:30:00");

        let input = ManagedString::new(&text);
        // SAFETY: both handles are live managed strings.
        let parsed = unsafe { hew_datetime_parse(input.as_ptr(), fmt.as_ptr()) };
        assert_eq!(parsed, epoch_ms);
    }

    #[test]
    fn test_component_extraction() {
        // 2024-03-15 14:30:45 UTC (Friday)
        let epoch_ms: i64 = 1_710_513_045_000;

        // SAFETY: all component functions have no preconditions.
        unsafe {
            assert_eq!(hew_datetime_year(epoch_ms), 2024);
            assert_eq!(hew_datetime_month(epoch_ms), 3);
            assert_eq!(hew_datetime_day(epoch_ms), 15);
            assert_eq!(hew_datetime_hour(epoch_ms), 14);
            assert_eq!(hew_datetime_minute(epoch_ms), 30);
            assert_eq!(hew_datetime_second(epoch_ms), 45);
            assert_eq!(hew_datetime_weekday(epoch_ms), 4); // Friday = 4
        }
    }

    #[test]
    fn test_now_returns_reasonable_values() {
        // SAFETY: now functions have no preconditions.
        unsafe {
            let ms = hew_datetime_now_ms();
            // Should be after 2024-01-01.
            assert!(ms > 1_704_067_200_000);
        }
    }

    #[test]
    fn test_parse_error_returns_negative_one() {
        let bad_input = ManagedString::new("not-a-date");
        let fmt = ManagedString::new("%Y-%m-%dT%H:%M:%S%.3fZ");
        // SAFETY: both handles are live managed strings.
        let result = unsafe { hew_datetime_parse(bad_input.as_ptr(), fmt.as_ptr()) };
        assert_eq!(result, -1);

        // SAFETY: hew_datetime_last_error returns a live managed string.
        let err = unsafe { read_and_free(hew_datetime_last_error()) };
        assert!(err.contains("parse"));
    }

    #[test]
    fn test_valid_pre_epoch_negative_one_has_no_last_error() {
        let input = ManagedString::new("1969-12-31T23:59:59.999Z");
        let fmt = ManagedString::new("%Y-%m-%dT%H:%M:%S%.3fZ");
        // SAFETY: both handles are live managed strings.
        let result = unsafe { hew_datetime_parse(input.as_ptr(), fmt.as_ptr()) };
        assert_eq!(result, -1);
        // SAFETY: hew_datetime_year has no preconditions for a valid epoch timestamp.
        assert_eq!(unsafe { hew_datetime_year(result) }, 1969);
        let err = hew_datetime_last_error();
        assert!(err.is_null());
    }

    #[test]
    fn test_positive_epoch_parse_returns_legitimate_value() {
        let input = ManagedString::new("2026-01-01T00:00:00Z");
        let fmt = ManagedString::new("%Y-%m-%dT%H:%M:%SZ");
        // SAFETY: both handles are live managed strings.
        let result = unsafe { hew_datetime_parse(input.as_ptr(), fmt.as_ptr()) };
        assert_eq!(result, 1_767_225_600_000);
        let err = hew_datetime_last_error();
        assert!(err.is_null());
    }

    #[test]
    fn test_successful_parse_clears_last_error() {
        let bad_input = ManagedString::new("not-a-date");
        let bad_fmt = ManagedString::new("%Y-%m-%dT%H:%M:%S%.3fZ");
        // SAFETY: both handles are live managed strings.
        let bad_result = unsafe { hew_datetime_parse(bad_input.as_ptr(), bad_fmt.as_ptr()) };
        assert_eq!(bad_result, -1);
        // SAFETY: hew_datetime_last_error returns a live managed string.
        let err = unsafe { read_and_free(hew_datetime_last_error()) };
        assert!(err.contains("parse"));

        let input = ManagedString::new("2026-01-01T00:00:00Z");
        let fmt = ManagedString::new("%Y-%m-%dT%H:%M:%SZ");
        // SAFETY: both handles are live managed strings.
        let result = unsafe { hew_datetime_parse(input.as_ptr(), fmt.as_ptr()) };
        assert_eq!(result, 1_767_225_600_000);
        let err = hew_datetime_last_error();
        assert!(err.is_null());
    }

    /// `hew_datetime_now_nanos` must read the SAME process-wide monotonic epoch
    /// as `hew_runtime::monotonic`, not a private datetime epoch.
    ///
    /// This is the single-epoch invariant for the datetime clock (issue #2060,
    /// CAP-09). The test has teeth: it primes the shared epoch, sleeps a
    /// measurable interval, then samples the datetime clock. With a shared
    /// epoch, the datetime reading already includes the elapsed interval. With a
    /// private datetime epoch (the pre-fold behaviour), the first datetime call
    /// would capture a fresh zero point at that moment and read near-zero —
    /// well below the slept interval — so this assertion fails.
    #[test]
    fn now_nanos_shares_the_runtime_process_epoch() {
        // Prime the shared process epoch so its zero point is firmly in the past
        // before we ever touch the datetime clock.
        let before = hew_runtime::monotonic::monotonic_ns();

        let sleep = std::time::Duration::from_millis(20);
        std::thread::sleep(sleep);

        // SAFETY: hew_datetime_now_nanos has no preconditions.
        let dt_after = unsafe { hew_datetime_now_nanos() };
        assert!(
            dt_after >= 0,
            "datetime monotonic reading must be non-negative"
        );

        let sleep_ns = i64::try_from(sleep.as_nanos()).expect("20ms fits i64");
        // A shared epoch means the datetime reading is anchored to the same
        // zero point primed above, so it must reflect at least the slept gap.
        // A private epoch captured at the datetime call would read far below it.
        assert!(
            dt_after >= sleep_ns,
            "hew_datetime_now_nanos read {dt_after}ns, below the {sleep_ns}ns slept since \
             the shared epoch was primed (before={before}ns) — datetime is on its own epoch"
        );

        // And the two clocks must stay tightly coupled: a fresh runtime reading
        // taken just after the datetime reading is only slightly ahead, never a
        // full second apart, which an independent epoch would produce.
        let runtime_after = hew_runtime::monotonic::monotonic_ns();
        let dt_after_u = u64::try_from(dt_after).expect("non-negative datetime ns fits u64");
        assert!(
            runtime_after >= dt_after_u,
            "runtime reading {runtime_after}ns sampled after datetime {dt_after_u}ns must be \
             at or ahead of it on one shared clock"
        );
        let skew = runtime_after - dt_after_u;
        assert!(
            skew < 1_000_000_000,
            "datetime and runtime monotonic clocks diverged by {skew}ns (>1s) — not one epoch"
        );
    }
}
