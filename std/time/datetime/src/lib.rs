//! Hew runtime: date/time formatting, parsing, and arithmetic.
//!
//! Provides datetime utilities for compiled Hew programs using Unix epoch
//! milliseconds as the canonical time representation. All returned strings
//! are allocated with `libc::malloc` so callers can free them with `libc::free`.

use hew_cabi::cabi::malloc_cstring;
use std::ffi::{c_char, CStr};

use chrono::{DateTime, Datelike, Duration, NaiveDateTime, Timelike, Utc, Weekday};

/// Convert epoch milliseconds to a `DateTime<Utc>`, returning `None` if out of range.
fn epoch_ms_to_utc(epoch_ms: i64) -> Option<DateTime<Utc>> {
    DateTime::<Utc>::from_timestamp_millis(epoch_ms)
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

/// Return the current time as Unix epoch seconds.
///
/// # Safety
///
/// No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_datetime_now_secs() -> i64 {
    Utc::now().timestamp()
}

// ---------------------------------------------------------------------------
// Formatting / Parsing
// ---------------------------------------------------------------------------

/// Format epoch milliseconds using a `strftime` format string.
///
/// Returns a `malloc`-allocated, NUL-terminated C string. The caller must
/// free it with `libc::free`. Returns null on invalid input.
///
/// # Safety
///
/// `fmt` must be a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_datetime_format(epoch_ms: i64, fmt: *const c_char) -> *mut c_char {
    if fmt.is_null() {
        return std::ptr::null_mut();
    }
    let Some(dt) = epoch_ms_to_utc(epoch_ms) else {
        return std::ptr::null_mut();
    };
    // SAFETY: caller guarantees `fmt` is a valid NUL-terminated C string.
    let Ok(fmt_str) = (unsafe { CStr::from_ptr(fmt) }).to_str() else {
        return std::ptr::null_mut();
    };
    let formatted = dt.format(fmt_str).to_string();
    // SAFETY: formatted.as_ptr() is valid for formatted.len() bytes.
    unsafe { malloc_cstring(formatted.as_ptr(), formatted.len()) }
}

/// Parse a datetime string with the given `strftime` format, returning epoch
/// milliseconds. Returns -1 on parse error.
///
/// # Safety
///
/// Both `s` and `fmt` must be valid NUL-terminated C strings.
#[no_mangle]
pub unsafe extern "C" fn hew_datetime_parse(s: *const c_char, fmt: *const c_char) -> i64 {
    if s.is_null() || fmt.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees both pointers are valid NUL-terminated C strings.
    let Ok(s_str) = (unsafe { CStr::from_ptr(s) }).to_str() else {
        return -1;
    };
    // SAFETY: caller guarantees fmt is a valid NUL-terminated C string.
    let Ok(fmt_str) = (unsafe { CStr::from_ptr(fmt) }).to_str() else {
        return -1;
    };
    let Ok(naive) = NaiveDateTime::parse_from_str(s_str, fmt_str) else {
        return -1;
    };
    naive.and_utc().timestamp_millis()
}

// ---------------------------------------------------------------------------
// Component extraction
// ---------------------------------------------------------------------------

/// Extract the year from epoch milliseconds. Returns 0 if out of range.
///
/// # Safety
///
/// No preconditions — pure computation.
#[no_mangle]
pub unsafe extern "C" fn hew_datetime_year(epoch_ms: i64) -> i32 {
    epoch_ms_to_utc(epoch_ms).map_or(0, |dt| dt.year())
}

/// Extract the month (1–12) from epoch milliseconds. Returns 0 if out of range.
///
/// # Safety
///
/// No preconditions — pure computation.
#[expect(
    clippy::cast_possible_wrap,
    reason = "month is 1..=12, always fits in i32"
)]
#[no_mangle]
pub unsafe extern "C" fn hew_datetime_month(epoch_ms: i64) -> i32 {
    epoch_ms_to_utc(epoch_ms).map_or(0, |dt| dt.month() as i32)
}

/// Extract the day (1–31) from epoch milliseconds. Returns 0 if out of range.
///
/// # Safety
///
/// No preconditions — pure computation.
#[expect(
    clippy::cast_possible_wrap,
    reason = "day is 1..=31, always fits in i32"
)]
#[no_mangle]
pub unsafe extern "C" fn hew_datetime_day(epoch_ms: i64) -> i32 {
    epoch_ms_to_utc(epoch_ms).map_or(0, |dt| dt.day() as i32)
}

/// Extract the hour (0–23) from epoch milliseconds. Returns -1 if out of range.
///
/// # Safety
///
/// No preconditions — pure computation.
#[expect(
    clippy::cast_possible_wrap,
    reason = "hour is 0..=23, always fits in i32"
)]
#[no_mangle]
pub unsafe extern "C" fn hew_datetime_hour(epoch_ms: i64) -> i32 {
    epoch_ms_to_utc(epoch_ms).map_or(-1, |dt| dt.hour() as i32)
}

/// Extract the minute (0–59) from epoch milliseconds. Returns -1 if out of range.
///
/// # Safety
///
/// No preconditions — pure computation.
#[expect(
    clippy::cast_possible_wrap,
    reason = "minute is 0..=59, always fits in i32"
)]
#[no_mangle]
pub unsafe extern "C" fn hew_datetime_minute(epoch_ms: i64) -> i32 {
    epoch_ms_to_utc(epoch_ms).map_or(-1, |dt| dt.minute() as i32)
}

/// Extract the second (0–59) from epoch milliseconds. Returns -1 if out of range.
///
/// # Safety
///
/// No preconditions — pure computation.
#[expect(
    clippy::cast_possible_wrap,
    reason = "second is 0..=59, always fits in i32"
)]
#[no_mangle]
pub unsafe extern "C" fn hew_datetime_second(epoch_ms: i64) -> i32 {
    epoch_ms_to_utc(epoch_ms).map_or(-1, |dt| dt.second() as i32)
}

/// Return the day of the week (0=Mon, 6=Sun) from epoch milliseconds.
/// Returns -1 if out of range.
///
/// # Safety
///
/// No preconditions — pure computation.
#[no_mangle]
pub unsafe extern "C" fn hew_datetime_weekday(epoch_ms: i64) -> i32 {
    epoch_ms_to_utc(epoch_ms).map_or(-1, |dt| match dt.weekday() {
        Weekday::Mon => 0,
        Weekday::Tue => 1,
        Weekday::Wed => 2,
        Weekday::Thu => 3,
        Weekday::Fri => 4,
        Weekday::Sat => 5,
        Weekday::Sun => 6,
    })
}

// ---------------------------------------------------------------------------
// Arithmetic
// ---------------------------------------------------------------------------

/// Add `days` to epoch milliseconds. Returns the new epoch ms.
///
/// # Safety
///
/// No preconditions — pure computation.
#[no_mangle]
pub unsafe extern "C" fn hew_datetime_add_days(epoch_ms: i64, days: i32) -> i64 {
    epoch_ms_to_utc(epoch_ms).map_or(epoch_ms, |dt| {
        (dt + Duration::days(i64::from(days))).timestamp_millis()
    })
}

/// Add `hours` to epoch milliseconds. Returns the new epoch ms.
///
/// # Safety
///
/// No preconditions — pure computation.
#[no_mangle]
pub unsafe extern "C" fn hew_datetime_add_hours(epoch_ms: i64, hours: i32) -> i64 {
    epoch_ms_to_utc(epoch_ms).map_or(epoch_ms, |dt| {
        (dt + Duration::hours(i64::from(hours))).timestamp_millis()
    })
}

/// Return the difference `a - b` in seconds.
///
/// # Safety
///
/// No preconditions — pure arithmetic.
#[no_mangle]
pub unsafe extern "C" fn hew_datetime_diff_secs(a: i64, b: i64) -> i64 {
    (a - b) / 1000
}

/// Format epoch milliseconds as an ISO 8601 string (e.g. `2024-01-15T09:30:00Z`).
///
/// Returns a `malloc`-allocated, NUL-terminated C string. The caller must
/// free it with `libc::free`. Returns null if `epoch_ms` is out of range.
///
/// # Safety
///
/// No preconditions. The returned pointer must be freed with `libc::free`.
#[no_mangle]
pub unsafe extern "C" fn hew_datetime_to_iso8601(epoch_ms: i64) -> *mut c_char {
    let Some(dt) = epoch_ms_to_utc(epoch_ms) else {
        return std::ptr::null_mut();
    };
    let s = dt.to_rfc3339_opts(chrono::SecondsFormat::Secs, true);
    // SAFETY: s.as_ptr() is valid for s.len() bytes.
    unsafe { malloc_cstring(s.as_ptr(), s.len()) }
}

/// Return the current monotonic clock time in nanoseconds.
///
/// Uses `CLOCK_MONOTONIC` for high-resolution timing suitable for
/// benchmarking. Not affected by wall-clock adjustments.
///
/// # Safety
///
/// No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_datetime_now_nanos() -> i64 {
    use std::sync::OnceLock;
    use std::time::Instant;
    static EPOCH: OnceLock<Instant> = OnceLock::new();
    let epoch = EPOCH.get_or_init(Instant::now);
    epoch.elapsed().as_nanos() as i64
}

#[cfg(test)]
extern crate hew_runtime; // Link hew_vec_* symbol implementations
mod tests {
    use super::*;
    use std::ffi::CString;

    /// Helper to read a malloc'd C string and free it.
    ///
    /// # Safety
    ///
    /// `ptr` must be a non-null, NUL-terminated, malloc-allocated C string.
    unsafe fn read_and_free(ptr: *mut c_char) -> String {
        assert!(!ptr.is_null());
        // SAFETY: ptr is a valid NUL-terminated C string per caller.
        let s = unsafe { CStr::from_ptr(ptr) }.to_str().unwrap().to_owned();
        // SAFETY: ptr was allocated with libc::malloc.
        unsafe { libc::free(ptr.cast()) };
        s
    }

    #[test]
    fn test_format_parse_roundtrip() {
        // 2024-01-15 09:30:00 UTC
        let epoch_ms: i64 = 1_705_311_000_000;
        let fmt = CString::new("%Y-%m-%d %H:%M:%S").unwrap();

        // SAFETY: fmt.as_ptr() is a valid NUL-terminated C string.
        let formatted = unsafe { hew_datetime_format(epoch_ms, fmt.as_ptr()) };
        // SAFETY: formatted was returned by hew_datetime_format.
        let text = unsafe { read_and_free(formatted) };
        assert_eq!(text, "2024-01-15 09:30:00");

        let input = CString::new(text).unwrap();
        // SAFETY: both pointers are valid NUL-terminated C strings.
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
    fn test_add_days_and_hours() {
        let epoch_ms: i64 = 1_705_311_000_000; // 2024-01-15 09:30:00 UTC

        // SAFETY: arithmetic functions have no preconditions.
        unsafe {
            let plus_one_day = hew_datetime_add_days(epoch_ms, 1);
            assert_eq!(plus_one_day, epoch_ms + 86_400_000);

            let minus_two_hours = hew_datetime_add_hours(epoch_ms, -2);
            assert_eq!(minus_two_hours, epoch_ms - 7_200_000);
        }
    }

    #[test]
    fn test_diff_secs_and_iso8601() {
        let a: i64 = 1_705_311_000_000; // 2024-01-15 09:30:00 UTC
        let b: i64 = 1_705_307_400_000; // 2024-01-15 08:30:00 UTC

        // SAFETY: diff_secs has no preconditions.
        assert_eq!(unsafe { hew_datetime_diff_secs(a, b) }, 3600);

        // SAFETY: hew_datetime_to_iso8601 has no preconditions.
        let iso = unsafe { hew_datetime_to_iso8601(a) };
        // SAFETY: iso was returned by hew_datetime_to_iso8601.
        let text = unsafe { read_and_free(iso) };
        assert_eq!(text, "2024-01-15T09:30:00Z");
    }

    #[test]
    fn test_now_returns_reasonable_values() {
        // SAFETY: now functions have no preconditions.
        unsafe {
            let ms = hew_datetime_now_ms();
            let secs = hew_datetime_now_secs();
            // Should be after 2024-01-01 and before 2100-01-01.
            assert!(ms > 1_704_067_200_000);
            assert!(secs > 1_704_067_200);
            // ms and secs should be consistent (within 1 second).
            assert!((ms / 1000 - secs).abs() <= 1);
        }
    }

    #[test]
    fn test_parse_error_returns_negative_one() {
        let bad_input = CString::new("not-a-date").unwrap();
        let fmt = CString::new("%Y-%m-%d").unwrap();
        // SAFETY: both pointers are valid NUL-terminated C strings.
        let result = unsafe { hew_datetime_parse(bad_input.as_ptr(), fmt.as_ptr()) };
        assert_eq!(result, -1);
    }
}
