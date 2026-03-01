//! Hew runtime: date/time formatting, parsing, and arithmetic.
//!
//! Provides datetime utilities for compiled Hew programs using Unix epoch
//! milliseconds as the canonical time representation. All returned strings
//! are allocated with `libc::malloc` so callers can free them with `libc::free`.

// Force-link hew-runtime so the linker can resolve hew_vec_* symbols
// referenced by hew-cabi's object code.
#[cfg(test)]
extern crate hew_runtime;

use hew_cabi::cabi::{cstr_to_str, str_to_malloc};
use std::ffi::c_char;

use chrono::{DateTime, Datelike, NaiveDateTime, Timelike, Utc, Weekday};

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
    // SAFETY: caller guarantees fmt is a valid NUL-terminated C string.
    let Some(fmt_str) = (unsafe { cstr_to_str(fmt) }) else {
        return std::ptr::null_mut();
    };
    let Some(dt) = epoch_ms_to_utc(epoch_ms) else {
        return std::ptr::null_mut();
    };
    let formatted = dt.format(fmt_str).to_string();
    str_to_malloc(&formatted)
}

/// Parse a datetime string with the given `strftime` format, returning epoch
/// milliseconds. Returns -1 on parse error.
///
/// # Safety
///
/// Both `s` and `fmt` must be valid NUL-terminated C strings.
#[no_mangle]
pub unsafe extern "C" fn hew_datetime_parse(s: *const c_char, fmt: *const c_char) -> i64 {
    // SAFETY: caller guarantees both pointers are valid NUL-terminated C strings.
    let Some(s_str) = (unsafe { cstr_to_str(s) }) else {
        return -1;
    };
    let Some(fmt_str) = (unsafe { cstr_to_str(fmt) }) else {
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

/// Extract the year from epoch milliseconds. Returns -1 if out of range.
///
/// # Safety
///
/// No preconditions — pure computation.
#[no_mangle]
pub unsafe extern "C" fn hew_datetime_year(epoch_ms: i64) -> i32 {
    epoch_ms_to_utc(epoch_ms).map_or(-1, |dt| dt.year())
}

/// Extract the month (1–12) from epoch milliseconds. Returns -1 if out of range.
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
    epoch_ms_to_utc(epoch_ms).map_or(-1, |dt| dt.month() as i32)
}

/// Extract the day (1–31) from epoch milliseconds. Returns -1 if out of range.
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
    epoch_ms_to_utc(epoch_ms).map_or(-1, |dt| dt.day() as i32)
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
    #[expect(
        clippy::cast_possible_truncation,
        reason = "monotonic ns since process start won't exceed i64"
    )]
    {
        epoch.elapsed().as_nanos() as i64
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::{CStr, CString};

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
        let bad_input = CString::new("not-a-date").unwrap();
        let fmt = CString::new("%Y-%m-%d").unwrap();
        // SAFETY: both pointers are valid NUL-terminated C strings.
        let result = unsafe { hew_datetime_parse(bad_input.as_ptr(), fmt.as_ptr()) };
        assert_eq!(result, -1);
    }
}
