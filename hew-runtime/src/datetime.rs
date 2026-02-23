//! Hew runtime: `datetime` module.
//!
//! Provides date/time operations for the `std::time::datetime` standard library module.
//! All timestamps are milliseconds since Unix epoch unless noted otherwise.

use core::ffi::c_char;
use core::ptr;

// ---------------------------------------------------------------------------
// Clock functions
// ---------------------------------------------------------------------------

/// Return the current time as milliseconds since Unix epoch (wall clock).
///
/// # Safety
///
/// No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_datetime_now_ms() -> i64 {
    let mut ts = libc::timespec {
        tv_sec: 0,
        tv_nsec: 0,
    };
    unsafe {
        libc::clock_gettime(libc::CLOCK_REALTIME, &raw mut ts);
    }
    ts.tv_sec * 1000 + ts.tv_nsec / 1_000_000
}

/// Return the current time as seconds since Unix epoch.
///
/// # Safety
///
/// No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_datetime_now_secs() -> i64 {
    let mut ts = libc::timespec {
        tv_sec: 0,
        tv_nsec: 0,
    };
    unsafe {
        libc::clock_gettime(libc::CLOCK_REALTIME, &raw mut ts);
    }
    ts.tv_sec
}

/// Return the current time as nanoseconds since Unix epoch.
///
/// # Safety
///
/// No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_datetime_now_nanos() -> i64 {
    let mut ts = libc::timespec {
        tv_sec: 0,
        tv_nsec: 0,
    };
    unsafe {
        libc::clock_gettime(libc::CLOCK_REALTIME, &raw mut ts);
    }
    ts.tv_sec * 1_000_000_000 + ts.tv_nsec
}

// ---------------------------------------------------------------------------
// Date component extraction (UTC)
// ---------------------------------------------------------------------------

/// Seconds per day.
const SECS_PER_DAY: i64 = 86400;

/// Convert epoch milliseconds to (year, month 1-12, day 1-31, hour, minute, second).
fn epoch_ms_to_components(epoch_ms: i64) -> (i32, i32, i32, i32, i32, i32) {
    let total_secs = epoch_ms / 1000;
    let day_secs = total_secs.rem_euclid(SECS_PER_DAY);
    let hour = (day_secs / 3600) as i32;
    let minute = ((day_secs % 3600) / 60) as i32;
    let second = (day_secs % 60) as i32;

    // Days since epoch (1970-01-01)
    let mut days = (total_secs - day_secs) / SECS_PER_DAY;
    if total_secs < 0 && day_secs != 0 {
        days -= 1;
    }

    // Civil date from day count (algorithm from Howard Hinnant)
    days += 719468; // shift to 0000-03-01
    let era = if days >= 0 { days } else { days - 146096 } / 146097;
    let doe = (days - era * 146097) as u32;
    let yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365;
    let y = (yoe as i64) + era * 400;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let d = doy - (153 * mp + 2) / 5 + 1;
    let m = if mp < 10 { mp + 3 } else { mp - 9 };
    let year = if m <= 2 { y + 1 } else { y };

    (year as i32, m as i32, d as i32, hour, minute, second)
}

/// Extract the year from an epoch-millisecond timestamp.
#[no_mangle]
pub extern "C" fn hew_datetime_year(epoch_ms: i64) -> i32 {
    epoch_ms_to_components(epoch_ms).0
}

/// Extract the month (1-12) from an epoch-millisecond timestamp.
#[no_mangle]
pub extern "C" fn hew_datetime_month(epoch_ms: i64) -> i32 {
    epoch_ms_to_components(epoch_ms).1
}

/// Extract the day of month (1-31) from an epoch-millisecond timestamp.
#[no_mangle]
pub extern "C" fn hew_datetime_day(epoch_ms: i64) -> i32 {
    epoch_ms_to_components(epoch_ms).2
}

/// Extract the hour (0-23) from an epoch-millisecond timestamp.
#[no_mangle]
pub extern "C" fn hew_datetime_hour(epoch_ms: i64) -> i32 {
    epoch_ms_to_components(epoch_ms).3
}

/// Extract the minute (0-59) from an epoch-millisecond timestamp.
#[no_mangle]
pub extern "C" fn hew_datetime_minute(epoch_ms: i64) -> i32 {
    epoch_ms_to_components(epoch_ms).4
}

/// Extract the second (0-59) from an epoch-millisecond timestamp.
#[no_mangle]
pub extern "C" fn hew_datetime_second(epoch_ms: i64) -> i32 {
    epoch_ms_to_components(epoch_ms).5
}

/// Return the weekday (0=Sunday, 6=Saturday) for an epoch-millisecond timestamp.
#[no_mangle]
pub extern "C" fn hew_datetime_weekday(epoch_ms: i64) -> i32 {
    let days = epoch_ms / 1000 / SECS_PER_DAY;
    // 1970-01-01 was Thursday (4). Adjust.
    ((days + 4).rem_euclid(7)) as i32
}

// ---------------------------------------------------------------------------
// Date arithmetic
// ---------------------------------------------------------------------------

/// Add `days` to an epoch-millisecond timestamp.
#[no_mangle]
pub extern "C" fn hew_datetime_add_days(epoch_ms: i64, days: i32) -> i64 {
    epoch_ms + (days as i64) * SECS_PER_DAY * 1000
}

/// Add `hours` to an epoch-millisecond timestamp.
#[no_mangle]
pub extern "C" fn hew_datetime_add_hours(epoch_ms: i64, hours: i32) -> i64 {
    epoch_ms + (hours as i64) * 3600 * 1000
}

/// Return difference in seconds between two epoch-millisecond timestamps.
#[no_mangle]
pub extern "C" fn hew_datetime_diff_secs(a: i64, b: i64) -> i64 {
    (a - b) / 1000
}

// ---------------------------------------------------------------------------
// Formatting / Parsing
// ---------------------------------------------------------------------------

/// Format an epoch-millisecond timestamp as an ISO 8601 string.
///
/// # Safety
///
/// Returned string must be freed by the caller via `hew_string_free`.
#[no_mangle]
pub extern "C" fn hew_datetime_to_iso8601(epoch_ms: i64) -> *mut c_char {
    let (y, m, d, h, min, s) = epoch_ms_to_components(epoch_ms);
    let ms = (epoch_ms % 1000).unsigned_abs();
    let formatted = format!("{y:04}-{m:02}-{d:02}T{h:02}:{min:02}:{s:02}.{ms:03}Z");
    match std::ffi::CString::new(formatted) {
        Ok(cs) => cs.into_raw(),
        Err(_) => ptr::null_mut(),
    }
}

/// Format an epoch-millisecond timestamp using a strftime-style format string.
///
/// # Safety
///
/// `fmt` must be a valid C string. Returned string must be freed by the caller.
#[no_mangle]
pub unsafe extern "C" fn hew_datetime_format(epoch_ms: i64, fmt: *const c_char) -> *mut c_char {
    if fmt.is_null() {
        return ptr::null_mut();
    }
    let fmt_str = unsafe { std::ffi::CStr::from_ptr(fmt) };
    let Ok(fmt_str) = fmt_str.to_str() else {
        return ptr::null_mut();
    };

    let (y, m, d, h, min, s) = epoch_ms_to_components(epoch_ms);
    // Simple strftime-like substitutions
    let result = fmt_str
        .replace("%Y", &format!("{y:04}"))
        .replace("%m", &format!("{m:02}"))
        .replace("%d", &format!("{d:02}"))
        .replace("%H", &format!("{h:02}"))
        .replace("%M", &format!("{min:02}"))
        .replace("%S", &format!("{s:02}"));

    match std::ffi::CString::new(result) {
        Ok(cs) => cs.into_raw(),
        Err(_) => ptr::null_mut(),
    }
}

/// Parse a date string using a strftime-style format and return epoch milliseconds.
/// Returns 0 on parse failure.
///
/// # Safety
///
/// `s` and `fmt` must be valid C strings.
#[no_mangle]
pub unsafe extern "C" fn hew_datetime_parse(s: *const c_char, _fmt: *const c_char) -> i64 {
    if s.is_null() {
        return 0;
    }
    let s_str = unsafe { std::ffi::CStr::from_ptr(s) };
    let Ok(s_str) = s_str.to_str() else {
        return 0;
    };
    // Try ISO 8601 parsing: YYYY-MM-DDTHH:MM:SS
    if s_str.len() >= 19 {
        let parts: Vec<&str> = s_str.split('T').collect();
        if parts.len() == 2 {
            let date_parts: Vec<&str> = parts[0].split('-').collect();
            let time_str = parts[1].trim_end_matches('Z');
            let time_parts: Vec<&str> = time_str.split(':').collect();
            if date_parts.len() == 3 && time_parts.len() >= 3 {
                let y: i32 = date_parts[0].parse().unwrap_or(1970);
                let m: i32 = date_parts[1].parse().unwrap_or(1);
                let d: i32 = date_parts[2].parse().unwrap_or(1);
                let h: i32 = time_parts[0].parse().unwrap_or(0);
                let min: i32 = time_parts[1].parse().unwrap_or(0);
                let sec_str = time_parts[2].split('.').next().unwrap_or("0");
                let sec: i32 = sec_str.parse().unwrap_or(0);
                return components_to_epoch_ms(y, m, d, h, min, sec);
            }
        }
    }
    0
}

/// Convert date components to epoch milliseconds (UTC).
fn components_to_epoch_ms(y: i32, m: i32, d: i32, h: i32, min: i32, s: i32) -> i64 {
    // Inverse of epoch_ms_to_components using the same civil calendar algorithm
    let y = y as i64;
    let (y_adj, m_adj) = if m <= 2 {
        (y - 1, (m + 9) as u32)
    } else {
        (y, (m - 3) as u32)
    };
    let era = if y_adj >= 0 { y_adj } else { y_adj - 399 } / 400;
    let yoe = (y_adj - era * 400) as u32;
    let doy = (153 * m_adj + 2) / 5 + (d as u32) - 1;
    let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    let days = era * 146097 + (doe as i64) - 719468;
    (days * SECS_PER_DAY + (h as i64) * 3600 + (min as i64) * 60 + (s as i64)) * 1000
}
