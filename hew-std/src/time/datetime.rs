//! Native bindings for the shared, checked UTC calendar engine.
//!
//! Every returned string field transfers one managed owner to the caller.
//! Empty fields use the canonical null string; errors travel with their result.

use hew_cabi::string::{string_as_str, string_from_str, HewString};
use hew_calendar::Error;

#[repr(C)]
#[derive(Debug)]
pub struct HewCalendarTextResult {
    pub status: i32,
    pub text: *mut HewString,
    pub message: *mut HewString,
}

#[repr(C)]
#[derive(Debug)]
pub struct HewCalendarTimeResult {
    pub status: i32,
    pub seconds: i64,
    pub nanoseconds: u32,
    pub message: *mut HewString,
}

#[repr(C)]
#[derive(Debug, Default)]
pub struct HewCalendarPartsResult {
    pub status: i32,
    pub year: i64,
    pub month: i64,
    pub day: i64,
    pub hour: i64,
    pub minute: i64,
    pub second: i64,
    pub nanosecond: u32,
    pub weekday: i64,
    pub days_in_month: i64,
    pub message: *mut HewString,
}

fn error_fields(error: &Error) -> (i32, *mut HewString) {
    (error.code(), string_from_str(&error.to_string()))
}

fn time_result(result: Result<(i64, u32), Error>) -> HewCalendarTimeResult {
    match result {
        Ok((seconds, nanoseconds)) => HewCalendarTimeResult {
            status: 0,
            seconds,
            nanoseconds,
            message: std::ptr::null_mut(),
        },
        Err(error) => {
            let (status, message) = error_fields(&error);
            HewCalendarTimeResult {
                status,
                seconds: 0,
                nanoseconds: 0,
                message,
            }
        }
    }
}

/// Format normalized Unix parts and return the text or checked error directly.
///
/// # Safety
/// `pattern` is a live borrowed managed string, or canonical empty.
#[no_mangle]
pub unsafe extern "C" fn hew_calendar_format(
    seconds: i64,
    nanoseconds: u32,
    pattern: *const HewString,
) -> HewCalendarTextResult {
    // SAFETY: the caller supplies a borrowed managed string.
    match hew_calendar::format(seconds, nanoseconds, unsafe { string_as_str(pattern) }) {
        Ok(text) => HewCalendarTextResult {
            status: 0,
            text: string_from_str(&text),
            message: std::ptr::null_mut(),
        },
        Err(error) => {
            let (status, message) = error_fields(&error);
            HewCalendarTextResult {
                status,
                text: std::ptr::null_mut(),
                message,
            }
        }
    }
}

/// Parse text into normalized Unix parts with its checked error.
///
/// # Safety
/// Both input handles are live borrowed managed strings, or canonical empty.
#[no_mangle]
pub unsafe extern "C" fn hew_calendar_parse(
    text: *const HewString,
    pattern: *const HewString,
) -> HewCalendarTimeResult {
    // SAFETY: the caller supplies borrowed managed strings.
    time_result(hew_calendar::parse(
        unsafe { string_as_str(text) },
        unsafe { string_as_str(pattern) },
    ))
}

/// Extract UTC components, returning a checked error outside the calendar.
#[no_mangle]
pub extern "C" fn hew_calendar_components(
    seconds: i64,
    nanoseconds: u32,
) -> HewCalendarPartsResult {
    match hew_calendar::components(seconds, nanoseconds) {
        Ok(value) => HewCalendarPartsResult {
            status: 0,
            year: value.year,
            month: i64::from(value.month),
            day: i64::from(value.day),
            hour: i64::from(value.hour),
            minute: i64::from(value.minute),
            second: i64::from(value.second),
            nanosecond: value.nanosecond,
            weekday: i64::from(value.weekday),
            days_in_month: i64::from(value.days_in_month),
            message: std::ptr::null_mut(),
        },
        Err(error) => {
            let (status, message) = error_fields(&error);
            HewCalendarPartsResult {
                status,
                message,
                ..HewCalendarPartsResult::default()
            }
        }
    }
}

/// Construct a UTC timestamp, checking every civil component.
#[no_mangle]
pub extern "C" fn hew_calendar_from_parts(
    year: i64,
    month: i64,
    day: i64,
    hour: i64,
    minute: i64,
    second: i64,
    nanosecond: u32,
) -> HewCalendarTimeResult {
    time_result(hew_calendar::from_parts(
        year, month, day, hour, minute, second, nanosecond,
    ))
}
