//! Hew runtime: cron expression parsing and scheduling.
//!
//! Provides cron expression parsing and next-occurrence calculation for
//! compiled Hew programs. The opaque [`HewCronExpr`] handle wraps a
//! [`cron::Schedule`] and must be freed with [`hew_cron_free`]. All returned
//! strings are allocated with `libc::malloc` so callers can free them with
//! [`hew_cron_free_string`] or `libc::free`.

// Force-link hew-runtime so the linker can resolve hew_vec_* symbols
// referenced by hew-cabi's object code.
#[cfg(test)]
extern crate hew_runtime;

use hew_cabi::cabi::{cstr_to_str, str_to_malloc};
use std::cell::RefCell;
use std::ffi::c_char;
use std::str::FromStr;

use chrono::Utc;
use cron::Schedule;

std::thread_local! {
    static LAST_CRON_ERROR: RefCell<Option<String>> = const { RefCell::new(None) };
}

fn set_cron_last_error(msg: impl Into<String>) {
    LAST_CRON_ERROR.with(|error| *error.borrow_mut() = Some(msg.into()));
}

fn clear_cron_last_error() {
    LAST_CRON_ERROR.with(|error| *error.borrow_mut() = None);
}

fn clone_cron_last_error() -> Option<String> {
    LAST_CRON_ERROR.with(|error| error.borrow().clone())
}

fn ensure_cron_last_error(msg: impl Into<String>) {
    let msg = msg.into();
    LAST_CRON_ERROR.with(|error| {
        if error.borrow().is_none() {
            *error.borrow_mut() = Some(msg);
        }
    });
}

/// Opaque handle wrapping a compiled [`cron::Schedule`].
///
/// Created by [`hew_cron_parse`], freed by [`hew_cron_free`].
#[derive(Debug)]
pub struct HewCronExpr {
    inner: Schedule,
}

// ---------------------------------------------------------------------------
// C ABI exports
// ---------------------------------------------------------------------------

/// Parse a cron expression string into a [`HewCronExpr`].
///
/// Accepts standard 7-field cron expressions (second minute hour day-of-month
/// month day-of-week year). Returns null on parse error or invalid input.
///
/// # Safety
///
/// `expr` must be a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_cron_parse(expr: *const c_char) -> *mut HewCronExpr {
    // SAFETY: caller guarantees expr is a valid NUL-terminated C string.
    let Some(s) = (unsafe { cstr_to_str(expr) }) else {
        set_cron_last_error("invalid cron expression: null pointer or invalid UTF-8");
        return std::ptr::null_mut();
    };
    match Schedule::from_str(s) {
        Ok(schedule) => {
            clear_cron_last_error();
            Box::into_raw(Box::new(HewCronExpr { inner: schedule }))
        }
        Err(err) => {
            set_cron_last_error(format!("cron parse error: {err}"));
            std::ptr::null_mut()
        }
    }
}

/// Return the next occurrence after the given epoch timestamp (seconds).
///
/// Returns the epoch timestamp (seconds) of the next matching time, or `-1`
/// if no future occurrence can be computed. Call [`hew_cron_last_error`] to
/// distinguish an error from a valid pre-epoch `-1` timestamp.
///
/// # Safety
///
/// `expr` must be a valid pointer returned by [`hew_cron_parse`].
#[no_mangle]
pub unsafe extern "C" fn hew_cron_next(expr: *const HewCronExpr, after_epoch_secs: i64) -> i64 {
    if expr.is_null() {
        ensure_cron_last_error("invalid cron expression handle: null pointer");
        return -1;
    }
    // SAFETY: expr is a valid HewCronExpr pointer per caller contract.
    let cron_expr = unsafe { &*expr };
    let Some(dt) = chrono::DateTime::<Utc>::from_timestamp(after_epoch_secs, 0) else {
        set_cron_last_error(format!("invalid epoch timestamp: {after_epoch_secs}"));
        return -1;
    };
    if let Some(next_dt) = cron_expr.inner.after(&dt).next() {
        clear_cron_last_error();
        next_dt.timestamp()
    } else {
        set_cron_last_error("cron schedule has no next occurrence after the given timestamp");
        -1
    }
}

/// Write up to `count` next occurrences after the given epoch timestamp into
/// the `out` array.
///
/// Returns the number of timestamps actually written (may be less than
/// `count` if the schedule has fewer future occurrences). When this function
/// returns `0`, call [`hew_cron_last_error`] to distinguish an error from a
/// schedule with no remaining matches.
///
/// # Safety
///
/// - `expr` must be a valid pointer returned by [`hew_cron_parse`].
/// - `out` must point to a writable array of at least `count` `i64` elements.
#[no_mangle]
pub unsafe extern "C" fn hew_cron_next_n(
    expr: *const HewCronExpr,
    after_epoch_secs: i64,
    count: i32,
    out: *mut i64,
) -> i32 {
    if expr.is_null() {
        ensure_cron_last_error("invalid cron expression handle: null pointer");
        return 0;
    }
    if out.is_null() {
        set_cron_last_error("invalid output buffer: null pointer");
        return 0;
    }
    if count <= 0 {
        set_cron_last_error(format!("invalid occurrence count: {count}"));
        return 0;
    }
    // SAFETY: expr is a valid HewCronExpr pointer per caller contract.
    let cron_expr = unsafe { &*expr };
    let Some(dt) = chrono::DateTime::<Utc>::from_timestamp(after_epoch_secs, 0) else {
        set_cron_last_error(format!("invalid epoch timestamp: {after_epoch_secs}"));
        return 0;
    };
    #[expect(clippy::cast_sign_loss, reason = "C ABI: negative count checked above")]
    let max = count as usize;
    let mut written = 0usize;
    for next_dt in cron_expr.inner.after(&dt).take(max) {
        // SAFETY: out has space for at least `count` elements; written < max <= count.
        unsafe { *out.add(written) = next_dt.timestamp() };
        written += 1;
    }
    #[expect(
        clippy::cast_possible_truncation,
        clippy::cast_possible_wrap,
        reason = "C ABI: written <= count which fits in i32"
    )]
    {
        let written = written as i32;
        clear_cron_last_error();
        written
    }
}

/// Return the last cron error recorded on the current thread.
///
/// Returns a `malloc`-allocated, NUL-terminated C string. The caller must free
/// it with [`hew_cron_free_string`] or `libc::free`. Returns null when no cron
/// error has been recorded.
#[no_mangle]
pub extern "C" fn hew_cron_last_error() -> *mut c_char {
    match clone_cron_last_error() {
        Some(message) => str_to_malloc(&message),
        None => std::ptr::null_mut(),
    }
}

/// Free a malloc-allocated string returned by cron APIs.
///
/// # Safety
///
/// `s` must be a pointer previously returned by a cron API that documents
/// malloc-backed string ownership, and must not have been freed already.
#[no_mangle]
pub unsafe extern "C" fn hew_cron_free_string(s: *mut c_char) {
    if s.is_null() {
        return;
    }
    // SAFETY: `s` was allocated with libc::malloc by a cron API in this module.
    unsafe { libc::free(s.cast()) };
}

/// Return the string representation of a cron expression.
///
/// Returns a `malloc`-allocated, NUL-terminated C string. The caller must
/// free it with [`hew_cron_free_string`] or `libc::free`. Returns null on
/// error.
///
/// # Safety
///
/// `expr` must be a valid pointer returned by [`hew_cron_parse`].
#[no_mangle]
pub unsafe extern "C" fn hew_cron_to_string(expr: *const HewCronExpr) -> *mut c_char {
    if expr.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: expr is a valid HewCronExpr pointer per caller contract.
    let cron_expr = unsafe { &*expr };
    let s = cron_expr.inner.to_string();
    str_to_malloc(&s)
}

/// Free a [`HewCronExpr`] previously returned by [`hew_cron_parse`].
///
/// # Safety
///
/// `expr` must be a pointer previously returned by [`hew_cron_parse`], and
/// must not have been freed already.
#[no_mangle]
pub unsafe extern "C" fn hew_cron_free(expr: *mut HewCronExpr) {
    if expr.is_null() {
        return;
    }
    // SAFETY: expr was allocated with Box::into_raw in hew_cron_parse.
    drop(unsafe { Box::from_raw(expr) });
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::{CStr, CString};

    unsafe fn read_and_free_optional(s: *mut c_char) -> Option<String> {
        if s.is_null() {
            return None;
        }
        // SAFETY: `s` is a valid NUL-terminated C string allocated with malloc.
        let text = unsafe { CStr::from_ptr(s) }
            .to_str()
            .expect("test error string should be valid UTF-8")
            .to_owned();
        // SAFETY: `s` was allocated with libc::malloc.
        unsafe { libc::free(s.cast()) };
        Some(text)
    }

    #[test]
    fn parse_valid_expression() {
        let expr_str = CString::new("0 30 9 * * Mon-Fri *").unwrap();
        // SAFETY: expr_str is a valid NUL-terminated C string.
        let expr = unsafe { hew_cron_parse(expr_str.as_ptr()) };
        assert!(!expr.is_null());
        // SAFETY: expr was returned by hew_cron_parse.
        unsafe { hew_cron_free(expr) };
    }

    #[test]
    fn parse_invalid_expression() {
        let bad = CString::new("not a cron expression").unwrap();
        // SAFETY: bad is a valid NUL-terminated C string.
        let expr = unsafe { hew_cron_parse(bad.as_ptr()) };
        assert!(expr.is_null());
        // SAFETY: hew_cron_last_error returns either null or a malloc-allocated C string.
        let err = unsafe { read_and_free_optional(hew_cron_last_error()) };
        assert!(err.is_some());
        assert!(err
            .as_deref()
            .is_some_and(|message| message.contains("parse") || message.contains("cron")));

        // Null input.
        // SAFETY: testing null pointer handling.
        assert!(unsafe { hew_cron_parse(std::ptr::null()) }.is_null());
    }

    #[test]
    fn invalid_expression_then_next_preserves_parse_error() {
        let bad = CString::new("not a cron expression").unwrap();
        // SAFETY: bad is a valid NUL-terminated C string.
        let expr = unsafe { hew_cron_parse(bad.as_ptr()) };
        assert!(expr.is_null());

        // SAFETY: null expr should be rejected without erasing the parse error.
        let next = unsafe { hew_cron_next(expr, 0) };
        assert_eq!(next, -1);
        // SAFETY: hew_cron_last_error returns either null or a malloc-allocated C string.
        let err = unsafe { read_and_free_optional(hew_cron_last_error()) };
        assert!(err
            .as_deref()
            .is_some_and(|message| message.contains("parse") || message.contains("cron")));
    }

    #[test]
    fn next_occurrence() {
        // Every minute: "0 * * * * * *"
        let expr_str = CString::new("0 * * * * * *").unwrap();
        // SAFETY: expr_str is a valid NUL-terminated C string.
        let expr = unsafe { hew_cron_parse(expr_str.as_ptr()) };
        assert!(!expr.is_null());

        // 2024-01-01 00:00:00 UTC = 1_704_067_200
        let after = 1_704_067_200_i64;
        // SAFETY: expr is valid.
        let next = unsafe { hew_cron_next(expr, after) };
        assert!(next > after, "next ({next}) should be after {after}");
        // Should be exactly one minute later.
        assert_eq!(next, after + 60);
        // SAFETY: hew_cron_last_error returns either null or a malloc-allocated C string.
        assert!(unsafe { read_and_free_optional(hew_cron_last_error()) }.is_none());

        // SAFETY: expr was returned by hew_cron_parse.
        unsafe { hew_cron_free(expr) };
    }

    #[test]
    fn missing_next_occurrence_sets_last_error() {
        let expr_str = CString::new("0 0 0 1 1 * 2024").unwrap();
        // SAFETY: expr_str is a valid NUL-terminated C string.
        let expr = unsafe { hew_cron_parse(expr_str.as_ptr()) };
        assert!(!expr.is_null());

        // SAFETY: expr is valid.
        let next = unsafe { hew_cron_next(expr, 1_735_689_600) };
        assert_eq!(next, -1);
        // SAFETY: hew_cron_last_error returns either null or a malloc-allocated C string.
        assert!(unsafe { read_and_free_optional(hew_cron_last_error()) }
            .is_some_and(|message| message.contains("no next occurrence")));

        // SAFETY: expr was returned by hew_cron_parse.
        unsafe { hew_cron_free(expr) };
    }

    #[test]
    fn next_n_occurrences() {
        // Every minute.
        let expr_str = CString::new("0 * * * * * *").unwrap();
        // SAFETY: expr_str is a valid NUL-terminated C string.
        let expr = unsafe { hew_cron_parse(expr_str.as_ptr()) };
        assert!(!expr.is_null());

        let after = 1_704_067_200_i64;
        let mut out = [0_i64; 5];
        // SAFETY: expr is valid, out has 5 elements.
        let written = unsafe { hew_cron_next_n(expr, after, 5, out.as_mut_ptr()) };
        assert_eq!(written, 5);

        // Each should be 60 seconds apart.
        for (idx, actual) in out.iter().enumerate() {
            #[expect(clippy::cast_possible_wrap, reason = "test: idx fits in i64")]
            let expected = after + 60 * (idx as i64 + 1);
            assert_eq!(*actual, expected, "occurrence {idx} mismatch");
        }

        // SAFETY: expr was returned by hew_cron_parse.
        unsafe { hew_cron_free(expr) };
    }

    #[test]
    fn to_string_roundtrip() {
        let original = "0 30 9 * * Mon-Fri *";
        let expr_str = CString::new(original).unwrap();
        // SAFETY: expr_str is a valid NUL-terminated C string.
        let expr = unsafe { hew_cron_parse(expr_str.as_ptr()) };
        assert!(!expr.is_null());

        // SAFETY: expr is valid.
        let s = unsafe { hew_cron_to_string(expr) };
        assert!(!s.is_null());
        // SAFETY: s is a valid NUL-terminated C string from malloc.
        let result = unsafe { CStr::from_ptr(s) }.to_str().unwrap();
        assert!(!result.is_empty());
        // SAFETY: s was allocated with libc::malloc.
        unsafe { libc::free(s.cast()) };

        // SAFETY: expr was returned by hew_cron_parse.
        unsafe { hew_cron_free(expr) };
    }

    #[test]
    fn null_safety() {
        // SAFETY: testing null pointer handling — should not crash.
        unsafe {
            assert_eq!(hew_cron_next(std::ptr::null(), 0), -1);
            assert!(read_and_free_optional(hew_cron_last_error()).is_some());
            assert_eq!(
                hew_cron_next_n(std::ptr::null(), 0, 5, std::ptr::null_mut()),
                0
            );
            assert!(hew_cron_to_string(std::ptr::null()).is_null());
            hew_cron_free(std::ptr::null_mut());
        }
    }

    #[test]
    fn successful_next_clears_last_error() {
        // SAFETY: testing invalid handle error recording.
        assert_eq!(unsafe { hew_cron_next(std::ptr::null(), 0) }, -1);
        // SAFETY: hew_cron_last_error returns either null or a malloc-allocated C string.
        assert!(unsafe { read_and_free_optional(hew_cron_last_error()) }.is_some());

        let expr_str = CString::new("0 * * * * * *").unwrap();
        // SAFETY: expr_str is a valid NUL-terminated C string.
        let expr = unsafe { hew_cron_parse(expr_str.as_ptr()) };
        assert!(!expr.is_null());

        // SAFETY: expr is valid.
        let next = unsafe { hew_cron_next(expr, 1_704_067_200) };
        assert!(next > 1_704_067_200);
        // SAFETY: hew_cron_last_error returns either null or a malloc-allocated C string.
        assert!(unsafe { read_and_free_optional(hew_cron_last_error()) }.is_none());

        // SAFETY: expr was returned by hew_cron_parse.
        unsafe { hew_cron_free(expr) };
    }
}
