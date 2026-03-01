//! Hew runtime: cron expression parsing and scheduling.
//!
//! Provides cron expression parsing and next-occurrence calculation for
//! compiled Hew programs. The opaque [`HewCronExpr`] handle wraps a
//! [`cron::Schedule`] and must be freed with [`hew_cron_free`].

use hew_cabi::cabi::{cstr_to_str, str_to_malloc};
use std::ffi::c_char;
use std::str::FromStr;

use chrono::Utc;
use cron::Schedule;

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
        return std::ptr::null_mut();
    };
    match Schedule::from_str(s) {
        Ok(schedule) => Box::into_raw(Box::new(HewCronExpr { inner: schedule })),
        Err(_) => std::ptr::null_mut(),
    }
}

/// Return the next occurrence after the given epoch timestamp (seconds).
///
/// Returns the epoch timestamp (seconds) of the next matching time, or `-1`
/// if no future occurrence can be computed.
///
/// # Safety
///
/// `expr` must be a valid pointer returned by [`hew_cron_parse`].
#[no_mangle]
pub unsafe extern "C" fn hew_cron_next(expr: *const HewCronExpr, after_epoch_secs: i64) -> i64 {
    if expr.is_null() {
        return -1;
    }
    // SAFETY: expr is a valid HewCronExpr pointer per caller contract.
    let cron_expr = unsafe { &*expr };
    let Some(dt) = chrono::DateTime::<Utc>::from_timestamp(after_epoch_secs, 0) else {
        return -1;
    };
    match cron_expr.inner.after(&dt).next() {
        Some(next_dt) => next_dt.timestamp(),
        None => -1,
    }
}

/// Write up to `count` next occurrences after the given epoch timestamp into
/// the `out` array.
///
/// Returns the number of timestamps actually written (may be less than
/// `count` if the schedule has fewer future occurrences).
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
    if expr.is_null() || out.is_null() || count <= 0 {
        return 0;
    }
    // SAFETY: expr is a valid HewCronExpr pointer per caller contract.
    let cron_expr = unsafe { &*expr };
    let Some(dt) = chrono::DateTime::<Utc>::from_timestamp(after_epoch_secs, 0) else {
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
        written as i32
    }
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

        // Null input.
        // SAFETY: testing null pointer handling.
        assert!(unsafe { hew_cron_parse(std::ptr::null()) }.is_null());
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
        for i in 0..5 {
            #[expect(clippy::cast_possible_wrap, reason = "test: i fits in i64")]
            let expected = after + 60 * (i as i64 + 1);
            assert_eq!(out[i], expected, "occurrence {i} mismatch");
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
            assert_eq!(
                hew_cron_next_n(std::ptr::null(), 0, 5, std::ptr::null_mut()),
                0
            );
            assert!(hew_cron_to_string(std::ptr::null()).is_null());
            hew_cron_free(std::ptr::null_mut());
        }
    }
}
