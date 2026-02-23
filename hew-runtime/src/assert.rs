//! Hew runtime: `assert` module.
//!
//! Assertion functions exposed with C ABI for use by compiled Hew test programs.
//! On failure, each function prints a diagnostic to stderr and aborts.

use std::os::raw::c_char;

/// Assert that a condition (as `i64`) is truthy (non-zero).
///
/// # Safety
///
/// Called from compiled Hew programs via C ABI. No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_assert(cond: i64) {
    if cond == 0 {
        eprintln!("assertion failed");
        // SAFETY: abort() is always safe to call.
        unsafe { libc::abort() };
    }
}

/// Assert that two `i64` values are equal.
///
/// # Safety
///
/// Called from compiled Hew programs via C ABI. No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_assert_eq_i64(left: i64, right: i64) {
    if left != right {
        eprintln!("assertion failed: assert_eq({left}, {right})");
        // SAFETY: abort() is always safe to call.
        unsafe { libc::abort() };
    }
}

/// Assert that two `i64` values are not equal.
///
/// # Safety
///
/// Called from compiled Hew programs via C ABI. No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_assert_ne_i64(left: i64, right: i64) {
    if left == right {
        eprintln!("assertion failed: assert_ne({left}, {right})");
        // SAFETY: abort() is always safe to call.
        unsafe { libc::abort() };
    }
}

/// Assert that two strings (C char pointers) are equal.
///
/// # Safety
///
/// Both `left` and `right` must be valid, NUL-terminated C strings.
/// Called from compiled Hew programs via C ABI.
#[no_mangle]
pub unsafe extern "C" fn hew_assert_eq_str(left: *const c_char, right: *const c_char) {
    // SAFETY: Caller guarantees both pointers are valid NUL-terminated strings.
    let cmp = unsafe { libc::strcmp(left, right) };
    if cmp != 0 {
        // SAFETY: Caller guarantees left is a valid NUL-terminated C string.
        let l = unsafe { std::ffi::CStr::from_ptr(left) }.to_string_lossy();
        // SAFETY: Caller guarantees right is a valid NUL-terminated C string.
        let r = unsafe { std::ffi::CStr::from_ptr(right) }.to_string_lossy();
        eprintln!("assertion failed: assert_eq(\"{l}\", \"{r}\")");
        // SAFETY: abort() is always safe to call.
        unsafe { libc::abort() };
    }
}

/// Assert that two strings (C char pointers) are not equal.
///
/// # Safety
///
/// Both `left` and `right` must be valid, NUL-terminated C strings.
/// Called from compiled Hew programs via C ABI.
#[no_mangle]
pub unsafe extern "C" fn hew_assert_ne_str(left: *const c_char, right: *const c_char) {
    // SAFETY: Caller guarantees both pointers are valid NUL-terminated strings.
    let cmp = unsafe { libc::strcmp(left, right) };
    if cmp == 0 {
        // SAFETY: Caller guarantees left is a valid NUL-terminated C string.
        let l = unsafe { std::ffi::CStr::from_ptr(left) }.to_string_lossy();
        eprintln!("assertion failed: assert_ne(\"{l}\", \"{l}\")");
        // SAFETY: abort() is always safe to call.
        unsafe { libc::abort() };
    }
}

/// Assert that two `f64` values are equal.
///
/// # Safety
///
/// Called from compiled Hew programs via C ABI. No preconditions.
#[no_mangle]
#[expect(
    clippy::float_cmp,
    reason = "exact equality is the intended semantics for assert_eq"
)]
pub unsafe extern "C" fn hew_assert_eq_f64(left: f64, right: f64) {
    if left != right {
        eprintln!("assertion failed: assert_eq({left}, {right})");
        // SAFETY: abort() is always safe to call.
        unsafe { libc::abort() };
    }
}

/// Assert that two `f64` values are not equal.
///
/// # Safety
///
/// Called from compiled Hew programs via C ABI. No preconditions.
#[no_mangle]
#[expect(
    clippy::float_cmp,
    reason = "exact equality is the intended semantics for assert_ne"
)]
pub unsafe extern "C" fn hew_assert_ne_f64(left: f64, right: f64) {
    if left == right {
        eprintln!("assertion failed: assert_ne({left}, {right})");
        // SAFETY: abort() is always safe to call.
        unsafe { libc::abort() };
    }
}

/// Assert that two `bool` values (as `i32`: 0 or 1) are equal.
///
/// # Safety
///
/// Called from compiled Hew programs via C ABI. No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_assert_eq_bool(left: i32, right: i32) {
    if left != right {
        let l = if left != 0 { "true" } else { "false" };
        let r = if right != 0 { "true" } else { "false" };
        eprintln!("assertion failed: assert_eq({l}, {r})");
        // SAFETY: abort() is always safe to call.
        unsafe { libc::abort() };
    }
}

/// Assert that two `bool` values (as `i32`: 0 or 1) are not equal.
///
/// # Safety
///
/// Called from compiled Hew programs via C ABI. No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_assert_ne_bool(left: i32, right: i32) {
    if left == right {
        let l = if left != 0 { "true" } else { "false" };
        let r = if right != 0 { "true" } else { "false" };
        eprintln!("assertion failed: assert_ne({l}, {r})");
        // SAFETY: abort() is always safe to call.
        unsafe { libc::abort() };
    }
}
