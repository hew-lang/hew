//! Hew runtime: `print` module.
//!
//! Typed print functions exposed with C ABI for use by compiled Hew programs.
//! Each function writes directly to stdout via `libc::printf`.

use std::os::raw::c_char;

/// Print an `i32` without trailing newline.
///
/// # Safety
///
/// Called from compiled Hew programs via C ABI. No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_print_i32(x: i32) {
    // SAFETY: Format string is a valid NUL-terminated C literal; x is a plain i32.
    unsafe { libc::printf(c"%d".as_ptr(), x) };
}

/// Print an `i32` followed by a newline.
///
/// # Safety
///
/// Called from compiled Hew programs via C ABI. No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_println_i32(x: i32) {
    // SAFETY: Format string is a valid NUL-terminated C literal; x is a plain i32.
    unsafe { libc::printf(c"%d\n".as_ptr(), x) };
}

/// Print an `i64` without trailing newline.
///
/// # Safety
///
/// Called from compiled Hew programs via C ABI. No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_print_i64(x: i64) {
    // SAFETY: Format string is a valid NUL-terminated C literal; x is a plain i64.
    unsafe { libc::printf(c"%ld".as_ptr(), x) };
}

/// Print an `i64` followed by a newline.
///
/// # Safety
///
/// Called from compiled Hew programs via C ABI. No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_println_i64(x: i64) {
    // SAFETY: Format string is a valid NUL-terminated C literal; x is a plain i64.
    unsafe { libc::printf(c"%ld\n".as_ptr(), x) };
}

/// Print an `f64` without trailing newline.
///
/// # Safety
///
/// Called from compiled Hew programs via C ABI. No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_print_f64(x: f64) {
    // SAFETY: Format string is a valid NUL-terminated C literal; x is a plain f64.
    unsafe { libc::printf(c"%g".as_ptr(), x) };
}

/// Print an `f64` followed by a newline.
///
/// # Safety
///
/// Called from compiled Hew programs via C ABI. No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_println_f64(x: f64) {
    // SAFETY: Format string is a valid NUL-terminated C literal; x is a plain f64.
    unsafe { libc::printf(c"%g\n".as_ptr(), x) };
}

/// Print a boolean as `"true"` or `"false"` without trailing newline.
///
/// # Safety
///
/// Called from compiled Hew programs via C ABI. No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_print_bool(x: bool) {
    let s = if x { c"true" } else { c"false" };
    // SAFETY: Format string and s are valid NUL-terminated C literals.
    unsafe { libc::printf(c"%s".as_ptr(), s.as_ptr()) };
}

/// Print a boolean as `"true"` or `"false"` followed by a newline.
///
/// # Safety
///
/// Called from compiled Hew programs via C ABI. No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_println_bool(x: bool) {
    let s = if x { c"true\n" } else { c"false\n" };
    // SAFETY: Format string and s are valid NUL-terminated C literals.
    unsafe { libc::printf(c"%s".as_ptr(), s.as_ptr()) };
}

/// Print a C string without trailing newline.
///
/// # Safety
///
/// `s` must be a valid, NUL-terminated C string (or null, which prints nothing).
#[no_mangle]
pub unsafe extern "C" fn hew_print_str(s: *const c_char) {
    if s.is_null() {
        return;
    }
    // SAFETY: Caller guarantees s is a valid NUL-terminated C string.
    unsafe { libc::printf(c"%s".as_ptr(), s) };
}

/// Print a C string followed by a newline.
///
/// # Safety
///
/// `s` must be a valid, NUL-terminated C string (or null, which prints only a newline).
#[no_mangle]
pub unsafe extern "C" fn hew_println_str(s: *const c_char) {
    if s.is_null() {
        // SAFETY: Format string is a valid NUL-terminated C literal.
        unsafe { libc::printf(c"\n".as_ptr()) };
        return;
    }
    // SAFETY: Caller guarantees s is a valid NUL-terminated C string.
    unsafe { libc::printf(c"%s\n".as_ptr(), s) };
}
