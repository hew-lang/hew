//! Hew runtime: `stdio` module.
//!
//! Standard I/O operations (stdout, stderr, stdin) with C ABI.
//!
//! All string values use the managed, length-bounded Hew string carrier.
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

use hew_cabi::string::{string_as_bytes, string_from_str, HewString};
use std::io::{self, Read, Write};

/// Write a string to stdout without a trailing newline.
///
/// # Safety
///
/// `s` must be null or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_io_write(s: *const HewString) {
    // SAFETY: the caller supplies a live managed string handle; null is empty.
    let bytes = unsafe { string_as_bytes(s) };
    let _ = io::stdout().write_all(bytes);
    let _ = io::stdout().flush();
}

/// Write a string to stderr without a trailing newline.
///
/// # Safety
///
/// `s` must be null or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_io_write_err(s: *const HewString) {
    // SAFETY: the caller supplies a live managed string handle; null is empty.
    let bytes = unsafe { string_as_bytes(s) };
    let _ = io::stderr().write_all(bytes);
    let _ = io::stderr().flush();
}

/// Read a single line from stdin, stripping the trailing newline.
///
/// Returns a managed string owner. Null represents both an empty line and the
/// existing EOF/error sentinel; a future typed I/O result must separate them.
///
/// # Safety
///
/// No preconditions.
///
/// # Ownership
///
/// The caller owns the returned handle and must release it with
/// `hew_string_drop`.
#[no_mangle]
pub extern "C" fn hew_io_read_line() -> *mut HewString {
    let mut buf = String::new();
    match io::stdin().read_line(&mut buf) {
        Ok(0) | Err(_) => std::ptr::null_mut(),
        Ok(_) => {
            // Trim the trailing newline, if present.
            if buf.ends_with('\n') {
                buf.pop();
                if buf.ends_with('\r') {
                    buf.pop();
                }
            }
            string_from_str(&buf)
        }
    }
}

/// Read all available valid UTF-8 data from stdin into a managed string.
///
/// Embedded NUL bytes are preserved. Null represents both empty input and the
/// existing read/UTF-8 error sentinel; a future typed I/O result must separate
/// them.
///
/// # Safety
///
/// No preconditions.
///
/// # Ownership
///
/// The caller owns the returned handle and must release it with
/// `hew_string_drop`.
#[no_mangle]
pub extern "C" fn hew_io_read_all() -> *mut HewString {
    let mut buf = String::new();
    match io::stdin().read_to_string(&mut buf) {
        Ok(_) => string_from_str(&buf),
        Err(_) => std::ptr::null_mut(),
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn managed(text: &str) -> *mut HewString {
        string_from_str(text)
    }

    #[test]
    fn write_null_is_noop() {
        // Passing null should not panic.
        // SAFETY: Null pointer passed deliberately to verify no-op behaviour.
        unsafe { hew_io_write(std::ptr::null()) };
    }

    #[test]
    fn write_err_null_is_noop() {
        // Passing null should not panic.
        // SAFETY: Null pointer passed deliberately to verify no-op behaviour.
        unsafe { hew_io_write_err(std::ptr::null()) };
    }

    #[test]
    fn write_valid_string() {
        let s = managed("hello from test");
        // SAFETY: `s` is a live managed owner and is released exactly once.
        unsafe {
            hew_io_write(s);
            hew_cabi::string::string_release(s);
        }
    }

    #[test]
    fn write_err_valid_string() {
        let s = managed("error from test");
        // SAFETY: `s` is a live managed owner and is released exactly once.
        unsafe {
            hew_io_write_err(s);
            hew_cabi::string::string_release(s);
        }
    }

    #[test]
    fn write_empty_string() {
        // SAFETY: null is the canonical managed empty string.
        unsafe { hew_io_write(std::ptr::null()) };
    }

    #[test]
    fn write_err_empty_string() {
        // SAFETY: null is the canonical managed empty string.
        unsafe { hew_io_write_err(std::ptr::null()) };
    }
}
