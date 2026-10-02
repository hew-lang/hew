//! Hew runtime: `stdio` module.
//!
//! Standard I/O operations (stdout, stderr, stdin) with C ABI.
//!
//! All string values use the managed, length-bounded Hew string carrier.
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

use hew_cabi::string::{string_as_bytes, HewString};

use crate::output::{write, Stream};

/// Write a string to stdout without a trailing newline.
///
/// # Safety
///
/// `s` must be null or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_io_write(s: *const HewString) {
    // SAFETY: the caller supplies a live managed string handle; null is empty.
    write(Stream::Out, unsafe { string_as_bytes(s) });
}

/// Write a string to stderr without a trailing newline.
///
/// # Safety
///
/// `s` must be null or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_io_write_err(s: *const HewString) {
    // SAFETY: the caller supplies a live managed string handle; null is empty.
    write(Stream::Err, unsafe { string_as_bytes(s) });
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use hew_cabi::string::string_from_str;

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
