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

/// Whether `fd` can be watched for readability: 0 when it can, 1 when it is
/// not an open descriptor, 2 when watching it is unsupported (a regular file,
/// which is always readable, or a platform without descriptor readiness).
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub extern "C" fn hew_io_watch_check(fd: i32) -> i32 {
    #[cfg(unix)]
    {
        // SAFETY: `stat` is plain data; all-zero is a valid value.
        let mut stat: libc::stat = unsafe { std::mem::zeroed() };
        // SAFETY: fstat writes one local stat for any descriptor number.
        if fd < 0 || unsafe { libc::fstat(fd, &raw mut stat) } != 0 {
            return 1;
        }
        if stat.st_mode & libc::S_IFMT == libc::S_IFREG {
            return 2;
        }
        0
    }
    #[cfg(not(unix))]
    {
        let _ = fd;
        2
    }
}

/// Watch a descriptor the program owns for readability as a `Stream<()>`;
/// null when [`hew_io_watch_check`] refuses it. The stream watches its own
/// duplicate, so the descriptor stays with its owner, who may close it in
/// either order with the stream.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub extern "C" fn hew_io_watch_readable(fd: i32) -> *mut crate::stream::HewStreamPair {
    if hew_io_watch_check(fd) != 0 {
        return std::ptr::null_mut();
    }
    #[cfg(unix)]
    {
        crate::stream::readiness_stream(fd)
    }
    #[cfg(not(unix))]
    {
        std::ptr::null_mut()
    }
}

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
