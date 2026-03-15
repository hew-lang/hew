//! Hew runtime: `stdio` module.
//!
//! Standard I/O operations (stdout, stderr, stdin) with C ABI.

use std::ffi::{c_char, CStr, CString};
use std::io::{self, Read, Write};

/// Write a string to stdout without a trailing newline.
///
/// # Safety
///
/// `s` must be a valid, NUL-terminated C string (or null, which writes nothing).
#[no_mangle]
pub unsafe extern "C" fn hew_io_write(s: *const c_char) {
    if s.is_null() {
        return;
    }
    // SAFETY: Caller guarantees s is a valid NUL-terminated C string.
    let c_str = unsafe { CStr::from_ptr(s) };
    let Ok(rust_str) = c_str.to_str() else {
        return;
    };
    let _ = io::stdout().write_all(rust_str.as_bytes());
    let _ = io::stdout().flush();
}

/// Write a string to stderr without a trailing newline.
///
/// # Safety
///
/// `s` must be a valid, NUL-terminated C string (or null, which writes nothing).
#[no_mangle]
pub unsafe extern "C" fn hew_io_write_err(s: *const c_char) {
    if s.is_null() {
        return;
    }
    // SAFETY: Caller guarantees s is a valid NUL-terminated C string.
    let c_str = unsafe { CStr::from_ptr(s) };
    let Ok(rust_str) = c_str.to_str() else {
        return;
    };
    let _ = io::stderr().write_all(rust_str.as_bytes());
    let _ = io::stderr().flush();
}

/// Read a single line from stdin, stripping the trailing newline.
///
/// Returns a null pointer on EOF or error.
///
/// # Safety
///
/// The caller is responsible for calling `free()` on the returned pointer.
#[no_mangle]
pub extern "C" fn hew_io_read_line() -> *mut c_char {
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
            match CString::new(buf) {
                Ok(cs) => cs.into_raw(),
                Err(_) => std::ptr::null_mut(),
            }
        }
    }
}

/// Read all available data from stdin and return as a heap-allocated C string.
///
/// Returns a null pointer on error.
///
/// # Safety
///
/// The caller is responsible for calling `free()` on the returned pointer.
#[no_mangle]
pub extern "C" fn hew_io_read_all() -> *mut c_char {
    let mut buf = String::new();
    match io::stdin().read_to_string(&mut buf) {
        Ok(_) => match CString::new(buf) {
            Ok(cs) => cs.into_raw(),
            Err(_) => std::ptr::null_mut(),
        },
        Err(_) => std::ptr::null_mut(),
    }
}
