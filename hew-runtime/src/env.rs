//! Hew runtime: environment variables and process arguments.
//!
//! Provides access to environment variables, command-line arguments, and basic
//! process/system information for compiled Hew programs. All returned strings
//! are allocated with `libc::malloc` so callers can free them with `libc::free`.

use crate::cabi::malloc_cstring;
use std::ffi::{c_char, CStr};

/// Convert a Rust `String` to a malloc-allocated C string. Returns null on failure.
fn string_to_malloc(s: &str) -> *mut c_char {
    // SAFETY: s.as_ptr() is valid for s.len() bytes.
    unsafe { malloc_cstring(s.as_ptr(), s.len()) }
}

// ---------------------------------------------------------------------------
// Environment variables
// ---------------------------------------------------------------------------

/// Get the value of an environment variable.
///
/// Returns a `malloc`-allocated, NUL-terminated C string, or null if the
/// variable is not set. The caller must free the result with `libc::free`.
///
/// # Safety
///
/// `key` must be a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_env_get(key: *const c_char) -> *mut c_char {
    if key.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: caller guarantees `key` is a valid NUL-terminated C string.
    let Ok(key_str) = (unsafe { CStr::from_ptr(key) }).to_str() else {
        return std::ptr::null_mut();
    };
    match std::env::var(key_str) {
        Ok(val) => string_to_malloc(&val),
        Err(_) => std::ptr::null_mut(),
    }
}

/// Set an environment variable.
///
/// # Safety
///
/// Both `key` and `val` must be valid NUL-terminated C strings.
/// Note: `std::env::set_var` is unsafe in multi-threaded contexts per Rust docs.
#[no_mangle]
pub unsafe extern "C" fn hew_env_set(key: *const c_char, val: *const c_char) {
    if key.is_null() || val.is_null() {
        return;
    }
    // SAFETY: caller guarantees both pointers are valid NUL-terminated C strings.
    let Ok(key_str) = (unsafe { CStr::from_ptr(key) }).to_str() else {
        return;
    };
    // SAFETY: caller guarantees val is a valid NUL-terminated C string.
    let Ok(val_str) = (unsafe { CStr::from_ptr(val) }).to_str() else {
        return;
    };
    // SAFETY: This is called from single-threaded Hew runtime initialization or
    // from compiled Hew programs where env access is serialized by the runtime.
    unsafe { std::env::set_var(key_str, val_str) };
}

/// Remove an environment variable.
///
/// # Safety
///
/// `key` must be a valid NUL-terminated C string.
/// Note: `std::env::remove_var` is unsafe in multi-threaded contexts per Rust docs.
#[no_mangle]
pub unsafe extern "C" fn hew_env_remove(key: *const c_char) {
    if key.is_null() {
        return;
    }
    // SAFETY: caller guarantees `key` is a valid NUL-terminated C string.
    let Ok(key_str) = (unsafe { CStr::from_ptr(key) }).to_str() else {
        return;
    };
    // SAFETY: This is called from single-threaded Hew runtime initialization or
    // from compiled Hew programs where env access is serialized by the runtime.
    unsafe { std::env::remove_var(key_str) };
}

/// Check if an environment variable exists. Returns 1 if set, 0 otherwise.
///
/// # Safety
///
/// `key` must be a valid NUL-terminated C string (or null, which returns 0).
#[no_mangle]
pub unsafe extern "C" fn hew_env_has(key: *const c_char) -> i32 {
    if key.is_null() {
        return 0;
    }
    // SAFETY: caller guarantees `key` is a valid NUL-terminated C string.
    let Ok(key_str) = (unsafe { CStr::from_ptr(key) }).to_str() else {
        return 0;
    };
    i32::from(std::env::var(key_str).is_ok())
}

// ---------------------------------------------------------------------------
// Command-line arguments
// ---------------------------------------------------------------------------

/// Return the number of command-line arguments.
///
/// # Safety
///
/// No preconditions.
#[expect(
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap,
    reason = "Argument count is bounded by OS limits, always fits in i32"
)]
#[no_mangle]
pub unsafe extern "C" fn hew_args_count() -> i32 {
    std::env::args().count() as i32
}

/// Get the command-line argument at `index`.
///
/// Returns a `malloc`-allocated, NUL-terminated C string, or null if the
/// index is out of bounds. The caller must free the result with `libc::free`.
///
/// # Safety
///
/// No preconditions beyond a valid `i32`.
#[no_mangle]
pub unsafe extern "C" fn hew_args_get(index: i32) -> *mut c_char {
    if index < 0 {
        return std::ptr::null_mut();
    }
    #[expect(clippy::cast_sign_loss, reason = "guarded by index >= 0")]
    let Some(arg) = std::env::args().nth(index as usize) else {
        return std::ptr::null_mut();
    };
    string_to_malloc(&arg)
}

// ---------------------------------------------------------------------------
// Process / system info
// ---------------------------------------------------------------------------

/// Return the current working directory as a malloc-allocated C string.
/// Returns null on failure.
///
/// # Safety
///
/// No preconditions. The returned pointer must be freed with `libc::free`.
#[no_mangle]
pub unsafe extern "C" fn hew_cwd() -> *mut c_char {
    let Ok(cwd) = std::env::current_dir() else {
        return std::ptr::null_mut();
    };
    let Some(s) = cwd.to_str() else {
        return std::ptr::null_mut();
    };
    string_to_malloc(s)
}

/// Return the home directory as a malloc-allocated C string.
/// Returns null if unavailable.
///
/// # Safety
///
/// No preconditions. The returned pointer must be freed with `libc::free`.
#[no_mangle]
pub unsafe extern "C" fn hew_home_dir() -> *mut c_char {
    match std::env::var("HOME") {
        Ok(home) => string_to_malloc(&home),
        Err(_) => std::ptr::null_mut(),
    }
}

/// Return the system hostname as a malloc-allocated C string.
/// Returns null on failure.
///
/// # Safety
///
/// No preconditions. The returned pointer must be freed with `libc::free`.
#[no_mangle]
pub unsafe extern "C" fn hew_hostname() -> *mut c_char {
    let mut buf = [0u8; 256];
    // SAFETY: gethostname with a valid buffer and size is always safe.
    let rc = unsafe { libc::gethostname(buf.as_mut_ptr().cast::<c_char>(), buf.len()) };
    if rc != 0 {
        return std::ptr::null_mut();
    }
    // Ensure NUL-termination.
    buf[buf.len() - 1] = 0;
    // SAFETY: buf is NUL-terminated after gethostname success.
    let cstr = unsafe { CStr::from_ptr(buf.as_ptr().cast::<c_char>()) };
    let Ok(s) = cstr.to_str() else {
        return std::ptr::null_mut();
    };
    string_to_malloc(s)
}

/// Return the current process ID.
///
/// # Safety
///
/// No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_pid() -> i32 {
    std::process::id().cast_signed()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CString;

    /// Helper to read a malloc'd C string and free it.
    ///
    /// # Safety
    ///
    /// `ptr` must be a non-null, NUL-terminated, malloc-allocated C string.
    unsafe fn read_and_free(ptr: *mut c_char) -> String {
        assert!(!ptr.is_null());
        // SAFETY: ptr is a valid NUL-terminated C string per caller.
        let s = unsafe { CStr::from_ptr(ptr) }.to_str().unwrap().to_owned();
        // SAFETY: ptr was allocated with libc::malloc.
        unsafe { libc::free(ptr.cast()) };
        s
    }

    #[test]
    fn test_env_set_get_has_roundtrip() {
        let key = CString::new("HEW_TEST_ENV_VAR_42").unwrap();
        let val = CString::new("hello_hew").unwrap();

        // SAFETY: both pointers are valid NUL-terminated C strings.
        unsafe {
            // Should not exist yet.
            assert_eq!(hew_env_has(key.as_ptr()), 0);
            assert!(hew_env_get(key.as_ptr()).is_null());

            // Set and verify.
            hew_env_set(key.as_ptr(), val.as_ptr());
            assert_eq!(hew_env_has(key.as_ptr()), 1);

            let got = hew_env_get(key.as_ptr());
            let text = read_and_free(got);
            assert_eq!(text, "hello_hew");

            // Remove and verify.
            hew_env_remove(key.as_ptr());
            assert_eq!(hew_env_has(key.as_ptr()), 0);
        }
    }

    #[test]
    fn test_env_get_null_and_missing() {
        // SAFETY: null is explicitly handled.
        assert!(unsafe { hew_env_get(std::ptr::null()) }.is_null());

        let missing = CString::new("HEW_DEFINITELY_NOT_SET_XYZ_123").unwrap();
        // SAFETY: missing.as_ptr() is a valid NUL-terminated C string.
        assert!(unsafe { hew_env_get(missing.as_ptr()) }.is_null());
    }

    #[test]
    fn test_args_count() {
        // SAFETY: no preconditions.
        let count = unsafe { hew_args_count() };
        // There is always at least the program name.
        assert!(count >= 1);
    }

    #[test]
    fn test_args_get_out_of_bounds() {
        // SAFETY: no preconditions.
        assert!(unsafe { hew_args_get(-1) }.is_null());
        // SAFETY: no preconditions; out-of-bounds index returns null.
        assert!(unsafe { hew_args_get(9999) }.is_null());
    }

    #[test]
    fn test_cwd_returns_nonempty() {
        // SAFETY: no preconditions.
        let ptr = unsafe { hew_cwd() };
        // SAFETY: cwd should succeed in test environment.
        let text = unsafe { read_and_free(ptr) };
        assert!(!text.is_empty());
    }

    #[test]
    fn test_hostname_returns_nonempty() {
        // SAFETY: no preconditions.
        let ptr = unsafe { hew_hostname() };
        // SAFETY: hostname should succeed in test environment.
        let text = unsafe { read_and_free(ptr) };
        assert!(!text.is_empty());
    }

    #[test]
    fn test_pid_is_positive() {
        // SAFETY: no preconditions.
        let pid = unsafe { hew_pid() };
        assert!(pid > 0);
    }
}
