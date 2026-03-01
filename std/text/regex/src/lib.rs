//! Hew runtime: `std::text::regex` module.
//!
//! Provides regular expression matching and replacement for compiled Hew
//! programs. All returned strings are allocated with `libc::malloc` so callers
//! can free them with `libc::free`.

// Force-link hew-runtime so the linker can resolve hew_vec_* symbols
// referenced by hew-cabi's object code.
#[cfg(test)]
extern crate hew_runtime;

use hew_cabi::cabi::{cstr_to_str, str_to_malloc};
use std::ffi::c_char;

/// Opaque handle wrapping a compiled [`regex::Regex`].
///
/// Created by [`hew_regex_new`], freed by [`hew_regex_free`].
#[derive(Debug)]
pub struct HewRegex {
    inner: regex::Regex,
}

/// Compile a regular expression pattern.
///
/// Returns a heap-allocated [`HewRegex`], or null if the pattern is invalid.
/// The caller must free it with [`hew_regex_free`].
///
/// # Safety
///
/// `pattern` must be a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_regex_new(pattern: *const c_char) -> *mut HewRegex {
    // SAFETY: caller guarantees pattern is a valid NUL-terminated C string.
    let Some(pat) = (unsafe { cstr_to_str(pattern) }) else {
        return std::ptr::null_mut();
    };
    match regex::Regex::new(pat) {
        Ok(re) => Box::into_raw(Box::new(HewRegex { inner: re })),
        Err(_) => std::ptr::null_mut(),
    }
}

/// Test whether `text` matches the compiled regex.
///
/// Returns `1` if the text matches, `0` otherwise.
///
/// # Safety
///
/// - `re` must be a valid pointer returned by [`hew_regex_new`].
/// - `text` must be a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_regex_is_match(re: *const HewRegex, text: *const c_char) -> i32 {
    if re.is_null() {
        return 0;
    }
    // SAFETY: re is a valid HewRegex pointer per caller contract.
    let regex = unsafe { &*re };
    // SAFETY: text is a valid NUL-terminated C string per caller contract.
    let Some(text_str) = (unsafe { cstr_to_str(text) }) else {
        return 0;
    };
    i32::from(regex.inner.is_match(text_str))
}

/// Find the first match of the compiled regex in `text`.
///
/// Returns a `malloc`-allocated, NUL-terminated C string containing the
/// matched substring, or null if no match is found. The caller must free
/// the returned string with `libc::free`.
///
/// # Safety
///
/// - `re` must be a valid pointer returned by [`hew_regex_new`].
/// - `text` must be a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_regex_find(re: *const HewRegex, text: *const c_char) -> *mut c_char {
    if re.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: re is a valid HewRegex pointer per caller contract.
    let regex = unsafe { &*re };
    // SAFETY: text is a valid NUL-terminated C string per caller contract.
    let Some(text_str) = (unsafe { cstr_to_str(text) }) else {
        return std::ptr::null_mut();
    };
    match regex.inner.find(text_str) {
        Some(m) => str_to_malloc(m.as_str()),
        None => std::ptr::null_mut(),
    }
}

/// Replace all matches of the compiled regex in `text` with `replacement`.
///
/// Returns a `malloc`-allocated, NUL-terminated C string. The caller must free
/// it with `libc::free`. Returns null on error.
///
/// # Safety
///
/// - `re` must be a valid pointer returned by [`hew_regex_new`].
/// - `text` and `replacement` must be valid NUL-terminated C strings.
#[no_mangle]
pub unsafe extern "C" fn hew_regex_replace(
    re: *const HewRegex,
    text: *const c_char,
    replacement: *const c_char,
) -> *mut c_char {
    if re.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: re is a valid HewRegex pointer per caller contract.
    let regex = unsafe { &*re };
    // SAFETY: text is a valid NUL-terminated C string per caller contract.
    let Some(text_str) = (unsafe { cstr_to_str(text) }) else {
        return std::ptr::null_mut();
    };
    // SAFETY: replacement is a valid NUL-terminated C string per caller contract.
    let Some(repl_str) = (unsafe { cstr_to_str(replacement) }) else {
        return std::ptr::null_mut();
    };
    str_to_malloc(&regex.inner.replace_all(text_str, repl_str))
}

/// Free a compiled [`HewRegex`] previously returned by [`hew_regex_new`].
///
/// # Safety
///
/// `re` must be a pointer previously returned by [`hew_regex_new`], and must
/// not have been freed already.
#[no_mangle]
pub unsafe extern "C" fn hew_regex_free(re: *mut HewRegex) {
    if re.is_null() {
        return;
    }
    // SAFETY: re was allocated with Box::into_raw in hew_regex_new.
    drop(unsafe { Box::from_raw(re) });
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::{CStr, CString};

    #[test]
    fn test_regex_is_match() {
        let pattern = CString::new(r"\d+").unwrap();
        // SAFETY: pattern is a valid NUL-terminated C string.
        let re = unsafe { hew_regex_new(pattern.as_ptr()) };
        assert!(!re.is_null());

        let text_yes = CString::new("abc123def").unwrap();
        let text_no = CString::new("abcdef").unwrap();
        // SAFETY: re and text pointers are valid.
        assert_eq!(unsafe { hew_regex_is_match(re, text_yes.as_ptr()) }, 1);
        // SAFETY: re and text pointers are valid.
        assert_eq!(unsafe { hew_regex_is_match(re, text_no.as_ptr()) }, 0);

        // SAFETY: re was returned by hew_regex_new.
        unsafe { hew_regex_free(re) };
    }

    #[test]
    fn test_regex_find() {
        let pattern = CString::new(r"[a-z]+").unwrap();
        // SAFETY: pattern is a valid NUL-terminated C string.
        let re = unsafe { hew_regex_new(pattern.as_ptr()) };
        assert!(!re.is_null());

        let text = CString::new("123hello456").unwrap();
        // SAFETY: re and text pointers are valid.
        let matched = unsafe { hew_regex_find(re, text.as_ptr()) };
        assert!(!matched.is_null());
        // SAFETY: matched was allocated by hew_regex_find.
        let matched_str = unsafe { CStr::from_ptr(matched) }.to_str().unwrap();
        assert_eq!(matched_str, "hello");
        // SAFETY: matched was allocated with libc::malloc.
        unsafe { libc::free(matched.cast()) };

        // Test no match
        let text_no = CString::new("123456").unwrap();
        // SAFETY: re and text pointers are valid.
        let no_match = unsafe { hew_regex_find(re, text_no.as_ptr()) };
        assert!(no_match.is_null());

        // SAFETY: re was returned by hew_regex_new.
        unsafe { hew_regex_free(re) };
    }

    #[test]
    fn test_regex_replace() {
        let pattern = CString::new(r"\d+").unwrap();
        // SAFETY: pattern is a valid NUL-terminated C string.
        let re = unsafe { hew_regex_new(pattern.as_ptr()) };
        assert!(!re.is_null());

        let text = CString::new("a1b2c3").unwrap();
        let repl = CString::new("X").unwrap();
        // SAFETY: re, text, and repl pointers are valid.
        let result = unsafe { hew_regex_replace(re, text.as_ptr(), repl.as_ptr()) };
        assert!(!result.is_null());
        // SAFETY: result was allocated by hew_regex_replace.
        let result_str = unsafe { CStr::from_ptr(result) }.to_str().unwrap();
        assert_eq!(result_str, "aXbXcX");
        // SAFETY: result was allocated with libc::malloc.
        unsafe { libc::free(result.cast()) };

        // SAFETY: re was returned by hew_regex_new.
        unsafe { hew_regex_free(re) };
    }

    #[test]
    fn test_regex_null_safety() {
        // SAFETY: Testing null pointer handling.
        assert!(unsafe { hew_regex_new(std::ptr::null()) }.is_null());
        // SAFETY: Testing null pointer handling.
        assert_eq!(
            unsafe { hew_regex_is_match(std::ptr::null(), std::ptr::null()) },
            0
        );
        // SAFETY: Testing null pointer handling — should not crash.
        unsafe { hew_regex_free(std::ptr::null_mut()) };
    }
}
