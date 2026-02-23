//! Common C ABI helpers used across runtime modules.
//!
//! These functions handle the repetitive patterns of converting between Rust
//! and C types in `#[no_mangle] extern "C"` functions: allocating
//! `malloc`-backed C strings, converting `*const c_char` to `&str`, etc.

use std::ffi::CStr;
use std::os::raw::c_char;

/// Allocate a NUL-terminated C string via `libc::malloc`, copying `len` bytes
/// from `src`. Returns null on allocation failure.
///
/// # Safety
///
/// If `len > 0`, `src` must point to at least `len` readable bytes.
#[must_use]
pub unsafe fn malloc_cstring(src: *const u8, len: usize) -> *mut c_char {
    // SAFETY: We request len+1 bytes from malloc; it returns a valid pointer or null.
    let ptr = unsafe { libc::malloc(len + 1) }.cast::<u8>();
    if ptr.is_null() {
        return ptr.cast::<c_char>();
    }
    if len > 0 {
        // SAFETY: Caller guarantees src is valid for len bytes; ptr is freshly
        // allocated with len+1 bytes, so both regions are valid and non-overlapping.
        unsafe { std::ptr::copy_nonoverlapping(src, ptr, len) };
    }
    // SAFETY: ptr + len is within the allocated region of len+1 bytes.
    unsafe { *ptr.add(len) = 0 };
    ptr.cast::<c_char>()
}

/// Copy a Rust `&str` into a `malloc`-allocated, NUL-terminated C string.
///
/// Returns null on allocation failure.
#[must_use]
pub fn str_to_malloc(s: &str) -> *mut c_char {
    // SAFETY: s.as_ptr() is valid for s.len() bytes.
    unsafe { malloc_cstring(s.as_ptr(), s.len()) }
}

/// Extract a NUL-terminated C string pointer into a `&str`, returning `None`
/// if the pointer is null or contains invalid UTF-8.
///
/// # Safety
///
/// If non-null, `ptr` must point to a valid NUL-terminated C string.
#[must_use]
pub unsafe fn cstr_to_str<'a>(ptr: *const c_char) -> Option<&'a str> {
    if ptr.is_null() {
        return None;
    }
    // SAFETY: Caller guarantees ptr is a valid NUL-terminated C string.
    unsafe { CStr::from_ptr(ptr) }.to_str().ok()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::c_void;

    #[test]
    fn str_to_malloc_roundtrip() {
        let input = "hello, hew!";
        let ptr = str_to_malloc(input);
        assert!(!ptr.is_null());
        // SAFETY: ptr was just allocated by str_to_malloc with a NUL terminator.
        let recovered = unsafe { CStr::from_ptr(ptr) }.to_str().unwrap();
        assert_eq!(recovered, input);
        // SAFETY: ptr was allocated with libc::malloc.
        unsafe { libc::free(ptr.cast::<c_void>()) };
    }

    #[test]
    fn str_to_malloc_empty() {
        let ptr = str_to_malloc("");
        assert!(!ptr.is_null());
        // SAFETY: ptr was just allocated by str_to_malloc with a NUL terminator.
        let recovered = unsafe { CStr::from_ptr(ptr) }.to_str().unwrap();
        assert_eq!(recovered, "");
        // SAFETY: ptr was allocated with libc::malloc.
        unsafe { libc::free(ptr.cast::<c_void>()) };
    }

    #[test]
    fn cstr_to_str_null_returns_none() {
        // SAFETY: Passing null is the scenario under test.
        assert!(unsafe { cstr_to_str(std::ptr::null()) }.is_none());
    }

    #[test]
    fn cstr_to_str_valid() {
        let s = c"hello";
        // SAFETY: s is a valid C string literal.
        let result = unsafe { cstr_to_str(s.as_ptr()) };
        assert_eq!(result, Some("hello"));
    }
}
