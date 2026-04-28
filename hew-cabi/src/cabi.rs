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

/// Allocate `src.len()` bytes via `libc::malloc`, copying all bytes from
/// `src`. If `src` is empty, allocates 1 byte (a sentinel) so the returned
/// pointer is always valid for a subsequent `libc::free`. Returns null on
/// allocation failure.
///
/// Null handling is the caller's responsibility.
#[must_use]
pub fn malloc_bytes(src: &[u8]) -> *mut u8 {
    let len = src.len();
    // Allocate at least 1 byte so malloc(0) implementation-defined behaviour
    // is avoided and the sentinel pointer can always be freed by the caller.
    // SAFETY: We request len.max(1) bytes from malloc; it returns a valid
    // pointer or null.
    let ptr = unsafe { libc::malloc(len.max(1)) }.cast::<u8>();
    if ptr.is_null() {
        return ptr;
    }
    if len > 0 {
        // SAFETY: src is a valid Rust slice of len bytes; ptr is freshly
        // allocated with len bytes, so both regions are valid and
        // non-overlapping.
        unsafe { std::ptr::copy_nonoverlapping(src.as_ptr(), ptr, len) };
    }
    ptr
}

/// Allocate a 1-byte sentinel via `libc::malloc`. Use this when a zero-length
/// allocation is needed but a non-null, freeable pointer is required (e.g. an
/// empty body buffer). Returns null on allocation failure.
///
/// Null handling is the caller's responsibility.
#[must_use]
pub fn malloc_empty() -> *mut u8 {
    // SAFETY: We request 1 byte from malloc; it returns a valid pointer or null.
    unsafe { libc::malloc(1) }.cast::<u8>()
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

/// Extract a NUL-terminated C string pointer into an owned `String`, replacing
/// invalid UTF-8 sequences with U+FFFD.
///
/// Returns an empty string when `ptr` is null.
///
/// # Safety
///
/// If non-null, `ptr` must point to a valid NUL-terminated C string.
#[must_use]
pub unsafe fn cstr_to_string_lossy(ptr: *const c_char) -> String {
    if ptr.is_null() {
        return String::new();
    }
    // SAFETY: Caller guarantees ptr is a valid NUL-terminated C string.
    unsafe { CStr::from_ptr(ptr) }
        .to_string_lossy()
        .into_owned()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::c_void;

    /// Helper: free a malloc'd pointer (reduces boilerplate).
    unsafe fn free_ptr(ptr: *mut c_char) {
        // SAFETY: `ptr` was allocated by a C allocator (malloc via `malloc_cstring`) and is being freed with the matching `libc::free`.
        unsafe { libc::free(ptr.cast::<c_void>()) };
    }

    // ── malloc_cstring ───────────────────────────────────────────────────

    #[test]
    fn malloc_cstring_copies_exact_bytes() {
        let src = b"ABC";
        // SAFETY: src points to 3 valid bytes.
        let ptr = unsafe { malloc_cstring(src.as_ptr(), src.len()) };
        assert!(!ptr.is_null());
        // Verify each byte individually, plus the NUL terminator.
        let raw = ptr.cast::<u8>();
        // SAFETY: ptr is freshly allocated with 4 bytes (3 + NUL).
        unsafe {
            assert_eq!(*raw, b'A');
            assert_eq!(*raw.add(1), b'B');
            assert_eq!(*raw.add(2), b'C');
            assert_eq!(*raw.add(3), 0u8, "missing NUL terminator");
            free_ptr(ptr);
        }
    }

    #[test]
    fn malloc_cstring_zero_length_produces_empty_cstr() {
        // SAFETY: len=0 means src is never read.
        let ptr = unsafe { malloc_cstring(std::ptr::null(), 0) };
        assert!(!ptr.is_null());
        // SAFETY: ptr is a valid NUL-terminated string of length 0.
        unsafe {
            assert_eq!(*ptr.cast::<u8>(), 0u8, "should be only a NUL byte");
            let recovered = CStr::from_ptr(ptr).to_str().unwrap();
            assert_eq!(recovered, "");
            free_ptr(ptr);
        }
    }

    #[test]
    fn malloc_cstring_single_byte() {
        let src = b"X";
        // SAFETY: src points to 1 valid byte.
        let ptr = unsafe { malloc_cstring(src.as_ptr(), 1) };
        assert!(!ptr.is_null());
        // SAFETY: ptr is a freshly allocated 2-byte buffer.
        unsafe {
            let recovered = CStr::from_ptr(ptr).to_str().unwrap();
            assert_eq!(recovered, "X");
            free_ptr(ptr);
        }
    }

    // ── str_to_malloc ────────────────────────────────────────────────────

    #[test]
    fn str_to_malloc_roundtrip() {
        let input = "hello, hew!";
        let ptr = str_to_malloc(input);
        assert!(!ptr.is_null());
        // SAFETY: ptr was just allocated by str_to_malloc with a NUL terminator.
        let recovered = unsafe { CStr::from_ptr(ptr) }.to_str().unwrap();
        assert_eq!(recovered, input);
        // SAFETY: ptr was allocated with libc::malloc.
        unsafe { free_ptr(ptr) };
    }

    #[test]
    fn str_to_malloc_empty() {
        let ptr = str_to_malloc("");
        assert!(!ptr.is_null());
        // SAFETY: ptr was just allocated by str_to_malloc with a NUL terminator.
        let recovered = unsafe { CStr::from_ptr(ptr) }.to_str().unwrap();
        assert_eq!(recovered, "");
        // SAFETY: ptr was allocated with libc::malloc.
        unsafe { free_ptr(ptr) };
    }

    #[test]
    fn str_to_malloc_multibyte_utf8() {
        // Covers multi-byte sequences: 2-byte (é), 3-byte (€), 4-byte (🍁).
        let input = "café €42 🍁";
        let ptr = str_to_malloc(input);
        assert!(!ptr.is_null());
        // SAFETY: ptr is a valid NUL-terminated string.
        unsafe {
            let recovered = CStr::from_ptr(ptr).to_str().unwrap();
            assert_eq!(recovered, input);
            assert_eq!(recovered.len(), input.len(), "byte length must match");
            free_ptr(ptr);
        }
    }

    #[test]
    fn str_to_malloc_embedded_nul_bytes_are_not_special() {
        // Rust &str can't contain NUL, but we verify the contract: the full
        // byte content of the str is copied and the result is NUL-terminated.
        let input = "ab";
        let ptr = str_to_malloc(input);
        assert!(!ptr.is_null());
        // SAFETY: ptr is a freshly allocated 3-byte buffer ("ab\0").
        unsafe {
            let raw = ptr.cast::<u8>();
            assert_eq!(*raw, b'a');
            assert_eq!(*raw.add(1), b'b');
            assert_eq!(*raw.add(2), 0u8);
            free_ptr(ptr);
        }
    }

    #[test]
    fn str_to_malloc_long_string() {
        // Exercise a longer allocation to catch off-by-one in the copy.
        let input: String = "hew".repeat(1000);
        let ptr = str_to_malloc(&input);
        assert!(!ptr.is_null());
        // SAFETY: ptr is a valid NUL-terminated string.
        unsafe {
            let recovered = CStr::from_ptr(ptr).to_str().unwrap();
            assert_eq!(recovered.len(), 3000);
            assert_eq!(recovered, input);
            free_ptr(ptr);
        }
    }

    // ── malloc_bytes ─────────────────────────────────────────────────────

    #[test]
    fn malloc_bytes_empty_returns_nonnull_and_freeable() {
        let ptr = malloc_bytes(&[]);
        assert!(!ptr.is_null(), "empty slice must yield a non-null sentinel");
        // SAFETY: ptr was allocated by malloc_bytes via libc::malloc.
        unsafe { libc::free(ptr.cast::<c_void>()) };
    }

    #[test]
    fn malloc_bytes_roundtrips_1kb_buffer() {
        // Build a 1 KiB buffer with a known pattern.
        let src: Vec<u8> = (0u8..=255).cycle().take(1024).collect();
        let ptr = malloc_bytes(&src);
        assert!(!ptr.is_null());
        // SAFETY: ptr was allocated with 1024 bytes by malloc_bytes.
        unsafe {
            for (i, &expected) in src.iter().enumerate() {
                assert_eq!(*ptr.add(i), expected, "byte mismatch at index {i}");
            }
            libc::free(ptr.cast::<c_void>());
        }
    }

    #[test]
    fn malloc_bytes_single_byte_roundtrip() {
        let src = [0x42u8];
        let ptr = malloc_bytes(&src);
        assert!(!ptr.is_null());
        // SAFETY: ptr points to 1 byte allocated by malloc_bytes.
        unsafe {
            assert_eq!(*ptr, 0x42);
            libc::free(ptr.cast::<c_void>());
        }
    }

    // ── malloc_empty ─────────────────────────────────────────────────────

    #[test]
    fn malloc_empty_returns_nonnull_and_freeable() {
        let ptr = malloc_empty();
        assert!(
            !ptr.is_null(),
            "malloc_empty must return a non-null sentinel"
        );
        // SAFETY: ptr was allocated by malloc_empty via libc::malloc.
        unsafe { libc::free(ptr.cast::<c_void>()) };
    }

    // ── cstr_to_str ──────────────────────────────────────────────────────

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

    #[test]
    fn cstr_to_str_empty_cstring() {
        let s = c"";
        // SAFETY: s is a valid (empty) C string literal.
        let result = unsafe { cstr_to_str(s.as_ptr()) };
        assert_eq!(result, Some(""));
    }

    #[test]
    fn cstr_to_str_invalid_utf8_returns_none() {
        // 0xFF is not valid in any UTF-8 sequence.
        let bytes: &[u8] = &[0xFF, 0xFE, 0x00];
        // SAFETY: bytes is a NUL-terminated buffer; invalid UTF-8 is intentional.
        let result = unsafe { cstr_to_str(bytes.as_ptr().cast::<c_char>()) };
        assert_eq!(result, None, "invalid UTF-8 should produce None");
    }

    #[test]
    fn cstr_to_str_multibyte_utf8() {
        let s = c"héllo 🍁";
        // SAFETY: s is a valid C string literal.
        let result = unsafe { cstr_to_str(s.as_ptr()) };
        assert_eq!(result, Some("héllo 🍁"));
    }

    // ── cstr_to_string_lossy ───────────────────────────────────────────────

    #[test]
    fn cstr_to_string_lossy_null_returns_empty_string() {
        // SAFETY: Passing null is the scenario under test.
        let result = unsafe { cstr_to_string_lossy(std::ptr::null()) };
        assert!(result.is_empty());
    }

    #[test]
    fn cstr_to_string_lossy_valid_utf8_preserves_text() {
        let s = c"metadata";
        // SAFETY: s is a valid C string literal.
        let result = unsafe { cstr_to_string_lossy(s.as_ptr()) };
        assert_eq!(result, "metadata");
    }

    #[test]
    fn cstr_to_string_lossy_invalid_utf8_replaces_bad_bytes() {
        let bytes: &[u8] = &[b'f', b'o', 0x80, 0x00];
        // SAFETY: bytes is a NUL-terminated buffer; invalid UTF-8 is intentional.
        let result = unsafe { cstr_to_string_lossy(bytes.as_ptr().cast::<c_char>()) };
        assert_eq!(result, "fo�");
    }

    // ── roundtrip: str_to_malloc → cstr_to_str ───────────────────────────

    #[test]
    fn str_to_malloc_then_cstr_to_str_roundtrip() {
        let original = "roundtrip through the ABI boundary";
        let ptr = str_to_malloc(original);
        assert!(!ptr.is_null());
        // SAFETY: ptr is a valid NUL-terminated C string from str_to_malloc.
        unsafe {
            let recovered = cstr_to_str(ptr);
            assert_eq!(recovered, Some(original));
            free_ptr(ptr);
        }
    }

    #[test]
    fn roundtrip_empty_string() {
        let ptr = str_to_malloc("");
        assert!(!ptr.is_null());
        // SAFETY: ptr is a valid NUL-terminated C string from str_to_malloc.
        unsafe {
            let recovered = cstr_to_str(ptr);
            assert_eq!(recovered, Some(""));
            free_ptr(ptr);
        }
    }

    #[test]
    fn roundtrip_unicode() {
        let original = "日本語テスト 🎵";
        let ptr = str_to_malloc(original);
        assert!(!ptr.is_null());
        // SAFETY: ptr is a valid NUL-terminated C string from str_to_malloc.
        unsafe {
            let recovered = cstr_to_str(ptr);
            assert_eq!(recovered, Some(original));
            free_ptr(ptr);
        }
    }
}
