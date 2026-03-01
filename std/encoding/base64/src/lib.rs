//! Hew runtime: `base64_enc` module.
//!
//! Provides Base64 encoding and decoding (standard and URL-safe) for compiled
//! Hew programs. All returned buffers are allocated with `libc::malloc` so
//! callers can free them with [`hew_base64_free`].

use std::ffi::{c_char, c_void};

use base64::engine::general_purpose::{STANDARD, URL_SAFE};
use base64::Engine as _;

/// Encode binary data to a standard Base64 string.
///
/// Returns a `malloc`-allocated, NUL-terminated C string. The caller must free
/// it with [`hew_base64_free`]. Returns null if `data` is null and `len > 0`,
/// or on allocation failure.
///
/// # Safety
///
/// `data` must point to at least `len` readable bytes, or be null when `len`
/// is 0.
#[no_mangle]
pub unsafe extern "C" fn hew_base64_encode(data: *const u8, len: usize) -> *mut c_char {
    // SAFETY: Caller guarantees data is valid for len bytes; forwarding contract
    // to encode_with_engine.
    unsafe { encode_with_engine(data, len, &STANDARD) }
}

/// Encode binary data to a URL-safe Base64 string.
///
/// Returns a `malloc`-allocated, NUL-terminated C string. The caller must free
/// it with [`hew_base64_free`]. Returns null if `data` is null and `len > 0`,
/// or on allocation failure.
///
/// # Safety
///
/// `data` must point to at least `len` readable bytes, or be null when `len`
/// is 0.
#[no_mangle]
pub unsafe extern "C" fn hew_base64_encode_url(data: *const u8, len: usize) -> *mut c_char {
    // SAFETY: Caller guarantees data is valid for len bytes; forwarding contract
    // to encode_with_engine.
    unsafe { encode_with_engine(data, len, &URL_SAFE) }
}

/// Shared encode implementation for both standard and URL-safe engines.
///
/// # Safety
///
/// `data` must point to at least `len` readable bytes when `len > 0`.
unsafe fn encode_with_engine(
    data: *const u8,
    len: usize,
    engine: &impl base64::Engine,
) -> *mut c_char {
    if data.is_null() && len > 0 {
        return std::ptr::null_mut();
    }
    let slice = if len == 0 {
        &[]
    } else {
        // SAFETY: Caller guarantees data is valid for len bytes.
        unsafe { std::slice::from_raw_parts(data, len) }
    };
    let encoded = engine.encode(slice);
    hew_cabi::cabi::str_to_malloc(&encoded)
}

/// Decode a Base64 string to binary data.
///
/// Returns a `malloc`-allocated buffer and writes its length to `out_len`. The
/// caller must free the buffer with [`hew_base64_free`]. Returns null on
/// invalid input, null arguments, or allocation failure.
///
/// # Safety
///
/// `s` must be a valid NUL-terminated C string (or null). `out_len` must point
/// to a writable `usize` (or be null, in which case the call returns null).
#[no_mangle]
pub unsafe extern "C" fn hew_base64_decode(s: *const c_char, out_len: *mut usize) -> *mut u8 {
    if out_len.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: If non-null, s is a valid NUL-terminated C string per caller contract.
    let Some(rust_str) = (unsafe { hew_cabi::cabi::cstr_to_str(s) }) else {
        return std::ptr::null_mut();
    };
    let Ok(decoded) = STANDARD.decode(rust_str) else {
        return std::ptr::null_mut();
    };
    let decoded_len = decoded.len();

    let alloc_size = if decoded_len == 0 { 1 } else { decoded_len };
    // SAFETY: We request alloc_size bytes from malloc; it returns a valid
    // pointer or null. alloc_size >= 1, avoiding implementation-defined malloc(0).
    let ptr = unsafe { libc::malloc(alloc_size) }.cast::<u8>();
    if ptr.is_null() {
        return std::ptr::null_mut();
    }
    if decoded_len > 0 {
        // SAFETY: ptr is freshly allocated with at least decoded_len bytes;
        // decoded bytes are valid and non-overlapping.
        unsafe { std::ptr::copy_nonoverlapping(decoded.as_ptr(), ptr, decoded_len) };
    }
    // SAFETY: out_len is a valid writable pointer per caller contract.
    unsafe { *out_len = decoded_len };
    ptr
}

/// Free a buffer previously returned by [`hew_base64_encode`],
/// [`hew_base64_encode_url`], or [`hew_base64_decode`].
///
/// # Safety
///
/// `ptr` must be a pointer previously returned by one of the `hew_base64_*`
/// functions, and must not have been freed already. Null is accepted (no-op).
#[no_mangle]
pub unsafe extern "C" fn hew_base64_free(ptr: *mut c_void) {
    if ptr.is_null() {
        return;
    }
    // SAFETY: ptr was allocated with libc::malloc (via str_to_malloc in
    // encode_with_engine, or directly in hew_base64_decode).
    unsafe { libc::free(ptr) };
}

#[cfg(test)]
extern crate hew_runtime; // Link hew_vec_* symbol implementations
#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::{CStr, CString};

    #[test]
    fn test_encode_decode_roundtrip() {
        let input = b"Hello, Hew!";
        // SAFETY: input.as_ptr() is valid for input.len() bytes.
        let encoded = unsafe { hew_base64_encode(input.as_ptr(), input.len()) };
        assert!(!encoded.is_null());

        // SAFETY: encoded is a valid NUL-terminated string.
        let encoded_str = unsafe { CStr::from_ptr(encoded) }.to_str().unwrap();
        assert_eq!(encoded_str, "SGVsbG8sIEhldyE=");

        let mut out_len: usize = 0;
        // SAFETY: encoded is a valid C string; out_len is a valid writable pointer.
        let decoded = unsafe { hew_base64_decode(encoded, &mut out_len) };
        assert!(!decoded.is_null());
        assert_eq!(out_len, input.len());
        // SAFETY: decoded is valid for out_len bytes.
        let decoded_slice = unsafe { std::slice::from_raw_parts(decoded, out_len) };
        assert_eq!(decoded_slice, input);

        // SAFETY: pointers were allocated by hew_base64_encode/decode.
        unsafe {
            hew_base64_free(encoded.cast());
            hew_base64_free(decoded.cast());
        };
    }

    #[test]
    fn test_url_safe_encoding() {
        // Bytes that produce +/ in standard but -_ in URL-safe.
        let input: &[u8] = &[0xfb, 0xff, 0xfe];
        // SAFETY: input.as_ptr() is valid for input.len() bytes.
        let standard = unsafe { hew_base64_encode(input.as_ptr(), input.len()) };
        // SAFETY: input.as_ptr() is valid for input.len() bytes.
        let url_safe = unsafe { hew_base64_encode_url(input.as_ptr(), input.len()) };

        // SAFETY: both are valid NUL-terminated strings.
        let std_str = unsafe { CStr::from_ptr(standard) }.to_str().unwrap();
        let url_str = unsafe { CStr::from_ptr(url_safe) }.to_str().unwrap();

        assert!(std_str.contains('+') || std_str.contains('/'));
        assert!(!url_str.contains('+') && !url_str.contains('/'));

        // SAFETY: pointers were allocated by hew_base64_encode*.
        unsafe {
            hew_base64_free(standard.cast());
            hew_base64_free(url_safe.cast());
        };
    }

    #[test]
    fn test_decode_invalid_input() {
        let invalid = CString::new("!!!not-base64!!!").unwrap();
        let mut out_len: usize = 0;
        // SAFETY: invalid.as_ptr() is a valid C string; out_len is writable.
        let result = unsafe { hew_base64_decode(invalid.as_ptr(), &mut out_len) };
        assert!(result.is_null());
    }

    #[test]
    fn test_null_handling() {
        // SAFETY: null data with len > 0 is explicitly handled by hew_base64_encode.
        let result = unsafe { hew_base64_encode(std::ptr::null(), 10) };
        assert!(result.is_null());

        // SAFETY: null string is explicitly handled by hew_base64_decode.
        let mut out_len: usize = 0;
        let result = unsafe { hew_base64_decode(std::ptr::null(), &mut out_len) };
        assert!(result.is_null());

        // SAFETY: null out_len is explicitly handled by hew_base64_decode.
        let s = CString::new("SGVsbG8=").unwrap();
        let result = unsafe { hew_base64_decode(s.as_ptr(), std::ptr::null_mut()) };
        assert!(result.is_null());

        // SAFETY: null is explicitly accepted as a no-op by hew_base64_free.
        unsafe { hew_base64_free(std::ptr::null_mut()) };
    }
}

// ---------------------------------------------------------------------------
// HewVec-ABI wrappers (used by std/base64.hew)
// ---------------------------------------------------------------------------

/// Encode a `bytes` `HewVec` to a standard Base64 string.
///
/// # Safety
///
/// `v` must be a valid, non-null pointer to a `HewVec` (i32 elements).
#[no_mangle]
pub unsafe extern "C" fn hew_base64_encode_hew(v: *mut hew_cabi::vec::HewVec) -> *mut c_char {
    // SAFETY: v validity forwarded to hwvec_to_u8.
    let bytes = unsafe { hew_cabi::vec::hwvec_to_u8(v) };
    // SAFETY: bytes slice is valid for its length.
    unsafe { hew_base64_encode(bytes.as_ptr(), bytes.len()) }
}

/// Encode a `bytes` `HewVec` to a URL-safe Base64 string.
///
/// # Safety
///
/// `v` must be a valid, non-null pointer to a `HewVec` (i32 elements).
#[no_mangle]
pub unsafe extern "C" fn hew_base64_encode_url_hew(v: *mut hew_cabi::vec::HewVec) -> *mut c_char {
    // SAFETY: v validity forwarded to hwvec_to_u8.
    let bytes = unsafe { hew_cabi::vec::hwvec_to_u8(v) };
    // SAFETY: bytes slice is valid for its length.
    unsafe { hew_base64_encode_url(bytes.as_ptr(), bytes.len()) }
}

/// Decode a Base64 string to a `bytes` `HewVec`.
///
/// Returns an empty `HewVec` on invalid input.
///
/// # Safety
///
/// `s` must be a valid NUL-terminated C string (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_base64_decode_hew(s: *const c_char) -> *mut hew_cabi::vec::HewVec {
    if s.is_null() {
        // SAFETY: hew_vec_new allocates a valid empty HewVec.
        return unsafe { hew_cabi::vec::hew_vec_new() };
    }
    let mut out_len: usize = 0;
    // SAFETY: s is a valid C string; out_len is a writable usize.
    let ptr = unsafe { hew_base64_decode(s, &raw mut out_len) };
    if ptr.is_null() {
        // SAFETY: hew_vec_new allocates a valid empty HewVec.
        return unsafe { hew_cabi::vec::hew_vec_new() };
    }
    // SAFETY: ptr is valid for out_len bytes; returned by hew_base64_decode.
    let slice = unsafe { std::slice::from_raw_parts(ptr, out_len) };
    // SAFETY: slice is valid.
    let result = unsafe { hew_cabi::vec::u8_to_hwvec(slice) };
    // SAFETY: ptr was allocated by hew_base64_decode.
    unsafe { hew_base64_free(ptr.cast::<c_void>()) };
    result
}
