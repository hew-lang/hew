//! Hew std::encoding::hex — hexadecimal encoding and decoding.
//!
//! Provides hex encoding (lowercase and uppercase) and decoding for compiled
//! Hew programs. All returned buffers are allocated with `libc::malloc` so
//! callers can free them with [`hew_hex_free`].

use std::ffi::{c_void, CStr};
use std::os::raw::c_char;

/// Encode binary data to a lowercase hexadecimal string.
///
/// Returns a `malloc`-allocated, NUL-terminated C string. The caller must free
/// it with [`hew_hex_free`]. Returns null if `data` is null and `len > 0`,
/// or on allocation failure.
///
/// # Safety
///
/// `data` must point to at least `len` readable bytes, or be null when `len`
/// is 0.
#[no_mangle]
pub unsafe extern "C" fn hew_hex_encode(data: *const u8, len: usize) -> *mut c_char {
    // SAFETY: Caller guarantees data is valid for len bytes; forwarding to
    // encode_impl.
    unsafe { encode_impl(data, len, false) }
}

/// Encode binary data to an uppercase hexadecimal string.
///
/// Returns a `malloc`-allocated, NUL-terminated C string. The caller must free
/// it with [`hew_hex_free`]. Returns null if `data` is null and `len > 0`,
/// or on allocation failure.
///
/// # Safety
///
/// `data` must point to at least `len` readable bytes, or be null when `len`
/// is 0.
#[no_mangle]
pub unsafe extern "C" fn hew_hex_encode_upper(data: *const u8, len: usize) -> *mut c_char {
    // SAFETY: Caller guarantees data is valid for len bytes; forwarding to
    // encode_impl.
    unsafe { encode_impl(data, len, true) }
}

/// Shared encode implementation for lowercase and uppercase hex.
///
/// # Safety
///
/// `data` must point to at least `len` readable bytes when `len > 0`.
unsafe fn encode_impl(data: *const u8, len: usize, upper: bool) -> *mut c_char {
    if data.is_null() && len > 0 {
        return std::ptr::null_mut();
    }
    let slice = if len == 0 {
        &[]
    } else {
        // SAFETY: Caller guarantees data is valid for len bytes.
        unsafe { std::slice::from_raw_parts(data, len) }
    };
    let encoded = if upper {
        hex::encode_upper(slice)
    } else {
        hex::encode(slice)
    };
    let encoded_len = encoded.len();

    // SAFETY: We request encoded_len+1 bytes from malloc.
    let ptr = unsafe { libc::malloc(encoded_len + 1) }.cast::<u8>();
    if ptr.is_null() {
        return std::ptr::null_mut();
    }
    if encoded_len > 0 {
        // SAFETY: ptr is freshly allocated with encoded_len+1 bytes; encoded
        // bytes are valid and non-overlapping with the malloc'd region.
        unsafe { std::ptr::copy_nonoverlapping(encoded.as_ptr(), ptr, encoded_len) };
    }
    // SAFETY: ptr + encoded_len is within the allocated region.
    unsafe { *ptr.add(encoded_len) = 0 };
    ptr.cast::<c_char>()
}

/// Decode a hexadecimal string to binary data.
///
/// Returns a `malloc`-allocated buffer and writes its length to `out_len`. The
/// caller must free the buffer with [`hew_hex_free`]. Returns null on
/// invalid input, null arguments, or allocation failure.
///
/// # Safety
///
/// `s` must be a valid NUL-terminated C string (or null). `out_len` must point
/// to a writable `usize` (or be null, in which case the call returns null).
#[no_mangle]
pub unsafe extern "C" fn hew_hex_decode(s: *const c_char, out_len: *mut usize) -> *mut u8 {
    if s.is_null() || out_len.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: s is a valid NUL-terminated C string per caller contract.
    let Ok(rust_str) = unsafe { CStr::from_ptr(s) }.to_str() else {
        return std::ptr::null_mut();
    };
    let Ok(decoded) = hex::decode(rust_str) else {
        return std::ptr::null_mut();
    };
    let decoded_len = decoded.len();

    let alloc_size = if decoded_len == 0 { 1 } else { decoded_len };
    // SAFETY: We request alloc_size bytes from malloc; alloc_size >= 1, avoiding
    // implementation-defined malloc(0).
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

/// Free a buffer previously returned by [`hew_hex_encode`],
/// [`hew_hex_encode_upper`], or [`hew_hex_decode`].
///
/// # Safety
///
/// `ptr` must be a pointer previously returned by one of the `hew_hex_*`
/// functions, and must not have been freed already. Null is accepted (no-op).
#[no_mangle]
pub unsafe extern "C" fn hew_hex_free(ptr: *mut c_void) {
    if ptr.is_null() {
        return;
    }
    // SAFETY: ptr was allocated with libc::malloc in encode_impl or
    // hew_hex_decode.
    unsafe { libc::free(ptr) };
}

// ---------------------------------------------------------------------------
// HewVec-ABI wrappers (used by std/encoding/hex/hex.hew)
// ---------------------------------------------------------------------------

/// Encode a `bytes` HewVec to a lowercase hex string.
///
/// # Safety
///
/// `v` must be a valid, non-null pointer to a HewVec (i32 elements).
#[no_mangle]
pub unsafe extern "C" fn hew_hex_encode_hew(v: *mut hew_cabi::vec::HewVec) -> *mut c_char {
    // SAFETY: v validity forwarded to hwvec_to_u8.
    let bytes = unsafe { hew_cabi::vec::hwvec_to_u8(v) };
    // SAFETY: bytes slice is valid for its length.
    unsafe { hew_hex_encode(bytes.as_ptr(), bytes.len()) }
}

/// Encode a `bytes` HewVec to an uppercase hex string.
///
/// # Safety
///
/// `v` must be a valid, non-null pointer to a HewVec (i32 elements).
#[no_mangle]
pub unsafe extern "C" fn hew_hex_encode_upper_hew(v: *mut hew_cabi::vec::HewVec) -> *mut c_char {
    // SAFETY: v validity forwarded to hwvec_to_u8.
    let bytes = unsafe { hew_cabi::vec::hwvec_to_u8(v) };
    // SAFETY: bytes slice is valid for its length.
    unsafe { hew_hex_encode_upper(bytes.as_ptr(), bytes.len()) }
}

/// Decode a hex string to a `bytes` HewVec.
///
/// Returns an empty HewVec on invalid input.
///
/// # Safety
///
/// `s` must be a valid NUL-terminated C string (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_hex_decode_hew(s: *const c_char) -> *mut hew_cabi::vec::HewVec {
    if s.is_null() {
        // SAFETY: hew_vec_new allocates a valid empty HewVec.
        return unsafe { hew_cabi::vec::hew_vec_new() };
    }
    let mut out_len: usize = 0;
    // SAFETY: s is a valid C string; out_len is a writable usize.
    let ptr = unsafe { hew_hex_decode(s, &mut out_len) };
    if ptr.is_null() {
        // SAFETY: hew_vec_new allocates a valid empty HewVec.
        return unsafe { hew_cabi::vec::hew_vec_new() };
    }
    // SAFETY: ptr is valid for out_len bytes; returned by hew_hex_decode.
    let slice = unsafe { std::slice::from_raw_parts(ptr, out_len) };
    // SAFETY: slice is valid.
    let result = unsafe { hew_cabi::vec::u8_to_hwvec(slice) };
    // SAFETY: ptr was allocated by hew_hex_decode.
    unsafe { hew_hex_free(ptr.cast::<std::ffi::c_void>()) };
    result
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CString;

    #[test]
    fn encode_decode_roundtrip() {
        let input = b"Hello, Hew!";
        // SAFETY: input.as_ptr() is valid for input.len() bytes.
        let encoded = unsafe { hew_hex_encode(input.as_ptr(), input.len()) };
        assert!(!encoded.is_null());

        // SAFETY: encoded is a valid NUL-terminated string.
        let encoded_str = unsafe { CStr::from_ptr(encoded) }.to_str().unwrap();
        assert_eq!(encoded_str, "48656c6c6f2c2048657721");

        let mut out_len: usize = 0;
        // SAFETY: encoded is a valid C string; out_len is a valid writable pointer.
        let decoded = unsafe { hew_hex_decode(encoded, &mut out_len) };
        assert!(!decoded.is_null());
        assert_eq!(out_len, input.len());
        // SAFETY: decoded is valid for out_len bytes.
        let decoded_slice = unsafe { std::slice::from_raw_parts(decoded, out_len) };
        assert_eq!(decoded_slice, input);

        // SAFETY: pointers were allocated by hew_hex_encode/decode.
        unsafe {
            hew_hex_free(encoded.cast());
            hew_hex_free(decoded.cast());
        };
    }

    #[test]
    fn upper_encoding() {
        let input: &[u8] = &[0xde, 0xad, 0xbe, 0xef];
        // SAFETY: input.as_ptr() is valid for input.len() bytes.
        let lower = unsafe { hew_hex_encode(input.as_ptr(), input.len()) };
        // SAFETY: input.as_ptr() is valid for input.len() bytes.
        let upper = unsafe { hew_hex_encode_upper(input.as_ptr(), input.len()) };

        // SAFETY: both are valid NUL-terminated strings.
        let lower_str = unsafe { CStr::from_ptr(lower) }.to_str().unwrap();
        let upper_str = unsafe { CStr::from_ptr(upper) }.to_str().unwrap();

        assert_eq!(lower_str, "deadbeef");
        assert_eq!(upper_str, "DEADBEEF");

        // SAFETY: pointers were allocated by hew_hex_encode*.
        unsafe {
            hew_hex_free(lower.cast());
            hew_hex_free(upper.cast());
        };
    }

    #[test]
    fn decode_invalid_input() {
        let invalid = CString::new("zzzz").unwrap();
        let mut out_len: usize = 0;
        // SAFETY: invalid.as_ptr() is a valid C string; out_len is writable.
        let result = unsafe { hew_hex_decode(invalid.as_ptr(), &mut out_len) };
        assert!(result.is_null());

        // Odd-length hex is also invalid.
        let odd = CString::new("abc").unwrap();
        // SAFETY: odd.as_ptr() is a valid C string; out_len is writable.
        let result = unsafe { hew_hex_decode(odd.as_ptr(), &mut out_len) };
        assert!(result.is_null());
    }

    #[test]
    fn null_handling() {
        // SAFETY: null data with len > 0 is explicitly handled by hew_hex_encode.
        let result = unsafe { hew_hex_encode(std::ptr::null(), 10) };
        assert!(result.is_null());

        // SAFETY: null string is explicitly handled by hew_hex_decode.
        let mut out_len: usize = 0;
        let result = unsafe { hew_hex_decode(std::ptr::null(), &mut out_len) };
        assert!(result.is_null());

        // SAFETY: null out_len is explicitly handled by hew_hex_decode.
        let s = CString::new("deadbeef").unwrap();
        let result = unsafe { hew_hex_decode(s.as_ptr(), std::ptr::null_mut()) };
        assert!(result.is_null());

        // SAFETY: null is explicitly accepted as a no-op by hew_hex_free.
        unsafe { hew_hex_free(std::ptr::null_mut()) };
    }

    #[test]
    fn empty_input() {
        // Encode empty slice.
        // SAFETY: null with len 0 is handled.
        let encoded = unsafe { hew_hex_encode(std::ptr::null(), 0) };
        assert!(!encoded.is_null());
        // SAFETY: encoded is a valid NUL-terminated string.
        let encoded_str = unsafe { CStr::from_ptr(encoded) }.to_str().unwrap();
        assert_eq!(encoded_str, "");
        // SAFETY: pointer was allocated by hew_hex_encode.
        unsafe { hew_hex_free(encoded.cast()) };

        // Decode empty string.
        let empty = CString::new("").unwrap();
        let mut out_len: usize = 999;
        // SAFETY: empty.as_ptr() is a valid C string; out_len is writable.
        let decoded = unsafe { hew_hex_decode(empty.as_ptr(), &mut out_len) };
        assert!(!decoded.is_null());
        assert_eq!(out_len, 0);
        // SAFETY: pointer was allocated by hew_hex_decode.
        unsafe { hew_hex_free(decoded.cast()) };
    }
}
