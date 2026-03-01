//! Hew runtime: `msgpack` module.
//!
//! Provides `MessagePack` encoding and decoding for compiled Hew programs.
//! Uses `rmp_serde` with `serde_json::Value` as the intermediate type for
//! JSON↔`MessagePack` conversion. All returned buffers are allocated with
//! `libc::malloc` and must be freed with [`hew_msgpack_free`]. All returned
//! strings are allocated with `libc::malloc` and NUL-terminated.

use hew_cabi::cabi::{cstr_to_str, str_to_malloc};
use std::ffi::c_char;

/// Allocate a buffer via `libc::malloc`, copying `len` bytes from `src`.
/// Returns null on allocation failure.
///
/// # Safety
///
/// `src` must point to at least `len` readable bytes.
unsafe fn malloc_buf(src: *const u8, len: usize) -> *mut u8 {
    if len == 0 {
        return std::ptr::null_mut();
    }
    // SAFETY: We request len bytes from malloc; it returns a valid pointer or null.
    let ptr = unsafe { libc::malloc(len) }.cast::<u8>();
    if ptr.is_null() {
        return ptr;
    }
    // SAFETY: Caller guarantees src is valid for len bytes; ptr is freshly
    // allocated with len bytes, so both regions are valid and non-overlapping.
    unsafe { std::ptr::copy_nonoverlapping(src, ptr, len) };
    ptr
}

// ---------------------------------------------------------------------------
// C ABI exports
// ---------------------------------------------------------------------------

/// Convert a JSON string to `MessagePack` binary.
///
/// Parses the JSON string, serializes it as `MessagePack`, and returns a
/// `malloc`-allocated buffer. The length is written to `out_len`. Returns null
/// on parse error or serialization failure.
///
/// # Safety
///
/// `json_str` must be a valid NUL-terminated C string.
/// `out_len` must be a valid pointer to a `usize`.
#[no_mangle]
pub unsafe extern "C" fn hew_msgpack_from_json(
    json_str: *const c_char,
    out_len: *mut usize,
) -> *mut u8 {
    if json_str.is_null() || out_len.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: json_str is a valid NUL-terminated C string per caller contract.
    let Some(s) = (unsafe { cstr_to_str(json_str) }) else {
        return std::ptr::null_mut();
    };
    let Ok(value) = serde_json::from_str::<serde_json::Value>(s) else {
        return std::ptr::null_mut();
    };
    let Ok(bytes) = rmp_serde::to_vec(&value) else {
        return std::ptr::null_mut();
    };
    // SAFETY: out_len is a valid pointer per caller contract.
    unsafe { *out_len = bytes.len() };
    // SAFETY: bytes.as_ptr() is valid for bytes.len() bytes.
    unsafe { malloc_buf(bytes.as_ptr(), bytes.len()) }
}

/// Convert `MessagePack` binary to a JSON string.
///
/// Deserializes the `MessagePack` data into `serde_json::Value`, then serializes
/// it as JSON. Returns a `malloc`-allocated, NUL-terminated C string. Returns
/// null on deserialization or serialization failure.
///
/// # Safety
///
/// `data` must point to at least `len` readable bytes.
#[no_mangle]
pub unsafe extern "C" fn hew_msgpack_to_json(data: *const u8, len: usize) -> *mut c_char {
    if data.is_null() || len == 0 {
        return std::ptr::null_mut();
    }
    // SAFETY: data is valid for len bytes per caller contract.
    let slice = unsafe { std::slice::from_raw_parts(data, len) };
    let Ok(value) = rmp_serde::from_slice::<serde_json::Value>(slice) else {
        return std::ptr::null_mut();
    };
    let Ok(json) = serde_json::to_string(&value) else {
        return std::ptr::null_mut();
    };
    str_to_malloc(&json)
}

/// Encode a single integer as `MessagePack`.
///
/// Returns a `malloc`-allocated buffer. The length is written to `out_len`.
/// Returns null on encoding failure.
///
/// # Safety
///
/// `out_len` must be a valid pointer to a `usize`.
#[no_mangle]
pub unsafe extern "C" fn hew_msgpack_encode_int(val: i64, out_len: *mut usize) -> *mut u8 {
    if out_len.is_null() {
        return std::ptr::null_mut();
    }
    let Ok(bytes) = rmp_serde::to_vec(&val) else {
        return std::ptr::null_mut();
    };
    // SAFETY: out_len is a valid pointer per caller contract.
    unsafe { *out_len = bytes.len() };
    // SAFETY: bytes.as_ptr() is valid for bytes.len() bytes.
    unsafe { malloc_buf(bytes.as_ptr(), bytes.len()) }
}

/// Encode a single string as `MessagePack`.
///
/// Returns a `malloc`-allocated buffer. The length is written to `out_len`.
/// Returns null on encoding failure or invalid input.
///
/// # Safety
///
/// `s` must be a valid NUL-terminated C string.
/// `out_len` must be a valid pointer to a `usize`.
#[no_mangle]
pub unsafe extern "C" fn hew_msgpack_encode_string(
    s: *const c_char,
    out_len: *mut usize,
) -> *mut u8 {
    if s.is_null() || out_len.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: s is a valid NUL-terminated C string per caller contract.
    let Some(rust_str) = (unsafe { cstr_to_str(s) }) else {
        return std::ptr::null_mut();
    };
    let Ok(bytes) = rmp_serde::to_vec(rust_str) else {
        return std::ptr::null_mut();
    };
    // SAFETY: out_len is a valid pointer per caller contract.
    unsafe { *out_len = bytes.len() };
    // SAFETY: bytes.as_ptr() is valid for bytes.len() bytes.
    unsafe { malloc_buf(bytes.as_ptr(), bytes.len()) }
}

/// Encode a binary blob as `MessagePack`.
///
/// Returns a `malloc`-allocated buffer. The length is written to `out_len`.
/// Returns null on encoding failure or invalid input.
///
/// # Safety
///
/// `data` must point to at least `len` readable bytes.
/// `out_len` must be a valid pointer to a `usize`.
#[no_mangle]
pub unsafe extern "C" fn hew_msgpack_encode_bytes(
    data: *const u8,
    len: usize,
    out_len: *mut usize,
) -> *mut u8 {
    if data.is_null() || out_len.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: data is valid for len bytes per caller contract.
    let slice = unsafe { std::slice::from_raw_parts(data, len) };
    let Ok(bytes) = rmp_serde::to_vec(&slice.to_vec()) else {
        return std::ptr::null_mut();
    };
    // SAFETY: out_len is a valid pointer per caller contract.
    unsafe { *out_len = bytes.len() };
    // SAFETY: bytes.as_ptr() is valid for bytes.len() bytes.
    unsafe { malloc_buf(bytes.as_ptr(), bytes.len()) }
}

/// Free a buffer previously returned by any of the `hew_msgpack_*` functions.
///
/// # Safety
///
/// `ptr` must be a pointer previously returned by a `hew_msgpack_*` function,
/// and must not have been freed already.
#[no_mangle]
pub unsafe extern "C" fn hew_msgpack_free(ptr: *mut u8) {
    if ptr.is_null() {
        return;
    }
    // SAFETY: ptr was allocated with libc::malloc and has not been freed.
    unsafe { libc::free(ptr.cast()) };
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
extern crate hew_runtime; // Link hew_vec_* symbol implementations
#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::{CStr, CString};

    #[test]
    fn json_roundtrip_object() {
        let json = r#"{"name":"hew","version":42}"#;
        let c_json = CString::new(json).unwrap();
        let mut len: usize = 0;

        // SAFETY: c_json is a valid C string; len is a valid pointer.
        unsafe {
            let buf = hew_msgpack_from_json(c_json.as_ptr(), &mut len);
            assert!(!buf.is_null());
            assert!(len > 0);

            let result = hew_msgpack_to_json(buf, len);
            assert!(!result.is_null());
            // SAFETY: result is a valid NUL-terminated C string from malloc.
            let result_str = CStr::from_ptr(result).to_str().unwrap();
            let v1: serde_json::Value = serde_json::from_str(json).unwrap();
            let v2: serde_json::Value = serde_json::from_str(result_str).unwrap();
            assert_eq!(v1, v2);

            libc::free(result.cast());
            hew_msgpack_free(buf);
        }
    }

    #[test]
    fn json_roundtrip_array() {
        let json = r#"[1,2,3,"hello",true,null]"#;
        let c_json = CString::new(json).unwrap();
        let mut len: usize = 0;

        // SAFETY: c_json is a valid C string; len is a valid pointer.
        unsafe {
            let buf = hew_msgpack_from_json(c_json.as_ptr(), &mut len);
            assert!(!buf.is_null());

            let result = hew_msgpack_to_json(buf, len);
            assert!(!result.is_null());
            // SAFETY: result is a valid NUL-terminated C string from malloc.
            let result_str = CStr::from_ptr(result).to_str().unwrap();
            let v1: serde_json::Value = serde_json::from_str(json).unwrap();
            let v2: serde_json::Value = serde_json::from_str(result_str).unwrap();
            assert_eq!(v1, v2);

            libc::free(result.cast());
            hew_msgpack_free(buf);
        }
    }

    #[test]
    fn encode_int_roundtrip() {
        let mut len: usize = 0;

        // SAFETY: len is a valid pointer.
        unsafe {
            let buf = hew_msgpack_encode_int(42, &mut len);
            assert!(!buf.is_null());
            assert!(len > 0);

            // Decode and verify.
            let slice = std::slice::from_raw_parts(buf, len);
            let val: i64 = rmp_serde::from_slice(slice).unwrap();
            assert_eq!(val, 42);

            hew_msgpack_free(buf);
        }
    }

    #[test]
    fn encode_string_roundtrip() {
        let s = CString::new("hello msgpack").unwrap();
        let mut len: usize = 0;

        // SAFETY: s is a valid C string; len is a valid pointer.
        unsafe {
            let buf = hew_msgpack_encode_string(s.as_ptr(), &mut len);
            assert!(!buf.is_null());
            assert!(len > 0);

            // Decode and verify.
            let slice = std::slice::from_raw_parts(buf, len);
            let val: String = rmp_serde::from_slice(slice).unwrap();
            assert_eq!(val, "hello msgpack");

            hew_msgpack_free(buf);
        }
    }

    #[test]
    fn encode_bytes_roundtrip() {
        let data: [u8; 4] = [0xDE, 0xAD, 0xBE, 0xEF];
        let mut len: usize = 0;

        // SAFETY: data is a valid buffer; len is a valid pointer.
        unsafe {
            let buf = hew_msgpack_encode_bytes(data.as_ptr(), data.len(), &mut len);
            assert!(!buf.is_null());
            assert!(len > 0);

            // Decode and verify.
            let slice = std::slice::from_raw_parts(buf, len);
            let val: Vec<u8> = rmp_serde::from_slice(slice).unwrap();
            assert_eq!(val, data);

            hew_msgpack_free(buf);
        }
    }

    #[test]
    fn null_inputs_return_null() {
        let mut len: usize = 0;

        // SAFETY: Testing null-safety of all functions.
        unsafe {
            assert!(hew_msgpack_from_json(std::ptr::null(), &mut len).is_null());
            assert!(hew_msgpack_to_json(std::ptr::null(), 10).is_null());
            assert!(hew_msgpack_to_json([0u8].as_ptr(), 0).is_null());
            assert!(hew_msgpack_encode_int(1, std::ptr::null_mut()).is_null());
            assert!(hew_msgpack_encode_string(std::ptr::null(), &mut len).is_null());
            assert!(hew_msgpack_encode_bytes(std::ptr::null(), 5, &mut len).is_null());
        }
    }

    #[test]
    fn nested_json_roundtrip() {
        let json = r#"{"a":{"b":{"c":[1,2,3]}}}"#;
        let c_json = CString::new(json).unwrap();
        let mut len: usize = 0;

        // SAFETY: c_json is a valid C string; len is a valid pointer.
        unsafe {
            let buf = hew_msgpack_from_json(c_json.as_ptr(), &mut len);
            assert!(!buf.is_null());

            let result = hew_msgpack_to_json(buf, len);
            assert!(!result.is_null());
            // SAFETY: result is a valid NUL-terminated C string from malloc.
            let result_str = CStr::from_ptr(result).to_str().unwrap();
            let v1: serde_json::Value = serde_json::from_str(json).unwrap();
            let v2: serde_json::Value = serde_json::from_str(result_str).unwrap();
            assert_eq!(v1, v2);

            libc::free(result.cast());
            hew_msgpack_free(buf);
        }
    }
}

// ---------------------------------------------------------------------------
// HewVec-ABI wrappers (used by std/msgpack.hew)
// ---------------------------------------------------------------------------

/// Encode a JSON string to `MessagePack` bytes, returning a `bytes` `HewVec`.
///
/// Returns an empty `HewVec` on invalid JSON.
///
/// # Safety
///
/// `json` must be a valid NUL-terminated C string (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_msgpack_from_json_hew(
    json: *const c_char,
) -> *mut hew_cabi::vec::HewVec {
    if json.is_null() {
        // SAFETY: hew_vec_new allocates a valid empty HewVec.
        return unsafe { hew_cabi::vec::hew_vec_new() };
    }
    let mut out_len: usize = 0;
    // SAFETY: json is a valid C string; out_len is writable.
    let ptr = unsafe { hew_msgpack_from_json(json, &raw mut out_len) };
    if ptr.is_null() {
        // SAFETY: hew_vec_new allocates a valid empty HewVec.
        return unsafe { hew_cabi::vec::hew_vec_new() };
    }
    // SAFETY: ptr is valid for out_len bytes.
    let slice = unsafe { std::slice::from_raw_parts(ptr, out_len) };
    // SAFETY: slice is valid.
    let result = unsafe { hew_cabi::vec::u8_to_hwvec(slice) };
    // SAFETY: ptr was allocated by hew_msgpack_from_json.
    unsafe { hew_msgpack_free(ptr) };
    result
}

/// Decode a `bytes` `HewVec` of `MessagePack` data to a JSON string.
///
/// Returns an empty string on error.
///
/// # Safety
///
/// `v` must be a valid, non-null pointer to a `HewVec` (i32 elements).
#[no_mangle]
pub unsafe extern "C" fn hew_msgpack_to_json_hew(v: *mut hew_cabi::vec::HewVec) -> *mut c_char {
    // SAFETY: v validity forwarded to hwvec_to_u8.
    let bytes = unsafe { hew_cabi::vec::hwvec_to_u8(v) };
    // SAFETY: bytes slice is valid for its length.
    unsafe { hew_msgpack_to_json(bytes.as_ptr(), bytes.len()) }
}

/// Encode an i64 integer as a `MessagePack` varint, returning a `bytes` `HewVec`.
///
/// # Safety
///
/// None — all memory is managed by the runtime allocator.
#[no_mangle]
pub unsafe extern "C" fn hew_msgpack_encode_int_hew(val: i64) -> *mut hew_cabi::vec::HewVec {
    let mut out_len: usize = 0;
    // SAFETY: out_len is writable.
    let ptr = unsafe { hew_msgpack_encode_int(val, &raw mut out_len) };
    if ptr.is_null() {
        // SAFETY: hew_vec_new allocates a valid empty HewVec.
        return unsafe { hew_cabi::vec::hew_vec_new() };
    }
    // SAFETY: ptr is valid for out_len bytes.
    let slice = unsafe { std::slice::from_raw_parts(ptr, out_len) };
    // SAFETY: slice is valid.
    let result = unsafe { hew_cabi::vec::u8_to_hwvec(slice) };
    // SAFETY: ptr was allocated by hew_msgpack_encode_int.
    unsafe { hew_msgpack_free(ptr) };
    result
}

/// Encode a C string as `MessagePack` str, returning a `bytes` `HewVec`.
///
/// # Safety
///
/// `s` must be a valid NUL-terminated C string (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_msgpack_encode_string_hew(
    s: *const c_char,
) -> *mut hew_cabi::vec::HewVec {
    if s.is_null() {
        // SAFETY: hew_vec_new allocates a valid empty HewVec.
        return unsafe { hew_cabi::vec::hew_vec_new() };
    }
    let mut out_len: usize = 0;
    // SAFETY: s is a valid C string; out_len is writable.
    let ptr = unsafe { hew_msgpack_encode_string(s, &raw mut out_len) };
    if ptr.is_null() {
        // SAFETY: hew_vec_new allocates a valid empty HewVec.
        return unsafe { hew_cabi::vec::hew_vec_new() };
    }
    // SAFETY: ptr is valid for out_len bytes.
    let slice = unsafe { std::slice::from_raw_parts(ptr, out_len) };
    // SAFETY: slice is valid.
    let result = unsafe { hew_cabi::vec::u8_to_hwvec(slice) };
    // SAFETY: ptr was allocated by hew_msgpack_encode_string.
    unsafe { hew_msgpack_free(ptr) };
    result
}

/// Encode a `bytes` `HewVec` as a `MessagePack` bin, returning a `bytes` `HewVec`.
///
/// # Safety
///
/// `v` must be a valid, non-null pointer to a `HewVec` (i32 elements).
#[no_mangle]
pub unsafe extern "C" fn hew_msgpack_encode_bytes_hew(
    v: *mut hew_cabi::vec::HewVec,
) -> *mut hew_cabi::vec::HewVec {
    // SAFETY: v validity forwarded to hwvec_to_u8.
    let input = unsafe { hew_cabi::vec::hwvec_to_u8(v) };
    let mut out_len: usize = 0;
    // SAFETY: input slice is valid; out_len is writable.
    let ptr = unsafe { hew_msgpack_encode_bytes(input.as_ptr(), input.len(), &raw mut out_len) };
    if ptr.is_null() {
        // SAFETY: hew_vec_new allocates a valid empty HewVec.
        return unsafe { hew_cabi::vec::hew_vec_new() };
    }
    // SAFETY: ptr is valid for out_len bytes.
    let slice = unsafe { std::slice::from_raw_parts(ptr, out_len) };
    // SAFETY: slice is valid.
    let result = unsafe { hew_cabi::vec::u8_to_hwvec(slice) };
    // SAFETY: ptr was allocated by hew_msgpack_encode_bytes.
    unsafe { hew_msgpack_free(ptr) };
    result
}
