//! Hew runtime: `msgpack` module.
//!
//! Provides `MessagePack` encoding and decoding for compiled Hew programs.
//! Uses `rmp_serde` with `serde_json::Value` as the intermediate type for
//! JSON↔`MessagePack` conversion. All returned buffers are allocated with
//! `libc::malloc` and must be freed with [`hew_msgpack_free`]. All returned
//! strings are allocated with `libc::malloc` and NUL-terminated.
use hew_cabi::cabi::{cstr_to_str, malloc_bytes, str_to_malloc};
use hew_runtime::bytes::{hew_bytes_from_static, BytesTriple};
use std::cell::RefCell;
use std::ffi::c_char;

const MAX_MSGPACK_DEPTH: usize = 128;
const MALFORMED_MSGPACK_ERROR: &str = "msgpack: malformed input during depth pre-scan";

std::thread_local! {
    static LAST_MSGPACK_ERROR: RefCell<Option<String>> = const { RefCell::new(None) };
}

fn set_msgpack_last_error(msg: impl Into<String>) {
    LAST_MSGPACK_ERROR.with(|error| *error.borrow_mut() = Some(msg.into()));
}

fn clear_msgpack_last_error() {
    LAST_MSGPACK_ERROR.with(|error| *error.borrow_mut() = None);
}

fn get_msgpack_last_error() -> String {
    LAST_MSGPACK_ERROR.with(|error| error.borrow().clone().unwrap_or_default())
}

fn depth_exceeded_error() -> String {
    format!("msgpack: maximum nesting depth ({MAX_MSGPACK_DEPTH}) exceeded")
}

fn read_u8(slice: &[u8], cursor: &mut usize) -> Result<u8, String> {
    let value = *slice
        .get(*cursor)
        .ok_or_else(|| MALFORMED_MSGPACK_ERROR.to_string())?;
    *cursor += 1;
    Ok(value)
}

fn read_u16_be(slice: &[u8], cursor: &mut usize) -> Result<u16, String> {
    let end = cursor
        .checked_add(2)
        .ok_or_else(|| MALFORMED_MSGPACK_ERROR.to_string())?;
    let bytes = slice
        .get(*cursor..end)
        .ok_or_else(|| MALFORMED_MSGPACK_ERROR.to_string())?;
    *cursor = end;
    Ok(u16::from_be_bytes([bytes[0], bytes[1]]))
}

fn read_u32_be(slice: &[u8], cursor: &mut usize) -> Result<u32, String> {
    let end = cursor
        .checked_add(4)
        .ok_or_else(|| MALFORMED_MSGPACK_ERROR.to_string())?;
    let bytes = slice
        .get(*cursor..end)
        .ok_or_else(|| MALFORMED_MSGPACK_ERROR.to_string())?;
    *cursor = end;
    Ok(u32::from_be_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]))
}

fn skip_bytes(slice: &[u8], cursor: &mut usize, len: usize) -> Result<(), String> {
    let end = cursor
        .checked_add(len)
        .ok_or_else(|| MALFORMED_MSGPACK_ERROR.to_string())?;
    if end > slice.len() {
        return Err(MALFORMED_MSGPACK_ERROR.to_string());
    }
    *cursor = end;
    Ok(())
}

fn read_u32_len(slice: &[u8], cursor: &mut usize) -> Result<usize, String> {
    let len = read_u32_be(slice, cursor)?;
    usize::try_from(len).map_err(|_| MALFORMED_MSGPACK_ERROR.to_string())
}

fn push_container(stack: &mut Vec<u64>, child_slots: u64) -> Result<(), String> {
    stack.push(child_slots);
    if stack.len() - 1 > MAX_MSGPACK_DEPTH {
        return Err(depth_exceeded_error());
    }
    Ok(())
}

fn check_depth(slice: &[u8]) -> Result<(), String> {
    let mut cursor = 0usize;
    let mut stack = vec![1u64];

    while let Some(remaining) = stack.last_mut() {
        if *remaining == 0 {
            stack.pop();
            continue;
        }
        *remaining -= 1;

        let opcode = read_u8(slice, &mut cursor)?;
        match opcode {
            0x00..=0x7f | 0xc0 | 0xc2 | 0xc3 | 0xe0..=0xff => {}
            0x80..=0x8f => push_container(&mut stack, u64::from(opcode & 0x0f) * 2)?,
            0x90..=0x9f => push_container(&mut stack, u64::from(opcode & 0x0f))?,
            0xa0..=0xbf => skip_bytes(slice, &mut cursor, usize::from(opcode & 0x1f))?,
            0xc1 => return Err(MALFORMED_MSGPACK_ERROR.to_string()),
            0xc4 | 0xd9 => {
                let len = usize::from(read_u8(slice, &mut cursor)?);
                skip_bytes(slice, &mut cursor, len)?;
            }
            0xc5 | 0xda => {
                let len = usize::from(read_u16_be(slice, &mut cursor)?);
                skip_bytes(slice, &mut cursor, len)?;
            }
            0xc6 | 0xdb => {
                let len = read_u32_len(slice, &mut cursor)?;
                skip_bytes(slice, &mut cursor, len)?;
            }
            0xc7 => {
                let len = usize::from(read_u8(slice, &mut cursor)?);
                skip_bytes(slice, &mut cursor, len + 1)?;
            }
            0xc8 => {
                let len = usize::from(read_u16_be(slice, &mut cursor)?);
                skip_bytes(slice, &mut cursor, len + 1)?;
            }
            0xc9 => {
                let len = usize::try_from(read_u32_be(slice, &mut cursor)?)
                    .map_err(|_| MALFORMED_MSGPACK_ERROR.to_string())?;
                skip_bytes(slice, &mut cursor, len + 1)?;
            }
            0xca | 0xce | 0xd2 => skip_bytes(slice, &mut cursor, 4)?,
            0xcb | 0xcf | 0xd3 => skip_bytes(slice, &mut cursor, 8)?,
            0xcc | 0xd0 => skip_bytes(slice, &mut cursor, 1)?,
            0xcd | 0xd1 | 0xd4 => skip_bytes(slice, &mut cursor, 2)?,
            0xd5 => skip_bytes(slice, &mut cursor, 3)?,
            0xd6 => skip_bytes(slice, &mut cursor, 5)?,
            0xd7 => skip_bytes(slice, &mut cursor, 9)?,
            0xd8 => skip_bytes(slice, &mut cursor, 17)?,
            0xdc => push_container(&mut stack, u64::from(read_u16_be(slice, &mut cursor)?))?,
            0xdd => push_container(&mut stack, u64::from(read_u32_be(slice, &mut cursor)?))?,
            0xde => push_container(
                &mut stack,
                u64::from(read_u16_be(slice, &mut cursor)?)
                    .checked_mul(2)
                    .ok_or_else(|| MALFORMED_MSGPACK_ERROR.to_string())?,
            )?,
            0xdf => push_container(
                &mut stack,
                u64::from(read_u32_be(slice, &mut cursor)?)
                    .checked_mul(2)
                    .ok_or_else(|| MALFORMED_MSGPACK_ERROR.to_string())?,
            )?,
        }

        while matches!(stack.last(), Some(0)) {
            stack.pop();
        }
    }

    if cursor != slice.len() {
        return Err(MALFORMED_MSGPACK_ERROR.to_string());
    }

    Ok(())
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
/// Call [`hew_msgpack_last_error`] to retrieve this actor's failure reason.
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
        set_msgpack_last_error("msgpack: null JSON input");
        return std::ptr::null_mut();
    }
    // SAFETY: json_str is a valid NUL-terminated C string per caller contract.
    let Some(s) = (unsafe { cstr_to_str(json_str) }) else {
        set_msgpack_last_error("msgpack: JSON input was not valid UTF-8");
        return std::ptr::null_mut();
    };
    let value = match serde_json::from_str::<serde_json::Value>(s) {
        Ok(value) => value,
        Err(err) => {
            set_msgpack_last_error(format!("msgpack: failed to parse JSON input: {err}"));
            return std::ptr::null_mut();
        }
    };
    let Ok(bytes) = rmp_serde::to_vec(&value) else {
        set_msgpack_last_error("msgpack: failed to serialize to MessagePack");
        return std::ptr::null_mut();
    };
    // SAFETY: out_len is a valid pointer per caller contract.
    unsafe { *out_len = bytes.len() };
    clear_msgpack_last_error();
    malloc_bytes(&bytes)
}

/// Convert `MessagePack` binary to a JSON string.
///
/// Deserializes the `MessagePack` data into `serde_json::Value`, then serializes
/// it as JSON. Returns a `malloc`-allocated, NUL-terminated C string. Returns
/// null on deserialization or serialization failure.
///
/// Call [`hew_msgpack_last_error`] to retrieve this actor's parse failure.
///
/// # Safety
///
/// `data` must point to at least `len` readable bytes.
#[no_mangle]
pub unsafe extern "C" fn hew_msgpack_to_json(data: *const u8, len: usize) -> *mut c_char {
    if data.is_null() || len == 0 {
        set_msgpack_last_error("msgpack: invalid input buffer");
        return std::ptr::null_mut();
    }
    // SAFETY: data is valid for len bytes per caller contract.
    let slice = unsafe { std::slice::from_raw_parts(data, len) };
    if let Err(err) = check_depth(slice) {
        set_msgpack_last_error(err);
        return std::ptr::null_mut();
    }
    let Ok(value) = rmp_serde::from_slice::<serde_json::Value>(slice) else {
        set_msgpack_last_error("msgpack: failed to deserialize input");
        return std::ptr::null_mut();
    };
    let Ok(json) = serde_json::to_string(&value) else {
        set_msgpack_last_error("msgpack: failed to serialize JSON output");
        return std::ptr::null_mut();
    };
    clear_msgpack_last_error();
    str_to_malloc(&json)
}

/// Return this actor's last `MessagePack` parse error.
///
/// Returns an empty string when no parse error has been recorded.
#[no_mangle]
pub extern "C" fn hew_msgpack_last_error() -> *mut c_char {
    str_to_malloc(&get_msgpack_last_error())
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
    malloc_bytes(&bytes)
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
    malloc_bytes(&bytes)
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
    malloc_bytes(&bytes)
}

/// Free a `malloc_bytes` buffer previously returned by a `hew_msgpack_*`
/// **encode** function (e.g. [`hew_msgpack_encode`]) — i.e. an opaque `*mut u8`
/// byte buffer.
///
/// This does **not** cover `to_json` results: those are header-aware Hew
/// strings and are released through `hew_string_drop` / `free_cstring`, never
/// here. Passing a `to_json` string (or any header-aware C string) to this
/// function would free `data` instead of the allocation base and corrupt the
/// heap.
///
/// # Safety
///
/// `ptr` must be a pointer previously returned by a `hew_msgpack_*` byte-buffer
/// (encode) function, and must not have been freed already.
#[no_mangle]
pub unsafe extern "C" fn hew_msgpack_free(ptr: *mut u8) {
    if ptr.is_null() {
        return;
    }
    // SAFETY: ptr is a malloc_bytes buffer and has not been freed.
    unsafe { libc::free(ptr.cast()) }; // CSTRING-FREE: libc-bytes (hew_msgpack_free: opaque *mut u8 encode buffers = malloc_bytes; to_json strings drop via hew_string_drop, NOT here)
}

// ---------------------------------------------------------------------------
// BytesTriple-ABI wrappers (used by std/encoding/msgpack/msgpack.hew)
//
// All `_hew` entry points follow the v0.5 BytesTriple ABI (mirrors
// std/encoding/compress):
//   - `bytes` input args are passed as `*const BytesTriple` (by-pointer consumer).
//   - `bytes` return values are returned as `BytesTriple` by value, with a
//     matching `_raw` sibling that writes through a caller-supplied
//     `*mut BytesTriple` for the compiler's call-site out-pointer path.
//
// The previous HewVec-returning shape silently misread as a `BytesTriple` at
// the codegen boundary (a `*mut HewVec` pointer reinterpreted as
// `{ptr, offset, len}`), so `bytes.len()` on a from_json result read garbage —
// passing the `len() > 0` round-trip assertions but failing the `len() == 0`
// invalid-input assertion. Registering these symbols in the codegen
// `is_bytes_triple_return_producer` / `is_bytes_by_pointer_consumer` allowlists
// completes the migration.
// ---------------------------------------------------------------------------

/// Extract a byte slice from a `BytesTriple` by-pointer consumer argument.
///
/// Returns an empty slice for a null pointer or a zero-length triple.
///
/// # Safety
///
/// If non-null, `triple` must point to a live `BytesTriple` whose payload
/// is valid for at least `offset + len` bytes.
unsafe fn bytes_slice_from_triple<'a>(triple: *const BytesTriple) -> &'a [u8] {
    if triple.is_null() {
        return &[];
    }
    // SAFETY: caller guarantees triple is non-null and valid.
    let t = unsafe { &*triple };
    if t.len == 0 || t.ptr.is_null() {
        return &[];
    }
    let offset = t.offset as usize;
    let len = t.len as usize;
    // SAFETY: t.ptr + offset is valid for len bytes per BytesTriple invariant.
    unsafe { std::slice::from_raw_parts(t.ptr.add(offset), len) }
}

/// Allocate a `BytesTriple` from a byte slice via the runtime allocator.
fn bytes_triple_from_slice(data: &[u8]) -> BytesTriple {
    if data.is_empty() {
        return BytesTriple {
            ptr: std::ptr::null_mut(),
            offset: 0,
            len: 0,
        };
    }
    let len = u32::try_from(data.len()).unwrap_or(u32::MAX);
    // SAFETY: data is valid for len bytes; hew_bytes_from_static copies the slice.
    unsafe { hew_bytes_from_static(data.as_ptr(), len) }
}

/// Convert a `malloc`-allocated `(ptr, out_len)` codec result into a
/// runtime-allocated `BytesTriple`, freeing the source buffer.
///
/// A null `ptr` (codec failure) yields an empty `BytesTriple` ({null, 0, 0}).
///
/// # Safety
///
/// `ptr` must be null or a `hew_msgpack_*` codec buffer valid for `out_len`
/// bytes (it is freed via `hew_msgpack_free`).
unsafe fn triple_from_codec_buf(ptr: *mut u8, out_len: usize) -> BytesTriple {
    if ptr.is_null() {
        return BytesTriple {
            ptr: std::ptr::null_mut(),
            offset: 0,
            len: 0,
        };
    }
    // SAFETY: ptr is valid for out_len bytes.
    let slice = unsafe { std::slice::from_raw_parts(ptr, out_len) };
    let result = bytes_triple_from_slice(slice);
    // SAFETY: ptr was allocated by a hew_msgpack_* codec function.
    unsafe { hew_msgpack_free(ptr) };
    result
}

/// Encode a JSON string to `MessagePack` bytes, returning a `BytesTriple`.
///
/// Returns an empty `BytesTriple` ({null, 0, 0}) on invalid JSON or null input.
///
/// # Safety
///
/// `json` must be a valid NUL-terminated C string (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_msgpack_from_json_hew(json: *const c_char) -> BytesTriple {
    let mut out_len: usize = 0;
    // SAFETY: json is null-or-valid per caller contract; out_len is writable.
    let ptr = unsafe { hew_msgpack_from_json(json, &raw mut out_len) };
    // SAFETY: ptr is null-or-valid for out_len bytes.
    unsafe { triple_from_codec_buf(ptr, out_len) }
}

/// Decode a `bytes` `BytesTriple` of `MessagePack` data to a JSON string.
///
/// Returns an empty string on error or null/empty input.
///
/// # Safety
///
/// `v` must be null or a valid `*const BytesTriple`.
#[no_mangle]
pub unsafe extern "C" fn hew_msgpack_to_json_hew(v: *const BytesTriple) -> *mut c_char {
    // SAFETY: v is null-or-valid per caller contract.
    let bytes = unsafe { bytes_slice_from_triple(v) };
    if bytes.is_empty() {
        // Null/empty input → empty string (the C layer rejects len == 0).
        set_msgpack_last_error("msgpack: invalid input buffer");
        return str_to_malloc("");
    }
    // SAFETY: bytes slice is valid for its length.
    unsafe { hew_msgpack_to_json(bytes.as_ptr(), bytes.len()) }
}

/// Encode an i64 integer as a `MessagePack` varint, returning a `BytesTriple`.
///
/// # Safety
///
/// None — all memory is managed by the runtime allocator.
#[no_mangle]
pub unsafe extern "C" fn hew_msgpack_encode_int_hew(val: i64) -> BytesTriple {
    let mut out_len: usize = 0;
    // SAFETY: out_len is writable.
    let ptr = unsafe { hew_msgpack_encode_int(val, &raw mut out_len) };
    // SAFETY: ptr is null-or-valid for out_len bytes.
    unsafe { triple_from_codec_buf(ptr, out_len) }
}

/// Encode a C string as `MessagePack` str, returning a `BytesTriple`.
///
/// Returns an empty `BytesTriple` ({null, 0, 0}) on null input or encode failure.
///
/// # Safety
///
/// `s` must be a valid NUL-terminated C string (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_msgpack_encode_string_hew(s: *const c_char) -> BytesTriple {
    let mut out_len: usize = 0;
    // SAFETY: s is null-or-valid per caller contract; out_len is writable.
    let ptr = unsafe { hew_msgpack_encode_string(s, &raw mut out_len) };
    // SAFETY: ptr is null-or-valid for out_len bytes.
    unsafe { triple_from_codec_buf(ptr, out_len) }
}

/// Encode a `bytes` `BytesTriple` as a `MessagePack` bin, returning a `BytesTriple`.
///
/// # Safety
///
/// `v` must be null or a valid `*const BytesTriple`.
#[no_mangle]
pub unsafe extern "C" fn hew_msgpack_encode_bytes_hew(v: *const BytesTriple) -> BytesTriple {
    // SAFETY: v is null-or-valid per caller contract.
    let input = unsafe { bytes_slice_from_triple(v) };
    let mut out_len: usize = 0;
    // SAFETY: input slice is valid; out_len is writable.
    let ptr = unsafe { hew_msgpack_encode_bytes(input.as_ptr(), input.len(), &raw mut out_len) };
    // SAFETY: ptr is null-or-valid for out_len bytes.
    unsafe { triple_from_codec_buf(ptr, out_len) }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::{CStr, CString};

    unsafe fn read_and_free(ptr: *mut c_char) -> String {
        assert!(!ptr.is_null());
        // SAFETY: ptr is a valid NUL-terminated C string allocated with malloc.
        let value = unsafe { CStr::from_ptr(ptr) }.to_str().unwrap().to_owned();
        // SAFETY: ptr was allocated with malloc.
        unsafe { hew_cabi::cabi::free_cstring(ptr) }; // CSTRING-FREE: str-open (read_and_free test helper for to_json strings)
        value
    }

    #[test]
    fn json_roundtrip_object() {
        let json = r#"{"name":"hew","version":42}"#;
        let c_json = CString::new(json).unwrap();
        let mut len: usize = 0;

        // SAFETY: c_json is a valid C string; len is a valid pointer.
        unsafe {
            let buf = hew_msgpack_from_json(c_json.as_ptr(), &raw mut len);
            assert!(!buf.is_null());
            assert!(len > 0);

            let result = hew_msgpack_to_json(buf, len);
            assert!(!result.is_null());
            // SAFETY: result is a valid NUL-terminated C string from malloc.
            let result_str = CStr::from_ptr(result).to_str().unwrap();
            let v1: serde_json::Value = serde_json::from_str(json).unwrap();
            let v2: serde_json::Value = serde_json::from_str(result_str).unwrap();
            assert_eq!(v1, v2);

            hew_cabi::cabi::free_cstring(result); // CSTRING-FREE: str-open (to_json result)
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
            let buf = hew_msgpack_from_json(c_json.as_ptr(), &raw mut len);
            assert!(!buf.is_null());

            let result = hew_msgpack_to_json(buf, len);
            assert!(!result.is_null());
            // SAFETY: result is a valid NUL-terminated C string from malloc.
            let result_str = CStr::from_ptr(result).to_str().unwrap();
            let v1: serde_json::Value = serde_json::from_str(json).unwrap();
            let v2: serde_json::Value = serde_json::from_str(result_str).unwrap();
            assert_eq!(v1, v2);

            hew_cabi::cabi::free_cstring(result); // CSTRING-FREE: str-open (to_json result)
            hew_msgpack_free(buf);
        }
    }

    #[test]
    fn encode_int_roundtrip() {
        let mut len: usize = 0;

        // SAFETY: len is a valid pointer.
        unsafe {
            let buf = hew_msgpack_encode_int(42, &raw mut len);
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
            let buf = hew_msgpack_encode_string(s.as_ptr(), &raw mut len);
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
            let buf = hew_msgpack_encode_bytes(data.as_ptr(), data.len(), &raw mut len);
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
            assert!(hew_msgpack_from_json(std::ptr::null(), &raw mut len).is_null());
            assert!(hew_msgpack_to_json(std::ptr::null(), 10).is_null());
            assert!(hew_msgpack_to_json([0u8].as_ptr(), 0).is_null());
            assert!(hew_msgpack_encode_int(1, std::ptr::null_mut()).is_null());
            assert!(hew_msgpack_encode_string(std::ptr::null(), &raw mut len).is_null());
            assert!(hew_msgpack_encode_bytes(std::ptr::null(), 5, &raw mut len).is_null());
        }
    }

    #[test]
    fn nested_json_roundtrip() {
        let json = r#"{"a":{"b":{"c":[1,2,3]}}}"#;
        let c_json = CString::new(json).unwrap();
        let mut len: usize = 0;

        // SAFETY: c_json is a valid C string; len is a valid pointer.
        unsafe {
            let buf = hew_msgpack_from_json(c_json.as_ptr(), &raw mut len);
            assert!(!buf.is_null());

            let result = hew_msgpack_to_json(buf, len);
            assert!(!result.is_null());
            // SAFETY: result is a valid NUL-terminated C string from malloc.
            let result_str = CStr::from_ptr(result).to_str().unwrap();
            let v1: serde_json::Value = serde_json::from_str(json).unwrap();
            let v2: serde_json::Value = serde_json::from_str(result_str).unwrap();
            assert_eq!(v1, v2);

            hew_cabi::cabi::free_cstring(result); // CSTRING-FREE: str-open (to_json result)
            hew_msgpack_free(buf);
        }
    }

    // ----- Null-pointer safety (per-function) -----

    #[test]
    fn from_json_null_out_len_returns_null() {
        let json = CString::new("42").unwrap();
        // SAFETY: testing null out_len.
        unsafe {
            assert!(hew_msgpack_from_json(json.as_ptr(), std::ptr::null_mut()).is_null());
        }
    }

    #[test]
    fn encode_int_null_out_len_returns_null() {
        // SAFETY: testing null out_len.
        unsafe {
            assert!(hew_msgpack_encode_int(42, std::ptr::null_mut()).is_null());
        }
    }

    #[test]
    fn encode_string_null_out_len_returns_null() {
        let s = CString::new("hello").unwrap();
        // SAFETY: testing null out_len.
        unsafe {
            assert!(hew_msgpack_encode_string(s.as_ptr(), std::ptr::null_mut()).is_null());
        }
    }

    #[test]
    fn encode_bytes_null_out_len_returns_null() {
        let data = [0xABu8];
        // SAFETY: testing null out_len.
        unsafe {
            assert!(
                hew_msgpack_encode_bytes(data.as_ptr(), data.len(), std::ptr::null_mut()).is_null()
            );
        }
    }

    #[test]
    fn free_null_is_noop() {
        // SAFETY: freeing null must be a safe no-op.
        unsafe { hew_msgpack_free(std::ptr::null_mut()) };
    }

    // ----- Malformed / invalid input -----

    #[test]
    fn from_json_invalid_json_returns_null() {
        let bad = CString::new("{not valid json}").unwrap();
        let mut len: usize = 0;
        // SAFETY: bad is a valid C string; len is a valid pointer.
        unsafe {
            assert!(hew_msgpack_from_json(bad.as_ptr(), &raw mut len).is_null());
        }
    }

    #[test]
    fn to_json_malformed_msgpack_returns_null() {
        // bin32 header (0xC6) claiming 256 bytes, but only 2 bytes follow.
        let garbage: &[u8] = &[0xC6, 0x00, 0x00, 0x01, 0x00, 0xAA, 0xBB];
        // SAFETY: garbage is a valid buffer.
        unsafe {
            assert!(hew_msgpack_to_json(garbage.as_ptr(), garbage.len()).is_null());
        }
    }

    #[test]
    fn to_json_truncated_msgpack_returns_null() {
        // Encode a valid value, then truncate it.
        let full = rmp_serde::to_vec(&serde_json::json!({"key": "value"})).unwrap();
        let truncated = &full[..full.len() / 2];
        // SAFETY: truncated is a valid buffer.
        unsafe {
            assert!(hew_msgpack_to_json(truncated.as_ptr(), truncated.len()).is_null());
        }
    }

    #[test]
    fn to_json_rejects_deeply_nested_arrays_before_deserializing() {
        let mut nested = vec![0x91; 10_000];
        nested.push(0xc0);

        // SAFETY: nested is a valid buffer.
        unsafe {
            assert!(hew_msgpack_to_json(nested.as_ptr(), nested.len()).is_null());
            assert_eq!(
                read_and_free(hew_msgpack_last_error()),
                depth_exceeded_error()
            );
        }
    }

    #[test]
    fn to_json_rejects_deeply_nested_maps_before_deserializing() {
        let mut nested = vec![0x81; 200];
        nested.push(0xc0);

        // SAFETY: nested is a valid buffer.
        unsafe {
            assert!(hew_msgpack_to_json(nested.as_ptr(), nested.len()).is_null());
            assert_eq!(
                read_and_free(hew_msgpack_last_error()),
                depth_exceeded_error()
            );
        }
    }

    #[test]
    fn to_json_allows_normal_nested_msgpack() {
        let msgpack = rmp_serde::to_vec(&serde_json::json!({
            "level1": [{"level2": {"level3": [{"level4": {"level5": null}}]}}]
        }))
        .unwrap();

        // SAFETY: msgpack is a valid buffer.
        unsafe {
            let json = hew_msgpack_to_json(msgpack.as_ptr(), msgpack.len());
            assert!(!json.is_null());

            let json_str = CStr::from_ptr(json).to_str().unwrap();
            let expected = serde_json::json!({
                "level1": [{"level2": {"level3": [{"level4": {"level5": null}}]}}]
            });
            let actual: serde_json::Value = serde_json::from_str(json_str).unwrap();
            assert_eq!(actual, expected);
            assert_eq!(read_and_free(hew_msgpack_last_error()), "");

            hew_cabi::cabi::free_cstring(json); // CSTRING-FREE: str-open (to_json json)
        }
    }

    #[test]
    fn to_json_allows_empty_array() {
        let empty_array = [0x90u8];

        // SAFETY: empty_array is a valid buffer.
        unsafe {
            let json = hew_msgpack_to_json(empty_array.as_ptr(), empty_array.len());
            assert!(!json.is_null());
            assert_eq!(CStr::from_ptr(json).to_str().unwrap(), "[]");
            assert_eq!(read_and_free(hew_msgpack_last_error()), "");
            hew_cabi::cabi::free_cstring(json); // CSTRING-FREE: str-open (to_json json)
        }
    }

    // ----- Boundary values -----

    #[test]
    fn encode_int_boundary_values() {
        for &val in &[0i64, 1, -1, i64::MIN, i64::MAX, 127, -128] {
            let mut len: usize = 0;
            // SAFETY: len is a valid pointer.
            unsafe {
                let buf = hew_msgpack_encode_int(val, &raw mut len);
                assert!(!buf.is_null(), "encode failed for {val}");
                assert!(len > 0, "zero-length encoding for {val}");

                let slice = std::slice::from_raw_parts(buf, len);
                let decoded: i64 = rmp_serde::from_slice(slice).unwrap();
                assert_eq!(decoded, val, "roundtrip mismatch for {val}");

                hew_msgpack_free(buf);
            }
        }
    }

    #[test]
    fn encode_string_empty() {
        let s = CString::new("").unwrap();
        let mut len: usize = 0;
        // SAFETY: s is a valid C string; len is a valid pointer.
        unsafe {
            let buf = hew_msgpack_encode_string(s.as_ptr(), &raw mut len);
            assert!(!buf.is_null());
            assert!(len > 0);

            let slice = std::slice::from_raw_parts(buf, len);
            let decoded: String = rmp_serde::from_slice(slice).unwrap();
            assert_eq!(decoded, "");

            hew_msgpack_free(buf);
        }
    }

    #[test]
    fn encode_bytes_empty_data_returns_null_from_null_guard() {
        let mut len: usize = 0;
        // encode_bytes with null data returns null (the null guard).
        // SAFETY: testing boundary — null data with len=0.
        unsafe {
            assert!(hew_msgpack_encode_bytes(std::ptr::null(), 0, &raw mut len).is_null());
        }
    }

    // ----- JSON type coverage -----

    #[test]
    fn json_roundtrip_boolean_and_null() {
        for json in &["true", "false", "null"] {
            let c_json = CString::new(*json).unwrap();
            let mut len: usize = 0;
            // SAFETY: c_json is a valid C string; len is a valid pointer.
            unsafe {
                let buf = hew_msgpack_from_json(c_json.as_ptr(), &raw mut len);
                assert!(!buf.is_null(), "encode failed for {json}");

                let result = hew_msgpack_to_json(buf, len);
                assert!(!result.is_null(), "decode failed for {json}");
                let result_str = CStr::from_ptr(result).to_str().unwrap();
                let v1: serde_json::Value = serde_json::from_str(json).unwrap();
                let v2: serde_json::Value = serde_json::from_str(result_str).unwrap();
                assert_eq!(v1, v2, "roundtrip mismatch for {json}");

                hew_cabi::cabi::free_cstring(result); // CSTRING-FREE: str-open (to_json result)
                hew_msgpack_free(buf);
            }
        }
    }

    #[test]
    fn json_roundtrip_float() {
        let json = "3.14159";
        let c_json = CString::new(json).unwrap();
        let mut len: usize = 0;
        // SAFETY: c_json is a valid C string; len is a valid pointer.
        unsafe {
            let buf = hew_msgpack_from_json(c_json.as_ptr(), &raw mut len);
            assert!(!buf.is_null());

            let result = hew_msgpack_to_json(buf, len);
            assert!(!result.is_null());
            let result_str = CStr::from_ptr(result).to_str().unwrap();
            let v1: serde_json::Value = serde_json::from_str(json).unwrap();
            let v2: serde_json::Value = serde_json::from_str(result_str).unwrap();
            assert_eq!(v1, v2);

            hew_cabi::cabi::free_cstring(result); // CSTRING-FREE: str-open (to_json result)
            hew_msgpack_free(buf);
        }
    }

    #[test]
    fn json_roundtrip_empty_object_and_array() {
        for json in &["{}", "[]"] {
            let c_json = CString::new(*json).unwrap();
            let mut len: usize = 0;
            // SAFETY: c_json is a valid C string; len is a valid pointer.
            unsafe {
                let buf = hew_msgpack_from_json(c_json.as_ptr(), &raw mut len);
                assert!(!buf.is_null(), "encode failed for {json}");

                let result = hew_msgpack_to_json(buf, len);
                assert!(!result.is_null(), "decode failed for {json}");
                let result_str = CStr::from_ptr(result).to_str().unwrap();
                let v1: serde_json::Value = serde_json::from_str(json).unwrap();
                let v2: serde_json::Value = serde_json::from_str(result_str).unwrap();
                assert_eq!(v1, v2, "roundtrip mismatch for {json}");

                hew_cabi::cabi::free_cstring(result); // CSTRING-FREE: str-open (to_json result)
                hew_msgpack_free(buf);
            }
        }
    }

    #[test]
    fn json_roundtrip_unicode_string() {
        let json = r#""Hello 🌍 café""#;
        let c_json = CString::new(json).unwrap();
        let mut len: usize = 0;
        // SAFETY: c_json is a valid C string; len is a valid pointer.
        unsafe {
            let buf = hew_msgpack_from_json(c_json.as_ptr(), &raw mut len);
            assert!(!buf.is_null());

            let result = hew_msgpack_to_json(buf, len);
            assert!(!result.is_null());
            let result_str = CStr::from_ptr(result).to_str().unwrap();
            let v1: serde_json::Value = serde_json::from_str(json).unwrap();
            let v2: serde_json::Value = serde_json::from_str(result_str).unwrap();
            assert_eq!(v1, v2);

            hew_cabi::cabi::free_cstring(result); // CSTRING-FREE: str-open (to_json result)
            hew_msgpack_free(buf);
        }
    }

    #[test]
    fn encode_string_unicode_roundtrip() {
        let s = CString::new("émojis: 🎉🚀").unwrap();
        let mut len: usize = 0;
        // SAFETY: s is a valid C string; len is a valid pointer.
        unsafe {
            let buf = hew_msgpack_encode_string(s.as_ptr(), &raw mut len);
            assert!(!buf.is_null());

            let slice = std::slice::from_raw_parts(buf, len);
            let decoded: String = rmp_serde::from_slice(slice).unwrap();
            assert_eq!(decoded, "émojis: 🎉🚀");

            hew_msgpack_free(buf);
        }
    }

    #[test]
    fn encode_bytes_large_payload_roundtrip() {
        let data: Vec<u8> = (0..=255).cycle().take(1024).collect();
        let mut len: usize = 0;
        // SAFETY: data is a valid buffer; len is a valid pointer.
        unsafe {
            let buf = hew_msgpack_encode_bytes(data.as_ptr(), data.len(), &raw mut len);
            assert!(!buf.is_null());
            assert!(len > 0);

            let slice = std::slice::from_raw_parts(buf, len);
            let decoded: Vec<u8> = rmp_serde::from_slice(slice).unwrap();
            assert_eq!(decoded, data);

            hew_msgpack_free(buf);
        }
    }

    // ----- malloc_bytes migration: sentinel and large-buffer coverage -----
    // These tests confirm that malloc_bytes (from hew-cabi) is used correctly
    // at the four migrated call sites. The canonical helper always returns a
    // non-null sentinel even when the encoded byte slice is empty; in practice
    // rmp_serde never produces a zero-length encoding, so these tests focus on
    // the smallest-possible (single-byte) encoding and a 4 KiB payload.

    #[test]
    fn encode_int_zero_produces_nonnull_single_byte_buf() {
        // rmp_serde encodes 0i64 as 0x00 — the smallest valid msgpack value.
        // Verifies that malloc_bytes returns non-null for a 1-byte encoded buffer.
        let mut len: usize = 0;
        // SAFETY: len is a valid pointer.
        unsafe {
            let buf = hew_msgpack_encode_int(0, &raw mut len);
            assert!(!buf.is_null(), "encode_int(0) must return non-null");
            assert_eq!(len, 1, "msgpack encoding of 0 must be exactly 1 byte");
            assert_eq!(*buf, 0x00, "msgpack encoding of 0 must be byte 0x00");
            hew_msgpack_free(buf);
        }
    }

    #[test]
    fn from_json_large_payload_produces_nonnull_buffer() {
        // Build a JSON array of 1024 integers (≈4 KiB encoded).
        let entries: Vec<String> = (0..1024).map(|i| i.to_string()).collect();
        let json = format!("[{}]", entries.join(","));
        let c_json = CString::new(json).unwrap();
        let mut len: usize = 0;
        // SAFETY: c_json is a valid C string; len is a valid pointer.
        unsafe {
            let buf = hew_msgpack_from_json(c_json.as_ptr(), &raw mut len);
            assert!(
                !buf.is_null(),
                "from_json must return non-null for large payload"
            );
            assert!(len > 0);
            // Decode and spot-check the first element.
            let slice = std::slice::from_raw_parts(buf, len);
            let value: serde_json::Value = rmp_serde::from_slice(slice).unwrap();
            assert!(value.is_array());
            assert_eq!(value.as_array().unwrap().len(), 1024);
            hew_msgpack_free(buf);
        }
    }
}
