//! `std.encoding.msgpack`: MessagePack to JSON and back.
//!
//! Properties: decoding never panics, whatever declared lengths and nesting
//! say; the depth pre-scan refuses nothing that `rmp_serde` reads within the
//! nesting limit and admits nothing `rmp_serde` refuses (both read exactly
//! one document); a decoded document
//! re-encodes and decodes to the same JSON.
#![no_main]

use std::ffi::{c_char, CStr, CString};

use hew_cabi::cabi::free_cstring;
use hew_std::msgpack::{hew_msgpack_free, hew_msgpack_from_json, hew_msgpack_to_json};
use libfuzzer_sys::fuzz_target;

/// Decode through the C ABI; `None` when the codec refuses the input.
fn to_json(data: &[u8]) -> Option<String> {
    // SAFETY: `data` is a live slice for the call.
    let out = unsafe { hew_msgpack_to_json(data.as_ptr(), data.len()) };
    if out.is_null() {
        return None;
    }
    // SAFETY: a non-null result is an owned NUL-terminated header-aware string.
    let text = unsafe { CStr::from_ptr(out) }.to_str().ok()?.to_owned();
    // SAFETY: released once, through its documented allocator.
    unsafe { free_cstring(out.cast::<c_char>()) };
    Some(text)
}

/// Encode JSON text through the C ABI.
fn from_json(text: &str) -> Option<Vec<u8>> {
    let input = CString::new(text).ok()?;
    let mut len = 0usize;
    // SAFETY: `input` is NUL-terminated and `len` is writable.
    let out = unsafe { hew_msgpack_from_json(input.as_ptr(), &raw mut len) };
    if out.is_null() {
        return None;
    }
    // SAFETY: `out` holds `len` bytes until it is freed below.
    let bytes = unsafe { std::slice::from_raw_parts(out, len) }.to_vec();
    // SAFETY: an encode buffer, released once.
    unsafe { hew_msgpack_free(out) };
    Some(bytes)
}

/// `rmp_serde` read as the codec reads: one whole document, no trailing
/// bytes (`from_slice` alone ignores them).
fn reference(data: &[u8]) -> Option<serde_json::Value> {
    use serde::Deserialize;
    let mut cursor = std::io::Cursor::new(data);
    let mut de = rmp_serde::Deserializer::new(&mut cursor);
    let value = serde_json::Value::deserialize(&mut de).ok()?;
    (usize::try_from(cursor.position()).ok()? == data.len()).then_some(value)
}

fuzz_target!(|data: &[u8]| {
    let ours = to_json(data);
    let reference = reference(data);
    match (&ours, &reference) {
        (Some(ours), Some(reference)) => {
            let ours: serde_json::Value = serde_json::from_str(ours).expect("codec emits JSON");
            assert_eq!(&ours, reference, "decoders disagree");
        }
        (Some(ours), None) => panic!("decoded what rmp_serde refuses: {ours}"),
        // A refusal the reference accepts must be the documented depth limit.
        (None, Some(_)) => {
            let error = support_last_error();
            assert!(
                error.contains("maximum nesting depth"),
                "refused a document rmp_serde reads: {error}"
            );
            return;
        }
        (None, None) => return,
    }
    let json = ours.expect("decoded above");
    let encoded = from_json(&json).expect("decoded JSON re-encodes");
    let again = to_json(&encoded).expect("its own encoding decodes");
    let first: serde_json::Value = serde_json::from_str(&json).expect("JSON");
    let second: serde_json::Value = serde_json::from_str(&again).expect("JSON");
    assert_eq!(first, second, "MessagePack round trip changed the value");
});

fn support_last_error() -> String {
    let error = hew_std::msgpack::hew_msgpack_last_error();
    // SAFETY: an owned managed string, read then released once.
    unsafe {
        let text = hew_cabi::string::string_as_str(error).to_owned();
        hew_cabi::string::string_release(error);
        text
    }
}
