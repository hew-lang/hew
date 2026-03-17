//! Byte-level stream/sink helpers.
//!
//! Bridges the raw `hew_stream_next_sized` / `hew_sink_write` functions to the
//! `bytes` (`HewVec<i32>`) representation used by the Hew language.

use std::ffi::c_void;

use crate::stream::{hew_stream_next_sized, HewStream};
use hew_cabi::sink::HewSink;
use hew_cabi::vec::HewVec;

/// Read the next item from a stream and return it as a `bytes` value.
///
/// Returns a `*mut HewVec` (i32-element vec, one byte per slot) on success —
/// including for zero-length items — or **null** on EOF.  The caller owns the
/// returned vec and must eventually free it.
///
/// The null-vs-empty distinction is important: the codegen wraps this into
/// `Option<bytes>` (`null` → `None`, non-null → `Some(bytes)`).
///
/// # Safety
///
/// `stream` must be a valid stream pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_next_bytes(stream: *mut HewStream) -> *mut HewVec {
    if stream.is_null() {
        return std::ptr::null_mut();
    }
    let mut size: usize = 0;
    // SAFETY: stream is valid per caller contract; size is a valid local.
    let ptr = unsafe { hew_stream_next_sized(stream, std::ptr::addr_of_mut!(size)) };
    if ptr.is_null() {
        return std::ptr::null_mut(); // EOF
    }
    // SAFETY: ptr is valid for `size` bytes (from hew_stream_next_sized contract).
    let raw = unsafe { std::slice::from_raw_parts(ptr.cast::<u8>(), size) };
    // SAFETY: u8_to_hwvec allocates a new HewVec and pushes each byte.
    let vec = unsafe { hew_cabi::vec::u8_to_hwvec(raw) };
    // Free the malloc'd buffer returned by hew_stream_next_sized.
    // SAFETY: ptr was allocated by libc::malloc inside hew_stream_next_sized.
    unsafe { libc::free(ptr) };
    vec
}

/// Write a `bytes` value to a sink.
///
/// Extracts the raw byte content from the `HewVec` and writes it as a single
/// stream item.  Zero-length writes are forwarded to the backing — they are
/// valid data, not no-ops.  Does nothing only if `sink` or `data` is null.
///
/// # Safety
///
/// `sink` must be a valid sink pointer.  `data` must be a valid `HewVec` pointer
/// (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_sink_write_bytes(sink: *mut HewSink, data: *mut HewVec) {
    if sink.is_null() || data.is_null() {
        return;
    }
    // SAFETY: data is a valid HewVec per caller contract.
    let bytes = unsafe { hew_cabi::vec::hwvec_to_u8(data) };
    if bytes.is_empty() {
        // hew_sink_write short-circuits on size=0, but empty bytes are valid
        // data items that must be delivered.  Write directly to the backing.
        // SAFETY: sink is valid per caller contract.
        unsafe { (*sink).inner.write_item(&[]) };
    } else {
        // SAFETY: sink is valid; bytes slice is valid for its length.
        unsafe {
            crate::stream::hew_sink_write(sink, bytes.as_ptr().cast::<c_void>(), bytes.len());
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn next_bytes_null_stream_returns_null() {
        // SAFETY: Passing null is the scenario under test.
        let result = unsafe { hew_stream_next_bytes(std::ptr::null_mut()) };
        assert!(result.is_null(), "null stream must yield null (EOF)");
    }

    #[test]
    fn write_bytes_null_sink_does_not_crash() {
        // SAFETY: Passing null is the scenario under test.
        unsafe { hew_sink_write_bytes(std::ptr::null_mut(), std::ptr::null_mut()) };
    }

    #[test]
    fn write_bytes_null_data_does_not_crash() {
        // SAFETY: Both pointers are null — the function should bail early.
        unsafe { hew_sink_write_bytes(std::ptr::null_mut(), std::ptr::null_mut()) };
    }
}
