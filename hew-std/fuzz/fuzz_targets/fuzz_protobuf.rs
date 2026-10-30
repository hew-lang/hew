//! `std.encoding.protobuf` dynamic messages from untrusted wire bytes.
//!
//! Properties: decoding never panics or over-reads, whatever the varints and
//! length prefixes declare; a decoded message encodes, the encoding decodes,
//! and encoding is a fixed point; typed getters on every decoded field
//! number never panic.
#![no_main]

use hew_std::protobuf::{
    hew_proto_msg_decode, hew_proto_msg_encode, hew_proto_msg_free, hew_proto_msg_get_bytes,
    hew_proto_msg_get_fixed32, hew_proto_msg_get_fixed64, hew_proto_msg_get_string,
    hew_proto_msg_get_varint, hew_proto_msg_has_field, HewProtoMsg,
};
use libfuzzer_sys::fuzz_target;

mod support;

fn encode(msg: *const HewProtoMsg) -> Vec<u8> {
    let mut len = 0usize;
    // SAFETY: `msg` is live and `len` writable.
    let out = unsafe { hew_proto_msg_encode(msg, &raw mut len) };
    assert!(!out.is_null(), "a decoded message encodes");
    // SAFETY: `out` holds `len` bytes until freed below.
    let bytes = unsafe { std::slice::from_raw_parts(out, len) }.to_vec();
    // SAFETY: a `malloc_bytes` block, released once.
    unsafe { hew_cabi::mem::buf_free(out.cast()) };
    bytes
}

fn decode(bytes: &[u8]) -> *mut HewProtoMsg {
    // SAFETY: `bytes` is a live slice for the call.
    unsafe { hew_proto_msg_decode(bytes.as_ptr(), bytes.len()) }
}

/// Field numbers named by the leading tags, for the getter probes.
fn probe_fields(data: &[u8]) -> impl Iterator<Item = u32> + '_ {
    data.iter().take(8).map(|byte| u32::from(byte >> 3))
}

fuzz_target!(|data: &[u8]| {
    let msg = decode(data);
    if msg.is_null() {
        return;
    }
    for field in probe_fields(data) {
        // SAFETY: `msg` is live; each result is released through its contract.
        unsafe {
            hew_proto_msg_has_field(msg, field);
            hew_proto_msg_get_varint(msg, field, 0);
            hew_proto_msg_get_fixed64(msg, field, 0);
            hew_proto_msg_get_fixed32(msg, field, 0);
            drop(support::take(hew_proto_msg_get_string(msg, field)));
            let mut len = 0usize;
            // Borrowed from the message: never freed here.
            let bytes = hew_proto_msg_get_bytes(msg, field, &raw mut len);
            if !bytes.is_null() {
                std::hint::black_box(std::slice::from_raw_parts(bytes, len));
            }
        }
    }
    let first = encode(msg);
    let again = decode(&first);
    assert!(
        !again.is_null(),
        "a message refuses its own encoding: {first:?}"
    );
    let second = encode(again);
    assert_eq!(first, second, "protobuf encoding is not a fixed point");
    // SAFETY: both handles are live and released once.
    unsafe {
        hew_proto_msg_free(again);
        hew_proto_msg_free(msg);
    }
});
