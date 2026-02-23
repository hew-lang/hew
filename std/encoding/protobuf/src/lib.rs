//! Hew runtime: Protocol Buffers wire-format encode/decode (schema-less).
//!
//! Provides dynamic protobuf message construction, serialization, and field
//! access for compiled Hew programs without requiring `.proto` schemas at
//! compile time. Each message is a bag of `(field_number, wire_type, value)`
//! tuples. All returned [`HewProtoMsg`] pointers are heap-allocated via `Box`
//! and must be freed with [`hew_proto_msg_free`].

use std::os::raw::c_char;

// ---------------------------------------------------------------------------
// Wire-format constants
// ---------------------------------------------------------------------------

/// Protobuf wire type: variable-length integer.
const WIRE_VARINT: u32 = 0;
/// Protobuf wire type: 64-bit fixed.
const WIRE_FIXED64: u32 = 1;
/// Protobuf wire type: length-delimited (bytes, strings, nested messages).
const WIRE_LEN: u32 = 2;
/// Protobuf wire type: 32-bit fixed.
const WIRE_FIXED32: u32 = 5;

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

/// A decoded protobuf message — a collection of fields.
#[derive(Debug)]
pub struct HewProtoMsg {
    fields: Vec<ProtoField>,
}

#[derive(Debug, Clone)]
struct ProtoField {
    field_number: u32,
    value: ProtoValue,
}

#[derive(Debug, Clone)]
enum ProtoValue {
    Varint(u64),
    Fixed64(u64),
    Fixed32(u32),
    Bytes(Vec<u8>),
}

// ---------------------------------------------------------------------------
// Varint helpers
// ---------------------------------------------------------------------------

/// Encode a `u64` as a protobuf base-128 varint, appending to `buf`.
fn encode_varint(mut val: u64, buf: &mut Vec<u8>) {
    loop {
        let byte = (val & 0x7F) as u8;
        val >>= 7;
        if val == 0 {
            buf.push(byte);
            return;
        }
        buf.push(byte | 0x80);
    }
}

/// Decode a base-128 varint from `data` starting at `pos`.
/// Returns `(value, new_pos)` or `None` on truncation / overflow.
fn decode_varint(data: &[u8], mut pos: usize) -> Option<(u64, usize)> {
    let mut result: u64 = 0;
    let mut shift: u32 = 0;
    loop {
        let byte = *data.get(pos)?;
        pos += 1;
        if shift == 63 && byte & 0x7E != 0 {
            return None; // overflow: 10th byte has invalid upper bits
        }
        result |= u64::from(byte & 0x7F) << shift;
        if byte & 0x80 == 0 {
            return Some((result, pos));
        }
        shift += 7;
        if shift >= 64 {
            return None; // overflow
        }
    }
}

// ---------------------------------------------------------------------------
// Encoding / decoding
// ---------------------------------------------------------------------------

impl HewProtoMsg {
    /// Encode all fields into protobuf wire format.
    fn encode(&self) -> Vec<u8> {
        let mut buf = Vec::new();
        for f in &self.fields {
            let wire_type = match &f.value {
                ProtoValue::Varint(_) => WIRE_VARINT,
                ProtoValue::Fixed64(_) => WIRE_FIXED64,
                ProtoValue::Fixed32(_) => WIRE_FIXED32,
                ProtoValue::Bytes(_) => WIRE_LEN,
            };
            let tag = (u64::from(f.field_number) << 3) | u64::from(wire_type);
            encode_varint(tag, &mut buf);
            match &f.value {
                ProtoValue::Varint(v) => encode_varint(*v, &mut buf),
                ProtoValue::Fixed64(v) => buf.extend_from_slice(&v.to_le_bytes()),
                ProtoValue::Fixed32(v) => buf.extend_from_slice(&v.to_le_bytes()),
                ProtoValue::Bytes(b) => {
                    encode_varint(b.len() as u64, &mut buf);
                    buf.extend_from_slice(b);
                }
            }
        }
        buf
    }

    /// Decode a protobuf wire-format buffer into a message.
    /// Returns `None` on malformed input.
    fn decode(data: &[u8]) -> Option<Self> {
        let mut fields = Vec::new();
        let mut pos = 0;
        while pos < data.len() {
            let (tag, new_pos) = decode_varint(data, pos)?;
            pos = new_pos;
            #[expect(
                clippy::cast_possible_truncation,
                reason = "field number fits in u32 by protobuf spec"
            )]
            let field_number = (tag >> 3) as u32;
            let wire_type = (tag & 0x07) as u32;
            let value = match wire_type {
                WIRE_VARINT => {
                    let (v, new_pos) = decode_varint(data, pos)?;
                    pos = new_pos;
                    ProtoValue::Varint(v)
                }
                WIRE_FIXED64 => {
                    if pos + 8 > data.len() {
                        return None;
                    }
                    let v = u64::from_le_bytes(data[pos..pos + 8].try_into().ok()?);
                    pos += 8;
                    ProtoValue::Fixed64(v)
                }
                WIRE_LEN => {
                    let (len, new_pos) = decode_varint(data, pos)?;
                    pos = new_pos;
                    #[expect(
                        clippy::cast_possible_truncation,
                        reason = "buffer lengths fit in usize on supported platforms"
                    )]
                    let len = len as usize;
                    if pos + len > data.len() {
                        return None;
                    }
                    let v = data[pos..pos + len].to_vec();
                    pos += len;
                    ProtoValue::Bytes(v)
                }
                WIRE_FIXED32 => {
                    if pos + 4 > data.len() {
                        return None;
                    }
                    let v = u32::from_le_bytes(data[pos..pos + 4].try_into().ok()?);
                    pos += 4;
                    ProtoValue::Fixed32(v)
                }
                _ => return None, // unknown wire type
            };
            fields.push(ProtoField {
                field_number,
                value,
            });
        }
        Some(Self { fields })
    }

    /// Find the last field with the given number (protobuf "last wins" semantics).
    fn find_field(&self, field_number: u32) -> Option<&ProtoField> {
        self.fields
            .iter()
            .rev()
            .find(|f| f.field_number == field_number)
    }

    /// Set a field, replacing any existing field with the same number.
    fn set_field(&mut self, field_number: u32, value: ProtoValue) {
        self.fields.retain(|f| f.field_number != field_number);
        self.fields.push(ProtoField {
            field_number,
            value,
        });
    }
}

// ---------------------------------------------------------------------------
// C ABI exports
// ---------------------------------------------------------------------------

/// Create a new, empty protobuf message.
#[no_mangle]
pub extern "C" fn hew_proto_msg_new() -> *mut HewProtoMsg {
    Box::into_raw(Box::new(HewProtoMsg { fields: Vec::new() }))
}

/// Set a varint field (int32/int64/uint32/uint64/bool/enum).
///
/// # Safety
///
/// `msg` must be a valid pointer returned by [`hew_proto_msg_new`] or
/// [`hew_proto_msg_decode`].
#[no_mangle]
pub unsafe extern "C" fn hew_proto_msg_set_varint(
    msg: *mut HewProtoMsg,
    field_number: u32,
    value: u64,
) {
    if msg.is_null() {
        return;
    }
    // SAFETY: msg is a valid HewProtoMsg pointer per caller contract.
    let m = unsafe { &mut *msg };
    m.set_field(field_number, ProtoValue::Varint(value));
}

/// Set a fixed64 field (fixed64/sfixed64/double).
///
/// # Safety
///
/// `msg` must be a valid pointer returned by [`hew_proto_msg_new`] or
/// [`hew_proto_msg_decode`].
#[no_mangle]
pub unsafe extern "C" fn hew_proto_msg_set_fixed64(
    msg: *mut HewProtoMsg,
    field_number: u32,
    value: u64,
) {
    if msg.is_null() {
        return;
    }
    // SAFETY: msg is a valid HewProtoMsg pointer per caller contract.
    let m = unsafe { &mut *msg };
    m.set_field(field_number, ProtoValue::Fixed64(value));
}

/// Set a fixed32 field (fixed32/sfixed32/float).
///
/// # Safety
///
/// `msg` must be a valid pointer returned by [`hew_proto_msg_new`] or
/// [`hew_proto_msg_decode`].
#[no_mangle]
pub unsafe extern "C" fn hew_proto_msg_set_fixed32(
    msg: *mut HewProtoMsg,
    field_number: u32,
    value: u32,
) {
    if msg.is_null() {
        return;
    }
    // SAFETY: msg is a valid HewProtoMsg pointer per caller contract.
    let m = unsafe { &mut *msg };
    m.set_field(field_number, ProtoValue::Fixed32(value));
}

/// Set a length-delimited field (bytes/string/nested message).
///
/// # Safety
///
/// `msg` must be a valid pointer returned by [`hew_proto_msg_new`] or
/// [`hew_proto_msg_decode`]. If `len > 0`, `data` must point to at least
/// `len` readable bytes.
#[no_mangle]
pub unsafe extern "C" fn hew_proto_msg_set_bytes(
    msg: *mut HewProtoMsg,
    field_number: u32,
    data: *const u8,
    len: usize,
) {
    if msg.is_null() {
        return;
    }
    let bytes = if data.is_null() || len == 0 {
        Vec::new()
    } else {
        // SAFETY: data is valid for len bytes per caller contract.
        unsafe { std::slice::from_raw_parts(data, len) }.to_vec()
    };
    // SAFETY: msg is a valid HewProtoMsg pointer per caller contract.
    let m = unsafe { &mut *msg };
    m.set_field(field_number, ProtoValue::Bytes(bytes));
}

/// Set a string field (convenience wrapper around [`hew_proto_msg_set_bytes`]).
///
/// # Safety
///
/// `msg` must be a valid pointer returned by [`hew_proto_msg_new`] or
/// [`hew_proto_msg_decode`]. `s` must be a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_proto_msg_set_string(
    msg: *mut HewProtoMsg,
    field_number: u32,
    s: *const c_char,
) {
    if msg.is_null() || s.is_null() {
        return;
    }
    // SAFETY: s is a valid NUL-terminated C string per caller contract.
    let cs = unsafe { std::ffi::CStr::from_ptr(s) };
    let bytes = cs.to_bytes().to_vec();
    // SAFETY: msg is a valid HewProtoMsg pointer per caller contract.
    let m = unsafe { &mut *msg };
    m.set_field(field_number, ProtoValue::Bytes(bytes));
}

/// Encode the message to protobuf wire format.
///
/// Returns a `malloc`-allocated buffer. The caller must free it with
/// `libc::free`. Writes the buffer length to `*out_len`. Returns null on
/// allocation failure or if `msg` is null.
///
/// # Safety
///
/// `msg` must be a valid pointer to a [`HewProtoMsg`].
/// `out_len` must be a valid pointer to a `usize`.
#[no_mangle]
pub unsafe extern "C" fn hew_proto_msg_encode(
    msg: *const HewProtoMsg,
    out_len: *mut usize,
) -> *mut u8 {
    if msg.is_null() || out_len.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: msg is a valid HewProtoMsg pointer per caller contract.
    let m = unsafe { &*msg };
    let encoded = m.encode();
    let len = encoded.len();

    let alloc_size = if len == 0 { 1 } else { len };
    // SAFETY: allocating alloc_size bytes via malloc (at least 1 to avoid malloc(0)).
    let ptr = unsafe { libc::malloc(alloc_size) }.cast::<u8>();
    if ptr.is_null() {
        return std::ptr::null_mut();
    }
    if len > 0 {
        // SAFETY: encoded.as_ptr() is valid for len bytes; ptr is freshly
        // allocated with len bytes; regions do not overlap.
        unsafe { std::ptr::copy_nonoverlapping(encoded.as_ptr(), ptr, len) };
    }
    // SAFETY: out_len is a valid pointer per caller contract.
    unsafe { *out_len = len };
    ptr
}

/// Decode a protobuf wire-format buffer into a [`HewProtoMsg`].
///
/// Returns null on malformed input or if `data` is null.
///
/// # Safety
///
/// If `len > 0`, `data` must point to at least `len` readable bytes.
#[no_mangle]
pub unsafe extern "C" fn hew_proto_msg_decode(data: *const u8, len: usize) -> *mut HewProtoMsg {
    if data.is_null() || len == 0 {
        return std::ptr::null_mut();
    }
    // SAFETY: data is valid for len bytes per caller contract.
    let slice = unsafe { std::slice::from_raw_parts(data, len) };
    match HewProtoMsg::decode(slice) {
        Some(msg) => Box::into_raw(Box::new(msg)),
        None => std::ptr::null_mut(),
    }
}

/// Get a varint field value. Returns `default` if the field is missing or
/// has a different wire type.
///
/// # Safety
///
/// `msg` must be a valid pointer to a [`HewProtoMsg`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_proto_msg_get_varint(
    msg: *const HewProtoMsg,
    field_number: u32,
    default: u64,
) -> u64 {
    if msg.is_null() {
        return default;
    }
    // SAFETY: msg is a valid HewProtoMsg pointer per caller contract.
    let m = unsafe { &*msg };
    match m.find_field(field_number) {
        Some(ProtoField {
            value: ProtoValue::Varint(v),
            ..
        }) => *v,
        _ => default,
    }
}

/// Get a fixed64 field value. Returns `default` if the field is missing or
/// has a different wire type.
///
/// # Safety
///
/// `msg` must be a valid pointer to a [`HewProtoMsg`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_proto_msg_get_fixed64(
    msg: *const HewProtoMsg,
    field_number: u32,
    default: u64,
) -> u64 {
    if msg.is_null() {
        return default;
    }
    // SAFETY: msg is a valid HewProtoMsg pointer per caller contract.
    let m = unsafe { &*msg };
    match m.find_field(field_number) {
        Some(ProtoField {
            value: ProtoValue::Fixed64(v),
            ..
        }) => *v,
        _ => default,
    }
}

/// Get a fixed32 field value. Returns `default` if the field is missing or
/// has a different wire type.
///
/// # Safety
///
/// `msg` must be a valid pointer to a [`HewProtoMsg`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_proto_msg_get_fixed32(
    msg: *const HewProtoMsg,
    field_number: u32,
    default: u32,
) -> u32 {
    if msg.is_null() {
        return default;
    }
    // SAFETY: msg is a valid HewProtoMsg pointer per caller contract.
    let m = unsafe { &*msg };
    match m.find_field(field_number) {
        Some(ProtoField {
            value: ProtoValue::Fixed32(v),
            ..
        }) => *v,
        _ => default,
    }
}

/// Get a bytes field. Returns a pointer into the message's internal buffer
/// (NOT `malloc`-allocated — do NOT free). Writes the length to `*out_len`.
/// Returns null if the field is missing or has a different wire type.
///
/// # Safety
///
/// `msg` must be a valid pointer to a [`HewProtoMsg`], or null.
/// `out_len` must be a valid pointer to a `usize`.
#[no_mangle]
pub unsafe extern "C" fn hew_proto_msg_get_bytes(
    msg: *const HewProtoMsg,
    field_number: u32,
    out_len: *mut usize,
) -> *const u8 {
    if msg.is_null() || out_len.is_null() {
        return std::ptr::null();
    }
    // SAFETY: msg is a valid HewProtoMsg pointer per caller contract.
    let m = unsafe { &*msg };
    if let Some(ProtoField {
        value: ProtoValue::Bytes(b),
        ..
    }) = m.find_field(field_number)
    {
        // SAFETY: out_len is a valid pointer per caller contract.
        unsafe { *out_len = b.len() };
        b.as_ptr()
    } else {
        // SAFETY: out_len is a valid pointer per caller contract.
        unsafe { *out_len = 0 };
        std::ptr::null()
    }
}

/// Get a string field as a `malloc`-allocated, NUL-terminated C string.
/// The caller must free the returned pointer with `libc::free`.
/// Returns null if the field is missing or has a different wire type.
///
/// # Safety
///
/// `msg` must be a valid pointer to a [`HewProtoMsg`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_proto_msg_get_string(
    msg: *const HewProtoMsg,
    field_number: u32,
) -> *mut c_char {
    if msg.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: msg is a valid HewProtoMsg pointer per caller contract.
    let m = unsafe { &*msg };
    match m.find_field(field_number) {
        Some(ProtoField {
            value: ProtoValue::Bytes(b),
            ..
        }) => {
            let len = b.len();
            // SAFETY: allocating len+1 bytes via malloc for NUL-terminated copy.
            let ptr = unsafe { libc::malloc(len + 1) }.cast::<u8>();
            if ptr.is_null() {
                return std::ptr::null_mut();
            }
            if len > 0 {
                // SAFETY: b.as_ptr() is valid for len bytes; ptr is freshly
                // allocated with len+1 bytes; regions do not overlap.
                unsafe { std::ptr::copy_nonoverlapping(b.as_ptr(), ptr, len) };
            }
            // SAFETY: ptr + len is within the allocated region.
            unsafe { *ptr.add(len) = 0 };
            ptr.cast::<c_char>()
        }
        _ => std::ptr::null_mut(),
    }
}

/// Check whether a field with the given number exists in the message.
/// Returns 1 if present, 0 otherwise.
///
/// # Safety
///
/// `msg` must be a valid pointer to a [`HewProtoMsg`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_proto_msg_has_field(
    msg: *const HewProtoMsg,
    field_number: u32,
) -> i32 {
    if msg.is_null() {
        return 0;
    }
    // SAFETY: msg is a valid HewProtoMsg pointer per caller contract.
    let m = unsafe { &*msg };
    i32::from(m.find_field(field_number).is_some())
}

/// Free a [`HewProtoMsg`] previously returned by [`hew_proto_msg_new`] or
/// [`hew_proto_msg_decode`].
///
/// # Safety
///
/// `msg` must be a pointer previously returned by a `hew_proto_msg_*`
/// constructor, and must not have been freed already.
#[no_mangle]
pub unsafe extern "C" fn hew_proto_msg_free(msg: *mut HewProtoMsg) {
    if msg.is_null() {
        return;
    }
    // SAFETY: msg was allocated with Box::into_raw and has not been freed.
    drop(unsafe { Box::from_raw(msg) });
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
        let msg = hew_proto_msg_new();
        assert!(!msg.is_null());

        // SAFETY: msg is a valid HewProtoMsg from hew_proto_msg_new.
        unsafe {
            hew_proto_msg_set_varint(msg, 1, 42);
            hew_proto_msg_set_varint(msg, 2, 1); // bool true
            hew_proto_msg_set_fixed64(msg, 3, 0xDEAD_BEEF_CAFE_BABE);
            hew_proto_msg_set_fixed32(msg, 4, 0x1234_5678);

            let hello = CString::new("hello").unwrap();
            hew_proto_msg_set_string(msg, 5, hello.as_ptr());

            let mut out_len: usize = 0;
            let encoded = hew_proto_msg_encode(msg, &mut out_len);
            assert!(!encoded.is_null());
            assert!(out_len > 0);

            let decoded = hew_proto_msg_decode(encoded, out_len);
            assert!(!decoded.is_null());

            assert_eq!(hew_proto_msg_get_varint(decoded, 1, 0), 42);
            assert_eq!(hew_proto_msg_get_varint(decoded, 2, 0), 1);
            assert_eq!(
                hew_proto_msg_get_fixed64(decoded, 3, 0),
                0xDEAD_BEEF_CAFE_BABE
            );
            assert_eq!(hew_proto_msg_get_fixed32(decoded, 4, 0), 0x1234_5678);

            let s = hew_proto_msg_get_string(decoded, 5);
            assert!(!s.is_null());
            let rs = std::ffi::CStr::from_ptr(s).to_str().unwrap();
            assert_eq!(rs, "hello");
            libc::free(s.cast());

            libc::free(encoded.cast());
            hew_proto_msg_free(decoded);
            hew_proto_msg_free(msg);
        }
    }

    #[test]
    fn varint_encoding_values() {
        let msg = hew_proto_msg_new();

        // SAFETY: msg is a valid HewProtoMsg from hew_proto_msg_new.
        unsafe {
            // Small value (single byte varint).
            hew_proto_msg_set_varint(msg, 1, 0);
            // Large value (multi-byte varint).
            hew_proto_msg_set_varint(msg, 2, u64::MAX);
            // Medium value.
            hew_proto_msg_set_varint(msg, 3, 300);

            let mut out_len: usize = 0;
            let encoded = hew_proto_msg_encode(msg, &mut out_len);
            assert!(!encoded.is_null());

            let decoded = hew_proto_msg_decode(encoded, out_len);
            assert!(!decoded.is_null());

            assert_eq!(hew_proto_msg_get_varint(decoded, 1, 99), 0);
            assert_eq!(hew_proto_msg_get_varint(decoded, 2, 0), u64::MAX);
            assert_eq!(hew_proto_msg_get_varint(decoded, 3, 0), 300);
            // Missing field returns default.
            assert_eq!(hew_proto_msg_get_varint(decoded, 99, 777), 777);

            libc::free(encoded.cast());
            hew_proto_msg_free(decoded);
            hew_proto_msg_free(msg);
        }
    }

    #[test]
    fn string_field_roundtrip() {
        let msg = hew_proto_msg_new();

        // SAFETY: msg is a valid HewProtoMsg from hew_proto_msg_new.
        unsafe {
            let name = CString::new("Hew language").unwrap();
            hew_proto_msg_set_string(msg, 1, name.as_ptr());

            let empty = CString::new("").unwrap();
            hew_proto_msg_set_string(msg, 2, empty.as_ptr());

            let mut out_len: usize = 0;
            let encoded = hew_proto_msg_encode(msg, &mut out_len);
            let decoded = hew_proto_msg_decode(encoded, out_len);
            assert!(!decoded.is_null());

            let s1 = hew_proto_msg_get_string(decoded, 1);
            assert!(!s1.is_null());
            assert_eq!(
                std::ffi::CStr::from_ptr(s1).to_str().unwrap(),
                "Hew language"
            );
            libc::free(s1.cast());

            let s2 = hew_proto_msg_get_string(decoded, 2);
            assert!(!s2.is_null());
            assert_eq!(std::ffi::CStr::from_ptr(s2).to_str().unwrap(), "");
            libc::free(s2.cast());

            // Missing string returns null.
            assert!(hew_proto_msg_get_string(decoded, 99).is_null());

            libc::free(encoded.cast());
            hew_proto_msg_free(decoded);
            hew_proto_msg_free(msg);
        }
    }

    #[test]
    fn bytes_field_roundtrip() {
        let msg = hew_proto_msg_new();

        // SAFETY: msg is a valid HewProtoMsg from hew_proto_msg_new.
        unsafe {
            let payload: &[u8] = &[0x00, 0xFF, 0x42, 0x13, 0x37];
            hew_proto_msg_set_bytes(msg, 10, payload.as_ptr(), payload.len());

            let mut out_len: usize = 0;
            let encoded = hew_proto_msg_encode(msg, &mut out_len);
            let decoded = hew_proto_msg_decode(encoded, out_len);
            assert!(!decoded.is_null());

            let mut bytes_len: usize = 0;
            let ptr = hew_proto_msg_get_bytes(decoded, 10, &mut bytes_len);
            assert!(!ptr.is_null());
            assert_eq!(bytes_len, 5);
            let result = std::slice::from_raw_parts(ptr, bytes_len);
            assert_eq!(result, payload);

            // Missing bytes returns null.
            let mut missing_len: usize = 0;
            assert!(hew_proto_msg_get_bytes(decoded, 99, &mut missing_len).is_null());
            assert_eq!(missing_len, 0);

            libc::free(encoded.cast());
            hew_proto_msg_free(decoded);
            hew_proto_msg_free(msg);
        }
    }

    #[test]
    fn nested_message_roundtrip() {
        // Encode an inner message, then embed it as bytes in an outer message.
        let inner = hew_proto_msg_new();

        // SAFETY: inner and outer are valid HewProtoMsg pointers.
        unsafe {
            hew_proto_msg_set_varint(inner, 1, 100);
            let tag = CString::new("nested").unwrap();
            hew_proto_msg_set_string(inner, 2, tag.as_ptr());

            // Encode inner.
            let mut inner_len: usize = 0;
            let inner_buf = hew_proto_msg_encode(inner, &mut inner_len);
            assert!(!inner_buf.is_null());

            // Embed inner as bytes field in outer.
            let outer = hew_proto_msg_new();
            hew_proto_msg_set_varint(outer, 1, 999);
            hew_proto_msg_set_bytes(outer, 2, inner_buf, inner_len);

            // Encode outer.
            let mut outer_len: usize = 0;
            let outer_buf = hew_proto_msg_encode(outer, &mut outer_len);
            assert!(!outer_buf.is_null());

            // Decode outer.
            let decoded_outer = hew_proto_msg_decode(outer_buf, outer_len);
            assert!(!decoded_outer.is_null());
            assert_eq!(hew_proto_msg_get_varint(decoded_outer, 1, 0), 999);

            // Extract and decode inner.
            let mut nested_len: usize = 0;
            let nested_ptr = hew_proto_msg_get_bytes(decoded_outer, 2, &mut nested_len);
            assert!(!nested_ptr.is_null());
            let decoded_inner = hew_proto_msg_decode(nested_ptr, nested_len);
            assert!(!decoded_inner.is_null());
            assert_eq!(hew_proto_msg_get_varint(decoded_inner, 1, 0), 100);

            let s = hew_proto_msg_get_string(decoded_inner, 2);
            assert!(!s.is_null());
            assert_eq!(std::ffi::CStr::from_ptr(s).to_str().unwrap(), "nested");
            libc::free(s.cast());

            hew_proto_msg_free(decoded_inner);
            hew_proto_msg_free(decoded_outer);
            libc::free(outer_buf.cast());
            hew_proto_msg_free(outer);
            libc::free(inner_buf.cast());
            hew_proto_msg_free(inner);
        }
    }

    #[test]
    fn has_field_check() {
        let msg = hew_proto_msg_new();

        // SAFETY: msg is a valid HewProtoMsg from hew_proto_msg_new.
        unsafe {
            assert_eq!(hew_proto_msg_has_field(msg, 1), 0);
            hew_proto_msg_set_varint(msg, 1, 42);
            assert_eq!(hew_proto_msg_has_field(msg, 1), 1);
            assert_eq!(hew_proto_msg_has_field(msg, 2), 0);

            // Null message returns 0.
            assert_eq!(hew_proto_msg_has_field(std::ptr::null(), 1), 0);

            hew_proto_msg_free(msg);
        }
    }

    #[test]
    fn decode_malformed_returns_null() {
        // SAFETY: testing with known invalid data.
        unsafe {
            // Truncated varint (high bit set, no continuation).
            let bad: &[u8] = &[0x80];
            assert!(hew_proto_msg_decode(bad.as_ptr(), bad.len()).is_null());

            // Null data.
            assert!(hew_proto_msg_decode(std::ptr::null(), 0).is_null());

            // Empty data.
            assert!(hew_proto_msg_decode([0u8; 0].as_ptr(), 0).is_null());
        }
    }

    #[test]
    fn overwrite_field_uses_last_value() {
        let msg = hew_proto_msg_new();

        // SAFETY: msg is a valid HewProtoMsg from hew_proto_msg_new.
        unsafe {
            hew_proto_msg_set_varint(msg, 1, 10);
            hew_proto_msg_set_varint(msg, 1, 20);
            assert_eq!(hew_proto_msg_get_varint(msg, 1, 0), 20);
            hew_proto_msg_free(msg);
        }
    }
}
