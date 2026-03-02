//! Hew Binary Format (HBF) wire encoding.
//!
//! Implements varint (unsigned LEB128), zigzag encoding, TLV field encoding,
//! an HBF header, and actor-message envelopes. All types use `#[repr(C)]` to
//! match the C runtime layout exactly.

use std::ffi::{c_char, c_int, c_void};

use crate::set_last_error;

// ---------------------------------------------------------------------------
// Wire type constants
// ---------------------------------------------------------------------------

/// Wire type: unsigned LEB128 varint.
pub const HEW_WIRE_VARINT: u32 = 0;
/// Wire type: 64-bit little-endian fixed.
pub const HEW_WIRE_FIXED64: u32 = 1;
/// Wire type: length-delimited bytes.
pub const HEW_WIRE_LENGTH_DELIMITED: u32 = 2;
/// Wire type: 32-bit little-endian fixed.
pub const HEW_WIRE_FIXED32: u32 = 5;

const INITIAL_BUF_CAP: usize = 64;
const HBF_HEADER_LEN: usize = 10;
const MAX_MSG_TYPE: i64 = 65_535;

/// HBF message magic bytes (`"HEW1"`).
pub const HBF_MAGIC: [u8; 4] = *b"HEW1";
/// HBF wire format version.
pub const HBF_VERSION: u8 = 0x01;
/// HBF compressed payload flag (bit 0).
pub const HBF_FLAG_COMPRESSED: u8 = 0x01;

// ---------------------------------------------------------------------------
// Wire buffer
// ---------------------------------------------------------------------------

/// Growable byte buffer used for encoding and decoding wire data.
#[repr(C)]
#[derive(Debug)]
pub struct HewWireBuf {
    /// Pointer to the data.
    pub data: *mut u8,
    /// Number of valid bytes.
    pub len: usize,
    /// Allocated capacity (0 if not owned).
    pub cap: usize,
    /// Current read cursor.
    pub read_pos: usize,
}

impl HewWireBuf {
    /// Ensure there is room for at least `additional` more bytes.
    ///
    /// # Safety
    ///
    /// `self.data` must be either null (cap == 0) or a `libc::malloc`-allocated
    /// buffer of at least `self.cap` bytes.
    unsafe fn ensure_capacity(&mut self, additional: usize) -> bool {
        let Some(required) = self.len.checked_add(additional) else {
            return false;
        };
        if required <= self.cap {
            return true;
        }
        let mut new_cap = if self.cap == 0 {
            INITIAL_BUF_CAP
        } else {
            self.cap
        };
        while new_cap < required {
            let Some(c) = new_cap.checked_mul(2) else {
                return false;
            };
            new_cap = c;
        }
        // SAFETY: realloc with a valid (or null) pointer and positive size.
        let new_data = unsafe { libc::realloc(self.data.cast::<c_void>(), new_cap) };
        if new_data.is_null() {
            return false;
        }
        self.data = new_data.cast::<u8>();
        self.cap = new_cap;
        true
    }

    /// Append a single byte.
    ///
    /// # Safety
    ///
    /// Same as [`ensure_capacity`](Self::ensure_capacity).
    unsafe fn push(&mut self, byte: u8) -> bool {
        // SAFETY: forwarded to ensure_capacity.
        if !unsafe { self.ensure_capacity(1) } {
            return false;
        }
        // SAFETY: we just ensured capacity; data is valid for self.len+1 bytes.
        unsafe {
            *self.data.add(self.len) = byte;
        }
        self.len += 1;
        true
    }

    /// Append a slice of bytes.
    ///
    /// # Safety
    ///
    /// Same as [`ensure_capacity`](Self::ensure_capacity). `src` must be valid
    /// for `count` bytes.
    unsafe fn push_bytes(&mut self, src: *const u8, count: usize) -> bool {
        if count == 0 {
            return true;
        }
        // SAFETY: forwarded to ensure_capacity.
        if !unsafe { self.ensure_capacity(count) } {
            return false;
        }
        // SAFETY: we ensured capacity; src is valid per caller contract.
        unsafe {
            std::ptr::copy_nonoverlapping(src, self.data.add(self.len), count);
        }
        self.len += count;
        true
    }

    /// Read `count` bytes at the current read position (without copying).
    fn peek(&self, count: usize) -> Option<*const u8> {
        let end = self.read_pos.checked_add(count)?;
        if end > self.len {
            return None;
        }
        if self.data.is_null() {
            return None;
        }
        // SAFETY: read_pos + count <= len and data is non-null.
        Some(unsafe { self.data.add(self.read_pos) })
    }
}

// ---------------------------------------------------------------------------
// Wire buffer lifecycle
// ---------------------------------------------------------------------------

/// Zero-initialise a wire buffer.
///
/// # Safety
///
/// `buf` must point to a valid, writable [`HewWireBuf`].
#[no_mangle]
pub unsafe extern "C" fn hew_wire_buf_init(buf: *mut HewWireBuf) {
    cabi_guard!(buf.is_null());
    // SAFETY: caller guarantees `buf` is valid.
    unsafe {
        (*buf).data = std::ptr::null_mut();
        (*buf).len = 0;
        (*buf).cap = 0;
        (*buf).read_pos = 0;
    }
}

/// Allocate and initialise a heap-owned wire buffer.
///
/// Returns null on allocation failure.
///
/// # Safety
///
/// The returned pointer must be released with [`hew_wire_buf_destroy`].
#[no_mangle]
pub unsafe extern "C" fn hew_wire_buf_new() -> *mut HewWireBuf {
    // SAFETY: malloc is called with the exact size of HewWireBuf.
    let buf = unsafe { libc::malloc(std::mem::size_of::<HewWireBuf>()) }.cast::<HewWireBuf>();
    if buf.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: `buf` points to newly allocated writable memory.
    unsafe { hew_wire_buf_init(buf) };
    buf
}

/// Free the buffer's data.
///
/// # Safety
///
/// `buf` must point to a valid [`HewWireBuf`] whose `data` was allocated by
/// `libc::malloc`/`libc::realloc` (or is null).
#[no_mangle]
pub unsafe extern "C" fn hew_wire_buf_free(buf: *mut HewWireBuf) {
    cabi_guard!(buf.is_null());
    // SAFETY: caller guarantees the pointer is valid.
    let b = unsafe { &mut *buf };
    if !b.data.is_null() && b.cap > 0 {
        // SAFETY: data was allocated with libc::malloc/realloc; cap > 0 means owned.
        unsafe {
            libc::free(b.data.cast::<c_void>());
        }
    }
    b.data = std::ptr::null_mut();
    b.len = 0;
    b.cap = 0;
    b.read_pos = 0;
}

/// Free a heap-owned wire buffer and its internal allocation.
///
/// # Safety
///
/// `buf` must be either null or a pointer returned by [`hew_wire_buf_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_wire_buf_destroy(buf: *mut HewWireBuf) {
    cabi_guard!(buf.is_null());
    // SAFETY: `buf` is valid per caller contract.
    unsafe { hew_wire_buf_free(buf) };
    // SAFETY: `buf` was allocated by hew_wire_buf_new via libc::malloc.
    unsafe {
        libc::free(buf.cast::<c_void>());
    }
}

/// Reset the buffer for reuse (keeps the allocation).
///
/// # Safety
///
/// `buf` must point to a valid [`HewWireBuf`].
#[no_mangle]
pub unsafe extern "C" fn hew_wire_buf_reset(buf: *mut HewWireBuf) {
    cabi_guard!(buf.is_null());
    // SAFETY: caller guarantees the pointer is valid.
    unsafe {
        (*buf).len = 0;
        (*buf).read_pos = 0;
    }
}

/// Initialise a buffer for reading from an existing byte slice.
///
/// # Safety
///
/// `buf` must point to a valid [`HewWireBuf`]. `data` must be valid for `len`
/// bytes and must outlive the buffer's use.
#[no_mangle]
pub unsafe extern "C" fn hew_wire_buf_init_read(buf: *mut HewWireBuf, data: *const u8, len: usize) {
    cabi_guard!(buf.is_null());
    // SAFETY: caller guarantees `buf` is valid.
    unsafe {
        (*buf).data = data.cast_mut();
        (*buf).len = len;
        (*buf).cap = 0; // not owned — must not free/realloc
        (*buf).read_pos = 0;
    }
}

// ---------------------------------------------------------------------------
// Varint (unsigned LEB128)
// ---------------------------------------------------------------------------

/// Encode a `u64` as an unsigned LEB128 varint.
///
/// Returns 0 on success, -1 on error.
///
/// # Safety
///
/// `buf` must point to a valid, writable [`HewWireBuf`] with malloc-backed data.
#[no_mangle]
pub unsafe extern "C" fn hew_wire_encode_varint(buf: *mut HewWireBuf, value: u64) -> c_int {
    if buf.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees `buf` is valid.
    let b = unsafe { &mut *buf };
    let mut v = value;
    loop {
        let mut byte = (v & 0x7F) as u8;
        v >>= 7;
        if v != 0 {
            byte |= 0x80;
        }
        // SAFETY: forwarded to HewWireBuf::push.
        if !unsafe { b.push(byte) } {
            return -1;
        }
        if v == 0 {
            break;
        }
    }
    0
}

/// Decode an unsigned LEB128 varint from the buffer.
///
/// Returns 0 on success, -1 on error.
///
/// # Safety
///
/// `buf` must point to a valid [`HewWireBuf`]. `out` must be non-null and
/// writable.
#[no_mangle]
pub unsafe extern "C" fn hew_wire_decode_varint(buf: *mut HewWireBuf, out: *mut u64) -> c_int {
    if buf.is_null() || out.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees both pointers are valid.
    let b = unsafe { &mut *buf };
    let mut result: u64 = 0;
    let mut shift: u32 = 0;
    let mut num_bytes: u8 = 0;
    let mut tenth_byte: Option<u8> = None;
    loop {
        if shift >= 64 {
            return -1; // overflow
        }
        let Some(ptr) = b.peek(1) else {
            return -1; // underflow
        };
        // SAFETY: peek confirmed at least 1 byte is available.
        let byte = unsafe { *ptr };
        b.read_pos += 1;
        num_bytes += 1;
        if num_bytes == 10 {
            tenth_byte = Some(byte & 0x7F);
        }
        result |= u64::from(byte & 0x7F) << shift;
        if byte & 0x80 == 0 {
            break;
        }
        shift += 7;
    }
    if tenth_byte.is_some_and(|b| b > 1) {
        set_last_error("non-canonical varint encoding");
        return -1;
    }
    // SAFETY: caller guarantees `out` is writable.
    unsafe {
        *out = result;
    }
    0
}

// ---------------------------------------------------------------------------
// Zigzag encoding
// ---------------------------------------------------------------------------

/// Zigzag-encode a signed 64-bit integer.
#[no_mangle]
pub extern "C" fn hew_wire_zigzag_encode(n: i64) -> u64 {
    ((n << 1) ^ (n >> 63)).cast_unsigned()
}

/// Zigzag-decode an unsigned 64-bit integer back to signed.
#[no_mangle]
pub extern "C" fn hew_wire_zigzag_decode(n: u64) -> i64 {
    (n >> 1).cast_signed() ^ (-((n & 1).cast_signed()))
}

// ---------------------------------------------------------------------------
// TLV field encoding helpers
// ---------------------------------------------------------------------------

/// Build a TLV tag from field number and wire type.
#[expect(
    clippy::cast_sign_loss,
    reason = "field_num is always non-negative in practice"
)]
fn make_tag(field_num: c_int, wire_type: u32) -> u64 {
    ((field_num as u64) << 3) | u64::from(wire_type)
}

/// Encode a varint TLV field.
///
/// # Safety
///
/// `buf` must point to a valid, writable [`HewWireBuf`].
#[no_mangle]
pub unsafe extern "C" fn hew_wire_encode_field_varint(
    buf: *mut HewWireBuf,
    field_num: c_int,
    value: u64,
) -> c_int {
    // SAFETY: forwarded to hew_wire_encode_varint.
    if unsafe { hew_wire_encode_varint(buf, make_tag(field_num, HEW_WIRE_VARINT)) } != 0 {
        return -1;
    }
    // SAFETY: forwarded.
    unsafe { hew_wire_encode_varint(buf, value) }
}

/// Encode a fixed-32 TLV field (little-endian).
///
/// # Safety
///
/// `buf` must point to a valid, writable [`HewWireBuf`].
#[no_mangle]
pub unsafe extern "C" fn hew_wire_encode_field_fixed32(
    buf: *mut HewWireBuf,
    field_num: c_int,
    value: u32,
) -> c_int {
    // SAFETY: forwarded.
    if unsafe { hew_wire_encode_varint(buf, make_tag(field_num, HEW_WIRE_FIXED32)) } != 0 {
        return -1;
    }
    if buf.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees `buf` is valid.
    let b = unsafe { &mut *buf };
    let bytes = value.to_le_bytes();
    // SAFETY: bytes is a stack array with known length.
    if !unsafe { b.push_bytes(bytes.as_ptr(), 4) } {
        return -1;
    }
    0
}

/// Encode a fixed-64 TLV field (little-endian).
///
/// # Safety
///
/// `buf` must point to a valid, writable [`HewWireBuf`].
#[no_mangle]
pub unsafe extern "C" fn hew_wire_encode_field_fixed64(
    buf: *mut HewWireBuf,
    field_num: c_int,
    value: u64,
) -> c_int {
    // SAFETY: forwarded.
    if unsafe { hew_wire_encode_varint(buf, make_tag(field_num, HEW_WIRE_FIXED64)) } != 0 {
        return -1;
    }
    if buf.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees `buf` is valid.
    let b = unsafe { &mut *buf };
    let bytes = value.to_le_bytes();
    // SAFETY: bytes is a stack array with known length.
    if !unsafe { b.push_bytes(bytes.as_ptr(), 8) } {
        return -1;
    }
    0
}

/// Encode a length-delimited bytes TLV field.
///
/// # Safety
///
/// `buf` must point to a valid, writable [`HewWireBuf`]. `data` must be valid
/// for `len` bytes (or null when `len` is 0).
#[no_mangle]
pub unsafe extern "C" fn hew_wire_encode_field_bytes(
    buf: *mut HewWireBuf,
    field_num: c_int,
    data: *const c_void,
    len: usize,
) -> c_int {
    // SAFETY: forwarded.
    if unsafe { hew_wire_encode_varint(buf, make_tag(field_num, HEW_WIRE_LENGTH_DELIMITED)) } != 0 {
        return -1;
    }
    // SAFETY: forwarded.
    if unsafe { hew_wire_encode_varint(buf, len as u64) } != 0 {
        return -1;
    }
    if len == 0 {
        return 0;
    }
    if buf.is_null() || data.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees `buf` and `data` are valid.
    let b = unsafe { &mut *buf };
    // SAFETY: `data` is valid for `len` bytes per caller contract.
    if !unsafe { b.push_bytes(data.cast::<u8>(), len) } {
        return -1;
    }
    0
}

/// Encode a NUL-terminated C string as a length-delimited TLV field.
///
/// # Safety
///
/// `buf` must point to a valid, writable [`HewWireBuf`]. `str_ptr` must be a
/// valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_wire_encode_field_string(
    buf: *mut HewWireBuf,
    field_num: c_int,
    str_ptr: *const c_char,
) -> c_int {
    if str_ptr.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees `str_ptr` is a valid C string.
    let len = unsafe { libc::strlen(str_ptr) };
    // SAFETY: forwarded.
    unsafe { hew_wire_encode_field_bytes(buf, field_num, str_ptr.cast::<c_void>(), len) }
}

// ---------------------------------------------------------------------------
// Fixed-width decode
// ---------------------------------------------------------------------------

/// Decode a little-endian `u32`.
///
/// # Safety
///
/// `buf` and `out` must be valid, non-null pointers.
#[no_mangle]
pub unsafe extern "C" fn hew_wire_decode_fixed32(buf: *mut HewWireBuf, out: *mut u32) -> c_int {
    if buf.is_null() || out.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees pointers are valid.
    let b = unsafe { &mut *buf };
    let Some(ptr) = b.peek(4) else { return -1 };
    let mut bytes = [0u8; 4];
    // SAFETY: peek confirmed 4 bytes available.
    unsafe {
        std::ptr::copy_nonoverlapping(ptr, bytes.as_mut_ptr(), 4);
    }
    b.read_pos += 4;
    // SAFETY: caller guarantees `out` is writable.
    unsafe {
        *out = u32::from_le_bytes(bytes);
    }
    0
}

/// Decode a little-endian `u64`.
///
/// # Safety
///
/// `buf` and `out` must be valid, non-null pointers.
#[no_mangle]
pub unsafe extern "C" fn hew_wire_decode_fixed64(buf: *mut HewWireBuf, out: *mut u64) -> c_int {
    if buf.is_null() || out.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees pointers are valid.
    let b = unsafe { &mut *buf };
    let Some(ptr) = b.peek(8) else { return -1 };
    let mut bytes = [0u8; 8];
    // SAFETY: peek confirmed 8 bytes available.
    unsafe {
        std::ptr::copy_nonoverlapping(ptr, bytes.as_mut_ptr(), 8);
    }
    b.read_pos += 8;
    // SAFETY: caller guarantees `out` is writable.
    unsafe {
        *out = u64::from_le_bytes(bytes);
    }
    0
}

/// Decode a length-delimited byte sequence.
///
/// Sets `*out` to point into the buffer's data (zero-copy) and `*out_len` to
/// the length. The returned pointer is only valid while the buffer is alive.
///
/// # Safety
///
/// `buf`, `out`, and `out_len` must be valid, non-null pointers.
#[no_mangle]
pub unsafe extern "C" fn hew_wire_decode_bytes(
    buf: *mut HewWireBuf,
    out: *mut *const c_void,
    out_len: *mut usize,
) -> c_int {
    if buf.is_null() || out.is_null() || out_len.is_null() {
        return -1;
    }
    let mut length: u64 = 0;
    // SAFETY: forwarded; `length` is a valid local.
    if unsafe { hew_wire_decode_varint(buf, &raw mut length) } != 0 {
        return -1;
    }
    #[expect(
        clippy::cast_possible_truncation,
        reason = "wire payloads bounded by buffer size which fits in usize"
    )]
    let len = length as usize;
    // SAFETY: caller guarantees `buf` is valid.
    let b = unsafe { &mut *buf };
    let Some(ptr) = b.peek(len) else { return -1 };
    b.read_pos += len;
    // SAFETY: caller guarantees `out` and `out_len` are writable.
    unsafe {
        *out = ptr.cast::<c_void>();
        *out_len = len;
    }
    0
}

// ---------------------------------------------------------------------------
// HBF header
// ---------------------------------------------------------------------------

/// Decoded HBF message header fields.
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct HewWireHeader {
    /// HBF wire format version.
    pub version: u8,
    /// Header flags.
    pub flags: u8,
    /// Length of the payload in bytes.
    pub payload_len: u32,
}

/// Encode and allocate a 10-byte HBF header.
///
/// Layout: `magic(4) | version(1) | flags(1) | payload_len(4, LE)`.
///
/// Returns a `libc::malloc`-allocated buffer on success, or null on failure.
///
/// # Safety
///
/// The returned pointer must be freed by the caller using `libc::free`.
#[no_mangle]
pub unsafe extern "C" fn hew_wire_encode_header(payload_len: u32, flags: u8) -> *mut u8 {
    // SAFETY: malloc is called with a positive byte size.
    let header = unsafe { libc::malloc(HBF_HEADER_LEN) }.cast::<u8>();
    if header.is_null() {
        return std::ptr::null_mut();
    }
    let payload_len_le = payload_len.to_le_bytes();
    let valid_flags = flags & HBF_FLAG_COMPRESSED;
    // SAFETY: `header` points to `HBF_HEADER_LEN` writable bytes.
    unsafe {
        std::ptr::copy_nonoverlapping(HBF_MAGIC.as_ptr(), header, HBF_MAGIC.len());
        *header.add(4) = HBF_VERSION;
        *header.add(5) = valid_flags;
        std::ptr::copy_nonoverlapping(payload_len_le.as_ptr(), header.add(6), payload_len_le.len());
    }
    header
}

/// Decode an HBF header from raw bytes.
///
/// Returns a zeroed header when validation fails.
///
/// # Safety
///
/// `data` must be valid for `len` bytes.
#[no_mangle]
pub unsafe extern "C" fn hew_wire_decode_header(data: *const u8, len: usize) -> HewWireHeader {
    // SAFETY: forwarded pointer validity contract.
    if unsafe { hew_wire_validate_header(data, len) } == 0 {
        return HewWireHeader::default();
    }
    // SAFETY: validation above guarantees non-null and at least `HBF_HEADER_LEN` bytes.
    let bytes = unsafe { std::slice::from_raw_parts(data, HBF_HEADER_LEN) };
    let payload_len = u32::from_le_bytes([bytes[6], bytes[7], bytes[8], bytes[9]]);
    HewWireHeader {
        version: bytes[4],
        flags: bytes[5],
        payload_len,
    }
}

/// Validate whether bytes begin with a valid HBF header.
///
/// Returns 1 if valid, 0 otherwise.
///
/// # Safety
///
/// `data` must be valid for `len` bytes.
#[no_mangle]
pub unsafe extern "C" fn hew_wire_validate_header(data: *const u8, len: usize) -> i32 {
    if data.is_null() || len < HBF_HEADER_LEN {
        return 0;
    }
    // SAFETY: `data` is non-null and `len >= HBF_HEADER_LEN`.
    let bytes = unsafe { std::slice::from_raw_parts(data, HBF_HEADER_LEN) };
    if bytes[..4] != HBF_MAGIC {
        return 0;
    }
    if bytes[4] != HBF_VERSION {
        return 0;
    }
    if (bytes[5] & !HBF_FLAG_COMPRESSED) != 0 {
        return 0;
    }
    1
}

/// Write a 10-byte HBF header.
///
/// Layout: `magic(4) | version(1) | flags(1) | payload_len(4, LE)`.
///
/// # Safety
///
/// `buf` must point to a valid, writable [`HewWireBuf`].
#[no_mangle]
pub unsafe extern "C" fn hew_wire_write_hbf_header(
    buf: *mut HewWireBuf,
    flags: u8,
    payload_len: u32,
) -> c_int {
    if buf.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees `buf` is valid.
    let b = unsafe { &mut *buf };
    // SAFETY: returns a malloc-allocated header pointer or null.
    let header = unsafe { hew_wire_encode_header(payload_len, flags) };
    if header.is_null() {
        return -1;
    }
    // SAFETY: header points to `HBF_HEADER_LEN` bytes.
    let ok = unsafe { b.push_bytes(header.cast_const(), HBF_HEADER_LEN) };
    // SAFETY: header came from libc::malloc.
    unsafe {
        libc::free(header.cast::<c_void>());
    }
    if !ok {
        return -1;
    }
    0
}

// ---------------------------------------------------------------------------
// Envelope
// ---------------------------------------------------------------------------

/// Actor-message envelope for wire transport.
#[repr(C)]
#[derive(Debug)]
pub struct HewWireEnvelope {
    /// Target actor identifier.
    pub target_actor_id: u64,
    /// Source actor identifier.
    pub source_actor_id: u64,
    /// Message type tag.
    pub msg_type: i32,
    /// Payload byte count.
    pub payload_size: u32,
    /// Pointer to payload bytes.
    pub payload: *mut u8,
}

/// Encode an envelope into the wire buffer.
///
/// Fields: `target_id` (1, varint), `source_id` (2, varint),
/// `msg_type` (3, varint), payload (4, bytes).
///
/// # Safety
///
/// `buf` and `env` must be valid, non-null pointers. `env.payload` must be
/// valid for `env.payload_size` bytes (or null when size is 0).
#[no_mangle]
pub unsafe extern "C" fn hew_wire_encode_envelope(
    buf: *mut HewWireBuf,
    env: *const HewWireEnvelope,
) -> c_int {
    if buf.is_null() || env.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees `env` is valid.
    let e = unsafe { &*env };
    let msg_type = i64::from(e.msg_type);
    if !(0..=MAX_MSG_TYPE).contains(&msg_type) {
        set_last_error(&format!("msg_type {} out of valid range", e.msg_type));
        return -1;
    }

    // Field 1: target_actor_id (varint)
    // SAFETY: forwarded.
    if unsafe { hew_wire_encode_field_varint(buf, 1, e.target_actor_id) } != 0 {
        return -1;
    }
    // Field 2: source_actor_id (varint)
    // SAFETY: forwarded.
    if unsafe { hew_wire_encode_field_varint(buf, 2, e.source_actor_id) } != 0 {
        return -1;
    }
    // Field 3: msg_type (varint, zigzag-encoded since it's signed)
    // SAFETY: forwarded.
    if unsafe {
        hew_wire_encode_field_varint(buf, 3, hew_wire_zigzag_encode(i64::from(e.msg_type)))
    } != 0
    {
        return -1;
    }
    // Field 4: payload (bytes)
    // SAFETY: forwarded; payload validity is caller's responsibility.
    if unsafe {
        hew_wire_encode_field_bytes(buf, 4, e.payload.cast::<c_void>(), e.payload_size as usize)
    } != 0
    {
        return -1;
    }
    0
}

/// Decode an envelope from the wire buffer.
///
/// # Safety
///
/// `buf` and `env` must be valid, non-null pointers.
#[no_mangle]
#[expect(
    clippy::too_many_lines,
    reason = "TLV field dispatch is inherently verbose"
)]
pub unsafe extern "C" fn hew_wire_decode_envelope(
    buf: *mut HewWireBuf,
    env: *mut HewWireEnvelope,
) -> c_int {
    if buf.is_null() || env.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees `env` is valid.
    let e = unsafe { &mut *env };
    e.target_actor_id = 0;
    e.source_actor_id = 0;
    e.msg_type = 0;
    e.payload_size = 0;
    e.payload = std::ptr::null_mut();

    // Decode TLV fields until we run out of data.
    loop {
        // SAFETY: caller guarantees `buf` is valid. Re-read each iteration
        // because decode functions mutate read_pos through the raw pointer.
        let read_pos = unsafe { (*buf).read_pos };
        // SAFETY: same — reading len through the raw pointer.
        let len = unsafe { (*buf).len };
        if read_pos >= len {
            break;
        }

        let mut tag: u64 = 0;
        // SAFETY: forwarded; `tag` is a valid local.
        if unsafe { hew_wire_decode_varint(buf, &raw mut tag) } != 0 {
            return -1;
        }
        let field_num = tag >> 3;
        let wire_type = (tag & 0x07) as u32;

        match (field_num, wire_type) {
            (1, w) if w == HEW_WIRE_VARINT => {
                let mut v: u64 = 0;
                // SAFETY: forwarded.
                if unsafe { hew_wire_decode_varint(buf, &raw mut v) } != 0 {
                    return -1;
                }
                e.target_actor_id = v;
            }
            (2, w) if w == HEW_WIRE_VARINT => {
                let mut v: u64 = 0;
                // SAFETY: forwarded.
                if unsafe { hew_wire_decode_varint(buf, &raw mut v) } != 0 {
                    return -1;
                }
                e.source_actor_id = v;
            }
            (3, w) if w == HEW_WIRE_VARINT => {
                let mut v: u64 = 0;
                // SAFETY: forwarded.
                if unsafe { hew_wire_decode_varint(buf, &raw mut v) } != 0 {
                    return -1;
                }
                let msg_type = hew_wire_zigzag_decode(v);
                if !(0..=MAX_MSG_TYPE).contains(&msg_type) {
                    set_last_error(&format!("invalid msg_type: {msg_type}"));
                    return -1;
                }
                #[expect(
                    clippy::cast_possible_truncation,
                    reason = "validated msg_type fits within i32"
                )]
                {
                    e.msg_type = msg_type as i32;
                }
            }
            (4, w) if w == HEW_WIRE_LENGTH_DELIMITED => {
                let mut data_ptr: *const c_void = std::ptr::null();
                let mut data_len: usize = 0;
                // SAFETY: forwarded; locals are valid.
                if unsafe { hew_wire_decode_bytes(buf, &raw mut data_ptr, &raw mut data_len) } != 0
                {
                    return -1;
                }
                e.payload = data_ptr.cast_mut().cast::<u8>();
                #[expect(
                    clippy::cast_possible_truncation,
                    reason = "payload_size is u32; wire payloads are bounded"
                )]
                {
                    e.payload_size = data_len as u32;
                }
            }
            // Unknown field — skip based on wire type.
            (_, w) if w == HEW_WIRE_VARINT => {
                let mut skip: u64 = 0;
                // SAFETY: forwarded.
                if unsafe { hew_wire_decode_varint(buf, &raw mut skip) } != 0 {
                    return -1;
                }
            }
            (_, w) if w == HEW_WIRE_FIXED32 => {
                // SAFETY: caller guarantees `buf` is valid — reading position.
                let rp = unsafe { (*buf).read_pos };
                // SAFETY: caller guarantees `buf` is valid — reading length.
                let bl = unsafe { (*buf).len };
                if bl.saturating_sub(rp) < 4 {
                    return -1;
                }
                // SAFETY: caller guarantees `buf` is valid.
                unsafe {
                    (*buf).read_pos += 4;
                }
            }
            (_, w) if w == HEW_WIRE_FIXED64 => {
                // SAFETY: caller guarantees `buf` is valid — reading position.
                let rp = unsafe { (*buf).read_pos };
                // SAFETY: caller guarantees `buf` is valid — reading length.
                let bl = unsafe { (*buf).len };
                if bl.saturating_sub(rp) < 8 {
                    return -1;
                }
                // SAFETY: caller guarantees `buf` is valid.
                unsafe {
                    (*buf).read_pos += 8;
                }
            }
            (_, w) if w == HEW_WIRE_LENGTH_DELIMITED => {
                let mut skip_ptr: *const c_void = std::ptr::null();
                let mut skip_len: usize = 0;
                // SAFETY: forwarded.
                if unsafe { hew_wire_decode_bytes(buf, &raw mut skip_ptr, &raw mut skip_len) } != 0
                {
                    return -1;
                }
            }
            _ => return -1, // unknown wire type
        }
    }
    0
}

// ---------------------------------------------------------------------------
// TLV helpers for the new #[wire] struct decoder
// ---------------------------------------------------------------------------

/// Decode a tag from the wire buffer. The tag is a varint encoding
/// `(field_number << 3) | wire_type`. Writes the field number to `field_num`
/// and the wire type to `wire_type`.
///
/// Returns 0 on success, -1 on error.
///
/// # Safety
///
/// `buf` must point to a valid, initialized `HewWireBuf`.
/// `field_num` and `wire_type` must be valid pointers.
#[no_mangle]
pub unsafe extern "C" fn hew_wire_decode_tag(
    buf: *mut HewWireBuf,
    field_num: *mut u32,
    wire_type: *mut u32,
) -> c_int {
    let mut tag: u64 = 0;
    // SAFETY: caller guarantees `buf` is valid.
    if unsafe { hew_wire_decode_varint(buf, &raw mut tag) } != 0 {
        return -1;
    }
    // SAFETY: caller guarantees pointers are valid.
    unsafe {
        *field_num = (tag >> 3) as u32;
        *wire_type = (tag & 7) as u32;
    }
    0
}

/// Skip a field in the wire buffer based on its wire type.
/// This is used for forward compatibility — unknown fields are silently skipped.
///
/// Returns 0 on success, -1 on error (e.g. truncated buffer).
///
/// # Safety
///
/// `buf` must point to a valid, initialized `HewWireBuf`.
#[no_mangle]
pub unsafe extern "C" fn hew_wire_skip_field(buf: *mut HewWireBuf, wire_type: u32) -> c_int {
    match wire_type {
        HEW_WIRE_VARINT => {
            let mut skip: u64 = 0;
            // SAFETY: caller guarantees `buf` is valid.
            if unsafe { hew_wire_decode_varint(buf, &raw mut skip) } != 0 {
                return -1;
            }
            0
        }
        HEW_WIRE_FIXED32 => {
            // SAFETY: caller guarantees `buf` is valid.
            let rp = unsafe { (*buf).read_pos };
            let bl = unsafe { (*buf).len };
            if bl.saturating_sub(rp) < 4 {
                return -1;
            }
            // SAFETY: caller guarantees `buf` is valid.
            unsafe {
                (*buf).read_pos += 4;
            }
            0
        }
        HEW_WIRE_FIXED64 => {
            // SAFETY: caller guarantees `buf` is valid.
            let rp = unsafe { (*buf).read_pos };
            let bl = unsafe { (*buf).len };
            if bl.saturating_sub(rp) < 8 {
                return -1;
            }
            // SAFETY: caller guarantees `buf` is valid.
            unsafe {
                (*buf).read_pos += 8;
            }
            0
        }
        HEW_WIRE_LENGTH_DELIMITED => {
            let mut skip_ptr: *const c_void = std::ptr::null();
            let mut skip_len: usize = 0;
            // SAFETY: caller guarantees `buf` is valid.
            if unsafe { hew_wire_decode_bytes(buf, &raw mut skip_ptr, &raw mut skip_len) } != 0 {
                return -1;
            }
            0
        }
        _ => -1, // unknown wire type
    }
}

/// Get the data pointer from a wire buffer.
///
/// # Safety
///
/// `buf` must point to a valid, initialized `HewWireBuf`.
#[no_mangle]
pub unsafe extern "C" fn hew_wire_buf_data(buf: *const HewWireBuf) -> *const u8 {
    // SAFETY: caller guarantees `buf` is valid.
    unsafe { (*buf).data }
}

/// Get the data length from a wire buffer.
///
/// # Safety
///
/// `buf` must point to a valid, initialized `HewWireBuf`.
#[no_mangle]
pub unsafe extern "C" fn hew_wire_buf_len(buf: *const HewWireBuf) -> usize {
    // SAFETY: caller guarantees `buf` is valid.
    unsafe { (*buf).len }
}

/// Check if the wire buffer has remaining data to read.
/// Returns 1 if `read_pos < len`, 0 otherwise.
///
/// # Safety
///
/// `buf` must point to a valid, initialized `HewWireBuf`.
#[no_mangle]
pub unsafe extern "C" fn hew_wire_buf_has_remaining(buf: *const HewWireBuf) -> i32 {
    // SAFETY: caller guarantees `buf` is valid.
    let b = unsafe { &*buf };
    if b.read_pos < b.len {
        1
    } else {
        0
    }
}

// ---------------------------------------------------------------------------
// HewVec-ABI wrappers (used by std/wire.hew)
// ---------------------------------------------------------------------------

/// Encode a 10-byte HBF header into a `bytes` `HewVec`.
///
/// `payload_len` is the byte length of the message payload.
/// `flags` controls optional features (e.g. compression).
///
/// # Safety
///
/// None — all memory is managed by the runtime allocator.
#[no_mangle]
pub unsafe extern "C" fn hew_wire_encode_header_hew(
    payload_len: i32,
    flags: i32,
) -> *mut crate::vec::HewVec {
    #[expect(
        clippy::cast_sign_loss,
        clippy::cast_possible_truncation,
        reason = "C ABI: values are non-negative and fit in target types"
    )]
    // SAFETY: hew_wire_encode_header is a pure function that allocates or returns null.
    let raw = unsafe { hew_wire_encode_header(payload_len as u32, flags as u8) };
    if raw.is_null() {
        // SAFETY: hew_vec_new allocates a valid empty HewVec.
        return unsafe { crate::vec::hew_vec_new() };
    }
    // SAFETY: hew_wire_encode_header returns exactly HBF_HEADER_LEN bytes.
    let slice = unsafe { std::slice::from_raw_parts(raw, HBF_HEADER_LEN) };
    // SAFETY: slice is valid.
    let result = unsafe { crate::vec::u8_to_hwvec(slice) };
    // SAFETY: raw was allocated by hew_wire_encode_header via libc::malloc.
    unsafe { libc::free(raw.cast()) };
    result
}

/// Validate and decode the payload length from a `bytes` `HewVec` HBF header.
///
/// Returns the payload length on success, or -1 if the header is invalid.
///
/// # Safety
///
/// `v` must be a valid, non-null pointer to a `HewVec` (i32 elements).
#[no_mangle]
pub unsafe extern "C" fn hew_wire_decode_header_hew(v: *mut crate::vec::HewVec) -> i64 {
    // SAFETY: v validity forwarded to hwvec_to_u8.
    let bytes = unsafe { crate::vec::hwvec_to_u8(v) };
    // SAFETY: bytes slice is valid for its length.
    let hdr = unsafe { hew_wire_decode_header(bytes.as_ptr(), bytes.len()) };
    if hdr.version == 0 && hdr.flags == 0 && hdr.payload_len == 0 {
        return -1;
    }
    i64::from(hdr.payload_len)
}

/// Validate that a `bytes` `HewVec` contains a well-formed HBF header.
///
/// Returns 1 if valid, 0 otherwise.
///
/// # Safety
///
/// `v` must be a valid, non-null pointer to a `HewVec` (i32 elements).
#[no_mangle]
pub unsafe extern "C" fn hew_wire_validate_header_hew(v: *mut crate::vec::HewVec) -> i32 {
    // SAFETY: v validity forwarded to hwvec_to_u8.
    let bytes = unsafe { crate::vec::hwvec_to_u8(v) };
    // SAFETY: bytes slice is valid for its length.
    unsafe { hew_wire_validate_header(bytes.as_ptr(), bytes.len()) }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn heap_buffer_varint_roundtrip() {
        // SAFETY: all pointers used here come from runtime allocators and are valid.
        unsafe {
            let buf = hew_wire_buf_new();
            assert!(!buf.is_null());
            assert_eq!(hew_wire_encode_varint(buf, 300), 0);

            let encoded = std::slice::from_raw_parts((*buf).data.cast_const(), (*buf).len).to_vec();
            assert!(!encoded.is_empty());

            let mut read_buf = HewWireBuf {
                data: std::ptr::null_mut(),
                len: 0,
                cap: 0,
                read_pos: 0,
            };
            hew_wire_buf_init_read(&raw mut read_buf, encoded.as_ptr(), encoded.len());
            let mut decoded: u64 = 0;
            assert_eq!(
                hew_wire_decode_varint(&raw mut read_buf, &raw mut decoded),
                0
            );
            assert_eq!(decoded, 300);

            hew_wire_buf_destroy(buf);
        }
    }

    #[test]
    fn hbf_header_roundtrip() {
        // SAFETY: header pointer is allocated by runtime and freed in this test.
        unsafe {
            let header = hew_wire_encode_header(1024, HBF_FLAG_COMPRESSED);
            assert!(!header.is_null());
            assert_eq!(
                hew_wire_validate_header(header.cast_const(), HBF_HEADER_LEN),
                1
            );

            let decoded = hew_wire_decode_header(header.cast_const(), HBF_HEADER_LEN);
            assert_eq!(decoded.version, HBF_VERSION);
            assert_eq!(decoded.flags, HBF_FLAG_COMPRESSED);
            assert_eq!(decoded.payload_len, 1024);

            libc::free(header.cast::<c_void>());
        }
    }
}
