//! Cross-node actor-message payload serialization codec.
//!
//! ## Why this exists
//!
//! A `Serializable` actor message that crosses a process boundary cannot be
//! transmitted as its in-memory representation: a `string` field is a pointer
//! to a header-aware refcounted C-string buffer, a nested record is an inline
//! struct that itself contains such pointers, and an `enum` is a tag plus an
//! opaque payload buffer holding the active variant's fields. Shipping those
//! raw bytes across a process boundary makes the receiver dereference pointers
//! that belong to the *sender's* address space — the documented crash is
//! `free_cstring: C-string header sentinel missing` when the receiving actor's
//! handler touches such a field.
//!
//! This module is the runtime half of the codec. Codegen emits, per cross-node
//! message type, a `__hew_serialize_<T>` / `__hew_deserialize_<T>` thunk pair
//! that walks the value's layout *in codegen* (the single source of truth for
//! field order and offsets) and calls the small primitives here to build / read
//! the wire bytes. The byte format authority lives here, in one place; the
//! layout-walk authority lives in codegen. The two never duplicate offset math.
//!
//! ## Wire format (positional, no schema)
//!
//! The encoder produces a flat little-endian byte stream. The accepted type
//! subset matches the `Serializable` floor the checker already enforces:
//! scalars, `string`, `bytes`, records (positional fields), and enums (a
//! tag followed by the active variant's positional fields). Because both nodes
//! compile the same type declarations, a positional encoding is sufficient —
//! no field-number or schema registry is required (that is future work; see the
//! lane plan "Out of scope"). Each primitive writes a 1-byte tag so the decoder
//! is self-checking and fails closed on a malformed / truncated stream rather
//! than fabricating a value (`boundary-fail-closed`).
//!
//! ## Ownership across the boundary
//!
//! - `hew_ser_buf_new` allocates a builder; `hew_ser_buf_finish` consumes it and
//!   returns a `libc::malloc`'d byte buffer the runtime owns. The runtime copies
//!   those bytes into the envelope and frees the buffer with `hew_ser_free_bytes`.
//! - On decode, `hew_de_reader_new` borrows the envelope bytes (no copy); the
//!   `hew_de_read_*` primitives the decode thunk calls reconstruct owned values
//!   (e.g. a `string` field becomes a freshly `malloc_cstring`'d buffer the
//!   reconstructed value owns). `hew_de_reader_free` frees the reader.

use std::os::raw::{c_char, c_void};
use std::sync::Mutex;

use hew_cabi::cabi::malloc_cstring;

/// 1-byte type tags written ahead of each value in the positional stream. The
/// decoder verifies the tag matches what the layout-walk expects, turning any
/// sender/receiver layout disagreement (a real bug) into a fail-closed decode
/// error rather than a silently mis-typed value.
mod tag {
    pub const BOOL: u8 = 0x01;
    pub const I64: u8 = 0x02;
    pub const U64: u8 = 0x03;
    pub const F64: u8 = 0x04;
    pub const STRING: u8 = 0x05;
    pub const BYTES: u8 = 0x06;
    pub const ENUM_TAG: u8 = 0x07;
    pub const UNIT: u8 = 0x08;
}

/// Incrementally-built encode buffer. Codegen's serialize thunk pushes one
/// value per field in layout order.
#[derive(Debug)]
pub struct SerBuf {
    out: Vec<u8>,
}

/// A read cursor over a borrowed byte slice. Codegen's deserialize thunk pulls
/// one value per field in layout order. `failed` latches a decode error so the
/// thunk can abort the whole reconstruction fail-closed.
#[derive(Debug)]
pub struct DeReader {
    data: *const u8,
    len: usize,
    pos: usize,
    failed: bool,
}

// SAFETY: SerBuf owns its Vec; no interior shared mutability across threads.
unsafe impl Send for SerBuf {}
// SAFETY: DeReader's borrowed pointer is only read on the thread that created it
// (the inbound-ask worker / decode site); it is not shared.
unsafe impl Send for DeReader {}

/// Reborrow an opaque handle as `&mut SerBuf`, returning `None` for null.
///
/// # Safety
/// `buf` must be null or a live handle from [`hew_ser_buf_new`] that no other
/// reference aliases for the duration of the returned borrow (each codec
/// primitive takes it for the span of a single push, satisfying this).
unsafe fn as_ser_buf<'a>(buf: *mut c_void) -> Option<&'a mut SerBuf> {
    // SAFETY: caller guarantees buf is null or a live, unaliased SerBuf handle.
    unsafe { buf.cast::<SerBuf>().as_mut() }
}

/// Reborrow an opaque handle as `&mut DeReader`, returning `None` for null.
///
/// # Safety
/// `reader` must be null or a live handle from [`hew_de_reader_new`] that no
/// other reference aliases for the duration of the returned borrow.
unsafe fn as_de_reader<'a>(reader: *mut c_void) -> Option<&'a mut DeReader> {
    // SAFETY: caller guarantees reader is null or a live, unaliased handle.
    unsafe { reader.cast::<DeReader>().as_mut() }
}

// ---------------------------------------------------------------------------
// Encode primitives — called from codegen's `__hew_serialize_<T>` thunk.
// ---------------------------------------------------------------------------

/// Allocate a fresh encode buffer. Returns an opaque handle the caller must
/// pass to `hew_ser_finish` (which consumes it) or `hew_ser_abort`.
#[no_mangle]
pub extern "C" fn hew_ser_buf_new() -> *mut c_void {
    let b = Box::new(SerBuf { out: Vec::new() });
    Box::into_raw(b).cast::<c_void>()
}

/// # Safety
/// `buf` must be a live handle from `hew_ser_buf_new`.
#[no_mangle]
pub unsafe extern "C" fn hew_ser_push_bool(buf: *mut c_void, value: i8) {
    // SAFETY: buf is a live SerBuf handle per this fn's contract.
    let Some(b) = (unsafe { as_ser_buf(buf) }) else {
        return;
    };
    b.out.push(tag::BOOL);
    b.out.push(u8::from(value != 0));
}

/// # Safety
/// `buf` must be a live handle from `hew_ser_buf_new`.
#[no_mangle]
pub unsafe extern "C" fn hew_ser_push_i64(buf: *mut c_void, value: i64) {
    // SAFETY: buf is a live SerBuf handle per this fn's contract.
    let Some(b) = (unsafe { as_ser_buf(buf) }) else {
        return;
    };
    b.out.push(tag::I64);
    b.out.extend_from_slice(&value.to_le_bytes());
}

/// # Safety
/// `buf` must be a live handle from `hew_ser_buf_new`.
#[no_mangle]
pub unsafe extern "C" fn hew_ser_push_u64(buf: *mut c_void, value: u64) {
    // SAFETY: buf is a live SerBuf handle per this fn's contract.
    let Some(b) = (unsafe { as_ser_buf(buf) }) else {
        return;
    };
    b.out.push(tag::U64);
    b.out.extend_from_slice(&value.to_le_bytes());
}

/// # Safety
/// `buf` must be a live handle from `hew_ser_buf_new`.
#[no_mangle]
pub unsafe extern "C" fn hew_ser_push_f64(buf: *mut c_void, value: f64) {
    // SAFETY: buf is a live SerBuf handle per this fn's contract.
    let Some(b) = (unsafe { as_ser_buf(buf) }) else {
        return;
    };
    b.out.push(tag::F64);
    b.out.extend_from_slice(&value.to_le_bytes());
}

/// Push a unit / zero-payload marker (e.g. a payload-less enum variant slot).
///
/// # Safety
/// `buf` must be a live handle from `hew_ser_buf_new`.
#[no_mangle]
pub unsafe extern "C" fn hew_ser_push_unit(buf: *mut c_void) {
    // SAFETY: buf is a live SerBuf handle per this fn's contract.
    let Some(b) = (unsafe { as_ser_buf(buf) }) else {
        return;
    };
    b.out.push(tag::UNIT);
}

/// Push an enum discriminant. The thunk follows this with the active variant's
/// positional fields.
///
/// # Safety
/// `buf` must be a live handle from `hew_ser_buf_new`.
#[no_mangle]
pub unsafe extern "C" fn hew_ser_push_enum_tag(buf: *mut c_void, tag_value: u64) {
    // SAFETY: buf is a live SerBuf handle per this fn's contract.
    let Some(b) = (unsafe { as_ser_buf(buf) }) else {
        return;
    };
    b.out.push(tag::ENUM_TAG);
    b.out.extend_from_slice(&tag_value.to_le_bytes());
}

/// Push a `string` field. `s` is the value's `*const c_char` slot (a header-aware
/// refcounted C string, or null). The bytes are copied into the stream; the
/// caller retains ownership of `s`.
///
/// # Safety
/// `buf` must be a live handle; `s` must be null or a valid NUL-terminated
/// C string.
#[no_mangle]
pub unsafe extern "C" fn hew_ser_push_string(buf: *mut c_void, s: *const c_char) {
    // SAFETY: buf is a live SerBuf handle per this fn's contract.
    let Some(b) = (unsafe { as_ser_buf(buf) }) else {
        return;
    };
    let len = if s.is_null() {
        0
    } else {
        // SAFETY: s is a valid NUL-terminated C string per contract.
        unsafe { libc::strlen(s) }
    };
    b.out.push(tag::STRING);
    // u32 length prefix is sufficient: Hew string lengths are i32-bounded.
    let len_u32 = u32::try_from(len).unwrap_or(u32::MAX);
    b.out.extend_from_slice(&len_u32.to_le_bytes());
    if len_u32 > 0 {
        // SAFETY: s points to at least `len` readable bytes.
        let bytes = unsafe { std::slice::from_raw_parts(s.cast::<u8>(), len_u32 as usize) };
        b.out.extend_from_slice(bytes);
    }
}

/// Push a `bytes` field given the active byte range `(ptr + offset)[0..len]`.
///
/// # Safety
/// `buf` must be a live handle; `(ptr+offset)` must be valid for `len` bytes
/// when `len > 0`.
#[no_mangle]
pub unsafe extern "C" fn hew_ser_push_bytes(
    buf: *mut c_void,
    ptr: *const u8,
    offset: u32,
    len: u32,
) {
    // SAFETY: buf is a live SerBuf handle per this fn's contract.
    let Some(b) = (unsafe { as_ser_buf(buf) }) else {
        return;
    };
    b.out.push(tag::BYTES);
    b.out.extend_from_slice(&len.to_le_bytes());
    if len > 0 && !ptr.is_null() {
        // SAFETY: caller guarantees (ptr+offset) is valid for len bytes.
        let start = unsafe { ptr.add(offset as usize) };
        // SAFETY: start is valid for len bytes (the active byte range).
        let bytes = unsafe { std::slice::from_raw_parts(start, len as usize) };
        b.out.extend_from_slice(bytes);
    }
}

/// Consume the builder and return a `libc::malloc`'d copy of the encoded bytes;
/// writes the length into `*out_len`. Returns null on empty/failure. The caller
/// owns the buffer and must free it with `hew_ser_free_bytes`.
///
/// # Safety
/// `buf` must be a live handle from `hew_ser_buf_new` (consumed here);
/// `out_len` must be a valid writable `*mut usize`.
#[no_mangle]
pub unsafe extern "C" fn hew_ser_finish(buf: *mut c_void, out_len: *mut usize) -> *mut u8 {
    if buf.is_null() {
        if !out_len.is_null() {
            // SAFETY: out_len validated non-null.
            unsafe { *out_len = 0 };
        }
        return std::ptr::null_mut();
    }
    // SAFETY: buf is a live handle from hew_ser_buf_new; reclaim ownership.
    let b = unsafe { Box::from_raw(buf.cast::<SerBuf>()) };
    let bytes = b.out;
    if !out_len.is_null() {
        // SAFETY: out_len validated non-null.
        unsafe { *out_len = bytes.len() };
    }
    // Always return a non-null, freeable buffer so an empty payload is
    // distinguishable from an allocation failure by the (ptr, len) pair.
    let total = bytes.len().max(1);
    // SAFETY: malloc returns a valid pointer or null.
    let dst = unsafe { libc::malloc(total) }.cast::<u8>();
    if dst.is_null() {
        if !out_len.is_null() {
            // SAFETY: out_len validated non-null.
            unsafe { *out_len = 0 };
        }
        return std::ptr::null_mut();
    }
    if !bytes.is_empty() {
        // SAFETY: dst has `total >= bytes.len()` bytes; bytes is a valid slice.
        unsafe { std::ptr::copy_nonoverlapping(bytes.as_ptr(), dst, bytes.len()) };
    }
    dst
}

/// Discard a builder without finishing (error path).
///
/// # Safety
/// `buf` must be a live handle from `hew_ser_buf_new`.
#[no_mangle]
pub unsafe extern "C" fn hew_ser_abort(buf: *mut c_void) {
    if buf.is_null() {
        return;
    }
    // SAFETY: buf is a live handle; reclaim and drop.
    drop(unsafe { Box::from_raw(buf.cast::<SerBuf>()) });
}

/// Free a buffer returned by `hew_ser_finish`.
///
/// # Safety
/// `ptr` must be null or a buffer from `hew_ser_finish`.
#[no_mangle]
pub unsafe extern "C" fn hew_ser_free_bytes(ptr: *mut u8) {
    if !ptr.is_null() {
        // SAFETY: ptr came from libc::malloc in hew_ser_finish.
        unsafe { libc::free(ptr.cast::<c_void>()) };
    }
}

// ---------------------------------------------------------------------------
// Decode primitives — called from codegen's `__hew_deserialize_<T>` thunk.
// ---------------------------------------------------------------------------

/// Create a read cursor over `len` borrowed bytes. The cursor does not own the
/// bytes; the caller keeps them alive until `hew_de_reader_free`.
///
/// # Safety
/// `data` must be valid for `len` bytes for the cursor's lifetime (or null when
/// `len == 0`).
#[no_mangle]
pub unsafe extern "C" fn hew_de_reader_new(data: *const u8, len: usize) -> *mut c_void {
    let r = Box::new(DeReader {
        data,
        len,
        pos: 0,
        failed: false,
    });
    Box::into_raw(r).cast::<c_void>()
}

/// Free a read cursor.
///
/// # Safety
/// `reader` must be a live handle from `hew_de_reader_new`.
#[no_mangle]
pub unsafe extern "C" fn hew_de_reader_free(reader: *mut c_void) {
    if reader.is_null() {
        return;
    }
    // SAFETY: reader is a live handle; reclaim and drop.
    drop(unsafe { Box::from_raw(reader.cast::<DeReader>()) });
}

/// Returns 1 if the cursor has latched a decode failure (truncated stream or
/// tag mismatch), else 0. The decode thunk checks this after the walk and fails
/// the whole reconstruction closed if set.
///
/// # Safety
/// `reader` must be a live handle from `hew_de_reader_new`.
#[no_mangle]
pub unsafe extern "C" fn hew_de_failed(reader: *const c_void) -> i32 {
    // SAFETY: reader is null or a live DeReader handle per this fn's contract.
    let Some(r) = (unsafe { reader.cast::<DeReader>().as_ref() }) else {
        return 1;
    };
    i32::from(r.failed)
}

impl DeReader {
    fn take(&mut self, n: usize) -> Option<&[u8]> {
        if self.failed || self.data.is_null() {
            self.failed = true;
            return None;
        }
        let end = self.pos.checked_add(n)?;
        if end > self.len {
            self.failed = true;
            return None;
        }
        // SAFETY: pos..end is within [0, len); data is valid for len bytes.
        let slice = unsafe { std::slice::from_raw_parts(self.data.add(self.pos), n) };
        self.pos = end;
        Some(slice)
    }

    fn expect_tag(&mut self, want: u8) -> bool {
        match self.take(1) {
            Some(b) if b[0] == want => true,
            _ => {
                self.failed = true;
                false
            }
        }
    }
}

/// Read a `bool`. Returns 0 on failure (and latches `failed`).
///
/// # Safety
/// `reader` must be a live handle from `hew_de_reader_new`.
#[no_mangle]
pub unsafe extern "C" fn hew_de_read_bool(reader: *mut c_void) -> i8 {
    // SAFETY: reader is a live DeReader handle per this fn's contract.
    let Some(r) = (unsafe { as_de_reader(reader) }) else {
        return 0;
    };
    if !r.expect_tag(tag::BOOL) {
        return 0;
    }
    match r.take(1) {
        Some(b) => i8::from(b[0] != 0),
        None => 0,
    }
}

/// Read an `i64`. Returns 0 on failure (and latches `failed`).
///
/// # Safety
/// `reader` must be a live handle from `hew_de_reader_new`.
#[no_mangle]
pub unsafe extern "C" fn hew_de_read_i64(reader: *mut c_void) -> i64 {
    // SAFETY: reader is a live DeReader handle per this fn's contract.
    let Some(r) = (unsafe { as_de_reader(reader) }) else {
        return 0;
    };
    if !r.expect_tag(tag::I64) {
        return 0;
    }
    match r.take(8) {
        Some(b) => i64::from_le_bytes(b.try_into().unwrap_or([0; 8])),
        None => 0,
    }
}

/// Read a `u64`. Returns 0 on failure (and latches `failed`).
///
/// # Safety
/// `reader` must be a live handle from `hew_de_reader_new`.
#[no_mangle]
pub unsafe extern "C" fn hew_de_read_u64(reader: *mut c_void) -> u64 {
    // SAFETY: reader is a live DeReader handle per this fn's contract.
    let Some(r) = (unsafe { as_de_reader(reader) }) else {
        return 0;
    };
    if !r.expect_tag(tag::U64) {
        return 0;
    }
    match r.take(8) {
        Some(b) => u64::from_le_bytes(b.try_into().unwrap_or([0; 8])),
        None => 0,
    }
}

/// Read an `f64`. Returns 0.0 on failure (and latches `failed`).
///
/// # Safety
/// `reader` must be a live handle from `hew_de_reader_new`.
#[no_mangle]
pub unsafe extern "C" fn hew_de_read_f64(reader: *mut c_void) -> f64 {
    // SAFETY: reader is a live DeReader handle per this fn's contract.
    let Some(r) = (unsafe { as_de_reader(reader) }) else {
        return 0.0;
    };
    if !r.expect_tag(tag::F64) {
        return 0.0;
    }
    match r.take(8) {
        Some(b) => f64::from_le_bytes(b.try_into().unwrap_or([0; 8])),
        None => 0.0,
    }
}

/// Read a `unit` marker. No value; latches `failed` on mismatch.
///
/// # Safety
/// `reader` must be a live handle from `hew_de_reader_new`.
#[no_mangle]
pub unsafe extern "C" fn hew_de_read_unit(reader: *mut c_void) {
    // SAFETY: reader is null or a live DeReader handle per this fn's contract.
    if let Some(r) = unsafe { as_de_reader(reader) } {
        let _ = r.expect_tag(tag::UNIT);
    }
}

/// Read an enum discriminant. Returns 0 on failure (and latches `failed`); the
/// thunk must check `hew_de_failed` before trusting a 0 tag.
///
/// # Safety
/// `reader` must be a live handle from `hew_de_reader_new`.
#[no_mangle]
pub unsafe extern "C" fn hew_de_read_enum_tag(reader: *mut c_void) -> u64 {
    // SAFETY: reader is a live DeReader handle per this fn's contract.
    let Some(r) = (unsafe { as_de_reader(reader) }) else {
        return 0;
    };
    if !r.expect_tag(tag::ENUM_TAG) {
        return 0;
    }
    match r.take(8) {
        Some(b) => u64::from_le_bytes(b.try_into().unwrap_or([0; 8])),
        None => 0,
    }
}

/// Read a `string` field, reconstructing a fresh header-aware C string the
/// caller owns (release via `hew_string_drop`). Returns an empty owned string
/// on failure (and latches `failed`) so the reconstructed value is never left
/// with a dangling slot.
///
/// # Safety
/// `reader` must be a live handle from `hew_de_reader_new`.
#[no_mangle]
pub unsafe extern "C" fn hew_de_read_string(reader: *mut c_void) -> *mut c_char {
    // SAFETY: reader is a live DeReader handle per this fn's contract.
    let Some(r) = (unsafe { as_de_reader(reader) }) else {
        // SAFETY: empty owned string.
        return unsafe { malloc_cstring(std::ptr::null(), 0) };
    };
    if !r.expect_tag(tag::STRING) {
        // SAFETY: empty owned string.
        return unsafe { malloc_cstring(std::ptr::null(), 0) };
    }
    let len = match r.take(4) {
        Some(b) => u32::from_le_bytes(b.try_into().unwrap_or([0; 4])) as usize,
        None => {
            // SAFETY: empty owned string.
            return unsafe { malloc_cstring(std::ptr::null(), 0) };
        }
    };
    if len == 0 {
        // SAFETY: empty owned string.
        return unsafe { malloc_cstring(std::ptr::null(), 0) };
    }
    match r.take(len) {
        Some(bytes) => {
            // SAFETY: bytes is valid for len; malloc_cstring copies them.
            unsafe { malloc_cstring(bytes.as_ptr(), len) }
        }
        None => {
            // SAFETY: empty owned string on truncation.
            unsafe { malloc_cstring(std::ptr::null(), 0) }
        }
    }
}

/// Read a `bytes` field into a freshly `libc::malloc`'d buffer; writes the
/// length to `*out_len`. Returns null for an empty/failed read. Caller owns the
/// buffer (the reconstructed `bytes` value's refcounted allocation is built by
/// the thunk via `hew_bytes_*`; see codegen).
///
/// # Safety
/// `reader` must be a live handle; `out_len` must be a valid writable pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_de_read_bytes(reader: *mut c_void, out_len: *mut u32) -> *mut u8 {
    if !out_len.is_null() {
        // SAFETY: out_len validated non-null.
        unsafe { *out_len = 0 };
    }
    // SAFETY: reader is a live DeReader handle per this fn's contract.
    let Some(r) = (unsafe { as_de_reader(reader) }) else {
        return std::ptr::null_mut();
    };
    if !r.expect_tag(tag::BYTES) {
        return std::ptr::null_mut();
    }
    let len = match r.take(4) {
        Some(b) => u32::from_le_bytes(b.try_into().unwrap_or([0; 4])),
        None => return std::ptr::null_mut(),
    };
    if len == 0 {
        return std::ptr::null_mut();
    }
    match r.take(len as usize) {
        Some(bytes) => {
            // SAFETY: malloc returns a valid pointer or null.
            let dst = unsafe { libc::malloc(len as usize) }.cast::<u8>();
            if dst.is_null() {
                r.failed = true;
                return std::ptr::null_mut();
            }
            // SAFETY: dst has len bytes; bytes is valid for len.
            unsafe { std::ptr::copy_nonoverlapping(bytes.as_ptr(), dst, len as usize) };
            if !out_len.is_null() {
                // SAFETY: out_len validated non-null.
                unsafe { *out_len = len };
            }
            dst
        }
        None => std::ptr::null_mut(),
    }
}

// ---------------------------------------------------------------------------
// Decode-thunk registry — receive-side lookup by msg_type.
// ---------------------------------------------------------------------------
//
// The receive path (`node_inbound_router` / `handle_inbound_ask`) holds only the
// inbound `msg_type` and the wire bytes. It must find the right
// `__hew_deserialize_<T>` thunk to rebuild the value into the receiving node's
// address space before handing it to the local mailbox / ask path. Codegen
// registers each decode (and the matching reply-encode) thunk at program start
// keyed by the same SipHash-derived `msg_type` discriminant the send path uses.

/// C-ABI signature of a codegen-emitted deserialize thunk: it borrows the wire
/// bytes, returns a `libc::malloc`'d reconstructed value the caller owns, and
/// writes the value's in-memory struct byte size to `*out_struct_size` (which
/// differs from the wire length, so the runtime can hand the mailbox / ask path
/// the correct byte count).
pub type DeserializeThunk =
    unsafe extern "C" fn(data: *const u8, len: usize, out_struct_size: *mut usize) -> *mut c_void;

/// C-ABI signature of a codegen-emitted serialize thunk: it reads the value at
/// `value_ptr` and returns a `libc::malloc`'d byte buffer (length via `out_len`)
/// the caller owns (free with `hew_ser_free_bytes`).
pub type SerializeThunk =
    unsafe extern "C" fn(value_ptr: *const c_void, out_len: *mut usize) -> *mut u8;

// `serialize` is read by `lookup_serialize` (consumed by the ask reply-encode
// path); `deserialize` by `lookup_deserialize` (the tell/ask receive-decode
// path). Both are wired in the runtime receive slices of this lane.
struct ThunkPair {
    serialize: SerializeThunk,
    deserialize: DeserializeThunk,
}

/// Codec-registry key: `(dispatch, msg_type)`.
///
/// `msg_type` alone is a 32-bit SipHash-1-3 truncation of `"Actor::handler"` and
/// therefore collision-prone — two DISTINCT actor types whose handler names
/// collide on the low 32 bits share one `msg_type`. Keying the codec table by
/// `msg_type` alone let the second registration displace the first, so one
/// actor's inbound frames were silently decoded by the OTHER actor's codec:
/// remote type-confusion (an OOB read, a wrong owned-field drop, corruption)
/// reachable by any peer that routes a frame to the colliding actor.
///
/// `dispatch` is the per-actor-TYPE dispatch function pointer
/// (`__hew_actor_dispatch_<Actor>`), already on `HewActor.dispatch` and already
/// the established cross-node disambiguator (the profiler handler-name registry
/// keys by the same `(dispatch_fn_ptr, msg_type)` pair). Two distinct actor
/// types have distinct dispatch pointers, so distinct `(dispatch, msg_type)`
/// keys even when `msg_type` collides — the codec for actor B is unreachable
/// from actor A's identity and vice versa. The dispatch pointer NEVER crosses
/// the wire: the frame still carries only `(target_actor_id, msg_type)`; each
/// node derives the key LOCALLY from its own dispatch globals (the inbound
/// decode resolves `target_actor_id -> HewActor.dispatch`; the originating
/// send/ask resolves the target/caller actor type's local dispatch global at
/// the call site). A dispatch pointer is a code-segment address, stable within
/// one process run but not across processes — which is exactly right, since it
/// is only ever compared within one process.
#[derive(Clone, Copy, PartialEq, Eq)]
struct CodecKey {
    dispatch: usize,
    msg_type: i32,
}

impl CodecKey {
    fn new(dispatch: *const c_void, msg_type: i32) -> Self {
        Self {
            dispatch: dispatch as usize,
            msg_type,
        }
    }
}

static THUNK_REGISTRY: Mutex<Vec<(CodecKey, ThunkPair)>> = Mutex::new(Vec::new());

/// Register the request/tell serialize/deserialize thunk pair for a cross-node
/// message type, keyed by `(dispatch, msg_type)` — the owning actor TYPE's
/// dispatch function pointer plus the message discriminant. Called once per
/// `(actor type, handler)` at program start from codegen-emitted registration
/// glue. Idempotent: a re-registration for the same `(dispatch, msg_type)` is
/// only valid when the thunks are identical (the same handler re-registered,
/// e.g. a duplicated `global_ctors` call). Distinct thunks under the same full
/// key is structurally impossible now (it would require two distinct handlers of
/// the same actor type to share one `msg_type`, which the intra-actor checker
/// already rejects); if it is ever observed it is a hard invariant violation and
/// we FAIL CLOSED in every build (`boundary-fail-closed`), not just debug —
/// silently overwriting would re-open the type-confusion in release.
///
/// # Safety
/// `serialize` / `deserialize` must be valid codegen-emitted thunks matching the
/// declared signatures. `dispatch` is an opaque key (never dereferenced).
#[no_mangle]
pub unsafe extern "C" fn hew_xnode_register_codec(
    dispatch: *const c_void,
    msg_type: i32,
    serialize: SerializeThunk,
    deserialize: DeserializeThunk,
) {
    register_codec_into(
        &THUNK_REGISTRY,
        "request",
        dispatch,
        msg_type,
        serialize,
        deserialize,
    );
}

/// Reply-codec registry, keyed by `(dispatch, msg_type)` — the responding
/// actor TYPE's dispatch pointer plus the REQUEST's `msg_type`. A handler's
/// reply type maps 1:1 to its request `msg_type` WITHIN one actor type; keying
/// by `(dispatch, msg_type)` keeps two colliding-`msg_type` actors' reply
/// codecs distinct. The inbound-ask worker (which has the target actor's
/// dispatch via `target_actor_id`) finds the reply SERIALIZE thunk to encode
/// the reply before shipping it back; the originating node decodes the reply
/// with the codec for ITS caller's statically-known target actor type (its
/// local dispatch global for that type), resolved at the ask call site.
static REPLY_REGISTRY: Mutex<Vec<(CodecKey, ThunkPair)>> = Mutex::new(Vec::new());

/// Register the reply serialize/deserialize thunk pair for an ask, keyed by
/// `(dispatch, request msg_type)`. Same fail-closed collision guard as
/// `hew_xnode_register_codec`.
///
/// # Safety
/// `serialize` / `deserialize` must be valid codegen-emitted reply-type thunks.
/// `dispatch` is an opaque key (never dereferenced).
#[no_mangle]
pub unsafe extern "C" fn hew_xnode_register_reply_codec(
    dispatch: *const c_void,
    msg_type: i32,
    serialize: SerializeThunk,
    deserialize: DeserializeThunk,
) {
    register_codec_into(
        &REPLY_REGISTRY,
        "reply",
        dispatch,
        msg_type,
        serialize,
        deserialize,
    );
}

/// Shared register body for both registries. On a same-key re-registration with
/// DIFFERENT thunk addresses, fail closed in EVERY build mode: abort the process
/// rather than silently overwrite (which would re-open the cross-node
/// type-confusion in release). With the `(dispatch, msg_type)` key this branch
/// is unreachable by construction — it guards an invariant, it is not a live
/// recovery path.
fn register_codec_into(
    registry: &Mutex<Vec<(CodecKey, ThunkPair)>>,
    which: &str,
    dispatch: *const c_void,
    msg_type: i32,
    serialize: SerializeThunk,
    deserialize: DeserializeThunk,
) {
    let key = CodecKey::new(dispatch, msg_type);
    let Ok(mut reg) = registry.lock() else {
        return;
    };
    if let Some(slot) = reg.iter_mut().find(|(k, _)| *k == key) {
        let same = slot.1.serialize as usize == serialize as usize
            && slot.1.deserialize as usize == deserialize as usize;
        // Fail closed in release AND debug: a TRUE collision on the full
        // `(dispatch, msg_type)` key means two distinct codecs claim the same
        // actor-type + discriminant — an unrecoverable wire-routing ambiguity.
        // Refuse rather than pick one and silently mis-route. Identical
        // re-registration (a duplicated ctor) passes the assert and is a no-op.
        assert!(
            same,
            "hew_xnode_register_codec ({which}): collision on dispatch={:#x} \
             msg_type={msg_type} — two distinct codecs share one \
             (actor-type, msg_type) key; cross-node routing is ambiguous",
            key.dispatch
        );
    } else {
        reg.push((
            key,
            ThunkPair {
                serialize,
                deserialize,
            },
        ));
    }
}

/// Look up the deserialize thunk for `(dispatch, msg_type)`, if registered.
pub(crate) fn lookup_deserialize(
    dispatch: *const c_void,
    msg_type: i32,
) -> Option<DeserializeThunk> {
    let key = CodecKey::new(dispatch, msg_type);
    let reg = THUNK_REGISTRY.lock().ok()?;
    reg.iter()
        .find(|(k, _)| *k == key)
        .map(|(_, p)| p.deserialize)
}

/// Look up the reply SERIALIZE thunk for `(dispatch, request msg_type)`.
pub(crate) fn lookup_reply_serialize(
    dispatch: *const c_void,
    msg_type: i32,
) -> Option<SerializeThunk> {
    let key = CodecKey::new(dispatch, msg_type);
    let reg = REPLY_REGISTRY.lock().ok()?;
    reg.iter()
        .find(|(k, _)| *k == key)
        .map(|(_, p)| p.serialize)
}

/// Look up the reply DESERIALIZE thunk for `(dispatch, request msg_type)`.
pub(crate) fn lookup_reply_deserialize(
    dispatch: *const c_void,
    msg_type: i32,
) -> Option<DeserializeThunk> {
    let key = CodecKey::new(dispatch, msg_type);
    let reg = REPLY_REGISTRY.lock().ok()?;
    reg.iter()
        .find(|(k, _)| *k == key)
        .map(|(_, p)| p.deserialize)
}

/// Decode an ask REPLY wire payload (keyed by `(dispatch, request msg_type)`)
/// into a freshly reconstructed reply value + its in-memory struct size. Returns
/// `(null, 0)` if no reply codec is registered or the decode fails.
///
/// # Safety
/// `data` must be valid for `len` bytes (or null when `len == 0`).
pub(crate) unsafe fn decode_reply(
    dispatch: *const c_void,
    msg_type: i32,
    data: *const u8,
    len: usize,
) -> (*mut c_void, usize) {
    let Some(thunk) = lookup_reply_deserialize(dispatch, msg_type) else {
        return (std::ptr::null_mut(), 0);
    };
    let mut struct_size: usize = 0;
    // SAFETY: thunk is a valid codegen-emitted reply deserialize thunk.
    let value = unsafe { thunk(data, len, &raw mut struct_size) };
    (value, struct_size)
}

/// Decode an inbound wire payload for `(dispatch, msg_type)` into a freshly
/// reconstructed value in this node's address space. Returns null if no codec is
/// registered for the key (fail-closed: the caller must drop the message rather
/// than feed raw bytes to the mailbox) or if the registered thunk reports
/// failure.
///
/// # Safety
/// `data` must be valid for `len` bytes (or null when `len == 0`).
pub(crate) unsafe fn decode_payload(
    dispatch: *const c_void,
    msg_type: i32,
    data: *const u8,
    len: usize,
) -> (*mut c_void, usize) {
    let Some(thunk) = lookup_deserialize(dispatch, msg_type) else {
        return (std::ptr::null_mut(), 0);
    };
    let mut struct_size: usize = 0;
    // SAFETY: thunk is a valid codegen-emitted deserialize thunk; data/len valid.
    let value = unsafe { thunk(data, len, &raw mut struct_size) };
    (value, struct_size)
}

/// Look up the request SERIALIZE thunk for `(dispatch, msg_type)`, if registered.
pub(crate) fn lookup_serialize(dispatch: *const c_void, msg_type: i32) -> Option<SerializeThunk> {
    let key = CodecKey::new(dispatch, msg_type);
    let reg = THUNK_REGISTRY.lock().ok()?;
    reg.iter()
        .find(|(k, _)| *k == key)
        .map(|(_, p)| p.serialize)
}

/// Encode a cross-node REQUEST/tell payload value (keyed by `(dispatch,
/// msg_type)`) into a freshly `malloc`'d byte buffer (length via `out_len`).
/// Returns null + `*out_len = 0` if no codec is registered (fail-closed: the
/// send path then refuses to transmit raw bytes).
///
/// # Safety
/// `value_ptr` must point to a valid value of the message type for `msg_type`;
/// `out_len` must be a valid writable pointer.
pub(crate) unsafe fn encode_payload(
    dispatch: *const c_void,
    msg_type: i32,
    value_ptr: *const c_void,
    out_len: *mut usize,
) -> *mut u8 {
    let Some(thunk) = lookup_serialize(dispatch, msg_type) else {
        if !out_len.is_null() {
            // SAFETY: out_len validated non-null.
            unsafe { *out_len = 0 };
        }
        return std::ptr::null_mut();
    };
    // SAFETY: thunk is a valid codegen-emitted serialize thunk.
    unsafe { thunk(value_ptr, out_len) }
}

/// Encode an ask REPLY value (keyed by `(dispatch, request msg_type)`) into a
/// freshly `malloc`'d byte buffer. Returns null + `*out_len = 0` if no reply
/// codec is registered (fail-closed: the inbound-ask worker then sends no reply,
/// and the originating ask times out rather than receiving raw bytes).
///
/// # Safety
/// `value_ptr` must point to a valid value of the reply type for `msg_type`;
/// `out_len` must be a valid writable pointer.
pub(crate) unsafe fn encode_reply(
    dispatch: *const c_void,
    msg_type: i32,
    value_ptr: *const c_void,
    out_len: *mut usize,
) -> *mut u8 {
    let Some(thunk) = lookup_reply_serialize(dispatch, msg_type) else {
        if !out_len.is_null() {
            // SAFETY: out_len validated non-null.
            unsafe { *out_len = 0 };
        }
        return std::ptr::null_mut();
    };
    // SAFETY: thunk is a valid codegen-emitted reply serialize thunk.
    unsafe { thunk(value_ptr, out_len) }
}

/// Clear both codec registries. Test-only: the global registries persist across
/// `#[test]` functions in one process, so a collision test that asserts on the
/// registry contents must reset them to stay hermetic.
#[cfg(test)]
pub(crate) fn clear_codec_registries_for_test() {
    THUNK_REGISTRY.lock().unwrap().clear();
    REPLY_REGISTRY.lock().unwrap().clear();
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CString;

    #[test]
    fn round_trip_record_with_string_and_i64() {
        // Simulate a `record { key: string, val: i64 }` walk: push in field
        // order, then read back in the same order. This is the contract the
        // codegen thunk pair must satisfy.
        let key = CString::new("hello").unwrap();
        // SAFETY: test-controlled handles.
        let bytes = unsafe {
            let buf = hew_ser_buf_new();
            hew_ser_push_string(buf, key.as_ptr());
            hew_ser_push_i64(buf, 42);
            let mut len = 0usize;
            let ptr = hew_ser_finish(buf, &raw mut len);
            assert!(!ptr.is_null());
            let v = std::slice::from_raw_parts(ptr, len).to_vec();
            hew_ser_free_bytes(ptr);
            v
        };

        // SAFETY: test-controlled reader over owned bytes.
        unsafe {
            let reader = hew_de_reader_new(bytes.as_ptr(), bytes.len());
            let s = hew_de_read_string(reader);
            let read_i = hew_de_read_i64(reader);
            assert_eq!(hew_de_failed(reader), 0);
            let s_str = std::ffi::CStr::from_ptr(s).to_str().unwrap().to_owned();
            assert_eq!(s_str, "hello");
            assert_eq!(read_i, 42);
            crate::string::hew_string_drop(s);
            hew_de_reader_free(reader);
        }
    }

    #[test]
    fn truncated_stream_latches_failure_not_garbage() {
        // SAFETY: test-controlled reader over a deliberately too-short buffer.
        unsafe {
            // A valid i64 tag (0x02) but only 3 of 8 payload bytes.
            let bad = [0x02u8, 1, 2, 3];
            let reader = hew_de_reader_new(bad.as_ptr(), bad.len());
            let _ = hew_de_read_i64(reader);
            assert_eq!(
                hew_de_failed(reader),
                1,
                "truncated read must latch failure"
            );
            hew_de_reader_free(reader);
        }
    }

    #[test]
    fn tag_mismatch_fails_closed() {
        // SAFETY: test-controlled reader.
        unsafe {
            // Stream holds a string, decoder asks for an i64 → tag mismatch.
            let s = CString::new("x").unwrap();
            let buf = hew_ser_buf_new();
            hew_ser_push_string(buf, s.as_ptr());
            let mut len = 0usize;
            let ptr = hew_ser_finish(buf, &raw mut len);
            let bytes = std::slice::from_raw_parts(ptr, len).to_vec();
            hew_ser_free_bytes(ptr);

            let reader = hew_de_reader_new(bytes.as_ptr(), bytes.len());
            let _ = hew_de_read_i64(reader);
            assert_eq!(hew_de_failed(reader), 1, "tag mismatch must fail closed");
            hew_de_reader_free(reader);
        }
    }

    #[test]
    fn enum_tag_round_trips() {
        // SAFETY: test-controlled handles.
        unsafe {
            let buf = hew_ser_buf_new();
            hew_ser_push_enum_tag(buf, 1);
            hew_ser_push_string(buf, CString::new("world").unwrap().as_ptr());
            let mut len = 0usize;
            let ptr = hew_ser_finish(buf, &raw mut len);
            let bytes = std::slice::from_raw_parts(ptr, len).to_vec();
            hew_ser_free_bytes(ptr);

            let reader = hew_de_reader_new(bytes.as_ptr(), bytes.len());
            let t = hew_de_read_enum_tag(reader);
            let s = hew_de_read_string(reader);
            assert_eq!(t, 1);
            assert_eq!(hew_de_failed(reader), 0);
            assert_eq!(std::ffi::CStr::from_ptr(s).to_str().unwrap(), "world");
            crate::string::hew_string_drop(s);
            hew_de_reader_free(reader);
        }
    }

    // ── Cross-node codec collision repro ────────────────────────────────────
    //
    // Two distinct actor TYPES whose single-field handlers SipHash-collide on
    // the low 32 bits of `msg_type` must each route to their OWN codec. The
    // registry key is `(dispatch_ptr, msg_type)`: the per-actor-type dispatch
    // function pointer disambiguates colliding `msg_type` integers. A frame for
    // actor B can only ever select B's codec; A's codec is keyed under A's
    // dispatch pointer and is unreachable from B's identity. Without the
    // dispatch key (the pre-fix `msg_type`-only registry) the second
    // registration overwrites the first, so one actor's frames are silently
    // decoded by the other actor's codec — remote type-confusion.
    //
    // The two codecs reconstruct DIFFERENT owned-field layouts (codec A: a bare
    // `i64`; codec B: an owned `string`) so the wrong-codec selection is a
    // genuine type confusion (an i64 read as a string length+pointer, or a
    // string's bytes read as an i64), not a size coincidence.

    /// Two distinct dispatch-pointer stand-ins for actor types A and B. The real
    /// key is `__hew_actor_dispatch_<ActorType>`'s address; in the unit test any
    /// two distinct stable addresses model two distinct actor types.
    static DISPATCH_A: u8 = 0;
    static DISPATCH_B: u8 = 0;

    fn dispatch_a() -> *const c_void {
        std::ptr::addr_of!(DISPATCH_A).cast()
    }
    fn dispatch_b() -> *const c_void {
        std::ptr::addr_of!(DISPATCH_B).cast()
    }

    /// Codec A serialize: encode a single `i64` field.
    unsafe extern "C" fn ser_a(value_ptr: *const c_void, out_len: *mut usize) -> *mut u8 {
        // SAFETY: the test passes a pointer to an i64.
        let v = unsafe { *value_ptr.cast::<i64>() };
        let buf = hew_ser_buf_new();
        // SAFETY: buf is a live builder handle.
        unsafe { hew_ser_push_i64(buf, v) };
        // SAFETY: buf consumed; out_len writable.
        unsafe { hew_ser_finish(buf, out_len) }
    }

    /// Codec A deserialize: reconstruct a single `i64` into a malloc'd slot.
    unsafe extern "C" fn de_a(
        data: *const u8,
        len: usize,
        out_struct_size: *mut usize,
    ) -> *mut c_void {
        // SAFETY: data valid for len bytes (test contract).
        let reader = unsafe { hew_de_reader_new(data, len) };
        // SAFETY: reader live.
        let v = unsafe { hew_de_read_i64(reader) };
        // SAFETY: reader live.
        let failed = unsafe { hew_de_failed(reader) };
        // SAFETY: reader live, consumed here.
        unsafe { hew_de_reader_free(reader) };
        if failed != 0 {
            return std::ptr::null_mut();
        }
        // SAFETY: malloc for an i64 slot.
        let slot = unsafe { libc::malloc(std::mem::size_of::<i64>()) }.cast::<i64>();
        if slot.is_null() {
            return std::ptr::null_mut();
        }
        // SAFETY: slot is a fresh i64.
        unsafe { *slot = v };
        if !out_struct_size.is_null() {
            // SAFETY: out_struct_size writable.
            unsafe { *out_struct_size = std::mem::size_of::<i64>() };
        }
        slot.cast()
    }

    /// Codec B serialize: encode a single owned `string` field.
    unsafe extern "C" fn ser_b(value_ptr: *const c_void, out_len: *mut usize) -> *mut u8 {
        // SAFETY: the test passes a pointer to a `*const c_char`.
        let s = unsafe { *value_ptr.cast::<*const c_char>() };
        let buf = hew_ser_buf_new();
        // SAFETY: buf live; s is a valid C string.
        unsafe { hew_ser_push_string(buf, s) };
        // SAFETY: buf consumed; out_len writable.
        unsafe { hew_ser_finish(buf, out_len) }
    }

    /// Codec B deserialize: reconstruct an owned `string` into a malloc'd slot
    /// holding the owned `*mut c_char` (owned-field layout differs from A).
    unsafe extern "C" fn de_b(
        data: *const u8,
        len: usize,
        out_struct_size: *mut usize,
    ) -> *mut c_void {
        // SAFETY: data valid for len bytes (test contract).
        let reader = unsafe { hew_de_reader_new(data, len) };
        // SAFETY: reader live.
        let s = unsafe { hew_de_read_string(reader) };
        // SAFETY: reader live.
        let failed = unsafe { hew_de_failed(reader) };
        // SAFETY: reader live, consumed here.
        unsafe { hew_de_reader_free(reader) };
        if failed != 0 {
            // SAFETY: drop the empty owned string the reader handed back.
            unsafe { crate::string::hew_string_drop(s) };
            return std::ptr::null_mut();
        }
        // SAFETY: malloc for a `*mut c_char` slot.
        let slot =
            unsafe { libc::malloc(std::mem::size_of::<*mut c_char>()) }.cast::<*mut c_char>();
        if slot.is_null() {
            // SAFETY: reclaim the owned string on alloc failure.
            unsafe { crate::string::hew_string_drop(s) };
            return std::ptr::null_mut();
        }
        // SAFETY: slot is a fresh owned-pointer slot.
        unsafe { *slot = s };
        if !out_struct_size.is_null() {
            // SAFETY: out_struct_size writable.
            unsafe { *out_struct_size = std::mem::size_of::<*mut c_char>() };
        }
        slot.cast()
    }

    /// Encode an i64 the way actor A's send path would (codec A serialize).
    fn encode_with_a(value: i64) -> Vec<u8> {
        let mut len = 0usize;
        // SAFETY: ser_a reads the i64 we point at; out_len writable.
        let bytes = unsafe { ser_a(std::ptr::addr_of!(value).cast(), &raw mut len) };
        assert!(!bytes.is_null());
        // SAFETY: bytes valid for len.
        let v = unsafe { std::slice::from_raw_parts(bytes, len) }.to_vec();
        // SAFETY: bytes came from hew_ser_finish.
        unsafe { hew_ser_free_bytes(bytes) };
        v
    }

    /// Encode a string the way actor B's send path would (codec B serialize).
    fn encode_with_b(value: &str) -> Vec<u8> {
        let cstr = CString::new(value).unwrap();
        let ptr = cstr.as_ptr();
        let mut len = 0usize;
        // SAFETY: ser_b reads the `*const c_char` we point at; out_len writable.
        let bytes = unsafe { ser_b(std::ptr::addr_of!(ptr).cast(), &raw mut len) };
        assert!(!bytes.is_null());
        // SAFETY: bytes valid for len.
        let v = unsafe { std::slice::from_raw_parts(bytes, len) }.to_vec();
        // SAFETY: bytes came from hew_ser_finish.
        unsafe { hew_ser_free_bytes(bytes) };
        v
    }

    /// Serializes the codec-registry collision tests: both share the global
    /// `THUNK_REGISTRY` / `REPLY_REGISTRY` statics and each clears them at the
    /// end, so they must not interleave.
    static REGISTRY_TEST_MUTEX: Mutex<()> = Mutex::new(());

    #[test]
    fn colliding_actors_route_to_own_codec() {
        // Both actors share one SipHash-colliding msg_type.
        const COLLIDING_MSG_TYPE: i32 = 0x5151_5151;

        let _guard = REGISTRY_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);

        // SAFETY: register two distinct codecs under the SAME msg_type but
        // DIFFERENT dispatch keys (modeling two distinct actor types).
        unsafe {
            hew_xnode_register_codec(dispatch_a(), COLLIDING_MSG_TYPE, ser_a, de_a);
            hew_xnode_register_codec(dispatch_b(), COLLIDING_MSG_TYPE, ser_b, de_b);
        }

        // Actor A's frame (an i64) must decode under A's codec to the i64 value.
        let a_bytes = encode_with_a(-987_654_321);
        // SAFETY: decode against A's dispatch key; bytes valid for their length.
        let (a_val, a_size) = unsafe {
            decode_payload(
                dispatch_a(),
                COLLIDING_MSG_TYPE,
                a_bytes.as_ptr(),
                a_bytes.len(),
            )
        };
        assert!(
            !a_val.is_null(),
            "actor A's frame must route to A's codec, not be lost to B's registration"
        );
        // SAFETY: a_val is a malloc'd i64 slot from de_a.
        let a_decoded = unsafe { *a_val.cast::<i64>() };
        assert_eq!(a_size, std::mem::size_of::<i64>());
        assert_eq!(
            a_decoded, -987_654_321,
            "actor A's i64 must decode correctly under A's codec"
        );
        // SAFETY: a_val came from de_a (libc::malloc of an i64 slot).
        unsafe { libc::free(a_val) };

        // Actor B's frame (a string) must decode under B's codec to the string.
        let b_bytes = encode_with_b("collision-payload");
        // SAFETY: decode against B's dispatch key; bytes valid for their length.
        let (b_val, b_size) = unsafe {
            decode_payload(
                dispatch_b(),
                COLLIDING_MSG_TYPE,
                b_bytes.as_ptr(),
                b_bytes.len(),
            )
        };
        assert!(
            !b_val.is_null(),
            "actor B's frame must route to B's codec, not A's (type-confusion)"
        );
        // SAFETY: b_val is a malloc'd `*mut c_char` slot from de_b.
        let b_str_ptr = unsafe { *b_val.cast::<*mut c_char>() };
        assert_eq!(b_size, std::mem::size_of::<*mut c_char>());
        // SAFETY: b_str_ptr is an owned C string from de_b.
        let b_decoded = unsafe { std::ffi::CStr::from_ptr(b_str_ptr) }
            .to_str()
            .unwrap()
            .to_owned();
        assert_eq!(
            b_decoded, "collision-payload",
            "actor B's string must decode correctly under B's codec, not be \
             reinterpreted by A's i64 codec"
        );
        // SAFETY: b_str_ptr is an owned string; b_val is its malloc'd slot.
        unsafe {
            crate::string::hew_string_drop(b_str_ptr);
            libc::free(b_val);
        }

        // Clean the registry so the serial test ordering is hermetic.
        clear_codec_registries_for_test();
    }
}
