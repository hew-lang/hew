//! CBOR wire-body codec.
//!
//! This module is the runtime half of the wire-type body codec. The compiler
//! emits `__hew_cbor_serialize_<key>` / `__hew_cbor_deserialize_<key>` thunks
//! (see `hew-codegen-rs/src/llvm.rs`) that drive these `hew_cbor_*` primitives
//! to turn a `#[wire]` value into CBOR bytes and back. The bytes ride the actor
//! envelope's CBOR `bstr` payload slot unchanged — the envelope framing does not
//! know or care that the payload is itself CBOR.
//!
//! ## Why CBOR, and why hoist ciborium
//! The envelope framing already depends on `ciborium` (see `envelope.rs`). The
//! body codec reuses that exact dependency rather than carrying a bespoke binary
//! format (the retired "xnode" positional codec) or a second serialization
//! crate. `ciborium` is already in `hew-runtime/Cargo.toml`; this module adds no
//! new dependency.
//!
//! ## Encoder: a container stack
//! `hew_cbor_ser_new` allocates a builder; `hew_cbor_ser_finish` consumes it and
//! returns a `libc::malloc`'d byte buffer (freed by the shared
//! `hew_ser_free_bytes`). Between those, the thunk frames a struct as a CBOR map
//! keyed by the wire `@N` field tags:
//!
//! ```text
//! begin_map; key_u64(1); <emit field 1>; key_u64(2); <emit field 2>; end_map
//! ```
//!
//! A pending key set by `key_u64` lives on the open map frame and survives while
//! a child container (a nested map or a list array) is built on top, so nested
//! and repeated fields attach to the right key when their container closes.
//!
//! ## Decoder: a consuming tree cursor
//! `hew_cbor_de_new` parses the whole payload up front (rejecting trailing bytes)
//! and stages the root value. `enter_map` converts a staged map into a
//! by-integer-key lookup; `select_key` removes and stages the value for one tag;
//! the typed reads (`hew_cbor_de_i64`, ...) consume the staged value. The reader
//! OWNS the parsed value tree, so `hew_cbor_de_free` drops every parsed-but-
//! unconsumed sub-value — no leak of a list/map the walk never reached.
//!
//! ## Fail-closed (CLAUDE.md §2)
//! Every malformed / truncated / type-mismatched / missing-required-key / unknown
//! shape latches `failed` and returns a zero value rather than fabricating data.
//! The deserialize thunk checks `hew_cbor_de_failed` after the walk and, on
//! failure, drops any owned fields already written and frees the value shell
//! before returning null — the codegen call site then traps. A decode never
//! delivers a partial or fabricated value.

use core::ffi::{c_char, c_void};
use std::collections::BTreeMap;
use std::io::Cursor;

use ciborium::value::{Integer, Value};
use hew_cabi::cabi::malloc_cstring;

// ── Encoder ────────────────────────────────────────────────────────────────

/// One open container on the encoder stack.
#[derive(Debug)]
enum SerFrame {
    /// A CBOR map under construction. `pending_key` is the integer key set by
    /// the most recent `hew_cbor_ser_key_u64`, awaiting its value; it persists
    /// while a child container is built on top of this frame.
    Map {
        pending_key: Option<i128>,
        entries: Vec<(Value, Value)>,
    },
    /// A CBOR array under construction (a `List` / `Vec<T>` field).
    Array { items: Vec<Value> },
}

/// The CBOR encode builder behind a `hew_cbor_ser_new` handle.
#[derive(Debug)]
struct CborSerBuf {
    /// Open containers, innermost last.
    stack: Vec<SerFrame>,
    /// The finished top-level value, set when the outermost container closes
    /// (or a bare scalar is emitted at the root).
    root: Option<Value>,
    /// Latched once the builder is driven into an inconsistent state (a value
    /// emitted into a map with no pending key, a key on a non-map, an unbalanced
    /// close). A real codegen walk never trips this; if it somehow does, `finish`
    /// returns null and the decoder fails closed rather than emitting half a value.
    poisoned: bool,
}

impl CborSerBuf {
    fn new() -> Self {
        Self {
            stack: Vec::new(),
            root: None,
            poisoned: false,
        }
    }

    /// Attach a fully-built value to the current context: the pending key of the
    /// top map, the top array, or the root.
    fn emit(&mut self, value: Value) {
        match self.stack.last_mut() {
            Some(SerFrame::Map {
                pending_key,
                entries,
            }) => match pending_key.take() {
                Some(key) => entries.push((Value::Integer(int_from_i128(key)), value)),
                None => self.poisoned = true,
            },
            Some(SerFrame::Array { items }) => items.push(value),
            None => {
                if self.root.is_some() {
                    self.poisoned = true;
                } else {
                    self.root = Some(value);
                }
            }
        }
    }
}

/// Build a ciborium `Integer` from an `i128`, clamping out-of-range values to a
/// saturating bound (CBOR integers span `[-2^64, 2^64-1]`, i.e. they fit i128).
/// A wire tag or scalar always fits, so this never actually clamps in practice.
fn int_from_i128(v: i128) -> Integer {
    Integer::try_from(v).unwrap_or_else(|_| {
        if v < 0 {
            Integer::from(i64::MIN)
        } else {
            Integer::from(u64::MAX)
        }
    })
}

/// SAFETY: `buf` must be null or a live handle from `hew_cbor_ser_new` that no
/// other thread is touching.
unsafe fn as_ser_buf<'a>(buf: *mut c_void) -> Option<&'a mut CborSerBuf> {
    if buf.is_null() {
        None
    } else {
        // SAFETY: non-null handle from `hew_cbor_ser_new`, exclusive per contract.
        Some(unsafe { &mut *(buf.cast::<CborSerBuf>()) })
    }
}

/// Allocate a CBOR encode builder. Free it with `hew_cbor_ser_finish` (which
/// also returns the bytes) or `hew_cbor_ser_abort`.
#[no_mangle]
pub extern "C" fn hew_cbor_ser_new() -> *mut c_void {
    Box::into_raw(Box::new(CborSerBuf::new())).cast::<c_void>()
}

/// Open a CBOR map (a struct / record body).
///
/// # Safety
/// `buf` must be a live handle from `hew_cbor_ser_new`.
#[no_mangle]
pub unsafe extern "C" fn hew_cbor_ser_begin_map(buf: *mut c_void) {
    // SAFETY: buf is a live handle per this fn's contract.
    if let Some(b) = unsafe { as_ser_buf(buf) } {
        b.stack.push(SerFrame::Map {
            pending_key: None,
            entries: Vec::new(),
        });
    }
}

/// Close the current CBOR map and attach it to the enclosing context.
///
/// # Safety
/// `buf` must be a live handle from `hew_cbor_ser_new`.
#[no_mangle]
pub unsafe extern "C" fn hew_cbor_ser_end_map(buf: *mut c_void) {
    // SAFETY: buf is a live handle per this fn's contract.
    let Some(b) = (unsafe { as_ser_buf(buf) }) else {
        return;
    };
    match b.stack.pop() {
        Some(SerFrame::Map { entries, .. }) => b.emit(Value::Map(entries)),
        _ => b.poisoned = true,
    }
}

/// Open a CBOR array (a `List` / `Vec<T>` field).
///
/// # Safety
/// `buf` must be a live handle from `hew_cbor_ser_new`.
#[no_mangle]
pub unsafe extern "C" fn hew_cbor_ser_begin_array(buf: *mut c_void) {
    // SAFETY: buf is a live handle per this fn's contract.
    if let Some(b) = unsafe { as_ser_buf(buf) } {
        b.stack.push(SerFrame::Array { items: Vec::new() });
    }
}

/// Close the current CBOR array and attach it to the enclosing context.
///
/// # Safety
/// `buf` must be a live handle from `hew_cbor_ser_new`.
#[no_mangle]
pub unsafe extern "C" fn hew_cbor_ser_end_array(buf: *mut c_void) {
    // SAFETY: buf is a live handle per this fn's contract.
    let Some(b) = (unsafe { as_ser_buf(buf) }) else {
        return;
    };
    match b.stack.pop() {
        Some(SerFrame::Array { items }) => b.emit(Value::Array(items)),
        _ => b.poisoned = true,
    }
}

/// Set the integer key (`@N` wire tag) for the next value emitted into the
/// current map.
///
/// # Safety
/// `buf` must be a live handle from `hew_cbor_ser_new`.
#[no_mangle]
pub unsafe extern "C" fn hew_cbor_ser_key_u64(buf: *mut c_void, key: u64) {
    // SAFETY: buf is a live handle per this fn's contract.
    let Some(b) = (unsafe { as_ser_buf(buf) }) else {
        return;
    };
    match b.stack.last_mut() {
        Some(SerFrame::Map { pending_key, .. }) => *pending_key = Some(i128::from(key)),
        _ => b.poisoned = true,
    }
}

/// Emit a signed integer value.
///
/// # Safety
/// `buf` must be a live handle from `hew_cbor_ser_new`.
#[no_mangle]
pub unsafe extern "C" fn hew_cbor_ser_i64(buf: *mut c_void, value: i64) {
    // SAFETY: buf is a live handle per this fn's contract.
    if let Some(b) = unsafe { as_ser_buf(buf) } {
        b.emit(Value::Integer(Integer::from(value)));
    }
}

/// Emit an unsigned integer value.
///
/// # Safety
/// `buf` must be a live handle from `hew_cbor_ser_new`.
#[no_mangle]
pub unsafe extern "C" fn hew_cbor_ser_u64(buf: *mut c_void, value: u64) {
    // SAFETY: buf is a live handle per this fn's contract.
    if let Some(b) = unsafe { as_ser_buf(buf) } {
        b.emit(Value::Integer(Integer::from(value)));
    }
}

/// Emit a floating-point value (f32 is widened to f64 by the thunk).
///
/// # Safety
/// `buf` must be a live handle from `hew_cbor_ser_new`.
#[no_mangle]
pub unsafe extern "C" fn hew_cbor_ser_f64(buf: *mut c_void, value: f64) {
    // SAFETY: buf is a live handle per this fn's contract.
    if let Some(b) = unsafe { as_ser_buf(buf) } {
        b.emit(Value::Float(value));
    }
}

/// Emit a boolean value (`value` is the i8 low bit).
///
/// # Safety
/// `buf` must be a live handle from `hew_cbor_ser_new`.
#[no_mangle]
pub unsafe extern "C" fn hew_cbor_ser_bool(buf: *mut c_void, value: i8) {
    // SAFETY: buf is a live handle per this fn's contract.
    if let Some(b) = unsafe { as_ser_buf(buf) } {
        b.emit(Value::Bool(value != 0));
    }
}

/// Emit a CBOR null (an `Optional` field's `None`).
///
/// # Safety
/// `buf` must be a live handle from `hew_cbor_ser_new`.
#[no_mangle]
pub unsafe extern "C" fn hew_cbor_ser_null(buf: *mut c_void) {
    // SAFETY: buf is a live handle per this fn's contract.
    if let Some(b) = unsafe { as_ser_buf(buf) } {
        b.emit(Value::Null);
    }
}

/// Emit a string value from a NUL-terminated C string. A null pointer encodes an
/// empty string.
///
/// # Safety
/// `buf` must be a live handle; `s` must be null or a valid NUL-terminated C
/// string that stays valid for the duration of the call.
#[no_mangle]
pub unsafe extern "C" fn hew_cbor_ser_string(buf: *mut c_void, s: *const c_char) {
    // SAFETY: buf is a live handle per this fn's contract.
    let Some(b) = (unsafe { as_ser_buf(buf) }) else {
        return;
    };
    let text = if s.is_null() {
        String::new()
    } else {
        // SAFETY: s is a valid NUL-terminated C string per this fn's contract.
        let cstr = unsafe { core::ffi::CStr::from_ptr(s) };
        cstr.to_string_lossy().into_owned()
    };
    b.emit(Value::Text(text));
}

/// Emit a `bytes` value from `(ptr + offset, len)`. A zero length or null pointer
/// encodes an empty byte string.
///
/// # Safety
/// `buf` must be a live handle; `(ptr + offset)` must be valid for `len` bytes.
#[no_mangle]
pub unsafe extern "C" fn hew_cbor_ser_bytes(
    buf: *mut c_void,
    ptr: *const u8,
    offset: u32,
    len: u32,
) {
    // SAFETY: buf is a live handle per this fn's contract.
    let Some(b) = (unsafe { as_ser_buf(buf) }) else {
        return;
    };
    if len == 0 || ptr.is_null() {
        b.emit(Value::Bytes(Vec::new()));
        return;
    }
    // SAFETY: caller guarantees (ptr + offset) is valid for len bytes.
    let start = unsafe { ptr.add(offset as usize) };
    // SAFETY: start is valid for len bytes (the active byte range).
    let bytes = unsafe { std::slice::from_raw_parts(start, len as usize) };
    b.emit(Value::Bytes(bytes.to_vec()));
}

/// Consume the builder and return a `libc::malloc`'d copy of the CBOR bytes;
/// writes the length into `*out_len`. Returns null on an unbalanced / poisoned
/// build (a codegen bug) so the caller sees a zero-length body and the decoder
/// fails closed. Caller owns the buffer; free with `hew_ser_free_bytes`.
///
/// # Safety
/// `buf` must be a live handle from `hew_cbor_ser_new` (consumed here);
/// `out_len` must be a valid writable `*mut usize`.
#[no_mangle]
pub unsafe extern "C" fn hew_cbor_ser_finish(buf: *mut c_void, out_len: *mut usize) -> *mut u8 {
    if !out_len.is_null() {
        // SAFETY: out_len validated non-null.
        unsafe { *out_len = 0 };
    }
    if buf.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: buf is a live handle from `hew_cbor_ser_new`; consume it.
    let b = unsafe { Box::from_raw(buf.cast::<CborSerBuf>()) };
    if b.poisoned || !b.stack.is_empty() {
        return std::ptr::null_mut();
    }
    let Some(root) = b.root else {
        return std::ptr::null_mut();
    };
    let mut encoded: Vec<u8> = Vec::new();
    if ciborium::ser::into_writer(&root, &mut encoded).is_err() {
        return std::ptr::null_mut();
    }
    if encoded.is_empty() {
        return std::ptr::null_mut();
    }
    // SAFETY: malloc returns a valid pointer or null.
    let dst = unsafe { libc::malloc(encoded.len()) }.cast::<u8>();
    if dst.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: dst has encoded.len() bytes; encoded is a valid slice.
    unsafe { std::ptr::copy_nonoverlapping(encoded.as_ptr(), dst, encoded.len()) };
    if !out_len.is_null() {
        // SAFETY: out_len validated non-null.
        unsafe { *out_len = encoded.len() };
    }
    dst
}

/// Discard an encode builder without producing output (the encode-side error
/// path).
///
/// # Safety
/// `buf` must be a live handle from `hew_cbor_ser_new` (consumed here).
#[no_mangle]
pub unsafe extern "C" fn hew_cbor_ser_abort(buf: *mut c_void) {
    if buf.is_null() {
        return;
    }
    // SAFETY: buf is a live handle from `hew_cbor_ser_new`; drop it.
    drop(unsafe { Box::from_raw(buf.cast::<CborSerBuf>()) });
}

// ── Decoder ────────────────────────────────────────────────────────────────

/// One open container on the decoder stack.
#[derive(Debug)]
enum DeFrame {
    /// An entered CBOR map, keyed by integer tag. Entries are removed as the walk
    /// selects them; whatever remains at `exit_map` is unknown-forward-compatible
    /// and dropped.
    Map(BTreeMap<i128, Value>),
    /// An entered CBOR array, walked by sequential `array_next`.
    Array(std::vec::IntoIter<Value>),
}

/// The CBOR decode cursor behind a `hew_cbor_de_new` handle. Owns the parsed
/// value tree; `hew_cbor_de_free` drops it (and any unconsumed sub-values).
#[derive(Debug)]
struct CborDeReader {
    /// Open containers, innermost last.
    stack: Vec<DeFrame>,
    /// The value selected by the most recent `select_key` / `array_next` /
    /// `hew_cbor_de_new`, awaiting a typed read.
    staged: Option<Value>,
    /// Latched on any malformed / truncated / type-mismatched / missing-key input.
    failed: bool,
}

impl CborDeReader {
    fn take_staged(&mut self) -> Option<Value> {
        self.staged.take()
    }
}

/// SAFETY: `reader` must be null or a live handle from `hew_cbor_de_new` that no
/// other thread is touching.
unsafe fn as_de_reader<'a>(reader: *mut c_void) -> Option<&'a mut CborDeReader> {
    if reader.is_null() {
        None
    } else {
        // SAFETY: non-null handle from `hew_cbor_de_new`, exclusive per contract.
        Some(unsafe { &mut *(reader.cast::<CborDeReader>()) })
    }
}

/// Parse `len` bytes at `data` as a single CBOR value and stage it for the walk.
/// Trailing bytes after the value are rejected (fail-closed). Returns a reader
/// handle (which may already be in the failed state); free with
/// `hew_cbor_de_free`.
///
/// # Safety
/// `data` must be valid for `len` bytes (or null when `len` is 0).
#[no_mangle]
pub unsafe extern "C" fn hew_cbor_de_new(data: *const u8, len: usize) -> *mut c_void {
    let mut reader = CborDeReader {
        stack: Vec::new(),
        staged: None,
        failed: false,
    };
    if data.is_null() || len == 0 {
        reader.failed = true;
        return Box::into_raw(Box::new(reader)).cast::<c_void>();
    }
    // SAFETY: data is valid for len bytes per this fn's contract.
    let slice = unsafe { std::slice::from_raw_parts(data, len) };
    let mut cursor = Cursor::new(slice);
    match ciborium::de::from_reader::<Value, _>(&mut cursor) {
        Ok(value) => {
            // Reject trailing bytes: a valid payload is exactly one CBOR value.
            if usize::try_from(cursor.position()).is_ok_and(|pos| pos == len) {
                reader.staged = Some(value);
            } else {
                reader.failed = true;
            }
        }
        Err(_) => reader.failed = true,
    }
    Box::into_raw(Box::new(reader)).cast::<c_void>()
}

/// Returns 1 if the reader has latched a decode failure, else 0.
///
/// # Safety
/// `reader` must be a live handle from `hew_cbor_de_new`.
#[no_mangle]
pub unsafe extern "C" fn hew_cbor_de_failed(reader: *const c_void) -> i32 {
    if reader.is_null() {
        return 1;
    }
    // SAFETY: reader is a live handle per this fn's contract.
    let r = unsafe { &*(reader.cast::<CborDeReader>()) };
    i32::from(r.failed)
}

/// Free a decode reader and the entire parsed value tree it owns.
///
/// # Safety
/// `reader` must be null or a live handle from `hew_cbor_de_new` (consumed here).
#[no_mangle]
pub unsafe extern "C" fn hew_cbor_de_free(reader: *mut c_void) {
    if reader.is_null() {
        return;
    }
    // SAFETY: reader is a live handle from `hew_cbor_de_new`; drop it.
    drop(unsafe { Box::from_raw(reader.cast::<CborDeReader>()) });
}

/// Enter the staged map value, pushing a by-tag lookup frame. Fails closed if the
/// staged value is not a map, has a non-integer key, or has a duplicate key.
///
/// # Safety
/// `reader` must be a live handle from `hew_cbor_de_new`.
#[no_mangle]
pub unsafe extern "C" fn hew_cbor_de_enter_map(reader: *mut c_void) {
    // SAFETY: reader is a live handle per this fn's contract.
    let Some(r) = (unsafe { as_de_reader(reader) }) else {
        return;
    };
    let Some(Value::Map(pairs)) = r.take_staged() else {
        r.failed = true;
        return;
    };
    let mut map: BTreeMap<i128, Value> = BTreeMap::new();
    for (k, v) in pairs {
        let Value::Integer(key) = k else {
            r.failed = true;
            return;
        };
        if map.insert(i128::from(key), v).is_some() {
            // Duplicate key: ambiguous, reject.
            r.failed = true;
            return;
        }
    }
    r.stack.push(DeFrame::Map(map));
}

/// Exit the current map frame. Any unselected (unknown / forward-compatible) keys
/// are dropped.
///
/// # Safety
/// `reader` must be a live handle from `hew_cbor_de_new`.
#[no_mangle]
pub unsafe extern "C" fn hew_cbor_de_exit_map(reader: *mut c_void) {
    // SAFETY: reader is a live handle per this fn's contract.
    let Some(r) = (unsafe { as_de_reader(reader) }) else {
        return;
    };
    match r.stack.pop() {
        Some(DeFrame::Map(_)) => {}
        _ => r.failed = true,
    }
}

/// Select the value under integer `key` in the current map frame, staging it for
/// a typed read. Fails closed if there is no open map or the key is absent (a
/// missing required field).
///
/// # Safety
/// `reader` must be a live handle from `hew_cbor_de_new`.
#[no_mangle]
pub unsafe extern "C" fn hew_cbor_de_select_key(reader: *mut c_void, key: u64) {
    // SAFETY: reader is a live handle per this fn's contract.
    let Some(r) = (unsafe { as_de_reader(reader) }) else {
        return;
    };
    let Some(DeFrame::Map(map)) = r.stack.last_mut() else {
        r.failed = true;
        return;
    };
    match map.remove(&i128::from(key)) {
        Some(value) => r.staged = Some(value),
        None => r.failed = true,
    }
}

/// Enter the staged array value, pushing an array frame read by sequential
/// `hew_cbor_de_array_next`. Fails closed if the staged value is not an array.
///
/// # Safety
/// `reader` must be a live handle from `hew_cbor_de_new`.
#[no_mangle]
pub unsafe extern "C" fn hew_cbor_de_enter_array(reader: *mut c_void) {
    // SAFETY: reader is a live handle per this fn's contract.
    let Some(r) = (unsafe { as_de_reader(reader) }) else {
        return;
    };
    let Some(value) = r.take_staged() else {
        r.failed = true;
        return;
    };
    let Value::Array(items) = value else {
        r.failed = true;
        return;
    };
    r.stack.push(DeFrame::Array(items.into_iter()));
}

/// Advance the current array frame, staging the next element for a read. Returns
/// 1 when an element was staged, 0 when the array is exhausted (the thunk stops
/// its element loop). Fails closed if there is no open array or a prior staged
/// element was never consumed.
///
/// # Safety
/// `reader` must be a live handle from `hew_cbor_de_new`.
#[no_mangle]
pub unsafe extern "C" fn hew_cbor_de_array_next(reader: *mut c_void) -> i32 {
    // SAFETY: reader is a live handle per this fn's contract.
    let Some(r) = (unsafe { as_de_reader(reader) }) else {
        return 0;
    };
    if r.staged.is_some() {
        r.failed = true;
        return 0;
    }
    let Some(DeFrame::Array(items)) = r.stack.last_mut() else {
        r.failed = true;
        return 0;
    };
    match items.next() {
        Some(value) => {
            r.staged = Some(value);
            1
        }
        None => 0,
    }
}

/// Exit the current array frame. Any unconsumed trailing elements are dropped.
///
/// # Safety
/// `reader` must be a live handle from `hew_cbor_de_new`.
#[no_mangle]
pub unsafe extern "C" fn hew_cbor_de_exit_array(reader: *mut c_void) {
    // SAFETY: reader is a live handle per this fn's contract.
    let Some(r) = (unsafe { as_de_reader(reader) }) else {
        return;
    };
    match r.stack.pop() {
        Some(DeFrame::Array(_)) => {}
        _ => r.failed = true,
    }
}

/// Begin decoding a wire enum, returning the active variant's integer tag and
/// staging its payload for the field reads that follow.
///
/// The wire shape of an enum (the "map-of-one" form, q185 Qa) is one of:
/// - a bare unsigned integer `N` — a *unit* variant with tag `N`; or
/// - a single-entry map `{N: [field0, field1, ...]}` — a *payload* variant with
///   tag `N` whose value is the positional payload array.
///
/// Either way this pushes an array frame so the caller reads payload fields with
/// `hew_cbor_de_array_next` and closes with `hew_cbor_de_enum_end` — symmetric
/// for both forms (a unit variant pushes an empty payload array, so the
/// caller's `enum_end` always pops exactly one array frame). Fails closed (and
/// still pushes a balancing empty frame) on anything else: nothing staged, a
/// negative tag, a multi-entry map, or a single entry whose value is not an
/// array.
///
/// # Safety
/// `reader` must be a live handle from `hew_cbor_de_new`.
#[no_mangle]
pub unsafe extern "C" fn hew_cbor_de_enum_begin(reader: *mut c_void) -> u64 {
    // SAFETY: reader is a live handle per this fn's contract.
    let Some(r) = (unsafe { as_de_reader(reader) }) else {
        return 0;
    };
    let Some(value) = r.take_staged() else {
        r.failed = true;
        r.stack.push(DeFrame::Array(Vec::new().into_iter()));
        return 0;
    };
    match value {
        // Unit variant: a bare non-negative integer tag, empty payload.
        Value::Integer(tag) => {
            let raw = i128::from(tag);
            let Ok(tag_u64) = u64::try_from(raw) else {
                r.failed = true;
                r.stack.push(DeFrame::Array(Vec::new().into_iter()));
                return 0;
            };
            r.stack.push(DeFrame::Array(Vec::new().into_iter()));
            tag_u64
        }
        // Payload variant: exactly one `{tag: [payload...]}` entry.
        Value::Map(pairs) => {
            let mut entries = pairs.into_iter();
            let (Some((key, body)), None) = (entries.next(), entries.next()) else {
                // Zero or more-than-one entries: not a well-formed payload variant.
                r.failed = true;
                r.stack.push(DeFrame::Array(Vec::new().into_iter()));
                return 0;
            };
            let (Value::Integer(tag), Value::Array(items)) = (key, body) else {
                r.failed = true;
                r.stack.push(DeFrame::Array(Vec::new().into_iter()));
                return 0;
            };
            let Ok(tag_u64) = u64::try_from(i128::from(tag)) else {
                r.failed = true;
                r.stack.push(DeFrame::Array(Vec::new().into_iter()));
                return 0;
            };
            r.stack.push(DeFrame::Array(items.into_iter()));
            tag_u64
        }
        _ => {
            r.failed = true;
            r.stack.push(DeFrame::Array(Vec::new().into_iter()));
            0
        }
    }
}

/// Close a wire enum's payload frame opened by `hew_cbor_de_enum_begin`. Any
/// unconsumed trailing payload elements (forward-compatible extra fields) are
/// dropped. Fails closed if the top frame is not the enum's payload array.
///
/// # Safety
/// `reader` must be a live handle from `hew_cbor_de_new`.
#[no_mangle]
pub unsafe extern "C" fn hew_cbor_de_enum_end(reader: *mut c_void) {
    // SAFETY: reader is a live handle per this fn's contract.
    let Some(r) = (unsafe { as_de_reader(reader) }) else {
        return;
    };
    match r.stack.pop() {
        Some(DeFrame::Array(_)) => {}
        _ => r.failed = true,
    }
}

/// Returns 1 if the staged value is CBOR null (an optional field's `None`), else
/// 0. Non-consuming: the thunk reads the underlying value afterwards for a
/// `Some`. Latches failure (and returns 1) if nothing is staged.
///
/// # Safety
/// `reader` must be a live handle from `hew_cbor_de_new`.
#[no_mangle]
pub unsafe extern "C" fn hew_cbor_de_is_null(reader: *mut c_void) -> i32 {
    // SAFETY: reader is a live handle per this fn's contract.
    let Some(r) = (unsafe { as_de_reader(reader) }) else {
        return 1;
    };
    match &r.staged {
        Some(Value::Null) => 1,
        Some(_) => 0,
        None => {
            r.failed = true;
            1
        }
    }
}

/// Discard the staged value (an optional field's `None`, whose CBOR null carries
/// no payload to reconstruct).
///
/// # Safety
/// `reader` must be a live handle from `hew_cbor_de_new`.
#[no_mangle]
pub unsafe extern "C" fn hew_cbor_de_skip(reader: *mut c_void) {
    // SAFETY: reader is a live handle per this fn's contract.
    if let Some(r) = unsafe { as_de_reader(reader) } {
        let _ = r.take_staged();
    }
}

/// Read the staged value as a signed integer. Latches failure and returns 0 on
/// type mismatch / out-of-range.
///
/// # Safety
/// `reader` must be a live handle from `hew_cbor_de_new`.
#[no_mangle]
pub unsafe extern "C" fn hew_cbor_de_i64(reader: *mut c_void) -> i64 {
    // SAFETY: reader is a live handle per this fn's contract.
    let Some(r) = (unsafe { as_de_reader(reader) }) else {
        return 0;
    };
    let Some(Value::Integer(integer)) = r.take_staged() else {
        r.failed = true;
        return 0;
    };
    let Ok(v) = i64::try_from(i128::from(integer)) else {
        r.failed = true;
        return 0;
    };
    v
}

/// Read the staged value as an unsigned integer. Latches failure and returns 0
/// on type mismatch / out-of-range.
///
/// # Safety
/// `reader` must be a live handle from `hew_cbor_de_new`.
#[no_mangle]
pub unsafe extern "C" fn hew_cbor_de_u64(reader: *mut c_void) -> u64 {
    // SAFETY: reader is a live handle per this fn's contract.
    let Some(r) = (unsafe { as_de_reader(reader) }) else {
        return 0;
    };
    let Some(Value::Integer(integer)) = r.take_staged() else {
        r.failed = true;
        return 0;
    };
    let Ok(v) = u64::try_from(i128::from(integer)) else {
        r.failed = true;
        return 0;
    };
    v
}

/// Read the staged value as a float. Latches failure and returns 0.0 on type
/// mismatch.
///
/// # Safety
/// `reader` must be a live handle from `hew_cbor_de_new`.
#[no_mangle]
pub unsafe extern "C" fn hew_cbor_de_f64(reader: *mut c_void) -> f64 {
    // SAFETY: reader is a live handle per this fn's contract.
    let Some(r) = (unsafe { as_de_reader(reader) }) else {
        return 0.0;
    };
    let Some(Value::Float(f)) = r.take_staged() else {
        r.failed = true;
        return 0.0;
    };
    f
}

/// Read the staged value as a boolean (returns the i8 0/1). Latches failure and
/// returns 0 on type mismatch.
///
/// # Safety
/// `reader` must be a live handle from `hew_cbor_de_new`.
#[no_mangle]
pub unsafe extern "C" fn hew_cbor_de_bool(reader: *mut c_void) -> i8 {
    // SAFETY: reader is a live handle per this fn's contract.
    let Some(r) = (unsafe { as_de_reader(reader) }) else {
        return 0;
    };
    let Some(Value::Bool(b)) = r.take_staged() else {
        r.failed = true;
        return 0;
    };
    i8::from(b)
}

/// Read the staged value as a string into a freshly `malloc_cstring`'d buffer the
/// caller owns (drop with `hew_string_drop`). Latches failure and returns an
/// empty owned string on type mismatch.
///
/// # Safety
/// `reader` must be a live handle from `hew_cbor_de_new`.
#[no_mangle]
pub unsafe extern "C" fn hew_cbor_de_string(reader: *mut c_void) -> *mut c_char {
    // SAFETY: reader is a live handle per this fn's contract.
    let Some(r) = (unsafe { as_de_reader(reader) }) else {
        // SAFETY: empty owned string.
        return unsafe { malloc_cstring(std::ptr::null(), 0) };
    };
    let Some(Value::Text(text)) = r.take_staged() else {
        r.failed = true;
        // SAFETY: empty owned string.
        return unsafe { malloc_cstring(std::ptr::null(), 0) };
    };
    let bytes = text.as_bytes();
    // SAFETY: bytes is valid for its length; malloc_cstring copies it.
    unsafe { malloc_cstring(bytes.as_ptr(), bytes.len()) }
}

/// Read the staged value as a `bytes` field into a freshly `libc::malloc`'d
/// buffer; writes the length to `*out_len`. Returns null for an empty read.
/// Latches failure on type mismatch. Caller owns the buffer.
///
/// # Safety
/// `reader` must be a live handle; `out_len` must be a valid writable pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_cbor_de_bytes(reader: *mut c_void, out_len: *mut u32) -> *mut u8 {
    if !out_len.is_null() {
        // SAFETY: out_len validated non-null.
        unsafe { *out_len = 0 };
    }
    // SAFETY: reader is a live handle per this fn's contract.
    let Some(r) = (unsafe { as_de_reader(reader) }) else {
        return std::ptr::null_mut();
    };
    let Some(Value::Bytes(bytes)) = r.take_staged() else {
        r.failed = true;
        return std::ptr::null_mut();
    };
    if bytes.is_empty() {
        return std::ptr::null_mut();
    }
    let Ok(len) = u32::try_from(bytes.len()) else {
        r.failed = true;
        return std::ptr::null_mut();
    };
    // SAFETY: malloc returns a valid pointer or null.
    let dst = unsafe { libc::malloc(bytes.len()) }.cast::<u8>();
    if dst.is_null() {
        r.failed = true;
        return std::ptr::null_mut();
    }
    // SAFETY: dst has bytes.len() bytes; bytes is a valid slice.
    unsafe { std::ptr::copy_nonoverlapping(bytes.as_ptr(), dst, bytes.len()) };
    if !out_len.is_null() {
        // SAFETY: out_len validated non-null.
        unsafe { *out_len = len };
    }
    dst
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A struct body `{1: "hi", 2: -7}` round-trips through encode → bytes →
    /// decode with the field values intact.
    #[test]
    fn round_trip_map_with_string_and_i64() {
        let key2 = std::ffi::CString::new("hi").unwrap();
        // SAFETY: test-controlled handles.
        let bytes = unsafe {
            let buf = hew_cbor_ser_new();
            hew_cbor_ser_begin_map(buf);
            hew_cbor_ser_key_u64(buf, 1);
            hew_cbor_ser_string(buf, key2.as_ptr());
            hew_cbor_ser_key_u64(buf, 2);
            hew_cbor_ser_i64(buf, -7);
            hew_cbor_ser_end_map(buf);
            let mut len = 0usize;
            let ptr = hew_cbor_ser_finish(buf, &raw mut len);
            assert!(!ptr.is_null());
            let v = std::slice::from_raw_parts(ptr, len).to_vec();
            libc::free(ptr.cast());
            v
        };
        // SAFETY: test-controlled reader.
        unsafe {
            let r = hew_cbor_de_new(bytes.as_ptr(), bytes.len());
            hew_cbor_de_enter_map(r);
            hew_cbor_de_select_key(r, 1);
            let s = hew_cbor_de_string(r);
            hew_cbor_de_select_key(r, 2);
            let n = hew_cbor_de_i64(r);
            hew_cbor_de_exit_map(r);
            assert_eq!(hew_cbor_de_failed(r), 0);
            assert_eq!(n, -7);
            let got = core::ffi::CStr::from_ptr(s).to_str().unwrap().to_owned();
            assert_eq!(got, "hi");
            hew_string_drop_for_test(s);
            hew_cbor_de_free(r);
        }
    }

    /// A truncated payload fails closed.
    #[test]
    fn truncated_bytes_fail_closed() {
        // SAFETY: test-controlled handles.
        let mut bytes = unsafe {
            let buf = hew_cbor_ser_new();
            hew_cbor_ser_begin_map(buf);
            hew_cbor_ser_key_u64(buf, 1);
            hew_cbor_ser_i64(buf, 1234);
            hew_cbor_ser_end_map(buf);
            let mut len = 0usize;
            let ptr = hew_cbor_ser_finish(buf, &raw mut len);
            let v = std::slice::from_raw_parts(ptr, len).to_vec();
            libc::free(ptr.cast());
            v
        };
        bytes.pop(); // chop the last byte
                     // SAFETY: test-controlled reader.
        unsafe {
            let r = hew_cbor_de_new(bytes.as_ptr(), bytes.len());
            assert_eq!(hew_cbor_de_failed(r), 1, "truncated CBOR must fail closed");
            hew_cbor_de_free(r);
        }
    }

    /// Trailing garbage after a valid value fails closed.
    #[test]
    fn trailing_bytes_fail_closed() {
        // SAFETY: test-controlled handles.
        let mut bytes = unsafe {
            let buf = hew_cbor_ser_new();
            hew_cbor_ser_begin_map(buf);
            hew_cbor_ser_key_u64(buf, 1);
            hew_cbor_ser_i64(buf, 1);
            hew_cbor_ser_end_map(buf);
            let mut len = 0usize;
            let ptr = hew_cbor_ser_finish(buf, &raw mut len);
            let v = std::slice::from_raw_parts(ptr, len).to_vec();
            libc::free(ptr.cast());
            v
        };
        bytes.push(0xff); // append a trailing byte
                          // SAFETY: test-controlled reader.
        unsafe {
            let r = hew_cbor_de_new(bytes.as_ptr(), bytes.len());
            assert_eq!(hew_cbor_de_failed(r), 1, "trailing bytes must fail closed");
            hew_cbor_de_free(r);
        }
    }

    /// Reading a field at the wrong type fails closed (never fabricates a value).
    #[test]
    fn type_mismatch_fails_closed() {
        // Encode `{1: "text"}`, then try to read key 1 as an integer.
        let text = std::ffi::CString::new("text").unwrap();
        // SAFETY: test-controlled handles.
        let bytes = unsafe {
            let buf = hew_cbor_ser_new();
            hew_cbor_ser_begin_map(buf);
            hew_cbor_ser_key_u64(buf, 1);
            hew_cbor_ser_string(buf, text.as_ptr());
            hew_cbor_ser_end_map(buf);
            let mut len = 0usize;
            let ptr = hew_cbor_ser_finish(buf, &raw mut len);
            let v = std::slice::from_raw_parts(ptr, len).to_vec();
            libc::free(ptr.cast());
            v
        };
        // SAFETY: test-controlled reader.
        unsafe {
            let r = hew_cbor_de_new(bytes.as_ptr(), bytes.len());
            hew_cbor_de_enter_map(r);
            hew_cbor_de_select_key(r, 1);
            let n = hew_cbor_de_i64(r);
            assert_eq!(n, 0, "type mismatch returns the zero value");
            assert_eq!(hew_cbor_de_failed(r), 1, "type mismatch must fail closed");
            hew_cbor_de_free(r);
        }
    }

    /// A required key that is absent fails closed.
    #[test]
    fn missing_required_key_fails_closed() {
        // Encode `{1: 7}`, then select absent key 2 → fail closed.
        // SAFETY: test-controlled handles.
        unsafe {
            let buf = hew_cbor_ser_new();
            hew_cbor_ser_begin_map(buf);
            hew_cbor_ser_key_u64(buf, 1);
            hew_cbor_ser_i64(buf, 7);
            hew_cbor_ser_end_map(buf);
            let mut len = 0usize;
            let ptr = hew_cbor_ser_finish(buf, &raw mut len);
            let bytes = std::slice::from_raw_parts(ptr, len).to_vec();
            libc::free(ptr.cast());

            let r = hew_cbor_de_new(bytes.as_ptr(), bytes.len());
            hew_cbor_de_enter_map(r);
            hew_cbor_de_select_key(r, 2);
            assert_eq!(
                hew_cbor_de_failed(r),
                1,
                "absent required key must fail closed"
            );
            hew_cbor_de_free(r);
        }
    }

    /// A list field round-trips through `{1: [10, 20, 30]}` (array under a key).
    #[test]
    fn round_trip_array_field() {
        // SAFETY: test-controlled handles.
        let bytes = unsafe {
            let buf = hew_cbor_ser_new();
            hew_cbor_ser_begin_map(buf);
            hew_cbor_ser_key_u64(buf, 1);
            hew_cbor_ser_begin_array(buf);
            hew_cbor_ser_i64(buf, 10);
            hew_cbor_ser_i64(buf, 20);
            hew_cbor_ser_i64(buf, 30);
            hew_cbor_ser_end_array(buf);
            hew_cbor_ser_end_map(buf);
            let mut len = 0usize;
            let ptr = hew_cbor_ser_finish(buf, &raw mut len);
            let v = std::slice::from_raw_parts(ptr, len).to_vec();
            libc::free(ptr.cast());
            v
        };
        // SAFETY: test-controlled reader.
        unsafe {
            let r = hew_cbor_de_new(bytes.as_ptr(), bytes.len());
            hew_cbor_de_enter_map(r);
            hew_cbor_de_select_key(r, 1);
            hew_cbor_de_enter_array(r);
            let mut got = Vec::new();
            while hew_cbor_de_array_next(r) == 1 {
                got.push(hew_cbor_de_i64(r));
            }
            hew_cbor_de_exit_array(r);
            hew_cbor_de_exit_map(r);
            assert_eq!(hew_cbor_de_failed(r), 0);
            assert_eq!(got, vec![10, 20, 30]);
            hew_cbor_de_free(r);
        }
    }

    /// An optional `None` rides as a present CBOR null distinguishable by
    /// `hew_cbor_de_is_null`, and a `bytes` field round-trips its payload.
    #[test]
    fn round_trip_null_and_bytes() {
        let payload = [0xde_u8, 0xad, 0xbe, 0xef];
        // SAFETY: test-controlled handles.
        let bytes = unsafe {
            let buf = hew_cbor_ser_new();
            hew_cbor_ser_begin_map(buf);
            hew_cbor_ser_key_u64(buf, 1);
            hew_cbor_ser_null(buf);
            hew_cbor_ser_key_u64(buf, 2);
            hew_cbor_ser_bytes(
                buf,
                payload.as_ptr(),
                0,
                u32::try_from(payload.len()).unwrap(),
            );
            hew_cbor_ser_end_map(buf);
            let mut len = 0usize;
            let ptr = hew_cbor_ser_finish(buf, &raw mut len);
            let v = std::slice::from_raw_parts(ptr, len).to_vec();
            libc::free(ptr.cast());
            v
        };
        // SAFETY: test-controlled reader.
        unsafe {
            let r = hew_cbor_de_new(bytes.as_ptr(), bytes.len());
            hew_cbor_de_enter_map(r);
            hew_cbor_de_select_key(r, 1);
            assert_eq!(hew_cbor_de_is_null(r), 1, "None rides as CBOR null");
            hew_cbor_de_skip(r);
            hew_cbor_de_select_key(r, 2);
            assert_eq!(hew_cbor_de_is_null(r), 0);
            let mut blen = 0u32;
            let bptr = hew_cbor_de_bytes(r, &raw mut blen);
            hew_cbor_de_exit_map(r);
            assert_eq!(hew_cbor_de_failed(r), 0);
            assert_eq!(blen, 4);
            let got = std::slice::from_raw_parts(bptr, blen as usize).to_vec();
            assert_eq!(got, payload);
            libc::free(bptr.cast());
            hew_cbor_de_free(r);
        }
    }

    /// Free a `malloc_cstring`'d test string via the runtime's string drop.
    unsafe fn hew_string_drop_for_test(s: *mut c_char) {
        // SAFETY: s came from malloc_cstring; hew_string_drop is its deallocator.
        unsafe { crate::string::hew_string_drop(s) };
    }

    /// The bytes the encoder emits are canonical CBOR a vanilla `ciborium`
    /// reader interops with: a wire struct `{ id: 42 @1 }` encodes to the exact
    /// 4-byte map `A1 01 18 2A` (`map(1){ 1: 42 }`), and re-parsing those bytes
    /// with `ciborium::de::from_reader` (not our reader) yields `{1: 42}`.
    #[test]
    fn encoded_bytes_are_canonical_ciborium() {
        // SAFETY: test-controlled handle.
        let bytes = unsafe {
            let buf = hew_cbor_ser_new();
            hew_cbor_ser_begin_map(buf);
            hew_cbor_ser_key_u64(buf, 1);
            hew_cbor_ser_i64(buf, 42);
            hew_cbor_ser_end_map(buf);
            let mut len = 0usize;
            let ptr = hew_cbor_ser_finish(buf, &raw mut len);
            let v = std::slice::from_raw_parts(ptr, len).to_vec();
            libc::free(ptr.cast());
            v
        };
        assert_eq!(bytes, vec![0xA1, 0x01, 0x18, 0x2A], "canonical CBOR map(1)");
        // Re-decode with a stock ciborium reader to confirm cross-impl interop.
        let value: Value =
            ciborium::de::from_reader(&mut std::io::Cursor::new(&bytes[..])).expect("valid CBOR");
        let Value::Map(entries) = value else {
            panic!("expected a CBOR map");
        };
        assert_eq!(entries.len(), 1);
        let (key, val) = &entries[0];
        assert_eq!(i128::from(key.as_integer().unwrap()), 1);
        assert_eq!(i128::from(val.as_integer().unwrap()), 42);
    }

    /// The q185 Qa payload-variant shape `{tag: [payload...]}` is canonical CBOR
    /// a stock `ciborium` reader interops with: `Circle(42)` (variant tag 1)
    /// encodes to `A1 01 81 18 2A` (`map(1){ 1: array(1)[42] }`).
    #[test]
    fn enum_payload_qa_shape_is_canonical_ciborium() {
        // SAFETY: test-controlled handle.
        let bytes = unsafe {
            let buf = hew_cbor_ser_new();
            hew_cbor_ser_begin_map(buf);
            hew_cbor_ser_key_u64(buf, 1);
            hew_cbor_ser_begin_array(buf);
            hew_cbor_ser_i64(buf, 42);
            hew_cbor_ser_end_array(buf);
            hew_cbor_ser_end_map(buf);
            let mut len = 0usize;
            let ptr = hew_cbor_ser_finish(buf, &raw mut len);
            let v = std::slice::from_raw_parts(ptr, len).to_vec();
            libc::free(ptr.cast());
            v
        };
        assert_eq!(
            bytes,
            vec![0xA1, 0x01, 0x81, 0x18, 0x2A],
            "canonical CBOR map(1){{ 1: [42] }}"
        );
        let value: Value =
            ciborium::de::from_reader(&mut std::io::Cursor::new(&bytes[..])).expect("valid CBOR");
        let Value::Map(entries) = value else {
            panic!("expected a CBOR map");
        };
        assert_eq!(entries.len(), 1, "Qa map-of-one");
        let (key, val) = &entries[0];
        assert_eq!(i128::from(key.as_integer().unwrap()), 1, "variant tag");
        let Value::Array(items) = val else {
            panic!("expected a payload array");
        };
        assert_eq!(items.len(), 1);
        assert_eq!(i128::from(items[0].as_integer().unwrap()), 42);
    }

    /// A unit variant rides as its bare integer tag and decodes via
    /// `hew_cbor_de_enum_begin` (which pushes a balancing empty payload frame so
    /// `hew_cbor_de_enum_end` is symmetric for unit and payload variants).
    #[test]
    fn enum_unit_decodes_via_enum_begin() {
        // SAFETY: test-controlled handles.
        unsafe {
            let buf = hew_cbor_ser_new();
            hew_cbor_ser_u64(buf, 2);
            let mut len = 0usize;
            let ptr = hew_cbor_ser_finish(buf, &raw mut len);
            let bytes = std::slice::from_raw_parts(ptr, len).to_vec();
            libc::free(ptr.cast());
            assert_eq!(bytes, vec![0x02], "bare unit tag");

            let r = hew_cbor_de_new(bytes.as_ptr(), bytes.len());
            assert_eq!(hew_cbor_de_enum_begin(r), 2, "unit tag");
            hew_cbor_de_enum_end(r);
            assert_eq!(hew_cbor_de_failed(r), 0, "unit variant decodes clean");
            hew_cbor_de_free(r);
        }
    }

    /// A payload variant `{1: [42]}` decodes via `enum_begin` (tag) +
    /// `array_next`/`de_i64` (payload) + `enum_end`.
    #[test]
    fn enum_payload_decodes_via_enum_begin() {
        // SAFETY: test-controlled handles.
        unsafe {
            let buf = hew_cbor_ser_new();
            hew_cbor_ser_begin_map(buf);
            hew_cbor_ser_key_u64(buf, 1);
            hew_cbor_ser_begin_array(buf);
            hew_cbor_ser_i64(buf, 42);
            hew_cbor_ser_end_array(buf);
            hew_cbor_ser_end_map(buf);
            let mut len = 0usize;
            let ptr = hew_cbor_ser_finish(buf, &raw mut len);
            let bytes = std::slice::from_raw_parts(ptr, len).to_vec();
            libc::free(ptr.cast());

            let r = hew_cbor_de_new(bytes.as_ptr(), bytes.len());
            assert_eq!(hew_cbor_de_enum_begin(r), 1, "payload tag");
            assert_eq!(hew_cbor_de_array_next(r), 1, "payload field present");
            assert_eq!(hew_cbor_de_i64(r), 42, "payload field value");
            hew_cbor_de_enum_end(r);
            assert_eq!(hew_cbor_de_failed(r), 0, "payload variant decodes clean");
            hew_cbor_de_free(r);
        }
    }

    /// `enum_begin` fails closed on a value that is neither a non-negative
    /// integer nor a single-entry `{int: array}` map (here: a bare text string),
    /// and still pushes a balancing frame so `enum_end` stays symmetric.
    #[test]
    fn enum_begin_fails_closed_on_malformed() {
        let text = std::ffi::CString::new("nope").unwrap();
        // SAFETY: test-controlled handles.
        unsafe {
            let buf = hew_cbor_ser_new();
            hew_cbor_ser_string(buf, text.as_ptr());
            let mut len = 0usize;
            let ptr = hew_cbor_ser_finish(buf, &raw mut len);
            let bytes = std::slice::from_raw_parts(ptr, len).to_vec();
            libc::free(ptr.cast());

            let r = hew_cbor_de_new(bytes.as_ptr(), bytes.len());
            assert_eq!(hew_cbor_de_enum_begin(r), 0, "malformed → 0");
            hew_cbor_de_enum_end(r);
            assert_eq!(hew_cbor_de_failed(r), 1, "malformed enum must fail closed");
            hew_cbor_de_free(r);
        }
    }

    /// `enum_begin` round-trips the full `u64` tag range, including tags above
    /// `i64::MAX` that a Hew integer literal cannot construct in a `.hew`
    /// fixture. Guards the `u64::try_from` decode path against silent wrap.
    #[test]
    fn enum_begin_high_bit_tag_round_trips() {
        // SAFETY: test-controlled handles.
        unsafe {
            let buf = hew_cbor_ser_new();
            hew_cbor_ser_u64(buf, u64::MAX);
            let mut len = 0usize;
            let ptr = hew_cbor_ser_finish(buf, &raw mut len);
            let bytes = std::slice::from_raw_parts(ptr, len).to_vec();
            libc::free(ptr.cast());

            let r = hew_cbor_de_new(bytes.as_ptr(), bytes.len());
            assert_eq!(
                hew_cbor_de_enum_begin(r),
                u64::MAX,
                "high-bit tag preserved"
            );
            hew_cbor_de_enum_end(r);
            assert_eq!(hew_cbor_de_failed(r), 0);
            hew_cbor_de_free(r);
        }
    }

    /// The scalar integer prims round-trip the true type extremes that a Hew
    /// literal cannot construct in a `.hew` fixture: `i64::MIN`/`i64::MAX` for
    /// the signed path (i8..i64 narrow to this in codegen) and `u64::MAX` for
    /// the unsigned path (u8..u64). The reachable mid-range values are covered
    /// by the compiled round-trip fixtures; these guard the codec at the
    /// boundaries those fixtures cannot express.
    #[test]
    fn scalar_int_extremes_round_trip() {
        // SAFETY: test-controlled handles; each prim wraps ciborium directly.
        unsafe {
            for signed in [i64::MIN, -1, 0, 1, i64::MAX] {
                let buf = hew_cbor_ser_new();
                hew_cbor_ser_i64(buf, signed);
                let mut len = 0usize;
                let ptr = hew_cbor_ser_finish(buf, &raw mut len);
                let bytes = std::slice::from_raw_parts(ptr, len).to_vec();
                libc::free(ptr.cast());

                let r = hew_cbor_de_new(bytes.as_ptr(), bytes.len());
                let got = hew_cbor_de_i64(r);
                assert_eq!(hew_cbor_de_failed(r), 0, "signed {signed} decode clean");
                assert_eq!(got, signed, "signed extreme {signed} round-trips");
                hew_cbor_de_free(r);
            }
            for unsigned in [0u64, 1, u64::from(u32::MAX), u64::MAX] {
                let buf = hew_cbor_ser_new();
                hew_cbor_ser_u64(buf, unsigned);
                let mut len = 0usize;
                let ptr = hew_cbor_ser_finish(buf, &raw mut len);
                let bytes = std::slice::from_raw_parts(ptr, len).to_vec();
                libc::free(ptr.cast());

                let r = hew_cbor_de_new(bytes.as_ptr(), bytes.len());
                let got = hew_cbor_de_u64(r);
                assert_eq!(hew_cbor_de_failed(r), 0, "unsigned {unsigned} decode clean");
                assert_eq!(got, unsigned, "unsigned extreme {unsigned} round-trips");
                hew_cbor_de_free(r);
            }
        }
    }
}
