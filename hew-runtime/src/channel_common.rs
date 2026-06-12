use std::ffi::c_char;
use std::ptr;

use std::ffi::c_void;

use hew_cabi::vec::{HewTypeOwnershipKind, HewVecElemLayout};

pub(crate) fn bytes_to_cstr(item: &[u8]) -> *mut c_char {
    let len = item.len();
    // Header-aware (S1): backs channel<string> recv; released via hew_string_drop / free_cstring.
    let buf = crate::cabi::alloc_cstring_data(len + 1); // CSTRING-ALLOC: str-open (bytes_to_cstr — header-aware String backing channel<string> recv; reaches hew_string_drop)
    if buf.is_null() {
        return ptr::null_mut();
    }
    if len > 0 {
        // SAFETY: `buf` has `len + 1` bytes and `item` has `len` readable bytes.
        unsafe { ptr::copy_nonoverlapping(item.as_ptr(), buf.cast::<u8>(), len) };
    }
    // SAFETY: writing the NUL terminator at offset `len` stays in-bounds.
    unsafe { *buf.cast::<u8>().add(len) = 0 };
    buf.cast::<c_char>()
}

pub(crate) unsafe fn free_channel_pair<P, S, R>(
    pair: *mut P,
    split: impl FnOnce(&mut P) -> (&mut *mut S, &mut *mut R),
) {
    if pair.is_null() {
        return;
    }

    // SAFETY: caller guarantees `pair` came from Box::into_raw.
    let mut pair = unsafe { Box::from_raw(pair) };
    let (sender, receiver) = split(&mut pair);
    // SAFETY: `sender` points into the boxed pair and is valid for replacement.
    let sender = unsafe { ptr::replace(sender, ptr::null_mut()) };
    // SAFETY: `receiver` points into the boxed pair and is valid for replacement.
    let receiver = unsafe { ptr::replace(receiver, ptr::null_mut()) };
    drop(pair);

    if !sender.is_null() {
        // SAFETY: unextracted sender handles are still Box-owned here.
        unsafe { drop(Box::from_raw(sender)) };
    }
    if !receiver.is_null() {
        // SAFETY: unextracted receiver handles are still Box-owned here.
        unsafe { drop(Box::from_raw(receiver)) };
    }
}

// ---------------------------------------------------------------------------
// Element-layout witness (generic Stream<T> / Sender<T> / Receiver<T> width)
// ---------------------------------------------------------------------------
//
// The channel and stream queue cores carry opaque `Vec<u8>` envelopes; the
// `*_layout` runtime entries use a `HewVecElemLayout` witness (the same
// descriptor W5.016 production-proved for `Vec<owned-T>`) to decide how a
// typed element is serialised into and decoded out of that envelope:
//
// | ownership_kind  | envelope contents                       | thunks      |
// |-----------------|-----------------------------------------|-------------|
// | `Plain`         | the element's raw bytes (`size` wide)   | none        |
// | `String`        | the string's CONTENT bytes (no NUL)     | none        |
// | `Bytes`         | the bytes value's content bytes         | none        |
// | `LayoutManaged` | the element representation (`size` wide)| clone + drop|
//
// String and bytes elements stay content-encoded — that is what every
// platform stream backing (TCP, file, lines/chunks adapters) produces
// natively, and it keeps the queue's deep-copy handoff (no buffer shared
// between the producer and consumer threads). Only `LayoutManaged` elements
// (heap-owning records/enums) carry live ownership inside the envelope:
// send deep-copies the element in via `clone_fn`; recv moves the envelope
// out to the consumer (no clone, no drop); an envelope discarded without a
// consumer (queue drop, post-close send) is released via `drop_fn` exactly
// once by `ChannelCore`.

/// Abort fail-closed on a malformed element witness or a broken envelope
/// invariant. A wrong witness on an owned element is a silent UAF/leak
/// channel — never proceed past it (`boundary-fail-closed` P0).
pub(crate) fn abort_elem_witness(context: &str, reason: &str) -> ! {
    eprintln!("PANIC: {context}: {reason}");
    std::process::abort();
}

/// Validate a caller-supplied element layout witness, aborting fail-closed on
/// a malformed descriptor (mirrors `hew_vec_new_with_elem_layout`).
///
/// # Safety
///
/// `layout`, when non-null, must point to a `HewVecElemLayout` that lives for
/// the duration of the caller's operation (in practice a codegen static).
pub(crate) unsafe fn elem_layout_witness<'a>(
    layout: *const HewVecElemLayout,
    context: &str,
) -> &'a HewVecElemLayout {
    if layout.is_null() {
        abort_elem_witness(context, "element layout witness must be non-null");
    }
    // SAFETY: null was rejected above; caller guarantees the pointee lives.
    let l = unsafe { &*layout };
    if l.size == 0 {
        abort_elem_witness(context, "element layout size must be non-zero");
    }
    if l.align == 0 || !l.align.is_power_of_two() {
        abort_elem_witness(
            context,
            "element layout align must be a non-zero power of two",
        );
    }
    if l.ownership_kind == HewTypeOwnershipKind::LayoutManaged
        && (l.clone_fn.is_none() || l.drop_fn.is_none())
    {
        abort_elem_witness(
            context,
            "layout-managed element witness is missing a clone or drop thunk",
        );
    }
    l
}

/// Serialise one typed element into a queue envelope (the send side).
///
/// `LayoutManaged` elements are deep-copied in (memcpy + `clone_fn`), so the
/// caller keeps its value and the envelope owns an independent copy.
///
/// # Safety
///
/// `data` must point to one live element of the witness's type: `size`
/// readable bytes for `Plain`/`LayoutManaged`, a `*const c_char` slot for
/// `String`, a `BytesTriple` slot for `Bytes`.
pub(crate) unsafe fn encode_elem_envelope(
    data: *const c_void,
    layout: &HewVecElemLayout,
    context: &str,
) -> Vec<u8> {
    match layout.ownership_kind {
        HewTypeOwnershipKind::Plain => {
            // SAFETY: caller guarantees `size` readable bytes at `data`.
            unsafe { std::slice::from_raw_parts(data.cast::<u8>(), layout.size) }.to_vec()
        }
        HewTypeOwnershipKind::String => {
            // SAFETY: caller guarantees `data` is a string slot.
            let sptr = unsafe { *data.cast::<*const c_char>() };
            if sptr.is_null() {
                Vec::new()
            } else {
                // SAFETY: a non-null Hew string is a valid NUL-terminated buffer.
                unsafe { std::ffi::CStr::from_ptr(sptr) }
                    .to_bytes()
                    .to_vec()
            }
        }
        HewTypeOwnershipKind::Bytes => {
            // SAFETY: caller guarantees `data` is a BytesTriple slot.
            let t = unsafe { &*data.cast::<crate::bytes::BytesTriple>() };
            if t.ptr.is_null() || t.len == 0 {
                Vec::new()
            } else {
                // SAFETY: `t.ptr + t.offset` is valid for `t.len` bytes per the
                // BytesTriple contract; read-only borrow.
                unsafe { std::slice::from_raw_parts(t.ptr.add(t.offset as usize), t.len as usize) }
                    .to_vec()
            }
        }
        HewTypeOwnershipKind::LayoutManaged => {
            let mut env = vec![0u8; layout.size];
            // Memcpy first so BitCopy fields / enum tag bytes are correct, then
            // run the clone thunk to deep-copy the owned heap (the W5.016
            // clone-fn contract — the thunk REQUIRES `dst <- src` memcpy'd).
            // SAFETY: caller guarantees `size` readable bytes at `data`; `env`
            // was just allocated with `size` writable bytes.
            unsafe {
                ptr::copy_nonoverlapping(data.cast::<u8>(), env.as_mut_ptr(), layout.size);
            }
            let Some(clone_fn) = layout.clone_fn else {
                // Unreachable past elem_layout_witness; keep the abort so a
                // direct caller cannot silently bit-copy owned heap.
                abort_elem_witness(
                    context,
                    "layout-managed element witness lost its clone thunk",
                );
            };
            // SAFETY: `data` is the live source element; `env` holds a byte
            // copy of it (precondition of the clone thunk).
            let status = unsafe { clone_fn(data, env.as_mut_ptr().cast::<c_void>()) };
            if status != 0 {
                // The thunk rolled back its partial work; fail closed rather
                // than enqueue a half-cloned element.
                abort_elem_witness(context, "element clone thunk failed");
            }
            env
        }
    }
}

/// Decode one queue envelope into the consumer's out slot (the recv side).
///
/// Returns 1 when a value was written to `out`, 0 when no value is available
/// (`item` was `None`, or the documented bytes empty-item narrowing applied).
/// Ownership of a decoded `LayoutManaged` element MOVES to the consumer: no
/// clone runs, no drop runs, and the envelope bytes are dead afterwards.
///
/// # Safety
///
/// `out` must point to one writable element slot of the witness's type (see
/// [`encode_elem_envelope`] for the per-kind slot shapes).
pub(crate) unsafe fn decode_elem_envelope(
    item: Option<Vec<u8>>,
    out: *mut c_void,
    layout: &HewVecElemLayout,
    context: &str,
) -> i32 {
    let Some(item) = item else {
        return 0;
    };
    match layout.ownership_kind {
        HewTypeOwnershipKind::String => {
            // Empty contents are a valid `Some("")` — only `None` maps to 0.
            // CSTRING-ALLOC: str-open (header-aware String element decode;
            // reaches hew_string_drop on the Hew side).
            let s = bytes_to_cstr(&item);
            // SAFETY: caller guarantees `out` is a string slot.
            unsafe { *out.cast::<*mut c_char>() = s };
            1
        }
        HewTypeOwnershipKind::Bytes => {
            if item.is_empty() {
                // Documented bytes narrowing: a present zero-length item is
                // indistinguishable from EOF (matches `hew_stream_next_bytes`).
                return 0;
            }
            #[expect(
                clippy::cast_possible_truncation,
                reason = "stream item lengths carry the u32 bytes-ABI width"
            )]
            let len = item.len() as u32;
            // SAFETY: item is valid for len bytes; from_static copies it into a
            // fresh refcount-1 buffer the consumer owns.
            let triple = unsafe { crate::bytes::hew_bytes_from_static(item.as_ptr(), len) };
            // SAFETY: caller guarantees `out` is a BytesTriple slot.
            unsafe { *out.cast::<crate::bytes::BytesTriple>() = triple };
            1
        }
        HewTypeOwnershipKind::Plain => {
            if item.len() != layout.size {
                // A Plain envelope of the wrong width is a malformed message
                // (e.g. a text payload received through an integer witness).
                // No ownership is at stake — report and bind "no value",
                // matching the legacy `decode_i64_payload` discipline.
                crate::set_last_error(format!(
                    "{context}: expected {}-byte element payload, got {} bytes",
                    layout.size,
                    item.len()
                ));
                return 0;
            }
            // SAFETY: lengths checked above; caller guarantees `size` writable
            // bytes at `out`.
            unsafe { ptr::copy_nonoverlapping(item.as_ptr(), out.cast::<u8>(), layout.size) };
            1
        }
        HewTypeOwnershipKind::LayoutManaged => {
            if item.len() != layout.size {
                // An owned envelope that does not match its witness means two
                // different witnesses were used on one queue — the ownership
                // state of these bytes is unknowable. Abort fail-closed; a
                // graceful return would leak the envelope's owned heap and
                // hand the consumer garbage.
                abort_elem_witness(
                    context,
                    "owned envelope size does not match the element witness",
                );
            }
            // Move out: byte-copy to `out`; the consumer is the new owner (no
            // clone, no drop — mirrors `hew_vec_pop_owned`).
            // SAFETY: lengths checked above; caller guarantees `size` writable
            // bytes at `out`.
            unsafe { ptr::copy_nonoverlapping(item.as_ptr(), out.cast::<u8>(), layout.size) };
            1
        }
    }
}
