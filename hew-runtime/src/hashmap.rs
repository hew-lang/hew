//! Hew runtime: `hashmap` module.
//!
//! Layout-backed open-addressing hash map (`HewLayoutHashMap`) with C ABI.
//! Stores opaque key/value blobs whose identity is delegated to
//! caller-supplied hash and equality thunks; uses linear probing with
//! tombstones.
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]
#![expect(
    clippy::cast_possible_wrap,
    reason = "hew_hashmap_len_layout casts usize→i64; workspace caps len well below i64::MAX so the cast is lossless"
)]

use core::ffi::c_void;
use core::ptr;

use crate::release_walker::{self, ReleaseItem};
use hew_cabi::map::{HewMapKeyEqThunk, HewMapKeyHashThunk, HewMapKeyLayout, HewValueLayout};
use hew_cabi::vec::{HewTypeOwnershipKind, HewVec};

/// Visit occupied keys and values using the map's exact value descriptors.
/// # Safety
/// The map remains exclusively borrowed until all selected children drain.
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_visit_close(map: *mut HewLayoutHashMap, context: *mut c_void) {
    // SAFETY: the caller retains every occupied entry and its descriptor.
    unsafe {
        let map = &*map;
        for index in 0..map.cap {
            if *slot_state(map.entries, index, map.stride) == OCCUPIED {
                let entry = map.entries.add(index * map.stride);
                if let Some(visit) = map.key_layout.value.visit_close {
                    visit(entry.add(map.key_offset).cast(), context);
                }
                if let Some(visit) = map.val_layout.visit_close {
                    visit(entry.add(map.val_offset).cast(), context);
                }
            }
        }
    }
}

#[cfg(test)]
#[path = "hashmap_zero_sized_tests.rs"]
mod zero_sized_tests;

/// Invoke a borrowed hash callback without reading its result on failure.
unsafe fn key_hash(
    hash: HewMapKeyHashThunk,
    key: *const c_void,
    fault_out: *mut *mut c_void,
) -> Result<u64, i32> {
    let mut out = core::mem::MaybeUninit::uninit();
    // SAFETY: Caller supplies a writable fault output with no live fault owner.
    unsafe { fault_out.write(ptr::null_mut()) };
    // SAFETY: Key matches the descriptor; both callback outputs are writable.
    let status = unsafe { hash(key, out.as_mut_ptr(), fault_out) };
    if status != 0 {
        return Err(status);
    }
    // SAFETY: status zero initializes the callback's scalar result.
    Ok(unsafe { out.assume_init() })
}

/// Invoke borrowed equality without reading its result on failure.
unsafe fn key_eq(
    eq: HewMapKeyEqThunk,
    lhs: *const c_void,
    rhs: *const c_void,
    fault_out: *mut *mut c_void,
) -> Result<bool, i32> {
    let mut out = core::mem::MaybeUninit::uninit();
    // SAFETY: Caller supplies a writable fault output with no live fault owner.
    unsafe { fault_out.write(ptr::null_mut()) };
    // SAFETY: Both keys match the descriptor; callback outputs are writable.
    let status = unsafe { eq(lhs, rhs, out.as_mut_ptr(), fault_out) };
    if status != 0 {
        return Err(status);
    }
    // SAFETY: status zero initializes the callback's scalar result.
    Ok(unsafe { out.assume_init() })
}

/// Publish a successful scalar result after all fallible work completes.
unsafe fn complete<T>(out: *mut T, value: T, fault_out: *mut *mut c_void) -> i32 {
    // SAFETY: Caller supplies disjoint writable scalar and fault output slots.
    unsafe {
        out.write(value);
        fault_out.write(ptr::null_mut());
    }
    0
}

/// Entry states.
const EMPTY: u8 = 0;
const OCCUPIED: u8 = 1;
const TOMBSTONE: u8 = 2;

/// Load factor percentage threshold for resize (shared by the layout family).
const LOAD_PCTG: usize = 75;

// Each occupied slot stores a state byte followed by aligned key and value
// blobs. Key identity callbacks borrow those blobs. Both fields carry the same
// semantic copy/drop protocol used by descriptor-backed vectors.

/// Initial capacity for a layout-backed map (must be a power of two).
const LAYOUT_INIT_CAP: usize = 16;

/// Test-only linker anchor (W4.001 Stage C0b).
///
/// Returns the layout-init capacity as a `u64`. The body is trivial; the
/// real purpose is to give external dev-dependency users (e.g.
/// `hew-types/tests/resolved_call_kernel_symbols`) a safe Rust function
/// to reference so the linker pulls `hew_runtime`'s archive into the
/// test binary — which folds in the `#[no_mangle]` layout descriptor
/// statics that the test then resolves via `dlsym`.
///
/// `#[inline(never)]` prevents constant-fold-through-LTO from elision.
#[inline(never)]
#[doc(hidden)]
#[must_use]
pub fn layout_init_capacity_for_link_anchor() -> u64 {
    LAYOUT_INIT_CAP as u64
}

/// Layout-backed open-addressing hash map with variable-stride slots.
///
/// The struct shape is fixed by the C ABI; field order and types are part of
/// the contract that C-3 codegen synthesizes against.
///
/// # Concurrency
///
/// This type contains **no internal synchronisation**. Every operation
/// (`insert_layout` / `get_layout` / `contains_key_layout` / `remove_layout`
/// / `len_layout` / `free_layout`) requires the caller to guarantee exclusive
/// access for the duration of the call. Borrowed value pointers returned by
/// `get_layout` are invalidated by any subsequent mutating call (insert /
/// remove / free) — concurrent access from another thread, or retention of
/// such pointers across a free, is undefined behaviour. See
/// [`hew_hashmap_free_layout`] for the precise free-time contract.
#[repr(C)]
#[derive(Debug)]
pub struct HewLayoutHashMap {
    /// Pointer to the raw byte storage holding `cap * stride` bytes.
    pub entries: *mut u8,
    /// Number of `OCCUPIED` slots.
    pub len: usize,
    /// Total slot count; always a power of two.
    pub cap: usize,
    /// Byte offset of the key blob inside a slot (after the `state` byte and
    /// any padding required to reach `key_layout.value.align`).
    pub key_offset: usize,
    /// Byte offset of the value blob inside a slot.
    pub val_offset: usize,
    /// Total slot size in bytes (already padded to `max(key_align, val_align)`).
    pub stride: usize,
    /// Key descriptor — **owned by-value snapshot** taken at constructor entry
    /// (W4.001 Stage C0a; plan rev6 §4 Blocker B2). Snapshotting decouples the
    /// map's hash/eq/drop discipline from any post-construction mutation the
    /// caller may make to their original descriptor: the constructor's
    /// fail-closed consistency check is therefore a lifetime invariant for the
    /// map, not just a one-shot at-entry check.
    pub key_layout: HewMapKeyLayout,
    /// Value descriptor — owned by-value snapshot (same rationale).
    pub val_layout: HewValueLayout,
}

/// Opaque cursor for borrowing occupied entries from a layout-backed map.
///
/// The cursor owns no collection data. The map must remain alive and must not
/// be mutated until the cursor is freed.
#[repr(C)]
#[derive(Debug)]
pub struct HewLayoutHashMapIter {
    map: *const HewLayoutHashMap,
    next_slot: usize,
}

mod probe;
pub use probe::*;

/// Create an iterator over the occupied entries in `m`.
///
/// # Safety
///
/// `m` must point to a live map and remain live and unmodified until the
/// returned iterator is freed.
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_iter_new_layout(
    m: *const HewLayoutHashMap,
) -> *mut HewLayoutHashMapIter {
    // SAFETY: m is a live map per this fn's contract.
    unsafe { validate_op_map(m) };
    Box::into_raw(Box::new(HewLayoutHashMapIter {
        map: m,
        next_slot: 0,
    }))
}

/// Advance an iterator, returning borrowed pointers to its next key and value.
///
/// # Safety
///
/// `iter`, `out_key`, and `out_value` must be non-null. The iterator's map
/// must still be live and unmodified. Returned pointers remain valid only
/// while that condition holds.
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_iter_next_layout(
    iter: *mut HewLayoutHashMapIter,
    out_key: *mut *const c_void,
    out_value: *mut *const c_void,
) -> bool {
    if iter.is_null() || out_key.is_null() || out_value.is_null() {
        crate::set_last_error("HewLayoutHashMap iterator: null argument");
        std::process::abort();
    }
    // SAFETY: all three pointers were checked above; the caller guarantees the
    // iterator and its source map remain live and unmodified.
    let cursor = unsafe { &mut *iter };
    // SAFETY: cursor.map is the live source map captured by the constructor.
    unsafe { validate_op_map(cursor.map) };
    // SAFETY: validate_op_map just established that cursor.map is live.
    let map = unsafe { &*cursor.map };
    while cursor.next_slot < map.cap {
        let index = cursor.next_slot;
        cursor.next_slot += 1;
        // SAFETY: index is below map.cap and validate_op_map established the
        // backing allocation and layout invariants.
        if unsafe { *slot_state(map.entries, index, map.stride) } != OCCUPIED {
            continue;
        }
        // SAFETY: this is an occupied in-bounds slot. The returned pointers
        // borrow its initialized key/value storage for the iterator lifetime.
        unsafe {
            *out_key = slot_key(map.entries, index, map.stride, map.key_offset)
                .cast_const()
                .cast();
            *out_value = slot_val(map.entries, index, map.stride, map.val_offset)
                .cast_const()
                .cast();
        }
        return true;
    }
    false
}

/// Free a map iterator. A null pointer is a no-op.
///
/// # Safety
///
/// `iter` must be null or a pointer returned by
/// [`hew_hashmap_iter_new_layout`] that has not already been freed.
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_iter_free_layout(iter: *mut HewLayoutHashMapIter) {
    if !iter.is_null() {
        // SAFETY: iter has unique ownership from Box::into_raw per the contract.
        drop(unsafe { Box::from_raw(iter) });
    }
}

/// Round `offset` up to the next multiple of `align`. `align` must be a
/// non-zero power of two. Returns `None` on overflow.
#[inline]
fn align_up(offset: usize, align: usize) -> Option<usize> {
    debug_assert!(
        align.is_power_of_two(),
        "align_up requires power-of-two alignment"
    );
    let mask = align - 1;
    offset.checked_add(mask).map(|sum| sum & !mask)
}

/// Compute `(key_offset, val_offset, stride, entries_align)` for a given key
/// and value layout. Returns `None` on any overflow.
fn compute_slot_layout(
    key_size: usize,
    key_align: usize,
    val_size: usize,
    val_align: usize,
) -> Option<(usize, usize, usize, usize)> {
    // State byte sits at offset 0; key blob follows after padding to key_align.
    let key_offset = align_up(1, key_align)?;
    let after_key = key_offset.checked_add(key_size)?;
    let val_offset = align_up(after_key, val_align)?;
    let after_val = val_offset.checked_add(val_size)?;
    let entries_align = core::cmp::max(key_align, val_align);
    let stride = align_up(after_val, entries_align)?;
    // Guard total table size against isize::MAX/4 per council Rev 1.
    // Caller will multiply stride * cap; pre-check stride alone now and recheck
    // after multiplying by capacity at allocation time.
    if stride > (isize::MAX as usize) / 4 {
        return None;
    }
    Some((key_offset, val_offset, stride, entries_align))
}

/// Allocate the entries byte array for a layout map with `cap` slots of size
/// `stride`, zero-initialised. Aborts on overflow or allocation failure.
///
/// # Safety
///
/// `stride > 0`, `cap > 0`, `entries_align` is a power of two.
unsafe fn alloc_layout_entries(cap: usize, stride: usize, entries_align: usize) -> *mut u8 {
    let total = match cap.checked_mul(stride) {
        Some(n) if n <= (isize::MAX as usize) / 4 => n,
        Some(_) | None => {
            crate::set_last_error("HewLayoutHashMap: entries allocation size overflow");
            std::process::abort();
        }
    };
    let Ok(layout) = std::alloc::Layout::from_size_align(total, entries_align) else {
        crate::set_last_error("HewLayoutHashMap: invalid entries Layout");
        std::process::abort();
    };
    // SAFETY: layout has non-zero size (stride > 0, cap > 0).
    let ptr = unsafe { std::alloc::alloc_zeroed(layout) };
    if ptr.is_null() {
        std::alloc::handle_alloc_error(layout);
    }
    ptr
}

/// Free the entries byte array allocated by `alloc_layout_entries`.
///
/// # Safety
///
/// `entries` must have been returned by `alloc_layout_entries` with the same
/// `cap`, `stride`, and `entries_align`.
unsafe fn dealloc_layout_entries(
    entries: *mut u8,
    cap: usize,
    stride: usize,
    entries_align: usize,
) {
    if entries.is_null() || cap == 0 || stride == 0 {
        return;
    }
    let total = cap.saturating_mul(stride);
    if let Ok(layout) = std::alloc::Layout::from_size_align(total, entries_align) {
        // SAFETY: pointer came from alloc_zeroed with this exact layout.
        unsafe { std::alloc::dealloc(entries, layout) };
    }
}

/// Slot view helpers — pure pointer arithmetic; never reads through the layout
/// pointers, so safe to call during resize while layout fields are mid-update.
#[inline]
unsafe fn slot_state(entries: *mut u8, idx: usize, stride: usize) -> *mut u8 {
    // SAFETY: caller guarantees idx < cap and stride matches allocation.
    unsafe { entries.add(idx * stride) }
}

#[inline]
unsafe fn slot_key(entries: *mut u8, idx: usize, stride: usize, key_offset: usize) -> *mut u8 {
    // SAFETY: caller guarantees idx < cap and offsets match allocation.
    unsafe { entries.add(idx * stride + key_offset) }
}

#[inline]
unsafe fn slot_val(entries: *mut u8, idx: usize, stride: usize, val_offset: usize) -> *mut u8 {
    // SAFETY: caller guarantees idx < cap and offsets match allocation.
    unsafe { entries.add(idx * stride + val_offset) }
}

/// Validate a `HewMapKeyLayout` at constructor time. Aborts fail-closed on any
/// violation (LESSONS `boundary-fail-closed` P0).
///
/// Exposed `pub` so `should_panic` tests can target the validator directly:
/// panics cannot unwind across the `extern "C"` boundary under
/// `panic = "abort"`, so test coverage of the abort gates lives at this
/// layer.
///
/// # Panics
///
/// Panics if `key_layout` is null or `align`
/// is not a power of two. Zero-sized keys retain their logical descriptor:
/// the state byte provides nonzero slot stride, and callbacks receive aligned,
/// non-null key addresses even when no payload bytes exist.
///
/// **W4.001 Stage C0a:** `LayoutManaged` ownership is no longer rejected
/// here. The new descriptor-consistency check in `new_layout` rejects
/// `String`/`LayoutManaged` ownership combined with `drop_fn == None`
/// instead — fail-closed but more precise.
///
/// # Safety
///
/// `key_layout` must be non-null and point to a valid `HewMapKeyLayout`.
pub unsafe fn validate_key_layout(key_layout: *const HewMapKeyLayout) {
    if key_layout.is_null() {
        crate::set_last_error("HewLayoutHashMap: key_layout is null");
        panic!("HewLayoutHashMap: key_layout is null");
    }
    // SAFETY: caller guarantees non-null + valid.
    let kl = unsafe { &*key_layout };
    if !kl.value.align.is_power_of_two() {
        crate::set_last_error("HewLayoutHashMap: key_layout.value.align is not a power of two");
        panic!("HewLayoutHashMap: key_layout.value.align is not a power of two");
    }
    // C0a: LayoutManaged / String ownership are admitted; their drop_fn
    // requirement is enforced by validate_descriptor_ownership (called from
    // the constructor).
}

/// Validate a `HewValueLayout` at constructor time.
///
/// Exposed `pub` for the same reason as [`validate_key_layout`].
///
/// # Panics
///
/// Panics if `val_layout` is null, `align` is not a power of two, or a
/// zero-size value layout has `align != 1`.
///
/// **W4.001 Stage C0a:** `LayoutManaged` ownership is no longer rejected
/// here; see [`validate_key_layout`].
///
/// # Safety
///
/// `val_layout` must be non-null and point to a valid `HewValueLayout`.
pub unsafe fn validate_val_layout(val_layout: *const HewValueLayout) {
    if val_layout.is_null() {
        crate::set_last_error("HewLayoutHashMap: val_layout is null");
        panic!("HewLayoutHashMap: val_layout is null");
    }
    // SAFETY: caller guarantees non-null + valid.
    let vl = unsafe { &*val_layout };
    if !vl.align.is_power_of_two() {
        crate::set_last_error("HewLayoutHashMap: val_layout.align is not a power of two");
        panic!("HewLayoutHashMap: val_layout.align is not a power of two");
    }
    if vl.size == 0 && vl.align != 1 {
        crate::set_last_error(
            "HewLayoutHashMap: zero-size value layout must have align == 1 (HashSet ZST contract)",
        );
        panic!("HewLayoutHashMap: zero-size value layout must have align == 1");
    }
    // C0a: see validate_key_layout for the LayoutManaged-relaxation rationale.
}

/// Fail-closed descriptor-consistency check (W4.001 Stage C0a; plan rev6
/// §4 "Fail-closed descriptor consistency check"; CLAUDE.md §2).
///
/// Rejects descriptors that declare owned-ownership semantics without a
/// matching drop thunk. The four enumerated rejection cases are asserted
/// by `hashmap_layout_descriptor_rejection.rs`.
///
/// Allowed: `Plain` ownership with `drop_fn = Some(_)` (no-op cleanup
/// is harmless; the relaxed-direction does not violate fail-closed
/// because extra cleanup is safe and missing cleanup is the leak hazard).
///
/// # Panics
///
/// Panics with message
/// `"HewLayoutHashMap: {key_layout|val_layout} ownership_kind={String|Bytes|LayoutManaged} requires drop_fn"`
/// when an owning descriptor has no drop thunk.
///
/// # Safety
///
/// Both pointers must be non-null and point to valid descriptors (call
/// `validate_key_layout` / `validate_val_layout` first).
pub unsafe fn validate_descriptor_ownership(
    key_layout: *const HewMapKeyLayout,
    val_layout: *const HewValueLayout,
) {
    // SAFETY: caller-guaranteed non-null + valid.
    let kl = unsafe { &*key_layout };
    // SAFETY: same.
    let vl = unsafe { &*val_layout };
    match kl.value.ownership_kind {
        HewTypeOwnershipKind::Plain => {}
        HewTypeOwnershipKind::String => {
            if kl.value.drop_fn.is_none() {
                crate::set_last_error(
                    "HewLayoutHashMap: key_layout ownership_kind=String requires drop_fn",
                );
                panic!("HewLayoutHashMap: key_layout ownership_kind=String requires drop_fn");
            }
        }
        HewTypeOwnershipKind::LayoutManaged => {
            if kl.value.drop_fn.is_none() {
                crate::set_last_error(
                    "HewLayoutHashMap: key_layout ownership_kind=LayoutManaged requires drop_fn",
                );
                panic!(
                    "HewLayoutHashMap: key_layout ownership_kind=LayoutManaged requires drop_fn"
                );
            }
        }
        HewTypeOwnershipKind::Bytes => {
            if kl.value.drop_fn.is_none() {
                crate::set_last_error(
                    "HewLayoutHashMap: key_layout ownership_kind=Bytes requires drop_fn",
                );
                panic!("HewLayoutHashMap: key_layout ownership_kind=Bytes requires drop_fn");
            }
        }
    }
    match vl.ownership_kind {
        HewTypeOwnershipKind::Plain => {}
        HewTypeOwnershipKind::String => {
            if vl.drop_fn.is_none() {
                crate::set_last_error(
                    "HewLayoutHashMap: val_layout ownership_kind=String requires drop_fn",
                );
                panic!("HewLayoutHashMap: val_layout ownership_kind=String requires drop_fn");
            }
        }
        HewTypeOwnershipKind::LayoutManaged => {
            if vl.drop_fn.is_none() {
                crate::set_last_error(
                    "HewLayoutHashMap: val_layout ownership_kind=LayoutManaged requires drop_fn",
                );
                panic!(
                    "HewLayoutHashMap: val_layout ownership_kind=LayoutManaged requires drop_fn"
                );
            }
        }
        HewTypeOwnershipKind::Bytes => {
            if vl.drop_fn.is_none() {
                crate::set_last_error(
                    "HewLayoutHashMap: val_layout ownership_kind=Bytes requires drop_fn",
                );
                panic!("HewLayoutHashMap: val_layout ownership_kind=Bytes requires drop_fn");
            }
        }
    }
}

/// Validate both layouts and return the slot geometry. Panics on overflow.
///
/// Exposed `pub` so `should_panic` tests can drive the full constructor-side
/// gate chain without crossing an `extern "C"` frame.
///
/// # Panics
///
/// Panics under any condition documented by [`validate_key_layout`] and
/// [`validate_val_layout`], plus on slot stride overflow.
///
/// # Safety
///
/// Both pointers must be non-null and point to valid descriptors.
#[must_use]
pub unsafe fn validate_and_compute_slot_layout(
    key_layout: *const HewMapKeyLayout,
    val_layout: *const HewValueLayout,
) -> (usize, usize, usize, usize) {
    // SAFETY: forwarded; the validator itself null-checks.
    unsafe { validate_key_layout(key_layout) };
    // SAFETY: same.
    unsafe { validate_val_layout(val_layout) };
    // SAFETY: both pointers validated non-null above; ownership check is
    // the W4.001 Stage C0a fail-closed descriptor-consistency gate.
    unsafe { validate_descriptor_ownership(key_layout, val_layout) };
    // SAFETY: validated non-null above.
    let kl = unsafe { &*key_layout };
    // SAFETY: validated non-null above.
    let vl = unsafe { &*val_layout };
    let Some(t) = compute_slot_layout(kl.value.size, kl.value.align, vl.size, vl.align) else {
        crate::set_last_error("HewLayoutHashMap: slot stride overflow");
        panic!("HewLayoutHashMap: slot stride overflow");
    };
    t
}

/// Fail-closed gate for operational entry points (`insert`/`get`/`contains`/
/// `remove`). Centralises the null checks so both the `extern "C"` boundary
/// and `should_panic` tests drive the same code path — under `panic = "abort"`
/// the panics on this path abort the process (matching the precedent set by
/// `hew_vtable_dispatch_panic_on_oob` in `trait_object.rs`), while under the
/// test profile they unwind so the negative gates can be observed.
///
/// `val` carries insert semantics: `Some(v)` means "the caller is performing
/// an insert with payload `v`"; the gate then consults `(*m).val_layout.size`
/// and rejects `v.is_null()` when `size > 0`. `None` means the op carries no
/// value payload (get / contains / remove).
///
/// # Panics
///
/// Panics if `m` is null, `key` is null, or `val` is `Some(null)` while the
/// registered value layout has non-zero size.
///
/// # Safety
///
/// When `val` is `Some(_)` and `m` is non-null, the function dereferences
/// `(*m).val_layout` to read its size; the layout pointer must remain valid
/// for the duration of the call (it is caller-stable per the ownership
/// contract documented on [`HewLayoutHashMap`]).
pub unsafe fn validate_op_inputs(
    m: *const HewLayoutHashMap,
    key: *const c_void,
    val: Option<*const c_void>,
) {
    if m.is_null() {
        crate::set_last_error("HewLayoutHashMap op: m is null");
        panic!("HewLayoutHashMap op: m is null");
    }
    if key.is_null() {
        crate::set_last_error("HewLayoutHashMap op: key is null");
        panic!("HewLayoutHashMap op: key is null");
    }
    if let Some(v) = val {
        // SAFETY: m non-null per check above; val_layout is now an owned
        // by-value field on the map (W4.001 Stage C0a snapshot), no
        // pointer chase required.
        let val_size = unsafe { (*m).val_layout.size };
        if val_size > 0 && v.is_null() {
            crate::set_last_error("HewLayoutHashMap op: val is null but value size > 0");
            panic!("HewLayoutHashMap op: val is null but value size > 0");
        }
    }
}

/// Fail-closed null check for the single-pointer ops (`len`). Exposed `pub`
/// so `should_panic` tests can drive the gate directly.
///
/// # Panics
///
/// Panics if `m` is null.
///
/// # Safety
///
/// Performs only a null check; safe to call with any `*const`.
pub unsafe fn validate_op_map(m: *const HewLayoutHashMap) {
    if m.is_null() {
        crate::set_last_error("HewLayoutHashMap op: m is null");
        panic!("HewLayoutHashMap op: m is null");
    }
}

// ---------------------------------------------------------------------------
// Constructor (layout-backed)
// ---------------------------------------------------------------------------

/// Create a new layout-backed `HewLayoutHashMap`.
///
/// Fail-closed gates (council Rev 2/3 + W4.001 Stage C0a): aborts on null
/// layout pointers, non-power-of-two
/// alignment, malformed ZST value layout, owned ownership without matching
/// `drop_fn` (`String`/`LayoutManaged` require `Some(_)`), or stride overflow.
///
/// **W4.001 Stage C0a — descriptor snapshot by value.** Caller-provided
/// descriptors are copied into the map at constructor entry (after all
/// fail-closed validation). Subsequent caller mutation of the original
/// descriptor bytes is harmless — the map honours its snapshot for the
/// remainder of its lifetime (plan rev6 §4 Blocker B2).
///
/// # Safety
///
/// `key_layout` and `val_layout` must point to valid descriptors **at
/// constructor entry**. After this call returns, the caller may free or
/// mutate the descriptor bytes; the map no longer reads through the
/// original pointers. The returned pointer must be freed with
/// [`hew_hashmap_free_layout`].
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_new_with_layout(
    key_layout: *const HewMapKeyLayout,
    val_layout: *const HewValueLayout,
) -> *mut HewLayoutHashMap {
    if key_layout.is_null() {
        crate::set_last_error("hew_hashmap_new_with_layout: key_layout is null");
        // SAFETY: extern "C" cannot unwind; abort the process. Tests cover the
        // null-layout case via the testable `validate_key_layout` helper.
        std::process::abort();
    }
    if val_layout.is_null() {
        crate::set_last_error("hew_hashmap_new_with_layout: val_layout is null");
        // SAFETY: same — abort across the C boundary.
        std::process::abort();
    }
    // SAFETY: non-null checked above; validator panics fail-closed on any
    // gate violation (including the C0a ownership/drop-fn consistency check).
    // Under `panic = "abort"` the panic aborts the process before unwinding
    // crosses this frame, preserving the C ABI contract.
    let (key_offset, val_offset, stride, entries_align) =
        unsafe { validate_and_compute_slot_layout(key_layout, val_layout) };

    // W4.001 Stage C0a: snapshot descriptors by value *after* validation.
    // SAFETY: validated non-null + valid by the call above; descriptors are
    // `#[repr(C)] Copy` PODs, so `read` is a fixed-size memcpy with no drop
    // glue. After this point the map no longer reads through the caller's
    // pointer.
    let key_layout_snapshot = unsafe { ptr::read(key_layout) };
    // SAFETY: same — see comment above.
    let val_layout_snapshot = unsafe { ptr::read(val_layout) };

    let cap = LAYOUT_INIT_CAP;
    // SAFETY: cap > 0, stride > 0, entries_align is power of two.
    let entries = unsafe { alloc_layout_entries(cap, stride, entries_align) };

    // Keep the handle on the same libc allocator family as Vec and HashSet.
    // SAFETY: malloc returns storage suitably aligned for HewLayoutHashMap.
    let raw: *mut HewLayoutHashMap =
        crate::mem::buf_try_alloc(core::mem::size_of::<HewLayoutHashMap>()).cast(); // ALLOCATOR-PAIRING: GlobalAlloc
    if raw.is_null() {
        // SAFETY: entries just allocated with these params.
        unsafe { dealloc_layout_entries(entries, cap, stride, entries_align) };
        std::process::abort();
    }
    // SAFETY: raw is a fresh allocation of HewLayoutHashMap size.
    unsafe {
        ptr::write(
            raw,
            HewLayoutHashMap {
                entries,
                len: 0,
                cap,
                key_offset,
                val_offset,
                stride,
                key_layout: key_layout_snapshot,
                val_layout: val_layout_snapshot,
            },
        );
    }
    raw
}

fn abort_layout_clone(reason: impl Into<String>) -> ! {
    crate::set_last_error(reason);
    std::process::abort();
}

fn require_clone(layout: &HewValueLayout, label: &str) {
    if layout.ownership_kind != HewTypeOwnershipKind::Plain && layout.clone_fn.is_none() {
        abort_layout_clone(format!("{label}: clone callback is unavailable"));
    }
}

/// Copy one value through the shared descriptor. The callback replaces owning
/// leaves after the complete representation is seeded, exactly as for vectors.
unsafe fn clone_layout_blob(layout: HewValueLayout, src: *const u8, dst: *mut u8, label: &str) {
    require_clone(&layout, label);
    if layout.size > 0 {
        // SAFETY: the caller supplies live, non-overlapping value slots of this layout.
        unsafe { ptr::copy_nonoverlapping(src, dst, layout.size) };
    }
    if let Some(clone) = layout.clone_fn {
        // SAFETY: the descriptor's callback accepts the two concrete value slots.
        let status = unsafe { clone(src.cast(), dst.cast()) };
        if status != 0 {
            abort_layout_clone(format!("{label}: clone callback returned {status}"));
        }
    }
}

/// Deep-clone a layout-backed map, duplicating owned slot blobs when the
/// key and value descriptors provide concrete clone callbacks. Both roles use
/// the same copy protocol as vector elements.
///
/// # Safety
///
/// `m` must have been returned by [`hew_hashmap_new_with_layout`] (or be null).
/// The returned pointer must eventually be freed with
/// [`hew_hashmap_free_layout`].
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_clone_layout(
    m: *const HewLayoutHashMap,
) -> *mut HewLayoutHashMap {
    if m.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: shared fail-closed gate (map-only variant).
    unsafe { validate_op_map(m) };

    // SAFETY: m non-null and constructed via hew_hashmap_new_with_layout.
    let src = unsafe { &*m };
    let entries_align = core::cmp::max(src.key_layout.value.align, src.val_layout.align);
    require_clone(&src.key_layout.value, "map key");
    require_clone(&src.val_layout, "map value");

    // SAFETY: source map was validated at construction time.
    let cloned_entries = unsafe { alloc_layout_entries(src.cap, src.stride, entries_align) };
    // SAFETY: malloc returns storage suitably aligned for HewLayoutHashMap.
    let cloned: *mut HewLayoutHashMap =
        crate::mem::buf_try_alloc(core::mem::size_of::<HewLayoutHashMap>()).cast(); // ALLOCATOR-PAIRING: GlobalAlloc
    if cloned.is_null() {
        // SAFETY: entries just allocated with these params.
        unsafe { dealloc_layout_entries(cloned_entries, src.cap, src.stride, entries_align) };
        std::process::abort();
    }
    // SAFETY: cloned is a fresh allocation of HewLayoutHashMap size.
    unsafe {
        ptr::write(
            cloned,
            HewLayoutHashMap {
                entries: cloned_entries,
                len: src.len,
                cap: src.cap,
                key_offset: src.key_offset,
                val_offset: src.val_offset,
                stride: src.stride,
                key_layout: src.key_layout,
                val_layout: src.val_layout,
            },
        );
    }

    for idx in 0..src.cap {
        // SAFETY: idx < cap, offsets/stride came from the source map.
        let src_state = unsafe { *slot_state(src.entries, idx, src.stride) };
        // SAFETY: idx < cap in the cloned allocation.
        let dst_state = unsafe { slot_state(cloned_entries, idx, src.stride) };
        // SAFETY: dst_state is in-bounds.
        unsafe { *dst_state = src_state };
        if src_state != OCCUPIED {
            continue;
        }
        // SAFETY: occupied slot has valid blobs at the stored offsets.
        let src_key = unsafe { slot_key(src.entries, idx, src.stride, src.key_offset) };
        // SAFETY: destination slot is in-bounds in the cloned allocation.
        let dst_key = unsafe { slot_key(cloned_entries, idx, src.stride, src.key_offset) };
        // SAFETY: forwarded blob contracts.
        unsafe {
            clone_layout_blob(src.key_layout.value, src_key, dst_key, "map key");
        }
        // SAFETY: occupied slot has a valid value blob.
        let src_val = unsafe { slot_val(src.entries, idx, src.stride, src.val_offset) };
        // SAFETY: destination slot is in-bounds in the cloned allocation.
        let dst_val = unsafe { slot_val(cloned_entries, idx, src.stride, src.val_offset) };
        // SAFETY: forwarded blob contracts.
        unsafe {
            clone_layout_blob(
                src.val_layout,
                src_val,
                dst_val,
                "hew_hashmap_clone_layout value",
            );
        }
    }

    cloned
}

// ---------------------------------------------------------------------------
// Insert / Get / Contains / Remove / Len (layout-backed)
// ---------------------------------------------------------------------------

/// Insert an independent copy of a borrowed key and value.
///
/// On success, writes true for a new key and false for replacement. Both
/// caller-owned inputs remain unchanged, including on callback failure. Inputs may borrow slots in this map:
/// the complete copies are staged before resizing or releasing an old value.
///
/// Status zero initializes the scalar output and clears `fault_out`. A callback
/// failure returns its exact nonzero status and fault owner, leaving all result
/// outputs untouched and retaining the receiver with the caller.
///
/// # Safety
///
/// Scalar and fault outputs must be non-null, aligned, writable and disjoint
/// from the receiver and input storage.
///
/// `m` must be a live map. `key` and `val` must borrow initialized slots matching
/// its descriptors. A zero-sized plain value may use a null `val`; a value
/// requiring a clone callback must supply a non-null slot even when zero-sized.
#[no_mangle]
pub unsafe extern "C-unwind" fn hew_hashmap_insert_clone_layout(
    m: *mut HewLayoutHashMap,
    key: *const c_void,
    val: *const c_void,
    present_out: *mut bool,
    fault_out: *mut *mut c_void,
) -> i32 {
    // SAFETY: the caller lends initialized operands matching the descriptors.
    unsafe { validate_op_inputs(m, key, Some(val)) };
    // SAFETY: all loans remain live while the shared probe drives C callbacks.
    let probe = match unsafe { probe::drive(m, key, true, fault_out) } {
        Ok(probe) => probe,
        Err(status) => return status,
    };
    // SAFETY: callbacks completed; commit follows this entry's copy/take contract.
    let inserted = unsafe { hew_hashmap_probe_insert_clone(probe, val) };
    // SAFETY: the caller supplies writable result/fault output slots.
    unsafe { complete(present_out, inserted, fault_out) }
}

/// Insert an independent copy of a borrowed key and take the caller's value.
///
/// This is the ingress for a value with no semantic clone: the caller's owner
/// moves into the slot the map keeps, while the key is cloned exactly as
/// [`hew_hashmap_insert_clone_layout`] does. On success, writes true for a new
/// key and false for replacement, and a replacement releases the value the map
/// held. A callback failure transfers nothing: the caller keeps its value.
///
/// Status zero initializes the scalar output and clears `fault_out`. A callback
/// failure returns its exact nonzero status and fault owner, leaving all result
/// outputs untouched and retaining the receiver with the caller.
///
/// # Safety
///
/// Scalar and fault outputs must be non-null, aligned, writable and disjoint
/// from the receiver and input storage.
///
/// `m` must be a live map. `key` must borrow an initialized slot matching its
/// descriptor. `val` must be an independent owner of a value blob matching the
/// value descriptor; the caller must not release it after a zero status.
#[no_mangle]
pub unsafe extern "C-unwind" fn hew_hashmap_insert_take_layout(
    m: *mut HewLayoutHashMap,
    key: *const c_void,
    val: *const c_void,
    present_out: *mut bool,
    fault_out: *mut *mut c_void,
) -> i32 {
    // SAFETY: the caller lends initialized operands matching the descriptors.
    unsafe { validate_op_inputs(m, key, Some(val)) };
    // SAFETY: all loans remain live while the shared probe drives C callbacks.
    let probe = match unsafe { probe::drive(m, key, true, fault_out) } {
        Ok(probe) => probe,
        Err(status) => return status,
    };
    // SAFETY: callbacks completed; commit follows this entry's copy/take contract.
    let inserted = unsafe { hew_hashmap_probe_insert_take(probe, val) };
    // SAFETY: the caller supplies writable result/fault output slots.
    unsafe { complete(present_out, inserted, fault_out) }
}

/// Insert or overwrite `key -> val`. On success, `present_out` is true for a
/// new entry and false for replacement.
///
/// `val` may be null only when the value layout's `size` is zero (`HashSet`
/// contract); otherwise null aborts fail-closed.
///
/// The value owner transfers on both successful paths. A new entry also takes the
/// key owner; replacement preserves the stored key and leaves the incoming
/// duplicate key with the caller. The Boolean output identifies which path ran.
/// On callback failure, the caller retains the receiver and both input owners.
/// Transfer-in inputs must be independent owners, not borrowed map slots; use
/// [`hew_hashmap_insert_clone_layout`] for borrowing insertion.
/// Cloning operations and eventual cleanup use the shared value descriptors.
///
/// Status zero initializes the scalar output and clears `fault_out`. A callback
/// failure returns its exact nonzero status and fault owner, leaving all result
/// outputs untouched and retaining the receiver with the caller.
///
/// # Safety
///
/// Scalar and fault outputs must be non-null, aligned, writable and disjoint
/// from the receiver and input storage.
///
/// `m` must be a valid `HewLayoutHashMap`. `key` must point to a readable
/// blob of the registered key layout. `val` likewise for the value layout
/// (when size > 0).
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_insert_layout(
    m: *mut HewLayoutHashMap,
    key: *const c_void,
    val: *const c_void,
    present_out: *mut bool,
    fault_out: *mut *mut c_void,
) -> i32 {
    // SAFETY: caller supplies independent key/value owners of matching layouts.
    unsafe { validate_op_inputs(m, key, Some(val)) };
    // SAFETY: all operands remain live through callback completion.
    let probe = match unsafe { probe::drive(m, key, true, fault_out) } {
        Ok(probe) => probe,
        Err(status) => return status,
    };
    // SAFETY: ready transfer-in consumes value and a vacant key only.
    let inserted = unsafe { probe::insert_transfer(probe, val) };
    // SAFETY: caller supplies writable scalar and fault outputs.
    unsafe { complete(present_out, inserted, fault_out) }
}

/// Look up a key. On success, writes a borrowed value pointer to `value_out`,
/// or null if the key is absent. Callback failure leaves `value_out` untouched.
///
/// **Pointer validity contract.** When the returned pointer is non-null it is
/// valid for reads of exactly `val_layout.size` bytes (the size registered on
/// the value layout descriptor at construction). The pointer remains valid
/// only until the next mutation of `m` (`insert_layout`, `remove_layout`,
/// `free_layout`, or any operation that may trigger a resize) — after such a
/// mutation the pointer is dangling and must not be read.
///
/// **ZST values (`HashSet` contract).** When `val_layout.size == 0` the
/// returned pointer is a *presence token* only: it indicates the key is
/// present but does not point to any readable byte. Dereferencing it is
/// undefined behaviour. Callers that only need a presence answer should use
/// [`hew_hashmap_contains_key_layout`] instead, which writes a Boolean and
/// avoids the misuse hazard.
///
/// The caller must not free the returned pointer or write through it.
///
/// Status zero initializes the scalar output and clears `fault_out`. A callback
/// failure returns its exact nonzero status and fault owner, leaving all result
/// outputs untouched and retaining the receiver with the caller.
///
/// # Safety
///
/// Scalar and fault outputs must be non-null, aligned, writable and disjoint
/// from the receiver and input storage.
///
/// `m` must be a valid `HewLayoutHashMap`. `key` must point to a valid key blob.
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_get_layout(
    m: *const HewLayoutHashMap,
    key: *const c_void,
    value_out: *mut *const c_void,
    fault_out: *mut *mut c_void,
) -> i32 {
    // SAFETY: the map and key remain live and unmodified during the lookup.
    unsafe { validate_op_inputs(m, key, None) };
    // SAFETY: lookup probes only borrow the map; this cast grants no mutation.
    let probe = match unsafe { probe::drive(m.cast_mut(), key, false, fault_out) } {
        Ok(probe) => probe,
        Err(status) => return status,
    };
    // SAFETY: the returned pointer remains borrowed from the caller's live map.
    let value = unsafe { probe::get_pointer(probe) };
    // SAFETY: caller supplies writable pointer and fault outputs.
    unsafe { complete(value_out, value, fault_out) }
}

/// Look up a key and semantic-clone the stored value into caller-provided
/// storage. On success, writes whether the key was found to `present_out`.
/// An absent lookup leaves the value output untouched.
///
/// This is the owned-return counterpart to [`hew_hashmap_get_layout`]. The
/// borrowed getter remains available for predicates and internal probes; Hew
/// `HashMap::get()` uses this entry so `Option<V>` owns an independent `V`.
///
/// Status zero initializes the scalar output and clears `fault_out`. A callback
/// failure returns its exact nonzero status and fault owner, leaving all result
/// outputs untouched and retaining the receiver with the caller.
///
/// # Safety
///
/// Scalar and fault outputs must be non-null, aligned, writable and disjoint
/// from the receiver and input storage.
///
/// `m` must be a valid `HewLayoutHashMap`. `key` must point to a valid key
/// blob. When the map's value size is non-zero, `out` must point to writable
/// storage for exactly `val_layout.size` bytes at `val_layout.align`.
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_get_clone_layout(
    m: *const HewLayoutHashMap,
    key: *const c_void,
    out: *mut c_void,
    present_out: *mut bool,
    fault_out: *mut *mut c_void,
) -> i32 {
    // SAFETY: caller provides a live map and matching key.
    unsafe { validate_op_inputs(m, key, None) };
    // SAFETY: the validated map's descriptor determines output storage needs.
    if unsafe { (*m).val_layout.size > 0 || (*m).val_layout.clone_fn.is_some() } && out.is_null() {
        panic!("map lookup output is null for a value that requires storage");
    }
    // SAFETY: lookup only borrows the map; its contents stay live throughout.
    let probe = match unsafe { probe::drive(m.cast_mut(), key, false, fault_out) } {
        Ok(probe) => probe,
        Err(status) => return status,
    };
    // SAFETY: ready lookup follows this entry's output ownership contract.
    let found = unsafe { hew_hashmap_probe_get_clone(probe, out) };
    // SAFETY: caller supplies writable scalar and fault outputs.
    unsafe { complete(present_out, found, fault_out) }
}

/// Look up a key and copy the stored value's bytes into caller-provided
/// storage without cloning them. On success, writes whether the key was found
/// to `present_out`. An absent lookup leaves the value output untouched.
///
/// This is the borrowed-return counterpart to
/// [`hew_hashmap_get_clone_layout`]: the map keeps the only owner of the value
/// and the caller reads the copy for the length of its loan on the map. The
/// caller must not release it, and the copy is dangling after the next
/// mutation of `m`.
///
/// Status zero initializes the scalar output and clears `fault_out`. A callback
/// failure returns its exact nonzero status and fault owner, leaving all result
/// outputs untouched and retaining the receiver with the caller.
///
/// # Safety
///
/// Scalar and fault outputs must be non-null, aligned, writable and disjoint
/// from the receiver and input storage.
///
/// `m` must be a valid `HewLayoutHashMap`. `key` must point to a valid key
/// blob. When the map's value size is non-zero, `out` must point to writable
/// storage for exactly `val_layout.size` bytes at `val_layout.align`.
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_get_borrow_layout(
    m: *const HewLayoutHashMap,
    key: *const c_void,
    out: *mut c_void,
    present_out: *mut bool,
    fault_out: *mut *mut c_void,
) -> i32 {
    // SAFETY: caller provides a live map and matching key.
    unsafe { validate_op_inputs(m, key, None) };
    // SAFETY: the validated map's descriptor determines output storage needs.
    if unsafe { (*m).val_layout.size > 0 } && out.is_null() {
        panic!("map lookup output is null for a value that requires storage");
    }
    // SAFETY: lookup only borrows the map; its contents stay live throughout.
    let probe = match unsafe { probe::drive(m.cast_mut(), key, false, fault_out) } {
        Ok(probe) => probe,
        Err(status) => return status,
    };
    // SAFETY: ready lookup follows this entry's output ownership contract.
    let found = unsafe { hew_hashmap_probe_get_borrow(probe, out) };
    // SAFETY: caller supplies writable scalar and fault outputs.
    unsafe { complete(present_out, found, fault_out) }
}

/// Predicate form of `hew_hashmap_get_layout`.
///
/// Status zero initializes the scalar output and clears `fault_out`. A callback
/// failure returns its exact nonzero status and fault owner, leaving all result
/// outputs untouched and retaining the receiver with the caller.
///
/// # Safety
///
/// Scalar and fault outputs must be non-null, aligned, writable and disjoint
/// from the receiver and input storage.
///
/// Same as [`hew_hashmap_get_layout`].
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_contains_key_layout(
    m: *const HewLayoutHashMap,
    key: *const c_void,
    present_out: *mut bool,
    fault_out: *mut *mut c_void,
) -> i32 {
    // SAFETY: caller provides a live map and matching borrowed key.
    unsafe { validate_op_inputs(m, key, None) };
    // SAFETY: lookup only borrows the map; its contents stay live throughout.
    let probe = match unsafe { probe::drive(m.cast_mut(), key, false, fault_out) } {
        Ok(probe) => probe,
        Err(status) => return status,
    };
    // SAFETY: the ready probe is consumed without changing the map.
    let found = unsafe { hew_hashmap_probe_contains(probe) };
    // SAFETY: caller supplies writable scalar and fault outputs.
    unsafe { complete(present_out, found, fault_out) }
}

/// Remove a key. On success, writes whether a matching entry was removed.
///
/// Status zero initializes the scalar output and clears `fault_out`. A callback
/// failure returns its exact nonzero status and fault owner, leaving all result
/// outputs untouched and retaining the receiver with the caller.
///
/// # Safety
///
/// Scalar and fault outputs must be non-null, aligned, writable and disjoint
/// from the receiver and input storage.
///
/// Same as [`hew_hashmap_get_layout`].
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_remove_layout(
    m: *mut HewLayoutHashMap,
    key: *const c_void,
    present_out: *mut bool,
    fault_out: *mut *mut c_void,
) -> i32 {
    // SAFETY: caller lends the live map exclusively and a matching query.
    unsafe { validate_op_inputs(m, key, None) };
    // SAFETY: no mutation occurs until every callback finishes.
    let probe = match unsafe { probe::drive(m, key, false, fault_out) } {
        Ok(probe) => probe,
        Err(status) => return status,
    };
    // SAFETY: the ready removal releases the occupied key and value once.
    let found = unsafe { probe::remove_drop(probe) };
    // SAFETY: caller supplies writable scalar and fault outputs.
    unsafe { complete(present_out, found, fault_out) }
}

/// Remove a key, MOVING its value out into `out`. On success, writes whether
/// it was found to `present_out`. A found value moves exactly `val_layout.size`
/// bytes into `out`; an absent lookup leaves the value output untouched. This is the
/// `Option<V>`-producing twin of [`hew_hashmap_remove_layout`]: the KEY is
/// dropped via `key_drop` (the map owned it and it is being removed), but the
/// VALUE is MOVED — byte-copied into `out` with NO `val_drop` — so ownership
/// transfers to the caller's `Some` payload.
///
/// # Ownership invariant (the crux)
///
/// After the move there is exactly ONE owner of V: the caller's `out` slot.
/// The tombstoned entry is never read or dropped again (the slot is dead and
/// `len` shrank), so the value is neither leaked (the caller owns and will
/// drop it) nor double-freed (the map does NOT run `val_drop`). The KEY, by
/// contrast, is dropped here because the map owned it and the caller never
/// receives it. Getting this wrong is a leaked key or a double-freed value.
/// Mirrors `hew_hashmap_get_clone_layout`'s out-param discipline but MOVES the
/// value instead of cloning it (the map keeps no copy).
///
/// Status zero initializes the scalar output and clears `fault_out`. A callback
/// failure returns its exact nonzero status and fault owner, leaving all result
/// outputs untouched and retaining the receiver with the caller.
///
/// # Safety
///
/// Scalar and fault outputs must be non-null, aligned, writable and disjoint
/// from the receiver and input storage.
///
/// `m` must be a valid `HewLayoutHashMap`. `key` must point to a valid key
/// blob. When the map's value size is non-zero, `out` must point to writable
/// storage for exactly `val_layout.size` bytes at `val_layout.align`.
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_remove_take_layout(
    m: *mut HewLayoutHashMap,
    key: *const c_void,
    out: *mut c_void,
    present_out: *mut bool,
    fault_out: *mut *mut c_void,
) -> i32 {
    // SAFETY: caller lends the live map exclusively and a matching query.
    unsafe { validate_op_inputs(m, key, None) };
    // SAFETY: the validated map's value descriptor determines output size.
    if unsafe { (*m).val_layout.size > 0 } && out.is_null() {
        panic!("map removal output is null for a nonzero value");
    }
    // SAFETY: no mutation occurs until every callback finishes.
    let probe = match unsafe { probe::drive(m, key, false, fault_out) } {
        Ok(probe) => probe,
        Err(status) => return status,
    };
    // SAFETY: ready removal transfers the value into disjoint output storage.
    let found = unsafe { hew_hashmap_probe_remove_take(probe, out) };
    // SAFETY: caller supplies writable scalar and fault outputs.
    unsafe { complete(present_out, found, fault_out) }
}

/// Number of occupied entries.
///
/// # Safety
///
/// `m` must be a valid `HewLayoutHashMap`.
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_len_layout(m: *const HewLayoutHashMap) -> i64 {
    // SAFETY: shared fail-closed gate (map-only variant).
    unsafe { validate_op_map(m) };
    // SAFETY: m non-null per gate.
    let map = unsafe { &*m };
    // Workspace caps len well below i64::MAX; the cast is documented lossless
    // by the crate-level `cast_possible_wrap` allow above.
    map.len as i64
}

/// Free a layout-backed map and its byte storage. A null `m` is a documented
/// no-op (LESSONS `boundary-fail-closed`: fail-closed shape, not silent
/// success — null in / null out, no further work).
///
/// # Concurrency contract
///
/// All `HewLayoutHashMap` operations — including `free_layout` — require
/// **external synchronisation by the caller**. The type does *not* contain
/// internal locking. Specifically, when this function is called:
///
/// * No other thread may hold a reference to (or pointer into) `*m`.
/// * No other thread may be mid-probe (mid-call) on any of the
///   `insert_layout` / `get_layout` / `contains_key_layout` / `remove_layout`
///   / `len_layout` entry points against `m`.
/// * No thread may still be holding a borrowed value pointer returned by a
///   prior `get_layout` call against `m` (such pointers become dangling the
///   instant this function returns; reading them after the free is undefined
///   behaviour).
///
/// Violating any of these requirements is undefined behaviour. The map ABI is
/// designed for single-owner / single-threaded use at the boundary; callers
/// that need shared ownership must layer their own synchronisation
/// (`Mutex<*mut HewLayoutHashMap>` or equivalent) above this surface.
///
/// # Safety
///
/// `m` must have been returned by [`hew_hashmap_new_with_layout`] (or be null).
/// After this call, `m` is invalid.
#[no_mangle]
pub unsafe extern "C-unwind" fn hew_hashmap_free_layout(m: *mut HewLayoutHashMap) {
    // SAFETY: forwarded allocation contract.
    unsafe { release_map(m, false) }
}

/// Free a layout-backed map through the walker, joining a walk already in
/// progress.
///
/// Codegen emits this where physical MIR proved the whole released subtree is
/// ordinary data, so a map of deep values flattens instead of nesting one
/// native frame per level.
///
/// # Safety
///
/// `m` must have been returned by [`hew_hashmap_new_with_layout`] (or be null).
/// After this call, `m` is invalid.
#[no_mangle]
pub unsafe extern "C-unwind" fn hew_hashmap_free_layout_walk(m: *mut HewLayoutHashMap) {
    // SAFETY: forwarded allocation contract.
    unsafe { release_map(m, true) }
}

/// Whether any occupied slot owns something the release protocol must drop.
///
/// `Plain` ownership descriptors carry `drop_fn = None`, so a map of plain keys
/// and values skips the slot walk entirely. The constructor's fail-closed check
/// guarantees owned ownership kinds (`String`/`LayoutManaged`) always have a
/// `drop_fn`, so skipping cannot leak.
///
/// # Safety
///
/// `m` must be a valid map pointer.
unsafe fn map_needs_slot_drop(m: *mut HewLayoutHashMap) -> bool {
    // SAFETY: caller guarantees `m` is valid.
    let map_ref = unsafe { &*m };
    !map_ref.entries.is_null()
        && map_ref.cap != 0
        && (map_ref.key_layout.value.drop_fn.is_some() || map_ref.val_layout.drop_fn.is_some())
}

/// Release a whole map through the walker.
///
/// `deferred` joins a walk already in progress instead of draining here, which
/// is admitted only where the whole released subtree is ordinary data.
///
/// # Safety
///
/// `m` must be null or a map allocation this call exclusively owns.
pub(crate) unsafe fn release_map(m: *mut HewLayoutHashMap, deferred: bool) {
    if m.is_null() {
        return;
    }
    // SAFETY: caller guarantees the allocation contract.
    unsafe {
        if !map_needs_slot_drop(m) {
            free_map_storage(m);
            return;
        }
        let item = ReleaseItem::Map { map: m };
        if deferred {
            release_walker::release_deferred(item);
        } else {
            release_walker::release_now(item);
        }
    }
}

/// Walker step: queue this map's occupied slots, then its own storage beneath
/// them.
///
/// # Safety
///
/// `m` must be a map allocation the walk in progress exclusively owns.
pub(crate) unsafe fn expand_map(m: *mut HewLayoutHashMap) {
    // The slots sit above the storage step, so the entry buffer outlives every
    // slot the cursor still addresses.
    release_walker::queue(ReleaseItem::MapStorage { map: m });
    release_walker::queue(ReleaseItem::MapSlots { map: m, next: 0 });
}

/// Walker step: release the next chunk of occupied slots at or after `next`,
/// keeping the rest of the scan.
///
/// Tombstoned slots already had their blobs dropped at remove time and must not
/// be dropped again.
///
/// # Safety
///
/// `m` must be a map the walk owns whose entry buffer is still live.
pub(crate) unsafe fn release_slot_chunk(m: *mut HewLayoutHashMap, next: usize) {
    // SAFETY: caller guarantees the map and its entry buffer are live.
    let map_ref = unsafe { &*m };
    let entries = map_ref.entries;
    let stride = map_ref.stride;
    let key_drop_fn = map_ref.key_layout.value.drop_fn;
    let val_drop_fn = map_ref.val_layout.drop_fn;
    let mut released = 0;
    let mut idx = next;
    while idx < map_ref.cap {
        // SAFETY: idx < cap; stride matches allocation.
        if unsafe { *slot_state(entries, idx, stride) } != OCCUPIED {
            idx += 1;
            continue;
        }
        if released == release_walker::STEP_ELEMENTS {
            // The remaining scan stays beneath whatever this chunk queued, so a
            // value's whole subtree is released before the rest of the map.
            release_walker::queue(ReleaseItem::MapSlots { map: m, next: idx });
            return;
        }
        if let Some(key_drop) = key_drop_fn {
            // SAFETY: occupied slot has a valid K blob at key_offset.
            let slot_key_ptr = unsafe { slot_key(entries, idx, stride, map_ref.key_offset) };
            key_drop(slot_key_ptr.cast::<c_void>());
        }
        if let Some(val_drop) = val_drop_fn {
            // SAFETY: occupied slot has a valid V blob at val_offset.
            let slot_val_ptr = unsafe { slot_val(entries, idx, stride, map_ref.val_offset) };
            val_drop(slot_val_ptr.cast::<c_void>());
        }
        released += 1;
        idx += 1;
    }
}

/// Walker step: release the map's entry buffer and its header.
///
/// # Safety
///
/// `m` must be a map allocation whose occupied slots are already released.
pub(crate) unsafe fn free_map_storage(m: *mut HewLayoutHashMap) {
    // SAFETY: caller guarantees the allocation contract.
    let map_ref = unsafe { &*m };
    let entries_align = core::cmp::max(map_ref.key_layout.value.align, map_ref.val_layout.align);
    // SAFETY: entries allocated by alloc_layout_entries with these exact params.
    unsafe { dealloc_layout_entries(map_ref.entries, map_ref.cap, map_ref.stride, entries_align) };
    // SAFETY: m allocated via the sized-block allocator in hew_hashmap_new_with_layout or clone.
    unsafe {
        ptr::drop_in_place(m);
        crate::mem::buf_free(m.cast());
    }
}

/// Remove every entry, dropping owned K/V blobs, but keep the `entries`
/// buffer allocated for reuse (mirrors [`hew_vec_clear`](crate::vec::hew_vec_clear)'s
/// retain-capacity contract). Both `OCCUPIED` and stale `TOMBSTONE` slots are
/// reset to `EMPTY`.
///
/// # Safety
///
/// `m` must be a valid `HewLayoutHashMap` pointer (non-null).
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_clear_layout(m: *mut HewLayoutHashMap) {
    // SAFETY: shared fail-closed gate; aborts on null (a genuine caller bug —
    // unlike `free_layout`, `clear` has no legitimate null-no-op use).
    unsafe { validate_op_map(m) };
    // SAFETY: m non-null per gate.
    let map = unsafe { &mut *m };
    let entries = map.entries;
    let cap = map.cap;
    let stride = map.stride;
    let key_offset = map.key_offset;
    let val_offset = map.val_offset;
    let kl = &map.key_layout;
    let vl = &map.val_layout;
    let key_drop_fn_opt = kl.value.drop_fn;
    let val_drop_fn_opt = vl.drop_fn;

    if !entries.is_null() && cap > 0 {
        for idx in 0..cap {
            // SAFETY: idx < cap; stride matches allocation.
            let state_ptr = unsafe { slot_state(entries, idx, stride) };
            // SAFETY: state byte in-bounds.
            let state = unsafe { *state_ptr };
            if state == OCCUPIED {
                if let Some(key_drop) = key_drop_fn_opt {
                    // SAFETY: occupied slot has a valid K blob at key_offset.
                    let slot_key_ptr = unsafe { slot_key(entries, idx, stride, key_offset) };
                    key_drop(slot_key_ptr.cast::<c_void>());
                }
                if let Some(val_drop) = val_drop_fn_opt {
                    // SAFETY: occupied slot has a valid V blob at val_offset.
                    let slot_val_ptr = unsafe { slot_val(entries, idx, stride, val_offset) };
                    val_drop(slot_val_ptr.cast::<c_void>());
                }
            }
            if state != EMPTY {
                // SAFETY: state byte in-bounds; resets both OCCUPIED and
                // stale TOMBSTONE slots so probing starts fresh post-clear.
                unsafe { *state_ptr = EMPTY };
            }
        }
    }

    map.len = 0;
}

// ---------------------------------------------------------------------------
// keys_layout / values_layout — eager Vec snapshot (Gap A)
// ---------------------------------------------------------------------------
//
// Both functions walk occupied slots (following the `free_layout` slot-walk
// pattern) and copy each key or value blob into a freshly allocated `HewVec`.
// The caller owns the returned Vec and is responsible for freeing it.
//
// Ownership contract:
//   - `Plain` blobs: raw-copied via `hew_vec_push_layout` (BitCopy).
//   - `String` blobs: the map slot holds a headerless `*const c_char` pointer;
//     `hew_vec_push_str` makes an independent header-aware copy. The Vec owns
//     the resulting header-bearing element.
//   - `LayoutManaged`: not supported — aborts fail-closed (no clone thunk
//     available at this stage; marked for revisit at W5.011-P2b-maps).
//
// Security note: both functions clone owned blobs (String keys/values are
// deep-copied via `hew_string_clone`). No aliasing of live map storage.
//
// WHY eager Vec (not lazy iterator): every Hew collection iterates a snapshot.
// `for x in v`, `for x in s` and `for (k, v) in m` all walk owned copies, so a
// yielded element is independently droppable and mutating the source inside the
// loop is defined rather than refused. These projections are that contract for
// maps: `HashMapIter<K, V>` holds the `keys()` and `values()` snapshots and a
// cursor index (`std/builtins.hew`).
// WHEN obsolete: when Hew adopts borrowed iteration — a cursor that holds a
// loan on its source, with mutation during iteration refused by the checker.
// That is a language semantics change across Vec, HashSet and HashMap, not a
// runtime gap: `hew_hashmap_iter_*_layout` below is already a real lazy cursor
// and has been unused by the compiler for exactly this reason.
// WHAT the real solution looks like: a loan on the iterated place for the
// loop's extent, the existing lazy cursor entries wired into the for-in
// desugar for all three collections, and a diagnostic naming the loan.

/// Collect one field from each occupied slot using the vector's copy protocol.
unsafe fn collect_layout_field(
    map: &HewLayoutHashMap,
    layout: HewValueLayout,
    offset: usize,
) -> *mut HewVec {
    require_clone(&layout, "map projection");
    // SAFETY: the descriptor was validated by the map constructor and is copied by the vector.
    let result = unsafe { crate::vec::hew_vec_new_with_elem_layout(&raw const layout) };
    for index in 0..map.cap {
        // SAFETY: the state byte is inside the live map allocation.
        if unsafe { *slot_state(map.entries, index, map.stride) } != OCCUPIED {
            continue;
        }
        // SAFETY: an occupied slot contains a live value at the supplied field offset.
        let source = unsafe { map.entries.add(index * map.stride + offset) };
        // SAFETY: push borrows the field and creates an independent value through the same descriptor.
        unsafe { crate::vec::hew_vec_push_owned(result, source.cast()) };
    }
    result
}

/// Collect all keys of a layout-backed map into a new `HewVec`.
///
/// Returns an eagerly allocated `*mut HewVec` containing one cloned copy of
/// each occupied key blob.  Order is unspecified (reflects slot-walk order).
///
/// # Ownership
///
/// The caller owns the returned `HewVec` and must free it via
/// `hew_vec_free_owned` when done, for every supported key type.
/// The source map is unchanged.
///
/// A null `m` returns null fail-closed.
///
/// # Safety
///
/// `m` must have been returned by [`hew_hashmap_new_with_layout`] (or be null).
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_keys_layout(m: *const HewLayoutHashMap) -> *mut HewVec {
    if m.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: the caller supplies a live map.
    unsafe { validate_op_map(m) };
    // SAFETY: m is non-null and live for the complete projection.
    let map = unsafe { &*m };
    // SAFETY: the descriptor and offset describe an initialized field in every occupied slot.
    unsafe { collect_layout_field(map, map.key_layout.value, map.key_offset) }
}

/// Collect every occupied `(K, V)` pair into an independently owned Vec.
///
/// `pair_layout` is the codegen-synthesised tuple element descriptor, including
/// its composite clone/drop thunks. `v_offset` is the target ABI offset of the
/// tuple's V field; the runtime deliberately does not reproduce tuple layout.
/// Each map element is cloned once into aligned scratch storage, then moved
/// into the Vec so the tuple descriptor owns exactly one copy.
///
/// # Safety
///
/// `m` and `pair_layout` must be valid descriptors produced by Hew codegen,
/// and `v_offset` must identify the V field within `pair_layout.size`.
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_entries_layout(
    m: *const HewLayoutHashMap,
    pair_layout: *const HewValueLayout,
    v_offset: u64,
) -> *mut HewVec {
    if m.is_null() || pair_layout.is_null() {
        return core::ptr::null_mut();
    }
    // SAFETY: m is non-null (checked above) and, per this fn's contract, a
    // valid codegen-produced map descriptor.
    unsafe { validate_op_map(m) };
    // SAFETY: m is non-null and was validated by the gate above.
    let map = unsafe { &*m };
    // SAFETY: pair_layout is non-null (checked above) and, per this fn's
    // contract, a valid codegen-produced element descriptor.
    let pair = unsafe { &*pair_layout };
    let v_offset = usize::try_from(v_offset)
        .unwrap_or_else(|_| abort_layout_clone("hew_hashmap_entries_layout: V offset too large"));
    let Some(v_end) = v_offset.checked_add(map.val_layout.size) else {
        abort_layout_clone("hew_hashmap_entries_layout: V field offset overflow");
    };
    if v_offset < map.key_layout.value.size || v_end > pair.size {
        abort_layout_clone("hew_hashmap_entries_layout: invalid pair field layout");
    }

    // SAFETY: pair_layout is non-null and a valid element descriptor, which is
    // exactly this constructor's precondition.
    let vec = unsafe { crate::vec::hew_vec_new_with_elem_layout(pair_layout) };
    if vec.is_null() {
        return core::ptr::null_mut();
    }
    // Logical empty pairs still need a non-null aligned address for callbacks
    // and the vector's move protocol. Only allocation geometry is enlarged.
    let scratch_layout = std::alloc::Layout::from_size_align(pair.size.max(1), pair.align)
        .unwrap_or_else(|_| abort_layout_clone("hew_hashmap_entries_layout: invalid pair layout"));

    for idx in 0..map.cap {
        // SAFETY: idx < map.cap, so this addresses a slot inside the live
        // entries allocation at the map's own stride.
        let state = unsafe { *slot_state(map.entries, idx, map.stride) };
        if state != OCCUPIED {
            continue;
        }
        // SAFETY: scratch_layout has nonzero allocation size and preserves the
        // pair's alignment, including when both logical fields are zero-sized.
        let scratch = unsafe { std::alloc::alloc_zeroed(scratch_layout) };
        if scratch.is_null() {
            std::alloc::handle_alloc_error(scratch_layout);
        }
        // SAFETY: idx addresses an OCCUPIED slot, so its key field is live and
        // initialised at the map's recorded key offset.
        let key = unsafe { slot_key(map.entries, idx, map.stride, map.key_offset) };
        // SAFETY: as above for the value field at the recorded value offset.
        let val = unsafe { slot_val(map.entries, idx, map.stride, map.val_offset) };
        // SAFETY: key/val point at live initialised blobs; scratch is a fresh
        // zeroed allocation of pair.size with v_offset..v_end validated to lie
        // inside it, so both clones write within bounds. The push then moves
        // that single owned copy into the Vec, after which the scratch storage
        // holds no owning references and is deallocated with its own layout.
        unsafe {
            clone_layout_blob(map.key_layout.value, key, scratch, "map key");
            clone_layout_blob(
                map.val_layout,
                val,
                scratch.add(v_offset),
                "hew_hashmap_entries_layout value",
            );
            crate::vec::hew_vec_push_owned_move(vec, scratch.cast::<c_void>());
            std::alloc::dealloc(scratch, scratch_layout);
        }
    }
    vec
}

/// Collect all values of a layout-backed map into a new `HewVec`.
///
/// Returns an eagerly allocated `*mut HewVec` containing one cloned copy of
/// each occupied value blob.  Order is unspecified (reflects slot-walk order).
///
/// # Ownership
///
/// The caller owns the returned `HewVec` and must free it via
/// `hew_vec_free_owned` when done, for every supported value type.
/// The source map is unchanged.
///
/// A null `m` returns null fail-closed.
///
/// # Safety
///
/// `m` must have been returned by [`hew_hashmap_new_with_layout`] (or be null).
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_values_layout(m: *const HewLayoutHashMap) -> *mut HewVec {
    if m.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: the caller supplies a live map.
    unsafe { validate_op_map(m) };
    // SAFETY: m is non-null and live for the complete projection.
    let map = unsafe { &*m };
    // SAFETY: the descriptor and offset describe an initialized field in every occupied slot.
    unsafe { collect_layout_field(map, map.val_layout, map.val_offset) }
}
