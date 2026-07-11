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

use core::ffi::{c_char, c_void};
use core::ptr;

use hew_cabi::map::{HewMapKeyLayout, HewMapValueLayout};
use hew_cabi::vec::{HewTypeLayout, HewTypeOwnershipKind, HewVec};

/// Entry states.
const EMPTY: u8 = 0;
const OCCUPIED: u8 = 1;
const TOMBSTONE: u8 = 2;

/// Load factor percentage threshold for resize (shared by the layout family).
const LOAD_PCTG: usize = 75;

// ===========================================================================
// Layout-backed HashMap (`HewLayoutHashMap`) — C-1b (W3.003 slice C-1b)
// ===========================================================================
//
// A sibling of `HewHashMap` that stores opaque key/value blobs whose identity
// is delegated to caller-supplied hash and equality thunks. Slot layout per
// council Rev 1:
//
//   [state: u8][pad to key_align][key blob: key_size]
//                  [pad to val_align][val blob: val_size][pad to max(key_align,val_align)]
//
// All `#[no_mangle]` entry points are fail-closed per LESSONS `boundary-fail-closed`
// (P0): null pointers, missing thunks, zero-size keys, non-power-of-two alignment,
// stride overflow, and `LayoutManaged` ownership are rejected at the boundary.
//
// Ownership contract (LESSONS `ffi-ownership-contracts` P0):
//   - The map owns the byte blobs it copies into its slots.
//   - The map does NOT own `key_layout` / `val_layout` — caller keeps these alive
//     for the lifetime of the map (the runtime reads `hash_fn`/`eq_fn` and the
//     size/align fields on every probe and on free).
//   - For `ownership_kind == Plain` (only supported variant in this slice), blobs
//     are byte-copy-safe — no per-slot drop is run on free / overwrite / removal.

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
    /// any padding required to reach `key_layout.align`).
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
    pub val_layout: HewMapValueLayout,
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
/// Panics if `key_layout` is null, `hash_fn`/`eq_fn` are `None`, `size == 0`,
/// or `align` is not a power of two.
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
    if kl.hash_fn.is_none() {
        crate::set_last_error("HewLayoutHashMap: key_layout.hash_fn is None");
        panic!("HewLayoutHashMap: key_layout.hash_fn is None");
    }
    if kl.eq_fn.is_none() {
        crate::set_last_error("HewLayoutHashMap: key_layout.eq_fn is None");
        panic!("HewLayoutHashMap: key_layout.eq_fn is None");
    }
    if kl.size == 0 {
        crate::set_last_error("HewLayoutHashMap: zero-size keys are not admissible");
        panic!("HewLayoutHashMap: zero-size keys are not admissible");
    }
    if !kl.align.is_power_of_two() {
        crate::set_last_error("HewLayoutHashMap: key_layout.align is not a power of two");
        panic!("HewLayoutHashMap: key_layout.align is not a power of two");
    }
    // C0a: LayoutManaged / String ownership are admitted; their drop_fn
    // requirement is enforced by validate_descriptor_ownership (called from
    // the constructor).
}

/// Validate a `HewMapValueLayout` at constructor time.
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
/// `val_layout` must be non-null and point to a valid `HewMapValueLayout`.
pub unsafe fn validate_val_layout(val_layout: *const HewMapValueLayout) {
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
/// `"HewLayoutHashMap: {key_layout|val_layout} ownership_kind={String|LayoutManaged} requires drop_fn"`
/// on any of the four rejection cases.
///
/// # Safety
///
/// Both pointers must be non-null and point to valid descriptors (call
/// `validate_key_layout` / `validate_val_layout` first).
pub unsafe fn validate_descriptor_ownership(
    key_layout: *const HewMapKeyLayout,
    val_layout: *const HewMapValueLayout,
) {
    // SAFETY: caller-guaranteed non-null + valid.
    let kl = unsafe { &*key_layout };
    // SAFETY: same.
    let vl = unsafe { &*val_layout };
    match kl.ownership_kind {
        HewTypeOwnershipKind::Plain => {}
        HewTypeOwnershipKind::String => {
            if kl.drop_fn.is_none() {
                crate::set_last_error(
                    "HewLayoutHashMap: key_layout ownership_kind=String requires drop_fn",
                );
                panic!("HewLayoutHashMap: key_layout ownership_kind=String requires drop_fn");
            }
        }
        HewTypeOwnershipKind::LayoutManaged => {
            if kl.drop_fn.is_none() {
                crate::set_last_error(
                    "HewLayoutHashMap: key_layout ownership_kind=LayoutManaged requires drop_fn",
                );
                panic!(
                    "HewLayoutHashMap: key_layout ownership_kind=LayoutManaged requires drop_fn"
                );
            }
        }
        HewTypeOwnershipKind::Bytes => {
            // The Bytes kind belongs to the channel/stream element witness;
            // map descriptors never carry it. Fail closed.
            crate::set_last_error("HewLayoutHashMap: key_layout ownership_kind=Bytes is not valid");
            panic!("HewLayoutHashMap: key_layout ownership_kind=Bytes is not valid");
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
            // The Bytes kind belongs to the channel/stream element witness;
            // map descriptors never carry it. Fail closed.
            crate::set_last_error("HewLayoutHashMap: val_layout ownership_kind=Bytes is not valid");
            panic!("HewLayoutHashMap: val_layout ownership_kind=Bytes is not valid");
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
    val_layout: *const HewMapValueLayout,
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
    let Some(t) = compute_slot_layout(kl.size, kl.align, vl.size, vl.align) else {
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

/// Probe the map for `key` starting at its hashed slot. Returns the slot index
/// of an existing OCCUPIED match, the first TOMBSTONE encountered, or the
/// first EMPTY slot. The shared walker is used by insert, get, contains, and
/// remove — there is exactly one probe implementation in this module.
///
/// `found_existing` is set to `true` when the returned slot already contains
/// the searched-for key (OCCUPIED + eq).
///
/// # Safety
///
/// All pointers must come from a live `HewLayoutHashMap`. `hash_fn` and
/// `eq_fn` must be the thunks registered on its `key_layout`. `cap` must be a
/// non-zero power of two.
#[allow(
    clippy::too_many_arguments,
    reason = "layout fields are read as separate locals before reborrow to avoid \
              re-deriving them through &self mid-resize (council Rev 1 §7)"
)]
#[allow(
    clippy::cast_possible_truncation,
    reason = "hash truncation u64 -> usize is intentional masking via probe modulus"
)]
unsafe fn layout_probe(
    entries: *mut u8,
    cap: usize,
    stride: usize,
    key_offset: usize,
    key: *const c_void,
    hash_fn: unsafe extern "C" fn(*const c_void) -> u64,
    eq_fn: unsafe extern "C" fn(*const c_void, *const c_void) -> i32,
) -> (usize, bool) {
    let mask = cap - 1;
    // SAFETY: caller guarantees key is valid for the type.
    let h = unsafe { hash_fn(key) };
    let start = (h as usize) & mask;
    let mut idx = start;
    let mut first_tombstone: Option<usize> = None;
    loop {
        // SAFETY: idx < cap; stride/key_offset come from a valid map.
        let state_ptr = unsafe { slot_state(entries, idx, stride) };
        // SAFETY: state byte is in-bounds.
        let state = unsafe { *state_ptr };
        match state {
            EMPTY => {
                return (first_tombstone.unwrap_or(idx), false);
            }
            OCCUPIED => {
                // SAFETY: key offset is in-bounds.
                let slot_key_ptr = unsafe { slot_key(entries, idx, stride, key_offset) };
                // SAFETY: caller-supplied thunk; both pointers valid for layout.
                let eq = unsafe { eq_fn(slot_key_ptr.cast::<c_void>(), key) };
                if eq != 0 {
                    return (idx, true);
                }
            }
            TOMBSTONE => {
                if first_tombstone.is_none() {
                    first_tombstone = Some(idx);
                }
            }
            _ => unreachable!("HewLayoutHashMap: invalid slot state"),
        }
        idx = (idx + 1) & mask;
        if idx == start {
            // Probed the whole table; must have a tombstone or we would have
            // hit EMPTY (resize keeps load < 100%).
            let Some(slot) = first_tombstone else {
                crate::set_last_error("HewLayoutHashMap: full table without empty/tombstone");
                std::process::abort();
            };
            return (slot, false);
        }
    }
}

/// Resize the table to double capacity, re-hashing every OCCUPIED slot.
///
/// # Safety
///
/// `m` must be a valid, fully-initialised `HewLayoutHashMap` pointer.
#[allow(
    clippy::cast_possible_truncation,
    reason = "hash truncation u64 -> usize on 32-bit targets is intentional probe masking"
)]
unsafe fn layout_resize(m: *mut HewLayoutHashMap) {
    // Council Rev 1 §7: bind every layout-derived value as a scalar / raw-pointer
    // local BEFORE any allocator call. No Rust reference (`&*m`, `&mut *m`, or
    // a reference borrowed through them) is retained across `alloc(...)` or
    // `dealloc(...)`. The rebuild loop below operates exclusively on these
    // locals and the raw entries pointers; field writes back to `*m` happen
    // only after the dealloc of the old entries.

    // -- Step 1: copy all needed map fields as scalars / raw pointers ----------
    // SAFETY: caller guarantees `m` is a valid, fully-initialised pointer; the
    // raw field projections below read plain scalar / raw-pointer fields and
    // do not borrow `*m`.
    //
    // W4.001 Stage C0a: `key_layout` / `val_layout` are now owned by-value
    // snapshots on the map (not raw pointers). We read their *fields* as
    // scalar locals here — never borrowing the descriptor itself across the
    // allocator — preserving the council Rev 1 §7 discipline.
    let (old_entries, old_cap, stride, key_offset, val_offset): (
        *mut u8,
        usize,
        usize,
        usize,
        usize,
    ) = unsafe {
        (
            (*m).entries,
            (*m).cap,
            (*m).stride,
            (*m).key_offset,
            (*m).val_offset,
        )
    };

    // -- Step 2: copy needed layout descriptor fields as scalars ---------------
    // SAFETY: `m` valid; reads of the by-value descriptor's scalar fields are
    // plain scalar loads and do not borrow `*m`.
    let (key_size, key_align, val_size, val_align, hash_fn_opt) = unsafe {
        (
            (*m).key_layout.size,
            (*m).key_layout.align,
            (*m).val_layout.size,
            (*m).val_layout.align,
            (*m).key_layout.hash_fn,
        )
    };
    let entries_align = core::cmp::max(key_align, val_align);

    let Some(hash_fn) = hash_fn_opt else {
        crate::set_last_error(
            "HewLayoutHashMap: hash_fn None at resize (constructor guard violated)",
        );
        std::process::abort();
    };

    let Some(new_cap) = old_cap.checked_mul(2) else {
        crate::set_last_error("HewLayoutHashMap: capacity doubling overflow");
        std::process::abort();
    };

    // -- Step 3: allocate new entries (no Rust reference to *m is live here) --
    // SAFETY: stride > 0 by construction; new_cap > 0.
    let new_entries = unsafe { alloc_layout_entries(new_cap, stride, entries_align) };

    // -- Step 4: rebuild via raw pointers and scalar locals only --------------
    let new_mask = new_cap - 1;
    for i in 0..old_cap {
        // SAFETY: i < old_cap; old_entries valid for the original table.
        let state = unsafe { *slot_state(old_entries, i, stride) };
        if state != OCCUPIED {
            continue;
        }
        // SAFETY: occupied slot has valid key + value blobs at known offsets.
        let src_key = unsafe { slot_key(old_entries, i, stride, key_offset) };
        // SAFETY: hash_fn is the thunk registered on key_layout, which has not
        // moved across the resize (caller-stable descriptors).
        let h = unsafe { hash_fn(src_key.cast::<c_void>()) };
        let mut idx = (h as usize) & new_mask;
        loop {
            // SAFETY: idx < new_cap; new_entries freshly zero-initialised so
            // every state byte is EMPTY (0) until we write OCCUPIED.
            let dst_state = unsafe { slot_state(new_entries, idx, stride) };
            // SAFETY: in-bounds; state byte is u8.
            if unsafe { *dst_state } == EMPTY {
                // SAFETY: dst_state is in-bounds; OCCUPIED constant is u8.
                unsafe { *dst_state = OCCUPIED };
                // SAFETY: dst key/value offsets in-bounds; src equally so;
                // key_size / val_size are the scalar copies bound above.
                unsafe {
                    let dst_key = slot_key(new_entries, idx, stride, key_offset);
                    ptr::copy_nonoverlapping(src_key, dst_key, key_size);
                    if val_size > 0 {
                        let src_val = slot_val(old_entries, i, stride, val_offset);
                        let dst_val = slot_val(new_entries, idx, stride, val_offset);
                        ptr::copy_nonoverlapping(src_val, dst_val, val_size);
                    }
                }
                break;
            }
            idx = (idx + 1) & new_mask;
        }
    }

    // -- Step 5: release the old entries before writing back ------------------
    // SAFETY: old_entries was allocated with (old_cap, stride, entries_align).
    unsafe { dealloc_layout_entries(old_entries, old_cap, stride, entries_align) };

    // -- Step 6: write the new pointer / capacity back via raw field writes ---
    // No `&mut *m` borrow is taken; the writes go through the raw pointer
    // directly. `len` is unchanged (every OCCUPIED slot moved exactly once).
    // SAFETY: `m` is a valid pointer; fields are plain scalars / raw pointers.
    unsafe {
        (*m).entries = new_entries;
        (*m).cap = new_cap;
    }
}

// ---------------------------------------------------------------------------
// Constructor (layout-backed)
// ---------------------------------------------------------------------------

/// Create a new layout-backed `HewLayoutHashMap`.
///
/// Fail-closed gates (council Rev 2/3 + W4.001 Stage C0a): aborts on null
/// layout pointers, missing hash/eq thunks, zero-size key, non-power-of-two
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
    val_layout: *const HewMapValueLayout,
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
        unsafe { libc::malloc(core::mem::size_of::<HewLayoutHashMap>()).cast() };
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

unsafe fn clone_layout_string_blob(src: *const u8, dst: *mut u8, label: &str) {
    // SAFETY: caller guarantees `src` points to a slot blob containing a
    // C-string pointer and `dst` points to writable slot storage for the same
    // blob shape.
    let src_ptr: *const c_char = unsafe { ptr::read_unaligned(src.cast::<*const c_char>()) };
    // SAFETY: `src_ptr` is either null or a valid NUL-terminated string by the
    // descriptor's String ownership contract.
    let cloned = unsafe { crate::string::hew_string_clone(src_ptr) };
    if !src_ptr.is_null() && cloned.is_null() {
        abort_layout_clone(format!("{label}: string clone allocation failed"));
    }
    // SAFETY: `dst` is writable for a pointer-sized String blob.
    unsafe { ptr::write_unaligned(dst.cast::<*mut c_char>(), cloned) };
}

unsafe fn clone_layout_key_blob(
    ownership_kind: HewTypeOwnershipKind,
    src: *const u8,
    dst: *mut u8,
    size: usize,
) {
    match ownership_kind {
        HewTypeOwnershipKind::Plain => {
            // SAFETY: caller guarantees both blobs are valid for `size` bytes.
            unsafe { ptr::copy_nonoverlapping(src, dst, size) };
        }
        HewTypeOwnershipKind::String => {
            // SAFETY: forwarded blob contract.
            unsafe { clone_layout_string_blob(src, dst, "hew_hashmap_clone_layout key") };
        }
        HewTypeOwnershipKind::LayoutManaged => abort_layout_clone(
            "hew_hashmap_clone_layout: key layout-managed clone thunk is unavailable",
        ),
        HewTypeOwnershipKind::Bytes => abort_layout_clone(
            "hew_hashmap_clone_layout: key ownership_kind=Bytes is not valid for maps",
        ),
    }
}

unsafe fn clone_layout_value_blob(
    layout: HewMapValueLayout,
    src: *const u8,
    dst: *mut u8,
    label: &str,
) {
    match layout.ownership_kind {
        HewTypeOwnershipKind::Plain => {
            if layout.size > 0 {
                // SAFETY: caller guarantees both blobs are valid for
                // `layout.size` bytes.
                unsafe { ptr::copy_nonoverlapping(src, dst, layout.size) };
            }
        }
        HewTypeOwnershipKind::String | HewTypeOwnershipKind::LayoutManaged => {
            let Some(clone_fn) = layout.clone_fn else {
                abort_layout_clone(format!("{label}: value clone thunk is unavailable"));
            };
            if layout.size > 0 {
                // The map-value clone thunk ABI matches the existing aggregate
                // clone helpers: caller first seeds dst with an exact byte copy
                // so tags/BitCopy fields are present, then the thunk overwrites
                // owned leaves with semantic clones.
                // SAFETY: caller guarantees both blobs are valid for
                // `layout.size` bytes and non-overlapping.
                unsafe { ptr::copy_nonoverlapping(src, dst, layout.size) };
            }
            // SAFETY: descriptor validator/checker ensures `clone_fn` matches
            // the value blob ABI; dst was seeded per thunk contract above.
            let rc = unsafe { clone_fn(src.cast::<c_void>(), dst.cast::<c_void>()) };
            if rc != 0 {
                abort_layout_clone(format!("{label}: value clone thunk returned {rc}"));
            }
        }
        HewTypeOwnershipKind::Bytes => abort_layout_clone(format!(
            "{label}: value ownership_kind=Bytes is not valid for maps"
        )),
    }
}

/// Deep-clone a layout-backed map, duplicating owned slot blobs when the
/// descriptor provides a concrete clone discipline. Layout-managed keys fail
/// closed because `HewMapKeyLayout` has no key clone thunk field.
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
    let entries_align = core::cmp::max(src.key_layout.align, src.val_layout.align);
    if matches!(
        src.key_layout.ownership_kind,
        HewTypeOwnershipKind::LayoutManaged
    ) {
        abort_layout_clone(
            "hew_hashmap_clone_layout: key layout-managed clone thunk is unavailable",
        );
    }
    if matches!(
        src.val_layout.ownership_kind,
        HewTypeOwnershipKind::String | HewTypeOwnershipKind::LayoutManaged
    ) && src.val_layout.clone_fn.is_none()
    {
        abort_layout_clone("hew_hashmap_clone_layout: value clone thunk is unavailable");
    }

    // SAFETY: source map was validated at construction time.
    let cloned_entries = unsafe { alloc_layout_entries(src.cap, src.stride, entries_align) };
    // SAFETY: malloc returns storage suitably aligned for HewLayoutHashMap.
    let cloned: *mut HewLayoutHashMap =
        unsafe { libc::malloc(core::mem::size_of::<HewLayoutHashMap>()).cast() };
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
            clone_layout_key_blob(
                src.key_layout.ownership_kind,
                src_key,
                dst_key,
                src.key_layout.size,
            );
        }
        if src.val_layout.size > 0 {
            // SAFETY: occupied slot has a valid value blob.
            let src_val = unsafe { slot_val(src.entries, idx, src.stride, src.val_offset) };
            // SAFETY: destination slot is in-bounds in the cloned allocation.
            let dst_val = unsafe { slot_val(cloned_entries, idx, src.stride, src.val_offset) };
            // SAFETY: forwarded blob contracts.
            unsafe {
                clone_layout_value_blob(
                    src.val_layout,
                    src_val,
                    dst_val,
                    "hew_hashmap_clone_layout value",
                );
            }
        }
    }

    cloned
}

// ---------------------------------------------------------------------------
// Insert / Get / Contains / Remove / Len (layout-backed)
// ---------------------------------------------------------------------------

/// Insert or overwrite `key -> val`. Returns `true` if a new entry was added,
/// `false` if an existing key was overwritten.
///
/// `val` may be null only when the value layout's `size` is zero (`HashSet`
/// contract); otherwise null aborts fail-closed.
///
/// # String-element ownership: MOVE on ingress (no copy-in)
///
/// For `String` keys/values codegen wires the String-ownership descriptors
/// `hew_layout_key_string` / `hew_layout_val_string`
/// (`hew-codegen-rs/src/llvm.rs`), whose `drop_fn` is the header-aware
/// `hew_layout_string_drop` → `hew_string_drop` (refcount decrement,
/// free-at-zero, static-literal safe). The strings codegen hands to this
/// function are therefore **already header-aware** Hew strings — there is no
/// `strdup` and no headerless map producer anywhere in the map runtime.
///
/// Consequently this function's String contract is an **ownership-transfer
/// MOVE, not a copy-in**: the `ptr::copy_nonoverlapping` of the key/value
/// pointer bits below relocates the sole owner into the map's slot. It is
/// deliberately *not* a `clone` — the caller's string is consumed (its single
/// owning reference now lives in the map), so the map does **not** retain on
/// ingress. The matched release side is symmetric: `hew_hashmap_clone_layout`
/// **retains** each string element (`clone_layout_string_blob` →
/// `hew_string_clone`, refcount bump) and `hew_hashmap_free_layout` /
/// `hew_hashmap_remove_layout` **release** via the descriptor `drop_fn`
/// (refcount decrement, free-at-zero). That clone-retains / drop-releases
/// symmetry is what makes the byte-copy MOVE sound rather than an unpaired
/// aliasing copy (LESSONS `alias-byte-copy-not-semantic-clone`): there is one
/// owner at a time, never two owners of one allocation.
///
/// Because ingress is MOVE, copy-in is **intentionally absent** — adding it
/// would waste a String allocation and (until P3 drop emission lands) orphan
/// the moved-from source, introducing a leak. A future *headerless* map
/// producer (none exists today) would have to either copy-in at the producer
/// or grow a dedicated map copy-in path; this is a fail-closed catalogue note,
/// not a present gap.
///
/// ## Conditional key-consume asymmetry (read this for P3 consuming-call lowering)
///
/// Key disposition is **runtime-conditional on `existed`**, which is why the
/// caller's K ownership cannot be resolved statically:
/// - **Vacant insert** (`!existed`): the caller's K is **MOVED** into the slot
///   — consumed, the map is now its sole owner.
/// - **Overwrite** (`existed`): the stored slot K is the equality-witness and
///   is **reused in place**; the caller's duplicate K is **NOT** consumed here
///   — the caller retains it. Its release is materialised by the codegen
///   conditional drop (`emit_insert_overwrite_key_release` in
///   `hew-codegen-rs/src/llvm.rs`), which branches on this function's `i1`
///   return and frees the caller's duplicate on the overwrite path (issue
///   #2033; see the occupied-slot branch below).
///
/// Since `insert` returns `!existed`, K consumption is decided at runtime; a
/// static null-after-move on the caller's K would be **wrong on the overwrite
/// path**. The codegen materialiser therefore derives the consuming-call
/// ownership model from this per-path contract — a runtime branch on the `i1`
/// return — rather than a uniform null-after-move (LESSONS
/// `raii-null-after-move`).
///
/// # Safety
///
/// `m` must be a valid `HewLayoutHashMap`. `key` must point to a readable
/// blob of the registered key layout. `val` likewise for the value layout
/// (when size > 0).
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_insert_layout(
    m: *mut HewLayoutHashMap,
    key: *const c_void,
    val: *const c_void,
) -> bool {
    // SAFETY: forwarded to the shared `pub` gate; null inputs abort under the
    // workspace `panic = "abort"` profile.
    unsafe { validate_op_inputs(m.cast_const(), key, Some(val)) };

    // Council Rev 1 §7: do NOT hold a Rust reference (`&mut *m`) across the
    // possible `layout_resize` call below. Read every needed scalar / raw
    // pointer via raw projection BEFORE the resize decision. Re-read fields
    // that resize may mutate (`entries`, `cap`) AFTER resize completes.

    // Snapshot layout descriptor fields we need (descriptors are now owned
    // by-value snapshots on the map; W4.001 Stage C0a).
    // SAFETY: m non-null per validate_op_inputs above; reads of the by-value
    // descriptor fields are plain scalar loads.
    let (key_size, val_size, hash_fn_opt, eq_fn_opt, val_drop_fn_opt) = unsafe {
        (
            (*m).key_layout.size,
            (*m).val_layout.size,
            (*m).key_layout.hash_fn,
            (*m).key_layout.eq_fn,
            (*m).val_layout.drop_fn,
        )
    };
    let (Some(hash_fn), Some(eq_fn)) = (hash_fn_opt, eq_fn_opt) else {
        crate::set_last_error(
            "hew_hashmap_insert_layout: hash_fn/eq_fn None (constructor guard violated)",
        );
        std::process::abort();
    };

    // Resize BEFORE probing so the slot we choose lives in the post-resize table.
    // Load factor threshold = 75%.
    // SAFETY: m non-null; raw field reads only — no Rust reference is held
    // across the resize call.
    let (len_now, cap_now) = unsafe { ((*m).len, (*m).cap) };
    if len_now.saturating_add(1) * 100 >= cap_now * LOAD_PCTG {
        // SAFETY: m valid; resize itself observes the Rev 1 §7 discipline.
        unsafe { layout_resize(m) };
    }

    // Re-read post-resize geometry via raw projection.
    // SAFETY: m valid; raw scalar field reads only.
    let (entries, cap, stride, key_offset, val_offset) = unsafe {
        (
            (*m).entries,
            (*m).cap,
            (*m).stride,
            (*m).key_offset,
            (*m).val_offset,
        )
    };

    // SAFETY: thunks valid; key valid per caller contract.
    let (idx, existed) =
        unsafe { layout_probe(entries, cap, stride, key_offset, key, hash_fn, eq_fn) };

    // SAFETY: idx < cap, offsets in-bounds.
    let state_ptr = unsafe { slot_state(entries, idx, stride) };
    // SAFETY: idx < cap.
    let dst_key = unsafe { slot_key(entries, idx, stride, key_offset) };
    // SAFETY: idx < cap.
    let dst_val = unsafe { slot_val(entries, idx, stride, val_offset) };

    // SAFETY: state byte in-bounds.
    unsafe { *state_ptr = OCCUPIED };

    if existed {
        // W4.001 Stage C0a contract (plan rev6 §4 "Acquisition / ownership
        // contract", occupied-slot row + invariant 5: "Overwrite never
        // drops K, always drops old V").
        //
        // 1. The stored slot K stays in place — it is the equality-witness
        //    that the probe found, and re-copying caller-K bytes would leak
        //    the stored K's owned allocation when K is `String` /
        //    `LayoutManaged`. The duplicate K the caller passed is released by
        //    the codegen conditional drop on the overwrite path
        //    (`emit_insert_overwrite_key_release`, branching on this function's
        //    `i1` return; issue #2033).
        // 2. The old V is dropped via `val_layout.drop_fn` before the new V
        //    overwrites it. `drop_fn = None` (Plain ownership) is the
        //    no-op fast path.
        // 3. The stored K's `drop_fn` is NEVER invoked here — the slot K is
        //    reused, not freed.
        if val_size > 0 {
            if let Some(val_drop) = val_drop_fn_opt {
                // SAFETY: dst_val points to the old V blob owned by the map;
                // `drop_fn` is the descriptor-registered owned-V drop thunk and
                // runs the type's drop plan in place (it must not deallocate
                // the slot bytes themselves — the kernel owns slot storage).
                val_drop(dst_val.cast::<c_void>());
            }
        }
        // Raw-copy new V over the (now-dropped) old V slot bytes. For a
        // `String` value this is the ownership-transfer MOVE described in the
        // function header: the new V's sole owner relocates into the slot (no
        // retain — the caller's V is consumed). The old V was already released
        // by `val_drop` above, so the clone-retains / drop-releases symmetry
        // holds and no allocation is leaked or double-freed.
        if val_size > 0 {
            // SAFETY: dst_val and val are valid blobs of `val_size` bytes (val
            // non-null was enforced by validate_op_inputs when val_size > 0).
            unsafe { ptr::copy_nonoverlapping(val.cast::<u8>(), dst_val, val_size) };
        }
    } else {
        // Vacant slot: raw-copy K and V bytes into slot; possession transfers
        // to the map (plan rev6 §4 contract-table vacant-slot row). For
        // `String` K/V these two copies are the ownership-transfer MOVE: the
        // caller's sole-owned, already-header-aware strings are relocated into
        // the slots and consumed (no retain on ingress — see the function
        // header's "MOVE on ingress" contract). The map's later
        // `hew_hashmap_free_layout` / `hew_hashmap_remove_layout` release them
        // through the descriptor `drop_fn`; `hew_hashmap_clone_layout` retains
        // them. On this vacant path the caller's K is CONSUMED (contrast the
        // overwrite path above, where the caller retains its duplicate K).
        // SAFETY: dst_key and key are valid blobs of `key_size` bytes.
        unsafe { ptr::copy_nonoverlapping(key.cast::<u8>(), dst_key, key_size) };
        if val_size > 0 {
            // SAFETY: dst_val and val are valid blobs of `val_size` bytes.
            unsafe { ptr::copy_nonoverlapping(val.cast::<u8>(), dst_val, val_size) };
        }
        // SAFETY: m valid; raw scalar increment.
        unsafe { (*m).len += 1 };
    }
    !existed
}

/// Look up a key. Returns a borrowed pointer to the value blob, or null if the
/// key is absent.
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
/// [`hew_hashmap_contains_key_layout`] instead, which returns a `bool` and
/// avoids the misuse hazard.
///
/// The caller must not free the returned pointer or write through it.
///
/// # Safety
///
/// `m` must be a valid `HewLayoutHashMap`. `key` must point to a valid key blob.
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_get_layout(
    m: *const HewLayoutHashMap,
    key: *const c_void,
) -> *const c_void {
    // SAFETY: shared fail-closed gate; no value pointer involved in lookup.
    unsafe { validate_op_inputs(m, key, None) };
    // SAFETY: m non-null per gate.
    let map = unsafe { &*m };
    if map.cap == 0 || map.len == 0 {
        return ptr::null();
    }
    // W4.001 Stage C0a: descriptors are owned by-value snapshots.
    let kl = &map.key_layout;
    let (Some(hash_fn), Some(eq_fn)) = (kl.hash_fn, kl.eq_fn) else {
        crate::set_last_error(
            "hew_hashmap_get_layout: hash_fn/eq_fn None (constructor guard violated)",
        );
        std::process::abort();
    };
    // SAFETY: layout fields valid.
    let (idx, found) = unsafe {
        layout_probe(
            map.entries,
            map.cap,
            map.stride,
            map.key_offset,
            key,
            hash_fn,
            eq_fn,
        )
    };
    if !found {
        return ptr::null();
    }
    // SAFETY: idx < cap; val_offset valid.
    let val_ptr = unsafe { slot_val(map.entries, idx, map.stride, map.val_offset) };
    val_ptr.cast::<c_void>().cast_const()
}

/// Look up a key and semantic-clone the stored value into caller-provided
/// storage. Returns `true` when the key was found, `false` when absent.
///
/// This is the owned-return counterpart to [`hew_hashmap_get_layout`]. The
/// borrowed getter remains available for predicates and internal probes; Hew
/// `HashMap::get()` uses this entry so `Option<V>` owns an independent `V`.
///
/// # Safety
///
/// `m` must be a valid `HewLayoutHashMap`. `key` must point to a valid key
/// blob. When the map's value size is non-zero, `out` must point to writable
/// storage for exactly `val_layout.size` bytes at `val_layout.align`.
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_get_clone_layout(
    m: *const HewLayoutHashMap,
    key: *const c_void,
    out: *mut c_void,
) -> bool {
    // SAFETY: shared fail-closed gate; no value pointer participates in lookup.
    unsafe { validate_op_inputs(m, key, None) };
    // SAFETY: m non-null per gate.
    let map = unsafe { &*m };
    if map.val_layout.size > 0 && out.is_null() {
        crate::set_last_error("hew_hashmap_get_clone_layout: out is null for non-zero value");
        std::process::abort();
    }
    if map.cap == 0 || map.len == 0 {
        return false;
    }
    let kl = &map.key_layout;
    let (Some(hash_fn), Some(eq_fn)) = (kl.hash_fn, kl.eq_fn) else {
        crate::set_last_error(
            "hew_hashmap_get_clone_layout: hash_fn/eq_fn None (constructor guard violated)",
        );
        std::process::abort();
    };
    // SAFETY: layout fields valid.
    let (idx, found) = unsafe {
        layout_probe(
            map.entries,
            map.cap,
            map.stride,
            map.key_offset,
            key,
            hash_fn,
            eq_fn,
        )
    };
    if !found {
        return false;
    }
    if map.val_layout.size > 0 {
        // SAFETY: idx < cap; val_offset valid.
        let val_ptr = unsafe { slot_val(map.entries, idx, map.stride, map.val_offset) };
        // SAFETY: val_ptr points at the occupied slot value and `out` was
        // validated for this value layout above.
        unsafe {
            clone_layout_value_blob(
                map.val_layout,
                val_ptr,
                out.cast::<u8>(),
                "hew_hashmap_get_clone_layout value",
            );
        }
    }
    true
}

/// Predicate form of `hew_hashmap_get_layout`.
///
/// # Safety
///
/// Same as [`hew_hashmap_get_layout`].
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_contains_key_layout(
    m: *const HewLayoutHashMap,
    key: *const c_void,
) -> bool {
    // SAFETY: forwarded to get_layout which validates inputs.
    !unsafe { hew_hashmap_get_layout(m, key) }.is_null()
}

/// Remove a key. Returns `true` if a matching entry was found and tombstoned,
/// `false` otherwise.
///
/// # Safety
///
/// Same as [`hew_hashmap_get_layout`].
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_remove_layout(
    m: *mut HewLayoutHashMap,
    key: *const c_void,
) -> bool {
    // SAFETY: shared fail-closed gate.
    unsafe { validate_op_inputs(m.cast_const(), key, None) };
    // SAFETY: m non-null per gate.
    let map = unsafe { &mut *m };
    if map.cap == 0 || map.len == 0 {
        return false;
    }
    // W4.001 Stage C0a: descriptors are owned by-value snapshots.
    // Snapshot the drop thunks + scalar layout fields BEFORE taking the
    // mutable handle to the entries (avoids any aliasing between the
    // descriptor read and the slot pointer arithmetic below).
    let (hash_fn_opt, eq_fn_opt, key_drop_fn_opt, val_drop_fn_opt, val_size) = (
        map.key_layout.hash_fn,
        map.key_layout.eq_fn,
        map.key_layout.drop_fn,
        map.val_layout.drop_fn,
        map.val_layout.size,
    );
    let (Some(hash_fn), Some(eq_fn)) = (hash_fn_opt, eq_fn_opt) else {
        crate::set_last_error(
            "hew_hashmap_remove_layout: hash_fn/eq_fn None (constructor guard violated)",
        );
        std::process::abort();
    };
    // SAFETY: layout fields valid.
    let (idx, found) = unsafe {
        layout_probe(
            map.entries,
            map.cap,
            map.stride,
            map.key_offset,
            key,
            hash_fn,
            eq_fn,
        )
    };
    if !found {
        return false;
    }
    // W4.001 Stage C0a (plan rev6 §4 contract-table remove row + invariant
    // 3): drop the stored K + V via the descriptor thunks BEFORE tombstoning.
    // The caller's borrowed lookup K is untouched (kernel never sees it
    // again after `eq_fn` returned true). The drop+tombstone sequence is
    // sync C with no `.await` between (CLAUDE.md §1 — kernel invariant
    // documented in plan §4).
    //
    // SAFETY: idx < cap; offsets in-bounds; slot is OCCUPIED.
    let slot_key_ptr = unsafe { slot_key(map.entries, idx, map.stride, map.key_offset) };
    if let Some(key_drop) = key_drop_fn_opt {
        // SAFETY: slot K is an owned blob registered to the K drop thunk.
        key_drop(slot_key_ptr.cast::<c_void>());
    }
    if val_size > 0 {
        if let Some(val_drop) = val_drop_fn_opt {
            // SAFETY: slot V is an owned blob registered to the V drop thunk;
            // val_size > 0 guarantees there is an actual blob to drop.
            let slot_val_ptr = unsafe { slot_val(map.entries, idx, map.stride, map.val_offset) };
            val_drop(slot_val_ptr.cast::<c_void>());
        }
    }
    // SAFETY: idx < cap.
    let state_ptr = unsafe { slot_state(map.entries, idx, map.stride) };
    // SAFETY: state byte in-bounds.
    unsafe { *state_ptr = TOMBSTONE };
    map.len -= 1;
    true
}

/// Remove a key, MOVING its value out into `out`. Returns `true` and writes
/// exactly `val_layout.size` bytes into `out` when a matching entry is found;
/// returns `false` (leaving `out` untouched) otherwise. This is the
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
/// # Safety
///
/// `m` must be a valid `HewLayoutHashMap`. `key` must point to a valid key
/// blob. When the map's value size is non-zero, `out` must point to writable
/// storage for exactly `val_layout.size` bytes at `val_layout.align`.
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_remove_take_layout(
    m: *mut HewLayoutHashMap,
    key: *const c_void,
    out: *mut c_void,
) -> bool {
    // SAFETY: shared fail-closed gate; no value pointer participates in lookup.
    unsafe { validate_op_inputs(m.cast_const(), key, None) };
    // SAFETY: m non-null per gate.
    let map = unsafe { &mut *m };
    if map.val_layout.size > 0 && out.is_null() {
        crate::set_last_error("hew_hashmap_remove_take_layout: out is null for non-zero value");
        std::process::abort();
    }
    if map.cap == 0 || map.len == 0 {
        return false;
    }
    // Snapshot the drop thunks + scalar layout fields BEFORE taking the mutable
    // handle to the entries (avoids aliasing the descriptor read against the
    // slot pointer arithmetic below). NOTE: the VALUE drop thunk is deliberately
    // NOT snapshotted — the value is moved out, never dropped here.
    let (hash_fn_opt, eq_fn_opt, key_drop_fn_opt, val_size) = (
        map.key_layout.hash_fn,
        map.key_layout.eq_fn,
        map.key_layout.drop_fn,
        map.val_layout.size,
    );
    let (Some(hash_fn), Some(eq_fn)) = (hash_fn_opt, eq_fn_opt) else {
        crate::set_last_error(
            "hew_hashmap_remove_take_layout: hash_fn/eq_fn None (constructor guard violated)",
        );
        std::process::abort();
    };
    // SAFETY: layout fields valid.
    let (idx, found) = unsafe {
        layout_probe(
            map.entries,
            map.cap,
            map.stride,
            map.key_offset,
            key,
            hash_fn,
            eq_fn,
        )
    };
    if !found {
        return false;
    }
    // Drop the KEY (the map owned it), then MOVE the VALUE out (transfer to the
    // caller — NO val_drop), then tombstone + shrink. The drop+move+tombstone
    // sequence is sync C with no `.await` between (kernel invariant).
    //
    // SAFETY: idx < cap; offsets in-bounds; slot is OCCUPIED.
    let slot_key_ptr = unsafe { slot_key(map.entries, idx, map.stride, map.key_offset) };
    if let Some(key_drop) = key_drop_fn_opt {
        // SAFETY: slot K is an owned blob registered to the K drop thunk.
        key_drop(slot_key_ptr.cast::<c_void>());
    }
    if val_size > 0 {
        // SAFETY: idx < cap; val_offset valid.
        let slot_val_ptr = unsafe { slot_val(map.entries, idx, map.stride, map.val_offset) };
        // MOVE-OUT: byte-copy the value into `out`, transferring possession to
        // the caller. NO `val_drop` runs — the caller is now the sole owner.
        // `out` was validated for this value layout above.
        // SAFETY: slot_val_ptr points at the occupied slot value; `out` has
        // room for `val_size` bytes.
        unsafe { core::ptr::copy_nonoverlapping(slot_val_ptr, out.cast::<u8>(), val_size) };
    }
    // SAFETY: idx < cap.
    let state_ptr = unsafe { slot_state(map.entries, idx, map.stride) };
    // SAFETY: state byte in-bounds.
    unsafe { *state_ptr = TOMBSTONE };
    map.len -= 1;
    true
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
pub unsafe extern "C" fn hew_hashmap_free_layout(m: *mut HewLayoutHashMap) {
    if m.is_null() {
        return;
    }
    // SAFETY: m non-null and constructed via hew_hashmap_new_with_layout.
    let map_ref = unsafe { &*m };
    let entries = map_ref.entries;
    let cap = map_ref.cap;
    let stride = map_ref.stride;
    let key_offset = map_ref.key_offset;
    let val_offset = map_ref.val_offset;
    // W4.001 Stage C0a: descriptors are owned by-value snapshots.
    let kl = &map_ref.key_layout;
    let vl = &map_ref.val_layout;
    let entries_align = core::cmp::max(kl.align, vl.align);
    let key_drop_fn_opt = kl.drop_fn;
    let val_drop_fn_opt = vl.drop_fn;
    let val_size = vl.size;

    // W4.001 Stage C0a (plan rev6 §4 contract-table free row + invariant 4):
    // iterate occupied slots, drop K + V on each via the descriptor thunks
    // before deallocating the entries buffer. Tombstoned slots already had
    // their blobs dropped at remove-time and must not be re-dropped.
    //
    // `Plain` ownership descriptors carry `drop_fn = None`; the per-slot loop
    // skips the call and the overall cost collapses to a single pass over
    // the state bytes (≈ one branch-not-taken per slot) plus the eventual
    // dealloc. The constructor's fail-closed check guarantees that owned
    // ownership kinds (`String`/`LayoutManaged`) always have a `drop_fn`,
    // so this loop cannot leak on those paths.
    if entries.is_null() || cap == 0 {
        // Defensive: nothing to iterate. Fall through to dealloc which is
        // itself a no-op on null entries (`dealloc_layout_entries`).
    } else if key_drop_fn_opt.is_some() || (val_size > 0 && val_drop_fn_opt.is_some()) {
        for idx in 0..cap {
            // SAFETY: idx < cap; stride matches allocation.
            let state = unsafe { *slot_state(entries, idx, stride) };
            if state != OCCUPIED {
                continue;
            }
            if let Some(key_drop) = key_drop_fn_opt {
                // SAFETY: occupied slot has a valid K blob at key_offset.
                let slot_key_ptr = unsafe { slot_key(entries, idx, stride, key_offset) };
                key_drop(slot_key_ptr.cast::<c_void>());
            }
            if val_size > 0 {
                if let Some(val_drop) = val_drop_fn_opt {
                    // SAFETY: occupied slot has a valid V blob at val_offset.
                    let slot_val_ptr = unsafe { slot_val(entries, idx, stride, val_offset) };
                    val_drop(slot_val_ptr.cast::<c_void>());
                }
            }
        }
    }

    // SAFETY: entries allocated by alloc_layout_entries with these exact params.
    unsafe { dealloc_layout_entries(entries, cap, stride, entries_align) };

    // SAFETY: m allocated by libc::malloc in hew_hashmap_new_with_layout or clone.
    unsafe {
        ptr::drop_in_place(m);
        libc::free(m.cast());
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
    let key_drop_fn_opt = kl.drop_fn;
    let val_drop_fn_opt = vl.drop_fn;
    let val_size = vl.size;

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
                if val_size > 0 {
                    if let Some(val_drop) = val_drop_fn_opt {
                        // SAFETY: occupied slot has a valid V blob at val_offset.
                        let slot_val_ptr = unsafe { slot_val(entries, idx, stride, val_offset) };
                        val_drop(slot_val_ptr.cast::<c_void>());
                    }
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
// WHY eager Vec (not lazy iterator): the lazy path requires a new Hew iterator
// type, cursor ABI, and changes to `lower_for_iter_desugar`. The eager Vec
// slots directly into the existing `for x in vec` path with no new IR. The
// heap cost of one extra allocation is acceptable for v0.5.
// WHEN obsolete: when a lazy `MapKeys<K>` / `MapValues<V>` iterator type exists.
// WHAT the real solution looks like: impl IntoIterator for HashMap<K,V> with
// cursor-based iteration, wired into lower_for_iter_desugar.

/// Collect all keys of a layout-backed map into a new `HewVec`.
///
/// Returns an eagerly allocated `*mut HewVec` containing one cloned copy of
/// each occupied key blob.  Order is unspecified (reflects slot-walk order).
///
/// # Ownership
///
/// The caller owns the returned `HewVec` and must free it via
/// `hew_vec_free_layout` (Plain keys) or `hew_vec_free` (String keys) when done.
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
        return core::ptr::null_mut();
    }
    // SAFETY: m non-null, validated by gate.
    unsafe { validate_op_map(m) };
    // SAFETY: m non-null and validated.
    let map = unsafe { &*m };

    match map.key_layout.ownership_kind {
        HewTypeOwnershipKind::LayoutManaged => {
            crate::set_last_error(
                "hew_hashmap_keys_layout: LayoutManaged keys not yet supported (W5.011-P2b-maps)",
            );
            std::process::abort();
        }
        HewTypeOwnershipKind::Bytes => {
            // The Bytes kind belongs to the channel/stream element witness;
            // map descriptors never carry it. Fail closed.
            crate::set_last_error("hew_hashmap_keys_layout: ownership_kind=Bytes is not valid");
            std::process::abort();
        }
        HewTypeOwnershipKind::Plain => {
            let type_layout = HewTypeLayout {
                size: map.key_layout.size,
                align: map.key_layout.align,
                ownership_kind: HewTypeOwnershipKind::Plain,
            };
            // SAFETY: non-null layout.
            let vec = unsafe { crate::vec::hew_vec_new_with_layout(&raw const type_layout) };
            if vec.is_null() {
                return core::ptr::null_mut();
            }
            for idx in 0..map.cap {
                // SAFETY: idx < cap, stride matches allocation.
                let state = unsafe { *slot_state(map.entries, idx, map.stride) };
                if state != OCCUPIED {
                    continue;
                }
                // SAFETY: occupied slot has a valid key blob.
                let key_ptr = unsafe { slot_key(map.entries, idx, map.stride, map.key_offset) };
                // SAFETY: key_ptr is valid for key_layout.size bytes; vec was
                // allocated with the matching HewTypeLayout.
                unsafe {
                    crate::vec::hew_vec_push_layout(
                        vec,
                        key_ptr.cast::<c_void>(),
                        &raw const type_layout,
                    );
                }
            }
            vec
        }
        HewTypeOwnershipKind::String => {
            // SAFETY: String Vec constructor.
            let vec = unsafe { crate::vec::hew_vec_new_str() };
            if vec.is_null() {
                return core::ptr::null_mut();
            }
            for idx in 0..map.cap {
                // SAFETY: idx < cap, stride matches allocation.
                let state = unsafe { *slot_state(map.entries, idx, map.stride) };
                if state != OCCUPIED {
                    continue;
                }
                // SAFETY: occupied slot stores a *const c_char in the key blob.
                let key_blob = unsafe { slot_key(map.entries, idx, map.stride, map.key_offset) };
                // SAFETY: blob holds a `*const c_char` (may be null).
                let key_ptr: *const c_char =
                    unsafe { ptr::read_unaligned(key_blob.cast::<*const c_char>()) };
                // hew_vec_push_str makes an independent header-aware copy.
                // SAFETY: key_ptr is a valid C string (or null).
                unsafe { crate::vec::hew_vec_push_str(vec, key_ptr) };
            }
            vec
        }
    }
}

/// Collect all values of a layout-backed map into a new `HewVec`.
///
/// Returns an eagerly allocated `*mut HewVec` containing one cloned copy of
/// each occupied value blob.  Order is unspecified (reflects slot-walk order).
///
/// # Ownership
///
/// The caller owns the returned `HewVec` and must free it via
/// `hew_vec_free_layout` (Plain values) or `hew_vec_free` (String values).
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
        return core::ptr::null_mut();
    }
    // SAFETY: m non-null, validated by gate.
    unsafe { validate_op_map(m) };
    // SAFETY: m non-null and validated.
    let map = unsafe { &*m };

    match map.val_layout.ownership_kind {
        HewTypeOwnershipKind::LayoutManaged => {
            crate::set_last_error(
                "hew_hashmap_values_layout: LayoutManaged values not yet supported (W5.011-P2b-maps)",
            );
            std::process::abort();
        }
        HewTypeOwnershipKind::Bytes => {
            // The Bytes kind belongs to the channel/stream element witness;
            // map descriptors never carry it. Fail closed.
            crate::set_last_error("hew_hashmap_values_layout: ownership_kind=Bytes is not valid");
            std::process::abort();
        }
        HewTypeOwnershipKind::Plain => {
            let type_layout = HewTypeLayout {
                size: map.val_layout.size,
                align: map.val_layout.align,
                ownership_kind: HewTypeOwnershipKind::Plain,
            };
            // SAFETY: non-null layout.
            let vec = unsafe { crate::vec::hew_vec_new_with_layout(&raw const type_layout) };
            if vec.is_null() {
                return core::ptr::null_mut();
            }
            for idx in 0..map.cap {
                // SAFETY: idx < cap, stride matches allocation.
                let state = unsafe { *slot_state(map.entries, idx, map.stride) };
                if state != OCCUPIED {
                    continue;
                }
                // SAFETY: occupied slot has a valid value blob.
                let val_ptr = unsafe { slot_val(map.entries, idx, map.stride, map.val_offset) };
                // SAFETY: val_ptr is valid for val_layout.size bytes; vec was
                // allocated with the matching HewTypeLayout.
                unsafe {
                    crate::vec::hew_vec_push_layout(
                        vec,
                        val_ptr.cast::<c_void>(),
                        &raw const type_layout,
                    );
                }
            }
            vec
        }
        HewTypeOwnershipKind::String => {
            // SAFETY: String Vec constructor.
            let vec = unsafe { crate::vec::hew_vec_new_str() };
            if vec.is_null() {
                return core::ptr::null_mut();
            }
            for idx in 0..map.cap {
                // SAFETY: idx < cap, stride matches allocation.
                let state = unsafe { *slot_state(map.entries, idx, map.stride) };
                if state != OCCUPIED {
                    continue;
                }
                // SAFETY: occupied slot stores a *const c_char in the value blob.
                let val_blob = unsafe { slot_val(map.entries, idx, map.stride, map.val_offset) };
                // SAFETY: blob holds a `*const c_char` (may be null).
                let val_ptr: *const c_char =
                    unsafe { ptr::read_unaligned(val_blob.cast::<*const c_char>()) };
                // hew_vec_push_str makes an independent header-aware copy.
                // SAFETY: val_ptr is a valid C string (or null).
                unsafe { crate::vec::hew_vec_push_str(vec, val_ptr) };
            }
            vec
        }
    }
}
