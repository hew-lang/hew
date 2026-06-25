//! Hew runtime: `hashset` module.
//!
//! Layout-backed hash set (`HewLayoutHashSet`): a thin wrapper over the C-1b
//! `HewLayoutHashMap` that fixes the value layout to the ZST (`size=0, align=1`)
//! marker. Element identity is entirely delegated to the caller-supplied
//! `HewMapKeyLayout` thunks; no probe logic is duplicated here
//! (LESSONS `collection-traversal-owner` P2).
//!
//! The legacy untyped `HewHashSet` (backed by `HewHashMap<string-key, ()>`) was
//! retired once `collection_layout_witness` became the sole codegen authority for
//! collection clone/drop (W5.001 / W5.003). All codegen paths now route
//! exclusively through the `_layout` family.
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

use core::ffi::c_void;
use core::ptr;

use hew_cabi::map::{HewMapKeyLayout, HewMapValueLayout};
use hew_cabi::vec::HewTypeOwnershipKind;

use crate::hashmap::{
    hew_hashmap_clone_layout, hew_hashmap_contains_key_layout, hew_hashmap_free_layout,
    hew_hashmap_insert_layout, hew_hashmap_keys_layout, hew_hashmap_len_layout,
    hew_hashmap_new_with_layout, hew_hashmap_remove_layout, HewLayoutHashMap,
};
use crate::vec::HewVec;

// RETIRED: HewHashSet (untyped, string-key wrapper over HewHashMap) was deleted
// in W5.003. The symbols hew_hashset_new / _insert_int / _insert_string /
// _contains_int / _contains_string / _remove_int / _remove_string / _len /
// _is_empty / _clear / _clone / _free are no longer exported. Use the
// _layout family below for all new code.

// ===========================================================================
// Layout-backed HashSet (`HewLayoutHashSet`) — C-1c (W3.003 slice C-1c)
// ===========================================================================
//
// Thin wrapper over `HewLayoutHashMap` (C-1b) that fixes the value layout to
// the ZST (`size=0, align=1`) marker. All probe, resize, and hash/eq logic
// is delegated entirely to the C-1b ABI — no duplication here
// (LESSONS `collection-traversal-owner` P2).
//
// Ownership contract (LESSONS `ffi-ownership-contracts` P0):
//   - `elem_layout` (`HewMapKeyLayout` pointer) is **caller-owned** and must
//     remain valid for the entire lifetime of the set. The underlying
//     `HewLayoutHashMap` stores a raw pointer to it and reads `hash_fn`,
//     `eq_fn`, `size`, and `align` on every probe and on free. The caller
//     must not free `elem_layout` before calling `hew_hashset_free_layout`.
//   - `HewLayoutHashSet` itself is heap-allocated by
//     `hew_hashset_new_with_layout` and must be freed with
//     `hew_hashset_free_layout`. No other allocator is permissible.
//   - The set is single-owner / single-threaded at the boundary; the type
//     contains no internal synchronisation. Callers needing shared access
//     must add external synchronisation above this surface.
//
// Fail-closed contract (LESSONS `boundary-fail-closed` P0):
//   - `hew_hashset_new_with_layout`: null `elem_layout` → `abort()`.
//   - `hew_hashset_insert_layout` / `_contains_layout` / `_remove_layout` /
//     `_len_layout`: null `set` or null `elem` → `panic!()` (under the
//     workspace `panic = "abort"` profile, every panic is a process abort;
//     tests observe this via the `pub` validator helpers from `should_panic`
//     tests, consistent with the C-1b pattern in `ffi_boundary_layout_hashmap.rs`).
//   - `hew_hashset_free_layout`: null `set` → documented no-op.

/// ZST value layout shared by all `HewLayoutHashSet` instances.
///
/// `size=0, align=1` is the canonical ZST marker accepted by the C-1b
/// `validate_val_layout` gate for hash-set use.
///
/// # Why `static`
///
/// `hew_hashmap_new_with_layout` stores the raw `val_layout` pointer inside
/// the `HewLayoutHashMap` and dereferences it on every probe and on free.
/// The pointer must therefore remain valid for the entire lifetime of the map.
/// A `static` binding provides program-lifetime validity without any
/// heap allocation.
static VALUE_LAYOUT: HewMapValueLayout = HewMapValueLayout {
    size: 0,
    align: 1,
    ownership_kind: HewTypeOwnershipKind::Plain,
    drop_fn: None,
    clone_fn: None,
};

/// Layout-backed hash set: thin wrapper over [`HewLayoutHashMap`] that fixes
/// the value layout to the ZST (`size=0, align=1`) marker understood by the
/// C-1b ABI.
///
/// # Ownership contract (LESSONS `ffi-ownership-contracts` P0)
///
/// - **`elem_layout`** (the `HewMapKeyLayout` pointer passed to
///   `hew_hashset_new_with_layout`) is **caller-owned** and must remain valid
///   for the entire lifetime of the set. The underlying map stores a raw
///   pointer to it and reads `hash_fn`, `eq_fn`, `size`, and `align` on
///   every probe and on free.
/// - **`HewLayoutHashSet`** is heap-allocated by
///   `hew_hashset_new_with_layout` and must be freed with
///   `hew_hashset_free_layout`. No other allocator is permissible.
///
/// # Concurrency
///
/// No internal synchronisation. All operations require exclusive access by the
/// caller for the duration of the call.
#[repr(C)]
#[derive(Debug)]
pub struct HewLayoutHashSet {
    /// Underlying layout-backed map. Elements are keys; values are ZST.
    map: *mut HewLayoutHashMap,
}

// ---------------------------------------------------------------------------
// Fail-closed validator helpers (pub so `should_panic` tests can drive them
// directly — consistent with the `validate_op_inputs` / `validate_op_map`
// pattern from C-1b in `hashmap.rs`).
// ---------------------------------------------------------------------------

/// Fail-closed null-set check for single-pointer `HewLayoutHashSet` operations
/// (e.g. `len_layout`).
///
/// # Panics
///
/// Panics if `set` is null.
///
/// # Safety
///
/// Performs only a null check; safe to call with any `*const`.
pub unsafe fn validate_set_op(set: *const HewLayoutHashSet) {
    if set.is_null() {
        crate::set_last_error("HewLayoutHashSet op: set is null");
        panic!("HewLayoutHashSet op: set is null");
    }
}

/// Fail-closed null-set and null-elem check for element operations
/// (`insert_layout` / `contains_layout` / `remove_layout`).
///
/// # Panics
///
/// Panics if `set` or `elem` is null.
///
/// # Safety
///
/// Performs only null checks; safe to call with any pointers.
pub unsafe fn validate_set_op_elem(set: *const HewLayoutHashSet, elem: *const c_void) {
    if set.is_null() {
        crate::set_last_error("HewLayoutHashSet op: set is null");
        panic!("HewLayoutHashSet op: set is null");
    }
    if elem.is_null() {
        crate::set_last_error("HewLayoutHashSet op: elem is null");
        panic!("HewLayoutHashSet op: elem is null");
    }
}

// ---------------------------------------------------------------------------
// Constructor
// ---------------------------------------------------------------------------

/// Create a new, empty `HewLayoutHashSet` with the given element layout.
///
/// `elem_layout` must be non-null; a null pointer aborts fail-closed via
/// `std::process::abort()`.  The caller must keep `elem_layout` alive for the
/// entire lifetime of the returned set (LESSONS `ffi-ownership-contracts` P0).
///
/// Additional layout invariants validated by the underlying
/// `hew_hashmap_new_with_layout` call (non-zero size, power-of-two align,
/// non-`LayoutManaged` ownership, non-`None` thunks) also apply; violations
/// abort fail-closed via the C-1b validator gate.
///
/// # Returns
///
/// A heap-allocated `HewLayoutHashSet`. Free with [`hew_hashset_free_layout`].
///
/// # Safety
///
/// `elem_layout` must be a valid, non-null pointer to a `HewMapKeyLayout`
/// satisfying the C-1b validator invariants.
#[no_mangle]
pub unsafe extern "C" fn hew_hashset_new_with_layout(
    elem_layout: *const HewMapKeyLayout,
) -> *mut HewLayoutHashSet {
    if elem_layout.is_null() {
        crate::set_last_error("hew_hashset_new_with_layout: elem_layout is null");
        // SAFETY: extern "C" cannot unwind; abort the process. The null
        // elem_layout case is covered by `should_panic` tests driving
        // `validate_key_layout` directly (the C-1b pattern).
        std::process::abort();
    }
    // SAFETY: elem_layout non-null (checked above); VALUE_LAYOUT is 'static and
    // satisfies the ZST contract (size=0, align=1, Plain).
    let map = unsafe { hew_hashmap_new_with_layout(elem_layout, &raw const VALUE_LAYOUT) };
    // hew_hashmap_new_with_layout aborts on OOM and on layout violations, so a
    // null return here is impossible under the current implementation; guard
    // defensively anyway.
    if map.is_null() {
        crate::set_last_error("hew_hashset_new_with_layout: inner map allocation failed");
        std::process::abort();
    }
    // SAFETY: allocating with libc::malloc for the outer HewLayoutHashSet struct.
    let set: *mut HewLayoutHashSet =
        unsafe { libc::malloc(core::mem::size_of::<HewLayoutHashSet>()).cast() };
    if set.is_null() {
        // Free the successfully-allocated inner map before aborting.
        // SAFETY: map was returned by hew_hashmap_new_with_layout.
        unsafe { hew_hashmap_free_layout(map) };
        crate::set_last_error("hew_hashset_new_with_layout: struct allocation failed");
        std::process::abort();
    }
    // SAFETY: set is a fresh libc::malloc'd allocation sized for HewLayoutHashSet.
    unsafe { (*set).map = map };
    set
}

// ---------------------------------------------------------------------------
// Insert
// ---------------------------------------------------------------------------

/// Insert an element into the set.
///
/// Returns `true` if the element was newly inserted, `false` if already present.
///
/// # String-element ownership: inherits the map key MOVE contract
///
/// A `HashSet<T>` is a thin wrapper whose element *is* the underlying map key
/// (the value layout is the ZST marker). It therefore inherits
/// `hew_hashmap_insert_layout`'s String-element contract verbatim: for a
/// `String` element codegen wires the header-aware `hew_layout_key_string`
/// descriptor, and ingress is an ownership-transfer **MOVE** — the caller's
/// sole-owned, already-header-aware string is relocated into the slot and
/// consumed (no retain, no `strdup`). On the vacant path the element is
/// consumed; on the already-present path the stored element is reused and the
/// caller's duplicate is NOT consumed here (the map's conditional-key
/// asymmetry — see `hew_hashmap_insert_layout`). The matched release side is
/// `hew_hashset_clone_layout` (retain via `hew_string_clone`) /
/// `hew_hashset_free_layout` / `hew_hashset_remove_layout` (release via the
/// descriptor `drop_fn`), preserving the clone-retains / drop-releases
/// symmetry (LESSONS `alias-byte-copy-not-semantic-clone`).
///
/// `set` and `elem` must be non-null; either being null panics fail-closed
/// (LESSONS `boundary-fail-closed` P0).
///
/// # Safety
///
/// `set` must be a valid `HewLayoutHashSet` pointer obtained from
/// `hew_hashset_new_with_layout`.  `elem` must point to a readable blob whose
/// size and alignment match the `elem_layout` registered at construction.
#[no_mangle]
pub unsafe extern "C" fn hew_hashset_insert_layout(
    set: *mut HewLayoutHashSet,
    elem: *const c_void,
) -> bool {
    // SAFETY: shared validator; panics on null set or null elem.
    unsafe { validate_set_op_elem(set.cast_const(), elem) };
    // SAFETY: set non-null per validator; map is a valid HewLayoutHashMap pointer.
    // ZST value contract: pass ptr::null() — validate_op_inputs inside
    // hew_hashmap_insert_layout allows null val when val_size == 0.
    unsafe { hew_hashmap_insert_layout((*set).map, elem, ptr::null()) }
}

// ---------------------------------------------------------------------------
// Contains
// ---------------------------------------------------------------------------

/// Check whether the set contains an element.
///
/// `set` and `elem` must be non-null.
///
/// # Safety
///
/// Same as [`hew_hashset_insert_layout`].
#[no_mangle]
pub unsafe extern "C" fn hew_hashset_contains_layout(
    set: *const HewLayoutHashSet,
    elem: *const c_void,
) -> bool {
    // SAFETY: shared validator; panics on null set or null elem.
    unsafe { validate_set_op_elem(set, elem) };
    // SAFETY: set non-null per validator; map pointer valid.
    unsafe { hew_hashmap_contains_key_layout((*set).map, elem) }
}

// ---------------------------------------------------------------------------
// Remove
// ---------------------------------------------------------------------------

/// Remove an element from the set.
///
/// Returns `true` if the element was present and removed, `false` otherwise.
///
/// `set` and `elem` must be non-null.
///
/// # Safety
///
/// Same as [`hew_hashset_insert_layout`].
#[no_mangle]
pub unsafe extern "C" fn hew_hashset_remove_layout(
    set: *mut HewLayoutHashSet,
    elem: *const c_void,
) -> bool {
    // SAFETY: shared validator; panics on null set or null elem.
    unsafe { validate_set_op_elem(set.cast_const(), elem) };
    // SAFETY: set non-null per validator; map pointer valid.
    unsafe { hew_hashmap_remove_layout((*set).map, elem) }
}

// ---------------------------------------------------------------------------
// Len
// ---------------------------------------------------------------------------

/// Return the number of elements in the set.
///
/// `set` must be non-null.
///
/// # Safety
///
/// `set` must be a valid `HewLayoutHashSet` pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_hashset_len_layout(set: *const HewLayoutHashSet) -> i64 {
    // SAFETY: shared validator; panics if set is null.
    unsafe { validate_set_op(set) };
    // SAFETY: set non-null per validator; map pointer valid.
    unsafe { hew_hashmap_len_layout((*set).map) }
}

/// Return `true` when the layout-backed set has no elements.
///
/// `set` must be non-null.
///
/// # Safety
///
/// `set` must be a valid `HewLayoutHashSet` pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_hashset_is_empty_layout(set: *const HewLayoutHashSet) -> bool {
    // SAFETY: hew_hashset_len_layout performs the null/handle validation.
    unsafe { hew_hashset_len_layout(set) == 0 }
}

/// Collect all elements of a layout-backed set into a new `HewVec`.
///
/// Elements are the keys of the inner map, so this delegates to
/// [`hew_hashmap_keys_layout`], which clones each element into the owned Vec
/// using the element layout's clone discipline (the same fresh-owner
/// clone-on-read the map accessor establishes). The caller owns the returned
/// `HewVec`; order is unspecified (slot-walk order). This is the projection the
/// `for x in s` desugar snapshots into a `VecIter` cursor — each yielded element
/// is independently droppable and the source set is unchanged.
///
/// A null `set` returns null fail-closed.
///
/// # Safety
///
/// `set` must have been returned by [`hew_hashset_new_with_layout`] (or be
/// null). The returned pointer must eventually be freed via the matching Vec
/// free entry for the element class.
#[no_mangle]
pub unsafe extern "C" fn hew_hashset_to_vec_layout(set: *const HewLayoutHashSet) -> *mut HewVec {
    if set.is_null() {
        return core::ptr::null_mut();
    }
    // SAFETY: set non-null per the gate; map was constructed via
    // hew_hashmap_new_with_layout, so keys_layout reads its occupied slots.
    unsafe {
        validate_set_op(set);
        hew_hashmap_keys_layout((*set).map)
    }
}

// ---------------------------------------------------------------------------
// Clone (layout-backed)
// ---------------------------------------------------------------------------

/// Deep-clone a layout-backed set and its underlying storage.
///
/// Operates on the layout-keyed [`HewLayoutHashSet`] substrate produced by
/// [`hew_hashset_new_with_layout`]: it allocates the outer wrapper and
/// delegates the inner map to [`hew_hashmap_clone_layout`], which honours the
/// element layout's clone discipline and fails closed on unsupported
/// layout-managed key paths.
///
/// Pairs with [`hew_hashset_free_layout`]. Using the retired legacy
/// `hew_hashset_clone` against a `HewLayoutHashSet*` would reinterpret the
/// 16-byte-stride layout entries as 48-byte `HewMapEntry` records (W4.045
/// use-after-free, `lifecycle-symmetry` / `codegen-abi-authority` P0) —
/// that symbol is no longer exported (W5.003).
///
/// # Safety
///
/// `set` must have been returned by [`hew_hashset_new_with_layout`] (or be
/// null, which returns null). The returned pointer must eventually be freed
/// with [`hew_hashset_free_layout`].
#[no_mangle]
pub unsafe extern "C" fn hew_hashset_clone_layout(
    set: *const HewLayoutHashSet,
) -> *mut HewLayoutHashSet {
    if set.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: set non-null; map was constructed via hew_hashmap_new_with_layout.
    let cloned_map = unsafe { hew_hashmap_clone_layout((*set).map) };
    // hew_hashmap_clone_layout aborts on OOM and on unsupported layout-managed
    // key paths, and returns null only for a null input. The inner map of a
    // valid set is never null, so a null here is impossible under the current
    // implementation; guard defensively anyway (fail-closed).
    if cloned_map.is_null() {
        crate::set_last_error("hew_hashset_clone_layout: inner map clone failed");
        std::process::abort();
    }
    // SAFETY: allocating with libc::malloc for the outer HewLayoutHashSet struct,
    // matching the allocator used by hew_hashset_new_with_layout.
    let cloned: *mut HewLayoutHashSet =
        unsafe { libc::malloc(core::mem::size_of::<HewLayoutHashSet>()).cast() };
    if cloned.is_null() {
        // Free the successfully-cloned inner map before aborting.
        // SAFETY: cloned_map was returned by hew_hashmap_clone_layout.
        unsafe { hew_hashmap_free_layout(cloned_map) };
        crate::set_last_error("hew_hashset_clone_layout: struct allocation failed");
        std::process::abort();
    }
    // SAFETY: cloned is a fresh libc::malloc'd allocation sized for HewLayoutHashSet.
    unsafe { (*cloned).map = cloned_map };
    cloned
}

// ---------------------------------------------------------------------------
// Free
// ---------------------------------------------------------------------------

/// Free the set and its underlying storage.
///
/// A null `set` is a documented no-op (LESSONS `boundary-fail-closed` P0:
/// `free(null)` is the only permitted silent-return shape).
///
/// # Safety
///
/// `set` must have been returned by [`hew_hashset_new_with_layout`] (or be
/// null). After this call, `set` is invalid and must not be used.
#[no_mangle]
pub unsafe extern "C" fn hew_hashset_free_layout(set: *mut HewLayoutHashSet) {
    if set.is_null() {
        return;
    }
    // SAFETY: set non-null; map was constructed via hew_hashmap_new_with_layout.
    unsafe { hew_hashmap_free_layout((*set).map) };
    // SAFETY: set was allocated with libc::malloc in hew_hashset_new_with_layout.
    unsafe { libc::free(set.cast()) };
}
