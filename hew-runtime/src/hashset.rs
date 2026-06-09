//! Hew runtime: `hashset` module.
//!
//! Hash set implementation backed by `HashMap<T, ()>` with C ABI.
//! Uses the existing `HashMap` infrastructure for storage.
//!
//! # Layout-backed `HashSet` (`HewLayoutHashSet`)
//!
//! The lower half of this module (below the existing `HewHashSet` code) adds
//! `HewLayoutHashSet`, a thin wrapper over the C-1b `HewLayoutHashMap` that
//! fixes the value layout to the ZST (`size=0, align=1`) marker. Element
//! identity is entirely delegated to the caller-supplied `HewMapKeyLayout`
//! thunks; no probe logic is duplicated here (LESSONS `collection-traversal-owner` P2).
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

use core::ffi::{c_char, c_void};
use core::ptr;

use hew_cabi::map::{HewMapKeyLayout, HewMapValueLayout};
use hew_cabi::vec::HewTypeOwnershipKind;

use crate::hashmap::{
    hew_hashmap_clear, hew_hashmap_clone_impl, hew_hashmap_contains_key,
    hew_hashmap_contains_key_layout, hew_hashmap_free_impl, hew_hashmap_free_layout,
    hew_hashmap_insert_i64, hew_hashmap_insert_impl, hew_hashmap_insert_layout, hew_hashmap_len,
    hew_hashmap_len_layout, hew_hashmap_new_impl, hew_hashmap_new_with_layout, hew_hashmap_remove,
    hew_hashmap_remove_layout, HewHashMap, HewLayoutHashMap,
};

/// Hash set backed by a `HewHashMap` where values are unused.
#[repr(C)]
#[derive(Debug)]
pub struct HewHashSet {
    /// Underlying hash map (keys are set elements, values are ignored).
    map: *mut HewHashMap,
}

// ---------------------------------------------------------------------------
// Constructor
// ---------------------------------------------------------------------------

/// Create a new, empty `HewHashSet`.
///
/// # Safety
///
/// The returned pointer must eventually be freed with [`hew_hashset_free`].
#[no_mangle]
pub unsafe extern "C" fn hew_hashset_new() -> *mut HewHashSet {
    // SAFETY: allocating with libc::malloc.
    unsafe {
        let set: *mut HewHashSet = libc::malloc(core::mem::size_of::<HewHashSet>()).cast();
        if set.is_null() {
            libc::abort();
        }
        (*set).map = hew_hashmap_new_impl();
        set
    }
}

// ---------------------------------------------------------------------------
// Insert (returns true if value was newly inserted)
// ---------------------------------------------------------------------------

/// Insert an `i64` value into the set.
///
/// Returns `true` if the value was newly inserted, `false` if it was already present.
///
/// # Safety
///
/// `set` must be a valid `HewHashSet` pointer.
#[expect(
    clippy::similar_names,
    reason = "key_str and key_cstr are related but distinct"
)]
#[no_mangle]
pub unsafe extern "C" fn hew_hashset_insert_int(set: *mut HewHashSet, value: i64) -> bool {
    // SAFETY: caller guarantees `set` is valid.
    unsafe {
        let map = (*set).map;
        // Convert i64 to string key for HashMap storage
        let key_str = format!("{value}\0");
        let key_cstr = key_str.as_ptr().cast::<c_char>();

        let was_present = hew_hashmap_contains_key(map, key_cstr);
        if !was_present {
            hew_hashmap_insert_i64(map, key_cstr, value);
        }
        !was_present
    }
}

/// Insert a string value into the set.
///
/// Returns `true` if the value was newly inserted, `false` if it was already present.
///
/// # Safety
///
/// `set` must be a valid `HewHashSet` pointer. `value` must be a valid C string.
#[no_mangle]
pub unsafe extern "C" fn hew_hashset_insert_string(
    set: *mut HewHashSet,
    value: *const c_char,
) -> bool {
    // SAFETY: caller guarantees `set` and `value` are valid.
    unsafe {
        let map = (*set).map;
        let was_present = hew_hashmap_contains_key(map, value);
        if !was_present {
            // Use the string itself as the key, with a dummy value
            hew_hashmap_insert_impl(map, value, 0, ptr::null());
        }
        !was_present
    }
}

// ---------------------------------------------------------------------------
// Contains
// ---------------------------------------------------------------------------

/// Check if the set contains an `i64` value.
///
/// # Safety
///
/// `set` must be a valid `HewHashSet` pointer.
#[expect(
    clippy::similar_names,
    reason = "key_str and key_cstr are related but distinct"
)]
#[no_mangle]
pub unsafe extern "C" fn hew_hashset_contains_int(set: *mut HewHashSet, value: i64) -> bool {
    // SAFETY: caller guarantees `set` is valid.
    unsafe {
        let map = (*set).map;
        let key_str = format!("{value}\0");
        let key_cstr = key_str.as_ptr().cast::<c_char>();
        hew_hashmap_contains_key(map, key_cstr)
    }
}

/// Check if the set contains a string value.
///
/// # Safety
///
/// `set` must be a valid `HewHashSet` pointer. `value` must be a valid C string.
#[no_mangle]
pub unsafe extern "C" fn hew_hashset_contains_string(
    set: *mut HewHashSet,
    value: *const c_char,
) -> bool {
    // SAFETY: caller guarantees `set` and `value` are valid.
    unsafe {
        let map = (*set).map;
        hew_hashmap_contains_key(map, value)
    }
}

// ---------------------------------------------------------------------------
// Remove
// ---------------------------------------------------------------------------

/// Remove an `i64` value from the set.
///
/// Returns `true` if the value was present and removed.
///
/// # Safety
///
/// `set` must be a valid `HewHashSet` pointer.
#[expect(
    clippy::similar_names,
    reason = "key_str and key_cstr are related but distinct"
)]
#[no_mangle]
pub unsafe extern "C" fn hew_hashset_remove_int(set: *mut HewHashSet, value: i64) -> bool {
    // SAFETY: caller guarantees `set` is valid.
    unsafe {
        let map = (*set).map;
        let key_str = format!("{value}\0");
        let key_cstr = key_str.as_ptr().cast::<c_char>();
        hew_hashmap_remove(map, key_cstr)
    }
}

/// Remove a string value from the set.
///
/// Returns `true` if the value was present and removed.
///
/// # Safety
///
/// `set` must be a valid `HewHashSet` pointer. `value` must be a valid C string.
#[no_mangle]
pub unsafe extern "C" fn hew_hashset_remove_string(
    set: *mut HewHashSet,
    value: *const c_char,
) -> bool {
    // SAFETY: caller guarantees `set` and `value` are valid.
    unsafe {
        let map = (*set).map;
        hew_hashmap_remove(map, value)
    }
}

// ---------------------------------------------------------------------------
// Queries
// ---------------------------------------------------------------------------

/// Return the number of elements in the set.
///
/// # Safety
///
/// `set` must be a valid `HewHashSet` pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_hashset_len(set: *mut HewHashSet) -> i64 {
    // SAFETY: caller guarantees `set` is valid.
    unsafe {
        let map = (*set).map;
        hew_hashmap_len(map)
    }
}

/// Return `true` if the set has no elements.
///
/// # Safety
///
/// `set` must be a valid `HewHashSet` pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_hashset_is_empty(set: *mut HewHashSet) -> bool {
    // SAFETY: caller guarantees `set` is valid.
    unsafe { hew_hashset_len(set) == 0 }
}

/// Remove all elements from the set.
///
/// # Safety
///
/// `set` must be a valid `HewHashSet` pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_hashset_clear(set: *mut HewHashSet) {
    // SAFETY: caller guarantees `set` is valid.
    unsafe {
        let map = (*set).map;
        hew_hashmap_clear(map);
    }
}

/// Deep-clone the set and its underlying storage.
///
/// # Safety
///
/// `set` must be a valid `HewHashSet` pointer (or null, which returns null).
/// The returned pointer must eventually be freed with [`hew_hashset_free`].
#[no_mangle]
pub unsafe extern "C" fn hew_hashset_clone(set: *const HewHashSet) -> *mut HewHashSet {
    cabi_guard!(set.is_null(), ptr::null_mut());
    // SAFETY: caller guarantees `set` is valid.
    unsafe {
        let cloned: *mut HewHashSet = libc::malloc(core::mem::size_of::<HewHashSet>()).cast();
        if cloned.is_null() {
            libc::abort();
        }
        (*cloned).map = hew_hashmap_clone_impl((*set).map);
        if (*cloned).map.is_null() {
            libc::free(cloned.cast());
            libc::abort();
        }
        cloned
    }
}

// ---------------------------------------------------------------------------
// Free
// ---------------------------------------------------------------------------

/// Free the set and its underlying storage.
///
/// # Safety
///
/// `set` must be a valid `HewHashSet` pointer (or null). After this call, `set` is
/// invalid.
#[no_mangle]
pub unsafe extern "C" fn hew_hashset_free(set: *mut HewHashSet) {
    // SAFETY: caller guarantees `set` was allocated with malloc (or is null).
    unsafe {
        cabi_guard!(set.is_null());
        hew_hashmap_free_impl((*set).map);
        libc::free(set.cast());
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CString;

    #[test]
    fn test_hashset_new_and_len() {
        // SAFETY: FFI calls use valid set pointer returned by hew_hashset_new.
        unsafe {
            let s = hew_hashset_new();
            assert!(!s.is_null());
            assert_eq!(hew_hashset_len(s), 0);
            assert!(hew_hashset_is_empty(s));
            hew_hashset_free(s);
        }
    }

    #[test]
    fn test_hashset_insert_int() {
        // SAFETY: FFI calls use valid set pointer returned by hew_hashset_new.
        unsafe {
            let s = hew_hashset_new();
            assert!(hew_hashset_insert_int(s, 1));
            assert!(hew_hashset_insert_int(s, 2));
            assert!(!hew_hashset_insert_int(s, 1)); // duplicate
            assert_eq!(hew_hashset_len(s), 2);
            hew_hashset_free(s);
        }
    }

    #[test]
    fn test_hashset_contains_int() {
        // SAFETY: FFI calls use valid set pointer returned by hew_hashset_new.
        unsafe {
            let s = hew_hashset_new();
            hew_hashset_insert_int(s, 42);
            assert!(hew_hashset_contains_int(s, 42));
            assert!(!hew_hashset_contains_int(s, 99));
            hew_hashset_free(s);
        }
    }

    #[test]
    fn test_hashset_remove_int() {
        // SAFETY: FFI calls use valid set pointer returned by hew_hashset_new.
        unsafe {
            let s = hew_hashset_new();
            hew_hashset_insert_int(s, 10);
            assert!(hew_hashset_remove_int(s, 10));
            assert!(!hew_hashset_contains_int(s, 10));
            assert_eq!(hew_hashset_len(s), 0);
            hew_hashset_free(s);
        }
    }

    #[test]
    fn test_hashset_insert_string() {
        // SAFETY: FFI calls use valid set pointer and valid C strings.
        unsafe {
            let s = hew_hashset_new();
            let hello = CString::new("hello").unwrap();
            let world = CString::new("world").unwrap();
            assert!(hew_hashset_insert_string(s, hello.as_ptr()));
            assert!(hew_hashset_insert_string(s, world.as_ptr()));
            assert!(!hew_hashset_insert_string(s, hello.as_ptr())); // duplicate
            assert_eq!(hew_hashset_len(s), 2);
            hew_hashset_free(s);
        }
    }

    #[test]
    fn test_hashset_contains_string() {
        // SAFETY: FFI calls use valid set pointer and valid C strings.
        unsafe {
            let s = hew_hashset_new();
            let val = CString::new("test").unwrap();
            let missing = CString::new("missing").unwrap();
            hew_hashset_insert_string(s, val.as_ptr());
            assert!(hew_hashset_contains_string(s, val.as_ptr()));
            assert!(!hew_hashset_contains_string(s, missing.as_ptr()));
            hew_hashset_free(s);
        }
    }

    #[test]
    fn test_hashset_remove_string() {
        // SAFETY: FFI calls use valid set pointer and valid C strings.
        unsafe {
            let s = hew_hashset_new();
            let val = CString::new("remove_me").unwrap();
            hew_hashset_insert_string(s, val.as_ptr());
            assert!(hew_hashset_remove_string(s, val.as_ptr()));
            assert!(!hew_hashset_contains_string(s, val.as_ptr()));
            assert_eq!(hew_hashset_len(s), 0);
            hew_hashset_free(s);
        }
    }

    #[test]
    fn test_hashset_clear() {
        // SAFETY: FFI calls use valid set pointer returned by hew_hashset_new.
        unsafe {
            let s = hew_hashset_new();
            hew_hashset_insert_int(s, 1);
            hew_hashset_insert_int(s, 2);
            hew_hashset_insert_int(s, 3);
            hew_hashset_clear(s);
            assert_eq!(hew_hashset_len(s), 0);
            assert!(hew_hashset_is_empty(s));
            hew_hashset_free(s);
        }
    }

    #[test]
    fn test_hashset_clone_ints() {
        // SAFETY: FFI calls use valid set pointers returned by Hew runtime constructors.
        unsafe {
            let s = hew_hashset_new();
            hew_hashset_insert_int(s, 7);
            hew_hashset_insert_int(s, 9);

            let cloned = hew_hashset_clone(s);
            assert!(!cloned.is_null());
            hew_hashset_free(s);

            assert_eq!(hew_hashset_len(cloned), 2);
            assert!(hew_hashset_contains_int(cloned, 7));
            assert!(hew_hashset_contains_int(cloned, 9));
            hew_hashset_free(cloned);
        }
    }

    #[test]
    fn test_hashset_clone_strings() {
        // SAFETY: FFI calls use valid set pointers and valid C strings.
        unsafe {
            let s = hew_hashset_new();
            let hello = CString::new("hello").unwrap();
            let world = CString::new("world").unwrap();
            hew_hashset_insert_string(s, hello.as_ptr());
            hew_hashset_insert_string(s, world.as_ptr());

            let cloned = hew_hashset_clone(s);
            assert!(!cloned.is_null());
            hew_hashset_free(s);

            assert_eq!(hew_hashset_len(cloned), 2);
            assert!(hew_hashset_contains_string(cloned, hello.as_ptr()));
            assert!(hew_hashset_contains_string(cloned, world.as_ptr()));
            hew_hashset_free(cloned);
        }
    }

    #[test]
    fn test_hashset_free_null() {
        // SAFETY: Null is explicitly handled by hew_hashset_free.
        unsafe { hew_hashset_free(core::ptr::null_mut()) };
    }

    #[test]
    fn test_hashset_many_elements() {
        // SAFETY: FFI calls use valid set pointer returned by hew_hashset_new.
        unsafe {
            let s = hew_hashset_new();
            for i in 0..100 {
                assert!(hew_hashset_insert_int(s, i));
            }
            assert_eq!(hew_hashset_len(s), 100);
            for i in 0..100 {
                assert!(hew_hashset_contains_int(s, i));
            }
            hew_hashset_free(s);
        }
    }
}

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
// WASM-TODO(#1451): hew_hashset_new_with_layout not yet ported to wasm32
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
/// `set` and `elem` must be non-null; either being null panics fail-closed
/// (LESSONS `boundary-fail-closed` P0).
///
/// # Safety
///
/// `set` must be a valid `HewLayoutHashSet` pointer obtained from
/// `hew_hashset_new_with_layout`.  `elem` must point to a readable blob whose
/// size and alignment match the `elem_layout` registered at construction.
// WASM-TODO(#1451): hew_hashset_insert_layout not yet ported to wasm32
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
// WASM-TODO(#1451): hew_hashset_contains_layout not yet ported to wasm32
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
// WASM-TODO(#1451): hew_hashset_remove_layout not yet ported to wasm32
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
// WASM-TODO(#1451): hew_hashset_len_layout not yet ported to wasm32
#[no_mangle]
pub unsafe extern "C" fn hew_hashset_len_layout(set: *const HewLayoutHashSet) -> i64 {
    // SAFETY: shared validator; panics if set is null.
    unsafe { validate_set_op(set) };
    // SAFETY: set non-null per validator; map pointer valid.
    unsafe { hew_hashmap_len_layout((*set).map) }
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
// WASM-TODO(#1451): hew_hashset_free_layout not yet ported to wasm32
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
