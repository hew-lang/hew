//! Non-vacuous refcount-ladder tests for the header-aware `String`-element
//! ownership contract of `HewLayoutHashMap` / `HewLayoutHashSet`.
//!
//! These tests pin the W5.011 P2b-maps decision: map string ingress is an
//! ownership-transfer **MOVE** of an already-header-aware string (no copy-in,
//! no retain on insert), and the matched release side is
//! `*_clone_layout` (retain via `hew_string_clone`) /
//! `*_free_layout` / `*_remove_layout` (release via the descriptor `drop_fn`).
//!
//! They are deliberately **non-vacuous** (LESSONS
//! `static-classification-vacuates-refcount-and-sanitizer`): every case asserts
//! the *visible* header refcount transition (`1→2→1→0`) read directly from the
//! C-string header at `data - CSTRING_HEADER_SIZE + 8`. A test that only
//! checked "no crash" would pass even if clone/drop were inert (the
//! pre-`__PAGEZERO`-fix bug, where macOS misclassified heap strings as static
//! literals and made every retain/release a no-op). Here:
//!   - `insert` MUST NOT bump rc (MOVE, not retain) — caught by asserting rc
//!     stays 1 across insert;
//!   - `clone` MUST bump rc `1→2` for both K and V — caught directly;
//!   - `free` / `remove` MUST decrement and free-at-zero — caught by the
//!     `2→1` transition (and by `ASan`'s free-at-zero / no-UAF / no-double-free
//!     on `ffi_boundary`, the second net).
//!
//! These use the REAL runtime descriptors (`hew_layout_key_string` /
//! `hew_layout_val_string`) and the REAL header-aware allocator
//! (`malloc_cstring`), so the path exercised is the one codegen wires for
//! `map<string, V>` / `set<string>`.

#![allow(
    clippy::undocumented_unsafe_blocks,
    reason = "test harness — every unsafe block carries an inline SAFETY note"
)]
#![allow(
    clippy::cast_ptr_alignment,
    reason = "tests read a u32 refcount out of a byte-addressed header"
)]

use std::ffi::{c_char, c_void};
use std::sync::atomic::{AtomicU32, Ordering};

use hew_cabi::cabi::CSTRING_HEADER_SIZE;
use hew_runtime::cabi::malloc_cstring;
use hew_runtime::hashmap::{
    hew_hashmap_clone_layout, hew_hashmap_free_layout, hew_hashmap_insert_layout,
    hew_hashmap_new_with_layout, hew_hashmap_remove_layout,
};
use hew_runtime::hashset::{
    hew_hashset_clone_layout, hew_hashset_free_layout, hew_hashset_insert_layout,
    hew_hashset_new_with_layout, hew_hashset_remove_layout,
};
use hew_runtime::layout_intrinsics::{hew_layout_key_string, hew_layout_val_string};
use hew_runtime::string::{hew_string_clone, hew_string_drop};

/// Allocate a fresh header-aware Hew string (`rc == 1`) the same way codegen's
/// string producers do. The returned pointer is a sole owner; it must be
/// released through `hew_string_drop` (never bare `free`).
fn make_string(s: &str) -> *mut c_char {
    // SAFETY: `s.as_ptr()` is readable for `s.len()` bytes; `malloc_cstring`
    // copies exactly that many and NUL-terminates, returning a header-aware
    // allocation.
    unsafe { malloc_cstring(s.as_ptr(), s.len()) }
}

/// Read the live refcount of a header-aware string element WITHOUT perturbing
/// it. The 16-byte header is `[magic:u64 | rc:AtomicU32 | reserved:u32]`, so
/// the refcount sits at offset 8, i.e. `data - CSTRING_HEADER_SIZE + 8`.
///
/// # Safety
/// `data` must be a live header-aware element produced by the `hew-cabi`
/// allocator.
unsafe fn element_refcount(data: *const c_char) -> u32 {
    // SAFETY: header-aware element; rc lives at base+8 (base = data - 16).
    unsafe {
        let base = data.cast::<u8>().sub(CSTRING_HEADER_SIZE);
        let rc = base.add(8).cast::<AtomicU32>();
        (*rc).load(Ordering::Relaxed)
    }
}

/// Insert `key -> val` into a `map<string, string>` by MOVE. `key`/`val` are
/// header-aware string data pointers; the slot stores the pointer bits, so we
/// pass the address of a slot-shaped local (`&raw const slot`).
///
/// # Safety
/// `m` must be a valid string-keyed/valued `HewLayoutHashMap`; `key`/`val`
/// must be live header-aware strings whose ownership transfers to the map.
unsafe fn insert_move(
    m: *mut hew_runtime::hashmap::HewLayoutHashMap,
    key: *mut c_char,
    val: *mut c_char,
) -> bool {
    let key_slot: *const c_char = key;
    let val_slot: *const c_char = val;
    // SAFETY: the slot locals are pointer-sized blobs matching the String
    // descriptor; insert copies their bits into the map (MOVE).
    unsafe {
        hew_hashmap_insert_layout(
            m,
            (&raw const key_slot).cast::<c_void>(),
            (&raw const val_slot).cast::<c_void>(),
        )
    }
}

/// The canonical clone ladder: insert MOVEs (no rc bump), clone retains both K
/// and V (`1→2`), freeing one map releases one owner of each (`2→1`), freeing
/// the last map frees the elements at zero (proved no-UAF/no-double-free under
/// `ASan` on `ffi_boundary`). The visible rc transitions are the non-vacuous
/// bar — they fail if `hew_string_clone` / `hew_string_drop` were inert.
#[test]
fn hashmap_string_kv_clone_retains_and_free_releases() {
    // SAFETY: all FFI calls use a valid string-keyed/valued map + header-aware
    // strings; the rc reads observe live headers.
    unsafe {
        let m = hew_hashmap_new_with_layout(
            &raw const hew_layout_key_string,
            &raw const hew_layout_val_string,
        );

        let k = make_string("alpha");
        let v = make_string("one");
        assert_eq!(element_refcount(k), 1, "fresh key is a sole owner");
        assert_eq!(element_refcount(v), 1, "fresh value is a sole owner");

        let was_new = insert_move(m, k, v);
        assert!(was_new, "vacant insert reports a new entry");

        // INGRESS IS A MOVE: insert transfers ownership, it does NOT retain.
        // The map is now the sole owner; rc must stay 1 (would be 2 if insert
        // wrongly copied-in + retained).
        assert_eq!(
            element_refcount(k),
            1,
            "insert MOVES the key — no retain on ingress (rc stays 1)",
        );
        assert_eq!(
            element_refcount(v),
            1,
            "insert MOVES the value — no retain on ingress (rc stays 1)",
        );

        // Clone retains a second owner of BOTH K and V (header-aware COW alias).
        let cloned = hew_hashmap_clone_layout(m);
        assert!(!cloned.is_null(), "clone of a non-null map is non-null");
        assert_eq!(
            element_refcount(k),
            2,
            "clone must retain the key: rc 1→2 (fails if hew_string_clone inert)",
        );
        assert_eq!(
            element_refcount(v),
            2,
            "clone must retain the value: rc 1→2 (fails if hew_string_clone inert)",
        );

        // Free the original: releases exactly one owner of each element.
        hew_hashmap_free_layout(m);
        assert_eq!(
            element_refcount(k),
            1,
            "free releases exactly one key owner: rc 2→1 (fails if drop inert)",
        );
        assert_eq!(
            element_refcount(v),
            1,
            "free releases exactly one value owner: rc 2→1 (fails if drop inert)",
        );

        // Free the clone: releases the last owners (free-at-zero). ASan proves
        // the buffers are freed exactly once here, with no UAF/double-free.
        hew_hashmap_free_layout(cloned);
    }
}

/// `remove` releases the map's owner of the entry's K and V. Observed against a
/// surviving clone so the rc transition (`2→1`) is visible without UAF.
#[test]
fn hashmap_string_remove_releases_owners() {
    // SAFETY: see above.
    unsafe {
        let m = hew_hashmap_new_with_layout(
            &raw const hew_layout_key_string,
            &raw const hew_layout_val_string,
        );

        let k = make_string("beta");
        let v = make_string("two");
        insert_move(m, k, v);
        assert_eq!(element_refcount(k), 1);
        assert_eq!(element_refcount(v), 1);

        // Clone gives a second owner (rc 2) so the element survives the remove
        // and we can read the post-remove rc.
        let cloned = hew_hashmap_clone_layout(m);
        assert_eq!(element_refcount(k), 2, "clone retains key: 1→2");
        assert_eq!(element_refcount(v), 2, "clone retains value: 1→2");

        // Remove from the original by probing with the same-content key blob.
        let probe: *const c_char = k;
        let removed = hew_hashmap_remove_layout(m, (&raw const probe).cast::<c_void>());
        assert!(removed, "key was present, remove reports true");
        assert_eq!(
            element_refcount(k),
            1,
            "remove releases the map's key owner: rc 2→1 (fails if drop inert)",
        );
        assert_eq!(
            element_refcount(v),
            1,
            "remove releases the map's value owner: rc 2→1 (fails if drop inert)",
        );

        // Tear down: free both maps; the clone still owns the entry, freed at 0.
        hew_hashmap_free_layout(m);
        hew_hashmap_free_layout(cloned);
    }
}

/// Overwrite drops the OLD value (rc decrement, free-at-zero) and MOVEs the new
/// value in; the stored key is reused and never re-dropped. Observed against an
/// extra owner of the old value so the `2→1` transition is visible.
#[test]
fn hashmap_string_overwrite_releases_old_value_keeps_key() {
    // SAFETY: see above.
    unsafe {
        let m = hew_hashmap_new_with_layout(
            &raw const hew_layout_key_string,
            &raw const hew_layout_val_string,
        );

        let k = make_string("gamma");
        let v1 = make_string("first");
        // Hold an extra owner of v1 so we can observe the overwrite release
        // without losing the buffer (rc 1→2).
        let v1_extra = hew_string_clone(v1);
        assert_eq!(v1_extra, v1, "clone of a header-aware string aliases (COW)");
        assert_eq!(
            element_refcount(v1),
            2,
            "v1 has a second owner before insert"
        );

        insert_move(m, k, v1);
        // MOVE: the map took v1's owner that originated from `make_string`; the
        // extra owner is independent. rc stays 2.
        assert_eq!(
            element_refcount(v1),
            2,
            "insert MOVES v1 (no retain): rc stays 2"
        );
        assert_eq!(element_refcount(k), 1, "key sole owner is the map");

        // Overwrite the same key with a fresh value. The caller passes a fresh
        // duplicate key (overwrite REUSES the stored key; the caller's K is not
        // consumed — the conditional-key asymmetry). We allocate and free that
        // duplicate ourselves to model the caller's retained-K responsibility.
        let dup_key = make_string("gamma");
        let v2 = make_string("second");
        let dup_key_slot: *const c_char = dup_key;
        let v2_slot: *const c_char = v2;
        let was_new = hew_hashmap_insert_layout(
            m,
            (&raw const dup_key_slot).cast::<c_void>(),
            (&raw const v2_slot).cast::<c_void>(),
        );
        assert!(!was_new, "overwrite reports an existing entry");

        // Overwrite dropped the map's owner of the OLD value v1: rc 2→1.
        assert_eq!(
            element_refcount(v1),
            1,
            "overwrite releases old value: rc 2→1 (fails if drop inert)",
        );
        // The stored key was reused, never re-dropped: still a sole owner.
        assert_eq!(
            element_refcount(k),
            1,
            "overwrite never drops the stored key"
        );
        // The new value moved in (no retain): sole owner is the map.
        assert_eq!(element_refcount(v2), 1, "new value MOVED in: sole owner");

        // Caller retains the duplicate key on overwrite — release it ourselves.
        hew_string_drop(dup_key);
        // Release our extra owner of v1 (rc 1→0, freed). ASan-clean.
        hew_string_drop(v1_extra);

        // Free the map: drops the stored key and v2 at zero.
        hew_hashmap_free_layout(m);
    }
}

/// `HashSet` parity: the element IS the map key. Insert MOVEs, clone retains
/// (`1→2`), free/remove release (`2→1→0`). One full ladder through the set
/// entry points.
#[test]
fn hashset_string_element_clone_retains_and_free_releases() {
    // SAFETY: see above; set element layout is the String key descriptor.
    unsafe {
        let s = hew_hashset_new_with_layout(&raw const hew_layout_key_string);

        let e = make_string("delta");
        assert_eq!(element_refcount(e), 1, "fresh element is a sole owner");

        let e_slot: *const c_char = e;
        let was_new = hew_hashset_insert_layout(s, (&raw const e_slot).cast::<c_void>());
        assert!(was_new, "vacant insert reports a new element");
        assert_eq!(
            element_refcount(e),
            1,
            "insert MOVES the element — no retain on ingress (rc stays 1)",
        );

        let cloned = hew_hashset_clone_layout(s);
        assert!(!cloned.is_null());
        assert_eq!(
            element_refcount(e),
            2,
            "clone must retain the element: rc 1→2 (fails if hew_string_clone inert)",
        );

        // Remove from the original: releases the original's owner (2→1).
        let probe: *const c_char = e;
        let removed = hew_hashset_remove_layout(s, (&raw const probe).cast::<c_void>());
        assert!(removed, "element present, remove reports true");
        assert_eq!(
            element_refcount(e),
            1,
            "remove releases one element owner: rc 2→1 (fails if drop inert)",
        );

        // Free both sets; the clone owns the last reference, freed at 0.
        hew_hashset_free_layout(s);
        hew_hashset_free_layout(cloned);
    }
}
