//! FFI boundary integration tests for the layout-backed `HewLayoutHashSet`
//! C ABI (W3.003 slice C-1c).
//!
//! Each test drives the runtime through the same `#[no_mangle] extern "C"`
//! entry points that C-3 codegen will emit. Synthetic element layouts and
//! hash/eq thunks stand in for codegen-synthesized identity.
//!
//! # `should_panic` pattern
//!
//! `should_panic` tests call the `pub` validator helpers (`validate_set_op`,
//! `validate_set_op_elem`) or the imported C-1b helpers (`validate_key_layout`)
//! rather than the `extern "C"` entry points directly. This mirrors the
//! pattern from `ffi_boundary_layout_hashmap.rs`: the test harness can only
//! observe a `panic!()` (not a `std::process::abort()`) at a Rust-only call
//! site, and the `extern "C"` entry points for null-set / null-elem use
//! `panic!()` internally via the shared validators.

#![expect(
    clippy::undocumented_unsafe_blocks,
    reason = "FFI test harness — safety invariants are documented per-test"
)]
#![allow(
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap,
    clippy::cast_ptr_alignment,
    clippy::cast_sign_loss,
    reason = "tests deliberately cast between pointer + integer for byte blobs"
)]

use std::ffi::c_void;
use std::ptr;

use hew_cabi::map::{HewMapKeyEqThunk, HewMapKeyHashThunk, HewMapKeyLayout};
use hew_cabi::vec::HewTypeOwnershipKind;
use hew_runtime::hashmap::{validate_descriptor_ownership, validate_key_layout};
use hew_runtime::hashset::{
    hew_hashset_clone_layout, hew_hashset_contains_layout, hew_hashset_free_layout,
    hew_hashset_insert_layout, hew_hashset_len_layout, hew_hashset_new_with_layout,
    hew_hashset_remove_layout, validate_set_op, validate_set_op_elem,
};

// ---------------------------------------------------------------------------
// Synthetic thunks (Point key: two i64 fields, 16 bytes, align 8)
// ---------------------------------------------------------------------------

unsafe extern "C" fn hash_point(key: *const c_void) -> u64 {
    // SAFETY: blob is 16 bytes (two i64 fields, no padding in this layout).
    let x = unsafe { *key.cast::<i64>() };
    let y = unsafe { *key.cast::<i64>().add(1) };
    (x as u64)
        .wrapping_mul(0x9E37_79B9_7F4A_7C15)
        .wrapping_add(y as u64)
}

unsafe extern "C" fn eq_point(lhs: *const c_void, rhs: *const c_void) -> i32 {
    // SAFETY: both blobs are 16 bytes (Point: two i64 fields).
    let lx = unsafe { *lhs.cast::<i64>() };
    let ly = unsafe { *lhs.cast::<i64>().add(1) };
    let rx = unsafe { *rhs.cast::<i64>() };
    let ry = unsafe { *rhs.cast::<i64>().add(1) };
    i32::from(lx == rx && ly == ry)
}

// ---------------------------------------------------------------------------
// Layout descriptor builder
// ---------------------------------------------------------------------------

fn elem_layout_point() -> HewMapKeyLayout {
    HewMapKeyLayout {
        size: 16,
        align: 8,
        ownership_kind: HewTypeOwnershipKind::Plain,
        hash_fn: Some(hash_point as HewMapKeyHashThunk),
        eq_fn: Some(eq_point as HewMapKeyEqThunk),
        drop_fn: None,
    }
}

// ---------------------------------------------------------------------------
// Happy-path tests
// ---------------------------------------------------------------------------

/// Round-trip insert + contains for a `Point` record key.
#[test]
fn layout_hashset_insert_contains_copy_record() {
    // SAFETY: layout is valid; set pointer is from hew_hashset_new_with_layout.
    let kl = elem_layout_point();
    unsafe {
        let s = hew_hashset_new_with_layout(&raw const kl);
        assert!(!s.is_null(), "constructor must return non-null");

        let point: [i64; 2] = [3, 4]; // Point { x: 3, y: 4 }
        let inserted = hew_hashset_insert_layout(s, point.as_ptr().cast::<c_void>());
        assert!(inserted, "first insert must return true (newly added)");

        let found = hew_hashset_contains_layout(s, point.as_ptr().cast::<c_void>());
        assert!(found, "contains must return true after insert");

        // A different point must not be present.
        let other: [i64; 2] = [5, 6];
        assert!(
            !hew_hashset_contains_layout(s, other.as_ptr().cast::<c_void>()),
            "contains must return false for absent element"
        );

        // Duplicate insert returns false.
        let dup = hew_hashset_insert_layout(s, point.as_ptr().cast::<c_void>());
        assert!(!dup, "duplicate insert must return false");

        hew_hashset_free_layout(s);
    }
}

/// Remove a previously inserted element and confirm it is gone.
#[test]
fn layout_hashset_remove_copy_record() {
    // SAFETY: layout and set pointers are valid.
    let kl = elem_layout_point();
    unsafe {
        let s = hew_hashset_new_with_layout(&raw const kl);

        let point: [i64; 2] = [10, 20];
        hew_hashset_insert_layout(s, point.as_ptr().cast::<c_void>());

        let removed = hew_hashset_remove_layout(s, point.as_ptr().cast::<c_void>());
        assert!(removed, "remove must return true for a present element");

        let still_there = hew_hashset_contains_layout(s, point.as_ptr().cast::<c_void>());
        assert!(!still_there, "contains must return false after remove");

        // Removing an absent element returns false.
        let again = hew_hashset_remove_layout(s, point.as_ptr().cast::<c_void>());
        assert!(!again, "second remove of same element must return false");

        hew_hashset_free_layout(s);
    }
}

/// `len_layout` tracks inserts and removes correctly.
#[test]
fn layout_hashset_len_after_insert_remove() {
    // SAFETY: layout and set pointers are valid.
    let kl = elem_layout_point();
    unsafe {
        let s = hew_hashset_new_with_layout(&raw const kl);
        assert_eq!(hew_hashset_len_layout(s), 0, "fresh set must have len 0");

        let a: [i64; 2] = [1, 2];
        let b: [i64; 2] = [3, 4];
        let c: [i64; 2] = [5, 6];

        hew_hashset_insert_layout(s, a.as_ptr().cast::<c_void>());
        hew_hashset_insert_layout(s, b.as_ptr().cast::<c_void>());
        hew_hashset_insert_layout(s, c.as_ptr().cast::<c_void>());
        assert_eq!(
            hew_hashset_len_layout(s),
            3,
            "len must be 3 after 3 inserts"
        );

        hew_hashset_remove_layout(s, b.as_ptr().cast::<c_void>());
        assert_eq!(
            hew_hashset_len_layout(s),
            2,
            "len must be 2 after removing one element"
        );

        // Duplicate insert must not inflate len.
        hew_hashset_insert_layout(s, a.as_ptr().cast::<c_void>());
        assert_eq!(
            hew_hashset_len_layout(s),
            2,
            "duplicate insert must not increase len"
        );

        hew_hashset_free_layout(s);
    }
}

/// `hew_hashset_free_layout(null)` is a documented no-op.
#[test]
fn layout_hashset_free_null_is_noop() {
    // SAFETY: null is explicitly handled by hew_hashset_free_layout.
    unsafe { hew_hashset_free_layout(ptr::null_mut()) };
}

/// `hew_hashset_clone_layout` deep-copies a non-empty set: the clone observes
/// every original element, and the two handles are independent storage (freed
/// separately, with no double-free / use-after-free).
///
/// W4.045 regression: pre-fix the codegen emitted the legacy
/// `hew_hashset_clone` against a `HewLayoutHashSet*`, reinterpreting the
/// 16-byte layout entries as 48-byte map entries. This drives the corrected
/// layout clone over a populated set so a sanitizer build (`make asan`) flags
/// any invalid free or out-of-bounds read.
#[test]
fn layout_hashset_clone_deep_copies_non_empty() {
    let kl = elem_layout_point();
    // SAFETY: layout is valid; all set pointers are from the layout ctor/clone.
    unsafe {
        let src = hew_hashset_new_with_layout(&raw const kl);

        let a: [i64; 2] = [1, 2];
        let b: [i64; 2] = [3, 4];
        let c: [i64; 2] = [5, 6];
        hew_hashset_insert_layout(src, a.as_ptr().cast::<c_void>());
        hew_hashset_insert_layout(src, b.as_ptr().cast::<c_void>());
        hew_hashset_insert_layout(src, c.as_ptr().cast::<c_void>());
        assert_eq!(hew_hashset_len_layout(src), 3);

        let clone = hew_hashset_clone_layout(src.cast_const());
        assert!(!clone.is_null(), "clone of non-null set must be non-null");
        assert_eq!(
            hew_hashset_len_layout(clone),
            3,
            "clone must contain every source element"
        );
        for elem in [&a, &b, &c] {
            assert!(
                hew_hashset_contains_layout(clone, elem.as_ptr().cast::<c_void>()),
                "clone must contain each original element"
            );
        }

        // Independence: removing from the source must not touch the clone.
        hew_hashset_remove_layout(src, a.as_ptr().cast::<c_void>());
        assert_eq!(hew_hashset_len_layout(src), 2);
        assert_eq!(
            hew_hashset_len_layout(clone),
            3,
            "mutating the source must not affect the clone"
        );
        assert!(
            hew_hashset_contains_layout(clone, a.as_ptr().cast::<c_void>()),
            "clone must retain an element removed from the source"
        );

        // Both handles free cleanly — distinct backing allocations.
        hew_hashset_free_layout(src);
        hew_hashset_free_layout(clone);
    }
}

/// `hew_hashset_clone_layout(null)` returns null (null-in / null-out),
/// mirroring `hew_hashset_clone` and `hew_hashmap_clone_layout`.
#[test]
fn layout_hashset_clone_null_returns_null() {
    // SAFETY: null is explicitly handled by hew_hashset_clone_layout.
    let cloned = unsafe { hew_hashset_clone_layout(ptr::null()) };
    assert!(cloned.is_null(), "clone of null must return null");
}

// ---------------------------------------------------------------------------
// Fail-closed gates
//
// `should_panic` tests call `pub` validator helpers or the C-1b
// `validate_key_layout` directly rather than `extern "C"` entry points.
// See module-level doc for the rationale.
// ---------------------------------------------------------------------------

/// A null `elem_layout` pointer to the constructor panics fail-closed.
///
/// We drive `validate_key_layout(null)` (the C-1b gate, `pub`) rather than
/// the `extern "C"` constructor (which uses `std::process::abort()` and
/// cannot be observed by `should_panic`).
#[test]
#[should_panic(expected = "key_layout is null")]
fn layout_hashset_null_elem_layout_aborts() {
    // SAFETY: intentionally null — tests the fail-closed gate.
    unsafe {
        validate_key_layout(ptr::null());
    }
}

/// A `LayoutManaged` element ownership kind without a `drop_fn` panics
/// fail-closed at the constructor (W4.001 Stage C0a; the old
/// `"LayoutManaged out of scope"` panic was relaxed and replaced by the
/// more precise descriptor-consistency gate).
///
/// We drive `validate_descriptor_ownership` directly rather than
/// `hew_hashset_new_with_layout`: the latter is `extern "C"` and panics
/// across it are non-unwinding aborts under `panic = "abort"`, so a
/// `should_panic` test cannot observe them.
#[test]
#[should_panic(expected = "key_layout ownership_kind=LayoutManaged requires drop_fn")]
fn layout_hashset_managed_elem_without_drop_aborts() {
    let kl = HewMapKeyLayout {
        size: 16,
        align: 8,
        ownership_kind: HewTypeOwnershipKind::LayoutManaged,
        hash_fn: Some(hash_point as HewMapKeyHashThunk),
        eq_fn: Some(eq_point as HewMapKeyEqThunk),
        drop_fn: None,
    };
    // The hashset's ZST value layout is internal to hew_hashset_new_with_layout;
    // for this gate-level test we synthesize an equivalent value descriptor.
    let vl = hew_cabi::map::HewMapValueLayout {
        size: 0,
        align: 1,
        ownership_kind: HewTypeOwnershipKind::Plain,
        drop_fn: None,
        clone_fn: None,
    };
    // SAFETY: both descriptors are well-formed addresses; the gate is what
    // we want to observe.
    unsafe {
        validate_descriptor_ownership(&raw const kl, &raw const vl);
    }
}

/// A null `set` pointer to any element operation panics fail-closed.
#[test]
#[should_panic(expected = "set is null")]
fn layout_hashset_null_set_aborts() {
    // SAFETY: intentionally null set — tests the fail-closed gate.
    unsafe {
        validate_set_op(ptr::null());
    }
}

/// A null `elem` pointer to an element operation panics fail-closed.
#[test]
#[should_panic(expected = "elem is null")]
fn layout_hashset_null_elem_aborts() {
    // Build a real set so the null-set branch is NOT triggered; then supply a
    // null elem to trip the null-elem gate. We deliberately do NOT free the
    // set (the unwind aborts the test fn; the leak is harmless under
    // should_panic semantics, consistent with the C-1b test pattern).
    let kl = elem_layout_point();
    let s = unsafe { hew_hashset_new_with_layout(&raw const kl) };
    // SAFETY: s non-null; elem is intentionally null — tests the fail-closed gate.
    unsafe {
        validate_set_op_elem(s.cast_const(), ptr::null());
    }
    // Unreachable; appease the type system.
    unsafe { hew_hashset_free_layout(s) };
}
