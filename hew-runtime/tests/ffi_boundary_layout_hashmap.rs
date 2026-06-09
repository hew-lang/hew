//! FFI boundary integration tests for the layout-backed `HewLayoutHashMap`
//! C ABI (W3.003 slice C-1b).
//!
//! Each test drives the runtime through the same `#[no_mangle] extern "C"`
//! entry points that C-3 codegen will emit. Synthetic key/value layouts and
//! hash/eq thunks stand in for codegen-synthesized identity.

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

use hew_cabi::map::{HewMapKeyEqThunk, HewMapKeyHashThunk, HewMapKeyLayout, HewMapValueLayout};
use hew_cabi::vec::HewTypeOwnershipKind;
use hew_runtime::hashmap::{
    hew_hashmap_contains_key_layout, hew_hashmap_free_layout, hew_hashmap_get_layout,
    hew_hashmap_insert_layout, hew_hashmap_len_layout, hew_hashmap_new_with_layout,
    hew_hashmap_remove_layout, validate_and_compute_slot_layout, validate_key_layout,
    validate_op_inputs, validate_op_map, validate_val_layout, HewLayoutHashMap,
};

// ---------------------------------------------------------------------------
// Synthetic thunks (i64 key)
// ---------------------------------------------------------------------------

unsafe extern "C" fn hash_i64(key: *const c_void) -> u64 {
    // Simple mix; never read padding (the blob is exactly 8 bytes).
    // SAFETY: the runtime guarantees key points to an i64-sized blob.
    let v = unsafe { *key.cast::<i64>() };
    (v as u64).wrapping_mul(0x9E37_79B9_7F4A_7C15)
}

unsafe extern "C" fn eq_i64(lhs: *const c_void, rhs: *const c_void) -> i32 {
    // SAFETY: caller guarantees both pointers are i64-sized blobs.
    let l = unsafe { *lhs.cast::<i64>() };
    let r = unsafe { *rhs.cast::<i64>() };
    i32::from(l == r)
}

unsafe extern "C" fn hash_i32(key: *const c_void) -> u64 {
    // SAFETY: blob is i32-sized.
    let v = unsafe { *key.cast::<i32>() };
    u64::from(v as u32).wrapping_mul(0x9E37_79B9_7F4A_7C15)
}

unsafe extern "C" fn eq_i32(lhs: *const c_void, rhs: *const c_void) -> i32 {
    // SAFETY: both blobs are i32-sized.
    let l = unsafe { *lhs.cast::<i32>() };
    let r = unsafe { *rhs.cast::<i32>() };
    i32::from(l == r)
}

unsafe extern "C" fn hash_point(key: *const c_void) -> u64 {
    // SAFETY: blob is 16 bytes (two i64).
    let x = unsafe { *key.cast::<i64>() };
    let y = unsafe { *key.cast::<i64>().add(1) };
    (x as u64)
        .wrapping_mul(0x9E37_79B9_7F4A_7C15)
        .wrapping_add(y as u64)
}

unsafe extern "C" fn eq_point(lhs: *const c_void, rhs: *const c_void) -> i32 {
    // SAFETY: both blobs are 16 bytes (Point).
    let lx = unsafe { *lhs.cast::<i64>() };
    let ly = unsafe { *lhs.cast::<i64>().add(1) };
    let rx = unsafe { *rhs.cast::<i64>() };
    let ry = unsafe { *rhs.cast::<i64>().add(1) };
    i32::from(lx == rx && ly == ry)
}

// ---------------------------------------------------------------------------
// Layout descriptor builders
// ---------------------------------------------------------------------------

fn key_layout_i64() -> HewMapKeyLayout {
    HewMapKeyLayout {
        size: 8,
        align: 8,
        ownership_kind: HewTypeOwnershipKind::Plain,
        hash_fn: Some(hash_i64 as HewMapKeyHashThunk),
        eq_fn: Some(eq_i64 as HewMapKeyEqThunk),
    }
}

fn key_layout_i32() -> HewMapKeyLayout {
    HewMapKeyLayout {
        size: 4,
        align: 4,
        ownership_kind: HewTypeOwnershipKind::Plain,
        hash_fn: Some(hash_i32 as HewMapKeyHashThunk),
        eq_fn: Some(eq_i32 as HewMapKeyEqThunk),
    }
}

fn key_layout_point() -> HewMapKeyLayout {
    HewMapKeyLayout {
        size: 16,
        align: 8,
        ownership_kind: HewTypeOwnershipKind::Plain,
        hash_fn: Some(hash_point as HewMapKeyHashThunk),
        eq_fn: Some(eq_point as HewMapKeyEqThunk),
    }
}

fn val_layout(size: usize, align: usize) -> HewMapValueLayout {
    HewMapValueLayout {
        size,
        align,
        ownership_kind: HewTypeOwnershipKind::Plain,
    }
}

// ---------------------------------------------------------------------------
// Happy-path round trips
// ---------------------------------------------------------------------------

#[test]
fn layout_hashmap_insert_contains_roundtrip_copy_record() {
    let kl = key_layout_point();
    let vl = val_layout(8, 8); // i64 value
    unsafe {
        let m = hew_hashmap_new_with_layout(&raw const kl, &raw const vl);
        assert!(!m.is_null());

        let key: [i64; 2] = [3, 4];
        let value: i64 = 25;
        let added = hew_hashmap_insert_layout(
            m,
            key.as_ptr().cast::<c_void>(),
            (&raw const value).cast::<c_void>(),
        );
        assert!(added, "first insert must report new entry");
        assert!(
            hew_hashmap_contains_key_layout(m, key.as_ptr().cast::<c_void>()),
            "round-trip lookup must find the inserted record key"
        );
        assert_eq!(hew_hashmap_len_layout(m), 1);

        hew_hashmap_free_layout(m);
    }
}

#[test]
fn layout_hashmap_get_returns_value_blob() {
    let kl = key_layout_i64();
    let vl = val_layout(8, 8);
    unsafe {
        let m = hew_hashmap_new_with_layout(&raw const kl, &raw const vl);

        let key: i64 = 42;
        let val: i64 = 99;
        hew_hashmap_insert_layout(
            m,
            (&raw const key).cast::<c_void>(),
            (&raw const val).cast::<c_void>(),
        );

        let got = hew_hashmap_get_layout(m, (&raw const key).cast::<c_void>());
        assert!(
            !got.is_null(),
            "get must return a non-null pointer for present key"
        );
        // memcmp the bytes — the runtime returned a pointer to the slot's value blob.
        let got_val = *got.cast::<i64>();
        assert_eq!(got_val, 99);

        // Absent key returns null.
        let missing: i64 = 1234;
        let absent = hew_hashmap_get_layout(m, (&raw const missing).cast::<c_void>());
        assert!(absent.is_null(), "absent key must return null");

        hew_hashmap_free_layout(m);
    }
}

#[test]
fn layout_hashmap_remove_shrinks_len() {
    let kl = key_layout_i64();
    let vl = val_layout(8, 8);
    unsafe {
        let m = hew_hashmap_new_with_layout(&raw const kl, &raw const vl);
        for i in 0..5_i64 {
            let v: i64 = i * 10;
            hew_hashmap_insert_layout(
                m,
                (&raw const i).cast::<c_void>(),
                (&raw const v).cast::<c_void>(),
            );
        }
        assert_eq!(hew_hashmap_len_layout(m), 5);

        let target: i64 = 3;
        assert!(hew_hashmap_remove_layout(
            m,
            (&raw const target).cast::<c_void>()
        ));
        assert_eq!(hew_hashmap_len_layout(m), 4);
        assert!(!hew_hashmap_contains_key_layout(
            m,
            (&raw const target).cast::<c_void>()
        ));

        // Removing an absent key returns false and does not shrink len.
        assert!(!hew_hashmap_remove_layout(
            m,
            (&raw const target).cast::<c_void>()
        ));
        assert_eq!(hew_hashmap_len_layout(m), 4);

        hew_hashmap_free_layout(m);
    }
}

#[test]
fn layout_hashmap_resize_preserves_all_entries() {
    let kl = key_layout_i64();
    let vl = val_layout(8, 8);
    unsafe {
        let m = hew_hashmap_new_with_layout(&raw const kl, &raw const vl);
        // Initial cap = 16, load threshold 75% → resize trips around the 12th insert.
        // Insert well past that to force at least one resize.
        let n: i64 = 64;
        for i in 0..n {
            let v: i64 = i * 7;
            hew_hashmap_insert_layout(
                m,
                (&raw const i).cast::<c_void>(),
                (&raw const v).cast::<c_void>(),
            );
        }
        assert_eq!(hew_hashmap_len_layout(m), n);
        // Each entry must still be retrievable post-resize.
        for i in 0..n {
            let got = hew_hashmap_get_layout(m, (&raw const i).cast::<c_void>());
            assert!(!got.is_null(), "key {i} missing after resize");
            assert_eq!(
                *got.cast::<i64>(),
                i * 7,
                "value drift after resize for key {i}"
            );
        }
        hew_hashmap_free_layout(m);
    }
}

// ---------------------------------------------------------------------------
// Slot stride / alignment correctness
// ---------------------------------------------------------------------------

#[test]
fn layout_hashmap_align8_key_stride_padding_is_correct() {
    // i64 key (align 8) following the state byte at offset 0 must be padded
    // up to offset 8.
    let kl = key_layout_i64();
    let vl = val_layout(8, 8);
    unsafe {
        let m = hew_hashmap_new_with_layout(&raw const kl, &raw const vl);
        let map: &HewLayoutHashMap = &*m;
        assert_eq!(
            map.key_offset, 8,
            "align-8 key must be padded past state byte"
        );
        assert_eq!(
            map.val_offset, 16,
            "val must follow 8-byte key at offset 16"
        );
        assert_eq!(map.stride, 24, "stride must pad to max-align (8)");
        hew_hashmap_free_layout(m);
    }
}

#[test]
fn layout_hashmap_mixed_align_key_value_stride() {
    // key = i64 (align 8, size 8); val = i32 (align 4, size 4). Expected:
    //   key_offset = align_up(1, 8) = 8
    //   val_offset = align_up(16, 4) = 16
    //   stride     = align_up(20, 8) = 24
    let kl = key_layout_i64();
    let vl = val_layout(4, 4);
    unsafe {
        let m = hew_hashmap_new_with_layout(&raw const kl, &raw const vl);
        let map: &HewLayoutHashMap = &*m;
        assert_eq!(map.key_offset, 8);
        assert_eq!(map.val_offset, 16);
        assert_eq!(
            map.stride, 24,
            "stride must round up to max(key_align=8, val_align=4) = 8"
        );

        // Round-trip a mixed-align entry to confirm offsets are usable.
        let key: i64 = 7;
        let val: i32 = -123;
        hew_hashmap_insert_layout(
            m,
            (&raw const key).cast::<c_void>(),
            (&raw const val).cast::<c_void>(),
        );
        let got = hew_hashmap_get_layout(m, (&raw const key).cast::<c_void>());
        assert!(!got.is_null());
        assert_eq!(*got.cast::<i32>(), -123);
        hew_hashmap_free_layout(m);
    }
}

#[test]
fn layout_hashmap_smaller_key_align_packs_correctly() {
    // i32 key (align 4) + i64 val (align 8). Expected:
    //   key_offset = align_up(1, 4) = 4
    //   val_offset = align_up(8, 8) = 8
    //   stride     = align_up(16, 8) = 16
    let kl = key_layout_i32();
    let vl = val_layout(8, 8);
    unsafe {
        let m = hew_hashmap_new_with_layout(&raw const kl, &raw const vl);
        let map: &HewLayoutHashMap = &*m;
        assert_eq!(map.key_offset, 4);
        assert_eq!(map.val_offset, 8);
        assert_eq!(map.stride, 16);
        hew_hashmap_free_layout(m);
    }
}

// ---------------------------------------------------------------------------
// Free semantics
// ---------------------------------------------------------------------------

#[test]
fn layout_hashmap_free_null_is_noop() {
    unsafe {
        hew_hashmap_free_layout(ptr::null_mut());
    }
}

// ---------------------------------------------------------------------------
// Fail-closed gates (council Rev 2/3)
//
// `should_panic` tests target the `pub` validator helpers rather than the
// `extern "C"` entry points: panics cannot unwind across an `extern "C"`
// frame under `panic = "abort"`, so the test harness must observe the
// fail-closed panic at a Rust-only call site. The extern entry calls the
// same validator, so coverage flows back to the boundary.
// ---------------------------------------------------------------------------

#[test]
#[should_panic(expected = "key_layout is null")]
fn layout_hashmap_null_key_layout_aborts() {
    let vl = val_layout(8, 8);
    unsafe {
        let _ = validate_and_compute_slot_layout(ptr::null(), &raw const vl);
    }
}

#[test]
#[should_panic(expected = "val_layout is null")]
fn layout_hashmap_null_val_layout_aborts() {
    let kl = key_layout_i64();
    unsafe {
        let _ = validate_and_compute_slot_layout(&raw const kl, ptr::null());
    }
}

#[test]
#[should_panic(expected = "hash_fn is None")]
fn layout_hashmap_null_hash_fn_aborts() {
    let kl = HewMapKeyLayout {
        size: 8,
        align: 8,
        ownership_kind: HewTypeOwnershipKind::Plain,
        hash_fn: None,
        eq_fn: Some(eq_i64 as HewMapKeyEqThunk),
    };
    unsafe { validate_key_layout(&raw const kl) };
}

#[test]
#[should_panic(expected = "eq_fn is None")]
fn layout_hashmap_null_eq_fn_aborts() {
    let kl = HewMapKeyLayout {
        size: 8,
        align: 8,
        ownership_kind: HewTypeOwnershipKind::Plain,
        hash_fn: Some(hash_i64 as HewMapKeyHashThunk),
        eq_fn: None,
    };
    unsafe { validate_key_layout(&raw const kl) };
}

#[test]
#[should_panic(expected = "LayoutManaged key ownership is out of scope")]
fn layout_hashmap_managed_key_aborts() {
    let kl = HewMapKeyLayout {
        size: 8,
        align: 8,
        ownership_kind: HewTypeOwnershipKind::LayoutManaged,
        hash_fn: Some(hash_i64 as HewMapKeyHashThunk),
        eq_fn: Some(eq_i64 as HewMapKeyEqThunk),
    };
    unsafe { validate_key_layout(&raw const kl) };
}

#[test]
#[should_panic(expected = "LayoutManaged value ownership is out of scope")]
fn layout_hashmap_managed_value_aborts() {
    let vl = HewMapValueLayout {
        size: 8,
        align: 8,
        ownership_kind: HewTypeOwnershipKind::LayoutManaged,
    };
    unsafe { validate_val_layout(&raw const vl) };
}

#[test]
#[should_panic(expected = "zero-size keys are not admissible")]
fn layout_hashmap_zero_size_key_aborts() {
    let kl = HewMapKeyLayout {
        size: 0,
        align: 1,
        ownership_kind: HewTypeOwnershipKind::Plain,
        hash_fn: Some(hash_i64 as HewMapKeyHashThunk),
        eq_fn: Some(eq_i64 as HewMapKeyEqThunk),
    };
    unsafe { validate_key_layout(&raw const kl) };
}

#[test]
#[should_panic(expected = "key_layout.align is not a power of two")]
fn layout_hashmap_invalid_align_aborts() {
    let kl = HewMapKeyLayout {
        size: 8,
        align: 3,
        ownership_kind: HewTypeOwnershipKind::Plain,
        hash_fn: Some(hash_i64 as HewMapKeyHashThunk),
        eq_fn: Some(eq_i64 as HewMapKeyEqThunk),
    };
    unsafe { validate_key_layout(&raw const kl) };
}

#[test]
#[should_panic(expected = "zero-size value layout must have align == 1")]
fn layout_hashmap_zero_size_value_with_nonunit_align_aborts() {
    let vl = HewMapValueLayout {
        size: 0,
        align: 8, // invalid: size==0 requires align==1 (HashSet ZST contract)
        ownership_kind: HewTypeOwnershipKind::Plain,
    };
    unsafe { validate_val_layout(&raw const vl) };
}

#[test]
#[should_panic(expected = "slot stride overflow")]
fn layout_hashmap_stride_overflow_aborts() {
    // Huge key size that, even before multiplication by capacity, exceeds the
    // isize::MAX/4 stride budget. align=8 is fine; the size itself trips the
    // overflow guard.
    let kl = HewMapKeyLayout {
        size: usize::MAX / 2,
        align: 8,
        ownership_kind: HewTypeOwnershipKind::Plain,
        hash_fn: Some(hash_i64 as HewMapKeyHashThunk),
        eq_fn: Some(eq_i64 as HewMapKeyEqThunk),
    };
    let vl = val_layout(8, 8);
    unsafe {
        let _ = validate_and_compute_slot_layout(&raw const kl, &raw const vl);
    }
}

#[test]
#[should_panic(expected = "val_layout.align is not a power of two")]
fn val_layout_non_power_of_two_align_aborts() {
    // align = 3 is not a power of two — gate at validate_val_layout.
    let vl = val_layout(8, 3);
    unsafe {
        validate_val_layout(&raw const vl);
    }
}

#[test]
#[should_panic(expected = "op: m is null")]
fn op_inputs_null_map_aborts() {
    // Drive the operational gate directly with a non-null key (any pointer)
    // and a null map. The val=None path means no insert semantics are
    // exercised; only the m-null gate trips.
    let key: i64 = 0;
    unsafe {
        validate_op_inputs(ptr::null(), (&raw const key).cast::<c_void>(), None);
    }
}

#[test]
#[should_panic(expected = "op: key is null")]
fn op_inputs_null_key_aborts() {
    // Build a real map so the m-not-null branch is satisfied; then trip the
    // key-null branch.
    let kl = key_layout_i64();
    let vl = val_layout(8, 8);
    let m = unsafe { hew_hashmap_new_with_layout(&raw const kl, &raw const vl) };
    // Use catch_unwind-incompatible approach: just call and let should_panic
    // observe the unwind under the test profile. We deliberately do NOT free
    // the map (the unwind aborts the test fn before reaching cleanup; the
    // process exits and the leak is harmless for a should_panic test).
    unsafe {
        validate_op_inputs(m.cast_const(), ptr::null(), None);
    }
    // Unreachable; satisfy the type system.
    unsafe { hew_hashmap_free_layout(m) };
}

#[test]
#[should_panic(expected = "val is null but value size > 0")]
fn op_inputs_null_val_with_nonzero_size_aborts() {
    // Insert semantics on an i64-value map: val=Some(null) must trip the
    // size>0 gate.
    let kl = key_layout_i64();
    let vl = val_layout(8, 8);
    let m = unsafe { hew_hashmap_new_with_layout(&raw const kl, &raw const vl) };
    let key: i64 = 0;
    unsafe {
        validate_op_inputs(
            m.cast_const(),
            (&raw const key).cast::<c_void>(),
            Some(ptr::null()),
        );
    }
    unsafe { hew_hashmap_free_layout(m) };
}

#[test]
fn op_inputs_null_val_with_zero_size_ok() {
    // HashSet contract: val_layout.size == 0 means a null val pointer is
    // acceptable on insert — the gate must NOT panic, and a real extern
    // insert must succeed.
    let kl = key_layout_i64();
    let vl = val_layout(0, 1); // ZST value, align==1 (the only legal ZST val layout)
    let m = unsafe { hew_hashmap_new_with_layout(&raw const kl, &raw const vl) };

    // First: drive the validator directly — should not panic.
    let key: i64 = 42;
    unsafe {
        validate_op_inputs(
            m.cast_const(),
            (&raw const key).cast::<c_void>(),
            Some(ptr::null()),
        );
    }

    // Second: drive the actual extern entry point end-to-end with a null val.
    let inserted =
        unsafe { hew_hashmap_insert_layout(m, (&raw const key).cast::<c_void>(), ptr::null()) };
    assert!(
        inserted,
        "first insert of ZST-value entry should report new"
    );
    assert_eq!(unsafe { hew_hashmap_len_layout(m) }, 1);
    assert!(unsafe { hew_hashmap_contains_key_layout(m, (&raw const key).cast::<c_void>()) });

    unsafe { hew_hashmap_free_layout(m) };
}

#[test]
#[should_panic(expected = "op: m is null")]
fn op_map_only_null_aborts() {
    // The len/free variant of the gate — validate_op_map covers `_len_layout`.
    unsafe {
        validate_op_map(ptr::null());
    }
}
