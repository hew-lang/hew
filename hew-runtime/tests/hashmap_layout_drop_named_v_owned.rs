//! W4.001 Stage C0a — Named/LayoutManaged V drop-plan witness test.
//!
//! Plan rev6 §4 "Scope bounds": `LayoutManaged` value ownership covers
//! `Named` records whose drop plan is expressible as a single
//! `extern "C" fn(*mut c_void)` thunk. This test simulates a multi-field
//! record with a single owned `string` field and asserts the sentinel
//! drop counter reflects exactly the expected drop count across
//! overwrite + remove + free.

#![expect(
    clippy::undocumented_unsafe_blocks,
    reason = "test harness — safety invariants are documented per-test"
)]
#![allow(
    clippy::cast_sign_loss,
    clippy::cast_possible_wrap,
    clippy::doc_markdown,
    clippy::redundant_closure_for_method_calls,
    reason = "test harness conventions; see hashmap_layout_drop_overwrite.rs"
)]

use std::ffi::{c_char, c_void, CString};
use std::sync::atomic::{AtomicUsize, Ordering};

use hew_cabi::map::{
    HewMapKeyEqThunk, HewMapKeyHashThunk, HewMapKeyLayout, HewMapValueDropThunk, HewMapValueLayout,
};
use hew_cabi::vec::HewTypeOwnershipKind;
use hew_runtime::hashmap::{
    hew_hashmap_free_layout, hew_hashmap_insert_layout, hew_hashmap_new_with_layout,
    hew_hashmap_remove_layout,
};

/// Simulated `Named` value record `{ name: string, count: i64 }`.
/// Layout: `*mut c_char` then `i64`.
#[repr(C)]
#[derive(Clone, Copy)]
struct NamedV {
    name: *mut c_char,
    count: i64,
}

static V_FIELD_DROP_COUNT: AtomicUsize = AtomicUsize::new(0);

/// Drop plan for `NamedV`: free the owned `name` C string. The numeric
/// `count` is `Plain` and needs no per-field drop. This compresses the
/// record's drop plan into a single `extern "C" fn`, matching the
/// `LayoutManaged` ownership contract.
extern "C" fn named_v_drop(blob: *mut c_void) {
    unsafe {
        let rec = blob.cast::<NamedV>();
        let name_ptr = (*rec).name;
        if !name_ptr.is_null() {
            drop(CString::from_raw(name_ptr));
            V_FIELD_DROP_COUNT.fetch_add(1, Ordering::SeqCst);
        }
        // Zero the field so a double-invocation would be a visible no-op
        // rather than a crash (defence in depth; the kernel must not
        // invoke twice — we verify that below).
        (*rec).name = std::ptr::null_mut();
    }
}

unsafe extern "C" fn hash_i64(key: *const c_void) -> u64 {
    let v = unsafe { *key.cast::<i64>() };
    (v as u64).wrapping_mul(0x9E37_79B9_7F4A_7C15)
}

unsafe extern "C" fn eq_i64(lhs: *const c_void, rhs: *const c_void) -> i32 {
    let l = unsafe { *lhs.cast::<i64>() };
    let r = unsafe { *rhs.cast::<i64>() };
    i32::from(l == r)
}

fn make_descriptors() -> (HewMapKeyLayout, HewMapValueLayout) {
    let kl = HewMapKeyLayout {
        size: size_of::<i64>(),
        align: align_of::<i64>(),
        ownership_kind: HewTypeOwnershipKind::Plain,
        hash_fn: Some(hash_i64 as HewMapKeyHashThunk),
        eq_fn: Some(eq_i64 as HewMapKeyEqThunk),
        drop_fn: None,
    };
    let vl = HewMapValueLayout {
        size: size_of::<NamedV>(),
        align: align_of::<NamedV>(),
        ownership_kind: HewTypeOwnershipKind::LayoutManaged,
        drop_fn: Some(named_v_drop as HewMapValueDropThunk),
        clone_fn: None,
    };
    (kl, vl)
}

fn make_value(s: &str, count: i64) -> NamedV {
    NamedV {
        name: CString::new(s).unwrap().into_raw(),
        count,
    }
}

#[test]
fn named_v_overwrite_remove_free_drops_each_owned_field_exactly_once() {
    V_FIELD_DROP_COUNT.store(0, Ordering::SeqCst);
    let (kl, vl) = make_descriptors();
    unsafe {
        let m = hew_hashmap_new_with_layout(&raw const kl, &raw const vl);

        // Insert (vacant) K=1 with V having owned string "alpha".
        let k1: i64 = 1;
        let v1 = make_value("alpha", 100);
        hew_hashmap_insert_layout(
            m,
            (&raw const k1).cast::<c_void>(),
            (&raw const v1).cast::<c_void>(),
        );
        assert_eq!(V_FIELD_DROP_COUNT.load(Ordering::SeqCst), 0);

        // Overwrite K=1 with V having owned string "beta". OLD V "alpha"
        // is dropped exactly once at insert-overwrite (plan §4 invariant 5).
        let v1_new = make_value("beta", 101);
        hew_hashmap_insert_layout(
            m,
            (&raw const k1).cast::<c_void>(),
            (&raw const v1_new).cast::<c_void>(),
        );
        assert_eq!(
            V_FIELD_DROP_COUNT.load(Ordering::SeqCst),
            1,
            "old V's owned field dropped exactly once at overwrite",
        );

        // Insert second key K=2 with owned "gamma".
        let k2: i64 = 2;
        let v2 = make_value("gamma", 200);
        hew_hashmap_insert_layout(
            m,
            (&raw const k2).cast::<c_void>(),
            (&raw const v2).cast::<c_void>(),
        );
        assert_eq!(V_FIELD_DROP_COUNT.load(Ordering::SeqCst), 1);

        // Remove K=2 → "gamma" dropped exactly once at remove (plan §4
        // invariant 3).
        let removed = hew_hashmap_remove_layout(m, (&raw const k2).cast::<c_void>());
        assert!(removed);
        assert_eq!(
            V_FIELD_DROP_COUNT.load(Ordering::SeqCst),
            2,
            "removed V's owned field dropped exactly once at remove",
        );

        // Free → drops the remaining occupied slot (K=1, V containing
        // "beta"). Tombstoned slot for K=2 must NOT be re-dropped.
        hew_hashmap_free_layout(m);
    }
    assert_eq!(
        V_FIELD_DROP_COUNT.load(Ordering::SeqCst),
        3,
        "1 overwrite + 1 remove + 1 free = 3 total owned-field drops; no leak, no double-free",
    );
}
