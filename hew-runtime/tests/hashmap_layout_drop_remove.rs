//! W4.001 Stage C0a — drop-on-remove contract test.
//!
//! Plan rev6 §4 "Acquisition / ownership contract" (remove row) +
//! invariant 3: "Remove uses borrowed K to find the slot, drops stored
//! K+V." Caller's lookup K is untouched.

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

use std::ffi::c_void;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Mutex;

use hew_cabi::map::{
    HewMapKeyEqThunk, HewMapKeyHashThunk, HewMapKeyLayout, HewMapValueDropThunk, HewMapValueLayout,
};
use hew_cabi::vec::HewTypeOwnershipKind;
use hew_runtime::hashmap::{
    hew_hashmap_free_layout, hew_hashmap_insert_layout, hew_hashmap_new_with_layout,
    hew_hashmap_remove_layout,
};

// Serialise tests within this binary — shared `extern "C"` thunk counters.
static TEST_MUTEX: Mutex<()> = Mutex::new(());

static K_DROP_COUNT: AtomicUsize = AtomicUsize::new(0);
static V_DROP_COUNT: AtomicUsize = AtomicUsize::new(0);

extern "C" fn k_drop_count(_blob: *mut c_void) {
    K_DROP_COUNT.fetch_add(1, Ordering::SeqCst);
}

extern "C" fn v_drop_count(_blob: *mut c_void) {
    V_DROP_COUNT.fetch_add(1, Ordering::SeqCst);
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

#[test]
fn remove_drops_stored_k_and_v_exactly_once_each() {
    let _g = TEST_MUTEX.lock().unwrap_or_else(|p| p.into_inner());
    K_DROP_COUNT.store(0, Ordering::SeqCst);
    V_DROP_COUNT.store(0, Ordering::SeqCst);

    let kl = HewMapKeyLayout {
        size: size_of::<i64>(),
        align: align_of::<i64>(),
        ownership_kind: HewTypeOwnershipKind::LayoutManaged,
        hash_fn: Some(hash_i64 as HewMapKeyHashThunk),
        eq_fn: Some(eq_i64 as HewMapKeyEqThunk),
        drop_fn: Some(k_drop_count as HewMapValueDropThunk),
    };
    let vl = HewMapValueLayout {
        size: size_of::<i64>(),
        align: align_of::<i64>(),
        ownership_kind: HewTypeOwnershipKind::LayoutManaged,
        drop_fn: Some(v_drop_count as HewMapValueDropThunk),
        clone_fn: None,
    };

    unsafe {
        let m = hew_hashmap_new_with_layout(&raw const kl, &raw const vl);
        let key: i64 = 42;
        let v: i64 = 100;
        hew_hashmap_insert_layout(
            m,
            (&raw const key).cast::<c_void>(),
            (&raw const v).cast::<c_void>(),
        );
        assert_eq!(K_DROP_COUNT.load(Ordering::SeqCst), 0);
        assert_eq!(V_DROP_COUNT.load(Ordering::SeqCst), 0);

        // Caller's lookup-K (borrowed) — kernel must not drop this.
        let lookup_key: i64 = 42;
        let removed = hew_hashmap_remove_layout(m, (&raw const lookup_key).cast::<c_void>());
        assert!(removed);
        assert_eq!(
            K_DROP_COUNT.load(Ordering::SeqCst),
            1,
            "stored K dropped exactly once at remove",
        );
        assert_eq!(
            V_DROP_COUNT.load(Ordering::SeqCst),
            1,
            "stored V dropped exactly once at remove",
        );

        // Tombstoned slot must not be re-dropped at free.
        hew_hashmap_free_layout(m);
        assert_eq!(K_DROP_COUNT.load(Ordering::SeqCst), 1);
        assert_eq!(V_DROP_COUNT.load(Ordering::SeqCst), 1);
    }
}

#[test]
fn remove_missing_key_invokes_no_drops() {
    let _g = TEST_MUTEX.lock().unwrap_or_else(|p| p.into_inner());
    K_DROP_COUNT.store(0, Ordering::SeqCst);
    V_DROP_COUNT.store(0, Ordering::SeqCst);
    let kl = HewMapKeyLayout {
        size: size_of::<i64>(),
        align: align_of::<i64>(),
        ownership_kind: HewTypeOwnershipKind::LayoutManaged,
        hash_fn: Some(hash_i64 as HewMapKeyHashThunk),
        eq_fn: Some(eq_i64 as HewMapKeyEqThunk),
        drop_fn: Some(k_drop_count as HewMapValueDropThunk),
    };
    let vl = HewMapValueLayout {
        size: size_of::<i64>(),
        align: align_of::<i64>(),
        ownership_kind: HewTypeOwnershipKind::LayoutManaged,
        drop_fn: Some(v_drop_count as HewMapValueDropThunk),
        clone_fn: None,
    };
    unsafe {
        let m = hew_hashmap_new_with_layout(&raw const kl, &raw const vl);
        let absent: i64 = 999;
        let removed = hew_hashmap_remove_layout(m, (&raw const absent).cast::<c_void>());
        assert!(!removed);
        assert_eq!(K_DROP_COUNT.load(Ordering::SeqCst), 0);
        assert_eq!(V_DROP_COUNT.load(Ordering::SeqCst), 0);
        hew_hashmap_free_layout(m);
    }
}
