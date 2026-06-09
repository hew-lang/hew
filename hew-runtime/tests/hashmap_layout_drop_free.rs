//! W4.001 Stage C0a — drop-on-free contract test.
//!
//! Plan rev6 §4 "Acquisition / ownership contract" (free row) +
//! invariant 4: "Free drops every occupied K+V. Tombstoned slots already
//! dropped at remove-time and must not be re-dropped."

#![expect(
    clippy::undocumented_unsafe_blocks,
    reason = "test harness — safety invariants are documented per-test"
)]
#![allow(
    clippy::cast_sign_loss,
    clippy::cast_possible_wrap,
    clippy::cast_possible_truncation,
    clippy::doc_markdown,
    clippy::items_after_statements,
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

fn make_descriptors() -> (HewMapKeyLayout, HewMapValueLayout) {
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
    (kl, vl)
}

#[test]
fn free_drops_every_occupied_kv_exactly_once() {
    let _g = TEST_MUTEX.lock().unwrap_or_else(|p| p.into_inner());
    K_DROP_COUNT.store(0, Ordering::SeqCst);
    V_DROP_COUNT.store(0, Ordering::SeqCst);

    let (kl, vl) = make_descriptors();
    const N: i64 = 10;
    unsafe {
        let m = hew_hashmap_new_with_layout(&raw const kl, &raw const vl);
        for i in 0..N {
            let v = i.wrapping_mul(2);
            hew_hashmap_insert_layout(
                m,
                (&raw const i).cast::<c_void>(),
                (&raw const v).cast::<c_void>(),
            );
        }
        assert_eq!(K_DROP_COUNT.load(Ordering::SeqCst), 0);
        assert_eq!(V_DROP_COUNT.load(Ordering::SeqCst), 0);
        hew_hashmap_free_layout(m);
    }
    assert_eq!(K_DROP_COUNT.load(Ordering::SeqCst), N as usize);
    assert_eq!(V_DROP_COUNT.load(Ordering::SeqCst), N as usize);
}

#[test]
fn free_skips_tombstoned_slots() {
    let _g = TEST_MUTEX.lock().unwrap_or_else(|p| p.into_inner());
    K_DROP_COUNT.store(0, Ordering::SeqCst);
    V_DROP_COUNT.store(0, Ordering::SeqCst);

    let (kl, vl) = make_descriptors();
    const N: i64 = 8;
    const REMOVED: i64 = 3; // remove 3 of the 8 entries
    unsafe {
        let m = hew_hashmap_new_with_layout(&raw const kl, &raw const vl);
        for i in 0..N {
            let v = i.wrapping_mul(2);
            hew_hashmap_insert_layout(
                m,
                (&raw const i).cast::<c_void>(),
                (&raw const v).cast::<c_void>(),
            );
        }
        // Remove some entries: each removal increments {K,V} drop counters
        // by 1 (verified by the dedicated remove test). After all removals
        // the K/V counters should equal REMOVED.
        for i in 0..REMOVED {
            let removed = hew_hashmap_remove_layout(m, (&raw const i).cast::<c_void>());
            assert!(removed);
        }
        assert_eq!(K_DROP_COUNT.load(Ordering::SeqCst), REMOVED as usize);
        assert_eq!(V_DROP_COUNT.load(Ordering::SeqCst), REMOVED as usize);

        // Free: drops the (N - REMOVED) still-occupied entries. Tombstoned
        // slots must not be re-dropped (invariant 4).
        hew_hashmap_free_layout(m);
    }
    let expected = N as usize;
    assert_eq!(
        K_DROP_COUNT.load(Ordering::SeqCst),
        expected,
        "REMOVED at remove-time + (N-REMOVED) at free-time == N total; no tombstone re-drop",
    );
    assert_eq!(V_DROP_COUNT.load(Ordering::SeqCst), expected);
}

#[test]
fn free_plain_descriptor_invokes_no_drops() {
    let _g = TEST_MUTEX.lock().unwrap_or_else(|p| p.into_inner());
    // Plain ownership + drop_fn=None: the per-slot drop loop is the
    // expected fast-path skip (validated indirectly by `hashmap_run_pass.hew`
    // staying exit 0 across this change; here we add an explicit counter
    // baseline).
    K_DROP_COUNT.store(0, Ordering::SeqCst);
    V_DROP_COUNT.store(0, Ordering::SeqCst);
    let kl = HewMapKeyLayout {
        size: size_of::<i64>(),
        align: align_of::<i64>(),
        ownership_kind: HewTypeOwnershipKind::Plain,
        hash_fn: Some(hash_i64 as HewMapKeyHashThunk),
        eq_fn: Some(eq_i64 as HewMapKeyEqThunk),
        drop_fn: None,
    };
    let vl = HewMapValueLayout {
        size: size_of::<i64>(),
        align: align_of::<i64>(),
        ownership_kind: HewTypeOwnershipKind::Plain,
        drop_fn: None,
        clone_fn: None,
    };
    unsafe {
        let m = hew_hashmap_new_with_layout(&raw const kl, &raw const vl);
        for i in 0..4i64 {
            let v = i;
            hew_hashmap_insert_layout(
                m,
                (&raw const i).cast::<c_void>(),
                (&raw const v).cast::<c_void>(),
            );
        }
        hew_hashmap_free_layout(m);
    }
    assert_eq!(K_DROP_COUNT.load(Ordering::SeqCst), 0);
    assert_eq!(V_DROP_COUNT.load(Ordering::SeqCst), 0);
}
