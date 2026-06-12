//! W4.001 Stage C0a — drop-on-overwrite contract test.
//!
//! Plan rev6 §4 "Acquisition / ownership contract" (occupied-slot row) +
//! invariant 5: "Overwrite never drops K, always drops old V."
//!
//! This test installs a `HewMapKeyLayout` with a counter-incrementing
//! `drop_fn` for K and a `HewMapValueLayout` with a counter-incrementing
//! `drop_fn` for V, then performs an insert + overwrite and asserts:
//!
//! - The OLD V's drop_fn was invoked exactly once (the overwrite-time drop).
//! - The stored K's drop_fn was invoked exactly zero times (slot K is
//!   reused; duplicate K_in is the caller's responsibility, hoisted to the
//!   Stage C HIR consumer per the contract table).
//!
//! Q281=A discipline: the drop thunk is a bare `extern "C" fn(*mut c_void)`,
//! incrementing an `AtomicUsize` static. No closures, no trait objects.

#![expect(
    clippy::undocumented_unsafe_blocks,
    reason = "test harness — safety invariants are documented per-test"
)]
#![allow(
    clippy::cast_sign_loss,
    clippy::cast_possible_wrap,
    clippy::doc_markdown,
    clippy::redundant_closure_for_method_calls,
    reason = "tests cast scalar literals freely, doc-comment narrative refers \
              to Rust identifiers without backticks, and the |p| p.into_inner() \
              closure form is clearer at the call site than the method-path form"
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
};

// Serialise tests within this binary — they share `K_DROP_COUNT` /
// `V_DROP_COUNT` statics for the bare `extern "C"` thunks (Q281=A; no
// closures). Parallel execution would race the counter reads.
static TEST_MUTEX: Mutex<()> = Mutex::new(());

// Counters live as `AtomicUsize` statics so the bare `extern "C"` thunks
// can mutate them without any closure state.
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
fn overwrite_drops_old_v_once_and_never_drops_stored_k() {
    let _g = TEST_MUTEX.lock().unwrap_or_else(|p| p.into_inner());
    K_DROP_COUNT.store(0, Ordering::SeqCst);
    V_DROP_COUNT.store(0, Ordering::SeqCst);

    // Declare K and V as "owned" (LayoutManaged) so the constructor accepts
    // the drop_fn. We do not actually own external allocations; the drop
    // thunks only bump counters, which is the harmless-direction Plain
    // wouldn't even invoke.
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
        let v1: i64 = 100;
        let v2: i64 = 200;

        // Insert (vacant): no drops at all.
        let was_new = hew_hashmap_insert_layout(
            m,
            (&raw const key).cast::<c_void>(),
            (&raw const v1).cast::<c_void>(),
        );
        assert!(was_new, "first insert reports vacant slot");
        assert_eq!(K_DROP_COUNT.load(Ordering::SeqCst), 0);
        assert_eq!(V_DROP_COUNT.load(Ordering::SeqCst), 0);

        // Insert (overwrite): old V dropped exactly once; stored K never dropped.
        let was_new = hew_hashmap_insert_layout(
            m,
            (&raw const key).cast::<c_void>(),
            (&raw const v2).cast::<c_void>(),
        );
        assert!(!was_new, "overwrite reports occupied slot");
        assert_eq!(
            K_DROP_COUNT.load(Ordering::SeqCst),
            0,
            "Plan rev6 §4 invariant 5: stored K is reused on overwrite, never \
             dropped by the kernel; duplicate K_in is the caller's responsibility",
        );
        assert_eq!(
            V_DROP_COUNT.load(Ordering::SeqCst),
            1,
            "old V must be dropped exactly once at insert-overwrite",
        );

        // Free: now drops K + V on the one remaining occupied slot, bringing
        // counters to {K=1, V=2}. This also verifies the free-time hook does
        // not skip the slot that survived the overwrite.
        hew_hashmap_free_layout(m);
        assert_eq!(K_DROP_COUNT.load(Ordering::SeqCst), 1);
        assert_eq!(V_DROP_COUNT.load(Ordering::SeqCst), 2);
    }
}

#[test]
fn plain_v_overwrite_does_not_invoke_drop() {
    let _g = TEST_MUTEX.lock().unwrap_or_else(|p| p.into_inner());
    // Baseline: Plain ownership with drop_fn=None ⇒ no per-slot drop calls.
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
        let key: i64 = 7;
        let v1: i64 = 1;
        let v2: i64 = 2;
        hew_hashmap_insert_layout(
            m,
            (&raw const key).cast::<c_void>(),
            (&raw const v1).cast::<c_void>(),
        );
        hew_hashmap_insert_layout(
            m,
            (&raw const key).cast::<c_void>(),
            (&raw const v2).cast::<c_void>(),
        );
        hew_hashmap_free_layout(m);
    }
    assert_eq!(
        V_DROP_COUNT.load(Ordering::SeqCst),
        0,
        "Plain ownership descriptor with drop_fn=None must never invoke the thunk",
    );
}
