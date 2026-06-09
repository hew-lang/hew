//! W4.001 Stage C0a — Descriptor-consistency rejection + immutability test.
//!
//! Plan rev6 §4 "Fail-closed descriptor consistency check" + Blocker B2
//! (by-value snapshot). Two halves:
//!
//! 1. **Rejection (CLAUDE.md §2 fail-closed):** four enumerated cases —
//!    {K, V} × {String, LayoutManaged} with `drop_fn=None` — each panic
//!    with message `"HewLayoutHashMap: {side} ownership_kind={kind}
//!    requires drop_fn"`. Driven through `validate_descriptor_ownership`
//!    so `should_panic` can observe the unwind under the test profile
//!    (the `extern "C"` constructor would abort across the C boundary
//!    under `panic = "abort"`).
//!
//! 2. **Immutability:** construct a well-formed map, mutate the caller's
//!    original descriptor post-construction (zero `drop_fn` etc.), drive
//!    insert/remove/free, and assert the kernel still honours the
//!    snapshot's drop_fn (counter rises as expected). This closes the
//!    raw-pointer hole the rev5 constructor check would otherwise leak
//!    through.

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
    hew_hashmap_remove_layout, validate_descriptor_ownership,
};

// Used only by the immutability test below; declared at module scope so the
// `extern "C"` `snapshot_drop` thunk can find it.
static TEST_MUTEX: Mutex<()> = Mutex::new(());

unsafe extern "C" fn hash_i64(key: *const c_void) -> u64 {
    let v = unsafe { *key.cast::<i64>() };
    (v as u64).wrapping_mul(0x9E37_79B9_7F4A_7C15)
}

unsafe extern "C" fn eq_i64(lhs: *const c_void, rhs: *const c_void) -> i32 {
    let l = unsafe { *lhs.cast::<i64>() };
    let r = unsafe { *rhs.cast::<i64>() };
    i32::from(l == r)
}

static SNAPSHOT_DROP_COUNT: AtomicUsize = AtomicUsize::new(0);

extern "C" fn snapshot_drop(_blob: *mut c_void) {
    SNAPSHOT_DROP_COUNT.fetch_add(1, Ordering::SeqCst);
}

fn plain_v_layout() -> HewMapValueLayout {
    HewMapValueLayout {
        size: 8,
        align: 8,
        ownership_kind: HewTypeOwnershipKind::Plain,
        drop_fn: None,
        clone_fn: None,
    }
}

fn plain_k_layout() -> HewMapKeyLayout {
    HewMapKeyLayout {
        size: 8,
        align: 8,
        ownership_kind: HewTypeOwnershipKind::Plain,
        hash_fn: Some(hash_i64 as HewMapKeyHashThunk),
        eq_fn: Some(eq_i64 as HewMapKeyEqThunk),
        drop_fn: None,
    }
}

// ---------------------------------------------------------------------------
// Half 1: four enumerated rejection cases
// ---------------------------------------------------------------------------

#[test]
#[should_panic(expected = "key_layout ownership_kind=String requires drop_fn")]
fn rejects_string_key_without_drop_fn() {
    let kl = HewMapKeyLayout {
        size: 8,
        align: 8,
        ownership_kind: HewTypeOwnershipKind::String,
        hash_fn: Some(hash_i64 as HewMapKeyHashThunk),
        eq_fn: Some(eq_i64 as HewMapKeyEqThunk),
        drop_fn: None,
    };
    let vl = plain_v_layout();
    unsafe {
        validate_descriptor_ownership(&raw const kl, &raw const vl);
    }
}

#[test]
#[should_panic(expected = "key_layout ownership_kind=LayoutManaged requires drop_fn")]
fn rejects_layout_managed_key_without_drop_fn() {
    let kl = HewMapKeyLayout {
        size: 8,
        align: 8,
        ownership_kind: HewTypeOwnershipKind::LayoutManaged,
        hash_fn: Some(hash_i64 as HewMapKeyHashThunk),
        eq_fn: Some(eq_i64 as HewMapKeyEqThunk),
        drop_fn: None,
    };
    let vl = plain_v_layout();
    unsafe {
        validate_descriptor_ownership(&raw const kl, &raw const vl);
    }
}

#[test]
#[should_panic(expected = "val_layout ownership_kind=String requires drop_fn")]
fn rejects_string_value_without_drop_fn() {
    let kl = plain_k_layout();
    let vl = HewMapValueLayout {
        size: 8,
        align: 8,
        ownership_kind: HewTypeOwnershipKind::String,
        drop_fn: None,
        clone_fn: None,
    };
    unsafe {
        validate_descriptor_ownership(&raw const kl, &raw const vl);
    }
}

#[test]
#[should_panic(expected = "val_layout ownership_kind=LayoutManaged requires drop_fn")]
fn rejects_layout_managed_value_without_drop_fn() {
    let kl = plain_k_layout();
    let vl = HewMapValueLayout {
        size: 8,
        align: 8,
        ownership_kind: HewTypeOwnershipKind::LayoutManaged,
        drop_fn: None,
        clone_fn: None,
    };
    unsafe {
        validate_descriptor_ownership(&raw const kl, &raw const vl);
    }
}

// ---------------------------------------------------------------------------
// Positive controls — Plain ownership with/without drop_fn accepted
// ---------------------------------------------------------------------------

#[test]
fn accepts_plain_with_no_drop_fn() {
    let kl = plain_k_layout();
    let vl = plain_v_layout();
    unsafe {
        validate_descriptor_ownership(&raw const kl, &raw const vl);
    }
}

#[test]
fn accepts_plain_with_drop_fn_no_op() {
    // Harmless direction: Plain + Some(drop) is accepted (no-op cleanup).
    // The relaxed-direction is safe because extra cleanup is harmless;
    // missing cleanup is the leak hazard the rejected cases above guard.
    let kl = HewMapKeyLayout {
        drop_fn: Some(snapshot_drop as HewMapValueDropThunk),
        ..plain_k_layout()
    };
    let vl = HewMapValueLayout {
        drop_fn: Some(snapshot_drop as HewMapValueDropThunk),
        ..plain_v_layout()
    };
    unsafe {
        validate_descriptor_ownership(&raw const kl, &raw const vl);
    }
}

// ---------------------------------------------------------------------------
// Half 2: descriptor immutability (Blocker B2 — snapshot wins)
// ---------------------------------------------------------------------------

#[test]
fn kernel_honours_snapshot_after_caller_mutates_descriptors() {
    let _g = TEST_MUTEX.lock().unwrap_or_else(|p| p.into_inner());
    SNAPSHOT_DROP_COUNT.store(0, Ordering::SeqCst);

    // Caller-owned descriptors, well-formed at construction (drop_fn
    // present, owned ownership kinds). The constructor snapshots both
    // descriptors into the map — see plan rev6 §4 Blocker B2.
    let mut kl = HewMapKeyLayout {
        size: size_of::<i64>(),
        align: align_of::<i64>(),
        ownership_kind: HewTypeOwnershipKind::LayoutManaged,
        hash_fn: Some(hash_i64 as HewMapKeyHashThunk),
        eq_fn: Some(eq_i64 as HewMapKeyEqThunk),
        drop_fn: Some(snapshot_drop as HewMapValueDropThunk),
    };
    let mut vl = HewMapValueLayout {
        size: size_of::<i64>(),
        align: align_of::<i64>(),
        ownership_kind: HewTypeOwnershipKind::LayoutManaged,
        drop_fn: Some(snapshot_drop as HewMapValueDropThunk),
        clone_fn: None,
    };

    unsafe {
        let m = hew_hashmap_new_with_layout(&raw const kl, &raw const vl);

        // Mutate the caller's descriptor bytes AFTER construction. A
        // pointer-storing implementation would now silently switch to
        // these mutated bytes; the by-value snapshot must ignore the
        // change. The `#[allow(unused_assignments)]` makes explicit that
        // we never read these locals back — the test's point is precisely
        // that the kernel doesn't either.
        #[allow(
            unused_assignments,
            reason = "we mutate kl/vl precisely to assert the kernel ignores the change"
        )]
        {
            kl.drop_fn = None;
            kl.ownership_kind = HewTypeOwnershipKind::Plain;
            vl.drop_fn = None;
            vl.ownership_kind = HewTypeOwnershipKind::Plain;
        }

        // Drive insert / remove / free. The kernel must invoke the
        // *snapshot's* drop_fn (snapshot_drop), not the mutated `None`.
        let k: i64 = 1;
        let v: i64 = 100;
        hew_hashmap_insert_layout(
            m,
            (&raw const k).cast::<c_void>(),
            (&raw const v).cast::<c_void>(),
        );
        assert_eq!(SNAPSHOT_DROP_COUNT.load(Ordering::SeqCst), 0);

        let k2: i64 = 2;
        hew_hashmap_insert_layout(
            m,
            (&raw const k2).cast::<c_void>(),
            (&raw const v).cast::<c_void>(),
        );

        // Remove K=1 → snapshot drop_fn invoked twice (1× K, 1× V).
        let removed = hew_hashmap_remove_layout(m, (&raw const k).cast::<c_void>());
        assert!(removed);
        assert_eq!(
            SNAPSHOT_DROP_COUNT.load(Ordering::SeqCst),
            2,
            "remove invokes snapshot's K + V drop_fn even though caller \
             mutated their original descriptor to None",
        );

        // Free → drops K=2 + V → counter += 2.
        hew_hashmap_free_layout(m);
    }
    assert_eq!(
        SNAPSHOT_DROP_COUNT.load(Ordering::SeqCst),
        4,
        "snapshot-by-value invariant: kernel must NOT pick up caller's \
         post-construction mutation; total drops = 2 (remove K+V) + \
         2 (free K+V on the one remaining slot) = 4",
    );
}
