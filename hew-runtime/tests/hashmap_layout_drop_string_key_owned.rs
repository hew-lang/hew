//! W4.001 Stage C0a — String-key ownership witness test.
//!
//! Plan rev6 §4 "Mapping to the equivalent legacy semantics": the layout
//! kernel only moves bytes; the caller is responsible for producing owned
//! K + V bytes (via `strdup`/Box-into-raw/Layout-record clone) BEFORE
//! calling `hew_hashmap_insert_layout`.
//!
//! This test confirms:
//! - On insert (vacant): the kernel takes possession of the caller-provided
//!   owned K_in (no further caller cleanup needed).
//! - On insert (overwrite): the kernel does NOT consume / drop K_in — the
//!   caller still owns it and must free it (the test does so explicitly
//!   and asserts no double-free via the counter).
//! - On free: the kernel invokes the registered `drop_fn` on every stored
//!   K, freeing the strings the caller transferred at insert time.

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
use std::sync::Mutex;

use hew_cabi::map::{
    HewMapKeyEqThunk, HewMapKeyHashThunk, HewMapKeyLayout, HewMapValueDropThunk, HewMapValueLayout,
};
use hew_cabi::vec::HewTypeOwnershipKind;
use hew_runtime::hashmap::{
    hew_hashmap_free_layout, hew_hashmap_insert_layout, hew_hashmap_new_with_layout,
};

// Serialise tests within this binary — shared `extern "C"` thunk counter.
static TEST_MUTEX: Mutex<()> = Mutex::new(());

static STRING_FREE_COUNT: AtomicUsize = AtomicUsize::new(0);

/// Drop thunk for a `String`-ownership K layout: the blob is the address of
/// a `*mut c_char` pointer (slot stores a pointer, not the chars themselves).
/// We `CString::from_raw` the pointed-to char* to reclaim and free it.
extern "C" fn string_key_drop(blob: *mut c_void) {
    // SAFETY: the slot is `*mut c_char` (size = pointer size); the kernel
    // hands us the slot address. Reading it gives the heap-owned char* the
    // caller transferred at insert-time.
    unsafe {
        let cstr_ptr = *blob.cast::<*mut c_char>();
        if !cstr_ptr.is_null() {
            drop(CString::from_raw(cstr_ptr));
            STRING_FREE_COUNT.fetch_add(1, Ordering::SeqCst);
        }
    }
}

unsafe extern "C" fn hash_cstr_slot(blob: *const c_void) -> u64 {
    // Slot stores `*const c_char`; the actual chars live elsewhere.
    let cstr_ptr = unsafe { *blob.cast::<*const c_char>() };
    // FNV-1a over the C string bytes.
    let mut h: u64 = 0xcbf2_9ce4_8422_2325;
    let mut p = cstr_ptr;
    unsafe {
        while *p != 0 {
            h ^= u64::from(*p as u8);
            h = h.wrapping_mul(0x0000_0100_0000_01B3);
            p = p.add(1);
        }
    }
    h
}

unsafe extern "C" fn eq_cstr_slot(lhs: *const c_void, rhs: *const c_void) -> i32 {
    let l = unsafe { *lhs.cast::<*const c_char>() };
    let r = unsafe { *rhs.cast::<*const c_char>() };
    if l == r {
        return 1;
    }
    unsafe {
        let mut lp = l;
        let mut rp = r;
        loop {
            let lc = *lp;
            let rc = *rp;
            if lc != rc {
                return 0;
            }
            if lc == 0 {
                return 1;
            }
            lp = lp.add(1);
            rp = rp.add(1);
        }
    }
}

fn make_descriptors() -> (HewMapKeyLayout, HewMapValueLayout) {
    let kl = HewMapKeyLayout {
        size: size_of::<*mut c_char>(),
        align: align_of::<*mut c_char>(),
        ownership_kind: HewTypeOwnershipKind::String,
        hash_fn: Some(hash_cstr_slot as HewMapKeyHashThunk),
        eq_fn: Some(eq_cstr_slot as HewMapKeyEqThunk),
        drop_fn: Some(string_key_drop as HewMapValueDropThunk),
    };
    let vl = HewMapValueLayout {
        size: size_of::<i32>(),
        align: align_of::<i32>(),
        ownership_kind: HewTypeOwnershipKind::Plain,
        drop_fn: None,
        clone_fn: None,
    };
    (kl, vl)
}

#[test]
fn string_key_is_consumed_on_insert_vacant() {
    let _g = TEST_MUTEX.lock().unwrap_or_else(|p| p.into_inner());
    STRING_FREE_COUNT.store(0, Ordering::SeqCst);
    let (kl, vl) = make_descriptors();
    unsafe {
        let m = hew_hashmap_new_with_layout(&raw const kl, &raw const vl);

        // Caller produces an owned C string and stages it in a slot-shaped
        // local (a `*mut c_char` is what the kernel will copy into the slot).
        let owned_key = CString::new("hello").unwrap().into_raw();
        let key_slot: *mut c_char = owned_key;
        let v: i32 = 42;
        let was_new = hew_hashmap_insert_layout(
            m,
            (&raw const key_slot).cast::<c_void>(),
            (&raw const v).cast::<c_void>(),
        );
        assert!(was_new);
        assert_eq!(
            STRING_FREE_COUNT.load(Ordering::SeqCst),
            0,
            "vacant insert transfers ownership; caller must NOT free K_in",
        );

        // Free: kernel invokes drop_fn on the stored K → frees the string once.
        hew_hashmap_free_layout(m);
        assert_eq!(
            STRING_FREE_COUNT.load(Ordering::SeqCst),
            1,
            "free invokes the K drop_fn on the transferred string exactly once",
        );
    }
}

#[test]
fn string_key_caller_owns_duplicate_on_insert_overwrite() {
    let _g = TEST_MUTEX.lock().unwrap_or_else(|p| p.into_inner());
    STRING_FREE_COUNT.store(0, Ordering::SeqCst);
    let (kl, vl) = make_descriptors();
    unsafe {
        let m = hew_hashmap_new_with_layout(&raw const kl, &raw const vl);

        // Vacant insert: transfer first allocation.
        let first = CString::new("dup").unwrap().into_raw();
        let first_slot: *mut c_char = first;
        let v1: i32 = 1;
        hew_hashmap_insert_layout(
            m,
            (&raw const first_slot).cast::<c_void>(),
            (&raw const v1).cast::<c_void>(),
        );
        assert_eq!(STRING_FREE_COUNT.load(Ordering::SeqCst), 0);

        // Overwrite with a fresh equal-content K_in. Per plan rev6 §4
        // contract-table occupied-slot row + invariant 5: kernel does NOT
        // drop the stored K (it stays in the slot); it also does NOT drop
        // K_in (caller's responsibility). So STRING_FREE_COUNT stays 0
        // until the caller explicitly frees K_in.
        let duplicate = CString::new("dup").unwrap().into_raw();
        let duplicate_slot: *mut c_char = duplicate;
        let v2: i32 = 2;
        let was_new = hew_hashmap_insert_layout(
            m,
            (&raw const duplicate_slot).cast::<c_void>(),
            (&raw const v2).cast::<c_void>(),
        );
        assert!(!was_new, "overwrite reports occupied slot");
        assert_eq!(
            STRING_FREE_COUNT.load(Ordering::SeqCst),
            0,
            "kernel does NOT free stored K (reused) or K_in (caller's responsibility)",
        );

        // Caller now drops their unused duplicate K_in (hoisted-to-HIR
        // responsibility, simulated here by the test as the caller).
        drop(CString::from_raw(duplicate));
        STRING_FREE_COUNT.fetch_add(1, Ordering::SeqCst);

        // Free: kernel drops the stored K (`first`) exactly once → counter +1.
        hew_hashmap_free_layout(m);
    }
    assert_eq!(
        STRING_FREE_COUNT.load(Ordering::SeqCst),
        2,
        "1 caller-side drop of K_in + 1 kernel-side drop of stored K at free; no double-free",
    );
}
