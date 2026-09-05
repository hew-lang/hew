//! Exact heap-allocation balance for `HashMap.entries()` owned tuple elements.

#![expect(
    clippy::undocumented_unsafe_blocks,
    reason = "FFI ownership test keeps the invariants next to each operation"
)]

use core::ffi::c_void;
use core::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Mutex;

use hew_cabi::map::{
    HewMapKeyEqThunk, HewMapKeyHashThunk, HewMapKeyLayout, HewVecElemCloneThunk,
    HewVecElemDropThunk, HewVecElemLayout,
};
use hew_cabi::vec::HewTypeOwnershipKind;
use hew_runtime::hashmap::{
    hew_hashmap_entries_layout, hew_hashmap_free_layout, hew_hashmap_insert_layout,
    hew_hashmap_new_with_layout,
};
use hew_runtime::vec::hew_vec_free_owned;

static TEST_MUTEX: Mutex<()> = Mutex::new(());
static HEAP_ALLOCS: AtomicUsize = AtomicUsize::new(0);
static HEAP_FREES: AtomicUsize = AtomicUsize::new(0);

#[repr(C)]
#[derive(Clone, Copy)]
struct OwnedValue {
    heap: *mut u64,
}

#[repr(C)]
#[derive(Clone, Copy)]
struct Pair {
    key: i64,
    value: OwnedValue,
}

fn allocate(value: u64) -> OwnedValue {
    HEAP_ALLOCS.fetch_add(1, Ordering::SeqCst);
    OwnedValue {
        heap: Box::into_raw(Box::new(value)),
    }
}

unsafe extern "C" fn clone_value(src: *const c_void, dst: *mut c_void) -> i32 {
    let value = unsafe { *(*src.cast::<OwnedValue>()).heap };
    unsafe { dst.cast::<OwnedValue>().write(allocate(value)) };
    0
}

extern "C" fn drop_value(blob: *mut c_void) {
    let value = unsafe { &mut *blob.cast::<OwnedValue>() };
    if !value.heap.is_null() {
        unsafe { drop(Box::from_raw(value.heap)) };
        value.heap = core::ptr::null_mut();
        HEAP_FREES.fetch_add(1, Ordering::SeqCst);
    }
}

unsafe extern "C" fn clone_pair(src: *const c_void, dst: *mut c_void) -> i32 {
    let src = unsafe { &*src.cast::<Pair>() };
    let dst = dst.cast::<Pair>();
    unsafe {
        dst.write(Pair {
            key: src.key,
            value: allocate(*src.value.heap),
        });
    }
    0
}

unsafe extern "C" fn drop_pair(blob: *mut c_void) {
    let value = unsafe { &mut (*blob.cast::<Pair>()).value };
    drop_value((value as *mut OwnedValue).cast());
}

unsafe extern "C" fn hash_i64(key: *const c_void) -> u64 {
    unsafe { (*key.cast::<i64>()).cast_unsigned() }
}

unsafe extern "C" fn eq_i64(lhs: *const c_void, rhs: *const c_void) -> i32 {
    unsafe { i32::from(*lhs.cast::<i64>() == *rhs.cast::<i64>()) }
}

#[test]
fn owned_entries_allocations_are_freed_exactly_once_after_map_drop() {
    let _guard = TEST_MUTEX
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    HEAP_ALLOCS.store(0, Ordering::SeqCst);
    HEAP_FREES.store(0, Ordering::SeqCst);

    let key_layout = HewMapKeyLayout {
        value: HewVecElemLayout {
            size: size_of::<i64>(),
            align: align_of::<i64>(),
            ownership_kind: HewTypeOwnershipKind::Plain,
            clone_fn: None,
            drop_fn: None,
        },
        hash_fn: Some(hash_i64 as HewMapKeyHashThunk),
        eq_fn: Some(eq_i64 as HewMapKeyEqThunk),
    };
    let value_layout = HewVecElemLayout {
        size: size_of::<OwnedValue>(),
        align: align_of::<OwnedValue>(),
        ownership_kind: HewTypeOwnershipKind::LayoutManaged,
        drop_fn: Some(drop_value as HewVecElemDropThunk),
        clone_fn: Some(clone_value as HewVecElemCloneThunk),
    };
    let pair_layout = HewVecElemLayout {
        size: size_of::<Pair>(),
        align: align_of::<Pair>(),
        ownership_kind: HewTypeOwnershipKind::LayoutManaged,
        clone_fn: Some(clone_pair),
        drop_fn: Some(drop_pair),
    };

    unsafe {
        let map = hew_hashmap_new_with_layout(&raw const key_layout, &raw const value_layout);
        for key in [11_i64, 22_i64] {
            let source = allocate(key.cast_unsigned());
            hew_hashmap_insert_layout(map, (&raw const key).cast(), (&raw const source).cast());
        }

        let entries = hew_hashmap_entries_layout(
            map,
            &raw const pair_layout,
            core::mem::offset_of!(Pair, value) as u64,
        );
        hew_hashmap_free_layout(map);
        hew_vec_free_owned(entries);
    }

    let allocations = HEAP_ALLOCS.load(Ordering::SeqCst);
    let frees = HEAP_FREES.load(Ordering::SeqCst);
    assert!(
        allocations >= 4,
        "the test must exercise map ingress and entries clone allocations"
    );
    assert_eq!(
        frees, allocations,
        "every owned entries allocation must be released"
    );
}
