use core::ffi::c_void;
use core::sync::atomic::{AtomicUsize, Ordering};

use hew_cabi::map::{HewMapKeyLayout, HewMapValueLayout};
use hew_cabi::vec::{HewTypeOwnershipKind, HewVec, HewVecElemLayout};
use hew_runtime::hashmap::{
    hew_hashmap_free_layout, hew_hashmap_insert_layout, hew_hashmap_new_with_layout,
};
use hew_runtime::vec::{hew_vec_free_owned, hew_vec_new_with_elem_layout, hew_vec_push_owned};

static MAP_VALUE_DROPS: AtomicUsize = AtomicUsize::new(0);
static INNER_ELEMENT_DROPS: AtomicUsize = AtomicUsize::new(0);

unsafe extern "C" fn hash_i64(key: *const c_void) -> u64 {
    // SAFETY: descriptor users pass an i64 key blob.
    unsafe { (*key.cast::<i64>()).cast_unsigned() }
}

unsafe extern "C" fn eq_i64(lhs: *const c_void, rhs: *const c_void) -> i32 {
    // SAFETY: descriptor users pass i64 key blobs.
    unsafe { i32::from(*lhs.cast::<i64>() == *rhs.cast::<i64>()) }
}

unsafe extern "C" fn clone_i64(_src: *const c_void, _dst: *mut c_void) -> i32 {
    0
}

unsafe extern "C" fn drop_i64(_slot: *mut c_void) {
    INNER_ELEMENT_DROPS.fetch_add(1, Ordering::SeqCst);
}

extern "C" fn drop_vec_value(slot: *mut c_void) {
    MAP_VALUE_DROPS.fetch_add(1, Ordering::SeqCst);
    // SAFETY: the map value blob stores one owned HewVec pointer.
    unsafe {
        let vec = *slot.cast::<*mut HewVec>();
        hew_vec_free_owned(vec);
    }
}

#[test]
fn hashmap_of_vec_drops_every_nested_element_exactly_once() {
    MAP_VALUE_DROPS.store(0, Ordering::SeqCst);
    INNER_ELEMENT_DROPS.store(0, Ordering::SeqCst);

    let key_layout = HewMapKeyLayout {
        size: size_of::<i64>(),
        align: align_of::<i64>(),
        ownership_kind: HewTypeOwnershipKind::Plain,
        hash_fn: Some(hash_i64),
        eq_fn: Some(eq_i64),
        drop_fn: None,
    };
    let value_layout = HewMapValueLayout {
        size: size_of::<*mut HewVec>(),
        align: align_of::<*mut HewVec>(),
        ownership_kind: HewTypeOwnershipKind::LayoutManaged,
        drop_fn: Some(drop_vec_value),
        clone_fn: None,
    };
    let elem_layout = HewVecElemLayout {
        size: size_of::<i64>(),
        align: align_of::<i64>(),
        ownership_kind: HewTypeOwnershipKind::LayoutManaged,
        clone_fn: Some(clone_i64),
        drop_fn: Some(drop_i64),
    };

    // SAFETY: all descriptors and inserted blobs match their declared layouts.
    unsafe {
        let map = hew_hashmap_new_with_layout(&raw const key_layout, &raw const value_layout);
        for key in 0..3i64 {
            let vec = hew_vec_new_with_elem_layout(&raw const elem_layout);
            for value in [key * 10, key * 10 + 1] {
                hew_vec_push_owned(vec, (&raw const value).cast());
            }
            hew_hashmap_insert_layout(map, (&raw const key).cast(), (&raw const vec).cast());
        }
        hew_hashmap_free_layout(map);
    }

    assert_eq!(MAP_VALUE_DROPS.load(Ordering::SeqCst), 3);
    assert_eq!(INNER_ELEMENT_DROPS.load(Ordering::SeqCst), 6);
}
