//! Composite keys and values share collection copy/drop semantics.

use core::ffi::c_void;
use core::mem::{offset_of, MaybeUninit};
use core::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Mutex;

use hew_cabi::map::HewMapKeyLayout;
use hew_cabi::string::{
    string_as_bytes, string_as_str, string_from_str, string_release, HewString,
};
use hew_cabi::vec::{HewTypeOwnershipKind, HewVecElemLayout};
use hew_runtime::{hashmap, hashset, vec};

static TEST_LOCK: Mutex<()> = Mutex::new(());
static LIVE_NUMBERS: AtomicUsize = AtomicUsize::new(0);

#[repr(C)]
struct Record {
    label: *mut HewString,
    number: *mut i64,
}

#[repr(C)]
struct Pair {
    key: Record,
    value: Record,
}

fn record(label: &str, number: i64) -> Record {
    LIVE_NUMBERS.fetch_add(1, Ordering::SeqCst);
    Record {
        label: string_from_str(label),
        number: Box::into_raw(Box::new(number)),
    }
}

unsafe extern "C" fn clone_record(source: *const c_void, destination: *mut c_void) -> i32 {
    // SAFETY: the descriptor uses these callbacks only for live Record slots.
    unsafe {
        let source = &*source.cast::<Record>();
        destination
            .cast::<Record>()
            .write(record(string_as_str(source.label), *source.number));
    }
    0
}

unsafe extern "C" fn drop_record(slot: *mut c_void) {
    // SAFETY: the collection transfers one complete Record owner to this callback.
    unsafe {
        let value = slot.cast::<Record>().read();
        string_release(value.label);
        drop(Box::from_raw(value.number));
    }
    LIVE_NUMBERS.fetch_sub(1, Ordering::SeqCst);
}

unsafe extern "C" fn hash_record(slot: *const c_void) -> u64 {
    // SAFETY: hash borrows the complete key and reads only its typed fields.
    unsafe {
        let value = &*slot.cast::<Record>();
        let mut hash = (*value.number).cast_unsigned();
        for byte in string_as_bytes(value.label) {
            hash = hash.wrapping_mul(31).wrapping_add(u64::from(*byte));
        }
        hash
    }
}

unsafe extern "C" fn equal_record(left: *const c_void, right: *const c_void) -> i32 {
    // SAFETY: both arguments borrow Record keys with live owned fields.
    unsafe {
        let left = &*left.cast::<Record>();
        let right = &*right.cast::<Record>();
        i32::from(
            *left.number == *right.number
                && string_as_bytes(left.label) == string_as_bytes(right.label),
        )
    }
}

unsafe extern "C" fn clone_pair(source: *const c_void, destination: *mut c_void) -> i32 {
    // SAFETY: these are Pair slots; each field uses the same Record descriptor.
    unsafe {
        let source = &*source.cast::<Pair>();
        let destination = destination.cast::<Pair>();
        clone_record(
            (&raw const source.key).cast(),
            (&raw mut (*destination).key).cast(),
        );
        clone_record(
            (&raw const source.value).cast(),
            (&raw mut (*destination).value).cast(),
        );
    }
    0
}

unsafe extern "C" fn drop_pair(slot: *mut c_void) {
    // SAFETY: the pair owns both complete fields, whose inline slots stay allocated here.
    unsafe {
        let pair = slot.cast::<Pair>();
        drop_record((&raw mut (*pair).key).cast());
        drop_record((&raw mut (*pair).value).cast());
    }
}

const RECORD_LAYOUT: HewVecElemLayout = HewVecElemLayout {
    size: size_of::<Record>(),
    align: align_of::<Record>(),
    ownership_kind: HewTypeOwnershipKind::LayoutManaged,
    clone_fn: Some(clone_record),
    drop_fn: Some(drop_record),
};

const KEY_LAYOUT: HewMapKeyLayout = HewMapKeyLayout {
    value: RECORD_LAYOUT,
    hash_fn: Some(hash_record),
    eq_fn: Some(equal_record),
};

const PAIR_LAYOUT: HewVecElemLayout = HewVecElemLayout {
    size: size_of::<Pair>(),
    align: align_of::<Pair>(),
    ownership_kind: HewTypeOwnershipKind::LayoutManaged,
    clone_fn: Some(clone_pair),
    drop_fn: Some(drop_pair),
};

#[test]
fn map_composite_copies_and_projections_survive_their_source() {
    let _guard = TEST_LOCK.lock().unwrap();
    assert_eq!(LIVE_NUMBERS.load(Ordering::SeqCst), 0);
    // SAFETY: every descriptor matches the concrete slots passed below. Insert
    // moves each input owner, extraction writes fresh storage, and all resulting
    // maps, vectors and standalone records have one balancing release.
    unsafe {
        let original = hashmap::hew_hashmap_new_with_layout(&KEY_LAYOUT, &RECORD_LAYOUT);
        for number in 0..32 {
            let key = record("clé\0key", number);
            let value = record("雪\0value", number * 10);
            assert!(hashmap::hew_hashmap_insert_layout(
                original,
                (&raw const key).cast(),
                (&raw const value).cast(),
            ));
        }
        let copy = hashmap::hew_hashmap_clone_layout(original);
        let keys = hashmap::hew_hashmap_keys_layout(original);
        let values = hashmap::hew_hashmap_values_layout(original);
        let entries = hashmap::hew_hashmap_entries_layout(
            original,
            &PAIR_LAYOUT,
            offset_of!(Pair, value) as u64,
        );
        hashmap::hew_hashmap_free_layout(original);

        let mut lookup = record("clé\0key", 7);
        let mut extracted = MaybeUninit::<Record>::uninit();
        assert!(hashmap::hew_hashmap_get_clone_layout(
            copy,
            (&raw const lookup).cast(),
            extracted.as_mut_ptr().cast(),
        ));
        let mut extracted = extracted.assume_init();
        assert_eq!(string_as_str(extracted.label), "雪\0value");
        assert_eq!(*extracted.number, 70);
        *extracted.number = 999;

        let mut removed = MaybeUninit::<Record>::uninit();
        assert!(hashmap::hew_hashmap_remove_take_layout(
            copy,
            (&raw const lookup).cast(),
            removed.as_mut_ptr().cast(),
        ));
        let mut removed = removed.assume_init();
        assert_eq!(*removed.number, 70);
        hashmap::hew_hashmap_free_layout(copy);
        assert_eq!(string_as_str(removed.label), "雪\0value");
        drop_record((&raw mut lookup).cast());
        drop_record((&raw mut extracted).cast());
        drop_record((&raw mut removed).cast());

        assert_eq!(vec::hew_vec_len(keys), 32);
        assert_eq!(vec::hew_vec_len(values), 32);
        assert_eq!(vec::hew_vec_len(entries), 32);
        for index in 0..32 {
            let key = &*vec::hew_vec_get_owned(keys, index).cast::<Record>();
            let value = &*vec::hew_vec_get_owned(values, index).cast::<Record>();
            let pair = &*vec::hew_vec_get_owned(entries, index).cast::<Pair>();
            assert_eq!(string_as_str(key.label), "clé\0key");
            assert_eq!(string_as_str(value.label), "雪\0value");
            assert_eq!(*value.number, *key.number * 10);
            assert_eq!(*pair.value.number, *pair.key.number * 10);
        }
        vec::hew_vec_free_owned(keys);
        vec::hew_vec_free_owned(values);
        vec::hew_vec_free_owned(entries);
    }
    assert_eq!(LIVE_NUMBERS.load(Ordering::SeqCst), 0);
}

#[test]
fn set_composite_copy_and_projection_preserve_independent_elements() {
    let _guard = TEST_LOCK.lock().unwrap();
    assert_eq!(LIVE_NUMBERS.load(Ordering::SeqCst), 0);
    // SAFETY: Record owners move into the set. The copy and vector each clone
    // them through RECORD_LAYOUT and each container receives one final release.
    unsafe {
        let original = hashset::hew_hashset_new_with_layout(&KEY_LAYOUT);
        for number in 0..4 {
            let value = record("set\0element", number);
            assert!(hashset::hew_hashset_insert_layout(
                original,
                (&raw const value).cast()
            ));
        }
        let copy = hashset::hew_hashset_clone_layout(original);
        let elements = hashset::hew_hashset_to_vec_layout(original);
        let borrowed = vec::hew_vec_get_owned(elements, 0);
        assert!(hashset::hew_hashset_remove_layout(original, borrowed));
        assert!(hashset::hew_hashset_contains_layout(copy, borrowed));
        hashset::hew_hashset_free_layout(original);
        hashset::hew_hashset_free_layout(copy);
        assert_eq!(vec::hew_vec_len(elements), 4);
        for index in 0..4 {
            let value = &*vec::hew_vec_get_owned(elements, index).cast::<Record>();
            assert_eq!(string_as_str(value.label), "set\0element");
            assert!((0..4).contains(&*value.number));
        }
        vec::hew_vec_free_owned(elements);
    }
    assert_eq!(LIVE_NUMBERS.load(Ordering::SeqCst), 0);
}

#[test]
fn borrowed_insert_copies_inputs_before_replacement_or_growth() {
    let _guard = TEST_LOCK.lock().unwrap();
    assert_eq!(LIVE_NUMBERS.load(Ordering::SeqCst), 0);
    // SAFETY: inputs remain separately owned because the copy-in entry point
    // borrows them. The iterator's borrowed slots stay live until insertion
    // stages its copies, after which the map may resize and replace them.
    unsafe {
        let map = hashmap::hew_hashmap_new_with_layout(&KEY_LAYOUT, &RECORD_LAYOUT);
        for number in 0..11 {
            let mut key = record("copy\0key", number);
            let mut value = record("copy\0value", number * 10);
            assert!(hashmap::hew_hashmap_insert_clone_layout(
                map,
                (&raw const key).cast(),
                (&raw const value).cast(),
            ));
            *value.number = -1;
            drop_record((&raw mut key).cast());
            drop_record((&raw mut value).cast());
        }
        let old_capacity = (*map).cap;
        let iterator = hashmap::hew_hashmap_iter_new_layout(map);
        let mut borrowed_key = core::ptr::null();
        let mut borrowed_value = core::ptr::null();
        assert!(hashmap::hew_hashmap_iter_next_layout(
            iterator,
            &raw mut borrowed_key,
            &raw mut borrowed_value,
        ));
        hashmap::hew_hashmap_iter_free_layout(iterator);
        assert!(!hashmap::hew_hashmap_insert_clone_layout(
            map,
            borrowed_key,
            borrowed_value
        ));
        assert!((*map).cap > old_capacity);
        assert_eq!(hashmap::hew_hashmap_len_layout(map), 11);

        let entries =
            hashmap::hew_hashmap_entries_layout(map, &PAIR_LAYOUT, offset_of!(Pair, value) as u64);
        hashmap::hew_hashmap_free_layout(map);
        for index in 0..11 {
            let pair = &*vec::hew_vec_get_owned(entries, index).cast::<Pair>();
            assert_eq!(*pair.value.number, *pair.key.number * 10);
        }
        vec::hew_vec_free_owned(entries);
    }
    assert_eq!(LIVE_NUMBERS.load(Ordering::SeqCst), 0);
}

#[test]
fn borrowed_set_insert_retains_the_input_on_both_paths() {
    let _guard = TEST_LOCK.lock().unwrap();
    assert_eq!(LIVE_NUMBERS.load(Ordering::SeqCst), 0);
    // SAFETY: both local Records remain owned, and the set owns an independent copy.
    unsafe {
        let set = hashset::hew_hashset_new_with_layout(&KEY_LAYOUT);
        let mut value = record("set\0copy", 1);
        assert!(hashset::hew_hashset_insert_clone_layout(
            set,
            (&raw const value).cast()
        ));
        assert!(!hashset::hew_hashset_insert_clone_layout(
            set,
            (&raw const value).cast()
        ));
        *value.number = 2;
        let mut lookup = record("set\0copy", 1);
        assert!(hashset::hew_hashset_contains_layout(
            set,
            (&raw const lookup).cast()
        ));
        hashset::hew_hashset_free_layout(set);
        assert_eq!(string_as_str(value.label), "set\0copy");
        drop_record((&raw mut value).cast());
        drop_record((&raw mut lookup).cast());
    }
    assert_eq!(LIVE_NUMBERS.load(Ordering::SeqCst), 0);
}

static ZERO_OWNERS: AtomicUsize = AtomicUsize::new(0);

unsafe extern "C" fn clone_zero(source: *const c_void, destination: *mut c_void) -> i32 {
    assert!(!source.is_null() && !destination.is_null());
    ZERO_OWNERS.fetch_add(1, Ordering::SeqCst);
    0
}

unsafe extern "C" fn drop_zero(slot: *mut c_void) {
    assert!(!slot.is_null());
    ZERO_OWNERS.fetch_sub(1, Ordering::SeqCst);
}

#[test]
fn zero_sized_value_callbacks_follow_logical_owners() {
    let _guard = TEST_LOCK.lock().unwrap();
    assert_eq!(ZERO_OWNERS.load(Ordering::SeqCst), 0);
    let layout = HewVecElemLayout {
        size: 0,
        align: 1,
        ownership_kind: HewTypeOwnershipKind::LayoutManaged,
        clone_fn: Some(clone_zero),
        drop_fn: Some(drop_zero),
    };
    let unit = ();
    // SAFETY: unit slots have no payload bytes. Their callbacks track logical
    // ownership independently of representation size; map keys are plain i64.
    unsafe {
        let map = hashmap::hew_hashmap_new_with_layout(
            &raw const hew_cabi::map::hew_layout_key_i64,
            &raw const layout,
        );
        for key in 0_i64..3 {
            assert!(hashmap::hew_hashmap_insert_clone_layout(
                map,
                (&raw const key).cast(),
                (&raw const unit).cast(),
            ));
        }
        assert_eq!(ZERO_OWNERS.load(Ordering::SeqCst), 3);
        let copy = hashmap::hew_hashmap_clone_layout(map);
        assert_eq!(ZERO_OWNERS.load(Ordering::SeqCst), 6);
        let values = hashmap::hew_hashmap_values_layout(map);
        assert_eq!(vec::hew_vec_len(values), 3);
        assert_eq!(ZERO_OWNERS.load(Ordering::SeqCst), 9);
        let key = 1_i64;
        let mut output = ();
        assert!(hashmap::hew_hashmap_get_clone_layout(
            map,
            (&raw const key).cast(),
            (&raw mut output).cast(),
        ));
        assert_eq!(ZERO_OWNERS.load(Ordering::SeqCst), 10);
        assert!(!hashmap::hew_hashmap_insert_clone_layout(
            map,
            (&raw const key).cast(),
            (&raw const unit).cast(),
        ));
        assert_eq!(ZERO_OWNERS.load(Ordering::SeqCst), 10);
        assert!(hashmap::hew_hashmap_remove_layout(
            map,
            (&raw const key).cast()
        ));
        assert_eq!(ZERO_OWNERS.load(Ordering::SeqCst), 9);
        hashmap::hew_hashmap_clear_layout(map);
        hashmap::hew_hashmap_free_layout(map);
        hashmap::hew_hashmap_free_layout(copy);
        vec::hew_vec_free_owned(values);
        drop_zero((&raw mut output).cast());
    }
    assert_eq!(ZERO_OWNERS.load(Ordering::SeqCst), 0);
}
