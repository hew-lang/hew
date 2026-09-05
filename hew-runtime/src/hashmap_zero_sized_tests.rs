//! Logical empty keys retain aligned addresses and one callback per owner.

use super::*;
use crate::{fault, hashset, vec};
use core::cell::Cell;

#[repr(align(64))]
struct AlignedUnit;

thread_local! {
    static ALIGN: Cell<usize> = const { Cell::new(1) };
    static CLONES: Cell<usize> = const { Cell::new(0) };
    static DROPS: Cell<usize> = const { Cell::new(0) };
    static FAIL: Cell<i32> = const { Cell::new(0) };
    static LAST_FAULT: Cell<*mut fault::HewFault> = const { Cell::new(ptr::null_mut()) };
}

fn check_slot(slot: *const c_void) {
    assert!(!slot.is_null());
    assert_eq!(slot.addr() % ALIGN.get(), 0);
}

unsafe extern "C" fn clone_unit(source: *const c_void, destination: *mut c_void) -> i32 {
    check_slot(source);
    check_slot(destination);
    CLONES.set(CLONES.get() + 1);
    0
}

unsafe extern "C" fn drop_unit(slot: *mut c_void) {
    check_slot(slot);
    DROPS.set(DROPS.get() + 1);
    assert!(DROPS.get() <= CLONES.get());
}

unsafe fn fail(status: i32, out: *mut *mut c_void) -> i32 {
    let owner = fault::hew_fault_new(202);
    assert!(LAST_FAULT.replace(owner).is_null());
    // SAFETY: callback callers supply writable fault storage.
    unsafe { out.write(owner.cast()) };
    status
}

unsafe extern "C" fn hash_unit(
    key: *const c_void,
    out: *mut u64,
    fault_out: *mut *mut c_void,
) -> i32 {
    check_slot(key);
    // SAFETY: callers provide writable, disjoint result and fault slots.
    unsafe {
        if FAIL.get() == -7 {
            return fail(-7, fault_out);
        }
        // Exercise the last slot, whose empty key may be one-past storage.
        out.write(u64::MAX);
        fault_out.write(ptr::null_mut());
    }
    0
}

unsafe extern "C" fn equal_unit(
    left: *const c_void,
    right: *const c_void,
    out: *mut bool,
    fault_out: *mut *mut c_void,
) -> i32 {
    check_slot(left);
    check_slot(right);
    // SAFETY: callers provide writable, disjoint result and fault slots.
    unsafe {
        if FAIL.get() == 13 {
            return fail(13, fault_out);
        }
        out.write(true);
        fault_out.write(ptr::null_mut());
    }
    0
}

fn layouts(align: usize, managed: bool) -> (HewMapKeyLayout, HewValueLayout) {
    ALIGN.set(align);
    CLONES.set(0);
    DROPS.set(0);
    FAIL.set(0);
    assert!(LAST_FAULT.get().is_null());
    let unit = HewValueLayout {
        size: 0,
        align: 1,
        ownership_kind: HewTypeOwnershipKind::Plain,
        clone_fn: None,
        drop_fn: None,
    };
    let key = HewMapKeyLayout {
        value: HewValueLayout {
            align,
            ownership_kind: if managed {
                HewTypeOwnershipKind::LayoutManaged
            } else {
                HewTypeOwnershipKind::Plain
            },
            clone_fn: managed.then_some(clone_unit),
            drop_fn: managed.then_some(drop_unit),
            ..unit
        },
        hash_fn: Some(hash_unit),
        eq_fn: Some(equal_unit),
    };
    (key, unit)
}

unsafe fn success<T>(call: impl FnOnce(*mut T, *mut *mut c_void) -> i32) -> T {
    let mut result = core::mem::MaybeUninit::uninit();
    let mut fault = ptr::null_mut();
    assert_eq!(call(result.as_mut_ptr(), &raw mut fault), 0);
    assert!(fault.is_null());
    // SAFETY: each call initializes its scalar output on success.
    unsafe { result.assume_init() }
}

unsafe fn release_fault(output: *mut c_void) {
    assert_eq!(output, LAST_FAULT.replace(ptr::null_mut()).cast());
    assert!(!output.is_null());
    // SAFETY: the operation transferred exactly this fault owner to the caller.
    unsafe { fault::hew_fault_drop(output.cast()) };
}

fn live() -> usize {
    CLONES.get() - DROPS.get()
}

#[test]
fn empty_record_key_replaces_removes_and_reuses_tombstone() {
    let (key_layout, mut value_layout) = layouts(1, false);
    value_layout.size = size_of::<u64>();
    value_layout.align = align_of::<u64>();
    let key = ptr::from_ref(&());
    // SAFETY: all slots and outputs match their descriptors and stay live.
    unsafe {
        let map = hew_hashmap_new_with_layout(&raw const key_layout, &raw const value_layout);
        for value in [41_u64, 42] {
            assert_eq!(
                success(|out, fault| hew_hashmap_insert_clone_layout(
                    map,
                    key.cast(),
                    (&raw const value).cast(),
                    out,
                    fault
                )),
                value == 41
            );
        }
        assert_eq!(hew_hashmap_len_layout(map), 1);
        let value = success(|out, fault| hew_hashmap_get_layout(map, key.cast(), out, fault));
        assert_eq!(*value.cast::<u64>(), 42);
        let copied = hew_hashmap_clone_layout(map);
        assert!(success(|out, fault| hew_hashmap_remove_layout(
            map,
            key.cast(),
            out,
            fault
        )));
        assert!(!success(|out, fault| hew_hashmap_contains_key_layout(
            map,
            key.cast(),
            out,
            fault
        )));
        let value = 43_u64;
        assert!(success(|out, fault| hew_hashmap_insert_layout(
            map,
            key.cast(),
            (&raw const value).cast(),
            out,
            fault
        )));
        let original_copy =
            success(|out, fault| hew_hashmap_get_layout(copied, key.cast(), out, fault));
        assert_eq!(*original_copy.cast::<u64>(), 42);
        hew_hashmap_clear_layout(map);
        assert_eq!(hew_hashmap_len_layout(map), 0);
        hew_hashmap_free_layout(map);
        hew_hashmap_free_layout(copied);
    }
}

#[test]
fn empty_pairs_and_aligned_keys_preserve_each_logical_owner() {
    for (alignment, managed) in [(1, false), (64, true)] {
        let (key_layout, unit) = layouts(alignment, managed);
        let key = AlignedUnit;
        // SAFETY: zero-sized slots need aligned non-null addresses; the pair
        // owns just its key because the empty value is plain.
        unsafe {
            let map = hew_hashmap_new_with_layout(&raw const key_layout, &raw const unit);
            assert_eq!((*map).key_layout.value.size, 0);
            assert_eq!((*map).stride, alignment);
            assert!(success(|out, fault| hew_hashmap_insert_clone_layout(
                map,
                (&raw const key).cast(),
                ptr::from_ref(&()).cast(),
                out,
                fault
            )));
            assert!(!success(|out, fault| hew_hashmap_insert_clone_layout(
                map,
                (&raw const key).cast(),
                ptr::from_ref(&()).cast(),
                out,
                fault
            )));
            assert_eq!(live(), usize::from(managed));
            let copy = hew_hashmap_clone_layout(map);
            let keys = hew_hashmap_keys_layout(map);
            let entries = hew_hashmap_entries_layout(map, &raw const key_layout.value, 0);
            let keys_copy = vec::hew_vec_clone_owned(keys);
            let entries_copy = vec::hew_vec_clone_owned(entries);
            let values = hew_hashmap_values_layout(map);
            for projection in [keys, entries, keys_copy, entries_copy, values] {
                assert_eq!(vec::hew_vec_len(projection), 1);
            }
            assert_eq!(live(), 6 * usize::from(managed));
            assert!(success(|out, fault| hew_hashmap_remove_layout(
                map,
                (&raw const key).cast(),
                out,
                fault
            )));
            hew_hashmap_free_layout(map);
            hew_hashmap_clear_layout(copy);
            hew_hashmap_free_layout(copy);
            for projection in [keys, entries, keys_copy, values] {
                vec::hew_vec_free_owned(projection);
            }
            assert_eq!(live(), usize::from(managed));
            vec::hew_vec_free_owned(entries_copy);
            assert_eq!(live(), 0);
        }
    }
}

#[test]
fn empty_set_clones_projects_and_releases_aligned_keys() {
    let (key_layout, _) = layouts(64, true);
    let key = AlignedUnit;
    // SAFETY: the borrowed key is aligned and callbacks own no payload bytes.
    unsafe {
        let set = hashset::hew_hashset_new_with_layout(&raw const key_layout);
        assert!(success(|out, fault| {
            hashset::hew_hashset_insert_clone_layout(set, (&raw const key).cast(), out, fault)
        }));
        assert!(!success(|out, fault| {
            hashset::hew_hashset_insert_clone_layout(set, (&raw const key).cast(), out, fault)
        }));
        let copy = hashset::hew_hashset_clone_layout(set);
        let keys = hashset::hew_hashset_to_vec_layout(set);
        assert_eq!(vec::hew_vec_len(keys), 1);
        assert_eq!(live(), 3);
        assert!(success(|out, fault| hashset::hew_hashset_remove_layout(
            set,
            (&raw const key).cast(),
            out,
            fault
        )));
        hashset::hew_hashset_free_layout(set);
        hashset::hew_hashset_free_layout(copy);
        assert_eq!(live(), 1);
        vec::hew_vec_free_owned(keys);
        assert_eq!(live(), 0);
    }
}

#[test]
fn empty_key_faults_preserve_results_entries_and_staged_owners() {
    let (key_layout, unit) = layouts(64, true);
    let key = AlignedUnit;
    // SAFETY: all slots match descriptors; each failed call transfers its fault.
    unsafe {
        let map = hew_hashmap_new_with_layout(&raw const key_layout, &raw const unit);
        assert!(success(|out, fault| hew_hashmap_insert_clone_layout(
            map,
            (&raw const key).cast(),
            ptr::from_ref(&()).cast(),
            out,
            fault
        )));
        for failure in [-7, 13] {
            FAIL.set(failure);
            let before_clones = CLONES.get();
            let before_drops = DROPS.get();
            let mut inserted = true;
            let mut fault = ptr::null_mut();
            assert_eq!(
                hew_hashmap_insert_clone_layout(
                    map,
                    (&raw const key).cast(),
                    ptr::from_ref(&()).cast(),
                    &raw mut inserted,
                    &raw mut fault
                ),
                failure
            );
            assert!(inserted);
            assert_eq!(CLONES.get(), before_clones + 1);
            assert_eq!(DROPS.get(), before_drops + 1);
            release_fault(fault);
            let sentinel = (&raw const key).cast();
            let mut result = sentinel;
            assert_eq!(
                hew_hashmap_get_layout(
                    map,
                    (&raw const key).cast(),
                    &raw mut result,
                    &raw mut fault
                ),
                failure
            );
            assert_eq!(result, sentinel);
            release_fault(fault);
            let mut removed = true;
            assert_eq!(
                hew_hashmap_remove_layout(
                    map,
                    (&raw const key).cast(),
                    &raw mut removed,
                    &raw mut fault
                ),
                failure
            );
            assert!(removed);
            release_fault(fault);
            assert_eq!(live(), 1);
            assert_eq!(hew_hashmap_len_layout(map), 1);
            FAIL.set(0);
            assert!(success(|out, fault| hew_hashmap_contains_key_layout(
                map,
                (&raw const key).cast(),
                out,
                fault
            )));
        }
        hew_hashmap_free_layout(map);
        assert_eq!(live(), 0);
    }
}

#[test]
fn empty_key_rehash_moves_owners_and_rolls_back_hash_fault() {
    let (key_layout, unit) = layouts(64, true);
    let key = AlignedUnit;
    // Equal empty keys cannot fill a table. Exercise the private staged growth
    // path directly without inventing address-dependent Hash/Eq semantics.
    // SAFETY: the test commits a successful resize using the production geometry.
    unsafe {
        let map = hew_hashmap_new_with_layout(&raw const key_layout, &raw const unit);
        assert!(success(|out, fault| hew_hashmap_insert_clone_layout(
            map,
            (&raw const key).cast(),
            ptr::from_ref(&()).cast(),
            out,
            fault
        )));
        let old_entries = (*map).entries;
        let old_cap = (*map).cap;
        let mut fault = ptr::null_mut();
        FAIL.set(-7);
        assert_eq!(layout_resize(map, &raw mut fault), Err(-7));
        release_fault(fault);
        assert_eq!((*map).entries, old_entries);
        assert_eq!((*map).cap, old_cap);
        FAIL.set(0);
        fault = ptr::null_mut();
        let (entries, cap) = layout_resize(map, &raw mut fault).unwrap();
        assert!(fault.is_null());
        assert_eq!(cap, old_cap * 2);
        dealloc_layout_entries(old_entries, old_cap, (*map).stride, 64);
        (*map).entries = entries;
        (*map).cap = cap;
        assert_eq!(CLONES.get(), 1);
        assert_eq!(DROPS.get(), 0);
        assert!(success(|out, fault| hew_hashmap_contains_key_layout(
            map,
            (&raw const key).cast(),
            out,
            fault
        )));
        hew_hashmap_free_layout(map);
        assert_eq!(live(), 0);
    }
}

#[test]
fn empty_key_still_requires_nonnull_input() {
    let (key_layout, unit) = layouts(1, false);
    // SAFETY: exercise the Rust validation gate so the expected panic unwinds;
    // the map remains live and is freed after the gate rejects the null key.
    unsafe {
        let map = hew_hashmap_new_with_layout(&raw const key_layout, &raw const unit);
        let result = std::panic::catch_unwind(|| validate_op_inputs(map, ptr::null(), None));
        hew_hashmap_free_layout(map);
        assert!(result.is_err());
    }
}
