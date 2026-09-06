//! Logical callback faults preserve result slots and collection ownership.

#[path = "common/map_status.rs"]
mod map_status;

use core::cell::Cell;
use core::ffi::c_void;
use core::mem::MaybeUninit;
use core::ptr;

use hew_cabi::map::HewMapKeyLayout;
use hew_cabi::value::{HewTypeOwnershipKind, HewValueLayout};
use hew_runtime::fault::{hew_fault_drop, hew_fault_new, HewFault};
use hew_runtime::{hashmap as map, hashset as set};

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
struct Counts {
    created: usize,
    cloned: usize,
    dropped: usize,
}

impl Counts {
    fn live(self) -> usize {
        self.created + self.cloned - self.dropped
    }
}

#[derive(Clone, Copy, Debug)]
enum Failure {
    Hash(usize),
    Equal(usize),
}

impl Failure {
    fn status(self) -> i32 {
        match self {
            Self::Hash(_) => 47,
            Self::Equal(_) => -19,
        }
    }
}

thread_local! {
    static COUNTS: Cell<Counts> = const { Cell::new(Counts { created: 0, cloned: 0, dropped: 0 }) };
    static FAIL: Cell<Option<Failure>> = const { Cell::new(None) };
    static HASH_CALLS: Cell<usize> = const { Cell::new(0) };
    static EQ_CALLS: Cell<usize> = const { Cell::new(0) };
    static LAST_FAULT: Cell<*mut HewFault> = const { Cell::new(ptr::null_mut()) };
}

#[repr(C)]
struct Owned {
    number: *mut i64,
}

fn owned(number: i64) -> Owned {
    COUNTS.with(|cell| {
        let mut counts = cell.get();
        counts.created += 1;
        cell.set(counts);
    });
    Owned {
        number: Box::into_raw(Box::new(number)),
    }
}

unsafe extern "C" fn clone_owned(source: *const c_void, destination: *mut c_void) -> i32 {
    // SAFETY: source is a live Owned and destination is fresh aligned slot storage.
    unsafe {
        destination.cast::<Owned>().write(Owned {
            number: Box::into_raw(Box::new(*(*source.cast::<Owned>()).number)),
        });
    }
    COUNTS.with(|cell| {
        let mut counts = cell.get();
        counts.cloned += 1;
        cell.set(counts);
    });
    0
}

unsafe extern "C" fn drop_owned(value: *mut c_void) {
    // SAFETY: this callback receives one unique owner of a live Box allocation.
    unsafe { drop(Box::from_raw((*value.cast::<Owned>()).number)) };
    COUNTS.with(|cell| {
        let mut counts = cell.get();
        counts.dropped += 1;
        cell.set(counts);
    });
}

unsafe fn publish_fault(failure: Failure, fault_out: *mut *mut c_void) -> i32 {
    let fault = hew_fault_new(202);
    assert!(LAST_FAULT.replace(fault).is_null());
    // SAFETY: the callback receives a writable opaque fault output slot.
    unsafe { fault_out.write(fault.cast()) };
    failure.status()
}

unsafe extern "C" fn hash_owned(
    _key: *const c_void,
    out: *mut u64,
    fault_out: *mut *mut c_void,
) -> i32 {
    HASH_CALLS.set(HASH_CALLS.get() + 1);
    if let Some(failure @ Failure::Hash(at)) = FAIL.get() {
        if at == HASH_CALLS.get() {
            // SAFETY: the caller provided this fault output; scalar output stays untouched.
            return unsafe { publish_fault(failure, fault_out) };
        }
    }
    // Deliberately collide distinct values to exercise equality during probes.
    // SAFETY: callback outputs are disjoint, writable scalar and pointer slots.
    unsafe {
        out.write(0);
        fault_out.write(ptr::null_mut());
    }
    0
}

unsafe extern "C" fn equal_owned(
    lhs: *const c_void,
    rhs: *const c_void,
    out: *mut bool,
    fault_out: *mut *mut c_void,
) -> i32 {
    EQ_CALLS.set(EQ_CALLS.get() + 1);
    if let Some(failure @ Failure::Equal(at)) = FAIL.get() {
        if at == EQ_CALLS.get() {
            // SAFETY: the caller provided this fault output; scalar output stays untouched.
            return unsafe { publish_fault(failure, fault_out) };
        }
    }
    // SAFETY: both inputs are live Owned keys and callback outputs are writable.
    unsafe {
        out.write(*(*lhs.cast::<Owned>()).number == *(*rhs.cast::<Owned>()).number);
        fault_out.write(ptr::null_mut());
    }
    0
}

static VALUE: HewValueLayout = HewValueLayout {
    visit_close: None,
    size: size_of::<Owned>(),
    align: align_of::<Owned>(),
    ownership_kind: HewTypeOwnershipKind::LayoutManaged,
    clone_fn: Some(clone_owned),
    drop_fn: Some(drop_owned),
};
static KEY: HewMapKeyLayout = HewMapKeyLayout {
    value: VALUE,
    hash_fn: Some(hash_owned),
    eq_fn: Some(equal_owned),
};

fn configure(failure: Option<Failure>) {
    assert!(LAST_FAULT.get().is_null());
    FAIL.set(failure);
    HASH_CALLS.set(0);
    EQ_CALLS.set(0);
}

fn reset() {
    assert_eq!(COUNTS.get().live(), 0);
    COUNTS.set(Counts::default());
    configure(None);
}

fn take_fault(status: i32, fault: *mut c_void, failure: Failure) {
    assert_eq!(status, failure.status());
    assert!(!fault.is_null());
    assert_eq!(fault, LAST_FAULT.replace(ptr::null_mut()).cast());
    // SAFETY: the kernel transferred exactly this existing fault owner unchanged.
    unsafe { hew_fault_drop(fault.cast()) };
    configure(None);
}

unsafe fn fill_map(len: i64) -> *mut map::HewLayoutHashMap {
    // SAFETY: both descriptors describe the owned key/value slots used below.
    unsafe {
        let result = map::hew_hashmap_new_with_layout(&raw const KEY, &raw const VALUE);
        for number in 0..len {
            let key = owned(number);
            let value = owned(number + 1000);
            assert!(map_status::success(|out, fault| {
                map::hew_hashmap_insert_layout(
                    result,
                    (&raw const key).cast(),
                    (&raw const value).cast(),
                    out,
                    fault,
                )
            }));
        }
        result
    }
}

unsafe fn assert_map_values(map: *const map::HewLayoutHashMap, len: i64) {
    // SAFETY: the map and its descriptors are live; each successful extraction is released.
    unsafe {
        assert_eq!(map::hew_hashmap_len_layout(map), len);
        for number in 0..len {
            let mut key = owned(number);
            let mut value = MaybeUninit::<Owned>::uninit();
            assert!(map_status::success(|out, fault| {
                map::hew_hashmap_get_clone_layout(
                    map,
                    (&raw const key).cast(),
                    value.as_mut_ptr().cast(),
                    out,
                    fault,
                )
            }));
            let mut value = value.assume_init();
            assert_eq!(*value.number, number + 1000);
            drop_owned((&raw mut value).cast());
            drop_owned((&raw mut key).cast());
        }
    }
}

fn assert_balanced() {
    let counts = COUNTS.get();
    assert_eq!(counts.created + counts.cloned, counts.dropped);
    assert!(LAST_FAULT.get().is_null());
}

#[derive(Clone, Copy, Debug)]
enum Lookup {
    Borrow,
    Clone,
    Contains,
    Remove,
    Take,
}

#[test]
fn map_lookup_and_removal_faults_leave_every_result_and_owner_untouched() {
    for failure in [Failure::Hash(1), Failure::Equal(1)] {
        for operation in [
            Lookup::Borrow,
            Lookup::Clone,
            Lookup::Contains,
            Lookup::Remove,
            Lookup::Take,
        ] {
            reset();
            // SAFETY: this scenario owns the map, query and sentinel slots independently.
            unsafe {
                let map = fill_map(2);
                let mut key = owned(1);
                let mut sentinel = owned(-100);
                let original_number = sentinel.number;
                let mut borrowed = (&raw const sentinel).cast::<c_void>();
                let original_borrowed = borrowed;
                let mut present = true;
                let mut fault = ptr::null_mut();
                let before = COUNTS.get();
                configure(Some(failure));
                let status = match operation {
                    Lookup::Borrow => map::hew_hashmap_get_layout(
                        map,
                        (&raw const key).cast(),
                        &raw mut borrowed,
                        &raw mut fault,
                    ),
                    Lookup::Clone => map::hew_hashmap_get_clone_layout(
                        map,
                        (&raw const key).cast(),
                        (&raw mut sentinel).cast(),
                        &raw mut present,
                        &raw mut fault,
                    ),
                    Lookup::Contains => map::hew_hashmap_contains_key_layout(
                        map,
                        (&raw const key).cast(),
                        &raw mut present,
                        &raw mut fault,
                    ),
                    Lookup::Remove => map::hew_hashmap_remove_layout(
                        map,
                        (&raw const key).cast(),
                        &raw mut present,
                        &raw mut fault,
                    ),
                    Lookup::Take => map::hew_hashmap_remove_take_layout(
                        map,
                        (&raw const key).cast(),
                        (&raw mut sentinel).cast(),
                        &raw mut present,
                        &raw mut fault,
                    ),
                };
                take_fault(status, fault, failure);
                assert!(present, "{operation:?}");
                assert_eq!(sentinel.number, original_number);
                assert_eq!(*sentinel.number, -100);
                assert_eq!(borrowed, original_borrowed);
                assert_eq!(COUNTS.get(), before);
                assert_map_values(map, 2);
                drop_owned((&raw mut key).cast());
                drop_owned((&raw mut sentinel).cast());
                map::hew_hashmap_free_layout(map);
            }
            assert_balanced();
        }
    }
}

fn assert_scratch_released(before: Counts, clones: usize) {
    let after = COUNTS.get();
    assert_eq!(after.created, before.created);
    assert_eq!(after.cloned, before.cloned + clones);
    assert_eq!(after.dropped, before.dropped + clones);
    assert_eq!(after.live(), before.live());
}

unsafe fn map_insert_fault(len: i64, number: i64, copy_in: bool, failure: Failure) {
    // SAFETY: map and two independent input owners stay live until explicit cleanup.
    unsafe {
        let map = fill_map(len);
        let mut key = owned(number);
        let mut value = owned(9000);
        let geometry = ((*map).entries, (*map).cap, (*map).len);
        let before = COUNTS.get();
        let mut present = true;
        let mut fault = ptr::null_mut();
        configure(Some(failure));
        let status = if copy_in {
            map::hew_hashmap_insert_clone_layout(
                map,
                (&raw const key).cast(),
                (&raw const value).cast(),
                &raw mut present,
                &raw mut fault,
            )
        } else {
            map::hew_hashmap_insert_layout(
                map,
                (&raw const key).cast(),
                (&raw const value).cast(),
                &raw mut present,
                &raw mut fault,
            )
        };
        take_fault(status, fault, failure);
        assert!(present);
        assert_eq!(((*map).entries, (*map).cap, (*map).len), geometry);
        assert_scratch_released(before, if copy_in { 2 } else { 0 });
        assert_eq!(*key.number, number);
        assert_eq!(*value.number, 9000);
        assert_map_values(map, len);
        drop_owned((&raw mut key).cast());
        drop_owned((&raw mut value).cast());
        map::hew_hashmap_free_layout(map);
    }
}

#[test]
fn map_transfer_and_copy_insertion_faults_preserve_vacant_and_occupied_inputs() {
    for copy_in in [false, true] {
        for number in [0, 4] {
            for failure in [Failure::Hash(1), Failure::Equal(1)] {
                reset();
                // SAFETY: the helper provides valid descriptors and retains all owners on error.
                unsafe { map_insert_fault(2, number, copy_in, failure) };
                assert_balanced();
            }
        }
    }
}

#[test]
fn map_resize_faults_discard_only_staged_storage_before_or_after_rehash() {
    for copy_in in [false, true] {
        // Eleven entries fill the initial table to the next insertion's growth
        // boundary. Fail midway through rehash, on the incoming hash, and in
        // the subsequent equality probe after rehash has fully completed.
        for failure in [Failure::Hash(3), Failure::Hash(12), Failure::Equal(3)] {
            reset();
            // SAFETY: the helper checks the old geometry and every old value after failure.
            unsafe { map_insert_fault(11, 20, copy_in, failure) };
            assert_balanced();
        }
    }
}

#[test]
fn copy_in_failure_preserves_inputs_borrowed_from_the_same_map() {
    for len in [2, 11] {
        for failure in [Failure::Hash(1), Failure::Equal(1)] {
            reset();
            // SAFETY: iterator borrows are released before insertion; the map
            // remains live and the failed insertion must not invalidate its slots.
            unsafe {
                let map = fill_map(len);
                let iterator = map::hew_hashmap_iter_new_layout(map);
                let mut key = ptr::null();
                let mut value = ptr::null();
                assert!(map::hew_hashmap_iter_next_layout(
                    iterator,
                    &raw mut key,
                    &raw mut value
                ));
                map::hew_hashmap_iter_free_layout(iterator);
                let old_key = *(*key.cast::<Owned>()).number;
                let old_value = *(*value.cast::<Owned>()).number;
                let before = COUNTS.get();
                let mut present = true;
                let mut fault = ptr::null_mut();
                configure(Some(failure));
                let status = map::hew_hashmap_insert_clone_layout(
                    map,
                    key,
                    value,
                    &raw mut present,
                    &raw mut fault,
                );
                take_fault(status, fault, failure);
                assert!(present);
                assert_scratch_released(before, 2);
                assert_eq!(*(*key.cast::<Owned>()).number, old_key);
                assert_eq!(*(*value.cast::<Owned>()).number, old_value);
                assert_map_values(map, len);
                map::hew_hashmap_free_layout(map);
            }
            assert_balanced();
        }
    }
}

unsafe fn fill_set(len: i64) -> *mut set::HewLayoutHashSet {
    // SAFETY: the descriptor matches each transferred Owned element.
    unsafe {
        let result = set::hew_hashset_new_with_layout(&raw const KEY);
        for number in 0..len {
            let value = owned(number);
            assert!(map_status::success(|out, fault| {
                set::hew_hashset_insert_layout(result, (&raw const value).cast(), out, fault)
            }));
        }
        result
    }
}

unsafe fn assert_set_values(set: *const set::HewLayoutHashSet, len: i64) {
    // SAFETY: the set stays live; each independent borrowed query is released afterwards.
    unsafe {
        assert_eq!(set::hew_hashset_len_layout(set), len);
        for number in 0..len {
            let mut query = owned(number);
            assert!(map_status::success(|out, fault| {
                set::hew_hashset_contains_layout(set, (&raw const query).cast(), out, fault)
            }));
            drop_owned((&raw mut query).cast());
        }
    }
}

#[derive(Clone, Copy, Debug)]
enum SetAction {
    Insert,
    CopyInsert,
    Contains,
    Remove,
}

unsafe fn set_operation_fault(len: i64, number: i64, action: SetAction, failure: Failure) {
    // SAFETY: the set and the independent input remain live through failure and cleanup.
    unsafe {
        let set = fill_set(len);
        let mut value = owned(number);
        let before = COUNTS.get();
        let mut present = true;
        let mut fault = ptr::null_mut();
        configure(Some(failure));
        let status = match action {
            SetAction::Insert => set::hew_hashset_insert_layout(
                set,
                (&raw const value).cast(),
                &raw mut present,
                &raw mut fault,
            ),
            SetAction::CopyInsert => set::hew_hashset_insert_clone_layout(
                set,
                (&raw const value).cast(),
                &raw mut present,
                &raw mut fault,
            ),
            SetAction::Contains => set::hew_hashset_contains_layout(
                set,
                (&raw const value).cast(),
                &raw mut present,
                &raw mut fault,
            ),
            SetAction::Remove => set::hew_hashset_remove_layout(
                set,
                (&raw const value).cast(),
                &raw mut present,
                &raw mut fault,
            ),
        };
        take_fault(status, fault, failure);
        assert!(present);
        assert_scratch_released(before, usize::from(matches!(action, SetAction::CopyInsert)));
        assert_eq!(*value.number, number);
        assert_set_values(set, len);
        drop_owned((&raw mut value).cast());
        set::hew_hashset_free_layout(set);
    }
}

#[test]
fn set_faults_preserve_inputs_presence_and_contents() {
    for action in [
        SetAction::Insert,
        SetAction::CopyInsert,
        SetAction::Contains,
        SetAction::Remove,
    ] {
        for number in [1, 99] {
            for failure in [Failure::Hash(1), Failure::Equal(1)] {
                reset();
                // SAFETY: the helper owns all values and checks the unchanged set on failure.
                unsafe { set_operation_fault(2, number, action, failure) };
                assert_balanced();
            }
        }
    }
}

#[test]
fn set_resize_faults_preserve_every_element_and_incoming_owner() {
    for action in [SetAction::Insert, SetAction::CopyInsert] {
        for failure in [Failure::Hash(3), Failure::Hash(12), Failure::Equal(3)] {
            reset();
            // SAFETY: these insertions reach the growth boundary with independently owned input.
            unsafe { set_operation_fault(11, 99, action, failure) };
            assert_balanced();
        }
    }
}

#[test]
fn set_copy_in_fault_preserves_an_element_borrowed_from_the_same_set() {
    for len in [2, 11] {
        for failure in [Failure::Hash(1), Failure::Equal(1)] {
            reset();
            // SAFETY: iterator storage is released before insertion; the set
            // remains live and failed insertion must preserve its element slots.
            unsafe {
                let set = fill_set(len);
                let iterator = set::hew_hashset_iter_new_layout(set);
                let mut element = ptr::null();
                assert!(set::hew_hashset_iter_next_layout(
                    iterator,
                    &raw mut element
                ));
                set::hew_hashset_iter_free_layout(iterator);
                let old_value = *(*element.cast::<Owned>()).number;
                let before = COUNTS.get();
                let mut present = true;
                let mut fault = ptr::null_mut();
                configure(Some(failure));
                let status = set::hew_hashset_insert_clone_layout(
                    set,
                    element,
                    &raw mut present,
                    &raw mut fault,
                );
                take_fault(status, fault, failure);
                assert!(present);
                assert_scratch_released(before, 1);
                assert_eq!(*(*element.cast::<Owned>()).number, old_value);
                assert_set_values(set, len);
                set::hew_hashset_free_layout(set);
            }
            assert_balanced();
        }
    }
}
