//! Persistent typed value slots. Preparation never calls this execution ABI.

use std::ffi::c_void;

use hew_cabi::{host_error::HewError, live::HewLiveSession, value::HewValueLayout};

use crate::host_value::{
    hew_host_value_forget, hew_host_value_new, hew_host_value_release, hew_host_value_storage,
    HewHostValue,
};

struct Slot {
    owner: *mut HewHostValue,
    leaves: Vec<Leaf>,
    initialized: Box<[u8]>,
    revision: u64,
}

struct Leaf {
    storage: *mut c_void,
    layout: HewValueLayout,
    path: Vec<u32>,
}

/// Address or create one typed slot before executing a submission body.
/// # Safety
/// `session` is exclusively borrowed with a valid slot array, `index` is in
/// bounds, and `layout` is the exact checked type of this declaration. Existing
/// slots keep their original destructor metadata and allocation.
#[no_mangle]
pub unsafe extern "C" fn hew_live_slot_storage(
    session: *mut HewLiveSession,
    index: usize,
    layout: *const HewValueLayout,
) -> *mut c_void {
    // SAFETY: the caller supplies the checked session context and slot index.
    let entry = unsafe { &mut *(*session).slots.add(index) };
    if entry.is_null() {
        // SAFETY: the compiler supplies the exact retained type's descriptor.
        let owner = unsafe { hew_host_value_new(layout) };
        *entry = Box::into_raw(Box::new(Slot {
            owner,
            leaves: Vec::new(),
            initialized: Box::new([]),
            revision: 0,
        }))
        .cast();
    }
    // SAFETY: an existing slot or the allocation above owns this typed storage.
    unsafe { hew_host_value_storage((*entry.cast::<Slot>()).owner) }
}

/// Install a place's canonical leaf partition and borrow its persistent bits.
/// # Safety
/// The context is exclusively borrowed. The complete leaf table addresses the
/// slot's own storage with exact same-type descriptors and declaration paths.
/// The slot's defining code owner retains every descriptor. No value has been
/// initialized before the first registration; later registrations preserve type.
///
/// # Panics
/// Panics if registration changes the stable slot's canonical type partition.
#[no_mangle]
pub unsafe extern "C" fn hew_live_slot_register(
    session: *mut HewLiveSession,
    index: usize,
    leaves: *const hew_cabi::live::HewLiveLeaf,
    count: usize,
) -> *mut u8 {
    // SAFETY: the compiler addresses one existing slot of this context.
    let slot = unsafe { &mut *(*(*session).slots.add(index)).cast::<Slot>() };
    assert_ne!(count, 0, "a typed place has at least one canonical leaf");
    // SAFETY: the generated entry lends the complete leaf table for this call.
    let leaves = unsafe { std::slice::from_raw_parts(leaves, count) };
    if slot.leaves.is_empty() {
        slot.leaves = leaves
            .iter()
            .map(|leaf| {
                // SAFETY: the descriptor and nonempty path are retained by the defining code owner.
                let layout = unsafe { *leaf.layout };
                let path = if leaf.path_len == 0 {
                    Vec::new()
                } else {
                    // SAFETY: the compiler lends this exact declaration-order path.
                    unsafe { std::slice::from_raw_parts(leaf.path, leaf.path_len) }.to_vec()
                };
                Leaf {
                    storage: leaf.storage,
                    layout,
                    path,
                }
            })
            .collect();
        slot.initialized = vec![0; count].into_boxed_slice();
    } else {
        assert_eq!(
            slot.leaves.len(),
            count,
            "persistent type partition changed"
        );
        for (old, new) in slot.leaves.iter().zip(leaves) {
            // SAFETY: registration supplies the checked descriptor and path.
            let layout = unsafe { &*new.layout };
            let path = if new.path_len == 0 {
                &[]
            } else {
                // SAFETY: the descriptor lends this exact path during registration.
                unsafe { std::slice::from_raw_parts(new.path, new.path_len) }
            };
            assert!(
                old.storage == new.storage
                    && old.layout.size == layout.size
                    && old.layout.align == layout.align
                    && old.path == path,
                "persistent type identity changed"
            );
        }
    }
    slot.initialized.as_mut_ptr()
}

/// Publish a completed write or move after updating the canonical leaf bits.
/// # Safety
/// The context and its registered slot are exclusively borrowed.
#[no_mangle]
pub unsafe extern "C" fn hew_live_slot_changed(session: *mut HewLiveSession, index: usize) {
    // SAFETY: the compiler addresses one existing slot of this context.
    let slot = unsafe { &mut *(*(*session).slots.add(index)).cast::<Slot>() };
    slot.revision = slot.revision.wrapping_add(1);
}

/// Read execution's ownership outcome without touching the retained value.
/// # Safety
/// `session` and its in-bounds slot are borrowed while no submission runs.
#[no_mangle]
pub unsafe extern "C" fn hew_live_slot_initialized(
    session: *const HewLiveSession,
    index: usize,
) -> bool {
    // SAFETY: the host supplies a valid context and in-bounds index.
    let slot = unsafe { *(*session).slots.add(index) }.cast::<Slot>();
    // SAFETY: a non-null slot was allocated by this module and remains live.
    unsafe { slot.as_ref() }.is_some_and(|slot| {
        !slot.initialized.is_empty() && slot.initialized.iter().all(|bit| *bit != 0)
    })
}

/// Return the stable canonical leaf count without touching the retained values.
/// # Safety
/// The session and in-bounds slot are borrowed while execution is stopped.
#[no_mangle]
pub unsafe extern "C" fn hew_live_slot_leaf_count(
    session: *const HewLiveSession,
    index: usize,
) -> usize {
    // SAFETY: the caller supplies a valid context and slot index.
    let slot = unsafe { *(*session).slots.add(index) }.cast::<Slot>();
    // SAFETY: a non-null slot remains owned by the stopped session.
    unsafe { slot.as_ref() }.map_or(0, |slot| slot.leaves.len())
}

/// Borrow one leaf's stable field path and copy its current occupancy.
/// # Safety
/// Execution is stopped, the leaf index is in bounds, and both outputs are
/// writable. The returned path lives until the session slot is released.
///
/// # Panics
/// Panics if the caller supplies an out-of-bounds leaf index.
#[no_mangle]
pub unsafe extern "C" fn hew_live_slot_leaf(
    session: *const HewLiveSession,
    index: usize,
    leaf: usize,
    length: *mut usize,
    initialized: *mut bool,
) -> *const u32 {
    // SAFETY: the stopped session retains the registered leaf and its path.
    let slot = unsafe { &*(*(*session).slots.add(index)).cast::<Slot>() };
    let entry = &slot.leaves[leaf];
    // SAFETY: the caller provides the two disjoint writable snapshot outputs.
    unsafe {
        *length = entry.path.len();
        *initialized = slot.initialized[leaf] != 0;
    }
    entry.path.as_ptr()
}

/// Read the slot's write/move revision to retain the code that supplied its value.
/// # Safety
/// `session` and its in-bounds slot are borrowed while no submission runs.
#[no_mangle]
pub unsafe extern "C" fn hew_live_slot_revision(
    session: *const HewLiveSession,
    index: usize,
) -> u64 {
    // SAFETY: the host supplies a valid context and in-bounds index.
    let slot = unsafe { *(*session).slots.add(index) }.cast::<Slot>();
    if slot.is_null() {
        0
    } else {
        // SAFETY: a non-null slot was allocated by this module and remains live.
        unsafe { (*slot).revision }
    }
}

/// Close one slot through ordinary Hew cleanup, including suspension and faults.
/// # Safety
/// The context is exclusively borrowed and no invocation or foreign view can
/// use this slot. `error` is a writable empty error-owner slot. Call off workers.
#[no_mangle]
pub unsafe extern "C" fn hew_live_slot_release(
    session: *mut HewLiveSession,
    index: usize,
    error: *mut *mut HewError,
) -> i32 {
    // SAFETY: the host transfers one slot with no outstanding uses.
    let entry = unsafe { &mut *(*session).slots.add(index) };
    let slot = std::mem::replace(entry, std::ptr::null_mut()).cast::<Slot>();
    // SAFETY: the caller provides an empty writable error result.
    unsafe { *error = std::ptr::null_mut() };
    if slot.is_null() {
        return 0;
    }
    // SAFETY: the slot was uniquely allocated by this module and detached above.
    let slot = unsafe { Box::from_raw(slot) };
    if !slot.initialized.is_empty() && slot.initialized.iter().all(|bit| *bit != 0) {
        // SAFETY: the unique initialized owner transfers to the release driver.
        unsafe { hew_host_value_release(slot.owner, error) }
    } else if slot.initialized.iter().all(|bit| *bit == 0) {
        // SAFETY: an empty slot has no semantic owner to destroy.
        unsafe { hew_host_value_forget(slot.owner) };
        0
    } else {
        let cursor = crate::release_walker::HewReleaseCursor::values(
            slot.leaves
                .iter()
                .zip(&slot.initialized)
                .filter(|(_, bit)| **bit != 0)
                .map(|(leaf, _)| (leaf.storage, leaf.layout)),
        );
        // SAFETY: only the actual remaining leaves enter cleanup; the original
        // typed allocation and defining code remain pinned until it finishes.
        unsafe { crate::host_value::release_parts(slot.owner, cursor, error) }
    }
}
