//! Persistent typed value slots. Preparation never calls this execution ABI.

use std::ffi::c_void;

use hew_cabi::{host_error::HewError, live::HewLiveSession, value::HewValueLayout};

use crate::host_value::{
    hew_host_value_forget, hew_host_value_new, hew_host_value_release, hew_host_value_storage,
    HewHostValue,
};

struct Slot {
    owner: *mut HewHostValue,
    initialized: bool,
    revision: u64,
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
            initialized: false,
            revision: 0,
        }))
        .cast();
    }
    // SAFETY: an existing slot or the allocation above owns this typed storage.
    unsafe { hew_host_value_storage((*entry.cast::<Slot>()).owner) }
}

/// Record the actual ownership change of a persistent binding.
/// # Safety
/// The context and initialized slot are exclusively borrowed. Setting `true`
/// follows complete value initialization; setting `false` follows a move out.
#[no_mangle]
pub unsafe extern "C" fn hew_live_slot_set_initialized(
    session: *mut HewLiveSession,
    index: usize,
    initialized: bool,
) {
    // SAFETY: the compiler addresses one existing slot of this context.
    let slot = unsafe { &mut *(*(*session).slots.add(index)).cast::<Slot>() };
    slot.initialized = initialized;
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
    !slot.is_null() && unsafe { (*slot).initialized }
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
    if slot.initialized {
        // SAFETY: the unique initialized owner transfers to the release driver.
        unsafe { hew_host_value_release(slot.owner, error) }
    } else {
        // SAFETY: an empty slot has no semantic owner to destroy.
        unsafe { hew_host_value_forget(slot.owner) };
        0
    }
}
