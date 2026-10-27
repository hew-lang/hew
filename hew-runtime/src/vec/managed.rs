//! Checked vector mutation and copied-result publication. The existing vector
//! implementation remains the authority for allocation and element semantics.

use super::{
    buffer_owner, buffer_start, capacity_for, hew_vec_append, hew_vec_get_clone,
    hew_vec_make_unique, hew_vec_set_owned_move_release, hew_vec_set_owned_release,
    owned_descriptor, HewValueLayout, HewVec,
};
use crate::coro_state::HewCoroState;
use crate::owned_value_budget::{
    materialize::{invocation_owner, HewManagedInput, MaterializationPermit},
    BackingChange, ManagedStorage, ValueOwner,
};
use crate::release_walker::HewReleaseCursor;
use core::{ffi::c_void, ptr};
use hew_cabi::managed::HEAP_EXCEEDED;
use std::sync::atomic::Ordering;

unsafe fn prepare_backing(
    owner: &ValueOwner,
    vector: *mut HewVec,
    needed: usize,
) -> Result<Option<BackingChange>, i32> {
    // SAFETY: the caller lends the live vector through the checked mutation.
    unsafe {
        let layout = owned_descriptor(vector);
        let width = crate::owned_value_budget::visit::slot_bytes(layout)?;
        let capacity = capacity_for((*vector).cap, needed).ok_or(HEAP_EXCEEDED)?;
        let bytes = width.checked_mul(capacity as u64).ok_or(HEAP_EXCEEDED)?;
        let unique =
            (*vector).data.is_null() || (*buffer_owner(vector)).load(Ordering::Acquire) == 1;
        if capacity == (*vector).cap && unique {
            return Ok(None);
        }
        let old = if (*vector).data.is_null() {
            None
        } else {
            Some(ManagedStorage {
                identity: buffer_start(vector) as usize,
                bytes: width
                    .checked_mul((*vector).cap as u64)
                    .ok_or(HEAP_EXCEEDED)?,
            })
        };
        owner
            .prepare_backing_change(old, bytes, unique)
            .map(Some)
            .map_err(|_| HEAP_EXCEEDED)
    }
}

unsafe fn element_permit(
    owner: ValueOwner,
    layout: &HewValueLayout,
    data: *const c_void,
    moved: bool,
) -> Result<MaterializationPermit, i32> {
    // SAFETY: the caller keeps the typed input live until mutation succeeds.
    unsafe {
        MaterializationPermit::prepare(
            owner,
            0,
            &[HewManagedInput {
                layout,
                slot: data,
                copies: u64::from(!moved),
                consume: u8::from(moved),
            }],
        )
    }
}

/// Detach shared vector storage before a checked mutation.
/// # Safety
/// The vector and optional invocation remain live through this exclusive call.
#[no_mangle]
pub unsafe extern "C" fn hew_vec_make_unique_managed(
    vector: *mut HewVec,
    invocation: *const HewCoroState,
) -> i32 {
    // SAFETY: source ownership is unchanged until exact preflight succeeds.
    unsafe {
        let Some(owner) = invocation_owner(invocation) else {
            hew_vec_make_unique(vector);
            return 0;
        };
        let backing = match prepare_backing(&owner, vector, (*vector).len) {
            Ok(backing) => backing,
            Err(status) => return status,
        };
        hew_vec_make_unique(vector);
        if let Some(backing) = backing {
            backing.commit(buffer_start(vector) as usize);
        }
        0
    }
}

/// Replace an element only after backing and copied descendants fit.
/// # Safety
/// The checked index is in bounds; source and output cursor are live and typed.
#[no_mangle]
pub unsafe extern "C" fn hew_vec_set_managed(
    vector: *mut HewVec,
    index: i64,
    data: *const c_void,
    moved: u32,
    invocation: *const HewCoroState,
    cleanup: *mut *mut HewReleaseCursor,
) -> i32 {
    // SAFETY: the caller retains the original inputs until success.
    unsafe {
        cleanup.write(ptr::null_mut());
        let owner = invocation_owner(invocation);
        let layout = *owned_descriptor(vector);
        let backing = match &owner {
            Some(owner) => match prepare_backing(owner, vector, (*vector).len) {
                Ok(backing) => backing,
                Err(status) => return status,
            },
            None => None,
        };
        let element = match owner {
            Some(owner) => match element_permit(owner, &layout, data, moved != 0) {
                Ok(element) => Some(element),
                Err(status) => return status,
            },
            None => None,
        };
        let displaced = if moved != 0 {
            hew_vec_set_owned_move_release(vector, index, data)
        } else {
            hew_vec_set_owned_release(vector, index, data)
        };
        if let Some(backing) = backing {
            backing.commit(buffer_start(vector) as usize);
        }
        if let Some(element) = element {
            if element
                .commit(
                    &layout,
                    (*vector).data.add(index as usize * layout.size).cast(),
                )
                .is_err()
            {
                std::process::abort();
            }
        }
        cleanup.write(displaced);
        0
    }
}

/// Append borrowed elements without changing the receiver on quota refusal.
/// # Safety
/// Both vectors have identical typed element layouts and remain live.
#[no_mangle]
pub unsafe extern "C" fn hew_vec_append_managed(
    destination: *mut HewVec,
    source: *const HewVec,
    invocation: *const HewCoroState,
) -> i32 {
    // SAFETY: every source element remains borrowed until all plans succeed.
    unsafe {
        let Some(owner) = invocation_owner(invocation) else {
            hew_vec_append(destination, source);
            return 0;
        };
        let count = (*source).len;
        if count == 0 {
            return 0;
        }
        let Some(needed) = (*destination).len.checked_add(count) else {
            return HEAP_EXCEEDED;
        };
        let layout = *owned_descriptor(destination);
        let backing = match prepare_backing(&owner, destination, needed) {
            Ok(backing) => backing,
            Err(status) => return status,
        };
        let mut elements = Vec::with_capacity(count);
        for index in 0..count {
            let slot = (*source).data.add(index * layout.size).cast();
            match element_permit(owner.clone(), &layout, slot, false) {
                Ok(permit) => elements.push(permit),
                Err(status) => return status,
            }
        }
        let first = (*destination).len;
        hew_vec_append(destination, source);
        if let Some(backing) = backing {
            backing.commit(buffer_start(destination) as usize);
        }
        for (index, permit) in elements.into_iter().enumerate() {
            if permit
                .commit(
                    &layout,
                    (*destination)
                        .data
                        .add((first + index) * layout.size)
                        .cast(),
                )
                .is_err()
            {
                std::process::abort();
            }
        }
        0
    }
}

/// Copy one element after reserving its canonical cloned payload.
/// # Safety
/// The descriptor and output storage match the vector's element type.
#[no_mangle]
pub unsafe extern "C" fn hew_vec_get_clone_managed(
    vector: *const HewVec,
    index: i64,
    output: *mut c_void,
    invocation: *const HewCoroState,
    status: *mut i32,
) -> bool {
    // SAFETY: no output is initialized until source bounds and quota succeed.
    unsafe {
        status.write(0);
        if index < 0 || index as usize >= (*vector).len {
            return false;
        }
        let Some(owner) = invocation_owner(invocation) else {
            return hew_vec_get_clone(vector, index, output);
        };
        let layout = owned_descriptor(vector);
        let source = (*vector).data.add(index as usize * layout.size).cast();
        let permit = match owner.reserve_clone(layout, source) {
            Ok(permit) => permit,
            Err(failure) => {
                status.write(failure);
                return false;
            }
        };
        if !hew_vec_get_clone(vector, index, output) {
            std::process::abort();
        }
        if permit.commit_value(layout, output).is_err() {
            std::process::abort();
        }
        true
    }
}
