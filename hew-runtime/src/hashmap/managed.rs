//! Checked map/set construction and snapshots. The ordinary collection code
//! remains the allocation and copy authority; this boundary reserves its typed
//! payload before executing it and adopts the result exactly once.

use super::{
    hew_hashmap_clone_layout, hew_hashmap_entries_layout, hew_hashmap_keys_layout,
    hew_hashmap_new_with_layout, hew_hashmap_values_layout, HewLayoutHashMap, LAYOUT_INIT_CAP,
};
use crate::owned_value_budget::{
    current_owner,
    materialize::{HewManagedInput, MaterializationPermit},
};
use core::{ffi::c_void, ptr};
use hew_cabi::managed::{HEAP_EXCEEDED, UNDESCRIBED};
use hew_cabi::{map::HewMapKeyLayout, value::HewValueLayout, vec::HewVec};

pub(super) fn slot_bytes(layout: &HewValueLayout) -> Result<u64, i32> {
    // SAFETY: descriptors are retained by the source value or executing code.
    unsafe { layout.managed.as_ref() }
        .map(|managed| managed.slot_bytes)
        .ok_or(UNDESCRIBED)
}

pub(super) fn backing_bytes(map: &HewLayoutHashMap, cap: usize) -> Result<u64, i32> {
    slot_bytes(&map.key_layout.value)?
        .checked_add(slot_bytes(&map.val_layout)?)
        .and_then(|width| width.checked_mul(cap as u64))
        .ok_or(HEAP_EXCEEDED)
}

pub(crate) fn copied(layout: *const HewValueLayout, slot: *const c_void) -> HewManagedInput {
    HewManagedInput {
        layout,
        slot,
        copies: 1,
        consume: 0,
    }
}

/// Reserve and adopt one source-visible result around its ordinary allocator.
/// # Safety
/// The supplied descriptors and slots are live; build returns their matching
/// fresh result, and status is writable. Inputs remain borrowed throughout.
pub(crate) unsafe fn result<T>(
    layout: *const HewValueLayout,
    status: *mut i32,
    plan: impl FnOnce() -> Result<(u64, Vec<HewManagedInput>), i32>,
    build: impl FnOnce() -> *mut T,
) -> *mut T {
    // SAFETY: the generated caller retains all descriptors and outputs.
    unsafe {
        status.write(0);
        let permit = if let Some(owner) = current_owner() {
            if layout.is_null() {
                status.write(UNDESCRIBED);
                return ptr::null_mut();
            }
            let prepared = plan()
                .and_then(|(bytes, inputs)| MaterializationPermit::prepare(owner, bytes, &inputs));
            match prepared {
                Ok(permit) => Some(permit),
                Err(error) => {
                    status.write(error);
                    return ptr::null_mut();
                }
            }
        } else {
            None
        };
        let value = build();
        if let Some(permit) = permit {
            if permit.commit(&*layout, (&raw const value).cast()).is_err() {
                std::process::abort();
            }
        }
        value
    }
}

/// Construct a map only after its initial typed capacity fits.
/// # Safety
/// Arguments satisfy the ordinary constructor; result layout and status are
/// retained, matching compiler outputs.
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_new_with_layout_checked(
    key: *const HewMapKeyLayout,
    value: *const HewValueLayout,
    layout: *const HewValueLayout,
    status: *mut i32,
) -> *mut HewLayoutHashMap {
    // SAFETY: forwarded constructor and checked output contracts.
    unsafe {
        result(
            layout,
            status,
            || {
                let bytes = slot_bytes(&(*key).value)?
                    .checked_add(slot_bytes(&*value)?)
                    .and_then(|width| width.checked_mul(LAYOUT_INIT_CAP as u64))
                    .ok_or(HEAP_EXCEEDED)?;
                Ok((bytes, Vec::new()))
            },
            || hew_hashmap_new_with_layout(key, value),
        )
    }
}

/// Clone a source map through its canonical typed copy plan.
/// # Safety
/// The map and matching result descriptor remain live through this call.
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_clone_layout_checked(
    map: *const HewLayoutHashMap,
    layout: *const HewValueLayout,
    status: *mut i32,
) -> *mut HewLayoutHashMap {
    // SAFETY: the source handle slot borrows the map until clone completes.
    unsafe {
        result(
            layout,
            status,
            || Ok((0, vec![copied(layout, (&raw const map).cast())])),
            || hew_hashmap_clone_layout(map),
        )
    }
}

unsafe fn projection_plan(
    map: *const HewLayoutHashMap,
    element: &HewValueLayout,
    keys: bool,
    values: bool,
) -> Result<(u64, Vec<HewManagedInput>), i32> {
    // SAFETY: the snapshot exclusively borrows a validated live map.
    unsafe {
        let Some(map) = map.as_ref() else {
            return Ok((0, Vec::new()));
        };
        let capacity = crate::vec::capacity_for(0, map.len).ok_or(HEAP_EXCEEDED)?;
        let bytes = slot_bytes(element)?
            .checked_mul(capacity as u64)
            .ok_or(HEAP_EXCEEDED)?;
        let mut inputs = Vec::new();
        for index in 0..map.cap {
            if let Some((key, value)) = map.managed_slot(index) {
                if keys {
                    inputs.push(copied(&raw const map.key_layout.value, key));
                }
                if values {
                    inputs.push(copied(&raw const map.val_layout, value));
                }
            }
        }
        Ok((bytes, inputs))
    }
}

/// Snapshot map keys after reserving the vector and every semantic copy.
/// # Safety
/// Arguments satisfy the ordinary projection and checked result contracts.
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_keys_layout_checked(
    map: *const HewLayoutHashMap,
    layout: *const HewValueLayout,
    status: *mut i32,
) -> *mut HewVec {
    // SAFETY: the source map pins its element descriptor and all copied slots.
    unsafe {
        result(
            layout,
            status,
            || {
                map.as_ref().map_or_else(
                    || Ok((0, Vec::new())),
                    |map| projection_plan(map, &map.key_layout.value, true, false),
                )
            },
            || hew_hashmap_keys_layout(map),
        )
    }
}

/// Snapshot map values after reserving the vector and every semantic copy.
/// # Safety
/// Arguments satisfy the ordinary projection and checked result contracts.
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_values_layout_checked(
    map: *const HewLayoutHashMap,
    layout: *const HewValueLayout,
    status: *mut i32,
) -> *mut HewVec {
    // SAFETY: the source map pins its element descriptor and all copied slots.
    unsafe {
        result(
            layout,
            status,
            || {
                map.as_ref().map_or_else(
                    || Ok((0, Vec::new())),
                    |map| projection_plan(map, &map.val_layout, false, true),
                )
            },
            || hew_hashmap_values_layout(map),
        )
    }
}

/// Snapshot map entries using the compiler-authored pair element descriptor.
/// # Safety
/// Arguments satisfy the ordinary entries projection and result contracts.
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_entries_layout_checked(
    map: *const HewLayoutHashMap,
    pair: *const HewValueLayout,
    value_offset: u64,
    layout: *const HewValueLayout,
    status: *mut i32,
) -> *mut HewVec {
    // SAFETY: pair is the exact native and canonical element layout.
    unsafe {
        result(
            layout,
            status,
            || projection_plan(map, &*pair, true, true),
            || hew_hashmap_entries_layout(map, pair, value_offset),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hashmap::*;
    use crate::layout_intrinsics::{hew_layout_key_i64, hew_layout_val_i64};
    use crate::owned_value_budget::{with_value_owner, ValueOwner};
    use hew_cabi::managed::HewManagedValueDescriptor;

    const MAP: HewManagedValueDescriptor = HewManagedValueDescriptor {
        slot_bytes: 8,
        visit: Some(crate::owned_value_budget::visit::hew_managed_visit_map),
    };
    const VECTOR: HewManagedValueDescriptor = HewManagedValueDescriptor {
        slot_bytes: 8,
        visit: Some(crate::owned_value_budget::visit::hew_managed_visit_vector),
    };

    fn handle(managed: &'static HewManagedValueDescriptor) -> HewValueLayout {
        HewValueLayout {
            managed,
            ..hew_layout_val_i64
        }
    }

    unsafe fn ready_probe(map: *mut HewLayoutHashMap, key: &i64) -> *mut HewLayoutMapProbe {
        // SAFETY: this test drives the ordinary i64 callbacks synchronously;
        // the checked probe retains its map/key loan through completion.
        unsafe {
            let probe = hew_hashmap_probe_begin(map, (key as *const i64).cast(), 1);
            assert!(!probe.is_null());
            loop {
                match hew_hashmap_probe_step(probe) {
                    0 => break,
                    1 => {
                        let mut hash = 0;
                        let mut fault = ptr::null_mut();
                        assert_eq!(
                            hew_layout_key_i64.hash_fn.unwrap()(
                                hew_hashmap_probe_left(probe),
                                &raw mut hash,
                                &raw mut fault
                            ),
                            0
                        );
                        hew_hashmap_probe_submit_hash(probe, hash);
                    }
                    2 => {
                        let mut equal = false;
                        let mut fault = ptr::null_mut();
                        assert_eq!(
                            hew_layout_key_i64.eq_fn.unwrap()(
                                hew_hashmap_probe_left(probe),
                                hew_hashmap_probe_right(probe),
                                &raw mut equal,
                                &raw mut fault
                            ),
                            0
                        );
                        hew_hashmap_probe_submit_eq(probe, equal);
                    }
                    other => panic!("unexpected probe status {other}"),
                }
            }
            probe
        }
    }

    unsafe fn insert(map: *mut HewLayoutHashMap, key: i64) {
        // SAFETY: the caller retains this test map and plain input through commit.
        unsafe {
            let probe = ready_probe(map, &key);
            let mut status = -1;
            let mut release = ptr::null_mut();
            assert!(hew_hashmap_probe_insert_clone_checked(
                probe,
                (&raw const key).cast(),
                &raw mut release,
                &raw mut status
            ));
            assert_eq!(status, 0);
            crate::release_walker::hew_release_sync(release);
        }
    }

    #[test]
    fn snapshots_reserve_capacity_once_and_refund_at_real_release() {
        let owner = ValueOwner::new(Some(288));
        with_value_owner(&Some(owner.clone()), || {
            // SAFETY: exact i64 descriptors match every owned map/vector slot.
            unsafe {
                let mut status = -1;
                let map = hew_hashmap_new_with_layout_checked(
                    &raw const hew_layout_key_i64,
                    &raw const hew_layout_val_i64,
                    &handle(&MAP),
                    &raw mut status,
                );
                assert_eq!(status, 0);
                assert_eq!(owner.live_bytes(), 256);
                insert(map, 7);
                let keys = hew_hashmap_keys_layout_checked(map, &handle(&VECTOR), &raw mut status);
                assert_eq!(status, 0);
                assert_eq!(crate::vec::hew_vec_len(keys), 1);
                assert_eq!(owner.live_bytes(), 288);
                let refused =
                    hew_hashmap_values_layout_checked(map, &handle(&VECTOR), &raw mut status);
                assert!(refused.is_null());
                assert_eq!(status, HEAP_EXCEEDED);
                assert_eq!((*map).len, 1);
                assert_eq!(owner.live_bytes(), 288);
                crate::vec::hew_vec_free_owned(keys);
                assert_eq!(owner.live_bytes(), 256);
                let values =
                    hew_hashmap_values_layout_checked(map, &handle(&VECTOR), &raw mut status);
                assert_eq!(status, 0);
                crate::vec::hew_vec_free_owned(values);
                hew_hashmap_clear_layout(map);
                assert_eq!(owner.live_bytes(), 256);
                hew_hashmap_free_layout(map);
                assert_eq!(owner.live_bytes(), 0);
            }
        });
    }

    #[test]
    fn growth_refusal_and_probe_cancellation_preserve_the_original_table() {
        for limit in [511, 512] {
            let owner = ValueOwner::new(Some(limit));
            with_value_owner(&Some(owner.clone()), || {
                // SAFETY: this test owns the entire map and all plain i64 inputs.
                unsafe {
                    let mut status = -1;
                    let map = hew_hashmap_new_with_layout_checked(
                        &raw const hew_layout_key_i64,
                        &raw const hew_layout_val_i64,
                        &handle(&MAP),
                        &raw mut status,
                    );
                    for key in 0..11 {
                        insert(map, key);
                    }
                    let entries = (*map).entries;
                    let key = 11_i64;
                    let probe = ready_probe(map, &key);
                    if limit == 511 {
                        let mut release = ptr::null_mut();
                        assert!(!hew_hashmap_probe_insert_clone_checked(
                            probe,
                            (&raw const key).cast(),
                            &raw mut release,
                            &raw mut status
                        ));
                        assert_eq!(status, HEAP_EXCEEDED);
                        assert!(release.is_null());
                    }
                    hew_hashmap_probe_free(probe);
                    assert_eq!((*map).entries, entries);
                    assert_eq!((*map).cap, 16);
                    assert_eq!((*map).len, 11);
                    assert_eq!(owner.live_bytes(), 256);
                    if limit == 512 {
                        insert(map, key);
                        assert_eq!((*map).cap, 32);
                        assert_eq!((*map).len, 12);
                        assert_eq!(owner.live_bytes(), 512);
                    }
                    hew_hashmap_free_layout(map);
                    assert_eq!(owner.live_bytes(), 0);
                }
            });
        }
    }
}
