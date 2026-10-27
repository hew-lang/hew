//! Concrete storage visits for canonical value descriptors. Container
//! algorithms and allocation capacities remain with their existing owners.

use core::ffi::c_void;
use hew_cabi::managed::{
    visit_value, HewManagedStorageVisitor, HewManagedWalkKind, DESCEND, HEAP_EXCEEDED,
    SKIP_DESCENDANTS, UNDESCRIBED,
};
use hew_cabi::value::HewValueLayout;

/// Foreign payloads have no described provider to rebind. This permits their
/// existing unbounded transfers without claiming they have an empty quota graph.
/// # Safety
/// The descriptor lends a live visitor for the synchronous phase.
#[no_mangle]
pub unsafe extern "C" fn hew_managed_visit_opaque(
    _slot: *const c_void,
    visitor: *const HewManagedStorageVisitor,
) -> i32 {
    // SAFETY: the descriptor retains this visitor for the entire callback.
    if unsafe { (*visitor).kind.is_transfer() } {
        0
    } else {
        UNDESCRIBED
    }
}

/// Report a compiler-described private backing without exposing host layout
/// details of the visitor to generated code.
///
/// # Safety
/// The synchronous visitor and retained backing identity remain live.
#[no_mangle]
pub unsafe extern "C" fn hew_managed_visit_backing(
    visitor: *const HewManagedStorageVisitor,
    identity: *const c_void,
    bytes: u64,
    fresh_on_clone: u8,
) -> i32 {
    // SAFETY: the caller lends the immutable visitor throughout this call.
    unsafe {
        let identity = if fresh_on_clone != 0 && (*visitor).kind == HewManagedWalkKind::ClonePlan {
            core::ptr::null()
        } else {
            identity
        };
        storage(visitor, identity, bytes)
    }
}

/// Read canonical metadata without substituting a host ABI size.
pub(crate) unsafe fn slot_bytes(layout: &HewValueLayout) -> Result<u64, i32> {
    // SAFETY: the caller retains the layout and defining code root.
    unsafe {
        layout
            .managed
            .as_ref()
            .map(|managed| managed.slot_bytes)
            .ok_or(UNDESCRIBED)
    }
}

/// Emit a storage edge. On a clone plan a null identity denotes fresh payload.
pub(crate) unsafe fn storage(
    visitor: *const HewManagedStorageVisitor,
    identity: *const c_void,
    bytes: u64,
) -> i32 {
    // SAFETY: every typed visitor receives a live synchronous context.
    unsafe { ((*visitor).storage)((*visitor).context, identity, bytes) }
}

/// # Safety
/// The slot contains a live Hew string handle and visitor remains live.
#[no_mangle]
pub unsafe extern "C" fn hew_managed_visit_string(
    slot: *const c_void,
    visitor: *const HewManagedStorageVisitor,
) -> i32 {
    // SAFETY: the descriptor supplies the concrete string slot.
    unsafe {
        let value = slot.cast::<*const hew_cabi::string::HewString>().read();
        if value.is_null() {
            return 0;
        }
        match storage(
            visitor,
            value.cast(),
            hew_cabi::string::string_payload_capacity(value) as u64,
        ) {
            DESCEND | SKIP_DESCENDANTS => 0,
            failure => failure,
        }
    }
}

/// # Safety
/// The slot contains a live `BytesTriple` and visitor remains live.
#[no_mangle]
pub unsafe extern "C" fn hew_managed_visit_bytes(
    slot: *const c_void,
    visitor: *const HewManagedStorageVisitor,
) -> i32 {
    // SAFETY: slices retain the original backing and its complete capacity.
    unsafe {
        let value = &*slot.cast::<crate::bytes::BytesTriple>();
        if value.ptr.is_null() {
            return 0;
        }
        match storage(
            visitor,
            value.ptr.cast(),
            u64::from(crate::bytes::capacity(value.ptr)),
        ) {
            DESCEND | SKIP_DESCENDANTS => 0,
            failure => failure,
        }
    }
}

/// # Safety
/// The slot contains a live typed `HewVec` pointer; no mutation races this walk.
#[no_mangle]
pub unsafe extern "C" fn hew_managed_visit_vector(
    slot: *const c_void,
    visitor: *const HewManagedStorageVisitor,
) -> i32 {
    // SAFETY: the descriptor supplies the live container handle.
    let pointer = unsafe { slot.cast::<*const hew_cabi::vec::HewVec>().read() };
    // SAFETY: the slot owner retains this immutable header throughout the visit.
    let end = unsafe { pointer.as_ref() }.map_or(0, |value| value.len);
    // SAFETY: the ordinary value owns the full initialized range and backing.
    unsafe { visit_vector_range(pointer, 0, end, true, visitor) }
}

/// Visit the cursor's remaining elements and, separately, its retained backing.
/// # Safety
/// The release owner pins this range and no destroyed prefix is included.
pub(crate) unsafe fn visit_vector_range(
    pointer: *const hew_cabi::vec::HewVec,
    start: usize,
    end: usize,
    backing: bool,
    visitor: *const HewManagedStorageVisitor,
) -> i32 {
    // SAFETY: the release/value owner retains the descriptor and selected range.
    unsafe {
        let Some(value) = pointer.as_ref() else {
            return 0;
        };
        if value.data.is_null() || value.cap == 0 {
            return 0;
        }
        let Some(layout) = value.layout.as_ref() else {
            return UNDESCRIBED;
        };
        let width = match slot_bytes(layout) {
            Ok(width) => width,
            Err(status) => return status,
        };
        let plain = layout.ownership_kind == hew_cabi::value::HewTypeOwnershipKind::Plain
            && layout.clone_fn.is_none()
            && layout.drop_fn.is_none()
            && layout.release_start.is_none();
        let fresh = (*visitor).kind == HewManagedWalkKind::ClonePlan && !plain;
        let capacity = if fresh { value.len } else { value.cap };
        let Some(bytes) = width.checked_mul(capacity as u64) else {
            return HEAP_EXCEEDED;
        };
        let identity = if fresh {
            core::ptr::null()
        } else {
            crate::vec::buffer_start(pointer).cast_const().cast()
        };
        if backing {
            match storage(visitor, identity, bytes) {
                SKIP_DESCENDANTS => return 0,
                DESCEND => {}
                failure => return failure,
            }
        }
        for index in start..end {
            let status = visit_value(
                layout,
                value.data.add(index * value.elem_size).cast(),
                visitor,
            );
            if status != 0 {
                return status;
            }
        }
        0
    }
}

/// # Safety
/// The slot contains a live layout-backed map pointer; no mutation races it.
#[no_mangle]
pub unsafe extern "C" fn hew_managed_visit_map(
    slot: *const c_void,
    visitor: *const HewManagedStorageVisitor,
) -> i32 {
    // SAFETY: the descriptor supplies the live container handle.
    let pointer = unsafe {
        slot.cast::<*const crate::hashmap::HewLayoutHashMap>()
            .read()
    };
    // SAFETY: the ordinary map owns all occupied slots and its backing.
    unsafe { visit_map_range(pointer, 0, true, visitor) }
}

/// Visit only map entries not yet handed to the semantic release cursor.
/// # Safety
/// The owner pins entries at and after `start`, and optionally their backing.
pub(crate) unsafe fn visit_map_range(
    pointer: *const crate::hashmap::HewLayoutHashMap,
    start: usize,
    backing: bool,
    visitor: *const HewManagedStorageVisitor,
) -> i32 {
    // SAFETY: the existing map descriptor and occupancy bytes remain authoritative.
    unsafe {
        let Some(value) = pointer.as_ref() else {
            return 0;
        };
        let key = &value.key_layout.value;
        let key_width = match slot_bytes(key) {
            Ok(width) => width,
            Err(status) => return status,
        };
        let value_width = match slot_bytes(&value.val_layout) {
            Ok(width) => width,
            Err(status) => return status,
        };
        let Some(bytes) = key_width
            .checked_add(value_width)
            .and_then(|width| width.checked_mul(value.cap as u64))
        else {
            return HEAP_EXCEEDED;
        };
        let fresh = (*visitor).kind == HewManagedWalkKind::ClonePlan;
        let identity = if fresh {
            core::ptr::null()
        } else {
            value.entries.cast_const().cast()
        };
        if backing {
            match storage(visitor, identity, bytes) {
                SKIP_DESCENDANTS => return 0,
                DESCEND => {}
                failure => return failure,
            }
        }
        for index in start..value.cap {
            if let Some((key_slot, value_slot)) = value.managed_slot(index) {
                let status = visit_value(key, key_slot, visitor);
                if status != 0 {
                    return status;
                }
                let status = visit_value(&value.val_layout, value_slot, visitor);
                if status != 0 {
                    return status;
                }
            }
        }
        0
    }
}

/// # Safety
/// The slot contains a live layout-backed set pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_managed_visit_set(
    slot: *const c_void,
    visitor: *const HewManagedStorageVisitor,
) -> i32 {
    // SAFETY: a set owns its underlying map; the temporary slot only lends it.
    unsafe {
        let set = slot
            .cast::<*const crate::hashset::HewLayoutHashSet>()
            .read();
        if set.is_null() {
            return 0;
        }
        let map = crate::hashset::managed_map(set);
        hew_managed_visit_map((&raw const map).cast(), visitor)
    }
}

/// # Safety
/// The slot contains a live strong Rc payload pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_managed_visit_shared(
    slot: *const c_void,
    visitor: *const HewManagedStorageVisitor,
) -> i32 {
    // SAFETY: the strong reference retains this exact cell throughout the typed walk.
    unsafe { crate::rc::visit_managed(slot.cast::<*mut u8>().read(), visitor) }
}

/// # Safety
/// The slot contains a live owned callable carrier.
#[no_mangle]
pub unsafe extern "C" fn hew_managed_visit_callable(
    slot: *const c_void,
    visitor: *const HewManagedStorageVisitor,
) -> i32 {
    // SAFETY: the carrier retains its environment and exact masked descriptor.
    unsafe {
        let value = &*slot.cast::<hew_cabi::callable::HewCallableValue>();
        if value.environment.is_null() {
            return 0;
        }
        let Some(descriptor) = value.descriptor.as_ref() else {
            return UNDESCRIBED;
        };
        let Some(layout) = descriptor.environment.as_ref() else {
            return UNDESCRIBED;
        };
        let width = match slot_bytes(layout) {
            Ok(width) => width,
            Err(status) => return status,
        };
        let identity = if (*visitor).kind == HewManagedWalkKind::ClonePlan {
            core::ptr::null()
        } else {
            value.environment.cast_const()
        };
        match storage(visitor, identity, width) {
            SKIP_DESCENDANTS => return 0,
            DESCEND => {}
            failure => return failure,
        }
        visit_value(layout, value.environment.cast_const(), visitor)
    }
}

/// # Safety
/// The slot contains a live owned trait object with its concrete value layout.
#[no_mangle]
pub unsafe extern "C" fn hew_managed_visit_dynamic(
    slot: *const c_void,
    visitor: *const HewManagedStorageVisitor,
) -> i32 {
    // SAFETY: the erased carrier retains the concrete value and vtable roots.
    unsafe {
        let value = &*slot.cast::<crate::trait_object::HewTraitObject>();
        if value.data.is_null() {
            return 0;
        }
        let Some(vtable) = value.vtable.as_ref() else {
            return UNDESCRIBED;
        };
        let Some(layout) = vtable.value_layout.as_ref() else {
            return UNDESCRIBED;
        };
        let width = match slot_bytes(layout) {
            Ok(width) => width,
            Err(status) => return status,
        };
        let identity = if (*visitor).kind == HewManagedWalkKind::ClonePlan {
            core::ptr::null()
        } else {
            value.data.cast_const().cast()
        };
        match storage(visitor, identity, width) {
            SKIP_DESCENDANTS => return 0,
            DESCEND => {}
            failure => return failure,
        }
        visit_value(layout, value.data.cast(), visitor)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    #[derive(Default)]
    struct Walk {
        retained: HashMap<usize, u64>,
        fresh: u64,
    }

    unsafe extern "C" fn collect(context: *mut c_void, identity: *const c_void, bytes: u64) -> i32 {
        // SAFETY: these tests lend one exclusive Walk as the callback context.
        let walk = unsafe { &mut *context.cast::<Walk>() };
        if identity.is_null() {
            walk.fresh += bytes;
            return DESCEND;
        }
        if walk.retained.insert(identity as usize, bytes).is_some() {
            SKIP_DESCENDANTS
        } else {
            DESCEND
        }
    }

    fn visitor(walk: &mut Walk, kind: HewManagedWalkKind) -> HewManagedStorageVisitor {
        HewManagedStorageVisitor {
            context: (&raw mut *walk).cast(),
            storage: collect,
            kind,
        }
    }

    #[test]
    fn foreign_payload_transfer_does_not_claim_bounded_accounting() {
        let mut walk = Walk::default();
        for kind in [
            HewManagedWalkKind::PrepareTransfer,
            HewManagedWalkKind::CommitTransfer,
            HewManagedWalkKind::AbortTransfer,
            HewManagedWalkKind::ResumeTransfer,
            HewManagedWalkKind::Retain,
            HewManagedWalkKind::ClonePlan,
        ] {
            let sink = visitor(&mut walk, kind);
            // SAFETY: this leaf never reads a payload; the visitor stays live.
            let status = unsafe { hew_managed_visit_opaque(core::ptr::null(), &raw const sink) };
            assert_eq!(status, if kind.is_transfer() { 0 } else { UNDESCRIBED });
        }
        assert!(walk.retained.is_empty());
    }

    #[test]
    fn plain_vector_visits_shared_capacity_and_detached_growth() {
        use crate::vec::{
            hew_vec_clone, hew_vec_free_owned, hew_vec_new_with_elem_layout, hew_vec_push_owned,
            hew_vec_remove_at_owned,
        };
        let mut walk = Walk::default();
        let sink = visitor(&mut walk, HewManagedWalkKind::Retain);
        // SAFETY: all slots use the canonical i64 element witness.
        unsafe {
            let value = hew_vec_new_with_elem_layout(
                &raw const crate::layout_intrinsics::hew_layout_val_i64,
            );
            for i in 0..4_i64 {
                hew_vec_push_owned(value, (&raw const i).cast());
            }
            let mut removed = -1_i64;
            assert_eq!(
                hew_vec_remove_at_owned(value, 0, (&raw mut removed).cast()),
                1
            );
            assert_eq!(removed, 0);
            let alias = hew_vec_clone(value);
            assert_eq!(
                hew_managed_visit_vector((&raw const value).cast(), &raw const sink),
                0
            );
            assert_eq!(
                hew_managed_visit_vector((&raw const alias).cast(), &raw const sink),
                0
            );
            assert_eq!(walk.retained.values().sum::<u64>(), 32);
            let item = 4_i64;
            hew_vec_push_owned(alias, (&raw const item).cast());
            let item = 5_i64;
            hew_vec_push_owned(alias, (&raw const item).cast());
            assert_eq!(
                hew_managed_visit_vector((&raw const alias).cast(), &raw const sink),
                0
            );
            assert_eq!(walk.retained.values().sum::<u64>(), 96);
            hew_vec_free_owned(alias);
            hew_vec_free_owned(value);
        }
    }

    #[test]
    fn managed_vector_clone_plan_uses_live_length_and_retains_string_backing() {
        use crate::vec::{hew_vec_free_owned, hew_vec_new_with_elem_layout, hew_vec_push_owned};
        // SAFETY: descriptor, strings and vector remain live during every visit.
        unsafe {
            let value = hew_vec_new_with_elem_layout(
                &raw const crate::layout_intrinsics::hew_layout_val_string,
            );
            let text = hew_cabi::string::string_from_str("abc");
            hew_vec_push_owned(value, (&raw const text).cast());
            hew_cabi::string::string_release(text);
            let mut walk = Walk::default();
            let sink = visitor(&mut walk, HewManagedWalkKind::ClonePlan);
            assert_eq!(
                hew_managed_visit_vector((&raw const value).cast(), &raw const sink),
                0
            );
            assert_eq!(
                walk.fresh, 8,
                "clone allocates exact live length, not source capacity4"
            );
            assert_eq!(walk.retained.values().sum::<u64>(), 3);
            hew_vec_free_owned(value);
        }
    }

    #[test]
    fn byte_slice_retains_full_backing_capacity() {
        // SAFETY: both triples keep the same backing live through the visit.
        unsafe {
            let input = b"0123456789abcdef0123456789abcdef";
            let original = crate::bytes::hew_bytes_from_static(
                input.as_ptr(),
                u32::try_from(input.len()).unwrap(),
            );
            let alias =
                crate::bytes::hew_bytes_slice(original.ptr, original.offset, original.len, 0, 1);
            let mut walk = Walk::default();
            let sink = visitor(&mut walk, HewManagedWalkKind::Retain);
            assert_eq!(
                hew_managed_visit_bytes((&raw const alias).cast(), &raw const sink),
                0
            );
            assert_eq!(walk.retained.values().sum::<u64>(), 32);
            crate::bytes::hew_bytes_drop(alias.ptr);
            crate::bytes::hew_bytes_drop(original.ptr);
        }
    }
}
