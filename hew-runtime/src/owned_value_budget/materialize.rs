//! Checked native value publication. Inputs remain intact on a refused reserve;
//! successful constructors transfer their typed obligations after adoption.

use super::{current_owner, StorageReservation, ValueOwner};
use crate::coro_state::HewCoroState;
use core::ffi::c_void;
use hew_cabi::managed::UNDESCRIBED;
use hew_cabi::value::HewValueLayout;
use std::alloc::{alloc, dealloc, handle_alloc_error, Layout};

/// One owning-stage input: semantic copies reserve before execution; a moved
/// input's old obligation ends only after result adoption succeeds.
#[repr(C)]
#[derive(Debug)]
pub struct HewManagedInput {
    pub layout: *const HewValueLayout,
    pub slot: *const c_void,
    pub copies: u64,
    pub consume: u8,
}

#[derive(Debug)]
struct Snapshot {
    layout: *const HewValueLayout,
    slot: *mut c_void,
    allocation: Layout,
}
impl Snapshot {
    unsafe fn new(layout: *const HewValueLayout, source: *const c_void) -> Self {
        // SAFETY: the input contract provides the exact native storage geometry.
        let value = unsafe { &*layout };
        let allocation = Layout::from_size_align(value.size.max(1), value.align)
            .expect("verified managed input layout");
        // SAFETY: a raw metadata snapshot never runs clone/drop or owns payloads.
        let slot = unsafe { alloc(allocation) };
        if slot.is_null() {
            handle_alloc_error(allocation);
        }
        // SAFETY: source is live; this temporary borrows the same retained graph.
        unsafe { std::ptr::copy_nonoverlapping(source.cast::<u8>(), slot, value.size) };
        Self {
            layout,
            slot: slot.cast(),
            allocation,
        }
    }
}
impl Drop for Snapshot {
    fn drop(&mut self) {
        // SAFETY: the snapshot owns only this raw bookkeeping allocation.
        unsafe { dealloc(self.slot.cast(), self.allocation) };
    }
}

#[derive(Debug)]
pub struct MaterializationPermit {
    owner: ValueOwner,
    reservation: StorageReservation,
    moved: Vec<Snapshot>,
}

impl MaterializationPermit {
    /// Prepare an exact provider or compiler-owned materialization.
    ///
    /// # Safety
    /// All typed inputs remain live and unchanged until construction commits.
    pub unsafe fn prepare(
        owner: ValueOwner,
        fresh_bytes: u64,
        inputs: &[HewManagedInput],
    ) -> Result<Self, i32> {
        let mut copies = Vec::new();
        for input in inputs {
            // SAFETY: descriptors are retained by the executing module.
            let Some(layout) = (unsafe { input.layout.as_ref() }) else {
                return Err(UNDESCRIBED);
            };
            if input.copies != 0 {
                copies.push((layout, input.slot, input.copies));
            }
        }
        // SAFETY: source slots remain unchanged throughout preflight.
        let reservation = unsafe { owner.reserve_materialization(fresh_bytes, &copies) }?;
        let moved = inputs
            .iter()
            .filter(|input| input.consume != 0)
            .map(|input| {
                // SAFETY: this only snapshots metadata held by the live source.
                unsafe { Snapshot::new(input.layout, input.slot) }
            })
            .collect();
        Ok(Self {
            owner,
            reservation,
            moved,
        })
    }

    /// Adopt an unpublished result, then end transferred input obligations.
    ///
    /// # Safety
    /// The result matches layout and retains all moved input payloads.
    pub unsafe fn commit(self, layout: &HewValueLayout, result: *const c_void) -> Result<(), i32> {
        let Self {
            owner,
            reservation,
            moved,
        } = self;
        // SAFETY: result remains unpublished through its typed adoption.
        unsafe { reservation.commit_value(layout, result) }?;
        for input in moved {
            // SAFETY: result adoption retains all payloads transferred from input.
            if unsafe { owner.release_value(&*input.layout, input.slot) }.is_err() {
                std::process::abort();
            }
        }
        Ok(())
    }
}

/// Resolve the explicit invocation first. Null is valid for a synchronous body
/// executing inside the canonical actor context.
///
/// # Safety
/// A non-null invocation is live and cannot be torn down during this borrow.
pub unsafe fn invocation_owner(state: *const HewCoroState) -> Option<ValueOwner> {
    // SAFETY: the caller retains the optional invocation.
    unsafe { state.as_ref() }
        .map_or_else(current_owner, |state| state.execution().value_owner.clone())
}

/// Reserve all materialization charges before any input slot is consumed.
/// Unbounded execution returns immediately without inspecting input graphs.
///
/// # Safety
/// Inputs describe live slots, and out is a writable initially empty pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_budget_materialize_begin(
    state: *const HewCoroState,
    fresh_bytes: u64,
    inputs: *const HewManagedInput,
    count: usize,
    out: *mut *mut MaterializationPermit,
) -> i32 {
    // SAFETY: out belongs exclusively to this checked operation.
    unsafe { out.write(core::ptr::null_mut()) };
    // SAFETY: the caller retains its optional invocation through publication.
    let Some(owner) = (unsafe { invocation_owner(state) }) else {
        return 0;
    };
    let inputs = if count == 0 {
        &[][..]
    } else {
        // SAFETY: the compiler passes one exact contiguous input table.
        unsafe { core::slice::from_raw_parts(inputs, count) }
    };
    // SAFETY: source slots remain unchanged until this preflight succeeds.
    let permit = match unsafe { MaterializationPermit::prepare(owner, fresh_bytes, inputs) } {
        Ok(permit) => permit,
        Err(status) => return status,
    };
    // SAFETY: this operation transfers one opaque permit to its generated caller.
    unsafe { out.write(Box::into_raw(Box::new(permit))) };
    0
}

/// Refuse canonical payload arithmetic overflow before executing construction.
///
/// # Safety
/// A non-null invocation is retained by the current execution.
#[no_mangle]
pub unsafe extern "C" fn hew_budget_materialize_overflow(state: *const HewCoroState) -> i32 {
    // SAFETY: caller retains its optional canonical invocation.
    if unsafe { invocation_owner(state) }.is_some() {
        hew_cabi::managed::HEAP_EXCEEDED
    } else {
        0
    }
}

/// Adopt the real result and finish moved input obligations. A failed budget
/// preflight never reaches this boundary; disagreement here is a descriptor bug.
///
/// # Safety
/// Permit is null or uniquely owned; result matches its retained layout.
#[no_mangle]
pub unsafe extern "C" fn hew_budget_materialize_commit(
    permit: *mut MaterializationPermit,
    layout: *const HewValueLayout,
    result: *const c_void,
) -> i32 {
    if permit.is_null() {
        return 0;
    }
    // SAFETY: this call consumes the sole checked permit.
    let permit = *unsafe { Box::from_raw(permit) };
    // SAFETY: result is still unpublished and matches layout.
    match unsafe { permit.commit(&*layout, result) } {
        Ok(()) => 0,
        // The checked plan already reserved every possible new byte before
        // construction consumed an input. A disagreement is an invalid native
        // descriptor, not a recoverable acquisition failure after consumption.
        Err(_) => std::process::abort(),
    }
}

/// Refund a checked operation abandoned before result publication.
///
/// # Safety
/// Permit is null or the unique uncommitted owner returned by begin.
#[no_mangle]
pub unsafe extern "C" fn hew_budget_materialize_abort(permit: *mut MaterializationPermit) {
    if !permit.is_null() {
        // SAFETY: consume exactly one opaque permit and its reservation guard.
        drop(unsafe { Box::from_raw(permit) });
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hew_cabi::managed::{HewManagedValueDescriptor, HEAP_EXCEEDED};
    use hew_cabi::value::HewTypeOwnershipKind;

    fn vector_layout() -> HewValueLayout {
        static MANAGED: HewManagedValueDescriptor = HewManagedValueDescriptor {
            slot_bytes: 8,
            visit: Some(super::super::visit::hew_managed_visit_vector),
        };
        HewValueLayout {
            size: size_of::<*mut c_void>(),
            align: align_of::<*mut c_void>(),
            ownership_kind: HewTypeOwnershipKind::LayoutManaged,
            clone_fn: None,
            drop_fn: None,
            release_start: None,
            code_owner: core::ptr::null(),
            managed: &raw const MANAGED,
        }
    }

    #[test]
    fn moved_input_stays_owned_until_the_private_result_is_adopted() {
        let owner = ValueOwner::new(Some(14));
        let text_layout = &crate::layout_intrinsics::hew_layout_val_string;
        // SAFETY: real string/vector values match their retained descriptors.
        unsafe {
            let text = hew_cabi::string::string_from_str("retain");
            owner
                .acquire_value(text_layout, (&raw const text).cast())
                .unwrap()
                .commit();
            let input = HewManagedInput {
                layout: text_layout,
                slot: (&raw const text).cast(),
                copies: 0,
                consume: 1,
            };
            let permit = MaterializationPermit::prepare(owner.clone(), 8, &[input]).unwrap();
            assert_eq!(owner.live_bytes(), 6);
            let value = crate::vec::hew_vec_new_with_elem_layout_capacity(text_layout, 1);
            crate::vec::hew_vec_push_owned_move(value, (&raw const text).cast_mut().cast());
            permit
                .commit(&vector_layout(), (&raw const value).cast())
                .unwrap();
            assert_eq!(owner.live_bytes(), 14);
            owner
                .release_value(&vector_layout(), (&raw const value).cast())
                .unwrap();
            assert_eq!(owner.live_bytes(), 0);
            crate::vec::hew_vec_free_owned(value);
        }
    }

    #[test]
    fn refused_private_capacity_preserves_the_original_input_and_charge() {
        let owner = ValueOwner::new(Some(13));
        let layout = &crate::layout_intrinsics::hew_layout_val_string;
        // SAFETY: this live string remains exclusively owned by the test.
        unsafe {
            let text = hew_cabi::string::string_from_str("retain");
            owner
                .acquire_value(layout, (&raw const text).cast())
                .unwrap()
                .commit();
            let input = HewManagedInput {
                layout,
                slot: (&raw const text).cast(),
                copies: 0,
                consume: 1,
            };
            assert!(matches!(
                MaterializationPermit::prepare(owner.clone(), 8, &[input]),
                Err(HEAP_EXCEEDED)
            ));
            assert_eq!(owner.live_bytes(), 6);
            owner
                .release_value(layout, (&raw const text).cast())
                .unwrap();
            hew_cabi::string::string_release(text);
        }
    }
}
