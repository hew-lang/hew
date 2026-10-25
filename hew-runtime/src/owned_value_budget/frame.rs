//! Compiler-owned live slot views for suspended invocation payloads.

use crate::coro_state::HewCoroState;
use hew_cabi::managed::{
    visit_value, HewManagedFrame, HewManagedStorageVisitor, HewManagedWalkKind,
};

/// Bind the frame's typed live roots after its parameter storage is initialized.
///
/// # Safety
/// State and view belong to the same invocation; the frame pins all view data.
#[no_mangle]
pub unsafe extern "C" fn hew_budget_bind_frame(
    state: *mut HewCoroState,
    frame: *const HewManagedFrame,
) {
    // SAFETY: HewCoroState transparently embeds this canonical ABI record.
    unsafe { (*state.cast::<hew_cabi::coro::HewCoroInvocation>()).managed_frame = frame };
}

/// Visit retained language values, never native coroutine or Rust frame bytes.
/// The provider serializes execution before requesting this view.
///
/// # Safety
/// State is live and exclusively frozen against execution/teardown. Visitor is
/// a synchronous canonical walk retained by its owner for the whole traversal.
#[no_mangle]
pub unsafe extern "C" fn hew_managed_visit_invocation(
    state: *mut HewCoroState,
    visitor: *const HewManagedStorageVisitor,
) -> i32 {
    if state.is_null() {
        return 0;
    }
    // SAFETY: the canonical record and pinned view outlive this frozen walk.
    let record = unsafe { &*state.cast::<hew_cabi::coro::HewCoroInvocation>() };
    if let Some(frame) = unsafe { record.managed_frame.as_ref() } {
        let slots = if frame.count == 0 {
            &[][..]
        } else {
            // SAFETY: the compiler initialized this exact contiguous root table.
            unsafe { core::slice::from_raw_parts(frame.slots, frame.count) }
        };
        for entry in slots {
            // SAFETY: the same occupancy flag drives semantic cleanup.
            if unsafe { entry.initialized.read() } == 0 {
                continue;
            }
            // SAFETY: only initialized slots are exposed by the frozen frame.
            let status = unsafe { visit_value(&*entry.layout, entry.slot, visitor) };
            if status != 0 {
                return status;
            }
        }
        if let Some(operation) = unsafe { frame.operation.as_ref() } {
            if let Some(layout) = unsafe { operation.layout.as_ref() } {
                // SAFETY: the active protocol owns this typed handle until
                // its emitter clears the same root before consumption/free.
                let status = unsafe { visit_value(layout, operation.slot, visitor) };
                if status != 0 {
                    return status;
                }
            }
        }
        if !frame.child.is_null() {
            // SAFETY: a non-null child remains pinned by its suspended parent.
            let child = unsafe { frame.child.read() }
                .cast_mut()
                .cast::<HewCoroState>();
            let status = unsafe { hew_managed_visit_invocation(child, visitor) };
            if status != 0 {
                return status;
            }
        }
    }
    // Rebinding is a commit-only mutation after destination quota acquisition.
    // Provider freeze protects this invocation and its retained contexts.
    if unsafe { (*visitor).kind } == HewManagedWalkKind::CommitTransfer {
        let owner = unsafe { super::transfer::destination_owner(visitor) };
        let binding = unsafe { super::transfer::destination_binding(visitor) };
        unsafe { crate::coro_state::rebind_value_owner(state, owner, binding) };
    }
    0
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::owned_value_budget::ValueOwner;
    use core::ffi::c_void;
    use hew_cabi::managed::{HewManagedFrameSlot, HewManagedValueDescriptor, HEAP_EXCEEDED};
    use hew_cabi::value::{HewTypeOwnershipKind, HewValueLayout};

    unsafe extern "C" fn visit(
        slot: *const c_void,
        visitor: *const HewManagedStorageVisitor,
    ) -> i32 {
        // SAFETY: this test descriptor holds one retained invocation pointer.
        unsafe { hew_managed_visit_invocation(slot.cast::<*mut HewCoroState>().read(), visitor) }
    }

    #[test]
    fn frozen_frame_visits_live_slots_operation_and_suspended_child() {
        let (_ready, waker) = crate::wake::blocking::Readiness::new();
        // SAFETY: all stack views and real string values outlive their bound
        // invocations; both invocations are idle during each account walk.
        unsafe {
            let root =
                crate::coro_state::hew_coro_state_new(waker.descriptor(), core::ptr::null_mut());
            let child =
                crate::coro_state::hew_coro_state_new(waker.descriptor(), core::ptr::null_mut());
            let text = hew_cabi::string::string_from_str("roots");
            let child_text = hew_cabi::string::string_from_str("nested");
            let operation_text = hew_cabi::string::string_from_str("protocol");
            let live = 1_u8;
            let absent = 0_u8;
            let text_layout = &crate::layout_intrinsics::hew_layout_val_string;
            let entries = [
                HewManagedFrameSlot {
                    layout: text_layout,
                    slot: (&raw const text).cast(),
                    initialized: &raw const live,
                },
                // An inactive slot may contain no initialized value at all.
                HewManagedFrameSlot {
                    layout: text_layout,
                    slot: core::ptr::null(),
                    initialized: &raw const absent,
                },
            ];
            let child_entry = HewManagedFrameSlot {
                layout: text_layout,
                slot: (&raw const child_text).cast(),
                initialized: &raw const live,
            };
            let child_view = HewManagedFrame {
                operation: core::ptr::null(),
                slots: &raw const child_entry,
                count: 1,
                child: core::ptr::null(),
            };
            let child_record = child
                .cast::<hew_cabi::coro::HewCoroInvocation>()
                .cast_const();
            let operation = hew_cabi::managed::HewManagedFrameOperation {
                layout: text_layout,
                slot: (&raw const operation_text).cast(),
            };
            let view = HewManagedFrame {
                operation: &raw const operation,
                slots: entries.as_ptr(),
                count: entries.len(),
                child: &raw const child_record,
            };
            hew_budget_bind_frame(root, &raw const view);
            hew_budget_bind_frame(child, &raw const child_view);
            let managed = HewManagedValueDescriptor {
                slot_bytes: 0,
                visit: Some(visit),
            };
            let layout = HewValueLayout {
                size: size_of::<*mut HewCoroState>(),
                align: align_of::<*mut HewCoroState>(),
                ownership_kind: HewTypeOwnershipKind::Plain,
                clone_fn: None,
                drop_fn: None,
                release_start: None,
                code_owner: core::ptr::null(),
                managed: &raw const managed,
            };
            let small = ValueOwner::new(Some(18));
            assert!(matches!(
                small.acquire_value(&layout, (&raw const root).cast()),
                Err(HEAP_EXCEEDED)
            ));
            assert_eq!(small.live_bytes(), 0);
            let exact = ValueOwner::new(Some(19));
            exact
                .acquire_value(&layout, (&raw const root).cast())
                .unwrap()
                .commit();
            assert_eq!(exact.live_bytes(), 19);
            exact
                .release_value(&layout, (&raw const root).cast())
                .unwrap();
            assert_eq!(exact.live_bytes(), 0);
            hew_budget_bind_frame(root, core::ptr::null());
            hew_budget_bind_frame(child, core::ptr::null());
            crate::coro_state::hew_coro_state_free(child);
            crate::coro_state::hew_coro_state_free(root);
            hew_cabi::string::string_release(text);
            hew_cabi::string::string_release(child_text);
            hew_cabi::string::string_release(operation_text);
        }
    }
}
