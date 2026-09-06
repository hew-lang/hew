//! Cooperative cleanup of borrowed children selected by the shared value layout.
//! The collector retains no storage ownership. Its source value and descriptor
//! remain live until all selected children drain and the collector is released.

use crate::coro_state::CoroStatus;
use crate::fault::{hew_fault_combine, hew_fault_drop, HewFault};
use hew_cabi::value::{HewValueClosePoll, HewValueLayout};
use std::{ffi::c_void, ptr};

#[derive(Debug)]
struct Child {
    owner: *mut c_void,
    poll: HewValueClosePoll,
}

#[derive(Debug, Default)]
pub struct HewValueClose {
    children: Vec<Child>,
    next: usize,
    fault: *mut HewFault,
}

impl Drop for HewValueClose {
    fn drop(&mut self) {
        if self.next != self.children.len() {
            std::process::abort();
        }
        // SAFETY: the collector owns its optional combined fault.
        unsafe { hew_fault_drop(self.fault) };
    }
}

/// Append an initialized child to the compiler-selected cleanup order.
///
/// # Safety
/// `context` points to a writable collector pointer initially null. The owner
/// is uniquely borrowed until this collector completes; its poll callback obeys
/// the shared completion contract and never releases the owner's storage.
#[no_mangle]
pub unsafe extern "C" fn hew_value_close_push(
    context: *mut c_void,
    owner: *mut c_void,
    poll: HewValueClosePoll,
) {
    // SAFETY: the generated walk supplies its caller's live collector slot.
    let slot = unsafe { &mut *context.cast::<*mut HewValueClose>() };
    if slot.is_null() {
        *slot = Box::into_raw(Box::default());
    }
    // SAFETY: construction above establishes the unique collector owner.
    unsafe { &mut **slot }.children.push(Child { owner, poll });
}

/// Collect the initialized children of an exact value layout.
///
/// # Safety
/// The slot has this layout and remains exclusively borrowed until close has
/// completed. `context` obeys `hew_value_close_push`'s collector-slot contract.
#[no_mangle]
pub unsafe extern "C" fn hew_value_close_collect(
    slot: *mut c_void,
    layout: *const HewValueLayout,
    context: *mut c_void,
) {
    // SAFETY: the checked caller supplies the exact live immutable descriptor.
    if let Some(visit) = unsafe { (*layout).visit_close } {
        // SAFETY: the descriptor's walk borrows this exact initialized value.
        unsafe { visit(slot, context) };
    }
}

/// Drain selected children in their ordinary destruction order, preserving
/// faults while continuing to close the remaining owners.
///
/// # Safety
/// The collector and every selected child remain live and uniquely borrowed.
/// `parent` is a retained shared invocation state through this poll.
#[no_mangle]
pub unsafe extern "C" fn hew_value_close_poll(
    collector: *mut HewValueClose,
    parent: *mut c_void,
) -> i32 {
    // SAFETY: null denotes an empty child walk; otherwise the caller owns it.
    let Some(collector) = (unsafe { collector.as_mut() }) else {
        return CoroStatus::Complete as i32;
    };
    while let Some(child) = collector.children.get(collector.next) {
        let mut fault = ptr::null_mut();
        // SAFETY: collection retained each exact owner and callback contract.
        let status = unsafe { (child.poll)(child.owner, parent, &raw mut fault) };
        if status == CoroStatus::Pending as i32 {
            if !fault.is_null() {
                std::process::abort();
            }
            return status;
        }
        if status != CoroStatus::Complete as i32 && status != CoroStatus::Fault as i32 {
            std::process::abort();
        }
        // SAFETY: the callback transfers its optional fault; the collector owns
        // its previous primary. Closing subsequent children cannot erase it.
        collector.fault = unsafe { hew_fault_combine(collector.fault, fault.cast()) };
        collector.next += 1;
    }
    (if collector.fault.is_null() {
        CoroStatus::Complete
    } else {
        CoroStatus::Fault
    }) as i32
}

/// Consume a completed collector, transferring its optional combined fault.
///
/// # Safety
/// The unique collector is null or has completed. `fault` is an uninitialized
/// writable fault slot. No child borrow survives this operation.
#[no_mangle]
pub unsafe extern "C" fn hew_value_close_finish(
    collector: *mut HewValueClose,
    fault: *mut *mut HewFault,
) -> i32 {
    if collector.is_null() {
        // SAFETY: the caller supplies an uninitialized output slot.
        unsafe { fault.write(ptr::null_mut()) };
        return 0;
    }
    // SAFETY: transfer the unique collector and its optional fault allocation.
    let mut collector = unsafe { Box::from_raw(collector) };
    let result = std::mem::take(&mut collector.fault);
    // SAFETY: the fault is uniquely owned and the caller supplied its output slot.
    unsafe {
        fault.write(result);
        result.as_ref().map_or(0, HewFault::code)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct Owner {
        polls: usize,
        fail: bool,
    }

    unsafe extern "C" fn poll_owner(
        owner: *mut c_void,
        _parent: *mut c_void,
        fault: *mut *mut c_void,
    ) -> i32 {
        // SAFETY: this test retains both owners until the collector finishes.
        let owner = unsafe { &mut *owner.cast::<Owner>() };
        owner.polls += 1;
        if owner.polls == 1 {
            return CoroStatus::Pending as i32;
        }
        if owner.fail {
            // SAFETY: the collector supplies one writable optional fault slot.
            unsafe { fault.write(crate::fault::hew_fault_new(212).cast()) };
            CoroStatus::Fault as i32
        } else {
            CoroStatus::Complete as i32
        }
    }

    #[test]
    fn pending_and_fault_preserve_the_remaining_child_borrows() {
        let mut first = Owner {
            polls: 0,
            fail: true,
        };
        let mut second = Owner {
            polls: 0,
            fail: false,
        };
        let mut collector: *mut HewValueClose = ptr::null_mut();
        // SAFETY: each child remains live and unchanged while the collector
        // holds its borrow; the test releases the collector and returned fault.
        unsafe {
            hew_value_close_push(
                (&raw mut collector).cast(),
                (&raw mut first).cast(),
                poll_owner,
            );
            hew_value_close_push(
                (&raw mut collector).cast(),
                (&raw mut second).cast(),
                poll_owner,
            );
            assert_eq!(
                hew_value_close_poll(collector, ptr::null_mut()),
                CoroStatus::Pending as i32
            );
            assert_eq!((first.polls, second.polls), (1, 0));
            assert_eq!(
                hew_value_close_poll(collector, ptr::null_mut()),
                CoroStatus::Pending as i32
            );
            assert_eq!((first.polls, second.polls), (2, 1));
            assert_eq!(
                hew_value_close_poll(collector, ptr::null_mut()),
                CoroStatus::Fault as i32
            );
            assert_eq!((first.polls, second.polls), (2, 2));
            let mut fault = ptr::null_mut();
            assert_eq!(hew_value_close_finish(collector, &raw mut fault), 212);
            hew_fault_drop(fault);
        }
    }
}
