//! Checked reply publication retains its payload while destination providers pause.

use super::{release_gate, HewReplyChannel};
use crate::coro_state::{CoroStatus, HewCoroState};
use crate::owned_value_budget::{transfer::ValueTransfer, ValueOwner};
use crate::release_walker::{HewReleaseCursor, ReleaseItem};
use crate::util::MutexExt;
use crate::wake::OwnedWaker;
use hew_cabi::managed::{HewManagedStorageVisitor, HewManagedValueDescriptor};
use hew_cabi::value::{HewTypeOwnershipKind, HewValueLayout};
use std::{ffi::c_void, ptr, sync::atomic::Ordering};

#[derive(Debug)]
struct ReplyPayload {
    value: *mut c_void,
    layout: *const HewValueLayout,
}

unsafe extern "C" fn visit_payload(
    slot: *const c_void,
    visitor: *const HewManagedStorageVisitor,
) -> i32 {
    // SAFETY: an operation pins this internal typed payload view during each walk.
    unsafe {
        let payload = &*slot.cast::<ReplyPayload>();
        if payload.value.is_null() {
            return 0;
        }
        let width = match crate::owned_value_budget::visit::slot_bytes(&*payload.layout) {
            Ok(width) => width,
            Err(failure) => return failure,
        };
        match crate::owned_value_budget::visit::storage(visitor, payload.value, width) {
            hew_cabi::managed::DESCEND => {
                hew_cabi::managed::visit_value(&*payload.layout, payload.value, visitor)
            }
            hew_cabi::managed::SKIP_DESCENDANTS => 0,
            failure => failure,
        }
    }
}

static PAYLOAD_MANAGED: HewManagedValueDescriptor = HewManagedValueDescriptor {
    slot_bytes: 0,
    visit: Some(visit_payload),
};
static PAYLOAD_LAYOUT: HewValueLayout = HewValueLayout {
    size: size_of::<ReplyPayload>(),
    align: align_of::<ReplyPayload>(),
    ownership_kind: HewTypeOwnershipKind::Plain,
    clone_fn: None,
    drop_fn: None,
    release_start: None,
    managed: &PAYLOAD_MANAGED,
    code_owner: ptr::null(),
};

/// One sender reference and one typed reply until publication or consuming cleanup.
#[derive(Debug)]
pub struct HewNativeReply {
    payload: ReplyPayload,
    owner: Option<ValueOwner>,
    channel: *mut HewReplyChannel,
    transfer: Option<ValueTransfer>,
    terminal: bool,
    taken: bool,
    code: crate::code_owner::CodeRoot,
}

/// Reserve the reply wrapper before consuming the handler's initialized result.
/// # Safety
/// The result matches the immutable layout and remains owned by the caller on
/// a null return. A successful return consumes it into the returned operation.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_reply_start_native(
    value: *mut c_void,
    layout: *const HewValueLayout,
    _parent: *mut HewCoroState,
    out_status: *mut i32,
) -> *mut HewNativeReply {
    // SAFETY: generated code lends the initialized result and writable status.
    unsafe {
        out_status.write(0);
        let owner = crate::owned_value_budget::current_owner();
        let width = if owner.is_some() {
            match crate::owned_value_budget::visit::slot_bytes(&*layout) {
                Ok(width) => width,
                Err(failure) => {
                    out_status.write(failure);
                    return ptr::null_mut();
                }
            }
        } else {
            0
        };
        let reservation = match owner
            .as_ref()
            .map(|owner| owner.reserve_new(width))
            .transpose()
        {
            Ok(reservation) => reservation,
            Err(_) => {
                out_status.write(hew_cabi::managed::HEAP_EXCEEDED);
                return ptr::null_mut();
            }
        };
        let copied = if (*layout).size == 0 {
            ptr::null_mut()
        } else {
            super::super::alloc_reply_buffer((*layout).size)
        };
        if (*layout).size != 0 && copied.is_null() {
            out_status.write(hew_cabi::managed::HEAP_EXCEEDED);
            return ptr::null_mut();
        }
        if !copied.is_null() {
            ptr::copy_nonoverlapping(value.cast::<u8>(), copied.cast::<u8>(), (*layout).size);
        }
        let mut operation = Box::new(HewNativeReply {
            payload: ReplyPayload {
                value: copied,
                layout,
            },
            owner: owner.clone(),
            channel: ptr::null_mut(),
            transfer: None,
            terminal: false,
            taken: false,
            code: crate::code_owner::CodeRoot::acquire((*layout).code_owner),
        });
        if let Some(reservation) = reservation {
            if let Err(failure) =
                reservation.commit_value(&PAYLOAD_LAYOUT, ptr::from_ref(&operation.payload).cast())
            {
                crate::mem::buf_free(copied);
                operation.payload.value = ptr::null_mut();
                operation.taken = true;
                out_status.write(failure);
                return ptr::null_mut();
            }
            owner
                .as_ref()
                .unwrap()
                .release_value(&*layout, value)
                .unwrap_or_else(|_| std::process::abort());
        }
        operation.channel = crate::execution_context::hew_get_reply_channel().cast();
        if !operation.channel.is_null() {
            crate::execution_context::mark_current_reply_channel_consumed(operation.channel.cast());
        }
        Box::into_raw(operation)
    }
}

/// Poll publication without blocking the current scheduler worker.
/// Recipient quota refusal terminates normally and leaves the value for cleanup.
/// # Safety
/// Operation and parent invocation remain exclusively borrowed through the poll.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_reply_poll_native(
    operation: *mut HewNativeReply,
    parent: *mut HewCoroState,
) -> i32 {
    // SAFETY: the checked continuation owns this operation until terminal cleanup.
    unsafe {
        let identity = operation as usize;
        let operation = &mut *operation;
        if operation.terminal {
            return CoroStatus::Complete as i32;
        }
        let channel = operation.channel;
        if channel.is_null() {
            operation.terminal = true;
            return CoroStatus::Complete as i32;
        }
        if (*channel).cancelled.load(Ordering::Acquire) {
            operation.transfer = None;
            release_gate(channel, identity);
            operation.terminal = true;
            return CoroStatus::Complete as i32;
        }
        let wake = OwnedWaker::retain(&*crate::coro_state::hew_coro_state_waker(parent));
        let (destination, binding) = {
            let mut owner = (*channel).native.lock_or_recover();
            if !owner.claim(identity, Some(wake.clone())) {
                return CoroStatus::Pending as i32;
            }
            (owner.value_owner.clone(), owner.binding.clone())
        };
        if operation.transfer.is_none() {
            let mut transfer = ValueTransfer::begin(
                operation.owner.clone(),
                destination.clone(),
                &PAYLOAD_LAYOUT,
                ptr::from_ref(&operation.payload).cast(),
            );
            transfer.set_destination_binding(binding);
            transfer.set_waker(wake);
            operation.transfer = Some(transfer);
        }
        match operation.transfer.as_mut().unwrap().poll_prepare() {
            Ok(false) => return CoroStatus::Pending as i32,
            Err(_) => {
                operation.transfer = None;
                release_gate(channel, identity);
                operation.terminal = true;
                if let Some(owner) = destination {
                    owner.heap_exceeded();
                }
                return CoroStatus::Complete as i32;
            }
            Ok(true) => {}
        }
        operation
            .transfer
            .take()
            .unwrap()
            .commit()
            .unwrap_or_else(|_| std::process::abort());
        operation.owner = destination;
        let wake = {
            let mut owner = (*channel).native.lock_or_recover();
            if !(*channel).cancelled.load(Ordering::Acquire) {
                (*channel).value = ptr::replace(&mut operation.payload.value, ptr::null_mut());
                (*channel).value_size = (*operation.payload.layout).size;
                owner.payload_owned = true;
                (*channel).ready.store(true, Ordering::Release);
                operation.taken = true;
            }
            owner.waker.take()
        };
        release_gate(channel, identity);
        operation.terminal = true;
        if let Some(wake) = wake {
            wake.wake();
        }
        CoroStatus::Complete as i32
    }
}

struct ReplyAllocation {
    value: *mut c_void,
    width: u64,
    owner: Option<ValueOwner>,
    _code: crate::code_owner::CodeRoot,
}

unsafe fn free_reply_allocation(raw: *mut c_void) {
    // SAFETY: the cursor runs this after every typed child has completed release.
    unsafe {
        let allocation = Box::from_raw(raw.cast::<ReplyAllocation>());
        if let Some(owner) = &allocation.owner {
            owner.release_backing(allocation.value, allocation.width);
        }
        crate::mem::buf_free(allocation.value);
    }
}

/// Hand an unpublished result to the existing typed consuming cleanup walker.
/// # Safety
/// Publication is terminal and this function is called once before free.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_reply_take_cleanup_native(
    operation: *mut HewNativeReply,
) -> *mut HewReleaseCursor {
    // SAFETY: terminal ownership belongs exclusively to this checked continuation.
    unsafe {
        let operation = &mut *operation;
        assert!(operation.terminal);
        if operation.taken {
            return ptr::null_mut();
        }
        operation.taken = true;
        let value = ptr::replace(&mut operation.payload.value, ptr::null_mut());
        if value.is_null() {
            return ptr::null_mut();
        }
        release_payload(
            value,
            &*operation.payload.layout,
            operation.owner.clone(),
            operation.code.clone(),
        )
    }
}

pub(super) unsafe fn release_payload(
    value: *mut c_void,
    layout: &HewValueLayout,
    owner: Option<ValueOwner>,
    code: crate::code_owner::CodeRoot,
) -> *mut HewReleaseCursor {
    // SAFETY: the caller transfers one initialized typed reply allocation.
    unsafe {
        let allocation = Box::new(ReplyAllocation {
            value,
            width: if owner.is_some() {
                crate::owned_value_budget::visit::slot_bytes(layout)
                    .unwrap_or_else(|_| std::process::abort())
            } else {
                0
            },
            owner: owner.clone(),
            _code: code,
        });
        let cursor = HewReleaseCursor::new(vec![
            ReleaseItem::Storage {
                owner: Box::into_raw(allocation).cast(),
                free: free_reply_allocation,
            },
            ReleaseItem::Value {
                slot: value,
                layout: *layout,
            },
        ]);
        (*cursor).set_value_owner(owner);
        cursor
    }
}

/// Release a terminal operation after taking its remaining cleanup obligations.
/// # Safety
/// The caller owns this operation and has stopped polling it.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_reply_free_native(operation: *mut HewNativeReply) {
    if operation.is_null() {
        return;
    }
    // SAFETY: the caller transfers one final operation owner.
    unsafe {
        let operation = Box::from_raw(operation);
        assert!(operation.terminal && operation.taken && operation.transfer.is_none());
        if !operation.channel.is_null() {
            super::super::hew_reply_channel_free(operation.channel);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::execution_context::{HewExecutionContext, TestExecutionContext};
    use crate::wake::blocking::Readiness;

    static SCALAR_MANAGED: HewManagedValueDescriptor = HewManagedValueDescriptor {
        slot_bytes: 8,
        visit: None,
    };
    static SCALAR: HewValueLayout = HewValueLayout {
        size: 8,
        align: 8,
        managed: &SCALAR_MANAGED,
        ..PAYLOAD_LAYOUT
    };

    unsafe extern "C" fn visit_channel(
        slot: *const c_void,
        visitor: *const HewManagedStorageVisitor,
    ) -> i32 {
        // SAFETY: this fixture retains the receiving channel through each walk.
        unsafe {
            super::super::visit_managed_reply(slot.cast::<*mut HewReplyChannel>().read(), visitor)
        }
    }
    static CHANNEL_MANAGED: HewManagedValueDescriptor = HewManagedValueDescriptor {
        slot_bytes: 0,
        visit: Some(visit_channel),
    };
    static CHANNEL: HewValueLayout = HewValueLayout {
        size: size_of::<*mut HewReplyChannel>(),
        align: align_of::<*mut HewReplyChannel>(),
        managed: &CHANNEL_MANAGED,
        ..PAYLOAD_LAYOUT
    };

    #[test]
    fn publication_waits_for_receiver_transfer_and_uses_its_new_account() {
        let (_ready, wake) = Readiness::new();
        let source = ValueOwner::new(Some(8));
        let original = ValueOwner::new(Some(8));
        let destination = ValueOwner::new(Some(8));
        // SAFETY: the fixture owns both channel references and every typed slot.
        unsafe {
            let channel =
                crate::owned_value_budget::with_value_owner(&Some(original.clone()), || {
                    super::super::hew_reply_channel_new_native(
                        wake.descriptor(),
                        None,
                        None,
                        &SCALAR,
                    )
                });
            super::super::super::hew_reply_channel_retain(channel);
            let _context = TestExecutionContext::install(HewExecutionContext {
                reply_channel: channel.cast(),
                ..HewExecutionContext::default()
            });
            let parent = crate::coro_state::hew_coro_state_new(wake.descriptor(), ptr::null_mut());
            let mut value = 42_u64;
            let mut status = -1;
            let operation =
                crate::owned_value_budget::with_value_owner(&Some(source.clone()), || {
                    hew_actor_reply_start_native(
                        (&raw mut value).cast(),
                        &SCALAR,
                        parent,
                        &raw mut status,
                    )
                });
            assert!(!operation.is_null());
            assert_eq!(status, 0);
            assert_eq!(source.live_bytes(), 8);
            let mut transfer = ValueTransfer::begin(
                Some(original.clone()),
                Some(destination.clone()),
                &CHANNEL,
                (&raw const channel).cast(),
            );
            transfer.set_waker(OwnedWaker::retain(&*wake.descriptor()));
            assert!(transfer.poll_prepare().unwrap());
            assert_eq!(
                hew_actor_reply_poll_native(operation, parent),
                CoroStatus::Pending as i32
            );
            assert_eq!(source.live_bytes(), 8);
            assert_eq!(destination.live_bytes(), 0);
            transfer.commit().unwrap();
            assert_eq!(
                hew_actor_reply_poll_native(operation, parent),
                CoroStatus::Complete as i32
            );
            assert_eq!(source.live_bytes(), 0);
            assert_eq!(original.live_bytes(), 0);
            assert_eq!(destination.live_bytes(), 8);
            assert!(hew_actor_reply_take_cleanup_native(operation).is_null());
            hew_actor_reply_free_native(operation);
            let mut output = 0_u64;
            assert_eq!(
                super::super::hew_reply_channel_poll_native(channel, 8, (&raw mut output).cast()),
                0
            );
            assert_eq!(output, 42);
            assert_eq!(destination.live_bytes(), 0);
            super::super::super::hew_reply_channel_free(channel);
            crate::coro_state::hew_coro_state_free(parent);
        }
    }

    #[test]
    fn refused_reply_wrapper_preserves_the_initialized_handler_value() {
        let (_ready, wake) = Readiness::new();
        let source = ValueOwner::new(Some(7));
        // SAFETY: the fixture retains the input and invocation through refusal.
        unsafe {
            let parent = crate::coro_state::hew_coro_state_new(wake.descriptor(), ptr::null_mut());
            let mut value = 42_u64;
            let mut status = 0;
            let operation =
                crate::owned_value_budget::with_value_owner(&Some(source.clone()), || {
                    hew_actor_reply_start_native(
                        (&raw mut value).cast(),
                        &SCALAR,
                        parent,
                        &raw mut status,
                    )
                });
            assert!(operation.is_null());
            assert_eq!(status, hew_cabi::managed::HEAP_EXCEEDED);
            assert_eq!(value, 42);
            assert_eq!(source.live_bytes(), 0);
            crate::coro_state::hew_coro_state_free(parent);
        }
    }

    #[test]
    fn recipient_refusal_keeps_the_reply_charged_until_sender_cleanup() {
        let (_ready, wake) = Readiness::new();
        let source = ValueOwner::new(Some(8));
        let destination = ValueOwner::new(Some(0));
        // SAFETY: the fixture owns both channel references and drains the reply.
        unsafe {
            let channel =
                crate::owned_value_budget::with_value_owner(&Some(destination.clone()), || {
                    super::super::hew_reply_channel_new_native(
                        wake.descriptor(),
                        None,
                        None,
                        &SCALAR,
                    )
                });
            super::super::super::hew_reply_channel_retain(channel);
            let _context = TestExecutionContext::install(HewExecutionContext {
                reply_channel: channel.cast(),
                ..HewExecutionContext::default()
            });
            let parent = crate::coro_state::hew_coro_state_new(wake.descriptor(), ptr::null_mut());
            let mut value = 42_u64;
            let mut status = -1;
            let operation =
                crate::owned_value_budget::with_value_owner(&Some(source.clone()), || {
                    hew_actor_reply_start_native(
                        (&raw mut value).cast(),
                        &SCALAR,
                        parent,
                        &raw mut status,
                    )
                });
            assert_eq!(status, 0);
            assert_eq!(
                hew_actor_reply_poll_native(operation, parent),
                CoroStatus::Complete as i32
            );
            assert_eq!(source.live_bytes(), 8);
            assert_eq!(destination.live_bytes(), 0);
            let cleanup = hew_actor_reply_take_cleanup_native(operation);
            assert!(!cleanup.is_null());
            crate::release_walker::hew_release_sync(cleanup);
            assert_eq!(source.live_bytes(), 0);
            hew_actor_reply_free_native(operation);
            super::super::super::hew_reply_channel_free(channel);
            crate::coro_state::hew_coro_state_free(parent);
        }
    }

    unsafe extern "C" fn cancel_after_adoption(
        slot: *const c_void,
        visitor: *const HewManagedStorageVisitor,
    ) -> i32 {
        // SAFETY: this controlled provider transition models cancellation after
        // adoption and before the publisher can acquire its publication lock.
        unsafe {
            if (*visitor).kind == hew_cabi::managed::HewManagedWalkKind::ResumeTransfer {
                super::super::super::hew_reply_channel_cancel(
                    slot.cast::<*mut HewReplyChannel>().read(),
                );
            }
        }
        0
    }

    #[test]
    fn cancellation_after_adoption_cleans_the_destination_owned_wrapper() {
        #[repr(C)]
        struct Payload {
            channel: *mut HewReplyChannel,
            expected: *const ValueOwner,
        }
        unsafe extern "C-unwind" fn close(slot: *mut c_void) {
            // SAFETY: this fixture retains the expected account through cleanup.
            let expected = unsafe { &*(*slot.cast::<Payload>()).expected };
            assert!(crate::owned_value_budget::current_owner()
                .unwrap()
                .same_account(expected));
        }
        static MANAGED: HewManagedValueDescriptor = HewManagedValueDescriptor {
            slot_bytes: 16,
            visit: Some(cancel_after_adoption),
        };
        static LAYOUT: HewValueLayout = HewValueLayout {
            size: size_of::<Payload>(),
            align: align_of::<Payload>(),
            managed: &MANAGED,
            drop_fn: Some(close),
            ..SCALAR
        };
        let (_ready, wake) = Readiness::new();
        let source = ValueOwner::new(Some(16));
        let destination = ValueOwner::new(Some(16));
        // SAFETY: the fixture pins the channel until the cancelled publisher drains.
        unsafe {
            let channel =
                crate::owned_value_budget::with_value_owner(&Some(destination.clone()), || {
                    super::super::hew_reply_channel_new_native(
                        wake.descriptor(),
                        None,
                        None,
                        &LAYOUT,
                    )
                });
            super::super::super::hew_reply_channel_retain(channel);
            let _context = TestExecutionContext::install(HewExecutionContext {
                reply_channel: channel.cast(),
                ..HewExecutionContext::default()
            });
            let parent = crate::coro_state::hew_coro_state_new(wake.descriptor(), ptr::null_mut());
            let mut value = Payload {
                channel,
                expected: &destination,
            };
            let mut status = -1;
            let operation =
                crate::owned_value_budget::with_value_owner(&Some(source.clone()), || {
                    hew_actor_reply_start_native(
                        (&raw mut value).cast(),
                        &LAYOUT,
                        parent,
                        &raw mut status,
                    )
                });
            assert_eq!(status, 0);
            assert_eq!(
                hew_actor_reply_poll_native(operation, parent),
                CoroStatus::Complete as i32
            );
            assert_eq!(source.live_bytes(), 0);
            assert_eq!(destination.live_bytes(), 16);
            let cleanup = hew_actor_reply_take_cleanup_native(operation);
            assert!(!cleanup.is_null());
            let mut driver = crate::release_walker::ReleaseDriver::new(cleanup);
            assert!(driver.poll(parent));
            assert!(driver.take_fault().is_null());
            drop(driver);
            assert_eq!(destination.live_bytes(), 0);
            hew_actor_reply_free_native(operation);
            super::super::super::hew_reply_channel_free(channel);
            crate::coro_state::hew_coro_state_free(parent);
        }
    }
}
