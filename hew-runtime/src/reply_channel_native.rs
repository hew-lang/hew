//! Non-blocking reply observation for checked native invocations.

use super::{
    hew_reply_channel_failure_kind, hew_reply_channel_free, hew_reply_channel_is_orphaned,
    hew_reply_channel_is_ready, hew_reply_channel_new, hew_reply_channel_retain,
    hew_reply_channel_set_reply_drop_fn, hew_reply_payload_free, take_ready_reply, HewReplyChannel,
    HewReplyDropFn,
};
use crate::internal::types::{AskError, HEW_REPLY_FAIL_HANDLER_TRAPPED, HEW_REPLY_FAIL_NONE};
use crate::release_walker::HewReleaseCursor;
use crate::util::MutexExt;
use crate::wake::{HewWaker, OwnedWaker};
use hew_cabi::value::HewValueReleaseStart;
use std::{ffi::c_void, ptr, sync::atomic::Ordering};

/// Create a receiver reference and register its retained readiness target and
/// typed reply destructor before any sender can publish a result.
///
/// # Safety
/// `waker` is a live descriptor. `drop_reply` destroys exactly the embedded
/// resources of the reply type, without freeing the supplied wrapper.
#[no_mangle]
pub unsafe extern "C" fn hew_reply_channel_new_native(
    waker: *const HewWaker,
    drop_reply: Option<HewReplyDropFn>,
    reply_release: Option<HewValueReleaseStart>,
) -> *mut HewReplyChannel {
    let channel = hew_reply_channel_new();
    // SAFETY: the new channel is exclusively owned and the descriptor is live.
    unsafe {
        *(*channel).native_waker.lock_or_recover() = Some(OwnedWaker::retain(&*waker));
        hew_reply_channel_set_reply_drop_fn(channel, drop_reply);
        (*channel).native_reply_release = reply_release;
    }
    channel
}

/// Poll once without blocking. Returns -1 while pending or a runtime `AskError`
/// tag (zero transfers the reply into `output`).
/// Failure leaves any deposited value with the channel's typed destructor.
///
/// # Safety
/// `channel` is the unique live receiver reference. `output` has `expected_size`
/// writable bytes. After a terminal result, release the channel without polling
/// again. The expected size and destructor describe the same reply type.
#[no_mangle]
pub unsafe extern "C" fn hew_reply_channel_poll_native(
    channel: *mut HewReplyChannel,
    expected_size: usize,
    output: *mut c_void,
) -> i32 {
    // SAFETY: observing shares the same unique receiver and expected layout.
    let status = unsafe { hew_reply_channel_status_native(channel, expected_size) };
    if status != AskError::None as i32 {
        return status;
    }
    // SAFETY: readiness published the matching payload; this is the only take.
    unsafe {
        let value = take_ready_reply(channel, None);
        if expected_size != 0 {
            ptr::copy_nonoverlapping(value.cast::<u8>(), output.cast::<u8>(), expected_size);
        }
        hew_reply_payload_free(value.cast(), expected_size);
    }
    AskError::None as i32
}

/// Observe completion without taking a reply or its embedded resources.
/// Repeated observations return the same result until the receiver takes it.
///
/// # Safety
/// `channel` is a live receiver, with no concurrent consuming observer.
#[no_mangle]
pub unsafe extern "C" fn hew_reply_channel_status_native(
    channel: *mut HewReplyChannel,
    expected_size: usize,
) -> i32 {
    // SAFETY: the caller owns the receiver reference. The ready acquire publishes
    // the sender's payload and classification before either is inspected.
    unsafe {
        if !hew_reply_channel_is_ready(channel) {
            return -1;
        }
        if hew_reply_channel_is_orphaned(channel) != 0 {
            return AskError::OrphanedAsk as i32;
        }
        if (*channel).cancelled.load(Ordering::Acquire) {
            return AskError::Cancelled as i32;
        }
        let failure = hew_reply_channel_failure_kind(channel);
        if failure == HEW_REPLY_FAIL_HANDLER_TRAPPED {
            return AskError::HandlerTrapped as i32;
        }
        if failure != HEW_REPLY_FAIL_NONE {
            return AskError::SendFailed as i32;
        }
        if (*channel).value_size != expected_size
            || (expected_size != 0 && (*channel).value.is_null())
        {
            return AskError::PayloadSizeMismatch as i32;
        }
        AskError::None as i32
    }
}

/// Submit an ask payload and retain its reply sender reference on admission.
/// This consumes the payload on every outcome, returning a runtime `AskError` tag.
///
/// # Safety
/// `payload` is a malloc wrapper whose initialized fields belong to this call.
/// `drop_payload` releases those fields without freeing the wrapper. `channel` is a live
/// receiver reference with its wake target and reply destructor already set.
#[no_mangle]
pub unsafe extern "C-unwind" fn hew_actor_ask_submit_native(
    token: crate::lifetime::local_handles::HewLocalPidId,
    message: i32,
    payload: *mut c_void,
    size: usize,
    drop_payload: crate::mailbox::HewMsgEnvelopeDropFn,
    channel: *mut HewReplyChannel,
) -> i32 {
    // SAFETY: this call owns the complete payload, including every failure leg.
    unsafe {
        let envelope = crate::mailbox::hew_msg_envelope_new(payload, size, Some(drop_payload));
        if envelope.is_null() {
            drop_payload(payload);
            crate::mem::buf_free(payload);
            return AskError::SendFailed as i32;
        }
        hew_reply_channel_retain(channel);
        let outcome =
            crate::actor::try_submit_native_request(token, message, envelope, channel.cast());
        let error = match outcome {
            crate::mailbox::SendOutcome::Enqueued => return AskError::None as i32,
            crate::mailbox::SendOutcome::Failed => AskError::MailboxFull,
            crate::mailbox::SendOutcome::Closed => AskError::ActorStopped,
            crate::mailbox::SendOutcome::Oom => AskError::SendFailed,
            _ => unreachable!("native admission does not apply implicit overflow policies"),
        };
        crate::mailbox::hew_msg_envelope_release(envelope);
        hew_reply_channel_free(channel);
        error as i32
    }
}

/// Cancel observation and detach an already deposited reply for consuming cleanup.
/// Publication and detachment use one lock, so late replies stay with their producer.
/// # Safety
/// The channel is null or retained by its unique receiving operation.
pub(crate) unsafe fn cancel_take_release(channel: *mut HewReplyChannel) -> *mut HewReleaseCursor {
    if channel.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: the operation retains the receiving reference through detachment.
    unsafe {
        let mut guard = (*channel).native_waker.lock_or_recover();
        (*channel).cancelled.store(true, Ordering::Release);
        let waker = guard.take();
        let value = std::mem::replace(&mut (*channel).value, ptr::null_mut());
        let size = (*channel).value_size;
        let start = (*channel).native_reply_release;
        drop(guard);
        drop(waker);
        if value.is_null() {
            return ptr::null_mut();
        }
        if let Some(start) = start {
            HewReleaseCursor::payload(value, size, start, true)
        } else {
            super::run_registered_reply_drop_on_value(channel, value);
            crate::mem::buf_free(value);
            ptr::null_mut()
        }
    }
}

/// Transfer a completed reply or return its undelivered owners for cleanup.
/// # Safety
/// The result slot remains live until the returned cursor is fully consumed.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_reply_native(
    value: *mut c_void,
    size: usize,
    drop_reply: Option<HewReplyDropFn>,
    reply_release: Option<HewValueReleaseStart>,
) -> *mut HewReleaseCursor {
    let channel = crate::execution_context::hew_get_reply_channel().cast::<HewReplyChannel>();
    let mut delivered = false;
    // SAFETY: this activation owns the sender reference and the live result slot.
    unsafe {
        if !channel.is_null() {
            crate::execution_context::mark_current_reply_channel_consumed(channel.cast());
            let mut guard = (*channel).native_waker.lock_or_recover();
            if !(*channel).cancelled.load(Ordering::Acquire) {
                let copied = if size == 0 {
                    ptr::null_mut()
                } else {
                    super::alloc_reply_buffer(size)
                };
                if size == 0 || !copied.is_null() {
                    if size != 0 {
                        ptr::copy_nonoverlapping(value.cast::<u8>(), copied.cast::<u8>(), size);
                    }
                    (*channel).value = copied;
                    (*channel).value_size = size;
                    delivered = true;
                } else {
                    super::hew_reply_channel_mark_allocation_failed(channel);
                }
                (*channel).ready.store(true, Ordering::Release);
            }
            let waker = guard.take();
            drop(guard);
            if let Some(waker) = waker {
                waker.wake();
            }
            super::hew_reply_channel_free(channel);
        }
        if delivered {
            ptr::null_mut()
        } else if let Some(start) = reply_release {
            HewReleaseCursor::payload(value, size, start, false)
        } else {
            if let Some(drop) = drop_reply {
                drop(value);
            }
            ptr::null_mut()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::reply_channel::{
        hew_reply, hew_reply_channel_cancel, hew_reply_channel_publish_crash_fallback,
        hew_reply_channel_retire_orphaned_ask_sender_ref,
    };
    use crate::wake::blocking::Readiness;
    use std::sync::atomic::AtomicUsize;
    use std::sync::Arc;

    #[repr(C)]
    struct Reply(*mut Arc<AtomicUsize>);

    fn reply(drops: &Arc<AtomicUsize>) -> Reply {
        Reply(Box::into_raw(Box::new(drops.clone())))
    }

    unsafe extern "C" fn drop_reply(value: *mut c_void) {
        // SAFETY: each initialized reply transfers its one Box exactly once.
        unsafe {
            let value = &mut *value.cast::<Reply>();
            let count = Box::from_raw(value.0);
            value.0 = ptr::null_mut();
            count.fetch_add(1, Ordering::SeqCst);
        }
    }

    #[test]
    fn native_ask_reply_wakes_after_creator_release_and_transfers_owned_value() {
        let (ready, waker) = Readiness::new();
        let drops = Arc::new(AtomicUsize::new(0));
        // SAFETY: descriptor and typed destructor meet the channel contract.
        unsafe {
            let channel = hew_reply_channel_new_native(waker.descriptor(), Some(drop_reply), None);
            hew_reply_channel_retain(channel);
            drop(waker);
            let mut output = std::mem::MaybeUninit::<Reply>::uninit();
            assert_eq!(
                hew_reply_channel_poll_native(
                    channel,
                    size_of::<Reply>(),
                    output.as_mut_ptr().cast()
                ),
                -1
            );
            let sender = channel as usize;
            let sender_drops = drops.clone();
            std::thread::spawn(move || {
                let mut value = reply(&sender_drops);
                assert!(hew_reply(
                    sender as *mut HewReplyChannel,
                    (&raw mut value).cast(),
                    size_of::<Reply>()
                ));
            })
            .join()
            .unwrap();
            assert!(ready.take_ready());
            assert_eq!(Arc::strong_count(&ready), 1);
            assert_eq!(
                hew_reply_channel_poll_native(
                    channel,
                    size_of::<Reply>(),
                    output.as_mut_ptr().cast()
                ),
                0
            );
            hew_reply_channel_free(channel);
            assert_eq!(drops.load(Ordering::SeqCst), 0);
            drop_reply(output.as_mut_ptr().cast());
            assert_eq!(drops.load(Ordering::SeqCst), 1);
        }
    }

    #[test]
    fn native_ask_cancel_detaches_waker_and_drops_late_owned_reply() {
        let (ready, waker) = Readiness::new();
        let drops = Arc::new(AtomicUsize::new(0));
        // SAFETY: the sender retains the channel after its receiver abandons it.
        unsafe {
            let channel = hew_reply_channel_new_native(waker.descriptor(), Some(drop_reply), None);
            hew_reply_channel_retain(channel);
            drop(waker);
            hew_reply_channel_cancel(channel);
            hew_reply_channel_free(channel);
            assert_eq!(Arc::strong_count(&ready), 1);
            let mut value = reply(&drops);
            assert!(!hew_reply(
                channel,
                (&raw mut value).cast(),
                size_of::<Reply>()
            ));
            assert!(!ready.take_ready());
            assert_eq!(drops.load(Ordering::SeqCst), 1);
        }
    }

    #[test]
    fn native_ask_rejected_payload_size_keeps_reply_with_channel_cleanup() {
        let (ready, waker) = Readiness::new();
        let drops = Arc::new(AtomicUsize::new(0));
        // SAFETY: the expected size is deliberately wrong but output is never read.
        unsafe {
            let channel = hew_reply_channel_new_native(waker.descriptor(), Some(drop_reply), None);
            hew_reply_channel_retain(channel);
            let mut value = reply(&drops);
            assert!(hew_reply(
                channel,
                (&raw mut value).cast(),
                size_of::<Reply>()
            ));
            assert!(ready.take_ready());
            assert_eq!(
                hew_reply_channel_poll_native(channel, 0, ptr::null_mut()),
                AskError::PayloadSizeMismatch as i32
            );
            hew_reply_channel_free(channel);
            assert_eq!(drops.load(Ordering::SeqCst), 1);
        }
    }

    #[test]
    fn native_ask_failed_admission_consumes_request_without_orphaning_receiver() {
        let (_ready, waker) = Readiness::new();
        let drops = Arc::new(AtomicUsize::new(0));
        // SAFETY: the unpublished malloc wrapper holds one initialized Reply.
        unsafe {
            let channel = hew_reply_channel_new_native(waker.descriptor(), Some(drop_reply), None);
            let payload = crate::mem::buf_try_alloc(size_of::<Reply>()).cast::<Reply>();
            assert!(!payload.is_null());
            payload.write(reply(&drops));
            assert_eq!(
                hew_actor_ask_submit_native(
                    crate::lifetime::local_handles::HewLocalPidId::INVALID,
                    0,
                    payload.cast(),
                    size_of::<Reply>(),
                    drop_reply,
                    channel,
                ),
                AskError::ActorStopped as i32
            );
            assert_eq!(drops.load(Ordering::SeqCst), 1);
            assert!(!hew_reply_channel_is_ready(channel));
            hew_reply_channel_cancel(channel);
            hew_reply_channel_free(channel);
        }
    }

    #[test]
    fn native_ask_full_mailbox_preserves_request_and_reply_sender_reference() {
        use crate::mailbox::{self, OverflowPolicy, SendOutcome};
        for policy in [OverflowPolicy::Fail, OverflowPolicy::Block] {
            let (ready, waker) = Readiness::new();
            let drops = Arc::new(AtomicUsize::new(0));
            // SAFETY: the test owns the mailbox, request and both reply references.
            unsafe {
                let mailbox = mailbox::hew_mailbox_new_with_policy(1, policy);
                assert!(!mailbox.is_null());
                let mut filler = 1_i32;
                assert_eq!(
                    mailbox::hew_mailbox_send(
                        mailbox,
                        0,
                        (&raw mut filler).cast(),
                        size_of::<i32>()
                    ),
                    0
                );
                let channel =
                    hew_reply_channel_new_native(waker.descriptor(), Some(drop_reply), None);
                hew_reply_channel_retain(channel);
                let payload = crate::mem::buf_try_alloc(size_of::<Reply>()).cast::<Reply>();
                assert!(!payload.is_null());
                payload.write(reply(&drops));
                let envelope = mailbox::hew_msg_envelope_new(
                    payload.cast(),
                    size_of::<Reply>(),
                    Some(drop_reply),
                );
                assert!(!envelope.is_null());
                assert!(matches!(
                    mailbox::try_admit_native_request(&*mailbox, 1, envelope, channel.cast()),
                    SendOutcome::Failed
                ));
                mailbox::hew_mailbox_free(mailbox);
                assert!(!ready.take_ready());
                assert!(!hew_reply_channel_is_ready(channel));
                assert_eq!(drops.load(Ordering::SeqCst), 0);
                mailbox::hew_msg_envelope_release(envelope);
                assert_eq!(drops.load(Ordering::SeqCst), 1);
                // The rejected node did not retire its unpublished sender ref.
                hew_reply_channel_publish_crash_fallback(channel);
                assert!(ready.take_ready());
                assert_eq!(
                    hew_reply_channel_poll_native(channel, 0, ptr::null_mut()),
                    AskError::HandlerTrapped as i32
                );
                hew_reply_channel_free(channel);
            }
        }
    }

    #[test]
    fn native_ask_receiver_trap_and_orphan_are_distinct_terminal_outcomes() {
        let (_ready, waker) = Readiness::new();
        // SAFETY: each branch retains exactly one sender reference for publication.
        unsafe {
            for orphan in [false, true] {
                let channel = hew_reply_channel_new_native(waker.descriptor(), None, None);
                hew_reply_channel_retain(channel);
                if orphan {
                    hew_reply_channel_retire_orphaned_ask_sender_ref(channel);
                } else {
                    hew_reply_channel_publish_crash_fallback(channel);
                }
                assert_eq!(
                    hew_reply_channel_poll_native(channel, 0, ptr::null_mut()),
                    if orphan {
                        AskError::OrphanedAsk as i32
                    } else {
                        AskError::HandlerTrapped as i32
                    }
                );
                hew_reply_channel_free(channel);
            }
        }
    }
}
