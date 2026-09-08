//! Native capacity readiness without retaining an actor or its mailbox.

use crate::wake::{HewWaker, OwnedWaker};
use std::sync::Arc;

/// The source message keeps its typed fields until admission succeeds. This
/// operation owns only the shallow unpublished wrapper and its readiness target.
#[derive(Debug)]
pub struct HewNativeSend {
    token: crate::lifetime::local_handles::HewLocalPidId,
    message: i32,
    envelope: *mut super::HewMsgEnvelope,
    _waker: Arc<OwnedWaker>,
}

impl Drop for HewNativeSend {
    fn drop(&mut self) {
        if !self.envelope.is_null() {
            // SAFETY: an unpublished wrapper owns no fields independently of the
            // source message. Admission clears this slot before relinquishing it.
            unsafe {
                libc::free((*self.envelope).payload);
                libc::free(self.envelope.cast());
            }
        }
    }
}

/// Register readiness before the first capacity check, closing the missed-wake
/// window without parking an actor worker or retaining an actor allocation.
///
/// # Safety
/// The payload is a unique malloc wrapper with shallow fields still owned by
/// the caller's message. The descriptor and destructor are valid for that type.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_send_wait_new(
    token: crate::lifetime::local_handles::HewLocalPidId,
    message: i32,
    payload: *mut std::ffi::c_void,
    size: usize,
    drop_payload: super::HewMsgEnvelopeDropFn,
    waker: *const HewWaker,
) -> *mut HewNativeSend {
    // SAFETY: the caller supplies a unique unpublished wrapper and live waker.
    let (envelope, waker) = unsafe {
        (
            super::hew_msg_envelope_new(payload, size, Some(drop_payload)),
            Arc::new(OwnedWaker::retain(&*waker)),
        )
    };
    if envelope.is_null() {
        // SAFETY: allocation failed before any field ownership transferred.
        unsafe { libc::free(payload) };
        return std::ptr::null_mut();
    }
    crate::actor::register_native_capacity(token, &waker);
    Box::into_raw(Box::new(HewNativeSend {
        token,
        message,
        envelope,
        _waker: waker,
    }))
}

/// A completion call waits for admission the same way a `.Wait` submission
/// does, but it carries the reply channel the handler answers on. Unlike a
/// one-way submission, the request wrapper owns the transferred fields, so an
/// abandoned admission releases them.
#[derive(Debug)]
pub struct HewNativeAsk {
    token: crate::lifetime::local_handles::HewLocalPidId,
    message: i32,
    envelope: *mut super::HewMsgEnvelope,
    channel: *mut std::ffi::c_void,
    _waker: Arc<OwnedWaker>,
}

impl Drop for HewNativeAsk {
    fn drop(&mut self) {
        if !self.envelope.is_null() {
            // SAFETY: an unadmitted request owns its transferred fields and one
            // reply-channel reference. Admission clears both slots first.
            unsafe {
                super::hew_msg_envelope_release(self.envelope);
                crate::reply_channel::hew_reply_channel_free(self.channel.cast());
            }
        }
    }
}

/// Register readiness before the first admission attempt of a completion call.
///
/// # Safety
/// The payload is a unique malloc wrapper whose fields have transferred to this
/// request. The channel, waker and destructor are valid for that request.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_ask_wait_new(
    token: crate::lifetime::local_handles::HewLocalPidId,
    message: i32,
    payload: *mut std::ffi::c_void,
    size: usize,
    drop_payload: super::HewMsgEnvelopeDropFn,
    channel: *mut std::ffi::c_void,
    waker: *const HewWaker,
) -> *mut HewNativeAsk {
    // SAFETY: the caller supplies a unique unpublished wrapper and live waker.
    let envelope = unsafe { super::hew_msg_envelope_new(payload, size, Some(drop_payload)) };
    if envelope.is_null() {
        // SAFETY: allocation failed; the wrapper still owns the typed fields.
        unsafe {
            drop_payload(payload);
            libc::free(payload);
        }
        return std::ptr::null_mut();
    }
    // SAFETY: the fresh envelope owns the request and all references are live.
    unsafe { hew_actor_ask_wait_resume(token, message, envelope, channel, waker) }
}

/// Return -1 while full, 0 after admitting the request, or the `AskError` code
/// of a terminal admission failure.
///
/// # Safety
/// The handle is null or uniquely borrowed until a terminal poll and release.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_ask_wait_poll(wait: *mut HewNativeAsk) -> i32 {
    use crate::internal::types::AskError;
    // SAFETY: the caller exclusively drives this operation.
    let Some(wait) = (unsafe { wait.as_mut() }) else {
        return AskError::SendFailed as i32;
    };
    // SAFETY: this operation owns its unpublished request until admission.
    let outcome = unsafe {
        crate::actor::try_submit_native_request(
            wait.token,
            wait.message,
            wait.envelope,
            wait.channel,
        )
    };
    match outcome {
        super::SendOutcome::Enqueued => {
            wait.envelope = std::ptr::null_mut();
            wait.channel = std::ptr::null_mut();
            AskError::None as i32
        }
        super::SendOutcome::Failed => -1,
        super::SendOutcome::Closed => AskError::ActorStopped as i32,
        super::SendOutcome::Oom => AskError::SendFailed as i32,
        _ => unreachable!("native admission does not select an overflow policy"),
    }
}

/// Release readiness, an unadmitted request and its reply-channel reference.
///
/// # Safety
/// `wait` is null or the unique owner returned by `hew_actor_ask_wait_new`.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_ask_wait_free(wait: *mut HewNativeAsk) {
    if !wait.is_null() {
        // SAFETY: the caller relinquishes its unique handle.
        drop(unsafe { Box::from_raw(wait) });
    }
}

/// Detach an unadmitted request without copying or destroying its payload.
/// The returned envelope owns the original wrapper and its typed destructor.
///
/// # Safety
/// `wait` is null or uniquely borrowed before admission. After detaching its
/// request, the caller must release the wait without polling it again.
/// The caller must release or resubmit the returned envelope exactly once.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_ask_wait_take_request(
    wait: *mut HewNativeAsk,
) -> *mut super::HewMsgEnvelope {
    // SAFETY: the caller exclusively owns this admission operation.
    let Some(wait) = (unsafe { wait.as_mut() }) else {
        return std::ptr::null_mut();
    };
    let envelope = std::mem::replace(&mut wait.envelope, std::ptr::null_mut());
    if !envelope.is_null() {
        // SAFETY: the unadmitted request owns one channel reference. A later
        // resubmission creates its own channel; the old reply cannot escape.
        unsafe { crate::reply_channel::hew_reply_channel_free(wait.channel.cast()) };
        wait.channel = std::ptr::null_mut();
    }
    envelope
}

/// Resume admission using the original sealed request, without repacking it.
///
/// # Safety
/// `envelope` uniquely owns an unadmitted request and its typed destructor.
/// The checked target and message have the same request and reply protocol.
/// The channel and waker are valid for the new completion call.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_ask_wait_resume(
    token: crate::lifetime::local_handles::HewLocalPidId,
    message: i32,
    envelope: *mut super::HewMsgEnvelope,
    channel: *mut std::ffi::c_void,
    waker: *const HewWaker,
) -> *mut HewNativeAsk {
    if envelope.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: the completion call supplies live channel and waker references.
    let waker = unsafe {
        crate::reply_channel::hew_reply_channel_retain(channel.cast());
        Arc::new(OwnedWaker::retain(&*waker))
    };
    crate::actor::register_native_capacity(token, &waker);
    Box::into_raw(Box::new(HewNativeAsk {
        token,
        message,
        envelope,
        channel,
        _waker: waker,
    }))
}

/// Return -1 while full, 0 after transferring the message, 2 when closed, or 3
/// on allocation failure. Terminal failures preserve the caller's typed fields.
///
/// # Safety
/// The handle is null or uniquely borrowed until a terminal poll and release.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_send_wait_poll(wait: *mut HewNativeSend) -> i32 {
    // SAFETY: the caller exclusively drives this operation.
    let Some(wait) = (unsafe { wait.as_mut() }) else {
        return 3;
    };
    // SAFETY: this operation owns its unpublished envelope until admission.
    let outcome = unsafe {
        crate::actor::try_submit_native_envelope(wait.token, wait.message, wait.envelope)
    };
    match outcome {
        super::SendOutcome::Enqueued => {
            wait.envelope = std::ptr::null_mut();
            0
        }
        super::SendOutcome::Failed => -1,
        super::SendOutcome::Closed => 2,
        super::SendOutcome::Oom => 3,
        _ => unreachable!("native admission does not select an overflow policy"),
    }
}

/// Release readiness and any unpublished shallow wrapper.
///
/// # Safety
/// `wait` is null or the unique owner returned by `hew_actor_send_wait_new`.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_send_wait_free(wait: *mut HewNativeSend) {
    if !wait.is_null() {
        // SAFETY: the caller relinquishes its unique handle.
        drop(unsafe { Box::from_raw(wait) });
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::mailbox;
    use crate::wake::blocking::Readiness;
    use std::ptr;
    use std::sync::atomic::{AtomicUsize, Ordering};

    #[test]
    fn native_capacity_dequeue_and_close_wake_live_registrations() {
        for policy in [
            mailbox::OverflowPolicy::Fail,
            mailbox::OverflowPolicy::Block,
        ] {
            let (ready, waker) = Readiness::new();
            let registration = Arc::new(waker);
            // SAFETY: the fixture owns its mailbox and each removed node.
            unsafe {
                let mailbox = mailbox::hew_mailbox_new_with_policy(1, policy);
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
                (*mailbox).native_capacity.register(&registration);
                assert!(!ready.take_ready());
                let node = mailbox::hew_mailbox_try_recv(mailbox);
                assert!(!node.is_null());
                mailbox::hew_msg_node_free(node);
                assert!(
                    ready.take_ready(),
                    "capacity release must notify a registered sender"
                );
                mailbox::mailbox_close(mailbox);
                assert!(
                    ready.take_ready(),
                    "close must wake a sender without admitting its payload"
                );
                drop(registration);
                assert_eq!(Arc::strong_count(&ready), 1);
                (*mailbox).native_capacity.notify();
                assert!(
                    !ready.take_ready(),
                    "cancelled registrations must not retain a wake target"
                );
                mailbox::hew_mailbox_free(mailbox);
            }
        }
    }

    unsafe extern "C" fn drop_owned(payload: *mut std::ffi::c_void) {
        // SAFETY: the wrapper contains one raw Arc reference owned by its source.
        let count = unsafe { Arc::from_raw(*payload.cast::<*const AtomicUsize>()) };
        count.fetch_add(1, Ordering::SeqCst);
    }

    #[test]
    fn sealed_request_detach_resume_and_drop_preserve_one_owner() {
        for resubmit in [false, true] {
            let (ready, waker) = Readiness::new();
            let drops = Arc::new(AtomicUsize::new(0));
            let source = Arc::into_raw(drops.clone());
            // SAFETY: each operation receives the unique request envelope;
            // channel references remain live until their owners release them.
            unsafe {
                let payload = libc::malloc(size_of_val(&source)).cast::<*const AtomicUsize>();
                payload.write(source);
                let channel = crate::reply_channel::native::hew_reply_channel_new_native(
                    waker.descriptor(),
                    None,
                );
                let wait = hew_actor_ask_wait_new(
                    crate::lifetime::local_handles::HewLocalPidId::INVALID,
                    7,
                    payload.cast(),
                    size_of_val(&source),
                    drop_owned,
                    channel.cast(),
                    waker.descriptor(),
                );
                let request = hew_actor_ask_wait_take_request(wait);
                assert!(!request.is_null());
                assert!(hew_actor_ask_wait_take_request(wait).is_null());
                hew_actor_ask_wait_free(wait);
                crate::reply_channel::hew_reply_channel_free(channel);
                assert_eq!(drops.load(Ordering::SeqCst), 0);
                assert_eq!(
                    super::super::hew_msg_envelope_payload_ptr(request),
                    payload.cast()
                );
                if resubmit {
                    let channel = crate::reply_channel::native::hew_reply_channel_new_native(
                        waker.descriptor(),
                        None,
                    );
                    let wait = hew_actor_ask_wait_resume(
                        crate::lifetime::local_handles::HewLocalPidId::INVALID,
                        7,
                        request,
                        channel.cast(),
                        waker.descriptor(),
                    );
                    assert_eq!(
                        hew_actor_ask_wait_poll(wait),
                        crate::internal::types::AskError::ActorStopped as i32
                    );
                    hew_actor_ask_wait_free(wait);
                    crate::reply_channel::hew_reply_channel_free(channel);
                } else {
                    super::super::hew_msg_envelope_release(request);
                }
                assert_eq!(drops.load(Ordering::SeqCst), 1);
                assert_eq!(Arc::strong_count(&drops), 1);
            }
            drop(waker);
            assert_eq!(Arc::strong_count(&ready), 1);
        }
    }

    #[test]
    fn native_wait_rejection_and_cancellation_preserve_source_fields() {
        for poll in [false, true] {
            let (ready, waker) = Readiness::new();
            let drops = Arc::new(AtomicUsize::new(0));
            let mut source = Arc::into_raw(drops.clone());
            // SAFETY: the unpublished wrapper shallowly aliases the source's Arc.
            unsafe {
                let payload = libc::malloc(size_of_val(&source)).cast::<*const AtomicUsize>();
                payload.write(source);
                let wait = hew_actor_send_wait_new(
                    crate::lifetime::local_handles::HewLocalPidId::INVALID,
                    0,
                    payload.cast(),
                    size_of_val(&source),
                    drop_owned,
                    waker.descriptor(),
                );
                drop(waker);
                if poll {
                    assert_eq!(hew_actor_send_wait_poll(wait), 2);
                }
                hew_actor_send_wait_free(wait);
                assert_eq!(drops.load(Ordering::SeqCst), 0);
                assert_eq!(Arc::strong_count(&ready), 1);
                assert!(!ready.take_ready());
                drop_owned((&raw mut source).cast());
                assert_eq!(drops.load(Ordering::SeqCst), 1);
                hew_actor_send_wait_free(ptr::null_mut());
            }
        }
    }
}
