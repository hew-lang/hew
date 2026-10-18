//! Native capacity readiness without retaining an actor or its mailbox.

use crate::actor_native::HewSubmitStatus;
use crate::wake::{HewWaker, OwnedWaker};
use std::sync::Arc;

/// A capacity subscription is needed only after admission finds a full queue.
/// The direct descriptor already retains its target independently of the frame.
#[derive(Debug)]
enum CapacityWake {
    Direct(OwnedWaker),
    Registered { _registration: Arc<OwnedWaker> },
}

impl CapacityWake {
    fn poll(
        &mut self,
        token: crate::lifetime::local_handles::HewLocalPidId,
        mut admit: impl FnMut() -> super::SendOutcome,
    ) -> super::SendOutcome {
        let outcome = admit();
        if matches!(outcome, super::SendOutcome::Failed) && self.register(token) {
            admit()
        } else {
            outcome
        }
    }

    /// Register before rechecking capacity, so a dequeue in either interval
    /// is observed by the retry or by a retained readiness notification.
    fn register(&mut self, token: crate::lifetime::local_handles::HewLocalPidId) -> bool {
        let Self::Direct(waker) = self else {
            return false;
        };
        let registration = Arc::new(waker.clone());
        crate::actor::register_native_capacity(token, &registration);
        *self = Self::Registered {
            _registration: registration,
        };
        true
    }
}

/// The source message keeps its typed fields until admission succeeds. This
/// operation owns only the shallow unpublished wrapper and its readiness target.
#[derive(Debug)]
pub struct HewNativeSend {
    caller: crate::actor_caller::HewCallerStamp,
    obligated: bool,
    token: crate::lifetime::local_handles::HewLocalPidId,
    message: i32,
    envelope: *mut super::HewMsgEnvelope,
    wake: CapacityWake,
}

impl Drop for HewNativeSend {
    fn drop(&mut self) {
        if !self.envelope.is_null() {
            // SAFETY: an unpublished wrapper owns no fields independently of the
            // source message. Admission clears this slot before relinquishing it.
            unsafe {
                crate::mem::buf_free((*self.envelope).payload);
                crate::mem::buf_free(self.envelope.cast());
            }
        }
    }
}

/// Retain readiness without parking a worker or retaining an actor allocation.
/// A full poll registers capacity readiness and rechecks before returning pending.
///
/// # Safety
/// The payload is a unique malloc wrapper with shallow fields still owned by
/// the caller's message. The descriptor and destructor are valid for that type.
#[allow(
    clippy::too_many_arguments,
    reason = "the ABI carries payload ownership, readiness and request obligation"
)]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_send_wait_new(
    token: crate::lifetime::local_handles::HewLocalPidId,
    message: i32,
    payload: *mut std::ffi::c_void,
    size: usize,
    drop_payload: super::HewMsgEnvelopeDropFn,
    waker: *const HewWaker,
    payload_release: Option<hew_cabi::value::HewValueReleaseStart>,
    obligated: i32,
) -> *mut HewNativeSend {
    // SAFETY: the caller supplies a unique unpublished wrapper and live waker.
    let (envelope, waker) = unsafe {
        (
            super::hew_msg_envelope_new(payload, size, Some(drop_payload)),
            CapacityWake::Direct(OwnedWaker::retain(&*waker)),
        )
    };
    if envelope.is_null() {
        // SAFETY: allocation failed before any field ownership transferred.
        unsafe { crate::mem::buf_free(payload) };
        return std::ptr::null_mut();
    }
    // SAFETY: the unpublished envelope's fields still belong to its source.
    unsafe { (*envelope).release_start = payload_release };
    Box::into_raw(Box::new(HewNativeSend {
        caller: crate::actor_caller::local_sender(),
        obligated: obligated != 0,
        token,
        message,
        envelope,
        wake: waker,
    }))
}

/// A completion call waits for admission the same way a `.Wait` submission
/// does, but it carries the reply channel the handler answers on. Unlike a
/// one-way submission, the request wrapper owns the transferred fields, so an
/// abandoned admission releases them.
#[derive(Debug)]
pub struct HewNativeAsk {
    caller: crate::actor_caller::HewCallerStamp,
    token: crate::lifetime::local_handles::HewLocalPidId,
    message: i32,
    envelope: *mut super::HewMsgEnvelope,
    channel: *mut std::ffi::c_void,
    wake: CapacityWake,
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

/// Retain the readiness target for a completion call. A full admission poll
/// registers capacity readiness and rechecks before returning pending.
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
            crate::mem::buf_free(payload);
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
    // SAFETY: the unpublished request retains its channel until admission.
    if unsafe { crate::reply_channel::native_request_deadline(wait.channel.cast()) }.expired() {
        // SAFETY: expiry and the reply owner settle the same request.
        unsafe { crate::reply_channel::native::expire_native_request(wait.channel.cast()) };
        return AskError::Timeout as i32;
    }
    // SAFETY: this operation owns its unpublished request until admission.
    let outcome = wait.wake.poll(wait.token, || {
        // SAFETY: a failed attempt keeps both owners unpublished for the retry.
        unsafe {
            crate::actor::try_submit_native_request(
                wait.token,
                wait.message,
                wait.envelope,
                wait.channel,
                true,
                wait.caller,
            )
        }
    });
    match outcome {
        super::SendOutcome::Enqueued => {
            wait.envelope = std::ptr::null_mut();
            wait.channel = std::ptr::null_mut();
            AskError::None as i32
        }
        super::SendOutcome::Failed => -1,
        super::SendOutcome::Closed => AskError::ActorStopped as i32,
        super::SendOutcome::ShuttingDown => AskError::LocalShutdown as i32,
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
    // SAFETY: forwards the unpublished request and live readiness descriptor.
    unsafe {
        ask_wait_resume(
            token,
            message,
            envelope,
            channel,
            waker,
            crate::actor_caller::local_sender(),
        )
    }
}

pub(crate) unsafe fn ask_wait_resume(
    token: crate::lifetime::local_handles::HewLocalPidId,
    message: i32,
    envelope: *mut super::HewMsgEnvelope,
    channel: *mut std::ffi::c_void,
    waker: *const HewWaker,
    caller: crate::actor_caller::HewCallerStamp,
) -> *mut HewNativeAsk {
    if envelope.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: the completion call supplies live channel and waker references.
    let waker = unsafe {
        crate::reply_channel::hew_reply_channel_retain(channel.cast());
        CapacityWake::Direct(OwnedWaker::retain(&*waker))
    };
    Box::into_raw(Box::new(HewNativeAsk {
        caller,
        token,
        message,
        envelope,
        channel,
        wake: waker,
    }))
}

/// Answer a [`HewSubmitStatus`]: `Pending` while the mailbox stays full,
/// otherwise the terminal admission. Terminal failures preserve the caller's
/// typed fields; `Accepted` and `Discarded` both mean the destination owns them.
///
/// # Safety
/// The handle is null or uniquely borrowed until a terminal poll and release.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_send_wait_poll(wait: *mut HewNativeSend) -> i32 {
    // SAFETY: the caller exclusively drives this operation.
    let Some(wait) = (unsafe { wait.as_mut() }) else {
        return HewSubmitStatus::Oom as i32;
    };
    // SAFETY: this operation owns its unpublished envelope until admission.
    let outcome = wait.wake.poll(wait.token, || {
        // SAFETY: refusal leaves the shallow wrapper and source fields intact.
        unsafe {
            crate::actor::try_submit_native_request(
                wait.token,
                wait.message,
                wait.envelope,
                std::ptr::null_mut(),
                wait.obligated,
                wait.caller,
            )
        }
    });
    let status = match crate::actor_native::submission_status(outcome) {
        HewSubmitStatus::Full => HewSubmitStatus::Pending,
        status => status,
    };
    if matches!(
        status,
        HewSubmitStatus::Accepted | HewSubmitStatus::Discarded
    ) {
        wait.envelope = std::ptr::null_mut();
    }
    status as i32
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
    fn capacity_wait_rechecks_a_dequeue_or_close_before_registration() {
        for close in [false, true] {
            let (ready, waker) = Readiness::new();
            let mut wake = CapacityWake::Direct(waker);
            let mut attempts = 0;
            // SAFETY: the fixture exclusively owns the mailbox and envelope;
            // a failed attempt preserves the envelope for the second attempt.
            unsafe {
                let mailbox =
                    mailbox::hew_mailbox_new_with_policy(1, mailbox::OverflowPolicy::Fail);
                assert_eq!(mailbox::hew_mailbox_send(mailbox, 0, ptr::null_mut(), 0), 0);
                let envelope = super::super::hew_msg_envelope_new(ptr::null_mut(), 0, None);
                let outcome = wake.poll(
                    crate::lifetime::local_handles::HewLocalPidId::INVALID,
                    || {
                        attempts += 1;
                        let outcome = mailbox::try_admit_native_request(
                            &*mailbox,
                            1,
                            envelope,
                            ptr::null_mut(),
                            true,
                            crate::actor_caller::local_sender(),
                        );
                        if attempts == 1 {
                            assert!(matches!(outcome, mailbox::SendOutcome::Failed));
                            let filler = mailbox::hew_mailbox_try_recv(mailbox);
                            assert!(!filler.is_null());
                            mailbox::hew_msg_node_free(filler);
                            if close {
                                mailbox::mailbox_close(mailbox);
                            }
                        }
                        outcome
                    },
                );
                assert_eq!(
                    attempts, 2,
                    "the retry must observe a change before registration"
                );
                if close {
                    assert!(matches!(outcome, mailbox::SendOutcome::Closed));
                    super::super::hew_msg_envelope_release(envelope);
                } else {
                    assert!(matches!(outcome, mailbox::SendOutcome::Enqueued));
                    let node = mailbox::hew_mailbox_try_recv(mailbox);
                    assert!(!node.is_null());
                    mailbox::hew_msg_node_free(node);
                }
                mailbox::hew_mailbox_free(mailbox);
            }
            assert!(
                !ready.take_ready(),
                "the state change preceded registration"
            );
        }
    }

    #[test]
    fn native_ask_capacity_wait_preserves_admission_wakes_and_cancellation() {
        let _guard = crate::runtime_test_guard();
        let _scheduler = crate::scheduler::NoWorkerSchedulerForTest::install();
        for outcome in ["immediate", "dequeue", "close", "cancel"] {
            let (ready, waker) = Readiness::new();
            let drops = Arc::new(AtomicUsize::new(0));
            // SAFETY: the fixture owns the mailbox, routable actor and request.
            // Running prevents scheduler handoffs; the test dequeues explicitly.
            unsafe {
                let mailbox =
                    mailbox::hew_mailbox_new_with_policy(1, mailbox::OverflowPolicy::Fail);
                let mut stub = crate::test_actor::stub_actor();
                stub.mailbox = mailbox.cast();
                stub.actor_state.store(
                    crate::internal::types::HewActorState::Running as i32,
                    Ordering::Release,
                );
                let actor = crate::test_actor::TrackedTestActor::install(stub);
                let runtime = crate::runtime::rt_current();
                let token = runtime
                    .local_handles
                    .register_actor(crate::runtime_id::RuntimeId::DEFAULT, actor.id)
                    .unwrap();
                (*actor.ptr()).runtime = std::ptr::from_ref(runtime);
                (*actor.ptr()).local_pid_id = token;
                if outcome != "immediate" {
                    assert_eq!(mailbox::hew_mailbox_send(mailbox, 0, ptr::null_mut(), 0), 0);
                }
                let payload = crate::mem::buf_try_alloc(size_of::<*const AtomicUsize>())
                    .cast::<*const AtomicUsize>();
                payload.write(Arc::into_raw(drops.clone()));
                // Isolate capacity notifications from reply/orphan readiness.
                let channel = crate::reply_channel::hew_reply_channel_new();
                let wait = hew_actor_ask_wait_new(
                    token,
                    7,
                    payload.cast(),
                    size_of::<*const AtomicUsize>(),
                    drop_owned,
                    channel.cast(),
                    waker.descriptor(),
                );
                assert!(!ready.take_ready());
                if outcome == "immediate" {
                    assert_eq!(hew_actor_ask_wait_poll(wait), 0);
                    let node = mailbox::hew_mailbox_try_recv(mailbox);
                    assert!(!node.is_null());
                    mailbox::hew_msg_node_free(node);
                    assert!(
                        !ready.take_ready(),
                        "an admitted request needs no capacity wake"
                    );
                } else {
                    assert_eq!(hew_actor_ask_wait_poll(wait), -1);
                    if outcome == "dequeue" {
                        let filler = mailbox::hew_mailbox_try_recv(mailbox);
                        assert!(!filler.is_null());
                        mailbox::hew_msg_node_free(filler);
                        assert!(
                            ready.take_ready(),
                            "a pending request must observe released capacity"
                        );
                        assert_eq!(hew_actor_ask_wait_poll(wait), 0);
                    } else if outcome == "close" {
                        mailbox::mailbox_close(mailbox);
                        assert!(ready.take_ready(), "close must wake a pending request");
                        assert_eq!(
                            hew_actor_ask_wait_poll(wait),
                            crate::internal::types::AskError::ActorStopped as i32
                        );
                    }
                }
                hew_actor_ask_wait_free(wait);
                crate::reply_channel::hew_reply_channel_free(channel);
                mailbox::mailbox_close(mailbox);
                assert!(
                    !ready.take_ready(),
                    "released admission must detach its wake target"
                );
                mailbox::hew_mailbox_free(mailbox);
                drop(actor);
                assert_eq!(drops.load(Ordering::SeqCst), 1);
            }
            drop(waker);
            assert_eq!(Arc::strong_count(&ready), 1);
        }
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
                let payload =
                    crate::mem::buf_try_alloc(size_of_val(&source)).cast::<*const AtomicUsize>();
                payload.write(source);
                let channel = crate::reply_channel::native::hew_reply_channel_new_native(
                    waker.descriptor(),
                    None,
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
                let payload =
                    crate::mem::buf_try_alloc(size_of_val(&source)).cast::<*const AtomicUsize>();
                payload.write(source);
                let wait = hew_actor_send_wait_new(
                    crate::lifetime::local_handles::HewLocalPidId::INVALID,
                    0,
                    payload.cast(),
                    size_of_val(&source),
                    drop_owned,
                    waker.descriptor(),
                    None,
                    0,
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
