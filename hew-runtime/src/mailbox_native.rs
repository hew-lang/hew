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
