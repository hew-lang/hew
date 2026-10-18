//! One immutable request contract and its atomic dispatch claim.
//! An envelope and optional reply owner retain the same state. Reply payloads
//! remain independently owned by the reply channel.

use crate::internal::types::AskError;
use crate::request_deadline::RequestDeadline;
use std::sync::atomic::{AtomicU8, Ordering};

#[derive(Debug)]
pub(crate) struct RequestState {
    // 1: withdrawable; 2: dispatched; 3: withdrawn.
    dispatch: AtomicU8,
    pub(crate) deadline: RequestDeadline,
    #[cfg(not(target_arch = "wasm32"))]
    admission: Option<crate::hew_node::service::RemoteAdmission>,
}

impl RequestState {
    pub(crate) fn new(
        deadline: RequestDeadline,
        #[cfg(not(target_arch = "wasm32"))] admission: Option<
            crate::hew_node::service::RemoteAdmission,
        >,
    ) -> Self {
        Self {
            dispatch: AtomicU8::new(1),
            deadline,
            #[cfg(not(target_arch = "wasm32"))]
            admission,
        }
    }

    pub(crate) fn withdraw(&self) {
        let _ = self
            .dispatch
            .compare_exchange(1, 3, Ordering::AcqRel, Ordering::Acquire);
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn has_authenticated_admission(&self) -> bool {
        self.admission.is_some()
    }

    fn claim(&self) -> Result<bool, AskError> {
        let claim = || {
            self.dispatch
                .compare_exchange(1, 2, Ordering::AcqRel, Ordering::Acquire)
                != Err(3)
        };
        #[cfg(not(target_arch = "wasm32"))]
        if let Some(admission) = &self.admission {
            // Keep the dispatch CAS inside the export authority's lock so
            // grant revocation and handler claim remain linearly ordered.
            return admission.claim(claim);
        }
        Ok(claim())
    }
}

/// Attach the optional reply owner's exact state at an ingress adapter.
/// # Safety
/// The unpublished envelope is exclusive and the optional channel is retained.
pub(crate) unsafe fn bind_reply(
    envelope: *mut crate::mailbox::HewMsgEnvelope,
    channel: *const crate::reply_channel::HewReplyChannel,
) {
    // SAFETY: the ingress adapter retains both owners before publication.
    let state = unsafe { crate::reply_channel::native_request_state(channel) };
    let Some(state) = state else {
        return;
    };
    // SAFETY: the caller exclusively owns the unpublished envelope.
    let envelope = unsafe { &mut *envelope };
    if let Some(existing) = &envelope.request {
        assert!(
            std::sync::Arc::ptr_eq(existing, &state),
            "request owners disagree before publication"
        );
    } else {
        envelope.request = Some(state);
    }
}

/// Claim a dequeued envelope, settling its optional reply on refusal.
/// # Safety
/// The worker owns the envelope and retains its optional sender-side reply.
pub(crate) unsafe fn claim_message(
    envelope: *const crate::mailbox::HewMsgEnvelope,
    channel: *mut crate::reply_channel::HewReplyChannel,
) -> bool {
    // SAFETY: the worker retains the dequeued envelope throughout this borrow.
    let Some(state) = (unsafe { envelope.as_ref() }).and_then(|envelope| envelope.request.as_ref())
    else {
        return true;
    };
    if state.deadline.expired() {
        if channel.is_null() {
            state.withdraw();
        } else {
            // Preserve reply-versus-expiry ordering under the existing reply lock.
            unsafe { crate::reply_channel::native::expire_native_request(channel) };
        }
    }
    match state.claim() {
        Ok(claimed) => claimed,
        Err(reason) => {
            // SAFETY: a null reply is allowed; otherwise the node retains it.
            unsafe { crate::reply_channel::native::reject_remote_request(channel, reason) };
            false
        }
    }
}

/// Read the envelope's immutable inherited budget without a reply-channel loan.
/// # Safety
/// The envelope is null or retained through the read.
pub(crate) unsafe fn message_deadline(
    envelope: *const crate::mailbox::HewMsgEnvelope,
) -> RequestDeadline {
    // SAFETY: the caller retains the optional envelope.
    unsafe { envelope.as_ref() }
        .and_then(|envelope| envelope.request.as_ref())
        .map_or_else(RequestDeadline::default, |state| state.deadline)
}
