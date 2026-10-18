//! One immutable request contract and its atomic dispatch claim.
//! An envelope and optional reply owner retain the same state. Reply payloads
//! remain independently owned by the reply channel.

use crate::internal::types::AskError;
use crate::request_deadline::RequestDeadline;
use std::sync::atomic::{AtomicU8, Ordering};

/// The request's admission policy and completion obligation, carried explicitly
/// across the authenticated wire boundary.
#[repr(u8)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RequestKind {
    CallWait = 0,
    CallReject = 1,
    SendWait = 2,
    SendReject = 3,
    SendDropNewest = 4,
}

impl RequestKind {
    pub(crate) fn from_wire(value: u8) -> Option<Self> {
        match value {
            0 => Some(Self::CallWait),
            1 => Some(Self::CallReject),
            2 => Some(Self::SendWait),
            3 => Some(Self::SendReject),
            4 => Some(Self::SendDropNewest),
            _ => None,
        }
    }

    pub(crate) const fn completion_policy(self) -> Option<i32> {
        match self {
            Self::CallWait => Some(0),
            Self::CallReject => Some(1),
            Self::SendWait | Self::SendReject | Self::SendDropNewest => None,
        }
    }
}

/// A request's result preserves proof of refusal separately from an error
/// received after admission or while the request's fate is unknown.
#[derive(Clone, Copy, Debug)]
pub(crate) enum RequestOutcome {
    Completed(i32),
    Refused(AskError),
    Submitted { discarded: bool },
}

impl RequestOutcome {
    pub(crate) const fn code(self) -> i32 {
        match self {
            Self::Completed(code) => code,
            Self::Refused(error) => error as i32,
            Self::Submitted { .. } => 0,
        }
    }

    pub(crate) fn refused(code: i32) -> Self {
        AskError::ALL
            .iter()
            .copied()
            .find(|error| *error as i32 == code)
            .map_or(Self::Completed(code), Self::Refused)
    }
}

#[derive(Debug)]
pub(crate) struct RequestState {
    // 1: withdrawable; 2: dispatched; 3: withdrawn.
    dispatch: AtomicU8,
    observer: Option<crate::wake::OwnedWaker>,
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
            observer: None,
            deadline,
            #[cfg(not(target_arch = "wasm32"))]
            admission,
        }
    }

    /// Install the queued-request observer before sharing this state.
    pub(crate) fn with_observer(mut self, observer: crate::wake::OwnedWaker) -> Self {
        self.observer = Some(observer);
        self
    }

    pub(crate) fn terminal(&self) -> bool {
        self.dispatch.load(Ordering::Acquire) != 1
    }

    fn notify(&self) {
        if let Some(observer) = &self.observer {
            observer.wake();
        }
    }

    pub(crate) fn withdraw(&self) {
        if self
            .dispatch
            .compare_exchange(1, 3, Ordering::AcqRel, Ordering::Acquire)
            .is_ok()
        {
            self.notify();
        }
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn has_authenticated_admission(&self) -> bool {
        self.admission.is_some()
    }

    fn claim(&self) -> Result<bool, AskError> {
        let claim = || {
            self.dispatch
                .compare_exchange(1, 2, Ordering::AcqRel, Ordering::Acquire)
        };
        #[cfg(not(target_arch = "wasm32"))]
        let claimed = if let Some(admission) = &self.admission {
            // The CAS stays inside the export authority's lock; readiness is
            // notified only after that authority has released its lock.
            admission.claim(claim)?
        } else {
            claim()
        };
        #[cfg(target_arch = "wasm32")]
        let claimed = claim();
        if claimed.is_ok() {
            self.notify();
        }
        Ok(claimed != Err(3))
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
            // SAFETY: the caller retains the reply channel throughout expiry.
            // Preserve reply-versus-expiry ordering under the existing reply lock.
            unsafe { crate::reply_channel::native::expire_native_request(channel) };
        }
    }
    match state.claim() {
        Ok(claimed) => claimed,
        Err(reason) => {
            state.withdraw();
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
