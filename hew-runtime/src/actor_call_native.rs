//! One owned completion call, from admission through non-consuming readiness to
//! the selected reply or rejected request. Ordinary calls and select arms use
//! this same operation; a borrowed task observation is a different lifetime.

use crate::coro_sleep::{
    hew_coro_sleep_free, hew_coro_sleep_status, hew_coro_sleep_until_new, HewCoroSleep,
};
use crate::internal::types::AskError;
use crate::lifetime::live_actors::ActorIncarnation;
use crate::lifetime::local_handles::HewLocalPidId;
use crate::mailbox::native::{
    hew_actor_ask_wait_free, hew_actor_ask_wait_take_request, poll_request_admission,
    request_wait_resume, HewNativeAsk,
};
use crate::mailbox::{hew_msg_envelope_new, HewMsgEnvelope, HewMsgEnvelopeDropFn};
use crate::release_walker::{HewReleaseCursor, ReleaseDriver};
use crate::reply_channel::native::{
    hew_reply_channel_new_native, hew_reply_channel_poll_native, hew_reply_channel_status_native,
};
use crate::reply_channel::{
    hew_reply_channel_cancel, hew_reply_channel_free, HewReplyChannel, HewReplyDropFn,
};
use crate::request_state::{RequestKind, RequestOutcome, RequestState};
use crate::wake::HewWaker;
use hew_cabi::value::HewValueReleaseStart;
use std::sync::Arc;
use std::{ffi::c_void, ptr};

#[derive(Debug)]
enum CallState {
    Admitting,
    Waiting,
    Ready(RequestOutcome),
    RemoteReady,
    Taken,
}

#[derive(Debug)]
struct LocalCall {
    admission: *mut HewNativeAsk,
    channel: *mut HewReplyChannel,
    timer: *mut HewCoroSleep,
    request: Arc<RequestState>,
}

#[derive(Debug)]
enum CallRoute {
    Local(LocalCall),
    #[cfg(not(target_arch = "wasm32"))]
    Imported {
        operation: *mut crate::hew_node::HewRemoteCall,
        request: *mut HewMsgEnvelope,
    },
}

/// The request and eventual result remain owned here until an explicit take.
/// Its drop withdraws an undispatched request or tombstones a dispatched reply.
#[derive(Debug)]
pub struct HewActorCall {
    route: CallRoute,
    owned_target: Option<HewLocalPidId>,
    expected_size: usize,
    kind: RequestKind,
    state: CallState,
    pub(crate) target: Option<ActorIncarnation>,
    cleanup_started: bool,
    cleanup: Option<Box<ReleaseDriver>>,
    rejected_payload: *mut HewReleaseCursor,
}

impl HewActorCall {
    unsafe fn prepare_cleanup(&mut self) {
        if self.cleanup_started {
            return;
        }
        self.cleanup_started = true;
        // SAFETY: this operation owns both receiving and unpublished request slots.
        unsafe {
            let (request, reply) = match &mut self.route {
                CallRoute::Local(local) => {
                    if !matches!(self.state, CallState::Taken)
                        || self.kind.completion_policy().is_some()
                    {
                        local.request.withdraw();
                    }
                    let reply = crate::reply_channel::native::cancel_take_release(local.channel);
                    (hew_actor_ask_wait_take_request(local.admission), reply)
                }
                #[cfg(not(target_arch = "wasm32"))]
                CallRoute::Imported { operation, request } => {
                    crate::hew_node::hew_remote_call_free(std::mem::replace(
                        operation,
                        ptr::null_mut(),
                    ));
                    (std::mem::replace(request, ptr::null_mut()), ptr::null_mut())
                }
            };
            let request = crate::cow_envelope::release_cursor(request);
            let cursor = HewReleaseCursor::join([self.rejected_payload, request, reply]);
            self.rejected_payload = ptr::null_mut();
            if !cursor.is_null() {
                self.cleanup = Some(ReleaseDriver::new(cursor));
            }
        }
    }

    pub(crate) fn has_deadline(&self) -> bool {
        match &self.route {
            CallRoute::Local(local) => !local.timer.is_null(),
            #[cfg(not(target_arch = "wasm32"))]
            CallRoute::Imported { operation, .. } => {
                // SAFETY: this call owns its optional live remote operation.
                unsafe { operation.as_ref() }
                    .is_some_and(crate::hew_node::HewRemoteCall::has_deadline)
            }
        }
    }

    /// Expiry prevents fresh admission; an already published reply retains its
    /// terminal outcome even when the caller resumes after the deadline.
    unsafe fn poll(&mut self) -> i32 {
        use crate::mailbox::SendOutcome;
        #[cfg(not(target_arch = "wasm32"))]
        if let CallRoute::Imported { operation, .. } = &self.route {
            return match self.state {
                CallState::Ready(outcome) => outcome.code(),
                CallState::RemoteReady => 0,
                CallState::Taken => -2,
                CallState::Admitting | CallState::Waiting => {
                    // SAFETY: this call exclusively drives the imported operation.
                    let status = unsafe { crate::hew_node::hew_remote_call_poll(*operation) };
                    if status == 0 {
                        self.state = CallState::RemoteReady;
                    }
                    status
                }
            };
        }
        match self.state {
            CallState::Ready(outcome) => return outcome.code(),
            CallState::RemoteReady => return 0,
            CallState::Taken => return -2,
            CallState::Admitting => {
                let deadline = self.poll_deadline();
                if deadline != -1 {
                    return deadline;
                }
                // SAFETY: this operation exclusively owns the admission.
                let result = unsafe { self.local().admission.as_mut() }
                    .map_or(Err(AskError::SendFailed), |admission| unsafe {
                        poll_request_admission(admission)
                    });
                let discarded = match result {
                    Ok(SendOutcome::Failed) => match self.kind {
                        RequestKind::CallWait | RequestKind::SendWait => {
                            return self.poll_deadline()
                        }
                        RequestKind::CallReject | RequestKind::SendReject => {
                            self.state =
                                CallState::Ready(RequestOutcome::Refused(AskError::MailboxFull));
                            return AskError::MailboxFull as i32;
                        }
                        RequestKind::SendDropNewest => true,
                    },
                    Ok(
                        SendOutcome::Enqueued | SendOutcome::Coalesced | SendOutcome::DroppedOld,
                    ) => false,
                    Ok(SendOutcome::Dropped) => true,
                    other => {
                        let error = match other {
                            Err(error) => error,
                            Ok(SendOutcome::Closed) => AskError::ActorStopped,
                            Ok(SendOutcome::ShuttingDown) => AskError::LocalShutdown,
                            _ => AskError::SendFailed,
                        };
                        self.state = CallState::Ready(RequestOutcome::Refused(error));
                        return error as i32;
                    }
                };
                if self.kind.completion_policy().is_none() {
                    self.state = CallState::Ready(RequestOutcome::Submitted { discarded });
                    return 0;
                }
                // SAFETY: admission transferred its request and sender reference.
                unsafe { hew_actor_ask_wait_free(self.local().admission) };
                self.local().admission = ptr::null_mut();
                self.state = CallState::Waiting;
            }
            CallState::Waiting => {}
        }
        // SAFETY: this operation owns the only receiving reference and knows
        // the exact reply size. Readiness does not take any payload.
        let channel = self.local().channel;
        // SAFETY: this operation owns the live channel with its checked reply size.
        let status = unsafe { hew_reply_channel_status_native(channel, self.expected_size) };
        if status != -1 {
            self.state = CallState::Ready(RequestOutcome::Completed(status));
            return status;
        }
        self.poll_deadline()
    }

    fn poll_deadline(&mut self) -> i32 {
        let unadmitted = matches!(self.state, CallState::Admitting);
        let local = self.local();
        let (timer, channel, deadline) = (local.timer, local.channel, local.request.deadline);
        if timer.is_null() {
            return -1;
        }
        // SAFETY: the operation uniquely owns its optional timer.
        let status = unsafe { hew_coro_sleep_status(timer) };
        // SAFETY: this operation retains the immutable channel metadata.
        let expired = deadline.expired();
        if status == 0 && !expired {
            return -1;
        }
        if status > 1 {
            self.state = CallState::Ready(RequestOutcome::Completed(-2));
            return -2;
        }
        if channel.is_null() {
            self.local().request.withdraw();
            self.state = CallState::Ready(RequestOutcome::Refused(AskError::Timeout));
            return AskError::Timeout as i32;
        }
        // SAFETY: expiry shares the reply publication lock, preserving a reply
        // that already won and withdrawing only an unclaimed request.
        unsafe { crate::reply_channel::native::expire_native_request(channel) };
        // SAFETY: the operation still owns its receiver and exact reply layout.
        let result = unsafe { hew_reply_channel_status_native(channel, self.expected_size) };
        if result != -1 {
            self.state = CallState::Ready(if unadmitted {
                RequestOutcome::refused(result)
            } else {
                RequestOutcome::Completed(result)
            });
        }
        result
    }

    /// Move the queued request's timer to its inbound publication owner.
    pub(crate) fn take_queued(&mut self) -> (Arc<RequestState>, *mut HewCoroSleep) {
        assert!(matches!(self.state, CallState::Taken));
        let local = self.local();
        (
            Arc::clone(&local.request),
            std::mem::replace(&mut local.timer, ptr::null_mut()),
        )
    }

    pub(crate) fn unaccepted(&self) -> bool {
        matches!(self.state, CallState::Ready(RequestOutcome::Refused(_)))
    }

    /// Take a ready result without discarding its admission evidence.
    /// # Safety
    /// This operation is ready and unique; output holds the completion reply.
    pub(crate) unsafe fn take_outcome(&mut self, output: *mut c_void) -> RequestOutcome {
        let outcome = match self.state {
            CallState::Ready(outcome) => {
                if let RequestOutcome::Completed(0) = outcome {
                    let CallRoute::Local(local) = &self.route else {
                        unreachable!("a local ready reply has a local route");
                    };
                    // SAFETY: readiness leaves the exact reply owned by this call.
                    let taken = unsafe {
                        hew_reply_channel_poll_native(local.channel, self.expected_size, output)
                    };
                    debug_assert_eq!(taken, 0, "ready reply changes before its unique take");
                }
                outcome
            }
            #[cfg(not(target_arch = "wasm32"))]
            CallState::RemoteReady => {
                let CallRoute::Imported { operation, .. } = &self.route else {
                    unreachable!("a remote ready reply has an imported route");
                };
                // SAFETY: this imported operation and output belong to the caller.
                unsafe { crate::hew_node::take_imported_request(*operation, output) }
            }
            _ => return RequestOutcome::Completed(-2),
        };
        self.state = CallState::Taken;
        outcome
    }

    unsafe fn detach_request(&mut self) -> *mut HewMsgEnvelope {
        match &mut self.route {
            // SAFETY: this operation exclusively owns the unaccepted request.
            CallRoute::Local(local) => unsafe { hew_actor_ask_wait_take_request(local.admission) },
            #[cfg(not(target_arch = "wasm32"))]
            CallRoute::Imported { request, .. } => std::mem::replace(request, ptr::null_mut()),
        }
    }

    fn local(&mut self) -> &mut LocalCall {
        match &mut self.route {
            CallRoute::Local(local) => local,
            #[cfg(not(target_arch = "wasm32"))]
            CallRoute::Imported { .. } => unreachable!("local admission requires a local route"),
        }
    }
}

impl Drop for HewActorCall {
    fn drop(&mut self) {
        // SAFETY: all three operations belong to this call. Cancellation wins
        // against dispatch before any sender reference can be released.
        unsafe {
            self.prepare_cleanup();
            if let Some(target) = self.owned_target.take() {
                crate::lifetime::local_handles::hew_actor_handle_release(target);
            }
            match &self.route {
                CallRoute::Local(local) => {
                    hew_reply_channel_cancel(local.channel);
                    hew_actor_ask_wait_free(local.admission);
                    hew_coro_sleep_free(local.timer);
                    hew_reply_channel_free(local.channel);
                }
                #[cfg(not(target_arch = "wasm32"))]
                CallRoute::Imported { .. } => {}
            }
        }
    }
}

/// Transfer fresh request fields and start admission without parking the caller.
/// `policy` is zero for Wait and one for Reject. `deadline_ns` is a local
/// monotonic expiry, or -1 for no request deadline.
///
/// # Safety
/// `caller` points to a live stamp copied before this function returns.
/// `payload` is null or an owned malloc wrapper with matching size/destructor.
/// The reply descriptor and retained waker obey their native contracts.
#[no_mangle]
#[allow(
    clippy::too_many_arguments,
    reason = "compiler-private typed operation ABI"
)]
pub unsafe extern "C" fn hew_actor_call_new(
    token: HewLocalPidId,
    message: i32,
    payload: *mut c_void,
    size: usize,
    drop_payload: HewMsgEnvelopeDropFn,
    reply_size: usize,
    drop_reply: Option<HewReplyDropFn>,
    waker: *const HewWaker,
    deadline_ns: i64,
    policy: i32,
    payload_release: Option<HewValueReleaseStart>,
    reply_release: Option<HewValueReleaseStart>,
    caller: *const crate::actor_caller::HewCallerStamp,
    dispatch: *const c_void,
) -> *mut HewActorCall {
    // SAFETY: forwards the owned request and captures its local sender.
    unsafe {
        actor_call_new(
            token,
            message,
            payload,
            size,
            drop_payload,
            reply_size,
            drop_reply,
            waker,
            deadline_ns,
            if policy == 0 {
                RequestKind::CallWait
            } else {
                RequestKind::CallReject
            },
            payload_release,
            reply_release,
            *caller,
            dispatch,
            #[cfg(not(target_arch = "wasm32"))]
            None,
        )
    }
}

#[allow(
    clippy::too_many_arguments,
    reason = "the checked request and reply carry one complete admission contract"
)]
pub(crate) unsafe fn actor_call_new(
    token: HewLocalPidId,
    message: i32,
    payload: *mut c_void,
    size: usize,
    drop_payload: HewMsgEnvelopeDropFn,
    reply_size: usize,
    drop_reply: Option<HewReplyDropFn>,
    waker: *const HewWaker,
    deadline_ns: i64,
    kind: RequestKind,
    payload_release: Option<HewValueReleaseStart>,
    reply_release: Option<HewValueReleaseStart>,
    caller: crate::actor_caller::HewCallerStamp,
    dispatch: *const c_void,
    #[cfg(not(target_arch = "wasm32"))] remote_admission: Option<
        crate::hew_node::service::RemoteAdmission,
    >,
) -> *mut HewActorCall {
    let mut rejected_payload = ptr::null_mut();
    let envelope = if payload.is_null() {
        ptr::null_mut()
    } else {
        // SAFETY: this call owns the fresh payload on every outcome.
        let envelope = unsafe { hew_msg_envelope_new(payload, size, Some(drop_payload)) };
        if envelope.is_null() {
            // SAFETY: no envelope accepted the transferred fields.
            unsafe {
                if let Some(start) = payload_release {
                    rejected_payload = HewReleaseCursor::payload(payload, size, start, true);
                } else {
                    drop_payload(payload);
                    crate::mem::buf_free(payload);
                }
            }
        }
        if !envelope.is_null() {
            // SAFETY: this unpublished envelope belongs to the operation.
            unsafe { (*envelope).release_start = payload_release };
        }
        envelope
    };
    // SAFETY: the sealed constructor takes this envelope and the same protocol.
    unsafe {
        let operation = actor_call_resume(
            token,
            message,
            envelope,
            reply_size,
            drop_reply,
            waker,
            deadline_ns,
            kind,
            reply_release,
            caller,
            dispatch,
            #[cfg(not(target_arch = "wasm32"))]
            remote_admission,
        );
        (*operation).rejected_payload = rejected_payload;
        operation
    }
}

/// Start the same operation using a sealed request, preserving its identity.
///
/// # Safety
/// `caller` points to a live stamp copied before this function returns.
/// `envelope` is null or uniquely owns the checked target's request protocol.
/// The reply descriptor and retained waker obey their native contracts.
#[no_mangle]
#[allow(
    clippy::too_many_arguments,
    reason = "compiler-private typed operation ABI"
)]
pub unsafe extern "C" fn hew_actor_call_resume(
    token: HewLocalPidId,
    message: i32,
    envelope: *mut HewMsgEnvelope,
    reply_size: usize,
    drop_reply: Option<HewReplyDropFn>,
    waker: *const HewWaker,
    deadline_ns: i64,
    policy: i32,
    reply_release: Option<HewValueReleaseStart>,
    caller: *const crate::actor_caller::HewCallerStamp,
    dispatch: *const c_void,
) -> *mut HewActorCall {
    // SAFETY: forwards the owned request and captures its local sender.
    unsafe {
        actor_call_resume(
            token,
            message,
            envelope,
            reply_size,
            drop_reply,
            waker,
            deadline_ns,
            if policy == 0 {
                RequestKind::CallWait
            } else {
                RequestKind::CallReject
            },
            reply_release,
            *caller,
            dispatch,
            #[cfg(not(target_arch = "wasm32"))]
            None,
        )
    }
}

#[allow(
    clippy::too_many_arguments,
    clippy::too_many_lines,
    reason = "the checked request and reply carry one complete admission contract"
)]
pub(crate) unsafe fn actor_call_resume(
    token: HewLocalPidId,
    message: i32,
    envelope: *mut HewMsgEnvelope,
    reply_size: usize,
    drop_reply: Option<HewReplyDropFn>,
    waker: *const HewWaker,
    deadline_ns: i64,
    kind: RequestKind,
    reply_release: Option<HewValueReleaseStart>,
    caller: crate::actor_caller::HewCallerStamp,
    dispatch: *const c_void,
    #[cfg(not(target_arch = "wasm32"))] remote_admission: Option<
        crate::hew_node::service::RemoteAdmission,
    >,
) -> *mut HewActorCall {
    #[cfg(not(target_arch = "wasm32"))]
    if let Some(service) = crate::lifetime::local_handles::current_imported_actor(token) {
        // SAFETY: this operation owns the sealed request throughout encoding.
        let remote = unsafe {
            let (payload, size) = envelope.as_ref().map_or((ptr::null_mut(), 0), |envelope| {
                (envelope.payload, envelope.payload_size)
            });
            crate::hew_node::service_api::start_imported_actor_call(
                &service,
                dispatch,
                message,
                payload,
                size,
                reply_size,
                deadline_ns,
                waker,
                kind,
            )
        };
        let mut operation = Box::new(HewActorCall {
            route: CallRoute::Imported {
                operation: remote,
                request: envelope,
            },
            owned_target: None,
            expected_size: reply_size,
            kind,
            state: CallState::Waiting,
            target: None,
            cleanup_started: false,
            cleanup: None,
            rejected_payload: ptr::null_mut(),
        });
        // SAFETY: the operation exclusively owns the remote readiness state.
        unsafe { operation.poll() };
        return Box::into_raw(operation);
    }
    #[cfg(target_arch = "wasm32")]
    let _ = dispatch;
    let channel = if kind.completion_policy().is_some() {
        // SAFETY: registration precedes any request publication.
        unsafe { hew_reply_channel_new_native(waker, drop_reply, reply_release) }
    } else {
        ptr::null_mut()
    };
    let deadline = crate::request_deadline::RequestDeadline::from_raw(deadline_ns);
    let timer = if deadline_ns >= 0 {
        // SAFETY: the descriptor remains live for construction and is retained.
        unsafe { hew_coro_sleep_until_new(deadline_ns, waker) }
    } else {
        ptr::null_mut()
    };
    #[cfg(not(target_arch = "wasm32"))]
    let observe_queue = remote_admission.is_some() && channel.is_null();
    #[cfg(target_arch = "wasm32")]
    let observe_queue = false;
    // SAFETY: metadata is installed once before either owner publishes it.
    let request = unsafe {
        if channel.is_null() {
            let state = RequestState::new(
                deadline,
                #[cfg(not(target_arch = "wasm32"))]
                remote_admission,
            );
            Arc::new(if observe_queue {
                state.with_observer(crate::wake::OwnedWaker::retain(&*waker))
            } else {
                state
            })
        } else {
            crate::reply_channel::configure_native_request(
                channel,
                deadline,
                #[cfg(not(target_arch = "wasm32"))]
                remote_admission,
            );
            crate::reply_channel::native_request_state(channel).expect("configured request")
        }
    };
    // SAFETY: the operation transfers its envelope and retains this exact state.
    let admission = unsafe {
        request_wait_resume(
            token,
            message,
            envelope,
            channel.cast(),
            waker,
            caller,
            Some(Arc::clone(&request)),
            kind.completion_policy().is_some(),
        )
    };
    let mut operation = Box::new(HewActorCall {
        route: CallRoute::Local(LocalCall {
            admission,
            channel,
            timer,
            request,
        }),
        owned_target: None,
        expected_size: reply_size,
        kind,
        state: CallState::Admitting,
        target: crate::actor_native::wait_graph::resolve_target(token),
        cleanup_started: false,
        cleanup: None,
        rejected_payload: ptr::null_mut(),
    });
    // SAFETY: the first attempt establishes admission in source evaluation order.
    unsafe { operation.poll() };
    Box::into_raw(operation)
}

/// Seal a mailbox message before transferring its fields and optional actor
/// descriptor. A null result leaves those fields with the caller.
/// # Safety
/// The wrapper shallowly references the caller's uniquely owned message fields;
/// all descriptors match the selected handler and remain live through this call.
#[no_mangle]
#[allow(
    clippy::too_many_arguments,
    reason = "compiler-private typed submission ABI"
)]
pub unsafe extern "C" fn hew_actor_submit_new(
    token: HewLocalPidId,
    message: i32,
    payload: *mut c_void,
    size: usize,
    drop_payload: HewMsgEnvelopeDropFn,
    waker: *const HewWaker,
    deadline_ns: i64,
    kind: u8,
    payload_release: Option<HewValueReleaseStart>,
    caller: *const crate::actor_caller::HewCallerStamp,
    dispatch: *const c_void,
    owns_target: bool,
) -> *mut HewActorCall {
    let Some(
        kind @ (RequestKind::SendWait | RequestKind::SendReject | RequestKind::SendDropNewest),
    ) = RequestKind::from_wire(kind)
    else {
        std::process::abort();
    };
    // SAFETY: sealing only allocates the envelope; the fields still belong to
    // the caller on failure and the shallow wrapper then has no ownership.
    unsafe {
        let envelope = hew_msg_envelope_new(payload, size, Some(drop_payload));
        if envelope.is_null() {
            crate::mem::buf_free(payload);
            return ptr::null_mut();
        }
        (*envelope).release_start = payload_release;
        let operation = actor_call_resume(
            token,
            message,
            envelope,
            0,
            None,
            waker,
            deadline_ns,
            kind,
            None,
            *caller,
            dispatch,
            #[cfg(not(target_arch = "wasm32"))]
            None,
        );
        (*operation).owned_target = owns_target.then_some(token);
        operation
    }
}

/// Take admission evidence. Only definite refusal returns ownership to the
/// original source message; uncertainty remains consumed by this operation.
/// # Panics
/// Panics if a refused request has lost its owned envelope.
/// # Safety
/// The call is ready and unique; both outputs are writable boolean slots.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_submit_take(
    operation: *mut HewActorCall,
    refused: *mut bool,
    discarded: *mut bool,
) -> i32 {
    // SAFETY: the caller owns the operation and its admission output slots.
    unsafe {
        let operation = &mut *operation;
        let outcome = operation.take_outcome(ptr::null_mut());
        *refused = matches!(outcome, RequestOutcome::Refused(_));
        *discarded = matches!(outcome, RequestOutcome::Submitted { discarded: true });
        if *refused {
            let envelope = operation.detach_request();
            assert!(
                !envelope.is_null(),
                "refused submission retains its original fields"
            );
            crate::mem::buf_free((*envelope).payload);
            crate::cow_envelope::free_allocation(envelope);
            operation.owned_target.take();
        }
        outcome.code()
    }
}

/// Observe a call without taking its reply or rejected request.
///
/// # Safety
/// `operation` is a live, exclusively driven call.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_call_poll(operation: *mut HewActorCall) -> i32 {
    // SAFETY: the caller exclusively drives this live operation.
    unsafe { (*operation).poll() }
}

/// Whether a ready status hands the unaccepted request back to the caller: a
/// Reject call refused for a full mailbox or for runtime shutdown. A Wait call
/// cannot reject, so cleanup releases its request instead.
fn call_returns_request(reject: bool, status: i32) -> bool {
    reject && (status == AskError::MailboxFull as i32 || status == AskError::LocalShutdown as i32)
}

/// Transfer the selected reply, or the original request on a Reject refusal.
/// No operation keeps either output address. Free the operation after taking.
///
/// # Safety
/// The call has a nonnegative ready outcome and has not been taken. `output`
/// has its declared reply size; `request` is one writable envelope pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_call_take(
    operation: *mut HewActorCall,
    output: *mut c_void,
    request: *mut *mut HewMsgEnvelope,
) -> i32 {
    // SAFETY: the selected caller owns the call and its result slots.
    unsafe {
        let operation = &mut *operation;
        *request = ptr::null_mut();
        let outcome = operation.take_outcome(output);
        let status = outcome.code();
        if matches!(outcome, RequestOutcome::Refused(_))
            && call_returns_request(operation.kind == RequestKind::CallReject, status)
        {
            *request = operation.detach_request();
        }
        status
    }
}

/// Drain owners abandoned by a completed or cancelled call before freeing it.
/// # Safety
/// The operation and parent invocation remain live until this returns complete.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_call_cleanup_poll(
    operation: *mut HewActorCall,
    parent: *mut crate::coro_state::HewCoroState,
) -> i32 {
    // SAFETY: generated cleanup exclusively owns this optional operation.
    let Some(operation) = (unsafe { operation.as_mut() }) else {
        return 1;
    };
    // SAFETY: the operation exclusively owns its abandoned request and reply.
    unsafe { operation.prepare_cleanup() };
    i32::from(operation.cleanup.as_mut().is_none_or(|driver| {
        // SAFETY: the parent and boxed driver remain live through cleanup.
        unsafe { driver.poll(parent) }
    }))
}

/// Transfer the completed cleanup diagnostic, if any.
/// # Safety
/// The operation's cleanup has completed and this fault is taken at most once.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_call_cleanup_fault(
    operation: *mut HewActorCall,
) -> *mut crate::fault::HewFault {
    // SAFETY: generated cleanup uniquely owns the completed operation.
    unsafe { operation.as_mut() }
        .and_then(|operation| operation.cleanup.as_mut())
        .map_or(ptr::null_mut(), |driver| driver.take_fault())
}

/// Release an owned call, withdrawing a queued loser or tombstoning a late reply.
///
/// # Safety
/// `operation` is null or the unique live call, with no active poll or take.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_call_free(operation: *mut HewActorCall) {
    if !operation.is_null() {
        // SAFETY: the caller transfers its one operation owner.
        drop(unsafe { Box::from_raw(operation) });
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::reply_channel::hew_reply_channel_retain;
    use crate::wake::blocking::Readiness;
    use std::sync::atomic::{AtomicUsize, Ordering};

    unsafe extern "C" fn drop_counted(payload: *mut c_void) {
        // SAFETY: this payload owns the one raw Arc transferred into its slot.
        let count = unsafe { Arc::from_raw(*payload.cast::<*const AtomicUsize>()) };
        count.fetch_add(1, Ordering::SeqCst);
    }

    struct DispatchState {
        operation: *mut HewActorCall,
        calls: usize,
        reply_drops: Arc<AtomicUsize>,
    }

    unsafe extern "C-unwind" fn dispatch_late_reply(
        _: *mut crate::execution_context::HewExecutionContext,
        state: *mut c_void,
        _: i32,
        _: *mut c_void,
        _: usize,
        _: i32,
    ) -> *mut c_void {
        // SAFETY: activation owns this fixture state. Abandon after the dispatch
        // claim, then publish an owned reply through the real scheduler context.
        unsafe {
            let state = &mut *state.cast::<DispatchState>();
            state.calls += 1;
            hew_actor_call_free(state.operation);
            state.operation = ptr::null_mut();
            let mut reply = Arc::into_raw(state.reply_drops.clone());
            crate::reply_channel::native::hew_actor_reply_native(
                (&raw mut reply).cast(),
                size_of_val(&reply),
                Some(drop_counted),
                None,
            );
        }
        ptr::null_mut()
    }

    #[test]
    fn queued_loser_never_dispatches_and_dispatched_loser_drops_its_late_reply() {
        let _guard = crate::runtime_test_guard();
        let _scheduler = crate::scheduler::NoWorkerSchedulerForTest::install();
        for (dispatched, expired) in [(false, false), (true, false), (false, true)] {
            let (ready, waker) = Readiness::new();
            let request_drops = Arc::new(AtomicUsize::new(0));
            let reply_drops = Arc::new(AtomicUsize::new(0));
            // SAFETY: the fixture owns the actor/mailbox, request envelope and
            // receiver; successful admission transfers one retained sender debt.
            unsafe {
                let channel =
                    hew_reply_channel_new_native(waker.descriptor(), Some(drop_counted), None);
                crate::reply_channel::configure_native_request(
                    channel,
                    crate::request_deadline::RequestDeadline::from_raw(if expired {
                        0
                    } else {
                        -1
                    }),
                    #[cfg(not(target_arch = "wasm32"))]
                    None,
                );
                let operation = Box::into_raw(Box::new(HewActorCall {
                    route: CallRoute::Local(LocalCall {
                        admission: ptr::null_mut(),
                        channel,
                        timer: ptr::null_mut(),
                        request: crate::reply_channel::native_request_state(channel).unwrap(),
                    }),
                    owned_target: None,
                    expected_size: size_of::<*const AtomicUsize>(),
                    kind: RequestKind::CallWait,
                    state: CallState::Waiting,
                    target: None,
                    cleanup_started: false,
                    cleanup: None,
                    rejected_payload: ptr::null_mut(),
                }));
                let payload = crate::mem::buf_try_alloc(size_of::<*const AtomicUsize>())
                    .cast::<*const AtomicUsize>();
                payload.write(Arc::into_raw(request_drops.clone()));
                let envelope = hew_msg_envelope_new(
                    payload.cast(),
                    size_of::<*const AtomicUsize>(),
                    Some(drop_counted),
                );
                let mailbox = crate::mailbox::hew_mailbox_new();
                hew_reply_channel_retain(channel);
                assert!(matches!(
                    crate::mailbox::try_admit_native_request(
                        &*mailbox,
                        7,
                        envelope,
                        channel.cast(),
                        true,
                        crate::actor_caller::local_sender()
                    ),
                    crate::mailbox::SendOutcome::Enqueued
                ));
                let mut state = DispatchState {
                    operation,
                    calls: 0,
                    reply_drops: reply_drops.clone(),
                };
                let mut actor = crate::test_actor::stub_actor();
                actor.mailbox = mailbox.cast();
                actor.state = (&raw mut state).cast();
                actor.state_size = size_of_val(&state);
                actor.dispatch = Some(dispatch_late_reply);
                drop(waker);
                if !dispatched && !expired {
                    hew_actor_call_free(operation);
                    state.operation = ptr::null_mut();
                    assert_eq!(Arc::strong_count(&ready), 1);
                }
                crate::scheduler::activate_actor_for_test(&raw mut actor);
                if expired {
                    assert_eq!(hew_actor_call_poll(operation), AskError::Timeout as i32);
                    hew_actor_call_free(operation);
                    state.operation = ptr::null_mut();
                    assert!(ready.take_ready());
                }
                assert!(state.operation.is_null());
                assert_eq!(state.calls, usize::from(dispatched));
                assert_eq!(request_drops.load(Ordering::SeqCst), 1);
                assert_eq!(reply_drops.load(Ordering::SeqCst), usize::from(dispatched));
                assert_eq!(Arc::strong_count(&ready), 1);
                assert!(!ready.take_ready());
                crate::mailbox::hew_mailbox_free(mailbox);
            }
        }
    }

    #[test]
    fn ready_owned_reply_is_observed_repeatedly_and_transferred_only_by_take() {
        let (ready, waker) = Readiness::new();
        let drops = Arc::new(AtomicUsize::new(0));
        // SAFETY: the call has one receiver and one sender; the typed Arc is
        // transferred to the reply and then to the selected output exactly once.
        unsafe {
            let channel =
                hew_reply_channel_new_native(waker.descriptor(), Some(drop_counted), None);
            crate::reply_channel::configure_native_request(
                channel,
                crate::request_deadline::RequestDeadline::default(),
                #[cfg(not(target_arch = "wasm32"))]
                None,
            );
            let operation = Box::into_raw(Box::new(HewActorCall {
                route: CallRoute::Local(LocalCall {
                    admission: ptr::null_mut(),
                    channel,
                    timer: ptr::null_mut(),
                    request: crate::reply_channel::native_request_state(channel).unwrap(),
                }),
                owned_target: None,
                expected_size: size_of::<*const AtomicUsize>(),
                kind: RequestKind::CallWait,
                state: CallState::Waiting,
                target: None,
                cleanup_started: false,
                cleanup: None,
                rejected_payload: ptr::null_mut(),
            }));
            hew_reply_channel_retain(channel);
            let mut reply = Arc::into_raw(drops.clone());
            assert!(crate::reply_channel::hew_reply(
                channel,
                (&raw mut reply).cast(),
                size_of_val(&reply)
            ));
            assert!(ready.take_ready());
            crate::reply_channel::native::expire_native_request(channel);
            assert_eq!(hew_actor_call_poll(operation), 0);
            assert_eq!(hew_actor_call_poll(operation), 0);
            assert_eq!(drops.load(Ordering::SeqCst), 0);
            let mut output = ptr::null::<AtomicUsize>();
            let mut request = ptr::null_mut();
            assert_eq!(
                hew_actor_call_take(operation, (&raw mut output).cast(), &raw mut request),
                0
            );
            assert!(request.is_null());
            assert_eq!(hew_actor_call_poll(operation), -2);
            hew_actor_call_free(operation);
            assert_eq!(drops.load(Ordering::SeqCst), 0);
            drop_counted((&raw mut output).cast());
            assert_eq!(drops.load(Ordering::SeqCst), 1);
        }
    }
}
