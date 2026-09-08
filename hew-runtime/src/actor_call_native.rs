//! One owned completion call, from admission through non-consuming readiness to
//! the selected reply or rejected request. Ordinary calls and select arms use
//! this same operation; a borrowed task observation is a different lifetime.

use crate::coro_sleep::{
    hew_coro_sleep_free, hew_coro_sleep_new, hew_coro_sleep_status, HewCoroSleep,
};
use crate::internal::types::AskError;
use crate::lifetime::{live_actors::ActorIncarnation, local_handles::HewLocalPidId};
use crate::mailbox::native::{
    hew_actor_ask_wait_free, hew_actor_ask_wait_poll, hew_actor_ask_wait_resume,
    hew_actor_ask_wait_take_request, HewNativeAsk,
};
use crate::mailbox::{hew_msg_envelope_new, HewMsgEnvelope, HewMsgEnvelopeDropFn};
use crate::reply_channel::native::{
    hew_reply_channel_new_native, hew_reply_channel_poll_native, hew_reply_channel_status_native,
};
use crate::reply_channel::{
    hew_reply_channel_cancel, hew_reply_channel_free, HewReplyChannel, HewReplyDropFn,
};
use crate::wake::HewWaker;
use std::{ffi::c_void, ptr};

#[derive(Debug)]
enum CallState {
    Admitting,
    Waiting,
    Ready(i32),
    Taken,
}

/// The request and eventual result remain owned here until an explicit take.
/// Its drop withdraws an undispatched request or tombstones a dispatched reply.
#[derive(Debug)]
pub struct HewActorCall {
    admission: *mut HewNativeAsk,
    channel: *mut HewReplyChannel,
    timer: *mut HewCoroSleep,
    expected_size: usize,
    reject: bool,
    state: CallState,
    pub(crate) target: Option<ActorIncarnation>,
}

impl HewActorCall {
    pub(crate) fn has_deadline(&self) -> bool {
        !self.timer.is_null()
    }

    /// Observe admission, reply and deadline in that order. A ready outcome is
    /// latched: later observations cannot submit twice or change the winner.
    unsafe fn poll(&mut self) -> i32 {
        match self.state {
            CallState::Ready(status) => return status,
            CallState::Taken => return -2,
            CallState::Admitting => {
                // SAFETY: this operation exclusively owns the admission.
                let status = unsafe { hew_actor_ask_wait_poll(self.admission) };
                if status == -1 {
                    if self.reject {
                        self.state = CallState::Ready(AskError::MailboxFull as i32);
                        return AskError::MailboxFull as i32;
                    }
                    return self.poll_deadline();
                }
                if status != 0 {
                    self.state = CallState::Ready(status);
                    return status;
                }
                // SAFETY: admission transferred its request and sender reference.
                unsafe { hew_actor_ask_wait_free(self.admission) };
                self.admission = ptr::null_mut();
                self.state = CallState::Waiting;
            }
            CallState::Waiting => {}
        }
        // SAFETY: this operation owns the only receiving reference and knows
        // the exact reply size. Readiness does not take any payload.
        let status = unsafe { hew_reply_channel_status_native(self.channel, self.expected_size) };
        if status != -1 {
            self.state = CallState::Ready(status);
            return status;
        }
        self.poll_deadline()
    }

    fn poll_deadline(&mut self) -> i32 {
        if self.timer.is_null() {
            return -1;
        }
        // SAFETY: the operation uniquely owns its optional timer.
        let status = unsafe { hew_coro_sleep_status(self.timer) };
        let result = match status {
            0 => return -1,
            1 => AskError::Timeout as i32,
            _ => -2,
        };
        self.state = CallState::Ready(result);
        result
    }
}

impl Drop for HewActorCall {
    fn drop(&mut self) {
        // SAFETY: all three operations belong to this call. Cancellation wins
        // against dispatch before any sender reference can be released.
        unsafe {
            crate::reply_channel::withdraw_native_request(self.channel);
            hew_reply_channel_cancel(self.channel);
            hew_actor_ask_wait_free(self.admission);
            hew_coro_sleep_free(self.timer);
            hew_reply_channel_free(self.channel);
        }
    }
}

/// Transfer fresh request fields and start admission without parking the caller.
/// `policy` is zero for Wait and one for Reject. `has_deadline` distinguishes
/// no deadline from every signed duration, including an already expired one.
///
/// # Safety
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
    duration_ns: i64,
    has_deadline: i32,
    policy: i32,
) -> *mut HewActorCall {
    let envelope = if payload.is_null() {
        ptr::null_mut()
    } else {
        // SAFETY: this call owns the fresh payload on every outcome.
        let envelope = unsafe { hew_msg_envelope_new(payload, size, Some(drop_payload)) };
        if envelope.is_null() {
            // SAFETY: no envelope accepted the transferred fields.
            unsafe {
                drop_payload(payload);
                libc::free(payload);
            }
        }
        envelope
    };
    // SAFETY: the sealed constructor takes this envelope and the same protocol.
    unsafe {
        hew_actor_call_resume(
            token,
            message,
            envelope,
            reply_size,
            drop_reply,
            waker,
            duration_ns,
            has_deadline,
            policy,
        )
    }
}

/// Start the same operation using a sealed request, preserving its identity.
///
/// # Safety
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
    duration_ns: i64,
    has_deadline: i32,
    policy: i32,
) -> *mut HewActorCall {
    // SAFETY: registration precedes any request publication.
    let channel = unsafe { hew_reply_channel_new_native(waker, drop_reply) };
    let timer = if has_deadline != 0 {
        // SAFETY: the descriptor remains live for construction and is retained.
        unsafe { hew_coro_sleep_new(duration_ns, waker) }
    } else {
        ptr::null_mut()
    };
    // SAFETY: channel and request are still unpublished, and this call owns both.
    let admission = unsafe {
        crate::reply_channel::enable_native_withdrawal(channel);
        hew_actor_ask_wait_resume(token, message, envelope, channel.cast(), waker)
    };
    let mut operation = Box::new(HewActorCall {
        admission,
        channel,
        timer,
        expected_size: reply_size,
        reject: policy != 0,
        state: CallState::Admitting,
        target: crate::actor_native::wait_graph::resolve_target(token),
    });
    // SAFETY: the first attempt establishes admission in source evaluation order.
    unsafe { operation.poll() };
    Box::into_raw(operation)
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
        let CallState::Ready(status) = operation.state else {
            return -2;
        };
        if status < 0 {
            return status;
        }
        *request = ptr::null_mut();
        if status == AskError::MailboxFull as i32 {
            *request = hew_actor_ask_wait_take_request(operation.admission);
        } else if status == 0 {
            let taken =
                hew_reply_channel_poll_native(operation.channel, operation.expected_size, output);
            debug_assert_eq!(taken, 0, "ready reply changes before its unique take");
        }
        operation.state = CallState::Taken;
        status
    }
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
    use std::sync::{
        atomic::{AtomicUsize, Ordering},
        Arc,
    };

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
            );
        }
        ptr::null_mut()
    }

    #[test]
    fn queued_loser_never_dispatches_and_dispatched_loser_drops_its_late_reply() {
        let _guard = crate::runtime_test_guard();
        let _scheduler = crate::scheduler::NoWorkerSchedulerForTest::install();
        for dispatched in [false, true] {
            let (ready, waker) = Readiness::new();
            let request_drops = Arc::new(AtomicUsize::new(0));
            let reply_drops = Arc::new(AtomicUsize::new(0));
            // SAFETY: the fixture owns the actor/mailbox, request envelope and
            // receiver; successful admission transfers one retained sender debt.
            unsafe {
                let channel = hew_reply_channel_new_native(waker.descriptor(), Some(drop_counted));
                crate::reply_channel::enable_native_withdrawal(channel);
                let operation = Box::into_raw(Box::new(HewActorCall {
                    admission: ptr::null_mut(),
                    channel,
                    timer: ptr::null_mut(),
                    expected_size: size_of::<*const AtomicUsize>(),
                    reject: false,
                    state: CallState::Waiting,
                    target: None,
                }));
                let payload =
                    libc::malloc(size_of::<*const AtomicUsize>()).cast::<*const AtomicUsize>();
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
                        channel.cast()
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
                actor.dispatch_ownership = crate::actor::HewDispatchOwnership::UniqueEnvelope;
                drop(waker);
                if !dispatched {
                    hew_actor_call_free(operation);
                    state.operation = ptr::null_mut();
                    assert_eq!(Arc::strong_count(&ready), 1);
                }
                crate::scheduler::activate_actor_for_test(&raw mut actor);
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
            let channel = hew_reply_channel_new_native(waker.descriptor(), Some(drop_counted));
            let operation = Box::into_raw(Box::new(HewActorCall {
                admission: ptr::null_mut(),
                channel,
                timer: ptr::null_mut(),
                expected_size: size_of::<*const AtomicUsize>(),
                reject: false,
                state: CallState::Waiting,
                target: None,
            }));
            hew_reply_channel_retain(channel);
            let mut reply = Arc::into_raw(drops.clone());
            assert!(crate::reply_channel::hew_reply(
                channel,
                (&raw mut reply).cast(),
                size_of_val(&reply)
            ));
            assert!(ready.take_ready());
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
