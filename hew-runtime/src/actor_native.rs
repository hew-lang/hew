//! Checked completion at the existing actor dispatch boundary.
//!
//! The execution context owns a returned logical fault until its scheduler
//! activation consumes it. Generated callbacks retain the nullable continuation
//! ABI and never encode a fault as a continuation or unwind a native frame.

use std::ptr;
use std::sync::Arc;

use crate::execution_context::HewExecutionContext;
use crate::fault::HewFault;
use crate::lifetime::live_actors::ActorIncarnation;

#[path = "actor_native_close.rs"]
mod close;
pub(crate) use close::{finish_native_terminal, hew_actor_close_native, hew_actor_wait_new};
pub use close::{HewNativeActorWait, NativeActorCompletion};
#[path = "actor_native_wait_graph.rs"]
pub(crate) mod wait_graph;

unsafe extern "C" fn wake_actor(context: *mut std::ffi::c_void) {
    // SAFETY: each descriptor retains the immutable incarnation allocation.
    let target = unsafe { *context.cast::<ActorIncarnation>() };
    wait_graph::ready(target);
    crate::scheduler::enqueue_resume_by_incarnation(target);
}

unsafe extern "C" fn retain_actor_wake(context: *mut std::ffi::c_void) {
    // SAFETY: the descriptor's owner keeps an Arc reference live during retain.
    unsafe { Arc::increment_strong_count(context.cast::<ActorIncarnation>()) };
}

unsafe extern "C" fn release_actor_wake(context: *mut std::ffi::c_void) {
    // SAFETY: consumes exactly one reference acquired by descriptor retention.
    unsafe { Arc::decrement_strong_count(context.cast::<ActorIncarnation>()) };
}

/// Create a handler invocation whose readiness resumes its exact actor turn.
/// Operations may retain the wake target after the invocation or actor is gone;
/// the live incarnation registry rejects those late notifications.
///
/// # Safety
/// The current execution context must belong to the live actor dispatch that
/// owns this invocation. Release the state only after destroying its frame.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_coro_state_new() -> *mut crate::coro_state::HewCoroState {
    let context = crate::execution_context::current_context();
    // SAFETY: the generated dispatch adapter runs under its live context.
    let context = unsafe { &*context };
    // SAFETY: activation ownership keeps the actor alive during capture.
    let target = Arc::new(unsafe { ActorIncarnation::of(context.actor) });
    let waker = crate::wake::HewWaker {
        context: Arc::as_ptr(&target).cast_mut().cast(),
        wake: wake_actor,
        retain: retain_actor_wake,
        release: release_actor_wake,
    };
    // SAFETY: the local Arc and current context retain both inputs for creation.
    let state =
        unsafe { crate::coro_state::hew_coro_state_new(&raw const waker, context.cancel_token) };
    // SAFETY: the new invocation belongs exclusively to this strict actor turn.
    unsafe { (*state).actor_turn = *target };
    // SAFETY: this activation owns the actor and its one strict turn. The
    // adapter clears the borrowed slot after child completion under the same
    // activation ownership, before another turn can start.
    unsafe { &*context.actor }
        .checked_invocation
        .store(state.cast(), std::sync::atomic::Ordering::Release);
    state
}

/// Publish a completed handler fault through the current resume context.
///
/// # Safety
/// The current context is the exclusively owned actor activation. `fault` is
/// null or an owned logical fault, relinquished by the completed handler.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_coro_set_fault(fault: *mut HewFault) {
    let context = crate::execution_context::current_context();
    // SAFETY: this activation exclusively owns the completed checked turn.
    unsafe { &*(*context).actor }
        .checked_invocation
        .store(ptr::null_mut(), std::sync::atomic::Ordering::Release);
    // SAFETY: resume installs a fresh context before entering generated code.
    unsafe { hew_actor_dispatch_set_fault(context, fault) };
}

/// Retain a `#[on(stop)]` fault as the actor's lifecycle diagnostic. The
/// terminal completion publishes its code to every termination observer.
///
/// # Safety
/// The current context is the terminate activation installed by
/// `call_terminate_fn`. `fault` is an owned logical fault, relinquished here.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_terminate_set_fault(fault: *mut HewFault) {
    // SAFETY: the generated terminate sequence relinquishes one owned fault.
    let fault = unsafe { Box::from_raw(fault) };
    let code = report_checked_failure(&fault);
    crate::trap_code::stamp_current_actor_error_code(code);
}

/// Request cooperative cleanup while preserving the scheduler's strict turn.
///
/// # Safety
/// The caller owns the actor activation, excluding execution and invocation
/// teardown. External stoppers must latch mailbox stop and wake the actor.
pub(crate) unsafe fn cancel_checked_turn(actor: &crate::actor::HewActor) -> bool {
    let state = actor
        .checked_invocation
        .load(std::sync::atomic::Ordering::Acquire);
    if state.is_null() {
        return false;
    }
    // SAFETY: activation ownership keeps the borrowed invocation state live.
    unsafe { crate::coro_state::hew_coro_state_cancel(state.cast()) };
    true
}

/// Finish the process root after its source cleanup and child joins complete.
/// Queued actor work and supervisor decisions use the existing shutdown path.
#[no_mangle]
pub extern "C" fn hew_native_runtime_finish(source_status: i32) -> i32 {
    crate::shutdown::hew_shutdown_initiate_implicit(0);
    let shutdown_status = crate::shutdown::hew_shutdown_wait();
    crate::scheduler::hew_runtime_cleanup_after_main();
    if source_status != 0 {
        source_status
    } else if shutdown_status != 0 {
        1
    } else {
        crate::exit_status::hew_runtime_exit_status()
    }
}

/// Allocate a generated state or message wrapper with the native allocator.
/// Allocation failure has the same process-fatal policy as other value buffers.
#[no_mangle]
#[must_use]
pub extern "C" fn hew_actor_payload_alloc(size: usize) -> *mut std::ffi::c_void {
    // SAFETY: malloc accepts every size; zero-sized wrappers get one byte.
    let allocation = unsafe { libc::malloc(size.max(1)) };
    if allocation.is_null() {
        std::process::abort();
    }
    allocation
}

/// Allocate an unpublished message wrapper, preserving the source on failure.
#[no_mangle]
#[must_use]
pub extern "C" fn hew_actor_payload_try_alloc(size: usize) -> *mut std::ffi::c_void {
    // SAFETY: malloc accepts every size; zero-sized wrappers still need an address.
    unsafe { libc::malloc(size.max(1)) }
}

/// Try to transfer a generated message wrapper into its exact destination.
/// Returns 0 for acceptance, 1 for full, 2 for closed, 3 for allocation failure,
/// and 4 for an explicitly selected newest-message discard.
///
/// # Safety
/// `payload` is an unpublished malloc wrapper containing shallowly transferred
/// typed fields. Acceptance or discard consumes those fields; rejection frees
/// only the wrapper bytes, leaving the original typed message with the caller.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_submit_native(
    token: crate::lifetime::local_handles::HewLocalPidId,
    message: i32,
    payload: *mut std::ffi::c_void,
    size: usize,
    drop_payload: crate::mailbox::HewMsgEnvelopeDropFn,
    policy: i32,
) -> i32 {
    if payload.is_null() {
        return 3;
    }
    // SAFETY: the wrapper is uniquely owned until the mailbox accepts it.
    let envelope =
        unsafe { crate::mailbox::hew_msg_envelope_new(payload, size, Some(drop_payload)) };
    if envelope.is_null() {
        // SAFETY: the source still owns all typed fields in the unpublished wrapper.
        unsafe {
            libc::free(payload);
        }
        return 3;
    }
    // SAFETY: the envelope is unpublished and transfers only on admission.
    let outcome = unsafe { crate::actor::try_submit_native_envelope(token, message, envelope) };
    let status = match outcome {
        crate::mailbox::SendOutcome::Enqueued => return 0,
        crate::mailbox::SendOutcome::Failed if policy == 2 => {
            // SAFETY: explicit DropNewest transfers the typed payload for destruction.
            unsafe {
                crate::mailbox::hew_msg_envelope_release(envelope);
            }
            return 4;
        }
        crate::mailbox::SendOutcome::Failed => 1,
        crate::mailbox::SendOutcome::Closed => 2,
        crate::mailbox::SendOutcome::Oom => 3,
        _ => unreachable!("native admission cannot apply implicit eviction or discard"),
    };
    // SAFETY: admission failed without publishing or aliasing. The source retains
    // the typed fields; these two allocations contain no other owning resources.
    unsafe {
        libc::free(payload);
        libc::free(envelope.cast());
    }
    status
}

/// One pointer-sized owning slot in the opaque runtime execution context.
#[repr(transparent)]
#[derive(Debug)]
pub struct CheckedActorFault(*mut HewFault);

impl Default for CheckedActorFault {
    fn default() -> Self {
        Self(ptr::null_mut())
    }
}

impl CheckedActorFault {
    /// Transfer the current fault to the scheduler, leaving the slot empty.
    pub(crate) fn take(&mut self) -> Option<Box<HewFault>> {
        let fault = std::mem::replace(&mut self.0, ptr::null_mut());
        if fault.is_null() {
            None
        } else {
            // SAFETY: this slot owns the allocation and has relinquished it.
            Some(unsafe { Box::from_raw(fault) })
        }
    }
}

impl Drop for CheckedActorFault {
    fn drop(&mut self) {
        // Also release a fault if activation setup or lock teardown fails.
        drop(self.take());
    }
}

/// Transfer one checked handler fault into its current execution context.
///
/// # Safety
/// `ctx` must be the live, exclusively accessed dispatch context supplied to
/// this handler. `fault` must be a unique owner from the native fault API or
/// null. The caller relinquishes it, including on a secondary failure.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_dispatch_set_fault(
    ctx: *mut HewExecutionContext,
    fault: *mut HewFault,
) {
    // SAFETY: the generated dispatch adapter supplies its live context.
    let ctx = unsafe { &mut *ctx };
    // SAFETY: the context slot and argument are distinct optional owners.
    ctx.checked_fault.0 = unsafe { crate::fault::hew_fault_combine(ctx.checked_fault.0, fault) };
}

pub(crate) enum DispatchFailure {
    Checked(Box<HewFault>),
    Unwind(Box<dyn std::any::Any + Send>),
}

/// Join the callback's continuation result with its owned checked completion.
///
/// # Safety
/// `ctx` must remain the exclusively accessed current activation context.
pub(crate) unsafe fn dispatch_result(
    ctx: *mut HewExecutionContext,
    outcome: std::thread::Result<*mut std::ffi::c_void>,
) -> Result<*mut std::ffi::c_void, DispatchFailure> {
    match outcome {
        Err(payload) => Err(DispatchFailure::Unwind(payload)),
        Ok(handle) => {
            // SAFETY: the callback returned and this activation owns the slot.
            let fault = unsafe { (*ctx).checked_fault.take() }.and_then(|fault| {
                // SAFETY: the returned dispatch context still owns this actor.
                unsafe { normalize_stopped_turn((*ctx).actor, fault) }
            });
            match fault {
                None => Ok(handle),
                Some(fault) => {
                    if !handle.is_null() {
                        eprintln!("fatal: actor dispatch returned both a fault and a continuation");
                        std::process::abort();
                    }
                    Err(DispatchFailure::Checked(fault))
                }
            }
        }
    }
}

/// Remove a clean stop's cancellation after its checked source cleanup, while
/// preserving a destructor failure carried as a secondary diagnostic.
///
/// # Safety
/// The caller owns the actor activation and this unique returned fault.
pub(crate) unsafe fn normalize_stopped_turn(
    actor: *mut crate::actor::HewActor,
    fault: Box<HewFault>,
) -> Option<Box<HewFault>> {
    if fault.code() != crate::fault::HEW_FAULT_CANCELLED
        // SAFETY: this activation owns the live actor and its mailbox.
        || !unsafe { crate::mailbox::mailbox_stop_requested((*actor).mailbox.cast()) }
    {
        return Some(fault);
    }
    // SAFETY: normalization consumes and returns unique optional fault owners.
    let fault = unsafe { crate::fault::hew_fault_finish_cleanup(Box::into_raw(fault)) };
    if fault.is_null() {
        None
    } else {
        // SAFETY: normalization returned one uniquely owned fault.
        Some(unsafe { Box::from_raw(fault) })
    }
}

/// Preserve the checked diagnostic when an unhandled actor fault reaches its
/// existing scheduler crash and supervisor boundary.
pub(crate) fn report_checked_failure(fault: &HewFault) -> i32 {
    // SAFETY: the returned fault remains owned during this reporting borrow.
    let _ = unsafe { crate::fault::hew_fault_report(fault) };
    fault.code()
}

/// Finish the matching native completion or legacy unwind cleanup boundary.
///
/// # Safety
/// This scheduler activation must exclusively own `actor` and its dispatch
/// cleanup scope; generated native code has already run its checked cleanup.
pub(crate) unsafe fn finish_dispatch_failure(
    actor: *mut crate::actor::HewActor,
    failure: DispatchFailure,
) -> i32 {
    match failure {
        DispatchFailure::Checked(fault) => {
            // SAFETY: checked completion owns the still-open dispatch scope.
            if !unsafe { crate::cont::finish_dispatch_crash_cleanup() } {
                eprintln!("fatal: checked actor failure retained crash-cleanup owners");
                std::process::abort();
            }
            report_checked_failure(&fault)
        }
        DispatchFailure::Unwind(payload) => {
            crate::execution_context::reply_channel_swap_unwind();
            // SAFETY: catch_unwind proves synchronous ramp frames are dead.
            let _ =
                unsafe { crate::cont::reclaim_active_coroutine_frames_excluding(ptr::null_mut()) };
            // SAFETY: the scheduler owns the legacy state-recovery boundary.
            let outcome =
                unsafe { crate::cont::recover_dispatch_crash_cleanup_with_outcome(false) };
            if outcome.state_authority_consumed {
                // SAFETY: this activation exclusively owns the actor.
                unsafe { crate::actor::record_dispatch_state_drop_consumed(actor) };
            }
            let code = payload
                .downcast_ref::<crate::actor::HewPanic>()
                .map_or(101, |panic| panic.code);
            crate::util::quarantine_panic_payload(payload);
            code
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::c_void;
    use std::sync::atomic::Ordering;

    #[test]
    fn checked_waker_retains_incarnation_after_invocation_and_actor_replacement() {
        let _guard = crate::runtime_test_guard();
        let scheduler = crate::scheduler::NoWorkerSchedulerForTest::install();
        let actor = crate::test_actor::TrackedTestActor::install_parked();
        let mut context = HewExecutionContext {
            actor: actor.ptr(),
            ..HewExecutionContext::default()
        };
        let previous = crate::execution_context::set_current_context(&raw mut context);
        // SAFETY: the fixture owns the installed actor context and invocation.
        // The retained operation descriptor outlives both, as a late I/O wake can.
        let operation = unsafe {
            let state = hew_actor_coro_state_new();
            let operation =
                crate::wake::OwnedWaker::retain(&*crate::coro_state::hew_coro_state_waker(state));
            operation.wake();
            crate::test_actor::assert_woken(&scheduler, &actor, "checked handler");
            crate::coro_state::hew_coro_state_free(state);
            hew_actor_coro_set_fault(ptr::null_mut());
            operation
        };
        assert_eq!(
            crate::execution_context::set_current_context(previous),
            &raw mut context
        );
        actor.reincarnate_parked_reusing_id();
        operation.wake();
        crate::test_actor::assert_not_woken(&scheduler, &actor, "checked handler");
    }

    unsafe extern "C-unwind" fn checked_failure(
        ctx: *mut HewExecutionContext,
        state: *mut c_void,
        _message: i32,
        _data: *mut c_void,
        _size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        // SAFETY: the fixture supplies a live scalar state and dispatch context.
        unsafe {
            *state.cast::<i64>() = 7;
            hew_actor_dispatch_set_fault(ctx, crate::fault::hew_fault_new(202));
        }
        ptr::null_mut()
    }

    #[test]
    fn checked_dispatch_crashes_and_completes_its_reply_without_unwinding() {
        let _guard = crate::runtime_test_guard();
        let _scheduler = crate::scheduler::NoWorkerSchedulerForTest::install();
        let baseline = crate::reply_channel::active_channel_count();
        let channel = crate::reply_channel::hew_reply_channel_new();
        assert!(!channel.is_null());
        let mut state = 3_i64;
        let mut actor = crate::test_actor::stub_actor();
        actor.state = (&raw mut state).cast();
        actor.state_size = std::mem::size_of::<i64>();
        actor.dispatch = Some(checked_failure);
        // SAFETY: all allocations and both reply references remain live until
        // this single synchronous scheduler activation finishes.
        unsafe {
            let mailbox = crate::mailbox::hew_mailbox_new();
            assert!(!mailbox.is_null());
            actor.mailbox = mailbox.cast();
            crate::reply_channel::hew_reply_channel_retain(channel);
            assert_eq!(
                crate::mailbox::hew_mailbox_send_with_reply(
                    mailbox.cast(),
                    1,
                    ptr::null_mut(),
                    0,
                    channel.cast(),
                ),
                0
            );
            crate::scheduler::activate_actor_for_test(&raw mut actor);
            assert_eq!(
                actor.actor_state.load(Ordering::Acquire),
                crate::internal::types::HewActorState::Crashed as i32
            );
            assert_eq!(actor.error_code.load(Ordering::Acquire), 202);
            assert_eq!(state, 7);
            assert!(!actor.state_drop_consumed.load(Ordering::Acquire));
            assert_eq!(crate::reply_channel::ref_count_for_test(channel), 1);
            assert_eq!(
                crate::reply_channel::hew_reply_channel_await_status(channel),
                crate::await_cancel::AwaitCancelStatus::Completed as i32
            );
            crate::reply_channel::hew_reply_channel_free(channel);
            crate::mailbox::hew_mailbox_free(mailbox);
        }
        assert_eq!(crate::reply_channel::active_channel_count(), baseline);
        assert!(crate::execution_context::current_context().is_null());
    }

    unsafe extern "C" fn checked_resume(frame: *mut c_void) {
        // SAFETY: the scheduler owns this live scratch frame and installs the
        // resumed actor's context before invoking its continuation.
        unsafe {
            let frame = &mut *frame.cast::<crate::coro_exec::test_support::ScratchFrame>();
            frame.resumes.fetch_add(1, Ordering::AcqRel);
            frame.resume = None;
            let context = crate::execution_context::current_context();
            *(*context).actor.as_ref().unwrap().state.cast::<i64>() = 7;
            hew_actor_dispatch_set_fault(context, crate::fault::hew_fault_new(202));
        }
    }

    #[test]
    fn checked_resume_crashes_and_retires_reply_after_normal_frame_cleanup() {
        let _guard = crate::runtime_test_guard();
        let _scheduler = crate::scheduler::NoWorkerSchedulerForTest::install();
        let baseline = crate::reply_channel::active_channel_count();
        let channel = crate::reply_channel::hew_reply_channel_new();
        assert!(!channel.is_null());
        let mut state = 3_i64;
        let mut actor = crate::test_actor::stub_actor();
        let mut frame = crate::coro_exec::test_support::ScratchFrameOwner::new(1);
        frame.resume = Some(checked_resume);
        actor.state = (&raw mut state).cast();
        actor.state_size = std::mem::size_of::<i64>();
        actor
            .suspended_cont
            .store(frame.handle(), Ordering::Release);
        actor.cont_tag.store(
            crate::internal::types::ContTag::Parked as i32,
            Ordering::Release,
        );
        actor.actor_state.store(
            crate::internal::types::HewActorState::Runnable as i32,
            Ordering::Release,
        );
        // SAFETY: the fixture owns the frame and mailbox, and transfers one
        // retained reply reference to the parked actor before resuming it.
        unsafe {
            let mailbox = crate::mailbox::hew_mailbox_new();
            assert!(!mailbox.is_null());
            actor.mailbox = mailbox.cast();
            crate::reply_channel::hew_reply_channel_retain(channel);
            actor
                .suspended_reply_channel
                .store(channel.cast(), Ordering::Release);
            crate::scheduler::activate_actor_for_test(&raw mut actor);
            assert_eq!(
                actor.actor_state.load(Ordering::Acquire),
                crate::internal::types::HewActorState::Crashed as i32
            );
            assert_eq!(actor.error_code.load(Ordering::Acquire), 202);
            assert_eq!(state, 7);
            assert!(!actor.state_drop_consumed.load(Ordering::Acquire));
            assert_eq!(frame.resumes.load(Ordering::Acquire), 1);
            assert_eq!(frame.destroyed.load(Ordering::Acquire), 1);
            assert!(frame.heap_guard.load(Ordering::Acquire).is_null());
            assert!(actor.suspended_cont.load(Ordering::Acquire).is_null());
            assert!(actor
                .suspended_reply_channel
                .load(Ordering::Acquire)
                .is_null());
            assert_eq!(crate::reply_channel::ref_count_for_test(channel), 1);
            assert_eq!(
                crate::reply_channel::hew_reply_channel_failure_kind(channel),
                crate::internal::types::HEW_REPLY_FAIL_HANDLER_TRAPPED
            );
            assert_eq!(
                crate::reply_channel::hew_reply_channel_await_status(channel),
                crate::await_cancel::AwaitCancelStatus::Completed as i32
            );
            crate::reply_channel::hew_reply_channel_free(channel);
            crate::mailbox::hew_mailbox_free(mailbox);
        }
        assert_eq!(crate::reply_channel::active_channel_count(), baseline);
        assert!(crate::execution_context::current_context().is_null());
    }

    unsafe extern "C" fn resume_checked_cleanup(frame: *mut c_void) {
        // SAFETY: the fixture's scheduler owns this scratch frame and installs
        // its actor context. The first cleanup poll deliberately stays pending.
        unsafe {
            let frame = &mut *frame.cast::<crate::coro_exec::test_support::ScratchFrame>();
            let actor = &*(*crate::execution_context::current_context()).actor;
            let state = actor.checked_invocation.load(Ordering::Acquire).cast();
            assert_eq!(crate::coro_state::hew_coro_state_is_cancelled(state), 1);
            if frame.resumes.fetch_add(1, Ordering::AcqRel) == 1 {
                crate::coro_state::hew_coro_state_publish(
                    state,
                    crate::coro_state::CoroStatus::Cancelled as i32,
                );
                crate::coro_state::hew_coro_state_free(state);
                hew_actor_coro_set_fault(ptr::null_mut());
                frame.resume = None;
            }
        }
    }

    #[test]
    fn checked_stop_keeps_turn_until_pending_cleanup_finishes() {
        let _guard = crate::runtime_test_guard();
        let _scheduler = crate::scheduler::NoWorkerSchedulerForTest::install();
        let (_, waker) = crate::wake::blocking::Readiness::new();
        let mut actor = crate::test_actor::stub_actor();
        let mut frame = crate::coro_exec::test_support::ScratchFrameOwner::new(1);
        frame.resume = Some(resume_checked_cleanup);
        // SAFETY: all fixture resources remain locally owned until scheduler
        // completion; the frame relinquishes its state on its terminal poll.
        unsafe {
            let state = crate::coro_state::hew_coro_state_new(waker.descriptor(), ptr::null_mut());
            actor
                .checked_invocation
                .store(state.cast(), Ordering::Release);
            actor
                .suspended_cont
                .store(frame.handle(), Ordering::Release);
            actor.cont_tag.store(
                crate::internal::types::ContTag::Parked as i32,
                Ordering::Release,
            );
            actor.actor_state.store(
                crate::internal::types::HewActorState::Runnable as i32,
                Ordering::Release,
            );
            let mailbox = crate::mailbox::hew_mailbox_new();
            actor.mailbox = mailbox.cast();
            crate::mailbox::mailbox_close(mailbox);
            crate::mailbox::mailbox_request_stop(mailbox);
            crate::scheduler::activate_actor_for_test(&raw mut actor);
            assert_eq!(frame.resumes.load(Ordering::Acquire), 1);
            assert_eq!(frame.destroyed.load(Ordering::Acquire), 0);
            assert_eq!(
                actor.actor_state.load(Ordering::Acquire),
                crate::internal::types::HewActorState::Suspended as i32
            );
            assert!(!actor.checked_invocation.load(Ordering::Acquire).is_null());
            actor.actor_state.store(
                crate::internal::types::HewActorState::Runnable as i32,
                Ordering::Release,
            );
            crate::scheduler::activate_actor_for_test(&raw mut actor);
            assert_eq!(frame.resumes.load(Ordering::Acquire), 2);
            assert_eq!(frame.destroyed.load(Ordering::Acquire), 1);
            assert_eq!(
                actor.actor_state.load(Ordering::Acquire),
                crate::internal::types::HewActorState::Stopped as i32
            );
            assert!(actor.checked_invocation.load(Ordering::Acquire).is_null());
            assert!(actor.suspended_cont.load(Ordering::Acquire).is_null());
            crate::mailbox::hew_mailbox_free(mailbox);
        }
    }
}
