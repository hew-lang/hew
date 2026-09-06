//! Checked completion at the existing actor dispatch boundary.
//!
//! The execution context owns a returned logical fault until its scheduler
//! activation consumes it. Generated callbacks retain the nullable continuation
//! ABI and never encode a fault as a continuation or unwind a native frame.

use std::ptr;

use crate::execution_context::HewExecutionContext;
use crate::fault::HewFault;

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
            match unsafe { (*ctx).checked_fault.take() } {
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
            fault.code()
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
}
