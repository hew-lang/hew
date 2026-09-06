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

/// Transfer an initialized generated message through the existing mailbox.
///
/// # Safety
/// `payload` is a uniquely owned malloc allocation, initialized according to
/// `drop_payload`. The caller relinquishes it on every outcome. `fault` is a
/// writable, initially null fault slot.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_send_native(
    token: crate::lifetime::local_handles::HewLocalPidId,
    message: i32,
    payload: *mut std::ffi::c_void,
    size: usize,
    drop_payload: crate::mailbox::HewMsgEnvelopeDropFn,
    fault: *mut *mut HewFault,
) -> i32 {
    // SAFETY: ownership and the destructor pass unchanged to the envelope.
    let envelope =
        unsafe { crate::mailbox::hew_msg_envelope_new(payload, size, Some(drop_payload)) };
    let status = if envelope.is_null() {
        // SAFETY: failed envelope allocation leaves payload ownership here.
        unsafe {
            drop_payload(payload);
            libc::free(payload);
        }
        crate::internal::types::HewError::ErrOom as i32
    } else {
        // SAFETY: the newly allocated envelope transfers one reference.
        unsafe { crate::actor::send_native_envelope(token, message, envelope) }
    };
    if status != 0 {
        // SAFETY: the generated caller supplies an empty writable fault slot.
        unsafe { *fault = crate::fault::hew_fault_new(status) };
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
}
