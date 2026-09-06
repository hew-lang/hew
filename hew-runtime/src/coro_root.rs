//! Drive a process root task using the shared continuation and wake substrate.
//! Scheduler startup, actor draining and shutdown belong to the process entry.

use crate::cont::{hew_cont_destroy, hew_cont_done, hew_cont_resume};
use crate::coro_state::{
    hew_coro_state_free, hew_coro_state_new, hew_coro_state_private_status, hew_coro_state_status,
    CoroStatus, HewCoroState,
};
use crate::execution_context::{current_context, set_current_context, HewExecutionContext};
use crate::fault::HewFault;
use crate::task_scope::{hew_cancel_token_new_child, hew_cancel_token_release};
use crate::wake::blocking::Readiness;
use std::ffi::c_void;

/// A generated adapter invokes the exact typed main body. Synchronous bodies
/// publish their checked outcome and return null; resumable bodies return the
/// actual LLVM continuation, including when they completed during the ramp.
pub type HewRootStart = unsafe extern "C" fn(
    *mut c_void,
    *mut c_void,
    *mut *mut HewFault,
    *mut HewCoroState,
) -> *mut c_void;

/// Run one checked root task on the process thread. It installs a task context,
/// retains the parent cancellation ancestry, and restores the previous context
/// after typed completion and frame destruction. It never starts a scheduler.
///
/// # Safety
/// `start` must obey [`HewRootStart`]'s generated adapter contract and never
/// unwind. `result_out` must have the exact main return layout (or be null for
/// unit); `fault_out` must be a writable pointer slot. Output storage stays live
/// until this call returns. This function must not run on an actor worker.
#[no_mangle]
pub unsafe extern "C" fn hew_coro_run_root(
    start: HewRootStart,
    arguments: *mut c_void,
    result_out: *mut c_void,
    fault_out: *mut *mut HewFault,
) -> i32 {
    let previous = current_context();
    let parent_token = if previous.is_null() {
        std::ptr::null_mut()
    } else {
        // SAFETY: the installed previous context is live on this thread.
        unsafe { (*previous).cancel_token }
    };
    // SAFETY: parent token is borrowed from the live previous context.
    let token = unsafe { hew_cancel_token_new_child(parent_token) };
    let mut context = HewExecutionContext {
        cancel_token: token,
        prev_context: previous,
        ..HewExecutionContext::default()
    };
    let (readiness, waker) = Readiness::new();
    // SAFETY: descriptor and token are owned by this root invocation.
    let state = unsafe { hew_coro_state_new(waker.descriptor(), token) };
    let _previous = set_current_context(&raw mut context);
    // SAFETY: caller supplies the generated adapter and correctly typed slots.
    let frame = unsafe { start(arguments, result_out, fault_out, state) };
    // SAFETY: this function owns both invocation state and returned frame.
    while unsafe { hew_coro_state_status(state) } == CoroStatus::Pending as i32 {
        // SAFETY: a non-null frame is still owned by this invocation.
        if frame.is_null() || unsafe { hew_cont_done(frame) } {
            // A generated body must publish its outcome before final suspend.
            std::process::abort();
        }
        readiness.wait();
        // SAFETY: frame is live, suspended and exclusively driven here.
        unsafe { hew_cont_resume(frame) };
    }
    // SAFETY: the state remains owned until after frame destruction below.
    if unsafe { hew_coro_state_status(state) } == CoroStatus::Yielded as i32 {
        // A process root is not a generator consumer.
        std::process::abort();
    }
    if !frame.is_null() {
        // SAFETY: the live frame is exclusively owned, including when complete.
        if !unsafe { hew_cont_done(frame) } {
            std::process::abort();
        }
        // SAFETY: final-suspended frame has discharged its typed cleanup.
        unsafe { hew_cont_destroy(frame) };
    }
    // SAFETY: completion was observed above and the state is still owned here.
    let status = unsafe { hew_coro_state_private_status(state) };
    let _installed = set_current_context(previous);
    // SAFETY: no frame accesses state; drop subscription before root token.
    unsafe {
        hew_coro_state_free(state);
        hew_cancel_token_release(token);
    }
    status
}

/// Drive an erased callable at an explicit synchronous hosting boundary.
///
/// # Safety
/// The invocation adapter, environment, argument pointers and output slots must
/// obey `HewCallableInvoke`'s exact typed ownership contract. As with the process
/// root driver, this function must not run on an actor worker.
#[no_mangle]
pub unsafe extern "C" fn hew_coro_run_callable(
    invoke: hew_cabi::callable::HewCallableInvoke,
    environment: *mut c_void,
    argument_slots: *const *mut c_void,
    result_out: *mut c_void,
    fault_out: *mut *mut c_void,
) -> i32 {
    struct Arguments {
        invoke: hew_cabi::callable::HewCallableInvoke,
        environment: *mut c_void,
        slots: *const *mut c_void,
    }
    unsafe extern "C" fn start(
        arguments: *mut c_void,
        result: *mut c_void,
        fault: *mut *mut HewFault,
        state: *mut HewCoroState,
    ) -> *mut c_void {
        // SAFETY: the hosting call retains this exact argument bundle and all
        // invocation inputs until the shared root driver finishes.
        unsafe {
            let args = &*arguments.cast::<Arguments>();
            (args.invoke)(
                args.environment,
                args.slots,
                result,
                fault.cast(),
                state.cast(),
            )
        }
    }
    let mut args = Arguments {
        invoke,
        environment,
        slots: argument_slots,
    };
    // SAFETY: the caller supplies the invocation contract; the bundle remains
    // on this stack until the shared driver has destroyed the finished frame.
    unsafe { hew_coro_run_root(start, (&raw mut args).cast(), result_out, fault_out.cast()) }
}
