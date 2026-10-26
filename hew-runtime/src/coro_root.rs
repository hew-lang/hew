//! Drive a process root task using the shared continuation and wake substrate.
//! Scheduler startup, actor draining and shutdown belong to the process entry.

use crate::cancel_token::{
    hew_cancel_token_new_child, hew_cancel_token_release, HewCancellationToken,
};
use crate::cont::{hew_cont_destroy, hew_cont_done, hew_cont_resume};
use crate::coro_state::{
    hew_coro_state_free, hew_coro_state_new, hew_coro_state_private_status, hew_coro_state_status,
    CoroStatus, HewCoroState,
};
use crate::execution_context::{current_context, set_current_context, HewExecutionContext};
use crate::fault::HewFault;
use crate::wake::blocking::Readiness;
use crate::wake::HewWaker;
use std::cell::Cell;
use std::ffi::c_void;

thread_local! {
    /// The context of the root task this thread is driving, or null.
    static ROOT_CONTEXT: Cell<*mut HewExecutionContext> = const { Cell::new(std::ptr::null_mut()) };
}

/// Whether the running code is a process root task's own body, which this
/// thread resumes directly: no scheduler worker is needed to wake it.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn current_context_is_process_root() -> bool {
    let context = current_context();
    !context.is_null() && ROOT_CONTEXT.with(Cell::get) == context
}

/// Run `body` as the body of a process root task on this thread.
#[cfg(test)]
pub(crate) fn as_process_root_for_test<R>(body: impl FnOnce() -> R) -> R {
    let mut context = HewExecutionContext::default();
    let previous = set_current_context(&raw mut context);
    let enclosing_root = ROOT_CONTEXT.with(|root| root.replace(&raw mut context));
    let result = body();
    ROOT_CONTEXT.with(|root| root.set(enclosing_root));
    let _ = set_current_context(previous);
    result
}

/// A generated adapter invokes the exact typed main body. Synchronous bodies
/// publish their checked outcome and return null; resumable bodies return the
/// actual LLVM continuation, including when they completed during the ramp.
pub type HewRootStart = unsafe extern "C" fn(
    *mut c_void,
    *mut c_void,
    *mut *mut HewFault,
    *mut HewCoroState,
) -> *mut c_void;

/// Stable execution storage for a root or host invocation. Each entry restores
/// the surrounding thread context before returning to its driver.
#[derive(Debug)]
pub(crate) struct RootExecution {
    context: HewExecutionContext,
    state: *mut HewCoroState,
    token: *mut HewCancellationToken,
}

impl RootExecution {
    /// # Safety
    /// The wake descriptor obeys its retained-callback lifetime contract.
    /// Destroy every child frame and operation before dropping this execution.
    pub(crate) unsafe fn new(waker: &HewWaker) -> Self {
        let previous = current_context();
        let parent_token = if previous.is_null() {
            std::ptr::null_mut()
        } else {
            // SAFETY: the enclosing invocation remains live during construction.
            unsafe { (*previous).cancel_token }
        };
        // SAFETY: the optional parent token is borrowed from that live context.
        let token = unsafe { hew_cancel_token_new_child(parent_token) };
        let context = HewExecutionContext {
            cancel_token: token,
            ..HewExecutionContext::default()
        };
        // SAFETY: this execution owns its token and retains the wake descriptor.
        let state = unsafe { hew_coro_state_new(waker, token) };
        Self {
            context,
            state,
            token,
        }
    }

    /// Enter for one start, resume or cleanup poll. No reference to the thread's
    /// enclosing context is kept while control is back in the host event loop.
    pub(crate) fn enter<R>(&mut self, run: impl FnOnce(*mut HewCoroState) -> R) -> R {
        struct RestoreContext {
            previous: *mut HewExecutionContext,
            enclosing_root: *mut HewExecutionContext,
        }
        impl Drop for RestoreContext {
            fn drop(&mut self) {
                ROOT_CONTEXT.with(|root| root.set(self.enclosing_root));
                let _installed = set_current_context(self.previous);
            }
        }
        let previous = current_context();
        self.context.prev_context = previous;
        let context = &raw mut self.context;
        let _previous = set_current_context(context);
        let enclosing_root = ROOT_CONTEXT.with(|root| root.replace(context));
        let restore = RestoreContext {
            previous,
            enclosing_root,
        };
        let result = run(self.state);
        drop(restore);
        self.context.prev_context = std::ptr::null_mut();
        result
    }

    pub(crate) fn pending(&self) -> bool {
        // SAFETY: this execution owns its live invocation record.
        (unsafe { hew_coro_state_status(self.state) }) == CoroStatus::Pending as i32
    }

    pub(crate) fn cancel(&self) {
        if self.pending() {
            // SAFETY: this execution owns the ancestor token and its child state.
            // Cancelling the ancestor also covers work launched from its carrier.
            unsafe {
                crate::cancel_token::hew_cancel_token_cancel(
                    self.token,
                    crate::fault::HEW_FAULT_CANCELLED,
                );
                crate::coro_state::hew_coro_state_cancel(self.state);
            }
        }
    }
}

impl Drop for RootExecution {
    fn drop(&mut self) {
        // SAFETY: the driver destroys every frame and operation before dropping
        // this execution storage, just as at the synchronous root boundary.
        unsafe {
            hew_coro_state_free(self.state);
            hew_cancel_token_release(self.token);
        }
    }
}

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
    // SAFETY: the root closure drains and destroys its frame before returning.
    unsafe {
        with_root_invocation(|state, readiness| {
            drive_root(start, arguments, result_out, fault_out, state, readiness)
        })
    }
}

/// Enter the shared host/root context until all invocation children are drained.
/// # Safety
/// `run` must finish all child frames before returning and must not unwind.
pub(crate) unsafe fn with_root_invocation(
    run: impl FnOnce(*mut HewCoroState, &Readiness) -> i32,
) -> i32 {
    let (readiness, waker) = Readiness::new();
    // SAFETY: readiness owns the retained wake target throughout the invocation.
    let mut execution = unsafe { RootExecution::new(waker.descriptor()) };
    execution.enter(|state| run(state, &readiness))
}

unsafe fn drive_root(
    start: HewRootStart,
    arguments: *mut c_void,
    result_out: *mut c_void,
    fault_out: *mut *mut HewFault,
    state: *mut HewCoroState,
    readiness: &Readiness,
) -> i32 {
    // SAFETY: caller supplies the generated adapter and correctly typed slots.
    let mut frame = unsafe { start(arguments, result_out, fault_out, state) };
    let mut ready = false;
    loop {
        // SAFETY: this driver owns the frame/state and consumes one wake per poll.
        if let Some(status) = unsafe { poll_root(&mut frame, state, ready) } {
            return status;
        }
        readiness.wait();
        ready = true;
    }
}

/// Poll the existing root frame after consuming readiness. Terminal cleanup and
/// frame destruction are shared by blocking and asynchronous hosting drivers.
/// # Safety
/// The driver exclusively owns `frame` and `state`, has installed their root
/// execution context, and keeps all typed input/output storage live.
pub(crate) unsafe fn poll_root(
    frame: &mut *mut c_void,
    state: *mut HewCoroState,
    ready: bool,
) -> Option<i32> {
    // SAFETY: the owning driver retains its live invocation state.
    if unsafe { hew_coro_state_status(state) } == CoroStatus::Pending as i32 {
        // SAFETY: a non-null frame is still owned by this invocation.
        if frame.is_null() || unsafe { hew_cont_done(*frame) } {
            // A generated body must publish its outcome before final suspend.
            std::process::abort();
        }
        if !ready {
            return None;
        }
        // SAFETY: frame is live, suspended and exclusively driven here.
        unsafe { hew_cont_resume(*frame) };
        // SAFETY: a pending invocation retains its suspended frame and state.
        if unsafe { hew_coro_state_status(state) } == CoroStatus::Pending as i32 {
            return None;
        }
    }
    // SAFETY: the state remains owned until after frame destruction below.
    if unsafe { hew_coro_state_status(state) } == CoroStatus::Yielded as i32 {
        // A process root is not a generator consumer.
        std::process::abort();
    }
    if !frame.is_null() {
        // SAFETY: the live frame is exclusively owned, including when complete.
        if !unsafe { hew_cont_done(*frame) } {
            std::process::abort();
        }
        // SAFETY: final-suspended frame has discharged its typed cleanup.
        unsafe { hew_cont_destroy(*frame) };
        *frame = std::ptr::null_mut();
    }
    // SAFETY: completion was observed above and the state is still owned here.
    Some(unsafe { hew_coro_state_private_status(state) })
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

#[cfg(all(test, not(target_arch = "wasm32")))]
mod tests {
    use super::*;

    #[test]
    fn root_execution_reenters_without_retaining_an_enclosing_host_context() {
        let previous = current_context();
        let mut first = HewExecutionContext::default();
        let mut second = HewExecutionContext::default();
        let (_, waker) = Readiness::new();
        // SAFETY: the wake owner remains live and this test creates no frames.
        let mut execution = unsafe { RootExecution::new(waker.descriptor()) };
        let _ = set_current_context(&raw mut first);
        let state = execution.enter(|state| {
            assert!(current_context_is_process_root());
            // SAFETY: enter installed the live, exclusively borrowed root context.
            let enclosing = unsafe { (*current_context()).prev_context };
            assert_eq!(enclosing, &raw mut first);
            state
        });
        assert_eq!(current_context(), &raw mut first);
        assert!(!current_context_is_process_root());
        assert!(execution.context.prev_context.is_null());
        let _ = set_current_context(&raw mut second);
        execution.enter(|next| {
            assert_eq!(state, next);
            // SAFETY: this entry borrows the second live enclosing host context.
            let enclosing = unsafe { (*current_context()).prev_context };
            assert_eq!(enclosing, &raw mut second);
        });
        assert_eq!(current_context(), &raw mut second);
        let _ = set_current_context(previous);
    }
}
