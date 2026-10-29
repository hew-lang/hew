//! The shared process-root and asynchronous-host frame ownership boundary.

use crate::cont::{hew_cont_destroy, hew_cont_done, hew_cont_resume};
use crate::coro_state::{
    hew_coro_state_private_status, hew_coro_state_status, CoroStatus, HewCoroState,
};
use std::ffi::c_void;

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
