//! Completion and cancellation boundary for checked native coroutines.
//!
//! The execution owner retains this object until the LLVM frame is destroyed.
//! Operations retain its independent wake target, never this object or a frame
//! address. Result and fault out-pointers remain owned by the invocation's
//! caller; publishing a terminal status follows initialization of those slots.

use crate::task_scope::{
    hew_cancel_observe, hew_cancel_token_cancel, hew_cancel_token_is_requested,
    hew_cancel_token_new_child, hew_cancel_token_release, hew_cancel_token_retain,
    hew_cancel_unobserve, HewCancelObserver, HewCancellationToken,
};
use crate::wake::{HewWaker, OwnedWaker};
use std::sync::atomic::{AtomicI32, Ordering};

/// ABI-visible coroutine outcome. A yielded value is available until the
/// generator's next resume; every terminal outcome requires frame destruction.
#[repr(i32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CoroStatus {
    Pending = 0,
    Complete = 1,
    Fault = 2,
    Cancelled = 3,
    Yielded = 4,
}

/// Invocation-owned state, separate from the LLVM frame and result storage.
#[derive(Debug)]
pub struct HewCoroState {
    /// Direct nested calls share their strict actor turn; fresh tasks do not.
    pub(crate) actor_turn: crate::lifetime::live_actors::ActorIncarnation,
    waker: OwnedWaker,
    token: *mut HewCancellationToken,
    enclosing_tokens: Vec<*mut HewCancellationToken>,
    observer: *mut HewCancelObserver,
    status: AtomicI32,
    private_status: AtomicI32,
}

impl Drop for HewCoroState {
    fn drop(&mut self) {
        // SAFETY: this object owns its subscription and invocation-local token.
        unsafe {
            hew_cancel_unobserve(self.observer);
            hew_cancel_token_release(self.token);
            for token in self.enclosing_tokens.drain(..) {
                hew_cancel_token_release(token);
            }
        }
    }
}

/// Create an invocation state using an existing execution owner's wake target.
///
/// # Safety
/// `waker` must point to a live descriptor obeying [`HewWaker`]'s contract.
/// `token` must be null or a live parent token. The state owns a fresh child
/// token and retains its ancestry. Free it after destroying its frame.
#[no_mangle]
pub unsafe extern "C" fn hew_coro_state_new(
    waker: *const HewWaker,
    token: *mut HewCancellationToken,
) -> *mut HewCoroState {
    if waker.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: the caller keeps the descriptor and token live during creation.
    let retained = unsafe { OwnedWaker::retain(&*waker) };
    // SAFETY: the caller keeps the parent token live during child creation.
    let token = unsafe { hew_cancel_token_new_child(token) };
    // SAFETY: token and the retained descriptor are live for registration.
    let observer = unsafe { hew_cancel_observe(token, retained.descriptor()) };
    Box::into_raw(Box::new(HewCoroState {
        actor_turn: crate::lifetime::live_actors::ActorIncarnation::NONE,
        waker: retained,
        token,
        enclosing_tokens: Vec::new(),
        observer,
        status: AtomicI32::new(CoroStatus::Pending as i32),
        private_status: AtomicI32::new(1),
    }))
}

/// Release an invocation state after its frame and operation handles.
///
/// # Safety
/// `state` must be null or an owned state returned by [`hew_coro_state_new`].
/// No execution or accessor may use it concurrently or after this call.
#[no_mangle]
pub unsafe extern "C" fn hew_coro_state_free(state: *mut HewCoroState) {
    if !state.is_null() {
        // SAFETY: the caller transfers the sole owning state handle.
        drop(unsafe { Box::from_raw(state) });
    }
}

/// Create a nested invocation using the current frame's wake target and token.
///
/// # Safety
/// `parent` must be a live state. The caller owns the new state until its nested
/// frame has been destroyed and then releases it with [`hew_coro_state_free`].
#[no_mangle]
pub unsafe extern "C" fn hew_coro_state_child(parent: *const HewCoroState) -> *mut HewCoroState {
    if parent.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: parent retains both inputs throughout child construction.
    let parent = unsafe { &*parent };
    // SAFETY: parent retains both inputs for child construction.
    let child = unsafe { hew_coro_state_new(parent.waker.descriptor(), parent.token) };
    // SAFETY: the new child is exclusively owned and its parent remains live.
    unsafe { (*child).actor_turn = parent.actor_turn };
    child
}

/// Request cancellation of this invocation and its descendants.
/// Siblings and the parent retain their independent cancellation state.
///
/// # Safety
/// `state` must be a live invocation state through this call.
#[no_mangle]
pub unsafe extern "C" fn hew_coro_state_cancel(state: *const HewCoroState) {
    if !state.is_null() {
        // SAFETY: the caller keeps the state and its owned token live.
        unsafe { hew_cancel_token_cancel((*state).token, 1) };
    }
}

/// Borrow the invocation's token when creating a nested structured scope.
///
/// # Safety
/// `state` must remain live for the returned borrow. The scope must retain the
/// token or create its child before returning control to the caller.
#[no_mangle]
pub unsafe extern "C" fn hew_coro_state_token(
    state: *const HewCoroState,
) -> *mut HewCancellationToken {
    if state.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: the caller keeps the state live for this borrow.
    unsafe { (*state).token }
}

/// Enter a lexical cancellation boundary for this invocation's operations.
///
/// # Safety
/// State is exclusively borrowed by its executing frame. Token is a live
/// descendant of the current token. Every entry is paired with a leave after
/// the scope's children and operation producers have drained.
#[no_mangle]
pub unsafe extern "C" fn hew_coro_state_enter_token(
    state: *mut HewCoroState,
    token: *mut HewCancellationToken,
) {
    // SAFETY: the frame exclusively owns state and retains token during entry.
    unsafe {
        let state = &mut *state;
        hew_cancel_token_retain(token);
        hew_cancel_unobserve(state.observer);
        state.enclosing_tokens.push(state.token);
        state.token = token;
        state.observer = hew_cancel_observe(token, state.waker.descriptor());
    }
}

/// Restore the enclosing cancellation boundary after lexical cleanup.
///
/// # Safety
/// State is exclusively borrowed by its executing frame and has a matching
/// token entry. No operation still borrows the token being left.
#[no_mangle]
pub unsafe extern "C" fn hew_coro_state_leave_token(state: *mut HewCoroState) {
    // SAFETY: caller guarantees balanced entries and completed scope cleanup.
    unsafe {
        let state = &mut *state;
        let Some(token) = state.enclosing_tokens.pop() else {
            std::process::abort();
        };
        hew_cancel_unobserve(state.observer);
        hew_cancel_token_release(state.token);
        state.token = token;
        state.observer = hew_cancel_observe(token, state.waker.descriptor());
    }
}

/// Borrow the readiness descriptor for registration of an operation.
///
/// # Safety
/// `state` must be live. The returned pointer is borrowed until state teardown;
/// an operation must retain the descriptor before returning to its caller.
#[no_mangle]
pub unsafe extern "C" fn hew_coro_state_waker(state: *const HewCoroState) -> *const HewWaker {
    if state.is_null() {
        return std::ptr::null();
    }
    // SAFETY: caller keeps state live for the borrow.
    unsafe { &*state }.waker.descriptor()
}

/// Read the retained cancellation token at a compiler-authored safepoint.
///
/// # Safety
/// `state` must be a live invocation state.
#[no_mangle]
pub unsafe extern "C" fn hew_coro_state_is_cancelled(state: *const HewCoroState) -> i32 {
    if state.is_null() {
        return 1;
    }
    // SAFETY: the state's subscription keeps its token and ancestors live.
    let token = unsafe { &*state }.token;
    if token.is_null() {
        0
    } else {
        // SAFETY: the state retains this non-null token through its observer.
        unsafe { hew_cancel_token_is_requested(token) }
    }
}

/// Classify cancellation at the current lexical boundary for fault transport.
///
/// # Safety
/// `state` is a live invocation state. Call after observing cancellation, or
/// while cooperatively destroying its frame.
#[no_mangle]
pub unsafe extern "C" fn hew_coro_state_cancel_code(state: *const HewCoroState) -> i32 {
    // SAFETY: the live state's retained token owns its complete ancestry.
    let reason = unsafe { crate::task_scope::cancel_token_reason((*state).token) };
    match reason {
        crate::fault::HEW_FAULT_DEADLINE | crate::fault::HEW_FAULT_RACE_LOST => reason,
        _ => crate::fault::HEW_FAULT_CANCELLED,
    }
}

/// Read the outcome after a ramp/resume returns to its execution owner.
///
/// # Safety
/// `state` must be a live invocation state. Terminal status publishes the
/// caller-owned result/fault slots, which must remain live for this invocation.
#[no_mangle]
pub unsafe extern "C" fn hew_coro_state_status(state: *const HewCoroState) -> i32 {
    if state.is_null() {
        return -1;
    }
    // SAFETY: state is live; Acquire observes preceding result writes.
    unsafe { &*state }.status.load(Ordering::Acquire)
}

/// Publish a checked function's existing private status after writing its
/// result or owned fault output. Returns 0 on repeated completion.
///
/// # Safety
/// The caller must exclusively own execution of a live `state`, and have
/// initialized its result (zero status) or owned fault (non-zero status).
#[no_mangle]
pub unsafe extern "C" fn hew_coro_state_finish(
    state: *mut HewCoroState,
    private_status: i32,
) -> i32 {
    if state.is_null() {
        return 0;
    }
    // SAFETY: the caller owns execution and keeps state live.
    let state_ref = unsafe { &*state };
    if state_ref.status.load(Ordering::Acquire) != CoroStatus::Pending as i32 {
        return 0;
    }
    state_ref
        .private_status
        .store(private_status, Ordering::Relaxed);
    // SAFETY: output initialization and exclusive execution are caller duties.
    unsafe {
        hew_coro_state_publish(
            state,
            if private_status == 0 {
                CoroStatus::Complete as i32
            } else if private_status == crate::fault::HEW_FAULT_CANCELLED {
                CoroStatus::Cancelled as i32
            } else {
                CoroStatus::Fault as i32
            },
        )
    }
}

/// Recover the checked private status after observing terminal completion.
///
/// # Safety
/// `state` must be live and its ramp/resume must have returned terminally.
#[no_mangle]
pub unsafe extern "C" fn hew_coro_state_private_status(state: *const HewCoroState) -> i32 {
    if state.is_null() {
        return 1;
    }
    // SAFETY: caller retains state and observes it only after execution returns.
    let state = unsafe { &*state };
    match state.status.load(Ordering::Acquire) {
        status if status == CoroStatus::Complete as i32 => 0,
        status if status == CoroStatus::Fault as i32 || status == CoroStatus::Cancelled as i32 => {
            state.private_status.load(Ordering::Relaxed)
        }
        _ => 1,
    }
}

/// Publish a yielded value or a terminal result after initializing its output.
/// Returns 0 for an invalid status or a second terminal publication. A generator
/// must reset Yielded to Pending before executing another resume.
///
/// # Safety
/// `state` must be live and the caller must own execution of its frame. The
/// corresponding output must be initialized before publication and cannot be
/// accessed concurrently before the owner's ramp/resume has returned.
#[no_mangle]
pub unsafe extern "C" fn hew_coro_state_publish(state: *mut HewCoroState, status: i32) -> i32 {
    if state.is_null() || !(1..=4).contains(&status) {
        return 0;
    }
    // SAFETY: caller owns execution; CAS rejects repeated publication.
    i32::from(
        unsafe { &*state }
            .status
            .compare_exchange(
                CoroStatus::Pending as i32,
                status,
                Ordering::Release,
                Ordering::Relaxed,
            )
            .is_ok(),
    )
}

/// Reset a consumed generator yield before resuming its frame.
///
/// # Safety
/// `state` must be live, with exclusive execution ownership. Its yielded value
/// must have been taken or destroyed before this call.
#[no_mangle]
pub unsafe extern "C" fn hew_coro_state_resume_yield(state: *mut HewCoroState) -> i32 {
    if state.is_null() {
        return 0;
    }
    // SAFETY: caller owns the yield and the next resume.
    i32::from(
        unsafe { &*state }
            .status
            .compare_exchange(
                CoroStatus::Yielded as i32,
                CoroStatus::Pending as i32,
                Ordering::AcqRel,
                Ordering::Acquire,
            )
            .is_ok(),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::wake::blocking::Readiness;

    #[test]
    fn lexical_cancellation_reaches_nested_calls_then_restores_parent() {
        let (_ready, waker) = Readiness::new();
        // SAFETY: locally owned invocation and lexical tokens have balanced
        // references; nested calls stop before their enclosing scope is left.
        unsafe {
            let state = hew_coro_state_new(waker.descriptor(), std::ptr::null_mut());
            let parent = hew_coro_state_token(state);
            let lexical = hew_cancel_token_new_child(parent);
            hew_coro_state_enter_token(state, lexical);
            let child = hew_coro_state_child(state);
            hew_cancel_token_cancel(lexical, crate::fault::HEW_FAULT_DEADLINE);
            hew_cancel_token_cancel(lexical, 1);
            assert_eq!(hew_coro_state_is_cancelled(state), 1);
            assert_eq!(hew_coro_state_is_cancelled(child), 1);
            assert_eq!(
                hew_coro_state_cancel_code(child),
                crate::fault::HEW_FAULT_DEADLINE
            );
            assert_eq!(hew_cancel_token_is_requested(parent), 0);
            hew_coro_state_free(child);
            hew_coro_state_leave_token(state);
            hew_cancel_token_release(lexical);
            assert_eq!(hew_coro_state_is_cancelled(state), 0);
            hew_cancel_token_cancel(parent, 1);
            assert_eq!(hew_coro_state_is_cancelled(state), 1);
            assert_eq!(
                hew_coro_state_cancel_code(state),
                crate::fault::HEW_FAULT_CANCELLED
            );
            hew_coro_state_free(state);
        }
    }

    #[test]
    fn cancelled_invocation_keeps_token_alive_and_late_wake_survives_teardown() {
        let (ready, waker) = Readiness::new();
        // SAFETY: all invocation handles are locally owned; the operation
        // retains the independent wake descriptor before state teardown.
        unsafe {
            let token = hew_cancel_token_new_child(std::ptr::null_mut());
            let state = hew_coro_state_new(waker.descriptor(), token);
            let operation = OwnedWaker::retain(&*hew_coro_state_waker(state));
            hew_cancel_token_cancel(token, 1);
            hew_cancel_token_release(token);
            assert!(ready.take_ready());
            assert_eq!(hew_coro_state_is_cancelled(state), 1);
            assert_eq!(
                hew_coro_state_publish(state, CoroStatus::Cancelled as i32),
                1
            );
            hew_coro_state_free(state);
            operation.wake();
            assert!(ready.take_ready());
        }
    }

    #[test]
    fn child_cancellation_preserves_siblings_and_parent_cancellation_reaches_all() {
        let (_, waker) = Readiness::new();
        // SAFETY: the test owns all invocation states and releases children first.
        unsafe {
            let parent = hew_coro_state_new(waker.descriptor(), std::ptr::null_mut());
            let first = hew_coro_state_child(parent);
            let second = hew_coro_state_child(parent);
            let grandchild = hew_coro_state_child(first);
            hew_coro_state_cancel(first);
            assert_eq!(hew_coro_state_is_cancelled(first), 1);
            assert_eq!(hew_coro_state_is_cancelled(grandchild), 1);
            assert_eq!(hew_coro_state_is_cancelled(parent), 0);
            assert_eq!(hew_coro_state_is_cancelled(second), 0);
            hew_coro_state_free(grandchild);
            hew_coro_state_free(first);
            hew_coro_state_cancel(parent);
            assert_eq!(hew_coro_state_is_cancelled(second), 1);
            hew_coro_state_free(second);
            hew_coro_state_free(parent);
        }
    }

    #[test]
    fn terminal_completion_cannot_be_republished_or_resumed_as_a_yield() {
        let (_, waker) = Readiness::new();
        // SAFETY: locally owned invocation and valid descriptor.
        unsafe {
            let state = hew_coro_state_new(waker.descriptor(), std::ptr::null_mut());
            assert_eq!(hew_coro_state_publish(state, CoroStatus::Yielded as i32), 1);
            assert_eq!(
                hew_coro_state_publish(state, CoroStatus::Complete as i32),
                0
            );
            assert_eq!(hew_coro_state_resume_yield(state), 1);
            assert_eq!(
                hew_coro_state_publish(state, CoroStatus::Complete as i32),
                1
            );
            assert_eq!(hew_coro_state_resume_yield(state), 0);
            assert_eq!(hew_coro_state_publish(state, CoroStatus::Fault as i32), 0);
            assert_eq!(hew_coro_state_status(state), CoroStatus::Complete as i32);
            hew_coro_state_free(state);
        }
    }
}
