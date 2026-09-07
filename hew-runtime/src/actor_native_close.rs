//! Actor termination observers outlive the incarnation's allocation.

use crate::actor::{HewActor, HewDispatchOwnership};
use crate::internal::types::HewActorState;
use crate::lifetime::{live_actors, local_handles};
use crate::wake::{HewWaker, OwnedWaker, ReadinessRegistrations};
use std::sync::atomic::{AtomicI32, AtomicU8, Ordering};
use std::sync::Arc;

/// Completion is published only after the checked turn and typed state cleanup.
/// Phase 1 reserves the one terminal cleanup owner; phase 2 publishes its result.
#[derive(Debug, Default)]
pub struct NativeActorCompletion {
    phase: AtomicU8,
    code: AtomicI32,
    ready: ReadinessRegistrations,
}

/// Finish native state ownership before any termination observer can continue.
///
/// # Safety
/// The caller owns terminal cleanup or the completed scheduler activation. No
/// checked frame may still borrow the state. Other calls only observe the claim.
pub(crate) unsafe fn finish_native_terminal(actor: &HewActor) {
    let Some(completion) = &actor.native_completion else {
        return;
    };
    let state = actor.actor_state.load(Ordering::Acquire);
    if !matches!(state, s if s == HewActorState::Stopped as i32 || s == HewActorState::Crashed as i32)
        || !actor.checked_invocation.load(Ordering::Acquire).is_null()
        || (state == HewActorState::Stopped as i32
            && !actor.terminate_finished.load(Ordering::Acquire))
        || completion
            .phase
            .compare_exchange(0, 1, Ordering::AcqRel, Ordering::Acquire)
            .is_err()
    {
        return;
    }
    debug_assert_eq!(
        actor.dispatch_ownership,
        HewDispatchOwnership::UniqueEnvelope
    );
    // SAFETY: this terminal owner has reserved completion before taking the
    // actor's existing exactly-once state destructor authority.
    unsafe { crate::actor::drop_initialized_actor_state(actor) };
    completion
        .code
        .store(actor.error_code.load(Ordering::Acquire), Ordering::Relaxed);
    completion.phase.store(2, Ordering::Release);
    completion.ready.notify();
}

/// Request cooperative stop through a stable actor identity.
#[no_mangle]
pub extern "C" fn hew_actor_close_native(token: local_handles::HewLocalPidId) {
    if let Some(id) = local_handles::resolve_current_actor(token) {
        live_actors::with_actor_send_by_id(id, |actor| {
            // SAFETY: the guard pins the actor while the request is latched.
            unsafe { crate::actor::hew_actor_stop(actor) };
        });
    }
}

#[derive(Debug)]
pub struct HewNativeActorWait {
    completion: Option<Arc<NativeActorCompletion>>,
    _waker: Arc<OwnedWaker>,
}

/// Register before checking completion so a terminal publication cannot pass
/// between observation and parking. No actor allocation is retained by the wait.
///
/// # Safety
/// The descriptor obeys the retained native wake contract.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_wait_new(
    token: local_handles::HewLocalPidId,
    waker: *const HewWaker,
) -> *mut HewNativeActorWait {
    // SAFETY: the caller keeps the descriptor live during retention.
    let waker = Arc::new(unsafe { OwnedWaker::retain(&*waker) });
    let completion = local_handles::resolve_current_actor(token).and_then(|id| {
        live_actors::with_actor_send_by_id(id, |actor| {
            // SAFETY: the send guard pins this exact actor during registration.
            let completion = unsafe { &*actor }.native_completion.clone();
            if let Some(completion) = &completion {
                completion.ready.register(&waker);
            }
            completion
        })
        .flatten()
    });
    Box::into_raw(Box::new(HewNativeActorWait {
        completion,
        _waker: waker,
    }))
}

/// Returns 0 while pending, 1 after clean termination, or 2 after a failure.
///
/// # Safety
/// The caller uniquely owns the wait until it detaches or consumes completion.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_wait_poll(wait: *const HewNativeActorWait) -> i32 {
    // SAFETY: the operation remains live during this poll.
    let wait = unsafe { &*wait };
    match &wait.completion {
        Some(completion) if completion.phase.load(Ordering::Acquire) != 2 => 0,
        Some(completion) if completion.code.load(Ordering::Relaxed) != 0 => 2,
        _ => 1,
    }
}

/// Read the terminal logical failure code after a failed poll.
///
/// # Safety
/// `wait` is live and the caller has acquired its completed failure.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_wait_error(wait: *const HewNativeActorWait) -> i32 {
    // SAFETY: the caller retains the immutable result descriptor.
    unsafe { &*wait }
        .completion
        .as_ref()
        .map_or(0, |c| c.code.load(Ordering::Relaxed))
}

/// Detach the observer without revoking any target actor cleanup.
///
/// # Safety
/// The caller transfers the unique wait owner; no polls may remain in flight.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_wait_free(wait: *mut HewNativeActorWait) {
    // SAFETY: the unique wait owner is relinquished here.
    drop(unsafe { Box::from_raw(wait) });
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::AtomicUsize;
    struct State(Arc<AtomicUsize>);
    impl Drop for State {
        fn drop(&mut self) {
            self.0.fetch_add(1, Ordering::SeqCst);
        }
    }
    unsafe extern "C" fn drop_state(state: *mut std::ffi::c_void) {
        // SAFETY: the actor owns one initialized State in the supplied wrapper.
        unsafe { std::ptr::drop_in_place(state.cast::<State>()) };
    }
    #[test]
    fn terminal_observer_waits_for_state_release_and_detaches_without_cancelling_cleanup() {
        for detach in [false, true] {
            let (ready, waker) = crate::wake::blocking::Readiness::new();
            let drops = Arc::new(AtomicUsize::new(0));
            let mut storage = std::mem::MaybeUninit::new(State(drops.clone()));
            let mut actor = crate::test_actor::stub_actor();
            actor.state = storage.as_mut_ptr().cast();
            actor.state_drop_fn = Some(drop_state);
            actor.dispatch_ownership = HewDispatchOwnership::UniqueEnvelope;
            let completion = Arc::new(NativeActorCompletion::default());
            actor.native_completion = Some(completion.clone());
            let waker = Arc::new(waker);
            completion.ready.register(&waker);
            let wait = Box::into_raw(Box::new(HewNativeActorWait {
                completion: Some(completion),
                _waker: waker,
            }));
            actor
                .actor_state
                .store(HewActorState::Stopped as i32, Ordering::Release);
            // SAFETY: the fixture owns all state and completion references.
            unsafe {
                finish_native_terminal(&actor);
                assert_eq!(hew_actor_wait_poll(wait), 0);
                assert_eq!(drops.load(Ordering::SeqCst), 0);
                if detach {
                    hew_actor_wait_free(wait);
                }
                actor.terminate_finished.store(true, Ordering::Release);
                finish_native_terminal(&actor);
                finish_native_terminal(&actor);
                assert_eq!(drops.load(Ordering::SeqCst), 1);
                drop(actor);
                assert_eq!(ready.take_ready(), !detach);
                if !detach {
                    assert_eq!(hew_actor_wait_poll(wait), 1);
                    hew_actor_wait_free(wait);
                }
                assert_eq!(Arc::strong_count(&ready), 1);
            }
        }
    }
}
