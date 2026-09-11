//! Local termination observers outlive the actor or supervisor allocation.

use crate::actor::{HewActor, HewDispatchOwnership};
use crate::internal::types::HewActorState;
use crate::lifetime::{live_actors, local_handles};
use crate::wake::{HewWaker, OwnedWaker, ReadinessRegistrations};
use std::sync::atomic::{AtomicI32, AtomicU8, Ordering};
use std::sync::Arc;

/// Completion is published only after the checked turn and typed state cleanup.
/// Actors reserve phase 1 for cleanup; supervisors use their teardown claim.
/// Phase 2 publishes the result after either owner finishes.
#[derive(Debug, Default)]
pub struct NativeActorCompletion {
    crash: Option<crate::actor::HewNativeCrashFn>,
    crash_action: AtomicI32,
    phase: AtomicU8,
    code: AtomicI32,
    ready: ReadinessRegistrations,
}

impl NativeActorCompletion {
    pub(crate) fn with_crash(crash: Option<crate::actor::HewNativeCrashFn>) -> Self {
        Self {
            crash,
            ..Self::default()
        }
    }

    pub(crate) fn crash_action(&self) -> Option<i32> {
        self.crash
            .filter(|_| self.is_finished())
            .map(|_| self.crash_action.load(Ordering::Relaxed))
    }

    pub(crate) fn is_finished(&self) -> bool {
        self.phase.load(Ordering::Acquire) == 2
    }

    /// Publish after the unique terminal owner has released all target state.
    pub(crate) fn finish(&self, code: i32) {
        self.code.store(code, Ordering::Relaxed);
        self.phase.store(2, Ordering::Release);
        self.ready.notify();
    }
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
    if state == HewActorState::Crashed as i32 {
        if let Some(hook) = completion.crash {
            let code = actor.error_code.load(Ordering::Acquire);
            let message = hew_cabi::string::string_from_str(
                crate::internal::types::ExitReason::from_error_code(code).trap_kind_name(),
            );
            let mut context = crate::execution_context::HewExecutionContext {
                actor: std::ptr::from_ref(actor).cast_mut(),
                actor_id: actor.id,
                arena: actor.arena,
                prev_context: crate::execution_context::current_context(),
                ..crate::execution_context::HewExecutionContext::default()
            };
            let previous = crate::execution_context::set_current_context(&raw mut context);
            // SAFETY: the terminal actor pins its owning runtime across cleanup.
            let _runtime = unsafe { actor.runtime.as_ref() }.map(|runtime| {
                // SAFETY: the actor retains this runtime until terminal cleanup returns.
                unsafe { crate::runtime::enter(runtime) }
            });
            // SAFETY: terminal cleanup owns the initialized incarnation state;
            // the generated adapter returns before its typed destructor runs.
            let action = unsafe { hook(actor.state, i64::from(code), message) };
            let restored = crate::execution_context::set_current_context(previous);
            debug_assert_eq!(restored, &raw mut context);
            completion.crash_action.store(action, Ordering::Release);
            // SAFETY: this frame owns the managed diagnostic string.
            unsafe { hew_cabi::string::string_release(message) };
        }
    }
    // SAFETY: this terminal owner has reserved completion before taking the
    // actor's existing exactly-once state destructor authority.
    unsafe { crate::actor::drop_initialized_actor_state(actor) };
    if state == HewActorState::Stopped as i32 {
        // Native cleanup owns normal DOWN publication, including an idle
        // actor closed without another scheduler activation. Queue the
        // notification before a close observer can release its monitor owner.
        crate::monitor::notify_monitors_on_death(actor.id, state, 0);
    }
    completion.finish(actor.error_code.load(Ordering::Acquire));
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
            unsafe { &*actor }.native_completion.clone()
        })
        .flatten()
    });
    let completion = completion.or_else(|| local_handles::current_supervisor_completion(token));
    if let Some(completion) = &completion {
        completion.ready.register(&waker);
    }
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
    #[test]
    fn supervisor_closed_observers_wait_through_config_cleanup() {
        use crate::supervisor::{hew_local_pid_supervisor_stop, hew_supervisor_native_spawn};
        use std::ffi::c_void;

        struct PausedConfigDrop {
            entered: Arc<std::sync::Barrier>,
            release: Arc<std::sync::Barrier>,
            drops: Arc<AtomicUsize>,
        }
        unsafe extern "C" fn drop_config(config: *mut c_void) {
            // SAFETY: the supervisor owns the initialized config until this callback returns.
            let config = unsafe { config.cast::<PausedConfigDrop>().read() };
            config.entered.wait();
            config.release.wait();
            config.drops.fetch_add(1, Ordering::SeqCst);
        }

        let _rt = crate::runtime_test_guard();
        let entered = Arc::new(std::sync::Barrier::new(2));
        let release = Arc::new(std::sync::Barrier::new(2));
        let drops = Arc::new(AtomicUsize::new(0));
        // SAFETY: the fixture creates and transfers one correctly sized config allocation.
        let token = unsafe {
            let config = crate::mem::buf_try_alloc(std::mem::size_of::<PausedConfigDrop>())
                .cast::<PausedConfigDrop>();
            assert!(!config.is_null());
            config.write(PausedConfigDrop {
                entered: entered.clone(),
                release: release.clone(),
                drops: drops.clone(),
            });
            let mut fault = std::ptr::null_mut();
            let token = hew_supervisor_native_spawn(
                0,
                3,
                5,
                config.cast(),
                Some(drop_config),
                [].as_ptr(),
                0,
                &raw mut fault,
            );
            assert!(fault.is_null());
            assert_ne!(token, local_handles::HewLocalPidId::INVALID);
            token
        };
        let (ready, wake) = crate::wake::blocking::Readiness::new();
        let (late_ready, late_wake) = crate::wake::blocking::Readiness::new();
        let (detached_ready, detached_wake) = crate::wake::blocking::Readiness::new();
        // SAFETY: all wake descriptors remain live, and each wait has one owner.
        unsafe {
            let early = hew_actor_wait_new(token, wake.descriptor());
            let detached = hew_actor_wait_new(token, detached_wake.descriptor());
            assert_eq!(hew_actor_wait_poll(early), 0);
            assert!(crate::lifetime::local_handles::pin_current_supervisor(token).is_some());
            hew_actor_wait_free(detached);
            let stop = std::thread::spawn(move || hew_local_pid_supervisor_stop(token));
            entered.wait();
            assert!(crate::lifetime::local_handles::pin_current_supervisor(token).is_none());
            let late = hew_actor_wait_new(token, late_wake.descriptor());
            assert_eq!(hew_actor_wait_poll(early), 0);
            assert_eq!(hew_actor_wait_poll(late), 0);
            assert!(!ready.take_ready());
            assert!(!late_ready.take_ready());
            // A concurrent close cannot publish completion ahead of the cleanup owner.
            hew_local_pid_supervisor_stop(token);
            assert_eq!(hew_actor_wait_poll(late), 0);
            assert_eq!(drops.load(Ordering::SeqCst), 0);
            release.wait();
            assert_eq!(stop.join().expect("supervisor close owner"), 0);
            assert_eq!(drops.load(Ordering::SeqCst), 1);
            assert!(ready.take_ready());
            assert!(late_ready.take_ready());
            assert!(!detached_ready.take_ready());
            assert_eq!(hew_actor_wait_poll(early), 1);
            assert_eq!(hew_actor_wait_poll(late), 1);
            hew_actor_wait_free(early);
            hew_actor_wait_free(late);
            let completed = hew_actor_wait_new(token, wake.descriptor());
            assert_eq!(hew_actor_wait_poll(completed), 1);
            hew_actor_wait_free(completed);
        }
    }

    #[test]
    #[expect(
        clippy::too_many_lines,
        reason = "the fixture owns both config allocations, concurrent cleanup owners and the observer"
    )]
    fn parent_closed_waits_for_a_concurrent_nested_cleanup_owner() {
        use crate::supervisor::{
            hew_local_pid_supervisor_stop, hew_supervisor_native_spawn, HewNativeChildSpec,
        };
        use std::ffi::c_void;

        struct ChildConfig {
            entered: Arc<std::sync::Barrier>,
            release: Arc<std::sync::Barrier>,
        }
        struct ParentConfig {
            child: local_handles::HewLocalPidId,
            drops: Arc<AtomicUsize>,
        }
        unsafe extern "C" fn drop_child(config: *mut c_void) {
            // SAFETY: the child owns the initialized config allocation.
            let config = unsafe { config.cast::<ChildConfig>().read() };
            config.entered.wait();
            config.release.wait();
        }
        unsafe extern "C" fn drop_parent(config: *mut c_void) {
            // SAFETY: the parent owns the initialized config allocation.
            let config = unsafe { config.cast::<ParentConfig>().read() };
            config.drops.fetch_add(1, Ordering::SeqCst);
        }
        unsafe extern "C-unwind" fn adopt_child(
            config: *const c_void,
            _fault: *mut *mut crate::fault::HewFault,
        ) -> local_handles::HewLocalPidId {
            // SAFETY: this initial-spawn adapter transfers the prepared child once.
            unsafe { (*config.cast::<ParentConfig>()).child }
        }

        let _rt = crate::runtime_test_guard();
        let entered = Arc::new(std::sync::Barrier::new(2));
        let release = Arc::new(std::sync::Barrier::new(2));
        let drops = Arc::new(AtomicUsize::new(0));
        // SAFETY: each config is initialized, then uniquely adopted by its supervisor.
        let (child, parent) = unsafe {
            let child_config =
                crate::mem::buf_try_alloc(std::mem::size_of::<ChildConfig>()).cast::<ChildConfig>();
            assert!(!child_config.is_null());
            child_config.write(ChildConfig {
                entered: entered.clone(),
                release: release.clone(),
            });
            let mut fault = std::ptr::null_mut();
            let child = hew_supervisor_native_spawn(
                0,
                3,
                5,
                child_config.cast(),
                Some(drop_child),
                [].as_ptr(),
                0,
                &raw mut fault,
            );
            assert!(fault.is_null());
            let parent_config = crate::mem::buf_try_alloc(std::mem::size_of::<ParentConfig>())
                .cast::<ParentConfig>();
            assert!(!parent_config.is_null());
            parent_config.write(ParentConfig {
                child,
                drops: drops.clone(),
            });
            let children = [HewNativeChildSpec {
                restart_policy: 0,
                role_kind: 1,
                spawn: adopt_child,
                name: std::ptr::null(),
            }];
            let parent = hew_supervisor_native_spawn(
                0,
                3,
                5,
                parent_config.cast(),
                Some(drop_parent),
                children.as_ptr(),
                children.len(),
                &raw mut fault,
            );
            assert!(fault.is_null());
            (child, parent)
        };
        let (_, wake) = crate::wake::blocking::Readiness::new();
        let role = local_handles::current_supervisor_role_owner(parent, 0);
        assert_eq!(
            role,
            local_handles::current_supervisor_role_owner(parent, 0)
        );
        assert!(local_handles::pin_current_supervisor(role).is_some());
        // SAFETY: the wake descriptor stays live and this test owns the observer.
        let observer = unsafe { hew_actor_wait_new(parent, wake.descriptor()) };
        let (_, child_wake) = crate::wake::blocking::Readiness::new();
        // SAFETY: this test retains the wake descriptor and owns both observers.
        let child_observer = unsafe {
            crate::supervisor::hew_supervisor_native_role_wait_new(
                parent,
                0,
                child_wake.descriptor(),
                0,
            )
        };
        assert!(local_handles::pin_current_supervisor(child).is_some());
        // SAFETY: the closing observer retains the same incarnation before stop.
        let child_close = unsafe {
            crate::supervisor::hew_supervisor_native_role_wait_new(
                parent,
                0,
                child_wake.descriptor(),
                1,
            )
        };
        entered.wait();
        let (finished, completion) = std::sync::mpsc::channel();
        let parent_close = std::thread::spawn(move || {
            let result = hew_local_pid_supervisor_stop(parent);
            finished.send(result).expect("parent completion receiver");
        });
        let early = completion.recv_timeout(std::time::Duration::from_millis(100));
        let early_drops = drops.load(Ordering::SeqCst);
        // SAFETY: the observer remains owned by this test through its last poll.
        let early_ready = unsafe { hew_actor_wait_poll(observer) };
        // SAFETY: both observers remain owned while child cleanup is paused.
        let early_child_ready = unsafe {
            (
                hew_actor_wait_poll(child_observer),
                hew_actor_wait_poll(child_close),
            )
        };
        release.wait();
        parent_close.join().expect("parent cleanup owner");
        assert!(matches!(
            early,
            Err(std::sync::mpsc::RecvTimeoutError::Timeout)
        ));
        assert_eq!(early_drops, 0);
        assert_eq!(early_ready, 0);
        assert_eq!(early_child_ready, (0, 0));
        assert_eq!(completion.recv().expect("parent result"), 0);
        assert_eq!(drops.load(Ordering::SeqCst), 1);
        // SAFETY: the observer has one owner and the cleanup threads have finished.
        unsafe {
            assert_eq!(hew_actor_wait_poll(observer), 1);
            hew_actor_wait_free(observer);
            assert_eq!(hew_actor_wait_poll(child_observer), 1);
            assert_eq!(hew_actor_wait_poll(child_close), 1);
            hew_actor_wait_free(child_observer);
            hew_actor_wait_free(child_close);
        }
        assert!(local_handles::pin_current_supervisor(role).is_none());
        crate::lifetime::live_actors::drain_deferred_teardown_threads();
        assert_eq!(local_handles::current_supervisor_counts_for_test(), (0, 0));
    }
}
