//! Local termination observers outlive the actor or supervisor allocation.

use crate::actor::HewActor;
use crate::internal::types::HewActorState;
use crate::lifetime::{live_actors, local_handles};
use crate::util::MutexExt;
use crate::wake::{HewWaker, OwnedWaker, ReadinessRegistrations};
use std::sync::atomic::{AtomicI32, AtomicU64, AtomicU8, Ordering};
use std::sync::{Arc, Mutex};

/// Completion is published only after the checked turn and typed state cleanup.
/// Actors reserve phase 1 for cleanup; supervisors use their teardown claim.
/// Actor phase 3 publishes terminal notifications after cleanup. Phase 2 makes
/// the final result observable after either owner finishes.
#[derive(Debug, Default)]
pub struct NativeActorCompletion {
    pub(crate) cleanup: super::cleanup::Cleanup,
    pub(crate) crash: Option<crate::actor::HewNativeCrashFn>,
    pub(crate) crash_action: AtomicI32,
    pub(crate) construction: Mutex<Option<crate::coro_state::RetainedCancellation>>,
    phase: AtomicU8,
    code: AtomicI32,
    ready: ReadinessRegistrations,
    terminal_notice: Mutex<Option<crate::actor::TerminalNotification>>,
    close_fault_record: AtomicU64,
    fault: Mutex<Option<crate::fault::HewFault>>,
    close_fault: Mutex<Option<crate::fault::HewFault>>,
}

impl NativeActorCompletion {
    pub(crate) fn with_crash(crash: Option<crate::actor::HewNativeCrashFn>) -> Self {
        Self {
            crash,
            ..Self::default()
        }
    }

    pub(crate) fn cancel_construction(&self, reason: i32) -> bool {
        let construction = self.construction.lock_or_recover();
        if let Some(wake) = construction.as_ref() {
            wake.cancel(reason);
            true
        } else {
            false
        }
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn crash_action(&self) -> Option<i32> {
        self.crash
            .filter(|_| self.phase.load(Ordering::Acquire) >= 2)
            .map(|_| self.crash_action.load(Ordering::Relaxed))
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn is_finished(&self) -> bool {
        self.phase.load(Ordering::Acquire) == 2
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn cleanup_in_progress(&self) -> bool {
        matches!(self.phase.load(Ordering::Acquire), 1 | 3)
    }

    pub(crate) fn defer_terminal_notification(&self, notice: crate::actor::TerminalNotification) {
        let previous = self.terminal_notice.lock_or_recover().replace(notice);
        assert!(
            previous.is_none(),
            "native terminal notification already owned"
        );
    }

    /// Retain the complete incarnation diagnostic for every close observer.
    pub(crate) fn record_fault(&self, fault: crate::fault::HewFault) -> i32 {
        let mut retained = self.fault.lock_or_recover();
        if let Some(primary) = retained.as_mut() {
            primary.append(fault);
        } else {
            *retained = Some(fault);
        }
        super::report_checked_failure(retained.as_ref().expect("retained terminal fault"))
    }

    /// Retain a fault raised by terminal hooks or state release separately
    /// from the crash that caused cleanup. `stopped` observes only this fault.
    pub(crate) fn record_close_fault(&self, fault: crate::fault::HewFault) -> i32 {
        let retained = {
            let mut retained = self.close_fault.lock_or_recover();
            if let Some(primary) = retained.as_mut() {
                primary.append(fault);
            } else {
                *retained = Some(fault);
            }
            retained.as_ref().expect("retained close fault").clone()
        };
        let code = retained.code();
        // Preserve the original crash as the primary report while keeping
        // cleanup diagnostics separate for `stopped`. HewFault clones share
        // each diagnostic's reported flag, so observers cannot print it twice.
        let original = self.fault.lock_or_recover().clone();
        let report = if let Some(mut original) = original {
            original.append(retained);
            original
        } else {
            retained.clone()
        };
        let _ = super::report_checked_failure(&report);
        code
    }

    pub(crate) fn close_fault_payload(&self) -> Option<(i32, String)> {
        self.close_fault
            .lock_or_recover()
            .as_ref()
            .map(crate::fault::HewFault::remote_payload)
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn finish_remote(&self, close_fault: Option<(i32, String)>) {
        let code = close_fault.as_ref().map_or(0, |(code, _)| *code);
        *self.close_fault.lock_or_recover() =
            close_fault.map(|(code, message)| crate::fault::HewFault::from_remote(code, message));
        self.finish(code);
    }

    /// Wake `waker` when this incarnation's terminal cleanup finishes. The
    /// registration is weak, and one made after the finish never fires, so the
    /// caller keeps the waker and checks [`Self::is_finished`] afterwards.
    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn wake_on_finish(&self, waker: &Arc<OwnedWaker>) {
        self.ready.register(waker);
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
    if actor.actor_state.load(Ordering::Acquire) == HewActorState::Stopped as i32
        && !actor.terminate_finished.load(Ordering::Acquire)
    {
        return;
    }
    // SAFETY: termination is complete or the actor crashed without a stop callback.
    unsafe { finish_native_terminal_after_terminate(actor) };
}

/// Finish while the terminate owner still retains the actor allocation.
///
/// # Safety
/// The caller retains the allocation and has completed or skipped its terminate
/// callback. No checked turn may still borrow the state.
pub(crate) unsafe fn finish_native_terminal_after_terminate(actor: &HewActor) {
    let Some(completion) = &actor.native_completion else {
        return;
    };
    let state = actor.actor_state.load(Ordering::Acquire);
    if !matches!(state, s if s == HewActorState::Stopped as i32 || s == HewActorState::Crashed as i32)
        || !actor.checked_invocation.load(Ordering::Acquire).is_null()
        || completion
            .phase
            .compare_exchange(0, 1, Ordering::AcqRel, Ordering::Acquire)
            .is_err()
    {
        return;
    }
    // A pending timer owns a registered payload. Retire it before terminal
    // cleanup closes the inbox and waits for its remaining owners.
    crate::timer_periodic::cancel_all_timers_for_actor(std::ptr::from_ref(actor).cast_mut());
    if completion.crash.is_some() || completion.cleanup.needs_terminal_cleanup() {
        // SAFETY: the terminal claim retains state until scheduler cleanup
        // completes; raw actor reclamation must observe this pending claim.
        unsafe { completion.cleanup.begin_terminal(actor, state) };
        return;
    }
    // SAFETY: this terminal owner has reserved completion before taking the
    // actor's existing exactly-once state destructor authority.
    unsafe { crate::actor::drop_initialized_actor_state(actor) };
    if !completion.cleanup.rc.close_empty() {
        // SAFETY: any allocations made during synchronous state release belong
        // to this same terminal owner and drain before completion publication.
        unsafe { completion.cleanup.begin_terminal(actor, state) };
        return;
    }
    finish_actor_terminal(actor, state);
}

/// Publish the terminal result after consuming payload and state cleanup.
pub(crate) fn finish_actor_terminal(actor: &HewActor, state: i32) {
    let Some(completion) = actor.native_completion.clone() else {
        return;
    };
    let code = actor.error_code.load(Ordering::Acquire);
    let close_fault_code = completion
        .close_fault
        .lock_or_recover()
        .as_ref()
        .map(crate::fault::HewFault::code);
    if state == HewActorState::Stopped as i32 && close_fault_code.is_some() {
        // A clean stop has no crash record to account for failed cleanup. A
        // crashed actor's existing record remains owned by its supervisor;
        // observing its close fault must not settle that crash or create a
        // second record that restart cannot rule on.
        let record = crate::exit_status::open_pending_fault();
        completion
            .close_fault_record
            .store(record.as_raw(), Ordering::Release);
    }
    let actor_id = actor.id;
    let notice = completion.terminal_notice.lock_or_recover().take();
    // Cleanup is complete, including the crash hook's decision, but close
    // observers stay pending until all terminal notifications are published.
    completion.phase.store(3, Ordering::Release);
    if let Some(notice) = notice {
        // SAFETY: terminal ownership retains the supervisor across publication.
        unsafe { notice.publish_terminal_notification(completion.close_fault_payload()) };
    } else if state == HewActorState::Stopped as i32 {
        crate::actor::notify_monitors_on_death(
            actor_id,
            state,
            0,
            crate::internal::types::ActorEndReason::from_terminal(
                state,
                // SAFETY: terminal publication retains the actor and mailbox.
                unsafe { crate::mailbox::mailbox_terminate_requested(actor.mailbox.cast()) },
            ),
            completion.close_fault_payload(),
        );
        #[cfg(not(target_arch = "wasm32"))]
        if !actor.supervisor.is_null() {
            if let Ok(child_index) = u32::try_from(actor.supervisor_child_index) {
                // SAFETY: the supervisor remains the child's parent through
                // terminal publication, and the zero record is a clean end.
                unsafe {
                    crate::supervisor::notify_child_actor_end_event(
                        actor.supervisor.cast(),
                        child_index,
                        actor_id,
                        state,
                        crate::internal::types::ActorEndReason::from_terminal(
                            state,
                            crate::mailbox::mailbox_terminate_requested(actor.mailbox.cast()),
                        ),
                        0,
                        0,
                    );
                }
            }
        }
    }
    // A crash was reported when it happened and already counts toward the
    // exit status (or its supervisor's ruling). Only a distinct fault raised
    // by terminal cleanup passes through `stopped`.
    completion.finish(close_fault_code.unwrap_or({
        if state == HewActorState::Crashed as i32 {
            0
        } else {
            code
        }
    }));
}

/// Request cooperative stop through a stable actor identity.
#[no_mangle]
pub extern "C" fn hew_actor_stop_native(token: local_handles::HewLocalPidId) {
    if let Some(id) = local_handles::resolve_current_actor(token) {
        live_actors::with_actor_send_by_id(id, |actor| {
            // SAFETY: the guard pins the actor while admission closes.
            unsafe { crate::actor::hew_actor_request_stop(actor) };
        });
        return;
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        if let Some((id, serial)) = crate::supervisor::current_actor_role_identity(token) {
            live_actors::with_actor_send_by_identity(id, serial, |actor| {
                // SAFETY: the identity pin protects the selected incarnation.
                unsafe { crate::actor::hew_actor_request_stop(actor.as_ptr()) };
            });
        }
    }
}

/// Request forceful termination through a stable actor identity.
#[no_mangle]
pub extern "C" fn hew_actor_terminate_native(token: local_handles::HewLocalPidId) {
    if let Some(id) = local_handles::resolve_current_actor(token) {
        live_actors::with_actor_send_by_id(id, |actor| {
            // SAFETY: the guard pins the actor while cancellation is latched.
            unsafe { crate::actor::hew_actor_terminate(actor) };
        });
        return;
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        if let Some((id, serial)) = crate::supervisor::current_actor_role_identity(token) {
            live_actors::with_actor_send_by_identity(id, serial, |actor| {
                // SAFETY: the identity pin protects the selected incarnation.
                unsafe { crate::actor::hew_actor_terminate(actor.as_ptr()) };
            });
        }
    }
}

#[derive(Debug)]
pub struct HewNativeActorWait {
    completion: Option<Arc<NativeActorCompletion>>,
    #[cfg(not(target_arch = "wasm32"))]
    imported: Option<Box<crate::hew_node::monitor_link::HewNativeActorObservation>>,
    pub(crate) target: live_actors::ActorIncarnation,
    _waker: Arc<OwnedWaker>,
}

/// One role resolution retains the chosen incarnation through owner teardown.
#[derive(Clone)]
pub(crate) struct NativeWaitTarget {
    pub(crate) completion: Arc<NativeActorCompletion>,
    pub(crate) actor: live_actors::ActorIncarnation,
    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) supervisor_token: Option<local_handles::HewLocalPidId>,
}

pub(crate) unsafe fn wait_from_target(
    target: Option<NativeWaitTarget>,
    waker: *const HewWaker,
) -> *mut HewNativeActorWait {
    // SAFETY: the caller keeps the descriptor live during retention.
    let waker = Arc::new(unsafe { OwnedWaker::retain(&*waker) });
    let actor = target
        .as_ref()
        .map_or(live_actors::ActorIncarnation::NONE, |target| target.actor);
    let completion = target.map(|target| target.completion);
    if let Some(completion) = &completion {
        completion.ready.register(&waker);
    }
    Box::into_raw(Box::new(HewNativeActorWait {
        completion,
        #[cfg(not(target_arch = "wasm32"))]
        imported: None,
        target: actor,
        _waker: waker,
    }))
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
    #[cfg(not(target_arch = "wasm32"))]
    if let Some(imported) = local_handles::current_imported_actor(token) {
        // SAFETY: the caller retains its live wake descriptor.
        let (completion, operation) =
            unsafe { crate::hew_node::monitor_link::imported_actor_wait(&imported, waker) };
        // SAFETY: this borrow remains live while retaining the descriptor.
        let owned = Arc::new(unsafe { OwnedWaker::retain(&*waker) });
        completion.ready.register(&owned);
        return Box::into_raw(Box::new(HewNativeActorWait {
            completion: Some(completion),
            imported: Some(operation),
            target: live_actors::ActorIncarnation::NONE,
            _waker: owned,
        }));
    }
    #[cfg(not(target_arch = "wasm32"))]
    if let Some((owner, slot, kind)) = local_handles::current_actor_role_owner_slot(token) {
        // SAFETY: the caller retains the descriptor during role resolution.
        return unsafe {
            crate::supervisor::hew_supervisor_native_role_wait_new(
                owner,
                slot,
                waker,
                i32::from(kind == local_handles::SupervisorRoleKind::Supervisor),
            )
        };
    }
    let target = local_handles::resolve_current_actor(token).and_then(|id| {
        live_actors::with_actor_send_by_id(id, |actor| {
            // SAFETY: the send guard pins this exact actor during registration.
            unsafe { &*actor }
                .native_completion
                .clone()
                .map(|completion| NativeWaitTarget {
                    completion,
                    // SAFETY: the send guard keeps this actor live during capture.
                    actor: unsafe { live_actors::ActorIncarnation::of(actor) },
                    #[cfg(not(target_arch = "wasm32"))]
                    supervisor_token: None,
                })
        })
        .flatten()
    });
    // A supervised child's completion is reachable through the local-PID
    // control table, which is native-only; wasm32 admits no supervisor.
    #[cfg(not(target_arch = "wasm32"))]
    let target = target.or_else(|| {
        local_handles::current_supervisor_completion(token).map(|completion| NativeWaitTarget {
            completion,
            actor: live_actors::ActorIncarnation::NONE,
            supervisor_token: Some(token),
        })
    });
    // SAFETY: forwarded descriptor contract and exact selected incarnation.
    unsafe { wait_from_target(target, waker) }
}

/// Returns 0 while pending, 1 after clean termination, or 2 after a failure.
///
/// # Safety
/// The caller uniquely owns the wait until it detaches or consumes completion.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_wait_poll(wait: *const HewNativeActorWait) -> i32 {
    // SAFETY: the operation remains live during this poll.
    let wait = unsafe { &mut *wait.cast_mut() };
    #[cfg(not(target_arch = "wasm32"))]
    if let Some(operation) = &mut wait.imported {
        // SAFETY: the unique wait owner serializes polls of its setup operation.
        unsafe {
            crate::hew_node::monitor_link::hew_native_actor_observe_poll(&raw mut **operation)
        };
    }
    match &wait.completion {
        Some(completion) if completion.phase.load(Ordering::Acquire) != 2 => 0,
        Some(completion) if completion.code.load(Ordering::Relaxed) != 0 => 2,
        _ => 1,
    }
}

/// Acquire the complete terminal diagnostic after a failed poll. Each observer
/// owns its wrapper, while the diagnostic's report identity remains shared.
///
/// # Safety
/// `wait` is live and the caller has acquired its completed failure.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_wait_take_fault(
    wait: *const HewNativeActorWait,
) -> *mut crate::fault::HewFault {
    // SAFETY: the caller retains the immutable completed result descriptor.
    let Some(completion) = &(unsafe { &*wait }).completion else {
        return crate::fault::hew_fault_new(0);
    };
    let record = crate::exit_status::FaultRecord::from_raw(
        completion.close_fault_record.swap(0, Ordering::AcqRel),
    );
    crate::exit_status::settle_supervised_fault(record, crate::exit_status::FaultRuling::Handled);
    let code = completion.code.load(Ordering::Relaxed);
    let fault = completion
        .close_fault
        .lock_or_recover()
        .clone()
        .or_else(|| completion.fault.lock_or_recover().clone());
    fault.map_or_else(
        || crate::fault::hew_fault_new(code),
        |fault| Box::into_raw(Box::new(fault)),
    )
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
            let completion = Arc::new(NativeActorCompletion::default());
            actor.native_completion = Some(completion.clone());
            let waker = Arc::new(waker);
            completion.ready.register(&waker);
            let wait = Box::into_raw(Box::new(HewNativeActorWait {
                completion: Some(completion),
                #[cfg(not(target_arch = "wasm32"))]
                imported: None,
                target: live_actors::ActorIncarnation::NONE,
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
    fn close_fault_requires_transfer_to_a_close_observer() {
        let _runtime = crate::runtime_test_guard();
        for observe in [false, true] {
            crate::exit_status::reset_process_exit_status();
            let mut actor = crate::test_actor::stub_actor();
            let completion = Arc::new(NativeActorCompletion::default());
            actor.native_completion = Some(completion.clone());
            actor
                .actor_state
                .store(HewActorState::Stopped as i32, Ordering::Release);
            actor.error_code.store(212, Ordering::Release);
            completion.record_close_fault(crate::fault::HewFault::with_message(
                212,
                "state close failed".into(),
            ));
            actor.terminate_finished.store(true, Ordering::Release);
            // SAFETY: the fixture owns the terminal actor and its empty state.
            unsafe { finish_native_terminal(&actor) };
            assert_eq!(crate::exit_status::hew_runtime_exit_status(), 1);
            let (_, waker) = crate::wake::blocking::Readiness::new();
            let wait = Box::into_raw(Box::new(HewNativeActorWait {
                completion: Some(completion),
                #[cfg(not(target_arch = "wasm32"))]
                imported: None,
                target: live_actors::ActorIncarnation::NONE,
                _waker: Arc::new(waker),
            }));
            // SAFETY: this test owns the unique observer until its final free.
            unsafe {
                assert_eq!(hew_actor_wait_poll(wait), 2);
                if observe {
                    for _ in 0..2 {
                        let fault = hew_actor_wait_take_fault(wait);
                        assert_eq!(crate::fault::hew_fault_code(fault), 212);
                        let message = crate::fault::hew_fault_take_message(fault);
                        assert_eq!(
                            hew_cabi::string::string_as_str(message),
                            "hew: failure: UserPanic (212): state close failed\n"
                        );
                        hew_cabi::string::string_release(message);
                    }
                }
                hew_actor_wait_free(wait);
            }
            assert_eq!(
                crate::exit_status::hew_runtime_exit_status(),
                i32::from(!observe)
            );
        }
        crate::exit_status::reset_process_exit_status();
    }

    #[test]
    fn supervisor_stop_request_observers_wait_through_config_cleanup() {
        use crate::supervisor::{hew_supervisor_native_spawn, hew_supervisor_stop_native};
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
            // The request must return before config cleanup reaches its gate.
            hew_supervisor_stop_native(token);
            entered.wait();
            assert!(crate::lifetime::local_handles::pin_current_supervisor(token).is_none());
            let late = hew_actor_wait_new(token, late_wake.descriptor());
            assert_eq!(hew_actor_wait_poll(early), 0);
            assert_eq!(hew_actor_wait_poll(late), 0);
            assert!(!ready.take_ready());
            assert!(!late_ready.take_ready());
            // A concurrent close cannot publish completion ahead of the cleanup owner.
            hew_supervisor_stop_native(token);
            assert_eq!(hew_actor_wait_poll(late), 0);
            assert_eq!(drops.load(Ordering::SeqCst), 0);
            release.wait();
            ready.wait();
            late_ready.wait();
            assert_eq!(drops.load(Ordering::SeqCst), 1);
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

    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn remote_completion_observes_only_cleanup_faults() {
        for close_fault in [None, Some((213, "remote state close failed".to_owned()))] {
            let completion = Arc::new(NativeActorCompletion::default());
            let payload = crate::envelope::MonitorDownPayload {
                ref_id: 7,
                target: crate::node_identity::Location::new(
                    crate::node_identity::NodeId::from_bytes([1; 16]),
                    1,
                    1,
                )
                .unwrap(),
                reason: HewActorState::Crashed as i32,
                end_reason: crate::internal::types::ActorEndReason::Crashed as u8,
                crash_kind: crate::internal::types::CrashKind::Crashed.tag(),
                close_fault: close_fault.clone(),
            };
            let encoded = crate::envelope::encode_monitor_down_payload(&payload).unwrap();
            let decoded = crate::envelope::decode_monitor_down_payload(&encoded).unwrap();
            completion.finish_remote(decoded.close_fault);
            let (_, waker) = crate::wake::blocking::Readiness::new();
            let wait = Box::into_raw(Box::new(HewNativeActorWait {
                completion: Some(completion),
                imported: None,
                target: live_actors::ActorIncarnation::NONE,
                _waker: Arc::new(waker),
            }));
            // SAFETY: this test owns its completed observation until final release.
            unsafe {
                assert_eq!(
                    hew_actor_wait_poll(wait),
                    if close_fault.is_some() { 2 } else { 1 }
                );
                if let Some((code, message)) = close_fault {
                    let fault = hew_actor_wait_take_fault(wait);
                    assert_eq!(crate::fault::hew_fault_code(fault), code);
                    let text = crate::fault::hew_fault_take_message(fault);
                    assert!(hew_cabi::string::string_as_str(text).contains(&message));
                    hew_cabi::string::string_release(text);
                }
                hew_actor_wait_free(wait);
            }
        }
    }

    #[test]
    fn stopped_after_crash_does_not_steal_supervisor_fault_ruling() {
        let _runtime = crate::runtime_test_guard();
        crate::exit_status::reset_process_exit_status();
        let mut actor = crate::test_actor::stub_actor();
        let completion = Arc::new(NativeActorCompletion::default());
        actor.native_completion = Some(completion.clone());
        actor
            .actor_state
            .store(HewActorState::Crashed as i32, Ordering::Release);
        actor.error_code.store(212, Ordering::Release);
        completion.record_fault(crate::fault::HewFault::with_message(
            212,
            "original handler crash".into(),
        ));
        // SAFETY: this fixture owns the completed crashed actor.
        finish_actor_terminal(&actor, HewActorState::Crashed as i32);
        let (_, waker) = crate::wake::blocking::Readiness::new();
        let wait = Box::into_raw(Box::new(HewNativeActorWait {
            completion: Some(completion.clone()),
            #[cfg(not(target_arch = "wasm32"))]
            imported: None,
            target: live_actors::ActorIncarnation::NONE,
            _waker: Arc::new(waker),
        }));
        // SAFETY: the test owns the unique observer until its final free.
        unsafe {
            assert_eq!(hew_actor_wait_poll(wait), 1);
            hew_actor_wait_free(wait);
        }

        crate::exit_status::reset_process_exit_status();
        let crash_record = crate::exit_status::open_supervised_fault();
        let mut actor = crate::test_actor::stub_actor();
        let completion = Arc::new(NativeActorCompletion::default());
        actor.native_completion = Some(completion.clone());
        actor
            .actor_state
            .store(HewActorState::Crashed as i32, Ordering::Release);
        actor.error_code.store(212, Ordering::Release);
        completion.record_fault(crate::fault::HewFault::with_message(
            212,
            "original handler crash".into(),
        ));
        completion.record_close_fault(crate::fault::HewFault::with_message(
            213,
            "crash hook failed".into(),
        ));
        // SAFETY: this fixture owns the completed crashed actor.
        finish_actor_terminal(&actor, HewActorState::Crashed as i32);
        assert_eq!(crate::exit_status::hew_runtime_exit_status(), 1);
        let (_, waker) = crate::wake::blocking::Readiness::new();
        let wait = Box::into_raw(Box::new(HewNativeActorWait {
            completion: Some(completion),
            #[cfg(not(target_arch = "wasm32"))]
            imported: None,
            target: live_actors::ActorIncarnation::NONE,
            _waker: Arc::new(waker),
        }));
        // SAFETY: the test owns the unique observer until its final free.
        unsafe {
            assert_eq!(hew_actor_wait_poll(wait), 2);
            let fault = hew_actor_wait_take_fault(wait);
            assert_eq!(crate::fault::hew_fault_code(fault), 213);
            crate::fault::hew_fault_drop(fault);
            // `stopped` observes the close failure but cannot rule on the
            // original supervised crash; only effective restart owns that.
            assert_eq!(crate::exit_status::hew_runtime_exit_status(), 1);
            crate::exit_status::settle_supervised_fault(
                crash_record,
                crate::exit_status::FaultRuling::Handled,
            );
            assert_eq!(crate::exit_status::hew_runtime_exit_status(), 0);
            hew_actor_wait_free(wait);
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
                stop_ns: -1,
            }];
            let parent = hew_supervisor_native_spawn(
                0,
                3,
                5,
                parent_config.cast(),
                Some(drop_parent),
                children.as_ptr(),
                children.len(),
                0,
                &raw mut fault,
            );
            assert!(fault.is_null());
            (child, parent)
        };
        let (_, wake) = crate::wake::blocking::Readiness::new();
        let role = local_handles::current_supervisor_role_owner(
            parent,
            0,
            local_handles::SupervisorRoleKind::Supervisor,
        );
        assert_eq!(
            role,
            local_handles::current_supervisor_role_owner(
                parent,
                0,
                local_handles::SupervisorRoleKind::Supervisor
            )
        );
        let actor_role = local_handles::current_supervisor_role_owner(
            parent,
            0,
            local_handles::SupervisorRoleKind::Actor,
        );
        assert_ne!(actor_role, role);
        assert_eq!(
            local_handles::hew_actor_handle_identical(actor_role, role),
            0
        );
        assert_eq!(
            actor_role,
            local_handles::current_supervisor_role_owner(
                parent,
                0,
                local_handles::SupervisorRoleKind::Actor
            )
        );
        assert!(local_handles::pin_current_supervisor(actor_role).is_none());
        assert!(crate::supervisor::current_actor_role_identity(role).is_none());
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
                1,
            )
        };
        assert!(local_handles::pin_current_supervisor(child).is_some());
        let (finished, completion) = std::sync::mpsc::channel();
        let parent_close = std::thread::spawn(move || {
            let result = hew_local_pid_supervisor_stop(parent);
            finished.send(result).expect("parent completion receiver");
        });
        // The stop request starts child cleanup; the hook holds it here until
        // both the early and late observers have checked their selected child.
        entered.wait();
        let route_closed = local_handles::pin_current_supervisor(parent).is_none();
        // SAFETY: the explicit stop retained the selected child completion
        // before closing the parent's route; this observer registers afterward.
        let child_close = unsafe {
            crate::supervisor::hew_supervisor_native_role_wait_new(
                parent,
                0,
                child_wake.descriptor(),
                1,
            )
        };
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
        assert!(route_closed);
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
