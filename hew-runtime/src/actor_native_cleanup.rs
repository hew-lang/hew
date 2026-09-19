//! Consuming payload and state cleanup remains on the actor's scheduler turn.

use crate::internal::types::HewActorState;
use crate::release_walker::{HewReleaseCursor, ReleaseDriver};
use crate::util::MutexExt;
use crate::{actor::HewActor, coro_state::HewCoroState, fault::HewFault};
use hew_cabi::value::HewValueReleaseStart;
use std::sync::atomic::{AtomicI32, AtomicPtr, Ordering};
use std::{collections::VecDeque, ffi::c_void, ptr, sync::Mutex};

#[derive(Debug, Default)]
struct Inbox {
    owners: usize,
    closed: bool,
    pending: VecDeque<*mut HewReleaseCursor>,
}
// SAFETY: each pointer transfers an exclusive cursor, consumed by one activation.
unsafe impl Send for Inbox {}

#[derive(Debug)]
struct Work {
    state: *mut HewCoroState,
    driver: Option<Box<ReleaseDriver>>,
    state_started: bool,
    state_done: bool,
    payload: bool,
    fault: *mut HewFault,
}
// SAFETY: the actor activation serializes execution; child wake targets retain
// identities independently and never borrow this work object.
unsafe impl Send for Work {}

#[derive(Debug, Default)]
pub(crate) struct Cleanup {
    inbox: Mutex<Inbox>,
    work: Mutex<Option<Box<Work>>>,
    state_release: AtomicPtr<c_void>,
    terminal: AtomicI32,
}

impl Cleanup {
    pub(crate) fn set_state_release(&self, callback: Option<HewValueReleaseStart>) {
        self.state_release.store(
            callback.map_or(ptr::null_mut(), |f| f as *mut c_void),
            Ordering::Release,
        );
    }

    pub(crate) fn register(&self) -> bool {
        let mut inbox = self.inbox.lock_or_recover();
        if inbox.closed {
            return false;
        }
        inbox.owners = inbox
            .owners
            .checked_add(1)
            .expect("native payload owner overflow");
        true
    }

    pub(crate) fn retire_without_release(&self) {
        let mut inbox = self.inbox.lock_or_recover();
        assert!(inbox.owners > 0);
        inbox.owners -= 1;
    }

    pub(crate) fn enqueue(&self, cursor: *mut HewReleaseCursor) {
        self.inbox.lock_or_recover().pending.push_back(cursor);
    }

    pub(crate) fn has_work(&self) -> bool {
        self.terminal.load(Ordering::Acquire) != 0
            || self.work.lock_or_recover().is_some()
            || !self.inbox.lock_or_recover().pending.is_empty()
    }

    pub(crate) fn is_driving(&self) -> bool {
        self.terminal.load(Ordering::Acquire) != 0 || self.work.lock_or_recover().is_some()
    }

    pub(crate) fn needs_terminal_cleanup(&self) -> bool {
        !self.state_release.load(Ordering::Acquire).is_null()
            || self.inbox.lock_or_recover().owners != 0
    }

    pub(crate) unsafe fn begin_terminal(&self, actor: &HewActor, terminal: i32) {
        self.inbox.lock_or_recover().closed = true;
        self.terminal.store(terminal, Ordering::Release);
        // The completion claim precedes this scheduling handoff, so free paths
        // cannot mistake terminal publication for completed state destruction.
        actor
            .actor_state
            .store(HewActorState::Runnable as i32, Ordering::Release);
        crate::scheduler::sched_enqueue(ptr::from_ref(actor).cast_mut());
    }
}

/// Wake queued cleanup without taking the actor away from an active handler.
pub(crate) fn wake(actor: &HewActor) {
    let raw = ptr::from_ref(actor).cast_mut();
    if actor
        .actor_state
        .compare_exchange(
            HewActorState::Idle as i32,
            HewActorState::Runnable as i32,
            Ordering::AcqRel,
            Ordering::Acquire,
        )
        .is_ok()
    {
        crate::scheduler::sched_enqueue(raw);
    } else {
        // SAFETY: the caller retains this exact actor incarnation.
        let target = unsafe { crate::lifetime::live_actors::ActorIncarnation::of(raw) };
        crate::scheduler::enqueue_resume_by_incarnation(target);
    }
}

/// Drive pending releases before another message or terminal completion.
/// Returns true when this activation was used by cleanup.
/// # Safety
/// The scheduler owns this actor's Running activation and no handler is live.
#[expect(
    clippy::too_many_lines,
    reason = "one scheduler activation serializes payload and state cleanup through terminal publication"
)]
pub(crate) unsafe fn drive_actor_cleanup(actor: &HewActor) -> bool {
    let Some(completion) = &actor.native_completion else {
        return false;
    };
    let cleanup = &completion.cleanup;
    if !cleanup.has_work() {
        return false;
    }
    let mut work = cleanup.work.lock_or_recover().take().unwrap_or_else(|| {
        // SAFETY: this activation pins the actor and owns its invocation slot.
        let state = unsafe { super::new_cleanup_state(actor) };
        Box::new(Work {
            state,
            driver: None,
            state_started: false,
            state_done: false,
            payload: false,
            fault: ptr::null_mut(),
        })
    });
    let previous = crate::execution_context::current_context();
    let mut context = crate::execution_context::HewExecutionContext {
        actor: ptr::from_ref(actor).cast_mut(),
        actor_id: actor.id,
        arena: actor.arena,
        parent_supervisor: actor.supervisor,
        supervisor_child_index: actor.supervisor_child_index,
        // SAFETY: the invocation remains owned until this driver completes.
        cancel_token: unsafe { crate::coro_state::hew_coro_state_token(work.state) },
        prev_context: previous,
        ..crate::execution_context::HewExecutionContext::default()
    };
    let _ = crate::execution_context::set_current_context(&raw mut context);
    let mut pending = false;
    loop {
        if let Some(driver) = work.driver.as_mut() {
            // SAFETY: the boxed driver and parent retain every borrowed output.
            if !unsafe { driver.poll(work.state) } {
                pending = true;
                break;
            }
            // SAFETY: both operands are distinct diagnostic owners.
            work.fault =
                unsafe { crate::fault::hew_fault_combine(work.fault, driver.take_fault()) };
            work.driver = None;
            if work.payload {
                cleanup.retire_without_release();
            } else {
                work.state_done = true;
            }
        }
        let cursor = cleanup.inbox.lock_or_recover().pending.pop_front();
        if let Some(cursor) = cursor {
            work.payload = true;
            work.driver = Some(ReleaseDriver::new(cursor));
            continue;
        }
        let terminal = cleanup.terminal.load(Ordering::Acquire);
        if terminal == 0 {
            break;
        }
        if cleanup.inbox.lock_or_recover().owners != 0 {
            pending = true;
            break;
        }
        if !work.state_started {
            work.state_started = true;
            work.payload = false;
            let start = cleanup.state_release.load(Ordering::Acquire);
            if !actor.state_drop_consumed.swap(true, Ordering::AcqRel)
                && !actor.state_drop_borrowed.load(Ordering::Acquire)
                && !actor.state.is_null()
            {
                if start.is_null() {
                    if let Some(drop) = actor.state_drop_fn {
                        // SAFETY: this cleanup owns the exact initialized state.
                        unsafe { drop(actor.state) };
                    }
                    work.state_done = true;
                } else {
                    // SAFETY: this pointer was installed from the exact callback type.
                    let start =
                        unsafe { std::mem::transmute::<*mut c_void, HewValueReleaseStart>(start) };
                    // SAFETY: state storage stays with the actor after semantic release.
                    let cursor = unsafe {
                        HewReleaseCursor::payload(actor.state, actor.state_size, start, false)
                    };
                    work.driver = Some(ReleaseDriver::new(cursor));
                    continue;
                }
            } else {
                work.state_done = true;
            }
        }
        break;
    }
    let _ = crate::execution_context::set_current_context(previous);
    if pending {
        *cleanup.work.lock_or_recover() = Some(work);
        actor
            .actor_state
            .store(HewActorState::Suspended as i32, Ordering::Release);
        if crate::coro_exec::take_pending_wake(actor)
            && actor
                .actor_state
                .compare_exchange(
                    HewActorState::Suspended as i32,
                    HewActorState::Runnable as i32,
                    Ordering::AcqRel,
                    Ordering::Acquire,
                )
                .is_ok()
        {
            crate::scheduler::sched_enqueue(ptr::from_ref(actor).cast_mut());
        }
        return true;
    }
    actor
        .checked_invocation
        .store(ptr::null_mut(), Ordering::Release);
    // SAFETY: every generated child has completed and been destroyed.
    unsafe { crate::coro_state::hew_coro_state_free(work.state) };
    let terminal = cleanup.terminal.swap(0, Ordering::AcqRel);
    if !work.fault.is_null() {
        // SAFETY: this driver owns the completed diagnostic while reporting it.
        let code = super::report_checked_failure(unsafe { &*work.fault });
        // SAFETY: all child callbacks have finished using this owned diagnostic.
        unsafe { crate::fault::hew_fault_drop(work.fault) };
        if actor.error_code.load(Ordering::Acquire) == 0 {
            actor.error_code.store(code, Ordering::Release);
        }
        if terminal == 0 {
            // SAFETY: a nonterminal discarded-payload failure faults its receiver.
            unsafe {
                crate::actor::hew_actor_trap_from_activation(ptr::from_ref(actor).cast_mut(), code);
            };
            return true;
        }
    }
    if terminal != 0 {
        actor.actor_state.store(terminal, Ordering::Release);
        // SAFETY: all consuming callbacks and their frames have completed.
        super::finish_actor_terminal(actor, terminal);
    } else {
        // SAFETY: this activation owns the completed cleanup turn.
        unsafe { crate::scheduler::settle_native_cleanup(ptr::from_ref(actor).cast_mut()) };
    }
    true
}
