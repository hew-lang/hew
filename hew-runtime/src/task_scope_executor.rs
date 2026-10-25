//! Scheduler ownership of one checked task continuation.

use super::group::RaceGroup;
use super::{checked, complete, release_task_ref, CheckedTaskInvocation, HewTask};
use crate::cont::{hew_cont_destroy, hew_cont_done, hew_cont_resume};
use crate::coro_state::{
    hew_coro_state_free, hew_coro_state_new, hew_coro_state_private_status, hew_coro_state_status,
    CoroStatus, HewCoroState,
};
use crate::execution_context::{current_context, set_current_context, HewExecutionContext};
use crate::fault::HewFault;
use crate::util::MutexExt;
use crate::wake::scheduled::{enqueue, ReadyState, ReadyWork};
use crate::wake::{HewWaker, OwnedWaker};
use std::cell::UnsafeCell;
use std::ffi::c_void;
use std::ptr;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};

static LIVE_TASKS: AtomicUsize = AtomicUsize::new(0);

/// Readiness owns this allocation independently of the generated frame.
pub(crate) struct TaskExecution {
    phase: ReadyState,
    driver: UnsafeCell<Driver>,
    transfer: Mutex<TransferGate>,
}

#[derive(Default)]
struct TransferGate {
    owner: Option<usize>,
    retained: Option<Arc<TaskExecution>>,
    waiters: Vec<(usize, OwnedWaker)>,
}

struct Driver {
    code: Option<crate::code_owner::CodeRoot>,
    task: *mut HewTask,
    state: *mut HewCoroState,
    frame: *mut c_void,
    group: Option<RaceGroup>,
    fault: *mut HewFault,
    actor_turn: crate::lifetime::live_actors::ActorIncarnation,
    rc_owner: Option<Arc<crate::rc::RcAllocations>>,
    value_owner: Option<crate::owned_value_budget::ValueOwner>,
    request_deadline: crate::request_deadline::RequestDeadline,
}

// SAFETY: the phase transition grants exactly one worker access to the driver.
// Wakes access only phase and retained Arc ownership, never the frame or driver.
unsafe impl Send for TaskExecution {}
// SAFETY: the same exclusive RUNNING ownership serializes all driver accesses.
unsafe impl Sync for TaskExecution {}

impl TaskExecution {
    pub(crate) fn has_live_tasks() -> bool {
        LIVE_TASKS.load(Ordering::Acquire) != 0
    }

    /// Caller transfers one execution reference, independent of scope/handle.
    pub(super) unsafe fn spawn(task: *mut HewTask) {
        LIVE_TASKS.fetch_add(1, Ordering::Release);
        let execution = Arc::new(Self {
            phase: ReadyState::queued(),
            transfer: Mutex::new(TransferGate::default()),
            driver: UnsafeCell::new(Driver {
                code: None,
                task,
                state: ptr::null_mut(),
                frame: ptr::null_mut(),
                group: None,
                fault: ptr::null_mut(),
                // SAFETY: the scope retains the exact actor incarnation.
                actor_turn: unsafe { (*(*task).scope).actor_turn },
                // SAFETY: the scope outlives every submitted child.
                rc_owner: unsafe { (*(*task).scope).rc_owner.clone() },
                // SAFETY: the lexical drain retains the incarnation account.
                value_owner: unsafe { (*(*task).scope).value_owner.clone() },
                request_deadline: crate::request_deadline::RequestDeadline::current(),
            }),
        });
        // SAFETY: the unpublished task owns its executor association.
        unsafe { checked(task) }.lock_or_recover().execution = Arc::downgrade(&execution);
        enqueue(execution);
    }

    fn notify(self: &Arc<Self>) {
        if self.phase.notify() {
            enqueue(self.clone());
        }
        // A nested provider may become quiescent while this task's frame is
        // frozen. Its ordinary notification must also retry the transfer walk.
        self.wake_frozen_owner();
    }

    pub(super) fn freeze(self: &Arc<Self>, identity: usize, wake: Option<OwnedWaker>) -> bool {
        let mut gate = self.transfer.lock_or_recover();
        if !gate.waiters.iter().any(|(id, _)| *id == identity) {
            if let Some(wake) = wake {
                gate.waiters.push((identity, wake));
            }
        }
        if gate.owner.is_some_and(|owner| owner != identity) {
            return false;
        }
        gate.owner = Some(identity);
        gate.retained = Some(self.clone());
        self.phase.freeze()
    }

    pub(super) fn unfreeze(self: &Arc<Self>, identity: usize) {
        let (wakes, requeue) = {
            let mut gate = self.transfer.lock_or_recover();
            gate.waiters.retain(|(id, _)| *id != identity);
            if gate.owner != Some(identity) {
                return;
            }
            gate.owner = None;
            gate.retained = None;
            let requeue = self.phase.unfreeze();
            let wakes = std::mem::take(&mut gate.waiters);
            (wakes, requeue)
        };
        if requeue {
            enqueue(self.clone());
        }
        for (_, wake) in wakes {
            wake.wake();
        }
    }

    pub(super) fn frozen_by(&self, identity: usize) -> bool {
        self.transfer.lock_or_recover().owner == Some(identity) && self.phase.frozen_quiescent()
    }

    pub(super) unsafe fn visit_frozen_frame(
        &self,
        visitor: *const hew_cabi::managed::HewManagedStorageVisitor,
    ) -> i32 {
        use hew_cabi::managed::{HewManagedWalkKind, UNDESCRIBED};
        if !self.phase.frozen_quiescent() {
            return UNDESCRIBED;
        }
        // SAFETY: the frozen scheduling gate excludes every mutable driver poll.
        unsafe {
            let driver = &mut *self.driver.get();
            let status = crate::owned_value_budget::frame::hew_managed_visit_invocation(
                driver.state,
                visitor,
            );
            if status != 0 {
                return status;
            }
            if let Some(group) = &mut driver.group {
                let status = group.visit_managed(visitor);
                if status != 0 {
                    return status;
                }
            }
            if (*visitor).kind == HewManagedWalkKind::CommitTransfer {
                driver.value_owner =
                    crate::owned_value_budget::transfer::destination_owner(visitor);
                if let Some(binding) =
                    crate::owned_value_budget::transfer::destination_binding(visitor)
                {
                    driver.actor_turn = binding.actor_turn;
                    driver.rc_owner = binding.rc_owner;
                }
            }
        }
        0
    }

    fn wake_frozen_owner(&self) {
        if !self.phase.frozen_quiescent() {
            return;
        }
        let wake = {
            let gate = self.transfer.lock_or_recover();
            gate.waiters
                .iter()
                .find(|(id, _)| Some(*id) == gate.owner)
                .map(|(_, wake)| wake.clone())
        };
        if let Some(wake) = wake {
            wake.wake();
        }
    }
}

impl ReadyWork for TaskExecution {
    fn poll(self: Arc<Self>) {
        if !self.phase.try_begin() {
            return;
        }
        // SAFETY: taking the sole queue entry grants exclusive driver access.
        let driver = unsafe { &mut *self.driver.get() };
        let previous = current_context();
        let mut context = HewExecutionContext {
            actor_id: driver.actor_turn.actor_id(),
            // SAFETY: the execution reference retains task and its token/scope.
            cancel_token: unsafe { (*driver.task).cancel_token },
            // SAFETY: structured drain keeps scope alive through completion.
            task_scope: unsafe { (*driver.task).scope },
            request_deadline: driver.request_deadline,
            prev_context: previous,
            ..HewExecutionContext::default()
        };
        let _ = set_current_context(&raw mut context);
        let finished = crate::fault::with_owning_actor(driver.actor_turn.actor_id(), || {
            // SAFETY: this worker exclusively drives the retained continuation.
            unsafe { driver.poll(&self) }
        });
        let _ = set_current_context(previous);
        let requeue = self.phase.finish(finished);
        self.wake_frozen_owner();
        if finished {
            LIVE_TASKS.fetch_sub(1, Ordering::Release);
        } else if requeue {
            enqueue(self.clone());
        }
    }
}

impl Driver {
    #[allow(
        clippy::too_many_lines,
        reason = "one driver advances and cleans up the checked task lifecycle"
    )]
    unsafe fn poll(&mut self, execution: &Arc<TaskExecution>) -> bool {
        // SAFETY: the phase protocol serializes access to frame and outputs.
        unsafe {
            if self.state.is_null() {
                let waker = HewWaker {
                    context: Arc::as_ptr(execution).cast_mut().cast(),
                    wake,
                    retain,
                    release,
                };
                self.state = hew_coro_state_new(&raw const waker, (*self.task).cancel_token);
                (*self.state).execution_mut().actor_turn = self.actor_turn;
                (*self.state)
                    .execution_mut()
                    .rc_owner
                    .clone_from(&self.rc_owner);
                (*self.state)
                    .execution_mut()
                    .value_owner
                    .clone_from(&self.value_owner);
                let (invocation, result, carrier) = {
                    let mut task = checked(self.task).lock_or_recover();
                    let carrier = task
                        .invocation
                        .as_ref()
                        .expect("one task invocation")
                        .carrier_storage();
                    (
                        task.invocation.take().expect("one task invocation"),
                        task.result,
                        carrier,
                    )
                };
                match invocation {
                    CheckedTaskInvocation::Callable {
                        code,
                        value: callable,
                        arguments,
                        consuming,
                        slot_bytes: _,
                    } => {
                        if let Some(owner) = &self.value_owner {
                            // Captures move into the invocation. Only the stored
                            // callable carrier ceases to occupy managed payload.
                            owner.release(&[carrier]);
                        }
                        self.code = Some(code);
                        let descriptor = &*callable.descriptor;
                        let invoke = if consuming {
                            descriptor.invoke_once
                        } else {
                            descriptor.invoke_borrow
                        };
                        self.frame = (invoke.unwrap_or_else(|| std::process::abort()))(
                            callable.environment,
                            arguments,
                            result,
                            (&raw mut self.fault).cast(),
                            self.state.cast(),
                        );
                    }
                    CheckedTaskInvocation::Race {
                        members,
                        slot_bytes,
                    } => {
                        self.group = Some(RaceGroup::new(
                            self.task,
                            members,
                            self.state,
                            &raw const waker,
                            slot_bytes,
                        ));
                    }
                }
            } else if self.group.is_none() {
                hew_cont_resume(self.frame);
            }
            if let Some(group) = &mut self.group {
                group.poll(self.state, &raw mut self.fault);
            }
            let status = hew_coro_state_status(self.state);
            if status == CoroStatus::Pending as i32 {
                if self.group.is_none() && (self.frame.is_null() || hew_cont_done(self.frame)) {
                    std::process::abort();
                }
                return false;
            }
            if status == CoroStatus::Yielded as i32 {
                std::process::abort();
            }
            if !self.frame.is_null() {
                if !hew_cont_done(self.frame) {
                    std::process::abort();
                }
                hew_cont_destroy(self.frame);
                self.frame = ptr::null_mut();
            }
            self.code = None;
            self.group = None;
            let status = hew_coro_state_private_status(self.state);
            hew_coro_state_free(self.state);
            self.state = ptr::null_mut();
            complete(
                self.task,
                status,
                std::mem::replace(&mut self.fault, ptr::null_mut()),
            );
            release_task_ref(self.task);
            self.task = ptr::null_mut();
            true
        }
    }
}

unsafe extern "C" fn wake(context: *mut c_void) {
    // SAFETY: the caller retains this Arc; borrow its reference without a drop.
    let execution =
        std::mem::ManuallyDrop::new(unsafe { Arc::from_raw(context.cast::<TaskExecution>()) });
    execution.notify();
}

unsafe extern "C" fn retain(context: *mut c_void) {
    // SAFETY: a live descriptor owner retains this allocation during the call.
    unsafe { Arc::increment_strong_count(context.cast::<TaskExecution>()) };
}

unsafe extern "C" fn release(context: *mut c_void) {
    // SAFETY: consume exactly one descriptor reference.
    unsafe { Arc::decrement_strong_count(context.cast::<TaskExecution>()) };
}
