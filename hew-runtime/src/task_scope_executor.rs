//! Scheduler ownership of one checked task continuation.

use super::{checked, complete, hew_task_free, HewTask};
use crate::cont::{hew_cont_destroy, hew_cont_done, hew_cont_resume};
use crate::coro_state::{
    hew_coro_state_free, hew_coro_state_new, hew_coro_state_private_status, hew_coro_state_status,
    CoroStatus, HewCoroState,
};
use crate::execution_context::{current_context, set_current_context, HewExecutionContext};
use crate::fault::HewFault;
use crate::util::MutexExt;
use crate::wake::HewWaker;
use std::cell::UnsafeCell;
use std::ffi::c_void;
use std::ptr;
use std::sync::atomic::{AtomicU8, AtomicUsize, Ordering};
use std::sync::Arc;

const IDLE: u8 = 0;
const QUEUED: u8 = 1;
const RUNNING: u8 = 2;
const NOTIFIED: u8 = 3;
const COMPLETE: u8 = 4;
static LIVE_TASKS: AtomicUsize = AtomicUsize::new(0);

/// Readiness owns this allocation independently of the generated frame.
pub(crate) struct TaskExecution {
    phase: AtomicU8,
    driver: UnsafeCell<Driver>,
}

struct Driver {
    task: *mut HewTask,
    state: *mut HewCoroState,
    frame: *mut c_void,
    fault: *mut HewFault,
    owner: u64,
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
            phase: AtomicU8::new(QUEUED),
            driver: UnsafeCell::new(Driver {
                task,
                state: ptr::null_mut(),
                frame: ptr::null_mut(),
                fault: ptr::null_mut(),
                owner: crate::fault::owning_actor_at_spawn(),
            }),
        });
        enqueue(execution);
    }

    fn notify(self: &Arc<Self>) {
        let mut phase = self.phase.load(Ordering::Acquire);
        loop {
            let next = match phase {
                IDLE => QUEUED,
                RUNNING => NOTIFIED,
                QUEUED | NOTIFIED | COMPLETE => return,
                _ => std::process::abort(),
            };
            match self
                .phase
                .compare_exchange_weak(phase, next, Ordering::AcqRel, Ordering::Acquire)
            {
                Ok(_) => {
                    if next == QUEUED {
                        enqueue(Arc::clone(self));
                    }
                    return;
                }
                Err(observed) => phase = observed,
            }
        }
    }

    pub(crate) fn poll(self: Arc<Self>) {
        assert_eq!(self.phase.swap(RUNNING, Ordering::AcqRel), QUEUED);
        // SAFETY: taking the sole queue entry grants exclusive driver access.
        let driver = unsafe { &mut *self.driver.get() };
        let previous = current_context();
        let mut context = HewExecutionContext {
            // SAFETY: the execution reference retains task and its token/scope.
            cancel_token: unsafe { (*driver.task).cancel_token },
            // SAFETY: structured drain keeps scope alive through completion.
            task_scope: unsafe { (*driver.task).scope },
            prev_context: previous,
            ..HewExecutionContext::default()
        };
        let _ = set_current_context(&raw mut context);
        let finished = crate::fault::with_owning_actor(driver.owner, || {
            // SAFETY: this worker exclusively drives the retained continuation.
            unsafe { driver.poll(&self) }
        });
        let _ = set_current_context(previous);
        if finished {
            self.phase.store(COMPLETE, Ordering::Release);
            LIVE_TASKS.fetch_sub(1, Ordering::Release);
        } else if self
            .phase
            .compare_exchange(RUNNING, IDLE, Ordering::AcqRel, Ordering::Acquire)
            .is_err()
        {
            // A wake during ramp/resume is consumed only after the frame parks.
            assert_eq!(self.phase.swap(QUEUED, Ordering::AcqRel), NOTIFIED);
            enqueue(Arc::clone(&self));
        }
    }
}

impl Driver {
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
                let (callable, result) = {
                    let mut task = checked(self.task).lock_or_recover();
                    (
                        task.callable.take().expect("one task invocation"),
                        task.result,
                    )
                };
                self.frame = ((*callable.descriptor).invoke_once)(
                    callable.environment,
                    ptr::null(),
                    result,
                    (&raw mut self.fault).cast(),
                    self.state.cast(),
                );
            } else {
                hew_cont_resume(self.frame);
            }
            let status = hew_coro_state_status(self.state);
            if status == CoroStatus::Pending as i32 {
                if self.frame.is_null() || hew_cont_done(self.frame) {
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
            let status = hew_coro_state_private_status(self.state);
            hew_coro_state_free(self.state);
            self.state = ptr::null_mut();
            complete(
                self.task,
                status,
                std::mem::replace(&mut self.fault, ptr::null_mut()),
            );
            hew_task_free(self.task);
            self.task = ptr::null_mut();
            true
        }
    }
}

fn enqueue(execution: Arc<TaskExecution>) {
    crate::scheduler::enqueue_task(execution);
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
