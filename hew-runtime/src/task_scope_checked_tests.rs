use super::*;
use crate::callable::{hew_callable_drop, hew_callable_env_alloc, HewCallableDescriptor};
use crate::coro_state::{hew_coro_state_finish, hew_coro_state_is_cancelled};
use crate::task_scope::hew_task_free;
use crate::wake::blocking::Readiness;
use hew_cabi::value::HewTypeOwnershipKind;
use std::sync::{mpsc, Condvar};

struct TaskRuntime {
    _guard: crate::RuntimeTestGuard,
}

impl TaskRuntime {
    fn new() -> Self {
        let guard = crate::runtime_test_guard();
        crate::scheduler::init_real_scheduler_for_test();
        Self { _guard: guard }
    }
}

impl Drop for TaskRuntime {
    fn drop(&mut self) {
        crate::scheduler::hew_runtime_cleanup();
    }
}

enum ResultValue {
    Scalar(i64),
    Owned,
    Closing(bool),
    Failure(i32),
}

struct Environment {
    ready: mpsc::Sender<()>,
    gate: Arc<(Mutex<bool>, Condvar)>,
    drops: Arc<std::sync::atomic::AtomicUsize>,
    value: ResultValue,
}

unsafe extern "C-unwind" fn drop_environment(raw: *mut c_void) {
    // SAFETY: descriptor gives this callback one initialized Environment.
    unsafe {
        let env = &*raw.cast::<Environment>();
        env.drops.fetch_add(1, Ordering::SeqCst);
        ptr::drop_in_place(raw.cast::<Environment>());
    }
}

unsafe extern "C" fn invoke(
    raw: *mut c_void,
    _: *const *mut c_void,
    output: *mut c_void,
    fault: *mut *mut c_void,
    state: *mut c_void,
) -> *mut c_void {
    // SAFETY: the test transfers a matching environment and i64 result slots.
    unsafe {
        let env = &*raw.cast::<Environment>();
        env.ready.send(()).unwrap();
        let (lock, condition) = &*env.gate;
        let guard = condition
            .wait_while(lock.lock_or_recover(), |open| !*open)
            .unwrap();
        drop(guard);
        let status = if hew_coro_state_is_cancelled(state.cast()) != 0 {
            let code = crate::coro_state::hew_coro_state_cancel_code(state.cast());
            fault.write(crate::fault::hew_fault_new(code).cast());
            code
        } else if let ResultValue::Failure(code) = env.value {
            fault.write(crate::fault::hew_fault_new(code).cast());
            code
        } else {
            match env.value {
                ResultValue::Scalar(value) => output.cast::<i64>().write(value),
                ResultValue::Owned => output
                    .cast::<*mut c_void>()
                    .write(Box::into_raw(Box::new(Arc::clone(&env.drops))).cast()),
                ResultValue::Closing(fail) => {
                    output
                        .cast::<*mut CloseResult>()
                        .write(Box::into_raw(Box::new(CloseResult {
                            drops: Arc::clone(&env.drops),
                            polls: 0,
                            fail,
                        })));
                }
                ResultValue::Failure(_) => unreachable!(),
            }
            0
        };
        let mut owner = HewCallableValue {
            environment: raw,
            descriptor: ptr::from_ref(&DESCRIPTOR),
        };
        hew_callable_drop(&raw mut owner);
        hew_coro_state_finish(state.cast(), status);
    }
    ptr::null_mut()
}

static ENVIRONMENT: HewValueLayout = HewValueLayout {
    code_owner: core::ptr::null(),
    managed: core::ptr::null(),
    release_start: None,
    size: size_of::<Environment>(),
    align: align_of::<Environment>(),
    ownership_kind: HewTypeOwnershipKind::LayoutManaged,
    clone_fn: None,
    drop_fn: Some(drop_environment),
};
static RESULT: HewValueLayout = HewValueLayout {
    code_owner: core::ptr::null(),
    managed: core::ptr::null(),
    release_start: None,
    size: size_of::<i64>(),
    align: align_of::<i64>(),
    ownership_kind: HewTypeOwnershipKind::Plain,
    clone_fn: None,
    drop_fn: None,
};
unsafe extern "C-unwind" fn drop_result(slot: *mut c_void) {
    // SAFETY: the owned-result descriptor supplies one Box<Arc<AtomicUsize>> pointer.
    unsafe {
        let pointer = *slot.cast::<*mut Arc<std::sync::atomic::AtomicUsize>>();
        let counter = Box::from_raw(pointer);
        counter.fetch_add(10, Ordering::SeqCst);
    }
}
static OWNED_RESULT: HewValueLayout = HewValueLayout {
    code_owner: core::ptr::null(),
    managed: core::ptr::null(),
    release_start: None,
    size: size_of::<*mut c_void>(),
    align: align_of::<*mut c_void>(),
    ownership_kind: HewTypeOwnershipKind::LayoutManaged,
    clone_fn: None,
    drop_fn: Some(drop_result),
};
const DESCRIPTOR: HewCallableDescriptor = HewCallableDescriptor {
    environment: &raw const ENVIRONMENT,
    invoke_borrow: None,
    invoke_once: Some(invoke),
    invoke_borrow_sync: None,
    invoke_once_sync: None,
    specializations: core::ptr::null(),
};

struct CloseResult {
    drops: Arc<std::sync::atomic::AtomicUsize>,
    polls: usize,
    fail: bool,
}

#[repr(C)]
struct CloseFrame {
    resume: Option<unsafe extern "C-unwind" fn(*mut c_void)>,
    destroy: Option<unsafe extern "C-unwind" fn(*mut c_void)>,
    owner: *mut CloseResult,
    state: *mut HewCoroState,
    fault: *mut *mut c_void,
}

unsafe extern "C-unwind" fn resume_close_result(frame: *mut c_void) {
    // SAFETY: the consuming callback allocated this exact continuation prefix.
    unsafe {
        let frame = &mut *frame.cast::<CloseFrame>();
        assert_eq!(
            hew_coro_state_is_cancelled(frame.state),
            0,
            "result cleanup is shielded"
        );
        let mut owner = Box::from_raw(frame.owner);
        assert_eq!(owner.polls, 1);
        owner.polls = 2;
        owner.drops.fetch_add(1100, Ordering::SeqCst);
        let status = if owner.fail {
            frame.fault.write(crate::fault::hew_fault_new(213).cast());
            213
        } else {
            0
        };
        hew_coro_state_finish(frame.state, status);
        frame.resume = None;
    }
}

unsafe extern "C-unwind" fn destroy_close_result(frame: *mut c_void) {
    // SAFETY: completion consumed the semantic owner; this frees only the frame.
    unsafe {
        assert!((*frame.cast::<CloseFrame>()).resume.is_none());
        crate::cont::hew_cont_frame_free(frame);
    }
}

unsafe extern "C" fn release_result(
    slot: *mut c_void,
    fault: *mut *mut c_void,
    state: *mut c_void,
) -> *mut c_void {
    // SAFETY: the task transfers one CloseResult owner and retains callback outputs.
    unsafe {
        let owner = ptr::replace(slot.cast::<*mut CloseResult>(), ptr::null_mut());
        (*owner).polls = 1;
        let frame =
            crate::cont::hew_cont_frame_alloc(size_of::<CloseFrame>() as u64).cast::<CloseFrame>();
        frame.write(CloseFrame {
            resume: Some(resume_close_result),
            destroy: Some(destroy_close_result),
            owner,
            state: state.cast(),
            fault,
        });
        let waker = crate::coro_state::hew_coro_state_waker(state.cast());
        ((*waker).wake)((*waker).context);
        frame.cast()
    }
}

static CLOSE_RESULT: HewValueLayout = HewValueLayout {
    code_owner: core::ptr::null(),
    managed: core::ptr::null(),
    size: size_of::<*mut c_void>(),
    align: align_of::<*mut c_void>(),
    ownership_kind: HewTypeOwnershipKind::LayoutManaged,
    clone_fn: None,
    drop_fn: None,

    release_start: Some(release_result),
};

#[test]
fn scope_retains_pending_result_cleanup_and_transfers_its_fault_once() {
    let _runtime = TaskRuntime::new();
    let (readiness, waker) = Readiness::new();
    let (started, receive) = mpsc::channel();
    let gate = Arc::new((Mutex::new(false), Condvar::new()));
    let drops = Arc::new(std::sync::atomic::AtomicUsize::new(0));
    // SAFETY: the test retains its observation through the completed drain.
    unsafe {
        let scope = hew_checked_scope_new(ptr::null_mut());
        let task = spawn(
            scope,
            started,
            Arc::clone(&gate),
            Arc::clone(&drops),
            ResultValue::Closing(true),
        );
        let observation = hew_checked_task_wait_new(task, waker.descriptor());
        receive.recv().unwrap();
        release(&gate);
        while hew_checked_task_wait_status(observation) == PENDING {
            readiness.wait();
        }
        let drain = hew_checked_scope_wait_new(scope, waker.descriptor(), ptr::null());
        let mut fault = ptr::null_mut();
        assert_eq!(
            hew_checked_scope_wait_take_fault(drain, &raw mut fault),
            PENDING
        );
        assert!(fault.is_null());
        assert_eq!(drops.load(Ordering::SeqCst), 1);
        assert_eq!(hew_checked_task_wait_status(observation), TAKEN);
        hew_checked_scope_cancel(scope);
        assert_eq!(hew_checked_scope_wait_status(drain), READY);
        assert_eq!(drops.load(Ordering::SeqCst), 1101);
        assert_eq!(
            hew_checked_scope_wait_take_fault(drain, &raw mut fault),
            213
        );
        hew_fault_drop(fault);
        fault = ptr::null_mut();
        assert_eq!(hew_checked_scope_wait_take_fault(drain, &raw mut fault), 0);
        assert!(fault.is_null());
        hew_checked_scope_wait_free(drain);
        hew_checked_scope_close(scope);
        hew_checked_task_wait_free(observation);
        assert_eq!(drops.load(Ordering::SeqCst), 1101);
    }
}

#[test]
fn discarded_await_result_closes_before_observer_release() {
    let _runtime = TaskRuntime::new();
    let (readiness, waker) = Readiness::new();
    let (started, receive) = mpsc::channel();
    let gate = Arc::new((Mutex::new(false), Condvar::new()));
    let drops = Arc::new(std::sync::atomic::AtomicUsize::new(0));
    // SAFETY: each scope, task, cursor and invocation has exactly one cleanup owner.
    unsafe {
        let scope = hew_checked_scope_new(ptr::null_mut());
        let task = spawn(
            scope,
            started,
            Arc::clone(&gate),
            Arc::clone(&drops),
            ResultValue::Closing(false),
        );
        let wait = hew_checked_task_wait_new(task, waker.descriptor());
        receive.recv().unwrap();
        release(&gate);
        while hew_checked_task_wait_status(wait) == PENDING {
            readiness.wait();
        }
        let mut fault = ptr::null_mut();
        let mut cursor = ptr::null_mut();
        assert_eq!(
            hew_checked_task_wait_take(wait, ptr::null_mut(), &raw mut fault, &raw mut cursor),
            READY
        );
        assert!(!cursor.is_null());
        let parent = hew_coro_state_new(waker.descriptor(), ptr::null_mut());
        let mut driver = ReleaseDriver::new(cursor);
        assert!(!driver.poll(parent));
        assert_eq!(drops.load(Ordering::SeqCst), 1);
        crate::coro_state::hew_coro_state_cancel(parent);
        assert!(driver.poll(parent));
        assert!(driver.take_fault().is_null());
        assert_eq!(drops.load(Ordering::SeqCst), 1101);
        hew_coro_state_free(parent);
        hew_checked_task_wait_free(wait);
        let drain = hew_checked_scope_wait_new(scope, waker.descriptor(), ptr::null());
        assert_eq!(hew_checked_scope_wait_status(drain), READY);
        hew_checked_scope_wait_free(drain);
        hew_checked_scope_close(scope);
        assert_eq!(drops.load(Ordering::SeqCst), 1101);
    }
}

#[test]
fn transferred_result_is_closed_only_by_its_new_owner() {
    let _runtime = TaskRuntime::new();
    let (readiness, waker) = Readiness::new();
    let (started, receive) = mpsc::channel();
    let gate = Arc::new((Mutex::new(false), Condvar::new()));
    let drops = Arc::new(std::sync::atomic::AtomicUsize::new(0));
    // SAFETY: the transferred owner remains live after its old task is released.
    unsafe {
        let scope = hew_checked_scope_new(ptr::null_mut());
        let task = spawn(
            scope,
            started,
            Arc::clone(&gate),
            Arc::clone(&drops),
            ResultValue::Closing(false),
        );
        let observation = hew_checked_task_wait_new(task, waker.descriptor());
        receive.recv().unwrap();
        release(&gate);
        while hew_checked_task_wait_status(observation) == PENDING {
            readiness.wait();
        }
        let mut result: *mut CloseResult = ptr::null_mut();
        let mut fault = ptr::null_mut();
        assert_eq!(
            hew_checked_task_wait_take(
                observation,
                (&raw mut result).cast(),
                &raw mut fault,
                ptr::null_mut()
            ),
            READY
        );
        hew_checked_task_wait_free(observation);
        let drain = hew_checked_scope_wait_new(scope, waker.descriptor(), ptr::null());
        assert_eq!(hew_checked_scope_wait_status(drain), READY);
        hew_checked_scope_wait_free(drain);
        hew_checked_scope_close(scope);
        assert_eq!(drops.load(Ordering::SeqCst), 1);
        assert_eq!((*result).polls, 0);
        let state = hew_coro_state_new(waker.descriptor(), ptr::null_mut());
        let cursor = HewReleaseCursor::values([((&raw mut result).cast(), CLOSE_RESULT)]);
        let mut driver = ReleaseDriver::new(cursor);
        assert!(!driver.poll(state));
        assert!(driver.poll(state));
        assert!(driver.take_fault().is_null());
        hew_coro_state_free(state);
        assert_eq!(drops.load(Ordering::SeqCst), 1101);
    }
}

#[test]
fn child_fault_remains_primary_while_result_cleanup_is_pending() {
    let _runtime = TaskRuntime::new();
    let (readiness, waker) = Readiness::new();
    let (started, receive) = mpsc::channel();
    let gate = Arc::new((Mutex::new(false), Condvar::new()));
    let drops = Arc::new(std::sync::atomic::AtomicUsize::new(0));
    // SAFETY: both children complete before cancellation, preserving one value
    // and one primary failure; the drain retains both through pending cleanup.
    unsafe {
        let scope = hew_checked_scope_new(ptr::null_mut());
        let value = spawn(
            scope,
            started.clone(),
            Arc::clone(&gate),
            Arc::clone(&drops),
            ResultValue::Closing(true),
        );
        let failed = spawn(
            scope,
            started,
            Arc::clone(&gate),
            Arc::clone(&drops),
            ResultValue::Failure(212),
        );
        let value = hew_checked_task_wait_new(value, waker.descriptor());
        let failed = hew_checked_task_wait_new(failed, waker.descriptor());
        receive.recv().unwrap();
        receive.recv().unwrap();
        release(&gate);
        for observation in [value, failed] {
            while hew_checked_task_wait_status(observation) == PENDING {
                readiness.wait();
            }
        }
        let drain = hew_checked_scope_wait_new(scope, waker.descriptor(), ptr::null());
        let mut fault = ptr::null_mut();
        assert_eq!(
            hew_checked_scope_wait_take_fault(drain, &raw mut fault),
            PENDING
        );
        assert!(fault.is_null());
        assert_eq!(hew_checked_task_wait_status(failed), FAULT);
        assert_eq!(drops.load(Ordering::SeqCst), 2);
        assert_eq!(
            hew_checked_scope_wait_take_fault(drain, &raw mut fault),
            212
        );
        assert_eq!((*fault).code(), 212);
        assert_eq!(drops.load(Ordering::SeqCst), 1102);
        let diagnostic = Box::from_raw(crate::fault::hew_fault_into_host_error(fault));
        let report = diagnostic.message();
        assert!(report.find("(212)").unwrap() < report.find("(213)").unwrap());
        hew_checked_task_wait_free(value);
        hew_checked_task_wait_free(failed);
        hew_checked_scope_wait_free(drain);
        hew_checked_scope_close(scope);
    }
}

fn release(gate: &Arc<(Mutex<bool>, Condvar)>) {
    *gate.0.lock_or_recover() = true;
    gate.1.notify_all();
}

unsafe fn spawn(
    scope: *mut HewTaskScope,
    ready: mpsc::Sender<()>,
    gate: Arc<(Mutex<bool>, Condvar)>,
    drops: Arc<std::sync::atomic::AtomicUsize>,
    value: ResultValue,
) -> *mut HewTask {
    // SAFETY: the descriptor's exact allocation receives one owned environment.
    unsafe {
        let layout = match value {
            ResultValue::Scalar(_) | ResultValue::Failure(_) => &RESULT,
            ResultValue::Owned => &OWNED_RESULT,
            ResultValue::Closing(_) => &CLOSE_RESULT,
        };
        let raw = hew_callable_env_alloc(ptr::from_ref(&DESCRIPTOR));
        raw.cast::<Environment>().write(Environment {
            ready,
            gate,
            drops,
            value,
        });
        let mut callable = HewCallableValue {
            environment: raw,
            descriptor: ptr::from_ref(&DESCRIPTOR),
        };
        let task = hew_checked_task_spawn(
            scope,
            &raw mut callable,
            ptr::from_ref(layout),
            8,
            ptr::null(),
        );
        assert!(callable.environment.is_null());
        assert!(callable.descriptor.is_null());
        hew_checked_task_start(task);
        task
    }
}

#[test]
#[expect(
    clippy::too_many_lines,
    reason = "one task creation, execution and scope-release lifecycle"
)]
fn managed_task_starts_after_adoption_and_keeps_result_slot_through_scope() {
    use hew_cabi::managed::HewManagedValueDescriptor;
    static CALLS: AtomicU64 = AtomicU64::new(0);
    static PLAIN: HewManagedValueDescriptor = HewManagedValueDescriptor {
        slot_bytes: 8,
        visit: None,
    };
    static EMPTY: HewManagedValueDescriptor = HewManagedValueDescriptor {
        slot_bytes: 0,
        visit: None,
    };
    static TASK: HewManagedValueDescriptor = HewManagedValueDescriptor {
        slot_bytes: 8,
        visit: Some(hew_managed_visit_task),
    };
    static RESULT_LAYOUT: HewValueLayout = HewValueLayout {
        managed: &raw const PLAIN,
        ..RESULT
    };
    static ENV_LAYOUT: HewValueLayout = HewValueLayout {
        size: 0,
        align: 1,
        managed: &raw const EMPTY,
        ..RESULT
    };
    static TASK_LAYOUT: HewValueLayout = HewValueLayout {
        size: size_of::<*mut HewTask>(),
        align: align_of::<*mut HewTask>(),
        managed: &raw const TASK,
        ..RESULT
    };
    unsafe extern "C" fn run(
        _: *mut c_void,
        _: *const *mut c_void,
        output: *mut c_void,
        _: *mut *mut c_void,
        state: *mut c_void,
    ) -> *mut c_void {
        CALLS.fetch_add(1, Ordering::SeqCst);
        // SAFETY: this fixture has the checked i64 result and invocation ABI.
        unsafe {
            output.cast::<i64>().write(42);
            hew_coro_state_finish(state.cast(), 0);
        }
        ptr::null_mut()
    }
    const CALLABLE: HewCallableDescriptor = HewCallableDescriptor {
        environment: &raw const ENV_LAYOUT,
        invoke_once: Some(run),
        ..DESCRIPTOR
    };
    let runtime = TaskRuntime::new();
    let owner = crate::owned_value_budget::ValueOwner::new(Some(16));
    let (readiness, waker) = Readiness::new();
    // SAFETY: all static descriptors match their slots and the scope drains
    // before runtime shutdown; this fixture models the compiler's preflight.
    unsafe {
        let scope = hew_checked_scope_new(ptr::null_mut());
        (*scope).value_owner = Some(owner.clone());
        let mut callable = HewCallableValue {
            environment: ptr::null_mut(),
            descriptor: &CALLABLE,
        };
        let reservation = owner.reserve_new(16).unwrap();
        let task = hew_checked_task_spawn(
            scope,
            &raw mut callable,
            &raw const RESULT_LAYOUT,
            8,
            &raw const TASK_LAYOUT,
        );
        assert_eq!(CALLS.load(Ordering::SeqCst), 0);
        reservation
            .commit_value(&TASK_LAYOUT, (&raw const task).cast())
            .unwrap();
        assert_eq!(owner.live_bytes(), 16);
        hew_checked_task_start(task);
        let wait = hew_checked_task_wait_new(task, waker.descriptor());
        while hew_checked_task_wait_status(wait) == PENDING {
            readiness.wait();
        }
        assert_eq!(CALLS.load(Ordering::SeqCst), 1);
        assert_eq!(owner.live_bytes(), 8);
        let mut value = 0_i64;
        let mut fault = ptr::null_mut();
        assert_eq!(
            hew_checked_task_wait_take(
                wait,
                (&raw mut value).cast(),
                &raw mut fault,
                ptr::null_mut()
            ),
            READY
        );
        assert_eq!(value, 42);
        assert!(fault.is_null());
        hew_checked_task_wait_free(wait);
        assert_eq!(owner.live_bytes(), 8, "scope still retains result storage");
        let drain = hew_checked_scope_wait_new(scope, waker.descriptor(), ptr::null());
        assert_eq!(hew_checked_scope_wait_status(drain), READY);
        hew_checked_scope_wait_free(drain);
        hew_checked_scope_close(scope);
    }
    drop(runtime);
    assert_eq!(owner.live_bytes(), 0);
}

#[test]
fn completed_task_observations_poll_without_rescheduling_their_caller() {
    let _runtime = TaskRuntime::new();
    let (readiness, waker) = Readiness::new();
    let (started, receive) = mpsc::channel();
    let gate = Arc::new((Mutex::new(false), Condvar::new()));
    let drops = Arc::new(std::sync::atomic::AtomicUsize::new(0));
    // SAFETY: the initial wait consumes the source handle; additional observations
    // retain it independently, and the scope stays live through their release.
    unsafe {
        let scope = hew_checked_scope_new(ptr::null_mut());
        let task = spawn(
            scope,
            started,
            Arc::clone(&gate),
            drops,
            ResultValue::Scalar(42),
        );
        receive.recv().unwrap();
        let pending = hew_checked_task_wait_new(task, waker.descriptor());
        release(&gate);
        readiness.wait();
        assert_eq!(hew_checked_task_wait_status(pending), READY);

        let selection = hew_checked_task_select_new(waker.descriptor());
        hew_checked_task_select_add_task(selection, task);
        assert!(!readiness.take_ready());
        assert_eq!(hew_checked_task_select_poll(selection), 0);
        hew_checked_task_select_free(selection);

        retain(task);
        let completed = hew_checked_task_wait_new(task, waker.descriptor());
        assert!(!readiness.take_ready());
        assert_eq!(hew_checked_task_wait_status(completed), READY);
        let drain = hew_checked_scope_wait_new(scope, waker.descriptor(), ptr::null());
        assert!(!readiness.take_ready());
        let mut value = 0_i64;
        let mut fault = ptr::null_mut();
        assert_eq!(
            hew_checked_task_wait_take(
                completed,
                (&raw mut value).cast(),
                &raw mut fault,
                ptr::null_mut()
            ),
            READY
        );
        assert_eq!(value, 42);
        assert!(fault.is_null());
        assert_eq!(hew_checked_scope_wait_status(drain), READY);
        hew_checked_task_wait_free(completed);
        hew_checked_task_wait_free(pending);
        hew_checked_scope_wait_free(drain);
        hew_checked_scope_close(scope);
    }
}

#[test]
fn pending_scope_drain_preserves_completed_child_faults() {
    let _runtime = TaskRuntime::new();
    let (readiness, waker) = Readiness::new();
    let (started, receive) = mpsc::channel();
    let pending_gate = Arc::new((Mutex::new(false), Condvar::new()));
    let completed_gate = Arc::new((Mutex::new(false), Condvar::new()));
    let drops = Arc::new(std::sync::atomic::AtomicUsize::new(0));
    // SAFETY: the test retains all observations until both workers have stopped.
    unsafe {
        let scope = hew_checked_scope_new(ptr::null_mut());
        let pending = spawn(
            scope,
            started.clone(),
            Arc::clone(&pending_gate),
            Arc::clone(&drops),
            ResultValue::Scalar(17),
        );
        let completed = spawn(
            scope,
            started,
            Arc::clone(&completed_gate),
            Arc::clone(&drops),
            ResultValue::Scalar(42),
        );
        receive.recv().unwrap();
        receive.recv().unwrap();
        let observation = hew_checked_task_wait_new(completed, waker.descriptor());
        let drain = hew_checked_scope_wait_new(scope, waker.descriptor(), ptr::null());
        hew_checked_scope_cancel(scope);
        release(&completed_gate);
        while hew_checked_task_wait_status(observation) == PENDING {
            readiness.wait();
        }
        let mut fault = ptr::null_mut();
        assert_eq!(
            hew_checked_scope_wait_take_fault(drain, &raw mut fault),
            PENDING
        );
        assert!(fault.is_null());
        assert_eq!(hew_checked_task_wait_status(observation), CANCELLED);
        release(&pending_gate);
        while hew_checked_scope_wait_status(drain) == 0 {
            readiness.wait();
        }
        assert_eq!(
            hew_checked_scope_wait_take_fault(drain, &raw mut fault),
            HEW_FAULT_CANCELLED
        );
        assert!(!fault.is_null());
        assert_eq!(hew_checked_task_wait_status(observation), TAKEN);
        hew_fault_drop(fault);
        hew_checked_task_wait_free(observation);
        hew_task_free(pending);
        hew_checked_scope_wait_free(drain);
        hew_checked_scope_close(scope);
        assert_eq!(drops.load(Ordering::SeqCst), 2);
    }
}

#[test]
fn cancellation_does_not_release_captures_before_the_child_stops() {
    let _runtime = TaskRuntime::new();
    let (readiness, waker) = Readiness::new();
    let (started, receive) = mpsc::channel();
    let gate = Arc::new((Mutex::new(false), Condvar::new()));
    let drops = Arc::new(std::sync::atomic::AtomicUsize::new(0));
    // SAFETY: this test owns the scope/handle/wait and drains before closing.
    unsafe {
        let scope = hew_checked_scope_new(ptr::null_mut());
        let task = spawn(
            scope,
            started,
            Arc::clone(&gate),
            Arc::clone(&drops),
            ResultValue::Scalar(42),
        );
        receive.recv().unwrap();
        let wait = hew_checked_scope_wait_new(scope, waker.descriptor(), ptr::null());
        hew_task_free(task); // unawaited handle does not end scope-owned execution
        hew_checked_scope_cancel(scope);
        assert_eq!(hew_checked_scope_wait_status(wait), 0);
        assert_eq!(drops.load(Ordering::SeqCst), 0);
        release(&gate);
        while hew_checked_scope_wait_status(wait) == 0 {
            readiness.wait();
        }
        assert_eq!(drops.load(Ordering::SeqCst), 1);
        let mut fault = ptr::null_mut();
        assert_eq!(
            hew_checked_scope_wait_take_fault(wait, &raw mut fault),
            HEW_FAULT_CANCELLED
        );
        assert!(!fault.is_null());
        hew_fault_drop(fault);
        hew_checked_scope_wait_free(wait);
        hew_checked_scope_close(scope);
    }
}

#[test]
fn children_start_concurrently_and_each_result_transfers_once() {
    let _runtime = TaskRuntime::new();
    let (readiness, waker) = Readiness::new();
    let (started, receive) = mpsc::channel();
    let gate = Arc::new((Mutex::new(false), Condvar::new()));
    let drops = Arc::new(std::sync::atomic::AtomicUsize::new(0));
    // SAFETY: each source handle moves into one wait; scope outlives all waits.
    unsafe {
        let scope = hew_checked_scope_new(ptr::null_mut());
        let first = spawn(
            scope,
            started.clone(),
            Arc::clone(&gate),
            Arc::clone(&drops),
            ResultValue::Scalar(17),
        );
        let second = spawn(
            scope,
            started,
            Arc::clone(&gate),
            Arc::clone(&drops),
            ResultValue::Scalar(42),
        );
        receive.recv().unwrap();
        receive.recv().unwrap();
        let first = hew_checked_task_wait_new(first, waker.descriptor());
        let second = hew_checked_task_wait_new(second, waker.descriptor());
        release(&gate);
        for (wait, expected) in [(first, 17), (second, 42)] {
            while hew_checked_task_wait_status(wait) == PENDING {
                readiness.wait();
            }
            let mut output = -1_i64;
            let mut fault = ptr::null_mut();
            assert_eq!(
                hew_checked_task_wait_take(
                    wait,
                    (&raw mut output).cast(),
                    &raw mut fault,
                    ptr::null_mut()
                ),
                READY
            );
            assert_eq!(output, expected);
            assert!(fault.is_null());
            output = -7;
            assert_eq!(
                hew_checked_task_wait_take(
                    wait,
                    (&raw mut output).cast(),
                    &raw mut fault,
                    ptr::null_mut()
                ),
                TAKEN
            );
            assert_eq!(output, -7);
            hew_checked_task_wait_free(wait);
        }
        let drain = hew_checked_scope_wait_new(scope, waker.descriptor(), ptr::null());
        assert_eq!(hew_checked_scope_wait_status(drain), 1);
        hew_checked_scope_wait_free(drain);
        hew_checked_scope_close(scope);
        assert_eq!(drops.load(Ordering::SeqCst), 2);
    }
}

#[test]
fn closing_scope_releases_unobserved_result_before_remaining_handle() {
    let _runtime = TaskRuntime::new();
    let (readiness, waker) = Readiness::new();
    let (started, receive) = mpsc::channel();
    let gate = Arc::new((Mutex::new(false), Condvar::new()));
    let drops = Arc::new(std::sync::atomic::AtomicUsize::new(0));
    // SAFETY: the test retains its source handle across scope close solely to
    // check that closing the scope already discharged the unobserved result.
    unsafe {
        let scope = hew_checked_scope_new(ptr::null_mut());
        let task = spawn(
            scope,
            started,
            Arc::clone(&gate),
            Arc::clone(&drops),
            ResultValue::Owned,
        );
        receive.recv().unwrap();
        let drain = hew_checked_scope_wait_new(scope, waker.descriptor(), ptr::null());
        release(&gate);
        while hew_checked_scope_wait_status(drain) == 0 {
            readiness.wait();
        }
        assert_eq!(drops.load(Ordering::SeqCst), 11);
        hew_checked_scope_wait_free(drain);
        hew_checked_scope_close(scope);
        assert_eq!(drops.load(Ordering::SeqCst), 11);
        hew_task_free(task);
        assert_eq!(drops.load(Ordering::SeqCst), 11);
    }
}

#[test]
fn select_observation_retains_both_results_after_a_task_or_timer_wins() {
    let _runtime = TaskRuntime::new();
    for timeout in [false, true] {
        let (readiness, waker) = Readiness::new();
        let (started, receive) = mpsc::channel();
        let first_gate = Arc::new((Mutex::new(false), Condvar::new()));
        let second_gate = Arc::new((Mutex::new(false), Condvar::new()));
        let drops = Arc::new(std::sync::atomic::AtomicUsize::new(0));
        // SAFETY: observations borrow source handles; ordinary waits consume
        // those handles only after selection has detached, then drain the scope.
        unsafe {
            let scope = hew_checked_scope_new(ptr::null_mut());
            let tasks = [
                spawn(
                    scope,
                    started.clone(),
                    Arc::clone(&first_gate),
                    Arc::clone(&drops),
                    ResultValue::Scalar(17),
                ),
                spawn(
                    scope,
                    started,
                    Arc::clone(&second_gate),
                    Arc::clone(&drops),
                    ResultValue::Scalar(42),
                ),
            ];
            receive.recv().unwrap();
            receive.recv().unwrap();
            let selection = hew_checked_task_select_new(waker.descriptor());
            for task in tasks {
                hew_checked_task_select_add_task(selection, task);
            }
            if timeout {
                hew_checked_task_select_arm_timer(selection, 0);
            }
            if !timeout {
                assert_eq!(hew_checked_task_select_poll(selection), -1);
                release(&second_gate);
            }
            let winner = loop {
                let winner = hew_checked_task_select_poll(selection);
                if winner != -1 {
                    break winner;
                }
                readiness.wait();
            };
            assert_eq!(winner, if timeout { 2 } else { 1 });
            assert_eq!(hew_checked_task_select_poll(selection), winner);
            hew_checked_task_select_free(selection);
            release(&first_gate);
            release(&second_gate);
            for (task, expected) in tasks.into_iter().zip([17, 42]) {
                let wait = hew_checked_task_wait_new(task, waker.descriptor());
                while hew_checked_task_wait_status(wait) == PENDING {
                    readiness.wait();
                }
                let mut output = 0_i64;
                let mut fault = ptr::null_mut();
                assert_eq!(
                    hew_checked_task_wait_take(
                        wait,
                        (&raw mut output).cast(),
                        &raw mut fault,
                        ptr::null_mut()
                    ),
                    READY
                );
                assert_eq!(output, expected);
                assert!(fault.is_null());
                hew_checked_task_wait_free(wait);
            }
            let drain = hew_checked_scope_wait_new(scope, waker.descriptor(), ptr::null());
            assert_eq!(hew_checked_scope_wait_status(drain), 1);
            hew_checked_scope_wait_free(drain);
            hew_checked_scope_close(scope);
            assert_eq!(drops.load(Ordering::SeqCst), 2);
        }
    }
}

#[test]
fn race_selection_uses_completion_order_and_drain_suppresses_only_its_cancellation() {
    let _runtime = TaskRuntime::new();
    for parent_code in [0, crate::fault::HEW_FAULT_DEADLINE] {
        let (readiness, waker) = Readiness::new();
        let (started, receive) = mpsc::channel();
        let first_gate = Arc::new((Mutex::new(false), Condvar::new()));
        let second_gate = Arc::new((Mutex::new(false), Condvar::new()));
        let pending_gate = Arc::new((Mutex::new(false), Condvar::new()));
        let drops = Arc::new(std::sync::atomic::AtomicUsize::new(0));
        // SAFETY: observations and drain retain all child storage until stopped.
        unsafe {
            let parent = hew_cancel_token_new_child(ptr::null_mut());
            let scope = hew_checked_scope_new(parent);
            let first = spawn(
                scope,
                started.clone(),
                Arc::clone(&first_gate),
                Arc::clone(&drops),
                ResultValue::Scalar(1),
            );
            let second = spawn(
                scope,
                started.clone(),
                Arc::clone(&second_gate),
                Arc::clone(&drops),
                ResultValue::Scalar(2),
            );
            let pending = spawn(
                scope,
                started,
                Arc::clone(&pending_gate),
                Arc::clone(&drops),
                ResultValue::Scalar(3),
            );
            // Two deliberately blocking test callbacks occupy both workers.
            // Complete the second before waiting for the queued third to start.
            for _ in 0..2 {
                receive.recv().unwrap();
            }
            release(&second_gate);
            receive.recv().unwrap();
            let first_wait = hew_checked_task_wait_new(first, waker.descriptor());
            let second_wait = hew_checked_task_wait_new(second, waker.descriptor());
            release(&second_gate);
            while hew_checked_task_wait_status(second_wait) == PENDING {
                readiness.wait();
            }
            release(&first_gate);
            while hew_checked_task_wait_status(first_wait) == PENDING {
                readiness.wait();
            }
            let handles = [first, second, pending];
            let select = hew_checked_task_select_new(waker.descriptor());
            for handle in handles {
                hew_checked_task_select_add_task(select, handle);
            }
            assert_eq!(hew_checked_task_select_poll(select), 0);
            assert_eq!(hew_checked_task_select_poll_first(select), 1);
            hew_checked_task_select_free(select);
            let drain = hew_checked_scope_wait_new(scope, waker.descriptor(), ptr::null());
            hew_checked_scope_wait_cancel_losers(drain);
            assert_eq!(
                crate::cancel_token::cancel_token_reason((*scope).cancel_token),
                0
            );
            assert_eq!(
                crate::cancel_token::cancel_token_reason((*second).cancel_token),
                0
            );
            if parent_code != 0 {
                hew_cancel_token_cancel(parent, parent_code);
            }
            release(&pending_gate);
            while hew_checked_scope_wait_status(drain) == PENDING {
                readiness.wait();
            }
            let mut fault = ptr::null_mut();
            assert_eq!(
                hew_checked_scope_wait_take_fault(drain, &raw mut fault),
                parent_code
            );
            assert_eq!(fault.is_null(), parent_code == 0);
            hew_fault_drop(fault);
            hew_checked_scope_wait_free(drain);
            hew_checked_task_wait_free(first_wait);
            hew_checked_task_wait_free(second_wait);
            hew_task_free(pending);
            hew_checked_scope_close(scope);
            super::super::hew_cancel_token_release(parent);
            assert_eq!(drops.load(Ordering::SeqCst), 3);
        }
    }
}

#[test]
fn transfer_waits_for_running_task_and_abort_preserves_sender_execution() {
    use hew_cabi::managed::HewManagedValueDescriptor;
    static TASK_MANAGED: HewManagedValueDescriptor = HewManagedValueDescriptor {
        slot_bytes: 8,
        visit: Some(hew_managed_visit_task),
    };
    static TASK_LAYOUT: HewValueLayout = HewValueLayout {
        managed: &raw const TASK_MANAGED,
        ..RESULT
    };
    static RESULT_MANAGED: HewManagedValueDescriptor = HewManagedValueDescriptor {
        slot_bytes: 8,
        visit: None,
    };
    static MANAGED_RESULT: HewValueLayout = HewValueLayout {
        managed: &raw const RESULT_MANAGED,
        ..RESULT
    };
    let _runtime = TaskRuntime::new();
    let (readiness, waker) = Readiness::new();
    let (started, receive) = mpsc::channel();
    let gate = Arc::new((Mutex::new(false), Condvar::new()));
    let drops = Arc::new(std::sync::atomic::AtomicUsize::new(0));
    let destination = crate::owned_value_budget::ValueOwner::new(Some(64));
    // SAFETY: the scope and handle retain the running child throughout the
    // attempted transfer, and the explicit gate excludes timing assumptions.
    unsafe {
        let scope = hew_checked_scope_new(ptr::null_mut());
        let task = spawn(
            scope,
            started,
            gate.clone(),
            drops.clone(),
            ResultValue::Scalar(42),
        );
        receive.recv().unwrap();
        let execution = {
            let mut state = checked(task).lock_or_recover();
            // The fixture produces the same i64 slot; supply its budget witness.
            state.layout = &raw const MANAGED_RESULT;
            state.execution.upgrade().unwrap()
        };
        let mut transfer = crate::owned_value_budget::transfer::ValueTransfer::begin(
            None,
            Some(destination.clone()),
            &raw const TASK_LAYOUT,
            (&raw const task).cast(),
        );
        transfer.set_waker(OwnedWaker::retain(waker.descriptor()));
        assert!(
            !transfer.poll_prepare().unwrap(),
            "a running frame is never inspected"
        );
        assert_eq!(destination.live_bytes(), 0);
        drop(transfer);
        let wait = hew_checked_task_wait_new(task, waker.descriptor());
        release(&gate);
        while hew_checked_task_wait_status(wait) == PENDING {
            readiness.wait();
        }
        // Completion wakes the observer before the executor's final Arc is
        // necessarily gone. A retained executor must not make its dead frame
        // part of ordinary result acquisition or final release.
        destination
            .acquire_value(&TASK_LAYOUT, (&raw const task).cast())
            .unwrap()
            .commit();
        assert_eq!(destination.live_bytes(), 8);
        destination
            .release_value(&TASK_LAYOUT, (&raw const task).cast())
            .unwrap();
        assert_eq!(destination.live_bytes(), 0);
        drop(execution);
        let mut value = 0_i64;
        let mut fault = ptr::null_mut();
        assert_eq!(
            hew_checked_task_wait_take(
                wait,
                (&raw mut value).cast(),
                &raw mut fault,
                ptr::null_mut()
            ),
            READY
        );
        assert_eq!(value, 42);
        assert!(fault.is_null());
        hew_checked_task_wait_free(wait);
        let drain = hew_checked_scope_wait_new(scope, waker.descriptor(), ptr::null());
        assert_eq!(hew_checked_scope_wait_status(drain), READY);
        hew_checked_scope_wait_free(drain);
        hew_checked_scope_close(scope);
    }
    assert_eq!(drops.load(Ordering::SeqCst), 1);
}
