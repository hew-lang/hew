use super::*;
use crate::callable::{hew_callable_env_alloc, HewCallableDescriptor};
use crate::coro_state::{hew_coro_state_finish, hew_coro_state_is_cancelled};
use crate::wake::blocking::Readiness;
use hew_cabi::value::HewTypeOwnershipKind;
use std::sync::{mpsc, Condvar};
use std::time::Duration;

enum ResultValue {
    Scalar(i64),
    Owned,
}

struct Environment {
    ready: mpsc::Sender<()>,
    gate: Arc<(Mutex<bool>, Condvar)>,
    drops: Arc<std::sync::atomic::AtomicUsize>,
    value: ResultValue,
}

unsafe extern "C" fn drop_environment(raw: *mut c_void) {
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
            fault.write(crate::fault::hew_fault_new(HEW_FAULT_CANCELLED).cast());
            HEW_FAULT_CANCELLED
        } else {
            match env.value {
                ResultValue::Scalar(value) => output.cast::<i64>().write(value),
                ResultValue::Owned => output
                    .cast::<*mut c_void>()
                    .write(Box::into_raw(Box::new(Arc::clone(&env.drops))).cast()),
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
    size: size_of::<Environment>(),
    align: align_of::<Environment>(),
    ownership_kind: HewTypeOwnershipKind::LayoutManaged,
    clone_fn: None,
    drop_fn: Some(drop_environment),
};
static RESULT: HewValueLayout = HewValueLayout {
    size: size_of::<i64>(),
    align: align_of::<i64>(),
    ownership_kind: HewTypeOwnershipKind::Plain,
    clone_fn: None,
    drop_fn: None,
};
unsafe extern "C" fn drop_result(slot: *mut c_void) {
    // SAFETY: the owned-result descriptor supplies one Box<Arc<AtomicUsize>> pointer.
    unsafe {
        let pointer = *slot.cast::<*mut Arc<std::sync::atomic::AtomicUsize>>();
        let counter = Box::from_raw(pointer);
        counter.fetch_add(10, Ordering::SeqCst);
    }
}
static OWNED_RESULT: HewValueLayout = HewValueLayout {
    size: size_of::<*mut c_void>(),
    align: align_of::<*mut c_void>(),
    ownership_kind: HewTypeOwnershipKind::LayoutManaged,
    clone_fn: None,
    drop_fn: Some(drop_result),
};
const DESCRIPTOR: HewCallableDescriptor = HewCallableDescriptor {
    environment: &raw const ENVIRONMENT,
    invoke_borrow: None,
    invoke_once: invoke,
};

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
            ResultValue::Scalar(_) => &RESULT,
            ResultValue::Owned => &OWNED_RESULT,
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
        let task = hew_checked_task_spawn(scope, &raw mut callable, ptr::from_ref(layout));
        assert!(callable.environment.is_null());
        assert!(callable.descriptor.is_null());
        task
    }
}

#[test]
fn pending_scope_drain_preserves_completed_child_faults() {
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
        receive.recv_timeout(Duration::from_secs(2)).unwrap();
        receive.recv_timeout(Duration::from_secs(2)).unwrap();
        let observation = hew_checked_task_wait_new(completed, waker.descriptor());
        let drain = hew_checked_scope_wait_new(scope, waker.descriptor());
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
        receive.recv_timeout(Duration::from_secs(2)).unwrap();
        let wait = hew_checked_scope_wait_new(scope, waker.descriptor());
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
        receive.recv_timeout(Duration::from_secs(2)).unwrap();
        receive.recv_timeout(Duration::from_secs(2)).unwrap();
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
                hew_checked_task_wait_take(wait, (&raw mut output).cast(), &raw mut fault),
                READY
            );
            assert_eq!(output, expected);
            assert!(fault.is_null());
            output = -7;
            assert_eq!(
                hew_checked_task_wait_take(wait, (&raw mut output).cast(), &raw mut fault),
                TAKEN
            );
            assert_eq!(output, -7);
            hew_checked_task_wait_free(wait);
        }
        let drain = hew_checked_scope_wait_new(scope, waker.descriptor());
        assert_eq!(hew_checked_scope_wait_status(drain), 1);
        hew_checked_scope_wait_free(drain);
        hew_checked_scope_close(scope);
        assert_eq!(drops.load(Ordering::SeqCst), 2);
    }
}

#[test]
fn closing_scope_releases_unobserved_result_before_remaining_handle() {
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
        receive.recv_timeout(Duration::from_secs(2)).unwrap();
        let drain = hew_checked_scope_wait_new(scope, waker.descriptor());
        release(&gate);
        while hew_checked_scope_wait_status(drain) == 0 {
            readiness.wait();
        }
        assert_eq!(drops.load(Ordering::SeqCst), 1);
        hew_checked_scope_wait_free(drain);
        hew_checked_scope_close(scope);
        assert_eq!(drops.load(Ordering::SeqCst), 11);
        hew_task_free(task);
        assert_eq!(drops.load(Ordering::SeqCst), 11);
    }
}
