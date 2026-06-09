#![expect(
    clippy::undocumented_unsafe_blocks,
    reason = "FFI runtime tests exercise raw actor lock ABI calls"
)]

use std::ffi::{c_char, c_void, CStr};
use std::sync::atomic::{AtomicI32, Ordering};
use std::sync::{Condvar, Mutex};
use std::time::{Duration, Instant};

use hew_runtime::actor::{
    hew_actor_get_error, hew_actor_state_lock_acquire, hew_actor_state_lock_poison_after_panic,
    hew_actor_state_lock_release, hew_actor_state_lock_release_after_panic,
    HEW_ACTOR_STATE_LOCK_ERR, HEW_ACTOR_STATE_LOCK_OK,
};
use hew_runtime::hew_last_error;
use hew_runtime_testkit::{ensure_scheduler, TestActor};

static TEST_LOCK: Mutex<()> = Mutex::new(());

struct DispatchSignal {
    count: Mutex<i32>,
    cond: Condvar,
}

impl DispatchSignal {
    const fn new() -> Self {
        Self {
            count: Mutex::new(0),
            cond: Condvar::new(),
        }
    }

    fn reset(&self) {
        *self.count.lock().unwrap() = 0;
    }

    fn record(&self) {
        let mut count = self.count.lock().unwrap();
        *count += 1;
        self.cond.notify_all();
    }

    fn wait_for(&self, expected: i32, timeout: Duration) -> bool {
        let deadline = Instant::now() + timeout;
        let mut count = self.count.lock().unwrap();
        while *count < expected {
            let remaining = deadline.saturating_duration_since(Instant::now());
            if remaining.is_zero() {
                return false;
            }
            let (guard, result) = self.cond.wait_timeout(count, remaining).unwrap();
            count = guard;
            if result.timed_out() && *count < expected {
                return false;
            }
        }
        true
    }
}

unsafe extern "C-unwind" fn noop_dispatch(
    _ctx: *mut hew_runtime::execution_context::HewExecutionContext,
    _state: *mut c_void,
    _msg_type: i32,
    _data: *mut c_void,
    _size: usize,
) {
}

#[test]
fn state_lock_serializes_parallel_dispatch_attempts() {
    let _test_lock = TEST_LOCK.lock().unwrap();
    let actor = TestActor::spawn(noop_dispatch);
    let actor_addr = actor.as_ptr() as usize;

    assert_eq!(
        unsafe { hew_actor_state_lock_acquire(actor.as_ptr()) },
        HEW_ACTOR_STATE_LOCK_OK
    );

    let (tx, rx) = std::sync::mpsc::channel();
    let handle = std::thread::spawn(move || {
        let actor = actor_addr as *mut hew_runtime::actor::HewActor;
        let rc = unsafe { hew_actor_state_lock_acquire(actor) };
        tx.send(rc).expect("send acquire result");
        if rc == HEW_ACTOR_STATE_LOCK_OK {
            let _ = unsafe { hew_actor_state_lock_release(actor) };
        }
    });

    assert!(
        rx.recv_timeout(Duration::from_millis(100)).is_err(),
        "second acquire must block while the first dispatch holds the lock"
    );
    assert_eq!(
        unsafe { hew_actor_state_lock_release(actor.as_ptr()) },
        HEW_ACTOR_STATE_LOCK_OK
    );
    assert_eq!(
        rx.recv_timeout(Duration::from_secs(2))
            .expect("second acquire should proceed after release"),
        HEW_ACTOR_STATE_LOCK_OK
    );
    handle.join().expect("lock waiter thread should finish");
}

#[test]
fn state_lock_released_after_dispatch_panic() {
    let _test_lock = TEST_LOCK.lock().unwrap();
    let actor = TestActor::spawn(noop_dispatch);

    assert_eq!(
        unsafe { hew_actor_state_lock_acquire(actor.as_ptr()) },
        HEW_ACTOR_STATE_LOCK_OK
    );
    assert_eq!(
        unsafe { hew_actor_state_lock_release_after_panic(actor.as_ptr()) },
        HEW_ACTOR_STATE_LOCK_OK
    );
    assert_eq!(
        unsafe { hew_actor_state_lock_acquire(actor.as_ptr()) },
        HEW_ACTOR_STATE_LOCK_OK,
        "crash recovery release must leave the actor lock acquirable"
    );
    assert_eq!(
        unsafe { hew_actor_state_lock_release(actor.as_ptr()) },
        HEW_ACTOR_STATE_LOCK_OK
    );
}

#[test]
fn poisoned_state_lock_fails_closed_with_typed_diagnostic() {
    let _test_lock = TEST_LOCK.lock().unwrap();
    let actor = TestActor::spawn(noop_dispatch);

    assert_eq!(
        unsafe { hew_actor_state_lock_acquire(actor.as_ptr()) },
        HEW_ACTOR_STATE_LOCK_OK
    );
    assert_eq!(
        unsafe { hew_actor_state_lock_poison_after_panic(actor.as_ptr()) },
        HEW_ACTOR_STATE_LOCK_OK
    );
    assert_eq!(
        unsafe { hew_actor_state_lock_acquire(actor.as_ptr()) },
        HEW_ACTOR_STATE_LOCK_ERR
    );

    let err = last_error_string();
    assert!(
        err.contains("poisoned"),
        "poisoned lock acquire should set a typed diagnostic, got {err:?}"
    );
}

static COUNTER_SIGNAL: DispatchSignal = DispatchSignal::new();
static COUNTER_ERRORS: AtomicI32 = AtomicI32::new(0);

unsafe extern "C-unwind" fn locked_counter_dispatch(
    ctx: *mut hew_runtime::execution_context::HewExecutionContext,
    state: *mut c_void,
    _msg_type: i32,
    data: *mut c_void,
    size: usize,
) {
    if ctx.is_null() || unsafe { (*ctx).lock_seat.is_null() } {
        COUNTER_ERRORS.fetch_add(1, Ordering::Relaxed);
        return;
    }
    if size == std::mem::size_of::<i32>() && !data.is_null() {
        let n = unsafe { *data.cast::<i32>() };
        let counter = state.cast::<i32>();
        unsafe { *counter += n };
    }
    COUNTER_SIGNAL.record();
}

#[test]
fn counter_actor_with_scheduler_guarded_concurrent_inc_is_linearized() {
    let _test_lock = TEST_LOCK.lock().unwrap();
    ensure_scheduler();
    COUNTER_SIGNAL.reset();
    COUNTER_ERRORS.store(0, Ordering::Relaxed);

    let mut initial = 0_i32;
    let actor = std::sync::Arc::new(TestActor::spawn_with_state(
        &mut initial,
        locked_counter_dispatch,
    ));

    let threads = 8;
    let messages_per_thread = 50;
    let mut handles = Vec::new();
    for _ in 0..threads {
        let actor = std::sync::Arc::clone(&actor);
        handles.push(std::thread::spawn(move || {
            for _ in 0..messages_per_thread {
                let mut n = 1_i32;
                actor.send(1, &mut n);
            }
        }));
    }
    for handle in handles {
        handle.join().expect("sender thread should finish");
    }

    let expected = threads * messages_per_thread;
    assert!(
        COUNTER_SIGNAL.wait_for(expected, Duration::from_secs(10)),
        "counter dispatches did not complete"
    );
    assert_eq!(
        COUNTER_ERRORS.load(Ordering::Relaxed),
        0,
        "scheduler-guarded dispatch should receive a non-null lock seat"
    );

    let final_state = unsafe { *(*actor.as_ptr()).state.cast::<i32>() };
    assert_eq!(
        final_state, expected,
        "guarded counter state must equal the sum of accepted messages"
    );
}

static PANIC_RELEASE_SIGNAL: DispatchSignal = DispatchSignal::new();

unsafe extern "C-unwind" fn panic_then_count_dispatch(
    _ctx: *mut hew_runtime::execution_context::HewExecutionContext,
    state: *mut c_void,
    msg_type: i32,
    _data: *mut c_void,
    _size: usize,
) {
    assert_ne!(
        msg_type, 1,
        "intentional actor-state-lock panic release test"
    );
    let counter = state.cast::<i32>();
    unsafe { *counter += 1 };
    PANIC_RELEASE_SIGNAL.record();
}

#[test]
fn scheduler_releases_state_lock_after_handler_panic() {
    let _test_lock = TEST_LOCK.lock().unwrap();
    ensure_scheduler();
    PANIC_RELEASE_SIGNAL.reset();

    let mut initial = 0_i32;
    let actor = TestActor::spawn_with_state(&mut initial, panic_then_count_dispatch);

    actor.send_empty(1);
    actor.send_empty(2);

    assert!(
        PANIC_RELEASE_SIGNAL.wait_for(1, Duration::from_secs(10)),
        "second dispatch did not run after the first handler panicked"
    );
    let final_state = unsafe { *(*actor.as_ptr()).state.cast::<i32>() };
    assert_eq!(final_state, 1);
}

static NULL_LOCK_SIGNAL: DispatchSignal = DispatchSignal::new();

unsafe extern "C-unwind" fn null_lock_must_not_enter_dispatch(
    _ctx: *mut hew_runtime::execution_context::HewExecutionContext,
    _state: *mut c_void,
    _msg_type: i32,
    _data: *mut c_void,
    _size: usize,
) {
    NULL_LOCK_SIGNAL.record();
}

#[test]
fn dispatch_refuses_null_context_lock_seat() {
    let _test_lock = TEST_LOCK.lock().unwrap();
    ensure_scheduler();
    NULL_LOCK_SIGNAL.reset();

    let actor = TestActor::spawn(null_lock_must_not_enter_dispatch);
    hew_runtime::scheduler::inject_null_lock_seat_once_for_test();
    actor.send_empty(1);

    let deadline = Instant::now() + Duration::from_secs(10);
    while unsafe { hew_actor_get_error(actor.as_ptr()) } == 0 {
        assert!(
            Instant::now() < deadline,
            "null lock-seat dispatch did not trap the actor"
        );
        std::thread::sleep(Duration::from_millis(5));
    }
    assert!(
        !NULL_LOCK_SIGNAL.wait_for(1, Duration::from_millis(50)),
        "dispatch body must not run when ctx->lock_seat is null"
    );
    assert_eq!(
        unsafe { hew_actor_get_error(actor.as_ptr()) },
        HEW_ACTOR_STATE_LOCK_ERR
    );
}

fn last_error_string() -> String {
    let ptr = hew_last_error().cast::<c_char>();
    if ptr.is_null() {
        return String::new();
    }
    unsafe { CStr::from_ptr(ptr) }
        .to_string_lossy()
        .into_owned()
}
