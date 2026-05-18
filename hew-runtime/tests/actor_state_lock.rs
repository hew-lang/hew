#![expect(
    clippy::undocumented_unsafe_blocks,
    reason = "FFI runtime tests exercise raw actor lock ABI calls"
)]

use std::ffi::{c_char, c_void, CStr};
use std::sync::atomic::{AtomicI32, Ordering};
use std::sync::{Condvar, Mutex};
use std::time::{Duration, Instant};

use hew_runtime::actor::{
    hew_actor_self, hew_actor_state_lock_acquire, hew_actor_state_lock_poison_after_panic,
    hew_actor_state_lock_release, hew_actor_state_lock_release_after_panic,
    HEW_ACTOR_STATE_LOCK_ERR, HEW_ACTOR_STATE_LOCK_OK,
};
use hew_runtime::hew_last_error;
use hew_runtime_testkit::{ensure_scheduler, TestActor};

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

unsafe extern "C" fn noop_dispatch(
    _state: *mut c_void,
    _msg_type: i32,
    _data: *mut c_void,
    _size: usize,
) {
}

#[test]
fn state_lock_serializes_parallel_dispatch_attempts() {
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

unsafe extern "C" fn locked_counter_dispatch(
    state: *mut c_void,
    _msg_type: i32,
    data: *mut c_void,
    size: usize,
) {
    let actor = hew_actor_self();
    if unsafe { hew_actor_state_lock_acquire(actor) } != HEW_ACTOR_STATE_LOCK_OK {
        COUNTER_ERRORS.fetch_add(1, Ordering::Relaxed);
        return;
    }
    if size == std::mem::size_of::<i32>() && !data.is_null() {
        let n = unsafe { *data.cast::<i32>() };
        let counter = state.cast::<i32>();
        unsafe { *counter += n };
    }
    if unsafe { hew_actor_state_lock_release(actor) } != HEW_ACTOR_STATE_LOCK_OK {
        COUNTER_ERRORS.fetch_add(1, Ordering::Relaxed);
    }
    COUNTER_SIGNAL.record();
}

#[test]
fn counter_actor_with_guarded_concurrent_inc_is_linearized() {
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
        "guarded dispatch should not report lock ABI failures"
    );

    let final_state = unsafe { *(*actor.as_ptr()).state.cast::<i32>() };
    assert_eq!(
        final_state, expected,
        "guarded counter state must equal the sum of accepted messages"
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
