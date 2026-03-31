//! End-to-end bounded execution coverage for actor budget, scheduler, and
//! supervisor restart budget controls.
//!
//! # Test-binary isolation
//!
//! This file contains exactly **one** `#[test]` function and is compiled into
//! its own integration-test binary by Cargo (`tests/runtime_bounded_coverage_e2e`).
//! The scheduler and deterministic subsystems are process-global singletons.
//! Correctness of the metric assertions below (e.g. worker count starts at 0,
//! `hew_sched_metrics_active_workers` reflects only activity in this test)
//! depends on this binary being the sole user of those globals.  Do not add
//! further tests here; create a new file if additional coverage is needed.

#![expect(
    clippy::undocumented_unsafe_blocks,
    reason = "FFI integration test; unsafe invariants are documented locally"
)]

use std::ffi::{c_void, CString};
use std::sync::{Condvar, Mutex, Once};
use std::time::{Duration, Instant};

use hew_runtime::actor::{hew_actor_send, hew_actor_set_budget, HewActor};
use hew_runtime::deterministic::{
    hew_deterministic_reset, hew_deterministic_set_seed, hew_fault_inject_crash,
};
use hew_runtime::internal::types::HewActorState;
use hew_runtime::mailbox::hew_mailbox_len;
use hew_runtime::scheduler::{
    hew_sched_init, hew_sched_metrics_active_workers, hew_sched_metrics_worker_count,
};
use hew_runtime::supervisor::{
    hew_supervisor_add_child_spec, hew_supervisor_get_child_wait, hew_supervisor_is_running,
    hew_supervisor_new, hew_supervisor_set_restart_notify, hew_supervisor_start,
    hew_supervisor_stop, hew_supervisor_wait_restart, HewChildSpec, HewSupervisor,
};

static SCHED_INIT: Once = Once::new();

fn ensure_single_worker_scheduler() {
    // This Once runs exactly once per process.  Because this binary contains
    // only this test, `hew_deterministic_reset()` starts from a clean slate
    // and there are no scheduler metrics from prior tests to account for.
    SCHED_INIT.call_once(|| {
        hew_deterministic_reset();
        // SAFETY: this integration test is the only test in this binary and
        // sets HEW_WORKERS before scheduler initialization.
        unsafe { std::env::set_var("HEW_WORKERS", "1") };
        hew_deterministic_set_seed(1);
        hew_sched_init();
    });

    assert_eq!(
        hew_sched_metrics_worker_count(),
        1,
        "bounded execution test requires a single scheduler worker"
    );
}

struct DispatchGateState {
    started: i32,
    released: i32,
}

struct DispatchGate {
    state: Mutex<DispatchGateState>,
    cond: Condvar,
}

impl DispatchGate {
    const fn new() -> Self {
        Self {
            state: Mutex::new(DispatchGateState {
                started: 0,
                released: 0,
            }),
            cond: Condvar::new(),
        }
    }

    fn reset(&self) {
        let mut state = self.state.lock().unwrap();
        state.started = 0;
        state.released = 0;
    }

    fn wait_for_started(&self, expected: i32, timeout: Duration) -> bool {
        let deadline = Instant::now() + timeout;
        let mut state = self.state.lock().unwrap();
        while state.started < expected {
            let remaining = deadline.saturating_duration_since(Instant::now());
            if remaining.is_zero() {
                return false;
            }
            let (guard, result) = self.cond.wait_timeout(state, remaining).unwrap();
            state = guard;
            if result.timed_out() && state.started < expected {
                return false;
            }
        }
        true
    }

    fn release_through(&self, expected: i32) {
        let mut state = self.state.lock().unwrap();
        assert!(
            state.started >= expected,
            "cannot release dispatch {expected} before it starts (started={})",
            state.started
        );
        if state.released < expected {
            state.released = expected;
            self.cond.notify_all();
        }
    }
}

static DISPATCH_GATE: DispatchGate = DispatchGate::new();

unsafe extern "C" fn gated_dispatch(
    _state: *mut c_void,
    _msg_type: i32,
    _data: *mut c_void,
    _data_size: usize,
) {
    let mut state = DISPATCH_GATE.state.lock().unwrap();
    state.started += 1;
    let target = state.started;
    DISPATCH_GATE.cond.notify_all();

    while state.released < target {
        state = DISPATCH_GATE.cond.wait(state).unwrap();
    }
}

fn cstr(s: &str) -> CString {
    CString::new(s).expect("CString::new failed")
}

/// Wait for the supervisor's child slot at `index` to hold a live actor,
/// reading the pointer under the restart-notify mutex via
/// `hew_supervisor_get_child_wait` so that the happens-before from
/// `restart_child_from_spec`'s write through `notify_restart`'s mutex
/// signal is formally established on the slow (condvar) path.
///
/// Returns the child pointer; panics on timeout.
unsafe fn wait_for_child(sup: *mut HewSupervisor, index: i32, timeout: Duration) -> *mut HewActor {
    // Convert to milliseconds, saturating at i32::MAX (~24 days).
    #[expect(
        clippy::cast_possible_truncation,
        reason = "timeout values in this test are well under i32::MAX ms"
    )]
    let timeout_ms = timeout.as_millis().min(i32::MAX as u128) as i32;
    let child = unsafe { hew_supervisor_get_child_wait(sup, index, timeout_ms) };
    assert!(
        !child.is_null(),
        "timed out waiting for child[{index}] to be spawned"
    );
    child
}

unsafe fn wait_for_actor_state(
    actor: *mut HewActor,
    expected: HewActorState,
    timeout: Duration,
) -> bool {
    let deadline = Instant::now() + timeout;
    loop {
        let state = unsafe {
            (*actor)
                .actor_state
                .load(std::sync::atomic::Ordering::Acquire)
        };
        if state == expected as i32 {
            return true;
        }
        if Instant::now() >= deadline {
            return false;
        }
        std::thread::sleep(Duration::from_millis(10));
    }
}

unsafe fn wait_for_supervisor_stop(sup: *mut HewSupervisor, timeout: Duration) -> bool {
    let deadline = Instant::now() + timeout;
    loop {
        if unsafe { hew_supervisor_is_running(sup) } == 0 {
            return true;
        }
        if Instant::now() >= deadline {
            return false;
        }
        std::thread::sleep(Duration::from_millis(10));
    }
}

unsafe fn actor_mailbox_len(actor: *mut HewActor) -> usize {
    unsafe { hew_mailbox_len((*actor).mailbox.cast()) }
}

unsafe fn crash_child(child: *mut HewActor) {
    let id = unsafe { (*child).id };
    hew_fault_inject_crash(id, 1);
    unsafe { hew_actor_send(child, 1, std::ptr::null_mut(), 0) };
}

#[test]
#[allow(
    clippy::too_many_lines,
    reason = "single scenario test; splitting would obscure the sequential state machine"
)]
fn single_worker_message_budget_and_restart_budget_bound_execution() {
    ensure_single_worker_scheduler();
    DISPATCH_GATE.reset();

    unsafe {
        let sup = hew_supervisor_new(0, 1, 60);
        assert!(!sup.is_null());
        hew_supervisor_set_restart_notify(sup);

        let name = cstr("bounded-worker");
        let mut state: i32 = 0;
        let spec = HewChildSpec {
            name: name.as_ptr(),
            init_state: (&raw mut state).cast(),
            init_state_size: std::mem::size_of::<i32>(),
            dispatch: Some(gated_dispatch),
            restart_policy: 0,
            mailbox_capacity: -1,
            overflow: 1,
        };
        assert_eq!(hew_supervisor_add_child_spec(sup, &raw const spec), 0);
        assert_eq!(hew_supervisor_start(sup), 0);

        let child = wait_for_child(sup, 0, Duration::from_secs(5));
        hew_actor_set_budget(child, 1);

        for _ in 0..3 {
            hew_actor_send(child, 1, std::ptr::null_mut(), 0);
        }

        assert!(
            DISPATCH_GATE.wait_for_started(1, Duration::from_secs(5)),
            "first dispatch should start under the single worker scheduler"
        );
        assert_eq!(
            hew_sched_metrics_active_workers(),
            1,
            "single-worker scheduler should report one active worker while the dispatch is blocked"
        );
        assert_eq!(
            actor_mailbox_len(child),
            2,
            "message budget of 1 should leave two queued messages after the first dispatch starts"
        );
        DISPATCH_GATE.release_through(1);

        assert!(
            DISPATCH_GATE.wait_for_started(2, Duration::from_secs(5)),
            "second activation should start after re-enqueue"
        );
        assert_eq!(
            actor_mailbox_len(child),
            1,
            "second activation should consume exactly one additional message"
        );
        DISPATCH_GATE.release_through(2);

        assert!(
            DISPATCH_GATE.wait_for_started(3, Duration::from_secs(5)),
            "third activation should drain the final queued message"
        );
        assert_eq!(
            actor_mailbox_len(child),
            0,
            "third activation should leave the mailbox empty"
        );
        DISPATCH_GATE.release_through(3);
        assert!(
            wait_for_actor_state(child, HewActorState::Idle, Duration::from_secs(5)),
            "child should return to Idle after draining the queued work"
        );

        crash_child(child);
        let restart_count = hew_supervisor_wait_restart(sup, 1, 5_000);
        assert!(
            restart_count >= 1,
            "first crash should trigger one restart within budget (count={restart_count})"
        );

        // `hew_supervisor_wait_restart` establishes a happens-before with the
        // `children[0]` write in `restart_child_from_spec` (sequenced-before
        // the mutex lock in `notify_restart` in the same supervisor thread).
        // `hew_supervisor_get_child_wait` then reads `children[0]` and, on
        // the slow (condvar) path, does so under the restart-notify mutex,
        // which is the formally synchronised access.  On the fast path the
        // h-b chain from `wait_restart`'s mutex reacquire still holds, but
        // using `get_child_wait` makes the intended synchronization explicit.
        let restarted_child = wait_for_child(sup, 0, Duration::from_secs(5));
        assert_ne!(
            restarted_child, child,
            "restart should install a fresh child actor at a different address"
        );
        hew_actor_set_budget(restarted_child, 1);

        for _ in 0..2 {
            hew_actor_send(restarted_child, 1, std::ptr::null_mut(), 0);
        }

        assert!(
            DISPATCH_GATE.wait_for_started(4, Duration::from_secs(5)),
            "restarted child should begin a fresh bounded activation"
        );
        assert_eq!(
            actor_mailbox_len(restarted_child),
            1,
            "restarted child should still respect the message budget of 1"
        );
        DISPATCH_GATE.release_through(4);

        assert!(
            DISPATCH_GATE.wait_for_started(5, Duration::from_secs(5)),
            "restarted child should need a second activation for the second queued message"
        );
        assert_eq!(
            actor_mailbox_len(restarted_child),
            0,
            "restarted child should drain its mailbox after the second activation"
        );
        DISPATCH_GATE.release_through(5);
        assert!(
            wait_for_actor_state(restarted_child, HewActorState::Idle, Duration::from_secs(5)),
            "restarted child should become Idle before the next crash"
        );

        crash_child(restarted_child);
        let restart_count = hew_supervisor_wait_restart(sup, 2, 5_000);
        assert!(
            restart_count >= 2,
            "second crash should exhaust the restart budget and notify the supervisor (count={restart_count})"
        );
        assert!(
            wait_for_supervisor_stop(sup, Duration::from_secs(5)),
            "supervisor should stop once the restart budget is exhausted"
        );
        assert_eq!(
            hew_supervisor_is_running(sup),
            0,
            "restart budget exhaustion must stop the supervisor"
        );

        hew_supervisor_stop(sup);
    }
}
