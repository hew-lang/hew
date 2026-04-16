//! Supervision lifecycle integration test.
//!
//! Demonstrates the full crash → recovery → restart → resume cycle:
//! 1. Supervisor spawns a child actor
//! 2. Fault injection causes the child to "crash" on next dispatch
//! 3. Signal recovery marks the actor as Crashed
//! 4. Supervisor receives crash notification and restarts the child
//! 5. Restarted child processes messages normally
//!
//! Also tests: link propagation, monitor DOWN notifications, circuit breaker,
//! and deterministic testing infrastructure.

#![expect(
    clippy::undocumented_unsafe_blocks,
    reason = "Integration test — safety invariants documented per-test"
)]
// (removed invalid lint: clippy::missing_docs_in_crate_items)

use std::ffi::{c_void, CString};
use std::sync::atomic::{AtomicI32, Ordering};
use std::sync::{Condvar, Mutex};
use std::time::{Duration, Instant};

use hew_runtime::actor::{hew_actor_send, hew_actor_spawn};
use hew_runtime::crash::{hew_crash_log_count, hew_crash_log_last};
use hew_runtime::deterministic::{hew_deterministic_reset, hew_fault_inject_crash};
use hew_runtime::internal::types::HewActorState;
use hew_runtime::link::hew_actor_link;
use hew_runtime::monitor::{hew_actor_demonitor, hew_actor_monitor};
use hew_runtime::supervisor::{
    hew_supervisor_add_child_spec, hew_supervisor_child_count,
    hew_supervisor_get_child_circuit_state, hew_supervisor_get_child_wait, hew_supervisor_new,
    hew_supervisor_set_circuit_breaker, hew_supervisor_set_restart_notify, hew_supervisor_start,
    hew_supervisor_stop, hew_supervisor_wait_restart, HewChildSpec, HEW_CIRCUIT_BREAKER_CLOSED,
    HEW_CIRCUIT_BREAKER_OPEN, SYS_MSG_DOWN,
};

static SCHED_INIT: std::sync::Once = std::sync::Once::new();
fn ensure_scheduler() {
    SCHED_INIT.call_once(|| {
        hew_runtime::scheduler::hew_sched_init();
    });
}

/// Global lock to serialize all tests in this file.
///
/// Tests share mutable global state: the fault injection table (cleared by
/// `hew_deterministic_reset`), dispatch counters, and the crash log.  Running
/// them in parallel causes one test's `hew_deterministic_reset` to clear
/// faults injected by another, leading to flaky failures.
static TEST_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());

// ── Dispatch counters ────────────────────────────────────────────────────

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

    fn record_dispatch(&self) {
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

/// Counts how many times the child dispatch function has been called.
static DISPATCH_COUNT: AtomicI32 = AtomicI32::new(0);
static DISPATCH_SIGNAL: DispatchSignal = DispatchSignal::new();

/// Simple dispatch: increments counter.
unsafe extern "C" fn counting_dispatch(
    _state: *mut c_void,
    _msg_type: i32,
    _data: *mut c_void,
    _data_size: usize,
) {
    DISPATCH_COUNT.fetch_add(1, Ordering::SeqCst);
    DISPATCH_SIGNAL.record_dispatch();
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(C)]
struct DownMessageView {
    monitored_actor_id: u64,
    ref_id: u64,
    reason: i32,
}

#[derive(Clone, Debug, Default)]
struct MonitorDispatchState {
    total_dispatches: usize,
    down_messages: Vec<DownMessageView>,
}

struct MonitorDispatchSignal {
    state: Mutex<MonitorDispatchState>,
    cond: Condvar,
}

impl MonitorDispatchSignal {
    const fn new() -> Self {
        Self {
            state: Mutex::new(MonitorDispatchState {
                total_dispatches: 0,
                down_messages: Vec::new(),
            }),
            cond: Condvar::new(),
        }
    }

    fn reset(&self) {
        *self.state.lock().unwrap() = MonitorDispatchState::default();
    }

    fn record_dispatch(&self, msg_type: i32, data: *mut c_void, data_size: usize) {
        let mut state = self.state.lock().unwrap();
        state.total_dispatches += 1;
        if msg_type == SYS_MSG_DOWN
            && !data.is_null()
            && data_size == std::mem::size_of::<DownMessageView>()
        {
            let down = unsafe { (data.cast::<DownMessageView>().cast_const()).read_unaligned() };
            state.down_messages.push(down);
        }
        self.cond.notify_all();
    }

    fn snapshot(&self) -> MonitorDispatchState {
        self.state.lock().unwrap().clone()
    }

    fn wait_for_total_dispatches(&self, expected: usize, timeout: Duration) -> bool {
        let mut state = self.state.lock().unwrap();
        let deadline = Instant::now() + timeout;
        while state.total_dispatches < expected {
            let remaining = deadline.saturating_duration_since(Instant::now());
            if remaining.is_zero() {
                return false;
            }
            let (guard, result) = self.cond.wait_timeout(state, remaining).unwrap();
            state = guard;
            if result.timed_out() && state.total_dispatches < expected {
                return false;
            }
        }
        true
    }

    fn wait_for_down_count(
        &self,
        expected: usize,
        timeout: Duration,
    ) -> Option<Vec<DownMessageView>> {
        let mut state = self.state.lock().unwrap();
        let deadline = Instant::now() + timeout;
        while state.down_messages.len() < expected {
            let remaining = deadline.saturating_duration_since(Instant::now());
            if remaining.is_zero() {
                return None;
            }
            let (guard, result) = self.cond.wait_timeout(state, remaining).unwrap();
            state = guard;
            if result.timed_out() && state.down_messages.len() < expected {
                return None;
            }
        }
        Some(state.down_messages.clone())
    }
}

static MONITOR_DISPATCH_SIGNAL: MonitorDispatchSignal = MonitorDispatchSignal::new();

unsafe extern "C" fn monitor_dispatch(
    _state: *mut c_void,
    msg_type: i32,
    data: *mut c_void,
    data_size: usize,
) {
    MONITOR_DISPATCH_SIGNAL.record_dispatch(msg_type, data, data_size);
}

unsafe extern "C" fn noop_dispatch(
    _state: *mut c_void,
    _msg_type: i32,
    _data: *mut c_void,
    _data_size: usize,
) {
}

fn wait_for_actor_state(
    actor: *mut hew_runtime::actor::HewActor,
    expected: HewActorState,
    timeout: Duration,
) -> bool {
    let deadline = Instant::now() + timeout;
    while Instant::now() < deadline {
        let state = unsafe { &*actor }.actor_state.load(Ordering::Acquire);
        if state == expected as i32 {
            return true;
        }
        std::thread::sleep(Duration::from_millis(10));
    }
    unsafe { &*actor }.actor_state.load(Ordering::Acquire) == expected as i32
}

fn wait_for_circuit_state(
    sup: *mut hew_runtime::supervisor::HewSupervisor,
    index: i32,
    expected: i32,
    timeout: Duration,
) -> bool {
    let deadline = Instant::now() + timeout;
    loop {
        let state = unsafe { hew_supervisor_get_child_circuit_state(sup, index) };
        if state == expected {
            return true;
        }
        if Instant::now() >= deadline {
            return false;
        }
        std::thread::sleep(Duration::from_millis(10));
    }
}

fn cstr(s: &str) -> CString {
    CString::new(s).expect("CString::new failed")
}

// ── Tests ────────────────────────────────────────────────────────────────

/// Full supervisor lifecycle: spawn → crash → restart → resume.
///
/// This is the critical demonstration test: a supervised actor crashes
/// (via fault injection), the supervisor detects it and restarts it,
/// and the restarted actor processes subsequent messages.
#[test]
fn supervised_actor_crash_and_restart() {
    const STRATEGY_ONE_FOR_ONE: i32 = 0;
    const RESTART_PERMANENT: i32 = 0;
    const OVERFLOW_DROP_NEW: i32 = 1;

    let _guard = TEST_LOCK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    ensure_scheduler();
    hew_deterministic_reset();
    DISPATCH_COUNT.store(0, Ordering::SeqCst);
    DISPATCH_SIGNAL.reset();

    unsafe {
        // 1. Create and start supervisor
        let sup = hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 5, 60);
        assert!(!sup.is_null(), "supervisor must be created");
        hew_supervisor_set_restart_notify(sup);

        let mut state: i32 = 42;
        let name = cstr("worker");
        let spec = HewChildSpec {
            name: name.as_ptr(),
            init_state: (&raw mut state).cast(),
            init_state_size: std::mem::size_of::<i32>(),
            dispatch: Some(counting_dispatch),
            restart_policy: RESTART_PERMANENT,
            mailbox_capacity: -1,
            overflow: OVERFLOW_DROP_NEW,
        };
        assert_eq!(hew_supervisor_add_child_spec(sup, &raw const spec), 0);
        assert_eq!(hew_supervisor_start(sup), 0);
        assert_eq!(hew_supervisor_child_count(sup), 1);

        // 2. Get the child actor and record its ID
        let child = hew_supervisor_get_child_wait(sup, 0, 5_000);
        assert!(!child.is_null(), "child must be spawned");
        let original_id = (*child).id;

        // 3. Send a normal message — should dispatch successfully
        hew_actor_send(child, 1, std::ptr::null_mut(), 0);
        assert!(
            DISPATCH_SIGNAL.wait_for(1, Duration::from_secs(5)),
            "dispatch should have run at least once (count={})",
            DISPATCH_COUNT.load(Ordering::SeqCst)
        );

        // 4. Inject a crash fault for the child actor
        hew_fault_inject_crash(original_id, 1); // crash on next dispatch

        // 5. Send another message — this should trigger the crash
        hew_actor_send(child, 1, std::ptr::null_mut(), 0);

        // 6. Wait for: crash detection → supervisor notification →
        //    supervisor dispatch → restart → new actor spawn
        let restart_count = hew_supervisor_wait_restart(sup, 1, 5_000);
        assert!(
            restart_count >= 1,
            "supervisor should report a completed restart cycle"
        );

        // 7. The supervisor should have restarted the child with a NEW actor.
        //    (The old `child` pointer may be freed — don't dereference it.)
        let restarted = hew_supervisor_get_child_wait(sup, 0, 5_000);
        assert!(
            !restarted.is_null(),
            "child should be available after restart"
        );
        assert_ne!(
            (*restarted).id,
            original_id,
            "restart should replace the crashed child with a new actor"
        );

        // The restarted actor should process messages normally.
        let pre = DISPATCH_COUNT.load(Ordering::SeqCst);
        hew_actor_send(restarted, 1, std::ptr::null_mut(), 0);
        assert!(
            DISPATCH_SIGNAL.wait_for(pre + 1, Duration::from_secs(5)),
            "restarted actor should process messages (pre={pre}, post={})",
            DISPATCH_COUNT.load(Ordering::SeqCst)
        );

        // 8. Verify crash was logged
        assert!(
            hew_crash_log_count() > 0,
            "crash log should have at least one entry"
        );

        // Clean up
        hew_deterministic_reset();
        hew_supervisor_stop(sup);
    }
}

/// Supervisor with circuit breaker: repeated crashes should trip the breaker.
#[test]
fn circuit_breaker_trips_on_repeated_crashes() {
    const STRATEGY_ONE_FOR_ONE: i32 = 0;
    const RESTART_PERMANENT: i32 = 0;
    const OVERFLOW_DROP_NEW: i32 = 1;

    let _guard = TEST_LOCK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    ensure_scheduler();
    hew_deterministic_reset();
    MONITOR_DISPATCH_SIGNAL.reset();

    unsafe {
        let sup = hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 10, 60);
        hew_supervisor_set_restart_notify(sup);
        let watcher = hew_actor_spawn(std::ptr::null_mut(), 0, Some(monitor_dispatch));
        assert!(!watcher.is_null(), "watcher actor should be spawned");

        let mut state: i32 = 0;
        let name = cstr("breaker-test");
        let spec = HewChildSpec {
            name: name.as_ptr(),
            init_state: (&raw mut state).cast(),
            init_state_size: std::mem::size_of::<i32>(),
            dispatch: Some(counting_dispatch),
            restart_policy: RESTART_PERMANENT,
            mailbox_capacity: -1,
            overflow: OVERFLOW_DROP_NEW,
        };
        assert_eq!(hew_supervisor_add_child_spec(sup, &raw const spec), 0);
        assert_eq!(hew_supervisor_start(sup), 0);

        // Configure circuit breaker: max 2 crashes in 60 seconds
        assert_eq!(hew_supervisor_set_circuit_breaker(sup, 0, 2, 60, 5), 0);
        assert_eq!(
            hew_supervisor_get_child_circuit_state(sup, 0),
            HEW_CIRCUIT_BREAKER_CLOSED,
        );

        // Crash the child twice, waiting for the first restart and the second
        // crash to trip the breaker.
        let crashes_before = hew_crash_log_count();
        for crash_num in 0usize..2 {
            let child = hew_supervisor_get_child_wait(sup, 0, 5_000);
            assert!(
                !child.is_null(),
                "child should be available before crash iteration {crash_num}"
            );
            let ref_id = hew_actor_monitor(watcher, child);
            assert_ne!(ref_id, 0, "monitor ref_id should be non-zero");

            let child_id = (*child).id;
            hew_fault_inject_crash(child_id, 1);
            hew_actor_send(child, 1, std::ptr::null_mut(), 0);
            MONITOR_DISPATCH_SIGNAL
                .wait_for_down_count(crash_num + 1, Duration::from_secs(5))
                .expect("watcher should observe each crash");

            if crash_num == 0 {
                let restart_count = hew_supervisor_wait_restart(sup, 1, 5_000);
                assert!(
                    restart_count >= 1,
                    "first crash should complete a supervisor restart cycle"
                );
            } else {
                assert!(
                    wait_for_circuit_state(
                        sup,
                        0,
                        HEW_CIRCUIT_BREAKER_OPEN,
                        Duration::from_secs(5)
                    ),
                    "second repeated crash should open the circuit breaker"
                );
            }
        }

        let final_crash_count = hew_crash_log_count();
        let _state = hew_supervisor_get_child_circuit_state(sup, 0);
        assert!(
            final_crash_count >= crashes_before + 2,
            "crash log should have at least 2 new entries (before={crashes_before}, after={final_crash_count})"
        );

        hew_deterministic_reset();
        hew_supervisor_stop(sup);
    }
}

/// Actor links: when a linked actor crashes, EXIT signal is delivered
/// to the linked partner's mailbox. The partner's dispatch receives the
/// EXIT system message (`msg_type` = 103).
#[test]
fn link_delivers_exit_on_crash() {
    static LINK_EXIT_RECEIVED: AtomicI32 = AtomicI32::new(0);
    static LINK_EXIT_SIGNAL: DispatchSignal = DispatchSignal::new();

    /// Dispatch that detects EXIT system messages.
    unsafe extern "C" fn exit_detecting_dispatch(
        _state: *mut c_void,
        msg_type: i32,
        _data: *mut c_void,
        _data_size: usize,
    ) {
        // SYS_MSG_EXIT = 103
        if msg_type == 103 {
            LINK_EXIT_RECEIVED.fetch_add(1, Ordering::SeqCst);
            LINK_EXIT_SIGNAL.record_dispatch();
        }
    }

    let _guard = TEST_LOCK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    ensure_scheduler();
    hew_deterministic_reset();
    LINK_EXIT_RECEIVED.store(0, Ordering::SeqCst);
    LINK_EXIT_SIGNAL.reset();

    unsafe {
        // Spawn two actors — actor_b uses exit-detecting dispatch
        let actor_a = hew_actor_spawn(std::ptr::null_mut(), 0, Some(counting_dispatch));
        let actor_b = hew_actor_spawn(std::ptr::null_mut(), 0, Some(exit_detecting_dispatch));
        assert!(!actor_a.is_null());
        assert!(!actor_b.is_null());

        // Link them bidirectionally
        hew_actor_link(actor_a, actor_b);

        // Crash actor_a via fault injection
        let id_a = (*actor_a).id;
        hew_fault_inject_crash(id_a, 1);
        hew_actor_send(actor_a, 1, std::ptr::null_mut(), 0);

        assert!(
            wait_for_actor_state(actor_a, HewActorState::Crashed, Duration::from_secs(5)),
            "actor_a should be in Crashed state"
        );

        // Link propagation wakes the idle actor automatically.
        assert!(
            LINK_EXIT_SIGNAL.wait_for(1, Duration::from_secs(5)),
            "linked actor_b should have received EXIT message (got {})",
            LINK_EXIT_RECEIVED.load(Ordering::SeqCst)
        );

        hew_deterministic_reset();
    }
}

/// Monitor: when monitored actor crashes, watcher receives a DOWN notification.
#[test]
fn monitor_detects_crash() {
    let _guard = TEST_LOCK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    ensure_scheduler();
    hew_deterministic_reset();
    MONITOR_DISPATCH_SIGNAL.reset();

    unsafe {
        let watcher = hew_actor_spawn(std::ptr::null_mut(), 0, Some(monitor_dispatch));
        let target = hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch));
        assert!(!watcher.is_null());
        assert!(!target.is_null());

        // Set up monitor: watcher monitors target
        let ref_id = hew_actor_monitor(watcher, target);
        assert_ne!(ref_id, 0, "monitor ref_id should be non-zero");

        // Crash the target
        let target_id = (*target).id;
        hew_fault_inject_crash(target_id, 1);
        hew_actor_send(target, 1, std::ptr::null_mut(), 0);

        assert!(
            wait_for_actor_state(target, HewActorState::Crashed, Duration::from_secs(5)),
            "target should enter Crashed state"
        );

        let down_messages = MONITOR_DISPATCH_SIGNAL
            .wait_for_down_count(1, Duration::from_secs(5))
            .expect("watcher should receive a DOWN notification");
        let down = down_messages
            .last()
            .copied()
            .expect("DOWN notification should be recorded");
        assert_eq!(
            down,
            DownMessageView {
                monitored_actor_id: target_id,
                ref_id,
                reason: HewActorState::Crashed as i32,
            },
            "DOWN payload should identify the crashed actor and monitor ref"
        );

        hew_deterministic_reset();
    }
}

/// Demonitoring before a crash suppresses the watcher DOWN notification.
#[test]
fn demonitor_before_crash_suppresses_down() {
    let _guard = TEST_LOCK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    ensure_scheduler();
    hew_deterministic_reset();
    MONITOR_DISPATCH_SIGNAL.reset();

    unsafe {
        let watcher = hew_actor_spawn(std::ptr::null_mut(), 0, Some(monitor_dispatch));
        let target = hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch));
        assert!(!watcher.is_null());
        assert!(!target.is_null());

        let ref_id = hew_actor_monitor(watcher, target);
        assert_ne!(ref_id, 0, "monitor ref_id should be non-zero");
        hew_actor_demonitor(ref_id);

        let target_id = (*target).id;
        hew_fault_inject_crash(target_id, 1);
        hew_actor_send(target, 1, std::ptr::null_mut(), 0);

        assert!(
            wait_for_actor_state(target, HewActorState::Crashed, Duration::from_secs(5)),
            "target should enter Crashed state after injected fault"
        );

        hew_actor_send(watcher, 77, std::ptr::null_mut(), 0);
        assert!(
            MONITOR_DISPATCH_SIGNAL.wait_for_total_dispatches(1, Duration::from_secs(5)),
            "watcher should process the probe message"
        );
        let dispatch_state = MONITOR_DISPATCH_SIGNAL.snapshot();
        assert!(
            dispatch_state.down_messages.is_empty(),
            "demonitored watcher must not receive DOWN for actor {target_id}"
        );

        hew_deterministic_reset();
    }
}

/// Monitoring an already-crashed actor resolves immediately with the usual DOWN payload.
#[test]
fn late_monitor_after_crash_delivers_immediate_down() {
    let _guard = TEST_LOCK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    ensure_scheduler();
    hew_deterministic_reset();
    MONITOR_DISPATCH_SIGNAL.reset();

    unsafe {
        let watcher = hew_actor_spawn(std::ptr::null_mut(), 0, Some(monitor_dispatch));
        let target = hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch));
        assert!(!watcher.is_null());
        assert!(!target.is_null());

        let target_id = (*target).id;
        hew_fault_inject_crash(target_id, 1);
        hew_actor_send(target, 1, std::ptr::null_mut(), 0);

        assert!(
            wait_for_actor_state(target, HewActorState::Crashed, Duration::from_secs(5)),
            "target should enter Crashed state before late monitor registration"
        );

        let ref_id = hew_actor_monitor(watcher, target);
        assert_ne!(ref_id, 0, "late monitor should still return a ref_id");

        let down_messages = MONITOR_DISPATCH_SIGNAL
            .wait_for_down_count(1, Duration::from_secs(5))
            .expect("late monitor should receive an immediate DOWN notification");
        let down = down_messages
            .last()
            .copied()
            .expect("DOWN notification should be recorded");
        assert_eq!(
            down,
            DownMessageView {
                monitored_actor_id: target_id,
                ref_id,
                reason: HewActorState::Crashed as i32,
            },
            "late monitor should reuse the existing DOWN payload shape"
        );

        hew_actor_demonitor(ref_id);
        hew_actor_send(watcher, 77, std::ptr::null_mut(), 0);
        assert!(
            MONITOR_DISPATCH_SIGNAL.wait_for_total_dispatches(2, Duration::from_secs(5)),
            "watcher should process the immediate DOWN and probe message"
        );
        let dispatch_state = MONITOR_DISPATCH_SIGNAL.snapshot();
        assert_eq!(
            dispatch_state.down_messages.len(),
            1,
            "late monitor should not leave behind a second pending DOWN"
        );

        hew_deterministic_reset();
    }
}

/// Crash forensics: crash reports contain meaningful metadata.
#[test]
fn crash_report_has_metadata() {
    let _guard = TEST_LOCK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    ensure_scheduler();
    hew_deterministic_reset();

    unsafe {
        // Record the crash log count before our test
        let log_before = hew_crash_log_count();

        let actor = hew_actor_spawn(std::ptr::null_mut(), 0, Some(counting_dispatch));
        assert!(!actor.is_null());

        let actor_id = (*actor).id;
        hew_fault_inject_crash(actor_id, 1);
        hew_actor_send(actor, 1, std::ptr::null_mut(), 0);

        // Poll until the crash is recorded (Windows CI can be slow).
        let deadline = std::time::Instant::now() + std::time::Duration::from_secs(5);
        loop {
            if hew_crash_log_count() > log_before {
                break;
            }
            assert!(
                std::time::Instant::now() < deadline,
                "timed out waiting for crash log entry (before={log_before})"
            );
            std::thread::sleep(std::time::Duration::from_millis(50));
        }

        let _log_after = hew_crash_log_count();

        // Get the latest crash report — it should be ours
        let report = hew_crash_log_last();
        // Injected crashes use signal=-1
        assert_eq!(report.signal, -1, "injected crash should have signal=-1");
        assert!(
            report.timestamp_ns > 0,
            "crash report should have a timestamp"
        );

        hew_deterministic_reset();
    }
}

/// Deterministic testing: seed control and fault injection work correctly.
#[test]
fn deterministic_seed_and_fault_injection() {
    let _guard = TEST_LOCK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    hew_deterministic_reset();

    // Set a seed and verify it sticks
    hew_runtime::deterministic::hew_deterministic_set_seed(12345);
    assert_eq!(
        hew_runtime::deterministic::hew_deterministic_get_seed(),
        12345,
        "seed should be set"
    );

    // Fault injection: inject and clear
    hew_fault_inject_crash(999, 3);
    assert_eq!(
        hew_runtime::deterministic::hew_fault_count(),
        1,
        "should have 1 fault registered"
    );

    hew_runtime::deterministic::hew_fault_clear_all();
    assert_eq!(
        hew_runtime::deterministic::hew_fault_count(),
        0,
        "faults should be cleared"
    );

    hew_deterministic_reset();
}
