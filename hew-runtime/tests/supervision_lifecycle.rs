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
//!
//! Supervisor and standalone-actor handle lifecycle is funnelled through
//! `hew_runtime_testkit::{TestSupervisor, TestActor}`. The runtime's child-
//! management FFI (`HewChildSpec`, `hew_supervisor_add_child_spec`,
//! `wait_restart`, `get_child_wait`, `set_child_state_drop`, …) is inherently
//! raw and is annotated per-test below.

#![allow(
    clippy::undocumented_unsafe_blocks,
    reason = "Integration test — supervisor child-management FFI is inherently raw; SAFETY notes on unsafe blocks where load-bearing"
)]

use std::ffi::{c_void, CString};
use std::sync::atomic::{AtomicI32, Ordering};
use std::sync::{mpsc, Arc, Barrier, Condvar, Mutex};
use std::time::{Duration, Instant};

use hew_runtime::actor::{
    hew_actor_send, hew_actor_set_crash_teardown_order_hook,
    HEW_ACTOR_CRASH_TEARDOWN_AFTER_EXIT_PROPAGATION,
    HEW_ACTOR_CRASH_TEARDOWN_BEFORE_EXIT_PROPAGATION,
};
use hew_runtime::crash::{hew_crash_log_count, hew_crash_log_last};
use hew_runtime::deterministic::{hew_deterministic_reset, hew_fault_inject_crash};
use hew_runtime::link::hew_actor_link;
use hew_runtime::monitor::{hew_actor_demonitor, hew_actor_monitor};
use hew_runtime::supervisor::{
    hew_supervisor_add_child_dynamic, hew_supervisor_add_child_spec, hew_supervisor_child_count,
    hew_supervisor_get_child_circuit_state, hew_supervisor_get_child_wait,
    hew_supervisor_set_child_state_drop, hew_supervisor_set_circuit_breaker,
    hew_supervisor_set_restart_notify, hew_supervisor_wait_restart, HewChildSpec,
    HEW_CIRCUIT_BREAKER_CLOSED, HEW_CIRCUIT_BREAKER_OPEN, SYS_MSG_DOWN,
};
use hew_runtime_testkit::{ensure_scheduler, HewActorState, TestActor, TestSupervisor};

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
unsafe extern "C-unwind" fn counting_dispatch(
    _ctx: *mut hew_runtime::execution_context::HewExecutionContext,
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
            // SAFETY: SYS_MSG_DOWN payload size matches DownMessageView.
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

unsafe extern "C-unwind" fn monitor_dispatch(
    _ctx: *mut hew_runtime::execution_context::HewExecutionContext,
    _state: *mut c_void,
    msg_type: i32,
    data: *mut c_void,
    data_size: usize,
) {
    MONITOR_DISPATCH_SIGNAL.record_dispatch(msg_type, data, data_size);
}

unsafe extern "C-unwind" fn noop_dispatch(
    _ctx: *mut hew_runtime::execution_context::HewExecutionContext,
    _state: *mut c_void,
    _msg_type: i32,
    _data: *mut c_void,
    _data_size: usize,
) {
}

fn wait_for_circuit_state(
    sup: *mut hew_runtime::supervisor::HewSupervisor,
    index: i32,
    expected: i32,
    timeout: Duration,
) -> bool {
    let deadline = Instant::now() + timeout;
    loop {
        // SAFETY: sup is a live supervisor pointer for the test's duration.
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

    let sup = TestSupervisor::new(STRATEGY_ONE_FOR_ONE, 5, 60);
    // SAFETY: sup wraps a live supervisor; child-management FFI is raw.
    unsafe {
        hew_supervisor_set_restart_notify(sup.as_ptr());

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
            arena_cap_bytes: 0,
            on_crash: None,
        };
        assert_eq!(
            hew_supervisor_add_child_spec(sup.as_ptr(), &raw const spec),
            0
        );
        assert_eq!(sup.start(), 0);
        assert_eq!(hew_supervisor_child_count(sup.as_ptr()), 1);

        // Get the child actor and record its ID.
        let child = hew_supervisor_get_child_wait(sup.as_ptr(), 0, 5_000);
        assert!(!child.is_null(), "child must be spawned");
        let original_id = (*child).id;

        // Send a normal message — should dispatch successfully.
        hew_actor_send(child, 1, std::ptr::null_mut(), 0);
        assert!(
            DISPATCH_SIGNAL.wait_for(1, Duration::from_secs(5)),
            "dispatch should have run at least once (count={})",
            DISPATCH_COUNT.load(Ordering::SeqCst)
        );

        // Inject a crash fault for the child actor.
        hew_fault_inject_crash(original_id, 1);

        // Send another message — this should trigger the crash.
        hew_actor_send(child, 1, std::ptr::null_mut(), 0);

        // Wait for restart cycle.
        let restart_count = hew_supervisor_wait_restart(sup.as_ptr(), 1, 5_000);
        assert!(
            restart_count >= 1,
            "supervisor should report a completed restart cycle"
        );

        let restarted = hew_supervisor_get_child_wait(sup.as_ptr(), 0, 5_000);
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

        // Verify crash was logged.
        assert!(
            hew_crash_log_count() > 0,
            "crash log should have at least one entry"
        );
    }
    hew_deterministic_reset();
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

    let sup = TestSupervisor::new(STRATEGY_ONE_FOR_ONE, 10, 60);
    let watcher = TestActor::spawn(monitor_dispatch);
    // SAFETY: sup and watcher are live for the test duration; child-mgmt FFI is raw.
    unsafe {
        hew_supervisor_set_restart_notify(sup.as_ptr());

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
            arena_cap_bytes: 0,
            on_crash: None,
        };
        assert_eq!(
            hew_supervisor_add_child_spec(sup.as_ptr(), &raw const spec),
            0
        );
        assert_eq!(sup.start(), 0);

        // Configure circuit breaker: max 2 crashes in 60 seconds.
        assert_eq!(
            hew_supervisor_set_circuit_breaker(sup.as_ptr(), 0, 2, 60, 5),
            0
        );
        assert_eq!(
            hew_supervisor_get_child_circuit_state(sup.as_ptr(), 0),
            HEW_CIRCUIT_BREAKER_CLOSED,
        );

        let crashes_before = hew_crash_log_count();
        for crash_num in 0usize..2 {
            let child = hew_supervisor_get_child_wait(sup.as_ptr(), 0, 5_000);
            assert!(
                !child.is_null(),
                "child should be available before crash iteration {crash_num}"
            );
            let ref_id = hew_actor_monitor(watcher.as_ptr(), child);
            assert_ne!(ref_id, 0, "monitor ref_id should be non-zero");

            let child_id = (*child).id;
            hew_fault_inject_crash(child_id, 1);
            hew_actor_send(child, 1, std::ptr::null_mut(), 0);
            MONITOR_DISPATCH_SIGNAL
                .wait_for_down_count(crash_num + 1, Duration::from_secs(5))
                .expect("watcher should observe each crash");

            if crash_num == 0 {
                let restart_count = hew_supervisor_wait_restart(sup.as_ptr(), 1, 5_000);
                assert!(
                    restart_count >= 1,
                    "first crash should complete a supervisor restart cycle"
                );
            } else {
                assert!(
                    wait_for_circuit_state(
                        sup.as_ptr(),
                        0,
                        HEW_CIRCUIT_BREAKER_OPEN,
                        Duration::from_secs(5)
                    ),
                    "second repeated crash should open the circuit breaker"
                );
            }
        }

        let final_crash_count = hew_crash_log_count();
        let _state = hew_supervisor_get_child_circuit_state(sup.as_ptr(), 0);
        assert!(
            final_crash_count >= crashes_before + 2,
            "crash log should have at least 2 new entries (before={crashes_before}, after={final_crash_count})"
        );
    }
    hew_deterministic_reset();
}

/// Actor links: when a linked actor crashes, EXIT signal is delivered
/// to the linked partner's mailbox. The partner's dispatch receives the
/// EXIT system message (`msg_type` = 103).
#[test]
fn link_delivers_exit_on_crash() {
    static LINK_EXIT_RECEIVED: AtomicI32 = AtomicI32::new(0);
    static LINK_EXIT_SIGNAL: DispatchSignal = DispatchSignal::new();

    unsafe extern "C-unwind" fn exit_detecting_dispatch(
        _ctx: *mut hew_runtime::execution_context::HewExecutionContext,
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

    let actor_a = TestActor::spawn(counting_dispatch);
    let actor_b = TestActor::spawn(exit_detecting_dispatch);

    // SAFETY: actors are live; reading id and FFI ops use the runtime's contract.
    unsafe {
        hew_actor_link(actor_a.as_ptr(), actor_b.as_ptr());

        let id_a = (*actor_a.as_ptr()).id;
        hew_fault_inject_crash(id_a, 1);
        hew_actor_send(actor_a.as_ptr(), 1, std::ptr::null_mut(), 0);
    }

    assert!(
        actor_a.wait_for_state(HewActorState::Crashed, Duration::from_secs(5)),
        "actor_a should be in Crashed state"
    );

    assert!(
        LINK_EXIT_SIGNAL.wait_for(1, Duration::from_secs(5)),
        "linked actor_b should have received EXIT message (got {})",
        LINK_EXIT_RECEIVED.load(Ordering::SeqCst)
    );

    hew_deterministic_reset();
}

/// D48 cascade invariant: a linked actor's crash-cascade EXIT is enqueued
/// before the supervisor's restart cycle for the crashed actor completes.
///
/// The EXIT-enqueue probe sends directly from the crash teardown path after
/// `propagate_exit_to_links`, while a separate waiter thread sends when
/// `hew_supervisor_wait_restart` observes restart completion. The test drains
/// the shared channel and asserts the first signal is the EXIT enqueue, so the
/// assertion is not constructed by sequentially waiting for EXIT before restart.
#[test]
#[allow(
    clippy::too_many_lines,
    reason = "Integration setup keeps the independent ordering probes in one test"
)]
fn linked_actor_receives_exit_before_supervisor_restarts() {
    const STRATEGY_ONE_FOR_ONE: i32 = 0;
    const RESTART_PERMANENT: i32 = 0;
    const OVERFLOW_DROP_NEW: i32 = 1;

    static LINK_EXIT_RECEIVED: AtomicI32 = AtomicI32::new(0);
    static CRASH_TEARDOWN_ORDER_TX: Mutex<Option<mpsc::Sender<(&'static str, usize)>>> =
        Mutex::new(None);

    struct CrashTeardownOrderHookGuard;

    impl Drop for CrashTeardownOrderHookGuard {
        fn drop(&mut self) {
            hew_actor_set_crash_teardown_order_hook(None);
            *CRASH_TEARDOWN_ORDER_TX.lock().unwrap() = None;
        }
    }

    fn crash_teardown_order_hook(event: i32) {
        if event == HEW_ACTOR_CRASH_TEARDOWN_BEFORE_EXIT_PROPAGATION {
            std::thread::sleep(Duration::from_millis(250));
            return;
        }

        if event == HEW_ACTOR_CRASH_TEARDOWN_AFTER_EXIT_PROPAGATION {
            if let Some(tx) = CRASH_TEARDOWN_ORDER_TX.lock().unwrap().as_ref() {
                let _ = tx.send(("exit_enqueued", 0));
            }
        }
    }

    unsafe extern "C-unwind" fn exit_observing_dispatch(
        _ctx: *mut hew_runtime::execution_context::HewExecutionContext,
        _state: *mut c_void,
        msg_type: i32,
        _data: *mut c_void,
        _data_size: usize,
    ) {
        // SYS_MSG_EXIT = 103.
        if msg_type == 103 {
            LINK_EXIT_RECEIVED.fetch_add(1, Ordering::SeqCst);
        }
    }

    let _guard = TEST_LOCK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    ensure_scheduler();
    hew_deterministic_reset();
    LINK_EXIT_RECEIVED.store(0, Ordering::SeqCst);
    DISPATCH_COUNT.store(0, Ordering::SeqCst);
    DISPATCH_SIGNAL.reset();
    let (order_tx, order_rx) = mpsc::channel();
    *CRASH_TEARDOWN_ORDER_TX.lock().unwrap() = Some(order_tx.clone());
    hew_actor_set_crash_teardown_order_hook(Some(crash_teardown_order_hook));
    let _order_hook_guard = CrashTeardownOrderHookGuard;

    let sup = TestSupervisor::new(STRATEGY_ONE_FOR_ONE, 5, 60);
    let linked = TestActor::spawn(exit_observing_dispatch);

    // SAFETY: supervisor and standalone actor wrappers keep the
    // underlying handles live; child-management and link FFI are raw.
    unsafe {
        hew_supervisor_set_restart_notify(sup.as_ptr());

        let mut state: i32 = 0;
        let name = cstr("cascade-source");
        let spec = HewChildSpec {
            name: name.as_ptr(),
            init_state: (&raw mut state).cast(),
            init_state_size: std::mem::size_of::<i32>(),
            dispatch: Some(counting_dispatch),
            restart_policy: RESTART_PERMANENT,
            mailbox_capacity: -1,
            overflow: OVERFLOW_DROP_NEW,
            arena_cap_bytes: 0,
            on_crash: None,
        };
        assert_eq!(
            hew_supervisor_add_child_spec(sup.as_ptr(), &raw const spec),
            0
        );
        assert_eq!(sup.start(), 0);

        let child = hew_supervisor_get_child_wait(sup.as_ptr(), 0, 5_000);
        assert!(!child.is_null(), "supervised child must be spawned");
        let crashed_id = (*child).id;

        // Wait until the child is actually dispatching so the link
        // attaches to a runnable target.
        hew_actor_send(child, 1, std::ptr::null_mut(), 0);
        assert!(
            DISPATCH_SIGNAL.wait_for(1, Duration::from_secs(5)),
            "supervised child should run at least once before linking"
        );

        // Link standalone actor → supervised child.  When the child
        // crashes, propagate_exit_to_links must enqueue SYS_MSG_EXIT in
        // the standalone actor's mailbox.
        hew_actor_link(linked.as_ptr(), child);

        // Inject the crash and trigger it.
        let restart_waiter_ready = Arc::new(Barrier::new(2));
        let sup_addr = sup.as_ptr() as usize;
        let restart_tx = order_tx.clone();
        let restart_waiter = {
            let restart_waiter_ready = Arc::clone(&restart_waiter_ready);
            std::thread::spawn(move || {
                restart_waiter_ready.wait();
                let restart_count = hew_supervisor_wait_restart(
                    sup_addr as *mut hew_runtime::supervisor::HewSupervisor,
                    1,
                    5_000,
                );
                let _ = restart_tx.send(("restart", restart_count));
            })
        };
        restart_waiter_ready.wait();

        hew_fault_inject_crash(crashed_id, 1);
        hew_actor_send(child, 1, std::ptr::null_mut(), 0);

        let first = order_rx
            .recv_timeout(Duration::from_secs(5))
            .expect("linked EXIT or supervisor restart observation must arrive");
        assert_eq!(
            first.0, "exit_enqueued",
            "D48 invariant: linked actor's EXIT enqueue must win the \
             independent race against supervisor restart completion (first={first:?})"
        );

        let second = order_rx
            .recv_timeout(Duration::from_secs(5))
            .expect("supervisor restart observation must arrive after linked EXIT");
        assert_eq!(
            second.0, "restart",
            "only linked EXIT and supervisor restart observations are expected"
        );
        assert!(
            second.1 >= 1,
            "supervisor must complete a restart cycle for the crashed child"
        );
        restart_waiter
            .join()
            .expect("restart waiter must not panic");

        // Sanity: the restarted child gets a fresh actor id, proving
        // the supervisor actually re-spawned.
        let restarted = hew_supervisor_get_child_wait(sup.as_ptr(), 0, 5_000);
        assert!(
            !restarted.is_null(),
            "restarted child must be available after restart cycle"
        );
        assert_ne!(
            (*restarted).id,
            crashed_id,
            "restart must replace the crashed child with a new actor"
        );
    }

    hew_deterministic_reset();
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

    let watcher = TestActor::spawn(monitor_dispatch);
    let target = TestActor::spawn(noop_dispatch);

    // SAFETY: actors are live; monitor/fault-inject/send use runtime contract.
    let (target_id, ref_id) = unsafe {
        let ref_id = hew_actor_monitor(watcher.as_ptr(), target.as_ptr());
        assert_ne!(ref_id, 0, "monitor ref_id should be non-zero");

        let target_id = (*target.as_ptr()).id;
        hew_fault_inject_crash(target_id, 1);
        hew_actor_send(target.as_ptr(), 1, std::ptr::null_mut(), 0);
        (target_id, ref_id)
    };

    assert!(
        target.wait_for_state(HewActorState::Crashed, Duration::from_secs(5)),
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

/// Demonitoring before a crash suppresses the watcher DOWN notification.
#[test]
fn demonitor_before_crash_suppresses_down() {
    let _guard = TEST_LOCK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    ensure_scheduler();
    hew_deterministic_reset();
    MONITOR_DISPATCH_SIGNAL.reset();

    let watcher = TestActor::spawn(monitor_dispatch);
    let target = TestActor::spawn(noop_dispatch);

    // SAFETY: actors are live; monitor/demonitor/fault-inject use runtime contract.
    let target_id = unsafe {
        let ref_id = hew_actor_monitor(watcher.as_ptr(), target.as_ptr());
        assert_ne!(ref_id, 0, "monitor ref_id should be non-zero");
        hew_actor_demonitor(ref_id);

        let target_id = (*target.as_ptr()).id;
        hew_fault_inject_crash(target_id, 1);
        hew_actor_send(target.as_ptr(), 1, std::ptr::null_mut(), 0);
        target_id
    };

    assert!(
        target.wait_for_state(HewActorState::Crashed, Duration::from_secs(5)),
        "target should enter Crashed state after injected fault"
    );

    watcher.send_empty(77);
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

/// Monitoring an already-crashed actor resolves immediately with the usual DOWN payload.
#[test]
fn late_monitor_after_crash_delivers_immediate_down() {
    let _guard = TEST_LOCK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    ensure_scheduler();
    hew_deterministic_reset();
    MONITOR_DISPATCH_SIGNAL.reset();

    let watcher = TestActor::spawn(monitor_dispatch);
    let target = TestActor::spawn(noop_dispatch);

    // SAFETY: actors are live; reading target id + fault-injecting use runtime contract.
    let target_id = unsafe {
        let id = (*target.as_ptr()).id;
        hew_fault_inject_crash(id, 1);
        hew_actor_send(target.as_ptr(), 1, std::ptr::null_mut(), 0);
        id
    };

    assert!(
        target.wait_for_state(HewActorState::Crashed, Duration::from_secs(5)),
        "target should enter Crashed state before late monitor registration"
    );

    // SAFETY: late monitor on a live watcher+target.
    let ref_id = unsafe { hew_actor_monitor(watcher.as_ptr(), target.as_ptr()) };
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
    watcher.send_empty(77);
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

/// Crash forensics: crash reports contain meaningful metadata.
#[test]
fn crash_report_has_metadata() {
    let _guard = TEST_LOCK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    ensure_scheduler();
    hew_deterministic_reset();

    // Record the crash log count before our test.
    // SAFETY: hew_crash_log_count is total over runtime initialization.
    let log_before = unsafe { hew_crash_log_count() };

    let actor = TestActor::spawn(counting_dispatch);

    // SAFETY: actor is live; reading id and fault-injecting use runtime contract.
    unsafe {
        let actor_id = (*actor.as_ptr()).id;
        hew_fault_inject_crash(actor_id, 1);
        hew_actor_send(actor.as_ptr(), 1, std::ptr::null_mut(), 0);
    }

    // Poll until the crash is recorded.
    // SAFETY: hew_crash_log_count / hew_crash_log_last are total over runtime init.
    let report = unsafe {
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
        hew_crash_log_last()
    };
    assert_eq!(report.signal, -1, "injected crash should have signal=-1");
    assert!(
        report.timestamp_ns > 0,
        "crash report should have a timestamp"
    );

    hew_deterministic_reset();
}

/// Deterministic testing: seed control and fault injection work correctly.
#[test]
fn deterministic_seed_and_fault_injection() {
    let _guard = TEST_LOCK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    hew_deterministic_reset();

    hew_runtime::deterministic::hew_deterministic_set_seed(12345);
    assert_eq!(
        hew_runtime::deterministic::hew_deterministic_get_seed(),
        12345,
        "seed should be set"
    );

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

// ── State-drop regression for supervisor-spawn paths ────────────────────────

static SUPERVISOR_STATE_DROP_COUNT: AtomicI32 = AtomicI32::new(0);

unsafe extern "C" fn supervisor_child_state_drop(_state: *mut c_void) {
    SUPERVISOR_STATE_DROP_COUNT.fetch_add(1, Ordering::SeqCst);
}

#[test]
fn supervisor_restart_runs_state_drop_on_new_actor() {
    const STRATEGY_ONE_FOR_ONE: i32 = 0;
    const RESTART_PERMANENT: i32 = 0;
    const OVERFLOW_DROP_NEW: i32 = 1;

    let _guard = TEST_LOCK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    ensure_scheduler();
    hew_deterministic_reset();
    DISPATCH_SIGNAL.reset();
    SUPERVISOR_STATE_DROP_COUNT.store(0, Ordering::SeqCst);

    let sup = TestSupervisor::new(STRATEGY_ONE_FOR_ONE, 5, 60);
    // SAFETY: sup is live; child-mgmt FFI is raw.
    unsafe {
        hew_supervisor_set_restart_notify(sup.as_ptr());

        let mut state: u64 = 0xDEAD_BEEF;
        let name = CString::new("drop-test-child").unwrap();
        let spec = HewChildSpec {
            name: name.as_ptr(),
            init_state: (&raw mut state).cast::<c_void>(),
            init_state_size: std::mem::size_of::<u64>(),
            dispatch: Some(counting_dispatch),
            restart_policy: RESTART_PERMANENT,
            mailbox_capacity: -1,
            overflow: OVERFLOW_DROP_NEW,
            arena_cap_bytes: 0,
            on_crash: None,
        };
        assert_eq!(
            hew_supervisor_add_child_spec(sup.as_ptr(), &raw const spec),
            0,
            "add_child_spec must succeed"
        );
        hew_supervisor_set_child_state_drop(sup.as_ptr(), 0, supervisor_child_state_drop);

        assert_eq!(sup.start(), 0, "supervisor must start");

        let child = hew_supervisor_get_child_wait(sup.as_ptr(), 0, 5_000);
        assert!(!child.is_null(), "child must be spawned");
        let original_id = (*child).id;

        assert!(
            (*child).state_drop_fn.is_some(),
            "initial actor must have state_drop_fn registered"
        );

        hew_fault_inject_crash(original_id, 1);
        hew_actor_send(child, 1, std::ptr::null_mut(), 0);
        let restart_count = hew_supervisor_wait_restart(sup.as_ptr(), 1, 10_000);
        assert!(
            restart_count >= 1,
            "supervisor must report at least one completed restart"
        );

        let restarted = hew_supervisor_get_child_wait(sup.as_ptr(), 0, 5_000);
        assert!(!restarted.is_null(), "restarted child must appear in slot");
        assert_ne!(
            (*restarted).id,
            original_id,
            "restarted actor must have a new identity"
        );

        assert!(
            (*restarted).state_drop_fn.is_some(),
            "restarted actor must have state_drop_fn registered"
        );

        hew_deterministic_reset();
    }
    // TestSupervisor::Drop runs hew_supervisor_stop, which tears down the
    // restarted actor and triggers the drop callback exactly once.

    drop(sup);
    let drops = SUPERVISOR_STATE_DROP_COUNT.load(Ordering::SeqCst);
    assert_eq!(
        drops, 1,
        "state_drop_fn must fire for the restarted actor's teardown (got {drops})"
    );
}

#[test]
fn dynamic_child_restart_runs_state_drop() {
    const STRATEGY_ONE_FOR_ONE: i32 = 0;
    const RESTART_PERMANENT: i32 = 0;
    const OVERFLOW_DROP_NEW: i32 = 1;

    let _guard = TEST_LOCK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    ensure_scheduler();
    hew_deterministic_reset();
    DISPATCH_SIGNAL.reset();
    SUPERVISOR_STATE_DROP_COUNT.store(0, Ordering::SeqCst);

    let sup = TestSupervisor::new(STRATEGY_ONE_FOR_ONE, 5, 60);
    // SAFETY: sup is live; child-mgmt FFI is raw.
    unsafe {
        hew_supervisor_set_restart_notify(sup.as_ptr());
        assert_eq!(sup.start(), 0, "supervisor must start");

        let mut state: u64 = 0xCAFE_BABE;
        let name = std::ffi::CString::new("dyn-drop-child").unwrap();
        let spec = HewChildSpec {
            name: name.as_ptr(),
            init_state: (&raw mut state).cast::<c_void>(),
            init_state_size: std::mem::size_of::<u64>(),
            dispatch: Some(counting_dispatch),
            restart_policy: RESTART_PERMANENT,
            mailbox_capacity: -1,
            overflow: OVERFLOW_DROP_NEW,
            arena_cap_bytes: 0,
            on_crash: None,
        };

        let idx = hew_supervisor_add_child_dynamic(sup.as_ptr(), &raw const spec);
        assert!(idx >= 0, "add_child_dynamic must succeed");
        hew_supervisor_set_child_state_drop(sup.as_ptr(), idx, supervisor_child_state_drop);

        let child = hew_supervisor_get_child_wait(sup.as_ptr(), idx, 5_000);
        assert!(!child.is_null(), "dynamically added child must be spawned");
        let original_id = (*child).id;

        assert!(
            (*child).state_drop_fn.is_some(),
            "dynamic child must have state_drop_fn registered after set_child_state_drop"
        );

        hew_fault_inject_crash(original_id, 1);
        hew_actor_send(child, 1, std::ptr::null_mut(), 0);
        let restart_count = hew_supervisor_wait_restart(sup.as_ptr(), 1, 10_000);
        assert!(
            restart_count >= 1,
            "supervisor must restart the dynamically added child"
        );

        let restarted = hew_supervisor_get_child_wait(sup.as_ptr(), idx, 5_000);
        assert!(!restarted.is_null(), "restarted dynamic child must appear");
        assert_ne!(
            (*restarted).id,
            original_id,
            "restarted actor must be a new instance"
        );
        assert!(
            (*restarted).state_drop_fn.is_some(),
            "restarted dynamic child must have state_drop_fn registered"
        );

        hew_deterministic_reset();
    }

    drop(sup);
    let drops = SUPERVISOR_STATE_DROP_COUNT.load(Ordering::SeqCst);
    assert_eq!(
        drops, 1,
        "state_drop_fn must fire once for the restarted dynamic child (got {drops})"
    );
}

#[test]
fn one_for_all_suppresses_state_drop_on_sibling_restart() {
    const STRATEGY_ONE_FOR_ALL: i32 = 1;
    const RESTART_PERMANENT: i32 = 0;
    const OVERFLOW_DROP_NEW: i32 = 1;

    let _guard = TEST_LOCK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    ensure_scheduler();
    hew_deterministic_reset();
    DISPATCH_SIGNAL.reset();
    SUPERVISOR_STATE_DROP_COUNT.store(0, Ordering::SeqCst);

    let sup = TestSupervisor::new(STRATEGY_ONE_FOR_ALL, 5, 60);
    // SAFETY: sup is live; child-mgmt FFI is raw.
    unsafe {
        hew_supervisor_set_restart_notify(sup.as_ptr());

        // Child 0.
        let mut state0: u64 = 0xAAAA_0000;
        let name0 = CString::new("ofs-child-0").unwrap();
        let spec0 = HewChildSpec {
            name: name0.as_ptr(),
            init_state: (&raw mut state0).cast::<c_void>(),
            init_state_size: std::mem::size_of::<u64>(),
            dispatch: Some(counting_dispatch),
            restart_policy: RESTART_PERMANENT,
            mailbox_capacity: -1,
            overflow: OVERFLOW_DROP_NEW,
            arena_cap_bytes: 0,
            on_crash: None,
        };
        assert_eq!(
            hew_supervisor_add_child_spec(sup.as_ptr(), &raw const spec0),
            0
        );

        // Child 1.
        let mut state1: u64 = 0xBBBB_0000;
        let name1 = CString::new("ofs-child-1").unwrap();
        let spec1 = HewChildSpec {
            name: name1.as_ptr(),
            init_state: (&raw mut state1).cast::<c_void>(),
            init_state_size: std::mem::size_of::<u64>(),
            dispatch: Some(counting_dispatch),
            restart_policy: RESTART_PERMANENT,
            mailbox_capacity: -1,
            overflow: OVERFLOW_DROP_NEW,
            arena_cap_bytes: 0,
            on_crash: None,
        };
        assert_eq!(
            hew_supervisor_add_child_spec(sup.as_ptr(), &raw const spec1),
            0
        );

        assert_eq!(sup.start(), 0, "supervisor must start");

        hew_supervisor_set_child_state_drop(sup.as_ptr(), 0, supervisor_child_state_drop);
        hew_supervisor_set_child_state_drop(sup.as_ptr(), 1, supervisor_child_state_drop);

        let child0 = hew_supervisor_get_child_wait(sup.as_ptr(), 0, 5_000);
        assert!(!child0.is_null(), "child 0 must be spawned");
        assert!(
            (*child0).state_drop_fn.is_some(),
            "child 0 must have state_drop_fn registered"
        );

        let child1 = hew_supervisor_get_child_wait(sup.as_ptr(), 1, 5_000);
        assert!(!child1.is_null(), "child 1 must be spawned");
        assert!(
            (*child1).state_drop_fn.is_some(),
            "child 1 must have state_drop_fn registered"
        );

        let id0 = (*child0).id;
        hew_fault_inject_crash(id0, 1);
        hew_actor_send(child0, 1, std::ptr::null_mut(), 0);
        let restart_count = hew_supervisor_wait_restart(sup.as_ptr(), 1, 10_000);
        assert!(
            restart_count >= 1,
            "ONE_FOR_ALL supervisor must complete a restart cycle"
        );

        let drops_after_restart = SUPERVISOR_STATE_DROP_COUNT.load(Ordering::SeqCst);
        assert_eq!(
            drops_after_restart, 0,
            "state_drop must be suppressed during ONE_FOR_ALL restart (got {drops_after_restart})"
        );

        let restarted0 = hew_supervisor_get_child_wait(sup.as_ptr(), 0, 5_000);
        assert!(!restarted0.is_null(), "restarted child 0 must appear");
        assert!(
            (*restarted0).state_drop_fn.is_some(),
            "restarted child 0 must have state_drop_fn registered"
        );

        let restarted1 = hew_supervisor_get_child_wait(sup.as_ptr(), 1, 5_000);
        assert!(!restarted1.is_null(), "restarted child 1 must appear");
        assert!(
            (*restarted1).state_drop_fn.is_some(),
            "restarted child 1 must have state_drop_fn registered"
        );

        hew_deterministic_reset();
    }

    drop(sup);
    let drops_after_stop = SUPERVISOR_STATE_DROP_COUNT.load(Ordering::SeqCst);
    assert_eq!(
        drops_after_stop, 2,
        "state_drop_fn must fire for each restarted actor on supervisor stop (got {drops_after_stop})"
    );
}
