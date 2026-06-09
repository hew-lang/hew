//! Integration tests for actor links and monitors.

use hew_runtime::actor::hew_actor_get_error;
use hew_runtime::deterministic::{hew_deterministic_reset, hew_fault_inject_crash};
use hew_runtime::link::{hew_actor_link, hew_actor_unlink};
use hew_runtime::monitor::{hew_actor_demonitor, hew_actor_monitor};
use hew_runtime::supervisor::{SYS_MSG_DOWN, SYS_MSG_EXIT};
use hew_runtime_testkit::{ensure_scheduler, HewActorState, TestActor};
use std::ffi::c_void;
use std::ptr;
use std::sync::{Condvar, Mutex};
use std::time::{Duration, Instant};

static TEST_LOCK: Mutex<()> = Mutex::new(());

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(C)]
struct DownMessageView {
    monitored_actor_id: u64,
    ref_id: u64,
    reason: i32,
}

#[derive(Clone, Debug, Default)]
struct MonitorDispatchState {
    down_messages: Vec<DownMessageView>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(C)]
struct ExitMessageView {
    crashed_actor_id: u64,
    reason: i32,
}

#[derive(Clone, Debug, Default)]
struct ExitDispatchState {
    exit_messages: Vec<ExitMessageView>,
}

struct MonitorDispatchSignal {
    state: Mutex<MonitorDispatchState>,
    cond: Condvar,
}

impl MonitorDispatchSignal {
    const fn new() -> Self {
        Self {
            state: Mutex::new(MonitorDispatchState {
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
        if msg_type == SYS_MSG_DOWN
            && !data.is_null()
            && data_size == std::mem::size_of::<DownMessageView>()
        {
            // SAFETY: The runtime sent a SYS_MSG_DOWN payload with the exact
            // expected size, so reading the packed value is valid here.
            let down = unsafe { (data.cast::<DownMessageView>().cast_const()).read_unaligned() };
            state.down_messages.push(down);
            self.cond.notify_all();
        }
    }

    fn wait_for_down_count(
        &self,
        expected: usize,
        timeout: Duration,
    ) -> Option<Vec<DownMessageView>> {
        let deadline = Instant::now() + timeout;
        let mut state = self.state.lock().unwrap();
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

struct ExitDispatchSignal {
    state: Mutex<ExitDispatchState>,
    cond: Condvar,
}

impl ExitDispatchSignal {
    const fn new() -> Self {
        Self {
            state: Mutex::new(ExitDispatchState {
                exit_messages: Vec::new(),
            }),
            cond: Condvar::new(),
        }
    }

    fn reset(&self) {
        *self.state.lock().unwrap() = ExitDispatchState::default();
    }

    fn record_dispatch(&self, msg_type: i32, data: *mut c_void, data_size: usize) {
        let mut state = self.state.lock().unwrap();
        if msg_type == SYS_MSG_EXIT
            && !data.is_null()
            && data_size == std::mem::size_of::<ExitMessageView>()
        {
            // SAFETY: The runtime sent a SYS_MSG_EXIT payload with the exact
            // expected size, so reading the packed value is valid here.
            let exit = unsafe { (data.cast::<ExitMessageView>().cast_const()).read_unaligned() };
            state.exit_messages.push(exit);
            self.cond.notify_all();
        }
    }

    fn wait_for_exit_count(
        &self,
        expected: usize,
        timeout: Duration,
    ) -> Option<Vec<ExitMessageView>> {
        let deadline = Instant::now() + timeout;
        let mut state = self.state.lock().unwrap();
        while state.exit_messages.len() < expected {
            let remaining = deadline.saturating_duration_since(Instant::now());
            if remaining.is_zero() {
                return None;
            }
            let (guard, result) = self.cond.wait_timeout(state, remaining).unwrap();
            state = guard;
            if result.timed_out() && state.exit_messages.len() < expected {
                return None;
            }
        }
        Some(state.exit_messages.clone())
    }
}

static EXIT_DISPATCH_SIGNAL: ExitDispatchSignal = ExitDispatchSignal::new();

unsafe extern "C-unwind" fn test_dispatch(
    _ctx: *mut hew_runtime::execution_context::HewExecutionContext,
    _state: *mut c_void,
    _msg_type: i32,
    _data: *mut c_void,
    _size: usize,
    _borrow_mode: i32,
) {
    // Simple test dispatch - does nothing
}

unsafe extern "C-unwind" fn monitor_dispatch(
    _ctx: *mut hew_runtime::execution_context::HewExecutionContext,
    _state: *mut c_void,
    msg_type: i32,
    data: *mut c_void,
    data_size: usize,
    _borrow_mode: i32,
) {
    MONITOR_DISPATCH_SIGNAL.record_dispatch(msg_type, data, data_size);
}

unsafe extern "C-unwind" fn exit_dispatch(
    _ctx: *mut hew_runtime::execution_context::HewExecutionContext,
    _state: *mut c_void,
    msg_type: i32,
    data: *mut c_void,
    data_size: usize,
    _borrow_mode: i32,
) {
    EXIT_DISPATCH_SIGNAL.record_dispatch(msg_type, data, data_size);
}

#[test]
fn test_link_and_monitor_basic() {
    let actor_a = TestActor::spawn(test_dispatch);
    let actor_b = TestActor::spawn(test_dispatch);

    // SAFETY: link/unlink/monitor/demonitor take live actor pointers; both
    // actors remain alive for the test's duration via their TestActor wrappers.
    unsafe {
        hew_actor_link(actor_a.as_ptr(), actor_b.as_ptr());
        let ref_id = hew_actor_monitor(actor_a.as_ptr(), actor_b.as_ptr());
        assert_ne!(ref_id, 0);
        hew_actor_unlink(actor_a.as_ptr(), actor_b.as_ptr());
        hew_actor_demonitor(ref_id);
    }
    // TestActor::Drop closes and frees both actors.
}

#[test]
fn test_null_handling() {
    // SAFETY: link/unlink/monitor are documented as no-ops on null inputs.
    unsafe {
        hew_actor_link(ptr::null_mut(), ptr::null_mut());
        hew_actor_unlink(ptr::null_mut(), ptr::null_mut());
        let ref_id = hew_actor_monitor(ptr::null_mut(), ptr::null_mut());
        assert_eq!(ref_id, 0);
    }
    hew_actor_demonitor(0);
    hew_actor_demonitor(99999);
}

#[test]
fn test_monitor_after_crash_delivers_down_without_stale_registration() {
    let _guard = TEST_LOCK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    ensure_scheduler();
    hew_deterministic_reset();
    MONITOR_DISPATCH_SIGNAL.reset();

    let watcher = TestActor::spawn(monitor_dispatch);
    let target = TestActor::spawn(test_dispatch);

    // SAFETY: target is a live actor; reading its id field through the raw
    // pointer is the runtime's documented way to obtain a fault-injection key.
    let target_id = unsafe { (*target.as_ptr()).id };
    hew_fault_inject_crash(target_id, 1);
    target.send_empty(1);

    assert!(
        target.wait_for_state(HewActorState::Crashed, Duration::from_secs(5)),
        "target should enter Crashed state"
    );

    // SAFETY: late monitor registration takes live watcher/target pointers.
    let ref_id = unsafe { hew_actor_monitor(watcher.as_ptr(), target.as_ptr()) };
    assert_ne!(ref_id, 0, "late monitor should still return a reference");

    let down_messages = MONITOR_DISPATCH_SIGNAL
        .wait_for_down_count(1, Duration::from_secs(5))
        .expect("late monitor registration should deliver DOWN immediately");
    assert_eq!(
        down_messages.last().copied(),
        Some(DownMessageView {
            monitored_actor_id: target_id,
            ref_id,
            reason: HewActorState::Crashed as i32,
        })
    );

    hew_actor_demonitor(ref_id);
    hew_deterministic_reset();
}

#[test]
fn test_link_after_crash_delivers_exit_without_stale_registration() {
    let _guard = TEST_LOCK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    ensure_scheduler();
    hew_deterministic_reset();
    EXIT_DISPATCH_SIGNAL.reset();

    let survivor = TestActor::spawn(exit_dispatch);
    let target = TestActor::spawn(test_dispatch);

    // SAFETY: target is a live actor; reading its id field through the raw
    // pointer is the runtime's documented way to obtain a fault-injection key.
    let target_id = unsafe { (*target.as_ptr()).id };
    hew_fault_inject_crash(target_id, 1);
    target.send_empty(1);

    assert!(
        target.wait_for_state(HewActorState::Crashed, Duration::from_secs(5)),
        "target should enter Crashed state"
    );
    // SAFETY: target's wrapper is alive; hew_actor_get_error reads from it.
    let exit_reason = unsafe { hew_actor_get_error(target.as_ptr()) };

    // SAFETY: link/unlink take live actor pointers.
    unsafe {
        hew_actor_link(survivor.as_ptr(), target.as_ptr());
    }

    let exit_messages = EXIT_DISPATCH_SIGNAL
        .wait_for_exit_count(1, Duration::from_secs(5))
        .expect("late link registration should deliver EXIT immediately");
    assert_eq!(
        exit_messages.last().copied(),
        Some(ExitMessageView {
            crashed_actor_id: target_id,
            reason: exit_reason,
        })
    );

    // SAFETY: unlink takes live actor pointers.
    unsafe {
        hew_actor_unlink(survivor.as_ptr(), target.as_ptr());
    }
    hew_deterministic_reset();
}
