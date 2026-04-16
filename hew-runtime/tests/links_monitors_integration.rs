//! Integration tests for actor links and monitors.

use hew_runtime::actor::{hew_actor_free, hew_actor_get_error, hew_actor_send, hew_actor_spawn};
use hew_runtime::deterministic::{hew_deterministic_reset, hew_fault_inject_crash};
use hew_runtime::internal::types::HewActorState;
use hew_runtime::link::{hew_actor_link, hew_actor_unlink};
use hew_runtime::monitor::{hew_actor_demonitor, hew_actor_monitor};
use hew_runtime::supervisor::{SYS_MSG_DOWN, SYS_MSG_EXIT};
use std::ffi::c_void;
use std::sync::atomic::Ordering;
use std::sync::{Condvar, Mutex, Once};
use std::time::{Duration, Instant};

static SCHED_INIT: Once = Once::new();

fn ensure_scheduler() {
    SCHED_INIT.call_once(|| {
        hew_runtime::scheduler::hew_sched_init();
    });
}

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

unsafe extern "C" fn test_dispatch(
    _state: *mut c_void,
    _msg_type: i32,
    _data: *mut c_void,
    _size: usize,
) {
    // Simple test dispatch - does nothing
}

unsafe extern "C" fn monitor_dispatch(
    _state: *mut c_void,
    msg_type: i32,
    data: *mut c_void,
    data_size: usize,
) {
    MONITOR_DISPATCH_SIGNAL.record_dispatch(msg_type, data, data_size);
}

unsafe extern "C" fn exit_dispatch(
    _state: *mut c_void,
    msg_type: i32,
    data: *mut c_void,
    data_size: usize,
) {
    EXIT_DISPATCH_SIGNAL.record_dispatch(msg_type, data, data_size);
}

fn wait_for_actor_state(
    actor: *mut hew_runtime::actor::HewActor,
    expected: HewActorState,
    timeout: Duration,
) -> bool {
    let deadline = Instant::now() + timeout;
    while Instant::now() < deadline {
        // SAFETY: Tests only pass actor pointers returned by hew_actor_spawn
        // and keep them live for the duration of this polling helper.
        let state = unsafe { &*actor }.actor_state.load(Ordering::Acquire);
        if state == expected as i32 {
            return true;
        }
        std::thread::sleep(Duration::from_millis(10));
    }
    // SAFETY: Same as above; the caller keeps `actor` valid while waiting.
    unsafe { &*actor }.actor_state.load(Ordering::Acquire) == expected as i32
}

#[test]
fn test_link_and_monitor_basic() {
    // Create two test actors
    // SAFETY: test_dispatch is a valid function pointer; null state is acceptable.
    let actor_a = unsafe { hew_actor_spawn(std::ptr::null_mut(), 0, Some(test_dispatch)) };

    // SAFETY: test_dispatch is a valid function pointer; null state is acceptable.
    let actor_b = unsafe { hew_actor_spawn(std::ptr::null_mut(), 0, Some(test_dispatch)) };

    assert!(!actor_a.is_null());
    assert!(!actor_b.is_null());

    // Create link between actors
    // SAFETY: actor_a and actor_b are valid pointers from hew_actor_spawn.
    unsafe {
        hew_actor_link(actor_a, actor_b);
    }

    // Create monitor from A to B
    // SAFETY: actor_a and actor_b are valid pointers from hew_actor_spawn.
    let ref_id = unsafe { hew_actor_monitor(actor_a, actor_b) };
    assert_ne!(ref_id, 0);

    // Remove the link and monitor
    // SAFETY: actor_a and actor_b are valid pointers from hew_actor_spawn.
    unsafe {
        hew_actor_unlink(actor_a, actor_b);
    }
    hew_actor_demonitor(ref_id);

    // Clean up
    // SAFETY: Both actors are valid and being freed exactly once.
    unsafe {
        hew_actor_free(actor_a);
        hew_actor_free(actor_b);
    }
}

#[test]
fn test_null_handling() {
    // Test that null pointers are handled gracefully
    // SAFETY: Null pointers are explicitly handled by link/unlink functions.
    unsafe {
        hew_actor_link(std::ptr::null_mut(), std::ptr::null_mut());
        hew_actor_unlink(std::ptr::null_mut(), std::ptr::null_mut());
    }

    // SAFETY: Null pointers are explicitly handled; function returns 0.
    let ref_id = unsafe { hew_actor_monitor(std::ptr::null_mut(), std::ptr::null_mut()) };
    assert_eq!(ref_id, 0);

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

    // SAFETY: This test owns the spawned actors, waits for the crash state
    // before late registration, and frees each actor exactly once.
    unsafe {
        let watcher = hew_actor_spawn(std::ptr::null_mut(), 0, Some(monitor_dispatch));
        let target = hew_actor_spawn(std::ptr::null_mut(), 0, Some(test_dispatch));
        assert!(!watcher.is_null());
        assert!(!target.is_null());

        let target_id = (*target).id;
        hew_fault_inject_crash(target_id, 1);
        hew_actor_send(target, 1, std::ptr::null_mut(), 0);

        assert!(
            wait_for_actor_state(target, HewActorState::Crashed, Duration::from_secs(5)),
            "target should enter Crashed state"
        );

        let ref_id = hew_actor_monitor(watcher, target);
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
        hew_actor_free(watcher);
        hew_actor_free(target);
        hew_deterministic_reset();
    }
}

#[test]
fn test_link_after_crash_delivers_exit_without_stale_registration() {
    let _guard = TEST_LOCK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    ensure_scheduler();
    hew_deterministic_reset();
    EXIT_DISPATCH_SIGNAL.reset();

    // SAFETY: This test owns the spawned actors, waits for the crash state
    // before late registration, and frees each actor exactly once.
    unsafe {
        let survivor = hew_actor_spawn(std::ptr::null_mut(), 0, Some(exit_dispatch));
        let target = hew_actor_spawn(std::ptr::null_mut(), 0, Some(test_dispatch));
        assert!(!survivor.is_null());
        assert!(!target.is_null());

        let target_id = (*target).id;
        hew_fault_inject_crash(target_id, 1);
        hew_actor_send(target, 1, std::ptr::null_mut(), 0);

        assert!(
            wait_for_actor_state(target, HewActorState::Crashed, Duration::from_secs(5)),
            "target should enter Crashed state"
        );
        let exit_reason = hew_actor_get_error(target);

        hew_actor_link(survivor, target);

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

        hew_actor_unlink(survivor, target);
        hew_actor_free(survivor);
        hew_actor_free(target);
        hew_deterministic_reset();
    }
}
