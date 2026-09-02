//! Integration tests for actor links and monitors.

use hew_runtime::actor::{
    hew_actor_get_error, hew_actor_set_crash_teardown_order_hook, HewActor,
    HEW_ACTOR_CRASH_TEARDOWN_BEFORE_FIRST_WAKE,
};
use hew_runtime::deterministic::{hew_deterministic_reset, hew_fault_inject_crash};
use hew_runtime::link::{hew_actor_link, hew_actor_unlink};
use hew_runtime::mailbox_header::HewSysMsg;
use hew_runtime::monitor::{hew_actor_demonitor, register_actor_monitor, HewDownMessage};
use hew_runtime_testkit::{ensure_scheduler, HewActorState, TestActor};
use std::ffi::c_void;
use std::ptr;
use std::sync::atomic::{AtomicI32, AtomicPtr, Ordering};
use std::sync::{Condvar, Mutex};
use std::time::{Duration, Instant};

static TEST_LOCK: Mutex<()> = Mutex::new(());

#[derive(Clone, Debug, Default)]
struct MonitorDispatchState {
    down_messages: Vec<HewDownMessage>,
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

    fn record_dispatch(&self, sys_msg: i32, data: *mut c_void, data_size: usize) {
        let mut state = self.state.lock().unwrap();
        if sys_msg == HewSysMsg::Down.as_i32()
            && !data.is_null()
            && data_size == std::mem::size_of::<HewDownMessage>()
        {
            // SAFETY: The runtime sent a Down payload with the exact
            // expected size, so reading the packed value is valid here.
            let down = unsafe { (data.cast::<HewDownMessage>().cast_const()).read_unaligned() };
            state.down_messages.push(down);
            self.cond.notify_all();
        }
    }

    fn wait_for_down_count(
        &self,
        expected: usize,
        timeout: Duration,
    ) -> Option<Vec<HewDownMessage>> {
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

    fn record_dispatch(&self, sys_msg: i32, data: *mut c_void, data_size: usize) {
        let mut state = self.state.lock().unwrap();
        if sys_msg == HewSysMsg::Exit.as_i32()
            && !data.is_null()
            && data_size == std::mem::size_of::<ExitMessageView>()
        {
            // SAFETY: The runtime sent an Exit payload with the exact
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
) -> *mut c_void {
    // Simple test dispatch - does nothing
    std::ptr::null_mut()
}

unsafe extern "C-unwind" fn monitor_sys_dispatch(
    _ctx: *mut hew_runtime::execution_context::HewExecutionContext,
    _state: *mut c_void,
    sys_msg: i32,
    data: *mut c_void,
    data_size: usize,
) {
    MONITOR_DISPATCH_SIGNAL.record_dispatch(sys_msg, data, data_size);
}

unsafe extern "C-unwind" fn exit_sys_dispatch(
    _ctx: *mut hew_runtime::execution_context::HewExecutionContext,
    _state: *mut c_void,
    sys_msg: i32,
    data: *mut c_void,
    data_size: usize,
) {
    EXIT_DISPATCH_SIGNAL.record_dispatch(sys_msg, data, data_size);
}

#[test]
fn test_link_and_monitor_basic() {
    let actor_a = TestActor::spawn(test_dispatch);
    let actor_b = TestActor::spawn(test_dispatch);

    // SAFETY: link/unlink/monitor/demonitor take live actor pointers; both
    // actors remain alive for the test's duration via their TestActor wrappers.
    unsafe {
        hew_actor_link(actor_a.as_ptr(), actor_b.as_ptr());
        let ref_id = register_actor_monitor(actor_a.as_ptr(), actor_b.as_ptr())
            .expect("monitor registration");
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
        let result = register_actor_monitor(ptr::null_mut(), ptr::null_mut());
        assert_eq!(result, Err(2));
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

    let watcher = TestActor::spawn_with_sys(test_dispatch, monitor_sys_dispatch);
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
    let ref_id = unsafe {
        register_actor_monitor(watcher.as_ptr(), target.as_ptr()).expect("monitor registration")
    };
    assert_ne!(ref_id, 0, "late monitor should still return a reference");

    let down_messages = MONITOR_DISPATCH_SIGNAL
        .wait_for_down_count(1, Duration::from_secs(5))
        .expect("late monitor registration should deliver DOWN immediately");
    let down = down_messages.last().copied().expect("captured DOWN");
    assert_eq!(down.monitor_id, ref_id);
    assert_eq!(down.target_kind, 0);
    assert_eq!(down.reason_kind, 1);
    assert_eq!(down.slot, hew_runtime::pid::hew_pid_serial(target_id));

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

    let survivor = TestActor::spawn_with_sys(test_dispatch, exit_sys_dispatch);
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

/// THE CRASH CODE IS PUBLISHED BEFORE THE TERMINAL STATE IS VISIBLE.
///
/// Every consumer of an exit reason reads the same two fields in the same
/// order: `actor_state` first, then `error_code`. `link::terminal_exit_reason`
/// does it for a link registered after the crash, `monitor`'s terminal-reason
/// lookup does it for a late monitor, and `hew_actor_await` does it for a
/// caller blocked on the actor. That read is only sound when the crash code is
/// already published by the time a terminal state can be observed.
///
/// The crash teardown's first-wake event fires immediately after the terminal
/// CAS — the actor is `Crashed` to every other thread from that point on, and
/// the teardown below it releases blocked senders and waiters. Reading the
/// error through the public accessor there turns the ordering into a property
/// no scheduler can decide. With the code published after the CAS the accessor
/// returned the field's still-live default, so a reader that won the race saw
/// reason `0` for an actor whose EXIT carried the real crash code.
#[test]
fn a_crash_code_is_published_before_the_terminal_state_is_visible() {
    static FIRST_WAKE_ACTOR: AtomicPtr<HewActor> = AtomicPtr::new(ptr::null_mut());
    static ERROR_AT_FIRST_WAKE: AtomicI32 = AtomicI32::new(HOOK_NEVER_FIRED);

    /// Outside the `i32` range any crash code takes, so "the hook never ran"
    /// can never be mistaken for an observation.
    const HOOK_NEVER_FIRED: i32 = i32::MIN;

    /// The code `scheduler.rs`'s injected-crash branch traps with.
    const INJECTED_CRASH_REASON: i32 = -1;

    struct FirstWakeHookGuard;

    impl Drop for FirstWakeHookGuard {
        fn drop(&mut self) {
            hew_actor_set_crash_teardown_order_hook(None);
            FIRST_WAKE_ACTOR.store(ptr::null_mut(), Ordering::Release);
        }
    }

    fn record_error_at_first_wake(event: i32) {
        if event != HEW_ACTOR_CRASH_TEARDOWN_BEFORE_FIRST_WAKE {
            return;
        }
        let actor = FIRST_WAKE_ACTOR.load(Ordering::Acquire);
        if actor.is_null() {
            return;
        }
        // SAFETY: the actor is live for the whole teardown that fires this
        // event, and the test clears the slot before dropping its handle.
        let error = unsafe { hew_actor_get_error(actor) };
        ERROR_AT_FIRST_WAKE.store(error, Ordering::Release);
    }

    let _guard = TEST_LOCK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    ensure_scheduler();
    hew_deterministic_reset();
    EXIT_DISPATCH_SIGNAL.reset();
    ERROR_AT_FIRST_WAKE.store(HOOK_NEVER_FIRED, Ordering::Release);

    let survivor = TestActor::spawn_with_sys(test_dispatch, exit_sys_dispatch);
    let target = TestActor::spawn(test_dispatch);

    // SAFETY: target is a live actor; reading its id field through the raw
    // pointer is the runtime's documented way to obtain a fault-injection key.
    let target_id = unsafe { (*target.as_ptr()).id };

    // A live actor reports no error. Without this the "published early"
    // assertion below would also hold for a field that is simply always set.
    // SAFETY: target's wrapper is alive; hew_actor_get_error reads from it.
    let error_while_running = unsafe { hew_actor_get_error(target.as_ptr()) };
    assert_eq!(
        error_while_running, 0,
        "a running actor must report no error, so the crash code below is a real transition"
    );

    FIRST_WAKE_ACTOR.store(target.as_ptr(), Ordering::Release);
    hew_actor_set_crash_teardown_order_hook(Some(record_error_at_first_wake));
    let _hook_guard = FirstWakeHookGuard;

    hew_fault_inject_crash(target_id, 1);
    target.send_empty(1);

    assert!(
        target.wait_for_state(HewActorState::Crashed, Duration::from_secs(5)),
        "target should enter Crashed state"
    );

    assert_eq!(
        ERROR_AT_FIRST_WAKE.load(Ordering::Acquire),
        INJECTED_CRASH_REASON,
        "the crash code must already be readable at the first point the terminal state is visible",
    );

    // The same reason the EXIT propagation delivers to a link registered after
    // the crash, read through the accessor the moment the state is terminal.
    // SAFETY: target's wrapper is alive; hew_actor_get_error reads from it.
    let exit_reason = unsafe { hew_actor_get_error(target.as_ptr()) };
    assert_eq!(exit_reason, INJECTED_CRASH_REASON);

    // SAFETY: link takes live actor pointers.
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
            reason: INJECTED_CRASH_REASON,
        })
    );

    // SAFETY: unlink takes live actor pointers.
    unsafe {
        hew_actor_unlink(survivor.as_ptr(), target.as_ptr());
    }
    hew_deterministic_reset();
}
