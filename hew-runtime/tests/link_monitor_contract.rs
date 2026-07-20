//! FFI-ownership-contracts pin tests for `hew_actor_link` / `hew_actor_monitor`.
//!
//! These tests pin the idempotence and death-handling invariants described in
//! LESSONS row P0 `ffi-ownership-contracts` and the v0.5 failure-philosophy
//! plan (Q47 / A24).
//!
//! Contract summary:
//! - `link(already_linked)` is idempotent — calling twice does not error or panic.
//! - `link(dead_target)` is void and delivers an EXIT signal to the caller —
//!   it does not crash the linker.
//! - `monitor(dead_target)` returns a valid non-zero `ref_id` and immediately
//!   fires a DOWN message to the watcher.
//! - `demonitor(ref_id)` after the target has already died returns silently (no
//!   panic, no double-free).

use hew_runtime::deterministic::{hew_deterministic_reset, hew_fault_inject_crash};
use hew_runtime::link::hew_actor_link;
use hew_runtime::monitor::{hew_actor_demonitor, register_actor_monitor, HewDownMessage};
use hew_runtime::supervisor::{SYS_MSG_DOWN, SYS_MSG_EXIT};
use hew_runtime_testkit::{ensure_scheduler, HewActorState, TestActor};
use std::ffi::c_void;
use std::sync::{Condvar, Mutex};
use std::time::{Duration, Instant};

static TEST_LOCK: Mutex<()> = Mutex::new(());

// ── Signal infrastructure ────────────────────────────────────────────────────

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(C)]
struct ExitMessageView {
    crashed_actor_id: u64,
    reason: i32,
}

struct ExitSignal {
    state: Mutex<Vec<ExitMessageView>>,
    cond: Condvar,
}

impl ExitSignal {
    const fn new() -> Self {
        Self {
            state: Mutex::new(Vec::new()),
            cond: Condvar::new(),
        }
    }

    fn reset(&self) {
        self.state.lock().unwrap().clear();
    }

    fn record(&self, msg_type: i32, data: *mut c_void, data_size: usize) {
        let mut state = self.state.lock().unwrap();
        if msg_type == SYS_MSG_EXIT
            && !data.is_null()
            && data_size == std::mem::size_of::<ExitMessageView>()
        {
            // SAFETY: The runtime sent a SYS_MSG_EXIT payload with the exact
            // expected size, so the cast and unaligned read are valid.
            let msg = unsafe { (data.cast::<ExitMessageView>().cast_const()).read_unaligned() };
            state.push(msg);
            self.cond.notify_all();
        }
    }

    fn wait_for_count(&self, n: usize, timeout: Duration) -> Option<Vec<ExitMessageView>> {
        let deadline = Instant::now() + timeout;
        let mut state = self.state.lock().unwrap();
        loop {
            if state.len() >= n {
                return Some(state.clone());
            }
            let remaining = deadline.saturating_duration_since(Instant::now());
            if remaining.is_zero() {
                return None;
            }
            let (guard, result) = self.cond.wait_timeout(state, remaining).unwrap();
            state = guard;
            if result.timed_out() && state.len() < n {
                return None;
            }
        }
    }
}

struct DownSignal {
    state: Mutex<Vec<HewDownMessage>>,
    cond: Condvar,
}

impl DownSignal {
    const fn new() -> Self {
        Self {
            state: Mutex::new(Vec::new()),
            cond: Condvar::new(),
        }
    }

    fn reset(&self) {
        self.state.lock().unwrap().clear();
    }

    fn record(&self, msg_type: i32, data: *mut c_void, data_size: usize) {
        if msg_type == SYS_MSG_DOWN
            && !data.is_null()
            && data_size == std::mem::size_of::<HewDownMessage>()
        {
            // SAFETY: runtime sent SYS_MSG_DOWN with exact expected size.
            let msg = unsafe { (data.cast::<HewDownMessage>().cast_const()).read_unaligned() };
            let mut state = self.state.lock().unwrap();
            state.push(msg);
            self.cond.notify_all();
        }
    }

    fn wait_for_count(&self, n: usize, timeout: Duration) -> Option<Vec<HewDownMessage>> {
        let deadline = Instant::now() + timeout;
        let mut state = self.state.lock().unwrap();
        loop {
            if state.len() >= n {
                return Some(state.clone());
            }
            let remaining = deadline.saturating_duration_since(Instant::now());
            if remaining.is_zero() {
                return None;
            }
            let (guard, result) = self.cond.wait_timeout(state, remaining).unwrap();
            state = guard;
            if result.timed_out() && state.len() < n {
                return None;
            }
        }
    }
}

static EXIT_SIGNAL: ExitSignal = ExitSignal::new();
static DOWN_SIGNAL: DownSignal = DownSignal::new();

unsafe extern "C-unwind" fn noop_dispatch(
    _ctx: *mut hew_runtime::execution_context::HewExecutionContext,
    _state: *mut c_void,
    _msg_type: i32,
    _data: *mut c_void,
    _size: usize,
    _borrow_mode: i32,
) -> *mut c_void {
    std::ptr::null_mut()
}

unsafe extern "C-unwind" fn exit_capture_dispatch(
    _ctx: *mut hew_runtime::execution_context::HewExecutionContext,
    _state: *mut c_void,
    msg_type: i32,
    data: *mut c_void,
    data_size: usize,
    _borrow_mode: i32,
) -> *mut c_void {
    EXIT_SIGNAL.record(msg_type, data, data_size);
    std::ptr::null_mut()
}

unsafe extern "C-unwind" fn down_capture_dispatch(
    _ctx: *mut hew_runtime::execution_context::HewExecutionContext,
    _state: *mut c_void,
    msg_type: i32,
    data: *mut c_void,
    data_size: usize,
    _borrow_mode: i32,
) -> *mut c_void {
    DOWN_SIGNAL.record(msg_type, data, data_size);
    std::ptr::null_mut()
}

// ── Contract tests ───────────────────────────────────────────────────────────

/// Linking two live actors twice is idempotent — the second call does not panic.
///
/// Per Q47/A24 ratification and LESSONS `ffi-ownership-contracts`: the runtime
/// absorbs a duplicate link silently. The Hew `link()` builtin therefore always
/// resolves to `Ok(())` when both actors are alive, regardless of whether the
/// link was already established.
///
/// Note: `hew_actor_link` returns void — `AlreadyLinked` and `TargetDead` are
/// forward-compatibility discriminants in `LinkError` for future runtime
/// revisions; the current runtime does not surface them as return codes.
#[test]
fn link_idempotent() {
    let actor_a = TestActor::spawn(noop_dispatch);
    let actor_b = TestActor::spawn(noop_dispatch);

    // First link — establishes the bidirectional link.
    // SAFETY: both actors are live for the duration of this test.
    unsafe { hew_actor_link(actor_a.as_ptr(), actor_b.as_ptr()) };

    // Second link — must not panic, must not leave inconsistent state.
    // SAFETY: both actors are still live.
    unsafe { hew_actor_link(actor_a.as_ptr(), actor_b.as_ptr()) };

    // TestActor::Drop closes and frees both actors — no assertion on teardown.
    // Success = both calls complete without panicking.
}

/// `link(dead_target)` does not crash the linker actor and delivers an EXIT
/// signal, identical to linking a live actor that subsequently dies.
///
/// Per Q47/A24: this is the race condition where the target dies between the
/// caller obtaining its handle and calling `link()`. The runtime handles it
/// gracefully by sending a synthetic EXIT signal to the survivor.
#[test]
fn link_to_dead_actor_sends_exit_and_does_not_panic() {
    let _guard = TEST_LOCK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    ensure_scheduler();
    hew_deterministic_reset();
    EXIT_SIGNAL.reset();

    let survivor = TestActor::spawn(exit_capture_dispatch);
    let target = TestActor::spawn(noop_dispatch);

    // SAFETY: reading the id field via a valid live pointer is the runtime's
    // documented way to obtain a fault-injection key.
    let target_id = unsafe { (*target.as_ptr()).id };

    // Crash the target before linking.
    hew_fault_inject_crash(target_id, 1);
    target.send_empty(1);
    assert!(
        target.wait_for_state(HewActorState::Crashed, Duration::from_secs(5)),
        "target actor should reach Crashed state"
    );

    // Link to the dead target — must not panic, must not crash the survivor.
    // SAFETY: survivor is live; target is dead but its allocation remains valid
    // until TestActor::Drop calls hew_actor_free at scope exit.
    unsafe { hew_actor_link(survivor.as_ptr(), target.as_ptr()) };

    // An EXIT signal must arrive at the survivor.
    let exits = EXIT_SIGNAL
        .wait_for_count(1, Duration::from_secs(5))
        .expect("link to dead target must deliver EXIT to the survivor");
    let exit = exits.last().copied().unwrap();
    assert_eq!(
        exit.crashed_actor_id, target_id,
        "EXIT must identify the dead target"
    );

    hew_deterministic_reset();
}

/// `monitor(dead_target)` returns a valid non-zero `ref_id` and fires a DOWN
/// message immediately, without leaving a stale monitor registration.
///
/// This pins the late-registration path in `hew_actor_monitor`: when `target`
/// is already in a terminal state, the runtime:
///   1. Generates a `ref_id` (non-zero).
///   2. Detects the terminal state via `terminal_reasons`.
///   3. Immediately sends a DOWN message to the watcher.
///   4. Does NOT insert a persistent monitor entry.
#[test]
fn monitor_to_dead_actor_returns_ok() {
    let _guard = TEST_LOCK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    ensure_scheduler();
    hew_deterministic_reset();
    DOWN_SIGNAL.reset();

    let watcher = TestActor::spawn(down_capture_dispatch);
    let target = TestActor::spawn(noop_dispatch);

    // SAFETY: reading id via valid live pointer.
    let target_id = unsafe { (*target.as_ptr()).id };

    // Crash the target.
    hew_fault_inject_crash(target_id, 1);
    target.send_empty(1);
    assert!(
        target.wait_for_state(HewActorState::Crashed, Duration::from_secs(5)),
        "target should reach Crashed state"
    );

    // Late monitor registration — must return a non-zero ref_id.
    // SAFETY: watcher is live; target is dead but pointer still valid.
    let ref_id = unsafe {
        register_actor_monitor(watcher.as_ptr(), target.as_ptr()).expect("monitor registration")
    };
    assert_ne!(
        ref_id, 0,
        "monitor on dead target must return a valid (non-zero) ref_id"
    );

    // DOWN must arrive immediately.
    let downs = DOWN_SIGNAL
        .wait_for_count(1, Duration::from_secs(5))
        .expect("monitor on dead target must deliver DOWN immediately");
    let down = downs.last().copied().unwrap();
    assert_eq!(
        down.slot,
        hew_runtime::pid::hew_pid_serial(target_id),
        "DOWN must identify the dead target"
    );
    assert_eq!(
        down.monitor_id, ref_id,
        "DOWN ref_id must match the ref_id returned by hew_actor_monitor"
    );

    // Demonitor with the ref_id from a dead-target monitor must be a silent no-op.
    // The entry was never inserted (no persistent registration for dead targets),
    // so this exercises the unknown-ref_id path in hew_actor_demonitor.
    hew_actor_demonitor(ref_id);

    hew_deterministic_reset();
}

/// `demonitor(ref_id)` is idempotent when the target has died and its monitor
/// table entry was swept by `notify_monitors_on_death`.
///
/// This pins the LESSONS `ffi-ownership-contracts` invariant from PR #1727:
/// `hew_actor_demonitor` must never panic for a stale or already-swept `ref_id`.
/// Callers following the `MonitorRef::close() + Drop` pattern call demonitor
/// twice on the same `ref_id` — both must succeed silently.
#[test]
fn demonitor_after_target_death_is_silent_ok() {
    let _guard = TEST_LOCK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    ensure_scheduler();
    hew_deterministic_reset();

    let watcher = TestActor::spawn(noop_dispatch);
    let target = TestActor::spawn(noop_dispatch);

    // SAFETY: reading id via valid live pointer.
    let target_id = unsafe { (*target.as_ptr()).id };

    // Establish a live monitor before the target dies.
    // SAFETY: both actors are live at this point.
    let ref_id = unsafe {
        register_actor_monitor(watcher.as_ptr(), target.as_ptr()).expect("monitor registration")
    };
    assert_ne!(ref_id, 0, "monitor setup must succeed");

    // Kill the target — this sweeps the monitor table entry via
    // `notify_monitors_on_death`.
    hew_fault_inject_crash(target_id, 1);
    target.send_empty(1);
    assert!(
        target.wait_for_state(HewActorState::Crashed, Duration::from_secs(5)),
        "target should reach Crashed state"
    );

    // Allow sweep to complete.
    std::thread::sleep(Duration::from_millis(100));

    // First demonitor — the entry may already be swept; must be a no-op.
    hew_actor_demonitor(ref_id);

    // Second demonitor — idempotence check (simulates MonitorRef::close + Drop).
    hew_actor_demonitor(ref_id);

    hew_deterministic_reset();
}
