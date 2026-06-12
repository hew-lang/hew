//! `on(crash)` handler invocation tests.
//!
//! Slice 4 of the v0.5 `on(crash)` runtime wire: when a supervised child
//! traps, the supervisor's `apply_restart` invokes any registered
//! `HewOnCrashFn` BEFORE consulting the restart policy. The handler runs in
//! the supervisor's own dispatch context, receives the real trap-code
//! captured on the crashed actor's `error_code` slot, and sees the child's
//! template seed-state pointer (the crashed actor was already torn down).
//!
//! These tests pin two invariants:
//! 1. The handler fires exactly once per crash.
//! 2. After the handler runs, the restart still happens.

#![allow(
    clippy::undocumented_unsafe_blocks,
    reason = "Integration test — supervisor child-management FFI is inherently raw; SAFETY notes on unsafe blocks where load-bearing"
)]

use std::ffi::{c_void, CString};
use std::sync::atomic::{AtomicI32, AtomicI64, Ordering};

use hew_runtime::actor::hew_actor_send;
use hew_runtime::deterministic::{hew_deterministic_reset, hew_fault_inject_crash};
use hew_runtime::supervisor::{
    hew_supervisor_add_child_spec, hew_supervisor_set_restart_notify, hew_supervisor_wait_restart,
    HewChildSpec,
};
use hew_runtime_testkit::{ensure_scheduler, TestSupervisor};

/// Global lock — these tests mutate fault-injection state and handler-side
/// global counters, which are not pairwise-isolated across `cargo test`'s
/// multi-threaded runner.
static TEST_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());

const STRATEGY_ONE_FOR_ONE: i32 = 0;
const RESTART_PERMANENT: i32 = 0;
const OVERFLOW_DROP_NEW: i32 = 1;

// ── Dispatch + handler globals ───────────────────────────────────────────

static DISPATCH_COUNT: AtomicI32 = AtomicI32::new(0);
static ON_CRASH_CALLS: AtomicI32 = AtomicI32::new(0);
static LAST_CRASH_CODE: AtomicI64 = AtomicI64::new(0);

unsafe extern "C-unwind" fn noop_dispatch(
    _ctx: *mut hew_runtime::execution_context::HewExecutionContext,
    _state: *mut c_void,
    _msg_type: i32,
    _data: *mut c_void,
    _data_size: usize,
    _borrow_mode: i32,
) -> *mut c_void {
    DISPATCH_COUNT.fetch_add(1, Ordering::SeqCst);
    std::ptr::null_mut()
}

/// Test `on_crash` handler: records that it fired and captures the trap code.
///
/// `crash_code` is `i64` matching the updated `HewOnCrashFn` ABI, which aligns
/// with `PanicInfo.code: i64` in `std/failure.hew`.
unsafe extern "C" fn recording_on_crash(
    _ctx: *mut hew_runtime::execution_context::HewExecutionContext,
    crash_code: i64,
    _actor_state_ptr: *mut c_void,
) {
    ON_CRASH_CALLS.fetch_add(1, Ordering::SeqCst);
    LAST_CRASH_CODE.store(crash_code, Ordering::SeqCst);
}

fn cstr(s: &str) -> CString {
    CString::new(s).expect("CString::new failed")
}

unsafe fn wait_for_child(
    sup: *mut hew_runtime::supervisor::HewSupervisor,
    index: i32,
    timeout_ms: u64,
) -> *mut hew_runtime::actor::HewActor {
    let deadline = std::time::Instant::now() + std::time::Duration::from_millis(timeout_ms);
    loop {
        let child = unsafe { hew_runtime::supervisor::hew_supervisor_get_child(sup, index) };
        if !child.is_null() {
            return child;
        }
        assert!(
            std::time::Instant::now() < deadline,
            "timed out waiting for child[{index}] to be spawned"
        );
        std::thread::sleep(std::time::Duration::from_millis(10));
    }
}

unsafe fn crash_child(child: *mut hew_runtime::actor::HewActor) {
    let id = unsafe { (*child).id };
    hew_fault_inject_crash(id, 1);
    unsafe { hew_actor_send(child, 1, std::ptr::null_mut(), 0) };
}

// ── Tests ────────────────────────────────────────────────────────────────

/// With a non-null `on_crash` handler installed in the child spec, the
/// supervisor must invoke it exactly once when the child crashes — before
/// applying the restart policy — and the restart must still happen.
#[test]
fn on_crash_handler_fires_once_per_crash_then_restart_proceeds() {
    let _guard = TEST_LOCK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    ensure_scheduler();
    hew_deterministic_reset();
    DISPATCH_COUNT.store(0, Ordering::SeqCst);
    ON_CRASH_CALLS.store(0, Ordering::SeqCst);
    LAST_CRASH_CODE.store(0, Ordering::SeqCst);

    // Budget of 10 — well above our one crash — and a generous 60s window.
    let sup = TestSupervisor::new(STRATEGY_ONE_FOR_ONE, 10, 60);

    let name = cstr("on-crash-worker");
    let mut state: i32 = 0;

    // SAFETY: sup is live; spec lives across the FFI call.
    unsafe {
        hew_supervisor_set_restart_notify(sup.as_ptr());

        let spec = HewChildSpec {
            name: name.as_ptr(),
            init_state: (&raw mut state).cast(),
            init_state_size: std::mem::size_of::<i32>(),
            dispatch: Some(noop_dispatch),
            restart_policy: RESTART_PERMANENT,
            mailbox_capacity: -1,
            overflow: OVERFLOW_DROP_NEW,
            arena_cap_bytes: 0,
            cycle_capable: 0,
            on_crash: Some(recording_on_crash),
        };
        assert_eq!(
            hew_supervisor_add_child_spec(sup.as_ptr(), &raw const spec),
            0
        );
        assert_eq!(sup.start(), 0);

        let child = wait_for_child(sup.as_ptr(), 0, 2000);
        crash_child(child);

        // Wait for the restart cycle to complete.
        let count = hew_supervisor_wait_restart(sup.as_ptr(), 1, 5000);
        assert!(count >= 1, "expected at least 1 restart cycle, got {count}");

        // Handler fired exactly once during this single restart cycle.
        assert_eq!(
            ON_CRASH_CALLS.load(Ordering::SeqCst),
            1,
            "on_crash handler should fire exactly once per crash"
        );

        // hew_fault_inject_crash drives the trap through hew_actor_trap
        // with error_code == -1 (see scheduler.rs:744); the routing path
        // must surface that value to the handler instead of the historical
        // 11 placeholder or 0/unknown.
        assert_eq!(
            LAST_CRASH_CODE.load(Ordering::SeqCst),
            -1i64,
            "handler must receive the real trap code captured on the actor"
        );

        // The restart still happened: child slot 0 is non-null again.
        let restarted = wait_for_child(sup.as_ptr(), 0, 2000);
        assert!(
            !restarted.is_null(),
            "child should have been restarted after the on_crash handler ran"
        );

        // Supervisor is still running — budget of 10 not exhausted.
        assert!(
            sup.is_running(),
            "supervisor should remain running after one crash inside budget"
        );
    }

    hew_deterministic_reset();
    // TestSupervisor::Drop calls hew_supervisor_stop.
}

/// A child spec with `on_crash: None` must NOT call any handler — the
/// existing null-branch must stay exercised after the wiring change. This
/// guards against the read path inadvertently de-referencing null.
#[test]
fn null_on_crash_handler_is_skipped_cleanly() {
    let _guard = TEST_LOCK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    ensure_scheduler();
    hew_deterministic_reset();
    DISPATCH_COUNT.store(0, Ordering::SeqCst);
    ON_CRASH_CALLS.store(0, Ordering::SeqCst);

    let sup = TestSupervisor::new(STRATEGY_ONE_FOR_ONE, 10, 60);

    let name = cstr("no-on-crash-worker");
    let mut state: i32 = 0;

    // SAFETY: sup is live; spec lives across the FFI call.
    unsafe {
        hew_supervisor_set_restart_notify(sup.as_ptr());

        let spec = HewChildSpec {
            name: name.as_ptr(),
            init_state: (&raw mut state).cast(),
            init_state_size: std::mem::size_of::<i32>(),
            dispatch: Some(noop_dispatch),
            restart_policy: RESTART_PERMANENT,
            mailbox_capacity: -1,
            overflow: OVERFLOW_DROP_NEW,
            arena_cap_bytes: 0,
            cycle_capable: 0,
            on_crash: None,
        };
        assert_eq!(
            hew_supervisor_add_child_spec(sup.as_ptr(), &raw const spec),
            0
        );
        assert_eq!(sup.start(), 0);

        let child = wait_for_child(sup.as_ptr(), 0, 2000);
        crash_child(child);

        let count = hew_supervisor_wait_restart(sup.as_ptr(), 1, 5000);
        assert!(count >= 1, "expected at least 1 restart cycle, got {count}");

        assert_eq!(
            ON_CRASH_CALLS.load(Ordering::SeqCst),
            0,
            "no handler installed: ON_CRASH_CALLS must stay at zero"
        );

        let restarted = wait_for_child(sup.as_ptr(), 0, 2000);
        assert!(
            !restarted.is_null(),
            "child should still be restarted when no handler is installed"
        );
    }

    hew_deterministic_reset();
}
