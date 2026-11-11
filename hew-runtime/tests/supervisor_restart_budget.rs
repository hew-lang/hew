//! Supervisor restart budget tests.
//!
//! Verifies that restart budget accounting is correct under concurrent
//! crashes. Every budget mutation happens on the supervisor's single-threaded
//! actor dispatch; delayed restarts under a declared `backoff` are covered by
//! the `supervisor-restart-backoff*` core-acceptance cases.
//!
//! The supervisor handle is owned by `TestSupervisor` so its Drop runs
//! the canonical stop teardown. The supervisor's FFI surface (add child
//! spec, child lookup, etc.) still uses raw `extern "C"` calls
//! because those are inherently bound to `HewChildSpec` and the
//! runtime-owned child actor pointers; restart completion itself is
//! observed through the Rust-only `test_wait_for_restart`.

#![allow(
    clippy::undocumented_unsafe_blocks,
    reason = "Integration test — supervisor child-management FFI is inherently raw; SAFETY notes on unsafe blocks where load-bearing"
)]

use std::ffi::{c_void, CString};
use std::sync::atomic::{AtomicI32, Ordering};

use hew_runtime::actor::hew_actor_send;
use hew_runtime::deterministic::{hew_deterministic_reset, hew_fault_inject_crash};
use hew_runtime::supervisor::{hew_supervisor_add_child_spec, test_wait_for_restart, HewChildSpec};
use hew_runtime_testkit::{ensure_scheduler, TestSupervisor};

/// Global lock — tests share mutable global state (fault injection table,
/// dispatch counters, crash log).
static TEST_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());

// ── Dispatch helpers ─────────────────────────────────────────────────────

static DISPATCH_COUNT: AtomicI32 = AtomicI32::new(0);

unsafe extern "C-unwind" fn counting_dispatch(
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

fn cstr(s: &str) -> CString {
    CString::new(s).expect("CString::new failed")
}

const STRATEGY_ONE_FOR_ONE: i32 = 0;
const RESTART_PERMANENT: i32 = 0;
const OVERFLOW_DROP_NEW: i32 = 1;

// ── Helpers ──────────────────────────────────────────────────────────────

/// Poll until the supervisor's child at `index` is non-null, returning
/// the child pointer and its actor ID.
///
/// # Safety
/// Caller guarantees `sup` is a live supervisor pointer.
unsafe fn wait_for_child(
    sup: *mut hew_runtime::supervisor::HewSupervisor,
    index: i32,
) -> (*mut hew_runtime::actor::HewActor, u64) {
    loop {
        // SAFETY: sup is live per fn contract.
        let child = unsafe { hew_runtime::supervisor::hew_supervisor_get_child(sup, index) };
        if !child.is_null() {
            // SAFETY: child is a runtime-owned actor pointer; reading id is safe.
            return (child, unsafe { (*child).id });
        }
        std::thread::sleep(std::time::Duration::from_millis(10));
    }
}

/// Crash a child actor and wait for the crash to be detected.
///
/// # Safety
/// Caller guarantees `child` is a live actor pointer.
unsafe fn crash_child(child: *mut hew_runtime::actor::HewActor) {
    // SAFETY: child is live; reading id and sending a message are safe.
    let id = unsafe { (*child).id };
    hew_fault_inject_crash(id, 1);
    unsafe { hew_actor_send(child, 1, std::ptr::null_mut(), 0) };
}

// ── Tests ────────────────────────────────────────────────────────────────

/// Simultaneous crashes on multiple children should each decrement the
/// budget exactly once — no double-counting or skipping.
#[test]
fn concurrent_crashes_decrement_budget_correctly() {
    let _guard = TEST_LOCK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    ensure_scheduler();
    hew_deterministic_reset();
    DISPATCH_COUNT.store(0, Ordering::SeqCst);

    // Budget of 10, window of 60s.
    let sup = TestSupervisor::new(STRATEGY_ONE_FOR_ONE, 10, 60);
    // SAFETY: sup wraps a live supervisor; the FFI calls below take its raw ptr.
    unsafe {
        // Add 3 children.
        let names: Vec<CString> = (0..3).map(|i| cstr(&format!("worker-{i}"))).collect();
        let mut states = [0i32; 3];
        for i in 0..3 {
            let spec = HewChildSpec {
                name: names[i].as_ptr(),
                init_state: (&raw mut states[i]).cast(),
                init_state_size: std::mem::size_of::<i32>(),
                dispatch: Some(counting_dispatch),
                restart_policy: RESTART_PERMANENT,
                mailbox_capacity: -1,
                overflow: OVERFLOW_DROP_NEW,
                coalesce_match_fn: None,
                coalesce_fallback: OVERFLOW_DROP_NEW,
                message_drop_fn: None,
                sys_dispatch: None,
                arena_cap_bytes: 0,
                cycle_capable: 0,
                lifecycle_fn: None,
                init_fn: None,
                config: std::ptr::null_mut(),
                config_size: 0,
            };
            assert_eq!(
                hew_supervisor_add_child_spec(sup.as_ptr(), &raw const spec),
                0
            );
        }
        assert_eq!(sup.start(), 0);

        // Wait for all 3 children to be spawned.
        let mut children = Vec::new();
        for i in 0..3 {
            let (child, _id) = wait_for_child(sup.as_ptr(), i);
            children.push(child);
        }

        // Crash all 3 simultaneously.
        for &child in &children {
            crash_child(child);
        }

        // Wait for 3 restart cycles.
        let count = test_wait_for_restart(sup.as_ptr(), 3, 5000);
        assert!(
            count >= 3,
            "expected at least 3 restart cycles, got {count}"
        );

        // Supervisor should still be running — budget was 10, only 3 used.
        assert!(
            sup.is_running(),
            "supervisor should still be running after 3 restarts within budget of 10"
        );

        // All 3 children should be respawned.
        for i in 0..3 {
            let (child, _) = wait_for_child(sup.as_ptr(), i);
            assert!(!child.is_null(), "child[{i}] should have been restarted");
        }
    }
    hew_deterministic_reset();
    // TestSupervisor::Drop calls hew_supervisor_stop.
}

/// Exceeding the restart budget causes the supervisor to stop.
#[test]
fn budget_exhaustion_stops_supervisor() {
    let _guard = TEST_LOCK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    ensure_scheduler();
    hew_deterministic_reset();
    DISPATCH_COUNT.store(0, Ordering::SeqCst);

    // Budget of 2 restarts in 60 seconds.
    let sup = TestSupervisor::new(STRATEGY_ONE_FOR_ONE, 2, 60);
    // SAFETY: live supervisor used for child-management FFI.
    unsafe {
        let name = cstr("doomed");
        let mut state: i32 = 0;
        let spec = HewChildSpec {
            name: name.as_ptr(),
            init_state: (&raw mut state).cast(),
            init_state_size: std::mem::size_of::<i32>(),
            dispatch: Some(counting_dispatch),
            restart_policy: RESTART_PERMANENT,
            mailbox_capacity: -1,
            overflow: OVERFLOW_DROP_NEW,
            coalesce_match_fn: None,
            coalesce_fallback: OVERFLOW_DROP_NEW,
            message_drop_fn: None,
            sys_dispatch: None,
            arena_cap_bytes: 0,
            cycle_capable: 0,
            lifecycle_fn: None,
            init_fn: None,
            config: std::ptr::null_mut(),
            config_size: 0,
        };
        assert_eq!(
            hew_supervisor_add_child_spec(sup.as_ptr(), &raw const spec),
            0
        );
        assert_eq!(sup.start(), 0);

        // Crash the child 3 times — the third crash should exhaust the
        // budget (2 restarts) and stop the supervisor.
        for round in 0..3 {
            let (child, _) = wait_for_child(sup.as_ptr(), 0);
            crash_child(child);

            // Wait for this restart cycle to complete.
            let count = test_wait_for_restart(sup.as_ptr(), round + 1, 5000);
            assert!(
                count > round,
                "restart cycle {round} not completed (count={count})"
            );

            // After the 3rd crash, the budget is exhausted.
            if round == 2 {
                // The supervisor stops after exhausting its budget.
                while sup.is_running() {
                    std::thread::sleep(std::time::Duration::from_millis(10));
                }
            }
        }

        assert!(
            !sup.is_running(),
            "supervisor must be stopped after exhausting restart budget"
        );
    }
    hew_deterministic_reset();
}

/// Without a declared `backoff`, repeated crashes of several children restart
/// in line every time and the budget counts each restart once.
#[test]
fn repeated_restarts_without_backoff_budget_consistent() {
    let _guard = TEST_LOCK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    ensure_scheduler();
    hew_deterministic_reset();
    DISPATCH_COUNT.store(0, Ordering::SeqCst);

    // Budget of 20 in 60s — enough for all restarts.
    let sup = TestSupervisor::new(STRATEGY_ONE_FOR_ONE, 20, 60);
    // SAFETY: live supervisor used for child-management FFI.
    unsafe {
        // Spawn 3 children.
        let names: Vec<CString> = (0..3).map(|i| cstr(&format!("multi-{i}"))).collect();
        let mut states = [0i32; 3];
        for i in 0..3 {
            let spec = HewChildSpec {
                name: names[i].as_ptr(),
                init_state: (&raw mut states[i]).cast(),
                init_state_size: std::mem::size_of::<i32>(),
                dispatch: Some(counting_dispatch),
                restart_policy: RESTART_PERMANENT,
                mailbox_capacity: -1,
                overflow: OVERFLOW_DROP_NEW,
                coalesce_match_fn: None,
                coalesce_fallback: OVERFLOW_DROP_NEW,
                message_drop_fn: None,
                sys_dispatch: None,
                arena_cap_bytes: 0,
                cycle_capable: 0,
                lifecycle_fn: None,
                init_fn: None,
                config: std::ptr::null_mut(),
                config_size: 0,
            };
            assert_eq!(
                hew_supervisor_add_child_spec(sup.as_ptr(), &raw const spec),
                0
            );
        }
        assert_eq!(sup.start(), 0);

        // Round 1: crash all 3 — immediate restarts.
        for i in 0..3 {
            let (child, _) = wait_for_child(sup.as_ptr(), i);
            crash_child(child);
        }
        let count = test_wait_for_restart(sup.as_ptr(), 3, 5000);
        assert!(count >= 3, "round 1: expected 3 restarts, got {count}");

        // Round 2: crash all 3 again — still immediate; no implicit delay.
        for i in 0..3 {
            let (child, _) = wait_for_child(sup.as_ptr(), i);
            crash_child(child);
        }
        let count = test_wait_for_restart(sup.as_ptr(), 6, 10_000);
        assert!(
            count >= 6,
            "round 2: expected 6 total restarts, got {count}"
        );

        // All children should be alive.
        for i in 0..3 {
            let (child, _) = wait_for_child(sup.as_ptr(), i);
            assert!(!child.is_null(), "child[{i}] should be alive after round 2");
        }

        // Supervisor should still be running.
        assert!(
            sup.is_running(),
            "supervisor should still be running after 6 restarts"
        );
    }
    hew_deterministic_reset();
}
