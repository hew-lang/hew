//! End-to-end oracle: a real supervisor restart surfaces on the export surface.
//!
//! Drives the actual runtime substrate path a compiled Hew program hits —
//! `hew_actor_send` → injected child crash → `hew_actor_trap` (signal context)
//! → supervisor-notify → supervisor dispatch → `apply_restart` →
//! `restart_with_budget_and_strategy` → supervisor span emission — and asserts
//! the `supervisor_restart` lifecycle event appears on the export surface that
//! `/api/traces` drains (`drain_events_json`), carrying a non-zero, sampled
//! trace id (the S3 crash-recovery context establishment).
//!
//! Also pins the behavioural-no-drift invariant: the restart-budget accounting
//! is identical with tracing ON vs OFF — supervisor observability is a
//! read-only side effect that never perturbs control flow.

#![allow(
    clippy::undocumented_unsafe_blocks,
    reason = "Integration test — supervisor child-management FFI is inherently raw; SAFETY notes where load-bearing"
)]

use std::ffi::{c_void, CString};
use std::sync::atomic::{AtomicI32, Ordering};

use hew_runtime::actor::hew_actor_send;
use hew_runtime::deterministic::{hew_deterministic_reset, hew_fault_inject_crash};
use hew_runtime::mailbox::{
    hew_mailbox_free, hew_mailbox_new, hew_mailbox_send, hew_mailbox_send_sys,
    hew_mailbox_try_recv, hew_mailbox_try_recv_sys, hew_msg_node_free,
};
use hew_runtime::supervisor::{
    hew_supervisor_add_child_spec, hew_supervisor_get_child, hew_supervisor_set_restart_notify,
    hew_supervisor_wait_restart, HewChildSpec,
};
use hew_runtime::tracing::{drain_events_json, hew_trace_enable, hew_trace_reset};
use hew_runtime_testkit::{ensure_scheduler, TestSupervisor};

/// Global lock — these tests share mutable global state (fault-injection table,
/// the trace ring buffer, the dispatch counters).
static TEST_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());

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

/// Poll until the supervisor's child at `index` is non-null.
unsafe fn wait_for_child(
    sup: *mut hew_runtime::supervisor::HewSupervisor,
    index: i32,
    timeout_ms: u64,
) -> *mut hew_runtime::actor::HewActor {
    let deadline = std::time::Instant::now() + std::time::Duration::from_millis(timeout_ms);
    loop {
        let child = unsafe { hew_supervisor_get_child(sup, index) };
        if !child.is_null() {
            return child;
        }
        assert!(
            std::time::Instant::now() < deadline,
            "timed out waiting for child[{index}] to spawn"
        );
        std::thread::sleep(std::time::Duration::from_millis(10));
    }
}

unsafe fn crash_child(child: *mut hew_runtime::actor::HewActor) {
    let id = unsafe { (*child).id };
    hew_fault_inject_crash(id, 1);
    unsafe { hew_actor_send(child, 1, std::ptr::null_mut(), 0) };
}

/// Build a one-for-one supervisor with a single permanent child and start it,
/// returning the started supervisor handle.
fn start_one_child_supervisor(name: &str) -> (TestSupervisor, CString) {
    let sup = TestSupervisor::new(STRATEGY_ONE_FOR_ONE, 10, 60);
    let cname = cstr(name);
    let mut state = 0i32;
    // SAFETY: sup wraps a live supervisor; spec fields are valid for the call.
    unsafe {
        hew_supervisor_set_restart_notify(sup.as_ptr());
        let spec = HewChildSpec {
            name: cname.as_ptr(),
            init_state: (&raw mut state).cast(),
            init_state_size: std::mem::size_of::<i32>(),
            dispatch: Some(counting_dispatch),
            restart_policy: RESTART_PERMANENT,
            mailbox_capacity: -1,
            overflow: OVERFLOW_DROP_NEW,
            coalesce_key_fn: None,
            coalesce_fallback: OVERFLOW_DROP_NEW,
            message_drop_fn: None,
            arena_cap_bytes: 0,
            cycle_capable: 0,
            on_crash: None,
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
    }
    (sup, cname)
}

/// Drain the whole export surface into one JSON array's worth of events,
/// repeatedly (the drain yields at most 256 events per call).
fn drain_all() -> Vec<serde_json::Value> {
    let mut out = Vec::new();
    loop {
        let json = drain_events_json();
        let batch: Vec<serde_json::Value> =
            serde_json::from_str(&json).expect("drain_events_json must produce valid JSON");
        let n = batch.len();
        out.extend(batch);
        if n < 256 {
            break;
        }
    }
    out
}

/// THE ORACLE: a forced crash produces a `supervisor_restart` event on the
/// export surface with a non-zero, sampled trace id.
#[test]
fn restart_emits_supervisor_restart_on_export_surface() {
    let _guard = TEST_LOCK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    ensure_scheduler();
    hew_deterministic_reset();
    DISPATCH_COUNT.store(0, Ordering::SeqCst);

    // Enable tracing and clear the ring buffer so we observe only this run.
    hew_trace_reset();
    hew_trace_enable(1);

    let (sup, _name) = start_one_child_supervisor("trace-worker");
    // SAFETY: sup is live for the body.
    unsafe {
        let child = wait_for_child(sup.as_ptr(), 0, 2000);
        crash_child(child);
        let count = hew_supervisor_wait_restart(sup.as_ptr(), 1, 5000);
        assert!(
            count >= 1,
            "expected at least one restart cycle, got {count}"
        );
        // The restarted child must come back.
        let restarted = wait_for_child(sup.as_ptr(), 0, 2000);
        assert!(!restarted.is_null(), "child should have been restarted");
    }

    // Give the supervisor dispatch a beat to flush the emission onto the ring.
    std::thread::sleep(std::time::Duration::from_millis(50));

    let events = drain_all();
    let restart = events
        .iter()
        .find(|e| e["event_type"] == "supervisor_restart")
        .unwrap_or_else(|| {
            panic!(
                "supervisor_restart must appear on the export surface; drained {} events: {:?}",
                events.len(),
                events
                    .iter()
                    .map(|e| e["event_type"].clone())
                    .collect::<Vec<_>>()
            )
        });

    // S3: the crash-recovery span parents under a real, sampled trace id — the
    // `trace_id` hex string must not be all zeros and the span id must be set.
    let trace_id = restart["trace_id"].as_str().unwrap_or("");
    assert!(
        trace_id.chars().any(|c| c != '0'),
        "supervisor_restart must carry a non-zero trace_id (S3 crash-recovery root); got {trace_id:?}"
    );
    assert_ne!(
        restart["span_id"].as_u64().unwrap_or(0),
        0,
        "supervisor_restart must carry a non-zero span id"
    );

    eprintln!("ORACLE supervisor_restart event on export surface: {restart}");

    hew_trace_enable(0);
    hew_trace_reset();
    hew_deterministic_reset();
}

/// Crash the single child three times in sequence, returning the final restart
/// count. Used to compare budget accounting with tracing on vs off.
fn run_three_crashes() -> usize {
    ensure_scheduler();
    hew_deterministic_reset();
    DISPATCH_COUNT.store(0, Ordering::SeqCst);

    let (sup, _name) = start_one_child_supervisor("nodrift-worker");
    // SAFETY: sup is live for the body.
    let count = unsafe {
        for round in 0..3 {
            let child = wait_for_child(sup.as_ptr(), 0, 2000);
            crash_child(child);
            let c = hew_supervisor_wait_restart(sup.as_ptr(), round + 1, 5000);
            assert!(
                c > round,
                "restart {} did not register (got {c})",
                round + 1
            );
        }
        hew_supervisor_wait_restart(sup.as_ptr(), 3, 5000)
    };
    hew_deterministic_reset();
    count
}

/// BEHAVIOURAL NO-DRIFT: the restart-budget accounting is identical with
/// tracing ON vs OFF. Supervisor emission is a read-only side effect that must
/// never change supervisor decisions or counts.
#[test]
fn supervisor_restart_count_is_identical_tracing_on_vs_off() {
    let _guard = TEST_LOCK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);

    // Tracing OFF.
    hew_trace_reset();
    hew_trace_enable(0);
    let off = run_three_crashes();

    // Tracing ON.
    hew_trace_reset();
    hew_trace_enable(1);
    let on = run_three_crashes();
    hew_trace_enable(0);
    hew_trace_reset();

    assert_eq!(
        off, on,
        "restart-budget accounting drifted with tracing enabled (off={off}, on={on})"
    );
    assert!(off >= 3, "expected at least 3 restarts, got {off}");
}

/// MINTING-LOCATION GUARD: a trap-originated supervisor notification
/// (`hew_actor_trap` → `hew_supervisor_notify_child_event` →
/// `hew_mailbox_send_sys`) must carry ZERO/absent trace context AT SEND TIME.
///
/// This pins WHERE the crash-recovery root is minted. The trap runs in
/// signal-handler-adjacent context with the crashing actor's execution context
/// already torn down — so no execution context is installed, exactly the
/// condition this test reproduces (a bare thread with tracing enabled and no
/// installed context). If the system-send path minted (as the pre-fix
/// `current_context()` seam did), the enqueued node would carry a non-zero,
/// sampled root born under the trap call chain — the security violation. The
/// fix routes system sends through the non-minting `system_context()` seam, so
/// the node carries zero and minting is deferred to
/// `ensure_supervisor_trace_root` in `supervisor_dispatch_impl`.
///
/// The contrast assertion proves the external-user-send minting seam (S3) is
/// left intact: a normal `hew_mailbox_send` under identical conditions DOES mint
/// a sampled root. So the fix separates the two seams rather than disabling
/// minting wholesale.
#[test]
fn trap_originated_system_send_carries_zero_context_at_send_time() {
    let _guard = TEST_LOCK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    ensure_scheduler();
    hew_deterministic_reset();

    // Tracing ON, ring cleared — and crucially NO execution context installed on
    // this thread, mirroring the post-teardown trap signal context.
    hew_trace_reset();
    hew_trace_enable(1);

    // SAFETY: mailbox FFI; nodes are freed before the mailbox.
    unsafe {
        let mb = hew_mailbox_new();
        assert!(!mb.is_null(), "mailbox allocation failed");

        let sys_val: i32 = 7;
        // The exact FFI seam the trap path drives.
        hew_mailbox_send_sys(
            mb,
            2,
            (&raw const sys_val).cast_mut().cast(),
            std::mem::size_of::<i32>(),
        );

        let sys_node = hew_mailbox_try_recv_sys(mb);
        assert!(!sys_node.is_null(), "system message must enqueue");
        let sys_ctx = (*sys_node).trace_context;
        assert_eq!(
            (
                sys_ctx.trace_id_hi,
                sys_ctx.trace_id_lo,
                sys_ctx.span_id,
                sys_ctx.parent_span_id,
                sys_ctx.flags
            ),
            (0, 0, 0, 0, 0),
            "trap-originated system send must carry ZERO trace context at send time \
             (mint deferred to supervisor_dispatch_impl); got {sys_ctx:?}"
        );
        hew_msg_node_free(sys_node);

        // Contrast: a normal external user send under the SAME no-context,
        // tracing-on conditions DOES mint a sampled root — the S3 external-send
        // seam is preserved, not disabled.
        let user_val: i32 = 11;
        let rc = hew_mailbox_send(
            mb,
            1,
            (&raw const user_val).cast_mut().cast(),
            std::mem::size_of::<i32>(),
        );
        assert_eq!(rc, 0, "user send should enqueue");
        let user_node = hew_mailbox_try_recv(mb);
        assert!(!user_node.is_null(), "user message must enqueue");
        let user_ctx = (*user_node).trace_context;
        assert!(
            user_ctx.trace_id_hi != 0 || user_ctx.trace_id_lo != 0,
            "external user send must MINT a sampled trace root (S3 external-send seam intact); \
             got {user_ctx:?}"
        );
        assert_eq!(
            user_ctx.flags & 1,
            1,
            "external-send minted root must be sampled; got flags={}",
            user_ctx.flags
        );
        hew_msg_node_free(user_node);

        hew_mailbox_free(mb);
    }

    hew_trace_enable(0);
    hew_trace_reset();
    hew_deterministic_reset();
}
