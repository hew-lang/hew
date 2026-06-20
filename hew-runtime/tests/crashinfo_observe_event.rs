//! Integration: a divide-by-zero trap inside an actor dispatch surfaces on
//! the profiler/observe event surface with the canonical `trap_kind`
//! discriminator populated.
//!
//! Producer: `hew_runtime::crash::snapshot_crashes_json` (the JSON body
//! returned by the profiler's `/api/crashes` endpoint).
//! Consumer: `hew_observe::client::CrashEntry` (the typed row downstream
//! tools deserialize from that endpoint).
//!
//! Trap mechanism: the test's dispatch invokes
//! `hew_runtime::supervisor::hew_trap_with_code(HEW_TRAP_DIVIDE_BY_ZERO)`,
//! which is exactly the C-ABI entry-point codegen emits in the IR it
//! produces for `lhs / 0`. The longjmp recovery seam catches the trap,
//! `handle_crash_recovery_impl` builds a `CrashReport` with
//! `signal = HEW_TRAP_DIVIDE_BY_ZERO`, and pushes it to the global crash
//! log that `snapshot_crashes_json` serialises.
//!
//! Gating: the longjmp seam is native-only. WASM has its own non-actor
//! `hew_trap_with_code` path (no recovery context, no `CrashReport`).
//! The producer-side `trap_kind` derivation lives in
//! `hew-runtime/src/crash.rs::snapshot_crashes_json`, behind the same
//! `profiler` feature it has always required.

#![cfg(all(not(target_arch = "wasm32"), feature = "profiler"))]
#![allow(
    clippy::undocumented_unsafe_blocks,
    reason = "Integration test — supervisor and dispatch FFI are inherently raw"
)]

use std::ffi::c_void;
use std::sync::Mutex;
use std::time::{Duration, Instant};

use hew_runtime::actor::hew_actor_send;
use hew_runtime::crash::{hew_crash_log_count, hew_crash_log_last, snapshot_crashes_json};
use hew_runtime::deterministic::hew_deterministic_reset;
use hew_runtime::supervisor::{
    hew_supervisor_add_child_spec, hew_supervisor_get_child_wait,
    hew_supervisor_set_restart_notify, hew_supervisor_wait_restart, hew_trap_with_code,
    HewChildSpec, HEW_TRAP_DIVIDE_BY_ZERO,
};
use hew_runtime_testkit::ensure_scheduler;
use serde::Deserialize;

/// Field-for-field mirror of `hew_observe::client::CrashEntry`
/// (`hew-observe/src/client.rs`). The integration boundary between
/// `hew-runtime`'s profiler `/api/crashes` producer and `hew-observe`'s
/// downstream consumer is *the JSON shape* — by deserialising the
/// producer's bytes into the exact same `#[derive(Deserialize)]`
/// declaration the observe TUI uses, the test asserts that the wire
/// contract is satisfied at the field level, not just that bytes flow.
///
/// `hew-observe` ships as a bin-only crate, so it cannot be added as a
/// `dev-dependency` of `hew-runtime` without converting it into a lib
/// (which would surface a wave of unrelated `pub` clippy lints across
/// the TUI internals). The mirror here is intentional duplication; if
/// the producer adds or renames a field, both this struct and the
/// observe `CrashEntry` must update together.
#[derive(Debug, Clone, Default, Deserialize)]
#[allow(
    dead_code,
    reason = "Mirror struct for the /api/crashes wire contract; fields are present \
              to pin the JSON shape downstream consumers depend on, even when this \
              test does not assert each one individually."
)]
struct ObserveCrashEntry {
    #[serde(default)]
    time_s: f64,
    #[serde(default)]
    actor_id: u64,
    #[serde(default)]
    signal: i32,
    #[serde(default)]
    trap_kind: String,
    #[serde(default)]
    msg_type: i32,
    #[serde(default)]
    fault_addr: u64,
}

/// Process-global lock — `RECENT_CRASHES`, fault-injection table, and the
/// scheduler are shared with other integration tests in the same binary.
static TEST_LOCK: Mutex<()> = Mutex::new(());

/// Message type that triggers the divide-by-zero trap from within dispatch.
const MSG_DIV_BY_ZERO: i32 = 1;

unsafe extern "C-unwind" fn divide_by_zero_dispatch(
    _ctx: *mut hew_runtime::execution_context::HewExecutionContext,
    _state: *mut c_void,
    msg_type: i32,
    _data: *mut c_void,
    _data_size: usize,
    _borrow_mode: i32,
) -> *mut c_void {
    if msg_type == MSG_DIV_BY_ZERO {
        // Same C-ABI seam codegen emits for `Terminator::Trap { kind: DivideByZero }`.
        // Longjmps back to the scheduler's sigsetjmp frame; never returns.
        unsafe {
            hew_trap_with_code(HEW_TRAP_DIVIDE_BY_ZERO);
        }
    }
    std::ptr::null_mut()
}

/// Wait for the global crash log to grow past `log_before`, then return the
/// newest report's `(actor_id, signal)`. `RECENT_CRASHES` is populated by
/// `handle_crash_recovery_impl` before the supervisor restart cycle runs, so
/// the report is visible long before restart bookkeeping completes.
unsafe fn wait_for_new_crash(log_before: i32, timeout: Duration) -> Option<(u64, i32)> {
    let deadline = Instant::now() + timeout;
    loop {
        let count = unsafe { hew_crash_log_count() };
        if count > log_before {
            let report = unsafe { hew_crash_log_last() };
            return Some((report.actor_id, report.signal));
        }
        if Instant::now() >= deadline {
            return None;
        }
        std::thread::sleep(Duration::from_millis(5));
    }
}

/// Gate test: a Hew-canonical divide-by-zero trap inside actor dispatch
/// surfaces on the profiler event surface with `trap_kind == "DivideByZero"`
/// AND the typed downstream `CrashEntry` consumer (`hew_observe::client`)
/// deserialises that field at the value level.
///
/// Steps:
/// 1. Supervised actor spawns with `divide_by_zero_dispatch`.
/// 2. The actor is sent `MSG_DIV_BY_ZERO`, which calls
///    `hew_trap_with_code(HEW_TRAP_DIVIDE_BY_ZERO)`.
/// 3. The longjmp seam recovers, pushes a `CrashReport` with
///    `signal = HEW_TRAP_DIVIDE_BY_ZERO` (202).
/// 4. `snapshot_crashes_json()` serialises the report; the new `trap_kind`
///    field resolves to `"DivideByZero"` via
///    `ExitReason::from_error_code(202).trap_kind_name()`.
/// 5. `serde_json::from_str::<Vec<CrashEntry>>` round-trips the body through
///    the observe consumer — the same struct downstream visualisers parse —
///    and the row that matches the actor's id has `trap_kind == "DivideByZero"`
///    and `signal == 202`.
#[test]
fn divide_by_zero_trap_surfaces_trap_kind_on_observe_event_surface() {
    const STRATEGY_ONE_FOR_ONE: i32 = 0;
    const RESTART_PERMANENT: i32 = 0;
    const OVERFLOW_DROP_NEW: i32 = 1;

    let _guard = TEST_LOCK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    ensure_scheduler();
    hew_deterministic_reset();

    let sup = hew_runtime_testkit::TestSupervisor::new(STRATEGY_ONE_FOR_ONE, 5, 60);
    unsafe {
        hew_supervisor_set_restart_notify(sup.as_ptr());

        let mut state: i32 = 0;
        let name = std::ffi::CString::new("divz-child").unwrap();
        let spec = HewChildSpec {
            name: name.as_ptr(),
            init_state: (&raw mut state).cast(),
            init_state_size: std::mem::size_of::<i32>(),
            dispatch: Some(divide_by_zero_dispatch),
            restart_policy: RESTART_PERMANENT,
            mailbox_capacity: -1,
            overflow: OVERFLOW_DROP_NEW,
            arena_cap_bytes: 0,
            cycle_capable: 0,
            on_crash: None,
            lifecycle_fn: None,
        };
        assert_eq!(
            hew_supervisor_add_child_spec(sup.as_ptr(), &raw const spec),
            0,
            "add_child_spec must succeed"
        );
        assert_eq!(sup.start(), 0, "supervisor must start");

        let child = hew_supervisor_get_child_wait(sup.as_ptr(), 0, 5_000);
        assert!(!child.is_null(), "child actor must be spawned within 5s");
        let original_id = (*child).id;

        #[allow(
            clippy::cast_sign_loss,
            reason = "hew_crash_log_count returns i32; the i32 form is what wait_for_new_crash compares"
        )]
        let log_before: i32 = hew_crash_log_count();

        // Trigger the trap.
        hew_actor_send(child, MSG_DIV_BY_ZERO, std::ptr::null_mut(), 0);

        let (crashed_id, crash_signal) = wait_for_new_crash(log_before, Duration::from_secs(5))
            .expect("crash report must appear within 5s of the divide-by-zero trap");
        assert_eq!(
            crashed_id, original_id,
            "the newest crash report must belong to the actor we trapped"
        );
        assert_eq!(
            crash_signal, HEW_TRAP_DIVIDE_BY_ZERO,
            "CrashReport.signal must carry the canonical divide-by-zero trap code"
        );

        // Wait for the supervisor to drain the crash so RECENT_CRASHES has a
        // stable head when we snapshot. The restart cycle is a side-effect; we
        // do not assert on it here.
        let _ = hew_supervisor_wait_restart(sup.as_ptr(), 1, 5_000);

        // ── Observe event surface assertion ─────────────────────────────────
        // The profiler's /api/crashes endpoint returns exactly this body.
        let body = snapshot_crashes_json();
        let entries: Vec<ObserveCrashEntry> = serde_json::from_str(&body)
            .unwrap_or_else(|err| panic!("observe CrashEntry deserialise failed: {err}\n{body}"));

        let observed = entries
            .iter()
            .find(|entry| entry.actor_id == original_id)
            .unwrap_or_else(|| {
                panic!(
                    "observe surface must include a CrashEntry for the trapped actor {original_id}; body was: {body}"
                )
            });

        assert_eq!(
            observed.trap_kind, "DivideByZero",
            "trap_kind on the observe event surface must resolve the canonical \
             discriminator (got {:?}; body was: {body})",
            observed.trap_kind,
        );
        assert_eq!(
            observed.signal, HEW_TRAP_DIVIDE_BY_ZERO,
            "raw signal field on the observe event surface must preserve the trap code"
        );
        assert_eq!(
            observed.actor_id, original_id,
            "observe event must carry the crashed actor's id"
        );
        assert!(
            observed.time_s > 0.0,
            "observe event must carry a monotonic timestamp (got {})",
            observed.time_s,
        );
    }

    hew_deterministic_reset();
}
