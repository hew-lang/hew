//! End-to-end integration test: `#[max_heap(N)]` arena cap triggers
//! `ExitReason::HeapExceeded` supervisor exit and restart.
//!
//! Verifies slice A3 of the v0.5 failure-philosophy invariants (§5.17):
//!
//! 1. An actor spawned with a per-dispatch arena cap has that cap enforced
//!    when a message causes allocations that exceed the cap.
//! 2. The over-cap allocation triggers `try_direct_longjmp_with_code(200)` in
//!    `hew_arena_malloc` (arena.rs), routing through the per-worker longjmp
//!    crash seam with `HEW_TRAP_HEAP_EXCEEDED` (200) as the error code.
//! 3. The crash log records signal=200 for that crash (distinct from any POSIX
//!    signal number — the highest is typically 31 on Linux, 31 on macOS).
//! 4. `ExitReason::from_error_code(200)` maps to `ExitReason::HeapExceeded`.
//! 5. The supervisor detects the crash and completes a restart cycle.
//!
//! ## Arena cap setup
//!
//! A Hew-compiled `#[max_heap(N)]` actor uses `hew_actor_spawn_opts` (with
//! `arena_cap_bytes` set) as the spawn path. `arena_cap_bytes` is now carried
//! through `HewChildSpec` and `InternalChildSpec` so the supervisor restart
//! path applies it to every restarted actor. This test verifies both the
//! initial cap and that the cap is preserved across the restart cycle.
//!
//! WASM: `try_direct_longjmp_with_code` is a no-op on WASM; the signal-based
//! crash recovery seam is absent. This test is gated on
//! `#[cfg(not(target_arch = "wasm32"))]`. The WASM heap-exceeded path is tracked
//! as WASM-TODO(#1451) (existing parity umbrella).

#![cfg(not(target_arch = "wasm32"))]
#![allow(
    clippy::undocumented_unsafe_blocks,
    reason = "Integration test — supervisor child-management FFI and arena mutation are inherently raw"
)]

use std::ffi::c_void;
use std::sync::atomic::{AtomicI32, Ordering};
use std::sync::{Condvar, Mutex};
use std::time::{Duration, Instant};

use hew_runtime::actor::hew_actor_send;
use hew_runtime::arena::ActorArena;
use hew_runtime::crash::{hew_crash_log_count, hew_crash_log_last};
use hew_runtime::deterministic::hew_deterministic_reset;
use hew_runtime::supervisor::{
    hew_supervisor_add_child_spec, hew_supervisor_get_child_wait,
    hew_supervisor_set_restart_notify, hew_supervisor_wait_restart, ExitReason, HewChildSpec,
    HEW_TRAP_HEAP_EXCEEDED,
};
use hew_runtime_testkit::ensure_scheduler;

/// Global lock — guards shared mutable state (fault injection table, crash log)
/// used by tests in this process.
static TEST_LOCK: Mutex<()> = Mutex::new(());

// ── Dispatch signal ───────────────────────────────────────────────────────────

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

    fn record(&self) {
        let mut g = self.count.lock().unwrap();
        *g += 1;
        self.cond.notify_all();
    }

    fn wait_for(&self, target: i32, timeout: Duration) -> bool {
        let deadline = Instant::now() + timeout;
        let mut g = self.count.lock().unwrap();
        while *g < target {
            let rem = deadline.saturating_duration_since(Instant::now());
            if rem.is_zero() {
                return false;
            }
            let (new_g, res) = self.cond.wait_timeout(g, rem).unwrap();
            g = new_g;
            if res.timed_out() && *g < target {
                return false;
            }
        }
        true
    }
}

static DISPATCH_COUNT: AtomicI32 = AtomicI32::new(0);
static DISPATCH_SIGNAL: DispatchSignal = DispatchSignal::new();

/// Wait for the crash log to grow past `log_before`, then return the signal
/// number recorded in the newest crash report.
///
/// # Safety
///
/// `hew_crash_log_count` and `hew_crash_log_last` are lock-free reads from a
/// global ring buffer. Safe to call from any thread.
unsafe fn wait_for_crash_report_signal(log_before: i32, timeout: Duration) -> Option<i32> {
    let deadline = Instant::now() + timeout;
    loop {
        // SAFETY: lock-free read of the global crash ring buffer counter.
        let count = unsafe { hew_crash_log_count() };
        if count > log_before {
            // SAFETY: reads the most-recent CrashReport from the ring buffer.
            let report = unsafe { hew_crash_log_last() };
            return Some(report.signal);
        }
        if Instant::now() >= deadline {
            return None;
        }
        std::thread::sleep(Duration::from_millis(5));
    }
}

// ── Message type constants ────────────────────────────────────────────────────

/// Message type for a normal dispatch (count and signal).
const MSG_NORMAL: i32 = 1;
/// Message type for the over-cap allocation trigger.
const MSG_ALLOC_OVER_CAP: i32 = 2;

/// Cap applied to the actor's arena after spawn (in bytes).
///
/// Small enough that a single 512-byte allocation fills it, leaving the next
/// byte over cap. Arena overhead for alignment padding is zero on the first
/// alloc in a fresh chunk (the OS guarantees page-aligned base pointers), so
/// 512 is exact: the first alloc of 512 bytes succeeds; the next byte fails.
const ARENA_CAP_BYTES: usize = 512;

/// Number of bytes allocated by the trigger message — must exceed `ARENA_CAP_BYTES`.
const ALLOC_OVER_CAP_BYTES: usize = 1024;

/// Dispatch function used by the capped actor.
///
/// `MSG_NORMAL` → count + signal (proves the actor is live before the cap
/// trigger).
///
/// `MSG_ALLOC_OVER_CAP` → allocates `ALLOC_OVER_CAP_BYTES` from the actor's
/// arena. When the arena cap has been set to `ARENA_CAP_BYTES`, this call to
/// `hew_arena_malloc` returns null AND triggers
/// `try_direct_longjmp_with_code(HEW_TRAP_HEAP_EXCEEDED)`, unwinding through
/// the scheduler's sigsetjmp frame back to the supervisor seam.
unsafe extern "C-unwind" fn capped_dispatch(
    _ctx: *mut hew_runtime::execution_context::HewExecutionContext,
    _state: *mut c_void,
    msg_type: i32,
    _data: *mut c_void,
    _data_size: usize,
    _borrow_mode: i32,
) -> *mut c_void {
    match msg_type {
        MSG_NORMAL => {
            DISPATCH_COUNT.fetch_add(1, Ordering::SeqCst);
            DISPATCH_SIGNAL.record();
        }
        MSG_ALLOC_OVER_CAP => {
            // Allocate over the cap. When cap is set, hew_arena_malloc triggers
            // try_direct_longjmp_with_code(200) and does NOT return here.
            // SAFETY: called from a scheduler-dispatched actor context; the
            // arena is installed as the current thread-local arena.
            let _p = unsafe { hew_runtime::arena::hew_arena_malloc(ALLOC_OVER_CAP_BYTES) };
            // Unreachable when the cap fires. If the cap is NOT set (e.g. during
            // the restarted actor's dispatch), the alloc succeeds and we fall
            // through — that is acceptable; the test does not send this message
            // to the restarted actor.
        }
        _ => {}
    }
    std::ptr::null_mut()
}

// ── Tests ─────────────────────────────────────────────────────────────────────

/// Core assertion: allocating over the per-actor arena cap triggers
/// `ExitReason::HeapExceeded` and the supervisor restarts the child.
///
/// Steps:
/// 1. Supervisor spawns a child actor with `arena_cap_bytes = ARENA_CAP_BYTES` in its spec.
/// 2. Verify the initial actor's arena cap equals `ARENA_CAP_BYTES`.
/// 3. A normal message confirms the actor is live.
/// 4. A trigger message allocates `ALLOC_OVER_CAP_BYTES` > cap — the arena
///    invokes `try_direct_longjmp_with_code(200)`, unwinding through the
///    scheduler's sigsetjmp frame.
/// 5. Crash log records `signal == HEW_TRAP_HEAP_EXCEEDED (200)`.
/// 6. `ExitReason::from_error_code(200)` maps to `ExitReason::HeapExceeded`.
/// 7. Supervisor completes a restart cycle (restart count ≥ 1).
/// 8. Restarted actor's arena cap equals `ARENA_CAP_BYTES` (cap survives restart).
/// 9. The restarted child processes a normal message (process stays alive).
#[test]
#[allow(
    clippy::too_many_lines,
    reason = "end-to-end cap→crash→ExitReason::HeapExceeded→restart→resume cycle; each step is load-bearing"
)]
fn max_heap_actor_crash_routes_through_heap_exceeded_supervisor_exit() {
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

    let sup = hew_runtime_testkit::TestSupervisor::new(STRATEGY_ONE_FOR_ONE, 5, 60);
    // SAFETY: sup wraps a live supervisor; child-management FFI pointers remain
    // valid for the duration of this block.
    unsafe {
        hew_supervisor_set_restart_notify(sup.as_ptr());

        let mut state: i32 = 0;
        let name = std::ffi::CString::new("capped-child").unwrap();
        let spec = HewChildSpec {
            name: name.as_ptr(),
            init_state: (&raw mut state).cast(),
            init_state_size: std::mem::size_of::<i32>(),
            dispatch: Some(capped_dispatch),
            restart_policy: RESTART_PERMANENT,
            mailbox_capacity: -1,
            overflow: OVERFLOW_DROP_NEW,
            coalesce_key_fn: None,
            coalesce_fallback: OVERFLOW_DROP_NEW,
            arena_cap_bytes: ARENA_CAP_BYTES,
            cycle_capable: 0,
            on_crash: None,
            lifecycle_fn: None,
            init_fn: None,
            config: std::ptr::null_mut(),
            config_size: 0,
        };
        assert_eq!(
            hew_supervisor_add_child_spec(sup.as_ptr(), &raw const spec),
            0,
            "add_child_spec must succeed"
        );
        assert_eq!(sup.start(), 0, "supervisor must start");

        // Step 1: wait for the child to be spawned.
        let child = hew_supervisor_get_child_wait(sup.as_ptr(), 0, 5_000);
        assert!(!child.is_null(), "child actor must be spawned within 5s");
        let original_id = (*child).id;

        // Step 2: verify the arena cap was applied via the child spec.
        let arena: *mut ActorArena = (*child).arena;
        assert!(
            !arena.is_null(),
            "actor arena must be allocated by hew_actor_spawn_opts"
        );
        assert_eq!(
            (*arena).cap,
            ARENA_CAP_BYTES,
            "initial actor arena cap must equal ARENA_CAP_BYTES from child spec"
        );

        // Step 3: send a normal message — confirms the actor is live and
        // dispatching correctly before the cap trigger.
        hew_actor_send(child, MSG_NORMAL, std::ptr::null_mut(), 0);
        assert!(
            DISPATCH_SIGNAL.wait_for(1, Duration::from_secs(5)),
            "normal dispatch must fire at least once before cap trigger (count={})",
            DISPATCH_COUNT.load(Ordering::SeqCst)
        );

        // Record crash log position before triggering the over-cap alloc.
        // SAFETY: lock-free read.
        let log_before: i32 = hew_crash_log_count();

        // Step 4: send the trigger message — allocates over the cap.
        hew_actor_send(child, MSG_ALLOC_OVER_CAP, std::ptr::null_mut(), 0);

        // Step 5: wait for the crash report to appear in the global log.
        //
        // The crash log is populated in handle_crash_recovery_impl before the
        // supervisor is notified, so the report is always available before the
        // restart cycle begins.
        //
        // SAFETY: wait_for_crash_report_signal only reads global lock-free state.
        let crash_signal = wait_for_crash_report_signal(log_before, Duration::from_secs(5))
            .expect("crash report must appear in the global log within 5s after over-cap alloc");

        // Step 5 assertion: crash code must be HEW_TRAP_HEAP_EXCEEDED (200).
        assert_eq!(
            crash_signal, HEW_TRAP_HEAP_EXCEEDED,
            "over-cap crash must record HEW_TRAP_HEAP_EXCEEDED ({HEW_TRAP_HEAP_EXCEEDED}), \
             got signal {crash_signal}"
        );

        // Step 6: ExitReason mapping.
        //
        // This is the critical typed assertion: the signal code 200 maps to
        // ExitReason::HeapExceeded and NOT to ExitReason::Signal(200) or
        // ExitReason::Normal.
        let exit_reason = ExitReason::from_error_code(crash_signal);
        assert!(
            matches!(exit_reason, ExitReason::HeapExceeded),
            "signal {crash_signal} must map to ExitReason::HeapExceeded, got {exit_reason:?}"
        );

        // Step 7: supervisor restart cycle.
        let restart_count = hew_supervisor_wait_restart(sup.as_ptr(), 1, 5_000);
        assert!(
            restart_count >= 1,
            "supervisor must complete a restart cycle after the HeapExceeded crash \
             (got {restart_count})"
        );

        // Step 7 continued: the restarted child has a new identity.
        let restarted = hew_supervisor_get_child_wait(sup.as_ptr(), 0, 5_000);
        assert!(
            !restarted.is_null(),
            "restarted child must be available within 5s"
        );
        assert_ne!(
            (*restarted).id,
            original_id,
            "restarted actor must have a new identity (HeapExceeded must not resurrect \
             the crashed actor)"
        );

        // Step 8: verify the restarted actor retains the original arena cap.
        // The cap must be threaded from the child spec through the supervisor
        // restart path — this is the regression guard for the arena_cap_bytes
        // threading fix.
        let restarted_arena: *mut ActorArena = (*restarted).arena;
        assert!(
            !restarted_arena.is_null(),
            "restarted actor arena must be allocated"
        );
        assert_eq!(
            (*restarted_arena).cap,
            ARENA_CAP_BYTES,
            "restarted actor arena cap must equal ARENA_CAP_BYTES (cap must survive restart)"
        );

        // Step 9: the restarted actor processes messages normally.
        let pre = DISPATCH_COUNT.load(Ordering::SeqCst);
        hew_actor_send(restarted, MSG_NORMAL, std::ptr::null_mut(), 0);
        assert!(
            DISPATCH_SIGNAL.wait_for(pre + 1, Duration::from_secs(5)),
            "restarted actor must process messages normally after HeapExceeded crash \
             (pre={pre}, post={})",
            DISPATCH_COUNT.load(Ordering::SeqCst)
        );
    }

    hew_deterministic_reset();
}
