//! Integration test: overflow trap surfaces as actor crash via supervisor.
//!
//! Verifies slice C of the v0.5 failure-philosophy invariants (§5.17):
//! when actor code executes a hardware trap instruction (the same `llvm.trap`
//! that the B-2 overflow lowering emits for `+`/`-`/`*` overflow), the trap
//! is caught by the per-worker signal handler and routed through the
//! supervisor crash seam — NOT delivered as a process-wide `SIGABRT`.
//!
//! Platform notes:
//! - x86-64 (Linux + macOS): `llvm.trap` → `ud2` → SIGILL (signal 4).
//! - aarch64 macOS: `llvm.trap` → `brk #1` → SIGILL (signal 4).
//! - aarch64 Linux: `llvm.trap` → `brk #1` → SIGTRAP (signal 5).
//!
//! SIGTRAP is registered alongside SIGILL in `signal.rs::init_crash_handling`
//! precisely for the Linux aarch64 case. This test exercises whichever signal
//! is actually delivered by the OS.
//!
//! WASM: signal-based crash recovery is not available. The test is gated on
//! `any(unix, windows)`. A full WASM-side overflow-trap path is tracked as
//! WASM-TODO(#1451) (existing parity umbrella).

#![cfg(not(target_arch = "wasm32"))]
#![allow(
    clippy::undocumented_unsafe_blocks,
    reason = "Integration test — dispatch callbacks and supervisor FFI are inherently raw"
)]

use std::ffi::c_void;
use std::sync::atomic::{AtomicI32, Ordering};
use std::sync::{Condvar, Mutex};
use std::time::{Duration, Instant};

use hew_runtime::actor::hew_actor_send;
use hew_runtime::crash::{hew_crash_log_count, hew_crash_log_last};
use hew_runtime::deterministic::hew_deterministic_reset;
use hew_runtime::supervisor::{
    hew_supervisor_add_child_spec, hew_supervisor_get_child_wait,
    hew_supervisor_set_restart_notify, hew_supervisor_wait_restart, ExitReason, HewChildSpec,
};
use hew_runtime_testkit::ensure_scheduler;

/// Global lock — shares global state (fault injection table, crash log) with
/// other tests in the same process.
static TEST_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());

// ── Dispatch counter ─────────────────────────────────────────────────────────

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
/// The crash log is populated in `signal.rs::handle_crash_recovery_impl` before
/// the supervisor is notified, so the report is always available before any
/// restart cycle completes. There is no race between "report written" and
/// "actor freed."
///
/// # Safety
///
/// `hew_crash_log_count` and `hew_crash_log_last` are lock-free reads from
/// a global ring buffer. Safe to call from any thread.
unsafe fn wait_for_crash_report_signal(log_before: i32, timeout: Duration) -> Option<i32> {
    let deadline = Instant::now() + timeout;
    loop {
        // SAFETY: hew_crash_log_count is a lock-free read of the global crash
        // ring buffer counter; safe from any thread.
        let count = unsafe { hew_crash_log_count() };
        if count > log_before {
            // SAFETY: hew_crash_log_last reads the most-recent CrashReport
            // from the ring buffer; safe from any thread.
            let report = unsafe { hew_crash_log_last() };
            return Some(report.signal);
        }
        if Instant::now() >= deadline {
            return None;
        }
        std::thread::sleep(Duration::from_millis(5));
    }
}

/// Normal dispatch: counts and signals.
unsafe extern "C-unwind" fn counting_dispatch(
    _ctx: *mut hew_runtime::execution_context::HewExecutionContext,
    _state: *mut c_void,
    _msg_type: i32,
    _data: *mut c_void,
    _data_size: usize,
) {
    DISPATCH_COUNT.fetch_add(1, Ordering::SeqCst);
    DISPATCH_SIGNAL.record();
}

// ── Tests ─────────────────────────────────────────────────────────────────────

/// Core assertion: an overflow trap (real hardware trap instruction) surfaces
/// as an actor crash routed through the supervisor, NOT a process-wide abort.
///
/// Steps:
/// 1. Supervisor spawns a child actor.
/// 2. A normal message is delivered to confirm the actor is live.
/// 3. A trap message triggers `trapping_dispatch` — the actor executes a
///    real hardware trap, which the signal handler catches and routes through
///    the supervisor crash seam.
/// 4. Supervisor detects the crash and restarts the child.
/// 5. The restarted child processes further messages (process is alive).
/// 6. The crashed actor's `ExitReason` is `Signal(SIGILL)` or `Signal(SIGTRAP)`
///    depending on platform — either way, it is not `Normal` or `HeapExceeded`.
#[test]
#[allow(
    clippy::too_many_lines,
    reason = "end-to-end trap→crash→restart→resume cycle; each step is load-bearing"
)]
fn overflow_trap_surfaces_as_actor_crash_not_process_abort() {
    const STRATEGY_ONE_FOR_ONE: i32 = 0;
    const RESTART_PERMANENT: i32 = 0;
    const OVERFLOW_DROP_NEW: i32 = 1;
    const MSG_NORMAL: i32 = 1;
    const MSG_TRAP: i32 = 2;

    // Dispatch that selects behaviour based on message type:
    // MSG_NORMAL → count and signal; MSG_TRAP → execute hardware trap.
    unsafe extern "C-unwind" fn selectable_dispatch(
        _ctx: *mut hew_runtime::execution_context::HewExecutionContext,
        _state: *mut c_void,
        msg_type: i32,
        _data: *mut c_void,
        _data_size: usize,
    ) {
        match msg_type {
            MSG_NORMAL => {
                DISPATCH_COUNT.fetch_add(1, Ordering::SeqCst);
                DISPATCH_SIGNAL.record();
            }
            MSG_TRAP => {
                // Real hardware trap — identical to what llvm.trap lowers to.
                #[cfg(target_arch = "x86_64")]
                unsafe {
                    std::arch::asm!("ud2", options(noreturn));
                }
                #[cfg(target_arch = "aarch64")]
                unsafe {
                    std::arch::asm!("brk #1", options(noreturn));
                }
                // Unreachable on supported targets, but satisfies the compiler
                // on targets where neither cfg branch fires (e.g. WASM, which
                // is already excluded by #![cfg(not(target_arch = "wasm32"))]).
                #[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
                {
                    // Supported platforms are exhausted above. Panic to surface
                    // an unsupported-target error at test time.
                    panic!("overflow_trap test: unsupported target architecture");
                }
            }
            _ => {}
        }
    }

    let _guard = TEST_LOCK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    ensure_scheduler();
    hew_deterministic_reset();
    DISPATCH_COUNT.store(0, Ordering::SeqCst);
    DISPATCH_SIGNAL.reset();

    let sup = hew_runtime_testkit::TestSupervisor::new(STRATEGY_ONE_FOR_ONE, 5, 60);
    // SAFETY: sup wraps a live supervisor; all child-management FFI takes the
    // raw pointer directly. Pointers remain valid for the duration of this block.
    unsafe {
        hew_supervisor_set_restart_notify(sup.as_ptr());

        let mut state: i32 = 0;
        let name = std::ffi::CString::new("trap-child").unwrap();
        let spec = HewChildSpec {
            name: name.as_ptr(),
            init_state: (&raw mut state).cast(),
            init_state_size: std::mem::size_of::<i32>(),
            dispatch: Some(selectable_dispatch),
            restart_policy: RESTART_PERMANENT,
            mailbox_capacity: -1,
            overflow: OVERFLOW_DROP_NEW,
            arena_cap_bytes: 0,
        };
        assert_eq!(
            hew_supervisor_add_child_spec(sup.as_ptr(), &raw const spec),
            0,
            "add_child_spec must succeed"
        );
        assert_eq!(sup.start(), 0, "supervisor must start");

        // Step 1: wait for the child to be spawned and record its identity.
        let child = hew_supervisor_get_child_wait(sup.as_ptr(), 0, 5_000);
        assert!(!child.is_null(), "child actor must be spawned within 5s");
        let original_id = (*child).id;

        // Step 2: send a normal message — confirms the actor is live and
        // dispatch is wired before we exercise the trap path.
        hew_actor_send(child, MSG_NORMAL, std::ptr::null_mut(), 0);
        assert!(
            DISPATCH_SIGNAL.wait_for(1, Duration::from_secs(5)),
            "normal dispatch must fire at least once before trap (count={})",
            DISPATCH_COUNT.load(Ordering::SeqCst)
        );

        // Record crash log position before triggering the trap.
        // SAFETY: hew_crash_log_count is a lock-free global read.
        #[allow(
            clippy::cast_sign_loss,
            reason = "hew_crash_log_count returns i32; we store it as i32 for comparison"
        )]
        let log_before: i32 = hew_crash_log_count();

        // Step 3: send a message that triggers the real hardware trap.
        hew_actor_send(child, MSG_TRAP, std::ptr::null_mut(), 0);

        // Step 6: wait for the crash report to appear in the global log.
        //
        // The crash log is populated in handle_crash_recovery_impl before the
        // supervisor is notified, so the report is always available before the
        // restart cycle begins. There is no race between "report written" and
        // "actor freed."
        //
        // SAFETY: wait_for_crash_report_signal only reads global lock-free state.
        let crash_signal = wait_for_crash_report_signal(log_before, Duration::from_secs(5))
            .expect("crash report must appear in the global log within 5s after hardware trap");

        // Step 4: wait for the supervisor to complete the restart cycle.
        let restart_count = hew_supervisor_wait_restart(sup.as_ptr(), 1, 5_000);
        assert!(
            restart_count >= 1,
            "supervisor must complete a restart cycle after the trap (got {restart_count})"
        );

        // Step 5: the restarted child has a new identity — the original actor
        // was replaced, not recycled.
        let restarted = hew_supervisor_get_child_wait(sup.as_ptr(), 0, 5_000);
        assert!(
            !restarted.is_null(),
            "restarted child must be available within 5s"
        );
        assert_ne!(
            (*restarted).id,
            original_id,
            "restarted actor must have a new identity (trap must not resurrect the crashed actor)"
        );

        // Step 5 continued: the restarted actor processes messages normally,
        // proving the process is still alive and the supervisor is functional.
        let pre = DISPATCH_COUNT.load(Ordering::SeqCst);
        hew_actor_send(restarted, MSG_NORMAL, std::ptr::null_mut(), 0);
        assert!(
            DISPATCH_SIGNAL.wait_for(pre + 1, Duration::from_secs(5)),
            "restarted actor must process messages normally (pre={pre}, post={})",
            DISPATCH_COUNT.load(Ordering::SeqCst)
        );

        // Step 6 assertions: the crash signal must be SIGILL or SIGTRAP.
        // The crash log stores the raw signal number (not mapped through
        // ExitReason), so we compare directly.
        //
        //   - x86-64 (Linux + macOS): ud2       → SIGILL = 4
        //   - aarch64 macOS:          brk #1    → SIGILL = 4
        //   - aarch64 Linux:          brk #1    → SIGTRAP = 5
        let sigill: i32 = libc::SIGILL;
        let sigtrap: i32 = libc::SIGTRAP;
        assert!(
            crash_signal == sigill || crash_signal == sigtrap,
            "overflow trap must deliver SIGILL ({sigill}) or SIGTRAP ({sigtrap}), \
             got signal {crash_signal}"
        );

        // Confirm the crash was routed through ExitReason::Signal (not Normal
        // or HeapExceeded) by mapping through the same path the supervisor uses.
        let exit_reason = ExitReason::from_error_code(crash_signal);
        assert!(
            matches!(exit_reason, ExitReason::Signal(_)),
            "crash signal {crash_signal} must map to ExitReason::Signal, got {exit_reason:?}"
        );
    }

    hew_deterministic_reset();
}

/// Regression guard: the existing fault-injection path is NOT broken by the
/// addition of SIGTRAP to the registered crash signals.
///
/// This mirrors `supervised_actor_crash_and_restart` from `supervision_lifecycle.rs`
/// but runs here to confirm there is no interaction between SIGTRAP registration
/// and the existing longjmp-based fault injection.
#[test]
fn fault_inject_crash_still_works_after_sigtrap_registration() {
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
    // SAFETY: sup and child actor pointers are valid for the duration of
    // this block; all FFI calls follow the runtime's ownership contract.
    unsafe {
        hew_supervisor_set_restart_notify(sup.as_ptr());

        let mut state: i32 = 0;
        let name = std::ffi::CString::new("fault-inject-child").unwrap();
        let spec = HewChildSpec {
            name: name.as_ptr(),
            init_state: (&raw mut state).cast(),
            init_state_size: std::mem::size_of::<i32>(),
            dispatch: Some(counting_dispatch),
            restart_policy: RESTART_PERMANENT,
            mailbox_capacity: -1,
            overflow: OVERFLOW_DROP_NEW,
            arena_cap_bytes: 0,
        };
        assert_eq!(
            hew_supervisor_add_child_spec(sup.as_ptr(), &raw const spec),
            0
        );
        assert_eq!(sup.start(), 0);

        let child = hew_supervisor_get_child_wait(sup.as_ptr(), 0, 5_000);
        assert!(!child.is_null(), "child must be spawned");
        let original_id = (*child).id;

        // Normal message to confirm dispatch works.
        hew_actor_send(child, 1, std::ptr::null_mut(), 0);
        assert!(
            DISPATCH_SIGNAL.wait_for(1, Duration::from_secs(5)),
            "initial dispatch must fire"
        );

        // Fault injection — exercises the longjmp path, not the signal path.
        hew_runtime::deterministic::hew_fault_inject_crash(original_id, 1);
        hew_actor_send(child, 1, std::ptr::null_mut(), 0);

        let restart_count = hew_supervisor_wait_restart(sup.as_ptr(), 1, 5_000);
        assert!(
            restart_count >= 1,
            "supervisor must restart after fault-injected crash"
        );

        let restarted = hew_supervisor_get_child_wait(sup.as_ptr(), 0, 5_000);
        assert!(!restarted.is_null(), "restarted child must be available");
        assert_ne!(
            (*restarted).id,
            original_id,
            "restarted child must have a new identity"
        );

        let pre = DISPATCH_COUNT.load(Ordering::SeqCst);
        hew_actor_send(restarted, 1, std::ptr::null_mut(), 0);
        assert!(
            DISPATCH_SIGNAL.wait_for(pre + 1, Duration::from_secs(5)),
            "restarted actor must process messages after fault-inject path (pre={pre})"
        );
    }

    hew_deterministic_reset();
}
