//! Integration test: a hardware overflow trap is process-fatal under RAII.
//!
//! Typed Hew failures unwind through generated cleanup edges and the scheduler
//! catch boundary. A synchronous hardware fault can interrupt an arbitrary
//! ownership operation, so the unwind-safe runtime deliberately handles it
//! with async-signal-safe diagnostic output and `_exit`; it never resumes the
//! interrupted worker or longjmps across live Rust frames.
//!
//! Platform notes:
//! - x86-64 (Linux + macOS): `llvm.trap` → `ud2` → SIGILL (signal 4).
//! - aarch64: `llvm.trap` → `brk #1` → SIGILL or SIGTRAP (signals 4/5),
//!   according to the host kernel/debug environment.
//!
//! SIGTRAP is registered alongside SIGILL in `signal.rs::init_crash_handling`.
//! This death test exercises whichever signal the platform delivers and proves
//! the fatal boundary without terminating the Cargo test process.
//!
//! WASM: signal-based crash recovery is not available. The test is gated on
//! `any(unix, windows)`. A full WASM-side overflow-trap path is tracked as
//! WASM-TODO(runtime-traps): implement WASM actor-trap recovery without native signals.

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
    _borrow_mode: i32,
) -> *mut c_void {
    DISPATCH_COUNT.fetch_add(1, Ordering::SeqCst);
    DISPATCH_SIGNAL.record();
    std::ptr::null_mut()
}

// ── Tests ─────────────────────────────────────────────────────────────────────

/// Core assertion: a real hardware trap terminates its subprocess through the
/// async-signal-safe fatal boundary.
///
/// Steps:
/// 1. Supervisor spawns a child actor.
/// 2. A normal message is delivered to confirm the actor is live.
/// 3. A trap message triggers `trapping_dispatch` — the actor executes a
///    real hardware trap, which the signal handler terminates with `_exit`.
///
/// The child continues through the historical restart assertions only if the
/// process-fatal handler fails to terminate it; that counterfactual exits zero,
/// which the parent rejects just as strongly as an unexpected signal/status.
// WINDOWS-TODO: install a VEH fatal boundary that emits the same stable
// diagnostic/status contract as the Unix async-signal-safe handler; until then
// the platform-default termination cannot satisfy this exact death-test oracle.
#[cfg_attr(windows, ignore)]
#[test]
#[allow(
    clippy::too_many_lines,
    reason = "the death test retains the old recovery path as counterfactual teeth"
)]
fn overflow_hardware_trap_is_process_fatal_under_unwind_safe_raii() {
    const CHILD_MARKER: &str = "HEW_OVERFLOW_HARDWARE_TRAP_CHILD";
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
        _borrow_mode: i32,
    ) -> *mut c_void {
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

        std::ptr::null_mut()
    }

    if std::env::var_os(CHILD_MARKER).is_none() {
        let executable = std::env::current_exe().expect("current test executable resolves");
        let output = std::process::Command::new(executable)
            .args([
                "overflow_hardware_trap_is_process_fatal_under_unwind_safe_raii",
                "--exact",
                "--nocapture",
            ])
            .env(CHILD_MARKER, "1")
            .output()
            .expect("hardware-trap child launches");
        assert!(
            matches!(output.status.code(), Some(132 | 133)),
            "hardware trap must exit as 128+SIGILL/SIGTRAP, got {:?}; stdout:\n{}\nstderr:\n{}",
            output.status,
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr),
        );
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            stderr.contains("hew: fatal synchronous hardware fault"),
            "fatal signal handler must publish its async-signal-safe diagnostic: {stderr}"
        );
        assert!(
            !stderr.contains("panicked at"),
            "hardware faults must not be misrouted through the typed panic hook: {stderr}"
        );
        return;
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
            coalesce_key_fn: None,
            coalesce_fallback: OVERFLOW_DROP_NEW,
            message_drop_fn: None,
            sys_dispatch: None,
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
        // The MSVC CRT does not define SIGTRAP; the runtime uses the canonical
        // POSIX number (5) as a raw signal id and Windows never delivers it for
        // a trap (EXCEPTION_ILLEGAL_INSTRUCTION maps to SIGILL=4). Use the
        // numeric value directly off-Unix so the assertion stays portable.
        #[cfg(unix)]
        let sigtrap: i32 = libc::SIGTRAP;
        #[cfg(not(unix))]
        let sigtrap: i32 = 5;
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
/// and the existing caught-unwind fault injection.
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
            coalesce_key_fn: None,
            coalesce_fallback: OVERFLOW_DROP_NEW,
            message_drop_fn: None,
            sys_dispatch: None,
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

        let child = hew_supervisor_get_child_wait(sup.as_ptr(), 0, 5_000);
        assert!(!child.is_null(), "child must be spawned");
        let original_id = (*child).id;

        // Normal message to confirm dispatch works.
        hew_actor_send(child, 1, std::ptr::null_mut(), 0);
        assert!(
            DISPATCH_SIGNAL.wait_for(1, Duration::from_secs(5)),
            "initial dispatch must fire"
        );

        // Fault injection — exercises the language-unwind path, not the signal path.
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
