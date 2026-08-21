//! Process exit-status authority for a Hew runtime program.
//!
//! ## Why this module exists
//!
//! An actor fault that no supervisor recovered must make the process exit
//! non-zero. Before this module the record of such a fault was a private
//! `static` in `actor`, read on exactly ONE shutdown path —
//! [`crate::shutdown::hew_shutdown_wait`], which codegen emits only for the
//! implicit actor-drain epilogue. A program containing a supervisor takes the
//! immediate `hew_sched_shutdown` epilogue instead, so an unrelated
//! UNSUPERVISED actor crashing in that program left the process exiting `0`:
//! the fault was recorded and never read.
//!
//! The exit status is therefore a runtime-level fact with ONE authority — this
//! flag — read on EVERY shutdown path. Codegen consults
//! [`hew_runtime_exit_status`] on every native `main` return regardless of
//! which drain/shutdown epilogue that program selected;
//! `hew_shutdown_wait`'s `-3` is the same authority surfaced through the
//! blocking-wait status word for embedders, not a second source of truth.
//!
//! ## The rule (`FAULTED` means: exit non-zero)
//!
//! The flag is SET when an actor fault reaches a point where no recovery
//! authority remains:
//!
//! * an actor CRASHES with no supervisor attached — nothing owns the recovery
//!   decision, so the fault is unrecovered by construction
//!   (`actor::actor_trap_terminal`); a top-level supervisor actor crashing is
//!   this case, since it has no supervisor of its own;
//! * a supervisor GIVES UP on a fault — restart budget exhausted, child not
//!   restartable, or the restart itself failed — and has no parent to escalate
//!   to, so the fault has reached the top of the supervision tree unrecovered
//!   (`supervisor::stop_and_maybe_escalate`).
//!
//! It is NOT set when a supervisor HANDLES the fault: a crash of a supervised
//! child whose supervisor restarts it (or escalates it to a parent that does)
//! is the supervision contract working, and leaves the flag clear. That —
//! supervised and handled — is the ONLY thing that keeps a crashed actor out
//! of the process exit status. Handling never CLEARS an already-set flag: a
//! successful restart of child B cannot retract the unrecovered fault of
//! unsupervised actor A. The single clear point is
//! [`reset_process_exit_status`], called when the scheduler initializes a
//! fresh runtime.
//!
//! Timing is irrelevant to the rule: a crash after shutdown has begun sets the
//! flag exactly like a crash mid-run, and every shutdown path reads the flag
//! after the drain/join it performs.

// WASM-TODO(actor-exit-status): report unrecovered actor faults in the wasm32
// program-exit status. This module is native-only (gated at `lib.rs`): its two
// readers are `hew_shutdown_wait` and the native `main`-return epilogue, and
// wasm32 has neither — it exits through `hew_wasm_runtime_exit`, with no
// shutdown phases and no supervision tree (supervisors are HIR-gated off
// wasm32). A wasm32 actor fault is therefore observable in its trap output but
// not in the program's exit status.

use std::ffi::c_int;
use std::sync::atomic::{AtomicBool, Ordering};

/// The one process exit-status fact: an actor fault reached a point with no
/// recovery authority left.
static UNRECOVERED_ACTOR_FAULT: AtomicBool = AtomicBool::new(false);

/// Record an actor fault that no supervision authority recovered.
///
/// Idempotent and monotonic within a runtime lifetime — the process exit
/// status only ever moves from success to failure.
pub(crate) fn record_unrecovered_actor_fault() {
    UNRECOVERED_ACTOR_FAULT.store(true, Ordering::Release);
}

/// True when an unrecovered actor fault was recorded for this runtime.
pub(crate) fn unrecovered_actor_fault() -> bool {
    UNRECOVERED_ACTOR_FAULT.load(Ordering::Acquire)
}

/// Clear the exit-status authority for a freshly initialized runtime.
///
/// Called from scheduler init (and by tests that drive the flag directly), so
/// a previous runtime's fault cannot colour a new one — notably in-process
/// test runs that install several runtimes in sequence.
pub fn reset_process_exit_status() {
    UNRECOVERED_ACTOR_FAULT.store(false, Ordering::Release);
}

/// C ABI: the process exit status a Hew `main` must report.
///
/// Returns `0` when every actor fault (if any) was handled by a supervisor,
/// and `1` when at least one fault went unrecovered. Codegen calls this on
/// every native `main` return path, after that program's drain/shutdown
/// epilogue has joined the scheduler, so faults raised during the drain are
/// included.
///
/// Safe to call on a runtime that was never initialized: no actor ever
/// crashed, so the answer is `0`.
#[no_mangle]
pub extern "C" fn hew_runtime_exit_status() -> c_int {
    c_int::from(unrecovered_actor_fault())
}
