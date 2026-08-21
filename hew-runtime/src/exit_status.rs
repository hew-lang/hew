//! Process exit-status authority for a Hew runtime program.
//!
//! ## Why this module exists
//!
//! An actor fault that no supervisor recovered must make the process exit
//! non-zero. That is a property of the RUN, not of the shutdown path taken to
//! end it, so it lives here as one authority every termination path consults —
//! `hew_shutdown_wait`, the native `main`-return epilogue, and the `exit()`
//! builtin alike. Reading it on only one of those (as an `actor`-private static
//! read solely by `hew_shutdown_wait` once did) makes the answer depend on
//! which epilogue a program's shape selected.
//!
//! ## The two-level record
//!
//! A crash's disposition is not always known at crash time. An UNSUPERVISED
//! actor's crash is settled immediately: nothing owns the recovery decision, so
//! it is unrecovered by construction. A SUPERVISED actor's crash is decided
//! later, on the supervisor's own dispatch, and that decision can be lost —
//! the supervisor may already be stopping, its mailbox may be closed, or an
//! immediate `hew_sched_shutdown` may join the workers before the queued
//! decision runs.
//!
//! So the authority holds two things:
//!
//! * [`OPEN_SUPERVISED_FAULTS`] — supervised crashes whose supervisor has not
//!   yet ruled. Each is opened at the crash site and must be settled exactly
//!   once by the supervisor's decision funnel.
//! * [`UNRECOVERED_ACTOR_FAULT`] — the terminal record. Set directly for a
//!   crash with no recovery authority, and by a settle whose outcome is
//!   [`SupervisedFaultOutcome::Unrecovered`].
//!
//! An OPEN fault counts as failing. A supervisor HANDLING the fault is the only
//! transition that may lower the status, and it lowers only its own open
//! record — it can never clear the terminal flag, so a later successful restart
//! of one child cannot retract an earlier unrecovered fault. A decision that is
//! never delivered therefore fails closed: the status stays non-zero because
//! nothing ever proved the fault was handled.
//!
//! ## The rule (`FAULTED` means: exit non-zero)
//!
//! The status is failing when an actor fault reached a point with no recovery
//! authority left:
//!
//! * an actor CRASHES with no supervisor attached, or with a supervisor
//!   back-pointer carrying no child index — nothing owns the recovery decision
//!   (`actor::hew_actor_trap_inner`); a top-level supervisor actor crashing is
//!   this case, since it has no supervisor of its own;
//! * a supervisor RULES that it cannot recover the fault — restart budget
//!   exhausted, child not restartable (`temporary`, or a tripped circuit
//!   breaker), the restart itself returned no child, an `#[on(crash)]` hook
//!   answering `Kill`, or `Escalate` with no parent to escalate to
//!   (`supervisor::decide_child_failure`, the single funnel every give-up path
//!   returns through);
//! * a supervised crash whose decision never arrives at all (the open record
//!   above).
//!
//! It is NOT failing when a supervisor HANDLES the fault: a restart now, a
//! scheduled restart, or an escalation to a parent that owns the decision. That
//! — supervised and handled — is the ONLY thing that keeps a crashed actor out
//! of the process exit status.
//!
//! Timing is irrelevant to the rule: a crash after shutdown has begun sets the
//! record exactly like a crash mid-run.
//!
//! ## The final exit code
//!
//! [`final_exit_code`] is the one place the status becomes a number:
//!
//! ```text
//! final = user_code            if user_code != 0
//!       = 1                    if an unrecovered fault was recorded
//!       = 0                    otherwise
//! ```
//!
//! A deliberate non-zero code the program chose is never overwritten — it is
//! already a failure and it carries more information than `1`. A `0` never
//! masks a fault. Every termination path routes through this: `hew_exit` (so
//! the `exit()` builtin cannot exit 0 over a recorded fault), and the native
//! `main` epilogue (so a returned non-zero code survives the fault report).

// WASM-TODO(actor-exit-status): report unrecovered actor faults in the wasm32
// program-exit status. This module is native-only (gated at `lib.rs`): its
// readers are `hew_shutdown_wait` and the native `main`-return epilogue, and
// wasm32 has neither — it exits through `hew_wasm_runtime_exit`, with no
// shutdown phases and no supervision tree (supervisors are HIR-gated off
// wasm32). A wasm32 actor fault is therefore observable in its trap output but
// not in the program's exit status.

use std::ffi::c_int;
use std::sync::atomic::{AtomicBool, AtomicI64, Ordering};

/// How a supervisor ruled on one supervised crash.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum SupervisedFaultOutcome {
    /// The supervisor owns the outcome: it restarted the child, scheduled a
    /// restart, or escalated to a parent that now owns the decision.
    Handled,
    /// No recovery authority remains for this fault.
    Unrecovered,
}

/// The two-level record described in the module docs, as one value so its
/// transitions can be exercised without touching process-global state.
#[derive(Debug)]
pub(crate) struct ExitStatusAuthority {
    /// An actor fault reached a point with no recovery authority left. Never
    /// lowered within a runtime lifetime.
    terminal: AtomicBool,
    /// Supervised crashes awaiting their supervisor's ruling. Non-zero means
    /// the process cannot yet claim success.
    open_supervised: AtomicI64,
}

impl ExitStatusAuthority {
    const fn new() -> Self {
        Self {
            terminal: AtomicBool::new(false),
            open_supervised: AtomicI64::new(0),
        }
    }

    fn record_unrecovered(&self) {
        self.terminal.store(true, Ordering::Release);
    }

    fn open_supervised(&self) {
        self.open_supervised.fetch_add(1, Ordering::AcqRel);
    }

    fn settle_supervised(&self, outcome: SupervisedFaultOutcome) {
        if outcome == SupervisedFaultOutcome::Unrecovered {
            self.record_unrecovered();
        }
        // Saturate at zero: a settle with nothing open means the accounting
        // drifted, and going negative would let that drift pre-pay for a later
        // crash's ruling.
        let _ = self
            .open_supervised
            .fetch_update(Ordering::AcqRel, Ordering::Acquire, |open| {
                (open > 0).then(|| open - 1)
            });
    }

    fn faulted(&self) -> bool {
        self.terminal.load(Ordering::Acquire) || self.open_supervised.load(Ordering::Acquire) > 0
    }

    fn open_count(&self) -> i64 {
        self.open_supervised.load(Ordering::Acquire)
    }

    fn reset(&self) {
        self.terminal.store(false, Ordering::Release);
        self.open_supervised.store(0, Ordering::Release);
    }
}

/// The installed authority for this process.
static AUTHORITY: ExitStatusAuthority = ExitStatusAuthority::new();

/// Record an actor fault that no supervision authority can recover.
///
/// Idempotent and monotonic within a runtime lifetime — the process exit status
/// only ever moves from success to failure.
pub(crate) fn record_unrecovered_actor_fault() {
    AUTHORITY.record_unrecovered();
}

/// Open a supervised crash's record, at the crash site, before its supervisor
/// is notified.
///
/// The paired [`settle_supervised_fault`] is the supervisor's ruling. Until it
/// arrives the fault counts as failing, so a ruling that is never delivered —
/// a stopping supervisor, a closed mailbox, workers joined by an immediate
/// shutdown — leaves the status non-zero rather than silently successful.
pub(crate) fn open_supervised_fault() {
    AUTHORITY.open_supervised();
}

/// Settle one open supervised crash with its supervisor's ruling.
pub(crate) fn settle_supervised_fault(outcome: SupervisedFaultOutcome) {
    AUTHORITY.settle_supervised(outcome);
}

/// True when this runtime carries an actor fault that was not recovered —
/// either terminally recorded, or still awaiting a ruling that may never come.
pub(crate) fn unrecovered_actor_fault() -> bool {
    AUTHORITY.faulted()
}

/// The exit-code rule, as a pure function of its two inputs. See the module
/// docs; this is the only place the two are combined.
pub(crate) const fn exit_code_rule(user_code: i64, faulted: bool) -> i64 {
    if user_code != 0 {
        user_code
    } else if faulted {
        1
    } else {
        0
    }
}

/// Apply [`exit_code_rule`] against the installed authority.
pub(crate) fn final_exit_code(user_code: i64) -> i64 {
    exit_code_rule(user_code, unrecovered_actor_fault())
}

/// Whether any supervised crash is still awaiting its supervisor's ruling.
///
/// Read by the scheduler's pre-teardown quiesce: workers must not be joined
/// while a queued supervisor decision could still settle a fault, or whether it
/// ran would decide the program's exit status. The provisional record is
/// reached only when a decision genuinely never comes.
pub(crate) fn has_open_supervised_faults() -> bool {
    AUTHORITY.open_count() > 0
}

/// Clear the exit-status authority for a freshly initialized runtime.
///
/// Called from scheduler init, so a previous runtime's fault cannot colour a
/// new one — notably in-process test runs that install several runtimes in
/// sequence.
pub fn reset_process_exit_status() {
    AUTHORITY.reset();
}

/// C ABI: the process exit status a Hew `main` must report.
///
/// Returns `0` when every actor fault (if any) was handled by a supervisor, and
/// `1` when at least one fault went unrecovered or is still unsettled. Codegen
/// calls this on every native `main` return path, after that program's
/// drain/shutdown epilogue has joined the scheduler, so faults raised during
/// the drain are included.
///
/// Safe to call on a runtime that was never initialized: no actor ever crashed,
/// so the answer is `0`.
#[no_mangle]
pub extern "C" fn hew_runtime_exit_status() -> c_int {
    c_int::from(unrecovered_actor_fault())
}

#[cfg(test)]
mod tests {
    use super::{exit_code_rule, ExitStatusAuthority, SupervisedFaultOutcome};

    /// A supervised crash counts as failing from the moment it is opened: its
    /// supervisor may never rule, and an unsettled fault must not read as
    /// success.
    #[test]
    fn open_supervised_fault_is_failing_until_settled() {
        let authority = ExitStatusAuthority::new();
        assert!(!authority.faulted());
        authority.open_supervised();
        assert!(authority.faulted());
        authority.settle_supervised(SupervisedFaultOutcome::Handled);
        assert!(!authority.faulted());
    }

    /// Handling is the only transition that may lower the status, and it lowers
    /// only its own open record — a later handled crash cannot retract an
    /// earlier unrecovered one.
    #[test]
    fn handled_settle_cannot_clear_an_earlier_unrecovered_fault() {
        let authority = ExitStatusAuthority::new();
        authority.record_unrecovered();
        authority.open_supervised();
        authority.settle_supervised(SupervisedFaultOutcome::Handled);
        assert!(authority.faulted());
    }

    /// An unrecovered ruling promotes the open record to the terminal one, so
    /// the status stays failing after the count returns to zero.
    #[test]
    fn unrecovered_settle_promotes_to_the_terminal_record() {
        let authority = ExitStatusAuthority::new();
        authority.open_supervised();
        authority.settle_supervised(SupervisedFaultOutcome::Unrecovered);
        assert!(authority.faulted());
    }

    /// Concurrent supervised crashes are counted, not collapsed: settling one
    /// must not clear the other.
    #[test]
    fn each_open_supervised_fault_needs_its_own_settle() {
        let authority = ExitStatusAuthority::new();
        authority.open_supervised();
        authority.open_supervised();
        authority.settle_supervised(SupervisedFaultOutcome::Handled);
        assert!(authority.faulted());
        authority.settle_supervised(SupervisedFaultOutcome::Handled);
        assert!(!authority.faulted());
    }

    /// A settle with nothing open saturates at zero rather than going negative,
    /// so accounting drift cannot pre-pay for a later crash's ruling.
    #[test]
    fn settle_without_an_open_record_does_not_go_negative() {
        let authority = ExitStatusAuthority::new();
        authority.settle_supervised(SupervisedFaultOutcome::Handled);
        authority.open_supervised();
        assert!(authority.faulted());
    }

    /// The scheduler's pre-teardown quiesce reads this: it must report an open
    /// fault until the ruling lands, so workers are not joined over a decision
    /// that would have settled it.
    #[test]
    fn open_faults_are_visible_to_the_shutdown_quiesce() {
        let authority = ExitStatusAuthority::new();
        assert_eq!(authority.open_count(), 0);
        authority.open_supervised();
        assert_eq!(authority.open_count(), 1);
        authority.settle_supervised(SupervisedFaultOutcome::Handled);
        assert_eq!(authority.open_count(), 0);
    }

    /// The shipped rule: a deliberate non-zero user code always wins, a zero
    /// user code never masks a fault.
    #[test]
    fn exit_code_rule_keeps_user_codes_and_reports_faults() {
        assert_eq!(exit_code_rule(0, false), 0);
        assert_eq!(exit_code_rule(0, true), 1);
        assert_eq!(exit_code_rule(7, false), 7);
        assert_eq!(exit_code_rule(7, true), 7);
    }
}
