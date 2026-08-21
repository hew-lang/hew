//! Process exit-status authority for a Hew runtime program.
//!
//! ## Why this module exists
//!
//! An actor fault that no supervisor recovered must make the process exit
//! non-zero. That is a property of the RUN, not of the shutdown path taken to
//! end it, so it lives here as one authority every termination path consults —
//! `hew_shutdown_wait`, the native `main`-return epilogue, the synthetic cancel
//! return, and the `exit()` builtin alike.
//!
//! ## The fault-record state machine
//!
//! The rule this module exists to hold is that a crash is HANDLED only when a
//! supervisor's recovery took EFFECT — a child that is alive again, a ruling
//! that actually arrived. Not when one was intended: a non-null parent pointer,
//! a message that was sent, a timer that was requested. Every one of those is a
//! plan that can fail after it is made.
//!
//! One record per SUPERVISED actor crash, created at the crash site before the
//! supervisor is notified. An UNSUPERVISED crash creates no record — no
//! authority exists to rule on it, so it is [`Unrecovered`] immediately.
//!
//! A record carries an id, and the id travels on the messages that transfer its
//! ownership (`ChildEvent`, `ChildSupervisorEscalation`, `DelayedRestartEvent`),
//! so a ruling settles exactly the record it is about.
//!
//! ```text
//!                     ┌──────────────────────────────┐
//!                     │  Open(child, supervisor)     │  COUNTS AS FAILING
//!                     └──────────────┬───────────────┘
//!        ┌──────────────┬────────────┼─────────────┬──────────────────┐
//!        │              │            │             │                  │
//!   restart returns   timer      escalation    policy declines    ruling never
//!   a NON-NULL child  ARMED      ACCEPTED      (temporary /       arrives
//!        │            (admitted   by a live     breaker / Kill /       │
//!        │             + spawned) parent        no parent / spec       │
//!        │              │            │           retired)              │
//!        ▼              ▼            ▼             │                   │
//!    ┌────────┐   ┌────────────┐ ┌───────────┐     │                   │
//!    │Handled │   │ArmedFor    │ │Escalated  │     │              stays Open
//!    │        │   │Restart     │ │(to parent)│     │              (failing)
//!    └────────┘   └─────┬──────┘ └─────┬─────┘     │
//!    terminal-       PROVISIONAL    PROVISIONAL    │
//!     success        record stays   record stays   │
//!                    Open, owned    Open, owned    │
//!                    by the timer   by the parent  │
//!                          │              │        │
//!            fires: restart│      parent's│ruling  │
//!            effect rules  │      rules   │        │
//!                          └──────┬───────┘        │
//!                                 ▼                ▼
//!                          ┌─────────────────────────────┐
//!                          │        Unrecovered          │ terminal-failure
//!                          └─────────────────────────────┘
//! ```
//!
//! ### Rules
//!
//! 1. A record is settled by EXACTLY ONE ruling. [`FaultRuling::Escalated`] and
//!    [`FaultRuling::ArmedForRestart`] are explicitly NOT settles: they hand the
//!    SAME record to the next authority, which settles it.
//! 2. A provisional escalation the parent then RECOVERS clears the record.
//!    Nested budget exhaustion followed by a successful parent restart is not
//!    terminal — the subtree was recovered, which is what the tree is for.
//! 3. A transfer that FAILS settles `Unrecovered`, never `Handled`: a null
//!    parent actor, an un-representable child-supervisor index, a refused
//!    `send_system_message` to a stopped or closed parent, a refused timer
//!    admission, a failed timer thread spawn. Handing a record to an authority
//!    that never receives it is not recovery.
//! 4. A record whose ruling never arrives stays Open, and Open counts as
//!    failing. Shutdown quiesces (bounded) so a ruling that IS coming lands
//!    first; what remains open after that genuinely never came.
//! 5. `Unrecovered` is terminal and monotonic. A later `Handled` on a DIFFERENT
//!    record cannot retract it.
//!
//! ## The final exit code
//!
//! [`final_exit_code`] is the one place the status becomes a number:
//!
//! ```text
//! final = user_code            if user_code != 0
//!       = 1                    if any record is Open or Unrecovered
//!       = 0                    otherwise
//! ```
//!
//! A deliberate non-zero code the program chose is never overwritten — it is
//! already a failure and carries more information than `1`. A `0` never masks a
//! fault.

// WASM-TODO(actor-exit-status): report unrecovered actor faults in the wasm32
// program-exit status. This module is native-only (gated at `lib.rs`): its
// readers are `hew_shutdown_wait` and the native `main`-return epilogue, and
// wasm32 has neither — it exits through `hew_wasm_runtime_exit`, with no
// shutdown phases and no supervision tree (supervisors are HIR-gated off
// wasm32). A wasm32 actor fault is therefore observable in its trap output but
// not in the program's exit status.

use std::ffi::c_int;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};

/// A supervised crash's record id.
///
/// Rides the system-message payloads that transfer the record's ownership, so a
/// ruling names the exact crash it is about. [`FaultRecord::NONE`] is the
/// "no record" value carried by events that predate a record (a graceful child
/// stop) or by a test that drives a dispatch arm directly.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct FaultRecord(u64);

impl FaultRecord {
    /// No record. Settling it is a no-op.
    pub(crate) const NONE: Self = Self(0);

    /// Reconstruct a record id received over a message payload.
    pub(crate) const fn from_raw(raw: u64) -> Self {
        Self(raw)
    }

    /// The id to place in a message payload.
    pub(crate) const fn as_raw(self) -> u64 {
        self.0
    }

    /// Whether this names a real record.
    pub(crate) const fn is_some(self) -> bool {
        self.0 != 0
    }
}

/// How an authority ruled on one fault record.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum FaultRuling {
    /// A recovery took EFFECT: the failed child is alive again.
    Handled,
    /// Ownership moved to a live parent supervisor that accepted the
    /// escalation. PROVISIONAL — the parent's ruling settles the record.
    Escalated,
    /// Ownership moved to an armed restart timer. PROVISIONAL — the restart's
    /// effect when the timer fires settles the record.
    ArmedForRestart,
    /// No recovery authority remains, or a transfer to one failed.
    Unrecovered,
}

impl FaultRuling {
    /// Whether this ruling settles the record here, or hands it on.
    const fn settles(self) -> bool {
        matches!(self, Self::Handled | Self::Unrecovered)
    }
}

/// How many crash records may be open at once.
///
/// A record is open only while a crash awaits its supervisor's ruling, so this
/// bounds concurrent in-flight crashes, not crashes per run. Exhausting it is
/// itself a fault signal: the overflow path records terminal failure rather
/// than dropping the record.
const MAX_OPEN_RECORDS: usize = 64;

/// The record table and the terminal flag, as one value so the state machine's
/// transitions can be exercised without touching process-global state.
///
/// LOCK-FREE by requirement, not by preference: [`open_record`] is called from
/// `actor::hew_actor_trap_inner`, which runs on the crash path — including the
/// `siglongjmp` recovery of a SEGV/BUS/FPE/ILL. Taking a mutex there can block
/// behind a thread the crash just interrupted. A fixed table of atomic slots
/// gives exactly-once settling with no lock on any path.
#[derive(Debug)]
pub(crate) struct ExitStatusAuthority {
    /// A fault reached a state with no recovery authority left. Never lowered
    /// within a runtime lifetime.
    terminal: AtomicBool,
    /// Source of record ids. Starts at 1 so `0` stays the NONE sentinel.
    next_id: AtomicU64,
    /// Open records, one id per occupied slot; `0` is free.
    slots: [AtomicU64; MAX_OPEN_RECORDS],
}

impl ExitStatusAuthority {
    pub(crate) const fn new() -> Self {
        #[allow(
            clippy::declare_interior_mutable_const,
            reason = "array initialiser for atomics; each element is a distinct value"
        )]
        const FREE: AtomicU64 = AtomicU64::new(0);
        Self {
            terminal: AtomicBool::new(false),
            next_id: AtomicU64::new(1),
            slots: [FREE; MAX_OPEN_RECORDS],
        }
    }

    fn record_unrecovered(&self) {
        self.terminal.store(true, Ordering::Release);
    }

    fn open_record(&self) -> FaultRecord {
        let id = self.next_id.fetch_add(1, Ordering::AcqRel);
        for slot in &self.slots {
            if slot
                .compare_exchange(0, id, Ordering::AcqRel, Ordering::Acquire)
                .is_ok()
            {
                return FaultRecord(id);
            }
        }
        // The table is full: more crashes are in flight than the runtime can
        // track. Fail closed rather than lose a record.
        self.record_unrecovered();
        FaultRecord::NONE
    }

    /// Apply a ruling. Returns whether this call was the one that settled the
    /// record — false for a provisional ruling, and false for a double-settle.
    fn settle(&self, record: FaultRecord, ruling: FaultRuling) -> bool {
        if ruling == FaultRuling::Unrecovered {
            // Raise the flag even on a NONE record or a double-settle: the
            // ruling names a fault with no authority, which is the terminal
            // case regardless of whether a slot is still held.
            self.record_unrecovered();
        }
        if !record.is_some() || !ruling.settles() {
            // A provisional ruling leaves the record open for its new owner.
            return false;
        }
        for slot in &self.slots {
            if slot
                .compare_exchange(record.0, 0, Ordering::AcqRel, Ordering::Acquire)
                .is_ok()
            {
                return true;
            }
        }
        false
    }

    fn open_count(&self) -> usize {
        self.slots
            .iter()
            .filter(|slot| slot.load(Ordering::Acquire) != 0)
            .count()
    }

    fn faulted(&self) -> bool {
        self.terminal.load(Ordering::Acquire) || self.open_count() > 0
    }

    fn reset(&self) {
        self.terminal.store(false, Ordering::Release);
        for slot in &self.slots {
            slot.store(0, Ordering::Release);
        }
    }
}

/// The installed authority for this process.
static AUTHORITY: ExitStatusAuthority = ExitStatusAuthority::new();

/// Record a fault that no supervision authority can rule on.
///
/// Idempotent and monotonic within a runtime lifetime — the process exit status
/// only ever moves from success to failure.
pub(crate) fn record_unrecovered_actor_fault() {
    AUTHORITY.record_unrecovered();
}

/// Open a supervised crash's record, at the crash site, before its supervisor
/// is notified.
///
/// The returned id must reach a ruling. Until it does the fault counts as
/// failing, so a ruling that is never delivered — a stopping supervisor, a
/// closed mailbox, workers joined by an immediate shutdown — leaves the status
/// non-zero rather than silently successful.
pub(crate) fn open_supervised_fault() -> FaultRecord {
    AUTHORITY.open_record()
}

/// Apply an authority's ruling to one record.
pub(crate) fn settle_supervised_fault(record: FaultRecord, ruling: FaultRuling) {
    AUTHORITY.settle(record, ruling);
}

/// Whether any supervised crash is still awaiting a ruling.
///
/// Read by the scheduler's pre-teardown quiesce: workers must not be joined
/// while a queued ruling could still settle a record, or whether it ran would
/// decide the program's exit status.
pub(crate) fn has_open_supervised_faults() -> bool {
    AUTHORITY.open_count() > 0
}

/// True when this runtime carries a fault that was not recovered — either
/// terminally recorded, or still awaiting a ruling that may never come.
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

/// Clear the authority for a freshly initialized runtime, so a previous
/// runtime's fault cannot colour a new one.
pub fn reset_process_exit_status() {
    AUTHORITY.reset();
}

/// C ABI: the process exit status a Hew `main` must report.
///
/// `0` when every actor fault (if any) was recovered, `1` when at least one
/// went unrecovered or is still unsettled. Codegen calls this on every native
/// `main` return path, after that program's drain/shutdown epilogue has joined
/// the scheduler, so faults raised during the drain are included.
///
/// Safe to call on a runtime that was never initialized: no actor ever crashed,
/// so the answer is `0`.
#[no_mangle]
pub extern "C" fn hew_runtime_exit_status() -> c_int {
    c_int::from(unrecovered_actor_fault())
}

#[cfg(test)]
mod tests {
    use super::{exit_code_rule, ExitStatusAuthority, FaultRecord, FaultRuling};

    /// A supervised crash counts as failing from the moment it is opened: its
    /// supervisor may never rule, and an unsettled fault must not read as
    /// success.
    #[test]
    fn an_open_record_is_failing_until_a_ruling_settles_it() {
        let authority = ExitStatusAuthority::new();
        assert!(!authority.faulted());
        let record = authority.open_record();
        assert!(authority.faulted());
        assert!(authority.settle(record, FaultRuling::Handled));
        assert!(!authority.faulted());
    }

    /// Escalation is a TRANSFER, not a settle: the record stays open until the
    /// parent rules. Concluding "handled" from the send is the bug this pins.
    #[test]
    fn escalation_leaves_the_record_open_for_the_parent() {
        let authority = ExitStatusAuthority::new();
        let record = authority.open_record();
        assert!(!authority.settle(record, FaultRuling::Escalated));
        assert!(
            authority.faulted(),
            "an escalated record is provisional, not recovered"
        );
        // The parent recovers the subtree: the SAME record is cleared.
        assert!(authority.settle(record, FaultRuling::Handled));
        assert!(!authority.faulted());
    }

    /// Arming a restart timer is likewise a transfer: the timer's fire rules.
    #[test]
    fn arming_a_restart_timer_leaves_the_record_open_for_the_timer() {
        let authority = ExitStatusAuthority::new();
        let record = authority.open_record();
        assert!(!authority.settle(record, FaultRuling::ArmedForRestart));
        assert!(authority.faulted());
        assert!(authority.settle(record, FaultRuling::Unrecovered));
        assert!(authority.faulted(), "the fire found no child to restart");
    }

    /// A ruling settles exactly one record; a second ruling on the same record
    /// does not settle a sibling's.
    #[test]
    fn a_ruling_settles_exactly_one_record() {
        let authority = ExitStatusAuthority::new();
        let first = authority.open_record();
        let second = authority.open_record();
        assert!(authority.settle(first, FaultRuling::Handled));
        assert!(
            !authority.settle(first, FaultRuling::Handled),
            "a double-settle must not consume the sibling's record"
        );
        assert!(authority.faulted(), "the second record is still open");
        assert!(authority.settle(second, FaultRuling::Handled));
        assert!(!authority.faulted());
    }

    /// Two unrelated supervisors: one recovers its child, the other does not.
    /// The handled one must not clear the unrecovered one.
    #[test]
    fn one_handled_supervisor_does_not_clear_an_unrecovered_sibling() {
        let authority = ExitStatusAuthority::new();
        let handled = authority.open_record();
        let unrecovered = authority.open_record();
        authority.settle(unrecovered, FaultRuling::Unrecovered);
        authority.settle(handled, FaultRuling::Handled);
        assert!(authority.faulted());
    }

    /// `Unrecovered` is terminal: a later handled record cannot retract it.
    #[test]
    fn a_later_handled_record_cannot_retract_a_terminal_fault() {
        let authority = ExitStatusAuthority::new();
        authority.record_unrecovered();
        let record = authority.open_record();
        authority.settle(record, FaultRuling::Handled);
        assert!(authority.faulted());
    }

    /// A fault with no record and no authority — an unsupervised crash reported
    /// through the ruling path — is terminal, not silently dropped.
    #[test]
    fn an_unrecovered_ruling_without_a_record_still_raises_the_flag() {
        let authority = ExitStatusAuthority::new();
        authority.settle(FaultRecord::NONE, FaultRuling::Unrecovered);
        assert!(authority.faulted());
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
