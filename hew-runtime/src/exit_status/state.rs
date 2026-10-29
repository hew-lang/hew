use crate::runtime_sync::{AtomicBool, AtomicU64};
use std::sync::atomic::Ordering;
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
pub(super) const MAX_OPEN_RECORDS: usize = 64;

/// The record table and the terminal flag, as one value so the state machine's
/// transitions can be exercised without touching process-global state.
///
/// LOCK-FREE by requirement, not by preference: [`open_record`] is called from
/// `actor::hew_actor_trap_inner`, which runs on the language-panic unwind path.
/// Taking a mutex there can block behind a thread involved in crash handling.
/// A fixed table of atomic slots gives exactly-once settling with no lock on any
/// path. Hardware synchronous faults terminate the process and never enter this
/// recovery state machine.
#[derive(Debug)]
pub(crate) struct ExitStatusAuthority {
    /// A fault reached a state with no recovery authority left. Never lowered
    /// within a runtime lifetime.
    terminal: AtomicBool,
    /// Source of record ids. Starts at 1 so `0` stays the NONE sentinel.
    next_id: AtomicU64,
    /// Open records, one id per occupied slot; `0` is free.
    slots: [AtomicU64; MAX_OPEN_RECORDS],
    /// Crashes whose teardown has begun but whose record has not been opened
    /// yet. See [`CrashPublication`].
    publishing: AtomicU64,
}

impl ExitStatusAuthority {
    #[cfg(not(hew_fault_loom))]
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
            publishing: AtomicU64::new(0),
        }
    }

    #[cfg(hew_fault_loom)]
    pub(crate) fn new() -> Self {
        Self {
            terminal: AtomicBool::new(false),
            next_id: AtomicU64::new(1),
            slots: std::array::from_fn(|_| AtomicU64::new(0)),
            publishing: AtomicU64::new(0),
        }
    }

    pub(super) fn record_unrecovered(&self) {
        self.terminal.store(true, Ordering::Release);
    }

    pub(super) fn begin_publication(&self) {
        self.publishing.fetch_add(1, Ordering::AcqRel);
    }

    pub(super) fn end_publication(&self) {
        // Saturating: `reset` zeroes the counter for a fresh runtime, so a
        // guard that outlives a reset must not wrap it to `u64::MAX` and pin
        // the next run's status at "a crash is in flight" forever.
        let _ = self
            .publishing
            .fetch_update(Ordering::AcqRel, Ordering::Acquire, |count| {
                Some(count.saturating_sub(1))
            });
    }

    pub(super) fn publication_in_flight(&self) -> bool {
        self.publishing.load(Ordering::Acquire) != 0
    }

    pub(super) fn open_record(&self) -> FaultRecord {
        let Ok(id) = self
            .next_id
            .fetch_update(Ordering::AcqRel, Ordering::Acquire, |id| {
                (id != 0).then(|| id.checked_add(1)).flatten()
            })
        else {
            self.record_unrecovered();
            return FaultRecord::NONE;
        };
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
    pub(super) fn settle(&self, record: FaultRecord, ruling: FaultRuling) -> bool {
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

    /// Whether this exact record is still awaiting a ruling.
    #[cfg(test)]
    pub(super) fn is_open(&self, record: FaultRecord) -> bool {
        record.is_some()
            && self
                .slots
                .iter()
                .any(|slot| slot.load(Ordering::Acquire) == record.0)
    }

    pub(super) fn open_count(&self) -> usize {
        self.slots
            .iter()
            .filter(|slot| slot.load(Ordering::Acquire) != 0)
            .count()
    }

    pub(super) fn faulted(&self) -> bool {
        // Read the publication counter before scanning the table. Observing
        // its final decrement acquires the record installed before that edge.
        self.publication_in_flight()
            || self.terminal.load(Ordering::Acquire)
            || self.open_count() > 0
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub(super) fn unsettled(&self) -> bool {
        self.publication_in_flight() || self.open_count() > 0
    }

    pub(super) fn reset(&self) {
        self.terminal.store(false, Ordering::Release);
        for slot in &self.slots {
            slot.store(0, Ordering::Release);
        }
        self.publishing.store(0, Ordering::Release);
    }
}

#[cfg(all(hew_protocol_host, any(kani, hew_fault_loom)))]
#[path = "../../../proofs/runtime_protocols/src/fault_records.rs"]
pub(crate) mod formal;
