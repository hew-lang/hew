//! Production readiness gate checked against the Lean scheduling contract.
use std::sync::atomic::{AtomicU8, Ordering};

const IDLE: u8 = 0;
const QUEUED: u8 = 1;
const RUNNING: u8 = 2;
const NOTIFIED: u8 = 3;
const COMPLETE: u8 = 4;
const FROZEN: u8 = 8;

/// Primitive atomic interface; production and Loom execute the same CAS loop
/// and use the same orderings. Implementations must have atomic semantics.
pub(crate) trait AtomicByte: std::fmt::Debug {
    fn load(&self, order: Ordering) -> u8;
    fn fetch_or(&self, value: u8, order: Ordering) -> u8;
    fn fetch_and(&self, value: u8, order: Ordering) -> u8;
    fn compare_exchange(
        &self,
        old: u8,
        new: u8,
        success: Ordering,
        failure: Ordering,
    ) -> Result<u8, u8>;
    fn compare_exchange_weak(
        &self,
        old: u8,
        new: u8,
        success: Ordering,
        failure: Ordering,
    ) -> Result<u8, u8>;
}

impl AtomicByte for AtomicU8 {
    fn load(&self, order: Ordering) -> u8 {
        self.load(order)
    }
    fn fetch_or(&self, value: u8, order: Ordering) -> u8 {
        self.fetch_or(value, order)
    }
    fn fetch_and(&self, value: u8, order: Ordering) -> u8 {
        self.fetch_and(value, order)
    }
    fn compare_exchange(
        &self,
        old: u8,
        new: u8,
        success: Ordering,
        failure: Ordering,
    ) -> Result<u8, u8> {
        self.compare_exchange(old, new, success, failure)
    }
    fn compare_exchange_weak(
        &self,
        old: u8,
        new: u8,
        success: Ordering,
        failure: Ordering,
    ) -> Result<u8, u8> {
        self.compare_exchange_weak(old, new, success, failure)
    }
}

#[derive(Debug)]
pub(crate) struct ReadyState<A = AtomicU8>(A);

impl ReadyState {
    pub(crate) fn idle() -> Self {
        Self::from_atomic(AtomicU8::new(IDLE))
    }
    pub(crate) fn queued() -> Self {
        Self::from_atomic(AtomicU8::new(QUEUED))
    }
}

impl<A: AtomicByte> ReadyState<A> {
    pub(super) fn from_atomic(atomic: A) -> Self {
        Self(atomic)
    }

    /// Return whether this notification owns a new queue entry.
    pub(crate) fn notify(&self) -> bool {
        let mut phase = self.0.load(Ordering::Acquire);
        loop {
            let next = match phase & !FROZEN {
                IDLE => QUEUED,
                RUNNING => NOTIFIED,
                QUEUED | NOTIFIED | COMPLETE => return false,
                _ => std::process::abort(),
            } | (phase & FROZEN);
            match self
                .0
                .compare_exchange_weak(phase, next, Ordering::AcqRel, Ordering::Acquire)
            {
                Ok(_) => return next == QUEUED,
                Err(observed) => phase = observed,
            }
        }
    }

    pub(crate) fn begin(&self) {
        assert!(self.try_begin());
    }

    /// Claim a queued notification without polling idle or running work.
    pub(crate) fn try_begin(&self) -> bool {
        self.0
            .compare_exchange(QUEUED, RUNNING, Ordering::AcqRel, Ordering::Acquire)
            .is_ok()
    }

    /// Stop future polls and report whether the current poll has relinquished
    /// its frame. Notifications remain pending until the owner unfreezes it.
    pub(crate) fn freeze(&self) -> bool {
        let phase = self.0.fetch_or(FROZEN, Ordering::AcqRel) & !FROZEN;
        !matches!(phase, RUNNING | NOTIFIED)
    }

    /// Release a frozen scheduling gate. A queued notification needs one new
    /// queue entry; a stale pre-freeze entry may be discarded by `try_begin`.
    pub(crate) fn unfreeze(&self) -> bool {
        let phase = self.0.fetch_and(!FROZEN, Ordering::AcqRel);
        phase & FROZEN != 0 && phase & !FROZEN == QUEUED
    }

    pub(crate) fn frozen_quiescent(&self) -> bool {
        let phase = self.0.load(Ordering::Acquire);
        phase & FROZEN != 0 && !matches!(phase & !FROZEN, RUNNING | NOTIFIED)
    }

    /// Return whether a notification received during this poll needs a queue entry.
    pub(crate) fn finish(&self, complete: bool) -> bool {
        let mut phase = self.0.load(Ordering::Acquire);
        loop {
            let next = if complete {
                COMPLETE
            } else {
                match phase & !FROZEN {
                    RUNNING => IDLE,
                    NOTIFIED => QUEUED,
                    _ => std::process::abort(),
                }
            } | (phase & FROZEN);
            match self
                .0
                .compare_exchange_weak(phase, next, Ordering::AcqRel, Ordering::Acquire)
            {
                Ok(_) => return next == QUEUED,
                Err(observed) => phase = observed,
            }
        }
    }
}
