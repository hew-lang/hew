//! Readiness ownership for work driven by the runtime scheduler.

use std::sync::atomic::{AtomicU8, Ordering};
use std::sync::Arc;

const IDLE: u8 = 0;
const QUEUED: u8 = 1;
const RUNNING: u8 = 2;
const NOTIFIED: u8 = 3;
const COMPLETE: u8 = 4;
const FROZEN: u8 = 8;

pub(crate) trait ReadyWork: Send + Sync {
    fn poll(self: Arc<Self>);
}

/// A wake during a poll is retained until that poll has relinquished its state.
#[derive(Debug)]
pub(crate) struct ReadyState(AtomicU8);

impl ReadyState {
    pub(crate) fn idle() -> Self {
        Self(AtomicU8::new(IDLE))
    }
    pub(crate) fn queued() -> Self {
        Self(AtomicU8::new(QUEUED))
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

pub(crate) fn enqueue(work: Arc<dyn ReadyWork>) {
    #[cfg(not(target_arch = "wasm32"))]
    if !crate::driver::active() {
        crate::scheduler::enqueue_work(work);
        return;
    }
    crate::driver::publish_work(work);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn frozen_task_keeps_notifications_without_exposing_a_running_frame() {
        let state = ReadyState::queued();
        state.begin();
        assert!(!state.freeze());
        assert!(!state.frozen_quiescent());
        assert!(!state.notify());
        assert!(!state.finish(false));
        assert!(state.frozen_quiescent());
        assert!(!state.try_begin());
        assert!(state.unfreeze());
        assert!(state.try_begin());
        assert!(!state.finish(true));
        assert!(!state.notify());
    }

    #[test]
    fn freezing_queued_task_preserves_its_one_pending_poll() {
        let state = ReadyState::queued();
        assert!(state.freeze());
        assert!(!state.try_begin());
        assert!(!state.notify());
        assert!(state.unfreeze());
        assert!(state.try_begin());
        assert!(!state.try_begin());
        assert!(!state.finish(false));
    }
}
