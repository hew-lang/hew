//! Readiness ownership for work driven by the runtime scheduler.

use std::sync::atomic::{AtomicU8, Ordering};
use std::sync::Arc;

const IDLE: u8 = 0;
const QUEUED: u8 = 1;
const RUNNING: u8 = 2;
const NOTIFIED: u8 = 3;
const COMPLETE: u8 = 4;

pub(crate) trait ReadyWork: Send + Sync {
    fn poll(self: Arc<Self>);
}

/// A wake during a poll is retained until that poll has relinquished its state.
#[derive(Debug)]
pub(crate) struct ReadyState(AtomicU8);

impl ReadyState {
    #[cfg(not(target_arch = "wasm32"))]
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
            let next = match phase {
                IDLE => QUEUED,
                RUNNING => NOTIFIED,
                QUEUED | NOTIFIED | COMPLETE => return false,
                _ => std::process::abort(),
            };
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

    /// Return whether a notification received during this poll needs a queue entry.
    pub(crate) fn finish(&self, complete: bool) -> bool {
        if complete {
            self.0.store(COMPLETE, Ordering::Release);
            false
        } else if self
            .0
            .compare_exchange(RUNNING, IDLE, Ordering::AcqRel, Ordering::Acquire)
            .is_err()
        {
            assert_eq!(self.0.swap(QUEUED, Ordering::AcqRel), NOTIFIED);
            true
        } else {
            false
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
