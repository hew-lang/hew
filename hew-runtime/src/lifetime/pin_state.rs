//! Supervisor admission, pin draining, and exclusive teardown authority.
use crate::runtime_sync::{AtomicBool, AtomicUsize};
use std::sync::atomic::Ordering;

pub(crate) const CLOSING: usize = 1usize << (usize::BITS - 1);
const PINS: usize = !CLOSING;

pub(crate) fn next_pin(state: usize) -> Option<usize> {
    if state & CLOSING != 0 || state & PINS == PINS {
        None
    } else {
        Some(state + 1)
    }
}

pub(crate) struct PinState {
    access: AtomicUsize,
    teardown: AtomicBool,
}

impl PinState {
    pub(crate) fn new() -> Self {
        Self {
            access: AtomicUsize::new(0),
            teardown: AtomicBool::new(false),
        }
    }
    pub(crate) fn try_pin(&self) -> bool {
        let mut state = self.access.load(Ordering::Acquire);
        loop {
            let Some(next) = next_pin(state) else {
                return false;
            };
            match self.access.compare_exchange_weak(
                state,
                next,
                Ordering::AcqRel,
                Ordering::Acquire,
            ) {
                Ok(_) => return true,
                Err(observed) => state = observed,
            }
        }
    }
    /// Return whether the final pin must notify a closing waiter.
    pub(crate) fn release_pin(&self) -> bool {
        let previous = self.access.fetch_sub(1, Ordering::AcqRel);
        debug_assert_ne!(previous & PINS, 0);
        previous & CLOSING != 0 && previous & PINS == 1
    }
    pub(crate) fn begin_close(&self) -> bool {
        self.access.fetch_or(CLOSING, Ordering::AcqRel) & CLOSING == 0
    }
    pub(crate) fn has_pins(&self) -> bool {
        self.access.load(Ordering::Acquire) & PINS != 0
    }
    pub(crate) fn is_closed_and_drained(&self) -> bool {
        self.access.load(Ordering::Acquire) == CLOSING
    }
    pub(crate) fn claim_teardown(&self) -> bool {
        self.teardown
            .compare_exchange(false, true, Ordering::AcqRel, Ordering::Acquire)
            .is_ok()
    }
    pub(crate) fn release_teardown(&self) {
        self.teardown.store(false, Ordering::Release);
    }
    #[cfg(test)]
    pub(crate) fn teardown_is_claimed(&self) -> bool {
        self.teardown.load(Ordering::Acquire)
    }
}

#[cfg(kani)]
#[path = "../../../proofs/runtime_protocols/src/pin_release.rs"]
mod formal_release;
