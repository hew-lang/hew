//! Cancellation publication authority.
use crate::runtime_sync::AtomicI32;
use std::sync::atomic::Ordering;

#[repr(i32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum HewCancellationState {
    Active = 0,
    CancelRequested = 1,
    Observed = 2,
    Trapped = 3,
    Completed = 4,
}

impl HewCancellationState {
    pub(super) fn from_i32(raw: i32) -> Self {
        match raw {
            0 => Self::Active,
            1 => Self::CancelRequested,
            2 => Self::Observed,
            3 => Self::Trapped,
            4 => Self::Completed,
            _ => panic!("HewCancellationToken.state contained an invalid discriminant"),
        }
    }

    pub(super) fn is_requested(self) -> bool {
        matches!(self, Self::CancelRequested | Self::Observed | Self::Trapped)
    }
}

#[inline]
pub(super) fn request(state: &AtomicI32, reason: &AtomicI32, requested_reason: i32) -> bool {
    let requested_reason = if requested_reason == 0 {
        1
    } else {
        requested_reason
    };
    let _ = reason.compare_exchange(0, requested_reason, Ordering::AcqRel, Ordering::Acquire);
    state
        .compare_exchange(
            HewCancellationState::Active as i32,
            HewCancellationState::CancelRequested as i32,
            Ordering::AcqRel,
            Ordering::Acquire,
        )
        .is_ok()
}
