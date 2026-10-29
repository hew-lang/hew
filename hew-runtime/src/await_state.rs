//! One-shot await outcome and side-effect authority.
use crate::runtime_sync::AtomicI32;
use std::sync::atomic::Ordering;

/// Terminal state for an internal suspended await registration.
///
/// Values are ABI-visible to codegen/runtime adapters.
#[repr(i32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AwaitCancelStatus {
    Pending = 0,
    Completed = 1,
    Cancelled = 2,
    TimedOut = 3,
}

impl AwaitCancelStatus {
    pub(super) fn terminal_for(raw: i32) -> Self {
        match raw {
            1 => Self::Completed,
            2 => Self::Cancelled,
            3 => Self::TimedOut,
            _ => Self::Pending,
        }
    }
}

#[inline]
pub(super) fn cancellation_status(raw: i32) -> AwaitCancelStatus {
    match AwaitCancelStatus::terminal_for(raw) {
        AwaitCancelStatus::TimedOut => AwaitCancelStatus::TimedOut,
        _ => AwaitCancelStatus::Cancelled,
    }
}

#[derive(Debug)]
pub(super) struct Disposition {
    pub(super) cleanup: bool,
    pub(super) wake: bool,
}

#[inline]
pub(super) fn settle(
    state: &AtomicI32,
    status: AwaitCancelStatus,
    wake_actor: bool,
) -> Option<Disposition> {
    if state
        .compare_exchange(
            AwaitCancelStatus::Pending as i32,
            status as i32,
            Ordering::AcqRel,
            Ordering::Acquire,
        )
        .is_err()
    {
        return None;
    }
    let cleanup = matches!(
        status,
        AwaitCancelStatus::Cancelled | AwaitCancelStatus::TimedOut
    );
    Some(Disposition {
        cleanup,
        wake: cleanup && wake_actor,
    })
}
