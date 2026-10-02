//! An operation deadline is one timer-wheel entry, independent of whatever the
//! operation waits on: a blocked name resolver, a silent peer or a full send
//! buffer.

use std::ffi::c_void;
use std::sync::{Arc, Weak};
use std::time::Duration;

use super::{HewAsyncIo, IoFailure};
use crate::timer_wheel::{
    hew_timer_wheel_remove, timer_wheel_schedule_at_handle, HewTimerHandle, HewTimerWheel,
};
use crate::util::MutexExt;

pub(super) struct Deadline {
    wheel: *mut HewTimerWheel,
    timer: HewTimerHandle,
}

// SAFETY: access is serialized by the operation's deadline mutex; the runtime
// keeps its wheel live until caller-owned operations have been released.
unsafe impl Send for Deadline {}

/// The expiring operation and the failure it completes with.
struct Expiry {
    operation: Weak<HewAsyncIo>,
    failure: fn() -> IoFailure,
}

unsafe extern "C" fn expired(context: *mut c_void) {
    // SAFETY: firing or successful removal owns this Box, never both.
    let expiry = unsafe { Box::from_raw(context.cast::<Expiry>()) };
    if let Some(operation) = expiry.operation.upgrade() {
        operation.complete(Err((expiry.failure)()));
    }
}

pub(super) fn timed_out(operation: &str) -> IoFailure {
    IoFailure::from_io(
        operation,
        &std::io::Error::from_raw_os_error(crate::transport::etimedout_errno()),
    )
}

impl Drop for Deadline {
    fn drop(&mut self) {
        // SAFETY: the runtime wheel outlives the caller's operation. Removal
        // checks generation and returns the payload only when firing cannot.
        let payload =
            unsafe { hew_timer_wheel_remove(self.wheel, self.timer.entry, self.timer.generation) };
        if !payload.is_null() {
            // SAFETY: successful removal transfers the callback's Box.
            drop(unsafe { Box::from_raw(payload.cast::<Expiry>()) });
        }
    }
}

impl HewAsyncIo {
    /// Complete with `failure` after `duration` unless the operation finishes
    /// first. Replaces any earlier deadline; completion removes it.
    pub(super) fn set_deadline(self: &Arc<Self>, duration: Duration, failure: fn() -> IoFailure) {
        if duration.is_zero() {
            self.complete(Err(failure()));
            return;
        }
        let wheel = crate::timer_periodic::global_wheel();
        if wheel.is_null() {
            self.complete(Err(IoFailure::invalid("I/O timer is unavailable")));
            return;
        }
        // Hold registration through publication: an immediate callback may
        // complete the operation but cannot remove an unpublished handle.
        let mut deadline = self.deadline.lock_or_recover();
        let payload = Box::into_raw(Box::new(Expiry {
            operation: Arc::downgrade(self),
            failure,
        }));
        let delay = u64::try_from(duration.as_millis())
            .unwrap_or(u64::MAX)
            .max(1);
        // Anchor to the clock, not the wheel cursor, which rests at its last
        // tick while the reactor sleeps; a delay from it would fire early.
        // SAFETY: hew_now_ms has no preconditions on native targets.
        let deadline_ms = unsafe { crate::clock::hew_now_ms() }.saturating_add(delay);
        // SAFETY: wheel is live and the callback receives one owned payload.
        let timer =
            unsafe { timer_wheel_schedule_at_handle(wheel, deadline_ms, expired, payload.cast()) };
        if timer.entry.is_null() {
            // SAFETY: failed registration did not accept the payload.
            drop(unsafe { Box::from_raw(payload) });
            drop(deadline);
            self.complete(Err(IoFailure::invalid("I/O timer registration failed")));
        } else {
            let previous = deadline.replace(Deadline { wheel, timer });
            drop(deadline);
            drop(previous);
        }
    }

    pub(super) fn has_deadline(&self) -> bool {
        self.deadline.lock_or_recover().is_some()
    }

    pub(super) fn clear_deadline(&self) {
        let deadline = self.deadline.lock_or_recover().take();
        drop(deadline);
    }
}
