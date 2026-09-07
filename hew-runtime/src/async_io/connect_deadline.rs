//! A connection deadline remains independent of a blocked name resolver.

use std::ffi::c_void;
use std::sync::{Arc, Weak};
use std::time::Duration;

use super::{HewAsyncIo, IoFailure};
use crate::timer_wheel::{
    hew_timer_wheel_remove, hew_timer_wheel_schedule_handle, HewTimerHandle, HewTimerWheel,
};
use crate::util::MutexExt;

pub(super) struct ConnectDeadline {
    wheel: *mut HewTimerWheel,
    timer: HewTimerHandle,
}

// SAFETY: access is serialized by the operation's deadline mutex; the runtime
// keeps its wheel live until caller-owned operations have been released.
unsafe impl Send for ConnectDeadline {}

unsafe extern "C" fn expired(context: *mut c_void) {
    // SAFETY: firing or successful removal owns this Box, never both.
    let target = unsafe { Box::from_raw(context.cast::<Weak<HewAsyncIo>>()) };
    if let Some(operation) = target.upgrade() {
        operation.complete(Err(timeout_failure()));
    }
}

pub(super) fn timeout_failure() -> IoFailure {
    IoFailure::from_io(
        "connect TCP",
        &std::io::Error::from_raw_os_error(libc::ETIMEDOUT),
    )
}

impl Drop for ConnectDeadline {
    fn drop(&mut self) {
        // SAFETY: the runtime wheel outlives the caller's operation. Removal
        // checks generation and returns the payload only when firing cannot.
        let payload =
            unsafe { hew_timer_wheel_remove(self.wheel, self.timer.entry, self.timer.generation) };
        if !payload.is_null() {
            // SAFETY: successful removal transfers the callback's Box.
            drop(unsafe { Box::from_raw(payload.cast::<Weak<HewAsyncIo>>()) });
        }
    }
}

impl HewAsyncIo {
    /// Called once before producer admission; completion removes this timer.
    pub(super) fn set_connect_timeout(self: &Arc<Self>, duration: Duration) {
        if duration.is_zero() {
            self.complete(Err(timeout_failure()));
            return;
        }
        let wheel = crate::timer_periodic::global_wheel();
        if wheel.is_null() {
            self.complete(Err(IoFailure::invalid("connection timer is unavailable")));
            return;
        }
        // Hold registration through publication: an immediate callback may
        // complete the operation but cannot remove an unpublished handle.
        let mut deadline = self.connect_deadline.lock_or_recover();
        let payload = Box::into_raw(Box::new(Arc::downgrade(self)));
        let delay = u64::try_from(duration.as_millis())
            .unwrap_or(u64::MAX)
            .max(1);
        // SAFETY: wheel is live and the callback receives one owned weak target.
        let timer =
            unsafe { hew_timer_wheel_schedule_handle(wheel, delay, expired, payload.cast()) };
        if timer.entry.is_null() {
            // SAFETY: failed registration did not accept the payload.
            drop(unsafe { Box::from_raw(payload) });
            drop(deadline);
            self.complete(Err(IoFailure::invalid(
                "connection timer registration failed",
            )));
        } else {
            *deadline = Some(ConnectDeadline { wheel, timer });
        }
    }

    pub(super) fn clear_connect_deadline(&self) {
        let deadline = self.connect_deadline.lock_or_recover().take();
        drop(deadline);
    }
}
