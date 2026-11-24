//! Native clock, timer reactor, and host-driver publication backend.

use std::sync::{Condvar, MutexGuard};
use std::time::Duration;

/// Signalled when a host reactor thread publishes while the driver waits.
static HOST_PUBLISHED: Condvar = Condvar::new();

#[inline]
pub(crate) fn now_ms() -> u64 {
    // SAFETY: `hew_now_ms` has no preconditions on native targets.
    unsafe { crate::clock::hew_now_ms() }
}

#[inline]
pub(crate) fn admit_periodic_tick() -> bool {
    true
}

#[inline]
pub(crate) fn shutdown_timer_reactor() {
    crate::reactor::reactor_shutdown();
}

#[inline]
pub(crate) fn host_driver_work_in_flight() -> bool {
    !crate::reactor::drain_is_idle()
}

#[inline]
pub(crate) fn notify_driver_host_published() {
    HOST_PUBLISHED.notify_one();
}

#[inline]
pub(crate) fn wait_driver_for_host<T>(guard: MutexGuard<'_, T>, timeout: Duration) {
    let _ = HOST_PUBLISHED.wait_timeout(guard, timeout);
}
