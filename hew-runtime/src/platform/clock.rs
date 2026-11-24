//! Clock, timer-wheel, and driver-tick coordination seam.

use super::imp;

/// Read the current monotonic clock in milliseconds for timer-wheel ticks.
#[inline]
pub(crate) fn now_ms() -> u64 {
    imp::clock::now_ms()
}

/// Return whether periodic timers may admit a tick in the current run phase.
#[inline]
pub(crate) fn admit_periodic_tick() -> bool {
    imp::clock::admit_periodic_tick()
}

/// Release reactor timer resources during periodic-timer teardown.
#[inline]
pub(crate) fn shutdown_timer_reactor() {
    imp::clock::shutdown_timer_reactor();
}

/// Return whether host-side reactor work may still publish to the driver.
#[inline]
pub(crate) fn host_driver_work_in_flight() -> bool {
    imp::clock::host_driver_work_in_flight()
}

/// Wake a cooperative driver waiting for host-reactor publication.
#[inline]
pub(crate) fn notify_driver_host_published() {
    imp::clock::notify_driver_host_published();
}

/// Wait on host-reactor publication while holding the driver ready-list lock.
#[inline]
pub(crate) fn wait_driver_for_host<T>(
    guard: std::sync::MutexGuard<'_, T>,
    timeout: std::time::Duration,
) {
    imp::clock::wait_driver_for_host(guard, timeout);
}
