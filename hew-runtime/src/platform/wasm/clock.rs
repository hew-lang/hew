//! `wasm32` clock and cooperative driver-tick backend.

#[inline]
pub(crate) fn now_ms() -> u64 {
    // SAFETY: `hew_now_ms` reads the target monotonic/WASI clock without preconditions.
    unsafe { crate::clock::hew_now_ms() }
}

#[inline]
pub(crate) fn admit_periodic_tick() -> bool {
    crate::driver::guest_ingress_open()
}

#[inline]
pub(crate) fn shutdown_timer_reactor() {}

#[inline]
pub(crate) fn host_driver_work_in_flight() -> bool {
    false
}

#[inline]
pub(crate) fn notify_driver_host_published() {}

#[inline]
pub(crate) fn wait_driver_for_host<T>(
    _guard: std::sync::MutexGuard<'_, T>,
    _timeout: std::time::Duration,
) {
}
