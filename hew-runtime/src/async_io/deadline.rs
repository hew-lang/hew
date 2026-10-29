//! An operation deadline is one timer-wheel entry, independent of whatever the
//! operation waits on: a blocked name resolver, a silent peer or a full send
//! buffer.

use std::ffi::c_void;
use std::sync::{Arc, Weak};
use std::time::Duration;

use super::{HewAsyncIo, IoFailure};
use crate::timer_wheel::{
    hew_timer_wheel_remove, timer_wheel_schedule_precise_handle, HewTimerHandle, HewTimerWheel,
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
        let wheel = crate::timer_periodic::global_wheel();
        // SAFETY: the process monotonic clock has no preconditions.
        let now_ns = u64::try_from(unsafe { crate::clock::hew_instant_now() }).unwrap_or(0);
        self.set_deadline_on_wheel(duration, failure, now_ns, wheel);
    }

    fn set_deadline_on_wheel(
        self: &Arc<Self>,
        duration: Duration,
        failure: fn() -> IoFailure,
        now_ns: u64,
        wheel: *mut HewTimerWheel,
    ) {
        if duration.is_zero() {
            self.complete(Err(failure()));
            return;
        }
        if wheel.is_null() {
            self.complete(Err(IoFailure::invalid("I/O timer is unavailable")));
            return;
        }
        // Hold registration through publication: an immediate callback may
        // complete the operation but cannot remove an unpublished handle.
        let mut deadline = self.deadline.lock_or_recover();
        if !self.is_pending() {
            return;
        }
        let payload = Box::into_raw(Box::new(Expiry {
            operation: Arc::downgrade(self),
            failure,
        }));
        // Preserve both the start clock and the duration exactly.
        let Some(absolute) = crate::timer_deadline::after(now_ns, duration) else {
            // SAFETY: registration has not accepted this callback payload.
            drop(unsafe { Box::from_raw(payload) });
            drop(deadline);
            self.complete(Err(IoFailure::invalid("I/O deadline exceeds timer range")));
            return;
        };
        // SAFETY: wheel is live and the callback receives one owned payload.
        let timer = unsafe {
            timer_wheel_schedule_precise_handle(wheel, absolute, expired, payload.cast())
        };
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::async_io::AsyncIoStatus;
    use crate::timer_wheel::{
        hew_timer_wheel_free, hew_timer_wheel_new, timer_wheel_cursor_ms, timer_wheel_tick_to,
    };
    use crate::wake::blocking::Readiness;

    fn timeout() -> IoFailure {
        timed_out("deadline probe")
    }

    #[test]
    fn io_deadline_never_rounds_a_fractional_start_down() {
        let _runtime = crate::runtime_test_guard();
        let (ready, waker) = Readiness::new();
        // SAFETY: this fixture owns its operation and wheel until cleanup.
        for (offset, duration, _early, _final_tick) in [
            (900_000, 1_000_000, 1, 2),
            (0, 1_500_000, 1, 2),
            (999_999, 2, 1, 2),
            (0, 1, 0, 1),
        ] {
            // SAFETY: this iteration owns the operation, wheel and retained waker.
            unsafe {
                let wheel = hew_timer_wheel_new();
                let base = timer_wheel_cursor_ms(wheel);
                let operation = HewAsyncIo::new(waker.descriptor());
                operation.set_deadline_on_wheel(
                    Duration::from_nanos(duration),
                    timeout,
                    base * 1_000_000 + offset,
                    wheel,
                );
                let expiry = base * 1_000_000 + offset + duration;
                crate::timer_wheel::timer_wheel_tick_at(
                    wheel,
                    crate::timer_deadline::Deadline::from_nanos(expiry - 1),
                );
                let early_status = operation.state.lock_or_recover().status();
                let early_wake = ready.take_ready();
                crate::timer_wheel::timer_wheel_tick_at(
                    wheel,
                    crate::timer_deadline::Deadline::from_nanos(expiry),
                );
                let final_status = operation.state.lock_or_recover().status();
                drop(operation);
                hew_timer_wheel_free(wheel);
                assert_eq!(early_status, AsyncIoStatus::Pending);
                assert_eq!(final_status, AsyncIoStatus::Error);
                assert!(!early_wake);
                assert!(ready.take_ready());
            }
        }
    }

    #[test]
    fn replacement_removes_old_deadline_and_preserves_fractional_start() {
        let _runtime = crate::runtime_test_guard();
        let (ready, waker) = Readiness::new();
        // SAFETY: the fixture owns its operation and wheel through removal.
        unsafe {
            let wheel = hew_timer_wheel_new();
            let base = timer_wheel_cursor_ms(wheel);
            let now = base * 1_000_000 + 900_000;
            let operation = HewAsyncIo::new(waker.descriptor());
            operation.set_deadline_on_wheel(Duration::from_millis(1), timeout, now, wheel);
            operation.set_deadline_on_wheel(Duration::from_millis(3), timeout, now, wheel);
            for tick in [base + 2, base + 3] {
                timer_wheel_tick_to(wheel, tick);
                assert_eq!(
                    operation.state.lock_or_recover().status(),
                    AsyncIoStatus::Pending
                );
                assert!(!ready.take_ready());
            }
            timer_wheel_tick_to(wheel, base + 4);
            assert_eq!(
                operation.state.lock_or_recover().status(),
                AsyncIoStatus::Error
            );
            assert!(ready.take_ready());
            assert!(!operation.has_deadline());
            drop(operation);
            hew_timer_wheel_free(wheel);
        }
    }

    #[test]
    fn zero_duration_completes_and_retires_existing_deadline() {
        let _runtime = crate::runtime_test_guard();
        let (ready, waker) = Readiness::new();
        // SAFETY: the fixture owns its operation and wheel through removal.
        unsafe {
            let wheel = hew_timer_wheel_new();
            let base = timer_wheel_cursor_ms(wheel);
            let operation = HewAsyncIo::new(waker.descriptor());
            operation.set_deadline_on_wheel(
                Duration::from_millis(1),
                timeout,
                base * 1_000_000,
                wheel,
            );
            operation.set_deadline_on_wheel(Duration::ZERO, timeout, base * 1_000_000, wheel);
            assert_eq!(
                operation.state.lock_or_recover().status(),
                AsyncIoStatus::Error
            );
            assert!(ready.take_ready());
            assert!(!operation.has_deadline());
            timer_wheel_tick_to(wheel, base + 2);
            assert!(!ready.take_ready());
            drop(operation);
            hew_timer_wheel_free(wheel);
        }
    }

    #[test]
    fn unrepresentable_deadline_is_rejected_without_registering_a_timer() {
        let _runtime = crate::runtime_test_guard();
        let (ready, waker) = Readiness::new();
        // SAFETY: the fixture owns its operation and wheel through removal.
        unsafe {
            let wheel = hew_timer_wheel_new();
            let operation = HewAsyncIo::new(waker.descriptor());
            operation.set_deadline_on_wheel(Duration::MAX, timeout, 0, wheel);
            assert!(
                matches!(&*operation.state.lock_or_recover(), super::super::State::Ready(Err(error)) if error.message.contains("exceeds timer range"))
            );
            assert!(ready.take_ready());
            assert!(!operation.has_deadline());
            drop(operation);
            hew_timer_wheel_free(wheel);
        }
    }
}
