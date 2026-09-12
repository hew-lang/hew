//! Owned sleep operations on the runtime's shared timer wheel.

use crate::coro_state::CoroStatus;
use crate::timer_wheel::{
    hew_timer_wheel_remove, timer_wheel_schedule_at_handle, HewTimerHandle, HewTimerWheel,
};
use crate::wake::{HewWaker, OwnedWaker};
use std::ffi::c_void;
use std::sync::atomic::{AtomicI32, Ordering};
use std::sync::Arc;

#[derive(Debug)]
struct SleepReady {
    status: AtomicI32,
    waker: OwnedWaker,
}

/// The creator owns this timer handle. The callback owns a separate reference
/// to readiness, so cancellation can reclaim the creator before a late callback.
#[derive(Debug)]
pub struct HewCoroSleep {
    ready: Arc<SleepReady>,
    wheel: *mut HewTimerWheel,
    timer: HewTimerHandle,
}

unsafe extern "C" fn timer_ready(context: *mut c_void) {
    // SAFETY: timer registration transfers one Arc reference to this callback;
    // removal returns it to the cancelling owner only if it cannot fire.
    let ready = unsafe { Arc::from_raw(context.cast::<SleepReady>()) };
    if ready
        .status
        .compare_exchange(
            CoroStatus::Pending as i32,
            CoroStatus::Complete as i32,
            Ordering::AcqRel,
            Ordering::Acquire,
        )
        .is_ok()
    {
        ready.waker.wake();
    }
}

impl Drop for HewCoroSleep {
    fn drop(&mut self) {
        let _ = self.ready.status.compare_exchange(
            CoroStatus::Pending as i32,
            CoroStatus::Cancelled as i32,
            Ordering::AcqRel,
            Ordering::Acquire,
        );
        if !self.timer.entry.is_null() {
            // SAFETY: the runtime-owned wheel outlives operations. Removal
            // validates both pointer and generation without dereferencing a
            // stale entry, and transfers the payload only if firing cannot.
            let payload = unsafe {
                hew_timer_wheel_remove(self.wheel, self.timer.entry, self.timer.generation)
            };
            if !payload.is_null() {
                // SAFETY: removal transferred the callback's Arc reference.
                drop(unsafe { Arc::from_raw(payload.cast::<SleepReady>()) });
            }
        }
    }
}

unsafe fn start_on_wheel(
    duration_ns: i64,
    now_ms: u64,
    waker: &HewWaker,
    wheel: *mut HewTimerWheel,
) -> *mut HewCoroSleep {
    let ready = Arc::new(SleepReady {
        status: AtomicI32::new(if duration_ns <= 0 {
            CoroStatus::Complete as i32
        } else {
            CoroStatus::Pending as i32
        }),
        // SAFETY: the caller keeps the borrowed descriptor live for retain.
        waker: unsafe { OwnedWaker::retain(waker) },
    });
    let mut timer = HewTimerHandle::null();
    if duration_ns > 0 {
        if wheel.is_null() {
            ready
                .status
                .store(CoroStatus::Fault as i32, Ordering::Release);
        } else {
            let callback = Arc::into_raw(ready.clone()).cast_mut().cast();
            // The wheel cursor may lag behind the caller's clock while its
            // ticker is parked. Anchor the delay to the sampled clock, not
            // that cursor, so catching up cannot complete a new sleep early.
            let deadline_ms =
                now_ms.saturating_add(duration_ns.cast_unsigned().div_ceil(1_000_000));
            // SAFETY: the caller supplies a live wheel. The callback owns the
            // transferred Arc even if the timer fires before this call returns.
            timer = unsafe {
                timer_wheel_schedule_at_handle(wheel, deadline_ms, timer_ready, callback)
            };
            if timer.entry.is_null() {
                // SAFETY: failed registration did not accept the payload.
                drop(unsafe { Arc::from_raw(callback.cast::<SleepReady>()) });
                ready
                    .status
                    .store(CoroStatus::Fault as i32, Ordering::Release);
            }
        }
    }
    Box::into_raw(Box::new(HewCoroSleep {
        ready,
        wheel,
        timer,
    }))
}

/// Start a nonblocking sleep using the existing process-wide timer wheel.
///
/// # Safety
/// `waker` must point to a live descriptor obeying [`HewWaker`]'s contract.
/// The caller owns the returned operation until [`hew_coro_sleep_free`].
#[no_mangle]
pub unsafe extern "C" fn hew_coro_sleep_new(
    duration_ns: i64,
    waker: *const HewWaker,
) -> *mut HewCoroSleep {
    if waker.is_null() {
        return std::ptr::null_mut();
    }
    let wheel = if duration_ns > 0 {
        crate::timer_periodic::global_wheel()
    } else {
        std::ptr::null_mut()
    };
    // SAFETY: the clock has no preconditions, the wheel is runtime-owned and
    // the caller provides a valid descriptor.
    unsafe { start_on_wheel(duration_ns, crate::io_time::hew_now_ms(), &*waker, wheel) }
}

/// Start a nonblocking sleep that ends at a monotonic `instant`.
///
/// The remaining wait is measured here, against the same clock
/// `hew_instant_now` reads, so a deadline that has already passed completes
/// immediately rather than waiting a stale span.
///
/// # Safety
/// `waker` must point to a live descriptor obeying [`HewWaker`]'s contract.
/// The caller owns the returned operation until [`hew_coro_sleep_free`].
#[no_mangle]
pub unsafe extern "C" fn hew_coro_sleep_until_new(
    deadline_ns: i64,
    waker: *const HewWaker,
) -> *mut HewCoroSleep {
    if waker.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: hew_instant_now has no preconditions.
    let now_ns = unsafe { crate::io_time::hew_instant_now() };
    let remaining_ns = deadline_ns.saturating_sub(now_ns);
    let wheel = if remaining_ns > 0 {
        crate::timer_periodic::global_wheel()
    } else {
        std::ptr::null_mut()
    };
    // Keep the original sample: re-sampling after computing the remainder
    // would extend the deadline by any time spent preparing the operation.
    // hew_instant_now is a non-negative millisecond clock expressed in ns.
    let sampled_millis = now_ns.cast_unsigned() / 1_000_000;
    // SAFETY: the wheel is runtime-owned and the caller keeps its descriptor
    // live. A non-positive remainder completes without registering a timer.
    unsafe { start_on_wheel(remaining_ns, sampled_millis, &*waker, wheel) }
}

/// Poll sleep completion without blocking an actor worker.
///
/// # Safety
/// `sleep` must be a live operation owned by this caller.
#[no_mangle]
pub unsafe extern "C" fn hew_coro_sleep_status(sleep: *const HewCoroSleep) -> i32 {
    if sleep.is_null() {
        return CoroStatus::Fault as i32;
    }
    // SAFETY: caller keeps the operation live while polling.
    unsafe { &*sleep }.ready.status.load(Ordering::Acquire)
}

/// Release a completed sleep or cancel and detach a pending timer.
///
/// # Safety
/// `sleep` must be null or a uniquely owned sleep operation, released once.
#[no_mangle]
pub unsafe extern "C" fn hew_coro_sleep_free(sleep: *mut HewCoroSleep) {
    if !sleep.is_null() {
        // SAFETY: caller transfers its sole operation handle.
        drop(unsafe { Box::from_raw(sleep) });
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::timer_wheel::{
        hew_timer_wheel_free, hew_timer_wheel_new, timer_wheel_advance_cursor_for_test,
        timer_wheel_cursor_ms, timer_wheel_tick_to,
    };
    use crate::wake::blocking::Readiness;

    #[test]
    fn cancellation_removes_timer_and_releases_callback_without_waking() {
        let (ready, waker) = Readiness::new();
        // SAFETY: the test owns its wheel and drops all operations before it.
        unsafe {
            let wheel = hew_timer_wheel_new();
            let sleep = start_on_wheel(
                10_000_000,
                timer_wheel_cursor_ms(wheel),
                waker.descriptor(),
                wheel,
            );
            let status = (*sleep).ready.clone();
            hew_coro_sleep_free(sleep);
            timer_wheel_tick_to(wheel, timer_wheel_cursor_ms(wheel) + 20);
            assert_eq!(
                status.status.load(Ordering::Acquire),
                CoroStatus::Cancelled as i32
            );
            assert_eq!(Arc::strong_count(&status), 1);
            assert!(!ready.take_ready());
            hew_timer_wheel_free(wheel);
        }
    }

    #[test]
    fn timer_ready_before_wait_is_preserved_and_teardown_reclaims_references() {
        let (ready, waker) = Readiness::new();
        // SAFETY: the test owns its wheel and operation until explicit teardown.
        unsafe {
            let wheel = hew_timer_wheel_new();
            let sleep = start_on_wheel(1, timer_wheel_cursor_ms(wheel), waker.descriptor(), wheel);
            assert_eq!(hew_coro_sleep_status(sleep), CoroStatus::Pending as i32);
            timer_wheel_tick_to(wheel, timer_wheel_cursor_ms(wheel) + 2);
            assert!(ready.take_ready());
            assert_eq!(hew_coro_sleep_status(sleep), CoroStatus::Complete as i32);
            hew_coro_sleep_free(sleep);
            hew_timer_wheel_free(wheel);
        }
        assert_eq!(Arc::strong_count(&ready), 2);
    }

    #[test]
    fn sleep_waits_from_the_callers_clock_when_the_wheel_lags() {
        let (ready, waker) = Readiness::new();
        // SAFETY: the test exclusively owns the wheel and operation. Explicit
        // clock samples and ticks exercise a stale cursor without wall time.
        unsafe {
            let wheel = hew_timer_wheel_new();
            let now = timer_wheel_cursor_ms(wheel) + 100;
            let sleep = start_on_wheel(50_000_001, now, waker.descriptor(), wheel);
            timer_wheel_tick_to(wheel, now + 50);
            assert_eq!(hew_coro_sleep_status(sleep), CoroStatus::Pending as i32);
            assert!(!ready.take_ready());
            timer_wheel_tick_to(wheel, now + 51);
            assert_eq!(hew_coro_sleep_status(sleep), CoroStatus::Complete as i32);
            assert!(ready.take_ready());
            hew_coro_sleep_free(sleep);
            hew_timer_wheel_free(wheel);
        }
    }

    #[test]
    fn delayed_registration_does_not_extend_the_sampled_deadline() {
        let (ready, waker) = Readiness::new();
        // SAFETY: the test exclusively owns the wheel and operation. Advance
        // the wheel to model preemption after the caller sampled its clock.
        unsafe {
            let wheel = hew_timer_wheel_new();
            let sampled_now = timer_wheel_cursor_ms(wheel);
            timer_wheel_advance_cursor_for_test(wheel, 100);
            let sleep = start_on_wheel(50_000_000, sampled_now, waker.descriptor(), wheel);
            timer_wheel_tick_to(wheel, sampled_now + 100);
            assert_eq!(hew_coro_sleep_status(sleep), CoroStatus::Complete as i32);
            assert!(ready.take_ready());
            hew_coro_sleep_free(sleep);
            hew_timer_wheel_free(wheel);
        }
    }
}
