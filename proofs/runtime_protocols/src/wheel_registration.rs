//! Full production registration entry points, from arbitrary scalar state.
use super::*;
use std::sync::atomic::{AtomicU64, Ordering};

static CLOCK: AtomicU64 = AtomicU64::new(0);
unsafe extern "C" fn sampled_clock() -> u64 {
    CLOCK.load(Ordering::Relaxed)
}
unsafe extern "C" fn callback(_: *mut c_void) {}
fn unexpected_panic(_: std::fmt::Arguments<'_>) -> ! {
    assert!(false, "wheel: unexpected runtime panic");
    kani::assume(false);
    loop {}
}

unsafe fn in_expected_list(wheel: &WheelInner, deadline: u64, target: *mut HewTimerEntry) -> bool {
    let delta = u128::from(deadline) - u128::from(wheel.current_ms);
    let mut head = if delta < 256 {
        wheel.l0[(deadline % 256) as usize]
    } else if delta < 16_384 {
        wheel.l1[((deadline / 256) % 64) as usize]
    } else {
        wheel.overflow
    };
    for _ in 0..2 {
        if head.is_null() {
            return false;
        }
        if head == target {
            return true;
        }
        head = (*head).next;
    }
    false
}

#[kani::proof]
#[kani::unwind(3)]
#[kani::stub(crate::clock::hew_now_ms, sampled_clock)]
#[kani::stub(core::panicking::panic_fmt, unexpected_panic)]
fn wheel_registration_preserves_generation_and_deadline() {
    let generation: u64 = kani::any();
    let cursor: u64 = kani::any();
    let mut wheel = HewTimerWheel {
        inner: Mutex::new(WheelInner {
            l0: [ptr::null_mut(); L0_SIZE],
            l1: [ptr::null_mut(); L1_SIZE],
            overflow: ptr::null_mut(),
            current_ms: cursor,
            current_fraction_ns: 0,
            next_generation: generation,
        }),
    };
    macro_rules! register {
        ($before:expr) => {{
            let before: u64 = $before;
            let requested: u64 = kani::any();
            let now: u64 = kani::any();
            let relative: bool = kani::any();
            CLOCK.store(now, Ordering::Relaxed);
            let expected: u128 = if relative {
                u128::from(cursor.max(now)) + u128::from(requested)
            } else {
                u128::from(cursor.max(requested))
            };
            let accepted = before != 0 && expected < u128::from(u64::MAX);
            let handle = unsafe {
                if relative {
                    hew_timer_wheel_schedule_handle(
                        &mut wheel,
                        requested,
                        callback,
                        ptr::null_mut(),
                    )
                } else {
                    timer_wheel_schedule_at_handle(&mut wheel, requested, callback, ptr::null_mut())
                }
            };
            assert_eq!(
                !handle.entry.is_null(),
                accepted,
                "wheel: registration accepted an exhausted generation or invalid deadline"
            );
            let after = if accepted {
                if before == u64::MAX {
                    0
                } else {
                    before + 1
                }
            } else {
                before
            };
            let state = wheel.inner.lock().unwrap();
            assert_eq!(
                state.next_generation, after,
                "wheel: registration reused or consumed the wrong generation"
            );
            assert_eq!(
                state.current_ms, cursor,
                "wheel: registration moved the wheel cursor"
            );
            if accepted {
                assert_eq!(
                    handle.generation, before,
                    "wheel: handle generation disagrees with its reservation"
                );
                unsafe {
                    assert_eq!(
                        (*handle.entry).generation,
                        before,
                        "wheel: node generation disagrees with its handle"
                    );
                    assert_eq!(
                        u128::from((*handle.entry).deadline_ms),
                        expected,
                        "wheel: absolute deadline changed or overflowed"
                    );
                    assert!(
                        in_expected_list(&state, expected as u64, handle.entry),
                        "wheel: registered node went into the wrong bucket"
                    );
                }
            }
            (handle, after, expected)
        }};
    }
    let (first, next, first_deadline) = register!(generation);
    let (second, _, _) = register!(next);
    if !first.entry.is_null() && !second.entry.is_null() {
        assert_ne!(
            first.generation, second.generation,
            "wheel: two live handles share a generation"
        );
    }
    let state = wheel.inner.into_inner().unwrap();
    if !first.entry.is_null() {
        unsafe {
            assert!(
                in_expected_list(&state, first_deadline as u64, first.entry),
                "wheel: another registration lost an existing timer"
            );
        }
    }
    // Consume the wheel before reclaiming its at-most-two owned nodes. No
    // calendar scan is substituted for any operation under verification.
    unsafe {
        if !first.entry.is_null() {
            drop(Box::from_raw(first.entry));
        }
        if !second.entry.is_null() {
            drop(Box::from_raw(second.entry));
        }
    }
}
