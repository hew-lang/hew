//! Full-width deadline contracts checked against the production Rust file.
#![allow(dead_code, unexpected_cfgs)]

#[path = "../../hew-runtime/src/request_deadline.rs"]
mod request_deadline;
use request_deadline::RequestDeadline;

// Environmental boundary: one arbitrary, nonnegative clock reading per call.
// The system clock and execution-context pointer machinery are not reimplemented
// or verified here; every deadline operation above is the production source.
mod clock {
    use std::sync::atomic::{AtomicI64, Ordering};

    static NOW: AtomicI64 = AtomicI64::new(0);

    pub fn set(now: i64) {
        NOW.store(now, Ordering::Relaxed);
    }

    pub unsafe fn hew_instant_now() -> i64 {
        NOW.load(Ordering::Relaxed)
    }
}

mod execution_context {
    pub struct Context {
        pub request_deadline: super::RequestDeadline,
    }

    pub fn current_context() -> *const Context {
        std::ptr::null()
    }
}

// Embed the entire ABI domain in a wider ordered domain. Every negative raw
// value means absence, which is strictly later than the largest finite expiry.
fn bound(deadline: RequestDeadline) -> i128 {
    if deadline.raw() < 0 {
        i128::from(i64::MAX) + 1
    } else {
        i128::from(deadline.raw())
    }
}

#[kani::proof]
fn merging_never_extends_deadlines() {
    let a = RequestDeadline::from_raw(kani::any());
    let b = RequestDeadline::from_raw(kani::any());
    let c = RequestDeadline::from_raw(kani::any());
    let merged = a.earlier(b);

    assert_eq!(
        bound(merged),
        bound(a).min(bound(b)),
        "deadline: merge must choose the tightest bound"
    );
    assert!(
        bound(merged) <= bound(a),
        "deadline: merge extended the first bound"
    );
    assert!(
        bound(merged) <= bound(b),
        "deadline: merge extended the second bound"
    );
    assert_eq!(
        bound(a.earlier(b)),
        bound(b.earlier(a)),
        "deadline: merge order changed the bound"
    );
    assert_eq!(
        bound(a.earlier(b).earlier(c)),
        bound(a.earlier(b.earlier(c))),
        "deadline: regrouping nested scopes changed the bound"
    );
    assert_eq!(
        a.earlier(a).raw(),
        a.raw(),
        "deadline: merging the same bound changed it"
    );
    assert_eq!(
        RequestDeadline::default().raw(),
        -1,
        "deadline: default must use the absent ABI value"
    );
    assert_eq!(
        bound(a.earlier(RequestDeadline::default())),
        bound(a),
        "deadline: absence must not change a bound"
    );
}

#[kani::proof]
fn durations_cannot_overflow_into_unlimited_time() {
    let now: i64 = kani::any();
    let duration: i64 = kani::any();
    kani::assume(now >= 0);
    clock::set(now);

    let deadline = RequestDeadline::after(duration);
    // i128 is wide enough for every possible sum of two nonnegative i64s;
    // this specification cannot itself overflow at the production boundary.
    let sum = i128::from(now) + i128::from(duration).max(0);
    assert_eq!(
        i128::from(deadline.raw()),
        sum.min(i128::from(i64::MAX)),
        "deadline: duration must clamp at the largest representable instant"
    );
    assert!(
        deadline.raw() >= now,
        "deadline: duration wrapped into the past or absence"
    );
    if duration <= 0 {
        assert!(
            deadline.expired(),
            "deadline: nonpositive duration must expire immediately"
        );
    }
}

#[kani::proof]
fn expiry_is_exact_and_cannot_reverse_as_time_advances() {
    let raw: i64 = kani::any();
    let now: i64 = kani::any();
    let later: i64 = kani::any();
    kani::assume(now >= 0 && later >= now);
    let deadline = RequestDeadline::from_raw(raw);
    clock::set(now);

    let remaining = deadline.remaining();
    assert_eq!(
        remaining.is_some(),
        raw >= 0,
        "deadline: absence changed into a finite bound"
    );
    if let Some(remaining) = remaining {
        assert_eq!(
            i128::from(remaining),
            i128::from(raw) - i128::from(now),
            "deadline: remaining time is incorrect"
        );
    }
    let expired = deadline.expired();
    assert_eq!(
        expired,
        raw >= 0 && raw <= now,
        "deadline: expiry must include the exact boundary"
    );

    clock::set(later);
    assert_eq!(
        deadline.expired(),
        raw >= 0 && raw <= later,
        "deadline: expiry disagrees with the later clock"
    );
    assert!(
        !expired || deadline.expired(),
        "deadline: expired work became live again"
    );
    if let (Some(before), Some(after)) = (remaining, deadline.remaining()) {
        assert!(
            after <= before,
            "deadline: remaining time increased as time advanced"
        );
    }
}
