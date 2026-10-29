use crate::timer_deadline::after;

#[kani::proof]
fn deadline_always_represents_nanosecond_clock_and_delay() {
    let now: u64 = kani::any();
    let duration: u64 = kani::any();
    assert!(after(now, std::time::Duration::from_nanos(duration)).is_some());
    kani::cover!(duration == 0, "deadline cover: zero duration");
    kani::cover!(
        now == u64::MAX && duration == u64::MAX,
        "deadline cover: largest clock and delay"
    );
}
