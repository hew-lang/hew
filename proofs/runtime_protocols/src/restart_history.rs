use super::*;
fn unexpected_panic(_: std::fmt::Arguments<'_>) -> ! {
    assert!(false, "restart: unexpected panic");
    kani::assume(false);
    loop {}
}
#[kani::proof]
#[kani::stub(core::panicking::panic_fmt, unexpected_panic)]
fn restart_window_full_width_arithmetic() {
    let now: u64 = kani::any();
    let stamp: u64 = kani::any();
    let window: i32 = kani::any();
    let elapsed = if now >= stamp {
        u128::from(now) - u128::from(stamp)
    } else {
        (1u128 << 64) + u128::from(now) - u128::from(stamp)
    };
    let bound = if window >= 0 {
        window as u128 * 1000
    } else {
        0
    };
    assert_eq!(
        within_window(now, stamp, window),
        elapsed <= bound,
        "restart: elapsed-time arithmetic changed restart allowance"
    );
    kani::cover!(
        now < stamp && window > 0,
        "restart cover: monotonic clock wrap"
    );
    kani::cover!(
        now == stamp && window == 0,
        "restart cover: zero-duration window"
    );
}
#[kani::proof]
#[kani::unwind(42)]
#[kani::stub(core::panicking::panic_fmt, unexpected_panic)]
fn restart_budget_above_32_cannot_turn_into_unbounded_restart_storm() {
    let mut history = RestartHistory::default();
    for _ in 0..40 {
        history.record(100, 40);
    }
    assert_eq!(
        history.recent(101, 1),
        40,
        "restart: fixed history silently disabled configured restart limit"
    );
    history.record(1200, 40);
    assert_eq!(
        history.recent(1200, 1),
        1,
        "restart: expired budget did not recover"
    );
}
