//! Coverage tests for `io_time`, `timer`, and `timer_wheel`.
//!
//! Duration helpers are pure functions — no global state needed.
//! Timer and timer-wheel tests use the simulated-time API so behaviour
//! is deterministic regardless of CI load.  Because simulated time uses
//! global atomics, tests that toggle it are serialised via `SIMTIME_LOCK`.

// Many tests deliberately exercise raw FFI functions that are inherently unsafe.
#![expect(
    clippy::undocumented_unsafe_blocks,
    reason = "FFI test harness — safety invariants are documented per-test"
)]

use std::ffi::c_void;
use std::ptr;
use std::sync::atomic::{AtomicI32, Ordering};
use std::sync::Mutex;

use hew_runtime::deterministic::{hew_simtime_advance_ms, hew_simtime_disable, hew_simtime_enable};
use hew_runtime::io_time::{hew_milliseconds, hew_now_ms, hew_seconds, hew_sleep_ms};
use hew_runtime::timer::{
    hew_timer_cancel, hew_timer_list_destroy, hew_timer_list_init, hew_timer_next_deadline_ms,
    hew_timer_schedule, hew_timer_tick, HewTimerList,
};
use hew_runtime::timer_wheel::{
    hew_timer_wheel_cancel, hew_timer_wheel_free, hew_timer_wheel_new,
    hew_timer_wheel_next_deadline_ms, hew_timer_wheel_schedule, hew_timer_wheel_tick,
};

// ── Shared helpers ──────────────────────────────────────────────────────

/// Global lock for tests that mutate simulated-time state.
static SIMTIME_LOCK: Mutex<()> = Mutex::new(());

/// Per-test fire counter (reset before each test that uses it).
static FIRE_COUNT: AtomicI32 = AtomicI32::new(0);

unsafe extern "C" fn counting_cb(_data: *mut c_void) {
    FIRE_COUNT.fetch_add(1, Ordering::SeqCst);
}

/// Callback that writes its `data` argument (interpreted as `*mut i32`)
/// to record which timer fired.
unsafe extern "C" fn tag_cb(data: *mut c_void) {
    let tag = data.cast::<AtomicI32>();
    unsafe { (*tag).fetch_add(1, Ordering::SeqCst) };
}

/// RAII guard that enables simulated time on construction and disables it
/// on drop, holding the global lock for the duration.
struct SimtimeGuard<'a> {
    _lock: std::sync::MutexGuard<'a, ()>,
}

// Statics declared before any test function to avoid `items_after_statements`.
static TAG_A: AtomicI32 = AtomicI32::new(0);
static TAG_B: AtomicI32 = AtomicI32::new(0);
static TAG_C: AtomicI32 = AtomicI32::new(0);
static TAG_L0: AtomicI32 = AtomicI32::new(0);
static TAG_L1: AtomicI32 = AtomicI32::new(0);
static TAG_OVF: AtomicI32 = AtomicI32::new(0);

impl SimtimeGuard<'_> {
    fn new(start_ms: i64) -> Self {
        let lock = SIMTIME_LOCK.lock().unwrap();
        hew_simtime_enable(start_ms);
        Self { _lock: lock }
    }
}

impl Drop for SimtimeGuard<'_> {
    fn drop(&mut self) {
        hew_simtime_disable();
    }
}

// ═══════════════════════════════════════════════════════════════════════
//  io_time — HewDuration and clock functions
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn duration_seconds_converts_correctly() {
    unsafe {
        let d = hew_seconds(3);
        assert_eq!(d.ms, 3000);
    }
}

#[test]
fn duration_seconds_zero() {
    unsafe {
        let d = hew_seconds(0);
        assert_eq!(d.ms, 0);
    }
}

#[test]
fn duration_milliseconds_converts_correctly() {
    unsafe {
        let d = hew_milliseconds(42);
        assert_eq!(d.ms, 42);
    }
}

#[test]
fn duration_milliseconds_zero() {
    unsafe {
        let d = hew_milliseconds(0);
        assert_eq!(d.ms, 0);
    }
}

#[test]
fn duration_seconds_large_value() {
    unsafe {
        // 86400 seconds = 1 day
        let d = hew_seconds(86400);
        assert_eq!(d.ms, 86_400_000);
    }
}

#[test]
fn now_ms_is_monotonic() {
    // Two consecutive calls should be non-decreasing.
    unsafe {
        let a = hew_now_ms();
        let b = hew_now_ms();
        assert!(b >= a, "hew_now_ms must be monotonically non-decreasing");
    }
}

#[test]
fn now_ms_respects_simulated_time() {
    let _guard = SimtimeGuard::new(1000);
    unsafe {
        assert_eq!(hew_now_ms(), 1000);
        hew_simtime_advance_ms(500);
        assert_eq!(hew_now_ms(), 1500);
    }
}

#[test]
fn sleep_ms_zero_returns_immediately() {
    // Should not block.
    unsafe {
        hew_sleep_ms(0);
    }
}

#[test]
fn sleep_ms_negative_returns_immediately() {
    // Negative values are guarded — should not block.
    unsafe {
        hew_sleep_ms(-1);
    }
}

#[test]
fn sleep_ms_short_duration() {
    let before = std::time::Instant::now();
    unsafe {
        hew_sleep_ms(10);
    }
    let elapsed = before.elapsed();
    assert!(
        elapsed.as_millis() >= 5,
        "hew_sleep_ms(10) should sleep for at least a few milliseconds"
    );
}

// ═══════════════════════════════════════════════════════════════════════
//  timer.rs — sorted linked-list timer
// ═══════════════════════════════════════════════════════════════════════

/// Helper: create an initialised `HewTimerList` on the stack.
fn make_timer_list() -> std::mem::MaybeUninit<HewTimerList> {
    let mut tl = std::mem::MaybeUninit::<HewTimerList>::uninit();
    unsafe {
        hew_timer_list_init(tl.as_mut_ptr());
    }
    tl
}

#[test]
fn timer_list_null_safety() {
    unsafe {
        // All functions should tolerate null without crashing.
        hew_timer_list_init(ptr::null_mut());
        hew_timer_list_destroy(ptr::null_mut());
        assert_eq!(hew_timer_tick(ptr::null_mut()), 0);
        assert_eq!(hew_timer_next_deadline_ms(ptr::null_mut()), -1);
        hew_timer_cancel(ptr::null_mut(), ptr::null_mut());
    }
}

#[test]
fn timer_list_multiple_timers_fire_in_order() {
    let _guard = SimtimeGuard::new(1000);

    TAG_A.store(0, Ordering::SeqCst);
    TAG_B.store(0, Ordering::SeqCst);
    TAG_C.store(0, Ordering::SeqCst);

    let mut tl = make_timer_list();
    unsafe {
        let tl = tl.as_mut_ptr();

        // Schedule three timers: 10ms, 20ms, 30ms.
        hew_timer_schedule(tl, 10, tag_cb, (&raw const TAG_A).cast_mut().cast());
        hew_timer_schedule(tl, 20, tag_cb, (&raw const TAG_B).cast_mut().cast());
        hew_timer_schedule(tl, 30, tag_cb, (&raw const TAG_C).cast_mut().cast());

        // Advance to t=1015 — only A should fire.
        hew_simtime_advance_ms(15);
        let fired = hew_timer_tick(tl);
        assert_eq!(fired, 1);
        assert_eq!(TAG_A.load(Ordering::SeqCst), 1);
        assert_eq!(TAG_B.load(Ordering::SeqCst), 0);

        // Advance to t=1025 — B fires.
        hew_simtime_advance_ms(10);
        let fired = hew_timer_tick(tl);
        assert_eq!(fired, 1);
        assert_eq!(TAG_B.load(Ordering::SeqCst), 1);
        assert_eq!(TAG_C.load(Ordering::SeqCst), 0);

        // Advance to t=1035 — C fires.
        hew_simtime_advance_ms(10);
        let fired = hew_timer_tick(tl);
        assert_eq!(fired, 1);
        assert_eq!(TAG_C.load(Ordering::SeqCst), 1);

        hew_timer_list_destroy(tl);
    }
}

#[test]
fn timer_list_cancel_middle_timer() {
    let _guard = SimtimeGuard::new(1000);
    FIRE_COUNT.store(0, Ordering::SeqCst);

    let mut tl = make_timer_list();
    unsafe {
        let tl = tl.as_mut_ptr();

        // Schedule three 0ms timers; cancel the second.
        hew_timer_schedule(tl, 0, counting_cb, ptr::null_mut());
        let middle = hew_timer_schedule(tl, 0, counting_cb, ptr::null_mut());
        hew_timer_schedule(tl, 0, counting_cb, ptr::null_mut());
        hew_timer_cancel(tl, middle);

        hew_simtime_advance_ms(1);
        let fired = hew_timer_tick(tl);
        assert_eq!(fired, 2, "cancelled timer must not fire");
        assert_eq!(FIRE_COUNT.load(Ordering::SeqCst), 2);

        hew_timer_list_destroy(tl);
    }
}

#[test]
fn timer_list_tick_with_no_expired_timers() {
    let _guard = SimtimeGuard::new(1000);
    FIRE_COUNT.store(0, Ordering::SeqCst);

    let mut tl = make_timer_list();
    unsafe {
        let tl = tl.as_mut_ptr();

        // Schedule a timer 100ms in the future, but don't advance time.
        hew_timer_schedule(tl, 100, counting_cb, ptr::null_mut());

        let fired = hew_timer_tick(tl);
        assert_eq!(fired, 0, "timer should not fire before its deadline");
        assert_eq!(FIRE_COUNT.load(Ordering::SeqCst), 0);

        hew_timer_list_destroy(tl);
    }
}

#[test]
fn timer_list_next_deadline_reports_remaining() {
    let _guard = SimtimeGuard::new(1000);

    let mut tl = make_timer_list();
    unsafe {
        let tl = tl.as_mut_ptr();

        hew_timer_schedule(tl, 50, counting_cb, ptr::null_mut());

        let remaining = hew_timer_next_deadline_ms(tl);
        // Should be approximately 50ms (the timer is 50ms in the future).
        assert!(
            remaining > 0 && remaining <= 50,
            "expected ~50ms remaining, got {remaining}"
        );

        // After time passes, remaining decreases.
        hew_simtime_advance_ms(30);
        let remaining2 = hew_timer_next_deadline_ms(tl);
        assert!(
            remaining2 < remaining,
            "remaining should decrease after time passes"
        );

        hew_timer_list_destroy(tl);
    }
}

#[test]
fn timer_list_next_deadline_returns_zero_when_overdue() {
    let _guard = SimtimeGuard::new(1000);

    let mut tl = make_timer_list();
    unsafe {
        let tl = tl.as_mut_ptr();

        hew_timer_schedule(tl, 10, counting_cb, ptr::null_mut());
        hew_simtime_advance_ms(100);

        let remaining = hew_timer_next_deadline_ms(tl);
        assert_eq!(remaining, 0, "overdue timer should report 0 remaining");

        hew_timer_list_destroy(tl);
    }
}

#[test]
fn timer_list_destroy_frees_pending_timers() {
    let _guard = SimtimeGuard::new(1000);
    FIRE_COUNT.store(0, Ordering::SeqCst);

    let mut tl = make_timer_list();
    unsafe {
        let tl = tl.as_mut_ptr();

        // Schedule timers but destroy the list without ticking — no leak.
        hew_timer_schedule(tl, 100, counting_cb, ptr::null_mut());
        hew_timer_schedule(tl, 200, counting_cb, ptr::null_mut());
        hew_timer_schedule(tl, 300, counting_cb, ptr::null_mut());
        hew_timer_list_destroy(tl);

        // If we get here without a crash or ASAN report, the memory was freed.
        assert_eq!(
            FIRE_COUNT.load(Ordering::SeqCst),
            0,
            "destroyed timers must not fire"
        );
    }
}

#[test]
fn timer_list_cancel_null_timer_is_safe() {
    let mut tl = make_timer_list();
    unsafe {
        let tl = tl.as_mut_ptr();
        // Cancelling a null timer on a valid list should not crash.
        hew_timer_cancel(tl, ptr::null_mut());
        hew_timer_list_destroy(tl);
    }
}

// ═══════════════════════════════════════════════════════════════════════
//  timer_wheel.rs — hierarchical timer wheel
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn wheel_l0_short_timer_fires() {
    // L0 handles timers < 256ms.
    let _guard = SimtimeGuard::new(1000);
    FIRE_COUNT.store(0, Ordering::SeqCst);

    unsafe {
        let tw = hew_timer_wheel_new();
        assert!(!tw.is_null());

        hew_timer_wheel_schedule(tw, 5, counting_cb, ptr::null_mut());

        // Advance past the deadline.
        hew_simtime_advance_ms(10);
        let fired = hew_timer_wheel_tick(tw);
        assert_eq!(fired, 1);
        assert_eq!(FIRE_COUNT.load(Ordering::SeqCst), 1);

        hew_timer_wheel_free(tw);
    }
}

#[test]
fn wheel_l1_medium_timer_fires() {
    // L1 handles timers 256ms–16384ms.
    let _guard = SimtimeGuard::new(1000);
    FIRE_COUNT.store(0, Ordering::SeqCst);

    unsafe {
        let tw = hew_timer_wheel_new();

        // 500ms delay → lands in L1.
        hew_timer_wheel_schedule(tw, 500, counting_cb, ptr::null_mut());

        // Advance 600ms — should cascade and fire.
        hew_simtime_advance_ms(600);
        let fired = hew_timer_wheel_tick(tw);
        assert_eq!(fired, 1);
        assert_eq!(FIRE_COUNT.load(Ordering::SeqCst), 1);

        hew_timer_wheel_free(tw);
    }
}

#[test]
fn wheel_overflow_long_timer_fires() {
    // Overflow handles timers > 16384ms.
    let _guard = SimtimeGuard::new(1000);
    FIRE_COUNT.store(0, Ordering::SeqCst);

    unsafe {
        let tw = hew_timer_wheel_new();

        // 20000ms delay → goes to overflow list.
        hew_timer_wheel_schedule(tw, 20_000, counting_cb, ptr::null_mut());

        // Advance past the deadline.
        hew_simtime_advance_ms(21_000);
        let fired = hew_timer_wheel_tick(tw);
        assert_eq!(fired, 1);
        assert_eq!(FIRE_COUNT.load(Ordering::SeqCst), 1);

        hew_timer_wheel_free(tw);
    }
}

#[test]
fn wheel_cancel_prevents_firing() {
    let _guard = SimtimeGuard::new(1000);
    FIRE_COUNT.store(0, Ordering::SeqCst);

    unsafe {
        let tw = hew_timer_wheel_new();
        let e = hew_timer_wheel_schedule(tw, 10, counting_cb, ptr::null_mut());
        hew_timer_wheel_cancel(tw, e);

        hew_simtime_advance_ms(20);
        let fired = hew_timer_wheel_tick(tw);
        assert_eq!(fired, 0);
        assert_eq!(FIRE_COUNT.load(Ordering::SeqCst), 0);

        hew_timer_wheel_free(tw);
    }
}

#[test]
fn wheel_multiple_timers_across_levels() {
    let _guard = SimtimeGuard::new(1000);

    TAG_L0.store(0, Ordering::SeqCst);
    TAG_L1.store(0, Ordering::SeqCst);
    TAG_OVF.store(0, Ordering::SeqCst);

    unsafe {
        let tw = hew_timer_wheel_new();

        // L0 timer: 10ms
        hew_timer_wheel_schedule(tw, 10, tag_cb, (&raw const TAG_L0).cast_mut().cast());
        // L1 timer: 500ms
        hew_timer_wheel_schedule(tw, 500, tag_cb, (&raw const TAG_L1).cast_mut().cast());
        // Overflow timer: 20000ms
        hew_timer_wheel_schedule(tw, 20_000, tag_cb, (&raw const TAG_OVF).cast_mut().cast());

        // Advance 15ms — only L0 fires.
        hew_simtime_advance_ms(15);
        hew_timer_wheel_tick(tw);
        assert_eq!(TAG_L0.load(Ordering::SeqCst), 1);
        assert_eq!(TAG_L1.load(Ordering::SeqCst), 0);
        assert_eq!(TAG_OVF.load(Ordering::SeqCst), 0);

        // Advance to 600ms total — L1 fires.
        hew_simtime_advance_ms(585);
        hew_timer_wheel_tick(tw);
        assert_eq!(TAG_L1.load(Ordering::SeqCst), 1);
        assert_eq!(TAG_OVF.load(Ordering::SeqCst), 0);

        // Advance to 21000ms total — overflow fires.
        hew_simtime_advance_ms(20_400);
        hew_timer_wheel_tick(tw);
        assert_eq!(TAG_OVF.load(Ordering::SeqCst), 1);

        hew_timer_wheel_free(tw);
    }
}

#[test]
fn wheel_next_deadline_empty() {
    let _guard = SimtimeGuard::new(1000);
    unsafe {
        let tw = hew_timer_wheel_new();
        assert_eq!(hew_timer_wheel_next_deadline_ms(tw), -1);
        hew_timer_wheel_free(tw);
    }
}

#[test]
fn wheel_next_deadline_reports_soonest() {
    let _guard = SimtimeGuard::new(1000);

    unsafe {
        let tw = hew_timer_wheel_new();

        hew_timer_wheel_schedule(tw, 100, counting_cb, ptr::null_mut());
        hew_timer_wheel_schedule(tw, 50, counting_cb, ptr::null_mut());
        hew_timer_wheel_schedule(tw, 200, counting_cb, ptr::null_mut());

        let next = hew_timer_wheel_next_deadline_ms(tw);
        // Should report the soonest timer (~50ms).
        assert!(
            next > 0 && next <= 50,
            "expected ~50ms for soonest timer, got {next}"
        );

        hew_timer_wheel_free(tw);
    }
}

#[test]
fn wheel_tick_no_expired() {
    let _guard = SimtimeGuard::new(1000);
    FIRE_COUNT.store(0, Ordering::SeqCst);

    unsafe {
        let tw = hew_timer_wheel_new();

        // Schedule a future timer but don't advance time.
        hew_timer_wheel_schedule(tw, 100, counting_cb, ptr::null_mut());
        let fired = hew_timer_wheel_tick(tw);
        assert_eq!(fired, 0);
        assert_eq!(FIRE_COUNT.load(Ordering::SeqCst), 0);

        hew_timer_wheel_free(tw);
    }
}

#[test]
fn wheel_free_cleans_up_pending() {
    let _guard = SimtimeGuard::new(1000);
    FIRE_COUNT.store(0, Ordering::SeqCst);

    unsafe {
        let tw = hew_timer_wheel_new();

        // Schedule across all levels, then free without ticking.
        hew_timer_wheel_schedule(tw, 5, counting_cb, ptr::null_mut());
        hew_timer_wheel_schedule(tw, 500, counting_cb, ptr::null_mut());
        hew_timer_wheel_schedule(tw, 20_000, counting_cb, ptr::null_mut());
        hew_timer_wheel_free(tw);

        // No crash or leak — callbacks never invoked.
        assert_eq!(FIRE_COUNT.load(Ordering::SeqCst), 0);
    }
}

#[test]
fn wheel_null_safety() {
    unsafe {
        assert!(
            hew_timer_wheel_schedule(ptr::null_mut(), 100, counting_cb, ptr::null_mut()).is_null()
        );
        hew_timer_wheel_cancel(ptr::null_mut(), ptr::null_mut());
        assert_eq!(hew_timer_wheel_tick(ptr::null_mut()), 0);
        assert_eq!(hew_timer_wheel_next_deadline_ms(ptr::null_mut()), -1);
        hew_timer_wheel_free(ptr::null_mut());
    }
}

#[test]
fn wheel_many_l0_timers_all_fire() {
    // Stress test: schedule 50 timers in L0 range and verify all fire.
    let _guard = SimtimeGuard::new(1000);
    FIRE_COUNT.store(0, Ordering::SeqCst);

    unsafe {
        let tw = hew_timer_wheel_new();

        for i in 1..=50 {
            hew_timer_wheel_schedule(tw, i, counting_cb, ptr::null_mut());
        }

        // Advance past all deadlines.
        hew_simtime_advance_ms(60);
        let fired = hew_timer_wheel_tick(tw);
        assert_eq!(fired, 50, "all 50 L0 timers should fire");
        assert_eq!(FIRE_COUNT.load(Ordering::SeqCst), 50);

        hew_timer_wheel_free(tw);
    }
}

#[test]
fn wheel_cancel_l1_timer() {
    let _guard = SimtimeGuard::new(1000);
    FIRE_COUNT.store(0, Ordering::SeqCst);

    unsafe {
        let tw = hew_timer_wheel_new();

        let e = hew_timer_wheel_schedule(tw, 500, counting_cb, ptr::null_mut());
        hew_timer_wheel_cancel(tw, e);

        hew_simtime_advance_ms(600);
        let fired = hew_timer_wheel_tick(tw);
        assert_eq!(fired, 0, "cancelled L1 timer must not fire");

        hew_timer_wheel_free(tw);
    }
}

#[test]
fn wheel_cancelled_timer_excluded_from_next_deadline() {
    let _guard = SimtimeGuard::new(1000);

    unsafe {
        let tw = hew_timer_wheel_new();

        let e = hew_timer_wheel_schedule(tw, 50, counting_cb, ptr::null_mut());
        hew_timer_wheel_schedule(tw, 200, counting_cb, ptr::null_mut());
        hew_timer_wheel_cancel(tw, e);

        let next = hew_timer_wheel_next_deadline_ms(tw);
        // With the 50ms timer cancelled, the next should be ~200ms.
        assert!(
            next > 50,
            "cancelled timer should be excluded; got {next}ms"
        );

        hew_timer_wheel_free(tw);
    }
}
