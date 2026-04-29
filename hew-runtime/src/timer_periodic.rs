//! Periodic timer support for Hew actors.
//!
//! Provides `hew_actor_schedule_periodic` which schedules a repeating
//! self-send to an actor at a fixed interval using the runtime's timer wheel.

use std::collections::HashMap;
use std::ffi::c_void;
use std::ptr;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex, OnceLock};
use std::thread::JoinHandle;

use crate::lifetime::PoisonSafe;

use crate::actor::{hew_actor_send, HewActor};
use crate::timer_wheel::{
    hew_timer_wheel_free, hew_timer_wheel_new, hew_timer_wheel_schedule, hew_timer_wheel_tick,
    HewTimerWheel,
};

// ---------------------------------------------------------------------------
// Global timer wheel (lazy-initialised singleton)
// ---------------------------------------------------------------------------

/// Wrapper to allow a raw timer-wheel pointer in a static Mutex.
struct WheelSlot(*mut HewTimerWheel);

// SAFETY: Access is guarded by the Mutex; the pointer is only dereferenced
// through the timer-wheel C ABI which uses its own internal Mutex.
unsafe impl Send for WheelSlot {}
// SAFETY: Only accessed under the GLOBAL_WHEEL Mutex; no unsynchronised sharing.
unsafe impl Sync for WheelSlot {}

static GLOBAL_WHEEL: PoisonSafe<WheelSlot> = PoisonSafe::new(WheelSlot(ptr::null_mut()));
pub(crate) static TICKER_RUNNING: AtomicBool = AtomicBool::new(false);
static TICKER_STOP: AtomicBool = AtomicBool::new(false);
static TICKER_HANDLE: OnceLock<Mutex<Option<JoinHandle<()>>>> = OnceLock::new();

/// Return (or create) the global timer wheel and ensure the ticker thread
/// is running.
pub(crate) fn global_wheel() -> *mut HewTimerWheel {
    GLOBAL_WHEEL.access(|guard| {
        if guard.0.is_null() {
            // SAFETY: hew_timer_wheel_new has no preconditions.
            let tw = unsafe { hew_timer_wheel_new() };
            if tw.is_null() {
                return ptr::null_mut();
            }
            guard.0 = tw;
            if !start_ticker_thread(tw) {
                guard.0 = ptr::null_mut();
                // SAFETY: tw was just allocated above and no ticker thread started.
                unsafe {
                    hew_timer_wheel_free(tw);
                }
            }
        } else if !TICKER_RUNNING.load(Ordering::SeqCst) {
            // Wheel exists but ticker was shut down - restart it
            if !start_ticker_thread(guard.0) {
                return ptr::null_mut();
            }
        }
        guard.0
    })
}

/// Spawn a background thread that ticks the global timer wheel every 1 ms.
fn start_ticker_thread(tw: *mut HewTimerWheel) -> bool {
    if TICKER_RUNNING.swap(true, Ordering::SeqCst) {
        return true; // already running
    }

    #[cfg(test)]
    if should_fail_ticker_spawn() {
        TICKER_RUNNING.store(false, Ordering::SeqCst);
        crate::set_last_error("hew_actor_schedule_periodic: failed to spawn timer ticker thread");
        return false;
    }

    let tw_addr = tw as usize;
    let Ok(handle) = std::thread::Builder::new()
        .name("hew-timer-tick".into())
        .spawn(move || {
            let tw = tw_addr as *mut HewTimerWheel;
            loop {
                // Check if shutdown was requested
                if TICKER_STOP.load(Ordering::Acquire) {
                    break;
                }
                std::thread::sleep(std::time::Duration::from_millis(1));
                // Check again after sleep to avoid one more tick after shutdown
                if TICKER_STOP.load(Ordering::Acquire) {
                    break;
                }
                // SAFETY: tw is valid until shutdown_ticker() is called.
                unsafe {
                    hew_timer_wheel_tick(tw);
                }
            }
        })
    else {
        TICKER_RUNNING.store(false, Ordering::SeqCst);
        crate::set_last_error("hew_actor_schedule_periodic: failed to spawn timer ticker thread");
        return false;
    };

    // Store the handle for later joining
    let handle_mutex = TICKER_HANDLE.get_or_init(|| Mutex::new(None));
    *handle_mutex
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner) = Some(handle);
    true
}

// ---------------------------------------------------------------------------
// Periodic timer context (stored as callback data)
// ---------------------------------------------------------------------------

/// Per-actor registry of active periodic timer contexts. Maps the actor's
/// raw address (as `usize`) to (ctx pointer, cancelled flag, in-flight guard)
/// triples. Both Arc flags survive ctx deallocation, so
/// `cancel_all_timers_for_actor` never dereferences raw ctx pointers.
type TimerEntry = (usize, Arc<AtomicBool>, Arc<AtomicBool>);
type TimerRegistry = HashMap<usize, Vec<TimerEntry>>;
static ACTOR_TIMERS: PoisonSafe<Option<TimerRegistry>> = PoisonSafe::new(None);

fn register_timer(
    actor: *mut HewActor,
    ctx_ptr: *mut c_void,
    cancelled: Arc<AtomicBool>,
    in_flight: Arc<AtomicBool>,
) {
    ACTOR_TIMERS.access(|lock| {
        lock.get_or_insert_with(HashMap::new)
            .entry(actor as usize)
            .or_default()
            .push((ctx_ptr as usize, cancelled, in_flight));
    });
}

fn unregister_timer(actor: *mut HewActor, ctx_ptr: *mut c_void) {
    ACTOR_TIMERS.access(|lock| {
        if let Some(map) = lock.as_mut() {
            if let Some(timers) = map.get_mut(&(actor as usize)) {
                timers.retain(|(addr, _, _)| *addr != ctx_ptr as usize);
                if timers.is_empty() {
                    map.remove(&(actor as usize));
                }
            }
        }
    });
}

/// Cancel all periodic timers for a given actor. Called from `hew_actor_free`
/// before deallocation to prevent timer callbacks from sending to freed memory.
///
/// Phase 1 marks all contexts cancelled via their Arc'd flags — no raw ctx
/// pointers are dereferenced. Phase 2 spin-waits on the in-flight guards.
/// Both Arcs survive ctx deallocation so the entire function is safe even
/// if a concurrent callback frees its ctx between phases.
pub(crate) fn cancel_all_timers_for_actor(actor: *mut HewActor) {
    let timers = ACTOR_TIMERS.access(|lock| {
        lock.as_mut()
            .and_then(|map| map.remove(&(actor as usize)))
            .unwrap_or_default()
    });

    // Phase 1: mark all as cancelled via the Arc (never deref raw ctx).
    for (_, cancelled, _) in &timers {
        cancelled.store(true, Ordering::SeqCst);
    }

    // Phase 2: wait for any in-flight callbacks to finish their send.
    for (_, _, in_flight) in &timers {
        while in_flight.load(Ordering::SeqCst) {
            std::hint::spin_loop();
        }
    }
}

/// Returns true if the given timer handle has been cancelled.
///
/// # Safety
///
/// `handle` must be a valid pointer returned by [`hew_actor_schedule_periodic`].
#[cfg(test)]
pub(crate) unsafe fn is_timer_cancelled(handle: *mut c_void) -> bool {
    if handle.is_null() {
        return true;
    }
    // SAFETY: caller guarantees `handle` is a valid PeriodicCtx pointer.
    unsafe { &*(handle.cast::<PeriodicCtx>()) }
        .cancelled
        .load(Ordering::Acquire)
}

/// Returns the number of active (registered) periodic timers for an actor.
#[cfg(test)]
pub(crate) fn timer_count_for_actor(actor: *mut HewActor) -> usize {
    ACTOR_TIMERS.access(|lock| {
        lock.as_ref()
            .and_then(|map| map.get(&(actor as usize)))
            .map_or(0, Vec::len)
    })
}

/// Heap-allocated context for a periodic timer callback.
struct PeriodicCtx {
    actor: *mut HewActor,
    msg_type: i32,
    interval_ms: u64,
    wheel: *mut HewTimerWheel,
    /// Shared with the registry so `cancel_all_timers_for_actor` can set
    /// this without dereferencing the raw ctx pointer.
    cancelled: Arc<AtomicBool>,
    /// Set true while the callback is between the cancelled-check and the
    /// completion of `hew_actor_send`. The `Arc` clone in `ACTOR_TIMERS`
    /// survives ctx deallocation, letting `cancel_all_timers_for_actor`
    /// spin-wait safely.
    in_flight: Arc<AtomicBool>,
}

// SAFETY: Actor pointers are thread-safe (interior atomics).
unsafe impl Send for PeriodicCtx {}
// SAFETY: All mutable state uses atomics; raw pointers are read-only after init.
unsafe impl Sync for PeriodicCtx {}

/// Timer callback: sends a zero-sized message to the actor, then re-schedules.
///
/// Uses a Dekker-style protocol with `in_flight` and `cancelled` (both
/// `SeqCst`) to prevent use-after-free: either the callback sees `cancelled`
/// and skips the send, or `cancel_all_timers_for_actor` sees `in_flight`
/// and waits. `SeqCst` provides the total-order guarantee that at least one
/// side observes the other's flag.
unsafe extern "C" fn periodic_timer_cb(data: *mut c_void) {
    if data.is_null() {
        return;
    }
    // SAFETY: data is a valid PeriodicCtx allocated by schedule_periodic.
    let ctx = unsafe { &*(data.cast::<PeriodicCtx>()) };

    // Mark in-flight BEFORE checking cancelled (Dekker protocol).
    ctx.in_flight.store(true, Ordering::SeqCst);

    if ctx.cancelled.load(Ordering::SeqCst) {
        ctx.in_flight.store(false, Ordering::SeqCst);
        // Cancelled — unregister from per-actor tracking, then free.
        unregister_timer(ctx.actor, data);
        // SAFETY: ctx was Box::into_raw'd, reclaim it.
        let _ = unsafe { Box::from_raw(data.cast::<PeriodicCtx>()) };
        return;
    }

    // Send a zero-payload message to the actor's dispatch function.
    // SAFETY: actor is valid — cancel_all_timers_for_actor is spinning
    // on our in_flight guard and won't free the actor until we clear it.
    unsafe {
        hew_actor_send(ctx.actor, ctx.msg_type, ptr::null_mut(), 0);
    }

    // Send complete — actor pointer is no longer needed.
    ctx.in_flight.store(false, Ordering::SeqCst);

    // Re-schedule.
    // SAFETY: wheel is valid, cb/data are valid (we keep ctx alive).
    unsafe {
        hew_timer_wheel_schedule(ctx.wheel, ctx.interval_ms, periodic_timer_cb, data);
    }
}

// ---------------------------------------------------------------------------
// C ABI export
// ---------------------------------------------------------------------------

/// Schedule a periodic self-send to an actor.
///
/// Every `interval_ms` milliseconds, sends a zero-payload message with type
/// `msg_type` to the actor's dispatch function. The timer repeats until the
/// actor is freed or the handle is cancelled.
///
/// Returns a handle (opaque pointer) that can be passed to
/// [`hew_actor_cancel_periodic`] to stop the timer.
///
/// # Safety
///
/// `actor` must be a valid pointer returned by a spawn function.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_schedule_periodic(
    actor: *mut HewActor,
    msg_type: i32,
    interval_ms: u64,
) -> *mut c_void {
    if actor.is_null() || interval_ms == 0 {
        return ptr::null_mut();
    }

    let tw = global_wheel();
    if tw.is_null() {
        return ptr::null_mut();
    }

    let cancelled = Arc::new(AtomicBool::new(false));
    let in_flight = Arc::new(AtomicBool::new(false));
    let ctx = Box::new(PeriodicCtx {
        actor,
        msg_type,
        interval_ms,
        wheel: tw,
        cancelled: Arc::clone(&cancelled),
        in_flight: Arc::clone(&in_flight),
    });
    let ctx_ptr = Box::into_raw(ctx).cast::<c_void>();

    // Track this timer for per-actor cleanup in hew_actor_free.
    register_timer(actor, ctx_ptr, cancelled, in_flight);

    // Schedule the first tick.
    // SAFETY: tw is valid, ctx_ptr is valid.
    unsafe {
        hew_timer_wheel_schedule(tw, interval_ms, periodic_timer_cb, ctx_ptr);
    }

    ctx_ptr
}

/// Cancel a periodic timer previously started by [`hew_actor_schedule_periodic`].
///
/// # Safety
///
/// `handle` must be a value returned by [`hew_actor_schedule_periodic`].
#[no_mangle]
pub unsafe extern "C" fn hew_actor_cancel_periodic(handle: *mut c_void) {
    if handle.is_null() {
        return;
    }
    // SAFETY: handle is a valid PeriodicCtx pointer.
    let ctx = unsafe { &*(handle.cast::<PeriodicCtx>()) };
    ctx.cancelled.store(true, Ordering::Release);
}

/// Mutex that serialises every test touching the process-wide ticker globals
/// (`GLOBAL_WHEEL`, `TICKER_RUNNING`, `TICKER_STOP`).  Declared at module
/// level so that coupled tests in other modules (e.g. `scheduler`) can import
/// and acquire it without duplicating the guard.
#[cfg(test)]
pub(crate) static TICKER_TEST_MUTEX: Mutex<()> = Mutex::new(());

#[cfg(test)]
thread_local! {
    static FAIL_TICKER_SPAWN: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

#[cfg(test)]
fn should_fail_ticker_spawn() -> bool {
    FAIL_TICKER_SPAWN.with(std::cell::Cell::get)
}

/// Gracefully stop the ticker thread.
///
/// Sets the stop flag, waits for the thread to join, then resets the flag
/// for potential re-initialisation. Safe to call multiple times.
pub(crate) fn shutdown_ticker() {
    // Set the stop flag
    TICKER_STOP.store(true, Ordering::Release);

    // Get the handle and join the thread if it exists
    if let Some(handle_mutex) = TICKER_HANDLE.get() {
        if let Ok(mut guard) = handle_mutex.lock() {
            if let Some(handle) = guard.take() {
                // Wait for the thread to finish
                let _ = handle.join();
            }
        }
    }

    // Reset flags for potential re-initialisation
    TICKER_RUNNING.store(false, Ordering::SeqCst);
    TICKER_STOP.store(false, Ordering::SeqCst);
}

/// Tear down the global periodic timer wheel.
///
/// Called once during process shutdown. After this call, no periodic timers
/// will fire.
///
/// # Safety
///
/// Must be called at most once. No periodic timer APIs may be called after
/// this.
#[no_mangle]
pub unsafe extern "C" fn hew_periodic_shutdown() {
    // First, shutdown the ticker thread to prevent access to the wheel
    shutdown_ticker();

    GLOBAL_WHEEL.access(|guard| {
        let tw = guard.0;
        if !tw.is_null() {
            guard.0 = ptr::null_mut();
            // SAFETY: tw was allocated by hew_timer_wheel_new, and the ticker
            // thread has been stopped.
            unsafe {
                hew_timer_wheel_free(tw);
            }
        }
    });
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::internal::types::HewActorState;
    use std::ffi::CStr;
    use std::sync::atomic::{AtomicI32, AtomicPtr, AtomicU32, AtomicU64};
    use std::time::{Duration, Instant};

    // TICKER_TEST_MUTEX is declared at module level (pub(crate)) so that
    // TICKER_TEST_MUTEX is declared at module level (pub(crate)) and
    // brought in by `use super::*` above; it is also imported directly by
    // coupled tests in other modules (e.g. scheduler).

    static TEST_COUNTER: AtomicU32 = AtomicU32::new(0);

    // Mock timer callback that increments a counter
    unsafe extern "C" fn test_timer_cb(_data: *mut c_void) {
        TEST_COUNTER.fetch_add(1, Ordering::SeqCst);
    }

    fn create_test_actor(id: u64) -> HewActor {
        HewActor {
            sched_link_next: AtomicPtr::new(std::ptr::null_mut()),
            id,
            pid: id,
            state: std::ptr::null_mut(),
            state_size: 0,
            dispatch: None,
            mailbox: std::ptr::null_mut(),
            actor_state: AtomicI32::new(HewActorState::Idle as i32),
            budget: AtomicI32::new(0),
            init_state: std::ptr::null_mut(),
            init_state_size: 0,
            coalesce_key_fn: None,
            terminate_fn: None,
            terminate_called: AtomicBool::new(false),
            terminate_finished: AtomicBool::new(false),
            error_code: AtomicI32::new(0),
            supervisor: std::ptr::null_mut(),
            supervisor_child_index: 0,
            priority: AtomicI32::new(1),
            reductions: AtomicI32::new(0),
            idle_count: AtomicI32::new(0),
            hibernation_threshold: AtomicI32::new(0),
            hibernating: AtomicI32::new(0),
            prof_messages_processed: AtomicU64::new(0),
            prof_processing_time_ns: AtomicU64::new(0),
            arena: std::ptr::null_mut(),
        }
    }

    #[test]
    fn test_ticker_shutdown_positive() {
        let _guard = TICKER_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);

        // Reset counter
        TEST_COUNTER.store(0, Ordering::SeqCst);

        // Get the global timer wheel which will start the ticker
        let tw = global_wheel();
        assert!(!tw.is_null());

        // Schedule a timer to verify the ticker is working
        // SAFETY: tw is a valid timer wheel pointer from global_wheel().
        unsafe {
            hew_timer_wheel_schedule(tw, 5, test_timer_cb, ptr::null_mut());
        }

        // Poll until the timer fires (or give up after 500 ms).  A fixed
        // sleep is fragile under parallel-test or heavy-load conditions.
        let deadline = Instant::now() + Duration::from_millis(500);
        while TEST_COUNTER.load(Ordering::SeqCst) == 0 {
            assert!(
                Instant::now() < deadline,
                "Timer did not fire within 500 ms"
            );
            std::thread::sleep(Duration::from_millis(2));
        }

        // Now shutdown the ticker and measure how long it takes
        let start_time = Instant::now();
        shutdown_ticker();
        let shutdown_duration = start_time.elapsed();

        // Should join within 500ms as required
        assert!(
            shutdown_duration < Duration::from_millis(500),
            "Ticker thread should join within 500ms, took {shutdown_duration:?}"
        );
    }

    #[test]
    fn test_ticker_shutdown_negative() {
        let _guard = TICKER_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);

        // Test calling shutdown when no ticker was started
        // This should not panic or hang
        shutdown_ticker(); // Should be safe to call multiple times
        shutdown_ticker(); // Should be safe to call again
    }

    #[test]
    fn test_ticker_shutdown_sabotage() {
        let _guard = TICKER_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);

        // Reset counter
        TEST_COUNTER.store(0, Ordering::SeqCst);

        // Get the global timer wheel
        let tw = global_wheel();

        // Schedule a timer
        // SAFETY: tw is a valid timer wheel pointer from global_wheel().
        unsafe {
            hew_timer_wheel_schedule(tw, 5, test_timer_cb, ptr::null_mut());
        }

        // Poll until the timer fires (or give up after 500 ms).
        let deadline = Instant::now() + Duration::from_millis(500);
        while TEST_COUNTER.load(Ordering::SeqCst) == 0 {
            assert!(
                Instant::now() < deadline,
                "Timer did not fire within 500 ms"
            );
            std::thread::sleep(Duration::from_millis(2));
        }

        // Now temporarily disable the stop check to test that our test detects the UAF scenario
        // This is commented out because it would break the fix, but this test structure
        // demonstrates that the fix is necessary.

        // TO TEST: Comment out the TICKER_STOP.load checks in the ticker loop,
        // then run this test. It should timeout/fail on the join, proving the test
        // detects the actual UAF scenario.

        let start_time = Instant::now();
        shutdown_ticker();
        let shutdown_duration = start_time.elapsed();

        // With the fix, this should complete quickly
        assert!(
            shutdown_duration < Duration::from_millis(500),
            "With the fix, ticker should join within 500ms, took {shutdown_duration:?}"
        );
    }

    #[test]
    fn test_ticker_restart_after_shutdown() {
        let _guard = TICKER_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);

        // Test that we can restart the ticker after shutdown
        TEST_COUNTER.store(0, Ordering::SeqCst);

        // Start ticker, then shutdown
        let tw1 = global_wheel();
        // SAFETY: tw1 is a valid timer wheel pointer from global_wheel().
        unsafe {
            hew_timer_wheel_schedule(tw1, 10, test_timer_cb, ptr::null_mut());
        }

        let deadline = Instant::now() + Duration::from_millis(500);
        while TEST_COUNTER.load(Ordering::SeqCst) == 0 {
            assert!(
                Instant::now() < deadline,
                "First timer did not fire within 500 ms"
            );
            std::thread::sleep(Duration::from_millis(2));
        }
        shutdown_ticker();

        let count_after_first = TEST_COUNTER.load(Ordering::SeqCst);
        assert!(count_after_first > 0);

        // Reset and start again - this should work due to flag reset
        TEST_COUNTER.store(0, Ordering::SeqCst);
        let tw2 = global_wheel();
        // SAFETY: tw2 is a valid timer wheel pointer from global_wheel().
        unsafe {
            hew_timer_wheel_schedule(tw2, 10, test_timer_cb, ptr::null_mut());
        }

        let deadline2 = Instant::now() + Duration::from_millis(500);
        while TEST_COUNTER.load(Ordering::SeqCst) == 0 {
            assert!(
                Instant::now() < deadline2,
                "Ticker should restart after shutdown — timer did not fire within 500 ms"
            );
            std::thread::sleep(Duration::from_millis(2));
        }

        // Cleanup
        shutdown_ticker();
    }

    #[test]
    fn cancel_all_timers_marks_contexts_cancelled() {
        let mut actor = create_test_actor(50_100);
        let actor_ptr = &raw mut actor;

        // Manually create PeriodicCtx entries and register them (bypassing
        // the timer wheel to avoid side-effects).
        let cancelled1 = Arc::new(AtomicBool::new(false));
        let guard1 = Arc::new(AtomicBool::new(false));
        let ctx1 = Box::new(PeriodicCtx {
            actor: actor_ptr,
            msg_type: 0,
            interval_ms: 100,
            wheel: ptr::null_mut(),
            cancelled: Arc::clone(&cancelled1),
            in_flight: Arc::clone(&guard1),
        });
        let ctx1_ptr = Box::into_raw(ctx1).cast::<c_void>();
        register_timer(actor_ptr, ctx1_ptr, cancelled1, guard1);

        let cancelled2 = Arc::new(AtomicBool::new(false));
        let guard2 = Arc::new(AtomicBool::new(false));
        let ctx2 = Box::new(PeriodicCtx {
            actor: actor_ptr,
            msg_type: 1,
            interval_ms: 200,
            wheel: ptr::null_mut(),
            cancelled: Arc::clone(&cancelled2),
            in_flight: Arc::clone(&guard2),
        });
        let ctx2_ptr = Box::into_raw(ctx2).cast::<c_void>();
        register_timer(actor_ptr, ctx2_ptr, cancelled2, guard2);

        assert_eq!(timer_count_for_actor(actor_ptr), 2);

        // Act: cancel all timers for this actor.
        cancel_all_timers_for_actor(actor_ptr);

        // Both contexts should be cancelled.
        // SAFETY: ctx1_ptr is still valid — only the `cancelled` flag changed.
        assert!(unsafe { is_timer_cancelled(ctx1_ptr) });
        // SAFETY: ctx2_ptr is still valid — only the `cancelled` flag changed.
        assert!(unsafe { is_timer_cancelled(ctx2_ptr) });

        // Registry should be empty for this actor.
        assert_eq!(timer_count_for_actor(actor_ptr), 0);

        // Clean up the leaked boxes.
        // SAFETY: We own these allocations.
        unsafe {
            let _ = Box::from_raw(ctx1_ptr.cast::<PeriodicCtx>());
            let _ = Box::from_raw(ctx2_ptr.cast::<PeriodicCtx>());
        }
    }

    #[test]
    fn cancel_all_timers_no_timers_does_not_panic() {
        let mut actor = create_test_actor(50_200);
        let actor_ptr = &raw mut actor;
        // Should be a no-op, not panic.
        cancel_all_timers_for_actor(actor_ptr);
        assert_eq!(timer_count_for_actor(actor_ptr), 0);
    }

    #[test]
    fn schedule_periodic_spawn_failure_returns_null_without_leaking_wheel() {
        let _guard = TICKER_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);

        // SAFETY: test owns teardown of the process-wide wheel state.
        unsafe {
            hew_periodic_shutdown();
        }
        crate::hew_clear_error();

        FAIL_TICKER_SPAWN.with(|fail| fail.set(true));
        let mut actor = create_test_actor(50_300);
        let actor_ptr = &raw mut actor;
        // SAFETY: actor_ptr points to a valid test actor and interval is non-zero.
        let handle = unsafe { hew_actor_schedule_periodic(actor_ptr, 7, 10) };
        FAIL_TICKER_SPAWN.with(|fail| fail.set(false));

        assert!(handle.is_null(), "spawn failure should fail closed");
        assert_eq!(
            timer_count_for_actor(actor_ptr),
            0,
            "failed start must not register a periodic timer"
        );
        assert!(
            !TICKER_RUNNING.load(Ordering::SeqCst),
            "ticker should not remain marked running after spawn failure"
        );
        let wheel_is_null = GLOBAL_WHEEL.access(|guard| guard.0.is_null());
        assert!(
            wheel_is_null,
            "newly created wheel should be freed when ticker spawn fails"
        );

        // SAFETY: hew_last_error returns a valid C string pointer for the current thread.
        let err = unsafe { CStr::from_ptr(crate::hew_last_error()) }
            .to_str()
            .expect("last error should be utf-8");
        assert!(
            err.contains("failed to spawn timer ticker thread"),
            "unexpected last error: {err}"
        );
    }
}
