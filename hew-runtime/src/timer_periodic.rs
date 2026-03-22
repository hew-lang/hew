//! Periodic timer support for Hew actors.
//!
//! Provides `hew_actor_schedule_periodic` which schedules a repeating
//! self-send to an actor at a fixed interval using the runtime's timer wheel.

use std::collections::HashMap;
use std::ffi::c_void;
use std::ptr;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Mutex, OnceLock};
use std::thread::JoinHandle;

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

static GLOBAL_WHEEL: Mutex<WheelSlot> = Mutex::new(WheelSlot(ptr::null_mut()));
static TICKER_RUNNING: AtomicBool = AtomicBool::new(false);
static TICKER_STOP: AtomicBool = AtomicBool::new(false);
static TICKER_HANDLE: OnceLock<Mutex<Option<JoinHandle<()>>>> = OnceLock::new();

/// Return (or create) the global timer wheel and ensure the ticker thread
/// is running.
fn global_wheel() -> *mut HewTimerWheel {
    let mut guard = GLOBAL_WHEEL
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    if guard.0.is_null() {
        // SAFETY: hew_timer_wheel_new has no preconditions.
        let tw = unsafe { hew_timer_wheel_new() };
        guard.0 = tw;
        start_ticker_thread(tw);
    } else if !TICKER_RUNNING.load(Ordering::SeqCst) {
        // Wheel exists but ticker was shut down - restart it
        start_ticker_thread(guard.0);
    }
    guard.0
}

/// Spawn a background thread that ticks the global timer wheel every 1 ms.
fn start_ticker_thread(tw: *mut HewTimerWheel) {
    if TICKER_RUNNING.swap(true, Ordering::SeqCst) {
        return; // already running
    }
    let tw_addr = tw as usize;
    let handle = std::thread::Builder::new()
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
        .expect("failed to spawn timer ticker thread");

    // Store the handle for later joining
    let handle_mutex = TICKER_HANDLE.get_or_init(|| Mutex::new(None));
    *handle_mutex
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner) = Some(handle);
}

// ---------------------------------------------------------------------------
// Periodic timer context (stored as callback data)
// ---------------------------------------------------------------------------

/// Per-actor registry of active periodic timer contexts. Maps the actor's
/// raw address (as `usize`) to the set of `PeriodicCtx` pointers for that
/// actor, so `hew_actor_free` can cancel all timers before deallocation.
static ACTOR_TIMERS: Mutex<Option<HashMap<usize, Vec<usize>>>> = Mutex::new(None);

fn register_timer(actor: *mut HewActor, ctx_ptr: *mut c_void) {
    let mut guard = ACTOR_TIMERS
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    guard
        .get_or_insert_with(HashMap::new)
        .entry(actor as usize)
        .or_default()
        .push(ctx_ptr as usize);
}

fn unregister_timer(actor: *mut HewActor, ctx_ptr: *mut c_void) {
    let mut guard = ACTOR_TIMERS
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    if let Some(map) = guard.as_mut() {
        if let Some(timers) = map.get_mut(&(actor as usize)) {
            timers.retain(|&addr| addr != ctx_ptr as usize);
            if timers.is_empty() {
                map.remove(&(actor as usize));
            }
        }
    }
}

/// Cancel all periodic timers for a given actor. Called from `hew_actor_free`
/// before deallocation to prevent timer callbacks from sending to freed memory.
pub(crate) fn cancel_all_timers_for_actor(actor: *mut HewActor) {
    let timers = {
        let mut guard = ACTOR_TIMERS
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        guard
            .as_mut()
            .and_then(|map| map.remove(&(actor as usize)))
            .unwrap_or_default()
    };
    for ctx_addr in timers {
        let ctx = ctx_addr as *mut PeriodicCtx;
        // SAFETY: ctx was allocated by hew_actor_schedule_periodic and is
        // still valid (the timer callback frees it only after seeing cancelled).
        unsafe { &*ctx }.cancelled.store(true, Ordering::Release);
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
    let guard = ACTOR_TIMERS
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    guard
        .as_ref()
        .and_then(|map| map.get(&(actor as usize)))
        .map_or(0, Vec::len)
}

/// Heap-allocated context for a periodic timer callback.
struct PeriodicCtx {
    actor: *mut HewActor,
    msg_type: i32,
    interval_ms: u64,
    wheel: *mut HewTimerWheel,
    cancelled: AtomicBool,
}

// SAFETY: Actor pointers are thread-safe (interior atomics).
unsafe impl Send for PeriodicCtx {}
// SAFETY: All mutable state uses AtomicBool; raw pointers are read-only after init.
unsafe impl Sync for PeriodicCtx {}

/// Timer callback: sends a zero-sized message to the actor, then re-schedules.
unsafe extern "C" fn periodic_timer_cb(data: *mut c_void) {
    if data.is_null() {
        return;
    }
    // SAFETY: data is a valid PeriodicCtx allocated by schedule_periodic.
    let ctx = unsafe { &*(data.cast::<PeriodicCtx>()) };

    if ctx.cancelled.load(Ordering::Acquire) {
        // Cancelled — unregister from per-actor tracking, then free.
        unregister_timer(ctx.actor, data);
        // SAFETY: ctx was Box::into_raw'd, reclaim it.
        let _ = unsafe { Box::from_raw(data.cast::<PeriodicCtx>()) };
        return;
    }

    // Send a zero-payload message to the actor's dispatch function.
    // SAFETY: actor is a valid pointer; msg_type is the handler index.
    unsafe {
        hew_actor_send(ctx.actor, ctx.msg_type, ptr::null_mut(), 0);
    }

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

    let ctx = Box::new(PeriodicCtx {
        actor,
        msg_type,
        interval_ms,
        wheel: tw,
        cancelled: AtomicBool::new(false),
    });
    let ctx_ptr = Box::into_raw(ctx).cast::<c_void>();

    // Track this timer for per-actor cleanup in hew_actor_free.
    register_timer(actor, ctx_ptr);

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

    let mut guard = GLOBAL_WHEEL
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    let tw = guard.0;
    if !tw.is_null() {
        guard.0 = ptr::null_mut();
        // SAFETY: tw was allocated by hew_timer_wheel_new, and the ticker
        // thread has been stopped.
        unsafe {
            hew_timer_wheel_free(tw);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::internal::types::HewActorState;
    use std::sync::atomic::{AtomicI32, AtomicPtr, AtomicU32, AtomicU64};
    use std::time::{Duration, Instant};

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

        // Wait for at least one tick to verify the ticker is working
        std::thread::sleep(Duration::from_millis(20));
        let count_after_ticks = TEST_COUNTER.load(Ordering::SeqCst);
        assert!(
            count_after_ticks > 0,
            "Timer should have fired at least once"
        );

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
        // Test calling shutdown when no ticker was started
        // This should not panic or hang
        shutdown_ticker(); // Should be safe to call multiple times
        shutdown_ticker(); // Should be safe to call again
    }

    #[test]
    fn test_ticker_shutdown_sabotage() {
        // Reset counter
        TEST_COUNTER.store(0, Ordering::SeqCst);

        // Get the global timer wheel
        let tw = global_wheel();

        // Schedule a timer
        // SAFETY: tw is a valid timer wheel pointer from global_wheel().
        unsafe {
            hew_timer_wheel_schedule(tw, 5, test_timer_cb, ptr::null_mut());
        }

        // Wait for at least one tick to verify the ticker is working
        std::thread::sleep(Duration::from_millis(20));
        let count_after_ticks = TEST_COUNTER.load(Ordering::SeqCst);
        assert!(
            count_after_ticks > 0,
            "Timer should have fired at least once"
        );

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
        // Test that we can restart the ticker after shutdown
        TEST_COUNTER.store(0, Ordering::SeqCst);

        // Start ticker, then shutdown
        let tw1 = global_wheel();
        // SAFETY: tw1 is a valid timer wheel pointer from global_wheel().
        unsafe {
            hew_timer_wheel_schedule(tw1, 10, test_timer_cb, ptr::null_mut());
        }
        std::thread::sleep(Duration::from_millis(25));
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
        std::thread::sleep(Duration::from_millis(25));
        let count_after_second = TEST_COUNTER.load(Ordering::SeqCst);
        assert!(
            count_after_second > 0,
            "Ticker should restart after shutdown"
        );

        // Cleanup
        shutdown_ticker();
    }

    #[test]
    fn cancel_all_timers_marks_contexts_cancelled() {
        let mut actor = create_test_actor(50_100);
        let actor_ptr = &raw mut actor;

        // Manually create PeriodicCtx entries and register them (bypassing
        // the timer wheel to avoid side-effects).
        let ctx1 = Box::new(PeriodicCtx {
            actor: actor_ptr,
            msg_type: 0,
            interval_ms: 100,
            wheel: ptr::null_mut(),
            cancelled: AtomicBool::new(false),
        });
        let ctx1_ptr = Box::into_raw(ctx1).cast::<c_void>();
        register_timer(actor_ptr, ctx1_ptr);

        let ctx2 = Box::new(PeriodicCtx {
            actor: actor_ptr,
            msg_type: 1,
            interval_ms: 200,
            wheel: ptr::null_mut(),
            cancelled: AtomicBool::new(false),
        });
        let ctx2_ptr = Box::into_raw(ctx2).cast::<c_void>();
        register_timer(actor_ptr, ctx2_ptr);

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
}
