//! Periodic timer support for Hew actors.
//!
//! Provides `hew_actor_schedule_periodic` which schedules a repeating
//! self-send to an actor at a fixed interval using the runtime's timer wheel.

use std::ffi::c_void;
use std::ptr;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Mutex;

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
    }
    guard.0
}

/// Spawn a background thread that ticks the global timer wheel every 1 ms.
fn start_ticker_thread(tw: *mut HewTimerWheel) {
    if TICKER_RUNNING.swap(true, Ordering::SeqCst) {
        return; // already running
    }
    let tw_addr = tw as usize;
    std::thread::Builder::new()
        .name("hew-timer-tick".into())
        .spawn(move || {
            let tw = tw_addr as *mut HewTimerWheel;
            loop {
                std::thread::sleep(std::time::Duration::from_millis(1));
                // SAFETY: tw is valid for the lifetime of the process.
                unsafe {
                    hew_timer_wheel_tick(tw);
                }
            }
        })
        .expect("failed to spawn timer ticker thread");
}

// ---------------------------------------------------------------------------
// Periodic timer context (stored as callback data)
// ---------------------------------------------------------------------------

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

    if ctx.cancelled.load(Ordering::Relaxed) {
        // Cancelled — free the context and stop re-scheduling.
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
    ctx.cancelled.store(true, Ordering::Relaxed);
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
    let mut guard = GLOBAL_WHEEL
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    let tw = guard.0;
    if !tw.is_null() {
        guard.0 = ptr::null_mut();
        // SAFETY: tw was allocated by hew_timer_wheel_new.
        unsafe {
            hew_timer_wheel_free(tw);
        }
    }
}
