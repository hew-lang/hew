//! Periodic timer support for Hew actors.
//!
//! Provides `hew_actor_schedule_periodic` which schedules a repeating
//! self-send to an actor at a fixed interval using the runtime's timer wheel.
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

use std::collections::HashMap;
use std::ffi::c_void;
use std::ptr;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Condvar, Mutex, OnceLock};
use std::thread::JoinHandle;

use crate::lifetime::PoisonSafe;

use crate::actor::{hew_actor_send, HewActor};
use crate::timer_wheel::{
    hew_timer_wheel_free, hew_timer_wheel_new, hew_timer_wheel_next_deadline_ms,
    hew_timer_wheel_remove, hew_timer_wheel_schedule_handle, hew_timer_wheel_tick, HewTimerHandle,
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

// ---------------------------------------------------------------------------
// Ticker park primitive (tickless idle — no spin when wheel is empty)
//
// The ticker parks here instead of unconditionally sleeping 1 ms per loop.
// When the wheel is empty it waits indefinitely; otherwise it waits until the
// next deadline.  Any new-timer insert (via register_ticker_notify_hook) and
// shutdown both signal this to interrupt the current park.
//
// INVARIANT: the `bool` inside the Mutex is `true` when a notification is
// pending (standard "spurious-wake eliminator" / condvar ping pattern).
// ---------------------------------------------------------------------------

struct TickerPark {
    mu: Mutex<bool>,
    cv: Condvar,
}

static TICKER_PARK: OnceLock<TickerPark> = OnceLock::new();

fn ticker_park() -> &'static TickerPark {
    TICKER_PARK.get_or_init(|| TickerPark {
        mu: Mutex::new(false),
        cv: Condvar::new(),
    })
}

/// Called (from the insert hook registered with the wheel) whenever a new
/// timer is inserted.  Signals the ticker to re-evaluate its park deadline.
fn ticker_park_notify() {
    let park = ticker_park();
    *park
        .mu
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner) = true;
    park.cv.notify_one();
}

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

/// Return the process-wide timer wheel, lazily creating it and ensuring the
/// 1 ms ticker thread is running. This is the deadline-schedule target for
/// NEW-6b `await … | after d`: codegen passes the returned wheel to
/// `hew_await_cancel_schedule_deadline_ms`. Returns null only if the wheel /
/// ticker could not be created (the caller then skips arming the deadline and
/// the await behaves as an un-deadlined suspend — fail-safe, never a hang of
/// the timer subsystem itself).
///
/// # Safety
///
/// FFI entry point. The returned pointer is owned by the runtime singleton and
/// outlives every scheduled deadline; callers must not free it.
#[no_mangle]
pub unsafe extern "C" fn hew_global_timer_wheel() -> *mut HewTimerWheel {
    global_wheel()
}

/// Spawn a background thread that ticks the global timer wheel, parking until
/// the next timer deadline (tickless when the wheel is empty).
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

    // Register the insert-notify hook so every new timer scheduled on the
    // global wheel wakes the ticker to re-evaluate its park deadline.
    crate::timer_wheel::register_ticker_notify_hook(ticker_park_notify);

    let tw_addr = tw as usize;
    let Ok(handle) = std::thread::Builder::new()
        .name("hew-timer-tick".into())
        .spawn(move || {
            let tw = tw_addr as *mut HewTimerWheel;
            let park = ticker_park();
            loop {
                // ── compute how long to park ──────────────────────────────
                // SAFETY: tw is valid until shutdown_ticker() is called.
                let gap_ms: i64 = unsafe { hew_timer_wheel_next_deadline_ms(tw) };
                // gap_ms == -1 → empty wheel → park indefinitely.
                // gap_ms == 0  → deadline already passed → do not park.
                // gap_ms > 0   → park for that many ms (minimum 1).

                if gap_ms != 0 {
                    let mut notified = park
                        .mu
                        .lock()
                        .unwrap_or_else(std::sync::PoisonError::into_inner);
                    // If a notification arrived between the gap computation
                    // and acquiring the lock, consume it and skip the park.
                    if *notified {
                        *notified = false;
                    } else if gap_ms < 0 {
                        // Empty wheel: park indefinitely until notified or
                        // shutdown signals.
                        let _guard = park
                            .cv
                            .wait_while(notified, |n| {
                                if *n {
                                    *n = false;
                                    false // stop waiting
                                } else {
                                    true // keep waiting
                                }
                            })
                            .unwrap_or_else(std::sync::PoisonError::into_inner);
                    } else {
                        // Known deadline: park for gap_ms (at least 1 ms so
                        // we never spin when gap rounds to zero).
                        let timeout =
                            std::time::Duration::from_millis(gap_ms.max(1).cast_unsigned());
                        let (_guard, _timeout_result) = park
                            .cv
                            .wait_timeout_while(notified, timeout, |n| {
                                if *n {
                                    *n = false;
                                    false // stop waiting
                                } else {
                                    true // keep waiting
                                }
                            })
                            .unwrap_or_else(std::sync::PoisonError::into_inner);
                    }
                }

                // ── check shutdown (after waking, before tick) ────────────
                if TICKER_STOP.load(Ordering::Acquire) {
                    break;
                }

                // ── tick the wheel ────────────────────────────────────────
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

/// Per-actor registry of active periodic timer contexts.
///
/// Each [`PeriodicCtx`] is owned through an [`Arc`]. There are at most two
/// strong references at any instant:
///
/// * one held here in the registry (per active timer), and
/// * one held by the *single* pending one-shot in the global wheel
///   (handed over via `Arc::into_raw` when the entry is scheduled).
///
/// Ownership of the ctx allocation is therefore decided by the `Arc` refcount —
/// the **last** holder to drop its strong reference frees it, so exactly one
/// party ever performs the deallocation. There is no separate "free flag": the
/// reclaim of the wheel's reference (`Arc::from_raw`) *is* the atomic claim. The
/// callback claims by reclaiming on fire; `cancel`/`shutdown` claim by removing
/// the pending wheel entry (`hew_timer_wheel_remove`) and reclaiming its
/// reference. The wheel removal is atomic with deadline firing, so the pending
/// reference is reclaimed by exactly one of the two — never both (no
/// double-free) and never neither (no leak). Because the wheel's strong
/// reference keeps the ctx alive until it is reclaimed, no party ever
/// dereferences a freed ctx (no use-after-free).
type TimerRegistry = HashMap<usize, Vec<Arc<PeriodicCtx>>>;
static ACTOR_TIMERS: PoisonSafe<Option<TimerRegistry>> = PoisonSafe::new(None);

fn register_timer(actor: *mut HewActor, ctx: Arc<PeriodicCtx>) {
    ACTOR_TIMERS.access(|lock| {
        lock.get_or_insert_with(HashMap::new)
            .entry(actor as usize)
            .or_default()
            .push(ctx);
    });
}

/// Drop the registry's strong reference to the ctx at `ctx_addr` for `actor`.
fn unregister_timer(actor: *mut HewActor, ctx_addr: usize) {
    ACTOR_TIMERS.access(|lock| {
        if let Some(map) = lock.as_mut() {
            if let Some(timers) = map.get_mut(&(actor as usize)) {
                timers.retain(|c| Arc::as_ptr(c) as usize != ctx_addr);
                if timers.is_empty() {
                    map.remove(&(actor as usize));
                }
            }
        }
    });
}

/// Reclaim and drop the wheel's pending strong reference for `ctx`, if any.
///
/// Takes the pending handle under the ctx lock and removes the entry from the
/// wheel. If the entry was still pending, `hew_timer_wheel_remove` returns its
/// `data` pointer (the `Arc::into_raw` reference handed to the wheel) which we
/// reclaim and drop. If the entry had already fired or been collected for
/// firing, the removal finds nothing and the firing callback reclaims that
/// reference instead — in either case the reference is reclaimed exactly once.
fn reclaim_pending_wheel_ref(ctx: &Arc<PeriodicCtx>) {
    let handle = ctx
        .pending
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .take();
    if let Some(h) = handle {
        // SAFETY: `ctx.wheel` is the wheel the handle was scheduled on; `h`
        // carries a matching entry/generation pair.
        let data = unsafe { hew_timer_wheel_remove(ctx.wheel, h.entry, h.generation) };
        if !data.is_null() {
            // SAFETY: `data` is the `Arc::into_raw` reference we handed the
            // wheel; reclaim it so its strong count is released.
            drop(unsafe { Arc::from_raw(data.cast::<PeriodicCtx>()) });
        }
    }
}

/// Cancel all periodic timers for a given actor. Called from `hew_actor_free`
/// before deallocation to prevent timer callbacks from sending to freed memory.
///
/// Phase 1 marks every ctx cancelled. Phase 2 spin-waits on the in-flight
/// guards so any executing callback finishes its `hew_actor_send` before the
/// actor is freed. Phase 3 reclaims each pending wheel reference so the ctx is
/// freed promptly instead of leaking until the (now suppressed) callback would
/// have fired. The `Arc` refcount guarantees exactly one free across the
/// callback and this function; ctxs are kept alive by the registry references
/// held in `timers` for the whole duration, so no raw ctx is ever dangling.
pub(crate) fn cancel_all_timers_for_actor(actor: *mut HewActor) {
    let timers = ACTOR_TIMERS.access(|lock| {
        lock.as_mut()
            .and_then(|map| map.remove(&(actor as usize)))
            .unwrap_or_default()
    });

    // Phase 1: mark all as cancelled. A callback that reaches its re-arm under
    // the pending lock after this store will not re-arm; a callback already
    // past the cancelled-check is bounded by the in-flight guard below.
    for ctx in &timers {
        ctx.cancelled.store(true, Ordering::SeqCst);
    }

    // Phase 2: wait for any in-flight callbacks to finish their send (Dekker
    // protocol — keeps the actor alive until the send completes).
    for ctx in &timers {
        while ctx.in_flight.load(Ordering::SeqCst) {
            std::hint::spin_loop();
        }
    }

    // Phase 3: reclaim each pending wheel reference (the single atomic claim).
    for ctx in &timers {
        reclaim_pending_wheel_ref(ctx);
    }

    // Dropping `timers` releases the registry references; any ctx whose wheel
    // reference was reclaimed above now reaches refcount zero and is freed.
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
///
/// Owned through an [`Arc`]; see [`ACTOR_TIMERS`] for the ownership protocol.
struct PeriodicCtx {
    actor: *mut HewActor,
    msg_type: i32,
    interval_ms: u64,
    wheel: *mut HewTimerWheel,
    /// Set true to stop the timer. Once observed by the callback (under the
    /// `pending` lock at re-arm, or at the Dekker cancelled-check) the timer
    /// will not re-arm and the ctx is released.
    cancelled: AtomicBool,
    /// Set true while the callback is between the cancelled-check and the
    /// completion of `hew_actor_send`. `cancel_all_timers_for_actor` spin-waits
    /// on this (Dekker protocol) so the actor outlives any in-flight send.
    in_flight: AtomicBool,
    /// Handle of the single pending one-shot in the wheel, or `None` while the
    /// callback is executing (the entry has been consumed and not yet
    /// re-armed). The wheel holds one `Arc::into_raw` strong reference for as
    /// long as this is `Some`. Guarded by a mutex so re-arm (callback) and
    /// reclaim (cancel/shutdown) never race on the pending entry.
    pending: Mutex<Option<HewTimerHandle>>,
}

// SAFETY: Actor pointers are thread-safe (interior atomics).
unsafe impl Send for PeriodicCtx {}
// SAFETY: All mutable state uses atomics / a mutex; raw pointers are read-only
// after init.
unsafe impl Sync for PeriodicCtx {}

/// Timer callback: sends a zero-sized message to the actor, then re-schedules.
///
/// The callback's first act is to *claim* the wheel's strong reference via
/// `Arc::from_raw` — this is always safe because that reference kept the ctx
/// alive until now. A Dekker-style protocol with `in_flight` and `cancelled`
/// (both `SeqCst`) prevents sending to a freed actor: either the callback sees
/// `cancelled` and skips the send, or `cancel_all_timers_for_actor` sees
/// `in_flight` and waits. Re-arming happens under the `pending` lock with a
/// re-check of `cancelled`, so a concurrent cancel either removes the freshly
/// armed entry or prevents it from being armed at all.
unsafe extern "C" fn periodic_timer_cb(data: *mut c_void) {
    if data.is_null() {
        return;
    }
    // SAFETY: `data` is the `Arc::into_raw` reference handed to the wheel when
    // this entry was scheduled; reclaim it as an owned strong reference. The
    // wheel's reference guarantees the ctx is still alive here.
    let ctx = unsafe { Arc::from_raw(data.cast::<PeriodicCtx>()) };
    let ctx_addr = Arc::as_ptr(&ctx) as usize;

    // The entry we are running has been removed from the wheel; clear the
    // pending handle so a concurrent reclaim does not also target it.
    *ctx.pending
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner) = None;

    // Mark in-flight BEFORE checking cancelled (Dekker protocol).
    ctx.in_flight.store(true, Ordering::SeqCst);

    if ctx.cancelled.load(Ordering::SeqCst) {
        ctx.in_flight.store(false, Ordering::SeqCst);
        // Cancelled — drop the registry reference (no-op if cancel_all already
        // removed it). Dropping `ctx` releases our reclaimed reference; the ctx
        // is freed once both references are gone.
        unregister_timer(ctx.actor, ctx_addr);
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

    // Re-arm under the pending lock, re-checking cancelled so we never leave a
    // stray entry behind a cancel that has already passed its reclaim phase.
    let mut pending = ctx
        .pending
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    if ctx.cancelled.load(Ordering::SeqCst) {
        drop(pending);
        unregister_timer(ctx.actor, ctx_addr);
        return;
    }
    // Hand a fresh strong reference to the wheel for the next one-shot.
    let next = Arc::into_raw(Arc::clone(&ctx)).cast::<c_void>().cast_mut();
    // SAFETY: wheel is valid; `next` stays valid until the entry fires or is
    // removed (both reclaim it exactly once).
    let handle = unsafe {
        hew_timer_wheel_schedule_handle(ctx.wheel, ctx.interval_ms, periodic_timer_cb, next)
    };
    if handle.entry.is_null() {
        // Scheduling failed: reclaim the reference we just leaked into `next`
        // so the ctx is not leaked, and stop the timer.
        // SAFETY: `next` is the reference we just created and the wheel did not
        // take ownership (null handle).
        drop(unsafe { Arc::from_raw(next.cast::<PeriodicCtx>()) });
        drop(pending);
        unregister_timer(ctx.actor, ctx_addr);
        return;
    }
    *pending = Some(handle);
    // `pending` lock and `ctx` reclaimed-reference dropped here.
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

    let ctx = Arc::new(PeriodicCtx {
        actor,
        msg_type,
        interval_ms,
        wheel: tw,
        cancelled: AtomicBool::new(false),
        in_flight: AtomicBool::new(false),
        pending: Mutex::new(None),
    });
    // Stable identity handle returned to the caller (valid while any strong
    // reference exists).
    let handle_ptr = Arc::as_ptr(&ctx).cast::<c_void>().cast_mut();

    // Track this timer for per-actor cleanup in hew_actor_free (registry
    // strong reference).
    register_timer(actor, Arc::clone(&ctx));

    // Hand a strong reference to the wheel for the first one-shot.
    let data = Arc::into_raw(Arc::clone(&ctx)).cast::<c_void>().cast_mut();
    // SAFETY: tw is valid; `data` stays valid until the entry fires or is
    // removed (both reclaim the reference exactly once).
    let wheel_handle =
        unsafe { hew_timer_wheel_schedule_handle(tw, interval_ms, periodic_timer_cb, data) };
    if wheel_handle.entry.is_null() {
        // Scheduling failed: reclaim the wheel reference and the registry
        // reference so nothing leaks, and report failure.
        // SAFETY: `data` is the reference we just created; the wheel did not
        // take ownership (null handle).
        drop(unsafe { Arc::from_raw(data.cast::<PeriodicCtx>()) });
        unregister_timer(actor, handle_ptr as usize);
        return ptr::null_mut();
    }
    *ctx.pending
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner) = Some(wheel_handle);

    handle_ptr
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
    // Locate a strong reference by identity (the handle address) WITHOUT
    // dereferencing the raw handle — if the timer already fired and was freed
    // it simply is not in the registry, so this is a safe no-op.
    let addr = handle as usize;
    let ctx = ACTOR_TIMERS.access(|lock| {
        lock.as_ref().and_then(|map| {
            map.values()
                .flatten()
                .find(|c| Arc::as_ptr(c) as usize == addr)
                .map(Arc::clone)
        })
    });
    let Some(ctx) = ctx else {
        return;
    };
    let actor = ctx.actor;
    ctx.cancelled.store(true, Ordering::SeqCst);
    while ctx.in_flight.load(Ordering::SeqCst) {
        std::hint::spin_loop();
    }
    reclaim_pending_wheel_ref(&ctx);
    unregister_timer(actor, addr);
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

fn report_ticker_join_result(join_result: std::thread::Result<()>) {
    if let Err(panic_payload) = join_result {
        let message = format!(
            "hew timer ticker panicked during shutdown: {}",
            crate::util::panic_payload_message(panic_payload.as_ref())
        );
        crate::set_last_error(message.clone());
        eprintln!("hew: {message}");
    }
}

/// Gracefully stop the ticker thread.
///
/// Sets the stop flag, waits for the thread to join, then resets the flag
/// for potential re-initialisation. Safe to call multiple times.
pub(crate) fn shutdown_ticker() {
    // Set the stop flag first so the ticker exits after its next wakeup.
    TICKER_STOP.store(true, Ordering::Release);

    // Signal the condvar so a parked ticker wakes immediately instead of
    // sleeping until its next deadline (or indefinitely on an empty wheel).
    ticker_park_notify();

    // Take the handle without holding its mutex while joining or reporting.
    let handle = TICKER_HANDLE.get().and_then(|handle_mutex| {
        handle_mutex
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .take()
    });
    if let Some(handle) = handle {
        let join_result = handle.join();
        report_ticker_join_result(join_result);
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
    // First, shutdown the ticker thread to prevent access to the wheel. After
    // this returns no callback is executing, so the only remaining strong
    // references to any PeriodicCtx are the registry references and the wheel's
    // pending one-shot references.
    shutdown_ticker();

    // Drain every tracked periodic ctx: reclaim its pending wheel reference and
    // drop its registry reference. Without this the wheel's pending references
    // would be silently dropped by `hew_timer_wheel_free` (which frees entry
    // nodes but not their `data`), leaking the PeriodicCtx allocations.
    let all: Vec<Arc<PeriodicCtx>> = ACTOR_TIMERS.access(|lock| {
        lock.take()
            .map(|map| map.into_values().flatten().collect())
            .unwrap_or_default()
    });
    for ctx in &all {
        ctx.cancelled.store(true, Ordering::SeqCst);
        reclaim_pending_wheel_ref(ctx);
    }
    drop(all);

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
    use crate::timer_wheel::hew_timer_wheel_schedule;
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
            state_drop_fn: None,
            state_clone_fn: None,
            terminate_called: AtomicBool::new(false),
            terminate_finished: AtomicBool::new(false),
            dispatch_active: AtomicBool::new(false),
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
            suspended_cont: AtomicPtr::new(std::ptr::null_mut()),
            cont_tag: AtomicI32::new(crate::internal::types::ContTag::Empty as i32),
            pending_wake: AtomicBool::new(false),
            suspended_reply_channel: AtomicPtr::new(std::ptr::null_mut()),
            suspended_cancel_token: AtomicPtr::new(std::ptr::null_mut()),
            runtime_id: crate::runtime_id::RuntimeId::DEFAULT,
            runtime: std::ptr::null(),
            send_pin_count: std::sync::atomic::AtomicU32::new(0),
            gen_sink: AtomicPtr::new(std::ptr::null_mut()),
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
    fn ticker_shutdown_reports_worker_panic() {
        let _guard = TICKER_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        crate::hew_clear_error();

        let handle_mutex = TICKER_HANDLE.get_or_init(|| Mutex::new(None));
        *handle_mutex
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner) =
            Some(std::thread::spawn(|| panic!("ticker intentional panic")));
        TICKER_RUNNING.store(true, Ordering::SeqCst);

        shutdown_ticker();

        let error_ptr = crate::hew_last_error();
        assert!(
            !error_ptr.is_null(),
            "joining a panicked ticker must record a diagnostic"
        );
        // SAFETY: shutdown_ticker populated this thread's last-error slot.
        let error = unsafe {
            CStr::from_ptr(error_ptr)
                .to_str()
                .expect("last error should be utf-8")
        };
        assert!(
            error.contains("hew timer ticker panicked during shutdown"),
            "unexpected last error: {error}"
        );
        assert!(
            error.contains("ticker intentional panic"),
            "panic payload must be included in the diagnostic: {error}"
        );
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

        // Manually create PeriodicCtx entries and register them (bypassing the
        // timer wheel: pending=None, so no wheel reference exists and
        // cancel_all frees them purely via the registry reference drop).
        let ctx1 = Arc::new(PeriodicCtx {
            actor: actor_ptr,
            msg_type: 0,
            interval_ms: 100,
            wheel: ptr::null_mut(),
            cancelled: AtomicBool::new(false),
            in_flight: AtomicBool::new(false),
            pending: Mutex::new(None),
        });
        let ctx1_probe = Arc::clone(&ctx1);
        register_timer(actor_ptr, ctx1);

        let ctx2 = Arc::new(PeriodicCtx {
            actor: actor_ptr,
            msg_type: 1,
            interval_ms: 200,
            wheel: ptr::null_mut(),
            cancelled: AtomicBool::new(false),
            in_flight: AtomicBool::new(false),
            pending: Mutex::new(None),
        });
        register_timer(actor_ptr, ctx2);

        assert_eq!(timer_count_for_actor(actor_ptr), 2);

        // Act: cancel all timers for this actor. With pending=None there is no
        // wheel reference, so dropping the registry references frees both ctxs.
        cancel_all_timers_for_actor(actor_ptr);

        // The retained probe reference must observe the cancelled store.
        assert!(ctx1_probe.cancelled.load(Ordering::SeqCst));
        drop(ctx1_probe);

        // Registry should be empty for this actor; both ctxs are now freed.
        assert_eq!(timer_count_for_actor(actor_ptr), 0);
    }

    /// Repro for the `freed_by_cb` UAF/double-free (review CRITICAL on
    /// 281ce970): schedule a real periodic so a one-shot is pending in the
    /// global wheel while the ticker thread is live, then cancel. Under the old
    /// two-flag protocol the pending callback fired into freed memory
    /// (heap-use-after-free + double-free under ASan/MallocScribble). The
    /// single-owner Arc protocol must not crash.
    #[test]
    fn cancel_all_with_live_ticker_and_pending_oneshot_no_uaf() {
        let _guard = TICKER_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);

        let mut actor = create_test_actor(50_400);
        let actor_ptr = &raw mut actor;

        // Starts the ticker thread and arms a one-shot 60 ms out.
        // SAFETY: actor_ptr is a valid live stack actor for the test's duration.
        let handle = unsafe { hew_actor_schedule_periodic(actor_ptr, 7, 60) };
        assert!(!handle.is_null());
        assert_eq!(timer_count_for_actor(actor_ptr), 1);

        // Cancel while the ticker is live and the one-shot is still pending.
        cancel_all_timers_for_actor(actor_ptr);
        assert_eq!(timer_count_for_actor(actor_ptr), 0);

        // Let the wheel tick well past the original 60 ms deadline. The pending
        // one-shot must have been removed (its ctx reference reclaimed) so no
        // callback fires into freed memory.
        std::thread::sleep(Duration::from_millis(200));

        shutdown_ticker();
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

    /// When the wheel is empty the ticker parks indefinitely.  Scheduling a
    /// timer must wake it and the timer must fire within a tight window.
    ///
    /// Verifies: parked-empty → schedule fires on time.
    #[test]
    fn tickless_parked_empty_fires_on_schedule() {
        let _guard = TICKER_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);

        TEST_COUNTER.store(0, Ordering::SeqCst);

        // Ensure the ticker is up and the wheel is empty (wheel may retain
        // entries from prior tests; start fresh).
        // SAFETY: test owns teardown of the process-wide timer state via TICKER_TEST_MUTEX.
        unsafe { hew_periodic_shutdown() };
        let tw = global_wheel();
        assert!(!tw.is_null(), "global_wheel must succeed");

        // Allow the ticker to settle into its indefinite park on the empty wheel.
        std::thread::sleep(Duration::from_millis(20));

        // Schedule a ~50 ms timer while the ticker is parked indefinitely.
        let t0 = Instant::now();
        // SAFETY: tw is valid; callback is thread-safe.
        unsafe { hew_timer_wheel_schedule(tw, 50, test_timer_cb, ptr::null_mut()) };

        // Wait up to 250 ms; the timer should fire in ~50 ms after the
        // insert-wakeup.  250 ms gives 5× headroom for a loaded CI runner.
        let deadline = t0 + Duration::from_millis(250);
        while TEST_COUNTER.load(Ordering::SeqCst) == 0 {
            assert!(
                Instant::now() < deadline,
                "timer scheduled while ticker was parked did not fire within 250 ms"
            );
            std::thread::sleep(Duration::from_millis(2));
        }
        let elapsed = t0.elapsed();
        // Must not fire so late that it waited for a full separate park cycle
        // (a park cycle defaults to no wakeup, so 250 ms is already the bound).
        assert!(
            elapsed < Duration::from_millis(250),
            "timer fired too late ({elapsed:?}); insert-wakeup may not be working"
        );

        shutdown_ticker();
    }

    /// While the ticker is parked waiting for a far deadline, inserting a
    /// nearer timer must cause the nearer one to fire on time.
    ///
    /// Verifies: sooner-insert-while-parked wakes the ticker and the near timer
    /// fires at its own deadline (not the far one).
    #[test]
    fn tickless_sooner_insert_fires_before_far_deadline() {
        let _guard = TICKER_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);

        TEST_COUNTER.store(0, Ordering::SeqCst);

        // SAFETY: test owns teardown of the process-wide timer state via TICKER_TEST_MUTEX.
        unsafe { hew_periodic_shutdown() };
        let tw = global_wheel();
        assert!(!tw.is_null(), "global_wheel must succeed");

        // Schedule a far timer (10 s) — the ticker parks for ~10 s.
        // SAFETY: tw is valid.
        unsafe { hew_timer_wheel_schedule(tw, 10_000, test_timer_cb, ptr::null_mut()) };

        // Let the ticker pick up the far deadline and park.
        std::thread::sleep(Duration::from_millis(20));

        // Now insert a near timer (50 ms).  The insert-notify must wake the
        // ticker so it re-parks for 50 ms, not 10 s.
        let t0 = Instant::now();
        // SAFETY: tw is valid.
        unsafe { hew_timer_wheel_schedule(tw, 50, test_timer_cb, ptr::null_mut()) };

        // The near timer must fire within 250 ms.
        let deadline = t0 + Duration::from_millis(250);
        while TEST_COUNTER.load(Ordering::SeqCst) == 0 {
            assert!(
                Instant::now() < deadline,
                "near timer did not fire within 250 ms after sooner insert while parked on far deadline"
            );
            std::thread::sleep(Duration::from_millis(2));
        }
        let elapsed = t0.elapsed();
        assert!(
            elapsed < Duration::from_millis(250),
            "near timer fired too late ({elapsed:?}); sooner-insert wakeup may not be working"
        );

        shutdown_ticker();
    }

    /// The ticker must not spin when the wheel is empty.  We verify this by
    /// checking that no tick-driven callback fires during a quiet period — and
    /// by observing that the condvar `notified` flag stays false (i.e. the
    /// ticker is not being re-woken repeatedly by its own activity).
    ///
    /// Verifies: no idle CPU wakeups when the wheel is empty.
    #[test]
    fn tickless_no_idle_wakeup_when_wheel_empty() {
        let _guard = TICKER_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);

        TEST_COUNTER.store(0, Ordering::SeqCst);

        // SAFETY: test owns teardown of the process-wide timer state via TICKER_TEST_MUTEX.
        unsafe { hew_periodic_shutdown() };
        let tw = global_wheel();
        assert!(!tw.is_null(), "global_wheel must succeed");

        // Let the ticker settle.
        std::thread::sleep(Duration::from_millis(20));

        // The wheel is empty.  Confirm the notified flag is false — the ticker
        // has not re-signalled itself (which would indicate a spin).
        let park = ticker_park();
        let notified = *park
            .mu
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        assert!(
            !notified,
            "ticker_park notified flag should be false when wheel is empty (ticker is parked)"
        );

        // No callback should have fired.
        assert_eq!(
            TEST_COUNTER.load(Ordering::SeqCst),
            0,
            "no timer was scheduled; no callback should have fired"
        );

        shutdown_ticker();
    }
}
