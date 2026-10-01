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
use std::sync::atomic::{AtomicBool, AtomicPtr, AtomicU64, Ordering};
use std::sync::{Arc, Mutex, RwLock};

use crate::lifetime::PoisonSafe;

use crate::actor::{hew_actor_send, HewActor};
use crate::timer_wheel::{
    hew_timer_wheel_free, hew_timer_wheel_remove, timer_wheel_cursor_ms,
    timer_wheel_schedule_at_handle, HewTimerHandle, HewTimerWheel,
};

// ---------------------------------------------------------------------------
// Global timer wheel (lazy-initialised singleton)
// ---------------------------------------------------------------------------

/// The process timer wheel, created on first use. The reactor thread reads it
/// every loop turn to bound its poll and fire due timers.
static GLOBAL_WHEEL: AtomicPtr<HewTimerWheel> = AtomicPtr::new(ptr::null_mut());
/// Serializes creation and teardown of `GLOBAL_WHEEL`.
static WHEEL_INIT: Mutex<()> = Mutex::new(());
static PERIODIC_ADMISSION: RwLock<()> = RwLock::new(());
static PERIODIC_ACCEPTING: AtomicBool = AtomicBool::new(true);

/// Return (or create) the process timer wheel, ensuring something ticks it.
///
/// On the threaded runtime the reactor ticks it: its poll timeout is the
/// wheel's next deadline. The single-thread driver has no reactor loop to
/// park: it drives its own wheel between steps, so nothing is started here.
pub(crate) fn global_wheel() -> *mut HewTimerWheel {
    if crate::driver::active() {
        return crate::driver::global_wheel();
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        native_global_wheel()
    }
    #[cfg(target_arch = "wasm32")]
    {
        unreachable!("wasm32 always runs on the driver")
    }
}

#[cfg(not(target_arch = "wasm32"))]
fn native_global_wheel() -> *mut HewTimerWheel {
    let mut wheel = GLOBAL_WHEEL.load(Ordering::Acquire);
    if wheel.is_null() {
        let _init = WHEEL_INIT
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        wheel = GLOBAL_WHEEL.load(Ordering::Acquire);
        if wheel.is_null() {
            // SAFETY: hew_timer_wheel_new has no preconditions.
            wheel = unsafe { crate::timer_wheel::hew_timer_wheel_new() };
            if wheel.is_null() {
                return ptr::null_mut();
            }
            crate::timer_wheel::register_insert_hook(wheel_inserted);
            GLOBAL_WHEEL.store(wheel, Ordering::Release);
        }
    }
    if !crate::reactor::ensure_reactor_started() {
        crate::set_last_error("hew timer wheel: the I/O reactor could not start");
        return ptr::null_mut();
    }
    wheel
}

/// The process wheel if one exists, for the reactor loop. Never creates one.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn process_wheel() -> *mut HewTimerWheel {
    GLOBAL_WHEEL.load(Ordering::Acquire)
}

/// Wake the reactor when an insert into the process wheel lands before its
/// current sleep limit. Inserts into other wheels (the driver's) need no wake.
#[cfg(not(target_arch = "wasm32"))]
fn wheel_inserted(wheel: *mut HewTimerWheel, deadline_ms: u64) {
    if wheel == GLOBAL_WHEEL.load(Ordering::Acquire) {
        crate::reactor::timer_inserted(deadline_ms);
    }
}

/// Return the process-wide timer wheel, lazily creating it and ensuring the
/// reactor ticks it. This is the deadline-schedule target for
/// `await … | after d`: codegen passes the returned wheel to
/// `hew_await_cancel_schedule_deadline_ms`. Returns null only if the wheel or
/// the reactor could not be created (the caller then skips arming the
/// deadline and the await behaves as an un-deadlined suspend).
///
/// # Safety
///
/// FFI entry point. The returned pointer is owned by the runtime singleton and
/// outlives every scheduled deadline; callers must not free it.
#[no_mangle]
pub unsafe extern "C" fn hew_global_timer_wheel() -> *mut HewTimerWheel {
    global_wheel()
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

/// Close periodic-timer admission and cancel every timer already registered.
///
/// A periodic tick becomes live work only when the ticker claims it for
/// callback delivery. Shutdown closes admission, waits for callbacks already
/// claimed, and cancels every pending entry, including entries that are due but
/// not yet claimed.
///
/// Shutdown calls this before observing scheduler quiescence. The admission
/// lock orders a concurrent schedule against the registry drain: a schedule
/// either publishes its wheel entry and registry reference before the close
/// (and is cancelled here), or observes the closed gate and publishes nothing.
/// Existing callbacks are waited out, so after this function returns no
/// periodic callback can enqueue actor work.
///
/// Registry references stay published until every callback has drained. This
/// lets a concurrent actor-local teardown find its timers and perform the same
/// cancel/wait/reclaim protocol; absence from the registry therefore continues
/// to mean that another path has already made the timer safe.
///
/// The shared timer wheel and ticker remain alive because suspended `sleep`
/// activations still need them during the drain and parked-frame retirement.
pub(crate) fn quiesce_periodic_timers() {
    let all: Vec<Arc<PeriodicCtx>> = {
        let _admission = PERIODIC_ADMISSION
            .write()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        PERIODIC_ACCEPTING.store(false, Ordering::Release);
        ACTOR_TIMERS.access(|lock| {
            lock.as_ref()
                .map(|map| map.values().flatten().cloned().collect())
                .unwrap_or_default()
        })
    };

    for ctx in &all {
        ctx.cancelled.store(true, Ordering::SeqCst);
    }
    for ctx in &all {
        while ctx.in_flight.load(Ordering::SeqCst) {
            std::hint::spin_loop();
        }
    }
    for ctx in &all {
        reclaim_pending_wheel_ref(ctx);
    }
    for ctx in &all {
        unregister_timer(ctx.actor, Arc::as_ptr(ctx) as usize);
    }
}

/// Open periodic-timer admission for a newly installed runtime generation.
pub(crate) fn reset_periodic_admission() {
    let _admission = PERIODIC_ADMISSION
        .write()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    PERIODIC_ACCEPTING.store(true, Ordering::Release);
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
    /// Absolute deadline for the next cadence slot.
    next_fire_ms: AtomicU64,
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

    // Fire one message for this due cadence slot. Re-arming preserves the
    // absolute phase while dropping any other slots missed during a cursor
    // stall, so the ticker cannot spin through overdue intervals.
    //
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
    let mut next_fire_ms = ctx
        .next_fire_ms
        .load(Ordering::SeqCst)
        .saturating_add(ctx.interval_ms);
    // SAFETY: ctx owns a live wheel for the lifetime of the periodic timer.
    let cursor_ms = unsafe { timer_wheel_cursor_ms(ctx.wheel) };
    if next_fire_ms < cursor_ms {
        let skipped_slots = cursor_ms.saturating_sub(next_fire_ms) / ctx.interval_ms + 1;
        next_fire_ms = next_fire_ms.saturating_add(ctx.interval_ms.saturating_mul(skipped_slots));
    }
    ctx.next_fire_ms.store(next_fire_ms, Ordering::SeqCst);

    // Hand a fresh strong reference to the wheel for the next one-shot.
    let next = Arc::into_raw(Arc::clone(&ctx)).cast::<c_void>().cast_mut();
    // SAFETY: wheel is valid; `next` stays valid until the entry fires or is
    // removed (both reclaim it exactly once).
    let handle =
        unsafe { timer_wheel_schedule_at_handle(ctx.wheel, next_fire_ms, periodic_timer_cb, next) };
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
/// On an absolute cadence every `interval_ms` milliseconds, sends a
/// zero-payload message with type `msg_type` to the actor's dispatch function.
/// A late timer fires once and drops any cadence slots missed while the wheel
/// cursor was stalled. The timer repeats until the actor is freed or the
/// handle is cancelled.
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

    let _admission = PERIODIC_ADMISSION
        .read()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    if !PERIODIC_ACCEPTING.load(Ordering::Acquire) {
        return ptr::null_mut();
    }

    let tw = global_wheel();
    if tw.is_null() {
        return ptr::null_mut();
    }

    // SAFETY: `actor` and `tw` were validated above; hew_now_ms has no
    // preconditions on native targets.
    unsafe {
        schedule_periodic_on_wheel(actor, msg_type, interval_ms, tw, crate::clock::hew_now_ms())
    }
}

/// Schedule one native periodic timer on a specific wheel and clock sample.
///
/// # Safety
///
/// `actor` and `tw` must be valid and `interval_ms` must be non-zero.
unsafe fn schedule_periodic_on_wheel(
    actor: *mut HewActor,
    msg_type: i32,
    interval_ms: u64,
    tw: *mut HewTimerWheel,
    now_ms: u64,
) -> *mut c_void {
    let next_fire_ms = now_ms.saturating_add(interval_ms);

    let ctx = Arc::new(PeriodicCtx {
        actor,
        msg_type,
        interval_ms,
        next_fire_ms: AtomicU64::new(next_fire_ms),
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
        unsafe { timer_wheel_schedule_at_handle(tw, next_fire_ms, periodic_timer_cb, data) };
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

/// Serializes tests that create, tick or free the process wheel. Declared at
/// module level so coupled tests in other modules can acquire the same guard.
#[cfg(test)]
pub(crate) static TICKER_TEST_MUTEX: Mutex<()> = Mutex::new(());

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
    // Stop the reactor, which ticks the wheel. After this returns no callback
    // is executing, so the only remaining strong references to any
    // PeriodicCtx are the registry references and the wheel's pending
    // one-shot references.
    #[cfg(not(target_arch = "wasm32"))]
    crate::reactor::reactor_shutdown();

    // Drain every tracked periodic ctx before freeing the wheel. Without this
    // the wheel would free entry nodes but not their `data` references.
    quiesce_periodic_timers();

    let _init = WHEEL_INIT
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    let tw = GLOBAL_WHEEL.swap(ptr::null_mut(), Ordering::AcqRel);
    if !tw.is_null() {
        // SAFETY: tw was allocated by hew_timer_wheel_new, and the reactor
        // that ticks it has been joined.
        unsafe {
            hew_timer_wheel_free(tw);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::internal::types::HewActorState;
    use crate::timer_wheel::{
        hew_timer_wheel_new, hew_timer_wheel_next_deadline_ms, hew_timer_wheel_schedule,
    };
    use std::sync::atomic::{AtomicI32, AtomicU32};
    use std::time::{Duration, Instant};

    // TICKER_TEST_MUTEX is declared at module level (pub(crate)) and brought
    // in by `use super::*` above; coupled tests in other modules import it.

    static TEST_COUNTER: AtomicU32 = AtomicU32::new(0);

    // Mock timer callback that increments a counter
    unsafe extern "C" fn test_timer_cb(_data: *mut c_void) {
        TEST_COUNTER.fetch_add(1, Ordering::SeqCst);
    }

    fn create_test_actor(id: u64) -> HewActor {
        HewActor {
            dispatch_ownership: crate::actor::HewDispatchOwnership::CopiedPayload,
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
            local_pid_id: crate::lifetime::local_handles::HewLocalPidId::INVALID,
            spawn_serial: id,
            sys_dispatch: None,
            state_drop_consumed: AtomicBool::new(false),
            state_drop_borrowed: AtomicBool::new(false),
            parked_ask_channel: AtomicPtr::new(std::ptr::null_mut()),
            checked_invocation: AtomicPtr::new(std::ptr::null_mut()),
            #[cfg(not(target_arch = "wasm32"))]
            pending_external_trap_code: AtomicI32::new(0),
            #[cfg(not(target_arch = "wasm32"))]
            native_completion: None,
        }
    }

    #[test]
    fn late_fire_skips_missed_slots_at_anchored_cadence() {
        let _guard = TICKER_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let mut actor = create_test_actor(50_050);
        actor
            .actor_state
            .store(HewActorState::Stopped as i32, Ordering::SeqCst);
        let actor_ptr = &raw mut actor;

        // SAFETY: the test owns this wheel for its complete lifetime.
        let wheel = unsafe { hew_timer_wheel_new() };
        assert!(!wheel.is_null());
        // SAFETY: `wheel` is live and test-owned.
        let now = unsafe { crate::timer_wheel::timer_wheel_cursor_ms(wheel) };
        // SAFETY: actor and wheel remain live until the timer is cancelled.
        let handle = unsafe { schedule_periodic_on_wheel(actor_ptr, 7, 10, wheel, now) };
        assert!(!handle.is_null());

        // SAFETY: the registry keeps this context live through cancellation.
        let next_fire = unsafe { &(*handle.cast::<PeriodicCtx>()).next_fire_ms };
        let first_fire = next_fire.load(Ordering::SeqCst);
        let late_tick = first_fire.saturating_add(25);

        // A late tick fires once and skips missed slots without drifting from
        // the original cadence phase.
        // SAFETY: wheel and callback payload remain live throughout.
        unsafe {
            assert_eq!(crate::timer_wheel::timer_wheel_tick_to(wheel, late_tick), 1);
            assert_eq!(next_fire.load(Ordering::SeqCst), first_fire + 30);
            assert_eq!(crate::timer_wheel::timer_wheel_tick_to(wheel, late_tick), 0);
            assert_eq!(
                crate::timer_wheel::timer_wheel_tick_to(wheel, first_fire + 30),
                1
            );
            assert_eq!(next_fire.load(Ordering::SeqCst), first_fire + 40);
            hew_actor_cancel_periodic(handle);
            hew_timer_wheel_free(wheel);
        }
    }

    #[test]
    fn cursor_stall_drops_missed_intervals_without_losing_phase() {
        let _guard = TICKER_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let mut actor = create_test_actor(50_051);
        actor
            .actor_state
            .store(HewActorState::Stopped as i32, Ordering::SeqCst);
        let actor_ptr = &raw mut actor;

        // SAFETY: the test owns this wheel for its complete lifetime.
        let wheel = unsafe { hew_timer_wheel_new() };
        assert!(!wheel.is_null());
        // Move the initially fresh cursor before modelling the stall; otherwise
        // a zero-based cursor can make the stall setup a no-op.
        // SAFETY: wheel is live and exclusively owned by this test.
        unsafe { crate::timer_wheel::timer_wheel_advance_cursor_for_test(wheel, 1) };
        // SAFETY: wheel is live and test-owned.
        let now = unsafe { crate::timer_wheel::timer_wheel_cursor_ms(wheel) };
        // SAFETY: actor and wheel remain live until the timer is cancelled.
        let handle = unsafe { schedule_periodic_on_wheel(actor_ptr, 7, 1, wheel, now) };
        assert!(!handle.is_null());

        // SAFETY: the registry keeps this context live through cancellation.
        let next_fire = unsafe { &(*handle.cast::<PeriodicCtx>()).next_fire_ms };
        let first_fire = next_fire.load(Ordering::SeqCst);
        let stalled_cursor = first_fire + 5_000;

        // SAFETY: wheel and callback payload remain live throughout.
        unsafe {
            assert_eq!(
                crate::timer_wheel::timer_wheel_tick_to(wheel, stalled_cursor),
                1
            );
            assert_eq!(next_fire.load(Ordering::SeqCst), stalled_cursor + 1);
            assert_eq!(
                crate::timer_wheel::timer_wheel_tick_to(wheel, stalled_cursor),
                0
            );
            assert_eq!(
                crate::timer_wheel::timer_wheel_tick_to(wheel, stalled_cursor + 1),
                1
            );
            assert_eq!(next_fire.load(Ordering::SeqCst), stalled_cursor + 2);
            hew_actor_cancel_periodic(handle);
            hew_timer_wheel_free(wheel);
        }
    }

    fn wait_for_counter(limit: Duration, context: &str) {
        let deadline = Instant::now() + limit;
        while TEST_COUNTER.load(Ordering::SeqCst) == 0 {
            assert!(Instant::now() < deadline, "{context}");
            std::thread::sleep(Duration::from_millis(2));
        }
    }

    /// The reactor ticks the process wheel, joins promptly on shutdown and
    /// starts again for the next wheel user.
    #[test]
    fn reactor_ticks_the_process_wheel_and_restarts_after_shutdown() {
        let _guard = TICKER_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        for round in 0..2 {
            TEST_COUNTER.store(0, Ordering::SeqCst);
            let tw = global_wheel();
            assert!(!tw.is_null());
            // SAFETY: tw is a valid timer wheel pointer from global_wheel().
            unsafe { hew_timer_wheel_schedule(tw, 5, test_timer_cb, ptr::null_mut()) };
            wait_for_counter(
                Duration::from_millis(500),
                "timer did not fire within 500 ms",
            );
            let started = Instant::now();
            // SAFETY: the test owns the process wheel under TICKER_TEST_MUTEX.
            unsafe { hew_periodic_shutdown() };
            assert!(
                started.elapsed() < Duration::from_millis(500),
                "round {round}: reactor should join promptly"
            );
        }
    }

    #[test]
    fn cancel_all_timers_marks_contexts_cancelled() {
        let _guard = TICKER_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let mut actor = create_test_actor(50_100);
        let actor_ptr = &raw mut actor;

        // Manually create PeriodicCtx entries and register them (bypassing the
        // timer wheel: pending=None, so no wheel reference exists and
        // cancel_all frees them purely via the registry reference drop).
        let ctx1 = Arc::new(PeriodicCtx {
            actor: actor_ptr,
            msg_type: 0,
            interval_ms: 100,
            next_fire_ms: AtomicU64::new(100),
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
            next_fire_ms: AtomicU64::new(200),
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
        reset_periodic_admission();

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

        crate::reactor::reactor_shutdown();
    }

    #[test]
    fn shutdown_quiescence_closes_periodic_admission_without_stopping_shared_timers() {
        let _guard = TICKER_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);

        // SAFETY: this test serializes ownership of the process-wide timer state.
        unsafe { hew_periodic_shutdown() };
        reset_periodic_admission();

        let mut actor = create_test_actor(50_401);
        let actor_ptr = &raw mut actor;
        // SAFETY: actor_ptr remains live until the timer subsystem is reset.
        let handle = unsafe { hew_actor_schedule_periodic(actor_ptr, 7, 60_000) };
        assert!(!handle.is_null());
        assert_eq!(timer_count_for_actor(actor_ptr), 1);

        quiesce_periodic_timers();

        assert_eq!(timer_count_for_actor(actor_ptr), 0);
        assert!(
            crate::reactor::reactor_running(),
            "shutdown quiescence must leave the reactor that ticks sleeps alive"
        );
        // SAFETY: actor_ptr is still live, but the shutdown admission gate must
        // reject a timer that races after periodic quiescence.
        let rejected = unsafe { hew_actor_schedule_periodic(actor_ptr, 8, 60_000) };
        assert!(
            rejected.is_null(),
            "periodic scheduling must stay closed after shutdown quiescence"
        );

        // A complete timer shutdown followed by wheel recreation starts a fresh
        // runtime generation and reopens admission.
        // SAFETY: this test serializes ownership of the process-wide timer state.
        unsafe { hew_periodic_shutdown() };
        reset_periodic_admission();
        // SAFETY: actor_ptr remains live through cancellation below.
        let restarted = unsafe { hew_actor_schedule_periodic(actor_ptr, 9, 60_000) };
        assert!(!restarted.is_null());
        // SAFETY: restarted is the live handle returned immediately above.
        unsafe { hew_actor_cancel_periodic(restarted) };
        // SAFETY: final serialized cleanup for this test.
        unsafe { hew_periodic_shutdown() };
    }

    #[test]
    fn shutdown_cancels_due_but_unclaimed_periodic_entry() {
        let _guard = TICKER_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);

        // Start from a fresh global wheel and leave the ticker parked on a far
        // deadline. The test advances only the wheel cursor, so the entry can
        // be made due without letting the ticker claim it.
        // SAFETY: this test serializes ownership of the process-wide timer state.
        unsafe { hew_periodic_shutdown() };
        reset_periodic_admission();

        let mut actor = create_test_actor(50_403);
        let actor_ptr = &raw mut actor;
        // SAFETY: actor_ptr remains live through the serialized shutdown.
        let handle = unsafe { hew_actor_schedule_periodic(actor_ptr, 7, 60_000) };
        assert!(!handle.is_null());

        let ctx = ACTOR_TIMERS.access(|lock| {
            Arc::clone(
                lock.as_ref()
                    .and_then(|map| map.get(&(actor_ptr as usize)))
                    .and_then(|timers| timers.first())
                    .expect("scheduled timer must be registered"),
            )
        });
        let wheel = ctx.wheel;
        let first_fire = ctx.next_fire_ms.load(Ordering::SeqCst);
        // SAFETY: `wheel` is the live wheel owned by the periodic context.
        let cursor = unsafe { timer_wheel_cursor_ms(wheel) };
        assert!(first_fire > cursor);

        // Let the ticker settle into its far-deadline park before making the
        // entry due. No insertion notification occurs for this cursor change.
        std::thread::sleep(Duration::from_millis(20));
        // SAFETY: the ticker is parked and the test owns the wheel cursor.
        unsafe {
            crate::timer_wheel::timer_wheel_advance_cursor_for_test(
                wheel,
                first_fire.saturating_sub(cursor).saturating_add(1),
            );
        }
        // SAFETY: `wheel` remains live until the serialized shutdown below.
        assert_eq!(unsafe { hew_timer_wheel_next_deadline_ms(wheel) }, 0);
        assert_eq!(ctx.next_fire_ms.load(Ordering::SeqCst), first_fire);

        // The due entry is still pending and unclaimed. Shutdown must cancel
        // it rather than silently delivering the callback.
        // SAFETY: this test owns the process-wide timer state.
        unsafe { hew_periodic_shutdown() };

        assert_eq!(ctx.next_fire_ms.load(Ordering::SeqCst), first_fire);
        assert!(ctx.cancelled.load(Ordering::SeqCst));
        assert_eq!(timer_count_for_actor(actor_ptr), 0);
        assert_eq!(Arc::strong_count(&ctx), 1, "pending wheel reference leaked");
    }

    #[test]
    fn shutdown_quiescence_keeps_registry_visible_until_callbacks_drain() {
        let _guard = TICKER_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);

        // SAFETY: this test serializes ownership of the process-wide timer state.
        unsafe { hew_periodic_shutdown() };
        reset_periodic_admission();

        let mut actor = create_test_actor(50_402);
        let actor_ptr = &raw mut actor;
        // SAFETY: actor_ptr remains live until the quiescer and final cleanup join.
        let handle = unsafe { hew_actor_schedule_periodic(actor_ptr, 7, 60_000) };
        assert!(!handle.is_null());
        let ctx = ACTOR_TIMERS.access(|lock| {
            Arc::clone(
                lock.as_ref()
                    .and_then(|map| map.get(&(actor_ptr as usize)))
                    .and_then(|timers| timers.first())
                    .expect("scheduled timer must be registered"),
            )
        });

        // Model a callback that passed its cancellation check and still owns the
        // actor pointer. Global quiescence must keep the registry entry published
        // until this in-flight guard clears, so actor-local teardown cannot mistake
        // the timer for already-safe.
        ctx.in_flight.store(true, Ordering::SeqCst);
        let quiescer = std::thread::spawn(quiesce_periodic_timers);
        while PERIODIC_ACCEPTING.load(Ordering::Acquire) {
            std::hint::spin_loop();
        }
        let visible_while_draining = timer_count_for_actor(actor_ptr) == 1;

        ctx.in_flight.store(false, Ordering::SeqCst);
        quiescer.join().expect("periodic quiescer must finish");

        assert!(
            visible_while_draining,
            "registry absence must never precede callback cancellation and drain"
        );
        assert_eq!(timer_count_for_actor(actor_ptr), 0);
        // SAFETY: final serialized cleanup for this test.
        unsafe { hew_periodic_shutdown() };
    }

    #[test]
    fn cancel_all_timers_no_timers_does_not_panic() {
        let _guard = TICKER_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let mut actor = create_test_actor(50_200);
        let actor_ptr = &raw mut actor;
        // Should be a no-op, not panic.
        cancel_all_timers_for_actor(actor_ptr);
        assert_eq!(timer_count_for_actor(actor_ptr), 0);
    }

    /// When the wheel is empty the reactor waits with no timeout. Scheduling a
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

        // Allow the reactor to settle into its unbounded wait on the empty wheel.
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

        crate::reactor::reactor_shutdown();
    }

    /// While the reactor waits for a far deadline, inserting a
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

        // Schedule a far timer (10 s): the reactor waits for ~10 s.
        // SAFETY: tw is valid.
        unsafe { hew_timer_wheel_schedule(tw, 10_000, test_timer_cb, ptr::null_mut()) };

        // Let the reactor pick up the far deadline and wait.
        std::thread::sleep(Duration::from_millis(20));

        // Now insert a near timer (50 ms). The insert hook must wake the
        // reactor so it waits 50 ms, not 10 s.
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

        crate::reactor::reactor_shutdown();
    }

    /// The reactor must not spin when the wheel is empty: with nothing to
    /// wait for, a quiet period passes without a single loop turn.
    #[test]
    fn tickless_no_idle_wakeup_when_wheel_empty() {
        let _guard = TICKER_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let _reactor = crate::reactor::REACTOR_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        // SAFETY: test owns teardown of the process-wide timer state via TICKER_TEST_MUTEX.
        unsafe { hew_periodic_shutdown() };
        let tw = global_wheel();
        assert!(!tw.is_null(), "global_wheel must succeed");
        std::thread::sleep(Duration::from_millis(20));
        let turns = crate::reactor::loop_turns_for_test();
        std::thread::sleep(Duration::from_millis(100));
        assert_eq!(
            crate::reactor::loop_turns_for_test(),
            turns,
            "an empty wheel and no I/O must leave the reactor asleep"
        );
        crate::reactor::reactor_shutdown();
    }
}
