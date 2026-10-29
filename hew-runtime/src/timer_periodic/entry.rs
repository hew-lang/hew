//! Per-registration periodic callback and wheel ownership. Shared unchanged
//! with the verification host, which supplies registry and actor adapters.
use super::{register_timer, unregister_timer};
use crate::actor::HewActor;
use crate::runtime_sync::{AtomicU64, Mutex, SeqCstFlag as AtomicBool};
use crate::timer_wheel::{
    hew_timer_wheel_remove, timer_wheel_schedule_precise_handle, HewTimerHandle, HewTimerWheel,
};
use std::ffi::c_void;
use std::ptr;
use std::sync::atomic::Ordering;
use std::sync::Arc;

/// Heap-allocated context for a periodic timer callback.
///
/// Owned through an [`Arc`]; the actor registry and wheel retain it independently.
pub(super) struct PeriodicCtx {
    pub(super) actor: *mut HewActor,
    pub(super) msg_type: i32,
    pub(super) interval_ns: u64,
    /// Absolute deadline for the next cadence slot.
    pub(super) next_fire_ns: AtomicU64,
    pub(super) wheel: *mut HewTimerWheel,
    /// Set true to stop the timer. Once observed by the callback (under the
    /// `pending` lock at re-arm, or at the Dekker cancelled-check) the timer
    /// will not re-arm and the ctx is released.
    pub(super) cancelled: AtomicBool,
    /// Set true while the callback is between the cancelled-check and the
    /// completion of admission. `cancel_all_timers_for_actor` spin-waits
    /// on this (Dekker protocol) so the actor outlives any in-flight send.
    pub(super) in_flight: AtomicBool,
    /// Handle of the single pending one-shot in the wheel, or `None` while the
    /// callback is executing (the entry has been consumed and not yet
    /// re-armed). The wheel holds one `Arc::into_raw` strong reference for as
    /// long as this is `Some`. Guarded by a mutex so re-arm (callback) and
    /// reclaim (cancel/shutdown) never race on the pending entry.
    pub(super) pending: Mutex<Option<HewTimerHandle>>,
}

// SAFETY: Actor pointers are thread-safe (interior atomics).
unsafe impl Send for PeriodicCtx {}
// SAFETY: All mutable state uses atomics / a mutex; raw pointers are read-only
// after init.
unsafe impl Sync for PeriodicCtx {}

impl PeriodicCtx {
    pub(super) fn mark_cancelled(&self) {
        self.cancelled.store(true, Ordering::SeqCst);
    }

    pub(super) fn drain(self: &Arc<Self>) {
        while self.in_flight.load(Ordering::SeqCst) {
            crate::runtime_sync::spin_loop();
        }
        reclaim_pending_wheel_ref(self);
    }
}

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
    #[cfg(target_arch = "wasm32")]
    let admit = crate::driver::guest_ingress_open();
    #[cfg(not(target_arch = "wasm32"))]
    let admit = true;
    if admit {
        // SAFETY: actor is valid — cancel_all_timers_for_actor is spinning
        // on our in_flight guard and won't free the actor until we clear it.
        unsafe {
            crate::actor::actor_send_result_internal_reply(
                ctx.actor,
                ctx.msg_type,
                ptr::null_mut(),
                0,
                ptr::null_mut(),
                crate::actor_caller::HewCallerStamp::timer(),
            );
        }
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
    let mut next_fire_ns = ctx
        .next_fire_ns
        .load(Ordering::SeqCst)
        .saturating_add(ctx.interval_ns);
    // SAFETY: ctx owns a live wheel for the lifetime of the periodic timer.
    let cursor_ns = unsafe { crate::timer_wheel::timer_wheel_cursor(ctx.wheel) }.saturated_nanos();
    if next_fire_ns < cursor_ns {
        let skipped_slots = cursor_ns.saturating_sub(next_fire_ns) / ctx.interval_ns + 1;
        next_fire_ns = next_fire_ns.saturating_add(ctx.interval_ns.saturating_mul(skipped_slots));
    }
    ctx.next_fire_ns.store(next_fire_ns, Ordering::SeqCst);

    // Hand a fresh strong reference to the wheel for the next one-shot.
    let next = Arc::into_raw(Arc::clone(&ctx)).cast::<c_void>().cast_mut();
    // SAFETY: wheel is valid; `next` stays valid until the entry fires or is
    // removed (both reclaim it exactly once).
    let handle = unsafe {
        timer_wheel_schedule_precise_handle(
            ctx.wheel,
            crate::timer_deadline::Deadline::from_nanos(next_fire_ns),
            periodic_timer_cb,
            next,
        )
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

/// Reclaim and drop the wheel's pending strong reference for `ctx`, if any.
///
/// Takes the pending handle under the ctx lock and removes the entry from the
/// wheel. If the entry was still pending, `hew_timer_wheel_remove` returns its
/// `data` pointer (the `Arc::into_raw` reference handed to the wheel) which we
/// reclaim and drop. If the entry had already fired or been collected for
/// firing, the removal finds nothing and the firing callback reclaims that
/// reference instead — in either case the reference is reclaimed exactly once.
pub(super) fn reclaim_pending_wheel_ref(ctx: &Arc<PeriodicCtx>) {
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

/// Schedule one native periodic timer on a specific wheel and clock sample.
///
/// # Safety
///
/// `actor` and `tw` must be valid and `interval_ns` must be non-zero.
pub(super) unsafe fn schedule_periodic_ns_on_wheel(
    actor: *mut HewActor,
    msg_type: i32,
    interval_ns: u64,
    tw: *mut HewTimerWheel,
    now_ns: u64,
) -> *mut c_void {
    let Some(next_fire_ns) = now_ns.checked_add(interval_ns) else {
        return ptr::null_mut();
    };

    let ctx = Arc::new(PeriodicCtx {
        actor,
        msg_type,
        interval_ns,
        next_fire_ns: AtomicU64::new(next_fire_ns),
        wheel: tw,
        cancelled: AtomicBool::new(false),
        in_flight: AtomicBool::new(false),
        pending: Mutex::new(None),
    });
    // Publish registration and its first handle under the same lock used by
    // firing/re-arm and cancellation. The wheel may collect this first entry
    // immediately; its callback must not replace the handle before we record it.
    let mut pending = ctx
        .pending
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
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
    let wheel_handle = unsafe {
        timer_wheel_schedule_precise_handle(
            tw,
            crate::timer_deadline::Deadline::from_nanos(next_fire_ns),
            periodic_timer_cb,
            data,
        )
    };
    if wheel_handle.entry.is_null() {
        // Scheduling failed: reclaim the wheel reference and the registry
        // reference so nothing leaks, and report failure.
        // SAFETY: `data` is the reference we just created; the wheel did not
        // take ownership (null handle).
        drop(unsafe { Arc::from_raw(data.cast::<PeriodicCtx>()) });
        unregister_timer(actor, handle_ptr as usize);
        return ptr::null_mut();
    }
    *pending = Some(wheel_handle);

    handle_ptr
}

/// Cancel an owned registration and wait out its actor access.
pub(super) fn cancel(ctx: &Arc<PeriodicCtx>) {
    let actor = ctx.actor;
    ctx.mark_cancelled();
    ctx.drain();
    unregister_timer(actor, Arc::as_ptr(ctx) as usize);
}
