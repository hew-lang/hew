//! Cooperative periodic timer support for WASM targets.
//!
//! Uses the same `HewTimerWheel` as the native path, sourcing current time
//! via the cfg-split `hew_now_ms()` already present in the runtime.
//! This replaces the former O(n²) sorted-Vec `PERIODIC_QUEUE`.
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

use std::collections::HashMap;
use std::ffi::c_void;
use std::ptr;
use std::sync::atomic::Ordering;

use crate::actor::{self, HewActor};
use crate::internal::types::HewActorState;
use crate::mailbox_wasm::HewMailboxWasm;
use crate::timer_wheel::{hew_timer_wheel_remove, hew_timer_wheel_schedule_handle, HewTimerHandle};

// ---------------------------------------------------------------------------
// Per-timer context
// ---------------------------------------------------------------------------

/// Heap-allocated context for one live periodic timer.
///
/// The pointer returned by [`hew_actor_schedule_periodic`] **is** this
/// struct cast to `*mut c_void`; it serves as both the opaque user handle and
/// the `data` payload passed to `wasm_periodic_cb`.
///
/// Tests read `next_fire_ms` through `(*handle.cast::<WasmPeriodicCtx>()).next_fire_ms`.
pub(crate) struct WasmPeriodicCtx {
    /// Owning actor; kept for registry lookup on actor death.
    pub(crate) actor: *mut HewActor,
    /// Message type delivered on each fire.
    pub(crate) msg_type: i32,
    // 4 bytes implicit padding for `interval_ms` alignment.
    /// Repeat interval in milliseconds.
    pub(crate) interval_ms: u64,
    /// Next scheduled fire time in ms; updated by the callback after each fire.
    pub(crate) next_fire_ms: u64,
    /// Handle to the current live wheel entry; updated on re-arm.
    pub(crate) pending_handle: HewTimerHandle,
}

// SAFETY: The cooperative WASM timer wheel is single-threaded; tests
// serialise access with `runtime_test_guard`.
unsafe impl Send for WasmPeriodicCtx {}
// SAFETY: WasmPeriodicCtx is accessed only from a single-threaded WASM
// cooperative scheduler; all fields are guarded by the scheduler's
// single-threaded invariant.
unsafe impl Sync for WasmPeriodicCtx {}

// ---------------------------------------------------------------------------
// Module-level state
// ---------------------------------------------------------------------------

/// Registry: actor address → list of `*mut WasmPeriodicCtx` owned by the wheel.
static mut PERIODIC_CTX_REGISTRY: Option<HashMap<usize, Vec<*mut WasmPeriodicCtx>>> = None;

/// Count of live periodic timer contexts (in wheel or being re-armed).
/// Combined with `WASM_SLEEP_COUNT` in `hew_wasm_sleeping_count`.
static mut WASM_PERIODIC_COUNT: usize = 0;

// ---------------------------------------------------------------------------
// Clock helper — mirrors `scheduler_wasm::hew_now_ms`
// ---------------------------------------------------------------------------

#[cfg(not(target_arch = "wasm32"))]
#[inline]
fn hew_now_ms_periodic() -> u64 {
    // SAFETY: `io_time::hew_now_ms` has no preconditions.
    unsafe { crate::io_time::hew_now_ms() }
}

#[cfg(target_arch = "wasm32")]
#[inline]
fn hew_now_ms_periodic() -> u64 {
    extern "C" {
        fn hew_now_ms() -> u64;
    }
    // SAFETY: `hew_now_ms` is always provided by the WASM host.
    unsafe { hew_now_ms() }
}

// ---------------------------------------------------------------------------
// Registry helpers
// ---------------------------------------------------------------------------

/// Add `ctx` to the per-actor registry.
///
/// # Safety
///
/// `PERIODIC_CTX_REGISTRY` is a mutable static; caller ensures single-threaded access.
unsafe fn register_ctx(actor: *mut HewActor, ctx: *mut WasmPeriodicCtx) {
    #[expect(
        static_mut_refs,
        reason = "single-threaded cooperative WASM scheduler; PERIODIC_CTX_REGISTRY not aliased"
    )]
    // SAFETY: single-threaded; `get_or_insert_with` takes `&mut self`.
    unsafe {
        PERIODIC_CTX_REGISTRY
            .get_or_insert_with(HashMap::new)
            .entry(actor as usize)
            .or_default()
            .push(ctx);
    }
}

/// Remove `ctx` from the per-actor registry (no-op if not present).
///
/// # Safety
///
/// `PERIODIC_CTX_REGISTRY` is a mutable static; caller ensures single-threaded access.
unsafe fn unregister_ctx(actor: *mut HewActor, ctx: *const WasmPeriodicCtx) {
    #[expect(
        static_mut_refs,
        reason = "single-threaded cooperative WASM scheduler; PERIODIC_CTX_REGISTRY not aliased"
    )]
    // SAFETY: single-threaded; `as_mut` takes `&mut self`.
    unsafe {
        if let Some(registry) = PERIODIC_CTX_REGISTRY.as_mut() {
            if let Some(vec) = registry.get_mut(&(actor as usize)) {
                vec.retain(|&p| !ptr::eq(p, ctx));
                if vec.is_empty() {
                    registry.remove(&(actor as usize));
                }
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Liveness helpers
// ---------------------------------------------------------------------------

fn actor_has_live_mailbox(actor: *mut HewActor) -> bool {
    if actor.is_null() {
        return false;
    }
    // SAFETY: non-null pointer; actor_state read is side-effect free.
    let state = unsafe { (*actor).actor_state.load(Ordering::Relaxed) };
    if state == HewActorState::Stopping as i32
        || state == HewActorState::Stopped as i32
        || state == HewActorState::Crashed as i32
    {
        return false;
    }
    // SAFETY: non-null actor; mailbox is a plain field read.
    let mailbox = unsafe { (*actor).mailbox.cast::<HewMailboxWasm>() };
    if mailbox.is_null() {
        return false;
    }
    // SAFETY: mailbox was read from a live actor and is only queried here.
    unsafe { !crate::mailbox_wasm::mailbox_is_closed(mailbox) }
}

unsafe fn send_periodic_message(actor: *mut HewActor, msg_type: i32) {
    // SAFETY: caller verified actor_has_live_mailbox; actor and mailbox are valid.
    let a = unsafe { &*actor };
    // SAFETY: mailbox belongs to actor and is live (verified by actor_has_live_mailbox);
    // wake_wasm_actor shares the same contract.
    unsafe {
        crate::mailbox_wasm::hew_mailbox_send(a.mailbox.cast(), msg_type, ptr::null_mut(), 0);
        actor::wake_wasm_actor(actor);
    }
}

// ---------------------------------------------------------------------------
// Wheel callback
// ---------------------------------------------------------------------------

/// Fires a periodic message, re-arms the wheel entry, and updates `ctx.pending_handle`.
///
/// # Safety
///
/// `data` is a `Box<WasmPeriodicCtx>` whose owning wheel entry has just been
/// unlinked. The actor pointer in `ctx` is live because
/// `cancel_all_timers_for_actor` removes wheel entries before the actor is
/// freed.
unsafe extern "C" fn wasm_periodic_cb(data: *mut c_void) {
    let ctx_ptr = data.cast::<WasmPeriodicCtx>();
    // SAFETY: data is a Box<WasmPeriodicCtx> allocated at schedule time and
    // re-owned here. The actor is live per the safety contract above.
    let ctx = unsafe { &mut *ctx_ptr };

    if !actor_has_live_mailbox(ctx.actor) {
        // Actor died while the timer was pending; free context and uncount.
        // SAFETY: ctx_ptr is uniquely owned (unlinked from wheel by tick).
        unsafe {
            unregister_ctx(ctx.actor, ctx_ptr);
            drop(Box::from_raw(ctx_ptr));
            WASM_PERIODIC_COUNT = WASM_PERIODIC_COUNT.saturating_sub(1);
        }
        return;
    }

    // Fire one message. (Catch-up delivery of multiple missed intervals
    // is intentionally omitted: the wheel fires once per deadline, and
    // the host is responsible for calling hew_wasm_timer_tick frequently
    // enough.  This matches native timer_periodic.rs semantics.)
    // SAFETY: actor_has_live_mailbox verified the actor and mailbox are valid.
    unsafe { send_periodic_message(ctx.actor, ctx.msg_type) };

    // Advance next_fire_ms by one interval so test observers and the
    // next re-arm agree on the expected fire time.
    ctx.next_fire_ms = ctx.next_fire_ms.saturating_add(ctx.interval_ms);

    // Re-arm: schedule interval_ms from the wheel's current position
    // (not from wall-clock now) so timed tests advance predictably.
    // SAFETY: wasm_timer_wheel() is safe to call from a wheel callback (the
    // lock is released before callbacks fire); wasm_periodic_cb re-acquires
    // the lock only to insert the new entry.
    let wheel = unsafe { crate::scheduler_wasm::wasm_timer_wheel() };
    // SAFETY: wheel is valid (guaranteed by wasm_timer_wheel); ctx_ptr is alive
    // and uniquely owned by this callback invocation.
    let new_handle = unsafe {
        hew_timer_wheel_schedule_handle(wheel, ctx.interval_ms, wasm_periodic_cb, ctx_ptr.cast())
    };
    if new_handle.entry.is_null() {
        // Re-arm failed (wheel destroyed or entry OOM) — fail closed.
        // Copy actor before ending the mutable borrow on ctx.
        let actor = ctx.actor;
        // End mutable borrow of *ctx_ptr before Box::from_raw.
        let _ = ctx;
        // SAFETY: unregister/decrement happen before free; single-threaded WASM.
        unsafe {
            unregister_ctx(actor, ctx_ptr);
            WASM_PERIODIC_COUNT = WASM_PERIODIC_COUNT.saturating_sub(1);
            drop(Box::from_raw(ctx_ptr));
        }
        return;
    }
    ctx.pending_handle = new_handle;
}

// ---------------------------------------------------------------------------
// Public interface
// ---------------------------------------------------------------------------

/// Return the number of live periodic timer contexts.
pub(crate) fn pending_periodic_count() -> usize {
    // SAFETY: single-threaded WASM; no concurrent mutation.
    unsafe { WASM_PERIODIC_COUNT }
}

/// Return `true` if the periodic-context registry has been cleared (is `None`).
///
/// Used by the shutdown-assertion helper to verify full teardown.
#[cfg(test)]
#[expect(
    static_mut_refs,
    reason = "single-threaded test; discriminant read, no mutation"
)]
pub(crate) fn periodic_registry_is_none() -> bool {
    // SAFETY: single-threaded test environment; no concurrent mutation.
    unsafe { PERIODIC_CTX_REGISTRY.is_none() }
}

/// Schedule a periodic self-send on the shared WASM timer wheel.
///
/// Returns a `*mut c_void` that is actually a `*mut WasmPeriodicCtx`; the
/// caller stores it and passes it to [`hew_actor_cancel_periodic`].
///
/// # Safety
///
/// `actor` must point to a live Hew actor.
#[cfg_attr(not(test), no_mangle)]
pub unsafe extern "C" fn hew_actor_schedule_periodic(
    actor: *mut HewActor,
    msg_type: i32,
    interval_ms: u64,
) -> *mut c_void {
    if actor.is_null() || interval_ms == 0 {
        return ptr::null_mut();
    }

    let now = hew_now_ms_periodic();
    let next_fire_ms = now.saturating_add(interval_ms);

    let ctx = Box::into_raw(Box::new(WasmPeriodicCtx {
        actor,
        msg_type,
        // no explicit padding field; Rust adds 4 implicit alignment bytes here
        interval_ms,
        next_fire_ms,
        pending_handle: HewTimerHandle::null(),
    }));

    // Schedule on the shared wheel before registering (avoids a half-initialised
    // entry in the registry).
    // SAFETY: wasm_timer_wheel() has no preconditions beyond single-threaded access.
    let wheel = unsafe { crate::scheduler_wasm::wasm_timer_wheel() };
    if wheel.is_null() {
        // Wheel unavailable (OOM on first creation) — fail closed.
        // SAFETY: ctx is exclusively owned (not yet in any registry).
        unsafe { drop(Box::from_raw(ctx)) };
        return ptr::null_mut();
    }
    // SAFETY: wheel is valid (non-null checked above); ctx is exclusively owned.
    let handle = unsafe {
        hew_timer_wheel_schedule_handle(wheel, interval_ms, wasm_periodic_cb, ctx.cast())
    };
    if handle.entry.is_null() {
        // Wheel rejected the insert (entry allocation failure) — fail closed.
        // SAFETY: ctx is exclusively owned (not yet in any registry).
        unsafe { drop(Box::from_raw(ctx)) };
        return ptr::null_mut();
    }
    // SAFETY: ctx was just created; no other reference exists.
    unsafe { (*ctx).pending_handle = handle };

    // Register only after the wheel entry is confirmed live.
    // SAFETY: PERIODIC_CTX_REGISTRY and WASM_PERIODIC_COUNT are single-threaded.
    unsafe {
        register_ctx(actor, ctx);
        WASM_PERIODIC_COUNT += 1;
    }

    ctx.cast()
}

/// Cancel a previously scheduled periodic timer.
///
/// After this call the timer will never fire again and `handle` becomes invalid.
///
/// # Safety
///
/// `handle` must be a non-null pointer returned by [`hew_actor_schedule_periodic`]
/// that has not already been cancelled.
#[cfg_attr(not(test), no_mangle)]
pub unsafe extern "C" fn hew_actor_cancel_periodic(handle: *mut c_void) {
    if handle.is_null() {
        return;
    }

    let ctx_ptr = handle.cast::<WasmPeriodicCtx>();
    // Read fields before any deallocation.
    // SAFETY: ctx_ptr is alive (not yet cancelled or freed).
    let actor = unsafe { (*ctx_ptr).actor };
    // SAFETY: ctx_ptr is alive (not yet cancelled or freed; same guarantee as above).
    let ph = unsafe { (*ctx_ptr).pending_handle };

    // Remove from registry while ctx_ptr is still valid.
    // SAFETY: PERIODIC_CTX_REGISTRY is single-threaded.
    unsafe { unregister_ctx(actor, ctx_ptr) };

    // Remove the wheel entry.  `ph.pending_handle` always reflects the
    // *current* wheel entry (updated by `wasm_periodic_cb` on re-arm).
    // On single-threaded WASM the callback cannot fire between here and the
    // end of this function, so `ph` is stable.
    // SAFETY: wasm_timer_wheel_raw is single-threaded.
    let wheel = unsafe { crate::scheduler_wasm::wasm_timer_wheel_raw() };
    if !wheel.is_null() && !ph.entry.is_null() {
        // SAFETY: wheel is valid and ph.entry is non-null (both checked above).
        let data = unsafe { hew_timer_wheel_remove(wheel, ph.entry, ph.generation) };
        if !data.is_null() {
            // Entry was still in the wheel; data == ctx_ptr, we reclaim the Box.
            // SAFETY: data is the Box<WasmPeriodicCtx> we allocated.
            unsafe { drop(Box::from_raw(ctx_ptr)) };
        }
        // data == null: entry was already collected by a concurrent tick
        // (impossible on single-threaded WASM).  Fall through without free to
        // avoid a double-free.  The count still decrements below.
    } else {
        // Wheel null (scheduler stopped) or schedule failed (null entry).
        // SAFETY: ctx_ptr is still valid — registry removal above did not free it.
        unsafe { drop(Box::from_raw(ctx_ptr)) };
    }

    // SAFETY: single-threaded; count is only mutated from schedule/cancel/callback.
    unsafe { WASM_PERIODIC_COUNT = WASM_PERIODIC_COUNT.saturating_sub(1) };
}

/// Drain all periodic contexts from the registry and remove their wheel entries.
///
/// Called by [`crate::scheduler_wasm::wasm_timers_shutdown_inner`] during
/// scheduler shutdown, **before** the wheel itself is freed.
///
/// # Safety
///
/// Must not race with timer queue mutation; the WASM scheduler is single-threaded.
#[cfg_attr(not(test), no_mangle)]
pub unsafe extern "C" fn hew_periodic_shutdown() {
    // Take ownership of the registry (leaves it as None).
    let mut all_ctxs: Vec<*mut WasmPeriodicCtx> = Vec::new();
    #[expect(
        static_mut_refs,
        reason = "single-threaded cooperative WASM scheduler; PERIODIC_CTX_REGISTRY not aliased"
    )]
    // SAFETY: single-threaded; `take` takes `&mut self` on the mutable static.
    unsafe {
        if let Some(registry) = PERIODIC_CTX_REGISTRY.take() {
            all_ctxs = registry.into_values().flatten().collect();
        }
    }

    // SAFETY: wasm_timer_wheel_raw is single-threaded.
    let wheel = unsafe { crate::scheduler_wasm::wasm_timer_wheel_raw() };
    for ctx_ptr in all_ctxs {
        if ctx_ptr.is_null() {
            continue;
        }
        // SAFETY: ctx_ptr was in the registry and is exclusively owned here.
        let ph = unsafe { (*ctx_ptr).pending_handle };
        if !wheel.is_null() && !ph.entry.is_null() {
            // Remove from wheel; ignore the return value — we free ctx regardless.
            // SAFETY: wheel is valid (non-null checked above); ph is valid (read from ctx_ptr).
            unsafe { hew_timer_wheel_remove(wheel, ph.entry, ph.generation) };
        }
        // SAFETY: exclusively owned Box; ctx_ptr was taken from the registry and
        // will not be freed by any other path after this point.
        unsafe { drop(Box::from_raw(ctx_ptr)) };
    }

    // SAFETY: single-threaded.
    unsafe { WASM_PERIODIC_COUNT = 0 };
}

/// Cancel all periodic timers registered for `actor`.
///
/// Called from `actor.rs` when an actor is stopping or being cleaned up.
///
/// # Safety
///
/// `actor` must be a live actor pointer or one that is about to be freed.
pub(crate) unsafe fn cancel_all_timers_for_actor(actor: *mut HewActor) {
    let ctxs_opt: Option<Vec<*mut WasmPeriodicCtx>>;
    #[expect(
        static_mut_refs,
        reason = "single-threaded cooperative WASM scheduler; PERIODIC_CTX_REGISTRY not aliased"
    )]
    // SAFETY: single-threaded; `as_mut` / `remove` take `&mut self` on the mutable static.
    unsafe {
        ctxs_opt = PERIODIC_CTX_REGISTRY
            .as_mut()
            .and_then(|r| r.remove(&(actor as usize)));
    }
    let Some(ctxs) = ctxs_opt else { return };

    // SAFETY: wasm_timer_wheel_raw is single-threaded.
    let wheel = unsafe { crate::scheduler_wasm::wasm_timer_wheel_raw() };
    for ctx_ptr in ctxs {
        if ctx_ptr.is_null() {
            continue;
        }
        // SAFETY: ctx_ptr was in the registry and is exclusively owned here.
        let ph = unsafe { (*ctx_ptr).pending_handle };
        if !wheel.is_null() && !ph.entry.is_null() {
            // SAFETY: wheel is valid (non-null checked above); ph is valid (read from ctx_ptr).
            unsafe { hew_timer_wheel_remove(wheel, ph.entry, ph.generation) };
        }
        // SAFETY: ctx_ptr is exclusively owned here; taken from ctxs (drained from registry)
        // and not freed elsewhere in this loop.
        unsafe { drop(Box::from_raw(ctx_ptr)) };
        // SAFETY: single-threaded count mutation.
        unsafe { WASM_PERIODIC_COUNT = WASM_PERIODIC_COUNT.saturating_sub(1) };
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicBool, AtomicI32, AtomicPtr, AtomicU32, AtomicU64};

    use crate::actor::{HEW_DEFAULT_REDUCTIONS, HEW_MSG_BUDGET, HEW_PRIORITY_NORMAL};

    static NEXT_TEST_ACTOR_ID: AtomicU64 = AtomicU64::new(1);

    unsafe extern "C-unwind" fn count_dispatch(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _data_size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        // SAFETY: tests install an AtomicU32 state pointer for every TestActor.
        let counter = unsafe { &*(state.cast::<AtomicU32>()) };
        counter.fetch_add(1, Ordering::Relaxed);

        std::ptr::null_mut()
    }

    struct TestActor {
        actor: *mut HewActor,
        counter: *mut AtomicU32,
    }

    impl TestActor {
        fn new() -> Self {
            let counter = Box::into_raw(Box::new(AtomicU32::new(0)));
            // SAFETY: test mailbox constructor has no preconditions.
            let mailbox = unsafe { crate::mailbox_wasm::hew_mailbox_new() }.cast::<c_void>();
            let arena = crate::arena::hew_arena_new();
            let id = NEXT_TEST_ACTOR_ID.fetch_add(1, Ordering::Relaxed);
            let actor = Box::into_raw(Box::new(HewActor {
                sched_link_next: AtomicPtr::new(ptr::null_mut()),
                id,
                state: counter.cast(),
                state_size: size_of::<AtomicU32>(),
                dispatch: Some(count_dispatch),
                mailbox,
                actor_state: AtomicI32::new(HewActorState::Idle as i32),
                budget: AtomicI32::new(HEW_MSG_BUDGET),
                init_state: ptr::null_mut(),
                init_state_size: 0,
                coalesce_key_fn: None,
                terminate_fn: None,
                state_drop_fn: None,
                state_clone_fn: None,
                terminate_called: AtomicBool::new(false),
                terminate_finished: AtomicBool::new(false),
                dispatch_active: AtomicBool::new(false),
                error_code: AtomicI32::new(0),
                supervisor: ptr::null_mut(),
                supervisor_child_index: -1,
                priority: AtomicI32::new(HEW_PRIORITY_NORMAL),
                reductions: AtomicI32::new(HEW_DEFAULT_REDUCTIONS),
                idle_count: AtomicI32::new(0),
                hibernation_threshold: AtomicI32::new(0),
                hibernating: AtomicI32::new(0),
                prof_messages_processed: AtomicU64::new(0),
                prof_processing_time_ns: AtomicU64::new(0),
                arena: arena.cast(),
                suspended_cont: AtomicPtr::new(std::ptr::null_mut()),
                cont_tag: AtomicI32::new(crate::internal::types::ContTag::Empty as i32),
                pending_wake: AtomicBool::new(false),
                suspended_reply_channel: AtomicPtr::new(std::ptr::null_mut()),
                suspended_cancel_token: AtomicPtr::new(std::ptr::null_mut()),
                runtime_id: crate::runtime_id::RuntimeId::DEFAULT,
                runtime: ptr::null(),
            }));
            Self { actor, counter }
        }

        fn count(&self) -> u32 {
            // SAFETY: TestActor owns `counter` for its full lifetime.
            unsafe { (*self.counter).load(Ordering::Relaxed) }
        }
    }

    impl Drop for TestActor {
        fn drop(&mut self) {
            // SAFETY: TestActor exclusively owns its actor, mailbox, arena, and counter allocations.
            unsafe {
                cancel_all_timers_for_actor(self.actor);
                crate::mailbox_wasm::hew_mailbox_free((*self.actor).mailbox.cast());
                crate::arena::hew_arena_free_all((*self.actor).arena.cast());
                let _ = Box::from_raw(self.counter);
                let _ = Box::from_raw(self.actor);
            }
        }
    }

    fn drive_scheduler() {
        loop {
            // SAFETY: tests serialize scheduler access and call this only after init.
            let remaining = unsafe { crate::scheduler_wasm::hew_wasm_sched_tick(16) };
            if remaining == 0 {
                break;
            }
        }
    }

    fn reset_runtime() {
        crate::scheduler_wasm::hew_sched_shutdown();
        // hew_sched_shutdown already calls wasm_timers_shutdown_inner which
        // calls hew_periodic_shutdown; the explicit call here is a no-op but
        // harmless and documents the intent.
        // SAFETY: tests serialize timer-queue access with runtime_test_guard.
        unsafe { hew_periodic_shutdown() };
        crate::scheduler_wasm::hew_sched_init();
    }

    #[test]
    fn periodic_fires_after_interval() {
        let _guard = crate::runtime_test_guard();
        reset_runtime();
        let actor = TestActor::new();
        // SAFETY: actor is a live TestActor-owned pointer.
        let handle = unsafe { hew_actor_schedule_periodic(actor.actor, 7, 10) };
        assert!(!handle.is_null());

        // SAFETY: handle is a live *mut WasmPeriodicCtx; next_fire_ms is valid.
        let first_fire = unsafe { (*handle.cast::<WasmPeriodicCtx>()).next_fire_ms };
        // SAFETY: tests serialize scheduler access and the runtime is initialized.
        unsafe {
            let _ = crate::scheduler_wasm::hew_wasm_timer_tick(first_fire.saturating_sub(1));
        }
        drive_scheduler();
        assert_eq!(actor.count(), 0, "periodic message must not fire early");

        // SAFETY: tests serialize scheduler access and the runtime is initialized.
        unsafe {
            let _ = crate::scheduler_wasm::hew_wasm_timer_tick(first_fire);
        }
        drive_scheduler();
        assert_eq!(actor.count(), 1, "periodic message must fire at deadline");
    }

    #[test]
    fn cancel_before_fire() {
        let _guard = crate::runtime_test_guard();
        reset_runtime();
        let actor = TestActor::new();
        // SAFETY: actor is a live TestActor-owned pointer.
        let handle = unsafe { hew_actor_schedule_periodic(actor.actor, 7, 10) };
        // SAFETY: handle is a live *mut WasmPeriodicCtx.
        let first_fire = unsafe { (*handle.cast::<WasmPeriodicCtx>()).next_fire_ms };

        // SAFETY: handle is active and owned by this test.
        unsafe { hew_actor_cancel_periodic(handle) };
        assert_eq!(
            pending_periodic_count(),
            0,
            "cancel must remove queued timer"
        );

        // SAFETY: tests serialize scheduler access and the runtime is initialized.
        unsafe {
            let _ = crate::scheduler_wasm::hew_wasm_timer_tick(first_fire.saturating_add(20));
        };
        drive_scheduler();
        assert_eq!(actor.count(), 0, "cancelled timer must not fire");
    }

    #[test]
    fn cancel_after_fire_does_not_double_send() {
        let _guard = crate::runtime_test_guard();
        reset_runtime();
        let actor = TestActor::new();
        // SAFETY: actor is a live TestActor-owned pointer.
        let handle = unsafe { hew_actor_schedule_periodic(actor.actor, 7, 10) };
        // SAFETY: handle is a live *mut WasmPeriodicCtx.
        let first_fire = unsafe { (*handle.cast::<WasmPeriodicCtx>()).next_fire_ms };

        // SAFETY: tests serialize scheduler access and the runtime is initialized.
        unsafe {
            let _ = crate::scheduler_wasm::hew_wasm_timer_tick(first_fire);
        };
        drive_scheduler();
        assert_eq!(actor.count(), 1);

        // SAFETY: handle is still alive (re-armed by callback into a new wheel entry).
        unsafe { hew_actor_cancel_periodic(handle) };
        // SAFETY: tests serialize scheduler access and the runtime is initialized.
        unsafe {
            let _ = crate::scheduler_wasm::hew_wasm_timer_tick(first_fire.saturating_add(50));
        };
        drive_scheduler();
        assert_eq!(actor.count(), 1, "cancelled timer must not re-arm");
    }

    #[test]
    fn multiple_actors_each_periodic() {
        let _guard = crate::runtime_test_guard();
        reset_runtime();
        let actor_a = TestActor::new();
        let actor_b = TestActor::new();
        // SAFETY: both actors are live TestActor-owned pointers.
        let handle_a = unsafe { hew_actor_schedule_periodic(actor_a.actor, 1, 20) };
        // SAFETY: both actors are live TestActor-owned pointers.
        let handle_b = unsafe { hew_actor_schedule_periodic(actor_b.actor, 2, 35) };
        // SAFETY: both handles are live *mut WasmPeriodicCtx.
        let fire_a = unsafe { (*handle_a.cast::<WasmPeriodicCtx>()).next_fire_ms };
        // SAFETY: both handles are live *mut WasmPeriodicCtx.
        let fire_b = unsafe { (*handle_b.cast::<WasmPeriodicCtx>()).next_fire_ms };

        // SAFETY: tests serialize scheduler access and the runtime is initialized.
        unsafe {
            let _ = crate::scheduler_wasm::hew_wasm_timer_tick(fire_a);
        };
        drive_scheduler();
        assert_eq!(actor_a.count(), 1);
        assert_eq!(actor_b.count(), 0);

        // SAFETY: tests serialize scheduler access and the runtime is initialized.
        unsafe {
            let _ = crate::scheduler_wasm::hew_wasm_timer_tick(fire_b);
        };
        drive_scheduler();
        assert_eq!(actor_a.count(), 1);
        assert_eq!(actor_b.count(), 1);

        // SAFETY: tests serialize scheduler access and the runtime is initialized.
        unsafe {
            let _ = crate::scheduler_wasm::hew_wasm_timer_tick(fire_a.saturating_add(20));
        };
        drive_scheduler();
        assert_eq!(actor_a.count(), 2);
        assert_eq!(actor_b.count(), 1);
    }

    #[test]
    fn shutdown_clears_all_entries() {
        let _guard = crate::runtime_test_guard();
        reset_runtime();
        let actor_a = TestActor::new();
        let actor_b = TestActor::new();
        // SAFETY: both actors are live TestActor-owned pointers.
        let handle_a = unsafe { hew_actor_schedule_periodic(actor_a.actor, 1, 10) };
        // SAFETY: both actors are live TestActor-owned pointers.
        let handle_b = unsafe { hew_actor_schedule_periodic(actor_b.actor, 2, 15) };
        // SAFETY: both handles are live *mut WasmPeriodicCtx.
        let next_fire = unsafe {
            (*handle_a.cast::<WasmPeriodicCtx>())
                .next_fire_ms
                .max((*handle_b.cast::<WasmPeriodicCtx>()).next_fire_ms)
        };

        assert_eq!(pending_periodic_count(), 2);
        assert_eq!(crate::scheduler_wasm::hew_wasm_sleeping_count(), 2);

        crate::scheduler_wasm::hew_sched_shutdown();
        assert_eq!(
            pending_periodic_count(),
            0,
            "shutdown must clear periodic queue"
        );
        assert_eq!(crate::scheduler_wasm::hew_wasm_sleeping_count(), 0);

        crate::scheduler_wasm::hew_sched_init();
        // SAFETY: tests serialize scheduler access and the runtime is initialized.
        unsafe {
            let _ = crate::scheduler_wasm::hew_wasm_timer_tick(next_fire.saturating_add(100));
        };
        drive_scheduler();
        assert_eq!(actor_a.count(), 0);
        assert_eq!(actor_b.count(), 0);
    }

    #[test]
    fn cancel_all_timers_for_actor_prevents_future_fires() {
        let _guard = crate::runtime_test_guard();
        reset_runtime();
        let actor = TestActor::new();
        // SAFETY: actor is a live TestActor-owned pointer.
        let handle = unsafe { hew_actor_schedule_periodic(actor.actor, 7, 10) };
        // SAFETY: handle is a live *mut WasmPeriodicCtx.
        let first_fire = unsafe { (*handle.cast::<WasmPeriodicCtx>()).next_fire_ms };

        // SAFETY: actor is still owned by this test and not freed until drop.
        unsafe { cancel_all_timers_for_actor(actor.actor) };
        assert_eq!(pending_periodic_count(), 0);

        // SAFETY: tests serialize scheduler access and the runtime is initialized.
        unsafe {
            let _ = crate::scheduler_wasm::hew_wasm_timer_tick(first_fire.saturating_add(50));
        };
        drive_scheduler();
        assert_eq!(
            actor.count(),
            0,
            "actor cleanup must cancel periodic timers"
        );
    }
}
