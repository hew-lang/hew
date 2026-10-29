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
use std::sync::atomic::{AtomicBool, AtomicPtr, Ordering};
use std::sync::{Arc, Mutex, RwLock};

use crate::lifetime::PoisonSafe;

use crate::actor::HewActor;
use crate::timer_wheel::{hew_timer_wheel_free, HewTimerWheel};

mod entry;
use entry::{schedule_periodic_ns_on_wheel, PeriodicCtx};

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
fn wheel_inserted(wheel: *mut HewTimerWheel, deadline_ns: u64) {
    if wheel == GLOBAL_WHEEL.load(Ordering::Acquire) {
        crate::reactor::timer_inserted(deadline_ns);
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
/// Each [`PeriodicCtx`] is owned through an [`Arc`]. Its persistent owners are:
///
/// * one held here in the registry (per active timer), and
/// * one held by the *single* pending one-shot in the global wheel
///   (handed over via `Arc::into_raw` when the entry is scheduled).
///
/// Registration, callbacks and teardown may hold temporary references too.
/// Ownership of the ctx allocation is decided by the `Arc` refcount —
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
#[derive(Clone)]
enum ActorTimer {
    Periodic(Arc<PeriodicCtx>),
    OneShot(Arc<crate::timer_actor::OneShot>),
}

impl ActorTimer {
    fn address(&self) -> usize {
        match self {
            Self::Periodic(ctx) => Arc::as_ptr(ctx) as usize,
            Self::OneShot(ctx) => Arc::as_ptr(ctx) as usize,
        }
    }

    fn cancel(&self) {
        match self {
            Self::Periodic(ctx) => ctx.mark_cancelled(),
            Self::OneShot(ctx) => ctx.cancel(),
        }
    }

    fn drain(&self) {
        if let Self::Periodic(ctx) = self {
            ctx.drain();
        }
    }
}

type TimerRegistry = HashMap<usize, Vec<ActorTimer>>;
static ACTOR_TIMERS: PoisonSafe<Option<TimerRegistry>> = PoisonSafe::new(None);

/// One-shot requests already admitted by handlers must finish before a guest
/// snapshot. Periodic registrations are retained behind the guest ingress gate.
#[cfg(target_arch = "wasm32")]
pub(crate) fn has_pending_guest_requests() -> bool {
    ACTOR_TIMERS.access(|lock| {
        lock.as_ref().is_some_and(|actors| {
            actors
                .values()
                .flatten()
                .any(|timer| matches!(timer, ActorTimer::OneShot(_)))
        })
    })
}

fn register_timer(actor: *mut HewActor, ctx: Arc<PeriodicCtx>) {
    ACTOR_TIMERS.access(|lock| {
        lock.get_or_insert_with(HashMap::new)
            .entry(actor as usize)
            .or_default()
            .push(ActorTimer::Periodic(ctx));
    });
}

/// Publish a one-shot while the shared timer admission gate is held.
/// The registry lock orders publication against actor-local stop cancellation.
/// # Safety
/// The caller pins the actor and owns the unpublished timer request.
pub(crate) unsafe fn register_one_shot(
    actor: *mut HewActor,
    ctx: Arc<crate::timer_actor::OneShot>,
) -> bool {
    ACTOR_TIMERS.access(|lock| {
        // SAFETY: the caller retains the actor and mailbox through publication.
        if unsafe { crate::mailbox::mailbox_is_closed((*actor).mailbox.cast()) } {
            return false;
        }
        lock.get_or_insert_with(HashMap::new)
            .entry(actor as usize)
            .or_default()
            .push(ActorTimer::OneShot(ctx));
        true
    })
}

pub(crate) fn admit_timer<T>(operation: impl FnOnce() -> T) -> Option<T> {
    let _admission = PERIODIC_ADMISSION
        .read()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    PERIODIC_ACCEPTING.load(Ordering::Acquire).then(operation)
}

/// Drop the registry's strong reference to the ctx at `ctx_addr` for `actor`.
pub(crate) fn unregister_timer(actor: *mut HewActor, ctx_addr: usize) {
    ACTOR_TIMERS.access(|lock| {
        if let Some(map) = lock.as_mut() {
            if let Some(timers) = map.get_mut(&(actor as usize)) {
                timers.retain(|c| c.address() != ctx_addr);
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
/// Phase 1 marks every ctx cancelled. Phase 2 spin-waits on the in-flight
/// guards so any executing callback finishes sending before the
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

    for timer in &timers {
        timer.cancel();
    }
    for timer in &timers {
        timer.drain();
    }
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
    let all: Vec<(usize, ActorTimer)> = {
        let _admission = PERIODIC_ADMISSION
            .write()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        PERIODIC_ACCEPTING.store(false, Ordering::Release);
        ACTOR_TIMERS.access(|lock| {
            lock.as_ref()
                .map(|map| {
                    map.iter()
                        .flat_map(|(actor, timers)| {
                            timers.iter().cloned().map(|timer| (*actor, timer))
                        })
                        .collect()
                })
                .unwrap_or_default()
        })
    };

    for (_, timer) in &all {
        timer.cancel();
    }
    for (actor, timer) in &all {
        timer.drain();
        unregister_timer(*actor as *mut HewActor, timer.address());
    }
}

/// Open periodic-timer admission for a newly installed runtime generation.
#[cfg(not(target_arch = "wasm32"))]
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
    let Some(period) = interval_ms.checked_mul(1_000_000) else {
        return ptr::null_mut();
    };
    if actor.is_null() || period == 0 {
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
        schedule_periodic_ns_on_wheel(
            actor,
            msg_type,
            period,
            tw,
            crate::clock::hew_instant_now().max(0).cast_unsigned(),
        )
    }
}

/// Arm a newly published actor's periodic handlers while admission is open.
/// Accepted actor turns may construct peers during shutdown drain. Those peers
/// finish construction with periodic work quiesced, rather than failing spawn.
///
/// # Safety
/// `actor` is pinned and remains undispatchable until publication; `handlers`
/// contains the compiler-selected, positive periodic intervals.
pub(crate) unsafe fn start_actor_periodic(
    actor: *mut HewActor,
    handlers: &[crate::actor::HewNativePeriodicHandler],
) -> bool {
    if handlers.is_empty() {
        return true;
    }
    admit_timer(|| {
        let wheel = global_wheel();
        if wheel.is_null() {
            return false;
        }
        // SAFETY: reading the runtime clock has no caller preconditions.
        let now = unsafe { crate::clock::hew_instant_now() }
            .max(0)
            .cast_unsigned();
        for handler in handlers {
            if handler.interval_ns == 0 {
                return false;
            }
            // SAFETY: admission retains the wheel; the caller pins the actor.
            let timer = unsafe {
                schedule_periodic_ns_on_wheel(
                    actor,
                    handler.message,
                    handler.interval_ns,
                    wheel,
                    now,
                )
            };
            if timer.is_null() {
                return false;
            }
        }
        true
    })
    .unwrap_or(true)
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
            map.values().flatten().find_map(|timer| match timer {
                ActorTimer::Periodic(ctx) if Arc::as_ptr(ctx) as usize == addr => {
                    Some(Arc::clone(ctx))
                }
                _ => None,
            })
        })
    });
    let Some(ctx) = ctx else {
        return;
    };
    entry::cancel(&ctx);
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
    use crate::timer_wheel::timer_wheel_cursor_ms;
    use crate::timer_wheel::{
        hew_timer_wheel_new, hew_timer_wheel_next_deadline_ms, hew_timer_wheel_schedule,
    };
    use std::sync::atomic::{AtomicI32, AtomicU32, AtomicU64};
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
            coalesce_match_fn: None,
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
        let handle = unsafe {
            schedule_periodic_ns_on_wheel(actor_ptr, 7, 10_000_000, wheel, now * 1_000_000)
        };
        assert!(!handle.is_null());

        // SAFETY: the registry keeps this context live through cancellation.
        let next_fire = unsafe { &(*handle.cast::<PeriodicCtx>()).next_fire_ns };
        let first_fire = next_fire.load(Ordering::SeqCst) / 1_000_000;
        let late_tick = first_fire.saturating_add(25);

        // A late tick fires once and skips missed slots without drifting from
        // the original cadence phase.
        // SAFETY: wheel and callback payload remain live throughout.
        unsafe {
            assert_eq!(crate::timer_wheel::timer_wheel_tick_to(wheel, late_tick), 1);
            assert_eq!(
                next_fire.load(Ordering::SeqCst) / 1_000_000,
                first_fire + 30
            );
            assert_eq!(crate::timer_wheel::timer_wheel_tick_to(wheel, late_tick), 0);
            assert_eq!(
                crate::timer_wheel::timer_wheel_tick_to(wheel, first_fire + 30),
                1
            );
            assert_eq!(
                next_fire.load(Ordering::SeqCst) / 1_000_000,
                first_fire + 40
            );
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
        let handle = unsafe {
            schedule_periodic_ns_on_wheel(actor_ptr, 7, 1_000_000, wheel, now * 1_000_000)
        };
        assert!(!handle.is_null());

        // SAFETY: the registry keeps this context live through cancellation.
        let next_fire = unsafe { &(*handle.cast::<PeriodicCtx>()).next_fire_ns };
        let first_fire = next_fire.load(Ordering::SeqCst) / 1_000_000;
        let stalled_cursor = first_fire + 5_000;

        // SAFETY: wheel and callback payload remain live throughout.
        unsafe {
            assert_eq!(
                crate::timer_wheel::timer_wheel_tick_to(wheel, stalled_cursor),
                1
            );
            assert_eq!(
                next_fire.load(Ordering::SeqCst) / 1_000_000,
                stalled_cursor + 1
            );
            assert_eq!(
                crate::timer_wheel::timer_wheel_tick_to(wheel, stalled_cursor),
                0
            );
            assert_eq!(
                crate::timer_wheel::timer_wheel_tick_to(wheel, stalled_cursor + 1),
                1
            );
            assert_eq!(
                next_fire.load(Ordering::SeqCst) / 1_000_000,
                stalled_cursor + 2
            );
            hew_actor_cancel_periodic(handle);
            hew_timer_wheel_free(wheel);
        }
    }

    #[test]
    fn fractional_periodic_cadence_survives_a_late_tick() {
        let _guard = TICKER_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let mut actor = create_test_actor(50_052);
        actor
            .actor_state
            .store(HewActorState::Stopped as i32, Ordering::SeqCst);
        // SAFETY: the fixture owns the stopped actor, wheel and timer through
        // cancellation. The stopped actor prevents mailbox delivery.
        unsafe {
            let wheel = hew_timer_wheel_new();
            let now = timer_wheel_cursor_ms(wheel) * 1_000_000 + 900_000;
            let handle = schedule_periodic_ns_on_wheel(&raw mut actor, 7, 250_000, wheel, now);
            assert!(!handle.is_null());
            let next = &(*handle.cast::<PeriodicCtx>()).next_fire_ns;
            assert_eq!(next.load(Ordering::SeqCst), now + 250_000);
            assert_eq!(
                crate::timer_wheel::timer_wheel_tick_at(
                    wheel,
                    crate::timer_deadline::Deadline::from_nanos(now + 249_999)
                ),
                0
            );
            assert_eq!(
                crate::timer_wheel::timer_wheel_tick_at(
                    wheel,
                    crate::timer_deadline::Deadline::from_nanos(now + 250_000)
                ),
                1
            );
            assert_eq!(next.load(Ordering::SeqCst), now + 500_000);
            assert_eq!(
                crate::timer_wheel::timer_wheel_tick_at(
                    wheel,
                    crate::timer_deadline::Deadline::from_nanos(now + 1_123_456)
                ),
                1
            );
            assert_eq!(next.load(Ordering::SeqCst), now + 1_250_000);
            hew_actor_cancel_periodic(handle);
            assert!(crate::timer_wheel::timer_wheel_next_deadline(wheel).is_none());
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
            interval_ns: 100,
            next_fire_ns: AtomicU64::new(100),
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
            interval_ns: 200,
            next_fire_ns: AtomicU64::new(200),
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
            let ActorTimer::Periodic(timer) = lock
                .as_ref()
                .and_then(|map| map.get(&(actor_ptr as usize)))
                .and_then(|timers| timers.first())
                .expect("scheduled timer must be registered")
            else {
                panic!("expected the registered periodic timer");
            };
            Arc::clone(timer)
        });
        let wheel = ctx.wheel;
        let first_fire = ctx.next_fire_ns.load(Ordering::SeqCst) / 1_000_000;
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
        assert_eq!(
            ctx.next_fire_ns.load(Ordering::SeqCst) / 1_000_000,
            first_fire
        );

        // The due entry is still pending and unclaimed. Shutdown must cancel
        // it rather than silently delivering the callback.
        // SAFETY: this test owns the process-wide timer state.
        unsafe { hew_periodic_shutdown() };

        assert_eq!(
            ctx.next_fire_ns.load(Ordering::SeqCst) / 1_000_000,
            first_fire
        );
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
            let ActorTimer::Periodic(timer) = lock
                .as_ref()
                .and_then(|map| map.get(&(actor_ptr as usize)))
                .and_then(|timers| timers.first())
                .expect("scheduled timer must be registered")
            else {
                panic!("expected the registered periodic timer");
            };
            Arc::clone(timer)
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

    #[cfg(not(target_arch = "wasm32"))]
    mod failure_properties {
        use super::*;
        use proptest::prelude::*;

        struct Registration {
            wheel: *mut HewTimerWheel,
            handle: *mut c_void,
        }
        impl Drop for Registration {
            fn drop(&mut self) {
                // SAFETY: the actor and ticker-test guard outlive this registration.
                unsafe {
                    hew_actor_cancel_periodic(self.handle);
                    hew_timer_wheel_free(self.wheel);
                }
            }
        }

        proptest! {
            #![proptest_config(ProptestConfig::with_cases(1024))]
            #[test]
            fn generated_periodic_histories_preserve_phase_and_retire(
                period in 1u64..=1_000_000,
                fraction in 0u64..1_000_000,
                actions in prop::collection::vec((0u8..4, prop_oneof![
                    4 => 0u64..2_000_000,
                    1 => 20_000_000_000u64..60_000_000_000,
                ], any::<bool>()), 1..24),
            ) {
                let _guard = TICKER_TEST_MUTEX.lock().unwrap_or_else(std::sync::PoisonError::into_inner);
                let mut actor = create_test_actor(50_450);
                actor.actor_state.store(HewActorState::Stopped as i32, Ordering::SeqCst);
                // The stopped actor excludes mailbox execution; this property
                // exercises actual registration, callback, rearm and removal.
                // SAFETY: this fixture owns the stopped actor and wheel until
                // Registration cancels and frees them under the ticker guard.
                let (wheel, start, handle) = unsafe {
                    let wheel = hew_timer_wheel_new();
                    let start = timer_wheel_cursor_ms(wheel) * 1_000_000 + fraction;
                    let handle = schedule_periodic_ns_on_wheel(&raw mut actor, 7, period, wheel, start);
                    (wheel, start, handle)
                };
                let registration = Registration { wheel, handle };
                prop_assert!(!handle.is_null());
                let probe = ACTOR_TIMERS.access(|lock| {
                    let ActorTimer::Periodic(ctx) = &lock.as_ref().unwrap()[&((&raw mut actor) as usize)][0] else { unreachable!() };
                    Arc::downgrade(ctx)
                });
                let first = start + period;
                let mut current = start;
                let mut cancelled = false;
                let next = || {
                    // SAFETY: Registration retains this test-owned wheel.
                    unsafe { crate::timer_wheel::timer_wheel_next_deadline(wheel) }
                        .map(crate::timer_deadline::Deadline::saturated_nanos)
                };
                prop_assert_eq!(next(), Some(first));
                for (mode, delta, cancel) in actions {
                    let before = next();
                    current = match mode {
                        0 => current,
                        1 => before.unwrap_or(current).saturating_sub(1).max(current),
                        2 => before.unwrap_or(current).max(current),
                        _ => current + delta,
                    };
                    if cancel {
                        // SAFETY: this is the fixture's original identity handle;
                        // cancellation never dereferences it after removal.
                        unsafe { hew_actor_cancel_periodic(handle) };
                        cancelled = true;
                    }
                    // SAFETY: actor, wheel and callback ownership remain live.
                    let fired = unsafe { crate::timer_wheel::timer_wheel_tick_at(wheel,
                        crate::timer_deadline::Deadline::from_nanos(current)) };
                    let eligible = !cancelled && before.is_some_and(|d| d <= current);
                    prop_assert_eq!(fired, i32::from(eligible));
                    if cancelled {
                        prop_assert_eq!(next(), None);
                        prop_assert!(probe.upgrade().is_none());
                    } else if eligible {
                        let after = next().expect("live cadence must rearm in this representable range");
                        prop_assert!(after > before.unwrap());
                        prop_assert_eq!((after - first) % period, 0);
                        prop_assert!(after >= current);
                        prop_assert!(after - period <= current);
                    } else {
                        prop_assert_eq!(next(), before);
                    }
                }
                // SAFETY: the fixture retains the original timer identity.
                unsafe { hew_actor_cancel_periodic(handle) };
                prop_assert_eq!(next(), None);
                prop_assert!(probe.upgrade().is_none());
                drop(registration);
            }
        }
    }
}
