//! Canonical runtime observability registry.
//!
//! Cheap gauges stay live at all times. Hot counters (per-allocation and
//! per-turn) are opt-in through `HEW_OBSERVE` or the runtime toggle below and
//! are sharded by worker to avoid cache-line contention when enabled.

use std::cell::Cell;
use std::collections::HashMap;
use std::ffi::{c_char, c_void, CStr};
use std::fmt::Write as _;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
#[cfg(not(target_arch = "wasm32"))]
use std::sync::{Condvar, Mutex};
#[cfg(not(target_arch = "wasm32"))]
use std::time::{Duration, Instant};

use crate::actor::HEW_MAX_WORKERS;
use crate::cabi::{free_cstring, str_to_malloc};
use crate::lifetime::PoisonSafe;
#[cfg(not(target_arch = "wasm32"))]
use crate::util::{CondvarExt, MutexExt};

const SHARD_COUNT: usize = HEW_MAX_WORKERS + 1;
const OBSERVE_BARRIER_OK: i64 = 0;
const OBSERVE_BARRIER_ERR_WORKER_CONTEXT: i64 = -1;
const OBSERVE_BARRIER_ERR_TIMEOUT: i64 = -2;
#[cfg(not(target_arch = "wasm32"))]
const OBSERVE_BARRIER_TIMEOUT: Duration = Duration::from_secs(30);

thread_local! {
    static WORKER_SHARD: Cell<usize> = const { Cell::new(0) };
}

static HOT_TIER_ENABLED: AtomicBool = AtomicBool::new(false);

static HEAP_ALLOCATED_TOTAL: [AtomicU64; SHARD_COUNT] = [const { AtomicU64::new(0) }; SHARD_COUNT];
static HEAP_FREED_TOTAL: [AtomicU64; SHARD_COUNT] = [const { AtomicU64::new(0) }; SHARD_COUNT];
static HEAP_ALLOCATIONS_TOTAL: [AtomicU64; SHARD_COUNT] =
    [const { AtomicU64::new(0) }; SHARD_COUNT];
static HEAP_FREES_TOTAL: [AtomicU64; SHARD_COUNT] = [const { AtomicU64::new(0) }; SHARD_COUNT];

static ACTOR_TURNS_TOTAL: [AtomicU64; SHARD_COUNT] = [const { AtomicU64::new(0) }; SHARD_COUNT];
static ACTOR_TURN_DURATION_NS_TOTAL: [AtomicU64; SHARD_COUNT] =
    [const { AtomicU64::new(0) }; SHARD_COUNT];

static SCHEDULER_PARKS_TOTAL: AtomicU64 = AtomicU64::new(0);
static SCHEDULER_UNPARKS_TOTAL: AtomicU64 = AtomicU64::new(0);
static COROUTINES_LIVE: AtomicU64 = AtomicU64::new(0);
static COROUTINES_SUSPENDED: AtomicU64 = AtomicU64::new(0);
static COROUTINES_RESUMES_TOTAL: AtomicU64 = AtomicU64::new(0);
static COROUTINES_SUSPENDS_TOTAL: AtomicU64 = AtomicU64::new(0);
static COROUTINES_FRAME_BYTES_LIVE: AtomicU64 = AtomicU64::new(0);
static REACTOR_REGISTRATIONS_LIVE: AtomicU64 = AtomicU64::new(0);
static REACTOR_READY_EVENTS_TOTAL: AtomicU64 = AtomicU64::new(0);
static ACTORS_CRASHES_TOTAL: AtomicU64 = AtomicU64::new(0);
static ACTORS_RESTARTS_TOTAL: AtomicU64 = AtomicU64::new(0);
static ARENA_RESETS_TOTAL: AtomicU64 = AtomicU64::new(0);
static THREADS_BLOCKING_COUNT: AtomicU64 = AtomicU64::new(0);

static ATTRIBUTED_TURNS: PoisonSafe<Option<HashMap<(usize, i32), AttributedTurn>>> =
    PoisonSafe::new(None);

#[cfg(not(target_arch = "wasm32"))]
static NEXT_DISPATCH_TICKET: AtomicU64 = AtomicU64::new(0);
#[cfg(not(target_arch = "wasm32"))]
static PUBLISHED_DISPATCH_TICKET: AtomicU64 = AtomicU64::new(0);
#[cfg(not(target_arch = "wasm32"))]
static DISPATCH_COMPLETION_WATERMARK: AtomicU64 = AtomicU64::new(0);
#[cfg(not(target_arch = "wasm32"))]
static DISPATCH_BARRIER_WAITERS: AtomicU64 = AtomicU64::new(0);
#[cfg(not(target_arch = "wasm32"))]
static DISPATCH_ACTIVE_TICKETS: [AtomicU64; SHARD_COUNT] =
    [const { AtomicU64::new(0) }; SHARD_COUNT];
#[cfg(all(test, not(target_arch = "wasm32")))]
static DISPATCH_BARRIER_MUTEX_ACQUISITIONS: AtomicU64 = AtomicU64::new(0);

#[cfg(not(target_arch = "wasm32"))]
static DISPATCH_BARRIER: DispatchBarrier = DispatchBarrier {
    lock: Mutex::new(()),
    cond: Condvar::new(),
};

#[cfg(not(target_arch = "wasm32"))]
struct DispatchBarrier {
    lock: Mutex<()>,
    cond: Condvar,
}

#[cfg(not(target_arch = "wasm32"))]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct ObserveDispatchTicket(u64);

#[cfg(not(target_arch = "wasm32"))]
struct DispatchBarrierWaiter;

#[cfg(not(target_arch = "wasm32"))]
impl Drop for DispatchBarrierWaiter {
    fn drop(&mut self) {
        DISPATCH_BARRIER_WAITERS.fetch_sub(1, Ordering::AcqRel);
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct AttributedTurn {
    pub turns_total: u64,
    pub duration_ns_total: u64,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct HotCounterSnapshot {
    pub heap_allocated_total: u64,
    pub heap_freed_total: u64,
    pub heap_allocations_total: u64,
    pub heap_frees_total: u64,
    pub actor_turns_total: u64,
    pub actor_turn_duration_ns_total: u64,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct CoroutineSnapshot {
    pub live: u64,
    pub suspended: u64,
    pub resumes_total: u64,
    pub suspends_total: u64,
    pub frame_bytes_live: u64,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct RuntimeHookSnapshot {
    pub reactor_registrations_live: u64,
    pub reactor_ready_events_total: u64,
    pub actors_crashes_total: u64,
    pub actors_restarts_total: u64,
    pub arena_resets_total: u64,
    pub threads_blocking_count: u64,
}

/// Configure the hot observe tier from `HEW_OBSERVE`.
///
/// Accepted true values are `1`, `true`, `yes`, `on`, and `hot`. Empty/unset
/// leaves the tier disabled. This is called during scheduler initialization.
pub(crate) fn configure_from_env() {
    let enabled = std::env::var("HEW_OBSERVE")
        .is_ok_and(|value| matches!(value.as_str(), "1" | "true" | "yes" | "on" | "hot"));
    HOT_TIER_ENABLED.store(enabled, Ordering::Release);
}

#[must_use]
pub fn observe_hot_tier_enabled() -> bool {
    HOT_TIER_ENABLED.load(Ordering::Acquire)
}

pub fn set_hot_tier_enabled(enabled: bool) {
    HOT_TIER_ENABLED.store(enabled, Ordering::Release);
}

pub(crate) fn set_current_worker_shard(worker_id: usize) {
    let shard = worker_id.saturating_add(1).min(SHARD_COUNT - 1);
    WORKER_SHARD.with(|slot| slot.set(shard));
}

fn current_shard() -> usize {
    WORKER_SHARD.with(Cell::get)
}

fn shard_add(shards: &[AtomicU64; SHARD_COUNT], value: u64) {
    shards[current_shard()].fetch_add(value, Ordering::Relaxed);
}

fn shard_sum(shards: &[AtomicU64; SHARD_COUNT]) -> u64 {
    shards
        .iter()
        .map(|counter| counter.load(Ordering::Relaxed))
        .sum()
}

fn shard_reset(shards: &[AtomicU64; SHARD_COUNT]) {
    for counter in shards {
        counter.store(0, Ordering::Relaxed);
    }
}

pub(crate) fn record_heap_alloc(size: u64) {
    if observe_hot_tier_enabled() {
        shard_add(&HEAP_ALLOCATIONS_TOTAL, 1);
        shard_add(&HEAP_ALLOCATED_TOTAL, size);
    }
}

pub(crate) fn record_heap_free(size: u64) {
    if observe_hot_tier_enabled() {
        shard_add(&HEAP_FREES_TOTAL, 1);
        shard_add(&HEAP_FREED_TOTAL, size);
    }
}

pub(crate) fn record_actor_turn(duration_ns: u64) {
    if observe_hot_tier_enabled() {
        shard_add(&ACTOR_TURNS_TOTAL, 1);
        shard_add(&ACTOR_TURN_DURATION_NS_TOTAL, duration_ns);
    }
}

#[cfg(not(target_arch = "wasm32"))]
fn dispatch_barrier_lock() -> std::sync::MutexGuard<'static, ()> {
    #[cfg(test)]
    DISPATCH_BARRIER_MUTEX_ACQUISITIONS.fetch_add(1, Ordering::Relaxed);
    DISPATCH_BARRIER.lock.lock_or_recover()
}

#[cfg(not(target_arch = "wasm32"))]
fn publish_dispatch_begin(ticket: u64) {
    while PUBLISHED_DISPATCH_TICKET
        .compare_exchange(
            ticket.saturating_sub(1),
            ticket,
            Ordering::Release,
            Ordering::Acquire,
        )
        .is_err()
    {
        std::hint::spin_loop();
    }
}

#[cfg(not(target_arch = "wasm32"))]
fn computed_dispatch_watermark() -> u64 {
    let published_ticket = PUBLISHED_DISPATCH_TICKET.load(Ordering::Acquire);
    DISPATCH_ACTIVE_TICKETS
        .iter()
        .filter_map(|slot| {
            let ticket = slot.load(Ordering::Acquire);
            (ticket != 0 && ticket <= published_ticket).then_some(ticket)
        })
        .min()
        .map_or(published_ticket, |ticket| ticket.saturating_sub(1))
}

#[cfg(not(target_arch = "wasm32"))]
fn publish_dispatch_watermark() -> u64 {
    let watermark = computed_dispatch_watermark();
    let mut current = DISPATCH_COMPLETION_WATERMARK.load(Ordering::Acquire);
    while watermark > current {
        match DISPATCH_COMPLETION_WATERMARK.compare_exchange(
            current,
            watermark,
            Ordering::Release,
            Ordering::Acquire,
        ) {
            Ok(_) => return watermark,
            Err(actual) => current = actual,
        }
    }
    current
}

#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn observe_dispatch_begin() -> ObserveDispatchTicket {
    let ticket = NEXT_DISPATCH_TICKET
        .fetch_add(1, Ordering::Relaxed)
        .saturating_add(1);
    DISPATCH_ACTIVE_TICKETS[current_shard()].store(ticket, Ordering::Release);
    publish_dispatch_begin(ticket);
    ObserveDispatchTicket(ticket)
}

#[cfg(not(target_arch = "wasm32"))]
fn clear_active_dispatch_ticket(ticket: ObserveDispatchTicket) {
    let shard = current_shard();
    if DISPATCH_ACTIVE_TICKETS[shard]
        .compare_exchange(ticket.0, 0, Ordering::Release, Ordering::Relaxed)
        .is_ok()
    {
        return;
    }

    for slot in &DISPATCH_ACTIVE_TICKETS {
        if slot
            .compare_exchange(ticket.0, 0, Ordering::Release, Ordering::Relaxed)
            .is_ok()
        {
            return;
        }
    }

    debug_assert!(
        false,
        "observe dispatch ticket closed without an active slot"
    );
}

#[cfg(not(target_arch = "wasm32"))]
fn observe_dispatch_close(ticket: ObserveDispatchTicket) {
    clear_active_dispatch_ticket(ticket);
    publish_dispatch_watermark();

    if DISPATCH_BARRIER_WAITERS.load(Ordering::Acquire) > 0 {
        let _guard = dispatch_barrier_lock();
        DISPATCH_BARRIER.cond.notify_all();
    }
}

#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn observe_dispatch_attributed(ticket: ObserveDispatchTicket) {
    observe_dispatch_close(ticket);
}

#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn observe_dispatch_abandon(ticket: ObserveDispatchTicket) {
    observe_dispatch_close(ticket);
}

pub(crate) fn record_scheduler_park() {
    SCHEDULER_PARKS_TOTAL.fetch_add(1, Ordering::Relaxed);
}

pub(crate) fn record_scheduler_unpark() {
    SCHEDULER_UNPARKS_TOTAL.fetch_add(1, Ordering::Relaxed);
}

pub(crate) fn record_coroutine_frame_alloc(frame_bytes: u64) {
    COROUTINES_LIVE.fetch_add(1, Ordering::Relaxed);
    COROUTINES_FRAME_BYTES_LIVE.fetch_add(frame_bytes, Ordering::Relaxed);
}

pub(crate) fn record_coroutine_frame_free(frame_bytes: u64) {
    COROUTINES_LIVE.fetch_sub(1, Ordering::Relaxed);
    COROUTINES_FRAME_BYTES_LIVE.fetch_sub(frame_bytes, Ordering::Relaxed);
}

pub(crate) fn record_coroutine_suspend() {
    COROUTINES_SUSPENDS_TOTAL.fetch_add(1, Ordering::Relaxed);
    COROUTINES_SUSPENDED.fetch_add(1, Ordering::Relaxed);
}

pub(crate) fn record_coroutine_resume() {
    COROUTINES_RESUMES_TOTAL.fetch_add(1, Ordering::Relaxed);
    let _ = COROUTINES_SUSPENDED.fetch_update(Ordering::Relaxed, Ordering::Relaxed, |value| {
        value.checked_sub(1)
    });
}

pub(crate) fn record_reactor_registration() {
    REACTOR_REGISTRATIONS_LIVE.fetch_add(1, Ordering::Relaxed);
}

pub(crate) fn record_reactor_unregistration(count: u64) {
    let _ =
        REACTOR_REGISTRATIONS_LIVE.fetch_update(Ordering::Relaxed, Ordering::Relaxed, |value| {
            Some(value.saturating_sub(count))
        });
}

pub(crate) fn record_reactor_ready_event() {
    REACTOR_READY_EVENTS_TOTAL.fetch_add(1, Ordering::Relaxed);
}

pub(crate) fn record_actor_crash() {
    ACTORS_CRASHES_TOTAL.fetch_add(1, Ordering::Relaxed);
}

pub(crate) fn record_actor_restart() {
    ACTORS_RESTARTS_TOTAL.fetch_add(1, Ordering::Relaxed);
}

pub(crate) fn record_arena_reset() {
    ARENA_RESETS_TOTAL.fetch_add(1, Ordering::Relaxed);
}

pub(crate) fn record_blocking_start() {
    THREADS_BLOCKING_COUNT.fetch_add(1, Ordering::Relaxed);
}

pub(crate) fn record_blocking_finish() {
    let _ = THREADS_BLOCKING_COUNT.fetch_update(Ordering::Relaxed, Ordering::Relaxed, |value| {
        value.checked_sub(1)
    });
}

#[no_mangle]
pub extern "C" fn hew_observe_hot_tier_enabled() -> i32 {
    i32::from(observe_hot_tier_enabled())
}

/// Wait until all native actor dispatches that started before this call have
/// reached their observe attribution point.
///
/// Returns 0 on success, -1 when called from inside an actor dispatch (which
/// would deadlock with `HEW_WORKERS=1`), and -2 after the bounded 30-second
/// internal timeout.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub extern "C" fn hew_observe_barrier() -> i64 {
    if !crate::execution_context::current_context().is_null() {
        crate::set_last_error("observe.barrier: cannot wait from inside an actor dispatch");
        return OBSERVE_BARRIER_ERR_WORKER_CONTEXT;
    }

    let target_ticket = PUBLISHED_DISPATCH_TICKET.load(Ordering::Acquire);
    if DISPATCH_COMPLETION_WATERMARK.load(Ordering::Acquire) >= target_ticket {
        return OBSERVE_BARRIER_OK;
    }

    let deadline = Instant::now() + OBSERVE_BARRIER_TIMEOUT;
    let mut guard = dispatch_barrier_lock();
    DISPATCH_BARRIER_WAITERS.fetch_add(1, Ordering::AcqRel);
    let _waiter = DispatchBarrierWaiter;

    while DISPATCH_COMPLETION_WATERMARK.load(Ordering::Acquire) < target_ticket {
        let now = Instant::now();
        if now >= deadline {
            crate::set_last_error("observe.barrier: timed out after 30 seconds");
            return OBSERVE_BARRIER_ERR_TIMEOUT;
        }
        let remaining = deadline.saturating_duration_since(now);
        let (next_guard, wait_result) = DISPATCH_BARRIER
            .cond
            .wait_timeout_or_recover(guard, remaining);
        guard = next_guard;
        if wait_result.timed_out()
            && DISPATCH_COMPLETION_WATERMARK.load(Ordering::Acquire) < target_ticket
        {
            crate::set_last_error("observe.barrier: timed out after 30 seconds");
            return OBSERVE_BARRIER_ERR_TIMEOUT;
        }
    }

    OBSERVE_BARRIER_OK
}

/// WASM currently has no scheduler attribution probe path; keep the observe
/// surface target-neutral like `series`/`scrape` and make the barrier a no-op.
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub extern "C" fn hew_observe_barrier() -> i64 {
    OBSERVE_BARRIER_OK
}

#[no_mangle]
pub extern "C" fn hew_observe_probe_turn(
    dispatch_ptr: *const c_void,
    msg_type: i32,
    duration_ns: u64,
) {
    if !observe_hot_tier_enabled() || dispatch_ptr.is_null() || duration_ns == 0 {
        return;
    }
    ATTRIBUTED_TURNS.access(|slot| {
        let entry = slot
            .get_or_insert_with(HashMap::new)
            .entry((dispatch_ptr as usize, msg_type))
            .or_default();
        entry.turns_total = entry.turns_total.saturating_add(1);
        entry.duration_ns_total = entry.duration_ns_total.saturating_add(duration_ns);
    });
}

#[no_mangle]
pub extern "C" fn hew_observe_probe_suspend(dispatch_ptr: *const c_void) {
    if !observe_hot_tier_enabled() || dispatch_ptr.is_null() {
        return;
    }
    let _ = dispatch_ptr;
}

#[must_use]
pub fn hot_counters_snapshot() -> HotCounterSnapshot {
    HotCounterSnapshot {
        heap_allocated_total: shard_sum(&HEAP_ALLOCATED_TOTAL),
        heap_freed_total: shard_sum(&HEAP_FREED_TOTAL),
        heap_allocations_total: shard_sum(&HEAP_ALLOCATIONS_TOTAL),
        heap_frees_total: shard_sum(&HEAP_FREES_TOTAL),
        actor_turns_total: shard_sum(&ACTOR_TURNS_TOTAL),
        actor_turn_duration_ns_total: shard_sum(&ACTOR_TURN_DURATION_NS_TOTAL),
    }
}

#[must_use]
pub fn scheduler_parks_total() -> u64 {
    SCHEDULER_PARKS_TOTAL.load(Ordering::Relaxed)
}

#[must_use]
pub fn scheduler_unparks_total() -> u64 {
    SCHEDULER_UNPARKS_TOTAL.load(Ordering::Relaxed)
}

#[must_use]
pub fn coroutine_snapshot() -> CoroutineSnapshot {
    CoroutineSnapshot {
        live: COROUTINES_LIVE.load(Ordering::Relaxed),
        suspended: COROUTINES_SUSPENDED.load(Ordering::Relaxed),
        resumes_total: COROUTINES_RESUMES_TOTAL.load(Ordering::Relaxed),
        suspends_total: COROUTINES_SUSPENDS_TOTAL.load(Ordering::Relaxed),
        frame_bytes_live: COROUTINES_FRAME_BYTES_LIVE.load(Ordering::Relaxed),
    }
}

#[must_use]
pub fn attributed_turns_snapshot() -> Vec<((usize, i32), AttributedTurn)> {
    ATTRIBUTED_TURNS.access(|slot| {
        slot.as_ref()
            .map(|map| map.iter().map(|(key, value)| (*key, *value)).collect())
            .unwrap_or_default()
    })
}

#[must_use]
fn attributed_turns_totals() -> AttributedTurn {
    attributed_turns_snapshot().into_iter().fold(
        AttributedTurn::default(),
        |mut totals, (_, turn)| {
            totals.turns_total = totals.turns_total.saturating_add(turn.turns_total);
            totals.duration_ns_total = totals
                .duration_ns_total
                .saturating_add(turn.duration_ns_total);
            totals
        },
    )
}

#[must_use]
pub fn runtime_hook_snapshot() -> RuntimeHookSnapshot {
    RuntimeHookSnapshot {
        reactor_registrations_live: REACTOR_REGISTRATIONS_LIVE.load(Ordering::Relaxed),
        reactor_ready_events_total: REACTOR_READY_EVENTS_TOTAL.load(Ordering::Relaxed),
        actors_crashes_total: ACTORS_CRASHES_TOTAL.load(Ordering::Relaxed),
        actors_restarts_total: ACTORS_RESTARTS_TOTAL.load(Ordering::Relaxed),
        arena_resets_total: ARENA_RESETS_TOTAL.load(Ordering::Relaxed),
        threads_blocking_count: THREADS_BLOCKING_COUNT.load(Ordering::Relaxed),
    }
}

#[cfg(not(target_arch = "wasm32"))]
fn heap_live_bytes() -> u64 {
    crate::profiler::allocator::snapshot().bytes_live
}

#[cfg(target_arch = "wasm32")]
fn heap_live_bytes() -> u64 {
    0
}

#[cfg(not(target_arch = "wasm32"))]
fn scheduler_workers() -> u64 {
    crate::scheduler::hew_sched_metrics_worker_count()
}

#[cfg(target_arch = "wasm32")]
fn scheduler_workers() -> u64 {
    1
}

#[cfg(not(target_arch = "wasm32"))]
fn scheduler_runnable_actors() -> u64 {
    crate::scheduler::hew_sched_metrics_runnable_actors()
}

#[cfg(target_arch = "wasm32")]
fn scheduler_runnable_actors() -> u64 {
    0
}

#[cfg(not(target_arch = "wasm32"))]
fn scheduler_runnable_coroutines() -> u64 {
    crate::scheduler::hew_sched_metrics_runnable_coroutines()
}

#[cfg(target_arch = "wasm32")]
fn scheduler_runnable_coroutines() -> u64 {
    0
}

#[cfg(not(target_arch = "wasm32"))]
fn scheduler_queue_depth() -> u64 {
    crate::scheduler::hew_sched_metrics_queue_depth()
}

#[cfg(target_arch = "wasm32")]
fn scheduler_queue_depth() -> u64 {
    0
}

#[cfg(not(target_arch = "wasm32"))]
fn scheduler_steals_total() -> u64 {
    crate::scheduler::hew_sched_metrics_steals()
}

#[cfg(target_arch = "wasm32")]
fn scheduler_steals_total() -> u64 {
    0
}

#[cfg(not(target_arch = "wasm32"))]
fn actors_live() -> u64 {
    crate::profiler::actor_registry::snapshot_all().len() as u64
}

#[cfg(target_arch = "wasm32")]
fn actors_live() -> u64 {
    0
}

#[must_use]
pub fn read_u64(name: &str) -> Option<u64> {
    let hot = hot_counters_snapshot();
    let coroutines = coroutine_snapshot();
    let hooks = runtime_hook_snapshot();
    match name {
        "heap.live_bytes" => Some(heap_live_bytes()),
        "heap.allocated_total" => Some(hot.heap_allocated_total),
        "heap.freed_total" => Some(hot.heap_freed_total),
        "heap.allocations_total" => Some(hot.heap_allocations_total),
        "heap.frees_total" => Some(hot.heap_frees_total),
        "scheduler.workers" => Some(scheduler_workers()),
        "scheduler.runnable_actors" => Some(scheduler_runnable_actors()),
        "scheduler.runnable_coroutines" => Some(scheduler_runnable_coroutines()),
        "scheduler.queue_depth" => Some(scheduler_queue_depth()),
        "scheduler.steals_total" => Some(scheduler_steals_total()),
        "scheduler.parks_total" => Some(crate::observe::scheduler_parks_total()),
        "scheduler.unparks_total" => Some(crate::observe::scheduler_unparks_total()),
        "actors.live" => Some(actors_live()),
        "actors.turns_total" => Some(hot.actor_turns_total),
        "actors.turn_duration_ns_total" => Some(hot.actor_turn_duration_ns_total),
        "actors.attributed_turns_total" => Some(attributed_turns_totals().turns_total),
        "actors.attributed_turn_duration_ns_total" => {
            Some(attributed_turns_totals().duration_ns_total)
        }
        "actors.crashes_total" => Some(hooks.actors_crashes_total),
        "actors.restarts_total" => Some(hooks.actors_restarts_total),
        "coroutines.live" => Some(coroutines.live),
        "coroutines.suspended" => Some(coroutines.suspended),
        "coroutines.resumes_total" => Some(coroutines.resumes_total),
        "coroutines.suspends_total" => Some(coroutines.suspends_total),
        "coroutines.frame_bytes_live" => Some(coroutines.frame_bytes_live),
        "threads.blocking_count" => Some(hooks.threads_blocking_count),
        "reactor.registrations_live" => Some(hooks.reactor_registrations_live),
        "reactor.ready_events_total" => Some(hooks.reactor_ready_events_total),
        "arena.resets_total" => Some(hooks.arena_resets_total),
        _ => None,
    }
}

fn observe_series() -> &'static [(&'static str, &'static str)] {
    &[
        ("heap.live_bytes", "gauge"),
        ("heap.allocated_total", "counter"),
        ("heap.freed_total", "counter"),
        ("heap.allocations_total", "counter"),
        ("heap.frees_total", "counter"),
        ("scheduler.workers", "gauge"),
        ("scheduler.runnable_actors", "gauge"),
        ("scheduler.runnable_coroutines", "gauge"),
        ("scheduler.queue_depth", "gauge"),
        ("scheduler.steals_total", "counter"),
        ("scheduler.parks_total", "counter"),
        ("scheduler.unparks_total", "counter"),
        ("actors.live", "gauge"),
        ("actors.turns_total", "counter"),
        ("actors.turn_duration_ns_total", "counter"),
        ("actors.attributed_turns_total", "counter"),
        ("actors.attributed_turn_duration_ns_total", "counter"),
        ("actors.crashes_total", "counter"),
        ("actors.restarts_total", "counter"),
        ("coroutines.live", "gauge"),
        ("coroutines.suspended", "gauge"),
        ("coroutines.resumes_total", "counter"),
        ("coroutines.suspends_total", "counter"),
        ("coroutines.frame_bytes_live", "gauge"),
        ("threads.blocking_count", "gauge"),
        ("reactor.registrations_live", "gauge"),
        ("reactor.ready_events_total", "counter"),
        ("arena.resets_total", "counter"),
    ]
}

/// Render a metric name into its Prometheus series name. Prometheus names
/// cannot contain `.`, so the scrape maps every `.` to `_`. This is the single
/// source of truth for that mapping: both the scrape render and the registry's
/// collision check must agree on it, or a user metric whose rendered name
/// equals a built-in's (`heap.live_bytes` vs `heap_live_bytes`) could slip
/// past a canonical-name check and corrupt the built-in's scrape output.
#[must_use]
pub fn render_prometheus_name(name: &str) -> String {
    name.replace('.', "_")
}

/// True when `name`'s *rendered* Prometheus series name collides with a runtime
/// built-in's rendered name.
///
/// The user-metric registry (`crate::metrics`) rejects any registration whose
/// rendered name collides with a built-in so a user metric cannot shadow
/// runtime telemetry in the shared scrape output. The check is on the rendered
/// form (`.` → `_`) rather than the canonical name because the scrape render is
/// non-injective: `heap_live_bytes` and `heap.live_bytes` are distinct
/// canonical names but render to the same series, and the second is the
/// built-in.
#[must_use]
pub fn rendered_name_collides_with_builtin(name: &str) -> bool {
    let rendered = render_prometheus_name(name);
    observe_series()
        .iter()
        .any(|(builtin, _)| render_prometheus_name(builtin) == rendered)
}

fn prometheus_label_value(value: &str) -> String {
    let mut escaped = String::with_capacity(value.len());
    for ch in value.chars() {
        match ch {
            '\\' => escaped.push_str("\\\\"),
            '"' => escaped.push_str("\\\""),
            '\n' => escaped.push_str("\\n"),
            _ => escaped.push(ch),
        }
    }
    escaped
}

#[cfg(all(not(target_arch = "wasm32"), feature = "profiler"))]
fn handler_name(dispatch_ptr: usize, msg_type: i32) -> Option<String> {
    crate::profiler::actor_registry::handler_name_by_ptr(dispatch_ptr, msg_type)
}

#[cfg(any(target_arch = "wasm32", not(feature = "profiler")))]
fn handler_name(_dispatch_ptr: usize, _msg_type: i32) -> Option<String> {
    None
}

fn push_attributed_turn_series(out: &mut String) {
    let mut turns = attributed_turns_snapshot();
    turns.sort_by_key(|((dispatch_ptr, msg_type), _)| (*dispatch_ptr, *msg_type));
    if turns.is_empty() {
        return;
    }

    out.push_str("# TYPE actors_attributed_turns_by_handler_total counter\n");
    for ((dispatch_ptr, msg_type), turn) in &turns {
        let handler = handler_name(*dispatch_ptr, *msg_type).unwrap_or_default();
        out.push_str("actors_attributed_turns_by_handler_total{dispatch=\"0x");
        write!(out, "{dispatch_ptr:x}").expect("write to String cannot fail");
        out.push_str("\",msg_type=\"");
        out.push_str(&msg_type.to_string());
        out.push_str("\",handler=\"");
        out.push_str(&prometheus_label_value(&handler));
        out.push_str("\"} ");
        out.push_str(&turn.turns_total.to_string());
        out.push('\n');
    }

    out.push_str("# TYPE actors_attributed_turn_duration_ns_by_handler_total counter\n");
    for ((dispatch_ptr, msg_type), turn) in &turns {
        let handler = handler_name(*dispatch_ptr, *msg_type).unwrap_or_default();
        out.push_str("actors_attributed_turn_duration_ns_by_handler_total{dispatch=\"0x");
        write!(out, "{dispatch_ptr:x}").expect("write to String cannot fail");
        out.push_str("\",msg_type=\"");
        out.push_str(&msg_type.to_string());
        out.push_str("\",handler=\"");
        out.push_str(&prometheus_label_value(&handler));
        out.push_str("\"} ");
        out.push_str(&turn.duration_ns_total.to_string());
        out.push('\n');
    }
}

/// Append the developer-defined metrics (`std::metrics`) to the scrape, in
/// Prometheus text form, alongside the runtime built-ins. Metric names already
/// have dots mapped to underscores by the registry snapshot; label values are
/// escaped through [`prometheus_label_value`] so a value carrying `"`, `\`, or
/// a newline cannot break the line format.
fn push_user_metric_series(out: &mut String) {
    for metric in crate::metrics::render_snapshot() {
        out.push_str("# TYPE ");
        out.push_str(&metric.prometheus_name);
        out.push(' ');
        out.push_str(metric.type_token);
        out.push('\n');
        if let Some(sum) = metric.histogram_sum {
            // A histogram must expose `_count` and `_sum` under the base name;
            // a bare sample under `# TYPE name histogram` is invalid exposition
            // and drops the sum. The Phase A scalar histogram carries no `le`
            // buckets, so emit the count and sum lines only. (Bucketed `_bucket`
            // lines arrive with the bucketed/labelled follow-on.)
            let count = metric.series.first().map_or(0, |(_, v)| *v);
            out.push_str(&metric.prometheus_name);
            out.push_str("_count ");
            out.push_str(&count.to_string());
            out.push('\n');
            out.push_str(&metric.prometheus_name);
            out.push_str("_sum ");
            out.push_str(&sum.to_string());
            out.push('\n');
            continue;
        }
        for (label_values, value) in &metric.series {
            out.push_str(&metric.prometheus_name);
            if !metric.label_keys.is_empty() {
                out.push('{');
                for (i, (key, val)) in metric
                    .label_keys
                    .iter()
                    .zip(label_values.iter())
                    .enumerate()
                {
                    if i > 0 {
                        out.push(',');
                    }
                    out.push_str(key);
                    out.push_str("=\"");
                    out.push_str(&prometheus_label_value(val));
                    out.push('"');
                }
                out.push('}');
            }
            out.push(' ');
            out.push_str(&value.to_string());
            out.push('\n');
        }
    }
    push_user_metric_self_series(out);
}

/// Append the registry self-metrics — the fail-closed drop/reject counters — so
/// a user discovers dropped data by reading the same scrape.
fn push_user_metric_self_series(out: &mut String) {
    let self_metrics = crate::metrics::self_metrics();
    for (name, value) in [
        (
            "hew_metrics_names_dropped_total",
            self_metrics.names_dropped,
        ),
        (
            "hew_metrics_series_dropped_total",
            self_metrics.series_dropped,
        ),
        ("hew_metrics_invalid_ops_total", self_metrics.invalid_ops),
        (
            "hew_metrics_collision_rejected_total",
            self_metrics.collision_rejected,
        ),
    ] {
        out.push_str("# TYPE ");
        out.push_str(name);
        out.push_str(" counter\n");
        out.push_str(name);
        out.push(' ');
        out.push_str(&value.to_string());
        out.push('\n');
    }
}

#[must_use]
pub fn scrape_text() -> String {
    let mut out = String::new();
    for (name, kind) in observe_series() {
        if let Some(value) = read_u64(name) {
            let prometheus_name = render_prometheus_name(name);
            out.push_str("# TYPE ");
            out.push_str(&prometheus_name);
            out.push(' ');
            out.push_str(kind);
            out.push('\n');
            out.push_str(&prometheus_name);
            out.push(' ');
            out.push_str(&value.to_string());
            out.push('\n');
        }
    }
    push_attributed_turn_series(&mut out);
    push_user_metric_series(&mut out);
    out
}

#[must_use]
pub fn series_text() -> String {
    let mut out = String::new();
    for (name, _) in observe_series() {
        out.push_str(name);
        out.push('\n');
    }
    out.push_str("actors.attributed_turns_by_handler_total\n");
    out.push_str("actors.attributed_turn_duration_ns_by_handler_total\n");
    // Developer-defined metrics (`std::metrics`), by canonical name, plus the
    // registry self-metrics — the same names the scrape renders.
    for metric in crate::metrics::render_snapshot() {
        out.push_str(&metric.canonical_name);
        out.push('\n');
    }
    out.push_str("hew_metrics_names_dropped_total\n");
    out.push_str("hew_metrics_series_dropped_total\n");
    out.push_str("hew_metrics_invalid_ops_total\n");
    out.push_str("hew_metrics_collision_rejected_total\n");
    out
}

fn u64_to_i64_saturating(value: u64) -> i64 {
    i64::try_from(value).unwrap_or(i64::MAX)
}

/// Read one metric by canonical name. Returns -1 when the metric is unknown.
///
/// # Safety
///
/// `name` must be null or a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_observe_read_u64(name: *const c_char) -> i64 {
    if name.is_null() {
        return -1;
    }
    // SAFETY: caller passes a valid C string per the function contract.
    let Ok(name) = unsafe { CStr::from_ptr(name) }.to_str() else {
        return -1;
    };
    read_u64(name).map_or(-1, u64_to_i64_saturating)
}

/// Return the current scrape text as an owned string.
///
/// # Ownership
///
/// The returned pointer is allocated with the Hew C-string allocator. Compiled
/// Hew `string` values release it through the normal string drop path; embedders
/// that call this C ABI directly may release it with [`hew_observe_string_free`].
#[no_mangle]
pub extern "C" fn hew_observe_scrape() -> *mut c_char {
    str_to_malloc(&scrape_text())
}

/// Return the canonical metric series names as newline-delimited text.
///
/// # Ownership
///
/// The returned pointer follows the same ownership rules as
/// [`hew_observe_scrape`].
#[no_mangle]
pub extern "C" fn hew_observe_series() -> *mut c_char {
    str_to_malloc(&series_text())
}

/// Release a string returned by observe C ABI helpers.
///
/// # Safety
///
/// `ptr` must be null or a pointer returned by an observe C ABI string helper.
#[no_mangle]
pub unsafe extern "C" fn hew_observe_string_free(ptr: *mut c_char) {
    // SAFETY: observe string helpers allocate through the header-aware Hew
    // C-string allocator and transfer ownership to the caller.
    unsafe { free_cstring(ptr) };
}

pub(crate) fn reset_all() {
    shard_reset(&HEAP_ALLOCATED_TOTAL);
    shard_reset(&HEAP_FREED_TOTAL);
    shard_reset(&HEAP_ALLOCATIONS_TOTAL);
    shard_reset(&HEAP_FREES_TOTAL);
    shard_reset(&ACTOR_TURNS_TOTAL);
    shard_reset(&ACTOR_TURN_DURATION_NS_TOTAL);
    SCHEDULER_PARKS_TOTAL.store(0, Ordering::Relaxed);
    SCHEDULER_UNPARKS_TOTAL.store(0, Ordering::Relaxed);
    COROUTINES_LIVE.store(0, Ordering::Relaxed);
    COROUTINES_SUSPENDED.store(0, Ordering::Relaxed);
    COROUTINES_RESUMES_TOTAL.store(0, Ordering::Relaxed);
    COROUTINES_SUSPENDS_TOTAL.store(0, Ordering::Relaxed);
    COROUTINES_FRAME_BYTES_LIVE.store(0, Ordering::Relaxed);
    REACTOR_REGISTRATIONS_LIVE.store(0, Ordering::Relaxed);
    REACTOR_READY_EVENTS_TOTAL.store(0, Ordering::Relaxed);
    ACTORS_CRASHES_TOTAL.store(0, Ordering::Relaxed);
    ACTORS_RESTARTS_TOTAL.store(0, Ordering::Relaxed);
    ARENA_RESETS_TOTAL.store(0, Ordering::Relaxed);
    THREADS_BLOCKING_COUNT.store(0, Ordering::Relaxed);
    ATTRIBUTED_TURNS.access(|slot| {
        if let Some(map) = slot.as_mut() {
            map.clear();
        }
    });
    // Clear the developer-defined metric registry on the same session
    // boundary so a fresh session never observes a prior session's metrics.
    crate::metrics::session_reset_metrics();
    #[cfg(not(target_arch = "wasm32"))]
    {
        let _guard = dispatch_barrier_lock();
        NEXT_DISPATCH_TICKET.store(0, Ordering::Release);
        PUBLISHED_DISPATCH_TICKET.store(0, Ordering::Release);
        DISPATCH_COMPLETION_WATERMARK.store(0, Ordering::Release);
        DISPATCH_BARRIER_WAITERS.store(0, Ordering::Release);
        for slot in &DISPATCH_ACTIVE_TICKETS {
            slot.store(0, Ordering::Release);
        }
        DISPATCH_BARRIER.cond.notify_all();
    }
}

pub(crate) fn register_reset_hooks() {
    use std::sync::Once;
    static ONCE: Once = Once::new();
    ONCE.call_once(|| crate::session::register_reset_hook(reset_all));
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CString;

    #[test]
    fn read_known_metric_and_unknown_sentinel() {
        let known = CString::new("heap.live_bytes").unwrap();
        let unknown = CString::new("does.not.exist").unwrap();
        // SAFETY: CStrings provide valid NUL-terminated names.
        unsafe {
            assert!(hew_observe_read_u64(known.as_ptr()) >= 0);
            assert_eq!(hew_observe_read_u64(unknown.as_ptr()), -1);
        }
    }

    #[test]
    fn scrape_contains_canonical_series() {
        let scrape = scrape_text();
        assert!(scrape.contains("heap_live_bytes"));
        assert!(scrape.contains("scheduler_queue_depth"));
    }

    #[test]
    fn scrape_renders_user_metrics_with_exact_values() {
        let _guard = crate::runtime_test_guard();
        reset_all();

        // A developer counter incremented five times, then a labelled gauge.
        let requests = crate::metrics::register_counter("app.requests");
        assert!(requests >= 0);
        for _ in 0..5 {
            crate::metrics::inc(requests);
        }

        let scrape = scrape_text();

        // The user counter renders with its exact value, dots → underscores,
        // and the correct # TYPE line — not `> 0`, the exact emitted bytes.
        assert!(
            scrape.contains("# TYPE app_requests counter"),
            "missing user-counter TYPE line:\n{scrape}"
        );
        assert!(
            scrape.contains("\napp_requests 5\n") || scrape.starts_with("app_requests 5\n"),
            "user counter must render the exact value `app_requests 5`:\n{scrape}"
        );

        // The registry self-metrics are present so a user can discover drops.
        assert!(scrape.contains("# TYPE hew_metrics_names_dropped_total counter"));

        // A built-in metric name is not corrupted by appending user metrics.
        assert!(scrape.contains("# TYPE heap_live_bytes gauge"));
        assert!(scrape.contains("scheduler_queue_depth"));

        // series() lists the canonical user-metric name alongside built-ins.
        let series = series_text();
        assert!(series.contains("\napp.requests\n") || series.contains("app.requests\n"));
        assert!(series.contains("heap.live_bytes"));
    }

    #[test]
    fn scrape_escapes_user_label_values() {
        let _guard = crate::runtime_test_guard();
        reset_all();

        let base = crate::metrics::register_labelled(
            "app.routed",
            crate::metrics::MetricKind::Counter,
            &["path".to_string()],
        );
        assert!(base >= 0);
        // A label value carrying a quote must be escaped so the line stays
        // well-formed Prometheus text.
        let series = crate::metrics::resolve_series(base, &["a\"b".to_string()]);
        assert!(series >= 0);
        crate::metrics::inc(series);

        let scrape = scrape_text();
        assert!(
            scrape.contains("app_routed{path=\"a\\\"b\"} 1"),
            "label value must be quote-escaped and carry the exact value:\n{scrape}"
        );
    }

    #[test]
    fn scrape_renders_histogram_as_valid_count_and_sum() {
        let _guard = crate::runtime_test_guard();
        reset_all();

        // A scalar histogram with three observations: 5 + 42 + 500 = 547.
        let durations = crate::metrics::register_histogram("app.request_duration", &[]);
        assert!(durations >= 0);
        crate::metrics::histogram_record(durations, 5);
        crate::metrics::histogram_record(durations, 42);
        crate::metrics::histogram_record(durations, 500);

        let scrape = scrape_text();

        // Valid Prometheus histogram exposition: a `# TYPE ... histogram` line
        // followed by `_count` and `_sum` samples under the base name — not a
        // bare sample, which Prometheus rejects and which drops the sum.
        assert!(
            scrape.contains("# TYPE app_request_duration histogram"),
            "missing histogram TYPE line:\n{scrape}"
        );
        assert!(
            scrape.contains("\napp_request_duration_count 3\n"),
            "histogram must emit an exact _count sample:\n{scrape}"
        );
        assert!(
            scrape.contains("\napp_request_duration_sum 547\n"),
            "histogram must emit an exact _sum sample:\n{scrape}"
        );
        // The invalid bare sample (`name <value>` under TYPE histogram) must NOT
        // appear: every histogram line carries the `_count` / `_sum` suffix.
        assert!(
            !scrape.contains("\napp_request_duration 3\n"),
            "histogram must not emit a bare sample under TYPE histogram:\n{scrape}"
        );
    }

    unsafe extern "C-unwind" fn fake_dispatch(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _s: *mut c_void,
        _m: i32,
        _p: *mut c_void,
        _n: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        std::ptr::null_mut()
    }

    #[test]
    fn scrape_includes_attributed_turns_by_handler() {
        let _guard = crate::runtime_test_guard();
        reset_all();
        set_hot_tier_enabled(true);
        let dispatch = fake_dispatch as *const c_void;
        hew_observe_probe_turn(dispatch, 7, 11);
        hew_observe_probe_turn(dispatch, 7, 13);

        let scrape = scrape_text();
        assert!(scrape.contains("actors_attributed_turns_by_handler_total"));
        assert!(scrape.contains("msg_type=\"7\""));
        assert!(scrape.contains(" 2\n"));
        assert!(scrape.contains("actors_attributed_turn_duration_ns_by_handler_total"));
        assert!(scrape.contains(" 24\n"));
        assert_eq!(read_u64("actors.attributed_turns_total"), Some(2));
        assert_eq!(
            read_u64("actors.attributed_turn_duration_ns_total"),
            Some(24)
        );

        set_hot_tier_enabled(false);
        reset_all();
    }

    #[test]
    fn observe_scrape_string_free_uses_header_aware_release() {
        let ptr = hew_observe_scrape();
        assert!(!ptr.is_null());
        // SAFETY: ptr was returned by hew_observe_scrape and is released once.
        unsafe { hew_observe_string_free(ptr) };
    }

    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn observe_barrier_returns_immediately_when_already_attributed() {
        let _guard = crate::runtime_test_guard();
        reset_all();
        set_current_worker_shard(0);
        let ticket = observe_dispatch_begin();
        observe_dispatch_attributed(ticket);

        let started = Instant::now();
        assert_eq!(hew_observe_barrier(), OBSERVE_BARRIER_OK);
        assert!(
            started.elapsed() < Duration::from_millis(100),
            "already-attributed barrier should not wait for a future dispatch"
        );
    }

    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn observe_barrier_wakes_when_concurrent_dispatch_is_attributed() {
        let _guard = crate::runtime_test_guard();
        reset_all();
        set_current_worker_shard(0);
        let ticket = observe_dispatch_begin();

        let worker = std::thread::spawn(move || {
            std::thread::sleep(Duration::from_millis(25));
            observe_dispatch_attributed(ticket);
        });

        let started = Instant::now();
        assert_eq!(hew_observe_barrier(), OBSERVE_BARRIER_OK);
        assert!(
            started.elapsed() < Duration::from_secs(1),
            "barrier should wake on attribution notify"
        );
        worker.join().unwrap();
    }

    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn observe_barrier_holds_for_earlier_ticket_when_later_ticket_closes_first() {
        let _guard = crate::runtime_test_guard();
        reset_all();
        set_current_worker_shard(0);
        let ticket_a = observe_dispatch_begin();
        let (done_tx, done_rx) = std::sync::mpsc::channel();

        let waiter = std::thread::spawn(move || {
            done_tx.send(hew_observe_barrier()).unwrap();
        });

        let wait_started = Instant::now();
        while DISPATCH_BARRIER_WAITERS.load(Ordering::Acquire) == 0 {
            assert!(
                wait_started.elapsed() < Duration::from_secs(1),
                "barrier did not start waiting"
            );
            std::thread::yield_now();
        }

        set_current_worker_shard(1);
        let ticket_b = observe_dispatch_begin();
        observe_dispatch_attributed(ticket_b);

        std::thread::sleep(Duration::from_millis(50));
        assert!(
            done_rx.try_recv().is_err(),
            "later ticket must not release a barrier targeting an earlier incomplete ticket"
        );

        set_current_worker_shard(0);
        observe_dispatch_attributed(ticket_a);
        assert_eq!(
            done_rx.recv_timeout(Duration::from_secs(1)).unwrap(),
            OBSERVE_BARRIER_OK
        );
        waiter.join().unwrap();
    }

    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn observe_dispatch_close_uses_no_mutex_without_waiters() {
        let _guard = crate::runtime_test_guard();
        reset_all();
        DISPATCH_BARRIER_MUTEX_ACQUISITIONS.store(0, Ordering::Relaxed);
        set_current_worker_shard(0);

        let ticket = observe_dispatch_begin();
        observe_dispatch_attributed(ticket);

        assert_eq!(
            DISPATCH_BARRIER_MUTEX_ACQUISITIONS.load(Ordering::Relaxed),
            0,
            "dispatch begin/close should stay on the atomic fast path when no barrier is waiting"
        );
    }

    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn observe_barrier_fails_fast_from_worker_context() {
        let _guard = crate::runtime_test_guard();
        reset_all();
        let mut ctx =
            std::mem::MaybeUninit::<crate::execution_context::HewExecutionContext>::uninit();
        let prev = crate::execution_context::set_current_context(ctx.as_mut_ptr());

        assert_eq!(hew_observe_barrier(), OBSERVE_BARRIER_ERR_WORKER_CONTEXT);

        let restored = crate::execution_context::set_current_context(prev);
        assert_eq!(restored, ctx.as_mut_ptr());
    }

    #[test]
    fn sharded_hot_counter_sum_on_read() {
        let _guard = crate::runtime_test_guard();
        reset_all();
        set_hot_tier_enabled(true);
        set_current_worker_shard(0);
        record_actor_turn(3);
        set_current_worker_shard(1);
        record_actor_turn(5);
        set_current_worker_shard(usize::MAX);
        record_actor_turn(11);

        let hot = hot_counters_snapshot();
        assert_eq!(hot.actor_turns_total, 3);
        assert_eq!(hot.actor_turn_duration_ns_total, 19);

        set_hot_tier_enabled(false);
        reset_all();
    }

    #[test]
    fn coroutine_frame_bytes_balance_across_alloc_free() {
        let _guard = crate::runtime_test_guard();
        reset_all();
        record_coroutine_frame_alloc(64);
        record_coroutine_frame_alloc(128);
        let live = coroutine_snapshot();
        assert_eq!(live.live, 2);
        assert_eq!(live.frame_bytes_live, 192);

        record_coroutine_frame_free(64);
        let after_one_free = coroutine_snapshot();
        assert_eq!(after_one_free.live, 1);
        assert_eq!(after_one_free.frame_bytes_live, 128);

        record_coroutine_frame_free(128);
        let balanced = coroutine_snapshot();
        assert_eq!(balanced.live, 0);
        assert_eq!(balanced.frame_bytes_live, 0);
    }

    #[test]
    fn parks_and_suspends_count_separate_invariants() {
        let _guard = crate::runtime_test_guard();
        reset_all();
        record_scheduler_park();
        record_scheduler_unpark();
        assert_eq!(scheduler_parks_total(), 1);
        assert_eq!(scheduler_unparks_total(), 1);
        assert_eq!(coroutine_snapshot().suspends_total, 0);

        record_coroutine_frame_alloc(96);
        record_coroutine_suspend();
        record_coroutine_resume();
        record_coroutine_frame_free(96);

        let snapshot = coroutine_snapshot();
        assert_eq!(scheduler_parks_total(), 1);
        assert_eq!(scheduler_unparks_total(), 1);
        assert_eq!(snapshot.suspends_total, 1);
        assert_eq!(snapshot.resumes_total, 1);
        assert_eq!(snapshot.suspended, 0);
        assert_eq!(snapshot.live, 0);
        assert_eq!(snapshot.frame_bytes_live, 0);
    }

    #[test]
    fn series_lists_static_and_attributed_metric_names() {
        let series = series_text();
        assert!(series.contains("heap.live_bytes\n"));
        assert!(series.contains("actors.attributed_turns_by_handler_total\n"));
    }
}
