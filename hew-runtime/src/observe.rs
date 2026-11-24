//! Canonical runtime observability registry.
//!
//! Cheap gauges stay live at all times. Hot counters (per-allocation and
//! per-turn) are opt-in through `HEW_OBSERVE` or the runtime toggle below and
//! are sharded by worker to avoid cache-line contention when enabled.

use std::cell::Cell;
use std::collections::HashMap;
use std::ffi::c_void;
use std::fmt::Write as _;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::{Condvar, Mutex};

use crate::actor::HEW_MAX_WORKERS;
use crate::lifetime::PoisonSafe;
use crate::util::MutexExt;
use hew_cabi::string::{string_as_str, string_from_str, HewString};

const SHARD_COUNT: usize = HEW_MAX_WORKERS + 1;
pub(crate) const OBSERVE_BARRIER_OK: i64 = 0;

thread_local! {
    static WORKER_SHARD: Cell<usize> = const { Cell::new(0) };
}

static HOT_TIER_ENABLED: AtomicBool = AtomicBool::new(false);

// Keep the counters written by one worker together, away from other workers'
// cache lines. Heap accounting is hot enough that packed atomic arrays cause
// false sharing even when every worker uses a distinct index.
#[repr(align(128))]
struct HeapCounters {
    allocated: AtomicU64,
    freed: AtomicU64,
    allocations: AtomicU64,
    frees: AtomicU64,
}

impl HeapCounters {
    const fn new() -> Self {
        Self {
            allocated: AtomicU64::new(0),
            freed: AtomicU64::new(0),
            allocations: AtomicU64::new(0),
            frees: AtomicU64::new(0),
        }
    }
}

static HEAP_COUNTERS: [HeapCounters; SHARD_COUNT] = [const { HeapCounters::new() }; SHARD_COUNT];

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
static SUPERVISOR_GIVEUPS_TOTAL: AtomicU64 = AtomicU64::new(0);
/// Crash counts by `(trap_kind, actor_type)`, the labels of
/// [`crate::failure_kinds::CRASH_SERIES`].
static CRASHES_BY_KIND: crate::lifetime::PoisonSafe<Vec<((&'static str, String), u64)>> =
    crate::lifetime::PoisonSafe::new(Vec::new());
static ACTORS_RESTARTS_TOTAL: AtomicU64 = AtomicU64::new(0);
static ARENA_RESETS_TOTAL: AtomicU64 = AtomicU64::new(0);
static THREADS_BLOCKING_COUNT: AtomicU64 = AtomicU64::new(0);
static IO_HANDLES_LIVE: AtomicU64 = AtomicU64::new(0);
static POOL_THREADS: AtomicU64 = AtomicU64::new(0);
static POOL_SATURATIONS_TOTAL: AtomicU64 = AtomicU64::new(0);

static ATTRIBUTED_TURNS: PoisonSafe<Option<HashMap<(usize, i32), AttributedTurn>>> =
    PoisonSafe::new(None);

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
    pub io_handles_live: u64,
    pub pool_threads: u64,
    pub pool_saturations_total: u64,
}

#[must_use]
pub fn observe_hot_tier_enabled() -> bool {
    HOT_TIER_ENABLED.load(Ordering::Acquire)
}

pub fn set_hot_tier_enabled(enabled: bool) {
    HOT_TIER_ENABLED.store(enabled, Ordering::Release);
}

#[inline]
pub(crate) fn current_shard() -> usize {
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

pub(crate) fn record_actor_turn(duration_ns: u64) {
    if observe_hot_tier_enabled() {
        shard_add(&ACTOR_TURNS_TOTAL, 1);
        shard_add(&ACTOR_TURN_DURATION_NS_TOTAL, duration_ns);
    }
}

#[cfg(test)]
pub(crate) fn coroutine_frame_bytes_live_for_test() -> u64 {
    COROUTINES_FRAME_BYTES_LIVE.load(Ordering::Relaxed)
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

/// Count one actor crash under its kind and actor type.
pub(crate) fn record_actor_crash(kind: &str, actor_type: &str) {
    ACTORS_CRASHES_TOTAL.fetch_add(1, Ordering::Relaxed);
    let kind = crate::failure_kinds::TRAP_KIND_LABEL_VALUES
        .iter()
        .copied()
        .find(|known| *known == kind)
        .unwrap_or(crate::failure_kinds::KIND_UNKNOWNFAULT);
    CRASHES_BY_KIND.access(|counts| {
        match counts
            .iter_mut()
            .find(|((k, t), _)| *k == kind && t == actor_type)
        {
            Some((_, count)) => *count += 1,
            None => counts.push(((kind, actor_type.to_owned()), 1)),
        }
    });
}

/// Count a supervisor giving up on a child.
pub(crate) fn record_supervisor_giveup() {
    SUPERVISOR_GIVEUPS_TOTAL.fetch_add(1, Ordering::Relaxed);
}

/// The calling thread's worker index plus one; zero off the worker pool.
#[must_use]
pub(crate) fn current_worker_id() -> u32 {
    u32::try_from(current_shard()).unwrap_or(u32::MAX)
}

pub(crate) fn record_actor_restart() {
    ACTORS_RESTARTS_TOTAL.fetch_add(1, Ordering::Relaxed);
}

pub(crate) fn record_arena_reset() {
    ARENA_RESETS_TOTAL.fetch_add(1, Ordering::Relaxed);
}

#[no_mangle]
pub extern "C" fn hew_observe_hot_tier_enabled() -> i32 {
    i32::from(observe_hot_tier_enabled())
}

/// Wait until in-flight observe attribution settles (no-op on `wasm32`).
#[no_mangle]
pub extern "C" fn hew_observe_barrier() -> i64 {
    crate::platform::host_io::observe_barrier()
}

/// The message type each worker shard is dispatching, for failure records and
/// the hang snapshot. Zero when the shard is between turns.
static TURN_MSG_TYPE: [std::sync::atomic::AtomicI32; SHARD_COUNT] =
    [const { std::sync::atomic::AtomicI32::new(0) }; SHARD_COUNT];
/// The actor each worker shard is dispatching, or zero.
static TURN_ACTOR: [AtomicU64; SHARD_COUNT] = [const { AtomicU64::new(0) }; SHARD_COUNT];
/// The dispatch function of each shard's actor, the handler registry's key.
static TURN_DISPATCH: [AtomicU64; SHARD_COUNT] = [const { AtomicU64::new(0) }; SHARD_COUNT];

static NEXT_DISPATCH_TICKET: AtomicU64 = AtomicU64::new(0);
static PUBLISHED_DISPATCH_TICKET: AtomicU64 = AtomicU64::new(0);
static DISPATCH_BARRIER_WAITERS: AtomicU64 = AtomicU64::new(0);
static DISPATCH_ACTIVE_TICKETS: [AtomicU64; SHARD_COUNT] =
    [const { AtomicU64::new(0) }; SHARD_COUNT];
#[cfg(test)]
static DISPATCH_BARRIER_MUTEX_ACQUISITIONS: AtomicU64 = AtomicU64::new(0);

static DISPATCH_BARRIER: DispatchBarrier = DispatchBarrier {
    lock: Mutex::new(()),
    cond: Condvar::new(),
};

struct DispatchBarrier {
    lock: Mutex<()>,
    cond: Condvar,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct ObserveDispatchTicket(u64);

fn dispatch_barrier_lock() -> std::sync::MutexGuard<'static, ()> {
    #[cfg(test)]
    DISPATCH_BARRIER_MUTEX_ACQUISITIONS.fetch_add(1, Ordering::Relaxed);
    DISPATCH_BARRIER.lock.lock_or_recover()
}

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

pub(crate) fn observe_dispatch_begin(
    actor_id: u64,
    dispatch: usize,
    msg_type: i32,
) -> ObserveDispatchTicket {
    let shard = current_shard();
    TURN_ACTOR[shard].store(actor_id, Ordering::Relaxed);
    TURN_DISPATCH[shard].store(dispatch as u64, Ordering::Relaxed);
    TURN_MSG_TYPE[shard].store(msg_type, Ordering::Relaxed);
    let ticket = NEXT_DISPATCH_TICKET
        .fetch_add(1, Ordering::Relaxed)
        .saturating_add(1);
    DISPATCH_ACTIVE_TICKETS[current_shard()].store(ticket, Ordering::Release);
    publish_dispatch_begin(ticket);
    ObserveDispatchTicket(ticket)
}

fn clear_active_dispatch_ticket(ticket: ObserveDispatchTicket) {
    let shard = current_shard();
    TURN_ACTOR[shard].store(0, Ordering::Relaxed);
    TURN_MSG_TYPE[shard].store(0, Ordering::Relaxed);
    if DISPATCH_ACTIVE_TICKETS[shard]
        .compare_exchange(ticket.0, 0, Ordering::SeqCst, Ordering::Relaxed)
        .is_ok()
    {
        return;
    }

    for slot in &DISPATCH_ACTIVE_TICKETS {
        if slot
            .compare_exchange(ticket.0, 0, Ordering::SeqCst, Ordering::Relaxed)
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

fn observe_dispatch_close(ticket: ObserveDispatchTicket) {
    clear_active_dispatch_ticket(ticket);

    if DISPATCH_BARRIER_WAITERS.load(Ordering::SeqCst) > 0 {
        let _guard = dispatch_barrier_lock();
        DISPATCH_BARRIER.cond.notify_all();
    }
}

pub(crate) fn observe_dispatch_attributed(ticket: ObserveDispatchTicket) {
    observe_dispatch_close(ticket);
}

pub(crate) fn observe_dispatch_abandon(ticket: ObserveDispatchTicket) {
    observe_dispatch_close(ticket);
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
    let mut snapshot = HotCounterSnapshot {
        actor_turns_total: shard_sum(&ACTOR_TURNS_TOTAL),
        actor_turn_duration_ns_total: shard_sum(&ACTOR_TURN_DURATION_NS_TOTAL),
        ..HotCounterSnapshot::default()
    };
    for counters in &HEAP_COUNTERS {
        snapshot.heap_allocated_total += counters.allocated.load(Ordering::Relaxed);
        snapshot.heap_freed_total += counters.freed.load(Ordering::Relaxed);
        snapshot.heap_allocations_total += counters.allocations.load(Ordering::Relaxed);
        snapshot.heap_frees_total += counters.frees.load(Ordering::Relaxed);
    }
    snapshot
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
    ATTRIBUTED_TURNS.access(|slot| {
        slot.as_ref().map_or_else(AttributedTurn::default, |turns| {
            turns
                .values()
                .fold(AttributedTurn::default(), |mut totals, turn| {
                    totals.turns_total = totals.turns_total.saturating_add(turn.turns_total);
                    totals.duration_ns_total = totals
                        .duration_ns_total
                        .saturating_add(turn.duration_ns_total);
                    totals
                })
        })
    })
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
        io_handles_live: IO_HANDLES_LIVE.load(Ordering::Relaxed),
        pool_threads: POOL_THREADS.load(Ordering::Relaxed),
        pool_saturations_total: POOL_SATURATIONS_TOTAL.load(Ordering::Relaxed),
    }
}

fn heap_counter_sum(field: impl Fn(&HeapCounters) -> &AtomicU64) -> u64 {
    HEAP_COUNTERS
        .iter()
        .map(|counters| field(counters).load(Ordering::Relaxed))
        .sum()
}

#[must_use]
pub fn read_u64(name: &str) -> Option<u64> {
    match name {
        "observe.hot_enabled" => Some(u64::from(observe_hot_tier_enabled())),
        "heap.allocated_total" => Some(heap_counter_sum(|counters| &counters.allocated)),
        "heap.freed_total" => Some(heap_counter_sum(|counters| &counters.freed)),
        "heap.allocations_total" => Some(heap_counter_sum(|counters| &counters.allocations)),
        "heap.frees_total" => Some(heap_counter_sum(|counters| &counters.frees)),
        "scheduler.parks_total" => Some(crate::observe::scheduler_parks_total()),
        "scheduler.unparks_total" => Some(crate::observe::scheduler_unparks_total()),
        "actors.turns_total" => Some(shard_sum(&ACTOR_TURNS_TOTAL)),
        "actors.turn_duration_ns_total" => Some(shard_sum(&ACTOR_TURN_DURATION_NS_TOTAL)),
        "actors.attributed_turns_total" => Some(attributed_turns_totals().turns_total),
        "actors.attributed_turn_duration_ns_total" => {
            Some(attributed_turns_totals().duration_ns_total)
        }
        "actors.crashes_total" => Some(ACTORS_CRASHES_TOTAL.load(Ordering::Relaxed)),
        "actors.restarts_total" => Some(ACTORS_RESTARTS_TOTAL.load(Ordering::Relaxed)),
        "supervisor.giveups_total" => Some(SUPERVISOR_GIVEUPS_TOTAL.load(Ordering::Relaxed)),
        "coroutines.live" => Some(COROUTINES_LIVE.load(Ordering::Relaxed)),
        "coroutines.suspended" => Some(COROUTINES_SUSPENDED.load(Ordering::Relaxed)),
        "coroutines.resumes_total" => Some(COROUTINES_RESUMES_TOTAL.load(Ordering::Relaxed)),
        "coroutines.suspends_total" => Some(COROUTINES_SUSPENDS_TOTAL.load(Ordering::Relaxed)),
        "coroutines.frame_bytes_live" => Some(COROUTINES_FRAME_BYTES_LIVE.load(Ordering::Relaxed)),
        "threads.blocking_count" => Some(THREADS_BLOCKING_COUNT.load(Ordering::Relaxed)),
        "reactor.registrations_live" => Some(REACTOR_REGISTRATIONS_LIVE.load(Ordering::Relaxed)),
        "reactor.ready_events_total" => Some(REACTOR_READY_EVENTS_TOTAL.load(Ordering::Relaxed)),
        "io.handles_live" => Some(IO_HANDLES_LIVE.load(Ordering::Relaxed)),
        "pool.threads" => Some(POOL_THREADS.load(Ordering::Relaxed)),
        "pool.saturations_total" => Some(POOL_SATURATIONS_TOTAL.load(Ordering::Relaxed)),
        "arena.resets_total" => Some(ARENA_RESETS_TOTAL.load(Ordering::Relaxed)),
        _ => crate::platform::host_io::read_host_observe_u64(name)
            .or_else(|| crate::metrics::read_u64(name)),
    }
}

fn observe_series() -> &'static [(&'static str, &'static str)] {
    &[
        ("observe.hot_enabled", "gauge"),
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
        ("supervisor.giveups_total", "counter"),
        ("mailbox.depth", "gauge"),
        ("mailbox.depth_max", "gauge"),
        ("mailbox.depth_p99", "gauge"),
        ("coroutines.live", "gauge"),
        ("coroutines.suspended", "gauge"),
        ("coroutines.resumes_total", "counter"),
        ("coroutines.suspends_total", "counter"),
        ("coroutines.frame_bytes_live", "gauge"),
        ("threads.blocking_count", "gauge"),
        ("reactor.registrations_live", "gauge"),
        ("reactor.ready_events_total", "counter"),
        ("io.handles_live", "gauge"),
        ("pool.threads", "gauge"),
        ("pool.saturations_total", "counter"),
        ("arena.resets_total", "counter"),
        ("node.peers", "gauge"),
        ("node.routes", "gauge"),
        ("node.gossip_in_total", "counter"),
        ("node.gossip_out_total", "counter"),
        ("node.pending_asks", "gauge"),
        ("swim.alive", "gauge"),
        ("swim.suspect", "gauge"),
        ("swim.dead", "gauge"),
        ("swim.quarantined", "gauge"),
        ("registry.names", "gauge"),
        ("registry.conflicts_total", "counter"),
    ]
}

/// Rendered Prometheus series names of the per-handler attributed-turn series
/// the scrape emits from [`push_attributed_turn_series`]. These are already in
/// rendered form (`_`, never `.`); they are the base names of the labelled
/// `{dispatch,msg_type,handler}` series. A user metric whose rendered name
/// equals one of these would emit a duplicate `# TYPE` block and shadow the
/// runtime's per-handler telemetry, so the registry rejects it.
const ATTRIBUTED_TURN_SERIES_NAMES: [&str; 2] = [
    "actors_attributed_turns_by_handler_total",
    "actors_attributed_turn_duration_ns_by_handler_total",
];

/// Canonical (dotted) names of the attributed-turn series, for `series_text`.
const ATTRIBUTED_TURN_SERIES_CANONICAL: [&str; 2] = [
    "actors.attributed_turns_by_handler_total",
    "actors.attributed_turn_duration_ns_by_handler_total",
];

/// Rendered Prometheus series name of the per-child supervisor restart series
/// the scrape emits from [`push_restarts_by_child_series`]. Already in
/// rendered form (`_`, never `.`); the base name of the labelled
/// `{supervisor,child}` series. A user metric whose rendered name equals this
/// would emit a duplicate `# TYPE` block and shadow the runtime's per-child
/// restart telemetry, so the registry rejects it.
const SUPERVISOR_RESTART_SERIES_NAMES: [&str; 1] = ["supervisor_restarts_by_child_total"];

/// Canonical (dotted) name of the supervisor restart series, for `series_text`.
const SUPERVISOR_RESTART_SERIES_CANONICAL: [&str; 1] = ["supervisor.restarts_by_child_total"];

const MAILBOX_LOSS_SERIES: [&str; 4] = [
    "mailbox.dropped_new_total",
    "mailbox.dropped_old_total",
    "mailbox.coalesced_total",
    "mailbox.dropped_on_stop_total",
];

/// Rendered Prometheus series names of the user-metric registry self-metrics
/// the scrape emits from [`push_user_metric_self_series`]. Already in rendered
/// form. A user metric that shadowed one of these would mask the very
/// fail-closed drop/reject counters that tell an operator data was dropped, so
/// the registry rejects it.
const SELF_METRIC_NAMES: [&str; 4] = [
    "hew_metrics_names_dropped_total",
    "hew_metrics_series_dropped_total",
    "hew_metrics_invalid_ops_total",
    "hew_metrics_collision_rejected_total",
];

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

/// Every rendered Prometheus series name the scrape owns. This is the single
/// authoritative set the registry's collision check consults so that *no*
/// scrape-owned name can be shadowed by a user metric — the `observe_series()`
/// built-ins (rendered), the per-handler attributed-turn series, the per-child
/// supervisor restart series, and the user-metric registry self-metrics.
///
/// Returning rendered names keeps the collision check on the rendered form (see
/// [`rendered_name_collides_with_builtin`]): the scrape render is non-injective,
/// so a distinct dotted canonical name can still collapse onto a reserved
/// rendered series.
fn reserved_rendered_names() -> impl Iterator<Item = String> {
    observe_series()
        .iter()
        .map(|(builtin, _)| render_prometheus_name(builtin))
        .chain(
            ATTRIBUTED_TURN_SERIES_NAMES
                .iter()
                .map(|s| (*s).to_string()),
        )
        .chain(
            SUPERVISOR_RESTART_SERIES_NAMES
                .iter()
                .map(|s| (*s).to_string()),
        )
        .chain(
            MAILBOX_LOSS_SERIES
                .iter()
                .map(|name| render_prometheus_name(name)),
        )
        .chain(SELF_METRIC_NAMES.iter().map(|s| (*s).to_string()))
}

/// True when `name`'s *rendered* Prometheus series name collides with any
/// scrape-owned reserved rendered name.
///
/// The user-metric registry (`crate::metrics`) rejects any registration whose
/// rendered name collides with a reserved name so a user metric cannot shadow
/// runtime telemetry in the shared scrape output. The reserved set covers every
/// series the scrape emits itself — the `observe_series()` built-ins, the
/// per-handler attributed-turn series, and the registry self-metrics. The check
/// is on the rendered form (`.` → `_`) rather than the canonical name because
/// the scrape render is non-injective: `heap_live_bytes` and `heap.live_bytes`
/// are distinct canonical names but render to the same series, and the second
/// is the built-in.
#[must_use]
pub fn rendered_name_collides_with_builtin(name: &str) -> bool {
    let rendered = render_prometheus_name(name);
    reserved_rendered_names().any(|reserved| reserved == rendered)
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

fn push_attributed_turn_series(out: &mut String) {
    let mut turns = attributed_turns_snapshot();
    turns.sort_by_key(|((dispatch_ptr, msg_type), _)| (*dispatch_ptr, *msg_type));
    if turns.is_empty() {
        return;
    }

    out.push_str("# TYPE ");
    out.push_str(ATTRIBUTED_TURN_SERIES_NAMES[0]);
    out.push_str(" counter\n");
    for ((dispatch_ptr, msg_type), turn) in &turns {
        let handler = crate::platform::host_io::observe_handler_name(*dispatch_ptr, *msg_type)
            .unwrap_or_default();
        out.push_str(ATTRIBUTED_TURN_SERIES_NAMES[0]);
        out.push_str("{dispatch=\"0x");
        write!(out, "{dispatch_ptr:x}").expect("write to String cannot fail");
        out.push_str("\",msg_type=\"");
        out.push_str(&msg_type.to_string());
        out.push_str("\",handler=\"");
        out.push_str(&prometheus_label_value(&handler));
        out.push_str("\"} ");
        out.push_str(&turn.turns_total.to_string());
        out.push('\n');
    }

    out.push_str("# TYPE ");
    out.push_str(ATTRIBUTED_TURN_SERIES_NAMES[1]);
    out.push_str(" counter\n");
    for ((dispatch_ptr, msg_type), turn) in &turns {
        let handler = crate::platform::host_io::observe_handler_name(*dispatch_ptr, *msg_type)
            .unwrap_or_default();
        out.push_str(ATTRIBUTED_TURN_SERIES_NAMES[1]);
        out.push_str("{dispatch=\"0x");
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

/// The crash counter, labelled by kind and actor type. Every kind in the
/// generated list appears, at zero when no actor crashed with it, so a query
/// for one kind does not depend on it having happened.
fn push_crash_series(out: &mut String) {
    use crate::failure_kinds::{CRASH_LABEL_KEYS, CRASH_SERIES, TRAP_KIND_LABEL_VALUES};
    let mut counts = CRASHES_BY_KIND.access(|counts| counts.clone());
    counts.sort();
    out.push_str("# TYPE ");
    out.push_str(CRASH_SERIES);
    out.push_str(" counter\n");
    let mut line = |kind: &str, actor_type: &str, value: u64| {
        let _ = writeln!(
            out,
            "{CRASH_SERIES}{{{}=\"{}\",{}=\"{}\"}} {value}",
            CRASH_LABEL_KEYS[0],
            prometheus_label_value(kind),
            CRASH_LABEL_KEYS[1],
            prometheus_label_value(actor_type),
        );
    };
    for kind in TRAP_KIND_LABEL_VALUES {
        if !counts.iter().any(|((counted, _), _)| counted == kind) {
            line(kind, "", 0);
        }
    }
    for ((kind, actor_type), count) in &counts {
        line(kind, actor_type, *count);
    }
}

fn push_restarts_by_child_series(out: &mut String) {
    let rows = crate::platform::host_io::supervisor_restarts_by_child_snapshot();
    if rows.is_empty() {
        return;
    }

    out.push_str("# TYPE ");
    out.push_str(SUPERVISOR_RESTART_SERIES_NAMES[0]);
    out.push_str(" counter\n");
    for (supervisor, child, restarts) in &rows {
        out.push_str(SUPERVISOR_RESTART_SERIES_NAMES[0]);
        out.push_str("{supervisor=\"");
        out.push_str(&prometheus_label_value(supervisor));
        out.push_str("\",child=\"");
        out.push_str(&prometheus_label_value(child));
        out.push_str("\"} ");
        out.push_str(&restarts.to_string());
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
    // Values are paired with `SELF_METRIC_NAMES` positionally, so the rendered
    // names stay the single source the reserved-name set also consults.
    let values = [
        self_metrics.names_dropped,
        self_metrics.series_dropped,
        self_metrics.invalid_ops,
        self_metrics.collision_rejected,
    ];
    for (name, value) in SELF_METRIC_NAMES.iter().zip(values) {
        out.push_str("# TYPE ");
        out.push_str(name);
        out.push_str(" counter\n");
        out.push_str(name);
        out.push(' ');
        out.push_str(&value.to_string());
        out.push('\n');
    }
}

fn push_mailbox_loss_series(out: &mut String) {
    crate::platform::host_io::append_mailbox_loss_series(out, &MAILBOX_LOSS_SERIES);
}

#[must_use]
pub fn scrape_text() -> String {
    let mut out = String::new();
    for (name, kind) in observe_series() {
        if let Some(value) = read_u64(name) {
            let prometheus_name = render_prometheus_name(name);
            if prometheus_name == crate::failure_kinds::CRASH_SERIES {
                push_crash_series(&mut out);
                continue;
            }
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
    push_restarts_by_child_series(&mut out);
    push_mailbox_loss_series(&mut out);
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
    for name in ATTRIBUTED_TURN_SERIES_CANONICAL {
        out.push_str(name);
        out.push('\n');
    }
    for name in SUPERVISOR_RESTART_SERIES_CANONICAL {
        out.push_str(name);
        out.push('\n');
    }
    for name in MAILBOX_LOSS_SERIES {
        out.push_str(name);
        out.push('\n');
    }
    // Developer-defined metrics (`std::metrics`), by canonical name, plus the
    // registry self-metrics — the same names the scrape renders.
    for metric in crate::metrics::render_snapshot() {
        out.push_str(&metric.canonical_name);
        out.push('\n');
    }
    for name in SELF_METRIC_NAMES {
        out.push_str(name);
        out.push('\n');
    }
    out
}

fn u64_to_i64_saturating(value: u64) -> i64 {
    i64::try_from(value).unwrap_or(i64::MAX)
}

/// Read one metric by canonical name. Returns -1 when the metric is unknown.
///
/// # Safety
///
/// `name` must be null (canonical empty) or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_observe_read_u64(name: *const HewString) -> i64 {
    // SAFETY: the caller keeps the managed owner alive for this borrow.
    let name = unsafe { string_as_str(name) };
    read_u64(name).map_or(-1, u64_to_i64_saturating)
}

/// Return the current scrape text as an owned managed string.
///
/// # Ownership
///
/// The caller receives one owner and releases it with `hew_string_drop`.
#[no_mangle]
pub extern "C" fn hew_observe_scrape() -> *mut HewString {
    string_from_str(&scrape_text())
}

/// Return the canonical metric series names as newline-delimited text.
///
/// # Ownership
///
/// The returned handle follows the same ownership rules as
/// [`hew_observe_scrape`].
#[no_mangle]
pub extern "C" fn hew_observe_series() -> *mut HewString {
    string_from_str(&series_text())
}

#[cfg(not(target_arch = "wasm32"))]
mod native {
    #[cfg(feature = "profiler")]
    use super::{current_shard, observe_hot_tier_enabled, observe_series, read_u64, PoisonSafe};
    use super::{
        dispatch_barrier_lock, AtomicU64, Ordering, ACTORS_CRASHES_TOTAL, ACTORS_RESTARTS_TOTAL,
        ACTOR_TURNS_TOTAL, ACTOR_TURN_DURATION_NS_TOTAL, ARENA_RESETS_TOTAL, ATTRIBUTED_TURNS,
        COROUTINES_FRAME_BYTES_LIVE, COROUTINES_LIVE, COROUTINES_RESUMES_TOTAL,
        COROUTINES_SUSPENDED, COROUTINES_SUSPENDS_TOTAL, CRASHES_BY_KIND, DISPATCH_ACTIVE_TICKETS,
        DISPATCH_BARRIER, DISPATCH_BARRIER_WAITERS, HEAP_COUNTERS, HOT_TIER_ENABLED,
        IO_HANDLES_LIVE, NEXT_DISPATCH_TICKET, OBSERVE_BARRIER_OK, POOL_SATURATIONS_TOTAL,
        POOL_THREADS, PUBLISHED_DISPATCH_TICKET, REACTOR_READY_EVENTS_TOTAL,
        REACTOR_REGISTRATIONS_LIVE, SCHEDULER_PARKS_TOTAL, SCHEDULER_UNPARKS_TOTAL, SHARD_COUNT,
        SUPERVISOR_GIVEUPS_TOTAL, THREADS_BLOCKING_COUNT, TURN_ACTOR, TURN_DISPATCH, TURN_MSG_TYPE,
        WORKER_SHARD,
    };
    use std::time::{Duration, Instant};

    use crate::util::CondvarExt;

    pub(super) const OBSERVE_BARRIER_ERR_WORKER_CONTEXT: i64 = -1;
    const OBSERVE_BARRIER_ERR_TIMEOUT: i64 = -2;
    const OBSERVE_BARRIER_TIMEOUT: Duration = Duration::from_secs(30);

    static SHARDS_IN_USE: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(1);
    pub(super) static DISPATCH_COMPLETION_WATERMARK: AtomicU64 = AtomicU64::new(0);

    struct DispatchBarrierWaiter;

    impl Drop for DispatchBarrierWaiter {
        fn drop(&mut self) {
            DISPATCH_BARRIER_WAITERS.fetch_sub(1, Ordering::AcqRel);
        }
    }

    #[cfg(feature = "profiler")]
    mod profiler_support {
        use super::{
            current_shard, observe_hot_tier_enabled, observe_series, read_u64, Ordering,
            PoisonSafe, HEAP_COUNTERS, SHARDS_IN_USE,
        };
        use std::collections::BTreeMap;

        static RC_LIVE_BY_LABEL: PoisonSafe<BTreeMap<usize, u64>> =
            PoisonSafe::new(BTreeMap::new());

        pub(crate) fn record_rc_allocation(label: *const std::ffi::c_char) {
            RC_LIVE_BY_LABEL.access(|counts| *counts.entry(label as usize).or_default() += 1);
        }

        pub(crate) fn record_rc_free(label: *const std::ffi::c_char) {
            RC_LIVE_BY_LABEL.access(|counts| {
                let count = counts
                    .get_mut(&(label as usize))
                    .expect("allocated Rc label is registered");
                *count -= 1;
                if *count == 0 {
                    counts.remove(&(label as usize));
                }
            });
        }

        /// Live Rc blocks by payload type name, merging labels that render alike.
        pub(crate) fn rc_live_by_type() -> Vec<hew_observe_protocol::RcSurvivorType> {
            let mut by_name = BTreeMap::<String, u64>::new();
            RC_LIVE_BY_LABEL.access(|counts| {
                for (&label, &count) in counts.iter() {
                    // SAFETY: a non-zero row's headers retain this NUL-terminated label,
                    // and the lock orders this read before the final free's removal.
                    let name =
                        unsafe { std::ffi::CStr::from_ptr(label as *const std::ffi::c_char) };
                    *by_name
                        .entry(name.to_string_lossy().into_owned())
                        .or_default() += count;
                }
            });
            by_name
                .into_iter()
                .map(|(type_name, count)| hew_observe_protocol::RcSurvivorType { type_name, count })
                .collect()
        }

        /// One past the highest shard any thread has been pinned to; shards at or
        /// above it have never been written, so readers can stop summing there.
        pub(crate) fn shards_in_use() -> usize {
            SHARDS_IN_USE.load(Ordering::Relaxed)
        }

        pub(crate) fn record_heap_alloc(size: u64) {
            if observe_hot_tier_enabled() {
                let counters = &HEAP_COUNTERS[current_shard()];
                counters.allocations.fetch_add(1, Ordering::Relaxed);
                counters.allocated.fetch_add(size, Ordering::Relaxed);
            }
        }

        pub(crate) fn record_heap_free(size: u64) {
            if observe_hot_tier_enabled() {
                let counters = &HEAP_COUNTERS[current_shard()];
                counters.frees.fetch_add(1, Ordering::Relaxed);
                counters.freed.fetch_add(size, Ordering::Relaxed);
            }
        }

        pub(crate) fn runtime_series_snapshot() -> Vec<hew_observe_protocol::RuntimeSeries> {
            observe_series()
                .iter()
                .filter_map(|(name, kind)| {
                    read_u64(name).map(|value| hew_observe_protocol::RuntimeSeries {
                        name: (*name).to_owned(),
                        kind: (*kind).to_owned(),
                        value,
                    })
                })
                .collect()
        }
    }

    #[cfg(feature = "profiler")]
    pub(crate) use profiler_support::{
        rc_live_by_type, record_heap_alloc, record_heap_free, record_rc_allocation, record_rc_free,
        runtime_series_snapshot, shards_in_use,
    };

    /// Configure the hot observe tier from `HEW_OBSERVE`.
    ///
    /// Accepted true values are `1`, `true`, `yes`, `on`, and `hot`. Empty/unset
    /// leaves the tier disabled. This is called during scheduler initialization.
    pub(crate) fn configure_from_env() {
        let enabled = match std::env::var("HEW_OBSERVE") {
            Ok(value) => matches!(value.as_str(), "1" | "true" | "yes" | "on" | "hot"),
            Err(_) => std::env::var_os("HEW_PPROF").is_some(),
        };
        HOT_TIER_ENABLED.store(enabled, Ordering::Release);
        crate::fault::configure_from_env();
    }

    /// Pin the calling thread to its own counter shard.
    pub(crate) fn set_current_worker_shard(worker_id: usize) {
        let shard = worker_id.saturating_add(1).min(SHARD_COUNT - 1);
        SHARDS_IN_USE.fetch_max(shard + 1, Ordering::Relaxed);
        WORKER_SHARD.with(|slot| slot.set(shard));
    }

    fn shard_reset(shards: &[AtomicU64; SHARD_COUNT]) {
        for counter in shards {
            counter.store(0, Ordering::Relaxed);
        }
    }

    pub(crate) fn record_scheduler_park() {
        SCHEDULER_PARKS_TOTAL.fetch_add(1, Ordering::Relaxed);
    }

    pub(crate) fn record_scheduler_unpark() {
        SCHEDULER_UNPARKS_TOTAL.fetch_add(1, Ordering::Relaxed);
    }

    pub(crate) fn record_reactor_registration() {
        REACTOR_REGISTRATIONS_LIVE.fetch_add(1, Ordering::Relaxed);
    }

    pub(crate) fn record_reactor_unregistration(count: u64) {
        let _ = REACTOR_REGISTRATIONS_LIVE.fetch_update(
            Ordering::Relaxed,
            Ordering::Relaxed,
            |value| Some(value.saturating_sub(count)),
        );
    }

    pub(crate) fn record_reactor_ready_event() {
        REACTOR_READY_EVENTS_TOTAL.fetch_add(1, Ordering::Relaxed);
    }

    pub(crate) fn record_io_handle_opened() {
        IO_HANDLES_LIVE.fetch_add(1, Ordering::Relaxed);
    }

    pub(crate) fn record_io_handle_closed() {
        let _ = IO_HANDLES_LIVE.fetch_update(Ordering::Relaxed, Ordering::Relaxed, |value| {
            value.checked_sub(1)
        });
    }

    pub(crate) fn record_pool_threads(threads: usize) {
        POOL_THREADS.store(threads as u64, Ordering::Relaxed);
    }

    pub(crate) fn record_pool_saturation() {
        POOL_SATURATIONS_TOTAL.fetch_add(1, Ordering::Relaxed);
    }

    /// One worker shard's turn in progress.
    #[derive(Debug, Clone, Copy)]
    pub(crate) struct BusyWorker {
        /// Worker index plus one; zero for threads off the pool.
        pub worker_id: usize,
        pub actor_id: u64,
        pub dispatch: usize,
        pub msg_type: i32,
    }

    /// What every worker shard is dispatching now.
    #[must_use]
    pub(crate) fn busy_workers() -> Vec<BusyWorker> {
        (0..SHARD_COUNT)
            .filter_map(|shard| {
                let actor_id = TURN_ACTOR[shard].load(Ordering::Relaxed);
                (actor_id != 0).then(|| BusyWorker {
                    worker_id: shard,
                    actor_id,
                    dispatch: usize::try_from(TURN_DISPATCH[shard].load(Ordering::Relaxed))
                        .unwrap_or(0),
                    msg_type: TURN_MSG_TYPE[shard].load(Ordering::Relaxed),
                })
            })
            .collect()
    }

    pub(crate) fn record_blocking_start() {
        THREADS_BLOCKING_COUNT.fetch_add(1, Ordering::Relaxed);
    }

    pub(crate) fn record_blocking_finish() {
        let _ =
            THREADS_BLOCKING_COUNT.fetch_update(Ordering::Relaxed, Ordering::Relaxed, |value| {
                value.checked_sub(1)
            });
    }

    fn computed_dispatch_watermark() -> u64 {
        let published_ticket = PUBLISHED_DISPATCH_TICKET.load(Ordering::Acquire);
        DISPATCH_ACTIVE_TICKETS
            .iter()
            .filter_map(|slot| {
                let ticket = slot.load(Ordering::SeqCst);
                (ticket != 0 && ticket <= published_ticket).then_some(ticket)
            })
            .min()
            .map_or(published_ticket, |ticket| ticket.saturating_sub(1))
    }

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

    /// Wait until in-flight attributed dispatches settle and return 0, or a
    /// negative status on worker context / timeout.
    pub(crate) fn native_observe_barrier() -> i64 {
        if crate::execution_context::current_context_is_actor_dispatch() {
            crate::set_last_error("observe.barrier: cannot wait from inside an actor dispatch");
            return OBSERVE_BARRIER_ERR_WORKER_CONTEXT;
        }

        let target_ticket = PUBLISHED_DISPATCH_TICKET.load(Ordering::Acquire);
        if DISPATCH_COMPLETION_WATERMARK.load(Ordering::Acquire) >= target_ticket {
            return OBSERVE_BARRIER_OK;
        }

        let deadline = Instant::now() + OBSERVE_BARRIER_TIMEOUT;
        let mut guard = dispatch_barrier_lock();
        DISPATCH_BARRIER_WAITERS.fetch_add(1, Ordering::SeqCst);
        let _waiter = DispatchBarrierWaiter;

        while publish_dispatch_watermark() < target_ticket {
            let now = Instant::now();
            if now >= deadline {
                crate::set_last_error("observe.barrier: timed out after 30 seconds");
                return OBSERVE_BARRIER_ERR_TIMEOUT;
            }
            let remaining = deadline.saturating_duration_since(now);
            let (next_guard, _) = DISPATCH_BARRIER
                .cond
                .wait_timeout_or_recover(guard, remaining);
            guard = next_guard;
        }

        OBSERVE_BARRIER_OK
    }

    /// Reset metrics that are safe to clear while dispatchers are live.
    pub(crate) fn reset_live_safe_counters() {
        for counters in &HEAP_COUNTERS {
            counters.allocated.store(0, Ordering::Relaxed);
            counters.freed.store(0, Ordering::Relaxed);
            counters.allocations.store(0, Ordering::Relaxed);
            counters.frees.store(0, Ordering::Relaxed);
        }
        shard_reset(&ACTOR_TURNS_TOTAL);
        shard_reset(&ACTOR_TURN_DURATION_NS_TOTAL);
        SCHEDULER_PARKS_TOTAL.store(0, Ordering::Relaxed);
        SCHEDULER_UNPARKS_TOTAL.store(0, Ordering::Relaxed);
        COROUTINES_RESUMES_TOTAL.store(0, Ordering::Relaxed);
        COROUTINES_SUSPENDS_TOTAL.store(0, Ordering::Relaxed);
        REACTOR_READY_EVENTS_TOTAL.store(0, Ordering::Relaxed);
        ACTORS_CRASHES_TOTAL.store(0, Ordering::Relaxed);
        SUPERVISOR_GIVEUPS_TOTAL.store(0, Ordering::Relaxed);
        CRASHES_BY_KIND.access(Vec::clear);
        ACTORS_RESTARTS_TOTAL.store(0, Ordering::Relaxed);
        ARENA_RESETS_TOTAL.store(0, Ordering::Relaxed);
        ATTRIBUTED_TURNS.access(|slot| {
            if let Some(map) = slot.as_mut() {
                map.clear();
            }
        });
    }

    /// Reset observe/session counters after the runtime has quiesced.
    pub(crate) fn reset_all() {
        reset_live_safe_counters();
        COROUTINES_LIVE.store(0, Ordering::Relaxed);
        COROUTINES_SUSPENDED.store(0, Ordering::Relaxed);
        COROUTINES_FRAME_BYTES_LIVE.store(0, Ordering::Relaxed);
        REACTOR_REGISTRATIONS_LIVE.store(0, Ordering::Relaxed);
        THREADS_BLOCKING_COUNT.store(0, Ordering::Relaxed);

        crate::metrics::session_reset_metrics();
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

    pub(crate) fn register_reset_hooks() {
        use std::sync::Once;
        static ONCE: Once = Once::new();
        ONCE.call_once(|| crate::session::register_reset_hook(reset_all));
    }
}

#[cfg(not(target_arch = "wasm32"))]
pub(crate) use native::{
    busy_workers, configure_from_env, native_observe_barrier, record_blocking_finish,
    record_blocking_start, record_io_handle_closed, record_io_handle_opened,
    record_pool_saturation, record_pool_threads, record_reactor_ready_event,
    record_reactor_registration, record_reactor_unregistration, record_scheduler_park,
    record_scheduler_unpark, register_reset_hooks, reset_live_safe_counters,
    set_current_worker_shard,
};
#[cfg(all(feature = "profiler", not(target_arch = "wasm32")))]
pub(crate) use native::{
    rc_live_by_type, record_heap_alloc, record_heap_free, record_rc_allocation, record_rc_free,
    runtime_series_snapshot, shards_in_use,
};
#[cfg(all(test, not(target_arch = "wasm32")))]
use native::{reset_all, DISPATCH_COMPLETION_WATERMARK, OBSERVE_BARRIER_ERR_WORKER_CONTEXT};

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_string::ManagedString;
    use hew_cabi::string::string_release;

    #[test]
    fn read_known_metric_and_unknown_sentinel() {
        let known = ManagedString::new("heap.live_bytes");
        let unknown = ManagedString::new("does.not.exist");
        // SAFETY: both fixtures own live managed names.
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
    fn reserved_rendered_names_cover_every_scrape_owned_family() {
        let reserved: std::collections::HashSet<String> = reserved_rendered_names().collect();
        // A representative built-in (rendered) is reserved.
        assert!(
            reserved.contains("heap_live_bytes"),
            "observe_series built-ins must be reserved"
        );
        // Both per-handler attributed-turn series are reserved.
        for name in ATTRIBUTED_TURN_SERIES_NAMES {
            assert!(
                reserved.contains(name),
                "attributed-turn series `{name}` must be reserved"
            );
        }
        // All four registry self-metrics are reserved.
        for name in SELF_METRIC_NAMES {
            assert!(
                reserved.contains(name),
                "self-metric `{name}` must be reserved"
            );
        }
        // The collision check agrees: each reserved name is rejected, including
        // the dotted canonical form of a built-in that renders onto a reserved
        // series.
        assert!(rendered_name_collides_with_builtin("heap.live_bytes"));
        assert!(rendered_name_collides_with_builtin(
            "actors_attributed_turns_by_handler_total"
        ));
        assert!(rendered_name_collides_with_builtin(
            "hew_metrics_collision_rejected_total"
        ));
        // A genuinely user-owned name is not reserved.
        assert!(!rendered_name_collides_with_builtin("app.requests"));
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
    fn observe_read_u64_returns_exact_value_for_user_counter() {
        let _guard = crate::runtime_test_guard();
        reset_all();

        // A user-registered counter is unknown to observe's built-in match arms,
        // so `observe::read_u64` must fall through to `crate::metrics::read_u64`
        // and return the exact counter value directly — not via `scrape_text`,
        // not a sentinel, not None.
        let handle = crate::metrics::register_counter("app.observe_reads");
        assert!(handle >= 0);
        for _ in 0..7 {
            crate::metrics::inc(handle);
        }

        assert_eq!(
            read_u64("app.observe_reads"),
            Some(7),
            "observe::read_u64 must report the exact user-counter value via the fallthrough",
        );

        // An absent user name still propagates None through the same fallthrough.
        assert_eq!(read_u64("app.never.registered"), None);
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
        crate::metrics::histogram_record(durations, 5.0);
        crate::metrics::histogram_record(durations, 42.0);
        crate::metrics::histogram_record(durations, 500.0);

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
    fn observe_scrape_transfers_one_managed_owner() {
        let value = hew_observe_scrape();
        assert!(!value.is_null());
        // SAFETY: `value` is the live owner returned by the producer.
        unsafe {
            assert!(hew_cabi::string::string_as_str(value).contains("heap_live_bytes"));
            string_release(value);
        }
    }

    #[cfg(not(target_arch = "wasm32"))]
    mod native_tests {
        use super::*;
        use std::time::{Duration, Instant};

        #[test]
        fn live_metrics_reset_keeps_an_active_dispatch_ticket_closable() {
            let _runtime = crate::runtime_test_guard();
            reset_all();
            set_current_worker_shard(0);

            let ticket = observe_dispatch_begin(0, 0, 0);
            std::thread::spawn(|| crate::scheduler::hew_sched_metrics_reset())
                .join()
                .expect("live metrics reset thread must not panic");

            observe_dispatch_attributed(ticket);
            assert_eq!(hew_observe_barrier(), OBSERVE_BARRIER_OK);
        }

        #[test]
        fn observe_barrier_returns_immediately_when_already_attributed() {
            let _guard = crate::runtime_test_guard();
            reset_all();
            set_current_worker_shard(0);
            let ticket = observe_dispatch_begin(0, 0, 0);
            observe_dispatch_attributed(ticket);

            let started = Instant::now();
            assert_eq!(hew_observe_barrier(), OBSERVE_BARRIER_OK);
            assert!(
                started.elapsed() < Duration::from_millis(100),
                "already-attributed barrier should not wait for a future dispatch"
            );
        }

        #[test]
        fn observe_barrier_wakes_when_concurrent_dispatch_is_attributed() {
            let _guard = crate::runtime_test_guard();
            reset_all();
            set_current_worker_shard(0);
            let ticket = observe_dispatch_begin(0, 0, 0);

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

        #[test]
        fn observe_barrier_holds_for_earlier_ticket_when_later_ticket_closes_first() {
            let _guard = crate::runtime_test_guard();
            reset_all();
            set_current_worker_shard(0);
            let ticket_a = observe_dispatch_begin(0, 0, 0);
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
            let ticket_b = observe_dispatch_begin(0, 0, 0);
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

        #[test]
        fn observe_dispatch_close_uses_no_mutex_without_waiters() {
            let _guard = crate::runtime_test_guard();
            reset_all();
            DISPATCH_BARRIER_MUTEX_ACQUISITIONS.store(0, Ordering::Relaxed);
            set_current_worker_shard(0);

            for _ in 0..1024 {
                let ticket = observe_dispatch_begin(0, 0, 0);
                observe_dispatch_attributed(ticket);
            }

            assert_eq!(
                DISPATCH_BARRIER_MUTEX_ACQUISITIONS.load(Ordering::Relaxed),
                0,
                "dispatch begin/close should stay on the atomic fast path when no barrier is waiting"
            );
        }

        fn wait_for_barrier_waiters(expected: u64) {
            let deadline = Instant::now() + Duration::from_secs(5);
            while DISPATCH_BARRIER_WAITERS.load(Ordering::Acquire) != expected {
                assert!(
                    Instant::now() < deadline,
                    "barrier did not register its cut"
                );
                std::thread::yield_now();
            }
        }

        #[test]
        fn observe_barriers_keep_distinct_cuts_when_future_dispatch_remains_live() {
            let _guard = crate::runtime_test_guard();
            reset_all();
            set_current_worker_shard(0);
            let first = observe_dispatch_begin(0, 0, 0);
            let (first_tx, first_rx) = std::sync::mpsc::channel();
            let first_waiter = std::thread::spawn(move || {
                first_tx.send(hew_observe_barrier()).unwrap();
            });
            wait_for_barrier_waiters(1);

            set_current_worker_shard(1);
            let second = observe_dispatch_begin(0, 0, 0);
            let (second_tx, second_rx) = std::sync::mpsc::channel();
            let second_waiter = std::thread::spawn(move || {
                second_tx.send(hew_observe_barrier()).unwrap();
            });
            wait_for_barrier_waiters(2);

            set_current_worker_shard(0);
            observe_dispatch_attributed(first);
            assert_eq!(
                first_rx.recv_timeout(Duration::from_secs(5)).unwrap(),
                OBSERVE_BARRIER_OK
            );
            assert!(
                second_rx.try_recv().is_err(),
                "a later cut still owns its live dispatch"
            );
            assert_eq!(
                DISPATCH_COMPLETION_WATERMARK.load(Ordering::Acquire),
                first.0
            );

            set_current_worker_shard(1);
            observe_dispatch_abandon(second);
            assert_eq!(
                second_rx.recv_timeout(Duration::from_secs(5)).unwrap(),
                OBSERVE_BARRIER_OK
            );
            first_waiter.join().unwrap();
            second_waiter.join().unwrap();
            assert_eq!(hew_observe_barrier(), OBSERVE_BARRIER_OK);
            reset_all();
            assert_eq!(hew_observe_barrier(), OBSERVE_BARRIER_OK);
        }

        #[test]
        fn observe_barrier_rechecks_a_close_before_waiter_registration() {
            let _guard = crate::runtime_test_guard();
            reset_all();
            set_current_worker_shard(0);
            let ticket = observe_dispatch_begin(0, 0, 0);
            let guard = dispatch_barrier_lock();
            let (done_tx, done_rx) = std::sync::mpsc::channel();
            let waiter = std::thread::spawn(move || {
                done_tx.send(hew_observe_barrier()).unwrap();
            });
            observe_dispatch_attributed(ticket);
            drop(guard);
            assert_eq!(
                done_rx.recv_timeout(Duration::from_secs(5)).unwrap(),
                OBSERVE_BARRIER_OK
            );
            waiter.join().unwrap();
        }

        #[test]
        fn observe_barrier_excludes_a_dispatch_until_begin_is_published() {
            let _guard = crate::runtime_test_guard();
            reset_all();
            set_current_worker_shard(0);
            let ticket = NEXT_DISPATCH_TICKET.fetch_add(1, Ordering::Relaxed) + 1;
            DISPATCH_ACTIVE_TICKETS[current_shard()].store(ticket, Ordering::Release);
            assert_eq!(hew_observe_barrier(), OBSERVE_BARRIER_OK);
            publish_dispatch_begin(ticket);
            let (done_tx, done_rx) = std::sync::mpsc::channel();
            let waiter = std::thread::spawn(move || {
                done_tx.send(hew_observe_barrier()).unwrap();
            });
            wait_for_barrier_waiters(1);
            assert!(done_rx.try_recv().is_err());
            observe_dispatch_abandon(ObserveDispatchTicket(ticket));
            assert_eq!(
                done_rx.recv_timeout(Duration::from_secs(5)).unwrap(),
                OBSERVE_BARRIER_OK
            );
            waiter.join().unwrap();
        }

        #[test]
        fn observe_barrier_fails_fast_from_worker_context() {
            let _guard = crate::runtime_test_guard();
            reset_all();
            let mut actor = std::mem::MaybeUninit::<crate::actor::HewActor>::uninit();
            let mut ctx = crate::execution_context::HewExecutionContext {
                actor: actor.as_mut_ptr(),
                ..crate::execution_context::HewExecutionContext::default()
            };
            let prev = crate::execution_context::set_current_context(&raw mut ctx);

            assert_eq!(hew_observe_barrier(), OBSERVE_BARRIER_ERR_WORKER_CONTEXT);

            let restored = crate::execution_context::set_current_context(prev);
            assert_eq!(restored, &raw mut ctx);
        }

        #[test]
        fn observe_barrier_waits_from_a_root_task_context() {
            let _guard = crate::runtime_test_guard();
            reset_all();
            let mut ctx = crate::execution_context::HewExecutionContext::default();
            let prev = crate::execution_context::set_current_context(&raw mut ctx);

            assert_eq!(hew_observe_barrier(), OBSERVE_BARRIER_OK);

            let restored = crate::execution_context::set_current_context(prev);
            assert_eq!(restored, &raw mut ctx);
        }

        #[cfg(feature = "profiler")]
        #[test]
        fn heap_counters_preserve_tier_and_reset_boundaries() {
            let _guard = crate::runtime_test_guard();
            reset_all();
            set_hot_tier_enabled(false);
            record_heap_alloc(128);
            set_hot_tier_enabled(true);
            set_current_worker_shard(0);
            record_heap_alloc(64);
            set_current_worker_shard(1);
            record_heap_alloc(32);
            record_heap_free(64);
            set_hot_tier_enabled(false);
            record_heap_free(32);

            let hot = hot_counters_snapshot();
            assert_eq!(hot.heap_allocated_total, 96);
            assert_eq!(hot.heap_freed_total, 64);
            assert_eq!(hot.heap_allocations_total, 2);
            assert_eq!(hot.heap_frees_total, 1);
            reset_live_safe_counters();
            let reset = hot_counters_snapshot();
            assert_eq!(reset.heap_allocated_total, 0);
            assert_eq!(reset.heap_freed_total, 0);
            assert_eq!(reset.heap_allocations_total, 0);
            assert_eq!(reset.heap_frees_total, 0);
            set_current_worker_shard(0);
        }
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
