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

use crate::actor::HEW_MAX_WORKERS;
use crate::cabi::{free_cstring, str_to_malloc};
use crate::lifetime::PoisonSafe;

const SHARD_COUNT: usize = HEW_MAX_WORKERS + 1;

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

#[must_use]
pub fn scrape_text() -> String {
    let mut out = String::new();
    for (name, kind) in observe_series() {
        if let Some(value) = read_u64(name) {
            let prometheus_name = name.replace('.', "_");
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
