//! Distributed tracing for the Hew actor runtime.
//!
//! Provides causal message tracing with trace context propagation.
//! Compatible with the [W3C Trace Context][w3c] standard for
//! interoperability with OpenTelemetry and other tracing systems.
//!
//! [w3c]: https://www.w3.org/TR/trace-context/
//!
//! # Architecture
//!
//! Each actor dispatch carries a trace context (`trace_id`, `span_id`,
//! `parent_span_id`). When an actor sends a message, the current span
//! becomes the parent of the new span at the receiving actor.
//!
//! Trace events are collected in a lock-free ring buffer and can be
//! exported by a background thread or on-demand.
//!
//! # C ABI
//!
//! - [`hew_trace_new_id`] — Generate a random 128-bit trace ID.
//! - [`hew_trace_new_span`] — Generate a random 64-bit span ID.
//! - [`hew_trace_begin`] — Start a new span for an actor dispatch.
//! - [`hew_trace_end`] — End the current span.
//! - [`hew_trace_set_context`] — Set the current trace context.
//! - [`hew_trace_get_context`] — Get the current trace context.
//! - [`hew_trace_enable`] — Enable/disable tracing globally.
//! - [`hew_trace_event_count`] — Number of recorded events.
//! - [`hew_trace_drain`] — Drain recorded events into a buffer.

use crate::util::MutexExt;
use std::cell::Cell;
use std::collections::VecDeque;
use std::ffi::c_int;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::{Mutex, Once};

// ── Trace context ──────────────────────────────────────────────────────

/// Trace context propagated across actor messages.
///
/// Follows the W3C Trace Context model: a 128-bit trace ID identifies
/// the entire trace, and 64-bit span IDs identify individual operations.
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct HewTraceContext {
    /// Upper 64 bits of the 128-bit trace ID.
    pub trace_id_hi: u64,
    /// Lower 64 bits of the 128-bit trace ID.
    pub trace_id_lo: u64,
    /// Current span ID (unique within this trace).
    pub span_id: u64,
    /// Parent span ID (0 = root span).
    pub parent_span_id: u64,
    /// Trace flags (bit 0 = sampled).
    pub flags: u8,
}

/// Span event types.
pub const SPAN_BEGIN: i32 = 0;
/// Span end event.
pub const SPAN_END: i32 = 1;
/// Actor spawn event.
pub const SPAN_SPAWN: i32 = 2;
/// Actor crash event.
pub const SPAN_CRASH: i32 = 3;
/// Actor stop event.
pub const SPAN_STOP: i32 = 4;
/// Message send event.
pub const SPAN_SEND: i32 = 5;

/// A recorded trace event.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct HewTraceEvent {
    /// Trace ID (upper 64 bits).
    pub trace_id_hi: u64,
    /// Trace ID (lower 64 bits).
    pub trace_id_lo: u64,
    /// Span ID.
    pub span_id: u64,
    /// Parent span ID.
    pub parent_span_id: u64,
    /// Actor ID that generated this event.
    pub actor_id: u64,
    /// Event type (`SPAN_BEGIN`, `SPAN_END`, etc.).
    pub event_type: i32,
    /// Message type being processed (or 0 if N/A).
    pub msg_type: i32,
    /// Monotonic timestamp in nanoseconds.
    pub timestamp_ns: u64,
}

// ── Global state ───────────────────────────────────────────────────────

/// Global tracing enable flag.
static TRACING_ENABLED: AtomicBool = AtomicBool::new(false);

/// Monotonically increasing span ID counter (xorshift for speed).
static SPAN_COUNTER: AtomicU64 = AtomicU64::new(1);

/// Recorded trace events (bounded ring buffer).
static TRACE_EVENTS: Mutex<VecDeque<HewTraceEvent>> = Mutex::new(VecDeque::new());

/// Maximum number of trace events to retain.
const MAX_TRACE_EVENTS: usize = 16384;

// ── Per-thread trace context ───────────────────────────────────────────

thread_local! {
    /// Current trace context for this thread (set during actor dispatch).
    static CURRENT_CONTEXT: Cell<HewTraceContext> = const { Cell::new(HewTraceContext {
        trace_id_hi: 0,
        trace_id_lo: 0,
        span_id: 0,
        parent_span_id: 0,
        flags: 0,
    }) };
}

// ── ID generation ──────────────────────────────────────────────────────

/// Generate a pseudo-random 64-bit value for span/trace IDs.
///
/// Uses a simple xorshift on an atomic counter for speed. Not
/// cryptographically secure but sufficient for tracing.
fn next_random_id() -> u64 {
    let val = SPAN_COUNTER.fetch_add(1, Ordering::Relaxed);
    // Xorshift64 mix to avoid sequential patterns.
    let mut x = val;
    x ^= x << 13;
    x ^= x >> 7;
    x ^= x << 17;
    if x == 0 {
        1
    } else {
        x
    }
}

/// Get current monotonic time in nanoseconds (cross-platform).
fn monotonic_ns() -> u64 {
    use std::sync::OnceLock;
    use std::time::Instant;
    static EPOCH: OnceLock<Instant> = OnceLock::new();
    let epoch = EPOCH.get_or_init(Instant::now);
    #[expect(
        clippy::cast_possible_truncation,
        reason = "monotonic ns since process start won't exceed u64"
    )]
    {
        epoch.elapsed().as_nanos() as u64
    }
}

/// Public wrapper around `monotonic_ns()` for use by internal consumers.
///
/// Returns the current monotonic time in nanoseconds since the process's
/// tracing epoch. Use this to compute relative ages of trace events.
#[must_use]
pub fn trace_now_ns() -> u64 {
    monotonic_ns()
}

/// Compute the nanosecond offset to convert monotonic timestamps to Unix epoch.
///
/// Call this once when the `OTel` exporter starts. The returned value satisfies:
///
/// ```text
/// unix_epoch_ns = unix_epoch_offset_ns() + event.timestamp_ns
/// ```
///
/// The measurement takes two back-to-back reads (wall clock then monotonic) so
/// the error is at most a few nanoseconds — negligible for tracing.
#[must_use]
pub fn unix_epoch_offset_ns() -> u64 {
    use std::time::{SystemTime, UNIX_EPOCH};

    // Ensure the monotonic EPOCH is initialised before we sample system time,
    // so the first `monotonic_ns()` call doesn't skew the offset.
    let _ = monotonic_ns();

    let sys_ns = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos();
    let mono_ns = u128::from(monotonic_ns());

    // offset = sys - mono  (saturating: protects against clock weirdness)
    #[expect(
        clippy::cast_possible_truncation,
        reason = "Unix epoch ns since 1970 fits comfortably in u64 until the year 2554"
    )]
    {
        sys_ns.saturating_sub(mono_ns) as u64
    }
}

// ── Rust-internal drain ────────────────────────────────────────────────

/// Drain up to `max` trace events for internal consumers (e.g. the `OTel` exporter).
///
/// Unlike the C ABI [`hew_trace_drain`], this returns a `Vec` directly and
/// is callable from Rust without unsafe code.
pub fn drain_events(max: usize) -> Vec<HewTraceEvent> {
    if max == 0 {
        return Vec::new();
    }
    let mut events = TRACE_EVENTS.lock_or_recover();
    let count = events.len().min(max);
    let mut out = Vec::with_capacity(count);
    for _ in 0..count {
        if let Some(ev) = events.pop_front() {
            out.push(ev);
        }
    }
    out
}

// ── Recording ──────────────────────────────────────────────────────────

/// Record a trace event if tracing is enabled.
fn record_event(event: HewTraceEvent) {
    if !TRACING_ENABLED.load(Ordering::Relaxed) {
        return;
    }
    let mut events = TRACE_EVENTS.lock_or_recover();
    while events.len() >= MAX_TRACE_EVENTS {
        events.pop_front();
    }
    events.push_back(event);
}

fn record_lifecycle_event(actor_id: u64, event_type: i32, msg_type: i32) {
    let ctx = CURRENT_CONTEXT.with(Cell::get);

    record_event(HewTraceEvent {
        trace_id_hi: ctx.trace_id_hi,
        trace_id_lo: ctx.trace_id_lo,
        span_id: ctx.span_id,
        parent_span_id: ctx.parent_span_id,
        actor_id,
        event_type,
        msg_type,
        timestamp_ns: monotonic_ns(),
    });
}

// ── C ABI ──────────────────────────────────────────────────────────────

/// Generate a new 128-bit trace ID (returned as two u64 values).
///
/// # Safety
///
/// `hi` and `lo` must be valid, non-null pointers.
#[no_mangle]
pub unsafe extern "C" fn hew_trace_new_id(hi: *mut u64, lo: *mut u64) {
    cabi_guard!(hi.is_null() || lo.is_null());
    // SAFETY: caller guarantees hi and lo are valid.
    unsafe {
        *hi = next_random_id();
        *lo = next_random_id();
    }
}

/// Generate a new 64-bit span ID.
#[no_mangle]
pub extern "C" fn hew_trace_new_span() -> u64 {
    next_random_id()
}

/// Begin a new span for an actor dispatch.
///
/// Creates a child span under the current context and records a
/// `SPAN_BEGIN` event.
#[no_mangle]
pub extern "C" fn hew_trace_begin(actor_id: u64, msg_type: i32) {
    if !TRACING_ENABLED.load(Ordering::Relaxed) {
        return;
    }
    let parent = CURRENT_CONTEXT.with(Cell::get);
    let span_id = next_random_id();

    let ctx = HewTraceContext {
        trace_id_hi: if parent.trace_id_hi != 0 {
            parent.trace_id_hi
        } else {
            next_random_id()
        },
        trace_id_lo: if parent.trace_id_lo != 0 {
            parent.trace_id_lo
        } else {
            next_random_id()
        },
        span_id,
        parent_span_id: parent.span_id,
        flags: parent.flags,
    };

    CURRENT_CONTEXT.with(|c| c.set(ctx));

    record_event(HewTraceEvent {
        trace_id_hi: ctx.trace_id_hi,
        trace_id_lo: ctx.trace_id_lo,
        span_id: ctx.span_id,
        parent_span_id: ctx.parent_span_id,
        actor_id,
        event_type: SPAN_BEGIN,
        msg_type,
        timestamp_ns: monotonic_ns(),
    });
}

/// End the current span and record a `SPAN_END` event.
#[no_mangle]
pub extern "C" fn hew_trace_end(actor_id: u64, msg_type: i32) {
    if !TRACING_ENABLED.load(Ordering::Relaxed) {
        return;
    }
    let ctx = CURRENT_CONTEXT.with(Cell::get);

    record_event(HewTraceEvent {
        trace_id_hi: ctx.trace_id_hi,
        trace_id_lo: ctx.trace_id_lo,
        span_id: ctx.span_id,
        parent_span_id: ctx.parent_span_id,
        actor_id,
        event_type: SPAN_END,
        msg_type,
        timestamp_ns: monotonic_ns(),
    });

    // Restore parent context.
    CURRENT_CONTEXT.with(|c| {
        c.set(HewTraceContext {
            trace_id_hi: ctx.trace_id_hi,
            trace_id_lo: ctx.trace_id_lo,
            span_id: ctx.parent_span_id,
            parent_span_id: 0,
            flags: ctx.flags,
        });
    });
}

/// Record a lifecycle event (spawn, crash, stop).
#[no_mangle]
pub extern "C" fn hew_trace_lifecycle(actor_id: u64, event_type: i32) {
    if !TRACING_ENABLED.load(Ordering::Relaxed) {
        return;
    }
    record_lifecycle_event(actor_id, event_type, 0);
}

/// Set the current thread's trace context.
///
/// Used when an actor receives a message with a trace context
/// propagated from the sender.
///
/// # Safety
///
/// `ctx` must be a valid pointer to a [`HewTraceContext`].
#[no_mangle]
pub unsafe extern "C" fn hew_trace_set_context(ctx: *const HewTraceContext) {
    cabi_guard!(ctx.is_null());
    // SAFETY: caller guarantees `ctx` is valid.
    let new_ctx = unsafe { *ctx };
    CURRENT_CONTEXT.with(|c| c.set(new_ctx));
}

/// Get the current thread's trace context.
///
/// # Safety
///
/// `out` must be a valid pointer to a [`HewTraceContext`].
#[no_mangle]
pub unsafe extern "C" fn hew_trace_get_context(out: *mut HewTraceContext) {
    cabi_guard!(out.is_null());
    let ctx = CURRENT_CONTEXT.with(Cell::get);
    // SAFETY: caller guarantees `out` is valid.
    unsafe { *out = ctx };
}

pub(crate) fn current_context() -> HewTraceContext {
    CURRENT_CONTEXT.with(Cell::get)
}

pub(crate) fn set_context(ctx: HewTraceContext) {
    CURRENT_CONTEXT.with(|c| c.set(ctx));
}

pub(crate) fn record_send(actor_id: u64, msg_type: i32) {
    if !TRACING_ENABLED.load(Ordering::Relaxed) {
        return;
    }
    record_lifecycle_event(actor_id, SPAN_SEND, msg_type);
}

/// Enable or disable tracing globally.
///
/// When disabled, all tracing functions are near-zero-cost no-ops
/// (a single atomic load).
#[no_mangle]
pub extern "C" fn hew_trace_enable(enable: c_int) {
    TRACING_ENABLED.store(enable != 0, Ordering::Release);
}

/// Check if tracing is enabled. Returns 1 if enabled, 0 if not.
#[no_mangle]
pub extern "C" fn hew_trace_is_enabled() -> c_int {
    c_int::from(TRACING_ENABLED.load(Ordering::Acquire))
}

/// Return the number of recorded trace events.
#[no_mangle]
pub extern "C" fn hew_trace_event_count() -> u64 {
    let events = TRACE_EVENTS.lock_or_recover();
    events.len() as u64
}

/// Drain up to `max_count` trace events into the provided buffer.
///
/// Returns the number of events written.
///
/// # Safety
///
/// `out` must point to an array of at least `max_count` [`HewTraceEvent`]s.
#[no_mangle]
pub unsafe extern "C" fn hew_trace_drain(out: *mut HewTraceEvent, max_count: u32) -> u32 {
    cabi_guard!(out.is_null() || max_count == 0, 0);
    let mut events = TRACE_EVENTS.lock_or_recover();
    let count = events.len().min(max_count as usize);
    for i in 0..count {
        if let Some(event) = events.pop_front() {
            // SAFETY: out has space for at least max_count events.
            unsafe { *out.add(i) = event };
        }
    }
    #[expect(
        clippy::cast_possible_truncation,
        reason = "count <= max_count which is u32"
    )]
    {
        count as u32
    }
}

/// Clear all recorded trace events.
#[no_mangle]
pub extern "C" fn hew_trace_clear() {
    let mut events = TRACE_EVENTS.lock_or_recover();
    events.clear();
}

/// Register `hew_trace_reset` as a session reset hook.
///
/// Safe to call multiple times; the registration is guarded by a `Once` so
/// the hook is added to the registry exactly once per process lifetime.
/// Called from `scheduler::hew_sched_init` (native) and
/// `scheduler_wasm::hew_sched_init` (WASM) so trace events are cleared on
/// every `session_reset()` regardless of target.
pub(crate) fn register_trace_reset_hook() {
    // Wrapper converts the extern "C" fn to a plain Rust fn() as required by
    // the ResetHook type alias.
    fn trace_reset_hook() {
        hew_trace_reset();
    }
    static ONCE: Once = Once::new();
    ONCE.call_once(|| {
        crate::session::register_reset_hook(trace_reset_hook);
    });
}

/// Reset all tracing state (disable + clear events + reset context).
#[no_mangle]
pub extern "C" fn hew_trace_reset() {
    TRACING_ENABLED.store(false, Ordering::Release);
    let mut events = TRACE_EVENTS.lock_or_recover();
    events.clear();
    CURRENT_CONTEXT.with(|c| c.set(HewTraceContext::default()));
}

// ── Profiler snapshot ───────────────────────────────────────────────────

/// Drain up to 256 trace events and return them as a JSON array.
///
/// Each element:
/// ```json
/// {
///   "trace_id": "HEX32",
///   "span_id": N,
///   "parent_span_id": N,
///   "actor_id": N,
///   "actor_type_id": N,
///   "actor_type": "TypeName"|null,
///   "event_type": "begin"|"end"|"spawn"|"crash"|"stop"|"send",
///   "msg_type": N,
///   "timestamp_ns": N,
///   "handler_name": "ActorType::handler_name"|null
/// }
/// ```
///
/// `actor_type_id` is the dispatch function pointer cast to `u64`.  It is `0`
/// when the actor is no longer live at drain time (short-lived actors may have
/// been freed before the drain runs).
///
/// `actor_type` is the registered Hew type name (e.g. `"Counter"`).  It is
/// `null` when the dispatch pointer has not been registered via
/// `hew_actor_register_type` (which requires codegen emission — see #1258).
///
/// `handler_name` is non-null on native builds when `hew_register_handler_name`
/// has been called for the `(dispatch_fn, msg_type)` pair (requires codegen
/// emission — see #1259).  On WASM builds the bridge metadata registry is used.
#[cfg(feature = "profiler")]
pub fn drain_events_json() -> String {
    use std::fmt::Write as _;

    let mut events = TRACE_EVENTS.lock_or_recover();

    let count = events.len().min(256);
    let mut json = String::from("[");
    for i in 0..count {
        if let Some(ev) = events.pop_front() {
            if i > 0 {
                json.push(',');
            }
            let event_type_str = match ev.event_type {
                SPAN_BEGIN => "begin",
                SPAN_END => "end",
                SPAN_SPAWN => "spawn",
                SPAN_CRASH => "crash",
                SPAN_STOP => "stop",
                SPAN_SEND => "send",
                _ => "unknown",
            };

            // Resolve actor_type_id and actor_type via the profiler actor registry.
            // Option B from #1260: drain-time lookup avoids adding a dispatch-fn
            // pointer field to HewTraceEvent (which would break the C ABI).
            //
            // SHIM: WHY: HewTraceEvent is #[repr(C)]; adding fields would break
            //       hew_trace_drain callers.  Drain-time lookup is the zero-ABI-
            //       impact path.
            //       WHEN: Remove this lookup if a future ABI revision embeds
            //       actor_type_id directly in HewTraceEvent.
            //       REAL: Store dispatch_fn in HewTraceEvent when the C ABI is versioned.
            #[cfg(not(any(target_arch = "wasm32", test)))]
            let (actor_type_id, actor_type_str, handler_name) = {
                let dispatch_ptr =
                    crate::profiler::actor_registry::lookup_dispatch_for_actor_id(ev.actor_id)
                        .unwrap_or(0);
                let type_id = dispatch_ptr as u64;
                let type_name =
                    crate::profiler::actor_registry::lookup_dispatch_type_by_ptr(dispatch_ptr);
                let hname = crate::profiler::actor_registry::lookup_handler_name_by_ptr(
                    dispatch_ptr,
                    ev.msg_type,
                );
                (type_id, type_name, hname)
            };

            // WASM-TODO(#1451): WASM codegen registration not yet implemented; actor_type_id/actor_type are zeroed on WASM path
            #[cfg(any(target_arch = "wasm32", test))]
            let (actor_type_id, actor_type_str, handler_name): (
                u64,
                &'static str,
                Option<String>,
            ) = (0, "Actor", crate::bridge::resolve_handler_name(ev.msg_type));

            let actor_type_json = if actor_type_str == "Actor" && actor_type_id == 0 {
                "null".to_owned()
            } else {
                format!("\"{actor_type_str}\"")
            };
            let handler_name_json = match &handler_name {
                Some(name) => format!("\"{name}\""),
                None => "null".to_owned(),
            };
            let _ = write!(
                json,
                r#"{{"trace_id":"{:016x}{:016x}","span_id":{},"parent_span_id":{},"actor_id":{},"actor_type_id":{},"actor_type":{},"event_type":"{}","msg_type":{},"timestamp_ns":{},"handler_name":{}}}"#,
                ev.trace_id_hi,
                ev.trace_id_lo,
                ev.span_id,
                ev.parent_span_id,
                ev.actor_id,
                actor_type_id,
                actor_type_json,
                event_type_str,
                ev.msg_type,
                ev.timestamp_ns,
                handler_name_json,
            );
        }
    }
    json.push(']');
    json
}

// ── Tests ──────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    /// Serialize tracing tests since they share global state
    /// (`TRACE_EVENTS`, `TRACING_ENABLED`, `CURRENT_CONTEXT`).
    static TEST_LOCK: Mutex<()> = Mutex::new(());

    fn setup() -> std::sync::MutexGuard<'static, ()> {
        let guard = TEST_LOCK.lock().unwrap();
        hew_trace_reset();
        hew_trace_enable(1);
        guard
    }

    #[test]
    fn enable_disable() {
        let _guard = TEST_LOCK.lock().unwrap();
        hew_trace_reset();
        assert_eq!(hew_trace_is_enabled(), 0);
        hew_trace_enable(1);
        assert_eq!(hew_trace_is_enabled(), 1);
        hew_trace_enable(0);
        assert_eq!(hew_trace_is_enabled(), 0);
    }

    #[test]
    fn span_ids_unique() {
        let a = hew_trace_new_span();
        let b = hew_trace_new_span();
        let c = hew_trace_new_span();
        assert_ne!(a, b);
        assert_ne!(b, c);
        assert_ne!(a, c);
    }

    #[test]
    fn trace_ids_unique() {
        let mut hi1: u64 = 0;
        let mut lo1: u64 = 0;
        let mut hi2: u64 = 0;
        let mut lo2: u64 = 0;
        // SAFETY: valid pointers.
        unsafe {
            hew_trace_new_id(&raw mut hi1, &raw mut lo1);
            hew_trace_new_id(&raw mut hi2, &raw mut lo2);
        }
        assert!(hi1 != 0 || lo1 != 0);
        assert!((hi1, lo1) != (hi2, lo2));
    }

    #[test]
    fn begin_end_records_events() {
        let _guard = setup();

        hew_trace_begin(100, 1);
        hew_trace_end(100, 1);

        assert_eq!(hew_trace_event_count(), 2);

        let mut events = [HewTraceEvent {
            trace_id_hi: 0,
            trace_id_lo: 0,
            span_id: 0,
            parent_span_id: 0,
            actor_id: 0,
            event_type: 0,
            msg_type: 0,
            timestamp_ns: 0,
        }; 2];
        // SAFETY: buffer has space for 2 events.
        let count = unsafe { hew_trace_drain(events.as_mut_ptr(), 2) };
        assert_eq!(count, 2);
        assert_eq!(events[0].event_type, SPAN_BEGIN);
        assert_eq!(events[0].actor_id, 100);
        assert_eq!(events[0].msg_type, 1);
        assert_eq!(events[1].event_type, SPAN_END);
        assert_eq!(events[1].actor_id, 100);

        // Same trace ID for both events.
        assert_eq!(events[0].trace_id_hi, events[1].trace_id_hi);
        assert_eq!(events[0].trace_id_lo, events[1].trace_id_lo);
        // Same span ID.
        assert_eq!(events[0].span_id, events[1].span_id);

        hew_trace_reset();
    }

    #[test]
    fn context_propagation() {
        let _guard = setup();

        // Start a parent span.
        hew_trace_begin(100, 1);

        let mut parent_ctx = HewTraceContext::default();
        // SAFETY: valid pointer.
        unsafe { hew_trace_get_context(&raw mut parent_ctx) };
        assert_ne!(parent_ctx.span_id, 0);

        // Start a child span (simulates receiving a forwarded message).
        hew_trace_begin(200, 2);

        let mut child_ctx = HewTraceContext::default();
        // SAFETY: valid pointer.
        unsafe { hew_trace_get_context(&raw mut child_ctx) };

        // Child has same trace ID but different span ID.
        assert_eq!(child_ctx.trace_id_hi, parent_ctx.trace_id_hi);
        assert_eq!(child_ctx.trace_id_lo, parent_ctx.trace_id_lo);
        assert_ne!(child_ctx.span_id, parent_ctx.span_id);
        // Child's parent is the parent span.
        assert_eq!(child_ctx.parent_span_id, parent_ctx.span_id);

        hew_trace_end(200, 2);
        hew_trace_end(100, 1);

        hew_trace_reset();
    }

    #[test]
    fn lifecycle_events() {
        let _guard = setup();

        hew_trace_lifecycle(100, SPAN_SPAWN);
        hew_trace_lifecycle(100, SPAN_CRASH);
        hew_trace_lifecycle(100, SPAN_STOP);

        assert_eq!(hew_trace_event_count(), 3);
        hew_trace_reset();
    }

    #[test]
    fn disabled_noop() {
        let _guard = TEST_LOCK.lock().unwrap();
        hew_trace_reset();
        // Tracing disabled — events should not be recorded.
        hew_trace_begin(100, 1);
        hew_trace_end(100, 1);
        hew_trace_lifecycle(100, SPAN_SPAWN);
        assert_eq!(hew_trace_event_count(), 0);
    }

    #[test]
    fn drain_partial() {
        let _guard = setup();
        for i in 0..10 {
            hew_trace_lifecycle(i, SPAN_SPAWN);
        }
        assert_eq!(hew_trace_event_count(), 10);

        let mut events = [HewTraceEvent {
            trace_id_hi: 0,
            trace_id_lo: 0,
            span_id: 0,
            parent_span_id: 0,
            actor_id: 0,
            event_type: 0,
            msg_type: 0,
            timestamp_ns: 0,
        }; 3];
        // SAFETY: buffer has space for 3 events.
        let count = unsafe { hew_trace_drain(events.as_mut_ptr(), 3) };
        assert_eq!(count, 3);
        assert_eq!(hew_trace_event_count(), 7); // 10 - 3 drained

        hew_trace_reset();
    }

    /// Verify that `drain_events_json` emits `"handler_name":"ActorType::handler"`
    /// for a registered `msg_type` and `"handler_name":null` for an unknown one.
    ///
    /// This test exercises the bridge registration path together with the tracing
    /// JSON emission path. It acquires the shared `BRIDGE_TEST_LOCK` so it
    /// serialises against all other bridge-global-touching tests.
    #[cfg(feature = "profiler")]
    #[test]
    fn drain_events_json_includes_handler_name() {
        use crate::bridge::{
            hew_wasm_register_actor_meta, reset_bridge_full, HewActorMeta, HewHandlerMeta,
            BRIDGE_TEST_LOCK,
        };
        let _runtime_guard = crate::runtime_test_guard();

        // Acquire both locks in a consistent order (bridge first, then tracing)
        // to avoid deadlocks with concurrent bridge tests.
        let _bridge_guard = BRIDGE_TEST_LOCK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let _trace_guard = TEST_LOCK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);

        // Full reset of both subsystems.
        reset_bridge_full();
        hew_trace_reset();
        hew_trace_enable(1);

        // Register a synthetic actor: "TestActor" with handler "on_ping" at msg_type 77.
        let actor_name = b"TestActor\0";
        let handler_name = b"on_ping\0";
        let handler = HewHandlerMeta {
            name: handler_name.as_ptr().cast(),
            msg_type: 77,
            params: std::ptr::null(),
            param_count: 0,
            return_type: std::ptr::null(),
            return_size: 0,
        };
        let actor_meta = HewActorMeta {
            name: actor_name.as_ptr().cast(),
            handlers: &raw const handler,
            handler_count: 1,
        };
        // SAFETY: all pointers are valid stack pointers with lifetimes that
        // outlast this call; hew_wasm_register_actor_meta copies the strings.
        unsafe { hew_wasm_register_actor_meta(&raw const actor_meta) };

        // Emit two trace events: one with the registered msg_type, one unknown.
        hew_trace_begin(42, 77); // known msg_type
        hew_trace_begin(42, 99); // unknown msg_type

        let json = crate::tracing::drain_events_json();

        // The known msg_type must carry "handler_name":"TestActor::on_ping".
        assert!(
            json.contains(r#""handler_name":"TestActor::on_ping""#),
            "expected handler_name for known msg_type in: {json}"
        );
        // The unknown msg_type must carry "handler_name":null.
        assert!(
            json.contains(r#""handler_name":null"#),
            "expected null handler_name for unknown msg_type in: {json}"
        );

        // Cleanup.
        reset_bridge_full();
        hew_trace_reset();
    }

    /// Regression test for #1260: `drain_events_json` must always emit
    /// `"actor_type_id"` and `"actor_type"` fields on every event, even when
    /// the actor registry has not been populated (they emit 0 / null in that
    /// case).  This verifies the JSON schema shape is present regardless of
    /// whether actor-type registration codegen emission has run.
    #[cfg(feature = "profiler")]
    #[test]
    fn drain_events_json_includes_actor_type_fields() {
        let _guard = TEST_LOCK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);

        hew_trace_reset();
        hew_trace_enable(1);

        // Emit a single trace event.
        hew_trace_begin(123, 0);

        let json = crate::tracing::drain_events_json();

        // Both new fields must be present in every event.
        assert!(
            json.contains(r#""actor_type_id":"#),
            "drain_events_json must always emit actor_type_id field; got: {json}"
        );
        assert!(
            json.contains(r#""actor_type":"#),
            "drain_events_json must always emit actor_type field; got: {json}"
        );

        hew_trace_reset();
    }

    // Hook wrapper used by session_reset_clears_trace_state_via_hook.
    // Defined at module level to satisfy the items-after-statements lint.
    fn trace_reset_hook_for_test() {
        hew_trace_reset();
    }

    /// Verifies that when a trace-reset hook is registered in `RESET_HOOKS`,
    /// calling `session_reset()` clears trace events and disables tracing.
    ///
    /// Acquires `SESSION_TEST_LOCK` first and the local `TEST_LOCK` second so
    /// that concurrent session tests cannot clear the hook list mid-test.  The
    /// `SESSION_TEST_LOCK`-first acquisition order must match any other test
    /// that holds both locks.
    #[test]
    fn session_reset_clears_trace_state_via_hook() {
        let _runtime_guard = crate::runtime_test_guard();
        // Acquire session lock first, tracing lock second (consistent order).
        let _session_guard = crate::session::reset_hooks_for_test();
        let _trace_guard = TEST_LOCK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);

        // Reset tracing state to a known baseline.
        hew_trace_reset();

        // Register the trace reset hook directly for this test (bypassing the
        // production Once guard so the test is self-contained even if the Once
        // already fired in an earlier test run).
        crate::session::register_reset_hook(trace_reset_hook_for_test);

        // Enable tracing and record an event.
        hew_trace_enable(1);
        hew_trace_lifecycle(42, SPAN_SPAWN);
        assert_eq!(hew_trace_event_count(), 1);
        assert_eq!(hew_trace_is_enabled(), 1);

        // Fire all session hooks — must invoke the registered trace_reset_hook.
        crate::session::session_reset();

        assert_eq!(
            hew_trace_event_count(),
            0,
            "trace events must be empty after session_reset"
        );
        assert_eq!(
            hew_trace_is_enabled(),
            0,
            "tracing must be disabled after session_reset"
        );
    }
}
