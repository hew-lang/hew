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

use std::cell::Cell;
use std::collections::VecDeque;
use std::ffi::c_int;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::Mutex;

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
    epoch.elapsed().as_nanos() as u64
}

// ── Recording ──────────────────────────────────────────────────────────

/// Record a trace event if tracing is enabled.
fn record_event(event: HewTraceEvent) {
    if !TRACING_ENABLED.load(Ordering::Relaxed) {
        return;
    }
    let mut events = match TRACE_EVENTS.lock() {
        Ok(g) => g,
        Err(e) => e.into_inner(),
    };
    while events.len() >= MAX_TRACE_EVENTS {
        events.pop_front();
    }
    events.push_back(event);
}

// ── C ABI ──────────────────────────────────────────────────────────────

/// Generate a new 128-bit trace ID (returned as two u64 values).
///
/// # Safety
///
/// `hi` and `lo` must be valid, non-null pointers.
#[no_mangle]
pub unsafe extern "C" fn hew_trace_new_id(hi: *mut u64, lo: *mut u64) {
    if hi.is_null() || lo.is_null() {
        return;
    }
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
    let ctx = CURRENT_CONTEXT.with(Cell::get);

    record_event(HewTraceEvent {
        trace_id_hi: ctx.trace_id_hi,
        trace_id_lo: ctx.trace_id_lo,
        span_id: ctx.span_id,
        parent_span_id: ctx.parent_span_id,
        actor_id,
        event_type,
        msg_type: 0,
        timestamp_ns: monotonic_ns(),
    });
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
    if ctx.is_null() {
        return;
    }
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
    if out.is_null() {
        return;
    }
    let ctx = CURRENT_CONTEXT.with(Cell::get);
    // SAFETY: caller guarantees `out` is valid.
    unsafe { *out = ctx };
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
///
/// # Panics
///
/// Panics if the internal event buffer mutex is poisoned.
#[no_mangle]
pub extern "C" fn hew_trace_event_count() -> u64 {
    let events = TRACE_EVENTS.lock().unwrap();
    events.len() as u64
}

/// Drain up to `max_count` trace events into the provided buffer.
///
/// Returns the number of events written.
///
/// # Safety
///
/// `out` must point to an array of at least `max_count` [`HewTraceEvent`]s.
///
/// # Panics
///
/// Panics if the internal event buffer mutex is poisoned.
#[no_mangle]
pub unsafe extern "C" fn hew_trace_drain(out: *mut HewTraceEvent, max_count: u32) -> u32 {
    if out.is_null() || max_count == 0 {
        return 0;
    }
    let mut events = TRACE_EVENTS.lock().unwrap();
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
///
/// # Panics
///
/// Panics if the internal event buffer mutex is poisoned.
#[no_mangle]
pub extern "C" fn hew_trace_clear() {
    let mut events = TRACE_EVENTS.lock().unwrap();
    events.clear();
}

/// Reset all tracing state (disable + clear events + reset context).
///
/// # Panics
///
/// Panics if the internal event buffer mutex is poisoned.
#[no_mangle]
pub extern "C" fn hew_trace_reset() {
    TRACING_ENABLED.store(false, Ordering::Release);
    let mut events = TRACE_EVENTS.lock().unwrap();
    events.clear();
    CURRENT_CONTEXT.with(|c| c.set(HewTraceContext::default()));
}

// ── Profiler snapshot ───────────────────────────────────────────────────

/// Drain up to 256 trace events and return them as a JSON array.
///
/// Each element: `{"trace_id":"HEX32","span_id":N,"parent_span_id":N,"actor_id":N,"event_type":"S","msg_type":N,"timestamp_ns":N}`
#[cfg(feature = "profiler")]
pub fn drain_events_json() -> String {
    use std::fmt::Write as _;

    let mut events = match TRACE_EVENTS.lock() {
        Ok(g) => g,
        Err(e) => e.into_inner(),
    };

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
            let _ = write!(
                json,
                r#"{{"trace_id":"{:016x}{:016x}","span_id":{},"parent_span_id":{},"actor_id":{},"event_type":"{}","msg_type":{},"timestamp_ns":{}}}"#,
                ev.trace_id_hi,
                ev.trace_id_lo,
                ev.span_id,
                ev.parent_span_id,
                ev.actor_id,
                event_type_str,
                ev.msg_type,
                ev.timestamp_ns,
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
    /// (TRACE_EVENTS, TRACING_ENABLED, CURRENT_CONTEXT).
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
}
