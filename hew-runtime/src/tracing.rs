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
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

use crate::lifetime::PoisonSafe;
use crate::util::MutexExt;
use std::collections::{HashMap, VecDeque};
use std::ffi::c_int;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::{LazyLock, Mutex, Once};

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
/// I/O accept event: a new TCP connection was accepted by the cluster transport.
pub const SPAN_IO_ACCEPT: i32 = 6;
/// I/O recv event: a decoded envelope was received and is being delivered to an actor.
pub const SPAN_IO_RECV: i32 = 7;
/// Duplex pair was created (`hew_duplex_pair`). `actor_id` holds the pointer address of handle A.
pub const SPAN_DUPLEX_CREATED: i32 = 8;
/// Duplex was split into a half-handle (`hew_duplex_send_half` / `hew_duplex_recv_half`).
/// `actor_id` holds the pointer address of the originating unified handle.
pub const SPAN_DUPLEX_HALF_SPLIT: i32 = 9;
/// Duplex unified handle was closed (`hew_duplex_close`). `actor_id` holds the pointer address.
pub const SPAN_DUPLEX_CLOSED: i32 = 10;
/// Sink handle was closed (`hew_sink_close`). `actor_id` holds the pointer address.
pub const SPAN_SINK_CLOSED: i32 = 11;
/// Stream handle was closed (`hew_stream_close`). `actor_id` holds the pointer address.
pub const SPAN_STREAM_CLOSED: i32 = 12;
/// Lambda-actor was spawned (`hew_lambda_actor_new`). `actor_id` holds the pointer address.
pub const SPAN_LAMBDA_SPAWNED: i32 = 13;
/// Lambda-actor strong handle was released (`hew_lambda_actor_release`).
/// `actor_id` holds the pointer address.
pub const SPAN_LAMBDA_RELEASED: i32 = 14;

// ── Supervisor lifecycle spans ─────────────────────────────────────────
//
// Emitted by `hew-runtime/src/supervisor.rs` at the core supervision
// decision points so that restart/escalate/circuit/backoff policy is
// self-describing on the export surface without any programmer-written
// observability. `actor_id` carries the supervisor's own actor id; the
// per-event `msg_type` carries a decision-specific discriminator
// (e.g. the restart strategy, the within-window restart count, or the
// scheduled backoff delay in ms).

/// Supervisor restarted a failed child (after the budget check passed).
/// `msg_type` carries the restart strategy (`STRATEGY_*`).
pub const SPAN_SUPERVISOR_RESTART: i32 = 15;
/// Supervisor escalated a failure to its parent supervisor.
pub const SPAN_SUPERVISOR_ESCALATE: i32 = 16;
/// Child circuit breaker transitioned `CLOSED`/`HALF_OPEN` → `OPEN` (restarts
/// suppressed for the cooldown window).
pub const SPAN_SUPERVISOR_CIRCUIT_OPEN: i32 = 17;
/// Child circuit breaker transitioned `OPEN` → `HALF_OPEN` (one probe restart
/// allowed).
pub const SPAN_SUPERVISOR_CIRCUIT_HALF_OPEN: i32 = 18;
/// Child circuit breaker transitioned `HALF_OPEN` → `CLOSED` (probe restart
/// succeeded; normal restarts resume).
pub const SPAN_SUPERVISOR_CIRCUIT_CLOSE: i32 = 19;
/// Supervisor hit max-restart-intensity (`recent >= max_restarts`) and is
/// stopping/escalating instead of restarting. `msg_type` carries the
/// within-window restart count.
pub const SPAN_SUPERVISOR_MAX_RESTARTS: i32 = 20;
/// Supervisor scheduled a delayed (backoff-window) restart. `msg_type`
/// carries the scheduled delay in milliseconds (saturated to `i32`).
pub const SPAN_SUPERVISOR_BACKOFF: i32 = 21;

/// Closed taxonomy of v0.5 concurrency trace event names emitted by this
/// runtime. The entries pair each `SPAN_*` constant with the canonical
/// `event_type` string that appears in the JSON drained by
/// [`drain_events_json`].
///
/// This array is the **producer-side source of truth** for the names the
/// `hew-observe` consumer must recognise. Adding a new `SPAN_*` constant
/// without extending this list will be caught by the
/// `event_type_name_mapping_is_total` unit test in this module.
///
/// Per R58 Q138 Option B the QUIC, cancellation, and lock event families
/// are intentionally absent from this list — the runtime does not yet emit
/// trace spans for them. The `hew-observe` taxonomy carries deferred
/// metadata rows for those names as non-actionable shims pending the
/// native-M3 producer-emission lane.
pub const EVENT_TYPE_NAMES: &[(i32, &str)] = &[
    (SPAN_BEGIN, "begin"),
    (SPAN_END, "end"),
    (SPAN_SPAWN, "spawn"),
    (SPAN_CRASH, "crash"),
    (SPAN_STOP, "stop"),
    (SPAN_SEND, "send"),
    (SPAN_IO_ACCEPT, "io_accept"),
    (SPAN_IO_RECV, "io_recv"),
    (SPAN_DUPLEX_CREATED, "duplex_created"),
    (SPAN_DUPLEX_HALF_SPLIT, "duplex_half_split"),
    (SPAN_DUPLEX_CLOSED, "duplex_closed"),
    (SPAN_SINK_CLOSED, "sink_closed"),
    (SPAN_STREAM_CLOSED, "stream_closed"),
    (SPAN_LAMBDA_SPAWNED, "lambda_spawned"),
    (SPAN_LAMBDA_RELEASED, "lambda_released"),
    (SPAN_SUPERVISOR_RESTART, "supervisor_restart"),
    (SPAN_SUPERVISOR_ESCALATE, "supervisor_escalate"),
    (SPAN_SUPERVISOR_CIRCUIT_OPEN, "supervisor_circuit_open"),
    (
        SPAN_SUPERVISOR_CIRCUIT_HALF_OPEN,
        "supervisor_circuit_half_open",
    ),
    (SPAN_SUPERVISOR_CIRCUIT_CLOSE, "supervisor_circuit_close"),
    (SPAN_SUPERVISOR_MAX_RESTARTS, "supervisor_max_restarts"),
    (SPAN_SUPERVISOR_BACKOFF, "supervisor_backoff"),
];

/// Resolve a span code to its canonical taxonomy name. Returns `None` for
/// any value outside [`EVENT_TYPE_NAMES`]; callers that need a fallback
/// (e.g. JSON serialisation) should map `None` to `"unknown"` explicitly so
/// invalid codes cannot silently masquerade as a known event type.
#[must_use]
pub fn event_type_name(event_type: i32) -> Option<&'static str> {
    EVENT_TYPE_NAMES
        .iter()
        .find(|(code, _)| *code == event_type)
        .map(|(_, name)| *name)
}

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
static TRACE_EVENTS: PoisonSafe<VecDeque<HewTraceEvent>> = PoisonSafe::new(VecDeque::new());

/// Maximum number of trace events to retain.
const MAX_TRACE_EVENTS: usize = 16384;

// ── Per-connection I/O span side table ────────────────────────────────
//
// Associates a `conn_id` (transport handle, an `i32`) with the
// `HewTraceContext` produced when the connection was accepted.  The reader
// thread looks this up to parent the `SPAN_IO_RECV` span under the accept
// span, propagating causal IDs into the actor mailbox.
//
// SHIM: WHY — `hew_connmgr_add` (accept site) and `reader_loop` (recv site)
//       run on different OS threads and share only `conn_id`.  A
//       `Mutex<HashMap>` is the minimal safe cross-thread store.
//       WHEN — Remove or upgrade if a sharded or lock-free structure is
//       warranted by profiler data showing this as a bottleneck.
//       REAL — A per-shard table or embedding the context in `ConnectionActor`
//       would be lower-contention, but requires ABI/struct changes.
static IO_SPAN_TABLE: LazyLock<Mutex<HashMap<c_int, HewTraceContext>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));

/// Stash the `HewTraceContext` of an accepted connection by `conn_id`.
///
/// Only called when `TRACING_ENABLED` is true.  No-op if tracing is later
/// disabled — the entry will be evicted on connection close.
// live on not(wasm32) — connection.rs; dead on wasm32; caller connection.rs:1953
#[cfg_attr(target_arch = "wasm32", allow(dead_code))]
pub(crate) fn io_span_stash(conn_id: c_int, ctx: HewTraceContext) {
    IO_SPAN_TABLE.lock_or_recover().insert(conn_id, ctx);
}

/// Evict any stashed context for `conn_id` on connection close.
///
/// Safe to call even when no entry exists (eviction is idempotent).
// live on not(wasm32) — connection.rs; dead on wasm32; caller connection.rs:1400
#[cfg_attr(target_arch = "wasm32", allow(dead_code))]
pub(crate) fn io_span_evict(conn_id: c_int) {
    IO_SPAN_TABLE.lock_or_recover().remove(&conn_id);
}

// live on not(wasm32) — io_recv_span_begin/end; dead on wasm32; callers tracing.rs:631, 666
#[cfg_attr(target_arch = "wasm32", allow(dead_code))]
const TRACE_OWNS_EXECUTION_CONTEXT: u32 = 1 << 31;

fn trace_context() -> Option<HewTraceContext> {
    let ctx = crate::execution_context::require_current_context();
    if ctx.is_null() {
        return None;
    }
    // SAFETY: a non-null canonical context points to a live context slot owned
    // by the current dispatch/scope boundary.
    Some(unsafe { (*ctx).trace })
}

fn write_trace_context(new_ctx: HewTraceContext) -> bool {
    let ctx = crate::execution_context::require_current_context();
    if ctx.is_null() {
        return false;
    }
    // SAFETY: a non-null canonical context points to a live context slot owned
    // by the current dispatch/scope boundary.
    unsafe {
        (*ctx).trace = new_ctx;
    }
    true
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

/// Get current monotonic time in nanoseconds, anchored on the process-wide
/// epoch ([`crate::monotonic`]).
fn monotonic_ns() -> u64 {
    crate::monotonic::monotonic_ns()
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
    TRACE_EVENTS.access(|events| {
        let count = events.len().min(max);
        let mut out = Vec::with_capacity(count);
        for _ in 0..count {
            if let Some(ev) = events.pop_front() {
                out.push(ev);
            }
        }
        out
    })
}

// ── Recording ──────────────────────────────────────────────────────────

/// Record a trace event if tracing is enabled.
fn record_event(event: HewTraceEvent) {
    if !TRACING_ENABLED.load(Ordering::Relaxed) {
        return;
    }
    TRACE_EVENTS.access(|events| {
        while events.len() >= MAX_TRACE_EVENTS {
            events.pop_front();
        }
        events.push_back(event);
    });
}

fn record_lifecycle_event(actor_id: u64, event_type: i32, msg_type: i32) {
    let Some(ctx) = trace_context() else {
        return;
    };

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

/// Record a supervisor lifecycle decision (restart, escalate, circuit
/// transition, max-restart-intensity, backoff) as a trace event on the same
/// ring buffer the export surface drains.
///
/// Modelled on [`record_lifecycle_event`]: it is a read-only side effect that
/// honours the `TRACING_ENABLED` fast-path guard (via [`record_event`]) and
/// emits under the supervisor's live execution context — supervisor decisions
/// run inside the supervisor's own actor dispatch, so a context is present on
/// the restart path. It early-returns (emitting nothing) when no context is
/// installed, so it can never gate or perturb supervisor control flow.
///
/// `discriminator` is carried in the event's `msg_type` slot and is
/// decision-specific (restart strategy, within-window restart count, or
/// scheduled backoff delay in ms).
pub(crate) fn record_supervisor_event(actor_id: u64, event_type: i32, discriminator: i32) {
    record_lifecycle_event(actor_id, event_type, discriminator);
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
    let Some(parent) = trace_context() else {
        return;
    };
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

    if !write_trace_context(ctx) {
        return;
    }

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
    let Some(ctx) = trace_context() else {
        return;
    };

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
    let _ = write_trace_context(HewTraceContext {
        trace_id_hi: ctx.trace_id_hi,
        trace_id_lo: ctx.trace_id_lo,
        span_id: ctx.parent_span_id,
        parent_span_id: 0,
        flags: ctx.flags,
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
    let _ = write_trace_context(new_ctx);
}

/// Get the current thread's trace context.
///
/// # Safety
///
/// `out` must be a valid pointer to a [`HewTraceContext`].
#[no_mangle]
pub unsafe extern "C" fn hew_trace_get_context(out: *mut HewTraceContext) {
    cabi_guard!(out.is_null());
    let ctx = trace_context().unwrap_or_default();
    // SAFETY: caller guarantees `out` is valid.
    unsafe { *out = ctx };
}

// live on not(wasm32) — mailbox.rs:msg_node_alloc external-send seam (active on native; wasm path is mailbox_wasm.rs)
#[cfg_attr(target_arch = "wasm32", allow(dead_code))]
pub(crate) fn current_context() -> HewTraceContext {
    if let Some(ctx) = trace_context() {
        return ctx;
    }

    // TRACE-CONTEXT COMPLETENESS (S3, external-send seam): no execution context
    // is installed, so this `msg_node_alloc` is an external send originating
    // OUTSIDE any actor dispatch (e.g. `hew_actor_send` from `main`). Returning
    // the all-zero default here would sever the causal chain: the receiving
    // dispatch would synthesise a fresh, UNSAMPLED trace from a zero parent,
    // and the message node would carry an orphan zero-id context.
    //
    // Instead mint a fresh SAMPLED root (same id source + flags convention as
    // `io_accept_span_begin`) so the message node carries a real trace root and
    // the receiver parents its dispatch span under a valid, sampled trace id.
    // Each external send is its own trace root, which is the correct causal
    // shape for an unsolicited boundary-crossing message.
    //
    // Fast path preserved: when tracing is disabled this returns the cheap
    // all-zero default without minting (the receiver never traces anyway).
    if !TRACING_ENABLED.load(Ordering::Relaxed) {
        return HewTraceContext::default();
    }
    HewTraceContext {
        trace_id_hi: next_random_id(),
        trace_id_lo: next_random_id(),
        span_id: next_random_id(),
        parent_span_id: 0,
        flags: 1, // sampled
    }
}

// live on not(wasm32) — mailbox.rs:msg_node_alloc_sys (native system-send path)
#[cfg_attr(target_arch = "wasm32", allow(dead_code))]
/// Capture the installed execution-context trace for a SYSTEM/control-plane
/// send WITHOUT minting a fresh root.
///
/// TRACE-CONTEXT COMPLETENESS (S3, crash-recovery seam): system sends
/// (supervisor child-event notifications, link/monitor signals) reach
/// `hew_mailbox_send_sys`, and the crash notification originates from
/// `hew_actor_trap` — signal-handler-adjacent context where minting a trace
/// root is forbidden. Unlike [`current_context`] (the external-user-send seam),
/// this helper NEVER mints: it returns the already-installed context when one
/// is present, otherwise the all-zero default. Minting the crash-recovery root
/// is therefore deferred to [`ensure_supervisor_trace_root`], which runs in
/// normal supervisor-dispatch context.
pub(crate) fn system_context() -> HewTraceContext {
    trace_context().unwrap_or_default()
}

/// Ensure the current (supervisor-dispatch) execution context carries a
/// non-zero, SAMPLED trace root, minting a fresh crash-origin root when the
/// installed context is absent or all-zero.
///
/// TRACE-CONTEXT COMPLETENESS (S3, crash-recovery seam): a child crash is
/// reported from `hew_actor_trap`, which runs in **signal-handler context**
/// where minting/allocating a context is not async-signal-safe. So the root is
/// established HERE — on the supervisor-dispatch side, which runs in normal
/// context — NOT inside the trap. The supervisor's restart/escalate spans (S2)
/// then parent under a real, sampled trace id instead of the unsampled
/// zero-parent fallback that `hew_trace_begin` would otherwise synthesise from
/// the trap's zero-context sys-message.
///
/// Read-only side effect: no-op when tracing is disabled or no context slot is
/// installed; never gates supervisor control flow.
pub(crate) fn ensure_supervisor_trace_root() {
    if !TRACING_ENABLED.load(Ordering::Relaxed) {
        return;
    }
    match trace_context() {
        // A real (non-zero) trace is already installed — inherit it so the
        // crash-recovery spans join the existing trace rather than starting a
        // disconnected root.
        Some(ctx) if ctx.trace_id_hi != 0 || ctx.trace_id_lo != 0 => {}
        // Absent or all-zero context: mint a fresh sampled crash-origin root.
        _ => {
            let root = HewTraceContext {
                trace_id_hi: next_random_id(),
                trace_id_lo: next_random_id(),
                span_id: next_random_id(),
                parent_span_id: 0,
                flags: 1, // sampled
            };
            let _ = write_trace_context(root);
        }
    }
}

#[cfg(test)]
pub(crate) fn set_context(ctx: HewTraceContext) {
    let _ = write_trace_context(ctx);
}

// live on not(wasm32) — schedule_actor_after_enqueue; dead on wasm32; caller actor.rs:3439
#[cfg_attr(target_arch = "wasm32", allow(dead_code))]
pub(crate) fn record_send(actor_id: u64, msg_type: i32) {
    if !TRACING_ENABLED.load(Ordering::Relaxed) {
        return;
    }
    record_lifecycle_event(actor_id, SPAN_SEND, msg_type);
}

/// Record a channel or lambda-actor lifecycle event (created, split, closed, spawned, released).
///
/// `handle_addr` is the raw pointer address of the relevant handle cast to `u64`.
/// It serves as the handle identity — there are no sequential IDs on channel substrate types.
///
/// This is the emission point for `SPAN_DUPLEX_*`, `SPAN_SINK_CLOSED`,
/// `SPAN_STREAM_CLOSED`, `SPAN_LAMBDA_SPAWNED`, and `SPAN_LAMBDA_RELEASED`.
// live on not(wasm32) — stream/lambda_actor/duplex (native-only modules); dead on wasm32
#[cfg_attr(target_arch = "wasm32", allow(dead_code))]
pub(crate) fn record_channel_event(handle_addr: u64, event_type: i32) {
    if !TRACING_ENABLED.load(Ordering::Relaxed) {
        return;
    }
    record_event(HewTraceEvent {
        trace_id_hi: 0,
        trace_id_lo: 0,
        span_id: 0,
        parent_span_id: 0,
        actor_id: handle_addr,
        event_type,
        msg_type: 0,
        timestamp_ns: monotonic_ns(),
    });
}

/// Record a single I/O-level event using an explicit `HewTraceContext`.
///
/// Used for `SPAN_IO_ACCEPT` and `SPAN_IO_RECV` which are not bound to any
/// actor ID (`actor_id` = 0) and carry their own freshly generated span context.
// live on not(wasm32) — io_accept_span_begin / io_recv_span_begin; dead on wasm32
#[cfg_attr(target_arch = "wasm32", allow(dead_code))]
fn record_io_event(ctx: &HewTraceContext, event_type: i32) {
    record_event(HewTraceEvent {
        trace_id_hi: ctx.trace_id_hi,
        trace_id_lo: ctx.trace_id_lo,
        span_id: ctx.span_id,
        parent_span_id: ctx.parent_span_id,
        actor_id: 0,
        event_type,
        msg_type: 0,
        timestamp_ns: monotonic_ns(),
    });
}

/// Called at `hew_connmgr_add` when a TCP connection is accepted.
///
/// Creates a new root I/O span, emits `SPAN_IO_ACCEPT` begin and end events
/// (accept is synchronous — the span collapses to an instant), then stashes
/// the context by `conn_id` so `reader_loop` can parent per-envelope
/// `SPAN_IO_RECV` spans under this accept span.
///
/// Fast path: returns immediately (no allocation) when tracing is disabled.
// live on not(wasm32) — connection.rs; dead on wasm32; caller connection.rs:1953
#[cfg_attr(target_arch = "wasm32", allow(dead_code))]
pub(crate) fn io_accept_span_begin(conn_id: c_int) {
    if !TRACING_ENABLED.load(Ordering::Relaxed) {
        return;
    }
    let ctx = HewTraceContext {
        trace_id_hi: next_random_id(),
        trace_id_lo: next_random_id(),
        span_id: next_random_id(),
        parent_span_id: 0,
        flags: 1, // sampled
    };
    record_io_event(&ctx, SPAN_IO_ACCEPT);
    // Accept is synchronous: immediately emit the end so the span is closed.
    record_io_event(&ctx, SPAN_END);
    // Stash so reader_loop can parent io_recv spans under this accept span.
    io_span_stash(conn_id, ctx);
}

/// Called in `reader_loop` before `router_fn` is invoked for a decoded envelope.
///
/// Looks up the accept span for `conn_id`, creates a child `SPAN_IO_RECV`
/// span, emits its `SPAN_IO_RECV` begin event, and sets the canonical trace
/// lane so that `msg_node_alloc` captures this span as the parent for
/// the subsequent actor-dispatch span.
///
/// Returns the prior trace context so the caller can restore it after
/// `router_fn` returns.  If tracing is disabled, returns `None` (caller
/// skips tracing entirely).
// live on not(wasm32) — connection.rs; dead on wasm32; caller connection.rs:1546
#[cfg_attr(target_arch = "wasm32", allow(dead_code))]
pub(crate) fn io_recv_span_begin(conn_id: c_int) -> Option<HewTraceContext> {
    if !TRACING_ENABLED.load(Ordering::Relaxed) {
        return None;
    }

    // Look up the accept span context.  NOT consumed — many envelopes arrive
    // per connection, each parented under the same accept span.
    //
    // INTENTIONAL DEFAULT (not fail-open): a missing entry yields the all-zero
    // context, but the child-span construction immediately below treats a zero
    // trace id as "no parent" and MINTS a fresh root via `next_random_id`
    // (mirroring `io_accept_span_begin`). So the span is never an orphan with
    // zero ids — it becomes a new trace root.
    //   WHY  — tracing can be enabled mid-connection, after the accept span
    //          for `conn_id` was already emitted/evicted; the recv still
    //          deserves a valid (root) span rather than being dropped.
    //   WHEN — revisit if accept-span stashing becomes guaranteed-before-recv
    //          (e.g. tracing can only toggle at startup), making the miss a bug.
    //   WHAT — promote to fail-closed (skip + count) once the miss is provably
    //          a producer error rather than a benign late-enable.
    let parent_ctx = IO_SPAN_TABLE
        .lock_or_recover()
        .get(&conn_id)
        .copied()
        .unwrap_or_default();

    // Build the io_recv span as a child of the accept span.
    let recv_ctx = HewTraceContext {
        trace_id_hi: if parent_ctx.trace_id_hi != 0 {
            parent_ctx.trace_id_hi
        } else {
            next_random_id()
        },
        trace_id_lo: if parent_ctx.trace_id_lo != 0 {
            parent_ctx.trace_id_lo
        } else {
            next_random_id()
        },
        span_id: next_random_id(),
        parent_span_id: parent_ctx.span_id,
        flags: 1, // sampled
    };

    record_io_event(&recv_ctx, SPAN_IO_RECV);

    let current = crate::execution_context::current_context();
    if current.is_null() {
        let mut boxed = Box::new(crate::execution_context::HewExecutionContext {
            trace: recv_ctx,
            flags: TRACE_OWNS_EXECUTION_CONTEXT,
            prev_context: current,
            ..crate::execution_context::HewExecutionContext::default()
        });
        let raw = (&raw mut *boxed).cast::<crate::execution_context::HewExecutionContext>();
        let installed_prev = crate::execution_context::set_current_context(raw);
        debug_assert_eq!(installed_prev, current);
        let _ = Box::into_raw(boxed); // ALLOCATOR-PAIRING: GlobalAlloc
        Some(HewTraceContext::default())
    } else {
        // SAFETY: current is the installed canonical context for this thread.
        let saved = unsafe { (*current).trace };
        // SAFETY: current is non-null and mutable through the TLS boundary.
        unsafe {
            (*current).trace = recv_ctx;
        }
        Some(saved)
    }
}

/// Called in `reader_loop` after `router_fn` returns.
///
/// Emits `SPAN_END` for the current `io_recv` span and restores `saved_ctx`.
///
/// `saved_ctx` must be the value returned by the matching `io_recv_span_begin`.
// live on not(wasm32) — connection.rs; dead on wasm32; caller connection.rs:1567
#[cfg_attr(target_arch = "wasm32", allow(dead_code))]
pub(crate) fn io_recv_span_end(saved_ctx: HewTraceContext) {
    // Emit SPAN_END using the current (recv) context that was installed in begin.
    let current = crate::execution_context::require_current_context();
    if current.is_null() {
        return;
    }
    // SAFETY: current is the installed canonical context for this thread.
    let recv_ctx = unsafe { (*current).trace };
    record_io_event(&recv_ctx, SPAN_END);
    // SAFETY: current is non-null and mutable through the TLS boundary.
    let owns_context = unsafe { (*current).flags & TRACE_OWNS_EXECUTION_CONTEXT != 0 };
    if owns_context {
        // SAFETY: contexts tagged with TRACE_OWNS_EXECUTION_CONTEXT were
        // allocated by io_recv_span_begin and are restored/dropped here.
        let prev = unsafe { (*current).prev_context };
        let restored = crate::execution_context::set_current_context(prev);
        debug_assert_eq!(restored, current);
        // SAFETY: current came from Box::into_raw in io_recv_span_begin.
        unsafe {
            drop(Box::from_raw(current)); // ALLOCATOR-PAIRING: GlobalAlloc
        }
    } else {
        // SAFETY: current is non-null and mutable through the TLS boundary.
        unsafe {
            (*current).trace = saved_ctx;
        }
    }
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
    TRACE_EVENTS.access(|events| events.len() as u64)
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
    TRACE_EVENTS.access(|events| {
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
    })
}

/// Clear all recorded trace events.
#[no_mangle]
pub extern "C" fn hew_trace_clear() {
    TRACE_EVENTS.access(std::collections::VecDeque::clear);
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

/// Reset all tracing state (disable + clear events + reset context + clear I/O span table).
#[no_mangle]
pub extern "C" fn hew_trace_reset() {
    TRACING_ENABLED.store(false, Ordering::Release);
    TRACE_EVENTS.access(std::collections::VecDeque::clear);
    let ctx = crate::execution_context::current_context();
    if !ctx.is_null() {
        // SAFETY: ctx is the installed canonical context for this thread.
        unsafe {
            (*ctx).trace = HewTraceContext::default();
        }
    }
    IO_SPAN_TABLE.lock_or_recover().clear();
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
///   "event_type": "begin"|"end"|"spawn"|"crash"|"stop"|"send"|
///                 "io_accept"|"io_recv"|
///                 "duplex_created"|"duplex_half_split"|"duplex_closed"|
///                 "sink_closed"|"stream_closed"|
///                 "lambda_spawned"|"lambda_released"|"unknown",
///   "msg_type": N,
///   "timestamp_ns": N,
///   "handler_name": "ActorType::handler_name"|null
/// }
/// ```
///
/// On native targets, `actor_type_id` is the dispatch function pointer cast to
/// `u64`. On WASM targets, it is a deterministic non-zero bridge-local id
/// derived from registered actor metadata. It is `0` when the actor attribution
/// source cannot resolve the event at drain time.
///
/// `actor_type` is the registered Hew type name (e.g. `"Counter"`). It is
/// `null` when neither native registration nor WASM bridge metadata can resolve
/// the event.
///
/// `handler_name` is non-null on native builds when `hew_register_handler_name`
/// has been called for the `(dispatch_fn, msg_type)` pair (requires codegen
/// emission — see #1259).  On WASM builds the bridge metadata registry is used.
/// Resolve the drained `event_type` string, FAIL-CLOSED for codes outside the
/// closed [`EVENT_TYPE_NAMES`] taxonomy.
///
/// For a mapped code this returns the canonical taxonomy name. For an *unmapped*
/// code it returns a distinguishable `invalid:<code>` token — never the
/// `"unknown"` sentinel, which `hew-observe` reserves for valid-but-future event
/// names. This is the producer-side fail-closed guarantee: an out-of-taxonomy
/// integer surfaces as detectable corruption rather than masquerading as a known
/// event type. See the `event_type_name` contract for the caller obligation.
#[cfg(feature = "profiler")]
fn drain_event_type_label(event_type: i32) -> std::borrow::Cow<'static, str> {
    match event_type_name(event_type) {
        Some(name) => std::borrow::Cow::Borrowed(name),
        None => std::borrow::Cow::Owned(format!("invalid:{event_type}")),
    }
}

#[cfg(feature = "profiler")]
pub fn drain_events_json() -> String {
    use std::fmt::Write as _;

    TRACE_EVENTS.access(|events| {
    let count = events.len().min(256);
    let mut json = String::from("[");
    for i in 0..count {
        if let Some(ev) = events.pop_front() {
            if i > 0 {
                json.push(',');
            }
            // FAIL-CLOSED: an `event_type` integer outside the closed
            // `EVENT_TYPE_NAMES` taxonomy is a producer bug (a `SPAN_*`
            // constant emitted without its taxonomy row). It must NOT be
            // laundered into the valid-looking `"unknown"` sentinel that
            // `hew-observe` reserves for genuinely-future-but-valid names —
            // doing so would let trace corruption masquerade as a known
            // event type, contradicting the `event_type_name` contract
            // documented at its definition. `drain_event_type_label` returns a
            // distinguishable `invalid:<code>` token instead; the `debug_assert`
            // is a dev-time tripwire so the offending producer surfaces loudly.
            let event_type_label = drain_event_type_label(ev.event_type);
            debug_assert!(
                !event_type_label.starts_with("invalid:"),
                "drain_events_json: event_type {} is outside the closed \
                 EVENT_TYPE_NAMES taxonomy; a SPAN_* constant was emitted \
                 without its taxonomy row (fail-closed)",
                ev.event_type
            );
            let event_type_str: &str = event_type_label.as_ref();

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
                // INTENTIONAL DEFAULT (not fail-open): a `None` from the
                // registry means the actor was freed before this drain ran (or
                // its type was never registered). Unlike the event_type case
                // above, `0` here is NOT a corruption sentinel — it is the
                // documented #1260 contract: `actor_type_id == 0` pairs with
                // `actor_type == null` (handled below) to mean "attribution
                // unavailable", a legitimate and detectable state distinct from
                // a valid non-zero dispatch pointer.
                //   WHY  — actor lifetime is shorter than the drain window; the
                //          event itself (ids, type) is still valid and worth
                //          emitting unattributed rather than dropped.
                //   WHEN — revisit if event records ever carry the dispatch ptr
                //          inline (ABI-versioned HewTraceEvent), removing the
                //          lookup race entirely.
                //   WHAT — consumers read (0, null) as "unattributed", never as
                //          a real actor type.
                let dispatch_ptr =
                    crate::profiler::actor_registry::lookup_dispatch_for_actor_id(ev.actor_id)
                        .unwrap_or(0);
                let type_id = dispatch_ptr as u64;
                let type_name =
                    crate::profiler::actor_registry::lookup_dispatch_type_by_ptr(dispatch_ptr);
                let hname = crate::profiler::actor_registry::handler_name_by_ptr(
                    dispatch_ptr,
                    ev.msg_type,
                );
                let actor_type = if type_name == "Actor" && type_id == 0 {
                    None
                } else {
                    Some(type_name.to_owned())
                };
                (type_id, actor_type, hname)
            };

            #[cfg(any(target_arch = "wasm32", test))]
            let (actor_type_id, actor_type_str, handler_name): (u64, Option<String>, Option<String>) =
                crate::bridge::resolve_actor_trace_attribution(ev.msg_type).map_or_else(
                    || (0, None, crate::bridge::resolve_handler_name(ev.msg_type)),
                    |(id, actor_type, handler_name)| (id, Some(actor_type), handler_name),
                );

            let actor_type_json = match actor_type_str {
                Some(actor_type_str) if actor_type_id != 0 => format!("\"{actor_type_str}\""),
                _ => "null".to_owned(),
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
    })
}

// ── Test serialisation ─────────────────────────────────────────────────
//
// `TRACING_ENABLED` and `TRACE_EVENTS` are process-level statics shared by
// every test thread in the binary.  Any test that reads or writes them must
// hold `TRACING_TEST_LOCK` for its entire duration.  This includes tests
// outside this module (e.g. `scheduler::tests::activate_records_dispatch_span_events`)
// — which is why the lock and guard constructor are `pub(crate)` rather than
// confined to `mod tests`.

/// Single serialisation mutex for all tests that touch tracing global state.
///
/// Hold this for the **entire** duration of any test that calls
/// `hew_trace_enable`, `hew_trace_reset`, `hew_trace_begin`, `hew_trace_end`,
/// `hew_trace_drain`, `record_channel_event`, or reads `TRACE_EVENTS` /
/// `TRACING_ENABLED`.  Failing to hold it against a concurrent test that does
/// the same causes `TRACING_ENABLED`/`TRACE_EVENTS` to race.
#[cfg(test)]
pub(crate) static TRACING_TEST_LOCK: Mutex<()> = Mutex::new(());

/// Acquire the tracing serialisation lock.
///
/// Returns a guard that releases the lock on drop.  Recovers from a poisoned
/// lock (a panicking test left it poisoned) so subsequent tests are not
/// permanently blocked.
#[cfg(test)]
pub(crate) fn tracing_test_guard() -> std::sync::MutexGuard<'static, ()> {
    TRACING_TEST_LOCK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
}

// ── Tests ──────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::execution_context::{HewExecutionContext, TestExecutionContext};

    struct TraceTestSetup {
        _guard: std::sync::MutexGuard<'static, ()>,
        _ctx: TestExecutionContext,
    }

    fn setup() -> TraceTestSetup {
        let guard = tracing_test_guard();
        let ctx = TestExecutionContext::install(HewExecutionContext::default());
        hew_trace_reset();
        hew_trace_enable(1);
        // Prime the monotonic clock EPOCH before any events are recorded.
        // On the first call to `monotonic_ns`, `OnceLock::get_or_init` sets
        // EPOCH to `Instant::now()` and then immediately calls `elapsed()` on
        // that same instant.  On fast hardware the two calls can land within the
        // same nanosecond tick, causing `elapsed().as_nanos() as u64 == 0` and
        // making the first recorded `timestamp_ns` equal to 0.  Calling
        // `trace_now_ns()` here ensures EPOCH is already set before any
        // assertions on `timestamp_ns` are made.
        let _ = trace_now_ns();
        TraceTestSetup {
            _guard: guard,
            _ctx: ctx,
        }
    }

    // ── Closed taxonomy tests ──────────────────────────────────────────

    /// Every `SPAN_*` constant declared at module scope must appear in
    /// [`EVENT_TYPE_NAMES`]. This is the producer-side gate that holds the
    /// closed v0.5 trace-event taxonomy in sync with what the runtime
    /// actually emits.
    #[test]
    fn event_type_name_mapping_is_total() {
        let all_spans = [
            SPAN_BEGIN,
            SPAN_END,
            SPAN_SPAWN,
            SPAN_CRASH,
            SPAN_STOP,
            SPAN_SEND,
            SPAN_IO_ACCEPT,
            SPAN_IO_RECV,
            SPAN_DUPLEX_CREATED,
            SPAN_DUPLEX_HALF_SPLIT,
            SPAN_DUPLEX_CLOSED,
            SPAN_SINK_CLOSED,
            SPAN_STREAM_CLOSED,
            SPAN_LAMBDA_SPAWNED,
            SPAN_LAMBDA_RELEASED,
            SPAN_SUPERVISOR_RESTART,
            SPAN_SUPERVISOR_ESCALATE,
            SPAN_SUPERVISOR_CIRCUIT_OPEN,
            SPAN_SUPERVISOR_CIRCUIT_HALF_OPEN,
            SPAN_SUPERVISOR_CIRCUIT_CLOSE,
            SPAN_SUPERVISOR_MAX_RESTARTS,
            SPAN_SUPERVISOR_BACKOFF,
        ];
        for code in all_spans {
            assert!(
                event_type_name(code).is_some(),
                "SPAN_* constant {code} is missing from EVENT_TYPE_NAMES; \
                 every producer-emitted span must have a taxonomy entry"
            );
        }
        assert_eq!(
            EVENT_TYPE_NAMES.len(),
            all_spans.len(),
            "EVENT_TYPE_NAMES has entries that no SPAN_* constant references; \
             the closed taxonomy must mirror declared span codes one-for-one"
        );
    }

    #[test]
    fn event_type_name_returns_none_for_unknown_codes() {
        assert!(event_type_name(i32::MAX).is_none());
        assert!(event_type_name(-1).is_none());
        // Any code beyond the last assigned SPAN_* must not resolve.
        assert!(event_type_name(SPAN_SUPERVISOR_BACKOFF + 1).is_none());
    }

    /// S1 fail-closed: the producer drain must NEVER serialise an `event_type`
    /// integer outside the closed taxonomy as the valid-looking `"unknown"`
    /// sentinel. An out-of-range code must surface as a distinguishable
    /// `invalid:<code>` corruption token so consumers can DETECT it rather than
    /// be deceived. This test FAILS against the pre-S1 `.unwrap_or("unknown")`
    /// drain code.
    #[cfg(feature = "profiler")]
    #[test]
    fn drain_label_for_invalid_event_type_is_not_unknown() {
        // A mapped code resolves to its canonical name.
        assert_eq!(drain_event_type_label(SPAN_SPAWN).as_ref(), "spawn");

        // An out-of-taxonomy code must NOT launder into "unknown".
        let bogus = i32::MAX;
        let label = drain_event_type_label(bogus);
        assert_ne!(
            label.as_ref(),
            "unknown",
            "out-of-range event_type must not masquerade as the valid \"unknown\" sentinel"
        );
        assert_eq!(label.as_ref(), format!("invalid:{bogus}"));
        assert!(label.starts_with("invalid:"));
    }

    #[test]
    fn event_type_names_are_unique_and_nonempty() {
        let mut seen = std::collections::HashSet::new();
        for (code, name) in EVENT_TYPE_NAMES {
            assert!(!name.is_empty(), "SPAN_{code} has empty name");
            assert!(
                seen.insert(*name),
                "duplicate event_type name {name:?} in EVENT_TYPE_NAMES"
            );
            assert_ne!(
                *name, "unknown",
                "{name:?} collides with the JSON fallback sentinel"
            );
        }
    }

    #[test]
    fn enable_disable() {
        let _guard = tracing_test_guard();
        hew_trace_reset();
        assert_eq!(hew_trace_is_enabled(), 0);
        hew_trace_enable(1);
        assert_eq!(hew_trace_is_enabled(), 1);
        hew_trace_enable(0);
        assert_eq!(hew_trace_is_enabled(), 0);
    }

    #[test]
    fn trace_get_context_without_execution_context_fails_closed() {
        let _guard = tracing_test_guard();
        crate::hew_clear_error();
        let mut out = HewTraceContext {
            trace_id_hi: 1,
            trace_id_lo: 1,
            span_id: 1,
            parent_span_id: 1,
            flags: 1,
        };
        // SAFETY: out is a valid output pointer.
        unsafe { hew_trace_get_context(&raw mut out) };
        assert_eq!(out.trace_id_hi, 0);
        assert_eq!(out.trace_id_lo, 0);
        assert_eq!(out.span_id, 0);
        assert_eq!(out.parent_span_id, 0);
        assert_eq!(out.flags, 0);
        let err = crate::hew_last_error();
        assert!(!err.is_null());
        // SAFETY: hew_last_error returned a non-null C string.
        let err = unsafe { std::ffi::CStr::from_ptr(err).to_str().unwrap() };
        assert_eq!(
            err,
            crate::execution_context::EXECUTION_CONTEXT_NOT_INSTALLED
        );
        crate::hew_clear_error();
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

    /// S2: a supervisor decision emitted via `record_supervisor_event` lands on
    /// the ring buffer with the canonical taxonomy name and a non-zero span id
    /// (inherited from the supervisor's live dispatch context), and drains
    /// through `drain_events_json` as a `supervisor_restart` event carrying the
    /// strategy discriminator in `msg_type`.
    #[cfg(feature = "profiler")]
    #[test]
    fn supervisor_event_drains_with_name_and_span() {
        let _guard = setup();

        // Establish a non-zero span context as the supervisor dispatch would.
        hew_trace_begin(7, 0);
        let mut ctx = HewTraceContext::default();
        // SAFETY: valid pointer.
        unsafe { hew_trace_get_context(&raw mut ctx) };
        assert_ne!(ctx.span_id, 0);

        // Emit a restart decision carrying strategy discriminator 2.
        record_supervisor_event(7, SPAN_SUPERVISOR_RESTART, 2);

        let json = drain_events_json();
        let parsed: serde_json::Value =
            serde_json::from_str(&json).expect("drain_events_json must produce valid JSON");
        let arr = parsed.as_array().expect("drain JSON is an array");
        let restart = arr
            .iter()
            .find(|e| e["event_type"] == "supervisor_restart")
            .expect("supervisor_restart event must be present on the export surface");
        assert_eq!(restart["msg_type"], 2);
        assert_eq!(restart["actor_id"], 7);
        assert_ne!(
            restart["span_id"].as_u64().unwrap(),
            0,
            "supervisor event must inherit the live dispatch span, not a zero id"
        );

        hew_trace_reset();
    }

    /// S2: every supervisor taxonomy name resolves (no `invalid:` token) and is
    /// distinct from the `"unknown"` sentinel.
    #[test]
    fn supervisor_event_names_resolve() {
        for code in [
            SPAN_SUPERVISOR_RESTART,
            SPAN_SUPERVISOR_ESCALATE,
            SPAN_SUPERVISOR_CIRCUIT_OPEN,
            SPAN_SUPERVISOR_CIRCUIT_HALF_OPEN,
            SPAN_SUPERVISOR_CIRCUIT_CLOSE,
            SPAN_SUPERVISOR_MAX_RESTARTS,
            SPAN_SUPERVISOR_BACKOFF,
        ] {
            let name = event_type_name(code).expect("supervisor span must map");
            assert!(name.starts_with("supervisor_"));
            assert_ne!(name, "unknown");
        }
    }

    /// S3 (external-send seam): with tracing enabled and NO execution context
    /// installed (a send originating outside any actor dispatch), the captured
    /// context is a fresh SAMPLED root with a non-zero trace id and a zero
    /// parent — not an all-zero orphan. With tracing disabled it stays the cheap
    /// zero default.
    #[test]
    fn current_context_mints_sampled_root_on_external_send() {
        let _guard = tracing_test_guard();
        hew_trace_reset();
        hew_trace_enable(1);

        // No execution context installed: external-send seam.
        let ctx = current_context();
        assert!(
            ctx.trace_id_hi != 0 || ctx.trace_id_lo != 0,
            "external send must mint a non-zero trace root, not a zero orphan"
        );
        assert_ne!(ctx.span_id, 0);
        assert_eq!(ctx.parent_span_id, 0, "external send is a trace root");
        assert_eq!(ctx.flags, 1, "minted root must be sampled");

        // Each external send is its own root.
        let ctx2 = current_context();
        assert_ne!(
            (ctx.trace_id_hi, ctx.trace_id_lo),
            (ctx2.trace_id_hi, ctx2.trace_id_lo)
        );

        // Disabled tracing returns the cheap zero default (no minting).
        hew_trace_enable(0);
        let off = current_context();
        assert_eq!(off.trace_id_hi, 0);
        assert_eq!(off.trace_id_lo, 0);
        assert_eq!(off.span_id, 0);

        hew_trace_reset();
    }

    /// S3 (crash-recovery seam): `ensure_supervisor_trace_root` mints a sampled
    /// root when the installed context is all-zero (the crash sys-message came
    /// from the signal-context trap), then INHERITS an already-non-zero context
    /// on a subsequent call (joins the live trace rather than re-rooting).
    #[test]
    fn ensure_supervisor_trace_root_mints_then_inherits() {
        let _guard = tracing_test_guard();
        let _ctx = TestExecutionContext::install(HewExecutionContext::default());
        hew_trace_reset();
        hew_trace_enable(1);

        // All-zero installed context → mint a sampled crash-origin root.
        ensure_supervisor_trace_root();
        let minted = trace_context().expect("context is installed");
        assert!(
            minted.trace_id_hi != 0 || minted.trace_id_lo != 0,
            "crash-recovery must parent under a real trace id"
        );
        assert_ne!(minted.span_id, 0);
        assert_eq!(minted.parent_span_id, 0);
        assert_eq!(minted.flags, 1, "minted crash-origin root must be sampled");

        // Already-non-zero context → inherited unchanged.
        ensure_supervisor_trace_root();
        let after = trace_context().expect("context is installed");
        assert_eq!(after.trace_id_hi, minted.trace_id_hi);
        assert_eq!(after.trace_id_lo, minted.trace_id_lo);
        assert_eq!(after.span_id, minted.span_id);

        hew_trace_enable(0);
        hew_trace_reset();
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
        let _guard = tracing_test_guard();
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
        let _trace_guard = tracing_test_guard();
        let _ctx = TestExecutionContext::install(HewExecutionContext::default());

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

    #[cfg(feature = "profiler")]
    #[test]
    fn drain_events_json_includes_wasm_registered_actor_type() {
        use crate::bridge::{
            hew_wasm_register_actor_meta, reset_bridge_full, HewActorMeta, HewHandlerMeta,
            BRIDGE_TEST_LOCK,
        };
        let _runtime_guard = crate::runtime_test_guard();

        let _bridge_guard = BRIDGE_TEST_LOCK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let _trace_guard = tracing_test_guard();
        let _ctx = TestExecutionContext::install(HewExecutionContext::default());

        reset_bridge_full();
        hew_trace_reset();
        hew_trace_enable(1);

        let actor_name = b"TypedActor\0";
        let handler_name = b"on_tick\0";
        let handler = HewHandlerMeta {
            name: handler_name.as_ptr().cast(),
            msg_type: 88,
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
        // SAFETY: all pointers remain valid for this call; registration copies strings.
        unsafe { hew_wasm_register_actor_meta(&raw const actor_meta) };

        hew_trace_begin(42, 88);
        hew_trace_begin(42, 89);

        let json = crate::tracing::drain_events_json();

        assert!(json.contains(r#""actor_type":"TypedActor""#), "{json}");
        assert!(
            !json.contains(r#""actor_type_id":0,"actor_type":"TypedActor""#),
            "registered actor_type_id must be non-zero: {json}"
        );
        assert!(
            json.contains(r#""handler_name":"TypedActor::on_tick""#),
            "{json}"
        );
        assert!(
            json.contains(r#""actor_type_id":0,"actor_type":null"#),
            "unknown msg_type must remain unattributed: {json}"
        );

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
        let _guard = setup();

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
        let _trace_guard = tracing_test_guard();
        let _ctx = TestExecutionContext::install(HewExecutionContext::default());

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

    // ── I/O span chain tests ───────────────────────────────────────────

    /// Verify that `io_accept_span_begin` emits a `SPAN_IO_ACCEPT` begin event
    /// followed by a `SPAN_END` event, and stashes a non-zero context.
    #[test]
    fn io_accept_span_emits_begin_and_end() {
        let _guard = setup();

        io_accept_span_begin(42);

        // Two events: SPAN_IO_ACCEPT (begin) + SPAN_END.
        assert_eq!(
            hew_trace_event_count(),
            2,
            "io_accept_span_begin must emit exactly 2 events"
        );

        let events = drain_events(2);
        assert_eq!(
            events[0].event_type, SPAN_IO_ACCEPT,
            "first event must be SPAN_IO_ACCEPT"
        );
        assert_eq!(
            events[1].event_type, SPAN_END,
            "second event must be SPAN_END"
        );

        // Both events must share the same trace and span ID.
        assert_eq!(events[0].trace_id_hi, events[1].trace_id_hi);
        assert_eq!(events[0].trace_id_lo, events[1].trace_id_lo);
        assert_eq!(events[0].span_id, events[1].span_id);
        assert_ne!(events[0].span_id, 0, "span_id must be non-zero");
        assert_eq!(
            events[0].parent_span_id, 0,
            "accept span must be a root span"
        );

        // Side table must contain the stashed context.
        let stashed = IO_SPAN_TABLE.lock_or_recover().get(&42).copied();
        assert!(
            stashed.is_some(),
            "accept span context must be stashed in IO_SPAN_TABLE"
        );
        assert_eq!(stashed.unwrap().span_id, events[0].span_id);

        // Cleanup.
        io_span_evict(42);
    }

    /// Verify that `io_recv_span_begin`/`io_recv_span_end` emits an `SPAN_IO_RECV`
    /// begin event parented under the accept span, and a `SPAN_END` event after.
    #[test]
    fn io_recv_span_parented_under_accept() {
        let _guard = setup();

        // Stash a fake accept context.
        let accept_ctx = HewTraceContext {
            trace_id_hi: 0xdead,
            trace_id_lo: 0xbeef,
            span_id: 0x1111,
            parent_span_id: 0,
            flags: 1,
        };
        io_span_stash(99, accept_ctx);

        let saved = io_recv_span_begin(99);
        assert!(
            saved.is_some(),
            "io_recv_span_begin must return Some when tracing is enabled"
        );

        io_recv_span_end(saved.unwrap());

        // Events: SPAN_IO_RECV (begin) + SPAN_END.
        let events = drain_events(2);
        assert_eq!(events.len(), 2);
        assert_eq!(events[0].event_type, SPAN_IO_RECV);
        assert_eq!(events[1].event_type, SPAN_END);

        // recv span must inherit the accept span's trace ID.
        assert_eq!(events[0].trace_id_hi, 0xdead);
        assert_eq!(events[0].trace_id_lo, 0xbeef);
        // recv span must be parented under the accept span.
        assert_eq!(events[0].parent_span_id, 0x1111);
        assert_ne!(
            events[0].span_id, 0x1111,
            "recv span_id must differ from accept span_id"
        );

        // Both events must share the recv span ID.
        assert_eq!(events[0].span_id, events[1].span_id);

        io_span_evict(99);
    }

    /// Verify that `io_recv_span_begin` sets the canonical trace lane so that
    /// a mailbox-enqueue (which captures `current_context()`) sees the `io_recv`
    /// span as its parent — this is the causal link to the actor-dispatch span.
    #[test]
    fn io_recv_span_propagates_context_to_mailbox_boundary() {
        let _guard = setup();

        // Simulate a stashed accept context.
        let accept_ctx = HewTraceContext {
            trace_id_hi: 0xaaaa,
            trace_id_lo: 0xbbbb,
            span_id: 0x2222,
            parent_span_id: 0,
            flags: 1,
        };
        io_span_stash(77, accept_ctx);

        // Simulate what reader_loop does before calling router_fn.
        let saved = io_recv_span_begin(77).expect("begin must succeed when tracing enabled");

        // At this point the canonical trace lane should be the io_recv span.
        let ctx_inside = current_context();
        assert_ne!(
            ctx_inside.span_id, 0,
            "context inside recv span must have non-zero span_id"
        );
        assert_eq!(
            ctx_inside.parent_span_id, 0x2222,
            "context inside recv span must be parented under accept span"
        );
        assert_eq!(ctx_inside.trace_id_hi, 0xaaaa);
        assert_eq!(ctx_inside.trace_id_lo, 0xbbbb);

        // Simulate router_fn → msg_node_alloc capturing current_context().
        let captured = current_context();

        io_recv_span_end(saved);

        // After end the canonical trace lane is restored to the prior default.
        let ctx_after = current_context();
        assert_eq!(
            ctx_after.span_id, 0,
            "trace lane must be restored after io_recv_span_end"
        );

        // The captured context (as mailbox would record it) links to the recv span.
        assert_eq!(captured.parent_span_id, 0x2222);

        io_span_evict(77);
    }

    /// Verify that when tracing is disabled, `io_accept_span_begin` emits no events
    /// and stashes nothing, and `io_recv_span_begin` returns `None`.
    #[test]
    fn io_span_disabled_path_emits_nothing() {
        let _guard = tracing_test_guard();
        hew_trace_reset();
        // Tracing is disabled after reset.

        io_accept_span_begin(55);
        assert_eq!(
            hew_trace_event_count(),
            0,
            "disabled io_accept_span_begin must emit no events"
        );
        assert!(
            IO_SPAN_TABLE.lock_or_recover().get(&55).is_none(),
            "disabled io_accept_span_begin must not stash context"
        );

        let result = io_recv_span_begin(55);
        assert!(
            result.is_none(),
            "disabled io_recv_span_begin must return None"
        );
        assert_eq!(
            hew_trace_event_count(),
            0,
            "disabled io_recv_span_begin must emit no events"
        );
    }

    /// Verify that `io_span_evict` removes the stashed context for a `conn_id`.
    #[test]
    fn io_span_evict_removes_entry() {
        let _guard = setup();

        io_accept_span_begin(33);
        assert!(IO_SPAN_TABLE.lock_or_recover().get(&33).is_some());

        io_span_evict(33);
        assert!(
            IO_SPAN_TABLE.lock_or_recover().get(&33).is_none(),
            "evict must remove the stashed context"
        );

        // Second evict is idempotent.
        io_span_evict(33);
    }

    // ── Channel lifecycle event tests ─────────────────────────────────────

    /// `record_channel_event` emits the expected event type and populates
    /// `actor_id` with the supplied handle address.
    #[test]
    fn channel_event_records_handle_addr_and_type() {
        let _guard = setup();

        let fake_addr: u64 = 0xDEAD_BEEF_0000_0001;
        record_channel_event(fake_addr, SPAN_DUPLEX_CREATED);

        assert_eq!(hew_trace_event_count(), 1);
        let events = drain_events(1);
        assert_eq!(events[0].event_type, SPAN_DUPLEX_CREATED);
        assert_eq!(events[0].actor_id, fake_addr);
        assert_ne!(events[0].timestamp_ns, 0, "timestamp must be populated");
    }

    /// `drain_events_json` renders all 7 new channel `event_type` strings
    /// round-trippable through `serde_json`.
    #[cfg(feature = "profiler")]
    #[test]
    fn channel_event_types_round_trip_through_json() {
        let _guard = setup();

        let new_types = [
            (SPAN_DUPLEX_CREATED, "duplex_created"),
            (SPAN_DUPLEX_HALF_SPLIT, "duplex_half_split"),
            (SPAN_DUPLEX_CLOSED, "duplex_closed"),
            (SPAN_SINK_CLOSED, "sink_closed"),
            (SPAN_STREAM_CLOSED, "stream_closed"),
            (SPAN_LAMBDA_SPAWNED, "lambda_spawned"),
            (SPAN_LAMBDA_RELEASED, "lambda_released"),
        ];

        for (span_const, expected_str) in new_types {
            hew_trace_reset();
            hew_trace_enable(1);

            record_channel_event(0xCAFE_0000_0000_0001, span_const);

            let json = drain_events_json();
            let parsed: Vec<serde_json::Value> =
                serde_json::from_str(&json).expect("drain_events_json must produce valid JSON");
            assert_eq!(
                parsed.len(),
                1,
                "expected exactly one event for {expected_str}"
            );
            assert_eq!(
                parsed[0]["event_type"].as_str().unwrap_or(""),
                expected_str,
                "event_type string must match for constant {span_const}"
            );
            // Wire-contract: actor_id field must be present and populated.
            assert!(
                parsed[0]["actor_id"].as_u64().is_some(),
                "actor_id must be a u64 for {expected_str}"
            );
            assert_ne!(
                parsed[0]["actor_id"].as_u64().unwrap(),
                0,
                "actor_id must be non-zero for {expected_str}"
            );
        }
    }

    /// `record_channel_event` is a no-op when tracing is disabled.
    #[test]
    fn channel_event_skipped_when_tracing_disabled() {
        let _guard = tracing_test_guard();
        hew_trace_reset();
        // tracing is disabled after reset

        record_channel_event(0x1234, SPAN_DUPLEX_CREATED);
        assert_eq!(hew_trace_event_count(), 0);
    }

    /// Verify that `hew_trace_reset` also clears the I/O span side table.
    #[test]
    fn trace_reset_clears_io_span_table() {
        let _guard = setup();

        io_accept_span_begin(11);
        assert!(IO_SPAN_TABLE.lock_or_recover().get(&11).is_some());

        hew_trace_reset();

        assert!(
            IO_SPAN_TABLE.lock_or_recover().get(&11).is_none(),
            "hew_trace_reset must clear IO_SPAN_TABLE"
        );
    }
}
