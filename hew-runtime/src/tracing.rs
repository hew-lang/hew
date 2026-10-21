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
//! Trace events are collected in a bounded sequence ring and can be
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
use std::cell::Cell;
use std::collections::{HashMap, VecDeque};
use std::ffi::c_int;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
#[cfg(not(target_arch = "wasm32"))]
use std::sync::Once;
use std::sync::{LazyLock, Mutex};

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
/// Retired with the duplex user surface; the id stays reserved so the
/// taxonomy numbering is stable.
pub const SPAN_DUPLEX_CREATED: i32 = 8;
/// Retired with the duplex user surface; the id stays reserved.
pub const SPAN_DUPLEX_HALF_SPLIT: i32 = 9;
/// Retired with the duplex user surface; the id stays reserved.
pub const SPAN_DUPLEX_CLOSED: i32 = 10;
/// Sink handle was closed (`hew_sink_close`). `actor_id` holds the pointer address.
pub const SPAN_SINK_CLOSED: i32 = 11;
/// Stream handle was closed (`hew_stream_close`). `actor_id` holds the pointer address.
pub const SPAN_STREAM_CLOSED: i32 = 12;

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
/// A compiled handler interceptor declined a delivery.
pub const SPAN_DELIVERY_REFUSED: i32 = 22;
/// Actor-terminal Rc survivors consumed by the typed release walker.
pub const SPAN_RC_SWEEP: i32 = 23;

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
    (SPAN_DELIVERY_REFUSED, "actor.delivery_refused"),
    (SPAN_RC_SWEEP, "actor.rc_sweep"),
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

/// Opaque identity of one producer process or one continuous trace stream.
///
/// The Observe wire representation is exactly 32 lowercase hexadecimal digits.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TraceStreamId {
    /// Upper 64 bits.
    pub hi: u64,
    /// Lower 64 bits.
    pub lo: u64,
}

impl TraceStreamId {
    /// Parse the canonical 32-lowercase-hex wire representation.
    #[must_use]
    pub fn parse_hex(value: &str) -> Option<Self> {
        if value.len() != 32
            || !value
                .bytes()
                .all(|byte| byte.is_ascii_digit() || (b'a'..=b'f').contains(&byte))
        {
            return None;
        }
        let hi = u64::from_str_radix(&value[..16], 16).ok()?;
        let lo = u64::from_str_radix(&value[16..], 16).ok()?;
        Some(Self { hi, lo })
    }
}

impl std::fmt::Display for TraceStreamId {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "{:016x}{:016x}", self.hi, self.lo)
    }
}

/// A richer internal trace record. The embedded [`HewTraceEvent`] remains the
/// stable C ABI projection while Observe receives lossless sequence and actor
/// endpoint metadata.
#[derive(Debug, Clone)]
pub struct TraceEventRecord {
    /// Monotonic sequence within `stream_id`, starting at one.
    pub sequence: u64,
    /// Stable C ABI projection.
    pub event: HewTraceEvent,
    /// Semantic subject kind for non-send events (`actor` or `channel`). Send
    /// and I/O events have no subject.
    pub subject_kind: Option<&'static str>,
    /// Stable subject ID in the namespace selected by `subject_kind`.
    pub subject_id: Option<u64>,
    /// Subject actor, only when `subject_kind == Some("actor")`.
    pub subject_actor_id: Option<u64>,
    /// Alias-proof subject actor incarnation when available.
    pub subject_spawn_serial: Option<u64>,
    /// Subject dispatch function captured while the actor is live.
    pub subject_dispatch_ptr: usize,
    /// Sending actor for a local send.
    pub source_actor_id: Option<u64>,
    /// Producer knowledge about the source endpoint.
    pub source_attribution: TraceActorAttribution,
    /// Alias-proof source incarnation where it is available at emission.
    pub source_spawn_serial: Option<u64>,
    /// Receiving actor for a local send; unknown for remote I/O events.
    pub destination_actor_id: Option<u64>,
    /// Producer knowledge about the destination endpoint.
    pub destination_attribution: TraceActorAttribution,
    /// Alias-proof destination incarnation where it is available at emission.
    pub destination_spawn_serial: Option<u64>,
    /// Source dispatch function captured while the actor is live.
    ///
    /// This is used only as a stable key into the profiler type/handler registry;
    /// consumers must not call the function through this integer.
    pub source_dispatch_ptr: usize,
    /// Destination dispatch function captured while the actor is live.
    pub destination_dispatch_ptr: usize,
    /// Authored refusal text, retained only while the bounded record is live.
    pub reason: Option<std::sync::Arc<str>>,
    /// Optional typed Rc reclamation summary for actor-terminal events.
    pub rc_sweep: Option<hew_observe_protocol::RcSweep>,
}

/// Correlation between the producer's monotonic clock and Unix wall time.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TraceClockAnchor {
    /// Producer monotonic timestamp at the anchor.
    pub monotonic_ns: u64,
    /// Unix timestamp corresponding to `monotonic_ns`.
    pub unix_epoch_ns: u64,
    /// Conservative uncertainty introduced while sampling the two clocks.
    pub uncertainty_ns: u64,
}

/// Producer knowledge about one actor endpoint.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TraceActorAttribution {
    /// A process-local actor identity is available.
    LocalActor,
    /// The send originated outside any actor or transport receive context.
    External,
    /// A transport receive context exists, but the remote actor identity was
    /// not propagated to this local mailbox boundary.
    RemoteUnknown,
    /// This event has no actor endpoint in this position.
    NotApplicable,
}

/// A non-destructive bounded snapshot for one stateless Observe cursor.
#[derive(Debug, Clone)]
pub struct TraceSnapshot {
    /// Stable for the lifetime of this runtime process.
    pub producer_instance_id: TraceStreamId,
    /// Changes whenever continuity is intentionally broken.
    pub stream_id: TraceStreamId,
    /// Whether tracing is currently accepting events.
    pub enabled: bool,
    /// First sequence still retained, or `next_sequence` when empty.
    pub oldest_sequence: u64,
    /// One greater than the largest sequence assigned in this stream.
    pub next_sequence: u64,
    /// Exclusive cursor to send on the next request.
    pub next_cursor: u64,
    /// More retained records follow `next_cursor`.
    pub has_more: bool,
    /// Exact number of records unavailable between the requested cursor and
    /// `oldest_sequence`. Zero means no cursor overrun.
    pub overrun_count: u64,
    /// Events accepted into this stream, including overwritten records.
    pub events_recorded_total: u64,
    /// Events evicted from bounded retention.
    pub events_overwritten_total: u64,
    /// Events rejected by sampling. Sampling is not implemented yet, so this is zero.
    pub events_sampled_out_total: u64,
    /// Current producer sampling policy (`"all"` or `"off"`).
    pub sampling_mode: &'static str,
    /// Producer monotonic time at snapshot creation.
    pub batch_source_monotonic_ns: u64,
    /// Stable clock anchor for this stream.
    pub clock_anchor: TraceClockAnchor,
    /// Retained records after the exclusive cursor, limited by the caller.
    pub events: Vec<TraceEventRecord>,
}

/// Invalid stateless-cursor requests.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TraceSnapshotError {
    /// The cursor belongs to a previous stream.
    StreamMismatch {
        /// Stable identity of the current producer process.
        producer_instance_id: TraceStreamId,
        /// Stream requested by the observer.
        requested: TraceStreamId,
        /// Current producer stream.
        current: TraceStreamId,
    },
    /// The exclusive cursor is beyond the largest sequence produced.
    FutureCursor {
        /// Cursor supplied by the observer.
        after: u64,
        /// One-past-last sequence in the current stream.
        next_sequence: u64,
    },
}

// ── Global state ───────────────────────────────────────────────────────

/// Global tracing enable flag.
static TRACING_ENABLED: AtomicBool = AtomicBool::new(false);

/// Cheap generation guard preventing an emitter that started before clear/reset
/// from appending into the replacement stream.
static STREAM_GENERATION: AtomicU64 = AtomicU64::new(1);

/// Monotonically increasing span ID counter (xorshift for speed).
static SPAN_COUNTER: AtomicU64 = AtomicU64::new(1);

/// Maximum number of trace events to retain.
const MAX_TRACE_EVENTS: usize = 16384;

#[derive(Debug)]
struct TraceStore {
    generation: u64,
    stream_id: TraceStreamId,
    next_sequence: u64,
    events_recorded_total: u64,
    events_overwritten_total: u64,
    clock_anchor: TraceClockAnchor,
    events: VecDeque<TraceEventRecord>,
}

impl TraceStore {
    fn new() -> Self {
        Self {
            generation: STREAM_GENERATION.load(Ordering::Acquire),
            stream_id: new_stream_id(),
            next_sequence: 1,
            events_recorded_total: 0,
            events_overwritten_total: 0,
            clock_anchor: sample_clock_anchor(),
            events: VecDeque::with_capacity(MAX_TRACE_EVENTS),
        }
    }

    fn begin_new_stream(&mut self) {
        self.generation = STREAM_GENERATION.fetch_add(1, Ordering::AcqRel) + 1;
        self.stream_id = new_stream_id();
        self.next_sequence = 1;
        self.events_recorded_total = 0;
        self.events_overwritten_total = 0;
        self.clock_anchor = sample_clock_anchor();
        self.events.clear();
    }
}

/// Recorded trace events (bounded, non-destructive sequence ring).
static TRACE_EVENTS: LazyLock<PoisonSafe<TraceStore>> =
    LazyLock::new(|| PoisonSafe::new(TraceStore::new()));

/// Stable identity of this producer process. It is deliberately distinct from
/// stream identity: clear/reset and disable→enable start new streams without
/// pretending that the runtime process restarted.
static PRODUCER_INSTANCE_ID: LazyLock<TraceStreamId> = LazyLock::new(new_stream_id);

#[cfg(all(feature = "profiler", not(target_arch = "wasm32")))]
pub(crate) fn producer_instance_id() -> String {
    PRODUCER_INSTANCE_ID.to_string()
}

#[derive(Debug, Clone, Copy)]
struct InternalCursor {
    stream_id: TraceStreamId,
    after: u64,
}

static RUST_DRAIN_CURSOR: PoisonSafe<Option<InternalCursor>> = PoisonSafe::new(None);
static C_DRAIN_CURSOR: PoisonSafe<Option<InternalCursor>> = PoisonSafe::new(None);
#[cfg(feature = "otel")]
static OTEL_CURSOR: PoisonSafe<Option<InternalCursor>> = PoisonSafe::new(None);
static TEST_REPORT_CURSOR: PoisonSafe<Option<InternalCursor>> = PoisonSafe::new(None);
#[cfg(feature = "profiler")]
static LEGACY_JSON_CURSOR: PoisonSafe<Option<InternalCursor>> = PoisonSafe::new(None);

#[cfg(test)]
#[derive(Clone)]
struct RecordPreLockBarrier {
    arrived: std::sync::Arc<std::sync::Barrier>,
    release: std::sync::Arc<std::sync::Barrier>,
}

#[cfg(test)]
static RECORD_PRE_LOCK_BARRIER: PoisonSafe<Option<RecordPreLockBarrier>> = PoisonSafe::new(None);

thread_local! {
    static REMOTE_SOURCE_DEPTH: Cell<u32> = const { Cell::new(0) };
}

/// Scoped marker for a local mailbox submission originating from a remote
/// transport frame. It carries no fabricated remote identity; it only prevents
/// the producer from mislabelling the missing source as an external local send.
#[cfg(any(test, not(target_arch = "wasm32")))]
pub(crate) struct RemoteTraceSourceGuard;

#[cfg(any(test, not(target_arch = "wasm32")))]
impl Drop for RemoteTraceSourceGuard {
    fn drop(&mut self) {
        REMOTE_SOURCE_DEPTH.with(|depth| depth.set(depth.get().saturating_sub(1)));
    }
}

#[cfg(any(test, not(target_arch = "wasm32")))]
pub(crate) fn enter_remote_trace_source() -> RemoteTraceSourceGuard {
    REMOTE_SOURCE_DEPTH.with(|depth| depth.set(depth.get().saturating_add(1)));
    RemoteTraceSourceGuard
}

fn remote_trace_source_active() -> bool {
    REMOTE_SOURCE_DEPTH.with(|depth| depth.get() != 0)
}

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
#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn io_span_stash(conn_id: c_int, ctx: HewTraceContext) {
    IO_SPAN_TABLE.lock_or_recover().insert(conn_id, ctx);
}

/// Evict any stashed context for `conn_id` on connection close.
///
/// Safe to call even when no entry exists (eviction is idempotent).
#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn io_span_evict(conn_id: c_int) {
    IO_SPAN_TABLE.lock_or_recover().remove(&conn_id);
}

#[cfg(not(target_arch = "wasm32"))]
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

fn new_stream_id() -> TraceStreamId {
    #[cfg(not(target_arch = "wasm32"))]
    {
        use rand::RngExt as _;
        let mut rng = rand::rng();
        let mut id = TraceStreamId {
            hi: rng.random(),
            lo: rng.random(),
        };
        if id.hi == 0 && id.lo == 0 {
            id.lo = 1;
        }
        id
    }
    #[cfg(target_arch = "wasm32")]
    {
        // The native Observe server is unavailable on wasm32. Keep the internal
        // tracing module buildable there without adding a browser entropy ABI.
        TraceStreamId {
            hi: next_random_id(),
            lo: next_random_id(),
        }
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

fn sample_clock_anchor() -> TraceClockAnchor {
    use std::time::{SystemTime, UNIX_EPOCH};

    // Bracket the wall-clock read with monotonic samples. The midpoint is our
    // best estimate of when the wall read occurred, and half the bracket width
    // is the honest correlation uncertainty.
    let monotonic_before = monotonic_ns();
    let unix_epoch_ns = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos();
    let monotonic_after = monotonic_ns();
    let uncertainty_ns = monotonic_after.saturating_sub(monotonic_before).div_ceil(2);
    let monotonic_ns = monotonic_before.saturating_add(uncertainty_ns);
    TraceClockAnchor {
        monotonic_ns,
        #[expect(
            clippy::cast_possible_truncation,
            reason = "Unix epoch ns fits in u64 until the year 2554"
        )]
        unix_epoch_ns: unix_epoch_ns as u64,
        uncertainty_ns,
    }
}

// ── Rust-internal drain ────────────────────────────────────────────────

/// Read a bounded, non-destructive snapshot after an exclusive sequence cursor.
///
/// With no cursor, reading begins at the oldest retained event. A stale cursor
/// likewise begins at the oldest retained event but reports the exact missing
/// count. `limit` is defensively capped at the Observe wire maximum of 256.
///
/// # Errors
///
/// Returns [`TraceSnapshotError::StreamMismatch`] for a cursor from another
/// stream, or [`TraceSnapshotError::FutureCursor`] when `after` is beyond the
/// largest sequence produced in the current stream.
pub fn trace_snapshot(
    expected_stream_id: Option<TraceStreamId>,
    after: Option<u64>,
    limit: usize,
) -> Result<TraceSnapshot, TraceSnapshotError> {
    trace_snapshot_inner(expected_stream_id, after, limit.min(256))
}

#[cfg(all(feature = "profiler", not(target_arch = "wasm32")))]
pub(crate) fn trace_stream_is_current(stream_id: TraceStreamId) -> bool {
    TRACE_EVENTS.access(|store| store.stream_id == stream_id)
}

fn trace_snapshot_inner(
    expected_stream_id: Option<TraceStreamId>,
    after: Option<u64>,
    limit: usize,
) -> Result<TraceSnapshot, TraceSnapshotError> {
    TRACE_EVENTS.access(|store| {
        if let Some(requested) = expected_stream_id {
            if requested != store.stream_id {
                return Err(TraceSnapshotError::StreamMismatch {
                    producer_instance_id: *PRODUCER_INSTANCE_ID,
                    requested,
                    current: store.stream_id,
                });
            }
        }

        let last_produced = store.next_sequence.saturating_sub(1);
        if let Some(after) = after {
            if after > last_produced {
                return Err(TraceSnapshotError::FutureCursor {
                    after,
                    next_sequence: store.next_sequence,
                });
            }
        }

        let oldest_sequence = store
            .events
            .front()
            .map_or(store.next_sequence, |record| record.sequence);
        let requested_next = after.map_or(oldest_sequence, |value| value.saturating_add(1));
        let overrun_count = oldest_sequence.saturating_sub(requested_next);
        let first_sequence = requested_next.max(oldest_sequence);
        let events: Vec<_> = store
            .events
            .iter()
            .skip_while(|record| record.sequence < first_sequence)
            .take(limit)
            .cloned()
            .collect();
        let next_cursor = events.last().map_or_else(
            || after.unwrap_or(store.next_sequence.saturating_sub(1)),
            |record| record.sequence,
        );
        let has_more = next_cursor < last_produced;
        let enabled = TRACING_ENABLED.load(Ordering::Acquire);

        Ok(TraceSnapshot {
            producer_instance_id: *PRODUCER_INSTANCE_ID,
            stream_id: store.stream_id,
            enabled,
            oldest_sequence,
            next_sequence: store.next_sequence,
            next_cursor,
            has_more,
            overrun_count,
            events_recorded_total: store.events_recorded_total,
            events_overwritten_total: store.events_overwritten_total,
            events_sampled_out_total: 0,
            sampling_mode: if enabled { "all" } else { "off" },
            batch_source_monotonic_ns: monotonic_ns(),
            clock_anchor: store.clock_anchor,
            events,
        })
    })
}

#[derive(Debug)]
pub(crate) struct TraceConsumerBatch {
    pub events: Vec<HewTraceEvent>,
    #[cfg(feature = "otel")]
    pub discontinuity: bool,
}

fn read_internal_consumer(
    cursor: &PoisonSafe<Option<InternalCursor>>,
    max: usize,
) -> TraceConsumerBatch {
    if max == 0 {
        return TraceConsumerBatch {
            events: Vec::new(),
            #[cfg(feature = "otel")]
            discontinuity: false,
        };
    }
    cursor.access(|saved| {
        #[cfg(feature = "otel")]
        let mut discontinuity = false;
        let snapshot = match *saved {
            Some(current) => {
                match trace_snapshot_inner(Some(current.stream_id), Some(current.after), max) {
                    Ok(snapshot) => snapshot,
                    Err(
                        TraceSnapshotError::StreamMismatch { .. }
                        | TraceSnapshotError::FutureCursor { .. },
                    ) => {
                        #[cfg(feature = "otel")]
                        {
                            discontinuity = true;
                        }
                        trace_snapshot_inner(None, None, max)
                            .expect("cursor-less snapshot cannot fail")
                    }
                }
            }
            None => {
                trace_snapshot_inner(None, None, max).expect("cursor-less snapshot cannot fail")
            }
        };
        #[cfg(feature = "otel")]
        {
            discontinuity |= snapshot.overrun_count != 0;
        }
        *saved = Some(InternalCursor {
            stream_id: snapshot.stream_id,
            after: snapshot.next_cursor,
        });
        TraceConsumerBatch {
            events: snapshot
                .events
                .into_iter()
                .map(|record| record.event)
                .collect(),
            #[cfg(feature = "otel")]
            discontinuity,
        }
    })
}

/// Read events through the legacy Rust-internal consumer cursor without
/// removing records needed by other observers.
#[must_use]
pub fn drain_events(max: usize) -> Vec<HewTraceEvent> {
    read_internal_consumer(&RUST_DRAIN_CURSOR, max).events
}

/// Read through `OTel`'s independent cursor.
#[cfg(feature = "otel")]
pub(crate) fn read_otel_events(max: usize) -> TraceConsumerBatch {
    read_internal_consumer(&OTEL_CURSOR, max)
}

/// Read through deterministic test reporting's independent cursor.
pub(crate) fn read_test_report_events(max: usize) -> TraceConsumerBatch {
    read_internal_consumer(&TEST_REPORT_CURSOR, max)
}

// ── Recording ──────────────────────────────────────────────────────────

/// Record a trace event if tracing is enabled.
fn recording_generation() -> Option<u64> {
    TRACING_ENABLED
        .load(Ordering::Acquire)
        .then(|| STREAM_GENERATION.load(Ordering::Acquire))
}

#[expect(
    clippy::too_many_arguments,
    reason = "hot-path scalar metadata avoids allocation and preserves the C ABI projection"
)]
fn record_event_with_attribution(
    generation: u64,
    event: HewTraceEvent,
    subject_kind: Option<&'static str>,
    subject_id: Option<u64>,
    subject_actor_id: Option<u64>,
    subject_spawn_serial: Option<u64>,
    subject_dispatch_ptr: usize,
    source_actor_id: Option<u64>,
    source_spawn_serial: Option<u64>,
    destination_actor_id: Option<u64>,
    destination_spawn_serial: Option<u64>,
    source_dispatch_ptr: usize,
    destination_dispatch_ptr: usize,
    source_attribution: TraceActorAttribution,
    destination_attribution: TraceActorAttribution,
    reason: Option<&str>,
    rc_sweep: Option<hew_observe_protocol::RcSweep>,
) {
    #[cfg(test)]
    if let Some(barrier) = RECORD_PRE_LOCK_BARRIER.access(|slot| slot.clone()) {
        barrier.arrived.wait();
        barrier.release.wait();
    }
    TRACE_EVENTS.access(|store| {
        // Serialize the second enabled check with every stream transition. An
        // emitter that observed the old epoch cannot append after reset/clear.
        if !TRACING_ENABLED.load(Ordering::Acquire) || store.generation != generation {
            return;
        }
        if store.next_sequence == u64::MAX {
            // The sequence cursor cannot represent one-past-u64::MAX. Rotate
            // atomically at commit time and make this event sequence 1 of the
            // replacement stream. Unlike a user reset, no other thread can
            // observe a boundary between the rotation and this append because
            // both happen while holding TRACE_EVENTS.
            store.begin_new_stream();
        }
        let sequence = store.next_sequence;
        store.next_sequence += 1;
        store.events_recorded_total += 1;
        while store.events.len() >= MAX_TRACE_EVENTS {
            store.events.pop_front();
            store.events_overwritten_total += 1;
        }
        store.events.push_back(TraceEventRecord {
            sequence,
            event,
            subject_kind,
            subject_id,
            subject_actor_id,
            subject_spawn_serial,
            subject_dispatch_ptr,
            source_actor_id,
            source_attribution,
            source_spawn_serial,
            destination_actor_id,
            destination_attribution,
            destination_spawn_serial,
            source_dispatch_ptr,
            destination_dispatch_ptr,
            reason: reason.map(std::sync::Arc::from),
            rc_sweep,
        });
    });
}

fn record_event_for_generation(generation: u64, event: HewTraceEvent) {
    let subject_kind = match event.event_type {
        SPAN_IO_ACCEPT | SPAN_IO_RECV | SPAN_SEND => None,
        SPAN_DUPLEX_CREATED
        | SPAN_DUPLEX_HALF_SPLIT
        | SPAN_DUPLEX_CLOSED
        | SPAN_SINK_CLOSED
        | SPAN_STREAM_CLOSED => Some("channel"),
        _ if event.actor_id != 0 => Some("actor"),
        _ => None,
    };
    let subject_id = subject_kind.map(|_| event.actor_id);
    let subject_actor_id = (subject_kind == Some("actor")).then_some(event.actor_id);
    #[cfg(all(feature = "profiler", not(target_arch = "wasm32")))]
    let identity =
        subject_actor_id.and_then(crate::profiler::actor_registry::capture_actor_identity);
    #[cfg(all(feature = "profiler", not(target_arch = "wasm32")))]
    let (spawn_serial, dispatch) = identity.map_or((None, 0), |(identity, dispatch)| {
        (identity.spawn_serial, dispatch)
    });
    #[cfg(any(not(feature = "profiler"), target_arch = "wasm32"))]
    let (spawn_serial, dispatch) = (None, 0);
    record_event_with_attribution(
        generation,
        event,
        subject_kind,
        subject_id,
        subject_actor_id,
        spawn_serial,
        dispatch,
        None,
        None,
        None,
        None,
        0,
        0,
        TraceActorAttribution::NotApplicable,
        TraceActorAttribution::NotApplicable,
        None,
        None,
    );
}

#[cfg(test)]
fn record_event(event: HewTraceEvent) {
    if let Some(generation) = recording_generation() {
        record_event_for_generation(generation, event);
    }
}

fn record_lifecycle_event(actor_id: u64, event_type: i32, msg_type: i32) {
    let Some(generation) = recording_generation() else {
        return;
    };
    let Some(ctx) = trace_context() else {
        return;
    };

    record_event_for_generation(
        generation,
        HewTraceEvent {
            trace_id_hi: ctx.trace_id_hi,
            trace_id_lo: ctx.trace_id_lo,
            span_id: ctx.span_id,
            parent_span_id: ctx.parent_span_id,
            actor_id,
            event_type,
            msg_type,
            timestamp_ns: monotonic_ns(),
        },
    );
}

/// Disclose survivor reclamation before terminal actor completion.
pub(crate) fn record_rc_survivors(
    actor: &crate::actor::HewActor,
    summary: hew_observe_protocol::RcSweep,
) {
    if summary.count == 0 {
        return;
    }
    let Some(generation) = recording_generation() else {
        return;
    };
    let ctx = system_context();
    record_event_with_attribution(
        generation,
        HewTraceEvent {
            trace_id_hi: ctx.trace_id_hi,
            trace_id_lo: ctx.trace_id_lo,
            span_id: ctx.span_id,
            parent_span_id: ctx.parent_span_id,
            actor_id: actor.id,
            event_type: SPAN_RC_SWEEP,
            msg_type: 0,
            timestamp_ns: monotonic_ns(),
        },
        Some("actor"),
        Some(actor.id),
        Some(actor.id),
        Some(actor.spawn_serial),
        actor.dispatch.map_or(0, |dispatch| dispatch as usize),
        None,
        None,
        None,
        None,
        0,
        0,
        TraceActorAttribution::NotApplicable,
        TraceActorAttribution::NotApplicable,
        None,
        Some(summary),
    );
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

pub(crate) fn record_delivery_refused(
    context: &crate::execution_context::HewExecutionContext,
    message: i32,
    reason: &str,
) {
    let Some(generation) = recording_generation() else {
        return;
    };
    // SAFETY: the installed dispatch context retains its actor during this call.
    let actor = unsafe { context.actor.as_ref() };
    let trace = context.trace;
    record_event_with_attribution(
        generation,
        HewTraceEvent {
            trace_id_hi: trace.trace_id_hi,
            trace_id_lo: trace.trace_id_lo,
            span_id: trace.span_id,
            parent_span_id: trace.parent_span_id,
            actor_id: context.actor_id,
            event_type: SPAN_DELIVERY_REFUSED,
            msg_type: message,
            timestamp_ns: monotonic_ns(),
        },
        Some("actor"),
        Some(context.actor_id),
        Some(context.actor_id),
        actor.map(|actor| actor.spawn_serial),
        actor
            .and_then(|actor| actor.dispatch)
            .map_or(0, |dispatch| dispatch as usize),
        None,
        None,
        None,
        None,
        0,
        0,
        TraceActorAttribution::NotApplicable,
        TraceActorAttribution::NotApplicable,
        Some(reason),
        None,
    );
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
    let Some(generation) = recording_generation() else {
        return;
    };
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

    record_event_for_generation(
        generation,
        HewTraceEvent {
            trace_id_hi: ctx.trace_id_hi,
            trace_id_lo: ctx.trace_id_lo,
            span_id: ctx.span_id,
            parent_span_id: ctx.parent_span_id,
            actor_id,
            event_type: SPAN_BEGIN,
            msg_type,
            timestamp_ns: monotonic_ns(),
        },
    );
}

/// End the current span and record a `SPAN_END` event.
#[no_mangle]
pub extern "C" fn hew_trace_end(actor_id: u64, msg_type: i32) {
    let Some(generation) = recording_generation() else {
        return;
    };
    let Some(ctx) = trace_context() else {
        return;
    };

    record_event_for_generation(
        generation,
        HewTraceEvent {
            trace_id_hi: ctx.trace_id_hi,
            trace_id_lo: ctx.trace_id_lo,
            span_id: ctx.span_id,
            parent_span_id: ctx.parent_span_id,
            actor_id,
            event_type: SPAN_END,
            msg_type,
            timestamp_ns: monotonic_ns(),
        },
    );

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
    if !TRACING_ENABLED.load(Ordering::Acquire) {
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
    if !TRACING_ENABLED.load(Ordering::Acquire) {
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
// KEEP(wasm32): three call sites in supervisor.rs, plus the
// supervisor_trace_export_e2e integration test. Same cfg asymmetry as
// `record_supervisor_event`: the supervisor module is native-only, this module
// is not.
pub(crate) fn ensure_supervisor_trace_root() {
    if !TRACING_ENABLED.load(Ordering::Acquire) {
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

pub(crate) fn record_send(
    source_actor_id: Option<u64>,
    source_spawn_serial: Option<u64>,
    source_dispatch_ptr: usize,
    destination_actor_id: u64,
    destination_spawn_serial: u64,
    destination_dispatch_ptr: usize,
    msg_type: i32,
) {
    let Some(generation) = recording_generation() else {
        return;
    };
    // An external producer has no installed actor trace context. Preserve that
    // truth explicitly instead of inventing a source actor or dropping the
    // send event entirely.
    let installed_context = trace_context();
    let source_attribution = if source_actor_id.is_some() {
        TraceActorAttribution::LocalActor
    } else if remote_trace_source_active() {
        TraceActorAttribution::RemoteUnknown
    } else {
        TraceActorAttribution::External
    };
    let ctx = installed_context.unwrap_or_default();
    record_event_with_attribution(
        generation,
        HewTraceEvent {
            trace_id_hi: ctx.trace_id_hi,
            trace_id_lo: ctx.trace_id_lo,
            span_id: ctx.span_id,
            parent_span_id: ctx.parent_span_id,
            // Kept solely as the stable C ABI projection. Observe uses the
            // explicit endpoint fields above and never interprets this value.
            actor_id: source_actor_id.unwrap_or(destination_actor_id),
            event_type: SPAN_SEND,
            msg_type,
            timestamp_ns: monotonic_ns(),
        },
        None,
        None,
        None,
        None,
        0,
        source_actor_id,
        source_spawn_serial,
        Some(destination_actor_id),
        Some(destination_spawn_serial),
        source_dispatch_ptr,
        destination_dispatch_ptr,
        source_attribution,
        TraceActorAttribution::LocalActor,
        None,
        None,
    );
}

/// Record a channel lifecycle event (created, split, closed).
///
/// `handle_addr` is the raw pointer address of the relevant handle cast to `u64`.
/// It serves as the handle identity — there are no sequential IDs on channel substrate types.
///
/// This is the emission point for `SPAN_DUPLEX_*`, `SPAN_SINK_CLOSED` and
/// `SPAN_STREAM_CLOSED`.
pub(crate) fn record_channel_event(handle_addr: u64, event_type: i32) {
    let Some(generation) = recording_generation() else {
        return;
    };
    record_event_for_generation(
        generation,
        HewTraceEvent {
            trace_id_hi: 0,
            trace_id_lo: 0,
            span_id: 0,
            parent_span_id: 0,
            actor_id: handle_addr,
            event_type,
            msg_type: 0,
            timestamp_ns: monotonic_ns(),
        },
    );
}

/// Record a single I/O-level event using an explicit `HewTraceContext`.
///
/// Used for `SPAN_IO_ACCEPT` and `SPAN_IO_RECV` which are not bound to any
/// actor ID (`actor_id` = 0) and carry their own freshly generated span context.
#[cfg(not(target_arch = "wasm32"))]
fn record_io_event(ctx: &HewTraceContext, event_type: i32) {
    let Some(generation) = recording_generation() else {
        return;
    };
    record_event_for_generation(
        generation,
        HewTraceEvent {
            trace_id_hi: ctx.trace_id_hi,
            trace_id_lo: ctx.trace_id_lo,
            span_id: ctx.span_id,
            parent_span_id: ctx.parent_span_id,
            actor_id: 0,
            event_type,
            msg_type: 0,
            timestamp_ns: monotonic_ns(),
        },
    );
}

/// Called at `hew_connmgr_add` when a TCP connection is accepted.
///
/// Creates a new root I/O span, emits `SPAN_IO_ACCEPT` begin and end events
/// (accept is synchronous — the span collapses to an instant), then stashes
/// the context by `conn_id` so `reader_loop` can parent per-envelope
/// `SPAN_IO_RECV` spans under this accept span.
///
/// Fast path: returns immediately (no allocation) when tracing is disabled.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn io_accept_span_begin(conn_id: c_int) {
    if !TRACING_ENABLED.load(Ordering::Acquire) {
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
#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn io_recv_span_begin(conn_id: c_int) -> Option<HewTraceContext> {
    if !TRACING_ENABLED.load(Ordering::Acquire) {
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
#[cfg(not(target_arch = "wasm32"))]
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
    let enable = enable != 0;
    TRACE_EVENTS.access(|store| {
        let was_enabled = TRACING_ENABLED.load(Ordering::Acquire);
        if enable && !was_enabled {
            store.begin_new_stream();
        }
        TRACING_ENABLED.store(enable, Ordering::Release);
    });
}

/// Check if tracing is enabled. Returns 1 if enabled, 0 if not.
#[no_mangle]
pub extern "C" fn hew_trace_is_enabled() -> c_int {
    c_int::from(TRACING_ENABLED.load(Ordering::Acquire))
}

/// Return the number of recorded trace events.
#[no_mangle]
pub extern "C" fn hew_trace_event_count() -> u64 {
    C_DRAIN_CURSOR.access(|cursor| {
        TRACE_EVENTS.access(|store| {
            let after = cursor
                .filter(|cursor| cursor.stream_id == store.stream_id)
                .map_or(0, |cursor| cursor.after);
            store
                .events
                .iter()
                .filter(|record| record.sequence > after)
                .count() as u64
        })
    })
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
    let batch = read_internal_consumer(&C_DRAIN_CURSOR, max_count as usize);
    for (index, event) in batch.events.iter().copied().enumerate() {
        // SAFETY: out has space for at least max_count events and batch length
        // is bounded by max_count.
        unsafe { *out.add(index) = event };
    }
    #[expect(
        clippy::cast_possible_truncation,
        reason = "batch length is bounded by max_count which is u32"
    )]
    {
        batch.events.len() as u32
    }
}

/// Clear all recorded trace events.
#[no_mangle]
pub extern "C" fn hew_trace_clear() {
    TRACE_EVENTS.access(TraceStore::begin_new_stream);
}

/// Register `hew_trace_reset` as a session reset hook.
///
/// Safe to call multiple times; the registration is guarded by a `Once` so
/// the hook is added to the registry exactly once per process lifetime.
/// Called from `scheduler::hew_sched_init` so trace events are cleared on
/// every `session_reset()`.
#[cfg(not(target_arch = "wasm32"))]
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
    TRACE_EVENTS.access(|store| {
        TRACING_ENABLED.store(false, Ordering::Release);
        store.begin_new_stream();
    });
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
///   "handler_name": "ActorType.handler_name"|null
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
#[must_use]
pub fn drain_events_json() -> String {
    use std::fmt::Write as _;

    let events = read_internal_consumer(&LEGACY_JSON_CURSOR, 256).events;
    let mut json = String::from("[");
    for (i, ev) in events.into_iter().enumerate() {
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
            // Owned variant: the name is copied under DISPATCH_TYPE_REGISTRY's
            // lock, so a concurrent `clear_dispatch_registry` cannot free the
            // leaked backing allocation between lookup and copy (the borrowing
            // variant releases the lock before returning — a TOCTOU UAF).
            let type_name =
                crate::profiler::actor_registry::lookup_dispatch_type_by_ptr_owned(dispatch_ptr);
            let hname =
                crate::profiler::actor_registry::handler_name_by_ptr(dispatch_ptr, ev.msg_type);
            let actor_type = if type_name == "Actor" && type_id == 0 {
                None
            } else {
                Some(type_name)
            };
            (type_id, actor_type, hname)
        };

        let mut actor_type_json = String::new();
        if let Some(actor_type_str) = actor_type_str.filter(|_| actor_type_id != 0) {
            crate::util::push_json_string(&mut actor_type_json, &actor_type_str);
        } else {
            actor_type_json.push_str("null");
        }
        let mut handler_name_json = String::new();
        if let Some(name) = &handler_name {
            crate::util::push_json_string(&mut handler_name_json, name);
        } else {
            handler_name_json.push_str("null");
        }
        let _ = write!(
            json,
            r#"{{"trace_id":"{:016x}{:016x}","span_id":"{}","parent_span_id":"{}","actor_id":"{}","actor_type_id":"{}","actor_type":{},"event_type":"{}","msg_type":{},"timestamp_ns":"{}","handler_name":{}}}"#,
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
    json.push(']');
    json
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
            SPAN_SUPERVISOR_RESTART,
            SPAN_SUPERVISOR_ESCALATE,
            SPAN_SUPERVISOR_CIRCUIT_OPEN,
            SPAN_SUPERVISOR_CIRCUIT_HALF_OPEN,
            SPAN_SUPERVISOR_CIRCUIT_CLOSE,
            SPAN_SUPERVISOR_MAX_RESTARTS,
            SPAN_SUPERVISOR_BACKOFF,
            SPAN_DELIVERY_REFUSED,
            SPAN_RC_SWEEP,
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
        assert!(event_type_name(SPAN_RC_SWEEP + 1).is_none());
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
        assert_eq!(restart["actor_id"], "7");
        assert_ne!(
            restart["span_id"]
                .as_str()
                .and_then(|value| value.parse::<u64>().ok())
                .unwrap(),
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

        // The clock reads zero at the epoch instant itself; move past it so a
        // populated timestamp is distinguishable from an unset one.
        while monotonic_ns() == 0 {
            std::hint::spin_loop();
        }
        let before = monotonic_ns();
        let fake_addr: u64 = 0xDEAD_BEEF_0000_0001;
        record_channel_event(fake_addr, SPAN_DUPLEX_CREATED);

        assert_eq!(hew_trace_event_count(), 1);
        let events = drain_events(1);
        assert_eq!(events[0].event_type, SPAN_DUPLEX_CREATED);
        assert_eq!(events[0].actor_id, fake_addr);
        assert!(
            events[0].timestamp_ns >= before,
            "timestamp must be populated from the clock"
        );
    }

    /// `drain_events_json` renders every channel `event_type` string
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
            let actor_id = parsed[0]["actor_id"]
                .as_str()
                .and_then(|value| value.parse::<u64>().ok())
                .unwrap_or_else(|| panic!("actor_id must be a decimal u64 for {expected_str}"));
            assert_ne!(actor_id, 0, "actor_id must be non-zero for {expected_str}");
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

    #[test]
    fn independent_observers_receive_identical_sequences() {
        let _guard = setup();
        for actor_id in 1..=4 {
            hew_trace_lifecycle(actor_id, SPAN_SPAWN);
        }

        let first = trace_snapshot(None, None, 256).expect("initial snapshot");
        let second = trace_snapshot(None, None, 256).expect("second observer snapshot");
        let first_sequences: Vec<_> = first.events.iter().map(|event| event.sequence).collect();
        let second_sequences: Vec<_> = second.events.iter().map(|event| event.sequence).collect();
        assert_eq!(first_sequences, vec![1, 2, 3, 4]);
        assert_eq!(first_sequences, second_sequences);

        let mut c_event = HewTraceEvent {
            trace_id_hi: 0,
            trace_id_lo: 0,
            span_id: 0,
            parent_span_id: 0,
            actor_id: 0,
            event_type: 0,
            msg_type: 0,
            timestamp_ns: 0,
        };
        // SAFETY: one-element output buffer.
        assert_eq!(unsafe { hew_trace_drain(&raw mut c_event, 1) }, 1);
        let after_c = trace_snapshot(None, None, 256).expect("HTTP ring remains intact");
        assert_eq!(
            after_c.events.len(),
            4,
            "C consumer must not remove records"
        );
    }

    #[cfg(feature = "otel")]
    #[test]
    fn internal_consumers_do_not_steal_each_others_records() {
        let _guard = setup();
        for actor_id in 1..=3 {
            hew_trace_lifecycle(actor_id, SPAN_SPAWN);
        }

        let otel_first = read_otel_events(2);
        assert_eq!(otel_first.events.len(), 2);

        let report = read_test_report_events(3);
        assert_eq!(report.events.len(), 3);
        let legacy_rust = drain_events(3);
        assert_eq!(legacy_rust.len(), 3);
        let http = trace_snapshot(None, None, 3).expect("HTTP observer snapshot");
        assert_eq!(http.events.len(), 3);

        let otel_second = read_otel_events(2);
        assert_eq!(otel_second.events.len(), 1);
        assert_eq!(otel_second.events[0].actor_id, 3);
    }

    #[test]
    fn stale_cursor_reports_exact_overrun() {
        let _guard = setup();
        for actor_id in 0..(MAX_TRACE_EVENTS as u64 + 5) {
            hew_trace_lifecycle(actor_id + 1, SPAN_SPAWN);
        }

        let snapshot = trace_snapshot(None, Some(0), 1).expect("stale cursor snapshot");
        assert_eq!(snapshot.oldest_sequence, 6);
        assert_eq!(snapshot.next_sequence, MAX_TRACE_EVENTS as u64 + 6);
        assert_eq!(snapshot.overrun_count, 5);
        assert_eq!(snapshot.events_overwritten_total, 5);
        assert_eq!(snapshot.events_recorded_total, MAX_TRACE_EVENTS as u64 + 5);
        assert_eq!(snapshot.events[0].sequence, 6);
        assert!(snapshot.has_more);
    }

    #[test]
    fn reset_and_reenable_rotate_stream_but_not_producer() {
        let _guard = setup();
        let initial = trace_snapshot(None, None, 1).expect("initial snapshot");

        hew_trace_clear();
        let cleared = trace_snapshot(None, None, 1).expect("cleared snapshot");
        assert_eq!(cleared.producer_instance_id, initial.producer_instance_id);
        assert_ne!(cleared.stream_id, initial.stream_id);
        assert_eq!(cleared.next_sequence, 1);

        hew_trace_enable(0);
        hew_trace_enable(1);
        let reenabled = trace_snapshot(None, None, 1).expect("re-enabled snapshot");
        assert_eq!(reenabled.producer_instance_id, initial.producer_instance_id);
        assert_ne!(reenabled.stream_id, cleared.stream_id);
    }

    #[test]
    fn enable_publishes_replacement_generation_before_recording() {
        let _guard = tracing_test_guard();
        hew_trace_reset();

        for actor_id in 1..=256_u64 {
            hew_trace_enable(1);
            let emitter = std::thread::spawn(move || {
                record_event(HewTraceEvent {
                    trace_id_hi: 1,
                    trace_id_lo: actor_id,
                    span_id: actor_id,
                    parent_span_id: 0,
                    actor_id,
                    event_type: SPAN_SPAWN,
                    msg_type: 0,
                    timestamp_ns: monotonic_ns(),
                });
            });
            emitter.join().expect("post-enable emitter");

            let snapshot = trace_snapshot(None, None, 2).expect("post-enable snapshot");
            assert_eq!(snapshot.events.len(), 1);
            assert_eq!(snapshot.events[0].sequence, 1);
            assert_eq!(snapshot.events[0].event.actor_id, actor_id);

            hew_trace_enable(0);
        }
        hew_trace_reset();
    }

    #[test]
    fn sequence_exhaustion_rotates_before_cursor_overflow() {
        let _guard = setup();
        let before = trace_snapshot(None, None, 1).expect("initial stream");
        TRACE_EVENTS.access(|store| {
            store.next_sequence = u64::MAX;
            store.events_recorded_total = u64::MAX - 1;
        });

        hew_trace_lifecycle(77, SPAN_SPAWN);

        let after = trace_snapshot(None, None, 2).expect("replacement stream");
        assert_ne!(after.stream_id, before.stream_id);
        assert_eq!(after.oldest_sequence, 1);
        assert_eq!(after.next_sequence, 2);
        assert_eq!(after.next_cursor, 1);
        assert_eq!(after.events_recorded_total, 1);
        assert_eq!(after.events.len(), 1);
        assert_eq!(after.events[0].sequence, 1);
        assert_eq!(after.events[0].event.actor_id, 77);
    }

    #[test]
    fn send_records_honest_local_endpoints() {
        let _guard = setup();
        record_send(Some(11), Some(111), 0xaaaa, 22, 222, 0xbbbb, 7);

        let snapshot = trace_snapshot(None, None, 1).expect("send snapshot");
        let record = &snapshot.events[0];
        assert_eq!(record.subject_kind, None);
        assert_eq!(record.source_attribution, TraceActorAttribution::LocalActor);
        assert_eq!(record.source_actor_id, Some(11));
        assert_eq!(record.source_spawn_serial, Some(111));
        assert_eq!(record.source_dispatch_ptr, 0xaaaa);
        assert_eq!(record.destination_actor_id, Some(22));
        assert_eq!(
            record.destination_attribution,
            TraceActorAttribution::LocalActor
        );
        assert_eq!(record.destination_spawn_serial, Some(222));
        assert_eq!(record.destination_dispatch_ptr, 0xbbbb);
        assert_eq!(
            record.event.actor_id, 11,
            "legacy C projection remains source"
        );
    }

    #[test]
    fn external_send_has_unknown_source_and_real_destination() {
        let _guard = tracing_test_guard();
        hew_trace_reset();
        hew_trace_enable(1);
        record_send(None, None, 0, 22, 222, 0xbbbb, 7);

        let record = &trace_snapshot(None, None, 1)
            .expect("external send snapshot")
            .events[0];
        assert_eq!(record.source_actor_id, None);
        assert_eq!(record.source_attribution, TraceActorAttribution::External);
        assert_eq!(record.destination_actor_id, Some(22));
        assert_eq!(record.destination_spawn_serial, Some(222));
        assert_eq!(
            record.event.actor_id, 22,
            "legacy C projection remains useful"
        );
    }

    #[test]
    fn remote_source_marker_without_identity_is_explicitly_unknown() {
        let _guard = setup();
        let _remote_source = enter_remote_trace_source();
        record_send(None, None, 0, 22, 222, 0xbbbb, 7);

        let record = &trace_snapshot(None, None, 1)
            .expect("remote send snapshot")
            .events[0];
        assert_eq!(record.source_actor_id, None);
        assert_eq!(
            record.source_attribution,
            TraceActorAttribution::RemoteUnknown
        );
        assert_eq!(record.destination_actor_id, Some(22));
    }

    #[test]
    fn trace_context_alone_does_not_fabricate_remote_source() {
        let _guard = setup();
        record_send(None, None, 0, 22, 222, 0xbbbb, 7);

        let record = &trace_snapshot(None, None, 1)
            .expect("context-only send snapshot")
            .events[0];
        assert_eq!(record.source_actor_id, None);
        assert_eq!(record.source_attribution, TraceActorAttribution::External);
    }

    #[test]
    fn reset_wins_against_emitter_paused_before_store_lock() {
        let _guard = setup();
        let arrived = std::sync::Arc::new(std::sync::Barrier::new(2));
        let release = std::sync::Arc::new(std::sync::Barrier::new(2));
        RECORD_PRE_LOCK_BARRIER.access(|slot| {
            *slot = Some(RecordPreLockBarrier {
                arrived: arrived.clone(),
                release: release.clone(),
            });
        });

        let emitter = std::thread::spawn(|| {
            record_event(HewTraceEvent {
                trace_id_hi: 1,
                trace_id_lo: 2,
                span_id: 3,
                parent_span_id: 0,
                actor_id: 44,
                event_type: SPAN_SPAWN,
                msg_type: 0,
                timestamp_ns: monotonic_ns(),
            });
        });
        arrived.wait();
        hew_trace_reset();
        release.wait();
        emitter.join().expect("emitter thread");
        RECORD_PRE_LOCK_BARRIER.access(|slot| *slot = None);

        let snapshot = trace_snapshot(None, None, 1).expect("post-reset snapshot");
        assert!(snapshot.events.is_empty());
        assert_eq!(snapshot.next_sequence, 1);
    }

    #[test]
    fn clear_wins_against_emitter_paused_before_store_lock() {
        let _guard = setup();
        let arrived = std::sync::Arc::new(std::sync::Barrier::new(2));
        let release = std::sync::Arc::new(std::sync::Barrier::new(2));
        RECORD_PRE_LOCK_BARRIER.access(|slot| {
            *slot = Some(RecordPreLockBarrier {
                arrived: arrived.clone(),
                release: release.clone(),
            });
        });

        let emitter = std::thread::spawn(|| {
            record_event(HewTraceEvent {
                trace_id_hi: 1,
                trace_id_lo: 2,
                span_id: 3,
                parent_span_id: 0,
                actor_id: 44,
                event_type: SPAN_SPAWN,
                msg_type: 0,
                timestamp_ns: monotonic_ns(),
            });
        });
        arrived.wait();
        hew_trace_clear();
        release.wait();
        emitter.join().expect("emitter thread");
        RECORD_PRE_LOCK_BARRIER.access(|slot| *slot = None);

        let snapshot = trace_snapshot(None, None, 1).expect("post-clear snapshot");
        assert!(snapshot.enabled);
        assert!(snapshot.events.is_empty());
        assert_eq!(snapshot.next_sequence, 1);
    }

    #[test]
    fn concurrent_producers_assign_strict_total_sequence() {
        let _guard = setup();
        let mut producers = Vec::new();
        for producer in 0..4_u64 {
            producers.push(std::thread::spawn(move || {
                for offset in 0..500_u64 {
                    record_event(HewTraceEvent {
                        trace_id_hi: producer,
                        trace_id_lo: offset,
                        span_id: offset + 1,
                        parent_span_id: 0,
                        actor_id: producer * 1_000 + offset + 1,
                        event_type: SPAN_SPAWN,
                        msg_type: 0,
                        timestamp_ns: monotonic_ns(),
                    });
                }
            }));
        }
        for producer in producers {
            producer.join().expect("producer thread");
        }

        let snapshot = trace_snapshot_inner(None, None, 2_000).expect("full snapshot");
        assert_eq!(snapshot.events.len(), 2_000);
        for (index, record) in snapshot.events.iter().enumerate() {
            assert_eq!(record.sequence, index as u64 + 1);
        }
    }

    #[test]
    fn stream_id_parser_requires_canonical_lowercase_hex() {
        let id = TraceStreamId {
            hi: 0x0123_4567_89ab_cdef,
            lo: 0xfedc_ba98_7654_3210,
        };
        let encoded = id.to_string();
        assert_eq!(TraceStreamId::parse_hex(&encoded), Some(id));
        assert_eq!(TraceStreamId::parse_hex(&encoded.to_uppercase()), None);
        assert_eq!(TraceStreamId::parse_hex("1234"), None);
    }
}
