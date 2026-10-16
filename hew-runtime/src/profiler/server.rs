//! Profiling HTTP server.
//!
//! Runs on a dedicated OS thread (not a Hew actor) so it remains
//! responsive even when the scheduler is overloaded.

use crate::util::{json_array, push_json_string, MutexExt};
use std::convert::Infallible;
use std::fmt::Write as _;
use std::sync::mpsc::SyncSender;
use std::sync::{Arc, Mutex};
use std::time::Duration;

pub use hew_observe_protocol::OBSERVE_SCHEMA_VERSION;
use hew_observe_protocol::{
    envelope_json_raw, ActorIdentity, TraceBatch, TraceClockAnchor, TraceEvent, TraceOverrun,
    TraceRequestError, TraceStreamMismatch, TraceSubject,
};
use http_body_util::Full;
use hyper::body::Bytes;
use hyper::server::conn::http1;
use hyper::service::service_fn;
use hyper::{Request, Response, StatusCode};
use hyper_util::rt::TokioIo;
#[cfg(unix)]
use std::os::unix::net::UnixListener as StdUnixListener;
use tokio::net::TcpListener;
#[cfg(unix)]
use tokio::net::UnixListener;

use crate::profiler::allocator;
use crate::profiler::metrics::MetricsRing;
use crate::profiler::pprof;

// ── Dashboard assets (embedded at compile time) ───────────────��─────────

const DASHBOARD_HTML: &str = include_str!("dashboard/index.html");
const DASHBOARD_JS: &str = include_str!("dashboard/dashboard.js");

// ── Profiler context ────────────���───────────────────────────────────────

/// Shared context passed to the profiler HTTP server thread.
///
/// Carries references to the metrics ring buffer and pointers to the
/// distributed runtime subsystems so the profiler can expose cluster,
/// connection, routing, and trace data over HTTP.
#[derive(Debug)]
pub struct ProfilerContext {
    pub ring: Arc<Mutex<MetricsRing>>,
    pub cluster: *mut crate::cluster::HewCluster,
    pub connmgr: *mut crate::connection::HewConnMgr,
    pub routing: *mut crate::routing::HewRoutingTable,
}

// SAFETY: The raw pointers reference heap-allocated structs that outlive
// the profiler thread. The pointed-to types use internal synchronization
// (Mutex / RwLock / atomics) for concurrent access.
unsafe impl Send for ProfilerContext {}
// SAFETY: All access through the pointers goes via functions that use
// internal locking; no unsynchronized mutation is performed.
unsafe impl Sync for ProfilerContext {}

// ── Server ───────────────────────────────────────────────���──────────────

/// Start the profiling HTTP server on a pre-bound `std::net::TcpListener`,
/// blocking the current thread.
///
/// The caller owns the bind step so startup can fail before tracing is enabled,
/// and tests can discover an ephemeral port (`bind("127.0.0.1:0")`) before any
/// client connects. The listener is set to non-blocking and converted to a
/// tokio listener inside the runtime.
pub fn run_tcp_with_listener(listener: std::net::TcpListener, ctx: Arc<ProfilerContext>) {
    run_tcp_with_listener_inner(listener, ctx, None);
}

/// Start the TCP server and report when all fallible listener/runtime setup is
/// complete. The profiler startup path waits for this signal before enabling
/// tracing, so a server that cannot actually accept connections cannot create
/// a phantom trace epoch.
pub(super) fn run_tcp_with_listener_ready(
    listener: std::net::TcpListener,
    ctx: Arc<ProfilerContext>,
    ready: SyncSender<Result<(), String>>,
) {
    run_tcp_with_listener_inner(listener, ctx, Some(ready));
}

fn run_tcp_with_listener_inner(
    listener: std::net::TcpListener,
    ctx: Arc<ProfilerContext>,
    mut ready: Option<SyncSender<Result<(), String>>>,
) {
    let Some(rt) = build_runtime() else {
        report_startup(&mut ready, Err("failed to create tokio runtime".to_owned()));
        return;
    };
    if let Err(e) = listener.set_nonblocking(true) {
        eprintln!("[hew-pprof] failed to set listener nonblocking: {e}");
        report_startup(
            &mut ready,
            Err(format!("failed to set listener nonblocking: {e}")),
        );
        return;
    }

    rt.block_on(async move {
        let tokio_listener = match TcpListener::from_std(listener) {
            Ok(l) => l,
            Err(e) => {
                eprintln!("[hew-pprof] failed to wrap std listener: {e}");
                report_startup(&mut ready, Err(format!("failed to wrap TCP listener: {e}")));
                return;
            }
        };
        report_startup(&mut ready, Ok(()));
        serve_loop(Listener::Tcp(tokio_listener), ctx).await;
    });
}

#[cfg(unix)]
/// Start the profiling HTTP server on an already bound Unix listener and
/// report when all fallible runtime/listener setup is complete.
pub(super) fn run_unix_with_listener_ready(
    listener: StdUnixListener,
    ctx: Arc<ProfilerContext>,
    ready: SyncSender<Result<(), String>>,
) {
    let mut ready = Some(ready);
    let Some(rt) = build_runtime() else {
        report_startup(&mut ready, Err("failed to create tokio runtime".to_owned()));
        return;
    };
    if let Err(e) = listener.set_nonblocking(true) {
        eprintln!("[hew-pprof] failed to set unix listener nonblocking: {e}");
        report_startup(
            &mut ready,
            Err(format!("failed to set unix listener nonblocking: {e}")),
        );
        return;
    }

    rt.block_on(async move {
        let listener = match UnixListener::from_std(listener) {
            Ok(l) => l,
            Err(e) => {
                eprintln!("[hew-pprof] failed to wrap unix listener: {e}");
                report_startup(
                    &mut ready,
                    Err(format!("failed to wrap unix listener: {e}")),
                );
                return;
            }
        };
        report_startup(&mut ready, Ok(()));
        serve_loop(Listener::Unix(listener), ctx).await;
    });
}

fn report_startup(ready: &mut Option<SyncSender<Result<(), String>>>, result: Result<(), String>) {
    if let Some(ready) = ready.take() {
        let _ = ready.send(result);
    }
}

fn build_runtime() -> Option<tokio::runtime::Runtime> {
    match tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
    {
        Ok(rt) => Some(rt),
        Err(e) => {
            eprintln!("[hew-pprof] failed to create tokio runtime: {e}");
            None
        }
    }
}

/// Listener abstraction over TCP and Unix sockets.
enum Listener {
    Tcp(TcpListener),
    #[cfg(unix)]
    Unix(UnixListener),
}

/// Accept loop that works with both TCP and Unix socket listeners.
async fn serve_loop(listener: Listener, ctx: Arc<ProfilerContext>) {
    loop {
        if super::PROFILER_SHUTDOWN.load(std::sync::atomic::Ordering::Acquire) {
            break;
        }

        // Accept with a timeout so we can check the shutdown flag.
        match &listener {
            Listener::Tcp(l) => {
                let result = tokio::select! {
                    r = l.accept() => Some(r),
                    () = tokio::time::sleep(Duration::from_millis(200)) => None,
                };
                if let Some(Ok((stream, _))) = result {
                    let io = TokioIo::new(stream);
                    spawn_connection(io, ctx.clone());
                }
            }
            #[cfg(unix)]
            Listener::Unix(l) => {
                let result = tokio::select! {
                    r = l.accept() => Some(r),
                    () = tokio::time::sleep(Duration::from_millis(200)) => None,
                };
                if let Some(Ok((stream, _))) = result {
                    let io = TokioIo::new(stream);
                    spawn_connection(io, ctx.clone());
                }
            }
        }
    }
}

/// Spawn a hyper HTTP/1.1 connection handler for an accepted stream.
fn spawn_connection<I>(io: TokioIo<I>, ctx: Arc<ProfilerContext>)
where
    I: tokio::io::AsyncRead + tokio::io::AsyncWrite + Unpin + Send + 'static,
{
    tokio::spawn(async move {
        let service = service_fn(move |req| {
            let ctx = ctx.clone();
            async move { Ok::<_, Infallible>(handle_request(&req, &ctx)) }
        });
        let _ = http1::Builder::new().serve_connection(io, service).await;
    });
}

// ── Request routing ────────────────────────────────────���────────────────

fn handle_request(
    req: &Request<hyper::body::Incoming>,
    ctx: &ProfilerContext,
) -> Response<Full<Bytes>> {
    match req.uri().path() {
        "/" => static_text_response(DASHBOARD_HTML, "text/html; charset=utf-8"),
        "/dashboard.js" => {
            static_text_response(DASHBOARD_JS, "application/javascript; charset=utf-8")
        }
        "/api/metrics" => serve_current_metrics(&ctx.ring),
        "/api/observe/scrape" => serve_observe_scrape(),
        "/api/memory" => serve_memory(),
        "/api/actors" => serve_actors(),
        "/api/metrics/history" => serve_history(&ctx.ring),
        "/api/cluster/members" => serve_cluster_members(ctx),
        "/api/connections" => serve_connections(ctx),
        "/api/routing/table" => serve_routing_table(ctx),
        "/api/traces" => serve_traces(req.uri().query()),
        "/api/supervisors" => serve_supervisors(),
        "/api/crashes" => serve_crashes(),
        "/debug/pprof/heap" => serve_pprof_heap(),
        "/debug/pprof/profile" => serve_flat_profile(),
        _ => not_found_response(),
    }
}

// ── Response helpers ────────────────��───────────────────────────────────

/// Serve a `&'static str` without copying.
fn static_text_response(body: &'static str, content_type: &str) -> Response<Full<Bytes>> {
    Response::builder()
        .status(StatusCode::OK)
        .header("Content-Type", content_type)
        .body(Full::new(Bytes::from_static(body.as_bytes())))
        .expect("valid response")
}

/// Serve an owned string as text.
fn text_response(body: String, content_type: &str) -> Response<Full<Bytes>> {
    Response::builder()
        .status(StatusCode::OK)
        .header("Content-Type", content_type)
        .body(Full::new(Bytes::from(body)))
        .expect("valid response")
}

fn json_response(body: &str) -> Response<Full<Bytes>> {
    json_response_with_status(StatusCode::OK, body)
}

fn json_response_with_status(status: StatusCode, body: &str) -> Response<Full<Bytes>> {
    Response::builder()
        .status(status)
        .header("Content-Type", "application/json; charset=utf-8")
        .header("X-Hew-Schema-Version", OBSERVE_SCHEMA_VERSION)
        .body(Full::new(Bytes::from(envelope_json_raw(body))))
        .expect("valid response")
}

fn not_found_response() -> Response<Full<Bytes>> {
    Response::builder()
        .status(StatusCode::NOT_FOUND)
        .body(Full::new(Bytes::from_static(b"404 Not Found")))
        .expect("valid response")
}

// ── API endpoints ───────────────────────────────────────────────────────

/// `GET /api/metrics` — current scheduler counters.
fn serve_current_metrics(ring: &Arc<Mutex<MetricsRing>>) -> Response<Full<Bytes>> {
    let ring_guard = ring.lock_or_recover();

    let snap = crate::profiler::metrics::MetricsSnapshot::capture(ring_guard.epoch());
    drop(ring_guard);

    json_response(&current_metrics_json(&snap))
}

/// `GET /api/observe/scrape` — current observe registry in scrape text format.
fn serve_observe_scrape() -> Response<Full<Bytes>> {
    text_response(crate::observe::scrape_text(), "text/plain; charset=utf-8")
}

fn current_metrics_json(snap: &crate::profiler::metrics::MetricsSnapshot) -> String {
    format!(
        r#"{{"timestamp_secs":"{}","tasks_spawned":"{}","tasks_completed":"{}","steals":"{}","messages_sent":"{}","messages_received":"{}","active_workers":"{}","alloc_count":"{}","dealloc_count":"{}","bytes_allocated":"{}","bytes_freed":"{}","bytes_live":"{}","peak_bytes_live":"{}","tcp_bytes_read":"{}","tcp_bytes_written":"{}","tcp_accept_count":"{}","tcp_connect_count":"{}","tcp_error_count":"{}"}}"#,
        snap.timestamp_secs,
        snap.tasks_spawned,
        snap.tasks_completed,
        snap.steals,
        snap.messages_sent,
        snap.messages_received,
        snap.active_workers,
        snap.alloc_count,
        snap.dealloc_count,
        snap.bytes_allocated,
        snap.bytes_freed,
        snap.bytes_live,
        snap.peak_bytes_live,
        snap.tcp_bytes_read,
        snap.tcp_bytes_written,
        snap.tcp_accept_count,
        snap.tcp_connect_count,
        snap.tcp_error_count,
    )
}

/// `GET /api/memory` — current allocator stats.
fn serve_memory() -> Response<Full<Bytes>> {
    let stats = allocator::snapshot();
    let memory = hew_observe_protocol::Memory {
        alloc_count: stats.alloc_count,
        dealloc_count: stats.dealloc_count,
        bytes_allocated: stats.bytes_allocated,
        bytes_freed: stats.bytes_freed,
        bytes_live: stats.bytes_live,
        peak_bytes_live: stats.peak_bytes_live,
        rc_live_by_type: Some(crate::observe::rc_live_by_type()),
    };
    json_response(&serde_json::to_string(&memory).expect("memory model serializes"))
}

/// `GET /api/metrics/history` — time-series from ring buffer.
fn serve_history(ring: &Arc<Mutex<MetricsRing>>) -> Response<Full<Bytes>> {
    let ring_guard = ring.lock_or_recover();
    let entries = ring_guard.read_all();
    drop(ring_guard);

    json_response(&history_json(&entries))
}

fn history_json(entries: &[crate::profiler::metrics::MetricsSnapshot]) -> String {
    // Build JSON array manually to avoid serde dependency for this path.
    let mut json = String::from("[");
    for (i, s) in entries.iter().enumerate() {
        if i > 0 {
            json.push(',');
        }
        let _ = write!(
            json,
            r#"{{"t":"{}","ts":"{}","tc":"{}","st":"{}","ms":"{}","mr":"{}","aw":"{}","ac":"{}","dc":"{}","ba":"{}","bf":"{}","bl":"{}","pb":"{}","tbr":"{}","tbw":"{}","tac":"{}","tcc":"{}","tec":"{}"}}"#,
            s.timestamp_secs,
            s.tasks_spawned,
            s.tasks_completed,
            s.steals,
            s.messages_sent,
            s.messages_received,
            s.active_workers,
            s.alloc_count,
            s.dealloc_count,
            s.bytes_allocated,
            s.bytes_freed,
            s.bytes_live,
            s.peak_bytes_live,
            s.tcp_bytes_read,
            s.tcp_bytes_written,
            s.tcp_accept_count,
            s.tcp_connect_count,
            s.tcp_error_count,
        );
    }
    json.push(']');

    json
}

/// `GET /debug/pprof/heap` — pprof-compatible heap profile (.pb.gz).
fn serve_pprof_heap() -> Response<Full<Bytes>> {
    let data = pprof::generate_heap_profile();
    Response::builder()
        .status(StatusCode::OK)
        .header("Content-Type", "application/octet-stream")
        .header("Content-Disposition", "attachment; filename=\"heap.pb.gz\"")
        .body(Full::new(Bytes::from(data)))
        .expect("valid response")
}

/// `GET /debug/pprof/profile` — gprof-style flat profile (text).
fn serve_flat_profile() -> Response<Full<Bytes>> {
    let body = pprof::generate_flat_profile();
    text_response(body, "text/plain; charset=utf-8")
}

/// Build the JSON array body for `/api/actors` from a snapshot slice.
///
/// Extracted for testability — `serve_actors` delegates here.
/// Uses `util::json_array` + `util::push_json_string` for all user-sourced
/// string fields so that RFC 8259 escaping (including `\b`, `\f`, and other
/// U+0000–U+001F control characters) is handled by the canonical helper.
fn actors_json(actors: &[crate::profiler::actor_registry::ActorSnapshot]) -> String {
    json_array(actors, |json, a| {
        let _ = write!(json, r#"{{"id":"{}","pid":"{}","actor_type":"#, a.id, a.pid);
        push_json_string(json, &a.actor_type);
        let _ = write!(json, r#","state":"#);
        push_json_string(json, a.state);
        let _ = write!(
            json,
            r#","msgs":"{}","time_ns":"{}","mbox_depth":"{}","mbox_hwm":"{}"}}"#,
            a.messages_processed, a.processing_time_ns, a.mailbox_depth, a.mailbox_hwm,
        );
    })
}

/// `GET /api/actors` — per-actor stats and mailbox depths.
fn serve_actors() -> Response<Full<Bytes>> {
    let actors = crate::profiler::actor_registry::snapshot_all();
    json_response(&actors_json(&actors))
}

// ── Distributed runtime endpoints ───────────────────────────────────────

/// `GET /api/cluster/members` — cluster membership snapshot.
fn serve_cluster_members(ctx: &ProfilerContext) -> Response<Full<Bytes>> {
    if ctx.cluster.is_null() {
        return json_response("[]");
    }
    // SAFETY: pointer is non-null and points to a valid HewCluster that
    // outlives the profiler thread. Internal access is mutex-protected.
    let cluster = unsafe { &*ctx.cluster };
    let json = crate::cluster::snapshot_members_json(cluster);
    json_response(&json)
}

/// `GET /api/connections` — connection snapshot.
fn serve_connections(ctx: &ProfilerContext) -> Response<Full<Bytes>> {
    if ctx.connmgr.is_null() {
        return json_response("[]");
    }
    // SAFETY: pointer is non-null and points to a valid HewConnMgr that
    // outlives the profiler thread. Internal access is mutex-protected.
    let mgr = unsafe { &*ctx.connmgr };
    let json = crate::connection::snapshot_connections_json(mgr);
    json_response(&json)
}

/// `GET /api/routing/table` — routing table snapshot.
fn serve_routing_table(ctx: &ProfilerContext) -> Response<Full<Bytes>> {
    if ctx.routing.is_null() {
        return json_response(
            r#"{"local_node_id":"unconfigured","local_route_slot":0,"session_incarnation":0,"routes":[]}"#,
        );
    }
    // SAFETY: pointer is non-null and points to a valid HewRoutingTable
    // that outlives the profiler thread. Internal access is RwLock-protected.
    let table = unsafe { &*ctx.routing };
    let json = crate::routing::snapshot_routing_json(table);
    json_response(&json)
}

const MAX_TRACE_BATCH_LIMIT: usize = 256;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct TraceQuery<'a> {
    stream_id: Option<&'a str>,
    after: Option<u64>,
    limit: usize,
}

fn is_canonical_u64(value: &str) -> bool {
    !value.is_empty()
        && value.bytes().all(|byte| byte.is_ascii_digit())
        && (value == "0" || !value.starts_with('0'))
}

fn is_canonical_stream_id(value: &str) -> bool {
    value.len() == 32
        && value
            .bytes()
            .all(|byte| byte.is_ascii_digit() || (b'a'..=b'f').contains(&byte))
}

fn parse_trace_query(query: Option<&str>) -> Result<TraceQuery<'_>, &'static str> {
    let mut parsed = TraceQuery {
        stream_id: None,
        after: None,
        limit: MAX_TRACE_BATCH_LIMIT,
    };

    let Some(query) = query.filter(|query| !query.is_empty()) else {
        return Ok(parsed);
    };

    let mut saw_after = false;
    let mut saw_limit = false;
    for field in query.split('&') {
        let Some((name, value)) = field.split_once('=') else {
            return Err("trace query parameters must use name=value");
        };
        match name {
            "stream_id" => {
                if parsed.stream_id.is_some() {
                    return Err("stream_id must not be repeated");
                }
                if !is_canonical_stream_id(value) {
                    return Err("stream_id must be 32 lowercase hexadecimal characters");
                }
                parsed.stream_id = Some(value);
            }
            "after" => {
                if saw_after {
                    return Err("after must not be repeated");
                }
                saw_after = true;
                if !is_canonical_u64(value) {
                    return Err("after must be a canonical unsigned decimal integer");
                }
                parsed.after = Some(
                    value
                        .parse()
                        .map_err(|_| "after is outside the unsigned 64-bit range")?,
                );
            }
            "limit" => {
                if saw_limit {
                    return Err("limit must not be repeated");
                }
                saw_limit = true;
                if !is_canonical_u64(value) {
                    return Err("limit must be a canonical unsigned decimal integer");
                }
                let limit: usize = value
                    .parse()
                    .map_err(|_| "limit is outside the platform integer range")?;
                if !(1..=MAX_TRACE_BATCH_LIMIT).contains(&limit) {
                    return Err("limit must be between 1 and 256");
                }
                parsed.limit = limit;
            }
            _ => return Err("unknown trace query parameter"),
        }
    }

    if parsed.stream_id.is_some() != parsed.after.is_some() {
        return Err("stream_id and after must be supplied together");
    }

    Ok(parsed)
}

fn trace_error_response(status: StatusCode, error: &str, message: &str) -> Response<Full<Bytes>> {
    let error = TraceRequestError {
        error: error.to_owned(),
        message: message.to_owned(),
    };
    let body = serde_json::to_string(&error).expect("TraceRequestError must serialize");
    json_response_with_status(status, &body)
}

fn actor_identity(
    actor_id: Option<u64>,
    spawn_serial: Option<u64>,
    dispatch_ptr: usize,
) -> Option<ActorIdentity> {
    let actor_id = actor_id.filter(|id| *id != 0)?;
    let actor_type =
        crate::profiler::actor_registry::lookup_registered_dispatch_type_by_ptr_owned(dispatch_ptr);
    Some(ActorIdentity {
        actor_id,
        spawn_serial,
        actor_type,
    })
}

fn trace_subject(record: &crate::tracing::TraceEventRecord) -> Option<TraceSubject> {
    let kind = record.subject_kind?;
    let id = record.subject_id?;
    let actor = if kind == "actor" {
        actor_identity(
            record.subject_actor_id,
            record.subject_spawn_serial,
            record.subject_dispatch_ptr,
        )
    } else {
        None
    };
    Some(TraceSubject {
        kind: kind.to_owned(),
        id,
        actor,
    })
}

fn trace_event_model(record: &crate::tracing::TraceEventRecord) -> TraceEvent {
    let event = record.event;
    let is_send = event.event_type == crate::tracing::SPAN_SEND;
    let source_actor = if is_send {
        actor_identity(
            record.source_actor_id,
            record.source_spawn_serial,
            record.source_dispatch_ptr,
        )
    } else {
        None
    };
    let destination_actor = if is_send {
        actor_identity(
            record.destination_actor_id,
            record.destination_spawn_serial,
            record.destination_dispatch_ptr,
        )
    } else {
        None
    };
    let attribution_name = |attribution| match attribution {
        crate::tracing::TraceActorAttribution::LocalActor => "local_actor",
        crate::tracing::TraceActorAttribution::External => "external",
        crate::tracing::TraceActorAttribution::RemoteUnknown => "remote_unknown",
        crate::tracing::TraceActorAttribution::NotApplicable => "not_applicable",
    };
    let attribution_dispatch_ptr = if is_send {
        record.destination_dispatch_ptr
    } else {
        record.subject_dispatch_ptr
    };
    let handler_name = if attribution_dispatch_ptr == 0 {
        None
    } else {
        crate::profiler::actor_registry::handler_name_by_ptr(
            attribution_dispatch_ptr,
            event.msg_type,
        )
    };
    let event_type = crate::tracing::event_type_name(event.event_type)
        .map_or_else(|| format!("invalid:{}", event.event_type), str::to_owned);

    TraceEvent {
        sequence: record.sequence,
        trace_id: format!("{:016x}{:016x}", event.trace_id_hi, event.trace_id_lo),
        span_id: event.span_id,
        parent_span_id: event.parent_span_id,
        subject: trace_subject(record),
        source_attribution: attribution_name(record.source_attribution).to_owned(),
        source_actor,
        destination_attribution: attribution_name(record.destination_attribution).to_owned(),
        destination_actor,
        event_type,
        msg_type: event.msg_type,
        timestamp_ns: event.timestamp_ns,
        handler_name,
        reason: record.reason.as_deref().map(str::to_owned),
        rc_sweep: record.rc_sweep.clone(),
    }
}

fn trace_batch_model(snapshot: &crate::tracing::TraceSnapshot) -> TraceBatch {
    let mut events: Vec<_> = snapshot.events.iter().map(trace_event_model).collect();
    if !crate::tracing::trace_stream_is_current(snapshot.stream_id) {
        for event in &mut events {
            event.handler_name = None;
            for actor in [
                event
                    .subject
                    .as_mut()
                    .and_then(|subject| subject.actor.as_mut()),
                event.source_actor.as_mut(),
                event.destination_actor.as_mut(),
            ]
            .into_iter()
            .flatten()
            {
                actor.actor_type = None;
            }
        }
    }
    let overrun = (snapshot.overrun_count != 0).then(|| TraceOverrun {
        first_missing_sequence: snapshot.oldest_sequence - snapshot.overrun_count,
        last_missing_sequence: snapshot.oldest_sequence - 1,
        count: snapshot.overrun_count,
    });
    TraceBatch {
        producer_instance_id: snapshot.producer_instance_id.to_string(),
        stream_id: snapshot.stream_id.to_string(),
        oldest_sequence: snapshot.oldest_sequence,
        next_sequence: snapshot.next_sequence,
        next_cursor: snapshot.next_cursor,
        has_more: snapshot.has_more,
        overrun,
        tracing_enabled: snapshot.enabled,
        sampling_mode: snapshot.sampling_mode.to_owned(),
        events_recorded_total: snapshot.events_recorded_total,
        events_overwritten_total: snapshot.events_overwritten_total,
        events_sampled_out_total: snapshot.events_sampled_out_total,
        batch_source_monotonic_ns: snapshot.batch_source_monotonic_ns,
        clock_anchor: TraceClockAnchor {
            monotonic_ns: snapshot.clock_anchor.monotonic_ns,
            unix_epoch_ns: snapshot.clock_anchor.unix_epoch_ns,
            uncertainty_ns: snapshot.clock_anchor.uncertainty_ns,
        },
        events,
    }
}

/// `GET /api/traces` — read a non-destructive batch of retained trace events.
fn serve_traces(query: Option<&str>) -> Response<Full<Bytes>> {
    let query = match parse_trace_query(query) {
        Ok(query) => query,
        Err(message) => {
            return trace_error_response(StatusCode::BAD_REQUEST, "invalid_trace_cursor", message);
        }
    };

    let expected_stream_id = query
        .stream_id
        .and_then(crate::tracing::TraceStreamId::parse_hex);
    match crate::tracing::trace_snapshot(expected_stream_id, query.after, query.limit) {
        Ok(snapshot) => {
            let batch = trace_batch_model(&snapshot);
            let body = serde_json::to_string(&batch).expect("TraceBatch must serialize");
            json_response(&body)
        }
        Err(crate::tracing::TraceSnapshotError::FutureCursor {
            after,
            next_sequence,
        }) => trace_error_response(
            StatusCode::BAD_REQUEST,
            "future_trace_cursor",
            &format!("after cursor {after} is not below the next sequence {next_sequence}"),
        ),
        Err(crate::tracing::TraceSnapshotError::StreamMismatch {
            requested,
            current,
            producer_instance_id,
        }) => {
            let mismatch = TraceStreamMismatch {
                error: "trace_stream_mismatch".to_owned(),
                requested_stream_id: requested.to_string(),
                current_stream_id: current.to_string(),
                current_producer_instance_id: producer_instance_id.to_string(),
            };
            let body =
                serde_json::to_string(&mismatch).expect("TraceStreamMismatch must serialize");
            json_response_with_status(StatusCode::CONFLICT, &body)
        }
    }
}

/// `GET /api/supervisors` — supervision tree rows.
fn serve_supervisors() -> Response<Full<Bytes>> {
    let json = crate::supervisor::snapshot_tree_json();
    json_response(&json)
}

/// `GET /api/crashes` — recent crash log entries.
fn serve_crashes() -> Response<Full<Bytes>> {
    let json = crate::crash::snapshot_crashes_json();
    json_response(&json)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::profiler::actor_registry::ActorSnapshot;
    use serde_json::json;

    fn make_snapshot(actor_type: &str, state: &'static str) -> ActorSnapshot {
        ActorSnapshot {
            id: 1,
            pid: 0,
            actor_type: actor_type.to_owned(),
            state,
            messages_processed: 0,
            processing_time_ns: 0,
            mailbox_depth: 0,
            mailbox_hwm: 0,
        }
    }

    fn parse_actor_type(output: &str) -> String {
        let parsed: serde_json::Value =
            serde_json::from_str(output).expect("actors_json must produce valid JSON");
        let arr = parsed.as_array().expect("top-level must be an array");
        arr[0]["actor_type"]
            .as_str()
            .expect("actor_type must be a string")
            .to_owned()
    }

    #[test]
    fn actors_json_metacharacters_in_type_name_round_trip() {
        // `"`, `\`, `\n`, `\r`, `\t` must be escaped so serde can parse and
        // the round-tripped value equals the original.
        let snapshots = vec![make_snapshot("quo\"te\\slash\nline\rreturn\t", "idle")];
        let output = actors_json(&snapshots);
        assert_eq!(parse_actor_type(&output), "quo\"te\\slash\nline\rreturn\t");
    }

    #[test]
    fn actors_json_clean_type_name_round_trips() {
        let snapshots = vec![make_snapshot("MyActorType", "idle")];
        let output = actors_json(&snapshots);
        assert_eq!(parse_actor_type(&output), "MyActorType");
    }

    #[test]
    fn actors_json_control_chars_in_type_name_produce_valid_json() {
        // U+0008 (\b), U+000C (\f), and U+0001 are all control characters
        // that the RFC 8259-incomplete `json_escape` helper (removed) missed.
        // `util::push_json_string` handles them via the `ch.is_control()` arm.
        let snapshots = vec![make_snapshot("a\x08b\x0Cc\x01d", "idle")];
        let output = actors_json(&snapshots);
        // Must parse as valid JSON and round-trip the original value.
        assert_eq!(parse_actor_type(&output), "a\x08b\x0Cc\x01d");
    }

    #[test]
    fn metrics_history_json_round_trips_tcp_fields() {
        let json = history_json(&[crate::profiler::metrics::MetricsSnapshot {
            timestamp_secs: 7,
            tasks_spawned: 1,
            tasks_completed: 2,
            steals: 3,
            messages_sent: 4,
            messages_received: 5,
            active_workers: 6,
            alloc_count: 8,
            dealloc_count: 9,
            bytes_allocated: 10,
            bytes_freed: 11,
            bytes_live: 12,
            peak_bytes_live: 13,
            tcp_bytes_read: 14,
            tcp_bytes_written: 15,
            tcp_accept_count: 16,
            tcp_connect_count: 17,
            tcp_error_count: 18,
            ..Default::default()
        }]);
        let parsed: serde_json::Value =
            serde_json::from_str(&json).expect("history must be valid json");
        assert_eq!(
            parsed,
            json!([{
                "t": "7",
                "ts": "1",
                "tc": "2",
                "st": "3",
                "ms": "4",
                "mr": "5",
                "aw": "6",
                "ac": "8",
                "dc": "9",
                "ba": "10",
                "bf": "11",
                "bl": "12",
                "pb": "13",
                "tbr": "14",
                "tbw": "15",
                "tac": "16",
                "tcc": "17",
                "tec": "18"
            }])
        );
    }

    #[test]
    fn envelope_json_wraps_array_body_with_schema_version() {
        let env = envelope_json_raw("[]");
        let parsed: serde_json::Value =
            serde_json::from_str(&env).expect("envelope must be valid JSON");
        assert_eq!(parsed["schema_version"], json!("v1"));
        assert_eq!(parsed["data"], json!([]));
    }

    #[test]
    fn envelope_json_wraps_object_body_with_schema_version() {
        let env = envelope_json_raw(
            r#"{"local_node_id":"unconfigured","local_route_slot":0,"session_incarnation":0,"routes":[]}"#,
        );
        let parsed: serde_json::Value =
            serde_json::from_str(&env).expect("envelope must be valid JSON");
        assert_eq!(parsed["schema_version"], json!("v1"));
        assert_eq!(parsed["data"]["local_node_id"], json!("unconfigured"));
    }

    #[test]
    fn observe_schema_version_matches_canonical_constant() {
        // The producer + consumer copies of the schema-version string must
        // stay in lockstep. Hard-coded here so a drift in either crate trips
        // a producer-side test alongside any consumer-side mismatch.
        assert_eq!(OBSERVE_SCHEMA_VERSION, "v1");
    }

    #[test]
    fn current_metrics_json_round_trips_tcp_fields() {
        let json = current_metrics_json(&crate::profiler::metrics::MetricsSnapshot {
            timestamp_secs: 1,
            tasks_spawned: 2,
            tasks_completed: 3,
            steals: 4,
            messages_sent: 5,
            messages_received: 6,
            active_workers: 7,
            alloc_count: 8,
            dealloc_count: 9,
            bytes_allocated: 10,
            bytes_freed: 11,
            bytes_live: 12,
            peak_bytes_live: 13,
            tcp_bytes_read: 14,
            tcp_bytes_written: 15,
            tcp_accept_count: 16,
            tcp_connect_count: 17,
            tcp_error_count: 18,
            ..Default::default()
        });
        let parsed: serde_json::Value =
            serde_json::from_str(&json).expect("current metrics must be valid json");
        assert_eq!(
            parsed,
            json!({
                "timestamp_secs": "1",
                "tasks_spawned": "2",
                "tasks_completed": "3",
                "steals": "4",
                "messages_sent": "5",
                "messages_received": "6",
                "active_workers": "7",
                "alloc_count": "8",
                "dealloc_count": "9",
                "bytes_allocated": "10",
                "bytes_freed": "11",
                "bytes_live": "12",
                "peak_bytes_live": "13",
                "tcp_bytes_read": "14",
                "tcp_bytes_written": "15",
                "tcp_accept_count": "16",
                "tcp_connect_count": "17",
                "tcp_error_count": "18"
            })
        );
    }

    #[test]
    fn trace_query_defaults_to_oldest_retained_batch() {
        assert_eq!(
            parse_trace_query(None),
            Ok(TraceQuery {
                stream_id: None,
                after: None,
                limit: 256,
            })
        );
    }

    #[test]
    fn trace_query_accepts_paired_canonical_cursor_and_limit() {
        assert_eq!(
            parse_trace_query(Some(
                "stream_id=0123456789abcdef0123456789abcdef&after=18446744073709551615&limit=1"
            )),
            Ok(TraceQuery {
                stream_id: Some("0123456789abcdef0123456789abcdef"),
                after: Some(u64::MAX),
                limit: 1,
            })
        );
    }

    #[test]
    fn trace_query_rejects_unpaired_or_noncanonical_cursor() {
        for query in [
            "stream_id=0123456789abcdef0123456789abcdef",
            "after=1",
            "stream_id=0123456789ABCDEF0123456789ABCDEF&after=1",
            "stream_id=0123456789abcdef0123456789abcdef&after=01",
            "stream_id=0123456789abcdef0123456789abcdef&after=+1",
            "stream_id=0123456789abcdef0123456789abcdef&after=18446744073709551616",
        ] {
            assert!(
                parse_trace_query(Some(query)).is_err(),
                "query unexpectedly accepted: {query}"
            );
        }
    }

    #[test]
    fn trace_query_rejects_duplicate_unknown_and_out_of_range_parameters() {
        for query in [
            "limit=0",
            "limit=257",
            "limit=01",
            "limit=1&limit=2",
            "wat=1",
            "limit",
        ] {
            assert!(
                parse_trace_query(Some(query)).is_err(),
                "query unexpectedly accepted: {query}"
            );
        }
    }
}
