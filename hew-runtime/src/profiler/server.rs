//! Profiling HTTP server.
//!
//! Runs on a dedicated OS thread (not a Hew actor) so it remains
//! responsive even when the scheduler is overloaded.

use crate::util::{json_array, push_json_string, MutexExt};
use std::convert::Infallible;
use std::fmt::Write as _;
use std::sync::{Arc, Mutex};
use std::time::Duration;

use http_body_util::Full;
use hyper::body::Bytes;
use hyper::server::conn::http1;
use hyper::service::service_fn;
use hyper::{Request, Response, StatusCode};
use hyper_util::rt::TokioIo;
#[cfg(unix)]
use std::os::unix::fs::PermissionsExt;
#[cfg(unix)]
use std::path::Path;
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

/// Start the profiling HTTP server on a TCP socket, blocking the current
/// thread.
///
/// Creates a single-threaded tokio runtime for the async hyper server.
/// Checks `PROFILER_SHUTDOWN` periodically to exit cleanly.
pub fn run_tcp(bind_addr: &str, ctx: Arc<ProfilerContext>) {
    let Some(rt) = build_runtime() else { return };

    rt.block_on(async move {
        let listener = match TcpListener::bind(bind_addr).await {
            Ok(l) => l,
            Err(e) => {
                eprintln!("[hew-pprof] failed to bind {bind_addr}: {e}");
                return;
            }
        };

        eprintln!("[hew-pprof] dashboard at http://{bind_addr}/");

        serve_loop(Listener::Tcp(listener), ctx).await;
    });
}

#[cfg(unix)]
/// Start the profiling HTTP server on a unix domain socket, blocking the
/// current thread.
///
/// The socket file must not already exist (caller should clean up stale
/// sockets before calling).
pub fn run_unix(socket_path: &Path, ctx: Arc<ProfilerContext>) {
    let Some(rt) = build_runtime() else { return };

    rt.block_on(async move {
        let listener = match UnixListener::bind(socket_path) {
            Ok(l) => l,
            Err(e) => {
                eprintln!(
                    "[hew-pprof] failed to bind unix socket {}: {e}",
                    socket_path.display(),
                );
                return;
            }
        };

        // Set socket to mode 0600 — only the owning user can connect.
        if let Err(e) =
            std::fs::set_permissions(socket_path, std::fs::Permissions::from_mode(0o600))
        {
            eprintln!("[hew-pprof] failed to set socket permissions: {e}");
            return;
        }

        eprintln!("[hew-pprof] listening on unix:{}", socket_path.display());

        serve_loop(Listener::Unix(listener), ctx).await;
    });
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
        "/api/memory" => serve_memory(),
        "/api/actors" => serve_actors(),
        "/api/metrics/history" => serve_history(&ctx.ring),
        "/api/cluster/members" => serve_cluster_members(ctx),
        "/api/connections" => serve_connections(ctx),
        "/api/routing/table" => serve_routing_table(ctx),
        "/api/traces" => serve_traces(),
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

fn json_response(body: String) -> Response<Full<Bytes>> {
    Response::builder()
        .status(StatusCode::OK)
        .header("Content-Type", "application/json; charset=utf-8")
        .body(Full::new(Bytes::from(body)))
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

    json_response(current_metrics_json(&snap))
}

fn current_metrics_json(snap: &crate::profiler::metrics::MetricsSnapshot) -> String {
    format!(
        r#"{{"timestamp_secs":{},"tasks_spawned":{},"tasks_completed":{},"steals":{},"messages_sent":{},"messages_received":{},"active_workers":{},"alloc_count":{},"dealloc_count":{},"bytes_allocated":{},"bytes_freed":{},"bytes_live":{},"peak_bytes_live":{},"tcp_bytes_read":{},"tcp_bytes_written":{},"tcp_accept_count":{},"tcp_connect_count":{},"tcp_error_count":{}}}"#,
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
    let json = format!(
        r#"{{"alloc_count":{},"dealloc_count":{},"bytes_allocated":{},"bytes_freed":{},"bytes_live":{},"peak_bytes_live":{}}}"#,
        stats.alloc_count,
        stats.dealloc_count,
        stats.bytes_allocated,
        stats.bytes_freed,
        stats.bytes_live,
        stats.peak_bytes_live,
    );
    json_response(json)
}

/// `GET /api/metrics/history` — time-series from ring buffer.
fn serve_history(ring: &Arc<Mutex<MetricsRing>>) -> Response<Full<Bytes>> {
    let ring_guard = ring.lock_or_recover();
    let entries = ring_guard.read_all();
    drop(ring_guard);

    json_response(history_json(&entries))
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
            r#"{{"t":{},"ts":{},"tc":{},"st":{},"ms":{},"mr":{},"aw":{},"ac":{},"dc":{},"ba":{},"bf":{},"bl":{},"pb":{},"tbr":{},"tbw":{},"tac":{},"tcc":{},"tec":{}}}"#,
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
        let _ = write!(json, r#"{{"id":{},"pid":{},"actor_type":"#, a.id, a.pid);
        push_json_string(json, a.actor_type);
        let _ = write!(json, r#","state":"#);
        push_json_string(json, a.state);
        let _ = write!(
            json,
            r#","msgs":{},"time_ns":{},"mbox_depth":{},"mbox_hwm":{}}}"#,
            a.messages_processed, a.processing_time_ns, a.mailbox_depth, a.mailbox_hwm,
        );
    })
}

/// `GET /api/actors` — per-actor stats and mailbox depths.
fn serve_actors() -> Response<Full<Bytes>> {
    let actors = crate::profiler::actor_registry::snapshot_all();
    json_response(actors_json(&actors))
}

// ── Distributed runtime endpoints ───────────────────────────────────────

/// `GET /api/cluster/members` — cluster membership snapshot.
fn serve_cluster_members(ctx: &ProfilerContext) -> Response<Full<Bytes>> {
    if ctx.cluster.is_null() {
        return json_response("[]".to_owned());
    }
    // SAFETY: pointer is non-null and points to a valid HewCluster that
    // outlives the profiler thread. Internal access is mutex-protected.
    let cluster = unsafe { &*ctx.cluster };
    let json = crate::cluster::snapshot_members_json(cluster);
    json_response(json)
}

/// `GET /api/connections` — connection snapshot.
fn serve_connections(ctx: &ProfilerContext) -> Response<Full<Bytes>> {
    if ctx.connmgr.is_null() {
        return json_response("[]".to_owned());
    }
    // SAFETY: pointer is non-null and points to a valid HewConnMgr that
    // outlives the profiler thread. Internal access is mutex-protected.
    let mgr = unsafe { &*ctx.connmgr };
    let json = crate::connection::snapshot_connections_json(mgr);
    json_response(json)
}

/// `GET /api/routing/table` — routing table snapshot.
fn serve_routing_table(ctx: &ProfilerContext) -> Response<Full<Bytes>> {
    if ctx.routing.is_null() {
        return json_response(r#"{"local_node_id":0,"routes":[]}"#.to_owned());
    }
    // SAFETY: pointer is non-null and points to a valid HewRoutingTable
    // that outlives the profiler thread. Internal access is RwLock-protected.
    let table = unsafe { &*ctx.routing };
    let json = crate::routing::snapshot_routing_json(table);
    json_response(json)
}

/// `GET /api/traces` — drain trace events.
fn serve_traces() -> Response<Full<Bytes>> {
    let json = crate::tracing::drain_events_json();
    json_response(json)
}

/// `GET /api/supervisors` — supervision tree rows.
fn serve_supervisors() -> Response<Full<Bytes>> {
    let json = crate::supervisor::snapshot_tree_json();
    json_response(json)
}

/// `GET /api/crashes` — recent crash log entries.
fn serve_crashes() -> Response<Full<Bytes>> {
    let json = crate::crash::snapshot_crashes_json();
    json_response(json)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::profiler::actor_registry::ActorSnapshot;
    use serde_json::json;

    fn make_snapshot(actor_type: &'static str, state: &'static str) -> ActorSnapshot {
        ActorSnapshot {
            id: 1,
            pid: 0,
            actor_type,
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
        }]);
        let parsed: serde_json::Value =
            serde_json::from_str(&json).expect("history must be valid json");
        assert_eq!(
            parsed,
            json!([{
                "t": 7,
                "ts": 1,
                "tc": 2,
                "st": 3,
                "ms": 4,
                "mr": 5,
                "aw": 6,
                "ac": 8,
                "dc": 9,
                "ba": 10,
                "bf": 11,
                "bl": 12,
                "pb": 13,
                "tbr": 14,
                "tbw": 15,
                "tac": 16,
                "tcc": 17,
                "tec": 18
            }])
        );
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
        });
        let parsed: serde_json::Value =
            serde_json::from_str(&json).expect("current metrics must be valid json");
        assert_eq!(
            parsed,
            json!({
                "timestamp_secs": 1,
                "tasks_spawned": 2,
                "tasks_completed": 3,
                "steals": 4,
                "messages_sent": 5,
                "messages_received": 6,
                "active_workers": 7,
                "alloc_count": 8,
                "dealloc_count": 9,
                "bytes_allocated": 10,
                "bytes_freed": 11,
                "bytes_live": 12,
                "peak_bytes_live": 13,
                "tcp_bytes_read": 14,
                "tcp_bytes_written": 15,
                "tcp_accept_count": 16,
                "tcp_connect_count": 17,
                "tcp_error_count": 18
            })
        );
    }
}
