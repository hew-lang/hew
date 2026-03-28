//! Profiling HTTP server.
//!
//! Runs on a dedicated OS thread (not a Hew actor) so it remains
//! responsive even when the scheduler is overloaded.

use crate::util::MutexExt;
use std::convert::Infallible;
use std::fmt::Write as _;
use std::os::unix::fs::PermissionsExt;
use std::path::Path;
use std::sync::{Arc, Mutex};
use std::time::Duration;

use http_body_util::Full;
use hyper::body::Bytes;
use hyper::server::conn::http1;
use hyper::service::service_fn;
use hyper::{Request, Response, StatusCode};
use hyper_util::rt::TokioIo;
use tokio::net::{TcpListener, UnixListener};

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
    let rt = match build_runtime() {
        Some(rt) => rt,
        None => return,
    };

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

/// Start the profiling HTTP server on a unix domain socket, blocking the
/// current thread.
///
/// The socket file must not already exist (caller should clean up stale
/// sockets before calling).
pub fn run_unix(socket_path: &Path, ctx: Arc<ProfilerContext>) {
    let rt = match build_runtime() {
        Some(rt) => rt,
        None => return,
    };

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
            eprintln!("[hew-pprof] failed to set socket permissions: {e}",);
            return;
        }

        eprintln!("[hew-pprof] listening on unix:{}", socket_path.display(),);

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
            async move { Ok::<_, Infallible>(handle_request(req, &ctx)) }
        });
        let _ = http1::Builder::new().serve_connection(io, service).await;
    });
}

// ── Request routing ────────────────────────────────────���────────────────

fn handle_request(
    req: Request<hyper::body::Incoming>,
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

    let json = format!(
        r#"{{"timestamp_secs":{},"tasks_spawned":{},"tasks_completed":{},"steals":{},"messages_sent":{},"messages_received":{},"active_workers":{},"alloc_count":{},"dealloc_count":{},"bytes_allocated":{},"bytes_freed":{},"bytes_live":{},"peak_bytes_live":{}}}"#,
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
    );
    json_response(json)
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

    // Build JSON array manually to avoid serde dependency for this path.
    let mut json = String::from("[");
    for (i, s) in entries.iter().enumerate() {
        if i > 0 {
            json.push(',');
        }
        let _ = write!(
            json,
            r#"{{"t":{},"ts":{},"tc":{},"st":{},"ms":{},"mr":{},"aw":{},"ac":{},"dc":{},"ba":{},"bf":{},"bl":{},"pb":{}}}"#,
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
        );
    }
    json.push(']');

    json_response(json)
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

/// `GET /api/actors` — per-actor stats and mailbox depths.
fn serve_actors() -> Response<Full<Bytes>> {
    let actors = crate::profiler::actor_registry::snapshot_all();

    let mut json = String::from("[");
    for (i, a) in actors.iter().enumerate() {
        if i > 0 {
            json.push(',');
        }
        let _ = write!(
            json,
            r#"{{"id":{},"pid":{},"state":"{}","msgs":{},"time_ns":{},"mbox_depth":{},"mbox_hwm":{}}}"#,
            a.id,
            a.pid,
            a.state,
            a.messages_processed,
            a.processing_time_ns,
            a.mailbox_depth,
            a.mailbox_hwm,
        );
    }
    json.push(']');

    json_response(json)
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
