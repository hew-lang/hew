//! Profiling HTTP server.
//!
//! Runs on a dedicated OS thread (not a Hew actor) so it remains
//! responsive even when the scheduler is overloaded.

use std::fmt::Write as _;
use std::sync::{Arc, Mutex};

use tiny_http::{Header, Request, Response, Server, StatusCode};

use crate::profiler::allocator;
use crate::profiler::metrics::MetricsRing;
use crate::profiler::pprof;

// ── Dashboard assets (embedded at compile time) ─────────────────────────

const DASHBOARD_HTML: &str = include_str!("dashboard/index.html");
const DASHBOARD_JS: &str = include_str!("dashboard/dashboard.js");

// ── Profiler context ────────────────────────────────────────────────────

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

// ── Server ──────────────────────────────────────────────────────────────

/// Start the profiling HTTP server, blocking the current thread.
///
/// # Panics
///
/// Panics if `tiny_http::Server::http` fails to bind.
pub fn run(bind_addr: &str, ctx: &ProfilerContext) {
    let server = match Server::http(bind_addr) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("[hew-pprof] failed to bind {bind_addr}: {e}");
            return;
        }
    };

    eprintln!("[hew-pprof] dashboard at http://{bind_addr}/");

    for request in server.incoming_requests() {
        handle_request(request, ctx);
    }
}

fn handle_request(req: Request, ctx: &ProfilerContext) {
    let path = req.url().to_owned();

    match path.as_str() {
        "/" => serve_text(req, DASHBOARD_HTML, "text/html; charset=utf-8"),
        "/dashboard.js" => serve_text(req, DASHBOARD_JS, "application/javascript; charset=utf-8"),
        "/api/metrics" => serve_current_metrics(req, &ctx.ring),
        "/api/memory" => serve_memory(req),
        "/api/actors" => serve_actors(req),
        "/api/metrics/history" => serve_history(req, &ctx.ring),
        "/api/cluster/members" => serve_cluster_members(req, ctx),
        "/api/connections" => serve_connections(req, ctx),
        "/api/routing/table" => serve_routing_table(req, ctx),
        "/api/traces" => serve_traces(req),
        "/debug/pprof/heap" => serve_pprof_heap(req),
        "/debug/pprof/profile" => serve_flat_profile(req),
        _ => serve_not_found(req),
    }
}

// ── Static assets ───────────────────────────────────────────────────────

fn serve_text(req: Request, body: &str, content_type: &str) {
    let header = Header::from_bytes("Content-Type", content_type).expect("valid header");
    let resp = Response::from_string(body)
        .with_header(header)
        .with_status_code(StatusCode(200));
    let _ = req.respond(resp);
}

fn serve_not_found(req: Request) {
    let resp = Response::from_string("404 Not Found").with_status_code(StatusCode(404));
    let _ = req.respond(resp);
}

// ── API endpoints ───────────────────────────────────────────────────────

fn json_response(req: Request, body: &str) {
    let header = Header::from_bytes("Content-Type", "application/json; charset=utf-8")
        .expect("valid header");
    let resp = Response::from_string(body)
        .with_header(header)
        .with_status_code(StatusCode(200));
    let _ = req.respond(resp);
}

/// `GET /api/metrics` — current scheduler counters.
fn serve_current_metrics(req: Request, ring: &Arc<Mutex<MetricsRing>>) {
    let ring_guard = match ring.lock() {
        Ok(g) => g,
        Err(e) => e.into_inner(),
    };

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
    json_response(req, &json);
}

/// `GET /api/memory` — current allocator stats.
fn serve_memory(req: Request) {
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
    json_response(req, &json);
}

/// `GET /api/metrics/history` — time-series from ring buffer.
fn serve_history(req: Request, ring: &Arc<Mutex<MetricsRing>>) {
    let ring_guard = match ring.lock() {
        Ok(g) => g,
        Err(e) => e.into_inner(),
    };
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

    json_response(req, &json);
}

/// `GET /debug/pprof/heap` — pprof-compatible heap profile (.pb.gz).
fn serve_pprof_heap(req: Request) {
    let data = pprof::generate_heap_profile();
    let ct = Header::from_bytes("Content-Type", "application/octet-stream").expect("valid header");
    let cd = Header::from_bytes("Content-Disposition", "attachment; filename=\"heap.pb.gz\"")
        .expect("valid header");
    let resp = Response::from_data(data)
        .with_header(ct)
        .with_header(cd)
        .with_status_code(StatusCode(200));
    let _ = req.respond(resp);
}

/// `GET /debug/pprof/profile` — gprof-style flat profile (text).
fn serve_flat_profile(req: Request) {
    let text = pprof::generate_flat_profile();
    serve_text(req, &text, "text/plain; charset=utf-8");
}

/// `GET /api/actors` — per-actor stats and mailbox depths.
fn serve_actors(req: Request) {
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

    json_response(req, &json);
}

// ── Distributed runtime endpoints ───────────────────────────────────────

/// `GET /api/cluster/members` — cluster membership snapshot.
fn serve_cluster_members(req: Request, ctx: &ProfilerContext) {
    if ctx.cluster.is_null() {
        json_response(req, "[]");
        return;
    }
    // SAFETY: pointer is non-null and points to a valid HewCluster that
    // outlives the profiler thread. Internal access is mutex-protected.
    let cluster = unsafe { &*ctx.cluster };
    let json = crate::cluster::snapshot_members_json(cluster);
    json_response(req, &json);
}

/// `GET /api/connections` — connection snapshot.
fn serve_connections(req: Request, ctx: &ProfilerContext) {
    if ctx.connmgr.is_null() {
        json_response(req, "[]");
        return;
    }
    // SAFETY: pointer is non-null and points to a valid HewConnMgr that
    // outlives the profiler thread. Internal access is mutex-protected.
    let mgr = unsafe { &*ctx.connmgr };
    let json = crate::connection::snapshot_connections_json(mgr);
    json_response(req, &json);
}

/// `GET /api/routing/table` — routing table snapshot.
fn serve_routing_table(req: Request, ctx: &ProfilerContext) {
    if ctx.routing.is_null() {
        json_response(req, r#"{"local_node_id":0,"routes":[]}"#);
        return;
    }
    // SAFETY: pointer is non-null and points to a valid HewRoutingTable
    // that outlives the profiler thread. Internal access is RwLock-protected.
    let table = unsafe { &*ctx.routing };
    let json = crate::routing::snapshot_routing_json(table);
    json_response(req, &json);
}

/// `GET /api/traces` — drain trace events.
fn serve_traces(req: Request) {
    let json = crate::tracing::drain_events_json();
    json_response(req, &json);
}
