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

// ── Server ──────────────────────────────────────────────────────────────

/// Start the profiling HTTP server, blocking the current thread.
///
/// # Panics
///
/// Panics if `tiny_http::Server::http` fails to bind.
pub fn run(bind_addr: &str, ring: &Arc<Mutex<MetricsRing>>) {
    let server = match Server::http(bind_addr) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("[hew-pprof] failed to bind {bind_addr}: {e}");
            return;
        }
    };

    eprintln!("[hew-pprof] dashboard at http://{bind_addr}/");

    for request in server.incoming_requests() {
        handle_request(request, ring);
    }
}

fn handle_request(req: Request, ring: &Arc<Mutex<MetricsRing>>) {
    let path = req.url().to_owned();

    match path.as_str() {
        "/" => serve_text(req, DASHBOARD_HTML, "text/html; charset=utf-8"),
        "/dashboard.js" => serve_text(req, DASHBOARD_JS, "application/javascript; charset=utf-8"),
        "/api/metrics" => serve_current_metrics(req, ring),
        "/api/memory" => serve_memory(req),
        "/api/actors" => serve_actors(req),
        "/api/metrics/history" => serve_history(req, ring),
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
