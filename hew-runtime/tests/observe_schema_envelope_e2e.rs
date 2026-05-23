//! E2E probe: every `/api/*` JSON response served by the profiler HTTP
//! surface carries the canonical observe schema-version envelope
//! (`{"schema_version":"v0.5","data":<body>}`).
//!
//! Producer: `hew_runtime::profiler::server::json_response`, exercised here
//! through `run_tcp_with_listener` (the same code path `run_tcp` uses in
//! production), so the assertion covers the real hyper Response surface
//! end-to-end — not just the `envelope_json` helper.
//!
//! Per R58 Q136 (Option B), the envelope is the ONLY new surface; the
//! `#[repr(C)]` payloads crossing the FFI boundary (`CrashReport`,
//! `CrashStats`) are untouched.

#![cfg(all(feature = "profiler", not(target_arch = "wasm32")))]

use std::net::TcpListener;
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::Duration;

use hew_runtime::profiler::{run_tcp_with_listener, ProfilerContext, OBSERVE_SCHEMA_VERSION};

/// Spin up the profiler HTTP server on an ephemeral port and assert that
/// `/api/actors` and `/api/routing/table` both come back wrapped in the
/// canonical envelope with `schema_version == "v0.5"`.
///
/// Covers both an array-body endpoint (`/api/actors` → `[]`) and an
/// object-body endpoint (`/api/routing/table` → `{"local_node_id":0, …}`)
/// to pin that the envelope shape is uniform across response shapes.
#[test]
fn profiler_api_responses_carry_observe_schema_version_envelope() {
    // Bind first, so the test knows the port before the server spawns and
    // any race between bind() and connect() is impossible.
    let listener = TcpListener::bind("127.0.0.1:0").expect("bind ephemeral port");
    let addr = listener.local_addr().expect("local addr");

    // Null subsystem pointers — endpoints check `is_null()` and return the
    // canonical empty bodies (`[]`, `{"local_node_id":0,"routes":[]}`).
    let ctx = Arc::new(ProfilerContext {
        ring: Arc::new(Mutex::new(
            hew_runtime::profiler::metrics::MetricsRing::new(),
        )),
        cluster: std::ptr::null_mut(),
        connmgr: std::ptr::null_mut(),
        routing: std::ptr::null_mut(),
    });

    // The server thread will outlive this test — cargo runs each integration
    // test in its own process, so the thread is reaped when the process exits.
    thread::Builder::new()
        .name("observe-envelope-e2e-server".into())
        .spawn({
            let ctx = Arc::clone(&ctx);
            move || run_tcp_with_listener(listener, ctx)
        })
        .expect("spawn profiler server thread");

    // Give the runtime a moment to wire up the listener loop before the
    // first GET. 200 ms is the accept-loop poll cadence in `serve_loop`.
    thread::sleep(Duration::from_millis(250));

    let base_url = format!("http://{addr}");

    // ── /api/actors (array body) ────────────────────────────────────────
    let actors_body = http_get(&format!("{base_url}/api/actors"));
    let actors_env: serde_json::Value = serde_json::from_str(&actors_body).unwrap_or_else(|err| {
        panic!("/api/actors envelope must parse as JSON: {err}\n{actors_body}")
    });
    assert_eq!(
        actors_env["schema_version"],
        serde_json::json!(OBSERVE_SCHEMA_VERSION),
        "schema_version on /api/actors envelope must equal the canonical \
         OBSERVE_SCHEMA_VERSION (got {actors_body})",
    );
    assert!(
        actors_env["data"].is_array(),
        "data field on /api/actors envelope must hold an array (got {actors_body})",
    );

    // ── /api/routing/table (object body) ────────────────────────────────
    let routing_body = http_get(&format!("{base_url}/api/routing/table"));
    let routing_env: serde_json::Value =
        serde_json::from_str(&routing_body).unwrap_or_else(|err| {
            panic!("/api/routing/table envelope must parse as JSON: {err}\n{routing_body}")
        });
    assert_eq!(
        routing_env["schema_version"],
        serde_json::json!(OBSERVE_SCHEMA_VERSION),
        "schema_version on /api/routing/table envelope must equal the \
         canonical OBSERVE_SCHEMA_VERSION (got {routing_body})",
    );
    assert!(
        routing_env["data"].is_object(),
        "data field on /api/routing/table envelope must hold an object \
         (got {routing_body})",
    );

    // Also pin the redundant X-Hew-Schema-Version header so consumers can
    // short-circuit without parsing the body.
    let resp = ureq::get(&format!("{base_url}/api/actors"))
        .call()
        .expect("GET /api/actors must succeed");
    let header = resp
        .headers()
        .get("X-Hew-Schema-Version")
        .and_then(|v| v.to_str().ok())
        .map(str::to_owned);
    assert_eq!(
        header.as_deref(),
        Some(OBSERVE_SCHEMA_VERSION),
        "X-Hew-Schema-Version response header must echo the canonical version",
    );
}

fn http_get(url: &str) -> String {
    let mut resp = ureq::get(url).call().unwrap_or_else(|err| {
        panic!("GET {url} must succeed: {err}");
    });
    resp.body_mut()
        .read_to_string()
        .unwrap_or_else(|err| panic!("read body for {url}: {err}"))
}
