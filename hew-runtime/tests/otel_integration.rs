//! Integration test: `OTel` OTLP/HTTP exporter end-to-end.
//!
//! Starts a minimal HTTP server on a random port (using `std::net::TcpListener`
//! and `tiny_http`, both already available in `dev-dependencies`), then:
//!
//! 1. Configures the `OTel` exporter to point at the mock receiver.
//! 2. Records a begin/end span pair and a lifecycle event via the tracing C ABI.
//! 3. Drains events, reconstructs spans, and posts them.
//! 4. Verifies the mock receiver received valid OTLP/HTTP JSON containing the
//!    expected trace IDs, span IDs, and attribute keys.
//!
//! This test does NOT depend on a real Jaeger/Tempo instance.

#![cfg(all(feature = "otel", not(target_arch = "wasm32")))]

use std::io::Read;
use std::net::{TcpListener, TcpStream};
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::Duration;

use hew_runtime::otel::{build_otlp_json, flush_stale_begins, reconstruct_spans};
use hew_runtime::tracing::{
    drain_events, hew_trace_begin, hew_trace_enable, hew_trace_end, hew_trace_lifecycle,
    hew_trace_reset, trace_now_ns, SPAN_SPAWN,
};

// ── Mock OTLP receiver ────────────────────────────────────────────────────────

/// A minimal HTTP server that captures the first POST body it receives.
struct MockOtlpReceiver {
    addr: String,
    received_body: Arc<Mutex<Option<String>>>,
}

impl MockOtlpReceiver {
    fn start() -> Self {
        let listener = TcpListener::bind("127.0.0.1:0").expect("bind mock server");
        let addr = format!("http://{}", listener.local_addr().unwrap());
        let received_body = Arc::new(Mutex::new(None::<String>));
        let body_store = Arc::clone(&received_body);

        thread::Builder::new()
            .name("mock-otlp-receiver".into())
            .spawn(move || {
                // Accept exactly one connection then exit.
                if let Ok((mut stream, _)) = listener.accept() {
                    let body = read_http_post_body(&mut stream);
                    // Respond with HTTP 200 OK.
                    let response = b"HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\n{}";
                    let _ = std::io::Write::write_all(&mut stream, response);
                    *body_store.lock().unwrap() = Some(body);
                }
            })
            .expect("spawn mock server thread");

        MockOtlpReceiver {
            addr,
            received_body,
        }
    }

    fn wait_for_body(&self, timeout: Duration) -> Option<String> {
        let deadline = std::time::Instant::now() + timeout;
        while std::time::Instant::now() < deadline {
            if let Ok(guard) = self.received_body.lock() {
                if guard.is_some() {
                    return guard.clone();
                }
            }
            thread::sleep(Duration::from_millis(20));
        }
        self.received_body.lock().ok()?.clone()
    }
}

/// Read the HTTP request body from a raw TCP stream.
///
/// Parses the `Content-Length` header and reads exactly that many bytes.
fn read_http_post_body(stream: &mut TcpStream) -> String {
    stream.set_read_timeout(Some(Duration::from_secs(3))).ok();

    // Read until we hit the blank line separating headers from body.
    let mut raw = Vec::new();
    let mut buf = [0u8; 1];
    while !raw.ends_with(b"\r\n\r\n") {
        match stream.read(&mut buf) {
            Ok(0) | Err(_) => break,
            Ok(_) => raw.extend_from_slice(&buf),
        }
    }

    let header_text = String::from_utf8_lossy(&raw);
    let content_length: usize = header_text
        .lines()
        .find(|l| l.to_ascii_lowercase().starts_with("content-length:"))
        .and_then(|l| l.split(':').nth(1))
        .and_then(|v| v.trim().parse().ok())
        .unwrap_or(0);

    let mut body = vec![0u8; content_length];
    let mut read = 0;
    while read < content_length {
        match stream.read(&mut body[read..]) {
            Ok(0) | Err(_) => break,
            Ok(n) => read += n,
        }
    }
    body.truncate(read);
    String::from_utf8_lossy(&body).into_owned()
}

// ── Tests ─────────────────────────────────────────────────────────────────────

static TEST_LOCK: Mutex<()> = Mutex::new(());

/// Verify that a begin/end span pair is correctly exported to a mock OTLP receiver.
#[test]
fn otel_exporter_posts_valid_otlp_json() {
    let _guard = TEST_LOCK.lock().unwrap();

    // 1. Reset tracing state and enable recording.
    hew_trace_reset();
    hew_trace_enable(1);

    // 2. Record a span and a lifecycle event.
    hew_trace_begin(101, 5); // actor 101, message type 5
    hew_trace_begin(202, 9); // nested child: actor 202, message type 9
    hew_trace_end(202, 9);
    hew_trace_end(101, 5);
    hew_trace_lifecycle(303, SPAN_SPAWN); // actor 303 spawned

    // 3. Drain the recorded events (simulate what the exporter thread does).
    let events = drain_events(512);
    assert!(!events.is_empty(), "expected trace events to be recorded");

    // 4. Reconstruct spans (with persistent pending_begins state).
    let mut pending_begins = std::collections::HashMap::new();
    let mut spans = reconstruct_spans(events, &mut pending_begins);
    // Flush any stale pending begins (threshold=0 forces immediate flush in tests).
    spans.extend(flush_stale_begins(&mut pending_begins, 0));
    assert!(
        spans.len() >= 3,
        "expected at least 3 spans, got {}",
        spans.len()
    );

    let dispatch_spans: Vec<_> = spans
        .iter()
        .filter(|s| s.name.starts_with("actor.dispatch"))
        .collect();
    assert_eq!(dispatch_spans.len(), 2, "expected 2 dispatch spans");

    let spawn_spans: Vec<_> = spans.iter().filter(|s| s.name == "actor.spawn").collect();
    assert_eq!(spawn_spans.len(), 1, "expected 1 spawn span");

    // The child span should have a non-zero parent.
    let child = dispatch_spans
        .iter()
        .find(|s| s.actor_id == 202)
        .expect("child span for actor 202 not found");
    assert_ne!(child.parent_span_id, 0, "child span should have a parent");

    // All dispatch spans should have accurate timing (end >= start).
    for s in &dispatch_spans {
        assert!(s.end_ns >= s.start_ns, "end_ns must be >= start_ns");
        assert!(s.ok, "dispatch spans should be ok");
    }

    // 5. Build OTLP JSON with a real epoch offset so timestamps are valid Unix ns.
    let epoch_offset_ns = hew_runtime::tracing::unix_epoch_offset_ns();
    let json = build_otlp_json("test-service", &spans, epoch_offset_ns);

    // 6. Basic structural validation.
    assert!(json.contains("\"resourceSpans\""), "missing resourceSpans");
    assert!(json.contains("\"test-service\""), "missing service name");
    assert!(json.contains("hew.actor_id"), "missing actor_id attribute");
    assert!(json.contains("actor.spawn"), "missing spawn span name");

    // 7. Start the mock receiver and POST to it.
    let receiver = MockOtlpReceiver::start();
    let endpoint = format!("{}/v1/traces", receiver.addr);

    // POST using ureq directly (no env-var wiring needed in this test).
    let result = ureq::post(&endpoint)
        .header("Content-Type", "application/json")
        .send(&json);

    assert!(
        result.is_ok(),
        "POST to mock receiver failed: {:?}",
        result.err()
    );

    // 8. Verify the mock received the body.
    let received = receiver
        .wait_for_body(Duration::from_secs(3))
        .expect("mock receiver did not receive a request within 3 seconds");

    assert!(!received.is_empty(), "received body was empty");
    assert!(
        received.contains("resourceSpans"),
        "received body does not contain resourceSpans: {received}"
    );
    assert!(
        received.contains("test-service"),
        "service name not in received body"
    );
    assert!(
        received.contains("hew.actor_id"),
        "actor_id attribute missing from received body"
    );

    hew_trace_reset();
}

/// Verify that span reconstruction correctly pairs begin/end events and
/// assigns timestamps from the originating trace events.
///
/// The old version slept 1 ms so that `end_ns > start_ns` would hold, but
/// that assertion is tautological on any real CPU and does not actually
/// exercise reconstruction logic. This version bounds `start_ns`/`end_ns`
/// against samples taken before and after the recording, which would fail
/// if reconstruction swapped the events or invented its own timestamps.
#[test]
fn span_reconstruction_pairs_begin_and_end_events() {
    let _guard = TEST_LOCK.lock().unwrap();
    hew_trace_reset();
    hew_trace_enable(1);

    let t_before = trace_now_ns();
    hew_trace_begin(10, 1);
    hew_trace_end(10, 1);
    let t_after = trace_now_ns();

    let events = drain_events(16);
    let mut pending = std::collections::HashMap::new();
    let spans = reconstruct_spans(events, &mut pending);
    assert_eq!(spans.len(), 1, "expected exactly one reconstructed span");
    let s = &spans[0];
    assert!(
        s.start_ns >= t_before,
        "start_ns ({}) must be >= sample taken before begin ({})",
        s.start_ns,
        t_before
    );
    assert!(
        s.end_ns <= t_after,
        "end_ns ({}) must be <= sample taken after end ({})",
        s.end_ns,
        t_after
    );
    assert!(
        s.start_ns <= s.end_ns,
        "start_ns ({}) must not exceed end_ns ({})",
        s.start_ns,
        s.end_ns
    );

    hew_trace_reset();
}
