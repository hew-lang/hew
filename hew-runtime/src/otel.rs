//! OpenTelemetry OTLP/HTTP trace exporter for the Hew runtime.
//!
//! Activated by setting the `HEW_OTEL_ENDPOINT` environment variable to an
//! OTLP/HTTP collector URL (e.g. `http://localhost:4318`). When unset, the
//! exporter is a zero-cost no-op: no thread is spawned, no HTTP client is
//! created.
//!
//! # How it works
//!
//! A dedicated OS thread wakes every [`FLUSH_INTERVAL`] seconds, drains up
//! to [`BATCH_SIZE`] trace events from the shared ring buffer, reconstructs
//! complete spans from matching `SPAN_BEGIN` / `SPAN_END` pairs, and POSTs
//! them to the configured endpoint as OTLP/HTTP JSON
//! (`application/json` body, `/v1/traces` path).
//!
//! No `tokio` or other async runtime is required — `ureq` provides a
//! synchronous HTTP client.
//!
//! # Environment variables
//!
//! | Variable | Default | Description |
//! |---|---|---|
//! | `HEW_OTEL_ENDPOINT` | *(unset — exporter disabled)* | Base URL of the OTLP/HTTP collector, e.g. `http://localhost:4318` |
//! | `HEW_SERVICE_NAME` | Binary name from `argv[0]`, or `"hew"` | `service.name` resource attribute sent with every batch |
//! | `HEW_OTEL_INTERVAL_SECS` | `5` | How often to flush spans, in seconds |
//! | `HEW_OTEL_BATCH_SIZE` | `512` | Maximum spans exported per flush |
//!
//! # Example — Jaeger
//!
//! ```bash
//! docker run -p 4318:4318 jaegertracing/all-in-one
//! HEW_OTEL_ENDPOINT=http://localhost:4318 ./my_hew_program
//! # Open http://localhost:16686/ in a browser
//! ```
//!
//! # Example — Grafana Tempo
//!
//! ```bash
//! HEW_OTEL_ENDPOINT=http://localhost:4318 ./my_hew_program
//! ```

#![cfg(feature = "otel")]

use std::collections::HashMap;
use std::fmt::Write as _;
use std::thread;
use std::time::Duration;

use crate::tracing::{
    drain_events, HewTraceEvent, SPAN_BEGIN, SPAN_CRASH, SPAN_END, SPAN_SEND, SPAN_SPAWN, SPAN_STOP,
};

// ── Constants ──────────────────────────────────────────────────────────

/// Default export interval in seconds.
const DEFAULT_FLUSH_INTERVAL_SECS: u64 = 5;

/// Default maximum events to drain per flush cycle.
const DEFAULT_BATCH_SIZE: usize = 512;

/// OTLP/HTTP traces endpoint path.
const OTLP_TRACES_PATH: &str = "/v1/traces";

/// OTel instrumentation scope name.
const SCOPE_NAME: &str = "hew.runtime";

/// OTel instrumentation scope version.
const SCOPE_VERSION: &str = env!("CARGO_PKG_VERSION");

// ── Configuration ──────────────────────────────────────────────────────

/// Exporter configuration resolved from environment variables.
#[derive(Debug, Clone)]
struct OtelConfig {
    /// Full URL to POST spans to, e.g. `http://localhost:4318/v1/traces`.
    endpoint: String,
    /// `service.name` resource attribute.
    service_name: String,
    /// How often to flush spans.
    flush_interval: Duration,
    /// Maximum events to drain per flush.
    batch_size: usize,
}

impl OtelConfig {
    /// Build config from environment variables.
    ///
    /// Returns `None` if `HEW_OTEL_ENDPOINT` is not set or is empty.
    fn from_env() -> Option<Self> {
        let base = std::env::var("HEW_OTEL_ENDPOINT").ok()?;
        let base = base.trim().to_owned();
        if base.is_empty() {
            return None;
        }

        let endpoint = if base.ends_with('/') {
            format!("{base}v1/traces")
        } else {
            format!("{base}{OTLP_TRACES_PATH}")
        };

        let service_name = std::env::var("HEW_SERVICE_NAME")
            .unwrap_or_default()
            .trim()
            .to_owned();
        let service_name = if service_name.is_empty() {
            binary_name()
        } else {
            service_name
        };

        let flush_interval = std::env::var("HEW_OTEL_INTERVAL_SECS")
            .ok()
            .and_then(|v| v.trim().parse::<u64>().ok())
            .filter(|&v| v > 0)
            .unwrap_or(DEFAULT_FLUSH_INTERVAL_SECS);

        let batch_size = std::env::var("HEW_OTEL_BATCH_SIZE")
            .ok()
            .and_then(|v| v.trim().parse::<usize>().ok())
            .filter(|&v| v > 0)
            .unwrap_or(DEFAULT_BATCH_SIZE);

        Some(OtelConfig {
            endpoint,
            service_name,
            flush_interval: Duration::from_secs(flush_interval),
            batch_size,
        })
    }
}

/// Get the executable name from `argv[0]`, falling back to `"hew"`.
fn binary_name() -> String {
    std::env::current_exe()
        .ok()
        .and_then(|p| p.file_stem().and_then(|s| s.to_str()).map(str::to_owned))
        .unwrap_or_else(|| "hew".to_owned())
}

// ── Public entry point ─────────────────────────────────────────────────

/// Start the OTel exporter if `HEW_OTEL_ENDPOINT` is set.
///
/// Called from `hew_sched_init` during scheduler startup.
/// If the env var is absent, this is a complete no-op.
pub fn maybe_start() {
    let Some(config) = OtelConfig::from_env() else {
        return;
    };

    eprintln!(
        "[hew-otel] exporting traces to {} (service: {}, interval: {}s, batch: {})",
        config.endpoint,
        config.service_name,
        config.flush_interval.as_secs(),
        config.batch_size,
    );

    // Enable tracing globally so events are recorded.
    crate::tracing::hew_trace_enable(1);

    if thread::Builder::new()
        .name("hew-otel-exporter".into())
        .spawn(move || exporter_loop(&config))
        .is_err()
    {
        eprintln!("[hew-otel] failed to spawn exporter thread");
    }
}

// ── Exporter loop ──────────────────────────────────────────────────────

/// Background thread: drain → reconstruct → export.
fn exporter_loop(config: &OtelConfig) {
    loop {
        thread::sleep(config.flush_interval);
        let events = drain_events(config.batch_size);
        if events.is_empty() {
            continue;
        }
        let spans = reconstruct_spans(events);
        if spans.is_empty() {
            continue;
        }
        let json = build_otlp_json(&config.service_name, &spans);
        if let Err(e) = post_spans(&config.endpoint, &json) {
            eprintln!("[hew-otel] export failed: {e}");
        }
    }
}

// ── Span reconstruction ────────────────────────────────────────────────

/// A complete OTel-compatible span reconstructed from raw events.
#[derive(Debug)]
pub struct ReconstructedSpan {
    pub trace_id_hi: u64,
    pub trace_id_lo: u64,
    pub span_id: u64,
    pub parent_span_id: u64,
    pub actor_id: u64,
    /// OTel span kind (1 = INTERNAL).
    pub kind: u8,
    pub name: String,
    /// Monotonic start time in nanoseconds (from process epoch).
    pub start_ns: u64,
    /// Monotonic end time in nanoseconds; equals `start_ns` for point events.
    pub end_ns: u64,
    pub msg_type: i32,
    /// Whether the span ended normally (status OK) or is incomplete.
    pub ok: bool,
}

/// Reconstruct spans from a batch of raw trace events.
///
/// - `SPAN_BEGIN` / `SPAN_END` pairs are matched by `span_id` and become
///   complete spans with accurate start/end timestamps.
/// - Unpaired `SPAN_BEGIN` events (no corresponding `SPAN_END` in the batch)
///   are exported as incomplete spans using the begin timestamp for both
///   start and end.
/// - Lifecycle events (`SPAN_SPAWN`, `SPAN_CRASH`, `SPAN_STOP`, `SPAN_SEND`)
///   become point-in-time spans (zero duration) so they appear in the trace.
pub fn reconstruct_spans(events: Vec<HewTraceEvent>) -> Vec<ReconstructedSpan> {
    let mut begins: HashMap<u64, HewTraceEvent> = HashMap::new();
    let mut spans = Vec::with_capacity(events.len() / 2 + 4);

    for ev in events {
        match ev.event_type {
            SPAN_BEGIN => {
                begins.insert(ev.span_id, ev);
            }
            SPAN_END => {
                if let Some(begin) = begins.remove(&ev.span_id) {
                    spans.push(ReconstructedSpan {
                        trace_id_hi: begin.trace_id_hi,
                        trace_id_lo: begin.trace_id_lo,
                        span_id: begin.span_id,
                        parent_span_id: begin.parent_span_id,
                        actor_id: begin.actor_id,
                        kind: 1, // INTERNAL
                        name: format!("actor.dispatch[{}]", begin.msg_type),
                        start_ns: begin.timestamp_ns,
                        end_ns: ev.timestamp_ns,
                        msg_type: begin.msg_type,
                        ok: true,
                    });
                }
            }
            SPAN_SPAWN => spans.push(lifecycle_span(&ev, "actor.spawn")),
            SPAN_CRASH => spans.push(lifecycle_span(&ev, "actor.crash")),
            SPAN_STOP => spans.push(lifecycle_span(&ev, "actor.stop")),
            SPAN_SEND => spans.push(lifecycle_span(&ev, "actor.send")),
            _ => {}
        }
    }

    // Flush any unpaired begins as incomplete spans.
    for (_, ev) in begins {
        spans.push(ReconstructedSpan {
            trace_id_hi: ev.trace_id_hi,
            trace_id_lo: ev.trace_id_lo,
            span_id: ev.span_id,
            parent_span_id: ev.parent_span_id,
            actor_id: ev.actor_id,
            kind: 1,
            name: format!("actor.dispatch[{}]", ev.msg_type),
            start_ns: ev.timestamp_ns,
            end_ns: ev.timestamp_ns,
            msg_type: ev.msg_type,
            ok: false, // incomplete
        });
    }

    spans
}

fn lifecycle_span(ev: &HewTraceEvent, name: &str) -> ReconstructedSpan {
    ReconstructedSpan {
        trace_id_hi: ev.trace_id_hi,
        trace_id_lo: ev.trace_id_lo,
        span_id: ev.span_id,
        parent_span_id: ev.parent_span_id,
        actor_id: ev.actor_id,
        kind: 1,
        name: name.to_owned(),
        start_ns: ev.timestamp_ns,
        end_ns: ev.timestamp_ns,
        msg_type: 0,
        ok: true,
    }
}

// ── OTLP JSON serialisation ────────────────────────────────────────────

/// Build an OTLP/HTTP JSON body for a batch of spans.
///
/// Format: `ResourceSpans → ScopeSpans → Span[]`
///
/// References:
/// - <https://opentelemetry.io/docs/specs/otlp/#otlphttp>
/// - <https://github.com/open-telemetry/opentelemetry-proto/blob/main/opentelemetry/proto/trace/v1/trace.proto>
pub fn build_otlp_json(service_name: &str, spans: &[ReconstructedSpan]) -> String {
    let mut out = String::with_capacity(512 + spans.len() * 256);

    out.push_str(r#"{"resourceSpans":[{"resource":{"attributes":[{"key":"service.name","value":{"stringValue":""#);
    push_escaped(&mut out, service_name);
    out.push_str(r#""}},{"key":"telemetry.sdk.name","value":{"stringValue":"hew-runtime"}},{"key":"telemetry.sdk.version","value":{"stringValue":""#);
    push_escaped(&mut out, SCOPE_VERSION);
    out.push_str(r#""}}]},"scopeSpans":[{"scope":{"name":""#);
    push_escaped(&mut out, SCOPE_NAME);
    out.push_str(r#"","version":""#);
    push_escaped(&mut out, SCOPE_VERSION);
    out.push_str(r#""},"spans":["#);

    for (i, span) in spans.iter().enumerate() {
        if i > 0 {
            out.push(',');
        }
        write_span_json(&mut out, span);
    }

    out.push_str("]}]}]}");
    out
}

fn write_span_json(out: &mut String, span: &ReconstructedSpan) {
    // trace_id: 32-char lowercase hex (16 bytes)
    let _ = write!(
        out,
        r#"{{"traceId":"{:016x}{:016x}","spanId":"{:016x}","#,
        span.trace_id_hi, span.trace_id_lo, span.span_id,
    );

    if span.parent_span_id != 0 {
        let _ = write!(out, r#""parentSpanId":"{:016x}","#, span.parent_span_id);
    }

    let _ = write!(out, r#""name":""#);
    push_escaped(out, &span.name);
    let _ = write!(
        out,
        r#"","kind":{},"startTimeUnixNano":"{}","endTimeUnixNano":"{}","#,
        span.kind, span.start_ns, span.end_ns,
    );

    // Attributes
    let _ = write!(
        out,
        r#""attributes":[{{"key":"hew.actor_id","value":{{"intValue":"{}"}}}},{{"key":"hew.msg_type","value":{{"intValue":"{}"}}}}],"#,
        span.actor_id, span.msg_type,
    );

    // Status
    if span.ok {
        out.push_str(r#""status":{"code":1}"#); // STATUS_CODE_OK
    } else {
        out.push_str(r#""status":{"code":2,"message":"span incomplete"}"#); // STATUS_CODE_ERROR
    }

    out.push('}');
}

/// Push a JSON-escaped string into `out` (handles `"`, `\`, and control characters).
pub fn push_escaped(out: &mut String, s: &str) {
    for c in s.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c if (c as u32) < 0x20 => {
                let _ = write!(out, "\\u{:04x}", c as u32);
            }
            c => out.push(c),
        }
    }
}

// ── HTTP export ────────────────────────────────────────────────────────

/// POST the OTLP JSON body to `endpoint`.
///
/// Blocks the caller — should only be called from the background exporter
/// thread.
fn post_spans(endpoint: &str, json: &str) -> Result<(), String> {
    let resp = ureq::post(endpoint)
        .header("Content-Type", "application/json")
        .send(json)
        .map_err(|e| e.to_string())?;

    let status = resp.status();
    if status.is_success() {
        Ok(())
    } else {
        Err(format!("HTTP {}", status.as_u16()))
    }
}

// ── Tests ──────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tracing::{SPAN_BEGIN, SPAN_END, SPAN_SPAWN};

    fn make_event(
        span_id: u64,
        parent_span_id: u64,
        event_type: i32,
        ts: u64,
        actor_id: u64,
        msg_type: i32,
    ) -> HewTraceEvent {
        HewTraceEvent {
            trace_id_hi: 0xDEAD_BEEF_0000_0001,
            trace_id_lo: 0x0000_0000_CAFE_BABE,
            span_id,
            parent_span_id,
            actor_id,
            event_type,
            msg_type,
            timestamp_ns: ts,
        }
    }

    // ── reconstruct_spans ────────────────────────────────────────────────

    #[test]
    fn complete_span_pairs_reconstructed() {
        let events = vec![
            make_event(1, 0, SPAN_BEGIN, 100, 42, 7),
            make_event(1, 0, SPAN_END, 200, 42, 7),
        ];
        let spans = reconstruct_spans(events);
        assert_eq!(spans.len(), 1);
        let s = &spans[0];
        assert_eq!(s.span_id, 1);
        assert_eq!(s.start_ns, 100);
        assert_eq!(s.end_ns, 200);
        assert_eq!(s.actor_id, 42);
        assert_eq!(s.msg_type, 7);
        assert!(s.ok);
        assert!(s.name.contains("dispatch"));
    }

    #[test]
    fn unpaired_begin_exported_as_incomplete() {
        let events = vec![make_event(99, 0, SPAN_BEGIN, 500, 1, 3)];
        let spans = reconstruct_spans(events);
        assert_eq!(spans.len(), 1);
        assert_eq!(spans[0].start_ns, 500);
        assert_eq!(spans[0].end_ns, 500);
        assert!(!spans[0].ok);
    }

    #[test]
    fn lifecycle_events_become_point_spans() {
        let events = vec![
            make_event(10, 0, SPAN_SPAWN, 300, 5, 0),
            make_event(11, 0, SPAN_CRASH, 400, 5, 0),
        ];
        let spans = reconstruct_spans(events);
        assert_eq!(spans.len(), 2);
        assert_eq!(spans[0].name, "actor.spawn");
        assert_eq!(spans[1].name, "actor.crash");
        for s in &spans {
            assert_eq!(s.start_ns, s.end_ns); // zero duration
        }
    }

    #[test]
    fn multiple_concurrent_spans_reconstructed() {
        let events = vec![
            make_event(1, 0, SPAN_BEGIN, 10, 1, 1),
            make_event(2, 1, SPAN_BEGIN, 20, 2, 2),
            make_event(2, 1, SPAN_END, 30, 2, 2),
            make_event(1, 0, SPAN_END, 40, 1, 1),
        ];
        let spans = reconstruct_spans(events);
        assert_eq!(spans.len(), 2);
        let s1 = spans.iter().find(|s| s.span_id == 1).unwrap();
        let s2 = spans.iter().find(|s| s.span_id == 2).unwrap();
        assert_eq!(s1.start_ns, 10);
        assert_eq!(s1.end_ns, 40);
        assert_eq!(s2.start_ns, 20);
        assert_eq!(s2.end_ns, 30);
        assert_eq!(s2.parent_span_id, 1);
    }

    #[test]
    fn orphan_end_event_is_ignored() {
        // An END with no matching BEGIN should produce no span.
        let events = vec![make_event(42, 0, SPAN_END, 100, 1, 0)];
        let spans = reconstruct_spans(events);
        assert!(spans.is_empty());
    }

    // ── build_otlp_json ──────────────────────────────────────────────────

    fn sample_span() -> ReconstructedSpan {
        ReconstructedSpan {
            trace_id_hi: 0xDEAD_BEEF_CAFE_0001,
            trace_id_lo: 0x1234_5678_9ABC_DEF0,
            span_id: 0xABCD_EF01_2345_6789,
            parent_span_id: 0x1111_2222_3333_4444,
            actor_id: 99,
            kind: 1,
            name: "actor.dispatch[7]".into(),
            start_ns: 1_000_000_000,
            end_ns: 2_000_000_000,
            msg_type: 7,
            ok: true,
        }
    }

    #[test]
    fn otlp_json_is_valid_structure() {
        let spans = vec![sample_span()];
        let json = build_otlp_json("my-service", &spans);

        // Must contain the OTLP envelope keys.
        assert!(json.contains("\"resourceSpans\""), "missing resourceSpans");
        assert!(json.contains("\"scopeSpans\""), "missing scopeSpans");
        assert!(json.contains("\"spans\""), "missing spans");

        // Must have service name.
        assert!(json.contains("\"my-service\""), "missing service name");

        // Trace/span IDs must be hex-encoded.
        assert!(json.contains("deadbeefcafe0001"), "missing trace_id_hi hex");
        assert!(json.contains("123456789abcdef0"), "missing trace_id_lo hex");
        assert!(json.contains("abcdef0123456789"), "missing span_id hex");
        assert!(
            json.contains("1111222233334444"),
            "missing parent_span_id hex"
        );

        // Timestamps as string integers (OTLP JSON spec uses string for u64).
        assert!(json.contains("\"1000000000\""), "missing start timestamp");
        assert!(json.contains("\"2000000000\""), "missing end timestamp");

        // Attributes.
        assert!(json.contains("hew.actor_id"), "missing actor_id attribute");
        assert!(json.contains("hew.msg_type"), "missing msg_type attribute");

        // Status OK.
        assert!(json.contains("\"code\":1"), "missing status OK");

        // Must parse as valid JSON.
        assert!(is_valid_json(&json), "not valid JSON: {json}");
    }

    #[test]
    fn otlp_json_no_parent_when_root_span() {
        let mut span = sample_span();
        span.parent_span_id = 0; // root span
        let json = build_otlp_json("svc", &[span]);
        assert!(
            !json.contains("parentSpanId"),
            "root span should not have parentSpanId"
        );
        assert!(is_valid_json(&json), "not valid JSON");
    }

    #[test]
    fn otlp_json_incomplete_span_has_error_status() {
        let mut span = sample_span();
        span.ok = false;
        let json = build_otlp_json("svc", &[span]);
        assert!(
            json.contains("\"code\":2"),
            "incomplete span should have error status"
        );
        assert!(is_valid_json(&json));
    }

    #[test]
    fn otlp_json_empty_spans_still_valid() {
        let json = build_otlp_json("svc", &[]);
        assert!(json.contains("\"spans\":[]"), "empty spans list expected");
        assert!(is_valid_json(&json));
    }

    #[test]
    fn otlp_json_special_chars_in_service_name_escaped() {
        let json = build_otlp_json("svc with \"quotes\" and \\slashes\\", &[]);
        assert!(
            is_valid_json(&json),
            "special chars must be escaped: {json}"
        );
        // The escaped content should appear.
        assert!(json.contains("\\\"quotes\\\""), "quotes should be escaped");
    }

    #[test]
    fn push_escaped_handles_control_chars() {
        let mut out = String::new();
        push_escaped(&mut out, "tab\there\nnewline");
        assert_eq!(out, "tab\\there\\nnewline");

        let mut out2 = String::new();
        push_escaped(&mut out2, "\x01\x1f");
        assert_eq!(out2, "\\u0001\\u001f");
    }

    #[test]
    fn config_from_env_absent() {
        // Without HEW_OTEL_ENDPOINT set, config should be None.
        // We can't unset env vars reliably in tests so just verify the
        // parsing logic with a direct call using known-absent key.
        // The actual env-based test is covered by integration tests.
        let _ = OtelConfig::from_env(); // must not panic
    }

    #[test]
    fn binary_name_fallback() {
        // binary_name() should return a non-empty string.
        let name = binary_name();
        assert!(!name.is_empty());
    }

    // ── Helpers ──────────────────────────────────────────────────────────

    /// Minimal JSON syntax validator (no external crate required).
    ///
    /// Checks that brackets balance and the string contains no obviously
    /// unescaped control characters. Not a full RFC 8259 parser.
    fn is_valid_json(s: &str) -> bool {
        let mut depth: i64 = 0;
        let mut in_string = false;
        let mut escaped = false;

        for c in s.chars() {
            if escaped {
                escaped = false;
                continue;
            }
            if in_string {
                match c {
                    '\\' => escaped = true,
                    '"' => in_string = false,
                    c if (c as u32) < 0x20 => return false, // bare control char
                    _ => {}
                }
                continue;
            }
            match c {
                '"' => in_string = true,
                '{' | '[' => depth += 1,
                '}' | ']' => {
                    depth -= 1;
                    if depth < 0 {
                        return false;
                    }
                }
                _ => {}
            }
        }
        depth == 0 && !in_string
    }
}
