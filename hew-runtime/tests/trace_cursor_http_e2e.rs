//! End-to-end tests for the non-destructive Observe trace cursor HTTP surface.

#![cfg(all(feature = "profiler", not(target_arch = "wasm32")))]

use std::ffi::c_void;
use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream};
use std::sync::atomic::{AtomicBool, AtomicPtr, Ordering};
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::{Duration, Instant};

use hew_runtime::actor::{hew_actor_send, HewActor};
use hew_runtime::execution_context::{set_current_context, HewExecutionContext};
use hew_runtime::profiler::actor_registry::{register_dispatch_type, register_handler_name};
use hew_runtime::profiler::{run_tcp_with_listener, ProfilerContext};
use hew_runtime::tracing::{
    hew_trace_enable, hew_trace_lifecycle, hew_trace_reset, HewTraceContext, SPAN_SPAWN, SPAN_STOP,
};
use hew_runtime_testkit::TestActor;

const TRACE_RETENTION_CAPACITY: u64 = 16_384;
static LOCAL_SEND_TARGET: AtomicPtr<HewActor> = AtomicPtr::new(std::ptr::null_mut());
static LOCAL_SEND_DONE: AtomicBool = AtomicBool::new(false);

fn test_trace_context() -> HewTraceContext {
    HewTraceContext {
        trace_id_hi: 1,
        trace_id_lo: 2,
        span_id: 3,
        parent_span_id: 0,
        flags: 1,
    }
}

unsafe extern "C-unwind" fn teardown_target_dispatch(
    _ctx: *mut HewExecutionContext,
    _state: *mut c_void,
    _msg_type: i32,
    _data: *mut c_void,
    _data_size: usize,
    _borrow_mode: i32,
) -> *mut c_void {
    std::ptr::null_mut()
}

unsafe extern "C-unwind" fn local_source_dispatch(
    _ctx: *mut HewExecutionContext,
    _state: *mut c_void,
    msg_type: i32,
    _data: *mut c_void,
    _data_size: usize,
    _borrow_mode: i32,
) -> *mut c_void {
    if msg_type == 1 {
        let target = LOCAL_SEND_TARGET.load(Ordering::Acquire);
        if !target.is_null() {
            // SAFETY: the test keeps the target wrapper alive until this
            // dispatch publishes LOCAL_SEND_DONE after the synchronous send.
            unsafe { hew_actor_send(target, 77, std::ptr::null_mut(), 0) };
            LOCAL_SEND_DONE.store(true, Ordering::Release);
        }
    }
    std::ptr::null_mut()
}

struct HttpResponse {
    status: u16,
    headers: String,
    body: String,
}

fn http_get(addr: std::net::SocketAddr, target: &str) -> HttpResponse {
    let mut stream = TcpStream::connect(addr).expect("connect to profiler server");
    write!(
        stream,
        "GET {target} HTTP/1.1\r\nHost: {addr}\r\nConnection: close\r\n\r\n"
    )
    .expect("write HTTP request");
    let mut wire = Vec::new();
    stream.read_to_end(&mut wire).expect("read HTTP response");
    let wire = String::from_utf8(wire).expect("profiler response must be UTF-8");
    let (head, body) = wire
        .split_once("\r\n\r\n")
        .expect("HTTP response must contain a header terminator");
    let status = head
        .lines()
        .next()
        .and_then(|line| line.split_whitespace().nth(1))
        .and_then(|status| status.parse().ok())
        .expect("HTTP response must contain a numeric status");
    HttpResponse {
        status,
        headers: head.to_owned(),
        body: body.to_owned(),
    }
}

fn json_get(addr: std::net::SocketAddr, target: &str) -> (HttpResponse, serde_json::Value) {
    let response = http_get(addr, target);
    let body = serde_json::from_str(&response.body).unwrap_or_else(|error| {
        panic!("{target} returned invalid JSON: {error}\n{}", response.body)
    });
    (response, body)
}

fn string_u64(value: &serde_json::Value) -> u64 {
    value
        .as_str()
        .and_then(|value| value.parse().ok())
        .unwrap_or_else(|| panic!("expected canonical decimal u64 string, got {value}"))
}

#[test]
fn trace_http_cursors_are_independent_and_report_overflow_and_reset() {
    hew_trace_reset();
    let mut execution_context = HewExecutionContext {
        trace: test_trace_context(),
        ..HewExecutionContext::default()
    };
    let previous_context = set_current_context(&raw mut execution_context);
    hew_trace_enable(1);

    let listener = TcpListener::bind("127.0.0.1:0").expect("bind profiler listener");
    let addr = listener.local_addr().expect("profiler listener address");
    let ctx = Arc::new(ProfilerContext {
        ring: Arc::new(Mutex::new(
            hew_runtime::profiler::metrics::MetricsRing::new(),
        )),
        cluster: std::ptr::null_mut(),
        connmgr: std::ptr::null_mut(),
        routing: std::ptr::null_mut(),
    });
    thread::Builder::new()
        .name("trace-cursor-http-e2e-server".into())
        .spawn(move || run_tcp_with_listener(listener, ctx))
        .expect("spawn profiler server");

    for (actor_id, event_type) in [(101, SPAN_SPAWN), (102, SPAN_SPAWN), (101, SPAN_STOP)] {
        hew_trace_lifecycle(actor_id, event_type);
    }

    let (first_response, first) = json_get(addr, "/api/traces?limit=2");
    assert_eq!(first_response.status, 200);
    assert!(first_response
        .headers
        .lines()
        .any(|line| line.eq_ignore_ascii_case("x-hew-schema-version: v1")));
    assert_eq!(first["schema_version"], "v1");
    let first = &first["data"];
    let producer_instance_id = first["producer_instance_id"]
        .as_str()
        .expect("producer_instance_id");
    let stream_id = first["stream_id"].as_str().expect("stream_id");
    assert_eq!(producer_instance_id.len(), 32);
    assert_eq!(stream_id.len(), 32);
    assert!(producer_instance_id
        .bytes()
        .all(|byte| byte.is_ascii_digit() || (b'a'..=b'f').contains(&byte)));
    assert!(stream_id
        .bytes()
        .all(|byte| byte.is_ascii_digit() || (b'a'..=b'f').contains(&byte)));
    assert_eq!(first["events"].as_array().expect("events").len(), 2);
    assert_eq!(first["events"][0]["sequence"], "1");
    assert_eq!(first["events"][1]["sequence"], "2");
    assert_eq!(first["next_cursor"], "2");
    assert_eq!(first["has_more"], true);
    assert!(first["overrun"].is_null());
    assert_eq!(first["events"][0]["subject"]["kind"], "actor");
    assert_eq!(first["events"][0]["subject"]["actor"]["actor_id"], "101");
    assert_eq!(first["events"][0]["source_attribution"], "not_applicable");
    assert_eq!(
        first["events"][0]["destination_attribution"],
        "not_applicable"
    );
    assert!(
        string_u64(&first["batch_source_monotonic_ns"])
            >= string_u64(&first["clock_anchor"]["monotonic_ns"])
    );
    assert!(string_u64(&first["clock_anchor"]["unix_epoch_ns"]) > 0);

    // Two independent observers at the same stateless cursor see byte-for-byte
    // equivalent event pages; neither request removes records from the other.
    let cursor_target = format!("/api/traces?stream_id={stream_id}&after=0&limit=2");
    let (_, observer_a) = json_get(addr, &cursor_target);
    let (_, observer_b) = json_get(addr, &cursor_target);
    assert_eq!(observer_a["data"]["events"], observer_b["data"]["events"]);
    assert_eq!(observer_a["data"]["next_cursor"], "2");
    assert_eq!(observer_b["data"]["next_cursor"], "2");

    let (_, tail) = json_get(
        addr,
        &format!("/api/traces?stream_id={stream_id}&after=2&limit=256"),
    );
    assert_eq!(tail["data"]["events"].as_array().expect("events").len(), 1);
    assert_eq!(tail["data"]["events"][0]["sequence"], "3");
    assert_eq!(tail["data"]["has_more"], false);

    let (_, caught_up) = json_get(addr, &format!("/api/traces?stream_id={stream_id}&after=3"));
    assert!(caught_up["data"]["events"]
        .as_array()
        .expect("events")
        .is_empty());
    assert_eq!(caught_up["data"]["next_cursor"], "3");
    assert_eq!(caught_up["data"]["has_more"], false);

    let (future_response, future) =
        json_get(addr, &format!("/api/traces?stream_id={stream_id}&after=4"));
    assert_eq!(future_response.status, 400);
    assert_eq!(future["schema_version"], "v1");
    assert_eq!(future["data"]["error"], "future_trace_cursor");

    for malformed in [
        "/api/traces?after=0",
        "/api/traces?stream_id=0123456789ABCDEF0123456789ABCDEF&after=0",
        "/api/traces?limit=257",
    ] {
        let (response, body) = json_get(addr, malformed);
        assert_eq!(response.status, 400, "target {malformed}");
        assert_eq!(body["data"]["error"], "invalid_trace_cursor");
    }

    // Reset creates a new stream while preserving producer-process identity.
    hew_trace_reset();
    let (mismatch_response, mismatch) =
        json_get(addr, &format!("/api/traces?stream_id={stream_id}&after=0"));
    assert_eq!(mismatch_response.status, 409);
    assert_eq!(mismatch["data"]["error"], "trace_stream_mismatch");
    assert_eq!(mismatch["data"]["requested_stream_id"], stream_id);
    assert_ne!(mismatch["data"]["current_stream_id"], stream_id);
    assert_eq!(
        mismatch["data"]["current_producer_instance_id"],
        producer_instance_id
    );

    // A real actor-to-actor send captures both endpoints, the destination type
    // and handler synchronously. Those facts remain exportable after both
    // actors are freed; HTTP never performs a racy actor-id lookup at read time.
    let installed_test_context = set_current_context(std::ptr::null_mut());
    hew_trace_enable(1);
    register_dispatch_type(Some(teardown_target_dispatch), "TeardownTarget".to_owned());
    register_handler_name(
        Some(teardown_target_dispatch),
        77,
        "TeardownTarget.handle".to_owned(),
    );
    register_dispatch_type(Some(local_source_dispatch), "LocalSource".to_owned());
    let target = TestActor::spawn(teardown_target_dispatch);
    let source = TestActor::spawn(local_source_dispatch);
    // SAFETY: target owns a live actor until it is explicitly dropped below.
    let target_id = unsafe { (*target.as_ptr()).id };
    // SAFETY: same live-actor ownership invariant as target_id.
    let target_spawn_serial = unsafe { (*target.as_ptr()).spawn_serial };
    // SAFETY: source owns a live actor until it is explicitly dropped below.
    let source_id = unsafe { (*source.as_ptr()).id };
    // SAFETY: same live-actor ownership invariant as source_id.
    let source_spawn_serial = unsafe { (*source.as_ptr()).spawn_serial };
    LOCAL_SEND_DONE.store(false, Ordering::Release);
    LOCAL_SEND_TARGET.store(target.as_ptr(), Ordering::Release);
    source.send_empty(1);
    let deadline = Instant::now() + Duration::from_secs(5);
    while !LOCAL_SEND_DONE.load(Ordering::Acquire) {
        assert!(
            Instant::now() < deadline,
            "local actor send did not complete"
        );
        thread::sleep(Duration::from_millis(1));
    }
    LOCAL_SEND_TARGET.store(std::ptr::null_mut(), Ordering::Release);
    drop(target);
    drop(source);

    let (_, after_teardown) = json_get(addr, "/api/traces");
    let send = after_teardown["data"]["events"]
        .as_array()
        .expect("events")
        .iter()
        .find(|event| event["event_type"] == "send" && event["msg_type"] == 77)
        .expect("send event must survive destination teardown");
    assert_eq!(send["subject"], serde_json::Value::Null);
    assert_eq!(send["source_attribution"], "local_actor");
    assert_eq!(send["source_actor"]["actor_id"], source_id.to_string());
    assert_eq!(
        send["source_actor"]["spawn_serial"],
        source_spawn_serial.to_string()
    );
    assert_eq!(send["source_actor"]["actor_type"], "LocalSource");
    assert_eq!(send["destination_attribution"], "local_actor");
    assert_eq!(send["destination_actor"]["actor_id"], target_id.to_string());
    assert_eq!(
        send["destination_actor"]["spawn_serial"],
        target_spawn_serial.to_string()
    );
    assert_eq!(send["destination_actor"]["actor_type"], "TeardownTarget");
    assert_eq!(send["handler_name"], "TeardownTarget.handle");

    hew_trace_reset();
    let _ = set_current_context(installed_test_context);

    // Attach before producing enough events to overflow bounded retention, so
    // the subsequent cursor has a known exact missing range.
    execution_context.trace = test_trace_context();
    hew_trace_enable(1);
    let (_, empty) = json_get(addr, "/api/traces");
    let overflow_stream = empty["data"]["stream_id"]
        .as_str()
        .expect("overflow stream id")
        .to_owned();
    assert_eq!(empty["data"]["next_sequence"], "1");
    for actor_id in 0..TRACE_RETENTION_CAPACITY + 7 {
        hew_trace_lifecycle(actor_id + 1, SPAN_SPAWN);
    }

    let (_, overflow) = json_get(
        addr,
        &format!("/api/traces?stream_id={overflow_stream}&after=0&limit=1"),
    );
    let overflow = &overflow["data"];
    assert_eq!(overflow["events_recorded_total"], "16391");
    assert_eq!(overflow["events_overwritten_total"], "7");
    assert_eq!(overflow["oldest_sequence"], "8");
    assert_eq!(overflow["overrun"]["first_missing_sequence"], "1");
    assert_eq!(overflow["overrun"]["last_missing_sequence"], "7");
    assert_eq!(overflow["overrun"]["count"], "7");
    assert_eq!(overflow["events"][0]["sequence"], "8");
    assert_eq!(overflow["has_more"], true);

    hew_trace_reset();
    let _ = set_current_context(previous_context);
}
