use hew_observe_protocol::{
    decode_envelope, envelope_json_raw, DecodeError, Envelope, Metrics, RoutingSnapshot,
    TraceEvent, OBSERVE_SCHEMA_VERSION,
};
use serde_json::json;

#[test]
fn max_width_integers_round_trip_without_loss() {
    let body = json!({
        "timestamp_secs": u64::MAX,
        "tasks_spawned": u64::MAX,
        "tasks_completed": u64::MAX,
        "steals": u64::MAX,
        "messages_sent": u64::MAX,
        "messages_received": u64::MAX,
        "active_workers": u64::MAX,
        "alloc_count": u64::MAX,
        "dealloc_count": u64::MAX,
        "bytes_allocated": u64::MAX,
        "bytes_freed": u64::MAX,
        "bytes_live": u64::MAX,
        "peak_bytes_live": u64::MAX,
        "tcp_bytes_read": u64::MAX,
        "tcp_bytes_written": u64::MAX,
        "tcp_accept_count": u64::MAX,
        "tcp_connect_count": u64::MAX,
        "tcp_error_count": u64::MAX,
    });
    let decoded: Metrics = serde_json::from_value(body).expect("decode max-width metrics");
    assert_eq!(decoded.timestamp_secs, u64::MAX);
    assert_eq!(decoded.bytes_live, u64::MAX);
    let encoded = serde_json::to_value(decoded).expect("encode max-width metrics");
    assert_eq!(encoded["messages_sent"], u64::MAX);
}

#[test]
fn trace_required_nulls_and_unknown_values_are_distinct() {
    let trace = json!({
        "trace_id": "0123456789abcdef0123456789abcdef",
        "span_id": u64::MAX,
        "parent_span_id": 0,
        "actor_id": u64::MAX,
        "actor_type_id": 0,
        "actor_type": null,
        "event_type": "future_event",
        "msg_type": -2_147_483_648,
        "timestamp_ns": u64::MAX,
        "handler_name": null,
        "future_property": {"kept_on_the_wire": true}
    });
    let decoded: TraceEvent = serde_json::from_value(trace.clone()).expect("decode open taxonomy");
    assert_eq!(decoded.event_type, "future_event");
    assert!(!decoded.is_actionable());
    assert_eq!(decoded.actor_type, None);
    assert_eq!(decoded.span_id, u64::MAX);

    let mut missing = trace;
    missing
        .as_object_mut()
        .expect("object")
        .remove("handler_name");
    let error = serde_json::from_value::<TraceEvent>(missing)
        .expect_err("omitted required nullable key must fail");
    assert!(error.to_string().contains("handler_name"), "{error}");
}

#[test]
fn actionable_trace_taxonomy_is_generated_from_the_contract() {
    let trace: TraceEvent = serde_json::from_value(json!({
        "trace_id": "0123456789abcdef0123456789abcdef",
        "span_id": 1,
        "parent_span_id": 0,
        "actor_id": 2,
        "actor_type_id": 0,
        "actor_type": null,
        "event_type": "lambda_spawned",
        "msg_type": 3,
        "timestamp_ns": 4,
        "handler_name": null
    }))
    .expect("decode actionable trace");
    assert!(trace.is_actionable());
}

#[test]
fn envelope_requires_shape_but_accepts_additive_properties() {
    let valid = json!({
        "schema_version": OBSERVE_SCHEMA_VERSION,
        "data": [],
        "future_envelope_property": true
    });
    let decoded: Envelope<Vec<TraceEvent>> =
        serde_json::from_value(valid).expect("additive envelope property must be ignored");
    assert_eq!(decoded.schema_version, OBSERVE_SCHEMA_VERSION);

    let missing_data = json!({"schema_version": OBSERVE_SCHEMA_VERSION});
    assert!(serde_json::from_value::<Envelope<Vec<TraceEvent>>>(missing_data).is_err());
}

#[test]
fn routing_model_matches_current_full_identity_wire_shape() {
    let routing: RoutingSnapshot = serde_json::from_value(json!({
        "local_node_id": "00112233-4455-6677-8899-aabbccddeeff",
        "local_route_slot": 7,
        "session_incarnation": 11,
        "routes": [{
            "node_id": "ffeeddcc-bbaa-9988-7766-554433221100",
            "route_slot": 9,
            "session_incarnation": 12,
            "conn_id": -1
        }]
    }))
    .expect("decode current routing shape");
    assert_eq!(routing.local_route_slot, 7);
    assert_eq!(routing.routes[0].session_incarnation, 12);
}

#[test]
fn raw_envelope_uses_generated_version() {
    assert_eq!(
        envelope_json_raw("[]"),
        r#"{"schema_version":"v0.5","data":[]}"#
    );
}

#[test]
fn rust_codec_rejects_wrong_version() {
    let error = decode_envelope::<Vec<TraceEvent>>(br#"{"schema_version":"v9","data":[]}"#)
        .expect_err("wrong version must fail");
    assert!(matches!(error, DecodeError::SchemaVersion { .. }));
}
