use hew_observe_protocol::{
    decode_envelope, envelope_json_raw, DecodeError, Envelope, Metrics, RoutingSnapshot,
    TraceBatch, TraceEvent, OBSERVE_SCHEMA_VERSION,
};
use serde_json::json;

#[test]
fn max_width_integers_round_trip_without_loss() {
    let body = json!({
        "timestamp_secs": u64::MAX.to_string(),
        "tasks_spawned": u64::MAX.to_string(),
        "tasks_completed": u64::MAX.to_string(),
        "steals": u64::MAX.to_string(),
        "messages_sent": u64::MAX.to_string(),
        "messages_received": u64::MAX.to_string(),
        "active_workers": u64::MAX.to_string(),
        "alloc_count": u64::MAX.to_string(),
        "dealloc_count": u64::MAX.to_string(),
        "bytes_allocated": u64::MAX.to_string(),
        "bytes_freed": u64::MAX.to_string(),
        "bytes_live": u64::MAX.to_string(),
        "peak_bytes_live": u64::MAX.to_string(),
        "tcp_bytes_read": u64::MAX.to_string(),
        "tcp_bytes_written": u64::MAX.to_string(),
        "tcp_accept_count": u64::MAX.to_string(),
        "tcp_connect_count": u64::MAX.to_string(),
        "tcp_error_count": u64::MAX.to_string(),
    });
    let decoded: Metrics = serde_json::from_value(body).expect("decode max-width metrics");
    assert_eq!(decoded.timestamp_secs, u64::MAX);
    assert_eq!(decoded.bytes_live, u64::MAX);
    let encoded = serde_json::to_value(decoded).expect("encode max-width metrics");
    assert_eq!(encoded["messages_sent"], u64::MAX.to_string());
    assert!(
        serde_json::from_value::<Metrics>(json!({
            "timestamp_secs": 0,
            "tasks_spawned": "0", "tasks_completed": "0", "steals": "0",
            "messages_sent": "0", "messages_received": "0", "active_workers": "0",
            "alloc_count": "0", "dealloc_count": "0", "bytes_allocated": "0",
            "bytes_freed": "0", "bytes_live": "0", "peak_bytes_live": "0",
            "tcp_bytes_read": "0", "tcp_bytes_written": "0", "tcp_accept_count": "0",
            "tcp_connect_count": "0", "tcp_error_count": "0"
        }))
        .is_err(),
        "JSON numbers are not valid v1 64-bit wire values"
    );
}

#[test]
fn trace_required_nulls_and_unknown_values_are_distinct() {
    let trace = json!({
        "sequence": u64::MAX.to_string(),
        "trace_id": "0123456789abcdef0123456789abcdef",
        "span_id": u64::MAX.to_string(),
        "parent_span_id": "0",
        "subject": null,
        "source_attribution": "remote_unknown",
        "source_actor": null,
        "destination_attribution": "not_applicable",
        "destination_actor": null,
        "event_type": "future_event",
        "msg_type": -2_147_483_648,
        "timestamp_ns": u64::MAX.to_string(),
        "handler_name": null,
        "future_property": {"accepted": true}
    });
    let decoded: TraceEvent = serde_json::from_value(trace.clone()).expect("decode open taxonomy");
    assert_eq!(decoded.event_type, "future_event");
    assert!(!decoded.is_actionable());
    assert!(decoded.source_actor.is_none());
    assert_eq!(decoded.sequence, u64::MAX);
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
        "sequence": "1",
        "trace_id": "0123456789abcdef0123456789abcdef",
        "span_id": "1",
        "parent_span_id": "0",
        "subject": null,
        "source_attribution": "not_applicable",
        "source_actor": null,
        "destination_attribution": "not_applicable",
        "destination_actor": null,
        "event_type": "lambda_spawned",
        "msg_type": 3,
        "timestamp_ns": "4",
        "handler_name": null
    }))
    .expect("decode actionable trace");
    assert!(trace.is_actionable());
}

#[test]
fn envelope_requires_shape_but_accepts_additive_properties() {
    let valid = json!({
        "schema_version": OBSERVE_SCHEMA_VERSION,
        "data": {
            "producer_instance_id": "00112233445566778899aabbccddeeff",
            "stream_id": "ffeeddccbbaa99887766554433221100",
            "oldest_sequence": "1",
            "next_sequence": "1",
            "next_cursor": "0",
            "has_more": false,
            "overrun": null,
            "tracing_enabled": true,
            "sampling_mode": "all",
            "events_recorded_total": "0",
            "events_overwritten_total": "0",
            "events_sampled_out_total": "0",
            "batch_source_monotonic_ns": "5",
            "clock_anchor": {
                "monotonic_ns": "5",
                "unix_epoch_ns": "6",
                "uncertainty_ns": "1"
            },
            "events": []
        },
        "future_envelope_property": true
    });
    let decoded: Envelope<TraceBatch> =
        serde_json::from_value(valid).expect("additive envelope property must be ignored");
    assert_eq!(decoded.schema_version, OBSERVE_SCHEMA_VERSION);
    assert_eq!(decoded.data.next_cursor, 0);

    let missing_data = json!({"schema_version": OBSERVE_SCHEMA_VERSION});
    assert!(serde_json::from_value::<Envelope<TraceBatch>>(missing_data).is_err());
}

#[test]
fn trace_batch_max_width_integers_round_trip_without_loss() {
    let max = u64::MAX.to_string();
    let batch: TraceBatch = serde_json::from_value(json!({
        "producer_instance_id": "00112233445566778899aabbccddeeff",
        "stream_id": "ffeeddccbbaa99887766554433221100",
        "oldest_sequence": max,
        "next_sequence": u64::MAX.to_string(),
        "next_cursor": u64::MAX.to_string(),
        "has_more": true,
        "overrun": {
            "first_missing_sequence": u64::MAX.to_string(),
            "last_missing_sequence": u64::MAX.to_string(),
            "count": u64::MAX.to_string()
        },
        "tracing_enabled": true,
        "sampling_mode": "future_sampler",
        "events_recorded_total": u64::MAX.to_string(),
        "events_overwritten_total": u64::MAX.to_string(),
        "events_sampled_out_total": u64::MAX.to_string(),
        "batch_source_monotonic_ns": u64::MAX.to_string(),
        "clock_anchor": {
            "monotonic_ns": u64::MAX.to_string(),
            "unix_epoch_ns": u64::MAX.to_string(),
            "uncertainty_ns": u64::MAX.to_string()
        },
        "events": [{
            "sequence": u64::MAX.to_string(),
            "trace_id": "0123456789abcdef0123456789abcdef",
            "span_id": u64::MAX.to_string(),
            "parent_span_id": u64::MAX.to_string(),
            "subject": {
                "kind": "actor",
                "id": u64::MAX.to_string(),
                "actor": {
                    "actor_id": u64::MAX.to_string(),
                    "spawn_serial": u64::MAX.to_string(),
                    "actor_type": null
                }
            },
            "source_attribution": "local_actor",
            "source_actor": {
                "actor_id": u64::MAX.to_string(),
                "spawn_serial": u64::MAX.to_string(),
                "actor_type": "Source"
            },
            "destination_attribution": "local_actor",
            "destination_actor": {
                "actor_id": u64::MAX.to_string(),
                "spawn_serial": null,
                "actor_type": "Destination"
            },
            "event_type": "send",
            "msg_type": i32::MAX,
            "timestamp_ns": u64::MAX.to_string(),
            "handler_name": "Destination.receive"
        }]
    }))
    .expect("decode max-width trace batch");
    assert_eq!(batch.events_recorded_total, u64::MAX);
    assert_eq!(batch.events[0].sequence, u64::MAX);
    assert_eq!(
        batch.events[0]
            .source_actor
            .as_ref()
            .expect("source")
            .spawn_serial,
        Some(u64::MAX)
    );

    let encoded = serde_json::to_value(batch).expect("encode max-width trace batch");
    assert_eq!(encoded["next_cursor"], u64::MAX.to_string());
    assert_eq!(
        encoded["events"][0]["source_actor"]["actor_id"],
        u64::MAX.to_string()
    );
}

#[test]
fn hex128_identifiers_reject_uppercase_and_wrong_length() {
    let valid = json!({
        "error": "trace_stream_mismatch",
        "requested_stream_id": "00112233445566778899aabbccddeeff",
        "current_stream_id": "ffeeddccbbaa99887766554433221100",
        "current_producer_instance_id": "0123456789abcdef0123456789abcdef"
    });
    serde_json::from_value::<hew_observe_protocol::TraceStreamMismatch>(valid.clone())
        .expect("lowercase 128-bit identifiers are valid");

    let mut uppercase = valid.clone();
    uppercase["current_stream_id"] = json!("FFEEDDCCBBAA99887766554433221100");
    assert!(
        serde_json::from_value::<hew_observe_protocol::TraceStreamMismatch>(uppercase).is_err()
    );

    let mut constructed =
        serde_json::from_value::<hew_observe_protocol::TraceStreamMismatch>(valid.clone())
            .expect("valid mismatch model");
    constructed.current_stream_id = "FFEEDDCCBBAA99887766554433221100".to_owned();
    assert!(serde_json::to_value(constructed).is_err());

    let mut short = valid;
    short["current_stream_id"] = json!("00");
    assert!(serde_json::from_value::<hew_observe_protocol::TraceStreamMismatch>(short).is_err());
}

#[test]
fn actor_identity_nullable_fields_must_be_present() {
    let identity = json!({
        "actor_id": "7",
        "spawn_serial": null,
        "actor_type": null
    });
    serde_json::from_value::<hew_observe_protocol::ActorIdentity>(identity.clone())
        .expect("required nullable fields may be null");

    let mut missing = identity;
    missing
        .as_object_mut()
        .expect("object")
        .remove("spawn_serial");
    assert!(serde_json::from_value::<hew_observe_protocol::ActorIdentity>(missing).is_err());
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
        r#"{"schema_version":"v1","data":[]}"#
    );
}

#[test]
fn rust_codec_rejects_wrong_version() {
    let error = decode_envelope::<Vec<TraceEvent>>(br#"{"schema_version":"v9","data":[]}"#)
        .expect_err("wrong version must fail");
    assert!(matches!(error, DecodeError::SchemaVersion { .. }));
}
