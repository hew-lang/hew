//! Round-trip and byte-shape tests for the CBOR wire envelope types.
//!
//! The gate is the CDDL shape, not just same-type round trips: integer keys,
//! explicit `frame_type`, and `bstr` payload fields.

use ciborium::value::{Integer, Value};
use hew_runtime::cluster::{GOSSIP_REGISTRY_ADD, GOSSIP_REGISTRY_REMOVE};
use hew_runtime::envelope::{
    decode_control_frame, decode_envelope_frame, decode_link_down_payload, decode_link_req_payload,
    decode_monitor_down_payload, decode_monitor_req_payload, decode_registry_gossip_payload,
    decode_swim_payload, decode_wire_frame, encode_control_frame, encode_envelope_frame,
    encode_link_down_payload, encode_link_req_payload, encode_monitor_down_payload,
    encode_monitor_req_payload, encode_registry_gossip_payload, encode_swim_payload, ControlFrame,
    DecodeError, EnvelopeFrame, LinkReqPayload, MonitorDownPayload, MonitorPayloadError,
    MonitorReqPayload, NodeSessionIdentity, RegistryGossipPayload, RegistryGossipPayloadError,
    SwimControlPayload, SwimGossipEntry, SwimPayloadError, WireFrame, CTRL_DEMONITOR,
    CTRL_LINK_DOWN, CTRL_LINK_REQ, CTRL_MONITOR_DOWN, CTRL_MONITOR_REQ, CTRL_REGISTRY_GOSSIP,
    CTRL_UNLINK, FRAME_TYPE_CONTROL, FRAME_TYPE_ENVELOPE, MAX_LINK_PAYLOAD_BYTES,
    MAX_MONITOR_PAYLOAD_BYTES, MAX_REGISTRY_GOSSIP_NAME_BYTES, MAX_REGISTRY_GOSSIP_PAYLOAD_BYTES,
    MAX_SWIM_GOSSIP_ENTRIES, MAX_SWIM_PAYLOAD_BYTES, REGISTRY_GOSSIP_OP_ADD,
    REGISTRY_GOSSIP_OP_REMOVE, WIRE_VERSION,
};
use hew_runtime::node_identity::{Location, NodeId};

fn to_cbor<T: serde::Serialize>(value: &T) -> Vec<u8> {
    let mut buf = Vec::new();
    ciborium::ser::into_writer(value, &mut buf).expect("ciborium serialisation failed");
    buf
}

fn from_cbor<T: serde::de::DeserializeOwned>(buf: &[u8]) -> T {
    ciborium::de::from_reader(buf).expect("ciborium deserialisation failed")
}

fn decode_value(buf: &[u8]) -> Value {
    ciborium::de::from_reader(buf).expect("ciborium value deserialisation failed")
}

fn value_to_cbor(value: &Value) -> Vec<u8> {
    to_cbor(value)
}

fn int<T: Into<Integer>>(value: T) -> Value {
    Value::Integer(value.into())
}

fn integer_to_i128(integer: Integer) -> i128 {
    integer.into()
}

fn map_entries(value: &Value) -> &[(Value, Value)] {
    match value {
        Value::Map(entries) => entries,
        other => panic!("expected top-level map, got {other:?}"),
    }
}

fn find_field(entries: &[(Value, Value)], key: u64) -> &Value {
    let mut found = None;
    for (entry_key, entry_value) in entries {
        if matches!(entry_key, Value::Integer(integer) if integer_to_i128(*integer) == i128::from(key))
        {
            assert!(found.is_none(), "duplicate key {key} in decoded value");
            found = Some(entry_value);
        }
    }
    found.unwrap_or_else(|| panic!("missing key {key} in decoded value"))
}

fn assert_integer_keys_and_order(entries: &[(Value, Value)], expected_keys: &[u64]) {
    let actual_keys: Vec<u64> = entries
        .iter()
        .map(|(key, _)| match key {
            Value::Integer(integer) => u64::try_from(integer_to_i128(*integer))
                .expect("key should be non-negative and fit u64"),
            other => panic!("expected integer map key, got {other:?}"),
        })
        .collect();
    assert_eq!(actual_keys, expected_keys);
}

fn assert_integer_value(value: &Value, expected: i128) {
    match value {
        Value::Integer(integer) => assert_eq!(integer_to_i128(*integer), expected),
        other => panic!("expected integer value {expected}, got {other:?}"),
    }
}

fn assert_bytes_value(value: &Value, expected: &[u8]) {
    match value {
        Value::Bytes(bytes) => assert_eq!(bytes, expected),
        Value::Array(_) => panic!("payload encoded as CBOR array, expected bstr"),
        other => panic!("expected CBOR bstr payload, got {other:?}"),
    }
}

fn assert_text_value(value: &Value, expected: &str) {
    match value {
        Value::Text(text) => assert_eq!(text, expected),
        other => panic!("expected CBOR text payload, got {other:?}"),
    }
}

fn node_id(seed: u8) -> NodeId {
    assert_ne!(seed, 0, "test node IDs must be non-zero");
    NodeId::from_bytes([seed; 16])
}

fn location(node_seed: u8, slot: u64, session_incarnation: u32) -> Location {
    Location::new(node_id(node_seed), slot, session_incarnation)
        .expect("test Location must have non-zero slot and session")
}

fn node_session(node_seed: u8, session_incarnation: u32) -> NodeSessionIdentity {
    assert_ne!(
        session_incarnation, 0,
        "test node sessions must be non-zero"
    );
    NodeSessionIdentity {
        node_id: node_id(node_seed),
        session_incarnation,
    }
}

fn raw_location_value(node: [u8; 16], slot: u64, session_incarnation: u64) -> Value {
    Value::Map(vec![
        (int(1u64), Value::Bytes(node.to_vec())),
        (int(2u64), int(slot)),
        (int(3u64), int(session_incarnation)),
    ])
}

fn location_value(location: Location) -> Value {
    raw_location_value(
        location.node().to_bytes(),
        location.slot(),
        u64::from(location.incarnation()),
    )
}

fn raw_node_session_value(node: [u8; 16], session_incarnation: u64) -> Value {
    Value::Map(vec![
        (int(1u64), Value::Bytes(node.to_vec())),
        (int(2u64), int(session_incarnation)),
    ])
}

fn node_session_value(identity: NodeSessionIdentity) -> Value {
    raw_node_session_value(
        identity.node_id.to_bytes(),
        u64::from(identity.session_incarnation),
    )
}

fn down_reason_value(reason: i32) -> Value {
    Value::Map(vec![(int(1u64), int(1u8)), (int(2u64), int(reason))])
}

fn assert_location_value(value: &Value, expected: Location) {
    let entries = map_entries(value);
    assert_eq!(entries.len(), 3);
    assert_integer_keys_and_order(entries, &[1, 2, 3]);
    assert_bytes_value(find_field(entries, 1), &expected.node().to_bytes());
    assert_integer_value(find_field(entries, 2), i128::from(expected.slot()));
    assert_integer_value(find_field(entries, 3), i128::from(expected.incarnation()));
}

fn assert_optional_location_value(value: &Value, expected: Option<Location>) {
    match expected {
        Some(location) => assert_location_value(value, location),
        None => assert!(
            matches!(value, Value::Null),
            "expected CBOR nil, got {value:?}"
        ),
    }
}

fn assert_node_session_value(value: &Value, expected: NodeSessionIdentity) {
    let entries = map_entries(value);
    assert_eq!(entries.len(), 2);
    assert_integer_keys_and_order(entries, &[1, 2]);
    assert_bytes_value(find_field(entries, 1), &expected.node_id.to_bytes());
    assert_integer_value(
        find_field(entries, 2),
        i128::from(expected.session_incarnation),
    );
}

fn envelope_frame(
    target: Option<Location>,
    source: Option<Location>,
    msg_type: i32,
    payload: Vec<u8>,
) -> EnvelopeFrame {
    EnvelopeFrame {
        version: WIRE_VERSION,
        target,
        source,
        msg_type,
        payload,
        request_id: 0,
    }
}

fn cddl_control_shape(frame: &ControlFrame) {
    let bytes = to_cbor(frame);
    let value = decode_value(&bytes);
    let entries = map_entries(&value);
    assert_eq!(entries.len(), 4);
    assert_integer_keys_and_order(entries, &[1, 2, 3, 4]);
    assert_integer_value(find_field(entries, 1), i128::from(WIRE_VERSION));
    assert_integer_value(find_field(entries, 2), i128::from(FRAME_TYPE_CONTROL));
    assert_integer_value(find_field(entries, 3), i128::from(frame.ctrl_kind));
    assert_bytes_value(find_field(entries, 4), &frame.payload);
}

fn cddl_envelope_shape(frame: &EnvelopeFrame) {
    let bytes = encode_envelope_frame(frame).expect("envelope should encode");
    let value = decode_value(&bytes);
    let entries = map_entries(&value);
    assert_eq!(entries.len(), 7);
    assert_integer_keys_and_order(entries, &[1, 2, 3, 4, 5, 6, 7]);
    assert_integer_value(find_field(entries, 1), i128::from(WIRE_VERSION));
    assert_integer_value(find_field(entries, 2), i128::from(FRAME_TYPE_ENVELOPE));
    assert_optional_location_value(find_field(entries, 3), frame.target);
    assert_optional_location_value(find_field(entries, 4), frame.source);
    assert_integer_value(find_field(entries, 5), i128::from(frame.msg_type));
    assert_bytes_value(find_field(entries, 6), &frame.payload);
    assert_integer_value(find_field(entries, 7), i128::from(frame.request_id));
}

fn cddl_envelope_shape_raw_serialize(frame: &EnvelopeFrame) {
    let bytes = to_cbor(frame);
    let value = decode_value(&bytes);
    let entries = map_entries(&value);
    assert_eq!(entries.len(), 7);
    assert_integer_keys_and_order(entries, &[1, 2, 3, 4, 5, 6, 7]);
    assert_bytes_value(find_field(entries, 6), &frame.payload);
}

fn payload(size: usize) -> Vec<u8> {
    (0u8..=255).cycle().take(size).collect()
}

fn minimal_control() -> ControlFrame {
    ControlFrame {
        version: WIRE_VERSION,
        ctrl_kind: 0,
        payload: vec![],
    }
}

fn minimal_envelope() -> EnvelopeFrame {
    envelope_frame(Some(location(1, 1, 1)), None, 0, vec![])
}

fn registry_payload(op: u8, name: &str, location: Location) -> RegistryGossipPayload {
    RegistryGossipPayload {
        op,
        name: name.to_owned(),
        location,
    }
}

fn well_formed_envelope_value_with_frame_type(frame_type: Value) -> Value {
    Value::Map(vec![
        (int(1u64), int(WIRE_VERSION)),
        (int(2u64), frame_type),
        (int(3u64), location_value(location(1, 1, 1))),
        (int(4u64), Value::Null),
        (int(5u64), int(0i32)),
        (int(6u64), Value::Bytes(vec![])),
        (int(7u64), int(0u64)),
    ])
}

fn legacy_text_keyed_envelope_value() -> Value {
    Value::Map(vec![
        (Value::Text("1".to_owned()), int(WIRE_VERSION)),
        (
            Value::Text("3".to_owned()),
            location_value(location(1, 1, 1)),
        ),
        (Value::Text("4".to_owned()), Value::Null),
        (Value::Text("5".to_owned()), int(0i32)),
        (
            Value::Text("6".to_owned()),
            Value::Array(vec![int(0xaau8), int(0xbbu8)]),
        ),
        (Value::Text("7".to_owned()), int(0u64)),
    ])
}

fn legacy_text_keyed_control_value() -> Value {
    Value::Map(vec![
        (Value::Text("1".to_owned()), int(WIRE_VERSION)),
        (Value::Text("3".to_owned()), int(2u64)),
        (
            Value::Text("4".to_owned()),
            Value::Array(vec![int(0xaau8), int(0xbbu8)]),
        ),
    ])
}

// -- Schema constant tests --------------------------------------------------

#[test]
fn wire_version_is_two() {
    assert_eq!(
        WIRE_VERSION, 2,
        "WIRE_VERSION must match schemas/envelope.cddl wire-version = 2"
    );
}

#[test]
fn frame_type_discriminants_match_cddl() {
    assert_eq!(
        FRAME_TYPE_CONTROL, 0,
        "control frame discriminant must be 0"
    );
    assert_eq!(
        FRAME_TYPE_ENVELOPE, 1,
        "envelope frame discriminant must be 1"
    );
}

// -- EnvelopeFrame round-trip tests ----------------------------------------

#[test]
fn envelope_frame_fire_and_forget_round_trips() {
    let original = EnvelopeFrame::fire_and_forget(
        location(1, 42, 7),
        Some(location(2, 7, 11)),
        3,
        vec![1, 2, 3],
    );
    let bytes = to_cbor(&original);
    let decoded: EnvelopeFrame = from_cbor(&bytes);
    assert_eq!(original, decoded);
}

#[test]
fn envelope_frame_with_request_id_round_trips() {
    let original = EnvelopeFrame {
        version: WIRE_VERSION,
        target: Some(location(3, 100, 13)),
        source: Some(location(4, 200, 17)),
        msg_type: 0,
        payload: b"hello".to_vec(),
        request_id: 99,
    };

    let bytes = to_cbor(&original);
    let decoded: EnvelopeFrame = from_cbor(&bytes);
    assert_eq!(original, decoded);
}

#[test]
fn envelope_frame_empty_payload_round_trips() {
    let original = EnvelopeFrame::fire_and_forget(location(5, 1, 19), None, 0, vec![]);
    let bytes = to_cbor(&original);
    let decoded: EnvelopeFrame = from_cbor(&bytes);
    assert_eq!(original, decoded);
}

#[test]
fn envelope_frame_large_payload_round_trips() {
    let original = EnvelopeFrame::fire_and_forget(
        location(6, 1000, 23),
        Some(location(7, 2000, 29)),
        7,
        payload(4096),
    );
    let bytes = to_cbor(&original);
    let decoded: EnvelopeFrame = from_cbor(&bytes);
    assert_eq!(original, decoded);
}

#[test]
fn envelope_frame_max_field_values_round_trips() {
    let original = EnvelopeFrame {
        version: WIRE_VERSION,
        target: Some(location(8, u64::MAX, u32::MAX)),
        source: Some(location(9, u64::MAX - 1, u32::MAX - 1)),
        msg_type: i32::MAX,
        payload: vec![0xFF; 16],
        request_id: u64::MAX,
    };

    let bytes = to_cbor(&original);
    let decoded: EnvelopeFrame = from_cbor(&bytes);
    assert_eq!(original, decoded);
}

// -- ControlFrame round-trip tests -----------------------------------------

#[test]
fn control_frame_bare_signal_round_trips() {
    let original = minimal_control();
    let bytes = to_cbor(&original);
    let decoded: ControlFrame = from_cbor(&bytes);
    assert_eq!(original, decoded);
}

#[test]
fn control_frame_with_payload_round_trips() {
    let original = ControlFrame {
        version: WIRE_VERSION,
        ctrl_kind: 42,
        payload: b"capability-negotiation-data".to_vec(),
    };

    let bytes = to_cbor(&original);
    let decoded: ControlFrame = from_cbor(&bytes);
    assert_eq!(original, decoded);
}

#[test]
fn control_frame_public_encoder_round_trips() {
    let original = ControlFrame {
        version: WIRE_VERSION,
        ctrl_kind: CTRL_REGISTRY_GOSSIP,
        payload: b"registry-gossip".to_vec(),
    };

    let bytes = encode_control_frame(&original).expect("control frame should encode");
    cddl_control_shape(&original);
    assert_eq!(decode_control_frame(&bytes).unwrap(), original);
}

#[test]
fn registry_gossip_payload_round_trips_and_matches_cluster_ops() {
    assert_eq!(REGISTRY_GOSSIP_OP_ADD, GOSSIP_REGISTRY_ADD);
    assert_eq!(REGISTRY_GOSSIP_OP_REMOVE, GOSSIP_REGISTRY_REMOVE);

    let original = registry_payload(
        REGISTRY_GOSSIP_OP_ADD,
        "worker",
        location(10, 0x6200_0000_0000_0042, 31),
    );
    let bytes = encode_registry_gossip_payload(&original).expect("payload should encode");
    let decoded = decode_registry_gossip_payload(&bytes).expect("payload should decode");
    assert_eq!(decoded, original);

    let value = decode_value(&bytes);
    let entries = map_entries(&value);
    assert_eq!(entries.len(), 3);
    assert_integer_keys_and_order(entries, &[1, 2, 3]);
    assert_integer_value(find_field(entries, 1), i128::from(REGISTRY_GOSSIP_OP_ADD));
    assert_text_value(find_field(entries, 2), "worker");
    assert_location_value(find_field(entries, 3), original.location);

    let remove = registry_payload(
        REGISTRY_GOSSIP_OP_REMOVE,
        "worker",
        location(10, 0x6200_0000_0000_0042, 31),
    );
    let remove_bytes = encode_registry_gossip_payload(&remove).expect("remove payload");
    assert_eq!(
        decode_registry_gossip_payload(&remove_bytes).unwrap(),
        remove
    );
}

#[test]
fn registry_gossip_payload_codec_rejects_malformed_payloads() {
    let unknown_op = Value::Map(vec![
        (int(1u64), int(99u8)),
        (int(2u64), Value::Text("worker".to_owned())),
        (int(3u64), location_value(location(11, 42, 37))),
    ]);
    assert!(matches!(
        decode_registry_gossip_payload(&value_to_cbor(&unknown_op)),
        Err(RegistryGossipPayloadError::InvalidOp { op: 99 })
    ));

    let missing_name = Value::Map(vec![(int(1u64), int(REGISTRY_GOSSIP_OP_ADD))]);
    assert!(matches!(
        decode_registry_gossip_payload(&value_to_cbor(&missing_name)),
        Err(RegistryGossipPayloadError::MissingKey { key: 2 })
    ));

    let duplicate_name = Value::Map(vec![
        (int(1u64), int(REGISTRY_GOSSIP_OP_ADD)),
        (int(2u64), Value::Text("worker".to_owned())),
        (int(2u64), Value::Text("other".to_owned())),
        (int(3u64), location_value(location(11, 42, 37))),
    ]);
    assert!(matches!(
        decode_registry_gossip_payload(&value_to_cbor(&duplicate_name)),
        Err(RegistryGossipPayloadError::DuplicateKey { key: 2 })
    ));

    let unknown_key = Value::Map(vec![
        (int(1u64), int(REGISTRY_GOSSIP_OP_ADD)),
        (int(2u64), Value::Text("worker".to_owned())),
        (int(3u64), location_value(location(11, 42, 37))),
        (int(4u64), int(0u64)),
    ]);
    assert!(matches!(
        decode_registry_gossip_payload(&value_to_cbor(&unknown_key)),
        Err(RegistryGossipPayloadError::UnknownKey { key: 4 })
    ));

    let add_zero_node = Value::Map(vec![
        (int(1u64), int(REGISTRY_GOSSIP_OP_ADD)),
        (int(2u64), Value::Text("worker".to_owned())),
        (int(3u64), raw_location_value([0; 16], 42, 37)),
    ]);
    assert!(matches!(
        decode_registry_gossip_payload(&value_to_cbor(&add_zero_node)),
        Err(RegistryGossipPayloadError::MalformedField { key: 3, .. })
    ));

    let nul_name = Value::Map(vec![
        (int(1u64), int(REGISTRY_GOSSIP_OP_ADD)),
        (int(2u64), Value::Text("bad\0name".to_owned())),
        (int(3u64), location_value(location(11, 42, 37))),
    ]);
    assert!(matches!(
        decode_registry_gossip_payload(&value_to_cbor(&nul_name)),
        Err(RegistryGossipPayloadError::MalformedField { key: 2, .. })
    ));

    let long_name = registry_payload(
        REGISTRY_GOSSIP_OP_ADD,
        &"x".repeat(MAX_REGISTRY_GOSSIP_NAME_BYTES + 1),
        location(11, 42, 37),
    );
    assert!(matches!(
        encode_registry_gossip_payload(&long_name),
        Err(RegistryGossipPayloadError::MalformedField { key: 2, .. })
    ));

    let oversized = vec![0u8; MAX_REGISTRY_GOSSIP_PAYLOAD_BYTES + 1];
    assert!(matches!(
        decode_registry_gossip_payload(&oversized),
        Err(RegistryGossipPayloadError::PayloadTooLarge { .. })
    ));
}

// -- Byte-shape conformance -------------------------------------------------

#[test]
fn control_frame_serialises_to_cddl_shape() {
    cddl_control_shape(&ControlFrame {
        version: WIRE_VERSION,
        ctrl_kind: 2,
        payload: vec![0xaa, 0xbb],
    });
}

#[test]
fn envelope_frame_serialises_to_cddl_shape() {
    cddl_envelope_shape(&EnvelopeFrame {
        version: WIRE_VERSION,
        target: Some(location(12, 10, 41)),
        source: Some(location(13, 20, 43)),
        msg_type: 7,
        payload: vec![0xaa, 0xbb],
        request_id: 30,
    });
}

#[test]
fn boundary_payload_sizes_encode_as_bstr_and_round_trip() {
    for size in [0usize, 1, 23, 24, 255, 256, 65_535, 65_536] {
        let envelope = envelope_frame(
            Some(location(14, 1, 47)),
            Some(location(15, 2, 53)),
            0,
            payload(size),
        );
        let envelope_bytes = encode_envelope_frame(&envelope).expect("envelope should encode");
        assert_eq!(decode_envelope_frame(&envelope_bytes).unwrap(), envelope);
        let envelope_value = decode_value(&envelope_bytes);
        assert_bytes_value(
            find_field(map_entries(&envelope_value), 6),
            &envelope.payload,
        );

        let control = ControlFrame {
            version: WIRE_VERSION,
            ctrl_kind: 0,
            payload: payload(size),
        };
        let control_bytes = to_cbor(&control);
        assert_eq!(decode_control_frame(&control_bytes).unwrap(), control);
        let control_value = decode_value(&control_bytes);
        assert_bytes_value(find_field(map_entries(&control_value), 4), &control.payload);
    }
}

#[test]
fn public_encoder_preserves_integer_width_boundaries_and_msg_type_gate() {
    for request_id in [0u64, 1, u64::MAX] {
        let frame = EnvelopeFrame {
            version: WIRE_VERSION,
            target: Some(location(16, 1, 1)),
            source: Some(location(17, u64::MAX, u32::MAX)),
            msg_type: 0,
            payload: vec![],
            request_id,
        };
        cddl_envelope_shape(&frame);
        assert_eq!(
            decode_envelope_frame(&encode_envelope_frame(&frame).unwrap()).unwrap(),
            frame
        );
    }

    for (slot, session_incarnation) in [(1, 1), (u64::MAX, u32::MAX)] {
        let frame = EnvelopeFrame {
            target: Some(location(18, slot, session_incarnation)),
            ..minimal_envelope()
        };
        cddl_envelope_shape(&frame);
    }

    for (target, source) in [
        (Some(location(19, 1, 59)), None),
        (None, Some(location(20, 2, 61))),
        (None, None),
    ] {
        let frame = EnvelopeFrame {
            target,
            source,
            ..minimal_envelope()
        };
        cddl_envelope_shape(&frame);
        assert_eq!(
            decode_envelope_frame(&encode_envelope_frame(&frame).unwrap()).unwrap(),
            frame
        );
    }

    for ctrl_kind in [0u64, u64::MAX] {
        let control = ControlFrame {
            version: WIRE_VERSION,
            ctrl_kind,
            payload: vec![],
        };
        cddl_control_shape(&control);
        assert_eq!(from_cbor::<ControlFrame>(&to_cbor(&control)), control);
    }

    // The encoder accepts the full i32 range — Hew codegen uses hashed i32
    // discriminants that may be negative. CBOR Integer covers this range on
    // both the encode and decode paths.
    for msg_type in [0, 1, 65_535, -1, i32::MIN, i32::MAX, 65_536] {
        let frame = EnvelopeFrame {
            msg_type,
            ..minimal_envelope()
        };
        let encoded = encode_envelope_frame(&frame);
        assert!(
            encoded.is_ok(),
            "msg_type {msg_type} must be accepted by the public encoder"
        );
        assert_eq!(
            decode_envelope_frame(&encoded.unwrap()).unwrap().msg_type,
            msg_type,
            "msg_type {msg_type} must round-trip through encode/decode"
        );
    }
}

#[test]
fn raw_serialize_preserves_signed_msg_type_wire_shape() {
    for msg_type in [-1, i32::MIN, i32::MAX] {
        let frame = EnvelopeFrame {
            msg_type,
            ..minimal_envelope()
        };
        cddl_envelope_shape_raw_serialize(&frame);
        let value = decode_value(&to_cbor(&frame));
        assert_integer_value(find_field(map_entries(&value), 5), i128::from(msg_type));
    }
}

#[test]
fn encoded_frames_match_cddl_structural_rules() {
    cddl_control_shape(&minimal_control());
    cddl_control_shape(&ControlFrame {
        version: WIRE_VERSION,
        ctrl_kind: 255,
        payload: payload(24),
    });
    cddl_envelope_shape(&minimal_envelope());
    cddl_envelope_shape(&EnvelopeFrame {
        version: WIRE_VERSION,
        target: Some(location(21, u64::MAX, u32::MAX)),
        source: Some(location(22, 1, 1)),
        msg_type: 65_535,
        payload: payload(256),
        request_id: u64::MAX,
    });
}

// -- Serialised output is non-empty and stable ------------------------------

#[test]
fn envelope_frame_serialises_to_nonempty_bytes() {
    let frame =
        EnvelopeFrame::fire_and_forget(location(23, 1, 67), Some(location(24, 2, 71)), 0, vec![]);
    let bytes = to_cbor(&frame);
    assert!(!bytes.is_empty(), "CBOR output must not be empty");
}

#[test]
fn same_envelope_produces_same_bytes() {
    let frame = EnvelopeFrame::fire_and_forget(
        location(25, 1, 73),
        Some(location(26, 2, 79)),
        0,
        vec![10, 20],
    );
    let bytes1 = to_cbor(&frame);
    let bytes2 = to_cbor(&frame);
    assert_eq!(bytes1, bytes2, "serialisation must be deterministic");

    // a4 map(4), then k/v pairs: 1=>version, 2=>frame_type,
    // 3=>ctrl_kind, 4=>empty bstr.
    assert_eq!(
        to_cbor(&minimal_control()),
        vec![0xa4, 0x01, 0x02, 0x02, 0x00, 0x03, 0x00, 0x04, 0x40]
    );

    // a7 map(7), then k/v pairs: 1=>version, 2=>frame_type,
    // 3=>target location, 4=>nil source, 5=>msg_type, 6=>empty bstr,
    // 7=>request_id.
    assert_eq!(
        encode_envelope_frame(&minimal_envelope()).unwrap(),
        vec![
            0xa7, 0x01, 0x02, 0x02, 0x01, 0x03, 0xa3, 0x01, 0x50, 0x01, 0x01, 0x01, 0x01, 0x01,
            0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x02, 0x01, 0x03,
            0x01, 0x04, 0xf6, 0x05, 0x00, 0x06, 0x40, 0x07, 0x00
        ]
    );
}

// -- Fail-closed decode invariants -----------------------------------------

#[test]
fn wrong_version_frame_is_rejected() {
    let bogus = EnvelopeFrame {
        version: WIRE_VERSION + 1,
        target: Some(location(27, 1, 83)),
        source: Some(location(28, 2, 89)),
        msg_type: 0,
        payload: vec![],
        request_id: 0,
    };
    let bytes = to_cbor(&bogus);

    match decode_envelope_frame(&bytes) {
        Err(DecodeError::UnknownVersion { found, expected }) => {
            assert_eq!(found, WIRE_VERSION + 1);
            assert_eq!(expected, WIRE_VERSION);
        }
        Ok(_) => panic!("decoder accepted a frame with a non-current wire version"),
        Err(other) => panic!("expected UnknownVersion, got {other:?}"),
    }

    let bogus_ctrl = ControlFrame {
        version: 0,
        ctrl_kind: 0,
        payload: vec![],
    };
    let ctrl_bytes = to_cbor(&bogus_ctrl);
    assert!(
        matches!(
            decode_control_frame(&ctrl_bytes),
            Err(DecodeError::UnknownVersion { found: 0, .. })
        ),
        "control-frame decoder must also reject unknown versions",
    );
}

#[test]
fn truncated_cbor_returns_error() {
    let frame = EnvelopeFrame::fire_and_forget(
        location(29, 42, 97),
        Some(location(30, 7, 101)),
        3,
        vec![1, 2, 3, 4, 5],
    );
    let bytes = to_cbor(&frame);
    assert!(bytes.len() > 4, "need a non-trivial frame to truncate");

    for cut in 1..bytes.len() {
        assert!(
            decode_envelope_frame(&bytes[..cut]).is_err(),
            "decoder returned Ok on a {cut}-byte truncation of a {}-byte frame",
            bytes.len()
        );
    }
}

#[test]
fn mismatched_frame_type_is_rejected() {
    let ctrl = ControlFrame {
        version: WIRE_VERSION,
        ctrl_kind: 9,
        payload: b"keepalive".to_vec(),
    };
    let ctrl_bytes = to_cbor(&ctrl);
    assert!(matches!(
        decode_envelope_frame(&ctrl_bytes),
        Err(DecodeError::FrameTypeUnsupported { found: 0 })
    ));

    let env =
        EnvelopeFrame::fire_and_forget(location(31, 1, 103), Some(location(32, 2, 107)), 0, vec![]);
    let env_bytes = to_cbor(&env);
    assert!(matches!(
        decode_control_frame(&env_bytes),
        Err(DecodeError::FrameTypeUnsupported { found: 1 })
    ));
}

#[test]
fn decode_wire_frame_dispatches_by_frame_type() {
    let control = ControlFrame {
        version: WIRE_VERSION,
        ctrl_kind: 5,
        payload: vec![1, 2, 3],
    };
    assert_eq!(
        decode_wire_frame(&to_cbor(&control)).unwrap(),
        WireFrame::Control(control)
    );

    let envelope = EnvelopeFrame::fire_and_forget(
        location(33, 1, 109),
        Some(location(34, 2, 113)),
        0,
        vec![4, 5, 6],
    );
    assert_eq!(
        decode_wire_frame(&encode_envelope_frame(&envelope).unwrap()).unwrap(),
        WireFrame::Envelope(envelope)
    );
}

#[test]
fn decode_wire_frame_rejects_unsupported_frame_type_values() {
    for (frame_type, expected) in [(int(99u64), 99), (int(256u64), 256), (int(-1i64), -1)] {
        let bytes = value_to_cbor(&well_formed_envelope_value_with_frame_type(frame_type));
        assert!(matches!(
            decode_wire_frame(&bytes),
            Err(DecodeError::FrameTypeUnsupported { found }) if found == expected
        ));
    }
}

#[test]
fn decode_wire_frame_rejects_malformed_frame_type_values() {
    for frame_type in [
        int(u64::MAX),
        Value::Text("control".to_owned()),
        Value::Array(vec![int(0u8)]),
        Value::Bytes(vec![0]),
    ] {
        let bytes = value_to_cbor(&well_formed_envelope_value_with_frame_type(frame_type));
        assert!(matches!(
            decode_wire_frame(&bytes),
            Err(DecodeError::FrameTypeMalformed)
        ));
    }
}

#[test]
fn decode_wire_frame_rejects_missing_and_duplicate_frame_type() {
    let missing = Value::Map(vec![
        (int(1u64), int(WIRE_VERSION)),
        (int(3u64), location_value(location(35, 1, 127))),
        (int(4u64), Value::Null),
        (int(5u64), int(0i32)),
        (int(6u64), Value::Bytes(vec![])),
        (int(7u64), int(0u64)),
    ]);
    assert!(matches!(
        decode_wire_frame(&value_to_cbor(&missing)),
        Err(DecodeError::FrameTypeMissing)
    ));

    let duplicate = Value::Map(vec![
        (int(1u64), int(WIRE_VERSION)),
        (int(2u64), int(FRAME_TYPE_CONTROL)),
        (int(2u64), int(FRAME_TYPE_ENVELOPE)),
        (int(3u64), int(0u64)),
        (int(4u64), Value::Bytes(vec![])),
    ]);
    assert!(matches!(
        decode_wire_frame(&value_to_cbor(&duplicate)),
        Err(DecodeError::DuplicateKey { key: 2 })
    ));
}

#[test]
fn variant_decoders_reject_duplicate_and_malformed_keys() {
    let duplicate_payload_key = Value::Map(vec![
        (int(1u64), int(WIRE_VERSION)),
        (int(2u64), int(FRAME_TYPE_CONTROL)),
        (int(3u64), int(0u64)),
        (int(4u64), Value::Bytes(vec![])),
        (int(4u64), Value::Bytes(vec![1])),
    ]);
    assert!(matches!(
        decode_control_frame(&value_to_cbor(&duplicate_payload_key)),
        Err(DecodeError::DuplicateKey { key: 4 })
    ));

    let text_key = Value::Map(vec![
        (int(1u64), int(WIRE_VERSION)),
        (Value::Text("2".to_owned()), int(FRAME_TYPE_CONTROL)),
        (int(3u64), int(0u64)),
        (int(4u64), Value::Bytes(vec![])),
    ]);
    assert!(matches!(
        decode_control_frame(&value_to_cbor(&text_key)),
        Err(DecodeError::MalformedKey)
    ));
}

#[test]
fn legacy_text_keyed_array_payload_encodings_are_rejected() {
    let legacy_envelope = value_to_cbor(&legacy_text_keyed_envelope_value());
    assert!(matches!(
        decode_envelope_frame(&legacy_envelope),
        Err(DecodeError::MalformedKey)
    ));
    assert!(matches!(
        decode_wire_frame(&legacy_envelope),
        Err(DecodeError::MalformedKey)
    ));

    let legacy_control = value_to_cbor(&legacy_text_keyed_control_value());
    assert!(matches!(
        decode_control_frame(&legacy_control),
        Err(DecodeError::MalformedKey)
    ));
    assert!(matches!(
        decode_wire_frame(&legacy_control),
        Err(DecodeError::MalformedKey)
    ));
}

#[test]
fn array_payload_with_integer_keys_is_rejected() {
    let array_payload = Value::Map(vec![
        (int(1u64), int(WIRE_VERSION)),
        (int(2u64), int(FRAME_TYPE_ENVELOPE)),
        (int(3u64), location_value(location(36, 1, 131))),
        (int(4u64), Value::Null),
        (int(5u64), int(0i32)),
        (int(6u64), Value::Array(vec![int(0xaau8), int(0xbbu8)])),
        (int(7u64), int(0u64)),
    ]);
    assert!(matches!(
        decode_envelope_frame(&value_to_cbor(&array_payload)),
        Err(DecodeError::MalformedField { key: 6, .. })
    ));
}

#[test]
fn envelope_location_fields_reject_malformed_shapes() {
    for malformed_target in [
        int(1u64),
        raw_location_value([0; 16], 1, 1),
        raw_location_value([66; 16], 0, 1),
        raw_location_value([66; 16], 1, 0),
        raw_location_value([66; 16], 1, u64::from(u32::MAX) + 1),
        Value::Map(vec![
            (int(1u64), Value::Bytes(vec![66; 15])),
            (int(2u64), int(1u64)),
            (int(3u64), int(1u64)),
        ]),
    ] {
        let malformed = Value::Map(vec![
            (int(1u64), int(WIRE_VERSION)),
            (int(2u64), int(FRAME_TYPE_ENVELOPE)),
            (int(3u64), malformed_target),
            (int(4u64), Value::Null),
            (int(5u64), int(0i32)),
            (int(6u64), Value::Bytes(vec![])),
            (int(7u64), int(0u64)),
        ]);
        assert!(matches!(
            decode_envelope_frame(&value_to_cbor(&malformed)),
            Err(DecodeError::MalformedField { key: 3, .. })
        ));
    }

    let malformed_source = Value::Map(vec![
        (int(1u64), int(WIRE_VERSION)),
        (int(2u64), int(FRAME_TYPE_ENVELOPE)),
        (int(3u64), location_value(location(67, 1, 269))),
        (int(4u64), int(2u64)),
        (int(5u64), int(0i32)),
        (int(6u64), Value::Bytes(vec![])),
        (int(7u64), int(0u64)),
    ]);
    assert!(matches!(
        decode_envelope_frame(&value_to_cbor(&malformed_source)),
        Err(DecodeError::MalformedField { key: 4, .. })
    ));
}

// -- SWIM control payload codec ---------------------------------------------

fn swim_payload(
    msg_type: i32,
    from: NodeSessionIdentity,
    gossip: Vec<SwimGossipEntry>,
) -> SwimControlPayload {
    SwimControlPayload {
        msg_type,
        from,
        incarnation: 7,
        target: None,
        gossip,
    }
}

#[test]
fn swim_payload_round_trips_with_piggybacked_gossip() {
    let original = SwimControlPayload {
        msg_type: 1, // SWIM_MSG_PING
        from: node_session(40, 137),
        incarnation: 9,
        target: Some(node_session(41, 139)),
        gossip: vec![
            SwimGossipEntry {
                member: node_session(42, 149),
                state: 2, // MEMBER_DEAD
                incarnation: 4,
            },
            SwimGossipEntry {
                member: node_session(43, 151),
                state: 0, // MEMBER_ALIVE
                incarnation: 12,
            },
        ],
    };
    let bytes = encode_swim_payload(&original).expect("payload should encode");
    let decoded = decode_swim_payload(&bytes).expect("payload should decode");
    assert_eq!(decoded, original);

    // CDDL shape: top-level map keyed {1,2,3,4,5}.
    let value = decode_value(&bytes);
    let entries = map_entries(&value);
    assert_eq!(entries.len(), 5);
    assert_integer_keys_and_order(entries, &[1, 2, 3, 4, 5]);
    assert_integer_value(find_field(entries, 1), 1);
    assert_node_session_value(find_field(entries, 2), original.from);
    assert_integer_value(find_field(entries, 3), 9);
    assert_node_session_value(find_field(entries, 4), original.target.unwrap());

    let Value::Array(gossip) = find_field(entries, 5) else {
        panic!("expected SWIM gossip array");
    };
    assert_eq!(gossip.len(), 2);
    for (encoded, expected) in gossip.iter().zip(&original.gossip) {
        let entry = map_entries(encoded);
        assert_eq!(entry.len(), 3);
        assert_integer_keys_and_order(entry, &[1, 2, 3]);
        assert_node_session_value(find_field(entry, 1), expected.member);
        assert_integer_value(find_field(entry, 2), i128::from(expected.state));
        assert_integer_value(find_field(entry, 3), i128::from(expected.incarnation));
    }
}

#[test]
fn swim_payload_empty_gossip_round_trips() {
    let original = swim_payload(2, node_session(44, 157), vec![]); // SWIM_MSG_ACK, no gossip
    let bytes = encode_swim_payload(&original).expect("payload should encode");
    assert_eq!(decode_swim_payload(&bytes).unwrap(), original);
    let value = decode_value(&bytes);
    assert!(matches!(find_field(map_entries(&value), 4), Value::Null));
}

#[test]
fn swim_payload_codec_rejects_malformed_payloads() {
    // Unknown top-level key.
    let unknown_key = Value::Map(vec![
        (int(1u64), int(1i32)),
        (int(2u64), node_session_value(node_session(45, 163))),
        (int(3u64), int(7u64)),
        (int(4u64), Value::Null),
        (int(5u64), Value::Array(vec![])),
        (int(6u64), int(0u64)),
    ]);
    assert!(matches!(
        decode_swim_payload(&value_to_cbor(&unknown_key)),
        Err(SwimPayloadError::UnknownKey { key: 6 })
    ));

    // Missing required key 5 (gossip array).
    let missing_gossip = Value::Map(vec![
        (int(1u64), int(1i32)),
        (int(2u64), node_session_value(node_session(45, 163))),
        (int(3u64), int(7u64)),
        (int(4u64), Value::Null),
    ]);
    assert!(matches!(
        decode_swim_payload(&value_to_cbor(&missing_gossip)),
        Err(SwimPayloadError::MissingKey { key: 5 })
    ));

    // Duplicate key 1.
    let duplicate = Value::Map(vec![
        (int(1u64), int(1i32)),
        (int(1u64), int(2i32)),
        (int(2u64), node_session_value(node_session(45, 163))),
        (int(3u64), int(7u64)),
        (int(4u64), Value::Null),
        (int(5u64), Value::Array(vec![])),
    ]);
    assert!(matches!(
        decode_swim_payload(&value_to_cbor(&duplicate)),
        Err(SwimPayloadError::DuplicateKey { key: 1 })
    ));

    // Gossip field is not an array.
    let gossip_not_array = Value::Map(vec![
        (int(1u64), int(1i32)),
        (int(2u64), node_session_value(node_session(45, 163))),
        (int(3u64), int(7u64)),
        (int(4u64), Value::Null),
        (int(5u64), int(0u64)),
    ]);
    assert!(matches!(
        decode_swim_payload(&value_to_cbor(&gossip_not_array)),
        Err(SwimPayloadError::MalformedField { key: 5, .. })
    ));

    for malformed_from in [
        raw_node_session_value([0; 16], 1),
        raw_node_session_value([46; 16], 0),
        raw_node_session_value([46; 16], u64::from(u32::MAX) + 1),
        Value::Map(vec![
            (int(1u64), Value::Bytes(vec![46; 15])),
            (int(2u64), int(1u64)),
        ]),
    ] {
        let malformed = Value::Map(vec![
            (int(1u64), int(1i32)),
            (int(2u64), malformed_from),
            (int(3u64), int(7u64)),
            (int(4u64), Value::Null),
            (int(5u64), Value::Array(vec![])),
        ]);
        assert!(matches!(
            decode_swim_payload(&value_to_cbor(&malformed)),
            Err(SwimPayloadError::MalformedField { key: 2, .. })
        ));
    }

    let malformed_target = Value::Map(vec![
        (int(1u64), int(1i32)),
        (int(2u64), node_session_value(node_session(45, 163))),
        (int(3u64), int(7u64)),
        (int(4u64), int(0u64)),
        (int(5u64), Value::Array(vec![])),
    ]);
    assert!(matches!(
        decode_swim_payload(&value_to_cbor(&malformed_target)),
        Err(SwimPayloadError::MalformedField { key: 4, .. })
    ));

    // Oversized payload (byte cap).
    let oversized = vec![0u8; MAX_SWIM_PAYLOAD_BYTES + 1];
    assert!(matches!(
        decode_swim_payload(&oversized),
        Err(SwimPayloadError::PayloadTooLarge { .. })
    ));
}

#[test]
fn swim_payload_rejects_oversized_gossip_batch() {
    // Encode-side guard: too many entries.
    let oversized_count = MAX_SWIM_GOSSIP_ENTRIES + 1;
    let too_many = SwimControlPayload {
        msg_type: 1,
        from: node_session(47, 167),
        incarnation: 1,
        target: None,
        gossip: (0..oversized_count)
            .map(|i| SwimGossipEntry {
                member: node_session(
                    u8::try_from((i % 200) + 1).expect("bounded test seed"),
                    u32::try_from(i + 1).expect("bounded test session"),
                ),
                state: 0,
                incarnation: 1,
            })
            .collect(),
    };
    assert!(matches!(
        encode_swim_payload(&too_many),
        Err(SwimPayloadError::TooManyGossipEntries { .. })
    ));

    // Decode-side guard: a hand-built array exceeding the cap is rejected.
    let entries: Vec<(Value, Value)> = vec![
        (int(1u64), int(1i32)),
        (int(2u64), node_session_value(node_session(47, 167))),
        (int(3u64), int(1u64)),
        (int(4u64), Value::Null),
        (
            int(5u64),
            Value::Array(
                (0..oversized_count)
                    .map(|i| {
                        Value::Map(vec![
                            (
                                int(1u64),
                                node_session_value(node_session(
                                    u8::try_from((i % 200) + 1).expect("bounded test seed"),
                                    u32::try_from(i + 1).expect("bounded test session"),
                                )),
                            ),
                            (int(2u64), int(0i32)),
                            (int(3u64), int(1u64)),
                        ])
                    })
                    .collect(),
            ),
        ),
    ];
    let oversized_batch = Value::Map(entries);
    assert!(matches!(
        decode_swim_payload(&value_to_cbor(&oversized_batch)),
        Err(SwimPayloadError::TooManyGossipEntries { .. })
    ));
}

#[test]
fn swim_payload_truncated_cbor_returns_error() {
    let original = swim_payload(
        1,
        node_session(48, 173),
        vec![SwimGossipEntry {
            member: node_session(49, 179),
            state: 1,
            incarnation: 2,
        }],
    );
    let bytes = encode_swim_payload(&original).expect("payload should encode");
    for cut in 1..bytes.len() {
        assert!(
            decode_swim_payload(&bytes[..cut]).is_err(),
            "decoder returned Ok on a {cut}-byte truncation"
        );
    }
}

// ── Cross-node monitor control payloads ──────────────────────────────────────

#[test]
fn monitor_ctrl_kinds_are_distinct_and_stable() {
    // The wire kinds must be stable and distinct from the existing families so a
    // peer routes each frame to the right handler.
    assert_eq!(CTRL_MONITOR_REQ, 3);
    assert_eq!(CTRL_MONITOR_DOWN, 4);
    assert_eq!(CTRL_DEMONITOR, 5);
    assert_eq!(CTRL_REGISTRY_GOSSIP, 1);
}

#[test]
fn monitor_req_payload_round_trips_with_cddl_shape() {
    let original = MonitorReqPayload {
        watcher: location(50, 55, 181),
        ref_id: 4242,
        target: location(51, 99, 191),
    };
    let bytes = encode_monitor_req_payload(&original).expect("req should encode");
    let decoded = decode_monitor_req_payload(&bytes).expect("req should decode");
    assert_eq!(decoded, original);

    // CDDL shape: definite map keyed {1,2,3} with exact Location values.
    let value = decode_value(&bytes);
    let entries = map_entries(&value);
    assert_eq!(entries.len(), 3);
    assert_integer_keys_and_order(entries, &[1, 2, 3]);
    assert_location_value(find_field(entries, 1), original.watcher);
    assert_integer_value(find_field(entries, 2), 4242);
    assert_location_value(find_field(entries, 3), original.target);
}

#[test]
fn monitor_down_payload_round_trips_with_cddl_shape() {
    // A crash reason (HewActorState::Crashed == 5) on the DOWN frame.
    let original = MonitorDownPayload {
        ref_id: 4242,
        target: location(51, 99, 191),
        reason: 5,
        crash_kind: 1,
    };
    let bytes = encode_monitor_down_payload(&original).expect("down should encode");
    let decoded = decode_monitor_down_payload(&bytes).expect("down should decode");
    assert_eq!(decoded, original);

    let value = decode_value(&bytes);
    let entries = map_entries(&value);
    assert_eq!(entries.len(), 4);
    assert_integer_keys_and_order(entries, &[1, 2, 3, 4]);
    assert_integer_value(find_field(entries, 1), 4242);
    assert_location_value(find_field(entries, 2), original.target);
    let reason = map_entries(find_field(entries, 3));
    assert_eq!(reason.len(), 2);
    assert_integer_keys_and_order(reason, &[1, 2]);
    assert_integer_value(find_field(reason, 1), 1);
    assert_integer_value(find_field(reason, 2), 5);
    assert_integer_value(find_field(entries, 4), 1);
}

#[test]
fn monitor_down_payload_carries_negative_sentinel_reason() {
    // The MonitorLost connection-drop sentinel is negative; CBOR `int` (not
    // `uint`) must round-trip it so the watcher distinguishes a partition from a
    // clean exit / crash (which are non-negative HewActorState values).
    let original = MonitorDownPayload {
        ref_id: 1,
        target: location(52, 101, 193),
        reason: -1,
        crash_kind: 0,
    };
    let bytes = encode_monitor_down_payload(&original).expect("down should encode");
    let decoded = decode_monitor_down_payload(&bytes).expect("down should decode");
    assert_eq!(decoded.reason, -1);
    assert_eq!(decoded, original);
}

#[test]
fn monitor_down_payload_rejects_malformed_location_and_reason_shapes() {
    let malformed_target = Value::Map(vec![
        (int(1u64), int(1u64)),
        (int(2u64), Value::Null),
        (int(3u64), down_reason_value(5)),
        (int(4u64), int(0i32)),
    ]);
    assert!(matches!(
        decode_monitor_down_payload(&value_to_cbor(&malformed_target)),
        Err(MonitorPayloadError::MalformedField { key: 2, .. })
    ));

    let untyped_reason = Value::Map(vec![
        (int(1u64), int(1u64)),
        (int(2u64), location_value(location(52, 101, 193))),
        (int(3u64), int(5i32)),
        (int(4u64), int(0i32)),
    ]);
    assert!(matches!(
        decode_monitor_down_payload(&value_to_cbor(&untyped_reason)),
        Err(MonitorPayloadError::MalformedField { key: 3, .. })
    ));

    let unknown_reason_tag = Value::Map(vec![
        (int(1u64), int(1u64)),
        (int(2u64), location_value(location(52, 101, 193))),
        (
            int(3u64),
            Value::Map(vec![(int(1u64), int(99u8)), (int(2u64), int(5i32))]),
        ),
        (int(4u64), int(0i32)),
    ]);
    assert!(matches!(
        decode_monitor_down_payload(&value_to_cbor(&unknown_reason_tag)),
        Err(MonitorPayloadError::MalformedField { key: 3, .. })
    ));
}

#[test]
fn monitor_req_payload_codec_rejects_malformed_payloads() {
    // Unknown key: a fourth key beyond the {1,2,3} set must be rejected.
    let unknown_key = Value::Map(vec![
        (int(1u64), location_value(location(53, 1, 197))),
        (int(2u64), int(2u64)),
        (int(3u64), location_value(location(54, 3, 199))),
        (int(4u64), int(4u64)),
    ]);
    assert!(matches!(
        decode_monitor_req_payload(&value_to_cbor(&unknown_key)),
        Err(MonitorPayloadError::UnknownKey { key: 4 })
    ));

    // Missing required key 3 (target Location).
    let missing = Value::Map(vec![
        (int(1u64), location_value(location(53, 1, 197))),
        (int(2u64), int(2u64)),
    ]);
    assert!(matches!(
        decode_monitor_req_payload(&value_to_cbor(&missing)),
        Err(MonitorPayloadError::MissingKey { key: 3 })
    ));

    // Wrong field type: watcher is text, not a Location map.
    let wrong_type = Value::Map(vec![
        (int(1u64), Value::Text("nope".to_owned())),
        (int(2u64), int(2u64)),
        (int(3u64), location_value(location(54, 3, 199))),
    ]);
    assert!(matches!(
        decode_monitor_req_payload(&value_to_cbor(&wrong_type)),
        Err(MonitorPayloadError::MalformedField { key: 1, .. })
    ));

    // Invalid node IDs, actor slots, and durable sessions fail closed.
    for malformed_watcher in [
        raw_location_value([0; 16], 1, 1),
        raw_location_value([55; 16], 0, 1),
        raw_location_value([55; 16], 1, 0),
        raw_location_value([55; 16], 1, u64::from(u32::MAX) + 1),
        Value::Map(vec![
            (int(1u64), Value::Bytes(vec![55; 15])),
            (int(2u64), int(1u64)),
            (int(3u64), int(1u64)),
        ]),
    ] {
        let malformed = Value::Map(vec![
            (int(1u64), malformed_watcher),
            (int(2u64), int(2u64)),
            (int(3u64), location_value(location(54, 3, 199))),
        ]);
        assert!(matches!(
            decode_monitor_req_payload(&value_to_cbor(&malformed)),
            Err(MonitorPayloadError::MalformedField { key: 1, .. })
        ));
    }

    let malformed_target = Value::Map(vec![
        (int(1u64), location_value(location(53, 1, 197))),
        (int(2u64), int(2u64)),
        (int(3u64), Value::Null),
    ]);
    assert!(matches!(
        decode_monitor_req_payload(&value_to_cbor(&malformed_target)),
        Err(MonitorPayloadError::MalformedField { key: 3, .. })
    ));
}

#[test]
fn monitor_payload_rejects_oversized_input() {
    // A payload larger than the cap is rejected before any allocation-heavy
    // decode (boundary-fail-closed): an attacker cannot force unbounded work.
    let oversized = vec![0u8; MAX_MONITOR_PAYLOAD_BYTES + 1];
    assert!(matches!(
        decode_monitor_req_payload(&oversized),
        Err(MonitorPayloadError::PayloadTooLarge { .. })
    ));
    assert!(matches!(
        decode_monitor_down_payload(&oversized),
        Err(MonitorPayloadError::PayloadTooLarge { .. })
    ));
}

#[test]
fn monitor_payload_truncated_cbor_returns_error() {
    let req = MonitorReqPayload {
        watcher: location(56, 3, 211),
        ref_id: 7,
        target: location(57, 11, 223),
    };
    let bytes = encode_monitor_req_payload(&req).expect("req should encode");
    for cut in 1..bytes.len() {
        assert!(
            decode_monitor_req_payload(&bytes[..cut]).is_err(),
            "req decoder returned Ok on a {cut}-byte truncation"
        );
    }

    let down = MonitorDownPayload {
        ref_id: 7,
        target: req.target,
        reason: 6,
        crash_kind: 0,
    };
    let down_bytes = encode_monitor_down_payload(&down).expect("down should encode");
    for cut in 1..down_bytes.len() {
        assert!(
            decode_monitor_down_payload(&down_bytes[..cut]).is_err(),
            "down decoder returned Ok on a {cut}-byte truncation"
        );
    }
}

// ── Cross-node link control payloads ─────────────────────────────────────────

#[test]
fn link_ctrl_kinds_are_distinct_and_stable() {
    // The link kinds must be stable and distinct from the monitor / gossip / SWIM
    // families so a peer routes each frame to the right handler.
    assert_eq!(CTRL_LINK_REQ, 6);
    assert_eq!(CTRL_LINK_DOWN, 7);
    assert_eq!(CTRL_UNLINK, 8);
    // Distinct from the monitor family (3/4/5) and the others.
    assert_ne!(CTRL_LINK_REQ, CTRL_MONITOR_REQ);
    assert_ne!(CTRL_LINK_DOWN, CTRL_MONITOR_DOWN);
    assert_ne!(CTRL_UNLINK, CTRL_DEMONITOR);
}

#[test]
fn link_req_payload_round_trips_with_cddl_shape() {
    // CrashLinked (policy_tag == 3) link request carrying both directions'
    // identity for the bidirectional reciprocation.
    let original = LinkReqPayload {
        linker: location(58, 55, 227),
        ref_id: 4242,
        target: location(59, 99, 229),
        policy_tag: 3,
        reciprocate: 1,
    };
    let bytes = encode_link_req_payload(&original).expect("link req should encode");
    let decoded = decode_link_req_payload(&bytes).expect("link req should decode");
    assert_eq!(decoded, original);

    // CDDL shape: definite map keyed {1,2,3,4,5} with exact Locations.
    let value = decode_value(&bytes);
    let entries = map_entries(&value);
    assert_eq!(entries.len(), 5);
    assert_integer_keys_and_order(entries, &[1, 2, 3, 4, 5]);
    assert_location_value(find_field(entries, 1), original.linker);
    assert_integer_value(find_field(entries, 2), 4242);
    assert_location_value(find_field(entries, 3), original.target);
    assert_integer_value(find_field(entries, 4), 3);
    assert_integer_value(find_field(entries, 5), 1);
}

#[test]
fn link_down_payload_reuses_monitor_down_shape() {
    // The link DOWN reuses the monitor DOWN {ref_id, reason} shape — the
    // divergence is in the RECEIVER (mailbox-EXIT crash), not the wire.
    let original = MonitorDownPayload {
        ref_id: 4242,
        target: location(59, 99, 229),
        reason: 5,
        crash_kind: 0,
    };
    let bytes = encode_link_down_payload(&original).expect("link down should encode");
    let decoded = decode_link_down_payload(&bytes).expect("link down should decode");
    assert_eq!(decoded, original);
    // Negative sentinel (partition) round-trips on the link DOWN too.
    let partition = MonitorDownPayload {
        ref_id: 1,
        target: location(59, 99, 229),
        reason: -1,
        crash_kind: 0,
    };
    let pbytes = encode_link_down_payload(&partition).expect("link down should encode");
    assert_eq!(decode_link_down_payload(&pbytes).unwrap().reason, -1);
}

#[test]
fn link_req_payload_codec_rejects_malformed_payloads() {
    // Unknown key beyond the {1..5} set: a fabricated link request with an extra
    // key must be rejected — a HIGHER-stakes bar than monitor because a forged
    // link can later crash a real actor.
    let unknown_key = Value::Map(vec![
        (int(1u64), location_value(location(60, 1, 233))),
        (int(2u64), int(2u64)),
        (int(3u64), location_value(location(61, 3, 239))),
        (int(4u64), int(3u64)),
        (int(5u64), int(1u64)),
        (int(6u64), int(9u64)),
    ]);
    assert!(matches!(
        decode_link_req_payload(&value_to_cbor(&unknown_key)),
        Err(MonitorPayloadError::UnknownKey { key: 6 })
    ));

    // Missing required key 3 (target Location): the reverse-link half cannot be
    // established without it, so it fails closed rather than registering a
    // one-directional link.
    let missing = Value::Map(vec![
        (int(1u64), location_value(location(60, 1, 233))),
        (int(2u64), int(2u64)),
        (int(4u64), int(3u64)),
        (int(5u64), int(1u64)),
    ]);
    assert!(matches!(
        decode_link_req_payload(&value_to_cbor(&missing)),
        Err(MonitorPayloadError::MissingKey { key: 3 })
    ));

    let malformed_linker = Value::Map(vec![
        (int(1u64), Value::Null),
        (int(2u64), int(2u64)),
        (int(3u64), location_value(location(61, 3, 239))),
        (int(4u64), int(3u64)),
        (int(5u64), int(1u64)),
    ]);
    assert!(matches!(
        decode_link_req_payload(&value_to_cbor(&malformed_linker)),
        Err(MonitorPayloadError::MalformedField { key: 1, .. })
    ));

    let malformed_target = Value::Map(vec![
        (int(1u64), location_value(location(60, 1, 233))),
        (int(2u64), int(2u64)),
        (int(3u64), raw_location_value([61; 16], 3, 0)),
        (int(4u64), int(3u64)),
        (int(5u64), int(1u64)),
    ]);
    assert!(matches!(
        decode_link_req_payload(&value_to_cbor(&malformed_target)),
        Err(MonitorPayloadError::MalformedField { key: 3, .. })
    ));

    // policy_tag out of u8 range fails closed rather than truncating.
    let oob_policy = Value::Map(vec![
        (int(1u64), location_value(location(60, 1, 233))),
        (int(2u64), int(2u64)),
        (int(3u64), location_value(location(61, 3, 239))),
        (int(4u64), int(9000u64)),
        (int(5u64), int(1u64)),
    ]);
    assert!(matches!(
        decode_link_req_payload(&value_to_cbor(&oob_policy)),
        Err(MonitorPayloadError::MalformedField { key: 4, .. })
    ));

    let oob_reciprocate = Value::Map(vec![
        (int(1u64), location_value(location(60, 1, 233))),
        (int(2u64), int(2u64)),
        (int(3u64), location_value(location(61, 3, 239))),
        (int(4u64), int(3u64)),
        (int(5u64), int(9000u64)),
    ]);
    assert!(matches!(
        decode_link_req_payload(&value_to_cbor(&oob_reciprocate)),
        Err(MonitorPayloadError::MalformedField { key: 5, .. })
    ));
}

#[test]
fn link_payload_rejects_oversized_input() {
    // A fabricated payload larger than the cap is rejected before any decode work.
    let oversized = vec![0u8; MAX_LINK_PAYLOAD_BYTES + 1];
    assert!(matches!(
        decode_link_req_payload(&oversized),
        Err(MonitorPayloadError::PayloadTooLarge { .. })
    ));
}

#[test]
fn link_req_payload_truncated_cbor_returns_error() {
    let req = LinkReqPayload {
        linker: location(62, 22, 241),
        ref_id: 7,
        target: location(63, 11, 251),
        policy_tag: 3,
        reciprocate: 1,
    };
    let bytes = encode_link_req_payload(&req).expect("link req should encode");
    for cut in 1..bytes.len() {
        assert!(
            decode_link_req_payload(&bytes[..cut]).is_err(),
            "link req decoder returned Ok on a {cut}-byte truncation"
        );
    }
}

#[test]
fn monitor_control_frame_round_trips_through_wire_frame() {
    // The monitor payload rides inside a ControlFrame; the full wire frame must
    // round-trip with the right ctrl_kind so the reader dispatches it.
    let req = MonitorReqPayload {
        watcher: location(64, 55, 257),
        ref_id: 4242,
        target: location(65, 99, 263),
    };
    let frame = ControlFrame {
        version: WIRE_VERSION,
        ctrl_kind: CTRL_MONITOR_REQ,
        payload: encode_monitor_req_payload(&req).expect("req should encode"),
    };
    cddl_control_shape(&frame);
    let bytes = encode_control_frame(&frame).expect("control frame should encode");
    let WireFrame::Control(decoded) = decode_wire_frame(&bytes).expect("frame should decode")
    else {
        panic!("expected a control frame");
    };
    assert_eq!(decoded.ctrl_kind, CTRL_MONITOR_REQ);
    assert_eq!(
        decode_monitor_req_payload(&decoded.payload).expect("payload should decode"),
        req
    );
}
