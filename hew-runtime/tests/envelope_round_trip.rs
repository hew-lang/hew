//! Round-trip tests for the CBOR wire envelope types.
//!
//! These tests exercise the serde-derived serialisation via ciborium directly
//! (no codec module yet — that's W2). The gate is: construct an envelope,
//! serialise to CBOR bytes, deserialise, assert equality.
//!
//! Tests also verify the version constant and type discriminants match the
//! values named in `schemas/envelope.cddl`.

use hew_runtime::envelope::{
    ControlFrame, EnvelopeFrame, FRAME_TYPE_CONTROL, FRAME_TYPE_ENVELOPE, WIRE_VERSION,
};

/// Serialise `value` to a CBOR byte vec using ciborium.
fn to_cbor<T: serde::Serialize>(value: &T) -> Vec<u8> {
    let mut buf = Vec::new();
    ciborium::ser::into_writer(value, &mut buf).expect("ciborium serialisation failed");
    buf
}

/// Deserialise a CBOR byte slice into `T` using ciborium.
fn from_cbor<T: serde::de::DeserializeOwned>(buf: &[u8]) -> T {
    ciborium::de::from_reader(buf).expect("ciborium deserialisation failed")
}

// ── Schema constant tests ─────────────────────────────────────────────────

#[test]
fn wire_version_is_one() {
    assert_eq!(
        WIRE_VERSION, 1,
        "WIRE_VERSION must match schemas/envelope.cddl wire-version = 1"
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

// ── EnvelopeFrame round-trip tests ───────────────────────────────────────

#[test]
fn envelope_frame_fire_and_forget_round_trips() {
    let original = EnvelopeFrame::fire_and_forget(
        42,            // target_actor_id
        7,             // source_actor_id
        3,             // msg_type
        vec![1, 2, 3], // payload
    );

    let bytes = to_cbor(&original);
    let decoded: EnvelopeFrame = from_cbor(&bytes);

    assert_eq!(original, decoded);
}

#[test]
fn envelope_frame_with_request_id_round_trips() {
    let original = EnvelopeFrame {
        version: WIRE_VERSION,
        target_actor_id: 100,
        source_actor_id: 200,
        msg_type: 0,
        payload: b"hello".to_vec(),
        request_id: 99,
        source_node_id: 5,
    };

    let bytes = to_cbor(&original);
    let decoded: EnvelopeFrame = from_cbor(&bytes);

    assert_eq!(original, decoded);
}

#[test]
fn envelope_frame_empty_payload_round_trips() {
    let original = EnvelopeFrame::fire_and_forget(1, 1, 0, vec![]);

    let bytes = to_cbor(&original);
    let decoded: EnvelopeFrame = from_cbor(&bytes);

    assert_eq!(original, decoded);
}

#[test]
fn envelope_frame_large_payload_round_trips() {
    let payload: Vec<u8> = (0u8..=255).cycle().take(4096).collect();
    let original = EnvelopeFrame::fire_and_forget(1000, 2000, 7, payload);

    let bytes = to_cbor(&original);
    let decoded: EnvelopeFrame = from_cbor(&bytes);

    assert_eq!(original, decoded);
}

#[test]
fn envelope_frame_max_field_values_round_trips() {
    let original = EnvelopeFrame {
        version: WIRE_VERSION,
        target_actor_id: u64::MAX,
        source_actor_id: u64::MAX - 1,
        msg_type: i32::MAX,
        payload: vec![0xFF; 16],
        request_id: u64::MAX,
        source_node_id: u16::MAX,
    };

    let bytes = to_cbor(&original);
    let decoded: EnvelopeFrame = from_cbor(&bytes);

    assert_eq!(original, decoded);
}

// ── ControlFrame round-trip tests ─────────────────────────────────────────

#[test]
fn control_frame_bare_signal_round_trips() {
    let original = ControlFrame {
        version: WIRE_VERSION,
        ctrl_kind: 0,
        payload: vec![],
    };

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

// ── Serialised output is non-empty and stable ─────────────────────────────

#[test]
fn envelope_frame_serialises_to_nonempty_bytes() {
    let frame = EnvelopeFrame::fire_and_forget(1, 2, 0, vec![]);
    let bytes = to_cbor(&frame);
    assert!(!bytes.is_empty(), "CBOR output must not be empty");
}

#[test]
fn same_envelope_produces_same_bytes() {
    let frame = EnvelopeFrame::fire_and_forget(1, 2, 0, vec![10, 20]);
    let bytes1 = to_cbor(&frame);
    let bytes2 = to_cbor(&frame);
    assert_eq!(bytes1, bytes2, "serialisation must be deterministic");
}
