//! libFuzzer entry points for `hew-runtime/fuzz` (feature `fuzzing`).
//!
//! The decoders here read bytes from untrusted peers. Each [`Decoder`] owns
//! one property: decoding never panics, and an accepted value is a fixed point
//! of the matching encoder, so `decode(encode(decode(x)))` equals
//! `decode(x)` and encoding it again yields the same bytes. Non-canonical
//! accepted input is allowed; only the re-encoded form must be stable. Seeds
//! come from the same encoders the runtime sends with.

use std::fmt::Debug;

use hew_codec::Format;

use crate::envelope::{
    decode_control_frame, decode_envelope_frame, decode_link_down_payload, decode_link_req_payload,
    decode_monitor_down_payload, decode_monitor_req_payload, decode_registry_gossip_payload,
    decode_service_offer_payload, decode_setup_result_payload, decode_swim_payload,
    decode_wire_frame, encode_control_frame, encode_envelope_frame, encode_link_down_payload,
    encode_link_req_payload, encode_monitor_down_payload, encode_monitor_req_payload,
    encode_registry_gossip_payload, encode_service_offer_payload, encode_setup_result_payload,
    encode_swim_payload, ControlFrame, EnvelopeFrame, EnvelopeMessage, LinkReqPayload,
    MonitorDownPayload, MonitorReqPayload, NodeSessionIdentity, RegistryGossipPayload, RequestKind,
    ServiceOfferPayload, SetupResultPayload, SwimControlPayload, SwimGossipEntry, WireFrame,
    CTRL_REGISTRY_GOSSIP, CTRL_SWIM, REGISTRY_GOSSIP_OP_ADD, WIRE_VERSION,
};
use crate::node_identity::{Location, NodeId};
use crate::node_protocol_generated as protocol;

pub use crate::connection::fuzz_fixture::{ControlHarness, Member, HARNESS_PEERS};

/// One untrusted-input decoder with its round-trip property and seeds.
#[derive(Debug)]
pub struct Decoder {
    pub name: &'static str,
    /// Panics when the decoder or its round trip breaks the property.
    pub check: fn(&[u8]),
    /// Encoded values the runtime itself produces.
    pub seeds: fn() -> Vec<Vec<u8>>,
}

/// Every decoder `fuzz_wire_frames` drives, selected by the first input byte.
pub const DECODERS: &[Decoder] = &[
    Decoder {
        name: "wire_frame",
        check: check_wire_frame,
        seeds: wire_frame_seeds,
    },
    Decoder {
        name: "control_frame",
        check: |bytes| {
            fixed_point(bytes, decode_control_frame, |v| {
                encode_control_frame(v).ok()
            });
        },
        seeds: || control_frames().iter().map(encode_control).collect(),
    },
    Decoder {
        name: "envelope_frame",
        check: |bytes| {
            fixed_point(bytes, decode_envelope_frame, |v| {
                encode_envelope_frame(v).ok()
            });
        },
        seeds: || {
            envelope_frames()
                .iter()
                .map(|f| encode_envelope_frame(f).expect("seed envelope encodes"))
                .collect()
        },
    },
    Decoder {
        name: "swim_payload",
        check: |bytes| fixed_point(bytes, decode_swim_payload, |v| encode_swim_payload(v).ok()),
        seeds: || {
            swim_payloads()
                .iter()
                .map(|p| encode_swim_payload(p).expect("seed SWIM payload encodes"))
                .collect()
        },
    },
    Decoder {
        name: "registry_gossip_payload",
        check: |bytes| {
            fixed_point(bytes, decode_registry_gossip_payload, |v| {
                encode_registry_gossip_payload(v).ok()
            });
        },
        seeds: || vec![encode_registry_gossip_payload(&registry_payload()).expect("seed")],
    },
    Decoder {
        name: "service_offer_payload",
        check: |bytes| {
            fixed_point(bytes, decode_service_offer_payload, |v| {
                encode_service_offer_payload(v).ok()
            });
        },
        seeds: || vec![encode_service_offer_payload(&service_offer()).expect("seed")],
    },
    Decoder {
        name: "monitor_req_payload",
        check: |bytes| {
            fixed_point(bytes, decode_monitor_req_payload, |v| {
                encode_monitor_req_payload(v).ok()
            });
        },
        seeds: || vec![encode_monitor_req_payload(&monitor_req()).expect("seed")],
    },
    Decoder {
        name: "monitor_down_payload",
        check: |bytes| {
            fixed_point(bytes, decode_monitor_down_payload, |v| {
                encode_monitor_down_payload(v).ok()
            });
        },
        seeds: || {
            monitor_downs()
                .iter()
                .map(|d| encode_monitor_down_payload(d).expect("seed"))
                .collect()
        },
    },
    Decoder {
        name: "link_req_payload",
        check: |bytes| {
            fixed_point(bytes, decode_link_req_payload, |v| {
                encode_link_req_payload(v).ok()
            });
        },
        seeds: || vec![encode_link_req_payload(&link_req()).expect("seed")],
    },
    Decoder {
        name: "setup_result_payload",
        check: |bytes| {
            fixed_point(bytes, decode_setup_result_payload, |v| {
                encode_setup_result_payload(v).ok()
            });
        },
        seeds: || vec![encode_setup_result_payload(&setup_result()).expect("seed")],
    },
    Decoder {
        name: "link_down_payload",
        check: |bytes| {
            fixed_point(bytes, decode_link_down_payload, |v| {
                encode_link_down_payload(v).ok()
            });
        },
        seeds: || {
            monitor_downs()
                .iter()
                .map(|d| encode_link_down_payload(d).expect("seed"))
                .collect()
        },
    },
    Decoder {
        name: "node_handshake",
        check: crate::connection::fuzz_fixture::check_handshake,
        seeds: crate::connection::fuzz_fixture::handshake_seeds,
    },
    generated::<protocol::RelayOpen>("relay_open"),
    generated::<protocol::RelayChunk>("relay_chunk"),
    generated::<protocol::RelayClose>("relay_close"),
    generated::<protocol::ClusterMember>("cluster_member"),
    generated::<protocol::ClusterEnrollment>("cluster_enrollment"),
    generated::<protocol::ServiceOffer>("generated_service_offer"),
    generated::<protocol::MonitorDown>("generated_monitor_down"),
    generated::<protocol::RegistryGossip>("generated_registry_gossip"),
    generated::<protocol::SwimControl>("generated_swim_control"),
    generated::<protocol::NodeHandshake>("generated_node_handshake"),
];

/// Assert the round-trip fixed point for one accepted input.
fn fixed_point<T: PartialEq + Debug, E: std::fmt::Display>(
    bytes: &[u8],
    decode: impl Fn(&[u8]) -> Result<T, E>,
    encode: impl Fn(&T) -> Option<Vec<u8>>,
) {
    let value = match decode(bytes) {
        Ok(value) => value,
        Err(err) => {
            // The reader formats every refusal into its last-error slot.
            let _ = err.to_string();
            return;
        }
    };
    let encoded =
        encode(&value).unwrap_or_else(|| panic!("accepted value does not re-encode: {value:?}"));
    let again = decode(&encoded)
        .unwrap_or_else(|err| panic!("re-encoded value is refused ({err}): {value:?}"));
    assert_eq!(again, value, "re-encoded value decodes differently");
    assert_eq!(
        encode(&again).as_deref(),
        Some(encoded.as_slice()),
        "re-encoding is not stable"
    );
}

fn check_wire_frame(bytes: &[u8]) {
    fixed_point(bytes, decode_wire_frame, |frame| match frame {
        WireFrame::Control(control) => encode_control_frame(control).ok(),
        WireFrame::Envelope(envelope) => encode_envelope_frame(envelope).ok(),
    });
}

const FORMATS: [Format; 5] = [
    Format::Cbor,
    Format::Json,
    Format::Yaml,
    Format::Toml,
    Format::Msgpack,
];

/// A checked-protocol record: the first input byte picks the codec format.
trait Generated: Sized + PartialEq + Debug {
    fn sample() -> Self;
    fn encode_as(&self, format: Format) -> Vec<u8>;
    fn decode_as(format: Format, bytes: &[u8]) -> Result<Self, hew_codec::DecodeError>;
}

macro_rules! generated_codec {
    () => {
        fn encode_as(&self, format: Format) -> Vec<u8> {
            self.encode(format)
        }
        fn decode_as(format: Format, bytes: &[u8]) -> Result<Self, hew_codec::DecodeError> {
            Self::decode(format, bytes)
        }
    };
}

impl Generated for protocol::RelayOpen {
    generated_codec!();
    fn sample() -> Self {
        protocol::RelayOpen {
            circuit: 7,
            origin: wire_session(2),
            target: wire_session(3),
        }
    }
}

impl Generated for protocol::RelayChunk {
    generated_codec!();
    fn sample() -> Self {
        protocol::RelayChunk {
            circuit: 7,
            offset: 4096,
            body: b"relayed bytes".to_vec(),
        }
    }
}

impl Generated for protocol::RelayClose {
    generated_codec!();
    fn sample() -> Self {
        protocol::RelayClose { circuit: 7 }
    }
}

impl Generated for protocol::ClusterMember {
    generated_codec!();
    fn sample() -> Self {
        protocol::ClusterMember {
            transport: 0,
            credential: vec![0x5a; 32],
        }
    }
}

impl Generated for protocol::ClusterEnrollment {
    generated_codec!();
    fn sample() -> Self {
        {
            protocol::ClusterEnrollment {
                authority: vec![0x11; 32],
                member: protocol::ClusterMember {
                    transport: 1,
                    credential: vec![0x30, 0x82, 0x01, 0x0a],
                }
                .encode(Format::Cbor),
                signature: vec![0x22; 64],
            }
        }
    }
}

impl Generated for protocol::ServiceOffer {
    generated_codec!();
    fn sample() -> Self {
        protocol::ServiceOffer {
            grant_id: 7,
            target: wire_location(2, 4),
            recipient: wire_session(1),
            handlers: vec!["tests.Service.read".into(), "tests.Service.write".into()],
            request_id: 3,
        }
    }
}

impl Generated for protocol::MonitorDown {
    generated_codec!();
    fn sample() -> Self {
        protocol::MonitorDown {
            ref_id: 19,
            target: wire_location(2, 4),
            reason: protocol::DownReason { tag: 1, code: -1 },
            crash_kind: 2,
            end_reason: 2,
            close_fault: Some(protocol::CleanupFault {
                code: -7,
                message: "cleanup café".into(),
            }),
        }
    }
}

impl Generated for protocol::RegistryGossip {
    generated_codec!();
    fn sample() -> Self {
        {
            protocol::RegistryGossip {
                op: REGISTRY_GOSSIP_OP_ADD,
                name: "worker café".into(),
                location: wire_location(2, 4),
            }
        }
    }
}

impl Generated for protocol::SwimControl {
    generated_codec!();
    fn sample() -> Self {
        protocol::SwimControl {
            msg_type: crate::cluster::SWIM_MSG_PING_REQ,
            from: wire_session(2),
            incarnation: 3,
            target: Some(wire_session(3)),
            gossip: vec![protocol::SwimGossip {
                member: wire_session(3),
                state: crate::cluster::MEMBER_SUSPECT,
                incarnation: 2,
            }],
        }
    }
}

impl Generated for protocol::NodeHandshake {
    generated_codec!();
    fn sample() -> Self {
        {
            protocol::NodeHandshake {
                protocol_version: 1,
                schema_hash: 0xdead_beef,
                feature_flags: 3,
                node: session(2).node_id.to_bytes().to_vec(),
                incarnation: 1,
                noise_key: vec![0x42; 32],
                enrollment: Vec::new(),
            }
        }
    }
}

const fn generated<T: Generated>(name: &'static str) -> Decoder {
    Decoder {
        name,
        check: check_generated::<T>,
        seeds: generated_seeds::<T>,
    }
}

fn check_generated<T: Generated>(bytes: &[u8]) {
    let Some((&selector, payload)) = bytes.split_first() else {
        return;
    };
    let format = FORMATS[usize::from(selector) % FORMATS.len()];
    fixed_point(
        payload,
        |bytes| T::decode_as(format, bytes),
        |value| Some(value.encode_as(format)),
    );
}

fn generated_seeds<T: Generated>() -> Vec<Vec<u8>> {
    let sample = T::sample();
    (0_u8..)
        .zip(FORMATS)
        .map(|(selector, format)| {
            let mut seed = vec![selector];
            seed.extend(sample.encode_as(format));
            seed
        })
        .collect()
}

fn session(slot: u16) -> NodeSessionIdentity {
    let mut bytes = [0_u8; 16];
    bytes[0] = 0x48;
    bytes[14..].copy_from_slice(&slot.to_be_bytes());
    NodeSessionIdentity {
        node_id: NodeId::from_bytes(bytes),
        session_incarnation: 1,
    }
}

fn location(slot: u16, actor: u64) -> Location {
    Location::direct(session(slot).node_id, actor, 1).expect("nonzero seed location")
}

fn wire_session(slot: u16) -> protocol::WireSession {
    protocol::WireSession {
        node: session(slot).node_id.to_bytes().to_vec(),
        incarnation: 1,
    }
}

fn wire_location(slot: u16, actor: u64) -> protocol::WireLocation {
    protocol::WireLocation {
        node: session(slot).node_id.to_bytes().to_vec(),
        target_id: actor,
        incarnation: 1,
        target_kind: 0,
    }
}

fn swim_payloads() -> Vec<SwimControlPayload> {
    let gossip = vec![
        SwimGossipEntry {
            member: session(3),
            state: crate::cluster::MEMBER_SUSPECT,
            incarnation: 2,
        },
        SwimGossipEntry {
            member: session(1),
            state: crate::cluster::MEMBER_DEAD,
            incarnation: 1,
        },
    ];
    vec![
        SwimControlPayload {
            msg_type: crate::cluster::SWIM_MSG_PING,
            from: session(2),
            incarnation: 1,
            target: None,
            gossip: Vec::new(),
        },
        SwimControlPayload {
            msg_type: crate::cluster::SWIM_MSG_ACK,
            from: session(2),
            incarnation: 4,
            target: None,
            gossip: gossip.clone(),
        },
        SwimControlPayload {
            msg_type: crate::cluster::SWIM_MSG_PING_REQ,
            from: session(2),
            incarnation: 1,
            target: Some(session(3)),
            gossip,
        },
    ]
}

fn registry_payload() -> RegistryGossipPayload {
    RegistryGossipPayload {
        op: REGISTRY_GOSSIP_OP_ADD,
        name: "worker café".into(),
        location: location(2, 4),
    }
}

fn service_offer() -> ServiceOfferPayload {
    ServiceOfferPayload {
        request_id: 3,
        grant_id: 7,
        target: location(2, 4),
        recipient: session(1),
        handlers: vec!["tests.Service.read".into(), "tests.Service.write".into()],
    }
}

fn monitor_req() -> MonitorReqPayload {
    MonitorReqPayload {
        grant_id: 7,
        watcher: location(1, 9),
        ref_id: 19,
        target: location(2, 4),
        setup_id: 5,
    }
}

fn monitor_downs() -> Vec<MonitorDownPayload> {
    vec![
        MonitorDownPayload {
            ref_id: 19,
            target: location(2, 4),
            reason: 1,
            end_reason: 2,
            crash_kind: 2,
            close_fault: Some((-7, "cleanup café".into())),
        },
        MonitorDownPayload {
            ref_id: 20,
            target: location(2, 5),
            reason: 0,
            end_reason: 0,
            crash_kind: 0,
            close_fault: None,
        },
    ]
}

fn link_req() -> LinkReqPayload {
    LinkReqPayload {
        grant_id: 7,
        reciprocal_ref: 8,
        linker: location(1, 9),
        ref_id: 19,
        target: location(2, 4),
        policy_tag: 1,
        reciprocate: 1,
        setup_id: 5,
    }
}

fn setup_result() -> SetupResultPayload {
    SetupResultPayload {
        setup_id: 5,
        ref_id: 19,
        target: location(2, 4),
        status: crate::envelope::SETUP_STATUS_ACCEPTED,
    }
}

fn envelope_frames() -> Vec<EnvelopeFrame> {
    vec![
        EnvelopeFrame {
            version: WIRE_VERSION,
            target: Some(location(1, 9)),
            source: Some(location(2, 4)),
            message: EnvelopeMessage::Service {
                handler: "tests.Service.read".into(),
                grant_id: 7,
                kind: RequestKind::CallWait,
            },
            payload: b"\xa1\x01\x02".to_vec(),
            request_id: 3,
            budget_ns: Some(1_000_000),
        },
        EnvelopeFrame {
            version: WIRE_VERSION,
            target: None,
            source: None,
            message: EnvelopeMessage::Reply,
            payload: b"reply".to_vec(),
            request_id: 3,
            budget_ns: None,
        },
    ]
}

/// Control frames carrying one seed payload of each control kind.
#[must_use]
pub fn control_frames() -> Vec<ControlFrame> {
    let control = |ctrl_kind, payload| ControlFrame {
        version: WIRE_VERSION,
        ctrl_kind,
        payload,
    };
    let mut frames: Vec<ControlFrame> = swim_payloads()
        .iter()
        .map(|p| control(CTRL_SWIM, encode_swim_payload(p).expect("seed")))
        .collect();
    frames.push(control(
        CTRL_REGISTRY_GOSSIP,
        encode_registry_gossip_payload(&registry_payload()).expect("seed"),
    ));
    frames.push(control(
        crate::envelope::CTRL_SERVICE_OFFER,
        encode_service_offer_payload(&service_offer()).expect("seed"),
    ));
    frames.push(control(
        crate::envelope::CTRL_MONITOR_REQ,
        encode_monitor_req_payload(&monitor_req()).expect("seed"),
    ));
    frames.push(control(
        crate::envelope::CTRL_MONITOR_DOWN,
        encode_monitor_down_payload(&monitor_downs()[0]).expect("seed"),
    ));
    frames.push(control(
        crate::envelope::CTRL_LINK_REQ,
        encode_link_req_payload(&link_req()).expect("seed"),
    ));
    frames.push(control(
        crate::envelope::CTRL_MONITOR_SETUP_RESULT,
        encode_setup_result_payload(&setup_result()).expect("seed"),
    ));
    frames.push(control(
        crate::envelope::CTRL_LINK_DOWN,
        encode_link_down_payload(&monitor_downs()[1]).expect("seed"),
    ));
    frames.push(control(crate::envelope::CTRL_RELAY_CLOSE, {
        protocol::RelayClose { circuit: 7 }.encode(Format::Cbor)
    }));
    frames
}

fn encode_control(frame: &ControlFrame) -> Vec<u8> {
    encode_control_frame(frame).expect("seed control frame encodes")
}

fn wire_frame_seeds() -> Vec<Vec<u8>> {
    let mut seeds: Vec<Vec<u8>> = control_frames().iter().map(encode_control).collect();
    seeds.extend(
        envelope_frames()
            .iter()
            .map(|f| encode_envelope_frame(f).expect("seed envelope encodes")),
    );
    seeds
}

/// Split a `fuzz_control_handler` input into `(peer, frame)` deliveries:
/// each record is one peer byte, a big-endian `u16` length and the frame.
#[must_use]
pub fn split_deliveries(mut data: &[u8]) -> Vec<(usize, &[u8])> {
    let mut deliveries = Vec::new();
    while let [peer, high, low, rest @ ..] = data {
        let len = usize::from(u16::from_be_bytes([*high, *low])).min(rest.len());
        let (frame, tail) = rest.split_at(len);
        deliveries.push((usize::from(*peer), frame));
        data = tail;
    }
    deliveries
}

/// The inverse of [`split_deliveries`].
///
/// # Panics
/// Panics when a frame exceeds `u16::MAX` bytes.
#[must_use]
pub fn join_deliveries(deliveries: &[(usize, Vec<u8>)]) -> Vec<u8> {
    let mut data = Vec::new();
    for (peer, frame) in deliveries {
        data.push(u8::try_from(*peer).expect("peer index fits a byte"));
        data.extend(
            u16::try_from(frame.len())
                .expect("seed frame fits")
                .to_be_bytes(),
        );
        data.extend(frame);
    }
    data
}

/// Delivery sequences built from frames the runtime sends, addressed with
/// the harness peers' authenticated identities.
#[must_use]
pub fn control_handler_seeds() -> Vec<Vec<u8>> {
    use crate::cluster::{
        MEMBER_ALIVE, MEMBER_DEAD, MEMBER_SUSPECT, SWIM_MSG_ACK, SWIM_MSG_PING, SWIM_MSG_PING_REQ,
    };
    let peer = ControlHarness::peer_session;
    let local = ControlHarness::local_session();
    let swim = |from: usize,
                msg_type,
                incarnation,
                target,
                gossip: Vec<(NodeSessionIdentity, i32, u64)>| {
        let payload = SwimControlPayload {
            msg_type,
            from: peer(from),
            incarnation,
            target,
            gossip: gossip
                .into_iter()
                .map(|(member, state, incarnation)| SwimGossipEntry {
                    member,
                    state,
                    incarnation,
                })
                .collect(),
        };
        encode_control(&ControlFrame {
            version: WIRE_VERSION,
            ctrl_kind: CTRL_SWIM,
            payload: encode_swim_payload(&payload).expect("seed SWIM payload"),
        })
    };
    let registry = |from: usize, op| {
        encode_control(&ControlFrame {
            version: WIRE_VERSION,
            ctrl_kind: CTRL_REGISTRY_GOSSIP,
            payload: encode_registry_gossip_payload(&RegistryGossipPayload {
                op,
                name: "worker".into(),
                location: Location::direct(peer(from).node_id, 4, 1).expect("seed location"),
            })
            .expect("seed registry payload"),
        })
    };
    let sequences: Vec<Vec<(usize, Vec<u8>)>> = vec![
        vec![(0, swim(0, SWIM_MSG_PING, 1, None, vec![]))],
        vec![(
            0,
            swim(0, SWIM_MSG_ACK, 1, None, vec![(peer(1), MEMBER_SUSPECT, 1)]),
        )],
        vec![
            (0, swim(0, SWIM_MSG_PING_REQ, 1, Some(peer(1)), vec![])),
            (1, swim(1, SWIM_MSG_ACK, 1, None, vec![])),
        ],
        vec![
            (
                0,
                swim(0, SWIM_MSG_PING, 1, None, vec![(local, MEMBER_SUSPECT, 1)]),
            ),
            (
                1,
                swim(1, SWIM_MSG_PING, 1, None, vec![(local, MEMBER_DEAD, 2)]),
            ),
        ],
        vec![
            (
                0,
                swim(0, SWIM_MSG_ACK, 2, None, vec![(peer(1), MEMBER_DEAD, 3)]),
            ),
            (
                1,
                swim(1, SWIM_MSG_PING, 4, None, vec![(peer(1), MEMBER_ALIVE, 4)]),
            ),
        ],
        vec![(
            2,
            swim(2, SWIM_MSG_PING, 1, None, vec![(peer(0), MEMBER_DEAD, 9)]),
        )],
        vec![
            (0, registry(0, REGISTRY_GOSSIP_OP_ADD)),
            (0, registry(0, crate::envelope::REGISTRY_GOSSIP_OP_REMOVE)),
        ],
        control_frames()
            .iter()
            .map(|frame| (0, encode_control(frame)))
            .collect(),
    ];
    sequences
        .iter()
        .map(|sequence| join_deliveries(sequence))
        .collect()
}
