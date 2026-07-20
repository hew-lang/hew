//! CBOR wire envelope types — the Rust-native representation of the Hew wire
//! protocol described by `schemas/envelope.cddl`.
//!
//! This module contains the native CBOR type definitions plus the small
//! fail-closed codec surface used by runtime call sites.
//!
//! # Payload encoding
//!
//! Runtime frames are encoded as definite-length CBOR maps with unsigned
//! integer keys. Key `2` carries the explicit `frame_type` discriminator, and
//! payload fields are encoded as CBOR byte strings (`bstr`), not arrays.
//! `schemas/envelope.cddl` is the wire contract.
//!
//! # Wire format history
//!
//! The HBF (Hew Binary Format) era closed when the CBOR-native wire format landed. This module is the
//! CBOR-native replacement: `EnvelopeFrame` owns the payload as a `Vec<u8>`,
//! the CDDL schema is the doc-of-truth, and there is no fallback HBF path.

use std::collections::BTreeMap;

use ciborium::value::{Integer, Value};
use serde::ser::SerializeMap;
use serde::{Deserialize, Deserializer, Serialize, Serializer};

use crate::node_identity::{Location, NodeId};

/// Current wire protocol version.
///
/// Frames carrying any other version value MUST be rejected by the decoder.
pub const WIRE_VERSION: u8 = 2;

/// Discriminant for a control frame (`frame_type` field value 0).
pub const FRAME_TYPE_CONTROL: u8 = 0;

/// Discriminant for an envelope frame (`frame_type` field value 1).
pub const FRAME_TYPE_ENVELOPE: u8 = 1;

/// Control-frame kind for registry-name gossip.
pub const CTRL_REGISTRY_GOSSIP: u64 = 1;

/// Control-frame kind for SWIM failure-detection protocol messages
/// (PING / ACK / `PING_REQ` with piggybacked membership gossip).
pub const CTRL_SWIM: u64 = 2;

/// Control-frame kind for a cross-node monitor request.
///
/// Sent by a watcher node to the node owning the target actor: "node W,
/// registration `ref_id`, is monitoring your actor `serial`." The owning node
/// records a remote-watcher entry so it can fan out a `CTRL_MONITOR_DOWN` when
/// the target reaches a terminal state.
pub const CTRL_MONITOR_REQ: u64 = 3;

/// Control-frame kind for a cross-node monitor DOWN notification.
///
/// Sent by the node owning a monitored actor back to the watcher node when the
/// target reaches a terminal state (clean exit / crash). Carries the watcher's
/// `ref_id` and the terminal reason code.
pub const CTRL_MONITOR_DOWN: u64 = 4;

/// Control-frame kind for a cross-node demonitor request.
///
/// Sent by a watcher node to retract a prior `CTRL_MONITOR_REQ`: "node W,
/// registration `ref_id`, no longer watches your actor `serial`." The owning
/// node removes the remote-watcher entry. Idempotent on the receiver.
pub const CTRL_DEMONITOR: u64 = 5;

/// Control-frame kind for a cross-node link request.
///
/// Sent by a linker node to the node owning the target actor: "node L, actor
/// `linker_serial`, registration `ref_id`, links your actor `target_serial`
/// under `policy_tag`." A cross-node link is BIDIRECTIONAL — the owning node
/// reciprocates by registering a reverse link so the linker's death also fires
/// into the target — so it carries both directions' identity. On the target
/// node this records a target-side remote-linker entry (so the target's death
/// fans a `CTRL_LINK_DOWN` back) AND a reverse watcher entry (so the linker's
/// death crashes the target per the policy).
pub const CTRL_LINK_REQ: u64 = 6;

/// Control-frame kind for a cross-node link DOWN notification.
///
/// Sent to a linked node when its remote peer reaches a terminal state. Unlike
/// a `CTRL_MONITOR_DOWN` (which arms a recv slot a program polls), a
/// `CrashLinked` `CTRL_LINK_DOWN` synthesizes a `SYS_MSG_EXIT` into the local
/// linked actor's MAILBOX and crashes it (the OTP fail-together semantic).
/// Carries the linked node's `ref_id` and the terminal reason code.
pub const CTRL_LINK_DOWN: u64 = 7;

/// Control-frame kind for a cross-node unlink request.
///
/// Sent by a linker node to retract a prior `CTRL_LINK_REQ`. The owning node
/// removes both the target-side remote-linker entry and the reverse watcher
/// entry. Idempotent on the receiver. Carries the same identifying payload as
/// `CTRL_LINK_REQ`.
pub const CTRL_UNLINK: u64 = 8;

/// Maximum accepted cross-node link control payload size, in bytes.
///
/// A link request payload is a fixed small CBOR map of bounded integers (two
/// node ids, two serials, a ref id, a policy tag); a link DOWN reuses the
/// monitor DOWN shape. 256 bytes is far more than any conformant payload needs
/// and bounds a malicious peer's allocation — a HIGHER-stakes bound than
/// monitor because a fabricated `CTRL_LINK_DOWN` would crash a REAL actor.
pub const MAX_LINK_PAYLOAD_BYTES: usize = 256;

/// Maximum accepted cross-node monitor control payload size, in bytes.
///
/// A monitor request / down / demonitor payload is a fixed small CBOR map of
/// bounded integers (node id, ref id, serial / reason). 256 bytes is far more
/// than any conformant payload needs and bounds a malicious peer's allocation.
pub const MAX_MONITOR_PAYLOAD_BYTES: usize = 256;

/// Maximum accepted SWIM control payload size, in bytes.
///
/// Bounds the piggybacked gossip list so a malicious or buggy peer cannot
/// force an unbounded allocation on decode. A single SWIM frame carries one
/// protocol message plus a small bounded gossip batch; 8 KiB is generous.
pub const MAX_SWIM_PAYLOAD_BYTES: usize = 8192;

/// Maximum number of piggybacked membership-gossip entries per SWIM frame.
///
/// Mirrors `ClusterConfig::max_gossip_per_msg`; the decoder rejects frames
/// carrying more so an oversized list cannot be smuggled past the byte cap
/// via tightly-packed small integers.
pub const MAX_SWIM_GOSSIP_ENTRIES: usize = 64;

/// Registry-gossip payload op for actor-name registration.
///
/// Matches `cluster::GOSSIP_REGISTRY_ADD` on native targets.
pub const REGISTRY_GOSSIP_OP_ADD: u8 = 5;

/// Registry-gossip payload op for actor-name removal.
///
/// Matches `cluster::GOSSIP_REGISTRY_REMOVE` on native targets.
pub const REGISTRY_GOSSIP_OP_REMOVE: u8 = 6;

/// Maximum accepted registry-gossip control payload size.
pub const MAX_REGISTRY_GOSSIP_PAYLOAD_BYTES: usize = 4096;

/// Maximum accepted registry name length on the gossip wire, in UTF-8 bytes.
pub const MAX_REGISTRY_GOSSIP_NAME_BYTES: usize = 1024;

/// A control frame: node-level signalling with an opaque byte payload.
///
/// CDDL: `control-frame` rule in `schemas/envelope.cddl`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ControlFrame {
    /// Wire protocol version. MUST equal [`WIRE_VERSION`].
    ///
    /// CDDL key 1.
    pub version: u8,

    /// Protocol-defined control kind tag.
    ///
    /// CDDL key 3.
    pub ctrl_kind: u64,

    /// Opaque payload. Empty for bare signals.
    ///
    /// CDDL key 4.
    pub payload: Vec<u8>,
}

/// An envelope frame: a single actor-to-actor message.
///
/// CDDL: `envelope-frame` rule in `schemas/envelope.cddl`.
///
/// Field numbers are stable across protocol versions; new fields use new
/// keys and leave older decoders unaffected.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnvelopeFrame {
    /// Wire protocol version. MUST equal [`WIRE_VERSION`].
    ///
    /// CDDL key 1.
    pub version: u8,

    /// Target actor identity. `None` only for reply envelopes.
    ///
    /// CDDL key 3.
    pub target: Option<Location>,

    /// Source actor identity when the sender is an actor.
    ///
    /// CDDL key 4.
    pub source: Option<Location>,

    /// Message type tag.
    ///
    /// Signed; valid range `0..=2^30-1`.
    ///
    /// CDDL key 5.
    pub msg_type: i32,

    /// Serialised message body. May be empty for zero-payload messages.
    ///
    /// Collapses the legacy `(payload_size: u32, payload: *mut u8)` C-FFI
    /// pair into a single owned byte string. See `schemas/README.md`.
    ///
    /// CDDL key 6.
    pub payload: Vec<u8>,

    /// Request ID for distributed ask/reply correlation.
    ///
    /// `0` = fire-and-forget. `> 0` = a reply is expected (or this frame
    /// is a reply to a prior ask with this ID).
    ///
    /// CDDL key 7.
    pub request_id: u64,
}

/// Top-level wire-frame dispatch.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WireFrame {
    /// Node-level signalling frame.
    Control(ControlFrame),
    /// Actor-to-actor message frame.
    Envelope(EnvelopeFrame),
}

/// Bounded registry-gossip control payload.
///
/// Encoded as a definite CBOR map `{1: op, 2: name, 3: location}`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RegistryGossipPayload {
    /// Registry operation (`REGISTRY_GOSSIP_OP_ADD` or `_REMOVE`).
    pub op: u8,
    /// Registered actor name.
    pub name: String,
    /// Exact actor identity being added or retracted.
    pub location: Location,
}

/// Key-derived node identity paired with its durable session incarnation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NodeSessionIdentity {
    /// Key-derived node identity.
    pub node_id: NodeId,
    /// Durable session incarnation.
    pub session_incarnation: u32,
}

/// A single piggybacked membership-gossip entry carried on a SWIM frame.
///
/// Encoded as a definite CBOR map `{1: node_id, 2: state, 3: incarnation}`.
/// This is the SWIM infection-style dissemination vehicle (C6): every PING /
/// ACK carries a bounded batch of recent membership transitions so that nodes
/// not directly connected to a failing peer still learn of its state change.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SwimGossipEntry {
    /// Affected member identity and durable session.
    pub member: NodeSessionIdentity,
    /// New member state (`MEMBER_ALIVE` / `_SUSPECT` / `_DEAD` / `_LEFT`).
    pub state: i32,
    /// Incarnation of the affected member at the time of the transition.
    pub incarnation: u64,
}

/// Bounded SWIM control payload.
///
/// Encoded as a definite CBOR map
/// `{1: msg_type, 2: from_node, 3: incarnation, 4: target_node, 5: [gossip]}`.
///
/// - `msg_type` is one of the `SWIM_MSG_*` constants (PING / ACK / `PING_REQ`).
/// - `from_node` is the sender's node ID. The receiver cross-checks this
///   against the authenticated handshake identity of the connection the frame
///   arrived on and rejects mismatches (cross-attribution defence).
/// - `incarnation` is the sender's current incarnation (used for ACKs and
///   self-refutation).
/// - `target_node` is the probe subject for `PING_REQ` indirect probing (C4);
///   `0` for direct PING / ACK.
/// - `gossip` is the bounded piggybacked membership batch (C6).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SwimControlPayload {
    /// SWIM message type (`SWIM_MSG_PING` / `_ACK` / `_PING_REQ`).
    pub msg_type: i32,
    /// Sender's authenticated identity and durable session.
    pub from: NodeSessionIdentity,
    /// Sender's incarnation number.
    pub incarnation: u64,
    /// Indirect-probe target for `PING_REQ`; absent for direct messages.
    pub target: Option<NodeSessionIdentity>,
    /// Piggybacked membership-gossip batch.
    pub gossip: Vec<SwimGossipEntry>,
}

/// Bounded cross-node monitor REQUEST / DEMONITOR control payload.
///
/// Encoded as a definite CBOR map `{1: watcher_node_id, 2: ref_id,
/// 3: target_serial}`. Used by both `CTRL_MONITOR_REQ` and `CTRL_DEMONITOR`:
/// the request registers, the demonitor retracts, the identifying triple
/// `(watcher_node_id, ref_id, target_serial)` is the same.
///
/// - `watcher_node_id` is the monitoring node's id; the receiver routes the
///   eventual `CTRL_MONITOR_DOWN` back to it.
/// - `ref_id` is the watcher's local monitor-registration id (the `MonitorRef`
///   value); the receiver echoes it verbatim in the DOWN so the watcher can
///   match the registration.
/// - `target_serial` is the monitored actor's serial on the owning node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MonitorReqPayload {
    /// Exact watcher actor identity.
    pub watcher: Location,
    /// Watcher's local monitor-registration id.
    pub ref_id: u64,
    /// Exact monitored actor identity.
    pub target: Location,
}

/// Bounded cross-node monitor DOWN control payload.
///
/// Encoded as a definite CBOR map `{1: ref_id, 2: reason}`. Sent from the node
/// owning the monitored actor back to the watcher node when the target reaches
/// a terminal state.
///
/// - `ref_id` echoes the watcher's monitor-registration id from the request, so
///   the watcher matches the DOWN to its `MonitorRef`.
/// - `reason` is the carried terminal reason: the target's terminal
///   `HewActorState` value for a clean exit / crash (verbatim, R4 fidelity).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MonitorDownPayload {
    /// Watcher's monitor-registration id (echoed from the request).
    pub ref_id: u64,
    /// Exact actor whose terminal state triggered the DOWN.
    pub target: Location,
    /// Carried terminal reason code, encoded with a typed reason tag.
    pub reason: i32,
}

/// Bounded cross-node link REQUEST / UNLINK control payload.
///
/// Encoded as a definite CBOR map `{1: linker_node_id, 2: ref_id,
/// 3: target_serial, 4: linker_serial, 5: policy_tag, 6: reciprocate}`. Used by both
/// `CTRL_LINK_REQ` and `CTRL_UNLINK`: the request establishes the bidirectional
/// link, the unlink retracts it; the identifying tuple is the same.
///
/// - `linker_node_id` is the linking node's id; the receiver routes the eventual
///   `CTRL_LINK_DOWN` back to it AND registers a reverse link toward it.
/// - `ref_id` is the linker's local link-registration id; the receiver echoes it
///   verbatim in the DOWN so the linker can match the registration and find the
///   local actor to crash.
/// - `target_serial` is the linked actor's serial on the owning (receiver) node.
/// - `linker_serial` is the LINKING actor's serial on the linker node — so the
///   receiver's reverse link knows which remote actor to crash when its own
///   local linked actor dies (the OTP bidirectional half).
/// - `policy_tag` is the `PartitionPolicy` discriminant governing the link
///   (`CrashLinked` == 3 crashes the linked actor on the peer's death).
/// - `reciprocate` is 1 for the ORIGINAL request (the receiver registers the
///   reverse half and sends a `reciprocate = 0` reverse request back) and 0 for
///   that reverse request (the original linker completes its target-side
///   registration and does NOT reply again — bounding the handshake to one round
///   trip, never an infinite reciprocation).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LinkReqPayload {
    /// Exact linking actor identity.
    pub linker: Location,
    /// Linker's local link-registration id.
    pub ref_id: u64,
    /// Exact linked actor identity on the owning node.
    pub target: Location,
    /// `PartitionPolicy` discriminant governing the link.
    pub policy_tag: u8,
    /// 1 = original request (receiver reciprocates); 0 = the reverse request.
    pub reciprocate: u8,
}

struct PayloadBytes<'a>(&'a [u8]);

impl Serialize for PayloadBytes<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_bytes(self.0)
    }
}

impl Serialize for ControlFrame {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut map = serializer.serialize_map(Some(4))?;
        map.serialize_entry(&1u64, &self.version)?;
        map.serialize_entry(&2u64, &FRAME_TYPE_CONTROL)?;
        map.serialize_entry(&3u64, &self.ctrl_kind)?;
        map.serialize_entry(&4u64, &PayloadBytes(&self.payload))?;
        map.end()
    }
}

impl Serialize for EnvelopeFrame {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut map = serializer.serialize_map(Some(7))?;
        map.serialize_entry(&1u64, &self.version)?;
        map.serialize_entry(&2u64, &FRAME_TYPE_ENVELOPE)?;
        map.serialize_entry(&3u64, &location_option_to_value(self.target))?;
        map.serialize_entry(&4u64, &location_option_to_value(self.source))?;
        map.serialize_entry(&5u64, &self.msg_type)?;
        map.serialize_entry(&6u64, &PayloadBytes(&self.payload))?;
        map.serialize_entry(&7u64, &self.request_id)?;
        map.end()
    }
}

impl<'de> Deserialize<'de> for ControlFrame {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let value = Value::deserialize(deserializer)?;
        control_frame_from_value(&value).map_err(serde::de::Error::custom)
    }
}

impl<'de> Deserialize<'de> for EnvelopeFrame {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let value = Value::deserialize(deserializer)?;
        envelope_frame_from_value(&value).map_err(serde::de::Error::custom)
    }
}

/// Errors returned by the fail-closed envelope encoder.
#[derive(Debug)]
pub enum EncodeError {
    /// A caller supplied a non-zero payload length with a null payload pointer.
    NullPayload { payload_len: usize },
    /// The message type is outside the range supported by the legacy runtime
    /// message dispatch surface.
    InvalidMsgType { msg_type: i32 },
    /// The underlying CBOR writer failed.
    Cbor(ciborium::ser::Error<std::io::Error>),
}

impl std::fmt::Display for EncodeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NullPayload { payload_len } => {
                write!(
                    f,
                    "payload pointer is null for non-zero payload_len {payload_len}"
                )
            }
            Self::InvalidMsgType { msg_type } => {
                write!(f, "msg_type {msg_type} out of valid range")
            }
            Self::Cbor(e) => write!(f, "CBOR encode failed: {e}"),
        }
    }
}

impl std::error::Error for EncodeError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Cbor(e) => Some(e),
            Self::NullPayload { .. } | Self::InvalidMsgType { .. } => None,
        }
    }
}

impl From<ciborium::ser::Error<std::io::Error>> for EncodeError {
    fn from(e: ciborium::ser::Error<std::io::Error>) -> Self {
        Self::Cbor(e)
    }
}

/// Errors returned by the bounded registry-gossip payload codec.
#[derive(Debug)]
pub enum RegistryGossipPayloadError {
    /// Payload exceeded [`MAX_REGISTRY_GOSSIP_PAYLOAD_BYTES`].
    PayloadTooLarge { len: usize, max: usize },
    /// Payload CBOR was malformed or truncated.
    CborDecode(ciborium::de::Error<std::io::Error>),
    /// Payload CBOR encoding failed.
    CborEncode(ciborium::ser::Error<std::io::Error>),
    /// Top-level payload shape was not the expected map.
    MalformedPayload { reason: &'static str },
    /// A payload map key was malformed.
    MalformedKey,
    /// Required payload key was absent.
    MissingKey { key: u64 },
    /// Unsupported payload key was present.
    UnknownKey { key: u64 },
    /// Payload key appeared more than once.
    DuplicateKey { key: u64 },
    /// Payload field had the wrong CBOR type or value range.
    MalformedField { key: u64, expected: &'static str },
    /// Registry op was not add or remove.
    InvalidOp { op: u8 },
}

impl std::fmt::Display for RegistryGossipPayloadError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::PayloadTooLarge { len, max } => {
                write!(f, "registry gossip payload is too large ({len} > {max})")
            }
            Self::CborDecode(e) => write!(f, "registry gossip CBOR decode failed: {e}"),
            Self::CborEncode(e) => write!(f, "registry gossip CBOR encode failed: {e}"),
            Self::MalformedPayload { reason } => {
                write!(f, "malformed registry gossip payload: {reason}")
            }
            Self::MalformedKey => write!(
                f,
                "registry gossip payload map key is not a non-negative integer"
            ),
            Self::MissingKey { key } => {
                write!(f, "registry gossip payload is missing required key {key}")
            }
            Self::UnknownKey { key } => {
                write!(f, "registry gossip payload contains unsupported key {key}")
            }
            Self::DuplicateKey { key } => {
                write!(f, "registry gossip payload contains duplicate key {key}")
            }
            Self::MalformedField { key, expected } => {
                write!(f, "registry gossip payload key {key} is not {expected}")
            }
            Self::InvalidOp { op } => write!(f, "registry gossip op {op} is unsupported"),
        }
    }
}

impl std::error::Error for RegistryGossipPayloadError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::CborDecode(e) => Some(e),
            Self::CborEncode(e) => Some(e),
            Self::PayloadTooLarge { .. }
            | Self::MalformedPayload { .. }
            | Self::MalformedKey
            | Self::MissingKey { .. }
            | Self::UnknownKey { .. }
            | Self::DuplicateKey { .. }
            | Self::MalformedField { .. }
            | Self::InvalidOp { .. } => None,
        }
    }
}

impl From<DecodeError> for RegistryGossipPayloadError {
    fn from(err: DecodeError) -> Self {
        match err {
            DecodeError::Cbor(e) => Self::CborDecode(e),
            DecodeError::MalformedFrame { reason } => Self::MalformedPayload { reason },
            DecodeError::MalformedKey => Self::MalformedKey,
            DecodeError::MissingKey { key } => Self::MissingKey { key },
            DecodeError::UnknownKey { key } => Self::UnknownKey { key },
            DecodeError::DuplicateKey { key } => Self::DuplicateKey { key },
            DecodeError::MalformedField { key, expected } => Self::MalformedField { key, expected },
            DecodeError::FrameTypeMissing
            | DecodeError::FrameTypeMalformed
            | DecodeError::FrameTypeUnsupported { .. }
            | DecodeError::UnknownVersion { .. } => Self::MalformedPayload {
                reason: "unexpected wire-frame field in registry gossip payload",
            },
        }
    }
}

/// Errors returned by the bounded SWIM control payload codec.
#[derive(Debug)]
pub enum SwimPayloadError {
    /// Encoded payload exceeded [`MAX_SWIM_PAYLOAD_BYTES`].
    PayloadTooLarge { len: usize, max: usize },
    /// Gossip batch exceeded [`MAX_SWIM_GOSSIP_ENTRIES`].
    TooManyGossipEntries { len: usize, max: usize },
    /// Payload CBOR was malformed or truncated.
    CborDecode(ciborium::de::Error<std::io::Error>),
    /// Payload CBOR encoding failed.
    CborEncode(ciborium::ser::Error<std::io::Error>),
    /// Top-level payload shape was not the expected map.
    MalformedPayload { reason: &'static str },
    /// A payload map key was malformed.
    MalformedKey,
    /// Required payload key was absent.
    MissingKey { key: u64 },
    /// Unsupported payload key was present.
    UnknownKey { key: u64 },
    /// Payload key appeared more than once.
    DuplicateKey { key: u64 },
    /// Payload field had the wrong CBOR type or value range.
    MalformedField { key: u64, expected: &'static str },
}

impl std::fmt::Display for SwimPayloadError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::PayloadTooLarge { len, max } => {
                write!(f, "SWIM payload is too large ({len} > {max})")
            }
            Self::TooManyGossipEntries { len, max } => {
                write!(
                    f,
                    "SWIM payload carries too many gossip entries ({len} > {max})"
                )
            }
            Self::CborDecode(e) => write!(f, "SWIM CBOR decode failed: {e}"),
            Self::CborEncode(e) => write!(f, "SWIM CBOR encode failed: {e}"),
            Self::MalformedPayload { reason } => write!(f, "malformed SWIM payload: {reason}"),
            Self::MalformedKey => {
                write!(f, "SWIM payload map key is not a non-negative integer")
            }
            Self::MissingKey { key } => {
                write!(f, "SWIM payload is missing required key {key}")
            }
            Self::UnknownKey { key } => {
                write!(f, "SWIM payload contains unsupported key {key}")
            }
            Self::DuplicateKey { key } => {
                write!(f, "SWIM payload contains duplicate key {key}")
            }
            Self::MalformedField { key, expected } => {
                write!(f, "SWIM payload key {key} is not {expected}")
            }
        }
    }
}

impl std::error::Error for SwimPayloadError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::CborDecode(e) => Some(e),
            Self::CborEncode(e) => Some(e),
            Self::PayloadTooLarge { .. }
            | Self::TooManyGossipEntries { .. }
            | Self::MalformedPayload { .. }
            | Self::MalformedKey
            | Self::MissingKey { .. }
            | Self::UnknownKey { .. }
            | Self::DuplicateKey { .. }
            | Self::MalformedField { .. } => None,
        }
    }
}

impl From<DecodeError> for SwimPayloadError {
    fn from(err: DecodeError) -> Self {
        match err {
            DecodeError::Cbor(e) => Self::CborDecode(e),
            DecodeError::MalformedFrame { reason } => Self::MalformedPayload { reason },
            DecodeError::MalformedKey => Self::MalformedKey,
            DecodeError::MissingKey { key } => Self::MissingKey { key },
            DecodeError::UnknownKey { key } => Self::UnknownKey { key },
            DecodeError::DuplicateKey { key } => Self::DuplicateKey { key },
            DecodeError::MalformedField { key, expected } => Self::MalformedField { key, expected },
            DecodeError::FrameTypeMissing
            | DecodeError::FrameTypeMalformed
            | DecodeError::FrameTypeUnsupported { .. }
            | DecodeError::UnknownVersion { .. } => Self::MalformedPayload {
                reason: "unexpected wire-frame field in SWIM payload",
            },
        }
    }
}

/// Errors returned by the bounded cross-node monitor payload codecs.
#[derive(Debug)]
pub enum MonitorPayloadError {
    /// Encoded payload exceeded [`MAX_MONITOR_PAYLOAD_BYTES`].
    PayloadTooLarge { len: usize, max: usize },
    /// Payload CBOR was malformed or truncated.
    CborDecode(ciborium::de::Error<std::io::Error>),
    /// Payload CBOR encoding failed.
    CborEncode(ciborium::ser::Error<std::io::Error>),
    /// Top-level payload shape was not the expected map.
    MalformedPayload { reason: &'static str },
    /// A payload map key was malformed.
    MalformedKey,
    /// Required payload key was absent.
    MissingKey { key: u64 },
    /// Unsupported payload key was present.
    UnknownKey { key: u64 },
    /// Payload key appeared more than once.
    DuplicateKey { key: u64 },
    /// Payload field had the wrong CBOR type or value range.
    MalformedField { key: u64, expected: &'static str },
}

impl std::fmt::Display for MonitorPayloadError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::PayloadTooLarge { len, max } => {
                write!(f, "monitor payload is too large ({len} > {max})")
            }
            Self::CborDecode(e) => write!(f, "monitor CBOR decode failed: {e}"),
            Self::CborEncode(e) => write!(f, "monitor CBOR encode failed: {e}"),
            Self::MalformedPayload { reason } => write!(f, "malformed monitor payload: {reason}"),
            Self::MalformedKey => {
                write!(f, "monitor payload map key is not a non-negative integer")
            }
            Self::MissingKey { key } => {
                write!(f, "monitor payload is missing required key {key}")
            }
            Self::UnknownKey { key } => {
                write!(f, "monitor payload contains unsupported key {key}")
            }
            Self::DuplicateKey { key } => {
                write!(f, "monitor payload contains duplicate key {key}")
            }
            Self::MalformedField { key, expected } => {
                write!(f, "monitor payload key {key} is not {expected}")
            }
        }
    }
}

impl std::error::Error for MonitorPayloadError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::CborDecode(e) => Some(e),
            Self::CborEncode(e) => Some(e),
            Self::PayloadTooLarge { .. }
            | Self::MalformedPayload { .. }
            | Self::MalformedKey
            | Self::MissingKey { .. }
            | Self::UnknownKey { .. }
            | Self::DuplicateKey { .. }
            | Self::MalformedField { .. } => None,
        }
    }
}

impl From<DecodeError> for MonitorPayloadError {
    fn from(err: DecodeError) -> Self {
        match err {
            DecodeError::Cbor(e) => Self::CborDecode(e),
            DecodeError::MalformedFrame { reason } => Self::MalformedPayload { reason },
            DecodeError::MalformedKey => Self::MalformedKey,
            DecodeError::MissingKey { key } => Self::MissingKey { key },
            DecodeError::UnknownKey { key } => Self::UnknownKey { key },
            DecodeError::DuplicateKey { key } => Self::DuplicateKey { key },
            DecodeError::MalformedField { key, expected } => Self::MalformedField { key, expected },
            DecodeError::FrameTypeMissing
            | DecodeError::FrameTypeMalformed
            | DecodeError::FrameTypeUnsupported { .. }
            | DecodeError::UnknownVersion { .. } => Self::MalformedPayload {
                reason: "unexpected wire-frame field in monitor payload",
            },
        }
    }
}

impl EnvelopeFrame {
    /// Construct a fire-and-forget envelope with no reply routing.
    ///
    /// Convenience constructor for the common case: `request_id = 0`.
    #[must_use]
    pub fn fire_and_forget(
        target: Location,
        source: Option<Location>,
        msg_type: i32,
        payload: Vec<u8>,
    ) -> Self {
        Self {
            version: WIRE_VERSION,
            target: Some(target),
            source,
            msg_type,
            payload,
            request_id: 0,
        }
    }
}

/// Encode a CBOR [`ControlFrame`].
///
/// # Errors
///
/// Returns [`EncodeError::Cbor`] when ciborium fails to serialise the frame.
pub fn encode_control_frame(frame: &ControlFrame) -> Result<Vec<u8>, EncodeError> {
    let mut bytes = Vec::new();
    ciborium::ser::into_writer(frame, &mut bytes)?;
    Ok(bytes)
}

/// Encode a CBOR [`EnvelopeFrame`].
///
/// # Errors
///
/// Returns [`EncodeError::Cbor`] when ciborium fails to serialise the frame.
pub fn encode_envelope_frame(frame: &EnvelopeFrame) -> Result<Vec<u8>, EncodeError> {
    // msg_type is the full i32 range — the Hew codegen uses hashed i32 discriminants
    // that may be negative. The CBOR Integer type covers this range on both encode and
    // decode sides. The original 0..=65535 guard was narrower than the field type and
    // blocked cross-node sends from compiled Hew programs.
    let mut bytes = Vec::new();
    ciborium::ser::into_writer(frame, &mut bytes)?;
    Ok(bytes)
}

/// Build and encode an [`EnvelopeFrame`] from a C-ABI payload pointer.
///
/// # Safety
///
/// `payload` must be valid for `payload_len` readable bytes, or null when
/// `payload_len` is zero.
// live on not(wasm32) — transport/connection/hew_node; dead on wasm32; callers in native-only modules
#[cfg_attr(target_arch = "wasm32", allow(dead_code))]
pub(crate) unsafe fn encode_envelope_frame_from_raw_parts(
    target: Option<Location>,
    source: Option<Location>,
    msg_type: i32,
    payload: *const u8,
    payload_len: usize,
    request_id: u64,
) -> Result<Vec<u8>, EncodeError> {
    let payload = if payload_len == 0 {
        Vec::new()
    } else if payload.is_null() {
        return Err(EncodeError::NullPayload { payload_len });
    } else {
        // SAFETY: caller guarantees `payload` is valid for `payload_len` bytes.
        unsafe { std::slice::from_raw_parts(payload, payload_len) }.to_vec()
    };
    encode_envelope_frame(&EnvelopeFrame {
        version: WIRE_VERSION,
        target,
        source,
        msg_type,
        payload,
        request_id,
    })
}

/// Encode a bounded registry-gossip control payload.
///
/// # Errors
///
/// Returns [`RegistryGossipPayloadError`] if `payload` is not schema-valid, is
/// too large, or CBOR serialisation fails.
pub fn encode_registry_gossip_payload(
    payload: &RegistryGossipPayload,
) -> Result<Vec<u8>, RegistryGossipPayloadError> {
    validate_registry_gossip_payload(payload)?;

    let value = Value::Map(vec![
        (
            Value::Integer(Integer::from(1u64)),
            Value::Integer(Integer::from(payload.op)),
        ),
        (
            Value::Integer(Integer::from(2u64)),
            Value::Text(payload.name.clone()),
        ),
        (
            Value::Integer(Integer::from(3u64)),
            location_to_value(payload.location),
        ),
    ]);
    let mut bytes = Vec::new();
    ciborium::ser::into_writer(&value, &mut bytes)
        .map_err(RegistryGossipPayloadError::CborEncode)?;
    if bytes.len() > MAX_REGISTRY_GOSSIP_PAYLOAD_BYTES {
        return Err(RegistryGossipPayloadError::PayloadTooLarge {
            len: bytes.len(),
            max: MAX_REGISTRY_GOSSIP_PAYLOAD_BYTES,
        });
    }
    Ok(bytes)
}

// -- Fail-closed decode surface --------------------------------------------

/// Errors returned by the fail-closed envelope decoders.
#[derive(Debug)]
pub enum DecodeError {
    /// The underlying CBOR bytes were malformed or truncated.
    Cbor(ciborium::de::Error<std::io::Error>),
    /// The top-level CBOR item is not a schema-conformant frame map.
    MalformedFrame { reason: &'static str },
    /// A top-level map key was not a non-negative CBOR integer representable as
    /// `u64`.
    MalformedKey,
    /// A required integer key is absent.
    MissingKey { key: u64 },
    /// A key outside the variant's CDDL key set is present.
    UnknownKey { key: u64 },
    /// Any integer key appeared more than once.
    DuplicateKey { key: u64 },
    /// Key 2 (`frame_type`) is absent.
    FrameTypeMissing,
    /// Key 2 is present but is not an `i64`-representable CBOR integer.
    FrameTypeMalformed,
    /// Key 2 is an integer but not supported by the requested decode surface.
    FrameTypeUnsupported { found: i64 },
    /// A required field value had the wrong CBOR major type or did not fit the
    /// Rust field type.
    MalformedField { key: u64, expected: &'static str },
    /// The frame's `version` field did not equal [`WIRE_VERSION`]. The
    /// CDDL contract requires decoders to refuse rather than try to interpret
    /// unknown versions.
    UnknownVersion { found: u8, expected: u8 },
}

impl std::fmt::Display for DecodeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Cbor(e) => write!(f, "CBOR decode failed: {e}"),
            Self::MalformedFrame { reason } => write!(f, "malformed CBOR frame: {reason}"),
            Self::MalformedKey => {
                write!(f, "CBOR frame map key is not a non-negative integer")
            }
            Self::MissingKey { key } => write!(f, "CBOR frame is missing required key {key}"),
            Self::UnknownKey { key } => write!(f, "CBOR frame contains unsupported key {key}"),
            Self::DuplicateKey { key } => write!(f, "CBOR frame contains duplicate key {key}"),
            Self::FrameTypeMissing => write!(f, "CBOR frame is missing frame_type key 2"),
            Self::FrameTypeMalformed => {
                write!(f, "CBOR frame_type key 2 is not an i64 integer")
            }
            Self::FrameTypeUnsupported { found } => {
                write!(f, "unsupported CBOR frame_type {found}")
            }
            Self::MalformedField { key, expected } => {
                write!(f, "CBOR frame key {key} is not {expected}")
            }
            Self::UnknownVersion { found, expected } => write!(
                f,
                "unknown wire version {found}, expected {expected}; \
                 decoders MUST reject unknown versions (see schemas/envelope.cddl)"
            ),
        }
    }
}

impl std::error::Error for DecodeError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Cbor(e) => Some(e),
            Self::MalformedFrame { .. }
            | Self::MalformedKey
            | Self::MissingKey { .. }
            | Self::UnknownKey { .. }
            | Self::DuplicateKey { .. }
            | Self::FrameTypeMissing
            | Self::FrameTypeMalformed
            | Self::FrameTypeUnsupported { .. }
            | Self::MalformedField { .. }
            | Self::UnknownVersion { .. } => None,
        }
    }
}

impl From<ciborium::de::Error<std::io::Error>> for DecodeError {
    fn from(e: ciborium::de::Error<std::io::Error>) -> Self {
        Self::Cbor(e)
    }
}

/// Decode any CBOR-encoded wire frame from `bytes`.
///
/// # Errors
///
/// Returns explicit discriminator errors when key 2 is missing, malformed, or
/// unsupported. Malformed CBOR bytes return [`DecodeError::Cbor`].
pub fn decode_wire_frame(bytes: &[u8]) -> Result<WireFrame, DecodeError> {
    let value: Value = ciborium::de::from_reader(bytes)?;
    let map = collect_map(&value)?;
    match frame_type_from_map(&map)? {
        found if found == i64::from(FRAME_TYPE_CONTROL) => {
            let frame = control_frame_from_map(&map)?;
            ensure_wire_version(frame.version)?;
            Ok(WireFrame::Control(frame))
        }
        found if found == i64::from(FRAME_TYPE_ENVELOPE) => {
            let frame = envelope_frame_from_map(&map)?;
            ensure_wire_version(frame.version)?;
            Ok(WireFrame::Envelope(frame))
        }
        found => Err(DecodeError::FrameTypeUnsupported { found }),
    }
}

/// Decode a CBOR-encoded [`EnvelopeFrame`] from `bytes`, enforcing the
/// wire-version invariant.
///
/// # Errors
///
/// - [`DecodeError::Cbor`] if `bytes` are not valid CBOR.
/// - [`DecodeError::FrameTypeMissing`], [`DecodeError::FrameTypeMalformed`],
///   or [`DecodeError::FrameTypeUnsupported`] for discriminator failures.
/// - [`DecodeError::DuplicateKey`] for duplicate integer keys.
/// - [`DecodeError::UnknownVersion`] if decode succeeded but the frame's
///   `version` field does not equal [`WIRE_VERSION`].
pub fn decode_envelope_frame(bytes: &[u8]) -> Result<EnvelopeFrame, DecodeError> {
    let value: Value = ciborium::de::from_reader(bytes)?;
    let frame = envelope_frame_from_value(&value)?;
    ensure_wire_version(frame.version)?;
    Ok(frame)
}

/// Decode a CBOR-encoded [`ControlFrame`] from `bytes`, enforcing the
/// wire-version invariant. See [`decode_envelope_frame`] for the error
/// contract.
///
/// # Errors
///
/// Mirrors [`decode_envelope_frame`].
pub fn decode_control_frame(bytes: &[u8]) -> Result<ControlFrame, DecodeError> {
    let value: Value = ciborium::de::from_reader(bytes)?;
    let frame = control_frame_from_value(&value)?;
    ensure_wire_version(frame.version)?;
    Ok(frame)
}

/// Decode a bounded registry-gossip control payload.
///
/// # Errors
///
/// Returns [`RegistryGossipPayloadError`] if the payload is too large, malformed,
/// contains unknown keys, or carries an unsupported operation.
pub fn decode_registry_gossip_payload(
    bytes: &[u8],
) -> Result<RegistryGossipPayload, RegistryGossipPayloadError> {
    if bytes.len() > MAX_REGISTRY_GOSSIP_PAYLOAD_BYTES {
        return Err(RegistryGossipPayloadError::PayloadTooLarge {
            len: bytes.len(),
            max: MAX_REGISTRY_GOSSIP_PAYLOAD_BYTES,
        });
    }

    let value: Value =
        ciborium::de::from_reader(bytes).map_err(RegistryGossipPayloadError::CborDecode)?;
    let map = collect_map(&value).map_err(RegistryGossipPayloadError::from)?;
    ensure_exact_keys(&map, &[1, 2, 3]).map_err(RegistryGossipPayloadError::from)?;
    let payload = RegistryGossipPayload {
        op: value_to_u8(
            required(&map, 1).map_err(RegistryGossipPayloadError::from)?,
            1,
        )
        .map_err(RegistryGossipPayloadError::from)?,
        name: value_to_text(
            required(&map, 2).map_err(RegistryGossipPayloadError::from)?,
            2,
        )
        .map_err(RegistryGossipPayloadError::from)?,
        location: value_to_location(
            required(&map, 3).map_err(RegistryGossipPayloadError::from)?,
            3,
        )
        .map_err(RegistryGossipPayloadError::from)?,
    };
    validate_registry_gossip_payload(&payload)?;
    Ok(payload)
}

/// Encode a bounded SWIM control payload.
///
/// # Errors
///
/// Returns [`SwimPayloadError`] if the gossip batch exceeds
/// [`MAX_SWIM_GOSSIP_ENTRIES`], the encoded payload exceeds
/// [`MAX_SWIM_PAYLOAD_BYTES`], or CBOR serialisation fails.
pub fn encode_swim_payload(payload: &SwimControlPayload) -> Result<Vec<u8>, SwimPayloadError> {
    if payload.gossip.len() > MAX_SWIM_GOSSIP_ENTRIES {
        return Err(SwimPayloadError::TooManyGossipEntries {
            len: payload.gossip.len(),
            max: MAX_SWIM_GOSSIP_ENTRIES,
        });
    }

    let gossip = Value::Array(
        payload
            .gossip
            .iter()
            .map(|entry| {
                Value::Map(vec![
                    (
                        Value::Integer(Integer::from(1u64)),
                        node_session_to_value(entry.member),
                    ),
                    (
                        Value::Integer(Integer::from(2u64)),
                        Value::Integer(Integer::from(entry.state)),
                    ),
                    (
                        Value::Integer(Integer::from(3u64)),
                        Value::Integer(Integer::from(entry.incarnation)),
                    ),
                ])
            })
            .collect(),
    );

    let value = Value::Map(vec![
        (
            Value::Integer(Integer::from(1u64)),
            Value::Integer(Integer::from(payload.msg_type)),
        ),
        (
            Value::Integer(Integer::from(2u64)),
            node_session_to_value(payload.from),
        ),
        (
            Value::Integer(Integer::from(3u64)),
            Value::Integer(Integer::from(payload.incarnation)),
        ),
        (
            Value::Integer(Integer::from(4u64)),
            node_session_option_to_value(payload.target),
        ),
        (Value::Integer(Integer::from(5u64)), gossip),
    ]);

    let mut bytes = Vec::new();
    ciborium::ser::into_writer(&value, &mut bytes).map_err(SwimPayloadError::CborEncode)?;
    if bytes.len() > MAX_SWIM_PAYLOAD_BYTES {
        return Err(SwimPayloadError::PayloadTooLarge {
            len: bytes.len(),
            max: MAX_SWIM_PAYLOAD_BYTES,
        });
    }
    Ok(bytes)
}

/// Decode a bounded SWIM control payload.
///
/// # Errors
///
/// Returns [`SwimPayloadError`] if the payload is too large, malformed,
/// contains unknown keys, or carries more than [`MAX_SWIM_GOSSIP_ENTRIES`]
/// gossip entries.
pub fn decode_swim_payload(bytes: &[u8]) -> Result<SwimControlPayload, SwimPayloadError> {
    if bytes.len() > MAX_SWIM_PAYLOAD_BYTES {
        return Err(SwimPayloadError::PayloadTooLarge {
            len: bytes.len(),
            max: MAX_SWIM_PAYLOAD_BYTES,
        });
    }

    let value: Value = ciborium::de::from_reader(bytes).map_err(SwimPayloadError::CborDecode)?;
    let map = collect_map(&value)?;
    ensure_exact_keys(&map, &[1, 2, 3, 4, 5])?;

    let gossip_value = required(&map, 5)?;
    let Value::Array(entries) = gossip_value else {
        return Err(SwimPayloadError::MalformedField {
            key: 5,
            expected: "array",
        });
    };
    if entries.len() > MAX_SWIM_GOSSIP_ENTRIES {
        return Err(SwimPayloadError::TooManyGossipEntries {
            len: entries.len(),
            max: MAX_SWIM_GOSSIP_ENTRIES,
        });
    }
    let mut gossip = Vec::with_capacity(entries.len());
    for entry in entries {
        let entry_map = collect_map(entry)?;
        ensure_exact_keys(&entry_map, &[1, 2, 3])?;
        gossip.push(SwimGossipEntry {
            member: value_to_node_session(required(&entry_map, 1)?, 1)?,
            state: value_to_i32(required(&entry_map, 2)?, 2)?,
            incarnation: value_to_u64(required(&entry_map, 3)?, 3)?,
        });
    }

    Ok(SwimControlPayload {
        msg_type: value_to_i32(required(&map, 1)?, 1)?,
        from: value_to_node_session(required(&map, 2)?, 2)?,
        incarnation: value_to_u64(required(&map, 3)?, 3)?,
        target: value_to_node_session_option(required(&map, 4)?, 4)?,
        gossip,
    })
}

/// Encode a bounded cross-node monitor REQUEST / DEMONITOR payload.
///
/// # Errors
///
/// Returns [`MonitorPayloadError`] if CBOR serialisation fails or the encoded
/// payload exceeds [`MAX_MONITOR_PAYLOAD_BYTES`].
pub fn encode_monitor_req_payload(
    payload: &MonitorReqPayload,
) -> Result<Vec<u8>, MonitorPayloadError> {
    let value = Value::Map(vec![
        (
            Value::Integer(Integer::from(1u64)),
            location_to_value(payload.watcher),
        ),
        (
            Value::Integer(Integer::from(2u64)),
            Value::Integer(Integer::from(payload.ref_id)),
        ),
        (
            Value::Integer(Integer::from(3u64)),
            location_to_value(payload.target),
        ),
    ]);
    let mut bytes = Vec::new();
    ciborium::ser::into_writer(&value, &mut bytes).map_err(MonitorPayloadError::CborEncode)?;
    if bytes.len() > MAX_MONITOR_PAYLOAD_BYTES {
        return Err(MonitorPayloadError::PayloadTooLarge {
            len: bytes.len(),
            max: MAX_MONITOR_PAYLOAD_BYTES,
        });
    }
    Ok(bytes)
}

/// Decode a bounded cross-node monitor REQUEST / DEMONITOR payload.
///
/// # Errors
///
/// Returns [`MonitorPayloadError`] if the payload is too large, malformed, or
/// contains unknown / duplicate / missing keys.
pub fn decode_monitor_req_payload(bytes: &[u8]) -> Result<MonitorReqPayload, MonitorPayloadError> {
    if bytes.len() > MAX_MONITOR_PAYLOAD_BYTES {
        return Err(MonitorPayloadError::PayloadTooLarge {
            len: bytes.len(),
            max: MAX_MONITOR_PAYLOAD_BYTES,
        });
    }
    let value: Value = ciborium::de::from_reader(bytes).map_err(MonitorPayloadError::CborDecode)?;
    let map = collect_map(&value)?;
    ensure_exact_keys(&map, &[1, 2, 3])?;
    Ok(MonitorReqPayload {
        watcher: value_to_location(required(&map, 1)?, 1)?,
        ref_id: value_to_u64(required(&map, 2)?, 2)?,
        target: value_to_location(required(&map, 3)?, 3)?,
    })
}

/// Encode a bounded cross-node monitor DOWN payload.
///
/// # Errors
///
/// Returns [`MonitorPayloadError`] if CBOR serialisation fails or the encoded
/// payload exceeds [`MAX_MONITOR_PAYLOAD_BYTES`].
pub fn encode_monitor_down_payload(
    payload: &MonitorDownPayload,
) -> Result<Vec<u8>, MonitorPayloadError> {
    let value = Value::Map(vec![
        (
            Value::Integer(Integer::from(1u64)),
            Value::Integer(Integer::from(payload.ref_id)),
        ),
        (
            Value::Integer(Integer::from(2u64)),
            location_to_value(payload.target),
        ),
        (
            Value::Integer(Integer::from(3u64)),
            down_reason_to_value(payload.reason),
        ),
    ]);
    let mut bytes = Vec::new();
    ciborium::ser::into_writer(&value, &mut bytes).map_err(MonitorPayloadError::CborEncode)?;
    if bytes.len() > MAX_MONITOR_PAYLOAD_BYTES {
        return Err(MonitorPayloadError::PayloadTooLarge {
            len: bytes.len(),
            max: MAX_MONITOR_PAYLOAD_BYTES,
        });
    }
    Ok(bytes)
}

/// Decode a bounded cross-node monitor DOWN payload.
///
/// # Errors
///
/// Returns [`MonitorPayloadError`] if the payload is too large, malformed, or
/// contains unknown / duplicate / missing keys.
pub fn decode_monitor_down_payload(
    bytes: &[u8],
) -> Result<MonitorDownPayload, MonitorPayloadError> {
    if bytes.len() > MAX_MONITOR_PAYLOAD_BYTES {
        return Err(MonitorPayloadError::PayloadTooLarge {
            len: bytes.len(),
            max: MAX_MONITOR_PAYLOAD_BYTES,
        });
    }
    let value: Value = ciborium::de::from_reader(bytes).map_err(MonitorPayloadError::CborDecode)?;
    let map = collect_map(&value)?;
    ensure_exact_keys(&map, &[1, 2, 3])?;
    Ok(MonitorDownPayload {
        ref_id: value_to_u64(required(&map, 1)?, 1)?,
        target: value_to_location(required(&map, 2)?, 2)?,
        reason: value_to_down_reason(required(&map, 3)?, 3)?,
    })
}

/// Encode a bounded cross-node link REQUEST / UNLINK payload.
///
/// # Errors
///
/// Returns [`MonitorPayloadError`] if CBOR serialisation fails or the encoded
/// payload exceeds [`MAX_LINK_PAYLOAD_BYTES`].
pub fn encode_link_req_payload(payload: &LinkReqPayload) -> Result<Vec<u8>, MonitorPayloadError> {
    let value = Value::Map(vec![
        (
            Value::Integer(Integer::from(1u64)),
            location_to_value(payload.linker),
        ),
        (
            Value::Integer(Integer::from(2u64)),
            Value::Integer(Integer::from(payload.ref_id)),
        ),
        (
            Value::Integer(Integer::from(3u64)),
            location_to_value(payload.target),
        ),
        (
            Value::Integer(Integer::from(4u64)),
            Value::Integer(Integer::from(payload.policy_tag)),
        ),
        (
            Value::Integer(Integer::from(5u64)),
            Value::Integer(Integer::from(payload.reciprocate)),
        ),
    ]);
    let mut bytes = Vec::new();
    ciborium::ser::into_writer(&value, &mut bytes).map_err(MonitorPayloadError::CborEncode)?;
    if bytes.len() > MAX_LINK_PAYLOAD_BYTES {
        return Err(MonitorPayloadError::PayloadTooLarge {
            len: bytes.len(),
            max: MAX_LINK_PAYLOAD_BYTES,
        });
    }
    Ok(bytes)
}

/// Decode a bounded cross-node link REQUEST / UNLINK payload.
///
/// # Errors
///
/// Returns [`MonitorPayloadError`] if the payload is too large, malformed, or
/// contains unknown / duplicate / missing keys. The decode bar is HIGHER-stakes
/// than monitor: a fabricated link request would register a link that can later
/// crash a real actor, so the exact-keys / range checks are not optional.
pub fn decode_link_req_payload(bytes: &[u8]) -> Result<LinkReqPayload, MonitorPayloadError> {
    if bytes.len() > MAX_LINK_PAYLOAD_BYTES {
        return Err(MonitorPayloadError::PayloadTooLarge {
            len: bytes.len(),
            max: MAX_LINK_PAYLOAD_BYTES,
        });
    }
    let value: Value = ciborium::de::from_reader(bytes).map_err(MonitorPayloadError::CborDecode)?;
    let map = collect_map(&value)?;
    ensure_exact_keys(&map, &[1, 2, 3, 4, 5])?;
    Ok(LinkReqPayload {
        linker: value_to_location(required(&map, 1)?, 1)?,
        ref_id: value_to_u64(required(&map, 2)?, 2)?,
        target: value_to_location(required(&map, 3)?, 3)?,
        policy_tag: value_to_u8(required(&map, 4)?, 4)?,
        reciprocate: value_to_u8(required(&map, 5)?, 5)?,
    })
}

/// Encode a bounded cross-node link DOWN payload.
///
/// The DOWN shape is identical to the monitor DOWN (`{1: ref_id, 2: reason}`);
/// the divergence is in the receiver's HANDLING (mailbox-EXIT crash, not a recv
/// slot), not the wire shape. Reuses [`encode_monitor_down_payload`] under the
/// [`MAX_LINK_PAYLOAD_BYTES`] bound.
///
/// # Errors
///
/// Returns [`MonitorPayloadError`] on encode failure or oversize.
pub fn encode_link_down_payload(
    payload: &MonitorDownPayload,
) -> Result<Vec<u8>, MonitorPayloadError> {
    encode_monitor_down_payload(payload)
}

/// Decode a bounded cross-node link DOWN payload. Same `{ref_id,
/// reason}` shape as the monitor DOWN.
///
/// # Errors
///
/// Returns [`MonitorPayloadError`] if the payload is too large, malformed, or
/// contains unknown / duplicate / missing keys.
pub fn decode_link_down_payload(bytes: &[u8]) -> Result<MonitorDownPayload, MonitorPayloadError> {
    decode_monitor_down_payload(bytes)
}

fn control_frame_from_value(value: &Value) -> Result<ControlFrame, DecodeError> {
    let map = collect_map(value)?;
    control_frame_from_map(&map)
}

fn envelope_frame_from_value(value: &Value) -> Result<EnvelopeFrame, DecodeError> {
    let map = collect_map(value)?;
    envelope_frame_from_map(&map)
}

fn control_frame_from_map(map: &BTreeMap<u64, &Value>) -> Result<ControlFrame, DecodeError> {
    ensure_frame_type(map, FRAME_TYPE_CONTROL)?;
    ensure_exact_keys(map, &[1, 2, 3, 4])?;
    Ok(ControlFrame {
        version: value_to_u8(required(map, 1)?, 1)?,
        ctrl_kind: value_to_u64(required(map, 3)?, 3)?,
        payload: value_to_bytes(required(map, 4)?, 4)?,
    })
}

fn envelope_frame_from_map(map: &BTreeMap<u64, &Value>) -> Result<EnvelopeFrame, DecodeError> {
    ensure_frame_type(map, FRAME_TYPE_ENVELOPE)?;
    ensure_exact_keys(map, &[1, 2, 3, 4, 5, 6, 7])?;
    Ok(EnvelopeFrame {
        version: value_to_u8(required(map, 1)?, 1)?,
        target: value_to_location_option(required(map, 3)?, 3)?,
        source: value_to_location_option(required(map, 4)?, 4)?,
        msg_type: value_to_i32(required(map, 5)?, 5)?,
        payload: value_to_bytes(required(map, 6)?, 6)?,
        request_id: value_to_u64(required(map, 7)?, 7)?,
    })
}

fn collect_map(value: &Value) -> Result<BTreeMap<u64, &Value>, DecodeError> {
    let Value::Map(entries) = value else {
        return Err(DecodeError::MalformedFrame {
            reason: "top-level CBOR item is not a map",
        });
    };

    let mut map = BTreeMap::new();
    for (key, value) in entries {
        let key = key_to_u64(key)?;
        if map.insert(key, value).is_some() {
            return Err(DecodeError::DuplicateKey { key });
        }
    }
    Ok(map)
}

fn key_to_u64(value: &Value) -> Result<u64, DecodeError> {
    let Value::Integer(integer) = value else {
        return Err(DecodeError::MalformedKey);
    };
    let raw = integer_to_i128(*integer);
    u64::try_from(raw).map_err(|_| DecodeError::MalformedKey)
}

fn ensure_exact_keys(
    map: &BTreeMap<u64, &Value>,
    expected_keys: &[u64],
) -> Result<(), DecodeError> {
    for key in expected_keys {
        if !map.contains_key(key) {
            return Err(DecodeError::MissingKey { key: *key });
        }
    }
    for key in map.keys() {
        if !expected_keys.contains(key) {
            return Err(DecodeError::UnknownKey { key: *key });
        }
    }
    Ok(())
}

fn ensure_frame_type(
    map: &BTreeMap<u64, &Value>,
    expected_frame_type: u8,
) -> Result<(), DecodeError> {
    let found = frame_type_from_map(map)?;
    if found == i64::from(expected_frame_type) {
        Ok(())
    } else {
        Err(DecodeError::FrameTypeUnsupported { found })
    }
}

fn frame_type_from_map(map: &BTreeMap<u64, &Value>) -> Result<i64, DecodeError> {
    let value = map.get(&2).copied().ok_or(DecodeError::FrameTypeMissing)?;
    let Value::Integer(integer) = value else {
        return Err(DecodeError::FrameTypeMalformed);
    };
    i64::try_from(integer_to_i128(*integer)).map_err(|_| DecodeError::FrameTypeMalformed)
}

fn required<'a>(map: &'a BTreeMap<u64, &Value>, key: u64) -> Result<&'a Value, DecodeError> {
    map.get(&key)
        .copied()
        .ok_or(DecodeError::MissingKey { key })
}

fn value_to_u8(value: &Value, key: u64) -> Result<u8, DecodeError> {
    let integer = value_to_integer(value, key, "a u8 integer")?;
    u8::try_from(integer).map_err(|_| DecodeError::MalformedField {
        key,
        expected: "a u8 integer",
    })
}

fn value_to_u64(value: &Value, key: u64) -> Result<u64, DecodeError> {
    let integer = value_to_integer(value, key, "a u64 integer")?;
    u64::try_from(integer).map_err(|_| DecodeError::MalformedField {
        key,
        expected: "a u64 integer",
    })
}

fn value_to_i32(value: &Value, key: u64) -> Result<i32, DecodeError> {
    let integer = value_to_integer(value, key, "an i32 integer")?;
    i32::try_from(integer).map_err(|_| DecodeError::MalformedField {
        key,
        expected: "an i32 integer",
    })
}

fn value_to_integer(value: &Value, key: u64, expected: &'static str) -> Result<i128, DecodeError> {
    match value {
        Value::Integer(integer) => Ok(integer_to_i128(*integer)),
        _ => Err(DecodeError::MalformedField { key, expected }),
    }
}

fn value_to_bytes(value: &Value, key: u64) -> Result<Vec<u8>, DecodeError> {
    match value {
        Value::Bytes(bytes) => Ok(bytes.clone()),
        _ => Err(DecodeError::MalformedField {
            key,
            expected: "a CBOR byte string",
        }),
    }
}

fn value_to_text(value: &Value, key: u64) -> Result<String, DecodeError> {
    match value {
        Value::Text(text) => Ok(text.clone()),
        _ => Err(DecodeError::MalformedField {
            key,
            expected: "a CBOR text string",
        }),
    }
}

fn node_id_to_value(node_id: NodeId) -> Value {
    Value::Bytes(node_id.to_bytes().to_vec())
}

fn value_to_node_id(value: &Value, key: u64) -> Result<NodeId, DecodeError> {
    let Value::Bytes(bytes) = value else {
        return Err(DecodeError::MalformedField {
            key,
            expected: "a 16-byte node id",
        });
    };
    let bytes: [u8; 16] = bytes
        .as_slice()
        .try_into()
        .map_err(|_| DecodeError::MalformedField {
            key,
            expected: "a 16-byte node id",
        })?;
    if bytes == [0; 16] {
        return Err(DecodeError::MalformedField {
            key,
            expected: "a non-zero 16-byte node id",
        });
    }
    Ok(NodeId::from_bytes(bytes))
}

fn location_to_value(location: Location) -> Value {
    Value::Map(vec![
        (
            Value::Integer(Integer::from(1u64)),
            node_id_to_value(location.node()),
        ),
        (
            Value::Integer(Integer::from(2u64)),
            Value::Integer(Integer::from(location.slot())),
        ),
        (
            Value::Integer(Integer::from(3u64)),
            Value::Integer(Integer::from(location.incarnation())),
        ),
    ])
}

fn location_option_to_value(location: Option<Location>) -> Value {
    location.map_or(Value::Null, location_to_value)
}

fn value_to_location(value: &Value, key: u64) -> Result<Location, DecodeError> {
    let map = collect_map(value).map_err(|_| DecodeError::MalformedField {
        key,
        expected: "a location map",
    })?;
    ensure_exact_keys(&map, &[1, 2, 3])?;
    let node = value_to_node_id(required(&map, 1)?, key)?;
    let slot = value_to_u64(required(&map, 2)?, key)?;
    let incarnation_raw = value_to_u64(required(&map, 3)?, key)?;
    let incarnation = u32::try_from(incarnation_raw).map_err(|_| DecodeError::MalformedField {
        key,
        expected: "a location with a u32 session incarnation",
    })?;
    Location::new(node, slot, incarnation).map_err(|_| DecodeError::MalformedField {
        key,
        expected: "a location with non-zero actor slot and session incarnation",
    })
}

fn value_to_location_option(value: &Value, key: u64) -> Result<Option<Location>, DecodeError> {
    if matches!(value, Value::Null) {
        Ok(None)
    } else {
        value_to_location(value, key).map(Some)
    }
}

fn node_session_to_value(identity: NodeSessionIdentity) -> Value {
    Value::Map(vec![
        (
            Value::Integer(Integer::from(1u64)),
            node_id_to_value(identity.node_id),
        ),
        (
            Value::Integer(Integer::from(2u64)),
            Value::Integer(Integer::from(identity.session_incarnation)),
        ),
    ])
}

fn node_session_option_to_value(identity: Option<NodeSessionIdentity>) -> Value {
    identity.map_or(Value::Null, node_session_to_value)
}

fn value_to_node_session(value: &Value, key: u64) -> Result<NodeSessionIdentity, DecodeError> {
    let map = collect_map(value).map_err(|_| DecodeError::MalformedField {
        key,
        expected: "a node-session map",
    })?;
    ensure_exact_keys(&map, &[1, 2])?;
    let session_raw = value_to_u64(required(&map, 2)?, key)?;
    let session_incarnation =
        u32::try_from(session_raw).map_err(|_| DecodeError::MalformedField {
            key,
            expected: "a node session with a u32 incarnation",
        })?;
    if session_incarnation == 0 {
        return Err(DecodeError::MalformedField {
            key,
            expected: "a node session with non-zero incarnation",
        });
    }
    Ok(NodeSessionIdentity {
        node_id: value_to_node_id(required(&map, 1)?, key)?,
        session_incarnation,
    })
}

fn value_to_node_session_option(
    value: &Value,
    key: u64,
) -> Result<Option<NodeSessionIdentity>, DecodeError> {
    if matches!(value, Value::Null) {
        Ok(None)
    } else {
        value_to_node_session(value, key).map(Some)
    }
}

const DOWN_REASON_ACTOR_STATE: u8 = 1;

fn down_reason_to_value(reason: i32) -> Value {
    Value::Map(vec![
        (
            Value::Integer(Integer::from(1u64)),
            Value::Integer(Integer::from(DOWN_REASON_ACTOR_STATE)),
        ),
        (
            Value::Integer(Integer::from(2u64)),
            Value::Integer(Integer::from(reason)),
        ),
    ])
}

fn value_to_down_reason(value: &Value, key: u64) -> Result<i32, DecodeError> {
    let map = collect_map(value).map_err(|_| DecodeError::MalformedField {
        key,
        expected: "a typed DOWN reason map",
    })?;
    ensure_exact_keys(&map, &[1, 2])?;
    let tag = value_to_u8(required(&map, 1)?, key)?;
    if tag != DOWN_REASON_ACTOR_STATE {
        return Err(DecodeError::MalformedField {
            key,
            expected: "a known DOWN reason tag",
        });
    }
    value_to_i32(required(&map, 2)?, key)
}

fn validate_registry_gossip_payload(
    payload: &RegistryGossipPayload,
) -> Result<(), RegistryGossipPayloadError> {
    match payload.op {
        REGISTRY_GOSSIP_OP_ADD | REGISTRY_GOSSIP_OP_REMOVE => {}
        op => return Err(RegistryGossipPayloadError::InvalidOp { op }),
    }

    if payload.name.len() > MAX_REGISTRY_GOSSIP_NAME_BYTES {
        return Err(RegistryGossipPayloadError::MalformedField {
            key: 2,
            expected: "a registry name within the gossip name length bound",
        });
    }
    if payload.name.as_bytes().contains(&0) {
        return Err(RegistryGossipPayloadError::MalformedField {
            key: 2,
            expected: "a registry name without NUL bytes",
        });
    }
    Ok(())
}

fn integer_to_i128(integer: Integer) -> i128 {
    integer.into()
}

fn ensure_wire_version(version: u8) -> Result<(), DecodeError> {
    if version == WIRE_VERSION {
        Ok(())
    } else {
        Err(DecodeError::UnknownVersion {
            found: version,
            expected: WIRE_VERSION,
        })
    }
}
