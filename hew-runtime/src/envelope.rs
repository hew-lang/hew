//! CBOR wire envelope types — the Rust-native representation of the Hew wire
//! protocol described by `schemas/envelope.cddl`.
//!
//! This module contains **type definitions only**. Encoding and decoding
//! are implemented in `cbor_envelope.rs` (W2). The types here carry enough
//! serde machinery for ciborium to round-trip them, but no codec surface is
//! exposed from this module.
//!
//! # Payload encoding
//!
//! `Vec<u8>` payload fields are serialised by ciborium as CBOR byte arrays
//! (major type 4) using standard serde sequence serialisation. W2 will add
//! `#[serde(with = "serde_bytes")]` or an equivalent approach to emit true
//! CBOR byte strings (major type 2, `bstr` in CDDL) if schema conformance
//! testing reveals a mismatch. For W1, the round-trip property is the gate.
//!
//! # Schema coordination
//!
//! Field numbers used in `#[serde(rename = "...")]` below match the integer
//! keys in `schemas/envelope.cddl`. Keep them in sync.
//!
//! # Legacy note
//!
//! `HewWireEnvelope` in `wire.rs` is the C-FFI struct for the HBF wire
//! format, which is being retired (W5 deletes it). This type is the
//! clean-break CBOR-native equivalent. The two are structurally equivalent
//! but not ABI-compatible: `HewWireEnvelope` carries a raw `*mut u8` payload
//! pointer; `EnvelopeFrame` owns the payload as a `Vec<u8>`.

use serde::{Deserialize, Serialize};

/// Current wire protocol version.
///
/// Frames carrying any other version value MUST be rejected by the decoder.
pub const WIRE_VERSION: u8 = 1;

/// Discriminant for a control frame (`frame_type` field value 0).
pub const FRAME_TYPE_CONTROL: u8 = 0;

/// Discriminant for an envelope frame (`frame_type` field value 1).
pub const FRAME_TYPE_ENVELOPE: u8 = 1;

/// A control frame: node-level signalling with an opaque byte payload.
///
/// CDDL: `control-frame` rule in `schemas/envelope.cddl`.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ControlFrame {
    /// Wire protocol version. MUST equal [`WIRE_VERSION`].
    ///
    /// CDDL key 1.
    #[serde(rename = "1")]
    pub version: u8,

    /// Protocol-defined control kind tag.
    ///
    /// CDDL key 3.
    #[serde(rename = "3")]
    pub ctrl_kind: u64,

    /// Opaque payload. Empty for bare signals.
    ///
    /// CDDL key 4.
    #[serde(rename = "4")]
    pub payload: Vec<u8>,
}

/// An envelope frame: a single actor-to-actor message.
///
/// CDDL: `envelope-frame` rule in `schemas/envelope.cddl`.
///
/// Field numbers mirror the legacy HBF field ordering (fields 1-6) so
/// documentation can cross-reference the two representations.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct EnvelopeFrame {
    /// Wire protocol version. MUST equal [`WIRE_VERSION`].
    ///
    /// CDDL key 1.
    #[serde(rename = "1")]
    pub version: u8,

    /// Target actor identity. MUST be non-zero.
    ///
    /// CDDL key 3.
    #[serde(rename = "3")]
    pub target_actor_id: u64,

    /// Source actor identity.
    ///
    /// CDDL key 4.
    #[serde(rename = "4")]
    pub source_actor_id: u64,

    /// Message type tag.
    ///
    /// Signed; valid range `0..=2^30-1` (matches HBF zigzag legacy range).
    ///
    /// CDDL key 5.
    #[serde(rename = "5")]
    pub msg_type: i32,

    /// Serialised message body. May be empty for zero-payload messages.
    ///
    /// Collapses the legacy `(payload_size: u32, payload: *mut u8)` C-FFI
    /// pair into a single owned byte string. See `schemas/README.md`.
    ///
    /// CDDL key 6.
    #[serde(rename = "6")]
    pub payload: Vec<u8>,

    /// Request ID for distributed ask/reply correlation.
    ///
    /// `0` = fire-and-forget. `> 0` = a reply is expected (or this frame
    /// is a reply to a prior ask with this ID).
    ///
    /// CDDL key 7.
    #[serde(rename = "7")]
    pub request_id: u64,

    /// Source node ID for routing replies back to the requester.
    ///
    /// Non-zero on outbound ask requests; zero on reply envelopes and
    /// fire-and-forget messages.
    ///
    /// CDDL key 8.
    #[serde(rename = "8")]
    pub source_node_id: u16,
}

impl EnvelopeFrame {
    /// Construct a fire-and-forget envelope with no reply routing.
    ///
    /// Convenience constructor for the common case: `request_id = 0`,
    /// `source_node_id = 0`.
    #[must_use]
    pub fn fire_and_forget(
        target_actor_id: u64,
        source_actor_id: u64,
        msg_type: i32,
        payload: Vec<u8>,
    ) -> Self {
        Self {
            version: WIRE_VERSION,
            target_actor_id,
            source_actor_id,
            msg_type,
            payload,
            request_id: 0,
            source_node_id: 0,
        }
    }
}
