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

// ── Fail-closed decode surface ────────────────────────────────────────────
//
// The W1 module docstring and `WIRE_VERSION` doc-comment both assert that
// "Frames carrying any other version value MUST be rejected". The types
// themselves cannot enforce this — ciborium will happily deserialise a
// frame whose `version` field is, say, `0` or `7`. Callers MUST decode
// through [`decode_envelope_frame`] / [`decode_control_frame`] (or perform
// an equivalent version check themselves) to honour the CDDL contract.
//
// # Frame-type discrimination
//
// The CDDL `wire-frame = control-frame / envelope-frame` choice carries a
// `frame_type` key (CBOR map key `2`) that selects the branch. The Rust
// representation here splits the choice at the *type* level — [`ControlFrame`]
// and [`EnvelopeFrame`] are separate structs — rather than carrying a runtime
// discriminant byte. Frame-type "mismatch" is therefore caught one layer up,
// by the codec choosing which struct to decode into based on the wire
// `frame_type` byte. A truly mis-typed payload (a control frame's bytes fed
// to `decode_envelope_frame`) surfaces as a `Cbor` decode error from ciborium
// because the required field set will not match — exercised by the
// `mismatched_frame_type_is_rejected` test.

/// Errors returned by the fail-closed envelope decoders.
#[derive(Debug)]
pub enum DecodeError {
    /// The underlying CBOR bytes were malformed, truncated, or did not match
    /// the expected struct shape.
    Cbor(ciborium::de::Error<std::io::Error>),
    /// The frame's `version` field did not equal [`WIRE_VERSION`]. The
    /// CDDL contract requires decoders to refuse rather than try to interpret
    /// unknown versions.
    UnknownVersion { found: u8, expected: u8 },
}

impl std::fmt::Display for DecodeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Cbor(e) => write!(f, "CBOR decode failed: {e}"),
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
            Self::UnknownVersion { .. } => None,
        }
    }
}

impl From<ciborium::de::Error<std::io::Error>> for DecodeError {
    fn from(e: ciborium::de::Error<std::io::Error>) -> Self {
        Self::Cbor(e)
    }
}

/// Decode a CBOR-encoded [`EnvelopeFrame`] from `bytes`, enforcing the
/// wire-version invariant.
///
/// Returns [`DecodeError::Cbor`] for malformed or truncated input (ciborium
/// surfaces short reads as decode errors) and [`DecodeError::UnknownVersion`]
/// if the decoded frame's `version` field is not [`WIRE_VERSION`].
///
/// # Errors
///
/// - [`DecodeError::Cbor`] if `bytes` are not a valid CBOR encoding of an
///   [`EnvelopeFrame`] (malformed, truncated, missing required fields, or
///   the bytes of a different frame type — see the module docs on
///   frame-type discrimination).
/// - [`DecodeError::UnknownVersion`] if decode succeeded but the frame's
///   `version` field does not equal [`WIRE_VERSION`].
pub fn decode_envelope_frame(bytes: &[u8]) -> Result<EnvelopeFrame, DecodeError> {
    let frame: EnvelopeFrame = ciborium::de::from_reader(bytes)?;
    if frame.version != WIRE_VERSION {
        return Err(DecodeError::UnknownVersion {
            found: frame.version,
            expected: WIRE_VERSION,
        });
    }
    Ok(frame)
}

/// Decode a CBOR-encoded [`ControlFrame`] from `bytes`, enforcing the
/// wire-version invariant. See [`decode_envelope_frame`] for the error
/// contract.
///
/// # Errors
///
/// Mirrors [`decode_envelope_frame`]: [`DecodeError::Cbor`] for malformed
/// or wrong-shape input; [`DecodeError::UnknownVersion`] if the decoded
/// frame's `version` field is not [`WIRE_VERSION`].
pub fn decode_control_frame(bytes: &[u8]) -> Result<ControlFrame, DecodeError> {
    let frame: ControlFrame = ciborium::de::from_reader(bytes)?;
    if frame.version != WIRE_VERSION {
        return Err(DecodeError::UnknownVersion {
            found: frame.version,
            expected: WIRE_VERSION,
        });
    }
    Ok(frame)
}
