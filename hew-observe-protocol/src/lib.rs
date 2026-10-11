//! Generated, versioned models for the Hew Observe HTTP protocol.
//!
//! The authority is `protocol/observe/v1/openapi.json`. Regenerate this
//! crate and the Swift/TypeScript siblings with
//! `make observe-protocol-generate`.

mod generated;

pub use generated::*;

/// A malformed JSON body or unsupported Observe schema version.
#[derive(Debug)]
pub enum DecodeError {
    Json(serde_json::Error),
    SchemaVersion {
        expected: &'static str,
        actual: String,
    },
}

impl std::fmt::Display for DecodeError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Json(error) => write!(formatter, "invalid Observe JSON: {error}"),
            Self::SchemaVersion { expected, actual } => {
                write!(
                    formatter,
                    "schema_version mismatch: expected {expected}, got {actual}"
                )
            }
        }
    }
}

impl std::error::Error for DecodeError {}

/// Decode and version-check a canonical Observe envelope.
///
/// # Errors
///
/// Returns [`DecodeError::Json`] for malformed JSON or payloads and
/// [`DecodeError::SchemaVersion`] when the envelope does not declare v1.
pub fn decode_envelope<T: serde::de::DeserializeOwned>(bytes: &[u8]) -> Result<T, DecodeError> {
    let envelope: Envelope<T> = serde_json::from_slice(bytes).map_err(DecodeError::Json)?;
    if envelope.schema_version != OBSERVE_SCHEMA_VERSION {
        return Err(DecodeError::SchemaVersion {
            expected: OBSERVE_SCHEMA_VERSION,
            actual: envelope.schema_version,
        });
    }
    Ok(envelope.data)
}

/// Wrap an already-serialized JSON value in the canonical v1 envelope.
///
/// Runtime serializers use this zero-copy-ish path for hot snapshots. The
/// compatibility suite validates each inner value against the contract before
/// this helper is used in production.
#[must_use]
pub fn envelope_json_raw(body: &str) -> String {
    let mut out = String::with_capacity(body.len() + OBSERVE_SCHEMA_VERSION.len() + 32);
    out.push_str(r#"{"schema_version":""#);
    out.push_str(OBSERVE_SCHEMA_VERSION);
    out.push_str(r#"","data":"#);
    out.push_str(body);
    out.push('}');
    out
}

impl ActorInfo {
    #[must_use]
    pub fn state_name(&self) -> &str {
        if self.state.is_empty() {
            "Unknown"
        } else {
            &self.state
        }
    }

    #[must_use]
    pub fn type_label(&self) -> &str {
        if self.actor_type.is_empty() {
            "Actor"
        } else {
            &self.actor_type
        }
    }
}

impl TraceEvent {
    /// Whether the event drives the v1 timeline and actor drill-down views.
    #[must_use]
    pub fn is_actionable(&self) -> bool {
        ACTIONABLE_TRACE_EVENT_TYPES.contains(&self.event_type.as_str())
    }
}
