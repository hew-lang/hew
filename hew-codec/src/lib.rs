//! Format-neutral serialization for Hew data.
//!
//! The compiler emits demanded encode and decode walks per concrete data type
//! and format. An encode walk drives a [`Sink`] with structural events; a decode
//! walk pulls the same structure from a [`Source`]. The five formats differ
//! only in how the sink spells those events and how the source parses them:
//!
//! - text formats (JSON, YAML, TOML) key records and variants by their text key,
//!   write `bytes` as padded base64, and write a map whose keys are not
//!   `string` as a sequence of `[key, value]` pairs;
//! - binary formats (CBOR, `MessagePack`) key a tagged (`#[wire]`) record or enum
//!   by its integer tags and anything else by its text key.
//!
//! Output is deterministic: maps and sets are written in canonical order
//! (sorted by encoded key; CBOR uses the length-first rule of RFC 8949
//! §4.2.3). Every input format refuses duplicate map keys, bounds nesting at
//! [`MAX_DEPTH`], and reports failures as a [`DecodeError`] with the path of
//! the failing value.
//!
//! JSON encoding writes events directly into its output buffer. Other encoders
//! build a [`Value`] tree; decoders parse one and hand it out pull by pull.

#![expect(
    clippy::missing_panics_doc,
    reason = "Sink and Source panic only when a walk breaks the event grammar, a compiler defect"
)]

mod error;
mod formats;
mod json;
mod json_sink;
pub mod managed_tree;
pub mod property;
mod sink;
mod source;
mod table;
pub mod typed;
mod value;
pub mod yaml_value;

pub use error::DecodeError;
pub use json_sink::JsonSink;
pub use sink::Sink;
pub use source::Source;
pub use table::{Member, Table};
pub use value::Value;

/// The deepest container nesting any parser accepts. Every format applies
/// the same bound, so a document is refused identically in each.
pub const MAX_DEPTH: usize = 128;

/// A serialization format.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Format {
    Cbor,
    Json,
    Yaml,
    Toml,
    Msgpack,
}

impl Format {
    /// Text formats key by name and spell `bytes` as base64.
    #[must_use]
    pub fn is_text(self) -> bool {
        matches!(self, Self::Json | Self::Yaml | Self::Toml)
    }
}
