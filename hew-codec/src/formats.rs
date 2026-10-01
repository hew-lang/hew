//! Parsing and writing a [`Value`] in each format, and the canonical key order.

use core::cmp::Ordering;
use std::io::Cursor;

use serde::de::DeserializeSeed;

use crate::value::{Value, ValueSeed};
use crate::{DecodeError, Format};

/// A whole document as one value, through [`ValueSeed`], so the serde-based
/// parsers that only take `DeserializeOwned` share its duplicate and depth rules.
struct Document(Value);

impl<'de> serde::Deserialize<'de> for Document {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        ValueSeed { depth: 0 }.deserialize(deserializer).map(Self)
    }
}

pub(crate) fn parse(format: Format, input: &[u8]) -> Result<Value, DecodeError> {
    match format {
        Format::Json => crate::json::parse(input),
        Format::Yaml => parse_yaml(utf8(input)?),
        Format::Toml => parse_toml(utf8(input)?),
        Format::Cbor => parse_binary(input, |cursor| {
            ciborium::de::from_reader::<Document, _>(cursor).map_err(|e| match e {
                ciborium::de::Error::Syntax(offset) => (Some(offset), "malformed CBOR".to_owned()),
                ciborium::de::Error::Semantic(offset, reason) => (offset, reason),
                ciborium::de::Error::RecursionLimitExceeded => (
                    None,
                    format!("nesting exceeds the limit of {}", crate::MAX_DEPTH),
                ),
                ciborium::de::Error::Io(_) => (None, "unexpected end of input".to_owned()),
            })
        }),
        Format::Msgpack => parse_binary(input, |cursor| {
            rmp_serde::from_read::<_, Document>(cursor).map_err(|e| match e {
                rmp_serde::decode::Error::InvalidMarkerRead(_)
                | rmp_serde::decode::Error::InvalidDataRead(_) => {
                    (None, "unexpected end of input".to_owned())
                }
                other => (None, other.to_string()),
            })
        }),
    }
}

fn utf8(input: &[u8]) -> Result<&str, DecodeError> {
    core::str::from_utf8(input)
        .map_err(|e| DecodeError::syntax_at(Some(input), e.valid_up_to(), "invalid UTF-8"))
}

/// Parse a binary document from a cursor; an error without its own offset
/// reports how far the parser read. Trailing bytes are refused.
fn parse_binary(
    input: &[u8],
    read: impl FnOnce(&mut Cursor<&[u8]>) -> Result<Document, (Option<usize>, String)>,
) -> Result<Value, DecodeError> {
    let mut cursor = Cursor::new(input);
    let document = read(&mut cursor).map_err(|(offset, reason)| {
        let offset = offset.unwrap_or_else(|| position(&cursor));
        DecodeError::syntax_at(None, offset, reason)
    })?;
    let end = position(&cursor);
    if end != input.len() {
        return Err(DecodeError::syntax_at(
            None,
            end,
            "trailing bytes after the value",
        ));
    }
    Ok(document.0)
}

fn position(cursor: &Cursor<&[u8]>) -> usize {
    usize::try_from(cursor.position()).unwrap_or(usize::MAX)
}

fn parse_yaml(text: &str) -> Result<Value, DecodeError> {
    serde_yaml::from_str::<Document>(text)
        .map(|document| document.0)
        .map_err(|e| {
            let offset = e.location().map_or(text.len(), |location| location.index());
            let message = e.to_string();
            // serde_yaml appends its own position; ours comes from the offset.
            let reason = message
                .find(" at line ")
                .map_or(message.as_str(), |at| &message[..at]);
            DecodeError::syntax_at(Some(text.as_bytes()), offset, reason)
        })
}

fn parse_toml(text: &str) -> Result<Value, DecodeError> {
    toml::from_str::<Document>(text)
        .map(|document| document.0)
        .map_err(|e| {
            let offset = e.span().map_or(0, |span| span.start);
            DecodeError::syntax_at(Some(text.as_bytes()), offset, e.message().trim_end())
        })
}

/// Write a finished document. The sink never builds a value its format
/// cannot hold; the checker refuses those types at the call site.
pub(crate) fn write(format: Format, value: &Value) -> Vec<u8> {
    let written = match format {
        Format::Json => serde_json::to_vec(value).map_err(|e| e.to_string()),
        Format::Yaml => serde_yaml::to_string(value)
            .map(String::into_bytes)
            .map_err(|e| e.to_string()),
        Format::Toml => toml::to_string(value)
            .map(String::into_bytes)
            .map_err(|e| e.to_string()),
        Format::Msgpack => rmp_serde::to_vec(value).map_err(|e| e.to_string()),
        Format::Cbor => {
            let mut out = Vec::new();
            ciborium::ser::into_writer(value, &mut out)
                .map(|()| out)
                .map_err(|e| e.to_string())
        }
    };
    written.unwrap_or_else(|reason| {
        panic!("hew-codec: {format:?} cannot hold this value ({reason}); the checker admits only representable types")
    })
}

/// Canonical order of two encoded keys: CBOR compares length first, then
/// bytes (RFC 8949 §4.2.3); every other format compares bytes.
pub(crate) fn canonical_cmp(format: Format, a: &[u8], b: &[u8]) -> Ordering {
    match format {
        Format::Cbor => a.len().cmp(&b.len()).then_with(|| a.cmp(b)),
        _ => a.cmp(b),
    }
}

/// The encoding a key is sorted by. Text formats sort by the JSON spelling.
pub(crate) fn sort_key(format: Format, key: &Value) -> Vec<u8> {
    match format {
        Format::Json | Format::Yaml | Format::Toml => write(Format::Json, key),
        Format::Msgpack | Format::Cbor => write(format, key),
    }
}
