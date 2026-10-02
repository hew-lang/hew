//! The decode error model, mirrored one to one by `std.encoding.wire.DecodeError`.

use core::fmt;

/// Why a decode failed. Paths render as `.field`, `[index]`, `["key"]` for a
/// map value and `.Variant` for a variant payload; the root is the empty path.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DecodeError {
    /// The input is not well-formed in its format. Text formats fill line and
    /// column (1-based, column in characters); binary formats leave them `None`.
    Syntax {
        offset: usize,
        line: Option<usize>,
        column: Option<usize>,
        reason: String,
    },
    /// A value has the wrong kind for its target.
    Type {
        path: String,
        expected: String,
        found: String,
    },
    /// A required key is absent.
    Missing { path: String },
    /// A number does not fit its target integer type.
    Range {
        path: String,
        value: String,
        target: String,
    },
    /// A variant name or tag matches no variant.
    UnknownVariant { path: String, name: String },
    /// A map key or set element repeats in a form the parser cannot refuse
    /// itself (a `[key, value]` pair sequence, a set's sequence).
    Duplicate { path: String, key: String },
    /// A representation override refused its representation.
    Invalid { path: String, reason: String },
}

impl DecodeError {
    pub(crate) fn syntax_at(text: Option<&[u8]>, offset: usize, reason: impl Into<String>) -> Self {
        let (line, column) = match text {
            Some(text) => {
                let (line, column) = line_column(text, offset);
                (Some(line), Some(column))
            }
            None => (None, None),
        };
        Self::Syntax {
            offset,
            line,
            column,
            reason: reason.into(),
        }
    }
}

/// 1-based line and character column of a byte offset.
fn line_column(text: &[u8], offset: usize) -> (usize, usize) {
    let before = &text[..offset.min(text.len())];
    let line_start = before
        .iter()
        .rposition(|&b| b == b'\n')
        .map_or(0, |at| at + 1);
    let line = before.split(|&b| b == b'\n').count();
    let column = String::from_utf8_lossy(&before[line_start..])
        .chars()
        .count()
        + 1;
    (line, column)
}

fn at(f: &mut fmt::Formatter<'_>, path: &str) -> fmt::Result {
    if path.is_empty() {
        Ok(())
    } else {
        write!(f, "{path}: ")
    }
}

impl fmt::Display for DecodeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Syntax {
                offset,
                line,
                column,
                reason,
            } => match (line, column) {
                (Some(line), Some(column)) => {
                    write!(f, "Syntax: line {line}, column {column}: {reason}")
                }
                _ => write!(f, "Syntax: offset {offset}: {reason}"),
            },
            Self::Type {
                path,
                expected,
                found,
            } => {
                f.write_str("Type: ")?;
                at(f, path)?;
                write!(f, "expected {expected}, found {found}")
            }
            Self::Missing { path } => write!(f, "Missing: {path}"),
            Self::Range {
                path,
                value,
                target,
            } => {
                f.write_str("Range: ")?;
                at(f, path)?;
                write!(f, "{value} does not fit {target}")
            }
            Self::UnknownVariant { path, name } => {
                f.write_str("UnknownVariant: ")?;
                at(f, path)?;
                write!(f, "no variant named {name}")
            }
            Self::Duplicate { path, key } => {
                f.write_str("Duplicate: ")?;
                at(f, path)?;
                write!(f, "{key} repeats")
            }
            Self::Invalid { path, reason } => {
                f.write_str("Invalid: ")?;
                at(f, path)?;
                f.write_str(reason)
            }
        }
    }
}

impl std::error::Error for DecodeError {}
