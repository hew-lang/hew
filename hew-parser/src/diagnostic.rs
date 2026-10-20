//! Structured diagnostic edits shared by compiler producers and tooling.

use serde::{Deserialize, Serialize};

/// A byte-offset span, independent of any editor position encoding.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct OffsetSpan {
    pub start: usize,
    pub end: usize,
}

impl From<crate::ast::Span> for OffsetSpan {
    fn from(span: crate::ast::Span) -> Self {
        Self {
            start: span.start,
            end: span.end,
        }
    }
}

/// One exact replacement in the diagnostic's source input.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TextEdit {
    pub span: OffsetSpan,
    pub new_text: String,
}

/// Whether a fix can be applied without a programmer choosing its meaning.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Applicability {
    MachineApplicable,
    MaybeIncorrect,
    HasPlaceholders,
}

/// One alternative. Its edits are a single indivisible change.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct DiagnosticFix {
    pub title: String,
    pub applicability: Applicability,
    pub edits: Vec<TextEdit>,
}

impl DiagnosticFix {
    /// Produce an exact single-site alternative from the owning syntax fact.
    #[must_use]
    pub fn replace(
        title: impl Into<String>,
        span: crate::ast::Span,
        replacement: impl Into<String>,
        applicability: Applicability,
    ) -> Self {
        Self {
            title: title.into(),
            applicability,
            edits: vec![TextEdit {
                span: span.into(),
                new_text: replacement.into(),
            }],
        }
    }
}
