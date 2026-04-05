//! Shared analysis library for Hew language tooling.
//!
//! This crate provides LSP-independent analysis logic that can be shared between
//! `hew-lsp` (native LSP server) and `hew-wasm` (browser-based editor support).
//! All result types use plain offsets and strings rather than LSP protocol types.

pub mod code_actions;
pub mod completions;
pub mod definition;
pub mod folding;
pub mod hover;
pub mod inlay_hints;
mod method_lookup;
pub mod references;
pub mod rename;
pub mod semantic_tokens;
pub mod signature_help;
pub mod symbols;
pub mod util;

use serde::{Deserialize, Serialize};

// ── Span ─────────────────────────────────────────────────────────────

/// A byte-offset span, independent of any LSP position encoding.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct OffsetSpan {
    pub start: usize,
    pub end: usize,
}

impl From<hew_parser::ast::Span> for OffsetSpan {
    fn from(span: hew_parser::ast::Span) -> Self {
        Self {
            start: span.start,
            end: span.end,
        }
    }
}

// ── Hover ────────────────────────────────────────────────────────────

/// Result of a hover request.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HoverResult {
    pub contents: String,
    pub span: Option<OffsetSpan>,
}

// ── Completions ──────────────────────────────────────────────────────

/// A single completion suggestion.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CompletionItem {
    pub label: String,
    pub kind: CompletionKind,
    pub detail: Option<String>,
    pub insert_text: Option<String>,
    pub insert_text_is_snippet: bool,
    pub sort_text: Option<String>,
}

/// The kind of a completion item.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum CompletionKind {
    Function,
    Variable,
    Keyword,
    Snippet,
    Type,
    Actor,
    Constant,
    Field,
    Method,
    Module,
}

// ── Document symbols ─────────────────────────────────────────────────

/// Information about a symbol in the document.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SymbolInfo {
    pub name: String,
    pub kind: SymbolKind,
    pub span: OffsetSpan,
    pub selection_span: OffsetSpan,
    pub children: Vec<SymbolInfo>,
}

/// The kind of a document symbol.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum SymbolKind {
    Function,
    Actor,
    Supervisor,
    Machine,
    Trait,
    Type,
    Constant,
    Wire,
    TypeAlias,
    Impl,
    Field,
    Method,
    State,
    Event,
    Enum,
    Variant,
    Module,
    Constructor,
}

// ── Semantic tokens ──────────────────────────────────────────────────

/// A single semantic token expressed in absolute byte offsets.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct SemanticToken {
    pub start: usize,
    pub length: usize,
    pub token_type: u32,
    pub modifiers: u32,
}

/// Semantic token type indices. These must match the legend order registered
/// with the editor.
pub mod token_types {
    pub const KEYWORD: u32 = 0;
    pub const TYPE: u32 = 1;
    pub const FUNCTION: u32 = 2;
    pub const VARIABLE: u32 = 3;
    pub const NUMBER: u32 = 4;
    pub const STRING: u32 = 5;
    pub const OPERATOR: u32 = 6;
    pub const COMMENT: u32 = 7;
}

/// Semantic token modifier bit flags. These must match the legend order
/// registered with the editor.
pub mod token_modifiers {
    pub const DECLARATION: u32 = 1 << 0;
    pub const READONLY: u32 = 1 << 1;
    pub const ASYNC: u32 = 1 << 2;
}

// ── References ───────────────────────────────────────────────────────

/// A reference location with a flag indicating whether it is the definition site.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct ReferenceSpan {
    pub span: OffsetSpan,
    pub is_definition: bool,
}

// ── Rename ───────────────────────────────────────────────────────────

/// A single text edit for a rename operation.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RenameEdit {
    pub span: OffsetSpan,
    pub new_text: String,
}

// ── Folding ──────────────────────────────────────────────────────────

/// A foldable range expressed in line numbers (0-based).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct FoldingRange {
    pub start_line: u32,
    pub end_line: u32,
    pub kind: FoldingKind,
}

/// The kind of a folding range.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum FoldingKind {
    Region,
    Imports,
    Comment,
}

// ── Inlay hints ──────────────────────────────────────────────────────

/// An inlay hint displayed inline in the editor.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InlayHint {
    pub offset: usize,
    pub label: String,
    pub kind: InlayHintKind,
    pub padding_left: bool,
}

/// The kind of an inlay hint.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum InlayHintKind {
    Type,
    Parameter,
}

// ── Signature help ───────────────────────────────────────────────────

/// Result of a signature help request.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SignatureHelpResult {
    pub signatures: Vec<SignatureInfo>,
    pub active_signature: Option<u32>,
    pub active_parameter: Option<u32>,
}

/// Information about a single function signature.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SignatureInfo {
    pub label: String,
    pub parameters: Vec<ParameterInfo>,
}

/// Label offsets for a single parameter within a signature label.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct ParameterInfo {
    pub label_start: u32,
    pub label_end: u32,
}

// ── Code actions ─────────────────────────────────────────────────────

/// A code action with its associated text edits.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CodeAction {
    pub title: String,
    pub edits: Vec<RenameEdit>,
}
