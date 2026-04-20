//! Shared analysis library for Hew language tooling.
//!
//! This crate provides LSP-independent analysis logic that can be shared between
//! `hew-lsp` (native LSP server) and `hew-wasm` (browser-based editor support).
//! All result types use plain offsets and strings rather than LSP protocol types.

mod ast_visit;
pub mod calls;
pub mod code_actions;
pub mod completions;
pub mod db;
pub mod definition;
pub mod folding;
pub mod hover;
pub mod inlay_hints;
mod method_lookup;
pub mod references;
pub mod rename;
pub mod resolver;
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

/// A rename conflict: applying the rename would introduce a name clash.
///
/// `existing_span` points to the pre-existing binding with the requested
/// name (i.e. the `new_name`'s current definition). `offending_span` is
/// the rename site whose new name would collide with it. `message` is a
/// user-facing description suitable for a preview / `showMessage`.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RenameConflict {
    pub kind: RenameConflictKind,
    pub existing_span: OffsetSpan,
    pub offending_span: OffsetSpan,
    pub message: String,
}

/// Classification of a rename conflict so consumers can decide how to
/// render it (preview, reject outright, offer force-override, ...).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum RenameConflictKind {
    /// The new name already refers to a local binding / parameter in
    /// the same scope.
    ShadowsLocal,
    /// The new name already refers to a top-level item (function, type,
    /// const, actor, wire, ...) in the same file.
    ShadowsTopLevel,
    /// The new name is already brought into scope by an `import`.
    ShadowsImport,
}

/// Failure modes for a rename request. Returned by
/// [`rename::plan_rename`](crate::rename::plan_rename) when the rename
/// must be refused before any edit is produced.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
#[non_exhaustive]
pub enum RenameError {
    /// The new name is a language keyword or a builtin identifier that
    /// cannot be shadowed by user code.
    Builtin { name: String, message: String },
    /// The new name is syntactically invalid (empty, starts with a
    /// digit, contains non-identifier characters).
    InvalidIdentifier { name: String, message: String },
    /// Applying the rename would introduce one or more name clashes.
    Conflicts { conflicts: Vec<RenameConflict> },
    /// The disk scan encountered an I/O error that prevents a complete
    /// conflict or edit check.  `path` is the file or directory that
    /// could not be read; `message` is the OS error string.
    Io { path: String, message: String },
}

impl From<(std::path::PathBuf, std::io::Error)> for RenameError {
    /// Converts a `(path, io_error)` pair into `RenameError::Io`.
    ///
    /// Used by `for_each_hew_file` to propagate traversal-level I/O errors
    /// (e.g. `symlink_metadata` or `read_dir` failing on a specific path)
    /// with the path that triggered the error included in the result, so the
    /// user-facing message reads "rename failed: /some/path: permission denied"
    /// rather than "rename failed: : permission denied".
    fn from((path, e): (std::path::PathBuf, std::io::Error)) -> Self {
        RenameError::Io {
            path: path.display().to_string(),
            message: e.to_string(),
        }
    }
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
