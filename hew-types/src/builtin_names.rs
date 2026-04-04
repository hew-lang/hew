//! Canonical builtin handle-type names used across the compiler.
//!
//! Every place in the Rust-side compiler that must recognise or name a
//! language-level builtin handle type (Sender, Receiver, Stream, Sink) should
//! import its string token from here rather than writing a bare string literal.
//! This makes drift between the dispatch table, the enricher, and any future
//! consumers a compile-time error instead of a silent mismatch.
//!
//! # Naming scheme
//!
//! The *short* constants (e.g. [`SENDER`]) match the canonical normalized name
//! that `Ty::canonical_named_builtin` produces after stripping any module
//! qualifier.  The *qualified* constants (e.g. [`QUALIFIED_SENDER`]) match the
//! module-prefixed form that appears in source before normalization, and are
//! used in pattern guards of the form `name == SENDER || name == QUALIFIED_SENDER`.

// ── Channel handle types ────────────────────────────────────────────────────

/// Canonical short name for a `Sender<T>` handle.
pub const SENDER: &str = "Sender";

/// Qualified name as it appears in source: `channel.Sender`.
pub const QUALIFIED_SENDER: &str = "channel.Sender";

/// Canonical short name for a `Receiver<T>` handle.
pub const RECEIVER: &str = "Receiver";

/// Qualified name as it appears in source: `channel.Receiver`.
pub const QUALIFIED_RECEIVER: &str = "channel.Receiver";

// ── Stream handle types ─────────────────────────────────────────────────────

/// Canonical short name for a `Stream<T>` handle.
pub const STREAM: &str = "Stream";

/// Qualified name as it appears in source: `stream.Stream`.
pub const QUALIFIED_STREAM: &str = "stream.Stream";

/// Canonical short name for a `Sink<T>` handle.
pub const SINK: &str = "Sink";

/// Qualified name as it appears in source: `stream.Sink`.
pub const QUALIFIED_SINK: &str = "stream.Sink";
