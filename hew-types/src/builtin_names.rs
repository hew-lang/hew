//! Canonical string tokens for builtin handle-type names.
//!
//! **Phase-1 scope** — these constants are currently consumed by:
//! * `hew-types/src/stdlib.rs` — method-dispatch match arms for `resolve_channel_method`
//!   and `resolve_stream_method`
//! * `hew-serialize/src/enrich.rs` — handle-dispatch pattern guards in the enricher
//!
//! Other compiler sources that carry the same names as bare string literals
//! (`ty.rs::canonical_named_builtin`, `check.rs`, LSP consumers) are **not yet
//! migrated** and are intentionally out of scope for this phase.  Follow-on
//! slices will extend usage of this module to those sites.
//!
//! # Naming scheme
//!
//! The *short* constants (e.g. [`SENDER`]) are the normalized forms that appear
//! after any module qualifier is stripped.  The *qualified* constants
//! (e.g. [`QUALIFIED_SENDER`]) are the module-prefixed forms that can appear in
//! source before normalization, used in pattern guards of the form
//! `name == SENDER || name == QUALIFIED_SENDER`.

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
