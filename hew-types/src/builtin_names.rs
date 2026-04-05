//! Canonical string tokens and helpers for builtin named types.
//!
//! These names are shared by type normalization, builtin method resolution,
//! enrichment, and analysis/LSP surfaces so that qualified and unqualified
//! spellings converge on one model.

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

/// Builtin named types whose resolution is intrinsic to the compiler.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BuiltinNamedType {
    Sender,
    Receiver,
    Stream,
    Sink,
}

impl BuiltinNamedType {
    /// Canonical short name used after normalization.
    #[must_use]
    pub const fn canonical_name(self) -> &'static str {
        match self {
            Self::Sender => SENDER,
            Self::Receiver => RECEIVER,
            Self::Stream => STREAM,
            Self::Sink => SINK,
        }
    }

    /// Qualified source spelling accepted before normalization.
    #[must_use]
    pub const fn qualified_name(self) -> &'static str {
        match self {
            Self::Sender => QUALIFIED_SENDER,
            Self::Receiver => QUALIFIED_RECEIVER,
            Self::Stream => QUALIFIED_STREAM,
            Self::Sink => QUALIFIED_SINK,
        }
    }

    /// Whether this builtin is one of the `channel.*` handle types.
    #[must_use]
    pub const fn is_channel_handle(self) -> bool {
        matches!(self, Self::Sender | Self::Receiver)
    }
}

/// Resolve any supported builtin named type spelling to its enum form.
#[must_use]
pub fn builtin_named_type(name: &str) -> Option<BuiltinNamedType> {
    Some(match name {
        SENDER | QUALIFIED_SENDER => BuiltinNamedType::Sender,
        RECEIVER | QUALIFIED_RECEIVER => BuiltinNamedType::Receiver,
        STREAM | QUALIFIED_STREAM => BuiltinNamedType::Stream,
        SINK | QUALIFIED_SINK => BuiltinNamedType::Sink,
        _ => return None,
    })
}

/// Resolve a builtin named type spelling to its canonical short name.
#[must_use]
pub fn canonical_builtin_named_type_name(name: &str) -> Option<&'static str> {
    builtin_named_type(name).map(BuiltinNamedType::canonical_name)
}
