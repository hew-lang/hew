//! Builtin method resolution for language-level types.
//!
//! Stream/Sink methods and handle type representations are intrinsic to
//! the compiler — they are NOT discovered from `.hew` files.

/// Resolves a method call on a first-class `Stream<T>` or `Sink<T>` to its C symbol.
///
/// These types are not opaque handle types (`Ty::Named`) — they are `Ty::Stream` /
/// `Ty::Sink` variants.  This resolver is called separately from
/// `resolve_handle_method` and covers both the read and write sides.
#[must_use]
pub fn resolve_stream_method(stream_kind: &str, method: &str) -> Option<&'static str> {
    match (stream_kind, method) {
        // Stream<T> methods
        ("Stream", "next") => Some("hew_stream_next"),
        ("Stream", "next_bytes") => Some("hew_stream_next_bytes"),
        ("Stream", "close") => Some("hew_stream_close"),
        ("Stream", "lines") => Some("hew_stream_lines"),
        ("Stream", "chunks") => Some("hew_stream_chunks"),
        ("Stream", "collect") => Some("hew_stream_collect_string"),
        // Sink<T> methods
        ("Sink", "write") => Some("hew_sink_write_string"),
        ("Sink", "write_bytes") => Some("hew_sink_write_bytes"),
        ("Sink", "flush") => Some("hew_sink_flush"),
        ("Sink", "close") => Some("hew_sink_close"),
        _ => None,
    }
}

/// Returns the MLIR representation for a handle type.
///
/// Most handle types are opaque pointers (`"handle"`).
/// File-descriptor types like `net.Listener` and `net.Connection` use `"i32"`.
///
// SHIM: This hardcoded mapping should eventually move to `hew.toml` metadata
// in each module's package directory, so modules can declare their own
// representation. Remove once the `hew.toml` format supports a `repr` field.
#[must_use]
pub fn handle_type_representation(name: &str) -> &'static str {
    match name {
        "net.Listener" | "net.Connection" => "i32",
        _ => "handle",
    }
}
