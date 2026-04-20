//! Builtin method resolution for language-level types.
//!
//! Stream/Sink methods and handle type representations are intrinsic to
//! the compiler — they are NOT discovered from `.hew` files.

use crate::builtin_names::{builtin_named_type, resolve_builtin_method_symbol, BuiltinNamedType};
#[cfg(test)]
use crate::builtin_names::{RECEIVER, SENDER, SINK, STREAM};

/// Resolves a method call on a `Sender<T>` or `Receiver<T>` to its C symbol.
///
/// String channels use the existing `hew_channel_*` functions.
/// Integer channels use the `hew_channel_*_int` variants.
/// Falls back to String symbols when the inner type is unknown.
#[must_use]
pub fn resolve_channel_method(
    handle_kind: &str,
    method: &str,
    inner_ty: Option<&crate::Ty>,
) -> Option<&'static str> {
    let kind = builtin_named_type(handle_kind)?;
    if !kind.is_channel_handle() {
        return None;
    }
    resolve_builtin_method_symbol(kind, method, inner_ty, None)
}

/// Resolves a method call on a first-class `Stream<T>` or `Sink<T>` to its C symbol.
///
/// The `element_type` parameter carries the resolved inner type name (e.g.
/// `"bytes"` or `"String"`). Element-type-sensitive methods now fail closed:
/// when the inner type is missing or not one of the lowerable runtime ABIs,
/// the resolver returns `None` instead of silently falling back to the string
/// entry point.
///
/// These types are not opaque handle types (`Ty::Named`) — they are `Ty::Stream` /
/// `Ty::Sink` variants.  This resolver is called separately from
/// `resolve_handle_method` and covers both the read and write sides.
#[must_use]
pub fn resolve_stream_method(
    stream_kind: &str,
    method: &str,
    element_type: Option<&str>,
) -> Option<&'static str> {
    let kind = builtin_named_type(stream_kind)?;
    if !matches!(kind, BuiltinNamedType::Stream | BuiltinNamedType::Sink) {
        return None;
    }
    resolve_builtin_method_symbol(kind, method, None, element_type)
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

#[cfg(test)]
mod tests {
    use super::*;

    // ── resolve_channel_method ──────────────────────────────────────────────

    #[test]
    fn channel_sender_methods_resolve() {
        assert_eq!(
            resolve_channel_method(SENDER, "send", None),
            Some("hew_channel_send")
        );
        assert_eq!(
            resolve_channel_method(SENDER, "clone", None),
            Some("hew_channel_sender_clone")
        );
        assert_eq!(
            resolve_channel_method(SENDER, "close", None),
            Some("hew_channel_sender_close")
        );
    }

    #[test]
    fn channel_receiver_methods_resolve() {
        assert_eq!(
            resolve_channel_method(RECEIVER, "recv", None),
            Some("hew_channel_recv")
        );
        assert_eq!(
            resolve_channel_method(RECEIVER, "try_recv", None),
            Some("hew_channel_try_recv")
        );
        assert_eq!(
            resolve_channel_method(RECEIVER, "close", None),
            Some("hew_channel_receiver_close")
        );
    }

    #[test]
    fn channel_unknown_method_returns_none() {
        assert_eq!(resolve_channel_method(SENDER, "nonexistent", None), None);
        assert_eq!(resolve_channel_method("Unknown", "send", None), None);
    }

    // ── resolve_stream_method ───────────────────────────────────────────────

    #[test]
    fn stream_methods_resolve() {
        assert_eq!(
            resolve_stream_method(STREAM, "next", Some("String")),
            Some("hew_stream_next")
        );
        assert_eq!(
            resolve_stream_method(STREAM, "next", Some("bytes")),
            Some("hew_stream_next_bytes")
        );
        assert_eq!(
            resolve_stream_method(STREAM, "close", None),
            Some("hew_stream_close")
        );
        assert_eq!(
            resolve_stream_method(STREAM, "collect", None),
            Some("hew_stream_collect_string")
        );
        assert_eq!(
            resolve_stream_method(STREAM, "lines", None),
            Some("hew_stream_lines")
        );
        assert_eq!(
            resolve_stream_method(STREAM, "chunks", None),
            Some("hew_stream_chunks")
        );
        assert_eq!(
            resolve_stream_method(STREAM, "take", None),
            Some("hew_stream_take")
        );
    }

    #[test]
    fn sink_methods_resolve() {
        assert_eq!(
            resolve_stream_method(SINK, "write", Some("String")),
            Some("hew_sink_write_string")
        );
        assert_eq!(
            resolve_stream_method(SINK, "write", Some("bytes")),
            Some("hew_sink_write_bytes")
        );
        assert_eq!(
            resolve_stream_method(SINK, "flush", None),
            Some("hew_sink_flush")
        );
        assert_eq!(
            resolve_stream_method(SINK, "close", None),
            Some("hew_sink_close")
        );
    }

    #[test]
    fn stream_unknown_method_returns_none() {
        assert_eq!(resolve_stream_method(STREAM, "nonexistent", None), None);
        assert_eq!(resolve_stream_method("Unknown", "next", None), None);
    }

    #[test]
    fn stream_element_sensitive_methods_require_lowerable_metadata() {
        assert_eq!(resolve_stream_method(STREAM, "next", None), None);
        assert_eq!(resolve_stream_method(STREAM, "next", Some("Row")), None);
        assert_eq!(resolve_stream_method(STREAM, "next", Some("string")), None);
        assert_eq!(resolve_stream_method(STREAM, "next", Some("str")), None);
        assert_eq!(resolve_stream_method(SINK, "write", None), None);
        assert_eq!(resolve_stream_method(SINK, "write", Some("Row")), None);
        assert_eq!(resolve_stream_method(SINK, "write", Some("string")), None);
        assert_eq!(resolve_stream_method(SINK, "write", Some("str")), None);
    }

    // ── constants match expected string values ──────────────────────────────

    #[test]
    fn builtin_name_constants_have_expected_values() {
        assert_eq!(SENDER, "Sender");
        assert_eq!(RECEIVER, "Receiver");
        assert_eq!(STREAM, "Stream");
        assert_eq!(SINK, "Sink");
    }
}
