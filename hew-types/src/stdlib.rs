//! Builtin method resolution for language-level types.
//!
//! Stream/Sink methods and handle type representations are intrinsic to
//! the compiler — they are NOT discovered from `.hew` files.

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
    let is_int = inner_ty.is_some_and(crate::Ty::is_integer);
    match (handle_kind, method, is_int) {
        // Sender methods
        (k, "send", false) if k == SENDER => Some("hew_channel_send"),
        (k, "send", true) if k == SENDER => Some("hew_channel_send_int"),
        (k, "clone", _) if k == SENDER => Some("hew_channel_sender_clone"),
        (k, "close", _) if k == SENDER => Some("hew_channel_sender_close"),
        // Receiver methods
        (k, "recv", false) if k == RECEIVER => Some("hew_channel_recv"),
        (k, "recv", true) if k == RECEIVER => Some("hew_channel_recv_int"),
        (k, "try_recv", false) if k == RECEIVER => Some("hew_channel_try_recv"),
        (k, "try_recv", true) if k == RECEIVER => Some("hew_channel_try_recv_int"),
        (k, "close", _) if k == RECEIVER => Some("hew_channel_receiver_close"),
        _ => None,
    }
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
    let is_string = matches!(element_type, Some("String" | "string" | "str"));
    let is_bytes = element_type == Some("bytes");
    match (stream_kind, method) {
        // Stream<T> methods — element-type-dependent
        (k, "next") if k == STREAM && is_bytes => Some("hew_stream_next_bytes"),
        (k, "next") if k == STREAM && is_string => Some("hew_stream_next"),
        (k, "collect") if k == STREAM => Some("hew_stream_collect_string"),
        // Stream<T> methods — element-type-independent
        (k, "close") if k == STREAM => Some("hew_stream_close"),
        (k, "lines") if k == STREAM => Some("hew_stream_lines"),
        (k, "chunks") if k == STREAM => Some("hew_stream_chunks"),
        (k, "take") if k == STREAM => Some("hew_stream_take"),
        // Sink<T> methods — element-type-dependent
        (k, "write") if k == SINK && is_bytes => Some("hew_sink_write_bytes"),
        (k, "write") if k == SINK && is_string => Some("hew_sink_write_string"),
        // Sink<T> methods — element-type-independent
        (k, "flush") if k == SINK => Some("hew_sink_flush"),
        (k, "close") if k == SINK => Some("hew_sink_close"),
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
        assert_eq!(resolve_stream_method(SINK, "write", None), None);
        assert_eq!(resolve_stream_method(SINK, "write", Some("Row")), None);
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
