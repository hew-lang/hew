//! Builtin method resolution for language-level types.
//!
//! Stream/Sink methods and handle type representations are intrinsic to
//! the compiler — they are NOT discovered from `.hew` files.

use crate::builtin_names::{builtin_named_type, resolve_builtin_method_symbol, BuiltinNamedType};
#[cfg(test)]
use crate::builtin_names::{SINK, STREAM};

/// Canonical source identities for the TCP substrate handles.
///
/// `net` is only an import binding. Semantic tables must use the full owner
/// so a user module ending in `net` cannot acquire TCP runtime behaviour.
pub const STD_NET_LISTENER: &str = "std.net.Listener";
pub const STD_NET_CONNECTION: &str = "std.net.Connection";
pub const STD_NET_ERROR: &str = "std.net.NetError";
pub const STD_NET_WRITE_ERROR: &str = "std.net.WriteError";

/// Resolves a method call on a first-class `Stream<T>` or `Sink<T>` to its C symbol.
///
/// Every pipe operation has one symbol for every describable element type:
/// the element identity travels on the checked type, never on the symbol.
#[must_use]
pub fn resolve_stream_method(stream_kind: &str, method: &str) -> Option<&'static str> {
    let kind = builtin_named_type(stream_kind)?;
    if !matches!(kind, BuiltinNamedType::Stream | BuiltinNamedType::Sink) {
        return None;
    }
    resolve_builtin_method_symbol(kind, method)
}

/// Returns the codegen representation for a handle type.
///
/// Most handle types are opaque pointers (`"handle"`).
/// File-descriptor types like `std.net.Listener` and `std.net.Connection` use `"i32"`.
///
// SHIM: This hardcoded mapping should eventually move to `hew.toml` metadata
// in each module's package directory, so modules can declare their own
// representation. Remove once the `hew.toml` format supports a `repr` field.
#[must_use]
pub fn handle_type_representation(name: &str) -> &'static str {
    match name {
        STD_NET_LISTENER | STD_NET_CONNECTION => "i32",
        _ => "handle",
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn stream_methods_resolve() {
        assert_eq!(
            resolve_stream_method(STREAM, "recv"),
            Some("hew_stream_next_layout")
        );
        assert_eq!(
            resolve_stream_method(STREAM, "try_recv"),
            Some("hew_stream_try_next_layout")
        );
        assert_eq!(
            resolve_stream_method(STREAM, "close"),
            Some("hew_stream_close")
        );
        assert_eq!(
            resolve_stream_method(STREAM, "lines"),
            Some("hew_stream_lines")
        );
        assert_eq!(
            resolve_stream_method(STREAM, "chunks"),
            Some("hew_stream_chunks")
        );
        assert_eq!(
            resolve_stream_method(STREAM, "take"),
            Some("hew_stream_take")
        );
        assert_eq!(
            resolve_stream_method(STREAM, "collect"),
            Some("hew_stream_collect_string")
        );
        assert_eq!(resolve_stream_method(STREAM, "next"), None);
    }

    #[test]
    fn sink_methods_resolve() {
        assert_eq!(
            resolve_stream_method(SINK, "send"),
            Some("hew_stream_send_layout")
        );
        assert_eq!(
            resolve_stream_method(SINK, "try_send"),
            Some("hew_stream_try_send_layout")
        );
        assert_eq!(resolve_stream_method(SINK, "clone"), Some("hew_sink_clone"));
        assert_eq!(
            resolve_stream_method(SINK, "finish"),
            Some("hew_sink_finish")
        );
        assert_eq!(resolve_stream_method(SINK, "close"), Some("hew_sink_close"));
        assert_eq!(resolve_stream_method(SINK, "write"), None);
        assert_eq!(resolve_stream_method(SINK, "flush"), None);
    }

    #[test]
    fn unknown_kind_or_method_returns_none() {
        assert_eq!(resolve_stream_method(STREAM, "nonexistent"), None);
        assert_eq!(resolve_stream_method("Unknown", "recv"), None);
        assert_eq!(STREAM, "Stream");
        assert_eq!(SINK, "Sink");
    }
}
