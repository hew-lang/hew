//! Builtin method resolution for language-level types.
//!
//! Stream/Sink methods and handle type representations are intrinsic to
//! the compiler — they are NOT discovered from `.hew` files.

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
        ("Sender", "send", false) => Some("hew_channel_send"),
        ("Sender", "send", true) => Some("hew_channel_send_int"),
        ("Sender", "clone", _) => Some("hew_channel_sender_clone"),
        ("Sender", "close", _) => Some("hew_channel_sender_close"),
        // Receiver methods
        ("Receiver", "recv", false) => Some("hew_channel_recv"),
        ("Receiver", "recv", true) => Some("hew_channel_recv_int"),
        ("Receiver", "try_recv", false) => Some("hew_channel_try_recv"),
        ("Receiver", "try_recv", true) => Some("hew_channel_try_recv_int"),
        ("Receiver", "close", _) => Some("hew_channel_receiver_close"),
        _ => None,
    }
}

/// Resolves a method call on a first-class `Stream<T>` or `Sink<T>` to its C symbol.
///
/// The `element_type` parameter carries the resolved inner type name (e.g.
/// `"bytes"` or `"String"`).  When the element is `bytes`, the enricher
/// dispatches to the bytes-specific runtime entry points; all other types
/// (including `None` / unknown) fall through to the default string ABI.
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
    let is_bytes = element_type == Some("bytes");
    match (stream_kind, method) {
        // Stream<T> methods — element-type-dependent
        ("Stream", "next") => Some(if is_bytes {
            "hew_stream_next_bytes"
        } else {
            "hew_stream_next"
        }),
        ("Stream", "collect") => Some("hew_stream_collect_string"),
        // Stream<T> methods — element-type-independent
        ("Stream", "close") => Some("hew_stream_close"),
        ("Stream", "lines") => Some("hew_stream_lines"),
        ("Stream", "chunks") => Some("hew_stream_chunks"),
        ("Stream", "take") => Some("hew_stream_take"),
        // Sink<T> methods — element-type-dependent
        ("Sink", "write") => Some(if is_bytes {
            "hew_sink_write_bytes"
        } else {
            "hew_sink_write_string"
        }),
        // Sink<T> methods — element-type-independent
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
