//! Standard library function registry for the type checker.
//!
//! Maps module paths (e.g., `std::encoding::json`, `std::net::http`) to their
//! function signatures, allowing the type checker to validate calls to stdlib
//! functions when users write `import std::encoding::json;` or
//! `import std::net::http;`.
//!
//! All tables are generated at build time from `.hew` files by `build.rs`.

use crate::ty::Ty;

/// A stdlib function signature: `(name, param_types, return_type)`.
type StdlibFn = (String, Vec<Ty>, Ty);

include!(concat!(env!("OUT_DIR"), "/stdlib_generated.rs"));

/// Returns the function signatures for a standard library module.
///
/// Returns `None` if the module path is not a known stdlib module.
#[must_use]
pub fn stdlib_functions(module_path: &str) -> Option<Vec<StdlibFn>> {
    generated_stdlib_functions(module_path)
}

/// Returns wrapper `pub fn` signatures for a standard library module.
///
/// These are the signatures of the Hew wrapper functions (not the underlying
/// extern C functions they may call). For example, `pub fn setup()` in the log
/// module has 0 params even though its underlying C function takes 1 param.
///
/// Returns `None` if the module path is not a known stdlib module.
#[must_use]
pub fn wrapper_fn_sigs(module_path: &str) -> Option<Vec<StdlibFn>> {
    generated_wrapper_fn_sigs(module_path)
}

/// Returns the short module name for a stdlib module path.
///
/// For example, `"std::encoding::json"` returns `"json"` and
/// `"std::net::http"` returns `"http"` (last path segment).
#[must_use]
pub fn module_short_name(module_path: &str) -> Option<&'static str> {
    generated_module_short_name(module_path)
}

/// Returns the clean-name-to-C-symbol mapping for a stdlib module.
///
/// Each entry is `(clean_method_name, c_symbol_name)`, e.g. `("listen", "hew_http_server_new")`.
/// Returns `None` if the module path is not a known stdlib module.
#[must_use]
pub fn stdlib_clean_names(module_path: &str) -> Option<Vec<(&'static str, &'static str)>> {
    generated_stdlib_clean_names(module_path)
}

/// Resolves a module-qualified method call to its C symbol name.
///
/// Given a module short name (e.g., `"http"`) and a method name (e.g., `"listen"`),
/// returns the C symbol name (e.g., `"hew_http_server_new"`).
#[must_use]
pub fn resolve_module_call(module: &str, method: &str) -> Option<&'static str> {
    generated_resolve_module_call(module, method)
}

/// Resolves a method call on a handle type to its C symbol name.
///
/// Given a handle type name (e.g., `"http.Request"`) and a method name (e.g., `"path"`),
/// returns the C symbol name (e.g., `"hew_http_request_path"`).
#[must_use]
pub fn resolve_handle_method(handle_type: &str, method: &str) -> Option<&'static str> {
    generated_resolve_handle_method(handle_type, method)
}

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
        ("Stream", "close") => Some("hew_stream_close"),
        ("Stream", "lines") => Some("hew_stream_lines"),
        ("Stream", "chunks") => Some("hew_stream_chunks"),
        ("Stream", "collect") => Some("hew_stream_collect_string"),
        // Sink<T> methods
        ("Sink", "write") => Some("hew_sink_write_string"),
        ("Sink", "flush") => Some("hew_sink_flush"),
        ("Sink", "close") => Some("hew_sink_close"),
        _ => None,
    }
}

/// Returns the handle type names introduced by a stdlib module.
///
/// For example, `"std::net::http"` returns `["http.Server", "http.Request", "http.Response"]`.
#[must_use]
pub fn handle_types_for_module(module_path: &str) -> Vec<&'static str> {
    generated_handle_types_for_module(module_path)
}

/// Checks whether a type name is an opaque handle type.
#[must_use]
pub fn is_handle_type(name: &str) -> bool {
    GENERATED_HANDLE_TYPES.contains(&name)
}

/// Returns `true` if the type has `impl Drop` and should be treated as
/// move-only (not Copy).  These types own resources and need ownership
/// transfer semantics when sent to actors.
pub fn is_drop_type(name: &str) -> bool {
    GENERATED_DROP_TYPES.contains(&name)
}

/// Returns `true` if the unqualified name matches a known drop type.
pub fn is_unqualified_drop_type(name: &str) -> bool {
    if name.contains('.') {
        return false;
    }
    GENERATED_DROP_TYPES
        .iter()
        .any(|dt| dt.rsplit_once('.').is_some_and(|(_, short)| short == name))
}

/// Returns all known handle type names.
#[must_use]
pub fn all_handle_types() -> Vec<String> {
    GENERATED_HANDLE_TYPES
        .iter()
        .map(|s| (*s).to_string())
        .collect()
}

/// Returns the MLIR representation for a handle type.
///
/// Most handle types are opaque pointers (`"handle"`).
/// File-descriptor types like `net.Listener` and `net.Connection` use `"i32"`.
#[must_use]
pub fn handle_type_representation(name: &str) -> &'static str {
    match name {
        "net.Listener" | "net.Connection" => "i32",
        _ => "handle",
    }
}

/// Returns `true` if the unqualified name (without module prefix) matches
/// the short name of any known handle type (e.g. `"Connection"` matches
/// `"net.Connection"`).
pub fn is_unqualified_handle_type(name: &str) -> bool {
    if name.contains('.') {
        return false;
    }
    GENERATED_HANDLE_TYPES
        .iter()
        .any(|ht| ht.rsplit_once('.').is_some_and(|(_, short)| short == name))
}

/// If `name` is an unqualified handle type (e.g. `"Connection"`), returns
/// the fully qualified name (e.g. `"net.Connection"`). Returns `None` if
/// the name doesn't match any known handle type.
pub fn qualify_handle_type(name: &str) -> Option<&'static str> {
    if name.contains('.') {
        return None;
    }
    GENERATED_HANDLE_TYPES
        .iter()
        .find(|ht| ht.rsplit_once('.').is_some_and(|(_, short)| short == name))
        .copied()
}
