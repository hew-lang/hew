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
/// `"bytes"` or `"string"`). Element-type-sensitive methods now fail closed:
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

/// Map a `Vec<T>` element type to its monomorphic runtime symbol suffix.
///
/// Suffixes correspond to the `hew_vec_*_<suffix>` entry points exported by
/// `hew-runtime/src/vec.rs`. Returns `None` for element kinds the runtime
/// has no monomorphic shim for (literal placeholders, `Bool`/`Char`, tuples,
/// inference variables, etc.); callers should leave the call site absent
/// from `method_call_rewrites` in that case so HIR/codegen fail closed.
#[must_use]
pub fn vec_element_runtime_suffix(ty: &crate::Ty) -> Option<&'static str> {
    match ty {
        crate::Ty::I32 | crate::Ty::U32 => Some("i32"),
        crate::Ty::I64 | crate::Ty::U64 => Some("i64"),
        crate::Ty::F64 => Some("f64"),
        crate::Ty::String => Some("str"),
        // Heap-handle / nominal element types share the pointer-shaped ABI.
        crate::Ty::Named { .. } => Some("ptr"),
        _ => None,
    }
}

/// Resolve a `Vec<T>` method call to its `hew_*` C-ABI runtime symbol.
///
/// Mirrors [`resolve_channel_method`] / [`resolve_stream_method`]: the checker
/// calls this when wiring `method_call_rewrites`, and `None` means "no runtime
/// symbol for this (method, element-type) pair — leave the rewrite entry
/// absent so the downstream HIR/MIR/codegen fail closed".
///
/// In-scope V0a methods (per the vec-iterator substrate plan):
/// - Monomorphic: `len`, `is_empty`, `clear`, `clone`, `append`/`extend`,
///   `remove` (by index — uses `hew_vec_remove_at`).
/// - Element-typed: `push`, `pop`, `get`, `set`, `contains` (the last has no
///   pointer-shaped overload in the runtime today).
///
/// Out of V0a: `map`, `filter`, `fold`, `iter` (closure substrate / V0b
/// dependencies), and any element type the runtime has no monomorphic shim
/// for (returns `None`).
#[must_use]
pub fn resolve_vec_method(method: &str, elem_ty: &crate::Ty) -> Option<&'static str> {
    match method {
        "len" => Some("hew_vec_len"),
        "is_empty" => Some("hew_vec_is_empty"),
        "clear" => Some("hew_vec_clear"),
        "clone" => Some("hew_vec_clone"),
        "append" | "extend" => Some("hew_vec_append"),
        // `Vec::remove(i64)` removes by index, not by value. The runtime entry
        // is `hew_vec_remove_at`; the by-value `hew_vec_remove_<elem>` family
        // backs a different (currently unbound) surface and is not wired here.
        "remove" => Some("hew_vec_remove_at"),
        "push" => match vec_element_runtime_suffix(elem_ty)? {
            "i32" => Some("hew_vec_push_i32"),
            "i64" => Some("hew_vec_push_i64"),
            "f64" => Some("hew_vec_push_f64"),
            "str" => Some("hew_vec_push_str"),
            "ptr" => Some("hew_vec_push_ptr"),
            _ => None,
        },
        "pop" => match vec_element_runtime_suffix(elem_ty)? {
            "i32" => Some("hew_vec_pop_i32"),
            "i64" => Some("hew_vec_pop_i64"),
            "f64" => Some("hew_vec_pop_f64"),
            "str" => Some("hew_vec_pop_str"),
            "ptr" => Some("hew_vec_pop_ptr"),
            _ => None,
        },
        "get" => match vec_element_runtime_suffix(elem_ty)? {
            "i32" => Some("hew_vec_get_i32"),
            "i64" => Some("hew_vec_get_i64"),
            "f64" => Some("hew_vec_get_f64"),
            "str" => Some("hew_vec_get_str"),
            "ptr" => Some("hew_vec_get_ptr"),
            _ => None,
        },
        "set" => match vec_element_runtime_suffix(elem_ty)? {
            "i32" => Some("hew_vec_set_i32"),
            "i64" => Some("hew_vec_set_i64"),
            "f64" => Some("hew_vec_set_f64"),
            "str" => Some("hew_vec_set_str"),
            "ptr" => Some("hew_vec_set_ptr"),
            _ => None,
        },
        "contains" => match vec_element_runtime_suffix(elem_ty)? {
            "i32" => Some("hew_vec_contains_i32"),
            "i64" => Some("hew_vec_contains_i64"),
            "f64" => Some("hew_vec_contains_f64"),
            "str" => Some("hew_vec_contains_str"),
            // No `hew_vec_contains_ptr` in the runtime today — fail closed.
            _ => None,
        },
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
        // Channel-family naming: .recv() is the fundamental recv surface.
        assert_eq!(
            resolve_stream_method(STREAM, "recv", Some("string")),
            Some("hew_stream_next")
        );
        assert_eq!(
            resolve_stream_method(STREAM, "recv", Some("bytes")),
            Some("hew_stream_next_bytes")
        );
        assert_eq!(
            resolve_stream_method(STREAM, "close", None),
            Some("hew_stream_close")
        );
        // Iterator-style aliases (.next, .collect, .lines) no longer resolve
        // via the fundamental method table.
        assert_eq!(resolve_stream_method(STREAM, "next", Some("string")), None);
        assert_eq!(resolve_stream_method(STREAM, "collect", None), None);
        assert_eq!(resolve_stream_method(STREAM, "lines", None), None);
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
        // Channel-family naming: .send() is the fundamental send surface.
        assert_eq!(
            resolve_stream_method(SINK, "send", Some("string")),
            Some("hew_sink_write_string")
        );
        assert_eq!(
            resolve_stream_method(SINK, "send", Some("bytes")),
            Some("hew_sink_write_bytes")
        );
        // .write and .flush no longer resolve via the fundamental method table.
        assert_eq!(resolve_stream_method(SINK, "write", Some("string")), None);
        assert_eq!(resolve_stream_method(SINK, "flush", None), None);
        assert_eq!(
            resolve_stream_method(SINK, "close", None),
            Some("hew_sink_close")
        );
    }

    #[test]
    fn stream_unknown_method_returns_none() {
        assert_eq!(resolve_stream_method(STREAM, "nonexistent", None), None);
        assert_eq!(resolve_stream_method("Unknown", "recv", None), None);
    }

    #[test]
    fn stream_element_sensitive_methods_require_lowerable_metadata() {
        // Element-sensitive methods return None for non-lowerable types.
        assert_eq!(resolve_stream_method(STREAM, "recv", None), None);
        assert_eq!(resolve_stream_method(STREAM, "recv", Some("Row")), None);
        // "string" is now canonical (Q57/R14) and must resolve.
        assert_eq!(
            resolve_stream_method(STREAM, "recv", Some("string")),
            Some("hew_stream_next")
        );
        assert_eq!(resolve_stream_method(STREAM, "recv", Some("str")), None);
        assert_eq!(resolve_stream_method(SINK, "send", None), None);
        assert_eq!(resolve_stream_method(SINK, "send", Some("Row")), None);
        // "string" is now canonical and must resolve.
        assert_eq!(
            resolve_stream_method(SINK, "send", Some("string")),
            Some("hew_sink_write_string")
        );
        assert_eq!(resolve_stream_method(SINK, "send", Some("str")), None);
    }

    // ── resolve_vec_method ───────────────────────────────────────────────────

    #[test]
    fn vec_monomorphic_methods_resolve_regardless_of_element_type() {
        use crate::Ty;
        // These do not depend on element type — the runtime exports a single
        // entry point shared across every Vec<T> instantiation.
        for (method, expected) in [
            ("len", "hew_vec_len"),
            ("is_empty", "hew_vec_is_empty"),
            ("clear", "hew_vec_clear"),
            ("clone", "hew_vec_clone"),
            ("append", "hew_vec_append"),
            ("extend", "hew_vec_append"),
            ("remove", "hew_vec_remove_at"),
        ] {
            assert_eq!(
                resolve_vec_method(method, &Ty::I64),
                Some(expected),
                "Vec::{method} should resolve to {expected}"
            );
            // Element-agnostic methods resolve even when the element is a
            // fresh inference variable — the runtime symbol does not depend
            // on suffix in that case.
            assert_eq!(
                resolve_vec_method(method, &Ty::Var(crate::ty::TypeVar::fresh()),),
                Some(expected),
                "Vec::{method} should still resolve for unresolved element",
            );
        }
    }

    #[test]
    fn vec_element_typed_methods_resolve_per_runtime_abi() {
        use crate::Ty;
        // push
        assert_eq!(
            resolve_vec_method("push", &Ty::I32),
            Some("hew_vec_push_i32")
        );
        assert_eq!(
            resolve_vec_method("push", &Ty::U32),
            Some("hew_vec_push_i32")
        );
        assert_eq!(
            resolve_vec_method("push", &Ty::I64),
            Some("hew_vec_push_i64")
        );
        assert_eq!(
            resolve_vec_method("push", &Ty::U64),
            Some("hew_vec_push_i64")
        );
        assert_eq!(
            resolve_vec_method("push", &Ty::F64),
            Some("hew_vec_push_f64")
        );
        assert_eq!(
            resolve_vec_method("push", &Ty::String),
            Some("hew_vec_push_str")
        );
        // pop
        assert_eq!(resolve_vec_method("pop", &Ty::I64), Some("hew_vec_pop_i64"));
        assert_eq!(
            resolve_vec_method("pop", &Ty::String),
            Some("hew_vec_pop_str")
        );
        // get
        assert_eq!(resolve_vec_method("get", &Ty::I64), Some("hew_vec_get_i64"));
        assert_eq!(resolve_vec_method("get", &Ty::F64), Some("hew_vec_get_f64"));
        // set
        assert_eq!(resolve_vec_method("set", &Ty::I32), Some("hew_vec_set_i32"));
        assert_eq!(
            resolve_vec_method("set", &Ty::String),
            Some("hew_vec_set_str")
        );
        // contains
        assert_eq!(
            resolve_vec_method("contains", &Ty::I64),
            Some("hew_vec_contains_i64")
        );
        assert_eq!(
            resolve_vec_method("contains", &Ty::String),
            Some("hew_vec_contains_str")
        );

        // Pointer-shaped (Named) elements share one runtime symbol per method,
        // except `contains` which the runtime does not expose for ptr today.
        let named = Ty::Named {
            name: "Foo".into(),
            args: vec![],
            builtin: None,
        };
        assert_eq!(resolve_vec_method("push", &named), Some("hew_vec_push_ptr"));
        assert_eq!(resolve_vec_method("pop", &named), Some("hew_vec_pop_ptr"));
        assert_eq!(resolve_vec_method("get", &named), Some("hew_vec_get_ptr"));
        assert_eq!(resolve_vec_method("set", &named), Some("hew_vec_set_ptr"));
        assert_eq!(resolve_vec_method("contains", &named), None);
    }

    #[test]
    fn vec_element_typed_methods_fail_closed_for_unresolved_element() {
        use crate::Ty;
        // Inference variable: no monomorphic suffix can be chosen — return
        // None so the caller leaves method_call_rewrites absent and HIR
        // fails closed.
        let var = Ty::Var(crate::ty::TypeVar::fresh());
        for method in ["push", "pop", "get", "set", "contains"] {
            assert_eq!(
                resolve_vec_method(method, &var),
                None,
                "Vec::{method} must not resolve when element type is a Var",
            );
        }
        // Bool/Char are not in the runtime ABI today.
        for elem in [Ty::Bool, Ty::Char] {
            for method in ["push", "pop", "get", "set", "contains"] {
                assert_eq!(
                    resolve_vec_method(method, &elem),
                    None,
                    "Vec::{method} must not resolve for {elem:?}",
                );
            }
        }
    }

    #[test]
    fn vec_unsupported_methods_return_none() {
        use crate::Ty;
        // map/filter/fold/iter are explicitly out of V0a (closure substrate).
        for method in ["map", "filter", "fold", "iter", "nonexistent"] {
            assert_eq!(resolve_vec_method(method, &Ty::I64), None);
        }
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
