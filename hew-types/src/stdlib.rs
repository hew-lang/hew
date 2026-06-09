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
/// `hew-runtime/src/vec.rs`. The `"layout"` suffix is the substrate
/// signal for the layout-descriptor protocol that lowers value records and
/// tuples (Stage 3a Step 1 — runtime/codegen support lands in Step 2 /
/// Stage 3a Step 3). Callers must turn `"layout"` into a precise
/// fail-closed diagnostic until those steps land.
///
/// Returns `None` for element kinds where no classification is yet
/// possible (inference variables, error sentinels, unresolved nominals,
/// types with no Vec-element shape). Callers should leave the call site
/// absent from `method_call_rewrites` in that case so HIR/codegen fail
/// closed.
///
/// Substrate routing of named (`Ty::Named`) elements follows the
/// `TypeDef::is_indirect` classification — the same authority that
/// `RuntimeCallingConvention::for_ty_with_layout` consults — rather than
/// the legacy "all named types lower as `ptr`" shortcut, which silently
/// punched value records through the pointer-shaped ABI.
#[must_use]
#[allow(
    clippy::implicit_hasher,
    reason = "uses the checker's concrete TypeDef table shape"
)]
pub fn vec_element_runtime_suffix<S: std::hash::BuildHasher>(
    ty: &crate::Ty,
    type_defs: &std::collections::HashMap<String, crate::check::TypeDef, S>,
) -> Option<&'static str> {
    match ty {
        crate::Ty::Bool => Some("bool"),
        crate::Ty::Char | crate::Ty::I32 | crate::Ty::U32 => Some("i32"),
        crate::Ty::I64 | crate::Ty::U64 => Some("i64"),
        crate::Ty::F64 => Some("f64"),
        crate::Ty::String => Some("string"),
        // Tuples lower through the layout-descriptor protocol.
        crate::Ty::Tuple(_) => Some("layout"),
        // Local actor-handle builtins (`LocalPid<T>` / `ActorRef<T>` /
        // `Actor<T>`) lower to a single pointer-shaped runtime word and their
        // `Vec<T>` constructor routes to `hew_vec_new_ptr` in codegen. Classify
        // them by the `builtin` discriminant — NOT the `TypeDef.is_indirect`
        // field, which these builtins leave `false` — so the element ops route
        // to the `hew_vec_*_ptr` family and agree with the constructor. Missing
        // this arm sent them to `"layout"`, which built a null-layout vec via
        // `hew_vec_new_ptr` and then pushed via `hew_vec_push_layout`, tripping
        // the runtime "layout-aware operation is not implemented" abort
        // (constructor-vs-push authority split).
        crate::Ty::Named {
            builtin: Some(b), ..
        } if b.lowers_as_pointer_vec_element() => Some("ptr"),
        // Named element types: heap-handle nominals (`is_indirect = true`)
        // share the pointer-shaped ABI; value-record nominals
        // (`is_indirect = false`) lower through the layout-descriptor
        // protocol. Unknown nominal — defer; callers fail closed.
        crate::Ty::Named { name, .. } => match type_defs.get(name) {
            Some(td) if td.is_indirect => Some("ptr"),
            Some(_) => Some("layout"),
            None => None,
        },
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
#[allow(
    clippy::implicit_hasher,
    reason = "uses the checker's concrete TypeDef table shape"
)]
pub fn resolve_vec_method<S: std::hash::BuildHasher>(
    method: &str,
    elem_ty: &crate::Ty,
    type_defs: &std::collections::HashMap<String, crate::check::TypeDef, S>,
) -> Option<&'static str> {
    match method {
        "len" => Some("hew_vec_len"),
        "is_empty" => Some("hew_vec_is_empty"),
        // `clear`, `clone`, `append`/`extend`, and `remove` are monomorphic
        // for scalar and pointer-shaped element types. Value-record and tuple
        // elements lower through the layout-descriptor protocol; route through
        // a `_layout`-suffixed symbol so `resolve_vec_runtime_symbol` can emit
        // the precise fail-closed diagnostic rather than silently using the
        // monomorphic entry on a type it cannot handle.
        //
        // The `_ =>` arm preserves the prior monomorphic behaviour for
        // scalar / ptr / unresolved (inference-variable) elements, keeping
        // the `vec_monomorphic_methods_resolve_regardless_of_element_type`
        // property intact for non-layout types.
        "clear" => match vec_element_runtime_suffix(elem_ty, type_defs) {
            Some("layout") => Some("hew_vec_clear_layout"),
            _ => Some("hew_vec_clear"),
        },
        "clone" => match vec_element_runtime_suffix(elem_ty, type_defs) {
            Some("layout") => Some("hew_vec_clone_layout"),
            _ => Some("hew_vec_clone"),
        },
        "append" | "extend" => match vec_element_runtime_suffix(elem_ty, type_defs) {
            Some("layout") => Some("hew_vec_append_layout"),
            _ => Some("hew_vec_append"),
        },
        // `Vec::remove(i64)` removes by index. For scalar/ptr elements the
        // operation is monomorphic (`hew_vec_remove_at`). For value-record
        // and tuple elements the layout-descriptor protocol is required —
        // route through `_layout` suffix so the fail-closed diagnostic fires.
        "remove" => match vec_element_runtime_suffix(elem_ty, type_defs) {
            Some("layout") => Some("hew_vec_remove_at_layout"),
            _ => Some("hew_vec_remove_at"),
        },
        "push" => match vec_element_runtime_suffix(elem_ty, type_defs)? {
            "bool" => Some("hew_vec_push_bool"),
            "i32" => Some("hew_vec_push_i32"),
            "i64" => Some("hew_vec_push_i64"),
            "f64" => Some("hew_vec_push_f64"),
            "string" => Some("hew_vec_push_str"),
            "ptr" => Some("hew_vec_push_ptr"),
            "layout" => Some("hew_vec_push_layout"),
            _ => None,
        },
        "pop" => match vec_element_runtime_suffix(elem_ty, type_defs)? {
            "bool" => Some("hew_vec_pop_bool"),
            "i32" => Some("hew_vec_pop_i32"),
            "i64" => Some("hew_vec_pop_i64"),
            "f64" => Some("hew_vec_pop_f64"),
            "string" => Some("hew_vec_pop_str"),
            "ptr" => Some("hew_vec_pop_ptr"),
            "layout" => Some("hew_vec_pop_layout"),
            _ => None,
        },
        "get" => match vec_element_runtime_suffix(elem_ty, type_defs)? {
            "bool" => Some("hew_vec_get_bool"),
            "i32" => Some("hew_vec_get_i32"),
            "i64" => Some("hew_vec_get_i64"),
            "f64" => Some("hew_vec_get_f64"),
            "string" => Some("hew_vec_get_str"),
            "ptr" => Some("hew_vec_get_ptr"),
            "layout" => Some("hew_vec_get_layout"),
            _ => None,
        },
        "set" => match vec_element_runtime_suffix(elem_ty, type_defs)? {
            "bool" => Some("hew_vec_set_bool"),
            "i32" => Some("hew_vec_set_i32"),
            "i64" => Some("hew_vec_set_i64"),
            "f64" => Some("hew_vec_set_f64"),
            "string" => Some("hew_vec_set_str"),
            "ptr" => Some("hew_vec_set_ptr"),
            "layout" => Some("hew_vec_set_layout"),
            _ => None,
        },
        "contains" => match vec_element_runtime_suffix(elem_ty, type_defs)? {
            "i32" => Some("hew_vec_contains_i32"),
            "i64" => Some("hew_vec_contains_i64"),
            "f64" => Some("hew_vec_contains_f64"),
            "string" => Some("hew_vec_contains_str"),
            // W3.032 Slice 3e: layout (value-record/tuple) elements now route
            // through `hew_vec_contains_thunk`.  The checker layer
            // (`check_vec_method` `contains` arm) gates by equality
            // eligibility + `Copy`; only equality-eligible Copy aggregates
            // reach this point with the rewrite recorded.  Ineligible /
            // non-Copy layouts produce a type-error before
            // `resolve_vec_runtime_symbol` is consulted.
            //
            // NOTE: `hew_vec_contains_layout` (the historical abort stub for
            // managed/non-Copy layouts) is intentionally retained in the
            // runtime ABI for future use, but is no longer the routing target
            // for any checker-accepted call.
            "layout" => Some("hew_vec_contains_thunk"),
            // Pointer-shaped heap-handle nominals (`ptr`) and `bool` have no
            // `contains` overload in the runtime — fail closed.
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
        let type_defs = std::collections::HashMap::new();
        // For scalar and pointer-shaped element types — and for inference
        // variables (unresolved `Ty::Var`) — these methods resolve to their
        // monomorphic runtime entry point.
        //
        // NOTE: `clear`, `clone`, `append`, `extend`, and `remove` are NOT
        // unconditionally monomorphic: value-record and tuple (layout) elements
        // route through `_layout`-suffixed symbols (asserted in
        // `vec_layout_methods_route_through_layout_suffix`). Only
        // scalar/ptr/unresolved elements use the monomorphic entry, which is
        // what this test exercises.
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
                resolve_vec_method(method, &Ty::I64, &type_defs),
                Some(expected),
                "Vec::{method} should resolve to {expected} for scalar element"
            );
            // Inference variable: suffix returns None, `_ =>` arm preserves
            // the monomorphic symbol so HIR gets a rewrite entry.
            assert_eq!(
                resolve_vec_method(method, &Ty::Var(crate::ty::TypeVar::fresh()), &type_defs),
                Some(expected),
                "Vec::{method} should still resolve for unresolved element (non-layout infer)",
            );
        }
    }

    #[allow(
        clippy::too_many_lines,
        reason = "exhaustive ABI matrix covering all element types and routing cases"
    )]
    #[test]
    fn vec_element_typed_methods_resolve_per_runtime_abi() {
        use crate::Ty;
        let type_defs = std::collections::HashMap::new();
        // push
        assert_eq!(
            resolve_vec_method("push", &Ty::Bool, &type_defs),
            Some("hew_vec_push_bool")
        );
        assert_eq!(
            resolve_vec_method("push", &Ty::Char, &type_defs),
            Some("hew_vec_push_i32")
        );
        assert_eq!(
            resolve_vec_method("push", &Ty::I32, &type_defs),
            Some("hew_vec_push_i32")
        );
        assert_eq!(
            resolve_vec_method("push", &Ty::U32, &type_defs),
            Some("hew_vec_push_i32")
        );
        assert_eq!(
            resolve_vec_method("push", &Ty::I64, &type_defs),
            Some("hew_vec_push_i64")
        );
        assert_eq!(
            resolve_vec_method("push", &Ty::U64, &type_defs),
            Some("hew_vec_push_i64")
        );
        assert_eq!(
            resolve_vec_method("push", &Ty::F64, &type_defs),
            Some("hew_vec_push_f64")
        );
        assert_eq!(
            resolve_vec_method("push", &Ty::String, &type_defs),
            Some("hew_vec_push_str")
        );
        // pop
        assert_eq!(
            resolve_vec_method("pop", &Ty::Bool, &type_defs),
            Some("hew_vec_pop_bool")
        );
        assert_eq!(
            resolve_vec_method("pop", &Ty::Char, &type_defs),
            Some("hew_vec_pop_i32")
        );
        assert_eq!(
            resolve_vec_method("pop", &Ty::I64, &type_defs),
            Some("hew_vec_pop_i64")
        );
        assert_eq!(
            resolve_vec_method("pop", &Ty::String, &type_defs),
            Some("hew_vec_pop_str")
        );
        // get
        assert_eq!(
            resolve_vec_method("get", &Ty::Bool, &type_defs),
            Some("hew_vec_get_bool")
        );
        assert_eq!(
            resolve_vec_method("get", &Ty::Char, &type_defs),
            Some("hew_vec_get_i32")
        );
        assert_eq!(
            resolve_vec_method("get", &Ty::I64, &type_defs),
            Some("hew_vec_get_i64")
        );
        assert_eq!(
            resolve_vec_method("get", &Ty::F64, &type_defs),
            Some("hew_vec_get_f64")
        );
        // set
        assert_eq!(
            resolve_vec_method("set", &Ty::Bool, &type_defs),
            Some("hew_vec_set_bool")
        );
        assert_eq!(
            resolve_vec_method("set", &Ty::Char, &type_defs),
            Some("hew_vec_set_i32")
        );
        assert_eq!(
            resolve_vec_method("set", &Ty::I32, &type_defs),
            Some("hew_vec_set_i32")
        );
        assert_eq!(
            resolve_vec_method("set", &Ty::String, &type_defs),
            Some("hew_vec_set_str")
        );
        // contains
        assert_eq!(
            resolve_vec_method("contains", &Ty::I64, &type_defs),
            Some("hew_vec_contains_i64")
        );
        assert_eq!(
            resolve_vec_method("contains", &Ty::String, &type_defs),
            Some("hew_vec_contains_str")
        );

        // Heap-handle named element (`is_indirect = true`) — pointer-shaped
        // runtime ABI; `contains` has no `_ptr` overload.
        let vec_handle_defs = {
            use crate::check::{TypeDef, TypeDefKind};
            let mut m = std::collections::HashMap::new();
            m.insert(
                "Vec".to_string(),
                TypeDef {
                    kind: TypeDefKind::Struct,
                    name: "Vec".to_string(),
                    type_params: vec!["T".to_string()],
                    fields: std::collections::HashMap::new(),
                    variants: std::collections::HashMap::new(),
                    methods: std::collections::HashMap::new(),
                    doc_comment: None,
                    field_order: vec![],
                    is_indirect: true,
                },
            );
            m
        };
        let vec_named = Ty::Named {
            name: "Vec".into(),
            args: vec![Ty::I32],
            builtin: None,
        };
        assert_eq!(
            resolve_vec_method("push", &vec_named, &vec_handle_defs),
            Some("hew_vec_push_ptr")
        );
        assert_eq!(
            resolve_vec_method("pop", &vec_named, &vec_handle_defs),
            Some("hew_vec_pop_ptr")
        );
        assert_eq!(
            resolve_vec_method("get", &vec_named, &vec_handle_defs),
            Some("hew_vec_get_ptr")
        );
        assert_eq!(
            resolve_vec_method("set", &vec_named, &vec_handle_defs),
            Some("hew_vec_set_ptr")
        );
        assert_eq!(
            resolve_vec_method("contains", &vec_named, &vec_handle_defs),
            None
        );

        // Value-record named element (`is_indirect = false`) — routes
        // through the layout-descriptor protocol. The checker turns this
        // suffix into a precise fail-closed diagnostic until Stage 3a
        // Step 2 lands the runtime BitCopy ops.
        let record_defs = {
            use crate::check::{TypeDef, TypeDefKind};
            let mut m = std::collections::HashMap::new();
            m.insert(
                "Point".to_string(),
                TypeDef {
                    kind: TypeDefKind::Struct,
                    name: "Point".to_string(),
                    type_params: vec![],
                    fields: std::collections::HashMap::new(),
                    variants: std::collections::HashMap::new(),
                    methods: std::collections::HashMap::new(),
                    doc_comment: None,
                    field_order: vec![],
                    is_indirect: false,
                },
            );
            m
        };
        let point = Ty::Named {
            name: "Point".into(),
            args: vec![],
            builtin: None,
        };
        assert_eq!(
            resolve_vec_method("push", &point, &record_defs),
            Some("hew_vec_push_layout")
        );
        assert_eq!(
            resolve_vec_method("pop", &point, &record_defs),
            Some("hew_vec_pop_layout")
        );
        assert_eq!(
            resolve_vec_method("get", &point, &record_defs),
            Some("hew_vec_get_layout")
        );
        assert_eq!(
            resolve_vec_method("set", &point, &record_defs),
            Some("hew_vec_set_layout")
        );
        // `contains` on layout-backed elements now routes through the
        // checker-authorized `hew_vec_contains_thunk` symbol (W3.032 Slice 3e);
        // ineligible/non-Copy layouts are blocked by the higher-layer
        // `check_vec_method` `contains` arm before this resolver is consulted.
        assert_eq!(
            resolve_vec_method("contains", &point, &record_defs),
            Some("hew_vec_contains_thunk")
        );
        assert_eq!(
            resolve_vec_method("clear", &point, &record_defs),
            Some("hew_vec_clear_layout")
        );
        assert_eq!(
            resolve_vec_method("clone", &point, &record_defs),
            Some("hew_vec_clone_layout")
        );
        assert_eq!(
            resolve_vec_method("append", &point, &record_defs),
            Some("hew_vec_append_layout")
        );
        assert_eq!(
            resolve_vec_method("extend", &point, &record_defs),
            Some("hew_vec_append_layout")
        );
        assert_eq!(
            resolve_vec_method("remove", &point, &record_defs),
            Some("hew_vec_remove_at_layout")
        );

        // Tuple element — always routes through the layout protocol,
        // independent of any TypeDef registration.
        let tup = Ty::Tuple(vec![Ty::I32, Ty::F64]);
        assert_eq!(
            resolve_vec_method("push", &tup, &type_defs),
            Some("hew_vec_push_layout")
        );
        assert_eq!(
            resolve_vec_method("get", &tup, &type_defs),
            Some("hew_vec_get_layout")
        );
        assert_eq!(
            resolve_vec_method("contains", &tup, &type_defs),
            Some("hew_vec_contains_thunk")
        );
        assert_eq!(
            resolve_vec_method("remove", &tup, &type_defs),
            Some("hew_vec_remove_at_layout")
        );
        assert_eq!(
            resolve_vec_method("clear", &tup, &type_defs),
            Some("hew_vec_clear_layout")
        );
        assert_eq!(
            resolve_vec_method("clone", &tup, &type_defs),
            Some("hew_vec_clone_layout")
        );
        assert_eq!(
            resolve_vec_method("append", &tup, &type_defs),
            Some("hew_vec_append_layout")
        );
        assert_eq!(
            resolve_vec_method("extend", &tup, &type_defs),
            Some("hew_vec_append_layout")
        );

        // Unknown nominal — caller cannot prove handle vs. value shape,
        // so the substrate fails closed (no runtime symbol chosen).
        let unknown = Ty::Named {
            name: "NotRegistered".into(),
            args: vec![],
            builtin: None,
        };
        assert_eq!(resolve_vec_method("push", &unknown, &type_defs), None);
    }

    #[test]
    fn vec_element_typed_methods_fail_closed_for_unresolved_element() {
        use crate::Ty;
        let type_defs = std::collections::HashMap::new();
        // Inference variable: no monomorphic suffix can be chosen — return
        // None so the caller leaves method_call_rewrites absent and HIR
        // fails closed.
        let var = Ty::Var(crate::ty::TypeVar::fresh());
        for method in ["push", "pop", "get", "set", "contains"] {
            assert_eq!(
                resolve_vec_method(method, &var, &type_defs),
                None,
                "Vec::{method} must not resolve when element type is a Var",
            );
        }
        assert_eq!(resolve_vec_method("contains", &Ty::Bool, &type_defs), None);
    }

    #[test]
    fn vec_unsupported_methods_return_none() {
        use crate::Ty;
        let type_defs = std::collections::HashMap::new();
        // map/filter/fold/iter are explicitly out of V0a (closure substrate).
        for method in ["map", "filter", "fold", "iter", "nonexistent"] {
            assert_eq!(resolve_vec_method(method, &Ty::I64, &type_defs), None);
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
