//! Declarative `#[extern_symbol]` templates for the `Vec<T>` stdlib
//! surface — W3.001 Stage 3 substrate.
//!
//! This module is the **attribute-side counterpart** of the legacy
//! magic table in [`crate::stdlib::resolve_vec_method`]. Each entry
//! mirrors the `#[extern_symbol(...)]` annotation that the stdlib
//! `impl<T> Vec<T>` block will carry once the type-checker side of
//! the inherent-impl-on-builtin path is verified (Stage 5; tracked
//! per A259 deviation note in the Stage 3 closeout).
//!
//! Until that wiring lands, this table is the canonical declarative
//! source the differential test pins against: `expand_vec_method`
//! consumes it and the legacy `resolve_vec_method` is the magic-table
//! oracle.
//!
//! ## Why a static table here, not yet in `std/builtins.hew`
//!
//! `impl<T> Vec<T> { ... }` on a builtin type touches type-checker
//! registration paths that are out of scope for the Stage 3 deliverable
//! — the differential test only needs the resolution function to be
//! testable in isolation. Stage 5 adds the stdlib block and reroutes
//! `check_vec_method` to consult `FnSig.extern_symbol` instead of this
//! static table; this module is deleted at that point.
//!
//! Per Invariant #10 (`string-identifier-fragility`): the table is
//! keyed by method name (a string) because that is the surface the
//! checker has at the dispatch site; the *target* (template) is
//! consumed structurally via [`ExternSymbolTemplate`], not by string
//! manipulation downstream.

use crate::extern_symbol::{ExternSymbolTemplate, TemplateExpansionError};
use crate::ty::Ty;
use std::collections::HashMap;

use crate::check::TypeDef;

/// Canonical `#[extern_symbol(...)]` template for each `Vec<T>` method
/// in W3.001 Stage 3 scope.
///
/// Returns `None` for any method outside the W3.001 substrate — those
/// are the open-coded `check_vec_method` arms (`map`/`filter`/etc.)
/// that depend on closure substrate landings (W3.003 / V0b).
///
/// **Source-of-truth invariant**: every entry here must match the
/// corresponding stdlib `#[extern_symbol]` annotation when Stage 5
/// adds the `impl<T> Vec<T>` block. The Stage 5 work includes an
/// equality assertion test pinning this table against the parsed
/// stdlib `FnSig` annotations.
///
/// Reconciliation note (Stage 3 prologue, plan §6 §"len" row):
/// `len` currently routes through `record_runtime_method_call_rewrite(span, "len_vec")`
/// in [`crate::check::Checker::check_vec_method`] — the literal `"len_vec"`
/// then resolves via the HIR catalog overload at
/// `hew-hir/src/stdlib_catalog.rs:354` to the runtime symbol
/// `hew_vec_len`. The attribute path declares the runtime symbol
/// directly; per plan §6 Stage 3 the legacy literal becomes
/// `"hew_vec_len"` in Stage 4 alongside the magic-table deletion (the
/// `len_vec` overload entry continues to back free-function
/// `len(v)` callers). The differential test exempts the `len` row
/// with a documented aliased-pair assertion so Stage 3 lands without
/// touching HIR plumbing.
#[must_use]
pub fn vec_method_template(method: &str) -> Option<&'static str> {
    match method {
        // Monomorphic — single runtime entry point per method.
        "len" => Some("hew_vec_len"),
        "is_empty" => Some("hew_vec_is_empty"),
        "clear" => Some("hew_vec_clear"),
        "clone" => Some("hew_vec_clone"),
        "append" | "extend" => Some("hew_vec_append"),
        // `Vec::remove(i64)` removes by index — runtime symbol is
        // monomorphic (`hew_vec_remove_at`); mirrors the legacy
        // `resolve_vec_method` "remove" arm.
        "remove" => Some("hew_vec_remove_at"),
        // Element-typed — `{T}` expands to the canonical token of the
        // calling-convention class for the element type.
        "push" => Some("hew_vec_push_{T}"),
        "pop" => Some("hew_vec_pop_{T}"),
        "get" => Some("hew_vec_get_{T}"),
        "set" => Some("hew_vec_set_{T}"),
        "contains" => Some("hew_vec_contains_{T}"),
        // `Vec::join` is `Vec<string>`-only by typecheck. The template
        // is monomorphic (no `{T}` placeholder); the diff test pins
        // expansion to `"hew_vec_join_str"` and the typecheck-side
        // rejection of other element types is preserved by
        // `check_vec_method` (plan §6 Stage 5 negative table).
        "join" => Some("hew_vec_join_str"),
        _ => None,
    }
}

/// Expand the attribute-driven template for a `Vec<T>` method.
///
/// Two-step lookup:
/// 1. [`vec_method_template`] returns the canonical template string
///    (or `None` if the method is not in W3.001 substrate scope).
/// 2. [`ExternSymbolTemplate::parse`] structures the template, then
///    [`ExternSymbolTemplate::expand`] substitutes `{T}` using
///    [`crate::RuntimeCallingConvention::for_ty_with_layout`].
///
/// Per Invariant #2 (fail-closed codegen): returns `None` only when
/// the method has no W3.001 template (legitimate "this is open-coded
/// elsewhere" signal); any expansion failure surfaces as
/// `Some(Err(_))` so the caller can route the precise diagnostic.
///
/// ## Per-(method, convention) runtime-existence exclusions
///
/// Some `(method, calling-convention)` pairs have no runtime symbol
/// today even though template expansion would produce a well-formed
/// name. The legacy magic table encodes these as `None`-returning
/// branches; this helper mirrors them via the
/// `runtime_symbol_exists` check so the differential paths agree
/// byte-for-byte. Source-of-truth for the exclusions is
/// [`hew-runtime/src/vec.rs`]; the matrix is documented at
/// `hew-types/src/stdlib.rs:137` (no `hew_vec_contains_ptr`).
///
/// # Returns
///
/// * `None` — `method` is not in the W3.001 Vec substrate (caller
///   should fall through to the legacy magic table), OR the
///   `(method, calling-convention)` pair has no runtime symbol today
///   (allowlist exclusion mirrored from the legacy table).
/// * `Some(Ok(symbol))` — fully-expanded runtime symbol.
/// * `Some(Err(e))` — template parsed but cannot expand for this
///   `elem_ty` (W3.001 fail-closed; Stage 5 negative-test target).
///
/// # Panics
///
/// Panics if one of this module's static templates fails to parse. That
/// indicates an internal bug in the checked-in table, not user input.
#[must_use]
#[allow(
    clippy::implicit_hasher,
    reason = "uses the checker's concrete TypeDef table shape"
)]
pub fn expand_vec_method(
    method: &str,
    elem_ty: &Ty,
    type_defs: &HashMap<String, TypeDef>,
) -> Option<Result<String, TemplateExpansionError>> {
    let template_raw = vec_method_template(method)?;
    // Static templates round-trip through the parser — parse failure
    // here is a compile-time bug in this module.
    let template = ExternSymbolTemplate::parse(template_raw).unwrap_or_else(|e| {
        panic!("internal: static Vec template `{template_raw}` failed to parse: {e:?}")
    });
    let expanded = template.expand(elem_ty, type_defs);
    if let Ok(ref sym) = expanded {
        if !runtime_symbol_exists(method, sym) {
            // Allowlist exclusion mirrored from the legacy magic
            // table: the runtime has no entry point for this
            // `(method, calling-convention)` pair today. Returning
            // `None` matches the legacy `resolve_vec_method` behaviour
            // exactly — the differential test pins it.
            return None;
        }
    }
    Some(expanded)
}

/// Mirror the legacy magic-table `(method, suffix) → Option` rejections
/// for `(method, calling-convention)` pairs the runtime does not back.
///
/// Today the only excluded pair is `(contains, Pointer)` —
/// `hew_vec_contains_ptr` is absent from `hew-runtime/src/vec.rs`
/// (see `hew-types/src/stdlib.rs:137` for the legacy site). When
/// W3.003 ships layout-driven element handling this list shrinks.
fn runtime_symbol_exists(method: &str, expanded: &str) -> bool {
    !matches!(method, "contains" if expanded.ends_with("_ptr"))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::check::TypeDefKind;

    fn vec_handle_type_defs() -> HashMap<String, TypeDef> {
        let mut m = HashMap::new();
        m.insert(
            "Vec".to_string(),
            TypeDef {
                kind: TypeDefKind::Struct,
                name: "Vec".to_string(),
                type_params: vec!["T".to_string()],
                fields: HashMap::new(),
                variants: HashMap::new(),
                methods: HashMap::new(),
                doc_comment: None,
                is_indirect: true,
            },
        );
        m
    }

    #[test]
    fn monomorphic_methods_return_template_verbatim() {
        for (method, expected) in [
            ("len", "hew_vec_len"),
            ("is_empty", "hew_vec_is_empty"),
            ("clear", "hew_vec_clear"),
            ("clone", "hew_vec_clone"),
            ("append", "hew_vec_append"),
            ("extend", "hew_vec_append"),
            ("remove", "hew_vec_remove_at"),
            ("join", "hew_vec_join_str"),
        ] {
            let out = expand_vec_method(method, &Ty::I32, &HashMap::new())
                .unwrap_or_else(|| panic!("template missing for `{method}`"))
                .unwrap_or_else(|e| panic!("expansion failed for `{method}`: {e:?}"));
            assert_eq!(out, expected, "method `{method}`");
        }
    }

    #[test]
    fn element_typed_methods_expand_per_calling_convention() {
        let cases = [
            ("push", Ty::I32, "hew_vec_push_i32"),
            ("push", Ty::I64, "hew_vec_push_i64"),
            ("push", Ty::F64, "hew_vec_push_f64"),
            ("push", Ty::String, "hew_vec_push_str"),
            ("pop", Ty::I32, "hew_vec_pop_i32"),
            ("get", Ty::I64, "hew_vec_get_i64"),
            ("set", Ty::F64, "hew_vec_set_f64"),
            ("contains", Ty::String, "hew_vec_contains_str"),
        ];
        for (method, ty, expected) in cases {
            let out = expand_vec_method(method, &ty, &HashMap::new())
                .unwrap()
                .unwrap();
            assert_eq!(out, expected, "({method}, {ty:?})");
        }
    }

    #[test]
    fn nested_vec_element_routes_to_ptr_token_via_typedef() {
        let type_defs = vec_handle_type_defs();
        let nested = Ty::Named {
            name: "Vec".to_string(),
            args: vec![Ty::I32],
            builtin: None,
        };
        let out = expand_vec_method("push", &nested, &type_defs)
            .unwrap()
            .unwrap();
        assert_eq!(out, "hew_vec_push_ptr");
    }

    #[test]
    fn unknown_method_returns_none() {
        assert!(expand_vec_method("map", &Ty::I32, &HashMap::new()).is_none());
        assert!(expand_vec_method("filter", &Ty::I32, &HashMap::new()).is_none());
        assert!(expand_vec_method("nonexistent", &Ty::I32, &HashMap::new()).is_none());
    }
}
