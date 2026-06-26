#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
pub(super) use super::*;

// ── W3.001 Stage 2 — #[extern_symbol] ingest into FnSig.extern_symbol ─────────

/// `#[extern_symbol("…")]` on an `extern "C"` block fn populates
/// `FnSig.extern_symbol` with a parsed structured template.
#[test]
fn extern_symbol_on_extern_c_fn_populates_fn_sig_spec() {
    let output = check_source(
        r#"
        extern "C" {
            #[extern_symbol("hew_vec_push_{T}")]
            fn vec_push(v: i64, x: i64);
        }
        "#,
    );
    let sig = output
        .fn_sigs
        .get("vec_push")
        .expect("extern fn must be registered");
    let spec = sig
        .extern_symbol
        .as_ref()
        .expect("Stage 2 must populate extern_symbol on the FnSig");
    assert_eq!(spec.template.raw, "hew_vec_push_{T}");
    assert_eq!(
        spec.template.placeholders,
        vec![crate::extern_symbol::PlaceholderName::T]
    );
    assert!(
        output.errors.is_empty(),
        "well-formed template must not emit diagnostics, got: {:#?}",
        output.errors
    );
}

/// Extern fns without `#[extern_symbol]` carry `None` (regression
/// guard: the field must remain opt-in and not default to a synthetic
/// template derived from the fn name).
#[test]
fn extern_fn_without_extern_symbol_attribute_has_none_spec() {
    let output = check_source(
        r#"
        extern "C" {
            fn unrelated(x: i64) -> i64;
        }
        "#,
    );
    let sig = output
        .fn_sigs
        .get("unrelated")
        .expect("extern fn must be registered");
    assert!(
        sig.extern_symbol.is_none(),
        "fn without #[extern_symbol] must not carry a spec"
    );
}

/// `#[extern_symbol("…")]` on an inherent impl method populates the
/// spec on BOTH the `fn_sigs` entry and the `TypeDef.methods` entry,
/// so Stage-3 method-call rewrite (which reads from `td.methods`) sees
/// the same template as Stage-2 ingest.
#[test]
fn extern_symbol_on_impl_method_populates_both_fn_sigs_and_type_def_methods() {
    let output = check_source(
        r#"
        type Holder { x: i64 }

        impl Holder {
            #[extern_symbol("hew_holder_clone")]
            fn cloned(self) -> Holder {
                self
            }
        }
        "#,
    );
    assert!(
        output.errors.is_empty(),
        "well-formed template must not emit diagnostics, got: {:#?}",
        output.errors
    );
    let sig = output
        .fn_sigs
        .get("Holder::cloned")
        .expect("impl method must be in fn_sigs");
    let spec = sig
        .extern_symbol
        .as_ref()
        .expect("Stage 2 must populate extern_symbol on the fn_sigs entry");
    assert_eq!(spec.template.raw, "hew_holder_clone");
    assert!(
        spec.template.is_monomorphic(),
        "no {{T}} placeholder → monomorphic"
    );

    let td = output
        .type_defs
        .get("Holder")
        .expect("Holder type must be registered");
    let method_sig = td
        .methods
        .get("cloned")
        .expect("cloned method must be on TypeDef");
    let method_spec = method_sig
        .extern_symbol
        .as_ref()
        .expect("Stage 2 must mirror the spec onto TypeDef.methods");
    assert_eq!(method_spec.template.raw, "hew_holder_clone");
}

/// A malformed template surfaces as `InvalidExternSymbolTemplate` with
/// the exact deterministic `reason` string from the parser — this is
/// the Stage-2 diagnostic gate referenced by plan §5.5.
#[test]
fn malformed_extern_symbol_template_emits_invalid_template_diagnostic() {
    let output = check_source(
        r#"
        extern "C" {
            #[extern_symbol("hew_vec_{Q}")]
            fn bad(x: i64);
        }
        "#,
    );
    let invalid: Vec<_> = output
        .errors
        .iter()
        .filter_map(|e| match &e.kind {
            TypeErrorKind::InvalidExternSymbolTemplate { reason } => Some(reason.as_str()),
            _ => None,
        })
        .collect();
    assert_eq!(
        invalid.len(),
        1,
        "expected exactly one InvalidExternSymbolTemplate, got errors: {:#?}",
        output.errors
    );
    assert!(
        invalid[0].contains("`{Q}`"),
        "diagnostic reason must name the offending placeholder, got: {:?}",
        invalid[0]
    );

    // Fail-closed: the FnSig must NOT carry a partial / malformed
    // template — Stage 3 should treat this fn as having no rewrite
    // and route through the legacy path (or surface an
    // unresolved-symbol diagnostic later).
    let sig = output
        .fn_sigs
        .get("bad")
        .expect("extern fn must still be registered for downstream resolution");
    assert!(
        sig.extern_symbol.is_none(),
        "rejected template must leave extern_symbol = None (fail-closed)",
    );
}

/// An empty template is rejected with the exact `"empty template"`
/// reason — Stage-5 diagnostic-precision tests pin against this
/// spelling.
#[test]
fn empty_extern_symbol_template_is_rejected_with_empty_reason() {
    let output = check_source(
        r#"
        extern "C" {
            #[extern_symbol("")]
            fn empty(x: i64);
        }
        "#,
    );
    let reason = output
        .errors
        .iter()
        .find_map(|e| match &e.kind {
            TypeErrorKind::InvalidExternSymbolTemplate { reason } => Some(reason.clone()),
            _ => None,
        })
        .expect("expected InvalidExternSymbolTemplate diagnostic");
    assert_eq!(reason, "empty template");
}
