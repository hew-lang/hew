//! Regression + negative pair for the assignment-target `UnresolvedPlace`
//! cascade guard (dogfood WO6). The `assign` else-arm at
//! `hew-mir/src/lower/expr.rs:2046` pushes `UnresolvedPlace` for a
//! binding with no `binding_locals` slot and no `capture_env_sources`
//! entry. The guard consults `poisoned_let_bindings` before that push —
//! mirroring the read-arm guard at `:2484` — so a binding whose
//! initializer already reported its own root error does not also stack
//! this diagnostic.
//!
//! Both tests drive `Builder::assign` directly on synthetic HIR: real
//! source text cannot reach a non-poisoned `UnresolvedPlace` on the
//! assignment path because `lower_params` unconditionally populates
//! `binding_locals` for every parameter, so there is no source-level
//! reproducer for the negative case.
//!
//! Declared as `mod poisoned_assign_target_cascade;` (default file-module
//! path) in `expr.rs` rather than inlined there, so it is a descendant of
//! `crate::lower::expr` (same private-item visibility as an inline `mod`)
//! while keeping `expr.rs` itself under the `src/lower/` line-count
//! ratchet.
use super::*;

fn expr(kind: HirExprKind, ty: ResolvedTy) -> HirExpr {
    HirExpr {
        node: hew_hir::HirNodeId(u32::MAX),
        site: SiteId(u32::MAX),
        ty,
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Read,
        kind,
        span: 0..0,
    }
}

fn binding_ref(name: &str, id: u32, ty: ResolvedTy) -> HirExpr {
    expr(
        HirExprKind::BindingRef {
            name: name.to_string(),
            resolved: ResolvedRef::Binding(BindingId(id)),
        },
        ty,
    )
}

fn i64_literal(value: i64) -> HirExpr {
    expr(
        HirExprKind::Literal(HirLiteral::Integer(value)),
        ResolvedTy::I64,
    )
}

/// Positive: a binding recorded in `poisoned_let_bindings` (its `let`/
/// `var` initializer already failed and reported the root error) emits
/// NO diagnostic when later assigned. This is the cascade this lane
/// suppresses.
#[test]
fn assign_to_poisoned_binding_emits_no_diagnostic() {
    let mut builder = Builder::default();
    builder.poisoned_let_bindings.insert(BindingId(7));
    let target = binding_ref("left", 7, ResolvedTy::I64);
    let value = i64_literal(42);

    builder.assign(&target, &value);

    assert!(
        builder.diagnostics.is_empty(),
        "assigning to a poisoned binding must emit no diagnostic (pure cascade \
         suppression); got {:#?}",
        builder.diagnostics
    );
}

/// Negative, mandatory: a binding with NO `binding_locals` slot that was
/// NEVER poisoned — a genuine unresolved place, e.g. a real compiler bug
/// with no prior error — must still emit `UnresolvedPlace`. Without this
/// test the guard could be widened into a fail-open that swallows a real
/// "emitter would silently load an uninitialised slot" case (the
/// justification for the diagnostic in the first place, per the read-arm
/// comment at `:2489-2493`).
#[test]
fn assign_to_genuinely_unresolved_non_poisoned_binding_still_reports() {
    let mut builder = Builder::default();
    // Deliberately NOT inserted into `poisoned_let_bindings`.
    let target = binding_ref("p", 9, ResolvedTy::I64);
    let value = i64_literal(1);

    builder.assign(&target, &value);

    assert_eq!(
        builder.diagnostics.len(),
        1,
        "a non-poisoned binding with no MIR place must still report exactly one \
         diagnostic; got {:#?}",
        builder.diagnostics
    );
    assert!(
        matches!(
            builder.diagnostics[0].kind,
            MirDiagnosticKind::UnresolvedPlace {
                binding: BindingId(9),
                ..
            }
        ),
        "the diagnostic for a genuinely unresolved assignment target must be \
         UnresolvedPlace; got {:#?}",
        builder.diagnostics[0]
    );
}
