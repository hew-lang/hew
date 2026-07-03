//! Verifier-invoking contract test for the user-facing `duplex_pair`
//! constructor.
//!
//! The existing Duplex HIR/MIR tests (`method_call_bridge.rs`,
//! `producer_method_send.rs`) exercise `duplex_pair<S, R>(N)` through
//! `lower_program` but never call `verify_hir`. That is exactly why the
//! constructor-reachability gap was invisible: `duplex_pair` is a checker
//! builtin with no AST `fn` item, so before it was seeded into `fn_registry`
//! it lowered to `ResolvedRef::Unresolved` and `verify_hir` rejected the call
//! with `UnresolvedSymbol` — but no test ran the verifier on it.
//!
//! These tests run `verify_hir` on a real `duplex_pair` program and assert the
//! verifier accepts the constructor call: neither `UnresolvedSymbol` (the
//! Unresolved-callee gate) nor `CallableUnsupportedInMir` (the callable-set
//! gate) fires on `duplex_pair`. This pins the two seams S1 closes.

use hew_hir::{lower_program, verify_hir, HirDiagnosticKind, ResolutionCtx};
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;

/// Lower `source` and return the verifier diagnostics.
fn verify(source: &str) -> Vec<hew_hir::HirDiagnostic> {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    let lower_output = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    verify_hir(&lower_output.module)
}

/// `duplex_pair<i64, i64>(16)` passes `verify_hir`: the constructor is now an
/// Item-resolved, callable-set member, so neither the Unresolved-callee gate
/// nor the callable-set gate rejects it.
#[test]
fn duplex_pair_constructor_passes_hir_verifier() {
    let source = r"
        fn main() -> i64 {
            let (a, b) = duplex_pair<i64, i64>(16);
            a.send(1);
            return 0;
        }
    ";
    let diags = verify(source);

    let constructor_rejections: Vec<_> = diags
        .iter()
        .filter(|d| match &d.kind {
            HirDiagnosticKind::UnresolvedSymbol { name }
            | HirDiagnosticKind::CallableUnsupportedInMir { name } => name == "duplex_pair",
            _ => false,
        })
        .collect();

    assert!(
        constructor_rejections.is_empty(),
        "duplex_pair must pass verify_hir as a resolved, callable builtin; \
         got rejections: {constructor_rejections:#?}"
    );
}
