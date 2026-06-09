//! Contract test for the `CloneNotYetSupported` diagnostic path.
//!
//! M-COW P0 introduces a fail-closed guard for `.clone()` method calls:
//! rather than silently returning an aliased handle, the HIR lowerer must:
//!
//!   1. Emit `HirDiagnosticKind::CloneNotYetSupported { receiver_ty }` with
//!      the receiver's resolved type in the diagnostic note.
//!   2. `into_result()` must return `Err` so the build never succeeds when
//!      `.clone()` is present in source.
//!
//! The runtime deep-copy path (e.g. `hew_bytes_clone_ref` for `Bytes`) is
//! wired in P2.  Until then every `.clone()` is a hard compile error.
//!
//! This test pins the fail-closed contract across the parser → checker →
//! HIR pipeline so a future refactor cannot accidentally re-introduce
//! silent aliasing.

use hew_hir::{lower_program, HirDiagnosticKind, ResolutionCtx};
use hew_types::{module_registry::ModuleRegistry, Checker};

fn run_pipeline(source: &str) -> (hew_types::TypeCheckOutput, hew_hir::LowerOutput) {
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
    (tc_output, lower_output)
}

/// `.clone()` on an `i32` value must emit `CloneNotYetSupported` and cause
/// the build to fail.  We use a primitive type (`i32`) to avoid needing
/// stdlib fixtures — the intercept fires before any side-table lookup, so
/// the type does not matter.
#[test]
fn clone_call_emits_not_yet_supported_diagnostic() {
    let source = r"
        fn main() -> i64 {
            let x: i32 = 42;
            let y = x.clone();
            return 0;
        }
    ";

    let (_, lower) = run_pipeline(source);

    let clone_diag = lower
        .diagnostics
        .iter()
        .find(|d| matches!(&d.kind, HirDiagnosticKind::CloneNotYetSupported { .. }));
    assert!(
        clone_diag.is_some(),
        "expected CloneNotYetSupported diagnostic for `.clone()` call; \
         got diagnostics: {:#?}",
        lower.diagnostics
    );

    // `into_result()` must propagate Err — the build must not succeed.
    assert!(
        lower.into_result().is_err(),
        "pipeline must fail when source contains `.clone()`"
    );
}

/// `.clone()` with one or more arguments must NOT be intercepted — only the
/// zero-argument form is the clone method.  This guards against accidentally
/// blocking methods named `clone` that happen to take parameters.
#[test]
fn clone_with_args_is_not_intercepted() {
    // `clone(n)` is a valid method name unrelated to copying; must not be
    // caught by the `.clone()` zero-args guard.  Parser accepts this freely,
    // and if no side-table entry exists it lands in `MethodCallNoRewrite`
    // (a separate diagnostic), NOT in `CloneNotYetSupported`.
    let source = r"
        fn main() -> i64 {
            let x: i32 = 42;
            let y = x.clone(1);
            return 0;
        }
    ";

    let (_, lower) = run_pipeline(source);

    // Must NOT have CloneNotYetSupported — that's only for zero-arg .clone().
    let clone_not_supported = lower
        .diagnostics
        .iter()
        .any(|d| matches!(&d.kind, HirDiagnosticKind::CloneNotYetSupported { .. }));
    assert!(
        !clone_not_supported,
        "CloneNotYetSupported must not fire for `.clone(n)` (non-zero args)"
    );
}
