//! Contract tests for the `.clone()` lowering paths and the residual
//! `CloneNotYetSupported` backstop.
//!
//! `.clone()` is now wired through the checker → HIR → MIR → codegen
//! pipeline for the types the checker admits:
//!
//!   - Copy/scalar receivers (`i32`, `bool`, `char`, …) emit a redundancy
//!     warning and lower as a plain read via `MethodCallRewrite::CopyCloneNoop`
//!     — the build SUCCEEDS (exit 0).
//!   - User record/struct receivers lower via
//!     `MethodCallRewrite::RecordCloneInplace`.
//!   - Builtin collections + `string`/`Bytes` have their own runtime copy paths.
//!
//! The HIR `CloneNotYetSupported` backstop remains the fail-closed guard for
//! any `.clone()` the checker admits but supplies no rewrite for — it must
//! never silently return an aliased handle. These tests pin both the new
//! Copy-clone contract and the backstop's continued existence.

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

/// `.clone()` on a Copy scalar (`i32`) now lowers as a plain read via
/// `CopyCloneNoop`: the checker emits a redundancy warning, HIR produces NO
/// `CloneNotYetSupported` diagnostic, and the build succeeds.  This pins the
/// Copy-clone contract — a redundant clone is a style nit, not a hard error.
#[test]
fn clone_call_on_copy_type_lowers_without_error() {
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
        clone_diag.is_none(),
        "Copy-type `.clone()` must NOT emit CloneNotYetSupported; \
         got diagnostics: {:#?}",
        lower.diagnostics
    );

    // The build must succeed: a redundant Copy clone is a warning, not an error.
    assert!(
        lower.into_result().is_ok(),
        "pipeline must succeed for a Copy-type `.clone()`"
    );
}

/// The `clone <expr>` prefix lowers through the *same* `lower_method_call`
/// path as `x.clone()`.  On a Copy scalar it behaves identically: no
/// `CloneNotYetSupported`, build succeeds.  This pins the symmetry between the
/// prefix surface and the method form at the HIR boundary.
#[test]
fn clone_prefix_on_copy_type_lowers_without_error() {
    let source = r"
        fn main() -> i64 {
            let x: i32 = 42;
            let y = clone x;
            return 0;
        }
    ";

    let (_, lower) = run_pipeline(source);

    let clone_diag = lower
        .diagnostics
        .iter()
        .find(|d| matches!(&d.kind, HirDiagnosticKind::CloneNotYetSupported { .. }));
    assert!(
        clone_diag.is_none(),
        "Copy-type `clone x` prefix must NOT emit CloneNotYetSupported; \
         got diagnostics: {:#?}",
        lower.diagnostics
    );

    assert!(
        lower.into_result().is_ok(),
        "pipeline must succeed for a Copy-type `clone x` prefix"
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

/// The HIR `CloneNotYetSupported` backstop must still fire fail-closed when a
/// zero-arg `.clone()` reaches HIR with NO rewrite, resolved-call, dyn, or
/// numeric entry for its span.  The checker now provides a rewrite for every
/// clone it admits (Copy → `CopyCloneNoop`, record → `RecordCloneInplace`,
/// collections/string → their own paths), so this state is unreachable through
/// the normal checker flow.  We simulate a checker that admits the call but
/// fails to supply a rewrite by clearing the rewrite table before lowering —
/// the backstop must catch it rather than silently emit an aliasing copy.
#[test]
fn backstop_fires_when_clone_reaches_hir_without_rewrite() {
    let source = r"
        fn main() -> i64 {
            let x: i32 = 42;
            let y = x.clone();
            return 0;
        }
    ";

    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let mut tc_output = checker.check_program(&parsed.program);

    // Drop every side-table entry the clone intercept could have produced,
    // forcing the HIR lowerer onto its fail-closed backstop path.
    tc_output.method_call_rewrites.clear();
    tc_output.numeric_method_lowerings.clear();
    tc_output.dyn_trait_method_calls.clear();
    tc_output.resolved_calls.clear();

    let lower = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );

    let clone_diag = lower
        .diagnostics
        .iter()
        .find(|d| matches!(&d.kind, HirDiagnosticKind::CloneNotYetSupported { .. }));
    assert!(
        clone_diag.is_some(),
        "backstop must emit CloneNotYetSupported when no rewrite exists; \
         got diagnostics: {:#?}",
        lower.diagnostics
    );
    assert!(
        lower.into_result().is_err(),
        "pipeline must fail closed when a `.clone()` has no lowering"
    );
}
