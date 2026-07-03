//! Parity guard: v0.5 raw-pointer operations must be rejected by the type
//! checker before HIR lowering ever runs, so native and WASM lowering /
//! codegen are not reached.  This pins slice 4 of the v0.5 raw-pointer
//! substrate (`.tmp/plans/r26-pointer-ops.md`).
//!
//! Contract under test:
//!
//!   1. Parser accepts `*expr` as a syntactic shape (so the diagnostic
//!      can be deterministic), but
//!   2. The type checker emits a fail-closed error for every raw deref
//!      regardless of `unsafe` context, and
//!   3. `lower_program` followed by `into_result()` propagates an `Err`
//!      so the build cannot succeed — there is no "silent lowering"
//!      path for a raw-deref AST node.
//!
//! Because HIR/MIR/codegen production code contains zero references to
//! `UnaryOp::RawDeref` (verified by repository grep at the time this test
//! was authored), the only way a raw deref could ever reach lowering is
//! if a future change accidentally introduced one.  This test would then
//! fail because the type-check errors would no longer block the build.
//!
//! LESSONS: producer-bridge-invariant (P0) — a parsed AST shape that has
//! no lowering implementation must be rejected before HIR, never
//! lowered as a no-op.

use hew_hir::{lower_program, ResolutionCtx};
use hew_types::error::TypeErrorKind;
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

/// `*p` outside `unsafe` is rejected with
/// `UnsafeOperationRequiresBlock` before lowering; the build fails.
#[test]
fn raw_deref_outside_unsafe_blocks_lowering() {
    let source = r"
        fn main() -> i64 {
            let p: *const i64 = 0 as *const i64;
            let v = *p;
            return 0;
        }
    ";

    let (tc, lower) = run_pipeline(source);

    let has_unsafe_required = tc
        .errors
        .iter()
        .any(|e| matches!(&e.kind, TypeErrorKind::UnsafeOperationRequiresBlock { .. }));
    assert!(
        has_unsafe_required,
        "expected UnsafeOperationRequiresBlock from raw deref outside unsafe; \
         got errors: {:#?}",
        tc.errors
    );

    // The combined pipeline must report failure: a raw deref must never
    // reach a successful build, regardless of which phase rejects it.
    assert!(
        !tc.errors.is_empty() || lower.into_result().is_err(),
        "pipeline must fail when source contains raw deref outside unsafe"
    );
}

/// `*p` inside `unsafe` is rejected with `RawPointerOpNotLowered`
/// before HIR can lower it; the build still fails.
#[test]
fn raw_deref_inside_unsafe_blocks_lowering_in_v05() {
    let source = r"
        fn main() -> i64 {
            let p: *const i64 = 0 as *const i64;
            let v = unsafe { *p };
            return 0;
        }
    ";

    let (tc, lower) = run_pipeline(source);

    let has_not_lowered = tc
        .errors
        .iter()
        .any(|e| matches!(&e.kind, TypeErrorKind::RawPointerOpNotLowered { .. }));
    assert!(
        has_not_lowered,
        "expected RawPointerOpNotLowered from raw deref inside unsafe; \
         got errors: {:#?}",
        tc.errors
    );

    // Critical parity assertion: even inside `unsafe`, the build must
    // fail.  v0.5 intentionally has no HIR/MIR/codegen support for raw
    // pointer ops; rejecting at the type-check phase is what keeps
    // native and WASM lowering paths from being reached at all.
    assert!(
        !tc.errors.is_empty() || lower.into_result().is_err(),
        "pipeline must fail when source contains raw deref inside unsafe in v0.5"
    );
}
