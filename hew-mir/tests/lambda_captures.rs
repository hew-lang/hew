//! End-to-end tests for the M2 substrate's lambda-actor capture
//! discovery. Exercises the source → HIR → MIR pipeline so the
//! HIR resolver's forward-bind discipline + the MIR producer's
//! `Place::LambdaActorHandle` allocation + the structural capture
//! checker all run together, not in isolation.
//!
//! The synthetic structural tests in
//! `hew-mir::lower::slice3_invariants` continue to exercise the
//! structural checker against hand-built `ElaboratedMirFunction`
//! inputs; these tests pin the producer-side discovery so a future
//! regression on either the resolver or the producer surfaces here.

use hew_hir::{lower_program, verify_hir, ResolutionCtx};
use hew_mir::{lower_hir_module, CaptureKind, ElaboratedMirFunction, IrPipeline, Place};
use hew_types::TypeCheckOutput;

/// Run the full source → HIR → MIR pipeline. Asserts parser cleanliness
/// and `verify_hir` cleanliness; tolerates `NotYetImplemented` HIR
/// diagnostics because the spine subset does not lower every actor-body
/// shape yet (calls inside actor bodies surface a cutover diagnostic
/// from `hew-hir` slice-2 lowering). The lambda-capture ledger is
/// produced by the MIR producer BEFORE codegen short-circuits on
/// cutover findings, so the capture set is still observable on the
/// elaborated function.
fn pipeline(source: &str) -> IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let output = lower_program(&parsed.program, &TypeCheckOutput::default(), &ResolutionCtx);
    // Allow NotYetImplemented diagnostics — the actor-body call
    // surface is deferred work in the spine. Reject everything else.
    let non_nyi: Vec<_> = output
        .diagnostics
        .iter()
        .filter(|d| !matches!(d.kind, hew_hir::HirDiagnosticKind::NotYetImplemented { .. }))
        .collect();
    assert!(
        non_nyi.is_empty(),
        "unexpected hir diagnostics: {non_nyi:?}"
    );
    let verify = verify_hir(&output.module);
    // Same tolerance: a NotYetImplemented HIR node may surface a
    // verifier diagnostic (defense-in-depth from `verify.rs`); the
    // capture ledger is independent of that finding.
    let non_nyi_verify: Vec<_> = verify
        .iter()
        .filter(|d| !matches!(d.kind, hew_hir::HirDiagnosticKind::NotYetImplemented { .. }))
        .collect();
    assert!(
        non_nyi_verify.is_empty(),
        "unexpected verify diagnostics: {non_nyi_verify:?}"
    );
    lower_hir_module(&output.module)
}

fn find_fn<'a>(p: &'a IrPipeline, name: &str) -> &'a ElaboratedMirFunction {
    p.elaborated_mir
        .iter()
        .find(|f| f.name == name)
        .unwrap_or_else(|| panic!("function `{name}` not in pipeline"))
}

#[test]
fn recursive_actor_lambda_records_exactly_one_weak_capture() {
    // The forward-bind discipline: `let fib = actor |n| { fib(...) }`
    // pre-binds the let-name BEFORE lowering the body, so the body's
    // recursive `fib(n - 1)` reference resolves to a Binding pointing
    // at the let. The capture-strength classifier then sees the
    // self-reference and emits exactly one Weak capture.
    // The body's self-reference is what the capture walker must
    // see. A trailing arithmetic that consumes `fib` would be a
    // type error (the body's return type is i64 but `fib` is a
    // Duplex<...>), so the source uses a discarded statement-
    // expression form: `fib;` lowers to an Expr statement whose
    // value is the BindingRef. The actor body returns i64 via the
    // trailing `n + 1`.
    let source = r"
fn make() {
    let fib = actor |n: i64| -> i64 {
        fib;
        n + 1
    };
}
";
    let p = pipeline(source);
    // Producer-driven path is fail-closed at the cutover seam (a
    // `NotYetImplemented` diagnostic surfaces because call-expression
    // lowering inside the actor body is not yet wired). The
    // lambda_captures ledger is still populated by the producer
    // BEFORE diagnostics short-circuit codegen — assert it directly
    // off the elaborated function.
    let func = find_fn(&p, "make");
    let weak_captures: Vec<&str> = func
        .lambda_captures
        .iter()
        .filter(|c| c.capture_kind == CaptureKind::Weak)
        .map(|c| c.name.as_str())
        .collect();
    assert_eq!(
        weak_captures,
        vec!["fib"],
        "recursive actor-lambda must have exactly one Weak self-capture (§5.9 ratification 2); \
         got captures = {:?}",
        func.lambda_captures
    );
    // The capture must attach to a LambdaActorHandle place (the
    // structural checker enforces this — pin the producer's choice).
    let weak = func
        .lambda_captures
        .iter()
        .find(|c| c.capture_kind == CaptureKind::Weak)
        .expect("Weak capture present");
    assert!(
        matches!(weak.actor_handle, Place::LambdaActorHandle(_)),
        "Weak capture must attach to LambdaActorHandle; got {:?}",
        weak.actor_handle
    );
}

#[test]
fn non_recursive_actor_lambda_records_zero_weak_captures() {
    // A lambda whose body does not reference its own let-name has no
    // self-capture. The producer emits zero Weak entries; any Strong
    // captures (none here, since the body closes over nothing) would
    // attach to the LambdaActorHandle place.
    let source = r"
fn make() {
    let f = actor |n: i64| -> i64 { n + 1 };
}
";
    let p = pipeline(source);
    let func = find_fn(&p, "make");
    let weak_count = func
        .lambda_captures
        .iter()
        .filter(|c| c.capture_kind == CaptureKind::Weak)
        .count();
    assert_eq!(
        weak_count, 0,
        "non-recursive actor-lambda must have zero Weak captures; got {:?}",
        func.lambda_captures
    );
}
