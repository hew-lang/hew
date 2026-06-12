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
    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
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

/// Run the full source → typecheck → HIR → MIR pipeline with the REAL
/// checker (mirrors `actor_send_ask::lower_checked`). The bare-checker
/// `pipeline` helper above cannot type actor spawns or method sends, which
/// the pid-capture shape needs.
fn lower_checked(source: &str) -> IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker =
        hew_types::Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    assert!(tc_output.errors.is_empty(), "{:?}", tc_output.errors);
    let hir = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(hir.diagnostics.is_empty(), "{:?}", hir.diagnostics);
    lower_hir_module(&hir.module)
}

#[test]
fn lambda_pid_capture_is_admitted_as_strong_no_drop() {
    // A lambda actor capturing a declared-actor pid: the pid is a BitCopy
    // alias with no drop glue, so the capture env admits it (no
    // CannotMaterializeClosureCapture) and records exactly one Strong
    // capture. Regression: the drop-class match previously had no pid arm
    // and the capture fell to the fail-closed reject.
    let source = r"
actor Counter {
    var count: i64;

    receive fn increment(n: i64) {
        count = count + n;
    }
}

fn main() {
    let counter = spawn Counter(count: 0);
    let forward = actor |by: i64| { counter.increment(by); };
    forward(1);
}
";
    let p = lower_checked(source);
    let rejects: Vec<_> = p
        .diagnostics
        .iter()
        .filter(|d| {
            matches!(
                d.kind,
                hew_mir::MirDiagnosticKind::CannotMaterializeClosureCapture { .. }
            )
        })
        .collect();
    assert!(
        rejects.is_empty(),
        "pid capture must be admitted (BitCopy alias, no drop glue); got {rejects:?}"
    );
    let func = find_fn(&p, "main");
    let strong_names: Vec<&str> = func
        .lambda_captures
        .iter()
        .filter(|c| c.capture_kind == CaptureKind::Strong)
        .map(|c| c.name.as_str())
        .collect();
    assert_eq!(
        strong_names,
        vec!["counter"],
        "pid capture must be recorded as exactly one Strong capture; got {:?}",
        func.lambda_captures
    );
}

#[test]
fn lambda_vec_capture_still_fails_closed() {
    // The pid admission must not widen the capture surface: an owned
    // aggregate (Vec) capture still refuses with
    // CannotMaterializeClosureCapture — a shallow byte copy would alias
    // its heap and double-free at shutdown.
    let source = r"
fn main() {
    let xs: Vec<i64> = Vec::new();
    let f = actor |n: i64| -> i64 { xs[0] + n };
}
";
    let p = pipeline(source);
    assert!(
        p.diagnostics.iter().any(|d| matches!(
            d.kind,
            hew_mir::MirDiagnosticKind::CannotMaterializeClosureCapture { .. }
        )),
        "Vec capture must stay fail-closed; diagnostics: {:?}",
        p.diagnostics
    );
}

/// Run the full pipeline but tolerate checker errors (used to test the MIR
/// defence-in-depth guard against the checker-rejected shapes).
///
/// Returns the `IrPipeline` even if the checker emitted errors so the MIR
/// diagnostic can be inspected.
fn lower_accepting_checker_errors(source: &str) -> IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker =
        hew_types::Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    // We expect checker errors (the Duplex capture), but HIR and MIR must still
    // run to exercise the defence-in-depth guard. Do NOT assert tc_output.errors.is_empty().
    let hir = hew_hir::lower_program(
        &parsed.program,
        &tc_output,
        &hew_hir::ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    lower_hir_module(&hir.module)
}

#[test]
fn closure_duplex_capture_rejected_at_checker_with_new_error() {
    // The authoritative wall for Duplex captures in fn-closures fires at the
    // checker boundary (`TypeErrorKind::ClosureCapturesDuplexHandle`) — not at
    // HIR's `CheckerBoundaryViolation`. This test confirms the checker emits the
    // new deliberate error for the canonical capture shape so downstream
    // reviewers have an assertion to point to.
    let source = r"
fn main() {
    let log = actor |n: i64| { n; };
    let relay = |n: i64| { log(n); };
    relay(1);
}
";
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker =
        hew_types::Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    assert!(
        tc_output
            .errors
            .iter()
            .any(|e| e.kind.as_kind_str() == "ClosureCapturesDuplexHandle"),
        "checker must emit ClosureCapturesDuplexHandle for fn-closure capturing a Duplex handle; \
         got: {:?}",
        tc_output.errors
    );
}

#[test]
fn closure_duplex_capture_mir_defence_in_depth_fires_on_method_evasion() {
    // The `.send()` evasion: inside a closure body, call `log.send(n)` instead
    // of `log(n)`. The method-call path synthesizes `log` (so the capture fact
    // IS generated), bypassing the `check_call` gate. MIR's
    // `materialize_closure_env` must then emit `ClosureCapturesDuplexHandle`
    // as a defence-in-depth guard.
    let source = r"
fn main() {
    let log = actor |n: i64| { n; };
    let relay = |n: i64| {
        log.send(n);
    };
    relay(1);
}
";
    let p = lower_accepting_checker_errors(source);
    assert!(
        p.diagnostics.iter().any(|d| matches!(
            &d.kind,
            hew_mir::MirDiagnosticKind::ClosureCapturesDuplexHandle { name, .. }
            if name == "log"
        )),
        "method-send evasion must trigger MIR ClosureCapturesDuplexHandle; got: {:?}",
        p.diagnostics
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
