//! Contract tests for `Terminator::Call` — MIR shape produced for direct
//! calls to user-defined functions in the same module.
//!
//! Tests exercise the full pipeline:
//!   parse → typecheck → HIR lower → MIR lower
//! to verify that `HirExprKind::Call` with a callee that resolves to a
//! module function emits `Terminator::Call` with the correct callee symbol
//! and argument Places, and that function parameters resolve to real
//! `Place::Local` slots rather than emitting `UnresolvedPlace` diagnostics.

use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{lower_hir_module, IrPipeline, MirDiagnosticKind, Terminator};
use hew_types::module_registry::ModuleRegistry;
use hew_types::{Checker, TypeCheckOutput};

fn pipeline_with_tc(source: &str) -> IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    let output = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    lower_hir_module(&output.module)
}

/// `add(2, 3)` from `main` must produce `Terminator::Call` with callee
/// `"add"` and two `Place::Local` arguments, with no `UnresolvedPlace` or
/// `NotYetImplemented` diagnostics.
#[test]
fn direct_call_emits_call_terminator_with_correct_callee_and_args() {
    let src = r"
        fn add(a: i64, b: i64) -> i64 {
            a + b
        }
        fn main() -> i64 {
            add(2, 3)
        }
    ";

    let pipeline = pipeline_with_tc(src);

    // No MIR diagnostics that would indicate lowering failure.
    let bad_diags: Vec<_> = pipeline
        .diagnostics
        .iter()
        .filter(|d| {
            matches!(
                d.kind,
                MirDiagnosticKind::NotYetImplemented { .. }
                    | MirDiagnosticKind::UnresolvedPlace { .. }
            )
        })
        .collect();
    assert!(
        bad_diags.is_empty(),
        "unexpected MIR diagnostics: {bad_diags:#?}"
    );

    // Find `main` in raw_mir.
    let main_fn = pipeline
        .raw_mir
        .iter()
        .find(|f| f.name == "main")
        .expect("main function must be in raw_mir");

    // Collect all call terminators across main's blocks.
    let calls: Vec<&Terminator> = main_fn
        .blocks
        .iter()
        .map(|b| &b.terminator)
        .filter(|t| matches!(t, Terminator::Call { .. }))
        .collect();

    assert_eq!(
        calls.len(),
        1,
        "main must contain exactly one call terminator; got: {calls:#?}"
    );

    match calls[0] {
        Terminator::Call {
            callee,
            args,
            dest,
            next: _,
        } => {
            assert_eq!(
                callee, "add",
                "callee symbol must be \"add\", got {callee:?}"
            );
            assert_eq!(
                args.len(),
                2,
                "add() takes 2 args; call terminator has {} args: {args:?}",
                args.len()
            );
            assert!(
                dest.is_some(),
                "add() returns i64; call terminator dest must be Some"
            );
        }
        other => panic!("expected call terminator, got {other:?}"),
    }
}

/// Parameters of the callee (`add`) must resolve to `Place::Local` slots,
/// not emit `UnresolvedPlace` diagnostics. Verifies `lower_params` wires
/// each `HirBinding` param into `binding_locals`.
#[test]
fn callee_params_resolve_to_local_slots_no_unresolved_place() {
    let src = r"
        fn add(a: i64, b: i64) -> i64 {
            a + b
        }
        fn main() -> i64 {
            add(2, 3)
        }
    ";

    let pipeline = pipeline_with_tc(src);

    // Find `add` in raw_mir and check no UnresolvedPlace diagnostics.
    let unresolved: Vec<_> = pipeline
        .diagnostics
        .iter()
        .filter(|d| matches!(d.kind, MirDiagnosticKind::UnresolvedPlace { .. }))
        .collect();
    assert!(
        unresolved.is_empty(),
        "function parameters must resolve to local slots; got UnresolvedPlace: {unresolved:#?}"
    );

    // `add` must have at least 2 params in its raw_mir entry.
    let add_fn = pipeline
        .raw_mir
        .iter()
        .find(|f| f.name == "add")
        .expect("add function must be in raw_mir");

    assert_eq!(
        add_fn.params.len(),
        2,
        "add must have 2 params in RawMirFunction; got {}",
        add_fn.params.len()
    );
}

/// An unresolved call (a callee name that is neither a runtime-ABI symbol
/// nor a declared module function) must produce `NotYetImplemented`, not
/// `Terminator::Call`. Guards the fail-closed boundary in `lower_value`.
///
/// The test uses a bare identifier `unknown_fn(42)` that is not declared in
/// the module — the HIR bridge emits `BindingRef { resolved: Unresolved }`.
/// After the runtime-ABI and module-fn checks both fail, the fallthrough
/// path must emit `NotYetImplemented`.
#[test]
fn unresolved_call_emits_not_yet_implemented_not_call_terminator() {
    // `unknown_fn` is not declared in this module and is not a runtime symbol.
    let src = r"
        fn main() -> i64 {
            unknown_fn(42)
        }
    ";

    // Parse and HIR-lower without a type checker (TypeCheckOutput::default)
    // so that `unknown_fn` stays unresolved.
    let parsed = hew_parser::parse(src);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    let pipeline = hew_mir::lower_hir_module(&output.module);

    // Must produce NotYetImplemented for the unresolved call.
    let has_nyi = pipeline
        .diagnostics
        .iter()
        .any(|d| matches!(d.kind, MirDiagnosticKind::NotYetImplemented { .. }));
    assert!(
        has_nyi,
        "unresolved call must produce NotYetImplemented; got diagnostics: {:#?}",
        pipeline.diagnostics
    );

    // Must not produce a call terminator — the fail-closed path must fire.
    let has_call = pipeline
        .raw_mir
        .iter()
        .flat_map(|f| f.blocks.iter())
        .any(|b| matches!(b.terminator, Terminator::Call { .. }));
    assert!(
        !has_call,
        "unresolved call must not emit a call terminator; fail-closed path must fire"
    );
}
