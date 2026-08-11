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
use hew_types::{CallTarget, Checker, DefId, TypeCheckOutput};

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
            callee, args, dest, ..
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

/// A checked direct call is linked only through the HIR-built DefId-to-symbol
/// projection.  This is the positive half of the fail-closed contract: the
/// selected declaration reaches the emitted symbol without re-reading the
/// callee spelling.
#[test]
fn user_call_target_projects_to_its_emitted_symbol() {
    let src = r"
        fn helper() -> i64 { 42 }
        fn main() -> i64 { helper() }
    ";
    let parsed = hew_parser::parse(src);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc = checker.check_program(&parsed.program);
    let output = lower_program(
        &parsed.program,
        &tc,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );

    let main = output
        .module
        .items
        .iter()
        .find_map(|item| match item {
            hew_hir::HirItem::Function(function) if function.name == "main" => Some(function),
            _ => None,
        })
        .expect("main must be lowered");
    let Some(tail) = &main.body.tail else {
        panic!("main must retain the direct call as its tail expression");
    };
    let hew_hir::HirExprKind::Call {
        target: CallTarget::User(declaration),
        ..
    } = &tail.kind
    else {
        panic!("main call must retain a checker-selected User target: {tail:#?}");
    };
    assert_eq!(declaration.full_path(), "helper");

    let pipeline = lower_hir_module(&output.module);
    assert!(
        pipeline.diagnostics.iter().all(|diagnostic| !matches!(
            diagnostic.kind,
            MirDiagnosticKind::NotYetImplemented { .. }
        )),
        "a mapped User target must lower without a missing-projection diagnostic: {:#?}",
        pipeline.diagnostics
    );
    let main = pipeline
        .raw_mir
        .iter()
        .find(|function| function.name == "main")
        .expect("main must reach MIR");
    assert!(
        main.blocks.iter().any(|block| matches!(
            &block.terminator,
            Terminator::Call { callee, .. } if callee == "helper"
        )),
        "the User target must lower through its exact emitted symbol: {:#?}",
        main.blocks
    );
}

/// A `DefId` not present in HIR's direct-call symbol index is a producer-boundary
/// failure.  MIR must diagnose it and stop rather than deriving a linker label
/// from the `DefId` path.
#[test]
fn user_call_target_without_hir_symbol_map_fails_closed() {
    let src = r"
        fn helper() -> i64 { 42 }
        fn main() -> i64 { helper() }
    ";
    let parsed = hew_parser::parse(src);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc = checker.check_program(&parsed.program);
    let mut output = lower_program(
        &parsed.program,
        &tc,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );

    let main = output
        .module
        .items
        .iter_mut()
        .find_map(|item| match item {
            hew_hir::HirItem::Function(function) if function.name == "main" => Some(function),
            _ => None,
        })
        .expect("main must be lowered");
    let tail = main
        .body
        .tail
        .as_mut()
        .expect("main must retain the direct call as its tail expression");
    let hew_hir::HirExprKind::Call { target, .. } = &mut tail.kind else {
        panic!("main tail must be an ordinary direct call: {tail:#?}");
    };
    *target = CallTarget::User(DefId::new("missing.owner.helper"));

    let pipeline = lower_hir_module(&output.module);
    assert!(
        pipeline.diagnostics.iter().any(|diagnostic| matches!(
            &diagnostic.kind,
            MirDiagnosticKind::NotYetImplemented { construct, .. }
                if construct == "direct declaration `missing.owner.helper` without an HIR symbol map"
        )),
        "missing User DefId projection must fail closed: {:#?}",
        pipeline.diagnostics
    );
    let main = pipeline
        .raw_mir
        .iter()
        .find(|function| function.name == "main")
        .expect("main must reach MIR for diagnostic recovery");
    assert!(
        main.blocks
            .iter()
            .all(|block| !matches!(block.terminator, Terminator::Call { .. })),
        "missing User DefId projection must not emit a reconstructed call: {:#?}",
        main.blocks
    );
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
/// nor a declared module function) must produce `UnsupportedNode`, not
/// `Terminator::Call`. Guards the fail-closed checker boundary.
///
/// The test uses a bare identifier `unknown_fn(42)` that is not declared in
/// the module — the HIR bridge emits `BindingRef { resolved: Unresolved }`.
/// With no checker target, HIR records the unsupported node and MIR must
/// preserve that hard diagnostic.
#[test]
fn unresolved_call_emits_unsupported_node_not_call_terminator() {
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

    let has_unsupported = pipeline.diagnostics.iter().any(|diagnostic| {
        matches!(
            &diagnostic.kind,
            MirDiagnosticKind::UnsupportedNode { reason, .. }
                if reason == "ordinary call has no checker target"
        )
    });
    assert!(
        has_unsupported,
        "unresolved call must preserve the missing-checker-target diagnostic: {:#?}",
        pipeline.diagnostics
    );

    // Must not produce a call terminator in `main` — the fail-closed path must
    // fire for the unresolved callee. Scope the check to user-declared functions
    // (here just `main`) so stdlib-synthesized fmt shims (e.g. `i64::fmt`) that
    // legitimately use `Terminator::Call` do not trigger a false positive.
    let main_fn = pipeline
        .raw_mir
        .iter()
        .find(|f| f.name == "main")
        .expect("main function must be in raw_mir");
    let has_call = main_fn
        .blocks
        .iter()
        .any(|b| matches!(b.terminator, Terminator::Call { .. }));
    assert!(
        !has_call,
        "unresolved call must not emit a call terminator in main; \
         fail-closed path must fire; got blocks: {:#?}",
        main_fn.blocks
    );
}
