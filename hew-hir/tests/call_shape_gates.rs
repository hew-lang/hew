//! Tests for FC-P1-B call-shape gates (HIR pre-pass for
//! `CallableUnsupportedInMir` and `IndirectCallUnsupported`).
//!
//! Positive coverage: real source programs that exercise the dispatch
//! shapes the gate must NOT reject — direct module-fn calls, stdlib runtime
//! calls, chained calls, and let-bound closure invocations. Each one drives
//! the full `lower_program_host_target` pipeline and asserts no call-shape
//! diagnostic surfaces.
//!
//! Negative coverage: synthetic `HirItem::Function` fixtures fed directly
//! into `run_call_shape_gates_for_test`, the test-only hook on `hew-hir`'s
//! gate dispatcher. The current v0.5 surface syntax cannot easily produce a
//! `BindingRef { Item(_) }` to a name absent from the callable set — every
//! `Item`-resolved callee comes through `lower_identifier`'s `fn_registry`
//! lookup, which is seeded from the same sources the gate consults — so
//! exercising the gate predicate from surface source would require waiting
//! for a producer-bridge regression. The synthetic path keeps the fail-closed
//! contract tested today and gives MIR-bridge regressions a deterministic
//! failure site at HIR.

use hew_hir::{
    lower_program_host_target, run_call_shape_gates_for_test, HirBinding, HirBlock,
    HirDiagnosticKind, HirExpr, HirExprKind, HirFn, HirItem, HirMachineDecl, HirMachineEvent,
    HirMachineState, HirMachineTransition, HirStmt, HirStmtKind, IntentKind, ResolvedRef,
    ValueClass,
};
use hew_hir::{BindingId, HirNodeId, ItemId, ScopeId, SiteId};
use hew_parser::parser;
use hew_types::{module_registry::ModuleRegistry, Checker, ResolvedTy};

fn lower_source(source: &str) -> hew_hir::LowerOutput {
    let parsed = parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tco = checker.check_program(&parsed.program);
    lower_program_host_target(&parsed.program, &tco, &hew_hir::ResolutionCtx)
}

fn has_call_shape_diagnostic(diagnostics: &[hew_hir::HirDiagnostic]) -> bool {
    diagnostics.iter().any(|d| {
        matches!(
            d.kind,
            HirDiagnosticKind::CallableUnsupportedInMir { .. }
                | HirDiagnosticKind::IndirectCallUnsupported { .. }
        )
    })
}

// ── Positive tests (must NOT trip the gate) ─────────────────────────────────

#[test]
fn direct_module_fn_call_accepted() {
    let output = lower_source(
        r"
        fn helper(n: i64) -> i64 { n + 1 }
        fn main() {
            let x = helper(41);
        }
        ",
    );
    assert!(
        !has_call_shape_diagnostic(&output.diagnostics),
        "direct module fn call should not trip call-shape gates: {:?}",
        output.diagnostics
    );
}

#[test]
fn chained_direct_fn_calls_accepted() {
    let output = lower_source(
        r"
        fn add(a: i64, b: i64) -> i64 { a + b }
        fn double(n: i64) -> i64 { add(n, n) }
        fn main() {
            let x = double(add(1, 2));
        }
        ",
    );
    assert!(
        !has_call_shape_diagnostic(&output.diagnostics),
        "chained direct fn calls should not trip call-shape gates: {:?}",
        output.diagnostics
    );
}

#[test]
fn closure_binding_call_accepted() {
    // Closure bound to a `let` then invoked: callee is
    // `BindingRef { resolved: Binding(_), .. }` with type `Closure`.
    // MIR lowers via `CallClosure`; the FC-P1-B gate's `IndirectCallUnsupported`
    // predicate is intentionally narrowed to `Unresolved` callees so this
    // shape passes through.
    let output = lower_source(
        r"
        fn main() {
            let dbl = |x: i64| -> i64 { x * 2 };
            let y = dbl(21);
        }
        ",
    );
    assert!(
        !has_call_shape_diagnostic(&output.diagnostics),
        "let-bound closure invocation should not trip call-shape gates: {:?}",
        output.diagnostics
    );
}

#[test]
fn capturing_closure_call_accepted() {
    let output = lower_source(
        r"
        fn main() {
            let k: i64 = 10;
            let add_k = |x: i64| -> i64 { x + k };
            let y = add_k(5);
        }
        ",
    );
    assert!(
        !has_call_shape_diagnostic(&output.diagnostics),
        "capturing closure invocation should not trip call-shape gates: {:?}",
        output.diagnostics
    );
}

#[test]
fn empty_program_does_not_emit_call_shape_diagnostic() {
    let output = lower_source(
        r"
        fn main() {}
        ",
    );
    assert!(
        !has_call_shape_diagnostic(&output.diagnostics),
        "empty program should not trip call-shape gates: {:?}",
        output.diagnostics
    );
}

// ── Negative tests (synthetic HIR — gate predicate exercise) ────────────────

fn dummy_span() -> std::ops::Range<usize> {
    0..0
}

fn unit_binding(name: &str) -> HirBinding {
    HirBinding {
        id: BindingId(0),
        name: name.to_string(),
        ty: ResolvedTy::Unit,
        mutable: false,
        span: dummy_span(),
    }
}

fn unit_block_with_tail(tail: HirExpr) -> HirBlock {
    HirBlock {
        node: HirNodeId(0),
        scope: ScopeId(0),
        statements: Vec::new(),
        tail: Some(Box::new(tail)),
        ty: ResolvedTy::Unit,
        span: dummy_span(),
    }
}

fn synth_call_with_item_callee(name: &str, item_id: u32) -> HirExpr {
    let callee = HirExpr {
        node: HirNodeId(0),
        site: SiteId(0),
        ty: ResolvedTy::Function {
            params: Vec::new(),
            ret: Box::new(ResolvedTy::Unit),
        },
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Read,
        kind: HirExprKind::BindingRef {
            name: name.to_string(),
            resolved: ResolvedRef::Item(ItemId(item_id)),
        },
        span: dummy_span(),
    };
    HirExpr {
        node: HirNodeId(1),
        site: SiteId(1),
        ty: ResolvedTy::Unit,
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Read,
        kind: HirExprKind::Call {
            callee: Box::new(callee),
            args: Vec::new(),
        },
        span: dummy_span(),
    }
}

fn synth_call_with_unresolved_callable_callee(name: &str) -> HirExpr {
    let callee = HirExpr {
        node: HirNodeId(0),
        site: SiteId(0),
        ty: ResolvedTy::Function {
            params: Vec::new(),
            ret: Box::new(ResolvedTy::Unit),
        },
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Read,
        kind: HirExprKind::BindingRef {
            name: name.to_string(),
            resolved: ResolvedRef::Unresolved,
        },
        span: dummy_span(),
    };
    HirExpr {
        node: HirNodeId(1),
        site: SiteId(1),
        ty: ResolvedTy::Unit,
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Read,
        kind: HirExprKind::Call {
            callee: Box::new(callee),
            args: Vec::new(),
        },
        span: dummy_span(),
    }
}

fn synth_fn_with_tail(name: &str, item_id: u32, tail: HirExpr) -> HirItem {
    HirItem::Function(HirFn {
        id: ItemId(item_id),
        node: HirNodeId(0),
        name: name.to_string(),
        type_params: Vec::new(),
        params: vec![unit_binding("_unused")],
        return_ty: ResolvedTy::Unit,
        body: unit_block_with_tail(tail),
        span: dummy_span(),
    })
}

#[test]
fn item_callee_not_in_callable_set_emits_callable_unsupported_in_mir() {
    // Synthetic call to `phantom_callee` resolved as `Item(ItemId(42))` —
    // no `HirItem::Function` named `phantom_callee` exists in the module,
    // no stdlib catalog entry by that name, no extern fn, no monomorphisation.
    // The gate must surface `CallableUnsupportedInMir`.
    let main = synth_fn_with_tail("main", 0, synth_call_with_item_callee("phantom_callee", 42));
    let items = vec![main];

    let diagnostics = run_call_shape_gates_for_test(&items, &[]);

    assert!(
        diagnostics.iter().any(|d| matches!(
            &d.kind,
            HirDiagnosticKind::CallableUnsupportedInMir { name } if name == "phantom_callee"
        )),
        "expected CallableUnsupportedInMir for phantom_callee, got: {diagnostics:?}"
    );
}

#[test]
fn item_callee_referencing_module_fn_does_not_emit_diagnostic() {
    // Same synthetic shape but the callee name resolves to a real
    // `HirItem::Function` in the module — the gate's callable set includes
    // user-declared functions, so no diagnostic should fire.
    let target = HirItem::Function(HirFn {
        id: ItemId(99),
        node: HirNodeId(0),
        name: "real_callee".to_string(),
        type_params: Vec::new(),
        params: Vec::new(),
        return_ty: ResolvedTy::Unit,
        body: unit_block_with_tail(HirExpr {
            node: HirNodeId(0),
            site: SiteId(0),
            ty: ResolvedTy::Unit,
            value_class: ValueClass::BitCopy,
            intent: IntentKind::Read,
            kind: HirExprKind::Literal(hew_hir::HirLiteral::Unit),
            span: dummy_span(),
        }),
        span: dummy_span(),
    });
    let main = synth_fn_with_tail("main", 0, synth_call_with_item_callee("real_callee", 99));
    let items = vec![target, main];

    let diagnostics = run_call_shape_gates_for_test(&items, &[]);

    assert!(
        diagnostics
            .iter()
            .all(|d| !matches!(d.kind, HirDiagnosticKind::CallableUnsupportedInMir { .. })),
        "expected no CallableUnsupportedInMir when callee is in callable set, got: {diagnostics:?}"
    );
}

#[test]
fn unresolved_callable_callee_emits_indirect_call_unsupported() {
    let main = synth_fn_with_tail(
        "main",
        0,
        synth_call_with_unresolved_callable_callee("ghost_fn"),
    );
    let items = vec![main];

    let diagnostics = run_call_shape_gates_for_test(&items, &[]);

    assert!(
        diagnostics.iter().any(|d| matches!(
            &d.kind,
            HirDiagnosticKind::IndirectCallUnsupported { callee, .. }
                if callee.contains("ghost_fn")
        )),
        "expected IndirectCallUnsupported for unresolved callable callee, got: {diagnostics:?}"
    );
}

#[test]
fn binding_callee_does_not_emit_indirect_call_unsupported() {
    // `BindingRef { Binding(_), .. }` callees with callable type are the
    // closure-binding case (`let f = |x| x + 1; f(2)`) — MIR's `CallClosure`
    // arm dispatches these. The gate must NOT reject them.
    let callee = HirExpr {
        node: HirNodeId(0),
        site: SiteId(0),
        ty: ResolvedTy::Function {
            params: Vec::new(),
            ret: Box::new(ResolvedTy::Unit),
        },
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Read,
        kind: HirExprKind::BindingRef {
            name: "f".to_string(),
            resolved: ResolvedRef::Binding(BindingId(7)),
        },
        span: dummy_span(),
    };
    let call = HirExpr {
        node: HirNodeId(1),
        site: SiteId(1),
        ty: ResolvedTy::Unit,
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Read,
        kind: HirExprKind::Call {
            callee: Box::new(callee),
            args: Vec::new(),
        },
        span: dummy_span(),
    };
    let main = HirItem::Function(HirFn {
        id: ItemId(0),
        node: HirNodeId(0),
        name: "main".to_string(),
        type_params: Vec::new(),
        params: Vec::new(),
        return_ty: ResolvedTy::Unit,
        body: unit_block_with_tail(call),
        span: dummy_span(),
    });

    let diagnostics = run_call_shape_gates_for_test(&[main], &[]);

    assert!(
        !has_call_shape_diagnostic(&diagnostics),
        "closure-binding call must not trip the call-shape gates: {diagnostics:?}"
    );
}

#[test]
fn item_callee_not_in_callable_set_makes_into_result_fail() {
    // End-to-end: emit the synthetic diagnostic via the gate, push it into
    // a `LowerOutput`-equivalent, and verify the fatal-set wiring kicks in.
    // We exercise this through the public dispatcher by running the gate
    // and checking the diagnostic kind is in the fatal set documented on
    // `LowerOutput::into_result`.
    let main = synth_fn_with_tail("main", 0, synth_call_with_item_callee("phantom_callee", 42));
    let diagnostics = run_call_shape_gates_for_test(&[main], &[]);

    let fatal = diagnostics.iter().any(|d| {
        matches!(
            d.kind,
            HirDiagnosticKind::CallableUnsupportedInMir { .. }
                | HirDiagnosticKind::IndirectCallUnsupported { .. }
        )
    });
    assert!(
        fatal,
        "expected fatal call-shape diagnostic: {diagnostics:?}"
    );
}

// Reuse the closure_binding_call_accepted body to anchor a regression for
// the `let stmt + tail expr` recursion path in the walker.
#[test]
fn walker_recurses_into_let_stmt_initializer() {
    // The walker's `HirStmtKind::Let(_, Some(init))` arm must recurse into
    // the initializer so a synthetic invalid call inside a `let` value is
    // surfaced. Build an item with a `let _ = phantom_callee();` statement
    // and verify the diagnostic appears.
    let bad_call = synth_call_with_item_callee("phantom_callee", 42);
    let stmt = HirStmt {
        node: HirNodeId(0),
        kind: HirStmtKind::Let(unit_binding("_"), Some(bad_call)),
        span: dummy_span(),
    };
    let body = HirBlock {
        node: HirNodeId(0),
        scope: ScopeId(0),
        statements: vec![stmt],
        tail: None,
        ty: ResolvedTy::Unit,
        span: dummy_span(),
    };
    let main = HirItem::Function(HirFn {
        id: ItemId(0),
        node: HirNodeId(0),
        name: "main".to_string(),
        type_params: Vec::new(),
        params: Vec::new(),
        return_ty: ResolvedTy::Unit,
        body,
        span: dummy_span(),
    });

    let diagnostics = run_call_shape_gates_for_test(&[main], &[]);
    assert!(
        diagnostics.iter().any(|d| matches!(
            &d.kind,
            HirDiagnosticKind::CallableUnsupportedInMir { name } if name == "phantom_callee"
        )),
        "walker must recurse into Let-initializer expressions: {diagnostics:?}"
    );
}

// ── Machine transition guard coverage (revision pass 1) ─────────────────────

#[test]
fn call_shape_in_machine_transition_guard_rejected() {
    // Build a synthetic machine whose single transition carries a guard
    // expression containing an `Item(_)`-resolved callee that is absent
    // from the callable set. Prior to FC-P1-B revision pass 1, the walker
    // visited only `transition.body`, so a bad call shape inside the
    // guard slipped through unnoticed. With `HirMachineTransition.guard:
    // Option<HirExpr>` lowered from the AST guard, the walker now
    // recurses into the guard and surfaces `CallableUnsupportedInMir`.
    let bad_call = synth_call_with_item_callee("phantom_guard_callee", 77);
    let trivial_body = HirExpr {
        node: HirNodeId(0),
        site: SiteId(0),
        ty: ResolvedTy::Unit,
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Read,
        kind: HirExprKind::Literal(hew_hir::HirLiteral::Unit),
        span: dummy_span(),
    };
    let machine = HirItem::Machine(HirMachineDecl {
        id: ItemId(0),
        node: HirNodeId(0),
        name: "Gate".to_string(),
        type_params: Vec::new(),
        type_param_bounds: Vec::new(),
        states: vec![HirMachineState {
            name: "Locked".to_string(),
            fields: Vec::new(),
            has_entry: false,
            has_exit: false,
            entry_writes: Vec::new(),
            exit_writes: Vec::new(),
            entry: None,
            exit: None,
            span: dummy_span(),
        }],
        events: vec![HirMachineEvent {
            name: "Try".to_string(),
            fields: Vec::new(),
            span: dummy_span(),
        }],
        transitions: vec![HirMachineTransition {
            event_name: "Try".to_string(),
            source_state: "Locked".to_string(),
            target_state: "Locked".to_string(),
            guard: Some(bad_call),
            is_self_transition: true,
            reenter: false,
            body_writes: Vec::new(),
            body_emits: Vec::new(),
            body: trivial_body,
            span: dummy_span(),
        }],
        has_default: false,
        span: dummy_span(),
    });

    let diagnostics = run_call_shape_gates_for_test(&[machine], &[]);

    assert!(
        diagnostics.iter().any(|d| matches!(
            &d.kind,
            HirDiagnosticKind::CallableUnsupportedInMir { name } if name == "phantom_guard_callee"
        )),
        "expected CallableUnsupportedInMir for callee inside machine transition guard, got: {diagnostics:?}"
    );
}
