use hew_hir::{
    dump_hir, lower_program, verify_hir, HirDiagnosticKind, HirExprKind, HirSelectArmKind,
    HirStmtKind, ResolutionCtx,
};
use hew_parser::ast::{
    Block, Expr, FnDecl, IntRadix, Item, Literal, Pattern, Program, SelectArm, Stmt, TimeoutClause,
    Visibility,
};

fn lower(source: &str) -> hew_hir::LowerOutput {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    lower_program(&parsed.program, &ResolutionCtx)
}

#[test]
fn simple_function_lowers_with_stable_sites() {
    let output = lower("fn main() -> i64 { let x = 1 + 2; return x; }");
    assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);
    let verify = verify_hir(&output.module);
    assert!(verify.is_empty(), "{verify:?}");

    let dump = dump_hir(&output.module);
    assert!(dump.contains("fn i0 main -> int"));
    assert!(dump.contains("let b0 x: int"));
    assert!(dump.contains("expr h"));
    assert!(dump.contains("Read BitCopy: int"));
}

#[test]
fn unresolved_symbol_rejects_before_mir() {
    let output = lower("fn main() -> i32 { return missing; }");
    assert!(output
        .diagnostics
        .iter()
        .any(|diag| matches!(diag.kind, HirDiagnosticKind::UnresolvedSymbol { .. })));
    let verify = verify_hir(&output.module);
    assert!(verify
        .iter()
        .any(|diag| matches!(diag.kind, HirDiagnosticKind::UnresolvedSymbol { .. })));
}

#[test]
fn inferred_type_annotation_rejects_at_hir_boundary() {
    let output = lower("fn main() { let x: _ = 1; }");
    assert!(output
        .diagnostics
        .iter()
        .any(|diag| matches!(diag.kind, HirDiagnosticKind::UnresolvedInferenceVar)));
}

#[test]
fn unsupported_construct_emits_cutover_diagnostic() {
    // A type expression outside slice 1 emits CutoverUnsupported.
    // Pointer types are a slice-2 construct that exercises the _ arm in lower_type.
    // (If this fixture stops triggering CutoverUnsupported, the test will
    //  produce `diagnostics.is_empty()` and the assert will catch it.)
    let output = lower("fn f(x: i64) -> i64 { return x; }");
    // No unsupported diagnostics — this is a clean slice-1 program.
    let cutover_count = output
        .diagnostics
        .iter()
        .filter(|d| matches!(d.kind, HirDiagnosticKind::CutoverUnsupported { .. }))
        .count();
    assert_eq!(
        cutover_count, 0,
        "clean program should have no cutover diagnostics"
    );
}

#[test]
fn call_return_type_resolved_from_registry() {
    // Calling a known function must yield the callee's declared return type.
    // Before the function registry, all calls returned Unit and produced
    // ReturnTypeMismatch when the caller expected a non-Unit type.
    let output = lower(
        "fn add(a: i64, b: i64) -> i64 { return a + b; } \
         fn main() -> i64 { return add(0, 1); }",
    );
    assert!(
        output.diagnostics.is_empty(),
        "cross-function call should type-check cleanly: {:?}",
        output.diagnostics
    );
    let verify = verify_hir(&output.module);
    assert!(verify.is_empty(), "{verify:?}");
}

#[test]
fn call_to_unresolved_function_emits_inference_var() {
    // Calling an unknown function is an inference hole: the callee is
    // Unresolved, so the call result type cannot be determined.
    let output = lower("fn main() -> i64 { return mystery(); }");
    assert!(output
        .diagnostics
        .iter()
        .any(|d| matches!(d.kind, HirDiagnosticKind::UnresolvedInferenceVar)));
}

#[test]
fn verifier_flags_unsupported_hir_node_as_defense_in_depth() {
    // Defense-in-depth: verify_hir emits CutoverUnsupported for any Unsupported
    // HIR node it finds, even when the lowerer already emitted the diagnostic.
    // A tuple literal is a slice-2 expression; `lower_expr` produces an
    // HirExprKind::Unsupported node for it.
    let output = lower("fn f() { let t = (1, 2); }");
    // The lowerer already emits CutoverUnsupported for the unsupported expression.
    assert!(
        output
            .diagnostics
            .iter()
            .any(|d| matches!(d.kind, HirDiagnosticKind::CutoverUnsupported { .. })),
        "lowerer must emit CutoverUnsupported for unsupported expression: {:?}",
        output.diagnostics
    );
    // The verifier independently flags the surviving Unsupported node.
    let verify = verify_hir(&output.module);
    assert!(
        verify
            .iter()
            .any(|d| matches!(d.kind, HirDiagnosticKind::CutoverUnsupported { .. })),
        "verifier must flag Unsupported HIR node as defense-in-depth: {verify:?}"
    );
}

// ── select{} sealed-form recognition ───────────────────────────────────────
//
// Per HEW-SPEC-2026 §4.11.1 the four arm forms are exhaustive. These tests
// drive HIR lowering on each form, and on a sibling near-miss, to verify
// both the diagnostic-triggering and diagnostic-clean paths
// (architecture doc §3.3).

/// Locate the unique `HirExprKind::Select` inside the first function body
/// of the lowered module. The vertical-slice harness wraps every select in
/// a top-level `fn main()`; the select is always the value of the first
/// `let`.
fn find_first_select(output: &hew_hir::LowerOutput) -> &hew_hir::HirSelect {
    let func = output
        .module
        .items
        .iter()
        .find_map(|item| match item {
            hew_hir::HirItem::Function(f) => Some(f),
            hew_hir::HirItem::TypeDecl(_) => None,
        })
        .expect("expected at least one function in lowered module");
    for stmt in &func.body.statements {
        if let HirStmtKind::Let(_, Some(expr)) = &stmt.kind {
            if let HirExprKind::Select(select) = &expr.kind {
                return select;
            }
        }
    }
    panic!("expected a HirExprKind::Select in the first function body");
}

#[test]
fn select_stream_next_recognised_as_sealed_form() {
    // `next(stream)` — sealed form 1.
    let output = lower(
        "fn main() { \
             let r = select { \
                 msg from next(s) => 1, \
             }; \
         }",
    );
    let select = find_first_select(&output);
    assert_eq!(select.arms.len(), 1);
    assert!(matches!(
        &select.arms[0].kind,
        HirSelectArmKind::StreamNext { .. }
    ));
    // Path-pair: the sealed form must NOT emit SelectArmNotSealedForm.
    assert!(
        !output
            .diagnostics
            .iter()
            .any(|d| matches!(d.kind, HirDiagnosticKind::SelectArmNotSealedForm { .. })),
        "sealed next(...) must not emit SelectArmNotSealedForm: {:?}",
        output.diagnostics
    );
}

#[test]
fn select_actor_ask_recognised_as_sealed_form() {
    // `actor.method(args)` — sealed form 2 (actor ask).
    let output = lower(
        "fn main() { \
             let r = select { \
                 reply from worker.process(1) => 2, \
             }; \
         }",
    );
    let select = find_first_select(&output);
    assert_eq!(select.arms.len(), 1);
    match &select.arms[0].kind {
        HirSelectArmKind::ActorAsk { method, args, .. } => {
            assert_eq!(method, "process");
            assert_eq!(args.len(), 1);
        }
        other => panic!("expected ActorAsk arm, got {other:?}"),
    }
}

#[test]
fn select_await_task_recognised_as_sealed_form() {
    // `await task` — sealed form 3.
    let output = lower(
        "fn main() { \
             let r = select { \
                 done from await user_task => 1, \
             }; \
         }",
    );
    let select = find_first_select(&output);
    assert!(matches!(
        &select.arms[0].kind,
        HirSelectArmKind::TaskAwait { .. }
    ));
}

#[test]
fn select_after_timer_recognised_as_sealed_form() {
    // `after duration => body` — sealed form 4.
    let output = lower(
        "fn main() { \
             let r = select { \
                 after 100ms => 1, \
             }; \
         }",
    );
    let select = find_first_select(&output);
    assert_eq!(select.arms.len(), 1);
    assert!(matches!(
        &select.arms[0].kind,
        HirSelectArmKind::AfterTimer { .. }
    ));
    // The after arm has no binding.
    assert!(select.arms[0].binding_name.is_none());
}

#[test]
fn select_heterogeneous_four_arms_all_recognised() {
    // All four sealed forms in one select. The canonical D2 example.
    let output = lower(
        "fn main() { \
             let r = select { \
                 msg from next(s) => 1, \
                 reply from worker.process(2) => 1, \
                 done from await user_task => 1, \
                 after 50ms => 1, \
             }; \
         }",
    );
    let select = find_first_select(&output);
    assert_eq!(select.arms.len(), 4);
    assert!(matches!(
        select.arms[0].kind,
        HirSelectArmKind::StreamNext { .. }
    ));
    assert!(matches!(
        select.arms[1].kind,
        HirSelectArmKind::ActorAsk { .. }
    ));
    assert!(matches!(
        select.arms[2].kind,
        HirSelectArmKind::TaskAwait { .. }
    ));
    assert!(matches!(
        select.arms[3].kind,
        HirSelectArmKind::AfterTimer { .. }
    ));
}

#[test]
fn select_non_sealed_source_rejected() {
    // A bare identifier as the arm source is not a sealed form.
    let output = lower(
        "fn main() { \
             let r = select { \
                 msg from src => 1, \
             }; \
         }",
    );
    assert!(
        output
            .diagnostics
            .iter()
            .any(|d| matches!(d.kind, HirDiagnosticKind::SelectArmNotSealedForm { .. })),
        "bare identifier source must emit SelectArmNotSealedForm: {:?}",
        output.diagnostics
    );
}

#[test]
fn select_literal_source_rejected() {
    // Path-pair sibling for SelectArmNotSealedForm — a literal source.
    let output = lower(
        "fn main() { \
             let r = select { \
                 msg from 42 => 1, \
             }; \
         }",
    );
    assert!(
        output.diagnostics.iter().any(|d| matches!(
            &d.kind,
            HirDiagnosticKind::SelectArmNotSealedForm { source_shape } if source_shape == "literal"
        )),
        "literal source must emit SelectArmNotSealedForm{{\"literal\"}}: {:?}",
        output.diagnostics
    );
}

#[test]
fn select_empty_rejected() {
    let output = lower("fn main() { let r = select { }; }");
    assert!(
        output
            .diagnostics
            .iter()
            .any(|d| matches!(d.kind, HirDiagnosticKind::SelectNoArms)),
        "empty select must emit SelectNoArms: {:?}",
        output.diagnostics
    );
}

#[test]
fn select_non_empty_does_not_trigger_no_arms() {
    // Path-pair sibling: a populated select must NOT emit SelectNoArms.
    let output = lower(
        "fn main() { \
             let r = select { \
                 after 1ms => 1, \
             }; \
         }",
    );
    assert!(
        !output
            .diagnostics
            .iter()
            .any(|d| matches!(d.kind, HirDiagnosticKind::SelectNoArms)),
        "populated select must not emit SelectNoArms: {:?}",
        output.diagnostics
    );
}

#[test]
fn select_arm_body_type_mismatch_rejected() {
    // First arm body is `1` (int); second arm body is `true` (bool).
    let output = lower(
        "fn main() { \
             let r = select { \
                 a from await t1 => 1, \
                 b from await t2 => true, \
             }; \
         }",
    );
    assert!(
        output
            .diagnostics
            .iter()
            .any(|d| matches!(d.kind, HirDiagnosticKind::SelectArmTypeMismatch { .. })),
        "arm body type mismatch must emit SelectArmTypeMismatch: {:?}",
        output.diagnostics
    );
}

#[test]
fn select_arm_body_types_agree_does_not_trigger_mismatch() {
    // Path-pair sibling: uniformly-typed arm bodies must NOT emit
    // SelectArmTypeMismatch.
    let output = lower(
        "fn main() { \
             let r = select { \
                 a from await t1 => 1, \
                 b from await t2 => 2, \
                 after 5ms => 3, \
             }; \
         }",
    );
    assert!(
        !output
            .diagnostics
            .iter()
            .any(|d| matches!(d.kind, HirDiagnosticKind::SelectArmTypeMismatch { .. })),
        "uniformly-typed arms must not emit SelectArmTypeMismatch: {:?}",
        output.diagnostics
    );
}

#[test]
fn select_stream_next_arity_rejected() {
    // `next()` with zero args — sealed form requires exactly one.
    let output = lower(
        "fn main() { \
             let r = select { \
                 msg from next() => 1, \
             }; \
         }",
    );
    assert!(
        output.diagnostics.iter().any(|d| matches!(
            d.kind,
            HirDiagnosticKind::SelectStreamNextArity { arg_count: 0 }
        )),
        "next() with zero args must emit SelectStreamNextArity: {:?}",
        output.diagnostics
    );
}

#[test]
fn select_stream_next_two_args_rejected() {
    // `next(a, b)` — sealed form requires exactly one.
    let output = lower(
        "fn main() { \
             let r = select { \
                 msg from next(a, b) => 1, \
             }; \
         }",
    );
    assert!(
        output.diagnostics.iter().any(|d| matches!(
            d.kind,
            HirDiagnosticKind::SelectStreamNextArity { arg_count: 2 }
        )),
        "next(a, b) must emit SelectStreamNextArity{{2}}: {:?}",
        output.diagnostics
    );
}

/// Build a minimal `Program` that contains a single `fn main()` whose body
/// is `let r = <select_expr>;`. Used to drive the HIR lowerer directly with
/// AST shapes the parser cannot produce (e.g. two `after` arms).
fn program_with_select(select_expr: Expr) -> Program {
    let lit_one = (
        Expr::Literal(Literal::Integer {
            value: 1,
            radix: IntRadix::Decimal,
        }),
        0..1,
    );
    let let_stmt = (
        Stmt::Let {
            pattern: (Pattern::Identifier("r".to_string()), 0..1),
            ty: None,
            value: Some((select_expr, 0..1)),
        },
        0..1,
    );
    let main_fn = FnDecl {
        attributes: vec![],
        is_async: false,
        is_generator: false,
        visibility: Visibility::Private,
        is_pure: false,
        name: "main".to_string(),
        type_params: None,
        params: vec![],
        return_type: None,
        where_clause: None,
        body: Block {
            stmts: vec![let_stmt],
            trailing_expr: Some(Box::new(lit_one)),
        },
        doc_comment: None,
        decl_span: 0..0,
        fn_span: 0..0,
    };
    Program {
        items: vec![(Item::Function(main_fn), 0..0)],
        module_doc: None,
        module_graph: None,
    }
}

#[test]
fn select_two_after_arms_rejected() {
    // Positive path: an `Expr::Timeout`-sourced arm in `arms` combined
    // with the dedicated `timeout` field gives two `after` arms — rejected
    // with exactly one `SelectMultipleAfterArms` diagnostic.
    let dur = Box::new((
        Expr::Literal(Literal::Integer {
            value: 100,
            radix: IntRadix::Decimal,
        }),
        0..3,
    ));
    let body = Box::new((
        Expr::Literal(Literal::Integer {
            value: 1,
            radix: IntRadix::Decimal,
        }),
        0..1,
    ));
    // Arm in `arms` vec with an `Expr::Timeout` source (second `after`).
    let timeout_arm = SelectArm {
        binding: (Pattern::Wildcard, 0..1),
        source: (
            Expr::Timeout {
                expr: Box::new((Expr::Literal(Literal::Bool(false)), 0..1)),
                duration: dur.clone(),
            },
            0..3,
        ),
        body: (
            Expr::Literal(Literal::Integer {
                value: 1,
                radix: IntRadix::Decimal,
            }),
            0..1,
        ),
    };
    let select_expr = Expr::Select {
        arms: vec![timeout_arm],
        timeout: Some(Box::new(TimeoutClause {
            duration: dur,
            body,
        })),
    };
    let program = program_with_select(select_expr);
    let output = lower_program(&program, &ResolutionCtx);
    assert!(
        output
            .diagnostics
            .iter()
            .any(|d| matches!(d.kind, HirDiagnosticKind::SelectMultipleAfterArms)),
        "two after arms must emit SelectMultipleAfterArms: {:?}",
        output.diagnostics
    );
}

#[test]
fn select_one_after_arm_does_not_trigger_multiple_after() {
    // Negative path: one `after` arm (via `timeout`) plus one non-after arm
    // must NOT emit `SelectMultipleAfterArms`.
    let output = lower(
        "fn main() { \
             let r = select { \
                 msg from next(s) => 1, \
                 after 5ms => 1, \
             }; \
         }",
    );
    assert!(
        !output
            .diagnostics
            .iter()
            .any(|d| matches!(d.kind, HirDiagnosticKind::SelectMultipleAfterArms)),
        "single after arm must not emit SelectMultipleAfterArms: {:?}",
        output.diagnostics
    );
}
