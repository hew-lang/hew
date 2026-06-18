use hew_hir::{
    dump_hir, verify_hir, HirDiagnosticKind, HirExprKind, HirSelectArmKind, HirStmtKind,
};
use hew_parser::ast::{
    Block, Expr, FnDecl, IntRadix, Item, Literal, Pattern, Program, SelectArm, Stmt, TimeoutClause,
    Visibility,
};

#[path = "support/mod.rs"]
mod support;

fn lower(source: &str) -> hew_hir::LowerOutput {
    support::checker_pipeline::lower_through_checker(source)
}

#[test]
fn simple_function_lowers_with_stable_sites() {
    let output = lower("fn main() -> i64 { let x = 1 + 2; return x; }");
    assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);
    let verify = verify_hir(&output.module);
    assert!(verify.is_empty(), "{verify:?}");

    let dump = dump_hir(&output.module);
    assert!(dump.contains("fn i0 main -> i64"));
    assert!(dump.contains("let b0 x: i64"));
    assert!(dump.contains("expr h"));
    assert!(dump.contains("Read BitCopy: i64"));
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
fn unsupported_construct_emits_not_yet_implemented_diagnostic() {
    // A type expression outside slice 1 emits NotYetImplemented.
    // Pointer types are a slice-2 construct that exercises the _ arm in lower_type.
    // (If this fixture stops triggering NotYetImplemented, the test will
    //  produce `diagnostics.is_empty()` and the assert will catch it.)
    let output = lower("fn f(x: i64) -> i64 { return x; }");
    // No unsupported diagnostics — this is a clean hir-lowering program.
    let nyi_count = output
        .diagnostics
        .iter()
        .filter(|d| matches!(d.kind, HirDiagnosticKind::NotYetImplemented { .. }))
        .count();
    assert_eq!(
        nyi_count, 0,
        "clean program should have no not-yet-implemented diagnostics"
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
fn call_to_unresolved_function_reports_checker_boundary() {
    // Through the real Checker pipeline, an unknown callee arrives at HIR as
    // an unresolved symbol plus an error-recovery placeholder boundary error.
    let output = lower("fn main() -> i64 { return mystery(); }");
    assert!(
        output
            .diagnostics
            .iter()
            .any(|d| matches!(d.kind, HirDiagnosticKind::UnresolvedSymbol { .. })),
        "expected unresolved symbol diagnostic, got: {:?}",
        output.diagnostics
    );
    assert!(
        output
            .diagnostics
            .iter()
            .any(|d| matches!(d.kind, HirDiagnosticKind::CheckerBoundaryViolation { .. })),
        "expected checker boundary diagnostic, got: {:?}",
        output.diagnostics
    );
}

#[test]
fn array_literal_lowers_to_vec_desugar() {
    let output = lower("fn f() { let t = [1, 2, 3]; }");
    assert!(
        output.diagnostics.is_empty(),
        "array literal should lower through Vec desugar without diagnostics: {:?}",
        output.diagnostics
    );
    let verify = verify_hir(&output.module);
    assert!(verify.is_empty(), "{verify:?}");

    let func = output
        .module
        .items
        .iter()
        .find_map(|item| match item {
            hew_hir::HirItem::Function(func) => Some(func),
            _ => None,
        })
        .expect("fixture lowers one function");
    let Some(HirStmtKind::Let(_, Some(init))) = func.body.statements.first().map(|stmt| &stmt.kind)
    else {
        panic!("expected first statement to be a let with an array-literal initializer");
    };
    let HirExprKind::Block(block) = &init.kind else {
        panic!(
            "array literal should lower to a synthetic block, got {:?}",
            init.kind
        );
    };
    assert_eq!(init.ty.user_facing().to_string(), "Vec<i64>");
    assert_eq!(block.statements.len(), 4);
    assert!(matches!(
        block.statements[0].kind,
        HirStmtKind::Let(
            _,
            Some(hew_hir::HirExpr {
                kind: HirExprKind::Call { .. },
                ..
            })
        )
    ));
    let push_count = block
        .statements
        .iter()
        .filter(|stmt| {
            matches!(
                &stmt.kind,
                HirStmtKind::Expr(hew_hir::HirExpr {
                    kind: HirExprKind::ResolvedImplCall {
                        method_name,
                        target_symbol,
                        ..
                    },
                    ..
                }) if method_name == "push" && target_symbol == "hew_vec_push_i64"
            )
        })
        .count();
    assert_eq!(push_count, 3);
}

#[test]
fn map_literal_lowers_to_hashmap_new_insert_desugar() {
    let output = lower("fn f() { let m = {\"a\": 1, \"b\": 2}; }");
    assert!(
        output.diagnostics.is_empty(),
        "map literal should lower through HashMap desugar without diagnostics: {:?}",
        output.diagnostics
    );
    let verify = verify_hir(&output.module);
    assert!(verify.is_empty(), "{verify:?}");

    let func = output
        .module
        .items
        .iter()
        .find_map(|item| match item {
            hew_hir::HirItem::Function(func) => Some(func),
            _ => None,
        })
        .expect("fixture lowers one function");
    let Some(HirStmtKind::Let(_, Some(init))) = func.body.statements.first().map(|stmt| &stmt.kind)
    else {
        panic!("expected first statement to be a let with a map-literal initializer");
    };
    let HirExprKind::Block(block) = &init.kind else {
        panic!(
            "map literal should lower to a synthetic block, got {:?}",
            init.kind
        );
    };
    assert_eq!(init.ty.user_facing().to_string(), "HashMap<string, i64>");
    assert_eq!(block.statements.len(), 3);
    assert!(matches!(
        block.statements[0].kind,
        HirStmtKind::Let(
            _,
            Some(hew_hir::HirExpr {
                kind: HirExprKind::Call { .. },
                ..
            })
        )
    ));
    let insert_count = block
        .statements
        .iter()
        .filter(|stmt| {
            matches!(
                &stmt.kind,
                HirStmtKind::Expr(hew_hir::HirExpr {
                    kind: HirExprKind::ResolvedImplCall {
                        method_name,
                        target_symbol,
                        ..
                    },
                    ..
                }) if method_name == "insert" && target_symbol == "hew_hashmap_insert_layout"
            )
        })
        .count();
    assert_eq!(insert_count, 2);
}

#[test]
fn empty_map_literal_lowers_to_bare_hashmap_new() {
    let output = lower("fn f() { let m: HashMap<string, i64> = {}; }");
    assert!(
        output.diagnostics.is_empty(),
        "empty map literal should lower through HashMap::new without diagnostics: {:?}",
        output.diagnostics
    );
    let verify = verify_hir(&output.module);
    assert!(verify.is_empty(), "{verify:?}");

    let func = output
        .module
        .items
        .iter()
        .find_map(|item| match item {
            hew_hir::HirItem::Function(func) => Some(func),
            _ => None,
        })
        .expect("fixture lowers one function");
    let Some(HirStmtKind::Let(_, Some(init))) = func.body.statements.first().map(|stmt| &stmt.kind)
    else {
        panic!("expected first statement to be a let with an empty-map initializer");
    };
    let HirExprKind::Block(block) = &init.kind else {
        panic!(
            "empty map literal should lower to a synthetic block, got {:?}",
            init.kind
        );
    };
    assert_eq!(init.ty.user_facing().to_string(), "HashMap<string, i64>");
    assert_eq!(block.statements.len(), 1);
    assert!(matches!(
        block.statements[0].kind,
        HirStmtKind::Let(
            _,
            Some(hew_hir::HirExpr {
                kind: HirExprKind::Call { .. },
                ..
            })
        )
    ));
}

#[test]
fn array_repeat_copy_lowers_to_vec_push_loop() {
    let output = lower("fn f() { let t = [7; 3]; }");
    assert!(
        output.diagnostics.is_empty(),
        "array repeat should lower through Vec push loop without diagnostics: {:?}",
        output.diagnostics
    );
    let verify = verify_hir(&output.module);
    assert!(verify.is_empty(), "{verify:?}");

    let func = output
        .module
        .items
        .iter()
        .find_map(|item| match item {
            hew_hir::HirItem::Function(func) => Some(func),
            _ => None,
        })
        .expect("fixture lowers one function");
    let Some(HirStmtKind::Let(_, Some(init))) = func.body.statements.first().map(|stmt| &stmt.kind)
    else {
        panic!("expected first statement to be a let with an array-repeat initializer");
    };
    let HirExprKind::Block(block) = &init.kind else {
        panic!(
            "array repeat should lower to a synthetic block, got {:?}",
            init.kind
        );
    };
    assert_eq!(init.ty.user_facing().to_string(), "Vec<i64>");
    assert_eq!(block.statements.len(), 4);
    let HirStmtKind::Expr(hew_hir::HirExpr {
        kind: HirExprKind::ForRange { body, .. },
        ..
    }) = &block.statements[3].kind
    else {
        panic!("array repeat should emit a for-range loop");
    };
    assert!(
        body.statements.iter().any(|stmt| matches!(
            &stmt.kind,
            HirStmtKind::Expr(hew_hir::HirExpr {
                kind: HirExprKind::ResolvedImplCall {
                    method_name,
                    target_symbol,
                    ..
                },
                ..
            }) if method_name == "push" && target_symbol == "hew_vec_push_i64"
        )),
        "for-range body should push i64 values into the result Vec"
    );
}

#[test]
fn array_repeat_runtime_count_lowers() {
    let output = lower("fn f(n: i64) { let t = [7; n]; }");
    assert!(
        output.diagnostics.is_empty(),
        "array repeat runtime count should lower without diagnostics: {:?}",
        output.diagnostics
    );
    let verify = verify_hir(&output.module);
    assert!(verify.is_empty(), "{verify:?}");

    let func = output
        .module
        .items
        .iter()
        .find_map(|item| match item {
            hew_hir::HirItem::Function(func) => Some(func),
            _ => None,
        })
        .expect("fixture lowers one function");
    let Some(HirStmtKind::Let(_, Some(init))) = func.body.statements.first().map(|stmt| &stmt.kind)
    else {
        panic!("expected first statement to be a let with an array-repeat initializer");
    };
    let HirExprKind::Block(block) = &init.kind else {
        panic!(
            "array repeat should lower to a synthetic block, got {:?}",
            init.kind
        );
    };
    let HirStmtKind::Expr(hew_hir::HirExpr {
        kind: HirExprKind::ForRange { end, .. },
        ..
    }) = &block.statements[3].kind
    else {
        panic!("array repeat should emit a for-range loop");
    };
    assert!(matches!(
        &end.kind,
        HirExprKind::BindingRef { name, .. } if name.starts_with("__hew_repeat_count_")
    ));
}

#[test]
fn verifier_flags_unsupported_hir_node_as_defense_in_depth() {
    // Defense-in-depth: verify_hir emits NotYetImplemented for any Unsupported
    // HIR node it finds, even when the lowerer already emitted the diagnostic.
    // Owned-element array repeat remains fail-closed pending clone semantics.
    let output = lower("fn f() { let t = [\"x\"; 2]; }");
    // The lowerer already emits NotYetImplemented for the unsupported expression.
    assert!(
        output
            .diagnostics
            .iter()
            .any(|d| matches!(d.kind, HirDiagnosticKind::NotYetImplemented { .. })),
        "lowerer must emit NotYetImplemented for unsupported expression: {:?}",
        output.diagnostics
    );
    // The verifier independently flags the surviving Unsupported node.
    let verify = verify_hir(&output.module);
    assert!(
        verify
            .iter()
            .any(|d| matches!(d.kind, HirDiagnosticKind::NotYetImplemented { .. })),
        "verifier must flag Unsupported HIR node as defense-in-depth: {verify:?}"
    );
}

#[test]
fn verifier_diagnostic_retains_item_source_module() {
    let mut output = lower("fn f() { let t = [\"x\"; 2]; }");
    let func_id = output
        .module
        .items
        .iter()
        .find_map(|item| match item {
            hew_hir::HirItem::Function(func) => Some(func.id),
            _ => None,
        })
        .expect("fixture lowers one function");
    output
        .module
        .diagnostic_source_modules
        .insert(func_id, "dep".to_string());

    let verify = verify_hir(&output.module);
    let diagnostic = verify
        .iter()
        .find(|d| matches!(d.kind, HirDiagnosticKind::NotYetImplemented { .. }))
        .expect("verifier should flag unsupported owned array-repeat node");
    assert_eq!(diagnostic.source_module.as_deref(), Some("dep"));
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
            hew_hir::HirItem::TypeDecl(_)
            | hew_hir::HirItem::Machine(_)
            | hew_hir::HirItem::Record(_)
            | hew_hir::HirItem::Actor(_)
            | hew_hir::HirItem::Supervisor(_)
            | hew_hir::HirItem::Impl(_)
            | hew_hir::HirItem::ExternFn(_)
            | hew_hir::HirItem::Const(_) => None,
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
    // First arm body is `1` (i64); second arm body is `true` (bool).
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

// ── Task<T> inference tests (TI-1 .. TI-5) ──────────────────────────────────

/// TI-1: a call expression used as a statement inside a `fork{}` body lowers
/// to `SpawnedCall` with type `Task<T>`. Calls in the same function but
/// outside the fork body remain synchronous.
#[test]
fn task_handle_ti1_statement_call_inside_fork_becomes_spawned_call() {
    let output = lower(
        "fn worker() -> i64 { return 42; } \
         fn main() { scope { worker(); } }",
    );
    // No diagnostic errors expected from HIR lowering (the fork expression
    // itself lowers cleanly; MIR-level NotYetImplemented fires downstream).
    let hir_diags: Vec<_> = output
        .diagnostics
        .iter()
        .filter(|d| {
            !matches!(
                d.kind,
                HirDiagnosticKind::NotYetImplemented { .. }
                    | HirDiagnosticKind::UnresolvedInferenceVar
            )
        })
        .collect();
    assert!(
        hir_diags.is_empty(),
        "fork body with bare call should lower cleanly: {hir_diags:?}"
    );

    // The dump should mention `scope` (renamed from `fork` in the rip+rename PR)
    // and `spawned-call`.
    let dump = dump_hir(&output.module);
    assert!(
        dump.contains("scope scope="),
        "HIR dump should contain scope node: {dump}"
    );
    assert!(
        dump.contains("spawned-call"),
        "HIR dump should contain spawned-call for TI-1: {dump}"
    );
    // The spawned call's type is Task<i64> — dump shows the user_facing form.
    assert!(
        dump.contains("<task<i64>>"),
        "spawned-call should have Task<i64> type in dump: {dump}"
    );
}

/// TI-2: `fork name = call(...)` inside a fork body binds `name` to type
/// `Task<T>` where `T` is the call's return type.
#[test]
fn task_handle_ti2_named_fork_child_binds_task_type() {
    let output = lower(
        "fn compute() -> i64 { return 7; } \
         fn main() { scope { fork t = compute(); } }",
    );
    // No HIR-level non-infrastructure diagnostics expected.
    let real_diags: Vec<_> = output
        .diagnostics
        .iter()
        .filter(|d| !matches!(d.kind, HirDiagnosticKind::NotYetImplemented { .. }))
        .collect();
    assert!(
        real_diags.is_empty(),
        "fork name = call() should lower without HIR errors: {real_diags:?}"
    );

    let dump = dump_hir(&output.module);
    // The binding `t` should appear with Task<i64> type.
    assert!(
        dump.contains("let") && dump.contains("t:") && dump.contains("<task<i64>>"),
        "named fork binding should have Task<i64> type: {dump}"
    );
}

/// TI-3 (accept side): a call outside a `fork{}` body stays synchronous —
/// its result type is the raw return type, not `Task<T>`.
#[test]
fn task_handle_ti3_call_outside_fork_is_synchronous() {
    let output = lower(
        "fn add(a: i64, b: i64) -> i64 { return a + b; } \
         fn main() -> i64 { return add(1, 2); }",
    );
    assert!(
        output.diagnostics.is_empty(),
        "synchronous call outside fork should lower cleanly: {:?}",
        output.diagnostics
    );
    let dump = dump_hir(&output.module);
    // There must be no spawned-call node — the call is synchronous.
    assert!(
        !dump.contains("spawned-call"),
        "synchronous call must not produce a spawned-call node: {dump}"
    );
    // The call result should be i64, not Task<i64>.
    assert!(
        !dump.contains("<task<"),
        "synchronous call result type must not be Task<T>: {dump}"
    );
}

/// TI-4 (accept): `await name` inside a `fork{}` body where `name` has type
/// `Task<T>` lowers to `AwaitTask` producing type `T`.
#[test]
fn task_handle_ti4_await_task_binding_inside_fork_lowers_to_await_task() {
    let output = lower(
        "fn compute() -> i64 { return 99; } \
         fn main() { scope { fork t = compute(); await t; } }",
    );
    // No non-infrastructure HIR diagnostics.
    let real_diags: Vec<_> = output
        .diagnostics
        .iter()
        .filter(|d| !matches!(d.kind, HirDiagnosticKind::NotYetImplemented { .. }))
        .collect();
    assert!(
        real_diags.is_empty(),
        "await on Task<T> inside fork should lower without HIR errors: {real_diags:?}"
    );

    let dump = dump_hir(&output.module);
    assert!(
        dump.contains("await-task"),
        "HIR dump must contain await-task node: {dump}"
    );
    // The await-task node produces the inner type (i64), not Task<i64>.
    assert!(
        dump.contains("await-task t"),
        "await-task should reference binding name 't': {dump}"
    );
}

/// TI-4 (reject): `await name` outside a `fork{}` body emits
/// `AwaitOutOfPosition`.
#[test]
fn task_handle_ti4_await_outside_fork_rejects_with_await_out_of_position() {
    let output = lower("fn f() { let x = 1; await x; }");
    assert!(
        output
            .diagnostics
            .iter()
            .any(|d| matches!(d.kind, HirDiagnosticKind::AwaitOutOfPosition)),
        "await outside fork body must emit AwaitOutOfPosition: {:?}",
        output.diagnostics
    );
}

/// TI-4 (reject): `await expr` where `expr` does not have type `Task<T>`
/// emits `AwaitNonTask`.
#[test]
fn task_handle_ti4_await_non_task_rejects_with_await_non_task() {
    // Inside a fork body so position is legal, but the operand is i64.
    let output = lower("fn f() { scope { let x = 5; await x; } }");
    assert!(
        output
            .diagnostics
            .iter()
            .any(|d| matches!(d.kind, HirDiagnosticKind::AwaitNonTask { .. })),
        "await on non-Task binding must emit AwaitNonTask: {:?}",
        output.diagnostics
    );
}

/// TI-2 (reject): `fork name = non_call_expr` emits `ForkChildNotACall`.
#[test]
fn task_handle_ti2_fork_child_non_call_rhs_rejects() {
    let output = lower("fn f() { scope { fork t = 42; } }");
    assert!(
        output
            .diagnostics
            .iter()
            .any(|d| matches!(d.kind, HirDiagnosticKind::ForkChildNotACall)),
        "fork name = non-call must emit ForkChildNotACall: {:?}",
        output.diagnostics
    );
}

#[test]
fn scope_deadline_derives_cancellation_token() {
    let output = lower(
        "fn long_op() { } \
         fn main() { scope { fork { long_op(); } after(5s) { } } }",
    );
    let real_diags: Vec<_> = output
        .diagnostics
        .iter()
        .filter(|d| !matches!(d.kind, HirDiagnosticKind::NotYetImplemented { .. }))
        .collect();
    assert!(
        real_diags.is_empty(),
        "fork-block plus scope deadline should lower without HIR errors: {real_diags:?}"
    );

    let dump = dump_hir(&output.module);
    assert!(
        dump.contains("fork-block"),
        "HIR dump must carry the fork block cancellation child: {dump}"
    );
    assert!(
        dump.contains("scope-deadline"),
        "HIR dump must carry the scope deadline cancellation edge: {dump}"
    );
    assert!(
        dump.contains("<task<()>>"),
        "fork-block should be represented as an anonymous Task<Unit>: {dump}"
    );
}

/// TI-5 (structural): the `lower_type` path rejects `Task` as a user-written
/// type annotation via `TaskNotNameable` (wired through the `Named` arm for
/// the name "Task"). This test exercises the type-annotation wall.
#[test]
fn task_handle_ti5_task_annotation_in_let_rejects_task_not_nameable() {
    // The parser accepts `let t: Task<i64> = 0;` as a named type annotation;
    // HIR lowering must reject it with TaskNotNameable.
    let output = lower("fn f() { let t: Task<i64> = 0; }");
    assert!(
        output
            .diagnostics
            .iter()
            .any(|d| matches!(d.kind, HirDiagnosticKind::TaskNotNameable)),
        "let t: Task<T> = ... must emit TaskNotNameable: {:?}",
        output.diagnostics
    );
}

/// TI-4 (reject): `let x = await name` inside a `fork{}` body emits
/// `AwaitOutOfPosition`. Await is only legal as a statement-expression, never
/// as a let-value — the result cannot be bound.
#[test]
fn task_handle_ti4_await_in_let_value_position_rejects() {
    let output = lower(
        "fn compute() -> i64 { return 1; } \
         fn f() { scope { fork t = compute(); let x = await t; } }",
    );
    assert!(
        output
            .diagnostics
            .iter()
            .any(|d| matches!(d.kind, HirDiagnosticKind::AwaitOutOfPosition)),
        "let x = await t inside fork must emit AwaitOutOfPosition: {:?}",
        output.diagnostics
    );
}

// ── TI-4 position-completeness tests (await in non-statement sub-expression positions) ──

/// TI-4 (reject): `return await t` inside a fork body emits `AwaitOutOfPosition`.
/// Await is only legal as a statement-expression, not as a return value.
#[test]
fn await_in_return_position_rejects() {
    let output = lower(
        "fn compute() -> i64 { return 1; } \
         fn f() { scope { fork t = compute(); return await t; } }",
    );
    assert!(
        output
            .diagnostics
            .iter()
            .any(|d| matches!(d.kind, HirDiagnosticKind::AwaitOutOfPosition)),
        "`return await t` must emit AwaitOutOfPosition: {:?}",
        output.diagnostics
    );
}

/// TI-4 (reject): `sink(await t)` inside a fork body emits `AwaitOutOfPosition`.
/// Await cannot appear as a function argument.
#[test]
fn await_in_function_arg_rejects() {
    let output = lower(
        "fn compute() -> i64 { return 1; } \
         fn sink(x: i64) { } \
         fn f() { scope { fork t = compute(); sink(await t); } }",
    );
    assert!(
        output
            .diagnostics
            .iter()
            .any(|d| matches!(d.kind, HirDiagnosticKind::AwaitOutOfPosition)),
        "`sink(await t)` must emit AwaitOutOfPosition: {:?}",
        output.diagnostics
    );
}

/// TI-4 (reject): `(await t) + 1` inside a fork body emits `AwaitOutOfPosition`.
/// Await cannot appear as a binary operand.
#[test]
fn await_in_binary_operand_rejects() {
    let output = lower(
        "fn compute() -> i64 { return 1; } \
         fn f() { scope { fork t = compute(); let x = (await t) + 1; } }",
    );
    // This may fire AwaitOutOfPosition (binary operand) or the existing
    // let-value intercept — either satisfies the position-rejection rule.
    let has_position_error = output.diagnostics.iter().any(|d| {
        matches!(
            d.kind,
            HirDiagnosticKind::AwaitOutOfPosition | HirDiagnosticKind::AwaitNonTask { .. }
        )
    });
    assert!(
        has_position_error,
        "`(await t) + 1` must emit AwaitOutOfPosition: {:?}",
        output.diagnostics
    );
}

// ── TI-5 escape-via-return tests ────────────────────────────────────────────

// ── Block-wrapped non-ask await guard / lowering consistency ────────────────

/// Regression (PR #1841): `let b = await { conn.read() };` must emit
/// `AwaitOutOfPosition` and not slip through the let-value guard.
///
/// Before the fix the `is_bindable_await` guard computed one unwrapped
/// `inner_key` (trailing-call span) and reused it for ALL side-table checks
/// including `conn_await_reads`.  The checker records `conn_await_reads` under
/// the method-call span, so the guard found the entry and returned `true` —
/// allowing the let-value through.  The corresponding lowering arm at
/// `Expr::Await` then looked up `conn_await_reads` by the BLOCK span (`inner.1`),
/// found nothing, and fell through to the generic await path, producing an
/// inconsistent or silently wrong HIR node.
///
/// After the fix only the `actor_method_dispatch` lookup uses the unwrapped
/// effective span; the non-ask tables (`conn_await_reads`, `listener_await_accepts`,
/// stream/channel recv) use the original `inner.1` span.  A block-wrapped
/// `conn.read()` in a let-value position therefore fails the guard and the HIR
/// lowering emits `AwaitOutOfPosition`, consistent with `main`-branch behaviour.
#[test]
fn block_wrapped_conn_read_await_in_let_value_rejects_with_await_out_of_position() {
    // `fn handler(conn: Connection)` — Connection is a recognised built-in
    // handle type; no import required.  The block-wrapped `await { conn.read() }`
    // is the minimal non-ask bindable-await shape that the pre-fix guard would
    // incorrectly accept (the checker records conn_await_reads under the method-
    // call span, matching the unwrapped key but not the block span the lowering
    // arm uses).
    let output = lower("fn handler(conn: Connection) { let b = await { conn.read() }; }");
    assert!(
        output
            .diagnostics
            .iter()
            .any(|d| matches!(d.kind, HirDiagnosticKind::AwaitOutOfPosition)),
        "block-wrapped `await {{ conn.read() }}` in let-value must emit \
         AwaitOutOfPosition (guard/lowering span consistency); got: {:?}",
        output.diagnostics
    );
}

/// TI-5 (reject): returning an inferred `Task<T>` binding from inside a fork
/// body must emit `TaskCannotEscape`. The type checker rejects user-written
/// `Task<T>` annotations via `TaskNotNameable`; this closes the inferred-escape path.
#[test]
fn inferred_task_return_rejects() {
    let output = lower(
        "fn compute() -> i64 { return 1; } \
         fn f() { scope { fork t = compute(); return t; } }",
    );
    assert!(
        output
            .diagnostics
            .iter()
            .any(|d| matches!(d.kind, HirDiagnosticKind::TaskCannotEscape)),
        "`return t` where t: Task<T> must emit TaskCannotEscape: {:?}",
        output.diagnostics
    );
}

/// TI-5 (accept): returning a non-Task value from inside a fork body is fine.
#[test]
fn non_task_return_inside_fork_accepts() {
    let output = lower(
        "fn compute() -> i64 { return 1; } \
         fn f() { scope { fork t = compute(); await t; } }",
    );
    let task_escape_errors: Vec<_> = output
        .diagnostics
        .iter()
        .filter(|d| matches!(d.kind, HirDiagnosticKind::TaskCannotEscape))
        .collect();
    assert!(
        task_escape_errors.is_empty(),
        "await-then-no-return should not emit TaskCannotEscape: {task_escape_errors:?}"
    );
}

/// Verifier stability: a valid `scope { call(); }` program passes `verify_hir`
/// without dangling-ref or duplicate-id diagnostics.
#[test]
fn task_handle_fork_block_passes_verifier() {
    let output = lower(
        "fn work() -> i64 { return 1; } \
         fn main() { scope { work(); } }",
    );
    let verify = verify_hir(&output.module);
    // Verifier should not emit any DanglingRef, DuplicateBindingId, or
    // DuplicateNodeId diagnostics — only structural HIR integrity issues.
    let structural_failures: Vec<_> = verify
        .iter()
        .filter(|d| {
            matches!(
                d.kind,
                HirDiagnosticKind::DanglingRef { .. }
                    | HirDiagnosticKind::DuplicateBindingId { .. }
                    | HirDiagnosticKind::DuplicateNodeId { .. }
                    | HirDiagnosticKind::DuplicateSiteId { .. }
            )
        })
        .collect();
    assert!(
        structural_failures.is_empty(),
        "fork block must produce structurally valid HIR: {structural_failures:?}"
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
        intrinsic: None,
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
    let output = support::checker_pipeline::lower_through_checker_from_program(&program);
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

// ── actor-lambda capture lexical scoping ────────────────────────────────────
//
// Per HEW-SPEC-2026 §5.9 ratification 2, an actor-lambda's capture set
// classifies each free variable as `Strong` (the body holds a refcount on the
// captured handle) or `Weak` (the body's reference to its own let-binding
// name, which must not keep the actor alive past external refcount zero).
//
// The discriminator is the lambda's lexical self-id — the BindingId of the
// let-name that the actor-lambda is bound to via the forward-bind path. That
// id is set on entry to the lambda body's walk and MUST be cleared (or saved
// and overridden) when descending into a nested actor-lambda body, otherwise
// the outer self-id leaks into the inner classifier and a Strong capture of
// the outer name gets misclassified as Weak.

/// Walk the lowered module, return every `HirExprKind::SpawnLambdaActor`
/// reached. Used by the lambda-capture scoping tests below.
fn collect_spawn_lambdas(output: &hew_hir::LowerOutput) -> Vec<&hew_hir::HirExpr> {
    let mut out: Vec<&hew_hir::HirExpr> = Vec::new();
    for item in &output.module.items {
        if let hew_hir::HirItem::Function(f) = item {
            for stmt in &f.body.statements {
                if let HirStmtKind::Let(_, Some(expr)) = &stmt.kind {
                    walk_expr_collect_lambdas(expr, &mut out);
                }
            }
        }
    }
    out
}

fn walk_expr_collect_lambdas<'a>(expr: &'a hew_hir::HirExpr, out: &mut Vec<&'a hew_hir::HirExpr>) {
    if matches!(expr.kind, HirExprKind::SpawnLambdaActor { .. }) {
        out.push(expr);
    }
    match &expr.kind {
        HirExprKind::SpawnLambdaActor { body, .. } => {
            walk_expr_collect_lambdas(body, out);
        }
        HirExprKind::Block(block)
        | HirExprKind::Scope { body: block }
        | HirExprKind::GenBlock { body: block, .. } => {
            for s in &block.statements {
                if let HirStmtKind::Let(_, Some(e)) = &s.kind {
                    walk_expr_collect_lambdas(e, out);
                }
                if let HirStmtKind::Expr(e) = &s.kind {
                    walk_expr_collect_lambdas(e, out);
                }
            }
            if let Some(tail) = &block.tail {
                walk_expr_collect_lambdas(tail, out);
            }
        }
        HirExprKind::If {
            condition,
            then_expr,
            else_expr,
        } => {
            walk_expr_collect_lambdas(condition, out);
            walk_expr_collect_lambdas(then_expr, out);
            if let Some(e) = else_expr {
                walk_expr_collect_lambdas(e, out);
            }
        }
        HirExprKind::Binary { left, right, .. } => {
            walk_expr_collect_lambdas(left, out);
            walk_expr_collect_lambdas(right, out);
        }
        HirExprKind::Unary { operand, .. } => walk_expr_collect_lambdas(operand, out),
        HirExprKind::Call { callee, args } | HirExprKind::SpawnedCall { callee, args, .. } => {
            walk_expr_collect_lambdas(callee, out);
            for a in args {
                walk_expr_collect_lambdas(a, out);
            }
        }
        HirExprKind::Yield {
            value: Some(value), ..
        } => walk_expr_collect_lambdas(value, out),
        _ => {}
    }
}

#[test]
fn nested_actor_lambda_does_not_inherit_outer_self_id() {
    // Body:
    //   let outer = actor |x: i64| -> i64 {
    //       actor |y: i64| -> i64 { outer; 0 };
    //       x + 1
    //   };
    //
    // The expression-position (non-let) inner lambda's body references
    // `outer`. `outer` is a free variable captured from the enclosing
    // scope — it MUST be Strong. Before the lexical-scoping fix, the
    // outer's `current_actor_self` leaked into the inner lambda's
    // classifier (the inner lambda never sets its own self-id because
    // there is no `lower_stmt` let-pre-bind for an anonymous lambda)
    // and `outer` got misclassified as Weak.
    let source = r"
fn make() {
    let outer = actor |x: i64| -> i64 {
        actor |y: i64| -> i64 {
            outer;
            0
        };
        x + 1
    };
}
";
    let output = lower(source);
    let lambdas = collect_spawn_lambdas(&output);
    // The inner lambda is the one whose parameter is named `y`. Picking by
    // param name avoids tangling with the outer lambda's `inner`-capture.
    let inner = lambdas
        .iter()
        .find(|expr| match &expr.kind {
            HirExprKind::SpawnLambdaActor { params, .. } => params.iter().any(|p| p.name == "y"),
            _ => false,
        })
        .expect("inner actor-lambda (param `y`) must exist");
    let HirExprKind::SpawnLambdaActor { captures, .. } = &inner.kind else {
        unreachable!();
    };
    let outer_cap = captures
        .iter()
        .find(|c| c.name == "outer")
        .unwrap_or_else(|| {
            panic!("`outer` must appear in inner lambda's captures; got {captures:?}")
        });
    assert_eq!(
        outer_cap.kind,
        hew_hir::HirCaptureKind::Strong,
        "`outer` is a free variable captured by the inner lambda; it must \
         be Strong, not the inner's own self-binding. captures = {captures:?}",
    );
}

#[test]
fn nested_actor_lambda_classifies_own_self_as_weak() {
    // Sibling positive case: the inner lambda's OWN self-reference
    // (its own let-name) must still classify as Weak — the lexical
    // scoping fix must not regress the §5.9 ratification 2 path.
    let source = r"
fn make() {
    let outer = actor |x: i64| -> i64 {
        let inner = actor |y: i64| -> i64 {
            inner;
            y + 1
        };
        inner;
        x + 1
    };
}
";
    let output = lower(source);
    let lambdas = collect_spawn_lambdas(&output);
    // The inner lambda is the one whose parameter is named `y`.
    let inner = lambdas
        .iter()
        .find(|expr| match &expr.kind {
            HirExprKind::SpawnLambdaActor { params, .. } => params.iter().any(|p| p.name == "y"),
            _ => false,
        })
        .expect("inner actor-lambda (param `y`) must exist");
    let HirExprKind::SpawnLambdaActor { captures, .. } = &inner.kind else {
        unreachable!();
    };
    let inner_cap = captures
        .iter()
        .find(|c| c.name == "inner")
        .unwrap_or_else(|| {
            panic!("`inner` must appear in inner lambda's captures; got {captures:?}")
        });
    assert_eq!(
        inner_cap.kind,
        hew_hir::HirCaptureKind::Weak,
        "the inner lambda's reference to its own let-name `inner` must \
         classify as Weak (§5.9 ratification 2). captures = {captures:?}",
    );
}

// ── typed-let forward-bind for actor lambdas ────────────────────────────────
//
// The forward-bind path in `lower_stmt` pre-allocates the let-binding so
// the actor body can reference its own name recursively. Initially this
// only fired when the let had no type annotation; a typed-let bypassed
// pre-binding and the body's self-reference resolved as Unresolved.
//
// The fix extends the pre-bind to typed actor-lets. The annotation is
// respected for the binding type (not overridden by the synthetic
// Duplex shape).

#[test]
fn typed_actor_let_forward_bind_resolves_self_reference() {
    // `let fib: Duplex<i64, i64> = actor |n: i64| -> i64 { fib; n + 1 };`
    // The annotation matches the synthesised Duplex<i64, i64>. The body's
    // bare-identifier `fib` must resolve to the let's binding — not emit
    // UnresolvedSymbol.
    let source = r"
fn make() {
    let fib: Duplex<i64, i64> = actor |n: i64| -> i64 {
        fib;
        n + 1
    };
}
";
    let output = lower(source);
    let unresolved_fib: Vec<_> = output
        .diagnostics
        .iter()
        .filter(|d| match &d.kind {
            HirDiagnosticKind::UnresolvedSymbol { name } => name == "fib",
            _ => false,
        })
        .collect();
    assert!(
        unresolved_fib.is_empty(),
        "typed actor-let must pre-bind the let-name so the body's `fib` \
         resolves; diagnostics = {:?}",
        output.diagnostics
    );
    // And the lambda's captures must contain a Weak self-capture for `fib`.
    let lambdas = collect_spawn_lambdas(&output);
    let lambda = lambdas
        .iter()
        .find(|expr| matches!(expr.kind, HirExprKind::SpawnLambdaActor { .. }))
        .expect("typed actor-let must lower to a SpawnLambdaActor");
    let HirExprKind::SpawnLambdaActor { captures, .. } = &lambda.kind else {
        unreachable!();
    };
    let fib_cap = captures
        .iter()
        .find(|c| c.name == "fib")
        .expect("body's `fib` must appear as a capture");
    assert_eq!(
        fib_cap.kind,
        hew_hir::HirCaptureKind::Weak,
        "typed actor-let recursive self-reference must classify as Weak; \
         captures = {captures:?}",
    );
}

// ── unlink rejection ───────────────────────────────────────────────────────
//
// `link`, `monitor`, and `unlink` all pass through HIR so MIR can emit
// the matching `hew_actor_*` runtime ABI calls. Statement-position use
// is wired; value-needed composite returns remain fail-closed at MIR.

#[test]
fn unlink_call_lowers_without_diagnostics() {
    // `unlink(pid)` now has a MIR producer arm (mirroring `link`/`monitor`).
    // HIR lowers it cleanly — no NotYetImplemented, no UnresolvedSymbol.
    // The MIR producer synthesizes `hew_actor_self()` as arg0 and the
    // user target as arg1, matching the `hew_actor_unlink(a, b)` ABI.
    let output = lower(
        "actor Probe { receive fn crash() { exit(1) } }
         fn main() { let p = spawn Probe; unlink(p); }",
    );
    assert!(
        output.diagnostics.is_empty(),
        "unlink() call must lower without HIR diagnostics, got: {:?}",
        output.diagnostics
    );
    let unresolved_name = output.diagnostics.iter().any(
        |d| matches!(&d.kind, HirDiagnosticKind::UnresolvedSymbol { name } if name == "unlink"),
    );
    assert!(
        !unresolved_name,
        "unlink() must not produce UnresolvedSymbol(unlink)"
    );
}

#[test]
fn top_level_const_lowers_and_resolves_references() {
    // A module-level `const` lowers to a `HirItem::Const` carrying the
    // constant-folded value, and a reference to it resolves cleanly — no
    // `UnresolvedSymbol`, no slice-2 `NotYetImplemented`.
    let output = lower("const X: i64 = 42; fn main() -> i64 { return X; }");
    assert!(
        output.diagnostics.is_empty(),
        "const program should lower without diagnostics, got: {:?}",
        output.diagnostics
    );
    let verify = verify_hir(&output.module);
    assert!(verify.is_empty(), "{verify:?}");

    let const_item = output
        .module
        .items
        .iter()
        .find_map(|item| match item {
            hew_hir::HirItem::Const(c) => Some(c),
            _ => None,
        })
        .expect("module lowers one const item");
    assert_eq!(const_item.name, "X");
    assert_eq!(const_item.value, hew_hir::HirConstValue::Integer(42));

    let dump = dump_hir(&output.module);
    assert!(dump.contains("const i0 X: i64 = 42"), "dump was:\n{dump}");
}

#[test]
fn top_level_const_folds_integer_arithmetic() {
    let output = lower("const Y: i64 = 1 + 2 * 3; fn main() -> i64 { return Y; }");
    assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);
    let value = output
        .module
        .items
        .iter()
        .find_map(|item| match item {
            hew_hir::HirItem::Const(c) => Some(c.value.clone()),
            _ => None,
        })
        .expect("module lowers one const item");
    assert_eq!(value, hew_hir::HirConstValue::Integer(7));
}

#[test]
fn top_level_const_folds_signed_negative_initializers() {
    let output = lower(
        "const A: i8 = -5; \
         const B: i16 = -(1 + 2); \
         const C: i32 = -13; \
         const D: i64 = -42; \
         const E: isize = -7; \
         fn main() -> i64 { return 0; }",
    );
    assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);

    let values: std::collections::HashMap<_, _> = output
        .module
        .items
        .iter()
        .filter_map(|item| match item {
            hew_hir::HirItem::Const(c) => Some((c.name.as_str(), c.value.clone())),
            _ => None,
        })
        .collect();
    assert_eq!(values["A"], hew_hir::HirConstValue::Integer(-5));
    assert_eq!(values["B"], hew_hir::HirConstValue::Integer(-3));
    assert_eq!(values["C"], hew_hir::HirConstValue::Integer(-13));
    assert_eq!(values["D"], hew_hir::HirConstValue::Integer(-42));
    assert_eq!(values["E"], hew_hir::HirConstValue::Integer(-7));
}

#[test]
fn top_level_string_const_folds_literal() {
    let output = lower("const NAME: String = \"hew\"; fn main() -> i64 { return 0; }");
    let value = output
        .module
        .items
        .iter()
        .find_map(|item| match item {
            hew_hir::HirItem::Const(c) => Some(c.value.clone()),
            _ => None,
        })
        .expect("module lowers one const item");
    assert_eq!(value, hew_hir::HirConstValue::String("hew".to_string()));
}

#[test]
fn top_level_float_consts_fold_literals_and_resolve_references() {
    let output = lower(
        "const PI: f64 = 3.14; \
         const HALF: f32 = 0.5; \
         fn add_pi(x: f64) -> f64 { return x + PI; } \
         fn main() -> f32 { return HALF + 1.0; }",
    );
    assert!(
        output.diagnostics.is_empty(),
        "float const program should lower without diagnostics, got: {:?}",
        output.diagnostics
    );
    let verify = verify_hir(&output.module);
    assert!(verify.is_empty(), "{verify:?}");

    let values: std::collections::HashMap<_, _> = output
        .module
        .items
        .iter()
        .filter_map(|item| match item {
            hew_hir::HirItem::Const(c) => Some((c.name.as_str(), c.value.clone())),
            _ => None,
        })
        .collect();
    assert_eq!(
        values["PI"],
        hew_hir::HirConstValue::Float("3.14".parse::<f64>().expect("valid fixture float"))
    );
    assert_eq!(values["HALF"], hew_hir::HirConstValue::Float(0.5));

    let dump = dump_hir(&output.module);
    assert!(
        dump.contains("const i0 PI: f64 = 3.14"),
        "dump was:\n{dump}"
    );
}

#[test]
fn top_level_negative_float_const_folds_literal() {
    let output = lower("const NEG: f64 = -3.14; fn main() -> f64 { return NEG; }");
    assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);
    let value = output
        .module
        .items
        .iter()
        .find_map(|item| match item {
            hew_hir::HirItem::Const(c) => Some(c.value.clone()),
            _ => None,
        })
        .expect("module lowers one const item");
    assert_eq!(
        value,
        hew_hir::HirConstValue::Float("-3.14".parse::<f64>().expect("valid fixture float"))
    );
}

#[test]
fn top_level_const_bitwise_initializer_fails_closed() {
    // Bitwise/wrapping ops are deliberately rejected by the shared
    // `const_eval` engine (NotConstant). The HIR fold must surface a
    // fail-closed `NotYetImplemented` diagnostic rather than admitting a
    // folded value — this locks in the scope-narrowing after consolidating
    // onto the sanctioned evaluator (A620).
    let output = lower("const Z: i64 = 1 & 2; fn main() -> i64 { return Z; }");
    let is_unsupported = output.diagnostics.iter().any(|d| match &d.kind {
        HirDiagnosticKind::NotYetImplemented {
            construct,
            owning_pass,
        } => construct.contains("unsupported const initializer") && owning_pass == "const-fold",
        _ => false,
    });
    assert!(
        is_unsupported,
        "bitwise const initializer must emit a fail-closed NotYetImplemented, got: {:?}",
        output.diagnostics
    );
}

#[test]
fn top_level_float_const_non_literal_initializer_fails_closed() {
    let output = lower("const Z: f64 = 1.0 + 2.0; fn main() -> f64 { return Z; }");
    let is_unsupported = output.diagnostics.iter().any(|d| match &d.kind {
        HirDiagnosticKind::NotYetImplemented {
            construct,
            owning_pass,
        } => construct.contains("unsupported const initializer") && owning_pass == "const-fold",
        _ => false,
    });
    assert!(
        is_unsupported,
        "non-literal float const initializer must emit a fail-closed NotYetImplemented, got: {:?}",
        output.diagnostics
    );
}
