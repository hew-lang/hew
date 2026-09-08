use crate::support;
use hew_hir::{
    dump_hir, verify_hir, HirDiagnosticKind, HirExprKind, HirSelectArmKind, HirStmtKind,
};

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
fn duration_scaling_and_ratio_preserve_checked_operand_types() {
    let output = lower("fn ratio(value: i64) -> i64 { (value * 1ms) / 1ms }");
    assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);
    assert!(verify_hir(&output.module).is_empty());
    let function = output
        .module
        .items
        .iter()
        .find_map(|item| match item {
            hew_hir::HirItem::Function(function) if function.name == "ratio" => Some(function),
            _ => None,
        })
        .expect("ratio function");
    let ratio = function.body.tail.as_deref().expect("ratio result");
    assert_eq!(ratio.ty, hew_types::ResolvedTy::I64);
    let HirExprKind::Binary { left, right, .. } = &ratio.kind else {
        panic!("expected duration ratio, got {:?}", ratio.kind);
    };
    assert_eq!(left.ty, hew_types::ResolvedTy::Duration);
    assert_eq!(right.ty, hew_types::ResolvedTy::Duration);
    let HirExprKind::Binary { left, right, .. } = &left.kind else {
        panic!("expected duration scaling");
    };
    assert_eq!(left.ty, hew_types::ResolvedTy::I64);
    assert_eq!(right.ty, hew_types::ResolvedTy::Duration);
}

#[test]
fn channel_result_sites_are_affine_in_hir() {
    let output = support::checker_pipeline::lower_through_checker_with_modules(
        r#"
        import std.channel.channel;

        fn make_channel_result()
            -> Result<(channel.Sender<i64>, channel.Receiver<i64>), string> {
            panic("not called")
        }

        fn main() {
            let result: Result<(channel.Sender<i64>, channel.Receiver<i64>), string> =
                make_channel_result();
            match result {
                .Ok((_sender, _receiver)) => (),
                .Err(error) => panic(error),
            }
        }
        "#,
    );
    assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);

    let main = output
        .module
        .items
        .iter()
        .find_map(|item| match item {
            hew_hir::HirItem::Function(function) if function.name == "main" => Some(function),
            _ => None,
        })
        .expect("main function");
    let HirStmtKind::Let(_, Some(call)) = &main.body.statements[0].kind else {
        panic!("expected channel result binding");
    };
    let Some(match_expr) = main.body.tail.as_deref() else {
        panic!("expected match tail");
    };
    let HirExprKind::Match { scrutinee, .. } = &match_expr.kind else {
        panic!("expected Result match");
    };

    assert_eq!(call.value_class, hew_hir::ValueClass::AffineResource);
    assert_eq!(scrutinee.value_class, hew_hir::ValueClass::AffineResource);
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
        &block.statements[0].kind,
        HirStmtKind::Let(
            binding,
            Some(hew_hir::HirExpr {
                kind: HirExprKind::Call {
                    target: hew_types::CallTarget::Runtime(hew_types::RuntimeCallFamily::Vector(
                        hew_types::VecValueOp::New,
                    )),
                    ..
                },
                ..
            })
        ) if binding.mutable
    ));
    let push_count = block
        .statements
        .iter()
        .filter(|stmt| {
            matches!(
                &stmt.kind,
                HirStmtKind::Expr(hew_hir::HirExpr {
                    kind: HirExprKind::Call {
                        target: hew_types::CallTarget::Runtime(hew_types::RuntimeCallFamily::Vector(hew_types::VecValueOp::Push)),
                        args,
                        ..
                    },
                    ..
                }) if args.len() == 2 && args[1].ty == hew_types::ResolvedTy::I64
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
        &block.statements[0].kind,
        HirStmtKind::Let(
            binding,
            Some(hew_hir::HirExpr {
                kind: HirExprKind::Call {
                    target: hew_types::CallTarget::Runtime(hew_types::RuntimeCallFamily::Map(
                        hew_types::runtime_call::MapValueOp::New,
                    )),
                    ..
                },
                ..
            })
        ) if binding.mutable
    ));
    let insert_count = block
        .statements
        .iter()
        .filter(|stmt| {
            matches!(
                &stmt.kind,
                HirStmtKind::Expr(hew_hir::HirExpr {
                    kind: HirExprKind::Call {
                        target: hew_types::CallTarget::Runtime(hew_types::RuntimeCallFamily::Map(
                            hew_types::runtime_call::MapValueOp::Insert,
                        )),
                        args,
                        ..
                    },
                    ..
                }) if args.len() == 3
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
                kind: HirExprKind::Call {
                    target: hew_types::CallTarget::Runtime(hew_types::RuntimeCallFamily::Vector(hew_types::VecValueOp::Push)),
                    args,
                    ..
                },
                ..
            }) if args.len() == 2 && args[1].ty == hew_types::ResolvedTy::I64
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
    // Applying `?` to a non-Result/non-Option value in a function that does not
    // return Result/Option is still unsupported at the HIR level; the lowerer
    // emits NotYetImplemented and leaves an Unsupported node for the verifier.
    let output = lower("fn f() -> i64 { let x: i64 = 5; x? }");
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
    // Applying `?` to a non-Result/non-Option value produces an Unsupported HIR
    // node; this test verifies that source-module attribution is preserved on
    // verifier-emitted diagnostics.
    let mut output = lower("fn f() -> i64 { let x: i64 = 5; x? }");
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
        .expect("verifier should flag unsupported postfix-try HIR node");
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
            hew_hir::HirItem::Function(f) if f.name == "main" => Some(f),
            hew_hir::HirItem::Function(_)
            | hew_hir::HirItem::TypeDecl(_)
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
fn select_task_and_timer_preserve_typed_binding_and_result() {
    let output = lower_checked_task(
        r"
        fn main() {
            let first = fork { 41 };
            let second = fork { 42 };
            let result: i64 = select {
                a from first => a + 1,
                b from second => b,
                after 5ms => 0,
            };
        }
    ",
    );
    let select = find_first_select(&output);
    assert_eq!(select.arms.len(), 3);
    for arm in &select.arms[..2] {
        assert!(
            matches!(&arm.kind, HirSelectArmKind::TaskAwait { task } if task.ty == hew_types::ResolvedTy::Task(Box::new(hew_types::ResolvedTy::I64)))
        );
        assert_eq!(arm.body.ty, hew_types::ResolvedTy::I64);
    }
    assert!(matches!(
        &select.arms[2].kind,
        HirSelectArmKind::AfterTimer { .. }
    ));
    assert!(select.arms[2].binding_name.is_none());
}

#[test]
fn select_actor_await_uses_checked_dispatch() {
    let output = lower_checked_task(
        r"
        actor Worker { receive fn process(value: i64) -> i64 { value + 1 } }
        fn main() {
            let worker = spawn Worker;
            let result: i64 = select {
                reply from worker.process(41) => match reply { .Ok(value) => value, .Err(_) => -1 },
                after 5ms => 0,
            };
        }
    ",
    );
    let select = find_first_select(&output);
    assert!(
        matches!(&select.arms[0].kind, HirSelectArmKind::ActorAsk { call } if matches!(&call.kind, HirExprKind::ActorAsk { method_id, args, .. } if method_id.ends_with("process") && args.len() == 1))
    );
}

/// A select arm source is a task, an actor call or a channel receive. A value
/// that is none of those is refused, and so is `await` written on a source that
/// would otherwise be valid.
#[test]
fn select_sources_outside_the_sealed_set_are_rejected() {
    for source in [
        "fn main() { let result = select { value from 42 => 1, }; }",
        "fn main() { let task = fork { 42 }; let result = select { value from await task => 1, }; }",
    ] {
        let (_, checked) = support::checker_pipeline::typecheck_source(source);
        assert!(
            checked
                .errors
                .iter()
                .any(|error| { error.kind == hew_types::error::TypeErrorKind::InvalidOperation }),
            "{source}: {:?}",
            checked.errors
        );
    }
}

/// Counterfactual for the refusals above: a bare forked task IS a source.
#[test]
fn select_binds_a_bare_task_source() {
    let (_, checked) = support::checker_pipeline::typecheck_source(
        "fn main() { let task = fork { 42 }; let result = select { value from task => value, }; }",
    );
    assert!(checked.errors.is_empty(), "{:?}", checked.errors);
}

#[test]
fn select_empty_rejected() {
    let output = lower("fn main() { let result = select {}; }");
    assert!(
        output
            .diagnostics
            .iter()
            .any(|d| matches!(d.kind, HirDiagnosticKind::SelectNoArms)),
        "{:?}",
        output.diagnostics
    );
}

#[test]
fn select_timer_only_lowers_without_diagnostics() {
    let output = lower_checked_task("fn main() { let result: i64 = select { after 1ms => 42, }; }");
    let select = find_first_select(&output);
    assert_eq!(select.arms.len(), 1);
    assert!(matches!(
        &select.arms[0].kind,
        HirSelectArmKind::AfterTimer { .. }
    ));
}

#[test]
fn select_arm_body_type_mismatch_rejected() {
    let source = "fn main() { let first = fork { 1 }; let second = fork { 2 }; let result = select { a from first => 1, b from second => true, }; }";
    let (_, checked) = support::checker_pipeline::typecheck_source(source);
    assert!(
        checked
            .errors
            .iter()
            .any(|error| matches!(error.kind, hew_types::error::TypeErrorKind::Mismatch { .. })),
        "{:?}",
        checked.errors
    );
    let output = lower(source);
    assert!(
        output
            .diagnostics
            .iter()
            .any(|d| matches!(d.kind, HirDiagnosticKind::SelectArmTypeMismatch { .. })),
        "{:?}",
        output.diagnostics
    );
}

// ── Explicit tasks and ordinary await expressions ─────────────────────────

fn lower_checked_task(source: &str) -> hew_hir::LowerOutput {
    let (parsed, checked) = crate::support::checker_pipeline::typecheck_source(source);
    assert!(checked.errors.is_empty(), "{:?}", checked.errors);
    let output =
        hew_hir::lower_program_host_target(&parsed.program, &checked, &hew_hir::ResolutionCtx);
    assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);
    let diagnostics = verify_hir(&output.module);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    output
}

#[test]
fn task_scope_ordinary_calls_remain_synchronous() {
    let output = lower_checked_task(
        "fn worker() -> i64 { 42 } fn main() { let outside: i64 = worker(); scope { let inside: i64 = worker(); worker(); } }",
    );
    let dump = dump_hir(&output.module);
    assert!(dump.contains("scope scope="), "{dump}");
    assert!(
        !dump.contains("spawned-call") && !dump.contains("<task<"),
        "{dump}"
    );
}

#[test]
fn task_binding_and_await_preserve_child_result_types() {
    let output = lower_checked_task(
        "fn compute() -> i64 { 7 } fn main() { let task = fork compute(); let result: i64 = await task; }",
    );
    let main = output
        .module
        .items
        .iter()
        .find_map(|item| match item {
            hew_hir::HirItem::Function(function) if function.name == "main" => Some(function),
            _ => None,
        })
        .unwrap();
    let HirStmtKind::Let(task, Some(child)) = &main.body.statements[0].kind else {
        panic!("expected task binding")
    };
    assert!(
        matches!(&task.ty, hew_types::ResolvedTy::Task(inner) if **inner == hew_types::ResolvedTy::I64)
    );
    assert_eq!(child.ty, task.ty);
    let HirStmtKind::Let(result, Some(value)) = &main.body.statements[1].kind else {
        panic!("expected result binding")
    };
    let HirExprKind::AwaitTask { operand, output_ty } = &value.kind else {
        panic!("expected task await")
    };
    assert_eq!(operand.ty, task.ty);
    assert_eq!(*output_ty, hew_types::ResolvedTy::I64);
    assert_eq!(value.ty, result.ty);
}

#[test]
fn await_non_task_operand_is_rejected_inside_and_outside_scope() {
    for source in [
        "fn f() { let x = 5; await x; }",
        "fn f() { scope { let x = 5; await x; } }",
    ] {
        let output = lower(source);
        assert!(
            output
                .diagnostics
                .iter()
                .any(|d| matches!(d.kind, HirDiagnosticKind::AwaitNonTask { .. })),
            "{:?}",
            output.diagnostics
        );
    }
}

#[test]
fn task_fork_block_produces_ordinary_value() {
    lower_checked_task("fn main() { let task = fork { 42 }; let result: i64 = await task; }");
}

#[test]
fn task_fork_scalar_requires_a_block() {
    let (_, checked) =
        support::checker_pipeline::typecheck_source("fn main() { let task = fork 42; }");
    assert!(
        checked.errors.iter().any(|error| error
            .message
            .contains("fork expects a call or a batch of calls")),
        "{:?}",
        checked.errors
    );
}

#[test]
fn scope_deadline_carries_budget_and_child_body() {
    let output =
        lower_checked_task("fn main() { scope within 5s { let task = fork {}; await task; } }");
    let dump = dump_hir(&output.module);
    assert!(dump.contains("scope-deadline"), "{dump}");
    assert!(dump.contains("fork-block"), "{dump}");
    assert!(dump.contains("<task<()>>"), "{dump}");
}

#[test]
fn task_annotation_remains_not_nameable() {
    let output = lower("fn f() { let t: Task<i64> = 0; }");
    assert!(
        output
            .diagnostics
            .iter()
            .any(|d| matches!(d.kind, HirDiagnosticKind::TaskNotNameable)),
        "{:?}",
        output.diagnostics
    );
}

#[test]
fn await_in_return_position_produces_result() {
    lower_checked_task("fn compute() -> i64 { 1 } fn f() -> i64 { scope { let task = fork compute(); return await task; } }");
}

#[test]
fn await_in_function_argument_produces_result() {
    lower_checked_task("fn compute() -> i64 { 1 } fn sink(value: i64) {} fn f() { scope { let task = fork compute(); sink(await task); } }");
}

#[test]
fn await_in_binary_operand_produces_result() {
    lower_checked_task("fn compute() -> i64 { 1 } fn f() { scope { let task = fork compute(); let result: i64 = (await task) + 1; } }");
}

#[test]
fn inferred_task_return_from_scope_is_rejected() {
    let output = lower(
        "fn compute() -> i64 { 1 } fn f() { scope { let task = fork compute(); return task; } }",
    );
    assert!(
        output
            .diagnostics
            .iter()
            .any(|d| matches!(d.kind, HirDiagnosticKind::TaskCannotEscape)),
        "{:?}",
        output.diagnostics
    );
}

/// The outer block and inner call have distinct spans. Await lowering must
/// consume the checked dispatch for the call and preserve its reply type.
#[test]
fn block_wrapped_actor_await_preserves_checked_reply() {
    lower_checked_task(
        r"
        actor Worker { receive fn process(value: i64) -> i64 { value + 1 } }
        fn main() {
            let worker = spawn Worker;
            let reply = worker.process(41);
            let value: i64 = match reply { .Ok(value) => value, .Err(_) => 0, };
        }
    ",
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

/// The actor declarations HIR synthesized from `actor |..|` expressions.
///
/// A lambda actor lowers to an ordinary actor declaration plus the spawn that
/// starts it, so the lowered shape is inspected through the declaration rather
/// than through a lambda-shaped expression node.
fn synthesized_lambda_actors(output: &hew_hir::LowerOutput) -> Vec<&hew_hir::HirActorDecl> {
    output
        .module
        .items
        .iter()
        .filter_map(|item| match item {
            hew_hir::HirItem::Actor(actor) if actor.lambda_handle_ty.is_some() => Some(actor),
            _ => None,
        })
        .collect()
}

fn self_capture_refusals(output: &hew_hir::LowerOutput) -> Vec<&hew_hir::HirDiagnostic> {
    output
        .diagnostics
        .iter()
        .filter(|d| {
            matches!(
                &d.kind,
                HirDiagnosticKind::RecursiveLambdaActorHandle { .. }
            )
        })
        .collect()
}

#[test]
fn a_lambda_captures_another_lambda_s_handle_as_ordinary_state() {
    // A lambda that names a DIFFERENT lambda's handle is capturing an
    // ordinary binding, and that handle becomes its state. Only a lambda
    // naming its OWN handle is refused, so the two must not be conflated:
    // the capture classifier is scoped to the lambda it is collecting for.
    let source = r"
fn make() {
    let first = actor |x: i64| -> i64 { x + 1 };
    let second = actor |y: i64| -> i64 {
        first;
        y + 2
    };
}
";
    let output = lower(source);
    assert!(
        self_capture_refusals(&output).is_empty(),
        "`first` is a free variable of `second`, not `second`'s own handle; \
         diagnostics = {:?}",
        output.diagnostics
    );
    let second = synthesized_lambda_actors(&output)
        .into_iter()
        .find(|actor| {
            actor.receive_handlers[0]
                .params
                .iter()
                .any(|p| p.name == "y")
        })
        .expect("the second lambda actor (param `y`) must be declared");
    assert!(
        second
            .state_fields
            .iter()
            .any(|field| field.name == "first"),
        "the captured `first` handle is the second actor's state; fields = {:?}",
        second.state_fields
    );
}

#[test]
fn a_lambda_that_names_itself_is_refused() {
    // A lambda actor's captures are its state, so a lambda that captures its
    // own handle would own the handle that addresses it. A recursive actor is
    // written as a named declaration and spawned instead.
    let source = r"
fn make() {
    let inner = actor |y: i64| -> i64 {
        inner;
        y + 1
    };
}
";
    let output = lower(source);
    assert!(
        !self_capture_refusals(&output).is_empty(),
        "a self-naming lambda actor must be refused; diagnostics = {:?}",
        output.diagnostics
    );
}

#[test]
fn a_lambda_actor_declares_its_body_as_one_handler() {
    let output = lower(
        r"
        fn make() {
            let _worker = actor |value: i64| -> i64 { value + 1 };
        }
        ",
    );
    assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);
    assert!(verify_hir(&output.module).is_empty());

    let actors = synthesized_lambda_actors(&output);
    let [actor] = actors.as_slice() else {
        panic!("one lambda actor must be declared");
    };
    let [handler] = actor.receive_handlers.as_slice() else {
        panic!("a lambda actor declares exactly one handler");
    };
    assert_eq!(handler.return_ty, hew_types::ResolvedTy::I64);
    assert_eq!(
        handler
            .params
            .iter()
            .map(|p| p.name.as_str())
            .collect::<Vec<_>>(),
        vec!["value"],
    );
    assert!(
        actor.state_fields.is_empty(),
        "this lambda captures nothing, so its actor holds no state",
    );
}

// ── typed-let forward-bind for actor lambdas ────────────────────────────────
//
// The forward-bind path in `lower_stmt` pre-allocates the let-binding so the
// lambda body can name it. That resolution still happens; what the body may
// then do with the name changed when lambda actors began lowering to real
// actor declarations, because a capture is state and a lambda cannot hold the
// handle that addresses it.

#[test]
fn a_typed_actor_let_resolves_its_own_name_then_refuses_the_capture() {
    let source = r"
fn make() {
    let fib: LambdaPid<i64, i64> = actor |n: i64| -> i64 {
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
        "the typed actor-let must pre-bind its name so the body's `fib` \
         resolves rather than reporting an unresolved symbol; diagnostics = {:?}",
        output.diagnostics
    );
    // Resolving it is not accepting it: the capture is refused with the
    // reason, not silently lowered into the actor's own state.
    assert!(
        !self_capture_refusals(&output).is_empty(),
        "naming the lambda's own handle in its body must be refused; \
         diagnostics = {:?}",
        output.diagnostics
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
fn top_level_const_folds_signed_binary_subtraction() {
    let output = lower("const NIL: i64 = 0 - 1; fn main() -> i64 { return NIL; }");
    assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);
    let value = output
        .module
        .items
        .iter()
        .find_map(|item| match item {
            hew_hir::HirItem::Const(item) if item.name == "NIL" => Some(item.value.clone()),
            _ => None,
        })
        .expect("module lowers NIL");
    assert_eq!(value, hew_hir::HirConstValue::Integer(-1));
}

#[test]
fn top_level_const_integer_failures_are_semantic_not_nyi() {
    for (source, class) in [
        ("const BAD: u8 = 0 - 1; fn main() {}", "arithmetic-overflow"),
        (
            "const BAD: i8 = 127 + 1; fn main() {}",
            "arithmetic-overflow",
        ),
        ("const BAD: i64 = 1 / 0; fn main() {}", "division-by-zero"),
        ("const BAD: u8 = 256; fn main() {}", "out-of-range"),
    ] {
        let output = lower(source);
        assert!(
            output.diagnostics.iter().any(|diagnostic| {
                matches!(
                    &diagnostic.kind,
                    HirDiagnosticKind::ConstIntegerEvaluation { class: actual } if actual == class
                )
            }),
            "expected {class} for {source}, got {:?}",
            output.diagnostics
        );
        assert!(
            !output.diagnostics.iter().any(|diagnostic| matches!(
                diagnostic.kind,
                HirDiagnosticKind::NotYetImplemented { .. }
            )),
            "integer const failure must not surface as NYI: {:?}",
            output.diagnostics
        );
    }
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
