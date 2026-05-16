use hew_hir::{
    dump_hir, lower_program, verify_hir, HirDiagnosticKind, HirExprKind, HirSelectArmKind,
    HirStmtKind, ResolutionCtx,
};
use hew_parser::ast::{
    Block, Expr, FnDecl, IntRadix, Item, Literal, Pattern, Program, SelectArm, Stmt, TimeoutClause,
    Visibility,
};
use hew_types::TypeCheckOutput;

fn lower(source: &str) -> hew_hir::LowerOutput {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    lower_program(&parsed.program, &TypeCheckOutput::default(), &ResolutionCtx)
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
            hew_hir::HirItem::TypeDecl(_) | hew_hir::HirItem::Machine(_) => None,
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
    // itself lowers cleanly; MIR-level CutoverUnsupported fires downstream).
    let hir_diags: Vec<_> = output
        .diagnostics
        .iter()
        .filter(|d| {
            !matches!(
                d.kind,
                HirDiagnosticKind::CutoverUnsupported { .. }
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
        dump.contains("<task<int>>"),
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
        .filter(|d| !matches!(d.kind, HirDiagnosticKind::CutoverUnsupported { .. }))
        .collect();
    assert!(
        real_diags.is_empty(),
        "fork name = call() should lower without HIR errors: {real_diags:?}"
    );

    let dump = dump_hir(&output.module);
    // The binding `t` should appear with Task<int> type.
    assert!(
        dump.contains("let") && dump.contains("t:") && dump.contains("<task<int>>"),
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
        .filter(|d| !matches!(d.kind, HirDiagnosticKind::CutoverUnsupported { .. }))
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

/// TI-5 (structural): the `lower_type` path rejects `Task` as a user-written
/// type annotation via `TaskNotNameable` (wired through the `Named` arm for
/// the name "Task"). This test exercises the type-annotation wall.
#[test]
fn task_handle_ti5_task_annotation_in_let_rejects_task_not_nameable() {
    // The parser accepts `let t: Task<int> = 0;` as a named type annotation;
    // HIR lowering must reject it with TaskNotNameable.
    let output = lower("fn f() { let t: Task<int> = 0; }");
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
    let output = lower_program(&program, &TypeCheckOutput::default(), &ResolutionCtx);
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
        HirExprKind::Block(block) | HirExprKind::Scope { body: block } => {
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
        HirExprKind::Call { callee, args } | HirExprKind::SpawnedCall { callee, args, .. } => {
            walk_expr_collect_lambdas(callee, out);
            for a in args {
                walk_expr_collect_lambdas(a, out);
            }
        }
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
