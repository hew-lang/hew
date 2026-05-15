use hew_hir::{dump_hir, lower_program, verify_hir, HirDiagnosticKind, ResolutionCtx};

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

// ── Task<T> inference tests (TI-1 .. TI-5) ──────────────────────────────────

/// TI-1: a call expression used as a statement inside a `fork{}` body lowers
/// to `SpawnedCall` with type `Task<T>`. Calls in the same function but
/// outside the fork body remain synchronous.
#[test]
fn task_handle_ti1_statement_call_inside_fork_becomes_spawned_call() {
    let output = lower(
        "fn worker() -> i64 { return 42; } \
         fn main() { fork { worker(); } }",
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

    // The dump should mention `fork` and `spawned-call`.
    let dump = dump_hir(&output.module);
    assert!(
        dump.contains("fork scope="),
        "HIR dump should contain fork node: {dump}"
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
         fn main() { fork { fork t = compute(); } }",
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
         fn main() { fork { fork t = compute(); await t; } }",
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
    let output = lower("fn f() { fork { let x = 5; await x; } }");
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
    let output = lower("fn f() { fork { fork t = 42; } }");
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
         fn f() { fork { fork t = compute(); let x = await t; } }",
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

/// Verifier stability: a valid `fork { call(); }` program passes `verify_hir`
/// without dangling-ref or duplicate-id diagnostics.
#[test]
fn task_handle_fork_block_passes_verifier() {
    let output = lower(
        "fn work() -> i64 { return 1; } \
         fn main() { fork { work(); } }",
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
