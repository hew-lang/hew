//! Checker-pipeline coverage for task gates whose predicates read checker
//! side tables. Adjacent `TypeCheckOutput::default()` debt outside
//! `task_gates.rs` is intentionally left to the follow-up lane.
//!
//! Current source cannot yet produce all task-gate side-table facts end to end:
//! `fork name = expr` is still a checker-level parser-only surface, and
//! block-wrapped closure calls do not present `Expr::Lambda` at the spawn gate.
//! These tests therefore pin the reachable checker facts plus the task-gate
//! predicates that consume them without changing production behavior.

#[path = "support/mod.rs"]
mod support;

use hew_hir::{lower_program_host_target, HirDiagnostic, HirDiagnosticKind, ResolutionCtx};
use hew_types::{SpanKey, Ty};

fn has_await_non_unit(diagnostics: &[HirDiagnostic]) -> bool {
    diagnostics
        .iter()
        .any(|d| matches!(d.kind, HirDiagnosticKind::AwaitTaskResultUnsupported { .. }))
}

fn non_send_capture_names(diagnostics: &[HirDiagnostic]) -> Vec<&str> {
    diagnostics
        .iter()
        .filter_map(|d| match &d.kind {
            HirDiagnosticKind::SpawnedClosureNonSendCapture { capture_name, .. } => {
                Some(capture_name.as_str())
            }
            _ => None,
        })
        .collect()
}

fn awaited_task_span_key(source: &str) -> SpanKey {
    let await_start = source
        .find("await task")
        .expect("test source must contain `await task`");
    let start = await_start + "await ".len();
    SpanKey {
        start,
        end: start + "task".len(),
    }
}

#[test]
fn await_unit_task_accepted_with_checker_expr_types() {
    let output = support::checker_pipeline::lower_through_checker(
        r"
        fn worker() {}
        fn main() {
            scope {
                fork task = worker();
                await task;
            }
        }
        ",
    );

    assert!(
        !has_await_non_unit(&output.diagnostics),
        "unit task await should not trip AwaitTaskResultUnsupported: {:#?}",
        output.diagnostics
    );
}

#[test]
fn await_non_unit_task_rejected_when_expr_type_is_present() {
    let source = r"
        fn worker() -> i64 { 42 }
        fn main() {
            scope {
                fork task = worker();
                await task;
            }
        }
        ";
    let (parsed, mut tco) = support::checker_pipeline::typecheck_source(source);
    tco.expr_types.insert(
        awaited_task_span_key(source),
        Ty::Named {
            builtin: None,
            name: "Task".to_string(),
            args: vec![Ty::I64],
        },
    );

    let output = lower_program_host_target(&parsed.program, &tco, &ResolutionCtx);

    assert!(
        has_await_non_unit(&output.diagnostics),
        "non-unit task await must trip AwaitTaskResultUnsupported when the checker side table carries Task<i64>: {:#?}",
        output.diagnostics
    );
    assert!(
        output.into_result().is_err(),
        "non-unit task await must make lowering fatal"
    );
}

#[test]
fn checker_pipeline_i64_capture_has_send_facts_and_no_non_send_diagnostic() {
    let (parsed, tco) = support::checker_pipeline::typecheck_source(
        r"
        fn main() {
            let k: i64 = 1;
            let f = || k;
        }
        ",
    );
    let captures: Vec<_> = tco
        .closure_capture_facts
        .values()
        .flat_map(|facts| facts.iter())
        .filter(|fact| fact.name == "k")
        .collect();
    assert!(
        !captures.is_empty(),
        "checker must produce capture facts for `k`: {:#?}",
        tco.closure_capture_facts
    );
    assert!(
        captures.iter().all(|fact| fact.is_send),
        "i64 captures must be Send facts: {captures:#?}"
    );

    let output = lower_program_host_target(&parsed.program, &tco, &ResolutionCtx);
    assert!(
        non_send_capture_names(&output.diagnostics).is_empty(),
        "Send capture must not trip SpawnedClosureNonSendCapture: {:#?}",
        output.diagnostics
    );
}

#[test]
fn checker_pipeline_i64_capture_fact_is_present_for_second_source_shape() {
    let (parsed, tco) = support::checker_pipeline::typecheck_source(
        r"
        fn main() {
            let k: i64 = 1;
            let f = |n: i64| n + k;
        }
        ",
    );
    let captures: Vec<_> = tco
        .closure_capture_facts
        .values()
        .flat_map(|facts| facts.iter())
        .filter(|fact| fact.name == "k")
        .collect();
    assert!(
        !captures.is_empty(),
        "checker must produce capture facts for `k`: {:#?}",
        tco.closure_capture_facts
    );
    assert!(
        captures.iter().all(|fact| fact.is_send),
        "i64 captures must be Send facts: {captures:#?}"
    );

    let output = lower_program_host_target(&parsed.program, &tco, &ResolutionCtx);
    assert!(
        non_send_capture_names(&output.diagnostics).is_empty(),
        "Send capture must not trip SpawnedClosureNonSendCapture: {:#?}",
        output.diagnostics
    );
}

#[test]
fn missing_await_expr_type_silently_accepts_current_behavior() {
    let source = r"
        fn worker() -> i64 { 42 }
        fn main() {
            scope {
                fork task = worker();
                await task;
            }
        }
        ";
    let (parsed, mut tco) = support::checker_pipeline::typecheck_source(source);
    let removed = tco.expr_types.remove(&awaited_task_span_key(source));
    assert!(
        removed.is_some(),
        "test setup must remove the checker-produced awaited expression type"
    );

    let output = lower_program_host_target(&parsed.program, &tco, &ResolutionCtx);
    assert!(
        !has_await_non_unit(&output.diagnostics),
        "missing awaited Task<T> expr type currently silently accepts: {:#?}",
        output.diagnostics
    );
    assert!(
        output.diagnostics.iter().all(|d| {
            !matches!(
                &d.kind,
                HirDiagnosticKind::CheckerBoundaryViolation { name, .. }
                    if name == "await task"
            )
        }),
        "check_await_task_result must not emit a boundary diagnostic today"
    );
}
