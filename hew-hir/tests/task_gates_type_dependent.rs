//! Checker-pipeline coverage for task gates whose predicates read checker
//! side tables. Adjacent `TypeCheckOutput::default()` debt outside
//! `task_gates.rs` is intentionally left to the follow-up lane.
//!
//! These tests pin the reachable checker facts plus the task-gate predicates
//! that consume them without changing production behavior. They predate the
//! checker accepting `fork name = call(...)` and remain the direct-predicate
//! coverage for facts end-to-end source cannot isolate.

#[path = "support/mod.rs"]
mod support;

use hew_hir::{lower_program_host_target, HirDiagnostic, HirDiagnosticKind, ResolutionCtx};

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
fn checker_pipeline_rc_capture_emits_non_send_diagnostic() {
    let source = r"
        fn main() {
            let r = Rc::new(1);
            scope { (move || { let _ = r; })(); };
        }
        ";
    let (parsed, tco) = support::checker_pipeline::typecheck_source(source);
    assert!(
        tco.errors.is_empty(),
        "Rc capture fixture must typecheck cleanly: {:#?}",
        tco.errors
    );

    let captures: Vec<_> = tco
        .closure_capture_facts
        .values()
        .flat_map(|facts| facts.iter())
        .filter(|fact| fact.name == "r")
        .collect();
    assert!(
        !captures.is_empty(),
        "checker must produce capture facts for `r`: {:#?}",
        tco.closure_capture_facts
    );
    assert!(
        captures.iter().any(|fact| !fact.is_send),
        "`Rc<i64>` capture must be recorded as non-Send: {captures:#?}"
    );

    let output = lower_program_host_target(&parsed.program, &tco, &ResolutionCtx);
    let capture_names = non_send_capture_names(&output.diagnostics);
    assert_eq!(
        capture_names,
        vec!["r"],
        "SpawnedClosureNonSendCapture must report the captured binding name: {:#?}",
        output.diagnostics
    );
    assert!(
        output.into_result().is_err(),
        "non-Send spawned closure capture must make lowering fatal"
    );
}
