//! Checker capture facts must authorize transfers into explicit child tasks.

use crate::support;

use hew_hir::{lower_program_host_target, HirDiagnosticKind, ResolutionCtx};

#[test]
fn forked_closure_accepts_checked_send_capture() {
    let (parsed, tco) = support::checker_pipeline::typecheck_source(
        r"
        fn main() {
            let k: i64 = 1;
            let task = fork (|| k)();
            let value: i64 = await task;
        }
        ",
    );
    assert!(tco.errors.is_empty(), "{:?}", tco.errors);
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
    assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);
    let diagnostics = hew_hir::verify::verify_hir(&output.module);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
}

#[test]
fn forked_parameterized_closure_accepts_checked_send_capture() {
    let (parsed, tco) = support::checker_pipeline::typecheck_source(
        r"
        fn main() {
            let k: i64 = 1;
            let task = fork (|n: i64| n + k)(41);
            let value: i64 = await task;
        }
        ",
    );
    assert!(tco.errors.is_empty(), "{:?}", tco.errors);
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
    assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);
    let diagnostics = hew_hir::verify::verify_hir(&output.module);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
}

#[test]
fn forked_closure_rejects_checked_non_send_capture() {
    let source = r"
        fn main() {
            let r = Rc.new(1);
            scope { let task = fork (move || { let _ = r; })(); };
        }
        ";
    let (_, tco) = support::checker_pipeline::typecheck_source(source);
    assert!(
        tco.errors
            .iter()
            .any(|error| matches!(error.kind, hew_types::error::TypeErrorKind::InvalidSend)),
        "non-Send child capture must be rejected by the checker: {:?}",
        tco.errors
    );
}

#[test]
fn missing_capture_facts_for_forked_closure_emit_boundary_diagnostics() {
    let source = r"
        fn main() {
            let k: i64 = 1;
            scope {
                let child = fork (move || { let _ = k; })();
            }
        }
    ";
    let (parsed, mut tco) = support::checker_pipeline::typecheck_source(source);
    assert!(
        tco.errors.is_empty(),
        "forked closure fixture must typecheck cleanly: {:#?}",
        tco.errors
    );
    assert_eq!(
        tco.closure_capture_facts.len(),
        1,
        "test setup requires one checker-produced closure capture entry: {:#?}",
        tco.closure_capture_facts
    );
    tco.closure_capture_facts.clear();

    let output = lower_program_host_target(&parsed.program, &tco, &ResolutionCtx);
    let boundary_count = output
        .diagnostics
        .iter()
        .filter(|diagnostic| {
            matches!(
                &diagnostic.kind,
                HirDiagnosticKind::CheckerBoundaryViolation { name, reason }
                    if name == "closure literal"
                        && reason == "closure_capture_facts has no record for closure literal span"
            )
        })
        .count();
    assert!(
        boundary_count > 0,
        "lowering must reject missing capture facts; got {:#?}",
        output.diagnostics
    );
    assert!(
        output.into_result().is_err(),
        "missing spawned-closure capture facts must make lowering fatal"
    );
}
