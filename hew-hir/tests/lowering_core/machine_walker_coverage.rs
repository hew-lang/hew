//! Machine normalization must preserve purity checks at every evaluator position.

use crate::support;

fn machine_source(entry: &str, exit: &str, guard: &str, body: &str, helper: &str) -> String {
    format!(
        r"
        {helper}
        machine M {{
            events {{ Tick, }}
            state Idle {{ entry {{ {entry} }} exit {{ {exit} }} }},
            on Tick: Idle => Idle reenter when {guard} {{ {body} .Idle }}
            default {{ state }}
        }}
        fn main() {{ var state: M = .Idle; let report = state.step(.Tick); }}
    "
    )
}

#[test]
fn machine_rejects_effectful_helpers_in_every_evaluator_position() {
    for (entry, exit, guard, body) in [
        ("helper();", "", "true", ""),
        ("", "helper();", "true", ""),
        ("", "", "helper()", ""),
        ("", "", "true", "helper();"),
    ] {
        let source = machine_source(
            entry,
            exit,
            guard,
            body,
            r#"fn helper() -> bool { println("effect"); true }"#,
        );
        let (_, checked) = support::checker_pipeline::typecheck_source(&source);
        assert!(
            !checked.errors.is_empty(),
            "effectful machine accepted: {source}"
        );
        assert!(
            checked
                .errors
                .iter()
                .all(|error| error.message.contains("not demonstrably pure")),
            "fixture must fail for machine purity: {:?}\n{source}",
            checked.errors
        );
    }
}

#[test]
fn pure_machine_positions_lower_through_normalized_hir() {
    let source = machine_source(
        "helper();",
        "helper();",
        "helper()",
        "helper();",
        "fn helper() -> bool { 1 + 1 == 2 }",
    );
    let (parsed, checked) = support::checker_pipeline::typecheck_source(&source);
    assert!(checked.errors.is_empty(), "{:?}", checked.errors);
    let output =
        hew_hir::lower_program_host_target(&parsed.program, &checked, &hew_hir::ResolutionCtx);
    assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);
    let diagnostics = hew_hir::verify::verify_hir(&output.module);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
}
