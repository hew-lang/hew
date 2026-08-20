//! Regression coverage for the assignment-target `UnresolvedPlace` cascade.
//!
//! The original reproducer reached MIR with a failed initializer and then
//! emitted a second diagnostic when the binding was reassigned. The complete
//! typed ownership path now lowers the initializer, assignment, and read, so
//! this test pins the stronger clean result.

use hew_mir::{lower_hir_module, IrPipeline, MirDiagnosticKind};
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;

fn pipeline_with_tc(source: &str) -> IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    assert!(
        tc_output.errors.is_empty(),
        "type-check errors: {:#?}",
        tc_output.errors
    );
    let hir = hew_hir::lower_program(
        &parsed.program,
        &tc_output,
        &hew_hir::ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        hir.diagnostics.is_empty(),
        "HIR diagnostics must be empty for this reproducer: {:#?}",
        hir.diagnostics
    );
    lower_hir_module(&hir.module)
}

/// A `var` initialized from a `?`-scrutinee call that forwards an owned
/// aggregate can be reassigned without poisoning the binding or losing its
/// release authority.
#[test]
fn forwarded_var_reassignment_lowers_without_diagnostics() {
    let pipeline = pipeline_with_tc(
        r"
        fn passthru(v: Vec<string>) -> Result<Vec<string>, string> { Ok(v) }
        fn caller(v: Vec<string>) -> Result<i64, string> {
            var left = passthru(v)?;
            let fresh: Vec<string> = Vec.new();
            left = fresh;
            Ok(left.len())
        }
        ",
    );

    assert!(
        pipeline.diagnostics.is_empty(),
        "the complete ownership path must lower cleanly: {:#?}",
        pipeline.diagnostics
    );
    assert!(
        !pipeline
            .diagnostics
            .iter()
            .any(|d| matches!(d.kind, MirDiagnosticKind::UnresolvedPlace { .. })),
        "the reassigned binding must remain resolved: {:#?}",
        pipeline.diagnostics
    );
}
