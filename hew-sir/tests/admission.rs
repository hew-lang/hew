//! Source forms admitted into SIR through existing ownership operations:
//! discarded consumed values, module constants and value clones.

use hew_hir::{lower_program_host_target, ResolutionCtx};
use hew_sir::{lower_module, verify_module, SemFunction, SemOpKind, SirLoweringStatus};
use hew_types::{module_registry::ModuleRegistry, Checker};

fn lower_source(source: &str) -> hew_sir::LoweredModule {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(Vec::new()));
    let facts = checker.check_program(&parsed.program);
    assert!(facts.errors.is_empty(), "type errors: {:#?}", facts.errors);
    let hir = lower_program_host_target(&parsed.program, &facts, &ResolutionCtx);
    assert!(
        hir.diagnostics.is_empty(),
        "HIR errors: {:#?}",
        hir.diagnostics
    );
    lower_module(&hir.module, &facts)
}

fn verified_main(lowered: &hew_sir::LoweredModule) -> &SemFunction {
    assert!(
        matches!(
            lowered.statuses.iter().find(|status| status.name == "main"),
            Some(status) if matches!(status.status, SirLoweringStatus::Lowered)
        ),
        "main must lower: {:#?}",
        lowered.statuses
    );
    assert!(
        verify_module(&lowered.module).is_empty(),
        "source must produce verified SIR: {:#?}",
        verify_module(&lowered.module)
    );
    lowered
        .module
        .functions
        .iter()
        .find(|function| function.declaration.full_path() == "main")
        .expect("main has a body")
}

fn count_ops(function: &SemFunction, matches: impl Fn(&SemOpKind) -> bool) -> usize {
    function
        .blocks
        .iter()
        .flat_map(|block| &block.ops)
        .filter(|op| matches(&op.kind))
        .count()
}

#[test]
fn a_discarded_consumed_value_is_released_where_it_is_dropped() {
    let lowered = lower_source(
        r#"
        fn make() -> string {
            "made"
        }

        fn main() {
            let _ = make();
        }
        "#,
    );
    let main = verified_main(&lowered);
    assert_eq!(
        count_ops(main, |kind| matches!(kind, SemOpKind::DestroyValue { .. })),
        1,
        "the discarded string is destroyed exactly once"
    );
}

#[test]
fn a_module_constant_reads_as_its_folded_literal() {
    let lowered = lower_source(
        r"
        const LIMIT: i64 = 5;

        fn main() -> i64 {
            LIMIT
        }
        ",
    );
    let main = verified_main(&lowered);
    assert_eq!(
        count_ops(main, |kind| matches!(kind, SemOpKind::ConstI64(5))),
        1
    );
}

#[test]
fn record_and_vector_clones_are_semantic_copies() {
    let lowered = lower_source(
        r#"
        type Point { x: i64, name: string }

        fn keep(point: Point) {}
        fn keep_all(values: Vec<i64>) {}

        fn main() {
            let point = Point { x: 1, name: "n" };
            let copy = point.clone();
            var values: Vec<i64> = Vec.new();
            let snapshot = values.clone();
            keep(point);
            keep(copy);
            keep_all(values);
            keep_all(snapshot);
        }
        "#,
    );
    let main = verified_main(&lowered);
    assert_eq!(
        count_ops(main, |kind| matches!(kind, SemOpKind::CopyValue { .. })),
        2
    );
}
