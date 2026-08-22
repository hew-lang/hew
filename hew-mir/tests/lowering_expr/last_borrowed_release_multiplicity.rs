use hew_mir::{DropKind, ExitPath, Instr, IrPipeline, MirDiagnosticKind};
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
    let output = hew_hir::lower_program(
        &parsed.program,
        &tc_output,
        &hew_hir::ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    hew_mir::lower_hir_module(&output.module)
}

#[test]
fn looped_last_read_keeps_one_exit_release_for_one_mint() {
    let pipeline = pipeline_with_tc(
        r#"
record Rec { label: string, n: i64 }

fn make(seed: i64) -> Rec {
    Rec { label: "payload".to_upper(), n: seed }
}

fn spin(seed: i64) -> i64 {
    let r = make(seed);
    var i: i64 = 0;
    var total: i64 = 0;
    loop {
        total = total + r.n;
        i = i + 1;
        if i >= 3 { break; }
    }
    total
}
"#,
    );
    assert!(
        pipeline.diagnostics.is_empty(),
        "a looped borrow must not synthesize repeated releases: {:?}",
        pipeline.diagnostics
    );
    let raw = pipeline
        .raw_mir
        .iter()
        .find(|function| function.name == "spin")
        .expect("raw fn spin");
    assert!(
        raw.blocks
            .iter()
            .flat_map(|block| block.instructions.iter())
            .all(|instruction| !matches!(
                instruction,
                Instr::Drop { ty, .. } | Instr::ValueSnapshotDrop { ty, .. }
                    if matches!(ty, hew_types::ResolvedTy::Named { name, .. } if name == "Rec")
            )),
        "the record release must not be inserted after a read that executes on every loop trip"
    );
    let elaborated = pipeline
        .elaborated_mir
        .iter()
        .find(|function| function.name == "spin")
        .expect("elaborated fn spin");
    let record_drops = elaborated
        .drop_plans
        .iter()
        .filter(|(exit, _)| matches!(exit, ExitPath::Return { .. }))
        .flat_map(|(_, plan)| &plan.drops)
        .filter(|drop| matches!(drop.kind, DropKind::RecordInPlace))
        .count();
    assert_eq!(
        record_drops, 1,
        "one mint before the loop must retain one release after the loop"
    );
}

#[test]
fn method_match_carrier_releases_on_early_return() {
    let pipeline = pipeline_with_tc(
        r"
record Rec { label: string }

fn probe(xs: Vec<Rec>) {
    let value = match xs.get(0) {
        Some(value) => value,
        None => return,
    };
    println(value.label);
}
",
    );
    assert!(
        pipeline.diagnostics.is_empty(),
        "an owned method-result carrier must release on every arm exit: {:?}",
        pipeline.diagnostics
    );
}

#[test]
fn fresh_local_match_carrier_releases_after_each_arm() {
    let pipeline = pipeline_with_tc(
        r#"
enum Shape {
    Named(string);
    Tagged(string);
    Empty;
}

fn probe() -> i64 {
    let shape = Shape.Named("owned payload");
    let cloned = clone shape;
    let first = match shape {
        Shape.Named(value) => 1,
        Shape.Tagged(value) => 2,
        Shape.Empty => 0,
    };
    let second = match cloned {
        Shape.Named(value) => 1,
        Shape.Tagged(value) => 2,
        Shape.Empty => 0,
    };
    first + second
}
"#,
    );
    assert!(
        pipeline.diagnostics.iter().all(|diagnostic| !matches!(
            diagnostic.kind,
            MirDiagnosticKind::ObligationUnderReleased { hard: true, .. }
        )),
        "a non-carrier local match must not be promoted to a hard call-carrier failure: {:?}",
        pipeline.diagnostics
    );
}
