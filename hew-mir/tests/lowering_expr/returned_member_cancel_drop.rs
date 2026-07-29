use std::collections::HashSet;

use hew_mir::{lower_hir_module, DropKind, ExitPath, IrPipeline};
use hew_types::{module_registry::ModuleRegistry, Checker};

const SOURCE: &str = r#"
actor Driver {
    receive fn choose(take_x: bool) -> Result<string, string> {
        let x = f"x={1}";
        let y = f"y={2}";
        var keep = take_x;
        while keep {
            keep = false;
        }
        if take_x { Err(x) } else { Ok(y) }
    }
}

fn main() {
    let d = spawn Driver;
    let _ = await d.choose(true);
}
"#;

fn pipeline() -> IrPipeline {
    let parsed = hew_parser::parse(SOURCE);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    assert!(
        tc_output.errors.is_empty(),
        "checker errors: {:#?}",
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
        "HIR diagnostics: {:#?}",
        hir.diagnostics
    );
    lower_hir_module(&hir.module)
}

fn is_string_drop(drop: &hew_mir::ElabDrop) -> bool {
    matches!(
        drop.kind,
        DropKind::CowHeap { release } if release.release_symbol() == "hew_string_drop"
    )
}

#[test]
fn returned_members_drop_on_loop_cancellation_with_their_normal_path_owners() {
    let pipeline = pipeline();
    assert!(
        pipeline.diagnostics.is_empty(),
        "MIR diagnostics: {:#?}",
        pipeline.diagnostics
    );
    let choose = pipeline
        .elaborated_mir
        .iter()
        .find(|function| function.name == "Driver__recv__choose")
        .expect("receive handler must be lowered");

    let cancellation_owners: HashSet<_> = choose
        .drop_plans
        .iter()
        .filter(|(exit, _)| matches!(exit, ExitPath::Cancel { block } if *block != 0))
        .flat_map(|(_, plan)| plan.drops.iter())
        .filter(|drop| is_string_drop(drop))
        .map(|drop| drop.place)
        .collect();
    let normal_path_owners: HashSet<_> = choose
        .drop_plans
        .iter()
        .filter(|(exit, _)| matches!(exit, ExitPath::Goto { .. }))
        .flat_map(|(_, plan)| plan.drops.iter())
        .filter(|drop| is_string_drop(drop))
        .map(|drop| drop.place)
        .collect();

    assert_eq!(
        cancellation_owners.len(),
        2,
        "loop cancellation must release both still-owned returned members: {:#?}",
        choose.drop_plans
    );
    assert_eq!(
        cancellation_owners, normal_path_owners,
        "cancellation must release the exact owners released by the divergent normal paths"
    );
    assert!(
        choose
            .drop_plans
            .iter()
            .filter(|(exit, _)| matches!(exit, ExitPath::Return { .. }))
            .flat_map(|(_, plan)| plan.drops.iter())
            .all(|drop| !is_string_drop(drop)),
        "the completed return transfers both members to the caller: {:#?}",
        choose.drop_plans
    );
}
