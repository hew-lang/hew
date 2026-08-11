use std::collections::HashSet;

use hew_mir::{lower_hir_module, DropKind, ExitPath, IrPipeline};
use hew_types::{module_registry::ModuleRegistry, Checker};

const SOURCE: &str = r#"
fn exists(path: string) -> bool {
    path.len() > 0
}

actor Driver {
    receive fn resolve() -> string {
        let path = f"path";
        if !exists(path) {
            let index = path + "/index.html";
            var before = true;
            while before {
                before = false;
            }
            if exists(index) {
                return index;
            }
        }
        var after = true;
        while after {
            after = false;
        }
        path
    }
}

fn main() {
    let d = spawn Driver;
    let _ = await d.resolve();
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
fn returned_member_cancellation_uses_one_owner_before_and_after_normal_goto() {
    let pipeline = pipeline();
    assert!(
        pipeline.diagnostics.is_empty(),
        "MIR diagnostics: {:#?}",
        pipeline.diagnostics
    );
    let resolve = pipeline
        .elaborated_mir
        .iter()
        .find(|function| function.name == "Driver__recv__resolve")
        .expect("receive handler must be lowered");

    let cancellation_owners: Vec<HashSet<_>> = resolve
        .drop_plans
        .iter()
        .filter(|(exit, _)| matches!(exit, ExitPath::Cancel { block } if *block != 0))
        .map(|(_, plan)| {
            plan.drops
                .iter()
                .filter(|drop| is_string_drop(drop))
                .map(|drop| drop.place)
                .collect()
        })
        .collect();
    let normal_path_owners: HashSet<_> = resolve
        .drop_plans
        .iter()
        .filter(|(exit, _)| matches!(exit, ExitPath::Goto { .. }))
        .flat_map(|(_, plan)| plan.drops.iter())
        .filter(|drop| is_string_drop(drop))
        .map(|drop| drop.place)
        .collect();

    assert_eq!(
        normal_path_owners.len(),
        1,
        "the scope-closing Goto must release index before the following loop: {:#?}",
        resolve.drop_plans,
    );
    assert!(
        cancellation_owners
            .iter()
            .any(|owners| { owners.is_superset(&normal_path_owners) }),
        "the loop before the Goto must still release index when cancellation \
         bypasses its normal release: {:#?}",
        resolve.drop_plans,
    );
    assert!(
        cancellation_owners
            .iter()
            .any(|owners| !owners.is_empty() && owners.is_disjoint(&normal_path_owners)),
        "the loop after the Goto must not release index again; its normal \
         scope-close already owns that release: {:#?}",
        resolve.drop_plans,
    );
    let returned_owners: HashSet<_> = resolve
        .drop_plans
        .iter()
        .filter(|(exit, _)| matches!(exit, ExitPath::Return { .. }))
        .flat_map(|(_, plan)| plan.drops.iter())
        .filter(|drop| is_string_drop(drop))
        .map(|drop| drop.place)
        .collect();
    assert!(
        normal_path_owners.is_disjoint(&returned_owners),
        "the completed return transfers index to the caller rather than releasing \
         it a second time: {:#?}",
        resolve.drop_plans
    );
}
