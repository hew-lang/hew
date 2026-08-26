use std::collections::HashSet;

use hew_mir::{
    lower_hir_module, DropFnSpec, DropKind, ExitPath, Instr, IrPipeline, MirStatement,
    OwnershipEvent, Terminator,
};
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
#[expect(
    clippy::too_many_lines,
    reason = "the regression audits one owner across mint, normal, unwind, cancel, and return paths"
)]
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
    let checked = pipeline
        .checked_mir
        .iter()
        .find(|function| function.name == "Driver__recv__resolve")
        .expect("checked receive handler must be present");
    let index_binding = checked
        .blocks
        .iter()
        .flat_map(|block| &block.statements)
        .find_map(|statement| match statement {
            MirStatement::Bind { binding, name, .. } if name == "index" => Some(*binding),
            _ => None,
        })
        .expect("index binding must be present");
    let handoffs = checked
        .blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instruction| match instruction {
            Instr::OwnershipEvent(OwnershipEvent::Transfer {
                to: Some(place),
                to_owner: Some(owner),
                ..
            }) if owner.binding == index_binding => Some((*owner, *place)),
            _ => None,
        })
        .collect::<Vec<_>>();
    let [(index_owner, index_place)] = handoffs.as_slice() else {
        panic!("index must receive exactly one owner handoff: {handoffs:?}");
    };

    let inline_cleanup_blocks = checked
        .blocks
        .iter()
        .filter(|block| matches!(block.terminator, Terminator::Goto { .. }))
        .filter(|block| {
            block.instructions.windows(3).any(|window| {
                matches!(window,
                [Instr::Drop {
                    place: drop_place,
                    drop_fn: Some(DropFnSpec::Release(symbol)),
                    ..
                }, Instr::OwnershipEvent(OwnershipEvent::Release {
                    owner,
                    place: release_place,
                }), Instr::OwnershipEvent(OwnershipEvent::ScopeExit { owners, .. })]
                    if *symbol == "hew_string_drop"
                        && drop_place == index_place
                        && release_place == index_place
                        && owner == index_owner
                        && owners.iter().filter(|candidate| *candidate == index_owner).count() == 1)
            })
        })
        .count();
    assert_eq!(
        inline_cleanup_blocks, 1,
        "the normal Goto path must close index once at its lexical ScopeExit"
    );

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
    let goto_plan_owners: HashSet<_> = resolve
        .drop_plans
        .iter()
        .filter(|(exit, _)| matches!(exit, ExitPath::Goto { .. }))
        .flat_map(|(_, plan)| plan.drops.iter())
        .filter(|drop| is_string_drop(drop))
        .map(|drop| drop.place)
        .collect();

    assert!(
        !goto_plan_owners.contains(index_place),
        "the inline scope close must not be duplicated in a Goto plan: {:#?}",
        resolve.drop_plans
    );
    assert_eq!(
        cancellation_owners
            .iter()
            .filter(|owners| owners.contains(index_place))
            .count(),
        1,
        "the loop before the Goto must still release index when cancellation \
         bypasses its normal release: {:#?}",
        resolve.drop_plans,
    );
    assert!(
        cancellation_owners
            .iter()
            .any(|owners| !owners.is_empty() && !owners.contains(index_place)),
        "the loop after the Goto must not release index again; its normal \
         scope-close already owns that release: {:#?}",
        resolve.drop_plans,
    );
    let unwind_index_drops = resolve
        .drop_plans
        .iter()
        .filter(|(exit, _)| matches!(exit, ExitPath::Unwind { callee, .. } if callee == "exists"))
        .flat_map(|(_, plan)| &plan.drops)
        .filter(|drop| drop.place == *index_place && is_string_drop(drop))
        .count();
    assert_eq!(
        unwind_index_drops, 1,
        "the borrowing exists(index) call must preserve index for its unwind cleanup"
    );
    let return_transfers = checked
        .blocks
        .iter()
        .filter(|block| matches!(block.terminator, Terminator::Return))
        .flat_map(|block| &block.instructions)
        .filter(|instruction| {
            matches!(instruction,
            Instr::OwnershipEvent(OwnershipEvent::Transfer {
                owner,
                from,
                to: None,
                ..
            }) if owner == index_owner && from == index_place)
        })
        .count();
    assert_eq!(
        return_transfers, 1,
        "the early return must transfer the same index owner to the caller"
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
        !returned_owners.contains(index_place),
        "the completed return transfers index to the caller rather than releasing \
         it a second time: {:#?}",
        resolve.drop_plans
    );
}
