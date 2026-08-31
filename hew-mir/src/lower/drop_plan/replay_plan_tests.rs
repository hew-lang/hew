//! Behavioural pins for the replay-derived exit plans: every exit's cleanup is
//! exactly the set of owners the Checked-MIR event stream leaves live there,
//! materialized through the owner's definition-site recipe and guard.

use super::*;
use crate::model::{OwnerId, OwnershipEvent, OwnershipGuardKind};

fn owner(binding: u32) -> OwnerId {
    OwnerId {
        binding: BindingId(binding),
        generation: 0,
    }
}

fn string_recipe(declaration_order: u32) -> crate::model::OwnerDropRecipe {
    crate::model::OwnerDropRecipe {
        declaration_order,
        ..checked_test_string_recipe()
    }
}

fn mint(owner: OwnerId, place: Place) -> Instr {
    Instr::OwnershipEvent(OwnershipEvent::Mint {
        owner,
        place,
        ty: ResolvedTy::String,
    })
}

fn recipe(owner: OwnerId, declaration_order: u32) -> Instr {
    Instr::OwnershipEvent(OwnershipEvent::DropRecipe {
        owner,
        recipe: string_recipe(declaration_order),
    })
}

fn release(owner: OwnerId, place: Place) -> Instr {
    Instr::OwnershipEvent(OwnershipEvent::Release { owner, place })
}

/// Every owner minted here also gets a source-level `Bind`, the way a real
/// function's `let` does. Without one the verifier has no value name to
/// report and routes the imbalance to the internal-error channel instead,
/// which is a different rule (see
/// `an_omitted_cleanup_over_an_unnamed_owner_is_an_internal_error`).
fn block(id: u32, instructions: Vec<Instr>, terminator: Terminator) -> BasicBlock {
    let statements = instructions
        .iter()
        .filter_map(|instruction| match instruction {
            Instr::OwnershipEvent(OwnershipEvent::Mint { owner, .. }) => Some(MirStatement::Bind {
                binding: owner.binding,
                name: format!("value{}", owner.binding.0),
                site: SiteId(owner.binding.0),
                ty: ResolvedTy::String,
            }),
            _ => None,
        })
        .collect();
    BasicBlock {
        id,
        statements,
        instructions,
        terminator,
    }
}

fn call_to(next: u32) -> Terminator {
    Terminator::Call {
        callee: "callee".to_owned(),
        authority: crate::model::CallAuthority::Direct,
        args: vec![],
        dest: None,
        next,
    }
}

fn plans_for(blocks: &[BasicBlock]) -> Vec<(ExitPath, DropPlan)> {
    let (_, exits) = enumerate_exits(blocks, &HashSet::new());
    derive_drop_plans_from_replay(blocks, &[], exits)
}

fn plan<'a>(plans: &'a [(ExitPath, DropPlan)], exit: &ExitPath) -> &'a DropPlan {
    &plans
        .iter()
        .find(|(candidate, _)| candidate == exit)
        .unwrap_or_else(|| panic!("no plan for {exit:?} in {plans:?}"))
        .1
}

fn checked(blocks: Vec<BasicBlock>, drop_plans: Vec<(ExitPath, DropPlan)>) -> CheckedMirFunction {
    CheckedMirFunction {
        name: "replay_plan".to_owned(),
        key: crate::model::MirCallableKey::for_test("replay_plan"),
        return_ty: ResolvedTy::Unit,
        blocks,
        decisions: vec![],
        checks: vec![],
        cooperate_sites: vec![],
        ownership_elaboration: Some(Box::new(ElaboratedMirFunction {
            name: "replay_plan".to_owned(),
            key: crate::model::MirCallableKey::for_test("replay_plan"),
            return_ty: ResolvedTy::Unit,
            statements: vec![],
            decisions: vec![],
            blocks: vec![],
            drop_plans,
            coroutine: None,
            lambda_captures: vec![],
        })),
    }
}

fn string_drop(place: Place) -> ElabDrop {
    ElabDrop {
        place,
        ty: ResolvedTy::String,
        drop_fn: Some(crate::model::DropFnSpec::Release("hew_string_drop")),
        kind: DropKind::CowHeap {
            release: crate::ownership::CowHeapRelease::String,
        },
        guard: None,
    }
}

#[test]
fn semantic_unreachable_has_a_normal_block_but_no_exit_or_cleanup_plan() {
    let raw = [block(0, vec![], Terminator::Unreachable)];
    let (elaborated, exits) = enumerate_exits(&raw, &HashSet::new());

    assert_eq!(elaborated.len(), 1);
    assert_eq!(elaborated[0].kind, BlockKind::Normal);
    assert!(elaborated[0].drops.is_empty());
    assert!(
        exits.is_empty(),
        "unreachable must not be reinterpreted as an exit: {exits:?}"
    );
}

#[test]
fn live_owner_is_dropped_on_return_through_its_recipe() {
    let a = owner(1);
    let blocks = [block(
        0,
        vec![mint(a, Place::Local(4)), recipe(a, 0)],
        Terminator::Return,
    )];
    let plans = plans_for(&blocks);
    assert_eq!(
        plan(&plans, &ExitPath::Return { block: 0 }).drops,
        vec![string_drop(Place::Local(4))]
    );
    assert!(validate_ownership_events(&checked(blocks.to_vec(), plans)).is_empty());
}

#[test]
fn released_owner_is_not_dropped_again_on_return() {
    let a = owner(1);
    let blocks = [block(
        0,
        vec![
            mint(a, Place::Local(4)),
            recipe(a, 0),
            release(a, Place::Local(4)),
        ],
        Terminator::Return,
    )];
    let plans = plans_for(&blocks);
    assert!(plan(&plans, &ExitPath::Return { block: 0 })
        .drops
        .is_empty());
}

#[test]
fn owner_transferred_to_the_return_slot_is_not_dropped() {
    let a = owner(1);
    let blocks = [block(
        0,
        vec![
            mint(a, Place::Local(4)),
            recipe(a, 0),
            Instr::OwnershipEvent(OwnershipEvent::Transfer {
                owner: a,
                from: Place::Local(4),
                to: Some(Place::ReturnSlot),
                to_owner: None,
                to_ty: None,
            }),
        ],
        Terminator::Return,
    )];
    let plans = plans_for(&blocks);
    assert!(plan(&plans, &ExitPath::Return { block: 0 })
        .drops
        .is_empty());
}

#[test]
fn unwind_before_inline_release_still_drops_the_owner() {
    let a = owner(1);
    let blocks = [
        block(0, vec![mint(a, Place::Local(4)), recipe(a, 0)], call_to(1)),
        block(1, vec![release(a, Place::Local(4))], Terminator::Return),
    ];
    let plans = plans_for(&blocks);
    let unwind = ExitPath::Unwind {
        block: 0,
        callee: "callee".to_owned(),
    };
    assert_eq!(
        plan(&plans, &unwind).drops,
        vec![string_drop(Place::Local(4))]
    );
    // The normal continuation still owns the value, so the Call exit
    // itself schedules nothing and the later Return sees it released.
    let call = ExitPath::Call {
        block: 0,
        callee: "callee".to_owned(),
        next: 1,
    };
    assert!(plan(&plans, &call).drops.is_empty());
    assert!(plan(&plans, &ExitPath::Return { block: 1 })
        .drops
        .is_empty());
    assert!(validate_ownership_events(&checked(blocks.to_vec(), plans)).is_empty());
}

#[test]
fn drops_run_in_reverse_declaration_order() {
    let first = owner(1);
    let second = owner(2);
    let blocks = [block(
        0,
        vec![
            mint(first, Place::Local(4)),
            recipe(first, 0),
            mint(second, Place::Local(5)),
            recipe(second, 1),
        ],
        Terminator::Return,
    )];
    let plans = plans_for(&blocks);
    assert_eq!(
        plan(&plans, &ExitPath::Return { block: 0 })
            .drops
            .iter()
            .map(|drop| drop.place)
            .collect::<Vec<_>>(),
        vec![Place::Local(5), Place::Local(4)]
    );
}

#[test]
fn published_guard_is_attached_to_the_owner_drop() {
    let a = owner(1);
    let blocks = [block(
        0,
        vec![
            mint(a, Place::Local(4)),
            recipe(a, 0),
            Instr::OwnershipEvent(OwnershipEvent::Guard {
                owner: a,
                flag: Place::Local(9),
                kind: OwnershipGuardKind::AffineRelease,
            }),
        ],
        Terminator::Return,
    )];
    let plans = plans_for(&blocks);
    assert_eq!(
        plan(&plans, &ExitPath::Return { block: 0 }).drops[0].guard,
        Some(crate::model::ElabDropGuard {
            owner: a,
            flag: Place::Local(9)
        })
    );
}

#[test]
fn owner_without_recipe_gets_no_drop_and_one_definition_site_finding() {
    let a = owner(1);
    let blocks = [
        block(0, vec![mint(a, Place::Local(4))], call_to(1)),
        block(1, vec![], Terminator::Return),
    ];
    let plans = plans_for(&blocks);
    assert!(plans.iter().all(|(_, plan)| plan.drops.is_empty()));
    let findings = validate_ownership_events(&checked(blocks.to_vec(), plans));
    let reasons = findings
        .iter()
        .map(|finding| match finding {
            MirCheck::DischargeAuthorityDrift { reason, .. } => reason.clone(),
            other => panic!("unexpected finding {other:?}"),
        })
        .collect::<Vec<_>>();
    // One finding at the definition site, not one per exit edge.
    assert_eq!(
        reasons,
        vec![format!(
            "owner {a} has no definition-site destructor recipe"
        )]
    );
}

#[test]
fn plan_drop_after_owner_release_is_rejected_by_the_verifier() {
    let a = owner(1);
    let blocks = vec![block(
        0,
        vec![
            mint(a, Place::Local(4)),
            recipe(a, 0),
            release(a, Place::Local(4)),
        ],
        Terminator::Return,
    )];
    let stale_plan = vec![(
        ExitPath::Return { block: 0 },
        DropPlan {
            drops: vec![string_drop(Place::Local(4))],
        },
    )];
    let findings = validate_ownership_events(&checked(blocks.clone(), stale_plan));
    // The plan and the replayed stream disagree about what the exit holds,
    // so the defect is the compiler's, not the program's.
    assert!(
        findings.iter().any(|finding| matches!(
            finding,
            MirCheck::DischargeAuthorityDrift { reason, .. }
                if reason.contains(
                    "releases a `string` that the replayed event stream no longer holds there"
                )
        )),
        "a destructor after the owner's Release must be rejected: {findings:?}"
    );
    // The replay-derived plan for the same stream is the accepted form.
    let derived = plans_for(&blocks);
    assert!(validate_ownership_events(&checked(blocks, derived)).is_empty());
}

#[test]
fn plan_omitting_a_live_owner_cleanup_is_rejected_by_the_verifier() {
    let a = owner(1);
    let blocks = vec![block(
        0,
        vec![mint(a, Place::Local(4)), recipe(a, 0)],
        Terminator::Return,
    )];
    let derived = plans_for(&blocks);
    assert_eq!(
        plan(&derived, &ExitPath::Return { block: 0 }).drops.len(),
        1,
        "replay derives exactly one cleanup for the live owner"
    );
    assert!(validate_ownership_events(&checked(blocks.clone(), derived)).is_empty());
    let empty_plan = vec![(ExitPath::Return { block: 0 }, DropPlan { drops: vec![] })];
    let findings = validate_ownership_events(&checked(blocks, empty_plan));
    let omissions = findings
        .iter()
        .filter(|finding| {
            matches!(
                finding,
                MirCheck::ObligationUnderReleased { reason, .. }
                    if reason.contains("the exit plan for the return path omits its cleanup")
            )
        })
        .count();
    assert_eq!(
        omissions, 1,
        "a live recipe-bearing owner with no plan entry must be rejected once: {findings:?}"
    );
}
