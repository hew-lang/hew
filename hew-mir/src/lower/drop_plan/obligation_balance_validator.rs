//! S1 obligation-balance unit fixtures — the plan's red-first validation
//! candidates as hand-constructed MIR (the `validate_drop_plan_*` /
//! `drop_kind_for_*` unit style). Each accept/reject decision below is a
//! pin: the branch-around leak (S1886 round-4 shape), the `move_out_arm`
//! double-free (S1882), and the transfer negative controls proving the
//! pass does not re-introduce move-checker liveness rejection (A278 /
//! S1875).
use super::*;
use crate::model::CallAuthority;

fn ret_ty() -> ResolvedTy {
    ResolvedTy::Unit
}

fn elab_with_plans(plans: Vec<(ExitPath, DropPlan)>) -> ElaboratedMirFunction {
    ElaboratedMirFunction {
        key: crate::model::MirCallableKey::for_test("obligation_fixture"),
        name: "obligation_fixture".to_string(),
        return_ty: ret_ty(),
        statements: Vec::new(),
        decisions: Vec::new(),
        blocks: Vec::new(),
        drop_plans: plans,
        coroutine: None,
        lambda_captures: Vec::new(),
    }
}

fn block(id: u32, instructions: Vec<Instr>, terminator: Terminator) -> BasicBlock {
    BasicBlock {
        id,
        statements: Vec::new(),
        instructions,
        terminator,
    }
}

fn plain_drop(place: Place) -> ElabDrop {
    ElabDrop {
        place,
        ty: ResolvedTy::String,
        drop_fn: None,
        kind: DropKind::Resource,
        guard: None,
    }
}

#[test]
fn unwind_cleanup_coverage_requires_exact_call_siblings() {
    let blocks = vec![block(
        0,
        vec![],
        Terminator::Call {
            callee: "may_fail".to_string(),
            authority: CallAuthority::default(),
            args: vec![],
            dest: None,
            next: 1,
        },
    )];
    let complete = elab_with_plans(vec![
        (
            ExitPath::Call {
                block: 0,
                callee: "may_fail".to_string(),
                next: 1,
            },
            DropPlan::default(),
        ),
        (
            ExitPath::Unwind {
                block: 0,
                callee: "may_fail".to_string(),
            },
            DropPlan {
                drops: vec![plain_drop(Place::Local(2))],
            },
        ),
    ]);
    assert!(validate_unwind_cleanup_coverage_over(&complete, &blocks).is_empty());

    let missing = elab_with_plans(vec![(
        ExitPath::Call {
            block: 0,
            callee: "may_fail".to_string(),
            next: 1,
        },
        DropPlan::default(),
    )]);
    assert!(matches!(
        validate_unwind_cleanup_coverage_over(&missing, &blocks).as_slice(),
        [MirCheck::ObligationBalanceUnverified { .. }]
    ));
}

#[test]
fn unwind_cleanup_coverage_rejects_duplicate_destroy() {
    let blocks = vec![block(
        0,
        vec![],
        Terminator::Call {
            callee: "may_fail".to_string(),
            authority: CallAuthority::default(),
            args: vec![],
            dest: None,
            next: 1,
        },
    )];
    let drop = plain_drop(Place::Local(2));
    let elab = elab_with_plans(vec![
        (
            ExitPath::Call {
                block: 0,
                callee: "may_fail".to_string(),
                next: 1,
            },
            DropPlan::default(),
        ),
        (
            ExitPath::Unwind {
                block: 0,
                callee: "may_fail".to_string(),
            },
            DropPlan {
                drops: vec![drop.clone(), drop],
            },
        ),
    ]);
    assert!(matches!(
        validate_unwind_cleanup_coverage_over(&elab, &blocks).as_slice(),
        [MirCheck::ObligationBalanceUnverified { .. }]
    ));
}

fn variant_place(local: u32) -> Place {
    Place::EnumVariant {
        local,
        variant_idx: 0,
        field_idx: 0,
    }
}

use crate::model::NeutralizeAuthority;

#[test]
fn discharge_authority_missing_fails_closed() {
    // A SendTransferLastUse neutralize structurally owns a destination, so a
    // `transferee: None` is a fact-erased site — reject fail-closed.
    let blocks = vec![block(
        0,
        vec![Instr::NeutralizePayloadSlot {
            place: variant_place(1),
            transferee: None,
            authority: NeutralizeAuthority::SendTransferLastUse,
        }],
        Terminator::Return,
    )];
    let findings = validate_discharge_authority_over("f", &blocks);
    assert!(
        matches!(
            findings.as_slice(),
            [MirCheck::DischargeAuthorityMissing {
                authority: NeutralizeAuthority::SendTransferLastUse,
                ..
            }]
        ),
        "a requires-transferee authority with no transferee must fail closed, got {findings:?}"
    );
}

#[test]
fn discharge_authority_missing_allows_move_out_arm_without_transferee() {
    // A MoveOutArmConsume neutralize consumes into an in-flight expression
    // with no destination local, so `transferee: None` is legitimate.
    let blocks = vec![block(
        0,
        vec![Instr::NeutralizePayloadSlot {
            place: variant_place(1),
            transferee: None,
            authority: NeutralizeAuthority::MoveOutArmConsume,
        }],
        Terminator::Return,
    )];
    assert!(
        validate_discharge_authority_over("f", &blocks).is_empty(),
        "a move-out-arm authority does not structurally require a transferee"
    );
}

#[test]
fn explicit_owner_transfer_flags_fabricated_source() {
    let owner = crate::model::OwnerId {
        binding: BindingId(90),
        generation: 0,
    };
    let checked = checked_with_ownership_events(vec![
        Instr::OwnershipEvent(crate::model::OwnershipEvent::Mint {
            owner,
            place: Place::Local(1),
            ty: ResolvedTy::String,
        }),
        Instr::OwnershipEvent(crate::model::OwnershipEvent::Transfer {
            owner,
            from: Place::Local(8),
            to: Some(Place::Local(9)),
            to_owner: None,
            to_ty: None,
        }),
    ]);
    assert!(matches!(
        validate_ownership_events(&checked).as_slice(),
        [MirCheck::DischargeAuthorityDrift { .. }]
    ));
}

/// The unverified verdict is a hard diagnostic with NO allowlist escape:
/// `project_findings` must upgrade it so the CLI rejects the program.
#[test]
fn unverified_balance_upgrades_to_hard_diagnostic() {
    let check = MirCheck::ObligationBalanceUnverified {
        function: "f".to_string(),
        reason: "cap exhausted".to_string(),
    };
    assert!(
        !super::diagnostic_projection::project_findings(std::slice::from_ref(&check)).is_empty(),
        "an unverified balance verdict must surface as a hard diagnostic"
    );
}
