//! S1 obligation-balance unit fixtures — the plan's red-first validation
//! candidates as hand-constructed MIR (the `validate_drop_plan_*` /
//! `drop_kind_for_*` unit style). Each accept/reject decision below is a
//! pin: the branch-around leak (S1886 round-4 shape), the `move_out_arm`
//! double-free (S1882), and the transfer negative controls proving the
//! pass does not re-introduce move-checker liveness rejection (A278 /
//! S1875).
use super::*;

fn ret_ty() -> ResolvedTy {
    ResolvedTy::Unit
}

fn elab_with_plans(plans: Vec<(ExitPath, DropPlan)>) -> ElaboratedMirFunction {
    ElaboratedMirFunction {
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

fn mint(local: u32) -> Instr {
    // Any whole-slot write is a mint; the value is irrelevant.
    Instr::ConstI64 {
        dest: Place::Local(local),
        value: 0,
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

fn variant_place(local: u32) -> Place {
    Place::EnumVariant {
        local,
        variant_idx: 0,
        field_idx: 0,
    }
}

#[allow(
    clippy::needless_pass_by_value,
    reason = "fixture helper: taking ownership keeps the call sites free \
              of borrow noise"
)]
fn run(
    blocks: Vec<BasicBlock>,
    plans: Vec<(ExitPath, DropPlan)>,
    tracked: &[(u32, &str)],
) -> Vec<MirCheck> {
    run_with_suspend_kinds(&blocks, plans, tracked, &HashMap::new())
}

fn run_with_suspend_kinds(
    blocks: &[BasicBlock],
    plans: Vec<(ExitPath, DropPlan)>,
    tracked: &[(u32, &str)],
    suspend_kinds: &HashMap<u32, SuspendKind>,
) -> Vec<MirCheck> {
    let elab = elab_with_plans(plans);
    let tracked: BTreeMap<u32, String> = tracked
        .iter()
        .map(|(local, name)| (*local, (*name).to_string()))
        .collect();
    let params = HashSet::new();
    let local_types = BTreeMap::new();
    let mint_sites: BTreeMap<u32, SiteId> = tracked
        .keys()
        .copied()
        .map(|local| (local, SiteId(local)))
        .collect();
    validate_obligation_balance_with(
        &elab,
        blocks,
        suspend_kinds,
        &tracked,
        (&local_types, &mint_sites),
        &params,
    )
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
fn discharge_authority_corroboration_flags_fabricated_transferee() {
    // The neutralize names local 9 as the new owner, but the primitive
    // stream never moves any value into local 9 — the carried transfer fact
    // and the actual routing disagree (dual-carrier drift).
    let blocks = vec![block(
        0,
        vec![Instr::NeutralizePayloadSlot {
            place: variant_place(1),
            transferee: Some(Place::Local(9)),
            authority: NeutralizeAuthority::WholeCarrierConsume,
        }],
        Terminator::Return,
    )];
    let findings = validate_discharge_authority_corroboration_over("f", &blocks);
    assert!(
        matches!(
            findings.as_slice(),
            [MirCheck::DischargeAuthorityDrift { .. }]
        ),
        "a transferee the stream never writes must drift, got {findings:?}"
    );
}

#[test]
fn discharge_authority_corroboration_accepts_real_transfer() {
    // The well-formed emit shape: the carrier is moved into the destination
    // immediately before the neutralize names it as the transferee. The two
    // carriers agree, so no drift.
    let blocks = vec![block(
        0,
        vec![
            Instr::Move {
                dest: Place::Local(9),
                src: variant_place(1),
            },
            Instr::NeutralizePayloadSlot {
                place: variant_place(1),
                transferee: Some(Place::Local(9)),
                authority: NeutralizeAuthority::WholeCarrierConsume,
            },
        ],
        Terminator::Return,
    )];
    assert!(
        validate_discharge_authority_corroboration_over("f", &blocks).is_empty(),
        "a transferee the stream actually writes must corroborate clean"
    );
}

#[test]
fn discharge_authority_corroborates_returned_aggregate_member() {
    let blocks = vec![block(
        0,
        vec![
            Instr::TupleConstruct {
                elements: vec![Place::Local(1)],
                dest: Place::Local(9),
            },
            Instr::NeutralizePayloadSlot {
                place: Place::Local(1),
                transferee: Some(Place::Local(9)),
                authority: NeutralizeAuthority::ReturnedAggregateMemberConsume,
            },
        ],
        Terminator::Return,
    )];
    assert!(
        validate_discharge_authority_corroboration_over("f", &blocks).is_empty(),
        "an exact aggregate member constructor must corroborate its carried transfer"
    );
}

#[test]
fn discharge_authority_corroborates_aggregate_member_through_move_alias() {
    let blocks = vec![block(
        0,
        vec![
            Instr::Move {
                dest: Place::Local(2),
                src: Place::Local(1),
            },
            Instr::RecordInit {
                ty: ResolvedTy::Unit,
                fields: vec![(FieldOffset(0), Place::Local(2))],
                dest: Place::Local(9),
            },
            Instr::NeutralizePayloadSlot {
                place: Place::Local(1),
                transferee: Some(Place::Local(9)),
                authority: NeutralizeAuthority::AggregateMemberConsume,
            },
        ],
        Terminator::Return,
    )];
    assert!(
        validate_discharge_authority_corroboration_over("f", &blocks).is_empty(),
        "a record member routed through a move alias must corroborate its transfer"
    );
}

/// A whole-local rebind out of a PARAMETER (`var iter = self;`) is a
/// caller-retained borrow alias: the rebound local must never produce a
/// definite under-release, even when re-minted per loop iteration from
/// a tuple-load (`iter = step.1`) — the `VecIter` cursor shape.
#[test]
fn param_rebind_and_tuple_load_remint_accept() {
    let blocks = vec![block(
        0,
        vec![
            Instr::Move {
                dest: Place::Local(3),
                src: Place::Local(0),
            },
            Instr::TupleFieldLoad {
                tuple: Place::Local(5),
                field_index: 1,
                dest: Place::Local(3),
            },
        ],
        Terminator::Return,
    )];
    let plans = vec![(ExitPath::Return { block: 0 }, DropPlan::default())];
    let elab = elab_with_plans(plans);
    let tracked: BTreeMap<u32, String> = [(3_u32, "iter".to_string())].into_iter().collect();
    let suspend_kinds = HashMap::new();
    let params: HashSet<u32> = [0_u32].into_iter().collect();
    let local_types = BTreeMap::new();
    let mint_sites = BTreeMap::new();
    let findings = validate_obligation_balance_with(
        &elab,
        &blocks,
        &suspend_kinds,
        &tracked,
        (&local_types, &mint_sites),
        &params,
    );
    assert!(
        findings.is_empty(),
        "borrow-derived mints never definite-leak: {findings:?}"
    );
}

#[test]
fn bytes_retain_move_requires_independent_destination_release() {
    let blocks = vec![block(
        0,
        vec![
            mint(1),
            Instr::BytesRetain {
                value: Place::Local(1),
            },
            Instr::Move {
                dest: Place::Local(2),
                src: Place::Local(1),
            },
        ],
        Terminator::Return,
    )];
    let plans = vec![(
        ExitPath::Return { block: 0 },
        DropPlan {
            drops: vec![plain_drop(Place::Local(1))],
        },
    )];
    let findings = run(blocks, plans, &[(1, "source"), (2, "retained")]);
    assert!(
        matches!(
            findings.as_slice(),
            [MirCheck::ObligationUnderReleased {
                name,
                hard: true,
                ..
            }] if name == "retained"
        ),
        "the explicit bytes retain mints a second owner; a source drop cannot pay the \
         destination's missing release: {findings:?}"
    );
}

#[test]
fn string_retain_move_preserves_source_obligation() {
    let blocks = vec![block(
        0,
        vec![
            mint(1),
            Instr::StringRetain {
                value: Place::Local(1),
                condition: crate::model::StringRetainCondition::Always,
            },
            Instr::Move {
                dest: Place::Local(2),
                src: Place::Local(1),
            },
        ],
        Terminator::Return,
    )];
    let plans = vec![(
        ExitPath::Return { block: 0 },
        DropPlan {
            drops: vec![plain_drop(Place::Local(2))],
        },
    )];
    let findings = run(blocks, plans, &[(1, "source"), (2, "retained")]);
    assert!(
        matches!(
            findings.as_slice(),
            [MirCheck::ObligationUnderReleased {
                name,
                hard: false,
                ..
            }] if name == "source"
        ),
        "a retain-backed move is a share, not an ambiguous source transfer; the original \
         string owner still needs its terminal release: {findings:?}"
    );
}

#[test]
fn standalone_retain_adds_an_owner_obligation() {
    let blocks = vec![block(
        0,
        vec![
            mint(1),
            Instr::BytesRetain {
                value: Place::Local(1),
            },
        ],
        Terminator::Return,
    )];
    let plans = vec![(
        ExitPath::Return { block: 0 },
        DropPlan {
            drops: vec![plain_drop(Place::Local(1))],
        },
    )];
    let findings = run(blocks, plans, &[(1, "shared")]);
    assert!(
        matches!(
            findings.as_slice(),
            [MirCheck::ObligationUnderReleased { hard: true, .. }]
        ),
        "one terminal drop cannot pay two owner mints after an explicit retain: {findings:?}"
    );
}

#[test]
fn bytes_handoff_commit_disposes_the_moved_from_generation() {
    let blocks = vec![block(
        0,
        vec![
            mint(1),
            Instr::RecordInit {
                ty: ResolvedTy::named_user("Carrier", vec![]),
                fields: vec![(crate::model::FieldOffset(0), Place::Local(1))],
                dest: Place::Local(2),
            },
            Instr::BytesLit {
                bytes: Vec::new(),
                dest: Place::Local(1),
            },
        ],
        Terminator::Return,
    )];
    let plans = vec![(ExitPath::Return { block: 0 }, DropPlan::default())];
    let findings = run(blocks, plans, &[(1, "published")]);
    assert!(
        findings.is_empty(),
        "the empty-bytes commit clears the transferred source owner: {findings:?}"
    );
}

#[test]
fn ordinary_empty_bytes_literal_still_mints_an_owner() {
    let blocks = vec![block(
        0,
        vec![Instr::BytesLit {
            bytes: Vec::new(),
            dest: Place::Local(1),
        }],
        Terminator::Return,
    )];
    let plans = vec![(ExitPath::Return { block: 0 }, DropPlan::default())];
    let findings = run(blocks, plans, &[(1, "literal")]);
    assert!(
        matches!(
            findings.as_slice(),
            [MirCheck::ObligationUnderReleased { blocks, .. }] if blocks == &[0]
        ),
        "an ordinary empty bytes allocation still needs its own release: {findings:?}"
    );
}

#[test]
fn conditional_string_leaf_retain_does_not_mint_whole_root_owner() {
    for condition in [
        crate::model::StringRetainCondition::AggregateBorrowedIngress,
        crate::model::StringRetainCondition::ActorStateRecordBorrowedIngress {
            state_field: crate::model::FieldOffset(0),
            record_path: vec![crate::model::FieldOffset(1)],
        },
    ] {
        let blocks = vec![block(
            0,
            vec![
                mint(1),
                Instr::StringRetain {
                    value: Place::Local(1),
                    condition,
                },
            ],
            Terminator::Return,
        )];
        let plans = vec![(
            ExitPath::Return { block: 0 },
            DropPlan {
                drops: vec![plain_drop(Place::Local(1))],
            },
        )];
        let findings = run(blocks, plans, &[(1, "aggregate")]);
        assert!(
            findings.is_empty(),
            "a leaf-layout retain must not create a second whole-root owner: {findings:?}"
        );
    }
}

#[test]
fn branch_local_retain_debt_is_not_diluted_at_join() {
    let blocks = vec![
        block(
            0,
            vec![mint(1)],
            Terminator::Branch {
                cond: Place::Local(0),
                then_target: 1,
                else_target: 2,
            },
        ),
        block(
            1,
            vec![Instr::BytesRetain {
                value: Place::Local(1),
            }],
            Terminator::Goto { target: 3 },
        ),
        block(2, Vec::new(), Terminator::Goto { target: 3 }),
        block(3, Vec::new(), Terminator::Return),
    ];
    let plans = vec![(
        ExitPath::Return { block: 3 },
        DropPlan {
            drops: vec![plain_drop(Place::Local(1))],
        },
    )];
    let findings = run(blocks, plans, &[(1, "shared")]);
    assert!(
        matches!(
            findings.as_slice(),
            [MirCheck::ObligationUnderReleased { hard: true, .. }]
        ),
        "the retained branch has two mints but only one release; the balanced sibling must \
         not dilute that explicit owner debt at the join: {findings:?}"
    );
}

#[test]
fn retain_move_with_both_terminal_releases_balances() {
    let blocks = vec![block(
        0,
        vec![
            mint(1),
            Instr::BytesRetain {
                value: Place::Local(1),
            },
            Instr::Move {
                dest: Place::Local(2),
                src: Place::Local(1),
            },
        ],
        Terminator::Return,
    )];
    let plans = vec![(
        ExitPath::Return { block: 0 },
        DropPlan {
            drops: vec![plain_drop(Place::Local(2)), plain_drop(Place::Local(1))],
        },
    )];
    let findings = run(blocks, plans, &[(1, "source"), (2, "retained")]);
    assert!(
        findings.is_empty(),
        "the source and retained destination each carry one mint and one release: {findings:?}"
    );
}

/// Fail-closed cap exhaustion: when the fixpoint cannot converge within
/// its iteration budget the verdict is UNKNOWN, and an unknown verdict
/// must NOT certify the body as balanced. A zero cap forces the exhaustion
/// path a converging body would never reach; the gate must emit an
/// unverified hard error rather than the old silent empty result.
#[test]
fn fixpoint_cap_exhaustion_fails_closed_unverified() {
    let blocks = vec![block(0, vec![mint(1)], Terminator::Return)];
    let plans = vec![(ExitPath::Return { block: 0 }, DropPlan::default())];
    let elab = elab_with_plans(plans);
    let tracked: BTreeMap<u32, String> = [(1_u32, "leaky".to_string())].into_iter().collect();
    let suspend_kinds = HashMap::new();
    let params = HashSet::new();
    let local_types = BTreeMap::new();
    let mint_sites = BTreeMap::new();
    let findings = validate_obligation_balance_capped(
        &elab,
        &blocks,
        &suspend_kinds,
        &tracked,
        (&local_types, &mint_sites),
        &params,
        0,
    );
    assert!(
        matches!(
            findings.as_slice(),
            [MirCheck::ObligationBalanceUnverified { .. }]
        ),
        "cap exhaustion must fail closed with an unverified verdict, not \
         silently certify balance: {findings:?}"
    );
}

/// The unverified verdict is a hard diagnostic with NO allowlist escape:
/// `check_to_diagnostic` must upgrade it so the CLI rejects the program.
#[test]
fn unverified_balance_upgrades_to_hard_diagnostic() {
    let check = MirCheck::ObligationBalanceUnverified {
        function: "f".to_string(),
        reason: "cap exhausted".to_string(),
    };
    assert!(
        check_to_diagnostic(&check).is_some(),
        "an unverified balance verdict must surface as a hard diagnostic"
    );
}

fn is_under(check: &MirCheck) -> bool {
    matches!(check, MirCheck::ObligationUnderReleased { .. })
}

fn is_over(check: &MirCheck) -> bool {
    matches!(check, MirCheck::ObligationOverReleased { .. })
}

/// S1886 round-4 branch-around shape: a guard early-return BEFORE the
/// consuming path, whose exit plan carries NO drop for the minted local
/// (`return[bb1] drop plan (none)`), while the other return discharges
/// it. The guard exit is a definite zero → under-release REJECT.
#[test]
fn branch_around_missing_guard_drop_rejects_under_release() {
    let blocks = vec![
        block(
            0,
            vec![mint(1)],
            Terminator::Branch {
                cond: Place::Local(0),
                then_target: 1,
                else_target: 2,
            },
        ),
        block(1, Vec::new(), Terminator::Return),
        block(2, Vec::new(), Terminator::Return),
    ];
    let plans = vec![
        (ExitPath::Return { block: 1 }, DropPlan::default()),
        (
            ExitPath::Return { block: 2 },
            DropPlan {
                drops: vec![plain_drop(Place::Local(1))],
            },
        ),
    ];
    let findings = run(blocks, plans, &[(1, "leaked")]);
    assert_eq!(
        findings.len(),
        1,
        "exactly the guard exit is unbalanced: {findings:?}"
    );
    let MirCheck::ObligationUnderReleased { blocks, name, .. } = &findings[0] else {
        panic!("expected under-release, got {:?}", findings[0]);
    };
    assert_eq!(blocks, &[1]);
    assert_eq!(name, "leaked");
}

#[test]
fn under_release_aggregates_all_exits_and_projects_the_mint_site() {
    let blocks = vec![
        block(
            0,
            vec![mint(1)],
            Terminator::Branch {
                cond: Place::Local(0),
                then_target: 1,
                else_target: 2,
            },
        ),
        block(1, Vec::new(), Terminator::Return),
        block(2, Vec::new(), Terminator::Return),
    ];
    let plans = vec![
        (ExitPath::Return { block: 1 }, DropPlan::default()),
        (ExitPath::Return { block: 2 }, DropPlan::default()),
    ];
    let findings = run(blocks, plans, &[(1, "resolve(...)")]);
    let [MirCheck::ObligationUnderReleased {
        blocks, site, name, ..
    }] = findings.as_slice()
    else {
        panic!("expected one aggregated under-release: {findings:?}");
    };
    assert_eq!(blocks, &[1, 2]);
    assert_eq!(*site, SiteId(1));
    assert_eq!(name, "resolve(...)");

    let diagnostic = check_to_diagnostic(&findings[0]).expect("finding must be visible");
    let MirDiagnosticKind::ObligationUnderReleased {
        blocks, site, name, ..
    } = diagnostic.kind
    else {
        panic!("expected projected under-release diagnostic");
    };
    assert_eq!(blocks, vec![1, 2]);
    assert_eq!(site, SiteId(1));
    assert_eq!(name, "resolve(...)");
}

/// The fixed round-4 shape: every return path carries exactly one
/// discharge → ACCEPT.
#[test]
fn branch_around_with_both_exit_drops_accepts() {
    let blocks = vec![
        block(
            0,
            vec![mint(1)],
            Terminator::Branch {
                cond: Place::Local(0),
                then_target: 1,
                else_target: 2,
            },
        ),
        block(1, Vec::new(), Terminator::Return),
        block(2, Vec::new(), Terminator::Return),
    ];
    let plans = vec![
        (
            ExitPath::Return { block: 1 },
            DropPlan {
                drops: vec![plain_drop(Place::Local(1))],
            },
        ),
        (
            ExitPath::Return { block: 2 },
            DropPlan {
                drops: vec![plain_drop(Place::Local(1))],
            },
        ),
    ];
    let findings = run(blocks, plans, &[(1, "balanced")]);
    assert!(findings.is_empty(), "balanced on both exits: {findings:?}");
}

/// S1882 `move_out_arm` shape: a match-arm payload binder is moved out
/// of the carrier's variant slot with NO `NeutralizePayloadSlot`, and
/// the exit plan releases BOTH the binder and the variant slot — two
/// discharges of one obligation on one path → over-release REJECT.
#[test]
fn move_out_arm_without_neutralize_rejects_over_release() {
    let blocks = vec![block(
        0,
        vec![
            mint(1),
            Instr::Move {
                dest: Place::Local(2),
                src: variant_place(1),
            },
        ],
        Terminator::Return,
    )];
    let plans = vec![(
        ExitPath::Return { block: 0 },
        DropPlan {
            drops: vec![plain_drop(Place::Local(2)), plain_drop(variant_place(1))],
        },
    )];
    let findings = run(blocks, plans, &[(1, "carrier"), (2, "w")]);
    assert_eq!(
        findings.len(),
        1,
        "one over-release for the carrier: {findings:?}"
    );
    let MirCheck::ObligationOverReleased { name, .. } = &findings[0] else {
        panic!("expected over-release, got {:?}", findings[0]);
    };
    assert_eq!(name, "carrier");
}

/// Branch-conditional double-free: the value is definitely discharged
/// TWICE on the `then` arm (returned twice) and once on the `else` arm,
/// and the two arms join before a common return. The join meet leaves the
/// per-path MINIMUM at 1, so a verdict keyed on `lo` path-dilutes the
/// double-free away and silently certifies. The per-path definite MAXIMUM
/// is 2, so the fixed verdict REJECTS: a double-free on any single path is
/// memory-unsafe regardless of the other paths.
#[test]
fn branch_conditional_double_free_on_one_arm_rejects_over_release() {
    let blocks = vec![
        block(
            0,
            vec![mint(1)],
            Terminator::Branch {
                cond: Place::Local(0),
                then_target: 1,
                else_target: 2,
            },
        ),
        // `then`: two definite discharges (return-transfer twice) — the
        // double-free arm.
        block(
            1,
            vec![
                Instr::Move {
                    dest: Place::ReturnSlot,
                    src: Place::Local(1),
                },
                Instr::Move {
                    dest: Place::ReturnSlot,
                    src: Place::Local(1),
                },
            ],
            Terminator::Goto { target: 3 },
        ),
        // `else`: exactly one definite discharge — balanced.
        block(
            2,
            vec![Instr::Move {
                dest: Place::ReturnSlot,
                src: Place::Local(1),
            }],
            Terminator::Goto { target: 3 },
        ),
        block(3, Vec::new(), Terminator::Return),
    ];
    let plans = vec![(ExitPath::Return { block: 3 }, DropPlan::default())];
    let findings = run(blocks, plans, &[(1, "doubled")]);
    assert_eq!(
        findings.len(),
        1,
        "the then-arm double-free must reject even though it merges with a \
         single-discharge arm: {findings:?}"
    );
    let MirCheck::ObligationOverReleased { name, .. } = &findings[0] else {
        panic!("expected over-release, got {:?}", findings[0]);
    };
    assert_eq!(name, "doubled");
}

/// The FIXED move-out shape (#2523): the move-out is paired with a
/// `NeutralizePayloadSlot`, so the binder is an independent owner and
/// the carrier's variant-slot drop is a null-tolerant no-op → ACCEPT.
#[test]
fn move_out_arm_with_neutralize_accepts() {
    let blocks = vec![block(
        0,
        vec![
            mint(1),
            Instr::Move {
                dest: Place::Local(2),
                src: variant_place(1),
            },
            Instr::NeutralizePayloadSlot {
                place: variant_place(1),
                transferee: Some(Place::Local(2)),
                authority: crate::model::NeutralizeAuthority::WholeCarrierConsume,
            },
        ],
        Terminator::Return,
    )];
    let plans = vec![(
        ExitPath::Return { block: 0 },
        DropPlan {
            drops: vec![plain_drop(Place::Local(2)), plain_drop(variant_place(1))],
        },
    )];
    let findings = run(blocks, plans, &[(1, "carrier"), (2, "w")]);
    assert!(
        findings.is_empty(),
        "neutralized transfer balances: {findings:?}"
    );
}

#[test]
fn empty_enum_tag_does_not_remint_a_discharged_call_scrutinee() {
    // `Option<string>` terminal helpers re-use their call-scrutinee slot for
    // the exhausted `None` result. The runtime already consumed the prior
    // `Some(string)` payload through the closure call; writing only the `None`
    // tag does not allocate another string and must not erase that discharge.
    let blocks = vec![
        block(
            0,
            vec![
                mint(1),
                Instr::CallClosure {
                    callee: Place::Local(8),
                    args: vec![Place::Local(1)],
                    dest: Some(Place::Local(9)),
                    ret_ty: ResolvedTy::Bool,
                },
            ],
            Terminator::Goto { target: 1 },
        ),
        block(
            1,
            vec![Instr::ConstI64 {
                dest: Place::EnumTag(1),
                value: 1,
            }],
            Terminator::Return,
        ),
    ];
    let plans = vec![(
        ExitPath::Return { block: 1 },
        DropPlan {
            drops: vec![plain_drop(Place::Local(1))],
        },
    )];
    let findings = run(blocks, plans, &[(1, "__hew_call_scrutinee")]);
    assert!(
        findings.is_empty(),
        "a tag-only empty variant must preserve the existing discharge: {findings:?}"
    );
}

/// A freshly minted string moved into a record field inside a loop body,
/// then committed with the empty-static-string marker, and dropped on the
/// loop back-edge because the slot is still nominally in scope. The commit
/// marker neutralizes the moved-from slot, so the back-edge drop walks a
/// nulled slot and is a no-op — ACCEPT. Before the fix the marker confirmed
/// the transfer as a definite discharge but left the slot un-neutralized,
/// so the back-edge drop counted as a SECOND definite discharge and
/// manufactured a phantom over-release for the common build-a-record-in-a-
/// loop shape.
#[test]
fn aggregate_transfer_commit_then_backedge_drop_accepts() {
    let blocks = vec![
        block(0, Vec::new(), Terminator::Goto { target: 1 }),
        block(
            1,
            Vec::new(),
            Terminator::Branch {
                cond: Place::Local(9),
                then_target: 2,
                else_target: 3,
            },
        ),
        block(
            2,
            vec![
                mint(1),
                Instr::RecordInit {
                    ty: ResolvedTy::Unit,
                    fields: vec![(FieldOffset(0), Place::Local(1))],
                    dest: Place::Local(3),
                },
                // The aggregate-transfer commit marker: the moved-from slot
                // is overwritten with an empty static string.
                Instr::StringLit {
                    bytes: Vec::new(),
                    dest: Place::Local(1),
                },
            ],
            Terminator::Goto { target: 1 },
        ),
        block(3, Vec::new(), Terminator::Return),
    ];
    // The loop back-edge drops the now-emptied `key` slot.
    let plans = vec![
        (
            ExitPath::Goto {
                block: 2,
                target: 1,
            },
            DropPlan {
                drops: vec![plain_drop(Place::Local(1))],
            },
        ),
        (ExitPath::Return { block: 3 }, DropPlan::default()),
    ];
    let findings = run(blocks, plans, &[(1, "key")]);
    assert!(
        findings.is_empty(),
        "the commit marker neutralizes the moved-from slot, so the \
         back-edge drop is a no-op, not a second discharge: {findings:?}"
    );
}

/// Distinguish-garbage pin for the neutralize above: a REAL defining write
/// AFTER the commit marker re-mints the slot to a fresh owner (neutralized
/// reset), so a genuine double-free on the NEW generation (two definite
/// return-transfers) still REJECTS. Proves the fix suppresses only the
/// no-op drop of the emptied slot, not genuine over-release of a live one.
#[test]
fn remint_after_commit_marker_still_rejects_double_free() {
    let blocks = vec![block(
        0,
        vec![
            mint(1),
            Instr::RecordInit {
                ty: ResolvedTy::Unit,
                fields: vec![(FieldOffset(0), Place::Local(1))],
                dest: Place::Local(3),
            },
            // Commit marker neutralizes the slot ...
            Instr::StringLit {
                bytes: Vec::new(),
                dest: Place::Local(1),
            },
            // ... then a real defining write re-mints a fresh owner.
            mint(1),
            // ... which is then definitely discharged TWICE — a genuine
            // double-free of the new generation.
            Instr::Move {
                dest: Place::ReturnSlot,
                src: Place::Local(1),
            },
            Instr::Move {
                dest: Place::ReturnSlot,
                src: Place::Local(1),
            },
        ],
        Terminator::Return,
    )];
    let plans = vec![(ExitPath::Return { block: 0 }, DropPlan::default())];
    let findings = run(blocks, plans, &[(1, "key")]);
    assert_eq!(
        findings.len(),
        1,
        "a re-minted generation double-freed after the commit marker must \
         still reject: {findings:?}"
    );
    let MirCheck::ObligationOverReleased { name, .. } = &findings[0] else {
        panic!("expected over-release, got {:?}", findings[0]);
    };
    assert_eq!(name, "key");
}

/// Negative control (A278 / S1873): a value transferred into an
/// aggregate (the `forward_param_into_field` family once the param
/// exclusion has removed the parameter itself) must NOT phantom-reject
/// — aggregation operands are ambiguous transfers, so an exit without a
/// terminal drop is not a definite leak.
#[test]
fn aggregate_transfer_without_exit_drop_accepts() {
    let blocks = vec![block(
        0,
        vec![
            mint(1),
            Instr::RecordInit {
                ty: ResolvedTy::Unit,
                fields: vec![(FieldOffset(0), Place::Local(1))],
                dest: Place::Local(3),
            },
        ],
        Terminator::Return,
    )];
    let plans = vec![(ExitPath::Return { block: 0 }, DropPlan::default())];
    let findings = run(blocks, plans, &[(1, "forwarded")]);
    assert!(
        findings.is_empty(),
        "aggregation is not a definite leak: {findings:?}"
    );
}

#[test]
fn rc_new_consumes_its_payload_owner() {
    let blocks = vec![block(
        0,
        vec![
            mint(1),
            Instr::RcIntrinsic {
                dest: Place::Local(2),
                op: hew_types::RcIntrinsicOp::New,
                payload_ty: ResolvedTy::Unit,
                receiver: None,
                value: Some(Place::Local(1)),
                result_ty: ResolvedTy::Unit,
            },
        ],
        Terminator::Return,
    )];
    let findings = run(
        blocks,
        vec![(ExitPath::Return { block: 0 }, DropPlan::default())],
        &[(1, "rc payload")],
    );
    assert!(
        findings.is_empty(),
        "Rc::new must take ownership of its payload: {findings:?}"
    );
}

/// Return-transfer is a definite discharge: `Move → ReturnSlot` with no
/// exit drop balances; adding a spurious exit drop on top is a definite
/// double release.
#[test]
fn return_transfer_balances_and_extra_drop_rejects() {
    let make_blocks = || {
        vec![block(
            0,
            vec![
                mint(1),
                Instr::Move {
                    dest: Place::ReturnSlot,
                    src: Place::Local(1),
                },
            ],
            Terminator::Return,
        )]
    };
    let clean = run(
        make_blocks(),
        vec![(ExitPath::Return { block: 0 }, DropPlan::default())],
        &[(1, "returned")],
    );
    assert!(clean.is_empty(), "return-transfer balances: {clean:?}");

    let doubled = run(
        make_blocks(),
        vec![(
            ExitPath::Return { block: 0 },
            DropPlan {
                drops: vec![plain_drop(Place::Local(1))],
            },
        )],
        &[(1, "returned")],
    );
    assert_eq!(doubled.len(), 1, "{doubled:?}");
    assert!(
        is_over(&doubled[0]),
        "spurious drop after return-transfer: {doubled:?}"
    );
}

/// A guard-gated exit drop is path-sensitive at runtime — the validator
/// treats it as ambiguous, so it neither certifies balance nor
/// manufactures a definite report.
#[test]
fn guarded_exit_drop_is_never_a_definite_verdict() {
    let blocks = vec![block(0, vec![mint(1)], Terminator::Return)];
    let guarded = ElabDrop {
        guard: Some(Place::Local(7)),
        ..plain_drop(Place::Local(1))
    };
    let plans = vec![(
        ExitPath::Return { block: 0 },
        DropPlan {
            drops: vec![guarded],
        },
    )];
    let findings = run(blocks, plans, &[(1, "gated")]);
    assert!(
        findings.is_empty(),
        "guarded drop widens, never decides: {findings:?}"
    );
}

/// Per-iteration mint + per-iteration inline release around a loop back
/// edge balances: the defining write resets the count each iteration.
#[test]
fn loop_body_mint_and_release_accepts() {
    let blocks = vec![
        block(0, Vec::new(), Terminator::Goto { target: 1 }),
        block(
            1,
            vec![
                mint(1),
                Instr::Drop {
                    place: Place::Local(1),
                    ty: ResolvedTy::String,
                    drop_fn: None,
                },
            ],
            Terminator::Branch {
                cond: Place::Local(0),
                then_target: 1,
                else_target: 2,
            },
        ),
        block(2, Vec::new(), Terminator::Return),
    ];
    let plans = vec![(ExitPath::Return { block: 2 }, DropPlan::default())];
    let findings = run(blocks, plans, &[(1, "cursor")]);
    assert!(
        findings.is_empty(),
        "per-iteration lifecycle balances: {findings:?}"
    );
}

/// An unminted local (its defining write never executes on any path to
/// the exit) carries no obligation — no finding, and an exit-plan drop
/// for it is not counted into a phantom mint.
#[test]
fn unminted_local_is_skipped() {
    let blocks = vec![block(0, Vec::new(), Terminator::Return)];
    let plans = vec![(
        ExitPath::Return { block: 0 },
        DropPlan {
            drops: vec![plain_drop(Place::Local(1))],
        },
    )];
    let findings = run(blocks, plans, &[(1, "never_minted")]);
    assert!(findings.is_empty(), "no mint, no obligation: {findings:?}");
}

/// The abandon plan balances the parked frame, but it must not be charged
/// to the resume edge: that path still leaks when its return plan omits the
/// terminal drop.
#[test]
fn suspend_resume_without_return_drop_is_under_release() {
    let blocks = vec![
        block(
            0,
            vec![mint(1)],
            Terminator::Suspend {
                resume: 1,
                cleanup: 1,
                is_final: false,
            },
        ),
        block(1, Vec::new(), Terminator::Return),
    ];
    let plans = vec![
        (
            ExitPath::Suspend {
                block: 0,
                resume: 1,
                cleanup: 1,
            },
            DropPlan {
                drops: vec![plain_drop(Place::Local(1))],
            },
        ),
        (ExitPath::Return { block: 1 }, DropPlan::default()),
    ];
    let findings = run(blocks, plans, &[(1, "framed")]);
    assert!(
        matches!(
            findings.as_slice(),
            [MirCheck::ObligationUnderReleased { blocks, .. }] if blocks == &[1]
        ),
        "only the resumed return edge is under-released: {findings:?}"
    );
}

/// A return drop balances normal completion, but cannot compensate for an
/// abandon edge whose frame-cleanup plan omits the parked owner.
#[test]
fn suspend_abandon_without_frame_drop_is_under_release() {
    let blocks = vec![
        block(
            0,
            vec![mint(1)],
            Terminator::Suspend {
                resume: 1,
                cleanup: 1,
                is_final: false,
            },
        ),
        block(1, Vec::new(), Terminator::Return),
    ];
    let plans = vec![
        (
            ExitPath::Suspend {
                block: 0,
                resume: 1,
                cleanup: 1,
            },
            DropPlan::default(),
        ),
        (
            ExitPath::Return { block: 1 },
            DropPlan {
                drops: vec![plain_drop(Place::Local(1))],
            },
        ),
    ];
    let findings = run(blocks, plans, &[(1, "framed")]);
    assert!(
        matches!(
            findings.as_slice(),
            [MirCheck::ObligationUnderReleased { blocks, .. }] if blocks == &[0]
        ),
        "only the abandon edge with no frame drop is under-released: {findings:?}"
    );
}

/// Resume and abandon are mutually exclusive terminal ownership paths;
/// each one needs (and may safely carry) its own drop of the frame owner.
#[test]
fn suspend_resume_and_abandon_drops_balance() {
    let blocks = vec![
        block(
            0,
            vec![mint(1)],
            Terminator::Suspend {
                resume: 1,
                cleanup: 1,
                is_final: false,
            },
        ),
        block(1, Vec::new(), Terminator::Return),
    ];
    let plans = vec![
        (
            ExitPath::Suspend {
                block: 0,
                resume: 1,
                cleanup: 1,
            },
            DropPlan {
                drops: vec![plain_drop(Place::Local(1))],
            },
        ),
        (
            ExitPath::Return { block: 1 },
            DropPlan {
                drops: vec![plain_drop(Place::Local(1))],
            },
        ),
    ];
    let findings = run(blocks, plans, &[(1, "framed")]);
    assert!(
        findings.is_empty(),
        "resume and abandon each carry one terminal drop: {findings:?}"
    );
}

/// A suspend result does not exist on abandon, but becomes a fresh owner
/// on case-0 resume and therefore needs a terminal drop on that path.
#[test]
fn suspend_result_mint_is_resume_edge_local() {
    let blocks = vec![
        block(
            0,
            Vec::new(),
            Terminator::Suspend {
                resume: 1,
                cleanup: 2,
                is_final: false,
            },
        ),
        block(1, Vec::new(), Terminator::Return),
        block(2, Vec::new(), Terminator::Return),
    ];
    let plans = vec![
        (
            ExitPath::Suspend {
                block: 0,
                resume: 1,
                cleanup: 2,
            },
            DropPlan::default(),
        ),
        (ExitPath::Return { block: 1 }, DropPlan::default()),
        (ExitPath::Return { block: 2 }, DropPlan::default()),
    ];
    let suspend_kinds = [(
        0,
        SuspendKind::Read {
            conn: Place::Local(0),
            result_dest: Place::Local(1),
            deadline_result_dest: None,
            error_dest: None,
            to_string: true,
        },
    )]
    .into_iter()
    .collect();
    let findings = run_with_suspend_kinds(&blocks, plans, &[(1, "reply")], &suspend_kinds);
    assert!(
        matches!(
            findings.as_slice(),
            [MirCheck::ObligationUnderReleased { blocks, .. }] if blocks == &[1]
        ),
        "the resumed result is minted only on resume, never on abandon: {findings:?}"
    );
}

/// Send transfers the prepared outbound owner to transport: definite
/// discharge, balanced without an exit drop.
#[test]
fn send_value_transfer_balances() {
    let blocks = vec![
        block(
            0,
            vec![mint(1)],
            Terminator::Send {
                actor: Place::Local(0),
                msg_type: 0,
                value: Place::Local(1),
                next: 1,
                arg_modes: Vec::new(),
                cleanup_plan: None,
            },
        ),
        block(1, Vec::new(), Terminator::Return),
    ];
    let plans = vec![(ExitPath::Return { block: 1 }, DropPlan::default())];
    let findings = run(blocks, plans, &[(1, "payload")]);
    assert!(
        findings.is_empty(),
        "transport consumed the owner: {findings:?}"
    );
}

/// Interval meets are sound at merges: discharge on one arm only, then
/// a joined return with no plan drop is [0,1] — genuinely path-dependent
/// (the guard-flag case), so no definite verdict either way.
#[test]
fn merge_with_one_armed_discharge_is_not_definite() {
    let blocks = vec![
        block(
            0,
            vec![mint(1)],
            Terminator::Branch {
                cond: Place::Local(0),
                then_target: 1,
                else_target: 2,
            },
        ),
        block(
            1,
            vec![Instr::Drop {
                place: Place::Local(1),
                ty: ResolvedTy::String,
                drop_fn: None,
            }],
            Terminator::Goto { target: 3 },
        ),
        block(2, Vec::new(), Terminator::Goto { target: 3 }),
        block(3, Vec::new(), Terminator::Return),
    ];
    let plans = vec![(ExitPath::Return { block: 3 }, DropPlan::default())];
    let findings = run(blocks, plans, &[(1, "joined")]);
    assert!(
        findings.iter().all(|f| !is_under(f) && !is_over(f)),
        "a some-paths discharge is ambiguous, not definite: {findings:?}"
    );
}
