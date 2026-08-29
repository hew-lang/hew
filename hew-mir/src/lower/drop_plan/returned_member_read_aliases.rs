//! Returned-member read-alias re-admission pins.
//!
//! Carved out of `drop_plan.rs` as a pure move so the module stays under its
//! line ceiling; `super` still resolves to `drop_plan`, so every helper the
//! pins reach is the same one they reached inline.

use super::*;

fn block(id: u32, terminator: Terminator) -> BasicBlock {
    BasicBlock {
        id,
        statements: Vec::new(),
        instructions: Vec::new(),
        terminator,
    }
}

/// Counterfactual for non-terminal re-admission: the candidate owner is
/// copied through unretained MIR handoff slots before a branch, and only
/// the final alias is read after the join. A direct-local scan sees no
/// future read of `_1` from either arm Goto and could free it there; the
/// alias-closed scan must attribute the joined `_3` read back to `_1`.
#[test]
fn unretained_move_alias_read_after_join_is_attributed_to_candidate() {
    let blocks = vec![
        BasicBlock {
            id: 0,
            statements: Vec::new(),
            instructions: vec![Instr::Move {
                dest: Place::Local(2),
                src: Place::Local(1),
            }],
            terminator: Terminator::Branch {
                cond: Place::Local(0),
                then_target: 1,
                else_target: 2,
            },
        },
        BasicBlock {
            id: 1,
            statements: Vec::new(),
            instructions: vec![Instr::Move {
                dest: Place::Local(3),
                src: Place::Local(2),
            }],
            terminator: Terminator::Goto { target: 3 },
        },
        BasicBlock {
            id: 2,
            statements: Vec::new(),
            instructions: vec![Instr::Move {
                dest: Place::Local(3),
                src: Place::Local(2),
            }],
            terminator: Terminator::Goto { target: 3 },
        },
        BasicBlock {
            id: 3,
            statements: Vec::new(),
            instructions: vec![Instr::Drop {
                place: Place::Local(3),
                ty: ResolvedTy::String,
                drop_fn: None,
            }],
            terminator: Terminator::Return,
        },
    ];
    let binding = BindingId(7);
    let candidate_locals = [(1_u32, binding)].into_iter().collect();
    let reads = returned_member_alias_read_blocks(&blocks, &HashMap::new(), &candidate_locals);

    assert!(
        reads
            .get(&binding)
            .is_some_and(|blocks| blocks.contains(&3)),
        "the post-join read through `_3` must be attributed to returned-member \
         candidate `_1`; otherwise an arm Goto may release the shared heap early: \
         {reads:?}"
    );
}

/// Diamond counterfactual: only the then arm has an earlier candidate A,
/// while both arms merge at candidate B. Reachability alone must not keep
/// A and suppress B (that leaks the else arm). B postdominates A's target,
/// so the selector keeps only B and both paths cross exactly one release.
#[test]
fn merged_later_candidate_covers_sibling_that_bypasses_earlier_candidate() {
    let blocks = vec![
        block(
            0,
            Terminator::Branch {
                cond: Place::Local(0),
                then_target: 1,
                else_target: 2,
            },
        ),
        block(1, Terminator::Goto { target: 3 }),
        block(2, Terminator::Goto { target: 3 }),
        block(3, Terminator::Goto { target: 4 }),
        block(4, Terminator::Return),
    ];
    let block_reach: HashMap<u32, HashSet<u32>> = blocks
        .iter()
        .map(|block| (block.id, blocks_reachable_from(&blocks, block.id)))
        .collect();
    let candidates = [
        ReturnedMemberReAdmission {
            plan_index: 10,
            block: 1,
            path: ReturnedMemberReAdmissionPath::Normal,
        },
        ReturnedMemberReAdmission {
            plan_index: 11,
            block: 3,
            path: ReturnedMemberReAdmissionPath::Normal,
        },
    ];

    let selected = select_returned_member_re_admissions(&blocks, &block_reach, &candidates)
        .expect("the later common candidate is unambiguous");
    assert_eq!(
        selected,
        vec![candidates[1]],
        "the common later candidate must replace the one-arm predecessor"
    );

    for path in [[0_u32, 1, 3, 4], [0_u32, 2, 3, 4]] {
        let releases = selected
            .iter()
            .filter(|candidate| path.contains(&candidate.block))
            .count();
        assert_eq!(
            releases, 1,
            "both diamond paths must cross exactly one selected release: \
             path={path:?}, selected={selected:?}"
        );
    }
}

/// Existing-plan diamond counterfactual: the then arm already releases the
/// owner and a later common candidate covers both arms. A reachability veto
/// would delete the common candidate and leak the else arm. Because the
/// common block postdominates the existing arm's continuation, it may
/// replace that arm-local release and both paths retain exactly one release.
#[test]
fn common_candidate_replaces_branch_local_existing_release() {
    let blocks = vec![
        block(
            0,
            Terminator::Branch {
                cond: Place::Local(0),
                then_target: 1,
                else_target: 2,
            },
        ),
        block(1, Terminator::Goto { target: 3 }),
        block(2, Terminator::Goto { target: 3 }),
        block(3, Terminator::Goto { target: 4 }),
        block(4, Terminator::Return),
    ];
    let block_reach: HashMap<u32, HashSet<u32>> = blocks
        .iter()
        .map(|block| (block.id, blocks_reachable_from(&blocks, block.id)))
        .collect();
    let existing = [ReturnedMemberReAdmission {
        plan_index: 20,
        block: 1,
        path: ReturnedMemberReAdmissionPath::Normal,
    }];
    let candidate = ReturnedMemberReAdmission {
        plan_index: 10,
        block: 3,
        path: ReturnedMemberReAdmissionPath::Normal,
    };

    let replaced =
        existing_releases_replaced_by_candidate(&blocks, &block_reach, candidate, &existing)
            .expect("the common postdominator must be comparable")
            .expect("the common postdominator can replace the arm-local release");
    assert_eq!(replaced, HashSet::from([existing[0].plan_index]));

    for path in [[0_u32, 1, 3, 4], [0_u32, 2, 3, 4]] {
        let releases = usize::from(
            path.contains(&existing[0].block) && !replaced.contains(&existing[0].plan_index),
        ) + usize::from(path.contains(&candidate.block));
        assert_eq!(
            releases, 1,
            "both diamond paths must cross exactly one release after relocation: \
             path={path:?}, replaced={replaced:?}"
        );
    }
}

/// A normal scope-closing Goto can precede a later loop cancellation. The
/// later cancel must not duplicate the already-completed release, but an
/// independent cancellation route that bypasses the Goto still owns one.
#[test]
fn normal_goto_replaces_only_the_later_loop_cancellation_release() {
    let blocks = vec![
        block(
            0,
            Terminator::Branch {
                cond: Place::Local(0),
                then_target: 1,
                else_target: 4,
            },
        ),
        block(1, Terminator::Goto { target: 2 }),
        block(2, Terminator::Goto { target: 2 }),
        block(4, Terminator::Goto { target: 5 }),
        block(5, Terminator::Return),
    ];
    let block_reach: HashMap<u32, HashSet<u32>> = blocks
        .iter()
        .map(|block| (block.id, blocks_reachable_from(&blocks, block.id)))
        .collect();
    let candidates = [
        ReturnedMemberReAdmission {
            plan_index: 10,
            block: 1,
            path: ReturnedMemberReAdmissionPath::Normal,
        },
        ReturnedMemberReAdmission {
            plan_index: 11,
            block: 2,
            path: ReturnedMemberReAdmissionPath::Abandonment,
        },
        ReturnedMemberReAdmission {
            plan_index: 12,
            block: 4,
            path: ReturnedMemberReAdmissionPath::Abandonment,
        },
    ];

    let selected = select_returned_member_re_admissions(&blocks, &block_reach, &candidates)
        .expect("the dominated loop cancellation has one prior release owner");
    assert_eq!(
        selected,
        vec![candidates[0], candidates[2]],
        "the post-Goto loop cancellation is redundant, while the route that bypasses \
         the Goto retains cancellation coverage"
    );
    let abandoned_existing = existing_releases_replaced_by_candidate(
        &blocks,
        &block_reach,
        candidates[0],
        &candidates[1..],
    )
    .expect("the normal Goto is comparable to both cancellation plans")
    .expect("cross-path authority arbitration occurs after candidate selection");
    assert_eq!(
        abandoned_existing,
        HashSet::new(),
        "normal/cancellation replacements must wait until normal candidates are final"
    );
    assert_eq!(
        existing_releases_replaced_by_candidate(
            &blocks,
            &block_reach,
            candidates[1],
            &candidates[..1],
        )
        .expect("the cancellation is comparable to the preceding Goto"),
        Some(HashSet::new()),
        "normal/cancellation arbitration must wait until normal candidates are final"
    );

    for path in [&[0_u32, 1, 2][..], &[0_u32, 4][..]] {
        let releases = selected
            .iter()
            .filter(|candidate| path.contains(&candidate.block))
            .count();
        assert_eq!(
            releases, 1,
            "each normal or cancellation path must have exactly one release: \
             path={path:?}, selected={selected:?}"
        );
    }
}

/// If a downstream normal Goto replaces A, a cancellation between A and the
/// replacement bypasses the final normal authority and must stay selected.
#[test]
fn later_normal_replacement_does_not_suppress_intermediate_cancellation() {
    let blocks = vec![
        block(0, Terminator::Goto { target: 1 }),
        block(1, Terminator::Goto { target: 2 }),
        block(2, Terminator::Goto { target: 3 }),
        block(3, Terminator::Goto { target: 4 }),
        block(4, Terminator::Return),
    ];
    let block_reach: HashMap<u32, HashSet<u32>> = blocks
        .iter()
        .map(|block| (block.id, blocks_reachable_from(&blocks, block.id)))
        .collect();
    let candidates = [
        ReturnedMemberReAdmission {
            plan_index: 10,
            block: 1,
            path: ReturnedMemberReAdmissionPath::Normal,
        },
        ReturnedMemberReAdmission {
            plan_index: 11,
            block: 2,
            path: ReturnedMemberReAdmissionPath::Abandonment,
        },
        ReturnedMemberReAdmission {
            plan_index: 12,
            block: 3,
            path: ReturnedMemberReAdmissionPath::Normal,
        },
    ];

    let selected = select_returned_member_re_admissions(&blocks, &block_reach, &candidates)
        .expect("the normal replacement and intermediate cancellation are unambiguous");
    assert_eq!(
        selected,
        vec![candidates[1], candidates[2]],
        "the replacement owns normal completion while the intermediate cancel \
         retains its bypass cleanup"
    );
    let cancellation_releases = selected
        .iter()
        .filter(|candidate| {
            matches!(candidate.path, ReturnedMemberReAdmissionPath::Abandonment)
                && candidate.block == 2
        })
        .count();
    assert_eq!(
        cancellation_releases, 1,
        "the cancellation at bb2 must retain exactly one release: {selected:?}"
    );
    let normal_releases = selected
        .iter()
        .filter(|candidate| {
            matches!(candidate.path, ReturnedMemberReAdmissionPath::Normal)
                && [0_u32, 1, 2, 3, 4].contains(&candidate.block)
        })
        .count();
    assert_eq!(
        normal_releases, 1,
        "the normal continuation must retain exactly one release: {selected:?}"
    );
}

/// Partial overlap counterfactual: one release can reach the other, but
/// neither covers all of the other's paths. Selecting either candidate
/// leaks one path and selecting both double-releases their overlap, so this
/// topology must reject instead of silently omitting every cleanup.
#[test]
fn partial_overlap_re_admission_is_rejected() {
    let blocks = vec![
        block(
            0,
            Terminator::Branch {
                cond: Place::Local(0),
                then_target: 1,
                else_target: 2,
            },
        ),
        block(1, Terminator::Goto { target: 3 }),
        block(2, Terminator::Goto { target: 4 }),
        block(
            3,
            Terminator::Branch {
                cond: Place::Local(0),
                then_target: 4,
                else_target: 5,
            },
        ),
        block(4, Terminator::Goto { target: 5 }),
        block(5, Terminator::Return),
    ];
    let block_reach: HashMap<u32, HashSet<u32>> = blocks
        .iter()
        .map(|block| (block.id, blocks_reachable_from(&blocks, block.id)))
        .collect();
    let candidates = [
        ReturnedMemberReAdmission {
            plan_index: 10,
            block: 1,
            path: ReturnedMemberReAdmissionPath::Normal,
        },
        ReturnedMemberReAdmission {
            plan_index: 11,
            block: 4,
            path: ReturnedMemberReAdmissionPath::Normal,
        },
    ];

    let ambiguity = select_returned_member_re_admissions(&blocks, &block_reach, &candidates)
        .expect_err("partial overlap has no exactly-once cleanup owner");
    assert_eq!(ambiguity.first, candidates[0]);
    assert_eq!(ambiguity.second, candidates[1]);
}
