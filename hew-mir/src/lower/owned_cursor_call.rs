//! Post-CFG caller handoff for the direct-call `OwnedCursor` ABI.
//!
//! Ordinary carrier preparation may allocate or unwind, while cursor ownership
//! must commit only after that work and remain the final side-effecting sequence
//! before invoke. This module validates the exact caller owner and guard, emits
//! the semantic handoff, and re-anchors it after later CFG materialisation.

use super::{
    drop_plan, shift_instr_spans_on_insert, shift_instr_spans_on_remove, BasicBlock, Builder,
    CommittedOwnedCursorArg, CommittedOwnedCursorCall, DischargeSite, HashMap, HashSet, Instr,
    IntentKind, MirDiagnostic, MirDiagnosticKind, MirStatement, PendingOwnedCallAnchor,
    PendingOwnedCallSite, PendingOwnedCursorArg, Place, ResolvedTy, SiteId, Terminator,
};

struct CursorCommit {
    owner: crate::model::OwnerId,
    source: Place,
    flag: Place,
    site: SiteId,
    name: String,
    ty: ResolvedTy,
}

fn owned_cursor_owner_and_guard(
    blocks: &[BasicBlock],
    builder: &mut Builder,
    exit: &drop_plan::ExactOwnerState,
    call_args: &[Place],
    arg: &PendingOwnedCursorArg,
) -> Option<(crate::model::OwnerId, Place)> {
    if !matches!(arg.source, Place::Local(_)) || call_args.get(arg.index) != Some(&arg.source) {
        builder.diagnostics.push(MirDiagnostic {
            kind: MirDiagnosticKind::NotYetImplemented {
                construct: "non-local OwnedCursor argument".to_string(),
                site: arg.site,
            },
            note: "the direct cursor ABI requires the exact whole local passed at its typed argument index"
                .to_string(),
        });
        return None;
    }
    let owners_at_source = exit
        .iter()
        .filter_map(|(owner, place)| (*place == arg.source).then_some(*owner))
        .collect::<Vec<_>>();
    let [owner] = owners_at_source.as_slice() else {
        builder.diagnostics.push(MirDiagnostic {
            kind: MirDiagnosticKind::NotYetImplemented {
                construct: "OwnedCursor argument without one caller owner".to_string(),
                site: arg.site,
            },
            note: "a borrowed closure parameter or ambiguous cursor lineage cannot be forwarded into an owner-minting direct callee"
                .to_string(),
        });
        return None;
    };
    let live_binding_owners = exit
        .keys()
        .filter(|candidate| candidate.binding == owner.binding)
        .count();
    if live_binding_owners != 1 {
        builder.diagnostics.push(MirDiagnostic {
            kind: MirDiagnosticKind::NotYetImplemented {
                construct: "ambiguous OwnedCursor owner generation".to_string(),
                site: arg.site,
            },
            note: "the caller must expose exactly one live owner generation at the cursor argument"
                .to_string(),
        });
        return None;
    }
    let flags = blocks
        .iter()
        .flat_map(|block| block.instructions.iter())
        .filter_map(|instruction| match instruction {
            Instr::OwnershipEvent(crate::model::OwnershipEvent::Guard {
                owner: guarded,
                flag,
                kind: crate::model::OwnershipGuardKind::VecIter,
            }) if *guarded == *owner => Some(*flag),
            _ => None,
        })
        .collect::<HashSet<_>>();
    if flags.len() != 1 {
        builder.diagnostics.push(MirDiagnostic {
            kind: MirDiagnosticKind::NotYetImplemented {
                construct: "OwnedCursor argument without one caller guard".to_string(),
                site: arg.site,
            },
            note: "the terminal handoff must disarm the exact guarded cursor release before invoke"
                .to_string(),
        });
        return None;
    }
    let flag = *flags.iter().next().expect("one VecIter guard flag");
    Some((*owner, flag))
}

fn owned_cursor_commit_for_arg(
    builder: &mut Builder,
    owner: crate::model::OwnerId,
    flag: Place,
    arg: &PendingOwnedCursorArg,
) -> Option<CursorCommit> {
    let Some((name, owned_ty)) = builder.owned_locals.iter().find_map(|entry| {
        (entry.binding == owner.binding).then(|| (entry.name.clone(), entry.ty.clone()))
    }) else {
        builder.diagnostics.push(MirDiagnostic {
            kind: MirDiagnosticKind::NotYetImplemented {
                construct: "OwnedCursor argument without a caller ledger entry".to_string(),
                site: arg.site,
            },
            note: "the checker Consume and ownership Transfer must name the same cursor binding"
                .to_string(),
        });
        return None;
    };
    if owned_ty != arg.ty {
        builder.diagnostics.push(MirDiagnostic {
            kind: MirDiagnosticKind::NotYetImplemented {
                construct: "OwnedCursor argument type mismatch".to_string(),
                site: arg.site,
            },
            note: format!(
                "caller owner has type `{}` but the direct parameter receives `{}`",
                owned_ty.user_facing(),
                arg.ty.user_facing()
            ),
        });
        return None;
    }
    Some(CursorCommit {
        owner,
        source: arg.source,
        flag,
        site: arg.site,
        name,
        ty: owned_ty,
    })
}

fn collect_owned_cursor_commits(
    blocks: &[BasicBlock],
    builder: &mut Builder,
    block_id: u32,
    call_args: &[Place],
    site: &PendingOwnedCallSite,
) -> Vec<CursorCommit> {
    let (_, owner_exits) = drop_plan::exact_owner_states(blocks);
    let exit = owner_exits.get(&block_id).cloned().unwrap_or_default();
    site.cursor_args
        .iter()
        .filter_map(|arg| {
            let (owner, flag) =
                owned_cursor_owner_and_guard(blocks, builder, &exit, call_args, arg)?;
            owned_cursor_commit_for_arg(builder, owner, flag, arg)
        })
        .collect()
}

fn commit_owned_cursor_args(
    block: &mut BasicBlock,
    builder: &mut Builder,
    commits: Vec<CursorCommit>,
    block_id: u32,
    anchor: PendingOwnedCallAnchor,
) -> CommittedOwnedCursorCall {
    let mut committed_args = Vec::with_capacity(commits.len());
    for commit in commits {
        block.statements.push(MirStatement::Use {
            binding: commit.owner.binding,
            name: commit.name,
            site: commit.site,
            ty: commit.ty,
            intent: IntentKind::Consume,
        });
        block.instructions.push(Instr::ConstI64 {
            dest: commit.flag,
            value: 1,
        });
        block.instructions.push(Instr::OwnershipEvent(
            crate::model::OwnershipEvent::Transfer {
                owner: commit.owner,
                from: commit.source,
                to: None,
                to_owner: None,
                to_ty: None,
            },
        ));
        builder.set_owned_local_consumed_post_lowering(
            commit.owner.binding,
            None,
            DischargeSite::BindingMoved,
        );
        committed_args.push(CommittedOwnedCursorArg {
            source: commit.source,
            flag: commit.flag,
            site: commit.site,
        });
    }
    CommittedOwnedCursorCall {
        block: block_id,
        anchor,
        args: committed_args,
    }
}

pub(super) fn prepare_owned_cursor_calls(
    blocks: &mut [BasicBlock],
    builder: &mut Builder,
    pending: &HashMap<u32, Vec<PendingOwnedCallSite>>,
    blocked: &HashSet<u32>,
) -> Vec<CommittedOwnedCursorCall> {
    let mut committed_calls = Vec::new();
    for block_index in 0..blocks.len() {
        let block_id = blocks[block_index].id;
        if blocked.contains(&block_id) {
            continue;
        }
        let Some(sites) = pending.get(&block_id) else {
            continue;
        };
        let [site] = sites.as_slice() else {
            continue;
        };
        if site.cursor_args.is_empty() {
            continue;
        }
        let Some(anchor) = site.anchor else {
            builder.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "OwnedCursor call without a typed invoke anchor".to_string(),
                    site: site.cursor_args[0].site,
                },
                note: "cursor ownership may commit only at a typed ordinary direct-call terminator"
                    .to_string(),
            });
            continue;
        };
        if anchor != PendingOwnedCallAnchor::DirectTerminator {
            builder.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "OwnedCursor call at an unsupported invoke anchor".to_string(),
                    site: site.cursor_args[0].site,
                },
                note: "only an ordinary direct Hew call has the callee entry owner required by OwnedCursor"
                    .to_string(),
            });
            continue;
        }
        let Terminator::Call { args, .. } = &blocks[block_index].terminator else {
            continue;
        };

        let mut duplicated = HashSet::new();
        let mut unique_sources = HashSet::new();
        for arg in &site.cursor_args {
            if !unique_sources.insert(arg.source) {
                duplicated.insert(arg.source);
            }
        }
        if let Some(arg) = site
            .cursor_args
            .iter()
            .find(|arg| duplicated.contains(&arg.source))
        {
            builder.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "duplicate OwnedCursor argument".to_string(),
                    site: arg.site,
                },
                note:
                    "one cursor owner cannot be transferred into two parameters of the same invoke"
                        .to_string(),
            });
            continue;
        }

        let diagnostics_before = builder.diagnostics.len();
        let commits = collect_owned_cursor_commits(blocks, builder, block_id, args, site);
        if builder.diagnostics.len() != diagnostics_before
            || commits.len() != site.cursor_args.len()
        {
            continue;
        }

        committed_calls.push(commit_owned_cursor_args(
            &mut blocks[block_index],
            builder,
            commits,
            block_id,
            anchor,
        ));
    }
    committed_calls
}

fn assert_owned_cursor_call_handoff_suffix(
    instructions: &[Instr],
    args: &[CommittedOwnedCursorArg],
) {
    let suffix_start = instructions.len() - args.len() * 2;
    for (arg, pair) in args
        .iter()
        .zip(instructions[suffix_start..].chunks_exact(2))
    {
        assert!(
            matches!(
                pair,
                [
                    Instr::ConstI64 { dest, value: 1 },
                    Instr::OwnershipEvent(crate::model::OwnershipEvent::Transfer {
                        from,
                        to: None,
                        ..
                    })
                ] if *dest == arg.flag && *from == arg.source
            ),
            "OwnedCursor transfer must be the final side-effecting sequence before invoke"
        );
    }
}

/// Re-anchor validated cursor commits after all later CFG materialisation.
pub(super) fn reanchor_owned_cursor_call_handoffs(
    blocks: &mut [BasicBlock],
    builder: &mut Builder,
    committed: &[CommittedOwnedCursorCall],
) {
    for call in committed {
        let block = blocks
            .iter_mut()
            .find(|block| block.id == call.block)
            .expect("a committed OwnedCursor call block must survive finalisation");
        assert!(
            matches!(
                (call.anchor, &block.terminator),
                (
                    PendingOwnedCallAnchor::DirectTerminator,
                    Terminator::Call { .. }
                )
            ),
            "OwnedCursor commit must remain anchored to its exact direct invoke"
        );

        let mut pair_indices = Vec::with_capacity(call.args.len());
        for arg in &call.args {
            let matches = block
                .instructions
                .windows(2)
                .enumerate()
                .filter_map(|(index, pair)| {
                    matches!(
                        pair,
                        [
                            Instr::ConstI64 { dest, value: 1 },
                            Instr::OwnershipEvent(crate::model::OwnershipEvent::Transfer {
                                from,
                                to: None,
                                ..
                            })
                        ] if *dest == arg.flag && *from == arg.source
                    )
                    .then_some(index)
                })
                .collect::<Vec<_>>();
            let [index] = matches.as_slice() else {
                panic!(
                    "OwnedCursor commit at {:?} must retain one exact flag/Transfer pair",
                    arg.site
                );
            };
            pair_indices.push(*index);
        }
        assert!(
            pair_indices.windows(2).all(|pair| pair[0] + 1 < pair[1]),
            "OwnedCursor commit pairs must remain distinct and argument ordered"
        );

        let mut pairs = Vec::with_capacity(call.args.len());
        for index in pair_indices.into_iter().rev() {
            let flag = block.instructions.remove(index);
            shift_instr_spans_on_remove(
                &mut builder.instr_spans,
                block.id,
                u32::try_from(index).unwrap_or(u32::MAX),
            );
            let transfer = block.instructions.remove(index);
            shift_instr_spans_on_remove(
                &mut builder.instr_spans,
                block.id,
                u32::try_from(index).unwrap_or(u32::MAX),
            );
            pairs.push((flag, transfer));
        }
        pairs.reverse();
        for (flag, transfer) in pairs {
            let insert_at = block.instructions.len();
            shift_instr_spans_on_insert(
                &mut builder.instr_spans,
                block.id,
                u32::try_from(insert_at).unwrap_or(u32::MAX),
            );
            block.instructions.push(flag);
            let insert_at = block.instructions.len();
            shift_instr_spans_on_insert(
                &mut builder.instr_spans,
                block.id,
                u32::try_from(insert_at).unwrap_or(u32::MAX),
            );
            block.instructions.push(transfer);
        }

        assert_owned_cursor_call_handoff_suffix(&block.instructions, &call.args);
    }
}
