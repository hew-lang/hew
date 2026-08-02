use super::{
    base_local, instr_source_places, propagate_whole_value_alias_roots, terminator_source_places,
    BasicBlock, BindingId, ExitPath, HashMap, HashSet, SuspendKind, Terminator, ENTRY_BLOCK_ID,
};

/// Blocks that read a returned-member candidate through either its registered
/// local or a whole-value `Move` alias of that local.
///
/// Returned-member Goto re-admission is an eager release at a non-terminal
/// edge, so a later read through `alias = member` is just as disqualifying as
/// `member` itself. The move may be a compiler-generated slot handoff with no
/// independent retain; following only the registered local would free the
/// shared heap before the alias read. Ambiguous alias joins are evicted by
/// [`propagate_whole_value_alias_roots`], which leaves this proof fail-closed.
pub(super) fn returned_member_alias_read_blocks(
    blocks: &[BasicBlock],
    suspend_kinds: &HashMap<u32, SuspendKind>,
    candidate_locals: &HashMap<u32, BindingId>,
) -> HashMap<BindingId, HashSet<u32>> {
    let alias_roots = propagate_whole_value_alias_roots(blocks, candidate_locals.keys().copied());
    let mut read_blocks: HashMap<BindingId, HashSet<u32>> = HashMap::new();
    for block in blocks {
        let sources = block
            .instructions
            .iter()
            .flat_map(instr_source_places)
            .chain(terminator_source_places(
                &block.terminator,
                suspend_kinds.get(&block.id),
            ));
        for source in sources {
            let Some(root) = base_local(source).and_then(|local| alias_roots.get(&local)) else {
                continue;
            };
            let Some(binding) = candidate_locals.get(root) else {
                continue;
            };
            read_blocks.entry(*binding).or_default().insert(block.id);
        }
    }
    read_blocks
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum ReturnedMemberReAdmissionPath {
    Normal,
    Abandonment,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct ReturnedMemberReAdmission {
    pub(super) plan_index: usize,
    pub(super) block: u32,
    pub(super) path: ReturnedMemberReAdmissionPath,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct ReturnedMemberReAdmissionAmbiguity {
    pub(super) first: ReturnedMemberReAdmission,
    pub(super) second: ReturnedMemberReAdmission,
}

pub(super) fn returned_member_re_admission_path(exit: &ExitPath) -> ReturnedMemberReAdmissionPath {
    match exit {
        ExitPath::Panic { .. }
        | ExitPath::Cancel { .. }
        | ExitPath::Yield { .. }
        | ExitPath::Suspend { .. } => ReturnedMemberReAdmissionPath::Abandonment,
        ExitPath::Return { .. }
        | ExitPath::Goto { .. }
        | ExitPath::Branch { .. }
        | ExitPath::Call { .. }
        | ExitPath::Send { .. }
        | ExitPath::Ask { .. }
        | ExitPath::Select { .. }
        | ExitPath::Join { .. } => ReturnedMemberReAdmissionPath::Normal,
    }
}

/// Blocks reachable from `start` without executing `removed`.
fn blocks_reachable_without(blocks: &[BasicBlock], start: u32, removed: u32) -> HashSet<u32> {
    if start == removed {
        return HashSet::new();
    }
    let by_id: HashMap<u32, &BasicBlock> = blocks.iter().map(|block| (block.id, block)).collect();
    let mut reachable = HashSet::new();
    let mut worklist = vec![start];
    while let Some(block) = worklist.pop() {
        if block == removed || !reachable.insert(block) {
            continue;
        }
        if let Some(block) = by_id.get(&block) {
            worklist.extend(block.successors());
        }
    }
    reachable
}

/// Whether every terminating normal path from `start` executes `postdominator`.
fn block_postdominates(blocks: &[BasicBlock], start: u32, postdominator: u32) -> bool {
    if start == postdominator {
        return true;
    }
    let bypass = blocks_reachable_without(blocks, start, postdominator);
    !blocks
        .iter()
        .any(|block| bypass.contains(&block.id) && block.successors().is_empty())
}

/// Whether every entry path to `block` executes `dominator`.
fn block_dominates(blocks: &[BasicBlock], dominator: u32, block: u32) -> bool {
    dominator == block
        || !blocks_reachable_without(blocks, ENTRY_BLOCK_ID, dominator).contains(&block)
}

/// Whether every normal continuation after a release in `block` executes
/// `postdominator`.
fn block_postdominates_release(blocks: &[BasicBlock], block: u32, postdominator: u32) -> bool {
    let Some(block) = blocks.iter().find(|candidate| candidate.id == block) else {
        return false;
    };
    let successors = block.successors();
    !successors.is_empty()
        && successors
            .into_iter()
            .all(|successor| block_postdominates(blocks, successor, postdominator))
}

/// Whether a normal Goto release executes before an abandonment release.
///
/// An abandonment edge leaves before its source block's terminator, so releases
/// attached to that same block remain independent. A release on an earlier Goto
/// is instead already complete on every path that reaches the later edge.
pub(super) fn normal_goto_precedes_abandonment(
    blocks: &[BasicBlock],
    block_reach: &HashMap<u32, HashSet<u32>>,
    normal: ReturnedMemberReAdmission,
    abandonment: ReturnedMemberReAdmission,
) -> bool {
    if !matches!(normal.path, ReturnedMemberReAdmissionPath::Normal)
        || !matches!(abandonment.path, ReturnedMemberReAdmissionPath::Abandonment)
        || normal.block == abandonment.block
    {
        return false;
    }
    let normal_is_goto = blocks.iter().any(|block| {
        block.id == normal.block && matches!(block.terminator, Terminator::Goto { .. })
    });
    normal_is_goto
        && block_reach
            .get(&normal.block)
            .is_some_and(|reachable| reachable.contains(&abandonment.block))
        && block_dominates(blocks, normal.block, abandonment.block)
}

/// Select a path-compatible subset of locally valid returned-member releases.
///
/// For two candidates `A -> B` on the same normal path:
///
/// * prefer `B` only when it postdominates A's Goto continuation (so no path
///   covered by A can bypass the later release);
/// * prefer `A` only when it dominates B (so every path reaching B already
///   crossed A);
/// * if neither proof holds, suppress both. Keeping both double-releases their
///   overlap, while keeping either one alone leaks the other side of the
///   partial overlap.
///
/// Incomparable sibling-arm candidates are all retained.
pub(super) fn select_returned_member_re_admissions(
    blocks: &[BasicBlock],
    block_reach: &HashMap<u32, HashSet<u32>>,
    candidates: &[ReturnedMemberReAdmission],
) -> Result<Vec<ReturnedMemberReAdmission>, ReturnedMemberReAdmissionAmbiguity> {
    let mut suppressed = HashSet::new();
    for upstream in candidates {
        for downstream in candidates {
            if upstream.plan_index == downstream.plan_index {
                continue;
            }
            if matches!(upstream.path, ReturnedMemberReAdmissionPath::Abandonment)
                || matches!(downstream.path, ReturnedMemberReAdmissionPath::Abandonment)
            {
                continue;
            }
            let reaches_downstream = block_reach
                .get(&upstream.block)
                .is_some_and(|reachable| reachable.contains(&downstream.block));
            if !reaches_downstream {
                continue;
            }
            if block_postdominates_release(blocks, upstream.block, downstream.block) {
                suppressed.insert(upstream.plan_index);
            } else if block_dominates(blocks, upstream.block, downstream.block) {
                suppressed.insert(downstream.plan_index);
            } else {
                return Err(ReturnedMemberReAdmissionAmbiguity {
                    first: *upstream,
                    second: *downstream,
                });
            }
        }
    }
    // Resolve normal releases before comparing them to abandonment exits. A
    // later normal release can replace the Goto that otherwise dominates a
    // cancellation; that cancellation then bypasses the surviving normal owner
    // and must retain its own plan.
    let surviving_normal_gotos: Vec<_> = candidates
        .iter()
        .copied()
        .filter(|candidate| {
            !suppressed.contains(&candidate.plan_index)
                && matches!(candidate.path, ReturnedMemberReAdmissionPath::Normal)
        })
        .collect();
    for abandonment in candidates
        .iter()
        .copied()
        .filter(|candidate| matches!(candidate.path, ReturnedMemberReAdmissionPath::Abandonment))
    {
        if surviving_normal_gotos.iter().any(|normal| {
            normal_goto_precedes_abandonment(blocks, block_reach, *normal, abandonment)
        }) {
            suppressed.insert(abandonment.plan_index);
        }
    }
    Ok(candidates
        .iter()
        .copied()
        .filter(|candidate| !suppressed.contains(&candidate.plan_index))
        .collect())
}

/// Existing release plans a candidate can safely subsume.
///
/// Existing plans are fixed until a selected candidate proves it can replace
/// them. A downstream candidate may replace an existing release only when it
/// postdominates every continuation after that release. An upstream candidate
/// may replace an existing release only when it dominates the existing site and
/// the existing release does not already postdominate all of the candidate's
/// continuations. Any partial overlap rejects the candidate: retaining both
/// would double-release their common path, while removing the existing plan
/// would leak a path the candidate does not cover.
pub(super) fn existing_releases_replaced_by_candidate(
    blocks: &[BasicBlock],
    block_reach: &HashMap<u32, HashSet<u32>>,
    candidate: ReturnedMemberReAdmission,
    existing: &[ReturnedMemberReAdmission],
) -> Result<Option<HashSet<usize>>, ReturnedMemberReAdmissionAmbiguity> {
    let mut replaced = HashSet::new();
    for release in existing {
        if matches!(candidate.path, ReturnedMemberReAdmissionPath::Abandonment)
            || matches!(release.path, ReturnedMemberReAdmissionPath::Abandonment)
        {
            continue;
        }
        let release_reaches_candidate = block_reach
            .get(&release.block)
            .is_some_and(|reachable| reachable.contains(&candidate.block));
        let candidate_reaches_release = block_reach
            .get(&candidate.block)
            .is_some_and(|reachable| reachable.contains(&release.block));

        if release_reaches_candidate {
            if block_postdominates_release(blocks, release.block, candidate.block) {
                replaced.insert(release.plan_index);
            } else {
                return Err(ReturnedMemberReAdmissionAmbiguity {
                    first: *release,
                    second: candidate,
                });
            }
        } else if candidate_reaches_release {
            if block_postdominates_release(blocks, candidate.block, release.block) {
                // The existing release already covers every path leaving this
                // candidate; adding the candidate would only release earlier.
                return Ok(None);
            }
            if block_dominates(blocks, candidate.block, release.block) {
                replaced.insert(release.plan_index);
            } else {
                return Err(ReturnedMemberReAdmissionAmbiguity {
                    first: candidate,
                    second: *release,
                });
            }
        }
    }
    Ok(Some(replaced))
}
