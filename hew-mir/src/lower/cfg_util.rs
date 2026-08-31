use super::{
    base_local, instr_source_places, terminator_source_places, BTreeMap, BasicBlock, HashMap,
    HashSet, SuspendKind, Terminator,
};
use crate::dataflow::instr_reads_writes;

pub(super) fn local_is_used_after(
    blocks: &[BasicBlock],
    suspend_kinds: &HashMap<u32, SuspendKind>,
    source_local: u32,
    move_block: u32,
    move_index: usize,
) -> bool {
    // The successor walk resolves a block id per pop. Scanning `blocks` for
    // each made the walk quadratic in block count, and this proof runs once per
    // candidate instruction.
    let successors = successors_by_id(blocks);
    let reachable = reachable_from(&successors, move_block);

    blocks.iter().any(|block| {
        let start = if block.id == move_block {
            move_index.saturating_add(1)
        } else if reachable.contains(&block.id) {
            0
        } else {
            return false;
        };
        block.instructions[start..].iter().any(|instr| {
            instr_source_places(instr)
                .into_iter()
                .any(|place| base_local(place) == Some(source_local))
        }) || terminator_source_places(&block.terminator, suspend_kinds.get(&block.id))
            .into_iter()
            .any(|place| base_local(place) == Some(source_local))
    })
}

/// Detect a later local generation without treating a loop's next dynamic
/// iteration as a rewrite of the generation released in this iteration.
pub(super) fn local_is_rewritten_after_current_iteration(
    blocks: &[BasicBlock],
    source_local: u32,
    release_block: u32,
    release_index: usize,
) -> bool {
    let successors = successors_by_id(blocks);
    let reachable = reachable_from(&successors, release_block);

    let mut reaches_release = HashSet::from([release_block]);
    loop {
        let predecessors: Vec<_> = blocks
            .iter()
            .filter(|block| {
                block
                    .successors()
                    .iter()
                    .any(|successor| reaches_release.contains(successor))
            })
            .map(|block| block.id)
            .collect();
        let before = reaches_release.len();
        reaches_release.extend(predecessors);
        if reaches_release.len() == before {
            break;
        }
    }

    let writes_local = |instructions: &[super::Instr]| {
        instructions.iter().any(|instruction| {
            instr_reads_writes(instruction)
                .1
                .iter()
                .any(|place| base_local(*place) == Some(source_local))
        })
    };
    blocks.iter().any(|block| {
        if block.id == release_block {
            block
                .instructions
                .get(release_index.saturating_add(1)..)
                .is_some_and(&writes_local)
        } else {
            reachable.contains(&block.id)
                && !reaches_release.contains(&block.id)
                && writes_local(&block.instructions)
        }
    })
}

/// The `next` continuation of a `Terminator::Call`, else `None`. The only
/// block-terminating shape a fresh-`string` producer or its borrowing use takes.
pub(super) fn call_terminator_next(term: &Terminator) -> Option<u32> {
    match term {
        Terminator::Call { next, .. } => Some(*next),
        _ => None,
    }
}
#[must_use]
pub(super) fn block_by_id(blocks: &[BasicBlock], id: u32) -> Option<&BasicBlock> {
    blocks.iter().find(|b| b.id == id)
}
/// Shift the `instr_spans` keys of `block_id` to account for an instruction
/// spliced into a sealed block at position `at`: every entry at index `>= at`
/// moves up by one. Post-seal splices (`apply_nested_fresh_string_temp_drops`'s
/// inline `hew_string_drop`, the `EnterContext` carrier in
/// `bracket_actor_handler_blocks`) mutate a block's `instructions` after the
/// per-block buffer was drained, shifting the positions the Stage 2 side-table
/// keys on; without this the per-statement line table would mis-attribute
/// every instruction after the splice. The spliced instruction itself is left
/// without an entry by design.
pub(super) fn shift_instr_spans_on_insert(
    instr_spans: &mut BTreeMap<(u32, u32), (u32, u32)>,
    block_id: u32,
    at: u32,
) {
    if instr_spans.is_empty() {
        return;
    }
    // Collect-then-reinsert: removing every shifted key before reinserting at
    // `idx + 1` avoids a transient collision (the vacated slots all lay at
    // `>= at`, the destinations at `> at`, and unshifted keys stay `< at`).
    let shifted: Vec<((u32, u32), (u32, u32))> = instr_spans
        .iter()
        .filter(|((bid, idx), _)| *bid == block_id && *idx >= at)
        .map(|(key, span)| (*key, *span))
        .collect();
    for (key, _) in &shifted {
        instr_spans.remove(key);
    }
    for ((bid, idx), span) in shifted {
        instr_spans.insert((bid, idx.saturating_add(1)), span);
    }
}

/// Realign instruction spans after removing one post-lowering instruction.
/// The removed operation may itself have no span, but every later authored
/// instruction still moves down by one and must retain its original source
/// location.
pub(super) fn shift_instr_spans_on_remove(
    instr_spans: &mut BTreeMap<(u32, u32), (u32, u32)>,
    block_id: u32,
    at: u32,
) {
    instr_spans.remove(&(block_id, at));
    let shifted: Vec<((u32, u32), (u32, u32))> = instr_spans
        .iter()
        .filter(|((bid, idx), _)| *bid == block_id && *idx > at)
        .map(|(key, span)| (*key, *span))
        .collect();
    for (key, _) in &shifted {
        instr_spans.remove(key);
    }
    for ((bid, idx), span) in shifted {
        instr_spans.insert((bid, idx.saturating_sub(1)), span);
    }
}
/// Block ids transitively reachable FROM `start` (via its successors; `start`
/// itself is included only when a cycle re-enters it).
pub(super) fn blocks_reachable_from(blocks: &[BasicBlock], start: u32) -> HashSet<u32> {
    reachable_from(&successors_by_id(blocks), start)
}

/// Each block's successor list, keyed by block id.
///
/// A caller that asks about reachability from many different starts builds this
/// once and calls [`reachable_from`]; rebuilding the map per query made such a
/// caller quadratic in block count.
pub(super) fn successors_by_id(blocks: &[BasicBlock]) -> HashMap<u32, Vec<u32>> {
    blocks
        .iter()
        .map(|block| (block.id, block.successors()))
        .collect()
}

/// Block ids that lie on a cycle: a block reachable from its own successors,
/// so its instructions re-execute for a second dynamic generation.
///
/// One successor map serves every block. Asking `blocks_reachable_from` per
/// block rebuilt that map for each question, which made the whole answer
/// quadratic in block count before any traversal.
pub(super) fn blocks_on_a_cycle(blocks: &[BasicBlock]) -> HashSet<u32> {
    let successors = successors_by_id(blocks);
    blocks
        .iter()
        .map(|block| block.id)
        .filter(|id| reachable_from(&successors, *id).contains(id))
        .collect()
}

/// Block ids transitively reachable FROM `start` over `successors`; `start`
/// itself appears only when a cycle re-enters it.
pub(super) fn reachable_from(successors: &HashMap<u32, Vec<u32>>, start: u32) -> HashSet<u32> {
    let mut seen: HashSet<u32> = HashSet::new();
    let mut work: Vec<u32> = successors.get(&start).cloned().unwrap_or_default();
    while let Some(id) = work.pop() {
        if seen.insert(id) {
            if let Some(next) = successors.get(&id) {
                work.extend(next.iter().copied());
            }
        }
    }
    seen
}

/// Dominator sets over a function's reachable blocks: `dominators[b]` is every
/// block that lies on EVERY path from the entry block to `b` (including `b`
/// itself). The classic iterative intersection fixpoint, seeded with the full
/// reachable set and narrowed to a fixed point.
///
/// Unreachable blocks carry no entry at all, and a reachable block whose
/// predecessor set is empty or unresolvable is dropped rather than reported
/// with a partial answer — a caller asking "does X dominate Y" about a block
/// with no entry gets `false`, which is the fail-closed direction for every
/// current consumer (a transfer is treated as CONDITIONAL, an alias as
/// unproven).
///
/// Returns an empty map when the block list is empty or carries duplicate ids,
/// so a malformed CFG can never yield a dominance claim.
pub(super) fn block_dominators(blocks: &[BasicBlock]) -> HashMap<u32, HashSet<u32>> {
    let _timing = crate::timing::stage("block_dominators");
    let Some(entry) = blocks.first().map(|block| block.id) else {
        return HashMap::new();
    };
    let by_id: HashMap<u32, &BasicBlock> = blocks.iter().map(|block| (block.id, block)).collect();
    if by_id.len() != blocks.len() {
        return HashMap::new();
    }

    let mut reachable = blocks_reachable_from(blocks, entry);
    reachable.insert(entry);
    let mut predecessors: HashMap<u32, HashSet<u32>> = HashMap::new();
    for block in blocks {
        for successor in block.successors() {
            if reachable.contains(&block.id) && reachable.contains(&successor) {
                predecessors.entry(successor).or_default().insert(block.id);
            }
        }
    }

    let mut dominators: HashMap<u32, HashSet<u32>> = reachable
        .iter()
        .copied()
        .map(|block| {
            if block == entry {
                (block, HashSet::from([entry]))
            } else {
                (block, reachable.clone())
            }
        })
        .collect();
    // Visit order for the fixpoint below. The iteration seeds every non-entry
    // block with the whole reachable set and only ever narrows it by
    // intersection, so it settles on the GREATEST fixed point and that answer
    // does not depend on the order. Order buys convergence speed: reverse
    // postorder reaches a block after its predecessors, so the whole set
    // settles in a couple of passes instead of one pass per level of the CFG.
    // Iterating the reachable HASH SET meant no order at all.
    let successors = successors_by_id(blocks);
    let mut order: Vec<u32> = Vec::with_capacity(reachable.len());
    let mut visited: HashSet<u32> = HashSet::from([entry]);
    let mut stack: Vec<(u32, usize)> = vec![(entry, 0)];
    while let Some(&(block, index)) = stack.last() {
        let outgoing = successors.get(&block).map_or(&[][..], Vec::as_slice);
        if index < outgoing.len() {
            if let Some(top) = stack.last_mut() {
                top.1 = index + 1;
            }
            let next = outgoing[index];
            if reachable.contains(&next) && visited.insert(next) {
                stack.push((next, 0));
            }
        } else {
            order.push(block);
            stack.pop();
        }
    }
    order.reverse();
    // A reachable id with no block of its own is never reached by the walk;
    // keep it in the iteration set so the loop below sees exactly the blocks it
    // saw before. Sorted, because a hash-set order here would make the removal
    // cascade below - and so a dominance answer on a malformed CFG - differ
    // between runs of the same compiler on the same input.
    let mut unwalked: Vec<u32> = reachable
        .iter()
        .copied()
        .filter(|id| !visited.contains(id))
        .collect();
    unwalked.sort_unstable();
    order.extend(unwalked);

    loop {
        let mut changed = false;
        for &block in &order {
            if block == entry {
                continue;
            }
            // Neither removal below is reachable from this function's own
            // construction: `reachable` is grown from successor lists, so every
            // non-entry member has a reachable predecessor recorded above, and
            // every reachable block is seeded with a dominator set. A probe that
            // panicked in both branches survived the whole `hew-mir` suite, both
            // compile-measure fixtures, and a 2 200-line real module. They stay
            // because the doc comment above promises them - a block with no
            // resolvable predecessor is dropped rather than reported with a
            // partial answer - and because a future caller may hand this a CFG
            // mid-rewrite. Recording the removal as a change keeps the loop
            // running until the cascade completes, so the answer would not
            // depend on where in `order` the removed block happened to sit.
            let Some(preds) = predecessors.get(&block) else {
                changed |= dominators.remove(&block).is_some();
                continue;
            };
            let mut pred_dominators = preds.iter().filter_map(|pred| dominators.get(pred));
            let Some(mut next) = pred_dominators.next().cloned() else {
                changed |= dominators.remove(&block).is_some();
                continue;
            };
            for pred_doms in pred_dominators {
                next.retain(|dominator| pred_doms.contains(dominator));
            }
            next.insert(block);
            if dominators.get(&block) != Some(&next) {
                dominators.insert(block, next);
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }
    dominators
}

#[cfg(test)]
mod tests {
    use super::{block_dominators, blocks_on_a_cycle, blocks_reachable_from};
    use crate::model::{BasicBlock, Place, Terminator};
    use std::collections::HashSet;

    fn goto(id: u32, target: u32) -> BasicBlock {
        BasicBlock {
            id,
            statements: Vec::new(),
            instructions: Vec::new(),
            terminator: Terminator::Goto { target },
        }
    }

    fn branch(id: u32, then_target: u32, else_target: u32) -> BasicBlock {
        BasicBlock {
            id,
            statements: Vec::new(),
            instructions: Vec::new(),
            terminator: Terminator::Branch {
                cond: Place::Local(0),
                then_target,
                else_target,
            },
        }
    }

    fn ret(id: u32) -> BasicBlock {
        BasicBlock {
            id,
            statements: Vec::new(),
            instructions: Vec::new(),
            terminator: Terminator::Return,
        }
    }

    fn dominators_of(blocks: &[BasicBlock], id: u32) -> Vec<u32> {
        let mut found: Vec<u32> = block_dominators(blocks)
            .get(&id)
            .cloned()
            .unwrap_or_default()
            .into_iter()
            .collect();
        found.sort_unstable();
        found
    }

    /// The join of two arms is dominated by the branch and by itself, and by
    /// neither arm. An intersection replaced by a union, or a fixpoint that
    /// stops after one predecessor, both put an arm in this answer.
    #[test]
    fn diamond_join_is_dominated_by_the_branch_and_neither_arm() {
        let blocks = vec![branch(0, 1, 2), goto(1, 3), goto(2, 3), ret(3)];

        assert_eq!(dominators_of(&blocks, 3), vec![0, 3]);
        assert_eq!(dominators_of(&blocks, 1), vec![0, 1]);
    }

    /// A loop header dominates its body; the body does not dominate the header,
    /// even though the header is reachable from it. Seeding the fixpoint with
    /// the empty set instead of the full reachable set inverts this.
    #[test]
    fn loop_header_dominates_its_body_and_the_body_does_not_dominate_the_header() {
        // 0 -> 1 (header) -> 2 (body) -> 1, and 1 -> 3 (exit).
        let blocks = vec![goto(0, 1), branch(1, 2, 3), goto(2, 1), ret(3)];

        assert_eq!(dominators_of(&blocks, 2), vec![0, 1, 2]);
        assert_eq!(dominators_of(&blocks, 1), vec![0, 1]);
    }

    /// Every path to the exit of a loop runs the header, so the header dominates
    /// the exit even though the exit is not inside the loop.
    #[test]
    fn loop_exit_is_dominated_by_the_header() {
        let blocks = vec![goto(0, 1), branch(1, 2, 3), goto(2, 1), ret(3)];

        assert_eq!(dominators_of(&blocks, 3), vec![0, 1, 3]);
    }

    /// A block no path from the entry reaches carries no dominator entry at all,
    /// which every consumer reads as "does not dominate" — the fail-closed
    /// direction.
    #[test]
    fn unreachable_block_carries_no_dominator_entry() {
        let blocks = vec![goto(0, 1), ret(1), goto(7, 1)];

        assert!(!block_dominators(&blocks).contains_key(&7));
    }

    /// Duplicate ids make "the block with id N" meaningless, so the whole answer
    /// is withheld rather than computed from whichever copy the map kept.
    #[test]
    fn duplicate_block_ids_yield_no_dominance_claim() {
        let blocks = vec![goto(0, 1), ret(1), ret(1)];

        assert!(block_dominators(&blocks).is_empty());
    }

    /// A successor id that names no block is reachable but never walked. Real
    /// blocks must keep the dominators they would have had without it, so a
    /// malformed or mid-rewrite CFG cannot silently weaken a dominance claim
    /// that a consumer reads as proof.
    #[test]
    fn a_successor_id_that_names_no_block_leaves_real_dominators_alone() {
        let with_phantom = vec![branch(0, 1, 900), goto(1, 2), ret(2)];
        let without = vec![branch(0, 1, 2), goto(1, 2), ret(2)];

        assert_eq!(dominators_of(&with_phantom, 1), vec![0, 1]);
        assert_eq!(dominators_of(&with_phantom, 2), vec![0, 1, 2]);
        assert_eq!(dominators_of(&without, 2), vec![0, 2]);
    }

    /// A block on a back edge re-executes; a block on a straight line does not.
    /// Reporting the whole loop's reachable set, rather than the blocks that
    /// reach themselves, would put the loop's exit in this answer.
    #[test]
    fn only_blocks_that_reach_themselves_are_on_a_cycle() {
        let blocks = vec![goto(0, 1), branch(1, 2, 3), goto(2, 1), ret(3)];

        assert_eq!(blocks_on_a_cycle(&blocks), HashSet::from([1, 2]));
    }

    #[test]
    fn a_straight_line_function_has_no_block_on_a_cycle() {
        let blocks = vec![goto(0, 1), goto(1, 2), ret(2)];

        assert!(blocks_on_a_cycle(&blocks).is_empty());
    }

    /// A self-loop is the smallest cycle there is, and the only one where the
    /// block is its own successor.
    #[test]
    fn a_self_looping_block_is_on_a_cycle() {
        let blocks = vec![goto(0, 1), branch(1, 1, 2), ret(2)];

        assert!(blocks_on_a_cycle(&blocks).contains(&1));
    }

    /// The documented contract: the start block appears in its own reachable set
    /// only when something branches back to it. Callers use that to tell "runs
    /// again" from "runs once".
    #[test]
    fn reachability_excludes_the_start_unless_a_cycle_re_enters_it() {
        let acyclic = vec![goto(0, 1), goto(1, 2), ret(2)];
        let cyclic = vec![goto(0, 1), branch(1, 2, 0), ret(2)];

        assert_eq!(blocks_reachable_from(&acyclic, 0), HashSet::from([1, 2]));
        assert_eq!(blocks_reachable_from(&cyclic, 0), HashSet::from([0, 1, 2]));
    }
}
