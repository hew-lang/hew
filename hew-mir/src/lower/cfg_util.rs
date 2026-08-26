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
    let mut reachable = HashSet::new();
    let mut frontier = blocks
        .iter()
        .find(|block| block.id == move_block)
        .map(BasicBlock::successors)
        .unwrap_or_default();
    while let Some(block_id) = frontier.pop() {
        if !reachable.insert(block_id) {
            continue;
        }
        if let Some(block) = blocks.iter().find(|block| block.id == block_id) {
            frontier.extend(block.successors());
        }
    }

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
    let mut reachable = HashSet::new();
    let mut frontier = blocks
        .iter()
        .find(|block| block.id == release_block)
        .map(BasicBlock::successors)
        .unwrap_or_default();
    while let Some(block_id) = frontier.pop() {
        if !reachable.insert(block_id) {
            continue;
        }
        if let Some(block) = blocks.iter().find(|block| block.id == block_id) {
            frontier.extend(block.successors());
        }
    }

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
    let by_id: HashMap<u32, &BasicBlock> = blocks.iter().map(|b| (b.id, b)).collect();
    let mut seen: HashSet<u32> = HashSet::new();
    let mut work: Vec<u32> = by_id
        .get(&start)
        .map(|b| b.successors())
        .unwrap_or_default();
    while let Some(id) = work.pop() {
        if seen.insert(id) {
            if let Some(b) = by_id.get(&id) {
                work.extend(b.successors());
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
    loop {
        let mut changed = false;
        for &block in &reachable {
            if block == entry {
                continue;
            }
            let Some(preds) = predecessors.get(&block) else {
                dominators.remove(&block);
                continue;
            };
            let mut pred_dominators = preds
                .iter()
                .filter_map(|pred| dominators.get(pred).cloned());
            let Some(mut next) = pred_dominators.next() else {
                dominators.remove(&block);
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
