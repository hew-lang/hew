use super::{
    base_local, instr_source_places, terminator_source_places, BTreeMap, BasicBlock, HashMap,
    HashSet, SuspendKind, Terminator,
};

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
