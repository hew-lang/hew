//! Bounded, call-free scalar regions that can execute outside a coroutine frame.

use hew_sir::{compute_indexed_dominators, CfgIndex, EdgeRef, SuccessorSlot};

use super::{
    defer::edges, storage_uses::operation_storage, storage_uses::terminator_storage, BlockId,
    CloneAction, OwnKind, PhysicalBlock, PhysicalFunction, PhysicalOp, PhysicalRepr,
    PhysicalTerminator, StorageId, StorageOrigin,
};
use std::collections::{BTreeMap, BTreeSet};

/// A natural loop with one scheduling boundary. Its checked failures leave the
/// region before raising a fault; no call, alias or resource operation can
/// observe its private scalar storage. This is an optimization eligibility
/// decision over physical CFG facts, not a change to suspension semantics.
#[derive(Debug)]
pub struct PhysicalLoopChunk {
    pub header: BlockId,
    pub blocks: BTreeSet<BlockId>,
    pub storage: BTreeSet<StorageId>,
    /// Initialized carriers available at the header, including mutable locals.
    pub inputs: BTreeSet<StorageId>,
    /// Immutable SSA constants needed by the region, retained across outlining.
    pub constants: Vec<PhysicalOp>,
    /// Targets reached after applying their incoming edge transfers.
    pub exits: BTreeMap<BlockId, BTreeSet<StorageId>>,
}

impl PhysicalFunction {
    /// Select innermost scalar loops whose scheduling ticks can be batched
    /// without moving calls, checked failures or cancellation past a poll.
    #[must_use]
    pub fn loop_chunks(&self) -> Vec<PhysicalLoopChunk> {
        let mut successors = Vec::new();
        for block in &self.blocks {
            for (slot, edge) in edges(&block.terminator).into_iter().enumerate() {
                let Ok(slot) = u32::try_from(slot) else {
                    return Vec::new();
                };
                successors.push((
                    EdgeRef {
                        source: block.id,
                        slot: SuccessorSlot(slot),
                    },
                    edge.target,
                ));
            }
        }
        let cfg = CfgIndex::from_edges(
            self.entry,
            self.blocks.iter().map(|block| block.id),
            successors,
        );
        let dominance = compute_indexed_dominators(&cfg);
        let blocks = self
            .blocks
            .iter()
            .map(|block| (block.id, block))
            .collect::<BTreeMap<_, _>>();
        let mut chunks = Vec::new();
        for header in &self.blocks {
            let PhysicalTerminator::Safepoint { normal, .. } = &header.terminator else {
                continue;
            };
            if !header.ops.is_empty() || !cfg.is_reachable(header.id) {
                continue;
            }
            let tails = cfg
                .predecessors_of(header.id)
                .iter()
                .filter(|edge| {
                    cfg.is_reachable(edge.source) && dominance.dominates(header.id, edge.source)
                })
                .map(|edge| edge.source)
                .collect::<Vec<_>>();
            if tails.is_empty() {
                continue;
            }
            let mut region = BTreeSet::from([header.id]);
            let mut pending = tails;
            while let Some(id) = pending.pop() {
                if region.insert(id) {
                    pending.extend(cfg.predecessors_of(id).iter().map(|edge| edge.source));
                }
            }
            if !region.contains(&normal.target)
                || matches!(&header.terminator, PhysicalTerminator::Safepoint { cancel: Some(cancel), .. } if region.contains(&cancel.target))
                || region.iter().any(|id| {
                    !dominance.dominates(header.id, *id)
                        || (*id != header.id
                            && cfg
                                .predecessors_of(*id)
                                .iter()
                                .any(|edge| !region.contains(&edge.source)))
                })
            {
                continue;
            }
            if let Some(chunk) = self.scalar_chunk(header.id, region, &blocks) {
                chunks.push(chunk);
            }
        }
        chunks
    }

    #[expect(
        clippy::too_many_lines,
        reason = "one eligibility contract keeps permitted operations and the complete scalar interface together"
    )]
    fn scalar_chunk(
        &self,
        header: BlockId,
        region: BTreeSet<BlockId>,
        blocks: &BTreeMap<BlockId, &PhysicalBlock>,
    ) -> Option<PhysicalLoopChunk> {
        let mut used = BTreeSet::new();
        let mut defined = BTreeSet::new();
        let mut locals = BTreeSet::new();
        let mut exits = BTreeMap::<_, BTreeSet<_>>::new();
        let mut destinations = BTreeSet::new();
        for id in &region {
            let block = blocks[id];
            if *id != header {
                defined.extend(&block.arguments);
            }
            for op in &block.ops {
                match op {
                    PhysicalOp::Const { .. }
                    | PhysicalOp::Unary { .. }
                    | PhysicalOp::Binary { .. }
                    | PhysicalOp::Cast { .. }
                    | PhysicalOp::Transfer { .. }
                    | PhysicalOp::Clone {
                        action: CloneAction::Bitwise,
                        ..
                    }
                    | PhysicalOp::Assign {
                        destroy_old: None, ..
                    }
                    | PhysicalOp::StorageLive { .. }
                    | PhysicalOp::StorageDead { destroy: None, .. } => {}
                    _ => return None,
                }
                operation_storage(op, &mut used, &mut defined, &mut locals);
            }
            match &block.terminator {
                PhysicalTerminator::Goto(_) | PhysicalTerminator::Branch { .. } => {}
                PhysicalTerminator::CheckedBinary { result, .. } => {
                    defined.insert(*result);
                }
                PhysicalTerminator::Safepoint { .. } if *id == header => {}
                _ => return None,
            }
            // Cancellation is emitted in the coroutine, never in the chunk.
            let outgoing = match &block.terminator {
                PhysicalTerminator::Safepoint { normal, .. } => vec![normal],
                term => edges(term),
            };
            for edge in outgoing {
                if !edge.leaf_transfers.is_empty() {
                    return None;
                }
                for (source, dest) in &edge.transfers {
                    used.insert(*source);
                    destinations.insert(*dest);
                }
                if !region.contains(&edge.target) {
                    exits
                        .entry(edge.target)
                        .or_default()
                        .extend(edge.transfers.iter().map(|(_, dest)| *dest));
                }
            }
            match &block.terminator {
                PhysicalTerminator::Branch { condition, .. } => {
                    used.insert(*condition);
                }
                PhysicalTerminator::CheckedBinary { lhs, rhs, .. } => {
                    used.extend([*lhs, *rhs]);
                }
                _ => {}
            }
        }
        defined.extend(&locals);
        let mut storage = used.union(&defined).copied().collect::<BTreeSet<_>>();
        storage.extend(&blocks[&header].arguments);
        storage.extend(destinations);
        let mut inputs = used.difference(&defined).copied().collect::<BTreeSet<_>>();
        inputs.extend(&blocks[&header].arguments);
        // An escaping SSA result needs an explicit exit carrier. Mutable locals
        // already reside in inputs; internal values remain private to the chunk.
        for block in self
            .blocks
            .iter()
            .filter(|block| !region.contains(&block.id))
        {
            let mut outside = BTreeSet::new();
            let mut outputs = BTreeSet::new();
            let mut live = BTreeSet::new();
            for op in &block.ops {
                operation_storage(op, &mut outside, &mut outputs, &mut live);
            }
            terminator_storage(&block.terminator, &mut outside);
            outside.retain(|id| !outputs.contains(id) && !block.arguments.contains(id));
            if outside.iter().any(|id| defined.contains(id)) {
                return None;
            }
        }
        if storage.iter().any(|id| {
            let slot = &self.storage[id.0 as usize];
            slot.own != OwnKind::None
                || slot.borrow_parent.is_some()
                || !matches!(slot.layout.repr, PhysicalRepr::Integer { .. })
                || !matches!(
                    slot.origin,
                    StorageOrigin::Parameter(_)
                        | StorageOrigin::BlockArgument(_)
                        | StorageOrigin::Value(_)
                        | StorageOrigin::Local(_)
                )
                || self.place_storage.get(id).is_some_and(|place| {
                    place.root != *id
                        || !place.path.is_empty()
                        || place.leaves.len() != 1
                        || place.leaves[0].storage != *id
                        || place.leaves[0].destroy.is_some()
                })
        }) {
            return None;
        }
        let constants = self.blocks.iter().filter(|block| !region.contains(&block.id))
            .flat_map(|block| &block.ops).filter(|op| matches!(op, PhysicalOp::Const { dest, .. } if inputs.contains(dest) && matches!(self.storage[dest.0 as usize].origin, StorageOrigin::Value(_))))
            .cloned().collect::<Vec<_>>();
        for op in &constants {
            if let PhysicalOp::Const { dest, .. } = op {
                inputs.remove(dest);
            }
        }
        Some(PhysicalLoopChunk {
            header,
            blocks: region,
            storage,
            inputs,
            constants,
            exits,
        })
    }
}
