//! Bounded, call-free scalar and read-only vector regions that can execute outside a coroutine frame.

use hew_sir::{compute_indexed_dominators, CfgIndex, EdgeRef, SuccessorSlot};

use super::{
    defer::edges, storage_uses::operation_storage, storage_uses::terminator_effects,
    storage_uses::terminator_storage, ArgumentTransfer, BlockId, CloneAction, OwnKind,
    PhysicalBlock, PhysicalConst, PhysicalFunction, PhysicalModule, PhysicalOp, PhysicalRepr,
    PhysicalRuntimeCarrier, PhysicalTerminator, PhysicalVectorOp, StorageId, StorageOrigin,
};
use std::collections::{BTreeMap, BTreeSet};

/// A natural loop with one scheduling boundary. Its checked failures leave the
/// region before raising a fault; no call or resource operation can observe
/// its private scalar storage. Vector handles are invariant borrowed inputs. This is an optimization eligibility
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
    /// Select innermost call-free loops whose scheduling ticks can be batched
    /// without moving calls, checked failures or cancellation past a poll.
    #[must_use]
    pub fn loop_chunks(&self, module: &PhysicalModule) -> Vec<PhysicalLoopChunk> {
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
            if let Some(chunk) = self.scalar_chunk(header.id, region, &blocks, module) {
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
        module: &PhysicalModule,
    ) -> Option<PhysicalLoopChunk> {
        // A scalar vector read cannot call cloning glue. Its owner remains in
        // the coroutine; the chunk only carries an invariant borrowed handle.
        let mut readers = BTreeSet::new();
        let mut borrows = BTreeMap::new();
        for id in &region {
            for op in &blocks[id].ops {
                if let PhysicalOp::Borrow { dest, source } = op {
                    borrows.insert(*dest, *source);
                }
            }
            if let PhysicalTerminator::RuntimeCall {
                action,
                args,
                result: Some(result),
                ..
            } = &blocks[id].terminator
            {
                let PhysicalRuntimeCarrier::Vector {
                    operation: PhysicalVectorOp::Index | PhysicalVectorOp::IndexBorrow,
                    glue,
                } = action.carrier
                else {
                    return None;
                };
                let glue = &module.vector_glue[glue.0 as usize];
                if !glue.has_inline_scalar_access(module.target.layout(&glue.element.ty)?)
                    || !matches!(
                        self.storage[result.0 as usize].layout.repr,
                        PhysicalRepr::Integer { .. }
                    )
                {
                    return None;
                }
                let [ArgumentTransfer::Borrow(receiver), _] = &args[..] else {
                    return None;
                };
                readers.insert(*receiver);
            }
        }
        let mut pending = readers.iter().copied().collect::<Vec<_>>();
        while let Some(id) = pending.pop() {
            if let Some(parent) = borrows.get(&id) {
                if readers.insert(*parent) {
                    pending.push(*parent);
                }
            }
        }
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
                    PhysicalOp::Borrow { dest, source }
                        if readers.contains(dest) && readers.contains(source) => {}
                    PhysicalOp::EndBorrow { source } if readers.contains(source) => {}
                    _ => return None,
                }
                operation_storage(op, &mut used, &mut defined, &mut locals);
            }
            match &block.terminator {
                PhysicalTerminator::Goto(_) | PhysicalTerminator::Branch { .. } => {}
                PhysicalTerminator::CheckedBinary { result, .. } => {
                    defined.insert(*result);
                }
                PhysicalTerminator::RuntimeCall {
                    args,
                    result: Some(result),
                    ..
                } => {
                    // The first pass checked the complete call-free read recipe.
                    terminator_storage(&block.terminator, &mut used);
                    defined.insert(*result);
                    if args
                        .iter()
                        .any(|arg| matches!(arg, ArgumentTransfer::BorrowMut(_)))
                    {
                        return None;
                    }
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
        storage.extend(destinations.iter().copied());
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
                // Ending a loan has no physical read or cleanup action. The
                // original caller retains the loan and owner on failure exits.
                if matches!(op, PhysicalOp::EndBorrow { source } if readers.contains(source)) {
                    continue;
                }
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
            if readers.contains(id) {
                return !matches!(slot.layout.repr, PhysicalRepr::Pointer)
                    || destinations.contains(id)
                    || (defined.contains(id) && !borrows.contains_key(id));
            }
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
        // Outlining must retain immutable facts already visible to the caller,
        // including scalar fields projected from a constructed aggregate.
        let mut definitions: BTreeMap<StorageId, Option<&PhysicalOp>> = BTreeMap::new();
        let mut transferred = BTreeSet::new();
        let mut borrowed = BTreeSet::new();
        // A loan can expose aggregate memory to mutation. Do not reconstruct
        // fields through either the recorded alias or its owner, even for a
        // shared loan; range construction needs no escaping aggregate loans.
        for slot in &self.storage {
            if let Some(parent) = slot.borrow_parent {
                borrowed.extend([slot.id, parent]);
            }
        }
        for block in &self.blocks {
            transferred.extend(&block.arguments);
            terminator_effects(
                &block.terminator,
                &mut BTreeSet::new(),
                &mut transferred,
                &mut borrowed,
            );
            for edge in edges(&block.terminator) {
                transferred.extend(
                    edge.transfers
                        .iter()
                        .chain(&edge.leaf_transfers)
                        .map(|(_, dest)| *dest),
                );
            }
            for op in &block.ops {
                let mut outputs = BTreeSet::new();
                operation_storage(op, &mut BTreeSet::new(), &mut outputs, &mut BTreeSet::new());
                if let PhysicalOp::Assign { dest, .. } = op {
                    outputs.insert(*dest);
                }
                if let PhysicalOp::Borrow { dest, source } = op {
                    borrowed.extend([*dest, *source]);
                }
                if let PhysicalOp::TaskInvoke { callee, args, .. } = op {
                    for argument in callee.receiver().iter().chain(args) {
                        if let ArgumentTransfer::Borrow(source)
                        | ArgumentTransfer::BorrowMut(source) = argument
                        {
                            borrowed.insert(*source);
                        }
                    }
                }
                for dest in outputs {
                    definitions
                        .entry(dest)
                        .and_modify(|definition| *definition = None)
                        .or_insert(Some(op));
                }
            }
        }
        transferred.extend(borrowed);
        let constants = inputs
            .iter()
            .filter_map(|dest| {
                self.immutable_scalar_constant(*dest, &definitions, &transferred)
                    .map(|value| PhysicalOp::Const { dest: *dest, value })
            })
            .collect::<Vec<_>>();
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

    fn immutable_scalar_constant(
        &self,
        id: StorageId,
        definitions: &BTreeMap<StorageId, Option<&PhysicalOp>>,
        transferred: &BTreeSet<StorageId>,
    ) -> Option<PhysicalConst> {
        let definition = |id: StorageId| {
            if !matches!(self.storage[id.0 as usize].origin, StorageOrigin::Value(_))
                || transferred.contains(&id)
            {
                None
            } else {
                definitions.get(&id).copied().flatten()
            }
        };
        match definition(id)? {
            PhysicalOp::Const { value, .. } => Some(value.clone()),
            PhysicalOp::Clone {
                source,
                action: CloneAction::Bitwise,
                ..
            }
            | PhysicalOp::Transfer { source, .. } => {
                self.immutable_scalar_constant(*source, definitions, transferred)
            }
            PhysicalOp::AggregateProjectCopy {
                aggregate,
                field,
                action: CloneAction::Bitwise,
                ..
            } => {
                let PhysicalOp::AggregateMake { fields, .. } = definition(*aggregate)? else {
                    return None;
                };
                self.immutable_scalar_constant(
                    *fields.get(*field as usize)?,
                    definitions,
                    transferred,
                )
            }
            PhysicalOp::Cast { source, .. }
                if self.storage[id.0 as usize].layout.repr
                    == self.storage[source.0 as usize].layout.repr =>
            {
                // Equal-width integer casts preserve the physical bit pattern;
                // all narrowing, extension and floating conversions stay inputs.
                let value = self.immutable_scalar_constant(*source, definitions, transferred)?;
                matches!(value, PhysicalConst::IntegerBits(_)).then_some(value)
            }
            _ => None,
        }
    }
}
