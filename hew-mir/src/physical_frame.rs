use std::collections::VecDeque;

use super::{
    defer, storage_uses, BTreeMap, BTreeSet, PhysicalError, PhysicalFunction, PhysicalModule,
    PhysicalOp, PhysicalTerminator, StorageId, StorageOrigin,
};

pub(super) fn retained_storage(
    module: &PhysicalModule,
    function: &PhysicalFunction,
) -> Result<BTreeSet<StorageId>, PhysicalError> {
    let callable = module
        .callables
        .get(function.callable.0 as usize)
        .ok_or_else(|| {
            PhysicalError::new("frame storage analysis encountered an unknown callable")
        })?;
    if !callable.is_resumable {
        return Ok(BTreeSet::new());
    }
    let mut indices = BTreeMap::new();
    let mut count = 0;
    for block in &function.blocks {
        indices.insert(block.id, count);
        count += 1 + block
            .ops
            .iter()
            .filter(|operation| operation_suspends(module, function, operation))
            .count();
    }
    let mut references = vec![BTreeSet::new(); function.storage.len()];
    let mut successors = vec![Vec::new(); count];
    let mut suspends = vec![false; count];
    let mut pinned = BTreeSet::new();
    for block in &function.blocks {
        let mut index = indices[&block.id];
        let mut arguments: BTreeSet<_> = block.arguments.iter().copied().collect();
        if block.id == function.entry {
            arguments.extend(&function.parameters);
        }
        record_references(function, arguments, index, &mut references)?;
        for operation in &block.ops {
            let used = operation_references(function, operation);
            if operation_suspends(module, function, operation) {
                pinned.extend(dependency_roots(function, used.clone())?);
                suspends[index] = true;
                successors[index].push(index + 1);
                record_references(function, used, index, &mut references)?;
                index += 1;
            } else {
                record_references(function, used, index, &mut references)?;
            }
        }
        let mut used = BTreeSet::new();
        storage_uses::terminator_storage(&block.terminator, &mut used);
        if terminator_suspends(module, &block.terminator)? {
            pinned.extend(dependency_roots(
                function,
                pinned_operands(function, &block.terminator, used.clone()),
            )?);
            suspends[index] = true;
        }
        record_references(function, used, index, &mut references)?;
        for edge in defer::edges(&block.terminator) {
            successors[index].push(*indices.get(&edge.target).ok_or_else(|| {
                PhysicalError::new("frame storage analysis encountered an unknown block")
            })?);
        }
    }
    let mut retained = pinned;
    for (index, accesses) in references.iter().enumerate() {
        if crosses_boundary(accesses, &successors, &suspends) {
            retained
                .insert(StorageId(u32::try_from(index).map_err(|_| {
                    PhysicalError::new("frame storage index exceeds u32")
                })?));
        }
    }
    Ok(retained)
}

fn operation_references(
    function: &PhysicalFunction,
    operation: &PhysicalOp,
) -> BTreeSet<StorageId> {
    let mut used = BTreeSet::new();
    let mut defined = BTreeSet::new();
    let mut locals = BTreeSet::new();
    match operation {
        PhysicalOp::EndBorrow { .. } | PhysicalOp::StorageLive { .. } => {}
        PhysicalOp::StorageDead {
            storage,
            destroy: None,
            cleanup,
        } => {
            if let Some(place) = function.place_storage.get(storage) {
                used.extend(
                    place
                        .leaves
                        .iter()
                        .filter(|leaf| {
                            leaf.destroy.is_some()
                                && cleanup.leaf(leaf.storage) != Some(super::LeafContents::Absent)
                        })
                        .map(|leaf| leaf.storage),
                );
            }
        }
        _ => {
            storage_uses::operation_storage(operation, &mut used, &mut defined, &mut locals);
        }
    }
    used.extend(defined);
    used
}

fn pinned_operands(
    function: &PhysicalFunction,
    term: &PhysicalTerminator,
    mut used: BTreeSet<StorageId>,
) -> BTreeSet<StorageId> {
    let copied: Vec<_> = match term {
        PhysicalTerminator::ActorCall {
            operation: super::ActorOperation::Spawn(_),
            args,
            ..
        }
        | PhysicalTerminator::ActorAsk { args, .. } => args
            .iter()
            .filter_map(|argument| match argument {
                super::ArgumentTransfer::Borrow(_) | super::ArgumentTransfer::BorrowMut(_) => None,
                super::ArgumentTransfer::Move(id) => Some(*id),
                super::ArgumentTransfer::Clone { source, .. } => Some(*source),
            })
            .collect(),
        PhysicalTerminator::Sleep { duration, .. } => vec![*duration],
        PhysicalTerminator::SleepUntil { deadline, .. } => vec![*deadline],
        _ => Vec::new(),
    };
    for id in copied {
        if function.storage[id.0 as usize].own == super::OwnKind::None {
            used.remove(&id);
        }
    }
    for edge in defer::edges(term) {
        used.extend(
            edge.transfers
                .iter()
                .chain(&edge.leaf_transfers)
                .flat_map(|(source, dest)| [*source, *dest]),
        );
    }
    used
}

fn record_references(
    function: &PhysicalFunction,
    used: BTreeSet<StorageId>,
    index: usize,
    references: &mut [BTreeSet<usize>],
) -> Result<(), PhysicalError> {
    for id in dependency_roots(function, used)? {
        references[id.0 as usize].insert(index);
    }
    Ok(())
}

fn crosses_boundary(
    accesses: &BTreeSet<usize>,
    successors: &[Vec<usize>],
    suspends: &[bool],
) -> bool {
    if accesses.is_empty() {
        return false;
    }
    let mut seen = [vec![false; successors.len()], vec![false; successors.len()]];
    let mut pending: VecDeque<_> = accesses.iter().map(|index| (*index, false)).collect();
    while let Some((index, crossed)) = pending.pop_front() {
        if std::mem::replace(&mut seen[usize::from(crossed)][index], true) {
            continue;
        }
        if crossed && accesses.contains(&index) {
            return true;
        }
        let crossed = crossed || suspends[index];
        pending.extend(successors[index].iter().map(|next| (*next, crossed)));
    }
    false
}

fn dependency_roots(
    function: &PhysicalFunction,
    mut used: BTreeSet<StorageId>,
) -> Result<BTreeSet<StorageId>, PhysicalError> {
    let mut pending: Vec<_> = used.iter().copied().collect();
    while let Some(id) = pending.pop() {
        let storage = function.storage.get(id.0 as usize).ok_or_else(|| {
            PhysicalError::new("frame storage analysis encountered an unknown storage")
        })?;
        let partition = function.place_storage.get(&id).map(|place| place.root);
        let owner = match storage.origin {
            StorageOrigin::ActorState { state, .. } => Some(state),
            StorageOrigin::Capture { environment, .. } => Some(environment),
            _ => None,
        };
        for dependency in partition
            .into_iter()
            .chain(storage.borrow_parent)
            .chain(owner)
        {
            if used.insert(dependency) {
                pending.push(dependency);
            }
        }
    }
    Ok(used)
}

fn operation_suspends(
    module: &PhysicalModule,
    function: &PhysicalFunction,
    operation: &PhysicalOp,
) -> bool {
    let release = match operation {
        PhysicalOp::Destroy { action, .. } => Some(*action),
        PhysicalOp::StorageDead { destroy, .. } => *destroy,
        PhysicalOp::Assign { destroy_old, .. } => *destroy_old,
        _ => None,
    };
    release.is_some_and(|action| module.releases.suspends(action))
        || matches!(operation, PhysicalOp::StorageDead { storage, destroy: None, cleanup }
            if function.place_storage.get(storage).is_some_and(|place| place.leaves.iter().any(|leaf|
                cleanup.leaf(leaf.storage) != Some(super::LeafContents::Absent)
                && leaf.destroy.is_some_and(|action| module.releases.suspends(action)))))
}

fn terminator_suspends(
    module: &PhysicalModule,
    term: &PhysicalTerminator,
) -> Result<bool, PhysicalError> {
    Ok(match term {
        PhysicalTerminator::Call { callee, .. } => module.callables[callee.0 as usize].is_resumable,
        PhysicalTerminator::ValueCall { ty, capability, .. } => {
            module.value_capabilities[&(ty.clone(), *capability)].is_resumable
        }
        PhysicalTerminator::DynCall { method, .. } => module
            .vtables
            .iter()
            .flat_map(|table| &table.slots)
            .filter(|slot| slot.method == *method)
            .any(|slot| module.callables[slot.callee.0 as usize].is_resumable),
        PhysicalTerminator::RuntimeCall { action, .. } => {
            super::runtime_receiver_release(module, action)?
                .is_some_and(|release| module.releases.suspends(release))
                || !action.family.value_callback_capabilities().is_empty()
                || matches!(action.carrier, super::PhysicalRuntimeCarrier::StructuralFormat(id)
                    if module.structural_glue[id.0 as usize].is_resumable)
        }
        PhysicalTerminator::StreamNext { .. }
        | PhysicalTerminator::StreamSend { .. }
        | PhysicalTerminator::ActorAsk { .. }
        | PhysicalTerminator::RemoteAsk { .. }
        | PhysicalTerminator::TaskSelect { .. }
        | PhysicalTerminator::GeneratorYield { .. }
        | PhysicalTerminator::GeneratorNext { .. }
        | PhysicalTerminator::TaskAwait { .. }
        | PhysicalTerminator::TaskScopeJoin { .. }
        | PhysicalTerminator::Offload { .. }
        | PhysicalTerminator::NativeIo { .. }
        | PhysicalTerminator::Sleep { .. }
        | PhysicalTerminator::SleepUntil { .. }
        | PhysicalTerminator::ActorCall { .. }
        | PhysicalTerminator::RecoverFault { .. }
        | PhysicalTerminator::IndirectCall { .. }
        | PhysicalTerminator::WireCodec { .. } => true,
        PhysicalTerminator::EnterDefer { .. }
        | PhysicalTerminator::FinishDefer { .. }
        | PhysicalTerminator::CleanupDispatch { .. }
        | PhysicalTerminator::CheckedRaiseFault { .. }
        | PhysicalTerminator::Return { .. }
        | PhysicalTerminator::Goto(_)
        | PhysicalTerminator::Branch { .. }
        | PhysicalTerminator::SwitchVariant { .. }
        | PhysicalTerminator::CheckedBinary { .. }
        | PhysicalTerminator::ExternCall { .. }
        | PhysicalTerminator::Panic { .. }
        | PhysicalTerminator::Trap(_)
        | PhysicalTerminator::PropagateFault { .. }
        | PhysicalTerminator::Unreachable => false,
    })
}
