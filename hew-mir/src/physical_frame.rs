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
    let indices: BTreeMap<_, _> = function
        .blocks
        .iter()
        .enumerate()
        .map(|(index, block)| (block.id, index))
        .collect();
    let mut references = vec![BTreeSet::new(); function.storage.len()];
    let mut successors = vec![Vec::new(); function.blocks.len()];
    let mut suspends = vec![false; function.blocks.len()];
    for (index, block) in function.blocks.iter().enumerate() {
        let mut used = BTreeSet::new();
        let mut defined = BTreeSet::new();
        let mut locals = BTreeSet::new();
        used.extend(&block.arguments);
        if block.id == function.entry {
            used.extend(&function.parameters);
        }
        for operation in &block.ops {
            storage_uses::operation_storage(operation, &mut used, &mut defined, &mut locals);
            suspends[index] |= operation_suspends(module, operation);
        }
        storage_uses::terminator_storage(&block.terminator, &mut used);
        used.extend(defined);
        used.extend(locals);
        for id in dependency_roots(function, used)? {
            references[id.0 as usize].insert(index);
        }
        suspends[index] |= terminator_suspends(module, &block.terminator)?;
        for edge in defer::edges(&block.terminator) {
            successors[index].push(*indices.get(&edge.target).ok_or_else(|| {
                PhysicalError::new("frame storage analysis encountered an unknown block")
            })?);
        }
    }
    let mut retained = BTreeSet::new();
    for (index, accesses) in references.iter().enumerate() {
        if accesses.iter().any(|block| suspends[*block])
            || crosses_boundary(accesses, &successors, &suspends)
        {
            retained
                .insert(StorageId(u32::try_from(index).map_err(|_| {
                    PhysicalError::new("frame storage index exceeds u32")
                })?));
        }
    }
    Ok(retained)
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

fn operation_suspends(module: &PhysicalModule, operation: &PhysicalOp) -> bool {
    let release = match operation {
        PhysicalOp::Destroy { action, .. } => Some(*action),
        PhysicalOp::StorageDead { destroy, .. } => *destroy,
        PhysicalOp::Assign { destroy_old, .. } => *destroy_old,
        _ => None,
    };
    release.is_some_and(|action| module.releases.suspends(action))
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
