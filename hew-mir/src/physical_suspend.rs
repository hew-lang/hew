//! Resumable callable ABI selection from explicit typed control flow.

use super::{CallableId, PhysicalError, PhysicalModule, PhysicalTerminator};
use std::collections::{BTreeMap, BTreeSet};

pub(super) fn verify_task_scopes(function: &super::PhysicalFunction) -> Result<(), PhysicalError> {
    use super::{PhysicalOp, TaskScopeId};
    let mut declared = BTreeSet::new();
    for op in function.blocks.iter().flat_map(|block| &block.ops) {
        if let PhysicalOp::TaskScopeEnter { scope, .. } = op {
            if !declared.insert(*scope) {
                return Err(PhysicalError::new(
                    "physical task scope has duplicate storage declarations",
                ));
            }
        }
    }
    let blocks: BTreeMap<_, _> = function
        .blocks
        .iter()
        .map(|block| (block.id, block))
        .collect();
    let mut states = BTreeMap::new();
    states.insert(function.entry, Vec::<(TaskScopeId, bool)>::new());
    let mut pending = vec![function.entry];
    while let Some(id) = pending.pop() {
        let Some(block) = blocks.get(&id) else {
            continue;
        };
        let mut stack = states[&id].clone();
        for op in &block.ops {
            match op {
                PhysicalOp::TaskScopeEnter { scope, parent, .. } => {
                    if stack.last().map(|(scope, _)| scope) != parent.as_ref()
                        || stack.iter().any(|(active, _)| active == scope)
                    {
                        return Err(PhysicalError::new(
                            "physical task scope has inconsistent cancellation ancestry",
                        ));
                    }
                    stack.push((*scope, false));
                }
                PhysicalOp::TaskSpawn { scope, .. } if stack.last() != Some(&(*scope, false)) => {
                    return Err(PhysicalError::new(
                        "physical task spawn has no active lexical scope",
                    ));
                }
                PhysicalOp::TaskScopeClose { scope } => {
                    let drained = stack.pop();
                    if drained != Some((*scope, true)) {
                        return Err(PhysicalError::new(
                            "physical task scope closes before drain",
                        ));
                    }
                }
                _ => {}
            }
        }
        if let PhysicalTerminator::TaskScopeJoin { scope, .. } = block.terminator {
            if stack.last() != Some(&(scope, false)) {
                return Err(PhysicalError::new(
                    "physical task join lacks its active lexical scope",
                ));
            }
            stack.last_mut().unwrap().1 = true;
        }
        let edges = super::defer::edges(&block.terminator);
        // Ownership lowering keeps impossible fault-dispatch successors as
        // Unreachable blocks. They are not executable scope exits; the fault
        // verifier separately rejects abandoning an active fault there.
        if edges.is_empty()
            && !stack.is_empty()
            && !matches!(block.terminator, PhysicalTerminator::Unreachable)
        {
            return Err(PhysicalError::new(format!(
                "physical exit {id:?} ({:?}) leaves task storage live: {stack:?}",
                block.terminator
            )));
        }
        for edge in edges {
            if let Some(previous) = states.get(&edge.target) {
                if previous != &stack {
                    return Err(PhysicalError::new(
                        "physical merge disagrees on task scope lifetime",
                    ));
                }
            } else {
                states.insert(edge.target, stack.clone());
                pending.push(edge.target);
            }
        }
    }
    Ok(())
}

fn close_callers(
    mut resumable: BTreeSet<CallableId>,
    calls: &BTreeMap<CallableId, Vec<CallableId>>,
) -> BTreeSet<CallableId> {
    loop {
        let mut changed = false;
        for (caller, callees) in calls {
            if callees.iter().any(|callee| resumable.contains(callee)) {
                changed |= resumable.insert(*caller);
            }
        }
        if !changed {
            return resumable;
        }
    }
}

fn semantic_value_callees(
    module: &hew_sir::SemModule,
    ty: &hew_types::ResolvedTy,
    capability: hew_types::ValueCapability,
) -> BTreeSet<CallableId> {
    let mut pending = vec![ty.clone()];
    let mut seen = BTreeSet::new();
    let mut callees = BTreeSet::new();
    while let Some(ty) = pending.pop() {
        if !seen.insert(ty.clone()) {
            continue;
        }
        let selected = &module.value_capabilities[&(ty.clone(), capability)];
        if let Some(callee) = selected.callable {
            callees.insert(callee);
        } else {
            pending.extend(
                hew_sir::derived_capability_components(
                    &ty,
                    &module.aggregate_shapes,
                    &module.variant_shapes,
                )
                .expect("verified derived value capability"),
            );
        }
    }
    callees
}

pub(super) fn semantic_callables(checked: &hew_sir::CheckedModule<'_>) -> BTreeSet<CallableId> {
    let module = checked.module();
    let mut resumable = BTreeSet::new();
    let mut calls = BTreeMap::<_, Vec<_>>::new();
    for function in &module.functions {
        let mut types = BTreeMap::new();
        for parameter in &function.params {
            types.insert(parameter.value, parameter.ty.clone());
        }
        for block in &function.blocks {
            for parameter in &block.args {
                types.insert(parameter.value, parameter.ty.clone());
            }
            for operation in &block.ops {
                for result in &operation.results {
                    types.insert(result.id, result.ty.clone());
                }
            }
            block.terminator.visit_results(|result| {
                types.insert(result.id, result.ty.clone());
            });
        }
        let lifetimes = checked
            .function(function.callable)
            .expect("verified callable body")
            .place_lifetimes();
        for block in function
            .blocks
            .iter()
            .filter(|block| lifetimes.is_reachable(block.id))
        {
            match &block.terminator {
                hew_sir::SemTerminator::Suspend {
                    kind:
                        hew_sir::SuspendKind::StreamNext { park: false }
                        | hew_sir::SuspendKind::StreamSend { park: false },
                    ..
                } => {}
                hew_sir::SemTerminator::RecoverFault { .. }
                | hew_sir::SemTerminator::Suspend { .. }
                | hew_sir::SemTerminator::IndirectCall { .. } => {
                    // Callable values have no source-level non-suspension
                    // guarantee, so their invocation must admit suspension.
                    resumable.insert(function.callable);
                }
                hew_sir::SemTerminator::ActorCall {
                    operation: hew_sir::ActorOperation::Submit { policy, .. },
                    ..
                } if policy.may_suspend() => {
                    resumable.insert(function.callable);
                }
                hew_sir::SemTerminator::ActorCall {
                    operation:
                        hew_sir::ActorOperation::AwaitClosed(_)
                        | hew_sir::ActorOperation::SupervisorAwaitClosed(_)
                        | hew_sir::ActorOperation::SupervisorRoleAwaitClosed { .. }
                        | hew_sir::ActorOperation::StreamStart { .. }
                        | hew_sir::ActorOperation::CallStart(_),
                    ..
                } => {
                    resumable.insert(function.callable);
                }
                hew_sir::SemTerminator::RtCall {
                    family: hew_types::RuntimeCallFamily::StructuralFormat,
                    args,
                    ..
                } => {
                    let mut pending = vec![hew_sir::StructuralType::canonical(
                        &types[&args[0].operand.value],
                    )];
                    let mut seen = BTreeSet::new();
                    while let Some(key) = pending.pop() {
                        if !seen.insert(key.clone()) {
                            continue;
                        }
                        let selected = &module.structural_display[&key];
                        if let Some(callee) = selected.display {
                            calls.entry(function.callable).or_default().push(callee);
                        }
                        pending.extend(selected.members.iter().cloned());
                    }
                }
                hew_sir::SemTerminator::ValueCall { ty, capability, .. } => {
                    calls
                        .entry(function.callable)
                        .or_default()
                        .extend(semantic_value_callees(module, ty, *capability));
                }
                hew_sir::SemTerminator::RtCall { family, args, .. } => {
                    for capability in family.value_callback_capabilities() {
                        let receiver = &types[&args[0].operand.value];
                        let (_, arguments) =
                            hew_types::runtime_call::collection_type_arguments(receiver)
                                .expect("verified collection callback receiver");
                        calls
                            .entry(function.callable)
                            .or_default()
                            .extend(semantic_value_callees(module, &arguments[0], *capability));
                    }
                }
                hew_sir::SemTerminator::Call { callee, .. } => {
                    calls.entry(function.callable).or_default().push(*callee);
                }
                _ => {}
            }
        }
    }
    close_callers(resumable, &calls)
}

pub(super) fn verify_callables(module: &PhysicalModule) -> Result<(), PhysicalError> {
    let mut resumable = BTreeSet::new();
    let mut calls = BTreeMap::<_, Vec<_>>::new();
    for function in &module.functions {
        for block in &function.blocks {
            match &block.terminator {
                PhysicalTerminator::RecoverFault { .. }
                | PhysicalTerminator::NativeIo { .. }
                | PhysicalTerminator::Sleep { .. }
                | PhysicalTerminator::SleepUntil { .. }
                | PhysicalTerminator::TaskSelect { .. }
                | PhysicalTerminator::GeneratorYield { .. }
                | PhysicalTerminator::GeneratorNext { .. }
                | PhysicalTerminator::StreamNext { park: true, .. }
                | PhysicalTerminator::StreamSend { park: true, .. }
                | PhysicalTerminator::ValueClose { .. }
                | PhysicalTerminator::IndirectCall { .. }
                | PhysicalTerminator::TaskAwait { .. }
                | PhysicalTerminator::ActorAsk { .. }
                | PhysicalTerminator::TaskScopeJoin { .. }
                | PhysicalTerminator::ActorCall {
                    operation:
                        hew_sir::ActorOperation::AwaitClosed(_)
                        | hew_sir::ActorOperation::SupervisorAwaitClosed(_)
                        | hew_sir::ActorOperation::SupervisorRoleAwaitClosed { .. }
                        | hew_sir::ActorOperation::StreamStart { .. }
                        | hew_sir::ActorOperation::CallStart(_),
                    ..
                } => {
                    resumable.insert(function.callable);
                }
                PhysicalTerminator::ActorCall {
                    operation: hew_sir::ActorOperation::Submit { policy, .. },
                    ..
                } if policy.may_suspend() => {
                    resumable.insert(function.callable);
                }
                PhysicalTerminator::RuntimeCall { action, args, .. } => {
                    for capability in action.family.value_callback_capabilities() {
                        let argument = args.first().ok_or_else(|| {
                            PhysicalError::new("collection callback has no receiver operand")
                        })?;
                        let source = match *argument {
                            super::ArgumentTransfer::Borrow(source)
                            | super::ArgumentTransfer::BorrowMut(source)
                            | super::ArgumentTransfer::Move(source)
                            | super::ArgumentTransfer::Clone { source, .. } => source,
                        };
                        let receiver =
                            function.storage.get(source.0 as usize).ok_or_else(|| {
                                PhysicalError::new("collection callback has no receiver storage")
                            })?;
                        let (_, arguments) =
                            hew_types::runtime_call::collection_type_arguments(&receiver.ty)
                                .ok_or_else(|| {
                                    PhysicalError::new("collection callback has no receiver type")
                                })?;
                        calls.entry(function.callable).or_default().extend(
                            super::capability::callees(module, &arguments[0], *capability)?,
                        );
                    }
                    if let super::PhysicalRuntimeCarrier::StructuralFormat(glue) = action.carrier {
                        calls.entry(function.callable).or_default().extend(
                            super::structural::display_callees(&module.structural_glue, glue)?,
                        );
                    }
                }
                PhysicalTerminator::ValueCall { ty, capability, .. } => {
                    calls
                        .entry(function.callable)
                        .or_default()
                        .extend(super::capability::callees(module, ty, *capability)?);
                }
                PhysicalTerminator::Call { callee, .. } => {
                    calls.entry(function.callable).or_default().push(*callee);
                }
                _ => {}
            }
        }
    }
    let expected = close_callers(resumable, &calls);
    for glue in &module.structural_glue {
        let may_suspend = super::structural::display_callees(&module.structural_glue, glue.id)?
            .iter()
            .any(|callee| expected.contains(callee));
        if glue.is_resumable != may_suspend {
            return Err(PhysicalError::new(
                "structural rendering has an inconsistent resumable ABI",
            ));
        }
    }
    for ((ty, capability), selected) in &module.value_capabilities {
        let may_suspend = super::capability::callees(module, ty, *capability)?
            .iter()
            .any(|callee| expected.contains(callee));
        if may_suspend != selected.is_resumable {
            return Err(PhysicalError::new(
                "selected value callback has an inconsistent resumable ABI",
            ));
        }
    }
    for callable in &module.callables {
        if callable.is_resumable != expected.contains(&callable.id) {
            return Err(PhysicalError::new(format!(
                "callable {} has an inconsistent resumable ABI",
                callable.id.0
            )));
        }
    }
    // Init and lifecycle hooks run synchronously inside spawn and the
    // terminal transition, outside any scheduler-owned frame.
    for actor in &module.actors {
        if actor
            .init
            .iter()
            .chain(&actor.start)
            .chain(&actor.stop)
            .chain(&actor.crash)
            .chain(&actor.exit)
            .chain(&actor.down)
            .any(|body| expected.contains(body))
        {
            return Err(PhysicalError::new(
                "native actor init and lifecycle suspension is not implemented",
            ));
        }
    }
    Ok(())
}
