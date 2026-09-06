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

pub(super) fn semantic_callables(checked: &hew_sir::CheckedModule<'_>) -> BTreeSet<CallableId> {
    let module = checked.module();
    let mut resumable = BTreeSet::new();
    let mut calls = BTreeMap::<_, Vec<_>>::new();
    for function in &module.functions {
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
                hew_sir::SemTerminator::RecoverFault { .. }
                | hew_sir::SemTerminator::Suspend { .. }
                | hew_sir::SemTerminator::IndirectCall { .. } => {
                    // Callable values have no source-level non-suspension
                    // guarantee, so their invocation must admit suspension.
                    resumable.insert(function.callable);
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
                | PhysicalTerminator::Sleep { .. }
                | PhysicalTerminator::TaskSelect { .. }
                | PhysicalTerminator::GeneratorYield { .. }
                | PhysicalTerminator::GeneratorNext { .. }
                | PhysicalTerminator::ValueClose { .. }
                | PhysicalTerminator::IndirectCall { .. }
                | PhysicalTerminator::TaskAwait { .. }
                | PhysicalTerminator::ActorAsk { .. }
                | PhysicalTerminator::TaskScopeJoin { .. } => {
                    resumable.insert(function.callable);
                }
                PhysicalTerminator::Call { callee, .. } => {
                    calls.entry(function.callable).or_default().push(*callee);
                }
                _ => {}
            }
        }
    }
    let expected = close_callers(resumable, &calls);
    for callable in &module.callables {
        if callable.is_resumable != expected.contains(&callable.id) {
            return Err(PhysicalError::new(format!(
                "callable {} has an inconsistent resumable ABI",
                callable.id.0
            )));
        }
    }
    Ok(())
}
