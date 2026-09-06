//! Resumable callable ABI selection from explicit typed control flow.

use super::{CallableId, PhysicalError, PhysicalModule, PhysicalTerminator};
use std::collections::{BTreeMap, BTreeSet};

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

pub(super) fn semantic_callables(module: &hew_sir::SemModule) -> BTreeSet<CallableId> {
    let mut resumable = BTreeSet::new();
    let mut calls = BTreeMap::<_, Vec<_>>::new();
    for function in &module.functions {
        for block in &function.blocks {
            match &block.terminator {
                hew_sir::SemTerminator::Suspend { .. }
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
                PhysicalTerminator::Sleep { .. } | PhysicalTerminator::IndirectCall { .. } => {
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
