//! Lexical task lifetime verification, independent of target storage.

use crate::{BlockId, SemFunction, SemOpKind, SemTerminator, SuspendKind, TaskScopeId};
use std::collections::{BTreeMap, BTreeSet, VecDeque};

pub(crate) fn verify(function: &SemFunction) -> Result<(), String> {
    let mut declared = BTreeSet::new();
    for block in &function.blocks {
        for op in &block.ops {
            if let SemOpKind::TaskScopeEnter { scope, .. } = op.kind {
                if !declared.insert(scope) {
                    return Err("task scope has more than one lexical declaration".into());
                }
            }
        }
    }
    let blocks: BTreeMap<_, _> = function.blocks.iter().map(|b| (b.id, b)).collect();
    let mut entries = BTreeMap::<BlockId, Vec<(TaskScopeId, bool)>>::new();
    entries.insert(function.entry, Vec::new());
    let mut queue = VecDeque::from([function.entry]);
    while let Some(id) = queue.pop_front() {
        let Some(block) = blocks.get(&id) else {
            continue;
        };
        let mut stack = entries[&id].clone();
        for op in &block.ops {
            match op.kind {
                SemOpKind::TaskScopeEnter { scope, parent } => {
                    if stack.last().map(|(id, _)| *id) != parent
                        || stack.iter().any(|(id, _)| *id == scope)
                    {
                        return Err("task scope entry has inconsistent lexical ancestry".into());
                    }
                    stack.push((scope, false));
                }
                SemOpKind::TaskSpawn { scope, .. } => {
                    if stack.last() != Some(&(scope, false)) {
                        return Err("task spawn requires the active innermost scope".into());
                    }
                }
                SemOpKind::TaskScopeClose { scope } => {
                    let drained = stack.pop();
                    if drained != Some((scope, true)) {
                        return Err("task scope close requires its completed drain".into());
                    }
                }
                _ => {}
            }
        }
        if let SemTerminator::Suspend {
            kind: SuspendKind::Join { scope, .. },
            ..
        } = block.terminator
        {
            if stack.last() != Some(&(scope, false)) {
                return Err("task scope drain requires the active innermost scope".into());
            }
            stack.last_mut().unwrap().1 = true;
        }
        let mut successors = Vec::new();
        block
            .terminator
            .visit_successors(|edge| successors.push(edge.target));
        if successors.is_empty() && !stack.is_empty() {
            return Err("function exit leaves an undrained task scope".into());
        }
        for target in successors {
            if let Some(previous) = entries.get(&target) {
                if previous != &stack {
                    return Err("control-flow merge disagrees on task scope lifetime".into());
                }
            } else {
                entries.insert(target, stack.clone());
                queue.push_back(target);
            }
        }
    }
    Ok(())
}
