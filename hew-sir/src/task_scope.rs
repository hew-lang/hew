//! Lexical task lifetime verification, independent of target storage.

use crate::{
    BlockId, SemFunction, SemOp, SemOpKind, SemTerminator, SuspendKind, TaskScopeId, ValueId,
};
use std::collections::{BTreeMap, BTreeSet, VecDeque};

#[expect(
    clippy::too_many_lines,
    reason = "verify lexical scope ancestry and the invocation loans each scope retains"
)]
pub(crate) fn verify(function: &SemFunction) -> Result<(), String> {
    let mut declared = BTreeSet::new();
    let mut invocation_loans = BTreeMap::new();
    for block in &function.blocks {
        for op in block.operations() {
            if let SemOpKind::TaskInvoke {
                scope,
                callee,
                args,
                ..
            } = &op.kind
            {
                for argument in callee.receiver().into_iter().chain(args) {
                    if matches!(
                        argument.decision,
                        crate::BoundaryDecision::Borrow | crate::BoundaryDecision::BorrowMut
                    ) && invocation_loans
                        .insert(argument.operand.value, *scope)
                        .is_some()
                    {
                        return Err("prepared invocations require distinct input loans".into());
                    }
                }
            }
            if let SemOpKind::TaskScopeEnter { scope, .. } = op.kind {
                if !declared.insert(scope) {
                    return Err("task scope has more than one lexical declaration".into());
                }
            }
        }
    }
    let blocks: BTreeMap<_, _> = function.blocks.iter().map(|b| (b.id, b)).collect();
    let mut entries = BTreeMap::<BlockId, TaskState>::new();
    entries.insert(function.entry, TaskState::default());
    let mut queue = VecDeque::from([function.entry]);
    while let Some(id) = queue.pop_front() {
        let Some(block) = blocks.get(&id) else {
            continue;
        };
        let mut state = entries[&id].clone();
        for op in &block.ops {
            state.apply(op)?;
        }
        if let SemTerminator::Suspend {
            kind: SuspendKind::Join { scope, .. },
            ..
        } = block.terminator
        {
            if state.scopes.last() != Some(&(scope, false)) {
                return Err("task scope drain requires the active innermost scope".into());
            }
            state.scopes.last_mut().unwrap().1 = true;
        }
        let mut successors = Vec::new();
        if let SemTerminator::Materialize {
            operation,
            normal,
            failure,
        } = &block.terminator
        {
            // Refusal has not published the invocation or its retained loans.
            let mut published = state.clone();
            published.apply(operation)?;
            successors.push((normal.target, published));
            successors.push((failure.target, state.clone()));
        } else {
            block
                .terminator
                .visit_successors(|edge| successors.push((edge.target, state.clone())));
        }
        // `Unreachable` follows a process exit or an impossible dispatch edge.
        // Other exits must have drained their lexical scopes.
        if successors.is_empty()
            && !state.scopes.is_empty()
            && !matches!(block.terminator, SemTerminator::Unreachable)
        {
            return Err("function exit leaves an undrained task scope".into());
        }
        for (target, next) in successors {
            if let Some(previous) = entries.get_mut(&target) {
                if previous.scopes != next.scopes {
                    return Err("control-flow merge disagrees on task scope lifetime".into());
                }
                // A shared cleanup may drain either a published invocation or
                // its refused acquisition. Loans may have been published on
                // any incoming path, while scope ancestry must agree exactly.
                let old_len = previous.invocation_loans.len();
                previous.invocation_loans.extend(next.invocation_loans);
                if previous.invocation_loans.len() != old_len {
                    queue.push_back(target);
                }
            } else {
                entries.insert(target, next);
                queue.push_back(target);
            }
        }
    }
    Ok(())
}

#[derive(Clone, Default, PartialEq, Eq)]
struct TaskState {
    scopes: Vec<(TaskScopeId, bool)>,
    invocation_loans: BTreeMap<ValueId, TaskScopeId>,
}

impl TaskState {
    fn apply(&mut self, op: &SemOp) -> Result<(), String> {
        match &op.kind {
            SemOpKind::TaskScopeEnter { scope, parent, .. } => {
                if self.scopes.last().map(|(id, _)| *id) != *parent
                    || self.scopes.iter().any(|(id, _)| id == scope)
                {
                    return Err("task scope entry has inconsistent lexical ancestry".into());
                }
                self.scopes.push((*scope, false));
            }
            SemOpKind::TaskSpawn { scope, .. } | SemOpKind::TaskRace { scope, .. } => {
                if !self.scopes.contains(&(*scope, false)) {
                    return Err("task spawn requires an active lexical scope".into());
                }
            }
            SemOpKind::TaskInvoke {
                scope,
                callee,
                args,
                ..
            } => {
                if self.scopes.last() != Some(&(*scope, false)) {
                    return Err("task spawn requires the active innermost scope".into());
                }
                for argument in callee.receiver().into_iter().chain(args) {
                    if matches!(
                        argument.decision,
                        crate::BoundaryDecision::Borrow | crate::BoundaryDecision::BorrowMut
                    ) {
                        self.invocation_loans.insert(argument.operand.value, *scope);
                    }
                }
            }
            SemOpKind::EndBorrow { borrow } => {
                if self
                    .invocation_loans
                    .get(&borrow.value)
                    .is_some_and(|scope| self.scopes.contains(&(*scope, false)))
                {
                    return Err(
                        "prepared input loan cannot end before its invocation scope drains".into(),
                    );
                }
                self.invocation_loans.remove(&borrow.value);
            }
            SemOpKind::TaskScopeClose { scope } => {
                if self.scopes.pop() != Some((*scope, true)) {
                    return Err("task scope close requires its completed drain".into());
                }
            }
            _ => {}
        }
        Ok(())
    }
}
