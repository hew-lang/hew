use std::collections::{BTreeMap, BTreeSet, HashMap};

use crate::{BlockId, SemFunction, ValueId};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Dominators {
    pub sets: BTreeMap<BlockId, BTreeSet<BlockId>>,
}

#[must_use]
///
/// # Panics
///
/// Panics only if a caller supplies a CFG whose predecessor relation names a
/// block that is absent from the function. `verify_module` reports that shape
/// before any valid SIR pipeline consumes the analysis.
pub fn compute_dominators(function: &SemFunction) -> Dominators {
    let all = function
        .blocks
        .iter()
        .map(|b| b.id)
        .collect::<BTreeSet<_>>();
    let mut predecessors: BTreeMap<BlockId, Vec<BlockId>> = BTreeMap::new();
    for block in &function.blocks {
        for edge in block.terminator.successors() {
            predecessors.entry(edge.target).or_default().push(block.id);
        }
    }
    let mut sets = function
        .blocks
        .iter()
        .map(|block| {
            let initial = if block.id == function.entry {
                [function.entry].into_iter().collect()
            } else {
                all.clone()
            };
            (block.id, initial)
        })
        .collect::<BTreeMap<_, _>>();
    let mut changed = true;
    while changed {
        changed = false;
        for block in &function.blocks {
            if block.id == function.entry {
                continue;
            }
            let mut next = predecessors
                .get(&block.id)
                .map_or_else(BTreeSet::new, |preds| {
                    let mut result = all.clone();
                    for pred in preds {
                        result = result
                            .intersection(sets.get(pred).expect("predecessor must be a block"))
                            .copied()
                            .collect();
                    }
                    result
                });
            next.insert(block.id);
            if sets.get(&block.id) != Some(&next) {
                sets.insert(block.id, next);
                changed = true;
            }
        }
    }
    Dominators { sets }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct DefUseIndex {
    pub definitions: HashMap<ValueId, BlockId>,
    pub uses: HashMap<ValueId, usize>,
}

#[must_use]
pub fn build_def_use(function: &SemFunction) -> DefUseIndex {
    let mut index = DefUseIndex::default();
    for param in &function.params {
        index.definitions.insert(param.value, function.entry);
    }
    for block in &function.blocks {
        for arg in &block.args {
            index.definitions.insert(arg.value, block.id);
        }
        for op in &block.ops {
            for result in &op.results {
                index.definitions.insert(result.id, block.id);
            }
            for value in op_uses(op) {
                *index.uses.entry(value).or_default() += 1;
            }
        }
        for value in terminator_uses(&block.terminator) {
            *index.uses.entry(value).or_default() += 1;
        }
    }
    index
}

fn op_uses(op: &crate::SemOp) -> Vec<ValueId> {
    use crate::SemOpKind;
    match &op.kind {
        SemOpKind::ConstI64(_) | SemOpKind::ConstBool(_) => Vec::new(),
        SemOpKind::Unary { value, .. } | SemOpKind::Cast { value, .. } => vec![value.value],
        SemOpKind::Binary { lhs, rhs, .. } => vec![lhs.value, rhs.value],
        SemOpKind::Call { args, .. } => args.iter().map(|arg| arg.value).collect(),
    }
}

fn terminator_uses(terminator: &crate::SemTerminator) -> Vec<ValueId> {
    use crate::SemTerminator;
    match terminator {
        SemTerminator::Return { value } => value.iter().copied().collect(),
        SemTerminator::Goto(edge) => edge.args.clone(),
        SemTerminator::Branch {
            condition,
            then_target,
            else_target,
        } => {
            let mut values = vec![*condition];
            values.extend(then_target.args.iter().copied());
            values.extend(else_target.args.iter().copied());
            values
        }
        SemTerminator::Unreachable => Vec::new(),
    }
}
