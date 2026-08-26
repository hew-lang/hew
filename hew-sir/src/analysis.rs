use std::collections::{BTreeMap, BTreeSet};

use crate::{BlockId, OpId, SemFunction, UseSite, ValueId};

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
        block
            .terminator
            .visit_successors(|edge| predecessors.entry(edge.target).or_default().push(block.id));
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

/// Def-use facts for one SIR function.
///
/// Both maps are ordered by stable SIR identity. A use is not merely a count:
/// it names the exact operation or terminator slot where a rewrite can act.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct DefUseIndex {
    pub definitions: BTreeMap<ValueId, BlockId>,
    pub uses: BTreeMap<ValueId, Vec<UseSite>>,
}

impl DefUseIndex {
    /// Return deterministic concrete use sites for `value`.
    #[must_use]
    pub fn uses_of(&self, value: ValueId) -> &[UseSite] {
        self.uses.get(&value).map_or(&[], Vec::as_slice)
    }

    /// Convenience count for clients that do not need the individual sites.
    #[must_use]
    pub fn use_count(&self, value: ValueId) -> usize {
        self.uses_of(value).len()
    }
}

/// Build the deterministic concrete def-use index for one semantic SSA
/// function. The verifier remains authoritative for duplicate definitions and
/// malformed CFGs; this index deliberately stays total to support diagnostics
/// on malformed intermediate states.
///
/// Transformations using [`replace_use`] or [`replace_all_uses`] require a
/// verifier-clean function with unique operation and block identities. A
/// def-use index over malformed SIR is useful for diagnostics, but is not a
/// repair authority for ambiguous identities.
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
            op.visit_operands(|operand, use_| {
                index
                    .uses
                    .entry(use_.value)
                    .or_default()
                    .push(UseSite::Operation {
                        op: op.id,
                        operand,
                        value: use_.value,
                        mode: use_.mode,
                    });
            });
        }
        block.terminator.visit_operands(|operand, use_| {
            index
                .uses
                .entry(use_.value)
                .or_default()
                .push(UseSite::Terminator {
                    block: block.id,
                    operand,
                    value: use_.value,
                    mode: use_.mode,
                });
        });
    }
    for uses in index.uses.values_mut() {
        uses.sort_unstable();
    }
    index
}

/// Failure to apply an indexed rewrite site to the current mutable function.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RewriteError {
    UnknownOperation(OpId),
    UnknownBlock(BlockId),
    StaleUseSite(UseSite),
}

/// Rewrite one indexed semantic use to `replacement`.
///
/// The caller normally obtains `site` from [`build_def_use`]. If an earlier
/// rewrite changed its ownership mode or removed the referenced operation or
/// block, this returns a precise stale-site error rather than writing through
/// an incidental vector index.
///
/// # Errors
///
/// Returns [`RewriteError::UnknownOperation`] or
/// [`RewriteError::UnknownBlock`] when the indexed identity no longer exists,
/// and [`RewriteError::StaleUseSite`] when its operand value or mode no longer
/// agrees with the current function.
pub fn replace_use(
    function: &mut SemFunction,
    site: UseSite,
    replacement: ValueId,
) -> Result<(), RewriteError> {
    let replaced = match site {
        UseSite::Operation {
            op,
            operand,
            value,
            mode,
        } => {
            let Some(operation) = function
                .blocks
                .iter_mut()
                .flat_map(|block| block.ops.iter_mut())
                .find(|operation| operation.id == op)
            else {
                return Err(RewriteError::UnknownOperation(op));
            };
            operation.replace_operand_at(operand, value, mode, replacement)
        }
        UseSite::Terminator {
            block,
            operand,
            value,
            mode,
        } => {
            let Some(block) = function
                .blocks
                .iter_mut()
                .find(|candidate| candidate.id == block)
            else {
                return Err(RewriteError::UnknownBlock(block));
            };
            block
                .terminator
                .replace_operand_at(operand, value, mode, replacement)
        }
    };
    if replaced {
        Ok(())
    } else {
        Err(RewriteError::StaleUseSite(site))
    }
}

/// Replace every current semantic use of `from` with `replacement`.
///
/// Definitions are intentionally untouched. The fresh index gives this a
/// deterministic snapshot of all use sites; rewriting values cannot change
/// operand slots, so every site stays valid for the duration of this operation.
///
/// The caller must verify that `function` has unique operation and block IDs
/// before rewriting. See [`build_def_use`] for the malformed-SIR diagnostic
/// contract.
///
/// # Errors
///
/// Returns the first [`RewriteError`] instead of silently applying a partial
/// rewrite. The snapshot remains valid while this function changes only its
/// operand values; a failure therefore signals malformed or concurrently
/// mutated SIR that a pass must not treat as a completed rewrite.
pub fn replace_all_uses(
    function: &mut SemFunction,
    from: ValueId,
    replacement: ValueId,
) -> Result<usize, RewriteError> {
    let sites = build_def_use(function).uses_of(from).to_vec();
    // Rewrite a clone first so malformed identities cannot leave a caller with
    // a partially rewritten semantic graph. Normal pass execution verifies
    // unique identities before this point; this guard makes the public helper
    // fail closed even when it is used during diagnostics or development.
    let mut rewritten = function.clone();
    let mut replaced = 0;
    for site in sites {
        replace_use(&mut rewritten, site, replacement)?;
        replaced += 1;
    }
    *function = rewritten;
    Ok(replaced)
}
