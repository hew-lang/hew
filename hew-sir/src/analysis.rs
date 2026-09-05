use std::collections::{BTreeMap, BTreeSet};

use crate::{BlockId, OpId, SemFunction, SuccessorSlot, UseSite, ValueId};

/// Stable identity of one outgoing CFG edge in a semantic function.
///
/// The source block plus [`Self::slot`] identify an edge independently of its
/// target. In particular, the two successor slots of a branch remain distinct
/// even when both target the same block. `EdgeRef` is a snapshot identity over
/// verified SIR: a transformation that changes a terminator must rebuild its
/// [`CfgIndex`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EdgeRef {
    pub source: BlockId,
    pub slot: SuccessorSlot,
}

/// Deterministic control-flow facts for one semantic function.
///
/// `predecessors` and `successors` retain one [`EdgeRef`] per semantic edge,
/// including duplicate edges. Their vectors are sorted by stable edge identity
/// rather than relying on incidental allocation order. `edge_targets` records
/// the target observed while indexing, so analyses can consume a coherent CFG
/// snapshot without re-inspecting mutable terminators.
///
/// Reachability and reverse postorder contain only blocks that exist in the
/// function. An edge to an unknown block remains visible in the index for
/// diagnostics, but does not manufacture a reachable CFG node. Duplicate
/// block identities are malformed SIR and must be rejected by the verifier
/// before clients rely on `EdgeRef` as a unique semantic identity.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct CfgIndex {
    predecessors: BTreeMap<BlockId, Vec<EdgeRef>>,
    successors: BTreeMap<BlockId, Vec<EdgeRef>>,
    edge_targets: BTreeMap<EdgeRef, BlockId>,
    reachable: BTreeSet<BlockId>,
    reverse_postorder: Vec<BlockId>,
}

impl CfgIndex {
    /// Return all incoming edges for `block` in stable edge-reference order.
    #[must_use]
    pub fn predecessors_of(&self, block: BlockId) -> &[EdgeRef] {
        self.predecessors.get(&block).map_or(&[], Vec::as_slice)
    }

    /// Return all outgoing edges for `block` in stable successor-slot order.
    #[must_use]
    pub fn successors_of(&self, block: BlockId) -> &[EdgeRef] {
        self.successors.get(&block).map_or(&[], Vec::as_slice)
    }

    /// Return the target captured for one indexed edge.
    #[must_use]
    pub fn edge_target(&self, edge: EdgeRef) -> Option<BlockId> {
        self.edge_targets.get(&edge).copied()
    }

    /// Whether `block` is reachable from the function's declared entry block.
    #[must_use]
    pub fn is_reachable(&self, block: BlockId) -> bool {
        self.reachable.contains(&block)
    }

    /// Return every block reachable from the function entry in stable block-ID
    /// order.
    #[must_use]
    pub fn reachable(&self) -> &BTreeSet<BlockId> {
        &self.reachable
    }

    /// Return the deterministic reverse-postorder traversal of reachable
    /// blocks. DFS visits successors in [`SuccessorSlot`] order before
    /// reversing its postorder.
    #[must_use]
    pub fn rpo(&self) -> &[BlockId] {
        &self.reverse_postorder
    }
}

/// Build the deterministic CFG facts for one semantic function.
///
/// The verifier remains authoritative for canonical, unique block identities
/// and known successor targets. Consumers that transform SIR must verify first
/// and rebuild the index after changing a terminator or block collection. An
/// otherwise uniquely identified function with an unknown target can still be
/// indexed for diagnostics, but duplicate block identities have no coherent
/// `EdgeRef` meaning.
#[must_use]
pub fn build_cfg_index(function: &SemFunction) -> CfgIndex {
    let mut index = CfgIndex::default();
    let known_blocks = function
        .blocks
        .iter()
        .map(|block| block.id)
        .collect::<BTreeSet<_>>();

    for block in &function.blocks {
        index.predecessors.entry(block.id).or_default();
        index.successors.entry(block.id).or_default();
    }
    for block in &function.blocks {
        block.terminator.visit_successors_with_slots(|slot, edge| {
            let edge_ref = EdgeRef {
                source: block.id,
                slot,
            };
            index.successors.entry(block.id).or_default().push(edge_ref);
            index
                .predecessors
                .entry(edge.target)
                .or_default()
                .push(edge_ref);
            index.edge_targets.insert(edge_ref, edge.target);
        });
    }
    for edges in index.predecessors.values_mut() {
        edges.sort_unstable();
    }
    for edges in index.successors.values_mut() {
        edges.sort_unstable();
    }

    let mut postorder = Vec::new();
    if known_blocks.contains(&function.entry) {
        visit_reachable_postorder(
            function.entry,
            &known_blocks,
            &index.successors,
            &index.edge_targets,
            &mut index.reachable,
            &mut postorder,
        );
    }
    postorder.reverse();
    index.reverse_postorder = postorder;
    index
}

fn visit_reachable_postorder(
    block: BlockId,
    known_blocks: &BTreeSet<BlockId>,
    successors: &BTreeMap<BlockId, Vec<EdgeRef>>,
    edge_targets: &BTreeMap<EdgeRef, BlockId>,
    reachable: &mut BTreeSet<BlockId>,
    postorder: &mut Vec<BlockId>,
) {
    if !reachable.insert(block) {
        return;
    }
    if let Some(edges) = successors.get(&block) {
        for edge in edges {
            let Some(target) = edge_targets.get(edge).copied() else {
                continue;
            };
            if known_blocks.contains(&target) {
                visit_reachable_postorder(
                    target,
                    known_blocks,
                    successors,
                    edge_targets,
                    reachable,
                    postorder,
                );
            }
        }
    }
    postorder.push(block);
}

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
    let cfg = build_cfg_index(function);
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
            if !cfg.is_reachable(block.id) {
                // No executable path reaches this block, so every known block
                // vacuously dominates it. Leaving its initial `all` set intact
                // keeps verifier-after-rewrite valid until the following CFG
                // compaction removes the dead block.
                continue;
            }
            // Dominance is a property of paths that can execute from the
            // entry. A structurally present predecessor from an unreachable
            // block must not invalidate a definition that dominates this
            // reachable block on every executable path. This matters after
            // CFG rewrites, where dead blocks can still point at a live join
            // until compaction removes them.
            let reachable_predecessors = cfg
                .predecessors_of(block.id)
                .iter()
                .filter(|predecessor| cfg.is_reachable(predecessor.source));
            let mut next = if reachable_predecessors.clone().next().is_none() {
                BTreeSet::new()
            } else {
                let mut result = all.clone();
                for predecessor in reachable_predecessors {
                    result = result
                        .intersection(
                            sets.get(&predecessor.source)
                                .expect("reachable predecessor must be a block"),
                        )
                        .copied()
                        .collect();
                }
                result
            };
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
                    });
            });
        }
        block
            .terminator
            .visit_results(|result| _ = index.definitions.insert(result.id, block.id));
        block.terminator.visit_operands(|operand, use_| {
            index
                .uses
                .entry(use_.value)
                .or_default()
                .push(UseSite::Terminator {
                    block: block.id,
                    operand,
                    value: use_.value,
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
        UseSite::Operation { op, operand, value } => {
            let Some(operation) = function
                .blocks
                .iter_mut()
                .flat_map(|block| block.ops.iter_mut())
                .find(|operation| operation.id == op)
            else {
                return Err(RewriteError::UnknownOperation(op));
            };
            operation.replace_operand_at(operand, value, replacement)
        }
        UseSite::Terminator {
            block,
            operand,
            value,
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
                .replace_operand_at(operand, value, replacement)
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

#[cfg(test)]
mod tests {
    use std::collections::{BTreeMap, BTreeSet};

    use hew_hir::ItemId;
    use hew_types::{DefId, ResolvedTy, TypeFactContext, TypeFactService};

    use super::{build_cfg_index, compute_dominators, EdgeRef};
    use crate::{
        BlockArg, BlockId, CallableId, CallableInstance, Edge, FunctionSourceOrigin, Operand,
        SemAbiParam, SemBlock, SemCallConv, SemCallable, SemCallableKind, SemFunction, SemModule,
        SemParamPassing, SemSignature, SemTerminator, SuccessorSlot, ValueId,
    };

    fn read(value: u32) -> Operand {
        Operand {
            value: ValueId(value),
        }
    }

    fn edge(target: u32) -> Edge {
        Edge {
            target: BlockId(target),
            args: Vec::new(),
        }
    }

    fn block(id: u32, terminator: SemTerminator) -> SemBlock {
        SemBlock {
            id: BlockId(id),
            args: Vec::new(),
            ops: Vec::new(),
            terminator,
        }
    }

    fn function(blocks: Vec<SemBlock>) -> SemFunction {
        SemFunction {
            id: ItemId(0),
            callable: CallableId(0),
            declaration: DefId::for_test("cfg_index"),
            name: "cfg_index".to_string(),
            span: 0..0,
            source_origin: FunctionSourceOrigin::Unknown,
            params: vec![BlockArg {
                value: ValueId(0),
                ty: ResolvedTy::Bool,
                own: crate::ownership::OwnKind::None,
            }],
            return_ty: ResolvedTy::Unit,
            entry: BlockId(0),
            blocks,
            places: Vec::new(),
            bindings: Vec::new(),
        }
    }

    /// The one-callable module `function` belongs to.
    ///
    /// A parameter's §1.2 kind is its ABI slot before it is its type's class,
    /// so a function that has parameters is verified against the callable table
    /// that names those slots rather than context-free.
    fn module(function: SemFunction) -> SemModule {
        let mut fact_service =
            TypeFactService::new(TypeFactContext::default(), BTreeMap::default());
        for ty in function
            .params
            .iter()
            .map(|value| &value.ty)
            .chain(std::iter::once(&function.return_ty))
            .chain(function.blocks.iter().flat_map(|block| {
                block.args.iter().map(|value| &value.ty).chain(
                    block
                        .ops
                        .iter()
                        .flat_map(|op| op.results.iter().map(|value| &value.ty)),
                )
            }))
        {
            let _ = fact_service.require(ty);
        }
        SemModule {
            callables: vec![SemCallable {
                id: function.callable,
                function: function.id,
                declaration: function.declaration.clone(),
                instance: CallableInstance::Monomorphic,
                symbol: function.name.clone(),
                source_origin: function.source_origin.clone(),
                signature: SemSignature {
                    params: function
                        .params
                        .iter()
                        .map(|parameter| SemAbiParam {
                            ty: parameter.ty.clone(),
                            passing: SemParamPassing::ReadOnly,
                            caller_visible_projection: false,
                        })
                        .collect(),
                    return_ty: function.return_ty.clone(),
                },
                call_conv: SemCallConv::Default,
                kind: SemCallableKind::HewDirect,
            }],
            generic_templates: Vec::new(),
            root_unit_callables: Vec::new(),
            entry_exit_plan: None,
            entry_callable: None,
            functions: vec![function],
            aggregate_shapes: Vec::new(),
            variant_shapes: Vec::new(),
            type_facts: fact_service.into_rows(),
            string_literals: std::collections::BTreeMap::new(),
            bytes_literals: std::collections::BTreeMap::new(),
        }
    }

    #[test]
    fn successor_slots_preserve_duplicate_branch_edges_and_legacy_visitors() {
        let mut terminator = SemTerminator::Branch {
            condition: read(0),
            then_target: edge(1),
            else_target: edge(1),
        };

        let mut slotted = Vec::new();
        terminator.visit_successors_with_slots(|slot, edge| slotted.push((slot, edge.target)));
        assert_eq!(
            slotted,
            vec![
                (SuccessorSlot(0), BlockId(1)),
                (SuccessorSlot(1), BlockId(1)),
            ]
        );

        let mut legacy = Vec::new();
        terminator.visit_successors(|edge| legacy.push(edge.target));
        assert_eq!(legacy, vec![BlockId(1), BlockId(1)]);

        terminator
            .successor_mut(SuccessorSlot(1))
            .expect("branch else edge must own successor slot 1")
            .target = BlockId(2);
        terminator.visit_successors_with_slots_mut(|slot, edge| {
            if slot == SuccessorSlot(0) {
                edge.target = BlockId(3);
            }
        });
        assert_eq!(
            terminator
                .successor(SuccessorSlot(0))
                .map(|edge| edge.target),
            Some(BlockId(3))
        );
        assert_eq!(
            terminator
                .successor(SuccessorSlot(1))
                .map(|edge| edge.target),
            Some(BlockId(2))
        );
        assert_eq!(terminator.successor(SuccessorSlot(2)), None);
    }

    #[test]
    fn cfg_index_preserves_duplicate_edges_and_computes_stable_rpo() {
        let function = function(vec![
            block(
                0,
                SemTerminator::Branch {
                    condition: read(0),
                    then_target: edge(1),
                    else_target: edge(1),
                },
            ),
            block(1, SemTerminator::Goto(edge(2))),
            block(
                2,
                SemTerminator::Branch {
                    condition: read(0),
                    then_target: edge(1),
                    else_target: edge(3),
                },
            ),
            block(3, SemTerminator::Return { value: None }),
            block(4, SemTerminator::Return { value: None }),
        ]);
        assert!(crate::verify_function_in_module(&module(function.clone()), &function).is_empty());

        let index = build_cfg_index(&function);
        let entry_then = EdgeRef {
            source: BlockId(0),
            slot: SuccessorSlot(0),
        };
        let entry_else = EdgeRef {
            source: BlockId(0),
            slot: SuccessorSlot(1),
        };
        let loop_back = EdgeRef {
            source: BlockId(2),
            slot: SuccessorSlot(0),
        };
        let exit = EdgeRef {
            source: BlockId(2),
            slot: SuccessorSlot(1),
        };

        assert_eq!(index.successors_of(BlockId(0)), &[entry_then, entry_else]);
        assert_eq!(
            index.predecessors_of(BlockId(1)),
            &[entry_then, entry_else, loop_back]
        );
        assert_eq!(index.successors_of(BlockId(2)), &[loop_back, exit]);
        assert_eq!(index.edge_target(entry_then), Some(BlockId(1)));
        assert_eq!(index.edge_target(entry_else), Some(BlockId(1)));
        assert_eq!(index.edge_target(exit), Some(BlockId(3)));
        assert_eq!(
            index.reachable,
            BTreeSet::from([BlockId(0), BlockId(1), BlockId(2), BlockId(3)])
        );
        assert!(!index.is_reachable(BlockId(4)));
        assert_eq!(
            index.rpo(),
            &[BlockId(0), BlockId(1), BlockId(2), BlockId(3)]
        );
        assert_eq!(index, build_cfg_index(&function));
    }

    #[test]
    fn cfg_index_keeps_unknown_targets_for_diagnostics_without_reaching_them() {
        let function = function(vec![block(0, SemTerminator::Goto(edge(9)))]);

        let index = build_cfg_index(&function);
        let unknown_edge = EdgeRef {
            source: BlockId(0),
            slot: SuccessorSlot(0),
        };
        assert_eq!(index.predecessors_of(BlockId(9)), &[unknown_edge]);
        assert_eq!(index.edge_target(unknown_edge), Some(BlockId(9)));
        assert_eq!(index.reachable, BTreeSet::from([BlockId(0)]));
        assert!(!index.is_reachable(BlockId(9)));
        assert_eq!(index.rpo(), &[BlockId(0)]);
    }

    #[test]
    fn dominators_ignore_unreachable_predecessors_of_reachable_blocks() {
        let mut function = function(vec![
            block(0, SemTerminator::Goto(edge(1))),
            block(
                1,
                SemTerminator::Return {
                    value: Some(crate::BoundaryOperand {
                        operand: read(0),
                        decision: crate::BoundaryDecision::Move,
                    }),
                },
            ),
            // This structural predecessor cannot execute from the entry and
            // must not make the entry parameter appear non-dominating at
            // bb1. CFG simplification commonly creates this shape briefly
            // before compaction removes the dead block.
            block(2, SemTerminator::Goto(edge(1))),
        ]);
        function.return_ty = ResolvedTy::Bool;

        assert!(crate::verify_function_in_module(&module(function.clone()), &function).is_empty());
        let index = build_cfg_index(&function);
        assert_eq!(index.reachable(), &BTreeSet::from([BlockId(0), BlockId(1)]));
        assert_eq!(index.rpo(), &[BlockId(0), BlockId(1)]);
        assert!(!index.is_reachable(BlockId(2)));

        let dominators = compute_dominators(&function);
        assert_eq!(
            dominators.sets.get(&BlockId(1)),
            Some(&BTreeSet::from([BlockId(0), BlockId(1)]))
        );
    }
}
