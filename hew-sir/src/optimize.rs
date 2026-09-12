//! Small, verifier-backed SIR canonicalization passes.
//!
//! This module intentionally starts with CFG canonicalization rather than a
//! general pass manager. Each transformation is transactional: malformed input
//! is rejected before mutation, and the verifier must accept the complete
//! result before it becomes visible to a caller.

use std::collections::{BTreeMap, BTreeSet};

use crate::ownership::TypeFactTable;
use crate::verify::verify_cfg_discard_safety;
use crate::{
    build_cfg_index, verify_module, BlockId, CallableId, SemFunction, SemModule, SemOpKind,
    SemTerminator, SirDiagnostic, ValueId,
};

/// Stable result facts from one constant-CFG canonicalization.
///
/// `removed_blocks` and `block_remap` refer to the input function's block
/// identities. Value and operation identities are intentionally not remapped:
/// they are semantic identities rather than vector positions.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CfgCanonicalizationReport {
    /// Number of direct constant boolean branches replaced with their selected
    /// edge.
    pub folded_branches: usize,
    /// Former identities of blocks unreachable after branch folding.
    pub removed_blocks: Vec<BlockId>,
    /// Every retained former block identity and its canonical new identity.
    pub block_remap: BTreeMap<BlockId, BlockId>,
}

/// A verifier boundary failure around a SIR optimization pass.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SirOptimizationError {
    /// The caller supplied malformed SIR. No transformation was attempted.
    InvalidInput(Vec<SirDiagnostic>),
    /// A pass implementation violated a SIR invariant. The caller's original
    /// SIR remains intact.
    InvalidOutput(Vec<SirDiagnostic>),
}

/// Canonicalize every verified SIR body in a module transactionally.
///
/// The module form preserves callable-table validation around direct calls and
/// is the intended SIR-to-MIR pipeline boundary.
///
/// # Errors
///
/// Returns [`SirOptimizationError::InvalidInput`] when the module is not
/// valid SIR, or [`SirOptimizationError::InvalidOutput`] if canonicalization
/// would violate a module invariant. In either case, `module` is unchanged.
pub fn canonicalize_module_constant_cfg(
    module: &mut SemModule,
) -> Result<Vec<(CallableId, CfgCanonicalizationReport)>, SirOptimizationError> {
    let diagnostics = verify_module(module);
    if !diagnostics.is_empty() {
        return Err(SirOptimizationError::InvalidInput(diagnostics));
    }

    let mut candidate = module.clone();
    let facts = candidate.type_facts.clone();
    let aggregate_shapes = candidate.aggregate_shapes.clone();
    let variant_shapes = candidate.variant_shapes.clone();
    // The callables are not touched by a CFG rewrite, so one index over them
    // serves every body. `verify_module` above already validated the table.
    let callables = candidate.callables.clone();
    let context = crate::verify::callable_context(
        &callables,
        &candidate.closures,
        &candidate.actors,
        &candidate.supervisors,
        &candidate.vtables,
    );
    let mut reports = Vec::with_capacity(candidate.functions.len());
    for function in &mut candidate.functions {
        let report = canonicalize_verified_function(
            function,
            Some(&context),
            &facts,
            &aggregate_shapes,
            &variant_shapes,
        )
        .map_err(SirOptimizationError::InvalidOutput)?;
        reports.push((function.callable, report));
    }
    let diagnostics = verify_module(&candidate);
    if !diagnostics.is_empty() {
        return Err(SirOptimizationError::InvalidOutput(diagnostics));
    }

    *module = candidate;
    Ok(reports)
}

fn canonicalize_verified_function(
    function: &mut SemFunction,
    callable_context: Option<&crate::verify::CallableContext<'_>>,
    facts: &TypeFactTable,
    aggregate_shapes: &[crate::SemAggregateShape],
    variant_shapes: &[crate::SemVariantShape],
) -> Result<CfgCanonicalizationReport, Vec<SirDiagnostic>> {
    let before_folding = function.clone();
    let constants = direct_bool_constants(function);
    let initial_cfg = build_cfg_index(function);
    let mut folded_branches = 0;

    for block in &mut function.blocks {
        if !initial_cfg.is_reachable(block.id) {
            continue;
        }
        let selected = match &block.terminator {
            SemTerminator::Branch {
                condition,
                then_target,
                else_target,
            } => constants.get(&condition.value).map(|is_true| {
                if *is_true {
                    then_target.clone()
                } else {
                    else_target.clone()
                }
            }),
            SemTerminator::Return { .. }
            | SemTerminator::CheckedBinary { .. }
            | SemTerminator::SwitchVariant { .. }
            | SemTerminator::Call { .. }
            | SemTerminator::WireCodec { .. }
            | SemTerminator::RtCall { .. }
            | SemTerminator::ExternCall { .. }
            | SemTerminator::ActorCall { .. }
            | SemTerminator::ValueCall { .. }
            | SemTerminator::IndirectCall { .. }
            | SemTerminator::DynCall { .. }
            | SemTerminator::Goto(_)
            | SemTerminator::Trap { .. }
            | SemTerminator::EnterDefer { .. }
            | SemTerminator::FinishDefer { .. }
            | SemTerminator::CleanupDispatch { .. }
            | SemTerminator::RecoverFault { .. }
            | SemTerminator::CheckedRaiseFault { .. }
            | SemTerminator::Panic { .. }
            | SemTerminator::Suspend { .. }
            | SemTerminator::ResumeUnwind
            | SemTerminator::Unreachable => None,
        };
        if let Some(edge) = selected {
            // Retain the whole selected edge: its forwarded values are part of
            // the CFG meaning, and §1.4 pins their ownership kinds to the
            // target's block arguments.
            block.terminator = SemTerminator::Goto(edge);
            folded_branches += 1;
        }
    }

    // Keep the verifier boundary at the actual CFG rewrite, not only at the
    // public call boundary. This deliberately makes dead-block compaction a
    // separate audited transformation: later passes can follow this shape
    // without inventing a second validation convention.
    let diagnostics = crate::verify::verify_function_with_context(
        function,
        callable_context,
        facts,
        aggregate_shapes,
        variant_shapes,
    );
    if !diagnostics.is_empty() {
        return Err(diagnostics);
    }
    if !verify_cfg_discard_safety(&before_folding, function).is_empty() {
        // Constant folding is optional. If removing the unselected region
        // would erase a trap or ownership obligation, retain the original
        // verified CFG instead of turning a refused optimization into a
        // compilation failure.
        *function = before_folding;
        folded_branches = 0;
    }

    let post_fold_cfg = build_cfg_index(function);
    let (removed_blocks, block_remap) = compact_unreachable(function, post_fold_cfg.reachable());
    let diagnostics = crate::verify::verify_function_with_context(
        function,
        callable_context,
        facts,
        aggregate_shapes,
        variant_shapes,
    );
    if !diagnostics.is_empty() {
        return Err(diagnostics);
    }
    Ok(CfgCanonicalizationReport {
        folded_branches,
        removed_blocks,
        block_remap,
    })
}

fn direct_bool_constants(function: &SemFunction) -> BTreeMap<ValueId, bool> {
    let mut constants = BTreeMap::new();
    for operation in function.blocks.iter().flat_map(|block| &block.ops) {
        if let (SemOpKind::ConstBool(value), [result]) =
            (&operation.kind, operation.results.as_slice())
        {
            constants.insert(result.id, *value);
        }
    }
    constants
}

fn compact_unreachable(
    function: &mut SemFunction,
    reachable: &BTreeSet<BlockId>,
) -> (Vec<BlockId>, BTreeMap<BlockId, BlockId>) {
    let removed_blocks = function
        .blocks
        .iter()
        .filter_map(|block| (!reachable.contains(&block.id)).then_some(block.id))
        .collect::<Vec<_>>();

    // Make the entry block `bb0` while retaining former order for all other
    // reachable blocks. That gives deterministic dumps even for hand-built
    // SIR whose entry started at a nonzero canonical block index.
    let mut retained = Vec::with_capacity(function.blocks.len() - removed_blocks.len());
    let entry = function.entry;
    retained.extend(
        function
            .blocks
            .iter()
            .filter(|block| block.id == entry && reachable.contains(&block.id))
            .cloned(),
    );
    retained.extend(
        function
            .blocks
            .iter()
            .filter(|block| block.id != entry && reachable.contains(&block.id))
            .cloned(),
    );

    let block_remap = retained
        .iter()
        .enumerate()
        .map(|(index, block)| {
            (
                block.id,
                BlockId(
                    u32::try_from(index)
                        .expect("SIR block count exceeds the module-local ID range"),
                ),
            )
        })
        .collect::<BTreeMap<_, _>>();

    for block in &mut retained {
        let former = block.id;
        block.id = *block_remap
            .get(&former)
            .expect("every retained SIR block must have a canonical remap");
        block.terminator.visit_successors_mut(|edge| {
            edge.target = *block_remap
                .get(&edge.target)
                .expect("a verified reachable SIR edge must target a retained block");
        });
    }
    function.entry = *block_remap
        .get(&entry)
        .expect("a verified SIR entry must be reachable");
    function.blocks = retained;
    // A registration and its elaborated body are one fact. When every entry
    // for an action is proven unreachable — the drain sits behind a diverging
    // sibling body, say — the registration leaves with them.
    let entered = function
        .blocks
        .iter()
        .filter_map(|block| match block.terminator {
            SemTerminator::EnterDefer { defer, .. } => Some(defer),
            _ => None,
        })
        .collect::<BTreeSet<_>>();
    for block in &mut function.blocks {
        block.ops.retain(|op| match op.kind {
            SemOpKind::RegisterDefer { defer, .. } => entered.contains(&defer),
            _ => true,
        });
    }
    // Source binding rows name definitions, including loop-carried versions.
    // Once a proven unreachable block is removed, its value-only debug rows
    // must leave with it; otherwise they name values absent from valid IR.
    let mut values = function
        .params
        .iter()
        .map(|param| param.value)
        .collect::<BTreeSet<_>>();
    for block in &function.blocks {
        values.extend(block.args.iter().map(|arg| arg.value));
        values.extend(
            block
                .ops
                .iter()
                .flat_map(|op| op.results.iter().map(|result| result.id)),
        );
        block.terminator.visit_results(|result| {
            values.insert(result.id);
        });
    }
    function.bindings.retain(|binding| match binding.target {
        crate::BindingTarget::Value(value) => values.contains(&value),
        crate::BindingTarget::Place(_) => true,
    });

    (removed_blocks, block_remap)
}

/// Local roots whose copying read became a transfer, per body.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DeadLocalTransferReport {
    /// Operations rewritten from `load.copy` to `load.take`.
    pub transferred_reads: Vec<crate::OpId>,
}

/// Transfer a local's contents at its last read instead of copying them.
///
/// A local place read as a copy hands the consumer an independent owner and
/// leaves the place holding its own. When nothing reads the place again before
/// it is overwritten or its lifetime ends, that copy is a structural
/// allocation whose only remaining consumer is the release that follows, so the
/// read transfers the contents instead and the place is left uninitialized.
///
/// Only whole local roots with a structural copy recipe are considered:
/// retained and bit-copied contents cost nothing to read twice, and their
/// elision is a separate concern.
///
/// # Errors
///
/// Returns [`SirOptimizationError::InvalidInput`] when the module is not valid
/// SIR, or [`SirOptimizationError::InvalidOutput`] when the rewritten module
/// no longer verifies. In either case, `module` is unchanged.
pub fn transfer_module_dead_local_reads(
    module: &mut SemModule,
) -> Result<Vec<(CallableId, DeadLocalTransferReport)>, SirOptimizationError> {
    let diagnostics = verify_module(module);
    if !diagnostics.is_empty() {
        return Err(SirOptimizationError::InvalidInput(diagnostics));
    }

    let mut candidate = module.clone();
    let facts = candidate.type_facts.clone();
    let aggregate_shapes = candidate.aggregate_shapes.clone();
    let mut reports = Vec::with_capacity(candidate.functions.len());
    for function in &mut candidate.functions {
        let transferred_reads = transfer_dead_local_reads(function, &aggregate_shapes, &facts);
        reports.push((
            function.callable,
            DeadLocalTransferReport { transferred_reads },
        ));
    }
    let diagnostics = verify_module(&candidate);
    if !diagnostics.is_empty() {
        return Err(SirOptimizationError::InvalidOutput(diagnostics));
    }

    *module = candidate;
    Ok(reports)
}

/// Rewrite one body's dead-after copying reads and report the operations moved.
fn transfer_dead_local_reads(
    function: &mut SemFunction,
    aggregate_shapes: &[crate::SemAggregateShape],
    facts: &TypeFactTable,
) -> Vec<crate::OpId> {
    let Ok(plan) = crate::place_plan(function, aggregate_shapes, facts) else {
        return Vec::new();
    };
    let roots = transferable_roots(function, &plan, facts);
    if roots.is_empty() {
        return Vec::new();
    }
    let flow = PlaceFlow::new(function, &plan, &roots);
    let live = flow.live_out_sets(function);

    let mut transferred = Vec::new();
    for block in &mut function.blocks {
        let Some(live_out) = live.get(&block.id) else {
            continue;
        };
        let mut after = live_out.clone();
        for operation in block.ops.iter_mut().rev() {
            if let SemOpKind::LoadCopy { place } = operation.kind {
                if roots.contains(&place) && !after.contains(&place) {
                    operation.kind = SemOpKind::LoadTake { place };
                    transferred.push(operation.id);
                }
            }
            flow.step(&operation.kind, &mut after);
        }
    }
    transferred.sort_unstable();
    transferred
}

/// Whole local roots whose contents cost a structural copy to read.
fn transferable_roots(
    function: &SemFunction,
    plan: &crate::PlacePlan,
    facts: &TypeFactTable,
) -> BTreeSet<crate::PlaceId> {
    // A loan of any leaf reads the root's contents, so the root's eligibility
    // follows the loan's root rather than the borrowed place itself.
    let local_root = |place: crate::PlaceId| match plan.projection(place).map(|found| found.root) {
        Some(crate::OwnerRoot::Local(root)) => Some(root),
        _ => None,
    };
    let mut open_borrows: BTreeSet<crate::PlaceId> = BTreeSet::new();
    let mut closed = BTreeSet::new();
    let mut borrow_roots = BTreeMap::new();
    for operation in function.blocks.iter().flat_map(|block| &block.ops) {
        match (&operation.kind, operation.results.as_slice()) {
            (SemOpKind::LoadBorrow { place }, results) => {
                let Some(root) = local_root(*place) else {
                    continue;
                };
                if let [result] = results {
                    borrow_roots.insert(result.id, root);
                }
                open_borrows.insert(root);
            }
            (SemOpKind::EndBorrow { borrow }, _) => {
                if let Some(root) = borrow_roots.get(&borrow.value) {
                    closed.insert(*root);
                }
            }
            _ => {}
        }
    }
    // A suspension drain names its own place; it is not part of the operation
    // place traversal, so those roots keep their copies.
    let mut suspended = BTreeSet::new();
    for block in &function.blocks {
        if let SemTerminator::Suspend {
            kind:
                crate::SuspendKind::ValueClose {
                    place: Some(place), ..
                },
            ..
        } = &block.terminator
        {
            suspended.insert(*place);
        }
    }

    function
        .places
        .iter()
        .filter(|place| place.origin == crate::PlaceOrigin::Local)
        .filter(|place| {
            plan.projection(place.id)
                .is_some_and(|projection| projection.path.is_empty())
        })
        .filter(|place| {
            // A loan that never ends could still be reading the contents where
            // liveness sees nothing; leave those roots to their copies.
            !open_borrows.contains(&place.id) || closed.contains(&place.id)
        })
        .filter(|place| !suspended.contains(&place.id))
        .filter(|place| {
            matches!(
                facts
                    .get(&hew_types::TypeInstanceKey(place.ty.clone()))
                    .map(|row| row.clone),
                Some(hew_types::CloneKind::DeepCopy | hew_types::CloneKind::FieldWise)
            )
        })
        .map(|place| place.id)
        .collect()
}

/// Backward liveness of transferable local roots over one body.
struct PlaceFlow<'a> {
    /// Every declared place mapped to the transferable root it belongs to.
    root_of: BTreeMap<crate::PlaceId, crate::PlaceId>,
    /// Borrow values mapped to the root their place belongs to.
    borrow_root: BTreeMap<ValueId, crate::PlaceId>,
    roots: &'a BTreeSet<crate::PlaceId>,
}

impl<'a> PlaceFlow<'a> {
    fn new(
        function: &SemFunction,
        plan: &crate::PlacePlan,
        roots: &'a BTreeSet<crate::PlaceId>,
    ) -> Self {
        let mut root_of = BTreeMap::new();
        for place in &function.places {
            if let Some(crate::OwnerRoot::Local(root)) =
                plan.projection(place.id).map(|projection| projection.root)
            {
                if roots.contains(&root) {
                    root_of.insert(place.id, root);
                }
            }
        }
        let mut borrow_root = BTreeMap::new();
        for operation in function.blocks.iter().flat_map(|block| &block.ops) {
            if let (SemOpKind::LoadBorrow { place }, [result]) =
                (&operation.kind, operation.results.as_slice())
            {
                if let Some(root) = root_of.get(place) {
                    borrow_root.insert(result.id, *root);
                }
            }
        }
        Self {
            root_of,
            borrow_root,
            roots,
        }
    }

    /// Update the live set backwards across one operation.
    fn step(&self, kind: &SemOpKind, live: &mut BTreeSet<crate::PlaceId>) {
        // A whole-root store or lifetime end replaces the contents without
        // reading them; every other mention keeps them live.
        match kind {
            SemOpKind::AllocPlace { place }
            | SemOpKind::StoreInit { place, .. }
            | SemOpKind::StoreAssign { place, .. }
            | SemOpKind::EndLifetime { place }
                if self.roots.contains(place) =>
            {
                live.remove(place);
                return;
            }
            SemOpKind::EndBorrow { borrow } => {
                if let Some(root) = self.borrow_root.get(&borrow.value) {
                    live.insert(*root);
                }
            }
            _ => {}
        }
        kind.visit_places(|place| {
            if let Some(root) = self.root_of.get(&place) {
                live.insert(*root);
            }
        });
    }

    /// Live-out sets per reachable block, to a fixed point over the CFG.
    fn live_out_sets(&self, function: &SemFunction) -> BTreeMap<BlockId, BTreeSet<crate::PlaceId>> {
        let cfg = build_cfg_index(function);
        let order: Vec<BlockId> = cfg.rpo().iter().rev().copied().collect();
        let mut live_in: BTreeMap<BlockId, BTreeSet<crate::PlaceId>> = BTreeMap::new();
        let mut live_out: BTreeMap<BlockId, BTreeSet<crate::PlaceId>> = BTreeMap::new();
        let blocks: BTreeMap<BlockId, &crate::SemBlock> = function
            .blocks
            .iter()
            .map(|block| (block.id, block))
            .collect();
        let mut changed = true;
        while changed {
            changed = false;
            for &id in &order {
                let Some(block) = blocks.get(&id) else {
                    continue;
                };
                let mut out = BTreeSet::new();
                for edge in cfg.successors_of(id) {
                    if let Some(target) = cfg.edge_target(*edge) {
                        if let Some(entry) = live_in.get(&target) {
                            out.extend(entry.iter().copied());
                        }
                    }
                }
                let mut state = out.clone();
                for operation in block.ops.iter().rev() {
                    self.step(&operation.kind, &mut state);
                }
                if live_out.get(&id) != Some(&out) {
                    live_out.insert(id, out);
                    changed = true;
                }
                if live_in.get(&id) != Some(&state) {
                    live_in.insert(id, state);
                    changed = true;
                }
            }
        }
        live_out
    }
}
