//! Small, verifier-backed SIR canonicalization passes.
//!
//! This module intentionally starts with CFG canonicalization rather than a
//! general pass manager. Each transformation is transactional: malformed input
//! is rejected before mutation, and the verifier must accept the complete
//! result before it becomes visible to a caller.

use std::collections::{BTreeMap, BTreeSet};

use crate::verify::verify_cfg_discard_safety;
use crate::{
    build_cfg_index, verify_function, verify_module, BlockId, CallableId, SemFunction, SemModule,
    SemOpKind, SemTerminator, SirDiagnostic, ValueId,
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

/// Fold direct constant-boolean branches and compact unreachable blocks.
///
/// This function is suitable for local SIR construction tests. Inter-IR
/// boundaries that need direct-call validation should use
/// [`canonicalize_module_constant_cfg`] instead.
///
/// # Errors
///
/// Returns [`SirOptimizationError::InvalidInput`] when `function` is not
/// valid SIR, or [`SirOptimizationError::InvalidOutput`] if the pass would
/// produce invalid SIR. In either case, `function` is unchanged.
pub fn canonicalize_constant_cfg(
    function: &mut SemFunction,
) -> Result<CfgCanonicalizationReport, SirOptimizationError> {
    let diagnostics = verify_function(function);
    if !diagnostics.is_empty() {
        return Err(SirOptimizationError::InvalidInput(diagnostics));
    }

    let mut candidate = function.clone();
    let report = canonicalize_verified_function(&mut candidate)
        .map_err(SirOptimizationError::InvalidOutput)?;
    let diagnostics = verify_function(&candidate);
    if !diagnostics.is_empty() {
        return Err(SirOptimizationError::InvalidOutput(diagnostics));
    }

    *function = candidate;
    Ok(report)
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
    let mut reports = Vec::with_capacity(candidate.functions.len());
    for function in &mut candidate.functions {
        let report = canonicalize_verified_function(function)
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
            SemTerminator::Return { .. } | SemTerminator::Goto(_) | SemTerminator::Unreachable => {
                None
            }
        };
        if let Some(edge) = selected {
            // Retain the whole selected edge: its forwarded values and their
            // semantic ownership modes are part of the CFG meaning.
            block.terminator = SemTerminator::Goto(edge);
            folded_branches += 1;
        }
    }

    // Keep the verifier boundary at the actual CFG rewrite, not only at the
    // public call boundary. This deliberately makes dead-block compaction a
    // separate audited transformation: later passes can follow this shape
    // without inventing a second validation convention.
    let diagnostics = verify_function(function);
    if !diagnostics.is_empty() {
        return Err(diagnostics);
    }
    let diagnostics = verify_cfg_discard_safety(&before_folding, function);
    if !diagnostics.is_empty() {
        return Err(diagnostics);
    }

    let post_fold_cfg = build_cfg_index(function);
    let (removed_blocks, block_remap) = compact_unreachable(function, post_fold_cfg.reachable());
    let diagnostics = verify_function(function);
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

    (removed_blocks, block_remap)
}
