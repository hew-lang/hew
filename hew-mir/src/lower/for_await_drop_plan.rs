//! Terminal drop-plan repair for `for await` handle hand-offs.
//!
//! The synthetic cursor takes ownership of a `Stream<T>` / `Receiver<T>` only
//! at the recorded whole-value move. Terminal exits before that move still own
//! the source; coroutine abandonment after it owns the cursor until the raw
//! lexical close. This module admits exactly that live owner and merges the
//! resulting close into the existing plan by the builder's total declaration
//! order.

use std::collections::{HashMap, HashSet};

use hew_hir::BindingId;
use hew_types::ResolvedTy;

use crate::model::{BasicBlock, CheckedMirFunction, DropPlan, ElabDrop, ExitPath, Instr, Place};

use super::{
    dataflow,
    drop_plan::{drop_kind_for, stream_handle_drop_descriptor},
    Builder, ENTRY_BLOCK_ID,
};

/// Re-admit the live side of each recorded `for await` handle hand-off on
/// terminal exits, then merge those closes into the existing plans in
/// declaration-LIFO order.
pub(super) fn admit_terminal_handoff_drops(
    checked: &CheckedMirFunction,
    builder: &Builder,
    dataflow_result: &dataflow::DataflowResult,
    returned_member_candidates: &[(BindingId, String, ResolvedTy)],
    drop_plans: &mut [(ExitPath, DropPlan)],
) {
    let mut pending = HashMap::new();
    admit_source_terminal_drops(
        builder,
        dataflow_result,
        returned_member_candidates,
        drop_plans,
        &mut pending,
    );
    admit_cursor_abandonment_drops(checked, builder, dataflow_result, drop_plans, &mut pending);
    merge_declaration_ranked_drops(builder, drop_plans, pending);
}

/// Admit the source close only on terminal paths that leave before hand-off.
fn admit_source_terminal_drops(
    builder: &Builder,
    dataflow_result: &dataflow::DataflowResult,
    returned_member_candidates: &[(BindingId, String, ResolvedTy)],
    drop_plans: &[(ExitPath, DropPlan)],
    pending: &mut HashMap<usize, Vec<(BindingId, ElabDrop)>>,
) {
    // A `for await` cursor takes ownership of a stream only at its recorded
    // whole-value hand-off. The source was correctly removed from function-wide
    // LIFO after that point, but all terminal exits before it still need the
    // original typed close. The append-only hand-off ledger is the provenance
    // authority; a source enters only if it remains an ownership candidate.
    for handoff in builder.for_await_handle_handoffs.iter().rev() {
        if !returned_member_candidates
            .iter()
            .any(|(binding, _name, _ty)| *binding == handoff.source_binding)
        {
            continue;
        }
        let Some(descriptor) = stream_handle_drop_descriptor(&handoff.ty) else {
            continue;
        };
        let Some(&place) = builder.binding_locals.get(&handoff.source_binding) else {
            continue;
        };
        let deferred_drop = ElabDrop {
            place,
            ty: handoff.ty.clone(),
            drop_fn: Some(crate::model::DropFnSpec::Runtime(descriptor)),
            kind: drop_kind_for(place, &handoff.ty, None),
            guard: None,
        };
        for (plan_index, (exit, plan)) in drop_plans.iter().enumerate() {
            let block = match exit {
                ExitPath::Return { block }
                | ExitPath::Cancel { block }
                | ExitPath::Panic { block }
                | ExitPath::Yield { block, .. }
                | ExitPath::Suspend { block, .. } => *block,
                ExitPath::Goto { .. }
                | ExitPath::Branch { .. }
                | ExitPath::Call { .. }
                | ExitPath::Send { .. }
                | ExitPath::Ask { .. }
                | ExitPath::Select { .. }
                | ExitPath::Join { .. } => continue,
            };
            let is_entry_cancel =
                matches!(exit, ExitPath::Cancel { .. }) && block == ENTRY_BLOCK_ID;
            let state_maps = if is_entry_cancel {
                &dataflow_result.entry_states
            } else {
                &dataflow_result.exit_states
            };
            let source_is_live = state_maps
                .get(&block)
                .and_then(|states| states.get(&handoff.source_binding))
                .copied()
                == Some(dataflow::BindingState::Live);
            if source_is_live
                && !plan
                    .drops
                    .iter()
                    .any(|drop| drop.place == deferred_drop.place)
            {
                pending
                    .entry(plan_index)
                    .or_default()
                    .push((handoff.source_binding, deferred_drop.clone()));
            }
        }
    }
}

/// Admit the cursor close only on abandonment paths after hand-off and before
/// its inline lexical close.
fn admit_cursor_abandonment_drops(
    checked: &CheckedMirFunction,
    builder: &Builder,
    dataflow_result: &dataflow::DataflowResult,
    drop_plans: &[(ExitPath, DropPlan)],
    pending: &mut HashMap<usize, Vec<(BindingId, ElabDrop)>>,
) {
    // The cursor itself normally closes inline when its desugar scope exits.
    // Returns already emit that inline edge drop; cutting the region at the raw
    // close prevents a stale dataflow `Live` state from reviving it after
    // normal exhaustion.
    for handoff in builder.for_await_handle_handoffs.iter().rev() {
        let Some(descriptor) = stream_handle_drop_descriptor(&handoff.ty) else {
            continue;
        };
        let Some(&place) = builder.binding_locals.get(&handoff.cursor_binding) else {
            continue;
        };
        let abandonment_region =
            cursor_abandonment_region(&checked.blocks, handoff.handoff_block, place, descriptor);
        let deferred_drop = ElabDrop {
            place,
            ty: handoff.ty.clone(),
            drop_fn: Some(crate::model::DropFnSpec::Runtime(descriptor)),
            kind: drop_kind_for(place, &handoff.ty, None),
            guard: None,
        };
        for (plan_index, (exit, plan)) in drop_plans.iter().enumerate() {
            let block = match exit {
                ExitPath::Cancel { block }
                | ExitPath::Panic { block }
                | ExitPath::Yield { block, .. }
                | ExitPath::Suspend { block, .. } => *block,
                ExitPath::Return { .. }
                | ExitPath::Goto { .. }
                | ExitPath::Branch { .. }
                | ExitPath::Call { .. }
                | ExitPath::Send { .. }
                | ExitPath::Ask { .. }
                | ExitPath::Select { .. }
                | ExitPath::Join { .. } => continue,
            };
            if !abandonment_region.contains(&block) {
                continue;
            }
            let is_entry_cancel =
                matches!(exit, ExitPath::Cancel { .. }) && block == ENTRY_BLOCK_ID;
            let state_maps = if is_entry_cancel {
                &dataflow_result.entry_states
            } else {
                &dataflow_result.exit_states
            };
            let cursor_is_live = state_maps
                .get(&block)
                .and_then(|states| states.get(&handoff.cursor_binding))
                .copied()
                == Some(dataflow::BindingState::Live);
            if cursor_is_live && !plan.drops.iter().any(|drop| drop.place == place) {
                pending
                    .entry(plan_index)
                    .or_default()
                    .push((handoff.cursor_binding, deferred_drop.clone()));
            }
        }
    }
}

/// Merge independently admitted source/cursor closes into existing plans by
/// the same total declaration-LIFO order used by ordinary owners.
fn merge_declaration_ranked_drops(
    builder: &Builder,
    drop_plans: &mut [(ExitPath, DropPlan)],
    pending: HashMap<usize, Vec<(BindingId, ElabDrop)>>,
) {
    // Source and cursor eligibility are intentionally proven separately above,
    // but all admitted drops share ordinary declaration-LIFO order. Rank them
    // against the total binding-declaration ledger, not `owned_locals`: tuple-
    // derived channel handles and their synthetic `for await` cursor own an
    // inline/abandonment close without entering the ordinary ownership ledger.
    // The declaration ledger includes both parameters and every checker-stream
    // Bind seam, so it orders those owners among ordinary and synthetic owners
    // without inventing a fallback rank.
    let declaration_rank: HashMap<BindingId, usize> = builder
        .binding_declaration_order
        .iter()
        .copied()
        .enumerate()
        .map(|(rank, binding)| (binding, rank))
        .collect();
    let mut declaration_rank_by_place = HashMap::new();
    for (rank, binding) in builder
        .binding_declaration_order
        .iter()
        .copied()
        .enumerate()
    {
        if let Some(&place) = builder.binding_locals.get(&binding) {
            declaration_rank_by_place.insert(place, rank);
        }
    }
    for (plan_index, mut plan_pending) in pending {
        plan_pending.sort_by_key(|(binding, _drop)| {
            std::cmp::Reverse(*declaration_rank.get(binding).unwrap_or(&usize::MAX))
        });
        let plan = &mut drop_plans[plan_index].1;
        plan_pending.retain(|(_binding, drop)| {
            !plan
                .drops
                .iter()
                .any(|existing| existing.place == drop.place)
        });
        if plan_pending.is_empty() {
            continue;
        }
        for (binding, drop) in plan_pending {
            let rank = *declaration_rank
                .get(&binding)
                .expect("recorded hand-off binding has a declaration-ledger rank");
            let insertion = plan
                .drops
                .iter()
                .position(|existing| {
                    declaration_rank_by_place
                        .get(&existing.place)
                        .is_some_and(|existing_rank| *existing_rank < rank)
                })
                .unwrap_or_else(|| {
                    plan.drops
                        .iter()
                        .rposition(|existing| {
                            declaration_rank_by_place.contains_key(&existing.place)
                        })
                        .map_or(plan.drops.len(), |index| index + 1)
                });
            plan.drops.insert(insertion, drop);
        }
    }
}

/// The synthetic cursor owns its handle after `handoff_block` until its raw
/// lexical close. This region is used only for abandonment plans: normal
/// return/scope-exit execution already reaches the inline close instruction.
fn cursor_abandonment_region(
    blocks: &[BasicBlock],
    handoff_block: u32,
    cursor_place: Place,
    descriptor: hew_types::runtime_call::RuntimeDropDescriptor,
) -> HashSet<u32> {
    let block_by_id: HashMap<u32, &BasicBlock> =
        blocks.iter().map(|block| (block.id, block)).collect();
    let mut region = HashSet::new();
    let mut worklist = vec![handoff_block];
    while let Some(block_id) = worklist.pop() {
        if !region.insert(block_id) {
            continue;
        }
        let Some(block) = block_by_id.get(&block_id) else {
            continue;
        };
        let closes_cursor = block.instructions.iter().any(|instruction| {
            matches!(
                instruction,
                Instr::Drop {
                    place,
                    drop_fn: Some(crate::model::DropFnSpec::Runtime(actual)),
                    ..
                } if *place == cursor_place && *actual == descriptor
            )
        });
        if closes_cursor {
            region.remove(&block_id);
            continue;
        }
        worklist.extend(block.successors());
    }
    region
}
