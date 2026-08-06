//! VecIter-yield abandonment analysis for the drop-elaboration pass.
//!
//! Carved from `drop_plan.rs` as a sibling concern module (line-ceiling
//! ratchet): the body-region walk and the conditionally-consumed-yield
//! rejection form one self-contained authority consumed by `elaborate` and
//! the exit-plan construction.
use super::{
    dataflow, BasicBlock, Builder, CheckedMirFunction, HashSet, MirDiagnostic, MirDiagnosticKind,
    Terminator,
};

/// Blocks belonging to a `VecIter` yield body, walked from the recorded
/// body-start block up to (and excluding) the body-end block.
pub(in crate::lower) fn vec_iter_yield_body_region(
    blocks: &[BasicBlock],
    exit_drop: &crate::lower::VecIterYieldExitDrop,
) -> HashSet<u32> {
    let mut region = HashSet::from([exit_drop.body_start_block]);
    let mut worklist = vec![exit_drop.body_start_block];
    while let Some(block_id) = worklist.pop() {
        if block_id == exit_drop.body_end_block {
            continue;
        }
        let Some(block) = blocks.iter().find(|block| block.id == block_id) else {
            continue;
        };
        for successor in block.successors() {
            if successor != exit_drop.body_end_block && region.insert(successor) {
                worklist.push(successor);
            }
        }
    }
    region.remove(&exit_drop.body_end_block);
    region
}

/// Reject a conditionally-consumed `VecIter` yield that reaches an abandonment
/// exit with `MaybeConsumed` state. An unconditional drop there could
/// double-release the consumed predecessor, while omitting it would leak the
/// still-live predecessor. Until the exit plan carries a runtime ownership
/// sidecar for yielded payloads, this shape has no exact cleanup authority.
pub(in crate::lower) fn vec_iter_yield_abandonment_diagnostics(
    checked: &CheckedMirFunction,
    builder: &Builder,
    dataflow_result: &dataflow::DataflowResult,
) -> Vec<MirDiagnostic> {
    let cancellation_blocks: HashSet<u32> = checked
        .cooperate_sites
        .iter()
        .map(|site| site.bb_id)
        .collect();
    let mut diagnostics = Vec::new();
    for exit_drop in &builder.vec_iter_yield_exit_drops {
        let region = vec_iter_yield_body_region(&checked.blocks, exit_drop);
        let ambiguous = checked.blocks.iter().any(|block| {
            if !region.contains(&block.id) {
                return false;
            }
            let abandons = cancellation_blocks.contains(&block.id)
                || matches!(
                    block.terminator,
                    Terminator::Trap { .. }
                        | Terminator::Yield { .. }
                        | Terminator::Suspend { .. }
                        | Terminator::SuspendingScopeDeadline { .. }
                        | Terminator::SuspendingSelect { .. }
                );
            abandons
                && matches!(
                    dataflow_result
                        .exit_states
                        .get(&block.id)
                        .and_then(|states| states.get(&exit_drop.binding)),
                    Some(dataflow::BindingState::MaybeConsumed(_))
                )
        });
        if ambiguous {
            diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "conditionally moved VecIter yield across an abandonment point"
                        .to_string(),
                    site: exit_drop.site,
                },
                note: "the yielded value is consumed on only some paths before a \
                       cancellation, panic, yield, or suspend exit. The exit cannot \
                       unconditionally release it without double-freeing the consumed \
                       path, and omitting the release would leak the live path; move the \
                       value on every path or place the abandonment point before the \
                       conditional move"
                    .to_string(),
            });
        }
    }
    diagnostics
}
