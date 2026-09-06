//! Source suspension boundaries and their explicit cleanup continuations.

use super::{Builder, CallResult, Edge, HirExpr, Operand, ResolvedTy, SemTerminator};
use crate::{BoundaryDecision, BoundaryOperand, SuspendKind};

impl Builder<'_, '_> {
    pub(super) fn lower_sleep(
        &mut self,
        expression: &HirExpr,
        args: &[HirExpr],
    ) -> Result<(), String> {
        let [duration] = args else {
            return Err("sleep requires exactly one duration".into());
        };
        if self.ty(&duration.ty) != ResolvedTy::Duration
            || self.ty(&expression.ty) != ResolvedTy::Unit
        {
            return Err("sleep requires a duration input and unit output".into());
        }
        let duration = self.lower_expr(duration)?;
        let live = self.owned_live.clone();
        let normal = self.new_block(Vec::new());
        let cancel = self.new_block(Vec::new());
        let unwind = self.new_block(Vec::new());
        self.set_terminator(SemTerminator::Suspend {
            kind: SuspendKind::Sleep,
            inputs: vec![BoundaryOperand {
                operand: Operand { value: duration },
                decision: BoundaryDecision::Copy,
            }],
            result: CallResult::Unit,
            resumes: vec![Edge {
                target: normal,
                args: Vec::new(),
            }],
            cancel: Edge {
                target: cancel,
                args: Vec::new(),
            },
            unwind: Edge {
                target: unwind,
                args: Vec::new(),
            },
        })?;
        for cleanup in [cancel, unwind] {
            self.current = cleanup;
            self.owned_live = live.clone();
            self.finish_fault_exit()?;
        }
        self.current = normal;
        self.owned_live = live;
        Ok(())
    }
}
