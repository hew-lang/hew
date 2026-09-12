//! Source suspension boundaries and their explicit cleanup continuations.

use super::{Builder, CallResult, Edge, HirExpr, Operand, ResolvedTy, SemTerminator};
use crate::{BoundaryDecision, BoundaryOperand, SuspendKind};

impl Builder<'_, '_> {
    pub(super) fn lower_sleep(
        &mut self,
        expression: &HirExpr,
        args: &[HirExpr],
    ) -> Result<(), String> {
        self.lower_timer_suspend(expression, args, SuspendKind::Sleep, &ResolvedTy::Duration)
    }

    /// `sleep_until(t)` suspends on a deadline, not on a span. The instant
    /// crosses the boundary intact (`instant` is `i64` by the time SIR sees
    /// it); the runtime measures the remaining wait against the same monotonic
    /// clock when it arms the timer, so the compiler never subtracts.
    pub(super) fn lower_sleep_until(
        &mut self,
        expression: &HirExpr,
        args: &[HirExpr],
    ) -> Result<(), String> {
        self.lower_timer_suspend(expression, args, SuspendKind::SleepUntil, &ResolvedTy::I64)
    }

    fn lower_timer_suspend(
        &mut self,
        expression: &HirExpr,
        args: &[HirExpr],
        kind: SuspendKind,
        input_ty: &ResolvedTy,
    ) -> Result<(), String> {
        let [input] = args else {
            return Err("a timer suspension requires exactly one input".into());
        };
        if self.ty(&input.ty) != *input_ty || self.ty(&expression.ty) != ResolvedTy::Unit {
            return Err("a timer suspension requires a scalar input and unit output".into());
        }
        let input = self.lower_expr(input)?;
        let live = self.owned_live.clone();
        let normal = self.new_block(Vec::new());
        let cancel = self.new_block(Vec::new());
        let unwind = self.new_block(Vec::new());
        self.set_terminator(SemTerminator::Suspend {
            kind,
            inputs: vec![BoundaryOperand {
                operand: Operand { value: input },
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
