//! Source suspension boundaries and their explicit cleanup continuations.

use crate::SirLoweringError;

use super::{Builder, CallResult, Edge, HirExpr, Operand, ResolvedTy, SemTerminator};
use crate::{BoundaryDecision, BoundaryOperand, SuspendKind};

impl Builder<'_, '_> {
    pub(super) fn lower_safepoint(&mut self) -> Result<(), SirLoweringError> {
        if !self.defer_bodies.is_empty() {
            return Ok(());
        }
        let saved = self.control_state();
        let recovery = self.recovery_bodies.clone();
        let normal = self.new_block(Vec::new());
        let mandatory_close = self
            .callable
            .signature
            .params
            .first()
            .is_some_and(|receiver| {
                crate::resource::record_resource_lifecycle(self.service.module, &receiver.ty)
                    .map(|lifecycle| lifecycle.close_declaration)
                    .or_else(|| {
                        crate::resource::authored_opaque_lifecycle(
                            self.service.module,
                            &receiver.ty,
                        )
                        .map(|lifecycle| lifecycle.close_declaration)
                    })
                    == Some(self.callable.declaration)
            });
        let cancel = (!mandatory_close).then(|| self.new_block(Vec::new()));
        let edge = |target| Edge {
            target,
            args: Vec::new(),
        };
        self.set_terminator(SemTerminator::Safepoint {
            resume: edge(normal),
            cancel: cancel.map(edge),
        })?;
        if let Some(cancel) = cancel {
            self.current = cancel;
            self.finish_fault_exit()?;
        }
        self.restore_control_state(&saved);
        self.recovery_bodies = recovery;
        self.current = normal;
        Ok(())
    }

    pub(super) fn lower_sleep(
        &mut self,
        expression: &HirExpr,
        args: &[HirExpr],
    ) -> Result<(), SirLoweringError> {
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
    ) -> Result<(), SirLoweringError> {
        self.lower_timer_suspend(expression, args, SuspendKind::SleepUntil, &ResolvedTy::I64)
    }

    fn lower_timer_suspend(
        &mut self,
        expression: &HirExpr,
        args: &[HirExpr],
        kind: SuspendKind,
        input_ty: &ResolvedTy,
    ) -> Result<(), SirLoweringError> {
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
