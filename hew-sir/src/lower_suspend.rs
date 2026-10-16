//! Source suspension boundaries and their explicit cleanup continuations.

use super::{Builder, CallResult, Edge, HirExpr, Operand, ResolvedTy, SemTerminator};
use crate::{BoundaryDecision, BoundaryOperand, SuspendKind};

impl Builder<'_, '_> {
    pub(super) fn lower_safepoint(&mut self) -> Result<(), String> {
        if !self.defer_bodies.is_empty() {
            return Ok(());
        }
        let saved = self.control_state();
        let recovery = self.recovery_bodies.clone();
        let normal = self.new_block(Vec::new());
        let cancel = self.new_block(Vec::new());
        let edge = |target| Edge {
            target,
            args: Vec::new(),
        };
        self.set_terminator(SemTerminator::Safepoint {
            resume: edge(normal),
            cancel: edge(cancel),
        })?;
        self.current = cancel;
        self.finish_fault_exit()?;
        self.restore_control_state(&saved);
        self.recovery_bodies = recovery;
        self.current = normal;
        Ok(())
    }

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

pub(super) fn remove_bounded_entry_poll(function: &mut crate::SemFunction) {
    // A call-free, straight-line body performs bounded work. Keep its
    // direct ABI; all loops and recursive call paths retain entry polling.
    let bounded_leaf = !function.blocks.iter().any(|block| {
        if block.id == function.entry {
            return false;
        }
        matches!(
            block.terminator,
            SemTerminator::Call { .. }
                | SemTerminator::IndirectCall { .. }
                | SemTerminator::DynCall { .. }
                | SemTerminator::ValueCall { .. }
                | SemTerminator::ActorCall { .. }
                | SemTerminator::WireCodec { .. }
                | SemTerminator::RtCall { .. }
                | SemTerminator::ExternCall { .. }
                | SemTerminator::Suspend { .. }
                | SemTerminator::Safepoint { .. }
        )
    });
    if bounded_leaf {
        let entry = &mut function.blocks[function.entry.0 as usize];
        if let SemTerminator::Safepoint { resume, .. } = &entry.terminator {
            entry.terminator = SemTerminator::Goto(resume.clone());
        }
    }
}
