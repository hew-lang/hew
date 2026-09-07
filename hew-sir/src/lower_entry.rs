//! The process-entry adapter for a `Result` entry: SIR realizes the checker's
//! exit plan as ordinary ownership operations, so physical lowering only ever
//! sees an integer exit status.

use super::{Builder, EntryAdapter, PreparedCallee};
use crate::{
    BoundaryDecision, BoundaryOperand, CallResult, Edge, Operand, OwnKind, Provenance, SemOpKind,
    SemParamPassing, SemTerminator,
};
use hew_types::{EntryExitAction, ResolvedTy, RuntimeCallFamily};

impl Builder<'_, '_> {
    /// Call the selected entry, then map its `Result` to the process status:
    /// `Ok(())` exits 0; `Err(error)` renders the error through the selected
    /// `Display::fmt`, writes `error: <text>` to stderr and exits 1.
    pub(super) fn lower_entry_adapter(&mut self, adapter: &EntryAdapter) -> Result<(), String> {
        let EntryExitAction::Result { display, .. } = &adapter.action else {
            return Err("entry adapter requires a Result exit action".into());
        };
        let entry = self
            .service
            .callable(adapter.entry)
            .cloned()
            .ok_or("entry adapter has no entry callable")?;
        self.service.request_body(entry.id);
        let live_before: std::collections::HashSet<_> = self.owned_live.keys().copied().collect();
        let result = self
            .finish_user_call(
                PreparedCallee::Direct(entry.id),
                entry.signature.clone(),
                Vec::new(),
                &[],
                &live_before,
                true,
            )?
            .ok_or("entry callable produced no result")?;
        let result_ty = entry.signature.return_ty.clone();
        let shape = self.service.require_variant_shape(&result_ty)?;
        let descriptor = self
            .service
            .variant_shapes
            .get(usize::try_from(shape.0).map_err(|_| "variant shape id exceeds usize")?)
            .cloned()
            .ok_or_else(|| format!("variant shape {} disappeared during lowering", shape.0))?;
        if descriptor.variants.len() != 2 {
            return Err(format!(
                "entry result `{}` is not the two-variant Result shape",
                result_ty.user_facing()
            ));
        }
        let branches = self.emit_variant_switch(shape, &descriptor, result)?;
        let inherited = self.control_state();
        for branch in branches {
            self.restore_control_state(&inherited);
            self.current = branch.block;
            self.owned_live = branch.owned_live;
            let status = if branch.variant == 0 {
                0
            } else {
                let error = branch
                    .fields
                    .first()
                    .ok_or("entry Err variant carries no payload")?
                    .value;
                let rendered = self.render_entry_error(display, error)?;
                self.emit_stderr_literal("error: ")?;
                self.emit_stderr_write(rendered)?;
                self.emit_destroy(rendered)?;
                self.emit_stderr_literal("\n")?;
                1
            };
            let status = self.emit_typed(
                Provenance::Synthesized,
                &ResolvedTy::I64,
                SemOpKind::ConstI64(status),
            )?;
            self.finish_return_value(Some(BoundaryOperand {
                operand: Operand { value: status },
                decision: BoundaryDecision::Move,
            }))?;
        }
        Ok(())
    }

    /// Call the checker-selected `Display::fmt` on the entry error.
    fn render_entry_error(
        &mut self,
        display: &hew_types::EntryDisplayTarget,
        error: crate::ValueId,
    ) -> Result<crate::ValueId, String> {
        let target = self.service.resolve_entry_display(display)?;
        let [receiver] = target.signature.params.as_slice() else {
            return Err("entry Display target must take exactly its receiver".into());
        };
        let decision = match receiver.passing {
            SemParamPassing::Consume => BoundaryDecision::Move,
            SemParamPassing::Borrow => BoundaryDecision::Borrow,
            SemParamPassing::BorrowMut => BoundaryDecision::BorrowMut,
            SemParamPassing::ReadOnly => BoundaryDecision::Copy,
        };
        let live_before: std::collections::HashSet<_> = self.owned_live.keys().copied().collect();
        let rendered = self
            .finish_user_call(
                PreparedCallee::Direct(target.id),
                target.signature.clone(),
                vec![BoundaryOperand {
                    operand: Operand { value: error },
                    decision,
                }],
                &[],
                &live_before,
                true,
            )?
            .ok_or("entry Display target produced no string")?;
        if self.owned_live.contains_key(&error) {
            self.emit_destroy(error)?;
        }
        Ok(rendered)
    }

    fn emit_stderr_literal(&mut self, text: &str) -> Result<(), String> {
        let literal = self.service.intern_string(text);
        let value = self.emit_typed(
            Provenance::Synthesized,
            &ResolvedTy::String,
            SemOpKind::ConstStr(literal),
        )?;
        self.emit_stderr_write(value)?;
        self.emit_destroy(value)
    }

    /// Write one live string to stderr and continue.
    fn emit_stderr_write(&mut self, value: crate::ValueId) -> Result<(), String> {
        if self.value_own_kind(value) != Some(OwnKind::Owned) {
            return Err("stderr write requires an owned string".into());
        }
        let normal = self.new_block(Vec::new());
        let id = crate::OpId(self.ops);
        self.ops += 1;
        self.set_terminator(SemTerminator::RtCall {
            id,
            family: RuntimeCallFamily::StderrWrite,
            args: vec![BoundaryOperand {
                operand: Operand { value },
                decision: BoundaryDecision::Borrow,
            }],
            result: CallResult::Unit,
            normal: Edge {
                target: normal,
                args: Vec::new(),
            },
            unwind: crate::CallUnwind::NotApplicable,
        })?;
        self.current = normal;
        Ok(())
    }
}
