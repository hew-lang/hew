//! The process-entry adapter for a `Result` entry: SIR realizes the checker's
//! exit plan as ordinary ownership operations, so physical lowering only ever
//! sees an integer exit status.

use crate::SirLoweringError;

use super::{Builder, EntryAdapter, PreparedCallee};
use crate::{
    BoundaryDecision, BoundaryOperand, CallResult, Edge, Operand, OwnKind, Provenance, SemOpKind,
    SemParamPassing, SemTerminator,
};
use hew_types::{EntryExitAction, ResolvedTy, RuntimeCallFamily};

impl Builder<'_, '_> {
    /// Call the selected entry, then map its `Result` to the process status:
    /// `Ok(())` exits 0; `Err(error.into())` renders the error through the selected
    /// `Display::fmt`, writes `error: <text>` to stderr and exits 1.
    #[expect(
        clippy::too_many_lines,
        reason = "entry adaptation keeps success and error ownership together"
    )]
    pub(super) fn lower_entry_adapter(
        &mut self,
        adapter: &EntryAdapter,
    ) -> Result<(), SirLoweringError> {
        let entry = self
            .service
            .callable(adapter.entry)
            .cloned()
            .ok_or(SirLoweringError::from(
                "entry adapter has no entry callable",
            ))?;
        self.service.request_body(entry.id);
        let mut arguments = Vec::with_capacity(entry.signature.params.len());
        for (index, parameter) in entry.signature.params.iter().enumerate() {
            let value = if adapter.benchmark {
                let shape = self.service.require_aggregate_shape(&parameter.ty)?;
                self.emit_typed(
                    Provenance::Synthesized,
                    &parameter.ty,
                    SemOpKind::AggregateMake {
                        shape,
                        fields: Vec::new(),
                    },
                )?
            } else {
                self.lower_property_argument(index, &parameter.ty)?
            };
            arguments.push(BoundaryOperand {
                operand: Operand { value },
                decision: match parameter.passing {
                    SemParamPassing::Consume => BoundaryDecision::Move,
                    SemParamPassing::Borrow => BoundaryDecision::Borrow,
                    SemParamPassing::BorrowMut => BoundaryDecision::BorrowMut,
                    SemParamPassing::ReadOnly => BoundaryDecision::Copy,
                },
            });
        }
        let live_before: std::collections::HashSet<_> = self.owned_live.keys().copied().collect();
        let result = self.finish_user_call(
            PreparedCallee::Direct {
                target: entry.id,
                site: None,
            },
            entry.signature.clone(),
            arguments,
            &[],
            &live_before,
            entry.signature.return_ty != ResolvedTy::Unit,
            None,
        )?;
        let EntryExitAction::Result { display, .. } = &adapter.action else {
            return self.finish_return_value(result.map(|value| BoundaryOperand {
                operand: Operand { value },
                decision: BoundaryDecision::Move,
            }));
        };
        let result = result.ok_or(SirLoweringError::from("entry callable produced no result"))?;
        let result_ty = entry.signature.return_ty.clone();
        let shape = self.service.require_variant_shape(&result_ty)?;
        let descriptor = self
            .service
            .variant_shapes
            .get(
                usize::try_from(shape.0)
                    .map_err(|_| SirLoweringError::from("variant shape id exceeds usize"))?,
            )
            .cloned()
            .ok_or_else(|| {
                SirLoweringError::from(format!(
                    "variant shape {} disappeared during lowering",
                    shape.0
                ))
            })?;
        let names = descriptor
            .variants
            .iter()
            .map(|variant| variant.name.as_str());
        if names.ne(["Ok", "Err"]) {
            return Err(format!(
                "entry result `{}` is not the `Ok`/`Err` Result shape",
                result_ty.user_facing()
            )
            .into());
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
                    .ok_or(SirLoweringError::from(
                        "entry Err variant carries no payload",
                    ))?
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
                SemOpKind::ConstInteger(status),
            )?;
            self.finish_return_value(Some(BoundaryOperand {
                operand: Operand { value: status },
                decision: BoundaryDecision::Move,
            }))?;
        }
        Ok(())
    }

    fn lower_property_argument(
        &mut self,
        index: usize,
        ty: &ResolvedTy,
    ) -> Result<crate::ValueId, SirLoweringError> {
        let plan = self.service.property_plans(ty)?;
        let input = self.emit_typed(
            Provenance::Synthesized,
            &ResolvedTy::I64,
            SemOpKind::ConstInteger(i128::from(
                i64::try_from(index)
                    .map_err(|_| SirLoweringError::from("property argument index exceeds i64"))?,
            )),
        )?;
        let own = OwnKind::of_ty(ty, self.service.checked_facts.rows())?;
        let raw = self.fresh_value();
        let value = self.fresh_value();
        let normal = self.new_block(vec![crate::BlockArg {
            value,
            ty: ty.clone(),
            own,
        }]);
        let unwind = self.new_block(Vec::new());
        let live = self.owned_live.clone();
        let id = crate::OpId(self.ops);
        self.ops += 1;
        self.set_terminator(SemTerminator::WireCodec {
            id,
            codec: hew_types::Codec {
                format: hew_types::CodecFormat::Cbor,
                direction: hew_types::CodecDirection::TestInput,
            },
            plan,
            decode_result: None,
            args: vec![BoundaryOperand {
                operand: Operand { value: input },
                decision: BoundaryDecision::Borrow,
            }],
            result: CallResult::Value(crate::ValueDef {
                id: raw,
                ty: ty.clone(),
                own,
            }),
            normal: Edge {
                target: normal,
                args: vec![Operand { value: raw }],
            },
            unwind: crate::CallUnwind::Cleanup(Edge {
                target: unwind,
                args: Vec::new(),
            }),
        })?;
        self.current = unwind;
        self.finish_fault_exit()?;
        self.current = normal;
        self.owned_live = live;
        if own == OwnKind::Owned {
            self.owned_live.insert(value, ty.clone());
        }
        Ok(value)
    }

    /// Call the checker-selected `Display::fmt` on the entry error.
    fn render_entry_error(
        &mut self,
        display: &hew_types::EntryDisplayTarget,
        error: crate::ValueId,
    ) -> Result<crate::ValueId, SirLoweringError> {
        let (callee, signature, decision) = match display {
            hew_types::EntryDisplayTarget::Declared {
                declaration,
                instance,
            } => {
                let target = self.service.resolve_entry_display(*declaration, instance)?;
                let [receiver] = target.signature.params.as_slice() else {
                    return Err("entry Display target must take exactly its receiver".into());
                };
                let decision = match receiver.passing {
                    SemParamPassing::Consume => BoundaryDecision::Move,
                    SemParamPassing::Borrow => BoundaryDecision::Borrow,
                    SemParamPassing::BorrowMut => BoundaryDecision::BorrowMut,
                    SemParamPassing::ReadOnly => BoundaryDecision::Copy,
                };
                (
                    PreparedCallee::Direct {
                        target: target.id,
                        site: None,
                    },
                    target.signature.clone(),
                    decision,
                )
            }
            // An erased entry error renders through the same vtable slot a
            // source-level `Display::fmt` call on that trait object uses.
            hew_types::EntryDisplayTarget::DynSlot { slot, method } => (
                PreparedCallee::Dyn {
                    receiver: BoundaryOperand {
                        operand: Operand { value: error },
                        decision: BoundaryDecision::Borrow,
                    },
                    slot: *slot,
                    method: *method,
                },
                crate::SemSignature {
                    failure_domain: hew_types::CallableFailureDomain::HewFault,
                    params: Vec::new(),
                    return_ty: hew_types::ResolvedTy::String,
                },
                BoundaryDecision::Borrow,
            ),
        };
        let live_before: std::collections::HashSet<_> = self.owned_live.keys().copied().collect();
        let arguments = if matches!(callee, PreparedCallee::Dyn { .. }) {
            Vec::new()
        } else {
            vec![BoundaryOperand {
                operand: Operand { value: error },
                decision,
            }]
        };
        let rendered = self
            .finish_user_call(callee, signature, arguments, &[], &live_before, true, None)?
            .ok_or(SirLoweringError::from(
                "entry Display target produced no string",
            ))?;
        if self.owned_live.contains_key(&error) {
            self.emit_destroy(error)?;
        }
        Ok(rendered)
    }

    fn emit_stderr_literal(&mut self, text: &str) -> Result<(), SirLoweringError> {
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
    fn emit_stderr_write(&mut self, value: crate::ValueId) -> Result<(), SirLoweringError> {
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
