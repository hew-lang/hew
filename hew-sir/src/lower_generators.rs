//! Generator construction and suspension use ordinary callable environments,
//! value transfers, loans and lexical cleanup.

use super::{
    lower_initial_value_transfer, BlockArg, BlockId, Builder, CallResult, CallableInstance, Edge,
    HirExpr, HirExprKind, IntentKind, Operand, OwnKind, OwnedBindingUse, Provenance, ResolvedTy,
    SemOpKind, SemTerminator, ValueDef, ValueId,
};
use crate::{BoundaryDecision, BoundaryOperand, RuntimeVariantRole, SuspendKind, VariantShapeId};

use crate::generator_parts as parts;

fn edge(target: BlockId) -> Edge {
    Edge {
        target,
        args: Vec::new(),
    }
}

impl Builder<'_, '_> {
    pub(super) fn lower_generator(&mut self, expression: &HirExpr) -> Result<ValueId, String> {
        let HirExprKind::GenBlock {
            body,
            yield_ty,
            return_ty,
            captures,
        } = &expression.kind
        else {
            unreachable!()
        };
        let output = self.ty(return_ty);
        let yielded = self.ty(yield_ty);
        if parts(&self.ty(&expression.ty)) != Some((&yielded, &output)) {
            return Err("generator construction disagrees with its checked output types".into());
        }
        let captures = captures
            .iter()
            .map(|capture| {
                if capture.source != hew_hir::HirGenCaptureSource::Local {
                    return Err(
                        "actor generator captures require the checked actor snapshot boundary"
                            .into(),
                    );
                }
                let ty = self.ty(&capture.ty);
                let facts = self
                    .service
                    .checked_facts
                    .require(&ty)
                    .map_err(|error| error.to_string())?;
                Ok(hew_hir::HirClosureCapture {
                    binding: capture.binding,
                    name: capture.name.clone(),
                    ty,
                    acquisition: if facts.clone == hew_types::CloneKind::None {
                        hew_types::ClosureCaptureAcquisition::Move
                    } else {
                        hew_types::ClosureCaptureAcquisition::Snapshot
                    },
                    access: hew_types::ClosureCaptureAccess::Read,
                    consumption: hew_types::ClosureCaptureConsumption::Consumed,
                    is_send: facts.send == hew_types::SendFact::Known(true),
                    // Generator creation grants no shared concurrent invocation.
                    is_sync: false,
                })
            })
            .collect::<Result<Vec<_>, String>>()?;
        let mut closure = expression.clone();
        closure.ty = ResolvedTy::Closure {
            capabilities: hew_types::CallableCapabilities {
                suspends: false,
                call: hew_types::CallableCallMode::Once,
                clone: false,
            },
            params: Vec::new(),
            ret: Box::new(output.clone()),
            captures: captures.iter().map(|capture| capture.ty.clone()).collect(),
        };
        let mut producer = expression.clone();
        producer.ty = output.clone();
        producer.kind = HirExprKind::Block(body.clone());
        closure.kind = HirExprKind::Closure {
            params: Vec::new(),
            ret_ty: output,
            body: Box::new(producer),
            captures,
            escape_kind: hew_types::ClosureEscapeKind::Escapes,
        };
        let id = self
            .service
            .request_closure(self.callable.id, &closure, &self.substitution)?;
        self.service.closures[id.0 as usize].generator_yield = Some(yielded);
        let callable = self.lower_closure(&closure)?;
        self.owned_live.remove(&callable);
        self.emit(
            expression,
            SemOpKind::GeneratorMake {
                closure: id,
                callable: Operand { value: callable },
            },
        )
    }

    pub(super) fn lower_generator_yield(
        &mut self,
        expression: &HirExpr,
        value: Option<&HirExpr>,
        ty: &ResolvedTy,
    ) -> Result<(), String> {
        let ty = self.ty(ty);
        if let Some((sink, element)) = self.stream_sink.clone() {
            if ty != element {
                return Err("yield differs from its stream producer's element type".into());
            }
            let value = self.lower_yield_value(expression, value)?;
            return self
                .lower_stream_send(sink, value, &[], true, None)
                .map(|_| ());
        }
        let CallableInstance::Closure(closure) = self.callable.instance else {
            return Err("yield requires a checked generator body".into());
        };
        if self.service.closures[closure.0 as usize]
            .generator_yield
            .as_ref()
            != Some(&ty)
        {
            return Err("yield differs from its generator's checked yield type".into());
        }
        let value = self.lower_yield_value(expression, value)?;
        self.owned_live.remove(&value);
        let live = self.owned_live.clone();
        let normal = self.new_block(Vec::new());
        let cancel = self.new_block(Vec::new());
        let unwind = self.new_block(Vec::new());
        self.set_terminator(SemTerminator::Suspend {
            kind: SuspendKind::Yield,
            inputs: vec![BoundaryOperand {
                operand: Operand { value },
                decision: BoundaryDecision::Move,
            }],
            result: CallResult::Unit,
            resumes: vec![edge(normal)],
            cancel: edge(cancel),
            unwind: edge(unwind),
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

    fn lower_yield_value(
        &mut self,
        expression: &HirExpr,
        value: Option<&HirExpr>,
    ) -> Result<ValueId, String> {
        let value = if let Some(value) = value {
            let mut transferred = value.clone();
            transferred.intent = IntentKind::Consume;
            // A yield resumes the same source body. Ordinary values therefore
            // preserve their local binding; affine values use the shared move
            // rule already selected for every binding transfer.
            lower_initial_value_transfer(self, &transferred, "yield value", OwnedBindingUse::Copy)?
        } else {
            self.emit_typed(
                Provenance::Site(expression.site),
                &ResolvedTy::Unit,
                SemOpKind::ConstUnit,
            )?
        };
        Ok(value)
    }

    /// `Sink.send` / `Sink.try_send`: transfer one element into the sink and
    /// answer with the runtime status HIR folds into `Result<(), SendError>`:
    /// `0` accepted, `1` closed (the reader left or the sink finished), `2`
    /// full (`try_send` only). `send` parks on a full pipe.
    pub(super) fn lower_sink_write(
        &mut self,
        expression: &HirExpr,
        sink: &HirExpr,
        value: &HirExpr,
        park: bool,
    ) -> Result<ValueId, String> {
        let result_ty = self.ty(&expression.ty);
        let mut loans = Vec::new();
        let sink = self.lower_borrowed_read(sink, &mut loans)?;
        let loan_depth = self.argument_receiver_loans.len();
        self.argument_receiver_loans.extend(loans.iter().copied());
        let mut transferred = value.clone();
        transferred.intent = IntentKind::Consume;
        let value = lower_initial_value_transfer(
            self,
            &transferred,
            "stream element",
            OwnedBindingUse::Copy,
        )?;
        self.argument_receiver_loans.truncate(loan_depth);
        if !self.is_open() {
            return Ok(self.fresh_value());
        }
        self.lower_stream_send(sink.value, value, &loans, park, Some(&result_ty))?
            .ok_or_else(|| "a pipe send answers with its Result".into())
    }

    /// Emit one stream send suspension. A parking send resumes on accepted,
    /// closed or timed out (a socket write deadline, carrying the bytes of
    /// the item the OS took); a nonparking send resumes on accepted, closed or
    /// full. A producer turn (`receive gen fn` pump, `result: None`) ends on
    /// any refusal through the ordinary return path and yields nothing; a
    /// user send joins every resume on its `Result<(), SendError>`.
    fn lower_stream_send(
        &mut self,
        sink: ValueId,
        value: ValueId,
        loans: &[ValueId],
        park: bool,
        result: Option<&ResolvedTy>,
    ) -> Result<Option<ValueId>, String> {
        self.owned_live.remove(&value);
        let live = self.owned_live.clone();
        let normal = self.new_block(Vec::new());
        let closed = self.new_block(Vec::new());
        let committed = park.then(|| self.fresh_value());
        let refused = self.new_block(committed.map_or_else(Vec::new, |value| {
            vec![BlockArg {
                value,
                ty: ResolvedTy::I64,
                own: OwnKind::None,
            }]
        }));
        let cancel = self.new_block(Vec::new());
        let unwind = self.new_block(Vec::new());
        let raw = committed.map(|_| self.fresh_value());
        let refused_edge = Edge {
            target: refused,
            args: raw.map(|value| vec![Operand { value }]).unwrap_or_default(),
        };
        self.set_terminator(SemTerminator::Suspend {
            kind: SuspendKind::StreamSend { park },
            inputs: vec![
                BoundaryOperand {
                    operand: Operand { value: sink },
                    decision: BoundaryDecision::Borrow,
                },
                BoundaryOperand {
                    operand: Operand { value },
                    decision: BoundaryDecision::Move,
                },
            ],
            result: raw.map_or(CallResult::Unit, |id| {
                CallResult::Value(ValueDef {
                    id,
                    ty: ResolvedTy::I64,
                    own: OwnKind::None,
                })
            }),
            resumes: vec![edge(normal), edge(closed), refused_edge],
            cancel: edge(cancel),
            unwind: edge(unwind),
        })?;
        for cleanup in [cancel, unwind] {
            self.current = cleanup;
            self.owned_live = live.clone();
            self.end_call_loans(loans)?;
            self.finish_fault_exit()?;
        }
        let Some(result_ty) = result else {
            for refusal in [closed, refused] {
                self.current = refusal;
                self.owned_live = live.clone();
                self.end_call_loans(loans)?;
                self.finish_return_value(None)?;
            }
            self.current = normal;
            self.owned_live = live;
            self.end_call_loans(loans)?;
            return Ok(None);
        };
        let shapes = self.send_result_shapes(result_ty)?;
        let outcome = self.fresh_value();
        let join = self.new_block(vec![BlockArg {
            value: outcome,
            ty: result_ty.clone(),
            own: OwnKind::of_ty(result_ty, self.service.checked_facts.rows())?,
        }]);
        for (block, role) in [
            (normal, None),
            (closed, Some(RuntimeVariantRole::SendErrorClosed)),
            (
                refused,
                Some(if park {
                    RuntimeVariantRole::SendErrorWriteTimedOut
                } else {
                    RuntimeVariantRole::SendErrorFull
                }),
            ),
        ] {
            self.current = block;
            self.owned_live = live.clone();
            self.end_call_loans(loans)?;
            let fields = if role == Some(RuntimeVariantRole::SendErrorWriteTimedOut) {
                committed
                    .map(|value| vec![Operand { value }])
                    .unwrap_or_default()
            } else {
                Vec::new()
            };
            let value = self.send_result_value(&shapes, result_ty, role, fields)?;
            self.set_terminator(SemTerminator::Goto(Edge {
                target: join,
                args: vec![Operand { value }],
            }))?;
        }
        self.current = join;
        self.owned_live = live;
        Ok(Some(outcome))
    }

    /// The `Result<(), SendError>` and `SendError` shapes a send builds.
    fn send_result_shapes(
        &mut self,
        result_ty: &ResolvedTy,
    ) -> Result<(VariantShapeId, VariantShapeId, ResolvedTy), String> {
        let ResolvedTy::Named { args, .. } = result_ty else {
            return Err("a pipe send answers with Result<(), SendError>".into());
        };
        let [ResolvedTy::Unit, error_ty] = args.as_slice() else {
            return Err("a pipe send answers with Result<(), SendError>".into());
        };
        let error_ty = error_ty.clone();
        let result = self.service.require_variant_shape(result_ty)?;
        let error = self.service.require_variant_shape(&error_ty)?;
        Ok((result, error, error_ty))
    }

    /// `Ok(())`, or `Err` of the `SendError` variant `role` with `fields`.
    fn send_result_value(
        &mut self,
        (result, error, error_ty): &(VariantShapeId, VariantShapeId, ResolvedTy),
        result_ty: &ResolvedTy,
        role: Option<RuntimeVariantRole>,
        fields: Vec<Operand>,
    ) -> Result<ValueId, String> {
        let tag = |this: &Self, shape: VariantShapeId, role| {
            this.service.variant_shapes[shape.0 as usize]
                .runtime_tag(role)
                .ok_or_else(|| format!("send result shape lacks its {role:?} variant"))
        };
        let (variant, payload) = match role {
            None => {
                let unit = self.emit_typed(
                    Provenance::Synthesized,
                    &ResolvedTy::Unit,
                    SemOpKind::ConstUnit,
                )?;
                (tag(self, *result, RuntimeVariantRole::ResultOk)?, unit)
            }
            Some(role) => {
                let reason = self.emit_typed(
                    Provenance::Synthesized,
                    error_ty,
                    SemOpKind::VariantMake {
                        shape: *error,
                        variant: tag(self, *error, role)?,
                        fields,
                    },
                )?;
                (tag(self, *result, RuntimeVariantRole::ResultErr)?, reason)
            }
        };
        self.emit_typed(
            Provenance::Synthesized,
            result_ty,
            SemOpKind::VariantMake {
                shape: *result,
                variant,
                fields: vec![Operand { value: payload }],
            },
        )
    }

    /// A stream consumer takes the next element. `park` is `recv()`: an
    /// exhausted stream with a live producer parks. `try_recv()` (`park:
    /// false`) resumes with `None` instead.
    pub(super) fn lower_stream_next(
        &mut self,
        expression: &HirExpr,
        receiver: &HirExpr,
        park: bool,
    ) -> Result<ValueId, String> {
        let mut loans = Vec::new();
        let stream = self.lower_borrowed_read(receiver, &mut loans)?;
        let output = self.ty(&expression.ty);
        self.lower_stream_next_prepared(stream, output, park, &loans)
    }

    /// Receive from a stream already evaluated and borrowed by a selection.
    /// A `select` winner knows the `Option<T>` from the stream's own element,
    /// not from a call expression.
    pub(super) fn lower_stream_next_prepared(
        &mut self,
        stream: Operand,
        output: ResolvedTy,
        park: bool,
        loans: &[ValueId],
    ) -> Result<ValueId, String> {
        self.service.require_type_facts(&output)?;
        self.service.require_variant_shape(&output)?;
        let own = OwnKind::of_ty(&output, self.service.checked_facts.rows())?;
        let raw = self.fresh_value();
        let value = self.fresh_value();
        let resumed = self.new_block(vec![BlockArg {
            value,
            ty: output.clone(),
            own,
        }]);
        let cancel = self.new_block(Vec::new());
        let unwind = self.new_block(Vec::new());
        let live = self.owned_live.clone();
        self.set_terminator(SemTerminator::Suspend {
            kind: SuspendKind::StreamNext { park },
            inputs: vec![BoundaryOperand {
                operand: stream,
                decision: BoundaryDecision::BorrowMut,
            }],
            result: CallResult::Value(ValueDef {
                id: raw,
                ty: output.clone(),
                own,
            }),
            resumes: vec![Edge {
                target: resumed,
                args: vec![Operand { value: raw }],
            }],
            cancel: edge(cancel),
            unwind: edge(unwind),
        })?;
        for cleanup in [cancel, unwind] {
            self.current = cleanup;
            self.owned_live = live.clone();
            self.end_call_loans(loans)?;
            self.finish_fault_exit()?;
        }
        self.current = resumed;
        self.owned_live = live;
        self.end_call_loans(loans)?;
        if own == OwnKind::Owned {
            self.owned_live.insert(value, output);
        }
        Ok(value)
    }

    pub(super) fn lower_generator_next(
        &mut self,
        expression: &HirExpr,
        receiver: &HirExpr,
    ) -> Result<ValueId, String> {
        let mut loans = Vec::new();
        let generator = self.lower_borrowed_read(receiver, &mut loans)?;
        let output = self.ty(&expression.ty);
        self.service.require_type_facts(&output)?;
        self.service.require_variant_shape(&output)?;
        let own = OwnKind::of_ty(&output, self.service.checked_facts.rows())?;
        let raw = self.fresh_value();
        let value = self.fresh_value();
        let resumed = self.new_block(vec![BlockArg {
            value,
            ty: output.clone(),
            own,
        }]);
        let cancel = self.new_block(Vec::new());
        let unwind = self.new_block(Vec::new());
        let live = self.owned_live.clone();
        self.set_terminator(SemTerminator::Suspend {
            kind: SuspendKind::GeneratorNext,
            inputs: vec![BoundaryOperand {
                operand: generator,
                decision: BoundaryDecision::BorrowMut,
            }],
            result: CallResult::Value(ValueDef {
                id: raw,
                ty: output.clone(),
                own,
            }),
            resumes: vec![Edge {
                target: resumed,
                args: vec![Operand { value: raw }],
            }],
            cancel: edge(cancel),
            unwind: edge(unwind),
        })?;
        for cleanup in [cancel, unwind] {
            self.current = cleanup;
            self.owned_live = live.clone();
            self.end_call_loans(&loans)?;
            self.finish_fault_exit()?;
        }
        self.current = resumed;
        self.owned_live = live;
        self.end_call_loans(&loans)?;
        if own == OwnKind::Owned {
            self.owned_live.insert(value, output);
        }
        Ok(value)
    }
}
