//! Generator construction and suspension use ordinary callable environments,
//! value transfers, loans and lexical cleanup.

use super::{
    lower_initial_value_transfer, BlockArg, BlockId, Builder, CallResult, CallableInstance, Edge,
    HirExpr, HirExprKind, IntentKind, Operand, OwnKind, OwnedBindingUse, Provenance, ResolvedTy,
    SemOpKind, SemTerminator, ValueDef, ValueId,
};
use crate::{BoundaryDecision, BoundaryOperand, SuspendKind};

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
                .lower_stream_send(sink, value, &[], true, true)
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
        let status_ty = self.ty(&expression.ty);
        if status_ty != ResolvedTy::I32 {
            return Err("a pipe send answers with its i32 runtime status".into());
        }
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
        let status = self
            .lower_stream_send(sink.value, value, &loans, false, park)?
            .ok_or("a pipe send answers with its status")?;
        Ok(status)
    }

    /// Emit one stream send suspension. A producer turn (`receive gen fn`
    /// pump) ends on the `closed` edge through the ordinary return path and
    /// yields no status; a user send joins every resume on one `i32` status
    /// value (`0` accepted, `1` closed, `2` full) that the caller folds.
    fn lower_stream_send(
        &mut self,
        sink: ValueId,
        value: ValueId,
        loans: &[ValueId],
        producer: bool,
        park: bool,
    ) -> Result<Option<ValueId>, String> {
        self.owned_live.remove(&value);
        let live = self.owned_live.clone();
        let normal = self.new_block(Vec::new());
        let closed = self.new_block(Vec::new());
        let full = (!park).then(|| self.new_block(Vec::new()));
        let cancel = self.new_block(Vec::new());
        let unwind = self.new_block(Vec::new());
        let mut resumes = vec![edge(normal), edge(closed)];
        if let Some(full) = full {
            resumes.push(edge(full));
        }
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
            result: CallResult::Unit,
            resumes,
            cancel: edge(cancel),
            unwind: edge(unwind),
        })?;
        for cleanup in [cancel, unwind] {
            self.current = cleanup;
            self.owned_live = live.clone();
            self.end_call_loans(loans)?;
            self.finish_fault_exit()?;
        }
        if producer {
            self.current = closed;
            self.owned_live = live.clone();
            self.end_call_loans(loans)?;
            self.finish_return_value(None)?;
            self.current = normal;
            self.owned_live = live;
            self.end_call_loans(loans)?;
            return Ok(None);
        }
        let status = self.fresh_value();
        let join = self.new_block(vec![BlockArg {
            value: status,
            ty: ResolvedTy::I32,
            own: OwnKind::None,
        }]);
        for (block, code) in [(normal, 0), (closed, 1)]
            .into_iter()
            .chain(full.map(|full| (full, 2)))
        {
            self.current = block;
            self.owned_live = live.clone();
            self.end_call_loans(loans)?;
            let literal = self.emit_typed(
                Provenance::Synthesized,
                &ResolvedTy::I32,
                SemOpKind::ConstInteger(code),
            )?;
            self.set_terminator(SemTerminator::Goto(Edge {
                target: join,
                args: vec![Operand { value: literal }],
            }))?;
        }
        self.current = join;
        self.owned_live = live;
        Ok(Some(status))
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
