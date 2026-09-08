//! Generator construction and suspension use ordinary callable environments,
//! value transfers, loans and lexical cleanup.

use super::{
    lower_initial_value_transfer, BlockArg, BlockId, Builder, CallResult, CallableInstance, Edge,
    HirExpr, HirExprKind, IntentKind, Operand, OwnKind, OwnedBindingUse, PlaceId, Provenance,
    ResolvedTy, SemOpKind, SemTerminator, ValueDef, ValueId,
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
            return self.lower_stream_send(sink, value);
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

    /// A stream producer parks on the consumer's capacity. A consumer that
    /// closed its half ends this turn normally: the sink and every local
    /// release through the ordinary return path.
    fn lower_stream_send(&mut self, sink: ValueId, value: ValueId) -> Result<(), String> {
        self.owned_live.remove(&value);
        let live = self.owned_live.clone();
        let normal = self.new_block(Vec::new());
        let closed = self.new_block(Vec::new());
        let cancel = self.new_block(Vec::new());
        let unwind = self.new_block(Vec::new());
        self.set_terminator(SemTerminator::Suspend {
            kind: SuspendKind::StreamSend,
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
            resumes: vec![edge(normal), edge(closed)],
            cancel: edge(cancel),
            unwind: edge(unwind),
        })?;
        for cleanup in [cancel, unwind] {
            self.current = cleanup;
            self.owned_live = live.clone();
            self.finish_fault_exit()?;
        }
        self.current = closed;
        self.owned_live = live.clone();
        self.finish_return_value(None)?;
        self.current = normal;
        self.owned_live = live;
        Ok(())
    }

    pub(super) fn lower_stream_next(
        &mut self,
        expression: &HirExpr,
        receiver: &HirExpr,
    ) -> Result<ValueId, String> {
        let mut loans = Vec::new();
        let stream = self.lower_borrowed_read(receiver, &mut loans)?;
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
            kind: SuspendKind::StreamNext,
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

    /// A channel consumer takes the next element. `recv()` parks on an empty
    /// queue with live senders; `try_recv()` (`park: false`) resumes with
    /// `None` instead. The element arrives decoded into the resume value
    /// exactly as a stream item does; the message type comes from the call's
    /// `Option<T>` result, never from the symbol.
    pub(super) fn lower_channel_recv(
        &mut self,
        expression: &HirExpr,
        receiver: &HirExpr,
        park: bool,
    ) -> Result<ValueId, String> {
        let mut loans = Vec::new();
        let channel = self.lower_borrowed_read(receiver, &mut loans)?;
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
            kind: SuspendKind::ChannelRecv { park },
            inputs: vec![BoundaryOperand {
                operand: channel,
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

    /// A channel producer parks on a bounded channel's capacity. The queue
    /// takes an independent deep copy, so the producer's element is borrowed
    /// and stays its own; a closed channel resumes normally.
    pub(super) fn lower_channel_send(
        &mut self,
        sender: &HirExpr,
        value: &HirExpr,
    ) -> Result<(), String> {
        let mut loans = Vec::new();
        let channel = self.lower_borrowed_read(sender, &mut loans)?;
        let element = self.lower_borrowed_read(value, &mut loans)?;
        let live = self.owned_live.clone();
        let normal = self.new_block(Vec::new());
        let cancel = self.new_block(Vec::new());
        let unwind = self.new_block(Vec::new());
        self.set_terminator(SemTerminator::Suspend {
            kind: SuspendKind::ChannelSend,
            inputs: vec![
                BoundaryOperand {
                    operand: channel,
                    decision: BoundaryDecision::BorrowMut,
                },
                BoundaryOperand {
                    operand: element,
                    decision: BoundaryDecision::Borrow,
                },
            ],
            result: CallResult::Unit,
            resumes: vec![edge(normal)],
            cancel: edge(cancel),
            unwind: edge(unwind),
        })?;
        for cleanup in [cancel, unwind] {
            self.current = cleanup;
            self.owned_live = live.clone();
            self.end_call_loans(&loans)?;
            self.finish_fault_exit()?;
        }
        self.current = normal;
        self.owned_live = live;
        self.end_call_loans(&loans)
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

    pub(super) fn value_needs_close(&self, ty: &ResolvedTy) -> bool {
        value_needs_close(self.service, ty)
    }

    /// Cleanup combines any producer fault with the current fault. The enclosing
    /// lexical cleanup dispatch decides whether source execution can continue.
    pub(super) fn close_value(
        &mut self,
        place: Option<PlaceId>,
        value: Option<ValueId>,
    ) -> Result<(), String> {
        self.close_selected_value(place, value, None)
    }

    pub(super) fn close_selected_value(
        &mut self,
        place: Option<PlaceId>,
        value: Option<ValueId>,
        index: Option<ValueId>,
    ) -> Result<(), String> {
        let next = self.new_block(Vec::new());
        self.set_terminator(SemTerminator::Suspend {
            kind: SuspendKind::ValueClose {
                place,
                selection: if index.is_some() {
                    crate::ValueCloseSelection::VectorElement
                } else {
                    crate::ValueCloseSelection::Whole
                },
            },
            inputs: value
                .into_iter()
                .map(|value| BoundaryOperand {
                    operand: Operand { value },
                    decision: BoundaryDecision::Borrow,
                })
                .chain(index.map(|value| BoundaryOperand {
                    operand: Operand { value },
                    decision: BoundaryDecision::Copy,
                }))
                .collect(),
            result: CallResult::Unit,
            resumes: vec![edge(next)],
            cancel: edge(next),
            unwind: edge(next),
        })?;
        self.current = next;
        self.cleanup_may_fail = true;
        Ok(())
    }
}

/// Generators and call-once callables drain cooperatively before their storage
/// is released; a synchronous destructor cannot finish them.
pub(super) fn value_needs_close(service: &super::InstanceService<'_>, ty: &ResolvedTy) -> bool {
    fn visit(
        ty: &ResolvedTy,
        service: &super::InstanceService<'_>,
        seen: &mut Vec<ResolvedTy>,
    ) -> bool {
        if seen.contains(ty) {
            return false;
        }
        seen.push(ty.clone());
        let result = match ty {
            ResolvedTy::Function { capabilities, .. } => !capabilities.clone,
            ResolvedTy::Closure { captures, .. } | ResolvedTy::Tuple(captures) => {
                captures.iter().any(|ty| visit(ty, service, seen))
            }
            ResolvedTy::Named {
                builtin: Some(hew_types::BuiltinType::Generator),
                ..
            } => true,
            ResolvedTy::Named {
                builtin:
                    Some(
                        hew_types::BuiltinType::Vec
                        | hew_types::BuiltinType::HashMap
                        | hew_types::BuiltinType::HashSet
                        | hew_types::BuiltinType::Option
                        | hew_types::BuiltinType::Result,
                    ),
                args,
                ..
            } => args.iter().any(|ty| visit(ty, service, seen)),
            _ => {
                service
                    .aggregate_shapes
                    .iter()
                    .find(|shape| shape.aggregate_ty == *ty)
                    .is_some_and(|shape| {
                        shape
                            .fields
                            .iter()
                            .any(|field| visit(&field.ty, service, seen))
                    })
                    || service
                        .variant_shapes
                        .iter()
                        .find(|shape| shape.enum_ty == *ty)
                        .is_some_and(|shape| {
                            shape.variants.iter().any(|variant| {
                                variant
                                    .fields
                                    .iter()
                                    .any(|field| visit(&field.ty, service, seen))
                            })
                        })
            }
        };
        seen.pop();
        result
    }
    visit(ty, service, &mut Vec::new())
}
