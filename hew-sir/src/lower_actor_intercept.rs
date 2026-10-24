//! Compile each interceptor chain into the actor's ordinary callable graph.

use crate::SirLoweringError;

use super::{
    ActorWeave, BindingTarget, BlockArg, Builder, CallResult, CallUnwind, CallableId,
    CallableInstance, Edge, HirExpr, InstanceService, InterceptorNext, OpId, Operand, OwnKind,
    PreparedCallee, Provenance, ResolvedTy, SemOpKind, SemParamPassing, SemTerminator,
    TypeSubstitution, ValueDef, ValueId,
};
use hew_types::actor_delivery::InterceptorOperation;

impl InstanceService<'_> {
    #[expect(
        clippy::too_many_lines,
        reason = "builds and publishes one actor interceptor chain with its checked callable facts"
    )]
    pub(super) fn weave_actor_interceptor(
        &mut self,
        actor: crate::ActorId,
        source: &hew_hir::HirActorDecl,
        substitution: &TypeSubstitution,
    ) -> Result<(), SirLoweringError> {
        let originals = self.actors[actor.0 as usize].handlers.clone();
        let mut next = InterceptorNext::Handlers(originals.clone());
        let mut outermost = None;
        if let Some(declaration) = source.protocol_descriptor.interceptor {
            let interceptor = self.actors[actor.0 as usize]
                .methods
                .iter()
                .copied()
                .find(|callable| {
                    self.callable(*callable)
                        .is_some_and(|body| body.declaration == declaration)
                })
                .ok_or(SirLoweringError::from(
                    "actor interceptor has no registered body",
                ))?;
            self.actor_sources
                .get_mut(&interceptor)
                .ok_or(SirLoweringError::from("interceptor source is absent"))?
                .weave = Some(ActorWeave::Interceptor { next });
            next = InterceptorNext::Layer(interceptor);
            outermost = Some(interceptor);
        }
        let chain = self.actors[actor.0 as usize].interceptor_chain.clone();
        for (index, supervisor_ty) in chain.iter().enumerate().rev() {
            let supervisor = super::supervisor::declaration(self.module, supervisor_ty)
                .ok_or(SirLoweringError::from(
                    "interceptor chain has no supervisor declaration",
                ))?
                .clone();
            let method = supervisor
                .interceptor
                .as_ref()
                .ok_or(SirLoweringError::from(
                    "interceptor chain names a supervisor without an interceptor",
                ))?;
            let instance = crate::actor::local_actor_instance(supervisor_ty).ok_or(
                SirLoweringError::from("interceptor supervisor lacks its type arguments"),
            )?;
            let mut arguments = instance.args;
            arguments.push(self.actors[actor.0 as usize].handle_ty.clone());
            if arguments.len() != method.type_params.len() {
                return Err(
                    "supervisor interceptor type arguments differ from its declaration".into(),
                );
            }
            let specialized = TypeSubstitution {
                params: method.type_params.clone(),
                args: arguments,
            };
            let interceptor = self.register_actor_body(
                actor,
                source,
                &specialized,
                method.declaration,
                &[],
                &method.params,
                specialized.apply(&method.return_ty),
                &method.body,
                &format!("supervisor_intercept_{index}"),
                true,
                None,
            )?;
            self.table.callables[interceptor.0 as usize].instance =
                CallableInstance::ActorSupervisorIntercept {
                    layer: u32::try_from(index)
                        .map_err(|_| SirLoweringError::from("interceptor layer exceeds u32"))?,
                };
            self.actors[actor.0 as usize].methods.push(interceptor);
            self.actor_sources
                .get_mut(&interceptor)
                .ok_or(SirLoweringError::from(
                    "supervisor interceptor source is absent",
                ))?
                .weave = Some(ActorWeave::Interceptor { next });
            next = InterceptorNext::Layer(interceptor);
            outermost = Some(interceptor);
        }
        let Some(interceptor) = outermost else {
            return Ok(());
        };
        for (index, (handler, original)) in
            source.receive_handlers.iter().zip(originals).enumerate()
        {
            let sink = original
                .stream
                .as_ref()
                .and_then(|_| original.params.last());
            let wrapper = self.register_actor_body(
                actor,
                source,
                substitution,
                handler.declaration,
                &handler.state_bindings,
                &handler.params,
                original.return_ty.clone(),
                &handler.body,
                &format!("intercept_{}", handler.name),
                true,
                sink,
            )?;
            self.actor_sources
                .get_mut(&wrapper)
                .ok_or(SirLoweringError::from(
                    "interceptor wrapper source is absent",
                ))?
                .weave = Some(ActorWeave::Entry {
                handler: index,
                interceptor,
            });
            self.table.callables[wrapper.0 as usize].instance =
                CallableInstance::ActorInterceptEntry {
                    handler: u32::try_from(index)
                        .map_err(|_| SirLoweringError::from("handler index exceeds u32"))?,
                };
            self.actors[actor.0 as usize]
                .methods
                .push(original.callable);
            self.actors[actor.0 as usize].handlers[index].callable = wrapper;
        }
        Ok(())
    }
}

impl Builder<'_, '_> {
    fn intercept_type(&self, known: hew_types::KnownDecl, actor: crate::ActorId) -> ResolvedTy {
        ResolvedTy::Named {
            head: known.head(),
            args: vec![self.service.actors[actor.0 as usize].handle_ty.clone()],
            is_opaque: false,
        }
    }

    fn intercept_fields(
        &self,
        ty: &ResolvedTy,
    ) -> Result<Vec<(String, ResolvedTy)>, SirLoweringError> {
        hew_types::actor_protocol::interceptor_fields(&self.service.module.defs, ty).ok_or_else(
            || {
                SirLoweringError::from(format!(
                    "interceptor token `{}` has no concrete actor",
                    ty.user_facing()
                ))
            },
        )
    }

    fn intercept_record(
        &mut self,
        ty: &ResolvedTy,
        fields: Vec<ValueId>,
    ) -> Result<ValueId, SirLoweringError> {
        let shape = self.service.require_aggregate_shape(ty)?;
        let value = self.emit_typed(
            Provenance::Synthesized,
            ty,
            SemOpKind::AggregateMake {
                shape,
                fields: fields
                    .iter()
                    .map(|value| Operand { value: *value })
                    .collect(),
            },
        )?;
        for field in fields {
            self.owned_live.remove(&field);
        }
        Ok(value)
    }

    fn intercept_variant(
        &mut self,
        ty: &ResolvedTy,
        variant: u32,
        fields: Vec<ValueId>,
    ) -> Result<ValueId, SirLoweringError> {
        let shape = self.service.require_variant_shape(ty)?;
        let value = self.emit_typed(
            Provenance::Synthesized,
            ty,
            SemOpKind::VariantMake {
                shape,
                variant,
                fields: fields
                    .iter()
                    .map(|value| Operand { value: *value })
                    .collect(),
            },
        )?;
        for field in fields {
            self.owned_live.remove(&field);
        }
        Ok(value)
    }

    fn intercept_bool(&mut self, value: bool) -> Result<ValueId, SirLoweringError> {
        self.emit_typed(
            Provenance::Synthesized,
            &ResolvedTy::Bool,
            SemOpKind::ConstBool(value),
        )
    }

    fn intercept_drop(&mut self, value: ValueId) -> Result<(), SirLoweringError> {
        if self.owned_live.contains_key(&value) {
            self.emit_destroy(value)?;
        }
        Ok(())
    }

    fn intercept_parts(
        &mut self,
        value: ValueId,
        ty: &ResolvedTy,
    ) -> Result<Vec<ValueId>, SirLoweringError> {
        let shape = self.service.require_aggregate_shape(ty)?;
        Ok(self
            .emit_destructure_value(value, ty, shape, Provenance::Synthesized)?
            .into_iter()
            .map(|field| field.id)
            .collect())
    }

    fn intercept_invoke(
        &mut self,
        callee: CallableId,
        values: Vec<ValueId>,
    ) -> Result<Option<ValueId>, SirLoweringError> {
        let callable = self
            .service
            .callable(callee)
            .cloned()
            .ok_or(SirLoweringError::from("interceptor next body is absent"))?;
        let state = self
            .params
            .first()
            .ok_or(SirLoweringError::from(
                "interceptor has no actor state seat",
            ))?
            .value;
        let mut values = vec![state].into_iter().chain(values);
        let arguments = callable
            .signature
            .params
            .iter()
            .map(|parameter| {
                let value = values.next().ok_or(SirLoweringError::from(
                    "interceptor call has too few arguments",
                ))?;
                Ok(crate::BoundaryOperand {
                    operand: Operand { value },
                    decision: match parameter.passing {
                        SemParamPassing::BorrowMut => crate::BoundaryDecision::BorrowMut,
                        SemParamPassing::Borrow => crate::BoundaryDecision::Borrow,
                        SemParamPassing::ReadOnly => crate::BoundaryDecision::Copy,
                        SemParamPassing::Consume => crate::BoundaryDecision::Move,
                    },
                })
            })
            .collect::<Result<Vec<_>, SirLoweringError>>()?;
        if values.next().is_some() {
            return Err("interceptor call has too many arguments".into());
        }
        let live = self.owned_live.keys().copied().collect();
        self.finish_user_call(
            PreparedCallee::Direct {
                target: callee,
                site: None,
            },
            callable.signature.clone(),
            arguments,
            &[],
            &live,
            callable.signature.return_ty != ResolvedTy::Unit,
            None,
        )
    }

    /// A synthesized runtime query with an already-checked, non-failing contract.
    fn intercept_runtime(
        &mut self,
        family: hew_types::RuntimeCallFamily,
        values: Vec<ValueId>,
        ty: &ResolvedTy,
    ) -> Result<Option<ValueId>, SirLoweringError> {
        let contract = family.semantic_contract().ok_or(SirLoweringError::from(
            "interceptor runtime contract is absent",
        ))?;
        if !contract.failures.is_empty() {
            return Err("interceptor runtime query unexpectedly fails".into());
        }
        let types = values
            .iter()
            .map(|value| {
                self.value_ty(*value).ok_or(SirLoweringError::from(
                    "interceptor runtime value lacks its type",
                ))
            })
            .collect::<Result<Vec<_>, SirLoweringError>>()?;
        contract.resolve_types(&self.service.module.defs, &types, ty)?;
        self.service.require_type_facts(ty)?;
        let arguments = values
            .into_iter()
            .zip(contract.arguments)
            .map(|(value, argument)| crate::BoundaryOperand {
                operand: Operand { value },
                decision: match argument.effect {
                    hew_types::RuntimeArgumentEffect::Borrow => crate::BoundaryDecision::Borrow,
                    hew_types::RuntimeArgumentEffect::Move => crate::BoundaryDecision::Move,
                    hew_types::RuntimeArgumentEffect::Copy
                    | hew_types::RuntimeArgumentEffect::Value => crate::BoundaryDecision::Copy,
                },
            })
            .collect();
        let (result, normal, output) = if *ty == ResolvedTy::Unit {
            (
                CallResult::Unit,
                Edge {
                    target: self.new_block(Vec::new()),
                    args: Vec::new(),
                },
                None,
            )
        } else {
            let own = OwnKind::of_ty(ty, self.service.checked_facts.rows())?;
            let raw = self.fresh_value();
            let value = self.fresh_value();
            let block = self.new_block(vec![BlockArg {
                value,
                ty: ty.clone(),
                own,
            }]);
            (
                CallResult::Value(ValueDef {
                    id: raw,
                    ty: ty.clone(),
                    own,
                }),
                Edge {
                    target: block,
                    args: vec![Operand { value: raw }],
                },
                Some(value),
            )
        };
        let id = OpId(self.ops);
        self.ops += 1;
        let target = normal.target;
        self.set_terminator(SemTerminator::RtCall {
            id,
            family,
            args: arguments,
            result,
            normal,
            unwind: CallUnwind::NotApplicable,
        })?;
        self.current = target;
        if let Some(value) = output {
            if self.value_own_kind(value) == Some(OwnKind::Owned) {
                self.owned_live.insert(value, ty.clone());
            }
        }
        Ok(output)
    }

    pub(super) fn lower_intercept_entry(
        &mut self,
        actor: crate::ActorId,
        handler: usize,
        interceptor: CallableId,
    ) -> Result<(), SirLoweringError> {
        let descriptor = self.service.actors[actor.0 as usize].handlers[handler].clone();
        let call_ty = self.intercept_type(hew_types::KnownDecl::Call, actor);
        let fields = self.intercept_fields(&call_ty)?;
        let mut arguments = Vec::new();
        for parameter in self.function.params.clone() {
            let ty = self.ty(&parameter.ty);
            let value = match self.binding_target(parameter.id)? {
                BindingTarget::Value(value) => value,
                BindingTarget::Place(place) => self.emit_typed(
                    Provenance::Synthesized,
                    &ty,
                    if OwnKind::of_ty(&ty, self.service.checked_facts.rows())? == OwnKind::Owned {
                        SemOpKind::LoadTake { place }
                    } else {
                        SemOpKind::LoadCopy { place }
                    },
                )?,
            };
            arguments.push(value);
        }
        let request = self.intercept_variant(
            &fields[0].1,
            u32::try_from(handler)
                .map_err(|_| SirLoweringError::from("handler index exceeds u32"))?,
            arguments,
        )?;
        self.service.require_variant_shape(&fields[1].1)?;
        let caller = self
            .intercept_runtime(
                hew_types::RuntimeCallFamily::ActorCaller,
                Vec::new(),
                &fields[1].1,
            )?
            .ok_or(SirLoweringError::from("caller query returned no value"))?;
        let literal = self.service.intern_string(&descriptor.name);
        let name = self.emit_typed(
            Provenance::Synthesized,
            &ResolvedTy::String,
            SemOpKind::ConstStr(literal),
        )?;
        let message = self.emit_typed(
            Provenance::Synthesized,
            &ResolvedTy::U32,
            SemOpKind::ConstInteger(i128::from(descriptor.message_id)),
        )?;
        let stream = self.intercept_variant(
            &fields[4].1,
            u32::try_from(handler)
                .map_err(|_| SirLoweringError::from("handler index exceeds u32"))?,
            self.stream_sink.iter().map(|(sink, _)| *sink).collect(),
        )?;
        let call = self.intercept_record(&call_ty, vec![request, caller, name, message, stream])?;
        let outcome = self
            .intercept_invoke(interceptor, vec![call])?
            .ok_or(SirLoweringError::from("interceptor returned no outcome"))?;
        let outcome_ty = self.intercept_type(hew_types::KnownDecl::Outcome, actor);
        let response_ty = self.intercept_fields(&outcome_ty)?[0].1.clone();
        let parts = self.intercept_parts(outcome, &outcome_ty)?;
        let shape = self.service.require_variant_shape(&response_ty)?;
        let shape_info = self.service.variant_shapes[shape.0 as usize].clone();
        let branches = self.emit_variant_switch(shape, &shape_info, parts[0])?;
        let state = self.control_state();
        for branch in branches {
            self.restore_control_state(&state);
            self.current = branch.block;
            self.owned_live = branch.owned_live;
            if branch.fields.is_empty() {
                self.finish_checked_fault(crate::TrapKind::ActorRefused)?;
                continue;
            }
            let response = &branch.fields[0];
            let shape = self.service.require_variant_shape(&response.ty)?;
            let shape_info = self.service.variant_shapes[shape.0 as usize].clone();
            let fields = self.emit_variant_destructure(
                shape,
                u32::try_from(handler)
                    .map_err(|_| SirLoweringError::from("handler index exceeds u32"))?,
                &shape_info,
                response.value,
            )?;
            self.finish_return_value(fields.first().map(|field| crate::BoundaryOperand {
                operand: Operand { value: field.value },
                decision: crate::BoundaryDecision::Move,
            }))?;
        }
        Ok(())
    }

    pub(super) fn lower_interceptor_operation(
        &mut self,
        expression: &HirExpr,
        receiver: &HirExpr,
        args: &[HirExpr],
        operation: InterceptorOperation,
    ) -> Result<ValueId, SirLoweringError> {
        use InterceptorOperation as Op;
        let ty = self.ty(&receiver.ty);
        if !operation.consumes() {
            let index = match operation {
                Op::Caller | Op::Failed => 1,
                Op::HandlerName | Op::Refused => 2,
                _ => unreachable!(),
            };
            let mut loans = Vec::new();
            let aggregate = self.lower_borrowed_read(receiver, &mut loans)?;
            let shape = self.service.require_aggregate_shape(&ty)?;
            let value = self.emit(
                expression,
                SemOpKind::AggregateProjectCopy {
                    shape,
                    aggregate,
                    field: index,
                },
            )?;
            self.end_call_loans(&loans)?;
            return Ok(value);
        }
        let call = self.lower_consuming_value(receiver)?;
        if operation == Op::Proceed {
            let source = self
                .service
                .actor_sources
                .get(&self.callable.id)
                .ok_or(SirLoweringError::from("proceed outside an interceptor"))?;
            let Some(ActorWeave::Interceptor { next }) = source.weave.clone() else {
                return Err("proceed has no next interceptor layer".into());
            };
            return match next {
                InterceptorNext::Layer(next) => self
                    .intercept_invoke(next, vec![call])?
                    .ok_or(SirLoweringError::from("next layer returned no outcome")),
                InterceptorNext::Handlers(handlers) => {
                    self.lower_intercept_handlers(call, &ty, &self.ty(&expression.ty), &handlers)
                }
            };
        }
        let parts = self.intercept_parts(call, &ty)?;
        let mut loans = Vec::new();
        let reason = self.lower_borrowed_read(&args[0], &mut loans)?.value;
        self.intercept_runtime(
            hew_types::RuntimeCallFamily::ActorRefusalLog,
            vec![parts[3], parts[2], reason],
            &ResolvedTy::Unit,
        )?;
        self.end_call_loans(&loans)?;
        for part in &parts[..4] {
            self.intercept_drop(*part)?;
        }
        self.intercept_outcome(&self.ty(&expression.ty), None, None, true, parts[4])
    }

    fn intercept_outcome(
        &mut self,
        ty: &ResolvedTy,
        response: Option<ValueId>,
        failed: Option<ValueId>,
        refused: bool,
        stream: ValueId,
    ) -> Result<ValueId, SirLoweringError> {
        let response_ty = self.intercept_fields(ty)?[0].1.clone();
        let shape = self.service.require_variant_shape(&response_ty)?;
        let role = if response.is_some() {
            crate::RuntimeVariantRole::OptionSome
        } else {
            crate::RuntimeVariantRole::OptionNone
        };
        let variant = self.service.variant_shapes[shape.0 as usize]
            .runtime_tags
            .iter()
            .find_map(|(candidate, tag)| (*candidate == role).then_some(*tag))
            .ok_or(SirLoweringError::from(
                "outcome response lacks its Option role",
            ))?;
        let response =
            self.intercept_variant(&response_ty, variant, response.into_iter().collect())?;
        let failed = match failed {
            Some(value) => value,
            None => self.intercept_bool(false)?,
        };
        let refused = self.intercept_bool(refused)?;
        self.intercept_record(ty, vec![response, failed, refused, stream])
    }

    fn lower_intercept_handlers(
        &mut self,
        call: ValueId,
        call_ty: &ResolvedTy,
        outcome_ty: &ResolvedTy,
        handlers: &[crate::SemActorHandler],
    ) -> Result<ValueId, SirLoweringError> {
        let request_ty = self.intercept_fields(call_ty)?[0].1.clone();
        let response_option = self.intercept_fields(outcome_ty)?[0].1.clone();
        let ResolvedTy::Named { args, .. } = response_option else {
            return Err("outcome response has no Option payload".into());
        };
        let response_ty = &args[0];
        let parts = self.intercept_parts(call, call_ty)?;
        for part in &parts[1..4] {
            self.intercept_drop(*part)?;
        }
        let shape = self.service.require_variant_shape(&request_ty)?;
        let descriptor = self.service.variant_shapes[shape.0 as usize].clone();
        let branches = self.emit_variant_switch(shape, &descriptor, parts[0])?;
        let inherited = self.control_state();
        let result = self.fresh_value();
        let join = self.new_block(vec![BlockArg {
            value: result,
            ty: outcome_ty.clone(),
            own: OwnKind::Owned,
        }]);
        for branch in branches {
            self.restore_control_state(&inherited);
            self.current = branch.block;
            self.owned_live = branch.owned_live;
            let handler = handlers
                .get(branch.variant as usize)
                .ok_or(SirLoweringError::from(
                    "request variant has no original handler",
                ))?;
            let stream_ty = self.intercept_fields(call_ty)?[4].1.clone();
            let stream_shape = self.service.require_variant_shape(&stream_ty)?;
            let stream_descriptor = self.service.variant_shapes[stream_shape.0 as usize].clone();
            let sinks = self.emit_variant_destructure(
                stream_shape,
                branch.variant,
                &stream_descriptor,
                parts[4],
            )?;
            let mut arguments: Vec<_> = branch.fields.iter().map(|field| field.value).collect();
            let mut retained = Vec::new();
            for sink in sinks {
                retained.push(
                    self.intercept_runtime(
                        hew_types::RuntimeCallFamily::SinkClone,
                        vec![sink.value],
                        &sink.ty,
                    )?
                    .ok_or(SirLoweringError::from(
                        "stream sink clone returned no value",
                    ))?,
                );
                arguments.push(sink.value);
            }
            // Keep the producer alive until every outer interceptor returns.
            // A refusal or fault then reaches the reader through normal cleanup.
            let stream = self.intercept_variant(&stream_ty, branch.variant, retained)?;
            let reply = self.intercept_invoke(handler.callable, arguments)?;
            let failed = if let Some(reply) = reply.filter(|_| handler.declared_failure) {
                let result_shape = self.service.require_variant_shape(&handler.return_ty)?;
                let tag = self.service.variant_shapes[result_shape.0 as usize]
                    .runtime_tags
                    .iter()
                    .find_map(|(role, tag)| {
                        (*role == crate::RuntimeVariantRole::ResultErr).then_some(*tag)
                    })
                    .ok_or(SirLoweringError::from(
                        "failed handler lacks its Err variant",
                    ))?;
                Some(self.emit_typed(
                    Provenance::Synthesized,
                    &ResolvedTy::Bool,
                    SemOpKind::VariantIs {
                        shape: result_shape,
                        variant: tag,
                        source: Operand { value: reply },
                    },
                )?)
            } else {
                None
            };
            let response =
                self.intercept_variant(response_ty, branch.variant, reply.into_iter().collect())?;
            let outcome =
                self.intercept_outcome(outcome_ty, Some(response), failed, false, stream)?;
            self.owned_live.remove(&outcome);
            self.set_terminator(SemTerminator::Goto(Edge {
                target: join,
                args: vec![Operand { value: outcome }],
            }))?;
        }
        self.restore_control_state(&inherited);
        self.current = join;
        self.owned_live.remove(&parts[0]);
        self.owned_live.remove(&parts[4]);
        self.owned_live.insert(result, outcome_ty.clone());
        Ok(result)
    }
}
