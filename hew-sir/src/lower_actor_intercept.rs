//! Compile each interceptor chain into the actor's ordinary callable graph.

use super::*;
use hew_types::actor_delivery::InterceptorOperation;

impl InstanceService<'_> {
    pub(super) fn weave_actor_interceptor(
        &mut self,
        actor: crate::ActorId,
        source: &hew_hir::HirActorDecl,
        substitution: &TypeSubstitution,
    ) -> Result<(), String> {
        let Some(declaration) = source.protocol_descriptor.interceptor else {
            return Ok(());
        };
        let interceptor = self.actors[actor.0 as usize]
            .methods
            .iter()
            .copied()
            .find(|callable| {
                self.callable(*callable)
                    .is_some_and(|body| body.declaration == declaration)
            })
            .ok_or("actor interceptor has no registered body")?;
        let originals = self.actors[actor.0 as usize].handlers.clone();
        self.actor_sources
            .get_mut(&interceptor)
            .ok_or("interceptor source is absent")?
            .weave = Some(ActorWeave::Interceptor {
            next: InterceptorNext::Handlers(originals.clone()),
        });
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
                .ok_or("interceptor wrapper source is absent")?
                .weave = Some(ActorWeave::Entry {
                handler: index,
                interceptor,
            });
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

    fn intercept_fields(&self, ty: &ResolvedTy) -> Result<Vec<(String, ResolvedTy)>, String> {
        hew_types::actor_protocol::interceptor_fields(&self.service.module.defs, ty).ok_or_else(
            || {
                format!(
                    "interceptor token `{}` has no concrete actor",
                    ty.user_facing()
                )
            },
        )
    }

    fn intercept_record(
        &mut self,
        ty: &ResolvedTy,
        fields: Vec<ValueId>,
    ) -> Result<ValueId, String> {
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
    ) -> Result<ValueId, String> {
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

    fn intercept_bool(&mut self, value: bool) -> Result<ValueId, String> {
        self.emit_typed(
            Provenance::Synthesized,
            &ResolvedTy::Bool,
            SemOpKind::ConstBool(value),
        )
    }

    fn intercept_drop(&mut self, value: ValueId) -> Result<(), String> {
        if self.owned_live.contains_key(&value) {
            self.emit_destroy(value)?;
        }
        Ok(())
    }

    fn intercept_parts(&mut self, value: ValueId, ty: &ResolvedTy) -> Result<Vec<ValueId>, String> {
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
    ) -> Result<Option<ValueId>, String> {
        let callable = self
            .service
            .callable(callee)
            .cloned()
            .ok_or("interceptor next body is absent")?;
        let state = self
            .params
            .first()
            .ok_or("interceptor has no actor state seat")?
            .value;
        let mut values = vec![state].into_iter().chain(values);
        let arguments = callable
            .signature
            .params
            .iter()
            .map(|parameter| {
                let value = values
                    .next()
                    .ok_or("interceptor call has too few arguments")?;
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
            .collect::<Result<Vec<_>, String>>()?;
        if values.next().is_some() {
            return Err("interceptor call has too many arguments".into());
        }
        let live = self.owned_live.keys().copied().collect();
        self.finish_user_call(
            PreparedCallee::Direct(callee),
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
    ) -> Result<Option<ValueId>, String> {
        let contract = family
            .semantic_contract()
            .ok_or("interceptor runtime contract is absent")?;
        if !contract.failures.is_empty() {
            return Err("interceptor runtime query unexpectedly fails".into());
        }
        let types = values
            .iter()
            .map(|value| {
                self.value_ty(*value)
                    .ok_or("interceptor runtime value lacks its type")
            })
            .collect::<Result<Vec<_>, _>>()?;
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
    ) -> Result<(), String> {
        let descriptor = self.service.actors[actor.0 as usize].handlers[handler].clone();
        if descriptor.stream.is_some() {
            return Err("stream interceptor carrier is not yet lowered".into());
        }
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
            u32::try_from(handler).map_err(|_| "handler index exceeds u32")?,
            arguments,
        )?;
        self.service.require_variant_shape(&fields[1].1)?;
        let caller = self
            .intercept_runtime(
                hew_types::RuntimeCallFamily::ActorCaller,
                Vec::new(),
                &fields[1].1,
            )?
            .ok_or("caller query returned no value")?;
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
        let call = self.intercept_record(&call_ty, vec![request, caller, name, message])?;
        let outcome = self
            .intercept_invoke(interceptor, vec![call])?
            .ok_or("interceptor returned no outcome")?;
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
            let variants = self.emit_variant_switch(shape, &shape_info, response.value)?;
            let state = self.control_state();
            for variant in variants {
                self.restore_control_state(&state);
                self.current = variant.block;
                self.owned_live = variant.owned_live;
                if variant.variant as usize != handler {
                    for field in variant.fields {
                        self.intercept_drop(field.value)?;
                    }
                    self.set_terminator(SemTerminator::Unreachable)?;
                } else {
                    self.finish_return_value(variant.fields.first().map(|field| {
                        crate::BoundaryOperand {
                            operand: Operand { value: field.value },
                            decision: crate::BoundaryDecision::Move,
                        }
                    }))?;
                }
            }
        }
        Ok(())
    }

    pub(super) fn lower_interceptor_operation(
        &mut self,
        expression: &HirExpr,
        receiver: &HirExpr,
        args: &[HirExpr],
        operation: InterceptorOperation,
    ) -> Result<ValueId, String> {
        use InterceptorOperation as Op;
        let ty = self.ty(&receiver.ty);
        if !operation.consumes() {
            let index = match operation {
                Op::Caller => 1,
                Op::HandlerName => 2,
                Op::Failed => 1,
                Op::Refused => 2,
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
                .ok_or("proceed outside an interceptor")?;
            let Some(ActorWeave::Interceptor { next }) = source.weave.clone() else {
                return Err("proceed has no next interceptor layer".into());
            };
            return match next {
                InterceptorNext::Layer(next) => self
                    .intercept_invoke(next, vec![call])?
                    .ok_or("next layer returned no outcome".into()),
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
        for part in parts {
            self.intercept_drop(part)?;
        }
        self.intercept_outcome(&self.ty(&expression.ty), None, None, true)
    }

    fn intercept_outcome(
        &mut self,
        ty: &ResolvedTy,
        response: Option<ValueId>,
        failed: Option<ValueId>,
        refused: bool,
    ) -> Result<ValueId, String> {
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
            .ok_or("outcome response lacks its Option role")?;
        let response =
            self.intercept_variant(&response_ty, variant, response.into_iter().collect())?;
        let failed = match failed {
            Some(value) => value,
            None => self.intercept_bool(false)?,
        };
        let refused = self.intercept_bool(refused)?;
        self.intercept_record(ty, vec![response, failed, refused])
    }

    fn lower_intercept_handlers(
        &mut self,
        call: ValueId,
        call_ty: &ResolvedTy,
        outcome_ty: &ResolvedTy,
        handlers: &[crate::SemActorHandler],
    ) -> Result<ValueId, String> {
        let request_ty = self.intercept_fields(call_ty)?[0].1.clone();
        let response_option = self.intercept_fields(outcome_ty)?[0].1.clone();
        let ResolvedTy::Named { args, .. } = response_option else {
            return Err("outcome response has no Option payload".into());
        };
        let response_ty = &args[0];
        let parts = self.intercept_parts(call, call_ty)?;
        for part in &parts[1..] {
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
                .ok_or("request variant has no original handler")?;
            let reply = self.intercept_invoke(
                handler.callable,
                branch.fields.iter().map(|field| field.value).collect(),
            )?;
            let failed =
                if let Some(reply) = reply.filter(|_| matches!(&handler.return_ty, ResolvedTy::Named { head, .. } if *head == hew_types::TypeHead::Builtin(hew_types::BuiltinType::Result))) {
                    let result_shape = self.service.require_variant_shape(&handler.return_ty)?;
                    let tag = self.service.variant_shapes[result_shape.0 as usize]
                        .runtime_tags
                        .iter()
                        .find_map(|(role, tag)| {
                            (*role == crate::RuntimeVariantRole::ResultErr).then_some(*tag)
                        })
                        .ok_or("failed handler lacks its Err variant")?;
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
            let outcome = self.intercept_outcome(outcome_ty, Some(response), failed, false)?;
            self.owned_live.remove(&outcome);
            self.set_terminator(SemTerminator::Goto(Edge {
                target: join,
                args: vec![Operand { value: outcome }],
            }))?;
        }
        self.restore_control_state(&inherited);
        self.current = join;
        self.owned_live.remove(&parts[0]);
        self.owned_live.insert(result, outcome_ty.clone());
        Ok(result)
    }
}
