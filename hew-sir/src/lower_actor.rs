//! Checked actor declarations and exclusive state bindings.

use super::{
    function_source_origin, lower_initial_value_transfer, Binding, BindingTarget, BlockArg,
    BodySource, Builder, CallResult, CallUnwind, CallableId, CallableInstance, CallableState,
    DefId, Edge, HirBinding, HirBlock, HirExpr, HirExprKind, HirFn, HirItem, HirModule,
    InstanceService, OpId, Operand, OwnKind, OwnedBindingUse, PlaceId, PlaceOrigin, ResolvedTy,
    SemAbiParam, SemCallConv, SemCallable, SemCallableKind, SemParamPassing, SemSignature,
    SemTerminator, ValueDef, ValueId,
};
use std::collections::BTreeSet;

pub(super) fn declaration<'a>(
    module: &'a HirModule,
    ty: &ResolvedTy,
) -> Option<&'a hew_hir::HirActorDecl> {
    // A lambda actor's handle is spelled `LambdaPid<M, R>`, not `LocalPid<A>`:
    // the declaration HIR synthesized for it records the handle it answers to.
    if matches!(
        ty,
        ResolvedTy::Named {
            builtin: Some(hew_types::BuiltinType::LambdaPid),
            ..
        }
    ) {
        return module.items.iter().find_map(|item| match item {
            HirItem::Actor(actor) if actor.lambda_handle_ty.as_deref() == Some(ty) => Some(actor),
            _ => None,
        });
    }
    let ResolvedTy::Named {
        builtin: Some(hew_types::BuiltinType::LocalPid | hew_types::BuiltinType::ChildRef),
        args,
        ..
    } = ty
    else {
        return None;
    };
    let [actor_ty] = args.as_slice() else {
        return None;
    };
    let instance = actor_ty.nominal_instance()?;
    module.items.iter().find_map(|item| match item {
        HirItem::Actor(actor)
            if &actor.declaration == instance.nominal.declaration() && instance.args.is_empty() =>
        {
            Some(actor)
        }
        _ => None,
    })
}

fn actor_overflow(source: &hew_hir::HirActorDecl) -> Result<crate::SemActorOverflow, String> {
    let overflow = match &source.overflow_policy {
        None | Some(hew_parser::ast::OverflowPolicy::Block) => crate::SemActorOverflow::Block,
        Some(hew_parser::ast::OverflowPolicy::DropNew) => crate::SemActorOverflow::DropNew,
        Some(hew_parser::ast::OverflowPolicy::DropOld) => crate::SemActorOverflow::DropOld,
        Some(hew_parser::ast::OverflowPolicy::Fail) => crate::SemActorOverflow::Fail,
        Some(hew_parser::ast::OverflowPolicy::Coalesce { .. }) => {
            return Err("coalescing requires a checked key projection".into())
        }
    };
    Ok(overflow)
}

impl InstanceService<'_> {
    /// The descriptor an actor handle or supervised role addresses.
    pub(super) fn require_actor(&mut self, ty: &ResolvedTy) -> Result<crate::ActorId, String> {
        self.require_actor_declaration(ty, None)
    }

    /// The descriptor for one actor, selected by an exact declaration when the
    /// handle type alone cannot name it.
    ///
    /// Two lambda actors with the same signature share one `LambdaPid<M, R>`,
    /// so a spawn — which must start the actor whose body it is — names its
    /// declaration. A call site does not: it reaches whichever lambda the
    /// handle addresses, and every actor answering to that handle carries the
    /// same message and reply glue.
    pub(super) fn require_actor_declaration(
        &mut self,
        ty: &ResolvedTy,
        exact: Option<&str>,
    ) -> Result<crate::ActorId, String> {
        if let Some(actor) = self.actors.iter().find(|actor| {
            exact.map_or_else(
                || actor.admits_target(ty),
                |name| actor.declaration.full_path() == name,
            )
        }) {
            return Ok(actor.id);
        }
        let source = match exact {
            Some(name) => self.module.items.iter().find_map(|item| match item {
                HirItem::Actor(actor) if actor.declaration.full_path() == name => Some(actor),
                _ => None,
            }),
            None => declaration(self.module, ty),
        }
        .ok_or("local actor handle lacks its exact declaration")?
        .clone();
        // A `ChildRef<A>` role and a `LocalPid<A>` handle address one actor, so
        // the descriptor is keyed by the pid spelling. A lambda actor's handle
        // is already its own spelling and has no separate role.
        let lambda_handle = source.lambda_handle_ty.clone().map(|ty| *ty);
        let ty = &lambda_handle.unwrap_or_else(|| {
            ResolvedTy::named_builtin(
                hew_types::BuiltinType::LocalPid.canonical_name(),
                hew_types::BuiltinType::LocalPid,
                match ty {
                    ResolvedTy::Named { args, .. } => args.clone(),
                    _ => unreachable!("declaration() matched a named handle"),
                },
            )
        });
        if !source.type_params.is_empty() {
            return Err("generic actors need their instance contracts".into());
        }
        if source
            .receive_handlers
            .iter()
            .any(|handler| handler.every_ns.is_some())
        {
            return Err("periodic receives need their scheduling contract".into());
        }
        if let Some(hook) = source.lifecycle_hooks.iter().find(|hook| {
            !matches!(
                hook.kind,
                hew_hir::HirLifecycleHookKind::Start | hew_hir::HirLifecycleHookKind::Stop
            )
        }) {
            return Err(format!(
                "`#[on({:?})]` needs the supervision and link notification contracts",
                hook.kind
            )
            .to_lowercase());
        }
        let overflow = actor_overflow(&source)?;
        if let Some(field) = source
            .state_fields
            .iter()
            .find(|field| super::generators::value_needs_close(self, &field.ty))
        {
            // Terminal cleanup releases state synchronously; a value that must
            // drain cooperatively first cannot live there yet.
            return Err(format!(
                "actor state field `{}` owns a value that needs cooperative close",
                field.name
            ));
        }
        let fields: Vec<_> = source
            .state_fields
            .iter()
            .map(|field| crate::SemActorField {
                ty: field.ty.clone(),
                mutable: field.is_mutable,
            })
            .collect();
        let state_ty = ResolvedTy::Tuple(fields.iter().map(|field| field.ty.clone()).collect());
        self.require_type_facts(ty)?;
        self.require_type_facts(&state_ty)?;
        let id = crate::ActorId(
            u32::try_from(self.actors.len()).map_err(|_| "actor count exceeds u32")?,
        );
        self.actors.push(crate::SemActor {
            id,
            declaration: source.declaration.clone(),
            handle_ty: ty.clone(),
            state_ty,
            fields,
            init: None,
            start: None,
            stop: Vec::new(),
            methods: Vec::new(),
            handlers: Vec::new(),
            mailbox_capacity: source.mailbox_capacity,
            overflow,
            max_heap_bytes: source.max_heap_bytes,
        });
        self.register_actor_bodies(id, &source)?;
        Ok(id)
    }

    fn register_actor_bodies(
        &mut self,
        id: crate::ActorId,
        source: &hew_hir::HirActorDecl,
    ) -> Result<(), String> {
        if let Some(init) = &source.init {
            let body = self.register_actor_body(
                id,
                source,
                init.declaration.clone(),
                &init.state_bindings,
                &init.params,
                ResolvedTy::Unit,
                &init.body,
                "init",
                true,
                None,
            )?;
            self.actors[id.0 as usize].init = Some(body);
        }
        for hook in &source.lifecycle_hooks {
            if !hook.params.is_empty() || hook.return_ty != ResolvedTy::Unit {
                return Err("lifecycle hook takes no parameters and returns unit".into());
            }
            let body = self.register_actor_body(
                id,
                source,
                hook.declaration.clone(),
                &hook.state_bindings,
                &[],
                ResolvedTy::Unit,
                &hook.body,
                &format!("hook_{}", hook.name),
                false,
                None,
            )?;
            match hook.kind {
                hew_hir::HirLifecycleHookKind::Start => {
                    self.actors[id.0 as usize].start = Some(body);
                }
                hew_hir::HirLifecycleHookKind::Stop => self.actors[id.0 as usize].stop.push(body),
                hew_hir::HirLifecycleHookKind::Crash
                | hew_hir::HirLifecycleHookKind::Exit
                | hew_hir::HirLifecycleHookKind::Down => {
                    unreachable!("supervision and link hooks are refused above")
                }
            }
        }
        for method in &source.methods {
            let body = self.register_actor_body(
                id,
                source,
                method.declaration.clone(),
                &method.state_bindings,
                &method.params,
                method.return_ty.clone(),
                &method.body,
                &format!("method_{}", method.name),
                false,
                None,
            )?;
            // Bare calls from this actor's bodies resolve through the ordinary
            // direct-call table; the call site supplies the state seat.
            self.table
                .monomorphic_by_declaration
                .insert(method.declaration.clone(), body);
            self.actors[id.0 as usize].methods.push(body);
        }
        self.register_actor_handlers(id, source)
    }

    fn register_actor_handlers(
        &mut self,
        id: crate::ActorId,
        source: &hew_hir::HirActorDecl,
    ) -> Result<(), String> {
        for handler in &source.receive_handlers {
            let row = source
                .protocol_descriptor
                .as_ref()
                .and_then(|protocol| {
                    protocol
                        .handlers
                        .iter()
                        .find(|row| row.name == handler.name)
                })
                .ok_or("receive body lacks its checker-selected protocol member")?;
            let mut params: Vec<_> = handler
                .params
                .iter()
                .map(|param| param.ty.clone())
                .collect();
            // The protocol names a generator receive by its stream type; the
            // body is checked against the element it yields.
            let checked_return = if handler.is_generator {
                ResolvedTy::named_builtin(
                    "Stream",
                    hew_types::BuiltinType::Stream,
                    vec![handler.return_ty.clone()],
                )
            } else {
                handler.return_ty.clone()
            };
            if params != row.param_tys || checked_return != row.return_ty {
                return Err(format!(
                    "actor protocol member `{}` ({:?} -> {}) differs from its checked body ({:?} -> {})",
                    handler.name,
                    row.param_tys,
                    row.return_ty.user_facing(),
                    params,
                    handler.return_ty.user_facing()
                ));
            }
            // A stream producer's request carries the consumer's sink and
            // replies through it, so its body returns unit.
            let stream = handler.is_generator.then(|| handler.return_ty.clone());
            let sink = stream.clone().map(|element| {
                ResolvedTy::named_builtin("Sink", hew_types::BuiltinType::Sink, vec![element])
            });
            let return_ty = if stream.is_some() {
                ResolvedTy::Unit
            } else {
                handler.return_ty.clone()
            };
            let callable = self.register_actor_body(
                id,
                source,
                handler.declaration.clone(),
                &handler.state_bindings,
                &handler.params,
                return_ty.clone(),
                &handler.body,
                &row.symbol,
                true,
                sink.as_ref(),
            )?;
            params.extend(sink);
            self.actors[id.0 as usize]
                .handlers
                .push(crate::SemActorHandler {
                    declaration: handler.declaration.clone(),
                    name: handler.name.clone(),
                    message_id: row.msg_id,
                    callable,
                    params,
                    return_ty,
                    stream,
                });
        }
        Ok(())
    }

    #[allow(
        clippy::too_many_arguments,
        reason = "one checked actor body supplies its complete callable contract"
    )]
    fn register_actor_body(
        &mut self,
        actor: crate::ActorId,
        source: &hew_hir::HirActorDecl,
        declaration: DefId,
        state_bindings: &[HirBinding],
        params: &[HirBinding],
        return_ty: ResolvedTy,
        body: &HirBlock,
        symbol: &str,
        transfers_arguments: bool,
        implicit_owned: Option<&ResolvedTy>,
    ) -> Result<CallableId, String> {
        let id = CallableId(
            u32::try_from(self.table.callables.len()).map_err(|_| "callable count exceeds u32")?,
        );
        let function = HirFn {
            id: source.id,
            node: body.node,
            declaration: declaration.clone(),
            name: symbol.to_string(),
            type_params: Vec::new(),
            params: params.to_vec(),
            var_self_receiver: None,
            return_ty: return_ty.clone(),
            body: body.clone(),
            span: body.span.clone(),
            is_generator: false,
            intrinsic_id: None,
        };
        let mut signature = SemSignature {
            params: vec![SemAbiParam {
                ty: self.actors[actor.0 as usize].state_ty.clone(),
                passing: SemParamPassing::BorrowMut,
                caller_visible_projection: true,
            }],
            return_ty,
        };
        for parameter in params {
            self.require_type_facts(&parameter.ty)?;
            // Messages and spawn arguments transfer every owning field. A
            // method call is an ordinary call and lends unless the source
            // consumes.
            let owned = OwnKind::of_ty(&parameter.ty, self.checked_facts.rows())? == OwnKind::Owned;
            signature.params.push(SemAbiParam {
                ty: parameter.ty.clone(),
                caller_visible_projection: false,
                passing: if !owned {
                    SemParamPassing::ReadOnly
                } else if transfers_arguments || parameter.is_consume {
                    SemParamPassing::Consume
                } else {
                    SemParamPassing::Borrow
                },
            });
        }
        if let Some(ty) = implicit_owned {
            self.require_type_facts(ty)?;
            signature.params.push(SemAbiParam {
                ty: ty.clone(),
                caller_visible_projection: false,
                passing: SemParamPassing::Consume,
            });
        }
        self.require_signature_shapes(&signature)?;
        self.table.callables.push(SemCallable {
            id,
            function: source.id,
            declaration,
            instance: CallableInstance::Monomorphic,
            symbol: format!("__hew_actor_{}_{}", actor.0, symbol),
            source_origin: function_source_origin(self.module, &function),
            signature,
            call_conv: SemCallConv::Default,
            kind: SemCallableKind::HewActor(actor),
        });
        self.actor_sources
            .insert(id, (function, state_bindings.to_vec()));
        self.states.push(CallableState::Unreached);
        self.statuses.push(None);
        self.request_body(id);
        Ok(id)
    }
}

impl Builder<'_, '_> {
    #[allow(
        clippy::too_many_lines,
        reason = "one ask boundary evaluates its request and constructs normal, cancellation and fault cleanup edges"
    )]
    pub(super) fn lower_actor_ask(&mut self, expression: &HirExpr) -> Result<ValueId, String> {
        let HirExprKind::ActorAsk {
            receiver,
            method_id,
            args,
            reply_ty: _,
            argument_order,
            policy,
            deadline_ns,
        } = &expression.kind
        else {
            unreachable!()
        };
        // A `policy(..)` view is a handle wrapper: the ask addresses the actor
        // it names, and the view itself carries no runtime operand.
        let target_ty =
            hew_types::actor_delivery::policy_view_parts(&self.ty(&receiver.ty).to_ty())
                .and_then(|(target, _)| ResolvedTy::from_ty(target).ok())
                .unwrap_or_else(|| self.ty(&receiver.ty));
        let actor = self.service.require_actor(&target_ty)?;
        let descriptor = &self.service.actors[actor.0 as usize];
        // A lambda actor's call site reaches it through `LambdaPid<M, R>`,
        // which names no particular lambda, so the dispatch id is the marker
        // rather than a declaration path; the actor's one handler is the
        // member. Every other actor selects its member by exact identity.
        let handler = if method_id == hew_types::actor_protocol::LAMBDA_ACTOR_METHOD_ID {
            match descriptor.handlers.as_slice() {
                [only] if descriptor.is_lambda() => only,
                _ => return Err("a lambda actor declares exactly one handler".into()),
            }
        } else {
            descriptor
                .handlers
                .iter()
                .find(|handler| handler.declaration.full_path() == method_id.as_str())
                .ok_or("ask has no exact receive protocol member")?
        };
        let message = handler.message_id;

        let output = self.ty(&expression.ty);
        let signature = descriptor.ask_signature(message, &target_ty, output.clone())?;
        if signature.return_ty != output
            || signature.params.len() != args.len() + 1
            || argument_order.iter().copied().collect::<BTreeSet<_>>() != (0..args.len()).collect()
            || argument_order.len() != args.len()
            || signature
                .params
                .iter()
                .skip(1)
                .zip(argument_order)
                .any(|(expected, index)| expected.ty != self.ty(&args[*index].ty))
        {
            return Err("ask must return its complete checked Result".into());
        }
        let mut inputs = Vec::new();
        let (target, _) = self.delivery_target(receiver)?;
        if !self.is_open() {
            return Ok(target);
        }
        inputs.push(crate::BoundaryOperand {
            operand: Operand { value: target },
            decision: crate::BoundaryDecision::Move,
        });
        for source in args {
            let value = lower_initial_value_transfer(
                self,
                source,
                "ask request argument",
                OwnedBindingUse::Copy,
            )?;
            if !self.is_open() {
                return Ok(value);
            }
            inputs.push(crate::BoundaryOperand {
                operand: Operand { value },
                decision: crate::BoundaryDecision::Move,
            });
        }
        inputs = std::iter::once(inputs[0].clone())
            .chain(argument_order.iter().map(|index| inputs[index + 1].clone()))
            .collect();
        for input in &inputs {
            self.owned_live.remove(&input.operand.value);
        }
        self.service.require_type_facts(&output)?;
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
        let edge = |target| Edge {
            target,
            args: Vec::new(),
        };
        self.set_terminator(SemTerminator::Suspend {
            kind: crate::SuspendKind::Ask {
                actor,
                message,
                policy: *policy,
                deadline_ns: *deadline_ns,
            },
            inputs,
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
        let saved = self.control_state();
        for cleanup in [cancel, unwind] {
            self.restore_control_state(&saved);
            self.current = cleanup;
            self.finish_fault_exit()?;
        }
        self.restore_control_state(&saved);
        self.current = resumed;
        if own == OwnKind::Owned {
            self.owned_live.insert(value, output);
        }
        Ok(value)
    }

    #[expect(
        clippy::too_many_lines,
        reason = "checked actor operation argument selection"
    )]
    fn actor_arguments(
        &mut self,
        expression: &HirExpr,
    ) -> Result<(crate::ActorOperation, Vec<HirExpr>, Vec<usize>), String> {
        match &expression.kind {
            HirExprKind::ActorDelivery {
                receiver,
                args,
                operation,
            } if matches!(
                operation,
                hew_types::actor_delivery::ActorDeliveryCall::Close
                    | hew_types::actor_delivery::ActorDeliveryCall::AwaitClosed
            ) =>
            {
                if !args.is_empty() {
                    return Err("actor lifecycle boundary has unexpected arguments".into());
                }
                let target_ty = self.ty(&receiver.ty);
                let closing = matches!(
                    operation,
                    hew_types::actor_delivery::ActorDeliveryCall::Close
                );
                let actor = self.service.require_actor(&target_ty)?;
                let boundary = if closing {
                    crate::ActorOperation::Close(actor)
                } else {
                    crate::ActorOperation::AwaitClosed(actor)
                };
                Ok((boundary, vec![(**receiver).clone()], vec![0]))
            }
            HirExprKind::ActorSelf => {
                let actor = self.service.require_actor(&self.ty(&expression.ty))?;
                Ok((
                    crate::ActorOperation::SelfHandle(actor),
                    Vec::new(),
                    Vec::new(),
                ))
            }
            HirExprKind::Spawn { args, .. }
                if super::supervisor::declaration(
                    self.service.module,
                    &self.ty(&expression.ty),
                )
                .is_some() =>
            {
                let ty = self.ty(&expression.ty);
                let id = self.service.require_supervisor(&ty)?;
                let source = super::supervisor::declaration(self.service.module, &ty)
                    .ok_or("spawn lost its supervisor declaration")?;
                let values: Vec<_> = args.iter().map(|(_, value)| value.clone()).collect();
                let mut argument_order = Vec::new();
                for parameter in &source.params {
                    let (index, _) = args
                        .iter()
                        .enumerate()
                        .find(|(_, (name, _))| *name == parameter.name)
                        .ok_or_else(|| {
                            format!("supervisor config `{}` is missing", parameter.name)
                        })?;
                    argument_order.push(index);
                }
                if argument_order.len() != args.len() {
                    return Err("spawn carries an unknown supervisor config argument".into());
                }
                Ok((
                    crate::ActorOperation::SupervisorSpawn(id),
                    values,
                    argument_order,
                ))
            }
            HirExprKind::Spawn { actor_name, args } => {
                let ty = self.ty(&expression.ty);
                // A spawn starts one exact declaration's body, so it selects
                // by the declaration it names rather than by handle type.
                let exact = ty
                    .is_builtin(hew_types::BuiltinType::LambdaPid)
                    .then_some(actor_name.as_str());
                let id = self.service.require_actor_declaration(&ty, exact)?;
                let declaration_path = self.service.actors[id.0 as usize]
                    .declaration
                    .full_path()
                    .to_string();
                let source = self
                    .service
                    .module
                    .items
                    .iter()
                    .find_map(|item| match item {
                        HirItem::Actor(actor)
                            if actor.declaration.full_path() == declaration_path =>
                        {
                            Some(actor)
                        }
                        _ => None,
                    })
                    .ok_or("spawn lost its actor declaration")?;
                let mut values: Vec<_> = args.iter().map(|(_, value)| value.clone()).collect();
                let mut argument_order = Vec::new();
                let mut used = BTreeSet::new();
                for field in &source.state_fields {
                    if let Some((index, _)) = args
                        .iter()
                        .enumerate()
                        .find(|(_, (name, _))| *name == field.name)
                    {
                        argument_order.push(index);
                        used.insert(index);
                    } else if let Some(default) = &field.default {
                        argument_order.push(values.len());
                        values.push(default.clone());
                    } else {
                        return Err(format!(
                            "actor state field `{}` requires an initialized spawn value",
                            field.name
                        ));
                    }
                }
                if let Some(init) = &source.init {
                    for parameter in &init.params {
                        let (index, _) = args
                            .iter()
                            .enumerate()
                            .find(|(_, (name, _))| *name == parameter.name)
                            .ok_or("actor init argument is missing")?;
                        if !used.insert(index) {
                            return Err(
                                "spawn argument cannot initialize both state and an init parameter"
                                    .into(),
                            );
                        }
                        argument_order.push(index);
                    }
                }
                if used.len() != args.len() {
                    return Err("spawn carries an unknown actor argument".into());
                }
                Ok((crate::ActorOperation::Spawn(id), values, argument_order))
            }
            HirExprKind::ActorDelivery {
                receiver,
                args,
                operation: hew_types::actor_delivery::ActorDeliveryCall::Submit { policy },
            } => {
                if !args.is_empty() {
                    return Err("submission has unexpected operands".into());
                }
                let message_ty = self.ty(&receiver.ty);
                let ResolvedTy::Named {
                    args: type_args, ..
                } = &message_ty
                else {
                    return Err("submission has no message type arguments".into());
                };
                let target = type_args.first().ok_or("message has no target type")?;
                let actor = self.service.require_actor(target)?;
                let result_ty = self.ty(&expression.ty);
                self.service.require_type_facts(&result_ty)?;
                Ok((
                    crate::ActorOperation::Submit {
                        actor,
                        policy: *policy,
                        message_ty,
                        result_ty,
                    },
                    vec![(**receiver).clone()],
                    vec![0],
                ))
            }
            _ => Err("actor boundary requires a checked spawn or send".into()),
        }
    }

    pub(super) fn lower_actor_boundary(
        &mut self,
        expression: &HirExpr,
    ) -> Result<Option<ValueId>, String> {
        let (operation, sources, argument_order) = self.actor_arguments(expression)?;
        let signature = self.actor_signature(&operation)?;
        let mut values = Vec::new();
        // Evaluate explicit arguments in source order, then defaults. Only the
        // completed values are rearranged into state and init parameter order.
        for source in &sources {
            let value = lower_initial_value_transfer(
                self,
                source,
                "actor message boundary",
                OwnedBindingUse::Copy,
            )?;
            if !self.is_open() {
                return Ok(None);
            }
            values.push(value);
        }
        if argument_order.len() != signature.params.len() {
            return Err("actor argument count differs from its protocol".into());
        }
        let mut args = Vec::new();
        for (index, parameter) in argument_order.into_iter().zip(&signature.params) {
            if self.ty(&sources[index].ty) != parameter.ty {
                return Err("actor argument changes its protocol type".into());
            }
            args.push(values[index]);
        }
        self.emit_actor_call(operation, signature, args)
    }

    pub(super) fn actor_signature(
        &self,
        operation: &crate::ActorOperation,
    ) -> Result<SemSignature, String> {
        operation.signature(&self.service.actors, &self.service.supervisors, |id| {
            self.service
                .callable(id)
                .map(|callable| callable.signature.clone())
        })
    }

    /// Transfer evaluated operands across one actor boundary and continue
    /// with its typed result.
    pub(super) fn emit_actor_call(
        &mut self,
        operation: crate::ActorOperation,
        signature: SemSignature,
        args: Vec<ValueId>,
    ) -> Result<Option<ValueId>, String> {
        let args: Vec<_> = args
            .into_iter()
            .map(|value| crate::BoundaryOperand {
                operand: Operand { value },
                decision: crate::BoundaryDecision::Move,
            })
            .collect();
        for arg in &args {
            self.owned_live.remove(&arg.operand.value);
        }
        let (result, normal, continuation) = if signature.return_ty == ResolvedTy::Unit {
            (
                CallResult::Unit,
                Edge {
                    target: self.new_block(Vec::new()),
                    args: Vec::new(),
                },
                None,
            )
        } else {
            let ty = signature.return_ty;
            let own = OwnKind::of_ty(&ty, self.service.checked_facts.rows())?;
            let raw = self.fresh_value();
            let continuation = self.fresh_value();
            let target = self.new_block(vec![BlockArg {
                value: continuation,
                ty: ty.clone(),
                own,
            }]);
            (
                CallResult::Value(ValueDef { id: raw, ty, own }),
                Edge {
                    target,
                    args: vec![Operand { value: raw }],
                },
                Some(continuation),
            )
        };
        let failure = self.new_block(Vec::new());
        let normal_target = normal.target;
        let id = OpId(self.ops);
        self.ops += 1;
        self.set_terminator(SemTerminator::ActorCall {
            id,
            operation,
            args,
            result,
            normal,
            unwind: CallUnwind::Cleanup(Edge {
                target: failure,
                args: Vec::new(),
            }),
        })?;
        let saved = self.control_state();
        self.current = failure;
        self.finish_fault_exit()?;
        self.restore_control_state(&saved);
        self.current = normal_target;
        if let Some(value) = continuation {
            if self.value_own_kind(value) == Some(OwnKind::Owned) {
                self.owned_live.insert(
                    value,
                    self.value_ty(value).ok_or("actor result lacks its type")?,
                );
            }
        }
        Ok(continuation)
    }

    pub(super) fn bind_actor_state(&mut self, source: &BodySource) -> Result<(), String> {
        let BodySource::Actor {
            actor,
            state_bindings,
        } = source
        else {
            return Ok(());
        };
        let descriptor = self
            .service
            .actors
            .get(actor.0 as usize)
            .ok_or_else(|| "actor body has no state descriptor".to_string())?
            .clone();
        if state_bindings.len() != descriptor.fields.len() {
            return Err("actor body bindings differ from its complete state".into());
        }
        let abi = &self.callable.signature.params[0];
        if abi.ty != descriptor.state_ty || abi.passing != SemParamPassing::BorrowMut {
            return Err("actor body requires its exclusive state receiver".into());
        }
        self.params.insert(
            0,
            BlockArg {
                value: ValueId(0),
                ty: abi.ty.clone(),
                own: OwnKind::Guaranteed,
            },
        );
        for (index, (binding, field)) in state_bindings.iter().zip(&descriptor.fields).enumerate() {
            if binding.ty != field.ty {
                return Err("actor field binding changes its declared type".into());
            }
            let place =
                PlaceId(u32::try_from(self.places.len()).map_err(|_| "place count exceeds u32")?);
            self.places.push(crate::PlaceDecl {
                id: place,
                ty: field.ty.clone(),
                origin: PlaceOrigin::ActorState {
                    actor: *actor,
                    state: ValueId(0),
                    field: u32::try_from(index).map_err(|_| "state field count exceeds u32")?,
                },
            });
            self.bindings
                .insert(binding.id, BindingTarget::Place(place));
            let declaration = self.source_bindings.len();
            self.source_bindings.push(Binding {
                id: crate::BindingId(
                    u32::try_from(declaration).map_err(|_| "binding count exceeds u32")?,
                ),
                name: binding.name.clone(),
                span: binding.span.clone(),
                mutable: field.mutable || descriptor.init == Some(self.callable.id),
                target: BindingTarget::Place(place),
            });
            self.binding_declarations.insert(binding.id, declaration);
            // The actor owns these seats beyond this body's lexical scope.
            // No EndLifetime or implicit extraction belongs to the handler.
        }
        Ok(())
    }
}

impl Builder<'_, '_> {
    fn delivery_target(&mut self, receiver: &HirExpr) -> Result<(ValueId, ResolvedTy), String> {
        let ty = self.ty(&receiver.ty);
        let value =
            lower_initial_value_transfer(self, receiver, "sender target", OwnedBindingUse::Copy)?;
        if hew_types::actor_delivery::sender_parts(&ty.to_ty()).is_none()
            && hew_types::actor_delivery::policy_view_parts(&ty.to_ty()).is_none()
        {
            return Ok((value, ty));
        }
        let ResolvedTy::Named { args, .. } = &ty else {
            unreachable!()
        };
        let target_ty = args[0].clone();
        let shape = self.service.require_aggregate_shape(&ty)?;
        let target = self.emit_typed(
            crate::Provenance::Site(receiver.site),
            &target_ty,
            crate::SemOpKind::AggregateProjectCopy {
                shape,
                aggregate: Operand { value },
                field: 0,
            },
        )?;
        Ok((target, target_ty))
    }

    pub(super) fn lower_actor_message(&mut self, expression: &HirExpr) -> Result<ValueId, String> {
        let HirExprKind::ActorMessage {
            receiver,
            method_id,
            args,
            policy,
            argument_order,
        } = &expression.kind
        else {
            unreachable!()
        };
        let (target, target_ty) = self.delivery_target(receiver)?;
        let actor = self.service.require_actor(&target_ty)?;
        let handler = self.service.actors[actor.0 as usize]
            .handlers
            .iter()
            .find(|handler| handler.declaration.full_path() == method_id.as_str())
            .ok_or("message description has no exact receive member")?
            .clone();
        if handler.return_ty != ResolvedTy::Unit
            || handler.params.len() != args.len()
            || argument_order.len() != args.len()
            || argument_order.iter().copied().collect::<BTreeSet<_>>() != (0..args.len()).collect()
            || handler
                .params
                .iter()
                .zip(argument_order)
                .any(|(expected, index)| *expected != self.ty(&args[*index].ty))
        {
            return Err("message description disagrees with its receive protocol".into());
        }
        let ty = self.ty(&expression.ty);
        let expected = hew_types::actor_delivery::message_type(
            target_ty.to_ty(),
            hew_types::Ty::Tuple(handler.params.iter().map(ResolvedTy::to_ty).collect()),
            *policy,
        );
        if ty.to_ty() != expected {
            return Err("message description changes its checked value type".into());
        }
        let payload_ty = ResolvedTy::Tuple(handler.params);
        let mut values = Vec::new();
        for arg in args {
            values.push(lower_initial_value_transfer(
                self,
                arg,
                "message argument",
                OwnedBindingUse::Copy,
            )?);
        }
        let fields = argument_order
            .iter()
            .map(|index| Operand {
                value: values[*index],
            })
            .collect();
        let shape = self.service.require_aggregate_shape(&payload_ty)?;
        let payload = self.emit_typed(
            crate::Provenance::Site(expression.site),
            &payload_ty,
            crate::SemOpKind::AggregateMake { shape, fields },
        )?;
        for value in values {
            self.owned_live.remove(&value);
        }
        let message = self.emit_typed(
            crate::Provenance::Site(expression.site),
            &ResolvedTy::U32,
            crate::SemOpKind::ConstInteger(i128::from(handler.message_id)),
        )?;
        self.make_delivery_record(expression, vec![target, message, payload])
    }

    fn make_delivery_record(
        &mut self,
        expression: &HirExpr,
        fields: Vec<ValueId>,
    ) -> Result<ValueId, String> {
        let shape = self
            .service
            .require_aggregate_shape(&self.ty(&expression.ty))?;
        let value = self.emit(
            expression,
            crate::SemOpKind::AggregateMake {
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

    pub(super) fn lower_actor_delivery(&mut self, expression: &HirExpr) -> Result<ValueId, String> {
        use hew_types::actor_delivery::ActorDeliveryCall;
        let HirExprKind::ActorDelivery {
            receiver,
            args,
            operation,
        } = &expression.kind
        else {
            unreachable!()
        };
        match operation {
            ActorDeliveryCall::Policy { .. } => {
                let (target, _) = self.delivery_target(receiver)?;
                self.make_delivery_record(expression, vec![target])
            }
            ActorDeliveryCall::Readdress { .. } => {
                let [destination] = args.as_slice() else {
                    return Err("readdressing requires one destination".into());
                };
                // Keep the original description alive through destination evaluation.
                let message = lower_initial_value_transfer(
                    self,
                    receiver,
                    "readdressed message",
                    OwnedBindingUse::Move,
                )?;
                let (target, _) = self.delivery_target(destination)?;
                let ty = self.ty(&receiver.ty);
                let shape = self.service.require_aggregate_shape(&ty)?;
                let fields = self.emit_destructure_value(
                    message,
                    &ty,
                    shape,
                    crate::Provenance::Site(expression.site),
                )?;
                let [_, message, payload] = fields.as_slice() else {
                    return Err(
                        "message description must contain its target, member and payload".into(),
                    );
                };
                self.make_delivery_record(expression, vec![target, message.id, payload.id])
            }
            ActorDeliveryCall::Submit { .. }
            | ActorDeliveryCall::Close
            | ActorDeliveryCall::AwaitClosed => self
                .lower_actor_boundary(expression)?
                .ok_or_else(|| "submission has no result".into()),
        }
    }

    /// `pid.stream()`: create a bounded pipe, start the producer turn with
    /// the request and its sink, and continue with the stream half.
    pub(super) fn lower_actor_stream(&mut self, expression: &HirExpr) -> Result<ValueId, String> {
        let HirExprKind::ActorGenStream {
            receiver,
            method,
            args,
        } = &expression.kind
        else {
            unreachable!()
        };
        let (target, target_ty) = self.delivery_target(receiver)?;
        let actor = self.service.require_actor(&target_ty)?;
        let handler = self.service.actors[actor.0 as usize]
            .handlers
            .iter()
            .find(|handler| {
                handler.declaration.full_path() == method.as_str() && handler.stream.is_some()
            })
            .ok_or("stream request has no exact producer member")?
            .clone();
        let stream_ty = self.ty(&expression.ty);
        let Some(sink_ty) = handler.params.last() else {
            return Err("stream producer lacks its sink parameter".into());
        };
        if crate::pipe_parts(&stream_ty, sink_ty) != handler.stream.as_ref()
            || handler.params.len() != args.len() + 1
            || handler
                .params
                .iter()
                .zip(args)
                .any(|(expected, arg)| *expected != self.ty(&arg.ty))
        {
            return Err("stream request disagrees with its producer protocol".into());
        }
        let mut values = Vec::new();
        for arg in args {
            values.push(lower_initial_value_transfer(
                self,
                arg,
                "stream request argument",
                OwnedBindingUse::Copy,
            )?);
            if !self.is_open() {
                return Err("stream request argument diverged".into());
            }
        }
        let provenance = crate::Provenance::Site(expression.site);
        let (stream, sink) = self.emit_stream_pipe(&stream_ty, sink_ty, provenance.clone())?;
        values.push(sink);
        let payload_ty = ResolvedTy::Tuple(handler.params.clone());
        let shape = self.service.require_aggregate_shape(&payload_ty)?;
        let payload = self.emit_typed(
            provenance.clone(),
            &payload_ty,
            crate::SemOpKind::AggregateMake {
                shape,
                fields: values
                    .iter()
                    .map(|value| Operand { value: *value })
                    .collect(),
            },
        )?;
        for value in values {
            self.owned_live.remove(&value);
        }
        let operation = crate::ActorOperation::StreamStart {
            actor,
            message: handler.message_id,
            target: target_ty,
        };
        let signature = self.actor_signature(&operation)?;
        if self
            .emit_actor_call(operation, signature, vec![target, payload])?
            .is_some()
        {
            return Err("stream start has no result".into());
        }
        Ok(stream)
    }

    /// Both pipe halves are owned; the stream stays live for the caller and
    /// the sink transfers into the producer's request.
    fn emit_stream_pipe(
        &mut self,
        stream_ty: &ResolvedTy,
        sink_ty: &ResolvedTy,
        provenance: crate::Provenance,
    ) -> Result<(ValueId, ValueId), String> {
        self.service.require_type_facts(stream_ty)?;
        self.service.require_type_facts(sink_ty)?;
        let stream = self.fresh_value();
        let sink = self.fresh_value();
        let id = OpId(self.ops);
        self.current_block_mut().append_op(crate::SemOp {
            id,
            results: vec![
                ValueDef {
                    id: stream,
                    ty: stream_ty.clone(),
                    own: OwnKind::Owned,
                },
                ValueDef {
                    id: sink,
                    ty: sink_ty.clone(),
                    own: OwnKind::Owned,
                },
            ],
            kind: crate::SemOpKind::StreamPipe {
                capacity: STREAM_PIPE_CAPACITY,
            },
            provenance,
        })?;
        self.ops += 1;
        self.owned_live.insert(stream, stream_ty.clone());
        Ok((stream, sink))
    }
}

/// Elements a producer may run ahead of its consumer before it parks.
const STREAM_PIPE_CAPACITY: u32 = 16;
