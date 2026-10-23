//! Spawn and lambda-actor lowering.

use super::*;
use hew_parser::ast::Ident;

impl LowerCtx {
    pub(super) fn lower_spawn(
        &mut self,
        target: &Spanned<Expr>,
        args: &[(Ident, Spanned<Expr>)],
        span: Span,
    ) -> (HirExprKind, ResolvedTy) {
        // Syntactic fallback only. The checker's spawn result type
        // (`bank.Account`'s own actor-handle type) is the identity authority
        // below; the dotted `{module}.{field}` spelling here mirrors the checker's
        // qualified-spawn resolution for the diagnostic-recovery paths where
        // no expr_types entry exists.
        let actor_name = match &target.0 {
            Expr::Ident(name) => Some(name.to_string()),
            Expr::FieldAccess { object, field } => {
                if let Expr::Ident(module) = &object.0 {
                    Some(format!("{module}.{field}", field = field.0))
                } else {
                    None
                }
            }
            _ => None,
        };
        let Some(mut actor_name) = actor_name else {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::NotYetImplemented {
                    construct: "spawn target expression".to_string(),
                    owning_pass: "actor-body-lowering".to_string(),
                },
                span,
                "`spawn` currently requires a named actor target",
            ));
            return (
                HirExprKind::Unsupported("unsupported spawn target".to_string()),
                ResolvedTy::Unit,
            );
        };

        let mut lowered_args = args
            .iter()
            .map(|(name, expr)| (name.to_string(), self.lower_expr(expr, IntentKind::Read)))
            .collect::<Vec<_>>();
        let ty = if let Some(ty) = self.expr_types.get(&self.mk_key(&span)).cloned() {
            match ResolvedTy::from_ty(&ty) {
                Ok(resolved) => resolved,
                Err(err) => {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::CheckerBoundaryViolation {
                            name: format!("spawn {actor_name}"),
                            reason: err.to_string(),
                        },
                        span.clone(),
                        "checker-authoritative spawn result type failed boundary conversion",
                    ));
                    ResolvedTy::Unit
                }
            }
        } else {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::CheckerBoundaryViolation {
                    name: format!("spawn {actor_name}"),
                    reason: "missing expr_types entry".to_string(),
                },
                span.clone(),
                "checker did not provide a result type for this actor spawn expression",
            ));
            ResolvedTy::Unit
        };
        // The checker's handle type names the actor's resolved identity:
        // dotted (`bank.Account`) for module actors, bare for root/flat
        // actors. It already encodes the local-first bare-name resolution (a
        // bare `spawn Account` inside module `bank` resolves to
        // `bank.Account`), so it overrides the syntactic spelling. MIR actor
        // layouts key on the same identity (`qualified_name()`).
        if let Some(inner) = Self::actor_handle_identity(&ty) {
            inner.clone_into(&mut actor_name);
        }
        if let Some(qualified) = self
            .imported_actor_rewrites
            .as_ref()
            .and_then(|rewrites| rewrites.get(&actor_name))
        {
            actor_name.clone_from(qualified);
        }
        let slots = self.checked_spawn_slots(&span, &actor_name, &mut lowered_args);
        (
            HirExprKind::Spawn {
                actor_name,
                args: lowered_args,
                slots,
            },
            ty,
        )
    }

    /// The checker's binding of a spawn's (or supervisor child's) keys.
    pub(super) fn checked_spawn_slots(
        &mut self,
        span: &Span,
        actor_name: &str,
        args: &mut Vec<(String, HirExpr)>,
    ) -> Vec<crate::node::HirSpawnSlot> {
        if let Some(slots) = self.spawn_argument_slots.get(&self.mk_key(span)).cloned() {
            return slots
                .into_iter()
                .map(|slot| match slot {
                    hew_types::check::SpawnSlot::Written(index) => {
                        crate::node::HirSpawnSlot::Written(index)
                    }
                    hew_types::check::SpawnSlot::Default => crate::node::HirSpawnSlot::Default,
                    hew_types::check::SpawnSlot::ArgumentDefault(default) => {
                        let index = args.len();
                        args.push((String::new(), self.lower_default_argument(&default)));
                        crate::node::HirSpawnSlot::ArgumentDefault(index)
                    }
                })
                .collect();
        }
        self.diagnostics.push(HirDiagnostic::new(
            HirDiagnosticKind::CheckerBoundaryViolation {
                name: format!("spawn {actor_name}"),
                reason: "missing spawn_argument_slots entry".to_string(),
            },
            span.clone(),
            "checker did not bind the keys of this spawn",
        ));
        Vec::new()
    }

    pub(super) fn actor_handle_identity(ty: &ResolvedTy) -> Option<&str> {
        let ResolvedTy::Named {
            head: head @ hew_types::TypeHead::Actor(_),
            ..
        } = ty
        else {
            return None;
        };
        Some(head.registry_key())
    }

    /// Lower a lambda actor to an ordinary actor declaration and spawn.
    ///
    /// Checker capture identities select the enclosing spawn inputs. Fresh
    /// state bindings shadow those identities while lowering the handler body.
    #[expect(
        clippy::too_many_lines,
        reason = "lambda actor lowering keeps capture state and handler scope in one operation"
    )]
    pub(super) fn lower_spawn_lambda_actor(
        &mut self,
        params: &[LambdaParam],
        body: &Spanned<Expr>,
        span: &Span,
    ) -> (HirExprKind, ResolvedTy) {
        let actor_ty = self.checked_actor_lambda_handle_ty(span);
        let ResolvedTy::Named { args, .. } = &actor_ty else {
            return (
                HirExprKind::Unsupported("missing actor lambda type".into()),
                actor_ty,
            );
        };
        let reply_ty = args[1].clone();
        let param_tys = match (params.len(), &args[0]) {
            (0, ResolvedTy::Unit) => Vec::new(),
            (1, message) => vec![message.clone()],
            (count, ResolvedTy::Tuple(elements)) if count == elements.len() => elements.clone(),
            _ => {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::CheckerBoundaryViolation {
                        name: "actor lambda".into(),
                        reason: "checked message type does not match parameter arity".into(),
                    },
                    span.clone(),
                    "actor lambda parameters require their checked message types",
                ));
                return (
                    HirExprKind::Unsupported("actor lambda parameter mismatch".into()),
                    actor_ty,
                );
            }
        };
        let Some(facts) = self.closure_capture_facts.get(&self.mk_key(span)).cloned() else {
            self.unsupported(
                span.clone(),
                "missing lambda actor capture facts",
                "hir-lowering",
            );
            return (
                HirExprKind::Unsupported("missing lambda actor capture facts".into()),
                actor_ty,
            );
        };
        let mut captures = Vec::with_capacity(facts.len());
        for fact in &facts {
            let Some((binding, ty)) = self.lookup_checked(fact.binding_id) else {
                self.unsupported(
                    span.clone(),
                    "missing lambda actor capture binding",
                    "hir-lowering",
                );
                return (
                    HirExprKind::Unsupported("missing lambda actor capture binding".into()),
                    actor_ty,
                );
            };
            captures.push(HirLambdaCapture {
                binding,
                name: fact.name.clone(),
                ty,
                kind: if self
                    .current_actor_self
                    .as_ref()
                    .is_some_and(|(id, _)| *id == binding)
                {
                    HirCaptureKind::Weak
                } else {
                    HirCaptureKind::Strong
                },
            });
        }
        self.push_scope();
        let mut state_bindings = Vec::with_capacity(captures.len());
        for (capture, fact) in captures.iter().zip(&facts) {
            let binding = self.bind(
                capture.name.clone(),
                capture.ty.clone(),
                fact.access == hew_types::ClosureCaptureAccess::Var,
                fact.def_span.clone().unwrap_or_else(|| span.clone()),
            );
            self.checked_scopes
                .last_mut()
                .expect("lambda actor scope")
                .insert(
                    fact.binding_id,
                    (binding.id, binding.ty.clone(), binding.span.clone()),
                );
            state_bindings.push(binding);
        }
        let mut hir_params: Vec<HirBinding> = Vec::with_capacity(params.len());
        for (param, ty) in params.iter().zip(param_tys) {
            hir_params.push(self.bind_checked(
                param.name.to_string(),
                ty,
                false,
                param.name_span.clone(),
            ));
        }
        // Lexically scope `current_actor_self` to THIS lambda body. If the
        // caller (`lower_stmt`'s let-pre-bind path) set it before invoking
        // `lower_expr`, that value is this lambda's self-id; otherwise this
        // lambda is in expression position (anonymous) and has no self-id.
        // Take the value out for the duration of the body walk so any
        // nested actor-lambda lowered from within `body` does not inherit
        // it — nested anonymous lambdas would otherwise misclassify
        // captures of THIS lambda's enclosing-scope bindings as Weak.
        // Restore the caller's binding after lowering this body.
        let my_self_id = self.current_actor_self.take();
        let lowered_body = self.with_current_return_type(reply_ty.clone(), |ctx| {
            ctx.lower_expr_with_tail_coercion(body, IntentKind::Read)
        });
        self.current_actor_self = my_self_id;
        self.pop_scope();
        self.synthesize_lambda_actor(
            span,
            hir_params,
            lowered_body,
            captures,
            state_bindings,
            actor_ty,
        )
    }

    /// Build and record the actor declaration one lambda actor lowers to.
    #[expect(
        clippy::too_many_arguments,
        reason = "the checked actor identity, handler and capture state form one declaration"
    )]
    pub(super) fn push_lambda_actor_declaration(
        &mut self,
        span: &Span,
        identity: &hew_types::actor_protocol::LambdaActorIdentity,
        params: Vec<HirBinding>,
        body: HirExpr,
        captures: &[HirLambdaCapture],
        state_bindings: Vec<HirBinding>,
        handle_ty: &ResolvedTy,
    ) {
        // The handle carries the protocol: `actor(Msg) -> Reply`.
        let reply_ty = match handle_ty {
            ResolvedTy::Named { args, .. } if args.len() == 2 => args[1].clone(),
            _ => ResolvedTy::Unit,
        };
        let state_fields: Vec<HirField> = captures
            .iter()
            .zip(&state_bindings)
            .map(|(capture, state)| HirField {
                name: capture.name.clone(),
                ty: capture.ty.clone(),
                default: None,
                is_mutable: state.mutable,
                deferred: false,
                span: span.clone(),
            })
            .collect();
        let param_tys: Vec<ResolvedTy> = params.iter().map(|param| param.ty.clone()).collect();
        let handler_name = LAMBDA_ACTOR_HANDLER.to_string();
        let protocol_descriptor = hew_types::ActorProtocolDescriptor::from_handlers(
            identity.path.clone(),
            &[hew_types::actor_protocol::ActorHandlerSpec {
                declaration: identity.handler,
                wire_identity: String::new(),
                wire: false,
                name: handler_name.clone(),
                param_tys,
                return_ty: reply_ty.clone(),
                symbol: format!("{}__{handler_name}", identity.path),
            }],
        );

        let body_block = HirBlock {
            node: self.ids.node(),
            scope: self.ids.scope(),
            statements: Vec::new(),
            ty: body.ty.clone(),
            tail: Some(Box::new(body)),
            span: span.clone(),
        };
        self.pending_lambda_actors.push(HirActorDecl {
            id: self.ids.item(),
            node: self.ids.node(),
            declaration: identity.actor,
            name: identity.path.clone(),
            defining_module: None,
            type_params: Vec::new(),
            state_fields,
            init: None,
            receive_handlers: vec![HirActorReceiveFn {
                declaration: identity.handler,
                state_bindings,
                name: handler_name,
                is_generator: false,
                params,
                return_ty: reply_ty,
                body: body_block,
                state_guard: HirActorStateGuard::Exclusive,
                every_ns: None,
                span: span.clone(),
            }],
            methods: Vec::new(),
            lifecycle_hooks: Vec::new(),
            max_heap_bytes: None,
            is_isolated: false,
            cycle_capable: false,
            protocol_descriptor,
            lambda_handle_ty: Some(Box::new(handle_ty.clone())),
            span: span.clone(),
        });
    }

    /// Lower `handle(msg)` on a lambda actor to the completion call it is.
    pub(super) fn lower_lambda_actor_call(
        &mut self,
        function: &Spanned<Expr>,
        args: &[CallArg],
        handler: hew_types::DefId,
        reply_ty: &hew_types::Ty,
        policy: hew_types::actor_delivery::SendPolicy,
        span: &Span,
    ) -> (HirExprKind, ResolvedTy) {
        let receiver = self.lower_expr(function, IntentKind::Read);
        let lowered_args = self.lower_positional_call_args(args, span);
        let Ok(reply_ty) = ResolvedTy::from_ty(reply_ty) else {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::CheckerBoundaryViolation {
                    name: "lambda actor call".to_string(),
                    reason: "the recorded reply type is not concrete".to_string(),
                },
                span.clone(),
                "a completion call needs its checked reply type",
            ));
            return (
                HirExprKind::Unsupported("lambda actor call has no reply type".to_string()),
                ResolvedTy::Unit,
            );
        };
        let Some(result_ty) = self.checked_actor_ask_result_ty(span, handler) else {
            return (
                HirExprKind::Unsupported("lambda actor call has no checked result".to_string()),
                ResolvedTy::Unit,
            );
        };
        (
            HirExprKind::ActorAsk {
                receiver: Box::new(receiver),
                handler,
                args: lowered_args,
                evaluation_order: Vec::new(),
                reply_ty,
                policy,
            },
            result_ty,
        )
    }

    /// `mailbox(handle, ..)(msg)` on a lambda actor: build the addressed
    /// message and submit it at the same site, exactly as a `receive fn`
    /// without a reply does through a named actor's mailbox view.
    pub(super) fn lower_lambda_actor_submission(
        &mut self,
        function: &Spanned<Expr>,
        args: &[CallArg],
        handler: hew_types::DefId,
        policy: hew_types::actor_delivery::SendPolicy,
        span: &Span,
    ) -> (HirExprKind, ResolvedTy) {
        let receiver = self.lower_expr(function, IntentKind::Read);
        let lowered_args: Vec<HirExpr> = args
            .iter()
            .map(|arg| {
                let spanned = arg.expr();
                self.lower_expr(spanned, self.actor_message_arg_intent(&spanned.1))
            })
            .collect();
        let Some(ty) = self.checker_expr_ty_if_present(span) else {
            return (
                HirExprKind::Unsupported("message submission has no checked type".into()),
                ResolvedTy::Unit,
            );
        };
        let Some(message_ty) = Self::submitted_message_ty(&ty) else {
            return (
                HirExprKind::Unsupported("message submission has no checked message type".into()),
                ResolvedTy::Unit,
            );
        };

        let message = HirExpr {
            node: self.ids.node(),
            site: self.ids.site(),
            ty: message_ty,
            intent: IntentKind::Consume,
            kind: HirExprKind::ActorMessage {
                receiver: Box::new(receiver),
                handler,
                args: lowered_args,
                evaluation_order: Vec::new(),
                policy,
            },
            span: span.clone(),
        };
        (
            HirExprKind::ActorDelivery {
                receiver: Box::new(message),
                args: Vec::new(),
                operation: hew_types::actor_delivery::ActorDeliveryCall::Submit { policy },
            },
            ty,
        )
    }

    /// Turn one lowered lambda actor into an ordinary actor declaration plus
    /// the spawn that starts it.
    ///
    /// Captured inputs initialize distinct handler state bindings; checker
    /// identities resolve directly to those bindings inside the handler.
    pub(super) fn synthesize_lambda_actor(
        &mut self,
        span: &Span,
        params: Vec<HirBinding>,
        body: HirExpr,
        captures: Vec<HirLambdaCapture>,
        state_bindings: Vec<HirBinding>,
        handle_ty: ResolvedTy,
    ) -> (HirExprKind, ResolvedTy) {
        let Some(identity) = self
            .lambda_actor_declarations
            .get(&SpanKey::from(span))
            .cloned()
        else {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::CheckerBoundaryViolation {
                    name: "lambda actor".to_string(),
                    reason: "the checker minted no declaration identity for this `actor |..|` expression"
                        .to_string(),
                },
                span.clone(),
                "a lambda actor lowers to a synthesized actor declaration, which needs the resolver-minted identity for its span",
            ));
            return (HirExprKind::Literal(HirLiteral::Unit), handle_ty);
        };
        if let Some(weak) = captures
            .iter()
            .find(|capture| capture.kind == HirCaptureKind::Weak)
        {
            // A lambda that names itself is a reference cycle between the
            // handle and its own state seat. The named-actor path has no
            // equivalent, so refuse it rather than synthesize a declaration
            // whose state owns its own handle.
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::RecursiveLambdaActorHandle {
                    name: weak.name.clone(),
                },
                span.clone(),
                "give the recursive actor a name and spawn it, so the handle and the state it reaches are separate declarations",
            ));
            return (HirExprKind::Literal(HirLiteral::Unit), handle_ty);
        }

        self.push_lambda_actor_declaration(
            span,
            &identity,
            params,
            body,
            &captures,
            state_bindings,
            &handle_ty,
        );

        // The spawn supplies the captured environment as the actor's state,
        // field by field, exactly as a named actor's spawn supplies its own.
        let args: Vec<(String, HirExpr)> = captures
            .into_iter()
            .map(|capture| {
                let value = HirExpr {
                    node: self.ids.node(),
                    site: self.ids.site(),
                    ty: capture.ty.clone(),
                    intent: IntentKind::Consume,
                    kind: HirExprKind::BindingRef {
                        name: capture.name.clone(),
                        resolved: ResolvedRef::Binding(capture.binding),
                    },
                    span: span.clone(),
                };
                (capture.name, value)
            })
            .collect();
        // The captures are the synthesized actor's state fields, in order.
        let slots = (0..args.len())
            .map(crate::node::HirSpawnSlot::Written)
            .collect();
        (
            HirExprKind::Spawn {
                actor_name: identity.path,
                args,
                slots,
            },
            handle_ty,
        )
    }
}
