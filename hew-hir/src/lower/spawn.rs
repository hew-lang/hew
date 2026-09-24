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

        let lowered_args = args
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
        // bare `spawn Account()` inside module `bank` resolves to
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
        (
            HirExprKind::Spawn {
                actor_name,
                args: lowered_args,
            },
            ty,
        )
    }

    pub(super) fn actor_handle_identity(ty: &ResolvedTy) -> Option<&str> {
        let ResolvedTy::Named {
            name,
            builtin: Some(BuiltinType::ActorHandle),
            ..
        } = ty
        else {
            return None;
        };
        Some(name)
    }

    /// Lower an `Expr::SpawnLambdaActor` to an
    /// `HirExprKind::SpawnLambdaActor` with a resolved capture set.
    ///
    /// The lambda body lowers inside a fresh scope so the parameter
    /// bindings shadow outer names; after the body is built the
    /// `current_actor_self` field tells us whether the body lives
    /// under a `let <name> = actor |..| { .. }` forward-bind. The
    /// capture walker then collects every `BindingRef { resolved:
    /// Binding(id) }` whose `id` refers to a binding from an outer
    /// scope (not a parameter introduced by this lambda) and
    /// classifies the strength: `id == current_actor_self.0` → Weak
    /// (recursive self-dispatch, §5.9 ratification 2), else → Strong.
    ///
    /// The HIR `expr.ty` is the `ActorFn` handle type (`actor(Msg) -> Reply`),
    /// whose drop releases the runtime wrapper.
    pub(super) fn lower_spawn_lambda_actor(
        &mut self,
        params: &[LambdaParam],
        return_type: Option<&Spanned<TypeExpr>>,
        body: &Spanned<Expr>,
        span: &Span,
    ) -> (HirExprKind, ResolvedTy) {
        let actor_ty = self.actor_lambda_handle_ty(params, return_type);
        let reply_ty = match &actor_ty {
            ResolvedTy::Named { args, .. } if args.len() == 2 => args[1].clone(),
            _ => ResolvedTy::Unit,
        };
        // Lower params + body inside a new scope. Track the parameter
        // BindingIds so the capture walker can exclude them (params
        // are intra-lambda bindings, not captures from the enclosing
        // scope).
        self.push_scope();
        let mut hir_params: Vec<HirBinding> = Vec::with_capacity(params.len());
        let mut param_ids: std::collections::HashSet<BindingId> =
            std::collections::HashSet::with_capacity(params.len());
        for param in params {
            let ty = param
                .ty
                .as_ref()
                .map_or(ResolvedTy::Unit, |ann| self.lower_type(ann));
            let binding = self.bind(param.name.to_string(), ty, false, param.name_span.clone());
            param_ids.insert(binding.id);
            hir_params.push(binding);
        }
        // Lexically scope `current_actor_self` to THIS lambda body. If the
        // caller (`lower_stmt`'s let-pre-bind path) set it before invoking
        // `lower_expr`, that value is this lambda's self-id; otherwise this
        // lambda is in expression position (anonymous) and has no self-id.
        // Take the value out for the duration of the body walk so any
        // nested actor-lambda lowered from within `body` does not inherit
        // it — nested anonymous lambdas would otherwise misclassify
        // captures of THIS lambda's enclosing-scope bindings as Weak.
        // Restored before `collect_lambda_captures` so the capture-strength
        // classifier sees the correct self-id, and restored to the caller's
        // prior value on exit.
        let my_self_id = self.current_actor_self.take();
        let lowered_body = self.with_current_return_type(reply_ty.clone(), |ctx| {
            ctx.lower_expr_with_tail_coercion(body, IntentKind::Read)
        });
        self.current_actor_self = my_self_id;
        self.pop_scope();
        let captures = self.collect_lambda_captures(&lowered_body, &param_ids);
        self.synthesize_lambda_actor(span, hir_params, lowered_body, captures, actor_ty)
    }

    /// Build and record the actor declaration one lambda actor lowers to.
    pub(super) fn push_lambda_actor_declaration(
        &mut self,
        span: &Span,
        identity: &hew_types::actor_protocol::LambdaActorIdentity,
        params: Vec<HirBinding>,
        body: HirExpr,
        captures: &[HirLambdaCapture],
        handle_ty: &ResolvedTy,
    ) {
        // The handle carries the protocol: `actor(Msg) -> Reply`.
        let reply_ty = match handle_ty {
            ResolvedTy::Named { args, .. } if args.len() == 2 => args[1].clone(),
            _ => ResolvedTy::Unit,
        };
        let state_fields: Vec<HirField> = captures
            .iter()
            .map(|capture| HirField {
                name: capture.name.clone(),
                ty: capture.ty.clone(),
                default: None,
                is_mutable: false,
                deferred: false,
                span: span.clone(),
            })
            .collect();
        // The body already refers to each capture by its original binding id;
        // binding the state seat to those same ids is what makes the captured
        // environment and the actor's state one thing.
        let state_bindings: Vec<HirBinding> = captures
            .iter()
            .map(|capture| HirBinding {
                id: capture.binding,
                name: capture.name.clone(),
                ty: capture.ty.clone(),
                mutable: false,
                span: span.clone(),
                is_consume: false,
            })
            .collect();

        let param_tys: Vec<ResolvedTy> = params.iter().map(|param| param.ty.clone()).collect();
        let handler_name = LAMBDA_ACTOR_HANDLER.to_string();
        let protocol_descriptor = hew_types::ActorProtocolDescriptor::from_handlers_with_ids(
            identity.path.clone(),
            &[(
                hew_types::actor_protocol::ActorHandlerSpec {
                    name: handler_name.clone(),
                    param_tys,
                    return_ty: reply_ty.clone(),
                    symbol: format!("{}__{handler_name}", identity.path),
                },
                hew_types::actor_protocol::LAMBDA_ACTOR_MESSAGE_ID,
            )],
        )
        .ok();

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
            mailbox_capacity: None,
            overflow_policy: None,
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
        method_id: &str,
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
        let Some(result_ty) = self.checked_actor_ask_result_ty(span, method_id) else {
            return (
                HirExprKind::Unsupported("lambda actor call has no checked result".to_string()),
                ResolvedTy::Unit,
            );
        };
        (
            HirExprKind::ActorAsk {
                receiver: Box::new(receiver),
                method_id: method_id.to_string(),
                args: lowered_args,
                evaluation_order: Vec::new(),
                reply_ty,
                policy,
                deadline_ns: None,
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
        method_id: &str,
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
        self.try_register_enum_instantiation_ty(&ty, span);
        let message = HirExpr {
            node: self.ids.node(),
            site: self.ids.site(),
            ty: message_ty,
            intent: IntentKind::Consume,
            kind: HirExprKind::ActorMessage {
                receiver: Box::new(receiver),
                method_id: method_id.to_string(),
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
    /// The captures become the actor's state fields in capture order, and the
    /// body becomes its single receive handler. The handler's state bindings
    /// carry the captures' original `BindingId`s, so every reference the body
    /// already lowered resolves to the state seat with no rewriting: a
    /// capture and an actor state field are the same thing to the body.
    pub(super) fn synthesize_lambda_actor(
        &mut self,
        span: &Span,
        params: Vec<HirBinding>,
        body: HirExpr,
        captures: Vec<HirLambdaCapture>,
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

        self.push_lambda_actor_declaration(span, &identity, params, body, &captures, &handle_ty);

        // The spawn supplies the captured environment as the actor's state,
        // field by field, exactly as a named actor's spawn supplies its own.
        let args = captures
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
        (
            HirExprKind::Spawn {
                actor_name: identity.path,
                args,
            },
            handle_ty,
        )
    }

    /// Walk a lowered lambda body collecting `BindingRef`s that resolve
    /// to bindings from the enclosing scope. A reference is a capture
    /// when its resolved binding id is not in `param_ids` (the lambda's
    /// own parameters). Each unique binding is classified Weak when
    /// its id matches `current_actor_self.0` (the let-name pre-bound
    /// before body lowering) and Strong otherwise.
    ///
    /// Duplicate references to the same binding produce a single
    /// capture entry — codegen needs the runtime to register the
    /// captured handle once per binding, not once per use site.
    pub(super) fn collect_lambda_captures(
        &self,
        body: &HirExpr,
        param_ids: &std::collections::HashSet<BindingId>,
    ) -> Vec<HirLambdaCapture> {
        let mut seen: std::collections::HashSet<BindingId> = std::collections::HashSet::new();
        let mut captures: Vec<HirLambdaCapture> = Vec::new();
        let self_id = self.current_actor_self.as_ref().map(|(id, _)| *id);
        collect_captures_walk(body, param_ids, &mut seen, &mut captures, self_id);
        captures
    }
}
