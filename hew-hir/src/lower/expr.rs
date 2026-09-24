//! Expression lowering.

use super::*;

impl LowerCtx {
    pub(super) fn lower_expr(&mut self, expr: &Spanned<Expr>, intent: IntentKind) -> HirExpr {
        let mut lowered = self.lower_expr_with_tail_coercion(expr, intent);
        let normalized_ty = self.qualify_current_module_record_ty(lowered.ty.clone());
        lowered.ty = normalized_ty;

        if let Some(target) = self.numeric_operand_coercions.get(&self.mk_key(&expr.1)) {
            if let Ok(to_ty) = ResolvedTy::from_ty(target) {
                if lowered.ty != to_ty {
                    return HirExpr {
                        node: self.ids.node(),
                        site: self.ids.site(),
                        ty: to_ty.clone(),
                        intent,
                        span: expr.1.clone(),
                        kind: HirExprKind::NumericCast {
                            from_ty: lowered.ty.clone(),
                            to_ty,
                            value: Box::new(lowered),
                        },
                    };
                }
            }
        }
        lowered
    }

    /// Apply the checker's explicit function-tail Result coercion.
    pub(super) fn lower_expr_with_tail_coercion(
        &mut self,
        expr: &Spanned<Expr>,
        intent: IntentKind,
    ) -> HirExpr {
        let lowered = self.lower_expr_inner(expr, intent);

        // Function-tail Ok-coercion: the checker marked this tail expression's
        // span when a `Result<Ok, Err>`-returning function's tail yields the
        // `Ok` payload (e.g. `db.find(id)?` typed `User` under
        // `-> Result<User, E>`). Wrap the lowered value in `Ok(..)` so the
        // function returns the declared `Result`. The marker is keyed by the
        // tail span and is set only at genuine tail positions, so this fires
        // exactly once per coerced tail and never on a non-tail sub-expression.
        if self.tail_ok_coercions.contains(&self.mk_key(&expr.1)) {
            self.wrap_tail_ok(lowered, &expr.1)
        } else {
            lowered
        }
    }

    pub(super) fn subsumed_value(
        &mut self,
        site: SiteId,
        span: &Span,
        intent: IntentKind,
        source: HirExpr,
    ) -> HirExpr {
        let ty = source.ty.clone();
        HirExpr {
            node: self.ids.node(),
            site,
            ty,
            intent,
            kind: HirExprKind::SubsumedValue {
                source: Box::new(source),
            },
            span: span.clone(),
        }
    }

    #[allow(
        clippy::too_many_lines,
        clippy::single_match_else,
        reason = "single large match on expr variants; splitting would hurt readability"
    )]
    pub(super) fn lower_expr_inner(&mut self, expr: &Spanned<Expr>, intent: IntentKind) -> HirExpr {
        let span = expr.1.clone();
        if let Some(input) = self.fork_input(&span, intent) {
            return input;
        }
        // `self.count` inside an actor body names the state binding `count`.
        // The checker resolved the projection to that binding and published the
        // span, so rewrite the receiver spelling to the bare name and lower it
        // through the identifier shell: both spellings then produce one binding
        // reference, and MIR sees one place.
        if let Expr::FieldAccess { field, .. } = &expr.0 {
            if self.actor_self_state_fields.contains(&self.mk_key(&span)) {
                let bare = Expr::Identifier(field.clone());
                return self.lower_expr_inner(&(bare, span), intent);
            }
        }
        if let Expr::Call {
            function,
            type_args,
            args,
            is_tail_call,
        } = &expr.0
        {
            if let Expr::FieldAccess { object, field } = &function.0 {
                if let Expr::Identifier(owner) = &object.0 {
                    if hew_types::lookup_builtin_type(owner).is_some() {
                        let compatibility = Expr::Call {
                            function: Box::new((
                                Expr::Identifier(format!("{owner}::{field}")),
                                function.1.clone(),
                            )),
                            type_args: type_args.clone(),
                            args: args.clone(),
                            is_tail_call: *is_tail_call,
                        };
                        return self.lower_expr_inner(&(compatibility, span), intent);
                    }
                }
            }
        }
        if let Expr::FieldAccess { object, field } = &expr.0 {
            if let Expr::Identifier(owner) = &object.0 {
                let qualified = format!("{owner}::{field}");
                let checker_ty = self.checker_expr_ty_if_present(&span);
                if matches!(
                    self.lookup_variant_ctor(&qualified, checker_ty.as_ref()),
                    Some((_, _, HirVariantKind::Unit))
                ) {
                    return self.lower_expr_inner(&(Expr::Identifier(qualified), span), intent);
                }
            }
        }
        if let Expr::MethodCall {
            receiver,
            method,
            args,
        } = &expr.0
        {
            if matches!(
                self.method_call_rewrites.get(&self.mk_key(&span)),
                Some(MethodCallRewrite::VecFrom)
            ) {
                if let [arg] = args.as_slice() {
                    let site = self.ids.site();
                    let lowered = self.lower_expr(arg.expr(), IntentKind::Consume);
                    return self.subsumed_value(site, &span, intent, lowered);
                }
            }
            if let Expr::GenericApplySuffix { target, type_args } = &receiver.0 {
                if !self.method_call_rewrites.contains_key(&self.mk_key(&span)) {
                    if let Expr::Identifier(owner) = &target.0 {
                        let compatibility = Expr::Call {
                            function: Box::new((
                                Expr::Identifier(format!("{owner}::{method}")),
                                target.1.clone(),
                            )),
                            type_args: Some(type_args.clone()),
                            args: args.clone(),
                            is_tail_call: false,
                        };
                        return self.lower_expr_inner(&(compatibility, span), intent);
                    }
                }
            }
            if let Expr::Identifier(owner) = &receiver.0 {
                let qualified = format!("{owner}::{method}");
                let checker_ty = self.checker_expr_ty_if_present(&span);
                if !self
                    .method_call_receiver_kinds
                    .contains_key(&self.mk_key(&span))
                    && matches!(
                        self.lookup_variant_ctor(&qualified, checker_ty.as_ref()),
                        Some((_, _, HirVariantKind::Tuple(_)))
                    )
                {
                    let compatibility = Expr::Call {
                        function: Box::new((Expr::Identifier(qualified), receiver.1.clone())),
                        type_args: None,
                        args: args.clone(),
                        is_tail_call: false,
                    };
                    return self.lower_expr_inner(&(compatibility, span), intent);
                }
            }
        }
        if let Expr::ContextVariant(context) = &expr.0 {
            let owner = self
                .checker_expr_ty_if_present(&span)
                .and_then(|ty| match ty {
                    ResolvedTy::Named { name, .. } => Some(name),
                    _ => None,
                });
            let contextual_name = owner.map_or_else(
                || context.name.clone(),
                |owner| format!("{owner}::{}", context.name),
            );
            let compatibility = if let Some(record) = &context.record {
                Expr::StructInit {
                    name: contextual_name,
                    fields: record.fields.clone(),
                    type_args: None,
                    base: record.base.clone(),
                }
            } else {
                Expr::Identifier(contextual_name)
            };
            return self.lower_expr_inner(&(compatibility, span), intent);
        }
        if let Expr::GenericApplySuffix { target, .. } = &expr.0 {
            return self.lower_expr(&(target.0.clone(), span), intent);
        }
        // Pre-allocate the SiteId for this expression so call-site
        // side-tables (e.g. `call_site_type_args`) can be keyed
        // by the eventual HirExpr.site before the wrapping struct is
        // built. Allocation order: site before node so existing
        // SiteId counts in tests stay stable (lower_expr previously
        // allocated node before site at the same call).
        let site = self.ids.site();
        if let Some(operations) = self
            .checked_indexed_place_operations
            .get(&self.mk_key(&span))
        {
            self.indexed_place_operations.insert(site, *operations);
        }
        if let Some(operation) = self.actor_delivery_calls.get(&self.mk_key(&span)).cloned() {
            use hew_types::actor_delivery::ActorDeliveryCall;
            let (receiver, args) = match (&operation, &expr.0) {
                (
                    ActorDeliveryCall::Close | ActorDeliveryCall::AwaitClosed,
                    Expr::Call { args, .. },
                ) if args.len() == 1 => (
                    self.lower_expr(args[0].expr(), IntentKind::Read),
                    Vec::new(),
                ),
                (
                    ActorDeliveryCall::Close
                    | ActorDeliveryCall::AwaitClosed
                    | ActorDeliveryCall::Stop,
                    Expr::MethodCall { receiver, args, .. },
                ) if args.is_empty() => (self.lower_expr(receiver, IntentKind::Read), Vec::new()),
                // `view(actor)` derives its admission from the destination's
                // declaration; `view(actor, on_full: ..)` overrides it. The
                // checker resolved both to one policy, so only the target
                // survives into HIR.
                (ActorDeliveryCall::Policy { .. }, Expr::Call { args, .. })
                    if matches!(args.len(), 1 | 2) =>
                {
                    (
                        self.lower_expr(args[0].expr(), IntentKind::Read),
                        Vec::new(),
                    )
                }
                (
                    ActorDeliveryCall::Readdress { .. } | ActorDeliveryCall::Resume { .. },
                    Expr::MethodCall { receiver, args, .. },
                ) => (
                    self.lower_expr(receiver, IntentKind::Consume),
                    args.iter()
                        .map(|arg| self.lower_expr(arg.expr(), IntentKind::Read))
                        .collect(),
                ),
                (ActorDeliveryCall::Submit { .. }, Expr::MethodCall { receiver, args, .. })
                    if args.is_empty() =>
                {
                    (self.lower_expr(receiver, IntentKind::Consume), Vec::new())
                }
                _ => {
                    return self.unsupported_expr(
                        span,
                        "actor delivery operation disagrees with its checked source",
                    )
                }
            };
            let Some(ty) = self.checker_expr_ty_if_present(&span) else {
                return self.unsupported_expr(span, "actor delivery operation has no checked type");
            };
            self.try_register_enum_instantiation_ty(&ty, &span);
            // `.to(actor)` readdresses AND resubmits in one call: the
            // readdressed description is an internal temporary that never
            // reaches a binding, so it is built and submitted here.
            if let ActorDeliveryCall::Readdress { policy, .. } = operation {
                let Some(message_ty) = Self::submitted_message_ty(&ty) else {
                    return self.unsupported_expr(
                        span,
                        "readdressed submission has no checked message type",
                    );
                };
                let readdressed = HirExpr {
                    node: self.ids.node(),
                    site,
                    ty: message_ty,
                    intent: IntentKind::Consume,
                    kind: HirExprKind::ActorDelivery {
                        receiver: Box::new(receiver),
                        args,
                        operation,
                    },
                    span: span.clone(),
                };
                return HirExpr {
                    node: self.ids.node(),
                    site: self.ids.site(),
                    ty,
                    intent,
                    kind: HirExprKind::ActorDelivery {
                        receiver: Box::new(readdressed),
                        args: Vec::new(),
                        operation: ActorDeliveryCall::Submit { policy },
                    },
                    span,
                };
            }
            // `pid.stop()` is the stop request alone: the same request
            // `close` makes, with the handle it yields discarded, so the call
            // is `()` and nothing waits.
            if let ActorDeliveryCall::Stop = operation {
                let request = HirExpr {
                    node: self.ids.node(),
                    site,
                    ty: receiver.ty.clone(),
                    intent: IntentKind::Read,
                    kind: HirExprKind::ActorDelivery {
                        receiver: Box::new(receiver),
                        args,
                        operation: ActorDeliveryCall::Close,
                    },
                    span: span.clone(),
                };
                return HirExpr {
                    node: self.ids.node(),
                    site: self.ids.site(),
                    ty,
                    intent,
                    kind: HirExprKind::Block(HirBlock {
                        node: self.ids.node(),
                        scope: self.ids.scope(),
                        statements: vec![HirStmt {
                            node: self.ids.node(),
                            kind: HirStmtKind::Expr(request),
                            span: span.clone(),
                        }],
                        tail: None,
                        ty: ResolvedTy::Unit,
                        span: span.clone(),
                    }),
                    span,
                };
            }
            // `close(actor)` requests the stop AND waits for terminal cleanup:
            // the request yields the same handle back, and the wait consumes it.
            // `fork close(actor)` is how the request runs without waiting, and
            // `closed(actor)` is the wait on its own.
            if let ActorDeliveryCall::Close = operation {
                let handle_ty = receiver.ty.clone();
                let requested = HirExpr {
                    node: self.ids.node(),
                    site,
                    ty: handle_ty,
                    intent: IntentKind::Read,
                    kind: HirExprKind::ActorDelivery {
                        receiver: Box::new(receiver),
                        args,
                        operation,
                    },
                    span: span.clone(),
                };
                return HirExpr {
                    node: self.ids.node(),
                    site: self.ids.site(),
                    ty,
                    intent,
                    kind: HirExprKind::ActorDelivery {
                        receiver: Box::new(requested),
                        args: Vec::new(),
                        operation: ActorDeliveryCall::AwaitClosed,
                    },
                    span,
                };
            }
            return HirExpr {
                node: self.ids.node(),
                site,
                ty,
                intent,
                kind: HirExprKind::ActorDelivery {
                    receiver: Box::new(receiver),
                    args,
                    operation,
                },
                span,
            };
        }
        let (kind, ty) = match &expr.0 {
            Expr::Literal(lit) => {
                let (kind, default_ty) = Self::lower_literal(lit);
                // Apply checker authority for integer literals: the checker may have
                // recorded a concrete width (e.g. `I32`) via `check_against` when the
                // literal appears in a comparison with a fixed-width integer
                // (e.g. `register("w", pid) == 0`). Without this override the literal
                // always defaults to `I64`, causing `IntCmp` to see mismatched widths.
                // If checker recorded `IntLiteral` (unconstrained) or no entry exists,
                // `from_ty` returns Err and we fall back to the `lower_literal` default.
                let ty = {
                    let checker_key = self.mk_key(&span);
                    if let Some(checker_ty) = self.expr_types.get(&checker_key) {
                        ResolvedTy::from_ty(checker_ty).unwrap_or(default_ty)
                    } else {
                        default_ty
                    }
                };
                // W4.047 P1.2: prove the typed handoff agrees with the live
                // path at this fail-open literal site (no behaviour change).
                self.assert_resolved_ty_totality(&span);
                (kind, ty)
            }
            Expr::RegexLiteral(pattern) => {
                // A standalone `re"..."` expression. Allocate (or reuse) the
                // module-level literal-table entry. The checker-assigned type is
                // `std.text.regex.Pattern`; read it from `expr_types` if present (it is
                // set by `synthesize_inner` for `Expr::RegexLiteral`), otherwise
                // fall back to the canonical `Named` form. Using the checker's
                // resolved type rather than hard-coding keeps capture / generic
                // resolution consistent with the rest of the pipeline.
                let checker_key = self.mk_key(&span);
                let resolved_ty = if let Some(ty) = self.expr_types.get(&checker_key).cloned() {
                    ResolvedTy::from_ty(&ty).unwrap_or(ResolvedTy::Named {
                        name: "std.text.regex.Pattern".to_string(),
                        args: Vec::new(),
                        builtin: None,
                        is_opaque: false,
                    })
                } else {
                    ResolvedTy::Named {
                        name: "std.text.regex.Pattern".to_string(),
                        args: Vec::new(),
                        builtin: None,
                        is_opaque: false,
                    }
                };
                // W4.047 P1.2: prove the typed handoff agrees with the live
                // path at this fail-open regex-literal site (no behaviour change).
                self.assert_resolved_ty_totality(&span);
                // No named captures from a standalone literal — captures are
                // resolved per-arm in the match-arm context. Pass an empty
                // capture list; the table deduplicates by pattern only.
                let literal_id = self.alloc_regex_literal(pattern, &[]);
                (
                    HirExprKind::RegexLiteralRef {
                        literal_id,
                        pattern: pattern.clone(),
                        captures: Vec::new(),
                    },
                    resolved_ty,
                )
            }
            Expr::Identifier(name) if name == "self" && self.lookup(name).is_none() => {
                // Bare `self` inside an actor `receive fn` — the actor's own
                // handle, whose type is the actor. The checker records it in
                // `expr_types`, but ONLY inside an actor; elsewhere it reports
                // an undefined variable and records nothing usable. HIR is
                // checker-authoritative here: it READS that recorded type, it
                // does NOT re-derive the actor identity from the AST. A
                // `self.field` access is intercepted earlier and never reaches
                // here, and an impl/trait method's `self` receiver is a real
                // binding, so `lookup` resolves it before this arm applies.
                let checker_key = self.mk_key(&span);
                match self.expr_types.get(&checker_key).cloned() {
                    Some(ty) => match ResolvedTy::from_ty(&ty) {
                        Ok(
                            resolved @ ResolvedTy::Named {
                                builtin: Some(BuiltinType::ActorHandle),
                                ..
                            },
                        ) => (HirExprKind::ActorSelf, resolved),
                        // The checker recorded a type for `self` that is not an
                        // actor handle. The only authoritative producer is the
                        // actor-handler synthesis (`Self` is the actor type);
                        // anything else is a boundary violation — fail closed,
                        // never fabricate a self-handle.
                        Ok(other) => {
                            self.diagnostics.push(HirDiagnostic::new(
                                HirDiagnosticKind::CheckerBoundaryViolation {
                                    name: "self".to_string(),
                                    reason: format!(
                                        "expected the actor's own handle type recorded by \
                                         the checker, got `{}`",
                                        other.user_facing()
                                    ),
                                },
                                span.clone(),
                                "`self` is the actor self-handle; its checker type must be \
                                 `Self`, the actor's own type",
                            ));
                            return self
                                .unsupported_expr(span, "`self` with a non-actor-handle type");
                        }
                        Err(err) => {
                            self.diagnostics.push(HirDiagnostic::new(
                                HirDiagnosticKind::CheckerBoundaryViolation {
                                    name: "self".to_string(),
                                    reason: err.to_string(),
                                },
                                span.clone(),
                                "`self` self-handle type failed the checker boundary conversion",
                            ));
                            return self.unsupported_expr(span, "`self` type boundary conversion");
                        }
                    },
                    // No recorded type means the checker did not synthesize a
                    // handle here; its diagnostic already fired. Fail closed
                    // without papering over it.
                    None => {
                        return self
                            .unsupported_expr(span, "`self` outside an actor receive handler");
                    }
                }
            }
            Expr::Identifier(name) => {
                // Inside a machine body, check if the identifier names one of the
                // enclosing machine's states (unit state ctor, e.g. `Green`).
                // If so, produce `MachineVariantCtor` rather than going through
                // `lower_identifier` (which would emit `UnresolvedSymbol` because
                // state names are not in the HIR scope).
                //
                // HIR-side authority: the type checker does not record a side-table
                // entry for this expression; the result type is derived from the
                // machine declaration context held in `current_machine_states`.
                self.lower_identifier(name, span.clone(), site)
            }
            Expr::ContextVariant(_) | Expr::GenericApplySuffix { .. } => {
                unreachable!("compatibility suffix expressions are lowered before site allocation")
            }
            Expr::RecordInitSuffix {
                target,
                fields,
                base,
            } => {
                self.lower_expr(target, IntentKind::Read);
                for (_, value) in fields {
                    self.lower_expr(value, IntentKind::Read);
                }
                if let Some(base) = base {
                    self.lower_expr(base, IntentKind::Read);
                }
                self.unsupported(
                    span.clone(),
                    "qualified record initializer",
                    "qualified-record-init",
                );
                (
                    HirExprKind::Unsupported("unsupported qualified record initializer".into()),
                    ResolvedTy::Unit,
                )
            }
            Expr::QualifiedAssoc(path) => {
                let _ = render_type_expr(&path.base.0);
                self.unsupported(
                    span.clone(),
                    "qualified associated value",
                    "qualified-assoc-value",
                );
                (
                    HirExprKind::Unsupported("unsupported qualified associated value".into()),
                    ResolvedTy::Unit,
                )
            }
            Expr::Binary { left, op, right } => {
                let left = self.lower_expr(left, IntentKind::Read);
                let right = self.lower_expr(right, IntentKind::Read);
                // D340: a user `impl Eq`/`impl Ord`/`impl PartialOrd` for the
                // operand type overrides the compiler's structural default —
                // dispatch to the resolved impl method instead of the
                // structural comparison this arm otherwise builds below.
                let dispatch_key = self.mk_key(&span);
                if let Some(dispatch) = self.user_comparison_dispatch.get(&dispatch_key).cloned() {
                    let call = self.lower_user_comparison_dispatch(
                        &dispatch,
                        *op,
                        left,
                        right,
                        span.clone(),
                    );
                    return HirExpr {
                        node: self.ids.node(),
                        site,
                        ty: call.ty.clone(),
                        intent,
                        kind: call.kind,
                        span,
                    };
                }
                let Some(checked_ty) = self.expr_types.get(&dispatch_key) else {
                    return self.unsupported_expr(span, "binary expression has no checked type");
                };
                let Ok(ty) = ResolvedTy::from_ty(checked_ty) else {
                    return self.unsupported_expr(span, "binary expression type is unresolved");
                };
                (
                    HirExprKind::Binary {
                        op: *op,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    ty,
                )
            }
            Expr::Unary { op, operand } => self.lower_unary_expr(*op, operand, &span),
            Expr::Call { function, args, .. } => {
                match self.lower_call_expr(function, args, span.clone(), site, intent) {
                    Ok(lowered) => lowered,
                    Err(done) => return *done,
                }
            }
            Expr::Block(block) if block.stmts.is_empty() && block.trailing_expr.is_none() => {
                if self
                    .checker_expr_ty_if_present(&span)
                    .is_some_and(|ty| Self::is_hashmap_ty(&ty))
                {
                    self.lower_map_literal(&[], &span)
                } else {
                    let block = self.lower_block(block, &ResolvedTy::Unit);
                    let ty = block.ty.clone();
                    (HirExprKind::Block(block), ty)
                }
            }
            Expr::Block(block) => {
                let block = self.lower_block(block, &ResolvedTy::Unit);
                let ty = block.ty.clone();
                (HirExprKind::Block(block), ty)
            }
            Expr::If {
                condition,
                then_block,
                else_block,
            } => {
                let condition = self.lower_expr(condition, IntentKind::Read);
                let then_expr = self.lower_expr(then_block, IntentKind::Read);
                let else_expr = else_block
                    .as_ref()
                    .map(|expr| Box::new(self.lower_expr(expr, IntentKind::Read)));
                // Mirror the checker's `unify_branches`: a diverging branch
                // (`Never`) must not set the construct's value type, so the
                // construct takes the value branch's type. Without this an
                // unannotated `let x = if c { v } else { return … }` would carry
                // the else's `Never` and break a later `x + 1` at MIR lowering.
                let ty = if_branch_result_ty(&then_expr.ty, else_expr.as_ref().map(|e| &e.ty));
                let ty = self.callable_join_type(&span, ty);
                (
                    HirExprKind::If {
                        condition: Box::new(condition),
                        then_expr: Box::new(then_expr),
                        else_expr,
                    },
                    ty,
                )
            }
            Expr::StructInit {
                name,
                fields,
                // Surface-level explicit type arguments (`Box<i64> { ... }`).
                // The HIR-recorded `type_args` below comes from the checker's
                // `record_init_type_args` side-table (which already reconciled
                // inferred-vs-explicit and substituted enclosing-fn type-params
                // where applicable), so we ignore the raw surface args here.
                type_args: _,
                base,
            } => {
                // Inside a machine body, check if the struct-init name is a state
                // with payload fields (e.g. `SynReceived { remote_port: remote_port }`).
                // Resolve to `MachineVariantCtor` before the record-layout path.
                //
                // HIR-side authority: same deviation as `MachineVariantCtor` for bare
                // identifiers — the checker has no side-table for state-ctor sites.
                // Payload fields are validated structurally (field names matched against
                // the state's declared fields). The `base` functional-update form is not
                // supported for machine state ctors and is rejected below if present.
                // Enum struct-variant ctor (`Shape::Box { w: 3, h: 4 }`).
                // Looks like a struct literal but resolves to a registered
                // enum variant in `enum_variants_by_name`. Resolved before
                // the machine-state path so a qualified `Shape::Box` is
                // routed correctly even outside any machine body.
                let checker_ctor_ty = self.checker_expr_ty_if_present(&span);
                let enum_struct_variant = if let Some((type_name, variant_idx, kind)) =
                    self.lookup_variant_ctor(name, checker_ctor_ty.as_ref())
                {
                    match kind {
                        HirVariantKind::Struct(_) => Some((type_name, variant_idx, kind.clone())),
                        // Unit / tuple variants written with struct-literal
                        // syntax are a shape mismatch; report and let the
                        // regular-record path handle the fallthrough so
                        // downstream diagnostics still surface.
                        HirVariantKind::Unit | HirVariantKind::Tuple(_) => {
                            self.diagnostics.push(HirDiagnostic::new(
                                HirDiagnosticKind::EnumVariantConstructorShapeMismatch {
                                    variant: name.clone(),
                                },
                                span.clone(),
                                "this variant has no named fields; use the matching call or identifier form",
                            ));
                            None
                        }
                    }
                } else {
                    None
                };
                if let Some((type_name, variant_idx, HirVariantKind::Struct(field_decls))) =
                    enum_struct_variant
                {
                    if base.is_some() {
                        self.diagnostics.push(HirDiagnostic::new(
                            HirDiagnosticKind::NotYetImplemented {
                                construct: "functional-update syntax on enum variant constructors"
                                    .to_string(),
                                owning_pass: "variant ctor validation".to_string(),
                            },
                            span.clone(),
                            "enum variant constructors do not support `..base` syntax",
                        ));
                    }
                    // Validate source-declared field names against the
                    // variant's declared field set (mirrors the
                    // assignment-target-authority LESSONS row: HIR does not
                    // re-derive type identity from the AST, but it does
                    // validate field-name coverage against the lowered
                    // descriptor). MIR stores by `field_idx`, so we reorder
                    // source fields into declaration order.
                    let mut hir_payload: Vec<(String, HirExpr)> =
                        Vec::with_capacity(field_decls.len());
                    for (field_name, _field_ty) in &field_decls {
                        if let Some((_, src_expr)) = fields.iter().find(|(n, _)| n == field_name) {
                            hir_payload.push((
                                field_name.clone(),
                                self.lower_expr(src_expr, IntentKind::Read),
                            ));
                        } else {
                            self.diagnostics.push(HirDiagnostic::new(
                                HirDiagnosticKind::EnumVariantConstructorMissingField {
                                    variant: name.clone(),
                                    field: field_name.clone(),
                                },
                                span.clone(),
                                "enum struct-variant constructor must initialise every declared field",
                            ));
                        }
                    }
                    // Flag unknown field names for checker-stream coverage;
                    // the canonical struct-init expr's checker entry covers
                    // most cases but the variant-ctor branch is HIR-side.
                    for (fname, src_expr) in fields {
                        if !field_decls.iter().any(|(n, _)| n == fname) {
                            // Lower the expression for coverage even though
                            // we discard it.
                            let _ = self.lower_expr(src_expr, IntentKind::Read);
                            self.diagnostics.push(HirDiagnostic::new(
                                HirDiagnosticKind::EnumVariantConstructorUnknownField {
                                    variant: name.clone(),
                                    field: fname.clone(),
                                },
                                span.clone(),
                                "extra field in enum struct-variant constructor",
                            ));
                        }
                    }
                    // Register the generic-enum instantiation before building
                    // result_ty so codegen's mangled-key lookup finds the entry.
                    self.try_register_enum_instantiation(&span);
                    // Checker-authoritative result type: the checker records the
                    // full `Named { name: "Maybe", args: [I64] }` at the
                    // struct-init expression span. Using it preserves type args
                    // so codegen computes the mangled registry key.
                    // Fall back to bare-name with a diagnostic if expr_types
                    // has no entry or boundary conversion fails.
                    let checker_key = self.mk_key(&span);
                    let result_ty = if let Some(ty) = self.expr_types.get(&checker_key).cloned() {
                        match ResolvedTy::from_ty(&ty) {
                            Ok(resolved) => self.qualify_current_module_record_ty(resolved),
                            Err(err) => {
                                self.diagnostics.push(HirDiagnostic::new(
                                    HirDiagnosticKind::CheckerBoundaryViolation {
                                        name: type_name.clone(),
                                        reason: err.to_string(),
                                    },
                                    span.clone(),
                                    "checker-authoritative struct-variant result type failed boundary conversion",
                                ));
                                ResolvedTy::Named {
                                    name: type_name.clone(),
                                    args: Vec::new(),
                                    builtin: None,
                                    is_opaque: false,
                                }
                            }
                        }
                    } else {
                        ResolvedTy::Named {
                            name: type_name.clone(),
                            args: Vec::new(),
                            builtin: None,
                            is_opaque: false,
                        }
                    };
                    (
                        HirExprKind::MachineVariantCtor {
                            machine_name: type_name,
                            state_idx: variant_idx,
                            payload: Some(hir_payload),
                        },
                        result_ty,
                    )
                } else {
                    // Not a machine state — regular record init path.
                    // Record the per-instantiation `RecordLayout` for
                    // generic user records and capture the concrete type-args
                    // for propagation onto this expression's resolved type.
                    // `None` is legitimate for monomorphic/builtin records
                    // (args: [] is correct).  For generic records with a
                    // checker-accepted span but missing type-arg entry,
                    // `record_record_layout` emits `RecordLayoutMissing`
                    // before returning `None` — fail-closed, not pretend.
                    // The surface constructor spelling can be a bare local
                    // name while the declaration is owned by a nested module.
                    // Select the current source owner before looking up the
                    // generic layout entry; the bare registry key is only a
                    // compatibility alias and may name a same-leaf sibling.
                    let record_identity = self
                        .expr_types
                        .get(&self.mk_key(&span))
                        .and_then(|ty| match ty {
                            Ty::Named {
                                name,
                                builtin: None,
                                ..
                            } if self.record_registry.contains_key(name) => Some(name.clone()),
                            _ => None,
                        })
                        .unwrap_or_else(|| self.canonical_current_module_record_name(name));
                    let resolved_type_args = self
                        .record_record_layout(&record_identity, &span)
                        .unwrap_or_default();
                    let hir_fields = fields
                        .iter()
                        .map(|(fname, expr)| {
                            (fname.clone(), self.lower_expr(expr, IntentKind::Read))
                        })
                        .collect();
                    // Lower the functional-update base if present. The checker has
                    // already validated type-compatibility and field coverage; HIR
                    // carries it verbatim so MIR lowering (A-7) can read the
                    // un-overridden fields from the base value.
                    let hir_base =
                        base.as_deref().map(|(base_expr, base_span)| {
                            Box::new(self.lower_expr(
                                &(base_expr.clone(), base_span.clone()),
                                IntentKind::Read,
                            ))
                        });
                    // A bare construction (`Widget { … }`) constrained by a
                    // module-qualified expected type is checked against the
                    // qualified type def, and the checker records the QUALIFIED
                    // `Named { name: "widgeti8.Widget" }` at this span. Carry
                    // that qualifier onto the init expression's type so MIR can
                    // resolve the per-module layout when two packages export a
                    // same-bare-name type; a single-module construction records
                    // the bare name and keeps `name` byte-identically. For a
                    // non-colliding qualified reference MIR keeps the bare layout
                    // key and codegen resolves the qualified name to the single
                    // bare struct by short-name, so this is safe even when the
                    // type does not collide.
                    //
                    // The recorded short name does NOT always equal the syntactic
                    // construction name. For an ALIASED import (`import m::{
                    // Payload as Tag }; Tag { … }`) the checker resolves `Tag`
                    // through `published_bare_type_qualified` to the SOURCE
                    // identity `m.Payload` and records `Named { name: "m.Payload"
                    // }` — short form `Payload`, not the binding `Tag`. Keying
                    // MIR's field-order lookup off the bare binding `Tag` finds no
                    // registered record (the layout is registered under the source
                    // `Payload`), tripping the field-order fail-closed. So adopt
                    // the checker-recorded qualified identity whenever its short
                    // form is a registered record — the same-name qualified case
                    // (`short == name`) and the aliased case (`short` names the
                    // source record) both route through the source identity, never
                    // the bare binding. The `record_registry` membership check
                    // mirrors the checker's `type_defs.contains_key(qualified)`
                    // guard in `published_bare_type_qualified`, so an unrelated or
                    // unregistered qualified name is never adopted.
                    let result_name = self
                        .expr_types
                        .get(&self.mk_key(&span))
                        .and_then(|ty| match ty {
                            Ty::Named {
                                name: recorded,
                                builtin: None,
                                ..
                            } if recorded.contains('.') => {
                                let short = hew_types::short_name(recorded);
                                (short == name.as_str() || self.record_registry.contains_key(short))
                                    .then(|| recorded.clone())
                            }
                            _ => None,
                        })
                        .unwrap_or_else(|| name.clone());
                    let result_name = self.canonical_current_module_record_name(&result_name);
                    let result_name = if name == "NodeConfig" {
                        "std.builtins.NodeConfig".to_string()
                    } else {
                        result_name
                    };
                    (
                        HirExprKind::StructInit {
                            name: result_name.clone(),
                            type_args: resolved_type_args.clone(),
                            fields: hir_fields,
                            base: hir_base,
                        },
                        ResolvedTy::Named {
                            name: result_name,
                            args: resolved_type_args,
                            builtin: None,
                            is_opaque: false,
                        },
                    )
                }
            }
            Expr::Scope { body } => {
                let Some(checked_ty) = self.expr_types.get(&self.mk_key(&span)) else {
                    return self.unsupported_expr(span, "scope has no checked result type");
                };
                let Ok(result_ty) = ResolvedTy::from_ty(checked_ty) else {
                    return self.unsupported_expr(span, "scope result type is unresolved");
                };
                self.scope_depth += 1;
                let hir_body = self.lower_block(body, &result_ty);
                self.scope_depth -= 1;
                (HirExprKind::Scope { body: hir_body }, result_ty)
            }
            Expr::ForkChild { expr } => {
                let array_branches: Option<Vec<Spanned<Expr>>> = match &expr.0 {
                    Expr::Array(elements) => Some(
                        elements
                            .iter()
                            .map(|element| element.expr().clone())
                            .collect(),
                    ),
                    _ => None,
                };
                if let Some(branches) = array_branches.as_deref().or(match &expr.0 {
                    Expr::Tuple(branches) => Some(branches.as_slice()),
                    _ => None,
                }) {
                    let Some(Ty::Task(output)) = self.expr_types.get(&self.mk_key(&span)) else {
                        return self
                            .unsupported_expr(span, "fork batch has no checked task result");
                    };
                    let Ok(output_ty) = ResolvedTy::from_ty(output) else {
                        return self.unsupported_expr(span, "fork batch result type is unresolved");
                    };
                    let batch = self.lower_fork_batch(branches, output_ty, span.clone());
                    (batch.kind, batch.ty)
                } else {
                    let child = self.lower_fork_invocation(expr);
                    (child.kind, child.ty)
                }
            }

            Expr::ForkBlock { body } => {
                let checker_key = self.mk_key(&span);
                let Some(Ty::Task(output)) = self.expr_types.get(&checker_key) else {
                    return self.unsupported_expr(span, "fork block has no checked task result");
                };
                let Ok(output_ty) = ResolvedTy::from_ty(output) else {
                    return self.unsupported_expr(span, "fork block result type is unresolved");
                };
                let task_ty = ResolvedTy::Task(Box::new(output_ty.clone()));
                let outer_bindings = self.visible_outer_bindings();
                let lowered_body = self.with_current_return_type(output_ty.clone(), |ctx| {
                    ctx.lower_block(body, &output_ty)
                });
                let Some(checker_facts) = self.closure_capture_facts.get(&checker_key).cloned()
                else {
                    return self.unsupported_expr(span, "fork block has no checked captures");
                };
                let captures = self.materialize_closure_block_captures(
                    &lowered_body,
                    &outer_bindings,
                    checker_facts,
                    span.clone(),
                );
                (
                    HirExprKind::ForkBlock {
                        body: lowered_body,
                        task_ty: task_ty.clone(),
                        captures,
                    },
                    task_ty,
                )
            }
            Expr::ScopeDeadline { duration, body } => {
                let Some(checked_ty) = self.expr_types.get(&self.mk_key(&span)) else {
                    return self.unsupported_expr(span, "scope has no checked result type");
                };
                let Ok(result_ty) = ResolvedTy::from_ty(checked_ty) else {
                    return self.unsupported_expr(span, "scope result type is unresolved");
                };
                let duration = self.lower_expr(duration, IntentKind::Read);
                self.scope_depth += 1;
                let body = self.lower_block(body, &result_ty);
                self.scope_depth -= 1;
                (
                    HirExprKind::ScopeDeadline {
                        duration: Box::new(duration),
                        body,
                    },
                    result_ty,
                )
            }
            Expr::AwaitRestart(inner) => {
                // `await_restart sup.child` — suspend until the static supervised
                // child restarts, then resume with the re-fetched live handle.
                // Lower the inner supervised-child accessor (a `FieldAccess`);
                // its `site` keys `supervisor_child_slots` with the (supervisor,
                // slot) discriminator MIR re-reads to emit `SuspendKind::RestartWait`.
                // The result type is the child's stable `ChildRef<ChildType>` (the
                // checker assigned this expression the same type as the accessor).
                let child = self.lower_expr(inner, IntentKind::Read);
                let result_ty = self
                    .expr_types
                    .get(&self.mk_key(&span))
                    .and_then(|t| ResolvedTy::from_ty(t).ok())
                    .unwrap_or_else(|| child.ty.clone());
                (
                    HirExprKind::AwaitRestart {
                        child: Box::new(child),
                    },
                    result_ty,
                )
            }
            Expr::Await(inner) => {
                // TCP methods retain their authored wrapper and checked return
                // type. Their canonical extern call owns native I/O suspension.
                // NEW-7: `await stream.recv()` over a `Stream<T>` — the
                // checker wired the inner method call to the layout-witness
                // `hew_stream_next_layout` entry (one symbol for every
                // describable element type). Strip the `await` and lower the
                // inner recv directly; its `Option<T>` result is bound on the
                // resume edge of the MIR `SuspendingStreamNext` (the
                // suspendable-caller flip in `lower_direct_call`), or the
                // blocking call for a context-free caller. Mirrors the
                // actor-ask / conn-read bindable-await paths.
                if self.is_stream_recv_await(&self.mk_key(&inner.1)) {
                    let source = self.lower_expr(inner, intent);
                    return self.subsumed_value(site, &span, intent, source);
                }
                // NEW-7: `await sink.send(x)` over a `Sink<bytes>` — the checker
                // wired the inner method call to `hew_sink_write_bytes`. Strip the
                // `await` and lower the inner send directly (unit); the MIR
                // `SuspendingStreamSend` suspends on a full ring. Statement
                // position only (unit value), like `actor.close()`.
                if self.is_stream_send_await(&self.mk_key(&inner.1)) {
                    let source = self.lower_expr(inner, intent);
                    return self.subsumed_value(site, &span, intent, source);
                }
                let inner_hir = self.lower_expr(inner, IntentKind::Consume);
                match &inner_hir.ty {
                    ResolvedTy::Task(output_ty) => {
                        let output_ty = *output_ty.clone();
                        (
                            HirExprKind::AwaitTask {
                                operand: Box::new(inner_hir),
                                output_ty: output_ty.clone(),
                            },
                            output_ty,
                        )
                    }
                    ResolvedTy::Named {
                        name,
                        builtin: Some(BuiltinType::Vec),
                        args,
                        ..
                    } if matches!(args.first(), Some(ResolvedTy::Task(_))) => {
                        let ResolvedTy::Task(output_ty) = args[0].clone() else {
                            unreachable!("matched a vector of task handles")
                        };
                        let results_ty = ResolvedTy::named_builtin(
                            name.clone(),
                            BuiltinType::Vec,
                            vec![(*output_ty).clone()],
                        );
                        let block = self.lower_vector_await(
                            inner_hir,
                            &output_ty,
                            &results_ty,
                            span.clone(),
                        );
                        (HirExprKind::Block(block), results_ty)
                    }
                    _ if self
                        .checked_call_effects
                        .contains_key(&self.mk_key(&inner.1)) =>
                    {
                        return self.subsumed_value(site, &span, intent, inner_hir);
                    }
                    found_ty => {
                        // The operand is not a Task<T> — reject with AwaitNonTask.
                        let found_ty = found_ty.clone();
                        self.diagnostics.push(HirDiagnostic::new(
                            HirDiagnosticKind::AwaitNonTask {
                                found_ty: found_ty.clone(),
                            },
                            span.clone(),
                            "`await` requires a task handle (`Task<T>`). \
                             Hint: did you mean to bind a task with `fork name = call(...)` first?",
                        ));
                        (
                            HirExprKind::Unsupported("`await` on non-task".to_string()),
                            ResolvedTy::Unit,
                        )
                    }
                }
            }
            Expr::Select { arms, timeout } => {
                self.lower_select(arms, timeout.as_deref(), span.clone())
            }
            Expr::Race(branches) => self.lower_race(branches, span.clone()),
            Expr::Spawn { target, args, .. } => self.lower_spawn(target, args, span.clone()),
            Expr::SpawnLambdaActor {
                params,
                return_type,
                body,
                ..
            } => self.lower_spawn_lambda_actor(params, return_type.as_ref(), body, &span),
            Expr::Lambda { params, body, .. } => self.lower_closure(params, body, span.clone()),
            Expr::GenBlock { body } => self.lower_gen_block(body, span.clone()),
            Expr::Yield(value) => {
                let value = value
                    .as_deref()
                    .map(|value| Box::new(self.lower_expr(value, IntentKind::Read)));
                let yield_ty = if let Some(yield_ty) = self.generator_yield_tys.last() {
                    yield_ty.clone()
                } else {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::CheckerBoundaryViolation {
                            name: "yield".to_string(),
                            reason: "yield expression lowered outside an enclosing gen block"
                                .to_string(),
                        },
                        span.clone(),
                        "yield expression has no enclosing generator yield type",
                    ));
                    ResolvedTy::Unit
                };
                (HirExprKind::Yield { value, yield_ty }, ResolvedTy::Unit)
            }
            Expr::Return(value) => {
                // `return [expr]` in expression position. Lower the operand with
                // `Consume` intent — exactly as the statement form
                // (`Stmt::Return`) — since the value leaves the function. The
                // construct is `Never`-typed (it diverges); MIR seals it with
                // `Terminator::Return` via the same shell as `HirStmtKind::Return`
                // (LESSONS `one-construct-one-lowering-shell`).
                let value = value
                    .as_deref()
                    .map(|value| Box::new(self.lower_expr(value, IntentKind::Consume)));
                let value = if self
                    .result_return_coercions
                    .contains_key(&self.mk_key(&span))
                {
                    let value =
                        value.map_or_else(|| self.make_unit_expr(span.clone()), |value| *value);
                    Some(Box::new(self.apply_result_return_coercion(value, &span)))
                } else {
                    value
                };
                // TI-5 escape check (defense-in-depth, mirrors the statement
                // form): a `Task<T>` handle must not escape via `return`.
                if let Some(expr) = &value {
                    if matches!(expr.ty, ResolvedTy::Task(_)) {
                        self.diagnostics.push(HirDiagnostic::new(
                            HirDiagnosticKind::TaskCannotEscape,
                            span.clone(),
                            "a `Task<T>` handle cannot escape via `return`; \
                             await it inside the `scope{}` body with `await name`",
                        ));
                    }
                }
                (HirExprKind::Return { value }, ResolvedTy::Never)
            }
            Expr::ReturnError(value) => {
                if self.result_return_coercions.get(&self.mk_key(&span))
                    == Some(&hew_types::ResultReturnKind::Error)
                {
                    let value = self.lower_expr(value, IntentKind::Consume);
                    let value = self.apply_result_return_coercion(value, &span);
                    (
                        HirExprKind::Return {
                            value: Some(Box::new(value)),
                        },
                        ResolvedTy::Never,
                    )
                } else {
                    self.unsupported_postfix_try(
                        &span,
                        "error return without checked failure selection",
                    )
                }
            }
            Expr::MethodCall {
                receiver,
                method,
                args,
            } => self.lower_method_call(receiver, method, args, span.clone(), site),
            // `clone <operand>` reuses the `.clone()` lowering: the checker
            // recorded the method resolution / rewrite at this span (see
            // `synthesize` `Expr::Clone`), so this routes through the same
            // copy-path selection and fail-closed `CloneNotYetSupported`
            // diagnostic as `<operand>.clone()`.
            Expr::Clone(operand) => {
                self.lower_method_call(operand, "clone", &[], span.clone(), site)
            }
            Expr::UnsafeBlock(block) => {
                // Unsafe clearance is a checker-only concept; the HIR represents the
                // body as a plain block.  The `in_unsafe` flag pushed by the type
                // checker is not carried into HIR or MIR — pointer and FFI safety
                // obligations are enforced by the checker before lowering.
                let hir_block = self.lower_block(block, &ResolvedTy::Unit);
                let ty = hir_block.ty.clone();
                (HirExprKind::Block(hir_block), ty)
            }
            Expr::Index { object, index } => {
                // The checker records the element/result type at the whole
                // index expression's span. LESSONS: `checker-authority` P0 —
                // we never re-derive the element type from the container;
                // the checker is the sole owner.
                let checker_key = self.mk_key(&span);
                let result_ty = if let Some(ty) = self.expr_types.get(&checker_key).cloned() {
                    match ResolvedTy::from_ty(&ty) {
                        Ok(resolved) => resolved,
                        Err(err) => {
                            self.diagnostics.push(HirDiagnostic::new(
                                HirDiagnosticKind::CheckerBoundaryViolation {
                                    name: "xs[..]".to_string(),
                                    reason: err.to_string(),
                                },
                                span.clone(),
                                "checker-authoritative index/slice result type failed boundary conversion",
                            ));
                            ResolvedTy::Unit
                        }
                    }
                } else {
                    // Fall through: no checker entry. This can happen when the
                    // checker emitted an error for this expression (e.g.
                    // indexing into a non-Vec) and skipped recording the
                    // type. The diagnostic from the checker covers the
                    // user-facing error; emit Unit to stay well-formed.
                    ResolvedTy::Unit
                };

                // Distinguish range-slice (`xs[a..b]` and the four open-end
                // forms) from single-element indexing (`xs[i]`). The parser
                // emits `Expr::Range` only when the bracket contents are a
                // range; all other expressions remain as `Expr::Index`.
                let container = self.lower_expr(object, IntentKind::Read);
                if let Expr::Range {
                    start,
                    end,
                    inclusive,
                } = &index.0
                {
                    // C-3 range-slice: result type is Vec<T> (checker-authoritative).
                    let lowered_start = start
                        .as_ref()
                        .map(|s| Box::new(self.lower_expr(s, IntentKind::Read)));
                    let lowered_end = end.as_ref().map(|e| {
                        let bound = self.lower_expr(e, IntentKind::Read);
                        Box::new(if *inclusive {
                            self.exclusive_slice_bound(bound, &e.1)
                        } else {
                            bound
                        })
                    });
                    (
                        HirExprKind::Slice {
                            container: Box::new(container),
                            start: lowered_start,
                            end: lowered_end,
                        },
                        result_ty,
                    )
                } else {
                    let index_expr = self.lower_expr(index, IntentKind::Read);
                    if let Some(dyn_call) = self
                        .dyn_trait_method_calls
                        .get(&self.mk_key(&span))
                        .cloned()
                    {
                        if !self.ensure_executable_target(
                            &dyn_call.target,
                            "dynamic trait call",
                            &span,
                        ) {
                            return self.make_expr(
                                HirExprKind::Unsupported(
                                    "dynamic trait call has no checker target".to_string(),
                                ),
                                result_ty,
                                intent,
                                span,
                            );
                        }
                        return HirExpr {
                            node: self.ids.node(),
                            site,
                            ty: result_ty.clone(),
                            intent,
                            kind: HirExprKind::CallDynMethod {
                                receiver: Box::new(container),
                                target: dyn_call.target,
                                trait_name: dyn_call.trait_name,
                                method_name: dyn_call.method_name,
                                slot: dyn_call.slot,
                                args: vec![index_expr],
                                evaluation_order: Vec::new(),
                                ret_ty: result_ty,
                                signature: Box::new(dyn_call.signature),
                            },
                            span: span.clone(),
                        };
                    }

                    // `m[k]` over a `HashMap<K, V>` in READ position is the
                    // trapping `Index::at` accessor (`-> V`): a missing key
                    // aborts with IndexOutOfBounds (the map analogue of `v[i]`
                    // OOB), mirroring `Vec`. It falls through to the plain
                    // `HirExprKind::Index` node below, which the MIR rvalue
                    // `Index` match lowers to `lower_hashmap_index_trap`
                    // (`hew_hashmap_get_clone_layout` + trap-on-miss). The
                    // non-aborting `Option<V>` outcome is `m.get(k)`, which
                    // takes the `ResolvedImplCall` get path above.
                    //
                    // An assignment target (`m[k] = v`, `IntentKind::Modify`)
                    // also falls through to the plain `Index` node, which the
                    // MIR `assign` arm recognises and lowers to
                    // `hew_hashmap_insert_layout`.

                    if let ResolvedTy::Named { name, builtin, .. } = &container.ty {
                        let callee_name = format!("{name}::at");
                        if !matches!(builtin, Some(BuiltinType::Vec | BuiltinType::HashMap))
                            && self.fn_registry.contains_key(&callee_name)
                        {
                            let target = self.registered_symbol_target(&callee_name);
                            let callee_ty = ResolvedTy::Function {
                                capabilities: hew_types::CallableCapabilities::FUNCTION_ITEM,
                                params: vec![container.ty.clone(), index_expr.ty.clone()],
                                ret: Box::new(result_ty.clone()),
                            };
                            let resolved = self
                                .fn_registry
                                .get(&callee_name)
                                .map_or(ResolvedRef::Unresolved, |entry| {
                                    ResolvedRef::Item(entry.id)
                                });
                            let callee = HirExpr {
                                node: self.ids.node(),
                                site: self.ids.site(),
                                ty: callee_ty,
                                intent: IntentKind::Read,
                                kind: HirExprKind::BindingRef {
                                    name: callee_name,
                                    resolved,
                                },
                                span: span.clone(),
                            };
                            return HirExpr {
                                node: self.ids.node(),
                                site,
                                ty: result_ty.clone(),
                                intent,
                                kind: HirExprKind::Call {
                                    target,
                                    callee: Box::new(callee),
                                    args: vec![container, index_expr],
                                    evaluation_order: Vec::new(),
                                },
                                span: span.clone(),
                            };
                        }
                    }

                    // C-2 single-element Vec indexing: result type is element type T.
                    // D432: the checker records the reads whose element has no
                    // clone; those bind a loan of the slot instead of copying.
                    let borrowed = self
                        .borrowed_element_index_reads
                        .contains(&self.mk_key(&span));
                    let container = Box::new(container);
                    let index = Box::new(index_expr);
                    (
                        if borrowed {
                            HirExprKind::BorrowedIndex { container, index }
                        } else {
                            HirExprKind::Index { container, index }
                        },
                        result_ty,
                    )
                }
            }
            Expr::Is { lhs, rhs } => {
                if self.is_type_patterns.contains_key(&self.mk_key(&rhs.1)) {
                    // The checker only records this side-table entry after
                    // proving the static lhs type matches the RHS type pattern.
                    // Do not lower the RHS identifier through the value namespace.
                    (
                        HirExprKind::Literal(HirLiteral::Bool(true)),
                        ResolvedTy::Bool,
                    )
                } else {
                    // Identity comparison: `lhs is rhs`. The checker (D-2) has already
                    // validated that both operands carry identity-bearing types and
                    // that neither is a scalar/String/record. The result is `bool`.
                    // LESSONS: `checker-authority` P0 — we do not re-validate the
                    // allowance set here; that is the checker's sole responsibility.
                    let left = self.lower_expr(lhs, IntentKind::Read);
                    let right = self.lower_expr(rhs, IntentKind::Read);
                    (
                        HirExprKind::IdentityCompare {
                            left: Box::new(left),
                            right: Box::new(right),
                        },
                        ResolvedTy::Bool,
                    )
                }
            }
            Expr::FieldAccess { object, field } => {
                // Dotted module-qualified unit constructor:
                // `module.Type.Variant`. The checker has already resolved
                // this nested field-access surface to the exact tagged-union
                // owner. Consume that fact before lowering `module.Type` as a
                // runtime projection (it is a namespace path, not a value).
                if let Expr::FieldAccess {
                    object: module,
                    field: type_name,
                } = &object.0
                {
                    if let Expr::Identifier(module_short) = &module.0 {
                        let canonical_type =
                            self.imported_module_member_key(module_short, type_name);
                        let checker_ty = self.checker_expr_ty_if_present(&span);
                        let checker_selects_type = matches!(
                            &checker_ty,
                            Some(ResolvedTy::Named { name, .. }) if name == &canonical_type
                        );
                        if checker_selects_type {
                            if let Some((type_name, variant_idx, HirVariantKind::Unit)) =
                                self.lookup_variant_ctor(field, checker_ty.as_ref())
                            {
                                let result_ty = checker_ty
                                    .expect("module-qualified variant selection checked above");
                                return HirExpr {
                                    node: self.ids.node(),
                                    site,
                                    ty: result_ty,
                                    intent,
                                    kind: HirExprKind::MachineVariantCtor {
                                        machine_name: type_name,
                                        state_idx: variant_idx,
                                        payload: None,
                                    },
                                    span,
                                };
                            }
                        }
                    }
                }
                // Pre-dispatch: module-qualified constant reference, e.g.
                // `module_short.CONST_NAME`.  The type checker accepted this as
                // a qualified const access and registered the result type in
                // `expr_types`; the HIR must produce a `BindingRef { Const(id) }`
                // rather than falling through to the generic struct-field path
                // (which would `lower_expr(object)` on a bare module name and
                // fail with `UnresolvedSymbol`).
                //
                // Guard: object is a bare `Expr::Identifier` and the checker-resolved
                // owner key is in `const_registry`. The registry pre-pass stores
                // imported consts under exact source owners, so the lexical module
                // binding must cross the same owner map used by type checking.
                if let Expr::Identifier(module_name) = &object.0 {
                    let qualified_key = self.imported_module_member_key(module_name, field);
                    // A file import's declaration is spliced into the root
                    // namespace and registered under its bare name, so the
                    // qualified spelling reaches it through the same mapping a
                    // bare reference uses.
                    let registry_key = self.published_const_key(&qualified_key).to_string();
                    if let Some(entry) = self.const_registry.get(&registry_key).cloned() {
                        let ty = entry.ty.clone();
                        let id = entry.id;
                        return HirExpr {
                            node: self.ids.node(),
                            site,
                            ty,
                            intent,
                            kind: HirExprKind::BindingRef {
                                name: qualified_key,
                                resolved: ResolvedRef::Const(id),
                            },
                            span,
                        };
                    }
                }

                // Resolve a module function value through its lexical owner.
                // A local record with the same name remains a field access.
                if let Expr::Identifier(module_name) = &object.0 {
                    let key = self.imported_module_member_key(module_name, field);
                    let symbol = crate::mangle_dotted_name(&key);
                    if self.lookup(module_name).is_none()
                        && self.fn_registry.contains_key(&symbol)
                        && matches!(
                            self.checker_expr_ty_if_present(&span),
                            Some(ResolvedTy::Function { .. })
                        )
                    {
                        let (kind, ty) = self.lower_function_value(&symbol, &span, site);
                        return HirExpr {
                            node: self.ids.node(),
                            site,
                            ty,
                            intent,
                            kind,
                            span,
                        };
                    }
                }

                let missing_import = if let Expr::Identifier(module_name) = &object.0 {
                    self.missing_stdlib_module_import(module_name)
                        .map(|module| (module_name, module))
                } else {
                    None
                };
                if let Some((module_name, module)) = missing_import {
                    let name = format!("{module_name}.{field}");
                    let source_module = module.replace("::", ".");
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::ImportMissing {
                            module: source_module,
                            name,
                        },
                        span.clone(),
                        stdlib_catalog::missing_import_hint(module).replace("::", "."),
                    ));
                    (
                        HirExprKind::FieldAccess {
                            object: Box::new(
                                self.unresolved_module_object(module_name, object.1.clone()),
                            ),
                            field: field.clone(),
                        },
                        ResolvedTy::Unit,
                    )
                } else {
                    // Named-field read on a record or struct type, or numeric
                    // projection on a tuple type (`t.0`). The checker has already
                    // resolved the field and recorded the result type in
                    // `expr_types`. LESSONS: `checker-authority` P0 — the type of
                    // the field read comes exclusively from the checker side-table,
                    // never re-derived here.
                    let hir_object = self.lower_expr(object, IntentKind::Read);
                    let tuple_index = match &hir_object.ty {
                        ResolvedTy::Tuple(elements) => field
                            .parse::<usize>()
                            .ok()
                            .map(|index| (index, elements.get(index).cloned(), elements.len())),
                        _ => None,
                    };
                    if let Some((index, expected_ty, tuple_len)) = tuple_index {
                        if let Some(expected_ty) = expected_ty {
                            if let Some(field_ty) =
                                self.checker_expr_ty(&span, "tuple field access")
                            {
                                if field_ty == expected_ty {
                                    (
                                        HirExprKind::TupleIndex {
                                            tuple: Box::new(hir_object),
                                            index,
                                        },
                                        field_ty,
                                    )
                                } else {
                                    self.diagnostics.push(HirDiagnostic::new(
                                        HirDiagnosticKind::CheckerBoundaryViolation {
                                            name: format!("tuple index .{index}"),
                                            reason: format!(
                                                "checker result type {} does not match tuple element type {}",
                                                field_ty.user_facing(),
                                                expected_ty.user_facing()
                                            ),
                                        },
                                        span.clone(),
                                        "tuple field access result type must match the projected element",
                                    ));
                                    (
                                        HirExprKind::Unsupported(
                                            "tuple field access checker type mismatch".to_string(),
                                        ),
                                        ResolvedTy::Unit,
                                    )
                                }
                            } else {
                                (
                                    HirExprKind::Unsupported(
                                        "tuple field access missing checker result type"
                                            .to_string(),
                                    ),
                                    ResolvedTy::Unit,
                                )
                            }
                        } else {
                            self.diagnostics.push(HirDiagnostic::new(
                                HirDiagnosticKind::CheckerBoundaryViolation {
                                    name: format!("tuple index .{index}"),
                                    reason: format!(
                                        "tuple index out of range: tuple has {tuple_len} elements"
                                    ),
                                },
                                span.clone(),
                                "tuple field access must be in bounds",
                            ));
                            (
                                HirExprKind::Unsupported(
                                    "tuple field access index out of bounds".to_string(),
                                ),
                                ResolvedTy::Unit,
                            )
                        }
                    } else {
                        let checker_key = self.mk_key(&span);
                        let field_ty = if let Some(ty) = self.expr_types.get(&checker_key).cloned()
                        {
                            match ResolvedTy::from_ty(&ty) {
                                // `Ty::Named` does not carry HIR's opacity bit.
                                // Normalize the checker-authored field result
                                // through the same module-identity funnel used
                                // by other checker→HIR reads. This is
                                // load-bearing for an imported resource wrapper
                                // reading its private opaque handle field:
                                // `regex.Pattern.handle` must remain
                                // `regex.PatternHandle`, not a bare user
                                // `PatternHandle` that reaches D10.
                                Ok(resolved) => self.qualify_current_module_record_ty(resolved),
                                Err(err) => {
                                    let diagnostic = HirDiagnostic::new(
                                        HirDiagnosticKind::CheckerBoundaryViolation {
                                            name: field.clone(),
                                            reason: err.to_string(),
                                        },
                                        span.clone(),
                                        "field-access result type failed checker-boundary conversion",
                                    );
                                    self.diagnostics.push(diagnostic);
                                    ResolvedTy::Unit
                                }
                            }
                        } else {
                            // No checker entry: malformed checker output. Fail-closed.
                            self.diagnostics.push(HirDiagnostic::new(
                                HirDiagnosticKind::CheckerBoundaryViolation {
                                    name: field.clone(),
                                    reason: "expr_types has no entry for field-access site".into(),
                                },
                                span.clone(),
                                "field-access result type missing from checker side-table",
                            ));
                            ResolvedTy::Unit
                        };
                        // Checker-authority: if the checker recorded a supervisor
                        // child-slot for this field-access span, propagate it into
                        // the accumulator keyed by the pre-allocated SiteId. MIR
                        // (S2) reads `HirModule.supervisor_child_slots` to intercept
                        // these sites before the `record_field_orders` path.
                        // Mirrors the `call_site_type_args` pattern (lower.rs line 1127).
                        if let Some(slot) = self.supervisor_child_slots_checker.get(&checker_key) {
                            self.supervisor_child_slots.insert(site, slot.clone());
                        }
                        (
                            HirExprKind::FieldAccess {
                                object: Box::new(hir_object),
                                field: field.clone(),
                            },
                            field_ty,
                        )
                    }
                }
            }
            // `emit` only appears inside a machine body, which the checker
            // normalizes into an ordinary push onto the step's output vector
            // before HIR. Reaching this arm means an un-normalized program.
            Expr::MachineEmit { event_name, .. } => {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::UnresolvedSymbol {
                        name: event_name.clone(),
                    },
                    span.clone(),
                    "`emit` outside a normalized machine body has no output vector",
                ));
                (
                    HirExprKind::Unsupported(format!("emit `{event_name}` outside a machine")),
                    ResolvedTy::Unit,
                )
            }
            Expr::Match { scrutinee, arms } => self.lower_match_expr(scrutinee, arms, &span),
            Expr::PostfixTry(inner) => self.lower_postfix_try(inner, &span),
            Expr::Coalesce { left, right } => self.lower_local_recovery(left, right, None, &span),
            Expr::Handle {
                operand,
                error,
                body,
            } => self.lower_local_recovery(operand, body, Some(error), &span),
            Expr::InterpolatedString(parts) => self.lower_interpolated_string(parts, span.clone()),
            Expr::Tuple(elems) => self.lower_tuple_literal(elems, &span),
            Expr::Array(elems) => self.lower_array_literal(elems, &span),
            Expr::ArrayRepeat { value, count } => self.lower_array_repeat(value, count, &span),
            Expr::MapLiteral { entries } => self.lower_map_literal(entries, &span),
            Expr::Cast { expr: value, ty } => self.lower_numeric_cast_expr(value, ty, &span),
            Expr::IfLet {
                conditions,
                body,
                else_body,
            } => {
                // Expression-position pattern condition — delegates to the
                // shared chain lowering.
                // The result type is looked up from the checker's `resolved_expr_types`
                // side-table, mirroring `Expr::If` (same authority path).
                let result_ty = self
                    .resolved_expr_types
                    .get(&self.mk_key(&span))
                    .cloned()
                    .unwrap_or(ResolvedTy::Unit);
                let lowered = self.lower_condition_chain(
                    conditions,
                    body,
                    &span,
                    ConditionFallthrough::Else(else_body.as_deref()),
                    &result_ty,
                    &span,
                );
                (lowered.kind, result_ty)
            }
            // `b"AB"` — byte-string literal. The parser already decoded the
            // escape sequences; `inner` is the raw byte sequence.
            Expr::ByteStringLiteral(inner) => (
                HirExprKind::Literal(HirLiteral::Bytes(inner.clone())),
                ResolvedTy::Bytes,
            ),
            // `bytes[0x41, 0x42]` — byte-array literal. The parser validated
            // each element is in 0..=255 and stored them as `Vec<u8>`.
            Expr::ByteArrayLiteral(elems) => (
                HirExprKind::Literal(HirLiteral::Bytes(elems.clone())),
                ResolvedTy::Bytes,
            ),
            Expr::Range { .. } => {
                self.unsupported(span.clone(), "expression", "slice-2");
                (
                    HirExprKind::Unsupported("unsupported expression".into()),
                    ResolvedTy::Unit,
                )
            }
        };
        let kind = self.normalize_collection_call(kind, &ty, &span);
        let inner = HirExpr {
            node: self.ids.node(),
            site,
            ty,
            intent,
            kind,
            span: span.clone(),
        };
        // Checker-authority: if the checker recorded a static-pool accessor for
        // this expression span (`sup.pool[i]` / `.get(i)` / `.len()`), propagate
        // it into the SiteId-keyed accumulator. MIR reads
        // `HirModule.pool_accessor_sites` to emit the pool ABI call.
        let pool_accessor_key = self.mk_key(&span);
        if let Some(accessor) = self.pool_accessor_sites_checker.get(&pool_accessor_key) {
            self.pool_accessor_sites.insert(site, accessor.clone());
        }
        // Checker-authority coercion: if the just-lowered expression sits at
        // a `T → dyn Trait` coercion site (recorded by the type checker at
        // the *argument* expression span), wrap the result in
        // `HirExprKind::CoerceToDynTrait`. MIR lowers this 1:1 to
        // `Instr::CoerceToDynTrait`. The wrapping ResolvedTy is the
        // destination trait-object type, which the wrapping HirExpr
        // carries; the inner expression keeps its concrete type.
        let coercion_key = self.mk_key(&span);
        if let Some(coercion) = self.dyn_trait_coercions.get(&coercion_key).cloned() {
            let mut resolved_bounds = Vec::new();
            for name in coercion.trait_name.split('+') {
                let mut assoc_bindings = Vec::new();
                for binding in coercion
                    .assoc_bindings
                    .iter()
                    .filter(|binding| binding.trait_name == name)
                {
                    let Ok(ty) = hew_types::ResolvedTy::from_ty(&binding.ty) else {
                        self.diagnostics.push(HirDiagnostic::new(
                            HirDiagnosticKind::CheckerBoundaryViolation {
                                name: "dyn-trait assoc binding".to_string(),
                                reason: format!(
                                    "`{}.{}` failed boundary conversion",
                                    binding.trait_name, binding.assoc_name
                                ),
                            },
                            span.clone(),
                            "associated type binding from dyn_trait_coercions failed boundary conversion",
                        ));
                        return inner;
                    };
                    assoc_bindings.push((binding.assoc_name.clone(), ty));
                }
                resolved_bounds.push(hew_types::ResolvedTraitBound {
                    trait_name: name.to_string(),
                    args: vec![],
                    assoc_bindings,
                });
            }
            let dyn_ty = ResolvedTy::TraitObject {
                traits: resolved_bounds,
            };
            // The checker side table is keyed by source span and may preserve
            // the original concrete provenance when an already-erased value is
            // passed to the same `dyn Trait` type. Re-wrapping that value would
            // make MIR box the two-word fat pointer as though it were the
            // concrete payload, while the vtable still describes the original
            // concrete allocation. Identical dyn-to-dyn adaptation is a no-op;
            // a genuine trait-object upcast needs an explicit vtable-adjusting
            // ABI and remains fail-closed.
            if matches!(inner.ty, ResolvedTy::TraitObject { .. }) {
                if inner.ty == dyn_ty {
                    return inner;
                }
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::CheckerBoundaryViolation {
                        name: "dyn-trait coercion".to_string(),
                        reason: format!(
                            "unsupported dyn-to-dyn adaptation from `{:?}` to `{:?}`",
                            inner.ty, dyn_ty
                        ),
                    },
                    span.clone(),
                    "dyn-to-dyn trait adaptation requires an explicit vtable upcast",
                ));
                return self.unsupported_expr(span, "dyn-to-dyn trait adaptation");
            }
            let concrete_resolved = match ResolvedTy::from_ty(&coercion.concrete_type) {
                Ok(r) => self.qualify_current_module_record_ty(r),
                Err(err) => {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::CheckerBoundaryViolation {
                            name: "dyn-trait coercion".to_string(),
                            reason: err.to_string(),
                        },
                        span.clone(),
                        "concrete type from dyn_trait_coercions failed boundary conversion",
                    ));
                    return inner;
                }
            };

            let wrapped = HirExpr {
                node: self.ids.node(),
                site: self.ids.site(),
                ty: dyn_ty,
                intent,
                kind: HirExprKind::CoerceToDynTrait {
                    value: Box::new(inner),
                    trait_name: coercion.trait_name,
                    concrete_type: concrete_resolved,
                    method_table: coercion.method_table,
                    vtable_entries: coercion.vtable_entries,
                },
                span,
            };

            return wrapped;
        }
        inner
    }
}
