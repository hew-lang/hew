//! Method-call lowering.

use super::*;

impl LowerCtx {
    /// Lower a method call the checker dispatched to an actor receive handler.
    /// Kept out of `lower_method_call` so its locals do not enlarge that frame.
    #[inline(never)]
    #[allow(
        clippy::too_many_lines,
        reason = "one actor dispatch selects message, ask or stream lowering"
    )]
    pub(super) fn lower_actor_method_call(
        &mut self,
        dispatch: ActorMethodKind,
        receiver: &Spanned<Expr>,
        method: &str,
        args: &[hew_parser::ast::CallArg],
        span: &Span,
    ) -> (HirExprKind, ResolvedTy) {
        let span = span.clone();
        let key = self.mk_key(&span);
        let lowered_receiver = self.lower_expr(receiver, IntentKind::Read);
        // A lambda dispatch on method-call syntax is a call on a stored
        // handle: `job.run(3)` addresses the handle in the field, so the
        // field read is the delivery receiver.
        let lowered_receiver = if matches!(&dispatch,
                ActorMethodKind::Ask { method_id, .. } | ActorMethodKind::Message { method_id, .. }
                    if method_id == hew_types::actor_protocol::LAMBDA_ACTOR_METHOD_ID)
        {
            match self.method_call_rewrites.get(&key).cloned() {
                Some(MethodCallRewrite::RecordFnFieldCall { field_ty }) => self.make_expr(
                    HirExprKind::FieldAccess {
                        object: Box::new(lowered_receiver),
                        field: method.to_string(),
                    },
                    field_ty,
                    IntentKind::Read,
                    span.clone(),
                ),
                // `handle.send(msg)` addresses the handle itself, so the
                // receiver is already the delivery target.
                _ => lowered_receiver,
            }
        } else {
            lowered_receiver
        };
        let LoweredCallArgs {
            args: lowered_args,
            evaluation_order,
        } = self.lower_call_args_by_slot(args, &span, |this, _, arg| {
            // A single-owner value crossing an actor message boundary
            // transfers ownership to the receiving handler — the
            // mailbox copies the handle/resource, not the underlying
            // thing it owns. Lower such args with `IntentKind::Consume`
            // so the move-checker marks the caller binding consumed: a
            // later use (`rx.close()`, `s.detach()`, a second send)
            // would race the new owner and free the value twice. Every
            // other arg keeps `Read` — CoW boundary copy semantics.
            //
            // The predicate is RECURSIVE: a direct handle/resource arg
            // AND any arg whose type transitively carries one (e.g. a
            // tuple `(Stream<T>, string)`) both transfer the owned
            // pointer. Without the recursive check a nested-handle arg
            // is lowered as `Read` (CowShare in MIR), the caller
            // binding stays live, and a subsequent close silently
            // double-frees.
            this.actor_message_arg_intent(&arg.1)
        });
        match dispatch {
            ActorMethodKind::Message { method_id, policy } => {
                let method_id = self.qualify_imported_actor_method_id(method_id);
                let Some(ty) = self.checker_expr_ty_if_present(&span) else {
                    return (
                        HirExprKind::Unsupported("message submission has no checked type".into()),
                        ResolvedTy::Unit,
                    );
                };
                // The call IS the send: a `receive fn` without a reply
                // builds its addressed description and submits it at the
                // same site. The description is an internal temporary.
                let Some(message_ty) = Self::submitted_message_ty(&ty) else {
                    return (
                        HirExprKind::Unsupported(
                            "message submission has no checked message type".into(),
                        ),
                        ResolvedTy::Unit,
                    );
                };
                self.try_register_enum_instantiation_ty(&ty, &span);
                let message = HirExpr {
                    node: self.ids.node(),
                    site: self.ids.site(),
                    ty: message_ty,
                    intent: IntentKind::Consume,
                    kind: HirExprKind::ActorMessage {
                        receiver: Box::new(lowered_receiver),
                        method_id,
                        args: lowered_args,
                        evaluation_order,
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
            ActorMethodKind::Ask {
                method_id,
                reply_ty,
                policy,
            } => {
                let method_id = self.qualify_imported_actor_method_id(method_id);
                let Some(result_ty) = self.checked_actor_ask_result_ty(&span, &method_id) else {
                    return (
                        HirExprKind::Unsupported("actor ask has no checked result".to_string()),
                        ResolvedTy::Unit,
                    );
                };
                match ResolvedTy::from_ty(&reply_ty) {
                    Ok(reply_ty) => {
                        // Owner-qualify the ask-reply record identity to the
                        // ASKED actor's declaring module when it collides, so
                        // this reply type and the actor-handler layout return
                        // type (qualified in `lower_imported_actor` under the
                        // same collision gate) both resolve to the SAME
                        // qualified identity the MIR record layout is keyed by.
                        // `method_id` is `{module}.{Actor}::{method}` for an
                        // imported actor; a bare/root actor has no leading module
                        // segment and is left unqualified (#2208).
                        let reply_ty = Self::actor_module_short_of_method_id(&method_id)
                            .map_or_else(
                                || reply_ty.clone(),
                                |module_short| {
                                    self.qualify_colliding_module_record_ty(&reply_ty, module_short)
                                },
                            );
                        (
                            HirExprKind::ActorAsk {
                                receiver: Box::new(lowered_receiver),
                                method_id,
                                args: lowered_args,
                                evaluation_order,
                                reply_ty: reply_ty.clone(),
                                policy,
                                deadline_ns: None,
                            },
                            result_ty,
                        )
                    }
                    Err(err) => {
                        self.diagnostics.push(HirDiagnostic::new(
                            HirDiagnosticKind::CheckerBoundaryViolation {
                                name: format!("actor method `.{method}`"),
                                reason: err.to_string(),
                            },
                            span.clone(),
                            "checker-authoritative actor_method_dispatch reply type failed boundary conversion",
                        ));
                        (
                            HirExprKind::Unsupported(format!(
                                "actor method `.{method}` has poisoned dispatch reply type"
                            )),
                            ResolvedTy::Unit,
                        )
                    }
                }
            }
            ActorMethodKind::StreamProducer(method_id, elem_ty) => {
                let method_id = self.qualify_imported_actor_method_id(method_id);
                match ResolvedTy::from_ty(&elem_ty) {
                    Ok(elem_ty) => {
                        let stream_ty = ResolvedTy::named_builtin(
                            "Stream",
                            hew_types::BuiltinType::Stream,
                            vec![elem_ty],
                        );
                        (
                            HirExprKind::ActorGenStream {
                                receiver: Box::new(lowered_receiver),
                                method: method_id,
                                args: lowered_args,
                                evaluation_order,
                            },
                            stream_ty,
                        )
                    }
                    Err(err) => {
                        self.diagnostics.push(HirDiagnostic::new(
                            HirDiagnosticKind::CheckerBoundaryViolation {
                                name: format!("actor method `.{method}`"),
                                reason: err.to_string(),
                            },
                            span.clone(),
                            "checker-authoritative actor_method_dispatch stream element \
                                 type failed boundary conversion",
                        ));
                        (
                            HirExprKind::Unsupported(format!(
                                "actor method `.{method}` has poisoned dispatch stream \
                                     element type"
                            )),
                            ResolvedTy::Unit,
                        )
                    }
                }
            }
        }
    }

    /// Lower `receiver.method(args)` using the checker's method-call side-tables.
    ///
    /// Fail-closed per `checker-output-boundary` (LESSONS P0): a missing entry for
    /// this call site's span is a hard diagnostic — HIR never re-infers the runtime
    /// symbol from the receiver type.  Only `RewriteToFunction` is recognised
    /// here; every other rewrite kind is handled by its own lowering path.
    #[allow(
        clippy::too_many_lines,
        reason = "single linear lowering path with three exclusive branches \
                  (dyn-method dispatch / dyn-receiver fail-closed / legacy \
                  rewrite); splitting would scatter related fail-closed \
                  diagnostics across helpers"
    )]
    pub(super) fn lower_method_call(
        &mut self,
        receiver: &Spanned<Expr>,
        method: &str,
        args: &[hew_parser::ast::CallArg],
        span: Span,
        site: SiteId,
    ) -> (HirExprKind, ResolvedTy) {
        // Static-pool accessor method: `sup.pool.get(i)` / `sup.pool.len()`.
        // The checker recorded the resolved accessor for this method-call span;
        // lower to a `Call` whose args carry the `sup.pool` receiver (and the
        // index for `get`). MIR intercepts by site (`pool_accessor_sites`) and
        // emits the pool ABI — the synthetic callee is never resolved.
        let pool_key = self.mk_key(&span);
        if let Some(accessor) = self.pool_accessor_sites_checker.get(&pool_key).cloned() {
            let Some(ty) = self.expr_types.get(&pool_key) else {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::CheckerBoundaryViolation {
                        name: format!("supervisor pool accessor `{method}`"),
                        reason: "missing checker expression type".to_string(),
                    },
                    span.clone(),
                    "a supervisor pool accessor must carry its checker-resolved return type; generic machine payloads cannot default to i64",
                ));
                return (
                    HirExprKind::Unsupported(format!(
                        "supervisor pool accessor `{method}` is missing its return type"
                    )),
                    ResolvedTy::Unit,
                );
            };
            let result_ty = match ResolvedTy::from_ty(ty) {
                Ok(resolved) => resolved,
                Err(err) => {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::CheckerBoundaryViolation {
                            name: format!("supervisor pool accessor `{method}`"),
                            reason: err.to_string(),
                        },
                        span.clone(),
                        "a supervisor pool accessor must carry its checker-resolved return type; generic machine payloads cannot default to i64",
                    ));
                    return (
                        HirExprKind::Unsupported(format!(
                            "supervisor pool accessor `{method}` has an unresolved return type"
                        )),
                        ResolvedTy::Unit,
                    );
                }
            };
            let lowered_receiver = self.lower_expr(receiver, IntentKind::Read);
            let mut call_args = vec![lowered_receiver];
            for arg in args {
                let (expr, sp) = arg.expr();
                call_args.push(self.lower_expr(&(expr.clone(), sp.clone()), IntentKind::Read));
            }
            // The callee names the pool runtime symbol so the HIR call-shape
            // gate (`build_callable_set` includes these names) accepts it. MIR
            // routes by SITE (`pool_accessor_sites`), never by this callee, so
            // the name only has to be a recognised callable — the args carry the
            // real `sup.pool` receiver + index. The `Item` ref keeps the verifier
            // (which rejects only `Unresolved` BindingRefs) happy.
            let callee_symbol = match accessor.kind {
                hew_types::PoolAccessorKind::Len => "hew_supervisor_pool_len",
                _ => "hew_supervisor_pool_child_get",
            };
            let callee = HirExpr {
                node: self.ids.node(),
                site: self.ids.site(),
                ty: ResolvedTy::Unit,
                intent: IntentKind::Read,
                kind: HirExprKind::BindingRef {
                    name: callee_symbol.to_string(),
                    resolved: ResolvedRef::Item(crate::ids::ItemId(u32::MAX)),
                },
                span: span.clone(),
            };
            return (
                HirExprKind::Call {
                    target: CallTarget::Runtime(match accessor.kind {
                        hew_types::PoolAccessorKind::Len => {
                            hew_types::runtime_call::RuntimeCallFamily::SupervisorPoolLen
                        }
                        _ => hew_types::runtime_call::RuntimeCallFamily::SupervisorPoolChildGet,
                    }),
                    callee: Box::new(callee),
                    args: call_args,
                    evaluation_order: Vec::new(),
                },
                result_ty,
            );
        }

        // Intercept `.clone()` before any side-table lookup — but only when the
        // type checker has NOT already resolved the call to a user-defined method
        // (e.g. a user-declared `trait Clone { fn clone(val: Self) -> Self; }`).
        //
        // Fail-closed per the no-silent-stub invariant (M-COW P0):
        // `.clone()` must never silently return the same handle.  Collection
        // clones with a ready runtime deep-copy (`HashMap`/`HashSet` via
        // `hew_hashmap_clone_layout` / `hew_hashset_clone_layout`) are resolved
        // by the checker to a `ResolvedCall` and never reach this gate.  The
        // remaining heap types whose copy path is not yet wired (e.g.
        // `hew_bytes_clone_ref` for `Bytes`) stay fail-closed here: every
        // unresolved `.clone()` call is a compile error with an explicit
        // diagnostic so the user is never left guessing why their code "works"
        // but produces aliased references instead of independent copies.
        let key = self.mk_key(&span);
        if let Some(MethodCallReceiverKind::EnumConstructorPath { type_name }) =
            self.method_call_receiver_kinds.get(&key).cloned()
        {
            let constructor = format!("{type_name}::{method}");
            let checker_ctor_ty = self.checker_expr_ty_if_present(&span);
            let variant_kind = self
                .lookup_variant_ctor(&constructor, checker_ctor_ty.as_ref())
                .map(|(_, _, kind)| kind.clone());
            if let Some(HirVariantKind::Tuple(_)) = variant_kind {
                let lowered_args = self.lower_positional_call_args(args, &span);
                return self.lower_variant_ctor_tuple_call(&constructor, lowered_args, &span);
            }
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::CheckerBoundaryViolation {
                    name: constructor.clone(),
                    reason: "checker-selected enum constructor is absent from HIR registry"
                        .to_string(),
                },
                span.clone(),
                "dotted enum construction must carry the exact checker-selected declaration",
            ));
            return (
                HirExprKind::Unsupported(format!(
                    "dotted enum constructor `{constructor}` has no HIR declaration"
                )),
                ResolvedTy::Unit,
            );
        }
        if method == "clone"
            && args.is_empty()
            && !self.resolved_calls.contains_key(&key)
            && !self.method_call_rewrites.contains_key(&key)
            && !self.dyn_trait_method_calls.contains_key(&key)
        {
            let lowered_receiver = self.lower_expr(receiver, IntentKind::Read);
            let receiver_ty = lowered_receiver.ty.to_string();
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::CloneNotYetSupported {
                    receiver_ty: receiver_ty.clone(),
                },
                span.clone(),
                format!(
                    "`.clone()` on `{receiver_ty}` is not yet wired to a copy path \
                     in this phase (M-COW P0). The runtime deep-copy for heap types \
                     will be connected in P2. Restructure to avoid `.clone()` for now, \
                     or pass by value (which already retains under M-COW)."
                ),
            ));
            let ret_ty = lowered_receiver.ty.clone();
            return (
                HirExprKind::Unsupported(format!(
                    "`.clone()` on `{receiver_ty}` not yet supported"
                )),
                ret_ty,
            );
        }
        // Width-conversion methods (.wrapping_as_<W> / .saturating_as_<W>) take
        // zero arguments (only the receiver).  Checked before
        // `method_call_rewrites` because both families share the
        // `wrapping_`/`saturating_` prefix.
        if let Some(lowering) = self.width_cast_lowerings.get(&key).cloned() {
            if !args.is_empty() {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::CheckerBoundaryViolation {
                        name: format!("width-cast method `.{method}`"),
                        reason: format!(
                            "checker side-table expected zero arguments, found {}",
                            args.len()
                        ),
                    },
                    span.clone(),
                    "width-cast method lowering requires exactly zero arguments",
                ));
                return (
                    HirExprKind::Unsupported(format!(
                        "width-cast method `.{method}` has invalid arity"
                    )),
                    ResolvedTy::Unit,
                );
            }
            let Ok(from_ty) = ResolvedTy::from_ty(&lowering.from_ty) else {
                return (
                    HirExprKind::Unsupported(format!(
                        "width-cast method `.{method}` has poisoned source type"
                    )),
                    ResolvedTy::Unit,
                );
            };
            let Ok(to_ty) = ResolvedTy::from_ty(&lowering.to_ty) else {
                return (
                    HirExprKind::Unsupported(format!(
                        "width-cast method `.{method}` has poisoned target type"
                    )),
                    ResolvedTy::Unit,
                );
            };
            let lowered_receiver = self.lower_expr(receiver, IntentKind::Read);
            return match lowering.kind {
                hew_types::WidthCastKind::Wrapping => (
                    HirExprKind::NumericCast {
                        value: Box::new(lowered_receiver),
                        from_ty,
                        to_ty: to_ty.clone(),
                    },
                    to_ty,
                ),
                hew_types::WidthCastKind::Saturating => (
                    HirExprKind::SaturatingWidthCast {
                        value: Box::new(lowered_receiver),
                        from_range: lowering.from_range,
                        to_range: lowering.to_range,
                        from_ty,
                        to_ty: to_ty.clone(),
                    },
                    to_ty,
                ),
            };
        }
        if let Some(lowering) = self.try_width_cast_lowerings.get(&key).cloned() {
            if !args.is_empty() {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::CheckerBoundaryViolation {
                        name: format!("try-width-cast method `.{method}`"),
                        reason: format!(
                            "checker side-table expected zero arguments, found {}",
                            args.len()
                        ),
                    },
                    span.clone(),
                    "try-width-cast method lowering requires exactly zero arguments",
                ));
                return (
                    HirExprKind::Unsupported(format!(
                        "try-width-cast method `.{method}` has invalid arity"
                    )),
                    ResolvedTy::Unit,
                );
            }
            let Ok(from_ty) = ResolvedTy::from_ty(&lowering.from_ty) else {
                return (
                    HirExprKind::Unsupported(format!(
                        "try-width-cast method `.{method}` has poisoned source type"
                    )),
                    ResolvedTy::Unit,
                );
            };
            let Ok(to_ty) = ResolvedTy::from_ty(&lowering.to_ty) else {
                return (
                    HirExprKind::Unsupported(format!(
                        "try-width-cast method `.{method}` has poisoned target type"
                    )),
                    ResolvedTy::Unit,
                );
            };
            let Ok(result_ty) = ResolvedTy::from_ty(&Ty::option(lowering.to_ty)) else {
                return (
                    HirExprKind::Unsupported(format!(
                        "try-width-cast method `.{method}` has poisoned result type"
                    )),
                    ResolvedTy::Unit,
                );
            };
            let lowered_receiver = self.lower_expr(receiver, IntentKind::Read);
            return (
                HirExprKind::TryWidthCast {
                    value: Box::new(lowered_receiver),
                    from_range: lowering.from_range,
                    to_range: lowering.to_range,
                    from_ty,
                    to_ty,
                    kind: lowering.kind,
                },
                result_ty,
            );
        }
        if let Some(dispatch) = self.actor_method_dispatch.get(&key).cloned() {
            return self.lower_actor_method_call(dispatch, receiver, method, args, &span);
        }
        if matches!(
            self.method_call_receiver_kinds.get(&key),
            Some(MethodCallReceiverKind::ActorInstance { .. })
        ) {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::CheckerBoundaryViolation {
                    name: format!("actor method `.{method}`"),
                    reason: "missing actor_method_dispatch entry".to_string(),
                },
                span.clone(),
                "checker classified this method-call receiver as actor-typed but omitted the actor dispatch discriminator",
            ));
            return (
                HirExprKind::Unsupported(format!(
                    "actor method `.{method}` has no dispatch discriminator"
                )),
                ResolvedTy::Unit,
            );
        }
        // `dyn Trait` receivers take precedence: the checker's
        // `dyn_trait_method_calls` side-table pins the trait/method/slot
        // resolution authoritatively, and these calls do NOT have a
        // `method_call_rewrites` entry (a direct-call rewrite would
        // collapse the dispatch indirection that the vtable provides).
        if let Some(dyn_call) = self.dyn_trait_method_calls.get(&key).cloned() {
            let preserves_receiver = dyn_call.signature.returns_receiver_identity
                && self.method_call_preserves_receiver_identity.contains(&key);
            let receiver_intent = self.method_receiver_intent(
                &key,
                dyn_call.signature.consumes_receiver,
                preserves_receiver,
            );
            let lowered_receiver = self.lower_expr(receiver, receiver_intent);
            let lowered_args = self.lower_call_args(args, &span);
            // Result type comes from the checker's expr_types side-table
            // (the call's full span). Fail-closed if absent or poisoned.
            let ret_ty = self
                .checked_ty(&span)
                .cloned()
                .or_else(|| {
                    self.expr_types
                        .get(&key)
                        .cloned()
                        .and_then(|ty| ResolvedTy::from_ty(&ty).ok())
                })
                .unwrap_or(ResolvedTy::Unit);
            if !self.ensure_executable_target(&dyn_call.target, "dynamic trait call", &span) {
                return (
                    HirExprKind::Unsupported(
                        "dynamic trait call has no checker target".to_string(),
                    ),
                    ret_ty,
                );
            }
            // W4.047 P1.2: prove the typed handoff agrees at this fail-open
            // dyn-method-return site (no behaviour change).
            self.assert_resolved_ty_totality(&span);
            return (
                HirExprKind::CallDynMethod {
                    receiver: Box::new(lowered_receiver),
                    target: dyn_call.target,
                    trait_name: dyn_call.trait_name,
                    method_name: dyn_call.method_name,
                    slot: dyn_call.slot,
                    args: lowered_args.args,
                    evaluation_order: lowered_args.evaluation_order,
                    ret_ty: ret_ty.clone(),
                    signature: Box::new(dyn_call.signature),
                },
                ret_ty,
            );
        }
        // Receiver typed as `Ty::TraitObject` but no side-table entry:
        // fail-closed per `checker-output-boundary`. The checker MUST
        // populate `dyn_trait_method_calls` for every accepted call on
        // a trait-object receiver; missing entry is a hard diagnostic.
        if let Some(receiver_ty) = self.expr_types.get(&self.mk_key(&receiver.1)) {
            if matches!(receiver_ty, hew_types::Ty::TraitObject { .. }) {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::TraitObjectMethodNoSideTableEntry {
                        method: method.to_string(),
                    },
                    span.clone(),
                    "method call on `dyn Trait` receiver has no \
                     dyn_trait_method_calls side-table entry; the \
                     checker must record one before HIR lowering",
                ));
                return (
                    HirExprKind::Unsupported(format!(
                        "method call `.{method}` on dyn-trait with no side-table entry"
                    )),
                    ResolvedTy::Unit,
                );
            }
        }
        // Runtime invocation facts already select an executable contract.
        // They supersede the ordinary source-body resolver verdict recorded
        // while checking the same declaration's public method signature.
        let rewrite = self.method_call_rewrites.get(&key).cloned();
        let runtime_rewrite_selected = matches!(
            &rewrite,
            Some(MethodCallRewrite::RewriteToFunction {
                target: CallTarget::Runtime(_) | CallTarget::DeclaredRuntime { .. },
                ..
            })
        );
        // Look up the checker's `resolved_calls` verdict unconditionally so any
        // boundary-type conversion failure (TyPattern -> ResolvedTy mapping
        // bugs, `expr_types` side-table inconsistency, missing impl registration)
        // surfaces as a real `CheckerBoundaryViolation` diagnostic at the
        // boundary — not deferred until a runtime user hits the regression.
        // When a resolver verdict survives boundary conversion, we emit
        // `HirExprKind::ResolvedImplCall` carrying `MethodTarget.symbol_name`
        // verbatim; MIR/codegen consume that symbol via `Terminator::Call`.
        //
        // Precedence: when both `resolved_calls` and `method_call_rewrites`
        // have an entry for the same key (the current dual-emit overlap for
        // HashMap/HashSet during the C2→C3 transition), `resolved_calls`
        // wins — it carries the structured `(ImplId, MethodTarget)` verdict.
        // The legacy `method_call_rewrites` entries for that overlap are
        // removed in the C3 commit that retires the per-V allowlists, so the
        // precedence is exercised by construction once dual-emit retires.
        if !runtime_rewrite_selected {
            if let Some(resolved) = self.resolved_calls.get(&key).cloned() {
                // The checker's `expr_types` is the authoritative source for
                // the call-site result type (LESSONS `checker-authority`).
                // A missing or non-convertible entry is a checker boundary
                // violation — we record it eagerly so the gap is visible
                // long before the consumer is wired.
                let ret_ty = match self
                    .expr_types
                    .get(&key)
                    .cloned()
                    .map(|ty| ResolvedTy::from_ty(&ty))
                {
                    Some(Ok(ty)) => Some(ty),
                    Some(Err(err)) => {
                        self.diagnostics.push(HirDiagnostic::new(
                            HirDiagnosticKind::CheckerBoundaryViolation {
                                name: format!("resolved-impl call `.{method}`"),
                                reason: err.to_string(),
                            },
                            span.clone(),
                            "checker-resolved method call has poisoned result type",
                        ));
                        None
                    }
                    None => {
                        self.diagnostics.push(HirDiagnostic::new(
                            HirDiagnosticKind::CheckerBoundaryViolation {
                                name: format!("resolved-impl call `.{method}`"),
                                reason: "missing expr_types entry for call site".to_string(),
                            },
                            span.clone(),
                            "checker recorded a ResolvedCall for this site but no \
                         expr_types entry — the side-tables are inconsistent",
                        ));
                        None
                    }
                };
                #[allow(
                    clippy::items_after_statements,
                    reason = "the activation gate lives next to the guard it controls; \
                          promoting to a module-level const would scatter the \
                          deferral rationale across the file"
                )]
                const RESOLVED_IMPL_CALL_ACTIVATED: bool = true;
                if RESOLVED_IMPL_CALL_ACTIVATED {
                    // Only emit when the resolver verdict survived boundary
                    // conversion; otherwise fall through to the legacy
                    // `method_call_rewrites` arm below.
                    if let Some(ret_ty) = ret_ty {
                        if !self.ensure_executable_target(
                            &resolved.target,
                            "resolved impl call",
                            &span,
                        ) {
                            return (
                                HirExprKind::Unsupported(
                                    "resolved impl call has no checker target".to_string(),
                                ),
                                ret_ty,
                            );
                        }
                        // Register any enum instantiation that surfaces as the
                        // return type (e.g. `HashMap::get -> Option<V>`,
                        // `HashMap::remove -> Option<V>`). Without this the
                        // module-level `enum_layouts` table lacks the matching
                        // key for `Named { name: "Option", .. }`, MIR records
                        // `ValueClass::Unknown → Strategy::UnknownBlocked` at
                        // the call site, and the totality gate fires
                        // (`DecisionMapTotal { offending_sites: [...] }`).
                        // This mirrors the legacy `RewriteToFunction` arm
                        // below — the new resolver-authority path inherits
                        // the same boundary obligation.
                        self.try_register_enum_instantiation(&span);
                        let lowered_receiver = self.lower_expr(receiver, IntentKind::Read);
                        let lowered_args = self.lower_positional_call_args(args, &span);
                        return (
                            HirExprKind::ResolvedImplCall {
                                receiver: Box::new(lowered_receiver),
                                target: resolved.target,
                                impl_id: resolved.impl_id,
                                method_name: resolved.method_name,
                                target_symbol: resolved.method_target.symbol_name,
                                target_family: resolved.method_target.family,
                                type_args: resolved.type_args,
                                args: lowered_args,
                                ret_ty: ret_ty.clone(),
                            },
                            ret_ty,
                        );
                    }
                }
                // Dormant path: the lookup ran (and diagnostics fired if the
                // boundary failed), but emission is deferred. Fall through to
                // the legacy `method_call_rewrites` branch below so live sites
                // keep their current dispatch behaviour.
                let _ = resolved;
            }
        }
        match rewrite {
            Some(MethodCallRewrite::RcIntrinsic { op, payload_ty }) => {
                let result_ty = self
                    .resolved_expr_types
                    .get(&key)
                    .cloned()
                    .unwrap_or(ResolvedTy::Unit);
                let receiver = (op != RcIntrinsicOp::New)
                    .then(|| Box::new(self.lower_expr(receiver, IntentKind::Read)));
                let value = if matches!(op, RcIntrinsicOp::New | RcIntrinsicOp::Set) {
                    args.first()
                        .map(|arg| Box::new(self.lower_expr(arg.expr(), IntentKind::Consume)))
                } else {
                    None
                };
                (
                    HirExprKind::RcIntrinsic {
                        op,
                        payload_ty,
                        receiver,
                        value,
                        result_ty: result_ty.clone(),
                    },
                    result_ty,
                )
            }
            Some(
                rewrite @ (MethodCallRewrite::BuiltinVecIntoIter
                | MethodCallRewrite::BuiltinVecIter
                | MethodCallRewrite::BuiltinVecIterNext),
            ) => {
                let expected = if matches!(rewrite, MethodCallRewrite::BuiltinVecIterNext) {
                    BuiltinType::VecIter
                } else {
                    BuiltinType::Vec
                };
                let element = match self.checked_ty(&receiver.1) {
                    Some(ResolvedTy::Named {
                        builtin: Some(actual),
                        args,
                        ..
                    }) if *actual == expected && args.len() == 1 => args[0].clone(),
                    _ => {
                        self.diagnostics.push(HirDiagnostic::new(
                            HirDiagnosticKind::CheckerBoundaryViolation {
                                name: "vector iteration".into(),
                                reason: "cursor operation lacks its exact checked receiver type"
                                    .into(),
                            },
                            span.clone(),
                            "vector iteration requires a resolved receiver",
                        ));
                        return (
                            HirExprKind::Unsupported(
                                "vector iteration receiver is unresolved".into(),
                            ),
                            ResolvedTy::Unit,
                        );
                    }
                };
                match rewrite {
                    MethodCallRewrite::BuiltinVecIntoIter => {
                        self.lower_builtin_vec_into_iter(receiver, element, span)
                    }
                    MethodCallRewrite::BuiltinVecIter => {
                        self.lower_builtin_vec_iter(receiver, element, span)
                    }
                    MethodCallRewrite::BuiltinVecIterNext => {
                        self.lower_builtin_vec_iter_next(receiver, &element, span)
                    }
                    _ => unreachable!("matched cursor operation"),
                }
            }
            Some(MethodCallRewrite::BuiltinHashMapIntoIter { key_ty, val_ty }) => {
                self.lower_builtin_hashmap_into_iter(receiver, &key_ty, &val_ty, span)
            }
            Some(MethodCallRewrite::BuiltinVecHigherOrder {
                op,
                elem_ty,
                out_ty,
            }) => self.lower_builtin_vec_higher_order(receiver, args, op, &elem_ty, &out_ty, span),
            Some(MethodCallRewrite::VecFrom) => (
                HirExprKind::Unsupported("Vec.from is a static-call rewrite".to_string()),
                ResolvedTy::Unit,
            ),
            Some(MethodCallRewrite::RecordFnFieldCall { field_ty }) => {
                self.lower_record_fn_field_call(receiver, method, args, &field_ty, span)
            }
            Some(MethodCallRewrite::WireCodec {
                direction,
                value_ty,
            }) => self.lower_wire_codec(receiver, args, direction, value_ty, span),
            Some(MethodCallRewrite::GenericWireCodec {
                direction,
                value_ty,
            }) => self.lower_generic_wire_codec(args, direction, value_ty, span),
            Some(MethodCallRewrite::RemoteActorSend) => {
                self.try_register_enum_instantiation(&span);
                let ret_ty = self
                    .expr_types
                    .get(&key)
                    .cloned()
                    .and_then(|ty| ResolvedTy::from_ty(&ty).ok());
                let (Some(ret_ty), [msg]) = (ret_ty, args) else {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::CheckerBoundaryViolation {
                            name: "RemotePid.send".to_string(),
                            reason: "checker side-table lacks the message or result type"
                                .to_string(),
                        },
                        span.clone(),
                        "remote actor send lowering requires its checked message and result",
                    ));
                    return (
                        HirExprKind::Unsupported("RemotePid.send lost its checked facts".into()),
                        ResolvedTy::Unit,
                    );
                };
                let receiver = Box::new(self.lower_expr(receiver, IntentKind::Read));
                let msg = Box::new(self.lower_expr(msg.expr(), IntentKind::Read));
                let status = self.make_expr(
                    HirExprKind::RemoteActorSend { receiver, msg },
                    ResolvedTy::I32,
                    IntentKind::Read,
                    span.clone(),
                );
                self.lower_send_status_result(status, ret_ty, REMOTE_SEND_STATUS, &span)
            }
            Some(MethodCallRewrite::RemoteActorAsk) => {
                self.try_register_enum_instantiation(&span);
                if args.len() != 2 {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::CheckerBoundaryViolation {
                            name: "RemotePid.ask".to_string(),
                            reason: format!(
                                "checker side-table expected two arguments, found {}",
                                args.len()
                            ),
                        },
                        span.clone(),
                        "remote actor ask lowering requires message and timeout arguments",
                    ));
                    return (
                        HirExprKind::Unsupported("RemotePid.ask has invalid arity".into()),
                        ResolvedTy::Unit,
                    );
                }
                let ret_ty = self
                    .expr_types
                    .get(&key)
                    .cloned()
                    .and_then(|ty| ResolvedTy::from_ty(&ty).ok())
                    .map_or(ResolvedTy::Unit, |ty| {
                        self.qualify_current_module_record_ty(ty)
                    });
                let reply_ty = match &ret_ty {
                    ResolvedTy::Named {
                        builtin: Some(BuiltinType::Result),
                        args,
                        ..
                    } => args.first().cloned(),
                    _ => None,
                };
                let Some(reply_ty) = reply_ty else {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::CheckerBoundaryViolation {
                            name: "RemotePid.ask".to_string(),
                            reason: format!(
                                "expected Result<Reply, ActorError>, got {}",
                                ret_ty.user_facing()
                            ),
                        },
                        span.clone(),
                        "remote actor ask lowering requires a Result return type",
                    ));
                    return (
                        HirExprKind::Unsupported("RemotePid.ask has poisoned return type".into()),
                        ResolvedTy::Unit,
                    );
                };
                let lowered_receiver = self.lower_expr(receiver, IntentKind::Read);
                let lowered_msg = self.lower_expr(args[0].expr(), IntentKind::Read);
                let lowered_timeout = self.lower_expr(args[1].expr(), IntentKind::Read);
                (
                    HirExprKind::RemoteActorAsk {
                        receiver: Box::new(lowered_receiver),
                        msg: Box::new(lowered_msg),
                        timeout_ms: Box::new(lowered_timeout),
                        reply_ty,
                    },
                    ret_ty,
                )
            }
            Some(MethodCallRewrite::RewriteToFunction {
                target,
                c_symbol,
                descriptor,
                consumes_receiver,
                requires_mutable_receiver,
                receiver_update,
                returns_receiver_identity,
                ..
            }) => {
                if !self.ensure_executable_target(&target, &c_symbol, &span) {
                    return (
                        HirExprKind::Unsupported(
                            "method call has no checker declaration target".to_string(),
                        ),
                        ResolvedTy::Unit,
                    );
                }
                // S5: a method-call rewrite that lands on a builtin-generic
                // enum result type (e.g. `Result<(), SendError>` for
                // `RemotePid<T>::send`) needs the per-instantiation enum
                // layout registered here. Unlike struct-ctor/variant-ctor/
                // match-scrutinee paths — which already invoke
                // `try_register_enum_instantiation` — the rewrite arm builds
                // a synthetic `HirExprKind::Call` whose return type is the
                // checker-recorded `Result<...>`. Without explicit
                // registration MIR sees the `Named { name: "Result", .. }`
                // type but `module.enum_layouts` lacks the matching key,
                // and `machine_layout_names` therefore omits "Result" →
                // `UnknownType { name: "Result" }` plus
                // `ValueClass::Unknown → Strategy::UnknownBlocked` at the
                // MIR boundary.
                self.try_register_enum_instantiation(&span);
                // Read the actual return type from the checker's expr_types table.
                // Unit-returning methods (e.g. channel send/recv) record Unit there,
                // so the fallback is safe and this path remains correct for all callers.
                // `params` is empty — the call arg list carries the real args.
                let ret_ty = self
                    .expr_types
                    .get(&key)
                    .cloned()
                    .and_then(|ty| ResolvedTy::from_ty(&ty).ok())
                    .map_or(ResolvedTy::Unit, |ty| {
                        self.qualify_current_module_record_ty(ty)
                    });
                if matches!(target, CallTarget::DeclaredRuntime { .. }) {
                    return self.lower_declared_runtime_invocation(
                        target,
                        receiver,
                        args,
                        consumes_receiver,
                        ret_ty,
                        &span,
                    );
                }
                let c_symbol = match &target {
                    CallTarget::ImplMethod(declaration) => {
                        let Some(symbol) = self.registered_impl_method_symbol(declaration) else {
                            self.diagnostics.push(HirDiagnostic::new(
                                HirDiagnosticKind::CallableUnsupportedInMir {
                                    name: declaration.full_path().to_string(),
                                },
                                span.clone(),
                                "checker selected an implementation declaration whose HIR body \
                                 was not registered; direct method dispatch cannot be lowered",
                            ));
                            return (
                                HirExprKind::Unsupported(
                                    "implementation call has no registered HIR body".to_string(),
                                ),
                                ret_ty,
                            );
                        };
                        symbol
                    }
                    _ => c_symbol,
                };
                if !self.ensure_executable_target(&target, "static trait call", &span) {
                    return (
                        HirExprKind::Unsupported(
                            "static trait call has no checker target".to_string(),
                        ),
                        ret_ty,
                    );
                }
                // W4.047 P1.2: prove the typed handoff agrees at this fail-open
                // receiver-method-rewrite return site (no behaviour change).
                self.assert_resolved_ty_totality(&span);
                if requires_mutable_receiver {
                    let lowered_receiver = self.lower_expr(receiver, IntentKind::Consume);
                    let receiver_ty = lowered_receiver.ty.clone();
                    self.record_var_self_direct_monomorphisation(
                        &c_symbol,
                        &receiver_ty,
                        &span,
                        site,
                    );
                    let lowered_args = self.lower_call_args(args, &span);
                    return (
                        HirExprKind::VarSelfMethodCall {
                            receiver_update,
                            receiver: Box::new(lowered_receiver),
                            call_target: target,
                            target: HirVarSelfMethodTarget::Direct,
                            args: lowered_args.args,
                            evaluation_order: lowered_args.evaluation_order,
                            ret_ty: ret_ty.clone(),
                            receiver_ty,
                        },
                        ret_ty,
                    );
                }
                // Lower receiver + args, then prepend receiver as first argument.
                // A consuming handle-release call (`.close()`-family: the
                // checker set `consumes_receiver` from the resolved runtime
                // symbol) takes ownership of the receiver, so lower it with
                // `IntentKind::Consume`. The MIR move-checker then marks the
                // handle moved-out and excludes it from the function-exit drop
                // set — a second close at scope exit would double-free the
                // underlying resource (LESSONS: raii-null-after-move,
                // cleanup-all-exits). Borrowing methods (`send`/`recv`) keep
                // `IntentKind::Read`.
                let preserves_receiver = returns_receiver_identity
                    && self.method_call_preserves_receiver_identity.contains(&key);
                let receiver_intent =
                    self.method_receiver_intent(&key, consumes_receiver, preserves_receiver);
                let lowered_receiver = self.lower_expr(receiver, receiver_intent);
                // `c_symbol` is either projected from the exact selected impl
                // declaration above or carried by a typed runtime/user target.
                // Never rediscover an imported owner from receiver/name leaf
                // equality: same-named types in sibling modules can both have
                // registered methods, making that fallback select a real but
                // wrong body.
                // Slice 2: a by-value direct-dot call to a generic impl method
                // on a concrete generic receiver (e.g. `p.first()` where
                // `impl<T> Pair<T> { fn first(self) -> T }` and `p: Pair<i64>`)
                // needs the impl-method monomorphisation registered so MIR has a
                // concrete body to dispatch to. The var-self (mutable-receiver)
                // branch above already does this via
                // `record_var_self_direct_monomorphisation`; the by-value branch
                // omitted it, so the call lowered to a `Call` whose mangled
                // callee (`Pair::first$$i64`) had no MIR body and failed closed
                // at the MIR boundary ("resolved callee has no MIR body").
                // `record_var_self_direct_monomorphisation` is a no-op for
                // non-generic / non-named-receiver / builtin callees, so this is
                // safe to call unconditionally for every RewriteToFunction by-value
                // method dispatch: it derives the concrete type-args from the
                // receiver type's args, registers the monomorphisation, and seeds
                // `call_site_type_args[site]` so MIR's `Call` arm mangles to the
                // same per-instantiation symbol.
                self.record_var_self_direct_monomorphisation(
                    &c_symbol,
                    &lowered_receiver.ty,
                    &span,
                    site,
                );
                let method_args = self.lower_call_args(args, &span);
                let evaluation_order = method_args.order_after_receiver();
                let mut lowered_args = vec![lowered_receiver];
                lowered_args.extend(method_args.args);
                // Closed-set builtin rewrites carry the checker-resolved
                // descriptor: resolve the callee to the typed family so MIR
                // dispatches on the resolution, not the name string. Rewrites
                // without a descriptor (open-set `#[extern_symbol]` methods,
                // user `Type::method` keys, pre-catalog symbols) fall back to
                // the seeded stdlib fn_registry (`seed_stdlib_fn_registry`),
                // matching the `RewriteModuleQualifiedToFunction` arm just
                // below. Without that fallback the BindingRef stays
                // `Unresolved` and the verifier emits
                // `UnresolvedSymbol(<c_symbol>)` even though the registry
                // entry exists.
                let resolved_ref = descriptor.as_ref().map_or_else(
                    || {
                        self.fn_registry
                            .get(&c_symbol)
                            .map_or(ResolvedRef::Unresolved, |entry| ResolvedRef::Item(entry.id))
                    },
                    |d| ResolvedRef::Builtin(d.family()),
                );
                // `Sink.send` / `Sink.try_send` answer with a runtime status
                // (0 accepted, 1 closed, 2 full) that this site folds into the
                // checked `Result<(), SendError>`; the call itself is typed as
                // that status.
                let send_status = matches!(
                    c_symbol.as_str(),
                    "hew_stream_send_layout" | "hew_stream_try_send_layout"
                );
                let call_ty = if send_status {
                    ResolvedTy::I32
                } else {
                    ret_ty.clone()
                };
                let callee = HirExpr {
                    node: self.ids.node(),
                    site: self.ids.site(),
                    ty: ResolvedTy::Function {
                        capabilities: hew_types::CallableCapabilities::FUNCTION_ITEM,
                        params: Vec::new(),
                        ret: Box::new(call_ty.clone()),
                    },
                    intent: IntentKind::Read,
                    kind: HirExprKind::BindingRef {
                        name: c_symbol,
                        resolved: resolved_ref,
                    },
                    span: span.clone(),
                };
                let call = HirExprKind::Call {
                    target,
                    callee: Box::new(callee),
                    args: lowered_args,
                    evaluation_order,
                };
                if send_status {
                    let call = self.make_expr(call, call_ty, IntentKind::Read, span.clone());
                    return self.lower_send_status_result(call, ret_ty, PIPE_SEND_STATUS, &span);
                }
                (call, ret_ty)
            }
            Some(MethodCallRewrite::GenericMathIntrinsic { op }) => {
                let LoweredCallArgs {
                    args: lowered_args,
                    evaluation_order,
                } = self.lower_call_args(args, &span);
                let checked_ret_ty = self
                    .expr_types
                    .get(&key)
                    .map(Ty::materialize_literal_defaults)
                    .and_then(|ty| ResolvedTy::from_ty(&ty).ok());
                let dispatch_ty = checked_ret_ty
                    .as_ref()
                    .filter(|ty| stdlib_catalog::generic_math_intrinsic_callee(op, ty).is_some())
                    .cloned()
                    .or_else(|| lowered_args.first().map(|arg| arg.ty.clone()));
                let Some(dispatch_ty) = dispatch_ty else {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::CheckerBoundaryViolation {
                            name: format!("math.{method}"),
                            reason: "generic math intrinsic had no lowered arguments".to_string(),
                        },
                        span.clone(),
                        "generic math intrinsic dispatch requires at least one checked argument",
                    ));
                    return (
                        HirExprKind::Unsupported(format!(
                            "generic math intrinsic `math.{method}` has no dispatch type"
                        )),
                        ResolvedTy::Unit,
                    );
                };
                let Some((symbol, ret_ty)) =
                    stdlib_catalog::generic_math_intrinsic_callee(op, &dispatch_ty)
                else {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::CheckerBoundaryViolation {
                            name: format!("math.{method}"),
                            reason: format!(
                                "no generic math intrinsic for operand type `{dispatch_ty}`"
                            ),
                        },
                        span.clone(),
                        "current math intrinsic dispatch supports i64 and f64 operands",
                    ));
                    return (
                        HirExprKind::Unsupported(format!(
                            "generic math intrinsic `math.{method}` unsupported for `{dispatch_ty}`"
                        )),
                        ResolvedTy::Unit,
                    );
                };
                let resolved_ref =
                    self.fn_registry
                        .get(symbol)
                        .map_or(ResolvedRef::Unresolved, |entry| {
                            entry
                                .builtin_family
                                .map_or(ResolvedRef::Item(entry.id), ResolvedRef::Builtin)
                        });
                let callee_ty = ResolvedTy::Function {
                    capabilities: hew_types::CallableCapabilities::FUNCTION_ITEM,
                    params: Vec::new(),
                    ret: Box::new(ret_ty.clone()),
                };
                let callee = HirExpr {
                    node: self.ids.node(),
                    site: self.ids.site(),
                    ty: callee_ty,
                    intent: IntentKind::Read,
                    kind: HirExprKind::BindingRef {
                        name: symbol.to_string(),
                        resolved: resolved_ref,
                    },
                    span: span.clone(),
                };
                (
                    HirExprKind::Call {
                        target: self.registered_symbol_target(symbol),
                        callee: Box::new(callee),
                        args: lowered_args,
                        evaluation_order,
                    },
                    ret_ty,
                )
            }
            Some(MethodCallRewrite::RewriteModuleQualifiedToFunction {
                target, c_symbol, ..
            }) => {
                // Module-qualified direct call: the receiver expression is the
                // module identifier, not a value. Lower the args only — do NOT
                // prepend the receiver (LESSONS `module-qualified-rewrite-authority`).
                // The namespaced `module::fn(args)` Call form routes through the
                // same helper from `lower_regular_call`.
                let lowered_args = self.lower_call_args(args, &span);
                let (call_kind, ret_ty) = self.lower_module_qualified_direct_call_lowered(
                    target,
                    &c_symbol,
                    lowered_args,
                    &span,
                    site,
                );
                (call_kind, ret_ty)
            }
            Some(MethodCallRewrite::DeferToLowering) => {
                // `DeferToLowering` belonged to the legacy codegen pipeline and is
                // not consumed by the Rust MIR pipeline.  Fail-closed.
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::NotYetImplemented {
                        construct: format!("method-call rewrite variant for `.{method}`"),
                        owning_pass: "mir-pipeline".to_string(),
                    },
                    span,
                    "this method-call rewrite variant is not supported in the Rust MIR pipeline",
                ));
                (
                    HirExprKind::Unsupported(format!(
                        "unsupported rewrite variant for method `{method}`"
                    )),
                    ResolvedTy::Unit,
                )
            }
            Some(MethodCallRewrite::CancellationTokenIsCancelled) => {
                let lowered_receiver = self.lower_expr(receiver, IntentKind::Read);
                (
                    HirExprKind::CancellationTokenIsCancelled {
                        receiver: Box::new(lowered_receiver),
                    },
                    ResolvedTy::Bool,
                )
            }
            Some(MethodCallRewrite::GeneratorNext { yield_ty }) => {
                // The result is `Option<yield_ty>`; register its layout so MIR /
                // codegen can size the enum slot the unbox writes into.
                self.register_option_layout(&yield_ty, &span, "Generator::next");
                let option_ty = Self::resolved_option_ty(yield_ty.clone());
                // The receiver is borrowed (Read): the coro `.next()` drive
                // resumes the generator but does not consume the handle — it
                // stays live for subsequent `.next()` calls and is freed by
                // `hew_gen_coro_destroy` on its own scope-exit drop.
                let lowered_receiver = self.lower_expr(receiver, IntentKind::Read);
                (
                    HirExprKind::GeneratorNext {
                        receiver: Box::new(lowered_receiver),
                        yield_ty,
                    },
                    option_ty,
                )
            }
            Some(MethodCallRewrite::StaticTraitDispatch {
                target,
                receiver_type_param,
                requires_mutable_receiver,
                consumes_receiver,
                returns_receiver_identity,
            }) => {
                let ret_ty = self
                    .expr_types
                    .get(&key)
                    .cloned()
                    .and_then(|ty| ResolvedTy::from_ty(&ty).ok())
                    .unwrap_or(ResolvedTy::Unit);
                // W4.047 P1.2: prove the typed handoff agrees at this fail-open
                // static-trait-dispatch return site (no behaviour change).
                self.assert_resolved_ty_totality(&span);

                // Trait default method body — concrete Self interception.
                //
                // When a trait default method body is lowered as a concrete
                // impl method (e.g. `Person::greet` instantiated from the
                // `Greeter::greet` default), the checker recorded
                // `StaticTraitDispatch { receiver_type_param: "Self" }` for
                // every `self.sibling_method()` call inside that body.  MIR
                // then tries to resolve "Self" through its monomorphization
                // substitution map — but there is no substitution for a
                // concrete function, causing `UnresolvedStaticDispatchSubstitution`.
                //
                // When `current_impl_self_ty` is set (we are inside
                // `lower_impl_block`) and `receiver_type_param == "Self"`,
                // the receiver is concretely known.  Derive the qualified
                // symbol `<ConcreteType>::<method>` and emit a direct `Call`
                // — the same shape a non-default trait-impl method would emit.
                // This bypasses `CallTraitMethodStatic` entirely for the
                // default-body context and lets MIR treat it as an ordinary
                // concrete call.
                // The checker-selected trait method declaration names the
                // member; its `Owner::method` path carries the method leaf.
                let trait_method_leaf = match &target {
                    CallTarget::StaticTraitMethod { method, .. } => method
                        .full_path()
                        .rsplit_once("::")
                        .map(|(_, leaf)| leaf.to_string()),
                    _ => None,
                };
                if let (true, Some(method_leaf)) =
                    (receiver_type_param == "Self", trait_method_leaf)
                {
                    if let Some(self_ty) = self.current_impl_self_ty.clone() {
                        if let Some(self_type) = self_ty.impl_receiver_instance() {
                            let c_symbol = crate::node::HirImplBlock::method_symbol(
                                self_type.nominal.declaration().full_path(),
                                &method_leaf,
                            );
                            let concrete_target = self.registered_symbol_target(&c_symbol);
                            if !self.ensure_executable_target(&concrete_target, &c_symbol, &span) {
                                return (
                                    HirExprKind::Unsupported(format!(
                                        "trait default call `{c_symbol}` has no concrete target"
                                    )),
                                    ret_ty,
                                );
                            }
                            // The identity path locates the declaration; it is
                            // not the symbol the body was emitted under. An
                            // impl written through a module binding
                            // (`impl Tagged for json.Value`) emits under the
                            // spelling the source wrote, so project the
                            // emitted symbol from the declaration rather than
                            // rebuilding it from the identity.
                            let c_symbol = match &concrete_target {
                                CallTarget::ImplMethod(declaration) => {
                                    let Some(symbol) =
                                        self.registered_impl_method_symbol(declaration)
                                    else {
                                        self.diagnostics.push(HirDiagnostic::new(
                                            HirDiagnosticKind::CallableUnsupportedInMir {
                                                name: declaration.full_path().to_string(),
                                            },
                                            span.clone(),
                                            "checker selected an implementation declaration whose                                              HIR body was not registered; a trait default body                                              cannot dispatch to it",
                                        ));
                                        return (
                                            HirExprKind::Unsupported(
                                                "trait default call has no registered HIR body"
                                                    .to_string(),
                                            ),
                                            ret_ty,
                                        );
                                    };
                                    symbol
                                }
                                _ => c_symbol,
                            };
                            self.try_register_enum_instantiation(&span);
                            self.record_var_self_direct_monomorphisation(
                                &c_symbol,
                                // receiver type will be the concrete self type
                                &self_ty, &span, site,
                            );
                            let preserves_receiver = returns_receiver_identity
                                && self.method_call_preserves_receiver_identity.contains(&key);
                            let receiver_intent = self.method_receiver_intent(
                                &key,
                                requires_mutable_receiver || consumes_receiver,
                                preserves_receiver,
                            );
                            let lowered_receiver = self.lower_expr(receiver, receiver_intent);
                            let method_args = self.lower_call_args(args, &span);
                            let evaluation_order = method_args.order_after_receiver();
                            let mut lowered_args = vec![lowered_receiver];
                            lowered_args.extend(method_args.args);
                            let callee_ty = ResolvedTy::Function {
                                capabilities: hew_types::CallableCapabilities::FUNCTION_ITEM,
                                params: Vec::new(),
                                ret: Box::new(ret_ty.clone()),
                            };
                            let resolved_ref = self
                                .fn_registry
                                .get(&c_symbol)
                                .map_or(ResolvedRef::Unresolved, |entry| {
                                    ResolvedRef::Item(entry.id)
                                });
                            let callee = HirExpr {
                                node: self.ids.node(),
                                site: self.ids.site(),
                                ty: callee_ty,
                                intent: IntentKind::Read,
                                kind: HirExprKind::BindingRef {
                                    name: c_symbol,
                                    resolved: resolved_ref,
                                },
                                span: span.clone(),
                            };
                            return (
                                HirExprKind::Call {
                                    target: concrete_target,
                                    callee: Box::new(callee),
                                    args: lowered_args,
                                    evaluation_order,
                                },
                                ret_ty,
                            );
                        }
                    }
                }

                self.record_static_trait_type_args(&span, site);

                if requires_mutable_receiver {
                    let lowered_receiver = self.lower_expr(receiver, IntentKind::Consume);
                    let receiver_ty = lowered_receiver.ty.clone();
                    let lowered_args = self.lower_call_args(args, &span);
                    return (
                        HirExprKind::VarSelfMethodCall {
                            receiver_update: hew_types::ReceiverUpdate::Replace,
                            receiver: Box::new(lowered_receiver),
                            call_target: target,
                            target: HirVarSelfMethodTarget::StaticTrait {
                                receiver_type_param,
                            },
                            args: lowered_args.args,
                            evaluation_order: lowered_args.evaluation_order,
                            ret_ty: ret_ty.clone(),
                            receiver_ty,
                        },
                        ret_ty,
                    );
                }
                // Static trait dispatch: emit `CallTraitMethodStatic` carrying
                // the structured metadata. MIR resolves the concrete callee from
                // the monomorphization substitution map.
                let preserves_receiver = returns_receiver_identity
                    && self.method_call_preserves_receiver_identity.contains(&key);
                let receiver_intent =
                    self.method_receiver_intent(&key, consumes_receiver, preserves_receiver);
                let lowered_receiver = self.lower_expr(receiver, receiver_intent);
                let lowered_args = self.lower_call_args(args, &span);
                (
                    self.make_static_trait_dispatch_call(
                        lowered_receiver,
                        target,
                        receiver_type_param,
                        lowered_args,
                        ret_ty.clone(),
                        &span,
                    ),
                    ret_ty,
                )
            }
            // `clone p` or `p.clone()` on a user-defined record type: lower to a
            // `RecordCloneCall` node. MIR lowers this to an alloca + memcpy +
            // call to `__hew_record_clone_inplace_<record_name>`.
            // Non-consuming read of the receiver — the original stays live.
            Some(MethodCallRewrite::RecordCloneInplace { record_name }) => {
                // Prefer the checker-typed result; fall back to constructing
                // a Named type from the record name (should always resolve).
                let ret_ty = self
                    .expr_types
                    .get(&key)
                    .and_then(|ty| ResolvedTy::from_ty(ty).ok())
                    .unwrap_or_else(|| ResolvedTy::Named {
                        name: record_name.clone(),
                        args: vec![],
                        builtin: None,
                        is_opaque: false,
                    });
                let sym = format!("__hew_record_clone_inplace_{record_name}");
                let lowered_receiver = self.lower_expr(receiver, IntentKind::Read);
                (
                    HirExprKind::RecordCloneCall {
                        src: Box::new(lowered_receiver),
                        clone_fn_sym: sym,
                        record_name,
                    },
                    ret_ty,
                )
            }
            // `clone x` or `x.clone()` on a Copy/BitCopy type: lower as a plain
            // read of the receiver — BitCopy semantics already duplicate on use.
            // The checker emitted a `StyleSuggestion` warning at check time.
            // Unwrap the receiver's inner kind directly; we do not insert any
            // new HIR node — the "result" is the operand itself.
            Some(MethodCallRewrite::CopyCloneNoop) => {
                let lowered_receiver = self.lower_expr(receiver, IntentKind::Read);
                let ty = lowered_receiver.ty.clone();
                (
                    HirExprKind::SubsumedValue {
                        source: Box::new(lowered_receiver),
                    },
                    ty,
                )
            }
            None => {
                if let Expr::Identifier(module_name) = &receiver.0 {
                    if let Some(module) = self.missing_stdlib_module_import(module_name) {
                        let name = format!("{module_name}.{method}");
                        let source_module = module.replace("::", ".");
                        self.diagnostics.push(HirDiagnostic::new(
                            HirDiagnosticKind::ImportMissing {
                                module: source_module,
                                name: name.clone(),
                            },
                            span.clone(),
                            stdlib_catalog::missing_import_hint(module).replace("::", "."),
                        ));
                        return (
                            // This source call was rejected by the checker/import
                            // boundary. Do not materialize a callable HIR node with
                            // a synthetic target: downstream MIR intentionally does
                            // not recover target identity from an unresolved callee.
                            HirExprKind::Unsupported(format!("unresolved module call `{name}`")),
                            ResolvedTy::Unit,
                        );
                    }
                    if let Some(MethodCallReceiverKind::ModuleBinding {
                        module_name: resolved_module,
                    }) = self.method_call_receiver_kinds.get(&key)
                    {
                        let name = format!("{module_name}.{method}");
                        self.diagnostics.push(HirDiagnostic::new(
                            HirDiagnosticKind::CheckerBoundaryViolation {
                                name,
                                reason: format!(
                                    "missing module-qualified call rewrite for `{resolved_module}`"
                                ),
                            },
                            span.clone(),
                            "checker admitted a user module-qualified call without its canonical rewrite target",
                        ));
                        return (
                            HirExprKind::Unsupported(format!(
                                "module call `{module_name}.{method}` has no rewrite entry"
                            )),
                            ResolvedTy::Unit,
                        );
                    }
                }
                // No rewrite entry — fail closed.  Do not re-infer from the receiver type.
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::MethodCallNoRewrite {
                        method: method.to_string(),
                    },
                    span,
                    "no checker-produced rewrite entry for this method call; \
                     typecheck must record a rewrite before HIR lowering",
                ));
                (
                    HirExprKind::Unsupported(format!(
                        "method call `.{method}` has no rewrite entry"
                    )),
                    ResolvedTy::Unit,
                )
            }
        }
    }
}
