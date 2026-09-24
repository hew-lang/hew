//! Call-argument, call-target and ordinary call lowering.

use super::*;

impl LowerCtx {
    /// Lower a call's ordinary (non-receiver) arguments, choosing each
    /// argument's move-vs-borrow intent through `arg_move_intent`. A by-value
    /// user-`#[resource]` argument is lowered `Consume` (an ownership move into
    /// the callee); every other argument — builtin handles passed to borrowing
    /// intrinsics included — keeps the borrowing `Read` default. This is the
    /// single funnel every free-call and method-call argument list flows
    /// through so the value-move consume decision lives in exactly one place.
    pub(super) fn lower_call_args(&mut self, args: &[CallArg], span: &Span) -> LoweredCallArgs {
        self.lower_call_args_for_callee(args, span, None)
    }

    /// Lower call arguments after the caller has identified an optional direct
    /// C-ABI symbol. All other call shapes use the normal by-value ownership
    /// intent; only a declared extern can consult the FFI contract table.
    pub(super) fn lower_call_args_for_callee(
        &mut self,
        args: &[CallArg],
        span: &Span,
        symbol: Option<&str>,
    ) -> LoweredCallArgs {
        self.lower_call_args_by_slot(args, span, |this, slot, arg| {
            this.call_arg_move_intent(symbol, slot, &arg.1)
        })
    }

    /// Lower arguments in source order and place each in the parameter slot
    /// the checker bound it to.
    pub(super) fn lower_call_args_by_slot(
        &mut self,
        args: &[CallArg],
        span: &Span,
        mut intent: impl FnMut(&mut Self, usize, &Spanned<Expr>) -> IntentKind,
    ) -> LoweredCallArgs {
        let slots = self
            .call_argument_slots
            .get(&self.mk_key(span))
            .cloned()
            .unwrap_or_default();
        let mut placed: Vec<Option<HirExpr>> = args.iter().map(|_| None).collect();
        for (index, arg) in args.iter().enumerate() {
            let slot = slots.get(index).copied().unwrap_or(index);
            let arg = arg.expr();
            let intent = intent(self, slot, arg);
            placed[slot] = Some(self.lower_expr(arg, intent));
        }
        LoweredCallArgs {
            args: placed
                .into_iter()
                .map(|arg| arg.expect("checker argument slots are a permutation"))
                .collect(),
            evaluation_order: slots,
        }
    }

    /// Lower the arguments of a callee that takes positional arguments only;
    /// the checker refused names for it, so no slot fact can exist.
    pub(super) fn lower_positional_call_args(
        &mut self,
        args: &[CallArg],
        span: &Span,
    ) -> Vec<HirExpr> {
        let lowered = self.lower_call_args(args, span);
        if !lowered.evaluation_order.is_empty() {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::CheckerBoundaryViolation {
                    name: "named call arguments".to_string(),
                    reason: "callee takes positional arguments only".to_string(),
                },
                span.clone(),
                "checker bound named arguments for a positional callee",
            ));
        }
        lowered.args
    }

    /// True when the checker typed the expression at `span` as a pipe
    /// half (`Sink<T>` / `Stream<T>`). Resolves through
    /// `ResolvedTy::from_ty` so the decision rides the typed builtin
    /// discriminant, never the (possibly module-qualified) name string.
    /// Absent or unconvertible entries answer `false` — the conservative
    /// no-ownership-transfer default.
    pub(super) fn checked_span_is_pipe_handle(&self, span: &Span) -> bool {
        self.expr_types
            .get(&self.mk_key(span))
            .and_then(|ty| ResolvedTy::from_ty(ty).ok())
            .is_some_and(|resolved| {
                matches!(
                    resolved,
                    ResolvedTy::Named {
                        builtin: Some(
                            hew_types::BuiltinType::Sink | hew_types::BuiltinType::Stream
                        ),
                        ..
                    }
                )
            })
    }

    /// Whether `ty` transitively carries a value whose SOLE ownership crosses
    /// an actor message boundary: a substrate handle (the builtin list owned by
    /// [`BuiltinType::transfers_ownership_across_actor_boundary`], shared with
    /// the env checker), or a user `#[resource]` / `#[linear]` declaration.
    ///
    /// The nominal arm reads `type_classes`, which the item passes populate
    /// before any function body is lowered, so a `Resource` / `Linear` marker
    /// is always available here.
    ///
    /// Actor references (`ActorHandle`, `BoxedActor`, `ActorFn`, `MonitorRef`)
    /// carry the `Resource` MARKER for drop elaboration but are shareable
    /// addresses, so they are excluded on both sides — sending a pid must not
    /// consume the sender's own handle.
    pub(super) fn resolved_ty_transfers_ownership_to_mailbox(&self, ty: &ResolvedTy) -> bool {
        resolved_ty_transfers_ownership_to_mailbox(ty, &self.type_classes, &self.type_member_tys)
    }

    /// The [`IntentKind`] an actor message argument at `span` must carry.
    ///
    /// `Consume` when the mailbox hand-off takes sole ownership (see
    /// [`Self::resolved_ty_transfers_ownership_to_mailbox`]), `Read` otherwise.
    /// MIR lowers EVERY message argument through `lower_value_for_move`, so
    /// this intent is what lets the MIR dataflow checker see the consume and
    /// refuse a second transfer — the ask/tell/select-arm paths all route
    /// through here so no argument position is left unowned.
    pub(super) fn actor_message_arg_intent(&self, span: &Span) -> IntentKind {
        let transfers = self
            .expr_types
            .get(&self.mk_key(span))
            .and_then(|ty| ResolvedTy::from_ty(ty).ok())
            .is_some_and(|resolved| self.resolved_ty_transfers_ownership_to_mailbox(&resolved));
        if transfers {
            IntentKind::Consume
        } else {
            IntentKind::Read
        }
    }

    pub(super) fn lower_stdlib_callee(&mut self, entry: &BuiltinEntry, span: Span) -> HirExpr {
        let (id, param_tys, return_ty) = {
            let registry_entry = self
                .fn_registry
                .get(entry.name)
                .expect("catalog entries are seeded before expression lowering");
            (
                registry_entry.id,
                registry_entry.param_tys.clone(),
                registry_entry.return_ty.clone(),
            )
        };
        let fn_ty = ResolvedTy::Function {
            capabilities: hew_types::CallableCapabilities::FUNCTION_ITEM,
            params: param_tys,
            ret: Box::new(return_ty),
        };
        HirExpr {
            node: self.ids.node(),
            site: self.ids.site(),
            ty: fn_ty,
            intent: IntentKind::Read,
            kind: HirExprKind::BindingRef {
                name: entry.name.to_string(),
                resolved: ResolvedRef::Item(id),
            },
            span,
        }
    }

    pub(super) fn unresolved_builtin_callee(&mut self, name: &str, span: Span) -> HirExpr {
        HirExpr {
            node: self.ids.node(),
            site: self.ids.site(),
            ty: ResolvedTy::Unit,
            intent: IntentKind::Read,
            kind: HirExprKind::BindingRef {
                name: name.to_string(),
                resolved: ResolvedRef::Unresolved,
            },
            span,
        }
    }

    pub(super) fn unresolved_module_object(&mut self, name: &str, span: Span) -> HirExpr {
        HirExpr {
            node: self.ids.node(),
            site: self.ids.site(),
            ty: ResolvedTy::Unit,
            intent: IntentKind::Read,
            kind: HirExprKind::BindingRef {
                name: name.to_string(),
                resolved: ResolvedRef::Unresolved,
            },
            span,
        }
    }

    /// Build a HIR `String` literal expression for the given content.  Used by
    /// the f-string interpolation lowering to materialise the static segments
    /// between `{ … }` placeholders.
    pub(super) fn build_string_literal_expr(&mut self, value: String, span: Span) -> HirExpr {
        HirExpr {
            node: self.ids.node(),
            site: self.ids.site(),
            ty: ResolvedTy::String,
            intent: IntentKind::Read,
            kind: HirExprKind::Literal(HirLiteral::String(value)),
            span,
        }
    }

    /// Emit a call to a stdlib catalog entry resolved by name.  Returns a
    /// fully-typed HIR `Call` expression with the catalog entry's return
    /// type. Used by f-string lowering to invoke `string_concat` and the
    /// primitive `to_string_*` overloads that back the built-in `Display`
    /// impls in stdlib.
    pub(super) fn build_catalog_call(
        &mut self,
        builtin_name: &str,
        args: Vec<HirExpr>,
        span: Span,
    ) -> HirExpr {
        let entry = crate::stdlib_catalog::CATALOG
            .iter()
            .find(|e| e.name == builtin_name)
            .expect("catalog entry exists for f-string lowering");
        let callee = self.lower_stdlib_callee(entry, span.clone());
        let return_ty = self
            .fn_registry
            .get(builtin_name)
            .map_or(ResolvedTy::String, |e| e.return_ty.clone());
        HirExpr {
            node: self.ids.node(),
            site: self.ids.site(),
            ty: return_ty,
            intent: IntentKind::Read,
            kind: HirExprKind::Call {
                target: self.registered_symbol_target(builtin_name),
                callee: Box::new(callee),
                args,
                evaluation_order: Vec::new(),
            },
            span,
        }
    }

    /// Emit a call to a user-defined function or impl-method registered in
    /// `fn_registry` under the given symbol.  Used by f-string interpolation
    /// to invoke `<Type>::fmt` for user `impl Display for Type` blocks.
    pub(super) fn build_user_fn_call(
        &mut self,
        fn_name: &str,
        args: Vec<HirExpr>,
        span: Span,
    ) -> Option<HirExpr> {
        let (id, param_tys, return_ty) = {
            let entry = self.fn_registry.get(fn_name)?;
            (entry.id, entry.param_tys.clone(), entry.return_ty.clone())
        };
        let fn_ty = ResolvedTy::Function {
            capabilities: hew_types::CallableCapabilities::FUNCTION_ITEM,
            params: param_tys,
            ret: Box::new(return_ty.clone()),
        };
        let callee = HirExpr {
            node: self.ids.node(),
            site: self.ids.site(),
            ty: fn_ty,
            intent: IntentKind::Read,
            kind: HirExprKind::BindingRef {
                name: fn_name.to_string(),
                resolved: ResolvedRef::Item(id),
            },
            span: span.clone(),
        };
        Some(HirExpr {
            node: self.ids.node(),
            site: self.ids.site(),
            ty: return_ty,
            intent: IntentKind::Read,
            kind: HirExprKind::Call {
                target: self.registered_symbol_target(fn_name),
                callee: Box::new(callee),
                args,
                evaluation_order: Vec::new(),
            },
            span,
        })
    }

    /// Wrap `operand` (already-lowered, `bool`-typed) in a boolean `!`.
    pub(super) fn build_bool_not(&mut self, operand: HirExpr, span: Span) -> HirExpr {
        HirExpr {
            node: self.ids.node(),
            site: self.ids.site(),
            ty: ResolvedTy::Bool,
            intent: IntentKind::Read,
            kind: HirExprKind::Unary {
                op: UnaryOp::Not,
                operand: Box::new(operand),
                operand_ty: ResolvedTy::Bool,
            },
            span,
        }
    }

    /// Lower a module-qualified direct call (`module.fn(args)` /
    /// `module::fn(args)`) given already-lowered arguments.
    ///
    /// Both the dot-form method-call surface and the namespaced `Call` surface
    /// record the same `RewriteModuleQualifiedToFunction` on the call span and
    /// route here. The `c_symbol` from the checker uses dotted notation for
    /// user-module calls (`module.fn`) and `hew_*` for stdlib calls. User-module
    /// keys are stored in `fn_registry` under the mangled form (`module$fn`) so
    /// they are safe as native object-file symbols on all targets. Apply
    /// `mangle_dotted_name` before the registry lookup AND in the emitted
    /// `BindingRef.name` so all three consumers (HIR verifier, MIR
    /// `module_fn_names`, codegen `add_function`) see the same mangled key.
    /// Stdlib `hew_*` symbols contain no dots, so mangling is identity.
    ///
    /// A direct call to an imported GENERIC free fn needs the per-instantiation
    /// monomorphisation registered here — the same authority the bare-identifier
    /// callee feeds through `record_monomorphisation` — or MIR's `Call` arm
    /// finds neither a `call_site_type_args` entry nor the mangled name in
    /// `module_fn_names`, and the callee falls through to the function-call NYI.
    /// The helper seeds `call_site_type_args` with the checker-recorded args and
    /// inserts the `MonoKey` whose `origin_name == symbol`. No-op for
    /// non-generic callees, so safe to call unconditionally.
    pub(super) fn lower_module_qualified_direct_call_lowered(
        &mut self,
        target: CallTarget,
        c_symbol: &str,
        lowered_args: LoweredCallArgs,
        span: &Span,
        site: SiteId,
    ) -> (HirExprKind, ResolvedTy) {
        if let CallTarget::RecordConstructor(declaration) = &target {
            return self.lower_positional_record_constructor(declaration, lowered_args.args, span);
        }

        if !matches!(
            target,
            CallTarget::User(_)
                | CallTarget::ImplMethod(_)
                | CallTarget::Runtime(_)
                | CallTarget::Builtin { .. }
        ) {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::CheckerBoundaryViolation {
                    name: c_symbol.to_string(),
                    reason: "module-qualified call target is not direct".to_string(),
                },
                span.clone(),
                "checker must publish a direct declaration or runtime target for module calls",
            ));
            return (
                HirExprKind::Unsupported(
                    "module-qualified call has unsupported target".to_string(),
                ),
                ResolvedTy::Unit,
            );
        }
        let symbol = if let CallTarget::ImplMethod(declaration) = &target {
            let Some(symbol) = self.registered_impl_method_symbol(declaration) else {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::CallableUnsupportedInMir {
                        name: declaration.full_path().to_string(),
                    },
                    span.clone(),
                    "checker selected an associated implementation declaration whose HIR body was not registered",
                ));
                return (
                    HirExprKind::Unsupported(
                        "associated implementation call has no registered HIR body".to_string(),
                    ),
                    ResolvedTy::Unit,
                );
            };
            symbol
        } else {
            crate::mangle_dotted_name(c_symbol)
        };
        let selected_declaration = match &target {
            CallTarget::User(declaration) | CallTarget::ImplMethod(declaration) => {
                Some(declaration)
            }
            CallTarget::Runtime(_) | CallTarget::Builtin { .. } => None,
            _ => unreachable!("direct-call target shape was validated above"),
        };
        self.register_free_fn_monomorphisation(&symbol, selected_declaration, span, site);
        let key = self.mk_key(span);
        let ret_ty = self
            .expr_types
            .get(&key)
            .cloned()
            .and_then(|ty| ResolvedTy::from_ty(&ty).ok())
            .map_or(ResolvedTy::Unit, |ty| {
                self.qualify_current_module_record_ty(ty)
            });
        self.assert_resolved_ty_totality(span);
        let resolved_ref = match &target {
            // The checker has already selected this closed executable family.
            // Do not require a synthetic fn-registry spelling (or recover one
            // from `symbol`) merely to mark the binding as directly callable.
            CallTarget::Runtime(family) => ResolvedRef::Builtin(*family),
            _ => self
                .fn_registry
                .get(&symbol)
                .map_or(ResolvedRef::Unresolved, |entry| {
                    entry
                        .builtin_family
                        .map_or(ResolvedRef::Item(entry.id), ResolvedRef::Builtin)
                }),
        };
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
                name: symbol,
                resolved: resolved_ref,
            },
            span: span.clone(),
        };
        (
            HirExprKind::Call {
                target,
                callee: Box::new(callee),
                args: lowered_args.args,
                evaluation_order: lowered_args.evaluation_order,
            },
            ret_ty,
        )
    }

    pub(super) fn lower_positional_record_constructor(
        &mut self,
        declaration: &hew_types::DefId,
        args: Vec<HirExpr>,
        span: &Span,
    ) -> (HirExprKind, ResolvedTy) {
        let ty = self
            .checker_expr_ty(span, declaration.full_path())
            .unwrap_or(ResolvedTy::Unit);
        let type_args = match &ty {
            ResolvedTy::Named { args, .. } => args.clone(),
            _ => vec![],
        };
        (
            HirExprKind::StructInit {
                name: declaration.full_path().to_string(),
                type_args,
                fields: args
                    .into_iter()
                    .enumerate()
                    .map(|(index, arg)| (index.to_string(), arg))
                    .collect(),
                base: None,
            },
            ty,
        )
    }

    #[expect(
        clippy::too_many_lines,
        reason = "regular call lowering reconciles checker targets and module identities atomically"
    )]
    pub(super) fn lower_regular_call(
        &mut self,
        function: &Spanned<Expr>,
        args: LoweredCallArgs,
        span: &Span,
        site: SiteId,
    ) -> (HirExprKind, ResolvedTy) {
        if let Some(CallTarget::RecordConstructor(declaration)) = self.ordinary_call_target(span) {
            return self.lower_positional_record_constructor(&declaration, args.args, span);
        }

        // Module-qualified call `module.fn(args)`: the callee is a
        // `FieldAccess` on a module identifier, not a value. The checker
        // recorded a `RewriteModuleQualifiedToFunction` on this call span (the
        // same rewrite the dot form `module.fn(args)` records via the
        // method-call path). Route through the identical direct-call lowering so
        // the callee resolves to the qualified registry symbol and the
        // per-instantiation monomorphisation is registered — without it the
        // `FieldAccess` callee lowers to an unresolved binding.
        let rewrite_key = self.mk_key(span);
        if let Some(MethodCallRewrite::RewriteModuleQualifiedToFunction {
            target, c_symbol, ..
        }) = self.method_call_rewrites.get(&rewrite_key).cloned()
        {
            return self
                .lower_module_qualified_direct_call_lowered(target, &c_symbol, args, span, site);
        }
        // A direct target on `module.fn(args)` is insufficient on its own:
        // the checker must also publish the module-qualified rewrite carrying
        // the exact callee symbol. Without that fact, lowering the field access
        // as an ordinary value call would leave an admitted HIR `Call` whose
        // legacy MIR consumer can re-infer a target from strings.
        if matches!(
            &function.0,
            Expr::FieldAccess { object, .. } if matches!(object.0, Expr::Ident(_))
        ) && matches!(
            self.ordinary_call_target(span),
            Some(CallTarget::User(_) | CallTarget::Runtime(_))
        ) {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::CheckerBoundaryViolation {
                    name: "module-qualified call".to_string(),
                    reason: "missing module-qualified call rewrite".to_string(),
                },
                span.clone(),
                "checker admitted a module-qualified call without its executable target fact",
            ));
            return (
                HirExprKind::Unsupported(
                    "module-qualified call has no checker rewrite".to_string(),
                ),
                ResolvedTy::Unit,
            );
        }
        if self.ordinary_call_target(span).is_none()
            && self.diagnose_poisoned_direct_call_type_args(&function.0, span)
        {
            return (
                HirExprKind::Unsupported(
                    "ordinary call has poisoned monomorphisation arguments".to_string(),
                ),
                ResolvedTy::Unit,
            );
        }
        let Some(target) = self.ordinary_call_target(span) else {
            // Preserve source-resolution diagnostics when the checker rejected
            // the callee before it could publish an executable target.  This is
            // deliberately presentation-only: lowering the callee lets an
            // unresolved identifier (including a non-callable layout descriptor)
            // report `UnresolvedSymbol`, but its spelling is never retried as a
            // direct-call target.
            let _ = self.lower_expr(function, IntentKind::Read);
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::CheckerBoundaryViolation {
                    // This is presentation context only.  In particular, do
                    // not turn it into a declaration lookup: a missing
                    // checker target remains a hard HIR boundary failure.
                    name: Self::ordinary_call_presentation_name(function),
                    reason: "missing direct_call_targets entry".to_string(),
                },
                span.clone(),
                "checker admitted an ordinary call without its canonical target",
            ));
            return (
                HirExprKind::Unsupported("ordinary call has no checker target".to_string()),
                ResolvedTy::Unit,
            );
        };
        let target_name = Self::call_target_presentation_name(&target);
        if !self.ensure_executable_target(&target, &target_name, span) {
            return (
                HirExprKind::Unsupported(
                    "ordinary call has unsupported checker target".to_string(),
                ),
                ResolvedTy::Unit,
            );
        }
        // Named imports may bind a canonical runtime declaration under an
        // arbitrary alias. Its checked target is sufficient; resolving the
        // alias as a source body would invent an unnecessary callable stub.
        if let CallTarget::Runtime(family) = target {
            return self.lower_module_qualified_direct_call_lowered(
                target,
                family.c_symbol(),
                args,
                span,
                site,
            );
        }
        let callee = self.lower_expr(function, IntentKind::Read);
        // Record the per-instantiation monomorphisation if the callee is a
        // generic top-level user fn. Direct-name callees only;
        // `record_monomorphisation` filters out non-generic callees, non-
        // `fn_registry` callees (builtins, runtime symbols, local bindings),
        // and callsites the checker did not record. Fail-closed on poisoned
        // entries and on registry-cap exhaustion.
        self.record_monomorphisation(&function.0, span, site);
        // Checker authority takes precedence: consult expr_types at the full
        // call-expression span. The checker records the call result type here
        // for checker-registered builtins that have no AST `fn` item and
        // therefore no `fn_registry` hit. (LESSONS: checker-authority P0)
        let checker_key = self.mk_key(span);
        let result_ty = if let Some(ty) = self.expr_types.get(&checker_key).cloned() {
            match ResolvedTy::from_ty(&ty) {
                Ok(resolved) => self.qualify_current_module_record_ty(resolved),
                Err(err) => {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::CheckerBoundaryViolation {
                            // The target's checker-owned declaration identity
                            // is the diagnostic authority once one exists;
                            // do not reconstruct it from the lowered callee.
                            name: target_name,
                            reason: err.to_string(),
                        },
                        span.clone(),
                        "checker-authoritative call result type failed boundary conversion",
                    ));
                    ResolvedTy::Unit
                }
            }
        } else if let ResolvedTy::Function { ret, .. } = &callee.ty {
            *ret.clone()
        } else {
            if matches!(
                callee.kind,
                HirExprKind::BindingRef {
                    resolved: ResolvedRef::Unresolved,
                    ..
                }
            ) {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::UnresolvedInferenceVar,
                    span.clone(),
                    "call result type cannot be determined: callee is unresolved",
                ));
            }
            ResolvedTy::Unit
        };
        (
            HirExprKind::Call {
                target,
                callee: Box::new(callee),
                args: args.args,
                evaluation_order: args.evaluation_order,
            },
            result_ty,
        )
    }

    /// Return the checker-authored declaration identities for a trait method.
    /// An absent entry is intentionally propagated as `None`: static-dispatch
    /// indexing must fail closed instead of constructing an id from HIR text.
    pub(super) fn trait_method_identity(
        &self,
        declaring_trait: &str,
        method_name: &str,
    ) -> Option<(hew_types::DefId, hew_types::DefId)> {
        // An imported trait alias is a lexical binding, not an owner prefix.
        // Consult the checker-published binding fact first; a bare trait bound
        // inside the active module then has one exact canonical-owner fallback.
        // Never reinterpret a final path segment as this module's owner.
        if let Some(ids) = self.trait_method_ids_by_binding.get(&(
            self.current_module_name.clone(),
            self.current_module_idx,
            declaring_trait.to_string(),
            method_name.to_string(),
        )) {
            return Some(ids.clone());
        }
        if let Some(module) = self.current_module_name.as_deref() {
            let key = format!("{module}.{declaring_trait}::{method_name}");
            if let Some(ids) = self.trait_method_ids.get(&key) {
                return Some(ids.clone());
            }
        }
        self.trait_method_ids
            .get(&format!("{declaring_trait}::{method_name}"))
            .cloned()
            .or_else(|| {
                // The prelude iterator may be available without a lexical
                // import binding.  Its lang-item binding carries the exact
                // checker-minted declaration identities; compare only its
                // checker-published surface names, never a hard-coded leaf or
                // a scan over declaration-ID strings. Local/import bindings
                // above remain higher priority.
                let binding = self
                    .lang_items
                    .get(hew_types::LangItem::IteratorNext.key())?;
                if declaring_trait == binding.trait_name
                    && binding.method_name.as_deref() == Some(method_name)
                {
                    Some((binding.trait_id.clone(), binding.method_id.clone()?))
                } else {
                    None
                }
            })
            .or_else(|| {
                // Display has the same prelude shape.  The method-level
                // lang-item is the single exact authority, including if the
                // stdlib later renames the trait or method.
                let binding = self.lang_items.get(hew_types::LANG_ITEM_DISPLAY_FMT)?;
                if declaring_trait == binding.trait_name
                    && binding.method_name.as_deref() == Some(method_name)
                {
                    Some((binding.trait_id.clone(), binding.method_id.clone()?))
                } else {
                    None
                }
            })
    }

    pub(super) fn register_trait_default_fn_entry(
        &mut self,
        self_name: &str,
        default: &hew_types::ResolvedTraitDefault,
        impl_type_params: &[String],
    ) {
        let previous_module =
            std::mem::replace(&mut self.current_module_name, default.source_module.clone());
        let previous_file = std::mem::replace(&mut self.current_module_idx, default.file_index);
        let function = trait_method_to_fn_decl(&default.method);
        self.register_impl_method_fn_entry(self_name, &function, impl_type_params);
        self.current_module_name = previous_module;
        self.current_module_idx = previous_file;
    }

    pub(super) fn trait_declaration(&self, binding: &str) -> Option<hew_types::DefId> {
        self.trait_bindings
            .get(&(
                self.current_module_name.clone(),
                self.current_module_idx,
                binding.to_string(),
            ))
            .cloned()
    }

    pub(super) fn ordinary_call_target(&self, span: &Span) -> Option<CallTarget> {
        self.direct_call_targets.get(&self.mk_key(span)).cloned()
    }

    /// Return the checker-owned declaration selected for a direct source call
    /// that can own a generic body monomorphisation. Runtime, FFI, and
    /// indirect targets deliberately have no source-body declaration.
    pub(super) fn direct_monomorph_declaration(&self, span: &Span) -> Option<hew_types::DefId> {
        match self.ordinary_call_target(span)? {
            CallTarget::User(declaration) | CallTarget::ImplMethod(declaration) => {
                Some(declaration)
            }
            _ => None,
        }
    }

    /// A call-site spelling is useful only when the checker did not publish a
    /// target at all.  It is deliberately diagnostic-only: dispatch continues
    /// to require a [`CallTarget`] fact and never retries this spelling.
    pub(super) fn ordinary_call_presentation_name(function: &Spanned<Expr>) -> String {
        match &function.0 {
            Expr::Ident(name) => name.to_string(),
            Expr::FieldAccess { object, field } => match &object.0 {
                Expr::Ident(owner) => format!("{owner}.{field}", field = field.0),
                _ => "<call expression>".to_string(),
            },
            _ => "<call expression>".to_string(),
        }
    }

    /// Render the checker-selected target without using the spelling carried
    /// by a lowered expression.  Declaration-bearing targets retain their
    /// full `DefId` path so diagnostics identify the same declaration that
    /// structured dispatch will carry to MIR.
    pub(super) fn call_target_presentation_name(target: &CallTarget) -> String {
        match target {
            CallTarget::User(declaration)
            | CallTarget::RecordConstructor(declaration)
            | CallTarget::ImplMethod(declaration)
            | CallTarget::Extern { declaration, .. }
            | CallTarget::DeclaredRuntime { declaration, .. } => {
                declaration.full_path().to_string()
            }
            CallTarget::Runtime(family) => format!("runtime::{family:?}"),
            CallTarget::Builtin { endpoint } => endpoint.clone(),
            CallTarget::RuntimeCollection(family) => format!("runtime collection::{family:?}"),
            CallTarget::DynamicVtable { method, .. }
            | CallTarget::StaticTraitMethod { method, .. } => method.full_path().to_string(),
            CallTarget::IndirectFunctionValue => "indirect function value".to_string(),
            CallTarget::Unsupported { reason } => format!("unsupported call ({reason})"),
        }
    }

    /// Reject a checker sentinel before it becomes executable HIR. Every typed
    /// call-family constructor routes through this gate.
    pub(super) fn ensure_executable_target(
        &mut self,
        target: &CallTarget,
        name: &str,
        span: &Span,
    ) -> bool {
        if !matches!(target, CallTarget::Unsupported { .. }) {
            return true;
        }
        self.diagnostics.push(HirDiagnostic::new(
            HirDiagnosticKind::CheckerBoundaryViolation {
                name: name.to_string(),
                reason: "checker target is unsupported".to_string(),
            },
            span.clone(),
            "checker admitted a call without an executable target",
        ));
        false
    }

    /// Return the checker-owned target for a compiler-synthesised call to an
    /// already registered symbol.  Synthetic lowering is not allowed to turn
    /// that presentation string back into a declaration ID: either the
    /// registry supplies a typed runtime/impl target or the call is explicit
    /// unsupported and stops at the HIR boundary.
    pub(super) fn registered_symbol_target(&self, symbol: &str) -> CallTarget {
        if let Some(entry) = self.fn_registry.get(symbol) {
            if let Some(family) = entry.builtin_family {
                return CallTarget::Runtime(family);
            }
            if entry.linkage.is_some() {
                return CallTarget::Builtin {
                    endpoint: symbol.to_string(),
                };
            }
        }
        if let Some(declaration) = self.impl_method_declaration_ids.get(symbol) {
            return CallTarget::ImplMethod(declaration.clone());
        }
        CallTarget::Unsupported {
            reason: format!("synthetic call `{symbol}` has no checker-owned target"),
        }
    }

    /// Lower a call expression. An early-complete call returns its finished
    /// expression; every other shape returns its kind and type for the
    /// caller to finish. Kept out of `lower_expr_inner` so its locals do not
    /// enlarge that recursive frame.
    #[inline(never)]
    #[allow(
        clippy::too_many_lines,
        reason = "call lowering selects one of many checked call shapes"
    )]
    pub(super) fn lower_call_expr(
        &mut self,
        function: &Spanned<Expr>,
        args: &[CallArg],
        span: Span,
        site: SiteId,
        intent: IntentKind,
    ) -> Result<(HirExprKind, ResolvedTy), Box<HirExpr>> {
        let lowered = {
            let rewrite_key = self.mk_key(&span);
            // `handle(msg)` on a lambda actor is a completion call, not a
            // callable-value invocation: the checker records it as an ask.
            if let Some(ActorMethodKind::Ask {
                method_id,
                reply_ty,
                policy,
            }) = self
                .actor_method_dispatch
                .get(&rewrite_key)
                .filter(|dispatch| {
                    matches!(dispatch, ActorMethodKind::Ask { method_id, .. }
                            if method_id == hew_types::actor_protocol::LAMBDA_ACTOR_METHOD_ID)
                })
                .cloned()
            {
                let (kind, ty) = self
                    .lower_lambda_actor_call(function, args, &method_id, &reply_ty, policy, &span);
                return Err(Box::new(HirExpr {
                    node: self.ids.node(),
                    site,
                    ty,
                    intent,
                    kind,
                    span,
                }));
            }
            // `mailbox(handle, ..)(msg)` submits one way: the checker
            // records the same lambda dispatch as a `Message`.
            if let Some(ActorMethodKind::Message { method_id, policy }) = self
                .actor_method_dispatch
                .get(&rewrite_key)
                .filter(|dispatch| {
                    matches!(dispatch, ActorMethodKind::Message { method_id, .. }
                            if method_id == hew_types::actor_protocol::LAMBDA_ACTOR_METHOD_ID)
                })
                .cloned()
            {
                let (kind, ty) =
                    self.lower_lambda_actor_submission(function, args, &method_id, policy, &span);
                return Err(Box::new(HirExpr {
                    node: self.ids.node(),
                    site,
                    ty,
                    intent,
                    kind,
                    span,
                }));
            }
            if let Some(MethodCallRewrite::GenericWireCodec {
                direction,
                value_ty,
            }) = self.method_call_rewrites.get(&rewrite_key).cloned()
            {
                let (kind, ty) =
                    self.lower_generic_wire_codec(args, direction, value_ty, span.clone());
                return Err(Box::new(HirExpr {
                    node: self.ids.node(),
                    site,
                    ty,
                    intent,
                    kind,
                    span,
                }));
            }
            if let Some(MethodCallRewrite::RcIntrinsic {
                op: RcIntrinsicOp::New,
                payload_ty,
            }) = self.method_call_rewrites.get(&rewrite_key).cloned()
            {
                let result_ty = self
                    .resolved_expr_types
                    .get(&rewrite_key)
                    .cloned()
                    .unwrap_or_else(|| ResolvedTy::Named {
                        name: "Rc".to_string(),
                        args: vec![payload_ty.clone()],
                        builtin: Some(BuiltinType::Rc),
                        is_opaque: false,
                    });
                let value = args
                    .first()
                    .map(|arg| Box::new(self.lower_expr(arg.expr(), IntentKind::Consume)));
                return Err(Box::new(HirExpr {
                    node: self.ids.node(),
                    site,
                    ty: result_ty.clone(),
                    intent,
                    kind: HirExprKind::RcIntrinsic {
                        op: RcIntrinsicOp::New,
                        payload_ty,
                        receiver: None,
                        value,
                        result_ty,
                    },
                    span,
                }));
            }
            let direct_extern_symbol = match &function.0 {
                Expr::Ident(name) if self.extern_fn_names.contains(name.name.as_str()) => {
                    Some(name.name.as_str())
                }
                _ => None,
            };
            let LoweredCallArgs {
                mut args,
                evaluation_order,
            } = self.lower_call_args_for_callee(args, &span, direct_extern_symbol);
            if matches!(
                self.method_call_rewrites.get(&rewrite_key),
                Some(MethodCallRewrite::VecFrom)
            ) {
                if args.len() == 1 {
                    return Err(Box::new(self.subsumed_value(
                        site,
                        &span,
                        intent,
                        args.remove(0),
                    )));
                }
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::CheckerBoundaryViolation {
                        name: "Vec.from".to_string(),
                        reason: format!(
                            "checker selected Vec.from rewrite with {} argument(s)",
                            args.len()
                        ),
                    },
                    span.clone(),
                    "Vec.from lowering requires exactly one checked source value",
                ));
                return Err(Box::new(
                    self.unsupported_expr(span, "Vec.from has invalid arity"),
                ));
            }
            // Hew array literals already lower to the owned `Vec<T>`
            // construction sequence. `Vec::from([..])` is therefore an
            // identity at HIR: preserve that one canonical construction
            // path rather than fabricating a second Vec-from-array ABI.
            // The checker accepts only the array/Vec source forms, so any
            // other source form is a clean checker diagnostic before this
            // lowering boundary.
            if let Expr::ContextVariant(context) = &function.0 {
                let checker_ctor_ty = self.checker_expr_ty_if_present(&span);
                let contextual_name = match &checker_ctor_ty {
                    Some(ResolvedTy::Named { name, .. }) => {
                        format!("{name}::{}", context.name)
                    }
                    _ => context.name.to_string(),
                };
                let variant_kind_for_call = self
                    .lookup_variant_ctor(&contextual_name, checker_ctor_ty.as_ref())
                    .map(|(_, _, kind)| kind.clone());
                if let Some(HirVariantKind::Tuple(_)) = &variant_kind_for_call {
                    let taken = std::mem::take(&mut args);
                    self.lower_variant_ctor_tuple_call(&contextual_name, taken, &span)
                } else {
                    if let Some(kind) = &variant_kind_for_call {
                        self.report_variant_ctor_call_shape_mismatch(&contextual_name, kind, &span);
                    } else {
                        self.diagnostics.push(HirDiagnostic::new(
                                HirDiagnosticKind::CheckerBoundaryViolation {
                                    name: contextual_name.clone(),
                                    reason: "missing contextual variant constructor".to_string(),
                                },
                                span.clone(),
                                "checker admitted a contextual variant call without an exact constructor",
                            ));
                    }
                    (
                            HirExprKind::Unsupported(format!(
                                "contextual variant call `{contextual_name}` is not a tuple constructor"
                            )),
                            ResolvedTy::Unit,
                        )
                }
            } else if let Expr::Ident(name) = &function.0 {
                // Intercept payload-bearing variant constructors written
                // as calls (`Shape::Line(5)`, bare `Line(5)`). The bare
                // identifier path produces `MachineVariantCtor { payload:
                // None }`; the call form must capture the args into
                // `payload: Some(...)`. Mismatched ctor shape (calling a
                // unit variant with args, or calling a struct variant
                // positionally) emits a structured diagnostic and falls
                // through to the regular-call path so checker-stream
                // coverage is preserved.
                let checker_ctor_ty = self.checker_expr_ty_if_present(&span);
                let variant_kind_for_call = self
                    .lookup_variant_ctor(name.name.as_str(), checker_ctor_ty.as_ref())
                    .map(|(_, _, kind)| kind.clone());
                if let Some(HirVariantKind::Tuple(_)) = &variant_kind_for_call {
                    let taken = std::mem::take(&mut args);
                    self.lower_variant_ctor_tuple_call(name.name.as_str(), taken, &span)
                } else if let Some(kind) = &variant_kind_for_call {
                    self.report_variant_ctor_call_shape_mismatch(name.name.as_str(), kind, &span);
                    // Fall through to regular-call to keep checker-stream
                    // coverage for the malformed source.
                    self.lower_regular_call(
                        function,
                        LoweredCallArgs {
                            args,
                            evaluation_order,
                        },
                        &span,
                        site,
                    )
                } else if matches!(name.name.as_str(), "assert_eq" | "assert_ne") {
                    self.lower_equality_assertion(name.name.as_str(), args, &span)
                } else if stdlib_catalog::is_overloaded_builtin(name.name.as_str()) {
                    let arg_tys = args.iter().map(|arg| arg.ty.clone()).collect::<Vec<_>>();
                    if let Some(entry) =
                        stdlib_catalog::resolve_overload(name.name.as_str(), &arg_tys)
                    {
                        let result_ty = entry.return_ty.to_resolved();
                        let callee = self.lower_stdlib_callee(entry, function.1.clone());
                        (
                            HirExprKind::Call {
                                target: self.registered_symbol_target(entry.name),
                                callee: Box::new(callee),
                                args,
                                evaluation_order: Vec::new(),
                            },
                            result_ty,
                        )
                    } else {
                        match self.try_lower_generic_display_builtin(
                            name.name.as_str(),
                            args,
                            &span,
                        ) {
                            Ok(lowered) => lowered,
                            Err(args) => {
                                let arg_ty = arg_tys.first().cloned().unwrap_or(ResolvedTy::Unit);
                                self.diagnostics.push(HirDiagnostic::new(
                                        HirDiagnosticKind::UnresolvedBuiltinOverload {
                                            name: name.to_string(),
                                            arg_ty,
                                        },
                                        span.clone(),
                                        "builtin call has no registered monomorphic overload for this argument type",
                                    ));
                                let callee = self.unresolved_builtin_callee(
                                    name.name.as_str(),
                                    function.1.clone(),
                                );
                                (
                                        HirExprKind::Call {
                                            target: CallTarget::Unsupported {
                                                reason: format!(
                                                    "builtin overload `{name}` was rejected by the checker"
                                                ),
                                            },
                                            callee: Box::new(callee),
                                            args,
                                            evaluation_order: Vec::new(),
                                        },
                                        ResolvedTy::Unit,
                                    )
                            }
                        }
                    }
                } else {
                    self.lower_regular_call(
                        function,
                        LoweredCallArgs {
                            args,
                            evaluation_order,
                        },
                        &span,
                        site,
                    )
                }
            } else {
                self.lower_regular_call(
                    function,
                    LoweredCallArgs {
                        args,
                        evaluation_order,
                    },
                    &span,
                    site,
                )
            }
        };
        Ok(lowered)
    }
}

/// Call arguments in parameter order, with the order the source evaluates
/// them: the index into `args` of each argument as written, empty when the
/// two orders agree.
pub(super) struct LoweredCallArgs {
    pub(super) args: Vec<HirExpr>,
    pub(super) evaluation_order: Vec<usize>,
}

impl LoweredCallArgs {
    /// The evaluation order once a receiver evaluated first is prepended to
    /// `args`.
    pub(super) fn order_after_receiver(&self) -> Vec<usize> {
        if self.evaluation_order.is_empty() {
            return Vec::new();
        }
        std::iter::once(0)
            .chain(self.evaluation_order.iter().map(|slot| slot + 1))
            .collect()
    }
}
