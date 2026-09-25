//! Identifier, function-value and numeric-cast lowering.

use super::*;

impl LowerCtx {
    /// Preserve the selected record field as the indirect-call callee.
    /// SIR decides the projection's borrow or consumption from its capabilities.
    pub(super) fn lower_record_fn_field_call(
        &mut self,
        receiver: &Spanned<Expr>,
        method: &str,
        args: &[hew_parser::ast::CallArg],
        field_ty: &ResolvedTy,
        span: Span,
    ) -> (HirExprKind, ResolvedTy) {
        let ret_ty = match field_ty {
            ResolvedTy::Function { ret, .. } | ResolvedTy::Closure { ret, .. } => (**ret).clone(),
            other => {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::CheckerBoundaryViolation {
                        name: format!("fn-field call `.{method}`"),
                        reason: format!("recorded field type `{other}` is not a function type"),
                    },
                    span.clone(),
                    "RecordFnFieldCall rewrite requires a function-typed field",
                ));
                return (
                    HirExprKind::Unsupported(format!(
                        "fn-field call `.{method}` has non-function field type"
                    )),
                    ResolvedTy::Unit,
                );
            }
        };
        let field_access = if let Some(callee) = self.fork_field_callee(&span) {
            callee
        } else {
            let lowered_receiver = self.lower_expr(receiver, IntentKind::Read);
            self.make_expr(
                HirExprKind::FieldAccess {
                    object: Box::new(lowered_receiver),
                    field: method.to_string(),
                },
                field_ty.clone(),
                IntentKind::Read,
                span.clone(),
            )
        };
        let lowered_args = self.lower_positional_call_args(args, &span);
        (
            HirExprKind::Call {
                target: CallTarget::IndirectFunctionValue,
                callee: Box::new(field_access),
                args: lowered_args,
                evaluation_order: Vec::new(),
            },
            ret_ty,
        )
    }

    pub(super) fn unary_op_label(op: UnaryOp) -> &'static str {
        match op {
            UnaryOp::Not => "!",
            UnaryOp::Negate => "-",
            UnaryOp::BitNot => "~",
            UnaryOp::RawDeref => "*",
        }
    }

    pub(super) fn lower_function_value(
        &mut self,
        symbol: &str,
        span: &std::ops::Range<usize>,
        site: SiteId,
    ) -> (HirExprKind, ResolvedTy) {
        let entry = self.fn_registry[symbol].clone();
        self.register_free_fn_monomorphisation(symbol, None, span, site);
        let key = self.mk_key(span);
        if !entry.type_params.is_empty()
            && (self.expr_types.contains_key(&key) || self.call_type_args.contains_key(&key))
            && (!self.call_type_args.contains_key(&key)
                || self.direct_monomorph_declaration(span).is_none())
        {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::CheckerBoundaryViolation {
                    name: symbol.to_string(),
                    reason:
                        "generic function value requires type arguments and declaration identity"
                            .to_string(),
                },
                span.clone(),
                "generic function value has incomplete checker instantiation facts",
            ));
            return (
                HirExprKind::Unsupported("incomplete generic function value".to_string()),
                ResolvedTy::Unit,
            );
        }
        let ty = if self.expr_types.contains_key(&key) || self.call_type_args.contains_key(&key) {
            let Some(ty) = self.checker_expr_ty(span, "function value") else {
                return (
                    HirExprKind::Unsupported("function value has no checked type".to_string()),
                    ResolvedTy::Unit,
                );
            };
            ty
        } else {
            // Direct-call callee references may be synthesised from the
            // declaration without a separate value expression in the checker.
            ResolvedTy::Function {
                capabilities: hew_types::CallableCapabilities::FUNCTION_ITEM,
                params: entry.param_tys,
                ret: Box::new(entry.return_ty),
            }
        };
        let emitted = self
            .call_site_type_args
            .get(&site)
            .filter(|args| !args.iter().any(|ty| self.contains_abstract_type_param(ty)))
            .map_or_else(
                || {
                    self.fn_symbol_overrides
                        .get(&entry.id)
                        .cloned()
                        .unwrap_or_else(|| symbol.to_string())
                },
                |args| crate::monomorph::function_monomorph_symbol(symbol, args),
            );
        (
            HirExprKind::BindingRef {
                name: emitted,
                resolved: entry
                    .builtin_family
                    .map_or(ResolvedRef::Item(entry.id), ResolvedRef::Builtin),
            },
            ty,
        )
    }

    #[allow(
        clippy::too_many_lines,
        reason = "multi-branch identifier resolution: context readers, bindings, fn_sigs, \
                  unit-variant ctors with generic-arg preservation, and fn-item references \
                  are each a short arm; extracting a helper gains nothing without a richer \
                  identifier-resolution abstraction"
    )]
    pub(super) fn lower_identifier(
        &mut self,
        name: &str,
        span: std::ops::Range<usize>,
        site: SiteId,
    ) -> (HirExprKind, ResolvedTy) {
        if let Some(reader) = ExecutionContextReader::from_surface_name(name) {
            let key = self.mk_key(&span);
            if self
                .expr_types
                .get(&key)
                .is_some_and(|ty| *ty == reader.ty())
            {
                let ty = ResolvedTy::from_ty(&reader.ty()).expect("reader type resolves");
                return (HirExprKind::ContextReader { reader }, ty);
            }
        }
        if let Some((id, ty)) = self.lookup(name) {
            return (
                HirExprKind::BindingRef {
                    name: name.to_string(),
                    resolved: ResolvedRef::Binding(id),
                },
                ty,
            );
        }
        // Module-scope tagged-union unit constructor: machine states
        // (`TrafficLight::Red`, bare `Red`), machine events
        // (`TrafficLightEvent::Tick`, bare `Tick`), and user-defined enum
        // unit variants (`Colour::Red`, bare `Red`). The pre-pass over
        // `program.items` populated `machine_ctor_registry` with both
        // qualified and unambiguous-bare entries (see registry doc).
        // Consulted after lexical lookup so a user binding shadows a
        // same-named ctor (preserves the usual lexical-scope rule).
        //
        // Checker-authority cross-check: when the type checker has recorded
        // an `expr_types` entry for this span (which it does for any
        // accepted unit-ctor reference via `resolve_identifier_variant`),
        // the registered tagged-union name must match the type the checker
        // selected. This guarantees HIR follows checker authority across
        // bare-name collisions (e.g. an `enum Colour { Red }` plus a
        // `machine M { state Red }` where the checker's `type_defs` map
        // iteration order picked one of them). When the checker has no
        // entry — pure HIR-side resolution at a span the checker did not
        // type (rare; mostly synthesised code) — we accept the registry
        // hit unconditionally.
        // Checker-authoritative qualified-name fallback: when the bare-name
        // registry entry disagrees with the checker (e.g. a user `state Closed`
        // in scope alongside the synthetic `SendError::Closed` makes the bare
        // name ambiguous-or-missing, while the checker has already chosen
        // `TcpHandshake` for this identifier span), promote the checker's
        // chosen type into a qualified lookup. This keeps bare references to
        // unit ctors resolving correctly across builtin/user name collisions
        // without depending on registration-order races in the pre-pass.
        let key = self.mk_key(&span);
        let checker_owner = self
            .expr_types
            .get(&key)
            .and_then(|ty| ResolvedTy::from_ty(ty).ok())
            .map(|ty| self.qualify_current_module_record_ty(ty));
        let registry_hit = self
            .lookup_variant_ctor(name, checker_owner.as_ref())
            .map(|(type_name, variant_idx, _)| (type_name, variant_idx));
        if let Some((tagged_union_name, variant_idx)) = registry_hit {
            let variant_name = name.rsplit_once("::").map_or(name, |(_, variant)| variant);
            let checker_agrees = match checker_owner.as_ref() {
                None => !self.expr_types.contains_key(&key),
                Some(ResolvedTy::Named { head, .. }) => {
                    let name = head.registry_key();
                    name == tagged_union_name
                        || (!name.contains('.')
                            && self
                                .machine_ctor_registry
                                .get(&tagged_union_surface_ctor_key(name, variant_name))
                                .is_some_and(|(owner, _)| owner == &tagged_union_name))
                }
                Some(_) => false,
            };
            if checker_agrees {
                // Register the generic-enum instantiation before building
                // result_ty so codegen's mangled-key lookup finds the entry.
                self.try_register_enum_instantiation(&span);
                // Checker-authoritative result type: the checker records the
                // full `Named { name: "Option", args: [I64] }` at this
                // identifier span. Using it preserves type args so codegen
                // can compute the mangled registry key (e.g. `"Option$$i64"`).
                // Fall back to bare-name with a diagnostic if expr_types has
                // no entry — absence is a real signal; the checker should
                // always populate accepted unit-ctor reference sites.
                let result_ty = if let Some(ty) = self.expr_types.get(&key).cloned() {
                    match ResolvedTy::from_ty(&ty) {
                        Ok(resolved) => self.qualify_current_module_record_ty(resolved),
                        Err(err) => {
                            self.diagnostics.push(HirDiagnostic::new(
                                HirDiagnosticKind::CheckerBoundaryViolation {
                                    name: tagged_union_name.clone(),
                                    reason: err.to_string(),
                                },
                                span.clone(),
                                "checker-authoritative unit-variant result type failed boundary conversion",
                            ));
                            ResolvedTy::named_path(&self.defs, &tagged_union_name, Vec::new())
                        }
                    }
                } else {
                    // None means the checker had no entry at this span.
                    // Treat as bare-name rather than a hard diagnostic so
                    // synthesised/non-typed paths don't regress.
                    ResolvedTy::named_path(&self.defs, &tagged_union_name, Vec::new())
                };
                // W4.047 P1.2: prove the typed handoff agrees at this fail-open
                // bare-name unit-ctor site (the B1 archetype; no behaviour
                // change). When the checker stamped the span (W4.042), the
                // typed map carries the concrete `Named` type and this assert
                // confirms it; when the span is genuinely absent, both miss.
                self.assert_resolved_ty_totality(&span);
                return (
                    HirExprKind::MachineVariantCtor {
                        machine_name: tagged_union_name,
                        state_idx: variant_idx,
                        payload: None,
                    },
                    result_ty,
                );
            }
        }
        if let Some(symbol) = self.resolved_bare_function_symbol(name) {
            if self.fn_registry.contains_key(&symbol) {
                return self.lower_function_value(&symbol, &span, site);
            }
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::CheckerBoundaryViolation {
                    name: name.to_string(),
                    reason: format!(
                        "checker-selected callee `{symbol}` missing from HIR fn registry"
                    ),
                },
                span.clone(),
                "checker-selected free-function declaration was not registered",
            ));
        }
        // Bare-name same-module const reference inside an imported module's
        // function body (e.g. `STATUS_OK` inside `tls.hew`).  The global
        // `const_registry` only carries the qualified key
        // `"tls.STATUS_OK"`; `imported_module_consts` holds the bare-name
        // entries that are in scope while this module's bodies are lowered.
        // Checked before `const_registry` so same-module bare names take
        // precedence over any qualified alias that happens to collide.
        if let Some(entry) = self
            .imported_module_consts
            .as_ref()
            .and_then(|m| m.get(name))
            .cloned()
        {
            let ty = entry.ty.clone();
            let id = entry.id;
            return (
                HirExprKind::BindingRef {
                    name: name.to_string(),
                    resolved: ResolvedRef::Const(id),
                },
                ty,
            );
        }
        // A bare constant an import published into THIS file's scope. The
        // checker recorded the exact declaring owner per importing file, so a
        // file the root spliced in resolves its own `import lib.{ LIB_K };`
        // here and a file that never wrote that import does not see `LIB_K`.
        if let Some(entry) = self
            .published_bare_const_owners
            .get(&(
                self.current_module_name.clone(),
                self.current_module_idx,
                name.to_string(),
            ))
            .filter(|owners| owners.len() == 1)
            .and_then(|owners| owners.iter().next())
            .and_then(|owner| self.const_registry.get(self.published_const_key(owner)))
            .cloned()
        {
            let ty = entry.ty.clone();
            let id = entry.id;
            return (
                HirExprKind::BindingRef {
                    name: name.to_string(),
                    resolved: ResolvedRef::Const(id),
                },
                ty,
            );
        }
        if let Some(entry) = self.const_registry.get(name) {
            // Module-level `const` reference. Resolves to a `Const` ref carrying
            // the declaration's stable ItemId; MIR/codegen map it back to the
            // folded descriptor. Consulted after lexical lookup (a local binding
            // shadows a const) and after tagged-union ctor resolution, mirroring
            // the usual scope precedence.
            let ty = entry.ty.clone();
            let id = entry.id;
            return (
                HirExprKind::BindingRef {
                    name: name.to_string(),
                    resolved: ResolvedRef::Const(id),
                },
                ty,
            );
        }
        if self.fn_registry.contains_key(name) {
            self.lower_function_value(name, &span, site)
        } else {
            if let Some(module) = self.missing_stdlib_module_import(name) {
                let source_module = module.replace("::", ".");
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::ImportMissing {
                        module: source_module,
                        name: name.to_string(),
                    },
                    span,
                    stdlib_catalog::missing_import_hint(module).replace("::", "."),
                ));
            } else {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::UnresolvedSymbol {
                        name: name.to_string(),
                    },
                    span,
                    "identifier has no binding in resolved HIR",
                ));
            }
            (
                HirExprKind::BindingRef {
                    name: name.to_string(),
                    resolved: ResolvedRef::Unresolved,
                },
                ResolvedTy::Unit,
            )
        }
    }

    pub(super) fn lower_numeric_cast_expr(
        &mut self,
        value: &Spanned<Expr>,
        ty: &Spanned<TypeExpr>,
        span: &Span,
    ) -> (HirExprKind, ResolvedTy) {
        let value = self.lower_expr(value, IntentKind::Read);
        let from_ty = value.ty.clone();
        let to_ty = self.lower_type(ty);
        if !from_ty.can_explicitly_numeric_cast_to(&to_ty) {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::CheckerBoundaryViolation {
                    name: "numeric cast".to_string(),
                    reason: format!(
                        "cast from {} to {} is outside the checker-admitted numeric matrix",
                        from_ty.user_facing(),
                        to_ty.user_facing()
                    ),
                },
                span.clone(),
                "only numeric<->numeric, bool->integer, integer->bool, and char->integer `as` casts lower to HIR",
            ));
            return (
                HirExprKind::Unsupported(format!(
                    "unsupported cast from {} to {}",
                    from_ty.user_facing(),
                    to_ty.user_facing()
                )),
                ResolvedTy::Unit,
            );
        }
        (
            HirExprKind::NumericCast {
                value: Box::new(value),
                from_ty: from_ty.clone(),
                to_ty: to_ty.clone(),
            },
            to_ty,
        )
    }
}
