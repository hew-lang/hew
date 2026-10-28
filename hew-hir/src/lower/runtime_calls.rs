//! Runtime invocations, send status results and variant constructor calls.

use super::*;

impl LowerCtx<'_> {
    /// Consume complete checker-authored physical payload initialization.
    pub(super) fn lower_planned_variant_constructor(
        &mut self,
        expr: &Spanned<Expr>,
        intent: IntentKind,
        plan: hew_types::VariantConstructorPlan,
    ) -> HirExpr {
        let span = &expr.1;
        let source = match &expr.0 {
            Expr::StructInit { fields, .. } => fields.as_slice(),
            Expr::ContextVariant(context) => context
                .record
                .as_ref()
                .map_or(&[][..], |record| record.fields.as_slice()),
            _ => &[],
        };
        let Some(result_ty) = self.checker_expr_resolved_ty(span, "planned variant constructor")
        else {
            return self.unsupported_expr(span.clone(), "planned variant type missing");
        };
        let qualified = format!("{}::{}", plan.variant.type_name, plan.variant.variant_name);
        let Some((owner, index, _)) = self.lookup_variant_ctor(&qualified, Some(&result_ty)) else {
            return self.unsupported_expr(span.clone(), "planned variant declaration missing");
        };
        let owner = owner.clone();
        let mut payload = Vec::with_capacity(plan.fields.len());
        for field in plan.fields {
            let value = match field.value {
                hew_types::VariantConstructorValue::Source(index) => {
                    let Some((_, value)) = source.get(index) else {
                        return self.unsupported_expr(
                            span.clone(),
                            "planned variant source field missing",
                        );
                    };
                    self.lower_expr(value, IntentKind::Read)
                }
                hew_types::VariantConstructorValue::EmptyOption => {
                    let Ok(ty) = ResolvedTy::from_ty(&field.ty) else {
                        return self.unsupported_expr(
                            span.clone(),
                            "planned variant field type unresolved",
                        );
                    };
                    let Some((_, none_index)) =
                        self.builtin_variant_predicate(BuiltinType::Option, "None", span)
                    else {
                        return self
                            .unsupported_expr(span.clone(), "planned option identity missing");
                    };
                    self.synthetic_variant_ctor("Option", none_index, None, ty, span)
                }
            };
            payload.push((field.name, value));
        }
        HirExpr {
            node: self.ids.node(),
            site: self.source_site(span),
            ty: result_ty,
            intent,
            span: span.clone(),
            kind: HirExprKind::MachineVariantCtor {
                machine_name: owner,
                state_idx: index,
                payload: Some(payload),
            },
        }
    }

    /// Consume the checked source contract and concrete callback identities.
    /// The runtime status is discarded, so SIR receives one raw invocation and
    /// owns the same cleanup paths as any other call.
    pub(super) fn lower_declared_runtime_invocation(
        &mut self,
        target: CallTarget,
        receiver: &Spanned<Expr>,
        args: &[hew_parser::ast::CallArg],
        consumes_receiver: bool,
        span: &Span,
    ) -> (HirExprKind, ResolvedTy) {
        let CallTarget::DeclaredRuntime { family, .. } = &target else {
            unreachable!("declared runtime invocation requires its checked target");
        };
        let family = *family;
        let receiver = self.lower_expr(
            receiver,
            if consumes_receiver {
                IntentKind::Consume
            } else {
                IntentKind::Read
            },
        );
        let mut lowered_args = vec![receiver];
        lowered_args.extend(
            args.iter()
                .map(|arg| self.lower_expr(arg.expr(), IntentKind::Read)),
        );
        let callee = self.make_expr(
            HirExprKind::BindingRef {
                name: family.c_symbol().to_string(),
                resolved: ResolvedRef::Builtin(family),
            },
            ResolvedTy::Function {
                capabilities: hew_types::CallableCapabilities::FUNCTION_ITEM,
                params: lowered_args
                    .iter()
                    .map(|arg| hew_types::CallableParameter::borrowed(arg.ty.clone()))
                    .collect(),
                ret: Box::new(ResolvedTy::I32),
            },
            IntentKind::Read,
            span.clone(),
        );
        let call = self.make_expr(
            HirExprKind::Call {
                target,
                callee: Box::new(callee),
                args: lowered_args,
                evaluation_order: Vec::new(),
            },
            ResolvedTy::I32,
            IntentKind::Read,
            span.clone(),
        );
        (
            HirExprKind::Block(HirBlock {
                node: self.ids.node(),
                scope: self.ids.scope(),
                statements: vec![HirStmt {
                    node: self.ids.node(),
                    kind: HirStmtKind::Expr(call),
                    span: span.clone(),
                }],
                tail: None,
                ty: ResolvedTy::Unit,
                span: span.clone(),
            }),
            ResolvedTy::Unit,
        )
    }

    /// Build the `HirExprKind::MachineVariantCtor` node for a tuple-variant
    /// constructor call. Synthetic field names "0", "1", ... are used —
    /// MIR/codegen discard names and index payload slots by position (lane
    /// plan D1).
    pub(super) fn lower_variant_ctor_tuple_call(
        &mut self,
        name: &str,
        args: Vec<HirExpr>,
        span: &std::ops::Range<usize>,
    ) -> (HirExprKind, ResolvedTy) {
        let checker_ctor_ty = self.checker_expr_ty_if_present(span);
        let Some((type_name, variant_idx, variant_kind)) =
            self.lookup_variant_ctor(name, checker_ctor_ty.as_ref())
        else {
            unreachable!("lower_variant_ctor_tuple_call called without registry hit");
        };
        let HirVariantKind::Tuple(field_tys) = variant_kind else {
            unreachable!("lower_variant_ctor_tuple_call called with non-tuple variant");
        };
        let field_count = field_tys.len();
        let type_name_owned = type_name;
        let variant_idx_owned = variant_idx;
        if args.len() != field_count {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::EnumVariantConstructorArityMismatch {
                    variant: name.to_string(),
                    expected: field_count,
                    actual: args.len(),
                },
                span.clone(),
                "tuple-variant constructor called with the wrong number of arguments",
            ));
        }
        let payload: Vec<(String, HirExpr)> = args
            .into_iter()
            .enumerate()
            .map(|(idx, expr)| (idx.to_string(), expr))
            .collect();
        // Register the generic-enum instantiation before building result_ty so
        // the registry is populated when codegen performs its mangled lookup.

        // Checker-authoritative result type: the checker records the full
        // `Named { name: "Option", args: [I64] }` at this call-expression span.
        // Using it directly preserves type args so codegen can compute the
        // mangled registry key (e.g. `"Option$$i64"`).  Fall back to bare-name
        // with a diagnostic if expr_types has no entry — absence is a real
        // signal (checker should always populate this site).
        let checker_key = self.mk_key(span);
        let result_ty = if let Some(ty) = self.checked_expr_type(&checker_key).map(Cow::into_owned)
        {
            match ResolvedTy::from_ty(&ty) {
                Ok(resolved) => self.restore_type_declaration_facts(resolved),
                Err(err) => {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::CheckerBoundaryViolation {
                            name: type_name_owned.clone(),
                            reason: err.to_string(),
                        },
                        span.clone(),
                        "checker-authoritative variant-ctor result type failed boundary conversion",
                    ));
                    ResolvedTy::named_path(&self.defs, &type_name_owned, Vec::new())
                }
            }
        } else {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::CheckerBoundaryViolation {
                    name: type_name_owned.clone(),
                    reason: "expr_types has no entry for variant-ctor site".into(),
                },
                span.clone(),
                "checker did not record a result type for this variant constructor call",
            ));
            ResolvedTy::named_path(&self.defs, &type_name_owned, Vec::new())
        };
        (
            HirExprKind::MachineVariantCtor {
                machine_name: type_name_owned,
                state_idx: variant_idx_owned,
                payload: Some(payload),
            },
            result_ty,
        )
    }

    /// Emit a structured diagnostic when a variant constructor is called in
    /// the wrong shape (unit variant invoked positionally, or struct variant
    /// invoked positionally). Caller falls through to the regular-call path
    /// to preserve checker-stream coverage.
    pub(super) fn report_variant_ctor_call_shape_mismatch(
        &mut self,
        name: &str,
        kind: &HirVariantKind,
        span: &std::ops::Range<usize>,
    ) {
        match kind {
            HirVariantKind::Unit => {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::EnumVariantConstructorShapeMismatch {
                        variant: name.to_string(),
                    },
                    span.clone(),
                    "unit-variant constructors are referenced as identifiers, not called",
                ));
            }
            HirVariantKind::Struct(_) => {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::EnumVariantConstructorShapeMismatch {
                        variant: name.to_string(),
                    },
                    span.clone(),
                    "struct-variant constructors require named-field syntax (e.g. `Shape.Box { w: ..., h: ... }`)",
                ));
            }
            HirVariantKind::Tuple(_) => {
                // Tuple shape is the happy path handled by
                // `lower_variant_ctor_tuple_call`; this arm is unreachable in
                // production but kept exhaustive for the match.
            }
        }
    }

    pub(super) fn missing_stdlib_module_import(
        &self,
        name: &str,
        span: &Span,
    ) -> Option<&'static str> {
        let is_local = matches!(
            self.resolutions.get(&self.mk_key(span)),
            Some(Resolution::Local(_) | Resolution::Field(_, _))
        );
        if !is_local && !self.fn_registry.contains_key(name) {
            stdlib_catalog::missing_import_module(name)
        } else {
            None
        }
    }
}
