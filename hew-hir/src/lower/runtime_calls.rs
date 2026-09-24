//! Runtime invocations, send status results and variant constructor calls.

use super::*;

impl LowerCtx {
    /// Consume the checked source contract and concrete callback identities.
    /// The runtime status is adapted with ordinary value/branch constructs, so
    /// SIR receives one raw invocation and owns the same cleanup paths as any
    /// other call and Result construction.
    pub(super) fn lower_declared_runtime_invocation(
        &mut self,
        target: CallTarget,
        receiver: &Spanned<Expr>,
        args: &[hew_parser::ast::CallArg],
        consumes_receiver: bool,
        result_ty: ResolvedTy,
        span: &Span,
    ) -> (HirExprKind, ResolvedTy) {
        use hew_types::check::dispatch::ResolvedRuntimeResult;
        let CallTarget::DeclaredRuntime { family, result, .. } = &target else {
            unreachable!("declared runtime invocation requires its checked target");
        };
        let family = *family;
        let adaptation = result.clone();
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
                params: lowered_args.iter().map(|arg| arg.ty.clone()).collect(),
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
        if adaptation == ResolvedRuntimeResult::DiscardStatus {
            return (
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
            );
        }
        let ResolvedRuntimeResult::StatusResult { error } = adaptation else {
            unreachable!("status discard was handled above");
        };
        self.lower_runtime_status_result(call, &error, result_ty, span)
    }

    /// Fold a send status into `Result<(), SendError>`: `0` is `Ok(())`, the
    /// table's code is its named error, and any other status is the table's
    /// remaining error.
    #[allow(
        clippy::too_many_lines,
        reason = "one fold builds every constructor of the Result it returns"
    )]
    pub(super) fn lower_send_status_result(
        &mut self,
        status: HirExpr,
        result_ty: ResolvedTy,
        codes: SendStatusCodes,
        span: &Span,
    ) -> (HirExprKind, ResolvedTy) {
        let ResolvedTy::Named { args, .. } = &result_ty else {
            return (
                HirExprKind::Unsupported("send result is not a Result".to_string()),
                result_ty,
            );
        };
        let Some(error_ty) = args.get(1).cloned() else {
            return (
                HirExprKind::Unsupported("send result has no error arm".to_string()),
                result_ty,
            );
        };
        // `SendError` is a monomorphic builtin enum registered under its
        // canonical `std.builtins` owner, the same identity `Err(SendError.X)`
        // match arms resolve through.
        let error_name = hew_types::builtin_enums::monomorphic_builtin_enum(
            BuiltinType::SendError.canonical_name(),
        )
        .map(|declaration| declaration.canonical_name.to_string());
        let variant_index = |this: &Self, variant: &str| {
            let error_name = error_name.as_ref()?;
            this.machine_ctor_registry
                .get(&format!("{error_name}::{variant}"))
                .map(|(_, index)| *index)
        };
        let constructors = variant_index(self, codes.named.1)
            .zip(variant_index(self, codes.otherwise))
            .zip(self.builtin_variant_predicate(BuiltinType::Result, "Ok", span))
            .zip(self.builtin_variant_predicate(BuiltinType::Result, "Err", span));
        let Some((((closed_index, full_index), ok), err)) = constructors else {
            return (
                HirExprKind::Unsupported("send result constructors".to_string()),
                result_ty,
            );
        };
        let (ok_index, err_index) = (ok.1, err.1);
        let error_name = error_name.unwrap_or_else(|| "SendError".to_string());
        let unit = self.make_expr(
            HirExprKind::Literal(HirLiteral::Unit),
            ResolvedTy::Unit,
            IntentKind::Read,
            span.clone(),
        );
        let accepted = self.synthetic_variant_ctor(
            "Result",
            ok_index,
            Some(vec![("0".to_string(), unit)]),
            result_ty.clone(),
            span,
        );
        let closed =
            self.synthetic_variant_ctor(&error_name, closed_index, None, error_ty.clone(), span);
        let closed = self.synthetic_variant_ctor(
            "Result",
            err_index,
            Some(vec![("0".to_string(), closed)]),
            result_ty.clone(),
            span,
        );
        let full = self.synthetic_variant_ctor(&error_name, full_index, None, error_ty, span);
        let full = self.synthetic_variant_ctor(
            "Result",
            err_index,
            Some(vec![("0".to_string(), full)]),
            result_ty.clone(),
            span,
        );
        let literal = |this: &mut Self, value: i128| {
            this.make_expr(
                HirExprKind::Literal(HirLiteral::Integer(value)),
                ResolvedTy::I32,
                IntentKind::Read,
                span.clone(),
            )
        };
        // The status is consulted twice, so it is bound once: a cloned call
        // node would reuse its node and site ids.
        let status_binding = self.ids.binding();
        let status_name = "__send_status";
        let status_ref = |this: &mut Self| {
            this.synthetic_binding_ref(status_name, status_binding, ResolvedTy::I32, span)
        };
        let is_closed = {
            let one = literal(self, codes.named.0);
            let status = status_ref(self);
            self.make_expr(
                HirExprKind::Binary {
                    op: hew_parser::ast::BinaryOp::Equal,
                    left: Box::new(status),
                    right: Box::new(one),
                },
                ResolvedTy::Bool,
                IntentKind::Read,
                span.clone(),
            )
        };
        let closed_or_full = self.make_expr(
            HirExprKind::If {
                condition: Box::new(is_closed),
                then_expr: Box::new(closed),
                else_expr: Some(Box::new(full)),
            },
            result_ty.clone(),
            IntentKind::Consume,
            span.clone(),
        );
        let zero = literal(self, 0);
        let status_read = status_ref(self);
        let is_accepted = self.make_expr(
            HirExprKind::Binary {
                op: hew_parser::ast::BinaryOp::Equal,
                left: Box::new(status_read),
                right: Box::new(zero),
            },
            ResolvedTy::Bool,
            IntentKind::Read,
            span.clone(),
        );
        let folded = self.make_expr(
            HirExprKind::If {
                condition: Box::new(is_accepted),
                then_expr: Box::new(accepted),
                else_expr: Some(Box::new(closed_or_full)),
            },
            result_ty.clone(),
            IntentKind::Consume,
            span.clone(),
        );
        let bind_status = HirStmt {
            node: self.ids.node(),
            kind: HirStmtKind::Let(
                HirBinding {
                    id: status_binding,
                    name: status_name.to_string(),
                    ty: ResolvedTy::I32,
                    mutable: false,
                    span: span.clone(),
                    is_consume: false,
                },
                Some(status),
            ),
            span: span.clone(),
        };
        (
            HirExprKind::Block(HirBlock {
                node: self.ids.node(),
                scope: self.ids.scope(),
                statements: vec![bind_status],
                tail: Some(Box::new(folded)),
                ty: result_ty.clone(),
                span: span.clone(),
            }),
            result_ty,
        )
    }

    /// Map the raw runtime status to the source-selected Result constructors.
    pub(super) fn lower_runtime_status_result(
        &mut self,
        call: HirExpr,
        error: &hew_types::VariantMatch,
        result_ty: ResolvedTy,
        span: &Span,
    ) -> (HirExprKind, ResolvedTy) {
        let error_ty = self.qualify_current_module_record_ty(ResolvedTy::Named {
            name: error.type_name.clone(),
            args: Vec::new(),
            builtin: None,
            is_opaque: false,
        });
        let error_key = format!("{}::{}", error.type_name, error.variant_name);
        // This is the ordinary checked VariantMatch identity. Project its
        // exact constructor into the HIR layout registry, without resolving a
        // source spelling or retrying a short variant name.
        let constructors = self
            .machine_ctor_registry
            .get(&error_key)
            .cloned()
            .zip(self.builtin_variant_predicate(BuiltinType::Result, "Ok", span))
            .zip(self.builtin_variant_predicate(BuiltinType::Result, "Err", span));
        let Some((((error_name, error_index), (_, ok_index)), (_, err_index))) = constructors
        else {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::CheckerBoundaryViolation {
                    name: error_key,
                    reason: "checked runtime result constructor has no HIR layout".to_string(),
                },
                span.clone(),
                "runtime result constructor facts could not be lowered",
            ));
            return (
                HirExprKind::Unsupported("invalid declared runtime result".to_string()),
                result_ty,
            );
        };
        let error = self.synthetic_variant_ctor(&error_name, error_index, None, error_ty, span);
        let unit = self.make_expr(
            HirExprKind::Literal(HirLiteral::Unit),
            ResolvedTy::Unit,
            IntentKind::Read,
            span.clone(),
        );
        let success = self.synthetic_variant_ctor(
            "Result",
            ok_index,
            Some(vec![("0".to_string(), unit)]),
            result_ty.clone(),
            span,
        );
        let refusal = self.synthetic_variant_ctor(
            "Result",
            err_index,
            Some(vec![("0".to_string(), error)]),
            result_ty.clone(),
            span,
        );
        let zero = self.make_expr(
            HirExprKind::Literal(HirLiteral::Integer(0)),
            ResolvedTy::I32,
            IntentKind::Read,
            span.clone(),
        );
        let condition = self.make_expr(
            HirExprKind::Binary {
                op: hew_parser::ast::BinaryOp::Equal,
                left: Box::new(call),
                right: Box::new(zero),
            },
            ResolvedTy::Bool,
            IntentKind::Read,
            span.clone(),
        );
        (
            HirExprKind::If {
                condition: Box::new(condition),
                then_expr: Box::new(success),
                else_expr: Some(Box::new(refusal)),
            },
            result_ty,
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
        self.try_register_enum_instantiation(span);
        // Checker-authoritative result type: the checker records the full
        // `Named { name: "Option", args: [I64] }` at this call-expression span.
        // Using it directly preserves type args so codegen can compute the
        // mangled registry key (e.g. `"Option$$i64"`).  Fall back to bare-name
        // with a diagnostic if expr_types has no entry — absence is a real
        // signal (checker should always populate this site).
        let checker_key = self.mk_key(span);
        let result_ty = if let Some(ty) = self.expr_types.get(&checker_key).cloned() {
            match ResolvedTy::from_ty(&ty) {
                Ok(resolved) => self.qualify_current_module_record_ty(resolved),
                Err(err) => {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::CheckerBoundaryViolation {
                            name: type_name_owned.clone(),
                            reason: err.to_string(),
                        },
                        span.clone(),
                        "checker-authoritative variant-ctor result type failed boundary conversion",
                    ));
                    ResolvedTy::Named {
                        name: type_name_owned.clone(),
                        args: Vec::new(),
                        builtin: None,
                        is_opaque: false,
                    }
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
            ResolvedTy::Named {
                name: type_name_owned.clone(),
                args: Vec::new(),
                builtin: None,
                is_opaque: false,
            }
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

    pub(super) fn missing_stdlib_module_import(&self, name: &str) -> Option<&'static str> {
        if self.lookup(name).is_none() && !self.fn_registry.contains_key(name) {
            stdlib_catalog::missing_import_module(name)
        } else {
            None
        }
    }
}
