//! Postfix `?`, result coercion and scope recovery lowering.

use super::*;
use hew_parser::ast::Ident;

impl LowerCtx {
    pub(super) fn unsupported_postfix_try(
        &mut self,
        span: &std::ops::Range<usize>,
        construct: impl Into<String>,
    ) -> (HirExprKind, ResolvedTy) {
        self.unsupported(span.clone(), construct, "question-operator");
        (
            HirExprKind::Unsupported("unsupported `?` expression".into()),
            ResolvedTy::Unit,
        )
    }

    /// Wrap a function-tail expression in `Ok(value)` for the checker-marked
    /// tail Ok-coercion (`TypeCheckOutput::tail_ok_coercions`). `value` is the
    /// lowered tail, typed as the `Ok` payload; the result is the enclosing
    /// function's declared `Result<Ok, Err>` return type, taken from
    /// `current_return_type` (the same authority the `?` desugar uses to build
    /// its `Err(..)` return). Fails closed if the `Result::Ok` variant is
    /// missing from the ctor registry or the return type is not a `Result`.
    pub(super) fn wrap_tail_ok(
        &mut self,
        value: HirExpr,
        span: &std::ops::Range<usize>,
    ) -> HirExpr {
        let Some(return_ty) = self.current_return_type.clone() else {
            self.unsupported(
                span.clone(),
                "tail Ok-coercion without an enclosing return type",
                "tail-ok-coercion",
            );
            return value;
        };
        if Self::resolved_result_parts(&return_ty).is_none() {
            self.unsupported(
                span.clone(),
                "tail Ok-coercion on a non-Result return type",
                "tail-ok-coercion",
            );
            return value;
        }
        let Some((_, ok_idx)) = self.builtin_variant_predicate(BuiltinType::Result, "Ok", span)
        else {
            // `builtin_variant_predicate` already recorded a diagnostic.
            return value;
        };
        self.try_register_enum_instantiation_ty(&return_ty, span);
        self.synthetic_variant_ctor(
            "Result",
            ok_idx,
            Some(vec![("0".to_string(), value)]),
            return_ty,
            span,
        )
    }

    pub(super) fn apply_result_return_coercion(&mut self, value: HirExpr, span: &Span) -> HirExpr {
        if matches!(value.ty, ResolvedTy::Task(_))
            && self
                .result_return_coercions
                .contains_key(&self.mk_key(span))
        {
            self.diagnostics.push(HirDiagnostic::new(HirDiagnosticKind::TaskCannotEscape, span.clone(),
                "a `Task<T>` handle cannot escape inside a Result return; await it inside its scope"));
        }
        match self.result_return_coercions.get(&self.mk_key(span)) {
            Some(hew_types::ResultReturnKind::Success) => self.wrap_tail_ok(value, span),
            Some(hew_types::ResultReturnKind::Error) => {
                let Some(return_ty) = self
                    .current_return_type
                    .clone()
                    .filter(|ty| Self::resolved_result_parts(ty).is_some())
                else {
                    self.unsupported(
                        span.clone(),
                        "error return without an enclosing Result type",
                        "result-return",
                    );
                    return value;
                };
                let Some((_, index)) =
                    self.builtin_variant_predicate(BuiltinType::Result, "Err", span)
                else {
                    return value;
                };
                self.try_register_enum_instantiation_ty(&return_ty, span);
                self.synthetic_variant_ctor(
                    "Result",
                    index,
                    Some(vec![("0".to_string(), value)]),
                    return_ty,
                    span,
                )
            }
            None => value,
        }
    }

    pub(super) fn lower_scope_recovery(
        &mut self,
        operand: &Spanned<Expr>,
        body: &Spanned<Expr>,
        name: &str,
        binding_span: &Span,
        failure_ty: ResolvedTy,
        span: &Span,
    ) -> (HirExprKind, ResolvedTy) {
        let scope = self.lower_expr(operand, IntentKind::Read);
        if !matches!(
            scope.kind,
            HirExprKind::Scope { .. } | HirExprKind::ScopeDeadline { .. }
        ) {
            return self
                .unsupported_postfix_try(span, "checked scope recovery lacks a scope operand");
        }
        let Some(result_ty) = self.checker_expr_resolved_ty(span, "scope recovery") else {
            return self.unsupported_postfix_try(span, "missing checked scope recovery result");
        };
        self.try_register_enum_instantiation_ty(&failure_ty, binding_span);
        self.push_scope();
        let error = self.bind(name.to_string(), failure_ty, false, binding_span.clone());
        let handler = self.lower_expr(body, IntentKind::Read);
        self.pop_scope();
        (
            HirExprKind::ScopeRecovery {
                scope: Box::new(scope),
                error,
                handler: Box::new(handler),
            },
            result_ty,
        )
    }

    /// Normalize source-local recovery into the same typed variant branch as
    /// match. SIR decides payload transfers and cleanup; HIR supplies only
    /// the resolved types, variant identity, bindings and lexical bodies.
    pub(super) fn lower_local_recovery(
        &mut self,
        operand: &Spanned<Expr>,
        body: &Spanned<Expr>,
        error: Option<&Spanned<Ident>>,
        span: &Span,
    ) -> (HirExprKind, ResolvedTy) {
        let Some(recovery) = self.recovery_kinds.get(&self.mk_key(span)).cloned() else {
            return self.unsupported_postfix_try(span, "missing checked recovery semantics");
        };
        if let hew_types::check::RecoveryKind::Scope { failure_ty } = recovery {
            let Some((name, binding_span)) = error else {
                return self
                    .unsupported_postfix_try(span, "scope recovery requires an error binder");
            };
            let name = name.name.as_str();
            return self.lower_scope_recovery(operand, body, name, binding_span, failure_ty, span);
        }
        let scrutinee = self.lower_expr(operand, IntentKind::Read);
        self.try_register_enum_instantiation_ty(&scrutinee.ty, &operand.1);
        let (builtin, success, failure, payload, error_ty) = if error.is_some() {
            let Some((ok, err)) = Self::resolved_result_parts(&scrutinee.ty) else {
                return self.unsupported_postfix_try(span, "handle operand without a Result type");
            };
            (
                BuiltinType::Result,
                "Ok",
                "Err",
                ok.clone(),
                Some(err.clone()),
            )
        } else {
            let Some(some) = Self::resolved_option_inner(&scrutinee.ty) else {
                return self
                    .unsupported_postfix_try(span, "default operand without an Option type");
            };
            (BuiltinType::Option, "Some", "None", some.clone(), None)
        };
        let Some((success_predicate, _)) = self.builtin_variant_predicate(builtin, success, span)
        else {
            return self.unsupported_postfix_try(span, "recovery success variant identity");
        };
        let Some((failure_predicate, _)) = self.builtin_variant_predicate(builtin, failure, span)
        else {
            return self.unsupported_postfix_try(span, "recovery failure variant identity");
        };
        let success_binding = self.ids.binding();
        let success_body =
            self.synthetic_binding_ref("__recovery_value", success_binding, payload.clone(), span);
        self.push_scope();
        let error_bindings = if let (Some((name, binding_span)), Some(error_ty)) = (error, error_ty)
        {
            let binding = self.bind(
                name.to_string(),
                error_ty.clone(),
                false,
                binding_span.clone(),
            );
            vec![HirMatchArmBinding {
                span: binding.span.clone(),
                binding: binding.id,
                field_idx: 0,
                name: name.to_string(),
                ty: error_ty,
            }]
        } else {
            Vec::new()
        };
        let failure_body = self.lower_expr(body, IntentKind::Read);
        self.pop_scope();
        let arms = vec![
            HirMatchArm {
                scope: Some(self.ids.scope()),
                predicate: success_predicate,
                bindings: vec![HirMatchArmBinding {
                    span: span.clone(),
                    binding: success_binding,
                    field_idx: 0,
                    name: "__recovery_value".to_string(),
                    ty: payload.clone(),
                }],
                payload_predicates: Vec::new(),
                payload_variant_predicates: Vec::new(),
                guard: None,
                body: success_body,
                span: span.clone(),
            },
            HirMatchArm {
                scope: Some(self.ids.scope()),
                predicate: failure_predicate,
                bindings: error_bindings,
                payload_predicates: Vec::new(),
                payload_variant_predicates: Vec::new(),
                guard: None,
                body: failure_body,
                span: body.1.clone(),
            },
        ];
        (
            HirExprKind::Match {
                scrutinee: Box::new(scrutinee),
                arms,
            },
            payload,
        )
    }

    pub(super) fn lower_postfix_try(
        &mut self,
        inner: &Spanned<Expr>,
        span: &std::ops::Range<usize>,
    ) -> (HirExprKind, ResolvedTy) {
        let scrutinee = self.lower_expr(inner, IntentKind::Read);
        let scrutinee_ty = scrutinee.ty.clone();
        self.try_register_enum_instantiation_ty(&scrutinee_ty, &inner.1);

        let Some(return_ty) = self.current_return_type.clone() else {
            return self.unsupported_postfix_try(
                span,
                "`?` without an enclosing Result/Option return type",
            );
        };

        if let Some((ok_ty, err_ty)) = Self::resolved_result_parts(&scrutinee_ty) {
            if Self::resolved_result_parts(&return_ty).is_none() {
                return self.unsupported_postfix_try(
                    span,
                    "`?` in a body whose return type is not Result",
                );
            }
            self.try_register_enum_instantiation_ty(&return_ty, span);
            let Some(result_ty) = self.checker_expr_resolved_ty(span, "`?` expression") else {
                return self.unsupported_postfix_try(span, "`?` checker payload type is missing");
            };
            if &result_ty != ok_ty {
                return self.unsupported_postfix_try(
                    span,
                    format!(
                        "`?` checker payload type `{result_ty}` disagrees with Result::Ok payload type `{ok_ty}`",
                    ),
                );
            }
            self.lower_result_postfix_try(scrutinee, result_ty, err_ty.clone(), return_ty, span)
        } else if let Some(some_ty) = Self::resolved_option_inner(&scrutinee_ty) {
            if Self::resolved_option_inner(&return_ty).is_none() {
                return self.unsupported_postfix_try(
                    span,
                    "`?` in a body whose return type is not Option",
                );
            }
            self.try_register_enum_instantiation_ty(&return_ty, span);
            let Some(result_ty) = self.checker_expr_resolved_ty(span, "`?` expression") else {
                return self.unsupported_postfix_try(span, "`?` checker payload type is missing");
            };
            if &result_ty != some_ty {
                return self.unsupported_postfix_try(
                    span,
                    format!(
                        "`?` checker payload type `{result_ty}` disagrees with Option::Some payload type `{some_ty}`",
                    ),
                );
            }
            self.lower_option_postfix_try(scrutinee, result_ty, return_ty, span)
        } else {
            self.unsupported_postfix_try(span, "`?` scrutinee that is not Result or Option")
        }
    }

    pub(super) fn lower_result_postfix_try(
        &mut self,
        scrutinee: HirExpr,
        ok_ty: ResolvedTy,
        err_ty: ResolvedTy,
        return_ty: ResolvedTy,
        span: &std::ops::Range<usize>,
    ) -> (HirExprKind, ResolvedTy) {
        let Some((ok_predicate, _)) =
            self.builtin_variant_predicate(BuiltinType::Result, "Ok", span)
        else {
            return self.unsupported_postfix_try(span, "`?` Result.Ok predicate");
        };
        let Some((err_predicate, err_idx)) =
            self.builtin_variant_predicate(BuiltinType::Result, "Err", span)
        else {
            return self.unsupported_postfix_try(span, "`?` Result.Err predicate");
        };

        let ok_binding = self.ids.binding();
        let err_binding = self.ids.binding();
        let ok_name = "__try_ok";
        let err_name = "__try_err";

        let ok_body = self.synthetic_binding_ref(ok_name, ok_binding, ok_ty.clone(), span);
        let err_payload = self.synthetic_binding_ref(err_name, err_binding, err_ty.clone(), span);
        let err_ctor = self.synthetic_variant_ctor(
            "Result",
            err_idx,
            Some(vec![("0".to_string(), err_payload)]),
            return_ty,
            span,
        );
        let err_body = self.synthetic_return_block_expr(err_ctor, span);

        let arms = vec![
            HirMatchArm {
                scope: Some(self.ids.scope()),
                predicate: ok_predicate,
                bindings: vec![HirMatchArmBinding {
                    span: span.clone(),
                    binding: ok_binding,
                    field_idx: 0,
                    name: ok_name.to_string(),
                    ty: ok_ty.clone(),
                }],
                payload_predicates: Vec::new(),
                payload_variant_predicates: Vec::new(),
                guard: None,
                body: ok_body,
                span: span.clone(),
            },
            HirMatchArm {
                scope: Some(self.ids.scope()),
                predicate: err_predicate,
                bindings: vec![HirMatchArmBinding {
                    span: span.clone(),
                    binding: err_binding,
                    field_idx: 0,
                    name: err_name.to_string(),
                    ty: err_ty,
                }],
                payload_predicates: Vec::new(),
                payload_variant_predicates: Vec::new(),
                guard: None,
                body: err_body,
                span: span.clone(),
            },
        ];

        (
            HirExprKind::Match {
                scrutinee: Box::new(scrutinee),
                arms,
            },
            ok_ty,
        )
    }

    pub(super) fn lower_option_postfix_try(
        &mut self,
        scrutinee: HirExpr,
        some_ty: ResolvedTy,
        return_ty: ResolvedTy,
        span: &std::ops::Range<usize>,
    ) -> (HirExprKind, ResolvedTy) {
        let Some((some_predicate, _)) =
            self.builtin_variant_predicate(BuiltinType::Option, "Some", span)
        else {
            return self.unsupported_postfix_try(span, "`?` Option.Some predicate");
        };
        let Some((none_predicate, none_idx)) =
            self.builtin_variant_predicate(BuiltinType::Option, "None", span)
        else {
            return self.unsupported_postfix_try(span, "`?` Option.None predicate");
        };

        let some_binding = self.ids.binding();
        let some_name = "__try_some";
        let some_body = self.synthetic_binding_ref(some_name, some_binding, some_ty.clone(), span);
        let none_ctor = self.synthetic_variant_ctor("Option", none_idx, None, return_ty, span);
        let none_body = self.synthetic_return_block_expr(none_ctor, span);

        let arms = vec![
            HirMatchArm {
                scope: Some(self.ids.scope()),
                predicate: some_predicate,
                bindings: vec![HirMatchArmBinding {
                    span: span.clone(),
                    binding: some_binding,
                    field_idx: 0,
                    name: some_name.to_string(),
                    ty: some_ty.clone(),
                }],
                payload_predicates: Vec::new(),
                payload_variant_predicates: Vec::new(),
                guard: None,
                body: some_body,
                span: span.clone(),
            },
            HirMatchArm {
                scope: None,
                predicate: none_predicate,
                bindings: Vec::new(),
                payload_predicates: Vec::new(),
                payload_variant_predicates: Vec::new(),
                guard: None,
                body: none_body,
                span: span.clone(),
            },
        ];

        (
            HirExprKind::Match {
                scrutinee: Box::new(scrutinee),
                arms,
            },
            some_ty,
        )
    }
}
