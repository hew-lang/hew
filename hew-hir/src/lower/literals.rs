//! Literal and unary-expression lowering.

use super::*;

impl LowerCtx {
    pub(super) fn lower_literal(lit: &Literal) -> (HirExprKind, ResolvedTy) {
        match lit {
            Literal::Integer { value, .. } => (
                HirExprKind::Literal(HirLiteral::Integer(*value)),
                ResolvedTy::I64,
            ),
            Literal::Float(value) => (
                HirExprKind::Literal(HirLiteral::Float(*value)),
                ResolvedTy::F64,
            ),
            Literal::String(value) => (
                HirExprKind::Literal(HirLiteral::String(value.clone())),
                ResolvedTy::String,
            ),
            Literal::Bool(value) => (
                HirExprKind::Literal(HirLiteral::Bool(*value)),
                ResolvedTy::Bool,
            ),
            Literal::Char(value) => (
                HirExprKind::Literal(HirLiteral::Char(*value)),
                ResolvedTy::Char,
            ),
            Literal::Duration(value) => (
                HirExprKind::Literal(HirLiteral::Duration(*value)),
                ResolvedTy::Duration,
            ),
        }
    }

    /// #2372: fold `-<int-literal>` into a single signed literal instead of
    /// negating an already-narrowed-to-width literal at runtime.
    ///
    /// Mirrors the `Expr::Literal(lit)` arm of `lower_expr_inner` exactly
    /// (same `expr_types` lookup, same `unwrap_or(default_ty)` fallback):
    /// after the fold this expression IS semantically a literal, so it
    /// inherits the literal contract rather than the compound-expression one.
    /// `value` is always non-negative here -- the parser hands
    /// `lower_unary_expr` a bare `Literal` operand under `Negate` and every
    /// literal magnitude fits the `i128` carrier, so `-value` cannot overflow.
    pub(super) fn lower_negated_int_literal(
        &mut self,
        value: i128,
        span: &Span,
    ) -> (HirExprKind, ResolvedTy) {
        let negated = -value;
        let default_ty = ResolvedTy::I64;
        let ty = {
            let checker_key = self.mk_key(span);
            if let Some(checker_ty) = self.expr_types.get(&checker_key) {
                ResolvedTy::from_ty(checker_ty).unwrap_or(default_ty)
            } else {
                default_ty
            }
        };
        self.assert_resolved_ty_totality(span);
        (HirExprKind::Literal(HirLiteral::Integer(negated)), ty)
    }

    pub(super) fn lower_unary_expr(
        &mut self,
        op: UnaryOp,
        operand: &Spanned<Expr>,
        span: &Span,
    ) -> (HirExprKind, ResolvedTy) {
        if op == UnaryOp::Negate {
            if let Expr::Literal(Literal::Integer { value, .. }) = &operand.0 {
                return self.lower_negated_int_literal(*value, span);
            }
        }

        let operand_expr = self.lower_expr(operand, IntentKind::Read);

        if op == UnaryOp::RawDeref {
            self.unsupported(
                span.clone(),
                "raw pointer unary dereference",
                "m5-raw-pointers",
            );
            return (
                HirExprKind::Unsupported("unsupported raw pointer unary dereference".into()),
                ResolvedTy::Unit,
            );
        }

        let Some(result_ty) = self.checker_expr_ty(span, "unary expression") else {
            return (
                HirExprKind::Unsupported("unary expression missing checker result type".into()),
                ResolvedTy::Unit,
            );
        };
        let Some(operand_ty) = self.checker_unary_operand_ty(op, operand, &result_ty) else {
            return (
                HirExprKind::Unsupported("unary expression missing checker operand type".into()),
                ResolvedTy::Unit,
            );
        };

        if !Self::unary_shape_supported(op, &operand_ty, &result_ty) {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::UnaryOperatorUnsupportedInMir {
                    op: Self::unary_op_label(op).to_string(),
                    operand_ty: operand_ty.to_string(),
                    result_ty: result_ty.to_string(),
                },
                span.clone(),
                "checker-approved unary expression has no MIR/codegen lowering for this typed shape",
            ));
            return (
                HirExprKind::Unsupported(format!(
                    "unsupported unary `{}` for operand `{operand_ty}` -> `{result_ty}`",
                    Self::unary_op_label(op)
                )),
                result_ty,
            );
        }

        (
            HirExprKind::Unary {
                op,
                operand: Box::new(operand_expr),
                operand_ty,
            },
            result_ty,
        )
    }

    /// Callable joins carry the guarantee intersection selected by the checker.
    /// Arm order cannot select the ownership contract of the resulting value.
    pub(super) fn callable_join_type(&mut self, span: &Span, inferred: ResolvedTy) -> ResolvedTy {
        if !inferred.to_ty().contains_callable() {
            return inferred;
        }
        self.checker_expr_ty(span, "callable join")
            .unwrap_or(ResolvedTy::Unit)
    }

    pub(super) fn checker_expr_ty(&mut self, span: &Span, label: &str) -> Option<ResolvedTy> {
        let key = self.mk_key(span);
        let Some(ty) = self.expr_types.get(&key).cloned() else {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::CheckerBoundaryViolation {
                    name: label.to_string(),
                    reason: "missing expr_types entry".to_string(),
                },
                span.clone(),
                "checker-authoritative expression type is required for lowering",
            ));
            return None;
        };
        match ResolvedTy::from_ty(&ty) {
            Ok(resolved) => Some(self.qualify_current_module_record_ty(resolved)),
            Err(err) => {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::CheckerBoundaryViolation {
                        name: label.to_string(),
                        reason: err.to_string(),
                    },
                    span.clone(),
                    "checker-authoritative expression type failed boundary conversion",
                ));
                None
            }
        }
    }

    pub(super) fn checker_unary_operand_ty(
        &mut self,
        op: UnaryOp,
        operand: &Spanned<Expr>,
        result_ty: &ResolvedTy,
    ) -> Option<ResolvedTy> {
        let key = self.mk_key(&operand.1);
        match self.expr_types.get(&key).cloned() {
            Some(ty) => match ResolvedTy::from_ty(&ty) {
                Ok(resolved) => {
                    // The operand span may have been recorded as `IntLiteral`
                    // during synthesis and then materialized to `I64` by
                    // `materialize_literal_defaults` at the checker boundary.
                    // If the outer result was narrowed to a different concrete
                    // integer (e.g. `I32`) by deferred range-bound resolution,
                    // the `I64` is a stale materialization artifact.  Use
                    // `result_ty` for the operand so `negate(5_literal) →
                    // i32` does not trip the `operand_ty == result_ty` shape
                    // guard in `unary_shape_supported`.
                    if Self::unary_literal_operand_uses_result_ty(op, &operand.0)
                        && Self::resolved_is_integer(&resolved)
                        && Self::resolved_is_integer(result_ty)
                        && resolved != *result_ty
                    {
                        Some(result_ty.clone())
                    } else {
                        Some(resolved)
                    }
                }
                Err(_) if Self::unary_literal_operand_uses_result_ty(op, &operand.0) => {
                    Some(result_ty.clone())
                }
                Err(err) => {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::CheckerBoundaryViolation {
                            name: "unary operand".to_string(),
                            reason: err.to_string(),
                        },
                        operand.1.clone(),
                        "checker-authoritative unary operand type failed boundary conversion",
                    ));
                    None
                }
            },
            None if Self::unary_literal_operand_uses_result_ty(op, &operand.0) => {
                Some(result_ty.clone())
            }
            None => {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::CheckerBoundaryViolation {
                        name: "unary operand".to_string(),
                        reason: "missing expr_types entry".to_string(),
                    },
                    operand.1.clone(),
                    "checker-authoritative expression type is required for lowering",
                ));
                None
            }
        }
    }

    pub(super) fn unary_literal_operand_uses_result_ty(op: UnaryOp, operand: &Expr) -> bool {
        matches!(operand, Expr::Literal(_))
            && matches!(op, UnaryOp::Not | UnaryOp::Negate | UnaryOp::BitNot)
    }

    pub(super) fn unary_shape_supported(
        op: UnaryOp,
        operand_ty: &ResolvedTy,
        result_ty: &ResolvedTy,
    ) -> bool {
        match op {
            UnaryOp::Not => operand_ty == &ResolvedTy::Bool && result_ty == &ResolvedTy::Bool,
            UnaryOp::Negate => {
                operand_ty == result_ty
                    && (Self::resolved_is_integer(operand_ty)
                        || Self::resolved_is_float(operand_ty))
            }
            UnaryOp::BitNot => operand_ty == result_ty && Self::resolved_is_integer(operand_ty),
            UnaryOp::RawDeref => false,
        }
    }

    /// `Some(reason)` when a channel/stream element type provably cannot ride
    /// the element-layout queue witness; `None` for every describable class.
    ///
    /// This is defence-in-depth behind the checker's `queue_elem_admissible`
    /// gate (which covers the `Stream<T>` method-call path): the for-await
    /// desugar can reach a `Stream<T>`
    /// whose element no method call ever validated. The HIR layer rejects only
    /// the classes the witness can NEVER describe — builtin container/handle
    /// nominals, opaque handles, function values, and unit/never — and admits
    /// the rest; codegen's witness synthesis stays the fail-closed authority
    /// for anything that slips past both layers.
    pub(super) fn queue_elem_witness_unsupported(ty: &ResolvedTy) -> Option<&'static str> {
        match ty {
            ResolvedTy::String
            | ResolvedTy::Bytes
            | ResolvedTy::F32
            | ResolvedTy::F64
            | ResolvedTy::Bool
            | ResolvedTy::Char
            | ResolvedTy::Duration
            | ResolvedTy::Tuple(_) => None,
            ResolvedTy::Named {
                builtin: None,
                is_opaque,
                ..
            } => {
                if *is_opaque {
                    Some("opaque handle types cannot be queue elements")
                } else {
                    None
                }
            }
            ResolvedTy::Named {
                builtin: Some(_), ..
            } => Some(
                "builtin container and handle types cannot ride the \
                 element-layout queue witness",
            ),
            _ if Self::resolved_is_integer(ty) => None,
            _ => Some("this type has no element-layout queue witness"),
        }
    }

    pub(super) fn resolved_is_integer(ty: &ResolvedTy) -> bool {
        matches!(
            ty,
            ResolvedTy::I8
                | ResolvedTy::I16
                | ResolvedTy::I32
                | ResolvedTy::I64
                | ResolvedTy::U8
                | ResolvedTy::U16
                | ResolvedTy::U32
                | ResolvedTy::U64
                | ResolvedTy::Isize
                | ResolvedTy::Usize
        )
    }

    pub(super) fn resolved_is_float(ty: &ResolvedTy) -> bool {
        matches!(ty, ResolvedTy::F32 | ResolvedTy::F64)
    }
}
