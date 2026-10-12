//! Constant declaration lowering and folding.

use super::*;
use hew_types::compile_time::ConstantValue as HirConstValue;

impl LowerCtx {
    pub(super) fn lower_constant_value(
        &mut self,
        value: &HirConstValue,
        ty: &ResolvedTy,
        span: &Span,
        intent: IntentKind,
    ) -> HirExpr {
        let kind = match value {
            HirConstValue::Integer(value) => HirExprKind::Literal(HirLiteral::Integer(*value)),
            HirConstValue::Float(value) => HirExprKind::Literal(HirLiteral::Float(*value)),
            HirConstValue::Duration(value) => HirExprKind::Literal(HirLiteral::Duration(*value)),
            HirConstValue::Bool(value) => HirExprKind::Literal(HirLiteral::Bool(*value)),
            HirConstValue::Char(value) => HirExprKind::Literal(HirLiteral::Char(*value)),
            HirConstValue::Unit => HirExprKind::Literal(HirLiteral::Unit),
            HirConstValue::String(value) => HirExprKind::Literal(HirLiteral::String(value.clone())),
            HirConstValue::Bytes(value) => HirExprKind::Literal(HirLiteral::Bytes(value.clone())),
            HirConstValue::Tuple(elements) => HirExprKind::TupleLiteral {
                elements: elements
                    .iter()
                    .map(|element| {
                        self.lower_constant_value(
                            &element.value,
                            &element.ty,
                            span,
                            IntentKind::Read,
                        )
                    })
                    .collect(),
            },
            HirConstValue::Array(elements) => HirExprKind::ArrayLiteral {
                elements: elements
                    .iter()
                    .map(|element| {
                        self.lower_constant_value(
                            &element.value,
                            &element.ty,
                            span,
                            IntentKind::Read,
                        )
                    })
                    .collect(),
            },
            HirConstValue::Vector(elements) => {
                let elements = elements
                    .iter()
                    .map(|element| {
                        self.lower_constant_value(
                            &element.value,
                            &element.ty,
                            span,
                            IntentKind::Read,
                        )
                    })
                    .collect();
                self.make_sequence_literal(elements, ty, span)
            }
            HirConstValue::Set(elements) => {
                let elements = elements
                    .iter()
                    .map(|element| {
                        self.lower_constant_value(
                            &element.value,
                            &element.ty,
                            span,
                            IntentKind::Read,
                        )
                    })
                    .collect();
                self.make_set_literal(elements, ty, span)
            }
            HirConstValue::Map(entries) => {
                let entries = entries
                    .iter()
                    .map(|(key, value)| {
                        (
                            self.lower_constant_value(&key.value, &key.ty, span, IntentKind::Read),
                            self.lower_constant_value(
                                &value.value,
                                &value.ty,
                                span,
                                IntentKind::Read,
                            ),
                        )
                    })
                    .collect();
                self.make_map_literal(entries, ty, span)
            }
            HirConstValue::Variant { index, fields } => HirExprKind::MachineVariantCtor {
                machine_name: ty.user_facing().to_string(),
                state_idx: *index,
                payload: (!fields.is_empty()).then(|| {
                    fields
                        .iter()
                        .map(|(name, field)| {
                            (
                                name.clone(),
                                self.lower_constant_value(
                                    &field.value,
                                    &field.ty,
                                    span,
                                    IntentKind::Read,
                                ),
                            )
                        })
                        .collect()
                }),
            },
            HirConstValue::Record(fields) => HirExprKind::StructInit {
                name: ty.user_facing().to_string(),
                type_args: Vec::new(),
                fields: fields
                    .iter()
                    .map(|(name, field)| {
                        (
                            name.clone(),
                            self.lower_constant_value(
                                &field.value,
                                &field.ty,
                                span,
                                IntentKind::Read,
                            ),
                        )
                    })
                    .collect(),
                base: None,
            },
        };
        HirExpr {
            node: self.ids.node(),
            site: self.ids.site(),
            ty: ty.clone(),
            intent,
            span: span.clone(),
            kind,
        }
    }

    /// First-pass registration of a module-level `const`. Allocates a stable
    /// `ItemId` and records the declared (checker-resolved) type so const
    /// references resolve regardless of source order. The folded value is
    /// produced later, canonically, in [`Self::lower_const`].
    pub(super) fn register_const_entry(&mut self, decl: &ConstDecl, span: &Span, bare: bool) {
        let Some(declaration) = self.source_declaration(span, hew_types::DeclarationKind::Const, 0)
        else {
            return;
        };
        let path = self.defs.path(declaration).to_string();
        let entry = if let Some(entry) = self.source_const_entries.get(&declaration) {
            entry.clone()
        } else {
            ConstEntry {
                id: self.ids.item(),
                ty: self.lower_type(&decl.ty),
            }
        };
        self.source_const_entries.insert(declaration, entry.clone());
        self.const_registry.insert(path, entry.clone());
        if bare {
            self.const_registry.insert(decl.name.to_string(), entry);
        }
    }

    /// Fold a module constant and project it into ordinary typed HIR.
    pub(super) fn lower_const(
        &mut self,
        decl: &ConstDecl,
        span: std::ops::Range<usize>,
    ) -> Option<crate::node::HirConst> {
        let declaration = self.source_declaration(&span, hew_types::DeclarationKind::Const, 0)?;
        let Some(entry) = self.source_const_entries.get(&declaration) else {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::CheckerBoundaryViolation {
                    name: self.defs.path(declaration).to_string(),
                    reason: "constant declaration has no registered HIR item".into(),
                },
                span,
                "constant registration must preserve the checker declaration",
            ));
            return None;
        };
        let (id, ty) = (entry.id, entry.ty.clone());

        let value = match self.compile_time_values.get(&self.mk_key(&decl.value.1)) {
            Some(value) => value.clone(),
            None => self.fold_const_expr(&decl.value.0, &ty, span.clone()),
        };
        if let HirConstValue::Integer(value) = &value {
            self.folded_integer_consts
                .insert(decl.name.to_string(), *value);
        }

        let value = self.lower_constant_value(&value, &ty, &decl.value.1, IntentKind::Consume);
        Some(crate::node::HirConst {
            id,
            node: self.ids.node(),
            declaration,
            name: decl.name.to_string(),
            ty,
            value,
            span,
        })
    }

    /// Fail-closed constant-fold for module-level `const` initializers.
    ///
    /// WHY this shape: integer const-evaluation is delegated wholesale to
    /// [`hew_types::check::const_eval::eval_integer_const_expr`], the single sanctioned
    /// constexpr authority (A620 / Q329 — const-eval must reuse `const_eval.rs`,
    /// not fork a parallel evaluator). Only two things live here: the
    /// string-literal short-circuit (strings are non-arithmetic and `const_eval`
    /// is integer-only, so they legitimately stay HIR-local) and the
    /// target-typed result → [`HirConstValue`] mapping with its
    /// fail-closed diagnostics. Const *references* in initializers resolve via
    /// the `ConstEnv`, populated only with same-module integer consts already
    /// folded earlier in source order.
    #[allow(
        clippy::too_many_lines,
        reason = "the explicit literal and shared-evaluator error mapping keeps every const failure class fail-closed"
    )]
    pub(super) fn fold_const_expr(
        &mut self,
        expr: &Expr,
        declared_ty: &ResolvedTy,
        span: std::ops::Range<usize>,
    ) -> HirConstValue {
        if let Expr::ByteStringLiteral(bytes) | Expr::ByteArrayLiteral(bytes) = expr {
            return HirConstValue::Bytes(bytes.clone());
        }
        // Dispatch on the initializer shape rather than the declared type:
        // the string primitive spells out as either `ResolvedTy::String` or
        // `ResolvedTy::Named { name: "String" }` depending on annotation form,
        // and the checker has already proven value/type agreement upstream.
        if let Expr::Literal(lit) = expr {
            match lit {
                Literal::String(s) => return HirConstValue::String(s.clone()),
                Literal::Float(value) if Self::is_float_ty(declared_ty) => {
                    if Self::float_literal_fits_declared_ty(*value, declared_ty) {
                        return HirConstValue::Float(*value);
                    }
                    self.unsupported(
                        span,
                        "float const initializer exceeds the declared float range",
                        "const-fold",
                    );
                    return HirConstValue::Float(0.0);
                }
                _ => {}
            }
        }

        if let Expr::Unary {
            op: UnaryOp::Negate,
            operand,
        } = expr
        {
            if let Expr::Literal(Literal::Float(value)) = &operand.0 {
                if Self::is_float_ty(declared_ty) {
                    let negated = -*value;
                    if Self::float_literal_fits_declared_ty(negated, declared_ty) {
                        return HirConstValue::Float(negated);
                    }
                    self.unsupported(
                        span,
                        "negative float const initializer exceeds the declared float range",
                        "const-fold",
                    );
                    return HirConstValue::Float(0.0);
                }
            }
        }

        // Delegate integer/arithmetic evaluation to the sanctioned engine.
        // `decl.value` is a `Spanned<Expr>`, but `fold_const_expr` is handed the
        // bare `Expr`; reconstruct the `Spanned` shape the engine expects. The
        // `UnknownConst` is mapped to a fail-closed diagnostic below for
        // forward references and unsupported const value shapes.
        let spanned: hew_parser::ast::Spanned<Expr> = (expr.clone(), span.clone());
        let env = self.const_eval_env_from_folded_integer_consts();
        let target_width = if self.target_arch == TargetArch::Wasm32 {
            32
        } else {
            64
        };
        let target = hew_types::check::const_eval::ConstIntegerTarget::from_resolved_ty(
            declared_ty,
            target_width,
        );
        match target.map(|target| {
            hew_types::check::const_eval::eval_integer_const_expr(&spanned, &env, target)
        }) {
            // The const evaluator and the HIR carrier are both `i128`, and the
            // evaluator has already range-checked the value against the
            // declared type, so the folded value passes through exactly.
            Some(Ok(value)) => return HirConstValue::Integer(value),
            Some(Err(hew_types::check::const_eval::ConstEvalError::UnknownConst(_))) => {
                self.unsupported(
                    span,
                    "const references in initializers are not yet supported",
                    "const-fold",
                );
            }
            Some(Err(
                hew_types::check::const_eval::ConstEvalError::ArithmeticOverflow
                | hew_types::check::const_eval::ConstEvalError::Overflow,
            )) => {
                self.const_integer_evaluation_error(
                    span,
                    "arithmetic-overflow",
                    "constant initializer arithmetic overflows its declared integer type",
                );
            }
            Some(Err(hew_types::check::const_eval::ConstEvalError::DivisionByZero)) => {
                self.const_integer_evaluation_error(
                    span,
                    "division-by-zero",
                    "constant initializer divides by zero",
                );
            }
            Some(Err(hew_types::check::const_eval::ConstEvalError::OutOfRange)) => {
                self.const_integer_evaluation_error(
                    span,
                    "out-of-range",
                    "constant initializer value does not fit in its declared integer type",
                );
            }
            Some(Err(hew_types::check::const_eval::ConstEvalError::NotConstant)) | None => {
                self.unsupported(
                    span,
                    "unsupported const initializer expression",
                    "const-fold",
                );
            }
        }

        // Fail-closed placeholder. A diagnostic was already emitted, so
        // compilation will not proceed; the placeholder only needs to match
        // the declared type's value shape so downstream structural invariants
        // hold.
        if Self::is_string_ty(declared_ty) {
            HirConstValue::String(String::new())
        } else if Self::is_float_ty(declared_ty) {
            HirConstValue::Float(0.0)
        } else {
            HirConstValue::Integer(0)
        }
    }

    pub(super) fn const_integer_evaluation_error(
        &mut self,
        span: std::ops::Range<usize>,
        class: &str,
        note: &str,
    ) {
        self.diagnostics.push(HirDiagnostic::new(
            HirDiagnosticKind::ConstIntegerEvaluation {
                class: class.to_string(),
            },
            span,
            note,
        ));
    }

    pub(super) fn const_eval_env_from_folded_integer_consts(
        &self,
    ) -> hew_types::check::const_eval::ConstEnv {
        let mut env = hew_types::check::const_eval::ConstEnv::new();
        for (name, value) in &self.folded_integer_consts {
            env.insert(name.clone(), *value);
        }
        env
    }

    /// True for the string primitive in either of its resolved spellings:
    /// the bare `ResolvedTy::String` or the `Named { name: "String" }` builtin
    /// form produced when the type is written as `String` in an annotation.
    pub(super) fn is_string_ty(ty: &ResolvedTy) -> bool {
        matches!(ty, ResolvedTy::String)
            || matches!(ty, ResolvedTy::Named { head, .. } if head.registry_key() == "String")
    }

    pub(super) fn is_float_ty(ty: &ResolvedTy) -> bool {
        matches!(ty, ResolvedTy::F32 | ResolvedTy::F64)
    }

    pub(super) fn float_literal_fits_declared_ty(value: f64, ty: &ResolvedTy) -> bool {
        match ty {
            ResolvedTy::F32 => {
                if value.is_infinite() || value.is_nan() {
                    return true;
                }
                let abs = value.abs();
                abs == 0.0 || (abs >= f64::from(f32::MIN_POSITIVE) && abs <= f64::from(f32::MAX))
            }
            ResolvedTy::F64 => true,
            _ => false,
        }
    }
}
