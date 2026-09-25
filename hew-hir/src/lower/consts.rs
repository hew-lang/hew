//! Constant declaration lowering and folding.

use super::*;

impl LowerCtx {
    /// First-pass registration of a module-level `const`. Allocates a stable
    /// `ItemId` and records the declared (checker-resolved) type so const
    /// references resolve regardless of source order. The folded value is
    /// produced later, canonically, in [`Self::lower_const`].
    pub(super) fn register_const_entry(&mut self, decl: &ConstDecl) {
        let id = self.ids.item();
        let ty = self.lower_type(&decl.ty);
        self.const_registry
            .insert(decl.name.to_string(), ConstEntry { id, ty });
    }

    /// Emit-pass lowering of a module-level `const NAME: T = <expr>;`.
    ///
    /// Reuses the `ItemId` allocated in the first pass and constant-folds the
    /// initializer into a [`HirConstValue`]. Fail-closed: any initializer the
    /// fold cannot evaluate to a concrete integer/string emits a
    /// `HirDiagnosticKind::NotYetImplemented` and lowers to a placeholder value
    /// (never silently fabricated semantics).
    pub(super) fn lower_const(
        &mut self,
        decl: &ConstDecl,
        span: std::ops::Range<usize>,
    ) -> Option<crate::node::HirConst> {
        // Reuse the stable ItemId + type pre-allocated during the first pass.
        let (id, ty) = match self.const_registry.get(decl.name.name.as_str()) {
            Some(entry) => (entry.id, entry.ty.clone()),
            None => (self.ids.item(), self.lower_type(&decl.ty)),
        };

        let value = self.fold_const_expr(&decl.value.0, &ty, span.clone());
        if let crate::node::HirConstValue::Integer(value) = &value {
            self.folded_integer_consts
                .insert(decl.name.to_string(), *value);
        }

        Some(crate::node::HirConst {
            id,
            node: self.ids.node(),
            declaration: self.source_declaration(&span, hew_types::DeclarationKind::Const, 0)?,
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
    ) -> crate::node::HirConstValue {
        // Dispatch on the initializer shape rather than the declared type:
        // the string primitive spells out as either `ResolvedTy::String` or
        // `ResolvedTy::Named { name: "String" }` depending on annotation form,
        // and the checker has already proven value/type agreement upstream.
        if let Expr::Literal(lit) = expr {
            match lit {
                Literal::String(s) => return crate::node::HirConstValue::String(s.clone()),
                Literal::Float(value) if Self::is_float_ty(declared_ty) => {
                    if Self::float_literal_fits_declared_ty(*value, declared_ty) {
                        return crate::node::HirConstValue::Float(*value);
                    }
                    self.unsupported(
                        span,
                        "float const initializer exceeds the declared float range",
                        "const-fold",
                    );
                    return crate::node::HirConstValue::Float(0.0);
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
                        return crate::node::HirConstValue::Float(negated);
                    }
                    self.unsupported(
                        span,
                        "negative float const initializer exceeds the declared float range",
                        "const-fold",
                    );
                    return crate::node::HirConstValue::Float(0.0);
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
            Some(Ok(value)) => return crate::node::HirConstValue::Integer(value),
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
            crate::node::HirConstValue::String(String::new())
        } else if Self::is_float_ty(declared_ty) {
            crate::node::HirConstValue::Float(0.0)
        } else {
            crate::node::HirConstValue::Integer(0)
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
