//! Constant declaration lowering and folding.

use super::*;
use hew_types::compile_time::{ConstantValue as HirConstValue, TypedConstant};

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
                    .map(|element| self.lower_constant_element(element, span))
                    .collect(),
            },
            HirConstValue::Array(elements) => HirExprKind::ArrayLiteral {
                elements: elements
                    .iter()
                    .map(|element| self.lower_constant_element(element, span))
                    .collect(),
            },
            HirConstValue::Vector(elements) => {
                let elements = elements
                    .iter()
                    .map(|element| self.lower_constant_element(element, span))
                    .collect();
                self.make_sequence_literal(elements, ty, span)
            }
            HirConstValue::Set(elements) => {
                let elements = elements
                    .iter()
                    .map(|element| self.lower_constant_element(element, span))
                    .collect();
                self.make_set_literal(elements, ty, span)
            }
            HirConstValue::Map(entries) => {
                let entries = entries
                    .iter()
                    .map(|(key, value)| {
                        (
                            self.lower_constant_element(key, span),
                            self.lower_constant_element(value, span),
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
                            (name.clone(), self.lower_constant_element(field, span))
                        })
                        .collect()
                }),
            },
            HirConstValue::Record(fields) => HirExprKind::StructInit {
                name: ty.user_facing().to_string(),
                type_args: Vec::new(),
                fields: fields
                    .iter()
                    .map(|(name, field)| (name.clone(), self.lower_constant_element(field, span)))
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

    fn lower_constant_element(&mut self, element: &TypedConstant, span: &Span) -> HirExpr {
        self.lower_constant_value(&element.value, &element.ty, span, IntentKind::Read)
    }

    /// First-pass registration of a module-level `const`. Allocates a stable
    /// `ItemId` and records the declared (checker-resolved) type so const
    /// references resolve regardless of source order. The folded value is
    /// produced on demand by declaration identity.
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
                initializer: decl.value.clone(),
                module_idx: self.current_module_idx,
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

        let value = self.fold_const_declaration(declaration)?.value;

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

    fn fold_const_declaration(&mut self, declaration: hew_types::DefId) -> Option<TypedConstant> {
        if let Some(value) = self.folded_constants.get(&declaration) {
            return Some(value.clone());
        }
        let entry = self.source_const_entries.get(&declaration)?.clone();
        if !self.folding_constants.insert(declaration) {
            let kind = HirDiagnosticKind::ConstDependencyCycle {
                name: self.defs.path(declaration).to_string(),
            };
            if !self
                .diagnostics
                .iter()
                .any(|diagnostic| diagnostic.kind == kind)
            {
                self.diagnostics.push(HirDiagnostic::new(
                    kind,
                    entry.initializer.1,
                    "cyclic constant initializer",
                ));
            }
            return None;
        }
        let previous_module = self.current_module_idx;
        self.current_module_idx = entry.module_idx;
        let value = self.fold_const_expr(&entry.initializer.0, &entry.ty, entry.initializer.1);
        self.current_module_idx = previous_module;
        self.folding_constants.remove(&declaration);
        let constant = TypedConstant {
            ty: entry.ty,
            value,
        };
        self.folded_constants.insert(declaration, constant.clone());
        Some(constant)
    }

    fn constant_at(&mut self, span: &Span) -> Option<HirConstValue> {
        let key = self.mk_key(span);
        if let Some(value) = self.compile_time_values.get(&key) {
            return Some(value.clone());
        }
        let Resolution::Def(declaration) = *self.resolutions.get(&key)? else {
            return None;
        };
        self.fold_const_declaration(declaration)
            .map(|constant| constant.value)
    }

    /// Fold checked literal structure; integer arithmetic uses the shared evaluator.
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
        if let Some(value) = self.constant_at(&span) {
            return value;
        }
        if self.diagnostics.iter().any(|diagnostic| {
            matches!(
                diagnostic.kind,
                HirDiagnosticKind::ConstDependencyCycle { .. }
            )
        }) {
            return Self::const_error_value(declared_ty);
        }
        if let Some(value) = self.fold_aggregate_const_expr(expr, declared_ty, &span) {
            return value;
        }
        let spanned = (expr.clone(), span.clone());
        let target_width = if self.target_arch == TargetArch::Wasm32 {
            32
        } else {
            64
        };
        let result = hew_types::check::const_eval::eval_scalar_const_expr(
            &spanned,
            declared_ty,
            target_width,
            &mut |expr| {
                let key = self.mk_key(&expr.1);
                let ty = self
                    .numeric_operand_coercions
                    .get(&key)
                    .and_then(|ty| ResolvedTy::from_ty(ty).ok())
                    .or_else(|| self.checker_expr_ty_if_present(&expr.1));
                let value = self.constant_at(&expr.1).or_else(|| {
                    ty.as_ref()
                        .and_then(|ty| self.fold_aggregate_const_expr(&expr.0, ty, &expr.1))
                });
                (ty, value)
            },
        );
        match result {
            Ok(value) => return value,
            Err(hew_types::check::const_eval::ConstEvalError::UnknownConst(_)) => {
                if !self.diagnostics.iter().any(|diagnostic| {
                    matches!(
                        diagnostic.kind,
                        HirDiagnosticKind::ConstDependencyCycle { .. }
                    )
                }) {
                    self.const_integer_evaluation_error(
                        span,
                        "unresolved-reference",
                        "constant reference could not be evaluated",
                    );
                }
            }
            Err(
                hew_types::check::const_eval::ConstEvalError::ArithmeticOverflow
                | hew_types::check::const_eval::ConstEvalError::Overflow,
            ) => self.const_integer_evaluation_error(
                span,
                "arithmetic-overflow",
                "constant initializer arithmetic overflows its declared type",
            ),
            Err(hew_types::check::const_eval::ConstEvalError::DivisionByZero) => self
                .const_integer_evaluation_error(
                    span,
                    "division-by-zero",
                    "constant initializer divides by zero",
                ),
            Err(hew_types::check::const_eval::ConstEvalError::OutOfRange) => self
                .const_integer_evaluation_error(
                    span,
                    "out-of-range",
                    "constant initializer value does not fit in its declared type",
                ),
            Err(hew_types::check::const_eval::ConstEvalError::NotConstant) => {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::CheckerBoundaryViolation {
                        name: "constant initializer".to_string(),
                        reason: "checked initializer has no constant evaluation plan".to_string(),
                    },
                    span,
                    "constant eligibility must be checked before HIR lowering",
                ));
            }
        }

        Self::const_error_value(declared_ty)
    }

    fn const_error_value(declared_ty: &ResolvedTy) -> HirConstValue {
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
}
