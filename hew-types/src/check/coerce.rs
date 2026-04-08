#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::*;

pub(super) fn can_implicitly_coerce_integer(actual: &Ty, expected: &Ty) -> bool {
    let Some(actual_info) = integer_type_info(actual) else {
        return false;
    };
    let Some(expected_info) = integer_type_info(expected) else {
        return false;
    };
    // Only allow widening: same sign and actual width fits in expected width
    actual_info.signed == expected_info.signed && actual_info.width <= expected_info.width
}

pub(super) fn common_integer_type(a: &Ty, b: &Ty) -> Option<Ty> {
    match (a, b) {
        (Ty::IntLiteral, Ty::IntLiteral) => return Some(Ty::IntLiteral),
        (Ty::IntLiteral, ty) | (ty, Ty::IntLiteral) if integer_type_info(ty).is_some() => {
            return Some(ty.clone());
        }
        _ => {}
    }
    let a_info = integer_type_info(a)?;
    let b_info = integer_type_info(b)?;
    if a_info.signed != b_info.signed {
        return None;
    }
    if a_info.width >= b_info.width {
        Some(a.clone())
    } else {
        Some(b.clone())
    }
}

pub(super) fn common_numeric_type(a: &Ty, b: &Ty) -> Option<Ty> {
    if a.is_float() && b.is_float() {
        if *a == Ty::F64 || *b == Ty::F64 {
            Some(Ty::F64)
        } else if *a == Ty::F32 || *b == Ty::F32 {
            Some(Ty::F32)
        } else {
            Some(Ty::FloatLiteral)
        }
    } else if a.is_float() && b.is_integer() {
        if a.is_float_literal() {
            Some(Ty::FloatLiteral)
        } else {
            Some(a.clone())
        }
    } else if a.is_integer() && b.is_float() {
        if b.is_float_literal() {
            Some(Ty::FloatLiteral)
        } else {
            Some(b.clone())
        }
    } else {
        common_integer_type(a, b)
    }
}

pub(super) fn cast_is_valid(actual: &Ty, target: &Ty) -> bool {
    (actual.is_numeric() && target.is_numeric())
        || (*actual == Ty::Bool && target.is_integer())
        || (actual.is_integer() && *target == Ty::Bool)
}

impl Checker {
    /// Check if an expression is a numeric literal or a reference to an untyped const
    /// with a known compile-time value (eligible for coercion).
    pub(super) fn is_coercible_numeric(&self, expr: &Expr) -> bool {
        is_integer_literal(expr)
            || is_float_literal(expr)
            || matches!(expr, Expr::Identifier(name) if self.const_values.contains_key(name))
    }

    /// Unify two branch types (if/else, if-let/else).
    ///
    /// - If one branch diverges (`Never`), returns the other branch's type.
    /// - If either branch is `Unit`, the expression evaluates to `Unit`.
    /// - Otherwise, unifies the two types and returns the then-branch type.
    pub(super) fn unify_branches(&mut self, then_ty: &Ty, else_ty: &Ty, span: &Span) -> Ty {
        if matches!(then_ty, Ty::Never) {
            return else_ty.clone();
        }
        if matches!(else_ty, Ty::Never) {
            return then_ty.clone();
        }
        if *then_ty == Ty::Unit || *else_ty == Ty::Unit {
            return Ty::Unit;
        }
        let then_resolved = self.subst.resolve(then_ty);
        let else_resolved = self.subst.resolve(else_ty);
        if then_resolved.is_numeric() && else_resolved.is_numeric() {
            if let Some(common_ty) = common_numeric_type(&then_resolved, &else_resolved) {
                return common_ty;
            }
        }
        self.expect_type(then_ty, else_ty, span);
        self.subst.resolve(then_ty)
    }

    pub(super) fn expect_type(&mut self, expected: &Ty, actual: &Ty, span: &Span) {
        // Snapshot substitution so partial bindings are rolled back on failure
        let snapshot = self.subst.snapshot();
        if let Err(_e) = unify(&mut self.subst, expected, actual) {
            // Restore substitution to avoid partial corruption
            self.subst.restore(snapshot);
            let expected_resolved = self.subst.resolve(expected);
            let actual_resolved = self.subst.resolve(actual);
            // Allow i32 where bool is expected (Hew uses i32 for truthiness)
            if expected_resolved == Ty::Bool && actual_resolved == Ty::I32 {
                return;
            }
            // Allow same-signed integer coercions (e.g. i32 <-> i64, u16 <-> u32).
            if can_implicitly_coerce_integer(&actual_resolved, &expected_resolved) {
                return;
            }
            // Allow handle types to coerce to/from string (both are !llvm.ptr at runtime)
            if let Ty::Named { name, .. } = &actual_resolved {
                if self.module_registry.is_handle_type(name) && expected_resolved == Ty::String {
                    return;
                }
            }
            if let Ty::Named { name, .. } = &expected_resolved {
                if self.module_registry.is_handle_type(name) && actual_resolved == Ty::String {
                    return;
                }
            }
            // Allow concrete type → dyn Trait coercion when the type implements all traits
            if let Ty::TraitObject { traits } = &expected_resolved {
                if let Ty::Named {
                    name: type_name, ..
                } = &actual_resolved
                {
                    // Check that the type implements all required traits
                    if traits
                        .iter()
                        .all(|bound| self.type_implements_trait(type_name, &bound.trait_name))
                    {
                        return;
                    }
                }
            }
            if expected_resolved.is_numeric() && actual_resolved.is_numeric() {
                self.report_error(
                    TypeErrorKind::Mismatch {
                        expected: expected_resolved.user_facing().to_string(),
                        actual: actual_resolved.user_facing().to_string(),
                    },
                    span,
                    format!(
                        "implicit numeric coercion from `{}` to `{}` is not allowed; use an explicit conversion",
                        actual_resolved.user_facing(),
                        expected_resolved.user_facing()
                    ),
                );
                return;
            }
            if expected_resolved != Ty::Error && actual_resolved != Ty::Error {
                self.report_error(
                    TypeErrorKind::Mismatch {
                        expected: expected_resolved.user_facing().to_string(),
                        actual: actual_resolved.user_facing().to_string(),
                    },
                    span,
                    format!(
                        "type mismatch: expected `{}`, found `{}`",
                        expected_resolved.user_facing(),
                        actual_resolved.user_facing()
                    ),
                );
            }
        }
    }
}
