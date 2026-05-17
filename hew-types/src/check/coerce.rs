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
            // Allow concrete type → dyn Trait coercion when the type implements all traits.
            // Records a `DynCoercion` side-table entry for the downstream MIR and LLVM
            // vtable emitters, and rejects non-object-safe traits at the coercion site
            // with `E_TRAIT_NOT_OBJECT_SAFE` (v0.5 predicate: no generic methods, no
            // `Self`-returning methods).
            if let Ty::TraitObject { traits } = &expected_resolved {
                let defaulted_concrete = actual_resolved.materialize_literal_defaults();
                if let Some(type_name) = concrete_type_name_for_dyn(&defaulted_concrete) {
                    if self.try_record_dyn_trait_coercion(
                        traits,
                        &type_name,
                        &defaulted_concrete,
                        span,
                    ) {
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

    /// Validate object safety for a single trait used in `dyn` position, and
    /// build the method-table entries for the (`trait`, `concrete`) pair.
    ///
    /// Returns `Some(methods)` when the trait is object-safe AND the concrete
    /// type implements it (either via a nominal `impl Trait for T` recorded
    /// in `trait_impls_set` / `primitive_trait_impls`, or via the structural
    /// `type_structurally_satisfies` path). Returns `None` when the trait is
    /// not registered, when the concrete type does not implement it, or when
    /// the trait is not object-safe — in which case a diagnostic is reported.
    ///
    /// `concrete_type_name` is the type-name spelling expected by the
    /// impl registries (e.g. `"i32"` for `Ty::I32` via
    /// `Ty::canonical_lowering_name`, `"MyStruct"` for user `Ty::Named`).
    ///
    /// The returned `method_table` entries are `(method_name, impl_fn_key)`:
    /// * `impl_fn_key = "<concrete_type_name>::<method_name>"` for user types
    ///   (matches `fn_sigs` qualified key).
    /// * `impl_fn_key = "<canonical>::<method_name>"` for primitive /
    ///   builtin-generic receivers; the impl `FnSig` lives in
    ///   `primitive_trait_impls`.
    fn validate_dyn_trait_bound(
        &mut self,
        trait_name: &str,
        concrete_type_name: &str,
        span: &Span,
    ) -> Option<Vec<(String, String)>> {
        // Resolve the trait declaration; an unregistered trait can never be
        // object-safe (and the caller's type-implements check would already
        // have rejected it).
        let trait_info = self.trait_defs.get(trait_name).cloned()?;

        // Object-safety predicate (v0.5):
        //   1. No generic methods.
        //   2. No `Self`-returning methods.
        // Both are rejected with `E_TRAIT_NOT_OBJECT_SAFE`.
        for method in &trait_info.methods {
            if method.type_params.as_ref().is_some_and(|tp| !tp.is_empty()) {
                self.report_error(
                    TypeErrorKind::TraitNotObjectSafe {
                        trait_name: trait_name.to_string(),
                        method_name: method.name.clone(),
                        reason: "generic method",
                    },
                    span,
                    format!(
                        "trait `{}` is not object-safe: method `{}` has generic parameters; \
                         remove the type parameters or avoid `dyn {}` here",
                        trait_name, method.name, trait_name
                    ),
                );
                return None;
            }
            if let Some(ret) = method.return_type.as_ref() {
                if type_expr_mentions_self(&ret.0) {
                    self.report_error(
                        TypeErrorKind::TraitNotObjectSafe {
                            trait_name: trait_name.to_string(),
                            method_name: method.name.clone(),
                            reason: "Self-returning method",
                        },
                        span,
                        format!(
                            "trait `{}` is not object-safe: method `{}` returns `Self`; \
                             v0.5 dyn-dispatch cannot recover the concrete type",
                            trait_name, method.name
                        ),
                    );
                    return None;
                }
            }
        }

        // Build the method-table. Prefer the nominal impl registries; fall
        // back to the structural match path so bare `impl T { fn ... }` that
        // structurally satisfies a trait also gets a populated table.
        let nominal_impl = self
            .trait_impls_set
            .contains(&(concrete_type_name.to_string(), trait_name.to_string()));

        let primitive_impl_methods = self
            .primitive_trait_impls
            .get(&(concrete_type_name.to_string(), trait_name.to_string()))
            .cloned();

        let structural_ok = !nominal_impl
            && primitive_impl_methods.is_none()
            && self.type_structurally_satisfies(concrete_type_name, trait_name);

        if !nominal_impl && primitive_impl_methods.is_none() && !structural_ok {
            return None;
        }

        let mut table: Vec<(String, String)> = Vec::with_capacity(trait_info.methods.len());
        for method in &trait_info.methods {
            let impl_fn_key = format!("{concrete_type_name}::{}", method.name);
            table.push((method.name.clone(), impl_fn_key));
        }
        Some(table)
    }

    /// Validate and record a `T → dyn Trait` coercion at `span`.  Walks every
    /// trait bound in the target trait-object type; if all bounds are
    /// satisfied and object-safe, inserts the (possibly multi-bound)
    /// [`DynCoercion`] into `dyn_trait_coercions` and returns `true`.
    ///
    /// Returns `false` when any bound is unsatisfied (caller falls through to
    /// the regular type-mismatch diagnostic) or when any bound failed
    /// object-safety (the diagnostic has already been emitted; returning
    /// `true` here would mask the error, returning `false` falls through to
    /// the mismatch diagnostic — the caller's outer `if expected != Error
    /// ...` guard suppresses the second emission when warranted).
    fn try_record_dyn_trait_coercion(
        &mut self,
        traits: &[crate::ty::TraitObjectBound],
        type_name: &str,
        concrete_type: &Ty,
        span: &Span,
    ) -> bool {
        // Collect per-bound method tables. Bail (false) if any bound is
        // unsatisfied or not object-safe.
        let mut per_bound: Vec<(String, Vec<(String, String)>)> = Vec::with_capacity(traits.len());
        for bound in traits {
            let Some(methods) = self.validate_dyn_trait_bound(&bound.trait_name, type_name, span)
            else {
                return false;
            };
            per_bound.push((bound.trait_name.clone(), methods));
        }

        // Composite trait_name: `A` for single-bound, `A+B` for multi-bound.
        let composite_trait_name = if per_bound.len() == 1 {
            per_bound[0].0.clone()
        } else {
            per_bound
                .iter()
                .map(|(name, _)| name.as_str())
                .collect::<Vec<_>>()
                .join("+")
        };

        // Flatten method tables. For multi-bound, prefix each method name with
        // its originating trait so downstream consumers can route correctly.
        let multi = per_bound.len() > 1;
        let mut method_table: Vec<(String, String)> = Vec::new();
        for (trait_name, methods) in &per_bound {
            for (method_name, impl_fn_key) in methods {
                let qualified = if multi {
                    format!("{trait_name}::{method_name}")
                } else {
                    method_name.clone()
                };
                method_table.push((qualified, impl_fn_key.clone()));
            }
        }

        self.dyn_trait_coercions.insert(
            SpanKey::from(span),
            DynCoercion {
                trait_name: composite_trait_name,
                concrete_type: concrete_type.clone(),
                method_table,
            },
        );
        true
    }
}

/// Map a resolved concrete `Ty` to the type-name string used by the impl
/// registries (`trait_impls_set`, `primitive_trait_impls`). Returns `None`
/// for types that cannot appear as the receiver of a trait impl visible to
/// the dyn-coercion path (function types, tuples, unresolved variables).
fn concrete_type_name_for_dyn(ty: &Ty) -> Option<String> {
    // Default unresolved numeric literals (`<int literal>`/`<float literal>`)
    // to their concrete kinds so a coercion site sees the same key the impl
    // registries use (e.g. `42 → dyn Display` looks up `("i64", "Display")`).
    let defaulted = ty.materialize_literal_defaults();
    if let Some(canonical) = defaulted.canonical_lowering_name() {
        return Some(canonical.to_string());
    }
    if let Ty::Named { name, .. } = &defaulted {
        return Some(name.clone());
    }
    None
}

/// Conservative AST predicate: does this type expression mention `Self`
/// anywhere? Used by the object-safety check to reject `Self`-returning
/// trait methods at `dyn Trait` coercion sites.
fn type_expr_mentions_self(expr: &TypeExpr) -> bool {
    match expr {
        TypeExpr::Named { name, type_args } => {
            if name == "Self" {
                return true;
            }
            type_args
                .as_ref()
                .is_some_and(|args| args.iter().any(|a| type_expr_mentions_self(&a.0)))
        }
        TypeExpr::Result { ok, err } => {
            type_expr_mentions_self(&ok.0) || type_expr_mentions_self(&err.0)
        }
        TypeExpr::Option(inner) | TypeExpr::Slice(inner) => type_expr_mentions_self(&inner.0),
        TypeExpr::Tuple(elems) => elems.iter().any(|e| type_expr_mentions_self(&e.0)),
        TypeExpr::Array { element, .. } => type_expr_mentions_self(&element.0),
        TypeExpr::Function {
            params,
            return_type,
        } => {
            params.iter().any(|p| type_expr_mentions_self(&p.0))
                || type_expr_mentions_self(&return_type.0)
        }
        TypeExpr::Pointer { pointee, .. } => type_expr_mentions_self(&pointee.0),
        TypeExpr::TraitObject(bounds) => bounds.iter().any(|b| {
            b.type_args
                .as_ref()
                .is_some_and(|args| args.iter().any(|a| type_expr_mentions_self(&a.0)))
        }),
        TypeExpr::Infer => false,
    }
}
