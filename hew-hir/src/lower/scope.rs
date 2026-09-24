//! Binding scopes and name lookup.

use super::*;

impl LowerCtx {
    pub(super) fn pattern_name(&mut self, pattern: &Spanned<Pattern>) -> Option<String> {
        if let Pattern::Identifier(name) = &pattern.0 {
            Some(name.to_string())
        } else {
            self.unsupported(pattern.1.clone(), "pattern", "slice-2");
            None
        }
    }

    pub(super) fn bind(
        &mut self,
        name: String,
        ty: ResolvedTy,
        mutable: bool,
        span: std::ops::Range<usize>,
    ) -> HirBinding {
        let id = self.ids.binding();
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.clone(), (id, ty.clone(), span.clone()));
        }
        HirBinding {
            id,
            name,
            ty,
            mutable,
            span,
            is_consume: false,
        }
    }

    /// Lower an AST function value parameter to its `HirBinding`, carrying the
    /// `consume` modifier (`param.is_consume`) onto the binding so the
    /// param-ownership classifier can pin its by-move disposition. Mirrors
    /// `bind` for the type/mutability/scope-registration mechanics; the only
    /// addition is propagating the consume annotation, which `bind` (shared by
    /// every non-param binder) always leaves `false`.
    pub(super) fn bind_param(&mut self, param: &Param) -> HirBinding {
        let ty = self.lower_type(&param.ty);
        let mut binding = self.bind(
            param.name.to_string(),
            ty,
            param.is_mutable,
            param.ty.1.clone(),
        );
        binding.is_consume = param.is_consume;
        binding
    }

    pub(super) fn bind_actor_param(&mut self, param: &Param) -> HirBinding {
        let ty = self.lower_type(&param.ty);
        let ty = self.qualify_current_module_record_ty(ty);
        let mut binding = self.bind(
            param.name.to_string(),
            ty,
            param.is_mutable,
            param.ty.1.clone(),
        );
        binding.is_consume = param.is_consume;
        binding
    }

    /// Register a pre-allocated `BindingId` in the current scope under `name`.
    ///
    /// Used when the caller needs the `BindingId` before the scope is pushed
    /// (e.g. `HirMatchArmPredicate::Binding` where the id is embedded in the
    /// predicate and must be available before the guard expression is lowered).
    pub(super) fn bind_existing(
        &mut self,
        id: BindingId,
        name: String,
        ty: ResolvedTy,
        _mutable: bool,
        span: std::ops::Range<usize>,
    ) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, (id, ty, span));
        }
    }

    pub(super) fn lookup(&self, name: &str) -> Option<(BindingId, ResolvedTy)> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get(name).map(|(id, ty, _)| (*id, ty.clone())))
    }

    /// Look up a tagged-union constructor using canonical owner identity.
    ///
    /// Imported declarations are registered only under module-qualified keys.
    /// The checker-selected result/scrutinee type therefore wins over surface
    /// spelling, then import aliases and the module currently being lowered are
    /// considered. Direct lookup is last and is intentionally limited by the
    /// registry producer to root-local and builtin short keys. This prevents
    /// two imported modules that both declare `Shape::Box` from overwriting one
    /// another through a process-global short key.
    pub(super) fn lookup_variant_ctor(
        &self,
        name: &str,
        owner_ty: Option<&ResolvedTy>,
    ) -> Option<(String, usize, &HirVariantKind)> {
        let variant_name = name
            .rsplit_once("::")
            .or_else(|| name.rsplit_once('.'))
            .map_or(name, |(_, variant)| variant);
        let mut candidates = Vec::with_capacity(5);
        if let Some(ResolvedTy::Named { name: owner, .. }) = owner_ty {
            candidates.push(format!("{owner}::{variant_name}"));
            if !owner.contains('.') {
                if let Some(module) = self.current_module_name.as_deref() {
                    candidates.push(format!("{module}.{owner}::{variant_name}"));
                }
            }
        }
        if let Some((prefix, variant)) = name.rsplit_once("::") {
            if let Some(canonical_prefix) = self.import_type_name_aliases.get(&(
                self.current_module_name.clone(),
                self.current_module_idx,
                prefix.to_string(),
            )) {
                candidates.push(format!("{canonical_prefix}::{variant}"));
            }
            if let Some(module) = self.current_module_name.as_deref() {
                candidates.push(format!("{module}.{name}"));
            }
        }
        // A checker-proven owner must not be replaced by an unrelated bare
        // constructor, such as NodeError.Config for a user record Config.
        if !matches!(owner_ty, Some(ResolvedTy::Named { .. })) {
            candidates.push(name.to_string());
        }

        for candidate in candidates {
            let Some((type_name, idx)) = self.machine_ctor_registry.get(&candidate) else {
                continue;
            };
            let Some(variants) = self.enum_variants_by_name.get(type_name) else {
                continue;
            };
            let Some(variant) = variants.get(*idx) else {
                continue;
            };
            return Some((type_name.clone(), *idx, &variant.kind));
        }
        None
    }

    /// Instantiate the declaration's payload types using the checked enum
    /// owner. Literal syntax must not supply a replacement type for a generic
    /// field; the same substitution also owns the enum's concrete layout.
    pub(super) fn instantiated_pattern_payload_types(
        &self,
        name: &str,
        owner_ty: &ResolvedTy,
        arity: usize,
    ) -> Result<Vec<ResolvedTy>, String> {
        let (owner, _, kind) = self
            .lookup_variant_ctor(name, Some(owner_ty))
            .ok_or_else(|| format!("missing checked variant constructor for {owner_ty:?}"))?;
        let fields = match kind {
            HirVariantKind::Tuple(fields) => fields.as_slice(),
            HirVariantKind::Unit => &[],
            HirVariantKind::Struct(_) => {
                return Err("tuple variant pattern has a record declaration".into());
            }
        };
        let ResolvedTy::Named { args, .. } = owner_ty else {
            return Err("variant pattern has a non-nominal owner".into());
        };
        let params = self
            .enum_type_params
            .get(&owner)
            .map_or(&[][..], Vec::as_slice);
        if params.len() != args.len() || fields.len() != arity {
            return Err(
                "variant pattern disagrees with its checked generic or payload arity".into(),
            );
        }
        Ok(fields
            .iter()
            .map(|ty| substitute_type_params(ty, params, args))
            .collect())
    }

    pub(super) fn resolved_option_inner(ty: &ResolvedTy) -> Option<&ResolvedTy> {
        match ty {
            ResolvedTy::Named {
                args,
                builtin: Some(BuiltinType::Option),
                ..
            } if args.len() == 1 => Some(&args[0]),
            _ => None,
        }
    }

    pub(super) fn resolved_result_parts(ty: &ResolvedTy) -> Option<(&ResolvedTy, &ResolvedTy)> {
        match ty {
            ResolvedTy::Named {
                args,
                builtin: Some(BuiltinType::Result),
                ..
            } if args.len() == 2 => Some((&args[0], &args[1])),
            _ => None,
        }
    }

    pub(super) fn checker_expr_resolved_ty(
        &mut self,
        span: &std::ops::Range<usize>,
        name: &str,
    ) -> Option<ResolvedTy> {
        let checker_key = self.mk_key(span);
        let Some(checker_ty) = self.expr_types.get(&checker_key).cloned() else {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::CheckerBoundaryViolation {
                    name: name.to_string(),
                    reason: "missing expr_types entry".to_string(),
                },
                span.clone(),
                "checker-authoritative expression type is required for `?` lowering",
            ));
            return None;
        };
        match ResolvedTy::from_ty(&checker_ty) {
            // `Ty::Named` does not carry source-declaration opacity. Route
            // checker-authored expression types through the same identity
            // normalisation funnel as other checker→HIR boundaries so an
            // opaque Result/Option payload remains pointer-shaped, including
            // when nested inside another generic carrier.
            Ok(resolved) => Some(self.qualify_current_module_record_ty(resolved)),
            Err(err) => {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::CheckerBoundaryViolation {
                        name: name.to_string(),
                        reason: err.to_string(),
                    },
                    span.clone(),
                    "checker-authoritative expression type failed boundary conversion",
                ));
                None
            }
        }
    }

    pub(super) fn builtin_variant_predicate(
        &mut self,
        builtin: BuiltinType,
        variant_name: &str,
        span: &std::ops::Range<usize>,
    ) -> Option<(HirMatchArmPredicate, usize)> {
        let type_name = builtin.canonical_name();
        let qualified = format!("{type_name}::{variant_name}");
        let Some((registered_type, variant_idx)) =
            self.machine_ctor_registry.get(&qualified).cloned()
        else {
            self.unsupported(
                span.clone(),
                format!("`?` builtin variant `{qualified}` missing from ctor registry"),
                "question-operator",
            );
            return None;
        };
        if registered_type != type_name {
            self.unsupported(
                span.clone(),
                format!(
                    "`?` builtin variant `{qualified}` resolved to unexpected type `{registered_type}`"
                ),
                "question-operator",
            );
            return None;
        }
        let Ok(variant_idx_u32) = u32::try_from(variant_idx) else {
            self.unsupported(
                span.clone(),
                format!("`?` builtin variant `{qualified}` index exceeds u32::MAX"),
                "question-operator",
            );
            return None;
        };
        Some((
            HirMatchArmPredicate::EnumVariant {
                variant_match: hew_types::VariantMatch {
                    type_name: type_name.to_string(),
                    variant_name: variant_name.to_string(),
                },
                variant_idx: variant_idx_u32,
            },
            variant_idx,
        ))
    }

    pub(super) fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub(super) fn pop_scope(&mut self) {
        self.scopes.pop();
    }
}
