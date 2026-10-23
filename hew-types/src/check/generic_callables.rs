//! Quantified callable binders and their ordinary instantiated call signatures.

use super::{
    scoped_module_item_name, Checker, FnSig, HashMap, ParamBounds, ResolvedTy, Span, SpanKey,
    Symbol, TraitRef, Ty, TypeError, TypeErrorKind, TypeParam,
};

impl Checker {
    /// Generalize only a directly bound literal. Free environment variables
    /// remain shared monotypes; calls instantiate only the new local binders.
    pub(super) fn generalize_bound_closure(
        &mut self,
        value: &super::Expr,
        span: &Span,
        ty: Ty,
    ) -> Ty {
        let super::Expr::Lambda {
            type_params: None,
            body,
            ..
        } = value
        else {
            return ty;
        };
        let resolved = self.subst.resolve(&ty);
        let Ty::Closure { params, ret, .. } = &resolved else {
            return ty;
        };
        let mut variables = super::HashSet::new();
        for ty in params.iter().chain(std::iter::once(ret.as_ref())) {
            super::collect_unresolved_inference_vars(ty, &mut variables);
        }
        for binding in self.env.visible_bindings() {
            let mut free = super::HashSet::new();
            super::collect_unresolved_inference_vars(&self.subst.resolve(&binding.ty), &mut free);
            variables.retain(|variable| !free.contains(variable));
        }
        if variables.is_empty() {
            return ty;
        }
        let mut uses = GeneralizationUses {
            checker: self,
            variables: &variables,
            ambiguous: Vec::new(),
        };
        super::lints::walk_expr(&body.0, &body.1, &mut uses);
        for span in uses.ambiguous {
            self.report_error_with_suggestions(
                TypeErrorKind::InferenceFailed,
                &span,
                "this operation does not determine a unique generic parameter type".to_string(),
                vec!["annotate the closure parameter with the intended concrete type".to_string()],
            );
        }
        let mut variables: Vec<_> = variables.into_iter().collect();
        variables.sort_unstable_by_key(|variable| variable.0);
        let written: Vec<_> = variables
            .iter()
            .enumerate()
            .map(|(index, _)| TypeParam {
                name: hew_parser::ast::Ident::new(&format!("T{index}")),
                bounds: Vec::new(),
            })
            .collect();
        let parameters =
            self.declare_callable_parameters(&written, span, crate::DeclarationKind::Closure);
        for (variable, parameter) in variables.iter().zip(&parameters) {
            self.subst
                .insert(*variable, &Ty::param(*parameter))
                .expect("a fresh closure binder cannot form an inference cycle");
        }
        let owner = parameters[0].id.owner;
        let bounds = self.inferred_callable_bounds(owner);
        for check in &mut self.deferred_bound_checks {
            let resolved = self.subst.resolve(&check.type_arg);
            if Self::ty_mentions_type_params(&resolved, &parameters) {
                check.scope_bounds.extend(&bounds);
            }
        }
        let callable = self.subst.resolve(&resolved);
        let (_, params, ret) = callable
            .callable_signature()
            .expect("literal remains a callable");
        let signature = self
            .fn_sigs
            .get_mut(&owner)
            .expect("literal owns a registered signature");
        signature.type_params.clone_from(&parameters);
        signature.bounds.clone_from(&bounds);
        signature.params = params.to_vec();
        signature.return_type = ret.clone();
        self.promote_inferred_callable_dependencies(owner, &parameters);
        let scheme = Ty::GenericCallable {
            parameters: self.callable_type_parameters(&parameters, &bounds),
            callable: Box::new(callable),
        };
        self.record_type(span, &scheme);
        scheme
    }

    fn inferred_callable_bounds(&self, owner: crate::DefId) -> ParamBounds {
        let mut bounds = ParamBounds::default();
        // A declaration's explicit trait contract is a unique obligation, not
        // an overload choice. Preserve it on the inferred callable signature.
        for check in &self.deferred_bound_checks {
            let argument = self.subst.resolve(&check.type_arg);
            let Some(parameter) =
                Self::bare_param(&argument).filter(|parameter| parameter.id.owner == owner)
            else {
                continue;
            };
            for bound in &check.bounds {
                bounds.push(
                    parameter.id,
                    TraitRef {
                        trait_id: bound.trait_id,
                        args: bound.args.iter().map(|ty| self.subst.resolve(ty)).collect(),
                        assoc: bound
                            .assoc
                            .iter()
                            .map(|(name, ty)| (*name, self.subst.resolve(ty)))
                            .collect(),
                    },
                );
            }
        }
        for requirement in self.generic_requirements.values().flatten() {
            let predicate = match requirement.kind {
                super::types::GenericRequirementKind::Eq => crate::Predicate::Eq,
                super::types::GenericRequirementKind::CaptureSnapshot { .. } => {
                    crate::Predicate::Clone
                }
                _ => continue,
            };
            let resolved = self.subst.resolve(&requirement.ty);
            if let Some(parameter) =
                Self::bare_param(&resolved).filter(|parameter| parameter.id.owner == owner)
            {
                bounds.push(
                    parameter.id,
                    TraitRef::bare(crate::DefTable::predicate(predicate)),
                );
            }
        }
        bounds
    }

    /// Re-publish demands that were recorded before inference named their
    /// binder. Nested closures use the same owner-selection rules as written
    /// generic bodies; no demand is stranded under a monomorphic inner body.
    fn promote_inferred_callable_dependencies(
        &mut self,
        owner: crate::DefId,
        parameters: &[crate::ParamHead],
    ) {
        let mut affected = Vec::new();
        for (declaration, requirements) in &mut self.generic_requirements {
            let mut retained = Vec::new();
            for mut requirement in std::mem::take(requirements) {
                let resolved = self.subst.resolve(&requirement.ty);
                if Self::ty_mentions_type_params(&resolved, parameters) {
                    requirement.ty = resolved;
                    affected.push((*declaration, requirement));
                } else {
                    if *declaration == Some(owner) {
                        requirement.owner_type_params = parameters.to_vec();
                    }
                    retained.push(requirement);
                }
            }
            *requirements = retained;
        }
        let previous = self.current_function.clone();
        for (declaration, requirement) in affected {
            self.current_function = declaration.map(|id| self.defs.path(id).to_string());
            self.record_generic_requirement(requirement.kind, &requirement.ty, &requirement.span);
        }
        self.current_function = previous;
        for site in std::mem::take(&mut self.generic_fn_instantiation_sites) {
            let substitution: HashMap<_, _> = site
                .substitution
                .iter()
                .map(|(parameter, ty)| (*parameter, self.subst.resolve(ty)))
                .collect();
            if site.caller == Some(owner)
                || substitution
                    .values()
                    .any(|ty| Self::ty_mentions_type_params(ty, parameters))
            {
                let caller = site.caller.map(|id| self.defs.path(id).to_string());
                self.record_generic_instantiation_site(
                    caller.as_deref(),
                    site.callee,
                    substitution,
                    &site.span,
                );
            } else {
                self.generic_fn_instantiation_sites.push(site);
            }
        }
    }

    pub(super) fn specialize_callable_value(
        &mut self,
        expected: &Ty,
        actual: &Ty,
        span: &Span,
    ) -> bool {
        if !matches!(expected, Ty::Function { .. } | Ty::Closure { .. }) {
            return false;
        }
        let Ty::GenericCallable { callable, .. } = actual else {
            return false;
        };
        let Some(sig) = Self::generic_callable_signature(actual) else {
            return false;
        };
        let (_, _, arguments) = self.instantiate_fn_sig_for_call(&sig, None, span);
        let substitution = sig
            .type_params
            .iter()
            .copied()
            .zip(arguments.iter().cloned())
            .collect();
        let instantiated = callable.substitute_type_params_parallel(&substitution);
        self.expect_type(expected, &instantiated, span);
        self.enforce_signature_bounds(&sig, &arguments, span);
        if let Some(owner) = self.generic_callable_owner(actual) {
            let substitution = sig
                .type_params
                .iter()
                .copied()
                .zip(
                    arguments
                        .iter()
                        .map(|argument| self.subst.resolve(argument)),
                )
                .collect();
            self.record_generic_instantiation_site(
                self.current_function.clone().as_deref(),
                self.fn_sig_keys[&owner],
                substitution,
                span,
            );
        }
        self.callable_value_specializations
            .insert(SpanKey::in_module(span, self.current_module_idx), arguments);
        true
    }

    pub(super) fn finalize_callable_value_specializations(
        &mut self,
    ) -> HashMap<SpanKey, Vec<ResolvedTy>> {
        let mut resolved = HashMap::new();
        for (key, arguments) in std::mem::take(&mut self.callable_value_specializations) {
            let arguments: Vec<_> = arguments
                .iter()
                .map(|ty| self.finalize_type_for_handoff(ty))
                .collect();
            if arguments.iter().any(Ty::contains_error) {
                continue;
            }
            if let Ok(arguments) = arguments
                .iter()
                .map(ResolvedTy::from_ty)
                .collect::<Result<Vec<_>, _>>()
            {
                resolved.insert(key, arguments);
            } else {
                let mut error = TypeError::new(
                    TypeErrorKind::InferenceFailed,
                    key.start..key.end,
                    "cannot infer the type arguments for this callable view".to_string(),
                );
                if let Some(module) = self.expr_type_source_modules.get(&key).cloned().flatten() {
                    error = error.with_source_module(module);
                }
                self.errors.push(error);
            }
        }
        resolved
    }

    pub(super) fn declare_callable_parameters(
        &mut self,
        written: &[TypeParam],
        span: &Span,
        kind: crate::DeclarationKind,
    ) -> Vec<crate::ParamHead> {
        if written.is_empty() {
            return Vec::new();
        }
        let module = self.current_declaration_module();
        let owner = self.declare_callable_owner(span, kind);
        if let Some(module) = module {
            self.scopes.declare_type_parameters(
                module,
                owner,
                span.clone(),
                written.iter().map(|parameter| parameter.name),
            );
        }
        written
            .iter()
            .enumerate()
            .map(|(index, parameter)| {
                crate::ParamHead::new(crate::TypeParamId::new(owner, index), parameter.name.name)
            })
            .collect()
    }

    pub(super) fn declare_callable_owner(
        &mut self,
        span: &Span,
        kind: crate::DeclarationKind,
    ) -> crate::DefId {
        let module = self.current_declaration_module();
        let occurrence =
            crate::DeclarationOccurrence::new_with_synthetic_ordinal(module, span, 0, kind, 0);
        let name = format!("<callable@{}:{}>", span.start, span.end);
        let path = scoped_module_item_name(self.canonical_fn_owner(), &name).unwrap_or(name);
        self.defs
            .declare(occurrence, Symbol::intern("callable"), None, path)
            .expect("one callable occurrence has one declaration identity")
    }

    /// Rename an expected universal signature to this literal's rigid binders.
    /// A written generic literal checked against a monomorphic destination is
    /// specialized only after its universally quantified body is checked.
    pub(super) fn contextual_closure_signature(
        expected: Option<&Ty>,
        parameters: &[crate::ParamHead],
    ) -> Option<FnSig> {
        let expected = expected?;
        if let Some(mut signature) = Self::generic_callable_signature(expected) {
            if signature.type_params.len() != parameters.len() {
                return None;
            }
            let rename = signature
                .type_params
                .iter()
                .copied()
                .zip(parameters.iter().copied().map(Ty::param))
                .collect();
            let mut bounds = ParamBounds::default();
            for (source, target) in signature.type_params.iter().zip(parameters) {
                for bound in signature.bounds.of(source.id) {
                    bounds.push(
                        target.id,
                        TraitRef {
                            trait_id: bound.trait_id,
                            args: bound
                                .args
                                .iter()
                                .map(|ty| ty.substitute_type_params_parallel(&rename))
                                .collect(),
                            assoc: bound
                                .assoc
                                .iter()
                                .map(|(name, ty)| {
                                    (*name, ty.substitute_type_params_parallel(&rename))
                                })
                                .collect(),
                        },
                    );
                }
            }
            signature.bounds = bounds;
            signature.params = signature
                .params
                .iter()
                .map(|ty| ty.substitute_type_params_parallel(&rename))
                .collect();
            signature.return_type = signature
                .return_type
                .substitute_type_params_parallel(&rename);
            signature.type_params = parameters.to_vec();
            Some(signature)
        } else if parameters.is_empty() {
            let (_, params, ret) = expected.callable_signature()?;
            Some(FnSig {
                params: params.to_vec(),
                return_type: ret.clone(),
                ..FnSig::default()
            })
        } else {
            None
        }
    }

    pub(super) fn callable_type_parameters(
        &self,
        parameters: &[crate::ParamHead],
        bounds: &ParamBounds,
    ) -> Vec<crate::CallableTypeParameter<crate::TraitObjectBound>> {
        parameters
            .iter()
            .map(|parameter| crate::CallableTypeParameter {
                parameter: *parameter,
                bounds: bounds
                    .of(parameter.id)
                    .map(|bound| crate::TraitObjectBound {
                        trait_name: self.defs.path(bound.trait_id).to_string(),
                        trait_id: Some(bound.trait_id),
                        args: bound.args.clone(),
                        assoc_bindings: bound
                            .assoc
                            .iter()
                            .map(|(name, ty)| (name.to_string(), ty.clone()))
                            .collect(),
                    })
                    .collect(),
            })
            .collect()
    }

    pub(super) fn generic_callable_owner(&self, ty: &Ty) -> Option<String> {
        let Ty::GenericCallable { parameters, .. } = ty else {
            return None;
        };
        let owner = parameters.first()?.parameter.id.owner;
        self.fn_sigs
            .contains_key(&owner)
            .then(|| self.defs.path(owner).to_string())
    }

    pub(super) fn record_generic_callable_coercion(
        &mut self,
        source: &Ty,
        target: &Ty,
        span: &Span,
    ) {
        let (Some(implementation), Some(destination)) = (
            self.generic_callable_owner(source),
            self.generic_callable_owner(target),
        ) else {
            return;
        };
        if implementation == destination {
            return;
        }
        let (
            Ty::GenericCallable {
                parameters: source, ..
            },
            Ty::GenericCallable {
                parameters: target, ..
            },
        ) = (source, target)
        else {
            return;
        };
        let substitution = source
            .iter()
            .zip(target)
            .map(|(source, target)| (source.parameter, Ty::param(target.parameter)))
            .collect();
        self.record_generic_instantiation_site(
            Some(&destination),
            self.fn_sig_keys[&implementation],
            substitution,
            span,
        );
    }

    pub(super) fn generic_callable_signature(ty: &Ty) -> Option<FnSig> {
        let Ty::GenericCallable {
            parameters,
            callable,
        } = ty
        else {
            return None;
        };
        let (_, params, ret) = callable.callable_signature()?;
        let mut bounds = ParamBounds::default();
        for parameter in parameters {
            for bound in &parameter.bounds {
                bounds.push(
                    parameter.parameter.id,
                    TraitRef {
                        trait_id: bound.trait_id?,
                        args: bound.args.clone(),
                        assoc: bound
                            .assoc_bindings
                            .iter()
                            .map(|(name, ty)| (Symbol::intern(name), ty.clone()))
                            .collect(),
                    },
                );
            }
        }
        Some(FnSig {
            type_params: parameters
                .iter()
                .map(|parameter| parameter.parameter)
                .collect(),
            bounds,
            params: params.to_vec(),
            return_type: ret.clone(),
            ..FnSig::default()
        })
    }
}

/// Operations whose open operand admits multiple unrelated interpretations
/// need a concrete context before a literal can be universally quantified.
/// This checks generalization eligibility; ordinary operator checking remains
/// the authority for whether a settled operand supports the operation.
struct GeneralizationUses<'a> {
    checker: &'a Checker,
    variables: &'a super::HashSet<crate::ty::TypeVar>,
    ambiguous: Vec<Span>,
}

impl super::lints::NodeVisitor for GeneralizationUses<'_> {
    fn visit_expr(&mut self, expression: &super::Expr, span: &Span) {
        use hew_parser::ast::{BinaryOp, Expr, UnaryOp};
        let operands: Vec<&super::Spanned<Expr>> = match expression {
            Expr::Binary { left, op, right }
                if !matches!(
                    op,
                    BinaryOp::Equal | BinaryOp::NotEqual | BinaryOp::And | BinaryOp::Or
                ) =>
            {
                vec![left, right]
            }
            Expr::Unary {
                op: UnaryOp::Negate | UnaryOp::BitNot,
                operand,
            } => vec![operand],
            Expr::Cast { expr, .. } => vec![expr],
            Expr::Is { lhs, rhs } => vec![lhs, rhs],
            _ => Vec::new(),
        };
        if operands.iter().any(|operand| {
            self.checker
                .expr_types
                .get(&SpanKey::in_module(
                    &operand.1,
                    self.checker.current_module_idx,
                ))
                .is_some_and(|ty| {
                    let ty = self.checker.subst.resolve(ty);
                    self.variables
                        .iter()
                        .any(|variable| ty.contains_var(*variable))
                })
        }) {
            self.ambiguous.push(span.clone());
        }
    }
}
