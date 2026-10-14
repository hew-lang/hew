//! Quantified callable binders and their ordinary instantiated call signatures.

use super::{
    scoped_module_item_name, Checker, FnSig, HashMap, ParamBounds, ResolvedTy, Span, SpanKey,
    Symbol, TraitRef, Ty, TypeError, TypeErrorKind, TypeParam,
};

impl Checker {
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
                self.current_function.clone(),
                owner,
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
        let occurrence =
            crate::DeclarationOccurrence::new_with_synthetic_ordinal(module, span, 0, kind, 0);
        let name = format!("<callable@{}:{}>", span.start, span.end);
        let path = scoped_module_item_name(self.canonical_fn_owner(), &name).unwrap_or(name);
        let owner = self
            .defs
            .declare(occurrence, Symbol::intern("callable"), None, path)
            .expect("one callable occurrence has one declaration identity");
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
            Some(destination),
            implementation,
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
