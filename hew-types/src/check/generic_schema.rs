use super::{BTreeMap, CallTarget, Checker, HashMap, SpanKey, Symbol, Ty};

#[derive(Debug, Clone, Default)]
/// Implicit declaration-owned binders for associated types in generic bodies.
pub struct AssociatedTypeSchemas {
    parameters: BTreeMap<(crate::ParamHead, crate::DefId, Symbol), crate::ParamHead>,
}

impl AssociatedTypeSchemas {
    /// Project implicit associated binders through a method's checked receiver.
    #[must_use]
    pub fn instantiate_receiver_type(
        &self,
        ty: &Ty,
        obligations: &[super::ReceiverObligation],
        defs: &crate::DefTable,
    ) -> Ty {
        if let Ty::Named {
            head: crate::TypeHead::Param(parameter),
            args,
        } = ty
        {
            if args.is_empty() {
                for ((base, trait_id, associated), binder) in &self.parameters {
                    if parameter == binder {
                        if let Some(receiver) = obligations.iter().find(|item| item.param == *base)
                        {
                            return Ty::AssocType {
                                base: Box::new(receiver.arg.clone()),
                                trait_name: defs.path(*trait_id).into(),
                                assoc_name: associated.as_str().into(),
                            };
                        }
                    }
                }
            }
        }
        ty.map_children_pub(&|child| self.instantiate_receiver_type(child, obligations, defs))
    }

    /// Expand source binders into the body's checked substitution schema.
    #[must_use]
    pub fn binders(&self, parameters: &[crate::ParamHead]) -> Vec<crate::ParamHead> {
        let mut expanded = Vec::new();
        for parameter in parameters {
            if !expanded.contains(parameter) {
                expanded.push(*parameter);
            }
            for ((base, _, _), binder) in &self.parameters {
                if base == parameter && !expanded.contains(binder) {
                    expanded.push(*binder);
                }
            }
        }
        expanded
    }
}

impl Checker {
    pub(super) fn prepare_associated_schema_parameters(&mut self) {
        let mut projections = std::collections::BTreeSet::new();
        let saved_module = self.current_module.clone();
        for (ty, module) in self.annotation_types.values() {
            self.current_module.clone_from(module);
            self.collect_associated_schema_projections(
                &self.normalize_for_use(ty),
                &mut projections,
            );
        }
        for (site, ty) in &self.expr_types {
            self.current_module = self.expr_type_source_modules.get(site).cloned().flatten();
            self.collect_associated_schema_projections(
                &self.normalize_for_use(ty),
                &mut projections,
            );
        }
        self.current_module = saved_module;
        let mut next_indices = HashMap::new();
        for key @ (parameter, _, associated) in projections {
            let index = next_indices
                .entry(parameter.id.owner)
                .or_insert_with(|| self.scopes.declaration_parameters(parameter.id.owner).len());
            let binder = crate::ParamHead::new(
                crate::TypeParamId::new(parameter.id.owner, *index),
                Symbol::intern(&format!("{}.{}", parameter.spelling, associated)),
            );
            *index += 1;
            self.associated_schema_parameters
                .parameters
                .insert(key, binder);
        }
    }

    fn collect_associated_schema_projections(
        &self,
        ty: &Ty,
        out: &mut std::collections::BTreeSet<(crate::ParamHead, crate::DefId, Symbol)>,
    ) {
        if let Some(key) = self.associated_schema_key(ty) {
            out.insert(key);
        }
        ty.map_children(&mut |child| {
            self.collect_associated_schema_projections(child, out);
            child.clone()
        });
    }

    fn associated_schema_key(&self, ty: &Ty) -> Option<(crate::ParamHead, crate::DefId, Symbol)> {
        let Ty::AssocType {
            base,
            trait_name,
            assoc_name,
        } = ty
        else {
            return None;
        };
        let Ty::Named {
            head: crate::TypeHead::Param(parameter),
            args,
        } = base.as_ref()
        else {
            return None;
        };
        if !args.is_empty() || parameter.is_receiver() {
            return None;
        }
        let trait_id = self.assoc_carrier_trait(trait_name)?;
        let owners = self.associated_type_owners(trait_id, assoc_name);
        let [owner] = owners.as_slice() else {
            return None;
        };
        Some((*parameter, *owner, Symbol::intern(assoc_name)))
    }

    pub(super) fn bind_associated_schema_type(&self, ty: &Ty) -> Ty {
        if let Some(parameter) = self
            .associated_schema_key(ty)
            .and_then(|key| self.associated_schema_parameters.parameters.get(&key))
        {
            return Ty::param(*parameter);
        }
        ty.map_children_pub(&|child| self.bind_associated_schema_type(child))
    }

    pub(super) fn complete_associated_schema_arguments(
        &self,
        calls: &mut HashMap<SpanKey, Vec<Ty>>,
    ) {
        for (site, arguments) in calls {
            let Some(CallTarget::User(declaration) | CallTarget::ImplMethod(declaration)) =
                self.direct_call_targets.get(site)
            else {
                continue;
            };
            let Some(signature) = self.fn_sigs.get(declaration) else {
                continue;
            };
            let mut expanded = Vec::new();
            for (parameter, argument) in signature.type_params.iter().zip(arguments.iter()) {
                expanded.push(argument.clone());
                for (base, trait_id, associated) in
                    self.associated_schema_parameters.parameters.keys()
                {
                    if base == parameter {
                        let projected =
                            self.project_associated_type(argument, *trait_id, *associated);
                        expanded.push(self.finalize_type_for_handoff(&projected));
                    }
                }
            }
            if !expanded.is_empty() {
                *arguments = expanded;
            }
        }
    }
}
