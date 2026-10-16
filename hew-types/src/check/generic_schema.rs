use super::{BTreeMap, Checker, HashMap, SpanKey, Symbol, Ty};

#[derive(Debug, Clone, Default)]
/// Implicit declaration-owned binders for associated types in generic bodies.
pub struct AssociatedTypeSchemas {
    parameters: BTreeMap<(crate::ParamHead, crate::DefId, Symbol), crate::ParamHead>,
}

impl AssociatedTypeSchemas {
    pub(super) fn projection_of(
        &self,
        parameter: crate::ParamHead,
    ) -> Option<(crate::ParamHead, crate::DefId, Symbol)> {
        self.parameters
            .iter()
            .find_map(|(projection, binder)| (*binder == parameter).then_some(*projection))
    }

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
        for projection in projections {
            self.intern_associated_schema_parameter(projection);
        }
    }

    fn intern_associated_schema_parameter(
        &mut self,
        projection: (crate::ParamHead, crate::DefId, Symbol),
    ) -> crate::ParamHead {
        if let Some(parameter) = self
            .associated_schema_parameters
            .parameters
            .get(&projection)
        {
            return *parameter;
        }
        let (base, _, associated) = projection;
        let index = self
            .associated_schema_parameters
            .parameters
            .values()
            .filter(|parameter| parameter.id.owner == base.id.owner)
            .map(|parameter| usize::from(parameter.id.index) + 1)
            .max()
            .unwrap_or_else(|| self.scopes.declaration_parameters(base.id.owner).len());
        let parameter = crate::ParamHead::new(
            crate::TypeParamId::new(base.id.owner, index),
            Symbol::intern(&format!("{}.{}", base.spelling, associated)),
        );
        self.associated_schema_parameters
            .parameters
            .insert(projection, parameter);
        parameter
    }

    /// Use the same declaration-owned binder for a projection's written bounds
    /// and the generic body's specialization schema.
    pub(super) fn register_projection_bound_subject(
        &mut self,
        ty: &Ty,
    ) -> Option<crate::ParamHead> {
        let normalized = self.normalize_for_use(ty);
        Self::bare_param(&normalized).or_else(|| {
            let projection = self
                .associated_schema_key(&normalized)
                .or_else(|| self.associated_schema_key(ty))?;
            Some(self.intern_associated_schema_parameter(projection))
        })
    }

    pub(super) fn bound_parameter(&self, ty: &Ty) -> Option<crate::ParamHead> {
        Self::bare_param(ty).or_else(|| {
            self.associated_schema_key(ty)
                .and_then(|projection| {
                    self.associated_schema_parameters
                        .parameters
                        .get(&projection)
                })
                .copied()
        })
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
            let Some(declaration) = self.selected_callable_declaration_at(site) else {
                continue;
            };
            let Some(signature) = self.fn_sigs.get(&declaration) else {
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
