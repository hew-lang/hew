//! Discharge written callable effects in the constructing generic instance.

use super::{
    CallTarget, Checker, EffectBody, EffectCallSite, EffectGraph, HashMap, HashSet, Invocation,
    SpanKey, Ty,
};

type Actuals = HashMap<crate::ParamHead, Ty>;
type Boundary = (Actuals, Option<(SpanKey, Option<String>)>);

impl Checker {
    pub(super) fn effect_construction_contexts(
        &self,
        body: &EffectBody,
        graph: &EffectGraph,
        visiting: &mut HashSet<EffectBody>,
    ) -> Vec<Boundary> {
        if !visiting.insert(body.clone()) {
            return Vec::new();
        }
        let mut contexts = Vec::new();
        match body {
            EffectBody::Declaration(declaration)
                if self
                    .fn_sigs
                    .get(declaration)
                    .is_some_and(|sig| !sig.type_params.is_empty()) =>
            {
                for (site, call) in &graph.calls {
                    if !matches!(self.effect_call_target(site), Some(CallTarget::User(id) | CallTarget::ImplMethod(id)) if id == declaration)
                    {
                        continue;
                    }
                    let enclosing = call.owner.as_ref().map_or_else(
                        || vec![(Actuals::new(), None)],
                        |owner| self.effect_construction_contexts(owner, graph, visiting),
                    );
                    for (actuals, boundary) in enclosing {
                        let applied = self.call_cleanup_substitutions(
                            *declaration,
                            site,
                            call.receiver.as_ref(),
                            &actuals,
                        );
                        contexts.push((
                            applied,
                            Some(boundary.unwrap_or_else(|| {
                                (site.diagnostic_span().clone(), call.source_module.clone())
                            })),
                        ));
                    }
                }
            }
            EffectBody::Closure(_) => {
                for (owner, children) in &graph.constructed_bodies {
                    if children.contains(body) {
                        contexts.extend(self.effect_construction_contexts(owner, graph, visiting));
                    }
                }
            }
            _ => contexts.push((Actuals::new(), None)),
        }
        visiting.remove(body);
        contexts
    }

    pub(super) fn instantiated_effect_target(
        &self,
        site: &EffectCallSite,
        call: &Invocation,
        actuals: &Actuals,
    ) -> Option<(crate::DefId, Actuals)> {
        match self.effect_call_target(site)? {
            CallTarget::User(id) | CallTarget::ImplMethod(id) => Some((
                *id,
                self.call_cleanup_substitutions(*id, site, call.receiver.as_ref(), actuals),
            )),
            CallTarget::StaticTraitMethod {
                declaring_trait,
                method,
            } => {
                use super::super::dispatch_table::MethodOwner;
                let receiver = self
                    .subst
                    .resolve(call.receiver.as_ref()?)
                    .substitute_type_params_parallel(actuals);
                let receiver = crate::ResolvedTy::from_ty(&receiver).ok()?;
                let instance = receiver.impl_receiver_instance(&self.defs)?;
                let trait_args: Vec<_> = self
                    .effect_trait_args(site)?
                    .iter()
                    .map(|ty| {
                        self.subst
                            .resolve(ty)
                            .substitute_type_params_parallel(actuals)
                    })
                    .collect();
                let mut candidates = self
                    .dispatch
                    .candidates(instance.nominal, Some(&instance), self.defs.name(*method))
                    .into_iter()
                    .filter_map(|(owner, declaration)| {
                        let admitted = owner == MethodOwner::Trait(*declaring_trait)
                            || (owner == MethodOwner::Inherent
                                && self.structural_witnesses.iter().any(|witness| {
                                    witness.self_type == instance.nominal
                                        && witness.declaring_trait == *declaring_trait
                                        && witness.method == *method
                                        && witness.inherent == declaration
                                }));
                        if !admitted {
                            return None;
                        }
                        let binders = self.impl_method_binders.get(&declaration)?;
                        let arguments = binders.implementation_arguments(
                            &self.defs,
                            declaration,
                            &receiver,
                            &trait_args,
                        )?;
                        let mut applied = self.call_cleanup_substitutions(
                            declaration,
                            site,
                            call.receiver.as_ref(),
                            actuals,
                        );
                        applied.extend(
                            binders
                                .impl_params
                                .iter()
                                .copied()
                                .zip(arguments.iter().map(crate::ResolvedTy::to_ty)),
                        );
                        Some((declaration, applied))
                    });
                let selected = candidates.next()?;
                candidates.next().is_none().then_some(selected)
            }
            _ => None,
        }
    }
}
