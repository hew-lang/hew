//! Discharge written callable effects in the constructing generic instance.

use super::{
    CallTarget, Checker, EffectBody, EffectGraph, HashMap, HashSet, Invocation, SpanKey,
    SuspensionObligation, Ty,
};

type Actuals = HashMap<crate::ParamHead, Ty>;
type Instance = (EffectBody, Vec<(crate::ParamHead, Ty)>);
type Boundary = (Actuals, Option<(SpanKey, Option<String>)>);

fn instance(body: &EffectBody, actuals: &Actuals) -> Instance {
    let mut arguments: Vec<_> = actuals
        .iter()
        .map(|(parameter, ty)| (*parameter, ty.clone()))
        .collect();
    arguments.sort_by_key(|(parameter, _)| parameter.id);
    (body.clone(), arguments)
}

impl Checker {
    pub(super) fn suspension_obligation_instances(
        &self,
        obligation: &SuspensionObligation,
        graph: &EffectGraph,
        bodies: &HashMap<EffectBody, bool>,
    ) -> Vec<(SpanKey, Option<String>)> {
        let contexts = obligation.owner.as_ref().map_or_else(
            || vec![(Actuals::new(), None)],
            |owner| self.effect_construction_contexts(owner, graph, &mut HashSet::new()),
        );
        contexts
            .into_iter()
            .filter_map(|(actuals, boundary)| {
                let (key, source) = boundary
                    .unwrap_or_else(|| (obligation.key.clone(), obligation.source_module.clone()));
                (self.instantiated_body_suspends(
                    &obligation.body,
                    &actuals,
                    graph,
                    bodies,
                    &mut HashSet::new(),
                ) || (obligation.once
                    && self.instantiated_releases_suspend(
                        self.effect_once_release_targets(&obligation.body, graph, &actuals),
                        &actuals,
                        graph,
                        bodies,
                        &mut HashSet::new(),
                    )))
                .then_some((key, source))
            })
            .collect()
    }

    fn effect_construction_contexts(
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
                for (key, call) in &graph.calls {
                    if !matches!(self.call_target(key), Some(CallTarget::User(id) | CallTarget::ImplMethod(id)) if id == declaration)
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
                            key,
                            call.receiver.as_ref(),
                            &actuals,
                        );
                        contexts.push((
                            applied,
                            Some(
                                boundary
                                    .unwrap_or_else(|| (key.clone(), call.source_module.clone())),
                            ),
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

    fn instantiated_effect_target(
        &self,
        key: &SpanKey,
        call: &Invocation,
        actuals: &Actuals,
    ) -> Option<(crate::DefId, Actuals)> {
        match self.call_target(key)? {
            CallTarget::User(id) | CallTarget::ImplMethod(id) => Some((
                *id,
                self.call_cleanup_substitutions(*id, key, call.receiver.as_ref(), actuals),
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
                let super::MethodCallRewrite::StaticTraitDispatch { trait_args, .. } =
                    self.method_call_rewrites.get(key)?
                else {
                    return None;
                };
                let trait_args: Vec<_> = trait_args
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
                            key,
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

    fn instantiated_body_suspends(
        &self,
        body: &EffectBody,
        actuals: &Actuals,
        graph: &EffectGraph,
        bodies: &HashMap<EffectBody, bool>,
        visiting: &mut HashSet<Instance>,
    ) -> bool {
        let identity = instance(body, actuals);
        if !visiting.insert(identity.clone()) {
            return false;
        }
        let result = graph.bodies.get(body).copied().unwrap_or(true)
            || graph
                .calls
                .iter()
                .filter(|(_, call)| call.owner.as_ref() == Some(body))
                .any(|(key, call)| {
                    if let Some((target, applied)) =
                        self.instantiated_effect_target(key, call, actuals)
                    {
                        return self.instantiated_body_suspends(
                            &EffectBody::Declaration(target),
                            &applied,
                            graph,
                            bodies,
                            visiting,
                        );
                    }
                    if matches!(
                        self.call_target(key),
                        Some(CallTarget::IndirectFunctionValue)
                    ) {
                        if let Some(Ty::Closure {
                            identity,
                            capabilities,
                            ..
                        }) = call.callee.as_ref().map(|ty| {
                            self.subst
                                .resolve(ty)
                                .substitute_type_params_parallel(actuals)
                                .callable_body()
                                .clone()
                        }) {
                            return self.instantiated_body_suspends(
                                &identity, actuals, graph, bodies, visiting,
                            ) || (capabilities.call == crate::CallableCallMode::Once
                                && self.instantiated_releases_suspend(
                                    self.effect_once_release_targets(&identity, graph, actuals),
                                    actuals,
                                    graph,
                                    bodies,
                                    visiting,
                                ));
                        }
                    }
                    self.invocation_suspends(key, call, graph, bodies)
                })
            || graph
                .releases
                .iter()
                .filter(|effect| &effect.owner == body)
                .any(|effect| {
                    self.instantiated_releases_suspend(
                        self.effect_release_targets(effect, actuals),
                        actuals,
                        graph,
                        bodies,
                        visiting,
                    )
                });
        visiting.remove(&identity);
        result
    }
    fn instantiated_releases_suspend(
        &self,
        targets: Vec<super::super::cleanup_effects::CleanupTarget>,
        actuals: &Actuals,
        graph: &EffectGraph,
        bodies: &HashMap<EffectBody, bool>,
        visiting: &mut HashSet<Instance>,
    ) -> bool {
        targets.into_iter().any(|target| match target {
            super::super::cleanup_effects::CleanupTarget::Body {
                body, type_args, ..
            } => {
                let mut applied = actuals.clone();
                if let EffectBody::Declaration(declaration) = &body {
                    if let Some(signature) = self.fn_sigs.get(declaration) {
                        applied.extend(signature.type_params.iter().copied().zip(type_args));
                    }
                }
                self.instantiated_body_suspends(&body, &applied, graph, bodies, visiting)
            }
            _ => true,
        })
    }
}
