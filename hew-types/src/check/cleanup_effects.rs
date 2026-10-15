//! Source-visible suspension caused by releasing owned values.

use std::collections::{HashMap, HashSet};

use hew_parser::ast::{Expr, Spanned};

use super::effects::{EffectBody, EffectGraph};
use super::{Checker, SpanKey, VariantDef};
use crate::env::{Binding, BindingOrigin, InitState};
use crate::{BuiltinType, ParamHead, Ty};

#[derive(Debug, Clone)]
pub struct CleanupValue {
    ty: Ty,
    absent: Vec<Vec<String>>,
    /// A borrowed parameter owns only the places it has replaced.
    owned: Option<Vec<Vec<String>>>,
    own_close: bool,
    static_callable: bool,
    candidates: super::IndirectCallCandidates,
}

#[derive(Debug, Clone)]
pub struct CleanupEffect {
    pub owner: EffectBody,
    value: CleanupValue,
    pub witness: String,
}

impl Checker {
    fn binding_cleanup(&self, binding: &Binding) -> Option<CleanupValue> {
        if binding.origin == BindingOrigin::Synthetic
            || self.actor_field_binding_ids.contains_key(&binding.id)
            || binding.is_moved
            || binding.released_at.is_some()
            || binding.init_state == InitState::Unassigned
            || binding.collection_borrow.is_some()
        {
            return None;
        }
        let owned = (binding.is_param()
            && binding.parameter_ownership == crate::env::ParameterOwnership::Borrow)
            .then(|| binding.parameter_replacements.clone());
        if owned.as_ref().is_some_and(Vec::is_empty) {
            return None;
        }
        let own_close = self
            .inherent_impl_method_declaration(&binding.ty, "close")
            .is_some_and(|close| {
                self.effect_graph.current_body == Some(EffectBody::Declaration(close))
                    && binding.is_receiver()
            });
        Some(CleanupValue {
            ty: binding.ty.clone(),
            absent: binding
                .moved_places
                .iter()
                .map(|place| place.path.clone())
                .collect(),
            owned,
            own_close,
            candidates: binding.value_candidates.clone(),
            static_callable: matches!(binding.ty.callable_body(), Ty::Function { .. })
                && Self::static_callable_candidates(&binding.value_candidates),
        })
    }

    fn static_callable_candidates(candidates: &super::IndirectCallCandidates) -> bool {
        !candidates.may_be_unknown
            && !candidates.known.is_empty()
            && candidates
                .known
                .iter()
                .all(|candidate| matches!(candidate, super::CallableCandidate::Declaration(_)))
    }

    fn record_cleanup(&mut self, value: CleanupValue) {
        if let Some(owner) = self.effect_graph.current_body.clone() {
            self.effect_graph.releases.push(CleanupEffect {
                owner,
                witness: format!("implicit release of {}", value.ty.user_facing()),
                value,
            });
        }
    }

    pub(super) fn record_scope_cleanup_effects(&mut self) {
        let values: Vec<_> = self
            .env
            .current_scope_bindings()
            .filter_map(|(_, id)| self.env.binding_by_id(id))
            .filter_map(|binding| self.binding_cleanup(binding))
            .collect();
        for value in values {
            self.record_cleanup(value);
        }
    }

    pub(super) fn pop_scope_with_cleanup(&mut self) {
        self.record_scope_cleanup_effects();
        self.env.pop_scope();
    }

    pub(super) fn record_return_cleanup_effects(&mut self) {
        let values: Vec<_> = self
            .env
            .visible_bindings_from_depth(self.lambda_capture_depth.unwrap_or(0))
            .filter(|binding| {
                matches!(
                    binding.origin,
                    BindingOrigin::Local
                        | BindingOrigin::Parameter
                        | BindingOrigin::ReceiverParameter
                )
            })
            .filter_map(|binding| self.binding_cleanup(binding))
            .collect();
        for value in values {
            self.record_cleanup(value);
        }
    }

    pub(super) fn record_discard_cleanup_effect(&mut self, expr: &Expr, ty: &Ty) {
        // An effect-position place read borrows the existing value. A produced
        // temporary, including a call result, is owned and released here.
        if self.expr_place(expr).is_none() {
            self.record_owned_cleanup_effect(ty);
        }
    }

    pub(super) fn record_owned_cleanup_effect(&mut self, ty: &Ty) {
        self.record_cleanup(CleanupValue {
            ty: ty.clone(),
            absent: Vec::new(),
            owned: None,
            own_close: false,
            static_callable: false,
            candidates: super::IndirectCallCandidates::unknown(),
        });
    }

    pub(super) fn record_assignment_cleanup_effect(&mut self, target: &Spanned<Expr>, ty: &Ty) {
        if let Some((root, path)) = self.expr_place(&target.0) {
            if self.env.lookup_ref(root).is_none_or(|binding| {
                binding.is_moved
                    || binding.init_state == InitState::Unassigned
                    || binding.released_at.is_some()
                    || binding.collection_borrow.is_some()
                    || (binding.is_param()
                        && binding.parameter_ownership == crate::env::ParameterOwnership::Borrow
                        && !binding
                            .parameter_replacements
                            .iter()
                            .any(|owned| path.starts_with(owned)))
            }) || self.env.place_move_conflict(root, &path).is_some()
            {
                return;
            }
        }
        let static_callable = matches!(ty.callable_body(), Ty::Function { .. })
            && self.expr_place(&target.0).is_some_and(|(root, path)| {
                path.is_empty()
                    && self.env.lookup_ref(root).is_some_and(|binding| {
                        Self::static_callable_candidates(&binding.value_candidates)
                    })
            });
        self.record_cleanup(CleanupValue {
            ty: ty.clone(),
            absent: Vec::new(),
            owned: None,
            own_close: false,
            static_callable,
            candidates: self.callable_candidates_for_expr(&target.0, &target.1),
        });
    }

    pub(super) fn record_once_capture_cleanup(
        &mut self,
        key: SpanKey,
        captures: &[super::ClosureCaptureFact],
    ) {
        let values = captures
            .iter()
            .map(|capture| CleanupValue {
                ty: capture.ty.clone(),
                absent: Vec::new(),
                owned: None,
                own_close: false,
                candidates: capture.value_candidates.clone(),
                static_callable: matches!(capture.ty.callable_body(), Ty::Function { .. })
                    && Self::static_callable_candidates(&capture.value_candidates),
            })
            .collect();
        self.effect_graph
            .once_releases
            .insert(EffectBody::Closure(key), values);
    }

    pub(super) fn published_cleanup_effects(&self, graph: &EffectGraph) -> Vec<CleanupEffect> {
        let mut effects = graph.releases.clone();
        for (owner, values) in &graph.once_releases {
            for value in values {
                effects.push(CleanupEffect {
                    owner: owner.clone(),
                    value: value.clone(),
                    witness: "release of captured values".to_string(),
                });
            }
        }
        for effect in &mut effects {
            effect.value.ty = self.subst.resolve(&effect.value.ty);
        }
        effects
    }

    pub(super) fn cleanup_suspends(
        &self,
        effect: &CleanupEffect,
        substitutions: &HashMap<ParamHead, Ty>,
        bodies: &HashMap<EffectBody, bool>,
        unknown: bool,
    ) -> bool {
        self.cleanup_value_suspends(&effect.value, substitutions, bodies, unknown)
    }

    pub(super) fn once_cleanup_suspends(
        &self,
        body: &EffectBody,
        graph: &EffectGraph,
        substitutions: &HashMap<ParamHead, Ty>,
        bodies: &HashMap<EffectBody, bool>,
        unknown: bool,
    ) -> bool {
        graph.once_releases.get(body).is_some_and(|values| {
            values
                .iter()
                .any(|value| self.cleanup_value_suspends(value, substitutions, bodies, unknown))
        })
    }

    fn cleanup_value_suspends(
        &self,
        value: &CleanupValue,
        substitutions: &HashMap<ParamHead, Ty>,
        bodies: &HashMap<EffectBody, bool>,
        unknown: bool,
    ) -> bool {
        let mut value = value.clone();
        value.ty = self.subst.resolve(&value.ty);
        cleanup_targets(&value, substitutions, self, unknown)
            .iter()
            .any(|target| match target {
                CleanupTarget::Body { body, .. } => bodies.get(body).copied().unwrap_or(true),
                CleanupTarget::Callable(_)
                | CleanupTarget::Unknown(_)
                | CleanupTarget::Suspends => true,
            })
    }
}

/// Executable releases selected by the same structural walk used for suspension.
#[derive(Debug, Clone)]
pub enum CleanupTarget {
    Body {
        body: EffectBody,
        type_args: Vec<Ty>,
    },
    Callable(super::IndirectCallCandidates),
    Unknown(Ty),
    Suspends,
}

trait CleanupModel {
    fn trivial(&self, ty: &Ty) -> bool;
    fn resource(&self, ty: &Ty) -> bool;
    fn close(&self, ty: &Ty) -> Option<crate::DefId>;
    fn definition(&self, id: crate::NominalId) -> Option<&super::TypeDef>;
}

impl CleanupModel for Checker {
    fn trivial(&self, ty: &Ty) -> bool {
        cleanup_is_trivial(ty, &self.class_declarations())
    }
    fn resource(&self, ty: &Ty) -> bool {
        (match ty {
            Ty::Named { head, .. } => head.nominal(),
            _ => None,
        })
        .is_some_and(|id| self.registry.is_resource(id))
    }
    fn close(&self, ty: &Ty) -> Option<crate::DefId> {
        self.inherent_impl_method_declaration(ty, "close")
    }
    fn definition(&self, id: crate::NominalId) -> Option<&super::TypeDef> {
        self.type_defs.get(&id)
    }
}

impl CleanupModel for super::TypeCheckOutput {
    fn trivial(&self, ty: &Ty) -> bool {
        cleanup_is_trivial(ty, self.type_fact_context.declarations())
    }
    fn resource(&self, ty: &Ty) -> bool {
        (match ty {
            Ty::Named { head, .. } => head.nominal(),
            _ => None,
        })
        .and_then(|id| self.type_fact_context.declarations().get(&id))
        .is_some_and(|decl| decl.marker == crate::DeclarationMarker::Resource)
    }
    fn close(&self, ty: &Ty) -> Option<crate::DefId> {
        let instance = crate::ResolvedTy::from_ty(ty)
            .ok()?
            .impl_receiver_instance(&self.defs)?;
        self.dispatch
            .candidates(
                instance.nominal,
                Some(&instance),
                hew_parser::ast::Symbol::intern("close"),
            )
            .into_iter()
            .find_map(|(owner, id)| {
                (owner == super::dispatch_table::MethodOwner::Inherent).then_some(id)
            })
    }
    fn definition(&self, id: crate::NominalId) -> Option<&super::TypeDef> {
        self.type_defs.get(&id)
    }
}

fn cleanup_is_trivial(ty: &Ty, declarations: &impl crate::value_class::ClassDeclarations) -> bool {
    crate::ResolvedTy::from_ty(ty).ok().is_some_and(|ty| {
        matches!(
            crate::value_class::classify_ty(
                &ty,
                &crate::value_class::ClassContext::new(declarations)
            ),
            Ok((
                crate::value_class::ValueClass::BitCopy
                    | crate::value_class::ValueClass::View
                    | crate::value_class::ValueClass::Linear,
                _
            )) | Err(crate::value_class::ClassError::RecursiveInstantiation { .. })
        )
    })
}

impl super::TypeCheckOutput {
    #[must_use]
    pub fn release_targets(
        &self,
        effect: &CleanupEffect,
        substitutions: &HashMap<ParamHead, Ty>,
    ) -> Vec<CleanupTarget> {
        cleanup_targets(&effect.value, substitutions, self, true)
    }

    #[must_use]
    pub fn owned_release_targets(
        &self,
        ty: &Ty,
        substitutions: &HashMap<ParamHead, Ty>,
    ) -> Vec<CleanupTarget> {
        cleanup_targets(
            &CleanupValue {
                ty: ty.clone(),
                absent: Vec::new(),
                owned: None,
                own_close: false,
                static_callable: false,
                candidates: super::IndirectCallCandidates::unknown(),
            },
            substitutions,
            self,
            true,
        )
    }
}

#[allow(
    clippy::too_many_lines,
    reason = "Keep the structural cleanup walk and its ownership filters together"
)]
fn cleanup_targets(
    value: &CleanupValue,
    substitutions: &HashMap<ParamHead, Ty>,
    model: &impl CleanupModel,
    unknown: bool,
) -> Vec<CleanupTarget> {
    let ty = value.ty.substitute_type_params_parallel(substitutions);
    let mut effects = Vec::new();
    let mut pending = vec![(ty, Vec::<String>::new(), value.own_close)];
    let mut seen = HashSet::new();

    while let Some((ty, path, own_close)) = pending.pop() {
        if value.absent.iter().any(|absent| path.starts_with(absent)) {
            continue;
        }
        if value.owned.as_ref().is_some_and(|owned| {
            !owned
                .iter()
                .any(|owned| path.starts_with(owned) || owned.starts_with(&path))
        }) {
            continue;
        }
        let whole = value
            .owned
            .as_ref()
            .is_none_or(|owned| owned.iter().any(|owned| path.starts_with(owned)));
        let remaining_absent: Vec<_> = value
            .absent
            .iter()
            .filter_map(|absent| absent.strip_prefix(path.as_slice()).map(<[String]>::to_vec))
            .collect();
        let remaining_owned: Vec<_> = value
            .owned
            .iter()
            .flatten()
            .filter_map(|owned| owned.strip_prefix(path.as_slice()).map(<[String]>::to_vec))
            .collect();
        if !seen.insert((
            ty.clone(),
            own_close,
            whole,
            remaining_absent,
            remaining_owned,
        )) {
            continue;
        }
        if model.trivial(&ty) {
            continue;
        }
        let mut child = |label: String, ty: Ty| {
            let mut child_path = path.clone();
            child_path.push(label);
            pending.push((ty, child_path, false));
        };
        match ty.callable_body() {
            Ty::Closure { captures, .. } => {
                for (index, ty) in captures.iter().enumerate() {
                    child(index.to_string(), ty.clone());
                }
            }
            Ty::Function { .. } if whole && !value.static_callable => {
                effects.push(CleanupTarget::Callable(value.candidates.clone()));
            }
            Ty::TraitObject { .. } if whole => effects.push(CleanupTarget::Unknown(ty.clone())),
            Ty::Var(_) if whole && unknown => effects.push(CleanupTarget::Unknown(ty.clone())),
            Ty::Tuple(fields) => {
                for (index, ty) in fields.iter().enumerate() {
                    child(index.to_string(), ty.clone());
                }
            }
            Ty::Array(element, size) if *size != 0 => child("[]".into(), (**element).clone()),
            Ty::Named { head, args } => {
                if head.is_param() && whole && unknown {
                    effects.push(CleanupTarget::Unknown(ty.clone()));
                    continue;
                }
                if whole && model.resource(&ty) && !own_close {
                    match model.close(&ty) {
                        Some(close) => effects.push(CleanupTarget::Body {
                            body: EffectBody::Declaration(close),
                            type_args: args.clone(),
                        }),
                        None => effects.push(CleanupTarget::Unknown(ty.clone())),
                    }
                    continue;
                }
                match head.builtin() {
                    // A generator erases its producer and captured cleanup.
                    // Suspension is known, but its source authority is not.
                    Some(BuiltinType::Generator) if whole && unknown => {
                        effects.push(CleanupTarget::Unknown(ty.clone()));
                    }
                    Some(BuiltinType::Generator) if whole => {
                        effects.push(CleanupTarget::Suspends);
                    }
                    Some(
                        BuiltinType::Task
                        | BuiltinType::ActorCall
                        | BuiltinType::Stream
                        | BuiltinType::Sink,
                    ) if whole => {
                        effects.push(CleanupTarget::Suspends);
                        for (index, ty) in args.iter().enumerate() {
                            child(index.to_string(), ty.clone());
                        }
                    }
                    Some(
                        BuiltinType::Vec
                        | BuiltinType::VecIter
                        | BuiltinType::HashMapIter
                        | BuiltinType::HashMap
                        | BuiltinType::HashSet
                        | BuiltinType::Rc
                        | BuiltinType::Option
                        | BuiltinType::Result,
                    ) => {
                        for (index, ty) in args.iter().enumerate() {
                            child(index.to_string(), ty.clone());
                        }
                    }
                    Some(_) => {}
                    None => {
                        if let Some(definition) = head.nominal().and_then(|id| model.definition(id))
                        {
                            for (label, ty) in &definition.fields {
                                child(
                                    label.clone(),
                                    Checker::instantiate_type_def_member(
                                        ty,
                                        &definition.type_params,
                                        args,
                                    ),
                                );
                            }
                            for variant in definition.variants.values() {
                                match variant {
                                    VariantDef::Unit => {}
                                    VariantDef::Tuple(fields) => {
                                        for (index, ty) in fields.iter().enumerate() {
                                            child(
                                                index.to_string(),
                                                Checker::instantiate_type_def_member(
                                                    ty,
                                                    &definition.type_params,
                                                    args,
                                                ),
                                            );
                                        }
                                    }
                                    VariantDef::Struct(fields) => {
                                        for (label, ty) in fields {
                                            child(
                                                label.clone(),
                                                Checker::instantiate_type_def_member(
                                                    ty,
                                                    &definition.type_params,
                                                    args,
                                                ),
                                            );
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            _ => {}
        }
    }
    effects
}
