//! Source-visible suspension caused by releasing owned values.

use std::collections::{HashMap, HashSet};

use hew_parser::ast::{Expr, Spanned};

use super::effects::{EffectBody, EffectGraph};
use super::{Checker, SpanKey, VariantDef};
use crate::env::{Binding, BindingOrigin, InitState};
use crate::{BuiltinType, ParamHead, Ty};

#[derive(Debug, Clone)]
pub(super) struct CleanupValue {
    ty: Ty,
    absent: Vec<Vec<String>>,
    /// A borrowed parameter owns only the places it has replaced.
    owned: Option<Vec<Vec<String>>>,
    own_close: bool,
    static_callable: bool,
}

#[derive(Debug, Clone)]
pub(super) struct CleanupEffect {
    pub(super) owner: EffectBody,
    value: CleanupValue,
    pub(super) witness: String,
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
            static_callable: matches!(&binding.ty, Ty::Function { .. })
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
        let static_callable = matches!(ty, Ty::Function { .. })
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
                static_callable: matches!(&capture.ty, Ty::Function { .. })
                    && Self::static_callable_candidates(&capture.value_candidates),
            })
            .collect();
        self.effect_graph
            .once_releases
            .insert(EffectBody::Closure(key), values);
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

    #[allow(
        clippy::too_many_lines,
        reason = "Keep the structural cleanup walk and its ownership filters together"
    )]
    fn cleanup_value_suspends(
        &self,
        value: &CleanupValue,
        substitutions: &HashMap<ParamHead, Ty>,
        bodies: &HashMap<EffectBody, bool>,
        unknown: bool,
    ) -> bool {
        let ty = self
            .subst
            .resolve(&value.ty)
            .substitute_type_params_parallel(substitutions);
        let mut pending = vec![(ty, Vec::<String>::new(), value.own_close)];
        let mut seen = HashSet::new();
        let declarations = self.class_declarations();
        let classes = crate::value_class::ClassContext::new(&declarations);
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
            if crate::ResolvedTy::from_ty(&ty).ok().is_some_and(|ty| {
                crate::value_class::classify_ty(&ty, &classes).is_ok_and(|(class, _)| {
                    matches!(
                        class,
                        crate::value_class::ValueClass::BitCopy
                            | crate::value_class::ValueClass::View
                            | crate::value_class::ValueClass::Linear
                    )
                })
            }) {
                continue;
            }
            let mut child = |label: String, ty: Ty| {
                let mut child_path = path.clone();
                child_path.push(label);
                pending.push((ty, child_path, false));
            };
            match &ty {
                Ty::Closure { captures, .. } => {
                    for (index, ty) in captures.iter().enumerate() {
                        child(index.to_string(), ty.clone());
                    }
                }
                Ty::Function { .. } if whole && !value.static_callable => return true,
                Ty::TraitObject { .. } if whole => return true,
                Ty::Var(_) if whole && unknown => return true,
                Ty::Tuple(fields) => {
                    for (index, ty) in fields.iter().enumerate() {
                        child(index.to_string(), ty.clone());
                    }
                }
                Ty::Array(element, size) if *size != 0 => child("[]".into(), (**element).clone()),
                Ty::Named { head, args } => {
                    if head.is_param() && whole && unknown {
                        return true;
                    }
                    if whole
                        && head
                            .nominal()
                            .is_some_and(|id| self.registry.is_resource(id))
                        && !own_close
                    {
                        let Some(close) = self.inherent_impl_method_declaration(&ty, "close")
                        else {
                            return true;
                        };
                        if bodies
                            .get(&EffectBody::Declaration(close))
                            .copied()
                            .unwrap_or(true)
                        {
                            return true;
                        }
                        continue;
                    }
                    match head.builtin() {
                        Some(
                            BuiltinType::Generator
                            | BuiltinType::ActorCall
                            | BuiltinType::Stream
                            | BuiltinType::Sink,
                        ) if whole => return true,
                        Some(
                            BuiltinType::Vec
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
                            if let Some(definition) = self.head_type_def(*head) {
                                for (label, ty) in &definition.fields {
                                    child(
                                        label.clone(),
                                        Self::instantiate_type_def_member(
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
                                                    Self::instantiate_type_def_member(
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
                                                    Self::instantiate_type_def_member(
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
        false
    }
}
