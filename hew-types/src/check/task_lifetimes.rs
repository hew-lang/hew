use super::effects::EffectBody;
use super::types::PendingCallableTarget;
use super::{
    CallableArgumentFlow, CallableCandidate, Checker, IndirectCallCandidates, SpanKey,
    TypeErrorKind,
};
use crate::env::TypeBindingId;
use hew_parser::ast::{Block, Expr, Span, Spanned};
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

#[derive(Clone, Debug, PartialEq, Eq)]
struct TaskLifetime {
    owner: Option<EffectBody>,
    scope: Option<SpanKey>,
}

#[derive(Clone, Debug)]
struct LifetimeScope {
    lifetime: TaskLifetime,
    outer: Vec<TypeBindingId>,
}

#[derive(Clone, Debug)]
enum Boundary {
    Actor(crate::Ty),
    Return(TaskLifetime),
}

#[derive(Clone, Debug, Default)]
pub(super) struct TaskLifetimes {
    scopes: Vec<LifetimeScope>,
    producers: HashMap<SpanKey, (TaskLifetime, IndirectCallCandidates)>,
    pub(super) scope_results: HashSet<SpanKey>,
    pub(super) discarded_values: HashSet<SpanKey>,
    escapes: Vec<(SpanKey, IndirectCallCandidates, Boundary, Option<String>)>,
}

impl TaskLifetimes {
    pub(super) fn checked_results(&self) -> HashSet<SpanKey> {
        self.producers.keys().cloned().collect()
    }
}

type Actuals = HashMap<TypeBindingId, IndirectCallCandidates>;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum Projection {
    TaskResult,
    Field(crate::NominalId, u32),
    Element(usize),
}

type Visit = (CallableCandidate, Vec<Projection>, usize);

#[derive(Clone, PartialEq, Eq, Hash)]
struct ValueOrigin {
    candidate: CallableCandidate,
    actuals: usize,
}

#[derive(Default)]
struct OriginEnvironments {
    values: Vec<Arc<Actuals>>,
    identities: HashMap<Vec<(TypeBindingId, IndirectCallCandidates)>, usize>,
    task_carriers: HashMap<crate::Ty, bool>,
}

impl OriginEnvironments {
    fn intern(&mut self, actuals: Actuals) -> usize {
        let mut identity: Vec<_> = actuals
            .iter()
            .map(|(binding, candidates)| (*binding, candidates.clone()))
            .collect();
        identity.sort_by_key(|(binding, _)| binding.0);
        if let Some(id) = self.identities.get(&identity) {
            return *id;
        }
        let id = self.values.len();
        self.values.push(Arc::new(actuals));
        self.identities.insert(identity, id);
        id
    }
}

fn formal_sources<'a>(
    formal: TypeBindingId,
    actuals: &'a Actuals,
    flows: &'a HashMap<SpanKey, Vec<CallableArgumentFlow>>,
) -> Vec<&'a IndirectCallCandidates> {
    if let Some(candidates) = actuals.get(&formal) {
        vec![candidates]
    } else {
        flows
            .values()
            .flatten()
            .filter(|flow| flow.formal == formal)
            .map(|flow| &flow.candidates)
            .collect()
    }
}

impl Checker {
    fn current_task_lifetime(&self) -> TaskLifetime {
        let owner = self.effect_graph.current_body.clone();
        self.task_lifetimes
            .scopes
            .iter()
            .rev()
            .find(|scope| scope.lifetime.owner == owner)
            .map(|scope| scope.lifetime.clone())
            .unwrap_or(TaskLifetime { owner, scope: None })
    }

    pub(super) fn record_task_producer(&mut self, expr: &Expr, span: &Span) {
        let result = match expr {
            Expr::ForkChild { expr } => self.callable_candidates_for_expr(&expr.0, &expr.1),
            Expr::ForkBlock { .. } => self
                .callable_return_candidates
                .get(&EffectBody::Closure(SpanKey::in_module(
                    span,
                    self.current_module_idx,
                )))
                .cloned()
                .unwrap_or_else(IndirectCallCandidates::unknown),
            Expr::Race(branches) => {
                let Some(kinds) = self
                    .race_operands
                    .get(&SpanKey::in_module(span, self.current_module_idx))
                else {
                    return;
                };
                let mut results = IndirectCallCandidates {
                    known: Vec::new(),
                    may_be_unknown: false,
                };
                for (branch, kind) in branches.iter().zip(kinds) {
                    let mut candidates = self.callable_candidates_for_expr(&branch.0, &branch.1);
                    if *kind == super::RaceOperandKind::Task {
                        candidates = candidates.task_results();
                    }
                    results.join(candidates);
                }
                results
            }
            _ => return,
        };
        self.task_lifetimes.producers.insert(
            SpanKey::in_module(span, self.current_module_idx),
            (self.current_task_lifetime(), result),
        );
    }

    pub(super) fn enter_task_lifetime_scope(&mut self, span: &Span) {
        self.task_lifetimes.scopes.push(LifetimeScope {
            lifetime: TaskLifetime {
                owner: self.effect_graph.current_body.clone(),
                scope: Some(SpanKey::in_module(span, self.current_module_idx)),
            },
            outer: self
                .env
                .visible_bindings()
                .map(|binding| binding.id)
                .collect(),
        });
    }

    pub(super) fn leave_task_lifetime_scope(&mut self, block: &Block, span: &Span) {
        let key = SpanKey::in_module(span, self.current_module_idx);
        let discarded = self.task_lifetimes.discarded_values.contains(&key);
        if let Some(tail) = &block.trailing_expr {
            if !discarded {
                self.record_task_escape(
                    tail,
                    Boundary::Return(TaskLifetime {
                        owner: self.effect_graph.current_body.clone(),
                        scope: Some(SpanKey::in_module(span, self.current_module_idx)),
                    }),
                );
            }
        }
        if block.trailing_expr.is_none() || discarded {
            self.task_lifetimes
                .scope_results
                .insert(SpanKey::in_module(span, self.current_module_idx));
        }
        let scope = self
            .task_lifetimes
            .scopes
            .pop()
            .expect("entered task lifetime");
        for id in scope.outer {
            let Some(binding) = self.env.binding_by_id(id) else {
                continue;
            };
            if binding.is_moved || binding.released_at.is_some() {
                continue;
            }
            let candidates = self.live_task_value_candidates(
                &binding.ty,
                &binding.value_candidates,
                &binding.moved_places,
            );
            self.task_lifetimes.escapes.push((
                SpanKey::in_module(span, self.current_module_idx),
                candidates,
                Boundary::Return(scope.lifetime.clone()),
                self.current_module.clone(),
            ));
        }
    }

    fn live_task_value_candidates(
        &self,
        ty: &crate::Ty,
        source: &IndirectCallCandidates,
        moved: &[crate::env::MovedPlace],
    ) -> IndirectCallCandidates {
        if moved.is_empty() {
            return source.clone();
        }
        let ty = self.subst.resolve(ty);
        let crate::Ty::Named { head, args } = &ty else {
            return source.clone();
        };
        let (Some(owner), Some(definition)) = (head.nominal(), self.ty_type_def(&ty)) else {
            return source.clone();
        };
        let substitutions: HashMap<_, _> = definition
            .type_params
            .iter()
            .copied()
            .zip(args.iter().cloned())
            .collect();
        let mut retained = Vec::new();
        for (index, name) in definition.field_order.iter().enumerate() {
            let selected: Vec<_> = moved
                .iter()
                .filter(|place| place.path.first() == Some(name))
                .collect();
            if selected.iter().any(|place| place.path.len() == 1) {
                continue;
            }
            let candidates = IndirectCallCandidates {
                known: source
                    .known
                    .iter()
                    .map(|receiver| CallableCandidate::Field {
                        receiver: Box::new(receiver.clone()),
                        owner,
                        index: u32::try_from(index).expect("checked field index"),
                    })
                    .collect(),
                may_be_unknown: source.may_be_unknown,
            };
            let nested: Vec<_> = selected
                .iter()
                .map(|place| crate::env::MovedPlace {
                    path: place.path[1..].to_vec(),
                    moved_at: place.moved_at.clone(),
                })
                .collect();
            if let Some(field) = definition.fields.get(name) {
                retained.push(self.live_task_value_candidates(
                    &field.substitute_type_params_parallel(&substitutions),
                    &candidates,
                    &nested,
                ));
            }
        }
        IndirectCallCandidates::single(CallableCandidate::Sequence(retained))
    }

    pub(super) fn record_task_actor_transfer(&mut self, expr: &Expr, span: &Span) {
        let ty = if matches!(expr, Expr::SpawnLambdaActor { .. }) {
            crate::Ty::Unit
        } else {
            self.expr_types
                .get(&SpanKey::in_module(span, self.current_module_idx))
                .cloned()
                .unwrap_or(crate::Ty::Error)
        };
        self.record_task_escape(&(expr.clone(), span.clone()), Boundary::Actor(ty));
    }

    pub(super) fn record_task_return(&mut self, value: &Spanned<Expr>) {
        if matches!(&value.0, Expr::Block(body) if body.trailing_expr.is_none()) {
            return;
        }
        if let Some(owner) = self.effect_graph.current_body.clone() {
            let candidates = self.callable_candidates_for_expr(&value.0, &value.1);
            self.callable_return_candidates
                .entry(owner)
                .and_modify(|existing| existing.join(candidates.clone()))
                .or_insert(candidates);
        }
        self.record_task_escape(
            value,
            Boundary::Return(TaskLifetime {
                owner: self.effect_graph.current_body.clone(),
                scope: None,
            }),
        );
    }

    fn record_task_escape(&mut self, value: &Spanned<Expr>, boundary: Boundary) {
        let candidates = self.callable_candidates_for_expr(&value.0, &value.1);
        self.task_lifetimes.escapes.push((
            SpanKey::in_module(&value.1, self.current_module_idx),
            candidates,
            boundary,
            self.current_module.clone(),
        ));
    }

    pub(super) fn finish_task_lifetimes(
        &mut self,
        flows: &HashMap<SpanKey, Vec<CallableArgumentFlow>>,
    ) {
        let mut environments = OriginEnvironments::default();
        let actuals = environments.intern(Actuals::new());
        for (site, candidates, boundary, module) in std::mem::take(&mut self.task_lifetimes.escapes)
        {
            let structural =
                matches!(&boundary, Boundary::Actor(ty) if self.subst.resolve(ty).contains_task());
            let may_carry_task = self
                .expr_types
                .get(&site)
                .is_none_or(|ty| self.task_carrier_type(ty, &mut environments.task_carriers));
            if !structural
                && (!may_carry_task
                    || !self.task_candidates_escape(
                        &candidates,
                        &boundary,
                        actuals,
                        flows,
                        &mut HashSet::new(),
                        &mut environments,
                    ))
            {
                if let Boundary::Return(TaskLifetime {
                    scope: Some(scope), ..
                }) = &boundary
                {
                    self.task_lifetimes.scope_results.insert(scope.clone());
                }
                continue;
            }
            let message = match boundary {
                Boundary::Actor(_) => "a scoped task handle cannot escape to an actor, including through a captured callable or aggregate",
                Boundary::Return(_) => "a task handle cannot outlive its scope, including through a returned callable or aggregate",
            };
            let mut error = crate::error::TypeError::new(
                TypeErrorKind::InvalidOperation,
                site.start..site.end,
                message,
            );
            error.source_module = module;
            self.errors.push(error);
        }
    }

    fn task_carrier_type(&self, ty: &crate::Ty, known: &mut HashMap<crate::Ty, bool>) -> bool {
        let ty = self.subst.resolve(ty);
        if let Some(carries) = known.get(&ty) {
            return *carries;
        }
        let carries = self.task_carrier_type_inner(&ty, &mut HashSet::new(), known);
        known.insert(ty, carries);
        carries
    }

    fn task_carrier_type_inner(
        &self,
        ty: &crate::Ty,
        seen: &mut HashSet<crate::Ty>,
        known: &HashMap<crate::Ty, bool>,
    ) -> bool {
        use crate::{BuiltinType, Ty};
        if let Some(carries) = known.get(ty) {
            return *carries;
        }
        if !seen.insert(ty.clone()) {
            return false;
        }
        let carries = match ty {
            Ty::Tuple(values) => values
                .iter()
                .any(|ty| self.task_carrier_type_inner(ty, seen, known)),
            Ty::Array(value, _) | Ty::Slice(value) => {
                self.task_carrier_type_inner(value, seen, known)
            }
            Ty::Named { head, args } => {
                let containers = matches!(
                    head.builtin(),
                    Some(
                        BuiltinType::Option
                            | BuiltinType::Result
                            | BuiltinType::Vec
                            | BuiltinType::HashMap
                            | BuiltinType::HashSet
                            | BuiltinType::Rc
                            | BuiltinType::Weak
                    )
                );
                if containers {
                    args.iter()
                        .any(|ty| self.task_carrier_type_inner(ty, seen, known))
                } else if let Some(nominal) = head.nominal() {
                    let recursive_arguments = seen.iter().any(|other| {
                        other != ty && matches!(other, Ty::Named { head: other, .. } if other.nominal() == Some(nominal))
                    });
                    recursive_arguments
                        || crate::value_class::ClassDeclarations::declared_type(
                            &self.class_declarations(),
                            nominal,
                        )
                        .is_none_or(|declaration| {
                            if declaration.is_opaque
                                || declaration.marker != crate::DeclarationMarker::None
                                || declaration.type_params.len() != args.len()
                            {
                                return true;
                            }
                            declaration.members.iter().any(|field| {
                                let field = Self::instantiate_type_def_member(
                                    &field.to_ty(),
                                    &declaration.type_params,
                                    args,
                                );
                                self.task_carrier_type_inner(&field, seen, known)
                            })
                        })
                } else {
                    true
                }
            }
            Ty::String | Ty::Bytes | Ty::CancellationToken | Ty::Never => false,
            ty if ty.is_primitive() => false,
            _ => true,
        };
        seen.remove(ty);
        carries
    }

    fn task_candidates_escape(
        &self,
        candidates: &IndirectCallCandidates,
        boundary: &Boundary,
        actuals: usize,
        flows: &HashMap<SpanKey, Vec<CallableArgumentFlow>>,
        seen: &mut HashSet<CallableCandidate>,
        environments: &mut OriginEnvironments,
    ) -> bool {
        if self.task_lifetimes.producers.is_empty() {
            return false;
        }
        self.task_value_origins(
            candidates,
            actuals,
            flows,
            &[],
            &mut HashSet::new(),
            environments,
        )
        .into_iter()
        .any(|origin| {
            if !seen.insert(origin.candidate.clone()) {
                return false;
            }
            let escaped = match &origin.candidate {
                CallableCandidate::TaskProducer(site) => self
                    .task_lifetimes
                    .producers
                    .get(site)
                    .is_some_and(|(lifetime, _)| match boundary {
                        Boundary::Actor(_) => true,
                        Boundary::Return(closing) => {
                            lifetime.owner == closing.owner
                                && (closing.scope.is_none() || lifetime.scope == closing.scope)
                        }
                    }),
                CallableCandidate::Closure(site) => self
                    .closure_capture_facts
                    .get(site)
                    .is_some_and(|captures| {
                        captures.iter().any(|capture| {
                            self.task_candidates_escape(
                                &capture.value_candidates,
                                boundary,
                                origin.actuals,
                                flows,
                                seen,
                                environments,
                            )
                        })
                    }),
                _ => false,
            };
            seen.remove(&origin.candidate);
            escaped
        })
    }

    fn task_value_origins(
        &self,
        candidates: &IndirectCallCandidates,
        actuals: usize,
        flows: &HashMap<SpanKey, Vec<CallableArgumentFlow>>,
        projections: &[Projection],
        seen: &mut HashSet<Visit>,
        environments: &mut OriginEnvironments,
    ) -> Vec<ValueOrigin> {
        let mut origins: Vec<_> = candidates
            .known
            .iter()
            .flat_map(|candidate| {
                self.task_value_origin(candidate, actuals, flows, projections, seen, environments)
            })
            .collect();
        let mut unique = HashSet::new();
        origins.retain(|origin| unique.insert(origin.clone()));
        origins
    }

    #[expect(
        clippy::too_many_lines,
        reason = "exhaustive checked value projection graph"
    )]
    fn task_value_origin(
        &self,
        candidate: &CallableCandidate,
        actuals: usize,
        flows: &HashMap<SpanKey, Vec<CallableArgumentFlow>>,
        projections: &[Projection],
        seen: &mut HashSet<Visit>,
        environments: &mut OriginEnvironments,
    ) -> Vec<ValueOrigin> {
        // Recursive calls can project the same checked formal indefinitely
        // without introducing another value source. The actual environment
        // still distinguishes nested applications at different call sites.
        let visit_projections = if matches!(candidate, CallableCandidate::Formal(_)) {
            Vec::new()
        } else {
            projections.to_vec()
        };
        let visit = (candidate.clone(), visit_projections, actuals);
        if !seen.insert(visit.clone()) {
            return Vec::new();
        }
        let result = match candidate {
            CallableCandidate::Erasure { value, .. } => {
                self.task_value_origins(value, actuals, flows, projections, seen, environments)
            }
            CallableCandidate::Formal(formal) => {
                let environment = Arc::clone(&environments.values[actuals]);
                formal_sources(*formal, &environment, flows)
                    .iter()
                    .flat_map(|candidates| {
                        self.task_value_origins(
                            candidates,
                            actuals,
                            flows,
                            projections,
                            seen,
                            environments,
                        )
                    })
                    .collect()
            }
            CallableCandidate::LoopInput { site, binding } => self
                .loop_value_candidates
                .get(&(site.clone(), *binding))
                .map_or_else(Vec::new, |candidates| {
                    self.task_value_origins(
                        candidates,
                        actuals,
                        flows,
                        projections,
                        seen,
                        environments,
                    )
                }),
            CallableCandidate::CallResult(_) | CallableCandidate::CallWriteback(_) => {
                self.task_call_origins(candidate, actuals, flows, projections, seen, environments)
            }
            CallableCandidate::Field {
                receiver,
                owner,
                index,
            } => {
                let mut path = projections.to_vec();
                path.push(Projection::Field(*owner, *index));
                self.task_value_origin(receiver, actuals, flows, &path, seen, environments)
            }
            CallableCandidate::Element { receiver, index } => {
                let mut path = projections.to_vec();
                path.push(Projection::Element(*index));
                self.task_value_origin(receiver, actuals, flows, &path, seen, environments)
            }
            CallableCandidate::TaskResult(task) => {
                let mut path = projections.to_vec();
                path.push(Projection::TaskResult);
                self.task_value_origin(task, actuals, flows, &path, seen, environments)
            }
            CallableCandidate::Sequence(values) => {
                if let Some((Projection::Element(index), rest)) = projections.split_last() {
                    values.get(*index).map_or_else(Vec::new, |value| {
                        self.task_value_origins(value, actuals, flows, rest, seen, environments)
                    })
                } else {
                    values
                        .iter()
                        .flat_map(|value| {
                            self.task_value_origins(
                                value,
                                actuals,
                                flows,
                                projections,
                                seen,
                                environments,
                            )
                        })
                        .collect()
                }
            }
            CallableCandidate::Aggregate(site)
                if self.expr_types.get(site).is_some_and(|ty| {
                    !self.task_carrier_type(ty, &mut environments.task_carriers)
                }) =>
            {
                Vec::new()
            }
            CallableCandidate::Aggregate(site) => self
                .aggregate_field_candidates
                .get(site)
                .map_or_else(Vec::new, |fields| {
                    let selected = projections.split_last();
                    fields
                        .iter()
                        .filter(|field| match selected {
                            Some((Projection::Field(owner, index), _)) => {
                                field.owner == *owner && field.index == *index
                            }
                            Some((Projection::Element(index), _)) => field.index as usize == *index,
                            _ => true,
                        })
                        .flat_map(|field| {
                            self.task_value_origins(
                                &field.candidates,
                                actuals,
                                flows,
                                selected.map_or(projections, |(_, rest)| rest),
                                seen,
                                environments,
                            )
                        })
                        .collect()
                }),
            CallableCandidate::TaskProducer(site)
                if projections.last() == Some(&Projection::TaskResult) =>
            {
                self.task_lifetimes
                    .producers
                    .get(site)
                    .map_or_else(Vec::new, |(_, result)| {
                        self.task_value_origins(
                            result,
                            actuals,
                            flows,
                            &projections[..projections.len() - 1],
                            seen,
                            environments,
                        )
                    })
            }
            CallableCandidate::TaskProducer(_)
            | CallableCandidate::Closure(_)
            | CallableCandidate::GeneratorBlock(_)
            | CallableCandidate::Declaration(_)
                if projections.is_empty() =>
            {
                vec![ValueOrigin {
                    candidate: candidate.clone(),
                    actuals,
                }]
            }
            _ => Vec::new(),
        };
        seen.remove(&visit);
        result
    }

    fn task_call_origins(
        &self,
        candidate: &CallableCandidate,
        actuals: usize,
        flows: &HashMap<SpanKey, Vec<CallableArgumentFlow>>,
        projections: &[Projection],
        seen: &mut HashSet<Visit>,
        environments: &mut OriginEnvironments,
    ) -> Vec<ValueOrigin> {
        let (site, receiver_output) = match candidate {
            CallableCandidate::CallResult(site) => (site, false),
            CallableCandidate::CallWriteback(site) => (site, true),
            _ => return Vec::new(),
        };
        let Some(pending) = self.pending_callable_arguments.get(site) else {
            return Vec::new();
        };
        let callees = match &pending.callee {
            PendingCallableTarget::Declaration(id) => {
                IndirectCallCandidates::single(CallableCandidate::Declaration(*id))
            }
            PendingCallableTarget::Indirect(callees) => callees.clone(),
        };
        self.task_value_origins(&callees, actuals, flows, &[], seen, environments)
            .into_iter()
            .flat_map(|callee| {
                let owner = match callee.candidate {
                    CallableCandidate::Declaration(id) => EffectBody::Declaration(id),
                    CallableCandidate::Closure(site) => EffectBody::Closure(site),
                    _ => return Vec::new(),
                };
                let outputs = if receiver_output {
                    &self.callable_receiver_outputs
                } else {
                    &self.callable_return_candidates
                };
                let Some(returns) = outputs.get(&owner) else {
                    return Vec::new();
                };
                let mut call_actuals = (*environments.values[callee.actuals]).clone();
                if let Some(formals) = self.callable_formals.get(&owner) {
                    let offset = usize::from(
                        pending.receiver.is_some() && formals.len() == pending.arguments.len() + 1,
                    );
                    if offset != 0 {
                        call_actuals.insert(
                            formals[0],
                            pending.receiver.clone().expect("receiver offset"),
                        );
                    }
                    for (index, actual) in pending.arguments.iter().enumerate() {
                        let slot = self
                            .call_argument_slots
                            .get(site)
                            .map_or(index, |slots| slots[index])
                            + offset;
                        if let Some(formal) = formals.get(slot) {
                            call_actuals.insert(*formal, actual.clone());
                        }
                    }
                }
                let call_actuals = environments.intern(call_actuals);
                self.task_value_origins(
                    returns,
                    call_actuals,
                    flows,
                    projections,
                    seen,
                    environments,
                )
            })
            .collect()
    }
}
