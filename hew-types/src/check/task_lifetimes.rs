use super::effects::EffectBody;
use super::types::PendingCallableTarget;
use super::{
    CallableArgumentFlow, CallableCandidate, Checker, IndirectCallCandidates, SpanKey,
    TypeErrorKind,
};
use crate::env::TypeBindingId;
use hew_parser::ast::{Block, Expr, Span, Spanned};
use std::collections::{HashMap, HashSet};

#[derive(Clone, Debug, PartialEq, Eq)]
struct TaskLifetime {
    owner: Option<EffectBody>,
    scope: Option<SpanKey>,
}

#[derive(Clone, Debug)]
enum Boundary {
    Actor,
    Return(TaskLifetime),
}

#[derive(Clone, Debug, Default)]
pub(super) struct TaskLifetimes {
    scopes: Vec<TaskLifetime>,
    producers: HashMap<SpanKey, (TaskLifetime, IndirectCallCandidates)>,
    pub(super) scope_results: HashSet<SpanKey>,
    escapes: Vec<(SpanKey, IndirectCallCandidates, Boundary, Option<String>)>,
}

impl TaskLifetimes {
    pub(super) fn checked_results(&self) -> HashSet<SpanKey> {
        self.producers.keys().cloned().collect()
    }
}

type Actuals = HashMap<TypeBindingId, IndirectCallCandidates>;

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
            .find(|scope| scope.owner == owner)
            .cloned()
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
        self.task_lifetimes.scopes.push(TaskLifetime {
            owner: self.effect_graph.current_body.clone(),
            scope: Some(SpanKey::in_module(span, self.current_module_idx)),
        });
    }

    pub(super) fn leave_task_lifetime_scope(&mut self, block: &Block, span: &Span) {
        if let Some(tail) = &block.trailing_expr {
            self.record_task_escape(
                tail,
                Boundary::Return(TaskLifetime {
                    owner: self.effect_graph.current_body.clone(),
                    scope: Some(SpanKey::in_module(span, self.current_module_idx)),
                }),
            );
        }
        if block.trailing_expr.is_none() {
            self.task_lifetimes
                .scope_results
                .insert(SpanKey::in_module(span, self.current_module_idx));
        }
        self.task_lifetimes
            .scopes
            .pop()
            .expect("entered task lifetime");
    }

    pub(super) fn record_task_actor_transfer(&mut self, expr: &Expr, span: &Span) {
        self.record_task_escape(&(expr.clone(), span.clone()), Boundary::Actor);
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
        for (site, candidates, boundary, module) in std::mem::take(&mut self.task_lifetimes.escapes)
        {
            if !self.task_candidates_escape(
                &candidates,
                &boundary,
                &Actuals::new(),
                flows,
                &mut HashSet::new(),
            ) {
                if let Boundary::Return(TaskLifetime {
                    scope: Some(scope), ..
                }) = &boundary
                {
                    self.task_lifetimes.scope_results.insert(scope.clone());
                }
                continue;
            }
            let message = match boundary {
                Boundary::Actor => "a scoped task handle cannot escape to an actor, including through a captured callable or aggregate",
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

    fn task_candidates_escape(
        &self,
        candidates: &IndirectCallCandidates,
        boundary: &Boundary,
        actuals: &Actuals,
        flows: &HashMap<SpanKey, Vec<CallableArgumentFlow>>,
        seen: &mut HashSet<CallableCandidate>,
    ) -> bool {
        candidates
            .known
            .iter()
            .any(|candidate| self.task_candidate_escapes(candidate, boundary, actuals, flows, seen))
    }

    fn task_call_result_escapes(
        &self,
        call: (&SpanKey, &IndirectCallCandidates),
        boundary: &Boundary,
        actuals: &Actuals,
        flows: &HashMap<SpanKey, Vec<CallableArgumentFlow>>,
        seen: &mut HashSet<CallableCandidate>,
        project_task: bool,
    ) -> bool {
        let (site, callees) = call;
        callees.known.iter().any(|callee| {
            let owner = match callee {
                CallableCandidate::Declaration(id) => EffectBody::Declaration(*id),
                CallableCandidate::Closure(key) => EffectBody::Closure(key.clone()),
                CallableCandidate::Formal(formal) => {
                    if !seen.insert(callee.clone()) {
                        return false;
                    }
                    let escaped = formal_sources(*formal, actuals, flows)
                        .iter()
                        .any(|callees| {
                            self.task_call_result_escapes(
                                (site, callees),
                                boundary,
                                actuals,
                                flows,
                                seen,
                                project_task,
                            )
                        });
                    seen.remove(callee);
                    return escaped;
                }
                _ => return false,
            };
            let Some(returns) = self.callable_return_candidates.get(&owner) else {
                return false;
            };
            let Some(pending) = self.pending_callable_arguments.get(site) else {
                return false;
            };
            let mut call_actuals = actuals.clone();
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
            if project_task {
                returns.known.iter().any(|candidate| {
                    self.task_candidate_escapes(
                        &CallableCandidate::TaskResult(Box::new(candidate.clone())),
                        boundary,
                        &call_actuals,
                        flows,
                        seen,
                    )
                })
            } else {
                self.task_candidates_escape(returns, boundary, &call_actuals, flows, seen)
            }
        })
    }

    fn task_result_escapes(
        &self,
        candidate: &CallableCandidate,
        boundary: &Boundary,
        actuals: &Actuals,
        flows: &HashMap<SpanKey, Vec<CallableArgumentFlow>>,
        seen: &mut HashSet<CallableCandidate>,
    ) -> bool {
        match candidate {
            CallableCandidate::TaskProducer(site) => self
                .task_lifetimes
                .producers
                .get(site)
                .is_some_and(|(_, result)| {
                    self.task_candidates_escape(result, boundary, actuals, flows, seen)
                }),
            CallableCandidate::Formal(formal) => formal_sources(*formal, actuals, flows)
                .iter()
                .any(|candidates| {
                    candidates.known.iter().any(|candidate| {
                        self.task_candidate_escapes(
                            &CallableCandidate::TaskResult(Box::new(candidate.clone())),
                            boundary,
                            actuals,
                            flows,
                            seen,
                        )
                    })
                }),
            CallableCandidate::CallResult(site) => {
                self.task_selected_result_escapes(site, boundary, actuals, flows, seen, true)
            }
            CallableCandidate::Sequence(values) => values.iter().any(|candidate| {
                self.task_candidate_escapes(
                    &CallableCandidate::TaskResult(Box::new(candidate.clone())),
                    boundary,
                    actuals,
                    flows,
                    seen,
                )
            }),
            _ => false,
        }
    }

    fn task_selected_result_escapes(
        &self,
        site: &SpanKey,
        boundary: &Boundary,
        actuals: &Actuals,
        flows: &HashMap<SpanKey, Vec<CallableArgumentFlow>>,
        seen: &mut HashSet<CallableCandidate>,
        project_task: bool,
    ) -> bool {
        self.pending_callable_arguments
            .get(site)
            .is_some_and(|pending| {
                let callees = match &pending.callee {
                    PendingCallableTarget::Declaration(id) => {
                        IndirectCallCandidates::single(CallableCandidate::Declaration(*id))
                    }
                    PendingCallableTarget::Indirect(callees) => callees.clone(),
                };
                self.task_call_result_escapes(
                    (site, &callees),
                    boundary,
                    actuals,
                    flows,
                    seen,
                    project_task,
                )
            })
    }

    fn task_candidate_escapes(
        &self,
        candidate: &CallableCandidate,
        boundary: &Boundary,
        actuals: &Actuals,
        flows: &HashMap<SpanKey, Vec<CallableArgumentFlow>>,
        seen: &mut HashSet<CallableCandidate>,
    ) -> bool {
        if !seen.insert(candidate.clone()) {
            return false;
        }
        let escaped = match candidate {
            CallableCandidate::TaskProducer(site) => {
                self.task_lifetimes.producers.get(site).is_some_and(
                    |(lifetime, _)| match boundary {
                        Boundary::Actor => true,
                        Boundary::Return(closing) => {
                            lifetime.owner == closing.owner
                                && (closing.scope.is_none() || lifetime.scope == closing.scope)
                        }
                    },
                )
            }
            CallableCandidate::Closure(site) => {
                self.closure_capture_facts
                    .get(site)
                    .is_some_and(|captures| {
                        captures.iter().any(|capture| {
                            self.callable_binding_candidates
                                .get(&capture.binding_id)
                                .is_some_and(|candidates| {
                                    self.task_candidates_escape(
                                        candidates, boundary, actuals, flows, seen,
                                    )
                                })
                        })
                    })
            }
            CallableCandidate::Sequence(values) => values
                .iter()
                .any(|value| self.task_candidate_escapes(value, boundary, actuals, flows, seen)),
            CallableCandidate::Aggregate(site) => self
                .aggregate_field_candidates
                .get(site)
                .is_some_and(|fields| {
                    fields.iter().any(|field| {
                        self.task_candidates_escape(
                            &field.candidates,
                            boundary,
                            actuals,
                            flows,
                            seen,
                        )
                    })
                }),
            CallableCandidate::Formal(formal) => formal_sources(*formal, actuals, flows)
                .iter()
                .any(|candidates| {
                    self.task_candidates_escape(candidates, boundary, actuals, flows, seen)
                }),
            CallableCandidate::CallResult(site) => {
                self.task_selected_result_escapes(site, boundary, actuals, flows, seen, false)
            }
            CallableCandidate::TaskResult(task) => {
                self.task_result_escapes(task, boundary, actuals, flows, seen)
            }
            CallableCandidate::Field {
                receiver,
                owner,
                index,
            } => {
                if let CallableCandidate::Aggregate(site) = receiver.as_ref() {
                    self.aggregate_field_candidates
                        .get(site)
                        .is_some_and(|fields| {
                            fields
                                .iter()
                                .filter(|field| field.owner == *owner && field.index == *index)
                                .any(|field| {
                                    self.task_candidates_escape(
                                        &field.candidates,
                                        boundary,
                                        actuals,
                                        flows,
                                        seen,
                                    )
                                })
                        })
                } else {
                    self.task_candidate_escapes(receiver, boundary, actuals, flows, seen)
                }
            }
            CallableCandidate::Declaration(_) => false,
        };
        seen.remove(candidate);
        escaped
    }
}
