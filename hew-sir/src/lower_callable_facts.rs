//! Project checked value origins through admitted semantic instances.

use super::{CallableId, DefId, HashMap, InstanceService, OpId, TypeCheckOutput};
use hew_types::ty::EffectBody;
use hew_types::{CallableCandidate, IndirectCallCandidates, SpanKey};
use std::collections::HashSet;

pub(super) struct CallableFacts<'a> {
    checked: &'a TypeCheckOutput,
    calls: HashMap<(CallableId, SpanKey), Vec<CallableId>>,
    values: HashMap<(CallableId, DefId), Vec<CallableId>>,
    pub(super) opaque_entries: HashSet<CallableId>,
    pub(super) indirect_sites: HashMap<(CallableId, OpId), SpanKey>,
}

impl<'a> CallableFacts<'a> {
    pub(super) fn new(checked: &'a TypeCheckOutput) -> Self {
        Self {
            checked,
            calls: HashMap::new(),
            values: HashMap::new(),
            opaque_entries: HashSet::new(),
            indirect_sites: HashMap::new(),
        }
    }

    pub(super) fn record_value(
        &mut self,
        owner: CallableId,
        declaration: DefId,
        target: CallableId,
    ) {
        let values = self.values.entry((owner, declaration)).or_default();
        if !values.contains(&target) {
            values.push(target);
        }
        // The checked direct-call argument rows do not describe invocations
        // through this value. Its formal arguments must remain open.
        self.opaque_entries.insert(target);
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum Projection {
    Field(hew_types::NominalId, u32),
    Element(usize),
}

type Visit = (CallableId, CallableCandidate);

fn empty() -> crate::CallableTargets {
    crate::CallableTargets {
        known: Vec::new(),
        may_be_unknown: false,
    }
}

fn join(targets: &mut crate::CallableTargets, next: crate::CallableTargets) {
    targets.may_be_unknown |= next.may_be_unknown;
    for target in next.known {
        if !targets.known.contains(&target) {
            targets.known.push(target);
        }
    }
}

impl InstanceService<'_> {
    pub(super) fn record_checked_call(
        &mut self,
        caller: CallableId,
        site: hew_hir::SiteId,
        target: CallableId,
    ) {
        if let Some(key) = self.module.checked_sites.get(&site) {
            let targets = self
                .callable_facts
                .calls
                .entry((caller, key.clone()))
                .or_default();
            if !targets.contains(&target) {
                targets.push(target);
            }
        } else {
            self.callable_facts.opaque_entries.insert(target);
        }
    }

    fn checked_body(&self, callable: CallableId) -> Option<EffectBody> {
        let target = self.callable(callable)?;
        match target.instance {
            super::CallableInstance::Closure(id) => {
                let (source, _) = self.closure_sources.get(id.0 as usize)?;
                Some(EffectBody::Closure(
                    self.module.checked_sites.get(&source.site)?.clone(),
                ))
            }
            super::CallableInstance::Monomorphic | super::CallableInstance::Generic(_) => {
                Some(EffectBody::Declaration(target.declaration))
            }
            _ => None,
        }
    }

    pub(super) fn project_callable_targets(&mut self) {
        let mut projected = HashMap::new();
        for function in &self.functions {
            for block in &function.blocks {
                let crate::SemTerminator::IndirectCall { id, signature, .. } = &block.terminator
                else {
                    continue;
                };
                let owner = function.callable;
                let Some(site) = self.callable_facts.indirect_sites.get(&(owner, *id)) else {
                    continue;
                };
                let Some(candidates) = self
                    .callable_facts
                    .checked
                    .indirect_call_candidates
                    .get(site)
                else {
                    continue;
                };
                let mut targets =
                    self.resolve_candidates(owner, candidates, &[], &mut HashSet::new());
                targets.known.retain(|id| {
                    self.callable(*id).is_some_and(|target| {
                        let receiver = usize::from(matches!(
                            target.instance,
                            super::CallableInstance::Closure(_)
                        ));
                        target.signature.params.get(receiver..) == Some(signature.params.as_slice())
                            && target.signature.return_ty == signature.return_ty
                    })
                });
                targets.known.sort_unstable();
                // An empty expansion proves no callable target.
                targets.may_be_unknown |= targets.known.is_empty();
                projected.insert((owner, *id), targets);
            }
        }
        for function in &mut self.functions {
            for block in &mut function.blocks {
                if let crate::SemTerminator::IndirectCall { id, targets, .. } =
                    &mut block.terminator
                {
                    if let Some(projected) = projected.remove(&(function.callable, *id)) {
                        *targets = projected;
                    }
                }
            }
        }
    }

    fn resolve_candidates(
        &self,
        owner: CallableId,
        candidates: &IndirectCallCandidates,
        projections: &[Projection],
        seen: &mut HashSet<Visit>,
    ) -> crate::CallableTargets {
        let mut targets = empty();
        targets.may_be_unknown = candidates.may_be_unknown;
        for candidate in &candidates.known {
            join(
                &mut targets,
                self.resolve_candidate(owner, candidate, projections, seen),
            );
        }
        targets
    }

    fn resolve_formal(
        &self,
        owner: CallableId,
        formal: hew_types::env::TypeBindingId,
        projections: &[Projection],
        seen: &mut HashSet<Visit>,
    ) -> crate::CallableTargets {
        let facts = &self.callable_facts;
        if let Some(super::CallableInstance::Closure(id)) =
            self.callable(owner).map(|target| &target.instance)
        {
            let closure = &self.closures[id.0 as usize];
            if let Some(EffectBody::Closure(site)) = self.checked_body(owner) {
                if let Some(capture) =
                    facts
                        .checked
                        .closure_capture_facts
                        .get(&site)
                        .and_then(|captures| {
                            captures.iter().find(|capture| capture.binding_id == formal)
                        })
                {
                    return self.resolve_candidates(
                        closure.instance.enclosing,
                        &capture.value_candidates,
                        projections,
                        seen,
                    );
                }
            }
        }
        if facts.opaque_entries.contains(&owner)
            || self
                .callable(owner)
                .is_none_or(|target| target.kind != super::SemCallableKind::HewDirect)
            || self
                .vtables
                .iter()
                .flat_map(|table| &table.slots)
                .any(|slot| slot.callee == owner)
            || self
                .value_capabilities
                .values()
                .any(|selected| selected.callable == Some(owner))
            || self
                .structural_display
                .values()
                .any(|selected| selected.display == Some(owner))
        {
            return crate::CallableTargets::default();
        }
        let mut result = empty();
        let mut supplied = false;
        for ((caller, site), callees) in &facts.calls {
            if !callees.contains(&owner) {
                continue;
            }
            supplied = true;
            let candidates = facts
                .checked
                .callable_argument_flows
                .get(site)
                .and_then(|flows| flows.iter().find(|flow| flow.formal == formal))
                .map(|flow| &flow.candidates)
                .or_else(|| {
                    let body = self.checked_body(owner)?;
                    let slot = facts
                        .checked
                        .callable_formals
                        .get(&body)?
                        .iter()
                        .position(|binding| *binding == formal)?;
                    facts
                        .checked
                        .generic_trait_call_arguments
                        .get(site)?
                        .iter()
                        .find(|actual| actual.slot == slot)
                        .map(|actual| &actual.candidates)
                });
            join(
                &mut result,
                candidates.map_or_else(crate::CallableTargets::default, |candidates| {
                    self.resolve_candidates(*caller, candidates, projections, seen)
                }),
            );
        }
        result.may_be_unknown |= !supplied;
        result
    }

    #[expect(
        clippy::too_many_lines,
        reason = "project each checker-owned value-origin variant"
    )]
    fn resolve_candidate(
        &self,
        owner: CallableId,
        candidate: &CallableCandidate,
        projections: &[Projection],
        seen: &mut HashSet<Visit>,
    ) -> crate::CallableTargets {
        let visit = (owner, candidate.clone());
        if !seen.insert(visit.clone()) {
            return crate::CallableTargets::default();
        }
        let facts = &self.callable_facts;
        let result = match candidate {
            CallableCandidate::Declaration(declaration) if projections.is_empty() => {
                facts.values.get(&(owner, *declaration)).map_or_else(
                    crate::CallableTargets::default,
                    |known| crate::CallableTargets {
                        known: known.clone(),
                        may_be_unknown: false,
                    },
                )
            }
            CallableCandidate::Closure(site) if projections.is_empty() => {
                let known: Vec<_> = self
                    .closures
                    .iter()
                    .filter(|closure| closure.instance.enclosing == owner)
                    .filter(|closure| {
                        self.module
                            .checked_sites
                            .get(&self.closure_sources[closure.id.0 as usize].0.site)
                            == Some(site)
                    })
                    .map(|closure| closure.body)
                    .collect();
                crate::CallableTargets {
                    may_be_unknown: known.is_empty(),
                    known,
                }
            }
            CallableCandidate::Formal(formal) => {
                self.resolve_formal(owner, *formal, projections, seen)
            }
            CallableCandidate::CallResult(site) => {
                if let Some(callees) = facts.calls.get(&(owner, site.clone())) {
                    let mut targets = empty();
                    for callee in callees {
                        let returns = self
                            .checked_body(*callee)
                            .and_then(|body| facts.checked.callable_return_candidates.get(&body));
                        join(
                            &mut targets,
                            returns.map_or_else(crate::CallableTargets::default, |returns| {
                                self.resolve_candidates(*callee, returns, projections, seen)
                            }),
                        );
                    }
                    targets
                } else {
                    crate::CallableTargets::default()
                }
            }
            CallableCandidate::Field {
                receiver,
                owner: nominal,
                index,
            } => {
                let mut path = projections.to_vec();
                path.push(Projection::Field(*nominal, *index));
                self.resolve_candidate(owner, receiver, &path, seen)
            }
            CallableCandidate::Element { receiver, index } => {
                let mut path = projections.to_vec();
                path.push(Projection::Element(*index));
                self.resolve_candidate(owner, receiver, &path, seen)
            }
            CallableCandidate::Sequence(values) => {
                if let Some((Projection::Element(index), rest)) = projections.split_last() {
                    values
                        .get(*index)
                        .map_or_else(crate::CallableTargets::default, |value| {
                            self.resolve_candidates(owner, value, rest, seen)
                        })
                } else {
                    crate::CallableTargets::default()
                }
            }
            CallableCandidate::Aggregate(site) => {
                let selected =
                    facts
                        .checked
                        .aggregate_field_candidates
                        .get(site)
                        .and_then(|fields| {
                            let (projection, rest) = projections.split_last()?;
                            fields
                                .iter()
                                .find(|field| match projection {
                                    Projection::Field(nominal, index) => {
                                        field.owner == *nominal && field.index == *index
                                    }
                                    Projection::Element(index) => {
                                        usize::try_from(field.index) == Ok(*index)
                                    }
                                })
                                .map(|field| (&field.candidates, rest))
                        });
                selected.map_or_else(crate::CallableTargets::default, |(candidates, rest)| {
                    self.resolve_candidates(owner, candidates, rest, seen)
                })
            }
            _ => crate::CallableTargets::default(),
        };
        seen.remove(&visit);
        result
    }
}
