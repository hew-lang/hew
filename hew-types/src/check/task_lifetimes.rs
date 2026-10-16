use super::effects::EffectBody;
use super::types::PendingCallableTarget;
use super::{
    CallableArgumentFlow, CallableCandidate, Checker, IndirectCallCandidates, SpanKey,
    TypeErrorKind,
};
use crate::env::TypeBindingId;
use hew_parser::ast::{Block, Expr, Span, Spanned};
use std::collections::{HashMap, HashSet, VecDeque};

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

#[derive(Clone, Debug)]
struct TaskEscape {
    site: SpanKey,
    candidates: IndirectCallCandidates,
    binding_type: Option<crate::Ty>,
    boundary: Boundary,
    module: Option<String>,
}

#[derive(Clone, Debug, Default)]
pub(super) struct TaskLifetimes {
    scopes: Vec<LifetimeScope>,
    producers: HashMap<SpanKey, (TaskLifetime, IndirectCallCandidates)>,
    pub(super) scope_results: HashSet<SpanKey>,
    pub(super) discarded_values: HashSet<SpanKey>,
    escapes: Vec<TaskEscape>,
}

impl TaskLifetimes {
    pub(super) fn checked_results(&self) -> HashSet<SpanKey> {
        self.producers.keys().cloned().collect()
    }
}

type Actuals = HashMap<TypeBindingId, ActualSource>;

#[derive(Clone, PartialEq, Eq, Hash)]
struct ActualSource {
    candidates: IndirectCallCandidates,
    // Actual expressions resolve in the caller's environment, even when a
    // recursive call binds the same checked formal again.
    actuals: usize,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum Projection {
    TaskResult,
    Field(crate::NominalId, u32),
    Element(usize),
    /// Recursive projection growth requires a conservative value summary.
    Descendants,
}

type Visit = (CallableCandidate, Vec<Projection>, usize);

#[derive(Clone, PartialEq, Eq, Hash)]
struct ValueOrigin {
    candidate: CallableCandidate,
    actuals: usize,
}

struct OriginEnvironment {
    bindings: Actuals,
    parent: Option<usize>,
}

type EnvironmentKey = (Option<usize>, Vec<(TypeBindingId, ActualSource)>);

#[derive(Default)]
struct OriginEnvironments {
    values: Vec<OriginEnvironment>,
    identities: HashMap<EnvironmentKey, usize>,
    task_carriers: HashMap<crate::Ty, bool>,
    formal_sources: HashMap<TypeBindingId, Vec<IndirectCallCandidates>>,
    origins: OriginGraph,
    recursive_sources: HashMap<(TypeBindingId, usize), HashSet<ActualSource>>,
    prepared_captures: HashSet<ValueOrigin>,
    recursive_parents: HashMap<usize, HashSet<usize>>,
}

#[derive(Default)]
struct OriginGraph {
    // Origin sets grow monotonically. Callee discoveries add dependencies,
    // and changes wake only dependent queries until every cycle is settled.
    nodes: Vec<OriginNode>,
    identities: HashMap<Visit, usize>,
    queries: HashMap<(CallableCandidate, usize), Vec<usize>>,
    environment_formals: HashMap<usize, HashSet<TypeBindingId>>,
    pending: VecDeque<usize>,
    current: Option<usize>,
    call_contexts: HashMap<usize, HashMap<(EffectBody, usize), usize>>,
}

struct OriginNode {
    query: Visit,
    origins: HashSet<ValueOrigin>,
    dependents: HashSet<usize>,
    queued: bool,
}

impl OriginGraph {
    fn read(
        &mut self,
        candidate: &CallableCandidate,
        projections: &[Projection],
        actuals: usize,
    ) -> Vec<ValueOrigin> {
        let request = (candidate.clone(), projections.to_vec(), actuals);
        let mut query = request.clone();
        if !self.identities.contains_key(&query) {
            if let Some(current) = self.current {
                if self.growing_projection(candidate, actuals, projections.len(), current) {
                    query.1 = vec![Projection::Descendants];
                }
            }
        }
        let id = if let Some(id) = self.identities.get(&query) {
            *id
        } else {
            let id = self.nodes.len();
            self.queries
                .entry((candidate.clone(), actuals))
                .or_default()
                .push(id);
            if let CallableCandidate::Formal(formal) = candidate {
                self.environment_formals
                    .entry(actuals)
                    .or_default()
                    .insert(*formal);
            }
            self.identities.insert(query.clone(), id);
            self.nodes.push(OriginNode {
                query,
                origins: HashSet::new(),
                dependents: HashSet::new(),
                queued: true,
            });
            self.pending.push_back(id);
            id
        };
        self.identities.insert(request, id);
        if let Some(current) = self.current {
            self.nodes[id].dependents.insert(current);
        }
        self.nodes[id].origins.iter().cloned().collect()
    }

    fn recursive_call_context(&self, owner: &EffectBody, current: usize) -> Option<usize> {
        let mut pending: VecDeque<_> = self.nodes[current].dependents.iter().copied().collect();
        let mut seen = HashSet::from([current]);
        while let Some(id) = pending.pop_front() {
            if !seen.insert(id) {
                continue;
            }
            if let Some(contexts) = self.call_contexts.get(&id) {
                if let Some((_, actuals)) = contexts
                    .iter()
                    .find(|((candidate, _), _)| candidate == owner)
                {
                    return Some(*actuals);
                }
            }
            pending.extend(self.nodes[id].dependents.iter().copied());
        }
        None
    }

    fn invalidate_environment(&mut self, actuals: usize) {
        let formals: Vec<_> = self
            .environment_formals
            .get(&actuals)
            .into_iter()
            .flatten()
            .copied()
            .collect();
        for formal in formals {
            self.invalidate_formal(formal, actuals);
        }
    }

    fn invalidate_formal(&mut self, formal: TypeBindingId, actuals: usize) {
        if let Some(queries) = self
            .queries
            .get(&(CallableCandidate::Formal(formal), actuals))
        {
            for id in queries {
                if !self.nodes[*id].queued {
                    self.nodes[*id].queued = true;
                    self.pending.push_back(*id);
                }
            }
        }
    }

    fn growing_projection(
        &self,
        candidate: &CallableCandidate,
        actuals: usize,
        length: usize,
        current: usize,
    ) -> bool {
        let Some(peers) = self.queries.get(&(candidate.clone(), actuals)) else {
            return false;
        };
        let shorter: HashSet<_> = peers
            .iter()
            .copied()
            .filter(|id| {
                let path = &self.nodes[*id].query.1;
                path.last() != Some(&Projection::Descendants) && path.len() < length
            })
            .collect();
        if shorter.is_empty() {
            return false;
        }
        // Only a dependency cycle may widen a projection. Distinct finite field
        // selections at independent call sites retain their exact paths.
        let mut pending = vec![current];
        let mut seen = HashSet::new();
        while let Some(id) = pending.pop() {
            if shorter.contains(&id) {
                return true;
            }
            if seen.insert(id) {
                pending.extend(self.nodes[id].dependents.iter().copied());
            }
        }
        false
    }
}

impl OriginEnvironments {
    fn intern(&mut self, actuals: Actuals, parent: Option<usize>) -> usize {
        let mut identity: Vec<_> = actuals
            .iter()
            .map(|(binding, candidates)| (*binding, candidates.clone()))
            .collect();
        identity.sort_by_key(|(binding, _)| binding.0);
        let identity = (parent, identity);
        if let Some(id) = self.identities.get(&identity) {
            return *id;
        }
        let id = self.values.len();
        self.values.push(OriginEnvironment {
            bindings: actuals,
            parent,
        });
        self.identities.insert(identity, id);
        id
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
            self.task_lifetimes.escapes.push(TaskEscape {
                site: SpanKey::in_module(span, self.current_module_idx),
                candidates,
                binding_type: Some(binding.ty.clone()),
                boundary: Boundary::Return(scope.lifetime.clone()),
                module: self.current_module.clone(),
            });
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
        self.task_lifetimes.escapes.push(TaskEscape {
            site: SpanKey::in_module(&value.1, self.current_module_idx),
            candidates,
            binding_type: None,
            boundary,
            module: self.current_module.clone(),
        });
    }

    pub(super) fn finish_task_lifetimes(
        &mut self,
        flows: &HashMap<SpanKey, Vec<CallableArgumentFlow>>,
    ) {
        let mut environments = OriginEnvironments::default();
        for flow in flows.values().flatten() {
            environments
                .formal_sources
                .entry(flow.formal)
                .or_default()
                .push(flow.candidates.clone());
        }
        let actuals = environments.intern(Actuals::new(), None);
        let escapes = std::mem::take(&mut self.task_lifetimes.escapes);
        if !self.task_lifetimes.producers.is_empty() {
            // Settle every root before emitting diagnostics. A later root may
            // contribute another incoming edge to a shared recursive context.
            for escape in &escapes {
                if escape
                    .binding_type
                    .as_ref()
                    .or_else(|| self.expr_types.get(&escape.site))
                    .is_none_or(|ty| self.task_carrier_type(ty, &mut environments.task_carriers))
                {
                    self.solve_task_origins(&escape.candidates, actuals, &mut environments);
                }
            }
        }
        for TaskEscape {
            site,
            candidates,
            binding_type,
            boundary,
            module,
        } in escapes
        {
            let structural =
                matches!(&boundary, Boundary::Actor(ty) if self.subst.resolve(ty).contains_task());
            let may_carry_task = binding_type
                .as_ref()
                .or_else(|| self.expr_types.get(&site))
                .is_none_or(|ty| self.task_carrier_type(ty, &mut environments.task_carriers));
            if !structural
                && (!may_carry_task
                    || !self.task_candidates_escape(
                        &candidates,
                        &boundary,
                        actuals,
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
        let root = self.subst.resolve(ty);
        if let Some(carries) = known.get(&root) {
            return *carries;
        }
        let mut dependents: HashMap<crate::Ty, Vec<crate::Ty>> = HashMap::new();
        let mut carries = HashSet::new();
        let mut expanded = HashSet::new();
        let mut active: HashMap<crate::NominalId, crate::Ty> = HashMap::new();
        let mut pending = vec![(root.clone(), false)];
        while let Some((ty, leaving)) = pending.pop() {
            let nominal = match &ty {
                crate::Ty::Named { head, .. } => head.nominal(),
                _ => None,
            };
            if leaving {
                if let Some(nominal) = nominal {
                    active.remove(&nominal);
                }
                continue;
            }
            if !expanded.insert(ty.clone()) {
                continue;
            }
            dependents.entry(ty.clone()).or_default();
            if let Some(known) = known.get(&ty) {
                if *known {
                    carries.insert(ty);
                }
                continue;
            }
            if nominal.is_some_and(|nominal| active.get(&nominal).is_some_and(|other| other != &ty))
            {
                // Expansive generic recursion cannot establish absence of a
                // task. Exact recursive types remain ordinary graph cycles.
                carries.insert(ty);
                continue;
            }
            let (direct, children) = self.task_carrier_members(&ty);
            if direct {
                carries.insert(ty.clone());
            }
            if let Some(nominal) = nominal {
                active.insert(nominal, ty.clone());
                pending.push((ty.clone(), true));
            }
            for child in children {
                dependents
                    .entry(child.clone())
                    .or_default()
                    .push(ty.clone());
                // A dependency row is not evidence that its type was expanded.
                pending.push((child, false));
            }
        }
        let mut pending: Vec<_> = carries.iter().cloned().collect();
        while let Some(ty) = pending.pop() {
            for dependent in &dependents[&ty] {
                if carries.insert(dependent.clone()) {
                    pending.push(dependent.clone());
                }
            }
        }
        for ty in dependents.keys() {
            known.insert(ty.clone(), carries.contains(ty));
        }
        known[&root]
    }

    fn task_carrier_members(&self, ty: &crate::Ty) -> (bool, Vec<crate::Ty>) {
        use crate::{BuiltinType, Ty};
        match ty {
            Ty::Tuple(values) => (false, values.clone()),
            Ty::Array(value, _) | Ty::Slice(value) => (false, vec![(**value).clone()]),
            Ty::Named { head, args } => {
                if matches!(
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
                ) {
                    return (false, args.clone());
                }
                let Some(nominal) = head.nominal() else {
                    return (true, Vec::new());
                };
                let Some(declaration) = crate::value_class::ClassDeclarations::declared_type(
                    &self.class_declarations(),
                    nominal,
                ) else {
                    return (true, Vec::new());
                };
                if declaration.is_opaque
                    || declaration.marker != crate::DeclarationMarker::None
                    || declaration.type_params.len() != args.len()
                {
                    return (true, Vec::new());
                }
                (
                    false,
                    declaration
                        .members
                        .iter()
                        .map(|field| {
                            Self::instantiate_type_def_member(
                                &field.to_ty(),
                                &declaration.type_params,
                                args,
                            )
                        })
                        .collect(),
                )
            }
            Ty::String | Ty::Bytes | Ty::CancellationToken | Ty::Never => (false, Vec::new()),
            ty if ty.is_primitive() => (false, Vec::new()),
            _ => (true, Vec::new()),
        }
    }

    fn task_candidates_escape(
        &self,
        candidates: &IndirectCallCandidates,
        boundary: &Boundary,
        actuals: usize,
        environments: &mut OriginEnvironments,
    ) -> bool {
        if self.task_lifetimes.producers.is_empty() {
            return false;
        }
        let mut pending = self.solve_task_origins(candidates, actuals, environments);
        let mut seen = HashSet::new();
        while let Some(origin) = pending.pop() {
            if !seen.insert(origin.clone()) {
                continue;
            }
            match &origin.candidate {
                CallableCandidate::TaskProducer(site) => {
                    if self
                        .task_lifetimes
                        .producers
                        .get(site)
                        .is_some_and(|(lifetime, _)| match boundary {
                            Boundary::Actor(_) => true,
                            Boundary::Return(closing) => {
                                lifetime.owner == closing.owner
                                    && (closing.scope.is_none() || lifetime.scope == closing.scope)
                            }
                        })
                    {
                        return true;
                    }
                }
                CallableCandidate::Closure(site) => {
                    if let Some(captures) = self.closure_capture_facts.get(site) {
                        for capture in captures {
                            pending.extend(self.solve_task_origins(
                                &capture.value_candidates,
                                origin.actuals,
                                environments,
                            ));
                        }
                    }
                }
                _ => {}
            }
        }
        false
    }

    fn solve_task_origins(
        &self,
        candidates: &IndirectCallCandidates,
        actuals: usize,
        environments: &mut OriginEnvironments,
    ) -> Vec<ValueOrigin> {
        Self::task_value_origins(candidates, actuals, &[], environments);
        while let Some(id) = environments.origins.pending.pop_front() {
            environments.origins.nodes[id].queued = false;
            let (candidate, projections, actuals) = environments.origins.nodes[id].query.clone();
            environments.origins.current = Some(id);
            let origins = self.task_value_origin(&candidate, actuals, &projections, environments);
            environments.origins.current = None;
            for origin in &origins {
                if let CallableCandidate::Closure(site) = &origin.candidate {
                    if environments.prepared_captures.insert(origin.clone()) {
                        if let Some(captures) = self.closure_capture_facts.get(site) {
                            for capture in captures {
                                Self::task_value_origins(
                                    &capture.value_candidates,
                                    origin.actuals,
                                    &[],
                                    environments,
                                );
                            }
                        }
                    }
                }
            }
            let node = &mut environments.origins.nodes[id];
            let before = node.origins.len();
            node.origins.extend(origins);
            if node.origins.len() != before {
                let dependents: Vec<_> = node.dependents.iter().copied().collect();
                for dependent in dependents {
                    let node = &mut environments.origins.nodes[dependent];
                    if !node.queued {
                        node.queued = true;
                        environments.origins.pending.push_back(dependent);
                    }
                }
            }
        }
        Self::task_value_origins(candidates, actuals, &[], environments)
    }

    fn task_value_origins(
        candidates: &IndirectCallCandidates,
        actuals: usize,
        projections: &[Projection],
        environments: &mut OriginEnvironments,
    ) -> Vec<ValueOrigin> {
        let mut origins: Vec<_> = candidates
            .known
            .iter()
            .flat_map(|candidate| environments.origins.read(candidate, projections, actuals))
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
        projections: &[Projection],
        environments: &mut OriginEnvironments,
    ) -> Vec<ValueOrigin> {
        let descendants = projections.last() == Some(&Projection::Descendants);
        match candidate {
            CallableCandidate::Formal(formal) => {
                let mut sources =
                    if let Some(source) = environments.values[actuals].bindings.get(formal) {
                        vec![source.clone()]
                    } else {
                        let mut parents = environments
                            .recursive_parents
                            .get(&actuals)
                            .cloned()
                            .unwrap_or_default();
                        parents.extend(environments.values[actuals].parent);
                        if parents.is_empty() {
                            environments
                                .formal_sources
                                .get(formal)
                                .into_iter()
                                .flatten()
                                .map(|candidates| ActualSource {
                                    candidates: candidates.clone(),
                                    actuals,
                                })
                                .collect()
                        } else {
                            parents
                                .into_iter()
                                .map(|actuals| ActualSource {
                                    candidates: IndirectCallCandidates::single(
                                        CallableCandidate::Formal(*formal),
                                    ),
                                    actuals,
                                })
                                .collect()
                        }
                    };
                if let Some(recursive) = environments.recursive_sources.get(&(*formal, actuals)) {
                    sources.extend(recursive.iter().cloned());
                }
                sources
                    .iter()
                    .flat_map(|source| {
                        Self::task_value_origins(
                            &source.candidates,
                            source.actuals,
                            projections,
                            environments,
                        )
                    })
                    .collect()
            }
            CallableCandidate::LoopInput { site, binding } => self
                .loop_value_candidates
                .get(&(site.clone(), *binding))
                .map_or_else(Vec::new, |candidates| {
                    Self::task_value_origins(candidates, actuals, projections, environments)
                }),
            CallableCandidate::CallResult(_) | CallableCandidate::CallWriteback(_) => {
                self.task_call_origins(candidate, actuals, projections, environments)
            }
            CallableCandidate::Field {
                receiver,
                owner,
                index,
            } => {
                let mut path = projections.to_vec();
                if !descendants {
                    path.push(Projection::Field(*owner, *index));
                }
                environments.origins.read(receiver, &path, actuals)
            }
            CallableCandidate::Element { receiver, index } => {
                let mut path = projections.to_vec();
                if !descendants {
                    path.push(Projection::Element(*index));
                }
                environments.origins.read(receiver, &path, actuals)
            }
            CallableCandidate::TaskResult(task) => {
                let mut path = projections.to_vec();
                if !descendants {
                    path.push(Projection::TaskResult);
                }
                environments.origins.read(task, &path, actuals)
            }
            CallableCandidate::Sequence(values) => {
                if let Some((Projection::Element(index), rest)) = projections.split_last() {
                    values.get(*index).map_or_else(Vec::new, |value| {
                        Self::task_value_origins(value, actuals, rest, environments)
                    })
                } else {
                    values
                        .iter()
                        .flat_map(|value| {
                            Self::task_value_origins(value, actuals, projections, environments)
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
                            Self::task_value_origins(
                                &field.candidates,
                                actuals,
                                if descendants {
                                    projections
                                } else {
                                    selected.map_or(projections, |(_, rest)| rest)
                                },
                                environments,
                            )
                        })
                        .collect()
                }),
            CallableCandidate::TaskProducer(site) if descendants => {
                let mut origins = vec![ValueOrigin {
                    candidate: candidate.clone(),
                    actuals,
                }];
                if let Some((_, result)) = self.task_lifetimes.producers.get(site) {
                    origins.extend(Self::task_value_origins(
                        result,
                        actuals,
                        projections,
                        environments,
                    ));
                }
                origins
            }
            CallableCandidate::TaskProducer(site)
                if projections.last() == Some(&Projection::TaskResult) =>
            {
                self.task_lifetimes
                    .producers
                    .get(site)
                    .map_or_else(Vec::new, |(_, result)| {
                        Self::task_value_origins(
                            result,
                            actuals,
                            &projections[..projections.len() - 1],
                            environments,
                        )
                    })
            }
            CallableCandidate::TaskProducer(_)
            | CallableCandidate::Closure(_)
            | CallableCandidate::Declaration(_)
                if projections.is_empty() || descendants =>
            {
                vec![ValueOrigin {
                    candidate: candidate.clone(),
                    actuals,
                }]
            }
            _ => Vec::new(),
        }
    }

    fn task_call_origins(
        &self,
        candidate: &CallableCandidate,
        actuals: usize,
        projections: &[Projection],
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
        Self::task_value_origins(&callees, actuals, &[], environments)
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
                let arguments = self.task_call_arguments(&owner, site, pending, actuals);
                let call_actuals =
                    Self::task_call_context(&owner, callee.actuals, arguments, environments);
                Self::task_value_origins(returns, call_actuals, projections, environments)
            })
            .collect()
    }

    fn task_call_arguments(
        &self,
        owner: &EffectBody,
        site: &SpanKey,
        pending: &super::types::PendingCallableArguments,
        actuals: usize,
    ) -> Vec<(TypeBindingId, ActualSource)> {
        let mut arguments = Vec::new();
        if let Some(formals) = self.callable_formals.get(owner) {
            let offset = usize::from(
                pending.receiver.is_some() && formals.len() == pending.arguments.len() + 1,
            );
            if offset != 0 {
                arguments.push((
                    formals[0],
                    ActualSource {
                        candidates: pending.receiver.clone().expect("receiver offset"),
                        actuals,
                    },
                ));
            }
            for (index, actual) in pending.arguments.iter().enumerate() {
                let slot = self
                    .call_argument_slots
                    .get(site)
                    .map_or(index, |slots| slots[index])
                    + offset;
                if let Some(formal) = formals.get(slot) {
                    arguments.push((
                        *formal,
                        ActualSource {
                            candidates: actual.clone(),
                            actuals,
                        },
                    ));
                }
            }
        }
        arguments
    }

    fn task_call_context(
        owner: &EffectBody,
        actuals: usize,
        arguments: Vec<(TypeBindingId, ActualSource)>,
        environments: &mut OriginEnvironments,
    ) -> usize {
        let current = environments.origins.current.expect("call origin transfer");
        let context_key = (owner.clone(), actuals);
        if let Some(context) = environments
            .origins
            .call_contexts
            .get(&current)
            .and_then(|contexts| contexts.get(&context_key))
        {
            return *context;
        }
        let context =
            if let Some(context) = environments.origins.recursive_call_context(owner, current) {
                // Recursive applications add incoming edges to the existing context.
                // Caller bindings and captured lexical environments retain their
                // identities instead of being overwritten by a new invocation.
                for (formal, source) in arguments {
                    if environments
                        .recursive_sources
                        .entry((formal, context))
                        .or_default()
                        .insert(source)
                    {
                        environments.origins.invalidate_formal(formal, context);
                    }
                }
                if matches!(owner, EffectBody::Closure(_))
                    && actuals != context
                    && environments
                        .recursive_parents
                        .entry(context)
                        .or_default()
                        .insert(actuals)
                {
                    environments.origins.invalidate_environment(context);
                }
                context
            } else {
                let parent = matches!(owner, EffectBody::Closure(_)).then_some(actuals);
                environments.intern(arguments.into_iter().collect(), parent)
            };
        environments
            .origins
            .call_contexts
            .entry(current)
            .or_default()
            .insert(context_key, context);
        context
    }
}
