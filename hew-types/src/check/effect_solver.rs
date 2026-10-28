//! Instantiate effect dependencies once and propagate suspension to their users.

use super::super::task_lifetimes::OriginEnvironments;
use super::super::CallableCandidate;
use super::{
    CallTarget, Checker, EffectBody, EffectCallSite, EffectGraph, Invocation, MethodCallRewrite,
    SpanKey,
};
use crate::check::cleanup_effects::{CleanupSession, CleanupTarget};
use crate::{ParamHead, Ty};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet, VecDeque};

type Actuals = HashMap<ParamHead, Ty>;

#[derive(Clone, Copy)]
struct Application<'a> {
    types: &'a Actuals,
    values: usize,
    unknown: bool,
}

fn contains_type(ty: &Ty, needle: &Ty) -> bool {
    let mut pending = vec![ty];
    while let Some(ty) = pending.pop() {
        if ty == needle {
            return true;
        }
        ty.for_each_child(&mut |child| pending.push(child));
    }
    false
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Demand {
    Cleanup,
    Exact,
}

type Parameters = HashMap<ParamHead, Demand>;

fn type_parameters(ty: &Ty, parameters: &mut Parameters, demand: Demand) {
    let mut pending = vec![ty];
    while let Some(ty) = pending.pop() {
        if let Ty::Named {
            head: crate::TypeHead::Param(parameter),
            ..
        } = ty
        {
            parameters
                .entry(*parameter)
                .and_modify(|current| *current = (*current).max(demand))
                .or_insert(demand);
        }
        ty.for_each_child(&mut |child| pending.push(child));
    }
}

fn cleanup_parameters(targets: impl IntoIterator<Item = CleanupTarget>) -> Parameters {
    let mut parameters = Parameters::new();
    for target in targets {
        match target {
            CleanupTarget::Body { type_args, .. } => {
                // A user-defined close can dispatch on the concrete receiver.
                for ty in type_args {
                    type_parameters(&ty, &mut parameters, Demand::Exact);
                }
            }
            CleanupTarget::Unknown(ty) => type_parameters(&ty, &mut parameters, Demand::Cleanup),
            CleanupTarget::Callable { .. } | CleanupTarget::Suspends => {}
        }
    }
    parameters
}

struct ParameterTransfer {
    owner: EffectBody,
    actuals: Actuals,
    inherited: bool,
}

/// Demand flows backwards through checked substitutions. Cleanup-only
/// parameters need their release effects; executable dispatch retains types.
fn effect_parameters(
    checker: &CleanupSession<'_>,
    graph: &EffectGraph,
) -> (HashMap<EffectBody, Parameters>, HashSet<EffectBody>) {
    let mut parameters: HashMap<EffectBody, Parameters> = HashMap::new();
    let mut values = HashSet::new();
    let mut users: HashMap<EffectBody, Vec<ParameterTransfer>> = HashMap::new();
    let empty = Actuals::new();
    for release in &graph.releases {
        let targets = checker.cleanup_effect_targets(release, &empty, true);
        if targets.iter().any(|target| {
            matches!(
                target,
                CleanupTarget::Callable { .. } | CleanupTarget::Body { .. }
            )
        }) {
            values.insert(release.owner.clone());
        }
        for (parameter, demand) in cleanup_parameters(targets) {
            parameters
                .entry(release.owner.clone())
                .or_default()
                .entry(parameter)
                .and_modify(|current| *current = (*current).max(demand))
                .or_insert(demand);
        }
    }
    for body in graph.once_releases.keys() {
        let targets = checker.once_cleanup_targets(body, graph, &empty, true);
        if targets.iter().any(|target| {
            matches!(
                target,
                CleanupTarget::Callable { .. } | CleanupTarget::Body { .. }
            )
        }) {
            values.insert(body.clone());
        }
        for (parameter, demand) in cleanup_parameters(targets) {
            parameters
                .entry(body.clone())
                .or_default()
                .entry(parameter)
                .and_modify(|current| *current = (*current).max(demand))
                .or_insert(demand);
        }
    }
    for (site, call) in &graph.calls {
        let Some(owner) = &call.owner else {
            continue;
        };
        if matches!(
            checker.effect_call_target(site),
            Some(CallTarget::IndirectFunctionValue | CallTarget::StaticTraitMethod { .. })
        ) || (checker.effect_call_target(site).is_none()
            && matches!(
                site.authored_span()
                    .and_then(|key| checker.method_call_rewrites.get(key)),
                Some(
                    MethodCallRewrite::RecordFnFieldCall { .. }
                        | MethodCallRewrite::BuiltinVecHigherOrder { .. }
                )
            ))
        {
            values.insert(owner.clone());
        }
        let target = call_parameter_transfer(
            checker,
            site,
            call,
            parameters.entry(owner.clone()).or_default(),
        );
        if let Some((body, actuals, inherited)) = target {
            users.entry(body).or_default().push(ParameterTransfer {
                owner: owner.clone(),
                actuals,
                inherited,
            });
        }
    }
    propagate_parameter_demands(checker, &mut parameters, &users);
    let mut pending: Vec<_> = values.iter().cloned().collect();
    while let Some(body) = pending.pop() {
        for user in users.get(&body).into_iter().flatten() {
            if values.insert(user.owner.clone()) {
                pending.push(user.owner.clone());
            }
        }
    }
    (parameters, values)
}

fn call_parameter_transfer(
    checker: &CleanupSession<'_>,
    site: &EffectCallSite,
    call: &Invocation,
    parameters: &mut Parameters,
) -> Option<(EffectBody, Actuals, bool)> {
    let empty = Actuals::new();
    if matches!(
        checker.effect_call_target(site),
        Some(CallTarget::StaticTraitMethod { .. })
    ) {
        if let Some(receiver) = &call.receiver {
            type_parameters(&checker.subst.resolve(receiver), parameters, Demand::Exact);
        }
        if let Some(trait_args) = checker.effect_trait_args(site) {
            for ty in trait_args {
                type_parameters(ty, parameters, Demand::Exact);
            }
        }
    }
    match checker.effect_call_target(site) {
        Some(
            CallTarget::User(id)
            | CallTarget::ImplMethod(id)
            | CallTarget::StaticTraitMethod { method: id, .. },
        ) => Some((
            EffectBody::Declaration(*id),
            checker.call_cleanup_substitutions(*id, site, call.receiver.as_ref(), &empty),
            false,
        )),
        target
            if matches!(target, Some(CallTarget::IndirectFunctionValue))
                || (target.is_none()
                    && matches!(
                        site.authored_span()
                            .and_then(|key| checker.method_call_rewrites.get(key)),
                        Some(
                            MethodCallRewrite::RecordFnFieldCall { .. }
                                | MethodCallRewrite::BuiltinVecHigherOrder { .. }
                        )
                    )) =>
        {
            call.callee.as_ref().and_then(|ty| {
                let ty = checker.subst.resolve(ty);
                type_parameters(&ty, parameters, Demand::Exact);
                match ty.callable_body() {
                    Ty::Closure { identity, .. } => Some((identity.clone(), Actuals::new(), true)),
                    _ => None,
                }
            })
        }
        _ => None,
    }
}

fn propagate_parameter_demands(
    checker: &CleanupSession<'_>,
    parameters: &mut HashMap<EffectBody, Parameters>,
    users: &HashMap<EffectBody, Vec<ParameterTransfer>>,
) {
    let empty = Actuals::new();
    let mut pending: VecDeque<_> = parameters
        .iter()
        .flat_map(|(body, parameters)| {
            parameters
                .iter()
                .map(|(parameter, demand)| (body.clone(), *parameter, *demand))
        })
        .collect();
    while let Some((body, parameter, demand)) = pending.pop_front() {
        if let Some(users) = users.get(&body) {
            for user in users {
                let mut demanded = Parameters::new();
                if let Some(ty) = user.actuals.get(&parameter) {
                    if demand == Demand::Exact {
                        type_parameters(ty, &mut demanded, Demand::Exact);
                    } else {
                        demanded =
                            cleanup_parameters(checker.cleanup_type_targets(ty, &empty, true));
                    }
                } else if user.inherited {
                    demanded.insert(parameter, demand);
                }
                for (parameter, demand) in demanded {
                    let current = parameters.entry(user.owner.clone()).or_default();
                    if current
                        .get(&parameter)
                        .is_none_or(|previous| *previous < demand)
                    {
                        current.insert(parameter, demand);
                        pending.push_back((user.owner.clone(), parameter, demand));
                    }
                }
            }
        }
    }
}

fn cleanup_argument(checker: &CleanupSession<'_>, ty: &Ty) -> Ty {
    let mut pending = vec![ty];
    let mut parts = Vec::new();
    let mut seen = HashSet::new();
    while let Some(ty) = pending.pop() {
        match ty {
            Ty::Tuple(fields) => pending.extend(fields.iter().rev()),
            Ty::Array(element, size) if *size != 0 => pending.push(element),
            Ty::Named { head, args }
                if matches!(
                    head.builtin(),
                    Some(
                        crate::BuiltinType::Vec
                            | crate::BuiltinType::VecIter
                            | crate::BuiltinType::HashMapIter
                            | crate::BuiltinType::HashMap
                            | crate::BuiltinType::HashSet
                            | crate::BuiltinType::Rc
                            | crate::BuiltinType::Option
                            | crate::BuiltinType::Result
                    )
                ) =>
            {
                pending.extend(args.iter().rev());
            }
            Ty::Var(_) | Ty::AssocType { .. } => {
                if seen.insert(ty.clone()) {
                    parts.push(ty.clone());
                }
            }
            _ => {
                if !checker
                    .cleanup_type_targets(ty, &Actuals::new(), true)
                    .is_empty()
                    && seen.insert(ty.clone())
                {
                    parts.push(ty.clone());
                }
            }
        }
    }
    match parts.len() {
        0 => Ty::Unit,
        1 => parts.pop().expect("one cleanup component"),
        _ => Ty::Tuple(parts),
    }
}

fn body_order(body: &EffectBody) -> (u8, u32, usize, usize) {
    match body {
        EffectBody::Declaration(id) => (0, id.index_u32(), 0, 0),
        EffectBody::Generator(id) => (1, id.index_u32(), 0, 0),
        EffectBody::Closure(key) => (2, key.module_idx, key.start, key.end),
        EffectBody::GeneratorBlock(key) => (3, key.module_idx, key.start, key.end),
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct Instance {
    body: EffectBody,
    arguments: Vec<(ParamHead, Ty)>,
    unknown: bool,
    captures: bool,
    values: usize,
}

#[derive(Default)]
struct Node {
    users: Vec<(usize, String)>,
    witness: Option<String>,
    parent: Option<usize>,
    expanding: bool,
}

#[derive(Default)]
struct Dependencies {
    intrinsic: Option<String>,
    bodies: Vec<(Instance, String)>,
}

impl Dependencies {
    fn suspends(&mut self, witness: &str) {
        self.intrinsic.get_or_insert_with(|| witness.to_string());
    }

    fn body(&mut self, instance: Instance, witness: &str) {
        self.bodies.push((instance, witness.to_string()));
    }
}

/// All inputs are frozen after body checking. Node identities include the
/// substitution and unknown-cleanup policy; answers never leak between them.
pub(super) struct EffectSolver<'a> {
    checker: CleanupSession<'a>,
    graph: &'a EffectGraph,
    calls: HashMap<EffectBody, Vec<(&'a EffectCallSite, &'a Invocation)>>,
    releases: HashMap<EffectBody, Vec<&'a super::super::cleanup_effects::CleanupEffect>>,
    parameters: HashMap<EffectBody, Parameters>,
    value_parameters: HashSet<EffectBody>,
    identities: HashMap<Instance, usize>,
    body_instances: HashSet<(EffectBody, bool, bool)>,
    instances: Vec<Instance>,
    nodes: Vec<Node>,
    expanded: usize,
    changed: VecDeque<usize>,
    values: RefCell<OriginEnvironments>,
}

impl<'a> EffectSolver<'a> {
    pub(super) fn new(
        checker: &'a Checker,
        graph: &'a EffectGraph,
        flows: &HashMap<SpanKey, Vec<super::super::CallableArgumentFlow>>,
    ) -> Self {
        let checker = CleanupSession::new(checker);
        let mut calls: HashMap<_, Vec<_>> = HashMap::new();
        for (site, invocation) in &graph.calls {
            if let Some(owner) = &invocation.owner {
                calls
                    .entry(owner.clone())
                    .or_default()
                    .push((site, invocation));
            }
        }
        // Source order makes the selected witness stable across hash seeds.
        for calls in calls.values_mut() {
            calls.sort_by_key(|(site, _)| {
                let key = site.diagnostic_span();
                (key.module_idx, key.start, key.end, site.order())
            });
        }
        let mut releases: HashMap<_, Vec<_>> = HashMap::new();
        for release in &graph.releases {
            releases
                .entry(release.owner.clone())
                .or_default()
                .push(release);
        }
        for releases in releases.values_mut() {
            releases.sort_by(|left, right| left.witness.cmp(&right.witness));
        }
        let (parameters, value_parameters) = effect_parameters(&checker, graph);
        let mut solver = Self {
            checker,
            graph,
            calls,
            releases,
            parameters,
            value_parameters,
            identities: HashMap::new(),
            body_instances: HashSet::new(),
            instances: Vec::new(),
            nodes: Vec::new(),
            expanded: 0,
            changed: VecDeque::new(),
            values: RefCell::new(Checker::value_environments(flows)),
        };
        // Seed roots before answering any query, so hash-map iteration cannot
        // change which suspending dependency supplies a diagnostic witness.
        let mut bodies: Vec<_> = graph.bodies.keys().collect();
        bodies.sort_by_key(|body| body_order(body));
        for body in bodies {
            for unknown in [false, true] {
                solver.intern(Self::instance(body, &Actuals::new(), unknown, false));
            }
        }
        solver
    }

    fn instance(body: &EffectBody, actuals: &Actuals, unknown: bool, captures: bool) -> Instance {
        let mut arguments: Vec<_> = actuals.iter().map(|(key, ty)| (*key, ty.clone())).collect();
        arguments.sort_by_key(|(key, _)| key.id);
        Instance {
            body: body.clone(),
            arguments,
            unknown,
            captures,
            values: 0,
        }
    }

    fn intern(&mut self, instance: Instance) -> usize {
        self.intern_dependency(instance, None)
    }

    fn intern_dependency(&mut self, mut instance: Instance, parent: Option<usize>) -> usize {
        instance.arguments.retain_mut(|(parameter, ty)| {
            match self
                .parameters
                .get(&instance.body)
                .and_then(|parameters| parameters.get(parameter))
            {
                Some(Demand::Cleanup) => {
                    *ty = cleanup_argument(&self.checker, ty);
                    true
                }
                Some(Demand::Exact) => true,
                None => false,
            }
        });
        if !self.value_parameters.contains(&instance.body)
            && !instance.arguments.iter().any(|(_, ty)| {
                self.checker
                    .cleanup_type_targets(ty, &Actuals::new(), true)
                    .iter()
                    .any(|target| {
                        matches!(
                            target,
                            CleanupTarget::Callable { .. } | CleanupTarget::Body { .. }
                        )
                    })
            })
        {
            instance.values = 0;
        }
        if let Some(id) = self.identities.get(&instance) {
            return *id;
        }
        let repeated = !self.body_instances.insert((
            instance.body.clone(),
            instance.unknown,
            instance.captures,
        ));
        let mut ancestor = if repeated && (!instance.arguments.is_empty() || instance.values != 0) {
            parent
        } else {
            None
        };
        let mut expanding = false;
        while let Some(id) = ancestor {
            let previous = &self.instances[id];
            if previous.body == instance.body
                && previous.unknown == instance.unknown
                && previous.captures == instance.captures
                && self
                    .values
                    .borrow()
                    .expands(instance.values, previous.values)
            {
                expanding = true;
                break;
            }
            if previous.body == instance.body
                && previous.unknown == instance.unknown
                && previous.captures == instance.captures
                && previous.arguments.len() == instance.arguments.len()
                && previous.arguments != instance.arguments
                && previous.arguments.iter().zip(&instance.arguments).all(
                    |((old_parameter, old), (parameter, ty))| {
                        old_parameter == parameter && contains_type(ty, old)
                    },
                )
            {
                expanding = true;
                break;
            }
            ancestor = self.nodes[id].parent;
        }
        let id = self.nodes.len();
        self.identities.insert(instance.clone(), id);
        self.instances.push(instance);
        self.nodes.push(Node {
            parent,
            expanding,
            ..Node::default()
        });
        id
    }

    fn mark(&mut self, id: usize, witness: String) {
        if self.nodes[id].witness.is_none() {
            self.nodes[id].witness = Some(witness);
            self.changed.push_back(id);
        }
    }

    /// A boolean least fixed point: cycles start false and become true only
    /// through a suspending dependency. Each node and edge is processed once.
    fn solve(&mut self) {
        while self.expanded < self.nodes.len() {
            let id = self.expanded;
            self.expanded += 1;
            if self.nodes[id].expanding {
                // Polymorphic recursion can require infinitely many concrete
                // instances. Preserve safety with a conservative effect at
                // that edge; irrelevant arguments have already been erased.
                self.mark(
                    id,
                    "cleanup of recursively expanding type arguments".to_string(),
                );
                continue;
            }
            let instance = self.instances[id].clone();
            let dependencies = self.dependencies(&instance);
            if let Some(witness) = dependencies.intrinsic {
                self.mark(id, witness);
                continue;
            }
            for (dependency, witness) in dependencies.bodies {
                let dependency = self.intern_dependency(dependency, Some(id));
                self.nodes[dependency].users.push((id, witness.clone()));
                if self.nodes[dependency].witness.is_some() {
                    self.mark(id, witness);
                }
            }
        }
        while let Some(id) = self.changed.pop_front() {
            for (user, witness) in self.nodes[id].users.clone() {
                self.mark(user, witness);
            }
        }
    }

    pub(super) fn body(&mut self, body: &EffectBody, unknown: bool) -> Option<String> {
        let id = self.intern(Self::instance(body, &Actuals::new(), unknown, false));
        self.solve();
        self.nodes[id].witness.clone()
    }

    pub(super) fn invocation(&mut self, site: &EffectCallSite, invocation: &Invocation) -> bool {
        let mut dependencies = Dependencies::default();
        self.invocation_dependencies(
            site,
            invocation,
            Application {
                types: &Actuals::new(),
                values: 0,
                unknown: false,
            },
            &mut dependencies,
        );
        let ids: Vec<_> = dependencies
            .bodies
            .into_iter()
            .map(|(instance, _)| self.intern(instance))
            .collect();
        self.solve();
        dependencies.intrinsic.is_some()
            || ids.into_iter().any(|id| self.nodes[id].witness.is_some())
    }

    pub(super) fn obligation(
        &mut self,
        obligation: &super::SuspensionObligation,
    ) -> Vec<(SpanKey, Option<String>, String)> {
        let contexts = obligation.owner.as_ref().map_or_else(
            || vec![(Actuals::new(), None)],
            |owner| {
                self.checker
                    .effect_construction_contexts(owner, self.graph, &mut HashSet::new())
            },
        );
        let mut instances = Vec::new();
        for (mut actuals, boundary) in contexts {
            if let (
                EffectBody::Declaration(declaration),
                super::ObligationSlot::Dyn { receiver, .. },
            ) = (&obligation.body, &obligation.slot)
            {
                let receiver_actuals =
                    self.checker
                        .receiver_cleanup_substitutions(*declaration, receiver, &actuals);
                actuals.extend(receiver_actuals);
                self.checker
                    .complete_associated_schema_substitutions(&mut actuals);
            }
            let body = self.intern(Self::instance(&obligation.body, &actuals, true, false));
            let captures = obligation
                .once
                .then(|| self.intern(Self::instance(&obligation.body, &actuals, true, true)));
            instances.push((boundary, body, captures));
        }
        self.solve();
        instances
            .into_iter()
            .filter_map(|(boundary, body, captures)| {
                let witness = self.nodes[body]
                    .witness
                    .as_ref()
                    .or_else(|| captures.and_then(|id| self.nodes[id].witness.as_ref()))?
                    .clone();
                let (key, source) = boundary
                    .unwrap_or_else(|| (obligation.key.clone(), obligation.source_module.clone()));
                Some((key, source, witness))
            })
            .collect()
    }

    fn dependencies(&self, instance: &Instance) -> Dependencies {
        let mut result = Dependencies::default();
        let actuals = instance.arguments.iter().cloned().collect();
        if instance.captures {
            for target in self.checker.once_cleanup_targets(
                &instance.body,
                self.graph,
                &actuals,
                instance.unknown,
            ) {
                self.cleanup_dependency(
                    target,
                    Application {
                        types: &actuals,
                        values: instance.values,
                        unknown: instance.unknown,
                    },
                    "implicit release of captured values",
                    &mut result,
                );
            }
            return result;
        }
        if self
            .graph
            .bodies
            .get(&instance.body)
            .copied()
            .unwrap_or(true)
        {
            result.suspends(
                self.graph
                    .witnesses
                    .get(&instance.body)
                    .map_or("an unchecked body", String::as_str),
            );
        }
        if let Some(releases) = self.releases.get(&instance.body) {
            for release in releases {
                for target in
                    self.checker
                        .cleanup_effect_targets(release, &actuals, instance.unknown)
                {
                    self.cleanup_dependency(
                        target,
                        Application {
                            types: &actuals,
                            values: instance.values,
                            unknown: instance.unknown,
                        },
                        &release.witness,
                        &mut result,
                    );
                }
            }
        }
        if let Some(calls) = self.calls.get(&instance.body) {
            for (site, invocation) in calls {
                self.invocation_dependencies(
                    site,
                    invocation,
                    Application {
                        types: &actuals,
                        values: instance.values,
                        unknown: instance.unknown,
                    },
                    &mut result,
                );
            }
        }
        result
    }

    fn cleanup_dependency(
        &self,
        target: CleanupTarget,
        application: Application<'_>,
        witness: &str,
        result: &mut Dependencies,
    ) {
        self.cleanup_dependency_inner(target, application, witness, result, &mut HashSet::new());
    }

    fn cleanup_body_instance(
        &self,
        body: &EffectBody,
        type_args: &[Ty],
        candidates: super::super::IndirectCallCandidates,
        application: Application<'_>,
    ) -> Instance {
        let actuals = match body {
            EffectBody::Declaration(id) => self
                .checker
                .fn_sigs
                .get(id)
                .and_then(|signature| signature.impl_method.as_ref())
                .and_then(|method| {
                    crate::method_resolution::impl_receiver_substitution(
                        &method.receiver_parameters,
                        &method.receiver_args,
                        type_args,
                    )
                })
                .unwrap_or_default(),
            EffectBody::Closure(_) | EffectBody::Generator(_) | EffectBody::GeneratorBlock(_) => {
                Actuals::new()
            }
        };
        let mut applied = application.types.clone();
        applied.extend(actuals);
        let mut instance = Self::instance(body, &applied, application.unknown, false);
        instance.values = self.checker.callable_environment(
            body,
            None,
            (&applied, application.values),
            Some(candidates),
            &mut self.values.borrow_mut(),
        );
        instance
    }

    fn cleanup_dependency_inner(
        &self,
        target: CleanupTarget,
        application: Application<'_>,
        witness: &str,
        result: &mut Dependencies,
        active: &mut HashSet<(SpanKey, usize)>,
    ) {
        let Application {
            types: enclosing,
            values,
            ..
        } = application;
        match target {
            CleanupTarget::Body {
                body,
                type_args,
                candidates,
                ..
            } => {
                result.body(
                    self.cleanup_body_instance(&body, &type_args, candidates, application),
                    witness,
                );
            }
            CleanupTarget::Callable { ty, candidates } => {
                if let Ty::Closure { identity, .. } = ty.callable_body() {
                    let mut instance = Self::instance(identity, enclosing, true, true);
                    instance.values = values;
                    result.body(instance, witness);
                    return;
                }
                let erased = matches!(ty.callable_body(), Ty::TraitObject { .. });
                let (origins, complete) = if erased {
                    self.checker.resolve_erased_values(
                        &candidates,
                        values,
                        &mut self.values.borrow_mut(),
                    )
                } else {
                    self.checker.resolve_callable_values(
                        &candidates,
                        values,
                        &mut self.values.borrow_mut(),
                    )
                };
                if !complete || origins.is_empty() {
                    result.suspends(witness);
                }
                for origin in origins {
                    match origin.candidate {
                        CallableCandidate::Declaration(_) if !erased => {}
                        CallableCandidate::Erasure { site, value } => {
                            let key = (site.clone(), origin.actuals);
                            if !active.insert(key.clone()) {
                                result.suspends(witness);
                                continue;
                            }
                            if let Some(coercion) = self.checker.dyn_trait_coercions.get(&site) {
                                for target in self.checker.cleanup_known_value_targets(
                                    &coercion.concrete_type,
                                    *value,
                                    enclosing,
                                    true,
                                ) {
                                    self.cleanup_dependency_inner(
                                        target,
                                        Application {
                                            values: origin.actuals,
                                            ..application
                                        },
                                        witness,
                                        result,
                                        active,
                                    );
                                }
                            } else {
                                result.suspends(witness);
                            }
                            active.remove(&key);
                        }
                        CallableCandidate::Closure(site) if !erased => {
                            let mut instance =
                                Self::instance(&EffectBody::Closure(site), enclosing, true, true);
                            instance.values = origin.actuals;
                            result.body(instance, witness);
                        }
                        _ => result.suspends(witness),
                    }
                }
            }
            CleanupTarget::Unknown(_) | CleanupTarget::Suspends => {
                result.suspends(witness);
            }
        }
    }

    fn callee_dependencies(
        &self,
        site: &EffectCallSite,
        invocation: &Invocation,
        application: Application<'_>,
        result: &mut Dependencies,
    ) {
        let Application {
            types: actuals,
            values,
            unknown,
        } = application;
        match invocation.callee.as_ref().map(|ty| {
            self.checker
                .subst
                .resolve(ty)
                .substitute_type_params_parallel(actuals)
                .callable_body()
                .clone()
        }) {
            Some(Ty::Function { capabilities, .. }) => {
                if capabilities.suspends {
                    result.suspends(&invocation.name);
                }
            }
            Some(Ty::Closure {
                identity,
                capabilities,
                ..
            }) => {
                let mut instance = Self::instance(&identity, actuals, unknown, false);
                instance.values = self.checker.callable_environment(
                    &identity,
                    site.authored_span(),
                    (actuals, values),
                    None,
                    &mut self.values.borrow_mut(),
                );
                result.body(instance, &invocation.name);
                if capabilities.call == crate::CallableCallMode::Once {
                    let mut instance = Self::instance(&identity, actuals, unknown, true);
                    instance.values = values;
                    result.body(instance, &invocation.name);
                }
            }
            _ => result.suspends(&invocation.name),
        }
    }

    fn invocation_dependencies(
        &self,
        site: &EffectCallSite,
        invocation: &Invocation,
        application: Application<'_>,
        result: &mut Dependencies,
    ) {
        let Application {
            types: actuals,
            values,
            unknown,
        } = application;
        if let Some(suspends) = site
            .authored_span()
            .and_then(|key| self.graph.submission_effects.get(key))
        {
            if *suspends {
                result.suspends(&invocation.name);
            }
            return;
        }
        match self.checker.effect_call_target(site) {
            Some(target) if self.graph.builtin_suspensions.contains(target) => {
                result.suspends(&invocation.name);
            }
            Some(CallTarget::IndirectFunctionValue) => {
                self.callee_dependencies(site, invocation, application, result);
            }
            Some(
                CallTarget::User(_)
                | CallTarget::ImplMethod(_)
                | CallTarget::StaticTraitMethod { .. },
            ) => {
                let Some((id, applied)) = self
                    .checker
                    .instantiated_effect_target(site, invocation, actuals)
                else {
                    result.suspends(&invocation.name);
                    return;
                };
                let body = EffectBody::Declaration(id);
                let mut instance = Self::instance(&body, &applied, unknown, false);
                instance.values = self.checker.callable_environment(
                    &body,
                    site.authored_span(),
                    (&applied, values),
                    None,
                    &mut self.values.borrow_mut(),
                );
                result.body(instance, &invocation.name);
            }
            Some(CallTarget::Runtime(family) | CallTarget::DeclaredRuntime { family, .. }) => {
                if family.is_async_suspending().is_some() {
                    result.suspends(&invocation.name);
                }
            }
            Some(CallTarget::Extern { declaration, .. }) => {
                if self.checker.extern_table.is_offload(*declaration) {
                    result.suspends(&invocation.name);
                }
            }
            Some(
                CallTarget::RecordConstructor(_)
                | CallTarget::RangeAdapter(_)
                | CallTarget::Builtin { .. }
                | CallTarget::RuntimeCollection(_),
            ) => {}
            Some(CallTarget::DynamicVtable { .. }) => {
                if site
                    .authored_span()
                    .and_then(|key| self.checker.dyn_trait_method_calls.get(key))
                    .is_none_or(|call| call.effect == super::super::SlotEffect::Suspends)
                {
                    result.suspends(&invocation.name);
                }
            }
            Some(CallTarget::Unsupported { .. }) => result.suspends(&invocation.name),
            None => match site
                .authored_span()
                .and_then(|key| self.checker.method_call_rewrites.get(key))
            {
                Some(
                    MethodCallRewrite::RecordFnFieldCall { .. }
                    | MethodCallRewrite::BuiltinVecHigherOrder { .. },
                ) => self.callee_dependencies(site, invocation, application, result),
                Some(MethodCallRewrite::GeneratorNext { .. }) => result.suspends(&invocation.name),
                Some(_) => {}
                None => {
                    if !matches!(
                        site.authored_span()
                            .and_then(|key| self.checker.actor_method_dispatch.get(key)),
                        Some(super::super::ActorMethodKind::Message { .. })
                    ) {
                        result.suspends(&invocation.name);
                    }
                }
            },
        }
    }
}
