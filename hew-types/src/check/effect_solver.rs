//! Instantiate effect dependencies once and propagate suspension to their users.

use super::{CallTarget, Checker, EffectBody, EffectGraph, Invocation, MethodCallRewrite, SpanKey};
use crate::check::cleanup_effects::CleanupTarget;
use crate::{ParamHead, Ty};
use std::collections::{HashMap, HashSet, VecDeque};

type Actuals = HashMap<ParamHead, Ty>;

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
            CleanupTarget::Callable(_) | CleanupTarget::Suspends => {}
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
fn effect_parameters(checker: &Checker, graph: &EffectGraph) -> HashMap<EffectBody, Parameters> {
    let mut parameters: HashMap<EffectBody, Parameters> = HashMap::new();
    let mut users: HashMap<EffectBody, Vec<ParameterTransfer>> = HashMap::new();
    let empty = Actuals::new();
    for release in &graph.releases {
        for (parameter, demand) in
            cleanup_parameters(checker.cleanup_effect_targets(release, &empty, true))
        {
            parameters
                .entry(release.owner.clone())
                .or_default()
                .entry(parameter)
                .and_modify(|current| *current = (*current).max(demand))
                .or_insert(demand);
        }
    }
    for body in graph.once_releases.keys() {
        for (parameter, demand) in
            cleanup_parameters(checker.once_cleanup_targets(body, graph, &empty, true))
        {
            parameters
                .entry(body.clone())
                .or_default()
                .entry(parameter)
                .and_modify(|current| *current = (*current).max(demand))
                .or_insert(demand);
        }
    }
    for (key, call) in &graph.calls {
        let Some(owner) = &call.owner else {
            continue;
        };
        let target = match checker.call_target(key) {
            Some(
                CallTarget::User(id)
                | CallTarget::ImplMethod(id)
                | CallTarget::StaticTraitMethod { method: id, .. },
            ) => Some((
                EffectBody::Declaration(*id),
                checker.call_cleanup_substitutions(*id, key, call.receiver.as_ref(), &empty),
                false,
            )),
            target
                if matches!(target, Some(CallTarget::IndirectFunctionValue))
                    || (target.is_none()
                        && matches!(
                            checker.method_call_rewrites.get(key),
                            Some(
                                MethodCallRewrite::RecordFnFieldCall { .. }
                                    | MethodCallRewrite::BuiltinVecHigherOrder { .. }
                            )
                        )) =>
            {
                call.callee.as_ref().and_then(|ty| {
                    let ty = checker.subst.resolve(ty);
                    type_parameters(
                        &ty,
                        parameters.entry(owner.clone()).or_default(),
                        Demand::Exact,
                    );
                    match ty.callable_body() {
                        Ty::Closure { identity, .. } => {
                            Some((identity.clone(), Actuals::new(), true))
                        }
                        _ => None,
                    }
                })
            }
            _ => None,
        };
        if let Some((body, actuals, inherited)) = target {
            users.entry(body).or_default().push(ParameterTransfer {
                owner: owner.clone(),
                actuals,
                inherited,
            });
        }
    }
    propagate_parameter_demands(checker, &mut parameters, &users);
    parameters
}

fn propagate_parameter_demands(
    checker: &Checker,
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

fn cleanup_argument(checker: &Checker, ty: &Ty) -> Ty {
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
    checker: &'a Checker,
    graph: &'a EffectGraph,
    calls: HashMap<EffectBody, Vec<(&'a SpanKey, &'a Invocation)>>,
    releases: HashMap<EffectBody, Vec<&'a super::super::cleanup_effects::CleanupEffect>>,
    parameters: HashMap<EffectBody, Parameters>,
    identities: HashMap<Instance, usize>,
    body_instances: HashSet<(EffectBody, bool, bool)>,
    instances: Vec<Instance>,
    nodes: Vec<Node>,
    expanded: usize,
    changed: VecDeque<usize>,
}

impl<'a> EffectSolver<'a> {
    pub(super) fn new(checker: &'a Checker, graph: &'a EffectGraph) -> Self {
        let mut calls: HashMap<_, Vec<_>> = HashMap::new();
        for (key, invocation) in &graph.calls {
            if let Some(owner) = &invocation.owner {
                calls
                    .entry(owner.clone())
                    .or_default()
                    .push((key, invocation));
            }
        }
        // Source order makes the selected witness stable across hash seeds.
        for calls in calls.values_mut() {
            calls.sort_by_key(|(key, _)| (key.module_idx, key.start, key.end));
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
        let parameters = effect_parameters(checker, graph);
        let mut solver = Self {
            checker,
            graph,
            calls,
            releases,
            parameters,
            identities: HashMap::new(),
            body_instances: HashSet::new(),
            instances: Vec::new(),
            nodes: Vec::new(),
            expanded: 0,
            changed: VecDeque::new(),
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
                    *ty = cleanup_argument(self.checker, ty);
                    true
                }
                Some(Demand::Exact) => true,
                None => false,
            }
        });
        if let Some(id) = self.identities.get(&instance) {
            return *id;
        }
        let repeated = !self.body_instances.insert((
            instance.body.clone(),
            instance.unknown,
            instance.captures,
        ));
        let mut ancestor = if repeated && !instance.arguments.is_empty() {
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

    pub(super) fn once(&mut self, body: &EffectBody, unknown: bool) -> Option<String> {
        let id = self.intern(Self::instance(body, &Actuals::new(), unknown, true));
        self.solve();
        self.nodes[id].witness.clone()
    }

    pub(super) fn invocation(&mut self, key: &SpanKey, invocation: &Invocation) -> bool {
        let mut dependencies = Dependencies::default();
        self.invocation_dependencies(key, invocation, &Actuals::new(), false, &mut dependencies);
        let ids: Vec<_> = dependencies
            .bodies
            .into_iter()
            .map(|(instance, _)| self.intern(instance))
            .collect();
        self.solve();
        dependencies.intrinsic.is_some()
            || ids.into_iter().any(|id| self.nodes[id].witness.is_some())
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
                    instance.unknown,
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
                        instance.unknown,
                        &release.witness,
                        &mut result,
                    );
                }
            }
        }
        if let Some(calls) = self.calls.get(&instance.body) {
            for (key, invocation) in calls {
                self.invocation_dependencies(
                    key,
                    invocation,
                    &actuals,
                    instance.unknown,
                    &mut result,
                );
            }
        }
        result
    }

    fn cleanup_dependency(
        &self,
        target: CleanupTarget,
        unknown: bool,
        witness: &str,
        result: &mut Dependencies,
    ) {
        match target {
            CleanupTarget::Body { body, type_args } => {
                let actuals = match &body {
                    EffectBody::Declaration(id) => self
                        .checker
                        .fn_sigs
                        .get(id)
                        .and_then(|signature| signature.impl_method.as_ref())
                        .and_then(|method| {
                            crate::method_resolution::impl_receiver_substitution(
                                &method.receiver_parameters,
                                &method.receiver_args,
                                &type_args,
                            )
                        })
                        .unwrap_or_default(),
                    EffectBody::Closure(_)
                    | EffectBody::Generator(_)
                    | EffectBody::GeneratorBlock(_) => Actuals::new(),
                };
                result.body(Self::instance(&body, &actuals, unknown, false), witness);
            }
            CleanupTarget::Callable(_) | CleanupTarget::Unknown(_) | CleanupTarget::Suspends => {
                result.suspends(witness);
            }
        }
    }

    fn callee_dependencies(
        &self,
        invocation: &Invocation,
        actuals: &Actuals,
        unknown: bool,
        result: &mut Dependencies,
    ) {
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
                result.body(
                    Self::instance(&identity, actuals, unknown, false),
                    &invocation.name,
                );
                if capabilities.call == crate::CallableCallMode::Once {
                    result.body(
                        Self::instance(&identity, actuals, unknown, true),
                        &invocation.name,
                    );
                }
            }
            _ => result.suspends(&invocation.name),
        }
    }

    fn invocation_dependencies(
        &self,
        key: &SpanKey,
        invocation: &Invocation,
        actuals: &Actuals,
        unknown: bool,
        result: &mut Dependencies,
    ) {
        if let Some(suspends) = self.graph.submission_effects.get(key) {
            if *suspends {
                result.suspends(&invocation.name);
            }
            return;
        }
        match self.checker.call_target(key) {
            Some(target) if self.graph.builtin_suspensions.contains(target) => {
                result.suspends(&invocation.name);
            }
            Some(CallTarget::IndirectFunctionValue) => {
                self.callee_dependencies(invocation, actuals, unknown, result);
            }
            Some(
                CallTarget::User(id)
                | CallTarget::ImplMethod(id)
                | CallTarget::StaticTraitMethod { method: id, .. },
            ) => {
                let applied = self.checker.call_cleanup_substitutions(
                    *id,
                    key,
                    invocation.receiver.as_ref(),
                    actuals,
                );
                result.body(
                    Self::instance(&EffectBody::Declaration(*id), &applied, unknown, false),
                    &invocation.name,
                );
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
                | CallTarget::Builtin { .. }
                | CallTarget::RuntimeCollection(_),
            ) => {}
            Some(CallTarget::DynamicVtable { .. }) => {
                if self
                    .checker
                    .dyn_trait_method_calls
                    .get(key)
                    .is_none_or(|call| call.effect == super::super::SlotEffect::Suspends)
                {
                    result.suspends(&invocation.name);
                }
            }
            Some(CallTarget::Unsupported { .. }) => result.suspends(&invocation.name),
            None => match self.checker.method_call_rewrites.get(key) {
                Some(
                    MethodCallRewrite::RecordFnFieldCall { .. }
                    | MethodCallRewrite::BuiltinVecHigherOrder { .. },
                ) => self.callee_dependencies(invocation, actuals, unknown, result),
                Some(
                    MethodCallRewrite::RemoteActorAsk | MethodCallRewrite::GeneratorNext { .. },
                ) => result.suspends(&invocation.name),
                Some(_) => {}
                None => {
                    if !matches!(
                        self.checker.actor_method_dispatch.get(key),
                        Some(super::super::ActorMethodKind::Message { .. })
                    ) {
                        result.suspends(&invocation.name);
                    }
                }
            },
        }
    }
}
