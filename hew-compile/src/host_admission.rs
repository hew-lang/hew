//! Compile-time admission of host operations for selected execution profiles.
//!
//! The capability manifest owns the operation set. Checker call targets and
//! declaration identities own edges; spellings are used only for diagnostics.

use std::collections::{HashMap, HashSet, VecDeque};
use std::ops::Range;
use std::path::PathBuf;

use hew_parser::ast::{Item, Program, TraitItem, TypeBodyItem};
use hew_types::check::dispatch::CallTarget;
use hew_types::check::SpanKey;
use hew_types::env::TypeBindingId;
use hew_types::{
    CallableCandidate, DeclarationKind, DeclarationOccurrence, DefId, IndirectCallCandidates,
    NominalId, ResolvedTy, Ty, TypeCheckOutput,
};

use crate::{
    configured_stdlib_roots, path_is_below, read_source, FrontendDiagnostic, FrontendOptions,
    HostAdmission, SourceSet,
};

type Operation = hew_types::HostOperation;

#[derive(Clone)]
struct CallEdge {
    span: SpanKey,
    target: CallTarget,
}

struct IterationCalls {
    calls: Vec<(DefId, Vec<Ty>)>,
    iterator: Ty,
}

type CallableNode = hew_types::check::effects::EffectBody;

/// A value origin resolved under its calling context. Aggregate fields and
/// closure captures retain that context when the value crosses another call.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum ResolvedValue {
    Declaration(DefId),
    Closure {
        span: SpanKey,
        captures: ResolvedContext,
    },
    Generator {
        body: CallableNode,
        captures: ResolvedContext,
    },
    Erasure {
        site: SpanKey,
        value: IndirectCallCandidates,
        captures: ResolvedContext,
    },
    Aggregate {
        span: SpanKey,
        captures: ResolvedContext,
    },
    Source {
        candidates: IndirectCallCandidates,
        captures: ResolvedContext,
    },
    /// A supplied declaration parameter during definition checking. Concrete
    /// callers resolve its existing checker demand before entering the body.
    Parameter(TypeBindingId),
    Unknown,
}

#[derive(Clone, Debug, Default)]
struct CandidateEnv {
    values: HashMap<TypeBindingId, Vec<ResolvedValue>>,
    types: Vec<(hew_types::ParamHead, Ty)>,
}

impl CandidateEnv {
    fn new() -> Self {
        Self::default()
    }
}

impl std::ops::Deref for CandidateEnv {
    type Target = HashMap<TypeBindingId, Vec<ResolvedValue>>;
    fn deref(&self) -> &Self::Target {
        &self.values
    }
}
impl std::ops::DerefMut for CandidateEnv {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.values
    }
}
impl FromIterator<(TypeBindingId, Vec<ResolvedValue>)> for CandidateEnv {
    fn from_iter<T: IntoIterator<Item = (TypeBindingId, Vec<ResolvedValue>)>>(values: T) -> Self {
        Self {
            values: values.into_iter().collect(),
            types: Vec::new(),
        }
    }
}

/// Value and type substitutions belong to the same saved calling context.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct ResolvedContext {
    values: Vec<(TypeBindingId, Vec<ResolvedValue>)>,
    types: Vec<(hew_types::ParamHead, Ty)>,
}
impl ResolvedContext {
    fn env(&self) -> CandidateEnv {
        CandidateEnv {
            values: self.values.iter().cloned().collect(),
            types: self.types.clone(),
        }
    }
}

fn selected_call_target<'a>(output: &'a TypeCheckOutput, span: &SpanKey) -> Option<&'a CallTarget> {
    output
        .method_call_rewrites
        .get(span)
        .and_then(|rewrite| match rewrite {
            hew_types::MethodCallRewrite::RewriteToFunction { target, .. }
            | hew_types::MethodCallRewrite::RewriteModuleQualifiedToFunction { target, .. }
            | hew_types::MethodCallRewrite::StaticTraitDispatch { target, .. }
            | hew_types::MethodCallRewrite::BinderStaticCall(hew_types::BinderTraitCall {
                target,
                ..
            }) => Some(target),
            _ => None,
        })
        .or_else(|| output.direct_call_targets.get(span))
        .or_else(|| {
            output
                .dyn_trait_method_calls
                .get(span)
                .map(|call| &call.target)
        })
}

fn selected_call_declaration(output: &TypeCheckOutput, span: &SpanKey) -> Option<DefId> {
    selected_call_target(output, span).and_then(target_declaration)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum Projection {
    Field(NominalId, u32),
    Element(usize),
}
type CandidateVisit = (CallableCandidate, Vec<Projection>, ResolvedContext);

fn resolve_candidates(
    output: &TypeCheckOutput,
    candidates: &IndirectCallCandidates,
    env: &CandidateEnv,
) -> Vec<ResolvedValue> {
    let values = resolve_candidates_at(output, candidates, env, &[], false, 0, &mut Vec::new());
    if values.is_empty() {
        vec![ResolvedValue::Unknown]
    } else {
        values
    }
}

fn resolve_candidates_at(
    output: &TypeCheckOutput,
    candidates: &IndirectCallCandidates,
    env: &CandidateEnv,
    projections: &[Projection],
    nominal_only: bool,
    depth: usize,
    seen: &mut Vec<CandidateVisit>,
) -> Vec<ResolvedValue> {
    if depth > 128 {
        return vec![ResolvedValue::Unknown];
    }
    let mut values = Vec::new();
    if candidates.may_be_unknown {
        values.push(ResolvedValue::Unknown);
    }
    for candidate in &candidates.known {
        for value in resolve_candidate_at(
            output,
            candidate,
            env,
            projections,
            nominal_only,
            depth + 1,
            seen,
        ) {
            if !values.contains(&value) {
                values.push(value);
            }
        }
    }
    values
}

fn project_values(
    output: &TypeCheckOutput,
    values: &[ResolvedValue],
    projections: &[Projection],
    nominal_only: bool,
    depth: usize,
    seen: &mut Vec<CandidateVisit>,
) -> Vec<ResolvedValue> {
    let mut projected = Vec::new();
    for value in values {
        let next = match value {
            ResolvedValue::Erasure {
                value, captures, ..
            } if !projections.is_empty() => resolve_candidates_at(
                output,
                value,
                &captures.env(),
                projections,
                nominal_only,
                depth + 1,
                seen,
            ),
            ResolvedValue::Source {
                candidates,
                captures,
            } => resolve_candidates_at(
                output,
                candidates,
                &captures.env(),
                projections,
                nominal_only,
                depth + 1,
                seen,
            ),
            ResolvedValue::Aggregate { span, captures } if !projections.is_empty() => {
                let (selected, rest) = projections.split_last().expect("nonempty projections");
                output
                    .aggregate_field_candidates
                    .get(span)
                    .and_then(|fields| {
                        fields
                            .iter()
                            .find(|field| Projection::Field(field.owner, field.index) == *selected)
                    })
                    .map_or_else(
                        || vec![ResolvedValue::Unknown],
                        |field| {
                            resolve_candidates_at(
                                output,
                                &field.candidates,
                                &captures.env(),
                                rest,
                                nominal_only,
                                depth + 1,
                                seen,
                            )
                        },
                    )
            }
            ResolvedValue::Parameter(_) => vec![value.clone()],
            value if projections.is_empty() => vec![value.clone()],
            _ => vec![ResolvedValue::Unknown],
        };
        for value in next {
            if !projected.contains(&value) {
                projected.push(value);
            }
        }
    }
    projected
}

fn receiver_actual<'a>(
    output: &'a TypeCheckOutput,
    site: &SpanKey,
) -> Option<&'a IndirectCallCandidates> {
    output
        .callable_argument_flows
        .get(site)
        .and_then(|flows| flows.first())
        .map(|flow| &flow.candidates)
        .or_else(|| {
            output
                .generic_trait_call_arguments
                .get(site)
                .and_then(|actuals| actuals.iter().find(|actual| actual.slot == 0))
                .map(|actual| &actual.candidates)
        })
}

#[expect(
    clippy::too_many_lines,
    reason = "project each checker-owned value-origin variant"
)]
fn resolve_candidate_at(
    output: &TypeCheckOutput,
    candidate: &CallableCandidate,
    env: &CandidateEnv,
    projections: &[Projection],
    nominal_only: bool,
    depth: usize,
    seen: &mut Vec<CandidateVisit>,
) -> Vec<ResolvedValue> {
    if depth > 128 {
        return vec![ResolvedValue::Unknown];
    }
    let visit = (candidate.clone(), projections.to_vec(), env_key(env));
    if let Some(cycle) = seen.iter().position(|active| active == &visit) {
        // A loop recurrence contributes no additional origin beyond its entry
        // and replacement edges. An expansion without a concrete origin is
        // still unknown at the invocation boundary.
        return if seen[cycle..]
            .iter()
            .any(|(candidate, _, _)| matches!(candidate, CallableCandidate::LoopInput { .. }))
        {
            Vec::new()
        } else {
            vec![ResolvedValue::Unknown]
        };
    }
    seen.push(visit);
    let values = match candidate {
        CallableCandidate::Erasure { site, value } => project_values(
            output,
            &[ResolvedValue::Erasure {
                site: site.clone(),
                value: (**value).clone(),
                captures: env_key(env),
            }],
            projections,
            nominal_only,
            depth + 1,
            seen,
        ),
        CallableCandidate::GeneratorBlock(span) if projections.is_empty() => {
            vec![ResolvedValue::Generator {
                body: CallableNode::GeneratorBlock(span.clone()),
                captures: env_key(env),
            }]
        }
        CallableCandidate::Declaration(id) if projections.is_empty() => {
            vec![ResolvedValue::Declaration(*id)]
        }
        CallableCandidate::Closure(span) if projections.is_empty() => {
            vec![ResolvedValue::Closure {
                span: span.clone(),
                captures: env_key(env),
            }]
        }
        CallableCandidate::Formal(formal) => env.get(formal).map_or_else(
            || vec![ResolvedValue::Unknown],
            |values| project_values(output, values, projections, nominal_only, depth + 1, seen),
        ),
        CallableCandidate::LoopInput { site, binding } => output
            .loop_value_candidates
            .get(&(site.clone(), *binding))
            .map_or_else(
                || vec![ResolvedValue::Unknown],
                |candidates| {
                    resolve_candidates_at(
                        output,
                        candidates,
                        env,
                        projections,
                        nominal_only,
                        depth + 1,
                        seen,
                    )
                },
            ),
        CallableCandidate::Aggregate(span) => project_values(
            output,
            &[ResolvedValue::Aggregate {
                span: span.clone(),
                captures: env_key(env),
            }],
            projections,
            nominal_only,
            depth + 1,
            seen,
        ),
        // Writeback can change fields, but cannot change the receiver's type.
        CallableCandidate::CallWriteback(site) if nominal_only => receiver_actual(output, site)
            .map_or_else(
                || vec![ResolvedValue::Unknown],
                |candidates| {
                    resolve_candidates_at(
                        output,
                        candidates,
                        env,
                        projections,
                        nominal_only,
                        depth + 1,
                        seen,
                    )
                },
            ),
        CallableCandidate::CallWriteback(site) | CallableCandidate::CallResult(site) => {
            let mut callees = selected_call_declaration(output, site)
                .into_iter()
                .map(|callee| (callee, None))
                .collect::<Vec<_>>();
            let mut symbolic_receiver = Vec::new();
            if callees.is_empty() {
                if let Some(CallTarget::StaticTraitMethod {
                    declaring_trait,
                    method,
                }) = selected_call_target(output, site)
                {
                    let receiver = receiver_actual(output, site)
                        .map(|candidates| {
                            resolve_candidates_at(
                                output,
                                candidates,
                                env,
                                &[],
                                true,
                                depth + 1,
                                &mut Vec::new(),
                            )
                        })
                        .unwrap_or_default();
                    if !receiver.is_empty()
                        && receiver
                            .iter()
                            .all(|value| matches!(value, ResolvedValue::Parameter(_)))
                    {
                        symbolic_receiver.clone_from(&receiver);
                    }
                    let (trait_args, method_args) = trait_method_arguments(output, site, env);
                    for value in receiver {
                        let Some(instance) = actual_type(output, &value)
                            .and_then(|ty| ResolvedTy::from_ty(&ty).ok())
                            .and_then(|ty| ty.impl_receiver_instance(&output.defs))
                        else {
                            continue;
                        };
                        for (callee, arguments) in service_methods(
                            output,
                            &instance,
                            (*declaring_trait, *method),
                            &trait_args,
                            &method_args,
                            true,
                        ) {
                            let selected = (callee, Some(arguments));
                            if !callees.contains(&selected) {
                                callees.push(selected);
                            }
                        }
                    }
                }
            }
            let mut values = symbolic_receiver;
            if callees.is_empty() && values.is_empty() {
                values.push(ResolvedValue::Unknown);
            }
            for (callee, arguments) in callees {
                let mut next_env = callee_env(output, site, callee, env);
                if let Some(arguments) = arguments {
                    bind_type_context(
                        output,
                        &CallableNode::Declaration(callee),
                        &arguments,
                        &mut next_env,
                    );
                }
                if matches!(candidate, CallableCandidate::CallResult(_))
                    && output
                        .suspension_effects
                        .bodies
                        .contains_key(&CallableNode::Generator(callee))
                {
                    values.push(ResolvedValue::Generator {
                        body: CallableNode::Generator(callee),
                        captures: env_key(&next_env),
                    });
                    continue;
                }
                let body = hew_types::check::effects::EffectBody::Declaration(callee);
                let returned = if matches!(candidate, CallableCandidate::CallWriteback(_)) {
                    output.callable_receiver_outputs.get(&body)
                } else {
                    output.callable_return_candidates.get(&body)
                };
                let next = returned.map_or_else(
                    || vec![ResolvedValue::Unknown],
                    |returned| {
                        resolve_candidates_at(
                            output,
                            returned,
                            &next_env,
                            projections,
                            nominal_only,
                            depth + 1,
                            seen,
                        )
                    },
                );
                for value in next {
                    if !values.contains(&value) {
                        values.push(value);
                    }
                }
            }
            values
        }
        CallableCandidate::Sequence(values) => {
            if let Some((Projection::Element(index), rest)) = projections.split_last() {
                values.get(*index).map_or_else(
                    || vec![ResolvedValue::Unknown],
                    |value| {
                        resolve_candidates_at(
                            output,
                            value,
                            env,
                            rest,
                            nominal_only,
                            depth + 1,
                            seen,
                        )
                    },
                )
            } else {
                vec![ResolvedValue::Unknown]
            }
        }
        CallableCandidate::Element { receiver, index } => {
            let mut path = projections.to_vec();
            path.push(Projection::Element(*index));
            resolve_candidate_at(output, receiver, env, &path, nominal_only, depth + 1, seen)
        }
        CallableCandidate::Field {
            receiver,
            owner,
            index,
        } => {
            let mut path = projections.to_vec();
            path.push(Projection::Field(*owner, *index));
            resolve_candidate_at(output, receiver, env, &path, nominal_only, depth + 1, seen)
        }
        _ => vec![ResolvedValue::Unknown],
    };
    seen.pop();
    values
}

/// Forwarding a formal preserves its value, rather than nesting another
/// suspended substitution each time a recursive call crosses the boundary.
fn actual_values(
    candidates: &IndirectCallCandidates,
    caller_env: &CandidateEnv,
) -> Vec<ResolvedValue> {
    if !candidates.may_be_unknown {
        match candidates.known.as_slice() {
            [] => return Vec::new(),
            [CallableCandidate::Formal(formal)] => {
                return caller_env
                    .get(formal)
                    .cloned()
                    .unwrap_or_else(|| vec![ResolvedValue::Unknown]);
            }
            _ => {}
        }
    }
    vec![ResolvedValue::Source {
        candidates: candidates.clone(),
        captures: env_key(caller_env),
    }]
}

fn callee_env(
    output: &TypeCheckOutput,
    span: &SpanKey,
    callee: DefId,
    caller_env: &CandidateEnv,
) -> CandidateEnv {
    let mut env = callee_value_env(output, span, callee, caller_env);
    env.types.clone_from(&caller_env.types);
    let substitutions: HashMap<_, _> = caller_env.types.iter().cloned().collect();
    let args: Vec<_> = output
        .call_type_args
        .get(span)
        .into_iter()
        .flatten()
        .map(|ty| ty.substitute_type_params_parallel(&substitutions))
        .collect();
    bind_type_context(output, &CallableNode::Declaration(callee), &args, &mut env);
    env
}

fn bind_type_context(
    output: &TypeCheckOutput,
    body: &CallableNode,
    args: &[Ty],
    env: &mut CandidateEnv,
) {
    for (parameter, argument) in body_parameters(output, body).into_iter().zip(args) {
        env.types.retain(|(existing, _)| *existing != parameter);
        env.types.push((parameter, argument.clone()));
    }
    env.types.sort_by_key(|(parameter, _)| parameter.id);
}

fn closure_env(
    output: &TypeCheckOutput,
    call: &SpanKey,
    closure: &SpanKey,
    captures: &ResolvedContext,
    caller_env: &CandidateEnv,
) -> CandidateEnv {
    let mut env = captures.env();
    if let Some(formals) = output
        .callable_formals
        .get(&CallableNode::Closure(closure.clone()))
    {
        for actual in output
            .generic_trait_call_arguments
            .get(call)
            .into_iter()
            .flatten()
        {
            if let Some(formal) = formals.get(actual.slot) {
                env.insert(*formal, actual_values(&actual.candidates, caller_env));
            }
        }
    }
    env
}

fn callee_value_env(
    output: &TypeCheckOutput,
    span: &SpanKey,
    callee: DefId,
    caller_env: &CandidateEnv,
) -> CandidateEnv {
    let direct: CandidateEnv = output
        .callable_argument_flows
        .get(span)
        .into_iter()
        .flatten()
        .filter(|flow| flow.callee == callee)
        .map(|flow| (flow.formal, actual_values(&flow.candidates, caller_env)))
        .collect();
    if !direct.is_empty() {
        return direct;
    }
    let Some(actuals) = output.generic_trait_call_arguments.get(span) else {
        return CandidateEnv::new();
    };
    let Some(formals) = output
        .callable_formals
        .get(&hew_types::check::effects::EffectBody::Declaration(callee))
    else {
        return CandidateEnv::new();
    };
    actuals
        .iter()
        .filter_map(|actual| {
            Some((
                *formals.get(actual.slot)?,
                actual_values(&actual.candidates, caller_env),
            ))
        })
        .collect()
}

fn confined_callee_env(
    output: &TypeCheckOutput,
    span: &SpanKey,
    callee: DefId,
    caller_env: &CandidateEnv,
) -> CandidateEnv {
    let mut env = callee_env(output, span, callee, caller_env);
    let actor = if output.defs.kind(callee) == DeclarationKind::Actor {
        Some(callee)
    } else {
        output
            .defs
            .owner(callee)
            .filter(|id| output.defs.kind(*id) == DeclarationKind::Actor)
    };
    let Some(actor) = actor else {
        return env;
    };
    for (binding, (owner, index)) in &output.suspension_effects.actor_fields {
        if owner.declaration() != actor {
            continue;
        }
        let value = if output.defs.kind(callee) == DeclarationKind::Actor {
            output
                .aggregate_field_candidates
                .get(span)
                .and_then(|fields| {
                    fields
                        .iter()
                        .find(|field| field.owner == *owner && field.index == *index)
                })
                .map_or_else(
                    || vec![ResolvedValue::Unknown],
                    |field| actual_values(&field.candidates, caller_env),
                )
        } else {
            caller_env
                .get(binding)
                .cloned()
                .unwrap_or_else(|| vec![ResolvedValue::Unknown])
        };
        env.insert(*binding, value);
    }
    env
}

fn body_parameters(output: &TypeCheckOutput, body: &CallableNode) -> Vec<hew_types::ParamHead> {
    match body {
        CallableNode::Declaration(id) | CallableNode::Generator(id) => output
            .fn_sigs
            .get(id)
            .map(|sig| sig.type_params.clone())
            .or_else(|| {
                output
                    .type_defs
                    .get(&NominalId::of_declaration(*id))
                    .map(|definition| definition.type_params.clone())
            })
            .unwrap_or_default(),
        CallableNode::Closure(span) => match output.expr_types.get(span) {
            Some(Ty::GenericCallable { parameters, .. }) => parameters
                .iter()
                .map(|parameter| parameter.parameter)
                .collect(),
            _ => Vec::new(),
        },
        CallableNode::GeneratorBlock(_) => Vec::new(),
    }
}

/// Interpret an uninstantiated declaration with its own symbolic inputs. The
/// checker tables retain the same demands for every concrete call/construction.
fn definition_env(output: &TypeCheckOutput, root: DefId) -> CandidateEnv {
    let body = CallableNode::Declaration(root);
    let mut env = CandidateEnv::new();
    for parameter in body_parameters(output, &body) {
        env.types.push((parameter, Ty::param(parameter)));
    }
    for formal in output.callable_formals.get(&body).into_iter().flatten() {
        env.insert(*formal, vec![ResolvedValue::Parameter(*formal)]);
    }
    for (binding, (owner, _)) in &output.suspension_effects.actor_fields {
        if owner.declaration() == root {
            env.insert(*binding, vec![ResolvedValue::Parameter(*binding)]);
        }
    }
    env.types.sort_by_key(|(parameter, _)| parameter.id);
    env
}

/// Only a declaration-owned, still unbound parameter is an obligation. Erased
/// generators, opaque callables and other unknown concrete types remain errors.
fn unbound_parameter(ty: &Ty, substitutions: &HashMap<hew_types::ParamHead, Ty>) -> bool {
    match ty {
        Ty::Named {
            head: hew_types::TypeHead::Param(parameter),
            args,
        } if args.is_empty() => substitutions
            .get(parameter)
            .is_some_and(|ty| *ty == Ty::param(*parameter)),
        Ty::AssocType { base, .. } => unbound_parameter(base, substitutions),
        _ => false,
    }
}

/// Follow the ordinary `for` protocol using the checked dispatch table. The
/// iterable is already checked: synthesizing it again would repeat moves.
fn iteration_calls(output: &TypeCheckOutput, iterable: &Ty) -> Option<IterationCalls> {
    use hew_types::check::dispatch_table::MethodSelection;

    let select = |ty: &Ty, name: &str| {
        let instance = ResolvedTy::from_ty(ty)
            .map_err(|_| ())?
            .impl_receiver_instance(&output.defs)
            .ok_or(())?;
        match output.dispatch.select(
            instance.nominal,
            Some(&instance),
            hew_parser::ast::Symbol::intern(name),
        ) {
            MethodSelection::Unique(_, method) => Ok(Some((
                method,
                instance
                    .args
                    .iter()
                    .map(ResolvedTy::to_ty)
                    .collect::<Vec<_>>(),
            ))),
            MethodSelection::Ambiguous(_) => Err(()),
            MethodSelection::Missing => Ok(None),
        }
    };
    let mut calls = Vec::new();
    let iterator = if let Some((method, args)) = select(iterable, "into_iter").ok()? {
        let sig = output.fn_sigs.get(&method)?;
        if sig.type_params.len() != args.len() {
            return None;
        }
        let substitutions = sig
            .type_params
            .iter()
            .copied()
            .zip(args.iter().cloned())
            .collect();
        let iterator = sig
            .return_type
            .substitute_type_params_parallel(&substitutions);
        calls.push((method, args));
        iterator
    } else {
        iterable.clone()
    };
    // VecIter's next operation is the compiler's value-only kernel, rather
    // than a source method. Every other iterator needs a checked declaration.
    if iterator.named_builtin() != Some(hew_types::BuiltinType::VecIter) {
        calls.push(select(&iterator, "next").ok()??);
    }
    Some(IterationCalls { calls, iterator })
}

fn nominal_actuals(
    output: &TypeCheckOutput,
    candidates: &IndirectCallCandidates,
    env: &CandidateEnv,
) -> Vec<(NominalId, Vec<Ty>)> {
    let values = resolve_candidates_at(output, candidates, env, &[], true, 0, &mut Vec::new());
    let mut instances = Vec::new();
    for value in values {
        if let Some(ty) = actual_type(output, &value) {
            if let Some(instance) = Some(ty)
                .and_then(|ty| ResolvedTy::from_ty(&ty).ok())
                .and_then(|ty| ty.impl_receiver_instance(&output.defs))
            {
                let instance = (
                    instance.nominal,
                    instance.args.iter().map(ResolvedTy::to_ty).collect(),
                );
                if !instances.contains(&instance) {
                    instances.push(instance);
                }
            }
        }
    }
    instances
}

fn actual_type(output: &TypeCheckOutput, value: &ResolvedValue) -> Option<Ty> {
    let (ty, captures) = match value {
        ResolvedValue::Aggregate { span, captures } => (output.expr_types.get(span)?, captures),
        ResolvedValue::Erasure { site, captures, .. } => (
            &output.dyn_trait_coercions.get(site)?.concrete_type,
            captures,
        ),
        _ => return None,
    };
    Some(ty.substitute_type_params_parallel(&captures.types.iter().cloned().collect()))
}

fn trait_method_arguments(
    output: &TypeCheckOutput,
    site: &SpanKey,
    env: &CandidateEnv,
) -> (Vec<Ty>, Vec<Ty>) {
    let substitutions = env.types.iter().cloned().collect();
    let trait_args = output
        .method_call_rewrites
        .get(site)
        .into_iter()
        .flat_map(|rewrite| match rewrite {
            hew_types::MethodCallRewrite::StaticTraitDispatch { trait_args, .. } => {
                trait_args.as_slice()
            }
            hew_types::MethodCallRewrite::BinderStaticCall(call) => call.trait_args.as_slice(),
            _ => &[],
        })
        .map(|ty| ty.substitute_type_params_parallel(&substitutions))
        .collect();
    let method_args = output
        .call_type_args
        .get(site)
        .into_iter()
        .flatten()
        .map(|ty| ty.substitute_type_params_parallel(&substitutions))
        .collect();
    (trait_args, method_args)
}

fn service_methods(
    output: &TypeCheckOutput,
    instance: &hew_types::NominalInstance,
    (declaring_trait, method): (DefId, DefId),
    trait_args: &[Ty],
    method_args: &[Ty],
    confined: bool,
) -> Vec<(DefId, Vec<Ty>)> {
    let name = output.defs.name(method);
    let methods: Vec<_> = if confined {
        output
            .dispatch
            .candidates(instance.nominal, Some(instance), name)
            .into_iter()
            .map(|(owner, method)| (name, owner, method))
            .collect()
    } else {
        output.dispatch.methods_of(instance.nominal).collect()
    };
    let receiver = ResolvedTy::Named {
        head: hew_types::TypeHead::of_declaration(&output.defs, instance.nominal),
        args: instance.args.clone(),
        is_opaque: false,
    };
    methods
        .into_iter()
        .filter(|(candidate_name, owner, candidate)| {
            *candidate_name == name
                && service_method_owner(
                    output,
                    *owner,
                    instance.nominal,
                    *candidate,
                    declaring_trait,
                    method,
                )
        })
        .filter_map(|(_, _, candidate)| {
            Some((
                candidate,
                method_instance_arguments(output, candidate, &receiver, trait_args, method_args)?,
            ))
        })
        .collect()
}

fn method_instance_arguments(
    output: &TypeCheckOutput,
    method: DefId,
    receiver: &ResolvedTy,
    trait_args: &[Ty],
    method_args: &[Ty],
) -> Option<Vec<Ty>> {
    let signature = output.fn_sigs.get(&method)?;
    if signature.type_params.is_empty() && trait_args.is_empty() {
        return Some(Vec::new());
    }
    let impl_args = output
        .type_fact_context
        .implementation_arguments(method, receiver, trait_args)?;
    let arguments: Vec<_> = method_args
        .iter()
        .cloned()
        .chain(impl_args.iter().map(ResolvedTy::to_ty))
        .collect();
    (signature.type_params.len() == arguments.len()).then_some(arguments)
}

fn service_method_owner(
    output: &TypeCheckOutput,
    owner: hew_types::check::dispatch_table::MethodOwner,
    nominal: NominalId,
    candidate: DefId,
    declaring_trait: DefId,
    method: DefId,
) -> bool {
    use hew_types::check::dispatch_table::MethodOwner;
    match owner {
        MethodOwner::Trait(id) => id == declaring_trait,
        MethodOwner::Inherent => output.structural_witnesses.iter().any(|witness| {
            witness.declaring_trait == declaring_trait
                && witness.method == method
                && witness.self_type == nominal
                && witness.inherent == candidate
        }),
    }
}

fn env_key(env: &CandidateEnv) -> ResolvedContext {
    let mut key = env
        .iter()
        .map(|(formal, values)| (*formal, values.clone()))
        .collect::<Vec<_>>();
    key.sort_by_key(|(formal, _)| formal.0);
    ResolvedContext {
        values: key,
        types: env.types.clone(),
    }
}

#[derive(Clone)]
struct SourceSite {
    span: Range<usize>,
    spelling: String,
}

fn refused_function(path: &str, admission: &HostAdmission) -> Option<&'static Operation> {
    let operations = if matches!(admission, HostAdmission::Generate) {
        hew_types::GENERATE_FUNCTION_REJECTIONS
    } else {
        hew_types::DETERMINISTIC_FUNCTION_REJECTIONS
    };
    operations.iter().find(|entry| entry.identity == path)
}

fn refused_endpoint(symbol: &str, admission: &HostAdmission) -> Option<&'static Operation> {
    let operations = if matches!(admission, HostAdmission::Generate) {
        hew_types::GENERATE_ENDPOINT_REJECTIONS
    } else {
        hew_types::DETERMINISTIC_ENDPOINT_REJECTIONS
    };
    operations.iter().find(|entry| entry.identity == symbol)
}

fn source_item<'a>(
    program: &'a Program,
    module_idx: u32,
    item_span: &Range<usize>,
) -> Option<&'a Item> {
    if let Some(graph) = &program.module_graph {
        let indices = graph.file_span_indices();
        for (module_path, module) in &graph.modules {
            for (item_index, (item, span)) in module.items.iter().enumerate() {
                if indices.item_index(module_path, item_index) == Some(module_idx)
                    && span.start <= item_span.start
                    && item_span.end <= span.end
                {
                    return Some(item);
                }
            }
        }
    }
    (module_idx == 0)
        .then(|| {
            program
                .items
                .iter()
                .find(|(_, span)| span.start <= item_span.start && item_span.end <= span.end)
                .map(|(item, _)| item)
        })
        .flatten()
}

fn body_span(item: &Item, site: DeclarationOccurrence) -> Option<Range<usize>> {
    let ordinal = usize::try_from(site.ordinal()).ok()?;
    let span = match (site.kind(), item) {
        (DeclarationKind::Function, Item::Function(function)) => &function.fn_span,
        (DeclarationKind::ImplMethod, Item::Impl(block)) => &block.methods.get(ordinal)?.fn_span,
        (DeclarationKind::TraitMethod, Item::Trait(decl)) => {
            let method = decl
                .items
                .iter()
                .filter_map(|item| match item {
                    TraitItem::Method(method) => Some(method),
                    TraitItem::AssociatedType { .. } => None,
                })
                .nth(ordinal)?;
            method.body.as_ref()?;
            &method.span
        }
        (DeclarationKind::TypeMethod, Item::TypeDecl(decl)) => {
            &decl
                .body
                .iter()
                .filter_map(|part| match part {
                    TypeBodyItem::Method(method) => Some(method),
                    _ => None,
                })
                .nth(ordinal)?
                .fn_span
        }
        (DeclarationKind::ActorMethod, Item::Actor(actor)) => &actor.methods.get(ordinal)?.fn_span,
        (DeclarationKind::ActorInit, Item::Actor(actor)) => &actor.init.as_ref()?.span,
        (DeclarationKind::Actor, Item::Actor(_))
        | (DeclarationKind::Supervisor, Item::Supervisor(_)) => return Some(site.span()),
        (DeclarationKind::SupervisorMethod, Item::Supervisor(supervisor)) => {
            &supervisor.methods.get(ordinal)?.fn_span
        }
        (DeclarationKind::ActorReceive, Item::Actor(actor)) => {
            &actor.receive_fns.get(ordinal)?.span
        }
        _ => return None,
    };
    (!span.is_empty()).then(|| span.clone())
}

fn module_index(program: &Program, output: &TypeCheckOutput, id: DefId) -> Option<u32> {
    let module = output.defs.site(id)?.module()?;
    if Some(module) == output.defs.root_module() {
        return Some(0);
    }
    let path = output.defs.module_source(module)?;
    program
        .module_graph
        .as_ref()?
        .file_span_indices()
        .path_index(path)
}

#[allow(
    clippy::too_many_lines,
    reason = "call graph construction joins the checker's complementary call ledgers"
)]
fn call_graph(
    program: &Program,
    output: &TypeCheckOutput,
    generating: bool,
    confined: bool,
    stdlib_roots: &[PathBuf],
) -> HashMap<CallableNode, Vec<CallEdge>> {
    let bodies: Vec<_> = output
        .defs
        .declarations()
        .filter_map(|(site, id)| {
            let index = module_index(program, output, id)?;
            let item = source_item(program, index, &site.span())?;
            Some((id, index, body_span(item, site)?))
        })
        .collect();
    let owner_for = |span: &SpanKey| {
        if let Some(owner) = output.suspension_effects.call_owners.get(span) {
            return Some(owner.clone());
        }
        if confined {
            return None;
        }
        if let Some(closure) = output
            .closure_escape_facts
            .keys()
            .filter(|closure| {
                closure.module_idx == span.module_idx
                    && closure.start <= span.start
                    && span.end <= closure.end
                    && (closure.start != span.start || closure.end != span.end)
            })
            .min_by_key(|closure| closure.end - closure.start)
        {
            return Some(CallableNode::Closure(closure.clone()));
        }
        bodies
            .iter()
            .filter(|(_, index, body)| {
                *index == span.module_idx && body.start <= span.start && span.end <= body.end
            })
            .min_by_key(|(_, _, body)| body.end - body.start)
            .map(|(id, _, _)| CallableNode::Declaration(*id))
    };
    let mut edges: HashMap<CallableNode, Vec<CallEdge>> = HashMap::new();
    for (span, target) in &output.direct_call_targets {
        let owner = owner_for(span);
        if let Some(owner) = owner {
            edges.entry(owner).or_default().push(CallEdge {
                span: span.clone(),
                target: target.clone(),
            });
        }
    }
    for (span, call) in &output.resolved_calls {
        if selected_call_target(output, span).is_some() {
            continue;
        }
        if let Some(owner) = owner_for(span) {
            edges.entry(owner).or_default().push(CallEdge {
                span: span.clone(),
                target: call.target.clone(),
            });
        }
    }
    for (span, rewrite) in &output.method_call_rewrites {
        let target = match rewrite {
            hew_types::MethodCallRewrite::RewriteToFunction { target, .. }
            | hew_types::MethodCallRewrite::RewriteModuleQualifiedToFunction { target, .. }
            | hew_types::MethodCallRewrite::StaticTraitDispatch { target, .. }
            | hew_types::MethodCallRewrite::BinderStaticCall(hew_types::BinderTraitCall {
                target,
                ..
            }) => target.clone(),
            // These checker-selected rewrites bypass ordinary call targets.
            // Project them to the codegen ABI endpoint, whose admission policy
            // remains owned by the capability manifest.
            hew_types::MethodCallRewrite::RemoteActorAsk { .. } => CallTarget::Builtin {
                endpoint: "hew_remote_call_new".to_string(),
            },
            hew_types::MethodCallRewrite::RemoteActorSend { .. } => CallTarget::Builtin {
                endpoint: "hew_node_api_send_location".to_string(),
            },
            _ => continue,
        };
        if let Some(owner) = owner_for(span) {
            edges.entry(owner).or_default().push(CallEdge {
                span: span.clone(),
                target,
            });
        }
    }
    for (span, call) in &output.dyn_trait_method_calls {
        if let Some(owner) = owner_for(span) {
            edges.entry(owner).or_default().push(CallEdge {
                span: span.clone(),
                target: call.target.clone(),
            });
        }
    }
    for (span, dispatch) in &output.actor_method_dispatch {
        let method = match dispatch {
            hew_types::check::ActorMethodKind::Ask { method, .. }
            | hew_types::check::ActorMethodKind::StreamProducer(method, _) => *method,
            hew_types::check::ActorMethodKind::Message { .. } => continue,
        };
        if let Some(owner) = owner_for(span) {
            edges.entry(owner).or_default().push(CallEdge {
                span: span.clone(),
                target: CallTarget::User(method),
            });
        }
    }
    for (span, delivery) in &output.actor_delivery_calls {
        if let hew_types::actor_delivery::ActorDeliveryCall::Resume { method, .. } = delivery {
            if let Some(owner) = owner_for(span) {
                edges.entry(owner).or_default().push(CallEdge {
                    span: span.clone(),
                    target: CallTarget::User(*method),
                });
            }
        }
    }
    for (span, target) in &output.direct_call_targets {
        if let CallTarget::DeclaredRuntime {
            actor_endpoints: Some(endpoints),
            ..
        } = target
        {
            if let Some(owner) = owner_for(span) {
                for endpoint in [&endpoints.data, &endpoints.close] {
                    edges.entry(owner.clone()).or_default().push(CallEdge {
                        span: span.clone(),
                        target: CallTarget::User(endpoint.handler),
                    });
                }
            }
        }
    }
    if generating {
        // A materialized value can invoke trait callbacks or resource cleanup
        // without a written function call. Use the checker's dispatch and
        // resource facts, including callbacks reached through generic code.
        for (span, ty) in &output.resolved_expr_types {
            let Some(instance) = ty.nominal_instance(&output.defs) else {
                continue;
            };
            let resource = output
                .type_fact_context
                .declarations()
                .get(&instance.nominal)
                .is_some_and(|declaration| {
                    declaration.marker == hew_types::DeclarationMarker::Resource
                });
            let Some(owner) = owner_for(span) else {
                continue;
            };
            for (_, method_owner, method) in output.dispatch.methods_of(instance.nominal) {
                if matches!(
                    method_owner,
                    hew_types::check::dispatch_table::MethodOwner::Trait(_)
                ) || resource && output.consuming_inherent_methods.contains(&method)
                {
                    edges.entry(owner.clone()).or_default().push(CallEdge {
                        span: span.clone(),
                        target: CallTarget::User(method),
                    });
                }
            }
        }
    }
    if generating || confined {
        // Once spawned, an actor can receive callbacks through messages,
        // lifecycle events or supervision. Admission conservatively checks
        // every callback of that checked actor, including its initializers.
        for span in output.spawn_argument_slots.keys() {
            let Some((actor, _)) = output
                .expr_types
                .get(span)
                .and_then(Ty::actor_handle_identity)
            else {
                continue;
            };
            let actor = actor.id.declaration();
            if let Some(owner) = owner_for(span) {
                edges.entry(owner).or_default().push(CallEdge {
                    span: span.clone(),
                    target: CallTarget::User(actor),
                });
            }
            for (site, callback) in output.defs.declarations() {
                if output.defs.owner(callback) == Some(actor)
                    && matches!(
                        site.kind(),
                        DeclarationKind::ActorInit
                            | DeclarationKind::ActorReceive
                            | DeclarationKind::ActorMethod
                            | DeclarationKind::SupervisorMethod
                    )
                {
                    edges
                        .entry(CallableNode::Declaration(actor))
                        .or_default()
                        .push(CallEdge {
                            span: span.clone(),
                            target: CallTarget::User(callback),
                        });
                }
            }
        }
    }
    if confined {
        // An annotation also applies to exported or otherwise unconstructed
        // actors. Concrete construction contexts still arrive through spawn.
        for actor in &output.suspension_effects.confined {
            if output.defs.kind(*actor) != DeclarationKind::Actor {
                continue;
            }
            let Some(site) = output.defs.site(*actor) else {
                continue;
            };
            let Some(module) = module_index(program, output, *actor) else {
                continue;
            };
            let calls = edges.entry(CallableNode::Declaration(*actor)).or_default();
            for (site_kind, callback) in output.defs.declarations() {
                if output.defs.owner(callback) == Some(*actor)
                    && matches!(
                        site_kind.kind(),
                        DeclarationKind::ActorInit
                            | DeclarationKind::ActorReceive
                            | DeclarationKind::ActorMethod
                    )
                    && !calls
                        .iter()
                        .any(|call| target_declaration(&call.target) == Some(callback))
                {
                    calls.push(CallEdge {
                        span: SpanKey::in_module(&site.span(), module),
                        target: CallTarget::User(callback),
                    });
                }
            }
        }
        for (span, owner) in &output.suspension_effects.unresolved_calls {
            if matches!(
                output.method_call_rewrites.get(span),
                Some(hew_types::MethodCallRewrite::GeneratorNext { .. })
            ) {
                continue;
            }
            edges.entry(owner.clone()).or_default().push(CallEdge {
                span: span.clone(),
                target: CallTarget::IndirectFunctionValue,
            });
        }
        for (site, id) in output.defs.declarations() {
            let Some(declaration) = output
                .extern_contracts
                .declaration(id)
                .filter(|declaration| declaration.owns_endpoint)
            else {
                continue;
            };
            let Some(index) = module_index(program, output, id) else {
                continue;
            };
            let trusted_compiled_stdlib = declaration
                .declaring_module
                .as_deref()
                .is_some_and(|module| module.starts_with("std."))
                && output
                    .defs
                    .module(id)
                    .and_then(|module| output.defs.module_source(module))
                    .is_some_and(|source| {
                        stdlib_roots.iter().any(|root| path_is_below(source, root))
                    });
            edges
                .entry(CallableNode::Declaration(id))
                .or_default()
                .push(CallEdge {
                    span: SpanKey::in_module(&site.span(), index),
                    target: CallTarget::Extern {
                        declaration: id,
                        endpoint: declaration.symbol.clone(),
                        trusted_compiled_stdlib,
                    },
                });
        }
        for (span, dispatch) in &output.user_comparison_dispatch {
            let (hew_types::check::UserComparisonDispatch::Eq { method }
            | hew_types::check::UserComparisonDispatch::Ordering { method }) = dispatch;
            if let Some(owner) = owner_for(span) {
                edges.entry(owner).or_default().push(CallEdge {
                    span: span.clone(),
                    target: CallTarget::User(*method),
                });
            }
        }
    }
    // Materialized defaults have distinct IDs but execute the declaring
    // trait's checked body. Join those IDs through the dispatch owner.
    for nominal in output.type_defs.keys() {
        for (name, owner, method) in output.dispatch.methods_of(*nominal) {
            if output.defs.kind(method) != DeclarationKind::DefaultImplMethod {
                continue;
            }
            let hew_types::check::dispatch_table::MethodOwner::Trait(trait_id) = owner else {
                continue;
            };
            let Some(source) =
                output
                    .defs
                    .member_of_kind(trait_id, name, DeclarationKind::TraitMethod)
            else {
                continue;
            };
            let Some(site) = output.defs.site(source) else {
                continue;
            };
            let Some(module_idx) = module_index(program, output, source) else {
                continue;
            };
            edges
                .entry(CallableNode::Declaration(method))
                .or_default()
                .push(CallEdge {
                    span: SpanKey::in_module(&site.span(), module_idx),
                    target: CallTarget::User(source),
                });
        }
    }
    for calls in edges.values_mut() {
        calls.sort_by_key(|call| (call.span.module_idx, call.span.start, call.span.end));
        calls.dedup_by(|left, right| left.span == right.span && left.target == right.target);
    }
    edges
}

fn source_path(program: &Program, index: u32) -> Option<PathBuf> {
    let graph = program.module_graph.as_ref()?;
    let indices = graph.file_span_indices();
    graph
        .modules
        .values()
        .flat_map(|module| &module.source_paths)
        .find(|path| indices.path_index(path) == Some(index))
        .cloned()
}

fn site_for_call(
    program: &Program,
    root_source: &str,
    root_label: &str,
    documents: &SourceSet,
    span: &SpanKey,
) -> Option<(SourceSite, String, String)> {
    let path = (span.module_idx != 0)
        .then(|| source_path(program, span.module_idx))
        .flatten();
    let (source, filename) = if let Some(path) = path {
        (
            read_source(documents, &path).ok()?,
            path.display().to_string(),
        )
    } else {
        (root_source.to_string(), root_label.to_string())
    };
    source.get(span.start..span.end)?;
    let mut display_start = span.start;
    while display_start > 0
        && (source.as_bytes()[display_start - 1].is_ascii_alphanumeric()
            || matches!(source.as_bytes()[display_start - 1], b'_' | b'.'))
    {
        display_start -= 1;
    }
    let text = source.get(display_start..span.end)?;
    let spelling = text.split('(').next().unwrap_or(text).trim().to_string();
    Some((
        SourceSite {
            span: display_start..span.end,
            spelling,
        },
        source,
        filename,
    ))
}

fn target_declaration(target: &CallTarget) -> Option<DefId> {
    match target {
        CallTarget::User(id) | CallTarget::ImplMethod(id) => Some(*id),
        CallTarget::DeclaredRuntime { declaration, .. } => Some(*declaration),
        _ => None,
    }
}

fn target_operation(
    output: &TypeCheckOutput,
    target: &CallTarget,
    stdlib_roots: &[PathBuf],
    admission: &HostAdmission,
) -> Option<&'static Operation> {
    if let Some(declaration) = target_declaration(target) {
        if output
            .defs
            .module(declaration)
            .and_then(|module| output.defs.module_source(module))
            .is_some_and(|source| stdlib_roots.iter().any(|root| path_is_below(source, root)))
        {
            if let Some(operation) = refused_function(output.defs.path(declaration), admission) {
                return Some(operation);
            }
        }
    }
    let symbol = match target {
        CallTarget::Extern { endpoint, .. } | CallTarget::Builtin { endpoint } => endpoint.as_str(),
        CallTarget::Runtime(family) | CallTarget::DeclaredRuntime { family, .. } => {
            family.c_symbol()
        }
        _ => return None,
    };
    refused_endpoint(symbol, admission)
}

fn opaque_extern_diagnostic(
    output: &TypeCheckOutput,
    root: DefId,
    site: SourceSite,
    source: &str,
    filename: &str,
    admission: &HostAdmission,
) -> FrontendDiagnostic {
    let name = output.defs.name(root);
    let mut diagnostic = FrontendDiagnostic::coded_message_at(
        "E_DETERMINISTIC_HOST_OPERATION",
        format!(
            "`{name}` reaches user extern `{}`, whose host behaviour a deterministic entry cannot schedule",
            site.spelling
        ),
        site.span,
        source,
        filename,
    );
    if let crate::FrontendDiagnosticKind::Message(detail) = &mut diagnostic.kind {
        detail.help.push(match admission {
            HostAdmission::ProcessEntry => {
                "run without `--deterministic` or remove the user extern call".to_string()
            }
            HostAdmission::Tests(_) => {
                format!("mark `{name}` `#[real_time]` or remove the user extern call")
            }
            HostAdmission::Generate => {
                "remove the user extern declaration from the generator".to_string()
            }
            HostAdmission::Off => unreachable!("admission has no selected roots when off"),
        });
    }
    diagnostic
}

fn selected_entries(output: &TypeCheckOutput, options: &FrontendOptions) -> Vec<DefId> {
    match &options.host_admission {
        HostAdmission::Off => Vec::new(),
        HostAdmission::Tests(selections) => selections
            .iter()
            .filter_map(|selection| {
                if options.test_entry_selections.is_empty() {
                    // Admission-only checks have no dispatcher selection.
                    output
                        .defs
                        .declaration(selection.with_module(output.defs.root_module()))
                } else {
                    // The checker publishes plans in discovery order and owns
                    // their resolved module, including directly tested std sources.
                    options
                        .test_entry_selections
                        .iter()
                        .position(|entry| entry == selection)
                        .and_then(|index| output.test_entry_plans.get(index))
                        .map(|plan| plan.entry)
                }
            })
            .collect(),
        HostAdmission::ProcessEntry | HostAdmission::Generate => output
            .defs
            .declarations()
            .find(|(site, id)| {
                site.module() == output.defs.root_module()
                    && site.kind() == DeclarationKind::Function
                    && output.defs.name(*id).as_str() == "main"
            })
            .map(|(_, id)| vec![id])
            .unwrap_or_default(),
    }
}

/// Apply source boundaries independently of the selected execution profile.
pub(super) fn check(
    program: &Program,
    output: &TypeCheckOutput,
    root_source: &str,
    root_label: &str,
    options: &FrontendOptions,
) -> Vec<FrontendDiagnostic> {
    let mut diagnostics = check_profile(program, output, root_source, root_label, options, false);
    if !output.suspension_effects.confined.is_empty() {
        diagnostics.extend(check_profile(
            program,
            output,
            root_source,
            root_label,
            options,
            true,
        ));
    }
    diagnostics
}

fn confined_roots(
    output: &TypeCheckOutput,
    graph: &HashMap<CallableNode, Vec<CallEdge>>,
    stdlib_roots: &[PathBuf],
) -> Vec<DefId> {
    let incoming: HashSet<_> = graph
        .values()
        .flatten()
        .filter_map(|call| target_declaration(&call.target))
        .collect();
    output
        .suspension_effects
        .bodies
        .keys()
        .filter_map(|body| {
            let CallableNode::Declaration(id) = body else {
                return None;
            };
            if !matches!(
                output.defs.kind(*id),
                DeclarationKind::Function | DeclarationKind::Actor
            ) || (incoming.contains(id) && output.defs.name(*id).as_str() != "main")
                || output
                    .defs
                    .module(*id)
                    .and_then(|module| output.defs.module_source(module))
                    .is_some_and(|source| {
                        stdlib_roots.iter().any(|root| path_is_below(source, root))
                    })
            {
                return None;
            }
            Some(*id)
        })
        .collect()
}

/// Trusted primitive summaries are owned by the runtime row or stdlib declaration.
/// A user declaration never adopts those summaries by reusing its linker symbol.
fn confined_primitive_refusal(
    output: &TypeCheckOutput,
    target: &CallTarget,
) -> Option<&'static str> {
    use hew_types::runtime_call::{RuntimeAuthority, RuntimeCallFamily};
    use hew_types::stdlib_authority::ExternRuntimeCapability;
    let authority = match target {
        CallTarget::Runtime(family) | CallTarget::DeclaredRuntime { family, .. } => {
            Some(family.row().authority)
        }
        CallTarget::RuntimeCollection(_) | CallTarget::RecordConstructor(_) => {
            Some(RuntimeAuthority::Value)
        }
        CallTarget::Extern {
            trusted_compiled_stdlib: false,
            ..
        } => return Some("an opaque user extern/raw escape"),
        CallTarget::Extern {
            declaration,
            endpoint,
            ..
        } => {
            let module = output
                .defs
                .module(*declaration)
                .map(|module| output.defs.module_path(module));
            module
                .and_then(|module| {
                    hew_types::stdlib_authority::authority()
                        .extern_runtime_capability(module, output.defs.name(*declaration).as_str())
                })
                .map(|capability| match capability {
                    ExternRuntimeCapability::Ambient | ExternRuntimeCapability::Metrics => {
                        RuntimeAuthority::Ambient
                    }
                    ExternRuntimeCapability::ResourceOperation => RuntimeAuthority::Service,
                    ExternRuntimeCapability::Value => RuntimeAuthority::Value,
                })
                .or_else(|| {
                    RuntimeCallFamily::from_c_symbol(endpoint).map(|family| family.row().authority)
                })
        }
        CallTarget::Builtin { endpoint } => RuntimeCallFamily::from_catalog_endpoint(endpoint)
            .or_else(|| RuntimeCallFamily::from_c_symbol(endpoint))
            .map(|family| family.row().authority)
            .or_else(|| hew_types::stdlib_catalog_identity::builtin_authority(endpoint)),
        CallTarget::Unsupported { .. } => return Some("an unsupported operation"),
        CallTarget::User(_)
        | CallTarget::ImplMethod(_)
        | CallTarget::DynamicVtable { .. }
        | CallTarget::StaticTraitMethod { .. }
        | CallTarget::IndirectFunctionValue => return None,
    };
    match authority {
        Some(RuntimeAuthority::Ambient) => Some("ambient authority acquisition"),
        Some(RuntimeAuthority::Value | RuntimeAuthority::Service) => None,
        None => Some("an extern without a checked authority summary"),
    }
}

fn body_site(
    program: &Program,
    output: &TypeCheckOutput,
    root: DefId,
    source: &str,
    label: &str,
    options: &FrontendOptions,
) -> Option<(SourceSite, String, String)> {
    let site = output.defs.site(root)?;
    let (mut site, source, filename) = site_for_call(
        program,
        source,
        label,
        &options.documents,
        &SpanKey::in_module(&site.span(), module_index(program, output, root)?),
    )?;
    site.spelling = format!("implicit release in {}", output.defs.name(root));
    Some((site, source, filename))
}

fn confinement_diagnostic(
    output: &TypeCheckOutput,
    root: DefId,
    site: SourceSite,
    source: &str,
    filename: &str,
    reason: &str,
) -> FrontendDiagnostic {
    FrontendDiagnostic::coded_message_at("E_CONFINED_AUTHORITY", format!("confined `{}` reaches `{}`: {reason}; supply a narrow resource or service at construction", output.defs.name(root), site.spelling), site.span, source, filename)
}

/// Check each selected root independently using checker-owned call edges.
#[allow(
    clippy::too_many_lines,
    reason = "one breadth-first traversal keeps dispatch candidates and source-site provenance together"
)]
fn check_profile(
    program: &Program,
    output: &TypeCheckOutput,
    root_source: &str,
    root_label: &str,
    options: &FrontendOptions,
    confined: bool,
) -> Vec<FrontendDiagnostic> {
    if !confined && matches!(options.host_admission, HostAdmission::Off) {
        return Vec::new();
    }
    let generating = !confined && matches!(options.host_admission, HostAdmission::Generate);
    let stdlib_roots = configured_stdlib_roots(options);
    let graph = call_graph(program, output, generating, confined, &stdlib_roots);
    let roots = if confined {
        confined_roots(output, &graph, &stdlib_roots)
    } else {
        selected_entries(output, options)
    };
    let requested = match &options.host_admission {
        HostAdmission::Off => 0,
        HostAdmission::ProcessEntry | HostAdmission::Generate => 1,
        HostAdmission::Tests(entries) => entries.len(),
    };
    if !confined && roots.len() != requested {
        return vec![FrontendDiagnostic::coded_message(
            if generating {
                "E_GENERATE_ENTRY"
            } else {
                "E_DETERMINISTIC_ENTRY"
            },
            "a selected host-admission entry has no checked declaration identity",
        )];
    }
    if roots.is_empty() && !confined {
        return Vec::new();
    }
    let mut diagnostics = Vec::new();
    if generating {
        for (site, id) in output.defs.declarations() {
            if site.kind() != DeclarationKind::ExternFunction {
                continue;
            }
            let source_path = output
                .defs
                .module(id)
                .and_then(|module| output.defs.module_source(module));
            if source_path
                .is_some_and(|path| stdlib_roots.iter().any(|root| path_is_below(path, root)))
            {
                continue;
            }
            let source = source_path
                .and_then(|path| read_source(&options.documents, path).ok())
                .unwrap_or_else(|| root_source.to_owned());
            let filename = source_path
                .map_or_else(|| root_label.to_owned(), |path| path.display().to_string());
            diagnostics.push(FrontendDiagnostic::coded_message_at(
                "E_GENERATE_EXTERN",
                format!("generator declares user extern `{}`; host access must use the admitted standard library", output.defs.name(id)),
                site.span(),
                &source,
                &filename,
            ));
        }
        if !diagnostics.is_empty() {
            return diagnostics;
        }
    }
    let mut reported = HashSet::new();
    let mut reported_confined = HashSet::new();
    let mut reported_indirect = HashSet::new();
    let mut reported_extern = HashSet::new();
    let mut value_facts = confined.then(|| {
        hew_types::TypeFactService::new(output.type_fact_context.clone(), output.type_facts.clone())
    });
    let mut pending_roots: VecDeque<_> = roots.into_iter().collect();
    let mut covered_boundaries = HashSet::new();
    while let Some(root) = pending_roots.pop_front().or_else(|| {
        confined
            .then(|| {
                output
                    .suspension_effects
                    .confined
                    .iter()
                    .find(|id| !covered_boundaries.contains(*id))
                    .copied()
            })
            .flatten()
    }) {
        let mut queue = VecDeque::from([(
            CallableNode::Declaration(root),
            None::<(SourceSite, String, String)>,
            Vec::<Ty>::new(),
            if confined {
                definition_env(output, root)
            } else {
                CandidateEnv::new()
            },
            Vec::<(CallableNode, Vec<Ty>)>::new(),
            None::<DefId>,
        )]);
        let mut visited = HashSet::new();
        while let Some((
            caller,
            user_site,
            type_args,
            mut caller_env,
            mut ancestors,
            mut boundary,
        )) = queue.pop_front()
        {
            if confined {
                if let CallableNode::Declaration(id) | CallableNode::Generator(id) = &caller {
                    if output.suspension_effects.confined.contains(id) {
                        covered_boundaries.insert(*id);
                        if boundary.is_none() {
                            boundary = Some(*id);
                        }
                    }
                }
            }
            if confined {
                bind_type_context(output, &caller, &type_args, &mut caller_env);
            }
            let substitutions: HashMap<_, _> = caller_env.types.iter().cloned().collect();
            let checking = !confined || boundary.is_some();
            let diagnostic_root = boundary.unwrap_or(root);
            if confined
                && checking
                && !output.suspension_effects.bodies.contains_key(&caller)
                && graph.get(&caller).is_none_or(Vec::is_empty)
            {
                if let Some((site, source, filename)) = user_site.clone().or_else(|| {
                    body_site(
                        program,
                        output,
                        diagnostic_root,
                        root_source,
                        root_label,
                        options,
                    )
                }) {
                    diagnostics.push(confinement_diagnostic(
                        output,
                        diagnostic_root,
                        site,
                        &source,
                        &filename,
                        "a callable without a checked body or authority summary",
                    ));
                }
                continue;
            }

            if confined
                && checking
                && ancestors
                    .iter()
                    .any(|(ancestor, args)| ancestor == &caller && args != &type_args)
            {
                if let Some((site, source, filename)) = user_site.clone().or_else(|| {
                    body_site(
                        program,
                        output,
                        diagnostic_root,
                        root_source,
                        root_label,
                        options,
                    )
                }) {
                    diagnostics.push(confinement_diagnostic(
                        output,
                        diagnostic_root,
                        site,
                        &source,
                        &filename,
                        "a recursive instance without a finite authority proof",
                    ));
                }
                continue;
            }

            if ancestors.contains(&(caller.clone(), type_args.clone()))
                && !visited.contains(&(
                    caller.clone(),
                    type_args.clone(),
                    env_key(&caller_env),
                    boundary,
                ))
            {
                // Changed origins on a recursive instance have no finite
                // receiver-writeback proof. Keep its formals conservative.
                for values in caller_env.values_mut() {
                    *values = vec![ResolvedValue::Unknown];
                }
            }
            if !visited.insert((
                caller.clone(),
                type_args.clone(),
                env_key(&caller_env),
                boundary,
            )) {
                continue;
            }
            ancestors.push((caller.clone(), type_args.clone()));
            let caller_source = match &caller {
                CallableNode::Declaration(id) | CallableNode::Generator(id) => output
                    .defs
                    .module(*id)
                    .and_then(|module| output.defs.module_source(module))
                    .map(std::path::Path::to_path_buf),
                CallableNode::Closure(span) | CallableNode::GeneratorBlock(span) => {
                    source_path(program, span.module_idx)
                }
            };
            let caller_is_std = caller_source
                .as_deref()
                .is_some_and(|path| stdlib_roots.iter().any(|root| path_is_below(path, root)));
            if let CallableNode::Declaration(id) = &caller {
                let deferred = CallableNode::Generator(*id);
                if output.suspension_effects.bodies.contains_key(&deferred) {
                    queue.push_back((
                        deferred,
                        user_site.clone(),
                        type_args.clone(),
                        caller_env.clone(),
                        ancestors.clone(),
                        boundary,
                    ));
                }
            }
            for body in output
                .suspension_effects
                .constructed_bodies
                .get(&caller)
                .into_iter()
                .flatten()
            {
                queue.push_back((
                    body.clone(),
                    user_site.clone(),
                    type_args.clone(),
                    caller_env.clone(),
                    ancestors.clone(),
                    boundary,
                ));
            }

            let mut implicit_releases = Vec::new();
            if confined && checking {
                let facts = value_facts
                    .as_mut()
                    .expect("confinement owns its value-fact service");
                let mut implicit = Vec::new();
                for (span, ty) in &output.suspension_effects.iteration_operations {
                    if output.suspension_effects.call_owners.get(span) != Some(&caller) {
                        continue;
                    }
                    let ty = ty.substitute_type_params_parallel(&substitutions);
                    if unbound_parameter(&ty, &substitutions) {
                        continue;
                    }
                    if ty.named_builtin() == Some(hew_types::BuiltinType::Generator) {
                        let candidates = receiver_actual(output, span)
                            .cloned()
                            .unwrap_or_else(IndirectCallCandidates::unknown);
                        implicit_releases.extend(output.owned_release_targets(
                            &ty,
                            &HashMap::new(),
                            &candidates,
                        ));
                    } else if let Some(calls) = iteration_calls(output, &ty) {
                        implicit_releases.extend(output.owned_release_targets(
                            &calls.iterator,
                            &HashMap::new(),
                            &IndirectCallCandidates::unknown(),
                        ));
                        implicit.extend(
                            calls
                                .calls
                                .into_iter()
                                .map(|(method, args)| (span.clone(), method, args)),
                        );
                    } else if let Some((site, source, filename)) =
                        site_for_call(program, root_source, root_label, &options.documents, span)
                    {
                        diagnostics.push(confinement_diagnostic(
                            output,
                            diagnostic_root,
                            site,
                            &source,
                            &filename,
                            "an iterator protocol without a checked authority summary",
                        ));
                    }
                }
                for (span, ty) in &output.suspension_effects.rendering_operations {
                    if output.suspension_effects.call_owners.get(span) != Some(&caller) {
                        continue;
                    }
                    let ty = ty
                        .substitute_type_params_parallel(&substitutions)
                        .materialize_literal_defaults();
                    if unbound_parameter(&ty, &substitutions) {
                        continue;
                    }
                    let calls = ResolvedTy::from_ty(&ty)
                        .ok()
                        .and_then(|ty| facts.rendering_calls(&ty).ok());
                    if let Some(calls) = calls {
                        implicit.extend(calls.into_iter().map(|(method, args)| {
                            (
                                span.clone(),
                                method,
                                args.iter().map(ResolvedTy::to_ty).collect::<Vec<_>>(),
                            )
                        }));
                    } else if let Some((site, source, filename)) =
                        site_for_call(program, root_source, root_label, &options.documents, span)
                    {
                        diagnostics.push(confinement_diagnostic(
                            output,
                            diagnostic_root,
                            site,
                            &source,
                            &filename,
                            "a Display implementation without a checked authority summary",
                        ));
                    }
                }
                let mut value_calls = Vec::new();
                for call in graph.get(&caller).into_iter().flatten() {
                    let family = match call.target {
                        CallTarget::Runtime(family)
                        | CallTarget::DeclaredRuntime { family, .. } => Some(family),
                        CallTarget::RuntimeCollection(method) => method.runtime_family(),
                        _ => None,
                    };
                    if let Some(family) = family {
                        value_calls.push((call.span.clone(), family));
                    }
                }
                for (span, (read, write)) in &output.indexed_place_operations {
                    if output.suspension_effects.call_owners.get(span) == Some(&caller) {
                        value_calls.extend([(span.clone(), *read), (span.clone(), *write)]);
                    }
                }
                let mut value_demands: Vec<_> = output
                    .suspension_effects
                    .value_operations
                    .iter()
                    .filter(|(span, _)| {
                        output.suspension_effects.call_owners.get(*span) == Some(&caller)
                    })
                    .map(|(span, (ty, capability))| (span.clone(), ty.clone(), *capability))
                    .collect();
                for (span, family) in value_calls {
                    let capabilities = family.value_callback_capabilities();
                    if capabilities.is_empty() {
                        continue;
                    }
                    let element = output
                        .suspension_effects
                        .receivers
                        .get(&span)
                        .and_then(|ty| match ty {
                            Ty::Named { args, .. } => args.first().cloned(),
                            _ => None,
                        });
                    for capability in capabilities {
                        value_demands.push((
                            span.clone(),
                            element.clone().unwrap_or(Ty::Error),
                            *capability,
                        ));
                    }
                }
                for (span, ty, capability) in value_demands {
                    let ty = ty
                        .substitute_type_params_parallel(&substitutions)
                        .materialize_literal_defaults();
                    if unbound_parameter(&ty, &substitutions) {
                        continue;
                    }
                    let selection = ResolvedTy::from_ty(&ty)
                        .ok()
                        .and_then(|ty| facts.capability_plan(&ty, capability).ok())
                        .flatten();
                    if let Some(selection) = selection {
                        for (method, args) in selection.calls() {
                            implicit.push((
                                span.clone(),
                                *method,
                                args.iter().map(ResolvedTy::to_ty).collect(),
                            ));
                        }
                    } else if let Some((site, source, filename)) =
                        site_for_call(program, root_source, root_label, &options.documents, &span)
                    {
                        diagnostics.push(confinement_diagnostic(
                            output,
                            diagnostic_root,
                            site,
                            &source,
                            &filename,
                            "a value callback without a checked authority summary",
                        ));
                    }
                }
                for (span, method, args) in implicit {
                    let site =
                        site_for_call(program, root_source, root_label, &options.documents, &span)
                            .or_else(|| user_site.clone());
                    queue.push_back((
                        CallableNode::Declaration(method),
                        site,
                        args,
                        callee_env(output, &span, method, &caller_env),
                        ancestors.clone(),
                        boundary,
                    ));
                }
            }
            if confined && checking {
                let mut releases: VecDeque<_> = output
                    .suspension_effects
                    .releases
                    .iter()
                    .filter(|effect| effect.owner == caller)
                    .flat_map(|effect| output.release_targets(effect, &substitutions))
                    .map(|target| (target, caller_env.clone()))
                    .collect();
                releases.extend(
                    implicit_releases
                        .into_iter()
                        .map(|target| (target, caller_env.clone())),
                );
                let mut released_callables = HashSet::new();
                while let Some((release, release_env)) = releases.pop_front() {
                    use hew_types::check::cleanup_effects::CleanupTarget;
                    match release {
                        CleanupTarget::Body {
                            body,
                            type_args: cleanup_args,
                            candidates,
                            ..
                        } => {
                            let mut next_env = release_env.clone();
                            if let Some(formal) = output
                                .callable_formals
                                .get(&body)
                                .and_then(|formals| formals.first())
                            {
                                next_env.insert(*formal, actual_values(&candidates, &release_env));
                            }
                            queue.push_back((
                                body,
                                user_site.clone(),
                                cleanup_args,
                                next_env,
                                ancestors.clone(),
                                boundary,
                            ));
                        }
                        CleanupTarget::Callable { ty, candidates } => {
                            for value in resolve_candidates(output, &candidates, &release_env) {
                                match value {
                                    ResolvedValue::Generator { body, captures } => {
                                        queue.push_back((
                                            body,
                                            user_site.clone(),
                                            Vec::new(),
                                            captures.env(),
                                            ancestors.clone(),
                                            boundary,
                                        ));
                                    }
                                    ResolvedValue::Erasure {
                                        site,
                                        value,
                                        captures,
                                    } => {
                                        if let Some(coercion) =
                                            output.dyn_trait_coercions.get(&site)
                                        {
                                            releases.extend(
                                                output
                                                    .owned_release_targets(
                                                        &coercion.concrete_type,
                                                        &captures.types.iter().cloned().collect(),
                                                        &value,
                                                    )
                                                    .into_iter()
                                                    .map(|target| (target, captures.env())),
                                            );
                                        } else {
                                            releases.push_back((
                                                CleanupTarget::Unknown(Ty::Error),
                                                captures.env(),
                                            ));
                                        }
                                    }
                                    ResolvedValue::Declaration(_) | ResolvedValue::Parameter(_) => {
                                    }
                                    ResolvedValue::Closure { span, captures } => {
                                        if released_callables
                                            .insert((span.clone(), captures.clone()))
                                        {
                                            if output.expr_types.contains_key(&span) {
                                                releases.extend(
                                                    output
                                                        .suspension_effects
                                                        .releases
                                                        .iter()
                                                        .filter(|effect| {
                                                            effect.captured
                                                                && effect.owner
                                                                    == CallableNode::Closure(
                                                                        span.clone(),
                                                                    )
                                                        })
                                                        .flat_map(|effect| {
                                                            output.release_targets(
                                                                effect,
                                                                &captures
                                                                    .types
                                                                    .iter()
                                                                    .cloned()
                                                                    .collect(),
                                                            )
                                                        })
                                                        .map(|target| (target, captures.env())),
                                                );
                                            } else if let Some((site, source, filename)) =
                                                user_site.clone().or_else(|| {
                                                    body_site(
                                                        program,
                                                        output,
                                                        diagnostic_root,
                                                        root_source,
                                                        root_label,
                                                        options,
                                                    )
                                                })
                                            {
                                                diagnostics.push(confinement_diagnostic(output, diagnostic_root, site, &source, &filename, "cleanup of a callable without checked captures"));
                                            }
                                        }
                                    }
                                    _ => {
                                        if let Some((site, source, filename)) =
                                            user_site.clone().or_else(|| {
                                                body_site(
                                                    program,
                                                    output,
                                                    diagnostic_root,
                                                    root_source,
                                                    root_label,
                                                    options,
                                                )
                                            })
                                        {
                                            let reason = if matches!(ty, Ty::TraitObject { .. })
                                                || ty.named_builtin()
                                                    == Some(hew_types::BuiltinType::Generator)
                                            {
                                                format!("cleanup of `{}` without a checked authority summary", ty.user_facing())
                                            } else {
                                                "cleanup of a callable with unknown captured authority".into()
                                            };
                                            diagnostics.push(confinement_diagnostic(
                                                output,
                                                diagnostic_root,
                                                site,
                                                &source,
                                                &filename,
                                                &reason,
                                            ));
                                        }
                                    }
                                }
                            }
                        }
                        CleanupTarget::Unknown(ty) => {
                            if unbound_parameter(&ty, &substitutions) {
                                continue;
                            }
                            if let Some((site, source, filename)) =
                                user_site.clone().or_else(|| {
                                    body_site(
                                        program,
                                        output,
                                        diagnostic_root,
                                        root_source,
                                        root_label,
                                        options,
                                    )
                                })
                            {
                                diagnostics.push(confinement_diagnostic(
                                    output,
                                    diagnostic_root,
                                    site,
                                    &source,
                                    &filename,
                                    &format!(
                                        "cleanup of `{}` without a checked authority summary",
                                        ty.user_facing()
                                    ),
                                ));
                            }
                        }
                        CleanupTarget::Suspends => {}
                    }
                }
            }
            for call in graph.get(&caller).into_iter().flatten() {
                let current = site_for_call(
                    program,
                    root_source,
                    root_label,
                    &options.documents,
                    &call.span,
                );
                let selected_site = if caller_is_std {
                    user_site.clone().or(current)
                } else {
                    current.or_else(|| user_site.clone())
                };
                if confined {
                    if let Some(reason) = confined_primitive_refusal(output, &call.target) {
                        if checking {
                            if let Some((site, source, filename)) = selected_site.clone() {
                                if reported_confined.insert((
                                    diagnostic_root,
                                    filename.clone(),
                                    site.span.start,
                                    reason,
                                )) {
                                    diagnostics.push(confinement_diagnostic(
                                        output,
                                        diagnostic_root,
                                        site,
                                        &source,
                                        &filename,
                                        reason,
                                    ));
                                }
                            }
                        }
                        continue;
                    }
                    if matches!(
                        call.target,
                        CallTarget::Runtime(_)
                            | CallTarget::DeclaredRuntime { .. }
                            | CallTarget::Builtin { .. }
                            | CallTarget::RuntimeCollection(_)
                            | CallTarget::RecordConstructor(_)
                            | CallTarget::Extern { .. }
                    ) {
                        // The selected primitive's row owns its effects. Value
                        // callbacks were queued through TypeFactService above.
                        continue;
                    }
                    // A supplied actor reference grants invocation of that service;
                    // its private construction authority is not inherited here.
                    if output.actor_method_dispatch.contains_key(&call.span)
                        || output.actor_delivery_calls.contains_key(&call.span)
                    {
                        continue;
                    }
                }
                if !confined
                    && matches!(
                        call.target,
                        CallTarget::Extern {
                            trusted_compiled_stdlib: false,
                            ..
                        }
                    )
                {
                    if let Some((site, source, filename)) = selected_site {
                        if reported_extern.insert((root, filename.clone(), site.span.start)) {
                            diagnostics.push(opaque_extern_diagnostic(
                                output,
                                root,
                                site,
                                &source,
                                &filename,
                                &options.host_admission,
                            ));
                        }
                    }
                } else if let Some(operation) = (!confined)
                    .then(|| {
                        target_operation(
                            output,
                            &call.target,
                            &stdlib_roots,
                            &options.host_admission,
                        )
                    })
                    .flatten()
                {
                    if let Some((site, source, filename)) = selected_site {
                        if !reported.insert((
                            root,
                            filename.clone(),
                            site.span.start,
                            operation.capability,
                        )) {
                            continue;
                        }
                        let name = output.defs.name(root);
                        let message = if generating {
                            format!(
                                "generator `{name}` reaches refused host operation `{}` ({})",
                                site.spelling, operation.capability
                            )
                        } else {
                            format!(
                            "`{}` reaches host operation `{}` ({}), which a deterministic entry cannot schedule",
                            name,
                            site.spelling,
                            operation.capability
                        )
                        };
                        let mut diagnostic = FrontendDiagnostic::coded_message_at(
                            if generating {
                                "E_GENERATE_HOST_OPERATION"
                            } else {
                                "E_DETERMINISTIC_HOST_OPERATION"
                            },
                            message,
                            site.span,
                            &source,
                            &filename,
                        );
                        if let crate::FrontendDiagnosticKind::Message(detail) = &mut diagnostic.kind
                        {
                            let help = match options.host_admission {
                                HostAdmission::ProcessEntry => "run without `--deterministic` or remove the host operation from reachable calls".to_string(),
                                HostAdmission::Tests(_) => format!("mark `{name}` `#[real_time]` or remove the host operation from its reachable calls"),
                                HostAdmission::Generate => "remove the host operation from the generator's reachable calls".to_string(),
                                HostAdmission::Off => unreachable!("admission has no selected roots when off"),
                            };
                            detail.help.push(help);
                        }
                        diagnostics.push(diagnostic);
                    }
                } else if matches!(call.target, CallTarget::IndirectFunctionValue) {
                    let values = output
                        .indirect_call_candidates
                        .get(&call.span)
                        .map(|candidates| resolve_candidates(output, candidates, &caller_env));
                    if checking
                        && values.as_ref().is_none_or(|values| {
                            values.iter().any(|value| {
                                matches!(
                                    value,
                                    ResolvedValue::Unknown
                                        | ResolvedValue::Aggregate { .. }
                                        | ResolvedValue::Source { .. }
                                )
                            })
                        })
                    {
                        if let Some((site, source, filename)) = selected_site.clone() {
                            if reported_indirect.insert((root, filename.clone(), site.span.start)) {
                                let name = output.defs.name(diagnostic_root);
                                let mut diagnostic = FrontendDiagnostic::coded_message_at(
                                    if confined { "E_CONFINED_INDIRECT_CALL" } else if generating { "E_GENERATE_INDIRECT_CALL" } else { "E_DETERMINISTIC_INDIRECT_CALL" },
                                    format!(
                                        "`{name}` reaches indirect call `{}` whose callable target is not fully known",
                                        site.spelling
                                    ),
                                    site.span,
                                    &source,
                                    &filename,
                                );
                                if let crate::FrontendDiagnosticKind::Message(detail) =
                                    &mut diagnostic.kind
                                {
                                    let help = if confined {
                                        "supply a callable whose transitive authority is known at the confined boundary".to_string()
                                    } else {
                                        match options.host_admission {
                                        HostAdmission::ProcessEntry => "run without `--deterministic` or call a locally known function".to_string(),
                                        HostAdmission::Tests(_) => format!("mark `{name}` `#[real_time]` or call a locally known function"),
                                        HostAdmission::Generate => "call a function whose target is known during generator admission".to_string(),
                                        HostAdmission::Off => unreachable!("admission has no selected roots when off"),
                                    }
                                    };
                                    detail.help.push(help);
                                }
                                diagnostics.push(diagnostic);
                            }
                        }
                    }
                    if let Some(values) = &values {
                        for value in values {
                            match value {
                                ResolvedValue::Declaration(id) => {
                                    let next_env = callee_env(output, &call.span, *id, &caller_env);
                                    queue.push_back((
                                        CallableNode::Declaration(*id),
                                        selected_site.clone(),
                                        Vec::new(),
                                        next_env,
                                        ancestors.clone(),
                                        boundary,
                                    ));
                                }
                                ResolvedValue::Closure { span, captures } => {
                                    queue.push_back((
                                        CallableNode::Closure(span.clone()),
                                        selected_site.clone(),
                                        output
                                            .call_type_args
                                            .get(&call.span)
                                            .into_iter()
                                            .flatten()
                                            .map(|ty| {
                                                ty.substitute_type_params_parallel(&substitutions)
                                            })
                                            .collect(),
                                        closure_env(
                                            output,
                                            &call.span,
                                            span,
                                            captures,
                                            &caller_env,
                                        ),
                                        ancestors.clone(),
                                        boundary,
                                    ));
                                }
                                ResolvedValue::Aggregate { .. }
                                | ResolvedValue::Erasure { .. }
                                | ResolvedValue::Generator { .. }
                                | ResolvedValue::Source { .. }
                                | ResolvedValue::Unknown
                                | ResolvedValue::Parameter(_) => {}
                            }
                        }
                    }
                } else if let Some(next) = target_declaration(&call.target) {
                    let substitutions = if confined {
                        substitutions.clone()
                    } else {
                        match &caller {
                            CallableNode::Declaration(id) | CallableNode::Generator(id) => {
                                output.fn_sigs.get(id).map(|sig| {
                                    sig.type_params
                                        .iter()
                                        .copied()
                                        .zip(type_args.iter().cloned())
                                        .collect()
                                })
                            }
                            CallableNode::Closure(_) | CallableNode::GeneratorBlock(_) => None,
                        }
                        .unwrap_or_default()
                    };
                    let next_args: Vec<_> = output
                        .call_type_args
                        .get(&call.span)
                        .into_iter()
                        .flatten()
                        .map(|ty| ty.substitute_type_params_parallel(&substitutions))
                        .collect();
                    let next_args = if confined && output.defs.kind(next) == DeclarationKind::Actor
                    {
                        output
                            .expr_types
                            .get(&call.span)
                            .and_then(Ty::actor_handle_identity)
                            .map_or(next_args, |(_, args)| {
                                args.iter()
                                    .map(|ty| ty.substitute_type_params_parallel(&substitutions))
                                    .collect()
                            })
                    } else if confined
                        && matches!(&caller, CallableNode::Declaration(id) if output.defs.kind(*id) == DeclarationKind::Actor)
                    {
                        type_args.clone()
                    } else {
                        next_args
                    };
                    let next_env = if confined {
                        confined_callee_env(output, &call.span, next, &caller_env)
                    } else {
                        callee_env(output, &call.span, next, &caller_env)
                    };
                    queue.push_back((
                        CallableNode::Declaration(next),
                        selected_site,
                        next_args,
                        next_env,
                        ancestors.clone(),
                        boundary,
                    ));
                } else if let CallTarget::StaticTraitMethod {
                    declaring_trait,
                    method,
                } = &call.target
                {
                    let (trait_args, method_args) =
                        trait_method_arguments(output, &call.span, &caller_env);
                    let receiver_param = output.method_call_rewrites.get(&call.span).and_then(
                        |rewrite| match rewrite {
                            hew_types::MethodCallRewrite::StaticTraitDispatch {
                                receiver_type_param,
                                ..
                            } => Some(receiver_type_param),
                            hew_types::MethodCallRewrite::BinderStaticCall(call) => {
                                Some(&call.self_param)
                            }
                            _ => None,
                        },
                    );
                    let concrete = if confined {
                        receiver_param
                            .and_then(|parameter| substitutions.get(parameter))
                            .and_then(|ty| ResolvedTy::from_ty(ty).ok())
                            .and_then(|ty| ty.impl_receiver_instance(&output.defs))
                            .map(|instance| {
                                (
                                    instance.nominal,
                                    instance.args.iter().map(ResolvedTy::to_ty).collect(),
                                )
                            })
                    } else {
                        receiver_param
                            .and_then(|param| {
                                output
                                    .fn_sigs
                                    .get(match &caller {
                                        CallableNode::Declaration(id)
                                        | CallableNode::Generator(id) => id,
                                        CallableNode::Closure(_)
                                        | CallableNode::GeneratorBlock(_) => return None,
                                    })?
                                    .type_params
                                    .iter()
                                    .position(|name| name == param)
                            })
                            .and_then(|index| type_args.get(index))
                            .and_then(|ty| ResolvedTy::from_ty(ty).ok())
                            .and_then(|ty| ty.impl_receiver_instance(&output.defs))
                            .map(|instance| {
                                (
                                    instance.nominal,
                                    instance.args.iter().map(ResolvedTy::to_ty).collect(),
                                )
                            })
                    };
                    let nominals: Vec<_> = concrete.map_or_else(
                        || {
                            if confined {
                                return receiver_actual(output, &call.span)
                                    .map(|candidates| {
                                        nominal_actuals(output, candidates, &caller_env)
                                    })
                                    .unwrap_or_default();
                            }
                            output
                                .type_defs
                                .keys()
                                .map(|id| (*id, Vec::new()))
                                .collect()
                        },
                        |concrete| vec![concrete],
                    );
                    let mut matched = false;
                    for (nominal, next_args) in nominals {
                        let instance = hew_types::NominalInstance {
                            nominal,
                            args: next_args
                                .iter()
                                .map(ResolvedTy::from_ty)
                                .collect::<Result<_, _>>()
                                .unwrap_or_default(),
                        };
                        for (candidate, arguments) in service_methods(
                            output,
                            &instance,
                            (*declaring_trait, *method),
                            &trait_args,
                            &method_args,
                            confined,
                        ) {
                            matched = true;
                            let next_env = callee_env(output, &call.span, candidate, &caller_env);
                            queue.push_back((
                                CallableNode::Declaration(candidate),
                                selected_site.clone(),
                                arguments,
                                next_env,
                                ancestors.clone(),
                                boundary,
                            ));
                        }
                    }
                    let deferred_receiver = receiver_param
                        .and_then(|parameter| substitutions.get(parameter))
                        .is_some_and(|ty| unbound_parameter(ty, &substitutions));
                    if checking && confined && !matched && !deferred_receiver {
                        if let Some((site, source, filename)) = selected_site.clone() {
                            diagnostics.push(confinement_diagnostic(output, diagnostic_root, site, &source, &filename, "a trait service implementation whose authority is not known in this construction"));
                        }
                    }
                } else if let CallTarget::DynamicVtable { method, .. } = &call.target {
                    if confined {
                        let values = receiver_actual(output, &call.span).map_or_else(
                            || vec![ResolvedValue::Unknown],
                            |candidates| resolve_candidates(output, candidates, &caller_env),
                        );
                        for value in values {
                            if matches!(value, ResolvedValue::Parameter(_)) {
                                continue;
                            }
                            let selected = (|| {
                                let ResolvedValue::Erasure { site, captures, .. } = &value else {
                                    return None;
                                };
                                let coercion = output.dyn_trait_coercions.get(site)?;
                                let slot = output
                                    .trait_object_layouts
                                    .get(&coercion.target)?
                                    .slots
                                    .iter()
                                    .find(|slot| slot.method == *method)?;
                                let substitutions = captures.types.iter().cloned().collect();
                                let trait_args: Vec<_> = slot
                                    .trait_args
                                    .iter()
                                    .map(|ty| ty.substitute_type_params_parallel(&substitutions))
                                    .collect();
                                let receiver =
                                    ResolvedTy::from_ty(&actual_type(output, &value)?).ok()?;
                                let instance = receiver.impl_receiver_instance(&output.defs)?;
                                let mut calls = Vec::new();
                                for (owner, candidate) in output.dispatch.candidates(
                                    instance.nominal,
                                    Some(&instance),
                                    output.defs.name(*method),
                                ) {
                                    if !service_method_owner(
                                        output,
                                        owner,
                                        instance.nominal,
                                        candidate,
                                        slot.declaring_trait,
                                        *method,
                                    ) {
                                        continue;
                                    }
                                    let arguments = method_instance_arguments(
                                        output,
                                        candidate,
                                        &receiver,
                                        &trait_args,
                                        &[],
                                    )?;
                                    calls.push((candidate, arguments));
                                }
                                (!calls.is_empty()).then_some(calls)
                            })();
                            if let Some(calls) = selected {
                                for (candidate, arguments) in calls {
                                    let mut env =
                                        callee_env(output, &call.span, candidate, &caller_env);
                                    if let Some(formal) = output
                                        .callable_formals
                                        .get(&CallableNode::Declaration(candidate))
                                        .and_then(|formals| formals.first())
                                    {
                                        env.insert(*formal, vec![value.clone()]);
                                    }
                                    queue.push_back((
                                        CallableNode::Declaration(candidate),
                                        selected_site.clone(),
                                        arguments,
                                        env,
                                        ancestors.clone(),
                                        boundary,
                                    ));
                                }
                            } else if checking {
                                if let Some((site, source, filename)) = selected_site.clone() {
                                    diagnostics.push(confinement_diagnostic(output, diagnostic_root, site, &source, &filename, "a dynamic service implementation without a checked authority summary"));
                                }
                            }
                        }
                        continue;
                    }
                    // A dyn call can reach the filler of its slot in every
                    // erasure whose layout carries the method: the concrete
                    // type's own method of that name or its impl of the
                    // declaring trait (nominal, default or structural).
                    let method_name = output.defs.name(*method);
                    let candidates: Vec<_> = output
                        .dyn_trait_coercions
                        .values()
                        .filter_map(|coercion| {
                            let slot = output
                                .trait_object_layouts
                                .get(&coercion.target)?
                                .slots
                                .iter()
                                .find(|slot| slot.method == *method)?;
                            let instance = ResolvedTy::from_ty(&coercion.concrete_type)
                                .ok()?
                                .impl_receiver_instance(&output.defs)?;
                            Some((slot.declaring_trait, instance.nominal))
                        })
                        .flat_map(|(declaring_trait, nominal)| {
                            output.dispatch.methods_of(nominal).filter_map(
                                move |(name, owner, candidate)| {
                                    (name == method_name
                                        && (owner
                                            == hew_types::check::dispatch_table::MethodOwner::Inherent
                                            || owner
                                                == hew_types::check::dispatch_table::MethodOwner::Trait(
                                                    declaring_trait,
                                                )))
                                    .then_some(candidate)
                                },
                            )
                        })
                        .collect();
                    if checking && confined && candidates.is_empty() {
                        if let Some((site, source, filename)) = selected_site.clone() {
                            diagnostics.push(confinement_diagnostic(output, diagnostic_root, site, &source, &filename, "a dynamic service implementation without a checked authority summary"));
                        }
                    }
                    for candidate in candidates {
                        let next_env = callee_env(output, &call.span, candidate, &caller_env);
                        queue.push_back((
                            CallableNode::Declaration(candidate),
                            selected_site.clone(),
                            Vec::new(),
                            next_env,
                            ancestors.clone(),
                            boundary,
                        ));
                    }
                }
            }
        }
    }
    diagnostics
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{check_file, FrontendDiagnosticKind};

    #[test]
    fn generator_admits_files_arguments_and_seeded_services() {
        check_source(
            "import std.fs; import std.os; import std.random; import std.time.datetime;\nfn main() { let args = os.args(); println(args.len()); let text = fs.read(\"schema.json\").expect(\"read\"); let _ = fs.write(\"generated/model.hew\", text); println(random.random()); println(datetime.now_ms()); }",
            HostAdmission::Generate,
        ).unwrap();
    }

    #[test]
    fn generator_refuses_host_access_through_direct_and_indirect_calls() {
        for source in [
            "import std.os; fn main() { let _ = os.env(\"PATH\"); }",
            "import std.process; fn main() { let _ = process.run(\"true\"); }",
            "import std.stream; fn main() { let _ = stream.open(\"secret\"); }",
            "import std.path; fn main() { let _ = path.glob(\"/*\"); }",
            "import std.os; fn main() { let read = || os.env(\"PATH\"); let _ = read(); }",
            "import std.os; fn invoke(f: fn() -> Option<string>) { let _ = f(); } fn main() { invoke(|| os.env(\"PATH\")); }",
            "import std.os; fn empty() -> Option<string> { .None } fn read() -> Option<string> { os.env(\"PATH\") } type Holder { action: fn[clone]() -> Option<string>; } impl Holder { fn replace(var self) { self.action = read; } fn through_helper(var self, n: i64) { self.replace(); if n > 0 { self.through_helper(n - 1); } } } fn main() { var holder = Holder { action: empty }; for _index in 0..2 { let _ = holder.action(); holder.through_helper(1); } }",
            "import std.os; actor Worker { receive fn inspect() { let _ = os.env(\"PATH\"); } } fn main() { let worker = spawn Worker; let _ = worker.inspect(); stop(worker); }",
            "import std.os; actor Worker { #[on(start)] fn boot() { let _ = os.env(\"PATH\"); } } fn main() { let worker = spawn Worker; stop(worker); }",
            "import std.os; #[resource] type Token { value: i64; } impl Token { fn close(consume self) { let _ = os.env(\"PATH\"); } } fn main() { let token = Token { value: 1 }; }",
            "import std.os; trait Reader { fn read(self) -> i64 { let _ = os.env(\"PATH\"); 1 } } type Token {} impl Reader for Token {} fn main() { let token = Token {}; println(token.read()); }",
            "import std.os; type Token {} impl Display for Token { fn fmt(self) -> string { let _ = os.env(\"PATH\"); \"token\" } } fn main() { let token = Token {}; println(f\"{token}\"); }",
        ] {
            let failure = check_source(source, HostAdmission::Generate).expect_err(source);
            assert!(failure.contains("E_GENERATE_HOST_OPERATION"), "{source}: {failure}");
        }
    }

    #[test]
    fn generator_refuses_unreachable_user_extern_declarations() {
        let failure = check_source(
            "extern \"C\" { fn getpid() -> i64; } fn main() { println(\"safe\"); }",
            HostAdmission::Generate,
        )
        .unwrap_err();
        assert!(failure.contains("E_GENERATE_EXTERN"), "{failure}");
    }

    #[test]
    fn generator_requires_typecheck() {
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("generate.hew");
        std::fs::write(&path, "fn main() {}").unwrap();
        let options = FrontendOptions {
            no_typecheck: true,
            host_admission: HostAdmission::Generate,
            ..FrontendOptions::default()
        };
        let failure = check_file(path.to_str().unwrap(), &options).unwrap_err();
        assert!(failure.diagnostics.iter().any(|diagnostic| matches!(&diagnostic.kind, FrontendDiagnosticKind::Message(message) if message.code == "E_GENERATE_TYPECHECK_REQUIRED")));
    }

    fn check_source(source: &str, admission: HostAdmission) -> Result<(), String> {
        let directory = tempfile::tempdir().expect("temporary source directory");
        let path = directory.path().join("admission.hew");
        std::fs::write(&path, source).expect("write source");
        let options = FrontendOptions {
            host_admission: admission,
            ..FrontendOptions::default()
        };
        match check_file(path.to_str().expect("UTF-8 path"), &options) {
            Ok(_) => Ok(()),
            Err(failure) => {
                let messages = failure
                    .diagnostics
                    .iter()
                    .filter_map(|diagnostic| match &diagnostic.kind {
                        FrontendDiagnosticKind::Message(message) => {
                            Some(format!("{}: {}", message.code, message.message))
                        }
                        FrontendDiagnosticKind::Type(error) => Some(format!("{error:?}")),
                        _ => None,
                    })
                    .collect::<Vec<_>>();
                Err(format!("{}: {messages:?}", failure.message))
            }
        }
    }

    fn selected_test(source: &str, name: &str) -> DeclarationOccurrence {
        let parsed = hew_parser::parse(source);
        assert!(parsed.errors.is_empty(), "{:#?}", parsed.errors);
        let (_, span) = parsed
            .program
            .items
            .iter()
            .find(|(item, _)| matches!(item, Item::Function(function) if function.name.name.as_str() == name))
            .expect("selected test declaration");
        DeclarationOccurrence::new(None, span, DeclarationKind::Function, 0)
    }

    #[test]
    fn process_entry_refuses_reachable_tcp_listen() {
        let source = "import std.net;\n\nfn helper() {\n    match net.listen(\":0\") {\n        .Ok(_) => {}\n        .Err(_) => {}\n    }\n}\n\nfn main() {\n    helper();\n}\n";
        let failure = check_source(source, HostAdmission::ProcessEntry).unwrap_err();
        assert!(
            failure.contains("E_DETERMINISTIC_HOST_OPERATION"),
            "{failure}"
        );
        assert!(failure.contains("net.listen"), "{failure}");
    }

    #[test]
    fn unreachable_host_helper_does_not_block_process_entry() {
        let source = "import std.net;\n\nfn unused() {\n    match net.listen(\":0\") {\n        .Ok(_) => {}\n        .Err(_) => {}\n    }\n}\n\nfn main() {\n    println(\"safe\");\n}\n";
        check_source(source, HostAdmission::ProcessEntry).unwrap();
    }

    #[test]
    fn aliased_std_module_keeps_its_host_identity() {
        let source = "import std.net as wire;\n\nfn main() {\n    match wire.listen(\":0\") {\n        .Ok(_) => {}\n        .Err(_) => {}\n    }\n}\n";
        let failure = check_source(source, HostAdmission::ProcessEntry).unwrap_err();
        assert!(
            failure.contains("E_DETERMINISTIC_HOST_OPERATION"),
            "{failure}"
        );
        assert!(failure.contains("wire.listen"), "{failure}");
    }

    #[test]
    fn a_user_function_with_a_host_like_name_is_admitted() {
        let source = "fn listen() -> i64 { 7 }\nfn main() { println(listen()); }";
        check_source(source, HostAdmission::ProcessEntry).unwrap();
    }

    #[test]
    fn selected_test_does_not_inherit_real_time_siblings_operation() {
        let source = "import std.net;\n\n#[test]\nfn safe() {\n    println(\"safe\");\n}\n\n#[test]\n#[real_time]\nfn socket() {\n    match net.listen(\":0\") {\n        .Ok(_) => {}\n        .Err(_) => {}\n    }\n}\n";
        let selection = selected_test(source, "safe");
        check_source(source, HostAdmission::Tests(vec![selection])).unwrap();
    }

    #[test]
    fn selected_test_refuses_stdin_read() {
        let source =
            "import std.io;\n#[test] fn input() { let line = io.read_line() ?? \"\"; println(line); }";
        let selection = selected_test(source, "input");
        let failure = check_source(source, HostAdmission::Tests(vec![selection])).unwrap_err();
        assert!(
            failure.contains("E_DETERMINISTIC_HOST_OPERATION"),
            "{failure}"
        );
        assert!(failure.contains("io.read_line"), "{failure}");
    }

    #[test]
    fn dynamic_trait_call_includes_checked_implementer() {
        let source = "import std.io;\n\ntrait Reader {\n    fn[suspends] read(self) -> string;\n}\n\ntype Host {\n    n: i64;\n}\n\nimpl Host {\n    fn read(self) -> string {\n        io.read_line() ?? \"\"\n    }\n}\n\nfn inspect(value: dyn Reader) -> string {\n    value.read()\n}\n\nfn main() {\n    let erased: dyn Reader = Host { n: 1 };\n    println(inspect(erased));\n}\n";
        let failure = check_source(source, HostAdmission::ProcessEntry).unwrap_err();
        assert!(
            failure.contains("E_DETERMINISTIC_HOST_OPERATION"),
            "{failure}"
        );
    }

    #[test]
    fn static_trait_dispatch_includes_checked_implementer() {
        let source = "import std.io;\n\ntrait Reader {\n    fn read(self) -> string;\n}\n\ntype Host {\n    n: i64;\n}\n\nimpl Host {\n    fn read(self) -> string {\n        io.read_line() ?? \"\"\n    }\n}\n\nfn inspect<T: Reader>(value: T) -> string {\n    value.read()\n}\n\nfn main() {\n    println(inspect(Host { n: 1 }));\n}\n";
        let failure = check_source(source, HostAdmission::ProcessEntry).unwrap_err();
        assert!(
            failure.contains("E_DETERMINISTIC_HOST_OPERATION"),
            "{failure}"
        );
    }

    #[test]
    fn static_trait_dispatch_excludes_unused_implementer() {
        let source = "import std.io;\n\ntrait Reader {\n    fn read(self) -> string;\n}\n\ntype Safe {\n    n: i64;\n}\n\nimpl Safe {\n    fn read(self) -> string {\n        \"safe\"\n    }\n}\n\ntype Host {\n    n: i64;\n}\n\nimpl Host {\n    fn read(self) -> string {\n        io.read_line() ?? \"\"\n    }\n}\n\nfn inspect<T: Reader>(value: T) -> string {\n    value.read()\n}\n\nfn main() {\n    println(inspect(Safe { n: 1 }));\n}\n";
        check_source(source, HostAdmission::ProcessEntry).unwrap();
    }

    #[test]
    fn remote_actor_send_and_ask_require_real_time() {
        let declaration = "#[wire]\ntype Ping {\n    n: i64 @1;\n}\n\nactor Worker {\n    receive fn ping(msg: Ping) -> i64 {\n        0\n    }\n}\n\nimpl ActorMsg for Worker {\n    type Msg = Ping;\n    type Reply = i64;\n}\n";
        for call in ["pid.send(Ping { n: 0 })", "pid.ask(Ping { n: 0 }, 1000)"] {
            let source =
                format!("{declaration}fn main() {{ let pid = Node.lookup<Worker>(\"worker\").expect(\"lookup\"); let _ = {call}; }}");
            let failure = check_source(&source, HostAdmission::ProcessEntry).unwrap_err();
            assert!(
                failure.contains("E_DETERMINISTIC_HOST_OPERATION"),
                "{failure}"
            );
            assert!(failure.contains("pid."), "{failure}");
        }
    }

    #[test]
    fn one_source_call_reports_one_host_operation() {
        let source = "import std.net;\n\nfn main() {\n    match net.listen(\":0\") {\n        .Ok(_) => {}\n        .Err(_) => {}\n    }\n}\n";
        let failure = check_source(source, HostAdmission::ProcessEntry).unwrap_err();
        assert_eq!(
            failure.matches("E_DETERMINISTIC_HOST_OPERATION").count(),
            1,
            "{failure}"
        );
    }

    #[test]
    fn process_entry_suggests_run_mode_instead_of_test_attribute() {
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("run.hew");
        std::fs::write(
            &path,
            "import std.io;\nfn main() { let line = io.read_line() ?? \"\"; println(line); }",
        )
        .unwrap();
        let options = FrontendOptions {
            host_admission: HostAdmission::ProcessEntry,
            ..FrontendOptions::default()
        };
        let failure = check_file(path.to_str().unwrap(), &options).unwrap_err();
        let help = failure
            .diagnostics
            .iter()
            .find_map(|diagnostic| match &diagnostic.kind {
                FrontendDiagnosticKind::Message(message)
                    if message.code == "E_DETERMINISTIC_HOST_OPERATION" =>
                {
                    Some(&message.help)
                }
                _ => None,
            })
            .expect("deterministic host diagnostic");
        assert!(help.iter().any(|line| line.contains("--deterministic")));
        assert!(!help.iter().any(|line| line.contains("#[real_time]")));
    }

    #[test]
    fn known_lambda_actor_and_delivery_view_are_deterministic() {
        for call in [
            "worker(7)",
            "mailbox(worker)(7)",
            "policy(worker, on_full: .Reject)(7)",
        ] {
            let source = format!("fn main() {{ let factor = 3; let worker = actor |n: i64| {{ if n > 1 {{ let _ = worker(n - 1); }} println(n * factor); }}; let _ = {call}; stop(worker); stopped(worker); }}");
            check_source(&source, HostAdmission::ProcessEntry).unwrap();
        }
    }

    #[test]
    fn lambda_actor_delivery_cannot_hide_a_host_operation() {
        for call in ["worker(7)", "mailbox(worker)(7)"] {
            let source = format!("import std.io; fn main() {{ let worker = actor |n: i64| {{ let line = io.read_line() ?? \"\"; println(line); }}; let _ = {call}; stop(worker); stopped(worker); }}");
            let failure = check_source(&source, HostAdmission::ProcessEntry).unwrap_err();
            assert!(
                failure.contains("E_DETERMINISTIC_HOST_OPERATION"),
                "{failure}"
            );
            assert!(
                !failure.contains("E_DETERMINISTIC_INDIRECT_CALL"),
                "{failure}"
            );
        }
    }

    #[test]
    fn invoked_closure_cannot_hide_stdin_read() {
        for source in [
            "import std.io;\nfn main() { let reader = || io.read_line() ?? \"\"; println(reader()); }",
            "import std.io;\nactor Worker { receive fn read(reader: fn[suspends]() -> string) -> string { reader() } } fn main() { let worker = spawn Worker; let _ = worker.read(|| io.read_line() ?? \"\"); stop(worker); }",
        ] {
            let failure = check_source(source, HostAdmission::ProcessEntry).unwrap_err();
            assert!(
                failure.contains("E_DETERMINISTIC_HOST_OPERATION"),
                "{failure}"
            );
            assert!(
                !failure.contains("E_DETERMINISTIC_INDIRECT_CALL"),
                "{failure}"
            );
        }
    }

    #[test]
    fn unused_closure_does_not_admit_its_host_operation() {
        let source =
            "import std.io;\nfn main() { let unused = || io.read_line() ?? \"\"; println(\"safe\"); }";
        check_source(source, HostAdmission::ProcessEntry).unwrap();
    }

    #[test]
    fn selected_branch_closure_reaches_host_operation() {
        let source = "import std.io;\nfn main() { let reader: fn[suspends]() -> string = if true { || \"safe\" } else { || io.read_line() ?? \"\" }; println(reader()); }";
        let failure = check_source(source, HostAdmission::ProcessEntry).unwrap_err();
        assert!(
            failure.contains("E_DETERMINISTIC_HOST_OPERATION"),
            "{failure}"
        );
    }

    #[test]
    fn opaque_function_parameter_is_refused() {
        let source = "fn call_reader(reader: fn() -> string) { println(reader()); }";
        let selection = selected_test(source, "call_reader");
        let failure = check_source(source, HostAdmission::Tests(vec![selection])).unwrap_err();
        assert!(
            failure.contains("E_DETERMINISTIC_INDIRECT_CALL"),
            "{failure}"
        );
    }

    #[test]
    fn known_function_parameter_is_admitted_from_its_call_site() {
        for source in [
            "fn call_reader(reader: fn() -> string) { println(reader()); }\nfn main() { call_reader(|| \"safe\"); }",
            "fn repeat(reader: fn[clone]() -> string, n: i64) -> string { if n == 0 { return reader(); } repeat(reader, n - 1) } fn main() { println(repeat(|| \"safe\", 3)); }",
        ] {
            check_source(source, HostAdmission::ProcessEntry).unwrap();
        }
    }

    #[test]
    fn known_function_value_is_admitted() {
        for source in [
            "fn answer() -> i64 { 7 }\nfn main() { let selected: fn() -> i64 = answer; println(selected()); }",
            "fn answer() -> i64 { 1 } type Holder { action: fn[clone]() -> i64; } fn main() { let holder = Holder { action: answer }; let value: i64 = holder.action(); println(value); }",
        ] {
            check_source(source, HostAdmission::ProcessEntry).unwrap();
        }
    }

    #[test]
    fn user_extern_without_a_manifest_capability_is_refused() {
        let source = "extern \"C\" { fn getpid() -> i64; }\nfn main() { let pid = unsafe { getpid() }; println(pid); }";
        let failure = check_source(source, HostAdmission::ProcessEntry).unwrap_err();
        assert!(
            failure.contains("E_DETERMINISTIC_HOST_OPERATION"),
            "{failure}"
        );
        assert!(failure.contains("getpid"), "{failure}");
    }

    #[test]
    fn imported_methods_use_their_checked_rewrite() {
        let source = "import std.text.semver;\nfn main() fails semver.VersionError { let version = semver.parse(\"1.2.3-beta\")?; assert(version.pre() == \"beta\"); }";
        check_source(source, HostAdmission::ProcessEntry).unwrap();
    }

    #[test]
    fn imported_iterator_combinators_preserve_callback_origin() {
        let source = "import std.iter;\nfn main() { var v: Vec<string> = Vec.new(); v.push(\"a\"); let result = iter.collect(iter.map(v.into_iter(), |s: string| s + \"!\")); assert(result.len() == 1); }";
        check_source(source, HostAdmission::ProcessEntry).unwrap();
    }

    #[test]
    fn imported_numeric_folds_preserve_concrete_iterator_arguments() {
        check_source(
            "import std.iter; fn main() { let values = [1.0, 2.0].into_iter(); \
             println(iter.sum_f64(values)); }",
            HostAdmission::ProcessEntry,
        )
        .expect("a numeric fold must select the concrete iterator implementation");
    }

    #[test]
    fn collected_iterator_reaches_its_callback_host_operation() {
        let source = "import std.process;\nimport std.iter;\nfn main() { var v: Vec<i64> = Vec.new(); v.push(1); let result = iter.collect(iter.map(v.into_iter(), |n: i64| { let _ = process.start(process.command(\"true\", Vec.new())); n })); assert(result.len() == 1); }";
        let failure = check_source(source, HostAdmission::ProcessEntry).unwrap_err();
        assert!(
            failure.contains("E_DETERMINISTIC_HOST_OPERATION"),
            "{failure}"
        );
        assert!(
            !failure.contains("E_DETERMINISTIC_INDIRECT_CALL"),
            "{failure}"
        );
    }

    #[test]
    fn uncollected_iterator_does_not_invoke_its_callback() {
        let source = "import std.process;\nimport std.iter;\nfn main() { var v: Vec<i64> = Vec.new(); v.push(1); let unused = iter.map(v.into_iter(), |n: i64| { let _ = process.start(process.command(\"true\", Vec.new())); n }); println(\"safe\"); }";
        check_source(source, HostAdmission::ProcessEntry).unwrap();
    }

    #[test]
    fn imported_function_value_uses_checked_declaration() {
        let directory = tempfile::tempdir().expect("temporary source directory");
        let path = directory.path().join("admission.hew");
        std::fs::write(
            directory.path().join("host.hew"),
            "import std.io;\npub fn read() -> string { io.read_line() ?? \"\" }",
        )
        .expect("write imported source");
        std::fs::write(
            &path,
            "import host;\nfn main() { let f: fn[suspends]() -> string = host.read; println(f()); }",
        )
        .expect("write entry source");
        let options = FrontendOptions {
            host_admission: HostAdmission::ProcessEntry,
            ..FrontendOptions::default()
        };
        let failure = check_file(path.to_str().unwrap(), &options).unwrap_err();
        assert!(
            failure.diagnostics.iter().any(|diagnostic| matches!(
                &diagnostic.kind,
                FrontendDiagnosticKind::Message(message)
                    if message.code == "E_DETERMINISTIC_HOST_OPERATION"
            )),
            "{failure:?}"
        );
        assert!(
            !failure.diagnostics.iter().any(|diagnostic| matches!(
                &diagnostic.kind,
                FrontendDiagnosticKind::Message(message)
                    if message.code == "E_DETERMINISTIC_INDIRECT_CALL"
            )),
            "{failure:?}"
        );
    }

    #[test]
    fn deterministic_entry_requires_checked_identity() {
        let failure = check_source("fn helper() {}", HostAdmission::ProcessEntry).unwrap_err();
        assert!(failure.contains("E_DETERMINISTIC_ENTRY"), "{failure}");
    }

    #[test]
    fn host_admission_cannot_be_bypassed_with_no_typecheck() {
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("unchecked.hew");
        std::fs::write(&path, "fn main() {}").unwrap();
        let options = FrontendOptions {
            no_typecheck: true,
            host_admission: HostAdmission::ProcessEntry,
            ..FrontendOptions::default()
        };
        let failure = check_file(path.to_str().unwrap(), &options).unwrap_err();
        assert!(failure.diagnostics.iter().any(|diagnostic| matches!(
            &diagnostic.kind,
            FrontendDiagnosticKind::Message(message)
                if message.code == "E_DETERMINISTIC_TYPECHECK_REQUIRED"
        )));
    }
}
