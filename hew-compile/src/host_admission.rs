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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum CallableNode {
    Declaration(DefId),
    Closure(SpanKey),
}

/// A value origin resolved under its calling context. Aggregate fields and
/// closure captures retain that context when the value crosses another call.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum ResolvedValue {
    Declaration(DefId),
    Closure {
        span: SpanKey,
        captures: Vec<(TypeBindingId, Vec<Self>)>,
    },
    Aggregate {
        span: SpanKey,
        captures: Vec<(TypeBindingId, Vec<Self>)>,
    },
    Source {
        candidates: IndirectCallCandidates,
        captures: Vec<(TypeBindingId, Vec<Self>)>,
    },
    Unknown,
}

type CandidateEnv = HashMap<TypeBindingId, Vec<ResolvedValue>>;

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
}

fn selected_call_declaration(output: &TypeCheckOutput, span: &SpanKey) -> Option<DefId> {
    selected_call_target(output, span).and_then(target_declaration)
}

type Projection = (NominalId, u32);
type CandidateVisit = (
    CallableCandidate,
    Vec<Projection>,
    Vec<(TypeBindingId, Vec<ResolvedValue>)>,
);

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
            ResolvedValue::Source {
                candidates,
                captures,
            } => resolve_candidates_at(
                output,
                candidates,
                &captures.iter().cloned().collect(),
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
                            .find(|field| (field.owner, field.index) == *selected)
                    })
                    .map_or_else(
                        || vec![ResolvedValue::Unknown],
                        |field| {
                            resolve_candidates_at(
                                output,
                                &field.candidates,
                                &captures.iter().cloned().collect(),
                                rest,
                                nominal_only,
                                depth + 1,
                                seen,
                            )
                        },
                    )
            }
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
                .collect::<Vec<_>>();
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
                    for value in receiver {
                        if let ResolvedValue::Aggregate { span, .. } = value {
                            if let Some(nominal) = output
                                .aggregate_field_candidates
                                .get(&span)
                                .and_then(|fields| fields.first())
                                .map(|field| field.owner)
                            {
                                for (name, owner, declaration) in
                                    output.dispatch.methods_of(nominal)
                                {
                                    if name == output.defs.name(*method) && (owner == hew_types::check::dispatch_table::MethodOwner::Inherent || owner == hew_types::check::dispatch_table::MethodOwner::Trait(*declaring_trait)) && !callees.contains(&declaration) { callees.push(declaration); }
                                }
                            }
                        }
                    }
                }
            }
            let mut values = Vec::new();
            if callees.is_empty() {
                values.push(ResolvedValue::Unknown);
            }
            for callee in callees {
                let body = hew_types::check::effects::EffectBody::Declaration(callee);
                let returned = if matches!(candidate, CallableCandidate::CallWriteback(_)) {
                    output.callable_receiver_outputs.get(&body)
                } else {
                    output.callable_return_candidates.get(&body)
                };
                let next_env = callee_env(output, site, callee, env);
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
        CallableCandidate::Field {
            receiver,
            owner,
            index,
        } => {
            let mut path = projections.to_vec();
            path.push((*owner, *index));
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

fn env_key(env: &CandidateEnv) -> Vec<(TypeBindingId, Vec<ResolvedValue>)> {
    let mut key = env
        .iter()
        .map(|(formal, values)| (*formal, values.clone()))
        .collect::<Vec<_>>();
    key.sort_by_key(|(formal, _)| formal.0);
    key
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
            hew_types::MethodCallRewrite::RemoteActorAsk => CallTarget::Builtin {
                endpoint: "hew_remote_call_new".to_string(),
            },
            hew_types::MethodCallRewrite::RemoteActorSend => CallTarget::Builtin {
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

/// Check each selected root independently using checker-owned call edges.
#[allow(
    clippy::too_many_lines,
    reason = "one breadth-first traversal keeps dispatch candidates and source-site provenance together"
)]
pub(super) fn check(
    program: &Program,
    output: &TypeCheckOutput,
    root_source: &str,
    root_label: &str,
    options: &FrontendOptions,
) -> Vec<FrontendDiagnostic> {
    let generating = matches!(options.host_admission, HostAdmission::Generate);
    let roots = selected_entries(output, options);
    let requested = match &options.host_admission {
        HostAdmission::Off => 0,
        HostAdmission::ProcessEntry | HostAdmission::Generate => 1,
        HostAdmission::Tests(entries) => entries.len(),
    };
    if roots.len() != requested {
        return vec![FrontendDiagnostic::coded_message(
            if generating {
                "E_GENERATE_ENTRY"
            } else {
                "E_DETERMINISTIC_ENTRY"
            },
            "a selected host-admission entry has no checked declaration identity",
        )];
    }
    if roots.is_empty() {
        return Vec::new();
    }
    let graph = call_graph(program, output, generating);
    let stdlib_roots = configured_stdlib_roots(options);
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
    let mut reported_indirect = HashSet::new();
    let mut reported_extern = HashSet::new();
    for root in roots {
        let mut queue = VecDeque::from([(
            CallableNode::Declaration(root),
            None::<(SourceSite, String, String)>,
            Vec::<Ty>::new(),
            CandidateEnv::new(),
            Vec::<(CallableNode, Vec<Ty>)>::new(),
        )]);
        let mut visited = HashSet::new();
        while let Some((caller, user_site, type_args, mut caller_env, mut ancestors)) =
            queue.pop_front()
        {
            if ancestors.contains(&(caller.clone(), type_args.clone()))
                && !visited.contains(&(caller.clone(), type_args.clone(), env_key(&caller_env)))
            {
                // Changed origins on a recursive instance have no finite
                // receiver-writeback proof. Keep its formals conservative.
                for values in caller_env.values_mut() {
                    *values = vec![ResolvedValue::Unknown];
                }
            }
            if !visited.insert((caller.clone(), type_args.clone(), env_key(&caller_env))) {
                continue;
            }
            ancestors.push((caller.clone(), type_args.clone()));
            let caller_source = match &caller {
                CallableNode::Declaration(id) => output
                    .defs
                    .module(*id)
                    .and_then(|module| output.defs.module_source(module))
                    .map(std::path::Path::to_path_buf),
                CallableNode::Closure(span) => source_path(program, span.module_idx),
            };
            let caller_is_std = caller_source
                .as_deref()
                .is_some_and(|path| stdlib_roots.iter().any(|root| path_is_below(path, root)));
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
                if matches!(
                    call.target,
                    CallTarget::Extern {
                        trusted_compiled_stdlib: false,
                        ..
                    }
                ) {
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
                } else if let Some(operation) =
                    target_operation(output, &call.target, &stdlib_roots, &options.host_admission)
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
                    if values.as_ref().is_none_or(|values| {
                        values.iter().any(|value| {
                            matches!(
                                value,
                                ResolvedValue::Unknown
                                    | ResolvedValue::Aggregate { .. }
                                    | ResolvedValue::Source { .. }
                            )
                        })
                    }) {
                        if let Some((site, source, filename)) = selected_site.clone() {
                            if reported_indirect.insert((root, filename.clone(), site.span.start)) {
                                let name = output.defs.name(root);
                                let mut diagnostic = FrontendDiagnostic::coded_message_at(
                                    if generating { "E_GENERATE_INDIRECT_CALL" } else { "E_DETERMINISTIC_INDIRECT_CALL" },
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
                                    let help = match options.host_admission {
                                        HostAdmission::ProcessEntry => "run without `--deterministic` or call a locally known function".to_string(),
                                        HostAdmission::Tests(_) => format!("mark `{name}` `#[real_time]` or call a locally known function"),
                                        HostAdmission::Generate => "call a function whose target is known during generator admission".to_string(),
                                        HostAdmission::Off => unreachable!("admission has no selected roots when off"),
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
                                    ));
                                }
                                ResolvedValue::Closure { span, captures } => {
                                    queue.push_back((
                                        CallableNode::Closure(span.clone()),
                                        selected_site.clone(),
                                        Vec::new(),
                                        captures.iter().cloned().collect(),
                                        ancestors.clone(),
                                    ));
                                }
                                ResolvedValue::Aggregate { .. }
                                | ResolvedValue::Source { .. }
                                | ResolvedValue::Unknown => {}
                            }
                        }
                    }
                } else if let Some(next) = target_declaration(&call.target) {
                    let substitutions = match &caller {
                        CallableNode::Declaration(id) => output.fn_sigs.get(id).map(|sig| {
                            sig.type_params
                                .iter()
                                .copied()
                                .zip(type_args.iter().cloned())
                                .collect()
                        }),
                        CallableNode::Closure(_) => None,
                    }
                    .unwrap_or_default();
                    let next_args = output
                        .call_type_args
                        .get(&call.span)
                        .into_iter()
                        .flatten()
                        .map(|ty| ty.substitute_type_params_parallel(&substitutions))
                        .collect();
                    let next_env = callee_env(output, &call.span, next, &caller_env);
                    queue.push_back((
                        CallableNode::Declaration(next),
                        selected_site,
                        next_args,
                        next_env,
                        ancestors.clone(),
                    ));
                } else if let CallTarget::StaticTraitMethod {
                    declaring_trait,
                    method,
                } = &call.target
                {
                    let method_name = output.defs.name(*method);
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
                    let concrete = receiver_param
                        .and_then(|param| {
                            output
                                .fn_sigs
                                .get(match &caller {
                                    CallableNode::Declaration(id) => id,
                                    CallableNode::Closure(_) => return None,
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
                        });
                    let nominals: Vec<_> = concrete.map_or_else(
                        || {
                            output
                                .type_defs
                                .keys()
                                .map(|id| (*id, Vec::new()))
                                .collect()
                        },
                        |concrete| vec![concrete],
                    );
                    for (nominal, next_args) in nominals {
                        for (name, owner, candidate) in output.dispatch.methods_of(nominal) {
                            if name == method_name
                                && (owner
                                    == hew_types::check::dispatch_table::MethodOwner::Inherent
                                    || owner
                                        == hew_types::check::dispatch_table::MethodOwner::Trait(
                                            *declaring_trait,
                                        ))
                            {
                                let next_env =
                                    callee_env(output, &call.span, candidate, &caller_env);
                                queue.push_back((
                                    CallableNode::Declaration(candidate),
                                    selected_site.clone(),
                                    next_args.clone(),
                                    next_env,
                                    ancestors.clone(),
                                ));
                            }
                        }
                    }
                } else if let CallTarget::DynamicVtable { method, .. } = &call.target {
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
                    for candidate in candidates {
                        let next_env = callee_env(output, &call.span, candidate, &caller_env);
                        queue.push_back((
                            CallableNode::Declaration(candidate),
                            selected_site.clone(),
                            Vec::new(),
                            next_env,
                            ancestors.clone(),
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
