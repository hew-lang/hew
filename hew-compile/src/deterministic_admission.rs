//! Compile-time admission of host operations for selected deterministic roots.
//!
//! The capability manifest owns the operation set. Checker call targets and
//! declaration identities own edges; spellings are used only for diagnostics.

use std::collections::{HashMap, HashSet, VecDeque};
use std::ops::Range;
use std::path::PathBuf;

use hew_parser::ast::{Item, Program, TypeBodyItem};
use hew_types::check::dispatch::CallTarget;
use hew_types::check::SpanKey;
use hew_types::env::TypeBindingId;
use hew_types::{
    CallableCandidate, DeclarationKind, DeclarationOccurrence, DefId, IndirectCallCandidates,
    NominalId, ResolvedTy, Ty, TypeCheckOutput,
};

use crate::{
    configured_stdlib_roots, path_is_below, read_source, DeterministicAdmission, DocumentSet,
    FrontendDiagnostic, FrontendOptions,
};

type Operation = hew_types::DeterministicOperation;

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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct ResolvedField {
    owner: NominalId,
    index: u32,
    values: Vec<ResolvedValue>,
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
    Aggregate(Vec<ResolvedField>),
    Unknown,
}

type CandidateEnv = HashMap<TypeBindingId, Vec<ResolvedValue>>;

fn selected_call_declaration(output: &TypeCheckOutput, span: &SpanKey) -> Option<DefId> {
    output
        .method_call_rewrites
        .get(span)
        .and_then(|rewrite| match rewrite {
            hew_types::MethodCallRewrite::RewriteToFunction { target, .. }
            | hew_types::MethodCallRewrite::RewriteModuleQualifiedToFunction { target, .. }
            | hew_types::MethodCallRewrite::StaticTraitDispatch { target, .. } => Some(target),
            _ => None,
        })
        .or_else(|| output.direct_call_targets.get(span))
        .and_then(target_declaration)
}

fn resolve_candidates(
    output: &TypeCheckOutput,
    candidates: &IndirectCallCandidates,
    env: &CandidateEnv,
    depth: usize,
) -> Vec<ResolvedValue> {
    if depth > 128 {
        return vec![ResolvedValue::Unknown];
    }
    let mut values = Vec::new();
    if candidates.may_be_unknown {
        values.push(ResolvedValue::Unknown);
    }
    for candidate in &candidates.known {
        for value in resolve_candidate(output, candidate, env, depth + 1) {
            if !values.contains(&value) {
                values.push(value);
            }
        }
    }
    if values.is_empty() {
        values.push(ResolvedValue::Unknown);
    }
    values
}

fn resolve_candidate(
    output: &TypeCheckOutput,
    candidate: &CallableCandidate,
    env: &CandidateEnv,
    depth: usize,
) -> Vec<ResolvedValue> {
    if depth > 128 {
        return vec![ResolvedValue::Unknown];
    }
    match candidate {
        CallableCandidate::Declaration(id) => vec![ResolvedValue::Declaration(*id)],
        CallableCandidate::Closure(span) => vec![ResolvedValue::Closure {
            span: span.clone(),
            captures: env_key(env),
        }],
        CallableCandidate::Formal(formal) => env
            .get(formal)
            .cloned()
            .unwrap_or_else(|| vec![ResolvedValue::Unknown]),
        CallableCandidate::Aggregate(span) => {
            let Some(fields) = output.aggregate_field_candidates.get(span) else {
                return vec![ResolvedValue::Unknown];
            };
            let mut fields = fields
                .iter()
                .map(|field| ResolvedField {
                    owner: field.owner,
                    index: field.index,
                    values: resolve_candidates(output, &field.candidates, env, depth + 1),
                })
                .collect::<Vec<_>>();
            fields.sort_by_key(|field| (field.owner, field.index));
            vec![ResolvedValue::Aggregate(fields)]
        }
        CallableCandidate::CallResult(span) => {
            let Some(callee) = selected_call_declaration(output, span) else {
                return vec![ResolvedValue::Unknown];
            };
            let Some(returned) = output.callable_return_candidates.get(&callee) else {
                return vec![ResolvedValue::Unknown];
            };
            let next_env = callee_env(output, span, callee, env, depth + 1);
            resolve_candidates(output, returned, &next_env, depth + 1)
        }
        CallableCandidate::Field {
            receiver,
            owner,
            index,
        } => {
            let mut values = Vec::new();
            for value in resolve_candidate(output, receiver, env, depth + 1) {
                let projected = match value {
                    ResolvedValue::Aggregate(fields) => fields
                        .into_iter()
                        .find(|field| field.owner == *owner && field.index == *index)
                        .map_or_else(|| vec![ResolvedValue::Unknown], |field| field.values),
                    _ => vec![ResolvedValue::Unknown],
                };
                for value in projected {
                    if !values.contains(&value) {
                        values.push(value);
                    }
                }
            }
            values
        }
    }
}

fn callee_env(
    output: &TypeCheckOutput,
    span: &SpanKey,
    callee: DefId,
    caller_env: &CandidateEnv,
    depth: usize,
) -> CandidateEnv {
    let direct: CandidateEnv = output
        .callable_argument_flows
        .get(span)
        .into_iter()
        .flatten()
        .filter(|flow| flow.callee == callee)
        .map(|flow| {
            (
                flow.formal,
                resolve_candidates(output, &flow.candidates, caller_env, depth + 1),
            )
        })
        .collect();
    if !direct.is_empty() {
        return direct;
    }
    let Some(actuals) = output.generic_trait_call_arguments.get(span) else {
        return CandidateEnv::new();
    };
    let Some(formals) = output.callable_formals.get(&callee) else {
        return CandidateEnv::new();
    };
    actuals
        .iter()
        .filter_map(|actual| {
            Some((
                *formals.get(actual.slot)?,
                resolve_candidates(output, &actual.candidates, caller_env, depth + 1),
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

fn refused_function(path: &str) -> Option<&'static Operation> {
    hew_types::DETERMINISTIC_FUNCTION_REJECTIONS
        .iter()
        .find(|entry| entry.identity == path)
}

fn refused_endpoint(symbol: &str) -> Option<&'static Operation> {
    hew_types::DETERMINISTIC_ENDPOINT_REJECTIONS
        .iter()
        .find(|entry| entry.identity == symbol)
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
fn call_graph(program: &Program, output: &TypeCheckOutput) -> HashMap<CallableNode, Vec<CallEdge>> {
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
            | hew_types::MethodCallRewrite::StaticTraitDispatch { target, .. } => target.clone(),
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
    documents: &DocumentSet,
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
) -> Option<&'static Operation> {
    if let Some(declaration) = target_declaration(target) {
        if output
            .defs
            .module(declaration)
            .and_then(|module| output.defs.module_source(module))
            .is_some_and(|source| stdlib_roots.iter().any(|root| path_is_below(source, root)))
        {
            if let Some(operation) = refused_function(output.defs.path(declaration)) {
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
    refused_endpoint(symbol)
}

fn opaque_extern_diagnostic(
    output: &TypeCheckOutput,
    root: DefId,
    site: SourceSite,
    source: &str,
    filename: &str,
    admission: &DeterministicAdmission,
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
            DeterministicAdmission::ProcessEntry => {
                "run without `--deterministic` or remove the user extern call".to_string()
            }
            DeterministicAdmission::Tests(_) => {
                format!("mark `{name}` `#[real_time]` or remove the user extern call")
            }
            DeterministicAdmission::Off => unreachable!("admission has no selected roots when off"),
        });
    }
    diagnostic
}

fn selected_entries(output: &TypeCheckOutput, admission: &DeterministicAdmission) -> Vec<DefId> {
    match admission {
        DeterministicAdmission::Off => Vec::new(),
        DeterministicAdmission::Tests(selections) => selections
            .iter()
            .filter_map(|selection| {
                output
                    .defs
                    .declaration(selection.with_module(output.defs.root_module()))
            })
            .collect(),
        DeterministicAdmission::ProcessEntry => output
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
    let roots = selected_entries(output, &options.deterministic_admission);
    let requested = match &options.deterministic_admission {
        DeterministicAdmission::Off => 0,
        DeterministicAdmission::ProcessEntry => 1,
        DeterministicAdmission::Tests(entries) => entries.len(),
    };
    if roots.len() != requested {
        return vec![FrontendDiagnostic::coded_message(
            "E_DETERMINISTIC_ENTRY",
            "a selected deterministic entry has no checked declaration identity",
        )];
    }
    if roots.is_empty() {
        return Vec::new();
    }
    let graph = call_graph(program, output);
    let stdlib_roots = configured_stdlib_roots(options);
    let mut diagnostics = Vec::new();
    let mut reported = HashSet::new();
    let mut reported_indirect = HashSet::new();
    let mut reported_extern = HashSet::new();
    for root in roots {
        let mut queue = VecDeque::from([(
            CallableNode::Declaration(root),
            None::<(SourceSite, String, String)>,
            Vec::<Ty>::new(),
            CandidateEnv::new(),
        )]);
        let mut visited = HashSet::new();
        while let Some((caller, user_site, type_args, caller_env)) = queue.pop_front() {
            if !visited.insert((caller.clone(), type_args.clone(), env_key(&caller_env))) {
                continue;
            }
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
                                &options.deterministic_admission,
                            ));
                        }
                    }
                } else if let Some(operation) =
                    target_operation(output, &call.target, &stdlib_roots)
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
                        let message = format!(
                            "`{}` reaches host operation `{}` ({}), which a deterministic entry cannot schedule",
                            name,
                            site.spelling,
                            operation.capability
                        );
                        let mut diagnostic = FrontendDiagnostic::coded_message_at(
                            "E_DETERMINISTIC_HOST_OPERATION",
                            message,
                            site.span,
                            &source,
                            &filename,
                        );
                        if let crate::FrontendDiagnosticKind::Message(detail) = &mut diagnostic.kind
                        {
                            let help = match options.deterministic_admission {
                                DeterministicAdmission::ProcessEntry => "run without `--deterministic` or remove the host operation from reachable calls".to_string(),
                                DeterministicAdmission::Tests(_) => format!("mark `{name}` `#[real_time]` or remove the host operation from its reachable calls"),
                                DeterministicAdmission::Off => unreachable!("admission has no selected roots when off"),
                            };
                            detail.help.push(help);
                        }
                        diagnostics.push(diagnostic);
                    }
                } else if matches!(call.target, CallTarget::IndirectFunctionValue) {
                    let values = output
                        .indirect_call_candidates
                        .get(&call.span)
                        .map(|candidates| resolve_candidates(output, candidates, &caller_env, 0));
                    if values.as_ref().is_none_or(|values| {
                        values.iter().any(|value| {
                            matches!(value, ResolvedValue::Unknown | ResolvedValue::Aggregate(_))
                        })
                    }) {
                        if let Some((site, source, filename)) = selected_site.clone() {
                            if reported_indirect.insert((root, filename.clone(), site.span.start)) {
                                let name = output.defs.name(root);
                                let mut diagnostic = FrontendDiagnostic::coded_message_at(
                                    "E_DETERMINISTIC_INDIRECT_CALL",
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
                                    let help = match options.deterministic_admission {
                                        DeterministicAdmission::ProcessEntry => "run without `--deterministic` or call a locally known function".to_string(),
                                        DeterministicAdmission::Tests(_) => format!("mark `{name}` `#[real_time]` or call a locally known function"),
                                        DeterministicAdmission::Off => unreachable!("admission has no selected roots when off"),
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
                                    let next_env =
                                        callee_env(output, &call.span, *id, &caller_env, 0);
                                    queue.push_back((
                                        CallableNode::Declaration(*id),
                                        selected_site.clone(),
                                        Vec::new(),
                                        next_env,
                                    ));
                                }
                                ResolvedValue::Closure { span, captures } => {
                                    queue.push_back((
                                        CallableNode::Closure(span.clone()),
                                        selected_site.clone(),
                                        Vec::new(),
                                        captures.iter().cloned().collect(),
                                    ));
                                }
                                ResolvedValue::Aggregate(_) | ResolvedValue::Unknown => {}
                            }
                        }
                    }
                } else if let Some(next) = target_declaration(&call.target) {
                    let substitutions = match &caller {
                        CallableNode::Declaration(id) => output.fn_sigs.get(id).map(|sig| {
                            sig.type_params
                                .iter()
                                .cloned()
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
                    let next_env = callee_env(output, &call.span, next, &caller_env, 0);
                    queue.push_back((
                        CallableNode::Declaration(next),
                        selected_site,
                        next_args,
                        next_env,
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
                                    callee_env(output, &call.span, candidate, &caller_env, 0);
                                queue.push_back((
                                    CallableNode::Declaration(candidate),
                                    selected_site.clone(),
                                    next_args.clone(),
                                    next_env,
                                ));
                            }
                        }
                    }
                } else if let CallTarget::DynamicVtable { method, .. } = &call.target {
                    // Every concrete-to-dyn coercion publishes its executable
                    // vtable entries by declaration identity. A dyn call can
                    // reach any implementation filed for this trait method.
                    let candidates = output
                        .dyn_trait_coercions
                        .values()
                        .flat_map(|coercion| &coercion.vtable_entries)
                        .filter(|entry| entry.method == *method)
                        .filter_map(|entry| entry.impl_method);
                    for candidate in candidates {
                        let next_env = callee_env(output, &call.span, candidate, &caller_env, 0);
                        queue.push_back((
                            CallableNode::Declaration(candidate),
                            selected_site.clone(),
                            Vec::new(),
                            next_env,
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

    fn check_source(source: &str, admission: DeterministicAdmission) -> Result<(), String> {
        let directory = tempfile::tempdir().expect("temporary source directory");
        let path = directory.path().join("admission.hew");
        std::fs::write(&path, source).expect("write source");
        let options = FrontendOptions {
            deterministic_admission: admission,
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
        let failure = check_source(source, DeterministicAdmission::ProcessEntry).unwrap_err();
        assert!(
            failure.contains("E_DETERMINISTIC_HOST_OPERATION"),
            "{failure}"
        );
        assert!(failure.contains("net.listen"), "{failure}");
    }

    #[test]
    fn unreachable_host_helper_does_not_block_process_entry() {
        let source = "import std.net;\n\nfn unused() {\n    match net.listen(\":0\") {\n        .Ok(_) => {}\n        .Err(_) => {}\n    }\n}\n\nfn main() {\n    println(\"safe\");\n}\n";
        check_source(source, DeterministicAdmission::ProcessEntry).unwrap();
    }

    #[test]
    fn aliased_std_module_keeps_its_host_identity() {
        let source = "import std.net as wire;\n\nfn main() {\n    match wire.listen(\":0\") {\n        .Ok(_) => {}\n        .Err(_) => {}\n    }\n}\n";
        let failure = check_source(source, DeterministicAdmission::ProcessEntry).unwrap_err();
        assert!(
            failure.contains("E_DETERMINISTIC_HOST_OPERATION"),
            "{failure}"
        );
        assert!(failure.contains("wire.listen"), "{failure}");
    }

    #[test]
    fn a_user_function_with_a_host_like_name_is_admitted() {
        let source = "fn listen() -> i64 { 7 }\nfn main() { println(listen()); }";
        check_source(source, DeterministicAdmission::ProcessEntry).unwrap();
    }

    #[test]
    fn selected_test_does_not_inherit_real_time_siblings_operation() {
        let source = "import std.net;\n\n#[test]\nfn safe() {\n    println(\"safe\");\n}\n\n#[test]\n#[real_time]\nfn socket() {\n    match net.listen(\":0\") {\n        .Ok(_) => {}\n        .Err(_) => {}\n    }\n}\n";
        let selection = selected_test(source, "safe");
        check_source(source, DeterministicAdmission::Tests(vec![selection])).unwrap();
    }

    #[test]
    fn selected_test_refuses_stdin_read() {
        let source =
            "import std.io;\n#[test] fn input() { let line = io.read_line(); println(line); }";
        let selection = selected_test(source, "input");
        let failure =
            check_source(source, DeterministicAdmission::Tests(vec![selection])).unwrap_err();
        assert!(
            failure.contains("E_DETERMINISTIC_HOST_OPERATION"),
            "{failure}"
        );
        assert!(failure.contains("io.read_line"), "{failure}");
    }

    #[test]
    fn dynamic_trait_call_includes_checked_implementer() {
        let source = "import std.io;\n\ntrait Reader {\n    fn read(value: Self) -> string;\n}\n\ntype Host {\n    n: i64;\n}\n\nimpl Host {\n    fn read(value: Host) -> string {\n        io.read_line()\n    }\n}\n\nfn inspect(value: dyn Reader) -> string {\n    value.read()\n}\n\nfn main() {\n    let erased: dyn Reader = Host { n: 1 };\n    println(inspect(erased));\n}\n";
        let failure = check_source(source, DeterministicAdmission::ProcessEntry).unwrap_err();
        assert!(
            failure.contains("E_DETERMINISTIC_HOST_OPERATION"),
            "{failure}"
        );
    }

    #[test]
    fn static_trait_dispatch_includes_checked_implementer() {
        let source = "import std.io;\n\ntrait Reader {\n    fn read(value: Self) -> string;\n}\n\ntype Host {\n    n: i64;\n}\n\nimpl Host {\n    fn read(value: Host) -> string {\n        io.read_line()\n    }\n}\n\nfn inspect<T: Reader>(value: T) -> string {\n    value.read()\n}\n\nfn main() {\n    println(inspect(Host { n: 1 }));\n}\n";
        let failure = check_source(source, DeterministicAdmission::ProcessEntry).unwrap_err();
        assert!(
            failure.contains("E_DETERMINISTIC_HOST_OPERATION"),
            "{failure}"
        );
    }

    #[test]
    fn static_trait_dispatch_excludes_unused_implementer() {
        let source = "import std.io;\n\ntrait Reader {\n    fn read(value: Self) -> string;\n}\n\ntype Safe {\n    n: i64;\n}\n\nimpl Safe {\n    fn read(value: Safe) -> string {\n        \"safe\"\n    }\n}\n\ntype Host {\n    n: i64;\n}\n\nimpl Host {\n    fn read(value: Host) -> string {\n        io.read_line()\n    }\n}\n\nfn inspect<T: Reader>(value: T) -> string {\n    value.read()\n}\n\nfn main() {\n    println(inspect(Safe { n: 1 }));\n}\n";
        check_source(source, DeterministicAdmission::ProcessEntry).unwrap();
    }

    #[test]
    fn remote_actor_send_and_ask_require_real_time() {
        let declaration = "#[wire]\ntype Ping {\n    n: i64 @1;\n}\n\nactor Worker {\n    receive fn ping(msg: Ping) -> i64 {\n        0\n    }\n}\n\nimpl ActorMsg for Worker {\n    type Msg = Ping;\n    type Reply = i64;\n}\n";
        for call in ["pid.send(Ping { n: 0 })", "pid.ask(Ping { n: 0 }, 1000)"] {
            let source =
                format!("{declaration}fn main() {{ let pid: RemotePid<Worker>; let _ = {call}; }}");
            let failure = check_source(&source, DeterministicAdmission::ProcessEntry).unwrap_err();
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
        let failure = check_source(source, DeterministicAdmission::ProcessEntry).unwrap_err();
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
            "import std.io;\nfn main() { let line = io.read_line(); println(line); }",
        )
        .unwrap();
        let options = FrontendOptions {
            deterministic_admission: DeterministicAdmission::ProcessEntry,
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
    fn invoked_closure_cannot_hide_stdin_read() {
        let source =
            "import std.io;\nfn main() { let reader = || io.read_line(); println(reader()); }";
        let failure = check_source(source, DeterministicAdmission::ProcessEntry).unwrap_err();
        assert!(
            failure.contains("E_DETERMINISTIC_HOST_OPERATION"),
            "{failure}"
        );
    }

    #[test]
    fn unused_closure_does_not_admit_its_host_operation() {
        let source =
            "import std.io;\nfn main() { let unused = || io.read_line(); println(\"safe\"); }";
        check_source(source, DeterministicAdmission::ProcessEntry).unwrap();
    }

    #[test]
    fn selected_branch_closure_reaches_host_operation() {
        let source = "import std.io;\nfn main() { let reader = if true { || \"safe\" } else { || io.read_line() }; println(reader()); }";
        let failure = check_source(source, DeterministicAdmission::ProcessEntry).unwrap_err();
        assert!(
            failure.contains("E_DETERMINISTIC_HOST_OPERATION"),
            "{failure}"
        );
    }

    #[test]
    fn opaque_function_parameter_is_refused() {
        let source = "fn call_reader(reader: fn() -> string) { println(reader()); }";
        let selection = selected_test(source, "call_reader");
        let failure =
            check_source(source, DeterministicAdmission::Tests(vec![selection])).unwrap_err();
        assert!(
            failure.contains("E_DETERMINISTIC_INDIRECT_CALL"),
            "{failure}"
        );
    }

    #[test]
    fn known_function_parameter_is_admitted_from_its_call_site() {
        let source = "fn call_reader(reader: fn() -> string) { println(reader()); }\nfn main() { call_reader(|| \"safe\"); }";
        check_source(source, DeterministicAdmission::ProcessEntry).unwrap();
    }

    #[test]
    fn known_function_value_is_admitted() {
        let source = "fn answer() -> i64 { 7 }\nfn main() { let selected: fn() -> i64 = answer; println(selected()); }";
        check_source(source, DeterministicAdmission::ProcessEntry).unwrap();
    }

    #[test]
    fn user_extern_without_a_manifest_capability_is_refused() {
        let source = "extern \"C\" { fn getpid() -> i64; }\nfn main() { let pid = unsafe { getpid() }; println(pid); }";
        let failure = check_source(source, DeterministicAdmission::ProcessEntry).unwrap_err();
        assert!(
            failure.contains("E_DETERMINISTIC_HOST_OPERATION"),
            "{failure}"
        );
        assert!(failure.contains("getpid"), "{failure}");
    }

    #[test]
    fn imported_methods_use_their_checked_rewrite() {
        let source = "import std.bench;\nfn main() { var s = bench.suite(\"suite\"); s.add(\"noop\", 1, || {}); s.report(); }";
        check_source(source, DeterministicAdmission::ProcessEntry).unwrap();
    }

    #[test]
    fn imported_iterator_combinators_preserve_callback_origin() {
        let source = "import std.iter;\nfn main() { var v: Vec<string> = Vec.new(); v.push(\"a\"); let result = iter.collect(iter.map(v.into_iter(), |s: string| s + \"!\")); assert(result.len() == 1); }";
        check_source(source, DeterministicAdmission::ProcessEntry).unwrap();
    }

    #[test]
    fn imported_numeric_folds_preserve_concrete_iterator_arguments() {
        check_source(
            "import std.iter; fn main() { let values = [1.0, 2.0].into_iter(); \
             println(iter.sum_f64(values)); }",
            DeterministicAdmission::ProcessEntry,
        )
        .expect("a numeric fold must select the concrete iterator implementation");
    }

    #[test]
    fn collected_iterator_reaches_its_callback_host_operation() {
        let source = "import std.io;\nimport std.iter;\nfn main() { var v: Vec<i64> = Vec.new(); v.push(1); let result = iter.collect(iter.map(v.into_iter(), |n: i64| { let line = io.read_line(); n })); assert(result.len() == 1); }";
        let failure = check_source(source, DeterministicAdmission::ProcessEntry).unwrap_err();
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
        let source = "import std.io;\nimport std.iter;\nfn main() { var v: Vec<i64> = Vec.new(); v.push(1); let unused = iter.map(v.into_iter(), |n: i64| { let line = io.read_line(); n }); println(\"safe\"); }";
        check_source(source, DeterministicAdmission::ProcessEntry).unwrap();
    }

    #[test]
    fn imported_function_value_uses_checked_declaration() {
        let directory = tempfile::tempdir().expect("temporary source directory");
        let path = directory.path().join("admission.hew");
        std::fs::write(
            directory.path().join("host.hew"),
            "import std.io;\npub fn read() -> string { io.read_line() }",
        )
        .expect("write imported source");
        std::fs::write(
            &path,
            "import host;\nfn main() { let f: fn() -> string = host.read; println(f()); }",
        )
        .expect("write entry source");
        let options = FrontendOptions {
            deterministic_admission: DeterministicAdmission::ProcessEntry,
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
        let failure =
            check_source("fn helper() {}", DeterministicAdmission::ProcessEntry).unwrap_err();
        assert!(failure.contains("E_DETERMINISTIC_ENTRY"), "{failure}");
    }

    #[test]
    fn deterministic_admission_cannot_be_bypassed_with_no_typecheck() {
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("unchecked.hew");
        std::fs::write(&path, "fn main() {}").unwrap();
        let options = FrontendOptions {
            no_typecheck: true,
            deterministic_admission: DeterministicAdmission::ProcessEntry,
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
