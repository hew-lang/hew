//! A bounded purity proof for the ordinary machine evaluator.
//!
//! Purity restricts what a transition does, not the types its state holds
//! (D530): every reachable source helper is inspected through its checked
//! declaration identity, runtime calls use the selected typed family, and
//! unknown calls, sends, spawns and I/O cannot acquire a purity promise by
//! spelling. `Rc` and `#[resource]` payloads move through transitions as
//! ordinary values, but releasing one runs its authored `close`, so every
//! `#[resource]` a helper's values can reach adds that `close` to the proof.

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};

use hew_parser::ast::{
    DeclarationOrigin, Expr, FnDecl, Item, Program, Span, Spanned, Stmt, StringPart,
};

use super::dispatch::{CallTarget, HashMapMethod, MethodTargetFamily};
use super::lints::{self, NodeVisitor};
use super::{MethodCallRewrite, SpanKey, TypeCheckOutput, UserComparisonDispatch};
use crate::error::{TypeError, TypeErrorKind};
use crate::{
    CloneKind, DeclarationKind, DeclarationMarker, DeclarationOccurrence, DefId, ResolvedTy,
    RuntimeCallFamily, TypeFactService,
};

struct Body<'a> {
    function: &'a FnDecl,
    module_idx: u32,
    source_module: Option<String>,
}

#[derive(Default)]
struct Summary {
    calls: Vec<(DefId, Span)>,
    /// Authored `close` bodies a release of this helper's values can run,
    /// with the resource they release.
    releases: Vec<(DefId, String, Span)>,
    refusal: Option<(Span, String)>,
}

/// The declared `#[resource]` types whose authored `close` a release of a
/// value of each type can run, memoized across helpers.
type ReleaseCache = HashMap<ResolvedTy, BTreeSet<String>>;

/// The surface identity a machine is instantiated through.
#[derive(Debug, Clone, Default)]
struct MachineShape {
    type_name: String,
    type_params: Vec<String>,
}

/// `resource_closes` names each `#[resource]` type's inherent `close`.
pub(super) fn validate(
    output: &TypeCheckOutput,
    resource_closes: &HashMap<String, DefId>,
) -> Vec<TypeError> {
    let Some(normalized) = &output.normalized_machines else {
        return Vec::new();
    };
    let mut bodies = HashMap::new();
    let mut machines = Vec::new();
    let mut missing_steps = Vec::new();
    collect_bodies(
        &normalized.program,
        output,
        &mut bodies,
        &mut machines,
        &mut missing_steps,
    );
    if !missing_steps.is_empty() {
        return missing_steps
            .into_iter()
            .map(|span| {
                TypeError::new(
                    TypeErrorKind::MachineExhaustivenessError,
                    span,
                    "generated machine step lost its checked declaration identity".to_string(),
                )
            })
            .collect();
    }
    let mut summaries = HashMap::new();
    let mut release_cache = ReleaseCache::new();
    for (declaration, body) in &bodies {
        let mut visitor = EffectVisitor {
            output,
            resource_closes,
            module_idx: body.module_idx,
            summary: Summary::default(),
            release_cache: &mut release_cache,
        };
        if body.function.is_generator
            || body.function.intrinsic.is_some()
            || body
                .function
                .attributes
                .iter()
                .any(|attribute| attribute.name == "extern_symbol")
        {
            visitor.refuse(
                &body.function.fn_span,
                "machine helpers must be ordinary synchronous source functions",
            );
        }
        lints::walk_body(&body.function.body, &mut visitor);
        summaries.insert(declaration.clone(), visitor.summary);
    }
    let mut facts =
        TypeFactService::new(output.type_fact_context.clone(), output.type_facts.clone());
    let mut errors = Vec::new();
    for (declaration, shape) in machines {
        let source_module = bodies
            .get(&declaration)
            .and_then(|body| body.source_module.clone());
        let mut visiting = HashSet::new();
        let mut proven = HashSet::new();
        let sites = instantiation_sites(&shape, output);
        let proof = prove(&declaration, &summaries, &mut visiting, &mut proven).and_then(|()| {
            prove_instantiation_releases(
                &sites,
                output,
                resource_closes,
                &summaries,
                &mut release_cache,
                &mut proven,
            )
        });
        match proof {
            Ok(()) => errors.extend(staging_refusals(
                &shape,
                &sites,
                source_module.as_deref(),
                output,
                normalized,
                &mut facts,
            )),
            Err((span, reason)) => {
                let mut error = TypeError::new(
                    TypeErrorKind::MachineExhaustivenessError,
                    span,
                    format!("machine evaluator is not demonstrably pure: {reason}"),
                );
                error.source_module = source_module;
                errors.push(error);
            }
        }
    }
    errors
}

/// A step evaluates an independent copy of its machine and commits it only
/// after the whole report is built, so a fault before commit leaves the
/// caller's machine intact. A state payload with no independent value copy
/// (a `#[resource]` held directly, not through `Rc`) cannot be staged yet.
///
/// WHY: the native step stages its receiver by copy. WHEN obsolete: once a
/// step can take an affine receiver and hand it back on a pre-commit fault.
/// WHAT: SIR's staged receiver update (`lower_var_self`) and its type-based
/// release-fault edges for the step's payload locals.
///
/// Every concrete instantiation the checked program produces is judged once,
/// at the transition that takes the offending state's payload, or else at the
/// earliest span that names the instantiation.
fn staging_refusals(
    shape: &MachineShape,
    sites: &BTreeMap<ResolvedTy, Span>,
    source_module: Option<&str>,
    output: &TypeCheckOutput,
    normalized: &super::machine_normalize::NormalizedMachines,
    facts: &mut TypeFactService,
) -> Vec<TypeError> {
    let transitions = normalized.transitions.get(&(
        source_module.unwrap_or("(root)").to_string(),
        shape.type_name.clone(),
    ));
    let mut errors = Vec::new();
    for (machine, site) in sites {
        let site = site.clone();
        if facts
            .require(machine)
            .is_ok_and(|facts| facts.clone != CloneKind::None)
        {
            continue;
        }
        let Some((state, field, field_ty)) = unstageable_field(machine, output, facts) else {
            continue;
        };
        let span = transitions
            .and_then(|rules| {
                rules
                    .iter()
                    .find(|(source, _)| *source == state || source == "_")
            })
            .map_or(site, |(_, body)| body.clone());
        let mut error = TypeError::new(
            TypeErrorKind::MachineExhaustivenessError,
            span,
            format!(
                "machine `{}` state `{state}` field `{field}` holds `{}`, which has no \
                 independent value copy: a step stages a copy of its state until it commits, \
                 so this transition cannot take it yet",
                machine.user_facing(),
                field_ty.user_facing()
            ),
        );
        error = error.with_suggestion(format!(
            "hold the payload through `Rc<{}>` to share it across the staged copy",
            field_ty.user_facing()
        ));
        error.source_module = source_module.map(str::to_string);
        errors.push(error);
    }
    errors
}

/// Every concrete instantiation of the machine the checked program produces,
/// with the earliest span that names it.
fn instantiation_sites(
    shape: &MachineShape,
    output: &TypeCheckOutput,
) -> BTreeMap<ResolvedTy, Span> {
    let mut sites: BTreeMap<ResolvedTy, Span> = BTreeMap::new();
    for (key, ty) in &output.resolved_expr_types {
        let mut found = Vec::new();
        collect_instantiations(ty, shape, output, &mut found);
        for machine in found {
            let span = key.start..key.end;
            sites
                .entry(machine)
                .and_modify(|earliest| {
                    if span.start < earliest.start {
                        *earliest = span.clone();
                    }
                })
                .or_insert(span);
        }
    }
    sites
}

/// A step releases the states and events it replaces or consumes. A generic
/// machine's helpers see only its parameters, so each concrete instantiation
/// proves the authored `close` of every `#[resource]` its states and events
/// can reach.
fn prove_instantiation_releases(
    sites: &BTreeMap<ResolvedTy, Span>,
    output: &TypeCheckOutput,
    resource_closes: &HashMap<String, DefId>,
    summaries: &HashMap<DefId, Summary>,
    cache: &mut ReleaseCache,
    proven: &mut HashSet<DefId>,
) -> Result<(), (Span, String)> {
    for (machine, site) in sites {
        let ResolvedTy::Named { name, args, .. } = machine else {
            continue;
        };
        let event = ResolvedTy::Named {
            name: format!("{name}Event"),
            args: args.clone(),
            builtin: None,
            is_opaque: false,
        };
        for ty in [machine, &event] {
            for resource in released_resources(ty, output, cache) {
                let Some(close) = resource_closes.get(&resource).cloned() else {
                    return Err((site.clone(), unknown_close(&resource)));
                };
                prove(&close, summaries, &mut HashSet::new(), proven)
                    .map_err(|(_, reason)| (site.clone(), releasing(&resource, &reason)))?;
            }
        }
    }
    Ok(())
}

/// The declared `#[resource]` types whose authored `close` releasing a value
/// of `ty` can run: the type itself and everything it holds, through `Rc`,
/// collections, records and enum payloads. A builtin handle's release is a
/// closed runtime operation, not an authored body.
fn released_resources(
    ty: &ResolvedTy,
    output: &TypeCheckOutput,
    cache: &mut ReleaseCache,
) -> BTreeSet<String> {
    if let Some(found) = cache.get(ty) {
        return found.clone();
    }
    let mut found = BTreeSet::new();
    collect_released_resources(ty, output, &mut found, &mut HashSet::new());
    cache.insert(ty.clone(), found.clone());
    found
}

fn collect_released_resources(
    ty: &ResolvedTy,
    output: &TypeCheckOutput,
    found: &mut BTreeSet<String>,
    seen: &mut HashSet<ResolvedTy>,
) {
    if !seen.insert(ty.clone()) {
        return;
    }
    match ty {
        ResolvedTy::Named { name, args, .. } => {
            for arg in args {
                collect_released_resources(arg, output, found, seen);
            }
            let Some(declaration) = output.type_fact_context.declarations().get(name.as_str())
            else {
                return;
            };
            if declaration.builtin.is_none() && declaration.marker == DeclarationMarker::Resource {
                found.insert(name.clone());
            }
            for member in &declaration.members {
                let member = crate::value_class::substitute(member, &declaration.type_params, args);
                collect_released_resources(&member, output, found, seen);
            }
        }
        ResolvedTy::Tuple(elements) => {
            for element in elements {
                collect_released_resources(element, output, found, seen);
            }
        }
        ResolvedTy::Array(element, _) | ResolvedTy::Slice(element) | ResolvedTy::Task(element) => {
            collect_released_resources(element, output, found, seen);
        }
        _ => {}
    }
}

fn unknown_close(resource: &str) -> String {
    format!("releasing `{resource}` runs a `close` with no inspectable checked body")
}

fn releasing(resource: &str, reason: &str) -> String {
    format!("releasing `{resource}` runs its `close`: {reason}")
}

/// The first `(state, field, type)` of a machine whose payload has no
/// independent value copy.
fn unstageable_field(
    machine: &ResolvedTy,
    output: &TypeCheckOutput,
    facts: &mut TypeFactService,
) -> Option<(String, String, ResolvedTy)> {
    let ResolvedTy::Named { name, args, .. } = machine else {
        return None;
    };
    let definition = output.type_defs.get(name)?;
    let mut states: Vec<_> = definition.variants.iter().collect();
    states.sort_by(|left, right| left.0.cmp(right.0));
    for (state, variant) in states {
        let super::VariantDef::Struct(fields) = variant else {
            continue;
        };
        for (field, ty) in fields {
            let Ok(ty) = ResolvedTy::from_ty(ty) else {
                continue;
            };
            let ty = crate::value_class::substitute(&ty, &definition.type_params, args);
            if facts
                .require(&ty)
                .is_ok_and(|facts| facts.clone == CloneKind::None)
            {
                return Some((state.clone(), field.clone(), ty));
            }
        }
    }
    None
}

/// Every concrete instantiation of `shape`'s own type that `ty` contains.
fn collect_instantiations(
    ty: &ResolvedTy,
    shape: &MachineShape,
    output: &TypeCheckOutput,
    found: &mut Vec<ResolvedTy>,
) {
    match ty {
        ResolvedTy::Named { name, args, .. } => {
            if name == &shape.type_name
                && args.len() == shape.type_params.len()
                && !args.iter().any(|arg| is_abstract(arg, output))
            {
                found.push(ty.clone());
            }
            for arg in args {
                collect_instantiations(arg, shape, output, found);
            }
        }
        ResolvedTy::Tuple(elements) => {
            for element in elements {
                collect_instantiations(element, shape, output, found);
            }
        }
        ResolvedTy::Array(element, _) | ResolvedTy::Slice(element) | ResolvedTy::Task(element) => {
            collect_instantiations(element, shape, output, found);
        }
        _ => {}
    }
}

/// Is any part of `ty` still an abstract type parameter?
///
/// A parameter reaches here spelled either as `TypeParam` or, resolved without
/// a type-parameter scope, as a zero-argument user `Named`. The second spelling
/// is told apart from a declared type by asking the checker's own tables: a
/// name it declared is a type, and a name it did not is a parameter. Asking
/// both the fact context and `type_defs` matters because an actor is a
/// declared type that carries no value facts.
fn is_abstract(ty: &ResolvedTy, output: &TypeCheckOutput) -> bool {
    match ty {
        ResolvedTy::TypeParam { .. } => true,
        ResolvedTy::Named {
            name,
            args,
            builtin: None,
            ..
        } if args.is_empty() => {
            !output
                .type_fact_context
                .declarations()
                .contains_key(name.as_str())
                && !output.type_defs.contains_key(name.as_str())
        }
        ResolvedTy::Named { args, .. } => args.iter().any(|arg| is_abstract(arg, output)),
        ResolvedTy::Tuple(elements) => elements.iter().any(|element| is_abstract(element, output)),
        ResolvedTy::Array(element, _) | ResolvedTy::Slice(element) | ResolvedTy::Task(element) => {
            is_abstract(element, output)
        }
        _ => false,
    }
}

fn collect_bodies<'a>(
    program: &'a Program,
    output: &TypeCheckOutput,
    bodies: &mut HashMap<DefId, Body<'a>>,
    machines: &mut Vec<(DefId, MachineShape)>,
    missing_steps: &mut Vec<Span>,
) {
    let mut add_items =
        |items: &'a [Spanned<Item>],
         contexts: Vec<(Option<crate::ModuleId>, u32, Option<String>)>| {
            for (ordinal, ((item, span), (module, module_idx, source_module))) in
                items.iter().zip(contexts).enumerate()
            {
                let mut add = |function: &'a FnDecl,
                               kind: DeclarationKind,
                               declaration_span: &Span,
                               shape: &MachineShape| {
                    let occurrence = DeclarationOccurrence::new_with_synthetic_ordinal(
                        module,
                        declaration_span,
                        ordinal,
                        kind,
                        0,
                    );
                    if let Some(declaration) = output.identity.declaration(occurrence) {
                        if function.origin == DeclarationOrigin::MachineStep
                            && !machines.iter().any(|(id, _)| id == declaration)
                        {
                            machines.push((declaration.clone(), shape.clone()));
                        }
                        bodies.entry(declaration.clone()).or_insert_with(|| Body {
                            function,
                            module_idx,
                            source_module: source_module.clone(),
                        });
                    } else if function.origin == DeclarationOrigin::MachineStep {
                        missing_steps.push(function.fn_span.clone());
                    }
                };
                match item {
                    Item::Function(function) => add(
                        function,
                        DeclarationKind::Function,
                        span,
                        &MachineShape::default(),
                    ),
                    Item::Impl(implementation) => {
                        // Only a machine's own generated impl may defer a
                        // purity obligation to instantiation; an ordinary
                        // generic helper is still judged where it is written.
                        let shape = if implementation
                            .methods
                            .iter()
                            .any(|function| function.origin == DeclarationOrigin::MachineStep)
                        {
                            machine_shape(implementation)
                        } else {
                            MachineShape::default()
                        };
                        for function in &implementation.methods {
                            add(
                                function,
                                DeclarationKind::ImplMethod,
                                &function.fn_span,
                                &shape,
                            );
                        }
                    }
                    _ => {}
                }
            }
        };
    add_items(
        &program.items,
        program
            .items
            .iter()
            .map(|_| (output.identity.root_module(), 0, None))
            .collect(),
    );
    if let Some(graph) = &program.module_graph {
        let indices = graph.file_span_indices();
        for id in &graph.topo_order {
            if *id == graph.root {
                continue;
            }
            let Some(module) = graph.modules.get(id) else {
                continue;
            };
            let dotted = id.path.join(".");
            let contexts = module
                .items
                .iter()
                .enumerate()
                .map(|(ordinal, _)| {
                    let owner = graph
                        .item_source(id, ordinal)
                        .or_else(|| module.source_paths.first())
                        .and_then(|source| output.identity.module_for_source(source))
                        .or_else(|| output.identity.module_for_path(&dotted));
                    (
                        owner,
                        indices.item_index(id, ordinal).unwrap_or_default(),
                        Some(dotted.clone()),
                    )
                })
                .collect();
            add_items(&module.items, contexts);
        }
    }
}

/// The machine's surface type name and its own parameters, read from the
/// generated impl the normalizer produced for it.
fn machine_shape(implementation: &hew_parser::ast::ImplDecl) -> MachineShape {
    let type_name = match &implementation.target_type.0 {
        hew_parser::ast::TypeExpr::Named { name, .. } => name.clone(),
        _ => String::new(),
    };
    MachineShape {
        type_name,
        type_params: implementation
            .type_params
            .iter()
            .flatten()
            .map(|param| param.name.clone())
            .collect(),
    }
}

fn prove(
    declaration: &DefId,
    summaries: &HashMap<DefId, Summary>,
    visiting: &mut HashSet<DefId>,
    proven: &mut HashSet<DefId>,
) -> Result<(), (Span, String)> {
    if proven.contains(declaration) || !visiting.insert(declaration.clone()) {
        return Ok(());
    }
    let Some(summary) = summaries.get(declaration) else {
        return Err((
            0..0,
            format!(
                "helper `{}` has no inspectable checked body",
                declaration.display_name()
            ),
        ));
    };
    if let Some(refusal) = &summary.refusal {
        return Err(refusal.clone());
    }
    for (callee, span) in &summary.calls {
        if let Err((_, reason)) = prove(callee, summaries, visiting, proven) {
            return Err((
                span.clone(),
                format!("call to `{}`: {reason}", callee.display_name()),
            ));
        }
    }
    for (close, resource, span) in &summary.releases {
        if let Err((_, reason)) = prove(close, summaries, visiting, proven) {
            return Err((span.clone(), releasing(resource, &reason)));
        }
    }
    visiting.remove(declaration);
    proven.insert(declaration.clone());
    Ok(())
}

struct EffectVisitor<'a> {
    output: &'a TypeCheckOutput,
    resource_closes: &'a HashMap<String, DefId>,
    module_idx: u32,
    summary: Summary,
    release_cache: &'a mut ReleaseCache,
}

impl EffectVisitor<'_> {
    fn refuse(&mut self, span: &Span, reason: impl Into<String>) {
        if self.summary.refusal.is_none() {
            self.summary.refusal = Some((span.clone(), reason.into()));
        }
    }
    /// A value of the expression's type may be released in this helper,
    /// running the `close` of every `#[resource]` it can reach.
    fn releases(&mut self, key: &SpanKey, span: &Span) {
        let Some(ty) = self.output.resolved_expr_types.get(key) else {
            return;
        };
        for resource in released_resources(ty, self.output, self.release_cache) {
            if self
                .summary
                .releases
                .iter()
                .any(|(_, known, _)| *known == resource)
            {
                continue;
            }
            match self.resource_closes.get(&resource).cloned() {
                Some(close) => self.summary.releases.push((close, resource, span.clone())),
                None => self.refuse(span, unknown_close(&resource)),
            }
        }
    }
    /// `call` is the source spelling of the callee, as the programmer wrote it.
    fn target(&mut self, target: &CallTarget, span: &Span, call: &str) {
        match target {
            CallTarget::User(declaration) | CallTarget::ImplMethod(declaration) => {
                self.summary.calls.push((declaration.clone(), span.clone()));
            }
            CallTarget::Runtime(family) if pure_runtime(*family) => {}
            CallTarget::RuntimeCollection(MethodTargetFamily::Vec(method))
                if *method != super::dispatch::VecMethod::Contains => {}
            // A machine may build and grow a local `Vec` or `HashMap` state
            // value purely, the same as it already may for `bytes` (see
            // `pure_runtime`). `insert` touches only the map's own local
            // value; the rest of `HashMapMethod` stays refused until each is
            // reviewed the same way (D360: extend minimally, name the gap).
            CallTarget::RuntimeCollection(MethodTargetFamily::HashMap(HashMapMethod::Insert)) => {}
            // These closed compiler intrinsics raise an ordinary checked fault;
            // they do not perform externally visible work before unwinding.
            CallTarget::Builtin { endpoint } if matches!(endpoint.as_str(), "panic" | "assert") => {
            }
            _ => self.refuse(
                span,
                format!("`{call}` is not admitted in a machine transition"),
            ),
        }
    }
    fn method(&mut self, key: &SpanKey, span: &Span, call: &str) {
        if let Some(target) = self
            .output
            .resolved_calls
            .get(key)
            .map(|call| &call.target)
            .or_else(|| self.output.direct_call_targets.get(key))
        {
            self.target(target, span, call);
            return;
        }
        match self.output.method_call_rewrites.get(key) {
            Some(
                MethodCallRewrite::RewriteToFunction { target, .. }
                | MethodCallRewrite::RewriteModuleQualifiedToFunction { target, .. },
            ) => self.target(target, span, call),
            Some(
                MethodCallRewrite::GenericMathIntrinsic { .. }
                | MethodCallRewrite::BuiltinVecIntoIter
                | MethodCallRewrite::BuiltinVecIter
                | MethodCallRewrite::BuiltinVecIterNext
                | MethodCallRewrite::VecFrom
                | MethodCallRewrite::RecordCloneInplace { .. }
                | MethodCallRewrite::CopyCloneNoop,
            ) => {}
            _ => self.refuse(
                span,
                format!("`{call}` has no admitted pure checked target"),
            ),
        }
    }
}

impl NodeVisitor for EffectVisitor<'_> {
    fn visit_stmt(&mut self, stmt: &Stmt, span: &Span) {
        if matches!(stmt, Stmt::Defer(_)) {
            self.refuse(span, "deferred effects are not admitted in machine helpers");
        }
    }
    fn visit_expr(&mut self, expr: &Expr, span: &Span) {
        let key = SpanKey::in_module(span, self.module_idx);
        self.releases(&key, span);
        if let Some(dispatch) = self.output.user_comparison_dispatch.get(&key) {
            let (UserComparisonDispatch::Eq { method }
            | UserComparisonDispatch::Ord { method }
            | UserComparisonDispatch::PartialOrd { method }) = dispatch;
            self.summary.calls.push((method.clone(), span.clone()));
        }
        match expr {
            Expr::Call { function, .. } => {
                let call = match &function.0 {
                    Expr::Identifier(name) => format!("{name}(...)"),
                    Expr::FieldAccess { field, .. } => format!("{field}(...)"),
                    _ => "call".to_string(),
                };
                if let Some(target) = self.output.direct_call_targets.get(&key) {
                    self.target(target, span, &call);
                } else {
                    self.refuse(span, format!("`{call}` has no checked direct target"));
                }
            }
            Expr::MethodCall { method, .. } => self.method(&key, span, &format!("{method}(...)")),
            Expr::Clone(_) => self.method(&key, span, "clone"),
            Expr::Spawn { .. }
            | Expr::SpawnLambdaActor { .. }
            | Expr::Scope { .. }
            | Expr::ForkChild { .. }
            | Expr::ForkBlock { .. }
            | Expr::ScopeDeadline { .. }
            | Expr::Select { .. }
            | Expr::Race(_)
            | Expr::UnsafeBlock(_)
            | Expr::Yield(_)
            | Expr::Await(_)
            | Expr::AwaitRestart(_)
            | Expr::GenBlock { .. }
            | Expr::Lambda { .. }
            | Expr::MachineEmit { .. } => self.refuse(
                span,
                "concurrency, unsafe access and latent effects are not admitted in machine helpers",
            ),
            Expr::Identifier(name)
                if crate::ExecutionContextReader::from_surface_name(name).is_some() =>
            {
                self.refuse(span, "machine evaluation cannot observe execution context");
            }
            Expr::InterpolatedString(parts) => {
                for part in parts {
                    if let StringPart::Expr(value) = part {
                        if self
                            .output
                            .interpolation_display_types
                            .contains_key(&SpanKey::in_module(&value.1, self.module_idx))
                        {
                            self.refuse(&value.1, "custom Display calls are not yet classified for machine evaluation");
                        }
                    }
                }
            }
            _ => {}
        }
    }
}

fn pure_runtime(family: RuntimeCallFamily) -> bool {
    use RuntimeCallFamily as R;
    matches!(
        family,
        // A fresh local collection's own constructor is as pure as
        // `BytesNew`: no external effect, no reachable prior state. The
        // methods called on the value it produces (`Vec` methods, `HashMap`
        // insert) are admitted separately in `EffectVisitor::target` via
        // `CallTarget::RuntimeCollection`, not through this typed-family
        // route.
        R::VecNew
            // A shared payload is an ordinary owned value: allocating one,
            // sharing it and reading through it touch nothing outside it.
            // `set` writes through every sharer and stays refused.
            | R::RcNew
            | R::RcClone
            | R::RcGet
            | R::HashMapNew
            | R::HashMapNewWithLayout
            | R::MathIntrinsic(_)
            | R::StringCharAt
            | R::StringCharAtUtf8
            | R::StringCharCount
            | R::StringByteLen
            | R::StringConcat
            | R::StringEquals
            | R::StringStartsWith
            | R::StringEndsWith
            | R::StringIsEmpty
            | R::StringIsDigit
            | R::StringIsAlpha
            | R::StringIsAlphanumeric
            | R::StringFind
            | R::StringIndex
            | R::StringLen
            | R::StringSliceCodepoints
            | R::StringToBytes
            | R::StringToUppercase
            | R::StringTrim
            | R::U8ToString
            | R::I64ToString
            | R::BytesAppend
            | R::BytesClear
            | R::BytesContains
            | R::BytesDecodeUtf8
            | R::BytesDecodeUtf8Lossy
            | R::BytesGet
            | R::BytesIndex
            | R::BytesIsEmpty
            | R::BytesLen
            | R::BytesPop
            | R::BytesPush
            | R::BytesSet
            | R::BytesSlice
            | R::BytesNew
    )
}
