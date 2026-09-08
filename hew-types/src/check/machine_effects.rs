//! A bounded purity proof for the ordinary machine evaluator.
//!
//! Every reachable source helper is inspected through its checked declaration
//! identity. Runtime calls use the selected typed family. Unknown calls and
//! values with external identity cannot acquire a purity promise by spelling.

use std::collections::{BTreeMap, HashMap, HashSet};

use hew_parser::ast::{
    DeclarationOrigin, Expr, FnDecl, Item, Program, Span, Spanned, Stmt, StringPart,
};

use super::dispatch::{CallTarget, MethodTargetFamily};
use super::lints::{self, NodeVisitor};
use super::{MethodCallRewrite, SpanKey, TypeCheckOutput, UserComparisonDispatch};
use crate::error::{TypeError, TypeErrorKind};
use crate::{
    BuiltinType, CloneKind, DeclarationKind, DeclarationMarker, DeclarationOccurrence, DefId,
    ResolvedTy, RuntimeCallFamily, TypeFactService,
};

struct Body<'a> {
    function: &'a FnDecl,
    module_idx: u32,
    source_module: Option<String>,
    /// The enclosing machine's own type parameters, empty for every body that
    /// is not part of a machine's generated impl. Only a machine's own
    /// parameters may defer a purity obligation to instantiation.
    type_params: Vec<String>,
}

#[derive(Default)]
struct Summary {
    calls: Vec<(DefId, Span)>,
    refusal: Option<(Span, String)>,
    deferred: Vec<DeferredPurity>,
}

/// One value inside a generic machine whose purity depends on the machine's
/// type arguments (D427).
///
/// This is the same shape as the checker's `deferred_bound_checks`: an
/// obligation recorded where it cannot yet be decided and discharged once a
/// concrete argument is known. It is a separate list owned by this module
/// rather than an entry on that vector, because the obligation is a purity
/// proof over a substituted type, not a trait bound.
#[derive(Debug, Clone)]
struct DeferredPurity {
    ty: ResolvedTy,
}

/// The surface identity a generic machine is instantiated through.
#[derive(Debug, Clone, Default)]
struct MachineShape {
    type_name: String,
    type_params: Vec<String>,
}

pub(super) fn validate(output: &TypeCheckOutput) -> Vec<TypeError> {
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
    let mut facts =
        TypeFactService::new(output.type_fact_context.clone(), output.type_facts.clone());
    for (declaration, body) in &bodies {
        let mut visitor = EffectVisitor {
            output,
            module_idx: body.module_idx,
            summary: Summary::default(),
            facts: &mut facts,
            callee_spans: HashSet::new(),
            type_params: body.type_params.clone(),
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
    let mut errors = Vec::new();
    for (declaration, shape) in machines {
        let mut visiting = HashSet::new();
        let mut proven = HashSet::new();
        match prove(&declaration, &summaries, &mut visiting, &mut proven) {
            Ok(deferred) => errors.extend(instantiation_refusals(
                &shape, &deferred, output, &mut facts,
            )),
            Err((span, reason)) => {
                let mut error = TypeError::new(
                    TypeErrorKind::MachineExhaustivenessError,
                    span,
                    format!("machine evaluator is not demonstrably pure: {reason}"),
                );
                error.source_module = bodies
                    .get(&declaration)
                    .and_then(|body| body.source_module.clone());
                errors.push(error);
            }
        }
    }
    errors
}

/// Discharge a generic machine's deferred purity obligations at every concrete
/// instantiation the checked program actually produces (D427).
///
/// `resolved_expr_types` is the post-inference authority for concrete accepted
/// spans, so every instantiation a program can run is typed at some expression
/// there. One refusal is reported per distinct argument list, at the earliest
/// span that names it.
fn instantiation_refusals(
    shape: &MachineShape,
    deferred: &[DeferredPurity],
    output: &TypeCheckOutput,
    facts: &mut TypeFactService,
) -> Vec<TypeError> {
    if deferred.is_empty() || shape.type_params.is_empty() {
        return Vec::new();
    }
    let mut sites: BTreeMap<Vec<ResolvedTy>, Span> = BTreeMap::new();
    for (key, ty) in &output.resolved_expr_types {
        let mut found = Vec::new();
        collect_instantiations(ty, shape, output, &mut found);
        for args in found {
            let span = key.start..key.end;
            sites
                .entry(args)
                .and_modify(|earliest| {
                    if span.start < earliest.start {
                        *earliest = span.clone();
                    }
                })
                .or_insert(span);
        }
    }
    let mut errors = Vec::new();
    for (args, span) in sites {
        let Some(reason) = deferred.iter().find_map(|obligation| {
            let substituted =
                crate::value_class::substitute(&obligation.ty, &shape.type_params, &args);
            pure_value(&substituted, output, facts).err()
        }) else {
            continue;
        };
        let argument = args
            .iter()
            .find(|arg| pure_value(arg, output, facts).is_err())
            .unwrap_or_else(|| &args[0]);
        errors.push(TypeError::new(
            TypeErrorKind::MachineExhaustivenessError,
            span,
            format!(
                "machine `{}` cannot be instantiated with `{}`: {reason}",
                shape.type_name,
                argument.user_facing()
            ),
        ));
    }
    errors
}

/// Every concrete instantiation of `shape`'s own type that `ty` contains.
fn collect_instantiations(
    ty: &ResolvedTy,
    shape: &MachineShape,
    output: &TypeCheckOutput,
    found: &mut Vec<Vec<ResolvedTy>>,
) {
    match ty {
        ResolvedTy::Named { name, args, .. } => {
            if name == &shape.type_name
                && args.len() == shape.type_params.len()
                && !args.iter().any(|arg| is_abstract(arg, output))
            {
                found.push(args.clone());
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

#[expect(
    clippy::too_many_lines,
    reason = "one item walk over the root program and every module in the graph"
)]
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
                            type_params: shape.type_params.clone(),
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
) -> Result<Vec<DeferredPurity>, (Span, String)> {
    if proven.contains(declaration) || !visiting.insert(declaration.clone()) {
        return Ok(Vec::new());
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
    let mut deferred = summary.deferred.clone();
    for (callee, span) in &summary.calls {
        match prove(callee, summaries, visiting, proven) {
            Ok(reached) => deferred.extend(reached),
            Err((_, reason)) => {
                return Err((
                    span.clone(),
                    format!("call to `{}`: {reason}", callee.display_name()),
                ))
            }
        }
    }
    visiting.remove(declaration);
    proven.insert(declaration.clone());
    Ok(deferred)
}

struct EffectVisitor<'a> {
    output: &'a TypeCheckOutput,
    module_idx: u32,
    summary: Summary,
    facts: &'a mut TypeFactService,
    callee_spans: HashSet<Span>,
    type_params: Vec<String>,
}

impl EffectVisitor<'_> {
    fn refuse(&mut self, span: &Span, reason: impl Into<String>) {
        if self.summary.refusal.is_none() {
            self.summary.refusal = Some((span.clone(), reason.into()));
        }
    }
    fn target(&mut self, target: &CallTarget, span: &Span) {
        match target {
            CallTarget::User(declaration) | CallTarget::ImplMethod(declaration) => {
                self.summary.calls.push((declaration.clone(), span.clone()));
            }
            CallTarget::Runtime(family) if pure_runtime(*family) => {}
            CallTarget::RuntimeCollection(MethodTargetFamily::Vec(method))
                if *method != super::dispatch::VecMethod::Contains => {}
            // These closed compiler intrinsics raise an ordinary checked fault;
            // they do not perform externally visible work before unwinding.
            CallTarget::Builtin { endpoint } if matches!(endpoint.as_str(), "panic" | "assert") => {
            }
            _ => self.refuse(
                span,
                format!("selected call `{target:?}` is not admitted in a machine transition"),
            ),
        }
    }
    fn method(&mut self, key: &SpanKey, span: &Span) {
        if let Some(call) = self.output.resolved_calls.get(key) {
            self.target(&call.target, span);
            return;
        }
        match self.output.method_call_rewrites.get(key) {
            Some(
                MethodCallRewrite::RewriteToFunction { target, .. }
                | MethodCallRewrite::RewriteModuleQualifiedToFunction { target, .. },
            ) => self.target(target, span),
            Some(
                MethodCallRewrite::GenericMathIntrinsic { .. }
                | MethodCallRewrite::BuiltinOptionResult { .. }
                | MethodCallRewrite::BuiltinVecIntoIter
                | MethodCallRewrite::BuiltinVecIter
                | MethodCallRewrite::BuiltinVecIterNext
                | MethodCallRewrite::VecFrom
                | MethodCallRewrite::RecordCloneInplace { .. }
                | MethodCallRewrite::CopyCloneNoop,
            ) => {}
            _ => self.refuse(span, "method call has no admitted pure checked target"),
        }
    }
    fn value(&mut self, ty: &ResolvedTy, span: &Span) {
        let Err(reason) = pure_value(ty, self.output, self.facts) else {
            return;
        };
        // A value spelled through the machine's own type parameter cannot be
        // judged before the machine is instantiated. Defer it when some
        // concrete argument would satisfy the proof, and refuse here when none
        // could, so an unconditionally impure generic machine still fails at
        // its declaration (D427).
        if mentions_type_param(ty, &self.type_params) {
            let probe = pure_probe_arguments(self.type_params.len());
            let substituted = crate::value_class::substitute(ty, &self.type_params, &probe);
            if pure_value(&substituted, self.output, self.facts).is_ok() {
                self.summary
                    .deferred
                    .push(DeferredPurity { ty: ty.clone() });
                return;
            }
        }
        self.refuse(span, reason);
    }
}

/// The one purity predicate. Both the declaration-time walk and the
/// instantiation-time discharge run this, so a machine cannot be admitted by
/// one rule and refused by a second copy of it.
fn pure_value(
    ty: &ResolvedTy,
    output: &TypeCheckOutput,
    facts: &mut TypeFactService,
) -> Result<(), String> {
    if !pure_data(ty, output, &mut HashSet::new()) {
        return Err(format!(
            "`{}` can carry external identity, effects or unclassified payloads",
            ty.user_facing()
        ));
    }
    if !facts
        .require(ty)
        .is_ok_and(|facts| facts.clone != CloneKind::None)
    {
        return Err(format!(
            "`{}` has no proven independent value copy",
            ty.user_facing()
        ));
    }
    Ok(())
}

/// A concrete argument list that is pure by construction, used only to decide
/// whether a refusal is about the parameter or about the shape around it.
fn pure_probe_arguments(count: usize) -> Vec<ResolvedTy> {
    vec![ResolvedTy::I64; count]
}

/// Does `ty` name one of `params`?
///
/// A declaration's own parameter reaches here spelled either as an abstract
/// `TypeParam` or, without a type-parameter scope, as a zero-argument user
/// `Named`. `value_class::substitute` accepts both, so both count here.
fn mentions_type_param(ty: &ResolvedTy, params: &[String]) -> bool {
    if params.is_empty() {
        return false;
    }
    match ty {
        ResolvedTy::TypeParam { name } => params.iter().any(|param| param == name),
        ResolvedTy::Named { name, args, .. } => {
            (args.is_empty() && params.iter().any(|param| param == name))
                || args.iter().any(|arg| mentions_type_param(arg, params))
        }
        ResolvedTy::Tuple(elements) => elements
            .iter()
            .any(|element| mentions_type_param(element, params)),
        ResolvedTy::Array(element, _) | ResolvedTy::Slice(element) | ResolvedTy::Task(element) => {
            mentions_type_param(element, params)
        }
        _ => false,
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
        if let Some(dispatch) = self.output.user_comparison_dispatch.get(&key) {
            let (UserComparisonDispatch::Eq { method }
            | UserComparisonDispatch::Ord { method }
            | UserComparisonDispatch::PartialOrd { method }) = dispatch;
            self.summary.calls.push((method.clone(), span.clone()));
        }
        match expr {
            Expr::Call { function, .. } => {
                self.callee_spans.insert(function.1.clone());
                if let Some(target) = self.output.direct_call_targets.get(&key) {
                    self.target(target, span);
                } else {
                    self.refuse(span, "call has no checked direct target");
                }
            }
            Expr::MethodCall { .. } | Expr::Clone(_) => self.method(&key, span),
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
        if !self.callee_spans.contains(span) {
            if let Some(ty) = self.output.resolved_expr_types.get(&key) {
                self.value(ty, span);
            }
        }
    }
}

fn pure_data(
    ty: &ResolvedTy,
    output: &TypeCheckOutput,
    visiting: &mut HashSet<ResolvedTy>,
) -> bool {
    if !visiting.insert(ty.clone()) {
        return true;
    }
    let admitted = match ty {
        ResolvedTy::I8
        | ResolvedTy::I16
        | ResolvedTy::I32
        | ResolvedTy::I64
        | ResolvedTy::U8
        | ResolvedTy::U16
        | ResolvedTy::U32
        | ResolvedTy::U64
        | ResolvedTy::Isize
        | ResolvedTy::Usize
        | ResolvedTy::F32
        | ResolvedTy::F64
        | ResolvedTy::Bool
        | ResolvedTy::Char
        | ResolvedTy::String
        | ResolvedTy::Bytes
        | ResolvedTy::Duration
        | ResolvedTy::Unit
        | ResolvedTy::Never => true,
        ResolvedTy::Tuple(fields) => fields.iter().all(|ty| pure_data(ty, output, visiting)),
        ResolvedTy::Array(element, _) => pure_data(element, output, visiting),
        ResolvedTy::Named {
            builtin: Some(BuiltinType::Vec | BuiltinType::Option | BuiltinType::Result),
            args,
            ..
        } => args.iter().all(|ty| pure_data(ty, output, visiting)),
        ResolvedTy::Named {
            name,
            builtin: None,
            is_opaque: false,
            args,
        } => output
            .type_fact_context
            .declarations()
            .get(name)
            .is_some_and(|declaration| {
                declaration.marker == DeclarationMarker::None
                    && !declaration.is_opaque
                    && declaration.type_params.len() == args.len()
                    && declaration.members.iter().all(|member| {
                        pure_data(
                            &crate::value_class::substitute(member, &declaration.type_params, args),
                            output,
                            visiting,
                        )
                    })
            }),
        _ => false,
    };
    visiting.remove(ty);
    admitted
}

fn pure_runtime(family: RuntimeCallFamily) -> bool {
    use RuntimeCallFamily as R;
    matches!(
        family,
        R::MathIntrinsic(_)
            | R::StringCharAt
            | R::StringCharAtUtf8
            | R::StringCharCount
            | R::StringByteLen
            | R::StringConcat
            | R::StringEquals
            | R::StringStartsWith
            | R::StringIsEmpty
            | R::StringFind
            | R::StringGet
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
