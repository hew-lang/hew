//! A bounded purity proof for the ordinary machine evaluator.
//!
//! Every reachable source helper is inspected through its checked declaration
//! identity. Runtime calls use the selected typed family. Unknown calls and
//! values with external identity cannot acquire a purity promise by spelling.

use std::collections::{HashMap, HashSet};

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
}

#[derive(Default)]
struct Summary {
    calls: Vec<(DefId, Span)>,
    refusal: Option<(Span, String)>,
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
        };
        if body.function.is_async
            || body.function.is_generator
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
    for declaration in machines {
        let mut visiting = HashSet::new();
        let mut proven = HashSet::new();
        if let Err((span, reason)) = prove(&declaration, &summaries, &mut visiting, &mut proven) {
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
    errors
}

fn collect_bodies<'a>(
    program: &'a Program,
    output: &TypeCheckOutput,
    bodies: &mut HashMap<DefId, Body<'a>>,
    machines: &mut Vec<DefId>,
    missing_steps: &mut Vec<Span>,
) {
    let mut add_items =
        |items: &'a [Spanned<Item>],
         contexts: Vec<(Option<crate::ModuleId>, u32, Option<String>)>| {
            for (ordinal, ((item, span), (module, module_idx, source_module))) in
                items.iter().zip(contexts).enumerate()
            {
                let mut add =
                    |function: &'a FnDecl, kind: DeclarationKind, declaration_span: &Span| {
                        let occurrence = DeclarationOccurrence::new_with_synthetic_ordinal(
                            module,
                            declaration_span,
                            ordinal,
                            kind,
                            0,
                        );
                        if let Some(declaration) = output.identity.declaration(occurrence) {
                            if function.origin == DeclarationOrigin::MachineStep
                                && !machines.contains(declaration)
                            {
                                machines.push(declaration.clone());
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
                    Item::Function(function) => add(function, DeclarationKind::Function, span),
                    Item::Impl(implementation) => {
                        for function in &implementation.methods {
                            add(function, DeclarationKind::ImplMethod, &function.fn_span);
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
    visiting.remove(declaration);
    proven.insert(declaration.clone());
    Ok(())
}

struct EffectVisitor<'a> {
    output: &'a TypeCheckOutput,
    module_idx: u32,
    summary: Summary,
    facts: &'a mut TypeFactService,
    callee_spans: HashSet<Span>,
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
        if !pure_data(ty, self.output, &mut HashSet::new()) {
            self.refuse(
                span,
                format!(
                    "`{}` can carry external identity, effects or unclassified payloads",
                    ty.user_facing()
                ),
            );
            return;
        }
        if !self
            .facts
            .require(ty)
            .is_ok_and(|facts| facts.clone != CloneKind::None)
        {
            self.refuse(
                span,
                format!(
                    "`{}` has no proven independent value copy",
                    ty.user_facing()
                ),
            );
        }
    }
}

impl NodeVisitor for EffectVisitor<'_> {
    fn visit_stmt(&mut self, stmt: &Stmt, span: &Span) {
        if matches!(stmt, Stmt::Defer(_) | Stmt::For { is_await: true, .. }) {
            self.refuse(
                span,
                "deferred effects and suspension are not admitted in machine helpers",
            );
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
            Expr::Send(_)
            | Expr::Spawn { .. }
            | Expr::SpawnLambdaActor { .. }
            | Expr::Scope { .. }
            | Expr::ForkChild { .. }
            | Expr::ForkBlock { .. }
            | Expr::ScopeDeadline { .. }
            | Expr::Select { .. }
            | Expr::Join(_)
            | Expr::Race(_)
            | Expr::Timeout { .. }
            | Expr::UnsafeBlock(_)
            | Expr::Yield(_)
            | Expr::This
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
