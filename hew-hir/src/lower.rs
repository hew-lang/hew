use std::collections::{HashMap, HashSet};

use hew_parser::ast::{
    ActorDecl, AttributeArg, BinaryOp, Block, Expr, FnDecl, Item, LambdaParam, Literal,
    MachineDecl, Param, Pattern, Program, ReceiveFnDecl, RecordDecl, RecordKind, ResourceMarker,
    RestartPolicy, SelectArm, Span, Spanned, Stmt, SupervisorDecl, SupervisorStrategy,
    TimeoutClause, TypeBodyItem, TypeDecl, TypeExpr,
};
use hew_types::{
    ActorStateGuard, AssignTargetKind, AssignTargetShape, ClosureCaptureFact, LoweringFact,
    MethodCallRewrite, ResolvedTy, SpanKey, Ty, TypeCheckOutput,
};

use crate::builtin_type_classes::seed_builtin_type_classes;
use crate::diagnostic::{HirDiagnostic, HirDiagnosticKind};
use crate::ids::{BindingId, IdGen, ItemId, ResolvedRef, SiteId};
use crate::monomorph::{
    contains_recursive_polymorphic_self, substitute_type_params, MonoKey, MonoRegistry,
    RecordLayoutRegistry, RecordMonoKey, MONOMORPHISATION_REGISTRY_CAP,
};
use crate::node::{
    HirActorDecl, HirActorInit, HirActorMethod, HirActorReceiveFn, HirActorStateGuard, HirBinding,
    HirBlock, HirCaptureKind, HirClosureCapture, HirExpr, HirExprKind, HirField, HirFn, HirItem,
    HirLambdaCapture, HirLifecycleHook, HirLifecycleHookKind, HirLiteral, HirMachineDecl,
    HirMachineEvent, HirMachineState, HirMachineTransition, HirModule, HirRecordDecl,
    HirRestartPolicy, HirSelect, HirSelectArm, HirSelectArmKind, HirStmt, HirStmtKind,
    HirSupervisorChild, HirSupervisorDecl, HirSupervisorStrategy, HirTypeDecl,
};
use crate::{IntentKind, ValueClass};

type ScopeBinding = (BindingId, ResolvedTy, std::ops::Range<usize>);
type ScopeMap = HashMap<String, ScopeBinding>;
type OuterClosureBinding = (String, ResolvedTy, std::ops::Range<usize>);
type ClosureCaptureCandidate = (BindingId, String, std::ops::Range<usize>);

#[derive(Debug, Clone, Default)]
pub struct ResolutionCtx;

#[derive(Debug, Clone, PartialEq)]
pub struct LowerOutput {
    pub module: HirModule,
    pub diagnostics: Vec<HirDiagnostic>,
}

/// Pre-collected signature of a top-level function item.
#[derive(Debug)]
struct FnEntry {
    id: ItemId,
    return_ty: ResolvedTy,
    param_tys: Vec<ResolvedTy>,
    /// Source-declared generic type parameter names, in order. Empty for
    /// non-generic functions. Consulted at `Expr::Call` lowering sites to
    /// decide whether the callee is a generic top-level user fn that
    /// requires a monomorphisation-registry entry.
    type_params: Vec<String>,
}

/// Pre-collected shape of a top-level user record or `pub type` item.
///
/// Populated in the type-decl / record pre-pass and consulted at
/// `Expr::StructInit` lowering to (a) decide whether a record-init site
/// needs a per-instantiation `RecordLayout` and (b) substitute the
/// type-params with the concrete args from `record_init_type_args` to
/// produce the layout's field shape.
///
/// Tuple-form records are admitted with `fields = vec![]` — their
/// constructor is reached via `Expr::Call`, not `StructInit`, so they
/// never hit the record-layout registry. The entry is kept for shape
/// uniformity with the named-form path.
#[derive(Debug)]
struct RecordEntry {
    id: ItemId,
    /// Source-declared generic type-parameter names, in order. Empty for
    /// monomorphic record/type declarations.
    type_params: Vec<String>,
    /// Field shape as written in source — types still mention the
    /// declaration's type params verbatim (no substitution yet). The
    /// record-layout registry walks these and substitutes per
    /// instantiation.
    fields: Vec<(String, ResolvedTy)>,
}

#[must_use]
pub fn lower_program(
    program: &Program,
    type_check_output: &TypeCheckOutput,
    ctx: &ResolutionCtx,
) -> LowerOutput {
    lower_program_with_mono_cap(
        program,
        type_check_output,
        ctx,
        MONOMORPHISATION_REGISTRY_CAP,
    )
}

/// Variant of [`lower_program`] with an explicit monomorphisation-registry
/// cap. Intended for tests that exercise the
/// `MonomorphisationCapExceeded` diagnostic with a small fixture; the
/// production entry point [`lower_program`] always uses
/// `MONOMORPHISATION_REGISTRY_CAP`.
#[must_use]
#[allow(
    clippy::too_many_lines,
    reason = "three structured passes (fn pre-pass, record/type-decl pre-pass, \
              source-order emit) read more clearly here than across helpers"
)]
pub fn lower_program_with_mono_cap(
    program: &Program,
    type_check_output: &TypeCheckOutput,
    _ctx: &ResolutionCtx,
    mono_cap: usize,
) -> LowerOutput {
    let mut ctx = LowerCtx::new(type_check_output, mono_cap);

    // First pass: collect all function signatures so that forward and mutual
    // references in call expressions resolve to the correct return type.
    // Diagnostics from this pass are discarded — the same types are re-lowered
    // in the second pass, which is where canonical diagnostics are emitted.
    for (item, _) in &program.items {
        match item {
            Item::Function(func) => {
                ctx.register_fn_entry(&func.name, func);
            }
            Item::Impl(impl_decl)
                if impl_decl
                    .trait_bound
                    .as_ref()
                    .is_some_and(|bound| bound.name == "Index") =>
            {
                if let TypeExpr::Named { name, .. } = &impl_decl.target_type.0 {
                    for method in &impl_decl.methods {
                        ctx.register_fn_entry(&format!("{name}::{}", method.name), method);
                    }
                }
            }
            _ => {}
        }
    }
    // Pre-pass: collect record/type-decl shapes so `Expr::StructInit`
    // lowering in the source-order pass can answer "is this a generic
    // user record?" regardless of declaration order relative to the
    // function that uses it. This mirrors the fn pre-pass above and is
    // the producer half of the record-layout registry.
    for (item, _) in &program.items {
        match item {
            Item::TypeDecl(decl) => {
                let id = ctx.ids.item();
                let type_params: Vec<String> = decl
                    .type_params
                    .as_ref()
                    .map_or(vec![], |ps| ps.iter().map(|p| p.name.clone()).collect());
                let mut fields: Vec<(String, ResolvedTy)> = Vec::new();
                for body_item in &decl.body {
                    if let TypeBodyItem::Field { name, ty, .. } = body_item {
                        fields.push((name.clone(), ctx.lower_type(ty)));
                    }
                }
                ctx.record_registry.insert(
                    decl.name.clone(),
                    RecordEntry {
                        id,
                        type_params,
                        fields,
                    },
                );
            }
            Item::Record(decl) => {
                let id = ctx.ids.item();
                let type_params: Vec<String> = decl
                    .type_params
                    .as_ref()
                    .map_or(vec![], |ps| ps.iter().map(|p| p.name.clone()).collect());
                let fields: Vec<(String, ResolvedTy)> = match &decl.kind {
                    RecordKind::Named(rfs) => rfs
                        .iter()
                        .map(|rf| (rf.name.clone(), ctx.lower_type(&rf.ty)))
                        .collect(),
                    // Tuple-form records are reached via Expr::Call, not
                    // Expr::StructInit, so their record-layout registry
                    // entry would never be exercised. Record an empty
                    // field list for shape uniformity.
                    RecordKind::Tuple(_) => Vec::new(),
                };
                ctx.record_registry.insert(
                    decl.name.clone(),
                    RecordEntry {
                        id,
                        type_params,
                        fields,
                    },
                );
            }
            _ => {}
        }
    }
    // Discard pre-pass diagnostics from `lower_type`; the third pass re-emits
    // any real ones when it produces the canonical HirTypeDecl/HirRecordDecl.
    ctx.diagnostics.clear();

    // Second pass: lower type declarations and populate the per-module
    // type-class registry. Stored here so the source-order pass can emit them
    // in program order without a second lowering call. Function bodies depend
    // on type markers (so `ValueClass::of_ty` resolves `Named` types
    // correctly), but type-decl bodies do not depend on function signatures,
    // so this pre-pass can safely run before the combined item pass below.
    let mut type_decl_cache: HashMap<*const hew_parser::ast::TypeDecl, HirTypeDecl> =
        HashMap::new();
    for (item, span) in &program.items {
        if let Item::TypeDecl(decl) = item {
            let hir_decl = ctx.lower_type_decl(decl, span.clone());
            let close_method = if hir_decl.marker == ResourceMarker::Resource {
                hir_decl
                    .consuming_methods
                    .iter()
                    .find(|m| m.as_str() == "close")
                    .cloned()
            } else {
                None
            };
            ctx.type_classes
                .insert(hir_decl.name.clone(), (hir_decl.marker, close_method));
            type_decl_cache.insert(decl as *const _, hir_decl);
        }
    }

    // Third pass: emit all items in source order now that both fn signatures
    // and type markers are fully resolved.
    let mut items: Vec<HirItem> = Vec::new();
    for (item, span) in &program.items {
        match item {
            Item::TypeDecl(decl) => {
                // Retrieve the already-lowered decl so diagnostics are not
                // emitted a second time.
                if let Some(hir_decl) = type_decl_cache.remove(&(decl as *const _)) {
                    items.push(HirItem::TypeDecl(hir_decl));
                }
            }
            Item::Function(func) => {
                items.push(HirItem::Function(ctx.lower_fn(func, span.clone())));
            }
            Item::Impl(impl_decl)
                if impl_decl
                    .trait_bound
                    .as_ref()
                    .is_some_and(|bound| bound.name == "Index") =>
            {
                if let TypeExpr::Named { name, .. } = &impl_decl.target_type.0 {
                    for method in &impl_decl.methods {
                        let fn_name = format!("{name}::{}", method.name);
                        items.push(HirItem::Function(ctx.lower_fn_with_name(
                            method,
                            &fn_name,
                            span.clone(),
                        )));
                    }
                }
            }
            Item::Machine(machine) => {
                if let Some(hir_machine) = ctx.lower_machine(machine, span.clone()) {
                    items.push(HirItem::Machine(hir_machine));
                }
            }
            Item::Actor(actor) => {
                items.push(HirItem::Actor(ctx.lower_actor(actor, span.clone())));
            }
            Item::Record(decl) => {
                items.push(HirItem::Record(ctx.lower_record_decl(decl, span.clone())));
            }
            Item::Supervisor(decl) => {
                items.push(HirItem::Supervisor(
                    ctx.lower_supervisor(decl, span.clone()),
                ));
            }
            _ => ctx.unsupported(span.clone(), "top-level-item", "slice-2"),
        }
    }

    let mut monomorphisations = ctx.mono_registry.into_vec();
    let call_site_type_args = ctx.call_site_type_args;
    let record_layouts = ctx.record_layout_registry.into_vec();

    // Closure under substitution: walk every monomorphisation's origin
    // body, find inner generic-fn call sites, substitute their recorded
    // type args via the monomorphisation's substitution map, and add
    // any newly discovered concrete instantiations to the registry.
    // Repeat to a fixed point (bounded by the configured cap).
    closure_under_substitution(
        &items,
        &call_site_type_args,
        &mut monomorphisations,
        mono_cap,
        &mut ctx.diagnostics,
    );

    LowerOutput {
        module: HirModule {
            items,
            type_classes: ctx.type_classes,
            monomorphisations,
            call_site_type_args,
            record_layouts,
        },
        diagnostics: ctx.diagnostics,
    }
}

/// Walk each monomorphisation's origin fn body, substitute the
/// per-monomorphisation type-arg map into every inner Call site's
/// recorded type arguments, and add any newly-discovered concrete
/// `(origin_fn, Vec<ResolvedTy>)` pairs to the registry. Iterates to a
/// fixed point.
///
/// Emits `MonomorphisationCapExceeded` (at most once) when the cap is
/// hit. Skips inner calls whose substituted args still contain any
/// abstract type-parameter symbol (no monomorphisation is possible
/// without a concrete instantiation).
fn closure_under_substitution(
    items: &[HirItem],
    call_site_type_args: &HashMap<SiteId, Vec<ResolvedTy>>,
    monomorphisations: &mut Vec<crate::monomorph::MonomorphizedFn>,
    cap: usize,
    diagnostics: &mut Vec<HirDiagnostic>,
) {
    use crate::monomorph::{mangle, MonomorphizedFn};

    // Build origin_id → &HirFn map for body lookup.
    let mut origin_fns: HashMap<ItemId, &HirFn> = HashMap::new();
    // Map of fn name → (origin id, type_params) for inner-call resolution.
    let mut fn_info: HashMap<String, (ItemId, Vec<String>)> = HashMap::new();
    for item in items {
        if let HirItem::Function(f) = item {
            origin_fns.insert(f.id, f);
            fn_info.insert(f.name.clone(), (f.id, f.type_params.clone()));
        }
    }

    let mut seen: HashSet<MonoKey> = monomorphisations.iter().map(|m| m.key.clone()).collect();
    let mut worklist: Vec<MonoKey> = monomorphisations.iter().map(|m| m.key.clone()).collect();
    let mut cap_diag_emitted = false;

    while let Some(key) = worklist.pop() {
        let Some(origin) = origin_fns.get(&key.origin).copied() else {
            continue;
        };
        // Build substitution map: type_param name → concrete arg.
        let subst: HashMap<String, ResolvedTy> = origin
            .type_params
            .iter()
            .cloned()
            .zip(key.type_args.iter().cloned())
            .collect();
        // Walk the body to discover Call sites.
        let mut inner_sites: Vec<(String, SiteId)> = Vec::new();
        collect_call_sites_in_block(&origin.body, &mut inner_sites);
        for (callee_name, site) in inner_sites {
            let Some((origin_id, type_params)) = fn_info.get(&callee_name).cloned() else {
                continue;
            };
            if type_params.is_empty() {
                continue;
            }
            let Some(args) = call_site_type_args.get(&site) else {
                continue;
            };
            // Substitute the recorded args.
            let substituted: Vec<ResolvedTy> =
                args.iter().map(|t| substitute_ty(t, &subst)).collect();
            // Skip if still abstract — the surrounding mono is generic
            // in some symbol that we don't have a concrete value for.
            if substituted
                .iter()
                .any(|t| contains_abstract_symbol(t, &fn_info))
            {
                continue;
            }
            let new_key = MonoKey {
                origin: origin_id,
                origin_name: callee_name.clone(),
                type_args: substituted.clone(),
            };
            if !seen.insert(new_key.clone()) {
                continue;
            }
            if monomorphisations.len() >= cap {
                if !cap_diag_emitted {
                    cap_diag_emitted = true;
                    diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::MonomorphisationCapExceeded { cap },
                        0..0,
                        "too many distinct generic-function instantiations discovered \
                         during inner-call closure; the compiler refuses to \
                         monomorphise beyond the configured cap",
                    ));
                }
                continue;
            }
            let mangled = mangle(&callee_name, &substituted);
            monomorphisations.push(MonomorphizedFn {
                key: new_key.clone(),
                mangled_name: mangled,
            });
            worklist.push(new_key);
        }
    }
}

fn collect_call_sites_in_block(block: &HirBlock, out: &mut Vec<(String, SiteId)>) {
    for stmt in &block.statements {
        collect_call_sites_in_stmt(stmt, out);
    }
    if let Some(tail) = &block.tail {
        collect_call_sites_in_expr(tail, out);
    }
}

fn collect_call_sites_in_stmt(stmt: &HirStmt, out: &mut Vec<(String, SiteId)>) {
    match &stmt.kind {
        HirStmtKind::Let(_, Some(e)) | HirStmtKind::Expr(e) | HirStmtKind::Return(Some(e)) => {
            collect_call_sites_in_expr(e, out);
        }
        _ => {}
    }
}

fn collect_call_sites_in_expr(expr: &HirExpr, out: &mut Vec<(String, SiteId)>) {
    match &expr.kind {
        HirExprKind::Call { callee, args } | HirExprKind::SpawnedCall { callee, args, .. } => {
            // Record the site if callee is a direct BindingRef name.
            if let HirExprKind::BindingRef { name, .. } = &callee.kind {
                out.push((name.clone(), expr.site));
            }
            collect_call_sites_in_expr(callee, out);
            for a in args {
                collect_call_sites_in_expr(a, out);
            }
        }
        HirExprKind::Binary { left, right, .. } | HirExprKind::IdentityCompare { left, right } => {
            collect_call_sites_in_expr(left, out);
            collect_call_sites_in_expr(right, out);
        }
        HirExprKind::Block(b) => collect_call_sites_in_block(b, out),
        HirExprKind::If {
            condition,
            then_expr,
            else_expr,
        } => {
            collect_call_sites_in_expr(condition, out);
            collect_call_sites_in_expr(then_expr, out);
            if let Some(e) = else_expr {
                collect_call_sites_in_expr(e, out);
            }
        }
        HirExprKind::StructInit { fields, base, .. } => {
            for (_, e) in fields {
                collect_call_sites_in_expr(e, out);
            }
            if let Some(b) = base {
                collect_call_sites_in_expr(b, out);
            }
        }
        HirExprKind::FieldAccess { object, .. } => collect_call_sites_in_expr(object, out),
        HirExprKind::Scope { body } | HirExprKind::ForkBlock { body, .. } => {
            collect_call_sites_in_block(body, out);
        }
        HirExprKind::ScopeDeadline { duration, body } => {
            collect_call_sites_in_expr(duration, out);
            collect_call_sites_in_block(body, out);
        }
        HirExprKind::TupleIndex { tuple, .. } => collect_call_sites_in_expr(tuple, out),
        HirExprKind::Index { container, index } => {
            collect_call_sites_in_expr(container, out);
            collect_call_sites_in_expr(index, out);
        }
        HirExprKind::Slice {
            container,
            start,
            end,
            ..
        } => {
            collect_call_sites_in_expr(container, out);
            if let Some(s) = start {
                collect_call_sites_in_expr(s, out);
            }
            if let Some(e) = end {
                collect_call_sites_in_expr(e, out);
            }
        }
        HirExprKind::SpawnLambdaActor { body, .. } | HirExprKind::Closure { body, .. } => {
            collect_call_sites_in_expr(body, out);
        }
        _ => {}
    }
}

/// Substitute `ResolvedTy::Named { name, args: [] }` whose `name`
/// appears in `subst` with the mapped concrete type. Recurses into
/// composite type constructors. Used by the closure-under-substitution
/// pass and by MIR-side monomorphisation lowering.
#[must_use]
pub fn substitute_ty<S: std::hash::BuildHasher>(
    ty: &ResolvedTy,
    subst: &HashMap<String, ResolvedTy, S>,
) -> ResolvedTy {
    match ty {
        ResolvedTy::Named { name, args } if args.is_empty() && subst.contains_key(name) => {
            subst[name].clone()
        }
        ResolvedTy::Named { name, args } => ResolvedTy::Named {
            name: name.clone(),
            args: args.iter().map(|a| substitute_ty(a, subst)).collect(),
        },
        ResolvedTy::Tuple(items) => {
            ResolvedTy::Tuple(items.iter().map(|t| substitute_ty(t, subst)).collect())
        }
        ResolvedTy::Array(elem, n) => ResolvedTy::Array(Box::new(substitute_ty(elem, subst)), *n),
        ResolvedTy::Slice(elem) => ResolvedTy::Slice(Box::new(substitute_ty(elem, subst))),
        ResolvedTy::Function { params, ret } => ResolvedTy::Function {
            params: params.iter().map(|p| substitute_ty(p, subst)).collect(),
            ret: Box::new(substitute_ty(ret, subst)),
        },
        ResolvedTy::Closure {
            params,
            ret,
            captures,
        } => ResolvedTy::Closure {
            params: params.iter().map(|p| substitute_ty(p, subst)).collect(),
            ret: Box::new(substitute_ty(ret, subst)),
            captures: captures.iter().map(|c| substitute_ty(c, subst)).collect(),
        },
        ResolvedTy::Pointer {
            is_mutable,
            pointee,
        } => ResolvedTy::Pointer {
            is_mutable: *is_mutable,
            pointee: Box::new(substitute_ty(pointee, subst)),
        },
        ResolvedTy::Task(inner) => ResolvedTy::Task(Box::new(substitute_ty(inner, subst))),
        _ => ty.clone(),
    }
}

fn contains_abstract_symbol(
    ty: &ResolvedTy,
    fn_info: &HashMap<String, (ItemId, Vec<String>)>,
) -> bool {
    // A type contains an abstract symbol if any `Named { args: [] }`
    // matches a type-parameter name declared on any top-level fn.
    let is_type_param = |name: &str| {
        fn_info
            .values()
            .any(|(_, params)| params.iter().any(|p| p == name))
    };
    match ty {
        ResolvedTy::Named { name, args } => {
            if args.is_empty() && is_type_param(name) {
                return true;
            }
            args.iter().any(|a| contains_abstract_symbol(a, fn_info))
        }
        ResolvedTy::Tuple(items) => items.iter().any(|t| contains_abstract_symbol(t, fn_info)),
        ResolvedTy::Array(elem, _) | ResolvedTy::Slice(elem) => {
            contains_abstract_symbol(elem, fn_info)
        }
        ResolvedTy::Function { params, ret } => {
            params.iter().any(|p| contains_abstract_symbol(p, fn_info))
                || contains_abstract_symbol(ret, fn_info)
        }
        ResolvedTy::Closure {
            params,
            ret,
            captures,
        } => {
            params.iter().any(|p| contains_abstract_symbol(p, fn_info))
                || contains_abstract_symbol(ret, fn_info)
                || captures
                    .iter()
                    .any(|c| contains_abstract_symbol(c, fn_info))
        }
        ResolvedTy::Pointer { pointee, .. } => contains_abstract_symbol(pointee, fn_info),
        ResolvedTy::Task(inner) => contains_abstract_symbol(inner, fn_info),
        _ => false,
    }
}

#[derive(Debug)]
struct LowerCtx {
    ids: IdGen,
    scopes: Vec<ScopeMap>,
    /// Maps function name → pre-allocated `ItemId` + return type + param types.
    fn_registry: HashMap<String, FnEntry>,
    /// Per-named-type marker + close-method registry. Pre-populated from
    /// every `Item::TypeDecl` before function bodies lower so that
    /// `ValueClass::of_ty` can resolve `Named` types as the body is walked.
    /// Also seeded with M2 substrate types (Duplex, Sink, Stream, etc.) via
    /// `builtin_type_classes::seed_builtin_type_classes` before the `TypeDecl` loop.
    type_classes: crate::value_class::TypeClassTable,
    diagnostics: Vec<HirDiagnostic>,
    /// Checker-owned method-call lowering decisions. Keyed by the method-call
    /// expression span. `Expr::MethodCall` lowering looks up each call site
    /// here and rewrites to `HirExprKind::Call` with the runtime symbol.
    /// A missing entry is a fail-closed diagnostic (`MethodCallNoRewrite`).
    method_call_rewrites: HashMap<SpanKey, MethodCallRewrite>,
    /// Per-call-site `T → dyn Trait` coercion side-table. Keyed by the
    /// argument expression span. `lower_expr` consults this at every
    /// expression's exit; if the just-lowered expression's span has an
    /// entry, the result is wrapped in `HirExprKind::CoerceToDynTrait`.
    /// Carries the checker's authoritative method-table resolution.
    dyn_trait_coercions: HashMap<SpanKey, hew_types::DynCoercion>,
    /// Per-call-site `dyn Trait` method-dispatch side-table. Keyed by the
    /// method-call expression span. `lower_method_call` consults this
    /// before the `method_call_rewrites` branch so that
    /// `obj.method()` on a `dyn`-typed receiver lowers to
    /// `HirExprKind::CallDynMethod` (vtable slot index attached) rather
    /// than failing closed on the missing rewrite entry.
    dyn_trait_method_calls: HashMap<SpanKey, hew_types::DynMethodCall>,
    /// Checker-inferred types for every expression, keyed by expression span.
    /// Consulted at `Expr::Call` sites to determine the call-result type from
    /// checker authority rather than re-deriving from the callee's HIR type.
    /// This is the canonical source of truth for builtin callee result types
    /// (e.g. `duplex_pair`) that have no AST `fn` entry and therefore no
    /// `fn_registry` hit.
    expr_types: HashMap<SpanKey, Ty>,
    /// Checker-authoritative general-closure capture facts keyed by closure
    /// literal span. HIR consumes this ledger fail-closed when materialising
    /// `HirExprKind::Closure`; it does not infer capture legality from syntax.
    closure_capture_facts: HashMap<SpanKey, Vec<ClosureCaptureFact>>,
    /// Depth counter for nested `scope{}` bodies. When > 0, statement-expression
    /// calls are inferred as child-task spawns (TI-1); outside any scope body
    /// all calls are synchronous (TI-3). Using a depth counter rather than a
    /// bool supports nested `scope{}` blocks correctly.
    scope_depth: u32,
    /// Set to `true` immediately before lowering the expression of a
    /// `Stmt::Expression` statement. `lower_expr` consumes it via
    /// `mem::replace(…, false)` at entry, so all recursive calls see `false`.
    /// This lets `Expr::Await` check whether it is the direct statement, not
    /// a sub-expression of a return value, argument, binary operand, etc.
    /// (TI-4 position rule.)
    statement_position: bool,
    /// `Some((let_id, let_name))` while lowering the body of an actor-lambda
    /// that is the value of `let <let_name> = actor |..| { .. }`. The
    /// capture-strength classifier inside the body walk compares each
    /// resolved capture's `BindingId` to `let_id` to discriminate the
    /// self-reference (Weak, §5.9 ratification 2) from every other captured
    /// binding (Strong). Nested actor-lambdas restore the prior value via
    /// `mem::replace` so the outer self-binding doesn't leak into an inner
    /// lambda's classification.
    current_actor_self: Option<(BindingId, String)>,
    /// Checker-resolved type arguments for generic function calls that
    /// lack explicit type annotations. Keyed by the call expression span.
    ///
    /// Consumed at `Expr::Call` lowering (G-1.a, producer-bridge wakeup):
    /// every callsite whose callee is a generic top-level user fn
    /// (non-empty `FnEntry.type_params`) is paired with the entry here
    /// and inserted into `mono_registry`. A poisoned entry (failing the
    /// `ResolvedTy::from_ty` boundary conversion) emits
    /// `MonomorphisationCallTypeArgsViolation`; a generic callsite with
    /// no entry at all is treated as the trivially-monomorphic case
    /// (e.g. a builtin or runtime-symbol call) and skipped.
    /// (LESSONS: checker-authority P0, producer-bridge-before-codegen P1)
    call_type_args: HashMap<SpanKey, Vec<Ty>>,
    /// Checker-authoritative ABI-selector facts for erased runtime types.
    /// Currently covers `HashSet` element-type dispatch (`i64`/`u64`/`str`
    /// → `Int64` or `String` ABI variant).
    ///
    /// Passive pass-through: `HashSet` ABI selection lives in MIR/codegen, not
    /// in HIR lowering.  Future consumer: E4 codegen and slice 4.7 spine
    /// widening when `HashSet` operations enter the Rust pipeline.
    /// (LESSONS: checker-authority P0, producer-bridge-before-codegen P1)
    #[expect(
        dead_code,
        reason = "passive pass-through; future consumer is HashSet ABI selection in E4 codegen"
    )]
    lowering_facts: HashMap<SpanKey, LoweringFact>,
    /// Checker-resolved assignment target classification keyed by the target
    /// expression span.
    ///
    /// Passive pass-through: HIR does not yet lower `Stmt::Assign` (it falls
    /// through to `unsupported`).  Future consumer: MIR/codegen compound-
    /// assignment lowering and Machine Lane B actor-field writes.
    /// (LESSONS: checker-authority P0, producer-bridge-before-codegen P1)
    #[expect(
        dead_code,
        reason = "passive pass-through; future consumer is Stmt::Assign lowering in MIR/codegen"
    )]
    assign_target_kinds: HashMap<SpanKey, AssignTargetKind>,
    /// Checker-resolved assignment target type-shape metadata (signedness flag)
    /// keyed by the target expression span.  Populated alongside
    /// `assign_target_kinds` for every accepted assignment.
    ///
    /// Passive pass-through: same consumer timeline as `assign_target_kinds`.
    /// (LESSONS: checker-authority P0, producer-bridge-before-codegen P1)
    #[expect(
        dead_code,
        reason = "passive pass-through; future consumer is compound-assignment signedness in codegen"
    )]
    assign_target_shapes: HashMap<SpanKey, AssignTargetShape>,
    /// Checker-owned actor receive-handler guard policy keyed by receive span.
    actor_handler_state_guards: HashMap<SpanKey, ActorStateGuard>,
    /// Actor type names that participate in reference cycles, computed by the
    /// checker's cycle-detection pass. Consumed by `lower_actor` to populate
    /// `HirActorDecl.cycle_capable`. Future runtime consumer: actor codegen
    /// (refcount-cycle-breaking strategy selection).
    /// (LESSONS: producer-bridge-before-codegen P1)
    cycle_capable_actors: HashSet<String>,
    /// Distinct concrete instantiations of generic top-level user fns,
    /// accumulated as `Expr::Call` lowering walks the program. Drained
    /// into `HirModule.monomorphisations` at the end of `lower_program`.
    /// Cap is configurable via `lower_program_with_mono_cap` for
    /// fail-closed-diagnostic tests.
    mono_registry: MonoRegistry,
    /// Tracks whether the `MonomorphisationCapExceeded` diagnostic has
    /// already been emitted for this lowering invocation, to avoid
    /// spamming one diagnostic per overflowing callsite.
    mono_cap_diag_emitted: bool,
    /// Per-call-site recorded concrete type arguments. Populated by
    /// `record_call_site_type_args` whenever a `HirExprKind::Call` is
    /// produced for a generic top-level user-fn callee. Drained into
    /// `HirModule.call_site_type_args` at end of `lower_program`.
    ///
    /// Includes "still-abstract" entries (where the callee is generic
    /// and the call is itself inside a generic body, so the recorded
    /// args contain the enclosing fn's type-parameter symbols). MIR
    /// lowering substitutes those symbols via the per-monomorphisation
    /// substitution map; the registry's closure-under-substitution pass
    /// uses the same data to discover inner monomorphisations.
    call_site_type_args: HashMap<SiteId, Vec<ResolvedTy>>,
    /// Pre-collected record/type-decl shape registry. Populated in the
    /// type-decl pre-pass before function bodies lower, so that
    /// `Expr::StructInit` lowering can answer "is this a generic user
    /// record?" and look up its source field shape. Tuple-form records
    /// land here with `fields = []`; they never trigger record-layout
    /// emission (their constructor goes through `Expr::Call`).
    record_registry: HashMap<String, RecordEntry>,
    /// Checker-resolved type arguments for generic record-init sites
    /// (`R { ... }` against a `pub type R<T>` or `record R<T>`),
    /// keyed by the struct-init expression span.
    ///
    /// Consumed at `Expr::StructInit` lowering: every init site whose
    /// record has non-empty `type_params` is paired with the entry here
    /// and inserted into `record_layout_registry`. A poisoned entry
    /// (failing the `ResolvedTy::from_ty` boundary conversion) emits
    /// `RecordLayoutTypeArgsViolation`; a generic init site with no
    /// entry at all is skipped (the checker only records sites whose
    /// args resolved fully concretely).
    /// (LESSONS: `checker-authority` P0, `producer-bridge-before-codegen` P1)
    record_init_type_args: HashMap<SpanKey, Vec<Ty>>,
    /// Distinct user-record instantiations observed at `StructInit`
    /// sites, accumulated as expression lowering walks the program.
    /// Drained into `HirModule.record_layouts` at the end of
    /// `lower_program`. Cap is configurable via
    /// `lower_program_with_mono_cap` (shared with the fn registry).
    record_layout_registry: RecordLayoutRegistry,
    /// Tracks whether the `RecordLayoutCapExceeded` diagnostic has
    /// already been emitted for this lowering invocation.
    record_layout_cap_diag_emitted: bool,
}

impl LowerCtx {
    fn new(tc_output: &TypeCheckOutput, mono_cap: usize) -> Self {
        let mut type_classes = crate::value_class::TypeClassTable::default();
        // Seed compiler-known M2 substrate types before source-order TypeDecls.
        // This ensures `ValueClass::of_ty` resolves Duplex/Sink/Stream as
        // AffineResource even though they are not user-declared TypeDecl items.
        seed_builtin_type_classes(&mut type_classes);
        Self {
            ids: IdGen::default(),
            scopes: Vec::new(),
            fn_registry: HashMap::new(),
            type_classes,
            diagnostics: Vec::new(),
            method_call_rewrites: tc_output.method_call_rewrites.clone(),
            dyn_trait_coercions: tc_output.dyn_trait_coercions.clone(),
            dyn_trait_method_calls: tc_output.dyn_trait_method_calls.clone(),
            expr_types: tc_output.expr_types.clone(),
            closure_capture_facts: tc_output.closure_capture_facts.clone(),
            scope_depth: 0,
            statement_position: false,
            current_actor_self: None,
            call_type_args: tc_output.call_type_args.clone(),
            lowering_facts: tc_output.lowering_facts.clone(),
            assign_target_kinds: tc_output.assign_target_kinds.clone(),
            assign_target_shapes: tc_output.assign_target_shapes.clone(),
            actor_handler_state_guards: tc_output.actor_handler_state_guards.clone(),
            cycle_capable_actors: tc_output.cycle_capable_actors.clone(),
            mono_registry: MonoRegistry::with_cap(mono_cap),
            mono_cap_diag_emitted: false,
            call_site_type_args: HashMap::new(),
            record_registry: HashMap::new(),
            record_init_type_args: tc_output.record_init_type_args.clone(),
            record_layout_registry: RecordLayoutRegistry::with_cap(mono_cap),
            record_layout_cap_diag_emitted: false,
        }
    }

    /// Try to record a generic-fn callsite in the monomorphisation
    /// registry. Returns silently for non-generic callees, callees not
    /// in `fn_registry` (builtins/runtime symbols/lambda bindings), and
    /// callsites with no `call_type_args` entry — the latter being the
    /// trivially-monomorphic case from the checker's perspective (an
    /// explicit `<T>` instantiation that already resolved to concrete
    /// types and was not recorded per `calls.rs:183` `record_call_type_args`).
    ///
    /// Emits `MonomorphisationCallTypeArgsViolation` when a recorded
    /// entry fails the `ResolvedTy::from_ty` boundary conversion, and
    /// `MonomorphisationCapExceeded` (at most once per invocation) when
    /// the registry cap is hit.
    /// Recursive check: does this `ResolvedTy` contain a `Named` whose
    /// name matches any type parameter declared on any top-level fn in
    /// this module? If so, the value is "still abstract" — the call
    /// site we're looking at is inside a generic body and the type-arg
    /// has not yet been substituted. Such entries must not enter the
    /// monomorphisation registry; G-1.b's body substitution pass will
    /// re-walk these callsites with substituted args and produce real
    /// entries.
    ///
    /// The check is a conservative over-approximation: a user-declared
    /// type with the same name as a type param (e.g. `pub type T { ... }`)
    /// would also be skipped. This is fine — that's not idiomatic Hew,
    /// and the checker uses `Named` for both cases; the only correct
    /// resolution requires bound-symbol metadata the checker side-table
    /// does not currently expose. Skipping is safe at G-1.a (no false
    /// emissions); a real `T`-named user type still produces the entry
    /// at the outer non-generic callsite where `type_args` is `[]`.
    fn contains_abstract_type_param(&self, ty: &ResolvedTy) -> bool {
        match ty {
            ResolvedTy::Named { name, args } => {
                if self.is_type_param_symbol(name) {
                    return true;
                }
                args.iter().any(|a| self.contains_abstract_type_param(a))
            }
            ResolvedTy::Tuple(items) => items.iter().any(|t| self.contains_abstract_type_param(t)),
            ResolvedTy::Array(elem, _) | ResolvedTy::Slice(elem) => {
                self.contains_abstract_type_param(elem)
            }
            ResolvedTy::Function { params, ret } => {
                params.iter().any(|p| self.contains_abstract_type_param(p))
                    || self.contains_abstract_type_param(ret)
            }
            ResolvedTy::Closure {
                params,
                ret,
                captures,
            } => {
                params.iter().any(|p| self.contains_abstract_type_param(p))
                    || self.contains_abstract_type_param(ret)
                    || captures
                        .iter()
                        .any(|c| self.contains_abstract_type_param(c))
            }
            ResolvedTy::Pointer { pointee, .. } => self.contains_abstract_type_param(pointee),
            ResolvedTy::TraitObject { traits } => traits
                .iter()
                .any(|tb| tb.args.iter().any(|a| self.contains_abstract_type_param(a))),
            ResolvedTy::Task(inner) => self.contains_abstract_type_param(inner),
            _ => false,
        }
    }

    /// Does `name` match any type-parameter declared on any top-level
    /// fn in `fn_registry`? Used to filter "still-abstract" type args
    /// from the monomorphisation registry. See
    /// `contains_abstract_type_param` for the rationale.
    fn is_type_param_symbol(&self, name: &str) -> bool {
        self.fn_registry
            .values()
            .any(|entry| entry.type_params.iter().any(|p| p == name))
    }

    fn record_monomorphisation(
        &mut self,
        callee_expr: &hew_parser::ast::Expr,
        call_span: &std::ops::Range<usize>,
        call_site: SiteId,
    ) {
        let Expr::Identifier(name) = callee_expr else {
            // Only direct-name callees are candidates for top-level-fn
            // monomorphisation. Method calls, indirect calls through
            // bindings, and complex callee expressions are out of scope
            // for G-1.a.
            return;
        };
        let Some(entry) = self.fn_registry.get(name) else {
            // Callee is not a top-level user fn — skip (builtin,
            // runtime-symbol, or unresolved). Filtering on `fn_registry`
            // membership inherently excludes runtime-symbol callees
            // since they have no AST `fn` item and therefore no
            // `fn_registry` insertion (`lower_program` first pass at
            // line 79).
            return;
        };
        if entry.type_params.is_empty() {
            // Non-generic callee — nothing to monomorphise.
            return;
        }
        let origin = entry.id;
        let origin_name = name.clone();
        let key = SpanKey::from(call_span);
        let Some(type_args_raw) = self.call_type_args.get(&key).cloned() else {
            // Generic callee with no recorded type args at this site.
            // The checker records every callsite whose freshened type
            // args were resolved to fully concrete `Ty`s
            // (`calls.rs:78`); the only ways to miss a site are
            // (a) an explicit `<T>` syntax that doesn't re-record
            //     (`calls.rs:183`'s `record_call_type_args` guard), or
            // (b) the call failed to type-check.
            // In (a) the call is trivially monomorphic at the source
            // surface but downstream stages still need a registry
            // entry — that wiring lands in G-1.b once the explicit
            // type-args path is examined end-to-end.
            // (Per plan G-1.a: do not re-infer; treat as out of scope.)
            return;
        };
        let mut type_args: Vec<ResolvedTy> = Vec::with_capacity(type_args_raw.len());
        for ty in &type_args_raw {
            match ResolvedTy::from_ty(ty) {
                Ok(resolved) => type_args.push(resolved),
                Err(err) => {
                    // Fail-closed: poisoned side-table for this call.
                    // Emit a diagnostic; skip the registry entry so the
                    // downstream MIR/LLVM emit doesn't reach an
                    // unresolved type. (LESSONS: checker-output-boundary P0)
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::MonomorphisationCallTypeArgsViolation {
                            callee: origin_name.clone(),
                            reason: err.to_string(),
                        },
                        call_span.clone(),
                        "checker-authoritative call_type_args entry failed boundary conversion",
                    ));
                    return;
                }
            }
        }
        // Record the per-call-site type arguments unconditionally —
        // including "still-abstract" entries where the args reference
        // the enclosing fn's type-parameter symbols. MIR lowering of a
        // specialised body substitutes those symbols via its
        // per-monomorphisation substitution map; the registry's
        // closure-under-substitution pass uses the same data to
        // discover inner monomorphisations.
        self.call_site_type_args
            .insert(call_site, type_args.clone());
        // Skip "still-abstract" entries from the registry — call sites
        // inside a generic body where the type arg is `T` (the
        // surrounding fn's own type-param symbol) rather than a
        // concrete substitution. These appear with
        // `ResolvedTy::Named { name: "T", args: [] }` because the
        // checker treats unbound type params as opaque named types for
        // body-checking purposes. MIR's monomorphisation pass re-walks
        // these sites with concrete args via substitution.
        if type_args
            .iter()
            .any(|t| self.contains_abstract_type_param(t))
        {
            return;
        }
        let mono_key = MonoKey {
            origin,
            origin_name: origin_name.clone(),
            type_args,
        };
        match self.mono_registry.insert(mono_key) {
            Ok(_) => {}
            Err(()) => {
                if !self.mono_cap_diag_emitted {
                    self.mono_cap_diag_emitted = true;
                    let cap = self.mono_registry.cap();
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::MonomorphisationCapExceeded { cap },
                        call_span.clone(),
                        "too many distinct generic-function instantiations; the \
                         compiler refuses to monomorphise beyond the configured \
                         cap (suspect polymorphic recursion or runaway inference)",
                    ));
                }
            }
        }
    }

    /// Try to record a generic record-init site in the record-layout
    /// registry. Mirrors `record_monomorphisation` for the
    /// record-type surface.
    ///
    /// Skips silently when:
    /// - the record is not in `record_registry` (builtin Vec/Option/
    ///   Result/HashMap/Range/channel-handle types are never registered
    ///   because they have no `Item::TypeDecl`/`Item::Record` AST node),
    /// - the record has empty `type_params` (monomorphic record — no
    ///   per-instantiation layout needed; the bare-name layout in MIR
    ///   suffices),
    /// - no `record_init_type_args` entry exists for the site (the
    ///   checker filters out sites whose args still contain inference
    ///   vars; a missing entry means the init is trivially monomorphic
    ///   at the checker's seam).
    ///
    /// Emits `RecordLayoutTypeArgsViolation` when a recorded entry
    /// fails the `ResolvedTy::from_ty` boundary conversion;
    /// `RecursiveGenericTypeUnsupported` when substituted fields name
    /// the same origin record with different concrete args;
    /// `RecordLayoutCapExceeded` (at most once per invocation) when the
    /// registry cap is hit.
    fn record_record_layout(
        &mut self,
        record_name: &str,
        init_span: &std::ops::Range<usize>,
    ) -> Option<Vec<ResolvedTy>> {
        let Some(entry) = self.record_registry.get(record_name) else {
            // Builtin or unresolved record — not a user-declared
            // generic type, so it has no record-layout registry entry.
            return None;
        };
        if entry.type_params.is_empty() {
            // Monomorphic record — bare-name layout (handled by MIR's
            // existing record-decl emission) is sufficient.
            return None;
        }
        let origin = entry.id;
        let origin_name = record_name.to_string();
        let type_params = entry.type_params.clone();
        let source_fields = entry.fields.clone();

        let key = SpanKey::from(init_span);
        let Some(type_args_raw) = self.record_init_type_args.get(&key).cloned() else {
            // Generic record-init with no recorded type args at this
            // site. Per the checker output contract this can only
            // happen when the site failed type-checking (and was
            // pruned at the boundary) or when an explicit type-arg
            // syntax took a path that doesn't re-record. Either way
            // there's nothing to monomorphise here.
            return None;
        };

        // Boundary conversion: any leaked inference var, error, or
        // unmaterialised literal is fail-closed per checker-authority.
        let mut type_args: Vec<ResolvedTy> = Vec::with_capacity(type_args_raw.len());
        for ty in &type_args_raw {
            match ResolvedTy::from_ty(ty) {
                Ok(resolved) => type_args.push(resolved),
                Err(err) => {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::RecordLayoutTypeArgsViolation {
                            record: origin_name.clone(),
                            reason: err.to_string(),
                        },
                        init_span.clone(),
                        "checker-authoritative record_init_type_args entry failed boundary conversion",
                    ));
                    return None;
                }
            }
        }

        // Arity guard: the checker enforces matching arity, but be
        // defensive — substitution panics on a mismatch.
        if type_args.len() != type_params.len() {
            return None;
        }

        // Substitute the type-params with the concrete args to produce
        // this layout's field shape.
        let substituted_fields: Vec<(String, ResolvedTy)> = source_fields
            .iter()
            .map(|(fname, fty)| {
                (
                    fname.clone(),
                    substitute_type_params(fty, &type_params, &type_args),
                )
            })
            .collect();

        // Recursive polymorphic-self detection: a substituted field
        // type that names `origin_name` with *different* concrete args
        // than `type_args` is unbounded (each layer demands another
        // distinct layout). Same-arg self-reference is fine — that's
        // an ordinary recursive shape and converges to one layout.
        for (_, fty) in &substituted_fields {
            if contains_recursive_polymorphic_self(fty, &origin_name, &type_args) {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::RecursiveGenericTypeUnsupported {
                        name: origin_name.clone(),
                    },
                    init_span.clone(),
                    "recursive generic type definition: a field substitutes to \
                     a reference to the same record with different concrete \
                     type arguments, which would force unbounded layout \
                     expansion (deferred to v0.6)",
                ));
                return None;
            }
        }

        let returned_type_args = type_args.clone();
        let key = RecordMonoKey {
            origin,
            origin_name,
            type_args,
        };
        if self
            .record_layout_registry
            .insert(key, substituted_fields, init_span.clone())
            .is_err()
        {
            if !self.record_layout_cap_diag_emitted {
                self.record_layout_cap_diag_emitted = true;
                let cap = self.record_layout_registry.cap();
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::RecordLayoutCapExceeded { cap },
                    init_span.clone(),
                    "too many distinct generic-record instantiations; the \
                     compiler refuses to monomorphise beyond the configured \
                     cap (suspect polymorphic recursion or runaway inference)",
                ));
            }
            return None;
        }
        Some(returned_type_args)
    }
}

impl LowerCtx {
    fn register_fn_entry(&mut self, name: &str, func: &FnDecl) {
        let id = self.ids.item();
        let return_ty = func
            .return_type
            .as_ref()
            .map_or(ResolvedTy::Unit, |ty| self.lower_type(ty));
        let param_tys = func.params.iter().map(|p| self.lower_type(&p.ty)).collect();
        let type_params = func
            .type_params
            .as_ref()
            .map(|params| params.iter().map(|p| p.name.clone()).collect())
            .unwrap_or_default();
        self.fn_registry.insert(
            name.to_string(),
            FnEntry {
                id,
                return_ty,
                param_tys,
                type_params,
            },
        );
    }

    fn lower_fn(&mut self, func: &FnDecl, span: std::ops::Range<usize>) -> HirFn {
        self.lower_fn_with_name(func, &func.name, span)
    }

    fn lower_fn_with_name(
        &mut self,
        func: &FnDecl,
        name: &str,
        span: std::ops::Range<usize>,
    ) -> HirFn {
        // Use the stable ItemId pre-allocated during the first pass.
        let id = self
            .fn_registry
            .get(name)
            .map_or_else(|| self.ids.item(), |entry| entry.id);

        self.push_scope();
        let mut params = Vec::new();
        for param in &func.params {
            let ty = self.lower_type(&param.ty);
            let binding = self.bind(param.name.clone(), ty, param.is_mutable, param.ty.1.clone());
            params.push(binding);
        }

        let return_ty = func
            .return_type
            .as_ref()
            .map_or(ResolvedTy::Unit, |ty| self.lower_type(ty));
        let body = self.lower_block(&func.body, &return_ty);
        self.pop_scope();

        HirFn {
            id,
            node: self.ids.node(),
            name: name.to_string(),
            type_params: func
                .type_params
                .as_ref()
                .map(|params| params.iter().map(|param| param.name.clone()).collect())
                .unwrap_or_default(),
            params,
            return_ty,
            body,
            span,
        }
    }

    fn lower_type_decl(&mut self, decl: &TypeDecl, span: std::ops::Range<usize>) -> HirTypeDecl {
        // Generic resource/linear types are rejected — the type→class map is
        // keyed by name, not by instantiation. This rule belongs at the
        // checker boundary (LESSONS `checker-output-boundary`); HIR is the
        // first place the marker is durable, so the check lands here.
        if decl.resource_marker != ResourceMarker::None && decl.type_params.is_some() {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::ResourceGenericUnsupported {
                    name: decl.name.clone(),
                },
                span.clone(),
                "`#[resource]` / `#[linear]` types cannot have type parameters in v0.5",
            ));
        }

        match decl.resource_marker {
            ResourceMarker::Resource => {
                // `#[resource]` must declare `close(consuming self)`.
                let has_close = decl
                    .consuming_methods
                    .iter()
                    .any(|name| name.as_str() == "close");
                if !has_close {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::ResourceMissingClose {
                            name: decl.name.clone(),
                        },
                        span.clone(),
                        "`#[resource]` type must declare `fn close(consuming self) -> ...` \
                         in its body; the implicit drop contract dispatches to this method",
                    ));
                }
            }
            ResourceMarker::Linear => {
                // `#[linear]` must declare at least one consuming method.
                if decl.consuming_methods.is_empty() {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::LinearNoConsumingMethods {
                            name: decl.name.clone(),
                        },
                        span.clone(),
                        "`#[linear]` type must declare at least one `consuming self` method; \
                         without one no exit path could exhaust a binding of this type",
                    ));
                }
            }
            ResourceMarker::None => {}
        }

        // Carry the field set so dump-hir and future analysis have something
        // to reason about; methods are out of scope for v0.5 MIR lowering
        // and stay on the parser-side `TypeBodyItem::Method`.
        let mut fields = Vec::new();
        for item in &decl.body {
            if let TypeBodyItem::Field {
                name,
                ty,
                span: field_span,
                ..
            } = item
            {
                fields.push(HirField {
                    name: name.clone(),
                    ty: self.lower_type(ty),
                    span: field_span.clone(),
                });
            }
            // `TypeBodyItem::Method` is not lowered into HIR in v0.5 — the
            // method-call expression form has no HIR/MIR lowering yet, so
            // method bodies cannot be exercised. Their *declared names* are
            // captured upstream as `TypeDecl.consuming_methods` and travel
            // on `HirTypeDecl.consuming_methods`. `TypeBodyItem::Variant`
            // belongs to enum bodies and is similarly out of slice scope.
        }

        // Reuse the stable ItemId pre-allocated during the record/
        // type-decl pre-pass. Falling back to a fresh id keeps
        // the path safe if the pre-pass ever skips a decl.
        let id = self
            .record_registry
            .get(&decl.name)
            .map_or_else(|| self.ids.item(), |entry| entry.id);
        let type_params: Vec<String> = decl
            .type_params
            .as_ref()
            .map_or(vec![], |ps| ps.iter().map(|p| p.name.clone()).collect());
        HirTypeDecl {
            id,
            node: self.ids.node(),
            name: decl.name.clone(),
            marker: decl.resource_marker,
            consuming_methods: decl.consuming_methods.clone(),
            type_params,
            fields,
            span,
        }
    }

    /// Lower a `record` declaration into `HirRecordDecl`.
    ///
    /// Only named-form records produce a field list; tuple-form records have an
    /// empty field list (`HirRecordDecl.fields` is empty) because their
    /// constructor is a `Call` (`R(a, b)`) rather than a `StructInit`
    /// (`R { x: a, y: b }`). Tuple records are never looked up in the
    /// `record_field_orders` table — the MIR producer only handles named-form.
    fn lower_record_decl(
        &mut self,
        decl: &RecordDecl,
        span: std::ops::Range<usize>,
    ) -> HirRecordDecl {
        let type_params: Vec<String> = decl.type_params.as_ref().map_or(vec![], |params| {
            params.iter().map(|p| p.name.clone()).collect()
        });

        let fields: Vec<HirField> = match &decl.kind {
            RecordKind::Named(record_fields) => record_fields
                .iter()
                .map(|rf| HirField {
                    name: rf.name.clone(),
                    ty: self.lower_type(&rf.ty),
                    span: rf.span.clone(),
                })
                .collect(),
            // Tuple records have no named fields; their constructor fn handles
            // positional argument binding.
            RecordKind::Tuple(_) => vec![],
        };

        // Reuse the stable ItemId pre-allocated during the record/
        // type-decl pre-pass. Falling back to a fresh id keeps
        // the path safe if the pre-pass ever skips a decl.
        let id = self
            .record_registry
            .get(&decl.name)
            .map_or_else(|| self.ids.item(), |entry| entry.id);
        HirRecordDecl {
            id,
            node: self.ids.node(),
            name: decl.name.clone(),
            type_params,
            fields,
            span,
        }
    }

    /// Lower a `supervisor` declaration to `HirSupervisorDecl`.
    ///
    /// Bodies (MIR producer wiring, codegen) are deferred to slices S-C/S-D.
    /// This function mirrors `lower_record_decl` in structure: structural lift
    /// only, no HIR expression lowering.
    fn lower_supervisor(&mut self, decl: &SupervisorDecl, span: Span) -> HirSupervisorDecl {
        let strategy = decl.strategy.map(|s| match s {
            SupervisorStrategy::OneForOne => HirSupervisorStrategy::OneForOne,
            SupervisorStrategy::OneForAll => HirSupervisorStrategy::OneForAll,
            SupervisorStrategy::RestForOne => HirSupervisorStrategy::RestForOne,
            SupervisorStrategy::SimpleOneForOne => HirSupervisorStrategy::SimpleOneForOne,
        });

        // Assign slot indices by partitioning children into static and pool spaces.
        // Each partition uses its own 0-based counter so the indices are disjoint,
        // matching the runtime layout (children[] for static, pool_slots[] for pool).
        let mut static_slot = 0u32;
        let mut pool_slot = 0u32;
        let children = decl
            .children
            .iter()
            .map(|child| {
                let slot_index = if child.is_pool {
                    let idx = pool_slot;
                    pool_slot += 1;
                    idx
                } else {
                    let idx = static_slot;
                    static_slot += 1;
                    idx
                };
                HirSupervisorChild {
                    name: child.name.clone(),
                    ty: child.actor_type.clone(),
                    restart_policy: child.restart.map(|r| match r {
                        RestartPolicy::Permanent => HirRestartPolicy::Permanent,
                        RestartPolicy::Transient => HirRestartPolicy::Transient,
                        RestartPolicy::Temporary => HirRestartPolicy::Temporary,
                    }),
                    wired_to: child.wired_to.clone(),
                    is_pool: child.is_pool,
                    slot_index,
                }
            })
            .collect();

        HirSupervisorDecl {
            id: self.ids.item(),
            node: self.ids.node(),
            name: decl.name.clone(),
            strategy,
            max_restarts: decl.max_restarts,
            window: decl.window.clone(),
            children,
            span,
        }
    }

    #[expect(
        clippy::too_many_lines,
        reason = "machine lowering has three distinct phases (structure, checks, assembly) \
                  that read more clearly as a single function than as multiple helpers"
    )]
    fn lower_machine(
        &mut self,
        decl: &MachineDecl,
        span: std::ops::Range<usize>,
    ) -> Option<HirMachineDecl> {
        // Lower states.
        let mut hir_states = Vec::new();
        for state in &decl.states {
            let fields: Vec<HirField> = state
                .fields
                .iter()
                .map(|(name, ty)| HirField {
                    name: name.clone(),
                    ty: self.lower_type(ty),
                    span: ty.1.clone(),
                })
                .collect();

            // Shallow-scan the entry and exit blocks for field-assignment targets.
            // Lane A does not fully lower block bodies; this is enough for
            // effect-parity checking.
            let entry_writes = state
                .entry
                .as_ref()
                .map(collect_assigned_field_names)
                .unwrap_or_default();
            let exit_writes = state
                .exit
                .as_ref()
                .map(collect_assigned_field_names)
                .unwrap_or_default();

            hir_states.push(HirMachineState {
                name: state.name.clone(),
                fields,
                has_entry: state.entry.is_some(),
                has_exit: state.exit.is_some(),
                entry_writes,
                exit_writes,
                span: span.clone(),
            });
        }

        // Lower events.
        let hir_events: Vec<HirMachineEvent> = decl
            .events
            .iter()
            .map(|ev| {
                let fields = ev
                    .fields
                    .iter()
                    .map(|(name, ty)| HirField {
                        name: name.clone(),
                        ty: self.lower_type(ty),
                        span: ty.1.clone(),
                    })
                    .collect();
                HirMachineEvent {
                    name: ev.name.clone(),
                    fields,
                    span: span.clone(),
                }
            })
            .collect();

        // Lower transitions — shallow: record names, guard presence, body writes,
        // and emitted event names (for static checks). Body expressions are not
        // lowered to HirExpr in Lane A.
        let hir_transitions: Vec<HirMachineTransition> = decl
            .transitions
            .iter()
            .map(|tr| {
                let is_self_transition =
                    tr.source_state == tr.target_state && tr.source_state != "_";
                let body_writes = collect_assigned_field_names_expr(&tr.body.0);
                let body_emits = collect_emitted_events(&tr.body.0);
                HirMachineTransition {
                    event_name: tr.event_name.clone(),
                    source_state: tr.source_state.clone(),
                    target_state: tr.target_state.clone(),
                    has_guard: tr.guard.is_some(),
                    is_self_transition,
                    reenter: tr.reenter,
                    body_writes,
                    body_emits,
                    span: tr.body.1.clone(),
                }
            })
            .collect();

        // ── Static checks ────────────────────────────────────────────────────

        // 1. Exhaustiveness: every concrete (state, event) pair must have a
        //    transition, or a `default` arm must exist, or a wildcard source `_`
        //    covers it.
        if !decl.has_default {
            let mut missing: Vec<(String, String)> = Vec::new();
            for state in &decl.states {
                for event in &decl.events {
                    let covered = decl.transitions.iter().any(|tr| {
                        tr.event_name == event.name
                            && (tr.source_state == state.name || tr.source_state == "_")
                    });
                    if !covered {
                        missing.push((state.name.clone(), event.name.clone()));
                    }
                }
            }
            if !missing.is_empty() {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::MachineExhaustivenessViolation {
                        machine_name: decl.name.clone(),
                        missing,
                    },
                    span.clone(),
                    format!(
                        "machine `{}` does not handle all (state, event) pairs; \
                         add the missing transitions or a `default` arm",
                        decl.name
                    ),
                ));
                return None;
            }
        }

        // 2. Self-transition @reenter rule: a non-empty self-loop body without
        //    @reenter is a compile error. Empty body OR @reenter are both OK.
        //    "Empty" means the body resolves to `Expr::Identifier(target_state)`
        //    (the no-body semicolon shorthand) or an `Expr::Block` with no stmts
        //    and no trailing expression.
        for tr in decl.transitions.iter().zip(hir_transitions.iter()) {
            let (ast_tr, hir_tr) = tr;
            if !hir_tr.is_self_transition || hir_tr.reenter {
                continue;
            }
            let body_is_empty = is_empty_self_body(&ast_tr.body.0, &hir_tr.target_state);
            if !body_is_empty {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::MachineSelfTransitionNeedsReenter {
                        machine_name: decl.name.clone(),
                        state_name: hir_tr.source_state.clone(),
                        event_name: hir_tr.event_name.clone(),
                    },
                    hir_tr.span.clone(),
                    format!(
                        "self-transition `on {event}` in state `{state}` has a non-empty body \
                         but is not annotated `@reenter`; annotate with `@reenter` to opt in to \
                         Mealy re-entry semantics, or remove the body",
                        event = hir_tr.event_name,
                        state = hir_tr.source_state,
                    ),
                ));
            }
        }

        // 3. Effect-parity: a transition body that writes a field also written
        //    by the *target* state's `entry` block or the *source* state's `exit`
        //    block creates ambiguous initialization/teardown order.
        for tr in &hir_transitions {
            if tr.body_writes.is_empty() {
                continue;
            }
            // Check target entry conflict.
            if let Some(target) = hir_states.iter().find(|s| s.name == tr.target_state) {
                for field in &tr.body_writes {
                    if let Some((_, entry_assign_span)) =
                        target.entry_writes.iter().find(|(n, _)| n == field)
                    {
                        self.diagnostics.push(
                            HirDiagnostic::new(
                                HirDiagnosticKind::MachineEffectParityViolation {
                                    machine_name: decl.name.clone(),
                                    state_name: tr.target_state.clone(),
                                    field_name: field.clone(),
                                    transition_event: tr.event_name.clone(),
                                    is_entry_conflict: true,
                                },
                                tr.span.clone(),
                                format!(
                                    "transition `on {}` body and state `{}` entry block both \
                                     write field `{}`; remove the write from one site",
                                    tr.event_name, tr.target_state, field
                                ),
                            )
                            .with_secondary_spans(vec![(
                                entry_assign_span.clone(),
                                format!(
                                    "state `{}` entry block assigns `{}` here",
                                    tr.target_state, field
                                ),
                            )]),
                        );
                    }
                }
            }
            // Check source exit conflict.
            if let Some(source) = hir_states.iter().find(|s| s.name == tr.source_state) {
                for field in &tr.body_writes {
                    if let Some((_, exit_assign_span)) =
                        source.exit_writes.iter().find(|(n, _)| n == field)
                    {
                        self.diagnostics.push(
                            HirDiagnostic::new(
                                HirDiagnosticKind::MachineEffectParityViolation {
                                    machine_name: decl.name.clone(),
                                    state_name: tr.source_state.clone(),
                                    field_name: field.clone(),
                                    transition_event: tr.event_name.clone(),
                                    is_entry_conflict: false,
                                },
                                tr.span.clone(),
                                format!(
                                    "transition `on {}` body and state `{}` exit block both \
                                     write field `{}`; remove the write from one site",
                                    tr.event_name, tr.source_state, field
                                ),
                            )
                            .with_secondary_spans(vec![(
                                exit_assign_span.clone(),
                                format!(
                                    "state `{}` exit block assigns `{}` here",
                                    tr.source_state, field
                                ),
                            )]),
                        );
                    }
                }
            }
        }

        // 4. Emit-cycle: `on E` transition that directly emits `E` would
        //    immediately re-trigger its own handler.
        for tr in &hir_transitions {
            if tr.body_emits.contains(&tr.event_name) {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::MachineEmitCycle {
                        machine_name: decl.name.clone(),
                        event_name: tr.event_name.clone(),
                    },
                    tr.span.clone(),
                    format!(
                        "transition `on {}` emits `{}` which immediately re-triggers itself; \
                         rename the emitted event or remove the emit",
                        tr.event_name, tr.event_name
                    ),
                ));
            }
        }

        // Fail-closed: if any diagnostic was pushed for this machine, abort.
        // (Effect-parity, self-transition, and emit-cycle diagnostics are not
        // return-None by themselves but we abort to avoid a partially-valid
        // machine in HIR.)
        let has_machine_errors = self.diagnostics.iter().any(|d| {
            matches!(
                &d.kind,
                HirDiagnosticKind::MachineSelfTransitionNeedsReenter { machine_name, .. }
                | HirDiagnosticKind::MachineEffectParityViolation { machine_name, .. }
                | HirDiagnosticKind::MachineEmitCycle { machine_name, .. }
                if machine_name == &decl.name
            )
        });
        if has_machine_errors {
            return None;
        }

        Some(HirMachineDecl {
            id: self.ids.item(),
            node: self.ids.node(),
            name: decl.name.clone(),
            states: hir_states,
            events: hir_events,
            transitions: hir_transitions,
            has_default: decl.has_default,
            span,
        })
    }

    /// Lower an `actor` declaration into `HirActorDecl`, including executable
    /// bodies for init blocks, receive handlers, methods, and lifecycle hooks.
    fn lower_actor(&mut self, decl: &ActorDecl, span: Span) -> HirActorDecl {
        let state_fields: Vec<HirField> = decl
            .fields
            .iter()
            .map(|f| HirField {
                name: f.name.clone(),
                ty: self.lower_type(&f.ty),
                span: f.ty.1.clone(),
            })
            .collect();

        let init = decl.init.as_ref().map(|init| {
            let (params, body) = self.lower_actor_body(&init.params, &init.body, &ResolvedTy::Unit);
            HirActorInit { params, body }
        });

        let receive_handlers: Vec<HirActorReceiveFn> = decl
            .receive_fns
            .iter()
            .map(|rf| self.lower_actor_receive_fn(rf))
            .collect();

        let (methods, lifecycle_hooks) = self.partition_actor_methods(&decl.methods);

        HirActorDecl {
            id: self.ids.item(),
            node: self.ids.node(),
            name: decl.name.clone(),
            state_fields,
            init,
            receive_handlers,
            methods,
            lifecycle_hooks,
            max_heap_bytes: decl.max_heap_bytes,
            is_isolated: decl.is_isolated,
            mailbox_capacity: decl.mailbox_capacity,
            overflow_policy: decl.overflow_policy.clone(),
            cycle_capable: self.cycle_capable_actors.contains(&decl.name),
            span,
        }
    }

    fn lower_actor_body(
        &mut self,
        params: &[Param],
        body: &Block,
        expected_ty: &ResolvedTy,
    ) -> (Vec<HirBinding>, HirBlock) {
        let saved_scope_depth = self.scope_depth;
        self.scope_depth = 0;
        self.push_scope();
        let params = params
            .iter()
            .map(|p| {
                let ty = self.lower_type(&p.ty);
                self.bind(p.name.clone(), ty, p.is_mutable, p.ty.1.clone())
            })
            .collect();
        let body = self.lower_block(body, expected_ty);
        self.pop_scope();
        self.scope_depth = saved_scope_depth;
        (params, body)
    }

    fn lower_actor_receive_fn(&mut self, rf: &ReceiveFnDecl) -> HirActorReceiveFn {
        let return_ty = rf
            .return_type
            .as_ref()
            .map_or(ResolvedTy::Unit, |ty| self.lower_type(ty));
        let body_expected_ty = if rf.is_generator {
            ResolvedTy::Unit
        } else {
            return_ty.clone()
        };
        let (params, body) = self.lower_actor_body(&rf.params, &rf.body, &body_expected_ty);
        let state_guard = match self
            .actor_handler_state_guards
            .get(&SpanKey::from(&rf.span))
            .copied()
        {
            Some(ActorStateGuard::Exclusive) => HirActorStateGuard::Exclusive,
            None => {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::ActorStateGuardMissing {
                        handler: rf.name.clone(),
                    },
                    rf.span.clone(),
                    "missing checker-owned actor-state guard fact",
                ));
                HirActorStateGuard::Exclusive
            }
        };
        let every_ns = rf
            .attributes
            .iter()
            .find(|a| a.name == "every")
            .and_then(|a| a.args.first())
            .and_then(AttributeArg::as_duration_ns);
        HirActorReceiveFn {
            name: rf.name.clone(),
            is_generator: rf.is_generator,
            params,
            return_ty,
            body,
            state_guard,
            every_ns,
            span: rf.span.clone(),
        }
    }

    /// Partition an actor's `methods` vec into plain methods and lifecycle
    /// hooks (`#[on(start|stop|crash|upgrade)]`). The checker has already
    /// validated hook-kind spellings and uniqueness; HIR consumes the
    /// post-validation shape and silently ignores methods whose `#[on(...)]`
    /// attribute is malformed (the checker has emitted a diagnostic and the
    /// HIR consumer should not see the entry as a lifecycle hook).
    fn partition_actor_methods(
        &mut self,
        methods: &[FnDecl],
    ) -> (Vec<HirActorMethod>, Vec<HirLifecycleHook>) {
        let mut plain = Vec::new();
        let mut hooks = Vec::new();
        for method in methods {
            let return_ty = method
                .return_type
                .as_ref()
                .map_or(ResolvedTy::Unit, |ty| self.lower_type(ty));
            let (params, body) = self.lower_actor_body(&method.params, &method.body, &return_ty);
            let hook_attr = method.attributes.iter().find(|a| a.name == "on");
            let hook_kind = hook_attr
                .and_then(|a| a.args.first())
                .map(AttributeArg::as_str)
                .and_then(|k| match k {
                    "start" => Some(HirLifecycleHookKind::Start),
                    "stop" => Some(HirLifecycleHookKind::Stop),
                    "crash" => Some(HirLifecycleHookKind::Crash),
                    "upgrade" => Some(HirLifecycleHookKind::Upgrade),
                    _ => None,
                });
            match hook_kind {
                Some(kind) => hooks.push(HirLifecycleHook {
                    kind,
                    name: method.name.clone(),
                    params,
                    return_ty,
                    body,
                    span: method.fn_span.clone(),
                }),
                None => plain.push(HirActorMethod {
                    name: method.name.clone(),
                    params,
                    return_ty,
                    body,
                    span: method.fn_span.clone(),
                }),
            }
        }
        (plain, hooks)
    }

    fn lower_block(&mut self, block: &Block, expected_ty: &ResolvedTy) -> HirBlock {
        self.push_scope();
        let scope = self.ids.scope();
        let mut statements = Vec::new();
        for (stmt, span) in &block.stmts {
            statements.extend(self.lower_stmt_multi(stmt, span.clone(), expected_ty.clone()));
        }
        let tail = block
            .trailing_expr
            .as_ref()
            .map(|expr| Box::new(self.lower_expr(expr, IntentKind::Read)));
        let ty = tail
            .as_ref()
            .map_or(ResolvedTy::Unit, |expr| expr.ty.clone());
        self.pop_scope();

        HirBlock {
            node: self.ids.node(),
            scope,
            statements,
            tail,
            ty,
            span: 0..0,
        }
    }

    /// Lower a statement, returning zero or more `HirStmt`s.
    ///
    /// Most statements produce exactly one `HirStmt` (delegated to `lower_stmt`).
    /// `let (a, b) = expr;` (Q33 tuple-let) produces:
    ///   1. `let __tuple_N = expr;`    — binds the tuple value to a synthetic temp
    ///   2. `let a = __tuple_N.0;`     — per-element projection (as many as elements)
    ///   3. `let b = __tuple_N.1;`
    ///
    /// The element projections use a synthetic `HirExprKind::TupleIndex` node.
    /// Downstream MIR lowering handles tuple projections in the `Expr::Call` return
    /// path for `duplex_pair`.
    #[expect(
        clippy::too_many_lines,
        reason = "tuple-let expansion has three phases (validation, temp-bind, per-element loop) \
                  that read more clearly as a single function; splitting would obscure the \
                  invariant that temp-bind and per-element refs share the same temp_name"
    )]
    fn lower_stmt_multi(
        &mut self,
        stmt: &Stmt,
        span: std::ops::Range<usize>,
        return_ty: ResolvedTy,
    ) -> Vec<HirStmt> {
        // Tuple-let: `let (a, b, ...) = value_expr;`
        if let Stmt::Let {
            pattern,
            ty: annotation,
            value: Some(value_expr),
        } = stmt
        {
            if let Pattern::Tuple(element_patterns) = &pattern.0 {
                // Lower the tuple value once into a synthetic temp binding.
                let tuple_val = self.lower_expr(value_expr, IntentKind::Consume);
                let tuple_ty = tuple_val.ty.clone();

                // Validate the pattern element count against the inferred tuple type.
                let element_tys: Vec<ResolvedTy> = if let ResolvedTy::Tuple(elems) = &tuple_ty {
                    if elems.len() != element_patterns.len() {
                        self.diagnostics.push(HirDiagnostic::new(
                            HirDiagnosticKind::CutoverUnsupported {
                                construct: format!(
                                    "tuple pattern with {} elements for tuple with {} elements",
                                    element_patterns.len(),
                                    elems.len()
                                ),
                                slice_target: "type-checker".to_string(),
                            },
                            span.clone(),
                            "tuple pattern element count does not match tuple value arity",
                        ));
                        return vec![HirStmt {
                            node: self.ids.node(),
                            kind: HirStmtKind::Expr(
                                self.unsupported_expr(span, "tuple arity mismatch"),
                            ),
                            span: 0..0,
                        }];
                    }
                    elems.clone()
                } else if annotation.is_none() {
                    // Type not yet resolved — use Unit for each element (diagnostic
                    // already emitted by the checker; HIR does best-effort lowering).
                    vec![ResolvedTy::Unit; element_patterns.len()]
                } else {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::CutoverUnsupported {
                            construct: "tuple pattern on non-tuple value".to_string(),
                            slice_target: "type-checker".to_string(),
                        },
                        span.clone(),
                        "tuple-let pattern requires the right-hand side to have a tuple type",
                    ));
                    return vec![HirStmt {
                        node: self.ids.node(),
                        kind: HirStmtKind::Expr(
                            self.unsupported_expr(span, "tuple pattern on non-tuple"),
                        ),
                        span: 0..0,
                    }];
                };

                // Synthetic temp name — unlikely to collide with user identifiers.
                let temp_name = format!("__tuple_{}", self.ids.binding().0);
                let temp_binding =
                    self.bind(temp_name.clone(), tuple_ty.clone(), false, span.clone());
                // Capture the binding id before `temp_binding` is moved into
                // the `HirStmt` below.  MIR lowering's `TupleIndex` arm needs
                // `ResolvedRef::Binding(temp_id)` to look up the proxy Place in
                // `binding_locals`; `Unresolved` would return `None` and break
                // the `tuple_decomp` lookup for non-BitCopy element types.
                let temp_id = temp_binding.id;
                let temp_stmt = HirStmt {
                    node: self.ids.node(),
                    kind: HirStmtKind::Let(temp_binding, Some(tuple_val)),
                    span: span.clone(),
                };

                let mut stmts = vec![temp_stmt];

                // Per-element projection lets.
                for (idx, (elem_pat, elem_ty)) in
                    element_patterns.iter().zip(element_tys).enumerate()
                {
                    let elem_name = match &elem_pat.0 {
                        Pattern::Identifier(n) => n.clone(),
                        Pattern::Wildcard => format!("_{idx}"),
                        _ => {
                            // Nested tuple / constructor patterns are out of scope.
                            self.diagnostics.push(HirDiagnostic::new(
                                HirDiagnosticKind::CutoverUnsupported {
                                    construct: "nested pattern in tuple-let".to_string(),
                                    slice_target: "pattern-matching".to_string(),
                                },
                                elem_pat.1.clone(),
                                "only identifier and wildcard patterns are supported \
                                 inside tuple-let in v0.5",
                            ));
                            format!("__unsupported_{idx}")
                        }
                    };

                    // Build a TupleIndex expression: `__tuple_N.<idx>`.
                    // Use `ResolvedRef::Binding(temp_id)` so MIR can resolve
                    // the proxy local from `binding_locals` and recover the
                    // per-element `Place` via `tuple_decomp`.
                    let temp_ref = HirExpr {
                        node: self.ids.node(),
                        site: self.ids.site(),
                        value_class: ValueClass::of_ty(&tuple_ty, &self.type_classes),
                        ty: tuple_ty.clone(),
                        intent: IntentKind::Read,
                        kind: HirExprKind::BindingRef {
                            name: temp_name.clone(),
                            resolved: ResolvedRef::Binding(temp_id),
                        },
                        span: span.clone(),
                    };
                    let projection = HirExpr {
                        node: self.ids.node(),
                        site: self.ids.site(),
                        value_class: ValueClass::of_ty(&elem_ty, &self.type_classes),
                        ty: elem_ty.clone(),
                        intent: IntentKind::Read,
                        kind: HirExprKind::TupleIndex {
                            tuple: Box::new(temp_ref),
                            index: idx,
                        },
                        span: elem_pat.1.clone(),
                    };

                    let elem_binding = self.bind(elem_name, elem_ty, false, elem_pat.1.clone());
                    stmts.push(HirStmt {
                        node: self.ids.node(),
                        kind: HirStmtKind::Let(elem_binding, Some(projection)),
                        span: elem_pat.1.clone(),
                    });
                }

                return stmts;
            }
        }

        // Non-tuple statements: delegate to the single-statement path.
        vec![self.lower_stmt(stmt, span, return_ty)]
    }

    #[allow(
        clippy::too_many_lines,
        reason = "single large match on stmt variants; splitting would hurt readability"
    )]
    fn lower_stmt(
        &mut self,
        stmt: &Stmt,
        span: std::ops::Range<usize>,
        return_ty: ResolvedTy,
    ) -> HirStmt {
        let kind = match stmt {
            Stmt::Let { pattern, ty, value } => {
                // `await expr` is only legal as a statement-expression inside a
                // scope{} body (TI-4). Using it as a let-value is always rejected —
                // the await result is consumed immediately and has no place to bind.
                if let Some(val_expr) = value {
                    if matches!(&val_expr.0, Expr::Await(_)) {
                        self.diagnostics.push(HirDiagnostic::new(
                            HirDiagnosticKind::AwaitOutOfPosition,
                            val_expr.1.clone(),
                            "`await` cannot be used as a let-value; \
                             it is only legal as a statement-expression inside a `scope{}` body",
                        ));
                        let name = self
                            .pattern_name(pattern)
                            .unwrap_or_else(|| "_".to_string());
                        let binding_ty = ty
                            .as_ref()
                            .map_or(ResolvedTy::Unit, |ty| self.lower_type(ty));
                        let binding = self.bind(name, binding_ty, false, pattern.1.clone());
                        let unsupported =
                            self.unsupported_expr(val_expr.1.clone(), "`await` in let-value");
                        return HirStmt {
                            node: self.ids.node(),
                            kind: HirStmtKind::Let(binding, Some(unsupported)),
                            span,
                        };
                    }
                }
                // Forward-bind for actor-lambda RHS. When the value is
                // `actor |params| { body }` and the let-pattern is a bare
                // identifier, the body may reference its own let-name for
                // recursive self-dispatch (HEW-SPEC §5.9 ratification 2).
                // Pre-bind the name BEFORE lowering the body so the body's
                // identifier reference resolves to a
                // `ResolvedRef::Binding(let_id)` rather than `Unresolved`.
                // The capture-strength classifier in `lower_expr`'s
                // `Expr::SpawnLambdaActor` arm checks the resolved id
                // against this let's id to discriminate Weak (self) from
                // Strong (every other free-variable capture).
                //
                // The pre-bind fires for both untyped and typed lets. The
                // binding type follows the annotation when present (the
                // type-checker layer reconciles the annotation against the
                // lambda's synthesised Duplex shape) and falls back to the
                // synthetic `Duplex<Msg, Reply>` derived from the lambda's
                // parameter / return annotations otherwise.
                if let (
                    Pattern::Identifier(name),
                    Some((
                        Expr::SpawnLambdaActor {
                            params: lambda_params,
                            return_type,
                            ..
                        },
                        _,
                    )),
                ) = (&pattern.0, value.as_ref())
                {
                    let binding_ty = match ty.as_ref() {
                        Some(annotation) => self.lower_type(annotation),
                        None => self.actor_lambda_duplex_ty(lambda_params, return_type.as_ref()),
                    };
                    // Pre-bind in the current scope; record the id so
                    // we can detect a self-reference inside the body
                    // walk via builder state.
                    let pre_binding = self.bind(name.clone(), binding_ty, false, pattern.1.clone());
                    let prior = self
                        .current_actor_self
                        .replace((pre_binding.id, name.clone()));
                    let lowered_value = self.lower_expr(
                        value.as_ref().expect("value Some checked above"),
                        IntentKind::Consume,
                    );
                    self.current_actor_self = prior;
                    return HirStmt {
                        node: self.ids.node(),
                        kind: HirStmtKind::Let(pre_binding, Some(lowered_value)),
                        span,
                    };
                }
                let value = value
                    .as_ref()
                    .map(|expr| self.lower_expr(expr, IntentKind::Consume));
                let binding_ty = ty.as_ref().map_or_else(
                    || {
                        value
                            .as_ref()
                            .map_or(ResolvedTy::Unit, |expr| expr.ty.clone())
                    },
                    |ty| self.lower_type(ty),
                );
                let name = self
                    .pattern_name(pattern)
                    .unwrap_or_else(|| "_".to_string());
                let binding = self.bind(name, binding_ty, false, pattern.1.clone());
                HirStmtKind::Let(binding, value)
            }
            Stmt::Var { name, ty, value } => {
                let value = value
                    .as_ref()
                    .map(|expr| self.lower_expr(expr, IntentKind::Consume));
                let binding_ty = ty.as_ref().map_or_else(
                    || {
                        value
                            .as_ref()
                            .map_or(ResolvedTy::Unit, |expr| expr.ty.clone())
                    },
                    |ty| self.lower_type(ty),
                );
                let binding = self.bind(name.clone(), binding_ty, true, span.clone());
                HirStmtKind::Let(binding, value)
            }
            Stmt::Expression(expr) => {
                // Inside a scope{} body, statement-expression calls are child-task
                // spawns (TI-1). Outside scope{} bodies all calls are synchronous
                // (TI-3). The TI-1 rewrite only applies when the expression is a
                // direct call — nested calls inside sub-expressions remain sync.
                //
                // Mark this as statement position before lowering so that
                // `lower_expr`'s `Expr::Await` arm can enforce TI-4 (await is
                // only legal in statement-expression position, not as a
                // sub-expression). The flag is consumed by `mem::replace` at the
                // top of `lower_expr`, so recursive calls see `false`.
                self.statement_position = true;
                if self.scope_depth > 0 {
                    if let Expr::Call { .. } = &expr.0 {
                        let spawned = self.lower_spawned_call(expr);
                        HirStmtKind::Expr(spawned)
                    } else {
                        HirStmtKind::Expr(self.lower_expr(expr, IntentKind::Read))
                    }
                } else {
                    HirStmtKind::Expr(self.lower_expr(expr, IntentKind::Read))
                }
            }
            Stmt::Return(value) => {
                if let Some(value) = value {
                    let expr = self.lower_expr(value, IntentKind::Consume);
                    // TI-5 escape check: a `Task<T>` value must not escape via
                    // return, whether the type was user-written or inferred. The
                    // `lower_type` wall blocks user-written `Task<T>` annotations;
                    // this check closes the inferred-escape path.
                    if matches!(expr.ty, ResolvedTy::Task(_)) {
                        self.diagnostics.push(HirDiagnostic::new(
                            HirDiagnosticKind::TaskCannotEscape,
                            value.1.clone(),
                            "a `Task<T>` handle cannot escape via `return`; \
                             await it inside the `scope{}` body with `await name`",
                        ));
                    } else if expr.ty != return_ty && return_ty != ResolvedTy::Unit {
                        self.diagnostics.push(HirDiagnostic::new(
                            HirDiagnosticKind::ReturnTypeMismatch {
                                expected: return_ty,
                                actual: expr.ty.clone(),
                            },
                            value.1.clone(),
                            "return expression type differs from function return annotation",
                        ));
                    }
                    HirStmtKind::Return(Some(expr))
                } else {
                    HirStmtKind::Return(None)
                }
            }
            _ => {
                self.unsupported(span.clone(), "statement", "slice-2");
                HirStmtKind::Expr(self.unsupported_expr(span.clone(), "unsupported statement"))
            }
        };
        HirStmt {
            node: self.ids.node(),
            kind,
            span,
        }
    }

    #[allow(
        clippy::too_many_lines,
        reason = "single large match on expr variants; splitting would hurt readability"
    )]
    fn lower_expr(&mut self, expr: &Spanned<Expr>, intent: IntentKind) -> HirExpr {
        // Consume the statement-position flag atomically. Every recursive call
        // to `lower_expr` (for arguments, operands, return values, block tails,
        // etc.) therefore sees `false`. Only the `Stmt::Expression` arm in
        // `lower_stmt` sets this to `true` immediately before calling us.
        let in_stmt_position = std::mem::replace(&mut self.statement_position, false);
        let span = expr.1.clone();
        // Pre-allocate the SiteId for this expression so call-site
        // side-tables (e.g. `call_site_type_args`) can be keyed
        // by the eventual HirExpr.site before the wrapping struct is
        // built. Allocation order: site before node so existing
        // SiteId counts in tests stay stable (lower_expr previously
        // allocated node before site at the same call).
        let site = self.ids.site();
        let (kind, ty) = match &expr.0 {
            Expr::Literal(lit) => Self::lower_literal(lit),
            Expr::Identifier(name) => self.lower_identifier(name, span.clone()),
            Expr::Binary { left, op, right } => {
                let left = self.lower_expr(left, IntentKind::Read);
                let right = self.lower_expr(right, IntentKind::Read);
                let ty = Self::binary_ty(*op, &left.ty, &right.ty);
                (
                    HirExprKind::Binary {
                        op: *op,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    ty,
                )
            }
            Expr::Call { function, args, .. } => {
                let callee = self.lower_expr(function, IntentKind::Read);
                let args = args
                    .iter()
                    .map(|arg| self.lower_expr(arg.expr(), IntentKind::Read))
                    .collect();
                // Record the per-instantiation monomorphisation if the
                // callee is a generic top-level user fn. Direct-name
                // callees only; `record_monomorphisation` filters out
                // non-generic callees, non-`fn_registry` callees (builtins,
                // runtime symbols, local bindings), and callsites the
                // checker did not record. Fail-closed on poisoned entries
                // and on registry-cap exhaustion.
                //
                // Also threads the resolved `Vec<ResolvedTy>` for this
                // call site (concrete or symbolic-T) into
                // `call_site_type_args` keyed by the wrapping HirExpr's
                // SiteId so MIR lowering can rewrite the call to the
                // mangled symbol of the right per-monomorphisation MIR
                // function.
                self.record_monomorphisation(&function.0, &span, site);
                // Checker authority takes precedence: consult expr_types at the
                // full call-expression span.  The checker records the call result
                // type here — including for checker-registered builtins like
                // `duplex_pair` that have no AST `fn` item and therefore no
                // `fn_registry` hit.  (LESSONS: checker-authority P0)
                let checker_key = SpanKey::from(&span);
                let result_ty = if let Some(ty) = self.expr_types.get(&checker_key).cloned() {
                    match ResolvedTy::from_ty(&ty) {
                        Ok(resolved) => resolved,
                        Err(err) => {
                            // Fail-closed: checker side-table is poisoned for
                            // this call.  Emit a diagnostic; never silently
                            // substitute Unit.  (LESSONS: checker-output-boundary P0)
                            let callee_name = if let Expr::Identifier(name) = &function.0 {
                                name.clone()
                            } else {
                                "<expr>".to_string()
                            };
                            self.diagnostics.push(HirDiagnostic::new(
                                HirDiagnosticKind::CheckerBoundaryViolation {
                                    name: callee_name,
                                    reason: err.to_string(),
                                },
                                span.clone(),
                                "checker-authoritative call result type failed boundary conversion",
                            ));
                            ResolvedTy::Unit
                        }
                    }
                } else if let ResolvedTy::Function { ret, .. } = &callee.ty {
                    // No checker entry: fall through to the callee's HIR-inferred
                    // function type (used for calls to functions that are in
                    // fn_registry or locally resolved).
                    *ret.clone()
                } else {
                    if matches!(
                        callee.kind,
                        HirExprKind::BindingRef {
                            resolved: ResolvedRef::Unresolved,
                            ..
                        }
                    ) {
                        self.diagnostics.push(HirDiagnostic::new(
                            HirDiagnosticKind::UnresolvedInferenceVar,
                            span.clone(),
                            "call result type cannot be determined: callee is unresolved",
                        ));
                    }
                    ResolvedTy::Unit
                };
                (
                    HirExprKind::Call {
                        callee: Box::new(callee),
                        args,
                    },
                    result_ty,
                )
            }
            Expr::Block(block) => {
                let block = self.lower_block(block, &ResolvedTy::Unit);
                let ty = block.ty.clone();
                (HirExprKind::Block(block), ty)
            }
            Expr::If {
                condition,
                then_block,
                else_block,
            } => {
                let condition = self.lower_expr(condition, IntentKind::Read);
                let then_expr = self.lower_expr(then_block, IntentKind::Read);
                let else_expr = else_block
                    .as_ref()
                    .map(|expr| Box::new(self.lower_expr(expr, IntentKind::Read)));
                let ty = else_expr
                    .as_ref()
                    .map_or(ResolvedTy::Unit, |expr| expr.ty.clone());
                (
                    HirExprKind::If {
                        condition: Box::new(condition),
                        then_expr: Box::new(then_expr),
                        else_expr,
                    },
                    ty,
                )
            }
            Expr::StructInit {
                name,
                fields,
                // Surface-level explicit type arguments (`Box<int> { ... }`).
                // The HIR-recorded `type_args` below comes from the checker's
                // `record_init_type_args` side-table (which already reconciled
                // inferred-vs-explicit and substituted enclosing-fn type-params
                // where applicable), so we ignore the raw surface args here.
                type_args: _,
                base,
            } => {
                // Record the per-instantiation `RecordLayout` for
                // generic user records and capture the concrete type-args
                // for propagation onto this expression's resolved type.
                // `None` indicates a monomorphic record or builtin; for
                // those, the resulting `Named` carries `args: []` as before.
                let resolved_type_args = self.record_record_layout(name, &span).unwrap_or_default();
                let hir_fields = fields
                    .iter()
                    .map(|(fname, expr)| (fname.clone(), self.lower_expr(expr, IntentKind::Read)))
                    .collect();
                // Lower the functional-update base if present. The checker has
                // already validated type-compatibility and field coverage; HIR
                // carries it verbatim so MIR lowering (A-7) can read the
                // un-overridden fields from the base value.
                let hir_base = base.as_deref().map(|(base_expr, base_span)| {
                    Box::new(
                        self.lower_expr(&(base_expr.clone(), base_span.clone()), IntentKind::Read),
                    )
                });
                (
                    HirExprKind::StructInit {
                        name: name.clone(),
                        type_args: resolved_type_args.clone(),
                        fields: hir_fields,
                        base: hir_base,
                    },
                    ResolvedTy::Named {
                        name: name.clone(),
                        args: resolved_type_args,
                    },
                )
            }
            Expr::Scope { body } => {
                // A `scope{}` block lowers to `HirExprKind::Scope`. Inside the
                // body, statement-calls become spawned-call nodes (TI-1) and
                // `fork name = call(...)` statements introduce `Task<T>` bindings
                // (TI-2). The scope block's type is `Unit` — it is a lifetime
                // boundary, not a value-producing expression.
                self.scope_depth += 1;
                let hir_body = self.lower_scope_block(body);
                self.scope_depth -= 1;
                (HirExprKind::Scope { body: hir_body }, ResolvedTy::Unit)
            }
            Expr::ForkChild { binding, expr } => {
                // `fork name = expr` outside a `scope{}` body: no spawn context,
                // so this is malformed. Emit CutoverUnsupported — the grammar
                // accepts this form but HIR-lowering requires scope context.
                // (Inside scope{} bodies this variant is handled by lower_scope_block,
                // not by lower_expr directly.)
                if self.scope_depth == 0 {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::AwaitOutOfPosition,
                        span.clone(),
                        "`fork name = expr` is only valid inside a `scope{}` body",
                    ));
                    (
                        HirExprKind::Unsupported(
                            "`fork name = expr` outside scope body".to_string(),
                        ),
                        ResolvedTy::Unit,
                    )
                } else {
                    // Inside a scope body, lower_scope_block handles this case;
                    // reaching here means the expression appeared in a non-statement
                    // position (e.g. tail expression). Reject: task handles cannot
                    // be used as values.
                    let _ = binding;
                    let _ = expr;
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::AwaitOutOfPosition,
                        span.clone(),
                        "`fork name = expr` must be a statement, not an expression value",
                    ));
                    (
                        HirExprKind::Unsupported("`fork name = expr` as expression".to_string()),
                        ResolvedTy::Unit,
                    )
                }
            }
            Expr::ForkBlock { body } => {
                if self.scope_depth == 0 || !in_stmt_position {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::AwaitOutOfPosition,
                        span.clone(),
                        "`fork { ... }` child-task blocks are only legal as statements inside a `scope{}` body",
                    ));
                    (
                        HirExprKind::Unsupported(
                            "`fork { ... }` outside scope statement".to_string(),
                        ),
                        ResolvedTy::Unit,
                    )
                } else {
                    let task_ty = ResolvedTy::Task(Box::new(ResolvedTy::Unit));
                    (
                        HirExprKind::ForkBlock {
                            body: self.lower_cancellation_clause_block(body),
                            task_ty: task_ty.clone(),
                        },
                        task_ty,
                    )
                }
            }
            Expr::ScopeDeadline { duration, body } => {
                if self.scope_depth == 0 || !in_stmt_position {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::AwaitOutOfPosition,
                        span.clone(),
                        "`after(duration) { ... }` deadline clauses are only legal as statements inside a `scope{}` body",
                    ));
                    (
                        HirExprKind::Unsupported(
                            "`after(duration) { ... }` outside scope statement".to_string(),
                        ),
                        ResolvedTy::Unit,
                    )
                } else {
                    let duration = self.lower_expr(duration, IntentKind::Read);
                    (
                        HirExprKind::ScopeDeadline {
                            duration: Box::new(duration),
                            body: self.lower_cancellation_clause_block(body),
                        },
                        ResolvedTy::Unit,
                    )
                }
            }
            Expr::Await(inner) => {
                // `await expr` — only legal as the direct statement-expression
                // inside a `scope{}` body in v0.5 (TI-4). Sub-expression positions
                // (return value, function argument, binary operand, let value, block
                // tail, etc.) are rejected with `AwaitOutOfPosition`.
                // `in_stmt_position` is set by `Stmt::Expression` in `lower_stmt`
                // and consumed by `mem::replace` at the top of this function, so
                // recursive calls always see `false`.
                if self.scope_depth == 0 || !in_stmt_position {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::AwaitOutOfPosition,
                        span.clone(),
                        "`await` is only legal as a statement-expression inside a `scope{}` body \
                         in v0.5. It cannot be used as a return value, function argument, \
                         binary operand, or let-value. Move the await to its own statement.",
                    ));
                    return HirExpr {
                        node: self.ids.node(),
                        site: self.ids.site(),
                        value_class: ValueClass::BitCopy,
                        ty: ResolvedTy::Unit,
                        intent,
                        kind: HirExprKind::Unsupported("`await` out of position".to_string()),
                        span,
                    };
                }
                // Resolve the inner expression. It must be a binding-ref with a
                // `Task<T>` type to be awaitable.
                let inner_hir = self.lower_expr(inner, IntentKind::Consume);
                match &inner_hir.ty {
                    ResolvedTy::Task(output_ty) => {
                        let output_ty = *output_ty.clone();
                        // Extract the binding name and id for the AwaitTask node.
                        // The inner expression must be a direct binding-ref; await
                        // on a complex expression is not supported in v0.5.
                        if let HirExprKind::BindingRef {
                            name: binding_name,
                            resolved: ResolvedRef::Binding(binding_id),
                        } = &inner_hir.kind
                        {
                            let (binding_name, binding_id) = (binding_name.clone(), *binding_id);
                            let value_class = ValueClass::of_ty(&output_ty, &self.type_classes);
                            return HirExpr {
                                node: self.ids.node(),
                                site: self.ids.site(),
                                value_class,
                                ty: output_ty.clone(),
                                intent,
                                kind: HirExprKind::AwaitTask {
                                    binding_name,
                                    binding_id,
                                    output_ty,
                                },
                                span,
                            };
                        }
                        // Await on a non-binding-ref Task<T>: reject. The form
                        // `await (some_expr)` where the expr is not a name is not
                        // supported — only named bindings can be awaited.
                        self.diagnostics.push(HirDiagnostic::new(
                            HirDiagnosticKind::AwaitOutOfPosition,
                            span.clone(),
                            "`await` requires a named task binding, not an expression",
                        ));
                        (
                            HirExprKind::Unsupported("`await` on non-binding-ref task".to_string()),
                            ResolvedTy::Unit,
                        )
                    }
                    found_ty => {
                        // The operand is not a Task<T> — reject with AwaitNonTask.
                        let found_ty = found_ty.clone();
                        self.diagnostics.push(HirDiagnostic::new(
                            HirDiagnosticKind::AwaitNonTask {
                                found_ty: found_ty.clone(),
                            },
                            span.clone(),
                            "`await` requires a task handle (`Task<T>`). \
                             Hint: did you mean to bind a task with `fork name = call(...)` first?",
                        ));
                        (
                            HirExprKind::Unsupported("`await` on non-task".to_string()),
                            ResolvedTy::Unit,
                        )
                    }
                }
            }
            Expr::Select { arms, timeout } => {
                self.lower_select(arms, timeout.as_deref(), span.clone())
            }
            Expr::SpawnLambdaActor {
                params,
                return_type,
                body,
                ..
            } => self.lower_spawn_lambda_actor(params, return_type.as_ref(), body),
            Expr::Lambda {
                params,
                return_type,
                body,
                ..
            } => self.lower_closure(params, return_type.as_ref(), body, span.clone()),
            Expr::MethodCall {
                receiver,
                method,
                args,
            } => self.lower_method_call(receiver, method, args, span.clone()),
            Expr::UnsafeBlock(block) => {
                // Unsafe clearance is a checker-only concept; the HIR represents the
                // body as a plain block.  The `in_unsafe` flag pushed by the type
                // checker is not carried into HIR or MIR — pointer and FFI safety
                // obligations are enforced by the checker before lowering.
                let hir_block = self.lower_block(block, &ResolvedTy::Unit);
                let ty = hir_block.ty.clone();
                (HirExprKind::Block(hir_block), ty)
            }
            Expr::Index { object, index } => {
                // The checker records the element/result type at the whole
                // index expression's span. LESSONS: `checker-authority` P0 —
                // we never re-derive the element type from the container;
                // the checker is the sole owner.
                let checker_key = SpanKey::from(&span);
                let result_ty = if let Some(ty) = self.expr_types.get(&checker_key).cloned() {
                    match ResolvedTy::from_ty(&ty) {
                        Ok(resolved) => resolved,
                        Err(err) => {
                            self.diagnostics.push(HirDiagnostic::new(
                                HirDiagnosticKind::CheckerBoundaryViolation {
                                    name: "xs[..]".to_string(),
                                    reason: err.to_string(),
                                },
                                span.clone(),
                                "checker-authoritative index/slice result type failed boundary conversion",
                            ));
                            ResolvedTy::Unit
                        }
                    }
                } else {
                    // Fall through: no checker entry. This can happen when the
                    // checker emitted an error for this expression (e.g.
                    // indexing into a non-Vec) and skipped recording the
                    // type. The diagnostic from the checker covers the
                    // user-facing error; emit Unit to stay well-formed.
                    ResolvedTy::Unit
                };

                // Distinguish range-slice (`xs[a..b]` and the four open-end
                // forms) from single-element indexing (`xs[i]`). The parser
                // emits `Expr::Range` only when the bracket contents are a
                // range; all other expressions remain as `Expr::Index`.
                let container = self.lower_expr(object, IntentKind::Read);
                if let Expr::Range {
                    start,
                    end,
                    inclusive,
                } = &index.0
                {
                    // C-3 range-slice: result type is Vec<T> (checker-authoritative).
                    let lowered_start = start
                        .as_ref()
                        .map(|s| Box::new(self.lower_expr(s, IntentKind::Read)));
                    let lowered_end = end
                        .as_ref()
                        .map(|e| Box::new(self.lower_expr(e, IntentKind::Read)));
                    (
                        HirExprKind::Slice {
                            container: Box::new(container),
                            start: lowered_start,
                            end: lowered_end,
                            inclusive: *inclusive,
                        },
                        result_ty,
                    )
                } else {
                    let index_expr = self.lower_expr(index, IntentKind::Read);
                    if let Some(dyn_call) = self
                        .dyn_trait_method_calls
                        .get(&SpanKey::from(&span))
                        .cloned()
                    {
                        return HirExpr {
                            node: self.ids.node(),
                            site,
                            value_class: ValueClass::of_ty(&result_ty, &self.type_classes),
                            ty: result_ty.clone(),
                            intent,
                            kind: HirExprKind::CallDynMethod {
                                receiver: Box::new(container),
                                trait_name: dyn_call.trait_name,
                                method_name: dyn_call.method_name,
                                slot: dyn_call.slot,
                                args: vec![index_expr],
                                ret_ty: result_ty,
                            },
                            span: span.clone(),
                        };
                    }

                    if let ResolvedTy::Named { name, .. } = &container.ty {
                        let callee_name = format!("{name}::at");
                        if name != "Vec" && self.fn_registry.contains_key(&callee_name) {
                            let callee_ty = ResolvedTy::Function {
                                params: vec![container.ty.clone(), index_expr.ty.clone()],
                                ret: Box::new(result_ty.clone()),
                            };
                            let resolved = self
                                .fn_registry
                                .get(&callee_name)
                                .map_or(ResolvedRef::Unresolved, |entry| {
                                    ResolvedRef::Item(entry.id)
                                });
                            let callee = HirExpr {
                                node: self.ids.node(),
                                site: self.ids.site(),
                                value_class: ValueClass::PersistentShare,
                                ty: callee_ty,
                                intent: IntentKind::Read,
                                kind: HirExprKind::BindingRef {
                                    name: callee_name,
                                    resolved,
                                },
                                span: span.clone(),
                            };
                            return HirExpr {
                                node: self.ids.node(),
                                site,
                                value_class: ValueClass::of_ty(&result_ty, &self.type_classes),
                                ty: result_ty.clone(),
                                intent,
                                kind: HirExprKind::Call {
                                    callee: Box::new(callee),
                                    args: vec![container, index_expr],
                                },
                                span: span.clone(),
                            };
                        }
                    }

                    // C-2 single-element Vec indexing: result type is element type T.
                    (
                        HirExprKind::Index {
                            container: Box::new(container),
                            index: Box::new(index_expr),
                        },
                        result_ty,
                    )
                }
            }
            Expr::Is { lhs, rhs } => {
                // Identity comparison: `lhs is rhs`. The checker (D-2) has already
                // validated that both operands carry identity-bearing types and
                // that neither is a scalar/String/record. The result is `bool`.
                // LESSONS: `checker-authority` P0 — we do not re-validate the
                // allowance set here; that is the checker's sole responsibility.
                let left = self.lower_expr(lhs, IntentKind::Read);
                let right = self.lower_expr(rhs, IntentKind::Read);
                (
                    HirExprKind::IdentityCompare {
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    ResolvedTy::Bool,
                )
            }
            Expr::FieldAccess { object, field } => {
                // Named-field read on a record or struct type. The checker has
                // already resolved the field and recorded the result type in
                // `expr_types`. LESSONS: `checker-authority` P0 — the type of
                // the field read comes exclusively from the checker side-table,
                // never re-derived here.
                let hir_object = self.lower_expr(object, IntentKind::Read);
                let checker_key = SpanKey::from(&span);
                let field_ty = if let Some(ty) = self.expr_types.get(&checker_key).cloned() {
                    match ResolvedTy::from_ty(&ty) {
                        Ok(resolved) => resolved,
                        Err(err) => {
                            self.diagnostics.push(HirDiagnostic::new(
                                HirDiagnosticKind::CheckerBoundaryViolation {
                                    name: field.clone(),
                                    reason: err.to_string(),
                                },
                                span.clone(),
                                "field-access result type failed checker-boundary conversion",
                            ));
                            ResolvedTy::Unit
                        }
                    }
                } else {
                    // No checker entry: malformed checker output. Fail-closed.
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::CheckerBoundaryViolation {
                            name: field.clone(),
                            reason: "expr_types has no entry for field-access site".into(),
                        },
                        span.clone(),
                        "field-access result type missing from checker side-table",
                    ));
                    ResolvedTy::Unit
                };
                (
                    HirExprKind::FieldAccess {
                        object: Box::new(hir_object),
                        field: field.clone(),
                    },
                    field_ty,
                )
            }
            _ => {
                self.unsupported(span.clone(), "expression", "slice-2");
                (
                    HirExprKind::Unsupported("unsupported expression".into()),
                    ResolvedTy::Unit,
                )
            }
        };
        let inner = HirExpr {
            node: self.ids.node(),
            site,
            value_class: ValueClass::of_ty(&ty, &self.type_classes),
            ty,
            intent,
            kind,
            span: span.clone(),
        };
        // Checker-authority coercion: if the just-lowered expression sits at
        // a `T → dyn Trait` coercion site (recorded by the type checker at
        // the *argument* expression span), wrap the result in
        // `HirExprKind::CoerceToDynTrait`. MIR lowers this 1:1 to
        // `Instr::CoerceToDynTrait`. The wrapping ResolvedTy is the
        // destination trait-object type, which the wrapping HirExpr
        // carries; the inner expression keeps its concrete type.
        let coercion_key = SpanKey::from(&span);
        if let Some(coercion) = self.dyn_trait_coercions.get(&coercion_key).cloned() {
            let mut resolved_bounds = Vec::new();
            for name in coercion.trait_name.split('+') {
                let mut assoc_bindings = Vec::new();
                for binding in coercion
                    .assoc_bindings
                    .iter()
                    .filter(|binding| binding.trait_name == name)
                {
                    let Ok(ty) = hew_types::ResolvedTy::from_ty(&binding.ty) else {
                        self.diagnostics.push(HirDiagnostic::new(
                            HirDiagnosticKind::CheckerBoundaryViolation {
                                name: "dyn-trait assoc binding".to_string(),
                                reason: format!(
                                    "`{}::{}` failed boundary conversion",
                                    binding.trait_name, binding.assoc_name
                                ),
                            },
                            span.clone(),
                            "associated type binding from dyn_trait_coercions failed boundary conversion",
                        ));
                        return inner;
                    };
                    assoc_bindings.push((binding.assoc_name.clone(), ty));
                }
                resolved_bounds.push(hew_types::ResolvedTraitBound {
                    trait_name: name.to_string(),
                    args: vec![],
                    assoc_bindings,
                });
            }
            let dyn_ty = ResolvedTy::TraitObject {
                traits: resolved_bounds,
            };
            let concrete_resolved = match ResolvedTy::from_ty(&coercion.concrete_type) {
                Ok(r) => r,
                Err(err) => {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::CheckerBoundaryViolation {
                            name: "dyn-trait coercion".to_string(),
                            reason: err.to_string(),
                        },
                        span.clone(),
                        "concrete type from dyn_trait_coercions failed boundary conversion",
                    ));
                    return inner;
                }
            };
            return HirExpr {
                node: self.ids.node(),
                site: self.ids.site(),
                value_class: ValueClass::of_ty(&dyn_ty, &self.type_classes),
                ty: dyn_ty,
                intent,
                kind: HirExprKind::CoerceToDynTrait {
                    value: Box::new(inner),
                    trait_name: coercion.trait_name,
                    concrete_type: concrete_resolved,
                    method_table: coercion.method_table,
                    vtable_entries: coercion.vtable_entries,
                },
                span,
            };
        }
        inner
    }

    /// Lower a parsed `select { ... }` expression to HIR.
    ///
    /// Per HEW-SPEC-2026 §4.11.1 the four arm forms are exhaustive:
    ///   1. `pat from next(<stream-expr>) => body`
    ///   2. `pat from <actor-expr>.<method>(<args>) => body`   (actor ask)
    ///   3. `pat from await <task-expr> => body`
    ///   4. `after <duration-expr> => body`                    (timer)
    ///
    /// Any other arm source shape is rejected with
    /// `SelectArmNotSealedForm`. Body-type disagreement is rejected
    /// with `SelectArmTypeMismatch`. Empty selects and multiple-after
    /// arms are rejected with `SelectNoArms` and
    /// `SelectMultipleAfterArms` respectively.
    fn lower_select(
        &mut self,
        arms: &[SelectArm],
        timeout: Option<&TimeoutClause>,
        span: std::ops::Range<usize>,
    ) -> (HirExprKind, ResolvedTy) {
        // Empty select — neither arms nor a timer — fires nothing.
        if arms.is_empty() && timeout.is_none() {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::SelectNoArms,
                span.clone(),
                "select expression contains no arms",
            ));
            return (
                HirExprKind::Unsupported("empty select".into()),
                ResolvedTy::Unit,
            );
        }

        // Multiple `after` arms have no meaningful join semantics and are
        // rejected. An `Expr::Timeout`-sourced arm in the `arms` vec is
        // treated as an `AfterTimer` arm; if that is combined with the
        // dedicated `timeout` field (or if two appear in `arms`) the
        // second one triggers `SelectMultipleAfterArms`.

        let mut hir_arms: Vec<HirSelectArm> = Vec::with_capacity(arms.len() + 1);
        let mut expected_ty: Option<ResolvedTy> = None;
        let mut first_after_span: Option<std::ops::Range<usize>> = None;

        for arm in arms {
            let binding_name = self.pattern_name(&arm.binding);
            let kind = self.recognize_sealed_arm_source(&arm.source);
            if matches!(kind, HirSelectArmKind::AfterTimer { .. }) {
                if first_after_span.is_some() {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::SelectMultipleAfterArms,
                        arm.source.1.clone(),
                        "select may have at most one `after` arm",
                    ));
                } else {
                    first_after_span = Some(arm.source.1.clone());
                }
            }
            let body = self.lower_expr(&arm.body, IntentKind::Read);
            if let Some(expected) = expected_ty.as_ref() {
                if &body.ty != expected {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::SelectArmTypeMismatch {
                            arm_index: hir_arms.len(),
                            expected: expected.clone(),
                            actual: body.ty.clone(),
                        },
                        arm.body.1.clone(),
                        "select arm body type differs from the first arm body type",
                    ));
                }
            } else {
                expected_ty = Some(body.ty.clone());
            }
            hir_arms.push(HirSelectArm {
                kind,
                binding_name,
                body,
            });
        }

        if let Some(timeout) = timeout {
            if first_after_span.is_some() {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::SelectMultipleAfterArms,
                    timeout.duration.1.clone(),
                    "select may have at most one `after` arm",
                ));
            }
            let duration = self.lower_expr(&timeout.duration, IntentKind::Read);
            let body = self.lower_expr(&timeout.body, IntentKind::Read);
            if let Some(expected) = expected_ty.as_ref() {
                if &body.ty != expected {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::SelectArmTypeMismatch {
                            arm_index: hir_arms.len(),
                            expected: expected.clone(),
                            actual: body.ty.clone(),
                        },
                        timeout.body.1.clone(),
                        "select after-arm body type differs from earlier arm body types",
                    ));
                }
            } else {
                expected_ty = Some(body.ty.clone());
            }
            hir_arms.push(HirSelectArm {
                kind: HirSelectArmKind::AfterTimer {
                    duration: Box::new(duration),
                },
                binding_name: None,
                body,
            });
        }

        let result_ty = expected_ty.unwrap_or(ResolvedTy::Unit);
        (HirExprKind::Select(HirSelect { arms: hir_arms }), result_ty)
    }

    /// Build the `Duplex<Msg, Reply>` `ResolvedTy` for an actor-lambda
    /// from its parameter list and optional return-type annotation.
    /// Mirrors the forward-bind logic in `hew-types::check::statements`:
    /// zero params → Unit message; one param → that param's type;
    /// multiple params → tuple of param types. The HIR layer only needs
    /// a placeholder shape so the forward-bind succeeds and capture
    /// resolution sees the let-name.
    ///
    /// SHIM — `ResolvedTy::Unit` for missing param / return annotations.
    ///
    /// - WHY: HIR has no type-inference machinery; the slice-2 type
    ///   checker uses `TypeVar::fresh()` to allocate inference variables
    ///   which HIR cannot represent. The forward-bind only needs a
    ///   syntactic placeholder so the body's recursive self-reference
    ///   resolves to a `BindingId`; the actual type identity is
    ///   reconstructed downstream.
    /// - WHEN OBSOLETE: either HIR gains a placeholder type variant
    ///   (an explicit `ResolvedTy::Hole` or equivalent) for forward-bind
    ///   sites, or the post-typecheck pipeline rewrites HIR binding
    ///   types from the slice-2 unifier's substitution table via a
    ///   side-table keyed on `BindingId`.
    /// - REAL SOLUTION: lift the slice-2 unifier's substitution into a
    ///   `BindingId → Ty` side-table emitted alongside HIR and consumed
    ///   by MIR lowering, so HIR never has to invent a stand-in.
    fn actor_lambda_duplex_ty(
        &mut self,
        params: &[LambdaParam],
        return_type: Option<&Spanned<TypeExpr>>,
    ) -> ResolvedTy {
        let param_tys: Vec<ResolvedTy> = params
            .iter()
            .map(|p| {
                p.ty.as_ref()
                    .map_or(ResolvedTy::Unit, |annotation| self.lower_type(annotation))
            })
            .collect();
        let msg_ty = match param_tys.len() {
            0 => ResolvedTy::Unit,
            1 => param_tys.into_iter().next().unwrap(),
            _ => ResolvedTy::Tuple(param_tys),
        };
        let reply_ty = return_type
            .as_ref()
            .map_or(ResolvedTy::Unit, |ann| self.lower_type(ann));
        ResolvedTy::Named {
            name: "Duplex".to_string(),
            args: vec![msg_ty, reply_ty],
        }
    }

    fn closure_signature_from_ty(ty: &ResolvedTy) -> Option<(Vec<ResolvedTy>, ResolvedTy)> {
        match ty {
            ResolvedTy::Function { params, ret } | ResolvedTy::Closure { params, ret, .. } => {
                Some((params.clone(), ret.as_ref().clone()))
            }
            _ => None,
        }
    }

    fn visible_outer_bindings(&self) -> HashMap<BindingId, OuterClosureBinding> {
        let mut visible_by_name: HashMap<String, ScopeBinding> = HashMap::new();
        for scope in self.scopes.iter().rev() {
            for (name, (id, ty, span)) in scope {
                visible_by_name
                    .entry(name.clone())
                    .or_insert_with(|| (*id, ty.clone(), span.clone()));
            }
        }
        visible_by_name
            .into_iter()
            .map(|(name, (id, ty, span))| (id, (name, ty, span)))
            .collect()
    }

    fn lower_closure(
        &mut self,
        params: &[LambdaParam],
        return_type: Option<&Spanned<TypeExpr>>,
        body: &Spanned<Expr>,
        span: std::ops::Range<usize>,
    ) -> (HirExprKind, ResolvedTy) {
        let checker_key = SpanKey::from(&span);
        let closure_ty = if let Some(ty) = self.expr_types.get(&checker_key).cloned() {
            match ResolvedTy::from_ty(&ty) {
                Ok(resolved) => resolved,
                Err(err) => {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::CheckerBoundaryViolation {
                            name: "closure literal".to_string(),
                            reason: err.to_string(),
                        },
                        span.clone(),
                        "closure literal type failed checker-boundary conversion",
                    ));
                    ResolvedTy::Function {
                        params: vec![],
                        ret: Box::new(ResolvedTy::Unit),
                    }
                }
            }
        } else {
            let param_tys: Vec<ResolvedTy> = params
                .iter()
                .map(|p| {
                    p.ty.as_ref()
                        .map_or(ResolvedTy::Unit, |annotation| self.lower_type(annotation))
                })
                .collect();
            let ret_ty = return_type
                .as_ref()
                .map_or(ResolvedTy::Unit, |ann| self.lower_type(ann));
            ResolvedTy::Function {
                params: param_tys,
                ret: Box::new(ret_ty),
            }
        };

        let (signature_params, ret_ty) = Self::closure_signature_from_ty(&closure_ty)
            .unwrap_or_else(|| {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::CheckerBoundaryViolation {
                        name: "closure literal".to_string(),
                        reason: format!("expected Function/Closure type, got {closure_ty:?}"),
                    },
                    span.clone(),
                    "closure literal did not type as a callable value",
                ));
                (vec![], ResolvedTy::Unit)
            });

        let outer_bindings = self.visible_outer_bindings();
        self.push_scope();
        let mut hir_params: Vec<HirBinding> = Vec::with_capacity(params.len());
        for (idx, param) in params.iter().enumerate() {
            let ty = signature_params
                .get(idx)
                .cloned()
                .or_else(|| {
                    param
                        .ty
                        .as_ref()
                        .map(|annotation| self.lower_type(annotation))
                })
                .unwrap_or(ResolvedTy::Unit);
            let binding = self.bind(param.name.clone(), ty, false, 0..0);
            hir_params.push(binding);
        }
        let lowered_body = self.lower_expr(body, IntentKind::Read);
        self.pop_scope();

        let checker_facts = if let Some(facts) = self.closure_capture_facts.get(&checker_key) {
            facts.clone()
        } else {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::CheckerBoundaryViolation {
                    name: "closure literal".to_string(),
                    reason: "closure_capture_facts has no record for closure literal span"
                        .to_string(),
                },
                span.clone(),
                "closure literal reached HIR without checker capture metadata",
            ));
            Vec::new()
        };

        let captures =
            self.materialize_closure_captures(&lowered_body, &outer_bindings, checker_facts, span);

        (
            HirExprKind::Closure {
                params: hir_params,
                ret_ty,
                body: Box::new(lowered_body),
                captures,
            },
            closure_ty,
        )
    }

    fn materialize_closure_captures(
        &mut self,
        body: &HirExpr,
        outer_bindings: &HashMap<BindingId, OuterClosureBinding>,
        facts: Vec<ClosureCaptureFact>,
        span: std::ops::Range<usize>,
    ) -> Vec<HirClosureCapture> {
        let mut seen: HashSet<BindingId> = HashSet::new();
        let mut ordered: Vec<ClosureCaptureCandidate> = Vec::new();
        collect_general_closure_captures_walk(body, outer_bindings, &mut seen, &mut ordered);

        let mut remaining_facts = facts;
        let mut captures = Vec::with_capacity(ordered.len());
        for (binding, name, def_span) in ordered {
            let fact_idx = remaining_facts.iter().position(|fact| {
                fact.name == name
                    && fact
                        .def_span
                        .as_ref()
                        .is_some_and(|fact_def_span| *fact_def_span == def_span)
            });
            let fact_idx = fact_idx.or_else(|| {
                let mut matches = remaining_facts
                    .iter()
                    .enumerate()
                    .filter(|(_, fact)| fact.name == name);
                let first = matches.next()?;
                if matches.next().is_none() {
                    Some(first.0)
                } else {
                    None
                }
            });
            let Some(fact_idx) = fact_idx else {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::CheckerBoundaryViolation {
                        name: name.clone(),
                        reason:
                            "closure_capture_facts has no unambiguous entry for captured HIR binding"
                                .to_string(),
                    },
                    span.clone(),
                    "closure capture reached HIR without checker materialization metadata",
                ));
                continue;
            };
            let fact = remaining_facts.remove(fact_idx);
            let ty = match ResolvedTy::from_ty(&fact.ty) {
                Ok(ty) => ty,
                Err(err) => {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::CheckerBoundaryViolation {
                            name: fact.name.clone(),
                            reason: err.to_string(),
                        },
                        span.clone(),
                        "closure capture type failed checker-boundary conversion",
                    ));
                    continue;
                }
            };
            captures.push(HirClosureCapture {
                binding,
                name,
                ty,
                mode: fact.mode,
                is_send: fact.is_send,
            });
        }

        for fact in remaining_facts {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::CheckerBoundaryViolation {
                    name: fact.name,
                    reason:
                        "checker reported a capture that HIR name resolution did not materialize"
                            .to_string(),
                },
                span.clone(),
                "closure capture metadata and lowered HIR body disagree",
            ));
        }

        captures
    }

    /// Lower an `Expr::SpawnLambdaActor` to an
    /// `HirExprKind::SpawnLambdaActor` with a resolved capture set.
    ///
    /// The lambda body lowers inside a fresh scope so the parameter
    /// bindings shadow outer names; after the body is built the
    /// `current_actor_self` field tells us whether the body lives
    /// under a `let <name> = actor |..| { .. }` forward-bind. The
    /// capture walker then collects every `BindingRef { resolved:
    /// Binding(id) }` whose `id` refers to a binding from an outer
    /// scope (not a parameter introduced by this lambda) and
    /// classifies the strength: `id == current_actor_self.0` → Weak
    /// (recursive self-dispatch, §5.9 ratification 2), else → Strong.
    ///
    /// The HIR `expr.ty` is the Duplex<Msg, Reply> handle type. The
    /// MIR producer wires this directly into a
    /// `Place::LambdaActorHandle` whose drop selects
    /// `DropKind::LambdaActorRelease`.
    fn lower_spawn_lambda_actor(
        &mut self,
        params: &[LambdaParam],
        return_type: Option<&Spanned<TypeExpr>>,
        body: &Spanned<Expr>,
    ) -> (HirExprKind, ResolvedTy) {
        let actor_ty = self.actor_lambda_duplex_ty(params, return_type);
        let reply_ty = match &actor_ty {
            ResolvedTy::Named { args, .. } if args.len() == 2 => args[1].clone(),
            _ => ResolvedTy::Unit,
        };
        // Lower params + body inside a new scope. Track the parameter
        // BindingIds so the capture walker can exclude them (params
        // are intra-lambda bindings, not captures from the enclosing
        // scope).
        self.push_scope();
        let mut hir_params: Vec<HirBinding> = Vec::with_capacity(params.len());
        let mut param_ids: std::collections::HashSet<BindingId> =
            std::collections::HashSet::with_capacity(params.len());
        for param in params {
            let ty = param
                .ty
                .as_ref()
                .map_or(ResolvedTy::Unit, |ann| self.lower_type(ann));
            let binding = self.bind(param.name.clone(), ty, false, 0..0);
            param_ids.insert(binding.id);
            hir_params.push(binding);
        }
        // Lexically scope `current_actor_self` to THIS lambda body. If the
        // caller (`lower_stmt`'s let-pre-bind path) set it before invoking
        // `lower_expr`, that value is this lambda's self-id; otherwise this
        // lambda is in expression position (anonymous) and has no self-id.
        // Take the value out for the duration of the body walk so any
        // nested actor-lambda lowered from within `body` does not inherit
        // it — nested anonymous lambdas would otherwise misclassify
        // captures of THIS lambda's enclosing-scope bindings as Weak.
        // Restored before `collect_lambda_captures` so the capture-strength
        // classifier sees the correct self-id, and restored to the caller's
        // prior value on exit.
        let my_self_id = self.current_actor_self.take();
        let lowered_body = self.lower_expr(body, IntentKind::Read);
        self.current_actor_self = my_self_id;
        self.pop_scope();
        let captures = self.collect_lambda_captures(&lowered_body, &param_ids);
        (
            HirExprKind::SpawnLambdaActor {
                params: hir_params,
                reply_ty,
                body: Box::new(lowered_body),
                captures,
            },
            actor_ty,
        )
    }

    /// Walk a lowered lambda body collecting `BindingRef`s that resolve
    /// to bindings from the enclosing scope. A reference is a capture
    /// when its resolved binding id is not in `param_ids` (the lambda's
    /// own parameters). Each unique binding is classified Weak when
    /// its id matches `current_actor_self.0` (the let-name pre-bound
    /// before body lowering) and Strong otherwise.
    ///
    /// Duplicate references to the same binding produce a single
    /// capture entry — codegen needs the runtime to register the
    /// captured handle once per binding, not once per use site.
    fn collect_lambda_captures(
        &self,
        body: &HirExpr,
        param_ids: &std::collections::HashSet<BindingId>,
    ) -> Vec<HirLambdaCapture> {
        let mut seen: std::collections::HashSet<BindingId> = std::collections::HashSet::new();
        let mut captures: Vec<HirLambdaCapture> = Vec::new();
        let self_id = self.current_actor_self.as_ref().map(|(id, _)| *id);
        collect_captures_walk(body, param_ids, &mut seen, &mut captures, self_id);
        captures
    }

    /// Recognise the sealed-form discriminator for a `select` arm
    /// source expression. Emits a `SelectArmNotSealedForm` /
    /// `SelectStreamNextSurface` / `SelectStreamNextArity` diagnostic
    /// on miss and returns a placeholder `AfterTimer` arm kind (the
    /// callers tolerate the placeholder because the diagnostic has
    /// already been emitted; MIR lowering treats any select with HIR
    /// diagnostics as fail-closed downstream).
    fn recognize_sealed_arm_source(&mut self, source: &Spanned<Expr>) -> HirSelectArmKind {
        let span = source.1.clone();
        match &source.0 {
            // Form 1: `next(<stream-expr>)` — a call where the callee
            // is the bare identifier `next`. `next` is not a lexer
            // keyword; the sealed-form discriminator is the callee
            // name.
            Expr::Call { function, args, .. } => {
                if let Expr::Identifier(name) = &function.0 {
                    if name == "next" {
                        if args.len() != 1 {
                            self.diagnostics.push(HirDiagnostic::new(
                                HirDiagnosticKind::SelectStreamNextArity {
                                    arg_count: args.len(),
                                },
                                span.clone(),
                                "next(<stream>) takes exactly one argument",
                            ));
                            return HirSelectArmKind::StreamNext {
                                stream: Box::new(
                                    self.unsupported_expr(span, "stream-next arity mismatch"),
                                ),
                            };
                        }
                        let stream = self.lower_expr(args[0].expr(), IntentKind::Read);
                        return HirSelectArmKind::StreamNext {
                            stream: Box::new(stream),
                        };
                    }
                }
                // Some other function call — not a sealed form.
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::SelectArmNotSealedForm {
                        source_shape: "function call".into(),
                    },
                    span.clone(),
                    "select arm source must be next(...), an actor method call, or `await <task>`",
                ));
                HirSelectArmKind::StreamNext {
                    stream: Box::new(self.unsupported_expr(span, "non-sealed arm source")),
                }
            }
            // Form 2: `<actor>.<method>(<args>)` — method call on an
            // actor expression. Per HEW-SPEC-2026 §4.11.1 this is the
            // actor-ask arm. `ask` is reserved as a future syntactic
            // marker (see HEW-FUTURE) but is not lexer-recognised in
            // edition 2026; the sealed-form discriminator is the
            // method-call surface.
            Expr::MethodCall {
                receiver,
                method,
                args,
            } => {
                let actor = self.lower_expr(receiver, IntentKind::Read);
                let lowered_args: Vec<HirExpr> = args
                    .iter()
                    .map(|arg| self.lower_expr(arg.expr(), IntentKind::Read))
                    .collect();
                HirSelectArmKind::ActorAsk {
                    actor: Box::new(actor),
                    method: method.clone(),
                    args: lowered_args,
                }
            }
            // Form 3: `await <task-expr>` — explicit await keyword.
            Expr::Await(task_expr) => {
                let task = self.lower_expr(task_expr, IntentKind::Read);
                HirSelectArmKind::TaskAwait {
                    task: Box::new(task),
                }
            }
            // Form 4 (arm-position): `after <duration>` written as an
            // arm source rather than the dedicated `timeout` field.
            // Recognised here so the `lower_select` multiple-after check
            // can fire; the duplicate check in `lower_select` emits the
            // diagnostic when this arm coexists with another after arm.
            Expr::Timeout { duration, .. } => {
                let dur = self.lower_expr(duration, IntentKind::Read);
                HirSelectArmKind::AfterTimer {
                    duration: Box::new(dur),
                }
            }
            // Method-call dressed up as `stream.next()` — sealed
            // surface is `next(stream)`. Diagnose specifically so the
            // user can fix the form.
            // (Already handled by the MethodCall arm above as a
            // generic actor-ask. The dedicated diagnostic for the
            // `.next()` shape would shadow the actor-ask recognition;
            // we keep the more general actor-ask interpretation and
            // rely on the SelectStreamNextSurface diagnostic only if
            // we later choose to special-case it. For now, `s.next()`
            // is recognised as an actor-ask of the `next` method on
            // `s`, which is the lower-noise default.)
            other => {
                let shape = describe_select_source_shape(other);
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::SelectArmNotSealedForm {
                        source_shape: shape,
                    },
                    span.clone(),
                    "select arm source must be next(...), an actor method call, or `await <task>`",
                ));
                HirSelectArmKind::StreamNext {
                    stream: Box::new(self.unsupported_expr(span, "non-sealed arm source")),
                }
            }
        }
    }

    fn lower_literal(lit: &Literal) -> (HirExprKind, ResolvedTy) {
        match lit {
            Literal::Integer { value, .. } => (
                HirExprKind::Literal(HirLiteral::Integer(*value)),
                ResolvedTy::I64,
            ),
            Literal::Float(value) => (
                HirExprKind::Literal(HirLiteral::Float(*value)),
                ResolvedTy::F64,
            ),
            Literal::String(value) => (
                HirExprKind::Literal(HirLiteral::String(value.clone())),
                ResolvedTy::String,
            ),
            Literal::Bool(value) => (
                HirExprKind::Literal(HirLiteral::Bool(*value)),
                ResolvedTy::Bool,
            ),
            Literal::Char(value) => (
                HirExprKind::Literal(HirLiteral::Char(*value)),
                ResolvedTy::Char,
            ),
            Literal::Duration(value) => (
                HirExprKind::Literal(HirLiteral::Duration(*value)),
                ResolvedTy::Duration,
            ),
        }
    }

    fn lower_identifier(
        &mut self,
        name: &str,
        span: std::ops::Range<usize>,
    ) -> (HirExprKind, ResolvedTy) {
        if let Some((id, ty)) = self.lookup(name) {
            (
                HirExprKind::BindingRef {
                    name: name.to_string(),
                    resolved: ResolvedRef::Binding(id),
                },
                ty,
            )
        } else if let Some(entry) = self.fn_registry.get(name) {
            // Known function item — expose as a function-typed reference so
            // callers can extract the return type from the call expression.
            let fn_ty = ResolvedTy::Function {
                params: entry.param_tys.clone(),
                ret: Box::new(entry.return_ty.clone()),
            };
            let id = entry.id;
            (
                HirExprKind::BindingRef {
                    name: name.to_string(),
                    resolved: ResolvedRef::Item(id),
                },
                fn_ty,
            )
        } else {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::UnresolvedSymbol {
                    name: name.to_string(),
                },
                span,
                "identifier has no binding in resolved HIR",
            ));
            (
                HirExprKind::BindingRef {
                    name: name.to_string(),
                    resolved: ResolvedRef::Unresolved,
                },
                ResolvedTy::Unit,
            )
        }
    }

    fn lower_type(&mut self, ty: &Spanned<TypeExpr>) -> ResolvedTy {
        match &ty.0 {
            TypeExpr::Named { name, type_args } => {
                let args = type_args
                    .as_ref()
                    .map(|args| args.iter().map(|arg| self.lower_type(arg)).collect())
                    .unwrap_or_default();
                match name.as_str() {
                    "i8" => ResolvedTy::I8,
                    "i16" => ResolvedTy::I16,
                    "i32" => ResolvedTy::I32,
                    // `int` is the user-facing alias for `i64` across the
                    // type checker (hew-types::stdlib_loader maps `int` to
                    // Ty::I64; ty.rs's alias table lists `int` as a synonym
                    // for `i64`). The HIR lowering must match — otherwise
                    // integer literals (which lower to I64) cannot be
                    // returned through a function typed `int` without an
                    // explicit cast. Aligns with hew-types ground truth.
                    "i64" | "int" => ResolvedTy::I64,
                    "u8" => ResolvedTy::U8,
                    "u16" => ResolvedTy::U16,
                    "u32" => ResolvedTy::U32,
                    "u64" => ResolvedTy::U64,
                    // Platform-sized integers: distinct from fixed-width
                    // int/uint. Codegen branches on target: 32-bit for
                    // wasm32, 64-bit for native (B-D1 / Q42 ratification).
                    "isize" => ResolvedTy::Isize,
                    "usize" => ResolvedTy::Usize,
                    "f32" => ResolvedTy::F32,
                    "f64" | "float" => ResolvedTy::F64,
                    "bool" | "Bool" => ResolvedTy::Bool,
                    "char" | "Char" => ResolvedTy::Char,
                    "string" | "str" => ResolvedTy::String,
                    "duration" => ResolvedTy::Duration,
                    "Bytes" => ResolvedTy::Bytes,
                    "Unit" | "()" => ResolvedTy::Unit,
                    // `Task` is a compiler-internal value class with no user-source
                    // syntax. Writing `Task<T>` in any annotation position is a
                    // compile error. Use `fork name = call(...)` to obtain a task
                    // handle. (TI-5 structural enforcement.)
                    "Task" => {
                        self.diagnostics.push(HirDiagnostic::new(
                            HirDiagnosticKind::TaskNotNameable,
                            ty.1.clone(),
                            "`Task<T>` is a compiler-internal type and cannot be written \
                             in source. Use `fork name = call(...)` to create a task handle.",
                        ));
                        ResolvedTy::Unit
                    }
                    _ => ResolvedTy::Named {
                        name: name.clone(),
                        args,
                    },
                }
            }
            TypeExpr::Infer => {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::UnresolvedInferenceVar,
                    ty.1.clone(),
                    "inferred type reached resolved HIR boundary",
                ));
                ResolvedTy::Unit
            }
            TypeExpr::Tuple(elems) => {
                ResolvedTy::Tuple(elems.iter().map(|elem| self.lower_type(elem)).collect())
            }
            TypeExpr::Array { element, size } => {
                ResolvedTy::Array(Box::new(self.lower_type(element)), *size)
            }
            TypeExpr::Slice(elem) => ResolvedTy::Slice(Box::new(self.lower_type(elem))),
            TypeExpr::Function {
                params,
                return_type,
            } => ResolvedTy::Function {
                params: params.iter().map(|param| self.lower_type(param)).collect(),
                ret: Box::new(self.lower_type(return_type)),
            },
            TypeExpr::Pointer {
                is_mutable,
                pointee,
            } => ResolvedTy::Pointer {
                is_mutable: *is_mutable,
                pointee: Box::new(self.lower_type(pointee)),
            },
            _ => {
                self.unsupported(ty.1.clone(), "type-expression", "slice-2");
                ResolvedTy::Unit
            }
        }
    }

    fn binary_ty(op: BinaryOp, left: &ResolvedTy, right: &ResolvedTy) -> ResolvedTy {
        match op {
            BinaryOp::Equal
            | BinaryOp::NotEqual
            | BinaryOp::Less
            | BinaryOp::LessEqual
            | BinaryOp::Greater
            | BinaryOp::GreaterEqual
            | BinaryOp::And
            | BinaryOp::Or => ResolvedTy::Bool,
            BinaryOp::Add if left == &ResolvedTy::String || right == &ResolvedTy::String => {
                ResolvedTy::String
            }
            // Wrapping ops (WrappingAdd/WrappingSub/WrappingMul) fall through
            // to the wildcard: they return the left operand's integer type,
            // same as any other integer arithmetic op. The type checker has
            // already enforced integer-only operands.
            _ => left.clone(),
        }
    }

    /// Lower `receiver.method(args)` using the checker's `method_call_rewrites` side-table.
    ///
    /// Fail-closed per `checker-output-boundary` (LESSONS P0): a missing entry for
    /// this call site's span is a hard diagnostic — HIR never re-infers the runtime
    /// symbol from the receiver type.  Only `RewriteToFunction` is recognised here;
    /// other rewrite variants are rejected as unsupported (they target the C++/MLIR
    /// pipeline, not the Rust MIR pipeline).
    #[allow(
        clippy::too_many_lines,
        reason = "single linear lowering path with three exclusive branches \
                  (dyn-method dispatch / dyn-receiver fail-closed / legacy \
                  rewrite); splitting would scatter related fail-closed \
                  diagnostics across helpers"
    )]
    fn lower_method_call(
        &mut self,
        receiver: &Spanned<Expr>,
        method: &str,
        args: &[hew_parser::ast::CallArg],
        span: Span,
    ) -> (HirExprKind, ResolvedTy) {
        let key = SpanKey::from(&span);
        // `dyn Trait` receivers take precedence: the checker's
        // `dyn_trait_method_calls` side-table pins the trait/method/slot
        // resolution authoritatively, and these calls do NOT have a
        // `method_call_rewrites` entry (a direct-call rewrite would
        // collapse the dispatch indirection that the vtable provides).
        if let Some(dyn_call) = self.dyn_trait_method_calls.get(&key).cloned() {
            let lowered_receiver = self.lower_expr(receiver, IntentKind::Read);
            let lowered_args: Vec<HirExpr> = args
                .iter()
                .map(|arg| self.lower_expr(arg.expr(), IntentKind::Read))
                .collect();
            // Result type comes from the checker's expr_types side-table
            // (the call's full span). Fail-closed if absent or poisoned.
            let ret_ty = self
                .expr_types
                .get(&key)
                .cloned()
                .and_then(|ty| ResolvedTy::from_ty(&ty).ok())
                .unwrap_or(ResolvedTy::Unit);
            return (
                HirExprKind::CallDynMethod {
                    receiver: Box::new(lowered_receiver),
                    trait_name: dyn_call.trait_name,
                    method_name: dyn_call.method_name,
                    slot: dyn_call.slot,
                    args: lowered_args,
                    ret_ty: ret_ty.clone(),
                },
                ret_ty,
            );
        }
        // Receiver typed as `Ty::TraitObject` but no side-table entry:
        // fail-closed per `checker-output-boundary`. The checker MUST
        // populate `dyn_trait_method_calls` for every accepted call on
        // a trait-object receiver; missing entry is a hard diagnostic.
        if let Some(receiver_ty) = self.expr_types.get(&SpanKey::from(&receiver.1)) {
            if matches!(receiver_ty, hew_types::Ty::TraitObject { .. }) {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::TraitObjectMethodNoSideTableEntry {
                        method: method.to_string(),
                    },
                    span.clone(),
                    "method call on `dyn Trait` receiver has no \
                     dyn_trait_method_calls side-table entry; the \
                     checker must record one before HIR lowering",
                ));
                return (
                    HirExprKind::Unsupported(format!(
                        "method call `.{method}` on dyn-trait with no side-table entry"
                    )),
                    ResolvedTy::Unit,
                );
            }
        }
        let rewrite = self.method_call_rewrites.get(&key).cloned();
        match rewrite {
            Some(MethodCallRewrite::RewriteToFunction { c_symbol }) => {
                // Lower receiver + args, then prepend receiver as first argument.
                let lowered_receiver = self.lower_expr(receiver, IntentKind::Read);
                let mut lowered_args = vec![lowered_receiver];
                for arg in args {
                    lowered_args.push(self.lower_expr(arg.expr(), IntentKind::Read));
                }
                // Synthetic callee: a runtime-symbol reference.  The function type
                // uses `Unit` return (runtime send/recv return unit in the Rust MIR
                // pipeline; future slices thread expr_types for richer return types).
                // `params` is empty — the call arg list carries the real args.
                let callee_ty = ResolvedTy::Function {
                    params: Vec::new(),
                    ret: Box::new(ResolvedTy::Unit),
                };
                let callee = HirExpr {
                    node: self.ids.node(),
                    site: self.ids.site(),
                    value_class: ValueClass::PersistentShare,
                    ty: callee_ty,
                    intent: IntentKind::Read,
                    kind: HirExprKind::BindingRef {
                        name: c_symbol,
                        resolved: ResolvedRef::Unresolved,
                    },
                    span: span.clone(),
                };
                (
                    HirExprKind::Call {
                        callee: Box::new(callee),
                        args: lowered_args,
                    },
                    ResolvedTy::Unit,
                )
            }
            Some(
                MethodCallRewrite::RewriteModuleQualifiedToFunction { .. }
                | MethodCallRewrite::DeferToLowering,
            ) => {
                // These rewrite variants target the C++/MLIR pipeline and are
                // not consumed by the Rust MIR pipeline.  Fail-closed.
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::CutoverUnsupported {
                        construct: format!("method-call rewrite variant for `.{method}`"),
                        slice_target: "mir-pipeline".to_string(),
                    },
                    span,
                    "this method-call rewrite variant is not supported in the Rust MIR pipeline",
                ));
                (
                    HirExprKind::Unsupported(format!(
                        "unsupported rewrite variant for method `{method}`"
                    )),
                    ResolvedTy::Unit,
                )
            }
            None => {
                // No rewrite entry — fail closed.  Do not re-infer from the receiver type.
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::MethodCallNoRewrite {
                        method: method.to_string(),
                    },
                    span,
                    "no checker-produced rewrite entry for this method call; \
                     typecheck must record a rewrite before HIR lowering",
                ));
                (
                    HirExprKind::Unsupported(format!(
                        "method call `.{method}` has no rewrite entry"
                    )),
                    ResolvedTy::Unit,
                )
            }
        }
    }

    fn pattern_name(&mut self, pattern: &Spanned<Pattern>) -> Option<String> {
        if let Pattern::Identifier(name) = &pattern.0 {
            Some(name.clone())
        } else {
            self.unsupported(pattern.1.clone(), "pattern", "slice-2");
            None
        }
    }

    fn bind(
        &mut self,
        name: String,
        ty: ResolvedTy,
        mutable: bool,
        span: std::ops::Range<usize>,
    ) -> HirBinding {
        let id = self.ids.binding();
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.clone(), (id, ty.clone(), span.clone()));
        }
        HirBinding {
            id,
            name,
            ty,
            mutable,
            span,
        }
    }

    fn lookup(&self, name: &str) -> Option<(BindingId, ResolvedTy)> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get(name).map(|(id, ty, _)| (*id, ty.clone())))
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn lower_cancellation_clause_block(&mut self, block: &Block) -> HirBlock {
        let saved_scope_depth = self.scope_depth;
        self.scope_depth = 0;
        let lowered = self.lower_block(block, &ResolvedTy::Unit);
        self.scope_depth = saved_scope_depth;
        lowered
    }

    fn unsupported(
        &mut self,
        span: std::ops::Range<usize>,
        construct: impl Into<String>,
        slice_target: impl Into<String>,
    ) {
        self.diagnostics.push(HirDiagnostic::new(
            HirDiagnosticKind::CutoverUnsupported {
                construct: construct.into(),
                slice_target: slice_target.into(),
            },
            span,
            "",
        ));
    }

    fn unsupported_expr(
        &mut self,
        span: std::ops::Range<usize>,
        note: impl Into<String>,
    ) -> HirExpr {
        HirExpr {
            node: self.ids.node(),
            site: self.ids.site(),
            ty: ResolvedTy::Unit,
            value_class: ValueClass::BitCopy,
            intent: IntentKind::Unknown,
            kind: HirExprKind::Unsupported(note.into()),
            span,
        }
    }

    /// Lower the body block of a `scope{}` expression. This is separate from
    /// `lower_block` because statements inside a scope body follow different
    /// rules:
    ///
    /// - `Stmt::Expression(Expr::Call{..})` → `SpawnedCall` (TI-1)
    /// - `Stmt::Expression(Expr::ForkChild { binding: Some(name), expr })` →
    ///   `HirStmtKind::Let` with a `Task<T>` typed binding (TI-2)
    /// - `Stmt::Expression(Expr::Await(..))` → `AwaitTask` (TI-4)
    /// - All other statements lower normally, including nested `scope{}` blocks.
    ///
    /// The caller is responsible for setting `scope_depth` before calling this
    /// function and restoring it after.
    #[allow(
        clippy::too_many_lines,
        reason = "single match on scope-body statement variants; splitting would hurt readability"
    )]
    fn lower_scope_block(&mut self, block: &Block) -> HirBlock {
        self.push_scope();
        let scope = self.ids.scope();
        let mut statements = Vec::new();

        for (stmt, span) in &block.stmts {
            let hir_stmt = match stmt {
                // `fork name = call(...)` inside a scope body → TI-2: typed Task<T> binding.
                Stmt::Expression(expr)
                    if matches!(
                        &expr.0,
                        Expr::ForkChild {
                            binding: Some(_),
                            ..
                        }
                    ) =>
                {
                    if let Expr::ForkChild {
                        binding: Some(binding_name),
                        expr: child_expr,
                    } = &expr.0
                    {
                        // The child expression must be a call; any other form is rejected.
                        if matches!(&child_expr.0, Expr::Call { .. }) {
                            // Lower the call synchronously first to get the return type,
                            // then wrap in SpawnedCall + Task<T>.
                            let call_hir = self.lower_expr(child_expr, IntentKind::Consume);
                            let call_ret_ty = call_hir.ty.clone();
                            let task_ty = ResolvedTy::Task(Box::new(call_ret_ty));

                            // Destructure call_hir.kind once to extract callee + args.
                            let HirExprKind::Call { callee, args } = call_hir.kind else {
                                unreachable!("just verified Call shape above")
                            };

                            // Re-wrap as a SpawnedCall node with Task<T> type.
                            let spawned = HirExpr {
                                node: self.ids.node(),
                                site: self.ids.site(),
                                value_class: ValueClass::Linear, // Task handles are linear (consume-once).
                                ty: task_ty.clone(),
                                intent: IntentKind::Consume,
                                kind: HirExprKind::SpawnedCall {
                                    callee,
                                    args,
                                    task_ty: task_ty.clone(),
                                },
                                span: child_expr.1.clone(),
                            };

                            // Bind the name with Task<T> type in the current scope.
                            let binding =
                                self.bind(binding_name.clone(), task_ty, false, span.clone());
                            HirStmt {
                                node: self.ids.node(),
                                kind: HirStmtKind::Let(binding, Some(spawned)),
                                span: span.clone(),
                            }
                        } else {
                            self.diagnostics.push(HirDiagnostic::new(
                                HirDiagnosticKind::ForkChildNotACall,
                                child_expr.1.clone(),
                                "`fork name = expr` requires a call expression as the \
                                 right-hand side; other expression forms cannot be spawned as tasks",
                            ));
                            // Emit an unsupported stmt and continue rather than stopping.
                            self.unsupported(span.clone(), "fork-child-non-call", "slice-2");
                            HirStmt {
                                node: self.ids.node(),
                                kind: HirStmtKind::Expr(
                                    self.unsupported_expr(span.clone(), "fork child non-call"),
                                ),
                                span: span.clone(),
                            }
                        }
                    } else {
                        unreachable!("pattern guard ensures this branch")
                    }
                }

                // `fork name = call(...)` without a binding name (bare ForkChild with no
                // name, e.g. `scope { fork = expr }` — the grammar produces binding: None).
                // Lower the child expression as a plain SpawnedCall; the result is not bound.
                Stmt::Expression(expr)
                    if matches!(&expr.0, Expr::ForkChild { binding: None, .. }) =>
                {
                    if let Expr::ForkChild {
                        binding: None,
                        expr: child_expr,
                    } = &expr.0
                    {
                        if matches!(&child_expr.0, Expr::Call { .. }) {
                            let spawned = self.lower_spawned_call(child_expr);
                            HirStmt {
                                node: self.ids.node(),
                                kind: HirStmtKind::Expr(spawned),
                                span: span.clone(),
                            }
                        } else {
                            self.diagnostics.push(HirDiagnostic::new(
                                HirDiagnosticKind::ForkChildNotACall,
                                child_expr.1.clone(),
                                "`fork = expr` requires a call expression",
                            ));
                            self.unsupported(span.clone(), "fork-child-non-call", "slice-2");
                            HirStmt {
                                node: self.ids.node(),
                                kind: HirStmtKind::Expr(
                                    self.unsupported_expr(span.clone(), "fork child non-call"),
                                ),
                                span: span.clone(),
                            }
                        }
                    } else {
                        unreachable!("pattern guard ensures this branch")
                    }
                }

                // All other statements lower normally (including regular calls,
                // let bindings, nested scope{} blocks, etc.). Inside scope depth,
                // `lower_stmt` will already handle statement-expression calls as
                // SpawnedCall nodes via TI-1 (the scope_depth > 0 path in lower_stmt).
                _ => self.lower_stmt(stmt, span.clone(), ResolvedTy::Unit),
            };
            statements.push(hir_stmt);
        }

        let tail = block
            .trailing_expr
            .as_ref()
            .map(|expr| Box::new(self.lower_expr(expr, IntentKind::Read)));
        let ty = tail
            .as_ref()
            .map_or(ResolvedTy::Unit, |expr| expr.ty.clone());
        self.pop_scope();

        HirBlock {
            node: self.ids.node(),
            scope,
            statements,
            tail,
            ty,
            span: 0..0,
        }
    }

    /// Lower a call expression appearing as a statement inside a `scope{}` body
    /// as a child-task spawn (TI-1). The resulting `HirExpr` has kind
    /// `SpawnedCall` and type `Task<call_return_ty>`.
    fn lower_spawned_call(&mut self, expr: &Spanned<Expr>) -> HirExpr {
        let span = expr.1.clone();
        // Lower the call normally to resolve the callee and argument types.
        let call_hir = self.lower_expr(expr, IntentKind::Consume);
        let call_ret_ty = call_hir.ty.clone();
        let task_ty = ResolvedTy::Task(Box::new(call_ret_ty));

        let HirExprKind::Call { callee, args } = call_hir.kind else {
            // Should not happen: caller verified the expression is a Call.
            return self.unsupported_expr(span, "lower_spawned_call on non-call");
        };

        HirExpr {
            node: self.ids.node(),
            site: self.ids.site(),
            value_class: ValueClass::Linear, // Task handles are linear (consume-once).
            ty: task_ty.clone(),
            intent: IntentKind::Consume,
            kind: HirExprKind::SpawnedCall {
                callee,
                args,
                task_ty,
            },
            span,
        }
    }
}

// ── Lambda-actor capture walker ─────────────────────────────────────────────

/// Recursive walk over a lowered actor-lambda body collecting captures.
///
/// A capture is any `HirExprKind::BindingRef { resolved: Binding(id) }`
/// whose `id` is NOT one of the lambda's own parameter bindings. The
/// first occurrence of each capture is recorded with a strength
/// classifier — `Weak` iff the id matches the lambda's let-binding id
/// passed in `self_id` (the forward-bound recursive-self case, §5.9
/// ratification 2), `Strong` otherwise. Subsequent references to the
/// same binding are skipped — codegen wires one runtime capture per
/// binding, not one per use site.
///
/// The walk is exhaustive over `HirExprKind` variants; nested lambdas
/// (an actor lambda inside another actor lambda's body) are NOT
/// descended into — their captures belong to the inner lambda's
/// frame and are reported separately when that lambda's own
/// `lower_spawn_lambda_actor` call ran. The nested lambda's appearance
/// in the outer body's capture set, if any, would come from the
/// outer body referencing a name that the inner lambda also referenced;
/// but `BindingRef` lives only in the outer body's expression tree, so
/// this falls out naturally from not descending into the inner body.
#[expect(
    clippy::too_many_lines,
    reason = "single-pass HIR walker; one arm per HirExprKind variant by design"
)]
fn collect_captures_walk(
    expr: &HirExpr,
    param_ids: &std::collections::HashSet<BindingId>,
    seen: &mut std::collections::HashSet<BindingId>,
    captures: &mut Vec<HirLambdaCapture>,
    self_id: Option<BindingId>,
) {
    match &expr.kind {
        HirExprKind::BindingRef {
            name,
            resolved: ResolvedRef::Binding(id),
        } => {
            // Parameters of the current lambda are intra-frame
            // bindings, never captures from the enclosing scope.
            if param_ids.contains(id) {
                return;
            }
            if !seen.insert(*id) {
                return;
            }
            let kind = if Some(*id) == self_id {
                HirCaptureKind::Weak
            } else {
                HirCaptureKind::Strong
            };
            captures.push(HirLambdaCapture {
                binding: *id,
                name: name.clone(),
                kind,
            });
        }
        // Empty-body terminals: nothing to walk.
        //   - BindingRef without a resolved binding (Item / Unresolved):
        //     does not produce a capture from the enclosing scope.
        //   - Literal: no sub-expressions.
        //   - SpawnLambdaActor (nested): its captures belong to its own
        //     frame and were classified when the inner lambda lowered.
        //   - Unsupported: nothing to walk.
        HirExprKind::BindingRef { .. }
        | HirExprKind::Literal(_)
        | HirExprKind::SpawnLambdaActor { .. }
        | HirExprKind::Closure { .. }
        | HirExprKind::Unsupported(_) => {}
        HirExprKind::Binary { left, right, .. } | HirExprKind::IdentityCompare { left, right } => {
            collect_captures_walk(left, param_ids, seen, captures, self_id);
            collect_captures_walk(right, param_ids, seen, captures, self_id);
        }
        HirExprKind::Call { callee, args } | HirExprKind::SpawnedCall { callee, args, .. } => {
            collect_captures_walk(callee, param_ids, seen, captures, self_id);
            for arg in args {
                collect_captures_walk(arg, param_ids, seen, captures, self_id);
            }
        }
        HirExprKind::Block(block)
        | HirExprKind::Scope { body: block }
        | HirExprKind::ForkBlock { body: block, .. } => {
            collect_captures_walk_block(block, param_ids, seen, captures, self_id);
        }
        HirExprKind::ScopeDeadline { duration, body } => {
            collect_captures_walk(duration, param_ids, seen, captures, self_id);
            collect_captures_walk_block(body, param_ids, seen, captures, self_id);
        }
        HirExprKind::If {
            condition,
            then_expr,
            else_expr,
        } => {
            collect_captures_walk(condition, param_ids, seen, captures, self_id);
            collect_captures_walk(then_expr, param_ids, seen, captures, self_id);
            if let Some(else_expr) = else_expr {
                collect_captures_walk(else_expr, param_ids, seen, captures, self_id);
            }
        }
        HirExprKind::StructInit { fields, base, .. } => {
            for (_, field) in fields {
                collect_captures_walk(field, param_ids, seen, captures, self_id);
            }
            if let Some(base) = base {
                collect_captures_walk(base, param_ids, seen, captures, self_id);
            }
        }
        HirExprKind::FieldAccess { object, .. } => {
            collect_captures_walk(object, param_ids, seen, captures, self_id);
        }
        HirExprKind::AwaitTask { binding_id, .. } => {
            // The awaited task handle is captured from the enclosing
            // scope unless it is one of the lambda's own params.
            if param_ids.contains(binding_id) || !seen.insert(*binding_id) {
                return;
            }
            let kind = if Some(*binding_id) == self_id {
                HirCaptureKind::Weak
            } else {
                HirCaptureKind::Strong
            };
            captures.push(HirLambdaCapture {
                binding: *binding_id,
                // The await arm doesn't carry the binding's surface
                // name on its own — reach for the binding name via
                // the binding_name slot.
                name: String::new(),
                kind,
            });
        }
        HirExprKind::Select(select) => {
            for arm in &select.arms {
                match &arm.kind {
                    HirSelectArmKind::StreamNext { stream } => {
                        collect_captures_walk(stream, param_ids, seen, captures, self_id);
                    }
                    HirSelectArmKind::ActorAsk { actor, args, .. } => {
                        collect_captures_walk(actor, param_ids, seen, captures, self_id);
                        for arg in args {
                            collect_captures_walk(arg, param_ids, seen, captures, self_id);
                        }
                    }
                    HirSelectArmKind::TaskAwait { task } => {
                        collect_captures_walk(task, param_ids, seen, captures, self_id);
                    }
                    HirSelectArmKind::AfterTimer { duration } => {
                        collect_captures_walk(duration, param_ids, seen, captures, self_id);
                    }
                }
                collect_captures_walk(&arm.body, param_ids, seen, captures, self_id);
            }
        }
        HirExprKind::TupleIndex { tuple, .. } => {
            collect_captures_walk(tuple, param_ids, seen, captures, self_id);
        }
        HirExprKind::Index { container, index } => {
            collect_captures_walk(container, param_ids, seen, captures, self_id);
            collect_captures_walk(index, param_ids, seen, captures, self_id);
        }
        HirExprKind::Slice {
            container,
            start,
            end,
            inclusive: _,
        } => {
            collect_captures_walk(container, param_ids, seen, captures, self_id);
            if let Some(s) = start {
                collect_captures_walk(s, param_ids, seen, captures, self_id);
            }
            if let Some(e) = end {
                collect_captures_walk(e, param_ids, seen, captures, self_id);
            }
        }
        HirExprKind::CoerceToDynTrait { value, .. } => {
            collect_captures_walk(value, param_ids, seen, captures, self_id);
        }
        HirExprKind::CallDynMethod { receiver, args, .. } => {
            collect_captures_walk(receiver, param_ids, seen, captures, self_id);
            for arg in args {
                collect_captures_walk(arg, param_ids, seen, captures, self_id);
            }
        }
    }
}

#[allow(
    clippy::too_many_lines,
    reason = "single exhaustive walker over HirExprKind variants; splitting risks traversal gaps"
)]
fn collect_general_closure_captures_walk(
    expr: &HirExpr,
    outer_bindings: &HashMap<BindingId, OuterClosureBinding>,
    seen: &mut HashSet<BindingId>,
    captures: &mut Vec<ClosureCaptureCandidate>,
) {
    match &expr.kind {
        HirExprKind::BindingRef {
            name,
            resolved: ResolvedRef::Binding(id),
        } if outer_bindings.contains_key(id) => {
            if seen.insert(*id) {
                let span = outer_bindings
                    .get(id)
                    .map(|(_, _, span)| span.clone())
                    .unwrap_or_default();
                captures.push((*id, name.clone(), span));
            }
        }
        HirExprKind::Closure {
            captures: nested, ..
        } => {
            for capture in nested {
                if outer_bindings.contains_key(&capture.binding) && seen.insert(capture.binding) {
                    let span = outer_bindings
                        .get(&capture.binding)
                        .map(|(_, _, span)| span.clone())
                        .unwrap_or_default();
                    captures.push((capture.binding, capture.name.clone(), span));
                }
            }
        }
        HirExprKind::BindingRef { .. }
        | HirExprKind::Literal(_)
        | HirExprKind::SpawnLambdaActor { .. }
        | HirExprKind::Unsupported(_) => {}
        HirExprKind::Binary { left, right, .. } | HirExprKind::IdentityCompare { left, right } => {
            collect_general_closure_captures_walk(left, outer_bindings, seen, captures);
            collect_general_closure_captures_walk(right, outer_bindings, seen, captures);
        }
        HirExprKind::Call { callee, args } | HirExprKind::SpawnedCall { callee, args, .. } => {
            collect_general_closure_captures_walk(callee, outer_bindings, seen, captures);
            for arg in args {
                collect_general_closure_captures_walk(arg, outer_bindings, seen, captures);
            }
        }
        HirExprKind::Block(block)
        | HirExprKind::Scope { body: block }
        | HirExprKind::ForkBlock { body: block, .. } => {
            collect_general_closure_captures_walk_block(block, outer_bindings, seen, captures);
        }
        HirExprKind::ScopeDeadline { duration, body } => {
            collect_general_closure_captures_walk(duration, outer_bindings, seen, captures);
            collect_general_closure_captures_walk_block(body, outer_bindings, seen, captures);
        }
        HirExprKind::If {
            condition,
            then_expr,
            else_expr,
        } => {
            collect_general_closure_captures_walk(condition, outer_bindings, seen, captures);
            collect_general_closure_captures_walk(then_expr, outer_bindings, seen, captures);
            if let Some(else_expr) = else_expr {
                collect_general_closure_captures_walk(else_expr, outer_bindings, seen, captures);
            }
        }
        HirExprKind::StructInit { fields, base, .. } => {
            for (_, field) in fields {
                collect_general_closure_captures_walk(field, outer_bindings, seen, captures);
            }
            if let Some(base) = base {
                collect_general_closure_captures_walk(base, outer_bindings, seen, captures);
            }
        }
        HirExprKind::FieldAccess { object, .. } => {
            collect_general_closure_captures_walk(object, outer_bindings, seen, captures);
        }
        HirExprKind::AwaitTask { binding_id, .. } => {
            if let Some((name, _, span)) = outer_bindings.get(binding_id) {
                if seen.insert(*binding_id) {
                    captures.push((*binding_id, name.clone(), span.clone()));
                }
            }
        }
        HirExprKind::Select(select) => {
            for arm in &select.arms {
                match &arm.kind {
                    HirSelectArmKind::StreamNext { stream } => {
                        collect_general_closure_captures_walk(
                            stream,
                            outer_bindings,
                            seen,
                            captures,
                        );
                    }
                    HirSelectArmKind::ActorAsk { actor, args, .. } => {
                        collect_general_closure_captures_walk(
                            actor,
                            outer_bindings,
                            seen,
                            captures,
                        );
                        for arg in args {
                            collect_general_closure_captures_walk(
                                arg,
                                outer_bindings,
                                seen,
                                captures,
                            );
                        }
                    }
                    HirSelectArmKind::TaskAwait { task } => {
                        collect_general_closure_captures_walk(task, outer_bindings, seen, captures);
                    }
                    HirSelectArmKind::AfterTimer { duration } => {
                        collect_general_closure_captures_walk(
                            duration,
                            outer_bindings,
                            seen,
                            captures,
                        );
                    }
                }
                collect_general_closure_captures_walk(&arm.body, outer_bindings, seen, captures);
            }
        }
        HirExprKind::TupleIndex { tuple, .. } => {
            collect_general_closure_captures_walk(tuple, outer_bindings, seen, captures);
        }
        HirExprKind::Index { container, index } => {
            collect_general_closure_captures_walk(container, outer_bindings, seen, captures);
            collect_general_closure_captures_walk(index, outer_bindings, seen, captures);
        }
        HirExprKind::Slice {
            container,
            start,
            end,
            inclusive: _,
        } => {
            collect_general_closure_captures_walk(container, outer_bindings, seen, captures);
            if let Some(s) = start {
                collect_general_closure_captures_walk(s, outer_bindings, seen, captures);
            }
            if let Some(e) = end {
                collect_general_closure_captures_walk(e, outer_bindings, seen, captures);
            }
        }
        HirExprKind::CoerceToDynTrait { value, .. } => {
            collect_general_closure_captures_walk(value, outer_bindings, seen, captures);
        }
        HirExprKind::CallDynMethod { receiver, args, .. } => {
            collect_general_closure_captures_walk(receiver, outer_bindings, seen, captures);
            for arg in args {
                collect_general_closure_captures_walk(arg, outer_bindings, seen, captures);
            }
        }
    }
}

fn collect_general_closure_captures_walk_block(
    block: &HirBlock,
    outer_bindings: &HashMap<BindingId, OuterClosureBinding>,
    seen: &mut HashSet<BindingId>,
    captures: &mut Vec<ClosureCaptureCandidate>,
) {
    for stmt in &block.statements {
        match &stmt.kind {
            HirStmtKind::Let(_, Some(value)) => {
                collect_general_closure_captures_walk(value, outer_bindings, seen, captures);
            }
            HirStmtKind::Expr(expr) | HirStmtKind::Return(Some(expr)) => {
                collect_general_closure_captures_walk(expr, outer_bindings, seen, captures);
            }
            HirStmtKind::Let(_, None) | HirStmtKind::Return(None) => {}
        }
    }
    if let Some(tail) = &block.tail {
        collect_general_closure_captures_walk(tail, outer_bindings, seen, captures);
    }
}

fn collect_captures_walk_block(
    block: &HirBlock,
    param_ids: &std::collections::HashSet<BindingId>,
    seen: &mut std::collections::HashSet<BindingId>,
    captures: &mut Vec<HirLambdaCapture>,
    self_id: Option<BindingId>,
) {
    for stmt in &block.statements {
        match &stmt.kind {
            HirStmtKind::Let(_, Some(value)) => {
                collect_captures_walk(value, param_ids, seen, captures, self_id);
            }
            HirStmtKind::Expr(expr) | HirStmtKind::Return(Some(expr)) => {
                collect_captures_walk(expr, param_ids, seen, captures, self_id);
            }
            HirStmtKind::Let(_, None) | HirStmtKind::Return(None) => {}
        }
    }
    if let Some(tail) = &block.tail {
        collect_captures_walk(tail, param_ids, seen, captures, self_id);
    }
}

// ── Machine static-check helpers ────────────────────────────────────────────

/// Determine whether a self-transition body is "empty" for the `@reenter` rule.
///
/// A body is considered empty when:
/// - It is `Expr::Identifier(target_state)` — the no-body semicolon shorthand
///   that the parser synthesises for `on E: S -> S;`.
/// - It is `Expr::Block` with no statements and no trailing expression.
///
/// Any other form (statements, expressions) is non-empty and requires `@reenter`.
fn is_empty_self_body(body: &Expr, target_state: &str) -> bool {
    match body {
        Expr::Identifier(name) => name == target_state,
        Expr::Block(block) => block.stmts.is_empty() && block.trailing_expr.is_none(),
        _ => false,
    }
}

/// Shallow-scan a `Block` for field names appearing as the left-hand side of
/// an assignment statement (`self.field = ...`). Used for effect-parity checking
/// in entry blocks — the scan is intentionally shallow (depth = 1) since a
/// full walk would require type information we don't have in Lane A.
fn collect_assigned_field_names(block: &Block) -> Vec<(String, Span)> {
    let mut names = Vec::new();
    for (stmt, _) in &block.stmts {
        if let Stmt::Assign { target, .. } = stmt {
            if let Expr::FieldAccess { object, field } = &target.0 {
                if matches!(object.0, Expr::This) {
                    names.push((field.clone(), target.1.clone()));
                }
            }
        }
    }
    names
}

/// Shallow-scan an `Expr` (transition body) for `self.field = ...` assignments.
fn collect_assigned_field_names_expr(expr: &Expr) -> Vec<String> {
    if let Expr::Block(block) = expr {
        collect_assigned_field_names(block)
            .into_iter()
            .map(|(name, _)| name)
            .collect()
    } else {
        Vec::new()
    }
}

/// Collect event names directly emitted by `emit EventName` expressions within
/// an expression (transition body). Only direct emits are tracked; deeper nesting
/// is deferred to runtime (per the plan's "direct cycles only" rule).
fn collect_emitted_events(expr: &Expr) -> Vec<String> {
    let mut events = Vec::new();
    collect_emitted_events_inner(expr, &mut events);
    events
}

fn collect_emitted_events_inner(expr: &Expr, out: &mut Vec<String>) {
    match expr {
        Expr::MachineEmit { event_name, .. } => out.push(event_name.clone()),
        Expr::Block(block) => {
            for (stmt, _) in &block.stmts {
                if let Stmt::Expression((e, _)) = stmt {
                    collect_emitted_events_inner(e, out);
                }
            }
            if let Some(tail) = &block.trailing_expr {
                collect_emitted_events_inner(&tail.0, out);
            }
        }
        _ => {}
    }
}

/// One-token description of a parser `Expr` shape, used by
/// `SelectArmNotSealedForm` diagnostic notes. Intentionally coarse — the
/// goal is to tell the user "you wrote a literal where a sealed form
/// belongs", not to echo the expression back at them.
fn describe_select_source_shape(expr: &Expr) -> String {
    match expr {
        Expr::Literal(_) => "literal".into(),
        Expr::Identifier(_) => "identifier".into(),
        Expr::Binary { .. } => "binary expression".into(),
        Expr::Block(_) => "block".into(),
        Expr::If { .. } => "if expression".into(),
        Expr::Select { .. } => "nested select".into(),
        Expr::Join(_) => "join expression".into(),
        Expr::FieldAccess { .. } => "field access".into(),
        Expr::Index { .. } => "index expression".into(),
        Expr::Range { .. } => "range expression".into(),
        Expr::Cast { .. } => "cast expression".into(),
        Expr::Timeout { .. } => "timeout expression".into(),
        Expr::UnsafeBlock(_) => "unsafe block".into(),
        Expr::Yield(_) => "yield expression".into(),
        Expr::This => "this".into(),
        _ => "expression".into(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hew_types::module_registry::ModuleRegistry;
    use hew_types::Checker;

    fn parse_typecheck_and_lower(
        source: &str,
    ) -> (hew_parser::ast::Program, TypeCheckOutput, LowerOutput) {
        let parsed = hew_parser::parse(source);
        assert!(
            parsed.errors.is_empty(),
            "parse errors: {:#?}",
            parsed.errors
        );

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let tco = checker.check_program(&parsed.program);
        assert!(tco.errors.is_empty(), "type errors: {:#?}", tco.errors);

        let lowered = lower_program(&parsed.program, &tco, &ResolutionCtx);
        (parsed.program, tco, lowered)
    }

    fn main_function_body(output: &LowerOutput) -> &HirBlock {
        let Some(HirItem::Function(function)) =
            output.module.items.iter().find(
                |item| matches!(item, HirItem::Function(function) if function.name == "main"),
            )
        else {
            panic!("expected lowered main function: {:#?}", output.module.items);
        };
        &function.body
    }

    fn checker_fact_named<'a>(
        facts: impl Iterator<Item = &'a ClosureCaptureFact>,
        name: &str,
    ) -> &'a ClosureCaptureFact {
        let mut matches = facts.filter(|fact| fact.name == name);
        let first = matches
            .next()
            .unwrap_or_else(|| panic!("expected checker fact named {name}"));
        assert!(
            matches.next().is_none(),
            "expected exactly one checker fact named {name}"
        );
        first
    }

    #[test]
    fn closure_capture_uses_checker_mode_and_send_fact() {
        let (_program, tco, lowered) = parse_typecheck_and_lower(
            r"
            fn main() {
                let k: i32 = 2;
                let f = |n: i32| n + k;
            }
            ",
        );
        assert!(
            lowered.diagnostics.is_empty(),
            "lower diagnostics: {:#?}",
            lowered.diagnostics
        );

        let checker_fact = checker_fact_named(
            tco.closure_capture_facts
                .values()
                .flat_map(|facts| facts.iter()),
            "k",
        );

        let body = main_function_body(&lowered);
        let HirStmtKind::Let(k_binding, _) = &body.statements[0].kind else {
            panic!("expected first statement to bind k");
        };
        let HirStmtKind::Let(_, Some(closure_expr)) = &body.statements[1].kind else {
            panic!("expected second statement to bind closure");
        };
        let HirExprKind::Closure { captures, .. } = &closure_expr.kind else {
            panic!("expected closure initializer, got {:#?}", closure_expr.kind);
        };
        let hir_capture = captures
            .iter()
            .find(|capture| capture.name == "k")
            .unwrap_or_else(|| panic!("expected HIR capture named k: {captures:#?}"));

        assert_eq!(hir_capture.binding, k_binding.id);
        assert_eq!(hir_capture.mode, checker_fact.mode);
        assert_eq!(hir_capture.is_send, checker_fact.is_send);
    }

    #[test]
    fn missing_closure_capture_facts_emit_boundary_diagnostic() {
        let (program, mut tco, _) = parse_typecheck_and_lower(
            r"
            fn main() {
                let k: i32 = 2;
                let f = |n: i32| n + k;
            }
            ",
        );
        assert!(
            !tco.closure_capture_facts.is_empty(),
            "test setup requires checker-produced closure capture facts"
        );
        tco.closure_capture_facts.clear();

        let lowered = lower_program(&program, &tco, &ResolutionCtx);

        assert!(
            lowered.diagnostics.iter().any(|diagnostic| matches!(
                &diagnostic.kind,
                HirDiagnosticKind::CheckerBoundaryViolation { name, reason }
                    if name == "closure literal"
                        && reason == "closure_capture_facts has no record for closure literal span"
            )),
            "missing closure_capture_facts entry must emit root-cause boundary diagnostic; got {:#?}",
            lowered.diagnostics
        );
    }
}
