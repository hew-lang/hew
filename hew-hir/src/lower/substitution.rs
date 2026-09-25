//! Type substitution, closure specialization and call-site collection.

use super::*;

/// A `CallTraitMethodStatic` site discovered inside a function body.
/// Carries enough info to derive the impl-method monomorphisation once the
/// surrounding function's type params have been substituted.
pub(super) struct TraitMethodStaticSite {
    /// Type-parameter name of the receiver (e.g. "T" in `fn display<T: Show>`).
    pub(super) receiver_type_param: String,
    /// Checker-selected trait-method identity. The monomorphisation lookup
    /// consumes its ids directly rather than rebuilding them from spellings.
    pub(super) target: hew_types::CallTarget,
}

#[allow(
    clippy::too_many_lines,
    reason = "two-phase worklist: direct calls + trait method sites"
)]
pub(super) fn closure_under_substitution(
    defs: &hew_types::DefTable,
    items: &[HirItem],
    call_site_type_args: &HashMap<SiteId, Vec<ResolvedTy>>,
    monomorphisations: &mut Vec<crate::monomorph::MonomorphizedFn>,
    cap: usize,
    diagnostics: &mut Vec<HirDiagnostic>,
) {
    use crate::monomorph::{function_monomorph_symbol, MonomorphizedFn};

    // Build origin_id → &HirFn map for body lookup.
    let mut origin_fns: HashMap<ItemId, &HirFn> = HashMap::new();
    // Map of emitted fn symbol → checker declaration identity plus the local
    // body origin and type parameters. The linker symbol locates the body;
    // the declaration is the authority carried into every MonoKey.
    let mut fn_info: HashMap<String, (ItemId, hew_types::DefId, Vec<String>)> = HashMap::new();
    for item in items {
        if let HirItem::Function(f) = item {
            origin_fns.insert(f.id, f);
            fn_info.insert(f.name.clone(), (f.id, f.declaration, f.type_params.clone()));
        }
    }
    // Structured `(declaring_trait, self_type_name, method_name)` index
    // built from `HirItem::Impl` metadata. Static-dispatch monomorphisation
    // resolves trait method calls through this rather than reconstructing
    // the impl symbol from a receiver display name.
    let impl_index = crate::dispatch::build_trait_impl_method_index(items);

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
        let mut trait_method_sites: Vec<TraitMethodStaticSite> = Vec::new();
        collect_call_sites_in_block(&origin.body, &mut inner_sites, &mut trait_method_sites);

        // ── Direct Call sites (existing logic) ───────────────────────────
        for (callee_name, site) in inner_sites {
            let Some((origin_id, declaration, type_params)) = fn_info.get(&callee_name).cloned()
            else {
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
                declaration,
                linker_symbol: callee_name.clone(),
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
            let mangled = function_monomorph_symbol(&callee_name, &substituted);
            monomorphisations.push(MonomorphizedFn {
                key: new_key.clone(),
                mangled_name: mangled,
            });
            worklist.push(new_key);
        }

        // ── CallTraitMethodStatic sites (impl-method monomorphisation) ───
        // When a generic function body contains `item.show()` via a trait
        // bound, and this mono's substitution resolves the receiver type
        // param to a concrete type, look up the matching impl method via
        // the structured registry — `(declaring_trait, self_type_name,
        // method_name)` — and register the impl method's monomorphisation.
        for tms in trait_method_sites {
            let Some(concrete_ty) = subst.get(&tms.receiver_type_param) else {
                continue;
            };
            // Canonical nominal instance for impl lookup.
            let Some(self_type) = concrete_ty.impl_receiver_instance(defs) else {
                continue;
            };
            let type_args = self_type.args.clone();
            // Structured registry lookup. The key is built from HIR-side
            // structured identities only — no symbol-name parsing or leaf
            // retry. A concrete instance is tried before the same nominal's
            // generic implementation.
            let hew_types::CallTarget::StaticTraitMethod {
                declaring_trait,
                method,
            } = &tms.target
            else {
                continue;
            };
            let Some(entry) = crate::dispatch::lookup_trait_impl_entry_by_id(
                &impl_index,
                declaring_trait,
                &self_type,
                method,
            ) else {
                continue;
            };
            // Non-generic impl methods need no per-instantiation
            // registration; their bare symbol is already a module fn.
            if entry.impl_type_params.is_empty() {
                continue;
            }
            // Find the origin fn that owns this impl method symbol so
            // we can build a `MonoKey` whose `origin` points at the
            // pre-substitution body.
            let Some((origin_id, origin_declaration, origin_type_params)) =
                fn_info.get(&entry.method_symbol).cloned()
            else {
                continue;
            };
            // The body found by linker symbol must attest to the exact
            // checker-selected implementation declaration. A symbol collision
            // is a boundary failure, not a reason to pick another body.
            if origin_declaration != entry.method {
                continue;
            }
            if origin_type_params.is_empty() {
                // Impl-block carried type params but the per-method
                // origin did not — invariant violation upstream;
                // skip to avoid wrong monomorphisation.
                continue;
            }
            // Skip if type_args still contain abstract symbols.
            if type_args
                .iter()
                .any(|t| contains_abstract_symbol(t, &fn_info))
            {
                continue;
            }
            let new_key = MonoKey {
                origin: origin_id,
                declaration: entry.method,
                linker_symbol: entry.method_symbol.clone(),
                type_args: type_args.clone(),
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
            let mangled = function_monomorph_symbol(&entry.method_symbol, &type_args);
            monomorphisations.push(MonomorphizedFn {
                key: new_key.clone(),
                mangled_name: mangled,
            });
            worklist.push(new_key);
        }
    }
}

pub(super) fn collect_call_sites_in_block(
    block: &HirBlock,
    out: &mut Vec<(String, SiteId)>,
    trait_out: &mut Vec<TraitMethodStaticSite>,
) {
    for stmt in &block.statements {
        collect_call_sites_in_stmt(stmt, out, trait_out);
    }
    if let Some(tail) = &block.tail {
        collect_call_sites_in_expr(tail, out, trait_out);
    }
}

/// Map the HIR lowering context's current-module authority to a total
/// [`ExternProvenance`] for an `extern` fn declared under it.
///
/// `None` is the genuine root compilation unit (module index 0), which lowers
/// to [`ExternProvenance::Root`]; `Some(dotted)` is a named module (std or
/// user/package) lowered to [`ExternProvenance::Module`]. This is the ONLY
/// mapping either construction site uses, so a std and a user extern are
/// classified from the same authority regardless of which lowering pass emitted
/// them.
pub(super) fn extern_provenance(current_module_name: Option<&str>) -> ExternProvenance {
    match current_module_name {
        None => ExternProvenance::Root,
        Some(name) => ExternProvenance::Module(name.to_string()),
    }
}

pub(super) fn extern_runtime_capability(
    provenance: &ExternProvenance,
    declaration: &str,
) -> Option<hew_types::ExternRuntimeCapability> {
    let ExternProvenance::Module(module_name) = provenance else {
        return None;
    };
    hew_types::stdlib_authority().extern_runtime_capability(module_name, declaration)
}

pub(super) fn record_source_modules_for_items(
    items: &[HirItem],
    source_module: &str,
    diagnostic_source_modules: &mut HashMap<ItemId, String>,
) {
    for item in items {
        let id = match item {
            HirItem::Function(item) => item.id,
            HirItem::TypeDecl(item) => item.id,
            HirItem::Record(item) => item.id,
            HirItem::Actor(item) => item.id,
            HirItem::Supervisor(item) => item.id,
            HirItem::Impl(item) => item.id,
            HirItem::ExternFn(item) => item.id,
            HirItem::Const(item) => item.id,
        };
        diagnostic_source_modules.insert(id, source_module.to_string());
    }
}

pub(super) fn collect_call_sites_in_stmt(
    stmt: &HirStmt,
    out: &mut Vec<(String, SiteId)>,
    trait_out: &mut Vec<TraitMethodStaticSite>,
) {
    match &stmt.kind {
        HirStmtKind::Let(_, Some(e)) | HirStmtKind::Expr(e) | HirStmtKind::Return(Some(e)) => {
            collect_call_sites_in_expr(e, out, trait_out);
        }
        HirStmtKind::Destructure { value, .. } => {
            collect_call_sites_in_expr(value, out, trait_out);
        }
        HirStmtKind::Assign { target, value, .. } => {
            collect_call_sites_in_expr(target, out, trait_out);
            collect_call_sites_in_expr(value, out, trait_out);
        }
        // Let-without-value and bare return have no sub-expression to collect.
        HirStmtKind::Let(_, None) | HirStmtKind::Return(None) => {}
        HirStmtKind::Defer { body, .. } => {
            collect_call_sites_in_expr(body, out, trait_out);
        }
    }
}

#[allow(
    clippy::too_many_lines,
    reason = "single recursive walker spanning all HirExprKind variants"
)]
pub(super) fn collect_call_sites_in_expr(
    expr: &HirExpr,
    out: &mut Vec<(String, SiteId)>,
    trait_out: &mut Vec<TraitMethodStaticSite>,
) {
    match &expr.kind {
        HirExprKind::BindingRef {
            name,
            resolved: ResolvedRef::Item(_),
        } => {
            out.push((name.clone(), expr.site));
        }

        HirExprKind::RcIntrinsic {
            receiver, value, ..
        } => {
            for operand in receiver.iter().chain(value.iter()) {
                collect_call_sites_in_expr(operand, out, trait_out);
            }
        }
        HirExprKind::Call { callee, args, .. } => {
            // Record the site if callee is a direct BindingRef name.
            if let HirExprKind::BindingRef { name, .. } = &callee.kind {
                out.push((name.clone(), expr.site));
            }
            collect_call_sites_in_expr(callee, out, trait_out);
            for a in args {
                collect_call_sites_in_expr(a, out, trait_out);
            }
        }
        HirExprKind::Spawn { args, .. } => {
            for (_, arg) in args {
                collect_call_sites_in_expr(arg, out, trait_out);
            }
        }
        HirExprKind::ActorMessage { receiver, args, .. }
        | HirExprKind::ActorDelivery { receiver, args, .. }
        | HirExprKind::ActorAsk { receiver, args, .. }
        | HirExprKind::ActorGenStream { receiver, args, .. }
        | HirExprKind::ResolvedImplCall { receiver, args, .. }
        | HirExprKind::CallDynMethod { receiver, args, .. } => {
            collect_call_sites_in_expr(receiver, out, trait_out);
            for arg in args {
                collect_call_sites_in_expr(arg, out, trait_out);
            }
        }
        HirExprKind::ConnAwaitRead { conn, .. } => {
            collect_call_sites_in_expr(conn, out, trait_out);
        }
        HirExprKind::AwaitRestart { child } | HirExprKind::AwaitTask { operand: child, .. } => {
            collect_call_sites_in_expr(child, out, trait_out);
        }
        HirExprKind::ListenerAwaitAccept { listener, .. } => {
            collect_call_sites_in_expr(listener, out, trait_out);
        }
        HirExprKind::RemoteActorAsk {
            receiver,
            msg,
            timeout_ms,
            ..
        } => {
            collect_call_sites_in_expr(receiver, out, trait_out);
            collect_call_sites_in_expr(msg, out, trait_out);
            collect_call_sites_in_expr(timeout_ms, out, trait_out);
        }
        HirExprKind::RemoteActorSend { receiver, msg } => {
            collect_call_sites_in_expr(receiver, out, trait_out);
            collect_call_sites_in_expr(msg, out, trait_out);
        }
        HirExprKind::CallTraitMethodStatic {
            receiver,
            receiver_type_param,
            target,
            args,
            ..
        } => {
            // Record this as a trait-method static dispatch site for the
            // monomorphisation closure to resolve once the enclosing
            // function's type params are substituted.
            trait_out.push(TraitMethodStaticSite {
                receiver_type_param: receiver_type_param.clone(),
                target: target.clone(),
            });
            collect_call_sites_in_expr(receiver, out, trait_out);
            for arg in args {
                collect_call_sites_in_expr(arg, out, trait_out);
            }
        }
        HirExprKind::VarSelfMethodCall {
            receiver,
            call_target,
            target,
            args,
            ..
        } => {
            match target {
                // Direct var-self monomorphisation is registered by the
                // checker-targeted source lowering site.  This marker carries
                // no presentation string to re-discover it here.
                HirVarSelfMethodTarget::Direct => {}
                HirVarSelfMethodTarget::StaticTrait {
                    receiver_type_param,
                    ..
                } => trait_out.push(TraitMethodStaticSite {
                    receiver_type_param: receiver_type_param.clone(),
                    target: call_target.clone(),
                }),
            }
            collect_call_sites_in_expr(receiver, out, trait_out);
            for arg in args {
                collect_call_sites_in_expr(arg, out, trait_out);
            }
        }
        HirExprKind::Binary { left, right, .. } | HirExprKind::IdentityCompare { left, right } => {
            collect_call_sites_in_expr(left, out, trait_out);
            collect_call_sites_in_expr(right, out, trait_out);
        }
        HirExprKind::Unary { operand, .. } | HirExprKind::WireCodec { operand, .. } => {
            collect_call_sites_in_expr(operand, out, trait_out);
        }
        HirExprKind::ArrayRepeat { value }
        | HirExprKind::NumericCast { value, .. }
        | HirExprKind::SaturatingWidthCast { value, .. }
        | HirExprKind::TryWidthCast { value, .. }
        | HirExprKind::CoerceToDynTrait { value, .. } => {
            collect_call_sites_in_expr(value, out, trait_out);
        }
        HirExprKind::TupleLiteral { elements } | HirExprKind::ArrayLiteral { elements } => {
            for elem in elements {
                collect_call_sites_in_expr(elem, out, trait_out);
            }
        }
        HirExprKind::Block(b) => collect_call_sites_in_block(b, out, trait_out),
        HirExprKind::GenBlock { body, .. } => collect_call_sites_in_block(body, out, trait_out),
        HirExprKind::Yield {
            value: Some(value), ..
        } => collect_call_sites_in_expr(value, out, trait_out),
        HirExprKind::If {
            condition,
            then_expr,
            else_expr,
        } => {
            collect_call_sites_in_expr(condition, out, trait_out);
            collect_call_sites_in_expr(then_expr, out, trait_out);
            if let Some(e) = else_expr {
                collect_call_sites_in_expr(e, out, trait_out);
            }
        }
        HirExprKind::StructInit { fields, base, .. } => {
            for (_, e) in fields {
                collect_call_sites_in_expr(e, out, trait_out);
            }
            if let Some(b) = base {
                collect_call_sites_in_expr(b, out, trait_out);
            }
        }
        HirExprKind::FieldAccess { object, .. } => {
            collect_call_sites_in_expr(object, out, trait_out);
        }
        HirExprKind::Scope { body }
        | HirExprKind::Race { body }
        | HirExprKind::ForkBlock { body, .. }
        | HirExprKind::Loop { body, .. } => {
            collect_call_sites_in_block(body, out, trait_out);
        }
        HirExprKind::ScopeRecovery { scope, handler, .. } => {
            collect_call_sites_in_expr(scope, out, trait_out);
            collect_call_sites_in_expr(handler, out, trait_out);
        }
        HirExprKind::ScopeDeadline { duration, body } => {
            collect_call_sites_in_expr(duration, out, trait_out);
            collect_call_sites_in_block(body, out, trait_out);
        }
        HirExprKind::TupleIndex { tuple, .. } => collect_call_sites_in_expr(tuple, out, trait_out),
        HirExprKind::Index { container, index }
        | HirExprKind::BorrowedIndex { container, index } => {
            collect_call_sites_in_expr(container, out, trait_out);
            collect_call_sites_in_expr(index, out, trait_out);
        }
        HirExprKind::Slice {
            container,
            start,
            end,
            ..
        } => {
            collect_call_sites_in_expr(container, out, trait_out);
            if let Some(s) = start {
                collect_call_sites_in_expr(s, out, trait_out);
            }
            if let Some(e) = end {
                collect_call_sites_in_expr(e, out, trait_out);
            }
        }
        HirExprKind::SpawnLambdaActor { body, .. } | HirExprKind::Closure { body, .. } => {
            collect_call_sites_in_expr(body, out, trait_out);
        }
        HirExprKind::While {
            condition, body, ..
        } => {
            collect_call_sites_in_expr(condition, out, trait_out);
            collect_call_sites_in_block(body, out, trait_out);
        }
        HirExprKind::ForRange {
            start,
            end,
            step,
            body,
            ..
        } => {
            collect_call_sites_in_expr(start, out, trait_out);
            collect_call_sites_in_expr(end, out, trait_out);
            collect_call_sites_in_expr(step, out, trait_out);
            collect_call_sites_in_block(body, out, trait_out);
        }
        HirExprKind::Match { scrutinee, arms } => {
            collect_call_sites_in_expr(scrutinee, out, trait_out);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    collect_call_sites_in_expr(guard, out, trait_out);
                }
                collect_call_sites_in_expr(&arm.body, out, trait_out);
            }
        }
        HirExprKind::Break { value, .. } | HirExprKind::Return { value } => {
            if let Some(value) = value {
                collect_call_sites_in_expr(value, out, trait_out);
            }
        }
        HirExprKind::CancellationTokenIsCancelled { receiver }
        | HirExprKind::GeneratorNext { receiver, .. }
        | HirExprKind::RecordCloneCall { src: receiver, .. }
        | HirExprKind::SubsumedValue {
            source: receiver, ..
        } => {
            collect_call_sites_in_expr(receiver, out, trait_out);
        }
        HirExprKind::StreamRecvAwait { stream, .. } => {
            collect_call_sites_in_expr(stream, out, trait_out);
        }
        HirExprKind::MachineVariantCtor { payload, .. } => {
            if let Some(fields) = payload {
                for (_, e) in fields {
                    collect_call_sites_in_expr(e, out, trait_out);
                }
            }
        }
        HirExprKind::Select(sel) => {
            for arm in &sel.arms {
                match &arm.kind {
                    HirSelectArmKind::StreamNext { stream } => {
                        collect_call_sites_in_expr(stream, out, trait_out);
                    }
                    HirSelectArmKind::ActorAsk { call } => {
                        collect_call_sites_in_expr(call, out, trait_out);
                    }
                    HirSelectArmKind::TaskAwait { task } => {
                        collect_call_sites_in_expr(task, out, trait_out);
                    }
                    HirSelectArmKind::AfterTimer { duration } => {
                        collect_call_sites_in_expr(duration, out, trait_out);
                    }
                }
                collect_call_sites_in_expr(&arm.body, out, trait_out);
            }
        }
        // Leaf variants: no sub-expressions, so no call sites to collect.
        HirExprKind::Literal(_)
        | HirExprKind::RegexLiteralRef { .. }
        | HirExprKind::BindingRef { .. }
        | HirExprKind::ContextReader { .. }
        | HirExprKind::Yield { value: None, .. }
        | HirExprKind::Continue { .. }
        | HirExprKind::ActorSelf
        | HirExprKind::Unsupported(_) => {}
    }
}

/// Substitute `ResolvedTy::Named { name, args: [] }` whose `name`
/// appears in `subst` with the mapped concrete type. Recurses into
/// composite type constructors. Used by the closure-under-substitution
/// pass and by MIR-side monomorphisation lowering.
#[must_use]
#[allow(
    clippy::match_same_arms,
    reason = "DI-019: the abstract-parameter `TypeParam` case is listed \
              explicitly (totality over the new variant) even though an \
              un-substituted parameter clones identically to the leaf \
              fall-through"
)]
pub fn substitute_ty<S: std::hash::BuildHasher>(
    ty: &ResolvedTy,
    subst: &HashMap<String, ResolvedTy, S>,
) -> ResolvedTy {
    match ty {
        // Only a binder is substituted: a nominal of the same spelling is a
        // different type.
        ResolvedTy::Named {
            head: head @ (hew_types::TypeHead::Param(_) | hew_types::TypeHead::Unresolved(_)),
            args,
            ..
        } if args.is_empty() && subst.contains_key(head.registry_key()) => {
            subst[head.registry_key()].clone()
        }
        ResolvedTy::Named {
            head,
            args,
            is_opaque,
        } => ResolvedTy::Named {
            head: *head,
            args: args.iter().map(|a| substitute_ty(a, subst)).collect(),
            is_opaque: *is_opaque,
        },
        ResolvedTy::Tuple(items) => {
            ResolvedTy::Tuple(items.iter().map(|t| substitute_ty(t, subst)).collect())
        }
        ResolvedTy::Array(elem, n) => ResolvedTy::Array(Box::new(substitute_ty(elem, subst)), *n),
        ResolvedTy::Slice(elem) => ResolvedTy::Slice(Box::new(substitute_ty(elem, subst))),
        ResolvedTy::Function {
            capabilities,
            params,
            ret,
        } => ResolvedTy::Function {
            capabilities: *capabilities,
            params: params.iter().map(|p| substitute_ty(p, subst)).collect(),
            ret: Box::new(substitute_ty(ret, subst)),
        },
        ResolvedTy::Closure {
            capabilities,
            params,
            ret,
            captures,
        } => ResolvedTy::Closure {
            capabilities: *capabilities,
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
        // Borrow and trait-object are composite type constructors: their
        // payloads must be descended so a nested `TypeParam` is substituted
        // (DI-019 — no wildcard absorbs a composite). Mirrors the scope-aware
        // converter in `hew_types::ResolvedTy::from_ty_scoped`.
        ResolvedTy::Borrow { pointee } => ResolvedTy::Borrow {
            pointee: Box::new(substitute_ty(pointee, subst)),
        },
        ResolvedTy::TraitObject { traits } => ResolvedTy::TraitObject {
            traits: traits
                .iter()
                .map(|bound| hew_types::ResolvedTraitBound {
                    trait_name: bound.trait_name.clone(),
                    trait_id: bound.trait_id,
                    args: bound.args.iter().map(|a| substitute_ty(a, subst)).collect(),
                    assoc_bindings: bound
                        .assoc_bindings
                        .iter()
                        .map(|(name, t)| (name.clone(), substitute_ty(t, subst)))
                        .collect(),
                })
                .collect(),
        },
        // A structural abstract parameter (A622). Substitute it when the map
        // carries its name; otherwise keep it abstract.
        ResolvedTy::TypeParam { name } if subst.contains_key(name) => subst[name].clone(),
        ResolvedTy::TypeParam { .. } => ty.clone(),
        _ => ty.clone(),
    }
}

pub(super) fn contains_abstract_symbol(
    ty: &ResolvedTy,
    fn_info: &HashMap<String, (ItemId, hew_types::DefId, Vec<String>)>,
) -> bool {
    // A type contains an abstract symbol if any `Named { args: [] }`
    // matches a type-parameter name declared on any top-level fn.
    let is_type_param = |name: &str| {
        fn_info
            .values()
            .any(|(_, _, params)| params.iter().any(|p| p == name))
    };
    match ty {
        ResolvedTy::Named { head, args, .. } => {
            let name = head.registry_key();
            if args.is_empty() && is_type_param(name) {
                return true;
            }
            args.iter().any(|a| contains_abstract_symbol(a, fn_info))
        }
        ResolvedTy::Tuple(items) => items.iter().any(|t| contains_abstract_symbol(t, fn_info)),
        ResolvedTy::Array(elem, _) | ResolvedTy::Slice(elem) => {
            contains_abstract_symbol(elem, fn_info)
        }
        ResolvedTy::Function { params, ret, .. } => {
            params.iter().any(|p| contains_abstract_symbol(p, fn_info))
                || contains_abstract_symbol(ret, fn_info)
        }
        ResolvedTy::Closure {
            params,
            ret,
            captures,
            ..
        } => {
            params.iter().any(|p| contains_abstract_symbol(p, fn_info))
                || contains_abstract_symbol(ret, fn_info)
                || captures
                    .iter()
                    .any(|c| contains_abstract_symbol(c, fn_info))
        }
        ResolvedTy::Pointer { pointee, .. } | ResolvedTy::Borrow { pointee } => {
            contains_abstract_symbol(pointee, fn_info)
        }
        ResolvedTy::TraitObject { traits } => traits.iter().any(|bound| {
            bound
                .args
                .iter()
                .any(|a| contains_abstract_symbol(a, fn_info))
                || bound
                    .assoc_bindings
                    .iter()
                    .any(|(_, t)| contains_abstract_symbol(t, fn_info))
        }),
        ResolvedTy::Task(inner) => contains_abstract_symbol(inner, fn_info),
        // A structural type parameter is abstract by construction.
        ResolvedTy::TypeParam { .. } => true,
        _ => false,
    }
}
