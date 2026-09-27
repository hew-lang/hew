//! Call-shape and supervisor-spawn scanning gates.

use super::*;

// ── FC-P1-B: Call-shape gates (HIR-level) ───────────────────────────────────
//
// Hoists MIR's two call-shape fail-closed diagnostics into HIR:
//   - `CallableUnsupportedInMir` (lifted from `hew-mir/src/lower.rs:4194`):
//     `BindingRef { Item(_) }` callees whose name is not in the module's
//     callable set.
//   - `IndirectCallUnsupported` (lifted from `hew-mir/src/lower.rs:4236`):
//     `BindingRef { Unresolved }` callees with callable static type
//     (`Function` / `Closure`) — narrowed deliberately so closure-binding
//     calls (`let f = |x| x + 1; f(2)` → `Binding(_)`) and direct module-fn
//     calls are not false-positively rejected.
//
// Walks the LOWERED HIR (not the parser AST) because the predicates depend
// on `ResolvedRef` and `ResolvedTy`, which only exist post-lowering. The
// walker shape mirrors `scan_*_for_blocking_recv`: per-item bucket, then a
// per-`HirBlock` / `HirStmt` / `HirExpr` recursion exhaustive over the HIR
// expression tree.
//
// The runtime ABI allowlist (`is_known_runtime_symbol`) lives in `hew-mir`;
// `hew-hir` cannot depend on `hew-mir`. Instead the gate recomputes the same
// callable set from sources that already exist in `hew-hir`:
// `stdlib_catalog::entries()` for the runtime allowlist, plus the no-AST-item
// builtin names that `LowerCtx::seed_stdlib_fn_registry` already mirrors when
// seeding the fn registry. (The typed-builtin names resolve to
// `ResolvedRef::Builtin` and are accepted by the gate unconditionally; their
// entries here only matter for the by-name `Item` check.)

/// Test-only entrypoint that runs the FC-P1-B call-shape gates against a
/// synthetic HIR module slice. Exposed so integration tests in
/// `hew-hir/tests/call_shape_gates.rs` can exercise the negative-test cases
/// directly without having to drive a real surface program through every
/// path that produces a `BindingRef { Item(_) }` to a name absent from the
/// callable set (which is hard to reach from current v0.5 surface syntax
/// because `lower_identifier` only emits `Item(_)` after a successful
/// `fn_registry` lookup).
#[doc(hidden)]
#[must_use]
#[cfg(any(test, feature = "internal-test-hooks"))]
pub fn run_call_shape_gates_for_test(
    items: &[HirItem],
    monomorphisations: &[crate::monomorph::MonomorphizedFn],
) -> Vec<HirDiagnostic> {
    let mut diagnostics = Vec::new();
    check_call_shape_gates(items, monomorphisations, &mut diagnostics);
    diagnostics
}

pub(super) fn check_call_shape_gates(
    items: &[HirItem],
    monomorphisations: &[crate::monomorph::MonomorphizedFn],
    diagnostics: &mut Vec<HirDiagnostic>,
) {
    let callable = build_callable_set(items, monomorphisations);
    for item in items {
        scan_item_for_call_shape(item, &callable, diagnostics);
    }
}

/// What the call-shape gate admits, in the two authorities MIR itself uses.
///
/// `declarations` is the primary one: MIR resolves a `CallTarget::User` call
/// through `direct_call_symbols[declaration]` and never reads the callee's
/// name, so the gate must ask the same question. A declaration may legitimately
/// be emitted under a name no call site spells — an imported `pub fn` is called
/// through its module-qualified spelling but emitted under its source-declared
/// one — and a name-only gate rejects exactly those.
///
/// `names` remains for every callee the checker did not resolve to a user
/// declaration: runtime bridges, stdlib catalog entries, and the seeded
/// registry symbols, which have no `DefId`.
pub(super) struct CallShapeTargets {
    pub(super) names: std::collections::HashSet<String>,
    pub(super) declarations: std::collections::HashSet<hew_types::DefId>,
}

/// Build what the MIR `Expr::Call` dispatch chain accepts.
///
/// `declarations` mirrors MIR's `direct_call_symbols` key set: every declaration
/// that realizes an emitted body (function, extern fn, impl method).
///
/// `names` mirrors MIR's `module_fn_names` plus the runtime-symbol bridges
/// consulted ahead of it in `runtime_symbol_for_call_expr`. Includes:
///
/// 1. Every `stdlib_catalog::entries()` name (intrinsic linkage included —
///    intrinsics route through `runtime_symbol_for_call_expr` before the
///    fail-closed arm).
/// 2. Every user `HirItem::Function` name — monomorphic AND generic origin.
///    The generic-origin name is admissible because MIR's
///    `call_site_type_args` + mangled-name dispatch resolves the call at the
///    site; if the mangled lookup fails MIR's defense-in-depth still catches
///    it. Excluding generic origins here would false-positively reject every
///    direct call to a generic user function.
/// 3. Every `HirItem::ExternFn` name.
/// 4. Every monomorphisation's mangled name.
/// 5. The hard-coded runtime-ABI bridge `supervisor_stop` that
///    `seed_stdlib_fn_registry` adds to `fn_registry` for the same reason.
pub(super) fn build_callable_set(
    items: &[HirItem],
    monomorphisations: &[crate::monomorph::MonomorphizedFn],
) -> CallShapeTargets {
    let mut declarations: std::collections::HashSet<hew_types::DefId> =
        std::collections::HashSet::new();
    for item in items {
        match item {
            HirItem::Function(f) => {
                declarations.insert(f.declaration);
            }
            HirItem::ExternFn(ef) => {
                declarations.insert(ef.declaration);
            }
            HirItem::Impl(block) => {
                declarations.extend(block.method_ids.iter().flatten().copied());
            }
            HirItem::Actor(actor) => {
                // An actor-body plain `fn` realizes an emitted body too: MIR's
                // actor lowering emits `{Actor}__fn__{name}` for it and maps
                // the declaration to that symbol in `direct_call_symbols`.
                // Receive handlers and lifecycle hooks stay out — the runtime
                // trampolines enter those, and no Hew call site may name them.
                declarations.extend(actor.methods.iter().map(|m| m.declaration));
            }
            _ => {}
        }
    }
    let mut set: std::collections::HashSet<String> = std::collections::HashSet::new();
    for entry in stdlib_catalog::entries() {
        // W4.001 Stage C0b: LayoutDescriptorSymbol rows declare runtime
        // statics, not callables. Excluding them here keeps the HIR
        // verifier's callable-set in sync with MIR's `module_fn_names`
        // (which already skips this linkage) and with `fn_registry`
        // (which now also skips it). A user-written call to a descriptor
        // symbol fails closed at HIR resolution with no callable found.
        if matches!(
            entry.linkage,
            stdlib_catalog::BuiltinLinkage::LayoutDescriptorSymbol { .. }
        ) {
            continue;
        }
        set.insert(entry.name.to_string());
        if let Some(symbol) = entry.linkage.runtime_symbol() {
            set.insert(symbol.to_string());
        }
    }
    for item in items {
        match item {
            HirItem::Function(f) => {
                set.insert(f.name.clone());
            }
            HirItem::ExternFn(ef) => {
                set.insert(ef.name.clone());
            }
            _ => {}
        }
    }
    for mono in monomorphisations {
        set.insert(mono.mangled_name.clone());
    }
    // No-AST-item builtins seeded into `fn_registry` by
    // `seed_stdlib_fn_registry`. These resolve to `ResolvedRef::Builtin`
    // (accepted by the gate without a set lookup); the names stay here so
    // the set continues to mirror the seeded registry contents.
    set.insert("supervisor_stop".to_string());
    set.insert("link".to_string());
    set.insert("monitor".to_string());
    set.insert("unlink".to_string());
    // Static-pool accessor symbols: the HIR pool-method intercept lowers
    // `sup.pool.get(i)` / `.len()` to a `Call` naming these runtime symbols so
    // the call-shape gate accepts them; MIR routes by site (`pool_accessor_sites`)
    // and emits the pool ABI. `sup.pool[i]` lowers as an `Index` and never hits
    // this gate.
    set.insert("hew_supervisor_pool_child_get".to_string());
    set.insert("hew_supervisor_pool_len".to_string());
    // Pipe layout-witness recv/send symbols — seeded into `fn_registry` by
    // `seed_stdlib_fn_registry` and lowered as suspensions by SIR.
    for name in [
        "hew_stream_next_layout",
        "hew_stream_try_next_layout",
        "hew_stream_send_layout",
        "hew_stream_try_send_layout",
    ] {
        set.insert(name.to_string());
    }
    CallShapeTargets {
        names: set,
        declarations,
    }
}

pub(super) fn scan_item_for_call_shape(
    item: &HirItem,
    callable: &CallShapeTargets,
    diagnostics: &mut Vec<HirDiagnostic>,
) {
    match item {
        HirItem::Function(f) => scan_block_for_call_shape(&f.body, callable, diagnostics),
        HirItem::Actor(actor) => {
            if let Some(init) = &actor.init {
                scan_block_for_call_shape(&init.body, callable, diagnostics);
            }
            for handler in &actor.receive_handlers {
                scan_block_for_call_shape(&handler.body, callable, diagnostics);
            }
            for method in &actor.methods {
                scan_block_for_call_shape(&method.body, callable, diagnostics);
            }
            for hook in &actor.lifecycle_hooks {
                scan_block_for_call_shape(&hook.body, callable, diagnostics);
            }
        }
        // TypeDecl, Record, Supervisor, Impl, ExternFn carry no user
        // expression bodies that contain `HirExprKind::Call` nodes at this
        // stage (impl methods are also re-emitted as `HirItem::Function`
        // entries, so their bodies are covered by that arm).
        HirItem::TypeDecl(_)
        | HirItem::Record(_)
        | HirItem::Supervisor(_)
        | HirItem::Impl(_)
        | HirItem::ExternFn(_)
        | HirItem::Const(_) => {}
    }
}

pub(super) fn scan_block_for_call_shape(
    block: &HirBlock,
    callable: &CallShapeTargets,
    diagnostics: &mut Vec<HirDiagnostic>,
) {
    for stmt in &block.statements {
        match &stmt.kind {
            HirStmtKind::Let(_, Some(init)) => {
                scan_expr_for_call_shape(init, callable, diagnostics);
            }
            HirStmtKind::Destructure { value, .. } => {
                scan_expr_for_call_shape(value, callable, diagnostics);
            }
            HirStmtKind::Assign { target, value, .. } => {
                scan_expr_for_call_shape(target, callable, diagnostics);
                scan_expr_for_call_shape(value, callable, diagnostics);
            }
            HirStmtKind::Expr(e) | HirStmtKind::Return(Some(e)) => {
                scan_expr_for_call_shape(e, callable, diagnostics);
            }
            HirStmtKind::Let(_, None) | HirStmtKind::Return(None) => {}
            HirStmtKind::Defer { body, .. } => {
                scan_expr_for_call_shape(body, callable, diagnostics);
            }
        }
    }
    if let Some(tail) = &block.tail {
        scan_expr_for_call_shape(tail, callable, diagnostics);
    }
}

/// Dispatch one `Item` to its body-bearing positions. Centralised so the
/// root-items loop and the module-graph loop emit identical coverage, and so
/// `Item::Machine`'s four positions (per A242) are handled in one place.
pub(super) fn scan_item_for_supervisor_spawn(
    item: &Item,
    current_module: SupervisorScanScope<'_>,
    registry: &SupervisorRegistry,
    diagnostics: &mut Vec<HirDiagnostic>,
) {
    match item {
        Item::Function(fn_decl) => {
            scan_block_for_supervisor_spawn(&fn_decl.body, current_module, registry, diagnostics);
        }
        Item::Actor(actor_decl) => {
            if let Some(init) = &actor_decl.init {
                scan_block_for_supervisor_spawn(&init.body, current_module, registry, diagnostics);
            }
            for recv_fn in &actor_decl.receive_fns {
                scan_block_for_supervisor_spawn(
                    &recv_fn.body,
                    current_module,
                    registry,
                    diagnostics,
                );
            }
            for method in &actor_decl.methods {
                scan_block_for_supervisor_spawn(
                    &method.body,
                    current_module,
                    registry,
                    diagnostics,
                );
            }
        }
        Item::Impl(impl_decl) => {
            for method in &impl_decl.methods {
                scan_block_for_supervisor_spawn(
                    &method.body,
                    current_module,
                    registry,
                    diagnostics,
                );
            }
        }
        // Machine bodies never reach here: normalization rewrites a machine
        // into ordinary declarations before checking, and a machine it refuses
        // fails type check before HIR. The expanded bodies are walked through
        // `Item::Impl` like any other method.
        // Const, Trait, Supervisor, Record, TypeDecl, TypeAlias, Wire,
        // Import, ExternBlock: no user expression bodies that can call
        // `spawn`.
        _ => {}
    }
}

pub(super) fn scan_block_for_supervisor_spawn(
    block: &hew_parser::ast::Block,
    current_module: SupervisorScanScope<'_>,
    registry: &SupervisorRegistry,
    diagnostics: &mut Vec<HirDiagnostic>,
) {
    for (stmt, _) in &block.stmts {
        scan_stmt_for_supervisor_spawn(stmt, current_module, registry, diagnostics);
    }
    if let Some(trailing) = &block.trailing_expr {
        scan_expr_for_supervisor_spawn(&trailing.0, current_module, registry, diagnostics);
    }
}

#[allow(
    clippy::too_many_lines,
    reason = "exhaustive HirExprKind match — every recursing variant is named \
              so adding a new variant forces a conscious decision"
)]
#[allow(
    clippy::match_same_arms,
    reason = "structurally identical recursion bodies on distinct HirExprKind \
              variants (e.g. ActorSend/ActorAsk, MachineEmit/StructInit-field \
              walks) are kept separate so adding a new variant forces an \
              explicit per-variant decision rather than silently joining a \
              merged arm"
)]
pub(super) fn scan_expr_for_call_shape(
    expr: &HirExpr,
    callable: &CallShapeTargets,
    diagnostics: &mut Vec<HirDiagnostic>,
) {
    match &expr.kind {
        HirExprKind::RcIntrinsic {
            receiver, value, ..
        } => {
            for operand in receiver.iter().chain(value.iter()) {
                scan_expr_for_call_shape(operand, callable, diagnostics);
            }
        }
        HirExprKind::Call {
            target,
            callee,
            args,
            ..
        } => {
            // Site 4194 + 4236 predicates fire on the callee's resolution.
            // Recurse first so any nested invalid call inside `callee` or
            // `args` still surfaces, then apply the gate to this site.
            scan_expr_for_call_shape(callee, callable, diagnostics);
            for arg in args {
                scan_expr_for_call_shape(arg, callable, diagnostics);
            }
            if let HirExprKind::BindingRef { name, resolved } = &callee.kind {
                match resolved {
                    ResolvedRef::Item(_) => {
                        // Ask the question MIR asks. A checker-resolved user
                        // call carries the declaration identity MIR projects
                        // its linker symbol from; the callee's spelling is
                        // presentation only, and a declaration emitted under a
                        // different one is still callable.
                        let admitted = match target {
                            hew_types::CallTarget::User(declaration) => {
                                callable.declarations.contains(declaration)
                            }
                            _ => callable.names.contains(name),
                        };
                        if !admitted {
                            let source_name = name.replace("::", ".");
                            let message = format!(
                                "call to `{source_name}` has no MIR body or runtime-ABI lowering; \
                                 only module functions, extern fns, monomorphisation \
                                 instantiations, and recognised runtime symbols are \
                                 callable here"
                            );
                            diagnostics.push(HirDiagnostic::new(
                                HirDiagnosticKind::CallableUnsupportedInMir { name: name.clone() },
                                callee.span.clone(),
                                message,
                            ));
                        }
                    }
                    ResolvedRef::Unresolved => {
                        // Narrow predicate: only fire when the unresolved
                        // callee has a callable static type. A `Binding(_)`
                        // resolved callee (closure binding, fn parameter,
                        // let-bound function value) is admitted and lowered
                        // by MIR's `CallClosure` arm; rejecting it here
                        // would block valid programs such as
                        // `let f = |x| x + 1; f(2)`.
                        let callable_ty = matches!(
                            callee.ty,
                            ResolvedTy::Function { .. } | ResolvedTy::Closure { .. }
                        );
                        if callable_ty {
                            diagnostics.push(HirDiagnostic::new(
                                HirDiagnosticKind::IndirectCallUnsupported {
                                    callee: format!("unresolved binding `{name}`"),
                                    callee_ty: format!("{:?}", callee.ty),
                                },
                                callee.span.clone(),
                                "indirect call through an unresolved callable binding has no \
                                 MIR dispatch path; only direct calls to module-declared \
                                 functions, extern fns, and recognised runtime symbols are \
                                 supported"
                                    .to_string(),
                            ));
                        }
                    }
                    ResolvedRef::Binding(_) => {
                        // Closure / fn-value bindings: MIR's `CallClosure`
                        // arm dispatches these. Intentionally NOT rejected.
                    }
                    ResolvedRef::Const(_) => {
                        // A `const` is never a callee. If a const name appears
                        // in callee position the checker has already rejected
                        // it as a non-callable type; nothing to gate here.
                    }
                    ResolvedRef::Builtin(_) => {
                        // A typed runtime-builtin reference is callable by
                        // construction: the family is catalog-closed and the
                        // MIR runtime-call producer dispatches on it directly.
                    }
                }
            }
        }
        HirExprKind::Binary { left, right, .. } | HirExprKind::IdentityCompare { left, right } => {
            scan_expr_for_call_shape(left, callable, diagnostics);
            scan_expr_for_call_shape(right, callable, diagnostics);
        }
        HirExprKind::Unary { operand, .. } | HirExprKind::WireCodec { operand, .. } => {
            scan_expr_for_call_shape(operand, callable, diagnostics);
        }
        HirExprKind::ConnAwaitRead { conn, .. } => {
            scan_expr_for_call_shape(conn, callable, diagnostics);
        }
        HirExprKind::AwaitRestart { child } | HirExprKind::AwaitTask { operand: child, .. } => {
            scan_expr_for_call_shape(child, callable, diagnostics);
        }
        HirExprKind::ListenerAwaitAccept { listener, .. } => {
            scan_expr_for_call_shape(listener, callable, diagnostics);
        }
        HirExprKind::StreamRecvAwait { stream, .. } => {
            scan_expr_for_call_shape(stream, callable, diagnostics);
        }
        HirExprKind::ArrayRepeat { value }
        | HirExprKind::NumericCast { value, .. }
        | HirExprKind::SaturatingWidthCast { value, .. }
        | HirExprKind::TryWidthCast { value, .. } => {
            scan_expr_for_call_shape(value, callable, diagnostics);
        }
        HirExprKind::TupleLiteral { elements } | HirExprKind::ArrayLiteral { elements } => {
            for elem in elements {
                scan_expr_for_call_shape(elem, callable, diagnostics);
            }
        }
        HirExprKind::Spawn { args, .. } => {
            for (_, v) in args {
                scan_expr_for_call_shape(v, callable, diagnostics);
            }
        }
        HirExprKind::ActorMessage { receiver, args, .. }
        | HirExprKind::ActorDelivery { receiver, args, .. }
        | HirExprKind::ActorAsk { receiver, args, .. }
        | HirExprKind::ActorGenStream { receiver, args, .. } => {
            scan_expr_for_call_shape(receiver, callable, diagnostics);
            for a in args {
                scan_expr_for_call_shape(a, callable, diagnostics);
            }
        }
        HirExprKind::RemoteActorAsk {
            receiver,
            msg,
            timeout_ms,
            ..
        } => {
            scan_expr_for_call_shape(receiver, callable, diagnostics);
            scan_expr_for_call_shape(msg, callable, diagnostics);
            scan_expr_for_call_shape(timeout_ms, callable, diagnostics);
        }
        HirExprKind::RemoteActorSend { receiver, msg } => {
            scan_expr_for_call_shape(receiver, callable, diagnostics);
            scan_expr_for_call_shape(msg, callable, diagnostics);
        }
        HirExprKind::Block(b) => scan_block_for_call_shape(b, callable, diagnostics),
        HirExprKind::If {
            condition,
            then_expr,
            else_expr,
        } => {
            scan_expr_for_call_shape(condition, callable, diagnostics);
            scan_expr_for_call_shape(then_expr, callable, diagnostics);
            if let Some(e) = else_expr {
                scan_expr_for_call_shape(e, callable, diagnostics);
            }
        }
        HirExprKind::StructInit { fields, base, .. } => {
            for (_, v) in fields {
                scan_expr_for_call_shape(v, callable, diagnostics);
            }
            if let Some(b) = base {
                scan_expr_for_call_shape(b, callable, diagnostics);
            }
        }
        HirExprKind::FieldAccess { object, .. } => {
            scan_expr_for_call_shape(object, callable, diagnostics);
        }
        HirExprKind::Scope { body }
        | HirExprKind::Race { body }
        | HirExprKind::ForkBlock { body, .. }
        | HirExprKind::GenBlock { body, .. } => {
            scan_block_for_call_shape(body, callable, diagnostics);
        }
        HirExprKind::ScopeRecovery { scope, handler, .. } => {
            scan_expr_for_call_shape(scope, callable, diagnostics);
            scan_expr_for_call_shape(handler, callable, diagnostics);
        }
        HirExprKind::ScopeDeadline { duration, body } => {
            scan_expr_for_call_shape(duration, callable, diagnostics);
            scan_block_for_call_shape(body, callable, diagnostics);
        }
        HirExprKind::Select(select) => {
            for arm in &select.arms {
                match &arm.kind {
                    HirSelectArmKind::StreamNext { stream } => {
                        scan_expr_for_call_shape(stream, callable, diagnostics);
                    }
                    HirSelectArmKind::ActorAsk { call } => {
                        scan_expr_for_call_shape(call, callable, diagnostics);
                    }
                    HirSelectArmKind::TaskAwait { task } => {
                        scan_expr_for_call_shape(task, callable, diagnostics);
                    }
                    HirSelectArmKind::AfterTimer { duration } => {
                        scan_expr_for_call_shape(duration, callable, diagnostics);
                    }
                }
                scan_expr_for_call_shape(&arm.body, callable, diagnostics);
            }
        }
        HirExprKind::SpawnLambdaActor { body, .. } | HirExprKind::Closure { body, .. } => {
            scan_expr_for_call_shape(body, callable, diagnostics);
        }
        HirExprKind::Yield { value: Some(v), .. } => {
            scan_expr_for_call_shape(v, callable, diagnostics);
        }
        HirExprKind::TupleIndex { tuple, .. } => {
            scan_expr_for_call_shape(tuple, callable, diagnostics);
        }
        HirExprKind::Index { container, index }
        | HirExprKind::BorrowedIndex { container, index } => {
            scan_expr_for_call_shape(container, callable, diagnostics);
            scan_expr_for_call_shape(index, callable, diagnostics);
        }
        HirExprKind::Slice {
            container,
            start,
            end,
            ..
        } => {
            scan_expr_for_call_shape(container, callable, diagnostics);
            if let Some(s) = start {
                scan_expr_for_call_shape(s, callable, diagnostics);
            }
            if let Some(e) = end {
                scan_expr_for_call_shape(e, callable, diagnostics);
            }
        }
        HirExprKind::CoerceToDynTrait { value, .. } => {
            scan_expr_for_call_shape(value, callable, diagnostics);
        }
        HirExprKind::CallDynMethod { receiver, args, .. }
        | HirExprKind::ResolvedImplCall { receiver, args, .. }
        | HirExprKind::CallTraitMethodStatic { receiver, args, .. }
        | HirExprKind::VarSelfMethodCall { receiver, args, .. } => {
            scan_expr_for_call_shape(receiver, callable, diagnostics);
            for a in args {
                scan_expr_for_call_shape(a, callable, diagnostics);
            }
        }
        HirExprKind::CancellationTokenIsCancelled { receiver }
        | HirExprKind::GeneratorNext { receiver, .. }
        | HirExprKind::RecordCloneCall { src: receiver, .. }
        | HirExprKind::SubsumedValue {
            source: receiver, ..
        } => {
            scan_expr_for_call_shape(receiver, callable, diagnostics);
        }
        HirExprKind::MachineVariantCtor {
            payload: Some(fields),
            ..
        } => {
            for (_, v) in fields {
                scan_expr_for_call_shape(v, callable, diagnostics);
            }
        }
        HirExprKind::While {
            condition, body, ..
        } => {
            scan_expr_for_call_shape(condition, callable, diagnostics);
            scan_block_for_call_shape(body, callable, diagnostics);
        }
        HirExprKind::ForRange {
            start,
            end,
            step,
            body,
            ..
        } => {
            scan_expr_for_call_shape(start, callable, diagnostics);
            scan_expr_for_call_shape(end, callable, diagnostics);
            scan_expr_for_call_shape(step, callable, diagnostics);
            scan_block_for_call_shape(body, callable, diagnostics);
        }
        HirExprKind::Match { scrutinee, arms } => {
            scan_expr_for_call_shape(scrutinee, callable, diagnostics);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    scan_expr_for_call_shape(guard, callable, diagnostics);
                }
                scan_expr_for_call_shape(&arm.body, callable, diagnostics);
            }
        }
        HirExprKind::Break { value, .. } | HirExprKind::Return { value } => {
            if let Some(value) = value {
                scan_expr_for_call_shape(value, callable, diagnostics);
            }
        }
        HirExprKind::Loop { body, .. } => {
            scan_block_for_call_shape(body, callable, diagnostics);
        }
        // Leaf / no-sub-expression variants: Literal, RegexLiteralRef,
        // BindingRef, ContextReader, AwaitTask, Yield { value: None },
        // MachineVariantCtor { payload: None }, MachineFieldAccess,
        // MachineEventFieldAccess, Continue, ActorSelf, Unsupported. Nothing to
        // recurse into.
        HirExprKind::Literal(_)
        | HirExprKind::RegexLiteralRef { .. }
        | HirExprKind::BindingRef { .. }
        | HirExprKind::ContextReader { .. }
        | HirExprKind::Yield { value: None, .. }
        | HirExprKind::MachineVariantCtor { payload: None, .. }
        | HirExprKind::Continue { .. }
        | HirExprKind::ActorSelf
        | HirExprKind::Unsupported(_) => {}
    }
}

#[allow(
    clippy::match_same_arms,
    reason = "explicit per-Stmt-variant arms read more clearly than collapsed or-patterns for this walker"
)]
pub(super) fn scan_stmt_for_supervisor_spawn(
    stmt: &hew_parser::ast::Stmt,
    current_module: SupervisorScanScope<'_>,
    registry: &SupervisorRegistry,
    diagnostics: &mut Vec<HirDiagnostic>,
) {
    match stmt {
        Stmt::Let { value: Some(v), .. } | Stmt::Var { value: Some(v), .. } => {
            scan_expr_for_supervisor_spawn(&v.0, current_module, registry, diagnostics);
        }
        Stmt::Assign { target, value, .. } => {
            scan_expr_for_supervisor_spawn(&target.0, current_module, registry, diagnostics);
            scan_expr_for_supervisor_spawn(&value.0, current_module, registry, diagnostics);
        }
        Stmt::Expression(e) => {
            scan_expr_for_supervisor_spawn(&e.0, current_module, registry, diagnostics);
        }
        Stmt::Return(Some(e)) => {
            scan_expr_for_supervisor_spawn(&e.0, current_module, registry, diagnostics);
        }
        Stmt::Defer(e) => {
            scan_expr_for_supervisor_spawn(&e.0, current_module, registry, diagnostics);
        }
        Stmt::Break { value: Some(v), .. } => {
            scan_expr_for_supervisor_spawn(&v.0, current_module, registry, diagnostics);
        }
        Stmt::WhileLet {
            conditions, body, ..
        } => {
            for expr in condition_exprs(conditions) {
                scan_expr_for_supervisor_spawn(&expr.0, current_module, registry, diagnostics);
            }
            scan_block_for_supervisor_spawn(body, current_module, registry, diagnostics);
        }
        Stmt::If {
            condition,
            then_block,
            else_block,
        } => {
            scan_expr_for_supervisor_spawn(&condition.0, current_module, registry, diagnostics);
            scan_block_for_supervisor_spawn(then_block, current_module, registry, diagnostics);
            if let Some(eb) = else_block {
                scan_else_block_for_supervisor_spawn(eb, current_module, registry, diagnostics);
            }
        }
        Stmt::IfLet {
            conditions,
            body,
            else_body,
        } => {
            for expr in condition_exprs(conditions) {
                scan_expr_for_supervisor_spawn(&expr.0, current_module, registry, diagnostics);
            }
            scan_block_for_supervisor_spawn(body, current_module, registry, diagnostics);
            if let Some(eb) = else_body {
                scan_expr_for_supervisor_spawn(&eb.0, current_module, registry, diagnostics);
            }
        }
        Stmt::Match { scrutinee, arms } => {
            scan_expr_for_supervisor_spawn(&scrutinee.0, current_module, registry, diagnostics);
            for arm in arms {
                if let Some(g) = &arm.guard {
                    scan_expr_for_supervisor_spawn(&g.0, current_module, registry, diagnostics);
                }
                scan_expr_for_supervisor_spawn(&arm.body.0, current_module, registry, diagnostics);
            }
        }
        Stmt::Loop { body, .. } => {
            scan_block_for_supervisor_spawn(body, current_module, registry, diagnostics);
        }
        Stmt::For { iterable, body, .. } => {
            scan_expr_for_supervisor_spawn(&iterable.0, current_module, registry, diagnostics);
            scan_block_for_supervisor_spawn(body, current_module, registry, diagnostics);
        }
        Stmt::While {
            condition, body, ..
        } => {
            scan_expr_for_supervisor_spawn(&condition.0, current_module, registry, diagnostics);
            scan_block_for_supervisor_spawn(body, current_module, registry, diagnostics);
        }
        // Stmt::Break, Stmt::Continue, Stmt::Return(None), and other leaf
        // statements carry no sub-expression to scan.
        _ => {}
    }
}

pub(super) fn scan_else_block_for_supervisor_spawn(
    eb: &hew_parser::ast::ElseBlock,
    current_module: SupervisorScanScope<'_>,
    registry: &SupervisorRegistry,
    diagnostics: &mut Vec<HirDiagnostic>,
) {
    if let Some(stmt) = &eb.if_stmt {
        scan_stmt_for_supervisor_spawn(&stmt.0, current_module, registry, diagnostics);
    }
    if let Some(b) = &eb.block {
        scan_block_for_supervisor_spawn(b, current_module, registry, diagnostics);
    }
}

/// Recursively walk an expression tree looking for `Expr::Spawn` whose target
/// names a known supervisor and whose args list is non-empty.
///
/// `current_module` scopes bare-identifier resolution: `None` for root-program
/// bodies, `Some(short_name)` for non-root module bodies. The supervisor set
/// consulted for a bare `spawn Name(args)` is the set the checker would use
/// when resolving that bare identifier — root's set at root, the module's own
/// set in a module body. Module-qualified `spawn mod.Name(args)` always
/// consults `mod`'s set regardless of `current_module`.
#[allow(
    clippy::too_many_lines,
    reason = "exhaustive Expr-variant walker mirrors scan_expr_for_blocking_recv above"
)]
pub(super) fn scan_expr_for_supervisor_spawn(
    expr: &Expr,
    current_module: SupervisorScanScope<'_>,
    registry: &SupervisorRegistry,
    diagnostics: &mut Vec<HirDiagnostic>,
) {
    match expr {
        Expr::Spawn { target, args, .. } => {
            // Module-scoped supervisor lookup.
            //
            // Two recognised spawn-target surface forms carry different
            // module qualifiers; the gate must consult the *correct*
            // module's supervisor set — not a flat union — to avoid both
            // false negatives (a module-local supervisor slipping through
            // because the root has no same-named supervisor) and false
            // positives (a root supervisor name rejecting an unrelated
            // module-local actor that happens to share the name).
            //
            //   bare `spawn Sup(args)` at root            → root supervisors
            //   bare `spawn Sup(args)` in module `M`      → M's supervisors
            //   `spawn mod.Sup(args)` (anywhere)          → mod's supervisors
            //
            // Bare identifiers in module bodies resolve against the module's
            // own scope; root names are not auto-imported, so falling back
            // to root's set would generate false positives. A module that
            // intends to spawn a root supervisor must reach it via a
            // module-qualified path, which dispatches through the
            // `Expr::FieldAccess` arm below. Discharges A237 / LESSONS
            // `string-identifier-fragility-vs-structured-resolution` and
            // the rev2 independent review finding on module-context threading.
            let resolved: Option<(&str, bool)> = match &target.0 {
                Expr::Ident(name) => {
                    let set = match current_module.module {
                        Some(m) => registry.by_module.get(m),
                        None => Some(&registry.root),
                    };
                    set.and_then(|s| {
                        s.get_key_value(name.name.as_str())
                            .map(|(n, has_cfg)| (n.as_str(), *has_cfg))
                    })
                }
                Expr::FieldAccess { object, field } => {
                    if let Expr::Ident(module) = &object.0 {
                        registry
                            .resolve_module_binding(current_module, module.name.as_str())
                            .and_then(|owner| registry.by_module.get(owner))
                            .and_then(|set| {
                                set.get_key_value(field.0.name.as_str())
                                    .map(|(n, has_cfg)| (n.as_str(), *has_cfg))
                            })
                    } else {
                        // Non-`Identifier` object (e.g. `foo().Sup`) is not a
                        // module-qualified spawn; the checker would already
                        // reject this shape upstream, so the gate stays
                        // silent here.
                        None
                    }
                }
                _ => None,
            };
            if let Some((name, has_config_param)) = resolved {
                // A config supervisor (`supervisor App(config: T)`) admits
                // exactly its config arg: `spawn App(config: value)`. A
                // no-config supervisor admits none. The gate rejects only the
                // unsupported shapes — args on a no-config supervisor, or more
                // than the single config arg on a config supervisor.
                let admits_config_arg = has_config_param && args.len() == 1;
                if !args.is_empty() && !admits_config_arg {
                    let detail = if has_config_param {
                        format!(
                            "supervisor `{name}` takes a single construction-time config \
                             argument; spawn it as `spawn {name}(config: value)` with exactly \
                             one config value."
                        )
                    } else {
                        format!(
                            "supervisor `{name}` cannot be spawned with init args: \
                             supervisors take their child specs declaratively in the \
                             `supervisor` body, and this supervisor declares no `(config: T)` \
                             param. Use `spawn {name}` with no arguments."
                        )
                    };
                    diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::SupervisorSpawnArgsUnsupported {
                            supervisor_name: name.to_string(),
                        },
                        target.1.clone(),
                        detail,
                    ));
                }
            }
            // Recurse into target + args so a nested supervisor spawn inside
            // an arg expression is also caught.
            scan_expr_for_supervisor_spawn(&target.0, current_module, registry, diagnostics);
            for (_, v) in args {
                scan_expr_for_supervisor_spawn(&v.0, current_module, registry, diagnostics);
            }
        }
        Expr::MethodCall { receiver, args, .. } => {
            scan_expr_for_supervisor_spawn(&receiver.0, current_module, registry, diagnostics);
            for arg in args {
                scan_expr_for_supervisor_spawn(
                    &arg.expr().0,
                    current_module,
                    registry,
                    diagnostics,
                );
            }
        }
        Expr::Call { function, args, .. } => {
            scan_expr_for_supervisor_spawn(&function.0, current_module, registry, diagnostics);
            for arg in args {
                scan_expr_for_supervisor_spawn(
                    &arg.expr().0,
                    current_module,
                    registry,
                    diagnostics,
                );
            }
        }
        Expr::Binary { left, right, .. }
        | Expr::Coalesce { left, right }
        | Expr::Handle {
            operand: left,
            body: right,
            ..
        } => {
            scan_expr_for_supervisor_spawn(&left.0, current_module, registry, diagnostics);
            scan_expr_for_supervisor_spawn(&right.0, current_module, registry, diagnostics);
        }
        Expr::Unary { operand, .. } => {
            scan_expr_for_supervisor_spawn(&operand.0, current_module, registry, diagnostics);
        }
        Expr::Tuple(es) | Expr::Race(es) => {
            for e in es {
                scan_expr_for_supervisor_spawn(&e.0, current_module, registry, diagnostics);
            }
        }
        Expr::Array(elements) => {
            for element in elements {
                scan_expr_for_supervisor_spawn(
                    &element.expr().0,
                    current_module,
                    registry,
                    diagnostics,
                );
            }
        }
        Expr::ArrayRepeat { value, count } => {
            scan_expr_for_supervisor_spawn(&value.0, current_module, registry, diagnostics);
            scan_expr_for_supervisor_spawn(&count.0, current_module, registry, diagnostics);
        }
        Expr::Block(b)
        | Expr::Scope { body: b }
        | Expr::ForkBlock { body: b }
        | Expr::GenBlock { body: b } => {
            scan_block_for_supervisor_spawn(b, current_module, registry, diagnostics);
        }
        Expr::If {
            condition,
            then_block,
            else_block,
            ..
        } => {
            scan_expr_for_supervisor_spawn(&condition.0, current_module, registry, diagnostics);
            scan_expr_for_supervisor_spawn(&then_block.0, current_module, registry, diagnostics);
            if let Some(e) = else_block {
                scan_expr_for_supervisor_spawn(&e.0, current_module, registry, diagnostics);
            }
        }
        Expr::IfLet {
            conditions,
            body,
            else_body,
        } => {
            for expr in condition_exprs(conditions) {
                scan_expr_for_supervisor_spawn(&expr.0, current_module, registry, diagnostics);
            }
            scan_block_for_supervisor_spawn(body, current_module, registry, diagnostics);
            if let Some(b) = else_body {
                scan_expr_for_supervisor_spawn(&b.0, current_module, registry, diagnostics);
            }
        }
        Expr::Match { scrutinee, arms } => {
            scan_expr_for_supervisor_spawn(&scrutinee.0, current_module, registry, diagnostics);
            for arm in arms {
                if let Some(g) = &arm.guard {
                    scan_expr_for_supervisor_spawn(&g.0, current_module, registry, diagnostics);
                }
                scan_expr_for_supervisor_spawn(&arm.body.0, current_module, registry, diagnostics);
            }
        }
        Expr::Lambda { body, .. } | Expr::SpawnLambdaActor { body, .. } => {
            scan_expr_for_supervisor_spawn(&body.0, current_module, registry, diagnostics);
        }
        Expr::ScopeDeadline { duration, body } => {
            scan_expr_for_supervisor_spawn(&duration.0, current_module, registry, diagnostics);
            scan_block_for_supervisor_spawn(body, current_module, registry, diagnostics);
        }
        Expr::ForkChild { expr, .. } | Expr::Cast { expr, .. } => {
            scan_expr_for_supervisor_spawn(&expr.0, current_module, registry, diagnostics);
        }
        Expr::StructInit { fields, base, .. } => {
            for (_, v) in fields {
                scan_expr_for_supervisor_spawn(&v.0, current_module, registry, diagnostics);
            }
            if let Some(b) = base {
                scan_expr_for_supervisor_spawn(&b.0, current_module, registry, diagnostics);
            }
        }
        Expr::MapLiteral { entries } => {
            for (k, v) in entries {
                scan_expr_for_supervisor_spawn(&k.0, current_module, registry, diagnostics);
                scan_expr_for_supervisor_spawn(&v.0, current_module, registry, diagnostics);
            }
        }
        Expr::InterpolatedString(parts) => {
            for part in parts {
                if let hew_parser::ast::StringPart::Expr(e)
                | hew_parser::ast::StringPart::StructuralExpr(e) = part
                {
                    scan_expr_for_supervisor_spawn(&e.0, current_module, registry, diagnostics);
                }
            }
        }
        Expr::Select { arms, timeout } => {
            for arm in arms {
                scan_expr_for_supervisor_spawn(
                    &arm.source.0,
                    current_module,
                    registry,
                    diagnostics,
                );
                scan_expr_for_supervisor_spawn(&arm.body.0, current_module, registry, diagnostics);
            }
            if let Some(t) = timeout {
                scan_expr_for_supervisor_spawn(
                    &t.duration.0,
                    current_module,
                    registry,
                    diagnostics,
                );
                scan_expr_for_supervisor_spawn(&t.body.0, current_module, registry, diagnostics);
            }
        }
        Expr::UnsafeBlock(b) => {
            scan_block_for_supervisor_spawn(b, current_module, registry, diagnostics);
        }
        Expr::FieldAccess { object, .. } | Expr::PostfixTry(object) | Expr::Await(object) => {
            scan_expr_for_supervisor_spawn(&object.0, current_module, registry, diagnostics);
        }
        Expr::Index { object, index } => {
            scan_expr_for_supervisor_spawn(&object.0, current_module, registry, diagnostics);
            scan_expr_for_supervisor_spawn(&index.0, current_module, registry, diagnostics);
        }
        Expr::Is { lhs, rhs } => {
            scan_expr_for_supervisor_spawn(&lhs.0, current_module, registry, diagnostics);
            scan_expr_for_supervisor_spawn(&rhs.0, current_module, registry, diagnostics);
        }
        Expr::Range { start, end, .. } => {
            if let Some(s) = start {
                scan_expr_for_supervisor_spawn(&s.0, current_module, registry, diagnostics);
            }
            if let Some(e) = end {
                scan_expr_for_supervisor_spawn(&e.0, current_module, registry, diagnostics);
            }
        }
        Expr::Yield(Some(e)) => {
            scan_expr_for_supervisor_spawn(&e.0, current_module, registry, diagnostics);
        }
        Expr::MachineEmit { fields, .. } => {
            for (_, v) in fields {
                scan_expr_for_supervisor_spawn(&v.0, current_module, registry, diagnostics);
            }
        }
        // Leaf nodes (Identifier, literals, etc.) and any other Expr variant
        // without sub-expressions: nothing to scan.
        _ => {}
    }
}
