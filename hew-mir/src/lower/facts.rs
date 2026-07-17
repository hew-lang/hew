#[cfg(test)]
use super::*;
#[cfg(not(test))]
use super::{
    named_type_names, BindingId, Builder, BuiltinType, HashMap, HashSet, HirBlock, HirExpr,
    HirExprKind, HirFn, HirItem, HirStmtKind, Instr, IntentKind, LayoutReadiness, MirDiagnostic,
    MirDiagnosticKind, MirStatement, ParamOwnershipFacts, Place, ResolvedRef, ResolvedTy,
    ResourceMarker, ScanCtx, Strategy, ValueClass,
};

/// Flow-insensitive prescan deciding, for every binding in `func`, whether a
/// destructive `{ ..<binding>, f: new }` is a PROVEN unique owner of its heap
/// fields — see the `Builder::funcupdate_base_proven` field and
/// `base_is_safe_for_destructive_funcupdate`.
///
/// A binding is proven iff it is introduced by a `let` or a by-value parameter
/// AND every one of its definitions (the `let` initialiser, every `=`
/// reassignment, and the parameter origin) is a materialised owner
/// (`Builder::expr_is_materialized_owner`) or a move-chain of such
/// (`let c = makeThing(); let d = c; { ..d, f }`). A binding bound from a
/// projection of a still-live owner (`let b = o.inner`), or introduced by any
/// other form (match-arm payload, let-else binder, loop variable), is left
/// UNPROVEN so the base gate fails closed.
///
/// FLOW-INSENSITIVE BY DESIGN: EVERY definition must prove safe, regardless of
/// control flow. This is conservative (a binding that is safely materialised at
/// the update site but aliased on a dead branch is rejected) but SOUND — it
/// cannot admit a binding that aliases a live owner on ANY path, which a
/// last-write-wins flow-sensitive map would (`var c = o.inner; if p { c =
/// makeThing() } { ..c, f }` is unsafe on the `p == false` path). The reassign-
/// loop idiom (`var c = Record { .. }; while .. { c = Record { ..c, f } }`) stays
/// admitted because every definition is a record literal / funcupdate result.
pub(super) fn compute_funcupdate_base_provenance<'f>(
    func: &'f HirFn,
    fresh: &'f HashMap<hew_hir::ItemId, bool>,
) -> HashMap<BindingId, bool> {
    let mut defs: HashMap<BindingId, Vec<&HirExpr>> = HashMap::new();
    let mut let_or_param: HashSet<BindingId> = HashSet::new();
    let mut params: HashSet<BindingId> = HashSet::new();
    for param in &func.params {
        params.insert(param.id);
        let_or_param.insert(param.id);
    }
    collect_binding_defs_in_block(&func.body, &mut defs, &mut let_or_param);

    let mut resolver = BaseOwnerResolver {
        defs,
        let_or_param,
        params,
        memo: HashMap::new(),
        fresh,
    };
    let ids: Vec<BindingId> = resolver.let_or_param.iter().copied().collect();
    for id in ids {
        let mut visiting: HashSet<BindingId> = HashSet::new();
        resolver.resolve(id, &mut visiting);
    }
    resolver.memo
}
/// Memoised resolver for `compute_funcupdate_base_provenance`. `defs` maps each
/// binding to every initialiser/reassignment expression that defines it;
/// `let_or_param` is the set of bindings introduced by a `let` or a parameter
/// (any other origin is unproven); `params` is the by-value parameter subset;
/// `fresh` is the module interprocedural freshness summary
/// (`compute_fn_returns_fresh_owner`) consulted when a definition is a call.
struct BaseOwnerResolver<'f> {
    defs: HashMap<BindingId, Vec<&'f HirExpr>>,
    let_or_param: HashSet<BindingId>,
    params: HashSet<BindingId>,
    memo: HashMap<BindingId, bool>,
    fresh: &'f HashMap<hew_hir::ItemId, bool>,
}
impl<'f> BaseOwnerResolver<'f> {
    /// True iff `{ ..<binding>, f: new }` is a proven unique owner: `binding` is
    /// `let`/param-introduced and EVERY definition proves a materialised owner.
    fn resolve(&mut self, binding: BindingId, visiting: &mut HashSet<BindingId>) -> bool {
        if let Some(&cached) = self.memo.get(&binding) {
            return cached;
        }
        // A binding NOT introduced by a `let`/parameter (match-arm payload,
        // let-else binder, loop variable, or an origin the prescan does not
        // model) cannot be proven a unique owner — fail closed.
        if !self.let_or_param.contains(&binding) {
            return false;
        }
        // A definition cycle (only reachable via pathological reassignment) is
        // not provable — fail closed without recursing forever.
        if !visiting.insert(binding) {
            return false;
        }
        let is_param = self.params.contains(&binding);
        // Clone out the def references (cheap: each is a pointer) so the borrow
        // of `self.defs` is released before the recursive `init_proves` calls.
        let inits: Vec<&'f HirExpr> = self.defs.get(&binding).cloned().unwrap_or_default();
        let result = if is_param {
            // A by-value heap parameter is a BORROW, not a move (LESSONS
            // `by-value-heap-params-are-borrows`): the caller retains ownership,
            // so the parameter's incoming value is NEVER a proven unique owner.
            // Flow-insensitively the borrowed origin reaches every use that a
            // reassignment does not provably dominate (`fn f(mut p) { if c { p =
            // makeInner() } { ..p, x } }` aliases the caller's argument on the
            // `!c` path), so a parameter-introduced base fails closed regardless
            // of any reassignment. The gate sees only the callee body, never the
            // call site: `fn upd(p: Cfg) -> Cfg { Cfg { name: .., ..p } }` is
            // sound for `upd(moved_in_local)` but a use-after-free for
            // `upd(o.cfg)` where the caller's `o` stays live — the override-drop
            // frees `o.cfg.name` under the live owner (empirically: SIGSEGV /
            // scribble-poison read under Guard Malloc). Indistinguishable here,
            // so reject.
            false
        } else if inits.is_empty() {
            // A `let x;` with no initialiser and no reassignment is
            // uninitialised (the move-checker rejects a read) — not an owner.
            false
        } else {
            // Every recorded definition must prove a materialised owner.
            inits.iter().all(|init| self.init_proves(init, visiting))
        };
        visiting.remove(&binding);
        self.memo.insert(binding, result);
        result
    }

    /// Classify a single definition expression of a binding.
    fn init_proves(&mut self, init: &HirExpr, visiting: &mut HashSet<BindingId>) -> bool {
        match &init.kind {
            // A whole-binding move (`let d = c`) CONSUMES the source — the
            // move-checker rejects a later use of `c` — so `d` inherits `c`'s
            // unique-ownership provenance. Recurse: a move-chain of materialised
            // owners is proven; a chain rooted at a live-projection rebind
            // (`let b = o.inner; let c = b; { ..c, f }`) is not.
            HirExprKind::BindingRef {
                resolved: ResolvedRef::Binding(source),
                ..
            } => self.resolve(*source, visiting),
            // Every other initialiser must be a materialised owner directly (a
            // call to a proven-fresh fn / clone / record-or-tuple literal /
            // funcupdate result / Vec element, or a projection rooted at one). A
            // projection of a live binding (`o.inner`, `t.0`) is NOT materialised
            // and fails here; a call is checked against the freshness summary; a
            // construction embedding a whole by-value parameter is rejected via
            // the prescan's `params` set.
            _ => Builder::expr_is_materialized_owner(init, self.fresh, &self.params),
        }
    }
}
/// Collect every binding definition in `block` into `defs` (and record each
/// `let`-introduced binding in `let_ids`) for `compute_funcupdate_base_-
/// provenance`. EXHAUSTIVE over statements: a missed reassignment to a live-
/// projection alias would reopen the funcupdate use-after-free, whereas a missed
/// binding merely fails the gate closed.
fn collect_binding_defs_in_block<'f>(
    block: &'f HirBlock,
    defs: &mut HashMap<BindingId, Vec<&'f HirExpr>>,
    let_ids: &mut HashSet<BindingId>,
) {
    for stmt in &block.statements {
        match &stmt.kind {
            HirStmtKind::Let(binding, init) => {
                let_ids.insert(binding.id);
                if let Some(init) = init {
                    defs.entry(binding.id).or_default().push(init);
                    collect_binding_defs_in_expr(init, defs, let_ids);
                }
            }
            HirStmtKind::Assign { target, value } => {
                // A reassignment of a whole binding redefines its provenance; a
                // field/index assignment (`o.f = ..`) does not rebind the name.
                if let HirExprKind::BindingRef {
                    resolved: ResolvedRef::Binding(binding_id),
                    ..
                } = &target.kind
                {
                    defs.entry(*binding_id).or_default().push(value);
                }
                collect_binding_defs_in_expr(target, defs, let_ids);
                collect_binding_defs_in_expr(value, defs, let_ids);
            }
            HirStmtKind::Expr(expr) | HirStmtKind::Return(Some(expr)) => {
                collect_binding_defs_in_expr(expr, defs, let_ids);
            }
            HirStmtKind::Return(None) => {}
            HirStmtKind::Defer { body, .. } => {
                collect_binding_defs_in_expr(body, defs, let_ids);
            }
            HirStmtKind::LetElse {
                scrutinee,
                success_prelude,
                else_body,
                ..
            } => {
                // The escaping let-else binders are deliberately NOT recorded in
                // `let_ids`: a binder destructured from a scrutinee projection is
                // not a proven unique owner, so it must fail the base gate
                // closed. Still recurse for nested defs/reassignments.
                collect_binding_defs_in_expr(scrutinee, defs, let_ids);
                for prelude_stmt in success_prelude {
                    if let HirStmtKind::Let(_, Some(value)) = &prelude_stmt.kind {
                        collect_binding_defs_in_expr(value, defs, let_ids);
                    }
                }
                collect_binding_defs_in_block(else_body, defs, let_ids);
            }
        }
    }
    if let Some(tail) = &block.tail {
        collect_binding_defs_in_expr(tail, defs, let_ids);
    }
}
/// Recurse into every sub-expression and nested block of `expr` so
/// `collect_binding_defs_in_block` reaches every `let`/`=` in inner scopes.
/// Mirrors the sealed `HirExprKind` surface (cf.
/// `collect_unknown_self_fields_in_expr`) so no nested reassignment is missed.
#[allow(
    clippy::too_many_lines,
    reason = "visitor mirrors the sealed HirExprKind surface so binding-def collection is exhaustive"
)]
fn collect_binding_defs_in_expr<'f>(
    expr: &'f HirExpr,
    defs: &mut HashMap<BindingId, Vec<&'f HirExpr>>,
    let_ids: &mut HashSet<BindingId>,
) {
    match &expr.kind {
        HirExprKind::Literal(_)
        | HirExprKind::RegexLiteralRef { .. }
        | HirExprKind::BindingRef { .. }
        | HirExprKind::AwaitTask { .. }
        | HirExprKind::ContextReader { .. }
        | HirExprKind::MachineFieldAccess { .. }
        | HirExprKind::MachineEventFieldAccess { .. }
        | HirExprKind::Continue { .. }
        | HirExprKind::ActorSelf
        | HirExprKind::Unsupported(_) => {}
        HirExprKind::Binary { left, right, .. } | HirExprKind::IdentityCompare { left, right } => {
            collect_binding_defs_in_expr(left, defs, let_ids);
            collect_binding_defs_in_expr(right, defs, let_ids);
        }
        HirExprKind::Unary { operand, .. } | HirExprKind::WireCodec { operand, .. } => {
            collect_binding_defs_in_expr(operand, defs, let_ids);
        }
        HirExprKind::ConnAwaitRead { conn, .. } => {
            collect_binding_defs_in_expr(conn, defs, let_ids);
        }
        HirExprKind::AwaitRestart { child } => {
            collect_binding_defs_in_expr(child, defs, let_ids);
        }
        HirExprKind::ListenerAwaitAccept { listener, .. } => {
            collect_binding_defs_in_expr(listener, defs, let_ids);
        }
        HirExprKind::StreamRecvAwait { stream, .. } => {
            collect_binding_defs_in_expr(stream, defs, let_ids);
        }
        HirExprKind::NumericCast { value, .. }
        | HirExprKind::SaturatingWidthCast { value, .. }
        | HirExprKind::TryWidthCast { value, .. }
        | HirExprKind::CoerceToDynTrait { value, .. } => {
            collect_binding_defs_in_expr(value, defs, let_ids);
        }
        HirExprKind::TupleLiteral { elements } => {
            for elem in elements {
                collect_binding_defs_in_expr(elem, defs, let_ids);
            }
        }
        HirExprKind::NumericMethod { receiver, arg, .. } => {
            collect_binding_defs_in_expr(receiver, defs, let_ids);
            collect_binding_defs_in_expr(arg, defs, let_ids);
        }
        HirExprKind::Call { callee, args } | HirExprKind::SpawnedCall { callee, args, .. } => {
            collect_binding_defs_in_expr(callee, defs, let_ids);
            for arg in args {
                collect_binding_defs_in_expr(arg, defs, let_ids);
            }
        }
        HirExprKind::Spawn { args, .. } => {
            for (_, arg) in args {
                collect_binding_defs_in_expr(arg, defs, let_ids);
            }
        }
        HirExprKind::ActorSend { receiver, args, .. }
        | HirExprKind::ActorAsk { receiver, args, .. }
        | HirExprKind::ActorGenStream { receiver, args, .. }
        | HirExprKind::CallDynMethod { receiver, args, .. }
        | HirExprKind::ResolvedImplCall { receiver, args, .. }
        | HirExprKind::CallTraitMethodStatic { receiver, args, .. }
        | HirExprKind::VarSelfMethodCall { receiver, args, .. } => {
            collect_binding_defs_in_expr(receiver, defs, let_ids);
            for arg in args {
                collect_binding_defs_in_expr(arg, defs, let_ids);
            }
        }
        HirExprKind::RemoteActorAsk {
            receiver,
            msg,
            timeout_ms,
            ..
        } => {
            collect_binding_defs_in_expr(receiver, defs, let_ids);
            collect_binding_defs_in_expr(msg, defs, let_ids);
            collect_binding_defs_in_expr(timeout_ms, defs, let_ids);
        }
        HirExprKind::Block(block)
        | HirExprKind::Scope { body: block }
        | HirExprKind::ForkBlock { body: block, .. }
        | HirExprKind::GenBlock { body: block, .. } => {
            collect_binding_defs_in_block(block, defs, let_ids);
        }
        HirExprKind::Yield { value, .. }
        | HirExprKind::Break { value, .. }
        | HirExprKind::Return { value } => {
            if let Some(value) = value {
                collect_binding_defs_in_expr(value, defs, let_ids);
            }
        }
        HirExprKind::If {
            condition,
            then_expr,
            else_expr,
        } => {
            collect_binding_defs_in_expr(condition, defs, let_ids);
            collect_binding_defs_in_expr(then_expr, defs, let_ids);
            if let Some(else_expr) = else_expr {
                collect_binding_defs_in_expr(else_expr, defs, let_ids);
            }
        }
        HirExprKind::StructInit { fields, base, .. } => {
            for (_, field_expr) in fields {
                collect_binding_defs_in_expr(field_expr, defs, let_ids);
            }
            if let Some(base) = base {
                collect_binding_defs_in_expr(base, defs, let_ids);
            }
        }
        HirExprKind::FieldAccess { object, .. } => {
            collect_binding_defs_in_expr(object, defs, let_ids);
        }
        HirExprKind::ScopeDeadline { duration, body } => {
            collect_binding_defs_in_expr(duration, defs, let_ids);
            collect_binding_defs_in_block(body, defs, let_ids);
        }
        HirExprKind::Select(select) => {
            for arm in &select.arms {
                match &arm.kind {
                    hew_hir::HirSelectArmKind::StreamNext { stream } => {
                        collect_binding_defs_in_expr(stream, defs, let_ids);
                    }
                    hew_hir::HirSelectArmKind::ActorAsk { actor, args, .. } => {
                        collect_binding_defs_in_expr(actor, defs, let_ids);
                        for arg in args {
                            collect_binding_defs_in_expr(arg, defs, let_ids);
                        }
                    }
                    hew_hir::HirSelectArmKind::TaskAwait { task } => {
                        collect_binding_defs_in_expr(task, defs, let_ids);
                    }
                    hew_hir::HirSelectArmKind::ChannelRecv { receiver, .. } => {
                        collect_binding_defs_in_expr(receiver, defs, let_ids);
                    }
                    hew_hir::HirSelectArmKind::AfterTimer { duration } => {
                        collect_binding_defs_in_expr(duration, defs, let_ids);
                    }
                }
                collect_binding_defs_in_expr(&arm.body, defs, let_ids);
            }
        }
        HirExprKind::Join(join) => {
            for branch in &join.branches {
                collect_binding_defs_in_expr(&branch.actor, defs, let_ids);
                for arg in &branch.args {
                    collect_binding_defs_in_expr(arg, defs, let_ids);
                }
            }
        }
        HirExprKind::SpawnLambdaActor { body, .. } | HirExprKind::Closure { body, .. } => {
            collect_binding_defs_in_expr(body, defs, let_ids);
        }
        HirExprKind::TupleIndex { tuple, .. } => {
            collect_binding_defs_in_expr(tuple, defs, let_ids);
        }
        HirExprKind::Index { container, index } => {
            collect_binding_defs_in_expr(container, defs, let_ids);
            collect_binding_defs_in_expr(index, defs, let_ids);
        }
        HirExprKind::Slice {
            container,
            start,
            end,
            ..
        } => {
            collect_binding_defs_in_expr(container, defs, let_ids);
            if let Some(start) = start {
                collect_binding_defs_in_expr(start, defs, let_ids);
            }
            if let Some(end) = end {
                collect_binding_defs_in_expr(end, defs, let_ids);
            }
        }
        HirExprKind::MachineEmit { fields, .. } => {
            for (_, field_val) in fields {
                collect_binding_defs_in_expr(field_val, defs, let_ids);
            }
        }
        HirExprKind::MachineStep {
            receiver, event, ..
        }
        | HirExprKind::MachineTakeEmits {
            receiver, event, ..
        } => {
            collect_binding_defs_in_expr(receiver, defs, let_ids);
            collect_binding_defs_in_expr(event, defs, let_ids);
        }
        HirExprKind::ChannelRecvAwait { receiver, .. }
        | HirExprKind::CancellationTokenIsCancelled { receiver }
        | HirExprKind::GeneratorNext { receiver, .. }
        | HirExprKind::MachineStateName { receiver, .. }
        | HirExprKind::RecordCloneCall { src: receiver, .. } => {
            collect_binding_defs_in_expr(receiver, defs, let_ids);
        }
        HirExprKind::MachineVariantCtor { payload, .. } => {
            if let Some(fields) = payload {
                for (_, val) in fields {
                    collect_binding_defs_in_expr(val, defs, let_ids);
                }
            }
        }
        HirExprKind::While {
            condition, body, ..
        } => {
            collect_binding_defs_in_expr(condition, defs, let_ids);
            collect_binding_defs_in_block(body, defs, let_ids);
        }
        HirExprKind::ForRange {
            start,
            end,
            step,
            body,
            ..
        } => {
            collect_binding_defs_in_expr(start, defs, let_ids);
            collect_binding_defs_in_expr(end, defs, let_ids);
            collect_binding_defs_in_expr(step, defs, let_ids);
            collect_binding_defs_in_block(body, defs, let_ids);
        }
        HirExprKind::Match { scrutinee, arms } => {
            collect_binding_defs_in_expr(scrutinee, defs, let_ids);
            for arm in arms {
                collect_binding_defs_in_expr(&arm.body, defs, let_ids);
            }
        }
        HirExprKind::WhileLet {
            scrutinee, body, ..
        } => {
            collect_binding_defs_in_expr(scrutinee, defs, let_ids);
            collect_binding_defs_in_block(body, defs, let_ids);
        }
        HirExprKind::IfLet {
            scrutinee,
            body,
            else_body,
            ..
        } => {
            collect_binding_defs_in_expr(scrutinee, defs, let_ids);
            collect_binding_defs_in_block(body, defs, let_ids);
            if let Some(eb) = else_body {
                collect_binding_defs_in_block(eb, defs, let_ids);
            }
        }
        HirExprKind::Loop { body, .. } => {
            collect_binding_defs_in_block(body, defs, let_ids);
        }
    }
}
/// True when `ty` is a user-declared affine `#[resource]` type — a
/// `Named { builtin: None }` whose value class is `AffineResource`. Mirrors the
/// HIR `checked_span_is_user_resource` predicate (hew-hir `lower.rs`): builtin
/// runtime handles (`builtin: Some(_)` — channels, cancellation tokens, …) and
/// the non-`Named` affine variants are EXCLUDED, because those reach borrowing
/// FFI intrinsics by value and must keep their existing `Read` treatment.
fn is_user_resource_ty(ty: &ResolvedTy, type_classes: &hew_hir::TypeClassTable) -> bool {
    matches!(ty, ResolvedTy::Named { builtin: None, .. })
        && ValueClass::of_ty(ty, type_classes) == ValueClass::AffineResource
}
/// Resolve a `Call` callee to the module function item it names, or `None` for
/// any non-statically-resolved callee (a closure value, a fn-pointer parameter,
/// a method receiver, indirect/dynamic dispatch). Mirrors `callee_is_resolved_item`.
fn callee_item_id(callee: &HirExpr) -> Option<hew_hir::ItemId> {
    if let HirExprKind::BindingRef {
        resolved: ResolvedRef::Item(id),
        ..
    } = &callee.kind
    {
        Some(*id)
    } else {
        None
    }
}
/// Classify every affine `#[resource]` free-function value parameter (#1295
/// RAII-2) as CONSUME or BORROW, and precompute the call-argument `SiteId`s
/// whose over-stamped `Consume` intent must be downgraded to a borrowing `Read`.
///
/// A resource parameter BORROWS BY DEFAULT (the caller keeps ownership and
/// auto-drops at its own scope exit, mirroring a method receiver). It is promoted
/// to CONSUME iff it is pinned with the `consume` keyword OR its body uses it in
/// a consume position. The ONLY positions the borrow recogniser admits are a
/// `Read`-intent method receiver (a non-consuming method like `is_match`) and a
/// direct argument to a resolved free function whose target parameter is itself
/// BORROW; EVERY other use — returned, stored (`let`/assign/struct/tuple),
/// passed to a consuming method, passed to a CONSUME param, passed to an
/// unresolved callee, captured by a closure, or any unmodelled position — is a
/// consume (fail closed).
///
/// The borrow-arg-to-borrow-param rule makes the classification interprocedural,
/// so it is solved as a monotone least-fixpoint seeded from each param's
/// annotation: a param only ever flips Borrow→Consume. The asymmetry is the
/// soundness contract — over-classifying as CONSUME is leak-safe (worst case the
/// callee-drop in `lower_params` frees it once), whereas under-classifying as
/// BORROW would double-free (caller AND callee both release it). The same table
/// drives BOTH the call-site intent downgrade and the callee-side drop, so a
/// parameter is consistently either moved-in-and-callee-drops or
/// kept-and-caller-drops — never split.
/// Collect the `ItemId` of every function that is an inherent/trait `impl`
/// method.
///
/// A method's receiver slot must never be relaxed by the borrow downgrade: an
/// inherent method call lowers to
/// `HirExprKind::Call { callee: Item(m), args: [recv, ...args] }` with the
/// receiver carrying its ACCURATE move intent (a borrowing receiver `Read`, a
/// `self`-consuming receiver `Consume`), whereas an ordinary free-call argument
/// is over-stamped `Consume` by `arg_move_intent`. Two authorities, unioned for
/// robustness:
///   * `method_symbols` from every `impl` block — the authoritative
///     `<Type>::<method>` Function names. This catches a method that names its
///     receiver something OTHER than `self` (the stdlib writes
///     `fn close(child: Child)`), whose destructor-consume of the receiver is
///     IMPLICIT: its body only reads a field, so the body scan would classify
///     the receiver BORROW and the explicit `recv.close()` would be downgraded
///     to a `Read`, leaving the caller to auto-drop a value `close` already
///     released — a double-free.
///   * the first-param-named-`self` heuristic — a cheap catch for any method
///     the symbol scan might miss.
///
/// A free function can never carry a `self` receiver and is never an `impl`
/// `method_symbol`, so its over-stamped `Consume` args stay eligible for the
/// downgrade. Associated/static `impl` functions are also captured here.
fn collect_method_item_ids(
    fns: &HashMap<hew_hir::ItemId, &HirFn>,
    items: &[HirItem],
) -> HashSet<hew_hir::ItemId> {
    let method_symbols: HashSet<&str> = items
        .iter()
        .filter_map(|item| match item {
            HirItem::Impl(b) => Some(b.method_symbols.iter().map(String::as_str)),
            _ => None,
        })
        .flatten()
        .collect();
    fns.iter()
        .filter(|(_, f)| {
            f.params.first().is_some_and(|p| p.name == "self")
                || method_symbols.contains(f.name.as_str())
        })
        .map(|(&id, _)| id)
        .collect()
}
/// Force-classify every NON-RECEIVER `#[resource]`/`#[linear]` value parameter
/// of an `impl`/trait method as CONSUME (callee owns and drops it).
///
/// The borrow-site collector never downgrades a method-call argument (a
/// `recv.m(args)` receiver carries authoritative move intent that must not be
/// relaxed, so the whole arg list is skipped), so a method's non-receiver
/// resource argument always reaches the call site with its HIR-over-stamped
/// `Consume` intent: the caller moves it in and does not auto-drop it. If the
/// fixpoint left such a parameter BORROW (its body only reads a field),
/// `lower_params` would not drop it either — NOBODY drops it → leak / split
/// ownership. Forcing CONSUME adds it to `owned_locals`, so the callee drops it
/// exactly once, matching the caller's move-in.
///
/// The RECEIVER slot is EXCLUDED: its drop disposition rides the accurate
/// call-site receiver intent (a borrowing `peek` Read kept by the caller, a
/// consuming `close` Consume); forcing it CONSUME would make a borrowing
/// receiver drop at BOTH the caller (auto-drop of the still-live binding) and
/// the callee → double-free. Receiver identity follows the checker's rule
/// (`is_receiver_param`): the first parameter is the receiver iff its type is
/// the impl Self type (or the bare `self` sugar). A true associated function
/// whose first parameter is NOT the Self type has no receiver, so all of its
/// resource parameters are forced. Borrow-pass INTO a method's non-receiver
/// parameter is intentionally not offered — the ratified borrow-default surface
/// is free-function value parameters and method receivers.
fn force_consume_method_nonreceiver_resource_params(
    fns: &HashMap<hew_hir::ItemId, &HirFn>,
    items: &[HirItem],
    type_classes: &hew_hir::TypeClassTable,
    methods: &HashSet<hew_hir::ItemId>,
    param_consume: &mut HashMap<(hew_hir::ItemId, usize), bool>,
) {
    let method_self_type: HashMap<&str, &str> = items
        .iter()
        .filter_map(|item| match item {
            HirItem::Impl(b) => Some(
                b.method_symbols
                    .iter()
                    .map(|sym| (sym.as_str(), b.self_type_name.as_str())),
            ),
            _ => None,
        })
        .flatten()
        .collect();
    for (&id, &f) in fns {
        if !methods.contains(&id) {
            continue;
        }
        let receiver_arity = usize::from(f.params.first().is_some_and(|p| {
            p.name == "self"
                || method_self_type.get(f.name.as_str()).is_some_and(
                    |self_ty| matches!(&p.ty, ResolvedTy::Named { name, .. } if name == self_ty),
                )
        }));
        for (i, param) in f.params.iter().enumerate().skip(receiver_arity) {
            if is_user_resource_ty(&param.ty, type_classes) {
                param_consume.insert((id, i), true);
            }
        }
    }
}
/// Prove borrow-only direct-call parameters across every free function. This
/// broader summary is consumed only by collection escape analysis; it does not
/// change call-site move intent or register non-resource parameters for
/// callee-side drops.
fn compute_call_param_consumption(
    fns: &HashMap<hew_hir::ItemId, &HirFn>,
    methods: &HashSet<hew_hir::ItemId>,
    resource_param_consume: &HashMap<(hew_hir::ItemId, usize), bool>,
) -> HashMap<(hew_hir::ItemId, usize), bool> {
    // Seed every parameter as BORROW, except explicit `consume` parameters and
    // resource parameters the RAII table already classified CONSUME. The
    // fail-closed body scan flips a parameter when it is returned, stored, sent,
    // captured, or forwarded to an unproven/consuming parameter.
    let mut consume: HashMap<(hew_hir::ItemId, usize), bool> = HashMap::new();
    for (&id, &f) in fns {
        for (i, param) in f.params.iter().enumerate() {
            let resource_consume = resource_param_consume
                .get(&(id, i))
                .copied()
                .unwrap_or(false);
            consume.insert((id, i), param.is_consume || resource_consume);
        }
    }
    loop {
        let mut changed = false;
        for (&id, &f) in fns {
            for (i, param) in f.params.iter().enumerate() {
                let key = (id, i);
                if consume.get(&key) != Some(&false) {
                    continue;
                }
                let consumed = {
                    let cx = ScanCtx {
                        consume: &consume,
                        methods,
                    };
                    param_consumed_in_body(&f.body, param.id, &cx)
                };
                if consumed {
                    consume.insert(key, true);
                    changed = true;
                }
            }
        }
        if !changed {
            break;
        }
    }
    consume
}
/// Collect direct free-call argument sites whose target parameter is BORROW,
/// across every user body in the module.
fn collect_module_borrow_arg_sites(
    items: &[HirItem],
    cx: &ScanCtx<'_>,
) -> HashSet<hew_hir::SiteId> {
    let mut sites = HashSet::new();
    for item in items {
        match item {
            HirItem::Function(f) => {
                collect_borrow_arg_sites_in_block(&f.body, cx, &mut sites);
            }
            HirItem::Actor(actor) => {
                if let Some(init) = &actor.init {
                    collect_borrow_arg_sites_in_block(&init.body, cx, &mut sites);
                }
                for handler in &actor.receive_handlers {
                    collect_borrow_arg_sites_in_block(&handler.body, cx, &mut sites);
                }
                for method in &actor.methods {
                    collect_borrow_arg_sites_in_block(&method.body, cx, &mut sites);
                }
                for hook in &actor.lifecycle_hooks {
                    collect_borrow_arg_sites_in_block(&hook.body, cx, &mut sites);
                }
            }
            HirItem::Machine(machine) => {
                for state in &machine.states {
                    if let Some(entry) = &state.entry {
                        collect_borrow_arg_sites_in_block(entry, cx, &mut sites);
                    }
                    if let Some(exit) = &state.exit {
                        collect_borrow_arg_sites_in_block(exit, cx, &mut sites);
                    }
                }
                for transition in &machine.transitions {
                    if let Some(guard) = &transition.guard {
                        collect_borrow_arg_sites_in_expr(guard, cx, &mut sites);
                    }
                    collect_borrow_arg_sites_in_expr(&transition.body, cx, &mut sites);
                }
            }
            // No call-bearing user bodies: extern fns have none; impl methods
            // are mirrored as `Function` items; other item kinds carry no calls.
            _ => {}
        }
    }
    sites
}
pub(super) fn compute_param_ownership(
    fns: &HashMap<hew_hir::ItemId, &HirFn>,
    items: &[HirItem],
    type_classes: &hew_hir::TypeClassTable,
) -> ParamOwnershipFacts {
    // Seed: every resource parameter starts at its `consume` annotation —
    // pinned CONSUME (`true`) when annotated, BORROW (`false`) otherwise.
    // Non-resource parameters never enter the map (and so never participate as
    // a borrow target below). Keyed by ORIGIN ItemId so every monomorphisation
    // of a generic origin shares one verdict.
    let mut param_consume: HashMap<(hew_hir::ItemId, usize), bool> = HashMap::new();
    for (&id, &f) in fns {
        for (i, param) in f.params.iter().enumerate() {
            if is_user_resource_ty(&param.ty, type_classes) {
                param_consume.insert((id, i), param.is_consume);
            }
        }
    }
    // Method items — every inherent/trait `impl` method (see
    // `collect_method_item_ids`). A method-call receiver slot carries accurate
    // move intent and is never relaxed by the borrow downgrade; associated/static
    // `impl` functions are captured here too.
    let methods = collect_method_item_ids(fns, items);
    // Force-consume non-receiver resource params of `impl`/trait methods so the
    // callee owns and drops them (the borrow collector skips method-call args).
    force_consume_method_nonreceiver_resource_params(
        fns,
        items,
        type_classes,
        &methods,
        &mut param_consume,
    );
    // Monotone least-fixpoint: a pass only ever flips a BORROW param to CONSUME;
    // a flip can only enable further flips (a now-CONSUME target turns its
    // callers' borrow args into consumes), so iteration converges in at most
    // (longest borrow-forwarding chain) passes.
    loop {
        let mut changed = false;
        for (&id, &f) in fns {
            for (i, param) in f.params.iter().enumerate() {
                let key = (id, i);
                // Only resource params still classified BORROW can flip.
                if param_consume.get(&key) != Some(&false) {
                    continue;
                }
                let consumed = {
                    let cx = ScanCtx {
                        consume: &param_consume,
                        methods: &methods,
                    };
                    param_consumed_in_body(&f.body, param.id, &cx)
                };
                if consumed {
                    param_consume.insert(key, true);
                    changed = true;
                }
            }
        }
        if !changed {
            break;
        }
    }
    let call_param_consume = compute_call_param_consumption(fns, &methods, &param_consume);
    // With the verdict final, collect every free-call argument `SiteId` whose
    // target parameter is a resource BORROW. The arg's `Use` is then emitted
    // `Read` instead of the HIR-over-stamped `Consume`, so the caller keeps the
    // binding live and drops it once at scope exit. Method receiver slots are
    // excluded — their intent is already accurate.
    //
    // The scan must cover EVERY body in the module, not only free functions: a
    // borrowing call written inside an actor `receive fn` / method / `init` /
    // lifecycle hook, or a machine entry/exit/transition body, needs its arg
    // sites downgraded just the same. Classification (above) stays scoped to
    // `fns` (free fns + impl methods — the only items whose value params take
    // part in borrow-pass), but if a call site here is NOT scanned its arg keeps
    // the HIR-over-stamped `Consume`, which is fail-CLOSED (the callee auto-drops
    // it — no leak, no double-free) but spuriously rejects a borrow-then-reuse
    // in that body. Scanning all bodies removes that false rejection. Impl
    // methods are reached via their mirror `HirItem::Function` entries, so the
    // `Impl` block itself is skipped to avoid a redundant re-scan.
    let borrow_arg_sites = collect_module_borrow_arg_sites(
        items,
        &ScanCtx {
            consume: &param_consume,
            methods: &methods,
        },
    );
    let proven_borrow_arg_sites = collect_module_borrow_arg_sites(
        items,
        &ScanCtx {
            consume: &call_param_consume,
            methods: &methods,
        },
    );
    ParamOwnershipFacts {
        param_consume,
        borrow_arg_sites,
        proven_borrow_arg_sites,
    }
}
/// True when `expr` is a bare reference to binding `b_p`.
fn is_binding_ref(expr: &HirExpr, b_p: BindingId) -> bool {
    matches!(
        &expr.kind,
        HirExprKind::BindingRef { resolved: ResolvedRef::Binding(id), .. } if *id == b_p
    )
}
/// Classify the base of a PLACE PROJECTION (`base.field`, `base[i]`, `base.0`,
/// `base[a..b]`). A projection reads THROUGH its base — it borrows `base`, it
/// never moves `base` out — so a bare reference to `b_p` as the projection base
/// is a BORROW (returns `false`, "not consumed"). Only a non-trivial base
/// expression can bury a consume of `b_p` (e.g. `consume_it(b_p).field`), so
/// that case recurses. Without this interception the generic leaf rule would
/// treat every field read (`self.id` in a `#[resource]`'s own `close`/`peek`)
/// as a consume — wrongly registering the borrowed receiver for a callee-side
/// drop (a double-free of a by-reference receiver, or infinite `close`-of-self
/// recursion for the destructor).
fn projection_base_consumes(base: &HirExpr, b_p: BindingId, pc: &ScanCtx<'_>) -> bool {
    if is_binding_ref(base, b_p) {
        false
    } else {
        scan_expr_for_consume(base, b_p, pc)
    }
}
/// Does any use of resource parameter `b_p` in `block` CONSUME it under the
/// RAII-2 borrow recogniser (fail closed)? Entry point for
/// [`compute_param_ownership`]'s body scan.
fn param_consumed_in_body(block: &HirBlock, b_p: BindingId, pc: &ScanCtx<'_>) -> bool {
    scan_block_for_consume(block, b_p, pc)
}
fn scan_block_for_consume(block: &HirBlock, b_p: BindingId, pc: &ScanCtx<'_>) -> bool {
    for stmt in &block.statements {
        match &stmt.kind {
            HirStmtKind::Let(_, init) => {
                if init
                    .as_ref()
                    .is_some_and(|init| scan_expr_for_consume(init, b_p, pc))
                {
                    return true;
                }
            }
            HirStmtKind::Assign { target, value } => {
                if scan_expr_for_consume(target, b_p, pc) || scan_expr_for_consume(value, b_p, pc) {
                    return true;
                }
            }
            HirStmtKind::Expr(expr) | HirStmtKind::Return(Some(expr)) => {
                if scan_expr_for_consume(expr, b_p, pc) {
                    return true;
                }
            }
            HirStmtKind::Return(None) => {}
            HirStmtKind::Defer { body, .. } => {
                if scan_expr_for_consume(body, b_p, pc) {
                    return true;
                }
            }
            HirStmtKind::LetElse {
                scrutinee,
                success_prelude,
                else_body,
                ..
            } => {
                if scan_expr_for_consume(scrutinee, b_p, pc) {
                    return true;
                }
                for prelude_stmt in success_prelude {
                    if let HirStmtKind::Let(_, Some(value)) = &prelude_stmt.kind {
                        if scan_expr_for_consume(value, b_p, pc) {
                            return true;
                        }
                    }
                }
                if scan_block_for_consume(else_body, b_p, pc) {
                    return true;
                }
            }
        }
    }
    block
        .tail
        .as_ref()
        .is_some_and(|tail| scan_expr_for_consume(tail, b_p, pc))
}
/// True when `expr` uses parameter `b_p` in a CONSUME position. A bare reference
/// to `b_p` reached here (by plain recursion from a parent that did NOT classify
/// it as one of the two recognised borrow slots) IS a consume; the two borrow
/// slots — a `Read`-intent method receiver and a direct arg to a BORROW param —
/// are intercepted by their parent arms and never recurse the bare ref. Mirrors
/// `collect_return_values_in_expr`'s exhaustive child coverage so no buried use
/// is missed (a missed consume would under-classify a param as Borrow and
/// double-free); the closure arms DESCEND via the capture ledger because a
/// captured param escapes the call (a consume).
#[allow(
    clippy::too_many_lines,
    clippy::match_same_arms,
    reason = "visitor mirrors the sealed HirExprKind surface so consume detection is exhaustive; arms are kept separate to document each position's borrow-vs-consume classification"
)]
fn scan_expr_for_consume(expr: &HirExpr, b_p: BindingId, pc: &ScanCtx<'_>) -> bool {
    match &expr.kind {
        // Leaf: a bare reference to `b_p` reached by plain recursion is a
        // CONSUME (a recognised borrow slot would have intercepted it). A ref to
        // any OTHER binding contributes nothing.
        HirExprKind::BindingRef { resolved, .. } => {
            matches!(resolved, ResolvedRef::Binding(id) if *id == b_p)
        }
        // Call. Inherent method calls lower to this form too, with the receiver
        // as `args[0]`. A direct argument `b_p` in slot `j` BORROWS iff:
        //  * the callee is a METHOD and slot `j` carries intent `Read` — the
        //    receiver of a non-consuming method (`peek`). For a method call the
        //    receiver's intent is authoritative; we never consult `pc` for it,
        //    because a method's `self` slot may be classified BORROW (an
        //    empty/borrowing body like `close`) yet still consume at the call
        //    site, which the `Consume` receiver intent already records.
        //  * the callee is a FREE function and slot `j`'s target parameter is
        //    classified BORROW in `pc`.
        // Every other arg form recurses (and a bare ref to a Consume/unresolved
        // target bottoms out at the leaf rule → consume). The callee slot is
        // scanned so a parameter used AS the callee (an indirect call) consumes.
        HirExprKind::Call { callee, args } => {
            if scan_expr_for_consume(callee, b_p, pc) {
                return true;
            }
            let callee_item = callee_item_id(callee);
            let is_method = callee_item.is_some_and(|id| pc.methods.contains(&id));
            for (j, arg) in args.iter().enumerate() {
                if is_binding_ref(arg, b_p) {
                    let borrows = if is_method {
                        arg.intent == IntentKind::Read
                    } else {
                        callee_item.and_then(|id| pc.consume.get(&(id, j))).copied() == Some(false)
                    };
                    if !borrows {
                        return true;
                    }
                    // Borrow slot: the arg is a bare ref, nothing more to scan.
                } else if scan_expr_for_consume(arg, b_p, pc) {
                    return true;
                }
            }
            false
        }
        // Method-call forms. The RECEIVER's own intent already encodes
        // borrow-vs-consume (a non-consuming method lowers the receiver `Read`, a
        // `self`-consuming method `Consume`), so a receiver `b_p` BORROWS iff its
        // intent is `Read`. Method ARGUMENTS carry no per-param classification,
        // so a resource argument fails closed to consume via the leaf rule.
        HirExprKind::ResolvedImplCall { receiver, args, .. }
        | HirExprKind::CallTraitMethodStatic { receiver, args, .. }
        | HirExprKind::VarSelfMethodCall { receiver, args, .. }
        | HirExprKind::CallDynMethod { receiver, args, .. } => {
            if is_binding_ref(receiver, b_p) {
                if receiver.intent != IntentKind::Read {
                    return true;
                }
            } else if scan_expr_for_consume(receiver, b_p, pc) {
                return true;
            }
            args.iter().any(|arg| scan_expr_for_consume(arg, b_p, pc))
        }
        // Closure / lambda-actor: a captured `b_p` escapes into the environment
        // (the closure may outlive the call) — an ownership move, so consume. The
        // capture ledger names captured bindings directly; the body is not
        // descended (its `return` exits the closure, and any inner use of `b_p`
        // is necessarily through a capture already listed).
        HirExprKind::Closure { captures, .. } => captures.iter().any(|c| c.binding == b_p),
        HirExprKind::SpawnLambdaActor { captures, .. } => captures.iter().any(|c| c.binding == b_p),
        // ---- generic recursion: a bare `b_p` in ANY child slot below is a
        // ---- consume (return value, store operand, construction field, …). ----
        HirExprKind::Literal(_)
        | HirExprKind::RegexLiteralRef { .. }
        | HirExprKind::AwaitTask { .. }
        | HirExprKind::ContextReader { .. }
        | HirExprKind::MachineFieldAccess { .. }
        | HirExprKind::MachineEventFieldAccess { .. }
        | HirExprKind::Continue { .. }
        | HirExprKind::ActorSelf
        | HirExprKind::Unsupported(_) => false,
        HirExprKind::Binary { left, right, .. } | HirExprKind::IdentityCompare { left, right } => {
            scan_expr_for_consume(left, b_p, pc) || scan_expr_for_consume(right, b_p, pc)
        }
        HirExprKind::Unary { operand, .. } | HirExprKind::WireCodec { operand, .. } => {
            scan_expr_for_consume(operand, b_p, pc)
        }
        HirExprKind::ConnAwaitRead { conn, .. } => scan_expr_for_consume(conn, b_p, pc),
        HirExprKind::AwaitRestart { child } => scan_expr_for_consume(child, b_p, pc),
        HirExprKind::ListenerAwaitAccept { listener, .. } => {
            scan_expr_for_consume(listener, b_p, pc)
        }
        HirExprKind::StreamRecvAwait { stream, .. } => scan_expr_for_consume(stream, b_p, pc),
        HirExprKind::NumericCast { value, .. }
        | HirExprKind::SaturatingWidthCast { value, .. }
        | HirExprKind::TryWidthCast { value, .. }
        | HirExprKind::CoerceToDynTrait { value, .. } => scan_expr_for_consume(value, b_p, pc),
        HirExprKind::TupleLiteral { elements } => {
            elements.iter().any(|e| scan_expr_for_consume(e, b_p, pc))
        }
        HirExprKind::NumericMethod { receiver, arg, .. } => {
            scan_expr_for_consume(receiver, b_p, pc) || scan_expr_for_consume(arg, b_p, pc)
        }
        HirExprKind::SpawnedCall { callee, args, .. } => {
            scan_expr_for_consume(callee, b_p, pc)
                || args.iter().any(|a| scan_expr_for_consume(a, b_p, pc))
        }
        HirExprKind::Spawn { args, .. } => {
            args.iter().any(|(_, a)| scan_expr_for_consume(a, b_p, pc))
        }
        HirExprKind::ActorSend { receiver, args, .. }
        | HirExprKind::ActorAsk { receiver, args, .. }
        | HirExprKind::ActorGenStream { receiver, args, .. } => {
            scan_expr_for_consume(receiver, b_p, pc)
                || args.iter().any(|a| scan_expr_for_consume(a, b_p, pc))
        }
        HirExprKind::RemoteActorAsk {
            receiver,
            msg,
            timeout_ms,
            ..
        } => {
            scan_expr_for_consume(receiver, b_p, pc)
                || scan_expr_for_consume(msg, b_p, pc)
                || scan_expr_for_consume(timeout_ms, b_p, pc)
        }
        HirExprKind::Block(block)
        | HirExprKind::Scope { body: block }
        | HirExprKind::ForkBlock { body: block, .. }
        | HirExprKind::GenBlock { body: block, .. } => scan_block_for_consume(block, b_p, pc),
        HirExprKind::Return { value } => value
            .as_deref()
            .is_some_and(|v| scan_expr_for_consume(v, b_p, pc)),
        HirExprKind::Yield { value, .. } | HirExprKind::Break { value, .. } => value
            .as_deref()
            .is_some_and(|v| scan_expr_for_consume(v, b_p, pc)),
        HirExprKind::If {
            condition,
            then_expr,
            else_expr,
        } => {
            scan_expr_for_consume(condition, b_p, pc)
                || scan_expr_for_consume(then_expr, b_p, pc)
                || else_expr
                    .as_deref()
                    .is_some_and(|e| scan_expr_for_consume(e, b_p, pc))
        }
        HirExprKind::StructInit { fields, base, .. } => {
            fields
                .iter()
                .any(|(_, v)| scan_expr_for_consume(v, b_p, pc))
                || base
                    .as_deref()
                    .is_some_and(|b| scan_expr_for_consume(b, b_p, pc))
        }
        HirExprKind::FieldAccess { object, .. } => projection_base_consumes(object, b_p, pc),
        HirExprKind::ScopeDeadline { duration, body } => {
            scan_expr_for_consume(duration, b_p, pc) || scan_block_for_consume(body, b_p, pc)
        }
        HirExprKind::Select(select) => select.arms.iter().any(|arm| {
            let in_kind = match &arm.kind {
                hew_hir::HirSelectArmKind::StreamNext { stream } => {
                    scan_expr_for_consume(stream, b_p, pc)
                }
                hew_hir::HirSelectArmKind::ActorAsk { actor, args, .. } => {
                    scan_expr_for_consume(actor, b_p, pc)
                        || args.iter().any(|a| scan_expr_for_consume(a, b_p, pc))
                }
                hew_hir::HirSelectArmKind::TaskAwait { task } => {
                    scan_expr_for_consume(task, b_p, pc)
                }
                hew_hir::HirSelectArmKind::ChannelRecv { receiver, .. } => {
                    scan_expr_for_consume(receiver, b_p, pc)
                }
                hew_hir::HirSelectArmKind::AfterTimer { duration } => {
                    scan_expr_for_consume(duration, b_p, pc)
                }
            };
            in_kind || scan_expr_for_consume(&arm.body, b_p, pc)
        }),
        HirExprKind::Join(join) => join.branches.iter().any(|branch| {
            scan_expr_for_consume(&branch.actor, b_p, pc)
                || branch
                    .args
                    .iter()
                    .any(|a| scan_expr_for_consume(a, b_p, pc))
        }),
        HirExprKind::TupleIndex { tuple, .. } => projection_base_consumes(tuple, b_p, pc),
        HirExprKind::Index { container, index } => {
            projection_base_consumes(container, b_p, pc) || scan_expr_for_consume(index, b_p, pc)
        }
        HirExprKind::Slice {
            container,
            start,
            end,
            ..
        } => {
            projection_base_consumes(container, b_p, pc)
                || start
                    .as_deref()
                    .is_some_and(|s| scan_expr_for_consume(s, b_p, pc))
                || end
                    .as_deref()
                    .is_some_and(|e| scan_expr_for_consume(e, b_p, pc))
        }
        HirExprKind::MachineEmit { fields, .. } => fields
            .iter()
            .any(|(_, v)| scan_expr_for_consume(v, b_p, pc)),
        HirExprKind::MachineStep {
            receiver, event, ..
        } => scan_expr_for_consume(receiver, b_p, pc) || scan_expr_for_consume(event, b_p, pc),
        HirExprKind::MachineTakeEmits {
            receiver, event, ..
        } => scan_expr_for_consume(receiver, b_p, pc) || scan_expr_for_consume(event, b_p, pc),
        HirExprKind::ChannelRecvAwait { receiver, .. }
        | HirExprKind::CancellationTokenIsCancelled { receiver }
        | HirExprKind::GeneratorNext { receiver, .. }
        | HirExprKind::MachineStateName { receiver, .. }
        | HirExprKind::RecordCloneCall { src: receiver, .. } => {
            scan_expr_for_consume(receiver, b_p, pc)
        }
        HirExprKind::MachineVariantCtor { payload, .. } => payload.as_ref().is_some_and(|fields| {
            fields
                .iter()
                .any(|(_, v)| scan_expr_for_consume(v, b_p, pc))
        }),
        HirExprKind::While {
            condition, body, ..
        } => scan_expr_for_consume(condition, b_p, pc) || scan_block_for_consume(body, b_p, pc),
        HirExprKind::ForRange {
            start,
            end,
            step,
            body,
            ..
        } => {
            scan_expr_for_consume(start, b_p, pc)
                || scan_expr_for_consume(end, b_p, pc)
                || scan_expr_for_consume(step, b_p, pc)
                || scan_block_for_consume(body, b_p, pc)
        }
        HirExprKind::Match { scrutinee, arms } => {
            scan_expr_for_consume(scrutinee, b_p, pc)
                || arms.iter().any(|arm| {
                    arm.guard
                        .as_ref()
                        .is_some_and(|g| scan_expr_for_consume(g, b_p, pc))
                        || scan_expr_for_consume(&arm.body, b_p, pc)
                })
        }
        HirExprKind::WhileLet {
            scrutinee, body, ..
        } => scan_expr_for_consume(scrutinee, b_p, pc) || scan_block_for_consume(body, b_p, pc),
        HirExprKind::IfLet {
            scrutinee,
            body,
            else_body,
            ..
        } => {
            scan_expr_for_consume(scrutinee, b_p, pc)
                || scan_block_for_consume(body, b_p, pc)
                || else_body
                    .as_ref()
                    .is_some_and(|eb| scan_block_for_consume(eb, b_p, pc))
        }
        HirExprKind::Loop { body, .. } => scan_block_for_consume(body, b_p, pc),
    }
}
/// Walk `block` collecting the `SiteId` of every free-call argument that is
/// passed to a resource BORROW parameter, recursing into all sub-expressions and
/// nested blocks. These sites' over-stamped `Consume` intents are downgraded to
/// `Read` at MIR `Use` emission. Mirrors the exhaustive child coverage of the
/// consume scan; only the `Call` arm records sites, every other arm recurses.
fn collect_borrow_arg_sites_in_block(
    block: &HirBlock,
    pc: &ScanCtx<'_>,
    out: &mut HashSet<hew_hir::SiteId>,
) {
    for stmt in &block.statements {
        match &stmt.kind {
            HirStmtKind::Let(_, init) => {
                if let Some(init) = init {
                    collect_borrow_arg_sites_in_expr(init, pc, out);
                }
            }
            HirStmtKind::Assign { target, value } => {
                collect_borrow_arg_sites_in_expr(target, pc, out);
                collect_borrow_arg_sites_in_expr(value, pc, out);
            }
            HirStmtKind::Expr(expr) | HirStmtKind::Return(Some(expr)) => {
                collect_borrow_arg_sites_in_expr(expr, pc, out);
            }
            HirStmtKind::Return(None) => {}
            HirStmtKind::Defer { body, .. } => {
                collect_borrow_arg_sites_in_expr(body, pc, out);
            }
            HirStmtKind::LetElse {
                scrutinee,
                success_prelude,
                else_body,
                ..
            } => {
                collect_borrow_arg_sites_in_expr(scrutinee, pc, out);
                for prelude_stmt in success_prelude {
                    if let HirStmtKind::Let(_, Some(value)) = &prelude_stmt.kind {
                        collect_borrow_arg_sites_in_expr(value, pc, out);
                    }
                }
                collect_borrow_arg_sites_in_block(else_body, pc, out);
            }
        }
    }
    if let Some(tail) = &block.tail {
        collect_borrow_arg_sites_in_expr(tail, pc, out);
    }
}
#[allow(
    clippy::too_many_lines,
    clippy::match_same_arms,
    reason = "visitor mirrors the sealed HirExprKind surface so site collection is exhaustive; the Call arm records borrow-arg sites, every other arm recurses its children uniformly"
)]
fn collect_borrow_arg_sites_in_expr(
    expr: &HirExpr,
    pc: &ScanCtx<'_>,
    out: &mut HashSet<hew_hir::SiteId>,
) {
    macro_rules! go {
        ($e:expr) => {
            collect_borrow_arg_sites_in_expr($e, pc, out)
        };
    }
    macro_rules! go_block {
        ($b:expr) => {
            collect_borrow_arg_sites_in_block($b, pc, out)
        };
    }
    match &expr.kind {
        // The only recording arm: a direct argument to a resolved FREE function
        // whose target param is a resource BORROW has its `SiteId` downgraded.
        // Method calls are excluded — a method receiver's intent is already
        // accurate (a borrowing receiver `Read`, a consuming receiver
        // `Consume`), so it must never be relaxed by the `pc` verdict (a
        // `close`-style `self` is BORROW in `pc` yet consumes at the call site).
        HirExprKind::Call { callee, args } => {
            if let Some(id) = callee_item_id(callee) {
                if !pc.methods.contains(&id) {
                    for (j, arg) in args.iter().enumerate() {
                        if pc.consume.get(&(id, j)).copied() == Some(false) {
                            out.insert(arg.site);
                        }
                    }
                }
            }
            go!(callee);
            for arg in args {
                go!(arg);
            }
        }
        HirExprKind::Literal(_)
        | HirExprKind::RegexLiteralRef { .. }
        | HirExprKind::BindingRef { .. }
        | HirExprKind::AwaitTask { .. }
        | HirExprKind::ContextReader { .. }
        | HirExprKind::MachineFieldAccess { .. }
        | HirExprKind::MachineEventFieldAccess { .. }
        | HirExprKind::Continue { .. }
        | HirExprKind::ActorSelf
        | HirExprKind::Unsupported(_) => {}
        HirExprKind::Binary { left, right, .. } | HirExprKind::IdentityCompare { left, right } => {
            go!(left);
            go!(right);
        }
        HirExprKind::Unary { operand, .. } | HirExprKind::WireCodec { operand, .. } => go!(operand),
        HirExprKind::ConnAwaitRead { conn, .. } => go!(conn),
        HirExprKind::AwaitRestart { child } => go!(child),
        HirExprKind::ListenerAwaitAccept { listener, .. } => go!(listener),
        HirExprKind::StreamRecvAwait { stream, .. } => go!(stream),
        HirExprKind::NumericCast { value, .. }
        | HirExprKind::SaturatingWidthCast { value, .. }
        | HirExprKind::TryWidthCast { value, .. }
        | HirExprKind::CoerceToDynTrait { value, .. } => go!(value),
        HirExprKind::TupleLiteral { elements } => {
            for elem in elements {
                go!(elem);
            }
        }
        HirExprKind::NumericMethod { receiver, arg, .. } => {
            go!(receiver);
            go!(arg);
        }
        HirExprKind::SpawnedCall { callee, args, .. } => {
            go!(callee);
            for arg in args {
                go!(arg);
            }
        }
        HirExprKind::Spawn { args, .. } => {
            for (_, arg) in args {
                go!(arg);
            }
        }
        HirExprKind::ActorSend { receiver, args, .. }
        | HirExprKind::ActorAsk { receiver, args, .. }
        | HirExprKind::ActorGenStream { receiver, args, .. }
        | HirExprKind::CallDynMethod { receiver, args, .. }
        | HirExprKind::ResolvedImplCall { receiver, args, .. }
        | HirExprKind::CallTraitMethodStatic { receiver, args, .. }
        | HirExprKind::VarSelfMethodCall { receiver, args, .. } => {
            go!(receiver);
            for arg in args {
                go!(arg);
            }
        }
        HirExprKind::RemoteActorAsk {
            receiver,
            msg,
            timeout_ms,
            ..
        } => {
            go!(receiver);
            go!(msg);
            go!(timeout_ms);
        }
        HirExprKind::Block(block)
        | HirExprKind::Scope { body: block }
        | HirExprKind::ForkBlock { body: block, .. }
        | HirExprKind::GenBlock { body: block, .. } => go_block!(block),
        HirExprKind::Return { value }
        | HirExprKind::Yield { value, .. }
        | HirExprKind::Break { value, .. } => {
            if let Some(value) = value {
                go!(value);
            }
        }
        HirExprKind::If {
            condition,
            then_expr,
            else_expr,
        } => {
            go!(condition);
            go!(then_expr);
            if let Some(else_expr) = else_expr {
                go!(else_expr);
            }
        }
        HirExprKind::StructInit { fields, base, .. } => {
            for (_, field_expr) in fields {
                go!(field_expr);
            }
            if let Some(base) = base {
                go!(base);
            }
        }
        HirExprKind::FieldAccess { object, .. } => go!(object),
        HirExprKind::ScopeDeadline { duration, body } => {
            go!(duration);
            go_block!(body);
        }
        HirExprKind::Select(select) => {
            for arm in &select.arms {
                match &arm.kind {
                    hew_hir::HirSelectArmKind::StreamNext { stream } => go!(stream),
                    hew_hir::HirSelectArmKind::ActorAsk { actor, args, .. } => {
                        go!(actor);
                        for arg in args {
                            go!(arg);
                        }
                    }
                    hew_hir::HirSelectArmKind::TaskAwait { task } => go!(task),
                    hew_hir::HirSelectArmKind::ChannelRecv { receiver, .. } => go!(receiver),
                    hew_hir::HirSelectArmKind::AfterTimer { duration } => go!(duration),
                }
                go!(&arm.body);
            }
        }
        HirExprKind::Join(join) => {
            for branch in &join.branches {
                go!(&branch.actor);
                for arg in &branch.args {
                    go!(arg);
                }
            }
        }
        HirExprKind::TupleIndex { tuple, .. } => go!(tuple),
        HirExprKind::Index { container, index } => {
            go!(container);
            go!(index);
        }
        HirExprKind::Slice {
            container,
            start,
            end,
            ..
        } => {
            go!(container);
            if let Some(start) = start {
                go!(start);
            }
            if let Some(end) = end {
                go!(end);
            }
        }
        HirExprKind::MachineEmit { fields, .. } => {
            for (_, field_val) in fields {
                go!(field_val);
            }
        }
        HirExprKind::MachineStep {
            receiver, event, ..
        } => {
            go!(receiver);
            go!(event);
        }
        HirExprKind::MachineTakeEmits {
            receiver, event, ..
        } => {
            go!(receiver);
            go!(event);
        }
        HirExprKind::ChannelRecvAwait { receiver, .. }
        | HirExprKind::CancellationTokenIsCancelled { receiver }
        | HirExprKind::GeneratorNext { receiver, .. }
        | HirExprKind::MachineStateName { receiver, .. }
        | HirExprKind::RecordCloneCall { src: receiver, .. } => go!(receiver),
        HirExprKind::MachineVariantCtor { payload, .. } => {
            if let Some(fields) = payload {
                for (_, val) in fields {
                    go!(val);
                }
            }
        }
        HirExprKind::While {
            condition, body, ..
        } => {
            go!(condition);
            go_block!(body);
        }
        HirExprKind::ForRange {
            start,
            end,
            step,
            body,
            ..
        } => {
            go!(start);
            go!(end);
            go!(step);
            go_block!(body);
        }
        HirExprKind::Match { scrutinee, arms } => {
            go!(scrutinee);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    go!(guard);
                }
                go!(&arm.body);
            }
        }
        HirExprKind::WhileLet {
            scrutinee, body, ..
        } => {
            go!(scrutinee);
            go_block!(body);
        }
        HirExprKind::IfLet {
            scrutinee,
            body,
            else_body,
            ..
        } => {
            go!(scrutinee);
            go_block!(body);
            if let Some(eb) = else_body {
                go_block!(eb);
            }
        }
        HirExprKind::Loop { body, .. } => go_block!(body),
        // Closure / lambda-actor bodies ARE lowered and `Use`-emitted, so a
        // borrowing call inside one still needs its arg sites downgraded.
        HirExprKind::Closure { body, .. } | HirExprKind::SpawnLambdaActor { body, .. } => go!(body),
    }
}
/// Per-function summary for the destructive-funcupdate base gate: does function
/// `f` provably return a FRESH MATERIALISED owner on EVERY return path — a value
/// in its own storage that does NOT originate from a by-value heap parameter?
///
/// A by-value heap parameter is a BORROW, not a move (LESSONS
/// `by-value-heap-params-are-borrows`): the caller retains ownership, so a
/// function that hands one of its params back — directly (`fn id(p) { p }`), as
/// a projection (`fn g(p) { p.inner }`), or laundered through another such call
/// (`fn h(p) { id(p) }`) — returns a value that ALIASES the caller's still-live
/// argument WITHOUT a refcount bump. Using that result as a `{ ..base, f: new }`
/// base then frees the caller's live storage at the override-drop: the
/// call-returns-borrowed-param use-after-free. This summary lets
/// `expr_is_materialized_owner` admit a `..f(args)` base ONLY when `f` cannot
/// leak a borrowed argument through its return — for ANY arguments, since the
/// call site cannot know whether a given argument is itself live elsewhere.
///
/// Least-fixpoint from all-false (a function earns `true` only by positive
/// proof). `fresh[f] == true` iff every return path of `f` is a construction /
/// `.clone()` / `Vec` element / funcupdate result, a projection rooted at one,
/// or a `Call` to an ALREADY-proven-fresh function. A return that is (or
/// projects, or is laundered through a call to) a parameter or a bare local
/// binding, a method call (which can return borrowed `self`/param), or a call to
/// an unproven / mutually-recursive callee fails the function closed. A return
/// whose value is produced by a closure / function-pointer / indirect call also
/// fails closed (the closure can hand back a captured parameter — see the
/// `callee_is_resolved_item` guard in `return_value_may_alias_borrow`). User
/// functions read their analyzed `fns` entry; a resolved Item with no body in
/// this module (an extern / runtime primitive / aggregate constructor) is fresh
/// by the owned-return ABI (`callee_returns_fresh_owner` → `true`).
///
/// Conservative by construction: a helper that returns a let-bound local
/// (`fn make() { let x = Inner { .. }; x }`) is classified non-fresh — sound
/// (fail-closed), at the cost of over-rejecting that idiom as a funcupdate base.
pub(crate) fn compute_fn_returns_fresh_owner(
    fns: &HashMap<hew_hir::ItemId, &HirFn>,
) -> HashMap<hew_hir::ItemId, bool> {
    let mut fresh: HashMap<hew_hir::ItemId, bool> = fns.keys().map(|&id| (id, false)).collect();
    // Monotone least-fixpoint: a pass only ever flips false→true, and a flip
    // requires every return path fresh under the CURRENT table, so iteration
    // converges in at most (longest fresh call chain) passes.
    loop {
        let mut changed = false;
        for (&id, &f) in fns {
            if fresh[&id] {
                continue;
            }
            if fn_body_returns_fresh_owner(f, &fresh) {
                fresh.insert(id, true);
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }
    fresh
}
/// True when EVERY value-bearing return path of `f` — every `return <expr>`
/// anywhere in the body that is not inside a nested closure, plus the
/// fall-through tail when it yields a value — is a fresh materialised owner
/// under the current `fresh` table. A body with no value-bearing return path is
/// not a fresh owner (fail closed).
fn fn_body_returns_fresh_owner(f: &HirFn, fresh: &HashMap<hew_hir::ItemId, bool>) -> bool {
    // Every explicit `return <expr>` value (statement and expression forms, at
    // any depth, NOT descending into closures whose `return` exits the closure
    // rather than `f`).
    let mut return_values: Vec<&HirExpr> = Vec::new();
    collect_return_values_in_block(&f.body, &mut return_values);
    // The fall-through tail is the remaining return path — but only when it
    // actually produces a value. A unit-/never-typed tail (`if c { return x }
    // else { return y }`, or a block ending in a `return`) is a diverging
    // continuation, not a value the function hands back, so it does not gate
    // freshness.
    if let Some(tail) = &f.body.tail {
        if !matches!(tail.ty, ResolvedTy::Unit | ResolvedTy::Never) {
            return_values.push(tail);
        }
    }
    if return_values.is_empty() {
        return false;
    }
    // Fresh iff NO return path aliases a by-value parameter (directly, via a
    // projection, embedded in a construction, or laundered through a call that
    // forwards a parameter). The aliasing question is param-INDEPENDENT — it
    // composes through nested calls without arg-flow tracking — so a single
    // module-global fixpoint suffices.
    return_values
        .iter()
        .all(|e| !return_value_may_alias_borrow(e, fresh))
}
/// True when `expr`, used as a function's return value, MAY alias a by-value
/// heap parameter of that function — i.e. the returned value is (or transitively
/// embeds / projects / launders) a borrow the caller still owns. The negation is
/// "provably a fresh owner". This is the leaf of the module freshness fixpoint.
///
/// WHY a dedicated may-alias predicate and not "is the operand itself fresh":
/// a constructor operand like `string.repeat("a", 32)` is a `Call` to a callee
/// the summary cannot prove fresh (its body bottoms out in a runtime method),
/// yet its result CANNOT alias the enclosing function's parameters because every
/// argument is a literal. Asking "is the operand fresh" would reject it (and
/// collapse every `Record { f: string.repeat(..) }` helper); asking "does the
/// operand alias a parameter" admits it. A `Call(g, args)` aliases a parameter
/// ONLY when `g` is not proven fresh AND some argument itself aliases a
/// parameter — the param-flow that composes interprocedurally.
///
/// EXHAUSTIVE and fail-closed: every form that is not provably non-aliasing
/// (a bare binding, a method call, a deref, any unmodelled form) returns `true`.
///
/// # Delegation (#2648)
///
/// The leaf walk now lives ONCE in [`crate::return_provenance::return_alias_bits`],
/// parameterized by a `LeafPolicy`. This function is the byte-identical **Coarse**
/// wrapper: `return_alias_bits(expr, &CoarsePolicy) != ∅` reproduces the exact
/// pre-refactor boolean, keeping the shared funcupdate (#2420 base gate) /
/// reassign consumers unchanged while #2648's Precise driver consumes the same
/// walk under a different policy. Pinned byte-identical by the
/// `coarse_verdict_differential` frozen-reference test.
fn return_value_may_alias_borrow(expr: &HirExpr, fresh: &HashMap<hew_hir::ItemId, bool>) -> bool {
    crate::return_provenance::coarse_may_alias_borrow(expr, fresh)
}
/// Resolve a `Call` callee to its freshness fact.
///
/// - A statically-resolved item callee (`BindingRef { resolved: Item(id) }`)
///   that names a function body IN THIS MODULE reads that body's summary entry
///   (the analyzed `fresh` verdict — a HEW function that forwards a by-value
///   parameter is `false`, caught by the interprocedural fixpoint).
/// - A resolved item callee with NO body in this module is an extern / runtime
///   primitive (`hew_string_repeat`, `hew_vec_*`, …) or an aggregate
///   constructor. Both return a freshly-owned value by the cross-ABI
///   owned-return contract: a callee returning a heap type hands back a value
///   the caller owns and must free, so it cannot be a borrowed alias of an
///   argument (an extern that returned an un-retained borrow would double-free
///   in EVERY caller that frees the result, not just funcupdate — that is an
///   ABI violation at the extern boundary, the same trust the `RecordCloneCall`
///   / `Index` / `Slice` arms already extend to the `hew_*_clone` /
///   `hew_*_get_clone` primitives they lower to). Classified fresh.
/// - Any other callee shape (an unresolved name, a value-typed fn pointer, an
///   indirect/closure call) is not statically resolvable and fails closed.
pub(super) fn callee_returns_fresh_owner(
    callee: &HirExpr,
    fresh: &HashMap<hew_hir::ItemId, bool>,
) -> bool {
    if let HirExprKind::BindingRef {
        resolved: ResolvedRef::Item(item_id),
        ..
    } = &callee.kind
    {
        // `Some(f)` — a module function body the summary analyzed; trust its
        // verdict. `None` — a resolved item with no analyzable body here
        // (extern primitive or constructor); fresh by the owned-return ABI.
        fresh.get(item_id).copied().unwrap_or(true)
    } else {
        false
    }
}
/// True when `callee` names a statically-resolved Item — a free function (whose
/// body the freshness fixpoint analyzed), an extern / runtime primitive, or an
/// aggregate constructor. False for a closure value, a function-pointer
/// parameter, a method receiver, or any indirect/dynamic dispatch, whose return
/// the summary cannot prove fresh because the called body (and any environment
/// it captures) is not statically in hand. This is the gate that stops a
/// zero-argument closure call (`g()`) from being mistaken for a fresh owner when
/// `g` captures a by-value heap parameter.
pub(super) fn callee_is_resolved_item(callee: &HirExpr) -> bool {
    matches!(
        &callee.kind,
        HirExprKind::BindingRef {
            resolved: ResolvedRef::Item(_),
            ..
        }
    )
}
/// Collect every explicit `return <expr>` value in `block` (statement form),
/// recursing into nested control flow but NOT into closures. Exhaustive over
/// `HirStmtKind`: a missed return statement would let a borrowed-param return
/// escape the freshness summary (a use-after-free), so every form is handled.
pub(crate) fn collect_return_values_in_block<'f>(block: &'f HirBlock, out: &mut Vec<&'f HirExpr>) {
    for stmt in &block.statements {
        match &stmt.kind {
            HirStmtKind::Let(_, init) => {
                if let Some(init) = init {
                    collect_return_values_in_expr(init, out);
                }
            }
            HirStmtKind::Assign { target, value } => {
                collect_return_values_in_expr(target, out);
                collect_return_values_in_expr(value, out);
            }
            HirStmtKind::Expr(expr) => collect_return_values_in_expr(expr, out),
            HirStmtKind::Return(Some(expr)) => {
                out.push(expr);
                collect_return_values_in_expr(expr, out);
            }
            HirStmtKind::Return(None) => {}
            HirStmtKind::Defer { body, .. } => collect_return_values_in_expr(body, out),
            HirStmtKind::LetElse {
                scrutinee,
                success_prelude,
                else_body,
                ..
            } => {
                collect_return_values_in_expr(scrutinee, out);
                for prelude_stmt in success_prelude {
                    if let HirStmtKind::Let(_, Some(value)) = &prelude_stmt.kind {
                        collect_return_values_in_expr(value, out);
                    }
                }
                collect_return_values_in_block(else_body, out);
            }
        }
    }
    if let Some(tail) = &block.tail {
        collect_return_values_in_expr(tail, out);
    }
}
/// Collect every explicit `return <expr>` value reachable from `expr` (the
/// `HirExprKind::Return` expression form plus any buried in sub-expressions and
/// nested blocks), recursing into all sub-expressions EXCEPT closure /
/// lambda-actor bodies (whose `return` exits the closure, not the enclosing
/// function). Exhaustive over the sealed `HirExprKind` surface (mirrors
/// `collect_binding_defs_in_expr`) so no buried return is missed.
#[allow(
    clippy::too_many_lines,
    clippy::match_same_arms,
    reason = "visitor mirrors the sealed HirExprKind surface so return-value collection is exhaustive; the closure/lambda-actor arm is kept separate from the leaf no-op arm to document the do-NOT-descend invariant (a `return` inside a closure exits the closure, not the enclosing function)"
)]
fn collect_return_values_in_expr<'f>(expr: &'f HirExpr, out: &mut Vec<&'f HirExpr>) {
    match &expr.kind {
        HirExprKind::Literal(_)
        | HirExprKind::RegexLiteralRef { .. }
        | HirExprKind::BindingRef { .. }
        | HirExprKind::AwaitTask { .. }
        | HirExprKind::ContextReader { .. }
        | HirExprKind::MachineFieldAccess { .. }
        | HirExprKind::MachineEventFieldAccess { .. }
        | HirExprKind::Continue { .. }
        | HirExprKind::ActorSelf
        | HirExprKind::Unsupported(_) => {}
        HirExprKind::Binary { left, right, .. } | HirExprKind::IdentityCompare { left, right } => {
            collect_return_values_in_expr(left, out);
            collect_return_values_in_expr(right, out);
        }
        HirExprKind::Unary { operand, .. } | HirExprKind::WireCodec { operand, .. } => {
            collect_return_values_in_expr(operand, out);
        }
        HirExprKind::ConnAwaitRead { conn, .. } => collect_return_values_in_expr(conn, out),
        HirExprKind::AwaitRestart { child } => collect_return_values_in_expr(child, out),
        HirExprKind::ListenerAwaitAccept { listener, .. } => {
            collect_return_values_in_expr(listener, out);
        }
        HirExprKind::StreamRecvAwait { stream, .. } => {
            collect_return_values_in_expr(stream, out);
        }
        HirExprKind::NumericCast { value, .. }
        | HirExprKind::SaturatingWidthCast { value, .. }
        | HirExprKind::TryWidthCast { value, .. }
        | HirExprKind::CoerceToDynTrait { value, .. } => {
            collect_return_values_in_expr(value, out);
        }
        HirExprKind::TupleLiteral { elements } => {
            for elem in elements {
                collect_return_values_in_expr(elem, out);
            }
        }
        HirExprKind::NumericMethod { receiver, arg, .. } => {
            collect_return_values_in_expr(receiver, out);
            collect_return_values_in_expr(arg, out);
        }
        HirExprKind::Call { callee, args } | HirExprKind::SpawnedCall { callee, args, .. } => {
            collect_return_values_in_expr(callee, out);
            for arg in args {
                collect_return_values_in_expr(arg, out);
            }
        }
        HirExprKind::Spawn { args, .. } => {
            for (_, arg) in args {
                collect_return_values_in_expr(arg, out);
            }
        }
        HirExprKind::ActorSend { receiver, args, .. }
        | HirExprKind::ActorAsk { receiver, args, .. }
        | HirExprKind::ActorGenStream { receiver, args, .. }
        | HirExprKind::CallDynMethod { receiver, args, .. }
        | HirExprKind::ResolvedImplCall { receiver, args, .. }
        | HirExprKind::CallTraitMethodStatic { receiver, args, .. }
        | HirExprKind::VarSelfMethodCall { receiver, args, .. } => {
            collect_return_values_in_expr(receiver, out);
            for arg in args {
                collect_return_values_in_expr(arg, out);
            }
        }
        HirExprKind::RemoteActorAsk {
            receiver,
            msg,
            timeout_ms,
            ..
        } => {
            collect_return_values_in_expr(receiver, out);
            collect_return_values_in_expr(msg, out);
            collect_return_values_in_expr(timeout_ms, out);
        }
        HirExprKind::Block(block)
        | HirExprKind::Scope { body: block }
        | HirExprKind::ForkBlock { body: block, .. }
        | HirExprKind::GenBlock { body: block, .. } => {
            collect_return_values_in_block(block, out);
        }
        // `return <e>` — the value escapes the FUNCTION; collect it and recurse
        // for nested returns inside `e`. `yield`/`break` carry values out of a
        // generator/loop, NOT the function, so recurse but do not collect.
        HirExprKind::Return { value } => {
            if let Some(value) = value {
                out.push(value);
                collect_return_values_in_expr(value, out);
            }
        }
        HirExprKind::Yield { value, .. } | HirExprKind::Break { value, .. } => {
            if let Some(value) = value {
                collect_return_values_in_expr(value, out);
            }
        }
        HirExprKind::If {
            condition,
            then_expr,
            else_expr,
        } => {
            collect_return_values_in_expr(condition, out);
            collect_return_values_in_expr(then_expr, out);
            if let Some(else_expr) = else_expr {
                collect_return_values_in_expr(else_expr, out);
            }
        }
        HirExprKind::StructInit { fields, base, .. } => {
            for (_, field_expr) in fields {
                collect_return_values_in_expr(field_expr, out);
            }
            if let Some(base) = base {
                collect_return_values_in_expr(base, out);
            }
        }
        HirExprKind::FieldAccess { object, .. } => collect_return_values_in_expr(object, out),
        HirExprKind::ScopeDeadline { duration, body } => {
            collect_return_values_in_expr(duration, out);
            collect_return_values_in_block(body, out);
        }
        HirExprKind::Select(select) => {
            for arm in &select.arms {
                match &arm.kind {
                    hew_hir::HirSelectArmKind::StreamNext { stream } => {
                        collect_return_values_in_expr(stream, out);
                    }
                    hew_hir::HirSelectArmKind::ActorAsk { actor, args, .. } => {
                        collect_return_values_in_expr(actor, out);
                        for arg in args {
                            collect_return_values_in_expr(arg, out);
                        }
                    }
                    hew_hir::HirSelectArmKind::TaskAwait { task } => {
                        collect_return_values_in_expr(task, out);
                    }
                    hew_hir::HirSelectArmKind::ChannelRecv { receiver, .. } => {
                        collect_return_values_in_expr(receiver, out);
                    }
                    hew_hir::HirSelectArmKind::AfterTimer { duration } => {
                        collect_return_values_in_expr(duration, out);
                    }
                }
                collect_return_values_in_expr(&arm.body, out);
            }
        }
        HirExprKind::Join(join) => {
            for branch in &join.branches {
                collect_return_values_in_expr(&branch.actor, out);
                for arg in &branch.args {
                    collect_return_values_in_expr(arg, out);
                }
            }
        }
        // Closure / lambda-actor bodies: a `return` inside exits the CLOSURE,
        // not the enclosing function, so do NOT descend.
        HirExprKind::SpawnLambdaActor { .. } | HirExprKind::Closure { .. } => {}
        HirExprKind::TupleIndex { tuple, .. } => collect_return_values_in_expr(tuple, out),
        HirExprKind::Index { container, index } => {
            collect_return_values_in_expr(container, out);
            collect_return_values_in_expr(index, out);
        }
        HirExprKind::Slice {
            container,
            start,
            end,
            ..
        } => {
            collect_return_values_in_expr(container, out);
            if let Some(start) = start {
                collect_return_values_in_expr(start, out);
            }
            if let Some(end) = end {
                collect_return_values_in_expr(end, out);
            }
        }
        HirExprKind::MachineEmit { fields, .. } => {
            for (_, field_val) in fields {
                collect_return_values_in_expr(field_val, out);
            }
        }
        HirExprKind::MachineStep {
            receiver, event, ..
        } => {
            collect_return_values_in_expr(receiver, out);
            collect_return_values_in_expr(event, out);
        }
        HirExprKind::MachineTakeEmits {
            receiver, event, ..
        } => {
            collect_return_values_in_expr(receiver, out);
            collect_return_values_in_expr(event, out);
        }
        HirExprKind::ChannelRecvAwait { receiver, .. }
        | HirExprKind::CancellationTokenIsCancelled { receiver }
        | HirExprKind::GeneratorNext { receiver, .. }
        | HirExprKind::MachineStateName { receiver, .. }
        | HirExprKind::RecordCloneCall { src: receiver, .. } => {
            collect_return_values_in_expr(receiver, out);
        }
        HirExprKind::MachineVariantCtor { payload, .. } => {
            if let Some(fields) = payload {
                for (_, val) in fields {
                    collect_return_values_in_expr(val, out);
                }
            }
        }
        HirExprKind::While {
            condition, body, ..
        } => {
            collect_return_values_in_expr(condition, out);
            collect_return_values_in_block(body, out);
        }
        HirExprKind::ForRange {
            start,
            end,
            step,
            body,
            ..
        } => {
            collect_return_values_in_expr(start, out);
            collect_return_values_in_expr(end, out);
            collect_return_values_in_expr(step, out);
            collect_return_values_in_block(body, out);
        }
        HirExprKind::Match { scrutinee, arms } => {
            collect_return_values_in_expr(scrutinee, out);
            for arm in arms {
                // A `return <expr>` buried in an arm GUARD exits the function:
                // its value is a return path the summary must union, or a
                // guard-forwarded borrow reads wrongly-Fresh and the preflight
                // mints a second owner over caller-owned storage.
                if let Some(guard) = &arm.guard {
                    collect_return_values_in_expr(guard, out);
                }
                collect_return_values_in_expr(&arm.body, out);
            }
        }
        HirExprKind::WhileLet {
            scrutinee, body, ..
        } => {
            collect_return_values_in_expr(scrutinee, out);
            collect_return_values_in_block(body, out);
        }
        HirExprKind::IfLet {
            scrutinee,
            body,
            else_body,
            ..
        } => {
            collect_return_values_in_expr(scrutinee, out);
            collect_return_values_in_block(body, out);
            if let Some(eb) = else_body {
                collect_return_values_in_block(eb, out);
            }
        }
        HirExprKind::Loop { body, .. } => collect_return_values_in_block(body, out),
    }
}
pub(super) fn collect_unknown_type_diagnostics(
    func: &HirFn,
    builder: &Builder,
    diagnostics: &mut Vec<MirDiagnostic>,
) {
    let mut reported = HashSet::new();

    for param in &func.params {
        let substituted = builder.subst_ty(&param.ty);
        push_unknown_type_diagnostics(&substituted, builder, &mut reported, diagnostics);
    }
    let substituted_ret = builder.subst_ty(&func.return_ty);
    push_unknown_type_diagnostics(&substituted_ret, builder, &mut reported, diagnostics);

    for decision in &builder.decisions {
        push_unknown_type_diagnostics(&decision.ty, builder, &mut reported, diagnostics);
        if decision.strategy == Strategy::UnknownBlocked
            && named_type_names(&decision.ty).is_empty()
        {
            push_unknown_type_diagnostic(format!("{:?}", decision.ty), &mut reported, diagnostics);
        }
    }

    for statement in &builder.statements {
        match statement {
            MirStatement::Bind { ty, .. }
            | MirStatement::Evaluate { ty, .. }
            | MirStatement::Use { ty, .. }
            | MirStatement::AggregateAlias { ty, .. }
            | MirStatement::Return { ty, .. }
            | MirStatement::Drop { ty, .. } => {
                push_unknown_type_diagnostics(ty, builder, &mut reported, diagnostics);
            }
        }
    }
}
pub(super) fn collect_layout_field_diagnostics(
    record_layouts: &[crate::model::RecordLayout],
    enum_layouts: &[crate::model::EnumLayout],
    machine_layouts: &[crate::model::MachineLayout],
    readiness: &LayoutReadiness,
    diagnostics: &mut Vec<MirDiagnostic>,
) {
    let mut reported = HashSet::new();
    for field_ty in record_layouts
        .iter()
        .flat_map(|layout| layout.field_tys.iter())
        .chain(
            enum_layouts
                .iter()
                .flat_map(|layout| layout.variants.iter())
                .flat_map(|variant| variant.field_tys.iter()),
        )
        .chain(
            machine_layouts
                .iter()
                .flat_map(|layout| layout.variants.iter().chain(layout.events.iter()))
                .flat_map(|variant| variant.field_tys.iter()),
        )
    {
        push_unknown_type_diagnostics_for_layout_ty(
            field_ty,
            readiness,
            &mut reported,
            diagnostics,
        );
    }
}
fn push_unknown_type_diagnostics_for_layout_ty(
    ty: &ResolvedTy,
    readiness: &LayoutReadiness,
    reported: &mut HashSet<String>,
    diagnostics: &mut Vec<MirDiagnostic>,
) {
    for component in codegen_relevant_named_components(ty) {
        if component.builtin.is_some() || is_codegen_ready_user_name(&component.name, readiness) {
            continue;
        }
        push_unknown_type_diagnostic(component.name, reported, diagnostics);
    }
}
/// Strip the module prefix from every `Named` name in a type spine, recursing
/// over every compound `ResolvedTy` shape. At import-use sites the MIR carries
/// module-qualified names in both the origin AND the type args
/// (`Pair<i64, json.Value>` -> `Named { name: "Pair", args: [i64, json.Value] }`),
/// while every layout-registration + codegen-thunk site keys on the bare
/// (short) names (`Pair$$i64$Value`). Normalising the whole spine here keeps
/// MIR's `user_record_layout_key` byte-congruent with the codegen consumers.
///
/// Delegates to the single canonical `hew_hir::shorten_named_arg_qualifiers`
/// so the record layout-registration key, the enum layout-registration key,
/// and every codegen lookup/drop/codec key are produced by ONE shortener and
/// cannot drift — including for a NESTED qualified payload
/// (`Pair<Vec<json.Value>, _>`) that a `Named`-only shortener would miss.
fn shorten_named_ty_spine(ty: &ResolvedTy) -> ResolvedTy {
    hew_hir::shorten_named_arg_qualifiers(ty.clone())
}
/// Mangle a generic instantiation's LAYOUT key after shortening the whole
/// type-arg spine to bare (unqualified) payload names — the MIR sibling of
/// codegen's `mangle_with_shortened_args` and the generic arm of
/// `user_record_layout_key`.
///
/// Every record/enum layout is registered under the bare-normalised spine (the
/// HIR `EnumLayoutRegistry::insert`, the `layout_mono` pass, and the record
/// origin-site path all route their `mangled_name` through
/// `hew_hir::shorten_named_arg_qualifiers`). So every MIR layout LOOKUP that
/// mangles a key from an expression's type — field access / field store,
/// `StructInit`, the owned-element and enum-layout reachability walks — MUST
/// shorten its spine identically, or a key carrying a module-qualified payload
/// (`Holder<lmonobox.Box>` → `Holder$$lmonobox.Box`) diverges from the
/// registered `Holder$$Box` and the lookup falls through the fail-closed gate.
/// Nested qualified payloads (`Result<Vec<json.Value>, _>`) are shortened at
/// every depth. In-repo unqualified generics are unaffected (a no-op strip).
pub(super) fn mangle_layout_key(name: &str, args: &[ResolvedTy]) -> String {
    let short_args: Vec<ResolvedTy> = args.iter().map(shorten_named_ty_spine).collect();
    hew_hir::mangle(name, &short_args)
}
/// Resolve the `record_field_orders` key for a user record type — the key MIR
/// and codegen must agree on so the value-class admit, the drop-plan validator,
/// and the synthesised `__hew_record_{clone,drop}_inplace_<R>` thunk all name
/// the same layout.
///
/// For a generic INSTANTIATION the origin name AND every type arg are
/// prefix-stripped before mangling, exactly as the codegen consumers do
/// (`record_inplace_drop_name`, `collect_record_inplace_drop_seeds`,
/// `is_heap_owning_record_composite_return`, and `resolve_ty` all
/// `mangle(short_name(name), &shorten_named_args(args))`). Without the strip an
/// IMPORTED generic record (`mymod.Pair<i64, string>`) keyed
/// `mymod.Pair$$i64$string` here — or one carrying a module-qualified arg
/// (`Pair<json.Value, i64>` keyed `Pair$$json.Value$i64`) — would diverge from
/// the codegen-side `Pair$$i64$string` / `Pair$$Value$i64`. MIR would then
/// admit/look up under a key downstream never resolves, a fail-closed mismatch.
/// In-repo unqualified generics (`name == short`, no qualified args) are
/// unaffected (the strip is a no-op).
///
/// The bare-name MONOMORPHIC arm keeps the FULL qualified name: imported
/// monomorphic records register under the bare name but `lookup_record_field_
/// order` already strips the prefix on a miss, so the full key resolves there
/// while preserving the legacy behaviour every monomorphic caller depends on.
pub(super) fn user_record_layout_key(ty: &ResolvedTy) -> Option<String> {
    match ty {
        ResolvedTy::Named {
            name,
            args,
            builtin: None,
            ..
        } if args.is_empty() => Some(name.clone()),
        ResolvedTy::Named {
            name,
            args,
            builtin: None,
            ..
        } => {
            let short_args: Vec<ResolvedTy> = args.iter().map(shorten_named_ty_spine).collect();
            Some(hew_hir::mangle(short_name(name), &short_args))
        }
        // M-5: a BUILTIN record with a registered `Struct` shape (today only
        // `CrashInfo`, which carries an owned `message: string`) is keyed by its
        // bare name so it routes through the SAME owned-aggregate record
        // clone/drop synthesis (`__hew_record_{clone,drop}_inplace_<R>`) user
        // records use. Its `record_field_orders` entry is seeded by
        // `register_builtin_record_layouts` from the registration shape, so the
        // field-kind classifier and the codegen thunk agree on the layout.
        ResolvedTy::Named {
            name,
            args,
            builtin: Some(_),
            ..
        } if args.is_empty()
            && matches!(
                hew_hir::builtin_type_classes::builtin_type_registration(name).map(|r| r.shape),
                Some(hew_hir::builtin_type_classes::BuiltinTypeShape::Struct(_))
            ) =>
        {
            Some(name.clone())
        }
        _ => None,
    }
}
pub(super) fn monomorphic_user_record_key(ty: &ResolvedTy) -> Option<String> {
    match ty {
        ResolvedTy::Named {
            name,
            args,
            builtin: None,
            ..
        } if args.is_empty() => Some(name.clone()),
        _ => None,
    }
}
pub(super) fn vec_iter_record_layout_key(ty: &ResolvedTy) -> Option<String> {
    let key = user_record_layout_key(ty)?;
    (key == "VecIter" || key.starts_with("VecIter$$")).then_some(key)
}
/// `true` when a `for x in vec`/`VecIter<T>` cursor whose element type is `elem`
/// can have its `vec` handle freed at loop/scope exit WITHOUT risking a
/// double-free of a shared element (#1949).
///
/// The for-in body extracts each element through `hew_vec_get_*`. For an OWNED
/// element type — `string` (`hew_vec_get_str` RETAINS a refcounted reference),
/// or any heap-owning element (`hew_vec_get_owned` clones via the descriptor) —
/// that extracted reference can be MOVED into another owner inside the body
/// (e.g. `for w in words { map.insert(w, …) }`). Freeing the source vec then
/// runs `free_string_elements` / the per-element descriptor drop over slots that
/// alias the value the downstream owner also frees: a double-free. The
/// element-escape accounting needed to free the source safely in that case is
/// not modelled here, so the cursor/source vec drop is restricted to elements
/// the body can only COPY, never retain-and-consume: the `BitCopy` scalars. For
/// every other element type the source vec is left undropped (leak, as before
/// this fix — fail-closed, never double-free).
///
/// Tuple / array / named-aggregate elements are excluded too: even an
/// all-`BitCopy` tuple element is read out by value with no retain, but the
/// `hew_vec_free` element walk for those layouts is descriptor-driven and shares
/// the same element-escape hazard surface, so they stay conservative here.
fn vec_iter_elem_drop_safe(elem: &ResolvedTy) -> bool {
    matches!(
        elem,
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
            | ResolvedTy::Duration
    )
}
/// `true` when `ty` is a `VecIter<T>` cursor whose element `T` is drop-safe at
/// loop/scope exit (see [`vec_iter_elem_drop_safe`]).
pub(super) fn vec_iter_ty_drop_safe(ty: &ResolvedTy) -> bool {
    let ResolvedTy::Named { name, args, .. } = ty else {
        return false;
    };
    if vec_iter_record_layout_key(ty).is_none() && name != "VecIter" {
        return false;
    }
    args.first().is_some_and(vec_iter_elem_drop_safe)
}
/// The `vec`-field (`.0`) source place of a `record_init VecIter { vec, idx }`,
/// or `None` for any other instruction.
///
/// A `for x in vec` desugar lowers the source collection into the synthetic
/// `VecIter { vec: <source>, idx: 0 }` cursor. The cursor BORROWS the source's
/// handle for the loop's duration — for a place source it is a `CowShare` (the
/// source binding stays Live and the iteration only READS the handle via
/// `len()` / `get(i)`); for an rvalue / `to_vec()` source it is a sole-owner
/// transfer. Either way the `record_init`'s `vec`-field read is NOT an
/// ownership escape of the source handle the way a user `record_init` field
/// store is — the cursor never frees what it borrows. Surfacing this field lets
/// `derive_local_collection_drop_allowed` exempt it from the escape scan so a
/// captured place source keeps its own scope-exit drop, and lets
/// `derive_vec_iter_drop_allowed` decide whether the cursor (rvalue source) or
/// the source binding (place source) is the sole owner that frees the handle.
pub(super) fn vec_iter_record_init_vec_source(instr: &Instr) -> Option<Place> {
    let Instr::RecordInit { ty, fields, .. } = instr else {
        return None;
    };
    vec_iter_record_layout_key(ty)?;
    // The `vec` field is declaration-order field 0; `idx` is field 1 (BitCopy,
    // never an alias member). Descriptor-driven recursive release makes this
    // borrow rule independent of element depth: the source remains the sole
    // owner for flat and nested Vecs alike, while indexed/rvalue cursors are
    // classified separately by `vec_iter_let_cursor_owns_handle`.
    fields
        .iter()
        .find(|(offset, _)| offset.0 == 0)
        .map(|(_, src)| *src)
}
pub(super) fn is_unsupported_user_record_value_class_ty(
    ty: &ResolvedTy,
    builder: &Builder,
) -> bool {
    let Some(key) = user_record_layout_key(ty) else {
        return false;
    };
    builder
        .lookup_record_field_order(&key)
        .is_some_and(|fields| !fields.is_empty())
}
/// Emit `UnknownType` diagnostics for user-named types that are not part of
/// the MIR layout graph codegen can resolve. Builtin-discriminated names bypass
/// only this user-name readiness predicate; unsupported builtin lowering remains
/// guarded by the downstream fail-closed paths.
fn push_unknown_type_diagnostics(
    ty: &ResolvedTy,
    builder: &Builder,
    reported: &mut HashSet<String>,
    diagnostics: &mut Vec<MirDiagnostic>,
) {
    let readiness = builder.layout_readiness();
    for component in codegen_relevant_named_components(ty) {
        if component.builtin.is_some() || is_codegen_ready_user_name(&component.name, &readiness) {
            continue;
        }
        push_unknown_type_diagnostic(component.name, reported, diagnostics);
    }
}
/// `true` for the opaque actor-reference families whose single type argument is
/// a phantom protocol/message marker. The pid itself is an opaque id/pointer;
/// its argument never contributes to the handle's runtime layout and is erased
/// before codegen. That marker is frequently a `trait` used purely for the
/// `LocalPid<Actor>` → `LocalPid<Handler>` coercion surface
/// (e.g. `LocalPid<ConnectionHandler>` / `LocalPid<WebSocketHandler>` in
/// std/net), a trait that has — and needs — no layout. The readiness walk must
/// not descend into these args or it would emit a spurious `UnknownType` for an
/// erased trait marker. A genuinely-unknown actor type still surfaces as a
/// checker-stage `UnresolvedType`; it is never first observed here.
fn is_phantom_arg_pid(builtin: Option<BuiltinType>) -> bool {
    matches!(
        builtin,
        Some(BuiltinType::LocalPid | BuiltinType::RemotePid)
    )
}
/// Named-type components codegen's layout graph must be able to resolve.
/// Mirrors `hew_hir::named_type_components` but prunes the phantom type
/// arguments of the opaque actor-reference families (see [`is_phantom_arg_pid`]).
fn codegen_relevant_named_components(ty: &ResolvedTy) -> Vec<hew_hir::NamedTypeComponent> {
    let mut components = Vec::new();
    collect_codegen_relevant_components(ty, &mut components);
    components
}
fn collect_codegen_relevant_components(
    ty: &ResolvedTy,
    components: &mut Vec<hew_hir::NamedTypeComponent>,
) {
    match ty {
        ResolvedTy::Tuple(elems) => {
            for elem in elems {
                collect_codegen_relevant_components(elem, components);
            }
        }
        ResolvedTy::Array(elem, _) | ResolvedTy::Slice(elem) => {
            collect_codegen_relevant_components(elem, components);
        }
        ResolvedTy::Named {
            name,
            args,
            builtin,
            ..
        } => {
            components.push(hew_hir::NamedTypeComponent {
                name: name.clone(),
                builtin: *builtin,
                has_args: !args.is_empty(),
            });
            // Opaque actor-reference handles carry a phantom protocol marker
            // (often a trait) in their type argument; it is erased before
            // codegen and has no layout. Do not descend.
            if is_phantom_arg_pid(*builtin) {
                return;
            }
            for arg in args {
                collect_codegen_relevant_components(arg, components);
            }
        }
        ResolvedTy::Function { params, ret } => {
            for param in params {
                collect_codegen_relevant_components(param, components);
            }
            collect_codegen_relevant_components(ret, components);
        }
        ResolvedTy::Closure {
            params,
            ret,
            captures,
        } => {
            for param in params {
                collect_codegen_relevant_components(param, components);
            }
            collect_codegen_relevant_components(ret, components);
            for capture in captures {
                collect_codegen_relevant_components(capture, components);
            }
        }
        ResolvedTy::Pointer { pointee, .. } | ResolvedTy::Borrow { pointee } => {
            collect_codegen_relevant_components(pointee, components);
        }
        ResolvedTy::TraitObject { traits } => {
            for bound in traits {
                for arg in &bound.args {
                    collect_codegen_relevant_components(arg, components);
                }
                for (_, ty) in &bound.assoc_bindings {
                    collect_codegen_relevant_components(ty, components);
                }
            }
        }
        ResolvedTy::Task(inner) => collect_codegen_relevant_components(inner, components),
        _ => {}
    }
}
fn is_codegen_ready_user_name(name: &str, readiness: &LayoutReadiness) -> bool {
    // `#[opaque]` runtime handles are registered in `type_classes` but carry no
    // record-field-order entry — they lower to `ptr`. Fieldless handles may be
    // `BitCopy` (borrowed/id handles) or `Resource` (owned handles with
    // `close()` cleanup); both are codegen-ready without a structural record
    // layout. Fielded marker types already have a `record_field_orders` entry and
    // are accepted by the check below.
    if matches!(
        hew_hir::lookup_type_marker(name, readiness.type_classes),
        Some(ResourceMarker::BitCopy | ResourceMarker::Resource)
    ) {
        return true;
    }
    readiness.record_field_orders.contains_key(name)
        || readiness
            .record_field_orders
            .keys()
            .any(|known| short_name(known) == short_name(name))
        // Generic record instantiations are keyed by the mangled SHORT name
        // (e.g. "Wrapper$$i64"); match against the bare (short) prefix so that
        // `UnknownType` is not emitted for types that DO have a concrete
        // layout entry under a monomorphised symbol — including a qualified
        // outer spelling (`shapes.Holder<...>` keyed `Holder$$Box`).
        || {
            let short = short_name(name);
            readiness
                .record_field_orders
                .keys()
                .any(|known| known.starts_with(short) && known[short.len()..].starts_with("$$"))
        }
        || readiness.actor_layouts.contains_key(name)
        || readiness.supervisor_layout_map.contains_key(name)
        || machine_layout_name_matches(readiness.machine_layout_names, name)
}
fn push_unknown_type_diagnostic(
    name: String,
    reported: &mut HashSet<String>,
    diagnostics: &mut Vec<MirDiagnostic>,
) {
    if reported.insert(name.clone()) {
        diagnostics.push(MirDiagnostic {
            kind: MirDiagnosticKind::UnknownType { name },
            note: "named user type has no known ValueClass at the MIR boundary; \
                   only builtin types are supported in slice 1"
                .to_string(),
        });
    }
}
pub(super) fn machine_layout_name_matches(layout_names: &HashSet<String>, name: &str) -> bool {
    layout_names
        .iter()
        .any(|known| known == name || short_name(known) == short_name(name))
}
pub(super) fn short_name(name: &str) -> &str {
    name.rsplit_once('.').map_or(name, |(_, short)| short)
}
#[cfg(test)]
mod runtime_callee_ownership_contract_parity {
    use crate::runtime_symbols::{callee_ownership_contract, known_runtime_symbols};
    use std::collections::BTreeSet;

    const CLASSIFIER_SYMBOLS: &[&str] = &[
        "hew_bool_to_string",
        "hew_bytes_append",
        "hew_bytes_clear",
        "hew_bytes_contains",
        "hew_bytes_get",
        "hew_bytes_index",
        "hew_bytes_is_empty",
        "hew_bytes_len",
        "hew_bytes_pop",
        "hew_bytes_push",
        "hew_bytes_set",
        "hew_bytes_slice",
        "hew_bytes_to_string",
        "hew_char_to_string",
        "hew_float_to_string",
        "hew_hashmap_clone_layout",
        "hew_hashmap_contains_key_layout",
        "hew_hashmap_get_clone_layout",
        "hew_hashmap_get_layout",
        "hew_hashmap_insert_layout",
        "hew_hashmap_keys_layout",
        "hew_hashmap_len_layout",
        "hew_hashmap_remove_layout",
        "hew_hashmap_remove_take_layout",
        "hew_hashmap_values_layout",
        "hew_hashset_clone_layout",
        "hew_hashset_contains_layout",
        "hew_hashset_insert_layout",
        "hew_hashset_is_empty_layout",
        "hew_hashset_len_layout",
        "hew_hashset_remove_layout",
        "hew_hashset_to_vec_layout",
        "hew_i64_to_string",
        "hew_int_to_string",
        "hew_string_char_at",
        "hew_string_char_at_utf8",
        "hew_string_char_count",
        "hew_string_chars",
        "hew_string_clone",
        "hew_string_concat",
        "hew_string_contains",
        "hew_string_ends_with",
        "hew_string_find",
        "hew_string_from_char",
        "hew_string_get",
        "hew_string_index",
        "hew_string_is_alpha",
        "hew_string_is_alphanumeric",
        "hew_string_is_digit",
        "hew_string_is_empty",
        "hew_string_length",
        "hew_string_lines",
        "hew_string_repeat",
        "hew_string_replace",
        "hew_string_slice",
        "hew_string_slice_codepoints",
        "hew_string_split",
        "hew_string_starts_with",
        "hew_string_to_bytes",
        "hew_string_to_lowercase",
        "hew_string_to_uppercase",
        "hew_string_trim",
        "hew_u64_to_string",
        "hew_uint_to_string",
        "hew_vec_append",
        "hew_vec_append_layout",
        "hew_vec_clear",
        "hew_vec_clear_layout",
        "hew_vec_clone",
        "hew_vec_clone_layout",
        "hew_vec_clone_owned",
        "hew_vec_contains_f64",
        "hew_vec_contains_i32",
        "hew_vec_contains_i64",
        "hew_vec_contains_owned",
        "hew_vec_contains_str",
        "hew_vec_contains_thunk",
        "hew_vec_get_bool",
        "hew_vec_get_clone",
        "hew_vec_get_f32",
        "hew_vec_get_f64",
        "hew_vec_get_i16",
        "hew_vec_get_i32",
        "hew_vec_get_i64",
        "hew_vec_get_i8",
        "hew_vec_get_layout",
        "hew_vec_get_owned",
        "hew_vec_get_ptr",
        "hew_vec_get_str",
        "hew_vec_get_u16",
        "hew_vec_get_u8",
        "hew_vec_is_empty",
        "hew_vec_join_str",
        "hew_vec_len",
        "hew_vec_pop_bool",
        "hew_vec_pop_f32",
        "hew_vec_pop_f64",
        "hew_vec_pop_i16",
        "hew_vec_pop_i32",
        "hew_vec_pop_i64",
        "hew_vec_pop_i8",
        "hew_vec_pop_layout",
        "hew_vec_pop_owned",
        "hew_vec_pop_ptr",
        "hew_vec_pop_str",
        "hew_vec_pop_u16",
        "hew_vec_pop_u8",
        "hew_vec_push_bool",
        "hew_vec_push_f32",
        "hew_vec_push_f64",
        "hew_vec_push_i16",
        "hew_vec_push_i32",
        "hew_vec_push_i64",
        "hew_vec_push_i8",
        "hew_vec_push_layout",
        "hew_vec_push_owned",
        "hew_vec_push_owned_move",
        "hew_vec_push_ptr",
        "hew_vec_push_str",
        "hew_vec_push_u16",
        "hew_vec_push_u8",
        "hew_vec_remove_at_bool",
        "hew_vec_remove_at_f32",
        "hew_vec_remove_at_f64",
        "hew_vec_remove_at_i16",
        "hew_vec_remove_at_i32",
        "hew_vec_remove_at_i64",
        "hew_vec_remove_at_i8",
        "hew_vec_remove_at_layout",
        "hew_vec_remove_at_owned",
        "hew_vec_remove_at_ptr",
        "hew_vec_remove_at_str",
        "hew_vec_remove_at_u16",
        "hew_vec_remove_at_u8",
        "hew_vec_set_bool",
        "hew_vec_set_f32",
        "hew_vec_set_f64",
        "hew_vec_set_i16",
        "hew_vec_set_i32",
        "hew_vec_set_i64",
        "hew_vec_set_i8",
        "hew_vec_set_layout",
        "hew_vec_set_owned",
        "hew_vec_set_ptr",
        "hew_vec_set_str",
        "hew_vec_set_u16",
        "hew_vec_set_u8",
        "hew_vec_slice_range_bytesize",
        "hew_vec_slice_range_f64",
        "hew_vec_slice_range_i32",
        "hew_vec_slice_range_i64",
        "hew_vec_slice_range_layout",
        "hew_vec_slice_range_owned",
        "hew_vec_slice_range_ptr",
        "hew_vec_slice_range_str",
        "print",
        "print_str",
        "println",
        "println_str",
        "to_string_bool",
        "to_string_char",
        "to_string_f64",
        "to_string_i32",
        "to_string_i64",
        "to_string_u16",
        "to_string_u32",
        "to_string_u64",
        "to_string_u8",
    ];

    const VEC_RECEIVER_SYMBOLS: &[&str] = &[
        "hew_vec_append",
        "hew_vec_append_layout",
        "hew_vec_clear",
        "hew_vec_clear_layout",
        "hew_vec_clone",
        "hew_vec_clone_layout",
        "hew_vec_clone_owned",
        "hew_vec_contains_f64",
        "hew_vec_contains_i32",
        "hew_vec_contains_i64",
        "hew_vec_contains_owned",
        "hew_vec_contains_str",
        "hew_vec_contains_thunk",
        "hew_vec_get_bool",
        "hew_vec_get_clone",
        "hew_vec_get_f32",
        "hew_vec_get_f64",
        "hew_vec_get_i16",
        "hew_vec_get_i32",
        "hew_vec_get_i64",
        "hew_vec_get_i8",
        "hew_vec_get_layout",
        "hew_vec_get_owned",
        "hew_vec_get_ptr",
        "hew_vec_get_str",
        "hew_vec_get_u16",
        "hew_vec_get_u8",
        "hew_vec_is_empty",
        "hew_vec_join_str",
        "hew_vec_len",
        "hew_vec_pop_bool",
        "hew_vec_pop_f32",
        "hew_vec_pop_f64",
        "hew_vec_pop_i16",
        "hew_vec_pop_i32",
        "hew_vec_pop_i64",
        "hew_vec_pop_i8",
        "hew_vec_pop_layout",
        "hew_vec_pop_owned",
        "hew_vec_pop_ptr",
        "hew_vec_pop_str",
        "hew_vec_pop_u16",
        "hew_vec_pop_u8",
        "hew_vec_push_bool",
        "hew_vec_push_f32",
        "hew_vec_push_f64",
        "hew_vec_push_i16",
        "hew_vec_push_i32",
        "hew_vec_push_i64",
        "hew_vec_push_i8",
        "hew_vec_push_layout",
        "hew_vec_push_owned",
        "hew_vec_push_owned_move",
        "hew_vec_push_ptr",
        "hew_vec_push_str",
        "hew_vec_push_u16",
        "hew_vec_push_u8",
        "hew_vec_remove_at_bool",
        "hew_vec_remove_at_f32",
        "hew_vec_remove_at_f64",
        "hew_vec_remove_at_i16",
        "hew_vec_remove_at_i32",
        "hew_vec_remove_at_i64",
        "hew_vec_remove_at_i8",
        "hew_vec_remove_at_layout",
        "hew_vec_remove_at_owned",
        "hew_vec_remove_at_ptr",
        "hew_vec_remove_at_str",
        "hew_vec_remove_at_u16",
        "hew_vec_remove_at_u8",
        "hew_vec_set_bool",
        "hew_vec_set_f32",
        "hew_vec_set_f64",
        "hew_vec_set_i16",
        "hew_vec_set_i32",
        "hew_vec_set_i64",
        "hew_vec_set_i8",
        "hew_vec_set_layout",
        "hew_vec_set_owned",
        "hew_vec_set_ptr",
        "hew_vec_set_str",
        "hew_vec_set_u16",
        "hew_vec_set_u8",
        "hew_vec_slice_range_bytesize",
        "hew_vec_slice_range_f64",
        "hew_vec_slice_range_i32",
        "hew_vec_slice_range_i64",
        "hew_vec_slice_range_layout",
        "hew_vec_slice_range_owned",
        "hew_vec_slice_range_ptr",
        "hew_vec_slice_range_str",
    ];

    const COLLECTION_RECEIVER_SYMBOLS: &[&str] = &[
        "hew_bytes_get",
        "hew_hashmap_clone_layout",
        "hew_hashmap_contains_key_layout",
        "hew_hashmap_get_clone_layout",
        "hew_hashmap_get_layout",
        "hew_hashmap_insert_layout",
        "hew_hashmap_keys_layout",
        "hew_hashmap_len_layout",
        "hew_hashmap_remove_layout",
        "hew_hashmap_remove_take_layout",
        "hew_hashmap_values_layout",
        "hew_hashset_clone_layout",
        "hew_hashset_contains_layout",
        "hew_hashset_insert_layout",
        "hew_hashset_is_empty_layout",
        "hew_hashset_len_layout",
        "hew_hashset_remove_layout",
        "hew_hashset_to_vec_layout",
        "hew_string_get",
    ];

    const COPY_IN_SYMBOLS: &[&str] = &["hew_vec_push_owned", "hew_vec_set_owned"];

    const BYTES_RECEIVER_SYMBOLS: &[&str] = &[
        "hew_bytes_clear",
        "hew_bytes_contains",
        "hew_bytes_index",
        "hew_bytes_is_empty",
        "hew_bytes_len",
        "hew_bytes_pop",
        "hew_bytes_push",
        "hew_bytes_set",
        "hew_bytes_slice",
        "hew_bytes_to_string",
        "hew_vec_len",
    ];

    const BYTES_ALL_ARGS_SYMBOLS: &[&str] = &["hew_bytes_append"];

    const STRING_USE_SYMBOLS: &[&str] = &[
        "hew_string_char_at",
        "hew_string_char_at_utf8",
        "hew_string_char_count",
        "hew_string_chars",
        "hew_string_clone",
        "hew_string_concat",
        "hew_string_contains",
        "hew_string_ends_with",
        "hew_string_find",
        "hew_string_index",
        "hew_string_is_alpha",
        "hew_string_is_alphanumeric",
        "hew_string_is_digit",
        "hew_string_is_empty",
        "hew_string_length",
        "hew_string_lines",
        "hew_string_repeat",
        "hew_string_replace",
        "hew_string_slice",
        "hew_string_slice_codepoints",
        "hew_string_split",
        "hew_string_starts_with",
        "hew_string_to_bytes",
        "hew_string_to_lowercase",
        "hew_string_to_uppercase",
        "hew_string_trim",
        "hew_vec_push_str",
        "hew_vec_set_str",
    ];

    const PRINT_SINK_SYMBOLS: &[&str] = &["print", "print_str", "println", "println_str"];

    const FRESH_STRING_SYMBOLS: &[&str] = &[
        "hew_bool_to_string",
        "hew_bytes_to_string",
        "hew_char_to_string",
        "hew_float_to_string",
        "hew_i64_to_string",
        "hew_int_to_string",
        "hew_string_clone",
        "hew_string_concat",
        "hew_string_from_char",
        "hew_string_repeat",
        "hew_string_replace",
        "hew_string_slice",
        "hew_string_slice_codepoints",
        "hew_string_to_lowercase",
        "hew_string_to_uppercase",
        "hew_string_trim",
        "hew_u64_to_string",
        "hew_uint_to_string",
        "hew_vec_get_str",
        "hew_vec_pop_str",
        "hew_vec_remove_at_str",
        "to_string_bool",
        "to_string_char",
        "to_string_f64",
        "to_string_i32",
        "to_string_i64",
        "to_string_u16",
        "to_string_u32",
        "to_string_u64",
        "to_string_u8",
    ];

    const INTERIOR_ALIAS_SYMBOLS: &[&str] = &["hew_vec_get_owned", "hew_vec_get_ptr"];

    const FRESH_BYTES_SYMBOLS: &[&str] = &["hew_bytes_slice"];

    fn expected_set(symbols: &'static [&'static str]) -> BTreeSet<&'static str> {
        let set = symbols.iter().copied().collect::<BTreeSet<_>>();
        assert_eq!(set.len(), symbols.len(), "literal set contains duplicates");
        set
    }

    fn parity_symbols() -> BTreeSet<&'static str> {
        let mut symbols = CLASSIFIER_SYMBOLS.iter().copied().collect::<BTreeSet<_>>();
        symbols.extend(known_runtime_symbols().iter().copied());
        symbols
    }

    #[test]
    fn callee_ownership_contract_matches_literal_projection_sets() {
        assert_eq!(CLASSIFIER_SYMBOLS.len(), 168);
        let vec_receiver = expected_set(VEC_RECEIVER_SYMBOLS);
        let collection_receiver = expected_set(COLLECTION_RECEIVER_SYMBOLS);
        let copy_in = expected_set(COPY_IN_SYMBOLS);
        let bytes_receiver = expected_set(BYTES_RECEIVER_SYMBOLS);
        let bytes_all_args = expected_set(BYTES_ALL_ARGS_SYMBOLS);
        let string_use = expected_set(STRING_USE_SYMBOLS);
        let print_sink = expected_set(PRINT_SINK_SYMBOLS);
        let fresh_string = expected_set(FRESH_STRING_SYMBOLS);
        let interior_alias = expected_set(INTERIOR_ALIAS_SYMBOLS);
        let fresh_bytes = expected_set(FRESH_BYTES_SYMBOLS);

        assert_eq!(vec_receiver.len(), 91);
        assert_eq!(collection_receiver.len(), 19);
        assert_eq!(bytes_receiver.len(), 11);
        assert_eq!(string_use.len(), 28);
        assert_eq!(fresh_string.len(), 30);

        for symbol in parity_symbols() {
            let contract = callee_ownership_contract(symbol);
            assert_eq!(
                contract.borrows_vec_receiver(),
                vec_receiver.contains(symbol),
                "vec receiver projection mismatch for {symbol}",
            );
            assert_eq!(
                contract.borrows_collection_receiver(),
                collection_receiver.contains(symbol),
                "collection receiver projection mismatch for {symbol}",
            );
            assert_eq!(
                contract.is_vec_copy_in_element_store(),
                copy_in.contains(symbol),
                "vec copy-in projection mismatch for {symbol}",
            );
            assert_eq!(
                contract.borrows_collection_binder_receiver(),
                vec_receiver.contains(symbol) || collection_receiver.contains(symbol),
                "binder receiver projection mismatch for {symbol}",
            );
            assert_eq!(
                contract.borrows_bytes_receiver(),
                bytes_receiver.contains(symbol),
                "bytes receiver projection mismatch for {symbol}",
            );
            assert_eq!(
                contract.borrows_all_bytes_args(),
                bytes_all_args.contains(symbol),
                "bytes all-args projection mismatch for {symbol}",
            );
            assert_eq!(
                contract.borrows_string_use(),
                string_use.contains(symbol),
                "string-use projection mismatch for {symbol}",
            );
            assert_eq!(
                contract.borrows_string_call_args(),
                string_use.contains(symbol) || print_sink.contains(symbol),
                "string-callee projection mismatch for {symbol}",
            );
            assert_eq!(
                contract.produces_fresh_owned_string(),
                fresh_string.contains(symbol),
                "fresh-string projection mismatch for {symbol}",
            );
            assert_eq!(
                contract.produces_fresh_owned_bytes(),
                fresh_bytes.contains(symbol),
                "fresh-bytes projection mismatch for {symbol}",
            );
            assert_eq!(
                contract.returns_receiver_interior_alias(),
                interior_alias.contains(symbol),
                "interior-alias projection mismatch for {symbol}",
            );
        }
    }

    #[test]
    fn copy_in_tail_exemption_split_is_limited_to_owned_vec_stores() {
        let split_symbols = parity_symbols()
            .into_iter()
            .filter(|symbol| callee_ownership_contract(symbol).is_vec_copy_in_element_store())
            .collect::<Vec<_>>();
        assert_eq!(
            split_symbols,
            vec!["hew_vec_push_owned", "hew_vec_set_owned"]
        );
    }
}
#[cfg(test)]
mod layout_key_shortening_guard {
    //! Structural guard: the GENERIC record-layout key the field-store,
    //! field-read, and `StructInit` arms build must route the OUTER record name
    //! through `short_name` before mangling. A generic record's layout is
    //! registered under the bare outer name (`Holder$$Box`), so a module-
    //! qualified outer spelling reached under qualified-by-default (`q.Holder`)
    //! would mangle to a divergent key and miss its layout unless the qualifier
    //! is shortened here. The three generic arms therefore use
    //! `mangle_layout_key(short_name(name|tname), args)` — never a bare
    //! `mangle_layout_key(name, args)`. (The MONOMORPHIC arms intentionally keep
    //! the possibly-qualified `name` so a same-bare-name record registered under
    //! its QUALIFIED key — `widgeti8.Widget` vs `widgeti64.Widget`, divergent
    //! layouts — hits its own layout, with `lookup_record_field_order` stripping
    //! the qualifier on a miss.) This scan keeps producer (registration) and
    //! consumer (field load/store) generic keys from silently diverging.

    /// The production (non-test) sources containing record layout-key consumers.
    ///
    /// Normalises CRLF→LF before splitting: on a Windows checkout
    /// (`core.autocrlf=true`) the embedded `include_str!` source carries
    /// `\r\n`, so a split on the LF-anchored `\n#[cfg(test)]\n` never matches
    /// and would return the WHOLE file — pulling the test module's own
    /// `mangle_layout_key(name, args)` string literals into the scan and
    /// breaking the bare-key count guard below. Normalising keeps the guard
    /// deterministic across line-ending conventions.
    fn production_source() -> String {
        [include_str!("mod.rs"), include_str!("expr.rs")]
            .into_iter()
            .map(|src| {
                src.replace("\r\n", "\n")
                    .split("\n#[cfg(test)]\nmod ")
                    .next()
                    .expect("lower module source has a non-test prefix")
                    .to_string()
            })
            .collect::<Vec<_>>()
            .join("\n")
    }

    /// The field-store AND field-read GENERIC arms each shorten the outer name.
    /// If a future edit drops the `short_name` wrap (re-opening the qualified-
    /// spine layout miss for generic field load/store), this check fails and
    /// names the regression.
    #[test]
    fn field_order_generic_arms_route_outer_name_through_short_name() {
        let prod = production_source();
        let generic_arm = "mangle_layout_key(short_name(name), args)";
        let generic_hits = prod.matches(generic_arm).count();
        assert!(
            generic_hits >= 2,
            "expected the field-store AND field-read generic arms to build the layout \
             key via `{generic_arm}` (parity with the StructInit arm); found {generic_hits}. \
             A bare `mangle_layout_key(name, args)` re-opens the qualified-spine layout miss."
        );
        // No field-order generic arm may feed a BARE-outer-name mangle. The only
        // remaining `mangle_layout_key(name, args)` (bare outer) is the
        // vec-owned-element key path, which has its own `short_name`-aware
        // fallback lookups — assert the field-order arms are not among them by
        // requiring every `mangle_layout_key(name, args)` to be paired with a
        // `short_name`-based membership check within a few lines (the
        // vec-owned-key shape). Here we simply pin that the shortened form is
        // the dominant one for the field-order sites.
        assert!(
            prod.matches("mangle_layout_key(name, args)").count() <= 1,
            "at most the single vec-owned-element-key site may mangle a bare outer \
             name; a new bare-outer `mangle_layout_key(name, args)` likely feeds a \
             field-order lookup and must shorten the outer name instead"
        );
    }

    /// The `StructInit` arm — the parity reference — also shortens its outer
    /// name. Pinned here so the three arms cannot drift apart.
    #[test]
    fn struct_init_arm_routes_outer_name_through_short_name() {
        let prod = production_source();
        assert!(
            prod.contains("mangle_layout_key(short_name(tname), args)"),
            "the StructInit record-key arm must shorten its outer name via \
             `mangle_layout_key(short_name(tname), args)` — the parity reference the \
             field-store and field-read arms mirror"
        );
    }
}
#[cfg(test)]
mod enum_layout_tests {
    use std::collections::HashMap;

    use super::{lower_hir_module, Builder};
    use hew_hir::{
        EnumLayout, EnumMonoKey, EnumVariantLayout, HirItem, HirModule, HirNodeId, HirTypeDecl,
        HirVariant, HirVariantKind, ItemId, ResourceMarker, SiteId,
    };
    use hew_types::{ChildSlot, ResolvedTy};

    fn minimal_module(items: Vec<HirItem>) -> HirModule {
        HirModule {
            items,
            diagnostic_source_modules: HashMap::default(),
            root_item_ids: std::collections::HashSet::new(),
            wire_layouts: std::sync::Arc::new(HashMap::default()),
            type_classes: hew_hir::TypeClassTable::default(),
            monomorphisations: vec![],
            call_site_type_args: HashMap::<SiteId, _>::default(),
            vec_generic_element_abi: HashMap::default(),
            record_layouts: vec![],
            enum_layouts: vec![],
            machine_instantiations: vec![],
            supervisor_child_slots: HashMap::<SiteId, ChildSlot>::default(),
            pool_accessor_sites: HashMap::default(),
            regex_literals: vec![],
        }
    }

    fn unit_variant(name: &str) -> HirVariant {
        HirVariant {
            name: name.to_string(),
            kind: HirVariantKind::Unit,
        }
    }

    fn tuple_variant(name: &str, tys: Vec<ResolvedTy>) -> HirVariant {
        HirVariant {
            name: name.to_string(),
            kind: HirVariantKind::Tuple(tys),
        }
    }

    fn struct_variant(name: &str, fields: Vec<(&str, ResolvedTy)>) -> HirVariant {
        HirVariant {
            name: name.to_string(),
            kind: HirVariantKind::Struct(
                fields
                    .into_iter()
                    .map(|(n, t)| (n.to_string(), t))
                    .collect(),
            ),
        }
    }

    fn mixed_enum_decl() -> HirTypeDecl {
        // `enum Shape { Point; Line(i64); Box { w: i64, h: i64 } }` — one
        // variant of each shape. Used to verify monomorphic mixed-enum
        // layout registration end-to-end.
        HirTypeDecl {
            id: ItemId(0),
            node: HirNodeId(0),
            name: "Shape".to_string(),
            defining_module: None,
            marker: ResourceMarker::None,
            is_opaque: false,
            is_indirect: false,
            consuming_methods: vec![],
            type_params: vec![],
            fields: vec![],
            variants: vec![
                unit_variant("Point"),
                tuple_variant("Line", vec![ResolvedTy::I64]),
                struct_variant("Box", vec![("w", ResolvedTy::I64), ("h", ResolvedTy::I64)]),
            ],
            span: 0..0,
        }
    }

    #[test]
    fn monomorphic_mixed_enum_registers_full_layout_without_diagnostic() {
        // Substrate-anchor test: the previous `mixed_enum_emits_typed_diagnostic`
        // shape is gone — monomorphic mixed enums now lower end-to-end with
        // per-variant `field_tys` populated. Variant-index ordering is
        // declaration-order (HIR ctor pre-pass authoritative — lane-plan D2).
        let module = minimal_module(vec![HirItem::TypeDecl(mixed_enum_decl())]);
        let pipeline = lower_hir_module(&module);

        assert!(
            pipeline.diagnostics.is_empty(),
            "no diagnostics expected for monomorphic mixed enum; got: {:?}",
            pipeline.diagnostics
        );
        let user_layouts: Vec<_> = pipeline
            .enum_layouts
            .iter()
            .filter(|l| {
                l.name != "LookupError"
                    && l.name != "SendError"
                    && l.name != "AskError"
                    && l.name != "TimeoutError"
                    && l.name != "LinkError"
                    && l.name != "CrashAction"
                    && l.name != "CrashKind"
            })
            .collect();
        assert_eq!(user_layouts.len(), 1, "expected one EnumLayout for Shape");
        let layout = user_layouts[0];
        assert_eq!(layout.name, "Shape");
        assert_eq!(layout.variants.len(), 3);
        // Declaration order is load-bearing: Point=0, Line=1, Box=2. MIR's
        // match-arm dispatch uses these tag values, so any drift from the
        // HIR ctor pre-pass would silently mis-route arms.
        assert_eq!(layout.variants[0].name, "Point");
        assert!(layout.variants[0].field_tys.is_empty());
        assert_eq!(layout.variants[1].name, "Line");
        assert_eq!(layout.variants[1].field_tys, vec![ResolvedTy::I64]);
        assert_eq!(layout.variants[2].name, "Box");
        assert_eq!(
            layout.variants[2].field_tys,
            vec![ResolvedTy::I64, ResolvedTy::I64]
        );
    }

    #[test]
    fn all_unit_enum_registers_layout_without_diagnostic() {
        // `enum Colour { Red; Green; Blue; }` — three unit variants, no payload.
        let decl = HirTypeDecl {
            id: ItemId(1),
            node: HirNodeId(1),
            name: "Colour".to_string(),
            defining_module: None,
            marker: ResourceMarker::None,
            is_opaque: false,
            is_indirect: false,
            consuming_methods: vec![],
            type_params: vec![],
            fields: vec![],
            variants: vec![
                unit_variant("Red"),
                unit_variant("Green"),
                unit_variant("Blue"),
            ],
            span: 0..0,
        };
        let module = minimal_module(vec![HirItem::TypeDecl(decl)]);
        let pipeline = lower_hir_module(&module);

        assert!(
            pipeline.diagnostics.is_empty(),
            "no diagnostics expected for all-unit enum; got: {:?}",
            pipeline.diagnostics
        );
        let user_layouts: Vec<_> = pipeline
            .enum_layouts
            .iter()
            .filter(|l| {
                l.name != "LookupError"
                    && l.name != "SendError"
                    && l.name != "AskError"
                    && l.name != "TimeoutError"
                    && l.name != "LinkError"
                    && l.name != "CrashAction"
                    && l.name != "CrashKind"
            })
            .collect();
        assert_eq!(user_layouts.len(), 1, "expected one EnumLayout for Colour");
        assert_eq!(user_layouts[0].name, "Colour");
        assert_eq!(user_layouts[0].variants.len(), 3);
    }

    #[test]
    fn fieldless_layout_key_matches_find_enum_layout_for_monomorphic() {
        use crate::model::{EnumLayout, MachineVariantLayout};

        let layouts = vec![EnumLayout {
            name: "Colour".to_string(),
            tag_width: 1,
            variants: vec![
                MachineVariantLayout {
                    name: "Red".to_string(),
                    field_tys: vec![],
                    field_names: vec![],
                },
                MachineVariantLayout {
                    name: "Green".to_string(),
                    field_tys: vec![],
                    field_names: vec![],
                },
                MachineVariantLayout {
                    name: "Blue".to_string(),
                    field_tys: vec![],
                    field_names: vec![],
                },
            ],
            is_indirect: false,
        }];
        let builder = Builder {
            enum_layouts: layouts.clone(),
            ..Builder::default()
        };
        let colour = ResolvedTy::named_user("Colour", vec![]);

        assert_eq!(
            builder.fieldless_enum_layout_key(&colour),
            Some("Colour".to_string())
        );
        assert_eq!(
            crate::model::find_enum_layout("Colour", &[], &layouts)
                .map(|layout| layout.name.clone()),
            Some("Colour".to_string())
        );
    }

    #[test]
    fn fieldless_layout_key_none_for_payload_enum() {
        use crate::model::{EnumLayout, MachineVariantLayout};

        let builder = Builder {
            enum_layouts: vec![EnumLayout {
                name: "MaybeI64".to_string(),
                tag_width: 1,
                variants: vec![
                    MachineVariantLayout {
                        name: "Some".to_string(),
                        field_tys: vec![ResolvedTy::I64],
                        field_names: vec![],
                    },
                    MachineVariantLayout {
                        name: "None".to_string(),
                        field_tys: vec![],
                        field_names: vec![],
                    },
                ],
                is_indirect: false,
            }],
            ..Builder::default()
        };

        assert_eq!(
            builder.fieldless_enum_layout_key(&ResolvedTy::named_user("MaybeI64", vec![])),
            None
        );
    }

    #[test]
    fn fieldless_layout_key_generic_uses_mangled_key_not_short_fallback() {
        use crate::model::{EnumLayout, MachineVariantLayout};

        fn fieldless_layout(name: String) -> EnumLayout {
            EnumLayout {
                name,
                tag_width: 1,
                variants: vec![
                    MachineVariantLayout {
                        name: "Empty".to_string(),
                        field_tys: vec![],
                        field_names: vec![],
                    },
                    MachineVariantLayout {
                        name: "Full".to_string(),
                        field_tys: vec![],
                        field_names: vec![],
                    },
                ],
                is_indirect: false,
            }
        }

        let registered_key = hew_hir::mangle("Slot", &[ResolvedTy::named_user("Box", vec![])]);
        let builder = Builder {
            enum_layouts: vec![
                fieldless_layout(registered_key.clone()),
                fieldless_layout("decoy.Slot".to_string()),
            ],
            ..Builder::default()
        };
        let qualified =
            ResolvedTy::named_user("Slot", vec![ResolvedTy::named_user("lmonobox.Box", vec![])]);
        assert_eq!(
            builder.fieldless_enum_layout_key(&qualified),
            Some(registered_key)
        );

        let missing_mangled_builder = Builder {
            enum_layouts: vec![fieldless_layout("decoy.Slot".to_string())],
            ..Builder::default()
        };
        let missing_mangled_probe = ResolvedTy::named_user(
            "Slot",
            vec![ResolvedTy::named_user("lmonobox.Crate", vec![])],
        );
        assert_eq!(
            missing_mangled_builder.fieldless_enum_layout_key(&missing_mangled_probe),
            None
        );
    }

    #[test]
    fn generic_enum_with_registered_instantiation_emits_mir_layout_without_diagnostic() {
        // Invariant: when `module.enum_layouts` carries at least one entry for
        // a generic enum decl's origin `ItemId`, the MIR pipeline emits the
        // mangled layout and no diagnostic fires.
        //
        // Fixture: `enum Option<T> { Some(T); None }` with one instantiation
        // `Option<i64>` pre-registered by the HIR mono pass.
        let option_item_id = ItemId(10);
        let decl = HirTypeDecl {
            id: option_item_id,
            node: HirNodeId(10),
            name: "Option".to_string(),
            defining_module: None,
            marker: ResourceMarker::None,
            is_opaque: false,
            is_indirect: false,
            consuming_methods: vec![],
            type_params: vec!["T".to_string()],
            fields: vec![],
            variants: vec![
                tuple_variant("Some", vec![ResolvedTy::named_user("T", vec![])]),
                unit_variant("None"),
            ],
            span: 0..0,
        };
        // The HIR mono pass would have produced this entry:
        let hir_layout = EnumLayout {
            key: EnumMonoKey {
                origin: option_item_id,
                origin_name: "Option".to_string(),
                type_args: vec![ResolvedTy::I64],
            },
            mangled_name: "Option$$i64".to_string(),
            variants: vec![
                EnumVariantLayout {
                    name: "Some".to_string(),
                    field_tys: vec![ResolvedTy::I64],
                },
                EnumVariantLayout {
                    name: "None".to_string(),
                    field_tys: vec![],
                },
            ],
        };
        let mut module = minimal_module(vec![HirItem::TypeDecl(decl)]);
        module.enum_layouts = vec![hir_layout];

        let pipeline = lower_hir_module(&module);

        assert!(
            pipeline.diagnostics.is_empty(),
            "no diagnostic expected when instantiation is registered; got: {:?}",
            pipeline.diagnostics
        );
        // The MIR pipeline emits the layout under the mangled name (not "Option").
        // Codegen finds it via the mangled key in machine_layout_map.
        // The builtin `LookupError` layout is always registered out-of-band; filter
        // it out so this test asserts on the user-declared layouts only.
        let user_layouts: Vec<_> = pipeline
            .enum_layouts
            .iter()
            .filter(|l| {
                l.name != "LookupError"
                    && l.name != "SendError"
                    && l.name != "AskError"
                    && l.name != "TimeoutError"
                    && l.name != "LinkError"
                    && l.name != "CrashAction"
                    && l.name != "CrashKind"
            })
            .collect();
        assert_eq!(
            user_layouts.len(),
            1,
            "expected one MIR EnumLayout for Option$$i64; got: {user_layouts:?}"
        );
        let layout = user_layouts[0];
        assert_eq!(
            layout.name, "Option$$i64",
            "layout must be emitted under mangled name"
        );
        assert_eq!(layout.variants.len(), 2);
        assert_eq!(layout.variants[0].name, "Some");
        assert_eq!(layout.variants[0].field_tys, vec![ResolvedTy::I64]);
        assert_eq!(layout.variants[1].name, "None");
        assert!(layout.variants[1].field_tys.is_empty());
    }
}
