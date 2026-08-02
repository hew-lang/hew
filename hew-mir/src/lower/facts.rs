#[cfg(test)]
use super::*;
#[cfg(not(test))]
use super::{
    named_type_names, project_match_ownership_mode, BindingId, Builder, BuiltinType,
    CheckedMirFunction, ConsumeVerdict, DecisionFact, ElaboratedMirFunction, FunctionCallConv,
    HashMap, HashSet, HirBlock, HirExpr, HirExprKind, HirFn, HirItem, HirStmtKind, Instr,
    IntentKind, LayoutReadiness, MirDiagnostic, MirDiagnosticKind, MirStatement, ParamBoundaryFact,
    ParamBoundaryMode, ParamCrashCleanupKind, ParamLoanStorage, ParamOwnershipFacts,
    ParamRepresentationEffect, Place, ProjectMatchOwnershipMode, RawMirFunction, ResolvedRef,
    ResolvedTy, ResourceMarker, ScanCtx, SiteId, Strategy, SuspendKind, Terminator, ValueClass,
};

impl Builder {
    pub(super) fn binding_ref_use_intent(&self, expr: &HirExpr) -> IntentKind {
        if self.param_ownership.borrow_arg_sites.contains(&expr.site)
            || self.bytes_local_share_sites.contains(&expr.site)
            || self.string_local_share_sites.contains_key(&expr.site)
        {
            IntentKind::Read
        } else {
            expr.intent
        }
    }

    pub(super) fn is_consumed_bound_local(&self, expr: &HirExpr) -> bool {
        let HirExprKind::BindingRef {
            resolved: ResolvedRef::Binding(binding),
            ..
        } = &expr.kind
        else {
            return false;
        };
        self.binding_locals.contains_key(binding)
            && !self.capture_env_sources.contains_key(binding)
            && !self.funcupdate_param_ids.contains(binding)
            && self.binding_ref_use_intent(expr) == IntentKind::Consume
    }
}

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
    fresh: &'f crate::return_provenance::FreshOwnerVerdicts,
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
        // This prescan runs BEFORE the function is lowered, so the lowering's
        // proven-foreign binder ledger does not exist yet. Empty is safe here:
        // a `let` bound to a foreign producer already fails this resolver's own
        // call check (the authority declines a direct extern and every wrapper
        // of one), so the ledger would add nothing a funcupdate base could use.
        proven_foreign: HashSet::new(),
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
/// `fresh` is the module's table-aware freshness authority
/// (`FreshOwnerVerdicts`) consulted when a definition is a call.
struct BaseOwnerResolver<'f> {
    defs: HashMap<BindingId, Vec<&'f HirExpr>>,
    let_or_param: HashSet<BindingId>,
    params: HashSet<BindingId>,
    memo: HashMap<BindingId, bool>,
    fresh: &'f crate::return_provenance::FreshOwnerVerdicts,
    proven_foreign: HashSet<BindingId>,
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
            _ => Builder::expr_is_materialized_owner(
                init,
                self.fresh,
                &self.params,
                &self.proven_foreign,
            ),
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

/// Every whole-binding reassignment of `binding` nested within `block`.
///
/// Binding ids are function-unique, so the existing exhaustive definition
/// collector can be reused here: the queried binding was declared outside the
/// while-let body, and therefore every definition found inside the body is an
/// assignment rather than a shadowing `let`.
pub(crate) fn binding_reassignment_values_in_block(
    block: &HirBlock,
    binding: BindingId,
) -> Vec<&HirExpr> {
    let mut defs = HashMap::new();
    let mut let_ids = HashSet::new();
    collect_binding_defs_in_block(block, &mut defs, &mut let_ids);
    defs.remove(&binding).unwrap_or_default()
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
        HirExprKind::Call { callee, args, .. } | HirExprKind::SpawnedCall { callee, args, .. } => {
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
        HirExprKind::RcIntrinsic {
            receiver, value, ..
        } => {
            if let Some(receiver) = receiver {
                collect_binding_defs_in_expr(receiver, defs, let_ids);
            }
            if let Some(value) = value {
                collect_binding_defs_in_expr(value, defs, let_ids);
            }
        }
        HirExprKind::ChannelRecvAwait { receiver, .. }
        | HirExprKind::CancellationTokenIsCancelled { receiver }
        | HirExprKind::GeneratorNext { receiver, .. }
        | HirExprKind::MachineStateName { receiver, .. }
        | HirExprKind::RecordCloneCall { src: receiver, .. }
        | HirExprKind::SubsumedValue {
            source: receiver, ..
        } => {
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

/// Symbols for source-declared extern functions. These bodyless ABI calls are
/// the sole free-call shape whose resource `Read` intent was proved directly
/// by HIR's generated FFI contract table.
fn collect_extern_fn_names(items: &[HirItem]) -> HashSet<String> {
    items
        .iter()
        .filter_map(|item| match item {
            HirItem::ExternFn(extern_fn) => Some(extern_fn.name.clone()),
            _ => None,
        })
        .collect()
}

fn resolved_ty_matches_impl_self(ty: &ResolvedTy, self_ty: &str) -> bool {
    let rendered = ty.user_facing().to_string();
    matches!(ty, ResolvedTy::Named { name, .. } if name == self_ty)
        || rendered
            .split('<')
            .next()
            .is_some_and(|root| root == self_ty)
}

/// Collect the subset of `impl` items whose parameter zero is a true receiver.
///
/// An associated/static function is present in `collect_method_item_ids`, but
/// its first value parameter is ordinary call data and must retain the same
/// carrier/consume contract as any other non-receiver parameter. Receiver
/// identity follows the checker authority: bare `self`, or a parameter whose
/// resolved named type is the enclosing impl's Self type.
fn collect_receiver_method_item_ids(
    fns: &HashMap<hew_hir::ItemId, &HirFn>,
    items: &[HirItem],
    methods: &HashSet<hew_hir::ItemId>,
) -> HashSet<hew_hir::ItemId> {
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
    fns.iter()
        .filter(|(id, _)| methods.contains(id))
        .filter_map(|(&id, &f)| {
            let param0 = f.params.first()?;
            (param0.name == "self"
                || method_self_type
                    .get(f.name.as_str())
                    .is_some_and(|self_ty| resolved_ty_matches_impl_self(&param0.ty, self_ty)))
            .then_some(id)
        })
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
                || method_self_type
                    .get(f.name.as_str())
                    .is_some_and(|self_ty| resolved_ty_matches_impl_self(&p.ty, self_ty))
        }));
        for (i, param) in f.params.iter().enumerate().skip(receiver_arity) {
            if is_user_resource_ty(&param.ty, type_classes) {
                param_consume.insert((id, i), true);
            }
        }
    }
}
/// Collect every `impl`/trait method whose PARAM 0 is a by-value `self`
/// receiver of a NON-resource type — the receiver slot the borrow-site
/// collector records for Shape A of #2753 so the caller keeps a value-receiver
/// record/collection's scope-exit drop.
///
/// Receiver identity follows the checker's rule (mirrors the receiver-arity
/// split in `force_consume_method_nonreceiver_resource_params` /
/// `is_receiver_param`): param 0 is the receiver iff it is named `self` OR its
/// type is the impl Self type (the stdlib writes `fn close(child: Child)`).
///
/// Two exclusions keep the caller-side receiver drop fail-closed against a
/// double-free:
///   * a `#[resource]` receiver — a `close`-style `self` reads BORROW in the
///     body scan yet CONSUMES at the call site via terminal-consume
///     registration (`raii-null-after-move`, LESSONS #19); the destructor
///     consume is invisible to `param_consumed_in_body`, so recording it would
///     free the receiver at BOTH the caller and the callee.
///   * an associated/static function (param 0 is NOT a receiver) — its first
///     argument keeps today's move-in-and-callee-drops posture; the ratified
///     borrow-default surface is free-function value params and method
///     receivers, not method non-receiver args.
///
/// Heap-ownership is intentionally NOT gated here: the recording only marks the
/// receiver's `SiteId`, and the downstream per-type sole-owner prover
/// (`derive_{record,tuple,enum}_composite_drop_allowed` /
/// `caller_borrowed_temp_arg_owned_ty`) decides the actual drop exactly as for
/// a free-fn positional borrow arg — a non-heap-owning receiver yields no drop.
/// Consuming the layout-blind checker verdict here and deferring the structural
/// question to the one heap-ownership authority avoids re-deriving it
/// (`checker-authority`).
fn collect_borrow_receiver_methods(
    fns: &HashMap<hew_hir::ItemId, &HirFn>,
    items: &[HirItem],
    methods: &HashSet<hew_hir::ItemId>,
    type_classes: &hew_hir::TypeClassTable,
) -> HashSet<hew_hir::ItemId> {
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
    fns.iter()
        .filter(|(id, _)| methods.contains(id))
        .filter_map(|(&id, &f)| {
            let param0 = f.params.first()?;
            let is_receiver = param0.name == "self"
                || method_self_type
                    .get(f.name.as_str())
                    .is_some_and(|self_ty| resolved_ty_matches_impl_self(&param0.ty, self_ty));
            (is_receiver && !is_user_resource_ty(&param0.ty, type_classes)).then_some(id)
        })
        .collect()
}
/// A method receiver earns the caller-side borrow-drop only when it is a WHOLE
/// owned operand: a bare binding reference (a named `let`/owned local — the
/// #2735 `proven_borrow_whole_arg_locals` preserve path) or a fresh
/// materialised producer temporary (the #2743 caller-side temp mint). A
/// projection (`a.b.touch()`), an index/slice, a call result, or any other
/// alias is EXCLUDED: its drop belongs to the base owner, and `base_local` of a
/// projection base would misattribute the preserve to a live owner
/// (leak-not-double-free). Mirrors `caller_borrowed_temp_arg_owned_ty`'s
/// fresh-producer allowlist and `proven_borrow_whole_arg_locals`'s whole-arg
/// requirement.
fn receiver_is_whole_owned_operand(receiver: &HirExpr) -> bool {
    matches!(
        &receiver.kind,
        HirExprKind::BindingRef {
            resolved: ResolvedRef::Binding(_),
            ..
        } | HirExprKind::StructInit { .. }
            | HirExprKind::TupleLiteral { .. }
            | HirExprKind::MachineVariantCtor { .. }
            | HirExprKind::RecordCloneCall { .. }
    )
}
/// Prove borrow-only direct-call parameters across every free function. This
/// broader summary is consumed only by collection escape analysis; it does not
/// change call-site move intent or register non-resource parameters for
/// callee-side drops.
fn compute_call_param_consumption(
    fns: &HashMap<hew_hir::ItemId, &HirFn>,
    methods: &HashSet<hew_hir::ItemId>,
    extern_fn_names: &HashSet<String>,
    true_receiver_methods: &HashSet<hew_hir::ItemId>,
    receiver_methods: &HashSet<hew_hir::ItemId>,
    resource_param_consume: &HashMap<(hew_hir::ItemId, usize), bool>,
    owned_projection_sinks: bool,
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
                        extern_fn_names,
                        true_receiver_methods,
                        receiver_methods,
                        owned_projection_sinks,
                        assume_forward_borrows: false,
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
/// Refine the converged BOOL consume map into the three-valued
/// [`ConsumeVerdict`] carried on `ParamOwnershipFacts::call_param_consume`.
///
/// The consume/borrow bit is authoritative and UNCHANGED — this only labels
/// WHY each already-decided consume flipped, so `is_consume()` on the result is
/// bit-identical to the input `bool`. The split is a single independent
/// re-scan per consuming param with `assume_forward_borrows` set, which
/// suppresses the fail-closed forward-to-unproven/consuming disjunct: a param
/// that STILL scans consume under that optimism has a positive escape
/// (returned/stored/sent/captured) → `ProvenConsume`; one that no longer does
/// flipped solely on the forward disjunct → `ConservativeConsume`. A positive
/// escape dominates a co-occurring forward (the optimistic scan finds the
/// escape regardless), so the label is order-insensitive and deterministic.
fn refine_call_param_verdicts(
    fns: &HashMap<hew_hir::ItemId, &HirFn>,
    methods: &HashSet<hew_hir::ItemId>,
    extern_fn_names: &HashSet<String>,
    true_receiver_methods: &HashSet<hew_hir::ItemId>,
    receiver_methods: &HashSet<hew_hir::ItemId>,
    consume_bool: &HashMap<(hew_hir::ItemId, usize), bool>,
) -> HashMap<(hew_hir::ItemId, usize), ConsumeVerdict> {
    let proven_cx = ScanCtx {
        consume: consume_bool,
        methods,
        extern_fn_names,
        true_receiver_methods,
        receiver_methods,
        owned_projection_sinks: false,
        assume_forward_borrows: true,
    };
    consume_bool
        .iter()
        .map(|(&(id, i), &consumed)| {
            if !consumed {
                return ((id, i), ConsumeVerdict::ProvenBorrow);
            }
            // A param present in the bool map is always a real param of a
            // known fn; the proven-only re-scan reads its body directly.
            let proven = fns.get(&id).is_some_and(|f| {
                f.params
                    .get(i)
                    .is_some_and(|param| param_consumed_in_body(&f.body, param.id, &proven_cx))
            });
            let verdict = if proven {
                ConsumeVerdict::ProvenConsume
            } else {
                ConsumeVerdict::ConservativeConsume
            };
            ((id, i), verdict)
        })
        .collect()
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
#[allow(
    clippy::too_many_lines,
    reason = "single orchestration of the param-ownership fixpoint, carrier summary, borrow-site collection, and verdict refinement; splitting would scatter the shared method/receiver sets"
)]
pub(super) fn compute_param_ownership(
    fns: &HashMap<hew_hir::ItemId, &HirFn>,
    items: &[HirItem],
    type_classes: &hew_hir::TypeClassTable,
    caller_visible_param_projections: &HashSet<(hew_hir::ItemId, usize)>,
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
    let extern_fn_names = collect_extern_fn_names(items);
    // Only this subset owns a receiver slot. Associated/static functions are
    // method items for symbol/dispatch purposes, but parameter zero remains
    // ordinary call data and must not lose carrier ownership evidence.
    let true_receiver_methods = collect_receiver_method_item_ids(fns, items, &methods);
    // Shape A of #2753: the subset of `methods` whose param 0 is a by-value
    // NON-resource `self` receiver — the receiver slot the borrow-site collector
    // records so the caller keeps a value-receiver record/collection's
    // scope-exit drop. Excludes resource receivers (double-free trap) and
    // associated/static functions (no receiver). Consulted only in the
    // `proven_borrow_arg_sites` walk.
    let receiver_methods = collect_borrow_receiver_methods(fns, items, &methods, type_classes);
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
                        extern_fn_names: &extern_fn_names,
                        true_receiver_methods: &true_receiver_methods,
                        receiver_methods: &receiver_methods,
                        owned_projection_sinks: false,
                        assume_forward_borrows: false,
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
    let call_param_consume_bool = compute_call_param_consumption(
        fns,
        &methods,
        &extern_fn_names,
        &true_receiver_methods,
        &receiver_methods,
        &param_consume,
        false,
    );
    let mut call_param_owned_carrier = compute_call_param_consumption(
        fns,
        &methods,
        &extern_fn_names,
        &true_receiver_methods,
        &receiver_methods,
        &param_consume,
        true,
    );
    // A by-value method receiver is governed by the receiver's accurate
    // read/consume intent, not the ordinary free-call carrier contract. In
    // particular, `Arena<T>::insert(self, value)` mutates through borrowed
    // `self`; only `value` crosses into owned storage.
    for method in &true_receiver_methods {
        call_param_owned_carrier.remove(&(*method, 0));
    }
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
            extern_fn_names: &extern_fn_names,
            true_receiver_methods: &true_receiver_methods,
            receiver_methods: &receiver_methods,
            owned_projection_sinks: false,
            assume_forward_borrows: false,
        },
    );
    let proven_borrow_arg_sites = collect_module_borrow_arg_sites(
        items,
        &ScanCtx {
            consume: &call_param_consume_bool,
            methods: &methods,
            extern_fn_names: &extern_fn_names,
            true_receiver_methods: &true_receiver_methods,
            receiver_methods: &receiver_methods,
            owned_projection_sinks: false,
            assume_forward_borrows: false,
        },
    );
    // Refine the converged BOOL consume verdict into the three-valued
    // `ConsumeVerdict` (see `refine_call_param_verdicts`).
    let call_param_consume = refine_call_param_verdicts(
        fns,
        &methods,
        &extern_fn_names,
        &true_receiver_methods,
        &receiver_methods,
        &call_param_consume_bool,
    );
    ParamOwnershipFacts {
        produced_value_facts: HashMap::new(),
        param_consume,
        borrow_arg_sites,
        proven_borrow_arg_sites,
        call_param_consume,
        call_param_owned_carrier,
        caller_visible_param_projections: caller_visible_param_projections.clone(),
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum RepresentationEffectState {
    None,
    MayReplace,
    Unproven,
}

fn boundary_decisions(decisions: &[DecisionFact]) -> Vec<ParamBoundaryFact> {
    let mut facts = decisions
        .iter()
        .filter_map(|decision| match decision.strategy {
            Strategy::ParamBoundary(fact) => Some(fact),
            _ => None,
        })
        .collect::<Vec<_>>();
    facts.sort_unstable_by_key(|fact| fact.param_index);
    facts
}

fn assert_total_boundary_facts(name: &str, param_count: usize, facts: &[ParamBoundaryFact]) {
    assert_eq!(
        facts.len(),
        param_count,
        "function `{name}` must carry exactly one boundary fact per parameter"
    );
    let param_count_u32 =
        u32::try_from(param_count).expect("function parameter count exceeds u32::MAX");
    for (expected_index, fact) in facts.iter().enumerate() {
        assert_eq!(
            fact.param_count, param_count_u32,
            "function `{name}` boundary facts disagree on parameter count"
        );
        assert_eq!(
            usize::try_from(fact.param_index).expect("parameter index exceeds usize"),
            expected_index,
            "function `{name}` boundary facts must cover every parameter exactly once"
        );
    }
}

fn seed_missing_boundary_facts(raw: &mut RawMirFunction) {
    let existing = boundary_decisions(&raw.decisions);
    if !existing.is_empty() || raw.params.is_empty() {
        assert_total_boundary_facts(&raw.name, raw.params.len(), &existing);
        return;
    }

    let param_count =
        u32::try_from(raw.params.len()).expect("function parameter count exceeds u32::MAX");
    for (param_index, ty) in raw.params.iter().cloned().enumerate() {
        let param_index =
            u32::try_from(param_index).expect("function parameter count exceeds u32::MAX");
        let mode = if raw.call_conv == FunctionCallConv::ActorHandler {
            ParamBoundaryMode::OwnedMessage
        } else {
            ParamBoundaryMode::BorrowReadOnly
        };
        raw.decisions.push(DecisionFact {
            site: SiteId(param_index),
            ty,
            value_class: ValueClass::Unknown,
            intent: IntentKind::Unknown,
            strategy: Strategy::ParamBoundary(ParamBoundaryFact {
                param_index,
                param_count,
                caller_visible_projection: false,
                mode,
            }),
            why: "synthetic function parameter boundary classification".to_string(),
        });
    }
    assert_total_boundary_facts(
        &raw.name,
        raw.params.len(),
        &boundary_decisions(&raw.decisions),
    );
}

fn mark_param_place(
    place: Place,
    param_tys: &[ResolvedTy],
    facts: &[ParamBoundaryFact],
    state: RepresentationEffectState,
    effects: &mut [RepresentationEffectState],
) {
    let Some(local) = crate::dataflow::write_place_local(place) else {
        return;
    };
    let Ok(param_index) = usize::try_from(local) else {
        return;
    };
    let Some(param_ty) = param_tys.get(param_index) else {
        return;
    };
    // The representation-loan ABI exists solely for inline `bytes` triples.
    // Every other interior write through a checker-proven caller-visible
    // projection has no admitted representation-mutation contract, so it must
    // fail closed instead of silently retaining a read-only boundary.
    effects[param_index] = if matches!(param_ty, ResolvedTy::Bytes) {
        effects[param_index].max(state)
    } else if facts
        .get(param_index)
        .is_some_and(|fact| fact.caller_visible_projection)
    {
        RepresentationEffectState::Unproven
    } else {
        effects[param_index]
    };
}

fn mark_unproven_param_place(
    place: Place,
    facts: &[ParamBoundaryFact],
    effects: &mut [RepresentationEffectState],
) {
    let Some(local) = crate::dataflow::write_place_local(place) else {
        return;
    };
    let Ok(param_index) = usize::try_from(local) else {
        return;
    };
    if facts
        .get(param_index)
        .is_some_and(|fact| fact.caller_visible_projection)
    {
        effects[param_index] = RepresentationEffectState::Unproven;
    }
}

type RepresentationEffectEdge = (usize, usize, usize, usize);

/// Whether one checker-authorized standard-library extern argument has an
/// audited contract that proves it is borrowed only. These calls do not
/// replace a caller parameter's representation, even though source-stdlib FFI
/// shims have no raw MIR body to appear in `function_index`.
///
/// This answers only the representation-effect question.  It is deliberately
/// a positive, argument-indexed query over the concrete emitted ABI symbol
/// (for example `hew_metric_counter_register` or `hew_tcp_listen`), but is
/// reachable only through [`crate::CallAuthority::Extern`]. The generated FFI
/// table is the authority for checker-proven source externs; a `Direct` call
/// cannot acquire it by reusing a linker spelling. Unknown, consuming, and
/// escaping arguments keep the normal fail-closed `Unproven` result below.
fn checked_std_extern_borrows_argument(callee: &str, arg_index: usize) -> bool {
    use crate::ffi_contracts::ExternParamOwnership;

    if let Some(contract) = crate::ffi_contracts::extern_ownership_contract(callee).contract() {
        return contract.params.get(arg_index) == Some(&ExternParamOwnership::Borrow);
    }
    crate::runtime_symbols::callee_ownership_contract(callee).borrows_string_call_args()
}

fn scan_function_representation_effects(
    function: usize,
    raw: &RawMirFunction,
    facts: &[ParamBoundaryFact],
    function_index: &HashMap<String, usize>,
    effects: &mut [Vec<RepresentationEffectState>],
    edges: &mut Vec<RepresentationEffectEdge>,
) {
    let param_count = raw.params.len();
    for block in &raw.blocks {
        for instr in &block.instructions {
            for place in crate::dataflow::instr_interior_write_places(instr) {
                // A record parameter is materialised as the callee's by-value
                // aggregate snapshot. `var self` iterator adapters update that
                // snapshot with `RecordFieldStore`; this is ordinary local
                // state evolution, not a write through shared caller storage.
                // Other interior operations can drain or neutralise projected
                // ownership and therefore retain the fail-closed path above.
                if matches!(instr, Instr::RecordFieldStore { record, .. } if *record == place) {
                    continue;
                }
                mark_param_place(
                    place,
                    &raw.params,
                    facts,
                    RepresentationEffectState::MayReplace,
                    &mut effects[function],
                );
            }
            match instr {
                Instr::CallClosure { args, .. } => {
                    for &arg in args {
                        mark_unproven_param_place(arg, facts, &mut effects[function]);
                    }
                }
                Instr::CallTraitMethod {
                    fat_pointer, args, ..
                } => {
                    mark_unproven_param_place(*fat_pointer, facts, &mut effects[function]);
                    for &arg in args {
                        mark_unproven_param_place(arg, facts, &mut effects[function]);
                    }
                }
                _ => {}
            }
        }

        if let Terminator::Call {
            callee,
            authority,
            args,
            ..
        } = &block.terminator
        {
            if let Some(&callee_index) = function_index.get(callee) {
                for (callee_param, &arg) in args.iter().enumerate() {
                    let Some(local) = crate::dataflow::write_place_local(arg) else {
                        continue;
                    };
                    let Ok(caller_param) = usize::try_from(local) else {
                        continue;
                    };
                    if caller_param >= param_count {
                        continue;
                    }
                    if callee_param < effects[callee_index].len() {
                        edges.push((function, caller_param, callee_index, callee_param));
                    } else if facts[caller_param].caller_visible_projection {
                        effects[function][caller_param] = RepresentationEffectState::Unproven;
                    }
                }
            } else if matches!(authority, crate::CallAuthority::Extern) {
                for (arg_index, &arg) in args.iter().enumerate() {
                    if !checked_std_extern_borrows_argument(callee, arg_index) {
                        mark_unproven_param_place(arg, facts, &mut effects[function]);
                    }
                }
            } else if matches!(authority, crate::CallAuthority::Direct) {
                // A direct call may be a user function, an opaque host extern,
                // or a malformed fixture. Its linker spelling is deliberately
                // not authority to read the generated FFI table.
                for &arg in args {
                    mark_unproven_param_place(arg, facts, &mut effects[function]);
                }
            }
        }
    }

    for suspend in raw.suspend_kinds.values() {
        if let SuspendKind::CallClosure { args, .. } = suspend {
            for &arg in args {
                mark_unproven_param_place(arg, facts, &mut effects[function]);
            }
        }
    }
}

fn collect_param_representation_effects(
    raw_mir: &[RawMirFunction],
) -> Vec<Vec<RepresentationEffectState>> {
    let function_index = raw_mir
        .iter()
        .enumerate()
        .map(|(index, raw)| (raw.name.clone(), index))
        .collect::<HashMap<_, _>>();
    let mut effects = raw_mir
        .iter()
        .map(|raw| vec![RepresentationEffectState::None; raw.params.len()])
        .collect::<Vec<_>>();
    let mut edges = Vec::<RepresentationEffectEdge>::new();
    for (function, raw) in raw_mir.iter().enumerate() {
        let facts = boundary_decisions(&raw.decisions);
        scan_function_representation_effects(
            function,
            raw,
            &facts,
            &function_index,
            &mut effects,
            &mut edges,
        );
    }

    loop {
        let mut changed = false;
        for &(caller, caller_param, callee, callee_param) in &edges {
            let propagated = effects[callee][callee_param];
            if propagated > effects[caller][caller_param] {
                effects[caller][caller_param] = propagated;
                changed = true;
            }
        }
        if !changed {
            return effects;
        }
    }
}

fn refine_raw_param_boundary_modes(
    raw_mir: &mut [RawMirFunction],
    effects: &[Vec<RepresentationEffectState>],
) {
    for (function, raw) in raw_mir.iter_mut().enumerate() {
        let coroutine = raw.coroutine_facts().is_coroutine;
        let whole_written = (0..raw.params.len())
            .map(|index| {
                crate::dataflow::local_is_written_in_body(
                    raw,
                    u32::try_from(index).expect("function parameter count exceeds u32::MAX"),
                )
            })
            .collect::<Vec<_>>();
        for decision in &mut raw.decisions {
            let Strategy::ParamBoundary(mut fact) = decision.strategy else {
                continue;
            };
            let param_index =
                usize::try_from(fact.param_index).expect("parameter boundary index exceeds usize");
            if matches!(fact.mode, ParamBoundaryMode::BorrowReadOnly) {
                fact.mode = match effects[function][param_index] {
                    RepresentationEffectState::None => ParamBoundaryMode::BorrowReadOnly,
                    RepresentationEffectState::MayReplace
                        if fact.caller_visible_projection
                            && raw.call_conv == FunctionCallConv::Default
                            && !coroutine
                            && !whole_written[param_index]
                            && matches!(raw.params[param_index], ResolvedTy::Bytes) =>
                    {
                        ParamBoundaryMode::BorrowRepresentationLoan {
                            storage: ParamLoanStorage::Aliasable,
                            effect: ParamRepresentationEffect::MayReplaceRepresentation,
                            crash_cleanup: ParamCrashCleanupKind::Bytes,
                        }
                    }
                    RepresentationEffectState::MayReplace | RepresentationEffectState::Unproven => {
                        ParamBoundaryMode::RejectUnprovenRepresentationMutation
                    }
                };
                decision.strategy = Strategy::ParamBoundary(fact);
            }
        }
        assert_total_boundary_facts(
            &raw.name,
            raw.params.len(),
            &boundary_decisions(&raw.decisions),
        );
    }
}

fn sync_param_boundary_modes(
    raw_mir: &[RawMirFunction],
    checked_mir: &mut [CheckedMirFunction],
    elaborated_mir: &mut [ElaboratedMirFunction],
) {
    let boundary_decisions_for = |name: &str| {
        raw_mir
            .iter()
            .find(|raw| raw.name == name)
            .expect("MIR function has no raw parameter-boundary authority")
            .decisions
            .iter()
            .filter(|decision| matches!(decision.strategy, Strategy::ParamBoundary(_)))
            .cloned()
            .collect::<Vec<_>>()
    };
    for checked in checked_mir {
        checked
            .decisions
            .retain(|decision| !matches!(decision.strategy, Strategy::ParamBoundary(_)));
        checked
            .decisions
            .extend(boundary_decisions_for(&checked.name));
    }
    for elaborated in elaborated_mir {
        elaborated
            .decisions
            .retain(|decision| !matches!(decision.strategy, Strategy::ParamBoundary(_)));
        elaborated
            .decisions
            .extend(boundary_decisions_for(&elaborated.name));
    }
}

/// Refine every initial parameter mode from exhaustive local MIR writers and
/// resolved call edges. This is a monotone module-wide fixpoint, so forwarded
/// calls and recursive SCCs converge without call-order dependence.
pub(super) fn finalize_param_boundary_modes(
    raw_mir: &mut [RawMirFunction],
    checked_mir: &mut [CheckedMirFunction],
    elaborated_mir: &mut [ElaboratedMirFunction],
) {
    for raw in raw_mir.iter_mut() {
        seed_missing_boundary_facts(raw);
    }
    let effects = collect_param_representation_effects(raw_mir);
    refine_raw_param_boundary_modes(raw_mir, &effects);
    sync_param_boundary_modes(raw_mir, checked_mir, elaborated_mir);
}

#[cfg(test)]
mod param_boundary_effect_tests {
    use super::*;
    use crate::model::RuntimeCall;

    fn boundary_decision(mode: ParamBoundaryMode) -> DecisionFact {
        DecisionFact {
            site: SiteId(0),
            ty: ResolvedTy::Bytes,
            value_class: ValueClass::CowValue,
            intent: IntentKind::Unknown,
            strategy: Strategy::ParamBoundary(ParamBoundaryFact {
                param_index: 0,
                param_count: 1,
                caller_visible_projection: true,
                mode,
            }),
            why: "test boundary".to_string(),
        }
    }

    fn raw_function(
        name: &str,
        call_conv: FunctionCallConv,
        instructions: Vec<Instr>,
        terminator: Terminator,
    ) -> RawMirFunction {
        RawMirFunction {
            name: name.to_string(),
            return_ty: ResolvedTy::Unit,
            call_conv,
            params: vec![ResolvedTy::Bytes],
            locals: vec![ResolvedTy::Bytes],
            local_names: vec![Some("value".to_string())],
            local_scopes: vec![None],
            local_decl_bytes: vec![None],
            scope_table: vec![],
            blocks: vec![BasicBlock {
                id: 0,
                statements: vec![],
                instructions,
                terminator,
            }],
            decisions: vec![boundary_decision(ParamBoundaryMode::BorrowReadOnly)],
            intrinsic_id: None,
            await_deadline_ns: HashMap::new(),
            suspend_kinds: HashMap::new(),
            lambda_actor_user_param_locals: vec![],
            span: None,
            instr_spans: std::collections::BTreeMap::new(),
            source_origin: SourceOrigin::Unknown,
        }
    }

    fn call(callee: &str) -> Terminator {
        call_with_args(callee, vec![Place::Local(0)])
    }

    fn call_with_args(callee: &str, args: Vec<Place>) -> Terminator {
        Terminator::Call {
            callee: callee.to_string(),
            authority: crate::model::CallAuthority::default(),
            args,
            dest: None,
            next: 0,
        }
    }

    fn bytes_writer() -> Instr {
        Instr::CallRuntimeAbi(
            RuntimeCall::new("hew_bytes_push", vec![Place::Local(0)], None)
                .expect("bytes push is an admitted runtime call"),
        )
    }

    fn checked(raw: &RawMirFunction) -> CheckedMirFunction {
        CheckedMirFunction {
            name: raw.name.clone(),
            return_ty: raw.return_ty.clone(),
            blocks: raw.blocks.clone(),
            decisions: raw.decisions.clone(),
            checks: vec![],
            cooperate_sites: vec![],
        }
    }

    fn elaborated(raw: &RawMirFunction) -> ElaboratedMirFunction {
        ElaboratedMirFunction {
            name: raw.name.clone(),
            return_ty: raw.return_ty.clone(),
            statements: vec![],
            decisions: raw.decisions.clone(),
            blocks: vec![],
            drop_plans: vec![],
            coroutine: None,
            lambda_captures: vec![],
        }
    }

    fn mode(raw: &RawMirFunction) -> ParamBoundaryMode {
        boundary_decisions(&raw.decisions)[0].mode
    }

    fn finalize(raw: &mut [RawMirFunction]) {
        let mut checked = raw.iter().map(checked).collect::<Vec<_>>();
        let mut elaborated = raw.iter().map(elaborated).collect::<Vec<_>>();
        finalize_param_boundary_modes(raw, &mut checked, &mut elaborated);
        for ((raw, checked), elaborated) in raw.iter().zip(&checked).zip(&elaborated) {
            assert_eq!(
                boundary_decisions(&raw.decisions),
                boundary_decisions(&checked.decisions)
            );
            assert_eq!(
                boundary_decisions(&raw.decisions),
                boundary_decisions(&elaborated.decisions)
            );
        }
    }

    #[test]
    fn representation_effect_propagates_through_forwarders_symbols_and_recursive_sccs() {
        let mut raw = vec![
            raw_function(
                "leaf",
                FunctionCallConv::Default,
                vec![bytes_writer()],
                Terminator::Return,
            ),
            raw_function("forward", FunctionCallConv::Default, vec![], call("leaf")),
            raw_function(
                "<Buffer>::touch",
                FunctionCallConv::Default,
                vec![],
                call("forward"),
            ),
            raw_function(
                "touch$bytes",
                FunctionCallConv::Default,
                vec![],
                call("<Buffer>::touch"),
            ),
            raw_function(
                "recursive_a",
                FunctionCallConv::Default,
                vec![],
                call("recursive_b"),
            ),
            raw_function(
                "recursive_b",
                FunctionCallConv::Default,
                vec![bytes_writer()],
                call("recursive_a"),
            ),
        ];
        finalize(&mut raw);

        for function in &raw {
            assert_eq!(
                mode(function),
                ParamBoundaryMode::BorrowRepresentationLoan {
                    storage: ParamLoanStorage::Aliasable,
                    effect: ParamRepresentationEffect::MayReplaceRepresentation,
                    crash_cleanup: ParamCrashCleanupKind::Bytes,
                },
                "{} must inherit the representation-replacement effect",
                function.name
            );
        }
    }

    #[test]
    fn adapter_state_field_store_is_not_a_bytes_representation_loan() {
        // `var self` adapters (Map/Filter/Take/Skip) update fields in their
        // callee-owned aggregate snapshot and return that state through the
        // ordinary var-self carrier. This must not be misclassified as the
        // special pointer-ABI mutation reserved for inline `bytes` values.
        let cursor = ResolvedTy::named_user("Cursor", Vec::new());
        let mut raw = vec![raw_function(
            "adapter_next",
            FunctionCallConv::Default,
            vec![Instr::RecordFieldStore {
                record: Place::Local(0),
                field_offset: crate::model::FieldOffset(0),
                src: Place::Local(1),
            }],
            Terminator::Return,
        )];
        raw[0].params = vec![cursor.clone()];
        raw[0].locals = vec![cursor.clone(), ResolvedTy::I64];
        raw[0].decisions[0].ty = cursor;

        finalize(&mut raw);

        assert_eq!(mode(&raw[0]), ParamBoundaryMode::BorrowReadOnly);
    }

    #[test]
    fn caller_visible_user_record_field_drain_fails_closed() {
        // A Holder value is passed by value, so an ordinary RecordFieldStore
        // updates only its callee-local snapshot (the adapter case above).
        // Draining an owned shared-handle field is different: without an
        // owned-carrier boundary it can retire storage still visible to the
        // caller. The representation pass must reject that unproven mutation,
        // not erase it merely because the outer parameter is not `bytes`.
        let holder = ResolvedTy::named_user("Holder", Vec::new());
        let shared_vec =
            ResolvedTy::named_builtin("Vec", hew_types::BuiltinType::Vec, vec![ResolvedTy::I64]);
        let mut raw = vec![raw_function(
            "drain_shared_field",
            FunctionCallConv::Default,
            vec![Instr::FieldDropInPlace {
                base: Place::Local(0),
                field: crate::model::FieldAddr::Record(crate::model::FieldOffset(0)),
                ty: shared_vec,
            }],
            Terminator::Return,
        )];
        raw[0].params = vec![holder.clone()];
        raw[0].locals = vec![holder.clone()];
        raw[0].decisions[0].ty = holder;

        finalize(&mut raw);

        assert_eq!(
            mode(&raw[0]),
            ParamBoundaryMode::RejectUnprovenRepresentationMutation,
            "a caller-visible shared-handle field drain needs explicit ownership authority"
        );
    }

    #[test]
    fn unresolved_and_copied_triple_mutation_forms_reject_explicitly() {
        let mut raw = vec![
            raw_function(
                "unresolved",
                FunctionCallConv::Default,
                vec![],
                call("unknown_callable"),
            ),
            raw_function(
                "closure_copy",
                FunctionCallConv::ClosureInvoke,
                vec![bytes_writer()],
                Terminator::Return,
            ),
        ];
        finalize(&mut raw);

        for function in &raw {
            assert_eq!(
                mode(function),
                ParamBoundaryMode::RejectUnprovenRepresentationMutation,
                "{} must fail closed instead of becoming a read-only borrow",
                function.name
            );
        }
    }

    #[test]
    fn catalog_string_borrow_shim_keeps_visible_parameter_read_only() {
        // The source builtin `len(s)` reaches raw MIR through the catalog row
        // `len_str`, whose ItemId-backed HIR join projects the concrete ABI
        // symbol `hew_string_length`.  It has no raw body, so the
        // representation-effect scan must consult the audited borrowing
        // contract instead of treating it like an arbitrary external call.
        let mut raw = vec![raw_function(
            "echo_len",
            FunctionCallConv::Default,
            vec![],
            call("hew_string_length"),
        )];
        raw[0].params = vec![ResolvedTy::String];
        raw[0].locals = vec![ResolvedTy::String];
        raw[0].decisions[0].ty = ResolvedTy::String;
        let Terminator::Call { authority, .. } = &mut raw[0].blocks[0].terminator else {
            unreachable!("test helper constructs a direct call terminator");
        };
        *authority = crate::CallAuthority::Extern;

        finalize(&mut raw);

        assert_eq!(
            mode(&raw[0]),
            ParamBoundaryMode::BorrowReadOnly,
            "an audited catalog string borrow must not manufacture \
             representation-mutation authority"
        );
    }

    #[test]
    fn direct_bytes_abi_contracts_distinguish_borrow_consume_and_absence() {
        // The representation scan sees concrete emitted ABI symbols. A
        // runtime receiver-borrow may keep a caller-visible Bytes projection
        // read-only, while both a generated consuming FFI row and an absent
        // row remain fail-closed. This guards the per-argument authority
        // boundary rather than any particular stdlib wrapper spelling.
        let mut raw = vec![
            raw_function(
                "bytes_borrow",
                FunctionCallConv::Default,
                vec![],
                call("hew_vec_len"),
            ),
            raw_function(
                "generated_consume",
                FunctionCallConv::Default,
                vec![],
                call("hew_tcp_listener_close"),
            ),
            raw_function(
                "absent_contract",
                FunctionCallConv::Default,
                vec![],
                call("unclassified_bytes_abi"),
            ),
        ];
        let Terminator::Call { authority, .. } = &mut raw[0].blocks[0].terminator else {
            unreachable!("test helper constructs a direct call terminator");
        };
        // The first is the real catalog family. Runtime families carry their
        // ownership semantics independently of this direct-extern
        // representation scan.
        *authority =
            crate::CallAuthority::Runtime(hew_types::runtime_call::RuntimeCallFamily::VecLen);

        finalize(&mut raw);

        assert_eq!(
            raw.iter().map(mode).collect::<Vec<_>>(),
            vec![
                ParamBoundaryMode::BorrowReadOnly,
                ParamBoundaryMode::RejectUnprovenRepresentationMutation,
                ParamBoundaryMode::RejectUnprovenRepresentationMutation,
            ],
            "only an exact per-parameter borrow authority may retain a caller-visible Bytes projection"
        );
    }

    #[test]
    fn vec_receiver_borrow_contract_covers_all_element_carriers() {
        // Direct stdlib algorithms reach `hew_vec_len` through a catalog
        // `Terminator::Call`, regardless of whether their Vec contains plain
        // values or strings. The receiver contract is independent of the
        // element carrier, so both retain their caller-visible read-only
        // boundary instead of being rejected as an unknown representation
        // mutation.
        let mut raw = [ResolvedTy::I64, ResolvedTy::String]
            .into_iter()
            .map(|element_ty| {
                let vec_ty = ResolvedTy::Named {
                    name: "Vec".to_string(),
                    args: vec![element_ty],
                    builtin: Some(hew_types::BuiltinType::Vec),
                    is_opaque: false,
                };
                let mut function = raw_function(
                    "vec_len_reader",
                    FunctionCallConv::Default,
                    vec![],
                    call("hew_vec_len"),
                );
                function.params = vec![vec_ty.clone()];
                function.locals = vec![vec_ty.clone()];
                function.decisions[0].ty = vec_ty;
                let Terminator::Call { authority, .. } = &mut function.blocks[0].terminator else {
                    unreachable!("test helper constructs a direct call terminator");
                };
                *authority = crate::CallAuthority::Runtime(
                    hew_types::runtime_call::RuntimeCallFamily::VecLen,
                );
                function
            })
            .collect::<Vec<_>>();

        finalize(&mut raw);

        assert!(
            raw.iter()
                .all(|function| mode(function) == ParamBoundaryMode::BorrowReadOnly),
            "the exact Vec receiver borrow contract must be element-agnostic: {raw:#?}"
        );
    }

    #[test]
    fn checker_authorized_std_extern_contract_keeps_network_address_read_only() {
        // Source stdlib wrappers call their declared externs directly, rather
        // than through a catalog presentation name. The generated FFI contract
        // is therefore the proof that `listen` and `connect_timeout` read the
        // caller's address string without replacing its representation.
        let mut raw = vec![
            raw_function(
                "listen",
                FunctionCallConv::Default,
                vec![],
                call("hew_tcp_listen"),
            ),
            raw_function(
                "connect_timeout",
                FunctionCallConv::Default,
                vec![],
                call_with_args(
                    "hew_tcp_connect_timeout",
                    vec![Place::Local(0), Place::Local(0), Place::Local(0)],
                ),
            ),
        ];
        for function in &mut raw {
            function.params = vec![ResolvedTy::String];
            function.locals = vec![ResolvedTy::String];
            function.decisions[0].ty = ResolvedTy::String;
            let Terminator::Call { authority, .. } = &mut function.blocks[0].terminator else {
                unreachable!("test helper constructs a direct call terminator");
            };
            *authority = crate::CallAuthority::Extern;
        }

        finalize(&mut raw);

        assert_eq!(
            raw.iter().map(mode).collect::<Vec<_>>(),
            vec![
                ParamBoundaryMode::BorrowReadOnly,
                ParamBoundaryMode::BorrowReadOnly,
            ],
            "checker-authorized std extern borrows must not manufacture representation-mutation authority"
        );
    }

    #[test]
    fn same_symbol_user_extern_cannot_claim_std_ffi_borrow_contract() {
        // The linker spelling is intentionally identical. Only the
        // checker/HIR-projected `Extern` authority may consult the generated
        // ownership row; a direct user declaration stays fail-closed.
        let mut raw = vec![
            raw_function(
                "compiled_std_metrics",
                FunctionCallConv::Default,
                vec![],
                call("hew_metric_counter_register"),
            ),
            raw_function(
                "user_same_symbol",
                FunctionCallConv::Default,
                vec![],
                call("hew_metric_counter_register"),
            ),
        ];
        for function in &mut raw {
            function.params = vec![ResolvedTy::String];
            function.locals = vec![ResolvedTy::String];
            function.decisions[0].ty = ResolvedTy::String;
        }
        let Terminator::Call { authority, .. } = &mut raw[0].blocks[0].terminator else {
            unreachable!("test helper constructs a direct call terminator");
        };
        *authority = crate::CallAuthority::Extern;

        finalize(&mut raw);

        assert_eq!(mode(&raw[0]), ParamBoundaryMode::BorrowReadOnly);
        assert_eq!(
            mode(&raw[1]),
            ParamBoundaryMode::RejectUnprovenRepresentationMutation,
            "a user extern must not acquire a standard-library representation contract by symbol collision"
        );
    }

    #[test]
    fn owned_resource_message_and_carrier_modes_remain_distinct() {
        let expected = [
            ParamBoundaryMode::TransferResource,
            ParamBoundaryMode::OwnedMessage,
            ParamBoundaryMode::OwnedCarrier,
        ];
        let mut raw = expected
            .iter()
            .enumerate()
            .map(|(index, &mode)| {
                let mut function = raw_function(
                    &format!("owned_{index}"),
                    FunctionCallConv::Default,
                    vec![bytes_writer()],
                    call("unknown_callable"),
                );
                function.decisions = vec![boundary_decision(mode)];
                function
            })
            .collect::<Vec<_>>();
        finalize(&mut raw);

        assert_eq!(
            raw.iter().map(mode).collect::<Vec<_>>(),
            expected,
            "the representation-effect pass refines borrowed parameters only"
        );
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

/// True when a projection chain is rooted directly in parameter `b_p`.
/// Wrappers that compute a new value are deliberately excluded: only a place
/// projection can be neutralized by the callee-side carrier machinery.
fn projection_is_rooted_in(expr: &HirExpr, b_p: BindingId) -> bool {
    if is_binding_ref(expr, b_p) {
        return true;
    }
    match &expr.kind {
        HirExprKind::FieldAccess { object, .. } => projection_is_rooted_in(object, b_p),
        HirExprKind::TupleIndex { tuple, .. } => projection_is_rooted_in(tuple, b_p),
        HirExprKind::Index { container, .. } | HirExprKind::Slice { container, .. } => {
            projection_is_rooted_in(container, b_p)
        }
        _ => false,
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
        //    classified BORROW in `pc`; or
        //  * the callee resolves to a source-declared `extern` symbol AND its HIR argument
        //    already carries `Read`. For a resource-valued direct C-ABI call
        //    this is HIR's generated-table verdict: it is reached only after
        //    the exact symbol/index, qualified nominal, and declaring-module
        //    provenance have been validated. Ordinary Hew free-call resource
        //    arguments cannot forge this borrow path.
        // Every other arg form recurses (and a bare ref to a Consume/unresolved
        // target bottoms out at the leaf rule → consume). Invoking a callable
        // parameter, including through one of its place projections, only reads
        // the callable pair: `CallClosure` does not store or take ownership of
        // its environment. Wrappers that compute a new callee remain outside
        // this place-root proof and recurse fail-closed.
        HirExprKind::Call { callee, args, .. } => {
            let borrows_callable_param = matches!(
                &callee.ty,
                ResolvedTy::Function { .. } | ResolvedTy::Closure { .. }
            ) && callee.intent == IntentKind::Read
                && projection_is_rooted_in(callee, b_p);
            if !borrows_callable_param && scan_expr_for_consume(callee, b_p, pc) {
                return true;
            }
            let callee_item = callee_item_id(callee);
            let is_method = callee_item.is_some_and(|id| pc.methods.contains(&id));
            for (j, arg) in args.iter().enumerate() {
                if is_binding_ref(arg, b_p) {
                    let borrows = if is_method {
                        // Carrier inference needs positive ownership evidence,
                        // not the fail-closed move intent stamped on a method
                        // receiver. Receiver slot zero retains the method's
                        // established borrowed-self authority (Arena and the
                        // declarative string/collection FFI surface).
                        (pc.owned_projection_sinks
                            && j == 0
                            && callee_item.is_some_and(|id| pc.true_receiver_methods.contains(&id)))
                            || arg.intent == IntentKind::Read
                    } else if callee_item.is_some()
                        && matches!(
                            &callee.kind,
                            HirExprKind::BindingRef { name, .. }
                                if pc.extern_fn_names.contains(name)
                        )
                        && arg.intent == IntentKind::Read
                    {
                        // A direct C-ABI resource borrow was proved by HIR
                        // from the sole generated FFI contract table. Preserve
                        // that fact in the callee's resource-parameter summary,
                        // or a stdlib wrapper such as Connection::write_string
                        // would incorrectly acquire and close its caller's
                        // receiver at wrapper return.
                        true
                    } else if pc.assume_forward_borrows {
                        // Proven-only differential pass: treat every free-fn
                        // forward as a borrow so the scan reports ONLY the
                        // positively-proven escapes. Suppresses exactly the
                        // fail-closed forward-to-unproven/consuming disjunct
                        // that distinguishes `ConservativeConsume` from
                        // `ProvenConsume`.
                        true
                    } else {
                        let target = callee_item.and_then(|id| pc.consume.get(&(id, j))).copied();
                        if pc.owned_projection_sinks {
                            // An unresolved/extern target supplies no proof
                            // that it stores the argument. Keep carrier facts
                            // monotone from explicit sinks and already-proven
                            // carrier callees only; the ordinary consume table
                            // remains fail-closed for move checking.
                            target != Some(true)
                        } else {
                            target == Some(false)
                        }
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
        // Owned-projection sinks require the projected RESULT to own heap: a
        // primitive-scalar extraction (`v[i]` on `Vec<i64>`, `p.fd`, `t.0` of
        // `i64`) copies bits and carries no ownership out of the container, so
        // it is vacuous carrier evidence even when the HIR stamps the
        // extraction `Consume`. Classifying the container param as an owned
        // carrier on scalar reads turns a shared-mutation borrow (`v.set`
        // through the param) into a callee-side clone, silently discarding
        // every mutation the caller expected to observe.
        HirExprKind::StructInit { fields, base, .. } => {
            fields.iter().any(|(_, v)| {
                (pc.owned_projection_sinks
                    && !is_binding_ref(v, b_p)
                    && !crate::return_provenance::ty_is_scalar_non_heap(&v.ty)
                    && projection_is_rooted_in(v, b_p))
                    || scan_expr_for_consume(v, b_p, pc)
            }) || base.as_deref().is_some_and(|b| {
                (pc.owned_projection_sinks
                    && !is_binding_ref(b, b_p)
                    && !crate::return_provenance::ty_is_scalar_non_heap(&b.ty)
                    && projection_is_rooted_in(b, b_p))
                    || scan_expr_for_consume(b, b_p, pc)
            })
        }
        HirExprKind::FieldAccess { object, .. } => {
            (pc.owned_projection_sinks
                && expr.intent == IntentKind::Consume
                && !crate::return_provenance::ty_is_scalar_non_heap(&expr.ty)
                && projection_is_rooted_in(object, b_p))
                || projection_base_consumes(object, b_p, pc)
        }
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
        HirExprKind::TupleIndex { tuple, .. } => {
            (pc.owned_projection_sinks
                && expr.intent == IntentKind::Consume
                && !crate::return_provenance::ty_is_scalar_non_heap(&expr.ty)
                && projection_is_rooted_in(tuple, b_p))
                || projection_base_consumes(tuple, b_p, pc)
        }
        // Handle-based collections (`Vec`, `HashMap`, `string`) hand an owned
        // element OUT as a NEW `+1` owner (`hew_vec_get_clone` /
        // `hew_vec_get_str` / `hew_hashmap_get_clone_layout` — the
        // `PROVED_OWNER_METHOD_SYMBOLS` contract): the container keeps its
        // element and its release authority, so an index extraction is NEVER
        // ownership evidence against the container param. Only an INLINE
        // aggregate container (a fixed array, whose element load is a
        // byte-copy alias like a record field) can carry ownership out
        // through an index, and then only for a heap-owning element.
        HirExprKind::Index { container, index } => {
            (pc.owned_projection_sinks
                && expr.intent == IntentKind::Consume
                && matches!(container.ty, ResolvedTy::Array(..))
                && !crate::return_provenance::ty_is_scalar_non_heap(&expr.ty)
                && projection_is_rooted_in(container, b_p))
                || projection_base_consumes(container, b_p, pc)
                || scan_expr_for_consume(index, b_p, pc)
        }
        HirExprKind::Slice {
            container,
            start,
            end,
            ..
        } => {
            (pc.owned_projection_sinks
                && expr.intent == IntentKind::Consume
                && projection_is_rooted_in(container, b_p))
                || projection_base_consumes(container, b_p, pc)
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
        // Rc/Weak intrinsic. Every op reads its RECEIVER handle through a
        // borrow: `clone`/`downgrade`/`weak_clone` mint a NEW independently
        // counted handle, the count/uniqueness probes read the header, and
        // `set`/`get` reach the payload through the still-caller-owned cell.
        // A bare-ref receiver must therefore be intercepted as a borrow slot
        // (mirroring the `Read`-intent method receiver rule) instead of
        // falling to the leaf consume rule — otherwise every by-value Rc/Weak
        // parameter whose methods are called is misclassified as an owning
        // sink and admitted as a call carrier. The VALUE operand (`Rc::new`,
        // `set`) IS stored into the cell and keeps the consume scan.
        HirExprKind::RcIntrinsic {
            receiver, value, ..
        } => {
            receiver.as_deref().is_some_and(|expr| {
                if is_binding_ref(expr, b_p) {
                    false
                } else {
                    scan_expr_for_consume(expr, b_p, pc)
                }
            }) || value
                .as_deref()
                .is_some_and(|expr| scan_expr_for_consume(expr, b_p, pc))
        }
        HirExprKind::ChannelRecvAwait { receiver, .. }
        | HirExprKind::CancellationTokenIsCancelled { receiver }
        | HirExprKind::GeneratorNext { receiver, .. }
        | HirExprKind::MachineStateName { receiver, .. }
        | HirExprKind::RecordCloneCall { src: receiver, .. }
        | HirExprKind::SubsumedValue {
            source: receiver, ..
        } => scan_expr_for_consume(receiver, b_p, pc),
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
            let scrutinee_consumes = if is_binding_ref(scrutinee, b_p) {
                !matches!(
                    project_match_ownership_mode(arms),
                    ProjectMatchOwnershipMode::Borrow
                )
            } else {
                scan_expr_for_consume(scrutinee, b_p, pc)
            };
            scrutinee_consumes
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
        HirExprKind::Call { callee, args, .. } => {
            if let Some(id) = callee_item_id(callee) {
                if !pc.methods.contains(&id) {
                    for (j, arg) in args.iter().enumerate() {
                        if pc.consume.get(&(id, j)).copied() == Some(false) {
                            out.insert(arg.site);
                        }
                    }
                } else if pc.receiver_methods.contains(&id) {
                    // Shape A of #2753: a value-receiver non-consuming method
                    // (`p.touch()`) lowers to `Call { callee: Item(m), args:
                    // [recv, ..] }`. The receiver (arg 0) is a BORROW — the
                    // callee reads `self` and releases nothing — so record its
                    // `SiteId` and the caller keeps the receiver record's
                    // scope-exit drop, converging method dispatch with the
                    // free-fn positional borrow path on ONE drop authority.
                    //
                    // Gated identically to the free-fn arm on `pc.consume ==
                    // Some(false)` (the exactly-once boundary): a method that
                    // CONSUMES `self` — returns `self`, moves `self.field` out,
                    // stores/sends/captures/forwards it — is flipped to CONSUME
                    // by the `compute_call_param_consumption` fixpoint, so its
                    // receiver is absent here → not recorded → the callee owns
                    // and drops it (mutually exclusive). Because a non-resource
                    // composite receiver is absent from the resource-only
                    // `param_consume` map, this fires ONLY in the
                    // `proven_borrow_arg_sites` walk (which passes
                    // `call_param_consume`), never in the intent-downgrade
                    // `borrow_arg_sites` walk. Only a whole owned local / fresh
                    // producer receiver is admitted (a projection/alias fails
                    // closed to leak-not-double-free). The method's non-receiver
                    // args stay unrecorded — the borrow-default surface is the
                    // receiver only.
                    if let Some(receiver) = args.first() {
                        if pc.consume.get(&(id, 0)).copied() == Some(false)
                            && receiver_is_whole_owned_operand(receiver)
                        {
                            out.insert(receiver.site);
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
        HirExprKind::RcIntrinsic {
            receiver, value, ..
        } => {
            if let Some(receiver) = receiver {
                go!(receiver);
            }
            if let Some(value) = value {
                go!(value);
            }
        }
        HirExprKind::ChannelRecvAwait { receiver, .. }
        | HirExprKind::CancellationTokenIsCancelled { receiver }
        | HirExprKind::GeneratorNext { receiver, .. }
        | HirExprKind::MachineStateName { receiver, .. }
        | HirExprKind::RecordCloneCall { src: receiver, .. }
        | HirExprKind::SubsumedValue {
            source: receiver, ..
        } => go!(receiver),
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
/// Fresh-owner see-through (fix (i)): a helper that tail-returns a
/// single-assignment `let`-bound local — the `fn make() { let x = Inner{..}; x }`
/// idiom AND the `[..]` array-literal desugar (`{ let __a = Vec::new();
/// __a.push(e); __a }`) — IS proven fresh, because the shared
/// [`crate::return_provenance::return_alias_bits`] walk now sees THROUGH such a
/// tail to its initializer plus interior appends. The see-through stays
/// fail-closed: a `var`, a reassignment, a param root (`let x = h; x`
/// re-derives the param leaf), or any other use of the local that could inject
/// an unmeasured alias keeps the `OPAQUE` leaf and the function non-fresh.
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
        .all(|e| !return_value_may_alias_borrow(e, &f.body, fresh))
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
///
/// # This proof is not a release licence
///
/// It cannot see an extern at all: a declared `extern "C"` fn is body-less, so
/// this walk's own `unwrap_or(true)` classifies it fresh and a Hew wrapper
/// around one inherits that verdict. That is precisely why NO ownership
/// consumer is ever handed this map. The veto of
/// [`crate::return_provenance::compute_fn_return_launders_opaque_extern`] is
/// conjoined into it ONCE, inside
/// [`crate::return_provenance::FreshOwnerVerdicts`], and that authority is the
/// only thing [`callee_returns_fresh_owner`] can be called with.
fn return_value_may_alias_borrow(
    expr: &HirExpr,
    body: &hew_hir::HirBlock,
    fresh: &HashMap<hew_hir::ItemId, bool>,
) -> bool {
    crate::return_provenance::coarse_may_alias_borrow_in_body(expr, body, fresh)
}
/// Resolve a `Call` callee to its freshness fact, read from the SINGLE
/// table-aware authority ([`FreshOwnerVerdicts`]): `true` only when `callee`
/// resolves to a function body this module's freshness fixpoint actually
/// ANALYZED, proved fresh, AND proved free of any ownership-opaque extern on
/// every return path.
///
/// - A declared `extern "C"` callee with no audited fresh-owner return contract
///   is vetoed FIRST, by NAME. Its call site's `ResolvedRef::Item` carries a
///   placeholder id, so no id lookup can catch it — an id lookup would also
///   COLLIDE with the module-fn summary space.
/// - A statically-resolved item callee (`BindingRef { resolved: Item(id) }`)
///   reads its authority row: the coarse interprocedural verdict CONJOINED with
///   the opaque-extern laundering veto. A Hew function that forwards a by-value
///   parameter is `false` (the coarse fixpoint); a Hew wrapper that launders an
///   opaque extern's return is `false` too (the taint fixpoint), transitively.
/// - A resolved item callee with NO row is an item this module never analysed —
///   a compiler-emitted body-less item, a cross-module item, or the placeholder
///   id of a declared extern. It answers `false`: not-analysed is not a
///   freshness proof, and the worst case of declining is a missed drop, never a
///   caller-side double release.
/// - Any other callee shape (an unresolved name, a value-typed fn pointer, an
///   indirect/closure call) is not statically resolvable and fails closed.
///
/// # The row must be TABLE-DERIVED, not merely present
///
/// Requiring a `true` row in the plain coarse summary is not enough, because
/// that summary is built before and independently of the extern contract table
/// and its own leaf policy launders a body-less extern into `Fresh`. A single
/// Hew frame is then enough to restore the forbidden caller drop:
///
/// ```hew
/// extern "C" { fn host_string() -> string; }
/// fn wrapper() -> string { unsafe { host_string() } }   // plain summary: true
/// ```
///
/// So the row read here comes from the single authority
/// [`crate::return_provenance::FreshOwnerVerdicts`], which is the coarse proof
/// CONJOINED with the veto of
/// [`crate::return_provenance::compute_fn_return_launders_opaque_extern`] — a
/// fixpoint that resolves every declared extern through the audited
/// [`ExternContractTable`]. That makes the answer transitive: a wrapper of a
/// wrapper, a generic wrapper and a recursive-looking wrapper all inherit the
/// `false` row.
///
/// Generic origins are included: a monomorphisation's callee resolves to the
/// generic ORIGIN `ItemId`, and the fixpoint is computed over `origin_fns`
/// (every `HirItem::Function`, generic or not), so a proven-fresh generic still
/// answers `true` here.
///
/// [`ExternContractTable`]: crate::return_provenance::ExternContractTable
pub(super) fn callee_returns_fresh_owner(
    callee: &HirExpr,
    verdicts: &crate::return_provenance::FreshOwnerVerdicts,
) -> bool {
    let HirExprKind::BindingRef { name, resolved } = &callee.kind else {
        return false;
    };
    let symbol = match resolved {
        ResolvedRef::Builtin(family) => family.c_symbol(),
        _ => name.as_str(),
    };
    if verdicts.symbol_is_ownership_opaque_extern(symbol) {
        return false;
    }
    let ResolvedRef::Item(item_id) = resolved else {
        return false;
    };
    verdicts.item_returns_fresh_owner(*item_id)
}

/// String-only sibling of [`callee_returns_fresh_owner`]. A retained field
/// projection aliases its source pointer but owns one independent refcount
/// share, so this query may license one string drop and nothing stronger.
pub(super) fn callee_returns_retained_string_owner(
    callee: &HirExpr,
    verdicts: &crate::return_provenance::FreshOwnerVerdicts,
) -> bool {
    let HirExprKind::BindingRef { name, resolved } = &callee.kind else {
        return false;
    };
    let symbol = match resolved {
        ResolvedRef::Builtin(family) => family.c_symbol(),
        _ => name.as_str(),
    };
    if verdicts.symbol_is_ownership_opaque_extern(symbol) {
        return false;
    }
    let ResolvedRef::Item(item_id) = resolved else {
        return false;
    };
    verdicts.item_returns_retained_string_owner(*item_id)
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
        HirExprKind::Call { callee, args, .. } | HirExprKind::SpawnedCall { callee, args, .. } => {
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
        HirExprKind::RcIntrinsic {
            receiver, value, ..
        } => {
            if let Some(receiver) = receiver {
                collect_return_values_in_expr(receiver, out);
            }
            if let Some(value) = value {
                collect_return_values_in_expr(value, out);
            }
        }
        HirExprKind::ChannelRecvAwait { receiver, .. }
        | HirExprKind::CancellationTokenIsCancelled { receiver }
        | HirExprKind::GeneratorNext { receiver, .. }
        | HirExprKind::MachineStateName { receiver, .. }
        | HirExprKind::RecordCloneCall { src: receiver, .. }
        | HirExprKind::SubsumedValue {
            source: receiver, ..
        } => {
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
        if component.builtin.is_some() || is_codegen_ready_user_component(&component, readiness) {
            continue;
        }
        push_unknown_type_diagnostic(component.name, reported, diagnostics);
    }
}
/// Build the canonical generic record/enum layout key.  This compatibility
/// wrapper keeps existing MIR call sites on the one HIR-owned authority: the
/// owner and every nominal type argument remain fully qualified, while dotted
/// owners are encoded once for native-safe registry symbols.
pub(crate) fn mangle_layout_key(name: &str, args: &[ResolvedTy]) -> String {
    hew_hir::mangle_layout_key(name, args)
}

/// Exact nominal layout key for a resolved named type.  Monomorphic names
/// retain their full dotted owner; generic applications use the shared HIR
/// mangler over that complete owner and argument spine.
pub(crate) fn named_layout_key(name: &str, args: &[ResolvedTy]) -> String {
    if args.is_empty() {
        name.to_string()
    } else {
        mangle_layout_key(name, args)
    }
}

pub(super) fn machine_layout_ty_matches(layout_names: &HashSet<String>, ty: &ResolvedTy) -> bool {
    match ty {
        ResolvedTy::Named { name, args, .. } => {
            let enum_key = named_layout_key(name, args);
            layout_names.contains(&enum_key)
                || layout_names.contains(&hew_hir::machine_layout_key(name, args))
        }
        _ => false,
    }
}
/// Resolve the `record_field_orders` key for a user record type — the key MIR
/// and codegen must agree on so the value-class admit, the drop-plan validator,
/// and the synthesised `__hew_record_{clone,drop}_inplace_<R>` thunk all name
/// the same layout.
///
/// For a generic INSTANTIATION the declaration owner and every type argument
/// remain canonical.  The owner is encoded into the native-safe `$` spelling
/// before mangling, matching HIR's `RecordLayoutRegistry`; stripping it would
/// collapse `left.render.Box<T>` and `right.render.Box<T>` into one layout.
///
/// The bare-name MONOMORPHIC arm keeps the FULL qualified name: imported
/// monomorphic records register under the bare name but `lookup_record_field_
/// order` already strips the prefix on a miss, so the full key resolves there
/// while preserving the legacy behaviour every monomorphic caller depends on.
pub(super) fn user_record_layout_key(ty: &ResolvedTy) -> Option<String> {
    match ty {
        ResolvedTy::Named {
            args,
            builtin:
                Some(
                    builtin @ (hew_types::BuiltinType::VecIter
                    | hew_types::BuiltinType::HashMapIter),
                ),
            ..
        } => hew_hir::synthetic_cursor_layout_key(*builtin, args),
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
        } => Some(mangle_layout_key(name, args)),
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
    match ty {
        ResolvedTy::Named {
            args,
            builtin: Some(hew_types::BuiltinType::VecIter),
            ..
        } if args.len() == 1 => {
            hew_hir::synthetic_cursor_layout_key(hew_types::BuiltinType::VecIter, args)
        }
        _ => None,
    }
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
        if component.builtin.is_some() || is_codegen_ready_user_component(&component, &readiness) {
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
#[derive(Debug, Clone)]
struct CodegenNamedComponent {
    name: String,
    args: Vec<ResolvedTy>,
    builtin: Option<BuiltinType>,
}

fn codegen_relevant_named_components(ty: &ResolvedTy) -> Vec<CodegenNamedComponent> {
    let mut components = Vec::new();
    collect_codegen_relevant_components(ty, &mut components);
    components
}
fn collect_codegen_relevant_components(
    ty: &ResolvedTy,
    components: &mut Vec<CodegenNamedComponent>,
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
            components.push(CodegenNamedComponent {
                name: name.clone(),
                args: args.clone(),
                builtin: *builtin,
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
fn is_codegen_ready_user_component(
    component: &CodegenNamedComponent,
    readiness: &LayoutReadiness,
) -> bool {
    let name = component.name.as_str();
    let layout_key = named_layout_key(name, &component.args);
    // `#[opaque]` runtime handles are registered in `type_classes` but carry no
    // record-field-order entry — they lower to `ptr`. Fieldless handles may be
    // `BitCopy` (borrowed/id handles) or `Resource` (owned handles with
    // `close()` cleanup); both are codegen-ready without a structural record
    // layout. Fielded marker types already have a `record_field_orders` entry and
    // are accepted by the check below.
    let marker = readiness
        .type_classes
        .get(&layout_key)
        .map(|(marker, _)| *marker)
        .or_else(|| hew_hir::lookup_type_marker(name, readiness.type_classes));
    if matches!(
        marker,
        Some(ResourceMarker::BitCopy | ResourceMarker::Resource)
    ) {
        return true;
    }
    readiness.record_field_orders.contains_key(&layout_key)
        || readiness.actor_layouts.contains_key(name)
        || readiness.supervisor_layout_map.contains_key(name)
        || readiness.machine_layout_names.contains(&layout_key)
        || readiness
            .machine_layout_names
            .contains(&hew_hir::machine_layout_key(name, &component.args))
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
        "hew_vec_set_owned_move",
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
        "hew_vec_set_owned_move",
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
        "hew_vec_join_str",
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
        "hew_vec_join_str",
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
        assert_eq!(CLASSIFIER_SYMBOLS.len(), 169);
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

        assert_eq!(vec_receiver.len(), 92);
        assert_eq!(collection_receiver.len(), 19);
        assert_eq!(bytes_receiver.len(), 11);
        assert_eq!(string_use.len(), 29);
        assert_eq!(fresh_string.len(), 31);

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
    //! Structural guard: generic record field-store, field-read, and
    //! `StructInit` consumers preserve the checker's full nominal owner when
    //! forming their layout key. A leaf-name rewrite would collapse
    //! `left.render.Box<T>` and `right.render.Box<T>` before the layout table.

    /// The production (non-test) sources containing record layout-key consumers.
    ///
    /// Normalises CRLF→LF for deterministic source scanning. Test-only
    /// helpers may appear before later production items in these large module
    /// files, so truncating at the first `#[cfg(test)] mod` would silently omit
    /// real consumers. Scan the complete files: the forbidden short-name call
    /// is absent everywhere, while the positive full-owner calls are ordinary
    /// Rust expressions rather than strings manufactured by this guard.
    fn production_source() -> String {
        [include_str!("mod.rs"), include_str!("expr.rs")]
            .into_iter()
            .map(|src| src.replace("\r\n", "\n"))
            .collect::<Vec<_>>()
            .join("\n")
    }

    /// The field-store, field-read, and `StructInit` generic arms must pass the
    /// full outer name to the shared key helper. No production layout consumer
    /// may shorten a nominal owner before keying the layout registry.
    #[test]
    fn generic_record_layout_consumers_preserve_outer_owner() {
        let prod = production_source();
        assert!(
            !prod.contains("mangle_layout_key(short_name"),
            "layout keys must retain the full nominal owner, never `short_name(..)`"
        );
        assert!(
            prod.matches("mangle_layout_key(name, args)").count() >= 3,
            "field-store, field-read, and StructInit must pass their resolved outer name"
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
            produced_value_facts: HashMap::default(),
            diagnostic_source_modules: HashMap::default(),
            root_item_ids: std::collections::HashSet::new(),
            caller_visible_param_projections: std::collections::HashSet::new(),
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
            declaration: hew_types::DefId::new("Shape"),
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
        let shape_layouts: Vec<_> = pipeline
            .enum_layouts
            .iter()
            .filter(|layout| layout.name == "Shape")
            .collect();
        assert_eq!(shape_layouts.len(), 1, "expected one EnumLayout for Shape");
        let layout = shape_layouts[0];
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
            declaration: hew_types::DefId::new("Colour"),
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
        let colour_layouts: Vec<_> = pipeline
            .enum_layouts
            .iter()
            .filter(|layout| layout.name == "Colour")
            .collect();
        assert_eq!(
            colour_layouts.len(),
            1,
            "expected one EnumLayout for Colour"
        );
        assert_eq!(colour_layouts[0].variants.len(), 3);
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
    fn fieldless_layout_key_generic_uses_full_owner_key_without_leaf_fallback() {
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

        let payload = ResolvedTy::named_user("lmonobox.Box", vec![]);
        let registered_key =
            hew_hir::mangle_layout_key("left.render.Slot", std::slice::from_ref(&payload));
        let builder = Builder {
            enum_layouts: vec![fieldless_layout(registered_key.clone())],
            ..Builder::default()
        };
        let qualified = ResolvedTy::named_user("left.render.Slot", vec![payload.clone()]);
        assert_eq!(
            builder.fieldless_enum_layout_key(&qualified),
            Some(registered_key)
        );

        let same_leaf_other_owner = ResolvedTy::named_user("right.render.Slot", vec![payload]);
        assert_eq!(
            builder.fieldless_enum_layout_key(&same_leaf_other_owner),
            None,
            "a same-leaf enum from another module must not reuse left.render.Slot's layout"
        );

        let missing_mangled_builder = Builder {
            enum_layouts: vec![fieldless_layout("decoy.Slot".to_string())],
            ..Builder::default()
        };
        let missing_mangled_probe = ResolvedTy::named_user(
            "left.render.Slot",
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
            declaration: hew_types::DefId::new("Option"),
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
        // The pipeline may also register bundled source enums. Select the
        // declaration under test by its exact monomorphised layout key rather
        // than inferring user ownership from the absence of a builtin row.
        let option_layouts: Vec<_> = pipeline
            .enum_layouts
            .iter()
            .filter(|layout| layout.name == "Option$$i64")
            .collect();
        assert_eq!(
            option_layouts.len(),
            1,
            "expected one MIR EnumLayout for Option$$i64; got: {option_layouts:?}"
        );
        let layout = option_layouts[0];
        assert_eq!(layout.variants.len(), 2);
        assert_eq!(layout.variants[0].name, "Some");
        assert_eq!(layout.variants[0].field_tys, vec![ResolvedTy::I64]);
        assert_eq!(layout.variants[1].name, "None");
        assert!(layout.variants[1].field_tys.is_empty());
    }
}

/// White-box pins for the three-valued `call_param_consume` verdict. The
/// consume/borrow bit is not observable in codegen (both consume flavours are
/// byte-identical), so the proven-vs-conservative split can only be validated
/// against the fixpoint directly.
#[cfg(test)]
mod call_param_verdict_tests {
    use super::*;
    use hew_hir::ItemId;

    fn expr(kind: HirExprKind) -> HirExpr {
        HirExpr {
            node: HirNodeId(u32::MAX),
            site: SiteId(u32::MAX),
            ty: ResolvedTy::I64,
            value_class: ValueClass::BitCopy,
            intent: IntentKind::Read,
            kind,
            span: 0..0,
        }
    }

    fn binding_ref(id: u32) -> HirExpr {
        expr(HirExprKind::BindingRef {
            name: format!("b{id}"),
            resolved: ResolvedRef::Binding(BindingId(id)),
        })
    }

    /// A one-param fn whose body `tail` is `body_tail`; param binding id is 1.
    fn one_param_fn(id: u32, body_tail: HirExpr) -> HirFn {
        HirFn {
            id: ItemId(id),
            node: HirNodeId(0),
            declaration: hew_types::DefId::new(format!("f{id}")),
            name: format!("f{id}"),
            type_params: Vec::new(),
            params: vec![HirBinding {
                id: BindingId(1),
                name: "p".to_string(),
                ty: ResolvedTy::I64,
                mutable: false,
                span: 0..0,
                is_consume: false,
            }],
            return_ty: ResolvedTy::I64,
            body: HirBlock {
                node: HirNodeId(0),
                scope: ScopeId(0),
                statements: Vec::new(),
                tail: Some(Box::new(body_tail)),
                ty: ResolvedTy::I64,
                span: 0..0,
            },
            span: 0..0,
            is_generator: false,
            intrinsic_id: None,
        }
    }

    fn verdicts(fns: &HashMap<ItemId, &HirFn>) -> HashMap<(ItemId, usize), ConsumeVerdict> {
        let empty = HashSet::new();
        let empty_names = HashSet::new();
        let consume_bool = compute_call_param_consumption(
            fns,
            &empty,
            &empty_names,
            &empty,
            &empty,
            &HashMap::new(),
            false,
        );
        refine_call_param_verdicts(fns, &empty, &empty_names, &empty, &empty, &consume_bool)
    }

    #[test]
    fn call_param_verdict_three_way() {
        // Proven escape: `p` is the returned tail value.
        let proven = one_param_fn(10, binding_ref(1));
        // Conservative: `p` is forwarded as an argument to an unresolved callee
        // (a bare binding ref, so `callee_item_id` is None → the fail-closed
        // forward-to-unproven disjunct is the ONLY flip reason).
        let conservative = one_param_fn(
            20,
            expr(HirExprKind::Call {
                target: hew_types::CallTarget::IndirectFunctionValue,
                callee: Box::new(binding_ref(99)),
                args: vec![binding_ref(1)],
            }),
        );
        // Proven borrow: `p` is never used in a consume position (the tail
        // references a different binding).
        let borrow = one_param_fn(30, binding_ref(2));

        let fns: HashMap<ItemId, &HirFn> = [
            (proven.id, &proven),
            (conservative.id, &conservative),
            (borrow.id, &borrow),
        ]
        .into_iter()
        .collect();

        let v = verdicts(&fns);
        assert_eq!(
            v.get(&(ItemId(10), 0)),
            Some(&ConsumeVerdict::ProvenConsume)
        );
        assert_eq!(
            v.get(&(ItemId(20), 0)),
            Some(&ConsumeVerdict::ConservativeConsume),
        );
        assert_eq!(v.get(&(ItemId(30), 0)), Some(&ConsumeVerdict::ProvenBorrow));

        // Safety projection: both consume flavours read as consume; the borrow
        // does not. This is the bit every consumer keys on.
        assert!(v[&(ItemId(10), 0)].is_consume());
        assert!(v[&(ItemId(20), 0)].is_consume());
        assert!(!v[&(ItemId(30), 0)].is_consume());
    }
}

#[cfg(test)]
mod analyzed_freshness_strictness {
    //! [`callee_returns_fresh_owner`] must answer `true` ONLY for an item this
    //! module analysed and proved clean.
    //!
    //! The reader used to fall back to `unwrap_or(true)` for an absent row, on
    //! the theory that a body-less resolved item (an aggregate constructor, a
    //! runtime primitive) returns an owned value by the cross-ABI contract. A
    //! declared `extern "C"` fn is body-less too, so that fallback handed a
    //! caller-side RELEASE obligation to an un-audited host — and it made an
    //! EMPTY authority answer fresh for EVERY id. Both are closed here: an
    //! absent row is not a freshness proof, so an empty authority grants
    //! nothing.
    use super::*;

    fn callee_with(resolved: ResolvedRef) -> HirExpr {
        HirExpr {
            node: hew_hir::HirNodeId(0),
            site: hew_hir::SiteId(0),
            ty: ResolvedTy::String,
            value_class: hew_hir::ValueClass::CowValue,
            intent: IntentKind::Read,
            kind: HirExprKind::BindingRef {
                name: "callee".to_string(),
                resolved,
            },
            span: hew_parser::ast::Span::default(),
        }
    }

    fn item_callee(id: u32) -> HirExpr {
        callee_with(ResolvedRef::Item(hew_hir::ItemId(id)))
    }

    /// An authority assembled from explicit rows and no declared extern.
    fn rows(pairs: &[(u32, bool)]) -> crate::return_provenance::FreshOwnerVerdicts {
        crate::return_provenance::FreshOwnerVerdicts::from_parts_for_tests(
            pairs
                .iter()
                .map(|&(id, fresh)| (hew_hir::ItemId(id), fresh))
                .collect(),
            HashSet::new(),
        )
    }

    #[test]
    fn an_absent_row_is_not_fresh() {
        let fresh = rows(&[]);
        assert!(
            !callee_returns_fresh_owner(&item_callee(7), &fresh),
            "the reader must require a body this module ANALYZED; an absent row \
             (an extern, a cross-module item, an un-analysed compiler-emitted \
             item) is NOT a freshness proof"
        );
    }

    /// The empty authority must be UNABLE to report fresh — the shape F1
    /// described, where a default-constructed authority answered `true` for
    /// every id and minted the caller release the type exists to forbid.
    ///
    /// The type no longer has a `Default`, so the only empty authority left to
    /// test is the one the empty `CallScrutineeProvenance` carries — the value a
    /// builder holds before the module context is threaded in. It must grant
    /// nothing, for an arbitrary id AND for the id space the real rows use.
    #[test]
    fn an_empty_authority_cannot_report_fresh() {
        let empty = crate::return_provenance::CallScrutineeProvenance::default();
        for id in [0_u32, 1, 7, 99, u32::MAX] {
            assert!(
                !callee_returns_fresh_owner(&item_callee(id), &empty.fresh_owner_verdicts),
                "an authority with no analysed rows must not license a release \
                 for item {id}"
            );
            assert!(
                !empty
                    .fresh_owner_verdicts
                    .item_returns_fresh_owner(hew_hir::ItemId(id)),
                "and the row reader itself must fail closed for item {id}"
            );
        }
        // The same emptiness at the reader assembled from explicit parts: no
        // rows, no extern names, no fresh answer.
        assert!(
            !callee_returns_fresh_owner(&item_callee(7), &rows(&[])),
            "an explicitly empty row set must not license a release either"
        );
    }

    #[test]
    fn an_analyzed_false_row_is_not_fresh() {
        let fresh = rows(&[(7, false)]);
        assert!(
            !callee_returns_fresh_owner(&item_callee(7), &fresh),
            "a forwarder the fixpoint proved non-fresh stays non-fresh — the \
             taint is written into the authority's rows, not conjoined per \
             consumer"
        );
    }

    #[test]
    fn a_declared_opaque_extern_is_not_fresh_at_either_reader() {
        // A direct extern call site's `ResolvedRef::Item` carries a PLACEHOLDER
        // id, so no row exists and the permissive reader's `unwrap_or(true)`
        // would classify it fresh. The authority's NAME veto is what stops it.
        let verdicts = crate::return_provenance::FreshOwnerVerdicts::from_parts_for_tests(
            HashMap::new(),
            HashSet::from(["host_string".to_string()]),
        );
        let mut callee = item_callee(99);
        if let HirExprKind::BindingRef { name, .. } = &mut callee.kind {
            *name = "host_string".to_string();
        }
        assert!(
            !callee_returns_fresh_owner(&callee, &verdicts),
            "a direct opaque-extern callee must not read as a fresh owner — an \
             id lookup cannot catch it, only the NAME veto can"
        );
    }

    /// Lower `source`, run the REAL fixpoints exactly as `lower_module` does,
    /// and report `(coarse verdict, strict verdict)` for the function `name`.
    ///
    /// This deliberately never seeds a row. A test that inserts the fact it
    /// means to test proves nothing about DERIVATION, and derivation is the
    /// whole defect: the coarse fixpoint classifies every body-less resolved
    /// item — a declared extern included — as fresh, so the `true` a wrapper
    /// inherits is `true` for a reason that has nothing to do with the wrapper.
    fn freshness_verdicts(source: &str, name: &str) -> (bool, bool) {
        let module = crate::return_provenance::tests::lower_source(source);
        let origin_fns: HashMap<hew_hir::ItemId, &HirFn> = module
            .items
            .iter()
            .filter_map(|item| match item {
                HirItem::Function(f) => Some((f.id, f)),
                _ => None,
            })
            .collect();
        let Some((&id, _)) = origin_fns.iter().find(|(_, f)| f.name == name) else {
            panic!("no function named `{name}` in the lowered module");
        };

        let coarse = compute_fn_returns_fresh_owner(&origin_fns);
        let provenance = crate::return_provenance::build_call_scrutinee_provenance(
            &module,
            &origin_fns,
            &coarse,
        );
        (
            coarse[&id],
            callee_returns_fresh_owner(&item_callee(id.0), &provenance.fresh_owner_verdicts),
        )
    }

    /// The strict verdict, guarded so the case genuinely exercises the strict
    /// variant's extra work rather than riding on a coarse decline.
    fn analyzed_fresh_for(source: &str, name: &str) -> bool {
        let (coarse, strict) = freshness_verdicts(source, name);
        assert!(
            coarse,
            "guard: the coarse summary must say `{name}` is fresh, otherwise \
             this case never exercises the strict variant's extra work"
        );
        strict
    }

    const EXTERN_DECL: &str = "extern \"C\" {\n    fn host_string() -> string;\n}\n";

    #[test]
    fn a_constructed_fresh_producer_is_fresh() {
        assert!(
            analyzed_fresh_for("fn mk(i: i64) -> string { f\"tok{i}\" }", "mk"),
            "the leak fix must survive: `fn mk(i: i64) -> string {{ f\"tok{{i}}\" }}` \
             is analyzed fresh and keeps its caller-side temp mint"
        );
    }

    #[test]
    fn a_direct_extern_wrapper_is_not_fresh() {
        let src = format!("{EXTERN_DECL}fn wrapper() -> string {{ unsafe {{ host_string() }} }}");
        assert!(
            !analyzed_fresh_for(&src, "wrapper"),
            "a Hew frame around an opaque extern return must not launder it into \
             a freshness proof — the caller would mint a release obligation over \
             an un-audited foreign handle"
        );
    }

    #[test]
    fn a_wrapper_of_a_wrapper_is_not_fresh() {
        let src = format!(
            "{EXTERN_DECL}fn wrapper() -> string {{ unsafe {{ host_string() }} }}\n\
             fn wrapper2() -> string {{ wrapper() }}"
        );
        assert!(
            !analyzed_fresh_for(&src, "wrapper2"),
            "the veto is a fixpoint, so it must be TRANSITIVE across an arbitrary \
             chain of Hew frames"
        );
    }

    #[test]
    fn a_generic_wrapper_is_not_fresh() {
        let src =
            format!("{EXTERN_DECL}fn gwrap<T>(t: T) -> string {{ unsafe {{ host_string() }} }}");
        assert!(
            !analyzed_fresh_for(&src, "gwrap"),
            "a monomorphisation's callee resolves to the generic ORIGIN item, so \
             the origin's row must carry the veto too"
        );
    }

    #[test]
    fn an_argument_launderer_is_not_fresh() {
        let src = format!(
            "{EXTERN_DECL}fn forward(s: string) -> string {{ s }}\n\
             fn launder() -> string {{ forward(unsafe {{ host_string() }}) }}"
        );
        assert!(
            !analyzed_fresh_for(&src, "launder"),
            "an opaque extern result passed THROUGH a pass-through Hew fn is \
             still an opaque extern result"
        );
    }

    #[test]
    fn a_recursive_wrapper_is_not_fresh() {
        let src = format!(
            "{EXTERN_DECL}fn rec(n: i64) -> string {{ \
             if n > 0 {{ rec(n - 1) }} else {{ unsafe {{ host_string() }} }} }}"
        );
        let (coarse, strict) = freshness_verdicts(&src, "rec");
        assert!(
            !strict,
            "a cycle must fail closed: the taint fixpoint only ever adds, so a \
             recursive path back to the extern taints the whole cycle"
        );
        // Defence in depth, and an honest note about WHICH guard fires here: the
        // coarse fixpoint is a LEAST-fixpoint needing positive proof, so a
        // self-recursive producer never reaches `true` there in the first place
        // (a pure recursive `f"base"` producer reads false too). The veto is
        // redundant for this shape rather than load-bearing — but it must still
        // hold, because the coarse verdict is not this gate's authority.
        assert!(
            !coarse,
            "guard: this shape is expected to be declined by the coarse fixpoint \
             as well; if that ever changes the veto becomes load-bearing here and \
             this test must keep passing"
        );
    }

    #[test]
    fn an_indirect_callee_fails_closed() {
        let callee = callee_with(ResolvedRef::Binding(hew_hir::BindingId(3)));
        assert!(
            !callee_returns_fresh_owner(&callee, &rows(&[(7, true)])),
            "a closure value / fn-pointer parameter is not a resolved item and \
             must fail closed"
        );
    }

    /// Lower `source`, run the REAL fixpoints, and ask the authority's COMPOSITE
    /// query about the tail expression of the function `name`.
    ///
    /// As with [`freshness_verdicts`], nothing is seeded: the extern contract
    /// table, the analysed set and the taint set all come out of the same
    /// derivation the module lowering performs.
    fn tail_is_free_of_opaque_foreign_provenance(source: &str, name: &str) -> bool {
        let module = crate::return_provenance::tests::lower_source(source);
        let origin_fns: HashMap<hew_hir::ItemId, &HirFn> = module
            .items
            .iter()
            .filter_map(|item| match item {
                HirItem::Function(f) => Some((f.id, f)),
                _ => None,
            })
            .collect();
        let Some(f) = origin_fns.values().find(|f| f.name == name) else {
            panic!("no function named `{name}` in the lowered module");
        };
        let Some(tail) = f.body.tail.as_ref() else {
            panic!("function `{name}` must end in a tail expression");
        };
        let coarse = compute_fn_returns_fresh_owner(&origin_fns);
        let provenance = crate::return_provenance::build_call_scrutinee_provenance(
            &module,
            &origin_fns,
            &coarse,
        );
        provenance
            .fresh_owner_verdicts
            .value_is_free_of_opaque_foreign_provenance(tail)
    }

    /// Same derivation as [`tail_is_free_of_opaque_foreign_provenance`], but
    /// asking the DUAL — the suppression-side query.
    fn tail_carries_proven_foreign_provenance(source: &str, name: &str) -> bool {
        let module = crate::return_provenance::tests::lower_source(source);
        let origin_fns: HashMap<hew_hir::ItemId, &HirFn> = module
            .items
            .iter()
            .filter_map(|item| match item {
                HirItem::Function(f) => Some((f.id, f)),
                _ => None,
            })
            .collect();
        let Some(f) = origin_fns.values().find(|f| f.name == name) else {
            panic!("no function named `{name}` in the lowered module");
        };
        let Some(tail) = f.body.tail.as_ref() else {
            panic!("function `{name}` must end in a tail expression");
        };
        let coarse = compute_fn_returns_fresh_owner(&origin_fns);
        let provenance = crate::return_provenance::build_call_scrutinee_provenance(
            &module,
            &origin_fns,
            &coarse,
        );
        provenance
            .fresh_owner_verdicts
            .value_carries_proven_foreign_provenance(tail)
    }

    const RECORD_DECL: &str = "record Outer { inner: string }\n\
         extern \"C\" {\n    fn host_string() -> string;\n}\n";

    #[test]
    fn a_direct_root_extern_call_is_proven_foreign() {
        assert!(tail_carries_proven_foreign_provenance(
            &format!("{RECORD_DECL}fn t() -> string {{ unsafe {{ host_string() }} }}"),
            "t"
        ));
    }

    #[test]
    fn a_hew_wrapper_over_a_root_extern_is_proven_foreign() {
        assert!(tail_carries_proven_foreign_provenance(
            &format!(
                "{RECORD_DECL}fn w() -> string {{ unsafe {{ host_string() }} }}\n\
                 fn t() -> string {{ w() }}"
            ),
            "t"
        ));
    }

    #[test]
    fn a_domestic_value_is_not_proven_foreign() {
        assert!(!tail_carries_proven_foreign_provenance(
            &format!("{RECORD_DECL}fn t() -> Outer {{ Outer {{ inner: \"x\" }} }}"),
            "t"
        ));
    }

    /// The polarity that separates the two queries. The strict, MINT-side query
    /// must read an unresolvable callee as foreign; the suppression-side query
    /// must read the same callee as domestic, because being wrong here costs a
    /// LEAK in code that never touches an extern rather than a double release.
    #[test]
    fn an_unresolvable_callee_is_opaque_to_the_strict_query_and_not_proven_to_the_dual() {
        let source = format!("{RECORD_DECL}fn t(f: fn() -> string) -> string {{ f() }}");
        assert!(
            !tail_is_free_of_opaque_foreign_provenance(&source, "t"),
            "the mint-side query must refuse an indirect callee"
        );
        assert!(
            !tail_carries_proven_foreign_provenance(&source, "t"),
            "the suppression-side query must not claim proof from an indirect callee"
        );
    }

    #[test]
    fn an_empty_authority_answers_no_to_the_proven_foreign_query() {
        let module = crate::return_provenance::tests::lower_source(&format!(
            "{RECORD_DECL}fn t() -> string {{ unsafe {{ host_string() }} }}"
        ));
        let f = module
            .items
            .iter()
            .find_map(|item| match item {
                HirItem::Function(f) if f.name == "t" => Some(f),
                _ => None,
            })
            .expect("fn t");
        let tail = f.body.tail.as_ref().expect("tail");
        let empty = crate::return_provenance::FreshOwnerVerdicts::from_parts_for_tests(
            HashMap::new(),
            HashSet::new(),
        );
        assert!(
            !empty.value_carries_proven_foreign_provenance(tail),
            "an authority built from no module analysis proves nothing"
        );
    }

    #[test]
    fn a_domestic_record_literal_is_free_of_foreign_provenance() {
        let src =
            format!("{RECORD_DECL}fn mk(i: i64) -> Outer {{ Outer {{ inner: f\"tok{{i}}\" }} }}");
        assert!(
            tail_is_free_of_opaque_foreign_provenance(&src, "mk"),
            "the composite rule must not be a blanket stop: a container built \
             entirely from domestic values keeps its caller-side mint"
        );
    }

    #[test]
    fn a_record_literal_embedding_a_direct_extern_is_not_free() {
        let src = format!(
            "{RECORD_DECL}fn mk() -> Outer {{ Outer {{ inner: unsafe {{ host_string() }} }} }}"
        );
        assert!(
            !tail_is_free_of_opaque_foreign_provenance(&src, "mk"),
            "F2: the OUTER record genuinely is fresh, but every composite \
             release in this compiler is recursive, so minting an owner over it \
             schedules a release of the host's handle in `inner`"
        );
    }

    #[test]
    fn a_record_literal_embedding_a_wrapper_is_not_free() {
        let src = format!(
            "{RECORD_DECL}fn wrapper() -> string {{ unsafe {{ host_string() }} }}\n\
             fn mk() -> Outer {{ Outer {{ inner: wrapper() }} }}"
        );
        assert!(
            !tail_is_free_of_opaque_foreign_provenance(&src, "mk"),
            "the composite query runs the SAME taint transfer as the return \
             channel, so a laundering Hew frame between the extern and the field \
             does not buy ownership"
        );
    }

    #[test]
    fn a_tuple_embedding_a_direct_extern_is_not_free() {
        let src =
            format!("{RECORD_DECL}fn mk() -> (string, i64) {{ (unsafe {{ host_string() }}, 1) }}");
        assert!(
            !tail_is_free_of_opaque_foreign_provenance(&src, "mk"),
            "the rule is about COMPOSITES, not about one container syntax: a \
             tuple's recursive release reaches the foreign element too"
        );
    }

    #[test]
    fn a_nested_record_embedding_a_direct_extern_is_not_free() {
        let src = format!(
            "record Mid {{ o: Outer }}\n{RECORD_DECL}\
             fn mk() -> Mid {{ Mid {{ o: Outer {{ inner: unsafe {{ host_string() }} }} }} }}"
        );
        assert!(
            !tail_is_free_of_opaque_foreign_provenance(&src, "mk"),
            "a foreign handle buried at any depth taints the whole spine, since \
             the outermost release walks all of it"
        );
    }

    #[test]
    fn an_empty_authority_answers_no_to_the_composite_query() {
        // The same unrepresentability the row reader has: an authority that
        // never ran the module analysis must not certify ANY value clean. The
        // taint policy's own body-less-item clause would otherwise classify
        // every call `Fresh` against an empty extern table — a `Default`-shaped
        // fail-open reintroduced at a new query.
        let empty = crate::return_provenance::CallScrutineeProvenance::default();
        let literal = HirExpr {
            node: hew_hir::HirNodeId(0),
            site: hew_hir::SiteId(0),
            ty: ResolvedTy::I64,
            value_class: hew_hir::ValueClass::CowValue,
            intent: IntentKind::Read,
            kind: HirExprKind::Literal(hew_hir::HirLiteral::Integer(1)),
            span: hew_parser::ast::Span::default(),
        };
        assert!(
            !empty
                .fresh_owner_verdicts
                .value_is_free_of_opaque_foreign_provenance(&literal),
            "an authority with no module analysis behind it must decline even \
             for an integer literal — the query reports a PROOF, and an empty \
             authority has proved nothing"
        );
    }
}
