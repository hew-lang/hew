//! Closure environment layout.
//!
//! Publishes the two foundation APIs that downstream consumers
//! (generator codegen, auto-lock injection) depend on:
//!
//! * [`ClosureEnvLayout::lock_slot_for`] — returns the lock-slot offset
//!   for a capture that requires auto-lock injection. `None` unless the
//!   capture is `BorrowMut` over a non-`Sync` type AND lock-slot
//!   construction is enabled (follow-on auto-lock toggle). Per plan
//!   §15.3 R7, the lock-slot type is left **opaque**
//!   (`*mut HewAutoMutex`); this module does not import any concrete
//!   mutex primitive — that belongs to the follow-on auto-lock
//!   implementation.
//! * [`ClosureEnvLayout::has_suspend_in_body`] — true iff the closure
//!   body contains an `await`, `yield`, channel `recv`/`send`, or
//!   `fork`-handle await suspend point. Generators consume this to
//!   decide whether the state-machine carrying this closure needs
//!   cancel-observation insertion.
//!
//! Both APIs are pure-functional derivations of facts the checker
//! already produces: per-capture `mode`/`is_sync` from
//! `ClosureCaptureFact`, per-literal escape kind from
//! `ClosureEscapeFact`, and an HIR walk for suspend-point detection
//! (HIR mirrors the AST-level scan from
//! `closure_inference.rs::scan_lambda_body`).
//!
//! **Contract freeze.** Per plan §15.1 the two method names and
//! signatures are frozen. Renaming requires a coordinated change
//! across every downstream consumer of this seam.
//!
//! **Layout shape** (plan §15.2):
//!
//! ```text
//!   ┌─────────────────────────────────┐  offset 0
//!   │ capture_data[0..N]              │
//!   ├─────────────────────────────────┤  offset = sum(capture sizes, aligned)
//!   │ lock_slots[0..M]   (*mut Mtx)   │  ← only populated when auto-locks enabled
//!   └─────────────────────────────────┘                AND capture is BorrowMut + !is_sync
//! ```
//!
//! Lock-slot tail offset is derivable from `capture_data` sum-of-sizes;
//! the auto-lock consumer reads `lock_slot_for()` rather than
//! recomputing.

// The suspend walker visits the `#[deprecated]` `CallTraitMethodStatic`
// variant exhaustively.
#![allow(
    deprecated,
    reason = "legacy CallTraitMethodStatic variant is allowlist-gated; \
              see hew-hir/tests/call_trait_method_static_creation_allowlist.rs"
)]

use hew_hir::{HirExpr, HirExprKind, HirSelectArmKind};
use hew_types::{ClosureCaptureMode, ClosureEscapeKind, ResolvedTy};

/// Stable identity of one capture within a closure's environment, in
/// first-use order assigned by the MIR builder when materialising the
/// env record. Mirrors the `FieldOffset` of the env-record field.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CaptureId(pub u32);

/// Index into the lock-slot tail array. Distinct from [`CaptureId`]
/// because the tail is sparse: only captures classified `BorrowMut`
/// over a non-`Sync` type contribute a slot. The auto-lock injection
/// consumer uses this index to address `hew_auto_mutex_alloc()`
/// outputs stored in the env's tail region.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LockSlotIdx(pub u32);

/// One capture descriptor consumed at layout time. Derived from
/// `HirClosureCapture` (which in turn carries the checker's
/// `ClosureCaptureFact`).
#[derive(Debug, Clone, PartialEq)]
pub struct CaptureField {
    /// Stable per-capture identity (matches env-record field offset).
    pub id: CaptureId,
    /// Fully-resolved captured type.
    pub ty: ResolvedTy,
    /// Checker-selected capture mode (`Copy`/`Move`/`Borrow`/`BorrowMut`).
    pub mode: ClosureCaptureMode,
    /// Whether the captured type satisfies `Sync`. Combined with `mode`
    /// to decide auto-lock eligibility.
    pub is_sync: bool,
}

/// Where the closure environment record itself lives. Derived from
/// the checker's escape classification (R242=B conservative):
///
/// * `Stack` — the env-record is allocated in the parent frame as a
///   plain local; lifetime is bounded by the introducing scope.
///   Selected for `ClosureEscapeKind::Local`.
/// * `Heap` — the env-record is allocated through the `hew_dyn_box_alloc`
///   heap-box ABI because the closure may outlive its
///   introducing scope. Selected for `ClosureEscapeKind::Escapes`
///   (conservative default).
/// * `ScopeOwned` — the env-record is handed off to a `scope { fork
///   ... }` child task; ownership rides the `SpawnTaskClosure` path
///   so the parent `scope{}` block reclaims the storage at the
///   structured-concurrency join. Selected for
///   `ClosureEscapeKind::Forked`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AllocationStrategy {
    Stack,
    Heap,
    ScopeOwned,
}

/// Concrete closure environment layout — the public seam consumed by
/// generator codegen and auto-lock injection.
///
/// Built once per closure literal by the MIR lowering pass; lives long
/// enough for the closure's MIR to be assembled and is discarded
/// thereafter. Pure data: no internal references into the HIR.
#[derive(Debug, Clone, PartialEq)]
pub struct ClosureEnvLayout {
    captures: Vec<CaptureField>,
    /// Sparse parallel array mapping `CaptureId.0 (u32) -> Option<LockSlotIdx>`.
    /// Indexed by capture id; `None` entries pad captures with no lock slot.
    /// Empty when lock-slot construction is disabled (current state)
    /// — `lock_slot_for` returns `None` for every capture under that gate
    /// for zero-cost.
    lock_slots: Vec<Option<LockSlotIdx>>,
    has_suspend: bool,
    alloc_strategy: AllocationStrategy,
}

impl ClosureEnvLayout {
    /// Build a layout from already-derived capture descriptors plus
    /// the closure body (walked to detect suspend points) plus the
    /// checker's escape classification.
    ///
    /// `enable_lock_slots` is the auto-lock gate: when `false` (current
    /// state — follow-on lock-slot implementation pending), no lock
    /// slots are constructed (`M = 0`, `lock_slot_for(_) == None` for
    /// every capture). When the follow-on consumer flips this to
    /// `true`, slots are allocated for every `BorrowMut + !is_sync`
    /// capture per plan §15.2.
    ///
    /// # Panics
    ///
    /// Panics if the input `captures` slice exceeds `u32::MAX` entries
    /// — a non-reachable bound in practice (closure capture counts are
    /// bounded by HIR's source-level enumeration of free variables).
    #[must_use]
    pub fn build(
        captures: Vec<CaptureField>,
        body: &HirExpr,
        escape_kind: ClosureEscapeKind,
        enable_lock_slots: bool,
    ) -> Self {
        let lock_slots = if enable_lock_slots {
            let mut next_slot: u32 = 0;
            captures
                .iter()
                .map(|cap| {
                    if needs_lock_slot(cap) {
                        let idx = LockSlotIdx(next_slot);
                        next_slot = next_slot
                            .checked_add(1)
                            .expect("lock-slot count exceeds u32::MAX");
                        Some(idx)
                    } else {
                        None
                    }
                })
                .collect()
        } else {
            Vec::new()
        };
        Self {
            captures,
            lock_slots,
            has_suspend: body_contains_suspend(body),
            alloc_strategy: allocation_strategy_for(escape_kind),
        }
    }

    /// **Frozen API (plan §15.1).** Returns the lock-slot index for a
    /// capture that needs auto-lock around mutation. Returns `None`
    /// when the capture has no lock slot (Copy, Move, `Sync`-typed
    /// `BorrowMut`, `Borrow`, OR lock-slot construction is
    /// disabled per the build-time gate).
    ///
    /// Out-of-range `capture_id` returns `None` (callers MUST source
    /// `capture_id` from this layout's own `captures()` list).
    #[must_use]
    pub fn lock_slot_for(&self, capture_id: CaptureId) -> Option<LockSlotIdx> {
        let idx = capture_id.0 as usize;
        self.lock_slots.get(idx).copied().flatten()
    }

    /// **Frozen API (plan §15.1).** True iff the closure body contains
    /// a suspend point (`await`, channel `recv`/`send`, `for await`,
    /// `yield`, fork-handle await). Consumed by generator codegen to
    /// decide whether cancel-observation insertion is required.
    #[must_use]
    pub fn has_suspend_in_body(&self) -> bool {
        self.has_suspend
    }

    /// Storage strategy for the env-record itself. Derived from the
    /// checker's `ClosureEscapeKind`. MIR lowering reads this to
    /// decide between stack alloca, `hew_dyn_box_alloc` heap-box, and
    /// `SpawnTaskClosure` scope-owned transfer.
    #[must_use]
    pub fn allocation_strategy(&self) -> AllocationStrategy {
        self.alloc_strategy
    }

    /// Capture descriptors in env-record field order.
    #[must_use]
    pub fn captures(&self) -> &[CaptureField] {
        &self.captures
    }

    /// Number of populated lock slots in the tail region. `0` when
    /// auto-lock construction is disabled (zero-cost path).
    ///
    /// # Panics
    ///
    /// Panics if the populated lock-slot count exceeds `u32::MAX` —
    /// not reachable in practice (closure capture counts are bounded
    /// by HIR's enumeration of free variables, and only a subset of
    /// captures qualify for lock slots).
    #[must_use]
    pub fn lock_slot_count(&self) -> u32 {
        u32::try_from(self.lock_slots.iter().filter(|s| s.is_some()).count())
            .expect("lock-slot count exceeds u32::MAX")
    }
}

fn needs_lock_slot(capture: &CaptureField) -> bool {
    matches!(capture.mode, ClosureCaptureMode::BorrowMut) && !capture.is_sync
}

fn allocation_strategy_for(escape_kind: ClosureEscapeKind) -> AllocationStrategy {
    match escape_kind {
        ClosureEscapeKind::Local => AllocationStrategy::Stack,
        ClosureEscapeKind::Forked => AllocationStrategy::ScopeOwned,
        ClosureEscapeKind::Escapes => AllocationStrategy::Heap,
    }
}

/// Walk a HIR closure body and return true iff any sub-expression is a
/// suspend point. Mirrors the AST-level scan in
/// `hew-types/src/check/closure_inference.rs::scan_lambda_body`, but
/// runs over the post-lowering HIR shape so MIR consumers do not need
/// to keep the parser AST live.
///
/// Suspend forms:
/// * `await` on a task handle (`HirExprKind::AwaitTask`)
/// * `yield` inside a generator body (`HirExprKind::Yield`)
/// * Channel `recv` arms inside `select{}` (the only first-class recv
///   surface — `HirSelectArmKind::ChannelRecv` / `Duplex` /
///   `StreamNext` / `TaskAwait` / `ActorAsk` all suspend)
/// * Actor `ask` (`HirExprKind::ActorAsk`) — blocks on the reply
/// * Fork-block bodies internally suspend via their task awaits;
///   conservative: any `ForkBlock` makes the surrounding body
///   suspending.
///
/// Channel `send` over a bounded mailbox CAN suspend; we report any
/// `ActorSend` as a suspend point to stay conservative (matches the
/// plan §15.3 R9 contract: a false `has_suspend_in_body == false`
/// would be a P0 bug; a false `true` only causes generator codegen
/// to insert a cancel-observation point unnecessarily — safe).
fn body_contains_suspend(expr: &HirExpr) -> bool {
    let mut found = false;
    walk_expr_for_suspend(expr, &mut found);
    found
}

#[allow(
    clippy::too_many_lines,
    reason = "exhaustive HIR walker — splitting across helpers obscures the \
              one-arm-per-kind suspend-classification table; mirrors the \
              shape of collect_hir_emitted_events_walk in hew-hir::lower"
)]
#[allow(
    clippy::match_same_arms,
    reason = "suspend-classification arms are deliberately separate per HIR \
              variant so future suspend-bearing additions land as targeted \
              edits; merging would obscure the per-variant boundary contract \
              (plan §15.3 R9: missed arm = P0 bug)"
)]
fn walk_expr_for_suspend(expr: &HirExpr, found: &mut bool) {
    if *found {
        return;
    }
    match &expr.kind {
        // --- direct suspend points (P0 contract: missing arm = bug) -
        HirExprKind::AwaitTask { .. }
        | HirExprKind::Yield { .. }
        | HirExprKind::ActorAsk { .. }
        | HirExprKind::RemoteActorAsk { .. }
        // `await conn.read()` suspends the handler on fd-readiness (NEW-1).
        | HirExprKind::ConnAwaitRead { .. }
        // `await listener.accept()` suspends the handler on listener-readiness
        // (NEW-2).
        | HirExprKind::ListenerAwaitAccept { .. }
        // `await rx.recv() | after d` suspends the handler on channel-readiness
        // with a deadline (L4 phase 2).
        | HirExprKind::ChannelRecvAwait { .. }
        // `await stream.recv() | after d` suspends the handler on stream-readiness
        // with a deadline (L4 phase 2).
        | HirExprKind::StreamRecvAwait { .. }
        // `await_restart sup.child` parks the actor on the supervisor restart
        // observer — a direct cooperative suspend point.
        | HirExprKind::AwaitRestart { .. }
        | HirExprKind::ActorSend { .. } => {
            *found = true;
        }
        HirExprKind::ForkBlock { body, .. } => {
            // A nested fork awaits its children at scope-exit, so the
            // outer body suspends. We still walk for nested suspends
            // inside the fork body so the boundary is symmetric with
            // the rest of the walker.
            *found = true;
            walk_block_for_suspend(body, found);
        }
        HirExprKind::Select(select) => {
            // Every value-bearing select arm receives from a
            // suspending source (stream-next, task-await, actor-ask)
            // or fires on a timer (AfterTimer) — all are suspend
            // points from the perspective of the enclosing body.
            for arm in &select.arms {
                match &arm.kind {
                    HirSelectArmKind::StreamNext { .. }
                    | HirSelectArmKind::ActorAsk { .. }
                    | HirSelectArmKind::TaskAwait { .. }
                    | HirSelectArmKind::ChannelRecv { .. }
                    | HirSelectArmKind::AfterTimer { .. } => {
                        *found = true;
                    }
                }
                walk_expr_for_suspend(&arm.body, found);
            }
        }
        HirExprKind::Join(join) => {
            // Every join branch is an actor-ask issued concurrently; the
            // wait-ALL is a suspend point from the enclosing body's view.
            *found = true;
            for branch in &join.branches {
                walk_expr_for_suspend(&branch.actor, found);
                for arg in &branch.args {
                    walk_expr_for_suspend(arg, found);
                }
            }
        }
        // --- containers: recurse into all sub-expressions -----------
        HirExprKind::Binary { left, right, .. } | HirExprKind::IdentityCompare { left, right } => {
            walk_expr_for_suspend(left, found);
            walk_expr_for_suspend(right, found);
        }
        // Wire codec is a synchronous serialize/deserialize call, never a
        // suspend point; walk its operand like any other borrowing read.
        HirExprKind::Unary { operand, .. } | HirExprKind::WireCodec { operand, .. } => {
            walk_expr_for_suspend(operand, found);
        }
        HirExprKind::NumericCast { value, .. }
        | HirExprKind::SaturatingWidthCast { value, .. } => {
            walk_expr_for_suspend(value, found);
        }
        HirExprKind::TupleLiteral { elements } => {
            for elem in elements {
                walk_expr_for_suspend(elem, found);
            }
        }
        HirExprKind::Call { callee, args } | HirExprKind::SpawnedCall { callee, args, .. } => {
            walk_expr_for_suspend(callee, found);
            for a in args {
                walk_expr_for_suspend(a, found);
            }
        }
        HirExprKind::Spawn { args, .. } => {
            for (_, e) in args {
                walk_expr_for_suspend(e, found);
            }
        }
        HirExprKind::If {
            condition,
            then_expr,
            else_expr,
        } => {
            walk_expr_for_suspend(condition, found);
            walk_expr_for_suspend(then_expr, found);
            if let Some(e) = else_expr {
                walk_expr_for_suspend(e, found);
            }
        }
        HirExprKind::Block(block) => walk_block_for_suspend(block, found),
        HirExprKind::StructInit { fields, base, .. } => {
            for (_, e) in fields {
                walk_expr_for_suspend(e, found);
            }
            if let Some(b) = base {
                walk_expr_for_suspend(b, found);
            }
        }
        HirExprKind::FieldAccess { object, .. } => walk_expr_for_suspend(object, found),
        HirExprKind::Scope { body } | HirExprKind::ScopeDeadline { body, .. } => {
            walk_block_for_suspend(body, found);
        }
        HirExprKind::SpawnLambdaActor { .. } | HirExprKind::Closure { .. } => {
            // A nested closure / lambda-actor literal's body is NOT
            // executed inside the enclosing closure — only
            // constructed.  Suspend points inside the nested body
            // suspend the nested closure, not this one.  Do not
            // recurse.
        }
        HirExprKind::GenBlock { body, .. } => walk_block_for_suspend(body, found),
        HirExprKind::TupleIndex { tuple, .. } => walk_expr_for_suspend(tuple, found),
        HirExprKind::Index { container, index } => {
            walk_expr_for_suspend(container, found);
            walk_expr_for_suspend(index, found);
        }
        HirExprKind::Slice {
            container,
            start,
            end,
            ..
        } => {
            walk_expr_for_suspend(container, found);
            if let Some(s) = start {
                walk_expr_for_suspend(s, found);
            }
            if let Some(e) = end {
                walk_expr_for_suspend(e, found);
            }
        }
        HirExprKind::CoerceToDynTrait { value, .. } => walk_expr_for_suspend(value, found),
        HirExprKind::CallDynMethod { receiver, args, .. }
        | HirExprKind::ResolvedImplCall { receiver, args, .. }
        | HirExprKind::CallTraitMethodStatic { receiver, args, .. }
        | HirExprKind::VarSelfMethodCall { receiver, args, .. } => {
            walk_expr_for_suspend(receiver, found);
            for a in args {
                walk_expr_for_suspend(a, found);
            }
        }
        HirExprKind::NumericMethod { receiver, arg, .. } => {
            walk_expr_for_suspend(receiver, found);
            walk_expr_for_suspend(arg, found);
        }
        HirExprKind::CancellationTokenIsCancelled { receiver }
        | HirExprKind::GeneratorNext { receiver, .. }
        | HirExprKind::RecordCloneCall { src: receiver, .. } => {
            walk_expr_for_suspend(receiver, found);
        }
        HirExprKind::MachineEmit { fields, .. } => {
            for (_, e) in fields {
                walk_expr_for_suspend(e, found);
            }
        }
        HirExprKind::MachineStep {
            receiver, event, ..
        } => {
            walk_expr_for_suspend(receiver, found);
            walk_expr_for_suspend(event, found);
        }
        HirExprKind::MachineStateName { receiver, .. } => walk_expr_for_suspend(receiver, found),
        HirExprKind::MachineFieldAccess { .. } | HirExprKind::MachineEventFieldAccess { .. } => {
            // Machine-field accesses are HIR-side-resolved leaves that
            // load from `self` / the matched event. No sub-expression
            // to walk.
        }
        HirExprKind::MachineVariantCtor { payload, .. } => {
            if let Some(fields) = payload {
                for (_, e) in fields {
                    walk_expr_for_suspend(e, found);
                }
            }
        }
        HirExprKind::While {
            condition, body, ..
        } => {
            walk_expr_for_suspend(condition, found);
            walk_block_for_suspend(body, found);
        }
        HirExprKind::ForRange {
            start,
            end,
            step,
            body,
            ..
        } => {
            walk_expr_for_suspend(start, found);
            walk_expr_for_suspend(end, found);
            walk_expr_for_suspend(step, found);
            walk_block_for_suspend(body, found);
        }
        HirExprKind::Match { scrutinee, arms } => {
            walk_expr_for_suspend(scrutinee, found);
            for arm in arms {
                walk_expr_for_suspend(&arm.body, found);
            }
        }
        HirExprKind::WhileLet {
            scrutinee, body, ..
        } => {
            walk_expr_for_suspend(scrutinee, found);
            walk_block_for_suspend(body, found);
        }
        HirExprKind::IfLet {
            scrutinee,
            body,
            else_body,
            ..
        } => {
            walk_expr_for_suspend(scrutinee, found);
            walk_block_for_suspend(body, found);
            if let Some(eb) = else_body {
                walk_block_for_suspend(eb, found);
            }
        }
        HirExprKind::Loop { body, .. } => walk_block_for_suspend(body, found),
        HirExprKind::Break { value, .. } | HirExprKind::Return { value } => {
            if let Some(value) = value {
                walk_expr_for_suspend(value, found);
            }
        }
        // --- leaves: no suspend, no children to recurse into --------
        HirExprKind::Literal(_)
        | HirExprKind::BindingRef { .. }
        | HirExprKind::ContextReader { .. }
        | HirExprKind::RegexLiteralRef { .. }
        | HirExprKind::Continue { .. }
        | HirExprKind::ActorSelf
        | HirExprKind::Unsupported(_) => {}
    }
}

fn walk_block_for_suspend(block: &hew_hir::HirBlock, found: &mut bool) {
    if *found {
        return;
    }
    for stmt in &block.statements {
        walk_stmt_for_suspend(stmt, found);
        if *found {
            return;
        }
    }
    if let Some(tail) = &block.tail {
        walk_expr_for_suspend(tail, found);
    }
}

fn walk_stmt_for_suspend(stmt: &hew_hir::HirStmt, found: &mut bool) {
    use hew_hir::HirStmtKind;
    match &stmt.kind {
        HirStmtKind::Let(_, Some(value)) | HirStmtKind::Return(Some(value)) => {
            walk_expr_for_suspend(value, found);
        }
        HirStmtKind::Expr(expr) => walk_expr_for_suspend(expr, found),
        HirStmtKind::Assign { target, value, .. } => {
            walk_expr_for_suspend(target, found);
            walk_expr_for_suspend(value, found);
        }
        HirStmtKind::Defer { body, .. } => walk_expr_for_suspend(body, found),
        HirStmtKind::LetElse {
            scrutinee,
            success_prelude,
            else_body,
            ..
        } => {
            walk_expr_for_suspend(scrutinee, found);
            for prelude_stmt in success_prelude {
                walk_stmt_for_suspend(prelude_stmt, found);
                if *found {
                    return;
                }
            }
            walk_block_for_suspend(else_body, found);
        }
        HirStmtKind::Let(_, None) | HirStmtKind::Return(None) => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hew_hir::{
        BindingId, HirBlock, HirLiteral, HirNodeId, HirStmt, HirStmtKind, IntentKind, ScopeId,
        SiteId, ValueClass,
    };

    fn site() -> SiteId {
        SiteId(0)
    }

    fn dummy_expr(kind: HirExprKind, ty: ResolvedTy) -> HirExpr {
        HirExpr {
            node: HirNodeId(0),
            site: site(),
            ty,
            value_class: ValueClass::BitCopy,
            intent: IntentKind::Read,
            kind,
            span: 0..0,
        }
    }

    fn lit_int(v: i64) -> HirExpr {
        dummy_expr(
            HirExprKind::Literal(HirLiteral::Integer(v)),
            ResolvedTy::I64,
        )
    }

    fn capture(id: u32, mode: ClosureCaptureMode, is_sync: bool) -> CaptureField {
        CaptureField {
            id: CaptureId(id),
            ty: ResolvedTy::I64,
            mode,
            is_sync,
        }
    }

    fn pure_body() -> HirExpr {
        // 1 + 2 — pure arithmetic, no suspend point reachable from
        // any walker arm.
        dummy_expr(
            HirExprKind::Binary {
                op: hew_parser::ast::BinaryOp::Add,
                left: Box::new(lit_int(1)),
                right: Box::new(lit_int(2)),
            },
            ResolvedTy::I64,
        )
    }

    fn await_body() -> HirExpr {
        // The walker classifies AwaitTask as a direct suspend leaf,
        // so the inner binding identity does not affect the outcome.
        dummy_expr(
            HirExprKind::AwaitTask {
                binding_name: "t".to_string(),
                binding_id: BindingId(0),
                output_ty: ResolvedTy::Unit,
            },
            ResolvedTy::Unit,
        )
    }

    /// Plan §15 R242=B: a `Local` escape classification must select
    /// stack-allocated env storage.
    #[test]
    fn closure_local_lowers_to_stack_allocated_env() {
        let layout = ClosureEnvLayout::build(
            vec![capture(0, ClosureCaptureMode::Copy, true)],
            &pure_body(),
            ClosureEscapeKind::Local,
            false,
        );
        assert_eq!(layout.allocation_strategy(), AllocationStrategy::Stack);
    }

    /// Plan §15 R242=B: an `Escapes` classification (conservative
    /// default) must select the `hew_dyn_box_alloc` heap-box path.
    #[test]
    fn closure_escapes_lowers_to_heap_box() {
        let layout = ClosureEnvLayout::build(
            vec![capture(0, ClosureCaptureMode::Move, true)],
            &pure_body(),
            ClosureEscapeKind::Escapes,
            false,
        );
        assert_eq!(layout.allocation_strategy(), AllocationStrategy::Heap);
    }

    /// Plan §15 R242=B: a `Forked` classification rides the
    /// `SpawnTaskClosure` scope-owned path (env reclaimed at the
    /// parent `scope{}` join).
    #[test]
    fn closure_forked_lowers_to_scope_owned() {
        let layout = ClosureEnvLayout::build(
            vec![capture(0, ClosureCaptureMode::Borrow, true)],
            &pure_body(),
            ClosureEscapeKind::Forked,
            false,
        );
        assert_eq!(layout.allocation_strategy(), AllocationStrategy::ScopeOwned);
    }

    /// Plan §15.2 selection rule: lock slot is excluded for every
    /// non-`BorrowMut` mode (Copy, Move, Borrow).
    #[test]
    fn lock_slot_for_returns_none_when_not_borrowmut() {
        let captures = vec![
            capture(0, ClosureCaptureMode::Copy, false),
            capture(1, ClosureCaptureMode::Move, false),
            capture(2, ClosureCaptureMode::Borrow, false),
        ];
        let layout = ClosureEnvLayout::build(
            captures,
            &pure_body(),
            ClosureEscapeKind::Local,
            /* enable_lock_slots = */ true,
        );
        assert_eq!(layout.lock_slot_for(CaptureId(0)), None);
        assert_eq!(layout.lock_slot_for(CaptureId(1)), None);
        assert_eq!(layout.lock_slot_for(CaptureId(2)), None);
        assert_eq!(
            layout.lock_slot_count(),
            0,
            "no BorrowMut captures must produce zero lock slots"
        );
    }

    /// Plan §15.2 selection rule (positive case for the auto-lock
    /// consumer): `BorrowMut` over a non-`Sync` type must yield a
    /// populated lock slot when lock-slot construction is enabled.
    #[test]
    fn lock_slot_for_returns_some_when_borrowmut_non_sync() {
        let captures = vec![
            capture(0, ClosureCaptureMode::Copy, true),
            capture(1, ClosureCaptureMode::BorrowMut, /* is_sync = */ false),
            capture(2, ClosureCaptureMode::BorrowMut, /* is_sync = */ true),
        ];
        let layout = ClosureEnvLayout::build(
            captures,
            &pure_body(),
            ClosureEscapeKind::Local,
            /* enable_lock_slots = */ true,
        );
        assert_eq!(layout.lock_slot_for(CaptureId(0)), None);
        assert_eq!(
            layout.lock_slot_for(CaptureId(1)),
            Some(LockSlotIdx(0)),
            "BorrowMut + !Sync must allocate a lock slot"
        );
        assert_eq!(
            layout.lock_slot_for(CaptureId(2)),
            None,
            "BorrowMut + Sync needs no auto-lock — Sync-typed values are \
             externally synchronised"
        );
        assert_eq!(layout.lock_slot_count(), 1);
    }

    /// Plan §15.3 R7 zero-cost contract: when auto-locks are disabled,
    /// no lock slots are constructed even for `BorrowMut + !Sync`
    /// captures. `lock_slot_for` returns `None` for every capture.
    #[test]
    fn lock_slot_for_zero_cost_when_auto_locks_disabled() {
        let captures = vec![capture(
            0,
            ClosureCaptureMode::BorrowMut,
            /* is_sync = */ false,
        )];
        let layout = ClosureEnvLayout::build(
            captures,
            &pure_body(),
            ClosureEscapeKind::Local,
            /* enable_lock_slots = */ false,
        );
        assert_eq!(layout.lock_slot_for(CaptureId(0)), None);
        assert_eq!(layout.lock_slot_count(), 0);
    }

    /// Plan §15.3 R9 (P0): a closure body containing a suspend point
    /// must report `has_suspend_in_body() == true`.
    #[test]
    fn has_suspend_in_body_positive() {
        let layout =
            ClosureEnvLayout::build(vec![], &await_body(), ClosureEscapeKind::Local, false);
        assert!(
            layout.has_suspend_in_body(),
            "closure body containing AwaitTask must report \
             has_suspend_in_body == true"
        );
    }

    /// Plan §15.3 R9 (P0): a pure-arithmetic body must report
    /// `has_suspend_in_body() == false`.
    #[test]
    fn has_suspend_in_body_negative() {
        let layout = ClosureEnvLayout::build(vec![], &pure_body(), ClosureEscapeKind::Local, false);
        assert!(
            !layout.has_suspend_in_body(),
            "pure-arithmetic body must report has_suspend_in_body == false"
        );
    }

    /// Suspend detection must recurse through container nodes —
    /// `1 + await(t)` (modelled as `Binary { left: lit, right:
    /// await_body }`) is a suspending body.
    #[test]
    fn has_suspend_in_body_recurses_through_binary() {
        let body = dummy_expr(
            HirExprKind::Binary {
                op: hew_parser::ast::BinaryOp::Add,
                left: Box::new(lit_int(1)),
                right: Box::new(await_body()),
            },
            ResolvedTy::I64,
        );
        let layout = ClosureEnvLayout::build(vec![], &body, ClosureEscapeKind::Local, false);
        assert!(layout.has_suspend_in_body());
    }

    // PIN TEST — Locked surface for W3.037/W3.038 parallel fan-out.
    // See plans/v05-coherence-sequencing-plan.md §Locked ClosureEnvLayout API.
    // Changes here must coordinate with downstream lanes.
    //
    // Each `let _: fn(...) = ClosureEnvLayout::method;` is a compile-time
    // coercion to a specific function-pointer type.  If the method is
    // renamed, gains/loses a parameter, or changes a return type the
    // coercion fails and the build breaks with a clear type-mismatch error —
    // surface rather than link-time, and earlier than any snapshot test.
    /// SC-5 / `wire-contract-test-presence` — Cluster B contract freeze
    /// (`closure_env.rs:27-29`). If any of these `fn` coercions stop
    /// compiling, a downstream consumer's expected API has shifted and
    /// the change must be co-ordinated with W3.036 / W3.037 / W3.038
    /// per `.tmp/orchestration/plans/v05-coherence-sequencing-plan.md`.
    #[test]
    fn closure_env_layout_api_frozen() {
        let _: fn(Vec<CaptureField>, &HirExpr, ClosureEscapeKind, bool) -> ClosureEnvLayout =
            ClosureEnvLayout::build;
        let _: fn(&ClosureEnvLayout, CaptureId) -> Option<LockSlotIdx> =
            ClosureEnvLayout::lock_slot_for;
        let _: fn(&ClosureEnvLayout) -> bool = ClosureEnvLayout::has_suspend_in_body;
        let _: fn(&ClosureEnvLayout) -> AllocationStrategy = ClosureEnvLayout::allocation_strategy;
        let _: fn(&ClosureEnvLayout) -> &[CaptureField] = ClosureEnvLayout::captures;
        let _: fn(&ClosureEnvLayout) -> u32 = ClosureEnvLayout::lock_slot_count;
    }

    /// A nested block-scoped suspend must propagate up.
    #[test]
    fn has_suspend_in_body_walks_into_block() {
        let block = HirBlock {
            node: HirNodeId(0),
            scope: ScopeId(0),
            statements: vec![HirStmt {
                node: HirNodeId(0),
                kind: HirStmtKind::Expr(await_body()),
                span: 0..0,
            }],
            tail: None,
            ty: ResolvedTy::Unit,
            span: 0..0,
        };
        let body = dummy_expr(HirExprKind::Scope { body: block }, ResolvedTy::Unit);
        let layout = ClosureEnvLayout::build(vec![], &body, ClosureEscapeKind::Local, false);
        assert!(layout.has_suspend_in_body());
    }
}
