//! Per-block move-checker dataflow over the four-state binding lattice.
//!
//! Replaces the pre-CFG flat-stream forward-scan that worked correctly
//! only while `HirExprKind::If` lowered its arms inline (single basic
//! block). With real CFG construction in `lower::Builder::lower_if`,
//! each function may have multiple `BasicBlock`s connected by
//! `Terminator::{Goto, Branch}` and the move-checker must reason about
//! every reachable path.
//!
//! ## Lattice
//!
//! Per-binding state at each program point. Initial state for every
//! binding is implicitly `Uninit`; the lattice carries only the
//! bindings that have been touched on at least one path.
//!
//! ```text
//!     Uninit  ⊔ X         = Uninit          (most-conservative)
//!     Live    ⊔ Live      = Live
//!     Live    ⊔ Consumed  = MaybeConsumed
//!     Live    ⊔ MaybeC    = MaybeConsumed
//!     Cons(a) ⊔ Cons(b)   = Consumed(min(a,b))
//!     Cons(a) ⊔ MaybeC(b) = MaybeConsumed(min(a,b))
//!     MaybeC(a)⊔ MaybeC(b)= MaybeConsumed(min(a,b))
//! ```
//!
//! The meet (`⊓` = `join` in the dataflow sense — we describe the
//! confluence of incoming paths; both names denote the same operator
//! over this finite lattice) is commutative, associative, and
//! idempotent — property-tested below.
//!
//! ## Transfer function
//!
//! On `Bind`: state := `Live` (overwriting any prior state on this
//! path, since `Bind` is the binding's initialiser). The `@linear`
//! ledger records the binding's name + type for the per-exit
//! `MustConsume` check.
//!
//! On `Use`:
//!  - `Uninit`     → emit `InitialisedBeforeUse`.
//!  - `Consumed(s)` → emit `UseAfterConsume{consumed_at: s, used_at}`.
//!  - `MaybeConsumed(s)` → emit `UseAfterConsume{consumed_at: s,
//!    used_at}` (the diagnostic surface is the same; a future polish
//!    cluster may add the "consumed on some paths" annotation).
//!  - If the use is `IntentKind::Consume` on a non-`BitCopy` type,
//!    transition to `Consumed(use_site)` after the read-check.
//!  - `BitCopy` uses do not transition the state.
//!
//! On `Return`: anchor — per-`@linear`-binding `MustConsume` check
//! runs against the exit-state of every `Terminator::Return` block.
//! If any `@linear` binding's state at a Return exit is `Live`,
//! `Consumed(s)` is fine, `MaybeConsumed(s)` is a hard error (was
//! consumed on some paths but not others reaching this exit).
//!
//! ## Fixpoint
//!
//! Worklist over `BlockId`. `entry[bb] = ⊓ exit[p] for p in visited_preds(bb)`.
//! Initial `entry[entry_block] = {}` (empty map = every binding
//! implicitly `Uninit`). CFGs may contain back-edges (while/for loop
//! bodies loop back to the header). The meet in `meet_predecessors`
//! skips unvisited predecessors (back-edges on the first pass) so that
//! bindings live before a loop header are not falsely flagged as
//! `InitialisedBeforeUse`. After the body block is processed its exit
//! state is recorded; the header is re-queued and the back-edge
//! contribution is included on subsequent visits. Convergence is
//! guaranteed because the binding-state lattice is finite and the meet
//! is monotone.

use std::collections::{BTreeMap, HashMap, HashSet, VecDeque};

use hew_hir::{BindingId, IntentKind, SiteId, TypeClassTable, ValueClass};
use hew_types::ResolvedTy;

use crate::model::{
    BasicBlock, CooperateKind, CooperateSite, Instr, MirCheck, MirStatement, Place, Terminator,
};

/// Per-binding state in the four-state lattice. `Uninit` is the
/// implicit default — a binding not present in the state map is
/// `Uninit` at that point.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BindingState {
    /// No `Bind` has been observed on any predecessor path that
    /// reaches this program point.
    Uninit,
    /// `Bind` observed on every predecessor path; the binding has
    /// not been consumed.
    Live,
    /// Consumed on every predecessor path; the carried site is the
    /// minimum (earliest) consume site over predecessors, for
    /// diagnostic anchoring.
    Consumed(SiteId),
    /// Consumed on some but not all predecessor paths reaching this
    /// point. Carries the earliest consume site for diagnostic
    /// anchoring.
    MaybeConsumed(SiteId),
    /// B1 — the binding was aliased into an aggregate (tuple) constructor on
    /// at least one predecessor path. It is STILL a live single owner for drop
    /// purposes (every drop reader treats this identically to `Live`), but a
    /// subsequent use is a use-after-move double-free (`(s, r); s.close()`) and
    /// is flagged `UseAfterConsume`. The carried site is the aggregate-construct
    /// site, used as the `consumed_at` anchor. Decoupling the use-check from the
    /// `Consumed` state is deliberate: `Consumed` suppresses the source's drop,
    /// which would break the alias/escape-scan drop machinery (W3.053).
    AliasedIntoAggregate(SiteId),
}

/// Meet over the four-state lattice. Commutative, associative,
/// idempotent — property-tested.
#[must_use]
pub fn meet(a: BindingState, b: BindingState) -> BindingState {
    use BindingState::AliasedIntoAggregate as Aliased;
    use BindingState::{Consumed, Live, MaybeConsumed, Uninit};
    // Order operands so the match table is half-size: handle (a, b)
    // and (b, a) via canonical ordering on the discriminant.
    let (lo, hi) = canonical_order(a, b);
    // The canonical ordering ensures `lo` ≤ `hi` on the
    // discriminant rank, so only the upper-triangle of the meet
    // table needs explicit arms. Cell semantics (Live⊓Consumed and
    // Consumed⊓MaybeConsumed both demote to MaybeConsumed with the
    // surviving consume site) are identical bodies by design — the
    // lattice's "any-path-consumed-but-not-all" projection.
    #[allow(
        clippy::match_same_arms,
        reason = "identical bodies are the lattice's MaybeConsumed projection"
    )]
    match (lo, hi) {
        (Uninit, _) => Uninit,
        (Live, Live) => Live,
        (Live, Consumed(s)) => MaybeConsumed(s),
        (Live, MaybeConsumed(s)) => MaybeConsumed(s),
        (Consumed(sa), Consumed(sb)) => Consumed(min_site(sa, sb)),
        (Consumed(sa), MaybeConsumed(sb)) => MaybeConsumed(min_site(sa, sb)),
        (MaybeConsumed(sa), MaybeConsumed(sb)) => MaybeConsumed(min_site(sa, sb)),
        // `AliasedIntoAggregate` is `Live` for every drop reader; it differs
        // only in flagging a later use. Self-meet keeps the marker (carrying
        // the earliest alias site); meeting with `Live` keeps it (the alias
        // survives the join). Meeting with `Consumed`/`MaybeConsumed` follows
        // the SAME `Live ⊓ {Consumed,MaybeConsumed} = MaybeConsumed` rule the
        // drop machinery already relies on (so no new drop behaviour), and the
        // resulting `MaybeConsumed` still flags a post-join use.
        (Aliased(sa), Aliased(sb)) => Aliased(min_site(sa, sb)),
        (Live, Aliased(s)) => Aliased(s),
        (Consumed(sa), Aliased(sb)) => MaybeConsumed(min_site(sa, sb)),
        (MaybeConsumed(sa), Aliased(sb)) => MaybeConsumed(min_site(sa, sb)),
        // The canonical ordering ensures `lo` ≤ `hi`; the remaining
        // mirrored arms are unreachable.
        _ => unreachable!("canonical_order ensures all reachable arms enumerated"),
    }
}

fn canonical_order(a: BindingState, b: BindingState) -> (BindingState, BindingState) {
    if discriminant_rank(a) <= discriminant_rank(b) {
        (a, b)
    } else {
        (b, a)
    }
}

fn discriminant_rank(s: BindingState) -> u8 {
    match s {
        BindingState::Uninit => 0,
        BindingState::Live => 1,
        BindingState::Consumed(_) => 2,
        BindingState::MaybeConsumed(_) => 3,
        BindingState::AliasedIntoAggregate(_) => 4,
    }
}

fn min_site(a: SiteId, b: SiteId) -> SiteId {
    if a.0 <= b.0 {
        a
    } else {
        b
    }
}

/// True for channel handle types (`Sender<T>` / `Receiver<T>`).
///
/// `#[opaque]`-only handles classify as `BitCopy` on the REPRESENTATION
/// axis (pointer-width, memcpy'd, no implicit drop), which normally
/// suppresses the consume transition below. Channel handles are still
/// single-owner on the OWNERSHIP axis: `close()` releases the underlying
/// resource and an actor-message transfer hands the pointer to the
/// receiving handler. A use after either consume races the new owner /
/// double-closes the channel, so explicit `Consume`-intent uses of these
/// types must transition to `Consumed` despite the `BitCopy` representation.
/// Read-intent uses (`send`/`recv`/`clone`, plain rebinds) are untouched.
fn is_channel_handle_ty(ty: &ResolvedTy) -> bool {
    matches!(
        ty,
        ResolvedTy::Named {
            builtin: Some(hew_types::BuiltinType::Sender | hew_types::BuiltinType::Receiver),
            ..
        }
    )
}

/// Forward-scan transfer function over one block's statements.
/// Emits `InitialisedBeforeUse` / `UseAfterConsume` checks as it
/// goes; returns the exit state for this block's terminator.
fn transfer_block(
    entry: BTreeMap<BindingId, BindingState>,
    block: &BasicBlock,
    type_classes: &TypeClassTable,
    linear_bindings: &mut BTreeMap<BindingId, (String, ResolvedTy)>,
    checks: &mut Vec<MirCheck>,
    use_after_consume_seen: &mut HashSet<(BindingId, SiteId)>,
    init_before_use_seen: &mut HashSet<(BindingId, SiteId)>,
) -> BTreeMap<BindingId, BindingState> {
    let mut state = entry;
    for statement in &block.statements {
        match statement {
            MirStatement::Bind {
                binding, name, ty, ..
            } => {
                state.insert(*binding, BindingState::Live);
                if ValueClass::of_ty(ty, type_classes) == ValueClass::Linear {
                    linear_bindings.insert(*binding, (name.clone(), ty.clone()));
                }
            }
            MirStatement::Use {
                binding,
                name,
                site,
                ty,
                intent,
            } => {
                let prior = state.get(binding).copied().unwrap_or(BindingState::Uninit);
                match prior {
                    BindingState::Uninit => {
                        // Deduplicate by (binding, use site) so the
                        // fixpoint's re-visits don't multiply
                        // diagnostics.
                        if init_before_use_seen.insert((*binding, *site)) {
                            checks.push(MirCheck::InitialisedBeforeUse {
                                binding: *binding,
                                name: name.clone(),
                                use_site: *site,
                            });
                        }
                    }
                    BindingState::Live => {}
                    BindingState::Consumed(consumed_at)
                    | BindingState::MaybeConsumed(consumed_at)
                    | BindingState::AliasedIntoAggregate(consumed_at) => {
                        if use_after_consume_seen.insert((*binding, *site)) {
                            checks.push(MirCheck::UseAfterConsume {
                                binding: *binding,
                                name: name.clone(),
                                consumed_at,
                                used_at: *site,
                            });
                        }
                    }
                }
                // A binding already aliased into a live aggregate keeps that
                // state: the consume transition below would suppress its drop
                // (the very breakage `AliasedIntoAggregate` exists to avoid),
                // and the use was already flagged. For any other prior state a
                // genuine `Consume` use transitions to `Consumed` as usual.
                if *intent == IntentKind::Consume
                    && (ValueClass::of_ty(ty, type_classes) != ValueClass::BitCopy
                        || is_channel_handle_ty(ty))
                    && !matches!(
                        state.get(binding),
                        Some(BindingState::AliasedIntoAggregate(_))
                    )
                {
                    state.insert(*binding, BindingState::Consumed(*site));
                }
            }
            MirStatement::AggregateAlias {
                binding,
                name,
                site,
                ..
            } => {
                match state.get(binding).copied() {
                    // The SAME owned handle placed into an aggregate twice
                    // (`(t, t)` / `(h, ..., h)`): the second placement is a
                    // use-after-move — both aggregate fields would free the one
                    // handle. Flag it and anchor at the first placement.
                    Some(BindingState::AliasedIntoAggregate(prev_site)) => {
                        if use_after_consume_seen.insert((*binding, *site)) {
                            checks.push(MirCheck::UseAfterConsume {
                                binding: *binding,
                                name: name.clone(),
                                consumed_at: prev_site,
                                used_at: *site,
                            });
                        }
                    }
                    // Mark the source aliased ONLY if it is currently a live
                    // owner (a fresh aggregate member always is). Aliasing is
                    // `Live` for every drop reader; it differs only in flagging
                    // a later use. A binding already `Consumed`/`MaybeConsumed`
                    // was caught by its own `Use` and is left untouched.
                    None | Some(BindingState::Live) => {
                        state.insert(*binding, BindingState::AliasedIntoAggregate(*site));
                    }
                    Some(
                        BindingState::Uninit
                        | BindingState::Consumed(_)
                        | BindingState::MaybeConsumed(_),
                    ) => {}
                }
            }
            MirStatement::Return { .. }
            | MirStatement::Evaluate { .. }
            | MirStatement::Drop { .. } => {}
        }
    }
    state
}

/// Merge multiple predecessor exit states into one entry state. For
/// each binding that appears in *any* predecessor, the result carries
/// the meet over predecessors — bindings missing from a predecessor
/// are treated as `Uninit` on that path.
fn meet_predecessors(
    preds: &[u32],
    exit_states: &HashMap<u32, BTreeMap<BindingId, BindingState>>,
    reachable: &HashSet<u32>,
) -> BTreeMap<BindingId, BindingState> {
    if preds.is_empty() {
        return BTreeMap::new();
    }
    // Only consider predecessors that have already been processed
    // (i.e., have an entry in `exit_states`). Unvisited predecessors
    // are back-edges (loop back-edges, specifically) that have not yet
    // established their exit state during the first worklist sweep.
    // Treating an unvisited predecessor as `Uninit` would cause a
    // false-positive `InitialisedBeforeUse` for bindings declared
    // before a loop header that are live on every acyclic path.
    //
    // The worklist fixpoint converges correctly because once the
    // body block is processed its exit state is recorded; on the
    // subsequent re-visit of the header the meet includes the body's
    // contribution. The `changed` guard (line above the `for succ`
    // loop) ensures re-visits only propagate when state actually
    // changes, so fixpoint terminates.
    // Only reachable, already-processed predecessors contribute. An
    // unreachable predecessor (no path from entry) never executes, so it
    // delivers no state — including it would let its empty exit state poison
    // the meet to `Uninit`.
    let visited_preds: Vec<u32> = preds
        .iter()
        .copied()
        .filter(|p| reachable.contains(p) && exit_states.contains_key(p))
        .collect();

    if visited_preds.is_empty() {
        // No visited predecessors yet (first visit of an unreachable
        // or not-yet-reached block). Return empty (implicitly Uninit).
        return BTreeMap::new();
    }

    let mut all_bindings: HashSet<BindingId> = HashSet::new();
    for p in &visited_preds {
        if let Some(s) = exit_states.get(p) {
            for k in s.keys() {
                all_bindings.insert(*k);
            }
        }
    }
    let mut entry = BTreeMap::new();
    for binding in all_bindings {
        // Meet across visited predecessors only. A binding absent from
        // a visited predecessor's exit state is `Uninit` on that path.
        // Order is deterministic on `preds` slice order; meet is
        // commutative + associative (property-tested), so any
        // permutation produces the same result.
        let acc = visited_preds
            .iter()
            .map(|p| {
                exit_states
                    .get(p)
                    .and_then(|m| m.get(&binding).copied())
                    .unwrap_or(BindingState::Uninit)
            })
            .reduce(meet)
            .unwrap_or(BindingState::Uninit);
        // Persist non-`Uninit` results only — `Uninit` is the
        // implicit default for absent map entries.
        if !matches!(acc, BindingState::Uninit) {
            entry.insert(binding, acc);
        }
    }
    entry
}

pub(crate) fn build_preds(blocks: &[BasicBlock]) -> HashMap<u32, Vec<u32>> {
    let mut preds: HashMap<u32, Vec<u32>> = HashMap::new();
    for block in blocks {
        let mut emit_edge = |target: u32| preds.entry(target).or_default().push(block.id);
        match &block.terminator {
            Terminator::Return | Terminator::Trap { .. } => {}
            Terminator::Goto { target } => emit_edge(*target),
            Terminator::Branch {
                then_target,
                else_target,
                ..
            } => {
                emit_edge(*then_target);
                emit_edge(*else_target);
            }
            Terminator::Call { next, .. }
            | Terminator::Yield { next, .. }
            | Terminator::MakeGenerator { next, .. }
            | Terminator::MakeLambdaActor { next, .. }
            | Terminator::Send { next, .. }
            | Terminator::Ask { next, .. }
            | Terminator::RemoteAsk { next, .. }
            | Terminator::Join { next, .. } => emit_edge(*next),
            // The runtime dispatch jumps to exactly one winning arm's
            // `body_block`; each body reaches `next` (the join) through its
            // own `Goto`. Without the body edges the arm bodies are
            // unreachable, their exit states are discarded by the
            // reachable-only meet, and any aggregate arm binding whose uses
            // span blocks inside a body trips a false `InitialisedBeforeUse`.
            // The direct `next` edge is kept as the conservative no-arm path
            // (`Join` has no body blocks — its branches converge in the
            // terminator itself).
            Terminator::Select { arms, next } => {
                for arm in arms {
                    emit_edge(arm.body_block);
                }
                emit_edge(*next);
            }
            // The suspending select's resume edge dispatches to exactly one
            // winning arm `body_block` (each body reaches `resume` — the join —
            // through its own `Goto`); cleanup is the abandon teardown edge. The
            // default suspend-return edge exits the function. Mirror the
            // `Select` arm-body edges plus the suspend resume/cleanup edges, or
            // the arm bodies are dead and an aggregate arm binding trips a false
            // `InitialisedBeforeUse`.
            Terminator::SuspendingSelect {
                arms,
                resume,
                cleanup,
            } => {
                for arm in arms {
                    emit_edge(arm.body_block);
                }
                emit_edge(*resume);
                emit_edge(*cleanup);
            }
            // Suspend's default edge exits the function (returns to the
            // executor); resume + cleanup are the in-CFG successor edges. The ten
            // collapsed suspension carriers all lower to this bare `Suspend`
            // (their distinguishing payload lives in the SuspendKind side-table,
            // which carries no CFG edge).
            Terminator::Suspend {
                resume, cleanup, ..
            } => {
                emit_edge(*resume);
                emit_edge(*cleanup);
            }
            Terminator::SuspendingScopeDeadline {
                timeout_body_block,
                resume,
                cleanup,
                ..
            } => {
                emit_edge(*timeout_body_block);
                emit_edge(*resume);
                emit_edge(*cleanup);
            }
        }
    }
    preds
}

pub(crate) fn successors(block: &BasicBlock) -> Vec<u32> {
    match &block.terminator {
        Terminator::Return | Terminator::Trap { .. } => Vec::new(),
        Terminator::Goto { target } => vec![*target],
        Terminator::Branch {
            then_target,
            else_target,
            ..
        } => vec![*then_target, *else_target],
        Terminator::Call { next, .. }
        | Terminator::Yield { next, .. }
        | Terminator::MakeGenerator { next, .. }
        | Terminator::MakeLambdaActor { next, .. }
        | Terminator::Send { next, .. }
        | Terminator::Ask { next, .. }
        | Terminator::RemoteAsk { next, .. }
        | Terminator::Join { next, .. } => vec![*next],
        // Arm body blocks are real runtime successors (the dispatch jumps to
        // the winning arm); `next` is kept as the conservative no-arm path.
        // Must mirror `build_preds` exactly or the worklist and the meet
        // disagree about the CFG.
        Terminator::Select { arms, next } => {
            let mut succs: Vec<u32> = arms.iter().map(|arm| arm.body_block).collect();
            succs.push(*next);
            succs
        }
        // Mirror `Select` (arm bodies are real runtime successors of the resume
        // dispatch) plus the suspend resume/cleanup edges. Must mirror
        // `build_preds` exactly or the worklist and the meet disagree.
        Terminator::SuspendingSelect {
            arms,
            resume,
            cleanup,
        } => {
            let mut succs: Vec<u32> = arms.iter().map(|arm| arm.body_block).collect();
            succs.push(*resume);
            succs.push(*cleanup);
            succs
        }
        // Suspend's default edge exits the function; resume + cleanup are the
        // in-CFG successors. The ten collapsed suspension carriers all lower to
        // this bare `Suspend` (their payload lives in the SuspendKind side-table,
        // which carries no CFG edge).
        Terminator::Suspend {
            resume, cleanup, ..
        } => vec![*resume, *cleanup],
        // The scope-deadline ramp's timeout-body block (deadline edge) is a real
        // runtime successor alongside resume (join + body convergence) and
        // cleanup; mirror `build_preds` exactly.
        Terminator::SuspendingScopeDeadline {
            timeout_body_block,
            resume,
            cleanup,
            ..
        } => vec![*timeout_body_block, *resume, *cleanup],
    }
}

/// Compute the reverse post-order (RPO) of blocks reachable from block 0.
///
/// RPO ensures that every block's dominators are processed before the
/// block itself — which means that on the first worklist sweep, all
/// "acyclic" predecessors of a block have already been processed before
/// the block is reached.  Back-edges (loop back-edges) remain unvisited
/// on the first sweep, which is correctly handled by the
/// `meet_predecessors` visited-only filter.
///
/// Unreachable blocks (not reachable from block 0) are appended at the
/// end in ID order so they still receive an exit-state entry.
/// Block IDs reachable from the entry block (id 0) along terminator edges.
/// Used to exclude unreachable (dangling) predecessors from the dataflow meet —
/// they never execute and must not contribute state.
pub(crate) fn reachable_from_entry(blocks: &[BasicBlock]) -> HashSet<u32> {
    let by_id: HashMap<u32, &BasicBlock> = blocks.iter().map(|b| (b.id, b)).collect();
    let mut visited: HashSet<u32> = HashSet::new();
    let mut stack: Vec<u32> = Vec::new();
    if by_id.contains_key(&0) {
        stack.push(0);
        visited.insert(0);
    }
    while let Some(cur) = stack.pop() {
        if let Some(block) = by_id.get(&cur) {
            for s in successors(block) {
                if visited.insert(s) {
                    stack.push(s);
                }
            }
        }
    }
    visited
}

pub(crate) fn compute_rpo(blocks: &[BasicBlock]) -> Vec<u32> {
    let by_id: HashMap<u32, &BasicBlock> = blocks.iter().map(|b| (b.id, b)).collect();
    let mut visited: HashSet<u32> = HashSet::new();
    let mut post_order: Vec<u32> = Vec::with_capacity(blocks.len());

    // Iterative DFS to avoid stack overflow on large CFGs.
    let mut stack: Vec<(u32, usize)> = Vec::new(); // (block_id, next_succ_index)
    if by_id.contains_key(&0) {
        stack.push((0, 0));
        visited.insert(0);
    }
    while let Some((cur_id, succ_idx)) = stack.last_mut() {
        let cur_id = *cur_id;
        let Some(block) = by_id.get(&cur_id) else {
            stack.pop();
            continue;
        };
        let succs = successors(block);
        if *succ_idx < succs.len() {
            let next = succs[*succ_idx];
            *succ_idx += 1;
            if visited.insert(next) {
                stack.push((next, 0));
            }
        } else {
            post_order.push(cur_id);
            stack.pop();
        }
    }

    // RPO = reverse of post-order.
    post_order.reverse();

    // Append unreachable blocks in ID order so they get exit_states entries.
    let all_ids: Vec<u32> = {
        let mut v: Vec<u32> = blocks.iter().map(|b| b.id).collect();
        v.sort_unstable();
        v
    };
    for id in all_ids {
        if !visited.contains(&id) {
            post_order.push(id);
        }
    }

    post_order
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
struct ContextFlowState {
    derived: HashSet<Place>,
    after_exit: bool,
}

impl ContextFlowState {
    fn meet(&self, other: &Self) -> Self {
        let mut derived = self.derived.clone();
        derived.extend(other.derived.iter().copied());
        Self {
            derived,
            after_exit: self.after_exit || other.after_exit,
        }
    }
}

#[allow(
    clippy::match_same_arms,
    clippy::too_many_lines,
    reason = "semantically distinct Instr variants share an extraction shape (e.g. \
              Move and EnumTagLoad both surface src→dest dataflow); merging arms by \
              pattern would obscure their distinct producer semantics"
)]
pub(crate) fn instr_reads_writes(instr: &Instr) -> (Vec<Place>, Vec<Place>) {
    match instr {
        Instr::EnterContext | Instr::ExitContext | Instr::CheckCancellation => (vec![], vec![]),
        Instr::ContextField { dest, .. }
        | Instr::ConstI64 { dest, .. }
        | Instr::StringLit { dest, .. }
        | Instr::BytesLit { dest, .. }
        | Instr::ConstGlobalLoad { dest, .. }
        | Instr::FloatLit { dest, .. }
        | Instr::CharLit { dest, .. }
        | Instr::UnitLit { dest }
        | Instr::DurationLit { dest, .. }
        | Instr::ActorStateFieldLoad { dest, .. } => (vec![], vec![*dest]),
        Instr::IntAdd { dest, lhs, rhs }
        | Instr::IntSub { dest, lhs, rhs }
        | Instr::IntMul { dest, lhs, rhs }
        | Instr::IntDiv { dest, lhs, rhs, .. }
        | Instr::IntRem { dest, lhs, rhs, .. }
        | Instr::IntBitAnd { dest, lhs, rhs }
        | Instr::IntBitOr { dest, lhs, rhs }
        | Instr::IntBitXor { dest, lhs, rhs }
        | Instr::IntShl { dest, lhs, rhs }
        | Instr::IntShr { dest, lhs, rhs, .. }
        | Instr::IntArithCheckedOption { dest, lhs, rhs, .. }
        | Instr::IntArithSaturating { dest, lhs, rhs, .. }
        | Instr::IntCmp { dest, lhs, rhs, .. }
        | Instr::FloatCmp { dest, lhs, rhs, .. }
        | Instr::IdentityCompare { dest, lhs, rhs }
        | Instr::FloatAdd { dest, lhs, rhs, .. }
        | Instr::FloatSub { dest, lhs, rhs, .. }
        | Instr::FloatMul { dest, lhs, rhs, .. }
        | Instr::FloatDiv { dest, lhs, rhs, .. }
        | Instr::FloatRem { dest, lhs, rhs, .. } => (vec![*lhs, *rhs], vec![*dest]),
        Instr::CancellationTokenIsCancelled { dest, token } => (vec![*token], vec![*dest]),
        Instr::GeneratorNext { dest, ctx, .. } => (vec![*ctx], vec![*dest]),
        Instr::WireCodec { dest, operand, .. } => (vec![*operand], vec![*dest]),
        Instr::RecordCloneInplace { dest, src, .. } => (vec![*src], vec![*dest]),
        Instr::BoolNot { dest, operand }
        | Instr::FloatNeg { dest, operand, .. }
        | Instr::IntBitNot { dest, operand } => (vec![*operand], vec![*dest]),
        Instr::NumericCast { dest, src, .. } | Instr::SaturatingWidthCast { dest, src, .. } => {
            (vec![*src], vec![*dest])
        }
        Instr::IntNegChecked {
            dest,
            operand,
            overflow_flag,
            ..
        } => (vec![*operand], vec![*dest, *overflow_flag]),
        Instr::IntArithChecked {
            dest,
            lhs,
            rhs,
            overflow_flag,
            ..
        } => (vec![*lhs, *rhs], vec![*dest, *overflow_flag]),
        Instr::Move { dest, src } => (vec![*src], vec![*dest]),
        Instr::CallRuntimeAbi(call) => {
            let reads = call.args().to_vec();
            let writes = call.dest().into_iter().collect();
            (reads, writes)
        }
        Instr::AutoLockAcquire { lock } | Instr::AutoLockRelease { lock } => {
            // The lock pointer is read (its address is passed to the
            // runtime FFI). No place is written — the FFI mutates the
            // pointee, which is opaque to the MIR dataflow.
            (vec![*lock], vec![])
        }
        Instr::CallClosure {
            callee, args, dest, ..
        } => {
            let mut reads = args.clone();
            reads.insert(0, *callee);
            let writes = dest.iter().copied().collect();
            (reads, writes)
        }
        Instr::MakeClosure { env, dest, .. } | Instr::ClosureEnvFieldLoad { env, dest, .. } => {
            (vec![*env], vec![*dest])
        }
        Instr::SpawnTaskDirect { task, .. } => (vec![*task], vec![]),
        Instr::SpawnTaskClosure { task, env, .. } => (vec![*task, *env], vec![]),
        Instr::Drop { place, .. } => (vec![*place], vec![]),
        Instr::WitnessSizeOf { dest, .. } | Instr::WitnessAlignOf { dest, .. } => {
            (vec![], vec![*dest])
        }
        Instr::WitnessDropGlue { place, .. } => (vec![*place], vec![]),
        Instr::WitnessMove { dest, src, .. } => (vec![*src], vec![*dest]),
        Instr::RecordInit { fields, dest, .. } => {
            let reads = fields.iter().map(|(_, place)| *place).collect();
            (reads, vec![*dest])
        }
        Instr::RecordFieldLoad { record, dest, .. } => (vec![*record], vec![*dest]),
        Instr::RecordFieldStore { record, src, .. } => {
            // Field-store reads both the aggregate (to GEP into it) and
            // the source. The aggregate stays Live — only the field bytes
            // are overwritten; ownership of the surrounding record does
            // not transfer. Returning the record as a read (not a write)
            // is what keeps the dataflow lattice in the `Live` state for
            // it after the store. See `Iterator::next(var self)` in
            // `std/builtins.hew` for the load-bearing consumer (the
            // mutable-receiver substrate).
            (vec![*record, *src], vec![])
        }
        Instr::ActorStateFieldStore { src, .. } => (vec![*src], vec![]),
        Instr::TupleFieldLoad { tuple, dest, .. } => (vec![*tuple], vec![*dest]),
        Instr::TupleConstruct { elements, dest } => (elements.clone(), vec![*dest]),
        Instr::SpawnActor {
            state,
            init_args,
            dest,
            ..
        } => {
            let mut reads: Vec<_> = state.iter().copied().collect();
            reads.extend(init_args.iter().copied());
            (reads, vec![*dest])
        }
        Instr::CoerceToDynTrait { value, dest, .. } => (vec![*value], vec![*dest]),
        Instr::CallTraitMethod {
            fat_pointer,
            args,
            dest,
            ..
        } => {
            let mut reads = Vec::with_capacity(args.len().saturating_add(1));
            reads.push(*fat_pointer);
            reads.extend(args.iter().copied());
            let writes = dest.iter().copied().collect();
            (reads, writes)
        }
        Instr::MachineEmitPlaceholder { payload, .. } => {
            // The placeholder reads all payload places; no write destination
            // (emit is void — the result is dispatched to the event queue).
            (payload.clone(), vec![])
        }
        Instr::EnumTagLoad { src, dest } => (vec![*src], vec![*dest]),
        Instr::MachineStateName {
            src_local, dest, ..
        } => (vec![Place::Local(*src_local)], vec![*dest]),
    }
}

fn transfer_context_flow(
    mut state: ContextFlowState,
    block: &BasicBlock,
    checks: &mut Vec<MirCheck>,
    seen: &mut HashSet<(Place, u32)>,
) -> ContextFlowState {
    for instr in &block.instructions {
        match instr {
            Instr::EnterContext => {
                state.after_exit = false;
            }
            Instr::ExitContext => {
                if state.derived.contains(&Place::ReturnSlot)
                    && seen.insert((Place::ReturnSlot, block.id))
                {
                    checks.push(MirCheck::ContextBindingEscapes {
                        place: Place::ReturnSlot,
                        block: block.id,
                    });
                }
                state.after_exit = true;
            }
            Instr::ContextField { dest, .. } => {
                if state.after_exit && seen.insert((*dest, block.id)) {
                    checks.push(MirCheck::ContextBindingEscapes {
                        place: *dest,
                        block: block.id,
                    });
                }
                state.derived.insert(*dest);
            }
            _ => {
                let (reads, writes) = instr_reads_writes(instr);
                let reads_context = reads.iter().any(|place| state.derived.contains(place));
                if state.after_exit && reads_context {
                    if let Some(place) = reads
                        .iter()
                        .copied()
                        .find(|place| state.derived.contains(place))
                    {
                        if seen.insert((place, block.id)) {
                            checks.push(MirCheck::ContextBindingEscapes {
                                place,
                                block: block.id,
                            });
                        }
                    }
                }
                for dest in writes {
                    if reads_context {
                        state.derived.insert(dest);
                    } else {
                        state.derived.remove(&dest);
                    }
                }
            }
        }
    }
    state
}

fn check_context_flow(blocks: &[BasicBlock]) -> Vec<MirCheck> {
    if blocks.is_empty() {
        return Vec::new();
    }
    let by_id: HashMap<u32, &BasicBlock> = blocks.iter().map(|b| (b.id, b)).collect();
    let entry_id = 0;
    let mut entry_states: HashMap<u32, ContextFlowState> = HashMap::new();
    let mut exit_states: HashMap<u32, ContextFlowState> = HashMap::new();
    let mut checks = Vec::new();
    let mut seen: HashSet<(Place, u32)> = HashSet::new();
    let mut worklist: VecDeque<u32> = VecDeque::from([entry_id]);
    entry_states.insert(entry_id, ContextFlowState::default());

    while let Some(cur_id) = worklist.pop_front() {
        let Some(block) = by_id.get(&cur_id).copied() else {
            continue;
        };
        let entry = entry_states.get(&cur_id).cloned().unwrap_or_default();
        let exit = transfer_context_flow(entry, block, &mut checks, &mut seen);
        let changed = exit_states.get(&cur_id) != Some(&exit);
        exit_states.insert(cur_id, exit.clone());
        if changed {
            for succ in successors(block) {
                let next = entry_states
                    .get(&succ)
                    .map_or_else(|| exit.clone(), |prev| prev.meet(&exit));
                if entry_states.get(&succ) != Some(&next) {
                    entry_states.insert(succ, next);
                    worklist.push_back(succ);
                }
            }
        }
    }

    checks
}

/// Run the per-block move-checker over a function's CFG. Emits the
/// `InitialisedBeforeUse` / `UseAfterConsume` / `MustConsume` checks
/// derived from the four-state lattice.
/// Result of the dataflow analysis. `checks` mirror what
/// `check_blocks` returns; `exit_states[bb]` is the per-binding
/// state map at each block's terminator — the elaborator consumes
/// it to derive per-`Return`-exit live sets for drop planning.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct DataflowResult {
    pub checks: Vec<MirCheck>,
    pub exit_states: HashMap<u32, BTreeMap<BindingId, BindingState>>,
}

#[must_use]
pub fn check_blocks(blocks: &[BasicBlock], type_classes: &TypeClassTable) -> Vec<MirCheck> {
    analyze(blocks, type_classes, &[]).checks
}

/// Run the full dataflow pass and return both diagnostics and the
/// per-block exit-state map. The elaborator uses `exit_states` to
/// filter the function-wide LIFO drop sequence down to per-exit
/// live sets (plan §5 Slice 4: "per-exit drop list with Place
/// threading").
///
/// `param_bindings` is the list of function parameter `BindingId`s that are
/// implicitly `Live` at function entry (supplied by the calling convention;
/// never produced by a `Bind` statement). These are seeded as `Live` in the
/// entry block's initial state so the dataflow checker does not flag uses of
/// parameters as `InitialisedBeforeUse`. An empty slice is correct for
/// zero-parameter functions and for hand-built test pipelines where no
/// parameters exist.
#[must_use]
#[allow(
    clippy::too_many_lines,
    reason = "two-phase dataflow (fixpoint + diagnostic sweep); splitting would require shared mutable state across functions"
)]
pub fn analyze(
    blocks: &[BasicBlock],
    type_classes: &TypeClassTable,
    param_bindings: &[BindingId],
) -> DataflowResult {
    if blocks.is_empty() {
        return DataflowResult::default();
    }
    let preds = build_preds(blocks);
    let by_id: HashMap<u32, &BasicBlock> = blocks.iter().map(|b| (b.id, b)).collect();

    // Blocks reachable from the entry. Unreachable blocks (lowering can emit
    // empty `goto`-only blocks with no predecessors that target a join block —
    // e.g. the continuation after an `if { return }`) must NOT contribute to a
    // join block's meet: they never execute, so they deliver no state. Without
    // this filter, such a dangling pred's empty exit state makes the meet treat
    // every live binding (params included) as `Uninit`, producing a
    // false-positive `InitialisedBeforeUse` at the join.
    let reachable = reachable_from_entry(blocks);

    // The function's entry block is id 0 by construction (see
    // `lower::Builder::finalize_blocks`).
    let entry_id = 0;

    // ── Phase 1: fixpoint ──────────────────────────────────────────────
    //
    // Compute per-block exit states to fixpoint WITHOUT emitting
    // diagnostics. Separating state propagation from diagnostic emission
    // prevents false-positive `InitialisedBeforeUse` reports for blocks
    // whose predecessor appears later in block-ID order (which happens
    // when a checked-arithmetic expression inside a while condition
    // allocates new blocks after `body_bb` in ID space, making
    // `body_bb`'s predecessor ID > `body_bb`'s own ID).
    //
    // During fixpoint, every block that is processed before some of its
    // predecessors would see an incomplete entry state. Deferring
    // diagnostics to Phase 2 — after all exit states are stable — means
    // we only evaluate `Use` nodes against states that reflect every
    // reachable path.
    let mut exit_states: HashMap<u32, BTreeMap<BindingId, BindingState>> = HashMap::new();
    let mut linear_bindings: BTreeMap<BindingId, (String, ResolvedTy)> = BTreeMap::new();

    // Worklist seeded in Reverse Post-Order (RPO). RPO ensures every
    // block's dominators on the acyclic spanning tree are processed
    // before the block itself. This means that on the first worklist
    // sweep, all "forward-edge" predecessors have been visited before
    // the block is processed — so `meet_predecessors`'s visited-only
    // filter produces a sound entry state without skipping any path that
    // could genuinely deliver `Uninit`. Only true back-edges (loop
    // back-edges) are skipped on the first sweep; those get picked up
    // on subsequent re-visits when the fixpoint propagates the body's
    // exit state back through the back-edge.
    let rpo = compute_rpo(blocks);
    let mut worklist: VecDeque<u32> = rpo.into_iter().collect();

    while let Some(cur_id) = worklist.pop_front() {
        let Some(block) = by_id.get(&cur_id) else {
            continue;
        };
        let entry = if cur_id == entry_id {
            // Seed parameters as `Live` at function entry. Parameters are
            // initialised by the calling convention (their values arrive via
            // LLVM function arguments + the parameter prologue in codegen);
            // they never appear as `Bind` statements in the checker-authority
            // stream. Without this seeding the dataflow would flag every use
            // of a parameter as `InitialisedBeforeUse`.
            let mut entry_state: BTreeMap<BindingId, BindingState> = BTreeMap::new();
            for &id in param_bindings {
                entry_state.insert(id, BindingState::Live);
            }
            entry_state
        } else {
            let empty = Vec::new();
            let preds_of_bb = preds.get(&cur_id).unwrap_or(&empty);
            // Phase 1 uses the visited-only meet so back-edges don't
            // contribute `Uninit` before they are processed.
            meet_predecessors(preds_of_bb, &exit_states, &reachable)
        };
        // In Phase 1 we only propagate state — diagnostics are discarded.
        let mut phase1_checks: Vec<MirCheck> = Vec::new();
        let mut phase1_use_seen: HashSet<(BindingId, SiteId)> = HashSet::new();
        let mut phase1_init_seen: HashSet<(BindingId, SiteId)> = HashSet::new();
        let new_exit = transfer_block(
            entry,
            block,
            type_classes,
            &mut linear_bindings,
            &mut phase1_checks,
            &mut phase1_use_seen,
            &mut phase1_init_seen,
        );
        drop(phase1_checks);
        drop(phase1_use_seen);
        drop(phase1_init_seen);
        let changed = exit_states
            .get(&cur_id)
            .is_none_or(|prev| *prev != new_exit);
        exit_states.insert(cur_id, new_exit);
        if changed {
            for succ in successors(block) {
                worklist.push_back(succ);
            }
        }
    }

    // ── Phase 2: diagnostic sweep ─────────────────────────────────────
    //
    // Now that exit_states is stable (fixpoint reached), do one more
    // pass over every reachable block in ID order to collect diagnostics
    // using the correct, fully-converged entry states. This guarantees
    // that every `Use` is checked against the state that reflects all
    // predecessor paths — including loop back-edges.
    let mut checks: Vec<MirCheck> = Vec::new();
    let mut use_after_consume_seen: HashSet<(BindingId, SiteId)> = HashSet::new();
    let mut init_before_use_seen: HashSet<(BindingId, SiteId)> = HashSet::new();
    // Reset linear_bindings for the diagnostic pass (Phase 1 populated it
    // as a side-effect; resetting avoids double-registration).
    linear_bindings.clear();

    for block in blocks {
        let blk_id = block.id;
        let entry = if blk_id == entry_id {
            let mut entry_state: BTreeMap<BindingId, BindingState> = BTreeMap::new();
            for &id in param_bindings {
                entry_state.insert(id, BindingState::Live);
            }
            entry_state
        } else {
            let empty = Vec::new();
            let preds_of_bb = preds.get(&blk_id).unwrap_or(&empty);
            // Phase 2 uses ALL predecessors (all are now in exit_states).
            meet_predecessors(preds_of_bb, &exit_states, &reachable)
        };
        transfer_block(
            entry,
            block,
            type_classes,
            &mut linear_bindings,
            &mut checks,
            &mut use_after_consume_seen,
            &mut init_before_use_seen,
        );
        // We don't update exit_states in Phase 2 — they're already
        // stable from Phase 1 and we're only collecting diagnostics.
    }

    // Per-exit MustConsume + MaybeConsumed-at-Return error. For every
    // `Terminator::Return`-terminated block, scan its exit state for
    // `@linear` bindings:
    //   - Consumed(_)        → OK
    //   - Live               → MustConsume at this exit
    //   - MaybeConsumed(s)   → MustConsume at this exit (anchored
    //                          at the consume site `s`, semantically
    //                          "may not have been consumed on every
    //                          path reaching this return"). Today's
    //                          MirCheck::MustConsume payload doesn't
    //                          distinguish the cases — a future
    //                          diagnostic polish cluster widens it.
    //   - Uninit             → impossible for a binding registered in
    //                          `linear_bindings`; that registration
    //                          only happens at a Bind site, which
    //                          would have transitioned the state to
    //                          Live on that block's path. Defensive.
    let mut must_consume_seen: HashSet<(BindingId, u32)> = HashSet::new();
    for block in blocks {
        let Terminator::Return = &block.terminator else {
            continue;
        };
        // The Return's anchor site is read off the block's last
        // `MirStatement::Return` entry (the lowering pushes one such
        // entry per Return-terminated block right before sealing).
        let exit_site = block
            .statements
            .iter()
            .rev()
            .find_map(|s| match s {
                MirStatement::Return { site, .. } => Some(site.unwrap_or(SiteId(0))),
                _ => None,
            })
            .unwrap_or(SiteId(0));
        let Some(exit_state) = exit_states.get(&block.id) else {
            continue;
        };
        for (binding, state) in exit_state {
            let Some((name, ty)) = linear_bindings.get(binding) else {
                continue;
            };
            let needs_report = matches!(
                state,
                BindingState::Live
                    | BindingState::MaybeConsumed(_)
                    | BindingState::Uninit
                    | BindingState::AliasedIntoAggregate(_)
            );
            if needs_report && must_consume_seen.insert((*binding, block.id)) {
                checks.push(MirCheck::MustConsume {
                    binding: *binding,
                    name: name.clone(),
                    exit_site,
                    ty: ty.clone(),
                });
            }
        }
        // A `@linear` binding may have been registered on some path
        // but never reach this block's exit_state at all — that means
        // it never lived along any predecessor of this Return, which
        // is structurally impossible for an entry-rooted CFG with a
        // single function-body. Defensive: if there is no exit-state
        // entry for the binding here, it was Uninit on every path,
        // and Uninit-at-Return doesn't need a separate diagnostic
        // (the Bind never happened on any path reaching the exit).
    }

    checks.extend(check_context_flow(blocks));

    DataflowResult {
        checks,
        exit_states,
    }
}

// ---------- Cooperate-site analysis ----------

/// Threshold for the leaf-function heuristic. Functions with fewer than
/// this many total `MirStatement` entries across all blocks, no call
/// instructions, and no back-edges are classified as leaves and receive
/// no cooperate-check site.
///
/// WHY 10: matches the skip-eligibility threshold ("≤ N … Threshold: 8
/// instructions"). We use 10 MIR-statement entries (the checker-
/// authority stream) as the observable boundary; the backend `Instr`
/// count is checked separately for call instructions. The value is a
/// first-cut conservative estimate — a future performance lane can lower
/// the threshold if benchmarks show overhead from injecting into short
/// non-leaf functions.
///
/// WHEN-OBSOLETE: a future tuning pass may expose this as a named
/// constant the caller passes in so integration tests can override it.
const LEAF_STATEMENT_THRESHOLD: usize = 10;

/// Determine whether a block has a back-edge terminator — i.e., a
/// `Goto` whose target block id is less than the current block's id.
///
/// WHY `target < block.id`: The MIR lowering emits blocks in
/// monotonically increasing id order, with entry block id = 0. A
/// forward edge always targets a block with a higher id than the
/// source; a back-edge (loop) targets an earlier block. The invariant
/// holds for all CFGs the v0.5 lowering constructs — forward Gotos
/// target the join block (always allocated after the arm blocks).
/// Synthetic test CFGs must respect this convention.
///
/// WHEN-OBSOLETE: if a future lowering phase emits blocks in non-
/// topological order, replace this predicate with a full DFS-ancestry
/// check using a discovered DFS tree over the CFG.
fn is_back_edge_goto(block: &BasicBlock) -> bool {
    match block.terminator {
        Terminator::Goto { target } => target < block.id,
        _ => false,
    }
}

/// Return true if a block contains a call — `Instr::CallRuntimeAbi` or
/// `Terminator::Call`.
///
/// Used by the leaf-function heuristic: a function that calls other
/// functions is not a leaf and cannot be skipped.
fn block_has_call(block: &BasicBlock) -> bool {
    let instr_has_call = block
        .instructions
        .iter()
        .any(|i| matches!(i, Instr::CallRuntimeAbi(_)));
    let terminator_is_call = matches!(block.terminator, Terminator::Call { .. });
    instr_has_call || terminator_is_call
}

/// Classify whether a function is a short leaf that should receive no
/// cooperate-check site.
///
/// A function is a leaf when ALL of the following hold:
/// 1. Total `MirStatement` count across all blocks is < `LEAF_STATEMENT_THRESHOLD`.
/// 2. No block contains a `CallRuntimeAbi` or `Terminator::Call`.
/// 3. No block has a back-edge `Goto` (no loops).
///
/// WHY factor this out: the future `#[no_reductions_check]` attribute and
/// the receive-handler skip will hook into this predicate. Keeping it as
/// a named function makes the extension point visible without requiring an
/// `Instr` variant or a new MIR pass.
///
/// WHEN-OBSOLETE: once a caller-supplied eligibility override (attribute
/// flag) is wired, that gates this check entirely.
fn is_leaf_function(blocks: &[BasicBlock]) -> bool {
    let total_statements: usize = blocks.iter().map(|b| b.statements.len()).sum();
    if total_statements >= LEAF_STATEMENT_THRESHOLD {
        return false;
    }
    let has_call = blocks.iter().any(block_has_call);
    if has_call {
        return false;
    }
    let has_back_edge = blocks.iter().any(is_back_edge_goto);
    if has_back_edge {
        return false;
    }
    true
}

/// Yield-equivalent terminators already cause the actor to surrender the
/// scheduler; no cooperate call is needed before them.
///
/// Returns `true` when a block's terminator is a yield-equivalent:
/// `Yield`, `Send`, `Ask`, or `Select`. `Terminator::Call` is NOT
/// yield-equivalent — it is a synchronous function call.
///
/// WHY checked per-block: the suppression rule ("don't add a cooperate
/// site whose entry terminator is yield-equivalent") applies at the
/// function-entry block only in theory (a function that immediately
/// yields doesn't need a prologue cooperate). In practice the v0.5 spine
/// never constructs these terminators, so the suppressor is a no-op; it
/// is factored out so codegen can extend it without touching the main logic.
fn is_yield_equivalent(block: &BasicBlock) -> bool {
    matches!(
        block.terminator,
        Terminator::Yield { .. }
            | Terminator::Send { .. }
            | Terminator::Ask { .. }
            | Terminator::RemoteAsk { .. }
            | Terminator::Select { .. }
            | Terminator::Join { .. }
    )
}

/// Compute the cooperate-check sites for a function.
///
/// Returns a `Vec<CooperateSite>` that codegen injects
/// `call @hew_actor_cooperate()` at. Empty means no injection is
/// needed (leaf function or yield-equivalent first block).
///
/// ## Algorithm
///
/// 1. **Leaf check**: if the function is a short leaf (`is_leaf_function`),
///    return an empty vec.
/// 2. **Function-entry site**: add a `FunctionEntry` site for block 0
///    unless that block's terminator is yield-equivalent (the actor
///    will cooperate via the yield anyway).
/// 3. **Back-edge sweep**: for every block whose `Goto` target id is
///    less than the block's own id, add a `LoopBackEdge` site — unless
///    the back-edge target block itself has a yield-equivalent
///    terminator (the loop header yields on every iteration).
///
/// ## Skip-eligibility extension point
///
/// This function does NOT implement the `#[no_reductions_check]`
/// attribute or the receive-handler skip. Both are wired via the same
/// return-empty-vec pattern: before calling `compute_cooperate_sites`,
/// the caller checks its eligibility predicate and skips the call if
/// ineligible. This keeps the analysis pure and testable in isolation.
///
/// ## Loop back-edges (v0.5)
///
/// Loop lowering has constructed production back-edges since `8d878b8e`.
/// `LoopBackEdge` sites are live for `for`, `while`, and `loop` bodies:
/// a back-edge `Goto` is detected by `is_back_edge_goto` and receives a
/// cooperate check before control returns to the loop header.
#[must_use]
pub fn compute_cooperate_sites(blocks: &[BasicBlock]) -> Vec<CooperateSite> {
    if blocks.is_empty() || is_leaf_function(blocks) {
        return Vec::new();
    }

    let mut sites: Vec<CooperateSite> = Vec::new();

    // Function-entry site: block 0, unless its terminator already yields.
    let entry_block = &blocks[0];
    if !is_yield_equivalent(entry_block) {
        sites.push(CooperateSite {
            bb_id: 0,
            kind: CooperateKind::FunctionEntry,
        });
    }

    // Back-edge sites: every block whose Goto targets an earlier block.
    // Suppress if the loop-header block itself has a yield-equivalent
    // terminator (the actor already cooperates at the loop header).
    let by_id: HashMap<u32, &BasicBlock> = blocks.iter().map(|b| (b.id, b)).collect();
    for block in blocks {
        if let Terminator::Goto { target } = block.terminator {
            if target < block.id {
                // Back-edge found. Check whether the loop-header block
                // itself is yield-equivalent — if so, no cooperate needed.
                let header_yields = by_id.get(&target).is_some_and(|h| is_yield_equivalent(h));
                if !header_yields {
                    sites.push(CooperateSite {
                        bb_id: block.id,
                        kind: CooperateKind::LoopBackEdge,
                    });
                }
            }
        }
    }

    sites
}

// ---------- Property tests for the lattice ----------

#[cfg(test)]
mod tests {
    use super::*;

    fn states() -> Vec<BindingState> {
        vec![
            BindingState::Uninit,
            BindingState::Live,
            BindingState::Consumed(SiteId(3)),
            BindingState::Consumed(SiteId(7)),
            BindingState::MaybeConsumed(SiteId(3)),
            BindingState::MaybeConsumed(SiteId(7)),
        ]
    }

    /// Wider exhaustive state-space for the M2 substrate's drop-plan
    /// invariants. Includes multiple consume sites with non-trivial
    /// ordering (1, 3, 7, 11) so the min-site rule for
    /// Consumed/MaybeConsumed meets is exercised at every pair.
    /// Property tests below sample every (state × state) and every
    /// (state × state × state) tuple — the lattice has 9 elements
    /// (Uninit + Live + 4×Consumed + 4×MaybeConsumed = 1+1+4+4 = 10),
    /// so the exhaustive cube is 1000 tuples; fast enough to keep in
    /// CI per the existing pattern.
    fn states_wide() -> Vec<BindingState> {
        vec![
            BindingState::Uninit,
            BindingState::Live,
            BindingState::Consumed(SiteId(1)),
            BindingState::Consumed(SiteId(3)),
            BindingState::Consumed(SiteId(7)),
            BindingState::Consumed(SiteId(11)),
            BindingState::MaybeConsumed(SiteId(1)),
            BindingState::MaybeConsumed(SiteId(3)),
            BindingState::MaybeConsumed(SiteId(7)),
            BindingState::MaybeConsumed(SiteId(11)),
        ]
    }

    #[test]
    fn meet_is_commutative() {
        for a in states() {
            for b in states() {
                assert_eq!(
                    meet(a, b),
                    meet(b, a),
                    "meet({a:?}, {b:?}) != meet({b:?}, {a:?})"
                );
            }
        }
    }

    #[test]
    fn meet_is_associative() {
        for a in states() {
            for b in states() {
                for c in states() {
                    let lhs = meet(meet(a, b), c);
                    let rhs = meet(a, meet(b, c));
                    assert_eq!(
                        lhs, rhs,
                        "associativity broke on ({a:?}, {b:?}, {c:?}): \
                         meet(meet(a,b),c)={lhs:?} meet(a,meet(b,c))={rhs:?}"
                    );
                }
            }
        }
    }

    #[test]
    fn meet_is_idempotent() {
        for a in states() {
            assert_eq!(meet(a, a), a, "meet({a:?}, {a:?}) != {a:?}");
        }
    }

    #[test]
    fn meet_uninit_dominates() {
        // Uninit ⊓ anything = Uninit. The most-conservative rule —
        // a path that didn't initialise the binding makes it Uninit
        // at the join even if the other path went all the way to
        // Consumed.
        for b in states() {
            assert_eq!(meet(BindingState::Uninit, b), BindingState::Uninit);
            assert_eq!(meet(b, BindingState::Uninit), BindingState::Uninit);
        }
    }

    #[test]
    fn meet_live_vs_consumed_becomes_maybeconsumed() {
        let s = SiteId(42);
        assert_eq!(
            meet(BindingState::Live, BindingState::Consumed(s)),
            BindingState::MaybeConsumed(s)
        );
        assert_eq!(
            meet(BindingState::Consumed(s), BindingState::Live),
            BindingState::MaybeConsumed(s)
        );
    }

    // ---------- Slice 3 (M2 substrate) lattice property pins ----------
    //
    // The M2 unified-concurrency substrate's per-Return drop-plan
    // narrowing depends on the meet lattice for its correctness: each
    // Duplex / lambda-actor handle's drop fires only when its state at
    // the Return is `Live` (the binding is still owned) or
    // `MaybeConsumed(_)` (the move-checker rejects upstream, but the
    // elaborator treats it as still-needing-a-drop for graceful
    // failure). A breaking change to the meet semantics would silently
    // shift which drops emit at each Return — exactly the
    // `cleanup-all-exits` invariant we cannot regress.
    //
    // These tests pin the slice-3 invariants over a wider state space
    // (4 consume sites instead of 2) so the min-site rule for
    // Consumed/MaybeConsumed pairs is exercised at every ordering.

    #[test]
    fn meet_is_commutative_on_wide_state_space() {
        for a in states_wide() {
            for b in states_wide() {
                assert_eq!(
                    meet(a, b),
                    meet(b, a),
                    "commutativity broke on ({a:?}, {b:?})"
                );
            }
        }
    }

    #[test]
    fn meet_is_associative_on_wide_state_space() {
        for a in states_wide() {
            for b in states_wide() {
                for c in states_wide() {
                    let lhs = meet(meet(a, b), c);
                    let rhs = meet(a, meet(b, c));
                    assert_eq!(lhs, rhs, "associativity broke on ({a:?}, {b:?}, {c:?})");
                }
            }
        }
    }

    #[test]
    fn meet_is_idempotent_on_wide_state_space() {
        for a in states_wide() {
            assert_eq!(meet(a, a), a, "idempotence broke on {a:?}");
        }
    }

    #[test]
    fn meet_consumed_pair_picks_min_site_over_wide_space() {
        // For every Consumed(a) ⊓ Consumed(b) pair, the result is
        // Consumed(min(a, b)). The min-site rule is the diagnostic
        // anchor for "earliest consume site reaching this join" —
        // pinning it across a wider site space catches a stray
        // max-site or any-site implementation drift.
        for a_site in [1u32, 3, 7, 11] {
            for b_site in [1u32, 3, 7, 11] {
                let a = BindingState::Consumed(SiteId(a_site));
                let b = BindingState::Consumed(SiteId(b_site));
                let result = meet(a, b);
                let expected_min = a_site.min(b_site);
                assert_eq!(
                    result,
                    BindingState::Consumed(SiteId(expected_min)),
                    "Consumed({a_site}) ⊓ Consumed({b_site}) should be Consumed({expected_min})"
                );
            }
        }
    }

    #[test]
    fn meet_maybe_consumed_pair_picks_min_site_over_wide_space() {
        // Same min-site rule for MaybeConsumed ⊓ MaybeConsumed.
        for a_site in [1u32, 3, 7, 11] {
            for b_site in [1u32, 3, 7, 11] {
                let a = BindingState::MaybeConsumed(SiteId(a_site));
                let b = BindingState::MaybeConsumed(SiteId(b_site));
                let result = meet(a, b);
                let expected_min = a_site.min(b_site);
                assert_eq!(
                    result,
                    BindingState::MaybeConsumed(SiteId(expected_min)),
                    "MaybeConsumed({a_site}) ⊓ MaybeConsumed({b_site}) should be MaybeConsumed({expected_min})"
                );
            }
        }
    }

    #[test]
    fn meet_consumed_vs_maybe_consumed_demotes_and_picks_min_site() {
        // Consumed ⊓ MaybeConsumed = MaybeConsumed(min(a, b)). The
        // "any-path-not-consumed" projection demotes Consumed to
        // MaybeConsumed; the carried site is still the earliest for
        // diagnostic anchoring. Wide-space pin.
        for c_site in [1u32, 3, 7, 11] {
            for m_site in [1u32, 3, 7, 11] {
                let c = BindingState::Consumed(SiteId(c_site));
                let m = BindingState::MaybeConsumed(SiteId(m_site));
                let result = meet(c, m);
                let expected_min = c_site.min(m_site);
                assert_eq!(
                    result,
                    BindingState::MaybeConsumed(SiteId(expected_min)),
                    "Consumed({c_site}) ⊓ MaybeConsumed({m_site}) should be MaybeConsumed({expected_min})"
                );
            }
        }
    }

    #[test]
    fn meet_live_demotes_consumed_pair_to_maybe_consumed() {
        // Three-way meet: Live ⊓ Consumed(a) ⊓ Consumed(b) =
        // MaybeConsumed(min(a, b)). This is the canonical "binding
        // was consumed on two paths and live on a third" shape —
        // the M2 substrate's per-Return drop plan must observe this
        // as MaybeConsumed so the drop still fires for the live-path
        // case.
        for a_site in [1u32, 3, 7, 11] {
            for b_site in [1u32, 3, 7, 11] {
                let result = meet(
                    BindingState::Live,
                    meet(
                        BindingState::Consumed(SiteId(a_site)),
                        BindingState::Consumed(SiteId(b_site)),
                    ),
                );
                let expected_min = a_site.min(b_site);
                assert_eq!(
                    result,
                    BindingState::MaybeConsumed(SiteId(expected_min)),
                    "Live ⊓ Consumed({a_site}) ⊓ Consumed({b_site}) should be MaybeConsumed({expected_min})"
                );
            }
        }
    }

    // Original property tests below — kept for narrow-space coverage.

    // ---------- Cooperate-site analysis tests ----------

    /// Build a minimal `BasicBlock` for test CFG construction.
    /// `statements` and `instructions` are empty — the cooperate-site
    /// analysis only inspects the terminator and the statement count.
    fn bb(id: u32, terminator: Terminator) -> BasicBlock {
        BasicBlock {
            id,
            statements: vec![],
            instructions: vec![],
            terminator,
        }
    }

    /// Build a `BasicBlock` with `n` dummy `MirStatement::Evaluate`
    /// entries so the leaf heuristic can be tested at the threshold.
    fn bb_with_stmts(id: u32, terminator: Terminator, n: usize) -> BasicBlock {
        BasicBlock {
            id,
            statements: vec![
                MirStatement::Evaluate {
                    site: SiteId(0),
                    ty: ResolvedTy::I64,
                };
                n
            ],
            instructions: vec![],
            terminator,
        }
    }

    /// A simple leaf function: two blocks, no loops, no calls, fewer than
    /// `LEAF_STATEMENT_THRESHOLD` statements total.
    ///
    ///   block 0: stmt×0, Goto { target: 1 }
    ///   block 1: stmt×0, Return
    ///
    /// Expected: zero sites (leaf skip).
    #[test]
    fn leaf_function_produces_no_sites() {
        let blocks = vec![
            bb(0, Terminator::Goto { target: 1 }),
            bb(1, Terminator::Return),
        ];
        let sites = compute_cooperate_sites(&blocks);
        assert!(
            sites.is_empty(),
            "leaf function should produce no cooperate sites, got {sites:?}"
        );
    }

    /// A non-leaf return-only function (over the statement threshold, no loops).
    ///
    ///   block 0: stmt×10, Return
    ///
    /// Expected: one `FunctionEntry` site at bb 0.
    #[test]
    fn non_leaf_return_only_produces_function_entry() {
        let blocks = vec![bb_with_stmts(
            0,
            Terminator::Return,
            LEAF_STATEMENT_THRESHOLD,
        )];
        let sites = compute_cooperate_sites(&blocks);
        assert_eq!(sites.len(), 1, "expected one cooperate site, got {sites:?}");
        assert_eq!(
            sites[0],
            CooperateSite {
                bb_id: 0,
                kind: CooperateKind::FunctionEntry,
            },
            "wrong site: {sites:?}"
        );
    }

    /// A non-leaf function with a single loop back-edge.
    ///
    /// Simulates: `fn loop_sum() -> i64 { for i in 0..100 { ... } }`.
    /// The CFG has three non-leaf blocks:
    ///
    ///   block 0: stmt×10, Goto { target: 1 }   ← entry; Goto fwd = not a back-edge
    ///   block 1: stmt×0,  Branch { then: 2, else: 3 } ← loop condition
    ///   block 2: stmt×0,  Goto { target: 1 }   ← loop back-edge (2 > 1)
    ///   block 3: stmt×0,  Return                ← loop exit
    ///
    /// Expected: `FunctionEntry` at bb 0 + `LoopBackEdge` at bb 2 = two sites.
    #[test]
    fn single_loop_produces_entry_and_back_edge_sites() {
        let blocks = vec![
            bb_with_stmts(0, Terminator::Goto { target: 1 }, LEAF_STATEMENT_THRESHOLD),
            bb(
                1,
                Terminator::Branch {
                    cond: Place::Local(0),
                    then_target: 2,
                    else_target: 3,
                },
            ),
            bb(2, Terminator::Goto { target: 1 }), // back-edge: 2 > 1
            bb(3, Terminator::Return),
        ];
        let sites = compute_cooperate_sites(&blocks);
        assert_eq!(
            sites.len(),
            2,
            "expected 2 sites (entry + back-edge), got {sites:?}"
        );
        assert!(
            sites.contains(&CooperateSite {
                bb_id: 0,
                kind: CooperateKind::FunctionEntry,
            }),
            "missing FunctionEntry site: {sites:?}"
        );
        assert!(
            sites.contains(&CooperateSite {
                bb_id: 2,
                kind: CooperateKind::LoopBackEdge,
            }),
            "missing LoopBackEdge site at bb 2: {sites:?}"
        );
    }

    /// A non-leaf function with two nested loops.
    ///
    ///   block 0: stmt×10, Goto { target: 1 }    ← entry
    ///   block 1: Branch { then: 2, else: 6 }     ← outer loop condition
    ///   block 2: Branch { then: 3, else: 5 }     ← inner loop condition
    ///   block 3: Goto { target: 2 }              ← inner back-edge (3 > 2)
    ///   block 4: (unreachable — placeholder)      not needed; collapse to:
    ///   block 5: Goto { target: 1 }              ← outer back-edge (5 > 1)
    ///   block 6: Return                           ← exit
    ///
    /// Expected: `FunctionEntry` at bb 0 + two `LoopBackEdge` sites = 3 sites.
    #[test]
    fn nested_loops_produce_entry_and_two_back_edge_sites() {
        let blocks = vec![
            bb_with_stmts(0, Terminator::Goto { target: 1 }, LEAF_STATEMENT_THRESHOLD),
            bb(
                1,
                Terminator::Branch {
                    cond: Place::Local(0),
                    then_target: 2,
                    else_target: 6,
                },
            ),
            bb(
                2,
                Terminator::Branch {
                    cond: Place::Local(1),
                    then_target: 3,
                    else_target: 5,
                },
            ),
            bb(3, Terminator::Goto { target: 2 }), // inner back-edge: 3 > 2
            bb(5, Terminator::Goto { target: 1 }), // outer back-edge: 5 > 1
            bb(6, Terminator::Return),
        ];
        let sites = compute_cooperate_sites(&blocks);
        assert_eq!(
            sites.len(),
            3,
            "expected 3 sites (entry + 2 back-edges), got {sites:?}"
        );
        assert!(
            sites.contains(&CooperateSite {
                bb_id: 0,
                kind: CooperateKind::FunctionEntry,
            }),
            "missing FunctionEntry: {sites:?}"
        );
        assert!(
            sites.contains(&CooperateSite {
                bb_id: 3,
                kind: CooperateKind::LoopBackEdge,
            }),
            "missing inner LoopBackEdge at bb 3: {sites:?}"
        );
        assert!(
            sites.contains(&CooperateSite {
                bb_id: 5,
                kind: CooperateKind::LoopBackEdge,
            }),
            "missing outer LoopBackEdge at bb 5: {sites:?}"
        );
    }

    /// A yield-equivalent entry block produces no sites.
    ///
    /// Simulates a receive handler: block 0's terminator is `Yield`, which
    /// already causes the actor to cooperate with the scheduler. No
    /// function-entry cooperate call is needed.
    ///
    ///   block 0: stmt×10, Yield { value: Local(0), next: 1 }
    ///   block 1: stmt×0,  Return
    ///
    /// Expected: zero sites (yield-equivalent suppresses entry; no back-edges).
    #[test]
    fn yield_equivalent_entry_produces_no_sites() {
        let blocks = vec![
            bb_with_stmts(
                0,
                Terminator::Yield {
                    value: Place::Local(0),
                    next: 1,
                },
                LEAF_STATEMENT_THRESHOLD,
            ),
            bb(1, Terminator::Return),
        ];
        let sites = compute_cooperate_sites(&blocks);
        assert!(
            sites.is_empty(),
            "yield-equivalent entry should suppress all cooperate sites, got {sites:?}"
        );
    }

    /// Empty block list produces no sites (defensive guard).
    #[test]
    fn empty_blocks_produces_no_sites() {
        let sites = compute_cooperate_sites(&[]);
        assert!(
            sites.is_empty(),
            "empty CFG should produce no sites, got {sites:?}"
        );
    }

    #[test]
    fn meet_consumed_picks_earlier_site() {
        let early = SiteId(3);
        let late = SiteId(7);
        // Consumed ⊓ Consumed: result carries the minimum (earliest)
        // site for diagnostic anchoring.
        assert_eq!(
            meet(BindingState::Consumed(early), BindingState::Consumed(late)),
            BindingState::Consumed(early)
        );
        // Same min-site rule across all (Consumed | MaybeConsumed)
        // pairings.
        assert_eq!(
            meet(
                BindingState::Consumed(late),
                BindingState::MaybeConsumed(early)
            ),
            BindingState::MaybeConsumed(early)
        );
    }
}
