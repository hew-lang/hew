//! Per-block move-checker dataflow over the binding-state lattice.
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
//!     Live    ⊔ Disch     = MaybeConsumed
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
//!  - `Discharged(s)` → permit non-consuming reads, but emit
//!    `UseAfterConsume` for another consume/discharge.
//!  - `MaybeConsumed(s)` → emit `UseAfterConsume{consumed_at: s,
//!    used_at}` (the diagnostic surface is the same; a future polish
//!    cluster may add the "consumed on some paths" annotation).
//!  - If the use is `IntentKind::Consume` on a non-`BitCopy` type,
//!    transition to `Consumed(use_site)` after the read-check.
//!  - If the use is `IntentKind::Discharge`, transition to
//!    `Discharged(use_site)` so exit cleanup is suppressed while later
//!    non-consuming closed-handle probes remain valid.
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
    BasicBlock, CooperateKind, CooperateSite, Instr, MirCheck, MirStatement, Place, RawMirFunction,
    Terminator,
};
use crate::{raw_virtual_operation_class, RawVirtualClass};

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
    /// The binding's affine release obligation was explicitly discharged on
    /// every predecessor path. Its closed handle bits remain available to
    /// non-consuming reads, but no later consume or discharge is legal.
    Discharged(SiteId),
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
    use BindingState::{Consumed, Discharged, Live, MaybeConsumed, Uninit};
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
        (Live, Discharged(s)) => MaybeConsumed(s),
        (Live, Consumed(s)) => MaybeConsumed(s),
        (Live, MaybeConsumed(s)) => MaybeConsumed(s),
        (Discharged(sa), Discharged(sb)) => Discharged(min_site(sa, sb)),
        (Discharged(sa), Consumed(sb)) => Consumed(min_site(sa, sb)),
        (Discharged(sa), MaybeConsumed(sb)) => MaybeConsumed(min_site(sa, sb)),
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
        (Discharged(sa), Aliased(sb)) => MaybeConsumed(min_site(sa, sb)),
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
        BindingState::Discharged(_) => 2,
        BindingState::Consumed(_) => 3,
        BindingState::MaybeConsumed(_) => 4,
        BindingState::AliasedIntoAggregate(_) => 5,
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
#[allow(
    clippy::too_many_arguments,
    clippy::too_many_lines,
    reason = "the transfer carries the shared checker sinks and covers every statement plus exact tail-return ownership"
)]
fn transfer_block<S: std::hash::BuildHasher>(
    entry: BTreeMap<BindingId, BindingState>,
    block: &BasicBlock,
    type_classes: &TypeClassTable,
    binding_locals: &HashMap<BindingId, Place, S>,
    linear_bindings: &mut BTreeMap<BindingId, (String, ResolvedTy, SiteId)>,
    checks: &mut Vec<MirCheck>,
    use_after_consume_seen: &mut HashSet<(BindingId, SiteId)>,
    init_before_use_seen: &mut HashSet<(BindingId, SiteId)>,
) -> BTreeMap<BindingId, BindingState> {
    let mut state = entry;
    for statement in &block.statements {
        match statement {
            MirStatement::Bind {
                binding,
                name,
                ty,
                site,
            } => {
                state.insert(*binding, BindingState::Live);
                if ValueClass::of_ty(ty, type_classes) == ValueClass::Linear {
                    linear_bindings.insert(*binding, (name.clone(), ty.clone(), *site));
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
                    BindingState::Discharged(discharged_at) => {
                        if matches!(intent, IntentKind::Consume | IntentKind::Discharge)
                            && use_after_consume_seen.insert((*binding, *site))
                        {
                            checks.push(MirCheck::UseAfterConsume {
                                binding: *binding,
                                name: name.clone(),
                                consumed_at: discharged_at,
                                used_at: *site,
                            });
                        }
                    }
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
                if *intent == IntentKind::Discharge {
                    if matches!(state.get(binding), Some(BindingState::Live)) {
                        state.insert(*binding, BindingState::Discharged(*site));
                    }
                } else if *intent == IntentKind::Consume
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
                partial_projection,
                ..
            } => {
                match state.get(binding).copied() {
                    // The SAME owned handle placed into an aggregate twice
                    // (`(t, t)` / `(h, ..., h)`): the second placement is a
                    // use-after-move — both aggregate fields would free the one
                    // handle. Flag it and anchor at the first placement.
                    //
                    // A PARTIAL-PROJECTION mark (#2523) is exempt: it records
                    // that an owned aggregate had a *projection* moved out, and
                    // each independent field move (`V(x, y) => let wx = x;
                    // let wy = y;`) re-emits it. The fields are distinct sub-
                    // objects, not the same handle placed twice, so a repeat on
                    // an already-aliased binding is an idempotent no-op — the
                    // binding is already in the re-read-forbidding state.
                    Some(BindingState::AliasedIntoAggregate(prev_site)) => {
                        if !*partial_projection && use_after_consume_seen.insert((*binding, *site))
                        {
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
                        | BindingState::Discharged(_)
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

    // Source move checking and cleanup ownership are deliberately separate
    // lattices. `MirStatement::{Bind,Use}` above is the complete authority for
    // source-language use-after-consume diagnostics. Generation-aware
    // `OwnershipEvent`s are replayed once by the Checked-MIR owner-state
    // interpreter; folding them back into this binding lattice loses program
    // order (all statements precede all instructions in a block) and made a
    // valid owner adoption look like a source binding consumed at SiteId(0).

    // Tail expressions lower their physical hand-off directly into the return
    // slot.  Unlike an explicit consuming use, that move has no corresponding
    // `MirStatement::Use { intent: Consume }`, so mirror the exact machine
    // transfer in the binding-state authority.  Only accept an unambiguous
    // named backing local: stale/synthetic aliases must be rekeyed by lowering
    // before they can affect exit cleanup.
    if matches!(block.terminator, Terminator::Return) {
        let return_site = block
            .statements
            .iter()
            .rev()
            .find_map(|statement| match statement {
                MirStatement::Return { site, .. } => Some(site.unwrap_or(SiteId(0))),
                _ => None,
            })
            .unwrap_or(SiteId(0));
        for instruction in &block.instructions {
            let Instr::Move {
                dest: Place::ReturnSlot,
                src: Place::Local(local),
            } = instruction
            else {
                continue;
            };
            let mut owners = binding_locals.iter().filter_map(|(binding, place)| {
                (*place == Place::Local(*local)).then_some(*binding)
            });
            let Some(binding) = owners.next() else {
                continue;
            };
            if owners.next().is_none() {
                state.insert(binding, BindingState::Consumed(return_site));
            }
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
        for succ in block.successors() {
            preds.entry(succ).or_default().push(block.id);
        }
    }
    preds
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
            for s in block.successors() {
                if visited.insert(s) {
                    stack.push(s);
                }
            }
        }
    }
    visited
}

/// Block IDs that can actually EXECUTE at runtime: reachable from the entry
/// block along terminator edges, never crossing the continuation edge of a
/// checker-proven `Never`-typed call. The no-return property is carried by
/// [`CallAuthority`](crate::CallAuthority), never rediscovered from a symbol.
///
/// A `Terminator::Call` structurally requires a `next` block, so lowering a
/// `Never`-typed call still emits a continuation (opened as a dead cursor,
/// often materialising a poison result and a `Goto` to the enclosing join).
/// That deadness is a lowering-time cursor flag and does not survive into
/// `BasicBlock`, so the plain structural [`reachable_from_entry`] view treats
/// the poison path as executable. Letting such a path contribute to a join
/// block's `meet_predecessors` kills — to `Uninit` — every binding that is
/// Live on all EXECUTABLE paths into the join. For a match whose sibling arm
/// panics (`let e = match f() { .Ok(x) => x, .Err(_) => panic(...) }`), that
/// false `Uninit` meet (a) moved the payload binder's admitted composite
/// release from the function exit to the arm's scope-close `Goto` — BEFORE
/// the join's field loads read the record, a use-after-free that turned into
/// a double-free abort on every `net.connect_timeout` call — and (b) starved
/// the return exits of the balancing drop, leaking one heap block per call in
/// the shapes where (a) did not fire. Excluding the never-executing poison
/// predecessors makes the meet agree with runtime reality: the binder stays
/// Live through the join and its single release fires at the true exits,
/// after every read.
///
/// Diagnostics and the RPO worklist deliberately keep the structural
/// [`reachable_from_entry`] view: post-panic code is still swept for
/// diagnostics, and every block still receives an exit-state entry.
pub(crate) fn execution_reachable_from_entry(blocks: &[BasicBlock]) -> HashSet<u32> {
    let by_id: HashMap<u32, &BasicBlock> = blocks.iter().map(|b| (b.id, b)).collect();
    let mut visited: HashSet<u32> = HashSet::new();
    let mut stack: Vec<u32> = Vec::new();
    if by_id.contains_key(&0) {
        stack.push(0);
        visited.insert(0);
    }
    while let Some(cur) = stack.pop() {
        if let Some(block) = by_id.get(&cur) {
            if let Terminator::Call { authority, .. } = &block.terminator {
                if authority.is_no_return() {
                    // The call never returns; its continuation edge is dead.
                    continue;
                }
            }
            for s in block.successors() {
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
        let succs = block.successors();
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
pub(crate) fn instr_reads_writes(instr: &Instr) -> (Vec<Place>, Vec<Place>, Vec<Place>) {
    match instr {
        // Raw value operations use a disjoint virtual-value namespace. They
        // neither read nor write addressable MIR places; the explicit ABI
        // materialization below is the sole storage boundary in this slice.
        Instr::Value(operation) => match raw_virtual_operation_class(operation) {
            Some(RawVirtualClass::Integer | RawVirtualClass::Bool | RawVirtualClass::Tuple)
            | None => (vec![], vec![], vec![]),
        },
        Instr::MaterializeValue { dest, .. } => (vec![], vec![*dest], vec![]),
        Instr::OwnershipEvent(_)
        | Instr::EnterContext
        | Instr::ExitContext
        | Instr::CheckCancellation => (vec![], vec![], vec![]),
        Instr::InteriorMutationCommit { place } => (vec![*place], vec![], vec![*place]),
        Instr::ContextField { dest, .. }
        | Instr::ConstI64 { dest, .. }
        | Instr::StringLit { dest, .. }
        | Instr::BytesLit { dest, .. }
        | Instr::ConstGlobalLoad { dest, .. }
        | Instr::FloatLit { dest, .. }
        | Instr::CharLit { dest, .. }
        | Instr::UnitLit { dest }
        | Instr::DurationLit { dest, .. }
        | Instr::ActorStateFieldLoad { dest, .. } => (vec![], vec![*dest], vec![]),
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
        | Instr::FloatRem { dest, lhs, rhs, .. } => (vec![*lhs, *rhs], vec![*dest], vec![]),
        Instr::CancellationTokenIsCancelled { dest, token } => (vec![*token], vec![*dest], vec![]),
        Instr::RcIntrinsic {
            dest,
            receiver,
            value,
            ..
        } => (
            receiver.iter().chain(value.iter()).copied().collect(),
            vec![*dest],
            vec![],
        ),
        Instr::GeneratorNext { dest, ctx, .. } => (vec![*ctx], vec![*dest], vec![]),
        Instr::WireCodec { dest, operand, .. } => (vec![*operand], vec![*dest], vec![]),
        Instr::RecordCloneInplace { dest, src, .. }
        | Instr::EnumCloneInplace { dest, src, .. }
        | Instr::ValueSnapshotClone { dest, src, .. } => (vec![*src], vec![*dest], vec![]),
        Instr::ValueSnapshotDrop { value, guard, .. } => {
            let mut reads = vec![*value];
            reads.extend(*guard);
            (reads, vec![], vec![*value])
        }
        Instr::BoolNot { dest, operand }
        | Instr::FloatNeg { dest, operand, .. }
        | Instr::IntBitNot { dest, operand } => (vec![*operand], vec![*dest], vec![]),
        Instr::NumericCast { dest, src, .. }
        | Instr::SaturatingWidthCast { dest, src, .. }
        | Instr::TryWidthCast { dest, src, .. } => (vec![*src], vec![*dest], vec![]),
        Instr::IntNegChecked {
            dest,
            operand,
            overflow_flag,
            ..
        } => (vec![*operand], vec![*dest, *overflow_flag], vec![]),
        Instr::IntArithChecked {
            dest,
            lhs,
            rhs,
            overflow_flag,
            ..
        } => (vec![*lhs, *rhs], vec![*dest, *overflow_flag], vec![]),
        Instr::Move { dest, src } => (vec![*src], vec![*dest], vec![]),
        // Refcount metadata only: reads the existing bytes triple and does not
        // move or overwrite the MIR place.
        Instr::BytesRetain { value } | Instr::StringRetain { value, .. } => {
            (vec![*value], vec![], vec![])
        }
        Instr::CallRuntimeAbi(call) => {
            let reads = call.args().to_vec();
            let writes = call.dest().into_iter().collect();
            // `bytes` is a stack-resident owned triple, and its mutating
            // runtime ABI receives arg[0] by address so it can release/replace
            // the backing buffer and write the updated triple back in place.
            // Move-state still sees a borrowed receiver, but helper crash
            // cleanup stores a byte Snapshot and must refresh that escrow
            // around the call. Other runtime handles mutate their pointees;
            // their MIR slot bytes do not change.
            let interior = runtime_call_interior_write_places(call.family(), call.args());
            (reads, writes, interior)
        }
        Instr::AutoLockAcquire { lock } | Instr::AutoLockRelease { lock } => {
            // The lock pointer is read (its address is passed to the
            // runtime FFI). No place is written — the FFI mutates the
            // pointee, which is opaque to the MIR dataflow.
            (vec![*lock], vec![], vec![])
        }
        Instr::CallClosure {
            callee, args, dest, ..
        } => {
            let mut reads = args.clone();
            reads.insert(0, *callee);
            let writes = dest.iter().copied().collect();
            (reads, writes, vec![])
        }
        Instr::MakeClosure { env, dest, .. } | Instr::ClosureEnvFieldLoad { env, dest, .. } => {
            (vec![*env], vec![*dest], vec![])
        }
        Instr::SpawnTaskDirect { task, .. } => (vec![*task], vec![], vec![]),
        Instr::SpawnTaskClosure { task, env, .. } => (vec![*task, *env], vec![], vec![]),
        Instr::Drop { place, .. } => {
            let interior = matches!(
                place,
                Place::MachineVariant { .. } | Place::EnumVariant { .. }
            )
            .then_some(*place)
            .into_iter()
            .collect();
            (vec![*place], vec![], interior)
        }
        Instr::AggregateOverwriteRelease {
            old, replacement, ..
        } => (vec![*old, *replacement], vec![], vec![*old]),
        Instr::WitnessSizeOf { dest, .. } | Instr::WitnessAlignOf { dest, .. } => {
            (vec![], vec![*dest], vec![])
        }
        Instr::WitnessDropGlue { place, .. } => (vec![*place], vec![], vec![]),
        Instr::WitnessMove { dest, src, .. } => (vec![*src], vec![*dest], vec![]),
        Instr::RecordInit { fields, dest, .. } => {
            let reads = fields.iter().map(|(_, place)| *place).collect();
            (reads, vec![*dest], vec![])
        }
        Instr::ClosureEnvInit { fields, dest, .. } => {
            let reads = fields.iter().map(|field| field.src).collect();
            (reads, vec![*dest], vec![])
        }
        Instr::RecordFieldLoad { record, dest, .. } => (vec![*record], vec![*dest], vec![]),
        Instr::RecordFieldDrop { record, .. } => {
            // RecordFieldDrop GEPs into `record` (the functional-update BASE
            // aggregate) to release the OLD value of an overridden owned field
            // in place and null-store that one slot. It is a READ of `record`,
            // not a move: the base's overall move-state is governed separately
            // (the base binding is marked consumed by `alias_moved_owned_operand`
            // at the `..base` ingress). It defines no place. Note the base is
            // NOT the record `RecordInit` builds — `RecordInit` constructs a
            // distinct new aggregate from the carried/override sources; this op
            // only neutralises the orphaned old field value on the consumed base.
            (vec![*record], vec![], vec![*record])
        }
        Instr::FieldDropInPlace { base, .. } => {
            // FieldDropInPlace GEPs into `base` to release ONE owned field
            // slot in place (type-directed; no `drop_fn`, no temp). It is a
            // READ of `base`, not a move — the base aggregate's overall
            // move-state is governed by its own consume marks — and it
            // defines no place (interior field op: uses base, no dest, no
            // alias). Mirrors `RecordFieldDrop` above.
            (vec![*base], vec![], vec![*base])
        }
        Instr::RecordFieldStore { record, src, .. } => {
            // Field-store reads both the aggregate (to GEP into it) and
            // the source. The aggregate stays Live — only the field bytes
            // are overwritten; ownership of the surrounding record does
            // not transfer. Returning the record as a read (not a write)
            // is what keeps the dataflow lattice in the `Live` state for
            // it after the store. See `Iterator::next(var self)` in
            // `std/builtins.hew` for the load-bearing consumer (the
            // mutable-receiver substrate).
            (vec![*record, *src], vec![], vec![*record])
        }
        Instr::ActorStateFieldStore { src, .. } => (vec![*src], vec![], vec![]),
        // The neutralize references the scrutinee's payload slot (keeping the
        // base local live through the null store) and defines no new SSA value.
        Instr::NeutralizePayloadSlot { place, .. } => (vec![*place], vec![], vec![*place]),
        Instr::AggregateProjectionNeutralize { root, .. } => (vec![*root], vec![], vec![*root]),
        // Closure-env write-back (#1′): reads the env pointer (to GEP into it)
        // and the stored value. The env stays Live — only the field bytes are
        // overwritten through the pointer, opaque to the MIR lattice — so the
        // env is a read, not a write, exactly like `RecordFieldStore`.
        Instr::ClosureEnvFieldStore { env, src, .. } => (vec![*env, *src], vec![], vec![]),
        Instr::TupleFieldLoad { tuple, dest, .. } => (vec![*tuple], vec![*dest], vec![]),
        Instr::TupleConstruct { elements, dest } => (elements.clone(), vec![*dest], vec![]),
        Instr::SpawnActor {
            state,
            init_args,
            dest,
            ..
        } => {
            let mut reads: Vec<_> = state.iter().copied().collect();
            reads.extend(init_args.iter().copied());
            (reads, vec![*dest], vec![])
        }
        Instr::CoerceToDynTrait { value, dest, .. } => (vec![*value], vec![*dest], vec![]),
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
            (reads, writes, vec![])
        }
        Instr::MachineEmitPlaceholder { payload, .. } => {
            // The placeholder reads all payload places; no write destination
            // (emit is void — the result is dispatched to the event queue).
            (payload.clone(), vec![], vec![])
        }
        Instr::EnumTagLoad { src, dest } => (vec![*src], vec![*dest], vec![]),
        Instr::MachineStateName {
            src_local, dest, ..
        } => (vec![Place::Local(*src_local)], vec![*dest], vec![]),
        Instr::MachineEmitTake {
            event_tag, dest, ..
        } => (vec![*event_tag], vec![*dest], vec![]),
    }
}

/// Stack-resident roots mutated in place by one typed runtime ABI family.
/// Shared by the legacy instruction classifier and the terminator
/// canonicalizer so moving a may-unwind call onto a CFG edge cannot lose its
/// normal-edge snapshot refresh authority.
#[must_use]
pub fn runtime_call_interior_write_places(
    family: hew_types::runtime_call::RuntimeCallFamily,
    args: &[Place],
) -> Vec<Place> {
    match family {
        hew_types::runtime_call::RuntimeCallFamily::BytesAppend
        | hew_types::runtime_call::RuntimeCallFamily::BytesClear
        | hew_types::runtime_call::RuntimeCallFamily::BytesPop
        | hew_types::runtime_call::RuntimeCallFamily::BytesPush
        | hew_types::runtime_call::RuntimeCallFamily::BytesSet => {
            args.first().copied().into_iter().collect()
        }
        _ => vec![],
    }
}

/// The exact whole-place writes performed by `instr`.
///
/// This is the narrow public authority for consumers that need to bracket
/// initialized destination writes without duplicating the exhaustive
/// instruction classification above. Interior mutations deliberately remain
/// reads in [`instr_reads_writes`], so they do not appear here.
#[must_use]
pub fn instr_write_places(instr: &Instr) -> Vec<Place> {
    instr_reads_writes(instr).1
}

/// Roots whose initialized bytes are mutated in place without defining a new
/// MIR value.
///
/// This is distinct from [`instr_write_places`]: move-state dataflow must keep
/// these roots live, while byte snapshots used by crash cleanup must refresh
/// after the mutation. The classification is part of the same exhaustive
/// [`Instr`] match as reads and whole-place writes, so a new instruction cannot
/// silently bypass either authority.
#[must_use]
pub fn instr_interior_write_places(instr: &Instr) -> Vec<Place> {
    instr_reads_writes(instr).2
}

/// The backing MIR local a write `Place` addresses, or `None` for the return
/// slot (which is not a local register). Exhaustive by construction
/// (fail-closed): a new `Place` variant forces a decision here rather than
/// silently escaping the write-set model. Mirrors `liveness::place_local`,
/// kept local to avoid widening that helper's module-private visibility.
pub(crate) fn write_place_local(place: Place) -> Option<u32> {
    match place {
        Place::Local(n)
        | Place::DuplexHandle(n)
        | Place::LambdaActorHandle(n)
        | Place::ActorHandle(n)
        | Place::SendHalf(n)
        | Place::RecvHalf(n)
        | Place::MachineTag(n)
        | Place::EnumTag(n) => Some(n),
        Place::MachineVariant { local, .. } | Place::EnumVariant { local, .. } => Some(local),
        Place::ReturnSlot => None,
    }
}

/// The write (def) slots of a `Terminator` — the complement of
/// [`crate::lower::terminator_source_places`], which classifies the read
/// operands. Exhaustive over every variant (fail-closed: a new terminator
/// forces a write-classification decision rather than silently dropping a
/// def). Each arm collects exactly the slots `terminator_source_places`
/// documents as writes (a `Call`'s `dest`, an `Ask`/`RemoteAsk`'s
/// result/reply/error dests, a `Select` arm's binding, a `Join`'s `result`
/// and per-branch `reply_dest`, a generator / lambda-actor handle `dest`).
///
/// `Suspend` / `SuspendingScopeDeadline` carry their resume-edge write dests
/// in the `SuspendKind` side-table (always freshly-allocated result/reply
/// temps, never a parameter local) and appear only in coroutine functions.
/// The sole caller that reasons about parameter writes
/// ([`local_is_written_in_body`], consumed by codegen's `bytes` aliasing
/// decision) gates coroutine functions out before consulting this, so the
/// empty set returned for the suspend carriers is sound for that use.
#[must_use]
pub fn terminator_write_places(term: &Terminator) -> Vec<Place> {
    match term {
        Terminator::Return
        | Terminator::Unreachable
        | Terminator::Goto { .. }
        | Terminator::Trap { .. }
        | Terminator::Branch { .. }
        | Terminator::Yield { .. }
        | Terminator::Suspend { .. }
        | Terminator::SuspendingScopeDeadline { .. } => Vec::new(),
        Terminator::Send { result_dest, .. } => result_dest.iter().copied().collect(),
        Terminator::Call { dest, .. } => dest.iter().copied().collect(),
        Terminator::MakeGenerator { dest, .. } | Terminator::MakeLambdaActor { dest, .. } => {
            vec![*dest]
        }
        Terminator::Ask {
            result_dest,
            reply_dest,
            error_dest,
            ..
        }
        | Terminator::RemoteAsk {
            result_dest,
            reply_dest,
            error_dest,
            ..
        } => vec![*result_dest, *reply_dest, *error_dest],
        Terminator::Select { arms, .. } | Terminator::SuspendingSelect { arms, .. } => {
            arms.iter().filter_map(|arm| arm.binding).collect()
        }
        // The join emitter stages raw reply buffers until every branch has
        // succeeded, then publishes only the final result tuple. `reply_dest`
        // remains type authority for each branch's reply ABI; it is not a MIR
        // definition and must not mint a phantom owner.
        Terminator::Join { result, .. } => vec![*result],
    }
}

/// Exact result slots written by a collapsed suspension emitter when its
/// parked operation becomes ready. This is separate from
/// [`terminator_write_places`] because the carrier `Terminator::Suspend` stores
/// its operation-specific destinations in [`crate::SuspendKind`].
///
/// Exhaustive by construction: adding a suspend kind requires deciding whether
/// its emitter initializes a result on the ready/resume edge. `RestartWait`
/// deliberately writes nothing here; its handle is re-fetched by a regular MIR
/// instruction in the resume block and is covered by [`instr_write_places`].
#[must_use]
pub fn suspend_kind_write_places(kind: &crate::SuspendKind) -> Vec<Place> {
    match kind {
        crate::SuspendKind::Ask {
            result_dest,
            reply_dest,
            error_dest,
            ..
        }
        | crate::SuspendKind::RemoteAsk {
            result_dest,
            reply_dest,
            error_dest,
            ..
        } => vec![*result_dest, *reply_dest, *error_dest],
        crate::SuspendKind::Read {
            result_dest,
            deadline_result_dest,
            error_dest,
            ..
        }
        | crate::SuspendKind::Accept {
            result_dest,
            deadline_result_dest,
            error_dest,
            ..
        }
        | crate::SuspendKind::StreamNext {
            result_dest,
            deadline_result_dest,
            error_dest,
            ..
        }
        | crate::SuspendKind::ChannelRecv {
            result_dest,
            deadline_result_dest,
            error_dest,
            ..
        } => {
            let mut places = vec![*result_dest];
            places.extend(deadline_result_dest);
            places.extend(error_dest);
            places
        }
        crate::SuspendKind::CallClosure { result_dest, .. }
        | crate::SuspendKind::TaskAwait { result_dest, .. } => {
            result_dest.iter().copied().collect()
        }
        crate::SuspendKind::ActorSend { .. }
        | crate::SuspendKind::StreamSend { .. }
        | crate::SuspendKind::RestartWait { .. }
        | crate::SuspendKind::Sleep { .. }
        | crate::SuspendKind::SleepUntil { .. } => Vec::new(),
    }
}

/// True when MIR `local` is ever WRITTEN (defined / reassigned) anywhere in
/// `func` — across every instruction (via [`instr_reads_writes`]) and every
/// terminator write slot (via [`terminator_write_places`]). Conservative and
/// fail-closed: any write whose backing local is `local` counts, and the
/// place→local map ([`write_place_local`]) is exhaustive so an unmodelled
/// place cannot hide a write.
///
/// Codegen consults this to decide whether a `bytes` parameter may be ALIASED
/// to the caller's triple (the pass-by-pointer write-back that makes a callee
/// mutation visible to the caller). A parameter that is only ever read is safe
/// to alias; one that is reassigned (`var b: bytes; b = ..`, which lowers to a
/// fresh temp plus an `Instr::Move` into the parameter local) must fall back
/// to a by-value copy so the reassignment stores into the callee's own slot
/// rather than clobbering the caller's.
#[must_use]
pub fn local_is_written_in_body(func: &RawMirFunction, local: u32) -> bool {
    func.blocks.iter().any(|block| {
        block.instructions.iter().any(|instr| {
            instr_reads_writes(instr)
                .1
                .into_iter()
                .filter_map(write_place_local)
                .any(|written| written == local)
        }) || terminator_write_places(&block.terminator)
            .into_iter()
            .filter_map(write_place_local)
            .any(|written| written == local)
    })
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
                let (reads, writes, _) = instr_reads_writes(instr);
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
            for succ in block.successors() {
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
    /// Per-block ENTRY (in-) state — the converged per-binding state at the
    /// TOP of each block, before any of its statements run. Mirrors
    /// `exit_states` but anchored at the block prologue. The elaborator needs
    /// this for cooperate-cancel drop planning: a `CooperateKind::FunctionEntry`
    /// cancel branches out of the function prologue, before the entry block's
    /// own `Bind` statements execute, so its drop set must reflect what is live
    /// at block ENTRY (parameters only, for a no-parameter function), never the
    /// block EXIT state (which would over-include locals constructed later in
    /// the same block and demonitor/free an uninitialised slot).
    pub entry_states: HashMap<u32, BTreeMap<BindingId, BindingState>>,
}

#[must_use]
pub fn check_blocks(blocks: &[BasicBlock], type_classes: &TypeClassTable) -> Vec<MirCheck> {
    analyze(blocks, type_classes, &[]).checks
}

/// Run the full dataflow pass and return both diagnostics and the
/// per-block exit-state map. The string-CoW lowering
/// (`temp_drop::remove_consumed_cow_bindings`) uses `exit_states` to
/// withdraw inline drops for bindings a path may have consumed; exit
/// drop plans themselves derive from the ownership-event replay
/// (`lower/drop_plan.rs`), not from this map.
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
    analyze_with_binding_locals(blocks, type_classes, param_bindings, &HashMap::new())
}

/// Run [`analyze`] with the lowering authority that maps source bindings to
/// their concrete backing places.  The additional mapping lets tail moves into
/// `ReturnSlot` participate in the same consume state as explicit uses.
#[must_use]
#[allow(
    clippy::too_many_lines,
    reason = "two-phase dataflow (fixpoint + diagnostic sweep); splitting would require shared mutable state across functions"
)]
pub fn analyze_with_binding_locals<S: std::hash::BuildHasher>(
    blocks: &[BasicBlock],
    type_classes: &TypeClassTable,
    param_bindings: &[BindingId],
    binding_locals: &HashMap<BindingId, Place, S>,
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
    // Executable blocks only, for the predecessor meets of blocks that can
    // themselves execute: a post-panic poison continuation is structurally
    // reachable but never runs, and its `Uninit` contribution at a join must
    // not kill bindings Live on every executable path (see
    // `execution_reachable_from_entry`). Blocks INSIDE the dead region keep
    // the structural view: their states still chain through the diverging
    // call's continuation, so code after an unconditional `panic(...)` carries
    // its parameter and binding states and is diagnosed exactly as before —
    // a closure parameter read after the panic must not regress into a false
    // `InitialisedBeforeUse` merely because its whole region is dead.
    let execution_reachable = execution_reachable_from_entry(blocks);
    let meet_filter = |block_id: u32| -> &HashSet<u32> {
        if execution_reachable.contains(&block_id) {
            &execution_reachable
        } else {
            &reachable
        }
    };

    // The function's entry block is id 0 by construction (see
    // `lower::Builder::seal_body_blocks`).
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
    let mut linear_bindings: BTreeMap<BindingId, (String, ResolvedTy, SiteId)> = BTreeMap::new();

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
            meet_predecessors(preds_of_bb, &exit_states, meet_filter(cur_id))
        };
        // In Phase 1 we only propagate state — diagnostics are discarded.
        let mut phase1_checks: Vec<MirCheck> = Vec::new();
        let mut phase1_use_seen: HashSet<(BindingId, SiteId)> = HashSet::new();
        let mut phase1_init_seen: HashSet<(BindingId, SiteId)> = HashSet::new();
        let new_exit = transfer_block(
            entry,
            block,
            type_classes,
            binding_locals,
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
            for succ in block.successors() {
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
    // Per-block ENTRY state, captured before each block's statements run.
    // Anchors cooperate-cancel drop planning at the block prologue (see
    // `DataflowResult::entry_states`).
    let mut entry_states: HashMap<u32, BTreeMap<BindingId, BindingState>> = HashMap::new();
    // Reset linear_bindings for the diagnostic pass (Phase 1 populated it
    // as a side-effect; resetting avoids double-registration).
    linear_bindings.clear();

    for block in blocks {
        let blk_id = block.id;
        // `seal_body_blocks` preserves a structurally valid home for source
        // after a failed/never-returning sub-lowering (for example, the loop
        // exit following a match whose scrutinee could not lower).  Such a
        // block has no path from entry, so it must not diagnose its implicit
        // `Uninit` state as though user code could execute there.  The
        // predecessor meet already excludes these blocks; keep the diagnostic
        // sweep aligned with that same reachability boundary.
        if !reachable.contains(&blk_id) {
            continue;
        }
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
            meet_predecessors(preds_of_bb, &exit_states, meet_filter(blk_id))
        };
        entry_states.insert(blk_id, entry.clone());
        transfer_block(
            entry,
            block,
            type_classes,
            binding_locals,
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
        if !reachable.contains(&block.id) {
            continue;
        }
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
            let Some((name, ty, bind_site)) = linear_bindings.get(binding) else {
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
                    bind_site: *bind_site,
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
        entry_states,
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

/// Determine whether a legacy MIR block has a scheduler back-edge.
///
/// The established raw-MIR scheduler contract uses the builder's monotonic
/// block allocation order: a `Goto` to a lower-numbered block is a loop latch.
/// Keep that policy behind [`compute_cooperate_sites`] exactly as it was so
/// ownership/drop lowering continues to receive the same cancellation sites.
/// New CFG producers that do not preserve that allocation convention must opt
/// into [`compute_structural_cooperate_sites`] instead.
fn is_legacy_back_edge_goto(block: &BasicBlock) -> bool {
    match block.terminator {
        Terminator::Goto { target } => target < block.id,
        _ => false,
    }
}

/// Return the source block ids of natural-loop `Goto` back-edges.
///
/// This is intentionally separate from the legacy scheduler predicate above.
/// A numeric block-id ordering is not a CFG invariant: an SSA lowering may
/// append edge-forwarding blocks after the source blocks they serve, so a
/// perfectly acyclic forwarding edge can legitimately have the shape
/// `bb7 -> bb3`.
///
/// A `Goto { target }` is a natural-loop back-edge exactly when `target`
/// dominates its source: every path from the entry to the source has already
/// passed through the target. The structural scheduler only places loop checks
/// on `Goto` edges (structured Hew loop lowerings use `Goto` for their latch);
/// other terminator forms deliberately retain the existing policy rather than
/// broadening scheduling behaviour as a side effect of this analysis.
fn structural_goto_loop_back_edge_blocks(blocks: &[BasicBlock]) -> HashSet<u32> {
    let by_id: HashMap<u32, &BasicBlock> = blocks.iter().map(|block| (block.id, block)).collect();
    if !by_id.contains_key(&0) {
        return HashSet::new();
    }

    // Ignore malformed successor ids and unreachable blocks. The ordinary MIR
    // producers guarantee valid CFG ids, but this keeps the scheduler analysis
    // conservative for hand-built test fixtures: an unknown target cannot be
    // proven to dominate anything, and an unreachable cycle never executes.
    let reachable: HashSet<u32> = reachable_from_entry(blocks)
        .into_iter()
        .filter(|id| by_id.contains_key(id))
        .collect();
    if reachable.is_empty() {
        return HashSet::new();
    }

    let predecessors = build_preds(blocks);
    let mut dominators: HashMap<u32, HashSet<u32>> = reachable
        .iter()
        .copied()
        .map(|id| {
            let initial = if id == 0 {
                HashSet::from([0])
            } else {
                reachable.clone()
            };
            (id, initial)
        })
        .collect();

    let mut changed = true;
    while changed {
        changed = false;
        for &block_id in &reachable {
            if block_id == 0 {
                continue;
            }
            let reachable_predecessors: Vec<u32> = predecessors
                .get(&block_id)
                .into_iter()
                .flatten()
                .copied()
                .filter(|predecessor| reachable.contains(predecessor))
                .collect();
            // A reachable non-entry block always has a reachable predecessor,
            // but retain a conservative singleton if a malformed CFG violates
            // that property.
            let mut next = if let Some((first, rest)) = reachable_predecessors.split_first() {
                let mut intersection = dominators.get(first).cloned().unwrap_or_default();
                for predecessor in rest {
                    if let Some(predecessor_dominators) = dominators.get(predecessor) {
                        intersection.retain(|id| predecessor_dominators.contains(id));
                    } else {
                        intersection.clear();
                    }
                }
                intersection
            } else {
                HashSet::new()
            };
            next.insert(block_id);
            if dominators.get(&block_id) != Some(&next) {
                dominators.insert(block_id, next);
                changed = true;
            }
        }
    }

    blocks
        .iter()
        .filter_map(|block| {
            let Terminator::Goto { target } = block.terminator else {
                return None;
            };
            (reachable.contains(&block.id)
                && dominators
                    .get(&block.id)
                    .is_some_and(|source_dominators| source_dominators.contains(&target)))
            .then_some(block.id)
        })
        .collect()
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
    if blocks.iter().any(is_legacy_back_edge_goto) {
        return false;
    }
    true
}

/// The structural counterpart to [`is_leaf_function`].
///
/// Kept distinct so the legacy raw-MIR scheduler stays behaviorally aligned
/// with its historic numeric heuristic, including for unusual hand-built CFG
/// fixtures. SIR alone opts into dominance-based latch recognition.
///
/// KNOWN MISCLASSIFICATION (size metric): the size test below counts
/// `MirStatement` entries, and the strict SIR lane refuses any raw block that
/// carries one (`hew-mir/src/sir.rs`, "raw bbN carries legacy MIR statements").
/// Every SIR-produced function therefore measures zero, so a call-free,
/// loop-free SIR body is classified a leaf and receives no cooperate site no
/// matter how large it is.
///
/// WHY IT STANDS: SIR's current surface is straight-line scalar code, whose
/// bodies are genuinely short and genuinely leaves, so the classification is
/// accidentally right for everything the strict lane can lower today.
///
/// WHEN IT BREAKS: S5, which brings loops and mutable locals — the first
/// slice able to produce a large call-free SIR body. The back-edge clause
/// catches looping bodies, not big straight-line ones.
///
/// REAL FIX: measure the size of the stream the function is actually built
/// from — the backend `Instr` count, or an op count carried as a SIR fact —
/// instead of the checker-authority statement stream, with a red test that a
/// large call-free SIR function still receives its entry safepoint.
fn is_structural_leaf_function(
    blocks: &[BasicBlock],
    loop_back_edge_blocks: &HashSet<u32>,
) -> bool {
    let total_statements: usize = blocks.iter().map(|b| b.statements.len()).sum();
    if total_statements >= LEAF_STATEMENT_THRESHOLD {
        return false;
    }
    if blocks.iter().any(block_has_call) {
        return false;
    }
    loop_back_edge_blocks.is_empty()
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

/// Compute structural cooperate-check sites using dominance-recognized loop
/// latches.
///
/// This deliberately does not serve legacy raw MIR: that scheduler preserves
/// its historic numeric block-order contract in [`compute_cooperate_sites`].
fn compute_structural_cooperate_sites_with_loop_back_edges(
    blocks: &[BasicBlock],
    loop_back_edge_blocks: &HashSet<u32>,
) -> Vec<CooperateSite> {
    if blocks.is_empty() || is_structural_leaf_function(blocks, loop_back_edge_blocks) {
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

    // Back-edge sites use the policy selected by the caller. Suppress if the
    // loop-header block itself has a yield-equivalent terminator (the actor
    // already cooperates at the loop header).
    let by_id: HashMap<u32, &BasicBlock> = blocks.iter().map(|b| (b.id, b)).collect();
    for block in blocks {
        let Terminator::Goto { target } = block.terminator else {
            continue;
        };
        if !loop_back_edge_blocks.contains(&block.id) {
            continue;
        }
        let header_yields = by_id
            .get(&target)
            .is_some_and(|header| is_yield_equivalent(header));
        if !header_yields {
            sites.push(CooperateSite {
                bb_id: block.id,
                kind: CooperateKind::LoopBackEdge,
            });
        }
    }

    sites
}

/// Compute the cooperate-check sites for a legacy raw-MIR function.
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
/// 3. **Back-edge sweep**: for every `Goto` whose target block id is less
///    than the source block id, add a `LoopBackEdge` site — unless
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
/// `LoopBackEdge` sites are live for `for`, `while`, and `loop` bodies. The
/// legacy HIR → MIR builder allocates their latches after their headers, so a
/// `Goto` to a lower-numbered block receives a cooperate check before control
/// returns to the loop header. This compatibility policy is deliberately not
/// generalized here: use [`compute_structural_cooperate_sites`] for a CFG
/// whose block allocation order is not a scheduling invariant.
#[must_use]
pub fn compute_cooperate_sites(blocks: &[BasicBlock]) -> Vec<CooperateSite> {
    let _timing = crate::timing::stage("compute_cooperate_sites");
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
                // Back-edge found. Check whether the loop-header block itself
                // is yield-equivalent — if so, no cooperate needed.
                let header_yields = by_id
                    .get(&target)
                    .is_some_and(|header| is_yield_equivalent(header));
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

/// Compute cooperate-check sites for a CFG whose block ids have no scheduling
/// meaning, such as SIR lowered through SSA edge-forwarding blocks.
///
/// This applies the same leaf and yield-equivalence policy as
/// [`compute_cooperate_sites`], but recognizes `Goto` latches structurally:
/// the target must dominate the source. It is deliberately opt-in so adopting
/// SIR does not silently change established ownership/drop cancellation sites
/// in legacy raw-MIR lowering.
#[must_use]
pub fn compute_structural_cooperate_sites(blocks: &[BasicBlock]) -> Vec<CooperateSite> {
    let loop_back_edge_blocks = structural_goto_loop_back_edge_blocks(blocks);
    compute_structural_cooperate_sites_with_loop_back_edges(blocks, &loop_back_edge_blocks)
}

// ---------- Property tests for the lattice ----------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::{
        CallAuthority, DropFnSpec, FieldAddr, FieldOffset, NeutralizeAuthority,
        PreparedCarrierBoundary, RuntimeCall,
    };

    #[test]
    #[allow(
        clippy::too_many_lines,
        reason = "one exhaustive table keeps every snapshot-mutating Instr class and \
                  its non-mutating controls auditable together"
    )]
    fn interior_write_authority_covers_every_snapshot_mutation_class() {
        let snapshot_plan =
            crate::state_clone::classify_value_snapshot_plan_with_lifecycle_registry(
                &ResolvedTy::String,
                &[],
                &[],
                &[],
                &hew_hir::LifecycleRegistry::default(),
            )
            .expect("string snapshot plan");
        let enum_projection = Place::EnumVariant {
            local: 6,
            variant_idx: 0,
            field_idx: 0,
        };
        let machine_projection = Place::MachineVariant {
            local: 2,
            variant_idx: 0,
            field_idx: 0,
        };
        let cases = [
            (
                "normal-edge interior mutation commit",
                Instr::InteriorMutationCommit {
                    place: Place::Local(0),
                },
                vec![Place::Local(0)],
            ),
            (
                "prepared snapshot drop",
                Instr::ValueSnapshotDrop {
                    value: Place::Local(1),
                    ty: ResolvedTy::String,
                    plan: snapshot_plan,
                    boundary: PreparedCarrierBoundary::LocalCall,
                    guard: None,
                },
                vec![Place::Local(1)],
            ),
            (
                "projected inline drop",
                Instr::Drop {
                    place: machine_projection,
                    ty: ResolvedTy::String,
                    drop_fn: Some(DropFnSpec::Release("hew_string_drop")),
                },
                vec![machine_projection],
            ),
            (
                "record field drop",
                Instr::RecordFieldDrop {
                    record: Place::Local(3),
                    field_offset: FieldOffset(0),
                    ty: ResolvedTy::String,
                    drop_fn: DropFnSpec::Release("hew_string_drop"),
                },
                vec![Place::Local(3)],
            ),
            (
                "type-directed field drop",
                Instr::FieldDropInPlace {
                    base: Place::Local(4),
                    field: FieldAddr::Tuple(0),
                    ty: ResolvedTy::String,
                },
                vec![Place::Local(4)],
            ),
            (
                "record field store",
                Instr::RecordFieldStore {
                    record: Place::Local(5),
                    field_offset: FieldOffset(0),
                    src: Place::Local(50),
                },
                vec![Place::Local(5)],
            ),
            (
                "variant payload neutralize",
                Instr::NeutralizePayloadSlot {
                    place: enum_projection,
                    transferee: Some(Place::Local(60)),
                    authority: NeutralizeAuthority::WholeCarrierConsume,
                },
                vec![enum_projection],
            ),
            (
                "aggregate projection neutralize",
                Instr::AggregateProjectionNeutralize {
                    root: Place::Local(7),
                    fields: vec![0, 1],
                    transferee: Place::Local(70),
                    scope_exit_owner: None,
                },
                vec![Place::Local(7)],
            ),
        ];

        for (label, instr, expected) in cases {
            assert_eq!(
                instr_interior_write_places(&instr),
                expected,
                "{label} must refresh a helper Snapshot owner"
            );
            assert!(
                instr_write_places(&instr).is_empty(),
                "{label} remains an interior mutation for move-state dataflow"
            );
        }

        for symbol in [
            "hew_bytes_append",
            "hew_bytes_clear",
            "hew_bytes_pop",
            "hew_bytes_push",
            "hew_bytes_set",
        ] {
            let call = RuntimeCall::new(symbol, vec![Place::Local(8)], None)
                .expect("bytes mutator is an admitted runtime family");
            let instr = Instr::CallRuntimeAbi(call);
            assert_eq!(
                instr_interior_write_places(&instr),
                vec![Place::Local(8)],
                "{symbol} mutates the owned bytes triple through arg[0]"
            );
            assert!(
                instr_write_places(&instr).is_empty(),
                "{symbol} keeps the receiver live for move-state dataflow"
            );
        }
        let bytes_len = Instr::CallRuntimeAbi(
            RuntimeCall::new("hew_vec_len", vec![Place::Local(8)], Some(Place::Local(80)))
                .expect("bytes/Vec len is an admitted runtime family"),
        );
        assert!(
            instr_interior_write_places(&bytes_len).is_empty(),
            "read-only runtime receivers never churn crash-cleanup snapshots"
        );

        assert!(
            instr_interior_write_places(&Instr::Drop {
                place: Place::Local(10),
                ty: ResolvedTy::String,
                drop_fn: Some(DropFnSpec::Release("hew_string_drop")),
            })
            .is_empty(),
            "whole-local Drop retires its token instead of refreshing it"
        );
        assert!(
            instr_interior_write_places(&Instr::ClosureEnvFieldStore {
                env: Place::Local(9),
                env_ty: ResolvedTy::named_user("Env", vec![]),
                field_offset: FieldOffset(0),
                src: Place::Local(90),
            })
            .is_empty(),
            "pointee mutation leaves the closure pointer bytes unchanged"
        );
    }

    fn states() -> Vec<BindingState> {
        vec![
            BindingState::Uninit,
            BindingState::Live,
            BindingState::Discharged(SiteId(3)),
            BindingState::Discharged(SiteId(7)),
            BindingState::Consumed(SiteId(3)),
            BindingState::Consumed(SiteId(7)),
            BindingState::MaybeConsumed(SiteId(3)),
            BindingState::MaybeConsumed(SiteId(7)),
        ]
    }

    /// Wider exhaustive state-space for the M2 substrate's drop-plan
    /// invariants. Includes multiple consume sites with non-trivial
    /// ordering (1, 3, 7, 11) so the min-site rule for
    /// Discharged/Consumed/MaybeConsumed meets is exercised at every pair.
    /// Property tests below sample every (state × state) and every
    /// (state × state × state) tuple — the lattice has 14 elements, so the
    /// exhaustive cube remains fast enough to keep in CI.
    fn states_wide() -> Vec<BindingState> {
        vec![
            BindingState::Uninit,
            BindingState::Live,
            BindingState::Discharged(SiteId(1)),
            BindingState::Discharged(SiteId(3)),
            BindingState::Discharged(SiteId(7)),
            BindingState::Discharged(SiteId(11)),
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

    #[test]
    fn unreachable_post_loop_cursor_does_not_report_uninitialised_reads() {
        // A failed sub-lowering can leave a loop with no `break` edge while
        // preserving its post-loop cursor as a structural CFG home.  The
        // cursor is unreachable; it must not turn a pre-loop binding into a
        // spurious source diagnostic merely because its entry state is empty.
        let acc = BindingId(33);
        let blocks = vec![
            BasicBlock {
                id: 0,
                statements: vec![MirStatement::Bind {
                    binding: acc,
                    name: "acc".to_string(),
                    site: SiteId(10),
                    ty: ResolvedTy::I64,
                }],
                instructions: vec![],
                terminator: Terminator::Goto { target: 1 },
            },
            bb(1, Terminator::Goto { target: 1 }),
            BasicBlock {
                id: 2,
                statements: vec![MirStatement::Use {
                    binding: acc,
                    name: "acc".to_string(),
                    site: SiteId(20),
                    ty: ResolvedTy::I64,
                    intent: IntentKind::Read,
                }],
                instructions: vec![],
                terminator: Terminator::Return,
            },
        ];

        let result = analyze(&blocks, &TypeClassTable::default(), &[]);
        assert!(
            !result.checks.iter().any(|check| matches!(
                check,
                MirCheck::InitialisedBeforeUse { binding, .. } if *binding == acc
            )),
            "unreachable post-loop cursor must not diagnose a read: {:?}",
            result.checks
        );
        assert!(
            !result.entry_states.contains_key(&2),
            "the diagnostic dataflow pass must not materialise unreachable cursor state"
        );
    }

    /// Build the two-arm join CFG both diverging-continuation tests share:
    /// bb0 branches to an ok arm (bb1, binds `b`, goto join) and a failing
    /// arm (bb2, calls `callee` whose continuation bb4 gotos the join bb3).
    fn panic_join_blocks(
        binding: BindingId,
        callee: &str,
        authority: CallAuthority,
    ) -> Vec<BasicBlock> {
        vec![
            bb(
                0,
                Terminator::Branch {
                    cond: Place::Local(0),
                    then_target: 1,
                    else_target: 2,
                },
            ),
            BasicBlock {
                id: 1,
                statements: vec![MirStatement::Bind {
                    binding,
                    name: "payload".to_string(),
                    site: SiteId(10),
                    ty: ResolvedTy::String,
                }],
                instructions: vec![],
                terminator: Terminator::Goto { target: 3 },
            },
            bb(
                2,
                Terminator::Call {
                    callee: callee.to_string(),
                    authority,
                    args: vec![],
                    dest: None,
                    next: 4,
                },
            ),
            bb(3, Terminator::Return),
            bb(4, Terminator::Goto { target: 3 }),
        ]
    }

    #[test]
    fn diverging_call_continuation_does_not_kill_join_liveness() {
        // The Err-arm-panics match shape: `Ok(x)` binds on one arm, the
        // sibling arm calls the never-returning panic shim and its poison
        // continuation gotos the join. The continuation never executes, so
        // its Uninit contribution must not kill the ok-arm binding at the
        // join — that false meet moved the binding's composite release from
        // the function exit to the arm edge, freeing the record before the
        // join's field loads read it (the net.connect_timeout double-free).
        let b = BindingId(40);
        let blocks = panic_join_blocks(b, "hew_panic_msg", CallAuthority::NoReturnExtern);
        let result = analyze(&blocks, &TypeClassTable::default(), &[]);
        assert_eq!(
            result.entry_states.get(&3).and_then(|m| m.get(&b)).copied(),
            Some(BindingState::Live),
            "a never-executing panic continuation must not poison the join meet"
        );
    }

    #[test]
    fn param_use_inside_dead_post_panic_region_is_not_uninitialised() {
        // Dead-region scoping: a block that only runs after an unconditional
        // panic still inherits its predecessor states through the diverging
        // continuation, so a parameter (or binding) read there is diagnosed
        // exactly as before the executable-join exclusion — never as a false
        // `InitialisedBeforeUse`. The nested-suspending-closure shape
        // (`panic(...)` first, parameter read after) compiles because of this.
        let p = BindingId(50);
        let blocks = vec![
            bb(
                0,
                Terminator::Call {
                    callee: "hew_panic_msg".to_string(),
                    authority: CallAuthority::NoReturnExtern,
                    args: vec![],
                    dest: None,
                    next: 1,
                },
            ),
            BasicBlock {
                id: 1,
                statements: vec![MirStatement::Use {
                    binding: p,
                    name: "inner_value".to_string(),
                    site: SiteId(20),
                    ty: ResolvedTy::String,
                    intent: IntentKind::Read,
                }],
                instructions: vec![],
                terminator: Terminator::Return,
            },
        ];
        let result = analyze(&blocks, &TypeClassTable::default(), &[p]);
        assert!(
            !result.checks.iter().any(|check| matches!(
                check,
                MirCheck::InitialisedBeforeUse { binding, .. } if *binding == p
            )),
            "a parameter read in the dead post-panic region must not diagnose \
             InitialisedBeforeUse: {:?}",
            result.checks
        );
    }

    #[test]
    fn ordinary_call_continuation_still_contributes_uninit_at_join() {
        // Control: a continuation of a NORMAL call really executes, so the
        // one-arm-only binding is genuinely absent on that path and the meet
        // must stay Uninit — dropping it at the join would read a slot the
        // other path never initialised.
        let b = BindingId(41);
        let blocks = panic_join_blocks(b, "hew_string_concat", CallAuthority::Direct);
        let result = analyze(&blocks, &TypeClassTable::default(), &[]);
        assert_eq!(
            result.entry_states.get(&3).and_then(|m| m.get(&b)).copied(),
            None,
            "an executable continuation path must keep the binding Uninit at the join"
        );
    }

    #[test]
    fn tail_move_to_return_slot_consumes_the_named_backing_binding() {
        // Tail returns do not emit a consuming `Use`; the physical move is the
        // authority.  Keep this small counterexample here so return cleanup
        // cannot silently regress to treating the escaped owner as live.
        let binding = BindingId(34);
        let blocks = vec![BasicBlock {
            id: 0,
            statements: vec![
                MirStatement::Bind {
                    binding,
                    name: "value".to_string(),
                    site: SiteId(10),
                    ty: ResolvedTy::I64,
                },
                MirStatement::Return {
                    site: Some(SiteId(11)),
                    ty: ResolvedTy::I64,
                },
            ],
            instructions: vec![Instr::Move {
                dest: Place::ReturnSlot,
                src: Place::Local(9),
            }],
            terminator: Terminator::Return,
        }];
        let binding_locals = HashMap::from([(binding, Place::Local(9))]);

        let result =
            analyze_with_binding_locals(&blocks, &TypeClassTable::default(), &[], &binding_locals);
        assert_eq!(
            result.exit_states[&0][&binding],
            BindingState::Consumed(SiteId(11))
        );
    }

    #[test]
    fn affine_discharge_suppresses_exit_drop_but_permits_closed_handle_read() {
        let binding = BindingId(35);
        let blocks = vec![BasicBlock {
            id: 0,
            statements: vec![
                MirStatement::Bind {
                    binding,
                    name: "socket".to_string(),
                    site: SiteId(10),
                    ty: ResolvedTy::String,
                },
                MirStatement::Use {
                    binding,
                    name: "socket".to_string(),
                    site: SiteId(11),
                    ty: ResolvedTy::String,
                    intent: IntentKind::Discharge,
                },
                MirStatement::Use {
                    binding,
                    name: "socket".to_string(),
                    site: SiteId(12),
                    ty: ResolvedTy::String,
                    intent: IntentKind::Read,
                },
            ],
            instructions: vec![],
            terminator: Terminator::Return,
        }];

        let result = analyze(&blocks, &TypeClassTable::default(), &[]);
        assert_eq!(
            result.exit_states[&0][&binding],
            BindingState::Discharged(SiteId(11))
        );
        assert!(
            !result.checks.iter().any(|check| matches!(
                check,
                MirCheck::UseAfterConsume { binding: used, used_at, .. }
                    if *used == binding && *used_at == SiteId(12)
            )),
            "a non-consuming closed-handle probe must stay legal: {:?}",
            result.checks
        );
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

    /// SIR block arguments lower through edge-forwarding blocks. Those blocks
    /// are allocated after the source CFG, so their numeric ids can be greater
    /// than the original arm/join targets even though the overall graph is
    /// acyclic. SIR's opt-in structural scheduler must use graph structure
    /// rather than that incidental allocation order.
    ///
    ///   bb0: Branch { then: bb4, else: bb5 }
    ///   bb4: Goto bb1     // high-id edge forwarder, not a loop
    ///   bb5: Goto bb2     // high-id edge forwarder, not a loop
    ///   bb1: Goto bb6
    ///   bb2: Goto bb7
    ///   bb6: Goto bb3     // high-id edge forwarder into the join
    ///   bb7: Goto bb3     // high-id edge forwarder into the join
    ///   bb3: Return
    ///
    /// The statement threshold makes this non-leaf so the assertion proves
    /// that only the entry site remains, rather than passing through the leaf
    /// fast path.
    #[test]
    fn acyclic_high_id_edge_forwarders_do_not_create_structural_loop_sites() {
        let blocks = vec![
            bb_with_stmts(
                0,
                Terminator::Branch {
                    cond: Place::Local(0),
                    then_target: 4,
                    else_target: 5,
                },
                LEAF_STATEMENT_THRESHOLD,
            ),
            bb(1, Terminator::Goto { target: 6 }),
            bb(2, Terminator::Goto { target: 7 }),
            bb(3, Terminator::Return),
            bb(4, Terminator::Goto { target: 1 }),
            bb(5, Terminator::Goto { target: 2 }),
            bb(6, Terminator::Goto { target: 3 }),
            bb(7, Terminator::Goto { target: 3 }),
        ];

        let sites = compute_structural_cooperate_sites(&blocks);
        assert_eq!(
            sites,
            vec![CooperateSite {
                bb_id: 0,
                kind: CooperateKind::FunctionEntry,
            }],
            "acyclic forwarding edges must not be treated as scheduler loop latches: {sites:?}"
        );
    }

    /// The public raw-MIR scheduler retains its historical numeric rule even
    /// for a CFG that a newer SIR producer would schedule structurally. This
    /// is an explicit compatibility seam: ownership/drop cancellation plans
    /// authored by legacy lowering cannot change merely because SIR gained a
    /// better CFG analysis.
    #[test]
    fn legacy_scheduler_retains_numeric_high_id_goto_sites() {
        let blocks = vec![
            bb_with_stmts(
                0,
                Terminator::Branch {
                    cond: Place::Local(0),
                    then_target: 4,
                    else_target: 5,
                },
                LEAF_STATEMENT_THRESHOLD,
            ),
            bb(1, Terminator::Goto { target: 6 }),
            bb(2, Terminator::Goto { target: 7 }),
            bb(3, Terminator::Return),
            bb(4, Terminator::Goto { target: 1 }),
            bb(5, Terminator::Goto { target: 2 }),
            bb(6, Terminator::Goto { target: 3 }),
            bb(7, Terminator::Goto { target: 3 }),
        ];

        assert_eq!(
            compute_cooperate_sites(&blocks),
            vec![
                CooperateSite {
                    bb_id: 0,
                    kind: CooperateKind::FunctionEntry,
                },
                CooperateSite {
                    bb_id: 4,
                    kind: CooperateKind::LoopBackEdge,
                },
                CooperateSite {
                    bb_id: 5,
                    kind: CooperateKind::LoopBackEdge,
                },
                CooperateSite {
                    bb_id: 6,
                    kind: CooperateKind::LoopBackEdge,
                },
                CooperateSite {
                    bb_id: 7,
                    kind: CooperateKind::LoopBackEdge,
                },
            ],
            "the legacy raw-MIR scheduler remains numeric by contract"
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
