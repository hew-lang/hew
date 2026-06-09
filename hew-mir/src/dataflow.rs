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
//! Worklist over `BlockId`. `entry[bb] = ⊓ exit[p] for p in preds(bb)`.
//! Initial `entry[entry_block] = {}` (empty map = every binding
//! implicitly `Uninit`). The CFGs constructed by Slice 2 are acyclic
//! (no loops in v0.5; `R-CFG.X-loops` defers them), so fixpoint
//! terminates in one RPO pass. The worklist shape is a
//! forward-compatibility hatch for the loop cluster.

use std::collections::{BTreeMap, HashMap, HashSet, VecDeque};

use hew_hir::{BindingId, IntentKind, SiteId, TypeClassTable, ValueClass};
use hew_types::ResolvedTy;

use crate::model::{BasicBlock, MirCheck, MirStatement, Terminator};

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
}

/// Meet over the four-state lattice. Commutative, associative,
/// idempotent — property-tested.
#[must_use]
pub fn meet(a: BindingState, b: BindingState) -> BindingState {
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
    }
}

fn min_site(a: SiteId, b: SiteId) -> SiteId {
    if a.0 <= b.0 {
        a
    } else {
        b
    }
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
                    | BindingState::MaybeConsumed(consumed_at) => {
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
                if *intent == IntentKind::Consume
                    && ValueClass::of_ty(ty, type_classes) != ValueClass::BitCopy
                {
                    state.insert(*binding, BindingState::Consumed(*site));
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
) -> BTreeMap<BindingId, BindingState> {
    if preds.is_empty() {
        return BTreeMap::new();
    }
    let mut all_bindings: HashSet<BindingId> = HashSet::new();
    for p in preds {
        if let Some(s) = exit_states.get(p) {
            for k in s.keys() {
                all_bindings.insert(*k);
            }
        }
    }
    let mut entry = BTreeMap::new();
    for binding in all_bindings {
        // Meet across predecessors. Absent-from-map = `Uninit` on
        // that path. Order is deterministic on `preds` slice order;
        // meet is commutative + associative (property-tested), so
        // any permutation produces the same result.
        let acc = preds
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

fn build_preds(blocks: &[BasicBlock]) -> HashMap<u32, Vec<u32>> {
    let mut preds: HashMap<u32, Vec<u32>> = HashMap::new();
    for block in blocks {
        let mut emit_edge = |target: u32| preds.entry(target).or_default().push(block.id);
        match &block.terminator {
            Terminator::Return | Terminator::Panic => {}
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
            | Terminator::Send { next, .. }
            | Terminator::Select { next, .. } => emit_edge(*next),
        }
    }
    preds
}

fn successors(block: &BasicBlock) -> Vec<u32> {
    match &block.terminator {
        Terminator::Return | Terminator::Panic => Vec::new(),
        Terminator::Goto { target } => vec![*target],
        Terminator::Branch {
            then_target,
            else_target,
            ..
        } => vec![*then_target, *else_target],
        Terminator::Call { next, .. }
        | Terminator::Yield { next, .. }
        | Terminator::Send { next, .. }
        | Terminator::Select { next, .. } => vec![*next],
    }
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
    analyze(blocks, type_classes).checks
}

/// Run the full dataflow pass and return both diagnostics and the
/// per-block exit-state map. The elaborator uses `exit_states` to
/// filter the function-wide LIFO drop sequence down to per-exit
/// live sets (plan §5 Slice 4: "per-exit drop list with Place
/// threading").
#[must_use]
pub fn analyze(blocks: &[BasicBlock], type_classes: &TypeClassTable) -> DataflowResult {
    if blocks.is_empty() {
        return DataflowResult::default();
    }
    let preds = build_preds(blocks);
    let by_id: HashMap<u32, &BasicBlock> = blocks.iter().map(|b| (b.id, b)).collect();

    // The function's entry block is id 0 by construction (see
    // `lower::Builder::finalize_blocks`).
    let entry_id = 0;

    let mut exit_states: HashMap<u32, BTreeMap<BindingId, BindingState>> = HashMap::new();
    let mut linear_bindings: BTreeMap<BindingId, (String, ResolvedTy)> = BTreeMap::new();
    let mut checks: Vec<MirCheck> = Vec::new();
    let mut use_after_consume_seen: HashSet<(BindingId, SiteId)> = HashSet::new();
    let mut init_before_use_seen: HashSet<(BindingId, SiteId)> = HashSet::new();

    // Worklist seeded with every block, processed in id order so the
    // first sweep matches RPO on the acyclic CFGs Slice 2 builds.
    let mut worklist: VecDeque<u32> = blocks.iter().map(|b| b.id).collect();

    while let Some(bb_id) = worklist.pop_front() {
        let Some(block) = by_id.get(&bb_id) else {
            continue;
        };
        let entry = if bb_id == entry_id {
            BTreeMap::new()
        } else {
            let empty = Vec::new();
            let preds_of_bb = preds.get(&bb_id).unwrap_or(&empty);
            meet_predecessors(preds_of_bb, &exit_states)
        };
        // Clear per-block diagnostic dedupe state? No — the seen-sets
        // are function-wide so re-visits during fixpoint don't
        // duplicate. The transfer function checks-then-inserts into
        // the seen set.
        let new_exit = transfer_block(
            entry,
            block,
            type_classes,
            &mut linear_bindings,
            &mut checks,
            &mut use_after_consume_seen,
            &mut init_before_use_seen,
        );
        let changed = exit_states.get(&bb_id).is_none_or(|prev| *prev != new_exit);
        exit_states.insert(bb_id, new_exit);
        if changed {
            for succ in successors(block) {
                worklist.push_back(succ);
            }
        }
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
                BindingState::Live | BindingState::MaybeConsumed(_) | BindingState::Uninit
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

    DataflowResult {
        checks,
        exit_states,
    }
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
