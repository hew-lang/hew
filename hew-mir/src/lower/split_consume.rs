#[cfg(test)]
use super::*;
#[cfg(not(test))]
use super::{
    exit_block_id, ty_is_indirect_enum, BTreeMap, BasicBlock, BindingId, BlockKind, ElabDrop,
    ElaboratedMirFunction, FieldBinderProvenance, HashMap, HashSet, HirExpr, HirExprKind, Instr,
    MatchBoundHopAliasFacts, MirCheck, Place, ResolvedRef, ResolvedTy, Terminator,
};

/// Parent-local id for a Duplex-family Place. `DuplexHandle(N)`,
/// `SendHalf(N)`, and `RecvHalf(N)` all reference the same parent
/// Duplex local `N`; the variants only differ in which directions
/// the drop closes. Returns `None` for non-Duplex-family Places.
#[must_use]
fn duplex_parent_local(place: Place) -> Option<u32> {
    match place {
        Place::DuplexHandle(n) | Place::SendHalf(n) | Place::RecvHalf(n) => Some(n),
        Place::LambdaActorHandle(_)
        | Place::ActorHandle(_)
        | Place::Local(_)
        | Place::ReturnSlot
        // Machine sub-structure places are not Duplex-family; they have no
        // parent Duplex local. MachineTag and MachineVariant are part of the
        // machine tagged-union, not the M2 duplex substrate. EnumTag and
        // EnumVariant share the same shape for user-declared enums.
        | Place::MachineTag(_)
        | Place::MachineVariant { .. }
        | Place::EnumTag(_)
        | Place::EnumVariant { .. } => None,
    }
}
/// Consume-on-split invariant for `Duplex<S, R>` handles.
///
/// A `Duplex` may be addressed by either the unified `DuplexHandle`
/// (close-both-dirs) or by the pair of direction-aliases
/// `SendHalf` / `RecvHalf` (close-one-dir each). The split operation
/// (`.send_half()` / `.recv_half()`) MOVES the underlying handle: after
/// a split, the original `DuplexHandle` binding is uninhabited and only
/// the half-handle binding(s) remain in the drop plan. If a stale
/// `DuplexHandle(N)` coexists with `SendHalf(N)` or `RecvHalf(N)` in the
/// same drop plan, codegen would emit close-both on the unified handle
/// AND close-one on the half — closing the same direction twice and, in
/// the runtime's refcount discipline, closing a direction that the
/// half-handle binding now owns. Same shape if two `SendHalf(N)` or two
/// `RecvHalf(N)` entries coexist (the half-handle is also a moved
/// resource; aliased drops would close the same queue twice).
///
/// The plan is sound iff, for each parent local `N`, the drop plan
/// contains at most one of `{DuplexHandle(N), <SendHalf(N) + RecvHalf(N)
/// pair>, SendHalf(N) alone, RecvHalf(N) alone}` — never a mix and
/// never duplicates within a half-class.
///
/// LESSONS: raii-null-after-move (consume-on-split is the affine
/// move-checker discipline expressed at the drop-plan layer),
/// cleanup-all-exits (each direction closes exactly once per exit
/// path), boundary-fail-closed (a stale-handle drop plan is rejected
/// before codegen observes it).
///
/// NOTE: the HIR construction surface for `.send_half()` /
/// `.recv_half()` is slice-4 work (`hew-types/src/builtin_names.rs:270`
/// WHEN-OBSOLETE marker). Until that surface exists, the upstream
/// lowering never EMITS a drop plan that violates the invariant; this
/// check is the structural backstop that fails closed when slice 4
/// wires the split methods through MIR. The synthetic property tests
/// in `slice3_invariants` exercise every legal and illegal split-state
/// shape against this checker today.
pub(super) fn check_duplex_split_state(
    block: u32,
    drops: &[ElabDrop],
    findings: &mut Vec<MirCheck>,
) {
    #[derive(Default)]
    struct PerParent {
        whole: u32,
        send: u32,
        recv: u32,
    }

    let mut per_parent: BTreeMap<u32, PerParent> = BTreeMap::new();
    for drop in drops {
        let Some(parent) = duplex_parent_local(drop.place) else {
            continue;
        };
        let entry = per_parent.entry(parent).or_default();
        match drop.place {
            Place::DuplexHandle(_) => entry.whole = entry.whole.saturating_add(1),
            Place::SendHalf(_) => entry.send = entry.send.saturating_add(1),
            Place::RecvHalf(_) => entry.recv = entry.recv.saturating_add(1),
            _ => {}
        }
    }

    for (parent, counts) in per_parent {
        // Whole + any half is the "stale unified handle after split"
        // bug: split would have moved the DuplexHandle out, so its
        // presence alongside a half means codegen would close the same
        // direction twice.
        if counts.whole > 0 && (counts.send > 0 || counts.recv > 0) {
            findings.push(MirCheck::DropPlanUndetermined {
                block,
                reason: format!(
                    "drop plan contains both DuplexHandle({parent}) and a half-handle \
                     (send={}, recv={}) for the same parent local; the split \
                     operation must consume the DuplexHandle (slice-4 HIR seam \
                     for .send_half() / .recv_half() must MOVE the unified handle)",
                    counts.send, counts.recv,
                ),
            });
        }
        // Multiple DuplexHandle entries for the same parent: should be
        // structurally impossible (one binding per local), but reject
        // defensively so a duplicated drop emission is caught.
        if counts.whole > 1 {
            findings.push(MirCheck::DropPlanUndetermined {
                block,
                reason: format!(
                    "drop plan contains {} DuplexHandle({parent}) entries for the \
                     same parent local; close-both-dirs must fire exactly once",
                    counts.whole,
                ),
            });
        }
        // Aliased halves: two SendHalf(N) or two RecvHalf(N) would
        // close the same queue twice. The split methods return a
        // single half per direction, so duplicates are a lowering bug.
        if counts.send > 1 {
            findings.push(MirCheck::DropPlanUndetermined {
                block,
                reason: format!(
                    "drop plan contains {} SendHalf({parent}) entries; \
                     the S-direction must close exactly once",
                    counts.send,
                ),
            });
        }
        if counts.recv > 1 {
            findings.push(MirCheck::DropPlanUndetermined {
                block,
                reason: format!(
                    "drop plan contains {} RecvHalf({parent}) entries; \
                     the R-direction must close exactly once",
                    counts.recv,
                ),
            });
        }
    }
}
/// Per-`Duplex` consume state at a program point. Tracks the
/// affine move-checker discipline expressed at the backend
/// instruction layer: a `.send_half()` / `.recv_half()` split
/// emits an `Instr::Move { src: Place::DuplexHandle(N), dest:
/// Place::SendHalf(N) | Place::RecvHalf(N) }`, which moves the
/// unified handle out and leaves only the half-handle binding(s)
/// live in the drop plan.
///
/// Lattice semantics over the CFG meet:
///   - `Live ⊓ Live = Live`
///   - `Live ⊓ Consumed = MaybeConsumed` (some-path-consumed)
///   - `Consumed ⊓ Consumed = Consumed`
///   - `MaybeConsumed ⊓ X = MaybeConsumed`
///
/// Either `Consumed` or `MaybeConsumed` at a block whose drop plan
/// contains `DuplexHandle(N)` is rejected — `Consumed` because the
/// drop is structurally stale, `MaybeConsumed` because the program
/// could close the same direction twice on some paths
/// (fail-closed for ambiguous shapes per the M2 substrate's
/// `boundary-fail-closed` discipline).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DuplexSplitState {
    /// No `.send_half()` / `.recv_half()` consume of the unified
    /// handle has been observed on any reaching path.
    Live,
    /// The unified handle was moved on every reaching path; carries
    /// the earliest split-emitting block id for diagnostic anchoring.
    Consumed(u32),
    /// The unified handle was moved on some-but-not-all reaching
    /// paths; the drop plan cannot decide whether to fire the close.
    /// Carries the earliest split-emitting block id.
    MaybeConsumed(u32),
}
impl DuplexSplitState {
    /// Lattice meet over the three-state space. Commutative,
    /// associative, idempotent — pinned by tests below.
    fn meet(self, other: DuplexSplitState) -> DuplexSplitState {
        use DuplexSplitState::{Consumed, Live, MaybeConsumed};
        match (self, other) {
            (Live, Live) => Live,
            (Live, Consumed(b) | MaybeConsumed(b)) | (Consumed(b) | MaybeConsumed(b), Live) => {
                MaybeConsumed(b)
            }
            (Consumed(a), Consumed(b)) => Consumed(a.min(b)),
            (Consumed(a), MaybeConsumed(b)) | (MaybeConsumed(a), Consumed(b)) => {
                MaybeConsumed(a.min(b))
            }
            (MaybeConsumed(a), MaybeConsumed(b)) => MaybeConsumed(a.min(b)),
        }
    }
}
/// Walk the backend `Instr` stream across a function's CFG and
/// reject drop plans that fire `Place::DuplexHandle(N)` on a block
/// whose reaching paths have already moved the unified handle into
/// a `SendHalf` / `RecvHalf` split.
///
/// The slice-3 structural checker (`check_duplex_split_state`) catches
/// only same-drop-list collisions of `DuplexHandle(N)` with
/// `SendHalf(N)` / `RecvHalf(N)`. It cannot detect the cross-block
/// case where block A emits the split and block B (a successor)
/// drops the now-stale `DuplexHandle(N)`. This dataflow closes the
/// gap.
///
/// The transfer function scans each block's instructions for
/// `Instr::Move { src: Place::DuplexHandle(N), dest:
/// Place::SendHalf(N) | Place::RecvHalf(N) }` and transitions the
/// parent's state to `Consumed(block_id)`. Inter-block, the entry
/// state of each block is the meet over predecessor exit states;
/// the entry block starts with every parent `Live`.
///
/// The CFG is acyclic in v0.5 (the loop cluster is deferred), so
/// the worklist terminates in one RPO pass. The check then iterates
/// every `(ExitPath, DropPlan)` plus every cleanup
/// `ElabBlock.drops`, looking for `DuplexHandle(N)` drops whose
/// owning block's exit state is `Consumed` (definitely stale) or
/// `MaybeConsumed` (ambiguous). Either case produces a
/// `MirCheck::DropPlanUndetermined`.
///
/// Cleanup blocks inherit the state of the normal block they
/// trap-cleanup from — `enumerate_exits` clones the normal-path
/// `drops_template` unfiltered into the cleanup block's drops, so a
/// `DuplexHandle` left in the cleanup block's drop list reflects a
/// program shape where the normal-path consume already happened
/// but the cleanup path was forgotten. Validate against the normal
/// predecessor's exit state.
///
/// LESSONS: cleanup-all-exits, raii-null-after-move,
/// boundary-fail-closed.
#[allow(
    clippy::too_many_lines,
    reason = "single fixpoint + drop-list-walk; splitting would obscure the dataflow"
)]
pub(super) fn validate_cross_block_split_consume(
    blocks: &[BasicBlock],
    elab: &ElaboratedMirFunction,
) -> Vec<MirCheck> {
    use std::collections::VecDeque;

    let mut findings = Vec::new();
    if blocks.is_empty() {
        return findings;
    }

    // Per-block per-parent exit state. Absent entries are implicitly
    // `Live` — the most-permissive default. The map carries only
    // parents whose state diverged from `Live` on at least one
    // reaching path; this keeps the lattice sparse.
    let mut exit_states: HashMap<u32, BTreeMap<u32, DuplexSplitState>> = HashMap::new();
    let by_id: HashMap<u32, &BasicBlock> = blocks.iter().map(|b| (b.id, b)).collect();

    // Predecessor edges for the meet step. Route through the canonical
    // `BasicBlock::successors()` so Select arm bodies (and all other real
    // CFG successors) are not silently dropped. The predecessor map is the
    // inverse of the successor relation: for each block B and each S in
    // B.successors(), B is a predecessor of S.
    let mut preds: HashMap<u32, Vec<u32>> = HashMap::new();
    for block in blocks {
        for succ in block.successors() {
            preds.entry(succ).or_default().push(block.id);
        }
    }

    // Forward fixpoint. The entry block is id 0 by construction.
    let entry_id = 0;
    let mut worklist: VecDeque<u32> = blocks.iter().map(|b| b.id).collect();
    let mut all_parents: HashSet<u32> = HashSet::new();
    // Seed all_parents from any DuplexHandle/Half-handle place that
    // appears in any block's instructions — those are the only
    // parents the cross-block dataflow needs to track.
    for block in blocks {
        for instr in &block.instructions {
            for place in instr_places(instr) {
                if let Some(parent) = duplex_parent_local(place) {
                    all_parents.insert(parent);
                }
            }
        }
    }
    if all_parents.is_empty() {
        return findings;
    }

    while let Some(bb_id) = worklist.pop_front() {
        let Some(block) = by_id.get(&bb_id) else {
            continue;
        };
        let entry: BTreeMap<u32, DuplexSplitState> = if bb_id == entry_id {
            BTreeMap::new()
        } else {
            let empty = Vec::new();
            let predecessors = preds.get(&bb_id).unwrap_or(&empty);
            meet_predecessors_split(predecessors, &exit_states, &all_parents)
        };
        let new_exit = transfer_block_split(entry, block);
        let changed = exit_states.get(&bb_id).is_none_or(|prev| *prev != new_exit);
        exit_states.insert(bb_id, new_exit);
        if changed {
            for succ in block.successors() {
                worklist.push_back(succ);
            }
        }
    }

    // Inspect every drop plan / cleanup-block-drop list against the
    // owning block's exit state for the relevant parent. A stale
    // unified-handle drop (`Consumed` or `MaybeConsumed`) is rejected.
    let report_drop = |block: u32, drops: &[ElabDrop], findings: &mut Vec<MirCheck>| {
        let Some(state_map) = exit_states.get(&block) else {
            return;
        };
        for drop in drops {
            let Place::DuplexHandle(parent) = drop.place else {
                continue;
            };
            let state = state_map
                .get(&parent)
                .copied()
                .unwrap_or(DuplexSplitState::Live);
            match state {
                DuplexSplitState::Live => {}
                DuplexSplitState::Consumed(consume_block) => {
                    findings.push(MirCheck::DropPlanUndetermined {
                        block,
                        reason: format!(
                            "drop plan contains DuplexHandle({parent}) but block {consume_block} \
                             on every reaching path moves the unified handle into a half-handle \
                             (split via .send_half()/.recv_half() consumes the DuplexHandle; \
                             the half-handle binding is the only remaining owner)"
                        ),
                    });
                }
                DuplexSplitState::MaybeConsumed(consume_block) => {
                    findings.push(MirCheck::DropPlanUndetermined {
                        block,
                        reason: format!(
                            "drop plan contains DuplexHandle({parent}) but block {consume_block} \
                             on some reaching paths moves the unified handle into a half-handle \
                             (split-on-some-paths leaves the drop ambiguous; fail-closed per \
                             cleanup-all-exits)"
                        ),
                    });
                }
            }
        }
    };

    for (exit, plan) in &elab.drop_plans {
        let block = exit_block_id(exit);
        report_drop(block, &plan.drops, &mut findings);
    }
    // Cleanup blocks: validate against the cleanup block's predecessor
    // (the normal block that trapped into it). The cleanup block id
    // itself is past the highest normal-block id and has no
    // predecessor entry in `exit_states`; consult the normal block
    // whose `Terminator::Panic` produced the cleanup edge.
    for elab_block in &elab.blocks {
        if elab_block.kind != BlockKind::Cleanup {
            continue;
        }
        // Find the normal block that traps into this cleanup. The
        // `Terminator::Panic` path generated cleanup ids past the
        // max normal id; the cleanup's drops mirror the normal
        // block's pre-terminator state. Without a back-reference,
        // we conservatively validate against EVERY normal block whose
        // Terminator is Panic (the elab structure has one cleanup
        // per Panic). For each, report any stale DuplexHandle drop.
        for raw_block in blocks {
            if matches!(raw_block.terminator, Terminator::Trap { .. }) {
                report_drop(raw_block.id, &elab_block.drops, &mut findings);
            }
        }
    }

    findings
}
fn meet_predecessors_split(
    preds: &[u32],
    exit_states: &std::collections::HashMap<u32, std::collections::BTreeMap<u32, DuplexSplitState>>,
    all_parents: &std::collections::HashSet<u32>,
) -> std::collections::BTreeMap<u32, DuplexSplitState> {
    if preds.is_empty() {
        return BTreeMap::new();
    }
    let mut entry = BTreeMap::new();
    for &parent in all_parents {
        let acc = preds
            .iter()
            .map(|p| {
                exit_states
                    .get(p)
                    .and_then(|m| m.get(&parent).copied())
                    .unwrap_or(DuplexSplitState::Live)
            })
            .reduce(DuplexSplitState::meet)
            .unwrap_or(DuplexSplitState::Live);
        if !matches!(acc, DuplexSplitState::Live) {
            entry.insert(parent, acc);
        }
    }
    entry
}
fn transfer_block_split(
    entry: std::collections::BTreeMap<u32, DuplexSplitState>,
    block: &BasicBlock,
) -> std::collections::BTreeMap<u32, DuplexSplitState> {
    let mut state = entry;
    for instr in &block.instructions {
        if let Instr::Move { dest, src } = instr {
            if let (Place::DuplexHandle(parent), true) = (
                *src,
                matches!(dest, Place::SendHalf(_) | Place::RecvHalf(_)),
            ) {
                // Transition the parent to Consumed by this block. A
                // prior MaybeConsumed/Consumed entry is overwritten —
                // the move-checker upstream has already determined
                // whether a re-use after consume is legal; for the
                // drop-plan check, the latest consume in this block
                // is the relevant anchor.
                state.insert(parent, DuplexSplitState::Consumed(block.id));
            }
        }
    }
    state
}
/// Return every `Place` mentioned by a backend `Instr`. Used by the
/// cross-block split-state seed pass to discover which parent
/// locals participate in the dataflow.
#[allow(
    clippy::match_same_arms,
    clippy::too_many_lines,
    reason = "i64 and float arithmetic arms share the same place-extraction shape but \
              represent semantically distinct ops; consolidating would force a later \
              re-split when codegen needs per-op dispatch. The match must remain exhaustive \
              across the full Instr surface, so line count grows with every new variant"
)]
fn instr_places(instr: &Instr) -> Vec<Place> {
    match instr {
        Instr::EnterContext | Instr::ExitContext | Instr::CheckCancellation => Vec::new(),
        Instr::ContextField { dest, .. } => vec![*dest],
        // Const-like producers write only their dest place.
        Instr::ConstI64 { dest, .. }
        | Instr::StringLit { dest, .. }
        | Instr::BytesLit { dest, .. }
        | Instr::ConstGlobalLoad { dest, .. } => vec![*dest],
        Instr::IntAdd { dest, lhs, rhs }
        | Instr::IntSub { dest, lhs, rhs }
        | Instr::IntMul { dest, lhs, rhs }
        | Instr::IntArithCheckedOption { dest, lhs, rhs, .. }
        | Instr::IntArithSaturating { dest, lhs, rhs, .. }
        | Instr::IntDiv { dest, lhs, rhs, .. }
        | Instr::IntRem { dest, lhs, rhs, .. }
        | Instr::IntBitAnd { dest, lhs, rhs }
        | Instr::IntBitOr { dest, lhs, rhs }
        | Instr::IntBitXor { dest, lhs, rhs }
        | Instr::IntShl { dest, lhs, rhs }
        | Instr::IntShr { dest, lhs, rhs, .. }
        | Instr::IntCmp { dest, lhs, rhs, .. }
        | Instr::FloatCmp { dest, lhs, rhs, .. }
        | Instr::IdentityCompare { dest, lhs, rhs } => vec![*dest, *lhs, *rhs],
        Instr::CancellationTokenIsCancelled { dest, token } => vec![*dest, *token],
        Instr::RcIntrinsic {
            dest,
            receiver,
            value,
            ..
        } => std::iter::once(*dest)
            .chain(receiver.iter().copied())
            .chain(value.iter().copied())
            .collect(),
        Instr::GeneratorNext { dest, ctx, .. } => vec![*dest, *ctx],
        Instr::WireCodec { dest, operand, .. } => vec![*dest, *operand],
        Instr::RecordCloneInplace { dest, src, .. } => vec![*dest, *src],
        Instr::EnumCloneInplace { dest, src, .. } => vec![*dest, *src],
        Instr::ValueSnapshotClone { dest, src, .. } => vec![*dest, *src],
        Instr::ValueSnapshotDrop { value, .. } => vec![*value],
        Instr::IntArithChecked {
            dest,
            lhs,
            rhs,
            overflow_flag,
            ..
        } => vec![*dest, *lhs, *rhs, *overflow_flag],
        Instr::BoolNot { dest, operand }
        | Instr::FloatNeg { dest, operand, .. }
        | Instr::IntBitNot { dest, operand } => vec![*dest, *operand],
        Instr::IntNegChecked {
            dest,
            operand,
            overflow_flag,
            ..
        } => vec![*dest, *operand, *overflow_flag],
        Instr::Move { dest, src } => vec![*dest, *src],
        Instr::BytesRetain { value } | Instr::StringRetain { value } => vec![*value],
        Instr::NumericCast { dest, src, .. }
        | Instr::SaturatingWidthCast { dest, src, .. }
        | Instr::TryWidthCast { dest, src, .. } => {
            vec![*dest, *src]
        }
        Instr::Drop { place, .. } => vec![*place],
        Instr::WitnessSizeOf { dest, .. } | Instr::WitnessAlignOf { dest, .. } => vec![*dest],
        Instr::WitnessDropGlue { place, .. } => vec![*place],
        Instr::WitnessMove { dest, src, .. } => vec![*dest, *src],
        Instr::CallRuntimeAbi(call) => {
            // Every Place participating in the runtime call surfaces
            // here so the cross-block split-state seed pass can
            // observe handle moves through C-ABI boundaries. The
            // `dest` (when present) is also a Place the dataflow
            // needs to discover.
            let mut places: Vec<Place> = call.args().to_vec();
            if let Some(d) = call.dest() {
                places.push(d);
            }
            places
        }
        Instr::AutoLockAcquire { lock } | Instr::AutoLockRelease { lock } => {
            // Auto-lock bracketing surfaces the lock-handle place to
            // the cross-block split-state seed pass so a handle that
            // crosses a block boundary remains discoverable.
            vec![*lock]
        }
        Instr::RecordInit { fields, dest, .. } => {
            let mut places: Vec<Place> = fields.iter().map(|(_, p)| *p).collect();
            places.push(*dest);
            places
        }
        Instr::ClosureEnvInit { fields, dest, .. } => {
            let mut places: Vec<Place> = fields.iter().map(|field| field.src).collect();
            places.push(*dest);
            places
        }
        Instr::RecordFieldLoad { record, dest, .. } => vec![*record, *dest],
        Instr::RecordFieldDrop { record, .. } => vec![*record],
        // FieldDropInPlace surfaces only its base aggregate: an interior
        // in-place field release has no dest place to discover.
        Instr::FieldDropInPlace { base, .. } => vec![*base],
        Instr::RecordFieldStore { record, src, .. } => vec![*record, *src],
        Instr::ActorStateFieldLoad { dest, .. } => vec![*dest],
        Instr::ActorStateFieldStore { src, .. } => vec![*src],
        Instr::NeutralizePayloadSlot { place, .. } => vec![*place],
        Instr::AggregateProjectionNeutralize { root, .. } => vec![*root],
        Instr::TupleFieldLoad { tuple, dest, .. } => vec![*tuple, *dest],
        Instr::TupleConstruct { elements, dest } => {
            let mut places = Vec::with_capacity(elements.len() + 1);
            places.extend(elements.iter().copied());
            places.push(*dest);
            places
        }
        Instr::FloatLit { dest, .. } => vec![*dest],
        Instr::FloatAdd { dest, lhs, rhs, .. }
        | Instr::FloatSub { dest, lhs, rhs, .. }
        | Instr::FloatMul { dest, lhs, rhs, .. }
        | Instr::FloatDiv { dest, lhs, rhs, .. }
        | Instr::FloatRem { dest, lhs, rhs, .. } => vec![*dest, *lhs, *rhs],
        // Char, Unit, and Duration literals each produce only their dest place
        // (no operand places). Grouped with the existing dest-only pattern
        // for clarity; kept as separate arms per the `match_same_arms` allow
        // above — they are semantically distinct even if the extraction shape
        // is identical.
        Instr::CharLit { dest, .. } | Instr::UnitLit { dest } | Instr::DurationLit { dest, .. } => {
            vec![*dest]
        }
        Instr::MakeClosure { env, dest, .. } => vec![*env, *dest],
        Instr::ClosureEnvFieldLoad { env, dest, .. } => vec![*env, *dest],
        Instr::ClosureEnvFieldStore { env, src, .. } => vec![*env, *src],
        Instr::CallClosure {
            callee,
            args,
            ret_ty: _,
            dest,
        } => {
            let mut places: Vec<Place> = vec![*callee];
            places.extend(args.iter().copied());
            if let Some(d) = dest {
                places.push(*d);
            }
            places
        }
        Instr::SpawnTaskDirect { task, .. } => vec![*task],
        Instr::SpawnTaskClosure { task, env, .. } => vec![*task, *env],
        Instr::SpawnActor {
            state,
            init_args,
            dest,
            ..
        } => {
            let mut places = Vec::new();
            if let Some(state) = state {
                places.push(*state);
            }
            places.extend(init_args.iter().copied());
            places.push(*dest);
            places
        }
        Instr::CoerceToDynTrait { value, dest, .. } => vec![*value, *dest],
        Instr::CallTraitMethod {
            fat_pointer,
            dest,
            args,
            ..
        } => {
            let mut places: Vec<Place> = vec![*fat_pointer];
            places.extend(args.iter().copied());
            if let Some(d) = dest {
                places.push(*d);
            }
            places
        }
        Instr::MachineEmitPlaceholder { payload, .. } => payload.clone(),
        Instr::EnumTagLoad { src, dest } => vec![*src, *dest],
        Instr::MachineStateName {
            src_local, dest, ..
        } => vec![Place::Local(*src_local), *dest],
        Instr::MachineEmitTake {
            event_tag, dest, ..
        } => vec![*event_tag, *dest],
    }
}
/// Resolve a `Place` to the backing MIR local id it addresses, or `None`
/// for the synthetic `ReturnSlot` (which has no local id). Every
/// handle/projection `Place` variant carries the `u32` local it
/// addresses; this collapses them to that id so the sole-owner scan can
/// reason about aliasing at local granularity.
///
/// W5-011 P3 — used by `derive_cow_sole_owner` to map source operands and
/// projection-alias dests back to the locals they touch.
#[must_use]
pub(super) fn base_local(place: Place) -> Option<u32> {
    match place {
        Place::ReturnSlot => None,
        Place::Local(n)
        | Place::DuplexHandle(n)
        | Place::LambdaActorHandle(n)
        | Place::ActorHandle(n)
        | Place::SendHalf(n)
        | Place::RecvHalf(n)
        | Place::MachineTag(n)
        | Place::EnumTag(n) => Some(n),
        Place::MachineVariant { local, .. } | Place::EnumVariant { local, .. } => Some(local),
    }
}
/// Whole-value alias grouping for the `derive_*_drop_allowed` escape scans.
///
/// Seeds each candidate as its own root (`alias_of[c] = c`) and then runs a
/// fixpoint that forward-propagates a candidate's root through every
/// whole-value `Move { dest: Local, src: Local | ReturnSlot }` copy — a rebind
/// `let m2 = m;` hands the same payload pointer to a new slot with NO retain,
/// so `m2` joins `m`'s alias group. The escape scans that consume this map
/// exclude an alias group's ROOT when any member escapes, and free the cleared
/// root exactly once.
///
/// MONOTONE CONVERGENCE (the #1942 fix). The fixpoint must only ever ADD
/// entries or REMOVE conflicted ones — never REPLACE a slot's root — or it
/// fails to converge. A naive `alias_of.insert(dl, root)` REPLACES the prior
/// value: when one slot `dl` receives `Move`s from two DIFFERENT roots (two
/// match arms each constructing a local collection that moves into the shared
/// result slot), the two roots OSCILLATE — each iteration overwrites the
/// other, `changed` never clears, and lowering spins until SIGKILL (#1942: a
/// 2-arm `Vec`-returning match hung `hew check` at ~9.6s CPU).
///
/// A candidate adopting another group's root for the FIRST time (its slot
/// still holds its own self-seed — the legitimate `let b = a;` merge) is not a
/// conflict; it takes the incoming root once. But a slot that already carries a
/// DIFFERENT non-self root and then sees a second distinct root is reachable
/// from two groups — by definition NOT the sole alias of either. Such a slot is
/// EVICTED from `alias_of` and recorded in `conflicted` so it is never
/// re-added. Each slot therefore transitions at most twice (self-seed/vacant →
/// merged-root → conflicted), so the map is monotone: it only gains entries or
/// sheds conflicted ones, is bounded below by the empty set, and converges in
/// at most O(locals) iterations.
///
/// DROP-SAFETY of eviction (conservative direction — this CANNOT introduce a
/// double-free or a leak the prior code avoided). An evicted slot is absent
/// from `alias_of`, so in the downstream escape scan a `Move` INTO it reads as
/// `dest_is_member == false` — i.e. an ESCAPE of the source — which EXCLUDES
/// the source's root from the allow-set. An excluded root earns NO scope-exit
/// free: it LEAKS (fail-closed), never double-frees. This is exactly the
/// `boundary-fail-closed` direction the escape scans already take for an
/// ambiguous alias group: when ownership cannot be proven to rest in a single
/// local, the prover declines to free. In the #1942 repro the conflicted slot
/// is the match result that is RETURNED to the caller, so excluding the arm
/// locals from the producing function's drops is also the *correct* result
/// (the caller owns and frees the returned handle) — not merely a safe leak.
///
/// LESSONS: `drop-allowset-from-value-flow`, `boundary-fail-closed`.
pub(super) fn propagate_whole_value_alias_roots(
    blocks: &[BasicBlock],
    candidate_locals: impl IntoIterator<Item = u32>,
) -> HashMap<u32, u32> {
    let mut alias_of: HashMap<u32, u32> = HashMap::new();
    for local in candidate_locals {
        alias_of.insert(local, local);
    }
    // Slots reachable from two distinct roots: evicted from `alias_of` and
    // permanently barred from re-entry so the fixpoint stays monotone.
    let mut conflicted: HashSet<u32> = HashSet::new();
    loop {
        let mut changed = false;
        for block in blocks {
            for instr in &block.instructions {
                if let Instr::Move { dest, src } = instr {
                    if let (Some(sl), Some(dl)) = (base_local(*src), base_local(*dest)) {
                        // Only whole-value Local→Local copies propagate the
                        // alias. An interior-projection src is a payload
                        // destructure, handled by the escape scan.
                        if matches!(src, Place::Local(_) | Place::ReturnSlot)
                            && matches!(dest, Place::Local(_))
                        {
                            if conflicted.contains(&dl) {
                                continue;
                            }
                            if let Some(&root) = alias_of.get(&sl) {
                                use std::collections::hash_map::Entry;
                                match alias_of.entry(dl) {
                                    Entry::Vacant(slot) => {
                                        slot.insert(root);
                                        changed = true;
                                    }
                                    // `dl` still holds its own self-seed root: a
                                    // candidate joining another group for the
                                    // FIRST time (`let b = a;`). A one-time merge,
                                    // not a conflict — adopt the incoming root.
                                    Entry::Occupied(mut slot)
                                        if *slot.get() == dl && root != dl =>
                                    {
                                        slot.insert(root);
                                        changed = true;
                                    }
                                    // `dl` already carries a DIFFERENT non-self
                                    // root: it is reachable from two distinct
                                    // groups (two match arms each moving a fresh
                                    // handle into one result slot — #1942). Not
                                    // the sole alias of either; evict and bar it
                                    // so the fixpoint converges (monotone:
                                    // alias_of only gains entries or sheds
                                    // conflicted ones). The evicted slot is a
                                    // non-member in the escape scan, so a Move
                                    // into it EXCLUDES the source's root —
                                    // fail-closed (the ambiguous group leaks,
                                    // never double-frees).
                                    Entry::Occupied(slot) if *slot.get() != root => {
                                        slot.remove();
                                        conflicted.insert(dl);
                                        changed = true;
                                    }
                                    Entry::Occupied(_) => {}
                                }
                            }
                        }
                    }
                }
            }
        }
        if !changed {
            break;
        }
    }
    alias_of
}
/// Owned-field-binder set for a record-candidate alias map: destinations of
/// `RecordFieldLoad { record: alias-set member }` whose loaded field is
/// itself heap-owning, closed forward over whole-value `Move` copies so a
/// binder handed to another slot is still tracked. Shared by
/// `derive_owned_record_drop_allowed` (the composite-drop prover) and
/// `apply_escaped_record_sibling_field_drops` (the #2212 sibling-discharge
/// emitter) so the two agree on what counts as a field binder.
/// Forward whole-value-`Move` closure of the recorded interior-alias binders:
/// starting from each `(alias_local, owner_local)` seed, every local the alias
/// value flows into via `Move { dest, src }` inherits the same owner. The
/// record and tuple composite provers fold the whole closure into their binder
/// set (and, for records, its provenance) so a deep projection alias that
/// escapes through a return / hand-off temp (`ret = move leaf`) still names the
/// owner it aliases — the field-load scan's own move-propagation runs before
/// the alias is folded in, so it never reaches these copies.
pub(super) fn close_alias_binders_forward(
    blocks: &[BasicBlock],
    seeds: &[(u32, u32)],
) -> HashMap<u32, u32> {
    let mut owner_of: HashMap<u32, u32> = seeds.iter().copied().collect();
    loop {
        let mut changed = false;
        for block in blocks {
            for instr in &block.instructions {
                if let Instr::Move { dest, src } = instr {
                    if let (Some(sl), Some(dl)) = (base_local(*src), base_local(*dest)) {
                        if let Some(&owner) = owner_of.get(&sl) {
                            if let std::collections::hash_map::Entry::Vacant(slot) =
                                owner_of.entry(dl)
                            {
                                slot.insert(owner);
                                changed = true;
                            }
                        }
                    }
                }
            }
        }
        if !changed {
            break;
        }
    }
    owner_of
}
/// True when `Place::Local(local)` holds a heap-owning INLINE aggregate — a
/// tuple, fixed array, user record, or inline enum — whose field/element load
/// codegen BYTE-COPIES with no retain (the
/// [`ByteCopyAlias`](FieldLoadClass::ByteCopyAlias) load class). The free-
/// function mirror of [`Builder::classify_field_load`]'s `ByteCopyAlias` arm
/// for the drop-allow provers, which have no `Builder` in scope.
///
/// Deliberately under-approximating (fail-closed): `string` (codegen retains
/// the load — the dest owns a fresh `+1`), the single-pointer handle leaves
/// (`Vec` / `bytes` / `HashMap` / `Generator` / …, which TRANSFER their one
/// handle), indirect enums (a single owned node pointer), and opaque types are
/// all refused. Refusing keeps a dest invisible to the match-hop descent —
/// exactly today's posture for it — never a wrong alias attribution.
pub(super) fn local_is_byte_copy_aggregate(
    local: u32,
    local_tys: &[ResolvedTy],
    record_field_orders: &HashMap<String, Vec<(String, ResolvedTy)>>,
    enum_layouts: &[crate::model::EnumLayout],
) -> bool {
    let Some(ty) = local_tys.get(local as usize) else {
        return false;
    };
    if !crate::model::ty_owns_heap_mir(ty, record_field_orders, enum_layouts) {
        return false;
    }
    match ty {
        ResolvedTy::Tuple(_) | ResolvedTy::Array(_, _) => true,
        ResolvedTy::Named {
            builtin: None,
            is_opaque: false,
            ..
        } => !ty_is_indirect_enum(ty, enum_layouts),
        _ => false,
    }
}
/// #2384 — forward closure of the interior-alias binders through MATCH-BOUND
/// hops. [`close_alias_binders_forward`] follows only whole-value `Move`s, so
/// a chain hop bound via `match` (`let leaf = match mid { Mid { leaf, x: _ }
/// => leaf }`) is invisible to the composite-drop provers: the destructure
/// lowers to a `RecordFieldLoad` / `TupleFieldLoad` off the scrutinee COPY
/// (itself a `Move` of the alias), and the loaded inline aggregate byte-copies
/// the member with no retain — the binder is still an alias of the OWNER
/// root's storage, not a fresh owner. When such a binder then escaped
/// (returned), the owner's composite stayed admitted and re-freed the subtree
/// the escapee handed to the caller (the `free_cstring` sentinel abort).
///
/// This walk extends the `Move` closure with one gated descent rule: a
/// `RecordFieldLoad` / `TupleFieldLoad` whose base is a tracked local marks
/// the DEST as an alias of the same owner IFF the dest is a byte-copy inline
/// aggregate ([`local_is_byte_copy_aggregate`] — a retained `string` or a
/// transferred handle dest is a real owner and must NOT be attributed).
///
/// Consumers fold the result into their ESCAPE arms only — never into the
/// `release_owner_bases` Defect-1 blanket. A match-bound binder is an owned
/// binding with its own (prover-suppressed) release path, so blanket-scanning
/// it would newly exclude owners in today-clean non-escaping shapes (turning
/// an exactly-once composite into a whole-tree leak); the escape arms widen
/// exclusion only when the binder actually leaves the function.
fn descend_match_bound_hop_alias_facts(
    blocks: &[BasicBlock],
    seeds: &HashMap<u32, u32>,
    seed_chain: &[(u32, u32, u32)],
    dest_is_byte_copy_aggregate: &dyn Fn(u32) -> bool,
) -> MatchBoundHopAliasFacts {
    let mut owner_of: HashMap<u32, u32> = seeds.clone();
    let mut parent_of: HashMap<u32, (u32, u32)> = seed_chain
        .iter()
        .copied()
        .map(|(alias, parent, field)| (alias, (parent, field)))
        .collect();
    let mut chain = Vec::new();
    loop {
        let mut changed = false;
        for block in blocks {
            for instr in &block.instructions {
                let (base, dest, field) = match instr {
                    Instr::Move { dest, src } => (*src, *dest, None),
                    Instr::RecordFieldLoad {
                        record,
                        field_offset,
                        dest,
                    } => {
                        let Some(dl) = base_local(*dest) else {
                            continue;
                        };
                        if !dest_is_byte_copy_aggregate(dl) {
                            continue;
                        }
                        (*record, *dest, Some(field_offset.0))
                    }
                    Instr::TupleFieldLoad {
                        tuple,
                        field_index,
                        dest,
                    } => {
                        let Some(dl) = base_local(*dest) else {
                            continue;
                        };
                        if !dest_is_byte_copy_aggregate(dl) {
                            continue;
                        }
                        (*tuple, *dest, Some(*field_index))
                    }
                    _ => continue,
                };
                if let (Some(sl), Some(dl)) = (base_local(base), base_local(dest)) {
                    if let Some(&owner) = owner_of.get(&sl) {
                        let owner_matches = match owner_of.entry(dl) {
                            std::collections::hash_map::Entry::Vacant(slot) => {
                                slot.insert(owner);
                                changed = true;
                                true
                            }
                            std::collections::hash_map::Entry::Occupied(slot) => {
                                *slot.get() == owner
                            }
                        };
                        if !owner_matches {
                            continue;
                        }
                        let parent = match field {
                            Some(field) => Some((sl, field)),
                            None => parent_of.get(&sl).copied(),
                        };
                        if let Some((parent, field)) = parent {
                            if let std::collections::hash_map::Entry::Vacant(slot) =
                                parent_of.entry(dl)
                            {
                                slot.insert((parent, field));
                                chain.push((dl, parent, field));
                                changed = true;
                            }
                        }
                    }
                }
            }
        }
        if !changed {
            break;
        }
    }
    MatchBoundHopAliasFacts { owner_of, chain }
}
pub(super) fn descend_match_bound_hop_aliases(
    blocks: &[BasicBlock],
    seeds: &HashMap<u32, u32>,
    dest_is_byte_copy_aggregate: &dyn Fn(u32) -> bool,
) -> HashMap<u32, u32> {
    descend_match_bound_hop_alias_facts(blocks, seeds, &[], dest_is_byte_copy_aggregate).owner_of
}
pub(super) fn descend_match_bound_hop_alias_chain(
    blocks: &[BasicBlock],
    seeds: &HashMap<u32, u32>,
    seed_chain: &[(u32, u32, u32)],
    dest_is_byte_copy_aggregate: &dyn Fn(u32) -> bool,
) -> Vec<(u32, u32, u32)> {
    descend_match_bound_hop_alias_facts(blocks, seeds, seed_chain, dest_is_byte_copy_aggregate)
        .chain
}
pub(super) fn alias_projection_chain_owner_seeds(
    alias_chain: &[(u32, u32, u32)],
    candidate_roots: &HashSet<u32>,
) -> HashMap<u32, u32> {
    let parent_of: HashMap<u32, u32> = alias_chain
        .iter()
        .copied()
        .map(|(alias, parent, _)| (alias, parent))
        .collect();
    let mut seeds = HashMap::new();
    for &(alias, _, _) in alias_chain {
        let mut cursor = alias;
        for _ in 0..=parent_of.len() {
            let Some(&parent) = parent_of.get(&cursor) else {
                break;
            };
            if candidate_roots.contains(&parent) {
                seeds.insert(alias, parent);
                break;
            }
            cursor = parent;
        }
    }
    seeds
}
pub(super) fn collect_record_field_binders(
    blocks: &[BasicBlock],
    alias_of: &HashMap<u32, u32>,
    local_is_heap_owning: &dyn Fn(u32) -> bool,
) -> HashSet<u32> {
    let mut field_binders: HashSet<u32> = HashSet::new();
    for block in blocks {
        for instr in &block.instructions {
            if let Instr::RecordFieldLoad { record, dest, .. } = instr {
                if let Some(sl) = base_local(*record) {
                    if alias_of.contains_key(&sl) {
                        if let Some(dl) = base_local(*dest) {
                            if local_is_heap_owning(dl) {
                                field_binders.insert(dl);
                            }
                        }
                    }
                }
            }
        }
    }
    loop {
        let mut changed = false;
        for block in blocks {
            for instr in &block.instructions {
                if let Instr::Move { dest, src } = instr {
                    if let (Some(sl), Some(dl)) = (base_local(*src), base_local(*dest)) {
                        if field_binders.contains(&sl) && field_binders.insert(dl) {
                            changed = true;
                        }
                    }
                }
            }
        }
        if !changed {
            break;
        }
    }
    field_binders
}
/// Attribute each field binder to the record root (and field) it was loaded
/// from. Fail-closed lattice: vacant → `Unique` → `RootOnly` → `Ambiguous`,
/// monotone in every merge, so the move-propagation fixpoint converges.
///
/// A binder local REUSED via an instruction write that is not a member field
/// load or a binder-to-binder move is forced `Ambiguous` up front. Terminator
/// dests (a call result written into a reused binder local) are NOT tracked:
/// a stale attribution can only redirect WHICH root an escape excludes, and
/// the only root a binder local can alias intraprocedurally is the one its
/// member load named — excluding that root remains sound, and the
/// sibling-discharge emitter's siblings never alias the binder's content.
#[allow(
    clippy::too_many_lines,
    reason = "the three passes (reuse-write forcing, load seeding, move-\
              propagation fixpoint) share the merge lattice and the binder \
              set; splitting them scatters the monotonicity argument the \
              fixpoint's convergence rests on"
)]
pub(super) fn attribute_field_binder_provenance(
    blocks: &[BasicBlock],
    alias_of: &HashMap<u32, u32>,
    field_binders: &HashSet<u32>,
) -> HashMap<u32, FieldBinderProvenance> {
    fn merge(
        provenance: &mut HashMap<u32, FieldBinderProvenance>,
        binder: u32,
        incoming: FieldBinderProvenance,
    ) -> bool {
        use std::collections::hash_map::Entry;
        match provenance.entry(binder) {
            Entry::Vacant(slot) => {
                slot.insert(incoming);
                true
            }
            Entry::Occupied(mut slot) => {
                let current = *slot.get();
                let merged = match (current, incoming) {
                    (a, b) if a == b => a,
                    (FieldBinderProvenance::Ambiguous, _)
                    | (_, FieldBinderProvenance::Ambiguous) => FieldBinderProvenance::Ambiguous,
                    (
                        FieldBinderProvenance::Unique { root: r1, .. }
                        | FieldBinderProvenance::RootOnly { root: r1 },
                        FieldBinderProvenance::Unique { root: r2, .. }
                        | FieldBinderProvenance::RootOnly { root: r2 },
                    ) => {
                        if r1 == r2 {
                            FieldBinderProvenance::RootOnly { root: r1 }
                        } else {
                            FieldBinderProvenance::Ambiguous
                        }
                    }
                };
                if merged == current {
                    false
                } else {
                    slot.insert(merged);
                    true
                }
            }
        }
    }

    let mut provenance: HashMap<u32, FieldBinderProvenance> = HashMap::new();
    // Pass 0 — any instruction write into a binder that is NOT its defining
    // member field load or a binder-to-binder whole-value move forces
    // `Ambiguous` (absorbing; later merges cannot downgrade it).
    for block in blocks {
        for instr in &block.instructions {
            let defining_write = match instr {
                Instr::RecordFieldLoad { record, .. } => {
                    base_local(*record).is_some_and(|rl| alias_of.contains_key(&rl))
                }
                Instr::Move { src, .. } => {
                    matches!(src, Place::Local(_))
                        && base_local(*src).is_some_and(|sl| field_binders.contains(&sl))
                }
                _ => false,
            };
            if defining_write {
                continue;
            }
            let (_, writes) = crate::dataflow::instr_reads_writes(instr);
            for w in writes {
                if let Some(wl) = base_local(w) {
                    if field_binders.contains(&wl) {
                        provenance.insert(wl, FieldBinderProvenance::Ambiguous);
                    }
                }
            }
        }
    }
    // Pass 1 — seed from member field loads.
    for block in blocks {
        for instr in &block.instructions {
            if let Instr::RecordFieldLoad {
                record,
                field_offset,
                dest,
            } = instr
            {
                let Some(dl) = base_local(*dest) else {
                    continue;
                };
                if !field_binders.contains(&dl) {
                    continue;
                }
                let incoming = match base_local(*record).and_then(|rl| alias_of.get(&rl)) {
                    Some(&root) => FieldBinderProvenance::Unique {
                        root,
                        field: field_offset.0,
                    },
                    // Loaded from a non-member record: not attributable.
                    None => FieldBinderProvenance::Ambiguous,
                };
                merge(&mut provenance, dl, incoming);
            }
        }
    }
    // Pass 2 — fixpoint over binder-to-binder whole-value moves.
    loop {
        let mut changed = false;
        for block in blocks {
            for instr in &block.instructions {
                if let Instr::Move { dest, src } = instr {
                    if !matches!(src, Place::Local(_)) || !matches!(dest, Place::Local(_)) {
                        continue;
                    }
                    if let (Some(sl), Some(dl)) = (base_local(*src), base_local(*dest)) {
                        if field_binders.contains(&sl) && field_binders.contains(&dl) {
                            if let Some(p) = provenance.get(&sl).copied() {
                                changed |= merge(&mut provenance, dl, p);
                            }
                        }
                    }
                }
            }
        }
        if !changed {
            break;
        }
    }
    provenance
}
/// True if reading this `Place` as a `Move` source yields a value that
/// *aliases interior storage* of a still-live parent aggregate rather than
/// a standalone slot the move can hand off ownership of.
///
/// W5-011 P3 (projection-alias taint). The M-COW spine emits NO retain on
/// share, so a payload binder bound by destructuring an enum/machine
/// variant (`Instr::Move { dest, src: Place::MachineVariant { .. } }` /
/// `EnumVariant` — see `lower_match_enum_tag`, `lower_while_let`) receives
/// a `string` handle that aliases the parent's rc=1 buffer. Dropping that
/// binder at scope exit would free a buffer the parent still owns (or that
/// a second destructure of the same payload also frees) → double-free.
/// `projection_alias_dest` seeds taint at the four `*FieldLoad` interior
/// loads, but variant-payload destructures lower as a `Move` *from* an
/// interior-projection place, which that scan does not see. This
/// classifier closes that hole: a `Move` whose source is an interior
/// projection taints its dest in `derive_cow_sole_owner`.
///
/// Exhaustive with no wildcard (fail-closed): only `Local` and the
/// synthetic `ReturnSlot` are plain, non-aliasing slots whose `Move` is an
/// ownership hand-off (forward-propagated by the fixpoint, not seeded).
/// Every other variant — the interior-projection loads
/// (`MachineVariant`/`EnumVariant`) *and* the handle/tag places
/// — is treated as interior so a future projection-shaped place cannot be
/// added without deciding it here; over-tainting a handle/tag place only
/// over-excludes its dest from drop (leaks, never double-frees).
#[must_use]
#[allow(
    clippy::match_same_arms,
    reason = "the interior-projection arm and the handle/tag arm both return \
              `true` but are kept separate: they are kept distinct so a future \
              `Place` variant must be classified deliberately on the correct \
              side (interior vs hand-off), and so the per-group rationale stays \
              attached to the variants it covers — folding them would erase the \
              fail-closed intent"
)]
pub(super) fn place_is_interior_projection(place: Place) -> bool {
    match place {
        // Plain ownership-bearing slots: a `Move` from here is a hand-off,
        // not an interior alias. Forward-propagation in the fixpoint loop
        // (not this seed) carries taint through `Local` → `Local` copies.
        Place::Local(_) | Place::ReturnSlot => false,
        // Interior-projection loads — the payload-destructure forms that
        // alias parent storage with no retain. These are the variants this
        // fix exists to catch.
        Place::MachineVariant { .. } | Place::EnumVariant { .. } => true,
        // Handle/tag places. Tainting these only over-excludes (leaks); kept
        // on the interior side so the match stays fail-closed and no future
        // projection-shaped place defaults to the hand-off branch.
        Place::DuplexHandle(_)
        | Place::LambdaActorHandle(_)
        | Place::ActorHandle(_)
        | Place::SendHalf(_)
        | Place::RecvHalf(_)
        | Place::MachineTag(_)
        | Place::EnumTag(_) => true,
    }
}
/// The [`BindingId`] a `root.field` / `root.N` projection's object resolves to
/// when it is a direct reference to a live binding. `None` for a nested or
/// non-binding object (a projection chain, a call result, `this`) — the caller
/// then fails closed and records no alias provenance.
pub(super) fn binding_ref_target(object: &HirExpr) -> Option<BindingId> {
    match &object.kind {
        HirExprKind::BindingRef {
            resolved: ResolvedRef::Binding(id),
            ..
        } => Some(*id),
        _ => None,
    }
}
/// True when `place` projects only the integer discriminant tag of an
/// enum/machine value (`Place::EnumTag` / `Place::MachineTag`).
///
/// A tag read copies a bitcopy ordinal out of the tagged-union header; it
/// transfers NO heap ownership. The nested-constructor predicate lowering
/// (`emit_payload_variant_predicate_checks`) reads the tag of an inner
/// payload binder this way to dispatch on it — that read must NOT be
/// classified as a payload escape in `derive_enum_composite_drop_allowed`,
/// otherwise the parent composite is wrongly excluded from its tag-aware
/// `DropKind::EnumInPlace` scope-exit drop and the inner payload leaks
/// (the W5.020 nested-payload leak). It is the exact symmetric analogue of
/// the parent composite's own benign tag read, which the whole-composite
/// escape branch already exempts by only flagging whole `Place::Local`
/// reads. Any read that could move the heap payload out (an interior
/// `MachineVariant` / `EnumVariant` projection, a whole-value `Local`
/// hand-off) is deliberately NOT covered here, so the heap-payload escape
/// scan stays fail-closed.
pub(super) fn place_is_tag_read(place: Place) -> bool {
    matches!(place, Place::EnumTag(_) | Place::MachineTag(_))
}
// ============================================================================
// Slice 3 (M2 substrate) drop-plan invariant tests.
//
// Pin the per-Return live-set narrowing semantics + the Place->DropKind
// invariants + the weak-ref capture discipline. Built directly against
// the internal helpers (`drop_kind_for`, `validate_drop_plan`) using
// synthetic `ElaboratedMirFunction` inputs so the HIR-construction gap
// (no LambdaActor/Duplex HIR shape yet) doesn't block coverage.
// ============================================================================

#[cfg(test)]
mod slice3_invariants {
    use super::*;
    use crate::model::{CaptureKind, Direction};

    #[cfg(not(target_arch = "wasm32"))]
    use hew_runtime::{
        HEW_CTX_OFFSET_ACTOR_ID as RUNTIME_CTX_OFFSET_ACTOR_ID,
        HEW_CTX_OFFSET_PARENT_SUPERVISOR as RUNTIME_CTX_OFFSET_PARENT_SUPERVISOR,
        HEW_CTX_OFFSET_TRACE as RUNTIME_CTX_OFFSET_TRACE,
        HEW_CTX_OFFSET_TRACE_SPAN as RUNTIME_CTX_OFFSET_TRACE_SPAN,
    };

    fn reader_ty(reader: ExecutionContextReader) -> ResolvedTy {
        ResolvedTy::from_ty(&reader.ty()).expect("context reader type resolves")
    }

    fn function_evaluating_context_reader(reader: ExecutionContextReader) -> HirFn {
        let ty = reader_ty(reader);
        HirFn {
            id: hew_hir::ItemId(0),
            node: hew_hir::HirNodeId(0),
            name: "handler".to_string(),
            type_params: vec![],
            is_generator: false,
            intrinsic_id: None,
            params: vec![],
            return_ty: ResolvedTy::Unit,
            body: HirBlock {
                node: hew_hir::HirNodeId(1),
                scope: hew_hir::ScopeId(0),
                statements: vec![HirStmt {
                    node: hew_hir::HirNodeId(2),
                    kind: HirStmtKind::Expr(HirExpr {
                        node: hew_hir::HirNodeId(3),
                        site: hew_hir::SiteId(0),
                        ty: ty.clone(),
                        value_class: ValueClass::of_ty(&ty, &hew_hir::TypeClassTable::default()),
                        intent: IntentKind::Read,
                        kind: HirExprKind::ContextReader { reader },
                        span: 0..0,
                    }),
                    span: 0..0,
                }],
                tail: None,
                ty: ResolvedTy::Unit,
                span: 0..0,
            },
            span: 0..0,
        }
    }

    #[test]
    fn context_readers_lower_to_context_field_offsets() {
        for (reader, expected_offset) in [
            (ExecutionContextReader::ActorId, HEW_CTX_OFFSET_ACTOR_ID),
            (
                ExecutionContextReader::Supervisor,
                HEW_CTX_OFFSET_PARENT_SUPERVISOR,
            ),
            (ExecutionContextReader::TraceSpan, HEW_CTX_OFFSET_TRACE_SPAN),
        ] {
            let func = function_evaluating_context_reader(reader);
            let lowered = lower_function(
                &func,
                "handler".to_string(),
                HashMap::new(),
                &hew_hir::TypeClassTable::default(),
                &HashMap::new(),
                &HashMap::new(),
                &HashMap::new(),
                &HashSet::new(),
                &[],
                &[],
                None,
                &HashSet::new(),
                &HashSet::new(),
                &std::rc::Rc::new(std::collections::HashMap::new()),
                &std::rc::Rc::new(crate::return_provenance::CallScrutineeProvenance::default()),
                &std::rc::Rc::new(ParamOwnershipFacts::default()),
                &HashMap::new(),
                &HashMap::new(),
                None,
                &HashMap::new(),
                &HashMap::new(),
                PointerWidth::Bits64,
                crate::model::FunctionCallConv::ActorHandler,
                TaskEntryAdapterSymbols::default(),
            );
            let offsets: Vec<_> = lowered
                .raw
                .blocks
                .iter()
                .flat_map(|block| &block.instructions)
                .filter_map(|instr| {
                    if let Instr::ContextField { offset, .. } = instr {
                        Some(*offset)
                    } else {
                        None
                    }
                })
                .collect();
            assert_eq!(offsets, vec![expected_offset], "{reader:?}");
            assert!(
                lowered.diagnostics.is_empty(),
                "{reader:?} diagnostics: {:?}",
                lowered.diagnostics
            );
        }

        #[cfg(not(target_arch = "wasm32"))]
        {
            assert_eq!(
                HEW_CTX_OFFSET_ACTOR_ID, RUNTIME_CTX_OFFSET_ACTOR_ID,
                "ActorId MIR offset must match the runtime authority"
            );
            assert_eq!(
                HEW_CTX_OFFSET_PARENT_SUPERVISOR, RUNTIME_CTX_OFFSET_PARENT_SUPERVISOR,
                "Supervisor MIR offset must match the runtime authority"
            );
            assert_eq!(
                HEW_CTX_OFFSET_TRACE, RUNTIME_CTX_OFFSET_TRACE,
                "TraceSpan base MIR offset must match the runtime authority"
            );

            let runtime_span_suboffset = RUNTIME_CTX_OFFSET_TRACE_SPAN
                .checked_sub(RUNTIME_CTX_OFFSET_TRACE)
                .expect("runtime trace span offset must be at or after its trace base");
            assert_eq!(
                HEW_TRACE_OFFSET_SPAN_ID, runtime_span_suboffset,
                "TraceSpan span-id suboffset must match the runtime authority"
            );
            assert_eq!(
                HEW_CTX_OFFSET_TRACE_SPAN, RUNTIME_CTX_OFFSET_TRACE_SPAN,
                "TraceSpan absolute MIR offset must match the runtime authority"
            );
        }
    }

    /// A `Duplex<i64, i64>` `ResolvedTy` used as a stand-in payload
    /// for synthetic `ElabDrop` entries. The body of these tests
    /// cares about `Place` + `DropKind`, not the inner type detail.
    fn duplex_int_int_ty() -> ResolvedTy {
        ResolvedTy::named_user("Duplex", vec![ResolvedTy::I64, ResolvedTy::I64])
    }

    fn make_elab(
        drop_plans: Vec<(ExitPath, DropPlan)>,
        lambda_captures: Vec<LambdaCapture>,
    ) -> ElaboratedMirFunction {
        ElaboratedMirFunction {
            name: "synthetic".to_string(),
            return_ty: ResolvedTy::Unit,
            statements: vec![],
            decisions: vec![],
            blocks: vec![],
            drop_plans,
            coroutine: None,
            lambda_captures,
        }
    }

    fn make_elab_with_drops(drops: Vec<ElabDrop>) -> ElaboratedMirFunction {
        make_elab(
            vec![(ExitPath::Return { block: 0 }, DropPlan { drops })],
            vec![],
        )
    }

    // ---------- drop_kind_for: Place -> DropKind mapping ----------

    #[test]
    fn drop_kind_for_duplex_handle_selects_duplex_close() {
        let ty = duplex_int_int_ty();
        assert_eq!(
            drop_kind_for(Place::DuplexHandle(0), &ty, None),
            DropKind::DuplexClose
        );
    }

    #[test]
    fn drop_kind_for_lambda_actor_handle_selects_lambda_actor_release() {
        let ty = duplex_int_int_ty();
        assert_eq!(
            drop_kind_for(Place::LambdaActorHandle(0), &ty, None),
            DropKind::LambdaActorRelease
        );
    }

    #[test]
    fn drop_kind_for_send_half_selects_duplex_half_close_send() {
        let ty = duplex_int_int_ty();
        assert_eq!(
            drop_kind_for(Place::SendHalf(0), &ty, None),
            DropKind::DuplexHalfClose(Direction::Send)
        );
    }

    #[test]
    fn drop_kind_for_recv_half_selects_duplex_half_close_recv() {
        let ty = duplex_int_int_ty();
        assert_eq!(
            drop_kind_for(Place::RecvHalf(0), &ty, None),
            DropKind::DuplexHalfClose(Direction::Recv)
        );
    }

    #[test]
    fn drop_kind_for_local_selects_resource() {
        // Pre-M2 path: generic Resource for Local Places. Pinning this
        // is the regression guard against accidentally routing Local
        // drops through a Duplex-specific protocol.
        let ty = duplex_int_int_ty();
        assert_eq!(
            drop_kind_for(Place::Local(0), &ty, None),
            DropKind::Resource
        );
    }

    // ---------- validate_drop_plan: structural invariants ----------

    #[test]
    fn validate_drop_plan_accepts_consistent_duplex_close() {
        // A DuplexHandle paired with DuplexClose — the canonical M2
        // substrate shape. No findings.
        let drops = vec![ElabDrop {
            place: Place::DuplexHandle(0),
            ty: duplex_int_int_ty(),
            drop_fn: None,
            kind: DropKind::DuplexClose,
            guard: None,
        }];
        let elab = make_elab_with_drops(drops);
        assert!(
            validate_drop_plan(&elab).is_empty(),
            "consistent (DuplexHandle, DuplexClose) must not flag"
        );
    }

    #[test]
    fn validate_drop_plan_rejects_duplex_handle_with_resource_kind() {
        // A DuplexHandle paired with DropKind::Resource would silently
        // route through generic Type::close dispatch and miss the
        // close-both-directions protocol. Must surface as
        // DropPlanUndetermined.
        let drops = vec![ElabDrop {
            place: Place::DuplexHandle(7),
            ty: duplex_int_int_ty(),
            drop_fn: Some(crate::model::DropFnSpec::Runtime(
                hew_types::runtime_call::RuntimeDropDescriptor::DuplexClose,
            )),
            kind: DropKind::Resource,
            guard: None,
        }];
        let elab = make_elab_with_drops(drops);
        let findings = validate_drop_plan(&elab);
        assert_eq!(findings.len(), 1, "exactly one finding expected");
        let MirCheck::DropPlanUndetermined { block, reason } = &findings[0] else {
            panic!("expected DropPlanUndetermined, got {:?}", findings[0]);
        };
        assert_eq!(*block, 0);
        assert!(
            reason.contains("DuplexHandle") && reason.contains("Resource"),
            "diagnostic must name both the Place and the wrong kind: {reason}"
        );
    }

    #[test]
    fn validate_drop_plan_rejects_lambda_actor_handle_with_duplex_close_kind() {
        // LambdaActorHandle MUST select LambdaActorRelease (the
        // stop-protocol with weak-ref body capture). DuplexClose would
        // skip the actor's stop protocol — silently leaking the actor.
        let drops = vec![ElabDrop {
            place: Place::LambdaActorHandle(3),
            ty: duplex_int_int_ty(),
            drop_fn: None,
            kind: DropKind::DuplexClose,
            guard: None,
        }];
        let elab = make_elab_with_drops(drops);
        let findings = validate_drop_plan(&elab);
        assert_eq!(findings.len(), 1);
        assert!(matches!(findings[0], MirCheck::DropPlanUndetermined { .. }));
    }

    #[test]
    fn validate_drop_plan_rejects_send_half_with_close_both_kind() {
        // SendHalf MUST close one direction only; pairing with
        // DuplexClose (close-both) would over-close the recv side.
        let drops = vec![ElabDrop {
            place: Place::SendHalf(1),
            ty: duplex_int_int_ty(),
            drop_fn: None,
            kind: DropKind::DuplexClose,
            guard: None,
        }];
        let elab = make_elab_with_drops(drops);
        assert_eq!(validate_drop_plan(&elab).len(), 1);
    }

    #[test]
    fn validate_drop_plan_rejects_recv_half_with_send_direction() {
        // RecvHalf MUST close Direction::Recv; pairing with
        // Direction::Send would close the wrong queue.
        let drops = vec![ElabDrop {
            place: Place::RecvHalf(2),
            ty: duplex_int_int_ty(),
            drop_fn: None,
            kind: DropKind::DuplexHalfClose(Direction::Send),
            guard: None,
        }];
        let elab = make_elab_with_drops(drops);
        assert_eq!(validate_drop_plan(&elab).len(), 1);
    }

    #[test]
    fn validate_drop_plan_accepts_half_handle_pair_closing_both_dirs() {
        // A SendHalf + RecvHalf pair, each closing its own direction —
        // the canonical "duplex split via .send_half() / .recv_half()"
        // shape. Together they close both directions; individually
        // each is a one-direction drop.
        let drops = vec![
            ElabDrop {
                place: Place::SendHalf(0),
                ty: duplex_int_int_ty(),
                drop_fn: None,
                kind: DropKind::DuplexHalfClose(Direction::Send),
                guard: None,
            },
            ElabDrop {
                place: Place::RecvHalf(0),
                ty: duplex_int_int_ty(),
                drop_fn: None,
                kind: DropKind::DuplexHalfClose(Direction::Recv),
                guard: None,
            },
        ];
        let elab = make_elab_with_drops(drops);
        assert!(validate_drop_plan(&elab).is_empty());
    }

    // ---------- consume-on-split invariant for Duplex<S, R> ----------

    /// Build an `ElabDrop` addressing a Duplex-family `Place` with its
    /// canonical `DropKind`. Used by every split-state shape below.
    fn duplex_drop(place: Place) -> ElabDrop {
        let ty = duplex_int_int_ty();
        let kind = drop_kind_for(place, &ty, None);
        ElabDrop {
            place,
            ty,
            drop_fn: None,
            kind,
            guard: None,
        }
    }

    #[test]
    fn split_state_whole_only_is_accepted() {
        // Pre-split: only DuplexHandle(N) is in the drop plan. Canonical
        // "no .send_half() was ever called" shape.
        let elab = make_elab_with_drops(vec![duplex_drop(Place::DuplexHandle(0))]);
        assert!(validate_drop_plan(&elab).is_empty());
    }

    #[test]
    fn split_state_both_halves_only_is_accepted() {
        // Post-full-split: DuplexHandle is gone (consumed by both
        // split methods), only SendHalf(N) + RecvHalf(N) remain.
        let elab = make_elab_with_drops(vec![
            duplex_drop(Place::SendHalf(0)),
            duplex_drop(Place::RecvHalf(0)),
        ]);
        assert!(validate_drop_plan(&elab).is_empty());
    }

    #[test]
    fn split_state_send_half_only_is_accepted() {
        // .send_half() called, RecvHalf retained on the original (rare
        // but legal: caller kept the unified handle's recv side via a
        // .recv_half() that wasn't yet called). The plan-level shape
        // is one half — codegen closes that direction; the other side
        // stays open under runtime refcount until its handle drops.
        let elab = make_elab_with_drops(vec![duplex_drop(Place::SendHalf(0))]);
        assert!(validate_drop_plan(&elab).is_empty());
    }

    #[test]
    fn split_state_recv_half_only_is_accepted() {
        let elab = make_elab_with_drops(vec![duplex_drop(Place::RecvHalf(0))]);
        assert!(validate_drop_plan(&elab).is_empty());
    }

    #[test]
    fn split_state_whole_plus_send_half_is_rejected() {
        // The stale-unified-handle bug: split would have moved the
        // DuplexHandle out, so it must NOT coexist with a half.
        // Without this rejection, codegen emits close-both on the
        // stale handle AND close-send on the half — the S-direction
        // closes twice; the R-direction closes once on a half-handle
        // binding that doesn't own it.
        let elab = make_elab_with_drops(vec![
            duplex_drop(Place::DuplexHandle(0)),
            duplex_drop(Place::SendHalf(0)),
        ]);
        let findings = validate_drop_plan(&elab);
        assert_eq!(findings.len(), 1, "expected exactly one finding");
        let MirCheck::DropPlanUndetermined { reason, .. } = &findings[0] else {
            panic!("expected DropPlanUndetermined, got {:?}", findings[0]);
        };
        assert!(
            reason.contains("DuplexHandle") && reason.contains("half-handle"),
            "diagnostic must name the stale-handle conflict: {reason}"
        );
    }

    #[test]
    fn split_state_whole_plus_recv_half_is_rejected() {
        let elab = make_elab_with_drops(vec![
            duplex_drop(Place::DuplexHandle(0)),
            duplex_drop(Place::RecvHalf(0)),
        ]);
        assert_eq!(validate_drop_plan(&elab).len(), 1);
    }

    #[test]
    fn split_state_whole_plus_both_halves_is_rejected() {
        let elab = make_elab_with_drops(vec![
            duplex_drop(Place::DuplexHandle(0)),
            duplex_drop(Place::SendHalf(0)),
            duplex_drop(Place::RecvHalf(0)),
        ]);
        assert_eq!(validate_drop_plan(&elab).len(), 1);
    }

    #[test]
    fn split_state_duplicate_send_half_is_rejected() {
        // Two SendHalf(N) entries would close the S-direction twice
        // — a lowering bug (the split method returns a single half).
        let elab = make_elab_with_drops(vec![
            duplex_drop(Place::SendHalf(0)),
            duplex_drop(Place::SendHalf(0)),
        ]);
        let findings = validate_drop_plan(&elab);
        assert_eq!(findings.len(), 1);
        let MirCheck::DropPlanUndetermined { reason, .. } = &findings[0] else {
            unreachable!();
        };
        assert!(reason.contains("SendHalf") && reason.contains("S-direction"));
    }

    #[test]
    fn split_state_duplicate_recv_half_is_rejected() {
        let elab = make_elab_with_drops(vec![
            duplex_drop(Place::RecvHalf(0)),
            duplex_drop(Place::RecvHalf(0)),
        ]);
        assert_eq!(validate_drop_plan(&elab).len(), 1);
    }

    #[test]
    fn split_state_duplicate_whole_is_rejected() {
        let elab = make_elab_with_drops(vec![
            duplex_drop(Place::DuplexHandle(0)),
            duplex_drop(Place::DuplexHandle(0)),
        ]);
        assert_eq!(validate_drop_plan(&elab).len(), 1);
    }

    #[test]
    fn split_state_independent_parents_do_not_interfere() {
        // Two distinct Duplex parents (N=0 and N=1). Parent 0 is in
        // the whole state; parent 1 is in the both-halves state.
        // Neither should flag — the invariant is per-parent.
        let elab = make_elab_with_drops(vec![
            duplex_drop(Place::DuplexHandle(0)),
            duplex_drop(Place::SendHalf(1)),
            duplex_drop(Place::RecvHalf(1)),
        ]);
        assert!(validate_drop_plan(&elab).is_empty());
    }

    #[test]
    fn split_state_each_direction_closes_exactly_once_property() {
        // Property: for every legal split-state shape on a single
        // Duplex, the emitted drop plan closes the S-direction at
        // most once AND the R-direction at most once. The four
        // legal shapes (Whole, SendOnly, RecvOnly, BothHalves) all
        // satisfy this; the rejected shapes (Whole+Send, Whole+Recv,
        // Whole+Both, dup-Send, dup-Recv, dup-Whole) violate it.
        //
        // Encoded as exhaustive enumeration matching the project's
        // existing exhaustive-small-state test style (see
        // dataflow.rs meet-lattice tests).
        let parent = 0u32;
        let candidates: Vec<(&str, Vec<Place>)> = vec![
            ("Whole", vec![Place::DuplexHandle(parent)]),
            ("SendOnly", vec![Place::SendHalf(parent)]),
            ("RecvOnly", vec![Place::RecvHalf(parent)]),
            (
                "BothHalves",
                vec![Place::SendHalf(parent), Place::RecvHalf(parent)],
            ),
        ];
        for (label, places) in candidates {
            // Count how many times each direction closes under the
            // canonical Place->DropKind mapping.
            let mut s_count = 0u32;
            let mut r_count = 0u32;
            for p in &places {
                match drop_kind_for(*p, &duplex_int_int_ty(), None) {
                    DropKind::DuplexClose => {
                        s_count += 1;
                        r_count += 1;
                    }
                    DropKind::DuplexHalfClose(Direction::Send) => s_count += 1,
                    DropKind::DuplexHalfClose(Direction::Recv) => r_count += 1,
                    DropKind::Resource
                    | DropKind::RcRelease
                    | DropKind::WeakRelease
                    | DropKind::LambdaActorRelease
                    | DropKind::CowHeap { .. }
                    | DropKind::RecordInPlace
                    | DropKind::AggregateRecursive
                    | DropKind::EnumInPlace
                    | DropKind::TupleInPlace
                    | DropKind::ClosurePair
                    | DropKind::IndirectEnum
                    | DropKind::TraitObject { .. } => {}
                }
            }
            assert!(
                s_count <= 1 && r_count <= 1,
                "{label}: each direction must close at most once (got S={s_count}, R={r_count})"
            );
            let drops: Vec<ElabDrop> = places.into_iter().map(duplex_drop).collect();
            let elab = make_elab_with_drops(drops);
            assert!(
                validate_drop_plan(&elab).is_empty(),
                "{label}: legal split state must validate"
            );
        }
    }

    // ---------- broadened validation: every ExitPath + ElabBlock.drops ----------

    /// Build a synthetic `ElaboratedMirFunction` whose sole `DropPlan`
    /// is attached to `exit`. Used to exercise non-`Return` `ExitPath`
    /// validation under the broadened walk.
    fn make_elab_with_exit_and_drops(
        exit: ExitPath,
        drops: Vec<ElabDrop>,
    ) -> ElaboratedMirFunction {
        make_elab(vec![(exit, DropPlan { drops })], vec![])
    }

    #[test]
    fn validate_walks_panic_exit_path() {
        // A Panic exit's DropPlan is the same LIFO sequence as the
        // Return exit at the same scope. A malformed kind here would
        // be silently accepted under the Return-only walk; the
        // broadened walk must reject it.
        let bad = ElabDrop {
            place: Place::DuplexHandle(0),
            ty: duplex_int_int_ty(),
            drop_fn: None,
            kind: DropKind::Resource, // wrong: DuplexHandle wants DuplexClose
            guard: None,
        };
        let elab = make_elab_with_exit_and_drops(ExitPath::Panic { block: 5 }, vec![bad]);
        let findings = validate_drop_plan(&elab);
        assert_eq!(findings.len(), 1, "Panic-exit plan must be validated");
        let MirCheck::DropPlanUndetermined { block, reason } = &findings[0] else {
            unreachable!();
        };
        assert_eq!(*block, 5);
        assert!(
            reason.contains("Panic"),
            "diagnostic should name the exit kind: {reason}"
        );
    }

    #[test]
    fn validate_walks_cancel_exit_path() {
        // Cancel is the scope-structural cancellation exit. Same
        // shape as Panic for drop-plan purposes.
        let bad = ElabDrop {
            place: Place::SendHalf(0),
            ty: duplex_int_int_ty(),
            drop_fn: None,
            kind: DropKind::DuplexClose, // wrong: SendHalf wants DuplexHalfClose(Send)
            guard: None,
        };
        let elab = make_elab_with_exit_and_drops(ExitPath::Cancel { block: 9 }, vec![bad]);
        let findings = validate_drop_plan(&elab);
        assert_eq!(findings.len(), 1);
        let MirCheck::DropPlanUndetermined { block, reason } = &findings[0] else {
            unreachable!();
        };
        assert_eq!(*block, 9);
        assert!(reason.contains("Cancel"));
    }

    #[test]
    fn validate_walks_yield_send_select_exit_paths() {
        // The three forward-compat exit kinds. Their DropPlans are
        // empty on the spine, but the validator must still apply the
        // Place-driven kind check when a future surface populates
        // them. Use a malformed DuplexHandle drop on each.
        let bad = ElabDrop {
            place: Place::DuplexHandle(0),
            ty: duplex_int_int_ty(),
            drop_fn: None,
            kind: DropKind::Resource,
            guard: None,
        };
        for exit in [
            ExitPath::Yield { block: 1, next: 99 },
            ExitPath::Send {
                block: 2,
                actor: String::new(),
                next: 99,
            },
            ExitPath::Select { block: 3, next: 99 },
        ] {
            let expected_label = exit_kind_label(&exit);
            let elab = make_elab_with_exit_and_drops(exit, vec![bad.clone()]);
            let findings = validate_drop_plan(&elab);
            assert_eq!(
                findings.len(),
                1,
                "exit {expected_label} must be walked by validator"
            );
        }
    }

    #[test]
    fn validate_walks_goto_branch_call_exit_paths() {
        // Intra-CFG edges. Empty DropPlans on the spine, but the
        // validator walks them uniformly so a future construction
        // surface can't slip a malformed drop past.
        let bad = ElabDrop {
            place: Place::LambdaActorHandle(0),
            ty: duplex_int_int_ty(),
            drop_fn: None,
            kind: DropKind::DuplexClose, // wrong
            guard: None,
        };
        for exit in [
            ExitPath::Goto {
                block: 1,
                target: 99,
            },
            ExitPath::Branch {
                block: 2,
                then_target: 98,
                else_target: 99,
            },
            ExitPath::Call {
                block: 3,
                callee: "f".to_string(),
                next: 99,
            },
        ] {
            let elab = make_elab_with_exit_and_drops(exit, vec![bad.clone()]);
            let findings = validate_drop_plan(&elab);
            assert_eq!(findings.len(), 1);
        }
    }

    #[test]
    fn validate_walks_split_state_invariant_on_panic_exit() {
        // Consume-on-split applies at every exit path, not just
        // Return. A Panic cleanup with both DuplexHandle + SendHalf
        // for the same parent must be rejected.
        let elab = make_elab_with_exit_and_drops(
            ExitPath::Panic { block: 4 },
            vec![
                duplex_drop(Place::DuplexHandle(0)),
                duplex_drop(Place::SendHalf(0)),
            ],
        );
        let findings = validate_drop_plan(&elab);
        assert_eq!(findings.len(), 1, "split-state must apply to Panic exit");
        assert!(matches!(findings[0], MirCheck::DropPlanUndetermined { .. }));
    }

    #[test]
    fn validate_walks_elab_block_drops() {
        // Cleanup blocks carry drops directly in ElabBlock.drops.
        // A malformed kind here must be rejected at the same
        // structural boundary.
        let bad = ElabDrop {
            place: Place::RecvHalf(0),
            ty: duplex_int_int_ty(),
            drop_fn: None,
            kind: DropKind::DuplexHalfClose(Direction::Send), // wrong queue
            guard: None,
        };
        let elab = ElaboratedMirFunction {
            name: "synthetic".to_string(),
            return_ty: ResolvedTy::Unit,
            statements: vec![],
            decisions: vec![],
            blocks: vec![ElabBlock {
                id: 12,
                kind: BlockKind::Cleanup,
                drops: vec![bad],
                successor: None,
            }],
            drop_plans: vec![],
            coroutine: None,
            lambda_captures: vec![],
        };
        let findings = validate_drop_plan(&elab);
        assert_eq!(findings.len(), 1);
        let MirCheck::DropPlanUndetermined { block, reason } = &findings[0] else {
            unreachable!();
        };
        assert_eq!(*block, 12);
        assert!(
            reason.contains("cleanup drop"),
            "diagnostic must name the cleanup-block context: {reason}"
        );
    }

    #[test]
    fn validate_walks_elab_block_drops_split_state() {
        // Consume-on-split applies inside cleanup blocks too. A
        // cleanup block whose drops list has DuplexHandle + RecvHalf
        // for the same parent must be rejected.
        let elab = ElaboratedMirFunction {
            name: "synthetic".to_string(),
            return_ty: ResolvedTy::Unit,
            statements: vec![],
            decisions: vec![],
            blocks: vec![ElabBlock {
                id: 13,
                kind: BlockKind::Cleanup,
                drops: vec![
                    duplex_drop(Place::DuplexHandle(0)),
                    duplex_drop(Place::RecvHalf(0)),
                ],
                successor: None,
            }],
            drop_plans: vec![],
            coroutine: None,
            lambda_captures: vec![],
        };
        let findings = validate_drop_plan(&elab);
        assert_eq!(findings.len(), 1);
    }

    // ---------- per-Return live-set narrowing (synthetic) ----------

    #[test]
    fn per_return_live_set_drops_exactly_match_kept_handles() {
        // The plan's invariant: at each Return block, the set of
        // Duplex/lambda-actor places dropped is exactly
        // (places-defined-in-this-fn) - (places-moved-out). Synthetic
        // shape: two Duplex handles defined, one moved out, one
        // dropped. With consistent kinds, validate_drop_plan accepts;
        // the drop list is exactly the one not-moved place.
        let kept = ElabDrop {
            place: Place::DuplexHandle(0),
            ty: duplex_int_int_ty(),
            drop_fn: None,
            kind: DropKind::DuplexClose,
            guard: None,
        };
        let elab = make_elab_with_drops(vec![kept.clone()]);
        let return_plan = elab
            .drop_plans
            .iter()
            .find(|(e, _)| matches!(e, ExitPath::Return { .. }))
            .unwrap();
        assert_eq!(return_plan.1.drops, vec![kept]);
        assert!(validate_drop_plan(&elab).is_empty());
    }

    #[test]
    fn per_return_no_spurious_drops_when_all_moved_out() {
        // If every defined Place was moved out before the Return, the
        // drop list is empty. (places-defined - places-moved-out = ∅.)
        // This is the dual of the previous test.
        let elab = make_elab_with_drops(vec![]);
        let return_plan = &elab.drop_plans[0];
        assert!(return_plan.1.drops.is_empty());
        assert!(validate_drop_plan(&elab).is_empty());
    }

    #[test]
    fn per_return_no_missed_drops_for_three_live_handles() {
        // Three live Duplex handles at the Return — all three must
        // appear in the drop list. Order doesn't matter for this
        // invariant; codegen consumes the list in LIFO source order
        // (a separate concern). Verify count + presence.
        let drops: Vec<ElabDrop> = (0..3u32)
            .map(|i| ElabDrop {
                place: Place::DuplexHandle(i),
                ty: duplex_int_int_ty(),
                drop_fn: None,
                kind: DropKind::DuplexClose,
                guard: None,
            })
            .collect();
        let elab = make_elab_with_drops(drops.clone());
        let return_plan = &elab.drop_plans[0];
        assert_eq!(return_plan.1.drops.len(), 3);
        for d in &drops {
            assert!(
                return_plan.1.drops.contains(d),
                "missing drop for {:?}",
                d.place
            );
        }
        assert!(validate_drop_plan(&elab).is_empty());
    }

    // ---------- weak-ref capture invariants ----------

    fn make_elab_with_captures(captures: Vec<LambdaCapture>) -> ElaboratedMirFunction {
        make_elab(vec![], captures)
    }

    #[test]
    fn weak_capture_on_lambda_actor_handle_is_accepted() {
        // The canonical §5.9 ratification 2 shape:
        //   let fib = actor |n| { ... fib(n-1) ... };
        // The body's `fib` reference is captured as Weak attached to
        // the lambda-actor's own LambdaActorHandle. Validation must
        // accept this.
        let captures = vec![LambdaCapture {
            actor_handle: Place::LambdaActorHandle(0),
            captured: BindingId(7),
            name: "fib".to_string(),
            capture_kind: CaptureKind::Weak,
        }];
        let elab = make_elab_with_captures(captures);
        assert!(
            validate_drop_plan(&elab).is_empty(),
            "Weak capture on a LambdaActorHandle is the recursive self-case (§5.9 ratification 2)"
        );
    }

    #[test]
    fn weak_capture_on_duplex_handle_is_rejected() {
        // Weak captures are exclusive to LambdaActorHandle Places.
        // Attaching a Weak capture to a Duplex handle would silently
        // relax the refcount discipline on a non-actor resource.
        let captures = vec![LambdaCapture {
            actor_handle: Place::DuplexHandle(0),
            captured: BindingId(7),
            name: "ch".to_string(),
            capture_kind: CaptureKind::Weak,
        }];
        let elab = make_elab_with_captures(captures);
        let findings = validate_drop_plan(&elab);
        assert_eq!(findings.len(), 1);
        let MirCheck::DropPlanUndetermined { reason, .. } = &findings[0] else {
            panic!("expected DropPlanUndetermined, got {:?}", findings[0]);
        };
        assert!(
            reason.contains("weak capture") && reason.contains("ch"),
            "diagnostic must name the capture and identify the misuse: {reason}"
        );
    }

    #[test]
    fn weak_capture_on_local_place_is_rejected() {
        // Same invariant for a plain Local — only LambdaActorHandle
        // can host a Weak capture.
        let captures = vec![LambdaCapture {
            actor_handle: Place::Local(5),
            captured: BindingId(2),
            name: "x".to_string(),
            capture_kind: CaptureKind::Weak,
        }];
        let elab = make_elab_with_captures(captures);
        assert_eq!(validate_drop_plan(&elab).len(), 1);
    }

    #[test]
    fn strong_capture_is_accepted_on_any_handle_kind() {
        // Strong captures are unrestricted — they're the default for
        // every non-self capture and the refcount discipline is
        // strong everywhere. Pair Strong with Local, DuplexHandle,
        // and LambdaActorHandle — all should accept.
        for handle in [
            Place::Local(0),
            Place::DuplexHandle(0),
            Place::LambdaActorHandle(0),
        ] {
            let captures = vec![LambdaCapture {
                actor_handle: handle,
                captured: BindingId(1),
                name: "captured".to_string(),
                capture_kind: CaptureKind::Strong,
            }];
            let elab = make_elab_with_captures(captures);
            assert!(
                validate_drop_plan(&elab).is_empty(),
                "Strong capture on {handle:?} must be accepted"
            );
        }
    }

    #[test]
    fn multiple_captures_one_weak_one_strong_validates_correctly() {
        // Mixed-capture case: the self-binding-name is Weak (on the
        // actor's own LambdaActorHandle), a non-self captured value
        // is Strong. Both must coexist without findings.
        let captures = vec![
            LambdaCapture {
                actor_handle: Place::LambdaActorHandle(0),
                captured: BindingId(1),
                name: "fib".to_string(),
                capture_kind: CaptureKind::Weak,
            },
            LambdaCapture {
                actor_handle: Place::LambdaActorHandle(0),
                captured: BindingId(2),
                name: "memo".to_string(),
                capture_kind: CaptureKind::Strong,
            },
        ];
        let elab = make_elab_with_captures(captures);
        assert!(validate_drop_plan(&elab).is_empty());
    }

    #[test]
    fn non_recursive_lambda_has_zero_weak_captures() {
        // The non-recursive lambda case: `let f = actor |n| { n + 1 }`.
        // The body does not reference its own binding name, so the
        // capture set contains zero Weak entries. The plan validates;
        // any Strong captures (closed-over outer bindings) coexist
        // freely.
        let captures = vec![LambdaCapture {
            actor_handle: Place::LambdaActorHandle(0),
            captured: BindingId(1),
            name: "outer".to_string(),
            capture_kind: CaptureKind::Strong,
        }];
        let elab = make_elab_with_captures(captures);
        assert!(validate_drop_plan(&elab).is_empty());
    }

    #[test]
    fn lambda_actor_capture_without_backend_slot_is_diagnostic() {
        let body = HirExpr {
            node: hew_hir::HirNodeId(1),
            site: hew_hir::SiteId(1),
            value_class: ValueClass::BitCopy,
            ty: ResolvedTy::Unit,
            intent: IntentKind::Read,
            kind: HirExprKind::Literal(HirLiteral::Unit),
            span: 0..0,
        };
        let expr = HirExpr {
            node: hew_hir::HirNodeId(2),
            site: hew_hir::SiteId(42),
            value_class: ValueClass::BitCopy,
            ty: ResolvedTy::Unit,
            intent: IntentKind::Read,
            kind: HirExprKind::SpawnLambdaActor {
                params: vec![],
                reply_ty: ResolvedTy::Unit,
                body: Box::new(body),
                captures: vec![hew_hir::HirLambdaCapture {
                    binding: BindingId(99),
                    name: "missing".to_string(),
                    kind: hew_hir::HirCaptureKind::Strong,
                }],
            },
            span: 0..0,
        };
        let mut builder = Builder::default();

        let _ = builder.lower_spawn_lambda_actor(&expr);

        assert!(
            builder.diagnostics.iter().any(|diag| matches!(
                diag.kind,
                MirDiagnosticKind::CannotMaterializeClosureCapture {
                    binding: BindingId(99),
                    site: hew_hir::SiteId(42),
                    ..
                }
            )),
            "missing backend slot must be diagnosed, got {:?}",
            builder.diagnostics
        );
        assert!(
            builder.lambda_captures.is_empty(),
            "unmaterialized captures must not enter lambda_captures"
        );
    }

    #[test]
    fn recursive_lambda_has_exactly_one_weak_capture() {
        // The canonical forward-bind recursive shape:
        //   let fib = actor |n| { ... fib(n - 1) ... };
        // The capture-set discovery (slice 4) must emit EXACTLY ONE
        // Weak capture (the `fib` self-reference) plus zero-or-more
        // Strong captures for outer bindings. Pin the "exactly one"
        // half of the discipline as a structural invariant on the
        // synthetic capture list.
        let captures = vec![LambdaCapture {
            actor_handle: Place::LambdaActorHandle(0),
            captured: BindingId(7),
            name: "fib".to_string(),
            capture_kind: CaptureKind::Weak,
        }];
        let elab = make_elab_with_captures(captures.clone());
        let weak_count = captures
            .iter()
            .filter(|c| matches!(c.capture_kind, CaptureKind::Weak))
            .count();
        assert_eq!(
            weak_count, 1,
            "recursive lambda must have exactly one Weak capture (the self-binding-name)"
        );
        assert!(validate_drop_plan(&elab).is_empty());
    }

    #[test]
    fn multiple_weak_captures_on_same_actor_handle_is_rejected() {
        // Two Weak captures on the same LambdaActorHandle would mean
        // the lambda has two self-binding-names — structurally
        // impossible (a let-binding has exactly one name). This is a
        // lowering bug; fail closed.
        let captures = vec![
            LambdaCapture {
                actor_handle: Place::LambdaActorHandle(0),
                captured: BindingId(1),
                name: "fib".to_string(),
                capture_kind: CaptureKind::Weak,
            },
            LambdaCapture {
                actor_handle: Place::LambdaActorHandle(0),
                captured: BindingId(2),
                name: "fib_shadow".to_string(),
                capture_kind: CaptureKind::Weak,
            },
        ];
        let elab = make_elab_with_captures(captures);
        let findings = validate_drop_plan(&elab);
        assert_eq!(findings.len(), 1, "expected one finding");
        let MirCheck::DropPlanUndetermined { reason, .. } = &findings[0] else {
            panic!("expected DropPlanUndetermined, got {:?}", findings[0]);
        };
        assert!(
            reason.contains("LambdaActorHandle(0)")
                && reason.contains("weak captures")
                && reason.contains("fib"),
            "diagnostic must name the actor handle, count, and capture names: {reason}"
        );
    }

    #[test]
    fn multiple_weak_captures_on_distinct_actor_handles_are_independent() {
        // Two distinct lambda-actors, each with their own Weak self-
        // capture. Validation must accept — the "exactly one" rule
        // is per-LambdaActorHandle.
        let captures = vec![
            LambdaCapture {
                actor_handle: Place::LambdaActorHandle(0),
                captured: BindingId(1),
                name: "fib".to_string(),
                capture_kind: CaptureKind::Weak,
            },
            LambdaCapture {
                actor_handle: Place::LambdaActorHandle(1),
                captured: BindingId(2),
                name: "fact".to_string(),
                capture_kind: CaptureKind::Weak,
            },
        ];
        let elab = make_elab_with_captures(captures);
        assert!(validate_drop_plan(&elab).is_empty());
    }
}
// ============================================================================
// Generative property tests for per-Return live-set narrowing.
//
// The fixed-shape tests in `slice3_invariants` pin three hand-built shapes
// (one live handle survives, every handle moved out, three live handles).
// These proptest cases exercise the narrowing algorithm over randomly-
// generated `(defined-bindings, moved-out-subset)` inputs so the
// `dropped(block) == defined(block) - moved_out(block)` invariant is
// checked across the full state space rather than three fixtures.
//
// The proptest dependency is scoped to `cfg(not(target_arch = "wasm32"))`
// in `Cargo.toml` (matches the pattern in `hew-runtime`). The
// `cfg_attr(target_arch = "wasm32", allow(unused))` on the module below
// keeps the wasm build clean — the entire module compiles away on wasm
// since `proptest` is not available there.
// ============================================================================

#[cfg(all(test, not(target_arch = "wasm32")))]
mod slice3_narrowing_proptests {
    use super::*;
    use crate::dataflow::BindingState;
    use proptest::prelude::*;

    /// A `Duplex<i64, i64>` `ResolvedTy` payload — the inner type
    /// detail is irrelevant for narrowing.
    fn duplex_ty() -> ResolvedTy {
        ResolvedTy::named_user("Duplex", vec![ResolvedTy::I64, ResolvedTy::I64])
    }

    /// Build a single-block `BasicBlock` with a `Return` terminator.
    fn single_return_block(block_id: u32) -> BasicBlock {
        BasicBlock {
            id: block_id,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Return,
        }
    }

    /// Build the function-wide LIFO drop template for `n` `DuplexHandle`
    /// bindings, indexed `0..n`. Each drop carries the canonical
    /// `(DuplexHandle(i), DropKind::DuplexClose)` shape.
    fn build_lifo(n: u32) -> Vec<ElabDrop> {
        (0..n)
            .map(|i| ElabDrop {
                place: Place::DuplexHandle(i),
                ty: duplex_ty(),
                drop_fn: None,
                kind: DropKind::DuplexClose,
                guard: None,
            })
            .collect()
    }

    /// Build the `binding_locals` map: `BindingId(i) -> DuplexHandle(i)`.
    fn build_binding_locals(n: u32) -> HashMap<BindingId, Place> {
        (0..n)
            .map(|i| (BindingId(i), Place::DuplexHandle(i)))
            .collect()
    }

    /// Build the `exit_states` map for block 0, marking each binding in
    /// `moved_out` as `Consumed` and every other binding (up to `n`) as
    /// `Live`. The narrowing must keep Live + `MaybeConsumed` and drop
    /// Consumed.
    fn build_exit_states(
        n: u32,
        moved_out: &[u32],
    ) -> HashMap<u32, BTreeMap<BindingId, BindingState>> {
        let mut per_binding: BTreeMap<BindingId, BindingState> = BTreeMap::new();
        for i in 0..n {
            let binding = BindingId(i);
            if moved_out.contains(&i) {
                per_binding.insert(binding, BindingState::Consumed(SiteId(0)));
            } else {
                per_binding.insert(binding, BindingState::Live);
            }
        }
        let mut map = HashMap::new();
        map.insert(0u32, per_binding);
        map
    }

    proptest! {
        /// `dropped(block) == defined(block) - moved_out(block)`.
        ///
        /// For every randomly-generated `(N, moved_out_subset)` input:
        ///   - N is the number of defined DuplexHandle bindings.
        ///   - moved_out_subset is the indices Consumed before Return.
        ///   - The narrowed drop list must contain exactly the indices
        ///     NOT in moved_out_subset, each as a DuplexHandle(i) drop.
        ///
        /// Proptest's default 256 cases sweeps shapes from N=0 (empty
        /// drop list) up to N=8 (all eight handles live or moved
        /// across every subset combination).
        #[test]
        fn dropped_equals_defined_minus_moved_out(
            n in 0u32..8,
            moved_out_mask in 0u32..256,
        ) {
            let moved_out: Vec<u32> = (0..n)
                .filter(|i| (moved_out_mask >> i) & 1 == 1)
                .collect();

            let blocks = vec![single_return_block(0)];
            let lifo = build_lifo(n);
            let exit_states = build_exit_states(n, &moved_out);
            let binding_locals = build_binding_locals(n);

            let (_, plans) = enumerate_exits(&blocks, &lifo, &exit_states, &HashMap::new(), &binding_locals, &HashSet::new(), &HashMap::new(), &HashMap::new(), &HashSet::new());

            // Exactly one Return plan for the single block.
            prop_assert_eq!(plans.len(), 1);
            let (exit, plan) = &plans[0];
            let is_return_block_0 = matches!(exit, ExitPath::Return { block: 0 });
            prop_assert!(is_return_block_0);

            // Build the expected drop list: every index 0..n not in
            // moved_out, as a DuplexHandle drop. The order matches the
            // input `lifo` template's iteration order (build_lifo
            // emits 0..n in forward order; enumerate_exits' filter
            // preserves that order).
            let expected: Vec<Place> = (0..n)
                .filter(|i| !moved_out.contains(i))
                .map(Place::DuplexHandle)
                .collect();
            let actual: Vec<Place> = plan.drops.iter().map(|d| d.place).collect();
            prop_assert_eq!(actual, expected,
                "narrowing: defined={}, moved_out={:?}", n, moved_out);
        }

        /// `MaybeConsumed` at a Return is treated as Live for drop-plan
        /// purposes (the move-checker rejects the program upstream, but
        /// the drop list stays informational). Sweep random subsets
        /// where bindings are MaybeConsumed and assert each appears in
        /// the drop list alongside the Live bindings.
        #[test]
        fn maybe_consumed_appears_in_drop_list(
            n in 0u32..6,
            maybe_mask in 0u32..64,
            consumed_mask in 0u32..64,
        ) {
            // Decide per-binding state: Consumed wins over MaybeConsumed
            // wins over Live so the masks don't overlap meaningfully —
            // a binding is Consumed if its bit is set in consumed_mask,
            // else MaybeConsumed if set in maybe_mask, else Live.
            let mut per_binding: BTreeMap<BindingId, BindingState> = BTreeMap::new();
            for i in 0..n {
                let state = if (consumed_mask >> i) & 1 == 1 {
                    BindingState::Consumed(SiteId(0))
                } else if (maybe_mask >> i) & 1 == 1 {
                    BindingState::MaybeConsumed(SiteId(0))
                } else {
                    BindingState::Live
                };
                per_binding.insert(BindingId(i), state);
            }
            let mut exit_states = HashMap::new();
            exit_states.insert(0u32, per_binding);

            let blocks = vec![single_return_block(0)];
            let lifo = build_lifo(n);
            let binding_locals = build_binding_locals(n);

            let (_, plans) = enumerate_exits(&blocks, &lifo, &exit_states, &HashMap::new(), &binding_locals, &HashSet::new(), &HashMap::new(), &HashMap::new(), &HashSet::new());
            let (_, plan) = &plans[0];

            // Expected: every binding NOT Consumed survives in the drop
            // list (Live and MaybeConsumed both qualify).
            let dropped: std::collections::HashSet<u32> = plan
                .drops
                .iter()
                .filter_map(|d| match d.place {
                    Place::DuplexHandle(i) => Some(i),
                    _ => None,
                })
                .collect();
            for i in 0..n {
                let is_consumed = (consumed_mask >> i) & 1 == 1;
                prop_assert_eq!(
                    dropped.contains(&i),
                    !is_consumed,
                    "binding {} state should determine drop-list membership", i
                );
            }
        }

        /// The narrowing is deterministic: running `enumerate_exits`
        /// twice on the same inputs produces the same drops. (Catches
        /// any HashMap-iteration-order leakage into the output.)
        #[test]
        fn narrowing_is_deterministic(
            n in 0u32..8,
            moved_out_mask in 0u32..256,
        ) {
            let moved_out: Vec<u32> = (0..n)
                .filter(|i| (moved_out_mask >> i) & 1 == 1)
                .collect();
            let blocks = vec![single_return_block(0)];
            let lifo = build_lifo(n);
            let exit_states = build_exit_states(n, &moved_out);
            let binding_locals = build_binding_locals(n);

            let (b1, p1) = enumerate_exits(&blocks, &lifo, &exit_states, &HashMap::new(), &binding_locals, &HashSet::new(), &HashMap::new(), &HashMap::new(), &HashSet::new());
            let (b2, p2) = enumerate_exits(&blocks, &lifo, &exit_states, &HashMap::new(), &binding_locals, &HashSet::new(), &HashMap::new(), &HashMap::new(), &HashSet::new());

            prop_assert_eq!(b1.len(), b2.len());
            prop_assert_eq!(p1.len(), p2.len());
            for ((e1, plan1), (e2, plan2)) in p1.iter().zip(p2.iter()) {
                prop_assert_eq!(e1, e2);
                prop_assert_eq!(&plan1.drops, &plan2.drops);
            }
        }

        /// No binding outside the function's owned set ever appears in
        /// the narrowed drop list. The narrowing must be a subset
        /// operation — it never INVENTS a drop.
        #[test]
        fn narrowing_never_invents_drops(
            n in 0u32..8,
            moved_out_mask in 0u32..256,
        ) {
            let moved_out: Vec<u32> = (0..n)
                .filter(|i| (moved_out_mask >> i) & 1 == 1)
                .collect();
            let blocks = vec![single_return_block(0)];
            let lifo = build_lifo(n);
            let exit_states = build_exit_states(n, &moved_out);
            let binding_locals = build_binding_locals(n);

            let (_, plans) = enumerate_exits(&blocks, &lifo, &exit_states, &HashMap::new(), &binding_locals, &HashSet::new(), &HashMap::new(), &HashMap::new(), &HashSet::new());
            let (_, plan) = &plans[0];

            for d in &plan.drops {
                let Place::DuplexHandle(i) = d.place else {
                    panic!("non-DuplexHandle drop appeared: {:?}", d.place);
                };
                prop_assert!(i < n, "drop for binding {} but only {} defined", i, n);
            }
        }
    }

    // ── #2395: suspend / yield abandon-edge drop plans ────────────────────────
    // enumerate_exits must populate the ExitPath::Suspend / ExitPath::Yield plan
    // with the exit's live owned-local drops (fired by codegen on the
    // destroy-while-parked abandon edge), and exclude a moved-out (Consumed)
    // binding so a value moved across the suspend is never double-freed.

    fn single_suspend_block(id: u32) -> BasicBlock {
        BasicBlock {
            id,
            statements: vec![],
            instructions: vec![],
            // resume/cleanup alias to a resume target as the collapsed carriers do;
            // the plan keys off the suspend terminator's own block id.
            terminator: Terminator::Suspend {
                resume: id + 1,
                cleanup: id + 1,
                is_final: false,
            },
        }
    }

    fn single_yield_block(id: u32) -> BasicBlock {
        BasicBlock {
            id,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Yield {
                value: Place::Local(99),
                next: id + 1,
            },
        }
    }

    #[test]
    fn suspend_exit_plan_carries_live_owned_local_drop() {
        let blocks = vec![single_suspend_block(0)];
        let lifo = build_lifo(1);
        let exit_states = build_exit_states(1, &[]);
        let binding_locals = build_binding_locals(1);
        let (_, plans) = enumerate_exits(
            &blocks,
            &lifo,
            &exit_states,
            &HashMap::new(),
            &binding_locals,
            &HashSet::new(),
            &HashMap::new(),
            &HashMap::new(),
            &HashSet::new(),
        );
        let (exit, plan) = &plans[0];
        assert!(matches!(exit, ExitPath::Suspend { block: 0, .. }));
        assert_eq!(
            plan.drops.iter().map(|d| d.place).collect::<Vec<_>>(),
            vec![Place::DuplexHandle(0)],
            "the live owned local must be dropped on the suspend abandon edge",
        );
    }

    #[test]
    fn suspend_exit_plan_excludes_moved_out_local() {
        let blocks = vec![single_suspend_block(0)];
        let lifo = build_lifo(1);
        // BindingId(0) is Consumed (moved out across the suspend).
        let exit_states = build_exit_states(1, &[0]);
        let binding_locals = build_binding_locals(1);
        let (_, plans) = enumerate_exits(
            &blocks,
            &lifo,
            &exit_states,
            &HashMap::new(),
            &binding_locals,
            &HashSet::new(),
            &HashMap::new(),
            &HashMap::new(),
            &HashSet::new(),
        );
        let (_, plan) = &plans[0];
        assert!(
            plan.drops.is_empty(),
            "a moved-out (Consumed) local must NOT be dropped on the abandon edge \
             (double-free wall); got {:?}",
            plan.drops,
        );
    }

    #[test]
    fn yield_exit_plan_carries_live_drop_and_excludes_consumed() {
        // Live binding: dropped on the yield abandon (destroy-while-parked-at-yield) edge.
        let blocks = vec![single_yield_block(0)];
        let lifo = build_lifo(1);
        let live = build_exit_states(1, &[]);
        let binding_locals = build_binding_locals(1);
        let (_, plans) = enumerate_exits(
            &blocks,
            &lifo,
            &live,
            &HashMap::new(),
            &binding_locals,
            &HashSet::new(),
            &HashMap::new(),
            &HashMap::new(),
            &HashSet::new(),
        );
        let (exit, plan) = &plans[0];
        assert!(matches!(exit, ExitPath::Yield { block: 0, .. }));
        assert_eq!(
            plan.drops.iter().map(|d| d.place).collect::<Vec<_>>(),
            vec![Place::DuplexHandle(0)],
        );

        // The just-yielded value is a MOVE into the companion out-slot, so its
        // binding is Consumed at the yield exit and is excluded — its sole owner
        // is hew_gen_coro_destroy's out_drop_thunk (no second dropper).
        let consumed = build_exit_states(1, &[0]);
        let (_, plans) = enumerate_exits(
            &blocks,
            &lifo,
            &consumed,
            &HashMap::new(),
            &binding_locals,
            &HashSet::new(),
            &HashMap::new(),
            &HashMap::new(),
            &HashSet::new(),
        );
        assert!(
            plans[0].1.drops.is_empty(),
            "a Consumed (moved-to-out-slot) yield value must be excluded from the \
             yield abandon plan; got {:?}",
            plans[0].1.drops,
        );
    }
}
// ============================================================================
// Slice 3.5 cross-block stale-DuplexHandle detection — generative property
// tests against `validate_cross_block_split_consume` built directly on
// hand-constructed `BasicBlock` + `Instr` shapes. The full source pipeline
// can't drive these tests because the parser surface for `.send_half()` /
// `.recv_half()` is slice-4 work; the synthetic inputs mirror what slice 4
// will eventually emit.
// ============================================================================

#[cfg(all(test, not(target_arch = "wasm32")))]
mod slice35_cross_block_proptests {
    use super::*;
    use proptest::prelude::*;

    fn duplex_ty() -> ResolvedTy {
        ResolvedTy::named_user("Duplex", vec![ResolvedTy::I64, ResolvedTy::I64])
    }

    /// Build a single-block CFG that splits `DuplexHandle(parent)` into
    /// a `SendHalf(parent)` then attempts to drop the unified handle
    /// after the split. The structural same-list check already rejects
    /// this shape; the cross-block check must also catch the
    /// dataflow-derived stale state on the same block (a Live entry
    /// followed by a Move-to-half transitions to Consumed before the
    /// block terminator, but the drop plan was assembled from the
    /// pre-Move LIFO — fail-closed).
    fn build_split_block(parent: u32, terminator: Terminator) -> BasicBlock {
        BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![Instr::Move {
                dest: Place::SendHalf(parent),
                src: Place::DuplexHandle(parent),
            }],
            terminator,
        }
    }

    fn elab_with_return_drop(parent: u32) -> ElaboratedMirFunction {
        ElaboratedMirFunction {
            name: "f".to_string(),
            return_ty: ResolvedTy::Unit,
            statements: vec![],
            decisions: vec![],
            blocks: vec![ElabBlock {
                id: 0,
                kind: BlockKind::Normal,
                drops: vec![],
                successor: None,
            }],
            drop_plans: vec![(
                ExitPath::Return { block: 0 },
                DropPlan {
                    drops: vec![ElabDrop {
                        place: Place::DuplexHandle(parent),
                        ty: duplex_ty(),
                        drop_fn: None,
                        kind: DropKind::DuplexClose,
                        guard: None,
                    }],
                },
            )],
            coroutine: None,
            lambda_captures: vec![],
        }
    }

    proptest! {
        /// Cross-block: A splits DuplexHandle(p) in block 0, a drop on
        /// it appears in the Return plan. The checker must fire
        /// DropPlanUndetermined.
        #[test]
        fn split_in_predecessor_rejects_stale_unified_drop(
            parent in 0u32..8,
        ) {
            let blocks = vec![build_split_block(parent, Terminator::Return)];
            let elab = elab_with_return_drop(parent);
            let findings = validate_cross_block_split_consume(&blocks, &elab);
            prop_assert!(
                findings
                    .iter()
                    .any(|f| matches!(f, MirCheck::DropPlanUndetermined { .. })),
                "expected DropPlanUndetermined when DuplexHandle({parent}) is split AND \
                 still appears in the drop plan; got {findings:?}"
            );
        }

        /// Cross-block: block 0 branches into 1 (split path) and 2
        /// (no-split path), both jump to block 3 whose Return plan
        /// drops the unified handle. The meet of preds at block 3 is
        /// `MaybeConsumed` — fail-closed.
        #[test]
        fn split_on_some_paths_rejects_unified_drop_at_join(
            parent in 0u32..8,
        ) {
            let blocks = vec![
                // Entry: branch on a dummy cond (cond Place won't be
                // evaluated by the validator — only Move shape matters).
                BasicBlock {
                    id: 0,
                    statements: vec![],
                    instructions: vec![],
                    terminator: Terminator::Branch {
                        cond: Place::Local(99),
                        then_target: 1,
                        else_target: 2,
                    },
                },
                // Then: split DuplexHandle(parent) into SendHalf.
                BasicBlock {
                    id: 1,
                    statements: vec![],
                    instructions: vec![Instr::Move {
                        dest: Place::SendHalf(parent),
                        src: Place::DuplexHandle(parent),
                    }],
                    terminator: Terminator::Goto { target: 3 },
                },
                // Else: no split.
                BasicBlock {
                    id: 2,
                    statements: vec![],
                    instructions: vec![],
                    terminator: Terminator::Goto { target: 3 },
                },
                // Join: Return — the drop plan is checked here.
                BasicBlock {
                    id: 3,
                    statements: vec![],
                    instructions: vec![],
                    terminator: Terminator::Return,
                },
            ];
            // The Return ExitPath references block 3 — point the elab
            // drop plan at it.
            let mut elab = elab_with_return_drop(parent);
            elab.drop_plans = vec![(
                ExitPath::Return { block: 3 },
                DropPlan {
                    drops: vec![ElabDrop {
                        place: Place::DuplexHandle(parent),
                        ty: duplex_ty(),
                        drop_fn: None,
                        kind: DropKind::DuplexClose,
                        guard: None,
                    }],
                },
            )];
            elab.blocks = vec![
                ElabBlock {
                    id: 0,
                    kind: BlockKind::Normal,
                    drops: vec![],
                    successor: None,
                },
                ElabBlock {
                    id: 1,
                    kind: BlockKind::Normal,
                    drops: vec![],
                    successor: None,
                },
                ElabBlock {
                    id: 2,
                    kind: BlockKind::Normal,
                    drops: vec![],
                    successor: None,
                },
                ElabBlock {
                    id: 3,
                    kind: BlockKind::Normal,
                    drops: vec![],
                    successor: None,
                },
            ];

            let findings = validate_cross_block_split_consume(&blocks, &elab);
            let has_undetermined = findings
                .iter()
                .any(|f| matches!(f, MirCheck::DropPlanUndetermined { .. }));
            prop_assert!(
                has_undetermined,
                "expected DropPlanUndetermined at join block 3 when DuplexHandle({parent}) \
                 is split on only the then-path; got {findings:?}"
            );
            // The reason text should anchor at the split-emitting block (1).
            let reason_mentions_block_1 = findings.iter().any(|f| match f {
                MirCheck::DropPlanUndetermined { reason, .. } => reason.contains("block 1"),
                _ => false,
            });
            prop_assert!(
                reason_mentions_block_1,
                "diagnostic reason should cite block 1 as the split-emitting block; \
                 got {findings:?}"
            );
        }

        /// Cross-block: split on EVERY path. The meet at the join is
        /// `Consumed`. The drop on the unified handle at the join
        /// must be rejected with a Consumed reason (not MaybeConsumed).
        #[test]
        fn split_on_every_path_rejects_unified_drop_at_join(
            parent in 0u32..8,
        ) {
            let blocks = vec![
                BasicBlock {
                    id: 0,
                    statements: vec![],
                    instructions: vec![],
                    terminator: Terminator::Branch {
                        cond: Place::Local(99),
                        then_target: 1,
                        else_target: 2,
                    },
                },
                BasicBlock {
                    id: 1,
                    statements: vec![],
                    instructions: vec![Instr::Move {
                        dest: Place::SendHalf(parent),
                        src: Place::DuplexHandle(parent),
                    }],
                    terminator: Terminator::Goto { target: 3 },
                },
                BasicBlock {
                    id: 2,
                    statements: vec![],
                    instructions: vec![Instr::Move {
                        dest: Place::RecvHalf(parent),
                        src: Place::DuplexHandle(parent),
                    }],
                    terminator: Terminator::Goto { target: 3 },
                },
                BasicBlock {
                    id: 3,
                    statements: vec![],
                    instructions: vec![],
                    terminator: Terminator::Return,
                },
            ];
            let mut elab = elab_with_return_drop(parent);
            elab.drop_plans = vec![(
                ExitPath::Return { block: 3 },
                DropPlan {
                    drops: vec![ElabDrop {
                        place: Place::DuplexHandle(parent),
                        ty: duplex_ty(),
                        drop_fn: None,
                        kind: DropKind::DuplexClose,
                        guard: None,
                    }],
                },
            )];
            elab.blocks = (0..=3)
                .map(|id| ElabBlock {
                    id,
                    kind: BlockKind::Normal,
                    drops: vec![],
                    successor: None,
                })
                .collect();

            let findings = validate_cross_block_split_consume(&blocks, &elab);
            prop_assert!(
                findings
                    .iter()
                    .any(|f| matches!(f, MirCheck::DropPlanUndetermined { .. })),
                "expected DropPlanUndetermined when DuplexHandle({parent}) is split on \
                 every reaching path; got {findings:?}"
            );
        }

        /// Non-regression: no split, no rejection. A drop plan that
        /// fires the unified DuplexHandle on a block with no
        /// predecessor split must accept silently — the checker is a
        /// fail-CLOSED gate, not a fail-OPEN one. (No findings is the
        /// expected outcome.)
        #[test]
        fn no_split_no_rejection(
            parent in 0u32..8,
        ) {
            let blocks = vec![
                BasicBlock {
                    id: 0,
                    statements: vec![],
                    instructions: vec![],
                    terminator: Terminator::Return,
                },
            ];
            let elab = elab_with_return_drop(parent);
            let findings = validate_cross_block_split_consume(&blocks, &elab);
            prop_assert!(
                findings.is_empty(),
                "no split observed; the cross-block check must not invent findings; \
                 got {findings:?}"
            );
        }

        /// Multi-return: two Return blocks, the unified DuplexHandle is
        /// split on the predecessor edge of one Return but not the
        /// other. The first Return's drop fires legally (no preceding
        /// split); the second's must be rejected.
        #[test]
        fn multi_return_per_path_drops_only_flag_split_path(
            parent in 0u32..8,
        ) {
            let blocks = vec![
                // Entry branches into two Return blocks.
                BasicBlock {
                    id: 0,
                    statements: vec![],
                    instructions: vec![],
                    terminator: Terminator::Branch {
                        cond: Place::Local(99),
                        then_target: 1,
                        else_target: 2,
                    },
                },
                // Then-arm: split + return.
                BasicBlock {
                    id: 1,
                    statements: vec![],
                    instructions: vec![Instr::Move {
                        dest: Place::SendHalf(parent),
                        src: Place::DuplexHandle(parent),
                    }],
                    terminator: Terminator::Return,
                },
                // Else-arm: no split, return.
                BasicBlock {
                    id: 2,
                    statements: vec![],
                    instructions: vec![],
                    terminator: Terminator::Return,
                },
            ];
            let mut elab = elab_with_return_drop(parent);
            // Two Return drop plans, one per Return-terminated block.
            elab.drop_plans = vec![
                (
                    ExitPath::Return { block: 1 },
                    DropPlan {
                        drops: vec![ElabDrop {
                            place: Place::DuplexHandle(parent),
                            ty: duplex_ty(),
                            drop_fn: None,
                            kind: DropKind::DuplexClose,
                            guard: None,
                        }],
                    },
                ),
                (
                    ExitPath::Return { block: 2 },
                    DropPlan {
                        drops: vec![ElabDrop {
                            place: Place::DuplexHandle(parent),
                            ty: duplex_ty(),
                            drop_fn: None,
                            kind: DropKind::DuplexClose,
                            guard: None,
                        }],
                    },
                ),
            ];
            elab.blocks = (0..=2)
                .map(|id| ElabBlock {
                    id,
                    kind: BlockKind::Normal,
                    drops: vec![],
                    successor: None,
                })
                .collect();

            let findings = validate_cross_block_split_consume(&blocks, &elab);
            // Exactly one finding: the Return at block 1 (split path).
            // Block 2's Return drop fires on the unified handle that
            // was never split on its path — accept.
            let on_block_1 = findings.iter().filter(|f| {
                matches!(f, MirCheck::DropPlanUndetermined { block, .. } if *block == 1)
            }).count();
            let on_block_2 = findings.iter().filter(|f| {
                matches!(f, MirCheck::DropPlanUndetermined { block, .. } if *block == 2)
            }).count();
            prop_assert_eq!(on_block_1, 1, "block 1 (split path) must reject the unified drop");
            prop_assert_eq!(on_block_2, 0, "block 2 (no-split path) must accept the unified drop");
        }
    }
}
// ============================================================================
// CFG Select-arm correctness — cross-block split-consume via `Terminator::Select`
//
// These are the LOAD-BEARING negative-regression tests for the
// `BasicBlock::successors()` Select-arm fix (NEW-A/vestigial-r2).
//
// Before the fix, `successors()` returned only `[next]` for a Select block,
// making every arm body unreachable in the predecessor / worklist computation.
// The `validate_cross_block_split_consume` checker could therefore not see any
// splits that occurred inside an arm body → a DuplexHandle consumed in BOTH
// arm bodies AND still present in the join block's drop plan was ADMITTED
// (fail-open).  After the fix the arm bodies are real successors → the checker
// visits them → the stale unified-handle drop is rejected (fail-closed).
//
// Test structure:
//  Block 0: `Terminator::Select { arms: [{body: 1}, {body: 2}], next: 3 }`
//  Block 1 (arm 0 body): splits DuplexHandle(p) → SendHalf(p); `Goto { target: 3 }`
//  Block 2 (arm 1 body): splits DuplexHandle(p) → RecvHalf(p); `Goto { target: 3 }`
//  Block 3 (join / next): `Return`, drop plan = [DuplexHandle(p)]
//
// Because exactly one arm executes at runtime, at block 3 the handle is in
// state MaybeConsumed (some-but-not-all paths consumed it) when viewed from
// the conservative no-arm Select→next edge — that alone is enough for
// fail-closed.  The checker reports DropPlanUndetermined.
// ============================================================================
#[cfg(all(test, not(target_arch = "wasm32")))]
mod select_arm_split_consume_tests {
    use super::*;

    fn duplex_ty() -> ResolvedTy {
        ResolvedTy::named_user("Duplex", vec![ResolvedTy::I64, ResolvedTy::I64])
    }

    fn select_arm_with_body(body_block: u32) -> SelectArm {
        SelectArm {
            body_block,
            kind: SelectArmKind::AfterTimer {
                duration: Place::Local(99),
            },
            binding: None,
        }
    }

    /// Build the four-block CFG:
    ///   0: Select { arms: [body=1, body=2], next: 3 }
    ///   1: split DuplexHandle(parent) → SendHalf(parent); Goto 3
    ///   2: split DuplexHandle(parent) → RecvHalf(parent); Goto 3
    ///   3: Return (drop plan has DuplexHandle(parent) — stale after either arm)
    fn build_select_double_split_cfg(parent: u32) -> (Vec<BasicBlock>, ElaboratedMirFunction) {
        let blocks = vec![
            BasicBlock {
                id: 0,
                statements: vec![],
                instructions: vec![],
                terminator: Terminator::Select {
                    arms: vec![select_arm_with_body(1), select_arm_with_body(2)],
                    next: 3,
                },
            },
            BasicBlock {
                id: 1,
                statements: vec![],
                instructions: vec![Instr::Move {
                    dest: Place::SendHalf(parent),
                    src: Place::DuplexHandle(parent),
                }],
                terminator: Terminator::Goto { target: 3 },
            },
            BasicBlock {
                id: 2,
                statements: vec![],
                instructions: vec![Instr::Move {
                    dest: Place::RecvHalf(parent),
                    src: Place::DuplexHandle(parent),
                }],
                terminator: Terminator::Goto { target: 3 },
            },
            BasicBlock {
                id: 3,
                statements: vec![],
                instructions: vec![],
                terminator: Terminator::Return,
            },
        ];
        let elab = ElaboratedMirFunction {
            name: "f".to_string(),
            return_ty: ResolvedTy::Unit,
            statements: vec![],
            decisions: vec![],
            blocks: (0u32..=3)
                .map(|id| ElabBlock {
                    id,
                    kind: BlockKind::Normal,
                    drops: vec![],
                    successor: None,
                })
                .collect(),
            drop_plans: vec![(
                ExitPath::Return { block: 3 },
                DropPlan {
                    drops: vec![ElabDrop {
                        place: Place::DuplexHandle(parent),
                        ty: duplex_ty(),
                        drop_fn: None,
                        kind: DropKind::DuplexClose,
                        guard: None,
                    }],
                },
            )],
            coroutine: None,
            lambda_captures: vec![],
        };
        (blocks, elab)
    }

    /// **Negative regression (fail-open → fail-closed):**
    ///
    /// A `Select` where BOTH arm bodies split the same `DuplexHandle` must
    /// produce a `DropPlanUndetermined` for the drop plan at the join block.
    ///
    /// Pre-fix: `successors()` returned only `[next]` for the Select block,
    /// so arm bodies 1 and 2 were never visited → checker saw `DuplexHandle`
    /// as `Live` at block 3 → NO finding → **fail-open** (admitted).
    ///
    /// Post-fix: arm bodies are real successors → checker processes them →
    /// MaybeConsumed(1) at block 3 → `DropPlanUndetermined` → **fail-closed**.
    #[test]
    fn select_arm_double_split_is_rejected_post_fix() {
        let (blocks, elab) = build_select_double_split_cfg(0);
        let findings = validate_cross_block_split_consume(&blocks, &elab);
        assert!(
            findings
                .iter()
                .any(|f| matches!(f, MirCheck::DropPlanUndetermined { .. })),
            "FAIL-OPEN regression: DuplexHandle(0) split in both Select arms must produce \
             DropPlanUndetermined at the join block; got {findings:?}"
        );
    }

    /// Same as above but with a different parent index — confirms the check is
    /// parametric on the parent local.
    #[test]
    fn select_arm_double_split_rejected_for_any_parent() {
        for parent in 0u32..4 {
            let (blocks, elab) = build_select_double_split_cfg(parent);
            let findings = validate_cross_block_split_consume(&blocks, &elab);
            assert!(
                findings
                    .iter()
                    .any(|f| matches!(f, MirCheck::DropPlanUndetermined { .. })),
                "parent={parent}: double-split across Select arms must be rejected; \
                 got {findings:?}"
            );
        }
    }

    /// **Positive control:** a legal `Select` where only ONE arm splits the
    /// `DuplexHandle` must still be caught (the split arm path is Consumed →
    /// join block is `MaybeConsumed` from the conservative Select→next edge).
    ///
    /// This is NOT the same as "a legal single-arm use" — any split followed by
    /// a stale unified-handle drop is wrong even on one path. We verify the
    /// checker is still fail-closed for single-arm splits (not accidentally
    /// widened to "only reject if both arms split").
    #[test]
    fn select_single_arm_split_still_rejected_at_join() {
        let parent = 0u32;
        let blocks = vec![
            BasicBlock {
                id: 0,
                statements: vec![],
                instructions: vec![],
                terminator: Terminator::Select {
                    arms: vec![select_arm_with_body(1), select_arm_with_body(2)],
                    next: 3,
                },
            },
            BasicBlock {
                id: 1,
                statements: vec![],
                // Only arm 0 splits.
                instructions: vec![Instr::Move {
                    dest: Place::SendHalf(parent),
                    src: Place::DuplexHandle(parent),
                }],
                terminator: Terminator::Goto { target: 3 },
            },
            BasicBlock {
                id: 2,
                statements: vec![],
                instructions: vec![],
                terminator: Terminator::Goto { target: 3 },
            },
            BasicBlock {
                id: 3,
                statements: vec![],
                instructions: vec![],
                terminator: Terminator::Return,
            },
        ];
        let elab = ElaboratedMirFunction {
            name: "f".to_string(),
            return_ty: ResolvedTy::Unit,
            statements: vec![],
            decisions: vec![],
            blocks: (0u32..=3)
                .map(|id| ElabBlock {
                    id,
                    kind: BlockKind::Normal,
                    drops: vec![],
                    successor: None,
                })
                .collect(),
            drop_plans: vec![(
                ExitPath::Return { block: 3 },
                DropPlan {
                    drops: vec![ElabDrop {
                        place: Place::DuplexHandle(parent),
                        ty: duplex_ty(),
                        drop_fn: None,
                        kind: DropKind::DuplexClose,
                        guard: None,
                    }],
                },
            )],
            coroutine: None,
            lambda_captures: vec![],
        };
        let findings = validate_cross_block_split_consume(&blocks, &elab);
        assert!(
            findings
                .iter()
                .any(|f| matches!(f, MirCheck::DropPlanUndetermined { .. })),
            "single-arm split followed by stale unified-handle drop must still be rejected; \
             got {findings:?}"
        );
    }

    /// **True positive control:** a `Select` where NEITHER arm splits the
    /// `DuplexHandle` must accept the unified-handle drop at the join block
    /// (handle was never consumed; the drop is valid).
    ///
    /// This confirms the fix is not over-broad: a non-split Select must not
    /// generate spurious `DropPlanUndetermined` findings.
    #[test]
    fn select_no_split_accepts_unified_handle_drop() {
        let parent = 0u32;
        let blocks = vec![
            BasicBlock {
                id: 0,
                statements: vec![],
                instructions: vec![],
                terminator: Terminator::Select {
                    arms: vec![select_arm_with_body(1), select_arm_with_body(2)],
                    next: 3,
                },
            },
            BasicBlock {
                id: 1,
                statements: vec![],
                instructions: vec![],
                terminator: Terminator::Goto { target: 3 },
            },
            BasicBlock {
                id: 2,
                statements: vec![],
                instructions: vec![],
                terminator: Terminator::Goto { target: 3 },
            },
            BasicBlock {
                id: 3,
                statements: vec![],
                instructions: vec![],
                terminator: Terminator::Return,
            },
        ];
        let elab = ElaboratedMirFunction {
            name: "f".to_string(),
            return_ty: ResolvedTy::Unit,
            statements: vec![],
            decisions: vec![],
            blocks: (0u32..=3)
                .map(|id| ElabBlock {
                    id,
                    kind: BlockKind::Normal,
                    drops: vec![],
                    successor: None,
                })
                .collect(),
            drop_plans: vec![(
                ExitPath::Return { block: 3 },
                DropPlan {
                    drops: vec![ElabDrop {
                        place: Place::DuplexHandle(parent),
                        ty: duplex_ty(),
                        drop_fn: None,
                        kind: DropKind::DuplexClose,
                        guard: None,
                    }],
                },
            )],
            coroutine: None,
            lambda_captures: vec![],
        };
        let findings = validate_cross_block_split_consume(&blocks, &elab);
        assert!(
            findings.is_empty(),
            "no-split Select must not generate spurious DropPlanUndetermined; \
             got {findings:?}"
        );
    }
}
