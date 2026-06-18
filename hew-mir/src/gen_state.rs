//! Generator state-record synthesis and cross-yield liveness pass (S3b).
//!
//! Runs after `lower::Builder::finalize_blocks` produces the CFG for a
//! `gen { … }` body. The pass computes which body locals are LIVE across
//! at least one `Terminator::Yield` site, synthesises a
//! [`GenStateLayout`] describing the resume struct that will carry them
//! across suspensions, rewrites every yield site to checkpoint
//! (store-before-yield) and every resume entry to reload
//! (load-after-resume) those locals through `Place::GenState` field
//! projections, populates the init-mask field so per-state liveness is
//! observable at any suspension point, and synthesises a per-gen-block
//! `__drop_in_state` MIR shim function.
//!
//! ## Scope (S3b — S3b1 + S3b2)
//!
//! 1. **Cross-yield live analysis** (S3b1). Standard backward liveness
//!    over the body CFG, with the per-block transfer function derived
//!    from [`crate::dataflow::instr_reads_writes`] plus terminator
//!    reads. A body local is "cross-yield live" when it is live at the
//!    program point IMMEDIATELY AFTER any `Terminator::Yield` — i.e.
//!    the resume block reads the local along some path before it is
//!    rewritten. The union of those per-yield sets is the lift set.
//!
//! 2. **State-record layout synthesis** (S3b1). Builds a
//!    [`GenStateLayout`] keyed by the body function's name. Field 0 is
//!    the state tag; field 1 is the init-mask; fields 2..N are the
//!    cross-yield-live locals in ascending body-local id order
//!    (deterministic and stable across rebuilds).
//!
//! 3. **Bookend Move synthesis** (S3b1). For every yield block, the
//!    pass inserts one `Instr::Move { dest: GenState { local:
//!    state_local, field: 2 + i }, src: Local(N_i) }` per cross-yield-
//!    live local IMMEDIATELY BEFORE the `Terminator::Yield`. For the
//!    matching resume block, the symmetric reload
//!    `Instr::Move { dest: Local(N_i), src: GenState { … } }` lands at
//!    the TOP of the block's instructions vector.
//!
//! 4. **State-local allocation** (S3b1). The pass appends one fresh
//!    `Place::Local(state_local)` to the body function's locals vector,
//!    typed as a synthetic `ResolvedTy::Named { name: "Gen$state$<owner>:<id>",
//!    … }` named type. S4 wires the LLVM struct that backs this local;
//!    S3b's contract is that every `Place::GenState` in the body addresses
//!    into this single local.
//!
//! 5. **Init-mask population** (S3b2). At every yield site, BEFORE the
//!    per-site checkpoint Moves and BEFORE the `Terminator::Yield`, the
//!    pass emits `ConstI64 mask_tmp = SITE_MASK` then `Move
//!    GenState{field: 1} = mask_tmp` where `SITE_MASK` is the bitmask
//!    over `live_locals` indices `i` of fields checkpointed at this
//!    site (bit `i` ⇔ `live_locals[i]` is in the per-site lift set).
//!    Symmetrically, at every resume entry, AFTER the per-site reload
//!    Moves, the pass emits `ConstI64 mask_tmp = 0` then `Move
//!    GenState{field: 1} = mask_tmp` because the reload Moves
//!    semantically consumed the field — the state-record slots are no
//!    longer authoritative. The discipline guarantees that
//!    `init_mask` at every suspension reflects exactly the fields that
//!    hold valid data, which is the invariant the `__drop_in_state`
//!    shim's runtime path requires.
//!
//!    Ordering choice (load-bearing). The mask-set Move precedes the
//!    checkpoint Moves at each yield so the existing S3b1 contract
//!    that the LAST instruction before `Terminator::Yield` is the
//!    checkpoint Move is preserved; the mask-clear Move follows the
//!    reload Moves at each resume so the FIRST instruction in a resume
//!    block remains the reload Move. The choice is semantically
//!    neutral (both writes complete before the suspension/resume
//!    boundary fires) and structurally pins the bookend Move
//!    discipline tests against future rewrites.
//!
//! 6. **`__drop_in_state` shim** (S3b2). The pass synthesises one MIR
//!    function per generator body, named `__hew_gen_drop_in_state_{owner}_{id}`,
//!    that the runtime invokes from the End / Cancelled / Panic / Drop
//!    exit paths to release fields held in the state record. The shim
//!    declared here is fail-closed (single Trap block) and is
//!    accompanied by a structured per-state drop manifest on
//!    [`GenStateLayout::drop_tables`] that S4 reads to regenerate the
//!    shim body as a switch-on-tag cascade with one drop block per
//!    state. See [`build_drop_shim_function`] for the placeholder
//!    layout and the rationale for the S3b2 / S4 split.
//!
//! ## Fail-closed invariants
//!
//! - If a cross-yield-live local has no entry in `body_locals`, the
//!   pass panics — that condition indicates a builder bug where a
//!   `Place::Local(N)` was constructed without a corresponding
//!   `locals[N]` slot. The panic is the correct fail-closed signal:
//!   silently dropping the field would lose user state across resume.
//!
//! - The pass refuses to lift a yield-block's terminator value that is
//!   NOT a `Place::Local(N)`. In S3a, `lower_yield_expr` always emits
//!   `Yield { value: Local(N), .. }` (the value Place is materialised
//!   through `alloc_local`), so this is a structural invariant; any
//!   future producer that breaks the shape needs to extend this pass
//!   first.
//!
//! - The number of `live_locals` is asserted to fit in 64 bits, since
//!   the init-mask is a `u64`. The panic surfaces a producer that
//!   tries to lift more than 64 locals across a single yield — a
//!   condition the dataflow analysis does not prevent today but the
//!   mask field cannot encode. If/when this fires in practice the
//!   mask must widen (e.g. to a `[u64; N]` bag); the panic is the
//!   fail-closed signal.

use std::collections::{BTreeMap, BTreeSet, HashMap};

use hew_types::ResolvedTy;

use crate::dataflow::instr_reads_writes;
use crate::model::{
    BasicBlock, FunctionCallConv, GenStateDropTable, GenStateLayout, GenStateLiveLocal, Instr,
    Place, RawMirFunction, Terminator, TrapKind,
};

/// Field ordinal of the state tag inside `GenStateLayout.fields`.
pub const STATE_TAG_FIELD: u32 = 0;
/// Field ordinal of the per-local init-mask inside `GenStateLayout.fields`.
pub const STATE_INIT_MASK_FIELD: u32 = 1;
/// Field ordinal of the first cross-yield-live local. Subsequent
/// locals occupy `LIVE_LOCAL_BASE + i` for `i` indexing
/// `GenStateLayout.live_locals`.
pub const LIVE_LOCAL_BASE: u32 = 2;

/// Maximum number of `live_locals` the init-mask `u64` can encode.
/// Each lifted local consumes one bit. Exceeding this limit is a
/// fail-closed condition (see module docs).
const MAX_LIVE_LOCALS_FOR_MASK: usize = 64;

/// Read places a Terminator depends on. Mirrors
/// `crate::dataflow::*` block-flow walks: the live-out of a block
/// must include the places its terminator reads.
fn terminator_reads(term: &Terminator) -> Vec<Place> {
    match term {
        // `MakeGenerator` reads no places — its body-fn is a static symbol, not
        // a Place, and it writes `dest`. It never appears inside a gen-body (only
        // in the enclosing function), so it is read-free here, like the other
        // no-operand terminators.
        // `Suspend` reads no places: the value channel is the explicit coro
        // frame out-pointer, not a `Place` (spike constraint — the promise is
        // null), so the suspend point itself depends on no live operand.
        Terminator::Return
        | Terminator::Trap { .. }
        | Terminator::Goto { .. }
        | Terminator::Suspend { .. }
        | Terminator::MakeGenerator { .. }
        | Terminator::MakeLambdaActor { .. } => Vec::new(),
        Terminator::Branch { cond, .. } => vec![*cond],
        Terminator::Call { args, .. } => args.clone(),
        Terminator::Yield { value, .. } => vec![*value],
        Terminator::Send { actor, value, .. }
        | Terminator::Ask { actor, value, .. }
        | Terminator::SuspendingAsk { actor, value, .. } => {
            vec![*actor, *value]
        }
        // `SuspendingRead` reads `conn` (the read source); `result_dest` is a
        // write slot bound on the resume edge, not a read.
        Terminator::SuspendingRead { conn, .. } => vec![*conn],
        // `SuspendingAccept` reads `listener` (the accept source); `result_dest`
        // is a write slot bound on the resume edge, not a read.
        Terminator::SuspendingAccept { listener, .. } => vec![*listener],
        // `SuspendingStreamNext` reads `stream` (the recv source); `result_dest`
        // is a write slot bound on the resume edge, not a read.
        Terminator::SuspendingStreamNext { stream, .. } => vec![*stream],
        // `SuspendingChannelRecv` reads `receiver` (the recv source); `result_dest`
        // is a write slot bound on the resume edge, not a read.
        Terminator::SuspendingChannelRecv { receiver, .. } => vec![*receiver],
        // `SuspendingStreamSend` reads `sink` + `value` (the send sources).
        Terminator::SuspendingStreamSend { sink, value, .. } => vec![*sink, *value],
        // The suspendable-callee driver reads the closure pair + forwarded args;
        // `result_dest` is a write slot bound on the completion edge, not a read.
        Terminator::SuspendingCallClosure { callee, args, .. } => {
            let mut r = vec![*callee];
            r.extend(args.iter().copied());
            r
        }
        // `SuspendingRemoteAsk` reads the same operands as `RemoteAsk` (`actor` +
        // `value` + `timeout_ms`); the result/reply/error dests are write slots
        // bound on the resume edge, not reads.
        Terminator::RemoteAsk {
            actor,
            value,
            timeout_ms,
            ..
        }
        | Terminator::SuspendingRemoteAsk {
            actor,
            value,
            timeout_ms,
            ..
        } => vec![*actor, *value, *timeout_ms],
        Terminator::Select { arms, .. } => arms
            .iter()
            .flat_map(|arm| match &arm.kind {
                crate::model::SelectArmKind::StreamNext { stream } => vec![*stream],
                crate::model::SelectArmKind::ActorAsk {
                    actor, args, value, ..
                } => {
                    let mut r = vec![*actor, *value];
                    r.extend(args.iter().copied());
                    r
                }
                crate::model::SelectArmKind::TaskAwait { task } => vec![*task],
                crate::model::SelectArmKind::ChannelRecv { receiver, .. } => vec![*receiver],
                crate::model::SelectArmKind::AfterTimer { duration } => vec![*duration],
            })
            .collect(),
        Terminator::Join { branches, .. } => branches
            .iter()
            .flat_map(|branch| {
                let mut r = vec![branch.actor, branch.value];
                r.extend(branch.args.iter().copied());
                r
            })
            .collect(),
    }
}

/// Extract the `u32` body-local id from a Place if it is `Place::Local`.
/// Non-Local places are skipped from the liveness analysis — the lift
/// set only ever contains body locals, never tagged-union projections.
fn place_as_local(p: Place) -> Option<u32> {
    match p {
        Place::Local(n) => Some(n),
        _ => None,
    }
}

/// Per-block local-liveness sets, computed by a backward sweep over
/// `block.instructions` and the block's terminator.
#[derive(Debug, Clone, Default)]
struct BlockLiveness {
    /// `live_in` after the backward sweep — the set of body locals
    /// that are live at the top of the block.
    live_in: BTreeSet<u32>,
    /// `live_out` after the backward sweep — the set of body locals
    /// live at the block's exit (immediately before the terminator
    /// fires).
    live_out: BTreeSet<u32>,
}

/// Successors of `block` consistent with `BasicBlock::successors`.
fn block_successors(block: &BasicBlock) -> Vec<u32> {
    block.successors()
}

/// Backward liveness fixed-point. Returns a map from block id to
/// `BlockLiveness`. Locals not present in any `live_in` / `live_out`
/// set are implicitly dead.
fn compute_block_liveness(blocks: &[BasicBlock]) -> HashMap<u32, BlockLiveness> {
    let by_id: HashMap<u32, &BasicBlock> = blocks.iter().map(|b| (b.id, b)).collect();
    let mut result: HashMap<u32, BlockLiveness> = blocks
        .iter()
        .map(|b| (b.id, BlockLiveness::default()))
        .collect();

    // Worklist of block ids whose successor live-in has changed and
    // therefore must re-run their backward sweep. Initial visit covers
    // every block; the fixpoint loop re-queues predecessors of any
    // block whose live_in expanded.
    let mut worklist: Vec<u32> = blocks.iter().map(|b| b.id).collect();

    // Predecessor map for fast re-queueing.
    let mut preds: HashMap<u32, Vec<u32>> = HashMap::new();
    for block in blocks {
        for succ in block_successors(block) {
            preds.entry(succ).or_default().push(block.id);
        }
    }

    while let Some(bid) = worklist.pop() {
        let Some(block) = by_id.get(&bid).copied() else {
            continue;
        };
        // live_out := ⋃ live_in[s] for s in successors(block)
        let mut live_out: BTreeSet<u32> = BTreeSet::new();
        for succ in block_successors(block) {
            if let Some(succ_live) = result.get(&succ) {
                live_out.extend(succ_live.live_in.iter().copied());
            }
        }
        // Plus places the terminator itself reads (those are live at
        // the block's exit).
        for p in terminator_reads(&block.terminator) {
            if let Some(n) = place_as_local(p) {
                live_out.insert(n);
            }
        }

        // Backward sweep over instructions: for each instr (in reverse),
        // remove writes from the live set then add reads.
        let mut live = live_out.clone();
        for instr in block.instructions.iter().rev() {
            let (reads, writes) = instr_reads_writes(instr);
            for p in writes {
                if let Some(n) = place_as_local(p) {
                    live.remove(&n);
                }
            }
            for p in reads {
                if let Some(n) = place_as_local(p) {
                    live.insert(n);
                }
            }
        }
        let live_in = live;

        let entry = result.get_mut(&bid).expect("block id present");
        if entry.live_in != live_in || entry.live_out != live_out {
            entry.live_in = live_in;
            entry.live_out = live_out;
            // Re-queue predecessors so their live_out picks up the
            // expanded live_in.
            if let Some(pred_list) = preds.get(&bid) {
                worklist.extend(pred_list.iter().copied());
            }
        }
    }

    result
}

/// Cross-yield liveness output. `lifted` lists every body local that is
/// live at the resume block of at least one `Terminator::Yield` site.
/// `yield_sites` enumerates each yield block id paired with its resume
/// block id and the per-site lifted set (`live_in` of the resume), in
/// source order (ascending yield-block id, which matches the order
/// `finalize_blocks` sealed the yields).
#[derive(Debug, Default, Clone)]
struct CrossYieldAnalysis {
    lifted: BTreeSet<u32>,
    /// `(yield_block_id, resume_block_id, per_site_lifted)` in
    /// ascending-yield-block-id order. The per-site lifted set is a
    /// subset of `lifted` (the union across all sites) and equals the
    /// resume block's `live_in`.
    yield_sites: Vec<(u32, u32, BTreeSet<u32>)>,
}

fn analyze_cross_yield(
    blocks: &[BasicBlock],
    liveness: &HashMap<u32, BlockLiveness>,
) -> CrossYieldAnalysis {
    let mut lifted: BTreeSet<u32> = BTreeSet::new();
    let mut yield_sites: Vec<(u32, u32, BTreeSet<u32>)> = Vec::new();

    for block in blocks {
        if let Terminator::Yield { next, .. } = &block.terminator {
            let site_lifted = liveness
                .get(next)
                .map(|l| l.live_in.clone())
                .unwrap_or_default();
            lifted.extend(site_lifted.iter().copied());
            yield_sites.push((block.id, *next, site_lifted));
        }
    }
    // Sort yield_sites by source block id so the per-yield ordering is
    // deterministic and matches the order S3a's resume-block allocation
    // produced.
    yield_sites.sort_by_key(|(bid, _, _)| *bid);

    CrossYieldAnalysis {
        lifted,
        yield_sites,
    }
}

/// Outcome of the S3b pass on one generator body.
#[derive(Debug)]
pub struct GenStateSynthesis {
    /// The rewritten body blocks, with bookend Move + init-mask
    /// set/clear instructions inserted around every yield/resume
    /// boundary.
    pub blocks: Vec<BasicBlock>,
    /// The synthesised state-record layout for this body, including
    /// per-state drop tables consumed by the shim regeneration in S4.
    pub layout: GenStateLayout,
    /// The MIR-local id of the state-record carrier allocated at the
    /// top of the body's `locals` vector. Every `Place::GenState`
    /// emitted by this pass uses this id as `local`.
    pub state_local: u32,
    /// The body's locals vector after state-local + init-mask-scratch
    /// insertion. Returned so the caller can swap it into the produced
    /// `RawMirFunction`.
    pub locals: Vec<ResolvedTy>,
    /// The synthesised `__drop_in_state` shim function. The caller is
    /// responsible for wrapping it in a `LoweredFunction` with stub
    /// checked/elaborated views and pushing it as a sibling of the
    /// body function so it surfaces in `IrPipeline.raw_mir`.
    pub drop_shim: RawMirFunction,
}

/// Run the S3b synthesis pass on one generator body.
///
/// `function_name` MUST match the eventual
/// `RawMirFunction.name` for the body (used as the `GenStateLayout`
/// key). `blocks` is the body CFG produced by
/// `body_builder.finalize_blocks(Terminator::Return)`; `body_locals`
/// is the body's `locals` vector at the same moment.
///
/// Returns `None` when the body contains no `Terminator::Yield`
/// (defensive — `lower_gen_block` only runs the pass on a sealed
/// generator body, but the no-yield branch is a structural no-op so
/// the pass should be idempotent / skip-safe).
///
/// # Panics
///
/// Panics when a cross-yield-live local has no entry in `body_locals`
/// (a builder-invariant violation — every `Place::Local(N)` in the
/// body CFG must have a corresponding `locals[N]`). Panics when the
/// body's `locals` length, the cross-yield-live set size, or the
/// yield-site count overflows `u32`. Panics when the cross-yield-live
/// set exceeds `MAX_LIVE_LOCALS_FOR_MASK` (the init-mask `u64` cannot
/// encode that many fields). All conditions indicate a producer-side
/// bug; the panic is the correct fail-closed signal so the defect
/// surfaces at MIR construction rather than silently dropping fields
/// across a suspension boundary.
#[must_use]
#[allow(
    clippy::too_many_lines,
    reason = "S3b2 synthesis is a single coherent pipeline (analysis → \
              local allocation → layout + drop_tables → bookend Move \
              emission → shim build) where each stage feeds the next \
              by structurally-coupled local-ids; splitting would force \
              an awkward struct just to pass `state_local`, \
              `mask_scratch_local`, `local_to_mask_bit`, \
              `yield_checkpoints`, and `resume_reloads` across the \
              seam. The function stays linear top-to-bottom so the \
              bookend ordering invariant pinned by \
              `cross_yield_live_locals_lifted_to_state` is auditable \
              at a glance."
)]
pub fn synthesise(
    function_name: &str,
    blocks: Vec<BasicBlock>,
    body_locals: Vec<ResolvedTy>,
) -> Option<GenStateSynthesis> {
    // Bail out if there is no yield — no state record needed.
    let any_yield = blocks
        .iter()
        .any(|b| matches!(b.terminator, Terminator::Yield { .. }));
    if !any_yield {
        return None;
    }

    // 1. Backward liveness over the body CFG.
    let liveness = compute_block_liveness(&blocks);

    // 2. Cross-yield live set + yield-site enumeration.
    let analysis = analyze_cross_yield(&blocks, &liveness);

    assert!(
        analysis.lifted.len() <= MAX_LIVE_LOCALS_FOR_MASK,
        "S3b cross-yield-live set has {} entries, but the init-mask u64 \
         encodes at most {MAX_LIVE_LOCALS_FOR_MASK}. Widen the mask field or split \
         the gen-block. Function: {function_name}",
        analysis.lifted.len()
    );

    // Synthesised state-record type name. The S4 codegen lane mints the
    // corresponding LLVM struct from this name.
    let state_ty_name = format!("Gen$state${function_name}");
    let state_ty = ResolvedTy::Named {
        name: state_ty_name,
        args: Vec::new(),
        builtin: None,
        is_opaque: false,
    };

    // 3. Allocate fresh locals at the END of the locals vector. Using
    // append-at-end keeps every existing `Place::Local(n)` id stable,
    // which is critical because the rewrites below alter Move
    // sources/dests but not local ids.
    //
    //   * mask_scratch_local — the i64 scratch used for the per-site
    //                          mask Const+Move pair. Shared across all
    //                          yield + resume sites because each use is
    //                          a fresh write (Const) followed by an
    //                          immediate read (Move into GenState) in
    //                          the same block — liveness never spans a
    //                          suspension boundary so the temp does not
    //                          itself become cross-yield-live.
    //   * state_local        — the state-record carrier (Named state-ty).
    //                          Always the LAST body local: the lowerer
    //                          and downstream tests rely on the
    //                          state-record being `body_locals.last()`
    //                          to find the resume / drop / yield
    //                          handle's record carrier.
    let mut new_locals = body_locals;
    let mask_scratch_local: u32 =
        u32::try_from(new_locals.len()).expect("mask scratch local id overflow");
    // The mask is declared `u64` in the layout docs; ConstI64 carries
    // the bit pattern in i64. The local's type is U64 so codegen sees
    // the right LLVM width when it lowers the Move into the GenState
    // field. The signedness is irrelevant — the value is consumed as
    // an opaque bag of bits.
    new_locals.push(ResolvedTy::U64);
    let state_local: u32 = u32::try_from(new_locals.len()).expect("body local id overflow");
    new_locals.push(state_ty.clone());

    // Deterministic field order: ascending body-local id.
    let live_locals_sorted: Vec<u32> = analysis.lifted.iter().copied().collect();
    let live_locals: Vec<GenStateLiveLocal> = live_locals_sorted
        .iter()
        .map(|&original_local| {
            let ty = new_locals
                .get(original_local as usize)
                .cloned()
                .unwrap_or_else(|| {
                    panic!(
                        "S3b cross-yield-live local {original_local} has no entry in body locals \
                         (len={}); builder invariant violated",
                        new_locals.len()
                    )
                });
            GenStateLiveLocal { original_local, ty }
        })
        .collect();

    // 4. Build the local-id → field-ordinal map and the live-locals-
    // index map (the bit position into init_mask). The two are 1:1
    // (bit i ⇔ field 2 + i ⇔ `live_locals[i]`).
    let local_to_field: BTreeMap<u32, u32> = live_locals_sorted
        .iter()
        .enumerate()
        .map(|(i, &local)| {
            (
                local,
                LIVE_LOCAL_BASE + u32::try_from(i).expect("field-index overflow"),
            )
        })
        .collect();
    let local_to_mask_bit: BTreeMap<u32, usize> = live_locals_sorted
        .iter()
        .enumerate()
        .map(|(i, &local)| (local, i))
        .collect();

    // Resume blocks need their reload Moves inserted at the front.
    // Build the resume-block → live-set map.
    let mut resume_reloads: HashMap<u32, Vec<(u32, u32)>> = HashMap::new();
    // Yield blocks need their checkpoint Moves inserted before the
    // terminator. Build the yield-block → (per-site pairs, SITE_MASK)
    // map. The mask uses i64 for ConstI64 compatibility; the bit
    // pattern matches the eventual u64 field.
    let mut yield_checkpoints: HashMap<u32, (Vec<(u32, u32)>, i64)> = HashMap::new();

    // Per-state drop tables, one entry per state tag in
    // `0..=yield_count + 1`. Populated below as we walk yields in
    // source order.
    let yield_count = analysis.yield_sites.len();
    let mut drop_tables: Vec<GenStateDropTable> = Vec::with_capacity(yield_count + 2);
    // State 0 = pre-first-yield. No lifted fields valid.
    drop_tables.push(GenStateDropTable {
        state_tag: 0,
        fields_in_drop_order: Vec::new(),
    });

    for (site_idx, (yield_block, resume_block, site_lifted)) in
        analysis.yield_sites.iter().enumerate()
    {
        let mut site_pairs: Vec<(u32, u32)> = site_lifted
            .iter()
            .filter_map(|local| local_to_field.get(local).map(|&field| (*local, field)))
            .collect();
        // Deterministic order: ascending local id (which is also
        // ascending field id by construction).
        site_pairs.sort_by_key(|(local, _)| *local);

        // SITE_MASK: bit i set when live_locals[i] is checkpointed
        // at this site. The mask is the u64 init-mask value to write
        // immediately before the Yield terminator.
        let mut site_mask: u64 = 0;
        for local in site_lifted {
            if let Some(&bit) = local_to_mask_bit.get(local) {
                // bit position is the live_locals index, bounded by
                // MAX_LIVE_LOCALS_FOR_MASK above; the shift is in range.
                site_mask |= 1u64 << bit;
            }
        }
        let site_mask_i64 = site_mask.cast_signed();

        yield_checkpoints.insert(*yield_block, (site_pairs.clone(), site_mask_i64));
        resume_reloads.insert(*resume_block, site_pairs.clone());

        // Per-state drop manifest entry for this suspension. State tag
        // is 1-based in source order (state 0 is pre-first-yield),
        // and `site_idx` is 0-based, so the tag for this site is
        // `site_idx + 1`.
        let mut indices: Vec<u32> = site_lifted
            .iter()
            .filter_map(|local| {
                // `bit` is bounded by MAX_LIVE_LOCALS_FOR_MASK == 64
                // above, so the conversion always succeeds.
                local_to_mask_bit
                    .get(local)
                    .map(|&i| u32::try_from(i).expect("mask bit index fits in u32"))
            })
            .collect();
        // Reverse-init drop order: highest live_locals index first
        // (LIFO mirror of the per-site checkpoint order, which sorts
        // ascending by local-id ↔ ascending bit position).
        indices.sort_unstable_by(|a, b| b.cmp(a));
        drop_tables.push(GenStateDropTable {
            state_tag: u32::try_from(site_idx + 1).expect("state-tag overflow"),
            fields_in_drop_order: indices,
        });
    }
    // Terminal Ended state. No lifted fields valid (the body returned
    // normally → all reload Moves ran and cleared the mask).
    drop_tables.push(GenStateDropTable {
        state_tag: u32::try_from(yield_count + 1).expect("state-tag overflow"),
        fields_in_drop_order: Vec::new(),
    });

    // 5. Apply the rewrites in-place.
    let mut new_blocks = blocks;
    for block in &mut new_blocks {
        // -------- Resume reload + mask-clear --------
        // Reload at resume entry: emit each `Move { dest: Local(N),
        // src: GenState { local: state_local, field } }` BEFORE the
        // first existing instruction. Then emit the mask-clear
        // ConstI64 + Move pair (writes init_mask = 0). The clear
        // ALWAYS fires at every resume — even when the per-site
        // reload set is empty — because the body may have set bits
        // at a previous yield that the current resume must drop on
        // the floor (its data has been moved out of the state record).
        if let Some(pairs) = resume_reloads.get(&block.id) {
            let mut prelude: Vec<Instr> = pairs
                .iter()
                .map(|&(local, field)| Instr::Move {
                    dest: Place::Local(local),
                    src: Place::GenState {
                        local: state_local,
                        field,
                    },
                })
                .collect();
            // Mask clear: ConstI64 mask_scratch = 0; Move
            // GenState{field:1} = mask_scratch.
            prelude.push(Instr::ConstI64 {
                dest: Place::Local(mask_scratch_local),
                value: 0,
            });
            prelude.push(Instr::Move {
                dest: Place::GenState {
                    local: state_local,
                    field: STATE_INIT_MASK_FIELD,
                },
                src: Place::Local(mask_scratch_local),
            });
            prelude.append(&mut block.instructions);
            block.instructions = prelude;
        }

        // -------- Yield mask-set + checkpoints --------
        // Checkpoint at yield exit:
        //   1. Emit ConstI64 mask_scratch = SITE_MASK.
        //   2. Emit Move GenState{field:1} = mask_scratch.
        //   3. Then emit each per-site checkpoint Move (Local→GenState).
        // The mask-set lands BEFORE the checkpoints so the test
        // contract that the LAST instruction is the checkpoint Move
        // is preserved (S3b1's `cross_yield_live_locals_lifted_to_state`
        // pins this); the two writes are semantically commutative since
        // both complete before the Yield terminator fires.
        if let Some((pairs, site_mask_i64)) = yield_checkpoints.get(&block.id) {
            block.instructions.push(Instr::ConstI64 {
                dest: Place::Local(mask_scratch_local),
                value: *site_mask_i64,
            });
            block.instructions.push(Instr::Move {
                dest: Place::GenState {
                    local: state_local,
                    field: STATE_INIT_MASK_FIELD,
                },
                src: Place::Local(mask_scratch_local),
            });
            for &(local, field) in pairs {
                block.instructions.push(Instr::Move {
                    dest: Place::GenState {
                        local: state_local,
                        field,
                    },
                    src: Place::Local(local),
                });
            }
        }
    }

    // 6. Synthesise the `__drop_in_state` shim function (placeholder
    // Trap body, real per-state drop manifest carried on the layout).
    // See the helper's doc for the S3b2 / S4 split rationale.
    let drop_shim_name = make_drop_shim_name(function_name);
    let drop_shim = build_drop_shim_function(&drop_shim_name, state_ty.clone());

    let layout = GenStateLayout {
        function_name: function_name.to_string(),
        live_locals,
        yield_count: u32::try_from(yield_count).expect("yield-count overflow"),
        drop_shim_name,
        drop_tables,
    };

    Some(GenStateSynthesis {
        blocks: new_blocks,
        layout,
        state_local,
        locals: new_locals,
        drop_shim,
    })
}

/// Mint the shim's MIR function name. The convention parallels the
/// body's `__hew_gen_body_*` name so reading `pipeline.raw_mir` makes
/// the body / shim pairing visually obvious. The body name is the
/// full `__hew_gen_body_{owner}_{id}` mint; the shim's name is
/// derived by replacing the `_body_` infix with `_drop_in_state_`.
fn make_drop_shim_name(body_function_name: &str) -> String {
    if let Some(suffix) = body_function_name.strip_prefix("__hew_gen_body_") {
        format!("__hew_gen_drop_in_state_{suffix}")
    } else {
        // Defensive fall-through: any future renaming of the body
        // convention still produces a syntactically valid, unique
        // shim name. The body / shim coupling is documented on
        // `GenStateLayout.drop_shim_name` so consumers should resolve
        // via that field rather than re-deriving the name.
        format!("__hew_gen_drop_in_state__{body_function_name}")
    }
}

/// Build the `__drop_in_state` shim as a `RawMirFunction`.
///
/// **S3b2 placeholder body, S4 regenerates.** The shim's MIR body is a
/// single fail-closed `Trap` block. The load-bearing per-state drop
/// information lives on [`GenStateLayout::drop_tables`] — S4 reads
/// that table and rewrites this function's body into the
/// switch-on-state-tag cascade with one drop block per state. The split
/// is deliberate:
///
/// - S3b2 owns: function existence, stable name, parameter shape
///   (state-record by-value), and the per-state drop manifest. These
///   are the contract S4 reads.
/// - S4 owns: the cascade-of-branches CFG that dispatches on
///   `GenState{field: 0}` (the state tag) plus the corresponding
///   `Instr::Drop` blocks. Generating it here would require a
///   well-typed tag-comparison cascade against `Place::GenState`,
///   which codegen still rejects in S3b2 — emitting it now would
///   yield dead MIR until codegen lifts the gate, and the natural
///   place for the cascade construction is alongside the LLVM
///   emission that consumes it.
///
/// The placeholder Trap fires only if the shim is invoked before S4
/// wires the entry switch; today no call site reaches it (codegen
/// rejects `Place::GenState` and the runtime has no Drop dispatch
/// table yet), so the Trap is a fail-closed backstop, not a live code
/// path.
fn build_drop_shim_function(shim_name: &str, state_ty: ResolvedTy) -> RawMirFunction {
    // The shim takes the state-record by value as its sole parameter.
    // `locals[0]` is the parameter slot per RawMirFunction's
    // params-occupy-prefix-of-locals invariant.
    let params = vec![state_ty.clone()];
    let locals = vec![state_ty];

    let entry = BasicBlock {
        id: 0,
        instructions: Vec::new(),
        terminator: Terminator::Trap {
            kind: TrapKind::MachineDispatchUnreachable,
        },
        statements: Vec::new(),
    };

    RawMirFunction {
        name: shim_name.to_string(),
        return_ty: ResolvedTy::Unit,
        call_conv: FunctionCallConv::Default,
        params,
        locals,
        blocks: vec![entry],
        decisions: Vec::new(),
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),
        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: std::collections::HashMap::new(),
    }
}
