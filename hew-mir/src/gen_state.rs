//! Generator state-record synthesis and cross-yield liveness pass (S3b).
//!
//! Runs after `lower::Builder::finalize_blocks` produces the CFG for a
//! `gen { … }` body. The pass computes which body locals are LIVE across
//! at least one `Terminator::Yield` site, synthesises a
//! [`GenStateLayout`] describing the resume struct that will carry them
//! across suspensions, and rewrites every yield site to checkpoint
//! (store-before-yield) and every resume entry to reload
//! (load-after-resume) those locals through `Place::GenState` field
//! projections.
//!
//! ## Scope (S3b1)
//!
//! This slice implements:
//!
//! 1. **Cross-yield live analysis.** Standard backward liveness over the
//!    body CFG, with the per-block transfer function derived from
//!    [`crate::dataflow::instr_reads_writes`] plus terminator reads. A
//!    body local is "cross-yield live" when it is live at the program
//!    point IMMEDIATELY AFTER any `Terminator::Yield` — i.e. the resume
//!    block reads the local along some path before it is rewritten. The
//!    union of those per-yield sets is the lift set.
//!
//! 2. **State-record layout synthesis.** Builds a [`GenStateLayout`]
//!    keyed by the body function's name. Field 0 is the state tag;
//!    field 1 is the init-mask; fields 2..N are the cross-yield-live
//!    locals in ascending body-local id order (deterministic and
//!    stable across rebuilds).
//!
//! 3. **Bookend Move synthesis.** For every yield block, the pass
//!    inserts one `Instr::Move { dest: GenState { local: state_local,
//!    field: 2 + i }, src: Local(N_i) }` per cross-yield-live local
//!    IMMEDIATELY BEFORE the `Terminator::Yield`. For the matching
//!    resume block, the symmetric reload
//!    `Instr::Move { dest: Local(N_i), src: GenState { … } }` lands at
//!    the TOP of the block's instructions vector.
//!
//! 4. **State-local allocation.** The pass allocates one fresh
//!    `Place::Local(state_local)` at the top of the body function's
//!    locals vector (the body has zero parameters in S3a — see
//!    `lower_gen_block`), typed as a synthetic
//!    `ResolvedTy::Named { name: "Gen$state$<owner>:<id>", … }` named
//!    type. S4 wires the LLVM struct that backs this local; S3b's
//!    contract is that every `Place::GenState` in the body addresses
//!    into this single local.
//!
//! ## Out of scope (deferred to S3b2)
//!
//! - Per-state init-mask population. S3b1 reserves field 1 of the
//!   layout and the local's type but does not emit set/test
//!   instructions for it; the field stays zero-initialised and
//!   codegen rejects any use through the `Unsupported` arm.
//!
//! - `__drop_in_state` shim synthesis. The per-state field drop
//!   tables and the synthesised shim function land in S3b2 alongside
//!   the init-mask wiring.
//!
//! - Locals that escape the body across a yield via stored state-record
//!   field reads (the dataflow already handles this case: the
//!   `GenState` destination of a bookend Move is treated as a "write"
//!   into a sink, so subsequent reads of the original local are
//!   correctly satisfied by the reload at the resume entry).
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

use std::collections::{BTreeMap, BTreeSet, HashMap};

use hew_types::ResolvedTy;

use crate::dataflow::instr_reads_writes;
use crate::model::{BasicBlock, GenStateLayout, GenStateLiveLocal, Instr, Place, Terminator};

/// Field ordinal of the state tag inside `GenStateLayout.fields`.
pub const STATE_TAG_FIELD: u32 = 0;
/// Field ordinal of the per-local init-mask inside `GenStateLayout.fields`.
/// Reserved by S3b1; populated by S3b2.
pub const STATE_INIT_MASK_FIELD: u32 = 1;
/// Field ordinal of the first cross-yield-live local. Subsequent
/// locals occupy `LIVE_LOCAL_BASE + i` for `i` indexing
/// `GenStateLayout.live_locals`.
pub const LIVE_LOCAL_BASE: u32 = 2;

/// Read places a Terminator depends on. Mirrors
/// `crate::dataflow::*` block-flow walks: the live-out of a block
/// must include the places its terminator reads.
fn terminator_reads(term: &Terminator) -> Vec<Place> {
    match term {
        Terminator::Return | Terminator::Trap { .. } | Terminator::Goto { .. } => Vec::new(),
        Terminator::Branch { cond, .. } => vec![*cond],
        Terminator::Call { args, .. } => args.clone(),
        Terminator::Yield { value, .. } => vec![*value],
        Terminator::Send { actor, value, .. } | Terminator::Ask { actor, value, .. } => {
            vec![*actor, *value]
        }
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
                crate::model::SelectArmKind::AfterTimer { duration } => vec![*duration],
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
/// block id (in `block.terminator`-source order — i.e. the order
/// yields appear in `blocks` after `finalize_blocks` sealing).
#[derive(Debug, Default, Clone)]
struct CrossYieldAnalysis {
    lifted: BTreeSet<u32>,
    yield_sites: Vec<(u32, u32)>,
}

fn analyze_cross_yield(
    blocks: &[BasicBlock],
    liveness: &HashMap<u32, BlockLiveness>,
) -> CrossYieldAnalysis {
    let mut lifted: BTreeSet<u32> = BTreeSet::new();
    let mut yield_sites: Vec<(u32, u32)> = Vec::new();

    for block in blocks {
        if let Terminator::Yield { next, .. } = &block.terminator {
            yield_sites.push((block.id, *next));
            if let Some(resume_live) = liveness.get(next) {
                lifted.extend(resume_live.live_in.iter().copied());
            }
        }
    }
    // Sort yield_sites by source block id so the per-yield ordering is
    // deterministic and matches the order S3a's resume-block allocation
    // produced.
    yield_sites.sort_by_key(|(bid, _)| *bid);

    CrossYieldAnalysis {
        lifted,
        yield_sites,
    }
}

/// Outcome of the S3b1 pass on one generator body.
#[derive(Debug)]
pub struct GenStateSynthesis {
    /// The rewritten body blocks, with bookend Move instructions
    /// inserted around every yield/resume boundary.
    pub blocks: Vec<BasicBlock>,
    /// The synthesised state-record layout for this body.
    pub layout: GenStateLayout,
    /// The MIR-local id of the state-record carrier allocated at the
    /// top of the body's `locals` vector. Every `Place::GenState`
    /// emitted by this pass uses this id as `local`.
    pub state_local: u32,
    /// The body's locals vector after state-local insertion. Returned
    /// so the caller can swap it into the produced `RawMirFunction`.
    pub locals: Vec<ResolvedTy>,
}

/// Run the S3b1 synthesis pass on one generator body.
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
/// yield-site count overflows `u32`. Both conditions indicate a
/// producer-side bug; the panic is the correct fail-closed signal so
/// the defect surfaces at MIR construction rather than silently
/// dropping fields across a suspension boundary.
#[must_use]
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

    // Synthesised state-record type name. The S4 codegen lane mints the
    // corresponding LLVM struct from this name.
    let state_ty_name = format!("Gen$state${function_name}");
    let state_ty = ResolvedTy::Named {
        name: state_ty_name,
        args: Vec::new(),
    };

    // 3. Allocate a fresh state-local at the END of the locals vector.
    // Using append-at-end keeps every existing `Place::Local(n)` id
    // stable, which is critical because we rewrite Move sources/dests
    // (not local ids) below.
    let mut new_locals = body_locals;
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

    // 4. Rewrite blocks: insert bookend Moves around every yield/resume.
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

    // Resume blocks need their reload Moves inserted at the front.
    // Build the resume-block → live-set map.
    let mut resume_reloads: HashMap<u32, Vec<(u32, u32)>> = HashMap::new();
    // Yield blocks need their checkpoint Moves inserted before the
    // terminator. Build the yield-block → live-set map.
    let mut yield_checkpoints: HashMap<u32, Vec<(u32, u32)>> = HashMap::new();
    for (yield_block, resume_block) in &analysis.yield_sites {
        // The cross-yield live-set at this specific site is the
        // resume block's `live_in`. The lift set we emit fields for is
        // the union across all sites, but the per-site bookend Moves
        // only checkpoint the locals THIS site actually needs.
        let site_lifted: BTreeSet<u32> = liveness
            .get(resume_block)
            .map(|l| l.live_in.clone())
            .unwrap_or_default();

        let mut site_pairs: Vec<(u32, u32)> = site_lifted
            .iter()
            .filter_map(|local| local_to_field.get(local).map(|&field| (*local, field)))
            .collect();
        // Deterministic order: ascending local id (which is also
        // ascending field id by construction).
        site_pairs.sort_by_key(|(local, _)| *local);

        yield_checkpoints.insert(*yield_block, site_pairs.clone());
        resume_reloads.insert(*resume_block, site_pairs);
    }

    // Apply the rewrites.
    let mut new_blocks = blocks;
    for block in &mut new_blocks {
        // Reload at resume entry: emit each `Move { dest: Local(N),
        // src: GenState { local: state_local, field } }` BEFORE the
        // first existing instruction.
        if let Some(pairs) = resume_reloads.get(&block.id) {
            // Skip empty checkpoint sets — no instructions to insert.
            if !pairs.is_empty() {
                let mut reloads: Vec<Instr> = pairs
                    .iter()
                    .map(|&(local, field)| Instr::Move {
                        dest: Place::Local(local),
                        src: Place::GenState {
                            local: state_local,
                            field,
                        },
                    })
                    .collect();
                // Prepend reloads to the block's instructions.
                reloads.append(&mut block.instructions);
                block.instructions = reloads;
            }
        }
        // Checkpoint at yield exit: emit each `Move { dest: GenState
        // { local: state_local, field }, src: Local(N) }` AFTER the
        // last existing instruction and BEFORE the terminator fires.
        if let Some(pairs) = yield_checkpoints.get(&block.id) {
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

    let layout = GenStateLayout {
        function_name: function_name.to_string(),
        live_locals,
        yield_count: u32::try_from(analysis.yield_sites.len()).expect("yield-count overflow"),
    };

    Some(GenStateSynthesis {
        blocks: new_blocks,
        layout,
        state_local,
        locals: new_locals,
    })
}
