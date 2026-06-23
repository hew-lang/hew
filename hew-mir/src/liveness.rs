//! Backward liveness dataflow over MIR locals, and the MIR-stage lints built on
//! it (`dead_store` today; `clean_counter` is registered in the lint registry
//! but intentionally not yet emitted — see the decision recorded in
//! `docs/design/lint-pass.md`).
//!
//! ## Why backward, and why over the `Instr` stream
//!
//! Liveness is a backward "may" analysis: a local is *live* at a program point
//! when some path from that point reads it before overwriting it. We compute it
//! by reusing the forward move-checker's CFG plumbing — [`build_preds`],
//! [`successors`], [`compute_rpo`] — with a backward worklist and a
//! meet-over-successors confluence (`live_out(b) = ⋃ live_in(succ)`).
//!
//! The gen/kill sets come from the backend-authority `Instr` stream
//! ([`instr_reads_writes`]) plus each terminator's source operands
//! ([`terminator_source_places`]). That stream is *complete*: every
//! machine-level read of a local is an `Instr` operand or a terminator source,
//! so liveness computed over it never misses a read. We deliberately do NOT
//! consult the checker-authority `statements` stream — it is `BindingId`-space
//! and lowers *into* the `Instr` stream, so it can only carry reads the `Instr`
//! stream already has.
//!
//! ## Soundness contract (precision-first)
//!
//! `dead_store` may only fire when a written value is *provably* never read.
//! That requires the computed live set to be an **over-approximation** of true
//! liveness — it may claim more locals live than truly are, never fewer:
//!
//!   * gen (reads) is a **superset** of the true reads: every operand `Place`
//!     maps through [`place_local`] and is added, including partial (field /
//!     tag / handle) reads. Over-approximating reads is safe.
//!   * kill (writes) is a **subset** of the true kills: only a full
//!     `Place::Local` def kills its local ([`full_def_local`]). Partial writes
//!     (machine/enum fields, tags, handle sub-views) do not kill the backing
//!     local. Under-approximating kills is safe.
//!   * a terminator's *write* slots are not killed at all (only its reads gen) —
//!     a further conservative under-kill.
//!
//! Under this contract `dest ∉ live_after(store)` implies the stored value is
//! truly dead, so a fired `dead_store` is never a false positive — at the cost
//! of occasionally missing one (acceptable recall for a precision-first lint).

use std::collections::{HashMap, HashSet, VecDeque};

use hew_types::{LintId, ResolvedTy};

use crate::dataflow::{
    build_preds, compute_rpo, instr_reads_writes, reachable_from_entry, successors,
};
use crate::lower::terminator_source_places;
use crate::model::{BasicBlock, Instr, MirLint, Place, RawMirFunction, SuspendKind};

/// The backing MIR local a `Place` addresses, or `None` for the return slot
/// (which is not a local register). Exhaustive by construction: a new `Place`
/// variant forces a compile error here rather than silently escaping the
/// liveness model (fail-closed — an unmodelled access must not vanish).
fn place_local(place: Place) -> Option<u32> {
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

/// The local a write `Place` *fully* overwrites, or `None` when the write is
/// partial (a field / tag / handle sub-view) and must NOT kill the backing
/// local. Keeping the kill set a subset of the true kills preserves the
/// over-approximation the `dead_store` soundness contract depends on. The
/// wildcard is the fail-safe direction: an unmodelled write place falls through
/// to "no kill", leaving the local live (conservative), never marking it dead.
fn full_def_local(place: Place) -> Option<u32> {
    match place {
        Place::Local(n) => Some(n),
        _ => None,
    }
}

/// The pure value-class scalars: copyable, own no heap resource, and never
/// participate in `Drop`. `dead_store` only targets these, which sidesteps
/// drop-safety entirely — overwriting such a value can never strand a resource
/// a later drop would observe.
fn is_no_drop_scalar(ty: &ResolvedTy) -> bool {
    matches!(
        ty,
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
    )
}

/// Apply the backward transfer of a terminator to `live`: gen its source
/// operands. The terminator's write slots (e.g. a `Call`/`Ask` reply dest) are
/// intentionally not killed — leaving them live is a conservative under-kill
/// that only costs recall.
fn apply_terminator_backward(
    block: &BasicBlock,
    suspend: Option<&SuspendKind>,
    live: &mut HashSet<u32>,
) {
    for place in terminator_source_places(&block.terminator, suspend) {
        if let Some(n) = place_local(place) {
            live.insert(n);
        }
    }
}

/// Apply the backward transfer of a single instruction to `live`:
/// `live = (live − full_defs) ∪ reads`. Kills are applied before gens so an
/// instruction that both reads and writes the same local (`x = x + 1`) leaves
/// that local live.
fn apply_instr_backward(instr: &Instr, live: &mut HashSet<u32>) {
    let (reads, writes) = instr_reads_writes(instr);
    for place in writes {
        if let Some(n) = full_def_local(place) {
            live.remove(&n);
        }
    }
    for place in reads {
        if let Some(n) = place_local(place) {
            live.insert(n);
        }
    }
}

/// `live_out(block) = ⋃ live_in(successor)` under the current `live_in` map.
fn block_live_out(block: &BasicBlock, live_in: &HashMap<u32, HashSet<u32>>) -> HashSet<u32> {
    let mut live = HashSet::new();
    for succ in successors(block) {
        if let Some(succ_in) = live_in.get(&succ) {
            live.extend(succ_in.iter().copied());
        }
    }
    live
}

/// For each instruction index in `block`, the set of locals live immediately
/// *after* that instruction executes. `result[i]` is `live_after(i)` — exactly
/// the query `dead_store` asks of a `Move`'s destination. `live_out` is the
/// block's live-out set.
fn live_after_points(
    block: &BasicBlock,
    suspend: Option<&SuspendKind>,
    live_out: &HashSet<u32>,
) -> Vec<HashSet<u32>> {
    let n = block.instructions.len();
    let mut points: Vec<HashSet<u32>> = vec![HashSet::new(); n];
    let mut live = live_out.clone();
    // Roll the terminator back first: `live` becomes live_after(n-1).
    apply_terminator_backward(block, suspend, &mut live);
    for idx in (0..n).rev() {
        points[idx].clone_from(&live);
        apply_instr_backward(&block.instructions[idx], &mut live);
    }
    points
}

/// Backward liveness fixpoint over one function's CFG. Returns the set of live
/// locals on entry to each block, keyed by block id.
fn compute_live_in(func: &RawMirFunction) -> HashMap<u32, HashSet<u32>> {
    let blocks = &func.blocks;
    let preds = build_preds(blocks);
    let order = compute_rpo(blocks);
    let index: HashMap<u32, usize> = blocks.iter().enumerate().map(|(i, b)| (b.id, i)).collect();

    let mut live_in: HashMap<u32, HashSet<u32>> =
        blocks.iter().map(|b| (b.id, HashSet::new())).collect();

    // Backward worklist: seed in reverse RPO so successors settle before
    // predecessors on the first sweep; re-enqueue a block's predecessors
    // whenever its live-in grows. Monotone growth over a finite local set
    // guarantees termination.
    let mut worklist: VecDeque<u32> = order.iter().rev().copied().collect();
    let mut queued: HashSet<u32> = worklist.iter().copied().collect();

    while let Some(bid) = worklist.pop_front() {
        queued.remove(&bid);
        let Some(&idx) = index.get(&bid) else {
            continue;
        };
        let block = &blocks[idx];

        let mut live = block_live_out(block, &live_in);
        apply_terminator_backward(block, func.suspend_kinds.get(&bid), &mut live);
        for instr in block.instructions.iter().rev() {
            apply_instr_backward(instr, &mut live);
        }

        if live_in.get(&bid) != Some(&live) {
            live_in.insert(bid, live);
            if let Some(preds) = preds.get(&bid) {
                for &pred in preds {
                    if queued.insert(pred) {
                        worklist.push_back(pred);
                    }
                }
            }
        }
    }

    live_in
}

/// Backward-liveness result for a single function, queryable per block and per
/// program point.
#[derive(Debug, Clone)]
pub struct Liveness {
    live_in: HashMap<u32, HashSet<u32>>,
    live_out: HashMap<u32, HashSet<u32>>,
}

impl Liveness {
    /// Whether `local` is live on entry to `block_id`.
    #[must_use]
    pub fn is_live_in(&self, block_id: u32, local: u32) -> bool {
        self.live_in
            .get(&block_id)
            .is_some_and(|s| s.contains(&local))
    }

    /// Whether `local` is live on exit from `block_id` (i.e. live on entry to
    /// some successor).
    #[must_use]
    pub fn is_live_out(&self, block_id: u32, local: u32) -> bool {
        self.live_out
            .get(&block_id)
            .is_some_and(|s| s.contains(&local))
    }

    /// Whether `local` is live immediately after instruction `instr_idx` of
    /// `block_id` — the value present after that instruction is read on some
    /// later path. This is the exact query `dead_store` runs on a `Move` dest.
    /// Returns `false` for an unknown block / out-of-range index.
    #[must_use]
    pub fn live_after(
        &self,
        func: &RawMirFunction,
        block_id: u32,
        instr_idx: usize,
        local: u32,
    ) -> bool {
        let Some(block) = func.blocks.iter().find(|b| b.id == block_id) else {
            return false;
        };
        let empty = HashSet::new();
        let live_out = self.live_out.get(&block_id).unwrap_or(&empty);
        let points = live_after_points(block, func.suspend_kinds.get(&block_id), live_out);
        points.get(instr_idx).is_some_and(|s| s.contains(&local))
    }
}

/// Compute backward liveness for `func`.
#[must_use]
pub fn analyze_liveness(func: &RawMirFunction) -> Liveness {
    let live_in = compute_live_in(func);
    let live_out = func
        .blocks
        .iter()
        .map(|b| (b.id, block_live_out(b, &live_in)))
        .collect();
    Liveness { live_in, live_out }
}

/// If `Local(n)` is a legitimate `dead_store` target — a user-named,
/// non-parameter local of a no-drop scalar type — return its source name;
/// otherwise `None` (skip). Returning `None` is the conservative "not a
/// candidate" path, not error-swallowing: every guard that fails means the
/// store is out of scope for this lint.
fn dead_store_target_name(func: &RawMirFunction, n: u32, param_count: usize) -> Option<&str> {
    let idx = n as usize;
    // Skip parameter slots (`locals[0..param_count]`); reassigning a parameter
    // is deliberately out of scope for this lint.
    if idx < param_count {
        return None;
    }
    // Must be user-named — skips compiler temporaries and synthesised slots
    // (including for-range counter machinery, though liveness already keeps a
    // live counter out of the dead set via the loop back-edge).
    let name = func.local_names.get(idx)?.as_deref()?;
    if name.is_empty() {
        return None;
    }
    // Must be a no-drop scalar; anything heap-owning / aggregate / handle is
    // excluded so we never reason about a value a `Drop` could observe.
    match func.locals.get(idx) {
        Some(ty) if is_no_drop_scalar(ty) => Some(name),
        _ => None,
    }
}

/// The span to anchor the diagnostic on: the offending store instruction's own
/// span when present, else the local's declaration byte (a zero-width span).
/// `None` when neither is available — a span-less lint is dropped rather than
/// rendered at a fabricated location (fail-closed). Synthesised functions carry
/// no `instr_spans` / `local_decl_bytes`, so this naturally suppresses findings
/// in compiler-generated code.
fn dead_store_span(
    func: &RawMirFunction,
    block_id: u32,
    instr_idx: usize,
    n: u32,
) -> Option<(u32, u32)> {
    if let Ok(idx) = u32::try_from(instr_idx) {
        if let Some(&span) = func.instr_spans.get(&(block_id, idx)) {
            return Some(span);
        }
    }
    if let Some(&Some(byte)) = func.local_decl_bytes.get(n as usize) {
        return Some((byte, byte));
    }
    None
}

/// Detect `dead_store` findings in one function from its liveness result.
fn detect_dead_stores(func: &RawMirFunction, liveness: &Liveness, out: &mut Vec<MirLint>) {
    let reachable = reachable_from_entry(&func.blocks);
    let param_count = func.params.len();
    let empty = HashSet::new();

    for block in &func.blocks {
        // Dead stores inside unreachable code are noise — the unreachable block
        // is itself the defect; skip it.
        if !reachable.contains(&block.id) {
            continue;
        }
        let live_out = liveness.live_out.get(&block.id).unwrap_or(&empty);
        let points = live_after_points(block, func.suspend_kinds.get(&block.id), live_out);

        for (idx, instr) in block.instructions.iter().enumerate() {
            // Only straight-line `Move` stores. Restricting to `Move` (not, say,
            // checked arithmetic, which can trap) keeps the candidate pure: the
            // store has no side effect whose removal would change behaviour.
            let Instr::Move {
                dest: Place::Local(n),
                ..
            } = instr
            else {
                continue;
            };
            let n = *n;
            let Some(name) = dead_store_target_name(func, n, param_count) else {
                continue;
            };
            // The store is dead iff its destination is not live afterwards.
            if points[idx].contains(&n) {
                continue;
            }
            let Some(span) = dead_store_span(func, block.id, idx, n) else {
                continue;
            };
            out.push(MirLint {
                lint: LintId::DeadStore,
                span,
                message: format!(
                    "the value assigned to `{name}` is never read before it is overwritten \
                     or goes out of scope"
                ),
            });
        }
    }
}

/// Drop duplicate findings sharing a `(lint, span)`. Generic functions are
/// lowered once per monomorphisation, all sharing one source span; this reports
/// a dead store in a generic body once rather than once per instantiation.
fn dedup_by_lint_span(findings: &mut Vec<MirLint>) {
    let mut seen: HashSet<(LintId, (u32, u32))> = HashSet::new();
    findings.retain(|f| seen.insert((f.lint, f.span)));
}

/// Run the MIR-stage lint pass over every lowered function, returning the raw
/// (level-agnostic) findings. The MIR stage decides only *what* is a finding;
/// the CLI applies the user's `LintLevels` and `// hew:allow(...)` policy at
/// render time.
///
/// Surfaced through the CLI front end only — the LSP / wasm front ends stop at
/// HIR and never lower to MIR, so they never call this. Editor / web surfacing
/// is tracked in issue #2176.
#[must_use]
pub fn run_mir_lints(functions: &[RawMirFunction]) -> Vec<MirLint> {
    let mut out = Vec::new();
    for func in functions {
        // Only real source functions carry the names / spans the lints need;
        // synthesised shims (drop glue, vtable thunks, dispatch) have
        // `span == None`. Skipping them is both an optimisation and a guard.
        if func.span.is_none() {
            continue;
        }
        let liveness = analyze_liveness(func);
        detect_dead_stores(func, &liveness, &mut out);
    }
    dedup_by_lint_span(&mut out);
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn place_local_extracts_backing_local_for_every_variant() {
        assert_eq!(place_local(Place::Local(3)), Some(3));
        assert_eq!(place_local(Place::DuplexHandle(4)), Some(4));
        assert_eq!(place_local(Place::LambdaActorHandle(5)), Some(5));
        assert_eq!(place_local(Place::ActorHandle(6)), Some(6));
        assert_eq!(place_local(Place::SendHalf(7)), Some(7));
        assert_eq!(place_local(Place::RecvHalf(8)), Some(8));
        assert_eq!(place_local(Place::MachineTag(9)), Some(9));
        assert_eq!(place_local(Place::EnumTag(10)), Some(10));
        assert_eq!(
            place_local(Place::MachineVariant {
                local: 11,
                variant_idx: 0,
                field_idx: 0,
            }),
            Some(11)
        );
        assert_eq!(
            place_local(Place::EnumVariant {
                local: 12,
                variant_idx: 0,
                field_idx: 0,
            }),
            Some(12)
        );
        assert_eq!(place_local(Place::ReturnSlot), None);
    }

    #[test]
    fn full_def_local_only_kills_whole_local_writes() {
        assert_eq!(full_def_local(Place::Local(1)), Some(1));
        // Partial writes must not kill the backing local.
        assert_eq!(full_def_local(Place::MachineTag(1)), None);
        assert_eq!(
            full_def_local(Place::MachineVariant {
                local: 1,
                variant_idx: 0,
                field_idx: 0,
            }),
            None
        );
        assert_eq!(full_def_local(Place::ReturnSlot), None);
    }

    #[test]
    fn no_drop_scalar_classifies_pure_values_only() {
        assert!(is_no_drop_scalar(&ResolvedTy::I64));
        assert!(is_no_drop_scalar(&ResolvedTy::U8));
        assert!(is_no_drop_scalar(&ResolvedTy::F64));
        assert!(is_no_drop_scalar(&ResolvedTy::Bool));
        assert!(is_no_drop_scalar(&ResolvedTy::Char));
        assert!(is_no_drop_scalar(&ResolvedTy::Usize));
        // Heap-owning / aggregate / unit types are excluded.
        assert!(!is_no_drop_scalar(&ResolvedTy::String));
        assert!(!is_no_drop_scalar(&ResolvedTy::Bytes));
        assert!(!is_no_drop_scalar(&ResolvedTy::Unit));
    }
}
