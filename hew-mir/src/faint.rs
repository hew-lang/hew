//! Faint-variable (strong-liveness) dataflow over MIR locals, and the
//! `clean_counter` MIR lint built on it.
//!
//! ## Liveness is not enough
//!
//! [`crate::liveness`] answers "is this local read again?". That is too coarse
//! for `clean_counter`, because a dead accumulator and a legitimate
//! `for i in 0..n` index are *both* "live throughout the loop, dead at the
//! exit" — the accumulator is read by its own increment, the index by its own
//! bound test and increment. Plain liveness cannot separate the populations.
//!
//! **Faint-variable analysis** (the standard dual, also called *strong
//! liveness*) asks the sharper question: does this local's **value** influence
//! an *observable*? A local is:
//!
//!   * **strongly live** at a point if some path reads it into an observable,
//!     or into another local that is itself strongly live at its own def;
//!   * **faint** if it is live but only ever feeds faint computations — a
//!     closed loop of self-reference that never escapes into anything the
//!     program can observe.
//!
//! An accumulator that is only ever `c = c + 1` is exactly a faint cycle: `c`
//! is read solely to compute the next `c`. A `for` index is *not* faint — it
//! feeds `icmp` into the header's `Branch` condition, which is observable
//! control flow.
//!
//! ## What counts as an observable (fail-closed)
//!
//! The `observable_uses` set is deliberately generous — every classification
//! doubt resolves toward "observable", which can only make the lint quieter:
//!
//!   * every terminator source operand ([`terminator_source_places`]): call /
//!     send / ask / yield / join args, and **branch conditions** (control flow
//!     is observable — including a trap branch);
//!   * every read by an instruction whose destination is not a plain
//!     `Place::Local` (a partial / aggregate / handle write, whose effect this
//!     analysis does not model);
//!   * every read by an instruction that is not on the pure-arithmetic
//!     allowlist ([`is_pure_value_instr`]) — anything that can trap, allocate,
//!     call the runtime, or touch memory is treated as a side effect, so its
//!     operands are observable;
//!   * the return slot, and every read of a parameter local.
//!
//! ## Soundness direction
//!
//! `clean_counter` is a *removal* suggestion, so it must never fire on a local
//! whose deletion changes behaviour. That requires the strongly-live set to be
//! an **over-approximation**: it may call a local strongly live when it is
//! truly faint (costing recall), never the reverse. Every fallback above adds
//! to the strongly-live set, so the direction holds by construction.
//!
//! ## Why checked arithmetic excludes integer counters
//!
//! Hew's default integer `+` lowers to [`Instr::IntArithChecked`], which writes
//! an `overflow_flag` that the very next terminator branches on, jumping to a
//! `Terminator::Trap { IntegerOverflow }`. That makes the counter's *value*
//! decide whether the program traps — genuinely, observably strongly live.
//! Empirically (issue #2178): a loop incrementing an `i64` counter from
//! `i64::MAX` traps and exits non-zero; delete the counter and the same program
//! runs to completion and exits zero. So an integer counter is **never**
//! removable, and the analysis reports it strongly live via the `overflow_flag`
//! → `Branch` → `Trap` chain with no special-casing required.
//!
//! Float accumulation (`Instr::FloatAdd` and friends) has no overflow flag and
//! no trap edge — IEEE-754 saturates to infinity — so a purely float
//! accumulator *is* soundly removable, and is the population this lint fires
//! on.

use std::collections::{HashMap, HashSet};

use hew_types::{LintId, ResolvedTy};

use crate::dataflow::{instr_reads_writes, reachable_from_entry};
use crate::liveness::Liveness;
use crate::lower::terminator_source_places;
use crate::model::{BasicBlock, Instr, MirLint, Place, RawMirFunction};

/// The backing MIR local a `Place` addresses, or `None` for the return slot.
///
/// Mirrors `liveness::place_local`. Kept exhaustive so a new `Place` variant is
/// a compile error here rather than silently escaping the faint model.
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

/// Whether `instr` is a **pure value computation**: it reads its operands,
/// writes its destination, and does nothing else — no trap edge, no allocation,
/// no runtime call, no memory effect.
///
/// This is the allowlist that decides whether an instruction's reads may be
/// *propagated* (its operands are observable only if its destination is) or
/// must be treated as **observable outright**. It is intentionally tiny and
/// explicitly enumerated rather than a `matches!` with a wildcard fallthrough:
/// the wildcard direction is "not pure ⇒ operands observable", the safe side.
///
/// LOAD-BEARING: [`Instr::IntArithChecked`] is **absent** on purpose. It writes
/// an `overflow_flag` consumed by a trap branch, so its operands genuinely
/// influence whether the program traps. Listing it here would make the lint
/// unsound — see the module docs and issue #2178.
fn is_pure_value_instr(instr: &Instr) -> bool {
    matches!(
        instr,
        Instr::Move { .. }
            | Instr::FloatAdd { .. }
            | Instr::FloatSub { .. }
            | Instr::FloatMul { .. }
            | Instr::FloatNeg { .. }
    )
}

/// Result of the faint-variable pass for one function: the set of locals that
/// are **strongly live** somewhere, i.e. whose value can reach an observable.
///
/// A local absent from this set is *faint*: it may be live (read by later
/// instructions), but every such read only ever feeds other faint values.
#[derive(Debug, Clone)]
pub struct Faintness {
    strongly_live: HashSet<u32>,
}

impl Faintness {
    /// Whether `local`'s value can influence an observable anywhere in the
    /// function.
    #[must_use]
    pub fn is_strongly_live(&self, local: u32) -> bool {
        self.strongly_live.contains(&local)
    }

    /// Whether `local` is *faint*: never able to influence an observable.
    ///
    /// The complement of [`Self::is_strongly_live`]; a faint local's defining
    /// computations are dead work.
    #[must_use]
    pub fn is_faint(&self, local: u32) -> bool {
        !self.strongly_live.contains(&local)
    }
}

/// Seed the strongly-live set with every *directly* observable read, and record
/// the propagation edges `dest → reads` contributed by pure value instructions.
///
/// Returns `(seeds, edges)` where `edges[d]` lists the locals read to compute
/// `d` through a pure instruction. Only pure instructions produce edges; every
/// other read is seeded as observable immediately.
fn collect_seeds_and_edges(
    func: &RawMirFunction,
    reachable: &HashSet<u32>,
) -> (HashSet<u32>, HashMap<u32, Vec<u32>>) {
    let mut seeds: HashSet<u32> = HashSet::new();
    let mut edges: HashMap<u32, Vec<u32>> = HashMap::new();

    // Parameters are observable at entry: the caller supplied the value, and
    // this analysis is intraprocedural, so it cannot know the value is unused
    // upstream. Conservative and cheap.
    for idx in 0..func.params.len() {
        if let Ok(n) = u32::try_from(idx) {
            seeds.insert(n);
        }
    }

    for block in &func.blocks {
        // Unreachable code cannot influence anything; skipping it is safe and
        // keeps dead blocks from seeding phantom observability.
        if !reachable.contains(&block.id) {
            continue;
        }

        // Every terminator source operand is observable: call/send/ask/yield
        // arguments escape the function, and a `Branch` condition steers
        // control flow (a trap edge included).
        for place in terminator_source_places(&block.terminator, func.suspend_kinds.get(&block.id))
        {
            if let Some(n) = place_local(place) {
                seeds.insert(n);
            }
        }

        for instr in &block.instructions {
            let (reads, writes, interior_writes) = instr_reads_writes(instr);
            let read_locals: Vec<u32> = reads.iter().filter_map(|p| place_local(*p)).collect();

            // An interior write mutates through a place whose MIR slot bytes do
            // not change (e.g. `BytesAppend` rewriting its receiver's buffer,
            // `Drop` on a variant place), so it never appears in `writes`.
            //
            // Today this is redundant: every instruction that produces interior
            // writes is impure, so `pure_single_dest` is `None` below and its
            // reads are already seeded. It is kept explicit because that is a
            // property of `is_pure_value_instr`'s current allowlist, not an
            // invariant of the IR — adding any interior-writing instruction to
            // that allowlist would otherwise let this lint call a counter dead
            // while it is still mutated through an alias. This lint tells users
            // to DELETE code, so it must fail toward silence.
            for n in interior_writes.iter().filter_map(|p| place_local(*p)) {
                seeds.insert(n);
            }

            // A pure instruction writing exactly one plain local propagates:
            // its operands matter only if its destination does. Anything else
            // — impure instruction, multiple writes, partial/handle write, or a
            // write to the return slot — makes the reads observable outright.
            let pure_single_dest = if is_pure_value_instr(instr) {
                match writes.as_slice() {
                    [Place::Local(d)] => Some(*d),
                    _ => None,
                }
            } else {
                None
            };

            match pure_single_dest {
                Some(dest) => edges.entry(dest).or_default().extend(read_locals),
                None => seeds.extend(read_locals),
            }

            // A write into the return slot makes the written value observable.
            // `instr_reads_writes` reports `Place::ReturnSlot` as a write, which
            // `place_local` maps to `None`, so the reads are caught above by the
            // `pure_single_dest == None` path. This assertion-by-construction is
            // why `[Place::Local(d)]` is matched exactly rather than loosely.
        }
    }

    (seeds, edges)
}

/// Compute the faint-variable (strong-liveness) result for `func`.
///
/// Implemented as a reachability closure over the propagation graph: start from
/// the directly-observable seeds and pull in, transitively, every local read to
/// compute a strongly-live local through a pure instruction. Termination is
/// guaranteed — the worklist only ever adds locals to a finite set, each at
/// most once.
#[must_use]
pub fn analyze_faintness(func: &RawMirFunction) -> Faintness {
    let reachable = reachable_from_entry(&func.blocks);
    let (seeds, edges) = collect_seeds_and_edges(func, &reachable);

    let mut strongly_live: HashSet<u32> = HashSet::new();
    let mut worklist: Vec<u32> = seeds.into_iter().collect();
    while let Some(n) = worklist.pop() {
        if !strongly_live.insert(n) {
            continue;
        }
        if let Some(sources) = edges.get(&n) {
            for &src in sources {
                if !strongly_live.contains(&src) {
                    worklist.push(src);
                }
            }
        }
    }

    Faintness { strongly_live }
}

/// The pure float value-classes `clean_counter` accepts as an accumulate step.
///
/// Restricted to floats because they are the only Hew arithmetic that neither
/// traps nor participates in `Drop`. Integers are excluded by the checked-arith
/// argument in the module docs; this type guard is a second, independent gate on
/// top of [`is_pure_value_instr`] so widening either one alone cannot make the
/// lint fire on trapping arithmetic.
fn is_non_trapping_scalar(ty: &ResolvedTy) -> bool {
    matches!(ty, ResolvedTy::F32 | ResolvedTy::F64)
}

/// A recovered counter/accumulator shape for `local` within one block.
///
/// The lowering of `c = c + 1.0` is *not* a single self-update instruction; it
/// is a temp + an arithmetic instruction + a writeback:
///
/// ```text
///   _11 = const.f64 1.0
///   _12 = fadd.f64 _1 _11     ← arith: reads the counter, writes a temp
///   _1  = move _12            ← writeback: temp back into the counter
/// ```
///
/// Recovering the shape means finding that `Move { dest: c, src: t }` whose `t`
/// was defined by a pure arithmetic instruction that itself read `c` — i.e. the
/// self-referential cycle, threaded through the temporary.
struct CounterShape {
    /// The instruction index of the writeback `Move` (the diagnostic anchor).
    writeback_idx: usize,
}

/// Recover a counter shape for `local` in `block`, or `None`.
///
/// Scans for the writeback `Move { dest: Local(local), src: Local(t) }`, then
/// walks *backwards* for `t`'s defining instruction and requires it to be a
/// non-trapping arithmetic op that reads `local`. Requiring the def to be found
/// in the same block, before the writeback, keeps the match purely local and
/// avoids reasoning about a temp defined on another path.
fn recover_counter_shape(block: &BasicBlock, local: u32) -> Option<CounterShape> {
    for (idx, instr) in block.instructions.iter().enumerate() {
        let Instr::Move {
            dest: Place::Local(d),
            src: Place::Local(t),
        } = instr
        else {
            continue;
        };
        if *d != local {
            continue;
        }
        let temp = *t;
        // Find the defining instruction of `temp` earlier in this block.
        for prior in block.instructions[..idx].iter().rev() {
            let (reads, writes, interior_writes) = instr_reads_writes(prior);
            // An interior write to the counter (or to the temp) means the value
            // is mutated through an alias the slot does not show. Refuse to
            // classify it as an accumulate step.
            if interior_writes
                .iter()
                .any(|w| matches!(w, Place::Local(n) if *n == temp || *n == local))
            {
                return None;
            }
            if !writes
                .iter()
                .any(|w| matches!(w, Place::Local(n) if *n == temp))
            {
                continue;
            }
            // `temp`'s def must be pure arithmetic (never `IntArithChecked`)
            // and must read the counter itself — that self-reference is what
            // makes it an accumulate step rather than a fresh assignment.
            let is_accumulate = matches!(
                prior,
                Instr::FloatAdd { .. }
                    | Instr::FloatSub { .. }
                    | Instr::FloatMul { .. }
                    | Instr::FloatNeg { .. }
            ) && reads
                .iter()
                .any(|r| matches!(r, Place::Local(n) if *n == local));
            return is_accumulate.then_some(CounterShape { writeback_idx: idx });
        }
    }
    None
}

/// Whether `local` is a user-named, non-parameter local of a non-trapping
/// scalar type — the only shape `clean_counter` will name in a diagnostic.
///
/// Mirrors `dead_store`'s target guard, including the `_`-prefix exemption for
/// the documented "intentionally discarded" idiom.
fn counter_target_name(func: &RawMirFunction, n: u32, param_count: usize) -> Option<&str> {
    let idx = n as usize;
    if idx < param_count {
        return None;
    }
    let name = func.local_names.get(idx)?.as_deref()?;
    if name.is_empty() || name.starts_with('_') {
        return None;
    }
    match func.locals.get(idx) {
        Some(ty) if is_non_trapping_scalar(ty) => Some(name),
        _ => None,
    }
}

/// The span to anchor the diagnostic on: the writeback instruction's own span
/// when present, else the local's declaration byte. `None` when neither exists,
/// which drops the finding rather than rendering it at a fabricated location.
fn counter_span(
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

/// Detect `clean_counter` findings in one function.
///
/// A finding requires **all** of:
///
///  1. a recovered counter shape (temp + pure arith reading the local + `Move`
///     writeback) — obstacle 1 of issue #2178;
///  2. the local is **faint** per [`analyze_faintness`] — its value never
///     reaches an observable, which is what separates a dead accumulator from a
///     legitimate `for` index whose value feeds the header's branch condition —
///     obstacle 2;
///  3. the accumulate step is non-trapping (float), so deleting it cannot
///     remove a trap the program would otherwise take — obstacle 3;
///  4. the local is a user-named, non-parameter, non-`_` scalar.
///
/// `dead_store` cannot double-fire on the same store: it only reports a `Move`
/// whose destination is **not live** afterwards, whereas a counter's writeback
/// is by construction live into the loop back-edge. The straight-line
/// `c = c + 1.0; c = c + 1.0;` case belongs to `dead_store` alone, and this lint
/// stays silent on it because the second writeback is the only one whose local
/// survives, and a single non-looping writeback is still reported here only when
/// faint — so the guard below additionally requires the writeback to be part of
/// a back-edge-carrying block, checked by the caller via `liveness`.
fn detect_clean_counters(
    func: &RawMirFunction,
    liveness: &Liveness,
    faint: &Faintness,
    out: &mut Vec<MirLint>,
) {
    let reachable = reachable_from_entry(&func.blocks);
    let param_count = func.params.len();

    for block in &func.blocks {
        if !reachable.contains(&block.id) {
            continue;
        }
        for n in 0..u32::try_from(func.locals.len()).unwrap_or(u32::MAX) {
            let Some(name) = counter_target_name(func, n, param_count) else {
                continue;
            };
            // Obstacle 2: the value must never reach an observable.
            if faint.is_strongly_live(n) {
                continue;
            }
            // Obstacle 1: recover the counter shape through the temp.
            let Some(shape) = recover_counter_shape(block, n) else {
                continue;
            };
            // The writeback must be live out of its block — that is what makes
            // this a *loop-carried* counter rather than a straight-line dead
            // store, and it is precisely the case `dead_store` does not report.
            if !liveness.is_live_out(block.id, n) {
                continue;
            }
            let Some(span) = counter_span(func, block.id, shape.writeback_idx, n) else {
                continue;
            };
            out.push(MirLint {
                lint: LintId::CleanCounter,
                span,
                message: format!(
                    "`{name}` is accumulated each iteration but its value is never used \
                     — the counting is dead work and `{name}` can be removed"
                ),
            });
        }
    }
}

/// Run the `clean_counter` detection for one function, given its liveness.
pub(crate) fn run_clean_counter(
    func: &RawMirFunction,
    liveness: &Liveness,
    out: &mut Vec<MirLint>,
) {
    let faint = analyze_faintness(func);
    detect_clean_counters(func, liveness, &faint, out);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn checked_int_arith_is_not_a_pure_value_instr() {
        // LOAD-BEARING: `IntArithChecked` must never be classified pure — its
        // overflow flag feeds a trap branch, so its operands are observable.
        let instr = Instr::IntArithChecked {
            op: crate::model::IntArithOp::Add,
            signed: crate::model::IntSignedness::Signed,
            dest: Place::Local(1),
            lhs: Place::Local(2),
            rhs: Place::Local(3),
            overflow_flag: Place::Local(4),
        };
        assert!(!is_pure_value_instr(&instr));
    }

    #[test]
    fn float_add_and_move_are_pure_value_instrs() {
        assert!(is_pure_value_instr(&Instr::Move {
            dest: Place::Local(1),
            src: Place::Local(2),
        }));
        assert!(is_pure_value_instr(&Instr::FloatAdd {
            dest: Place::Local(1),
            lhs: Place::Local(2),
            rhs: Place::Local(3),
            width: crate::model::FloatWidth::F64,
        }));
    }

    #[test]
    fn non_trapping_scalar_accepts_floats_only() {
        assert!(is_non_trapping_scalar(&ResolvedTy::F32));
        assert!(is_non_trapping_scalar(&ResolvedTy::F64));
        // Integers trap on overflow under Hew's default arithmetic.
        assert!(!is_non_trapping_scalar(&ResolvedTy::I64));
        assert!(!is_non_trapping_scalar(&ResolvedTy::U64));
        assert!(!is_non_trapping_scalar(&ResolvedTy::Usize));
        assert!(!is_non_trapping_scalar(&ResolvedTy::Bool));
        assert!(!is_non_trapping_scalar(&ResolvedTy::String));
    }
}
