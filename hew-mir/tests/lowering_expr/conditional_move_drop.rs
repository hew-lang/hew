//! Conditionally-moved collection locals — scope-exit drop registration and
//! path-sensitive guard pinning (#2418).
//!
//! An owned collection local moved out on only SOME control-flow paths
//! (`let xs = make(); if take { let ys = xs; }`) used to be retracted from
//! the scope-exit set path-insensitively at its consume site: no drop was
//! registered at all, and the not-moved path leaked the value at return.
//! The whole-value `Move` lowering does not null the source slot, so a naked
//! registration would double-free on the moved path instead — the fix keeps
//! the registration and gates the release on a runtime drop-flag (set at
//! each consume site), mirroring the non-idempotent `#[resource]` close
//! discipline. This suite pins the structure:
//!
//! - Admit (positive): the conditionally-moved source earns its scope-exit
//!   drop on the Return exit WITH `guard: Some(..)` — skipped at runtime on
//!   the moved path, fired on the not-moved path. The move's destination
//!   keeps its own (unguarded) release on its arm's scope-close edge.
//! - Common case unchanged: a never-consumed local's drop carries
//!   `guard: None` — byte-identical to the pre-fix plan.
//! - Unconditional move: the source is `Consumed` at the Return exit, so the
//!   per-exit state filter excludes its drop statically; only the
//!   destination releases.
//! - Escape (negative controls): a conditional by-value call argument and a
//!   conditional `return xs` keep their fail-closed exclusions — no drop of
//!   the source on any path (the callee / caller owns the release).
//!
//! The negative controls are load-bearing: per
//! `drop-allowset-from-value-flow` an allow-set test without a paired
//! exclusion would pass even if the gate admitted everything (the
//! double-free this fix must never introduce).

use hew_mir::{DropKind, ElabDrop, ExitPath, IrPipeline};
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;

/// Full pipeline with type-checking so the `Vec`/`HashMap` builtins resolve
/// their element types and the builtin discriminant flows onto the MIR
/// binding type (the class predicates dispatch on it).
fn pipeline_with_tc(source: &str) -> IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    let output = hew_hir::lower_program(
        &parsed.program,
        &tc_output,
        &hew_hir::ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    hew_mir::lower_hir_module(&output.module)
}

/// Every `ElabDrop` on the named function's `Return` exits, in plan order.
fn return_drops(p: &IrPipeline, fn_name: &str) -> Vec<ElabDrop> {
    drops_matching(p, fn_name, |exit| matches!(exit, ExitPath::Return { .. }))
}

/// Every `ElabDrop` on the named function's forward `Goto` (scope-close)
/// exits, in plan order.
fn goto_drops(p: &IrPipeline, fn_name: &str) -> Vec<ElabDrop> {
    drops_matching(p, fn_name, |exit| matches!(exit, ExitPath::Goto { .. }))
}

/// Every `ElabDrop` across EVERY exit of the named function (used by the
/// negative controls: an escaped handle must not be dropped on ANY path).
fn all_exit_drops(p: &IrPipeline, fn_name: &str) -> Vec<ElabDrop> {
    drops_matching(p, fn_name, |_| true)
}

fn drops_matching(
    p: &IrPipeline,
    fn_name: &str,
    pred: impl Fn(&ExitPath) -> bool,
) -> Vec<ElabDrop> {
    p.elaborated_mir
        .iter()
        .find(|f| f.name == fn_name)
        .unwrap_or_else(|| panic!("function {fn_name} must be present in elaborated_mir"))
        .drop_plans
        .iter()
        .filter(|(exit, _)| pred(exit))
        .flat_map(|(_, plan)| plan.drops.iter().cloned())
        .collect()
}

/// True when `drop` is a `CowHeap` release naming `symbol`.
fn is_cow_heap_free(drop: &ElabDrop, symbol: &str) -> bool {
    matches!(drop.kind, DropKind::CowHeap { release } if release.release_symbol() == symbol)
}

fn frees<'d>(drops: &'d [ElabDrop], symbol: &str) -> Vec<&'d ElabDrop> {
    drops
        .iter()
        .filter(|d| is_cow_heap_free(d, symbol))
        .collect()
}

const CONDITIONAL_MOVE: &str = r"
    fn make_vec() -> Vec<i64> {
        let v: Vec<i64> = Vec::new();
        v.push(40);
        v.push(2);
        return v;
    }

    fn probe(take: bool) {
        let xs = make_vec();
        if take {
            let ys = xs;
        }
    }

    fn main() {
        probe(false);
    }
    ";

// ---------------------------------------------------------------------------
// Admit — the conditionally-moved source earns a GUARDED Return-exit drop.
// ---------------------------------------------------------------------------

/// The issue repro: `xs` must appear on the Return exit with a runtime guard
/// (exactly one guarded `hew_vec_free`), so the not-moved path releases it
/// and the moved path skips. LESSONS: `cleanup-all-exits`,
/// `drop-allowset-from-value-flow`, `boundary-fail-closed`.
#[test]
fn conditional_move_source_gets_guarded_return_drop() {
    let pipeline = pipeline_with_tc(CONDITIONAL_MOVE);
    let drops = return_drops(&pipeline, "probe");
    let vec_frees = frees(&drops, "hew_vec_free");
    assert_eq!(
        vec_frees.len(),
        1,
        "the conditionally-moved source must earn exactly one Return-exit \
         hew_vec_free drop; got {drops:?}"
    );
    assert!(
        vec_frees[0].guard.is_some(),
        "the Return-exit drop of a conditionally-moved binding must carry a \
         path-sensitive guard (an unguarded drop double-frees on the moved \
         path — the Move lowering does not null the source slot); got \
         {vec_frees:?}"
    );
}

/// The move's destination (`ys`, bound on one arm only) keeps its own
/// UNGUARDED release on the arm's scope-close edge — it is the sole owner
/// exactly where the arm executed.
#[test]
fn conditional_move_destination_keeps_arm_scope_close_drop() {
    let pipeline = pipeline_with_tc(CONDITIONAL_MOVE);
    let drops = goto_drops(&pipeline, "probe");
    let vec_frees = frees(&drops, "hew_vec_free");
    assert_eq!(
        vec_frees.len(),
        1,
        "the move destination must keep exactly one scope-close hew_vec_free \
         on its arm's closing goto; got {drops:?}"
    );
    assert!(
        vec_frees[0].guard.is_none(),
        "the destination's arm-edge drop needs no guard — the edge only \
         exists on the path where the move executed; got {vec_frees:?}"
    );
}

// ---------------------------------------------------------------------------
// Common case unchanged — no consume, no guard.
// ---------------------------------------------------------------------------

/// A never-consumed local keeps its unguarded drop: the guard machinery must
/// not perturb the common case.
#[test]
fn unconsumed_local_drop_carries_no_guard() {
    let pipeline = pipeline_with_tc(
        r"
        fn main() -> i64 {
            let v: Vec<i64> = Vec::new();
            v.push(1);
            v.len()
        }
        ",
    );
    let drops = return_drops(&pipeline, "main");
    let vec_frees = frees(&drops, "hew_vec_free");
    assert_eq!(vec_frees.len(), 1, "plain local vec still drops: {drops:?}");
    assert!(
        vec_frees[0].guard.is_none(),
        "a never-consumed local's drop must stay unguarded (byte-identical \
         common case); got {vec_frees:?}"
    );
}

// ---------------------------------------------------------------------------
// Unconditional move — the source is statically excluded at the Return.
// ---------------------------------------------------------------------------

/// A straight-line rebind consumes the source on every path: the per-exit
/// state filter excludes its drop statically, so exactly one release (the
/// destination's) survives on the Return exit.
#[test]
fn unconditional_move_leaves_single_return_release() {
    let pipeline = pipeline_with_tc(
        r"
        fn main() -> i64 {
            let a: Vec<i64> = Vec::new();
            a.push(7);
            let b = a;
            b.len()
        }
        ",
    );
    let drops = return_drops(&pipeline, "main");
    let vec_frees = frees(&drops, "hew_vec_free");
    assert_eq!(
        vec_frees.len(),
        1,
        "an unconditionally-moved source must not add a second Return-exit \
         release alongside the destination's; got {drops:?}"
    );
}

// ---------------------------------------------------------------------------
// HashMap — the collection-handle class rides the same guard.
// ---------------------------------------------------------------------------

/// A conditionally-moved `HashMap` handle earns a guarded Return-exit
/// release through the same flag machinery.
#[test]
fn conditional_move_hashmap_gets_guarded_return_drop() {
    let pipeline = pipeline_with_tc(
        r#"
        fn probe(take: bool) {
            let m: HashMap<string, i64> = HashMap::new();
            m.insert("k", 1);
            if take {
                let n = m;
            }
        }

        fn main() {
            probe(false);
        }
        "#,
    );
    let drops = return_drops(&pipeline, "probe");
    let guarded: Vec<&ElabDrop> = drops.iter().filter(|d| d.guard.is_some()).collect();
    assert_eq!(
        guarded.len(),
        1,
        "the conditionally-moved HashMap handle must earn exactly one \
         guarded Return-exit release; got {drops:?}"
    );
}

// ---------------------------------------------------------------------------
// Escape — fail-closed exclusions unchanged (negative controls).
// ---------------------------------------------------------------------------

/// A vec conditionally consumed by a BY-VALUE CALL stays excluded (the
/// callee-ownership question is the by-value-argument frontier, not this
/// fix): no drop of the source on any path — leak, never a wrong free.
#[test]
fn conditional_by_value_call_arg_stays_excluded() {
    let pipeline = pipeline_with_tc(
        r"
        fn sink(xs: Vec<i64>) -> i64 {
            xs.len()
        }

        fn probe(take: bool) -> i64 {
            let xs = make_vec();
            if take {
                return sink(xs);
            }
            0
        }

        fn make_vec() -> Vec<i64> {
            let v: Vec<i64> = Vec::new();
            v.push(1);
            return v;
        }

        fn main() {
            probe(false);
        }
        ",
    );
    let drops = all_exit_drops(&pipeline, "probe");
    assert_eq!(
        frees(&drops, "hew_vec_free").len(),
        0,
        "a vec handed to a by-value callee must keep its fail-closed \
         exclusion on every exit; got {drops:?}"
    );
}

/// A vec RETURNED on one arm stays excluded on every path — the caller owns
/// the returned handle; the not-returned path keeps today's posture.
#[test]
fn conditional_return_stays_excluded() {
    let pipeline = pipeline_with_tc(
        r"
        fn make_vec() -> Vec<i64> {
            let v: Vec<i64> = Vec::new();
            v.push(1);
            return v;
        }

        fn probe(take: bool) -> Vec<i64> {
            let xs = make_vec();
            if take {
                return xs;
            }
            let other: Vec<i64> = Vec::new();
            other
        }

        fn main() {
            let r = probe(false);
            let _n = r.len();
        }
        ",
    );
    let drops = all_exit_drops(&pipeline, "probe");
    // `other` (the not-taken arm's fresh vec) legitimately drops nowhere in
    // `probe` either — it is returned. No hew_vec_free may target `xs`.
    assert_eq!(
        frees(&drops, "hew_vec_free").len(),
        0,
        "a conditionally-returned vec must keep its fail-closed exclusion \
         on every exit; got {drops:?}"
    );
}
