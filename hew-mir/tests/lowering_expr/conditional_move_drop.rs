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
// Two exclusive destinations of one guarded source.
// ---------------------------------------------------------------------------

const DOUBLE_DESTINATION: &str = r"
    fn make_vec() -> Vec<i64> {
        let v: Vec<i64> = Vec::new();
        v.push(40);
        v.push(2);
        return v;
    }

    fn probe(a: bool, b: bool) {
        let xs = make_vec();
        if a {
            let y = xs;
        } else if b {
            let z = xs;
        }
    }

    fn main() {
        probe(false, false);
    }
    ";

/// `if a { let y = xs; } else if b { let z = xs; }` — the source keeps
/// exactly one GUARDED Return-exit release; the two destinations keep one
/// unguarded release each on their own (mutually-exclusive) arm edges. The
/// fan-out collapse must not conflate exclusive branch destinations with a
/// parallel fan-out.
#[test]
fn exclusive_double_destination_keeps_all_three_releases() {
    let pipeline = pipeline_with_tc(DOUBLE_DESTINATION);
    let ret = return_drops(&pipeline, "probe");
    let ret_frees = frees(&ret, "hew_vec_free");
    assert_eq!(
        ret_frees.len(),
        1,
        "the double-destination source must keep exactly one Return-exit \
         release; got {ret:?}"
    );
    assert!(
        ret_frees[0].guard.is_some(),
        "the double-destination source's Return-exit release must be \
         guarded; got {ret_frees:?}"
    );
    let gotos = goto_drops(&pipeline, "probe");
    let goto_frees = frees(&gotos, "hew_vec_free");
    assert_eq!(
        goto_frees.len(),
        2,
        "each exclusive destination keeps one release on its own arm's \
         scope-close edge; got {gotos:?}"
    );
    assert!(
        goto_frees.iter().all(|d| d.guard.is_none()),
        "destination releases need no guard — each edge exists only on the \
         path where its move executed; got {goto_frees:?}"
    );
}

/// Rebinds on BOTH arms of an if/else: every runtime path moves the source,
/// so its guarded drop is statically excluded at each arm's Return, and each
/// destination releases exactly once on its own path.
#[test]
fn both_arm_destinations_each_release_once() {
    let pipeline = pipeline_with_tc(
        r"
        fn make_vec() -> Vec<i64> {
            let v: Vec<i64> = Vec::new();
            v.push(40);
            v.push(2);
            return v;
        }

        fn probe(c: bool) -> i64 {
            let xs = make_vec();
            if c {
                let a = xs;
                return a.len();
            } else {
                let b = xs;
                return b.len() + 10;
            }
        }

        fn main() {
            let _r = probe(false);
        }
        ",
    );
    let drops = all_exit_drops(&pipeline, "probe");
    let all_frees = frees(&drops, "hew_vec_free");
    // One binding's release may appear on several exit PLANS (its own arm's
    // return plus the cancel/trap cleanups) — exactly-once is per runtime
    // path, so count distinct released PLACES.
    let unguarded_places: std::collections::HashSet<_> = all_frees
        .iter()
        .filter(|d| d.guard.is_none())
        .map(|d| d.place)
        .collect();
    assert_eq!(
        unguarded_places.len(),
        2,
        "each arm's destination must keep its own unguarded release; got {drops:?}"
    );
    assert!(
        all_frees.iter().all(|d| d.guard.is_none()),
        "no guarded release survives — the source is consumed on every \
         path, so its drop is statically excluded per-exit; got {drops:?}"
    );
}

// ---------------------------------------------------------------------------
// Mixed rebind + aggregate-ingress arms — base-parity fallback.
// ---------------------------------------------------------------------------

/// A rebind on one arm and a record-literal ingress on the other: the
/// aggregate ingress is an owning-sink escape, so the source falls back to
/// the legacy posture (excluded — its flag never engages a release), while
/// the rebind destination keeps its own release exactly as the
/// retract-at-consume compiler admitted it. Pins the base-parity fallback:
/// a flagged binding the escape scan cannot admit must not drag its move
/// destination down with it.
#[test]
fn mixed_rebind_and_record_ingress_keeps_destination_release() {
    let pipeline = pipeline_with_tc(
        r"
        fn make_vec() -> Vec<i64> {
            let v: Vec<i64> = Vec::new();
            v.push(40);
            v.push(2);
            return v;
        }

        record Holder {
            items: Vec<i64>,
        }

        fn probe(a: bool, b: bool) -> i64 {
            let xs = make_vec();
            var out: i64 = 0;
            if a {
                let y = xs;
                out = y.len();
            } else if b {
                let h = Holder { items: xs };
                out = h.items.len() + 100;
            } else {
                out = 999;
            }
            out
        }

        fn main() {
            let _r = probe(false, false);
        }
        ",
    );
    let drops = all_exit_drops(&pipeline, "probe");
    let vec_frees = frees(&drops, "hew_vec_free");
    // One binding's release may appear on several exit PLANS (arm goto plus
    // cancel cleanups) — count distinct released PLACES. Exactly one place
    // (the rebind destination) earns a bare-vec release; the escaped source
    // is excluded on every exit (its handle is the record's on the ingress
    // arm and leaks fail-closed on the others).
    let free_places: std::collections::HashSet<_> = vec_frees.iter().map(|d| d.place).collect();
    assert_eq!(
        free_places.len(),
        1,
        "only the rebind destination keeps a bare-vec release (the escaped \
         source is excluded, fail-closed); got {drops:?}"
    );
    assert!(
        vec_frees.iter().all(|d| d.guard.is_none()),
        "the destination's release is unguarded and the excluded source \
         contributes no guarded release; got {vec_frees:?}"
    );
}

// ---------------------------------------------------------------------------
// Escape and borrow-call controls.
// ---------------------------------------------------------------------------

/// A Vec passed to a helper whose parameter body is proven borrow-only remains
/// caller-owned. Both the early-return call path and the ordinary fallthrough
/// path must release it.
#[test]
fn conditional_borrowing_value_call_drops_on_both_exits() {
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
        2,
        "a Vec handed to a proven borrow-only callee must be released on both \
         return exits; got {drops:?}"
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
