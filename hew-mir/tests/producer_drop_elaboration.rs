//! E3: MIR drop elaboration for M2 substrate types — invariant pinning.
//!
//! With the typecheck→HIR bridge, HIR `expr_types` threading,
//! `Instr::CallRuntimeAbi` + producer arms, and the consumer-side
//! drop-plan scaffolding (slice 3/3.5/3.6) all in place, drop
//! elaboration now flows end-to-end for real source-text-derived MIR.
//! A `let (a, b) = duplex_pair<int, int>(N);` registers both bindings
//! in `owned_locals` with the resolved `Duplex<int, int>` type, the
//! producer wraps both handle Places as `Place::DuplexHandle(_)`, and
//! the elaborator emits one `ElabDrop` per binding at every
//! Return-terminated block in reverse-binding order with
//! `DropKind::DuplexClose`.
//!
//! These tests pin the invariants the elaborator must continue to
//! honour as downstream slices (codegen drop dispatch, follow-on
//! `send_half`/`recv_half` consuming-method support) land:
//!
//! - `cleanup-all-exits` (P0): every Return-path carries the drop
//!   plan.  Synthetic-MIR property tests in `tests/elaborate.rs`
//!   cover the Panic/Cancel/Yield/Send exits; this file covers
//!   real-source-text Return.
//! - `lifecycle-symmetry` (P0): every owned-handle creation gets a
//!   matched drop.  No `Place::DuplexHandle` from a `duplex_pair`
//!   producer is missing from the function's drop plan.
//! - `raii-null-after-move` (P0): `a.send(N)` is non-consuming;
//!   repeated sends do not duplicate or remove drops.
//! - `exhaustive-coverage` (P0): each `Place` variant for owned
//!   handles selects a distinct `DropKind` via `drop_kind_for`;
//!   no wildcard fall-through.
//! - LIFO discipline: drops fire in reverse-binding order so
//!   dependent resources release after their dependants.
//!
//! Out of scope for this file:
//!
//! - `if cond { return 0; }` early-return: HIR currently emits
//!   `Unsupported` for `if` statements on the spine, so the
//!   elaborator only sees a single Return block.  When `if`
//!   lowering lands, an `early_return_drops` test extends this
//!   suite.
//! - `send_half` / `recv_half` consuming-method drop interaction:
//!   no producer for these exists yet; the consumer-side
//!   `validate_cross_block_split_consume` checker is covered by
//!   synthetic-MIR fixtures in `tests/elaborate.rs`.

use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{lower_hir_module, DropKind, IrPipeline, Place};
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;

/// Run the full pipeline with type-checking so that checker-registered
/// builtins (e.g. `duplex_pair`) resolve to their inferred call-result
/// types via E1.5's HIR `expr_types` threading.
fn pipeline_with_tc(source: &str) -> IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    let output = lower_program(&parsed.program, &tc_output, &ResolutionCtx);
    lower_hir_module(&output.module)
}

/// Collect every `ElabDrop` from every `(ExitPath, DropPlan)` entry of
/// the named function's elaborated MIR, preserving in-plan order.
fn all_plan_drops(p: &IrPipeline, fn_name: &str) -> Vec<hew_mir::ElabDrop> {
    p.elaborated_mir
        .iter()
        .find(|f| f.name == fn_name)
        .expect("function must be present in elaborated_mir")
        .drop_plans
        .iter()
        .flat_map(|(_, plan)| plan.drops.iter().cloned())
        .collect()
}

// ---------------------------------------------------------------------------
// duplex_pair — drop emission on Return
// ---------------------------------------------------------------------------

/// `let (a, b) = duplex_pair<int, int>(16); return 0;` emits exactly
/// two `ElabDrop` entries on the Return exit, both with
/// `DropKind::DuplexClose` and `Place::DuplexHandle(_)`.  The two
/// handles must select distinct local indices.
///
/// LESSONS: `cleanup-all-exits`, `lifecycle-symmetry`.
#[test]
fn duplex_pair_emits_two_duplex_close_drops_on_return() {
    let source = r"
        fn main() -> int {
            let (a, b) = duplex_pair<int, int>(16);
            return 0;
        }
    ";
    let pipeline = pipeline_with_tc(source);
    let drops = all_plan_drops(&pipeline, "main");

    assert_eq!(
        drops.len(),
        2,
        "two duplex handles → two drops; got {drops:?}"
    );
    for drop in &drops {
        assert_eq!(drop.kind, DropKind::DuplexClose);
        assert!(
            matches!(drop.place, Place::DuplexHandle(_)),
            "drop place must be DuplexHandle; got {:?}",
            drop.place
        );
    }
    // Distinct local indices: the two handles must not alias.
    let (Place::DuplexHandle(n0), Place::DuplexHandle(n1)) = (drops[0].place, drops[1].place)
    else {
        unreachable!("matched above")
    };
    assert_ne!(
        n0, n1,
        "the two duplex handles must occupy distinct local slots"
    );
}

/// Drops fire in reverse-binding order: the second tuple element (`b`)
/// drops before the first (`a`).
///
/// `Builder::owned_locals` is appended in declaration order; `elaborate`
/// reverses it (`iter().rev()` at `build_lifo_drops`).  The drop plan's
/// first entry must correspond to `b`'s Place index, the second to
/// `a`'s.  We recover the per-binding index from the `CallRuntimeAbi`
/// args (`args[2]` is `dh0` for `a`, `args[3]` is `dh1` for `b`).
///
/// LESSONS: LIFO discipline within a single binding site.
#[test]
fn duplex_pair_drops_fire_in_reverse_binding_order() {
    let source = r"
        fn main() -> int {
            let (a, b) = duplex_pair<int, int>(16);
            return 0;
        }
    ";
    let pipeline = pipeline_with_tc(source);

    // Find the pair call's two out-params to identify which handle
    // index belongs to a vs b.
    let raw = pipeline
        .raw_mir
        .iter()
        .find(|f| f.name == "main")
        .expect("main");
    let pair_call = raw
        .blocks
        .iter()
        .flat_map(|b| b.instructions.iter())
        .find_map(|i| match i {
            hew_mir::Instr::CallRuntimeAbi(c) if c.symbol() == "hew_duplex_pair" => Some(c),
            _ => None,
        })
        .expect("hew_duplex_pair call must be present");
    let args = pair_call.args();
    let Place::DuplexHandle(idx_a) = args[2] else {
        panic!("args[2] (dh0/a) must be DuplexHandle")
    };
    let Place::DuplexHandle(idx_b) = args[3] else {
        panic!("args[3] (dh1/b) must be DuplexHandle")
    };

    let drops = all_plan_drops(&pipeline, "main");
    assert_eq!(drops.len(), 2);
    // First drop in plan order is the LAST bound: b before a.
    assert_eq!(
        drops[0].place,
        Place::DuplexHandle(idx_b),
        "first drop must be b (last-bound); got {:?}",
        drops[0].place,
    );
    assert_eq!(
        drops[1].place,
        Place::DuplexHandle(idx_a),
        "second drop must be a (first-bound); got {:?}",
        drops[1].place,
    );
}

/// Two sequential `let (..) = duplex_pair(..)` statements emit four
/// drops on Return in full LIFO order: [d, c, b, a].
///
/// LESSONS: LIFO discipline across multiple binding sites.
#[test]
fn two_duplex_pairs_emit_four_drops_in_full_lifo_order() {
    let source = r"
        fn main() -> int {
            let (a, b) = duplex_pair<int, int>(16);
            let (c, d) = duplex_pair<int, int>(16);
            return 0;
        }
    ";
    let pipeline = pipeline_with_tc(source);
    let drops = all_plan_drops(&pipeline, "main");

    assert_eq!(drops.len(), 4, "two pairs → four drops; got {drops:?}");
    for drop in &drops {
        assert_eq!(drop.kind, DropKind::DuplexClose);
        assert!(matches!(drop.place, Place::DuplexHandle(_)));
    }

    // The four DuplexHandle indices must be pairwise distinct.
    let idxs: Vec<u32> = drops
        .iter()
        .map(|d| {
            let Place::DuplexHandle(n) = d.place else {
                unreachable!()
            };
            n
        })
        .collect();
    let mut sorted = idxs.clone();
    sorted.sort_unstable();
    sorted.dedup();
    assert_eq!(
        sorted.len(),
        4,
        "all four handle local indices must be distinct; got {idxs:?}"
    );

    // LIFO: each successive drop's index must be strictly less than the
    // previous one's, because `lower_duplex_pair` allocates the
    // handle locals via `alloc_local` (monotonic increasing) and the
    // elaborator reverses `owned_locals`.  This is a stronger pin
    // than "set membership matches" — it asserts the order.
    for window in idxs.windows(2) {
        assert!(
            window[0] > window[1],
            "drop order must be strictly decreasing in local-index \
             (LIFO over allocation order); got {idxs:?}"
        );
    }
}

// ---------------------------------------------------------------------------
// send is non-consuming (raii-null-after-move negative case)
// ---------------------------------------------------------------------------

/// `a.send(N)` does not consume `a`; the drop for `a` must still appear
/// on Return exactly once regardless of the number of sends.
///
/// LESSONS: `raii-null-after-move` — a non-consuming use does NOT mark
/// the binding as moved; the drop fires at scope exit.
#[test]
fn send_is_non_consuming_drop_fires_once_per_handle() {
    let source = r"
        fn main() -> int {
            let (a, b) = duplex_pair<int, int>(16);
            a.send(42);
            a.send(43);
            a.send(44);
            return 0;
        }
    ";
    let pipeline = pipeline_with_tc(source);
    let drops = all_plan_drops(&pipeline, "main");

    // Exactly two drops, one per Duplex handle — three sends on `a`
    // must NOT introduce or remove drops for `a`.
    assert_eq!(
        drops.len(),
        2,
        "three sends on a + one b → still two drops; got {drops:?}"
    );
    let duplex_close: Vec<_> = drops
        .iter()
        .filter(|d| d.kind == DropKind::DuplexClose)
        .collect();
    assert_eq!(duplex_close.len(), 2);
}

// ---------------------------------------------------------------------------
// LambdaActorHandle drop emission (spawn_actor / actor literal)
// ---------------------------------------------------------------------------

/// `let h = actor |msg: int| -> int { ... };` produces a
/// `Place::LambdaActorHandle(_)` slot and a Return-path drop with
/// `DropKind::LambdaActorRelease`.
///
/// Verifies the slice-3.6 producer at `lower.rs:545-552` is wired
/// through the standard `owned_locals` registration path and that the
/// elaborator selects `LambdaActorRelease` (not `DuplexClose`) for the
/// `LambdaActorHandle` Place — the actor's stop-on-last-handle-drop
/// protocol differs from the generic `Duplex` close-both-dirs.
///
/// LESSONS: `exhaustive-coverage` (no wildcard fall-through for handle
/// Place variants), `cleanup-all-exits`.
#[test]
fn actor_literal_emits_lambda_actor_release_drop_on_return() {
    let source = r"
        fn main() -> int {
            let h = actor |msg: int| -> int { return msg; };
            return 0;
        }
    ";
    let pipeline = pipeline_with_tc(source);
    let drops = all_plan_drops(&pipeline, "main");

    assert_eq!(drops.len(), 1, "one actor handle → one drop; got {drops:?}");
    assert_eq!(drops[0].kind, DropKind::LambdaActorRelease);
    assert!(
        matches!(drops[0].place, Place::LambdaActorHandle(_)),
        "actor handle drop place must be LambdaActorHandle; got {:?}",
        drops[0].place
    );
}

// ---------------------------------------------------------------------------
// Place→DropKind exhaustive coverage (boundary invariant)
// ---------------------------------------------------------------------------

/// Every owned-handle Place variant emitted by the producer maps to a
/// distinct `DropKind` via the single-source-of-truth `drop_kind_for`.
/// This pins the boundary invariant that the elaborator never falls
/// back to a generic `DropKind::Resource` for substrate handles —
/// the runtime close protocol differs per variant.
///
/// We exercise the two surfaces that have producers today
/// (`duplex_pair` → `DuplexHandle`, actor literal →
/// `LambdaActorHandle`); `SendHalf` and `RecvHalf` are covered by
/// `tests/elaborate.rs` synthetic-MIR fixtures until a producer for
/// `send_half` / `recv_half` lands.
///
/// LESSONS: `exhaustive-coverage`, `boundary-fail-closed` (Place is
/// authoritative for `DropKind` selection).
#[test]
fn substrate_handle_places_select_specialised_drop_kinds() {
    // Mixed-handle program: duplex_pair AND actor literal.  Both
    // bindings reach the same elaborator and must yield specialised
    // DropKind values, not the generic Resource.
    let source = r"
        fn main() -> int {
            let (a, b) = duplex_pair<int, int>(16);
            let h = actor |msg: int| -> int { return msg; };
            return 0;
        }
    ";
    let pipeline = pipeline_with_tc(source);
    let drops = all_plan_drops(&pipeline, "main");

    // Three handles → three drops.
    assert_eq!(drops.len(), 3, "two duplex + one actor → three drops");

    // Partition by kind.  Every drop must be one of the M2 specialised
    // kinds; none may be Resource (the pre-M2 generic close path).
    let mut duplex = 0usize;
    let mut actor = 0usize;
    for drop in &drops {
        match (drop.place, drop.kind) {
            (Place::DuplexHandle(_), DropKind::DuplexClose) => duplex += 1,
            (Place::LambdaActorHandle(_), DropKind::LambdaActorRelease) => actor += 1,
            (place, DropKind::Resource) => {
                panic!(
                    "substrate handle Place {place:?} selected generic \
                     DropKind::Resource; elaborator must select the \
                     specialised kind via drop_kind_for"
                )
            }
            (place, kind) => panic!(
                "unexpected (place, kind) pairing in drop plan: \
                 ({place:?}, {kind:?})"
            ),
        }
    }
    assert_eq!(duplex, 2, "exactly two DuplexClose drops");
    assert_eq!(actor, 1, "exactly one LambdaActorRelease drop");
}

/// Every Return-terminated block carries a non-empty `DropPlan` when
/// the function owns any substrate handle.  This is the
/// `cleanup-all-exits` invariant in its simplest form: the elaborator
/// must walk every Return terminator, not the first one only.
///
/// The exemplar function has a single Return block (until `if`
/// lowering lands), but the assertion is parameterised over every
/// Return entry in `drop_plans` so the test continues to pin the
/// invariant when additional CFG surfaces enter the spine.
///
/// LESSONS: `cleanup-all-exits`.
#[test]
fn every_return_exit_carries_drop_plan_for_owned_handles() {
    let source = r"
        fn main() -> int {
            let (a, b) = duplex_pair<int, int>(16);
            return 0;
        }
    ";
    let pipeline = pipeline_with_tc(source);
    let func = pipeline
        .elaborated_mir
        .iter()
        .find(|f| f.name == "main")
        .expect("main");

    let return_plans: Vec<_> = func
        .drop_plans
        .iter()
        .filter(|(exit, _)| matches!(exit, hew_mir::ExitPath::Return { .. }))
        .collect();
    assert!(
        !return_plans.is_empty(),
        "at least one Return exit path must be present"
    );
    for (exit, plan) in &return_plans {
        assert_eq!(
            plan.drops.len(),
            2,
            "Return exit {exit:?} must carry both Duplex handle drops; \
             got {} drops",
            plan.drops.len()
        );
    }
}

// ---------------------------------------------------------------------------
// drop_fn population (registry-driven, not hardcoded)
// ---------------------------------------------------------------------------

/// `drop_fn` is populated from the HIR `type_classes` registry, not
/// hardcoded per Place variant.  For `Duplex<int, int>` the close
/// method is `"close"`, so `drop_fn` reads `"Duplex::close"`.  This
/// pins that `build_lifo_drops` consults the registry and that
/// `seed_builtin_type_classes` seeds the substrate types with a
/// `close` method (codegen E4 will route the dispatch to
/// `hew_duplex_close`).
///
/// LESSONS: `producer-bridge-before-codegen` — the producer carries
/// the discriminator the next layer (E4 codegen) needs.
#[test]
fn duplex_drop_fn_resolves_via_registry() {
    let source = r"
        fn main() -> int {
            let (a, b) = duplex_pair<int, int>(16);
            return 0;
        }
    ";
    let pipeline = pipeline_with_tc(source);
    let drops = all_plan_drops(&pipeline, "main");

    for drop in &drops {
        assert_eq!(
            drop.drop_fn.as_deref(),
            Some("Duplex::close"),
            "duplex drop_fn must resolve to \"Duplex::close\" via the \
             type_classes registry; got {:?}",
            drop.drop_fn
        );
    }
}
