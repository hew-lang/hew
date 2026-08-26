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
//!   adopts the source generation and closes inline before its arm's lexical
//!   `ScopeExit`.
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

use hew_mir::{
    CheckedMirFunction, DropKind, ElabDrop, ExitPath, Instr, IrPipeline, MirStatement, OwnerId,
    OwnershipEvent, Place,
};
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

/// Every `ElabDrop` across EVERY exit of the named function (used by the
/// negative controls: an escaped handle must not be dropped on ANY path).
fn all_exit_drops(p: &IrPipeline, fn_name: &str) -> Vec<ElabDrop> {
    drops_matching(p, fn_name, |exit| !matches!(exit, ExitPath::Unwind { .. }))
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

/// Resolve the one typed source-to-destination owner transfer for a binding.
fn destination_transfer(function: &CheckedMirFunction, binding_name: &str) -> (OwnerId, Place) {
    let binding = function
        .blocks
        .iter()
        .flat_map(|block| &block.statements)
        .find_map(|statement| match statement {
            MirStatement::Bind { binding, name, .. } if name == binding_name => Some(*binding),
            _ => None,
        })
        .unwrap_or_else(|| {
            panic!(
                "binding {binding_name} must be present in {}",
                function.name
            )
        });
    let transfers: Vec<(OwnerId, Place, OwnerId, Place)> = function
        .blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instruction| match instruction {
            Instr::OwnershipEvent(OwnershipEvent::Transfer {
                owner,
                from,
                to: Some(to),
                to_owner: Some(successor),
                ..
            }) if successor.binding == binding => Some((*owner, *from, *successor, *to)),
            _ => None,
        })
        .collect();
    assert_eq!(
        transfers.len(),
        1,
        "{binding_name} must adopt exactly one source generation: {transfers:?}"
    );
    let (source_owner, source_place, owner, place) = transfers[0];
    assert!(
        source_owner != owner && source_place != place,
        "{binding_name} must adopt a distinct source owner and place: {transfers:?}"
    );
    (owner, place)
}

fn assert_vec_plain_recipe(function: &CheckedMirFunction, owner: OwnerId, binding_name: &str) {
    let recipes: Vec<&DropKind> = function
        .blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instruction| match instruction {
            Instr::OwnershipEvent(OwnershipEvent::DropRecipe {
                owner: candidate,
                recipe,
            }) if *candidate == owner => Some(&recipe.kind),
            _ => None,
        })
        .collect();
    assert_eq!(
        recipes,
        vec![&DropKind::CowHeap {
            release: hew_mir::CowHeapRelease::VecPlain,
        }],
        "{binding_name} must publish one exact Vec cleanup recipe"
    );
}

/// Assert the canonical lifecycle for a scope-local destination that adopts a
/// conditionally moved `Vec`. The arm owns the only executable cleanup: the
/// source generation transfers to the named binding, which publishes one
/// `VecPlain` recipe and is retired by an inline Drop/Release before its
/// lexical `ScopeExit`. Exit-plan elaboration must not add a competing release.
fn inline_vec_destination_lifecycle(p: &IrPipeline, fn_name: &str, binding_name: &str) -> u32 {
    let function = p
        .checked_mir
        .iter()
        .find(|function| function.name == fn_name)
        .unwrap_or_else(|| panic!("function {fn_name} must be present in checked_mir"));
    let (owner, place) = destination_transfer(function, binding_name);
    assert_vec_plain_recipe(function, owner, binding_name);

    let release_blocks: Vec<u32> = function
        .blocks
        .iter()
        .filter_map(|block| {
            let releases: Vec<usize> = block
                .instructions
                .iter()
                .enumerate()
                .filter_map(|(index, instruction)| {
                    matches!(
                        instruction,
                        Instr::OwnershipEvent(OwnershipEvent::Release {
                            owner: candidate,
                            place: candidate_place,
                        }) if *candidate == owner && *candidate_place == place
                    )
                    .then_some(index)
                })
                .collect();
            if releases.is_empty() {
                return None;
            }
            assert_eq!(
                releases.len(),
                1,
                "{binding_name} must release at most once in block {}",
                block.id
            );
            let release = releases[0];
            let drops: Vec<usize> = block
                .instructions
                .iter()
                .enumerate()
                .filter_map(|(index, instruction)| {
                    matches!(
                        instruction,
                        Instr::Drop {
                            place: candidate,
                            drop_fn: Some(hew_mir::DropFnSpec::Release(symbol)),
                            ..
                        } if *candidate == place && *symbol == "hew_vec_free"
                    )
                    .then_some(index)
                })
                .collect();
            let scope_exits: Vec<usize> = block
                .instructions
                .iter()
                .enumerate()
                .filter_map(|(index, instruction)| {
                    matches!(
                        instruction,
                        Instr::OwnershipEvent(OwnershipEvent::ScopeExit { owners, .. })
                            if owners.iter().filter(|candidate| **candidate == owner).count() == 1
                    )
                    .then_some(index)
                })
                .collect();
            assert!(
                matches!((drops.as_slice(), scope_exits.as_slice()), ([drop], [scope_exit]) if *drop < release && release < *scope_exit),
                "{binding_name} must have one ordered Drop -> Release -> ScopeExit in block {}: {:#?}",
                block.id,
                block.instructions
            );
            Some(block.id)
        })
        .collect();
    assert_eq!(
        release_blocks.len(),
        1,
        "{binding_name} must release inline on exactly one arm: {release_blocks:?}"
    );

    let exit_drops = all_exit_drops(p, fn_name);
    assert!(
        exit_drops
            .iter()
            .all(|drop| drop.place != place || !is_cow_heap_free(drop, "hew_vec_free")),
        "{binding_name}'s inline release must be sole cleanup authority; got {exit_drops:?}"
    );
    release_blocks[0]
}

fn block_reaches(function: &CheckedMirFunction, from: u32, target: u32) -> bool {
    let mut pending = function
        .blocks
        .iter()
        .find(|block| block.id == from)
        .map_or_else(Vec::new, hew_mir::BasicBlock::successors);
    let mut visited = std::collections::HashSet::new();
    while let Some(block_id) = pending.pop() {
        if block_id == target {
            return true;
        }
        if !visited.insert(block_id) {
            continue;
        }
        if let Some(block) = function.blocks.iter().find(|block| block.id == block_id) {
            pending.extend(block.successors());
        }
    }
    false
}

const CONDITIONAL_MOVE: &str = r"
    fn make_vec() -> Vec<i64> {
        let v: Vec<i64> = Vec.new();
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

/// The move's destination (`ys`, bound on one arm only) keeps its own inline
/// release immediately before the arm's lexical `ScopeExit` — it is the sole
/// owner exactly where the arm executed.
#[test]
fn conditional_move_destination_keeps_arm_scope_close_drop() {
    let pipeline = pipeline_with_tc(CONDITIONAL_MOVE);
    inline_vec_destination_lifecycle(&pipeline, "probe", "ys");
}

/// An `if let` resource payload can outlive its carrier's drop admission. The
/// arm-closing edge must retain the payload's own close in that case; treating
/// projection taint as an unconditional suppression strands the resource.
#[test]
fn if_let_resource_payload_keeps_arm_scope_close_drop() {
    let pipeline = pipeline_with_tc(
        r"
        #[resource]
        type Probe { fd: i64 }

        impl Probe {
            fn close(self) {}
        }

        fn make() -> Result<Probe, string> {
            Ok(Probe { fd: 1 })
        }

        fn main() {
            if let Ok(probe) = make() {
                let fd = probe.fd;
            }
        }
        ",
    );
    assert!(
        pipeline.diagnostics.is_empty(),
        "the reduced resource binder must lower without diagnostics: {:?}",
        pipeline.diagnostics
    );
    let drops = all_exit_drops(&pipeline, "main");
    let resource_drops: Vec<_> = drops
        .iter()
        .filter(|drop| matches!(drop.kind, DropKind::Resource | DropKind::EnumInPlace))
        .collect();
    assert_eq!(
        resource_drops.len(),
        1,
        "the matched resource payload needs exactly one close authority; got {drops:?}"
    );
}

/// A payload binder destructured from a BORROWED actor-state load
/// (`match self.field { Ok(r) => r.fd, ... }`) aliases a composite the actor
/// field owns for the actor's whole lifetime. The carrier temp is a bare
/// byte-copy view with no in-function drop obligation, so its absence from an
/// edge's drop set is not withheld admission — the binder must keep its alias
/// suppression on EVERY exit. Granting it close authority released a live
/// `MonitorRef` (demonitor immediately after arming) in the distributed e2e
/// fixtures.
#[test]
fn actor_field_match_payload_keeps_no_exit_drop() {
    let pipeline = pipeline_with_tc(
        r"
        #[resource]
        type Probe { fd: i64 }

        impl Probe {
            fn close(self) {}
        }

        fn make() -> Result<Probe, string> {
            Ok(Probe { fd: 3 })
        }

        actor Holder {
            var slot: Result<Probe, string>;

            receive fn arm() -> i64 {
                slot = make();
                match slot {
                    Result.Ok(probe) => probe.fd,
                    Result.Err(_) => 0,
                }
            }
        }

        fn main() {}
        ",
    );
    assert!(
        pipeline.diagnostics.is_empty(),
        "the field-backed match binder must lower without diagnostics: {:?}",
        pipeline.diagnostics
    );
    let drops = all_exit_drops(&pipeline, "Holder__recv__arm");
    let resource_drops: Vec<_> = drops
        .iter()
        .filter(|drop| matches!(drop.kind, DropKind::Resource | DropKind::EnumInPlace))
        .collect();
    assert!(
        resource_drops.is_empty(),
        "a payload binder aliasing a live actor field owns nothing — no exit \
         may close the resource the field still holds; got {drops:?}"
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
            let v: Vec<i64> = Vec.new();
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
            let a: Vec<i64> = Vec.new();
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
            let m: HashMap<string, i64> = HashMap.new();
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
        let v: Vec<i64> = Vec.new();
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
/// inline release each on their own (mutually-exclusive) arm edges. The
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
    let y_release = inline_vec_destination_lifecycle(&pipeline, "probe", "y");
    let z_release = inline_vec_destination_lifecycle(&pipeline, "probe", "z");
    let function = pipeline
        .checked_mir
        .iter()
        .find(|function| function.name == "probe")
        .expect("probe checked MIR");
    assert!(
        y_release != z_release
            && !block_reaches(function, y_release, z_release)
            && !block_reaches(function, z_release, y_release),
        "destination releases must remain on mutually-exclusive arms: y=bb{y_release}, \
         z=bb{z_release}"
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
            let v: Vec<i64> = Vec.new();
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
    let probe = pipeline
        .raw_mir
        .iter()
        .find(|function| function.name == "probe")
        .expect("probe must be present in raw MIR");
    let guarded_sources = probe
        .blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instruction| match instruction {
            hew_mir::Instr::OwnershipEvent(hew_mir::OwnershipEvent::Guard {
                owner,
                kind: hew_mir::OwnershipGuardKind::Collection,
                ..
            }) => Some(*owner),
            _ => None,
        })
        .collect::<std::collections::HashSet<_>>();
    let rebound_destinations = probe
        .blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instruction| match instruction {
            hew_mir::Instr::OwnershipEvent(hew_mir::OwnershipEvent::Transfer {
                owner,
                to_owner: Some(successor),
                ..
            }) if guarded_sources.contains(owner) && successor.binding != owner.binding => {
                Some(*successor)
            }
            _ => None,
        })
        .collect::<std::collections::HashSet<_>>();
    assert_eq!(
        rebound_destinations.len(),
        2,
        "fixture must expose one destination owner on each arm"
    );
    let destination_guards = probe
        .blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter(|instruction| {
            matches!(
                instruction,
                hew_mir::Instr::OwnershipEvent(hew_mir::OwnershipEvent::Guard {
                    owner,
                    kind: hew_mir::OwnershipGuardKind::Collection,
                    ..
                }) if rebound_destinations.contains(owner)
            )
        })
        .count();
    assert_eq!(
        destination_guards, 0,
        "a destination must not inherit the source collection's already-consumed physical flag"
    );
}

// ---------------------------------------------------------------------------
// Mixed rebind + aggregate-ingress arms — base-parity fallback.
// ---------------------------------------------------------------------------

/// A rebind on one arm and a record-literal ingress on another: the source's
/// runtime flag suppresses its destructor on both transfer paths and lets it
/// release on the third path where it remains locally owned. The rebind
/// destination keeps its own inline release.
#[test]
fn mixed_rebind_and_record_ingress_keeps_destination_release() {
    let pipeline = pipeline_with_tc(
        r"
        fn make_vec() -> Vec<i64> {
            let v: Vec<i64> = Vec.new();
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
    let drops = return_drops(&pipeline, "probe");
    let vec_frees = frees(&drops, "hew_vec_free");
    assert_eq!(
        vec_frees.len(),
        1,
        "the conditionally transferred source keeps one Return authority; got {drops:?}"
    );
    assert!(
        vec_frees[0].guard.is_some(),
        "the source Return release must remain path-guarded; got {vec_frees:?}"
    );
    inline_vec_destination_lifecycle(&pipeline, "probe", "y");
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
            let v: Vec<i64> = Vec.new();
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

/// A vec returned on one arm belongs to the caller there, but remains locally
/// owned and must be released on the sibling path.
#[test]
fn conditional_return_drops_only_the_nonreturn_path() {
    let pipeline = pipeline_with_tc(
        r"
        fn make_vec() -> Vec<i64> {
            let v: Vec<i64> = Vec.new();
            v.push(1);
            return v;
        }

        fn probe(take: bool) -> Vec<i64> {
            let xs = make_vec();
            if take {
                return xs;
            }
            let other: Vec<i64> = Vec.new();
            other
        }

        fn main() {
            let r = probe(false);
            let _n = r.len();
        }
        ",
    );
    let drops = all_exit_drops(&pipeline, "probe");
    // `other` is returned on the not-taken arm. The sole local release targets
    // `xs` only on the sibling path where it was not returned.
    assert_eq!(
        frees(&drops, "hew_vec_free").len(),
        1,
        "a conditionally returned vec must release on the sibling path and never on the \
         successful handoff path; got {drops:?}"
    );
}
