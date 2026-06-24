//! Local `HashMap` / `HashSet` scope-exit drop elaboration — invariant pinning.
//!
//! Before this fix a local `let m: HashMap<K, V> = HashMap::new();` handle was
//! `ValueClass::CowValue` but had no dedicated drop class, so it fell through to
//! the no-op `CowValue` arm and LEAKED its layout-keyed backing storage on every
//! normal-return AND cancel/cooperate path. This suite pins the fix:
//!
//! - Admit (positive): a local map/set whose handle never escapes earns a
//!   `DropKind::CowHeap` scope-exit drop (`hew_hashmap_free_layout` /
//!   `hew_hashset_free_layout`) on its Return exit. The receiver-borrowing ops
//!   (`m.insert(..)`, `m.get(..)`, `s.contains(..)`, …) read the handle as
//!   arg[0] but do NOT count as escapes, so a useful map is still dropped.
//! - Cancel parity (positive): with a cooperate site (a loop back-edge) where
//!   the handle is live, the `ExitPath::Cancel` plan carries the SAME drop as the
//!   Return plan — cancellation cannot leak what normal return frees
//!   (`cleanup-all-exits`).
//! - Escape (negative controls): a handle moved into an actor's initial state
//!   (`spawn A(f: m)`) or returned to the caller (`return m`) is NOT dropped in
//!   the moving/producing function. The actor's synthesised `state_drop_fn` /
//!   the caller now solely owns the free, so emitting a second free here would
//!   be a double free. The fail-closed escape-scan excludes it
//!   (`boundary-fail-closed`, `container-ingress-ownership-is-per-container`).
//!
//! The negative controls are load-bearing: per `drop-allowset-from-value-flow`
//! an allow-set test without a paired exclusion would pass even if the gate
//! admitted everything (the double-free this fix must never introduce).

use hew_mir::{DropKind, ElabDrop, ExitPath, IrPipeline};
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;

/// Run the full pipeline with type-checking so the checker-registered
/// `HashMap` / `HashSet` builtins resolve to their inferred element types and
/// the `builtin` discriminant flows onto the MIR binding type (which both
/// `drop_kind_for` and the escape-scan dispatch on).
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

/// Every `ElabDrop` on the named function's `Cancel` exits, in plan order.
fn cancel_drops(p: &IrPipeline, fn_name: &str) -> Vec<ElabDrop> {
    drops_matching(p, fn_name, |exit| matches!(exit, ExitPath::Cancel { .. }))
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
    matches!(drop.kind, DropKind::CowHeap { drop_fn } if drop_fn == symbol)
}

fn count_free(drops: &[ElabDrop], symbol: &str) -> usize {
    drops.iter().filter(|d| is_cow_heap_free(d, symbol)).count()
}

// ---------------------------------------------------------------------------
// Admit — a local handle that never escapes earns its scope-exit free.
// ---------------------------------------------------------------------------

/// A local `HashMap<i64, i64>` used only through receiver-borrowing ops earns
/// exactly one `hew_hashmap_free_layout` `CowHeap` drop on the Return exit.
/// LESSONS: `cleanup-all-exits`, `drop-allowset-from-value-flow`.
#[test]
fn hashmap_hashset_local_drop_admits_local_map_on_return() {
    let pipeline = pipeline_with_tc(
        r"
        fn main() -> i64 {
            let m: HashMap<i64, i64> = HashMap::new();
            m.insert(1, 10);
            let _v = m.get(1);
            let _n = m.len();
            0
        }
        ",
    );
    let drops = return_drops(&pipeline, "main");
    assert_eq!(
        count_free(&drops, "hew_hashmap_free_layout"),
        1,
        "a local non-escaping HashMap must earn exactly one scope-exit \
         hew_hashmap_free_layout drop; got {drops:?}"
    );
}

/// A local `HashSet<i64>` earns exactly one `hew_hashset_free_layout` `CowHeap`
/// drop on the Return exit.
#[test]
fn hashmap_hashset_local_drop_admits_local_set_on_return() {
    let pipeline = pipeline_with_tc(
        r"
        fn main() -> i64 {
            let s: HashSet<i64> = HashSet::new();
            s.insert(1);
            let _c = s.contains(1);
            0
        }
        ",
    );
    let drops = return_drops(&pipeline, "main");
    assert_eq!(
        count_free(&drops, "hew_hashset_free_layout"),
        1,
        "a local non-escaping HashSet must earn exactly one scope-exit \
         hew_hashset_free_layout drop; got {drops:?}"
    );
}

/// Both a map and a set in the same scope each earn their own free, in LIFO
/// (reverse-binding) order: the later-bound set frees before the map.
#[test]
fn hashmap_hashset_local_drop_admits_map_and_set_in_lifo_order() {
    let pipeline = pipeline_with_tc(
        r"
        fn main() -> i64 {
            let m: HashMap<i64, i64> = HashMap::new();
            m.insert(1, 10);
            let s: HashSet<i64> = HashSet::new();
            s.insert(2);
            0
        }
        ",
    );
    let drops = return_drops(&pipeline, "main");
    let frees: Vec<&str> = drops
        .iter()
        .filter_map(|d| match d.kind {
            DropKind::CowHeap { drop_fn }
                if drop_fn == "hew_hashmap_free_layout" || drop_fn == "hew_hashset_free_layout" =>
            {
                Some(drop_fn)
            }
            _ => None,
        })
        .collect();
    assert_eq!(
        frees,
        vec!["hew_hashset_free_layout", "hew_hashmap_free_layout"],
        "LIFO: the last-bound set frees before the first-bound map; got {frees:?}"
    );
}

// ---------------------------------------------------------------------------
// Cancel parity — cancellation frees what normal return frees.
// ---------------------------------------------------------------------------

/// A local map live across a loop back-edge (a cooperate / cancellation site)
/// earns the SAME `hew_hashmap_free_layout` drop on the `Cancel` exit as on the
/// `Return` exit. The cancel path reuses `drops_for_exit`, so once the handle is
/// admitted to the LIFO it is freed identically on both — cancellation can never
/// leak what normal return frees. LESSONS: `cleanup-all-exits`.
#[test]
fn hashmap_hashset_local_drop_cancel_path_frees_live_map() {
    let pipeline = pipeline_with_tc(
        r"
        fn sink(x: i64) -> i64 { x }
        fn main() -> i64 {
            let m: HashMap<i64, i64> = HashMap::new();
            m.insert(1, 10);
            for i in 0 .. 3 {
                let _ = sink(i);
            }
            m.len()
        }
        ",
    );
    let ret = return_drops(&pipeline, "main");
    let cancel = cancel_drops(&pipeline, "main");
    assert_eq!(
        count_free(&ret, "hew_hashmap_free_layout"),
        1,
        "the live map must be freed on Return; got {ret:?}"
    );
    assert!(
        count_free(&cancel, "hew_hashmap_free_layout") >= 1,
        "the live map must ALSO be freed on the Cancel (cooperate) exit — \
         cancellation cannot leak what normal return frees; got {cancel:?}"
    );
}

// ---------------------------------------------------------------------------
// Escape — negative controls (no drop where the handle leaves this scope).
// ---------------------------------------------------------------------------

/// A map moved into an actor's initial state (`spawn Holder(counts: counts)`
/// lowers to `RecordInit` → `SpawnActor`) is owned by the actor now. The
/// spawning function must NOT free it — the actor's synthesised `state_drop_fn`
/// is the sole owner of that free, and a second free here would be a double
/// free (the W4.045 class). The fail-closed escape-scan excludes it.
/// LESSONS: `boundary-fail-closed`, `container-ingress-ownership-is-per-container`.
#[test]
fn hashmap_hashset_local_drop_excludes_spawn_escaped_map() {
    let pipeline = pipeline_with_tc(
        r"
        actor Holder {
            let counts: HashMap<i64, i64>;
            receive fn ping() -> i64 { 1 }
        }
        fn main() -> i64 {
            let counts: HashMap<i64, i64> = HashMap::new();
            counts.insert(1, 10);
            let _h = spawn Holder(counts: counts);
            0
        }
        ",
    );
    let drops = all_exit_drops(&pipeline, "main");
    assert_eq!(
        count_free(&drops, "hew_hashmap_free_layout"),
        0,
        "a map moved into actor state must NOT be freed in the spawning \
         function (the actor's state_drop_fn owns the free) — emitting one \
         here is a double free; got {drops:?}"
    );
}

/// A set moved into an actor's initial state is likewise excluded from the
/// spawning function's drops.
#[test]
fn hashmap_hashset_local_drop_excludes_spawn_escaped_set() {
    let pipeline = pipeline_with_tc(
        r"
        actor SetHolder {
            let names: HashSet<i64>;
            receive fn ping() -> i64 { 1 }
        }
        fn main() -> i64 {
            let names: HashSet<i64> = HashSet::new();
            names.insert(1);
            let _h = spawn SetHolder(names: names);
            0
        }
        ",
    );
    let drops = all_exit_drops(&pipeline, "main");
    assert_eq!(
        count_free(&drops, "hew_hashset_free_layout"),
        0,
        "a set moved into actor state must NOT be freed in the spawning \
         function; got {drops:?}"
    );
}

/// A map returned to the caller (`return m`, a `Move { dest: ReturnSlot }`) is
/// owned by the caller now; the producing function must NOT free it. The
/// escape-scan excludes the `ReturnSlot` move, so no double free on the caller's
/// value. LESSONS: `boundary-fail-closed`.
#[test]
fn hashmap_hashset_local_drop_excludes_returned_map() {
    let pipeline = pipeline_with_tc(
        r"
        fn make() -> HashMap<i64, i64> {
            let m: HashMap<i64, i64> = HashMap::new();
            m.insert(1, 10);
            return m;
        }
        fn main() -> i64 { 0 }
        ",
    );
    let drops = all_exit_drops(&pipeline, "make");
    assert_eq!(
        count_free(&drops, "hew_hashmap_free_layout"),
        0,
        "a returned map must NOT be freed in its producing function — the \
         caller owns it; got {drops:?}"
    );
}

// ---------------------------------------------------------------------------
// Escape through a WHOLE-VALUE ALIAS — `let b = a;` then `b` escapes.
//
// A rebind `let counts2 = counts;` hands the same handle to a new slot before
// the handle escapes (`spawn A(f: counts2)` / `return m2`). The escape-scan
// records the escape under the alias ROOT, and the final admission resolves
// each candidate to its root before excluding (the alias-root fix) — so neither
// the moved-from original nor the escaping alias earns a producer-side free.
// (The focused `derive_local_collection_drop_allowed_excludes_aliased_member_
// when_root_escapes` unit test pins the root-resolution directly; these end-to-
// end controls pin the observable drop plan for the reviewer's reported shapes.)
// ---------------------------------------------------------------------------

/// `let counts2 = counts; spawn Holder(counts: counts2)` — the aliased handle
/// escapes into actor state. Neither `counts` (moved-from) nor `counts2`
/// (escaped) may be freed in `main`; the actor's `state_drop_fn` owns the free.
#[test]
fn hashmap_hashset_local_drop_excludes_alias_then_spawn_escaped_map() {
    let pipeline = pipeline_with_tc(
        r"
        actor Holder {
            let counts: HashMap<i64, i64>;
            receive fn ping() -> i64 { 1 }
        }
        fn main() -> i64 {
            let counts: HashMap<i64, i64> = HashMap::new();
            counts.insert(1, 10);
            let counts2 = counts;
            let _h = spawn Holder(counts: counts2);
            0
        }
        ",
    );
    let drops = all_exit_drops(&pipeline, "main");
    assert_eq!(
        count_free(&drops, "hew_hashmap_free_layout"),
        0,
        "an aliased map (`let counts2 = counts`) moved into actor state must \
         NOT be freed in main on ANY path — the actor owns it; got {drops:?}"
    );
}

/// `let names2 = names; spawn SetHolder(names: names2)` — the set analogue.
#[test]
fn hashmap_hashset_local_drop_excludes_alias_then_spawn_escaped_set() {
    let pipeline = pipeline_with_tc(
        r"
        actor SetHolder {
            let names: HashSet<i64>;
            receive fn ping() -> i64 { 1 }
        }
        fn main() -> i64 {
            let names: HashSet<i64> = HashSet::new();
            names.insert(1);
            let names2 = names;
            let _h = spawn SetHolder(names: names2);
            0
        }
        ",
    );
    let drops = all_exit_drops(&pipeline, "main");
    assert_eq!(
        count_free(&drops, "hew_hashset_free_layout"),
        0,
        "an aliased set moved into actor state must NOT be freed in main; \
         got {drops:?}"
    );
}

/// `let m2 = m; return m2` — the aliased handle is returned. The producing
/// function must NOT free it (the caller owns it); no producer-side double free.
#[test]
fn hashmap_hashset_local_drop_excludes_alias_then_returned_map() {
    let pipeline = pipeline_with_tc(
        r"
        fn make() -> HashMap<i64, i64> {
            let m: HashMap<i64, i64> = HashMap::new();
            m.insert(1, 10);
            let m2 = m;
            return m2;
        }
        fn main() -> i64 { 0 }
        ",
    );
    let drops = all_exit_drops(&pipeline, "make");
    assert_eq!(
        count_free(&drops, "hew_hashmap_free_layout"),
        0,
        "an aliased returned map (`let m2 = m; return m2`) must NOT be freed in \
         its producing function — the caller owns it; got {drops:?}"
    );
}
