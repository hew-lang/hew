//! Plain `Vec<T>` scope-exit drop elaboration — invariant pinning.
//!
//! Before this fix a local `Vec<T>` whose element is a `BitCopy` scalar or
//! `string` (`Vec<i64>`, `Vec<u8>`, `Vec<bool>`, `Vec<f64>`, `Vec<string>`,
//! …) had no drop class at all: a plain Vec is `ValueClass::CowValue` but
//! `cow_value_leaf_drop_symbol` only handles the leaf `string` case, so the
//! binding fell through to the no-op `CowValue` arm and LEAKED its backing
//! buffer (and, for `Vec<string>`, every element) on every normal-return AND
//! cancel/cooperate path. This suite pins the fix:
//!
//! - Admit (positive): a local plain Vec whose handle never escapes earns a
//!   `DropKind::CowHeap { "hew_vec_free" }` scope-exit drop on its Return
//!   exit. The receiver-borrowing ops (`v.push(..)`, `v.len()`, `v[i]`, …)
//!   read the handle as arg[0] but do NOT count as escapes, so a useful vec
//!   is still dropped.
//! - Hand-off dedup (positive): the array-literal desugar binds the fresh vec
//!   to a synthetic let and the user binding receives the SAME handle through
//!   whole-value `Move`s — exactly ONE free fires, on the final owner.
//! - Cancel parity (positive): with a cooperate site (a loop back-edge) where
//!   the handle is live, the `ExitPath::Cancel` plan carries the SAME drop as
//!   the Return plan — cancellation cannot leak what normal return frees
//!   (`cleanup-all-exits`).
//! - Class boundaries (non-displacement): an owned-element Vec keeps its
//!   `hew_vec_free_owned` release and never picks up the plain `hew_vec_free`
//!   — the plain class must not claim what the specialised arms own.
//! - Escape (negative controls): a handle returned to the caller, moved into
//!   an actor's initial state (`spawn A(f: v)`), or consumed by a by-value
//!   call / `for-in` is NOT dropped in the moving/producing function — the
//!   new owner's release is the only release (`boundary-fail-closed`,
//!   `container-ingress-ownership-is-per-container`).
//!
//! The negative controls are load-bearing: per `drop-allowset-from-value-flow`
//! an allow-set test without a paired exclusion would pass even if the gate
//! admitted everything (the double-free this fix must never introduce).

use hew_mir::{
    DropFnSpec, DropKind, ElabDrop, ExitPath, Instr, IrPipeline, MirStatement, OwnerId,
    OwnershipEvent, Place, Terminator,
};
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;

/// Run the full pipeline with type-checking so the checker-registered `Vec`
/// builtin resolves to its inferred element type and the `builtin`
/// discriminant flows onto the MIR binding type (which both the plain-element
/// filter and the escape-scan dispatch on).
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

/// Every `ElabDrop` on unwind edges from calls to `callee`.
fn unwind_drops(p: &IrPipeline, fn_name: &str, callee: &str) -> Vec<ElabDrop> {
    drops_matching(
        p,
        fn_name,
        |exit| matches!(exit, ExitPath::Unwind { callee: unwind_callee, .. } if unwind_callee == callee),
    )
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

fn count_free(drops: &[ElabDrop], symbol: &str) -> usize {
    drops.iter().filter(|d| is_cow_heap_free(d, symbol)).count()
}

fn count_calls(p: &IrPipeline, fn_name: &str, symbol: &str) -> usize {
    p.raw_mir
        .iter()
        .find(|f| f.name == fn_name)
        .unwrap_or_else(|| panic!("function {fn_name} must be present in raw_mir"))
        .blocks
        .iter()
        .filter(|block| {
            matches!(&block.terminator, Terminator::Call { callee, .. } if callee == symbol)
        })
        .count()
}

fn count_binds_with_prefix(p: &IrPipeline, fn_name: &str, prefix: &str) -> usize {
    p.raw_mir
        .iter()
        .find(|f| f.name == fn_name)
        .unwrap_or_else(|| panic!("function {fn_name} must be present in raw_mir"))
        .blocks
        .iter()
        .flat_map(|block| block.statements.iter())
        .filter(|statement| {
            matches!(statement, MirStatement::Bind { name, .. } if name.starts_with(prefix))
        })
        .count()
}

fn neutralized_sources(p: &IrPipeline, fn_name: &str) -> std::collections::HashSet<Place> {
    p.raw_mir
        .iter()
        .find(|function| function.name == fn_name)
        .unwrap_or_else(|| panic!("function {fn_name} must be present"))
        .blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instruction| match instruction {
            Instr::NeutralizePayloadSlot {
                place,
                transferee: Some(_),
                ..
            } => Some(*place),
            _ => None,
        })
        .collect()
}

fn live_vec_free_places(p: &IrPipeline, fn_name: &str) -> std::collections::HashSet<Place> {
    let neutralized = neutralized_sources(p, fn_name);
    return_drops(p, fn_name)
        .iter()
        .filter(|drop| is_cow_heap_free(drop, "hew_vec_free"))
        .map(|drop| drop.place)
        .filter(|place| !neutralized.contains(place))
        .collect()
}

/// The largest set of distinct owner identities carried from mutually
/// exclusive predecessors into one shared selection carrier.
fn divergent_arm_carries(p: &IrPipeline, fn_name: &str) -> Vec<(OwnerId, Place, u32)> {
    let function = p
        .raw_mir
        .iter()
        .find(|f| f.name == fn_name)
        .unwrap_or_else(|| panic!("function {fn_name} must be present in raw_mir"));
    let mut groups = std::collections::HashMap::<(Place, u32), Vec<OwnerId>>::new();
    for (owner, place, target) in function
        .blocks
        .iter()
        .flat_map(|block| block.instructions.iter())
        .filter_map(|instr| match instr {
            Instr::OwnershipEvent(OwnershipEvent::EdgeCarry {
                owner,
                place,
                target,
            }) => Some((*owner, *place, *target)),
            _ => None,
        })
    {
        let owners = groups.entry((place, target)).or_default();
        if !owners.contains(&owner) {
            owners.push(owner);
        }
    }
    groups
        .into_iter()
        .max_by_key(|(_, owners)| owners.len())
        .filter(|(_, owners)| owners.len() >= 2)
        .map_or_else(Vec::new, |((place, target), owners)| {
            owners
                .into_iter()
                .map(|owner| (owner, place, target))
                .collect()
        })
}

/// Inline plain-Vec cleanup rituals whose physical Drop is immediately paired
/// with the exact logical Release it discharges.
fn inline_plain_vec_releases(p: &IrPipeline, fn_name: &str) -> Vec<(OwnerId, Place)> {
    p.raw_mir
        .iter()
        .find(|f| f.name == fn_name)
        .unwrap_or_else(|| panic!("function {fn_name} must be present in raw_mir"))
        .blocks
        .iter()
        .flat_map(|block| block.instructions.windows(2))
        .filter_map(|pair| match pair {
            [Instr::Drop {
                place: dropped,
                drop_fn: Some(DropFnSpec::Release(symbol)),
                ..
            }, Instr::OwnershipEvent(OwnershipEvent::Release { owner, place })]
                if *symbol == "hew_vec_free" && dropped == place =>
            {
                Some((*owner, *place))
            }
            _ => None,
        })
        .collect()
}

// ---------------------------------------------------------------------------
// Admit — a local plain Vec that never escapes earns its scope-exit free.
// ---------------------------------------------------------------------------

/// A local `Vec<i64>` used only through receiver-borrowing ops earns exactly
/// one `hew_vec_free` `CowHeap` drop on the Return exit — the probe's primary
/// leaking shape. LESSONS: `cleanup-all-exits`, `drop-allowset-from-value-flow`.
#[test]
fn plain_vec_local_drop_admits_local_i64_vec_on_return() {
    let pipeline = pipeline_with_tc(
        r"
        fn main() -> i64 {
            let v: Vec<i64> = Vec.new();
            v.push(1);
            v.push(2);
            let _n = v.len();
            let _x = v[0];
            0
        }
        ",
    );
    let drops = return_drops(&pipeline, "main");
    assert_eq!(
        count_free(&drops, "hew_vec_free"),
        1,
        "a local non-escaping Vec<i64> must earn exactly one scope-exit \
         hew_vec_free drop; got {drops:?}"
    );
}

/// A local `Vec<string>` earns the same single `hew_vec_free` drop — the
/// runtime's `ElemKind::String` walk releases the elements inside that one
/// free, so no per-element drop appears in the plan.
#[test]
fn plain_vec_local_drop_admits_local_string_vec_on_return() {
    let pipeline = pipeline_with_tc(
        r#"
        fn main() -> i64 {
            let v: Vec<string> = Vec.new();
            v.push("alpha");
            v.push("beta");
            v.len()
        }
        "#,
    );
    let drops = return_drops(&pipeline, "main");
    assert_eq!(
        count_free(&drops, "hew_vec_free"),
        1,
        "a local non-escaping Vec<string> must earn exactly one scope-exit \
         hew_vec_free drop (element release lives inside the runtime walk); \
         got {drops:?}"
    );
}

/// An array-literal binding (`let xs = [1, 2, 3];`) routes the fresh handle
/// through a synthetic let plus whole-value `Move`s into the user binding —
/// the hand-off dedup must leave exactly ONE `hew_vec_free` on the final
/// owner, never two releases of the one handle.
/// LESSONS: `raii-null-after-move`.
#[test]
fn plain_vec_local_drop_array_literal_frees_exactly_once() {
    let pipeline = pipeline_with_tc(
        r"
        fn main() -> i64 {
            let xs = [1, 2, 3];
            let _n = xs.len();
            0
        }
        ",
    );
    let drops = all_exit_drops(&pipeline, "main");
    assert_eq!(
        live_vec_free_places(&pipeline, "main").len(),
        1,
        "an array-literal vec must free exactly once across all exits — the \
         synthetic desugar binding and the user binding share ONE handle; \
         neutralized {:?}; got {drops:?}",
        neutralized_sources(&pipeline, "main")
    );
}

/// A local `Vec<Point>` whose element is an all-`BitCopy` value record
/// (`type Point { x: i64, y: i64 }`) earns the same single `hew_vec_free` drop.
/// Such a Vec is constructed inline via `hew_vec_new_with_layout` (NOT the
/// owned-element descriptor) and its element owns no heap, so the buffer-only
/// `hew_vec_free` is the matching release. Pre-fix the `BitCopy` record element
/// fell into the gap between the owned-element arm (heap-owning records only)
/// and the plain arm (scalars/string only) and earned NO drop — the backing
/// buffer leaked on every exit. LESSONS: `cleanup-all-exits`,
/// `drop-allowset-from-value-flow`.
#[test]
fn plain_vec_local_drop_admits_local_bitcopy_record_vec_on_return() {
    let pipeline = pipeline_with_tc(
        r"
        type Point { x: i64, y: i64 }
        fn main() -> i64 {
            let pts: Vec<Point> = Vec.new();
            pts.push(Point { x: 10, y: 20 });
            pts.len()
        }
        ",
    );
    let drops = return_drops(&pipeline, "main");
    assert_eq!(
        count_free(&drops, "hew_vec_free"),
        1,
        "a local non-escaping Vec<Point> (BitCopy value record element) must \
         earn exactly one scope-exit hew_vec_free drop; got {drops:?}"
    );
    assert_eq!(
        count_free(&drops, "hew_vec_free_owned"),
        0,
        "a BitCopy record element owns no heap, so the owned-element descriptor \
         release must NOT fire (it would walk a descriptor the layout Vec never \
         carries); got {drops:?}"
    );
}

/// An array-repeat of a `BitCopy` value record (`[Point { .. }; 3]`) routes the
/// fresh handle through the array-repeat desugar plus whole-value `Move`s into
/// the user binding — exactly ONE `hew_vec_free` must fire on the final owner,
/// never two releases of the one handle. The array-repeat-of-records gate's own
/// MIR-level invariant. LESSONS: `raii-null-after-move`.
#[test]
fn plain_vec_local_drop_bitcopy_record_array_repeat_frees_exactly_once() {
    let pipeline = pipeline_with_tc(
        r"
        type Point { x: i64, y: i64 }
        fn main() -> i64 {
            let pts = [Point { x: 1, y: 2 }; 3];
            pts.len()
        }
        ",
    );
    let drops = all_exit_drops(&pipeline, "main");
    assert_eq!(
        live_vec_free_places(&pipeline, "main").len(),
        1,
        "an array-repeat of a BitCopy record must free exactly once across all \
         exits — the desugar binding and the user binding share ONE handle; \
         got {drops:?}"
    );
}

/// A bound owned value remains caller-owned after `push`, so it must keep the
/// copy-in clone route rather than transferring its heap into the Vec.
#[test]
fn owned_vec_push_of_bound_value_keeps_copy_in_route() {
    let pipeline = pipeline_with_tc(
        r#"
        type Header {
            name: string;
        }
        fn main() -> i64 {
            let hs: Vec<Header> = Vec.new();
            let header = Header { name: "content-type".to_upper() };
            hs.push(header);
            header.name.len() + hs.len()
        }
        "#,
    );
    assert_eq!(
        count_calls(&pipeline, "main", "hew_vec_push_owned"),
        1,
        "a bound source must be cloned into the Vec"
    );
    assert_eq!(
        count_calls(&pipeline, "main", "hew_vec_push_owned_move"),
        0,
        "a bound source must retain its independent owner"
    );
}

// ---------------------------------------------------------------------------
// Cancel parity — cancellation frees what normal return frees.
// ---------------------------------------------------------------------------

/// A local vec live across a loop back-edge (a cooperate / cancellation site)
/// earns the SAME `hew_vec_free` drop on the `Cancel` exit as on the `Return`
/// exit. LESSONS: `cleanup-all-exits`.
#[test]
fn plain_vec_local_drop_cancel_path_frees_live_vec() {
    let pipeline = pipeline_with_tc(
        r"
        fn sink(x: i64) -> i64 { x }
        fn main() -> i64 {
            let v: Vec<i64> = Vec.new();
            v.push(7);
            for i in 0 .. 3 {
                let _ = sink(i);
            }
            v.len()
        }
        ",
    );
    let ret = return_drops(&pipeline, "main");
    let cancel = cancel_drops(&pipeline, "main");
    assert_eq!(
        count_free(&ret, "hew_vec_free"),
        1,
        "the live vec must be freed on Return; got {ret:?}"
    );
    assert!(
        count_free(&cancel, "hew_vec_free") >= 1,
        "the live vec must ALSO be freed on the Cancel (cooperate) exit — \
         cancellation cannot leak what normal return frees; got {cancel:?}"
    );
}

// ---------------------------------------------------------------------------
// Class boundaries — the plain arm must not displace the specialised arms.
// ---------------------------------------------------------------------------

/// An owned-element Vec (element owns heap through a record field) keeps its
/// `hew_vec_free_owned` release and never carries the plain `hew_vec_free` —
/// the plain class's default-deny element filter excludes named elements, so
/// the two arms cannot fight over one binding.
#[test]
fn plain_vec_local_drop_does_not_displace_owned_element_vec() {
    let pipeline = pipeline_with_tc(
        r#"
        type Header {
            name: string;
        }
        fn main() -> i64 {
            let hs: Vec<Header> = Vec.new();
            hs.push(Header { name: "content-type" });
            hs.len()
        }
        "#,
    );
    let drops = return_drops(&pipeline, "main");
    assert_eq!(
        count_free(&drops, "hew_vec_free_owned"),
        1,
        "an owned-element Vec must keep its hew_vec_free_owned release; \
         got {drops:?}"
    );
    assert_eq!(
        count_free(&drops, "hew_vec_free"),
        0,
        "the plain hew_vec_free must never fire on an owned-element Vec \
         (it would skip the per-element descriptor drops); got {drops:?}"
    );
}

/// A direct generic helper that only borrows its `Vec<T>` parameter through
/// `Vec::get` must not suppress the caller's owned-element Vec release. The
/// returned `Option<Header>` owns a fresh clone; the caller still owns and drops
/// the original Vec.
#[test]
fn owned_vec_get_through_borrowing_helper_keeps_caller_drop() {
    let pipeline = pipeline_with_tc(
        r#"
        type Header {
            name: string;
        }
        fn first<T>(xs: Vec<T>) -> Option<T> {
            xs.get(0)
        }
        fn main() -> i64 {
            let hs: Vec<Header> = Vec.new();
            hs.push(Header { name: "content-type" });
            match first(hs) {
                .Some(header) => header.name.len(),
                .None => 0,
            }
        }
        "#,
    );
    assert_eq!(
        count_calls(&pipeline, "main", "hew_vec_push_owned_move"),
        1,
        "the fresh Header literal must move into the Vec instead of leaking the \
         anonymous source through clone-in"
    );
    let drops = return_drops(&pipeline, "main");
    assert_eq!(
        count_free(&drops, "hew_vec_free_owned"),
        1,
        "a borrow-only generic helper must leave the caller's owned Vec admitted \
         for exactly one hew_vec_free_owned; got {drops:?}"
    );
}

/// Trapping `v[i]` over an owned element must use the same fresh-owner clone
/// choke as `Vec::get`; borrowing the live slot would let the indexed result
/// release heap still owned by the Vec.
#[test]
fn owned_vec_index_uses_fresh_clone_choke() {
    let pipeline = pipeline_with_tc(
        r#"
        type Header {
            name: string;
        }
        fn main() -> i64 {
            let hs: Vec<Header> = Vec.new();
            hs.push(Header { name: "content-type".to_upper() });
            let first = hs[0];
            first.name.len() + hs[0].name.len()
        }
        "#,
    );
    assert_eq!(
        count_calls(&pipeline, "main", "hew_vec_get_clone"),
        2,
        "each owned-element scalar index must materialise a fresh owner through \
         hew_vec_get_clone"
    );
}

// ---------------------------------------------------------------------------
// Escape — negative controls (no drop where the handle leaves this scope).
// ---------------------------------------------------------------------------

/// A vec returned to the caller (`Move { dest: ReturnSlot }`) is transferred on
/// the successful Return edge, but remains owned by the producer on earlier
/// unwind edges. The release decision is per-exit, never a global escape bit.
/// LESSONS: `boundary-fail-closed`, `cleanup-all-exits`.
#[test]
fn plain_vec_return_transfers_on_return_and_cleans_up_on_earlier_unwind() {
    let pipeline = pipeline_with_tc(
        r"
        fn make() -> Vec<i64> {
            let v: Vec<i64> = Vec.new();
            v.push(1);
            return v;
        }
        fn main() -> i64 { 0 }
        ",
    );
    let drops = return_drops(&pipeline, "make");
    assert_eq!(
        count_free(&drops, "hew_vec_free"),
        0,
        "a returned vec must NOT be freed on the successful Return edge — the \
         caller owns it there; got {drops:?}"
    );

    let unwind = unwind_drops(&pipeline, "make", "hew_vec_push_i64");
    assert_eq!(
        unwind
            .iter()
            .filter(|drop| matches!(drop.kind, DropKind::CowHeap { .. }))
            .count(),
        1,
        "an initialized vec must be released when a later push unwinds before \
         the Return-slot transfer; got {unwind:?}"
    );
}

/// A vec moved into an actor's initial state (`spawn Holder(items: v)`) is
/// owned by the actor now; the spawning function must NOT free it — the
/// actor's synthesised `state_drop_fn` is the sole owner of that free.
/// LESSONS: `container-ingress-ownership-is-per-container`.
#[test]
fn plain_vec_local_drop_excludes_spawn_escaped_vec() {
    let pipeline = pipeline_with_tc(
        r"
        actor Holder {
            let items: Vec<i64>;
            receive fn ping() -> i64 { 1 }
        }
        fn main() -> i64 {
            let v: Vec<i64> = Vec.new();
            v.push(1);
            let _h = spawn Holder(items: v);
            0
        }
        ",
    );
    let drops = all_exit_drops(&pipeline, "main");
    assert_eq!(
        count_free(&drops, "hew_vec_free"),
        0,
        "a vec moved into actor state must NOT be freed in the spawning \
         function (the actor's state_drop_fn owns the free) — emitting one \
         here is a double free; got {drops:?}"
    );
}

// ---------------------------------------------------------------------------
// Fan-out aliases — sibling whole-value copies of ONE handle must never each
// fire their own free (the Vec-pipeline receiver rebind shape).
// ---------------------------------------------------------------------------

/// A Vec used as the receiver of MULTIPLE pipeline stages is explicitly cloned
/// for each stage. Each clone has a distinct owner and an inline Drop+Release;
/// the original receiver plus the two Vec results remain live at Return. The
/// old assertion deliberately expected the ambiguous receiver group to leak,
/// before clone/result ownership became first-class Checked MIR authority.
#[test]
fn plain_vec_multi_pipeline_receiver_never_double_freed() {
    let pipeline = pipeline_with_tc(
        r"
        fn double(x: i64) -> i64 { x * 2 }
        fn main() -> i64 {
            let v: Vec<i64> = [1, 2, 3];
            let a = v.map(double);
            let b = v.filter(|x: i64| x % 2 == 0);
            let t = v.reduce(|x: i64, y: i64| x + y, 0);
            a.len() + b.len() + t
        }
        ",
    );
    let returned = return_drops(&pipeline, "main");
    let inline = inline_plain_vec_releases(&pipeline, "main");
    assert_eq!(
        count_free(&returned, "hew_vec_free"),
        3,
        "the original receiver and two Vec results are three exact live owners at Return: \
         {returned:?}"
    );
    assert_eq!(
        inline.len(),
        count_calls(&pipeline, "main", "hew_vec_clone"),
        "every explicit receiver clone must have one adjacent physical/logical cleanup pair: \
         {inline:?}"
    );
    assert_eq!(
        inline
            .iter()
            .map(|(owner, _)| *owner)
            .collect::<std::collections::HashSet<_>>()
            .len(),
        inline.len(),
        "no receiver generation may be released twice: {inline:?}"
    );
}

/// A single pipeline stage is the admitted variant: the receiver's alias
/// group holds exactly one admitted binding after the hand-off strip, so the
/// fan-out collapse must NOT touch it — the receiver's handle and the result
/// vec are each freed exactly once (no over-exclusion, no leak).
#[test]
fn plain_vec_single_pipeline_frees_receiver_and_result_exactly_once() {
    let pipeline = pipeline_with_tc(
        r"
        fn double(x: i64) -> i64 { x * 2 }
        fn main() -> i64 {
            let v: Vec<i64> = [1, 2, 3];
            let a = v.map(double);
            a.len()
        }
        ",
    );
    let drops = return_drops(&pipeline, "main");
    assert_eq!(
        live_vec_free_places(&pipeline, "main").len(),
        2,
        "a single-stage pipeline has two distinct handles (receiver + result), \
         each with exactly one admitted owner; both must free exactly once; \
         got {drops:?}"
    );
}

/// The chained form closes each stage receiver at its lexical `ScopeExit`. Those
/// exact Drop+Release pairs are inline Checked-MIR operations, while only the
/// original root survives to the Return plan. Counting Return drops alone is
/// therefore a stale representation assertion.
#[test]
fn plain_vec_chained_pipeline_frees_each_intermediate_exactly_once() {
    let pipeline = pipeline_with_tc(
        r"
        fn main() -> i64 {
            let v: Vec<i64> = [1, 2, 3, 4];
            v.filter(|x: i64| x > 1).map(|x: i64| x * 10).reduce(|a: i64, b: i64| a + b, 0)
        }
        ",
    );
    let returned = return_drops(&pipeline, "main");
    let inline = inline_plain_vec_releases(&pipeline, "main");
    assert_eq!(
        count_free(&returned, "hew_vec_free"),
        1,
        "only the original root remains live at Return: {returned:?}"
    );
    assert_eq!(
        inline.len(),
        3,
        "filter, map and reduce each close their exact stage receiver inline: {inline:?}"
    );
    assert_eq!(
        inline
            .iter()
            .map(|(owner, _)| *owner)
            .collect::<std::collections::HashSet<_>>()
            .len(),
        inline.len(),
        "each inline cleanup must discharge a distinct owner generation: {inline:?}"
    );
}

// ---------------------------------------------------------------------------
// Multi-arm match — the alias-propagation fixpoint must converge (#1942) and
// the ambiguous result group must fail closed (leak, never double-free).
// ---------------------------------------------------------------------------

/// A `match` whose result type is `Vec<i64>`, where two arms each construct a
/// fresh local Vec and yield it, moves both arm handles into ONE shared result
/// slot. Before the monotone-fixpoint fix the two distinct alias roots
/// oscillated forever and MIR lowering spun until SIGKILL (#1942: `hew check`
/// hung at ~9.6s CPU on this exact shape). The fix EVICTS the dual-rooted
/// result slot so the fixpoint converges.
///
/// Each arm now publishes an exact Relocate plus `EdgeCarry` into the shared
/// result carrier. Since these fresh arm owners are mutually exclusive, there
/// is no losing live owner to drop on either arm; the selected carrier moves
/// directly into `ReturnSlot`.
///
/// What must still never appear is a release of the RESULT slot in the
/// producing function — the caller owns what it receives.
/// LESSONS: `drop-allowset-from-value-flow`, `boundary-fail-closed`,
/// `raii-null-after-move`.
#[test]
fn plain_vec_local_drop_multi_arm_match_returned_releases_only_neutralized_arm_locals() {
    let pipeline = pipeline_with_tc(
        r"
        enum Action { Move; Left }
        fn codes(a: Action) -> Vec<i64> {
            match a {
                Action.Move => { let v: Vec<i64> = Vec.new(); v },
                Action.Left => { let v: Vec<i64> = Vec.new(); v.push(20); v }
            }
        }
        fn main() -> i64 { 0 }
        ",
    );
    let carries = divergent_arm_carries(&pipeline, "codes");
    assert_eq!(
        carries.len(),
        2,
        "each mutually-exclusive arm must carry its distinct owner into the shared result: \
         {carries:?}"
    );
    assert!(carries.windows(2).all(|pair| {
        pair[0].0 != pair[1].0 && pair[0].1 == pair[1].1 && pair[0].2 == pair[1].2
    }));
    let join_place = carries[0].1;
    let function = pipeline
        .raw_mir
        .iter()
        .find(|function| function.name == "codes")
        .unwrap();
    assert!(
        function.blocks.iter().any(|block| {
            matches!(block.terminator, Terminator::Return)
                && block.instructions.windows(2).any(|pair| {
                    matches!(
                        pair,
                        [
                            Instr::Move {
                                dest: Place::ReturnSlot,
                                src,
                            },
                            Instr::NeutralizePayloadSlot {
                                place,
                                transferee: Some(Place::ReturnSlot),
                                ..
                            },
                        ] if *src == join_place && *place == join_place
                    )
                })
        }),
        "the shared carrier must transfer directly into ReturnSlot"
    );
    assert_eq!(
        count_free(&return_drops(&pipeline, "codes"), "hew_vec_free"),
        0,
        "the successful Return transfers the selected Vec to its caller and must release none"
    );
    assert_eq!(
        count_free(&cancel_drops(&pipeline, "codes"), "hew_vec_free"),
        2,
        "each mutually-exclusive arm cancellation point must still clean its live selected owner"
    );
}

/// The same dual-root result slot when the match value is consumed LOCALLY
/// rather than returned: the two mutually-exclusive arm identities carry one
/// physical value into the shared carrier, which the lexical result binding
/// adopts. Exactly that result owner remains live at Return.
#[test]
fn plain_vec_local_drop_multi_arm_match_consumed_locally_releases_every_owner() {
    let pipeline = pipeline_with_tc(
        r"
        enum Action { Move; Left }
        fn main() -> i64 {
            let r: Vec<i64> = match Action.Left {
                Action.Move => { let v: Vec<i64> = Vec.new(); v },
                Action.Left => { let v: Vec<i64> = Vec.new(); v.push(20); v }
            };
            r.len()
        }
        ",
    );
    let carries = divergent_arm_carries(&pipeline, "main");
    assert_eq!(
        carries.len(),
        2,
        "both arm owners must reach the same shared carrier: {carries:?}"
    );
    assert!(carries.windows(2).all(|pair| {
        pair[0].0 != pair[1].0 && pair[0].1 == pair[1].1 && pair[0].2 == pair[1].2
    }));
    let freed = return_drops(&pipeline, "main")
        .iter()
        .filter(|drop| is_cow_heap_free(drop, "hew_vec_free"))
        .map(|drop| drop.place)
        .collect::<Vec<_>>();
    assert_eq!(
        freed.len(),
        1,
        "the selected carrier is adopted by one lexical result owner and freed exactly once: \
         carries {carries:?}, freed {freed:?}"
    );
    assert_ne!(
        freed[0], carries[0].1,
        "cleanup belongs to the adopted result place, not the predecessor carrier"
    );
}

/// A SINGLE-arm match yielding a fresh local Vec is the unambiguous control:
/// exactly one root flows into the result slot, so the result earns its single
/// scope-exit free — the monotone fix must NOT over-evict a sole-alias slot.
/// LESSONS: `cleanup-all-exits`.
#[test]
fn plain_vec_local_drop_single_arm_match_frees_result_exactly_once() {
    let pipeline = pipeline_with_tc(
        r"
        enum Tag { Only }
        fn main() -> i64 {
            let r: Vec<i64> = match Tag.Only {
                Tag.Only => { let v: Vec<i64> = Vec.new(); v.push(7); v }
            };
            r.len()
        }
        ",
    );
    let drops = return_drops(&pipeline, "main");
    assert_eq!(
        count_free(&drops, "hew_vec_free"),
        1,
        "a single-arm match has one alias root flowing into the result; the \
         sole-owner result must free exactly once (the fix must not evict a \
         non-conflicting slot); got {drops:?}"
    );
}

/// A direct helper whose parameter body only borrows the Vec leaves ownership
/// with the caller, which must keep its scope-exit release.
#[test]
fn plain_vec_local_drop_keeps_vec_across_borrowing_value_call() {
    let pipeline = pipeline_with_tc(
        r"
        fn total(xs: Vec<i64>) -> i64 {
            xs.len()
        }
        fn main() -> i64 {
            let v: Vec<i64> = Vec.new();
            v.push(1);
            total(v)
        }
        ",
    );
    let drops = all_exit_drops(&pipeline, "main");
    assert_eq!(
        count_free(&drops, "hew_vec_free"),
        1,
        "a borrow-only by-value helper leaves the Vec caller-owned; the caller \
         must free it exactly once; got {drops:?}"
    );
}

/// An anonymous array literal already has one exact-site `__hew_array_N`
/// owner. Passing it to a proven-borrow generic helper must reuse that owner,
/// never mint a parallel `__hew_temp_arg` over the same Vec local.
#[test]
fn array_literal_borrowing_generic_call_reuses_desugar_owner() {
    let pipeline = pipeline_with_tc(
        r"
        fn first<T>(xs: Vec<T>) -> T {
            xs[0]
        }
        fn main() -> i64 {
            first([1, 2, 3])
        }
        ",
    );
    assert!(
        pipeline.diagnostics.is_empty(),
        "the one physical Vec owner must verify without authority drift: {:#?}",
        pipeline.diagnostics
    );
    assert_eq!(
        count_binds_with_prefix(&pipeline, "main", "__hew_array_"),
        1
    );
    assert_eq!(
        count_binds_with_prefix(&pipeline, "main", "__hew_temp_arg"),
        0
    );
    assert!(
        !neutralized_sources(&pipeline, "main").is_empty(),
        "the array desugar owner must cross a committed scoped-tail Move"
    );
}

/// Negative control: a parameter-forwarding helper transfers the array Vec
/// through its result. The borrowing-owner repair must not retain a caller
/// temporary at the call edge; only the result binding owns the live handle.
#[test]
fn array_literal_forwarding_generic_call_does_not_mint_borrow_owner() {
    let pipeline = pipeline_with_tc(
        r"
        fn identity<T>(xs: Vec<T>) -> Vec<T> {
            xs
        }
        fn main() -> i64 {
            let out = identity([1, 2, 3]);
            out.len()
        }
        ",
    );
    assert!(
        pipeline.diagnostics.is_empty(),
        "the transferred Vec must keep one exact owner: {:#?}",
        pipeline.diagnostics
    );
    assert_eq!(
        count_binds_with_prefix(&pipeline, "main", "__hew_temp_arg"),
        0
    );
    let drops = all_exit_drops(&pipeline, "main");
    assert_eq!(
        count_free(&drops, "hew_vec_free"),
        1,
        "only the returned Vec binding may release the transferred handle; got {drops:?}"
    );
}

/// Negative control: a helper that returns its Vec parameter hands the handle
/// to the result owner, so the caller's source binding must remain excluded.
#[test]
fn plain_vec_local_drop_excludes_value_call_that_returns_vec() {
    let pipeline = pipeline_with_tc(
        r"
        fn identity(xs: Vec<i64>) -> Vec<i64> {
            xs
        }
        fn main() -> i64 {
            let v: Vec<i64> = Vec.new();
            let out = identity(v);
            out.len()
        }
        ",
    );
    let drops = all_exit_drops(&pipeline, "main");
    assert_eq!(
        count_free(&drops, "hew_vec_free"),
        1,
        "the returned Vec owner must free once while the caller's source stays \
         excluded; got {drops:?}"
    );
}
