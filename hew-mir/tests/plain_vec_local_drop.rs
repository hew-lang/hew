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

use hew_mir::{DropKind, ElabDrop, ExitPath, IrPipeline};
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
            let v: Vec<i64> = Vec::new();
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
            let v: Vec<string> = Vec::new();
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
        count_free(&drops, "hew_vec_free"),
        1,
        "an array-literal vec must free exactly once across all exits — the \
         synthetic desugar binding and the user binding share ONE handle; \
         got {drops:?}"
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
            let pts: Vec<Point> = Vec::new();
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
        count_free(&drops, "hew_vec_free"),
        1,
        "an array-repeat of a BitCopy record must free exactly once across all \
         exits — the desugar binding and the user binding share ONE handle; \
         got {drops:?}"
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
            let v: Vec<i64> = Vec::new();
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
            let hs: Vec<Header> = Vec::new();
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

// ---------------------------------------------------------------------------
// Escape — negative controls (no drop where the handle leaves this scope).
// ---------------------------------------------------------------------------

/// A vec returned to the caller (`Move { dest: ReturnSlot }`) is owned by the
/// caller now; the producing function must NOT free it.
/// LESSONS: `boundary-fail-closed`.
#[test]
fn plain_vec_local_drop_excludes_returned_vec() {
    let pipeline = pipeline_with_tc(
        r"
        fn make() -> Vec<i64> {
            let v: Vec<i64> = Vec::new();
            v.push(1);
            return v;
        }
        fn main() -> i64 { 0 }
        ",
    );
    let drops = all_exit_drops(&pipeline, "make");
    assert_eq!(
        count_free(&drops, "hew_vec_free"),
        0,
        "a returned vec must NOT be freed in its producing function — the \
         caller owns it; got {drops:?}"
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
            let v: Vec<i64> = Vec::new();
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

/// A Vec used as the receiver of MULTIPLE pipeline stages is whole-value
/// rebound into one synthetic `__hew_pipe_src_N` PER stage — sibling aliases
/// of ONE handle, none flowing into another. The fan-out collapse must leave
/// at most one free per Move-connected component: here the receiver's whole
/// alias group is ambiguous, so it leaks (fail-closed) and ONLY the two
/// pipeline result vecs are freed. Before the collapse, every sibling fired
/// its own `hew_vec_free` of the receiver's handle — the exit-time
/// invalid-free crash. LESSONS: `boundary-fail-closed`.
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
    let drops = return_drops(&pipeline, "main");
    assert_eq!(
        count_free(&drops, "hew_vec_free"),
        2,
        "three pipeline stages over one receiver share ONE handle across the \
         synthetic per-stage rebinds; the plan must free exactly the two \
         result vecs and leak the ambiguous receiver group — one extra free \
         is the exit-time invalid-free crash; got {drops:?}"
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
        count_free(&drops, "hew_vec_free"),
        2,
        "a single-stage pipeline has two distinct handles (receiver + result), \
         each with exactly one admitted owner; both must free exactly once; \
         got {drops:?}"
    );
}

/// The chained form hands each intermediate vec off through exactly one
/// downstream synthetic rebind — a CHAIN, not a fan-out. Every intermediate
/// must keep its exactly-one free (the collapse must not over-exclude
/// single-owner components). LESSONS: `cleanup-all-exits`.
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
    let drops = return_drops(&pipeline, "main");
    assert_eq!(
        count_free(&drops, "hew_vec_free"),
        3,
        "a filter->map->reduce chain has three distinct handles (receiver, \
         filter out, map out), each a single-owner hand-off chain; all three \
         must free exactly once; got {drops:?}"
    );
}

/// A vec consumed by a by-value call is owned by the callee now; the calling
/// function must NOT also free it. LESSONS: `raii-null-after-move`.
#[test]
fn plain_vec_local_drop_excludes_by_value_consumed_vec() {
    let pipeline = pipeline_with_tc(
        r"
        fn total(xs: Vec<i64>) -> i64 {
            xs.len()
        }
        fn main() -> i64 {
            let v: Vec<i64> = Vec::new();
            v.push(1);
            total(v)
        }
        ",
    );
    let drops = all_exit_drops(&pipeline, "main");
    assert_eq!(
        count_free(&drops, "hew_vec_free"),
        0,
        "a vec passed by value is consumed by the callee; the caller must \
         NOT also free it; got {drops:?}"
    );
}
