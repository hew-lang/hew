//! A Hew frame must not launder an opaque `extern "C"` result into a
//! caller-side release obligation.
//!
//! The freshness summary a caller reads before minting that obligation is built
//! before the extern contract table exists, and its coarse leaf policy maps
//! EVERY body-less resolved item to fresh. A declared extern is body-less, so a
//! one-line Hew wrapper used to inherit a `true` row:
//!
//! ```hew
//! extern "C" { fn host_string() -> string; }
//! fn wrapper() -> string { unsafe { host_string() } }
//! fn main() -> i64 { println(f"value={wrapper()}"); 0 }
//! ```
//!
//! `main` bound the result to `__hew_temp_arg` and emitted
//! `drop _1 ty=string kind=cow_heap(hew_string_drop)` — the exact caller drop
//! the table forbids for a direct extern. These cases pin that no synthetic
//! owner and no `CowHeap` drop survive for the wrapper shapes, while a Hew-bodied
//! producer keeps its mint (that mint IS the f-string temp leak fix).

use hew_mir::{DropKind, IrPipeline, MirStatement, Terminator};
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;

const SYNTHETIC_TEMP_ARG: &str = "__hew_temp_arg";

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

fn synthetic_binds(p: &IrPipeline, fn_name: &str) -> usize {
    p.raw_mir
        .iter()
        .find(|f| f.name == fn_name)
        .unwrap_or_else(|| panic!("function {fn_name} must be present"))
        .blocks
        .iter()
        .flat_map(|b| b.statements.iter())
        .filter(
            |stmt| matches!(stmt, MirStatement::Bind { name, .. } if name == SYNTHETIC_TEMP_ARG),
        )
        .count()
}

fn cow_heap_drops(p: &IrPipeline, fn_name: &str) -> usize {
    p.elaborated_mir
        .iter()
        .find(|f| f.name == fn_name)
        .unwrap_or_else(|| panic!("function {fn_name} must be present"))
        .drop_plans
        .iter()
        .flat_map(|(_, plan)| plan.drops.iter())
        .filter(|drop| matches!(drop.kind, DropKind::CowHeap { .. }))
        .count()
}

fn record_in_place_drops(p: &IrPipeline, fn_name: &str) -> usize {
    p.elaborated_mir
        .iter()
        .find(|f| f.name == fn_name)
        .unwrap_or_else(|| panic!("function {fn_name} must be present"))
        .drop_plans
        .iter()
        .flat_map(|(_, plan)| plan.drops.iter())
        .filter(|drop| matches!(drop.kind, DropKind::RecordInPlace))
        .count()
}

fn call_count(p: &IrPipeline, fn_name: &str, symbol: &str) -> usize {
    p.raw_mir
        .iter()
        .find(|f| f.name == fn_name)
        .unwrap_or_else(|| panic!("function {fn_name} must be present"))
        .blocks
        .iter()
        .filter(|block| {
            matches!(&block.terminator, Terminator::Call { callee, .. } if callee == symbol)
        })
        .count()
}

/// Every fixture declares the same externs so the only variable is the SHAPE of
/// the Hew frame between the extern and the interpolation.
///
/// `host_record` is the NON-STRING heap class: the taint must be type-agnostic,
/// so a record handle the host owns is exactly as un-droppable as a string one.
const PRELUDE: &str = r#"extern "C" {
    fn host_string() -> string;
    fn host_sink(s: string);
    fn host_record() -> Holder;
}
record Holder { label: string }
"#;

fn main_with(defs: &str, body: &str) -> IrPipeline {
    pipeline_with_tc(&format!(
        "{PRELUDE}{defs}\nfn main() -> i64 {{\n    {body}\n    0\n}}\n"
    ))
}

#[test]
fn direct_extern_wrapper_result_mints_no_owner_and_no_drop() {
    let p = main_with(
        "fn wrapper() -> string { unsafe { host_string() } }",
        r#"println(f"value={wrapper()}");"#,
    );
    assert_eq!(
        synthetic_binds(&p, "main"),
        0,
        "a wrapper around an opaque extern must not produce a synthetic \
         caller-owned temporary: the handle is the host's, not ours"
    );
    assert_eq!(
        cow_heap_drops(&p, "main"),
        0,
        "and no `hew_string_drop` may be scheduled over it"
    );
}

#[test]
fn wrapper_of_a_wrapper_mints_no_owner_and_no_drop() {
    let p = main_with(
        "fn wrapper() -> string { unsafe { host_string() } }\n\
         fn wrapper2() -> string { wrapper() }",
        r#"println(f"value={wrapper2()}");"#,
    );
    assert_eq!(
        synthetic_binds(&p, "main"),
        0,
        "the veto is a fixpoint and must be TRANSITIVE: adding Hew frames \
         cannot turn a foreign handle into a fresh one"
    );
    assert_eq!(cow_heap_drops(&p, "main"), 0);
}

#[test]
fn generic_wrapper_mints_no_owner_and_no_drop() {
    let p = main_with(
        "fn gwrap<T>(t: T) -> string { unsafe { host_string() } }",
        r#"println(f"value={gwrap(1)}");"#,
    );
    assert_eq!(
        synthetic_binds(&p, "main"),
        0,
        "a monomorphisation's callee resolves to the generic ORIGIN item, so \
         the origin must carry the veto"
    );
    assert_eq!(cow_heap_drops(&p, "main"), 0);
}

#[test]
fn recursive_wrapper_mints_no_owner_and_no_drop() {
    let p = main_with(
        "fn rec(n: i64) -> string { if n > 0 { rec(n - 1) } else { unsafe { host_string() } } }",
        r#"println(f"value={rec(2)}");"#,
    );
    assert_eq!(synthetic_binds(&p, "main"), 0, "a cycle must fail closed");
    assert_eq!(cow_heap_drops(&p, "main"), 0);
}

#[test]
fn argument_launderer_mints_no_owner_and_no_drop() {
    let p = main_with(
        "fn forward(s: string) -> string { s }\n\
         fn launder() -> string { forward(unsafe { host_string() }) }",
        r#"println(f"value={launder()}");"#,
    );
    assert_eq!(
        synthetic_binds(&p, "main"),
        0,
        "passing the foreign handle THROUGH a pass-through Hew fn does not \
         make it ours"
    );
    assert_eq!(cow_heap_drops(&p, "main"), 0);
}

#[test]
fn hew_bodied_producer_keeps_its_mint() {
    let p = main_with(
        "fn mk(i: i64) -> string { f\"tok{i}\" }",
        r#"println(f"value={mk(1)}");"#,
    );
    assert_eq!(
        synthetic_binds(&p, "main"),
        1,
        "control: the f-string interpolation temp leak fix depends on this \
         mint surviving for an analyzed Hew producer"
    );
    assert_eq!(
        cow_heap_drops(&p, "main"),
        1,
        "and on exactly one `hew_string_drop` releasing it"
    );
}

#[test]
fn temp_argument_passed_to_an_extern_mints_no_owner() {
    let p = main_with(
        "fn mk(i: i64) -> string { f\"tok{i}\" }",
        "unsafe { host_sink(mk(2)); }",
    );
    assert_eq!(
        synthetic_binds(&p, "main"),
        0,
        "the temp-arg mint must consult the audited contract table, not the \
         call-DISPATCH set: `host_sink` is in `module_fn_names` only so the \
         extern call lowers as a `Terminator::Call`, and the host may retain \
         or release the handle it is passed"
    );
    assert_eq!(cow_heap_drops(&p, "main"), 0);
}

#[test]
fn temp_argument_passed_to_a_hew_fn_keeps_its_mint() {
    let p = main_with(
        "fn mk(i: i64) -> string { f\"tok{i}\" }\n\
         fn hew_sink(s: string) -> i64 { s.len() }",
        "hew_sink(mk(3));",
    );
    assert_eq!(
        synthetic_binds(&p, "main"),
        1,
        "control: an analyzed Hew callee that only BORROWS its string argument \
         leaves the caller holding the sole owner"
    );
    assert_eq!(cow_heap_drops(&p, "main"), 1);
}

// ── non-string heap classes ───────────────────────────────────────────────
//
// The veto lives in the freshness AUTHORITY, not in the string mint, so a
// record travelling the same laundering path must be refused identically.

/// F1 — a Hew wrapper returning a RECORD from an opaque extern, passed to a
/// BORROWING Hew callee.
///
/// The direct-extern name veto never fired here (a wrapper is not an extern),
/// so the composite temp-arg mint fell through to the coarse map's
/// `unwrap_or(true)` and registered a caller-owned temp over the host's handle,
/// scheduling an in-place record release the host never asked for.
#[test]
fn record_wrapper_result_passed_to_a_borrowing_callee_mints_no_owner_and_no_drop() {
    let p = main_with(
        "fn wrapRecord() -> Holder { unsafe { host_record() } }\n\
         fn borrowRecord(h: Holder) -> i64 { h.label.len() }",
        "borrowRecord(wrapRecord());",
    );
    assert_eq!(
        synthetic_binds(&p, "main"),
        0,
        "a Hew wrapper over an opaque extern is not a fresh producer for a \
         RECORD either: minting a caller-owned temp here hands a drop to a \
         handle the host still owns"
    );
    assert_eq!(
        record_in_place_drops(&p, "main"),
        0,
        "and exactly zero in-place record releases may be scheduled over it"
    );
}

/// Control for the case above: an analyzed Hew producer of the same record type
/// keeps its mint and gets exactly ONE release. Without this, the assertion
/// above would also pass if the composite mint had simply been switched off.
#[test]
fn hew_bodied_record_producer_keeps_its_mint_and_drops_once() {
    let p = main_with(
        "fn mkRecord(i: i64) -> Holder { Holder { label: f\"tok{i}\" } }\n\
         fn borrowRecord(h: Holder) -> i64 { h.label.len() }",
        "borrowRecord(mkRecord(1));",
    );
    assert_eq!(
        synthetic_binds(&p, "main"),
        1,
        "control: an analyzed Hew record producer still earns its caller-side \
         temp owner"
    );
    assert_eq!(
        record_in_place_drops(&p, "main"),
        1,
        "and EXACTLY one in-place release balances it — an exact count, not a \
         bound, so a future widening that double-registers the temp fails here"
    );
}

/// F2 — Vec ingress. `expr_is_materialized_owner` read the coarse map with no
/// veto at all, so the wrapper's result was admitted as a materialised owner
/// and routed to the MOVE-in `hew_vec_push_owned_move`; the Vec's teardown then
/// released the host's handle. It must stay on the COPY-IN
/// `hew_vec_push_owned`.
#[test]
fn record_wrapper_result_pushed_into_a_vec_stays_copy_in() {
    let p = main_with(
        "fn wrapRecord() -> Holder { unsafe { host_record() } }",
        "var v: Vec<Holder> = Vec::new();\n    v.push(wrapRecord());",
    );
    assert_eq!(
        call_count(&p, "main", "hew_vec_push_owned_move"),
        0,
        "moving a foreign handle into the Vec makes the Vec's teardown release \
         it — the same laundering, one call site over"
    );
    assert_eq!(
        call_count(&p, "main", "hew_vec_push_owned"),
        1,
        "the push must stay COPY-IN: exactly one clone-in call, and the host \
         keeps its handle"
    );
}

/// Control for the Vec route: an analyzed Hew producer of the same record type
/// still earns the MOVE-in route, so the assertion above is not just "the move
/// route is dead".
#[test]
fn hew_bodied_record_producer_pushed_into_a_vec_still_moves() {
    let p = main_with(
        "fn mkRecord(i: i64) -> Holder { Holder { label: f\"tok{i}\" } }",
        "var v: Vec<Holder> = Vec::new();\n    v.push(mkRecord(1));",
    );
    assert_eq!(
        call_count(&p, "main", "hew_vec_push_owned_move"),
        1,
        "control: the unbound-temp push leak fix depends on an analyzed Hew \
         producer keeping the MOVE-in route"
    );
    assert_eq!(call_count(&p, "main", "hew_vec_push_owned"), 0);
}

/// A DIRECT extern call at the Vec seam. The Vec route never carried the
/// name veto, so `v.push(host_record())` moved a foreign handle in even
/// without a Hew frame; folding the name veto into the authority closes it.
#[test]
fn direct_extern_result_pushed_into_a_vec_stays_copy_in() {
    let p = main_with(
        "",
        "var v: Vec<Holder> = Vec::new();\n    unsafe { v.push(host_record()); }",
    );
    assert_eq!(
        call_count(&p, "main", "hew_vec_push_owned_move"),
        0,
        "an extern result is ownership-OPAQUE at every seam, not just the ones \
         that happened to spell the name veto out"
    );
    assert_eq!(call_count(&p, "main", "hew_vec_push_owned"), 1);
}
