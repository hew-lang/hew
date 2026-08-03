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
//! `main` emitted `drop _1 ty=string kind=cow_heap(hew_string_drop)` — the exact
//! caller drop the table forbids for a direct extern. These cases pin that no
//! `CowHeap` drop survives for the wrapper shapes, while a Hew-bodied producer
//! keeps the release required by the f-string temporary fix.

use std::collections::HashSet;

use hew_mir::{DropFnSpec, DropKind, Instr, IrPipeline, Place, Terminator};
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;

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

fn call_result_release_count(p: &IrPipeline, fn_name: &str, callee: &str) -> usize {
    let raw = p
        .raw_mir
        .iter()
        .find(|f| f.name == fn_name)
        .unwrap_or_else(|| panic!("function {fn_name} must be present"));
    let destinations: HashSet<Place> = raw
        .blocks
        .iter()
        .filter_map(|block| match &block.terminator {
            Terminator::Call {
                callee: symbol,
                dest: Some(dest),
                ..
            } if symbol == callee => Some(*dest),
            _ => None,
        })
        .collect();
    let inline = raw
        .blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter(|instruction| {
            matches!(instruction, Instr::Drop {
                place,
                drop_fn: Some(DropFnSpec::Release("hew_string_drop")),
                ..
            } if destinations.contains(place))
        })
        .count();
    let exit = p
        .elaborated_mir
        .iter()
        .find(|f| f.name == fn_name)
        .unwrap_or_else(|| panic!("function {fn_name} must be present"))
        .drop_plans
        .iter()
        .flat_map(|(_, plan)| &plan.drops)
        .filter(|drop| {
            destinations.contains(&drop.place) && matches!(drop.kind, DropKind::CowHeap { .. })
        })
        .count();
    inline + exit
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
    assert_eq!(cow_heap_drops(&p, "main"), 0);
}

#[test]
fn generic_wrapper_mints_no_owner_and_no_drop() {
    let p = main_with(
        "fn gwrap<T>(t: T) -> string { unsafe { host_string() } }",
        r#"println(f"value={gwrap(1)}");"#,
    );
    assert_eq!(cow_heap_drops(&p, "main"), 0);
}

#[test]
fn recursive_wrapper_mints_no_owner_and_no_drop() {
    let p = main_with(
        "fn rec(n: i64) -> string { if n > 0 { rec(n - 1) } else { unsafe { host_string() } } }",
        r#"println(f"value={rec(2)}");"#,
    );
    assert_eq!(cow_heap_drops(&p, "main"), 0);
}

#[test]
fn argument_launderer_mints_no_owner_and_no_drop() {
    let p = main_with(
        "fn forward(s: string) -> string { s }\n\
         fn launder() -> string { forward(unsafe { host_string() }) }",
        r#"println(f"value={launder()}");"#,
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
        call_result_release_count(&p, "main", "mk"),
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
    assert_eq!(cow_heap_drops(&p, "main"), 0);
}

#[test]
fn temp_argument_passed_to_a_hew_fn_keeps_its_mint() {
    let p = main_with(
        "fn mk(i: i64) -> string { f\"tok{i}\" }\n\
         fn hew_sink(s: string) -> i64 { s.len() }",
        "hew_sink(mk(3));",
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

/// F2 — the COMPOSITE rule. The outer record literal genuinely IS fresh: this
/// frame allocated it. The defect was taking that freshness to imply ownership
/// of every field, so the outer value's mint scheduled a RECURSIVE release that
/// reached the host's handle in `inner`.
///
/// Every composite release in this compiler is generated from the container's
/// LAYOUT, not from a per-field provenance map, so there is no drop plan that
/// frees the container's spine while sparing a foreign field. The container is
/// therefore not minted at all.
#[test]
fn record_literal_embedding_a_direct_extern_mints_no_owner_and_no_drop() {
    let p = main_with(
        "record Outer { inner: Holder }\n\
         fn borrowOuter(o: Outer) -> i64 { o.inner.label.len() }",
        "unsafe { borrowOuter(Outer { inner: host_record() }); }",
    );
    assert_eq!(
        record_in_place_drops(&p, "main"),
        0,
        "and no in-place release may be scheduled over the container either"
    );
}

/// The same composite one Hew frame away from the extern. The composite query
/// runs the SAME taint transfer as the return channel, so a laundering wrapper
/// between the extern and the field buys no ownership.
#[test]
fn record_literal_embedding_a_wrapper_mints_no_owner_and_no_drop() {
    let p = main_with(
        "record Outer { inner: Holder }\n\
         fn wrapRecord() -> Holder { unsafe { host_record() } }\n\
         fn borrowOuter(o: Outer) -> i64 { o.inner.label.len() }",
        "borrowOuter(Outer { inner: wrapRecord() });",
    );
    assert_eq!(record_in_place_drops(&p, "main"), 0);
}

/// COUNTERFACTUAL for the two cases above: the identical container built from a
/// DOMESTIC field keeps its mint and gets exactly one release. Reverting the
/// composite rule flips the two zeros above to one; deleting the composite mint
/// outright flips these to zero. Only the real rule satisfies both.
#[test]
fn record_literal_of_a_domestic_field_keeps_its_mint_and_drops_once() {
    let p = main_with(
        "record Outer { inner: Holder }\n\
         fn mkRecord(i: i64) -> Holder { Holder { label: f\"tok{i}\" } }\n\
         fn borrowOuter(o: Outer) -> i64 { o.inner.label.len() }",
        "borrowOuter(Outer { inner: mkRecord(1) });",
    );
    assert_eq!(
        record_in_place_drops(&p, "main"),
        1,
        "and EXACTLY one in-place release balances it"
    );
}

/// The rule is about COMPOSITES, not about one container syntax: a tuple
/// literal reaches the same three mint predicates, and its recursive release
/// reaches its foreign element too. Pinned at the Vec seam because the
/// borrowing-callee seam does not currently mint for tuple arguments at all
/// (measured: the domestic control mints zero there, so that shape would be a
/// vacuous assertion).
#[test]
fn tuple_embedding_a_direct_extern_pushed_into_a_vec_stays_copy_in() {
    let p = main_with(
        "",
        "var v: Vec<(Holder, i64)> = Vec::new();\n    unsafe { v.push((host_record(), 1)); }",
    );
    assert_eq!(
        call_count(&p, "main", "hew_vec_push_owned_move"),
        0,
        "a tuple is as composite as a record: moving it in hands the Vec's \
         teardown a release of the host's handle"
    );
    assert_eq!(call_count(&p, "main", "hew_vec_push_owned"), 1);
}

/// COUNTERFACTUAL for the tuple: a domestic pair still MOVES in.
#[test]
fn tuple_of_a_domestic_value_pushed_into_a_vec_still_moves() {
    let p = main_with(
        "fn mkRecord(i: i64) -> Holder { Holder { label: f\"tok{i}\" } }",
        "var v: Vec<(Holder, i64)> = Vec::new();\n    v.push((mkRecord(1), 1));",
    );
    assert_eq!(call_count(&p, "main", "hew_vec_push_owned_move"), 1);
    assert_eq!(call_count(&p, "main", "hew_vec_push_owned"), 0);
}

/// A foreign handle buried at depth taints the whole spine, because the
/// outermost release walks all of it.
#[test]
fn nested_container_embedding_a_direct_extern_mints_no_owner() {
    let p = main_with(
        "record Outer { inner: Holder }\n\
         record Mid { o: Outer }\n\
         fn borrowMid(m: Mid) -> i64 { m.o.inner.label.len() }",
        "unsafe { borrowMid(Mid { o: Outer { inner: host_record() } }); }",
    );
    assert_eq!(record_in_place_drops(&p, "main"), 0);
}

fn not_yet_implemented_count(p: &IrPipeline, needle: &str) -> usize {
    p.diagnostics
        .iter()
        .filter(|d| match &d.kind {
            hew_mir::MirDiagnosticKind::NotYetImplemented { construct, .. } => {
                construct.contains(needle)
            }
            _ => false,
        })
        .count()
}

/// F3 — HashMap/HashSet ingress. Unlike the Vec seam there is no COPY-IN
/// fallback: `hew-runtime`'s hashmap documents ingress as MOVE by ABI and says
/// copy-in is intentionally absent. So "fail closed" here cannot mean "route
/// the other way" — it must mean refusing the ingress, because accepting it
/// hands the map's teardown a release of the host's handle.
#[test]
fn hashmap_insert_of_a_wrapped_extern_record_fails_closed() {
    let p = main_with(
        "fn wrapRecord() -> Holder { unsafe { host_record() } }",
        "var m: HashMap<i64, Holder> = HashMap::new();\n    m.insert(1, wrapRecord());",
    );
    assert_eq!(
        not_yet_implemented_count(&p, "ownership-opaque provenance"),
        1,
        "exactly one fail-closed diagnostic: {:#?}",
        p.diagnostics
    );
}

/// The same for a DIRECT extern operand — the seam must not depend on whether a
/// Hew frame happens to sit in between.
#[test]
fn hashmap_insert_of_a_direct_extern_record_fails_closed() {
    let p = main_with(
        "",
        "var m: HashMap<i64, Holder> = HashMap::new();\n    unsafe { m.insert(1, host_record()); }",
    );
    assert_eq!(
        not_yet_implemented_count(&p, "ownership-opaque provenance"),
        1,
        "exactly one fail-closed diagnostic: {:#?}",
        p.diagnostics
    );
}

/// COUNTERFACTUAL for F3: a domestic producer at the same seam still compiles
/// and still MOVES in. Reverting the reject makes the two cases above report
/// zero diagnostics; widening it into a blanket stop makes this one report a
/// diagnostic. Only the provenance-directed reject satisfies both.
#[test]
fn hashmap_insert_of_a_domestic_record_still_compiles() {
    let p = main_with(
        "fn mkRecord(i: i64) -> Holder { Holder { label: f\"tok{i}\" } }",
        "var m: HashMap<i64, Holder> = HashMap::new();\n    m.insert(1, mkRecord(1));",
    );
    assert_eq!(
        not_yet_implemented_count(&p, "ownership-opaque provenance"),
        0,
        "a domestic value at the collection seam must be unaffected: {:#?}",
        p.diagnostics
    );
}

/// And the same for `HashSet`, whose ingress carries the identical move contract.
#[test]
fn hashset_insert_of_a_wrapped_extern_string_fails_closed() {
    let p = main_with(
        "fn wrapper() -> string { unsafe { host_string() } }",
        "var s: HashSet<string> = HashSet::new();\n    s.insert(wrapper());",
    );
    assert_eq!(
        not_yet_implemented_count(&p, "ownership-opaque provenance"),
        1,
        "exactly one fail-closed diagnostic: {:#?}",
        p.diagnostics
    );
}

// ---------------------------------------------------------------------------
// The `let` binder. Seeding drop elaboration from a binding's TYPE alone means
// a binder over an opaque foreign producer gets a scope-exit release the
// program never earned. These pin the proven-foreign veto and, just as
// importantly, the three places it must NOT reach.
// ---------------------------------------------------------------------------

#[test]
fn a_let_bound_direct_extern_record_gets_no_scope_exit_drop() {
    let p = main_with("", "var i: i64 = 0;\n    while i < 2 {\n        let h = unsafe { host_record() };\n        let n = h.label.len();\n        println(f\"x={n}\");\n        i = i + 1;\n    }");
    assert_eq!(
        record_in_place_drops(&p, "main"),
        0,
        "a binder over a root extern's record must not be released by the caller"
    );
}

#[test]
fn a_let_bound_domestic_record_still_gets_its_scope_exit_drop() {
    let p = main_with(
        "fn mk(n: i64) -> Holder { Holder { label: f\"x{n}\" } }",
        "var i: i64 = 0;\n    while i < 2 {\n        let h = mk(i);\n        let n = h.label.len();\n        println(f\"x={n}\");\n        i = i + 1;\n    }",
    );
    assert_eq!(
        record_in_place_drops(&p, "main"),
        3,
        "the veto is provenance-directed: a domestic producer keeps its release \
         at each of the loop body's three exit edges"
    );
}

#[test]
fn a_let_bound_record_embedding_a_direct_extern_gets_no_scope_exit_drop() {
    let p = main_with(
        "record Outer { inner: Holder }",
        "var i: i64 = 0;\n    while i < 2 {\n        let o = Outer { inner: unsafe { host_record() } };\n        let n = o.inner.label.len();\n        println(f\"x={n}\");\n        i = i + 1;\n    }",
    );
    assert_eq!(
        record_in_place_drops(&p, "main"),
        0,
        "the foreign fact must travel with the binder into a container"
    );
}

#[test]
fn a_let_bound_extern_string_keeps_its_adoption_drop() {
    let p = main_with(
        "",
        "let s = unsafe { host_string() };\n    let n = s.len();\n    println(f\"x={n}\");",
    );
    assert_eq!(
        cow_heap_drops(&p, "main"),
        1,
        "a root `extern -> string` is ADOPTED at the call edge, so the binder \
         holds a value the program really owns and its release must survive"
    );
}
