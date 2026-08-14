//! Every scope-exit owner mint presents a provenance answer.
//!
//! Round 5 gave the `let` binder a ledger consultation and immediately found a
//! double release there. Thirteen other seams still decided ownership from
//! type, layout or dataflow, each recorded as "measures zero in the shapes I
//! could construct" — a property of the shapes reached, not a proof.
//!
//! The structural close is [`OwnerMintWarrant`](hew_mir): a token with private
//! fields whose only constructors are `Builder` methods that put the question
//! to the ledger or the module authority. Every owner-mint registrar demands
//! one, so a mint site cannot compile without an answer and no fourteenth seam
//! can open later.
//!
//! This file pins what that close DOES, with exact counts, and pairs every
//! foreign assertion with an identically shaped domestic control so no
//! assertion can be satisfied by deleting a mint outright.

use hew_mir::{DropKind, DumpStage, IrPipeline};
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
    assert!(
        tc_output.errors.is_empty(),
        "type errors: {:#?}",
        tc_output.errors
    );
    let output = hew_hir::lower_program(
        &parsed.program,
        &tc_output,
        &hew_hir::ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    hew_mir::lower_hir_module(&output.module)
}

/// Owned closure-env capture fields, read off the raw MIR's
/// `closure_env_init` ownership manifest. `own_moved` is the manifest entry
/// that both consumes the source binding's owner AND makes the heap env
/// destructor the release authority for the captured value.
fn own_moved_env_fields(p: &IrPipeline) -> usize {
    hew_mir::dump_mir(p, DumpStage::Raw)
        .matches("own=own_moved")
        .count()
}

/// Retained-share closure-env capture fields (`own_cloned_or_retained`): the
/// env destructor releases the env's OWN share while the source binding keeps
/// its scope-exit owner — the checker-`Borrow` capture manifest.
fn retained_share_env_fields(p: &IrPipeline) -> usize {
    hew_mir::dump_mir(p, DumpStage::Raw)
        .matches("own=own_cloned_or_retained")
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

/// The same declarations every round-5 fixture uses, so the only variable
/// between a foreign case and its control is the SHAPE of the Hew frame.
///
/// `host_record` is the non-string heap class: a root `extern "C" -> string` is
/// ADOPTED into a domestic refcounted buffer at the call edge and is therefore
/// deliberately NOT foreign afterwards, so a string extern cannot express these
/// shapes.
const PRELUDE: &str = r#"extern "C" {
    fn host_record() -> Holder;
    fn host_sink(s: string);
}
record Holder { label: string }
record Wrapper { inner: Holder }
enum Boxed { Full(Holder); Empty }
"#;

/// Everything under test runs inside a bounded loop, matching the round-5 pins:
/// a loop body has three exit edges, so a single scope-exit owner shows up as
/// three drop-plan entries and a withheld one as zero.
fn in_loop(defs: &str, body: &str) -> IrPipeline {
    pipeline_with_tc(&format!(
        "{PRELUDE}{defs}\nfn main() -> i64 {{\n    var i: i64 = 0;\n    \
         while i < 2 {{\n        {body}\n        i = i + 1;\n    }}\n    0\n}}\n"
    ))
}

const CAPTURE_BODY: &str = "let h = Wrapper { inner: mk(i) };\n        \
     let n = run(|| h.inner.label.len());\n        println(f\"x={n}\");";
const RUN: &str = "fn run(f: fn() -> i64) -> i64 { f() }\n";
const FOREIGN_MK: &str = "fn mk(i: i64) -> Holder { unsafe { host_record() } }";
const DOMESTIC_MK: &str = "fn mk(i: i64) -> Holder { Holder { label: f\"x{i}\" } }";

// ---------------------------------------------------------------------------
// U2 — closure env captures. The round-6 finding.
// ---------------------------------------------------------------------------

/// THE FINDING. A closure whose env is heap-allocated — the checker classifies
/// any closure crossing a call boundary as escaping — used to take a captured
/// binding as `own_moved` purely from the env layout and the capture's
/// `ValueClass`. The ledger holding the proven-foreign fact is per-function and
/// was never consulted.
///
/// `own_moved` is not a bookkeeping label. It (a) consumes the source binding's
/// scope-exit owner and (b) installs the heap env destructor as the release
/// authority for the captured value. For a value carrying a handle a declared,
/// non-audited `extern` produced, (b) is a release of a handle this program
/// never owned — the same double release the `let` binder had, one layer down.
///
/// The decision recorded for U2 is THE LEDGER CROSSES:
/// `closure_env_capture_ownership` already runs in the ENCLOSING builder, which
/// is exactly the frame whose ledger holds the fact, and the parent's ledger is
/// additionally cloned into every child builder so a nested closure sees it too.
///
/// Exact count: **1 → 0** owned env fields.
#[test]
fn a_heap_env_capture_of_a_proven_foreign_binding_takes_no_ownership() {
    let p = in_loop(&format!("{RUN}{FOREIGN_MK}"), CAPTURE_BODY);
    assert_eq!(
        own_moved_env_fields(&p),
        0,
        "a capture of a proven-foreign binding must not become an owned env \
         field: the env destructor would release a handle this program never \
         owned. Before the ledger crossed into the capture decision this was 1."
    );
    assert_eq!(
        record_in_place_drops(&p, "main"),
        0,
        "and no scope-exit record release survives anywhere in the frame"
    );
}

/// The counterfactual, and it is the whole reason the assertion above cannot be
/// satisfied by deleting the mint: the identically shaped DOMESTIC capture
/// still makes the env destructor a release authority. Since the checker
/// classifies this read-only capture as `Borrow`, the env now owns a RETAINED
/// SHARE (`own_cloned_or_retained`) rather than consuming the source: the env
/// destructor releases the env's share and the source binding keeps its own
/// scope-exit owner. Exact counts: **1** retained env field, **0** moved, and
/// the source's `RecordInPlace` scope-exit releases survive in `main`.
#[test]
fn a_heap_env_capture_of_a_domestic_binding_still_owns_it() {
    let p = in_loop(&format!("{RUN}{DOMESTIC_MK}"), CAPTURE_BODY);
    assert_eq!(
        retained_share_env_fields(&p),
        1,
        "the withhold is provenance-directed: a domestic read-only capture \
         mints a retained share into the closure env"
    );
    assert_eq!(
        own_moved_env_fields(&p),
        0,
        "a Borrow-mode capture of a retainable shape must not consume the source"
    );
    assert!(
        record_in_place_drops(&p, "main") > 0,
        "the source binding keeps its own scope-exit release alongside the \
         env's retained share"
    );
}

// ---------------------------------------------------------------------------
// U1 — pattern payload binders
// ---------------------------------------------------------------------------

const MATCH_BODY: &str = "let b = Boxed::Full(mk(i));\n        \
     match b { Boxed::Full(h) => { let n = h.label.len(); println(f\"x={n}\"); } \
     Boxed::Empty => {} }";
const IF_LET_BODY: &str = "let b = Boxed::Full(mk(i));\n        \
     if let Boxed::Full(h) = b { let n = h.label.len(); println(f\"x={n}\"); }";

/// A `match` payload binder over a proven-foreign scrutinee acquires no
/// scope-exit owner. The binder now presents a warrant built by
/// `owner_warrant_for_scrutinee_payload`, which asks the ledger about the
/// SCRUTINEE — the value the payload is projected out of.
#[test]
fn a_match_payload_binder_over_a_proven_foreign_scrutinee_mints_no_owner() {
    let p = in_loop(FOREIGN_MK, MATCH_BODY);
    assert_eq!(record_in_place_drops(&p, "main"), 0);
    assert_eq!(cow_heap_drops(&p, "main"), 0);
}

/// The domestic control: six `EnumInPlace` releases across the loop body's exit
/// edges, unchanged by the warrant.
#[test]
fn a_match_payload_binder_over_a_domestic_scrutinee_keeps_its_releases() {
    let p = in_loop(DOMESTIC_MK, MATCH_BODY);
    let enum_drops = p
        .elaborated_mir
        .iter()
        .find(|f| f.name == "main")
        .expect("main")
        .drop_plans
        .iter()
        .flat_map(|(_, plan)| plan.drops.iter())
        .filter(|drop| matches!(drop.kind, DropKind::EnumInPlace))
        .count();
    assert_eq!(
        enum_drops, 6,
        "the domestic shape keeps every release it had — a blanket withhold \
         would leak ordinary payload binders"
    );
}

/// The same for the `if let` payload binder, which is a separate mint site in
/// `control_flow.rs` and therefore a separate warrant.
#[test]
fn an_if_let_payload_binder_over_a_proven_foreign_scrutinee_mints_no_owner() {
    let p = in_loop(FOREIGN_MK, IF_LET_BODY);
    assert_eq!(record_in_place_drops(&p, "main"), 0);
    assert_eq!(cow_heap_drops(&p, "main"), 0);
}

#[test]
fn an_if_let_payload_binder_over_a_domestic_scrutinee_keeps_its_releases() {
    let p = in_loop(DOMESTIC_MK, IF_LET_BODY);
    let enum_drops = p
        .elaborated_mir
        .iter()
        .find(|f| f.name == "main")
        .expect("main")
        .drop_plans
        .iter()
        .flat_map(|(_, plan)| plan.drops.iter())
        .filter(|drop| matches!(drop.kind, DropKind::EnumInPlace))
        .count();
    assert_eq!(enum_drops, 4);
}

// ---------------------------------------------------------------------------
// U3 / U9 — the caller-side ownership-transfer refusal
// ---------------------------------------------------------------------------

/// A parameter has no expression in the callee's frame and `lower_params` runs
/// strictly before the body, so the callee's ledger is provably empty of
/// parameters — the callee CANNOT ask. The question therefore moves to the
/// caller, which is the only frame that can answer it, and handing a
/// proven-foreign value into a parameter the callee will mint an owner for is
/// refused.
///
/// `Boxed` is a heap-owning enum composite and `takes` consumes it, which is
/// exactly the #2732 callee-drop mint condition.
#[test]
fn transferring_a_proven_foreign_value_into_an_owning_parameter_is_refused() {
    let p = in_loop(
        &format!(
            "{FOREIGN_MK}\nfn takes(b: Boxed) -> Holder {{ \
             match b {{ Boxed::Full(h) => h, Boxed::Empty => Holder {{ label: \"e\" }} }} }}"
        ),
        "let b = Boxed::Full(mk(i));\n        let h = takes(b);\n        \
         let n = h.label.len();\n        println(f\"x={n}\");",
    );
    assert!(
        p.diagnostics
            .iter()
            .any(|d| format!("{:?}", d.kind)
                .contains("ownership transfer of a proven-foreign value")),
        "expected the caller-side refusal, got: {:#?}",
        p.diagnostics
    );
}

/// The counterfactual: the identical transfer of a DOMESTIC value compiles
/// clean. The refusal is provenance-directed, not a ban on owning parameters.
#[test]
fn transferring_a_domestic_value_into_an_owning_parameter_still_compiles() {
    let p = in_loop(
        &format!(
            "{DOMESTIC_MK}\nfn takes(b: Boxed) -> Holder {{ \
             match b {{ Boxed::Full(h) => h, Boxed::Empty => Holder {{ label: \"e\" }} }} }}"
        ),
        "let b = Boxed::Full(mk(i));\n        let h = takes(b);\n        \
         let n = h.label.len();\n        println(f\"x={n}\");",
    );
    assert!(
        p.diagnostics.is_empty(),
        "a domestic ownership transfer must still compile: {:#?}",
        p.diagnostics
    );
}

/// The refusal predicate MIRRORS `lower_params`, and this pins the difference
/// that measurement forced.
///
/// `call_param_consume` is a body-escape summary, not a mint predicate. The
/// `string::fmt` display shim every f-string interpolation routes through
/// carries `ProvenConsume` on its `string` parameter and mints nothing at all,
/// because `lower_params` conjoins the heap-owning-enum-composite type gate.
/// Reading the summary alone refused `println(f"…{h.label}…")` for every
/// proven-foreign `h` — a program with no double release in it.
///
/// Refusing where the callee does not mint is not "fail closed", it is a false
/// rejection, so this must compile.
#[test]
fn interpolating_a_field_of_a_proven_foreign_binding_is_not_refused() {
    let p = in_loop(
        FOREIGN_MK,
        "let h = mk(i);\n        println(f\"x={h.label}\");",
    );
    assert!(
        p.diagnostics.is_empty(),
        "`string::fmt` mints no owner for its `string` parameter, so there is \
         nothing to refuse here: {:#?}",
        p.diagnostics
    );
    assert_eq!(
        record_in_place_drops(&p, "main"),
        0,
        "and the foreign record itself still acquires no release"
    );
}

// ---------------------------------------------------------------------------
// U6 / U10 — string temps, closed by a type-and-operator exclusion
// ---------------------------------------------------------------------------

/// The string-concat exclusion is deliberately NOT a provenance query, and this
/// pins why: `hew_string_concat` returns a buffer it allocated at the site from
/// bytes copied OUT OF its borrowed operands, so the minted value is never an
/// operand's allocation whatever the operands' provenance. Asking the strict
/// query about the operand tree would answer OPAQUE here and withhold the mint,
/// leaking the fresh buffer — which is the f-string temp leak this branch
/// exists to fix.
#[test]
fn a_concat_over_a_proven_foreign_operand_keeps_its_fresh_buffer_release() {
    let p = in_loop(
        FOREIGN_MK,
        "let h = mk(i);\n        let s = \"v=\" + h.label;\n        \
         let n = s.len();\n        println(f\"x={n}\");",
    );
    assert!(
        cow_heap_drops(&p, "main") > 0,
        "the concat result is this frame's own fresh allocation and keeps its \
         release regardless of operand provenance"
    );
    assert_eq!(
        record_in_place_drops(&p, "main"),
        0,
        "while the foreign operand itself still acquires no release"
    );
}

/// The domestic control for the same shape.
#[test]
fn a_concat_over_a_domestic_operand_keeps_its_fresh_buffer_release() {
    let p = in_loop(
        DOMESTIC_MK,
        "let h = mk(i);\n        let s = \"v=\" + h.label;\n        \
         let n = s.len();\n        println(f\"x={n}\");",
    );
    assert!(cow_heap_drops(&p, "main") > 0);
}
