//! From-call match-scrutinee composite drop elaboration (#2429).
//!
//! `match f() { Ok(b) => …, Err(e) => {} }` consumes the called function's
//! `Result`/`Option` return through an anonymous MIR temp. Before this fix the
//! temp had no `BindingId`, so `build_lifo_drops` / `enumerate_exits` never saw
//! it and the arm-destructured payload was released on NO edge — not the loop
//! back-edge, not the return plan. Each iteration of a
//! `while … { match f() { … } }` loop leaked one payload allocation (n at O0),
//! the primary consumption shape for every read-style API (`while` over
//! `tls.read()`).
//!
//! The fix mints a synthetic owned binding over the scrutinee temp so the
//! PROVEN let-bound discipline covers it end to end: the enum-composite
//! sole-owner prover decides admission, the back-edge body-scope filter
//! releases per iteration, and the return plan covers the straight-line case.
//!
//! The `bytes` payload rode a second gap: `hew_bytes_len` (`b.len()`) reads the
//! payload binder as its BORROWED receiver, but the composite prover's borrow
//! exemption only consulted the string-borrow contract, so the read classified
//! as an owning escape and excluded the composite. The
//! `binder_read_is_borrow_safe_*` helpers now carry the bytes receiver-borrow
//! contract (the same authority `derive_local_bytes_drop_allowed` applies).
//!
//! Negative controls are load-bearing (`drop-allowset-from-value-flow`): a
//! let-bound scrutinee must not gain a SECOND owner over the same slot, and an
//! escaping payload must keep the composite excluded (leak, never double-free).

use hew_mir::{DropKind, ElabDrop, ExitPath, IrPipeline};
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

/// Every `ElabDrop` on the named function's `Return` exits.
fn return_drops(p: &IrPipeline, fn_name: &str) -> Vec<ElabDrop> {
    drops_matching(p, fn_name, |exit| matches!(exit, ExitPath::Return { .. }))
}

/// Every `ElabDrop` on the named function's `Goto` exits (the loop back-edge
/// plan lives on the body-closing `Goto`).
fn goto_drops(p: &IrPipeline, fn_name: &str) -> Vec<ElabDrop> {
    drops_matching(p, fn_name, |exit| matches!(exit, ExitPath::Goto { .. }))
}

/// Every `ElabDrop` across every exit of the named function.
fn all_exit_drops(p: &IrPipeline, fn_name: &str) -> Vec<ElabDrop> {
    drops_matching(p, fn_name, |_| true)
}

fn enum_in_place(drops: &[ElabDrop]) -> Vec<ElabDrop> {
    drops
        .iter()
        .filter(|d| matches!(d.kind, DropKind::EnumInPlace))
        .cloned()
        .collect()
}

/// The #2429 headline shape: a `Result<bytes, string>` returned from a call and
/// consumed directly by a `match` inside a `while` loop. The scrutinee temp
/// must earn a per-iteration `EnumInPlace` release on the loop back-edge
/// `Goto` — the edge that previously leaked one payload per iteration.
#[test]
fn from_call_bytes_scrutinee_in_loop_gets_backedge_enum_in_place_drop() {
    let p = pipeline_with_tc(
        r#"
fn f() -> Result<bytes, string> {
    Ok("payload".to_bytes())
}

fn main() {
    var i = 0;
    while i < 5 {
        match f() {
            Ok(b) => { let n = b.len(); }
            Err(e) => {}
        }
        i = i + 1;
    }
}
"#,
    );
    let backedge = enum_in_place(&goto_drops(&p, "main"));
    assert_eq!(
        backedge.len(),
        1,
        "the from-call Result<bytes, string> scrutinee must be released exactly \
         once per iteration on the loop back-edge Goto plan; got {backedge:?}"
    );
}

/// Straight-line variant: a single un-looped `match f() { … }` releases the
/// scrutinee temp on the `Return` plan (the shape that leaked one payload
/// even without a loop).
#[test]
fn from_call_bytes_scrutinee_single_gets_return_enum_in_place_drop() {
    let p = pipeline_with_tc(
        r#"
fn f() -> Result<bytes, string> {
    Ok("payload".to_bytes())
}

fn main() {
    match f() {
        Ok(b) => { let n = b.len(); }
        Err(e) => {}
    }
}
"#,
    );
    let ret = enum_in_place(&return_drops(&p, "main"));
    assert_eq!(
        ret.len(),
        1,
        "the straight-line from-call scrutinee must be released exactly once on \
         the Return plan; got {ret:?}"
    );
}

/// String payloads ride the same seam: the from-call `Result<string, string>`
/// scrutinee (previously released only when let-bound) earns the back-edge
/// release in the unbound shape too.
#[test]
fn from_call_string_scrutinee_in_loop_gets_backedge_enum_in_place_drop() {
    let p = pipeline_with_tc(
        r#"
fn f() -> Result<string, string> {
    Ok("payload".to_upper())
}

fn main() {
    var i = 0;
    while i < 5 {
        match f() {
            Ok(b) => { let n = b.len(); }
            Err(e) => {}
        }
        i = i + 1;
    }
}
"#,
    );
    let backedge = enum_in_place(&goto_drops(&p, "main"));
    assert_eq!(
        backedge.len(),
        1,
        "the from-call Result<string, string> scrutinee must be released exactly \
         once per iteration on the loop back-edge Goto plan; got {backedge:?}"
    );
}

/// Bytes-payload admission (the second #2429 gap): a LET-BOUND
/// `Result<bytes, string>` whose payload binder is only read by the borrowing
/// `b.len()` keeps the composite's `EnumInPlace` drop. Before the fix the
/// `hew_bytes_len` receiver read classified as an owning escape and excluded
/// the composite entirely (zero drops on every plan).
#[test]
fn letbound_bytes_composite_with_borrowing_len_keeps_enum_in_place_drop() {
    let p = pipeline_with_tc(
        r#"
fn f() -> Result<bytes, string> {
    Ok("payload".to_bytes())
}

fn main() {
    let r = f();
    match r {
        Ok(b) => { let n = b.len(); }
        Err(e) => {}
    }
}
"#,
    );
    let ret = enum_in_place(&return_drops(&p, "main"));
    assert_eq!(
        ret.len(),
        1,
        "a let-bound Result<bytes, string> whose payload is only borrowed by \
         .len() must keep its EnumInPlace Return drop; got {ret:?}"
    );
}

/// Negative control — no second owner. A let-bound scrutinee already owns its
/// slot; the from-call registration must not mint a second owner over the same
/// local (two admitted owners over one slot would double-free). Exactly one
/// `EnumInPlace` drop may appear on the Return plan.
#[test]
fn letbound_scrutinee_gains_no_second_enum_in_place_owner() {
    let p = pipeline_with_tc(
        r#"
fn f() -> Result<string, string> {
    Ok("payload".to_upper())
}

fn main() {
    let r = f();
    match r {
        Ok(b) => { let n = b.len(); }
        Err(e) => {}
    }
}
"#,
    );
    let ret = enum_in_place(&return_drops(&p, "main"));
    assert_eq!(
        ret.len(),
        1,
        "a let-bound scrutinee owns its slot exactly once; a second EnumInPlace \
         entry means the from-call registration double-registered; got {ret:?}"
    );
}

/// Negative control — escaping payload stays fail-closed. When the arm moves
/// the payload into an outer binding that survives the loop back-edge, the
/// composite must stay EXCLUDED from the per-iteration release (freeing it
/// would leave the outer binding dangling — the #2384 class). Leak, never
/// double-free.
#[test]
fn escaping_payload_keeps_composite_excluded_from_backedge_drop() {
    let p = pipeline_with_tc(
        r#"
fn f() -> Result<string, string> {
    Ok("payload".to_upper())
}

fn main() {
    var carry = "";
    var i = 0;
    while i < 5 {
        match f() {
            Ok(b) => { carry = b; }
            Err(e) => {}
        }
        i = i + 1;
    }
    let n = carry.len();
}
"#,
    );
    let all = enum_in_place(&all_exit_drops(&p, "main"));
    assert!(
        all.is_empty(),
        "a payload moved to an outer surviving binding escapes the composite; \
         the prover must keep the scrutinee excluded on every plan \
         (leak-not-double-free); got {all:?}"
    );
}
