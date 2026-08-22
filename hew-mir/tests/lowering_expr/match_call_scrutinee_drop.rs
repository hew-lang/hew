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

use hew_mir::{DropKind, ElabDrop, ExitPath, Instr, IrPipeline};
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

fn enum_in_place(drops: &[ElabDrop]) -> Vec<ElabDrop> {
    drops
        .iter()
        .filter(|d| matches!(d.kind, DropKind::EnumInPlace))
        .cloned()
        .collect()
}

fn string_cow_drops(drops: &[ElabDrop]) -> Vec<ElabDrop> {
    drops
        .iter()
        .filter(|drop| {
            matches!(
                drop.kind,
                DropKind::CowHeap { release }
                    if release.release_symbol() == "hew_string_drop"
            )
        })
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

/// Moving a payload binder into an early return transfers only the selected
/// payload slot. The direct-call carrier still owns its shell and every
/// remaining slot, so that same return edge must release the neutralized
/// carrier rather than treating the partial transfer as a whole-owner escape.
#[test]
fn returned_payload_binder_releases_remaining_call_carrier_on_early_return() {
    let p = pipeline_with_tc(
        r#"
record Snap { label: string, terminal: bool }

actor Svc {
    receive fn snapshot() -> Snap {
        Snap { label: "ready".to_upper(), terminal: true }
    }
}

fn empty_snap() -> Snap {
    Snap { label: "", terminal: false }
}

fn poll(svc: LocalPid<Svc>) -> Snap {
    var i = 0;
    while i < 3 {
        match await svc.snapshot() {
            .Ok(s) => {
                if s.terminal {
                    return s;
                }
            },
            .Err(_) => {},
        }
        i = i + 1;
    }
    empty_snap()
}

fn poll_let(svc: LocalPid<Svc>) -> Snap {
    var i = 0;
    while i < 3 {
        let result = await svc.snapshot();
        match result {
            .Ok(s) => {
                if s.terminal {
                    return s;
                }
            },
            .Err(_) => {},
        }
        i = i + 1;
    }
    empty_snap()
}
"#,
    );
    assert!(
        p.diagnostics.is_empty(),
        "the payload transfer and remaining carrier release must balance: {:?}",
        p.diagnostics
    );
    for name in ["poll", "poll_let"] {
        let poll = p
            .raw_mir
            .iter()
            .find(|function| function.name == name)
            .unwrap_or_else(|| panic!("raw fn {name}"));
        let return_block = poll
            .blocks
            .iter()
            .find(|block| {
                matches!(block.terminator, hew_mir::Terminator::Return)
                    && block.instructions.iter().any(|instr| {
                        matches!(
                            instr,
                            Instr::NeutralizePayloadSlot {
                                place: hew_mir::Place::MachineVariant { .. },
                                ..
                            }
                        )
                    })
            })
            .unwrap_or_else(|| panic!("payload-return block in {name}"));
        let neutralize = return_block
            .instructions
            .iter()
            .position(|instr| matches!(instr, Instr::NeutralizePayloadSlot { .. }))
            .expect("payload slot transfer");
        let release = return_block
            .instructions
            .iter()
            .position(|instr| {
                matches!(
                    instr,
                    Instr::Drop {
                        place: hew_mir::Place::Local(_),
                        drop_fn: Some(_),
                        ..
                    }
                )
            })
            .expect("remaining carrier release");
        assert!(
            release > neutralize,
            "the carrier release must follow the selected payload transfer in {name}"
        );
    }
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

/// A `CoW` payload moved directly out as the match result already receives its
/// sole release through the string ownership derivation. The record-specific
/// projection exemption must not also re-admit this binder while the fresh
/// call scrutinee remains a release authority.
#[test]
fn from_call_string_match_result_has_one_release_authority() {
    let p = pipeline_with_tc(
        r#"
type Problem { code: i64; }

fn f() -> Result<string, Problem> {
    Ok("payload".to_upper())
}

fn problem_message(problem: Problem) -> string {
    "problem-".to_upper()
}

fn main() -> i64 {
    let value = match f() {
        Ok(text) => text,
        Err(error) => problem_message(error),
    };
    value.len()
}
"#,
    );
    assert!(
        p.diagnostics.is_empty(),
        "moving a fresh CoW payload out as the match result must not make the \
         call scrutinee and binder both discharge one owner: {:?}",
        p.diagnostics
    );
    let ret = return_drops(&p, "main");
    let strings = string_cow_drops(&ret);
    assert_eq!(
        strings.len(),
        1,
        "the moved-out match result must retain exactly one string release; got {ret:?}"
    );
    assert_eq!(
        enum_in_place(&ret).len(),
        1,
        "the call-carrier shell remains the complementary inactive-alternative cleanup, \
         but must not be counted as a second discharge of the moved-out string; got {ret:?}"
    );
}

/// A non-idempotent resource selected as the value of a call-result match must
/// transfer out of the arm binder. The payload slot is cleared at that exact
/// handoff so an explicit close of the result cannot be followed by a second
/// implicit close of the binder or its carrier.
#[test]
fn from_call_resource_match_result_neutralizes_the_payload_slot() {
    let p = pipeline_with_tc(
        r#"
#[resource]
type Handle { raw: Raw; }

#[opaque]
type Raw {}

impl Handle {
    fn close(self) {}
}

fn make() -> Result<Handle, string> {
    Err("unavailable".to_upper())
}

fn main() -> i64 {
    let handle = match make() {
        Ok(value) => value,
        Err(_) => return 1,
    };
    handle.close();
    0
}
"#,
    );
    assert!(
        p.diagnostics.is_empty(),
        "the match-result handoff must preserve exactly one resource owner: {:?}",
        p.diagnostics
    );
    let main = p
        .raw_mir
        .iter()
        .find(|function| function.name == "main")
        .expect("raw fn main");
    let neutralized_fields = main
        .blocks
        .iter()
        .flat_map(|block| block.instructions.iter())
        .filter_map(|instr| match instr {
            Instr::NeutralizePayloadSlot {
                place: hew_mir::Place::MachineVariant { field_idx, .. },
                ..
            } => Some(*field_idx),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(
        neutralized_fields,
        vec![0, 0],
        "the selected resource and skipped error payloads must each transfer out of their \
         carrier on their own arm"
    );
}

/// A projected record field returned through a shared match-result local
/// transfers only that field. The record's other owned field must be released
/// on the predecessor where the projection is still uniquely attributed.
#[test]
fn vec_clone_match_field_return_releases_the_record_sibling() {
    let p = pipeline_with_tc(
        r#"
record Secret { value: string, kind: string }
enum CredErr { Missing(string); Denied(string) }

fn resolve(n: i64) -> Result<Secret, CredErr> {
    if n == 0 { return Err(CredErr.Missing("missing".to_upper())); }
    Ok(Secret { value: "secret".to_upper(), kind: "api".to_upper() })
}

fn collect(n: i64) -> string {
    let bag: Vec<Secret> = [];
    let secret = match resolve(n) {
        Err(_) => return "error",
        Ok(value) => value,
    };
    bag.push(secret);
    let first = bag.get(0);
    match first {
        Some(found) => found.value,
        None => "empty",
    }
}
"#,
    );
    assert!(
        p.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        p.diagnostics
    );
    let collect = p
        .raw_mir
        .iter()
        .find(|function| function.name == "collect")
        .expect("raw fn collect");
    let sibling_drops = collect
        .blocks
        .iter()
        .flat_map(|block| block.instructions.iter())
        .filter(|instruction| {
            matches!(
                instruction,
                Instr::FieldDropInPlace {
                    field: hew_mir::FieldAddr::Record(hew_mir::FieldOffset(1)),
                    ty: hew_types::ResolvedTy::String,
                    ..
                }
            )
        })
        .count();
    assert_eq!(
        sibling_drops, 1,
        "the returned value field must leave one exact release for the kind sibling"
    );
}

/// Moving a payload into an outer binding neutralizes its source slot. The
/// carrier therefore keeps its back-edge release for the shell and sibling
/// slot without invalidating the escaped payload.
#[test]
fn escaping_payload_neutralizes_slot_before_backedge_carrier_drop() {
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
    assert!(
        p.diagnostics.is_empty(),
        "the escaped payload and remaining carrier must both balance: {:?}",
        p.diagnostics
    );
    let main = p
        .raw_mir
        .iter()
        .find(|function| function.name == "main")
        .expect("raw fn main");
    assert!(
        main.blocks.iter().any(
            |block| block.instructions.iter().any(|instruction| matches!(
                instruction,
                Instr::NeutralizePayloadSlot {
                    place: hew_mir::Place::MachineVariant { .. },
                    ..
                }
            ))
        ),
        "the escaped payload must clear its carrier slot"
    );
    let backedge = enum_in_place(&goto_drops(&p, "main"));
    assert!(
        !backedge.is_empty(),
        "the neutralized carrier must release on the loop back-edge"
    );
}

/// A payload may itself contain a carrier whose selected leaf escapes. The
/// nested match must retain the original call carrier's projection authority:
/// clearing only the copied inner temp leaves the original record field live
/// and its terminal carrier drop would free the escaped string a second time.
#[test]
fn nested_record_payload_escape_neutralizes_original_call_carrier() {
    let p = pipeline_with_tc(
        r#"
record Slot { generation: i64, value: Option<string> }

fn clone_slot() -> Option<Slot> {
    Some(Slot { generation: 1, value: Some("payload".to_upper()) })
}

fn take() -> Option<string> {
    match clone_slot() {
        Some(slot) => match slot.value {
            Some(value) => Some(value),
            None => None,
        },
        None => None,
    }
}
"#,
    );
    assert!(
        p.diagnostics.is_empty(),
        "the nested payload transfer and original carrier release must balance: {:?}",
        p.diagnostics
    );
    let take = p
        .raw_mir
        .iter()
        .find(|function| function.name == "take")
        .expect("raw fn take");
    let projection_paths = take
        .blocks
        .iter()
        .flat_map(|block| block.instructions.iter())
        .filter_map(|instruction| match instruction {
            Instr::AggregateProjectionNeutralize {
                root: hew_mir::Place::MachineVariant { .. },
                fields,
                ..
            } => Some(fields.clone()),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(
        projection_paths,
        vec![vec![1]],
        "the escaped inner payload must clear field 1 in the original outer carrier"
    );
}
