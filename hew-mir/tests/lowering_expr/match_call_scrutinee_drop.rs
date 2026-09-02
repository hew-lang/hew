//! From-call match-scrutinee composite drop elaboration (#2429).
//!
//! `match f() { Ok(b) => …, Err(e) => {} }` consumes the called function's
//! `Result`/`Option` return through an anonymous MIR temp. Before this fix the
//! temp had no `BindingId`, so the drop elaborator never minted an owner for
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

use hew_mir::{
    CheckedMirFunction, DropFnSpec, DropKind, ElabDrop, ExitPath, InPlaceReleaseKind, Instr,
    IrPipeline, MirStatement, OwnerId, OwnershipEvent, Place,
};
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

fn binding_owner(function: &CheckedMirFunction, name: &str) -> (OwnerId, Place) {
    let binding = function
        .blocks
        .iter()
        .flat_map(|block| &block.statements)
        .find_map(|statement| match statement {
            MirStatement::Bind {
                binding,
                name: binding_name,
                ..
            } if binding_name == name => Some(*binding),
            _ => None,
        })
        .unwrap_or_else(|| panic!("owned binding {name}"));
    let definitions: Vec<(OwnerId, Place)> = function
        .blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instruction| match instruction {
            Instr::OwnershipEvent(OwnershipEvent::Mint { owner, place, .. })
                if owner.binding == binding =>
            {
                Some((*owner, *place))
            }
            Instr::OwnershipEvent(OwnershipEvent::Transfer {
                to: Some(place),
                to_owner: Some(owner),
                ..
            }) if owner.binding == binding => Some((*owner, *place)),
            _ => None,
        })
        .collect();
    assert_eq!(
        definitions.len(),
        1,
        "{name} must publish exactly one owner: {definitions:?}"
    );
    definitions[0]
}

fn call_carrier_owner(function: &CheckedMirFunction) -> (OwnerId, Place) {
    binding_owner(function, "__hew_call_scrutinee")
}

fn block_reaches_before_owner_redefinition(
    function: &CheckedMirFunction,
    from: u32,
    target: u32,
    owner: OwnerId,
) -> bool {
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
            if block.instructions.iter().any(|instruction| {
                matches!(
                    instruction,
                    Instr::OwnershipEvent(OwnershipEvent::Mint { owner: candidate, .. })
                        if *candidate == owner
                )
            }) {
                continue;
            }
            pending.extend(block.successors());
        }
    }
    false
}

fn carrier_release_block(block: &hew_mir::BasicBlock, owner: OwnerId, place: Place) -> Option<u32> {
    let pairs: Vec<usize> = block
        .instructions
        .windows(2)
        .enumerate()
        .filter_map(|(index, pair)| {
            matches!(
                pair,
                [
                    Instr::Drop {
                        place: drop_place,
                        drop_fn: Some(DropFnSpec::InPlace(InPlaceReleaseKind::Enum)),
                        ..
                    },
                    Instr::OwnershipEvent(OwnershipEvent::Release {
                        owner: release_owner,
                        place: release_place,
                    })
                ] if *drop_place == place && *release_owner == owner && *release_place == place
            )
            .then_some(index + 1)
        })
        .collect();
    if pairs.is_empty() {
        return None;
    }
    assert_eq!(
        pairs.len(),
        1,
        "call carrier must release at most once in block {}",
        block.id
    );
    assert!(
        block.instructions[pairs[0] + 1..]
            .iter()
            .any(|instruction| matches!(
                instruction,
                Instr::OwnershipEvent(OwnershipEvent::ScopeExit { owners, .. })
                    if owners.iter().filter(|candidate| **candidate == owner).count() == 1
            )),
        "call-carrier Release must be ratified by a later ScopeExit in block {}: {:#?}",
        block.id,
        block.instructions
    );
    Some(block.id)
}

/// Assert the canonical per-arm call-carrier cleanup. Each selected variant
/// owns one inline enum Drop/Release before its lexical `ScopeExit`; the two
/// sites are mutually exclusive and no closing `Goto` plan duplicates either.
fn inline_call_carrier_cleanup(p: &IrPipeline, fn_name: &str) -> (OwnerId, Place, Vec<u32>) {
    let function = p
        .checked_mir
        .iter()
        .find(|function| function.name == fn_name)
        .unwrap_or_else(|| panic!("function {fn_name} must be present in checked_mir"));
    let (owner, place) = call_carrier_owner(function);
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
        vec![&DropKind::EnumInPlace],
        "call carrier must publish one exact EnumInPlace recipe"
    );
    let release_blocks: Vec<u32> = function
        .blocks
        .iter()
        .filter_map(|block| carrier_release_block(block, owner, place))
        .collect();
    assert_eq!(
        release_blocks.len(),
        2,
        "each selected Result arm must release the carrier once: {release_blocks:?}"
    );
    assert!(
        !block_reaches_before_owner_redefinition(
            function,
            release_blocks[0],
            release_blocks[1],
            owner,
        ) && !block_reaches_before_owner_redefinition(
            function,
            release_blocks[1],
            release_blocks[0],
            owner,
        ),
        "call-carrier release blocks must be mutually exclusive within one owner generation: \
         {release_blocks:?}"
    );
    let elaborated = p
        .elaborated_mir
        .iter()
        .find(|function| function.name == fn_name)
        .expect("elaborated function");
    for release_block in &release_blocks {
        assert!(
            elaborated.drop_plans.iter().all(|(exit, plan)| {
                !matches!(exit, ExitPath::Goto { block, .. } if block == release_block)
                    || plan.drops.iter().all(|drop| drop.place != place)
            }),
            "inline call-carrier cleanup must not be duplicated by bb{release_block}'s Goto plan"
        );
    }
    (owner, place, release_blocks)
}

fn has_enum_release_pair(block: &hew_mir::BasicBlock, owner: OwnerId, place: Place) -> bool {
    block.instructions.windows(2).any(|pair| {
        matches!(
            pair,
            [
                Instr::Drop {
                    place: drop_place,
                    drop_fn: Some(DropFnSpec::InPlace(InPlaceReleaseKind::Enum)),
                    ..
                },
                Instr::OwnershipEvent(OwnershipEvent::Release {
                    owner: release_owner,
                    place: release_place,
                })
            ] if *drop_place == place && *release_owner == owner && *release_place == place
        )
    })
}

fn assert_returned_payload_carrier_cleanup(p: &IrPipeline) {
    let poll = p
        .checked_mir
        .iter()
        .find(|function| function.name == "poll")
        .expect("checked poll");
    let (poll_owner, poll_place) = call_carrier_owner(poll);
    assert!(
        poll.blocks.iter().any(|block| {
            matches!(block.terminator, hew_mir::Terminator::Return)
                && has_enum_release_pair(block, poll_owner, poll_place)
        }),
        "the direct-call early-return path keeps its existing exact carrier cleanup"
    );

    let poll_let = p
        .checked_mir
        .iter()
        .find(|function| function.name == "poll_let")
        .expect("checked poll_let");
    let (result_owner, result_place) = binding_owner(poll_let, "result");
    let release_blocks = poll_let
        .blocks
        .iter()
        .filter(|block| has_enum_release_pair(block, result_owner, result_place))
        .collect::<Vec<_>>();
    assert_eq!(
        release_blocks.len(),
        2,
        "the early-return and continuing/failure paths must each retire result exactly once"
    );
    let return_release = release_blocks
        .iter()
        .find(|block| matches!(block.terminator, hew_mir::Terminator::Return))
        .expect("early-return result release");
    let neutralize = return_release
        .instructions
        .iter()
        .position(|instruction| {
            matches!(
                instruction,
                Instr::NeutralizePayloadSlot {
                    place: Place::MachineVariant { local, .. },
                    ..
                } if result_place == Place::Local(*local)
            )
        })
        .expect("selected payload neutralize");
    let release = return_release
        .instructions
        .iter()
        .position(|instruction| {
            matches!(
                instruction,
                Instr::OwnershipEvent(OwnershipEvent::Release { owner, place })
                    if *owner == result_owner && *place == result_place
            )
        })
        .expect("logical result release");
    assert!(
        release > neutralize,
        "the carrier release must follow the selected payload transfer"
    );
    let continuing_release = release_blocks
        .iter()
        .find(|block| !matches!(block.terminator, hew_mir::Terminator::Return))
        .expect("continuing/failure result release");
    assert!(
        continuing_release
            .instructions
            .iter()
            .any(|instruction| matches!(
                instruction,
                Instr::OwnershipEvent(OwnershipEvent::ScopeExit { owners, .. })
                    if owners.iter().filter(|owner| **owner == result_owner).count() == 1
            )),
        "the existing failure/continuation cleanup must remain claimed by its scope exit"
    );
}

/// The #2429 headline shape: a `Result<bytes, string>` returned from a call and
/// consumed directly by a `match` inside a `while` loop. The scrutinee temp
/// must earn one `EnumInPlace` release on each selected arm before the loop
/// back-edge — the paths that previously leaked one payload per iteration.
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
            .Ok(b) => { let n = b.len(); }
            .Err(e) => {}
        }
        i = i + 1;
    }
}
"#,
    );
    inline_call_carrier_cleanup(&p, "main");
}

/// A selected payload arm with `continue` seals the current call-result
/// carrier before the loop reuses its physical result slot. The escape scan
/// must stop at that exact non-carrying lexical close: a later iteration's
/// payload operations are a new lifetime, not an escape of the prior owner.
#[test]
fn from_call_payload_continue_loop_releases_before_result_slot_reuse() {
    let p = pipeline_with_tc(
        r#"
fn f() -> Result<bytes, string> {
    Ok("payload".to_bytes())
}

fn main() {
    var i = 0;
    while i < 4 {
        i = i + 1;
        match f() {
            .Ok(b) => {
                if b.len() == 7 && i == 2 {
                    continue;
                }
            }
            .Err(e) => {}
        }
    }
}
"#,
    );
    assert!(
        p.diagnostics.is_empty(),
        "each iteration must end its synthetic call-result generation before the next static Mint: {:?}",
        p.diagnostics
    );

    let main = p
        .raw_mir
        .iter()
        .find(|function| function.name == "main")
        .expect("raw main");
    let carrier_owner = main
        .blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .find_map(|instruction| match instruction {
            Instr::OwnershipEvent(hew_mir::OwnershipEvent::Mint {
                owner,
                ty:
                    hew_types::ResolvedTy::Named {
                        builtin: Some(hew_types::BuiltinType::Result),
                        ..
                    },
                ..
            }) => Some(*owner),
            _ => None,
        })
        .expect("synthetic Result call carrier Mint");
    assert!(
        main.blocks
            .iter()
            .flat_map(|block| &block.instructions)
            .any(|instruction| matches!(
                instruction,
                Instr::OwnershipEvent(hew_mir::OwnershipEvent::Release { owner, .. })
                    if *owner == carrier_owner
            )),
        "the selected-arm paths must publish an exact Release for the call carrier"
    );
}

/// Straight-line variant: a single un-looped `match f() { … }` releases the
/// scrutinee temp on each selected arm (the shape that leaked one payload even
/// without a loop).
#[test]
fn from_call_bytes_scrutinee_single_gets_return_enum_in_place_drop() {
    let p = pipeline_with_tc(
        r#"
fn f() -> Result<bytes, string> {
    Ok("payload".to_bytes())
}

fn main() {
    match f() {
        .Ok(b) => { let n = b.len(); }
        .Err(e) => {}
    }
}
"#,
    );
    inline_call_carrier_cleanup(&p, "main");
}

/// String payloads ride the same seam: the from-call `Result<string, string>`
/// scrutinee (previously released only when let-bound) earns the selected-arm
/// releases in the unbound shape too.
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
            .Ok(b) => { let n = b.len(); }
            .Err(e) => {}
        }
        i = i + 1;
    }
}
"#,
    );
    inline_call_carrier_cleanup(&p, "main");
}

/// Moving a payload binder into an early return transfers only the selected
/// payload slot. The direct-call carrier still owns its shell and every
/// remaining slot, so that same return edge must release the neutralized
/// carrier rather than treating the partial transfer as a whole-owner escape.
#[test]
fn returned_payload_binder_releases_remaining_call_carrier_on_early_return() {
    let p = pipeline_with_tc(
        r#"
type Snap { label: string, terminal: bool }

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
    assert_returned_payload_carrier_cleanup(&p);
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
        .Ok(b) => { let n = b.len(); }
        .Err(e) => {}
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
        .Ok(b) => { let n = b.len(); }
        .Err(e) => {}
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
        .Ok(text) => text,
        .Err(error) => problem_message(error),
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
    inline_call_carrier_cleanup(&p, "main");
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
        .Ok(value) => value,
        .Err(_) => return 1,
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

/// A projected string field is retained by codegen before it is returned. When
/// the record binder is still backed by an owned enum carrier, that carrier —
/// not the byte-copy binder — remains responsible for both original fields.
#[test]
fn vec_clone_match_field_return_defers_original_fields_to_carrier() {
    let p = pipeline_with_tc(
        r#"
type Secret { value: string, kind: string }
enum CredErr { Missing(string); Denied(string) }

fn resolve(n: i64) -> Result<Secret, CredErr> {
    if n == 0 { return Err(CredErr.Missing("missing".to_upper())); }
    Ok(Secret { value: "secret".to_upper(), kind: "api".to_upper() })
}

fn collect(n: i64) -> string {
    let bag: Vec<Secret> = [];
    let secret = match resolve(n) {
        .Err(_) => return "error",
        .Ok(value) => value,
    };
    bag.push(secret);
    let first = bag.get(0);
    match first {
        .Some(found) => found.value,
        .None => "empty",
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
    let inline_field_drops = collect
        .blocks
        .iter()
        .flat_map(|block| block.instructions.iter())
        .filter(|instruction| matches!(instruction, Instr::FieldDropInPlace { .. }))
        .count();
    assert_eq!(
        inline_field_drops, 0,
        "the carrier's terminal enum drop owns both original record fields; an inline \
         binder-field release would double-release the same carrier storage"
    );
}

/// Moving a payload into an outer binding neutralizes its source slot. The
/// carrier therefore keeps its per-arm inline release for the shell and sibling
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
            .Ok(b) => { carry = b; }
            .Err(e) => {}
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
        .checked_mir
        .iter()
        .find(|function| function.name == "main")
        .expect("checked main");
    let (_, carrier_place, release_blocks) = inline_call_carrier_cleanup(&p, "main");
    let Place::Local(carrier_local) = carrier_place else {
        panic!("call carrier must use a local place: {carrier_place:?}");
    };
    let escaped_arm = main
        .blocks
        .iter()
        .find(|block| {
            block.instructions.iter().any(|instruction| {
                matches!(
                    instruction,
                    Instr::NeutralizePayloadSlot {
                        place: Place::MachineVariant { local, .. },
                        transferee: Some(_),
                        ..
                    } if *local == carrier_local
                )
            })
        })
        .expect("escaped payload arm");
    let (neutralize, transferee) = escaped_arm
        .instructions
        .iter()
        .enumerate()
        .find_map(|(index, instruction)| match instruction {
            Instr::NeutralizePayloadSlot {
                place: Place::MachineVariant { local, .. },
                transferee: Some(transferee),
                ..
            } if *local == carrier_local => Some((index, *transferee)),
            _ => None,
        })
        .expect("carrier payload neutralize");
    let transfer = escaped_arm.instructions.iter().position(|instruction| {
        matches!(
                instruction,
                Instr::OwnershipEvent(OwnershipEvent::Transfer {
                    from,
                    to: None,
                    to_owner: None,
                    ..
                }) if *from == transferee
        )
    });
    let carrier_drop = escaped_arm.instructions.iter().position(|instruction| {
        matches!(
            instruction,
            Instr::Drop {
                place: candidate,
                drop_fn: Some(DropFnSpec::InPlace(InPlaceReleaseKind::Enum)),
                ..
            } if *candidate == carrier_place
        )
    });
    assert!(
        matches!((transfer, carrier_drop), (Some(transfer), Some(drop)) if neutralize < transfer && transfer < drop)
            && release_blocks.contains(&escaped_arm.id),
        "payload escape must precede the exact inline carrier cleanup: {:#?}",
        escaped_arm.instructions
    );
}

/// A payload may itself contain a carrier whose selected leaf escapes. The
/// nested match must retain an independent owner for the escaped string. The
/// selected outer payload is already transferred into a `RecordInPlace` owner,
/// so suppressing that parent cleanup would leak its other fields; the inner
/// string copy instead needs one balanced retain before entering the result.
#[test]
fn nested_record_payload_escape_neutralizes_original_call_carrier() {
    let p = pipeline_with_tc(
        r#"
type Slot { generation: i64, value: Option<string> }

fn clone_slot() -> Option<Slot> {
    Some(Slot { generation: 1, value: Some("payload".to_upper()) })
}

fn take() -> Option<string> {
    match clone_slot() {
        .Some(slot) => match slot.value {
            .Some(value) => Some(value),
            .None => None,
        },
        .None => None,
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
    let escaped_retain_and_store = take
        .blocks
        .iter()
        .find_map(|block| {
            block.instructions.windows(2).find_map(|pair| match pair {
                [Instr::StringRetain {
                    value,
                    condition: hew_mir::StringRetainCondition::Always,
                }, Instr::Move {
                    dest: Place::MachineVariant { .. },
                    src,
                }] if value == src => Some(*src),
                _ => None,
            })
        })
        .expect("the escaped projected string must retain before result ingress");
    assert!(matches!(escaped_retain_and_store, Place::Local(_)));
    assert!(
        take.blocks
            .iter()
            .flat_map(|block| &block.instructions)
            .all(|instruction| !matches!(instruction, Instr::AggregateProjectionNeutralize { .. })),
        "the borrowed record projection must not gain destructive neutralization authority"
    );
    assert!(
        p.elaborated_mir
            .iter()
            .find(|function| function.name == "take")
            .expect("elaborated fn take")
            .drop_plans
            .iter()
            .flat_map(|(_, plan)| &plan.drops)
            .any(|drop| matches!(drop.kind, DropKind::RecordInPlace)),
        "the parent Slot owner must retain its structural cleanup"
    );
}
