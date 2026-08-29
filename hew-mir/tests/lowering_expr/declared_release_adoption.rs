//! The adoption boundary: a construction whose release the PROGRAM declared.
//!
//! The provenance ledger used to have exactly two answers for a value —
//! domestic/fresh (mintable) and ownership-OPAQUE foreign (never mintable) —
//! and the composite rule propagated the second answer from a field to its
//! container. That rule's stated premise is that every composite release in
//! this compiler is recursive and generated from the container's LAYOUT, so
//! there is no drop plan that frees the container's spine while sparing a
//! field.
//!
//! That premise is FALSE for a `#[resource]` record with a declared `close`.
//! `IrPipeline::lifecycle_registry` makes codegen's
//! `__hew_record_drop_inplace_<R>` thunk call `<R>::close(self)` FIRST and only
//! then tear the fields down field-wise. The declared destructor is exactly the
//! per-value drop plan the composite rule assumed did not exist, so the answer
//! for such a construction is neither of the two the ledger could spell: its
//! ORIGIN is foreign, but its RELEASE is declared. Constructing one is the
//! program taking delivery — the same adoption the ABI already performs for a
//! root `extern "C" -> string` result.
//!
//! These cases pin the boundary from both sides. The admitted shape must mint
//! and release EXACTLY once; each of the three admission clauses must have a
//! counterfactual that still refuses, so the rule cannot be satisfied by
//! "`#[resource]` is exempt".

use hew_mir::{
    CheckedMirFunction, CowHeapRelease, DropKind, ExitPath, Instr, IrPipeline, MirStatement,
    OwnerId, OwnershipEvent, Place, Terminator, TrapKind,
};
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;

const SYNTHETIC_TEMP_ARG: &str = "__hew_call_scrutinee";

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

fn scrutinee_binds(p: &IrPipeline, fn_name: &str) -> usize {
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

fn enum_in_place_drops(p: &IrPipeline, fn_name: &str) -> usize {
    p.elaborated_mir
        .iter()
        .find(|f| f.name == fn_name)
        .unwrap_or_else(|| panic!("function {fn_name} must be present"))
        .drop_plans
        .iter()
        .filter(|(exit, _)| !matches!(exit, ExitPath::Unwind { .. }))
        .flat_map(|(_, plan)| plan.drops.iter())
        .filter(|drop| matches!(drop.kind, DropKind::EnumInPlace))
        .count()
}

fn owner_for_binding(function: &CheckedMirFunction, binding_name: &str) -> OwnerId {
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
    let mints: Vec<OwnerId> = function
        .blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instruction| match instruction {
            Instr::OwnershipEvent(OwnershipEvent::Mint { owner, .. })
                if owner.binding == binding =>
            {
                Some(*owner)
            }
            _ => None,
        })
        .collect();
    assert_eq!(
        mints.len(),
        1,
        "{binding_name} must publish exactly one owner generation"
    );
    mints[0]
}

fn owner_definition_place(function: &CheckedMirFunction, owner: OwnerId) -> Place {
    function
        .blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .find_map(|instruction| match instruction {
            Instr::OwnershipEvent(OwnershipEvent::Mint {
                owner: candidate,
                place,
                ..
            }) if *candidate == owner => Some(*place),
            _ => None,
        })
        .expect("owner must have one definition-site place")
}

fn exit_has_drop(
    p: &IrPipeline,
    fn_name: &str,
    exit: &ExitPath,
    place: Place,
    kind: DropKind,
) -> bool {
    p.elaborated_mir
        .iter()
        .find(|function| function.name == fn_name)
        .unwrap_or_else(|| panic!("function {fn_name} must be present"))
        .drop_plans
        .iter()
        .find(|(candidate, _)| candidate == exit)
        .is_some_and(|(_, plan)| {
            plan.drops
                .iter()
                .any(|drop| drop.place == place && drop.kind == kind)
        })
}

/// Assert one canonical owner generation and recipe for a named binding, then
/// return every block that releases it through the matching lexical `ScopeExit`.
/// Static release sites may be mutually exclusive match arms; unlike summing
/// exit plans, this follows the exact Checked-MIR authority that elaboration
/// replays on each path.
fn owner_lifecycle(
    p: &IrPipeline,
    fn_name: &str,
    binding_name: &str,
    expected_kind: DropKind,
) -> Vec<u32> {
    let function = p
        .checked_mir
        .iter()
        .find(|f| f.name == fn_name)
        .unwrap_or_else(|| panic!("function {fn_name} must be present"));
    let owner = owner_for_binding(function, binding_name);
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
        vec![&expected_kind],
        "{binding_name} must carry exactly one {expected_kind:?} recipe and no competing kind"
    );

    function
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
                            ..
                        }) if *candidate == owner
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
                "{binding_name} must release at most once in block {}: {:#?}",
                block.id,
                block.instructions
            );
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
                matches!(scope_exits.as_slice(), [scope_exit] if releases[0] < *scope_exit),
                "{binding_name} release in block {} must be ratified by its later ScopeExit: {:#?}",
                block.id,
                block.instructions
            );
            Some(block.id)
        })
        .collect()
}

fn assert_declared_close_handoff(
    p: &IrPipeline,
    function: &CheckedMirFunction,
    parent: OwnerId,
    parent_place: Place,
    child: OwnerId,
) {
    let close_block = function
        .blocks
        .iter()
        .find(|block| {
            matches!(
                &block.terminator,
                Terminator::Call { callee, .. } if callee == "Handle::close"
            )
        })
        .expect("Loaded arm must call the declared close");
    let close_next = match &close_block.terminator {
        Terminator::Call { next, .. } => *next,
        _ => unreachable!("block selected by call terminator"),
    };
    let close_success = function
        .blocks
        .iter()
        .find(|block| block.id == close_next)
        .expect("close normal successor must exist");
    let transfer_index = |owner| {
        let indices = close_success
            .instructions
            .iter()
            .enumerate()
            .filter_map(|(index, instruction)| {
                matches!(
                    instruction,
                    Instr::OwnershipEvent(OwnershipEvent::Transfer {
                        owner: candidate,
                        to: None,
                        ..
                    }) if *candidate == owner
                )
                .then_some(index)
            })
            .collect::<Vec<_>>();
        assert_eq!(
            indices.len(),
            1,
            "owner {owner:?} must transfer exactly once"
        );
        indices[0]
    };
    let child_transfer = transfer_index(child);
    let parent_transfer = transfer_index(parent);
    assert!(
        child_transfer < parent_transfer,
        "the child close must commit before its parent shell retires: {:#?}",
        close_success.instructions
    );
    assert!(
        close_block.instructions.iter().all(|instruction| !matches!(
            instruction,
            Instr::OwnershipEvent(OwnershipEvent::Transfer {
                owner: candidate,
                ..
            }) if *candidate == parent
        )),
        "the pre-call edge must keep the shell owner for early failure"
    );
    assert!(
        exit_has_drop(
            p,
            "main",
            &ExitPath::Unwind {
                block: close_block.id,
                callee: "Handle::close".to_string(),
            },
            parent_place,
            DropKind::EnumInPlace,
        ),
        "an unwind before the close commits must retain shell cleanup"
    );
    assert!(
        !exit_has_drop(
            p,
            "main",
            &ExitPath::Cancel { block: close_next },
            parent_place,
            DropKind::EnumInPlace,
        ),
        "cancellation after a successful close must not close the shell again"
    );
}

fn assert_nonconsuming_shell_paths(
    p: &IrPipeline,
    function: &CheckedMirFunction,
    parent_place: Place,
) {
    let failed_call = function
        .blocks
        .iter()
        .find(|block| {
            matches!(
                &block.terminator,
                Terminator::Call { callee, .. } if callee == "println_str"
            )
        })
        .expect("Failed arm must call println");
    let failed_next = match &failed_call.terminator {
        Terminator::Call { next, .. } => *next,
        _ => unreachable!("block selected by call terminator"),
    };
    // Unwinding out of the println leaves the shell owned: the arm has not
    // reached its release yet, so the exit must clean it up.
    let unwind = ExitPath::Unwind {
        block: failed_call.id,
        callee: "println_str".to_string(),
    };
    assert!(
        exit_has_drop(p, "main", &unwind, parent_place, DropKind::EnumInPlace),
        "the non-consuming Failed path must retain shell cleanup at {unwind:?}"
    );
    // The Loaded arm consumes the shell and the Failed arm does not, so the
    // shell is conditionally consumed at their join. Lowering releases it
    // inline on the Failed edge (before the loop back-edge, where the next
    // iteration mints a fresh generation over the same place), and the
    // back-edge cancellation checkpoint runs after that release — dropping
    // there again would double-free.
    let back_edge_cancel = ExitPath::Cancel { block: failed_next };
    assert!(
        !exit_has_drop(
            p,
            "main",
            &back_edge_cancel,
            parent_place,
            DropKind::EnumInPlace
        ),
        "cancellation after the Failed edge's inline shell release must not close it again"
    );
    let invalid_tag = function
        .blocks
        .iter()
        .find(|block| {
            matches!(
                block.terminator,
                Terminator::Trap {
                    kind: TrapKind::ExhaustivenessFallthrough
                }
            )
        })
        .expect("match must retain its invalid-tag trap");
    assert!(
        exit_has_drop(
            p,
            "main",
            &ExitPath::Panic {
                block: invalid_tag.id,
            },
            parent_place,
            DropKind::EnumInPlace,
        ),
        "the invalid-tag path must retain the pre-selection shell owner"
    );
    let overflow = function
        .blocks
        .iter()
        .find(|block| {
            matches!(
                block.terminator,
                Terminator::Trap {
                    kind: TrapKind::IntegerOverflow
                }
            )
        })
        .expect("loop increment must retain its overflow trap");
    assert!(
        !exit_has_drop(
            p,
            "main",
            &ExitPath::Panic { block: overflow.id },
            parent_place,
            DropKind::EnumInPlace,
        ),
        "a post-close join cannot revive the retired parent generation"
    );
}

/// Every fixture shares the same host handle and the same enum carrier, so the
/// only variable is the DECLARED SHAPE of the payload type — which admission
/// clause it satisfies or fails.
const PRELUDE: &str = r#"#[opaque]
type Dq {}
extern "C" {
    fn host_new() -> Dq;
    fn host_free(consume d: Dq);
}
"#;

/// Everything under test runs inside a bounded loop, matching the round-5 and
/// round-6 pins: a loop body has three exit edges, so a single scope-exit owner
/// shows up as drop-plan entries on each and a withheld one as zero.
fn program(defs: &str) -> IrPipeline {
    pipeline_with_tc(&format!(
        "{PRELUDE}{defs}\n\
         fn main() -> i64 {{\n    var i: i64 = 0;\n    \
         while i < 2 {{\n        \
         match make() {{ Outcome.Loaded(h) => {{ println(\"a\"); }} \
         Outcome.Failed(n) => {{ println(n); }} }}\n        \
         i = i + 1;\n    }}\n    0\n}}\n"
    ))
}

/// The regression, at the mint site. A `#[resource]` record whose only field is
/// an `#[opaque]` handle, carried as an enum payload out of a Hew function: the
/// call-scrutinee owner is the SOLE release authority for that payload, because
/// the payload binder itself owns no heap (`Handle`'s only field is opaque, so
/// `ty_owns_heap` is false for it) and never reaches a binder gate at all.
///
/// Refuse the scrutinee owner and the enum's in-place release — the thunk chain
/// `__hew_enum_drop_inplace_Outcome` → `__hew_record_drop_inplace_Handle` →
/// `Handle::close` — is never scheduled, so the handle closes ZERO times.
#[test]
fn a_declared_release_payload_over_a_direct_extern_mints_once_and_releases_once() {
    let p = program(
        "#[resource]\n\
         type Handle { raw: Dq; }\n\
         impl Handle { fn close(self) { unsafe { host_free(self.raw) }; } }\n\
         enum Outcome { Loaded(Handle); Failed(string); }\n\
         fn make() -> Outcome { Outcome.Loaded(Handle { raw: unsafe { host_new() } }) }",
    );
    assert_eq!(
        scrutinee_binds(&p, "main"),
        1,
        "the construction is the program taking delivery: `Handle {{ raw: .. }}` \
         declares a release for the value it builds, so the enum carrying it is \
         a domestic owner and the scrutinee earns EXACTLY one mint"
    );
    let scrutinee_releases = owner_lifecycle(&p, "main", SYNTHETIC_TEMP_ARG, DropKind::EnumInPlace);
    assert_eq!(
        scrutinee_releases.len(),
        1,
        "the two match arms rejoin before the scrutinee's one normal release"
    );
    assert_eq!(
        owner_lifecycle(&p, "main", "h", DropKind::Resource).len(),
        1,
        "the selected declared-release payload has one typed child authority"
    );
    assert_eq!(
        owner_lifecycle(
            &p,
            "main",
            "n",
            DropKind::CowHeap {
                release: CowHeapRelease::String,
            },
        )
        .len(),
        1,
        "the alternate string payload has one typed child authority"
    );
}

/// The CONSUME polarity of the admission: an arm that closes the payload
/// itself (`Loaded(h) => h.close()`) hands the payload off through a
/// `NeutralizePayloadSlot`, after which the shell's `EnumInPlace` drop would
/// run the record close a SECOND time over the zeroed slot (a record close is
/// user code behind the still-set tag — it is not null-safe the way a
/// string/bytes/opaque drop step is). The neutralize scan must exclude the
/// selected owner on the close call's NORMAL successor: the arm's close is the
/// sole release on that path. The pre-call unwind, invalid-tag trap, and
/// non-consuming `Failed(string)` path keep their shell cleanup; a later join
/// cannot revive the parent generation retired by the successful close.
#[test]
fn a_consuming_arm_keeps_the_arm_as_the_sole_close_authority() {
    let p = pipeline_with_tc(&format!(
        "{PRELUDE}#[resource]\n\
         type Handle {{ raw: Dq; }}\n\
         impl Handle {{ fn close(self) {{ unsafe {{ host_free(self.raw) }}; }} }}\n\
         enum Outcome {{ Loaded(Handle); Failed(string); }}\n\
         fn make() -> Outcome {{ Outcome.Loaded(Handle {{ raw: unsafe {{ host_new() }} }}) }}\n\
         fn main() -> i64 {{\n    var i: i64 = 0;\n    \
         while i < 2 {{\n        \
         match make() {{ Outcome.Loaded(h) => {{ h.close(); }} \
         Outcome.Failed(n) => {{ println(n); }} }}\n        \
         i = i + 1;\n    }}\n    0\n}}\n"
    ));
    let function = p
        .checked_mir
        .iter()
        .find(|function| function.name == "main")
        .expect("main must lower");
    let parent = owner_for_binding(function, SYNTHETIC_TEMP_ARG);
    let parent_place = owner_definition_place(function, parent);
    let child = owner_for_binding(function, "h");
    assert_declared_close_handoff(&p, function, parent, parent_place, child);
    assert_nonconsuming_shell_paths(&p, function, parent_place);
}

/// CLAUSE 3 — the adoption is not "`#[resource]` is exempt". A declared field
/// the post-`close` field-wise teardown CAN free (here `log: string`, released
/// by the record thunk after `close` returns) puts the container back in the
/// two-answer world: a foreign value reaching any such field would be freed by
/// a plan the program never declared. The whole type is refused, not the one
/// field — the rule is about what the DECLARED LAYOUT can free, not about which
/// field happens to carry the foreign value.
///
/// This direction costs a leak, never a double release.
#[test]
fn a_resource_record_with_a_field_the_teardown_frees_is_still_refused() {
    let p = program(
        "#[resource]\n\
         type Handle { raw: Dq; log: string; }\n\
         impl Handle { fn close(self) { unsafe { host_free(self.raw) }; } }\n\
         enum Outcome { Loaded(Handle); Failed(string); }\n\
         fn make() -> Outcome { Outcome.Loaded(Handle { raw: unsafe { host_new() }, log: \"t\" }) }",
    );
    assert_eq!(
        scrutinee_binds(&p, "main"),
        0,
        "`log` really is torn down field-wise after `close`, so the declared \
         destructor is not the whole drop plan and the composite rule's premise \
         holds again"
    );
    assert_eq!(enum_in_place_drops(&p, "main"), 0);
}

/// CLAUSE 1 — the F2 composite refusal is untouched for a PLAIN record. The
/// container is genuinely fresh (this frame allocated it) but declares no
/// release, so its only drop plan is the recursive layout walk the composite
/// rule was written for. This is the round-5 shape that motivated the warrant
/// and it must stay refused.
#[test]
fn a_plain_record_over_a_direct_extern_is_still_refused() {
    let p = program(
        "type Handle { raw: Dq; }\n\
         enum Outcome { Loaded(Handle); Failed(string); }\n\
         fn make() -> Outcome { Outcome.Loaded(Handle { raw: unsafe { host_new() } }) }",
    );
    assert_eq!(
        scrutinee_binds(&p, "main"),
        0,
        "no `#[resource]` marker is no declared release: minting here schedules \
         a recursive release over a handle the host still owns"
    );
    assert_eq!(enum_in_place_drops(&p, "main"), 0);
}

/// The bare extern result is refused at the SAME seam, so the adoption is
/// attached to the construction and not to the enum carrier or the match.
#[test]
fn a_bare_extern_handle_payload_is_still_refused() {
    let p = program(
        "enum Outcome { Loaded(Dq); Failed(string); }\n\
         fn make() -> Outcome { Outcome.Loaded(unsafe { host_new() }) }",
    );
    assert_eq!(
        scrutinee_binds(&p, "main"),
        0,
        "nothing declared a release for a raw host handle — it is ownership-\
         OPAQUE wherever it is carried"
    );
    assert_eq!(enum_in_place_drops(&p, "main"), 0);
}

/// The adoption survives a laundering Hew frame between the extern and the
/// field, because it is answered at the CONSTRUCTION, not by the operand's
/// provenance: the operand may be as foreign as it likes once the value being
/// built declares how it is released.
#[test]
fn a_declared_release_payload_over_a_wrapper_still_mints_once() {
    let p = program(
        "#[resource]\n\
         type Handle { raw: Dq; }\n\
         impl Handle { fn close(self) { unsafe { host_free(self.raw) }; } }\n\
         enum Outcome { Loaded(Handle); Failed(string); }\n\
         fn fresh() -> Dq { unsafe { host_new() } }\n\
         fn make() -> Outcome { Outcome.Loaded(Handle { raw: fresh() }) }",
    );
    assert_eq!(scrutinee_binds(&p, "main"), 1);
    assert_eq!(
        owner_lifecycle(&p, "main", SYNTHETIC_TEMP_ARG, DropKind::EnumInPlace).len(),
        1
    );
    assert_eq!(
        owner_lifecycle(&p, "main", "h", DropKind::Resource).len(),
        1
    );
    assert_eq!(
        owner_lifecycle(
            &p,
            "main",
            "n",
            DropKind::CowHeap {
                release: CowHeapRelease::String,
            },
        )
        .len(),
        1
    );
}

/// COUNTERFACTUAL for all four refusals above: the identical carrier over a
/// wholly DOMESTIC payload still mints and still releases once. Deleting the
/// scrutinee mint outright would satisfy every `0` assertion in this file; only
/// a provenance-directed rule satisfies this one too.
#[test]
fn a_domestic_payload_keeps_its_mint_and_releases_once() {
    let p = program(
        "type Handle { label: string; }\n\
         enum Outcome { Loaded(Handle); Failed(string); }\n\
         fn make() -> Outcome { Outcome.Loaded(Handle { label: \"t\" }) }",
    );
    assert_eq!(
        scrutinee_binds(&p, "main"),
        1,
        "control: a domestic composite payload still earns its scrutinee owner"
    );
    let mut child_release_blocks = owner_lifecycle(&p, "main", "h", DropKind::RecordInPlace);
    child_release_blocks.extend(owner_lifecycle(
        &p,
        "main",
        "n",
        DropKind::CowHeap {
            release: CowHeapRelease::String,
        },
    ));
    child_release_blocks.sort_unstable();
    child_release_blocks.dedup();
    assert_eq!(
        owner_lifecycle(&p, "main", SYNTHETIC_TEMP_ARG, DropKind::EnumInPlace),
        child_release_blocks,
        "each mutually-exclusive selected payload path must release both its \
         typed child owner and the neutralized enum shell exactly once"
    );
}

/// CLAUSE 2 — the marker alone is not the declaration. A `#[resource]` type with
/// no `close` method registers as `(Resource, None)` in `type_classes`, so
/// codegen's record-drop thunk has nothing to call first and the teardown is the
/// plain recursive layout walk again. Authority and codegen read the SAME entry,
/// so they cannot disagree about whether a release was declared.
#[test]
fn a_resource_marker_without_a_close_method_is_still_refused() {
    let p = program(
        "#[resource]\n\
         type Handle { raw: Dq; }\n\
         enum Outcome { Loaded(Handle); Failed(string); }\n\
         fn make() -> Outcome { Outcome.Loaded(Handle { raw: unsafe { host_new() } }) }",
    );
    assert_eq!(
        scrutinee_binds(&p, "main"),
        0,
        "the adoption is the DECLARED close, not the marker: with no close \
         method there is no per-value drop plan to adopt under"
    );
    assert_eq!(enum_in_place_drops(&p, "main"), 0);
}
