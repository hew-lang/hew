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

use hew_mir::{DropKind, IrPipeline, MirStatement};
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
        .flat_map(|(_, plan)| plan.drops.iter())
        .filter(|drop| matches!(drop.kind, DropKind::EnumInPlace))
        .count()
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

/// One scope-exit owner, expressed as drop-plan entries. Everything under test
/// runs inside a bounded loop, matching the round-5 and round-6 pins: the loop
/// body's exit edges each carry the plan, so ONE owner over a two-arm match
/// shows up as this many entries and a withheld one as zero. Measured against
/// the domestic control in this file, which is the same carrier with nothing
/// foreign in it.
const LOOP_EXIT_RELEASES: usize = 6;

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
    assert_eq!(
        enum_in_place_drops(&p, "main"),
        LOOP_EXIT_RELEASES,
        "and EXACTLY one release balances it — zero is the leak this fixes, two \
         is the double close over-correcting produces"
    );
}

/// The CONSUME polarity of the admission: an arm that closes the payload
/// itself (`Loaded(h) => h.close()`) hands the payload off through a
/// `NeutralizePayloadSlot`, after which the shell's `EnumInPlace` drop would
/// run the record close a SECOND time over the zeroed slot (a record close is
/// user code behind the still-set tag — it is not null-safe the way a
/// string/bytes/opaque drop step is). The neutralize scan must exclude the
/// whole candidate: the arm's close is the sole release, the shell drops
/// nothing, and the `Failed(string)` sibling leaks fail-closed.
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
    assert_eq!(
        enum_in_place_drops(&p, "main"),
        0,
        "the arm's explicit close consumed the payload; a shell drop here \
         would close the neutralized slot a second time (the S2200 class)"
    );
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
    assert_eq!(enum_in_place_drops(&p, "main"), LOOP_EXIT_RELEASES);
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
    assert_eq!(enum_in_place_drops(&p, "main"), LOOP_EXIT_RELEASES);
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
