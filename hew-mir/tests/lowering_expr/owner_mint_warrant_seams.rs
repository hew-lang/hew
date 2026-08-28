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

use hew_mir::{
    DropFnSpec, DropKind, DumpStage, ElaboratedMirFunction, ExitPath, InPlaceReleaseKind, Instr,
    IrPipeline, MirStatement, NeutralizeAuthority, OwnerId, OwnershipEvent, Place, RawMirFunction,
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
        .filter(|(exit, _)| !matches!(exit, ExitPath::Unwind { .. }))
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
        .filter(|(exit, _)| !matches!(exit, ExitPath::Unwind { .. }))
        .flat_map(|(_, plan)| plan.drops.iter())
        .filter(|drop| matches!(drop.kind, DropKind::CowHeap { .. }))
        .count()
}

fn binding_named(function: &RawMirFunction, name: &str) -> hew_hir::BindingId {
    function
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
        .unwrap_or_else(|| panic!("binding {name} must be present"))
}

fn unique_owner_acquisition(function: &RawMirFunction, name: &str) -> (OwnerId, Place) {
    let binding = binding_named(function, name);
    let publications = function
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
        .collect::<Vec<_>>();
    assert_eq!(
        publications.len(),
        1,
        "domestic binding {name} must acquire exactly one owner: {publications:?}"
    );
    publications[0]
}

fn inline_in_place_drop_release_count(
    function: &RawMirFunction,
    owner: OwnerId,
    place: Place,
    kind: InPlaceReleaseKind,
) -> usize {
    function
        .blocks
        .iter()
        .filter(|block| {
            let drop_index = block.instructions.iter().position(|instruction| {
                matches!(
                    instruction,
                    Instr::Drop {
                        place: dropped,
                        drop_fn: Some(DropFnSpec::InPlace(actual_kind)),
                        ..
                    } if *dropped == place && *actual_kind == kind
                )
            });
            let release_index = block.instructions.iter().position(|instruction| {
                matches!(
                    instruction,
                    Instr::OwnershipEvent(OwnershipEvent::Release {
                        owner: released_owner,
                        place: released_place,
                    }) if *released_owner == owner && *released_place == place
                )
            });
            matches!((drop_index, release_index), (Some(drop), Some(release)) if drop < release)
        })
        .count()
}

fn assert_typed_recipe(function: &RawMirFunction, owner: OwnerId, expected: DropKind) {
    let recipes = function
        .blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instruction| match instruction {
            Instr::OwnershipEvent(OwnershipEvent::DropRecipe {
                owner: recipe_owner,
                recipe,
            }) if *recipe_owner == owner => Some(recipe.kind),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(recipes, vec![expected]);
}

fn drop_paths(function: &ElaboratedMirFunction, place: Place, kind: DropKind) -> Vec<&ExitPath> {
    function
        .drop_plans
        .iter()
        .filter(|(_, plan)| {
            plan.drops
                .iter()
                .any(|drop| drop.place == place && drop.kind == kind)
        })
        .map(|(exit, _)| exit)
        .collect()
}

fn assert_shell_plan_paths(paths: &[&ExitPath], expected_cancel: usize, expected_panic: usize) {
    assert_eq!(
        paths
            .iter()
            .filter(|exit| matches!(exit, ExitPath::Cancel { .. }))
            .count(),
        expected_cancel
    );
    assert_eq!(
        paths
            .iter()
            .filter(|exit| matches!(exit, ExitPath::Panic { .. }))
            .count(),
        expected_panic
    );
    assert_eq!(
        paths
            .iter()
            .filter(|exit| matches!(exit, ExitPath::Unwind { .. }))
            .count(),
        4
    );
    assert_eq!(
        paths.len(),
        expected_cancel + expected_panic + 4,
        "the shell must appear only on its live exceptional paths"
    );
}

fn assert_payload_plan_paths(paths: &[&ExitPath]) {
    assert_eq!(paths.len(), 4);
    assert!(
        paths
            .iter()
            .all(|exit| matches!(exit, ExitPath::Unwind { .. })),
        "the payload binder's remaining plans are exactly its four unwind paths: {paths:?}"
    );
}

fn assert_domestic_payload_release_paths(
    p: &IrPipeline,
    expected_shell_cancel: usize,
    expected_shell_panic: usize,
) {
    let raw = p
        .raw_mir
        .iter()
        .find(|function| function.name == "main")
        .expect("main raw MIR");
    let (shell_owner, shell_place) = unique_owner_acquisition(raw, "b");
    let (payload_owner, payload_place) = unique_owner_acquisition(raw, "h");

    assert_typed_recipe(raw, shell_owner, DropKind::EnumInPlace);
    assert_typed_recipe(raw, payload_owner, DropKind::RecordInPlace);
    assert_eq!(
        inline_in_place_drop_release_count(raw, shell_owner, shell_place, InPlaceReleaseKind::Enum,),
        1,
        "normal loop fallthrough releases the shell exactly once"
    );
    assert_eq!(
        inline_in_place_drop_release_count(
            raw,
            payload_owner,
            payload_place,
            InPlaceReleaseKind::Record,
        ),
        1,
        "the selected arm releases its payload binder exactly once"
    );

    let payload_transfers = raw
        .blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter(|instruction| {
            matches!(
                instruction,
                Instr::NeutralizePayloadSlot {
                    transferee: Some(transferee),
                    authority: NeutralizeAuthority::PayloadBindingTransfer,
                    ..
                } if *transferee == payload_place
            )
        })
        .count();
    assert_eq!(
        payload_transfers, 1,
        "the payload projection must transfer into its domestic binder exactly once"
    );

    let elaborated = p
        .elaborated_mir
        .iter()
        .find(|function| function.name == "main")
        .expect("main elaborated MIR");
    let shell_paths = drop_paths(elaborated, shell_place, DropKind::EnumInPlace);
    assert_shell_plan_paths(&shell_paths, expected_shell_cancel, expected_shell_panic);
    let payload_paths = drop_paths(elaborated, payload_place, DropKind::RecordInPlace);
    assert_payload_plan_paths(&payload_paths);
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
type Holder { label: string }
type Wrapper { inner: Holder }
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
// The TRANSITIVE capture: a closure inside a generator body capturing a binding
// of the ENCLOSING frame. The value has no local slot here — it is loaded out
// of the generator's own environment by `ClosureEnvFieldLoad` — so the field
// init records `source_binding = None`.
//
// Two independent questions live on that path, and conflating them was the bug:
//   * can this frame CONSUME an owner (is there a slot)  -> gates `OwnsMoved`
//   * what is the value's PROVENANCE (the ledger)        -> gates the share
// ---------------------------------------------------------------------------

/// `make` binds `mk(0)`, the `gen` block captures it, and a closure inside the
/// generator body captures it AGAIN. `run` forces that inner closure across a
/// call boundary so the checker classifies it as escaping and its env is
/// heap-boxed; a stack env is `BorrowsOnly` by construction and would not
/// discriminate.
fn transitive_gen(mk: &str, capture: &str) -> IrPipeline {
    pipeline_with_tc(&format!(
        "{PRELUDE}{RUN}{mk}\n\
         fn make() -> Generator<i64, ()> {{\n    \
         let h = mk(0);\n    gen {{\n        yield run({capture} h.label.len());\n    }}\n}}\n\
         fn main() -> i64 {{\n    \
         for value in make() {{\n        println(f\"x={{value}}\");\n    }}\n    0\n}}\n"
    ))
}

/// The ownership manifest as it reaches `MakeClosure` — the operand codegen
/// hands to the heap-box free thunk. `ClosureEnvInit` deciding correctly is
/// only half the story; the verdict has to travel to the instruction that
/// synthesises the environment's release authority.
fn make_closure_env_ownership(p: &IrPipeline) -> Vec<String> {
    hew_mir::dump_mir(p, DumpStage::Raw)
        .lines()
        .filter_map(|line| {
            let idx = line.find("make_closure ")?;
            let own = line[idx..].split("env_own=[").nth(1)?;
            Some(own.trim_end_matches(']').to_string())
        })
        .filter(|own| !own.is_empty())
        .collect()
}

/// The per-field verdicts `ClosureEnvInit` recorded, in field order.
fn closure_env_init_ownership(p: &IrPipeline) -> Vec<String> {
    hew_mir::dump_mir(p, DumpStage::Raw)
        .lines()
        .filter(|line| line.contains("closure_env_init"))
        .flat_map(|line| {
            line.split("own=")
                .skip(1)
                .map(|rest| {
                    rest.split_whitespace()
                        .next()
                        .unwrap_or_default()
                        .to_string()
                })
                .collect::<Vec<_>>()
        })
        .collect()
}

/// Generator environments that own a captured field, read off the raw MIR's
/// `make_generator` field plan. This is the counterparty to every assertion
/// below: the closure env taking nothing is only correct because the generator
/// env is still there, owning the field.
fn generator_env_owned_fields(p: &IrPipeline) -> usize {
    hew_mir::dump_mir(p, DumpStage::Raw)
        .matches("fields=[owned(")
        .count()
}

/// THE FINDING behind the consumable-source gate. `OwnsMoved` is a TRANSFER:
/// consume the source binding's owner, and install the env destructor as the
/// release authority. A capture with no local slot cannot do the first half —
/// there is no `Use` statement to write against a binding with no slot — while
/// the generator env goes on owning the field regardless. Taking the second
/// half alone does not transfer anything; it mints a SECOND owner of one value.
///
/// It takes an independent SHARE instead. The alternative — plain
/// `BorrowsOnly` — would be valid only while the generator is alive, and a
/// closure carrying a transitive capture can outlive its generator; the runtime
/// oracle `generator_transitive_capture_outliving_its_generator_is_not_a_use_after_free`
/// pins that end of it.
///
/// Exact counts: **0** consumed fields, **1** retained share, **1** owning
/// generator env — two independent owners, each released exactly once.
#[test]
fn a_transitive_move_capture_takes_a_share_not_the_enclosing_envs_owner() {
    let p = transitive_gen(DOMESTIC_MK, "move ||");
    assert_eq!(
        own_moved_env_fields(&p),
        0,
        "a `move` capture read out of the enclosing generator env has no owner \
         here to consume, so it must not claim one: before the gate this was 1, \
         alongside the generator env's own owner for the same field"
    );
    assert_eq!(
        retained_share_env_fields(&p),
        1,
        "it mints its own balancing share instead — `string_share_sink_places` \
         keys off this verdict, not off the capture mode"
    );
    assert_eq!(
        generator_env_owned_fields(&p),
        1,
        "and the generator env keeps its own owner: two owners, two releases"
    );
}

/// The control: the identically shaped capture with a local slot in the frame
/// that materializes it. Here `OwnsMoved` can emit its consuming `Use`, so the
/// transfer is real and the env destructor legitimately becomes the release
/// authority. This is what stops the assertion above from being satisfiable by
/// refusing every `move` capture outright.
#[test]
fn a_direct_move_capture_of_a_domestic_binding_still_owns_it() {
    let p = pipeline_with_tc(&format!(
        "{PRELUDE}{RUN}{DOMESTIC_MK}\n\
         fn feed() -> i64 {{\n    let h = mk(0);\n    run(move || h.label.len())\n}}\n\
         fn main() -> i64 {{\n    println(f\"x={{feed()}}\");\n    0\n}}\n"
    ));
    assert_eq!(
        own_moved_env_fields(&p),
        1,
        "the gate is about the availability of a consumable source, not about \
         `move` captures in general"
    );
}

/// The ledger question, which the gate above does NOT answer. A checker-`Borrow`
/// capture consumes nothing, so the consumable-source gate does not apply to it;
/// it takes a RETAINED SHARE instead. Minting a share means a retain now and a
/// release from the env free thunk later — on a handle a declared, non-audited
/// `extern` still owns, that is a refcount operation against foreign memory.
///
/// Keying the proven-foreign query off `source_binding` answered "not foreign"
/// for every transitively captured field and minted the share anyway. The query
/// keys off the captured binding, which is a valid ledger key regardless of slot
/// residency, and the parent's ledger is cloned into every child builder so a
/// nested body sees the fact.
#[test]
fn a_transitive_borrow_capture_of_a_proven_foreign_binding_mints_no_share() {
    let p = transitive_gen(FOREIGN_MK, "||");
    assert_eq!(
        retained_share_env_fields(&p),
        0,
        "a read-only capture of a proven-foreign binding must not retain: \
         before the ledger was keyed by binding this was 1"
    );
    assert_eq!(
        own_moved_env_fields(&p),
        0,
        "and it must not consume either"
    );
}

/// The domestic control for the share: same shape, same slot-less capture, and
/// the share is still minted — so the withhold above is provenance-directed
/// rather than a blanket refusal on the transitive path.
#[test]
fn a_transitive_borrow_capture_of_a_domestic_binding_mints_a_share() {
    let p = transitive_gen(DOMESTIC_MK, "||");
    assert_eq!(
        retained_share_env_fields(&p),
        1,
        "a domestic read-only capture retains its own share, which the env free \
         thunk releases independently of the generator env's owner"
    );
}

/// Reassignment: `var h` bound from a foreign call and then overwritten with a
/// domestic value, and the mirror image. The proven-foreign ledger is monotone
/// per function — nothing retracts the fact — so its answer for `h` is STALE
/// after the store.
///
/// On the transitive path that staleness is structurally moot: the verdict is
/// decided by the consumable-source gate, which depends on slot residency and
/// not on provenance at all. Both reassignment directions therefore land on the
/// same answer, and neither can mint a second owner of the generator env's
/// field.
#[test]
fn a_reassigned_binding_captured_through_an_enclosing_env_takes_no_owner() {
    for (name, mk_def, second) in [
        (
            "foreign then domestic",
            FOREIGN_MK,
            "Holder { label: \"d\" + \"omestic\" }",
        ),
        (
            "domestic then foreign",
            DOMESTIC_MK,
            "unsafe { host_record() }",
        ),
    ] {
        let p = pipeline_with_tc(&format!(
            "{PRELUDE}{RUN}{mk_def}\n\
             fn make() -> Generator<i64, ()> {{\n    \
             var h = mk(0);\n    h = {second};\n    \
             gen {{\n        yield run(move || h.label.len());\n    }}\n}}\n\
             fn main() -> i64 {{\n    \
             for value in make() {{\n        println(f\"x={{value}}\");\n    }}\n    0\n}}\n"
        ));
        assert_eq!(
            own_moved_env_fields(&p),
            0,
            "{name}: a reassigned binding captured through the generator env \
             must not claim an owner the frame cannot consume"
        );
        assert_eq!(
            generator_env_owned_fields(&p),
            1,
            "{name}: the generator env remains the field's single owner"
        );
        // The verdict must REACH the instruction that synthesises the free
        // thunk. Codegen derived that thunk's drop set from the env record's
        // field types until this manifest was threaded through `MakeClosure`,
        // so it released a field the environment did not own. Assert the
        // verdicts are the SAME object at both instructions rather than a
        // literal: the two reassignment directions land on different verdicts
        // (the ledger is monotone, so `foreign then domestic` still reads
        // foreign and takes a bare alias), and what this pins is that whatever
        // was decided is what codegen receives.
        assert_eq!(
            make_closure_env_ownership(&p),
            closure_env_init_ownership(&p),
            "{name}: the manifest must travel to `MakeClosure`, not stop at \
             `ClosureEnvInit`"
        );
    }
}

/// An ESCAPING environment that can neither own the field nor take a share of
/// it is REFUSED, not silently left aliasing storage the enclosing environment
/// releases at its own destruction.
///
/// `bytes` has no whole-value retain authority, so the share promotion that
/// rescues a string-tree capture does not apply. The remaining options are a
/// second owner or a dangling alias — the checker admits yielding this closure
/// out of the `gen` body, so the alias really can be called after the generator
/// is destroyed. Neither is emitted.
#[test]
fn an_escaping_capture_that_can_be_neither_owned_nor_shared_is_refused() {
    let p = pipeline_with_tc(
        r"record Blob { payload: bytes }

fn mk() -> Blob {
    let b: bytes = bytes.new();
    b.push(7);
    Blob { payload: b }
}

fn make() -> Generator<fn() -> i64, ()> {
    let h = mk();
    gen {
        yield move || h.payload.len();
    }
}

fn main() -> i64 {
    var kept: fn() -> i64 = || 0;
    for f in make() {
        kept = f;
    }
    kept()
}
",
    );
    assert!(
        p.diagnostics.iter().any(|d| matches!(
            d.kind,
            hew_mir::MirDiagnosticKind::EscapingCaptureAliasesEnclosingEnv { .. }
        )),
        "a non-retainable capture read out of an enclosing env, in an escaping \
         closure, must fail closed: {:#?}",
        p.diagnostics
    );
}

/// The control: the SAME shape with a retainable field is admitted, because the
/// environment can take an independent share of it. The refusal is directed at
/// the absence of a retain authority, not at transitive captures in general.
#[test]
fn an_escaping_capture_with_a_retainable_field_is_admitted() {
    let p = transitive_gen(DOMESTIC_MK, "move ||");
    assert!(
        !p.diagnostics.iter().any(|d| matches!(
            d.kind,
            hew_mir::MirDiagnosticKind::EscapingCaptureAliasesEnclosingEnv { .. }
        )),
        "a string-tree capture takes a share and must not be refused: {:#?}",
        p.diagnostics
    );
}

// ---------------------------------------------------------------------------
// U1 — pattern payload binders
// ---------------------------------------------------------------------------

const MATCH_BODY: &str = "let b = Boxed.Full(mk(i));\n        \
     match b { Boxed.Full(h) => { let n = h.label.len(); println(f\"x={n}\"); } \
     Boxed.Empty => {} }";
const IF_LET_BODY: &str = "let b = Boxed.Full(mk(i));\n        \
     if let Boxed.Full(h) = b { let n = h.label.len(); println(f\"x={n}\"); }";

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

/// The domestic control: the payload binder and its enum shell retain one owner
/// each. Normal cleanup is inline; live unwind, panic, and cancel edges retain
/// their typed plans.
#[test]
fn a_match_payload_binder_over_a_domestic_scrutinee_keeps_its_releases() {
    let p = in_loop(DOMESTIC_MK, MATCH_BODY);
    assert_domestic_payload_release_paths(&p, 2, 2);
}

/// The same foreign withholding for the `if let` payload binder, which is a
/// separate mint site in `control_flow.rs` and therefore a separate warrant.
#[test]
fn an_if_let_payload_binder_over_a_proven_foreign_scrutinee_mints_no_owner() {
    let p = in_loop(FOREIGN_MK, IF_LET_BODY);
    assert_eq!(record_in_place_drops(&p, "main"), 0);
    assert_eq!(cow_heap_drops(&p, "main"), 0);
}

#[test]
fn an_if_let_payload_binder_over_a_domestic_scrutinee_keeps_its_releases() {
    let p = in_loop(DOMESTIC_MK, IF_LET_BODY);
    assert_domestic_payload_release_paths(&p, 1, 1);
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
             match b {{ Boxed.Full(h) => h, Boxed.Empty => Holder {{ label: \"e\" }} }} }}"
        ),
        "let b = Boxed.Full(mk(i));\n        let h = takes(b);\n        \
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
             match b {{ Boxed.Full(h) => h, Boxed.Empty => Holder {{ label: \"e\" }} }} }}"
        ),
        "let b = Boxed.Full(mk(i));\n        let h = takes(b);\n        \
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
