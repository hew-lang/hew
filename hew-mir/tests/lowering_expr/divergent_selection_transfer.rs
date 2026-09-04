//! Divergent-arm value selection must leave EVERY arm source with an exact
//! owner on every path.
//!
//! `let out = match c { true => a, false => b };` writes one join slot from two
//! mutually-exclusive arms. Exactly one of `a` / `b` ends up in the join slot;
//! the other still owns its own live allocation. Before the fix the join slot
//! was reachable from two distinct whole-value alias roots, so it was evicted
//! as conflicted (`propagate_whole_value_alias_roots`) and each arm's `Move`
//! read as an ownership escape — which excluded BOTH arm sources from the
//! scope-exit drop set. The join slot's owner freed the selected value and the
//! losing arm's value leaked, once per call.
//!
//! The rewrite makes the transfer physical: the arm's `Move` is followed by a
//! `NeutralizePayloadSlot` carrying `NeutralizeAuthority::WholeCarrierConsume`,
//! then an exact `OwnershipEvent::Relocate` moves the definition owner to that
//! carrier. Each arm then releases the opposite owner and terminally hands off
//! the selected owner before the shared block mints the lexical output.
//! Ownership is therefore explicit on every path rather than inferred from a
//! legacy divergent-selection marker or deferred to path-insensitive null-slot
//! drops at Return.
//!
//! These assertions are platform-independent and run on every host: the
//! ownership invariant is stated as "which instruction is emitted where" and as
//! "which owner remains in the return exit plan", neither of which needs an allocator
//! inspector. The macOS leak-slope oracle in
//! `hew-cli/tests/divergent_selection_arm_ownership_oracle.rs` measures the
//! same shapes empirically.
//!
//! The NEGATIVE CONTROLS are load-bearing. A rewrite that fired on every
//! whole-local `Move` would neutralize ordinary straight-line rebinds (changing
//! the `#2418` conditional-move machinery underneath it). A source read after
//! a selected-arm move must instead be rejected by immutable ownership
//! validation, while an explicit clone remains valid.

use hew_mir::{
    lower_hir_module, ElaboratedMirFunction, ExitPath, Instr, IrPipeline, MirStatement,
    OwnershipEvent, Place,
};
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;

fn pipeline(source: &str) -> IrPipeline {
    let pl = pipeline_with_diagnostics(source);
    assert!(
        pl.diagnostics.is_empty(),
        "MIR diagnostics: {:?}",
        pl.diagnostics
    );
    pl
}

fn pipeline_with_diagnostics(source: &str) -> IrPipeline {
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
    lower_hir_module(&output.module)
}

const MAKE_VEC: &str = "fn make() -> Vec<i64> {\n\
     \x20   let v: Vec<i64> = Vec.new();\n\
     \x20   v.push(40);\n\
     \x20   v.push(2);\n\
     \x20   return v;\n\
     }\n\n";

/// Every `(definition_local, first_carrier_local)` pair moved under canonical
/// whole-carrier authority in `fn_name`.
///
/// The hard-cutover representation publishes three adjacent facts: the
/// physical `Move`, its `WholeCarrierConsume` neutralization, and an exact
/// `Relocate` of the owner minted at the source definition. Restricting the
/// scan to that owner's definition place excludes later arm-temp -> join-slot
/// carrier hops while still covering direct `match` and two-hop `if` lowering.
fn paired_selection_owner_relocations(pl: &IrPipeline, fn_name: &str) -> Vec<(u32, u32)> {
    let func = pl
        .raw_mir
        .iter()
        .find(|f| f.name == fn_name)
        .unwrap_or_else(|| panic!("no MIR function named `{fn_name}`"));
    let definition_places = func
        .blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instruction| match instruction {
            Instr::OwnershipEvent(hew_mir::OwnershipEvent::Mint { owner, place, .. }) => {
                Some((*owner, *place))
            }
            _ => None,
        })
        .collect::<std::collections::HashMap<_, _>>();
    let mut pairs = Vec::new();
    for block in &func.blocks {
        for (index, instr) in block.instructions.iter().enumerate() {
            let Instr::Move {
                dest: Place::Local(transferee),
                src: Place::Local(source),
            } = instr
            else {
                continue;
            };
            let Some(Instr::NeutralizePayloadSlot {
                place,
                transferee: Some(neutralized_to),
                authority: hew_mir::NeutralizeAuthority::WholeCarrierConsume,
            }) = block.instructions.get(index + 1)
            else {
                continue;
            };
            let Some(Instr::OwnershipEvent(hew_mir::OwnershipEvent::Relocate { owner, from, to })) =
                block.instructions.get(index + 2)
            else {
                continue;
            };
            if *place != Place::Local(*source)
                || *neutralized_to != Place::Local(*transferee)
                || *from != Place::Local(*source)
                || *to != Place::Local(*transferee)
                || definition_places.get(owner) != Some(from)
            {
                continue;
            }
            pairs.push((*source, *transferee));
        }
    }
    pairs
}

fn paired_selection_transfers(pl: &IrPipeline, fn_name: &str) -> Vec<(u32, u32)> {
    let func = pl
        .raw_mir
        .iter()
        .find(|f| f.name == fn_name)
        .unwrap_or_else(|| panic!("no MIR function named `{fn_name}`"));
    let mut pairs = Vec::new();
    for block in &func.blocks {
        for (index, instr) in block.instructions.iter().enumerate() {
            let Instr::NeutralizePayloadSlot {
                place: Place::Local(source),
                transferee: Some(Place::Local(transferee)),
                authority: hew_mir::NeutralizeAuthority::DivergentSelectionTransfer,
            } = instr
            else {
                continue;
            };
            let paired = index
                .checked_sub(1)
                .and_then(|previous| block.instructions.get(previous))
                .is_some_and(|previous| {
                    matches!(
                        previous,
                        Instr::Move {
                            dest: Place::Local(move_dest),
                            src: Place::Local(move_src),
                        } if move_dest == transferee && move_src == source
                    )
                });
            assert!(
                paired,
                "a DivergentSelectionTransfer neutralize of _{source} -> _{transferee} in \
                 `{fn_name}` must sit directly after the whole-local Move it pays for; every \
                 downstream ownership scan recognises the transfer by that adjacency"
            );
            pairs.push((*source, *transferee));
        }
    }
    pairs
}

fn elaborated<'a>(pl: &'a IrPipeline, fn_name: &str) -> &'a ElaboratedMirFunction {
    pl.elaborated_mir
        .iter()
        .find(|f| f.name == fn_name)
        .unwrap_or_else(|| panic!("no elaborated MIR function named `{fn_name}`"))
}

/// Distinct places dropped across every `Return` exit plan of `fn_name`.
fn return_exit_drop_places(pl: &IrPipeline, fn_name: &str) -> Vec<Place> {
    let mut places: Vec<Place> = Vec::new();
    for (exit, plan) in &elaborated(pl, fn_name).drop_plans {
        if !matches!(exit, ExitPath::Return { .. }) {
            continue;
        }
        for drop in &plan.drops {
            if !places.contains(&drop.place) {
                places.push(drop.place);
            }
        }
    }
    places
}

/// `(released losers, selected carrier)` for each branch-exact selection arm.
fn selection_arm_closures(pl: &IrPipeline, fn_name: &str) -> Vec<(Vec<Place>, Place)> {
    let func = pl
        .raw_mir
        .iter()
        .find(|f| f.name == fn_name)
        .unwrap_or_else(|| panic!("no MIR function named `{fn_name}`"));
    let mut closures = Vec::new();
    for block in &func.blocks {
        let selected = block
            .instructions
            .iter()
            .find_map(|instruction| match instruction {
                Instr::OwnershipEvent(OwnershipEvent::Transfer {
                    from,
                    to: None,
                    to_owner: None,
                    ..
                }) => Some(*from),
                _ => None,
            });
        let Some(selected) = selected else {
            continue;
        };
        let released = block
            .instructions
            .iter()
            .filter_map(|instruction| match instruction {
                Instr::OwnershipEvent(OwnershipEvent::Release { place, .. }) => Some(*place),
                _ => None,
            })
            .collect();
        closures.push((released, selected));
    }
    closures
}

fn binding_owner_place(pl: &IrPipeline, fn_name: &str, name: &str) -> (hew_mir::OwnerId, Place) {
    let func = pl
        .raw_mir
        .iter()
        .find(|f| f.name == fn_name)
        .unwrap_or_else(|| panic!("no MIR function named `{fn_name}`"));
    let binding = func
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
        .unwrap_or_else(|| panic!("no binding named `{name}` in `{fn_name}`"));
    func.blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .find_map(|instruction| match instruction {
            Instr::OwnershipEvent(OwnershipEvent::Mint { owner, place, .. })
                if owner.binding == binding =>
            {
                Some((*owner, *place))
            }
            _ => None,
        })
        .unwrap_or_else(|| panic!("no owner definition for `{name}` in `{fn_name}`"))
}

/// ADMIT: the two-arm `match` selection neutralizes BOTH arm sources.
///
/// This is the regression pin for the reported defect: 96 leaks / 6912 bytes
/// over 48 calls, one whole `Vec` per call, with no `clone` anywhere.
#[test]
fn match_arm_selection_neutralizes_every_arm_source() {
    let pl = pipeline(&format!(
        "{MAKE_VEC}fn probe(c: bool) -> i64 {{\n\
         \x20   let a = make();\n\
         \x20   let b = make();\n\
         \x20   let out = match c {{ true => a, false => b }};\n\
         \x20   out.len()\n\
         }}\n\n\
         fn main() -> i64 {{ probe(true) }}\n"
    ));
    let transfers = paired_selection_owner_relocations(&pl, "probe");
    assert_eq!(
        transfers.len(),
        2,
        "each arm of a two-arm value selection transfers one owned local into the join slot, so \
         both source owners must relocate into their physical carriers; got {transfers:?}"
    );
    let join_slots: Vec<u32> = transfers.iter().map(|(_, dest)| *dest).collect();
    assert!(
        join_slots.windows(2).all(|pair| pair[0] == pair[1]),
        "both arms write the SAME join slot; differing transferees mean the pairing found \
         unrelated moves: {transfers:?}"
    );
    let sources: Vec<u32> = transfers.iter().map(|(source, _)| *source).collect();
    assert!(
        sources[0] != sources[1],
        "the two arms must neutralize DISTINCT sources: {transfers:?}"
    );
}

/// ADMIT: each arm releases its one losing owner and terminally transfers its
/// selected owner before the shared output is minted. The return therefore
/// owns only the output; no path-insensitive pair of null-slot drops is needed.
#[test]
fn match_arm_selection_plans_a_release_for_every_owner() {
    let pl = pipeline(&format!(
        "{MAKE_VEC}fn probe(c: bool) -> i64 {{\n\
         \x20   let a = make();\n\
         \x20   let b = make();\n\
         \x20   let out = match c {{ true => a, false => b }};\n\
         \x20   out.len()\n\
         }}\n\n\
         fn main() -> i64 {{ probe(true) }}\n"
    ));
    let arm_closures = selection_arm_closures(&pl, "probe");
    assert_eq!(
        arm_closures.len(),
        2,
        "both executable selection arms must publish one exact owner closure: {arm_closures:?}"
    );
    assert!(
        arm_closures.iter().all(|(losers, _)| losers.len() == 1),
        "each arm must release exactly its one unselected owner: {arm_closures:?}"
    );
    assert_ne!(
        arm_closures[0].0, arm_closures[1].0,
        "the mutually-exclusive arms must release opposite owners: {arm_closures:?}"
    );
    assert_eq!(
        arm_closures[0].1, arm_closures[1].1,
        "both selected owners must hand off through the same join carrier: {arm_closures:?}"
    );
    let dropped = return_exit_drop_places(&pl, "probe");
    assert_eq!(
        dropped.len(),
        1,
        "the branch closures retire both source-owner identities, leaving only the lexical \
         output live at Return; got {dropped:?}"
    );
}

/// NEGATIVE CONTROL: an unrelated owner that is read after the selection is
/// not an arm participant. It must survive both branch closures and retain its
/// ordinary Return cleanup.
#[test]
fn match_arm_selection_does_not_close_an_unrelated_outer_owner() {
    let pl = pipeline(&format!(
        "{MAKE_VEC}fn probe(c: bool) -> i64 {{\n\
         \x20   let keep = make();\n\
         \x20   let a = make();\n\
         \x20   let b = make();\n\
         \x20   let out = match c {{ true => a, false => b }};\n\
         \x20   out.len() + keep.len()\n\
         }}\n\n\
         fn main() -> i64 {{ probe(false) }}\n"
    ));
    let (keep_owner, keep_place) = binding_owner_place(&pl, "probe", "keep");
    let func = pl.raw_mir.iter().find(|f| f.name == "probe").unwrap();
    assert!(func.blocks.iter().all(|block| {
        !block.instructions.iter().any(|instruction| {
            matches!(
                instruction,
                Instr::OwnershipEvent(OwnershipEvent::Release { owner, .. })
                    if *owner == keep_owner
            )
        })
    }));
    let dropped = return_exit_drop_places(&pl, "probe");
    assert!(
        dropped.contains(&keep_place),
        "the non-participating `keep` owner must survive to Return: {dropped:?}"
    );
    assert_eq!(
        selection_arm_closures(&pl, "probe")
            .iter()
            .map(|(losers, _)| losers.len())
            .collect::<Vec<_>>(),
        vec![1, 1],
        "each arm must close only the opposite selection source"
    );
}

/// ADMIT: the `if`/`else` value form. Its lowering copies each arm's value
/// through a per-arm temp before the join slot. Each definition owner first
/// relocates into its own arm temp, then that owner follows the later temp ->
/// join carrier hop. The definition-site scan must therefore still find both
/// initial owner relocations.
#[test]
fn if_else_selection_neutralizes_through_the_arm_temp() {
    let pl = pipeline(&format!(
        "{MAKE_VEC}fn probe(c: bool) -> i64 {{\n\
         \x20   let a = make();\n\
         \x20   let b = make();\n\
         \x20   let out = if c {{ a }} else {{ b }};\n\
         \x20   out.len()\n\
         }}\n\n\
         fn main() -> i64 {{ probe(true) }}\n"
    ));
    let transfers = paired_selection_owner_relocations(&pl, "probe");
    assert_eq!(
        transfers.len(),
        2,
        "the `if`/`else` value form selects between two owned locals exactly as `match` does; \
         got {transfers:?}"
    );
}

/// ADMIT: a selection whose sibling arm DIVERGES. Only one arm reaches the join
/// slot, so the shape is not a two-source merge — but the early-return exit
/// still leaves the un-transferred source owning its value, and that exit must
/// plan its release.
#[test]
fn selection_with_a_diverging_arm_releases_on_the_early_exit() {
    let pl = pipeline(&format!(
        "{MAKE_VEC}fn probe(c: bool) -> i64 {{\n\
         \x20   let a = make();\n\
         \x20   let b = make();\n\
         \x20   let out = match c {{\n\
         \x20       true => a,\n\
         \x20       false => {{ return b.len(); }}\n\
         \x20   }};\n\
         \x20   out.len()\n\
         }}\n\n\
         fn main() -> i64 {{ probe(true) }}\n"
    ));
    let transfers = paired_selection_owner_relocations(&pl, "probe");
    assert_eq!(
        transfers.len(),
        1,
        "the single non-diverging arm transfers `a` into the join slot; got {transfers:?}"
    );
    let elab = elaborated(&pl, "probe");
    let early_exit_drops: Vec<usize> = elab
        .drop_plans
        .iter()
        .filter(|(exit, _)| matches!(exit, ExitPath::Return { .. }))
        .map(|(_, plan)| plan.drops.len())
        .collect();
    assert!(
        early_exit_drops.iter().all(|count| *count >= 2),
        "both return exits own two live Vecs — the early return owns `a` and `b`, the normal \
         return owns `b` and the join binding; a plan with fewer leaks one of them: \
         {early_exit_drops:?}"
    );
}

/// NEGATIVE CONTROL: a straight-line rebind is NOT a divergent selection.
///
/// `let b = a;` at the top level of a function dominates every return, so
/// ownership hands over on all paths and the pre-existing alias machinery (and
/// the `#2418` conditional-move drop-flag family layered on it) already resolves
/// it. Neutralizing here would rewrite that machinery's inputs underneath it.
#[test]
fn unconditional_rebind_is_not_neutralized() {
    let pl = pipeline(&format!(
        "{MAKE_VEC}fn probe() -> i64 {{\n\
         \x20   let a = make();\n\
         \x20   let b = a;\n\
         \x20   b.len()\n\
         }}\n\n\
         fn main() -> i64 {{ probe() }}\n"
    ));
    assert!(
        paired_selection_transfers(&pl, "probe").is_empty(),
        "a rebind that dominates every return transfers on ALL paths; it is not an arm selection \
         and must stay byte-identical"
    );
}

/// NEGATIVE CONTROL: a source READ after the selection keeps its bits.
///
/// `out` and `a` alias the same allocation on the selected path, and reading
/// `a` afterwards is accepted today. Nulling `a` under that read would convert
/// a leak into a use-after-move fault, so the shape holds its pre-existing
/// posture until the surface decides the read is an error.
///
/// `Vec` is one of the D2-ratified retain (copy-on-write) collections, so this
/// relocation still reports — but as `CollectionCopyUnsupported`
/// (`E_LIMIT_COLLECTION_COPY`, D8), not `UseAfterConsume`: this lowering has
/// no retain path yet, and the program is legal Hew, not a use-after-move.
#[test]
fn source_read_after_move_semantics_selection_is_rejected_without_an_explicit_clone() {
    let pl = pipeline_with_diagnostics(&format!(
        "{MAKE_VEC}fn probe(c: bool) -> i64 {{\n\
         \x20   let a = make();\n\
         \x20   let b = make();\n\
         \x20   let out = match c {{ true => a, false => b }};\n\
         \x20   out.len() + a.len()\n\
         }}\n\n\
         fn main() -> i64 {{ probe(true) }}\n"
    ));
    assert!(
        pl.diagnostics.iter().any(|diagnostic| matches!(
            diagnostic.kind,
            hew_mir::MirDiagnosticKind::CollectionCopyUnsupported { .. }
        )),
        "a move-semantics collection selected into a second owner and read again without an \
         explicit clone is E_LIMIT_COLLECTION_COPY (D8), not a plain UseAfterConsume; \
         diagnostics: {:?}",
        pl.diagnostics
    );
}

/// NEGATIVE: an explicit clone leaves the source owner at its definition
/// place, so a read after the selection remains valid even though the cloned
/// result follows the same branch/join topology.
#[test]
fn source_read_after_cloned_selection_remains_valid() {
    let pl = pipeline_with_diagnostics(&format!(
        "{MAKE_VEC}fn probe(c: bool) -> i64 {{\n\
         \x20   let a = make();\n\
         \x20   let b = make();\n\
         \x20   let out = match c {{ true => a.clone(), false => b.clone() }};\n\
         \x20   out.len() + a.len()\n\
         }}\n\n\
         fn main() -> i64 {{ probe(true) }}\n"
    ));
    assert!(
        pl.diagnostics.iter().all(|diagnostic| !matches!(
            diagnostic.kind,
            hew_mir::MirDiagnosticKind::UseAfterConsume { .. }
        )),
        "cloning into the selection must not consume the later-read source: {:?}",
        pl.diagnostics
    );
}
