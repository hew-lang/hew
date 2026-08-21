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
//! `NeutralizePayloadSlot` carrying
//! `NeutralizeAuthority::DivergentSelectionTransfer`, which zeroes the source
//! slot. Ownership is then unambiguous per path — the source keeps its ordinary
//! scope-exit release, which walks a nulled slot (null-tolerant no-op) where it
//! transferred and frees where it did not.
//!
//! These assertions are platform-independent and run on every host: the
//! ownership invariant is stated as "which instruction is emitted where" and as
//! "which drops the return exit plans", neither of which needs an allocator
//! inspector. The macOS leak-slope oracle in
//! `hew-cli/tests/divergent_selection_arm_ownership_oracle.rs` measures the
//! same shapes empirically.
//!
//! The NEGATIVE CONTROLS are load-bearing. A rewrite that fired on every
//! whole-local `Move` would neutralize ordinary straight-line rebinds (changing
//! the `#2418` conditional-move machinery underneath it) and would null a
//! source that is still READ after the selection, turning a leak into a
//! use-after-move fault. Both must stay untouched.

use hew_mir::{lower_hir_module, ElaboratedMirFunction, ExitPath, Instr, IrPipeline, Place};
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;

fn pipeline(source: &str) -> IrPipeline {
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
    let pl = lower_hir_module(&output.module);
    assert!(
        pl.diagnostics.is_empty(),
        "MIR diagnostics: {:?}",
        pl.diagnostics
    );
    pl
}

const MAKE_VEC: &str = "fn make() -> Vec<i64> {\n\
     \x20   let v: Vec<i64> = Vec.new();\n\
     \x20   v.push(40);\n\
     \x20   v.push(2);\n\
     \x20   return v;\n\
     }\n\n";

/// Every `(source_local, transferee_local)` pair neutralized under the
/// divergent-selection authority in `fn_name`, each verified to sit directly
/// after the whole-local `Move` it pays for. A neutralize that is NOT paired
/// with its move is not counted: the alias, escape, hand-off and returned-flow
/// scans all key on that adjacency, so an unpaired site would be invisible to
/// them.
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
    let transfers = paired_selection_transfers(&pl, "probe");
    assert_eq!(
        transfers.len(),
        2,
        "each arm of a two-arm value selection transfers one owned local into the join slot, so \
         both sources must be neutralized; got {transfers:?}"
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

/// ADMIT: with both arm sources neutralized, the return exit plans a release
/// for each of them AND for the join binding — three owners, three releases,
/// each exactly-once at runtime because two of them walk a nulled slot on the
/// path that transferred.
///
/// Pre-fix this exit planned exactly ONE drop (the join binding), which is the
/// leak stated structurally.
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
    let dropped = return_exit_drop_places(&pl, "probe");
    assert_eq!(
        dropped.len(),
        3,
        "`a`, `b` and `out` are three separately-minted Vec owners; the return exit must plan a \
         release for each or whichever arm lost the selection leaks its allocation; got \
         {dropped:?}"
    );
}

/// ADMIT: the `if`/`else` value form. Its lowering copies each arm's value
/// through a per-arm temp before the join slot, so the join slot's direct
/// sources are temps — the rewrite has to walk back through that single-writer
/// copy chain to the owned local, or the shape stays leaking.
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
    let transfers = paired_selection_transfers(&pl, "probe");
    assert_eq!(
        transfers.len(),
        2,
        "the `if`/`else` value form selects between two owned locals exactly as `match` does; \
         got {transfers:?}"
    );
    assert_eq!(
        return_exit_drop_places(&pl, "probe").len(),
        3,
        "same three owners as the `match` form"
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
    let transfers = paired_selection_transfers(&pl, "probe");
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
#[test]
fn source_read_after_the_selection_is_not_neutralized() {
    let pl = pipeline(&format!(
        "{MAKE_VEC}fn probe(c: bool) -> i64 {{\n\
         \x20   let a = make();\n\
         \x20   let b = make();\n\
         \x20   let out = match c {{ true => a, false => b }};\n\
         \x20   out.len() + a.len()\n\
         }}\n\n\
         fn main() -> i64 {{ probe(true) }}\n"
    ));
    let a_local = binding_local(&pl, "probe", "a");
    let neutralized: Vec<u32> = paired_selection_transfers(&pl, "probe")
        .into_iter()
        .map(|(source, _)| source)
        .collect();
    assert!(
        !neutralized.contains(&a_local),
        "`a` is read after the selection, so its slot must keep its bits; neutralized {neutralized:?} \
         includes _{a_local}"
    );
}

/// The MIR local backing a named binding of `fn_name`.
fn binding_local(pl: &IrPipeline, fn_name: &str, binding: &str) -> u32 {
    let func = pl
        .raw_mir
        .iter()
        .find(|f| f.name == fn_name)
        .unwrap_or_else(|| panic!("no MIR function named `{fn_name}`"));
    let index = func
        .local_names
        .iter()
        .position(|name| name.as_deref() == Some(binding))
        .unwrap_or_else(|| panic!("no local named `{binding}` in `{fn_name}`"));
    u32::try_from(index).expect("local index fits u32")
}
