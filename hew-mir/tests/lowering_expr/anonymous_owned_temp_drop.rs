//! Anonymous caller-owned result and while-let scrutinee drop regressions.

use hew_mir::{DropKind, ElabDrop, ExitPath, Instr, IrPipeline, MirStatement, Terminator};
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

fn synthetic_binds(p: &IrPipeline, fn_name: &str, name: &str) -> usize {
    p.raw_mir
        .iter()
        .find(|f| f.name == fn_name)
        .unwrap_or_else(|| panic!("function {fn_name} must be present"))
        .blocks
        .iter()
        .flat_map(|b| b.statements.iter())
        .filter(|stmt| matches!(stmt, MirStatement::Bind { name: n, .. } if n == name))
        .count()
}

fn enum_drops(p: &IrPipeline, fn_name: &str, pred: impl Fn(&ExitPath) -> bool) -> Vec<ElabDrop> {
    p.elaborated_mir
        .iter()
        .find(|f| f.name == fn_name)
        .unwrap_or_else(|| panic!("function {fn_name} must be present"))
        .drop_plans
        .iter()
        .filter(|(exit, _)| pred(exit))
        .flat_map(|(_, plan)| plan.drops.iter())
        .filter(|drop| matches!(drop.kind, DropKind::EnumInPlace))
        .cloned()
        .collect()
}

fn inline_enum_drops(p: &IrPipeline, fn_name: &str) -> usize {
    p.raw_mir
        .iter()
        .find(|f| f.name == fn_name)
        .unwrap_or_else(|| panic!("function {fn_name} must be present"))
        .blocks
        .iter()
        .flat_map(|block| block.instructions.iter())
        .filter(|instr| {
            matches!(
                instr,
                Instr::Drop {
                    drop_fn: Some(hew_mir::DropFnSpec::InPlace(
                        hew_mir::InPlaceReleaseKind::Enum
                    )),
                    ..
                }
            )
        })
        .count()
}

fn record_drops(p: &IrPipeline, fn_name: &str, pred: impl Fn(&ExitPath) -> bool) -> Vec<ElabDrop> {
    p.elaborated_mir
        .iter()
        .find(|f| f.name == fn_name)
        .unwrap_or_else(|| panic!("function {fn_name} must be present"))
        .drop_plans
        .iter()
        .filter(|(exit, _)| pred(exit))
        .flat_map(|(_, plan)| plan.drops.iter())
        .filter(|drop| matches!(drop.kind, DropKind::RecordInPlace))
        .cloned()
        .collect()
}

fn tuple_drops(p: &IrPipeline, fn_name: &str, pred: impl Fn(&ExitPath) -> bool) -> Vec<ElabDrop> {
    p.elaborated_mir
        .iter()
        .find(|f| f.name == fn_name)
        .unwrap_or_else(|| panic!("function {fn_name} must be present"))
        .drop_plans
        .iter()
        .filter(|(exit, _)| pred(exit))
        .flat_map(|(_, plan)| plan.drops.iter())
        .filter(|drop| matches!(drop.kind, DropKind::TupleInPlace))
        .cloned()
        .collect()
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

fn string_retain_count(p: &IrPipeline, fn_name: &str) -> usize {
    p.raw_mir
        .iter()
        .find(|f| f.name == fn_name)
        .unwrap_or_else(|| panic!("function {fn_name} must be present"))
        .blocks
        .iter()
        .flat_map(|block| block.instructions.iter())
        .filter(|instr| matches!(instr, Instr::StringRetain { .. }))
        .count()
}

fn assert_record_param_embed_mints(p: &IrPipeline) {
    for (fn_name, copy_symbol, move_symbol) in [
        ("pushParam", "hew_vec_push_owned", "hew_vec_push_owned_move"),
        ("setParam", "hew_vec_set_owned", "hew_vec_set_owned_move"),
    ] {
        assert_eq!(
            string_retain_count(p, fn_name),
            1,
            "{fn_name} embeds a borrowed string param: the copy-in temp mints \
             exactly one +1 retain (the caller keeps its own count)"
        );
        assert_eq!(call_count(p, fn_name, copy_symbol), 1);
        assert_eq!(call_count(p, fn_name, move_symbol), 0);
        assert_eq!(synthetic_binds(p, fn_name, "__hew_copy_in_param_temp"), 1);
        assert_eq!(
            record_drops(p, fn_name, |exit| matches!(exit, ExitPath::Return { .. })).len(),
            1,
            "{fn_name} must drop the prepared source-temp carrier exactly once"
        );
    }
}

fn assert_tuple_param_embed_mints(p: &IrPipeline) {
    for (fn_name, copy_symbol, move_symbol) in [
        (
            "tuplePushParam",
            "hew_vec_push_owned",
            "hew_vec_push_owned_move",
        ),
        (
            "tupleSetParam",
            "hew_vec_set_owned",
            "hew_vec_set_owned_move",
        ),
    ] {
        assert_eq!(
            string_retain_count(p, fn_name),
            1,
            "{fn_name} embeds a borrowed string param: the copy-in temp mints \
             exactly one +1 retain (the caller keeps its own count)"
        );
        assert_eq!(call_count(p, fn_name, copy_symbol), 1);
        assert_eq!(call_count(p, fn_name, move_symbol), 0);
        assert_eq!(synthetic_binds(p, fn_name, "__hew_copy_in_param_temp"), 1);
        assert_eq!(
            tuple_drops(p, fn_name, |exit| matches!(exit, ExitPath::Return { .. })).len(),
            1,
            "{fn_name} must drop the prepared tuple source-temp carrier exactly once"
        );
    }
}

fn assert_deep_param_embeds_use_prepared_carriers(p: &IrPipeline) {
    for fn_name in ["unsupported", "unsupportedProjection"] {
        assert_eq!(
            synthetic_binds(p, fn_name, "__hew_copy_in_param_temp"),
            1,
            "{fn_name} must mint one owner for its prepared carrier aggregate"
        );
        assert_eq!(
            call_count(p, fn_name, "hew_vec_push_owned"),
            1,
            "{fn_name} keeps COPY-IN with one balancing source-temp owner"
        );
        assert_eq!(
            record_drops(p, fn_name, |exit| matches!(exit, ExitPath::Return { .. })).len(),
            1,
            "{fn_name} must release the prepared source temp exactly once"
        );
    }
}

#[test]
fn vec_copy_in_param_temps_own_only_their_prepared_carriers() {
    let p = pipeline_with_tc(
        r#"
type Holder { items: Vec<string> }
type Wrap { f: Option<string> }
type HolderWrap { f: Option<Holder> }
type MixedWrap { s: string, items: Vec<string> }

fn pushParam(p: string) {
    let v: Vec<Wrap> = [];
    v.push(Wrap { f: Some(p) });
}

fn setParam(p: string) {
    let v: Vec<Wrap> = [];
    v.set(0, Wrap { f: Some(p) });
}

fn tuplePushParam(p: string) {
    let v: Vec<(string, i64)> = [];
    v.push((p, 1));
}

fn tupleSetParam(p: string) {
    let v: Vec<(string, i64)> = [];
    v.set(0, (p, 1));
}

fn boundFirst(p: string) {
    let v: Vec<Wrap> = [];
    let w = Wrap { f: Some(p) };
    v.push(w);
}

fn freshMove() {
    let v: Vec<Wrap> = [];
    v.push(Wrap { f: Some("item".to_upper()) });
}

fn unsupported(p: Holder) {
    let v: Vec<HolderWrap> = [];
    v.push(HolderWrap { f: Some(p) });
}

fn unsupportedProjection(p: string, h: Holder) {
    let v: Vec<MixedWrap> = [];
    v.push(MixedWrap { s: p, items: h.items });
}
"#,
    );

    assert_record_param_embed_mints(&p);
    assert_tuple_param_embed_mints(&p);

    assert_eq!(
        synthetic_binds(&p, "boundFirst", "__hew_copy_in_param_temp"),
        0,
        "a named source already has its ordinary owner"
    );
    assert_eq!(call_count(&p, "freshMove", "hew_vec_push_owned_move"), 1);
    assert_eq!(
        synthetic_binds(&p, "freshMove", "__hew_copy_in_param_temp"),
        0,
        "a no-parameter fresh owner must stay on MOVE-IN"
    );
    assert_deep_param_embeds_use_prepared_carriers(&p);
}

#[test]
fn discarded_owned_hashmap_results_get_one_synthetic_owner() {
    let p = pipeline_with_tc(
        r#"
fn bare_remove() {
    let m: HashMap<i64, string> = HashMap.new();
    m.insert(1, "payload".to_upper());
    m.remove(1);
}

fn wildcard_remove() {
    let m: HashMap<i64, string> = HashMap.new();
    m.insert(1, "payload".to_upper());
    let _ = m.remove(1);
}

fn bare_get() {
    let m: HashMap<i64, string> = HashMap.new();
    m.insert(1, "payload".to_upper());
    m.get(1);
}

fn scalar_control() {
    let m: HashMap<i64, i64> = HashMap.new();
    m.insert(1, 7);
    m.remove(1);
}
"#,
    );

    for fn_name in ["bare_remove", "wildcard_remove", "bare_get"] {
        assert_eq!(
            synthetic_binds(&p, fn_name, "__hew_discarded_call_result"),
            1,
            "{fn_name} must expose exactly one synthetic owner in raw MIR"
        );
        assert_eq!(inline_enum_drops(&p, fn_name), 1);
        assert!(
            enum_drops(&p, fn_name, |exit| matches!(exit, ExitPath::Return { .. })).is_empty(),
            "{fn_name} transfers its publication owner into the immediate discard, so no \
             second scope-exit release may remain"
        );
    }

    assert_eq!(
        synthetic_binds(&p, "scalar_control", "__hew_discarded_call_result"),
        0,
        "Option<i64> owns no heap and must not gain a synthetic drop owner"
    );
    assert!(
        enum_drops(&p, "scalar_control", |_| true).is_empty(),
        "the scalar control must not emit a bogus enum payload drop"
    );
}

#[test]
fn from_call_while_let_releases_each_iteration_and_final_false_scrutinee() {
    let p = pipeline_with_tc(
        r#"
fn next(i: i64, cap: i64) -> Result<string, string> {
    if i < cap {
        Ok("payload".to_upper())
    } else {
        Err("done".to_upper())
    }
}

fn run(cap: i64) -> i64 {
    var i = 0;
    var total = 0;
    while let Ok(value) = next(i, cap) {
        total = total + value.len();
        i = i + 1;
    }
    total
}
"#,
    );

    assert_eq!(
        synthetic_binds(&p, "run", "__hew_call_scrutinee"),
        1,
        "while-let must reuse the from-call synthetic scrutinee owner"
    );
    assert_eq!(
        enum_drops(&p, "run", |exit| matches!(exit, ExitPath::Goto { .. })).len(),
        2,
        "while-let needs one drop on the body back-edge and one on the final false edge"
    );
    assert!(
        enum_drops(&p, "run", |exit| matches!(exit, ExitPath::Return { .. })).is_empty(),
        "the final false edge consumes the owner, so the later function return must not drop it again"
    );
}

#[test]
fn from_call_while_let_early_return_drops_in_flight_scrutinee() {
    let p = pipeline_with_tc(
        r#"
fn next() -> Result<string, string> {
    Ok("payload".to_upper())
}

fn run() -> i64 {
    while let Ok(value) = next() {
        if value.len() > 0 {
            return 7;
        }
    }
    0
}
"#,
    );

    assert_eq!(
        enum_drops(&p, "run", |exit| matches!(exit, ExitPath::Return { .. })).len(),
        1,
        "an early return from the while-let body must release the current scrutinee exactly once"
    );
}

#[test]
fn escaping_while_let_payload_transfers_owner_and_drops_composite_shells() {
    let p = pipeline_with_tc(
        r#"
fn next() -> Result<string, string> {
    Ok("payload".to_upper())
}

fn run() -> i64 {
    var carry = "";
    while let Ok(value) = next() {
        carry = value;
        break;
    }
    carry.len()
}
"#,
    );
    let drops = enum_drops(&p, "run", |_| true);
    assert_eq!(
        drops.len(),
        3,
        "the live scrutinee must release on each reachable exit: {drops:?}"
    );
    assert!(
        drops.iter().all(|drop| drop.place == drops[0].place),
        "every exit must discharge the same composite owner: {drops:?}"
    );
    let run = p
        .raw_mir
        .iter()
        .find(|function| function.name == "run")
        .expect("raw fn run");
    assert_eq!(
        run.blocks
            .iter()
            .flat_map(|block| block.instructions.iter())
            .filter(|instr| matches!(instr, Instr::NeutralizePayloadSlot { .. }))
            .count(),
        1,
        "the escaping payload must transfer out of its composite slot exactly once"
    );
    assert_eq!(
        string_retain_count(&p, "run"),
        0,
        "the exact payload transfer must not mint a competing share"
    );
}

/// A `machine` value never enters the owned call-carrier protocol: its
/// layout registers in `machine_layouts` (codegen's enum-layout lookup for
/// snapshot free synthesis fails closed on it) and machines pass BY VALUE
/// with the caller keeping an independent copy. The by-value machine param
/// must lower with no terminal snapshot drop and no slot neutralization —
/// the guide's "drive a machine through a free-function parameter" fence.
#[test]
fn machine_param_stays_off_the_carrier_protocol() {
    let p = pipeline_with_tc(
        r"
machine Door {
    events { Open; Close; }
    state Shut;
    state Ajar { angle: i64; }
    on Open: Shut => Ajar { Ajar { angle: 90 } }
    on Close: Ajar => Shut { Shut }
    default { state }
}
fn drive(d: Door) -> string {
    var local = d;
    local.step(Open);
    local.state_name()
}
fn main() { println(drive(Door.Shut)); }
",
    );
    assert!(
        p.diagnostics.is_empty(),
        "MIR diagnostics: {:#?}",
        p.diagnostics
    );
    for fn_name in ["drive", "main"] {
        let func = p
            .raw_mir
            .iter()
            .find(|f| f.name == fn_name)
            .unwrap_or_else(|| panic!("raw fn {fn_name}"));
        let carrier_instrs: Vec<&Instr> = func
            .blocks
            .iter()
            .flat_map(|b| b.instructions.iter())
            .filter(|i| {
                matches!(
                    i,
                    Instr::ValueSnapshotDrop { .. }
                        | Instr::ValueSnapshotClone { .. }
                        | Instr::NeutralizePayloadSlot { .. }
                )
            })
            .collect();
        assert!(
            carrier_instrs.is_empty(),
            "{fn_name} must not run machine values through the carrier \
             protocol; got {carrier_instrs:?}"
        );
    }
}

/// Sibling `if` arms start from the same owned-carrier authority. Lowering the
/// first arm must not consume that compiler fact before the second arm is
/// visited, otherwise the second returned Vec aliases a parameter that the
/// terminal carrier drop has already freed.
#[test]
fn carrier_param_if_arms_each_neutralize_whole_slot() {
    let p = pipeline_with_tc(
        r"
enum Slot { Filled(i64); Empty; }

fn step(items: Vec<string>, i: i64) -> (Vec<string>, Slot) {
    if i < 1 {
        (items, Filled(i))
    } else {
        (items, Empty)
    }
}

fn main() -> i64 {
    let items: Vec<string> = Vec.new();
    step(items, 1).0.len()
}
",
    );
    assert!(
        p.diagnostics.is_empty(),
        "MIR diagnostics: {:#?}",
        p.diagnostics
    );
    let step = p
        .raw_mir
        .iter()
        .find(|f| f.name == "step")
        .expect("raw fn step");
    let neutralized_param_slots: Vec<_> = step
        .blocks
        .iter()
        .flat_map(|block| block.instructions.iter())
        .filter_map(|instr| match instr {
            Instr::NeutralizePayloadSlot {
                place: hew_mir::Place::Local(0),
                transferee: Some(_),
                ..
            } => Some(()),
            _ => None,
        })
        .collect();
    assert_eq!(
        neutralized_param_slots.len(),
        2,
        "both mutually-exclusive return arms must transfer the Vec parameter"
    );
    assert_eq!(
        step.blocks
            .iter()
            .flat_map(|block| block.instructions.iter())
            .filter(|instr| matches!(
                instr,
                Instr::ValueSnapshotDrop {
                    value: hew_mir::Place::Local(0),
                    ..
                }
            ))
            .count(),
        1,
        "one terminal drop remains and observes a null slot on either returned arm"
    );
}

/// A divergent arm does not reach the `if` join. Its transfer must not consume
/// the authority used by the reachable sibling and the function's later tail.
#[test]
fn divergent_if_arm_preserves_reachable_carrier_authority() {
    let p = pipeline_with_tc(
        r"
fn choose(items: Vec<string>, early: bool) -> Vec<string> {
    if early {
        return items;
    }
    items
}

fn main() -> i64 {
    let items: Vec<string> = Vec.new();
    choose(items, false).len()
}
",
    );
    assert!(
        p.diagnostics.is_empty(),
        "MIR diagnostics: {:#?}",
        p.diagnostics
    );
    let choose = p
        .raw_mir
        .iter()
        .find(|f| f.name == "choose")
        .expect("raw fn choose");
    assert_eq!(
        choose
            .blocks
            .iter()
            .flat_map(|block| block.instructions.iter())
            .filter(|instr| matches!(
                instr,
                Instr::NeutralizePayloadSlot {
                    place: hew_mir::Place::Local(0),
                    transferee: Some(_),
                    ..
                }
            ))
            .count(),
        2,
        "the early return and reachable tail each need their own path-local transfer"
    );
}

/// The same path fact applies to ordered match arms. A transfer in one body
/// cannot suppress the mutually-exclusive body selected by the next predicate.
#[test]
fn carrier_param_match_arms_each_neutralize_whole_slot() {
    let p = pipeline_with_tc(
        r"
fn choose(items: Vec<string>, tag: i64) -> Vec<string> {
    match tag {
        0 => items,
        _ => items,
    }
}

fn main() -> i64 {
    let items: Vec<string> = Vec.new();
    choose(items, 1).len()
}
",
    );
    assert!(
        p.diagnostics.is_empty(),
        "MIR diagnostics: {:#?}",
        p.diagnostics
    );
    let choose = p
        .raw_mir
        .iter()
        .find(|f| f.name == "choose")
        .expect("raw fn choose");
    assert_eq!(
        choose
            .blocks
            .iter()
            .flat_map(|block| block.instructions.iter())
            .filter(|instr| matches!(
                instr,
                Instr::NeutralizePayloadSlot {
                    place: hew_mir::Place::Local(0),
                    transferee: Some(_),
                    ..
                }
            ))
            .count(),
        2,
        "both ordered match bodies start with the parameter's transfer authority"
    );
}

/// An alias-returning composite callee's result must stay `Borrowed` — never
/// minted as a caller-side `__hew_temp_arg` owner — on EVERY compile.
///
/// `getself(w)` hands back `w.h`, an interior alias of the still-live `w`;
/// `w`'s own scope-exit drop is the exactly-once release. The removed
/// `Borrowed → Owned { Retained }` user-call upgrade minted a SECOND owner
/// over the same storage (no runtime retain behind it — a double-free), and
/// whether it fired depended on `HashMap` fixpoint pass order, so the mint
/// flipped per compiler process. Eight pipelines make the pre-fix flip
/// reliably observable; the invariant is zero mints on all of them.
#[test]
fn getfield_alias_return_result_never_mints_temp_arg_owner() {
    const SOURCE: &str = "
type Holder { s: string }
type Wrap { h: Holder }
fn borrowLen(h: Holder) -> i64 { h.s.len() }
fn getself(w: Wrap) -> Holder { w.h }
fn main() -> i64 {
    let w: Wrap = Wrap { h: Holder { s: \"a\" + \"b\" } };
    borrowLen(getself(w))
}
";
    for run in 0..8 {
        let p = pipeline_with_tc(SOURCE);
        assert!(
            p.diagnostics.is_empty(),
            "run {run}: MIR diagnostics: {:#?}",
            p.diagnostics
        );
        assert_eq!(
            synthetic_binds(&p, "main", "__hew_temp_arg"),
            0,
            "run {run}: the borrowed-alias call result aliases `w.h`; a minted \
             owner would double-free against `w`'s own scope-exit drop"
        );
        let record_drops = record_drops(&p, "main", |_| true);
        assert!(
            record_drops
                .iter()
                .all(|drop| matches!(drop.ty, hew_types::ResolvedTy::Named { ref name, .. } if name == "Wrap")),
            "run {run}: only `w: Wrap` may carry a record drop; a Holder drop \
             is the minted alias double-discharge: {record_drops:#?}"
        );
    }
}
