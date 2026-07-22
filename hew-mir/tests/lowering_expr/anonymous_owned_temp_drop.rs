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
            0,
            "{fn_name} transfers the prepared carrier without another retain"
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
            0,
            "{fn_name} transfers the prepared carrier without another retain"
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
    let m: HashMap<i64, string> = HashMap::new();
    m.insert(1, "payload".to_upper());
    m.remove(1);
}

fn wildcard_remove() {
    let m: HashMap<i64, string> = HashMap::new();
    m.insert(1, "payload".to_upper());
    let _ = m.remove(1);
}

fn bare_get() {
    let m: HashMap<i64, string> = HashMap::new();
    m.insert(1, "payload".to_upper());
    m.get(1);
}

fn scalar_control() {
    let m: HashMap<i64, i64> = HashMap::new();
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
        assert_eq!(
            enum_drops(&p, fn_name, |exit| matches!(exit, ExitPath::Return { .. })).len(),
            1,
            "{fn_name} must release the discarded owned Option exactly once"
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
fn escaping_while_let_payload_keeps_composite_fail_closed() {
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
    assert!(
        drops.is_empty(),
        "a payload moved into a surviving outer binding must keep the composite excluded; \
         got {drops:?}"
    );
}
