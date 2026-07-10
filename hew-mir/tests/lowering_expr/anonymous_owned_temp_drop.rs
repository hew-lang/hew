//! Anonymous caller-owned result and while-let scrutinee drop regressions.

use hew_mir::{DropKind, ElabDrop, ExitPath, IrPipeline, MirStatement};
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
