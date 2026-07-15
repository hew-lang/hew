use hew_hir::{
    lower_program_host_target, HirDiagnosticKind, HirExprKind, HirItem, HirMatchArmPredicate,
    HirStmtKind, ResolutionCtx,
};
use hew_types::{module_registry::ModuleRegistry, Checker, ResolvedTy};

fn checked(source: &str) -> (hew_parser::ParseResult, hew_types::TypeCheckOutput) {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&parsed.program);
    assert!(
        output.errors.is_empty(),
        "type errors: {:#?}",
        output.errors
    );
    (parsed, output)
}

fn function<'a>(output: &'a hew_hir::LowerOutput, name: &str) -> &'a hew_hir::HirFn {
    output
        .module
        .items
        .iter()
        .find_map(|item| match item {
            HirItem::Function(function) if function.name == name => Some(function),
            _ => None,
        })
        .unwrap_or_else(|| panic!("function `{name}` not found"))
}

#[test]
fn uppercase_or_leaf_binders_use_checker_resolutions() {
    let source = r"
fn classify(x: i64) -> i64 {
    match x {
        MAX | MAX => MAX,
    }
}";
    let (parsed, output) = checked(source);
    let lowered = lower_program_host_target(&parsed.program, &output, &ResolutionCtx);
    assert!(
        lowered.diagnostics.is_empty(),
        "unexpected HIR diagnostics: {:#?}",
        lowered.diagnostics
    );
    let Some(tail) = &function(&lowered, "classify").body.tail else {
        panic!("classify must have a tail expression");
    };
    let HirExprKind::Match { arms, .. } = &tail.kind else {
        panic!("expected match expression, got {:#?}", tail.kind);
    };
    assert_eq!(arms.len(), 2);
    assert!(arms.iter().all(|arm| {
        matches!(
            &arm.predicate,
            HirMatchArmPredicate::Binding { name, .. } if name == "MAX"
        )
    }));
}

#[test]
fn missing_or_leaf_resolution_fails_closed() {
    let source = r"
fn classify(x: i64) -> i64 {
    match x {
        0 | 1 => 1,
        _ => 0,
    }
}";
    let (parsed, mut output) = checked(source);
    output.pattern_resolutions.clear();
    let lowered = lower_program_host_target(&parsed.program, &output, &ResolutionCtx);
    assert!(lowered.diagnostics.iter().any(|diagnostic| {
        matches!(
            &diagnostic.kind,
            HirDiagnosticKind::CheckerBoundaryViolation { name, reason }
                if name == "match pattern"
                    && reason.contains("missing checker pattern resolution")
        )
    }));
}

#[test]
fn record_let_rest_projects_omitted_fields_as_wildcards() {
    let source = r"
type Pair {
    a: i64;
    b: i64;
}

fn main() -> i64 {
    let p = Pair { a: 1, b: 2 };
    let Pair { a, .. } = p;
    a
}";
    let (parsed, output) = checked(source);
    let lowered = lower_program_host_target(&parsed.program, &output, &ResolutionCtx);
    assert!(
        lowered.diagnostics.is_empty(),
        "unexpected HIR diagnostics: {:#?}",
        lowered.diagnostics
    );
    let statements = &function(&lowered, "main").body.statements;
    let projected: Vec<_> = statements
        .iter()
        .filter_map(|stmt| match &stmt.kind {
            HirStmtKind::Let(binding, Some(init)) => {
                let HirExprKind::FieldAccess { field, .. } = &init.kind else {
                    return None;
                };
                Some((binding.name.as_str(), field.as_str()))
            }
            _ => None,
        })
        .collect();
    assert!(projected.contains(&("a", "a")));
    assert!(
        projected
            .iter()
            .any(|(binding, field)| binding.starts_with('_') && *field == "b"),
        "rest-generated wildcard must project field b: {projected:?}"
    );
}

#[test]
fn missing_record_pattern_plan_fails_closed() {
    let source = r"
type Pair { a: i64; b: i64; }
fn main() -> i64 {
    let p = Pair { a: 1, b: 2 };
    let Pair { a, .. } = p;
    a
}";
    let (parsed, mut output) = checked(source);
    output.pattern_plans.clear();
    let lowered = lower_program_host_target(&parsed.program, &output, &ResolutionCtx);
    assert!(lowered.diagnostics.iter().any(|diagnostic| {
        matches!(
            &diagnostic.kind,
            HirDiagnosticKind::CheckerBoundaryViolation { name, reason }
                if name == "record pattern" && reason.contains("missing checker PatternPlan")
        )
    }));
}

#[test]
fn owned_record_literal_predicate_reads_pattern_plan() {
    let source = r#"
type Packet {
    tag: string;
    payload: string;
}

fn classify(packet: Packet) -> i64 {
    match packet {
        Packet { tag: "ok", .. } => 1,
        _ => 0,
    }
}"#;
    let (parsed, output) = checked(source);
    let lowered = lower_program_host_target(&parsed.program, &output, &ResolutionCtx);
    assert!(
        lowered.diagnostics.is_empty(),
        "unexpected HIR diagnostics: {:#?}",
        lowered.diagnostics
    );
    let Some(tail) = &function(&lowered, "classify").body.tail else {
        panic!("classify must have a tail expression");
    };
    let HirExprKind::Match { arms, .. } = &tail.kind else {
        panic!("expected match expression");
    };
    assert_eq!(arms[0].payload_predicates.len(), 1);
    assert_eq!(arms[0].payload_predicates[0].field_idx, 0);
    assert_eq!(arms[0].payload_predicates[0].ty, ResolvedTy::String);
}
