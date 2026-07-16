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

#[test]
fn or_pattern_struct_leaves_lower_through_the_single_producer() {
    // The single arm-resolution producer classifies struct leaves inside an
    // or-pattern using the scrutinee type, so `Point { x: 1, .. } | Point { x:
    // 2, .. }` lowers cleanly. Before the or-leaf reclassifier was deleted the
    // HIR fallback fail-closed these leaves; this pins the widening so an
    // accidental re-narrowing regresses visibly.
    let source = r"
type Point {
    x: i64;
    y: i64;
}

fn classify(p: Point) -> i64 {
    match p {
        Point { x: 1, .. } | Point { x: 2, .. } => 100,
        Point { x, .. } => x,
    }
}";
    let (parsed, output) = checked(source);
    let lowered = lower_program_host_target(&parsed.program, &output, &ResolutionCtx);
    assert!(
        lowered.diagnostics.is_empty(),
        "or-pattern struct leaves must lower without diagnostics: {:#?}",
        lowered.diagnostics
    );
    let Some(tail) = &function(&lowered, "classify").body.tail else {
        panic!("classify must have a tail expression");
    };
    let HirExprKind::Match { arms, .. } = &tail.kind else {
        panic!("expected match expression");
    };
    // The or-pattern expands to one arm per leaf plus the catch-all arm; every
    // leaf carries the literal `x` sub-predicate from the plan.
    assert!(
        arms.len() >= 3,
        "or-pattern should expand to two literal leaves plus the binding arm, got {}",
        arms.len()
    );
    let literal_leaves = arms
        .iter()
        .filter(|arm| {
            arm.payload_predicates
                .iter()
                .any(|predicate| predicate.field_idx == 0)
        })
        .count();
    assert_eq!(
        literal_leaves, 2,
        "both or-leaves must carry the x literal predicate"
    );
}

#[test]
fn nested_record_rest_projects_omitted_fields_as_wildcards() {
    // The nested record materializer reads the same PatternPlan as the
    // top-level record-let, so a nested rest (`Inner { a, .. }`) projects the
    // omitted owned field as a wildcard rather than dropping it from the field
    // list — one field-list source, no erasure-ordering divergence.
    let source = r"
type Inner {
    a: i64;
    b: i64;
}

type Outer {
    inner: Inner;
    tag: i64;
}

fn main() -> i64 {
    let o = Outer { inner: Inner { a: 1, b: 2 }, tag: 3 };
    let Outer { inner: Inner { a, .. }, tag } = o;
    a + tag
}";
    let (parsed, output) = checked(source);
    let lowered = lower_program_host_target(&parsed.program, &output, &ResolutionCtx);
    assert!(
        lowered.diagnostics.is_empty(),
        "nested record rest must lower without diagnostics: {:#?}",
        lowered.diagnostics
    );
    // The nested `b` field materializes as a `__2`-style wildcard projection
    // (a FieldAccess `let`), proving the plan's full field list is projected.
    let projected_fields: Vec<String> = function(&lowered, "main")
        .body
        .statements
        .iter()
        .filter_map(|stmt| match &stmt.kind {
            HirStmtKind::Let(_, Some(init)) => match &init.kind {
                HirExprKind::FieldAccess { field, .. } => Some(field.clone()),
                _ => None,
            },
            _ => None,
        })
        .collect();
    assert!(
        projected_fields.iter().any(|f| f == "b"),
        "nested rest must project the omitted `b` field as a wildcard; projected: {projected_fields:?}"
    );
}

#[test]
fn missing_enum_struct_plan_fails_closed_in_refutable_positions() {
    // The refutable consumers (if-let / while-let / let-else) consume the
    // AST-derived ArmResolution for their binding/skipped descriptors, but the
    // checker's PatternPlan is the authoritative field-list source for every
    // record-shaped pattern — including an enum struct-variant `Packet::Data
    // { a, .. }`. A cleared/missing plan for that shape must fail closed with a
    // boundary violation in ALL positions, never silently fall back to the AST
    // resolution (the same contract the record-let path enforces).
    let cases = [
        (
            "if_let",
            r#"
enum Packet { Data { a: string, b: string }; Empty; }
fn make() -> Packet { Packet::Data { a: "a".to_upper(), b: "b".to_upper() } }
fn main() -> i64 {
    let p = make();
    if let Packet::Data { a, .. } = p { a.len() } else { 0 }
}"#,
        ),
        (
            "while_let",
            r#"
enum Packet { Data { a: string, b: string }; Empty; }
fn make() -> Packet { Packet::Data { a: "a".to_upper(), b: "b".to_upper() } }
fn main() {
    var p = make();
    while let Packet::Data { a, .. } = p {
        let _ = a.len();
        p = Packet::Empty;
    }
}"#,
        ),
        (
            "let_else",
            r#"
enum Packet { Data { a: string, b: string }; Empty; }
fn make() -> Packet { Packet::Data { a: "a".to_upper(), b: "b".to_upper() } }
fn main() -> i64 {
    let Packet::Data { a, .. } = make() else { return 0 };
    a.len()
}"#,
        ),
    ];
    for (position, source) in cases {
        let (parsed, mut output) = checked(source);
        // The AST-derived resolution survives; only the plan is cleared. A
        // fail-OPEN handoff would lower off the resolution with no diagnostic.
        output.pattern_plans.clear();
        let lowered = lower_program_host_target(&parsed.program, &output, &ResolutionCtx);
        assert!(
            lowered.diagnostics.iter().any(|diagnostic| {
                matches!(
                    &diagnostic.kind,
                    HirDiagnosticKind::CheckerBoundaryViolation { name, reason }
                        if name == "record-shaped pattern"
                            && reason.contains("missing checker PatternPlan")
                )
            }),
            "enum-struct `{{ a, .. }}` in {position} must fail closed on a missing \
             PatternPlan; diagnostics: {:#?}",
            lowered.diagnostics
        );
    }
}
