use hew_hir::{
    lower_program, HirExprKind, HirItem, HirLiteral, HirMatchArmPredicate, ResolutionCtx,
};
use hew_types::{module_registry::ModuleRegistry, Checker, ResolvedTy};

fn lower_checked(source: &str) -> hew_hir::LowerOutput {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    assert!(
        tc_output.errors.is_empty(),
        "type errors: {:?}",
        tc_output.errors
    );
    lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    )
}

fn find_match_in_fn<'a>(output: &'a hew_hir::LowerOutput, fn_name: &str) -> &'a HirExprKind {
    let fn_item = output
        .module
        .items
        .iter()
        .find_map(|item| {
            if let HirItem::Function(f) = item {
                (f.name == fn_name).then_some(f)
            } else {
                None
            }
        })
        .unwrap_or_else(|| panic!("{fn_name} function not found in HIR"));

    fn_item
        .body
        .tail
        .as_ref()
        .map(|e| &e.kind)
        .filter(|k| matches!(k, HirExprKind::Match { .. }))
        .unwrap_or_else(|| panic!("no Match expr in {fn_name} body"))
}

#[test]
fn variant_payload_literal_lowers_to_pending_payload_predicate() {
    let output = lower_checked(
        r"
enum Maybe { Some(i64); None }
fn classify(x: Maybe) -> i64 {
    match x {
        Some(0) => 1,
        Some(_) => 2,
        None => 3,
    }
}",
    );
    assert!(
        output.diagnostics.is_empty(),
        "unexpected HIR diagnostics: {:?}",
        output.diagnostics
    );

    let HirExprKind::Match { arms, .. } = find_match_in_fn(&output, "classify") else {
        panic!("expected match expression")
    };
    assert_eq!(arms[0].payload_predicates.len(), 1);
    assert_eq!(arms[0].payload_predicates[0].field_idx, 0);
    assert_eq!(
        arms[0].payload_predicates[0].literal,
        HirLiteral::Integer(0)
    );
    assert!(arms[0].bindings.is_empty());
}

#[test]
fn variant_payload_bindings_still_lower_to_arm_bindings() {
    let output = lower_checked(
        r"
enum List { Cons(i64, i64); Nil }
fn sum_pair(x: List) -> i64 {
    match x {
        Cons(h, t) => h + t,
        Nil => 0,
    }
}",
    );
    assert!(
        output.diagnostics.is_empty(),
        "unexpected HIR diagnostics: {:?}",
        output.diagnostics
    );

    let HirExprKind::Match { arms, .. } = find_match_in_fn(&output, "sum_pair") else {
        panic!("expected match expression")
    };
    assert_eq!(arms[0].payload_predicates.len(), 0);
    assert_eq!(arms[0].bindings.len(), 2);
    assert_eq!(arms[0].bindings[0].name, "h");
    assert_eq!(arms[0].bindings[1].name, "t");
}

#[test]
fn string_literal_match_lowers_to_literal_predicate() {
    let output = lower_checked(
        r#"
fn classify(s: string) -> i64 {
    match s {
        "yes" => 1,
        "no" => 0,
        _ => -1,
    }
}"#,
    );
    assert!(
        output.diagnostics.is_empty(),
        "unexpected HIR diagnostics: {:?}",
        output.diagnostics
    );

    let HirExprKind::Match { arms, .. } = find_match_in_fn(&output, "classify") else {
        panic!("expected match expression")
    };
    assert!(matches!(
        &arms[0].predicate,
        HirMatchArmPredicate::Literal {
            lit: HirLiteral::String(value),
            ty: ResolvedTy::String,
        } if value == "yes"
    ));
    assert!(matches!(
        &arms[1].predicate,
        HirMatchArmPredicate::Literal {
            lit: HirLiteral::String(value),
            ty: ResolvedTy::String,
        } if value == "no"
    ));
}

#[test]
fn record_destructure_match_lowers_to_record_project() {
    let output = lower_checked(
        r"
type Point {
    x: i64,
    y: i64,
}

fn sum(p: Point) -> i64 {
    match p {
        Point { x, y } => x + y,
    }
}",
    );
    assert!(
        output.diagnostics.is_empty(),
        "unexpected HIR diagnostics: {:?}",
        output.diagnostics
    );

    let HirExprKind::Match { arms, .. } = find_match_in_fn(&output, "sum") else {
        panic!("expected match expression")
    };
    assert!(matches!(
        &arms[0].predicate,
        HirMatchArmPredicate::RecordProject {
            ty: ResolvedTy::Named { name, .. },
        } if name == "Point"
    ));
    assert_eq!(arms[0].bindings.len(), 2);
    assert_eq!(arms[0].bindings[0].name, "x");
    assert_eq!(arms[0].bindings[0].field_idx, 0);
    assert_eq!(arms[0].bindings[1].name, "y");
    assert_eq!(arms[0].bindings[1].field_idx, 1);
}

#[test]
fn tuple_destructure_match_lowers_to_tuple_project() {
    let output = lower_checked(
        r"
fn sum(t: (i64, i64, i64)) -> i64 {
    match t {
        (a, b, c) => a + b + c,
    }
}",
    );
    assert!(
        output.diagnostics.is_empty(),
        "unexpected HIR diagnostics: {:?}",
        output.diagnostics
    );

    let HirExprKind::Match { arms, .. } = find_match_in_fn(&output, "sum") else {
        panic!("expected match expression")
    };
    assert!(matches!(
        &arms[0].predicate,
        HirMatchArmPredicate::TupleProject { arity: 3 }
    ));
    assert_eq!(arms[0].bindings.len(), 3);
    assert_eq!(arms[0].bindings[0].name, "a");
    assert_eq!(arms[0].bindings[0].field_idx, 0);
    assert_eq!(arms[0].bindings[1].name, "b");
    assert_eq!(arms[0].bindings[1].field_idx, 1);
    assert_eq!(arms[0].bindings[2].name, "c");
    assert_eq!(arms[0].bindings[2].field_idx, 2);
}
