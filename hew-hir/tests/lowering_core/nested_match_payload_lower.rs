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
enum Maybe { Some(i64), None }
fn classify(x: Maybe) -> i64 {
    match x {
        .Some(0) => 1,
        .Some(_) => 2,
        .None => 3,
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
enum List { Cons(i64, i64), Nil }
fn sum_pair(x: List) -> i64 {
    match x {
        .Cons(h, t) => h + t,
        .Nil => 0,
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
fn integer_literal_match_lowers_to_scrutinee_width() {
    let output = lower_checked(
        r"
fn classify(x: i32) -> i32 {
    match x {
        41 => 1,
        42 => 2,
        _ => 0,
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
    assert!(matches!(
        &arms[0].predicate,
        HirMatchArmPredicate::Literal {
            lit: HirLiteral::Integer(41),
            ty: ResolvedTy::I32,
        }
    ));
    assert!(matches!(
        &arms[1].predicate,
        HirMatchArmPredicate::Literal {
            lit: HirLiteral::Integer(42),
            ty: ResolvedTy::I32,
        }
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

// ── Struct-variant field aggregate destructure (issue #2354) ──────────────────

// Follow the resolved payload binding into its typed destructure group.
// Swapping two payload sources or tuple selectors must fail even when all
// expected user-visible names are still present.
fn assert_tuple_payload_destructure(
    statements: &[hew_hir::HirStmt],
    payload: &hew_hir::HirMatchArmBinding,
    names: &[&str],
) {
    let fields = statements
        .iter()
        .find_map(|statement| match &statement.kind {
            hew_hir::HirStmtKind::Destructure { value, fields }
                if matches!(&value.kind, HirExprKind::BindingRef {
                resolved: hew_hir::ResolvedRef::Binding(id), ..
            } if *id == payload.binding) =>
            {
                Some(fields)
            }
            _ => None,
        })
        .expect("payload binding must feed a typed destructure group");
    assert_eq!(
        payload.ty,
        ResolvedTy::Tuple(vec![ResolvedTy::I64; names.len()])
    );
    assert_eq!(fields.len(), names.len());
    for (index, (field, name)) in fields.iter().zip(names).enumerate() {
        assert_eq!(
            field.selector,
            hew_hir::HirDestructureSelector::Tuple(u32::try_from(index).unwrap())
        );
        assert_eq!(field.binding.name, *name);
        assert_eq!(field.binding.ty, ResolvedTy::I64);
    }
}

fn assert_arm_tuple_payload(arm: &hew_hir::HirMatchArm, field_idx: u32, names: &[&str]) {
    let HirExprKind::Block(block) = &arm.body.kind else {
        panic!("aggregate payload must have a destructure prelude");
    };
    let payload = arm
        .bindings
        .iter()
        .find(|binding| binding.field_idx == field_idx)
        .expect("declared payload field must be bound");
    assert_tuple_payload_destructure(&block.statements, payload, names);
}

#[test]
fn qualified_tuple_variant_aggregate_binds_nested_names() {
    let output = lower_checked(
        r"
enum Pair { Both((i64, i64)), None }

fn sum(pair: Pair) -> i64 {
    match pair {
        Pair.Both((a, b)) => a + b,
        Pair.None => 0,
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
    assert_arm_tuple_payload(&arms[0], 0, &["a", "b"]);
}

// Regression for hew-lang/hew#2354: a tuple sub-pattern in enum struct-variant
// field position (`Data { value: (a, b) }`) passed the checker but failed HIR
// lowering with `E_HIR: identifier has no binding` because the aggregate
// destructure lowering ran only for tuple-variant (`Pattern::Constructor`)
// arms, never for struct-variant (`Pattern::Struct`) arms. The inner binders
// must materialise in a typed destructure of the resolved payload binding.
#[test]
fn struct_variant_tuple_field_destructure_binds_inner_names() {
    let output = lower_checked(
        r"
enum Packet {
    Data { value: (i64, i64) },
    Empty,
}

fn sum(p: Packet) -> i64 {
    match p {
        .Data { value: (a, b) } => a + b,
        .Empty => 0,
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
    assert_arm_tuple_payload(&arms[0], 0, &["a", "b"]);
}

// A plain field binder alongside an aggregate field must both resolve, and the
// field-to-declared-type mapping is by NAME, so a pattern whose field order
// differs from declaration order still lowers cleanly (no dangling binders).
#[test]
fn struct_variant_mixed_and_reordered_fields_all_bind() {
    let output = lower_checked(
        r"
enum P {
    D { p: (i64, i64), q: (i64, i64) },
    E,
}

fn sum(v: P) -> i64 {
    match v {
        .D { q: (c, d), p: (a, b) } => a + b + c + d,
        .E => 0,
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
    assert_arm_tuple_payload(&arms[0], 0, &["a", "b"]);
    assert_arm_tuple_payload(&arms[0], 1, &["c", "d"]);
}

// Generic enum struct-variant: the field's declared type mentions a type param
// that must be substituted from the scrutinee's type args before the tuple
// sub-pattern's inner binders are typed and materialised. The synthetic field
// temp carries the substituted concrete field type (`(i64, i64)`).
#[test]
fn generic_struct_variant_tuple_field_destructure_binds_inner_names() {
    let output = lower_checked(
        r"
enum Box<T> {
    Pair { both: (T, T) },
    Empty,
}

fn sum(b: Box<i64>) -> i64 {
    match b {
        .Pair { both: (a, c) } => a + c,
        .Empty => 0,
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
    assert_arm_tuple_payload(&arms[0], 0, &["a", "c"]);
}

#[test]
fn struct_variant_tuple_field_let_else_binds_inner_names() {
    let output = lower_checked(
        r"
enum Packet {
    Data { pair: (i64, i64) },
    Empty,
}

fn sum(value: Packet) -> i64 {
    let Packet.Data { pair: (a, b) } = value else { return 0 };
    a + b
}",
    );
    assert!(
        output.diagnostics.is_empty(),
        "unexpected HIR diagnostics: {:?}",
        output.diagnostics
    );

    let HirItem::Function(function) = output
        .module
        .items
        .iter()
        .find(|item| matches!(item, HirItem::Function(function) if function.name == "sum"))
        .expect("sum function present")
    else {
        unreachable!()
    };
    let let_else = function
        .body
        .statements
        .iter()
        .find_map(|statement| match &statement.kind {
            hew_hir::HirStmtKind::LetElse {
                success_prelude,
                bindings,
                ..
            } => Some((success_prelude, bindings)),
            _ => None,
        })
        .expect("struct-variant let-else present");
    let payload = let_else
        .1
        .iter()
        .find(|binding| binding.field_idx == 0)
        .expect("let-else payload field must be bound");
    assert_tuple_payload_destructure(let_else.0, payload, &["a", "b"]);
}
