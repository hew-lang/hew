/// Parser regression tests for cross-module enum variant construction
/// via the dot-postfix path (e.g. `fs.IoError.TimedOut(0)`).
///
/// The parser preserves the final dotted segment as a method call; nominal
/// constructor resolution happens later in the type checker.
use hew_parser::ast::{CallArg, Expr, Item, Stmt};

fn first_body_expr(source: &str) -> Expr {
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "unexpected parse errors: {:?}",
        result.errors
    );
    let (item, _) = result.program.items.first().expect("no items");
    let Item::Function(f) = item else {
        panic!("expected function item");
    };
    // The sole expression may be a trailing_expr or Stmt::Expression — handle both.
    if let Some(trailing) = &f.body.trailing_expr {
        return trailing.0.clone();
    }
    let (stmt, _) = f
        .body
        .stmts
        .first()
        .expect("no statements and no trailing expr");
    let Stmt::Expression((expr, _)) = stmt else {
        panic!("expected expression statement, got: {stmt:?}");
    };
    expr.clone()
}

// --- Positive: cross-module enum variant with payload ---

#[test]
fn cross_module_enum_variant_tuple_payload_parses() {
    let expr = first_body_expr("fn f() { a.B.C(1) }");
    let Expr::MethodCall {
        receiver,
        method,
        args,
    } = expr
    else {
        panic!("expected MethodCall, got: {expr:?}");
    };
    assert!(
        matches!(&receiver.0, Expr::FieldAccess { object, field }
            if matches!(&object.0, Expr::Identifier(n) if n == "a") && field == "B"),
        "expected a.B receiver, got: {:?}",
        receiver.0
    );
    assert_eq!(method, "C");
    assert_eq!(args.len(), 1);
    let CallArg::Positional((Expr::Literal(lit), _)) = &args[0] else {
        panic!("expected positional literal arg, got: {:?}", args[0]);
    };
    assert!(
        matches!(lit, hew_parser::ast::Literal::Integer { value: 1, .. }),
        "expected integer literal 1, got: {lit:?}"
    );
}

// --- Positive: deeper nesting (three segments) ---

#[test]
fn cross_module_enum_nested_segments_parses() {
    let expr = first_body_expr("fn f() { a.B.C.D(0) }");
    let Expr::MethodCall {
        receiver,
        method,
        args,
    } = expr
    else {
        panic!("expected MethodCall, got: {expr:?}");
    };
    assert!(
        matches!(&receiver.0, Expr::FieldAccess { object, field } if field == "C"
            && matches!(&object.0, Expr::FieldAccess { object, field } if field == "B"
                && matches!(&object.0, Expr::Identifier(n) if n == "a"))),
        "expected a.B.C receiver, got: {:?}",
        receiver.0
    );
    assert_eq!(method, "D");
    assert_eq!(args.len(), 1);
}

// --- Positive: chained postfix — FieldAccess then MethodCall on the result ---

#[test]
fn cross_module_enum_chained_field_then_variant_parses() {
    // `a.b.C` is a FieldAccess chain; `.D(0)` is a MethodCall on that result.
    let expr = first_body_expr("fn f() { a.b.C.D(0) }");
    let Expr::MethodCall {
        receiver,
        method,
        args,
    } = expr
    else {
        panic!("expected MethodCall, got: {expr:?}");
    };
    assert!(
        matches!(&receiver.0, Expr::FieldAccess { object, field } if field == "C"
            && matches!(&object.0, Expr::FieldAccess { object, field } if field == "b"
                && matches!(&object.0, Expr::Identifier(n) if n == "a"))),
        "expected a.b.C receiver, got: {:?}",
        receiver.0
    );
    assert_eq!(method, "D");
    assert_eq!(args.len(), 1);
}

// --- Negative regression: single-segment method call still works ---

#[test]
fn single_segment_method_call_regression() {
    let expr = first_body_expr("fn f() { obj.method() }");
    let Expr::MethodCall {
        receiver,
        method,
        args,
    } = expr
    else {
        panic!("expected MethodCall, got: {expr:?}");
    };
    assert!(
        matches!(receiver.0, Expr::Identifier(ref n) if n == "obj"),
        "expected Identifier(obj) receiver"
    );
    assert_eq!(method, "method");
    assert!(args.is_empty());
}

// --- Negative regression: field access still works ---

#[test]
fn field_access_regression() {
    let expr = first_body_expr("fn f() { obj.field }");
    let Expr::FieldAccess { object, field } = expr else {
        panic!("expected FieldAccess, got: {expr:?}");
    };
    assert!(
        matches!(object.0, Expr::Identifier(ref n) if n == "obj"),
        "expected Identifier(obj) object"
    );
    assert_eq!(field, "field");
}

// --- Positive: multi-arg cross-module variant ---

#[test]
fn cross_module_enum_variant_multi_arg_parses() {
    let expr = first_body_expr("fn f() { mod.Outer.Inner(x, y) }");
    let Expr::MethodCall {
        receiver,
        method,
        args,
    } = expr
    else {
        panic!("expected MethodCall, got: {expr:?}");
    };
    assert!(
        matches!(&receiver.0, Expr::FieldAccess { object, field }
            if matches!(&object.0, Expr::Identifier(n) if n == "mod") && field == "Outer"),
        "expected mod.Outer receiver, got: {:?}",
        receiver.0
    );
    assert_eq!(method, "Inner");
    assert_eq!(args.len(), 2);
}
