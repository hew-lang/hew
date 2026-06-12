/// Parser tests for module-qualified struct literal syntax:
/// `module.Type::Variant { fields }` in the dot-postfix tail.
///
/// Slice I-a: parser-only substrate.  Checker resolution (Slice I-b) is a
/// separate lane; tests here verify AST shape, not type correctness.
use hew_parser::ast::{Expr, Item, Stmt};

// ── helpers ──────────────────────────────────────────────────────────────────

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

fn first_body_expr_with_let(source: &str) -> Expr {
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
    let (stmt, _) = f.body.stmts.first().expect("no statements");
    let Stmt::Let {
        value: Some(val), ..
    } = stmt
    else {
        panic!("expected let statement with value, got: {stmt:?}");
    };
    val.0.clone()
}

// ── Positive: struct literal with named fields ────────────────────────────────

/// `fs.IoError::Custom { code: 42 }` parses to
/// `StructInit { name: "fs.IoError::Custom", fields: [("code", 42)], ... }`
#[test]
fn module_qualified_struct_literal_with_field_parses() {
    let expr = first_body_expr_with_let("fn f() { let e = fs.IoError::Custom { code: 42 }; }");
    let Expr::StructInit {
        name,
        fields,
        type_args,
        base,
    } = expr
    else {
        panic!("expected StructInit, got: {expr:?}");
    };
    assert_eq!(name, "fs.IoError::Custom");
    assert_eq!(fields.len(), 1);
    assert_eq!(fields[0].0, "code");
    assert!(type_args.is_none());
    assert!(base.is_none());
}

/// Multiple fields parse correctly.
#[test]
fn module_qualified_struct_literal_multiple_fields_parses() {
    let expr = first_body_expr_with_let("fn f() { let e = m.E::V { x: 1, y: 2 }; }");
    let Expr::StructInit { name, fields, .. } = expr else {
        panic!("expected StructInit, got: {expr:?}");
    };
    assert_eq!(name, "m.E::V");
    assert_eq!(fields.len(), 2);
    assert_eq!(fields[0].0, "x");
    assert_eq!(fields[1].0, "y");
}

/// Empty braces: `m.E::V {}` is an empty struct literal, not a block.
#[test]
fn module_qualified_struct_literal_empty_braces_parses() {
    let expr = first_body_expr_with_let("fn f() { let e = m.E::V {}; }");
    let Expr::StructInit { name, fields, .. } = expr else {
        panic!("expected StructInit, got: {expr:?}");
    };
    assert_eq!(name, "m.E::V");
    assert!(fields.is_empty());
}

/// Functional-update form: `m.E::V { x: 1, ..base }` carries a base expression.
#[test]
fn module_qualified_struct_literal_functional_update_parses() {
    let expr = first_body_expr_with_let("fn f() { let e = m.E::V { x: 1, ..old }; }");
    let Expr::StructInit {
        name, fields, base, ..
    } = expr
    else {
        panic!("expected StructInit, got: {expr:?}");
    };
    assert_eq!(name, "m.E::V");
    assert_eq!(fields.len(), 1);
    assert_eq!(fields[0].0, "x");
    assert!(base.is_some());
}

/// Trailing comma inside the brace list is accepted.
#[test]
fn module_qualified_struct_literal_trailing_comma_parses() {
    let expr = first_body_expr_with_let("fn f() { let e = m.E::V { x: 1, }; }");
    let Expr::StructInit { name, fields, .. } = expr else {
        panic!("expected StructInit, got: {expr:?}");
    };
    assert_eq!(name, "m.E::V");
    assert_eq!(fields.len(), 1);
}

// ── Regression: `fs.IoError::TimedOut(0)` still emits Call ──────────────────

/// Cross-module enum variant with tuple payload must produce a flat
/// `Call { function: Identifier("fs.IoError::TimedOut"), ... }`, not a
/// `StructInit` or a `MethodCall`.
///
/// The dot-postfix path converts `receiver.Type::Variant` to the composite
/// `Identifier("receiver.Type::Variant")` which the outer call-expression
/// parser wraps in `Call`.  This is the shape downstream consumers (enrich,
/// codegen) expect for variant construction via a module alias.
#[test]
fn regression_cross_module_enum_variant_call_unaffected() {
    let expr = first_body_expr("fn f() { fs.IoError::TimedOut(0) }");
    let Expr::Call { function, args, .. } = expr else {
        panic!("expected Call (regression: call form broken), got: {expr:?}");
    };
    assert!(
        matches!(function.0, Expr::Identifier(ref n) if n == "fs.IoError::TimedOut"),
        "callee must be Identifier(fs.IoError::TimedOut), got: {:?}",
        function.0
    );
    assert_eq!(args.len(), 1);
}

// ── Regression: plain field access and method calls unaffected ───────────────

/// `obj.field` still produces `FieldAccess`, not `StructInit`.
#[test]
fn regression_plain_field_access_unaffected() {
    let expr = first_body_expr("fn f() { obj.field }");
    assert!(
        matches!(expr, Expr::FieldAccess { .. }),
        "expected FieldAccess, got: {expr:?}"
    );
}

/// `obj.method()` still produces `MethodCall`.
#[test]
fn regression_plain_method_call_unaffected() {
    let expr = first_body_expr("fn f() { obj.method() }");
    assert!(
        matches!(expr, Expr::MethodCall { .. }),
        "expected MethodCall, got: {expr:?}"
    );
}

/// Single-segment dot-field followed by `{` that is actually a block should
/// NOT be parsed as a struct literal (no `::` in field name).
#[test]
fn regression_field_followed_by_block_is_not_struct_literal() {
    // `obj.method` has no `::`, so `{}` starts the next statement (a block).
    // The expression `obj.method` parses as FieldAccess; the `{}` becomes a
    // separate block expression statement.
    let result = hew_parser::parse("fn f() { obj.method; {} }");
    assert!(
        result.errors.is_empty(),
        "unexpected parse errors: {:?}",
        result.errors
    );
    let (item, _) = result.program.items.first().expect("no items");
    let Item::Function(f) = item else {
        panic!("expected function");
    };
    let (stmt, _) = f.body.stmts.first().expect("no stmts");
    assert!(
        matches!(stmt, Stmt::Expression((Expr::FieldAccess { .. }, _))),
        "first statement must be FieldAccess expression, got: {stmt:?}"
    );
}

// ── Negative: parse error inside braces emits a diagnostic ───────────────────

/// When a parse error occurs inside the struct literal body, the parser
/// records an error diagnostic (rather than silently producing a partial AST).
#[test]
fn module_qualified_struct_literal_bad_field_value_emits_error() {
    // `@` is not a valid expression — the parser should record an error.
    let result = hew_parser::parse("fn f() { let _ = m.E::V { x: @ }; }");
    assert!(
        !result.errors.is_empty(),
        "expected a parse error for invalid field value, got no errors"
    );
}

// ── Three-segment path (`Type::Outer::Inner { fields }`) ─────────────────────

/// Deeper `::` chains are preserved in the qualified name.
#[test]
fn module_qualified_struct_literal_three_segment_path_parses() {
    let expr = first_body_expr_with_let("fn f() { let e = m.A::B::C { x: 1 }; }");
    let Expr::StructInit { name, .. } = expr else {
        panic!("expected StructInit, got: {expr:?}");
    };
    assert_eq!(name, "m.A::B::C");
}
