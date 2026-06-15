#[test]
fn parse_empty_struct_literal() {
    let result = hew_parser::parse("type Foo {} fn main() { let f = Foo {}; }");
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn parse_empty_struct_literal_with_call() {
    let result =
        hew_parser::parse("type Answer {} fn check(a: Answer) {} fn main() { check(Answer {}); }");
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn parse_non_empty_struct_literal() {
    let result = hew_parser::parse(
        "type Point { x: i32, y: i32 } fn main() { let p = Point { x: 1, y: 2 }; }",
    );
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn parse_block_after_if_still_works() {
    let result = hew_parser::parse("fn main() { if true {} }");
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

// ── Empty-block bare-identifier condition (regression, #1912) ──
//
// A bare identifier directly followed by an empty `{ }` block in an
// `if`/`while` condition or `match` scrutinee must open the block, not start an
// empty struct literal. Before the fix, `flag { }` was consumed as an empty
// struct literal and the real block went missing ("expected `{`, found
// `else`"). The non-empty / parenthesised / field-access / comparison forms
// already parsed because none of them reach the bare-identifier struct-init
// arm.

#[test]
fn parse_if_bare_ident_empty_then_block() {
    let result = hew_parser::parse("fn f(flag: bool) { if flag { } else { } }");
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn parse_while_bare_ident_empty_body() {
    let result = hew_parser::parse("fn f(flag: bool) { while flag { } }");
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn parse_match_bare_ident_empty_arms() {
    let result = hew_parser::parse("fn f(flag: bool) { match flag { } }");
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn parse_if_bare_ident_nonempty_then_block_still_works() {
    let result = hew_parser::parse("fn f(flag: bool) { if flag { let _z = 0; } else { } }");
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn parse_if_parenthesised_struct_literal_in_condition() {
    // The no-struct-literal restriction is lifted inside `(...)`, so a struct
    // literal nested in a parenthesised condition still parses.
    let result = hew_parser::parse(
        "type Foo { a: i64, b: bool } fn f() { if (Foo { a: 1, b: true }).b { } }",
    );
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn parse_if_struct_literal_call_arg_in_condition() {
    // Restriction lifted inside call args: `if g(Foo { a: 1 }) { }`.
    let result = hew_parser::parse(
        "type Foo { a: i64 } fn g(x: Foo) -> bool { true } fn f() { if g(Foo { a: 1 }) { } }",
    );
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn parse_if_struct_literal_array_arg_in_condition() {
    // Restriction lifted inside `[...]`: `if h([Foo { a: 1 }]) { }`.
    let result = hew_parser::parse(
        "type Foo { a: i64 } fn h(xs: [Foo; 1]) -> bool { true } fn f() { if h([Foo { a: 1 }]) { } }",
    );
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn parse_empty_struct_literal_expression_position_unaffected() {
    // No regression: an empty struct literal in expression (non-condition)
    // position still parses as a struct literal, not a block.
    let result = hew_parser::parse("type Empty {} fn main() { let y = Empty {}; }");
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    let Some((stmt, _)) = first_stmt(&result) else {
        panic!("expected a statement");
    };
    let hew_parser::ast::Stmt::Let { value: Some(v), .. } = stmt else {
        panic!("expected a let with initialiser, got {stmt:?}");
    };
    assert!(
        matches!(v.0, hew_parser::ast::Expr::StructInit { .. }),
        "empty struct literal must stay a StructInit in expression position, got {:?}",
        v.0
    );
}

fn first_stmt(
    result: &hew_parser::ParseResult,
) -> Option<&hew_parser::ast::Spanned<hew_parser::ast::Stmt>> {
    let main = result
        .program
        .items
        .iter()
        .find_map(|(item, _)| match item {
            hew_parser::ast::Item::Function(f) if f.name == "main" => Some(f),
            _ => None,
        })?;
    main.body.stmts.first()
}
