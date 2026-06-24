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

// ── Condition-position struct-literal disambiguation edges (#1912) ──
//
// The empty-block fix added a `no_struct_literal` restriction in `if`/`while`
// condition and `match` scrutinee position. These tests cover the three edges
// the independent review found: module-qualified enum-variant struct-init in a
// condition, a parenthesised struct literal directly in condition/scrutinee
// position (formatter must keep the parens), and a block expression in a
// condition (the restriction must not leak into the block body).

/// Assert `src` parses, formats, and re-parses to an AST equal modulo spans.
/// This is the round-trip property the fmt corpus enforces, applied to one
/// program: if the formatter drops a paren the struct literal depends on, the
/// reformatted output either fails to parse or parses to a different AST.
fn assert_roundtrips(src: &str) {
    let parsed1 = hew_parser::parse(src);
    assert!(
        parsed1.errors.is_empty(),
        "source must parse cleanly, errors: {:?}",
        parsed1.errors
    );
    let formatted = hew_parser::fmt::format_program(&parsed1.program);
    let parsed2 = hew_parser::parse(&formatted);
    assert!(
        parsed2.errors.is_empty(),
        "reformatted output must re-parse, output:\n{formatted}\nerrors: {:?}",
        parsed2.errors
    );
    assert!(
        hew_parser::ast_eq::program_eq_ignoring_spans(&parsed1.program, &parsed2.program),
        "reformatted output must round-trip to the same AST, output:\n{formatted}"
    );
}

#[test]
fn parse_if_module_qualified_variant_empty_block() {
    // Module-qualified enum-variant in condition position: `if m.E::V { } else
    // { }` must open the then-block, not consume `{}` as an empty struct
    // literal (which orphaned the `else` before the fix). The parser does not
    // resolve the module receiver — `m.E::V` reaches the `::`-accumulation
    // postfix branch regardless of whether `m` is declared.
    let result = hew_parser::parse("fn f() { if m.E::V { } else { } }");
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn parse_while_module_qualified_variant_empty_block() {
    let result = hew_parser::parse("fn f() { while m.E::V { } }");
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn parse_match_module_qualified_variant_empty_arms() {
    let result = hew_parser::parse("fn f() { match m.E::V { } }");
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn roundtrip_parenthesised_struct_literal_if_condition() {
    // The formatter must keep the wrapping parens for a struct literal that is
    // the direct condition: `if (Foo { a: true }) {}` re-parses correctly only
    // with the parens. Without them it formats to `if Foo { a: true } {}`, whose
    // `{ a: true }` is the struct body and the real block goes missing.
    assert_roundtrips("type Foo { a: bool } fn f() { if (Foo { a: true }) { } }");
}

#[test]
fn roundtrip_parenthesised_struct_literal_match_scrutinee() {
    assert_roundtrips("type Foo { a: i64 } fn f() { match (Foo { a: 1 }) { _ => { } } }");
}

#[test]
fn roundtrip_parenthesised_struct_literal_while_condition() {
    assert_roundtrips("type Foo { a: bool } fn f() { while (Foo { a: true }) { } }");
}

#[test]
fn parse_if_block_expression_condition_with_inner_struct_literal() {
    // A block expression in condition position is unambiguous (the `{` opens a
    // block, not a struct literal), so the `no_struct_literal` restriction must
    // be lifted inside it — a struct literal nested in the block still parses.
    let result = hew_parser::parse(
        "type Foo { a: bool } fn f() { if { let x = Foo { a: true }; x.a } { } }",
    );
    assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
}

#[test]
fn roundtrip_if_block_expression_condition_with_inner_struct_literal() {
    assert_roundtrips("type Foo { a: bool } fn f() { if { let x = Foo { a: true }; x.a } { } }");
}
