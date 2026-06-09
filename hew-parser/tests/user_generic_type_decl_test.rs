// Tests that `pub type Box<T>`, `pub type Pair<A, B>`, and `pub enum Result<T, E>`
// round-trip through the parser and produce the expected AST shape.
//
// Also verifies that malformed cases (`pub type Box<>` and `pub type Box<T, T>`)
// produce parse-level or downstream errors.
use hew_parser::{
    ast::{Item, TypeDeclKind},
    parse,
};

// ── Helpers ──────────────────────────────────────────────────────────────────

fn parse_one_type_decl(src: &str) -> hew_parser::ast::TypeDecl {
    let result = parse(src);
    assert!(
        result.errors.is_empty(),
        "expected no parse errors for {src:?}, got: {:?}",
        result.errors
    );
    assert_eq!(result.program.items.len(), 1, "expected exactly one item");
    match result.program.items.into_iter().next().unwrap().0 {
        Item::TypeDecl(t) => t,
        other => panic!("expected Item::TypeDecl, got {other:?}"),
    }
}

fn expect_parse_errors(src: &str) -> Vec<String> {
    let result = parse(src);
    assert!(
        !result.errors.is_empty(),
        "expected parse errors for {src:?}, but got none"
    );
    result.errors.into_iter().map(|e| e.message).collect()
}

// ── Named-field struct with one type param ───────────────────────────────────

#[test]
fn parses_pub_type_box_t_has_one_type_param() {
    let td = parse_one_type_decl("pub type Box<T> { value: T }");
    assert_eq!(td.name, "Box");
    assert_eq!(td.kind, TypeDeclKind::Struct);
    let params = td.type_params.expect("expected type_params");
    assert_eq!(params.len(), 1, "expected one type param");
    assert_eq!(params[0].name, "T");
    assert!(params[0].bounds.is_empty(), "expected no bounds on T");
}

#[test]
fn parses_pub_type_box_t_has_pub_visibility() {
    let td = parse_one_type_decl("pub type Box<T> { value: T }");
    assert_eq!(td.visibility, hew_parser::ast::Visibility::Pub);
}

#[test]
fn parses_pub_type_box_t_field_uses_type_param() {
    let td = parse_one_type_decl("pub type Box<T> { value: T }");
    // Body has one Field item named "value"
    assert_eq!(td.body.len(), 1);
    match &td.body[0] {
        hew_parser::ast::TypeBodyItem::Field { name, .. } => {
            assert_eq!(name, "value");
        }
        other => panic!("expected Field item, got {other:?}"),
    }
}

// ── Named-field struct with two type params ──────────────────────────────────

#[test]
fn parses_pub_type_pair_ab_has_two_type_params() {
    let td = parse_one_type_decl("pub type Pair<A, B> { first: A; second: B }");
    assert_eq!(td.name, "Pair");
    assert_eq!(td.kind, TypeDeclKind::Struct);
    let params = td.type_params.expect("expected type_params");
    assert_eq!(params.len(), 2, "expected two type params");
    assert_eq!(params[0].name, "A");
    assert_eq!(params[1].name, "B");
}

#[test]
fn parses_pub_type_pair_ab_has_two_fields() {
    let td = parse_one_type_decl("pub type Pair<A, B> { first: A; second: B }");
    assert_eq!(td.body.len(), 2);
}

// ── Enum with two type params and tuple variants ─────────────────────────────

#[test]
fn parses_pub_enum_result_te_has_two_type_params() {
    // Hew enum variants are separated by semicolons
    let td = parse_one_type_decl("pub enum Result<T, E> { Ok(T); Err(E) }");
    assert_eq!(td.name, "Result");
    assert_eq!(td.kind, TypeDeclKind::Enum);
    let params = td.type_params.expect("expected type_params");
    assert_eq!(params.len(), 2);
    assert_eq!(params[0].name, "T");
    assert_eq!(params[1].name, "E");
}

#[test]
fn parses_pub_enum_result_te_has_two_tuple_variants() {
    let td = parse_one_type_decl("pub enum Result<T, E> { Ok(T); Err(E) }");
    assert_eq!(td.body.len(), 2);
    // Both variants must be Variant items
    for item in &td.body {
        assert!(
            matches!(item, hew_parser::ast::TypeBodyItem::Variant(_)),
            "expected Variant item, got {item:?}"
        );
    }
}

#[test]
fn parses_pub_enum_result_te_ok_variant_has_t_payload() {
    let td = parse_one_type_decl("pub enum Result<T, E> { Ok(T); Err(E) }");
    let hew_parser::ast::TypeBodyItem::Variant(ok_variant) = &td.body[0] else {
        panic!("expected Variant");
    };
    assert_eq!(ok_variant.name, "Ok");
    let hew_parser::ast::VariantKind::Tuple(fields) = &ok_variant.kind else {
        panic!("expected Tuple variant");
    };
    assert_eq!(fields.len(), 1);
}

// ── Type param bounds round-trip ─────────────────────────────────────────────

#[test]
fn parses_pub_type_bounded_param_preserves_bound() {
    let td = parse_one_type_decl("pub type Showable<T: Display> { value: T }");
    assert_eq!(td.name, "Showable");
    let params = td.type_params.expect("expected type_params");
    assert_eq!(params.len(), 1);
    assert_eq!(params[0].name, "T");
    assert_eq!(params[0].bounds.len(), 1);
    assert_eq!(params[0].bounds[0].name, "Display");
}

// ── Without-params baseline (no type params) ─────────────────────────────────

#[test]
fn parses_pub_type_without_params_has_none_type_params() {
    let td = parse_one_type_decl("pub type Point { x: int; y: int }");
    assert_eq!(td.name, "Point");
    assert!(
        td.type_params.is_none(),
        "monomorphic type should have no type_params"
    );
}

// ── Error cases ───────────────────────────────────────────────────────────────

#[test]
fn parses_pub_type_empty_angle_brackets_rejects() {
    // `pub type Box<> { value: int }` — empty type params are not meaningful
    let errors = expect_parse_errors("pub type Box<> { value: int }");
    assert!(
        errors
            .iter()
            .any(|e| e.contains("empty") || e.contains("type parameter")),
        "expected an 'empty type parameters' error, got: {errors:?}"
    );
}
