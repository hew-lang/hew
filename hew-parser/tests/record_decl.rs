use hew_parser::{ast::Item, parse};

// Helper: parse src, expect it to produce exactly one Item::Record and return it.
fn parse_one_record(src: &str) -> hew_parser::ast::RecordDecl {
    let result = parse(src);
    assert!(
        result.errors.is_empty(),
        "expected no parse errors for {src:?}, got: {:?}",
        result.errors
    );
    assert_eq!(result.program.items.len(), 1, "expected exactly one item");
    match result.program.items.into_iter().next().unwrap().0 {
        Item::Record(r) => r,
        other => panic!("expected Item::Record, got {other:?}"),
    }
}

// Helper: parse src, expect at least one parse error.
fn expect_parse_error(src: &str) {
    let result = parse(src);
    assert!(
        !result.errors.is_empty(),
        "expected parse errors for {src:?}, but got none"
    );
}

// ── Accept cases ──────────────────────────────────────────────────────────────

#[test]
fn record_named_fields_accept() {
    let r = parse_one_record("record Point { x: int, y: int }");
    assert_eq!(r.name, "Point");
    let hew_parser::ast::RecordKind::Named(fields) = r.kind else {
        panic!("expected Named kind");
    };
    assert_eq!(fields.len(), 2);
    assert_eq!(fields[0].name, "x");
    assert_eq!(fields[1].name, "y");
}

#[test]
fn record_generic_accept() {
    let r = parse_one_record("record Vec3<T> { x: T, y: T, z: T }");
    assert_eq!(r.name, "Vec3");
    assert!(r.type_params.is_some());
    let hew_parser::ast::RecordKind::Named(fields) = r.kind else {
        panic!("expected Named kind");
    };
    assert_eq!(fields.len(), 3);
}

#[test]
fn record_trailing_comma_accept() {
    let r = parse_one_record("record Point { x: int, y: int, }");
    assert_eq!(r.name, "Point");
    let hew_parser::ast::RecordKind::Named(fields) = r.kind else {
        panic!("expected Named kind");
    };
    assert_eq!(fields.len(), 2);
}

#[test]
fn record_pub_visibility_accept() {
    let result = parse("pub record Point { x: int, y: int }");
    assert!(
        result.errors.is_empty(),
        "unexpected errors: {:?}",
        result.errors
    );
    let item = result.program.items.into_iter().next().unwrap().0;
    let Item::Record(r) = item else {
        panic!("expected Item::Record");
    };
    assert_eq!(r.visibility, hew_parser::ast::Visibility::Pub);
    assert_eq!(r.name, "Point");
}

#[test]
fn record_where_clause_accept() {
    let r = parse_one_record("record Pair<T> where T: Display { first: T, second: T }");
    assert_eq!(r.name, "Pair");
    assert!(r.where_clause.is_some());
    let hew_parser::ast::RecordKind::Named(fields) = r.kind else {
        panic!("expected Named kind");
    };
    assert_eq!(fields.len(), 2);
}

// ── Tuple-record accept cases ─────────────────────────────────────────────────

#[test]
fn record_tuple_single_field_accept() {
    let r = parse_one_record("record UserId(int);");
    assert_eq!(r.name, "UserId");
    let hew_parser::ast::RecordKind::Tuple(fields) = r.kind else {
        panic!("expected Tuple kind");
    };
    assert_eq!(fields.len(), 1);
}

#[test]
fn record_tuple_two_fields_accept() {
    let r = parse_one_record("record Pair(int, int);");
    assert_eq!(r.name, "Pair");
    let hew_parser::ast::RecordKind::Tuple(fields) = r.kind else {
        panic!("expected Tuple kind");
    };
    assert_eq!(fields.len(), 2);
}

#[test]
fn record_tuple_generic_accept() {
    let r = parse_one_record("record Wrapper<T>(T);");
    assert_eq!(r.name, "Wrapper");
    assert!(r.type_params.is_some());
    let hew_parser::ast::RecordKind::Tuple(fields) = r.kind else {
        panic!("expected Tuple kind");
    };
    assert_eq!(fields.len(), 1);
}

#[test]
fn record_tuple_trailing_comma_accept() {
    let r = parse_one_record("record Pair(int, int,);");
    assert_eq!(r.name, "Pair");
    let hew_parser::ast::RecordKind::Tuple(fields) = r.kind else {
        panic!("expected Tuple kind");
    };
    assert_eq!(fields.len(), 2);
}

// ── Reject cases ─────────────────────────────────────────────────────────────

#[test]
fn record_empty_body_reject() {
    expect_parse_error("record Empty { }");
}

#[test]
fn record_missing_type_annotation_reject() {
    expect_parse_error("record Bad { x }");
}

#[test]
fn record_missing_field_name_reject() {
    expect_parse_error("record Bad { : int }");
}

#[test]
fn record_missing_comma_between_fields_reject() {
    expect_parse_error("record Bad { x: int y: int }");
}

#[test]
fn record_tuple_empty_reject() {
    expect_parse_error("record Empty();");
}
