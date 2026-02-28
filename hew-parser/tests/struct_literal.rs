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
