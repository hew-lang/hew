use hew_types::Checker;

#[test]
fn extern_call_requires_unsafe() {
    let source = r#"
extern "C" {
    fn abs(n: int) -> int;
}

fn main() {
    let result = abs(-42);
}
"#;
    let parse = hew_parser::parse(source);
    assert!(parse.errors.is_empty(), "parse errors: {:?}", parse.errors);
    let mut checker = Checker::new();
    let output = checker.check_program(&parse.program);
    assert!(
        output.errors.iter().any(|e| e.message.contains("unsafe")),
        "expected unsafe error, got: {:?}",
        output.errors
    );
}
