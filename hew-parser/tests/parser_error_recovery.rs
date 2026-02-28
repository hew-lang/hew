#[test]
fn missing_param_type_reports_error() {
    let source = r#"
        fn demo(a) {}
    "#;
    let result = hew_parser::parse(source);
    assert!(
        result.errors.iter().any(|err| err
            .message
            .contains("expected ':' and type annotation for parameter")),
        "expected missing type error, got {:?}",
        result.errors
    );
}

#[test]
fn invalid_pub_scope_reports_error() {
    let source = r#"
        pub(invalid) fn demo() {}
    "#;
    let result = hew_parser::parse(source);
    assert!(
        result.errors.iter().any(|err| err
            .message
            .contains("expected 'package' or 'super' after 'pub('")),
        "expected pub scope error, got {:?}",
        result.errors
    );
}
