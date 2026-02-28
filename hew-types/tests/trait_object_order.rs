fn typecheck(source: &str) -> hew_types::TypeCheckOutput {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parser errors: {:?}",
        parsed.errors
    );
    let mut checker = hew_types::Checker::new();
    checker.check_program(&parsed.program)
}

#[test]
fn trait_object_different_order_unifies() {
    let output = typecheck(
        r#"
        trait A { fn a(self) -> int; }
        trait B { fn b(self) -> int; }
        fn takes_ab(x: dyn (A + B)) -> int { x.a() }
        fn gives_ba(x: dyn (B + A)) -> int { takes_ab(x) }
    "#,
    );
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );
}
