use hew_parser::{
    ast::{Item, TypeBodyItem},
    parse,
};

#[test]
fn inline_methods_are_refused_with_an_impl_directive_and_recovery() {
    for method in [
        "fn read(self) -> i64 { self.value }",
        "fn set(var self) { self.value = 2; }",
        "fn close(consume self) { if true { println(\"closed\"); } }",
        "fn create() -> Value { Value { value: 1 } }",
        "pub fn read(self) -> i64 { self.value }",
        "gen fn values(self) -> i64 { yield self.value; }",
        "/// A method.\n#[extern_symbol(\"value_read\")] fn read(self) -> i64 { self.value }",
    ] {
        let source = format!("type Value {{ {method} value: i64; }} fn main() {{}}");
        let parsed = parse(&source);
        assert_eq!(parsed.errors.len(), 1, "{method}: {:?}", parsed.errors);
        let error = &parsed.errors[0];
        assert!(error.message.contains("E_TYPE_METHOD_IN_DECL"), "{error:?}");
        assert_eq!(
            error.kind,
            hew_parser::ParseDiagnosticKind::TypeMethodInDecl
        );
        assert_eq!(
            error.hint.as_deref(),
            Some("move this method to `impl Value { ... }`")
        );
        let Item::TypeDecl(decl) = &parsed.program.items[0].0 else {
            panic!("type lost")
        };
        assert!(matches!(decl.body.as_slice(), [TypeBodyItem::Field { .. }]));
        assert!(matches!(parsed.program.items[1].0, Item::Function(_)));
    }
}

#[test]
fn enum_methods_also_belong_in_impl_blocks() {
    let parsed =
        parse("enum State { Ready; fn read(self) {} Done; } impl State { fn read(self) {} }");
    assert_eq!(parsed.errors.len(), 1, "{:?}", parsed.errors);
    assert_eq!(
        parsed.errors[0].hint.as_deref(),
        Some("move this method to `impl State { ... }`")
    );
    let Item::TypeDecl(decl) = &parsed.program.items[0].0 else {
        panic!("enum lost")
    };
    assert_eq!(decl.body.len(), 2);
    assert!(matches!(parsed.program.items[1].0, Item::Impl(_)));
}
