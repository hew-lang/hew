/// Tests for generic lambda syntax removal.
///
/// Generic lambda literals `<T>(params) => body` were removed in v0.5.
/// Generic behaviour is achieved through generic named functions.
/// These tests confirm the parser emits a typed diagnostic when the old
/// syntax is encountered, satisfying the fail-closed contract.
#[test]
fn generic_lambda_removed_emits_typed_diagnostic() {
    // Simple case: `<T>(x: T) -> T => x`
    for source in [
        r"fn main() { let id = <T>(x: T) -> T => x; let a: i64 = id(42); }",
        r#"fn main() { let id = <T>(x: T) -> T => x; let a: i64 = id(42); let b: string = id("hi"); }"#,
        r"fn main() { var id = <T>(x: T) -> T => x; let a: i64 = id(42); }",
        r"fn main() { let pick = <A, B>(a: A, b: B) -> A => a; pick(42, true); }",
    ] {
        let result = hew_parser::parse(source);
        assert!(
            result.errors.iter().any(|e| {
                matches!(e.kind, hew_parser::ParseDiagnosticKind::ClosurePipeSyntax)
                    && e.message.contains("E_CLOSURE_PIPE_SYNTAX")
            }),
            "expected typed E_CLOSURE_PIPE_SYNTAX for removed generic lambda: {source}\ngot: {:?}",
            result.errors
        );
    }
}
