use std::path::{Path, PathBuf};

use hew_types::error::TypeErrorKind;

fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .to_path_buf()
}

fn typecheck(source: &str) -> hew_types::TypeCheckOutput {
    let parse_result = hew_parser::parse(source);
    assert!(
        parse_result.errors.is_empty(),
        "should parse cleanly, got: {:#?}",
        parse_result.errors
    );
    let mut checker =
        hew_types::Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![
            repo_root(),
        ]));
    checker.check_program(&parse_result.program)
}

#[test]
fn regex_pattern_clone_preserves_pattern_type_via_registry_fallback() {
    let output = typecheck(
        r#"
        import std::text::regex;

        fn main() {
            let re = regex.new("a+");
            let copy: regex.Pattern = re.clone();
            copy.free();
            re.free();
        }
        "#,
    );

    assert!(
        output.errors.is_empty(),
        "expected regex.Pattern clone() to keep regex.Pattern type, got: {:#?}",
        output.errors
    );
}

#[test]
fn regex_pattern_is_match_rejects_non_string_argument_via_registry_fallback() {
    let output = typecheck(
        r#"
        import std::text::regex;

        fn main() {
            let re = regex.new("a+");
            re.is_match(42);
        }
        "#,
    );

    assert!(
        output.errors.iter().any(|error| matches!(
            &error.kind,
            TypeErrorKind::Mismatch { expected, .. } if expected == "String"
        )),
        "expected regex.Pattern::is_match to reject non-String arg via fallback, got: {:#?}",
        output.errors
    );
}

#[test]
fn regex_pattern_replace_rejects_non_string_replacement_via_registry_fallback() {
    let output = typecheck(
        r#"
        import std::text::regex;

        fn main() {
            let re = regex.new("[0-9]+");
            re.replace("abc123", 99);
        }
        "#,
    );

    assert!(
        output.errors.iter().any(|error| matches!(
            &error.kind,
            TypeErrorKind::Mismatch { expected, .. } if expected == "String"
        )),
        "expected regex.Pattern::replace to reject non-String replacement via fallback, got: {:#?}",
        output.errors
    );
}

#[test]
fn regex_pattern_unknown_method_reports_undefined_method_via_registry_fallback() {
    let output = typecheck(
        r#"
        import std::text::regex;

        fn main() {
            let re = regex.new("a+");
            re.nonexistent_method();
        }
        "#,
    );

    assert!(
        output
            .errors
            .iter()
            .any(|error| error.kind == TypeErrorKind::UndefinedMethod),
        "expected regex.Pattern unknown method to report UndefinedMethod, got: {:#?}",
        output.errors
    );
}
