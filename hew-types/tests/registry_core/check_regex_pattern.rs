use crate::common;

use common::typecheck;
use hew_types::{error::TypeErrorKind, SpanKey, Ty};

#[test]
fn regex_literal_close_uses_canonical_resource_identity() {
    let source = r#"
        import std.text.regex;

        fn main() {
            let pattern = re"a+";
            pattern.close();
        }
        "#;
    let output = typecheck(source);

    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    let literal_start = source.find("re\"a+\"").expect("regex literal is in source");
    let literal_key = SpanKey {
        start: literal_start,
        end: literal_start + "re\"a+\"".len(),
        module_idx: 0,
    };
    assert!(
        matches!(
            output.expr_types.get(&literal_key),
            Some(
                Ty::Named { head: name_head, .. } if name_head.spelling() == "std.text.regex.Pattern",
            )
        ),
        "regex literal must use the canonical pattern identity: {:#?}",
        output.expr_types
    );
    let root_close_sites: Vec<&SpanKey> = output
        .method_call_discharges_receiver
        .iter()
        .filter(|key| key.module_idx == 0)
        .collect();
    assert_eq!(
        root_close_sites.len(),
        1,
        "root discharge facts: {:?}",
        output.method_call_discharges_receiver
    );
    let close_start = source
        .find("pattern.close()")
        .expect("root close call is in source");
    let close_end = close_start + "pattern.close()".len();
    let close_key = root_close_sites[0];
    assert!(
        close_key.start <= close_start && close_end <= close_key.end,
        "expected root discharge span to cover `pattern.close()` at {close_start}..{close_end}; got {close_key:?}",
    );
}

#[test]
fn regex_pattern_clone_preserves_pattern_type_via_registry_fallback() {
    let output = typecheck(
        r#"
        import std.text.regex;

        fn main() {
            let re = regex.new("a+");
            let copy: regex.Pattern = re.clone();
            copy.close();
            re.close();
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
        import std.text.regex;

        fn main() {
            let re = regex.new("a+");
            re.is_match(42);
        }
        "#,
    );

    assert!(
        output.errors.iter().any(|error| matches!(
            &error.kind,
            TypeErrorKind::Mismatch { expected, .. } if expected == "string"
        )),
        "expected regex.Pattern::is_match to reject non-String arg via fallback, got: {:#?}",
        output.errors
    );
}

#[test]
fn regex_pattern_replace_rejects_non_string_replacement_via_registry_fallback() {
    let output = typecheck(
        r#"
        import std.text.regex;

        fn main() {
            let re = regex.new("[0-9]+");
            re.replace("abc123", 99);
        }
        "#,
    );

    assert!(
        output.errors.iter().any(|error| matches!(
            &error.kind,
            TypeErrorKind::Mismatch { expected, .. } if expected == "string"
        )),
        "expected regex.Pattern::replace to reject non-String replacement via fallback, got: {:#?}",
        output.errors
    );
}

#[test]
fn regex_pattern_unknown_method_reports_undefined_method_via_registry_fallback() {
    let output = typecheck(
        r#"
        import std.text.regex;

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
