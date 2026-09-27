use super::check_source;
use crate::error::TypeErrorKind;

#[test]
fn every_bare_result_statement_is_refused() {
    for body in [
        "might_fail();",
        "{ might_fail(); }",
        "if true { might_fail(); }",
    ] {
        let source = format!(
            "fn might_fail() -> Result<i64, string> {{ .Err(\"bad\") }} \
             fn main() {{ {body} }}"
        );
        let output = check_source(&source);
        let hits = output
            .errors
            .iter()
            .filter(|error| error.kind == TypeErrorKind::ResultDropped)
            .collect::<Vec<_>>();
        assert_eq!(hits.len(), 1, "{body}: {:?}", output.errors);
        assert!(hits[0].message.contains("string"), "{body}: {hits:?}");
    }
}

#[test]
fn handled_and_explicitly_discarded_results_are_accepted() {
    for body in [
        "let _ = might_fail();",
        "let outcome = might_fail(); let _ = outcome;",
        "match might_fail() { .Ok(_) => {} .Err(_) => {} }",
        "might_fail() handle failure { };",
    ] {
        let source = format!(
            "fn might_fail() -> Result<i64, string> {{ .Err(\"bad\") }} \
             fn main() {{ {body} }}"
        );
        let output = check_source(&source);
        assert!(
            !output
                .errors
                .iter()
                .any(|error| error.kind == TypeErrorKind::ResultDropped),
            "{body}: {:?}",
            output.errors
        );
    }
}
