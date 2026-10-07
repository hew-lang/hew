use crate::support::checker_pipeline::lower_through_checker;
use hew_hir::HirDiagnosticKind;

#[test]
fn tuple_pattern_arity_mismatch_is_a_user_error() {
    let output = lower_through_checker(
        r"
fn main() {
    let (first, second) = (1, 2, 3);
}",
    );

    assert!(
        output.diagnostics.iter().any(|diagnostic| {
            matches!(
                diagnostic.kind,
                HirDiagnosticKind::TuplePatternArityMismatch {
                    expected: 3,
                    actual: 2,
                }
            )
        }),
        "tuple arity mismatch must have a structured user-error diagnostic: {:#?}",
        output.diagnostics
    );
    assert!(
        !output.diagnostics.iter().any(|diagnostic| matches!(
            diagnostic.kind,
            HirDiagnosticKind::NotYetImplemented { .. }
        )),
        "tuple arity mismatch must not be reported as unsupported: {:#?}",
        output.diagnostics
    );
}

#[test]
fn enum_constructor_mistakes_are_structured_user_errors() {
    use hew_types::error::TypeErrorKind;
    for (expression, kind, detail) in [
        ("Shape.Pair(1)", TypeErrorKind::ArityMismatch, "2 argument"),
        (
            "Shape.Pair { left: 1 }",
            TypeErrorKind::UndefinedType,
            "Shape.Pair",
        ),
        (
            "Shape.Record { left: 1 }",
            TypeErrorKind::UndefinedField,
            "right",
        ),
        (
            "Shape.Record { left: 1, right: 2, extra: 3 }",
            TypeErrorKind::UndefinedField,
            "extra",
        ),
        (
            "Shape.Unit()",
            TypeErrorKind::PathKindMismatch,
            "parentheses",
        ),
        (
            "Shape.Record(1, 2)",
            TypeErrorKind::PathKindMismatch,
            "`Shape.Record { left: .., right: .. }`",
        ),
    ] {
        let source = format!("enum Shape {{ Pair(i64, i64); Record {{ left: i64; right: i64; }} Unit; }}\nfn main() {{ let value = {expression}; }}");
        // These are checker errors. A rejected program does not have the
        // complete declaration facts needed to enter HIR.
        let (_, checked) = crate::support::checker_pipeline::typecheck_source(&source);
        let diagnostic = checked
            .errors
            .iter()
            .find(|error| error.kind == kind)
            .unwrap_or_else(|| panic!("{expression}: {:?}", checked.errors));
        assert!(diagnostic.message.contains(detail), "{diagnostic:?}");
        assert_eq!(source[diagnostic.span.clone()].trim(), expression);
    }
}
