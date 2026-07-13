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
    let output = lower_through_checker(
        r"
enum Shape {
    Pair(i64, i64);
    Record { left: i64; right: i64 };
    Unit;
}

fn main() {
    let _wrong_arity = Pair(1);
    let _wrong_shape = Pair { left: 1 };
    let _missing = Record { left: 1 };
    let _unknown = Record { left: 1, right: 2, extra: 3 };
    let _unit_call = Unit();
    let _record_call = Record(1, 2);
}",
    );

    assert!(
        output.diagnostics.iter().any(|diagnostic| matches!(
            diagnostic.kind,
            HirDiagnosticKind::EnumVariantConstructorArityMismatch { .. }
        )),
        "expected tuple-variant arity diagnostic, got: {:#?}",
        output.diagnostics
    );
    assert!(
        output.diagnostics.iter().any(|diagnostic| matches!(
            diagnostic.kind,
            HirDiagnosticKind::EnumVariantConstructorShapeMismatch { .. }
        )),
        "expected enum constructor shape diagnostic, got: {:#?}",
        output.diagnostics
    );
    assert!(
        output.diagnostics.iter().any(|diagnostic| matches!(
            diagnostic.kind,
            HirDiagnosticKind::EnumVariantConstructorMissingField { .. }
        )),
        "expected missing enum variant field diagnostic, got: {:#?}",
        output.diagnostics
    );
    assert!(
        output.diagnostics.iter().any(|diagnostic| matches!(
            diagnostic.kind,
            HirDiagnosticKind::EnumVariantConstructorUnknownField { .. }
        )),
        "expected unknown enum variant field diagnostic, got: {:#?}",
        output.diagnostics
    );
    assert!(
        !output.diagnostics.iter().any(|diagnostic| matches!(
            diagnostic.kind,
            HirDiagnosticKind::NotYetImplemented { .. }
        )),
        "enum constructor mistakes must not be reported as unsupported: {:#?}",
        output.diagnostics
    );
}
