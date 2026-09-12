//! Ordinary closure values retain their facts without speculative refactoring advice.

use crate::common::typecheck;
use hew_types::{
    ClosureCaptureAccess, ClosureCaptureAcquisition, ClosureCaptureConsumption, ClosureEscapeKind,
    ClosureEscapeRule,
};

#[test]
fn returned_closure_factories_keep_escape_and_capture_facts_without_warnings() {
    let output = typecheck(include_str!("../fixtures/closure_factories.hew"));
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    assert!(output.warnings.is_empty(), "{:#?}", output.warnings);
    assert_eq!(output.closure_escape_facts.len(), 4);
    assert_eq!(output.closure_capture_facts.len(), 4);
    for (span, escape) in &output.closure_escape_facts {
        assert_eq!(escape.kind, ClosureEscapeKind::Escapes);
        assert!(matches!(
            escape.rule,
            ClosureEscapeRule::Returned | ClosureEscapeRule::EscapesViaBlockValue
        ));
        let captures = &output.closure_capture_facts[span];
        assert_eq!(captures.len(), 1);
        assert_eq!(captures[0].name, "captured");
        assert_eq!(captures[0].acquisition, ClosureCaptureAcquisition::Snapshot);
        assert_eq!(captures[0].access, ClosureCaptureAccess::Read);
        assert_eq!(captures[0].consumption, ClosureCaptureConsumption::Retained);
    }
}

#[test]
fn unused_and_immediately_invoked_closures_keep_conservative_facts_without_advice() {
    let output = typecheck(
        r"
        fn main() {
            let _unused = || 1;
            println((|| 7)());
        }
        ",
    );
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    assert!(output.warnings.is_empty(), "{:#?}", output.warnings);
    assert_eq!(output.closure_escape_facts.len(), 2);
    for escape in output.closure_escape_facts.values() {
        assert_eq!(escape.kind, ClosureEscapeKind::Escapes);
        assert_eq!(escape.rule, ClosureEscapeRule::NoStaticBinding);
    }
}

#[test]
fn higher_order_closures_keep_escape_facts_without_advice() {
    let output = typecheck(
        r"
        fn apply(f: fn(i64) -> i64, value: i64) -> i64 { f(value) }
        fn main() {
            let double = |x: i64| x * 2;
            println(apply(double, 5));
        }
        ",
    );
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    assert!(output.warnings.is_empty(), "{:#?}", output.warnings);
    assert_eq!(output.closure_escape_facts.len(), 1);
    let escape = output.closure_escape_facts.values().next().unwrap();
    assert_eq!(escape.kind, ClosureEscapeKind::Escapes);
    assert_eq!(escape.rule, ClosureEscapeRule::PassedToHigherOrder);
}
