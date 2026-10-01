//! Bounds on generic binders resolve to a trait and its arguments
//! (`F: From<Low>`), are satisfied by the impls declared at those arguments,
//! and select the associated functions a binder names (`T.make(n)`).

use crate::common;

use common::typecheck;
use hew_types::error::TypeErrorKind;
use hew_types::{CallTarget, TypeCheckOutput};

const TYPES: &str = r"
type Low {
    code: i64;
}

type High {
    code: i64;
}

type Wrapped {
    code: i64;
}

impl From<Low> for Wrapped {
    fn from(value: Low) -> Wrapped {
        Wrapped { code: value.code }
    }
}

type Narrow {
    code: i64;
}

impl From<High> for Narrow {
    fn from(value: High) -> Narrow {
        Narrow { code: value.code }
    }
}

fn need<F: From<Low>>(value: F) -> F {
    value
}

trait Make {
    fn make(n: i64) -> Self;
    fn value(self) -> i64;
}

type Cell {
    v: i64;
}

impl Make for Cell {
    fn make(n: i64) -> Cell {
        Cell { v: n }
    }

    fn value(self) -> i64 {
        self.v
    }
}
";

fn check(body: &str) -> TypeCheckOutput {
    typecheck(&format!("{TYPES}\n{body}"))
}

fn assert_clean(output: &TypeCheckOutput) {
    assert!(
        output.errors.is_empty(),
        "expected a clean check, got: {:#?}",
        output.errors
    );
}

fn error_of(output: &TypeCheckOutput, kind: &TypeErrorKind) -> String {
    output
        .errors
        .iter()
        .find(|error| &error.kind == kind)
        .unwrap_or_else(|| panic!("expected {kind:?}, got: {:#?}", output.errors))
        .message
        .clone()
}

/// The one binder call the program makes, as `(Self spelling, method name)`.
/// The selection is complete, and refused until code generation can take
/// `Self` from the binder's instantiation.
fn binder_call(output: &TypeCheckOutput) -> (String, String) {
    let refusals: Vec<_> = output.errors.iter().collect();
    let [refusal] = refusals.as_slice() else {
        panic!("expected only the build refusal, got: {refusals:#?}");
    };
    assert_eq!(refusal.kind, TypeErrorKind::InvalidOperation);
    assert!(
        refusal.message.contains("cannot build yet"),
        "{}",
        refusal.message
    );
    let calls: Vec<_> = output.binder_trait_calls.values().collect();
    let [call] = calls.as_slice() else {
        panic!("expected one binder call, got: {calls:#?}");
    };
    let CallTarget::StaticTraitMethod { method, .. } = call.target else {
        panic!("a binder call targets its trait method: {:?}", call.target);
    };
    (
        call.self_param.spelling.to_string(),
        output.defs.name(method).to_string(),
    )
}

#[test]
fn positional_bound_is_satisfied_by_the_impl_at_its_arguments() {
    let output = check(
        r"
fn main() {
    let wrapped = need(Wrapped { code: 4 });
    println(wrapped.code);
}
",
    );
    assert_clean(&output);
}

#[test]
fn positional_bound_refuses_an_impl_at_other_arguments() {
    let output = check(
        r"
fn main() {
    let narrow = need(Narrow { code: 4 });
    println(narrow.code);
}
",
    );
    let message = error_of(&output, &TypeErrorKind::BoundsNotSatisfied);
    assert!(
        message.contains("From<Low>"),
        "the refusal names the bound with its argument: {message}"
    );
}

#[test]
fn positional_bound_with_the_wrong_arity_is_refused() {
    let output = check(
        r"
fn both<F: From<Low, High>>(value: F) -> F {
    value
}
",
    );
    error_of(
        &output,
        &TypeErrorKind::UnknownTraitBoundShape {
            trait_name: "From".to_string(),
        },
    );
}

#[test]
fn a_binder_provides_a_positional_bound_only_at_its_own_arguments() {
    let refused = check(
        r"
fn outer<G: From<High>>(value: G) -> G {
    need(value)
}
",
    );
    error_of(&refused, &TypeErrorKind::BoundsNotSatisfied);

    let accepted = check(
        r"
fn outer<G: From<Low>>(value: G) -> G {
    need(value)
}
",
    );
    assert_clean(&accepted);
}

#[test]
fn a_binder_names_the_associated_function_its_bound_declares() {
    let output = check(
        r"
fn build<T: Make>(n: i64) -> T {
    T.make(n)
}
",
    );
    assert_eq!(binder_call(&output), ("T".to_string(), "make".to_string()));
}

#[test]
fn a_binder_converts_through_a_positional_from_bound() {
    let output = check(
        r"
fn lift<F: From<Low>>(low: Low) -> F {
    F.from(low)
}
",
    );
    assert_eq!(binder_call(&output), ("F".to_string(), "from".to_string()));
}

#[test]
fn a_binder_call_checks_its_arguments_against_the_bound_arguments() {
    let output = check(
        r"
fn lift<F: From<Low>>(high: High) -> F {
    F.from(high)
}
",
    );
    error_of(
        &output,
        &TypeErrorKind::Mismatch {
            expected: "Low".to_string(),
            actual: "High".to_string(),
        },
    );
}

#[test]
fn a_binder_without_a_bound_declaring_the_function_is_refused() {
    let output = check(
        r"
fn build<T: Display>(n: i64) -> T {
    T.make(n)
}
",
    );
    let message = error_of(&output, &TypeErrorKind::UndefinedMethod);
    assert!(
        message.contains("no associated function `make`"),
        "{message}"
    );
}

#[test]
fn a_binder_does_not_call_a_receiver_method() {
    let output = check(
        r"
fn read<T: Make>() -> i64 {
    T.value()
}
",
    );
    let message = error_of(&output, &TypeErrorKind::UndefinedMethod);
    assert!(message.contains("takes a receiver"), "{message}");
}

#[test]
fn a_value_named_like_no_binder_is_still_a_value() {
    let output = check(
        r"
fn read<T: Make>(t: T) -> i64 {
    t.value()
}
",
    );
    assert_clean(&output);
    assert!(output.binder_trait_calls.is_empty());
}

#[test]
fn a_trait_default_names_an_associated_function_through_self() {
    let output = check(
        r"
trait Remake {
    fn remake(n: i64) -> Self;
    fn again(self) -> Self {
        Self.remake(2)
    }
}
",
    );
    assert_eq!(
        binder_call(&output),
        ("Self".to_string(), "remake".to_string())
    );
}
