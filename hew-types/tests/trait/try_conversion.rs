//! The failure-edge rule (D547): `?` and `return error` carry an error into
//! the enclosing function's error type by the first rule that holds - the
//! same type, erasure into a trait object, or a declared `impl From<E> for F`
//! - and the checker records that choice for lowering at the edge's span.

use crate::common;

use common::typecheck;
use hew_types::error::TypeErrorKind;
use hew_types::{ErrorConversion, TypeCheckOutput};

const ERRORS: &str = r#"
enum Low {
    Broken;
}

impl Display for Low {
    fn fmt(self) -> string {
        "Broken"
    }
}

impl Error for Low {}

enum Mid {
    Wrapped(Low);
    Code(i64);
}

impl Display for Mid {
    fn fmt(self) -> string {
        match self {
            .Wrapped(e) => f"Wrapped: {e}",
            .Code(n) => f"Code: {n}",
        }
    }
}

impl Error for Mid {}

impl From<Low> for Mid {
    fn from(value: Low) -> Mid {
        .Wrapped(value)
    }
}

impl From<i64> for Mid {
    fn from(value: i64) -> Mid {
        .Code(value)
    }
}

enum Top {
    Wrapped(Mid);
}

impl Display for Top {
    fn fmt(self) -> string {
        match self {
            .Wrapped(e) => f"Wrapped: {e}",
        }
    }
}

impl Error for Top {}

impl From<Mid> for Top {
    fn from(value: Mid) -> Top {
        .Wrapped(value)
    }
}

fn low() -> Result<i64, Low> {
    .Err(.Broken)
}

fn code() -> Result<i64, i64> {
    .Err(7)
}
"#;

fn check(body: &str) -> TypeCheckOutput {
    typecheck(&format!("{ERRORS}\n{body}"))
}

fn assert_clean(output: &TypeCheckOutput) {
    assert!(
        output.errors.is_empty(),
        "expected a clean check, got: {:#?}",
        output.errors
    );
}

fn from_methods(output: &TypeCheckOutput) -> Vec<hew_types::DefId> {
    let mut methods: Vec<_> = output
        .error_conversions
        .values()
        .filter_map(|conversion| match conversion {
            ErrorConversion::From { method } => Some(*method),
            _ => None,
        })
        .collect();
    methods.sort_unstable();
    methods
}

fn no_conversion(output: &TypeCheckOutput) -> &hew_types::TypeError {
    output
        .errors
        .iter()
        .find(|err| err.kind == TypeErrorKind::ErrorNoConversion)
        .unwrap_or_else(|| panic!("expected E_ERROR_NO_CONVERSION, got: {:#?}", output.errors))
}

#[test]
fn each_declared_from_impl_converts_its_own_source() {
    let output = check(
        r"
fn both() -> i64 fails Mid {
    let a = low()?;
    let b = code()?;
    a + b
}
",
    );
    assert_clean(&output);
    let methods = from_methods(&output);
    assert_eq!(methods.len(), 2, "one From edge per `?`: {methods:?}");
    assert_ne!(
        methods[0], methods[1],
        "`From<Low>` and `From<i64>` are distinct impl methods"
    );
}

#[test]
fn return_error_selects_from_like_question_mark() {
    let output = check(
        r"
fn by_return(n: i64) -> i64 fails Mid {
    if n > 0 {
        return error n;
    }
    return error Low.Broken;
}
",
    );
    assert_clean(&output);
    assert_eq!(from_methods(&output).len(), 2);
}

#[test]
fn same_type_and_erasure_are_recorded_at_the_edge() {
    let output = check(
        r#"
fn same() -> i64 fails Low {
    let a = low()?;
    a
}

fn erased() -> i64 fails dyn Error {
    let a = low()?;
    if a > 1 {
        return error f"too large: {a}";
    }
    a
}
"#,
    );
    assert_clean(&output);
    let kinds: Vec<_> = output.error_conversions.values().collect();
    assert!(kinds.contains(&&ErrorConversion::Same), "{kinds:?}");
    assert_eq!(
        kinds
            .iter()
            .filter(|kind| matches!(kind, ErrorConversion::Erase(_)))
            .count(),
        2,
        "both edges into `dyn Error` erase: {kinds:?}"
    );
}

#[test]
fn conversions_never_chain() {
    let output = check(
        r"
fn chained() -> i64 fails Top {
    let a = low()?;
    a
}
",
    );
    let error = no_conversion(&output);
    assert!(
        error.message.contains("`Low`") && error.message.contains("`Top`"),
        "the diagnostic names both types: {}",
        error.message
    );
}

#[test]
fn missing_conversion_offers_all_three_repairs() {
    let output = check(
        r"
fn unconverted() -> i64 fails Low {
    let n = code()?;
    n
}
",
    );
    let error = no_conversion(&output);
    assert!(
        error.message.contains("E_ERROR_NO_CONVERSION")
            && error.message.contains("`i64`")
            && error.message.contains("`Low`"),
        "{}",
        error.message
    );
    assert_eq!(error.suggestions.len(), 3, "{:#?}", error.suggestions);
    assert!(error.suggestions[0].contains("impl From<i64> for Low"));
    assert!(error.suggestions[1].contains("map_err"));
    assert!(error.suggestions[2].contains("fails dyn Error"));
}

#[test]
fn a_value_that_is_not_an_error_does_not_erase() {
    let output = check(
        r#"
type Bare {
    detail: string;
}

fn parse() -> i64 fails dyn Error {
    return error Bare { detail: "empty" };
}
"#,
    );
    let error = no_conversion(&output);
    assert!(
        error.message.contains("erase") && error.message.contains("Bare"),
        "{}",
        error.message
    );
}

#[test]
fn only_failure_edges_convert() {
    // A constructed `Err` and a `handle` block value keep their exact type.
    let output = check(
        r"
fn constructed() -> Result<i64, Mid> {
    .Err(Low.Broken)
}

fn recovered() -> Mid {
    let n = low() handle e { e };
    Mid.Code(n)
}
",
    );
    let mismatches = output
        .errors
        .iter()
        .filter(|err| matches!(err.kind, TypeErrorKind::Mismatch { .. }))
        .count();
    assert!(
        mismatches >= 2,
        "`.Err(low)` and the handle value are both refused: {:#?}",
        output.errors
    );
    assert!(from_methods(&output).is_empty());
}

#[test]
fn option_does_not_convert_to_result() {
    let output = check(
        r"
fn absent(o: Option<i64>) -> i64 fails Mid {
    let n = o?;
    n
}
",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|err| err.message.contains("absence requires an Option return")),
        "{:#?}",
        output.errors
    );
    let output = check(
        r"
fn present(consume o: Option<i64>) -> i64 fails Mid {
    let n = o.ok_or(Mid.Code(0))?;
    n
}
",
    );
    assert_clean(&output);
}

#[test]
fn invalid_from_impls_are_refused_by_case() {
    let output = check(
        r"
impl From<i64> for dyn Error {
    fn from(value: i64) -> dyn Error {
        Low.Broken
    }
}

impl<T> From<T> for Top {
    fn from(value: T) -> Top {
        Top.Wrapped(Mid.Code(0))
    }
}
",
    );
    let messages: Vec<_> = output
        .errors
        .iter()
        .filter(|err| err.kind == TypeErrorKind::FromInvalid)
        .map(|err| err.message.as_str())
        .collect();
    assert!(
        messages.iter().any(|m| m.contains("trait object")),
        "{messages:?}"
    );
    assert!(
        messages.iter().any(|m| m.contains("bare type parameter")),
        "{messages:?}"
    );
}

#[test]
fn identity_from_impl_is_refused() {
    let output = check(
        r"
impl From<Low> for Low {
    fn from(value: Low) -> Low {
        value
    }
}
",
    );
    assert!(
        output.errors.iter().any(
            |err| err.kind == TypeErrorKind::FromInvalid && err.message.contains("into itself")
        ),
        "{:#?}",
        output.errors
    );
}

#[test]
fn string_is_an_error() {
    let output = typecheck(
        r#"
fn check_port(port: i64) -> i64 fails dyn Error {
    if port > 65535 {
        return error f"port {port} is out of range";
    }
    port
}

fn main() fails string {
    return error "plain failure";
}
"#,
    );
    assert_clean(&output);
}

#[test]
fn entry_error_without_error_impl_names_both_repairs() {
    let output = typecheck(
        r"
fn main() -> Result<(), i64> {
    .Err(1)
}
",
    );
    let error = output
        .errors
        .iter()
        .find(|err| err.kind == TypeErrorKind::BoundsNotSatisfied)
        .unwrap_or_else(|| panic!("expected the entry bound, got: {:#?}", output.errors));
    assert!(
        error
            .suggestions
            .iter()
            .any(|help| help.contains("implement `Error` for `i64`")
                && help.contains("fails dyn Error")),
        "{:#?}",
        error.suggestions
    );
}
