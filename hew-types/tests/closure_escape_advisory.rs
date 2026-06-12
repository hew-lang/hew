//! Tests for the `ClosureEscapeAdvisory` warning's per-span dedup.
//!
//! The escape classifier visits a closure literal more than once (the
//! let-bound block walk and the anonymous-expression walk, and the
//! top-level item list plus the module graph both cover the entry module),
//! so without a seen-span gate the advisory printed twice for the same
//! literal. One warning per literal; distinct literals warn independently.

mod common;

use common::{typecheck, warnings_of_kind};
use hew_types::error::TypeErrorKind;

fn escape_advisories(output: &hew_types::TypeCheckOutput) -> usize {
    warnings_of_kind(
        output,
        &TypeErrorKind::ClosureEscapeAdvisory {
            rule: "EscapesViaBlockValue".to_string(),
        },
    )
    .len()
}

/// A single escaping closure literal emits exactly one advisory.
#[test]
fn single_escaping_closure_warns_once() {
    let output = typecheck(
        r"
        fn get() -> fn(i64) -> i64 {
            |x: i64| x + 1
        }
        fn main() {
            let f = get();
            println(f(41));
        }
        ",
    );
    assert_eq!(
        escape_advisories(&output),
        1,
        "expected exactly one ClosureEscapeAdvisory, got: {:#?}",
        output.warnings
    );
}

/// Two distinct escaping literals each emit their own advisory — the
/// per-span dedup must not collapse distinct spans.
#[test]
fn two_distinct_escaping_closures_warn_once_each() {
    let output = typecheck(
        r"
        fn pick(up: bool) -> fn(i64) -> i64 {
            if up { |x: i64| x + 1 } else { |x: i64| x - 1 }
        }
        fn main() {
            let f = pick(true);
            println(f(41));
        }
        ",
    );
    assert_eq!(
        escape_advisories(&output),
        2,
        "expected one advisory per distinct literal, got: {:#?}",
        output.warnings
    );
}
