mod common;

use common::typecheck_isolated as typecheck;
use hew_types::error::TypeErrorKind;

#[test]
fn extern_call_requires_unsafe() {
    let source = r#"
extern "C" {
    fn abs(n: int) -> int;
}

fn main() {
    let result = abs(-42);
}
"#;
    let output = typecheck(source);
    assert!(
        output.errors.iter().any(|error| {
            error.kind == TypeErrorKind::InvalidOperation
                && error.message == "calling extern function `abs` requires `unsafe { ... }`"
        }),
        "expected explicit unsafe-call diagnostic, got: {:?}",
        output.errors
    );
}
