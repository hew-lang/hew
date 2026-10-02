use crate::common;

use common::typecheck;
use hew_types::error::TypeErrorKind;

#[test]
fn handle_wrapper_accessor_returning_raw_field_is_rejected() {
    let output = typecheck(
        "import std.text.regex;\n\ntype PatternWrapper {\n    pattern: regex.Pattern;\n}\n\nimpl PatternWrapper {\n    fn pattern(self) -> regex.Pattern {\n        self.pattern\n    }\n}\n",
    );

    assert!(
        output
            .handle_bearing_structs
            .contains(&output.defs.lookup_nominal("PatternWrapper").unwrap()),
        "expected PatternWrapper to be marked handle-bearing, got: {:#?}",
        output.handle_bearing_structs
    );
    assert!(
        output.errors.iter().any(|error| {
            error.kind == TypeErrorKind::InvalidOperation
                && error
                    .message
                    .contains("exposes owned handle field `pattern`")
                && error.message.contains("double-free")
        }),
        "expected owned-handle accessor rejection, got: {:#?}",
        output.errors
    );
}

#[test]
fn handle_wrapper_methods_can_use_inner_handle_without_exposing_it() {
    let output = typecheck(
        r#"import std.text.regex;

type PatternWrapper {
    pattern: regex.Pattern;
}

impl PatternWrapper {
    fn matches(self, text: string) -> bool {
        self.pattern.is_match(text)
    }
}

fn main() {
    let wrapper = PatternWrapper { pattern: regex.new("a+") };
    assert(wrapper.matches("aaa"));
}
"#,
    );

    assert!(
        output.errors.is_empty(),
        "expected safe handle-wrapper method to typecheck, got: {:#?}",
        output.errors
    );
}
