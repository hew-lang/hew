use std::path::{Path, PathBuf};

use hew_types::error::TypeErrorKind;

fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .to_path_buf()
}

fn typecheck(source: &str) -> hew_types::TypeCheckOutput {
    let parse_result = hew_parser::parse(source);
    assert!(
        parse_result.errors.is_empty(),
        "should parse cleanly, got: {:#?}",
        parse_result.errors
    );
    let mut checker =
        hew_types::Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![
            repo_root(),
        ]));
    checker.check_program(&parse_result.program)
}

#[test]
fn handle_wrapper_accessor_returning_raw_field_is_rejected() {
    let output = typecheck(
        "
        import std::text::regex;

        type PatternWrapper {
            pattern: regex.Pattern
        }

        impl PatternWrapper {
            fn pattern(wrapper: PatternWrapper) -> regex.Pattern {
                wrapper.pattern
            }
        }
        ",
    );

    assert!(
        output.handle_bearing_structs.contains("PatternWrapper"),
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
        r#"
        import std::text::regex;

        type PatternWrapper {
            pattern: regex.Pattern
        }

        impl PatternWrapper {
            fn matches(wrapper: PatternWrapper, text: String) -> bool {
                wrapper.pattern.is_match(text)
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
