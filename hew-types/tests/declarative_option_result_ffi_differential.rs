//! Option/Result receiver methods lower through a closed generic-enum marker.

mod common;

use hew_types::check::{MethodCallRewrite, OptionResultMethod};

use common::typecheck;

fn has_marker(output: &hew_types::TypeCheckOutput, method: OptionResultMethod) -> bool {
    output.method_call_rewrites.values().any(
        |rewrite| matches!(rewrite, MethodCallRewrite::BuiltinOptionResult { method: got } if *got == method),
    )
}

#[test]
fn option_result_methods_record_structured_generic_markers() {
    let source = r#"
        type Point { x: i64; y: i64; }

        fn exercise_option(opt_i64: Option<i64>, opt_str: Option<string>, opt_point: Option<Point>) {
            let _: bool = opt_point.is_some();
            let _: bool = opt_str.is_none();
            let _: i64 = opt_i64.unwrap();
            let _: string = opt_str.unwrap_or("fallback");
            let _: Point = opt_point.unwrap();
        }

        fn exercise_result(r_i64: Result<i64, string>, r_f64: Result<f64, string>, r_point: Result<Point, string>) {
            let _: bool = r_point.is_ok();
            let _: bool = r_i64.is_err();
            let _: i64 = r_i64.unwrap();
            let _: f64 = r_f64.unwrap_or(0.0);
            let _: Point = r_point.unwrap();
        }
    "#;
    let output = typecheck(source);
    assert!(
        output.errors.is_empty(),
        "generic Option/Result receiver methods should typecheck; got: {:#?}",
        output.errors
    );
    for method in [
        OptionResultMethod::OptionIsSome,
        OptionResultMethod::OptionIsNone,
        OptionResultMethod::OptionUnwrap,
        OptionResultMethod::OptionUnwrapOr,
        OptionResultMethod::ResultIsOk,
        OptionResultMethod::ResultIsErr,
        OptionResultMethod::ResultUnwrap,
        OptionResultMethod::ResultUnwrapOr,
    ] {
        assert!(
            has_marker(&output, method),
            "expected {method:?} in method_call_rewrites; got: {:#?}",
            output.method_call_rewrites
        );
    }
    assert!(
        output
            .method_call_rewrites
            .values()
            .all(|rewrite| !matches!(rewrite, MethodCallRewrite::RewriteToFunction { .. })),
        "Option/Result methods must not lower to string runtime symbols: {:#?}",
        output.method_call_rewrites
    );
}

#[test]
fn existing_option_result_module_helpers_still_typecheck() {
    let source = r"
        import std::option;
        import std::result;

        fn main() {
            let opt: Option<i64> = Some(42);
            let _: bool = option.is_some_int(opt);
            let _: i64 = option.unwrap_or_int(opt, 0);

            let res: Result<i64, i64> = Ok(7);
            let _: bool = result.is_ok_int(res);
            let _: i64 = result.unwrap_or_int(res, 0);
        }
    ";
    let output = typecheck(source);
    assert!(
        output.errors.is_empty(),
        "module-level Option/Result helpers should continue to typecheck; got: {:#?}",
        output.errors
    );
}
