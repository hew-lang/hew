//! Option/Result receiver methods dispatch to their std source declarations.

use crate::common;

use hew_types::check::MethodCallRewrite;
use hew_types::CallTarget;

use common::typecheck;

#[test]
fn option_result_methods_dispatch_to_std_declarations() {
    let source = r#"
        type Point { x: i64, y: i64, }

        fn exercise_option(opt_i64: Option<i64>, opt_str: Option<string>, consume opt_point: Option<Point>) {
            let _: bool = opt_point.is_some();
            let _: bool = opt_str.is_none();
            let _: Point = opt_point.expect("the value is present");
        }

        fn exercise_result(r_i64: Result<i64, string>, consume r_f64: Result<f64, string>) {
            let _: bool = r_i64.is_err();
            let _: f64 = r_f64.unwrap_or(0.0);
        }
    "#;
    let output = typecheck(source);
    assert!(
        output.errors.is_empty(),
        "generic Option/Result receiver methods should typecheck; got: {:#?}",
        output.errors
    );
    let targets: Vec<&str> = output
        .method_call_rewrites
        .values()
        .filter_map(|rewrite| match rewrite {
            MethodCallRewrite::RewriteToFunction {
                target: CallTarget::ImplMethod(declaration),
                ..
            } => Some(declaration.full_path()),
            _ => None,
        })
        .collect();
    for (owner, method) in [
        ("std.option", "is_some"),
        ("std.option", "is_none"),
        ("std.option", "expect"),
        ("std.result", "is_err"),
        ("std.result", "unwrap_or"),
    ] {
        assert!(
            targets
                .iter()
                .any(|path| path.starts_with(owner) && path.ends_with(&format!("::{method}"))),
            "expected `{method}` to dispatch to {owner}; got: {targets:?}"
        );
    }
}

#[test]
fn existing_option_result_module_helpers_still_typecheck() {
    let source = r"
        import std.option;
        import std.result;

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
