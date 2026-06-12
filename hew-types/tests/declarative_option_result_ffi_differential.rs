//! W3.002 Stage 3 — Option/Result declarative FFI cutover.
//!
//! Pins builtin `Option<T>` / `Result<T, E>` receiver methods to the
//! `#[extern_symbol]` annotations declared in `std/option.hew` and
//! `std/result.hew`, including `{T}` template expansion for non-Vec receivers.

mod common;

use hew_types::check::MethodCallRewrite;
use hew_types::error::TypeErrorKind;

use common::typecheck;

fn has_rewrite(output: &hew_types::TypeCheckOutput, symbol: &str) -> bool {
    output.method_call_rewrites.values().any(
        |rewrite| matches!(rewrite, MethodCallRewrite::RewriteToFunction { c_symbol, .. } if c_symbol == symbol),
    )
}

#[test]
fn option_result_methods_resolve_through_std_extern_symbols() {
    let source = r"
        fn exercise_option(opt_i32: Option<i32>, opt_i64: Option<i64>, opt_f64: Option<f64>) {
            let _: bool = opt_i32.is_some();
            let _: bool = opt_i64.is_none();
            let _: i32 = opt_i32.unwrap();
            let _: i64 = opt_i64.unwrap_or(7);
            let _: f64 = opt_f64.unwrap_or(1.0);
        }

        fn exercise_result(r_i32: Result<i32, string>, r_i64: Result<i64, string>, r_f64: Result<f64, string>) {
            let _: bool = r_i32.is_ok();
            let _: bool = r_i64.is_err();
            let _: i32 = r_i32.unwrap();
            let _: i64 = r_i64.unwrap_or(7);
            let _: f64 = r_f64.unwrap();
        }
    ";
    let output = typecheck(source);
    assert!(
        output.errors.is_empty(),
        "Option/Result receiver extern-symbol methods should typecheck; got: {:#?}",
        output.errors
    );
    for symbol in [
        "hew_option_is_some",
        "hew_option_is_none",
        "hew_option_unwrap_i32",
        "hew_option_unwrap_or_i64",
        "hew_option_unwrap_or_f64",
        "hew_result_is_ok",
        "hew_result_is_err",
        "hew_result_unwrap_i32",
        "hew_result_unwrap_or_i64",
        "hew_result_unwrap_f64",
    ] {
        assert!(
            has_rewrite(&output, symbol),
            "expected {symbol} in method_call_rewrites; got: {:#?}",
            output.method_call_rewrites
        );
    }
}

#[test]
fn option_result_unsupported_runtime_symbols_fail_closed() {
    let source = r"
        fn option_string(opt: Option<string>) {
            let _: string = opt.unwrap();
        }

        fn result_f64_or(r: Result<f64, string>) {
            let _: f64 = r.unwrap_or(0.0);
        }
    ";
    let output = typecheck(source);
    for expected_symbol in ["hew_option_unwrap_str", "hew_result_unwrap_or_f64"] {
        assert!(
            output.errors.iter().any(|err| {
                err.kind == TypeErrorKind::InvalidOperation && err.message.contains(expected_symbol)
            }),
            "expected fail-closed diagnostic mentioning {expected_symbol}; got: {:#?}",
            output.errors
        );
        assert!(
            !has_rewrite(&output, expected_symbol),
            "unsupported {expected_symbol} must not record a rewrite; got: {:#?}",
            output.method_call_rewrites
        );
    }
}

#[test]
fn option_result_unsupported_calling_conventions_fail_closed() {
    let source = r"
        fn option_bool(opt: Option<bool>) {
            let _: bool = opt.unwrap();
        }

        fn result_bool(r: Result<bool, string>) {
            let _: bool = r.unwrap();
        }
    ";
    let output = typecheck(source);
    for expected_symbol in ["hew_option_unwrap_bool", "hew_result_unwrap_bool"] {
        assert!(
            output.errors.iter().any(|err| {
                err.kind == TypeErrorKind::InvalidOperation
                    && err.message.contains(expected_symbol)
            }),
            "expected unsupported calling-convention diagnostic mentioning {expected_symbol}; got: {:#?}",
            output.errors
        );
        assert!(
            !has_rewrite(&output, expected_symbol),
            "unsupported {expected_symbol} must not record a rewrite; got: {:#?}",
            output.method_call_rewrites
        );
    }
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
        "existing std::option/std::result helper functions should still typecheck; got: {:#?}",
        output.errors
    );
}
