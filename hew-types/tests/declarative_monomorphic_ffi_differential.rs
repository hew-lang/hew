//! W3.002 Stage 1 — monomorphic stdlib FFI canaries.
//!
//! The assertions pin checker-owned method rewrites to the
//! `#[extern_symbol]` annotations declared in stdlib source for
//! `duration`, `instant`, and `LambdaActorHandle`.

mod common;

use hew_types::check::MethodCallRewrite;

use common::typecheck;

fn has_rewrite(output: &hew_types::TypeCheckOutput, symbol: &str) -> bool {
    output.method_call_rewrites.values().any(
        |rewrite| matches!(rewrite, MethodCallRewrite::RewriteToFunction { c_symbol, .. } if c_symbol == symbol),
    )
}

#[test]
fn duration_methods_resolve_through_extern_symbol_annotations() {
    let source = r"
        fn main() {
            let d: duration = 5s;
            let _: i64 = d.nanos();
            let _: i64 = d.micros();
            let _: i64 = d.millis();
            let _: i64 = d.secs();
            let _: i64 = d.mins();
            let _: i64 = d.hours();
            let _: duration = d.abs();
            let _: bool = d.is_zero();
        }
    ";
    let output = typecheck(source);
    assert!(
        output.errors.is_empty(),
        "duration extern-symbol canaries should typecheck; got: {:#?}",
        output.errors
    );
    for symbol in [
        "hew_duration_nanos",
        "hew_duration_micros",
        "hew_duration_millis",
        "hew_duration_secs",
        "hew_duration_mins",
        "hew_duration_hours",
        "hew_duration_abs",
        "hew_duration_is_zero",
    ] {
        assert!(
            has_rewrite(&output, symbol),
            "expected {symbol} in method_call_rewrites; got: {:#?}",
            output.method_call_rewrites
        );
    }
}

#[test]
fn instant_methods_resolve_through_extern_symbol_annotations() {
    // `instant` is a compiler builtin scalar (i64-backed nanosecond timestamp),
    // not a record: instances come from `instant::now()`, and the receiver
    // methods rewrite to their `hew_instant_*` runtime symbols.
    let source = r"
        fn main() {
            let start: instant = instant::now();
            let end: instant = instant::now();
            let _: duration = start.elapsed();
            let _: duration = end.duration_since(start);
        }
    ";
    let output = typecheck(source);
    assert!(
        output.errors.is_empty(),
        "instant extern-symbol canaries should typecheck; got: {:#?}",
        output.errors
    );
    for symbol in ["hew_instant_elapsed", "hew_instant_duration_since"] {
        assert!(
            has_rewrite(&output, symbol),
            "expected {symbol} in method_call_rewrites; got: {:#?}",
            output.method_call_rewrites
        );
    }
}

#[test]
fn lambda_actor_handle_methods_resolve_through_extern_symbol_annotations() {
    let source = r"
        import std::concurrency::lambda_actor;

        fn exercise(
            handle: lambda_actor.LambdaActorHandle,
            weak: lambda_actor.LambdaActorWeakHandle,
        ) {
            let payload = bytes::new();

            let _: i32 = handle.send(payload);
            let _: i32 = handle.ask(bytes::new());
            let _: lambda_actor.LambdaActorHandle = handle.clone();
            let _: i32 = handle.release();
            let _: lambda_actor.LambdaActorWeakHandle = handle.downgrade();
            let _: i32 = weak.send(bytes::new());
            let _: lambda_actor.LambdaActorWeakHandle = weak.clone();
            let _: i32 = weak.release();
        }
    ";
    let output = typecheck(source);
    assert!(
        output.errors.is_empty(),
        "lambda actor extern-symbol canaries should typecheck; got: {:#?}",
        output.errors
    );
    for symbol in [
        "hew_lambda_actor_send",
        "hew_lambda_actor_ask",
        "hew_lambda_actor_clone",
        "hew_lambda_actor_release",
        "hew_lambda_actor_downgrade",
        "hew_lambda_actor_weak_send",
        "hew_lambda_actor_weak_clone",
        "hew_lambda_actor_weak_drop",
    ] {
        assert!(
            has_rewrite(&output, symbol),
            "expected {symbol} in method_call_rewrites; got: {:#?}",
            output.method_call_rewrites
        );
    }
}
