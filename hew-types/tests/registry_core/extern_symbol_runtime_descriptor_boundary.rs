//! `checker-output-boundary`: an `#[extern_symbol]` FFI method must NOT be
//! reclassified into a typed runtime-call descriptor, even when its resolved C
//! symbol string collides with a [`RuntimeCallFamily`] catalog name.
//!
//! The typed `RewriteToFunction.descriptor` is reserved for closed-set builtin
//! calls the checker resolves with first-class family knowledge. Open-set
//! `#[extern_symbol]` symbols — stdlib `duration` / `instant` /
//! `LambdaActorHandle` bindings as well as user-authored FFI on inherent impls
//! — are open-set *by mechanism*: their family is only recoverable by
//! reverse-parsing the symbol string. So they carry `descriptor: None` and
//! consumers fall back to the raw `c_symbol`.
//!
//! Regression guard for the seam where a single rewrite recorder reverse-parsed
//! the resolved C symbol into a descriptor for *every* producer — including the
//! open-set `#[extern_symbol]` path — so a user FFI binding (or a stdlib
//! `#[extern_symbol]` method) whose symbol matched a catalog entry was silently
//! lifted into a runtime ABI descriptor. These tests FAIL if that single-path
//! reverse-parse is reintroduced: the `from_c_symbol(..).is_some()` precondition
//! proves the symbol IS a catalog member, so a `Some` descriptor here can only
//! mean the extern method was reclassified.

use crate::common;

use hew_types::check::{MethodCallRewrite, SpanKey};
use hew_types::error::TypeErrorKind;
use hew_types::runtime_call::{ProducedArgumentBoundary, RuntimeCallFamily};
use hew_types::TypeCheckOutput;

use common::{parse_and_typecheck_inline, typecheck};

/// Locate the `RewriteToFunction` recorded for `symbol` and report whether it
/// carries a typed runtime-call descriptor.
///
/// Returns `None` when no rewrite names `symbol`, so a test that expected the
/// extern method to be rewritten fails loudly instead of passing vacuously.
fn descriptor_present_for(output: &TypeCheckOutput, symbol: &str) -> Option<bool> {
    output
        .method_call_rewrites
        .values()
        .find_map(|rewrite| match rewrite {
            MethodCallRewrite::RewriteToFunction {
                c_symbol,
                descriptor,
                ..
            } if c_symbol == symbol => Some(descriptor.is_some()),
            _ => None,
        })
}

fn function_rewrite_for(output: &TypeCheckOutput, symbol: &str) -> Option<(SpanKey, bool, bool)> {
    output
        .method_call_rewrites
        .iter()
        .find_map(|(site, rewrite)| match rewrite {
            MethodCallRewrite::RewriteToFunction {
                c_symbol,
                descriptor,
                consumes_receiver,
                ..
            } if c_symbol == symbol => {
                Some((site.clone(), descriptor.is_some(), *consumes_receiver))
            }
            _ => None,
        })
}

#[test]
fn stdlib_extern_symbol_method_colliding_with_catalog_has_no_descriptor() {
    // `duration.hours()` is declared in stdlib via
    // `#[extern_symbol(hew_duration_hours)]`, and `hew_duration_hours` IS a
    // catalog entry (`RuntimeCallFamily::DurationHours`). The collision is the
    // whole point: descriptor must still be `None`.
    assert!(
        RuntimeCallFamily::from_c_symbol("hew_duration_hours").is_some(),
        "precondition: `hew_duration_hours` must be a RuntimeCallFamily catalog \
         name for this collision test to be meaningful",
    );

    let output = typecheck(
        r"
        fn main() {
            let d: duration = 5s;
            let _: i64 = d.hours();
            let _: i64 = d.hours();
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "duration.hours() should typecheck: {:#?}",
        output.errors,
    );

    match descriptor_present_for(&output, "hew_duration_hours") {
        Some(true) => panic!(
            "checker-output-boundary violation: the #[extern_symbol] method \
             `duration.hours()` was reclassified into a typed runtime descriptor \
             because its symbol collides with RuntimeCallFamily::DurationHours",
        ),
        Some(false) => { /* correct: open-set extern symbol carries no descriptor */ }
        None => panic!(
            "expected a RewriteToFunction recorded for `hew_duration_hours`; \
             rewrites: {:#?}",
            output.method_call_rewrites,
        ),
    }

    let (site, _, consumes_receiver) =
        function_rewrite_for(&output, "hew_duration_hours").expect("duration.hours rewrite");
    assert!(
        !consumes_receiver,
        "duration.hours must borrow its receiver"
    );
    assert!(
        !output.method_call_consumes_receiver.contains(&site),
        "non-consuming extern method must not enter the consume side table",
    );
    assert_eq!(
        output
            .produced_value_ownership
            .get(&site)
            .and_then(|fact| fact.receiver_boundary),
        Some(ProducedArgumentBoundary::Borrow),
        "non-consuming extern method must publish an exact borrow boundary",
    );
}

#[test]
fn user_extern_symbol_method_colliding_with_catalog_has_no_descriptor() {
    // The direct threat model: a user inherent impl binds an FFI method to a
    // symbol that string-matches a runtime catalog entry. The checker must treat
    // it as open-set FFI — record the rewrite (so HIR can emit the call) but
    // leave the typed descriptor `None`.
    assert!(
        RuntimeCallFamily::from_c_symbol("hew_duration_hours").is_some(),
        "precondition: the colliding symbol must be a catalog name",
    );

    let (_, output) = parse_and_typecheck_inline(
        r"
        type Widget { tag: i64 }

        impl Widget {
            #[extern_symbol(hew_duration_hours)]
            fn poke(self) -> i64 { return 0; }
        }

        fn main() {
            let w: Widget = Widget { tag: 1 };
            let _: i64 = w.poke();
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "user extern-symbol method should typecheck: {:#?}",
        output.errors,
    );

    match descriptor_present_for(&output, "hew_duration_hours") {
        Some(true) => panic!(
            "checker-output-boundary violation: a user #[extern_symbol] FFI symbol \
             colliding with a catalog name was reclassified into a typed runtime \
             descriptor",
        ),
        Some(false) => { /* correct */ }
        None => panic!(
            "expected a RewriteToFunction recorded for the user extern symbol; \
             rewrites: {:#?}",
            output.method_call_rewrites,
        ),
    }
}

#[test]
fn extern_symbol_consuming_release_keeps_consume_mark_without_descriptor() {
    // `LambdaActorHandle.release()` binds `#[extern_symbol(hew_lambda_actor_release)]`
    // — a genuine consuming handle release AND a catalog name
    // (`RuntimeCallFamily::LambdaActorRelease`). The descriptor must be `None`
    // (open-set mechanism), but `consumes_receiver` MUST stay true: dropping the
    // consume mark would let the handle's scope-exit drop fire on already-freed
    // memory (double-free). This pins the deliberate asymmetry — descriptor is a
    // forward-compat typed fact the checker cannot honestly assert for an extern
    // symbol, while consume is a load-bearing ownership fact with no other source.
    assert!(
        RuntimeCallFamily::from_c_symbol("hew_lambda_actor_release").is_some(),
        "precondition: `hew_lambda_actor_release` must be a catalog name",
    );

    let (_, output) = parse_and_typecheck_inline(
        r"
        import std::concurrency::lambda_actor;

        fn exercise(handle: lambda_actor.LambdaActorHandle) {
            handle.release();
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "LambdaActorHandle.release() should typecheck: {:#?}",
        output.errors,
    );

    let release = function_rewrite_for(&output, "hew_lambda_actor_release");

    match release {
        Some((site, descriptor_present, consumes_receiver)) => {
            assert!(
                !descriptor_present,
                "extern-symbol release must not carry a typed descriptor",
            );
            assert!(
                consumes_receiver,
                "extern-symbol consuming release MUST keep its consume mark \
                 (else the handle double-frees at scope exit)",
            );
            assert!(
                output.method_call_consumes_receiver.contains(&site),
                "extern-symbol release must survive in the checker consume side table",
            );
            assert_eq!(
                output
                    .produced_value_ownership
                    .get(&site)
                    .and_then(|fact| fact.receiver_boundary),
                Some(ProducedArgumentBoundary::Transfer),
                "extern-symbol release must publish the exact transfer boundary",
            );
        }
        None => panic!(
            "expected a RewriteToFunction recorded for `hew_lambda_actor_release`; \
             rewrites: {:#?}",
            output.method_call_rewrites,
        ),
    }

    let (_, reused_output) = parse_and_typecheck_inline(
        r"
        import std::concurrency::lambda_actor;

        fn exercise(handle: lambda_actor.LambdaActorHandle) {
            handle.release();
            handle.release();
        }
        ",
    );
    assert!(
        reused_output
            .errors
            .iter()
            .any(|error| error.kind == TypeErrorKind::UseAfterMove),
        "a second use of the consuming extern receiver must fail as moved: {:#?}",
        reused_output.errors,
    );
}
