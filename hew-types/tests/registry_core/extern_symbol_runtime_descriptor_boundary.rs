//! `checker-output-boundary`: an `#[extern_symbol]` FFI method must NOT be
//! reclassified into a typed runtime-call descriptor, even when its resolved C
//! symbol string collides with a [`RuntimeCallFamily`] catalog name.
//!
//! The typed `RewriteToFunction.descriptor` is reserved for closed-set builtin
//! calls the checker resolves with first-class family knowledge. Open-set
//! `#[extern_symbol]` symbols — stdlib `duration` / `instant` bindings
//! as well as user-authored FFI on inherent impls
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
use hew_types::runtime_call::RuntimeCallFamily;
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
fn canonical_stdlib_time_method_carries_its_typed_descriptor() {
    // `duration.hours()` is declared in stdlib via
    // `#[extern_symbol(hew_duration_hours)]` and is a canonical stdlib extern
    // signature, so the checker resolves it to `RuntimeCallFamily::DurationHours`
    // and records the typed descriptor the final path needs. The join is on the
    // exact declaration identity, its trusted stdlib provenance and its checked
    // signature — never on the symbol spelling, which the lookalike test below
    // holds to account.
    assert!(
        RuntimeCallFamily::from_c_symbol("hew_duration_hours").is_some(),
        "precondition: `hew_duration_hours` must be a RuntimeCallFamily catalog name",
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
        Some(true) => { /* correct: the canonical stdlib method is typed */ }
        Some(false) => panic!(
            "`duration.hours()` lost its typed runtime descriptor; the final path \
             refuses a stdlib extern with no runtime family",
        ),
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
