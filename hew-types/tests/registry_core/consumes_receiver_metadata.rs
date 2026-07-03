//! Consume-receiver metadata plumbing for issue #1295.
//!
//! These tests exercise the `consumes_receiver` seam introduced by the
//! RAII / Closable rollout: a per-method-signature flag that, when set,
//! causes the type checker to mark the receiver expression moved after
//! the call and to record a per-call-site flag in
//! [`TypeCheckOutput::method_call_consumes_receiver`] so codegen can
//! null the receiver's drop slot.
//!
//! PR 1 ships the wiring with the recognised consume set empty (no Hew
//! surface syntax sets the flag yet). Tests inject names directly via
//! [`Checker::register_consume_receiver_method`] to exercise the path
//! end-to-end before PR 2 introduces `Closable::close`.

use crate::common;

use hew_types::check::SpanKey;
use hew_types::error::TypeErrorKind;
use hew_types::module_registry::ModuleRegistry;
use hew_types::traits::{MethodSig, TraitDef, TraitRegistry};
use hew_types::ty::Ty;
use hew_types::Checker;

use common::parse_program;

const TRAIT_AND_IMPL: &str = r#"
    trait Sink {
        fn drain(val: Self);
    }

    type Bucket { tag: string; }

    impl Sink for Bucket {
        fn drain(val: Bucket) {}
    }

    fn main() {
        let b = Bucket { tag: "x" };
        b.drain();
        b.drain();
    }
"#;

fn run_checker(source: &str, register_consume: bool) -> hew_types::TypeCheckOutput {
    let program = parse_program(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    if register_consume {
        checker.register_consume_receiver_method("Sink::drain");
    }
    checker.check_program(&program)
}

/// A method declared with `consumes_receiver = true` causes the receiver
/// binding to be marked moved after the call, surfacing the existing
/// `UseAfterMove` diagnostic on the second call.
#[test]
fn consume_receiver_marks_binding_moved() {
    let output = run_checker(TRAIT_AND_IMPL, /*register_consume=*/ true);
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::UseAfterMove),
        "expected UseAfterMove on second call when Sink::drain consumes its \
         receiver; got errors: {:?}",
        output.errors,
    );
}

/// With `consumes_receiver = false` (the PR 1 default — recognised set
/// empty), the same program type-checks cleanly: the receiver is not
/// marked moved, and the second call is admitted.
#[test]
fn no_consume_receiver_does_not_mark_moved() {
    let output = run_checker(TRAIT_AND_IMPL, /*register_consume=*/ false);
    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::UseAfterMove),
        "did not expect UseAfterMove when consume set is empty; got errors: {:?}",
        output.errors,
    );
}

/// When the consume marker fires, the per-call-site flag is recorded in
/// the side table that codegen consults to null the receiver's drop slot.
#[test]
fn consume_receiver_records_per_call_site_flag() {
    let output = run_checker(TRAIT_AND_IMPL, /*register_consume=*/ true);
    assert!(
        !output.method_call_consumes_receiver.is_empty(),
        "expected at least one call site flagged in \
         method_call_consumes_receiver; got {:?}",
        output.method_call_consumes_receiver,
    );
}

/// Construction-side: `MethodSig` carries `consumes_receiver` as a
/// public field with `false` as the conventional default. Registering an
/// impl preserves the flag through `lookup_impl`. PR 1 ships this field
/// passively; PR 2 wires it from a `Closable` trait declaration.
#[test]
fn method_sig_round_trips_consumes_receiver_flag() {
    let mut reg = TraitRegistry::new();
    reg.register_trait(TraitDef {
        name: "Closable".to_string(),
        type_params: vec![],
        super_traits: vec![],
        methods: vec![MethodSig {
            name: "close".to_string(),
            params: vec![],
            return_type: Ty::Unit,
            takes_self: true,
            self_mutable: false,
            consumes_receiver: true,
        }],
        associated_types: vec![],
    });
    reg.register_impl(
        "Server".to_string(),
        "Closable".to_string(),
        vec![MethodSig {
            name: "close".to_string(),
            params: vec![],
            return_type: Ty::Unit,
            takes_self: true,
            self_mutable: false,
            consumes_receiver: true,
        }],
    );

    let trait_def = reg.lookup_trait("Closable").expect("trait registered");
    assert!(trait_def.methods[0].consumes_receiver);

    let impl_methods = reg
        .lookup_impl("Server", "Closable")
        .expect("impl registered");
    assert!(impl_methods[0].consumes_receiver);
}

/// Sanity: the per-call-site flag in the checker output is keyed by the
/// span of the consuming call, so codegen can look it up by call site.
#[test]
fn flag_span_matches_consuming_call_site() {
    let source = r#"
        trait Sink {
            fn drain(val: Self);
        }

        type Bucket { tag: string; }

        impl Sink for Bucket {
            fn drain(val: Bucket) {}
        }

        fn main() {
            let b = Bucket { tag: "x" };
            b.drain();
        }
    "#;

    let program = parse_program(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.register_consume_receiver_method("Sink::drain");
    let output = checker.check_program(&program);

    assert_eq!(
        output.method_call_consumes_receiver.len(),
        1,
        "expected exactly one consuming call site; got {:?}",
        output.method_call_consumes_receiver,
    );

    // The recorded span must point into the source where `b.drain()` was
    // written. We don't know byte offsets a priori, but the call site
    // must overlap a region containing the literal `b.drain()`.
    let key: &SpanKey = output
        .method_call_consumes_receiver
        .iter()
        .next()
        .expect("set has one entry");
    let snippet = &source[key.start..key.end];
    assert!(
        snippet.contains("b.drain"),
        "expected span to cover `b.drain(...)`; got {snippet:?} at {key:?}",
    );
}
