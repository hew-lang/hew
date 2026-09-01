//! Callable-key child synthesis tests, kept beside the lowering module so the
//! production coordinator remains below the structural line ceiling.

use super::*;

#[test]
fn a_builder_carrying_a_callable_identity_mints_ordinal_children_of_it() {
    let parent = crate::model::MirCallableKey::for_test("app.owner");
    let mut builder = Builder {
        current_callable_key: Some(parent.clone()),
        ..Builder::default()
    };

    let first =
        builder.mint_synthesized_child_key(crate::model::SynthesizedCallable::GeneratorBody);
    let second =
        builder.mint_synthesized_child_key(crate::model::SynthesizedCallable::ClosureInvokeShim);

    assert_eq!(
        first,
        parent.child(crate::model::SynthesizedCallable::GeneratorBody(0))
    );
    assert_eq!(
        second,
        parent.child(crate::model::SynthesizedCallable::ClosureInvokeShim(1)),
        "one shared per-parent sequence: the second child is ordinal 1 even though it \
         is the first of its variant"
    );
}

#[test]
#[should_panic(expected = "carries no callable identity")]
fn a_builder_with_no_callable_identity_refuses_to_mint_a_child_key() {
    // The negative control for the `current_callable_key: Option<..>`
    // fail-closed claim. Without it the producer would have to invent an
    // identity from the emitted symbol — the exact reconstruction this slice
    // removes — and two synthesized callables could then collide.
    let mut builder = Builder::default();
    let _ = builder.mint_synthesized_child_key(crate::model::SynthesizedCallable::GeneratorBody);
}
