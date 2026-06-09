//! Stage 1 of the iteration-foundation lane: pin the `Iterator` and
//! `IntoIterator` trait surfaces as they appear in `std/builtins.hew`.
//!
//! These tests parse and type-check the trait declarations through the real
//! parser and checker, confirm a user-defined `impl Iterator for FakeCounter`
//! type-checks against the locked shape (`fn next(self) -> Option<Self::Item>`
//! — Q001), and confirm that a mismatched `next` signature is rejected loudly
//! rather than silently accepted.
//!
//! The `IntoIterator` surface adds the locked associated-type bound
//! `type IntoIter: Iterator<Item = Self::Item>` so that callers projecting
//! `<T as IntoIterator>::IntoIter::Item` always agree with
//! `<T as IntoIterator>::Item`.

#![allow(
    clippy::missing_panics_doc,
    reason = "integration-test panics on assertion failure are intentional"
)]

mod common;

use common::typecheck_isolated;
use hew_parser::ast::{Item, TraitItem};
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;

/// The locked v0.5 trait surface as it appears in `std/builtins.hew`. Pinning
/// it inline keeps these tests hermetic to unrelated stdlib drift; the
/// `stdlib_builtins_*` test below confirms the live file still carries both
/// trait names.
const ITER_TRAIT_PRELUDE: &str = r"
pub trait Iterator {
    type Item;

    fn next(self) -> Option<Self::Item>;
}

pub trait IntoIterator {
    type Item;
    type IntoIter: Iterator<Item = Self::Item>;

    fn into_iter(self) -> Self::IntoIter;
}
";

fn assert_no_errors(label: &str, output: &hew_types::TypeCheckOutput) {
    assert!(
        output.errors.is_empty(),
        "{label}: expected clean type-check; got: {:#?}",
        output.errors
    );
}

#[test]
fn iterator_trait_surface_parses_and_typechecks() {
    let output = typecheck_isolated(ITER_TRAIT_PRELUDE);
    assert_no_errors("Iterator + IntoIterator prelude", &output);
}

#[test]
fn iterator_trait_locks_next_signature_shape() {
    // Pin the AST shape so a future drive-by edit to `std/builtins.hew` that
    // (for example) flips `self` to `&mut self` or drops the trailing
    // `Option<Self::Item>` return type fails CI loudly. Q001 locks the by-move
    // shape.
    let parsed = hew_parser::parse(ITER_TRAIT_PRELUDE);
    assert!(
        parsed.errors.is_empty(),
        "prelude must parse: {:#?}",
        parsed.errors
    );

    let iter = parsed
        .program
        .items
        .iter()
        .find_map(|(item, _)| match item {
            Item::Trait(td) if td.name == "Iterator" => Some(td),
            _ => None,
        })
        .expect("Iterator trait must be parsed");

    let mut assoc_items = 0;
    let mut methods = 0;
    for ti in &iter.items {
        match ti {
            TraitItem::AssociatedType {
                name,
                bounds,
                default,
                ..
            } => {
                assoc_items += 1;
                assert_eq!(name, "Item", "Iterator's only assoc type must be `Item`");
                assert!(bounds.is_empty(), "Iterator::Item carries no bound in v0.5");
                assert!(
                    default.is_none(),
                    "Iterator::Item carries no default in v0.5"
                );
            }
            TraitItem::Method(m) => {
                methods += 1;
                assert_eq!(m.name, "next", "Iterator's only method must be `next`");
                assert!(
                    m.body.is_none(),
                    "trait `next` is abstract in v0.5; impls provide the body"
                );
                // First param is the `self` receiver. Q001 locks it as by-move
                // `self`, not `&mut self`. The parser models `self` as a Param
                // named "self" with `is_mutable: false` for the by-move form.
                let recv = m.params.first().expect("next must have a receiver");
                assert_eq!(recv.name, "self", "first param of next must be `self`");
                assert!(
                    !recv.is_mutable,
                    "Q001 locks `next` receiver as by-move `self`, not `&mut self`"
                );
                assert!(m.return_type.is_some(), "next must declare a return type");
            }
        }
    }
    assert_eq!(assoc_items, 1);
    assert_eq!(methods, 1);
}

#[test]
fn intoiterator_trait_locks_assoc_type_bound() {
    let parsed = hew_parser::parse(ITER_TRAIT_PRELUDE);
    assert!(
        parsed.errors.is_empty(),
        "prelude must parse: {:#?}",
        parsed.errors
    );

    let ii = parsed
        .program
        .items
        .iter()
        .find_map(|(item, _)| match item {
            Item::Trait(td) if td.name == "IntoIterator" => Some(td),
            _ => None,
        })
        .expect("IntoIterator trait must be parsed");

    // Locate the `IntoIter` assoc type and confirm it carries the
    // `Iterator<Item = ...>` bound. The whole point of this stage is that
    // callers can rely on this projection equality.
    let into_iter_bounds = ii
        .items
        .iter()
        .find_map(|ti| match ti {
            TraitItem::AssociatedType { name, bounds, .. } if name == "IntoIter" => Some(bounds),
            _ => None,
        })
        .expect("IntoIterator must declare assoc type `IntoIter`");
    assert!(
        into_iter_bounds
            .iter()
            .any(|tb| tb.name == "Iterator"
                && tb.assoc_type_bindings.iter().any(|b| b.name == "Item")),
        "IntoIter must be bounded by `Iterator<Item = ...>`; got: {into_iter_bounds:#?}"
    );

    // And confirm `Item` exists with no bound — it is fixed by the IntoIter
    // bound's `Item = Self::Item` projection.
    let item_assoc = ii.items.iter().any(|ti| {
        matches!(
            ti,
            TraitItem::AssociatedType { name, bounds, .. }
                if name == "Item" && bounds.is_empty()
        )
    });
    assert!(
        item_assoc,
        "IntoIterator must declare bare assoc type `Item`"
    );
}

#[test]
fn impl_iterator_for_fake_counter_typechecks() {
    // User-defined record + `impl Iterator` mirroring the locked shape.
    // Establishes that downstream users can implement the trait without any
    // compiler-magic special-case.
    let src = format!(
        "{ITER_TRAIT_PRELUDE}

pub type FakeCounter {{
    n: i64;
    limit: i64;
}}

impl Iterator for FakeCounter {{
    type Item = i64;

    fn next(it: FakeCounter) -> Option<i64> {{
        if it.n >= it.limit {{
            None
        }} else {{
            Some(it.n)
        }}
    }}
}}
"
    );
    let output = typecheck_isolated(&src);
    assert_no_errors("impl Iterator for FakeCounter", &output);
}

#[test]
fn impl_iterator_missing_associated_type_errors_loudly() {
    // The locked Iterator trait declares `type Item;` — an impl that drops the
    // `type Item = ...` alias must be rejected with a clear diagnostic, not
    // silently accepted. The negative case is anchored on the assoc-type
    // surface (which the checker DOES verify); full method-signature-vs-trait
    // shape matching is tracked separately (see Q### in handoff) and out of
    // scope for Stage 1.
    let src = format!(
        "{ITER_TRAIT_PRELUDE}

pub type BadCounter {{
    n: i64;
}}

impl Iterator for BadCounter {{
    // BUG: missing `type Item = i64;`. Must surface as a checker error.
    fn next(it: BadCounter) -> Option<i64> {{
        None
    }}
}}
"
    );
    let output = typecheck_isolated(&src);
    assert!(
        !output.errors.is_empty(),
        "impl Iterator missing `type Item = ...` must produce a checker error; got no errors"
    );
    let mentions_item = output.errors.iter().any(|e| e.message.contains("Item"));
    assert!(
        mentions_item,
        "diagnostic must cite the missing `Item` assoc type; got: {:#?}",
        output.errors
    );
}

#[test]
fn intoiterator_bound_resolves_in_generic_function() {
    // Synthetic `fn drive<I: IntoIterator>(x: I)` confirms trait-bound
    // resolution accepts IntoIterator as a registered trait (it is not
    // hard-coded anywhere — the registry picks it up from the stdlib decl).
    let src = format!(
        "{ITER_TRAIT_PRELUDE}

pub fn drive<I: IntoIterator>(_x: I) {{}}
"
    );
    let output = typecheck_isolated(&src);
    assert_no_errors("fn drive<I: IntoIterator>", &output);
}

#[test]
fn stdlib_builtins_carries_iterator_and_intoiterator_decls() {
    // Smoke against the live `std/builtins.hew`: confirm both trait
    // declarations are present in the parsed program. Pre-existing reds on
    // builtins.hew (display dup, RemotePid mismatch, etc.) are tolerated — we
    // only own the surface this stage adds.
    let mut path = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.pop();
    path.push("std/builtins.hew");
    let src = std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("read std/builtins.hew at {path:?}: {e}"));
    let parsed = hew_parser::parse(&src);
    assert!(
        parsed.errors.is_empty(),
        "std/builtins.hew must parse cleanly; got: {:#?}",
        parsed.errors
    );

    let trait_names: Vec<&str> = parsed
        .program
        .items
        .iter()
        .filter_map(|(item, _)| match item {
            Item::Trait(td) => Some(td.name.as_str()),
            _ => None,
        })
        .collect();
    assert!(
        trait_names.contains(&"Iterator"),
        "Iterator must be declared in std/builtins.hew; saw traits: {trait_names:?}"
    );
    assert!(
        trait_names.contains(&"IntoIterator"),
        "IntoIterator must be declared in std/builtins.hew; saw traits: {trait_names:?}"
    );

    // Run the checker end-to-end so registration paths execute. Pre-existing
    // reds are tolerated — this test asserts trait surface, not stdlib cleanness.
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let _ = checker.check_program(&parsed.program);
}
