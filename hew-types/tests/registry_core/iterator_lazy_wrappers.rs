//! Stage 2 of the iteration-foundation lane: lock the surface of the lazy
//! iterator wrappers (`Map`, `Filter`, `Take`, `Skip`) and the terminal
//! helpers (`fold`, `count`, `collect`).
//!
//! Each wrapper is a `pub type` record-shaped declaration that implements
//! `Iterator` per the Q001-locked `fn next(self) -> Option<Self::Item>`
//! shape. These tests pin:
//!
//! 1. The wrapper type's name + type-parameter list + field names.
//! 2. The matching `impl Iterator` block's `type Item = ...` projection.
//! 3. The free constructor functions returning the right wrapper type.
//! 4. A chained-adapter type-check exercise
//!    (`take(filter(map(...), p), 2)`).
//! 5. The terminal helper signatures (`fold`/`count`/`collect`).
//!
//! Pinning the surface inline keeps these tests hermetic to drift elsewhere
//! in `std/iter.hew`; the `stdlib_iter_*` test below smokes the live file
//! to confirm it actually carries the wrapper declarations.

#![allow(
    clippy::missing_panics_doc,
    reason = "integration-test panics on assertion failure are intentional"
)]

use crate::common;

use common::typecheck_isolated;
use hew_parser::ast::{ImplDecl, Item, Program, TraitItem, TypeBodyItem, TypeDecl, TypeExpr};

/// The Stage-1 trait surface, inlined to keep these tests independent of
/// `std/builtins.hew` churn.
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

// Mirrors the real `panic` builtin (std/builtins.hew). The Stage-2
// surface uses it to fail loud on Q004-deferred bodies; we inline the
// minimum signature so the isolated checker sees the same shape the
// live module relies on.
pub fn panic(message: string) {}
";

/// The Stage-2 surface as it should appear in `std/iter.hew`. Bodies that
/// trip the Q004 checker substitution rule fail loud via `panic(...)` (the
/// deferred-work invariant: deferrals must not look like working code).
/// The wrapper shapes and `Item` projections are what this stage locks.
const ITER_WRAPPER_SURFACE: &str = r#"
pub type Map<I, A, B> {
    iter: I;
    f: fn(A) -> B;
}

impl<I, A, B> Iterator for Map<I, A, B>
where
    I: Iterator<Item = A>,
{
    type Item = B;
    fn next(it: Map<I, A, B>) -> Option<B> {
        panic("Map::next deferred pending Q004");
        None
    }
}

pub type Filter<I, A> {
    iter: I;
    pred: fn(A) -> bool;
}

impl<I, A> Iterator for Filter<I, A>
where
    I: Iterator<Item = A>,
{
    type Item = A;
    fn next(it: Filter<I, A>) -> Option<A> {
        panic("Filter::next deferred (by-move self cannot loop)");
        None
    }
}

pub type Take<I> {
    iter: I;
    remaining: i64;
}

impl<I, A> Iterator for Take<I>
where
    I: Iterator<Item = A>,
{
    type Item = A;
    fn next(it: Take<I>) -> Option<A> {
        panic("Take::next deferred (by-move self cannot persist remaining)");
        None
    }
}

pub type Skip<I> {
    iter: I;
    remaining: i64;
}

impl<I, A> Iterator for Skip<I>
where
    I: Iterator<Item = A>,
{
    type Item = A;
    fn next(it: Skip<I>) -> Option<A> {
        panic("Skip::next deferred (by-move self cannot loop)");
        None
    }
}

pub fn map<I, A, B>(it: I, f: fn(A) -> B) -> Map<I, A, B>
where
    I: Iterator<Item = A>,
{
    Map { iter: it, f: f }
}

pub fn filter<I, A>(it: I, pred: fn(A) -> bool) -> Filter<I, A>
where
    I: Iterator<Item = A>,
{
    Filter { iter: it, pred: pred }
}

pub fn take<I, A>(it: I, n: i64) -> Take<I>
where
    I: Iterator<Item = A>,
{
    Take { iter: it, remaining: n }
}

pub fn skip<I, A>(it: I, n: i64) -> Skip<I>
where
    I: Iterator<Item = A>,
{
    Skip { iter: it, remaining: n }
}

pub fn fold<I, A, B>(it: I, init: B, f: fn(B, A) -> B) -> B
where
    I: Iterator<Item = A>,
{
    panic("iter::fold deferred pending Q004");
    init
}

pub fn count<I, A>(it: I) -> i64
where
    I: Iterator<Item = A>,
{
    panic("iter::count deferred pending Q004");
    0
}

pub fn collect<I, A>(it: I) -> Vec<A>
where
    I: Iterator<Item = A>,
{
    panic("iter::collect deferred pending Q004");
    Vec::new()
}
"#;

fn assert_no_errors(label: &str, output: &hew_types::TypeCheckOutput) {
    assert!(
        output.errors.is_empty(),
        "{label}: expected clean type-check; got: {:#?}",
        output.errors
    );
}

fn find_type_decl<'a>(program: &'a Program, name: &str) -> &'a TypeDecl {
    program
        .items
        .iter()
        .find_map(|(item, _)| match item {
            Item::TypeDecl(td) if td.name == name => Some(td),
            _ => None,
        })
        .unwrap_or_else(|| panic!("type decl `{name}` must be parsed"))
}

fn find_impl_iterator_for<'a>(program: &'a Program, self_ty_name: &str) -> &'a ImplDecl {
    program
        .items
        .iter()
        .find_map(|(item, _)| match item {
            Item::Impl(imp) => {
                let is_iter_trait = imp
                    .trait_bound
                    .as_ref()
                    .is_some_and(|tb| tb.name == "Iterator");
                let self_matches = match &imp.target_type.0 {
                    TypeExpr::Named { name, .. } => name == self_ty_name,
                    _ => false,
                };
                if is_iter_trait && self_matches {
                    Some(imp)
                } else {
                    None
                }
            }
            _ => None,
        })
        .unwrap_or_else(|| panic!("impl Iterator for {self_ty_name} must be parsed"))
}

fn type_param_names(td: &TypeDecl) -> Vec<&str> {
    td.type_params
        .as_ref()
        .map(|ps| ps.iter().map(|p| p.name.as_str()).collect())
        .unwrap_or_default()
}

fn impl_type_param_names(imp: &ImplDecl) -> Vec<&str> {
    imp.type_params
        .as_ref()
        .map(|ps| ps.iter().map(|p| p.name.as_str()).collect())
        .unwrap_or_default()
}

fn field_names(td: &TypeDecl) -> Vec<&str> {
    td.body
        .iter()
        .filter_map(|item| match item {
            TypeBodyItem::Field { name, .. } => Some(name.as_str()),
            _ => None,
        })
        .collect()
}

fn impl_item_assoc_named(imp: &ImplDecl, name: &str) -> Option<String> {
    imp.type_aliases.iter().find_map(|al| {
        if al.name == name {
            match &al.ty.0 {
                TypeExpr::Named { name, .. } => Some(name.clone()),
                _ => None,
            }
        } else {
            None
        }
    })
}

#[test]
fn wrapper_surface_parses_and_typechecks() {
    let src = format!("{ITER_TRAIT_PRELUDE}{ITER_WRAPPER_SURFACE}");
    let parsed = hew_parser::parse(&src);
    assert!(
        parsed.errors.is_empty(),
        "wrapper surface must parse: {:#?}",
        parsed.errors
    );
    let output = typecheck_isolated(&src);
    assert_no_errors("Stage-2 wrapper surface", &output);
}

#[test]
fn map_record_fields_and_item_projection() {
    let src = format!("{ITER_TRAIT_PRELUDE}{ITER_WRAPPER_SURFACE}");
    let parsed = hew_parser::parse(&src);
    assert!(parsed.errors.is_empty(), "{:#?}", parsed.errors);

    let map_td = find_type_decl(&parsed.program, "Map");
    assert_eq!(
        type_param_names(map_td),
        vec!["I", "A", "B"],
        "Map carries `<I, A, B>` so the closure's input/output projections are explicit"
    );
    assert_eq!(
        field_names(map_td),
        vec!["iter", "f"],
        "Map's fields are `iter` and `f`"
    );

    let imp = find_impl_iterator_for(&parsed.program, "Map");
    let item = impl_item_assoc_named(imp, "Item")
        .expect("impl Iterator for Map must declare `type Item = ...` as a `Named` projection");
    assert_eq!(item, "B", "Map's Item is the closure's output type `B`");
}

#[test]
fn filter_record_fields_and_item_projection() {
    let src = format!("{ITER_TRAIT_PRELUDE}{ITER_WRAPPER_SURFACE}");
    let parsed = hew_parser::parse(&src);
    assert!(parsed.errors.is_empty(), "{:#?}", parsed.errors);

    let td = find_type_decl(&parsed.program, "Filter");
    assert_eq!(type_param_names(td), vec!["I", "A"]);
    assert_eq!(field_names(td), vec!["iter", "pred"]);

    let imp = find_impl_iterator_for(&parsed.program, "Filter");
    let item = impl_item_assoc_named(imp, "Item").expect("Filter::Item assoc");
    assert_eq!(item, "A", "Filter's Item is `A` (subset of the inner)");
}

#[test]
fn take_item_is_inner_item_projection() {
    let src = format!("{ITER_TRAIT_PRELUDE}{ITER_WRAPPER_SURFACE}");
    let parsed = hew_parser::parse(&src);
    assert!(parsed.errors.is_empty(), "{:#?}", parsed.errors);

    let td = find_type_decl(&parsed.program, "Take");
    assert_eq!(type_param_names(td), vec!["I"]);
    assert_eq!(field_names(td), vec!["iter", "remaining"]);

    let imp = find_impl_iterator_for(&parsed.program, "Take");
    let item = impl_item_assoc_named(imp, "Item").expect("Take::Item assoc");
    assert_eq!(item, "A");

    // The impl introduces `A` as the inner-iterator's Item projection — the
    // wrapper itself only carries `<I>`. This shape lets Take's Item agree
    // with the inner's Item without burdening the type with an extra
    // parameter.
    assert!(
        impl_type_param_names(imp).contains(&"A"),
        "impl Iterator for Take must introduce `A` for the inner Item projection: {:?}",
        impl_type_param_names(imp),
    );
}

#[test]
fn skip_item_is_inner_item_projection() {
    let src = format!("{ITER_TRAIT_PRELUDE}{ITER_WRAPPER_SURFACE}");
    let parsed = hew_parser::parse(&src);
    assert!(parsed.errors.is_empty(), "{:#?}", parsed.errors);

    let td = find_type_decl(&parsed.program, "Skip");
    assert_eq!(type_param_names(td), vec!["I"]);
    assert_eq!(field_names(td), vec!["iter", "remaining"]);

    let imp = find_impl_iterator_for(&parsed.program, "Skip");
    let item = impl_item_assoc_named(imp, "Item").expect("Skip::Item assoc");
    assert_eq!(item, "A");
    assert!(
        impl_type_param_names(imp).contains(&"A"),
        "impl Iterator for Skip must introduce `A` for the inner Item projection",
    );
}

#[test]
fn iterator_trait_surface_carries_locked_next_shape() {
    // Sanity-check: the prelude actually contains the Q001-locked trait so
    // the wrapper impls above are being checked against the right surface.
    let src = format!("{ITER_TRAIT_PRELUDE}{ITER_WRAPPER_SURFACE}");
    let parsed = hew_parser::parse(&src);
    let iter_trait = parsed
        .program
        .items
        .iter()
        .find_map(|(item, _)| match item {
            Item::Trait(td) if td.name == "Iterator" => Some(td),
            _ => None,
        })
        .expect("Iterator trait in prelude");
    let next = iter_trait
        .items
        .iter()
        .find_map(|ti| match ti {
            TraitItem::Method(m) if m.name == "next" => Some(m),
            _ => None,
        })
        .expect("Iterator::next must exist");
    let recv = next.params.first().expect("next has a receiver");
    assert_eq!(recv.name, "self");
    assert!(!recv.is_mutable, "Q001 locks `next` as by-move `self`");
}

#[test]
fn chained_adapters_typecheck() {
    // The Stage-2 gate exercise: `take(filter(map(...), p), n)` followed by
    // `skip(_, k)` type-checks end-to-end. Drives the chain through a
    // user-defined `Iterator` impl so no compiler-magic special-case is
    // involved.
    let driver = r"

pub type Counter {
    n: i64;
    limit: i64;
}

impl Iterator for Counter {
    type Item = i64;
    fn next(it: Counter) -> Option<i64> {
        if it.n >= it.limit {
            None
        } else {
            Some(it.n)
        }
    }
}

pub fn drive() {
    let c = Counter { n: 0, limit: 10 };
    let doubled: Map<Counter, i64, i64> = map(c, |x: i64| x * 2);
    let evens: Filter<Map<Counter, i64, i64>, i64> = filter(doubled, |y: i64| y % 4 == 0);
    let two: Take<Filter<Map<Counter, i64, i64>, i64>> = take(evens, 2);
    let _three: Skip<Take<Filter<Map<Counter, i64, i64>, i64>>> = skip(two, 1);
}
";
    let src = format!("{ITER_TRAIT_PRELUDE}{ITER_WRAPPER_SURFACE}{driver}");
    let output = typecheck_isolated(&src);
    assert_no_errors("chained map.filter.take.skip", &output);
}

#[test]
fn terminal_helpers_typecheck() {
    let driver = r"

pub type Counter {
    n: i64;
    limit: i64;
}

impl Iterator for Counter {
    type Item = i64;
    fn next(it: Counter) -> Option<i64> {
        Some(it.n)
    }
}

pub fn drive() {
    let c = Counter { n: 0, limit: 3 };
    let _total: i64 = fold(c, 0, |acc: i64, x: i64| acc + x);
    let d = Counter { n: 0, limit: 3 };
    let _n: i64 = count(d);
    let e = Counter { n: 0, limit: 3 };
    let _all: Vec<i64> = collect(e);
}
";
    let src = format!("{ITER_TRAIT_PRELUDE}{ITER_WRAPPER_SURFACE}{driver}");
    let output = typecheck_isolated(&src);
    assert_no_errors("fold/count/collect on Counter", &output);
}

#[test]
fn stdlib_iter_module_carries_wrapper_decls() {
    // Smoke against the live `std/iter.hew`: the four wrapper types and the
    // three terminal helpers are present. The transitional eager
    // combinators are tolerated — we only assert on the surface this stage
    // adds.
    let mut path = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.pop();
    path.push("std/iter.hew");
    let src = std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("read std/iter.hew at {path:?}: {e}"));
    let parsed = hew_parser::parse(&src);
    assert!(
        parsed.errors.is_empty(),
        "std/iter.hew must parse cleanly; got: {:#?}",
        parsed.errors
    );

    let type_names: Vec<&str> = parsed
        .program
        .items
        .iter()
        .filter_map(|(item, _)| match item {
            Item::TypeDecl(td) => Some(td.name.as_str()),
            _ => None,
        })
        .collect();
    for needed in ["Map", "Filter", "Take", "Skip"] {
        assert!(
            type_names.contains(&needed),
            "std/iter.hew must declare `{needed}`; saw: {type_names:?}"
        );
    }

    let fn_names: Vec<&str> = parsed
        .program
        .items
        .iter()
        .filter_map(|(item, _)| match item {
            Item::Function(f) => Some(f.name.as_str()),
            _ => None,
        })
        .collect();
    for needed in ["map", "filter", "take", "skip", "fold", "count", "collect"] {
        assert!(
            fn_names.contains(&needed),
            "std/iter.hew must declare free function `{needed}`; saw: {fn_names:?}"
        );
    }
}
