//! Cross-crate parity pin: `hew_types::resolved_ty::mangle_resolved_ty_segment`
//! MUST render byte-identical output to
//! `hew_hir::monomorph::mangle_resolved_ty` for every concrete `ResolvedTy`.
//!
//! The two encoders can't share code — `hew-types` cannot depend on
//! `hew-hir` (that would be circular) — so their doc comments are the only
//! thing declaring the sync invariant. This test is what actually enforces
//! it: a change to one scheme without a matching change to the other fails
//! here.
//!
//! Regression this guards: the `$`-token mangling scheme
//! (`hew-hir/src/monomorph.rs::mangle_resolved_ty`) updated the HIR-side
//! encoder but left `mangle_resolved_ty_segment` on the old `_`-join scheme.
//! Leaf args (`Wrapper<i64>`) and literal names happened to agree under both
//! schemes, so the desync was invisible until a COMPOUND-NESTED type arg
//! (`Wrapper<Boxx<i64>>`) diverged — see
//! `tests/hew/generic_mangling_compound_nested_impl_test.hew` for the
//! compiled-and-run reproduction of the resulting dispatch failure.

use std::collections::HashSet;

use hew_hir::mangle_resolved_ty;
use hew_types::resolved_ty::mangle_resolved_ty_segment;
use hew_types::{ResolvedTraitBound, ResolvedTy};

fn named(name: &str, args: Vec<ResolvedTy>) -> ResolvedTy {
    ResolvedTy::named_user(name, args)
}

/// Assert the two encoders agree for a concrete (`TypeParam`-free) type.
fn assert_parity(ty: &ResolvedTy) {
    let hir_mangled = mangle_resolved_ty(ty);
    let types_mangled = mangle_resolved_ty_segment(ty);
    assert_eq!(
        types_mangled.as_deref(),
        Some(hir_mangled.as_str()),
        "mangle_resolved_ty_segment desynced from mangle_resolved_ty for {ty:?}: \
         hew-types produced {types_mangled:?}, hew-hir produced {hir_mangled:?}"
    );
}

#[test]
fn leaf_scalar_matches() {
    assert_parity(&ResolvedTy::I64);
    assert_parity(&ResolvedTy::String);
    assert_parity(&ResolvedTy::Bool);
}

#[test]
fn single_generic_named_matches() {
    // Foo<i64>
    assert_parity(&named("Foo", vec![ResolvedTy::I64]));
}

#[test]
fn literal_named_matches() {
    // Foo_i64 (a type literally named this way, distinct from Foo<i64>)
    assert_parity(&named("Foo_i64", vec![]));
}

#[test]
fn compound_nested_named_matches() {
    // impl Describe for Wrapper<Boxx<i64>> / Wrapper<Boxx<string>> — the
    // exact shape that regressed (BLOCK verdict, issue #2485).
    let boxx_i64 = named("Boxx", vec![ResolvedTy::I64]);
    assert_parity(&named("Wrapper", vec![boxx_i64]));

    let boxx_string = named("Boxx", vec![ResolvedTy::String]);
    assert_parity(&named("Wrapper", vec![boxx_string]));
}

#[test]
fn named_with_tuple_arg_matches() {
    // Vec<(string, i64)>
    let tuple = ResolvedTy::Tuple(vec![ResolvedTy::String, ResolvedTy::I64]);
    assert_parity(&named("Vec", vec![tuple]));
}

#[test]
fn qualified_name_separator_matches() {
    assert_parity(&named("a::b", vec![]));
    assert_parity(&named("a.b", vec![]));
}

#[test]
fn family_of_concrete_nested_types_stays_in_lockstep() {
    for ty in concrete_type_family(2) {
        assert_parity(&ty);
    }
}

/// Same shape as `mangle_resolved_ty_test.rs::type_family`, but seeded
/// without `TypeParam`: `mangle_resolved_ty_segment` intentionally diverges
/// (returns `None`) for abstract type parameters — see its doc comment — so
/// `TypeParam`-bearing shapes are out of scope for this parity pin, which
/// covers only the concrete-type contract the two encoders must share.
fn concrete_type_family(depth: usize) -> Vec<ResolvedTy> {
    let mut types = vec![
        ResolvedTy::I64,
        ResolvedTy::Bool,
        ResolvedTy::String,
        named("Leaf", vec![]),
    ];

    for _ in 0..depth {
        let previous = types.clone();
        for ty in previous {
            types.extend([
                named("Box", vec![ty.clone()]),
                named("Pair", vec![ty.clone(), ResolvedTy::Bool]),
                ResolvedTy::Tuple(vec![ty.clone(), ResolvedTy::String]),
                ResolvedTy::Array(Box::new(ty.clone()), 3),
                ResolvedTy::Slice(Box::new(ty.clone())),
                ResolvedTy::Function {
                    params: vec![ty.clone(), ResolvedTy::Bool],
                    ret: Box::new(ResolvedTy::String),
                },
                ResolvedTy::Closure {
                    params: vec![ty.clone()],
                    ret: Box::new(ResolvedTy::Bool),
                    captures: vec![],
                },
                ResolvedTy::Pointer {
                    is_mutable: false,
                    pointee: Box::new(ty.clone()),
                },
                ResolvedTy::Pointer {
                    is_mutable: true,
                    pointee: Box::new(ty.clone()),
                },
                ResolvedTy::Borrow {
                    pointee: Box::new(ty.clone()),
                },
                ResolvedTy::TraitObject {
                    traits: vec![ResolvedTraitBound {
                        trait_name: "Iterator".to_string(),
                        args: vec![ty.clone()],
                        assoc_bindings: vec![("Item".to_string(), ty.clone())],
                    }],
                },
                ResolvedTy::Task(Box::new(ty)),
            ]);
        }
    }

    let mut unique = HashSet::with_capacity(types.len());
    types.retain(|ty| unique.insert(ty.clone()));
    types
}
