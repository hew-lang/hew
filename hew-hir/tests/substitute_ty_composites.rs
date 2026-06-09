//! W5.007a fix — `substitute_ty` must descend EVERY composite `ResolvedTy`
//! constructor so a nested `Named` type-parameter is rewritten to the single
//! authoritative `ResolvedTy::TypeParam` shape (A622 / DI-019/DI-020).
//!
//! The pre-fix code recursed tuple/array/slice/function/closure/pointer/task
//! and the top-level parameter, but fell through `_ => ty.clone()` for
//! `Borrow` and `TraitObject`, leaving a `TypeParam` nested under those in the
//! legacy `Named` shape. These tests pin the descent and FAIL on the pre-fix
//! tree.

use std::collections::HashMap;

use hew_hir::lower::substitute_ty;
use hew_types::{ResolvedTraitBound, ResolvedTy};

/// The abstract substitution a generic origin is lowered against: every
/// declared parameter maps to its structural `TypeParam`.
fn abstract_subst(name: &str) -> HashMap<String, ResolvedTy> {
    let mut m = HashMap::new();
    m.insert(
        name.to_string(),
        ResolvedTy::TypeParam {
            name: name.to_string(),
        },
    );
    m
}

fn named(name: &str) -> ResolvedTy {
    ResolvedTy::Named {
        name: name.to_string(),
        args: vec![],
        builtin: None,
    }
}

/// `&T` (a generic origin's `fn f<T>(x: &T) -> &T` parameter) must lower to
/// `Borrow { pointee: TypeParam }`, NOT `Borrow { pointee: Named }`.
#[test]
fn substitute_descends_into_borrow_pointee() {
    let ty = ResolvedTy::Borrow {
        pointee: Box::new(named("T")),
    };
    let out = substitute_ty(&ty, &abstract_subst("T"));
    assert_eq!(
        out,
        ResolvedTy::Borrow {
            pointee: Box::new(ResolvedTy::TypeParam {
                name: "T".to_string()
            })
        },
        "borrow pointee must be substituted to the abstract TypeParam, got: {out:?}"
    );
}

/// A trait object carrying `T` in a trait argument must descend into the
/// argument and rewrite it to the abstract `TypeParam`.
#[test]
fn substitute_descends_into_trait_object_args() {
    let ty = ResolvedTy::TraitObject {
        traits: vec![ResolvedTraitBound {
            trait_name: "Into".to_string(),
            args: vec![named("T")],
            assoc_bindings: vec![],
        }],
    };
    let out = substitute_ty(&ty, &abstract_subst("T"));
    let ResolvedTy::TraitObject { traits } = out else {
        panic!("expected TraitObject, got: {out:?}");
    };
    assert_eq!(
        traits[0].args,
        vec![ResolvedTy::TypeParam {
            name: "T".to_string()
        }],
        "trait-object arg must be substituted to the abstract TypeParam"
    );
}

/// A trait object carrying `T` in an associated-type binding must descend into
/// the binding payload and rewrite it.
#[test]
fn substitute_descends_into_trait_object_assoc_bindings() {
    let ty = ResolvedTy::TraitObject {
        traits: vec![ResolvedTraitBound {
            trait_name: "Iterator".to_string(),
            args: vec![],
            assoc_bindings: vec![("Item".to_string(), named("T"))],
        }],
    };
    let out = substitute_ty(&ty, &abstract_subst("T"));
    let ResolvedTy::TraitObject { traits } = out else {
        panic!("expected TraitObject, got: {out:?}");
    };
    assert_eq!(
        traits[0].assoc_bindings,
        vec![(
            "Item".to_string(),
            ResolvedTy::TypeParam {
                name: "T".to_string()
            }
        )],
        "assoc-type binding must be substituted to the abstract TypeParam"
    );
}

/// A `TypeParam` buried under multiple composites including a borrow is fully
/// rewritten — the descent is recursive, not one-level.
#[test]
fn substitute_descends_through_nested_borrow_in_tuple() {
    let ty = ResolvedTy::Tuple(vec![ResolvedTy::Borrow {
        pointee: Box::new(named("T")),
    }]);
    let out = substitute_ty(&ty, &abstract_subst("T"));
    assert_eq!(
        out,
        ResolvedTy::Tuple(vec![ResolvedTy::Borrow {
            pointee: Box::new(ResolvedTy::TypeParam {
                name: "T".to_string()
            })
        }]),
        "nested borrow pointee under a tuple must be substituted, got: {out:?}"
    );
}
