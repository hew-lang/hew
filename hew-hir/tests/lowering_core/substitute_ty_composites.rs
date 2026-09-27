//! Generic substitution reaches borrow pointees and trait-object arguments.

use std::collections::HashMap;

use hew_hir::lower::substitute_ty;
use hew_types::{ResolvedTraitBound, ResolvedTy};

fn concrete_subst(name: &str) -> HashMap<hew_types::ParamHead, ResolvedTy> {
    HashMap::from([(hew_types::ParamHead::for_test(name), ResolvedTy::I64)])
}

fn parameter(name: &str) -> ResolvedTy {
    ResolvedTy::param(hew_types::ParamHead::for_test(name))
}

#[test]
fn substitute_descends_into_borrow_pointee() {
    let ty = ResolvedTy::Borrow {
        pointee: Box::new(parameter("T")),
    };
    let out = substitute_ty(&ty, &concrete_subst("T"));
    assert_eq!(
        out,
        ResolvedTy::Borrow {
            pointee: Box::new(ResolvedTy::I64)
        },
        "borrow pointee must be substituted to i64, got: {out:?}"
    );
}

/// A trait object carrying `T` in a trait argument must descend into the
/// argument and rewrite it to `i64`.
#[test]
fn substitute_descends_into_trait_object_args() {
    let ty = ResolvedTy::TraitObject {
        traits: vec![ResolvedTraitBound {
            trait_name: "Into".to_string(),
            trait_id: None,
            args: vec![parameter("T")],
            assoc_bindings: vec![],
        }],
    };
    let out = substitute_ty(&ty, &concrete_subst("T"));
    let ResolvedTy::TraitObject { traits } = out else {
        panic!("expected TraitObject, got: {out:?}");
    };
    assert_eq!(
        traits[0].args,
        vec![ResolvedTy::I64],
        "trait-object arg must be substituted to i64"
    );
}

/// A trait object carrying `T` in an associated-type binding must descend into
/// the binding payload and rewrite it.
#[test]
fn substitute_descends_into_trait_object_assoc_bindings() {
    let ty = ResolvedTy::TraitObject {
        traits: vec![ResolvedTraitBound {
            trait_name: "Iterator".to_string(),
            trait_id: None,
            args: vec![],
            assoc_bindings: vec![("Item".to_string(), parameter("T"))],
        }],
    };
    let out = substitute_ty(&ty, &concrete_subst("T"));
    let ResolvedTy::TraitObject { traits } = out else {
        panic!("expected TraitObject, got: {out:?}");
    };
    assert_eq!(
        traits[0].assoc_bindings,
        vec![("Item".to_string(), ResolvedTy::I64)],
        "assoc-type binding must be substituted to i64"
    );
}

/// A `TypeParam` buried under multiple composites including a borrow is fully
/// rewritten — the descent is recursive, not one-level.
#[test]
fn substitute_descends_through_nested_borrow_in_tuple() {
    let ty = ResolvedTy::Tuple(vec![ResolvedTy::Borrow {
        pointee: Box::new(parameter("T")),
    }]);
    let out = substitute_ty(&ty, &concrete_subst("T"));
    assert_eq!(
        out,
        ResolvedTy::Tuple(vec![ResolvedTy::Borrow {
            pointee: Box::new(ResolvedTy::I64)
        }]),
        "nested borrow pointee under a tuple must be substituted, got: {out:?}"
    );
}
