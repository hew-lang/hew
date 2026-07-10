use std::collections::HashSet;

use hew_hir::{mangle_dotted_name, mangle_resolved_ty};
use hew_types::{ResolvedTraitBound, ResolvedTy};

fn named(name: &str, args: Vec<ResolvedTy>) -> ResolvedTy {
    ResolvedTy::named_user(name, args)
}

#[test]
fn nested_type_fragments_distinguish_former_collision_pairs() {
    let foo_i64 = named("Foo", vec![ResolvedTy::I64]);
    let literal_foo_i64 = named("Foo_i64", vec![]);
    assert_eq!(mangle_resolved_ty(&foo_i64), "Foo$li64$g");
    assert_eq!(mangle_resolved_ty(&literal_foo_i64), "Foo_i64");
    assert_ne!(
        mangle_resolved_ty(&foo_i64),
        mangle_resolved_ty(&literal_foo_i64)
    );

    let pair = named("Pair", vec![named("A", vec![]), named("B", vec![])]);
    let nested = named("Foo", vec![named("A", vec![named("B", vec![])])]);
    assert_ne!(mangle_resolved_ty(&pair), mangle_resolved_ty(&nested));

    let option_foo_i64 = named("Option", vec![foo_i64]);
    let option_literal_foo_i64 = named("Option", vec![literal_foo_i64]);
    assert_ne!(
        mangle_resolved_ty(&option_foo_i64),
        mangle_resolved_ty(&option_literal_foo_i64)
    );

    let tuple = ResolvedTy::Tuple(vec![named("a", vec![]), named("b", vec![])]);
    let literal_tuple = named("tuple_a_b", vec![]);
    assert_ne!(
        mangle_resolved_ty(&tuple),
        mangle_resolved_ty(&literal_tuple)
    );

    assert_ne!(
        mangle_resolved_ty(&named("a::b", vec![])),
        mangle_resolved_ty(&named("a_b", vec![]))
    );

    let iterator_i64 = ResolvedTy::TraitObject {
        traits: vec![ResolvedTraitBound {
            trait_name: "Iterator".to_string(),
            args: vec![],
            assoc_bindings: vec![("Item".to_string(), ResolvedTy::I64)],
        }],
    };
    let iterator_string = ResolvedTy::TraitObject {
        traits: vec![ResolvedTraitBound {
            trait_name: "Iterator".to_string(),
            args: vec![],
            assoc_bindings: vec![("Item".to_string(), ResolvedTy::String)],
        }],
    };
    assert_ne!(
        mangle_resolved_ty(&iterator_i64),
        mangle_resolved_ty(&iterator_string)
    );
}

#[test]
fn type_fragments_are_injective_and_safe_through_depth_three() {
    let types = type_family(3);
    let mut fragments = HashSet::with_capacity(types.len());

    for ty in types {
        let fragment = mangle_resolved_ty(&ty);
        assert!(
            fragments.insert(fragment.clone()),
            "fragment collision for {ty:?}: {fragment}"
        );
        assert!(!fragment.contains("$$"), "fragment has $$: {fragment}");
        assert!(
            !fragment.starts_with('$') && !fragment.ends_with('$'),
            "fragment has unsafe boundary: {fragment}"
        );
        assert_eq!(
            mangle_dotted_name(&fragment),
            fragment,
            "fragment contains a literal dot"
        );
    }
}

fn type_family(depth: usize) -> Vec<ResolvedTy> {
    let mut types = vec![
        ResolvedTy::I64,
        ResolvedTy::Bool,
        ResolvedTy::String,
        named("Leaf", vec![]),
        ResolvedTy::TypeParam {
            name: "T".to_string(),
        },
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
