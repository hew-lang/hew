use hew_types::{
    module_registry::ModuleRegistry, BuiltinType, Checker, CloneKind, ResolvedTy, TypeFactContext,
    TypeFactService,
};

fn facts(source: &str) -> TypeFactService {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let mut checker = Checker::new(ModuleRegistry::new(Vec::new()));
    let output = checker.check_program(&parsed.program);
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    TypeFactService::new(output.type_fact_context, output.type_facts)
}

#[test]
fn vector_cursor_uses_its_source_record_fields_and_recursive_copy() {
    let mut service = facts("fn main() -> i64 { 0 }");
    for element in [ResolvedTy::I64, ResolvedTy::String] {
        let cursor =
            ResolvedTy::named_builtin("VecIter", BuiltinType::VecIter, vec![element.clone()]);
        let (instance, fields) = service.record_fields(&cursor).unwrap();
        assert_eq!(instance.nominal.full_path(), "std.builtins.VecIter");
        assert_eq!(
            fields,
            vec![
                (
                    "vec".to_string(),
                    ResolvedTy::named_builtin("Vec", BuiltinType::Vec, vec![element])
                ),
                ("idx".to_string(), ResolvedTy::I64),
            ]
        );
        assert_eq!(
            service.require(&cursor).unwrap().clone,
            CloneKind::FieldWise
        );
    }
}

#[test]
fn same_spelling_user_record_does_not_select_the_builtin_cursor() {
    let service = facts("type VecIter<T> { tail: T, head: i64, } fn main() -> i64 { 0 }");
    let user = ResolvedTy::named_user("VecIter", vec![ResolvedTy::String]);
    let builtin =
        ResolvedTy::named_builtin("VecIter", BuiltinType::VecIter, vec![ResolvedTy::String]);
    let (user_instance, fields) = service.record_fields(&user).unwrap();
    let (builtin_instance, _) = service.record_fields(&builtin).unwrap();
    assert_ne!(user_instance, builtin_instance);
    assert_eq!(
        fields,
        vec![
            ("tail".into(), ResolvedTy::String),
            ("head".into(), ResolvedTy::I64)
        ]
    );
}

#[test]
fn record_contract_requires_a_declaration_and_exact_arity() {
    let cursor = ResolvedTy::named_builtin("VecIter", BuiltinType::VecIter, vec![ResolvedTy::I64]);
    let empty = TypeFactService::new(
        TypeFactContext::default(),
        std::collections::BTreeMap::default(),
    );
    assert!(empty.record_fields(&cursor).is_err());
    let service = facts("enum Item { Empty, } #[opaque] type Handle {} fn main() -> i64 { 0 }");
    for ty in [
        ResolvedTy::named_builtin("VecIter", BuiltinType::VecIter, vec![]),
        ResolvedTy::named_user("Missing", vec![]),
        ResolvedTy::named_user("Item", vec![]),
        ResolvedTy::named_opaque("Handle", vec![]),
    ] {
        assert!(service.record_fields(&ty).is_err(), "{ty:?}");
    }
}

#[test]
fn record_marker_belongs_to_the_container_declaration() {
    use hew_types::DeclarationMarker;
    let mut service = facts(
        "#[resource] type Handle { id: i64 } type Wrapper<T> { value: T } fn main() -> i64 { 0 }",
    );
    let handle = ResolvedTy::named_user("Handle", vec![]);
    let wrapper = ResolvedTy::named_user("Wrapper", vec![handle.clone()]);
    assert_eq!(service.require(&handle).unwrap().clone, CloneKind::None);
    assert_eq!(service.require(&wrapper).unwrap().clone, CloneKind::None);
    assert_eq!(
        service.declaration_marker(&handle).unwrap(),
        DeclarationMarker::Resource
    );
    assert_eq!(
        service.declaration_marker(&wrapper).unwrap(),
        DeclarationMarker::None
    );
    assert!(service
        .declaration_marker(&ResolvedTy::named_user("Wrapper", vec![]))
        .is_err());
    assert!(service
        .declaration_marker(&ResolvedTy::named_user("Missing", vec![]))
        .is_err());
}

#[test]
fn value_capabilities_preserve_selected_methods_and_derived_defaults() {
    use hew_types::{
        ValueCapability::{Eq, Hash},
        ValueMethodPlan::{Derived, User},
    };
    let mut service = facts(
        r"
        type Key { id: i64 }
        impl Hash for Key { fn hash(self) -> i64 { self.id % 10 } }
        impl Eq for Key { fn eq(self, other: Key) -> bool { self.id % 10 == other.id % 10 } }
        type Plain { id: i64 }
        type HashOnly { id: i64 }
        impl Hash for HashOnly { fn hash(self) -> i64 { 0 } }
        fn main() -> i64 { 0 }
    ",
    );
    let key = ResolvedTy::named_user("Key", vec![]);
    let mut methods = Vec::new();
    for capability in [Hash, Eq] {
        let selection: hew_types::ValueMethodSelection =
            service.capability_plan(&key, capability).unwrap().unwrap();
        assert_eq!(selection.ty(), &key);
        assert_eq!(selection.capability(), capability);
        let User { method, type_args } = selection.plan() else {
            panic!("user override must be selected");
        };
        assert!(type_args.is_empty());
        methods.push(method.clone());
        let plain = ResolvedTy::named_user("Plain", vec![]);
        let derived = service
            .capability_plan(&plain, capability)
            .unwrap()
            .unwrap();
        assert_eq!(derived.ty(), &plain);
        assert_eq!(derived.capability(), capability);
        assert_eq!(derived.plan(), &Derived);
    }
    assert_ne!(methods[0], methods[1]);
    assert!(service.require(&key).unwrap().hash);
    assert!(service.require(&key).unwrap().eq);
    let hash_only = ResolvedTy::named_user("HashOnly", vec![]);
    assert!(matches!(
        service
            .capability_plan(&hash_only, Hash)
            .unwrap()
            .map(|selection| selection.plan().clone()),
        Some(User { .. })
    ));
    assert_eq!(
        service
            .capability_plan(&hash_only, Eq)
            .unwrap()
            .map(|selection| selection.plan().clone()),
        Some(Derived)
    );
}

#[test]
fn value_capabilities_infer_impl_binders_and_concrete_specialization() {
    use hew_types::{ValueCapability::Hash, ValueMethodPlan::User};
    let mut service = facts(
        r"
        type Key<A, B> { first: A, second: B }
        impl<X, Y> Hash for Key<Y, X> { fn hash(self) -> i64 { 1 } }
        impl Hash for Key<i64, string> { fn hash(self) -> i64 { 2 } }
        fn main() -> i64 { 0 }
    ",
    );
    let generic = ResolvedTy::named_user("Key", vec![ResolvedTy::Bool, ResolvedTy::I64]);
    let exact = ResolvedTy::named_user("Key", vec![ResolvedTy::I64, ResolvedTy::String]);
    let Some(User {
        method: generic_method,
        type_args,
    }) = service
        .capability_plan(&generic, Hash)
        .unwrap()
        .map(|selection| {
            assert_eq!(selection.ty(), &generic);
            assert_eq!(selection.capability(), Hash);
            selection.plan().clone()
        })
    else {
        panic!("generic impl");
    };
    assert_eq!(type_args, vec![ResolvedTy::I64, ResolvedTy::Bool]);
    let Some(User {
        method: exact_method,
        type_args,
    }) = service
        .capability_plan(&exact, Hash)
        .unwrap()
        .map(|selection| {
            assert_eq!(selection.ty(), &exact);
            assert_eq!(selection.capability(), Hash);
            selection.plan().clone()
        })
    else {
        panic!("specialized impl");
    };
    assert!(type_args.is_empty());
    assert_ne!(generic_method, exact_method);
}

#[test]
fn value_capabilities_refuse_unsupported_and_abstract_receivers() {
    use hew_types::{
        ValueCapability::{Eq, Hash},
        ValueMethodPlan::Derived,
    };
    let mut service = facts("type Vec { id: i64 } enum Choice { Empty } fn main() -> i64 { 0 }");
    let user = ResolvedTy::named_user("Vec", vec![]);
    assert_eq!(
        service
            .capability_plan(&user, Hash)
            .unwrap()
            .map(|selection| selection.plan().clone()),
        Some(Derived)
    );
    for ty in [
        ResolvedTy::Tuple(vec![ResolvedTy::I64]),
        ResolvedTy::named_builtin("Vec", BuiltinType::Vec, vec![ResolvedTy::I64]),
        ResolvedTy::named_user("Choice", vec![]),
    ] {
        assert_eq!(service.capability_plan(&ty, Hash).unwrap(), None, "{ty:?}");
        assert!(!service.require(&ty).unwrap().hash);
    }
    for capability in [Hash, Eq] {
        assert!(service
            .capability_plan(&ResolvedTy::TypeParam { name: "T".into() }, capability)
            .is_err());
        assert!(service
            .capability_plan(&ResolvedTy::named_user("elsewhere.Vec", vec![]), capability)
            .is_err());
    }
}

#[test]
fn value_capabilities_keep_generic_selection_when_specialization_is_registered_first() {
    use hew_types::{ValueCapability::Hash, ValueMethodPlan::User};
    let mut service = facts(
        r"
        type Key<A> { value: A }
        impl Hash for Key<i64> { fn hash(self) -> i64 { 2 } }
        impl<T> Hash for Key<T> { fn hash(self) -> i64 { 1 } }
        fn main() -> i64 { 0 }
    ",
    );
    let Some(User {
        method: generic,
        type_args,
    }) = service
        .capability_plan(&ResolvedTy::named_user("Key", vec![ResolvedTy::Bool]), Hash)
        .unwrap()
        .map(|selection| selection.plan().clone())
    else {
        panic!("generic impl")
    };
    assert_eq!(type_args, vec![ResolvedTy::Bool]);
    let Some(User {
        method: exact,
        type_args,
    }) = service
        .capability_plan(&ResolvedTy::named_user("Key", vec![ResolvedTy::I64]), Hash)
        .unwrap()
        .map(|selection| selection.plan().clone())
    else {
        panic!("exact impl")
    };
    assert!(type_args.is_empty());
    assert_ne!(generic, exact);
}

#[test]
fn value_capabilities_refuse_unresolved_method_binders_independently() {
    use hew_types::{
        ClassError,
        ValueCapability::{Eq, Hash},
        ValueMethodPlan::Derived,
    };
    let mut service = facts(
        r"
        type Key { value: i64 }
        impl Hash for Key { fn hash<T>(self) -> i64 { 1 } }
        fn main() -> i64 { 0 }
    ",
    );
    let key = ResolvedTy::named_user("Key", vec![]);
    assert_eq!(
        service.capability_plan(&key, Hash),
        Err(ClassError::TypeParam { name: "T".into() })
    );
    assert_eq!(
        service
            .capability_plan(&key, Eq)
            .unwrap()
            .map(|selection| selection.plan().clone()),
        Some(Derived)
    );
    assert!(service.require(&key).is_err());
    let abstract_fn = ResolvedTy::Function {
        capabilities: hew_parser::ast::CallableCapabilities::default(),
        params: vec![ResolvedTy::TypeParam { name: "U".into() }],
        ret: Box::new(ResolvedTy::Unit),
    };
    assert_eq!(
        service.capability_plan(&abstract_fn, Eq),
        Err(ClassError::TypeParam { name: "U".into() })
    );
}

#[test]
fn value_capabilities_derive_substituted_records_without_authorizing_opaque_hashing() {
    use hew_types::{ValueCapability::Hash, ValueMethodPlan::Derived};
    let mut service = facts(
        r"
        type Key<T> { value: T }
        #[opaque] type Handle {}
        fn main() -> i64 { 0 }
    ",
    );
    let key = ResolvedTy::named_user("Key", vec![ResolvedTy::I64]);
    assert_eq!(
        service
            .capability_plan(&key, Hash)
            .unwrap()
            .map(|selection| selection.plan().clone()),
        Some(Derived)
    );
    assert!(service.require(&key).unwrap().hash);
    let opaque = ResolvedTy::named_opaque("Handle", vec![]);
    assert_eq!(service.capability_plan(&opaque, Hash).unwrap(), None);
    assert!(!service.require(&opaque).unwrap().hash);
}

#[test]
fn value_capabilities_keep_trait_identity_when_a_lookalike_method_is_registered_later() {
    use hew_types::{ValueCapability::Hash, ValueMethodPlan::User};
    let parsed = hew_parser::parse(
        r"
        type Key { id: i64 }
        impl Hash for Key { fn hash(self) -> i64 { 1 } }
        trait Other { fn hash(self) -> i64; }
        impl Other for Key { fn hash(self) -> i64 { 2 } }
        fn main() -> i64 { 0 }
    ",
    );
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let output = Checker::new(ModuleRegistry::new(vec![])).check_program(&parsed.program);
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    let selected = output
        .identity
        .declaration_by_path("Key::<impl Hash for Key>::hash")
        .unwrap()
        .clone();
    let lookalike = output
        .identity
        .declaration_by_path("Key::<impl Other for Key>::hash")
        .unwrap();
    assert_ne!(&selected, lookalike);
    let mut service = TypeFactService::new(output.type_fact_context, output.type_facts);
    assert_eq!(
        service
            .capability_plan(&ResolvedTy::named_user("Key", vec![]), Hash)
            .unwrap()
            .map(|selection| selection.plan().clone()),
        Some(User {
            method: selected,
            type_args: vec![]
        })
    );
}

#[test]
fn value_capabilities_substitute_nested_impl_patterns_and_refuse_a_nonmatching_receiver() {
    use hew_types::{ValueCapability::Hash, ValueMethodPlan::User};
    let mut service = facts(
        r"
        type Key<T> { value: T }
        impl<T> Hash for Key<Vec<T>> { fn hash(self) -> i64 { 1 } }
        fn main() -> i64 { 0 }
    ",
    );
    let nested = ResolvedTy::named_user(
        "Key",
        vec![ResolvedTy::named_builtin(
            "Vec",
            BuiltinType::Vec,
            vec![ResolvedTy::I64],
        )],
    );
    let Some(User { type_args, .. }) = service
        .capability_plan(&nested, Hash)
        .unwrap()
        .map(|selection| selection.plan().clone())
    else {
        panic!("nested binder")
    };
    assert_eq!(type_args, vec![ResolvedTy::I64]);
    assert!(service
        .capability_plan(&ResolvedTy::named_user("Key", vec![ResolvedTy::I64]), Hash)
        .is_err());
}

#[test]
fn derived_value_capabilities_compose_selected_member_methods_independently() {
    use hew_types::{
        ValueCapability::{Eq, Hash},
        ValueMethodPlan::{Derived, User},
    };
    let mut service = facts(
        r"
        type Leaf { callback: fn() -> i64 }
        impl Eq for Leaf { fn eq(self, other: Leaf) -> bool { true } }
        impl Hash for Leaf { fn hash(self) -> i64 { 1 } }
        type EqLeaf { callback: fn() -> i64 }
        impl Eq for EqLeaf { fn eq(self, other: EqLeaf) -> bool { true } }
        type Wrapper<T> { value: T }
        enum Choice { SomeLeaf(Leaf), Empty }
        type Unsupported { callback: fn() -> i64 }
        fn main() -> i64 { 0 }
    ",
    );
    let leaf = ResolvedTy::named_user("Leaf", vec![]);
    for capability in [Hash, Eq] {
        assert!(matches!(
            service
                .capability_plan(&leaf, capability)
                .unwrap()
                .map(|selection| selection.plan().clone()),
            Some(User { .. })
        ));
        let wrapper = ResolvedTy::named_user("Wrapper", vec![leaf.clone()]);
        assert_eq!(
            service
                .capability_plan(&wrapper, capability)
                .unwrap()
                .map(|selection| selection.plan().clone()),
            Some(Derived)
        );
        let row = service.require(&wrapper).unwrap();
        assert!(row.hash && row.eq);
        assert_eq!(
            service
                .capability_plan(&ResolvedTy::named_user("Unsupported", vec![]), capability)
                .unwrap(),
            None
        );
    }
    let eq_only = ResolvedTy::named_user("Wrapper", vec![ResolvedTy::named_user("EqLeaf", vec![])]);
    assert_eq!(
        service
            .capability_plan(&eq_only, Eq)
            .unwrap()
            .map(|selection| selection.plan().clone()),
        Some(Derived)
    );
    assert_eq!(service.capability_plan(&eq_only, Hash).unwrap(), None);
    let row = service.require(&eq_only).unwrap();
    assert!(row.eq && !row.hash);
    for container in [
        ResolvedTy::Tuple(vec![leaf.clone()]),
        ResolvedTy::named_builtin("Vec", BuiltinType::Vec, vec![leaf]),
        ResolvedTy::named_user("Choice", vec![]),
    ] {
        assert_eq!(
            service
                .capability_plan(&container, Eq)
                .unwrap()
                .map(|selection| selection.plan().clone()),
            Some(Derived)
        );
        assert_eq!(service.capability_plan(&container, Hash).unwrap(), None);
    }
}

#[test]
fn selected_value_methods_require_impl_and_method_where_obligations() {
    use hew_types::{
        ValueCapability::{Eq, Hash},
        ValueMethodPlan::User,
    };
    let mut service = facts(
        r"
        type Inline<T> { value: T }
        impl<T: Hash> Hash for Inline<T> { fn hash(self) -> i64 { 1 } }
        type Where<T> { value: T }
        impl<T> Hash for Where<T> where T: Hash { fn hash(self) -> i64 { 1 } }
        type Method<T> { value: T }
        impl<T> Eq for Method<T> { fn eq(self, other: Method<T>) -> bool where T: Hash { true } }
        type Custom<T> { value: T }
        trait Other {}
        impl Other for i64 {}
        impl<T: Other> Hash for Custom<T> { fn hash(self) -> i64 { 1 } }
        fn main() -> i64 { 0 }
    ",
    );
    let non_hash = ResolvedTy::Function {
        capabilities: hew_parser::ast::CallableCapabilities::default(),
        params: vec![],
        ret: Box::new(ResolvedTy::I64),
    };
    for (name, capability) in [("Inline", Hash), ("Where", Hash), ("Method", Eq)] {
        let good = ResolvedTy::named_user(name, vec![ResolvedTy::I64]);
        let bad = ResolvedTy::named_user(name, vec![non_hash.clone()]);
        assert!(matches!(
            service
                .capability_plan(&good, capability)
                .unwrap()
                .map(|selection| selection.plan().clone()),
            Some(User { .. })
        ));
        assert!(service.capability_plan(&bad, capability).is_err());
        assert!(service.require(&bad).is_err());
    }
    // The immutable context has no general trait solver: a custom bound needs
    // an explicit refusal even when a live checker could establish it.
    assert!(service
        .capability_plan(
            &ResolvedTy::named_user("Custom", vec![ResolvedTy::I64]),
            Hash
        )
        .is_err());
}

#[test]
fn a_user_hash_method_does_not_expand_the_admitted_collection_shapes() {
    use hew_types::ValueCapability::Hash;
    let mut service = facts(
        r"
        enum Choice { Empty }
        impl Hash for Choice { fn hash(self) -> i64 { 1 } }
        impl<T> Hash for Vec<T> { fn hash(self) -> i64 { 2 } }
        fn main() -> i64 { 0 }
    ",
    );
    for ty in [
        ResolvedTy::named_user("Choice", vec![]),
        ResolvedTy::named_builtin("Vec", BuiltinType::Vec, vec![ResolvedTy::I64]),
    ] {
        assert_eq!(service.capability_plan(&ty, Hash).unwrap(), None);
        assert!(!service.require(&ty).unwrap().hash);
    }
}

#[test]
fn concrete_comparisons_and_capability_queries_select_the_same_eq_specialization() {
    use hew_types::{UserComparisonDispatch, ValueCapability::Eq, ValueMethodPlan::User};
    let parsed = hew_parser::parse(
        r"
        type Key<T> { value: T }
        impl<T> Eq for Key<T> { fn eq(self, other: Key<T>) -> bool { true } }
        impl Eq for Key<i64> { fn eq(self, other: Key<i64>) -> bool { false } }
        fn same(a: Key<i64>, b: Key<i64>) -> bool { a == b }
    ",
    );
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let output = Checker::new(ModuleRegistry::new(vec![])).check_program(&parsed.program);
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    let selected = output
        .identity
        .declaration_by_path("Key::<impl Eq for Key<i64>>::eq")
        .unwrap()
        .clone();
    assert!(output.user_comparison_dispatch.values().any(
        |dispatch| matches!(dispatch, UserComparisonDispatch::Eq { method } if *method == selected)
    ));
    let mut service = TypeFactService::new(output.type_fact_context, output.type_facts);
    assert_eq!(
        service
            .capability_plan(&ResolvedTy::named_user("Key", vec![ResolvedTy::I64]), Eq)
            .unwrap()
            .map(|selection| selection.plan().clone()),
        Some(User {
            method: selected,
            type_args: vec![]
        })
    );
}
