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
        let Some(User { method, type_args }) = service.capability_plan(&key, capability).unwrap()
        else {
            panic!("user override must be selected");
        };
        assert!(type_args.is_empty());
        methods.push(method);
        assert_eq!(
            service
                .capability_plan(&ResolvedTy::named_user("Plain", vec![]), capability)
                .unwrap(),
            Some(Derived)
        );
    }
    assert_ne!(methods[0], methods[1]);
    assert!(service.require(&key).unwrap().hash);
    assert!(service.require(&key).unwrap().eq);
    let hash_only = ResolvedTy::named_user("HashOnly", vec![]);
    assert!(matches!(
        service.capability_plan(&hash_only, Hash).unwrap(),
        Some(User { .. })
    ));
    assert_eq!(
        service.capability_plan(&hash_only, Eq).unwrap(),
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
    }) = service.capability_plan(&generic, Hash).unwrap()
    else {
        panic!("generic impl");
    };
    assert_eq!(type_args, vec![ResolvedTy::I64, ResolvedTy::Bool]);
    let Some(User {
        method: exact_method,
        type_args,
    }) = service.capability_plan(&exact, Hash).unwrap()
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
    assert_eq!(service.capability_plan(&user, Hash).unwrap(), Some(Derived));
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
