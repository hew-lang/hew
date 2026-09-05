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
