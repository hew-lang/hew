use hew_types::error::TypeErrorKind;
use hew_types::{
    module_registry::ModuleRegistry, CallTarget, Checker, HashSetMethod, MethodTargetFamily,
    SpanKey, Ty, TypeCheckOutput,
};

fn check(source: &str) -> TypeCheckOutput {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "parse: {:#?}", parsed.errors);
    Checker::new(ModuleRegistry::new(Vec::new())).check_program(&parsed.program)
}

fn check_ok(source: &str) -> TypeCheckOutput {
    let output = check(source);
    assert!(output.errors.is_empty(), "checker: {:#?}", output.errors);
    output
}

#[test]
fn set_snapshot_has_the_canonical_element_result_and_dispatch() {
    let source = r#"
        type Member { name: string, rank: i64 }
        fn main() -> i64 {
            var members: HashSet<Member> = HashSet.new();
            members.insert(Member { name: "kept", rank: 7 });
            let snapshot: Vec<Member> = members.to_vec();
            members.clear();
            snapshot.len()
        }
    "#;
    let output = check_ok(source);
    let start = source.find("members.to_vec()").unwrap();
    let site = SpanKey::from(&(start..start + "members.to_vec()".len()));
    assert_eq!(
        output.resolved_calls[&site].target,
        CallTarget::RuntimeCollection(MethodTargetFamily::HashSet(HashSetMethod::ToVec))
    );
    assert_eq!(
        output.expr_types[&site],
        Ty::builtin_named(
            hew_types::BuiltinType::Vec,
            vec![Ty::Named {
                name: "Member".into(),
                args: vec![],
                builtin: None,
            }]
        )
    );
    assert!(
        !output.method_call_rewrites.contains_key(&site),
        "the existing typed collection dispatch must be the sole method authority"
    );
}

#[test]
fn set_snapshot_checks_arity_and_result_type() {
    for source in [
        "fn main() { let values: HashSet<i64> = HashSet.new(); values.to_vec(1); }",
        "fn main() { let values: HashSet<i64> = HashSet.new(); let snapshot: Vec<string> = values.to_vec(); }",
    ] {
        let output = check(source);
        assert!(!output.errors.is_empty(), "invalid snapshot call was admitted");
        assert!(
            !output.errors.iter().any(|error| error.kind == TypeErrorKind::UndefinedMethod),
            "to_vec must resolve before checking its arguments and result: {:#?}",
            output.errors
        );
    }
}

const MUTATIONS: &[(&str, &str, &str)] = &[
    ("Vec<i64>", "Vec.new()", "push(1)"),
    ("Vec<i64>", "Vec.new()", "pop()"),
    ("Vec<i64>", "Vec.new()", "clear()"),
    ("HashMap<i64, i64>", "HashMap.new()", "insert(1, 2)"),
    ("HashMap<i64, i64>", "HashMap.new()", "remove(1)"),
    ("HashMap<i64, i64>", "HashMap.new()", "clear()"),
    ("HashSet<i64>", "HashSet.new()", "insert(1)"),
    ("HashSet<i64>", "HashSet.new()", "remove(1)"),
    ("HashSet<i64>", "HashSet.new()", "clear()"),
];

#[test]
fn mutating_collections_require_var_and_count_as_binding_writes() {
    for (ty, constructor, mutation) in MUTATIONS {
        let source = format!(
            "fn main() -> i64 {{ var values: {ty} = {constructor}; values.{mutation}; values.len() }}"
        );
        let output = check_ok(&source);
        assert!(
            !output
                .warnings
                .iter()
                .any(|warning| warning.kind == TypeErrorKind::UnusedMut),
            "{ty}.{mutation} must write the receiver: {:#?}",
            output.warnings
        );
        let immutable = source.replace("var values:", "let values:");
        let output = check(&immutable);
        assert!(
            output
                .errors
                .iter()
                .any(|error| error.kind == TypeErrorKind::MutabilityError),
            "{ty}.{mutation} must obey the same immutability policy: {:#?}",
            output.errors
        );
    }
}

#[test]
fn collection_reads_do_not_count_as_binding_writes() {
    for (ty, constructor) in [
        ("Vec<i64>", "Vec.new()"),
        ("HashMap<i64, i64>", "HashMap.new()"),
        ("HashSet<i64>", "HashSet.new()"),
    ] {
        let output = check_ok(&format!(
            "fn main() -> i64 {{ var values: {ty} = {constructor}; values.len() }}"
        ));
        assert!(output
            .warnings
            .iter()
            .any(|warning| warning.kind == TypeErrorKind::UnusedMut));
    }
}

#[test]
fn collection_field_mutation_tracks_the_containing_binding() {
    for (ty, constructor, mutation) in MUTATIONS {
        let source = format!(
            "type Holder {{ values: {ty} }} fn main() -> i64 {{ var holder = Holder {{ values: {constructor} }}; holder.values.{mutation}; holder.values.len() }}"
        );
        let output = check_ok(&source);
        assert!(
            !output
                .warnings
                .iter()
                .any(|warning| warning.kind == TypeErrorKind::UnusedMut),
            "{ty}.{mutation} must write the containing binding: {:#?}",
            output.warnings
        );
        let output = check(&source.replace("var holder", "let holder"));
        assert!(output
            .errors
            .iter()
            .any(|error| error.kind == TypeErrorKind::MutabilityError));
    }
}

#[test]
fn permanent_map_and_set_mutations_do_not_warn_about_unused_mutability() {
    for source in [
        include_str!("../../tests/core-acceptance/cases/map-value-copy.hew"),
        include_str!("../../tests/core-acceptance/cases/map-scalar-growth-remove.hew"),
        include_str!("../../tests/core-acceptance/cases/map-record-key.hew"),
        include_str!("../../tests/core-acceptance/cases/map-index-fault.hew"),
        include_str!("../../tests/core-acceptance/cases/set-value-copy.hew"),
    ] {
        let output = check_ok(source);
        assert!(
            !output
                .warnings
                .iter()
                .any(|warning| warning.kind == TypeErrorKind::UnusedMut),
            "permanent source mutations must be recognised: {:#?}",
            output.warnings
        );
    }
}
