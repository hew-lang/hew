use hew_types::error::TypeErrorKind;
use hew_types::{
    module_registry::ModuleRegistry, CallTarget, Checker, HashMapMethod, HashSetMethod,
    MethodTargetFamily, SpanKey, Ty, TypeCheckOutput,
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
        include_str!("../../tests/core-acceptance/cases/map-owned-projections.hew"),
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

#[test]
fn map_emptiness_resolves_to_a_composed_typed_method() {
    let source = r"
        fn inspect() -> bool {
            let values: HashMap<string, Vec<string>> = HashMap.new();
            let empty = values.is_empty();
            empty
        }
        fn main() -> i64 { if inspect() { 1 } else { 0 } }
    ";
    let output = check_ok(source);
    let start = source.find("values.is_empty()").unwrap();
    let site = SpanKey::from(&(start..start + "values.is_empty()".len()));
    assert_eq!(output.expr_types[&site], Ty::Bool);
    assert_eq!(
        output.resolved_calls[&site].target,
        CallTarget::RuntimeCollection(MethodTargetFamily::HashMap(HashMapMethod::IsEmpty))
    );
    assert!(output.resolved_calls[&site]
        .method_target
        .symbol_name
        .is_empty());
    assert!(hew_types::runtime_call::MapValueOp::from_method(HashMapMethod::IsEmpty).is_none());
    let invalid = check(&source.replace("values.is_empty()", "values.is_empty(1)"));
    assert!(invalid
        .errors
        .iter()
        .any(|error| error.kind == TypeErrorKind::ArityMismatch));
}

#[test]
fn map_snapshots_admit_nested_ordinary_values_and_owned_keys() {
    for value in [
        "Vec<string>",
        "Payload",
        "Vec<Payload>",
        "Option<Result<Payload, string>>",
        "HashMap<Key, Vec<Payload>>",
        "HashSet<Key>",
        "Rc<Payload>",
    ] {
        let source = format!(
            r"
            type Key {{ name: string, rank: i64 }}
            enum Payload {{ Empty, Text(string), Children(Vec<Payload>), }}
            fn main() -> i64 {{
                let values: HashMap<Key, {value}> = HashMap.new();
                let keys: Vec<Key> = values.keys();
                let snapshot: Vec<{value}> = values.values();
                let entries: Vec<(Key, {value})> = values.entries();
                keys.len() + snapshot.len() + entries.len()
            }}
        "
        );
        check_ok(&source);
    }
}

#[test]
fn map_iteration_uses_the_same_recursive_snapshot_admission() {
    check_ok(
        r"
        enum Carrier<T> { Leaf(T), Branches(Vec<Entry<T>>), }
        type Entry<T> { value: Carrier<T> }
        fn main() -> i64 {
            let values: HashMap<string, Carrier<string>> = HashMap.new();
            var count = 0;
            for (key, value) in values { count += key.len(); }
            var cursor = values.into_iter();
            match cursor.next() {
                .Some((key, value)) => count + key.len(),
                .None => count,
            }
        }
    ",
    );
}

#[test]
fn map_snapshots_preserve_resource_and_function_value_refusals() {
    // A bare callable value has no map ingress at all, so its refusal is the
    // shape's, not a projection's. Every other clone-free value is stored and
    // refused only where an operation copies it out.
    for (declarations, value, reason, keys_ok) in [
        ("#[resource] type Token { id: i64 } impl Token { fn close(consume self) {} }", "Token", "resource/linear", true),
        ("#[resource] type Token { id: i64 } impl Token { fn close(consume self) {} } type Holder { token: Token }", "Holder", "resource/linear", true),
        ("#[resource] type Token { id: i64 } impl Token { fn close(consume self) {} }", "Vec<Token>", "resource/linear", true),
        ("", "fn(i64) -> i64", "no map ingress", false),
        ("type Holder { callback: fn(i64) -> i64 }", "Holder", "closure value", true),
        ("", "Vec<fn(i64) -> i64>", "closure value", true),
    ] {
        for projection in ["values", "entries", "into_iter"] {
            let source = format!(
                "{declarations} fn main() {{ let values: HashMap<string, {value}> = HashMap.new(); values.{projection}(); }}"
            );
            let output = check(&source);
            assert!(
                output.errors.iter().any(|error| error.kind == TypeErrorKind::InvalidOperation
                    && error.message.contains(reason)),
                "{value}.{projection} must refuse an unavailable copy operation: {:#?}",
                output.errors
            );
        }
        // `keys()` projects only the keys, so it copies no value.
        if keys_ok {
            check_ok(&format!(
                "{declarations} fn main() {{ let values: HashMap<string, {value}> = HashMap.new(); values.keys(); }}"
            ));
        }
    }
}

#[test]
fn generic_key_uses_its_substituted_semantic_capabilities() {
    for collection in ["HashMap<Key<i64>, string>", "HashSet<Key<i64>>"] {
        let (constructor, insert) = if collection.starts_with("HashMap") {
            ("HashMap", "values.insert(Key { value: 7 }, \"kept\");")
        } else {
            ("HashSet", "values.insert(Key { value: 7 });")
        };
        check_ok(&format!(
            "type Key<T> {{ value: T }}
             impl<T> Hash for Key<T> {{ fn hash(self) -> i64 {{ 1 }} }}
             fn main() -> i64 {{
                 var values: {collection} = {constructor}.new();
                 {insert}
                 values.len()
             }}"
        ));
    }
}

#[test]
fn generic_keys_keep_owned_values_and_projection_types() {
    check_ok(
        r#"
        type Key<T> { value: T }
        impl<T> Hash for Key<T> { fn hash(self) -> i64 { 1 } }
        type Payload<T> { value: T }
        fn main() -> i64 {
            var values: HashMap<Key<string>, Vec<Payload<string>>> = HashMap.new();
            values.insert(Key { value: "key" }, Vec.new());
            let keys: Vec<Key<string>> = values.keys();
            let payloads: Vec<Vec<Payload<string>>> = values.values();
            let entries: Vec<(Key<string>, Vec<Payload<string>>)> = values.entries();
            keys.len() + payloads.len() + entries.len()
        }
    "#,
    );
}

#[test]
fn key_hash_override_does_not_invent_eq_or_resource_copy() {
    for (declarations, key) in [
        (
            "#[resource] type Token { id: i64 } impl Token { fn close(consume self) {} }",
            "Token",
        ),
        ("type Key<T> { value: T }", "Key<fn(i64) -> i64>"),
        ("enum Choice { A, B }", "Choice"),
    ] {
        for collection in [format!("HashMap<{key}, i64>"), format!("HashSet<{key}>")] {
            let source = format!("{declarations} fn inspect(values: {collection}) -> i64 {{ values.len() }} fn main() {{}}");
            let output = check(&source);
            assert!(
                output
                    .errors
                    .iter()
                    .any(|error| error.kind == TypeErrorKind::BoundsNotSatisfied),
                "unsupported key {collection} must fail semantic capability admission: {:#?}",
                output.errors
            );
        }
    }
    let output = check("type Key<T> { value: T } impl<T> Hash for Key<T> { fn hash(self) -> i64 { 1 } } fn main() { let values: HashMap<Key<fn(i64) -> i64>, string> = HashMap.new(); }");
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.kind == TypeErrorKind::BoundsNotSatisfied
                && error.message.contains("Eq")),
        "Hash override must not imply Eq: {:#?}",
        output.errors
    );
}

#[test]
fn forward_declared_map_values_are_checked_after_registration() {
    for declarations in [
        "type First { values: HashMap<string, Second> } type Second { name: string }",
        "type Second { name: string } type First { values: HashMap<string, Second> }",
    ] {
        check_ok(&format!(
            "{declarations} fn main() {{ let value = First {{ values: HashMap.new() }}; }}"
        ));
    }
    // A map of a clone-free value is an ordinary type: it is read by borrow and
    // drained by removal, so only an operation that copies its values refuses.
    for declarations in [
        "type First { values: HashMap<string, Second> } #[resource] type Second { id: i64 } impl Second { fn close(consume self) {} }",
        "#[resource] type Second { id: i64 } impl Second { fn close(consume self) {} } type First { values: HashMap<string, Second> }",
    ] {
        check_ok(&format!("{declarations} fn main() {{}}"));
        let output = check(&format!(
            "{declarations} fn main() {{ var values: HashMap<string, Second> = HashMap.new(); values.values(); }}"
        ));
        assert!(output.errors.iter().any(|error| error.kind == TypeErrorKind::InvalidOperation && error.message.contains("resource/linear")), "a forward resource value must refuse the copying projection: {:#?}", output.errors);
    }
}

#[test]
fn abstract_map_key_does_not_hide_a_forward_resource_value() {
    // The value obligation belongs to the operation that copies values out, and
    // an abstract key must not hide it there.
    let output = check("#[resource] type Second { id: i64 } impl Second { fn close(consume self) {} } fn snap<K: Hash + Eq>(values: HashMap<K, Second>) { values.entries(); } fn main() {}");
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.kind == TypeErrorKind::InvalidOperation
                && error.message.contains("resource/linear")),
        "an abstract key must not bypass the copying projection's value admission: {:#?}",
        output.errors
    );
}

#[test]
fn bounded_collection_parameter_keeps_its_template_capabilities() {
    check_ok("fn inspect<T: Hash + Eq>(values: HashSet<T>) -> i64 { values.len() } fn main() -> i64 { let values: HashSet<i64> = HashSet.new(); inspect(values) }");
    check_ok("fn inspect<K: Hash + Eq, V>(values: HashMap<K, V>) -> i64 { values.len() } fn main() -> i64 { let values: HashMap<i64, string> = HashMap.new(); inspect(values) }");
    let output = check("fn inspect<T>(values: HashSet<T>) -> i64 { values.len() } fn main() {}");
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.kind == TypeErrorKind::BoundsNotSatisfied),
        "an unbounded template parameter has no key capabilities: {:#?}",
        output.errors
    );
}
