use hew_types::check::dispatch::{
    bound_diagnostic_strings, resolve_method_call, Bound, CallAbiHint, ImplDef, ImplId,
    ImplRegistry, LookupError, MethodTarget, RuntimeAbi, TyPattern,
};
use hew_types::traits::MarkerTrait;

fn primitive(name: &str) -> TyPattern {
    TyPattern::Primitive(name.to_string())
}

fn var(name: &str) -> TyPattern {
    TyPattern::Var(name.to_string())
}

fn app(ctor: &str, args: Vec<TyPattern>) -> TyPattern {
    TyPattern::App {
        ctor: ctor.to_string(),
        args,
    }
}

fn seed_registry() -> (ImplRegistry, ImplId, ImplId) {
    let mut registry = ImplRegistry::new();
    let map_id = registry.register(ImplDef {
        trait_name: "Map".to_string(),
        self_pattern: app("HashMap", vec![var("K"), var("V")]),
        where_bounds: vec![
            Bound {
                trait_name: MarkerTrait::Hash,
                var: "K".to_string(),
            },
            Bound {
                trait_name: MarkerTrait::Eq,
                var: "K".to_string(),
            },
        ],
        methods: vec![(
            "insert".to_string(),
            MethodTarget {
                symbol_name: "hew_hashmap_insert".to_string(),
                abi: RuntimeAbi::ByRefMut,
                call_hint: CallAbiHint::RuntimeShim,
                consumes_receiver: false,
            },
        )],
    });
    let set_id = registry.register(ImplDef {
        trait_name: "Set".to_string(),
        self_pattern: app("HashSet", vec![var("T")]),
        where_bounds: vec![
            Bound {
                trait_name: MarkerTrait::Hash,
                var: "T".to_string(),
            },
            Bound {
                trait_name: MarkerTrait::Eq,
                var: "T".to_string(),
            },
        ],
        methods: vec![(
            "insert".to_string(),
            MethodTarget {
                symbol_name: "hew_hashset_insert".to_string(),
                abi: RuntimeAbi::ByRefMut,
                call_hint: CallAbiHint::RuntimeShim,
                consumes_receiver: false,
            },
        )],
    });
    (registry, map_id, set_id)
}

fn stage_c_marker_predicate(marker: MarkerTrait, pattern: &TyPattern) -> bool {
    let TyPattern::Primitive(name) = pattern else {
        return false;
    };
    match marker {
        MarkerTrait::Hash | MarkerTrait::Eq => matches!(
            name.as_str(),
            "i8" | "i16"
                | "i32"
                | "i64"
                | "u8"
                | "u16"
                | "u32"
                | "u64"
                | "bool"
                | "char"
                | "duration"
                | "String"
        ),
        _ => false,
    }
}

#[test]
fn hashmap_i64_key_resolves_at_unit_level_without_checker_admission() {
    let (registry, map_id, _) = seed_registry();
    let resolved = resolve_method_call(
        &registry,
        "Map",
        "insert",
        &app("HashMap", vec![primitive("i64"), primitive("V")]),
        &stage_c_marker_predicate,
    )
    .expect("HashMap<i64, V> satisfies K: Hash + Eq in the unified resolver");

    assert_eq!(resolved.impl_id, map_id);
    assert_eq!(resolved.type_args, vec![primitive("i64"), primitive("V")]);
}

#[test]
fn hashset_i64_resolves_at_unit_level_without_checker_admission() {
    let (registry, _, set_id) = seed_registry();
    let resolved = resolve_method_call(
        &registry,
        "Set",
        "insert",
        &app("HashSet", vec![primitive("i64")]),
        &stage_c_marker_predicate,
    )
    .expect("HashSet<i64> satisfies T: Hash + Eq in the unified resolver");

    assert_eq!(resolved.impl_id, set_id);
    assert_eq!(resolved.type_args, vec![primitive("i64")]);
}

#[test]
fn hashmap_f64_key_reports_bound_form_diagnostic_strings() {
    let (registry, map_id, _) = seed_registry();
    let err = resolve_method_call(
        &registry,
        "Map",
        "insert",
        &app("HashMap", vec![primitive("f64"), primitive("V")]),
        &stage_c_marker_predicate,
    )
    .expect_err("f64 does not satisfy Hash/Eq bounds");

    match &err {
        LookupError::BoundsNotSatisfied {
            impl_id,
            unsatisfied,
            witness,
        } => {
            assert_eq!(*impl_id, map_id);
            assert_eq!(*witness, primitive("f64"));
            assert_eq!(unsatisfied.len(), 2);
        }
        other => panic!("expected BoundsNotSatisfied, got {other:?}"),
    }

    let messages = bound_diagnostic_strings(&err);
    assert_eq!(
        messages,
        vec![
            "f64 does not implement Hash".to_string(),
            "f64 does not implement Eq".to_string(),
        ]
    );
}
